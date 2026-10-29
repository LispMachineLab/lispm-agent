;;; lispm-session-tools.el --- Bounded searches of recorded conversations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Session tools read the same records as the session list, without
;; starting an agent or accepting a transcript path from the model.
;; Searches inspect a bounded recent tail, one file per event-loop turn.
;; They deliberately omit reasoning blocks, images and system prompts.

;;; Code:

(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-agents-claude)
(require 'lispm-agents-codex)

(defconst lispm-session-tools-scan-limit 200
  "Maximum number of recent transcript files a search inspects.")

(defconst lispm-session-tools-tail-bytes (* 256 1024)
  "Maximum bytes read from the end of each transcript.")

(defconst lispm-session-tools-text-limit 800
  "Maximum characters returned from one message or search match.")

(defun lispm-session-tools--limit (limit default)
  "Validate LIMIT, using DEFAULT when it is omitted."
  (let ((limit (or limit default)))
    (unless (and (integerp limit) (<= 1 limit 30))
      (error "Limit must be between 1 and 30"))
    limit))

(defun lispm-session-tools--since (since)
  "Read SINCE as an ISO date or timestamp, or return nil."
  (when since
    (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:T.*\\)?\\'" since)
      (error "Since must be an ISO date or timestamp"))
    (condition-case nil
        (float-time (parse-iso8601-time-string
                     (if (= (length since) 10) (concat since "T00:00:00Z") since)))
      (error (error "Since must be an ISO date or timestamp")))))

(defun lispm-session-tools--files (provider)
  "Known transcript files for PROVIDER in newest-first order.
Return (FILES . ERRORS); FILES contain (PROVIDER FILE MTIME ARCHIVED).
Discovery reads directory names and attributes, not transcript bodies.
Metadata, rather than a filename, determines a session's identity: a
Codex fork can record the same id under a different rollout filename."
  (let (files errors)
    (dolist (kind (if provider (list (intern provider)) '(lispm claude codex)))
      (condition-case err
          (let ((found (pcase kind
                         ('lispm (lispm--session-files-in "projects/*/sessions/*.org"))
                         ('claude (lispm-agents-claude--files))
                         ('codex (lispm-agents-codex--files)))))
            ;; Codex moves archived rollouts beside its sessions directory.
            (when (eq kind 'codex)
              (let ((archive (expand-file-name "../archived_sessions/"
                                               lispm-agents-codex-sessions-directory)))
                (when (file-directory-p archive)
                  (dolist (file (directory-files-recursively archive "\\`rollout-.*\\.jsonl\\'"))
                    (push (list kind file nil t) files)))))
            (dolist (file found) (push (list kind file nil nil) files)))
        (error (push (format "%s: %s" kind (error-message-string err)) errors))))
    (setq files
          (delq nil
                (mapcar (lambda (entry)
                          (let ((file (nth 1 entry)))
                            (when (not (file-remote-p file))
                              (when-let* ((attrs (file-attributes file))
                                          ((null (file-attribute-type attrs))))
                                (setf (nth 2 entry) (float-time (file-attribute-modification-time attrs)))
                                entry))))
                        files)))
    (cons (sort files (lambda (a b) (> (nth 2 a) (nth 2 b)))) (nreverse errors))))

(defun lispm-session-tools--archived ()
  "Read LispM's archive keys without repairing or rewriting its state file.
Return (KEYS . KNOWN); a malformed or oversized file leaves KNOWN nil."
  (if (not (eq lispm-agents--archived 'unloaded))
      (cons lispm-agents--archived t)
    (if (not (file-exists-p lispm-agents-state-file))
        (cons nil t)
      (condition-case nil
          (when (<= (file-attribute-size (file-attributes lispm-agents-state-file)) 65536)
            (with-temp-buffer
              (insert-file-contents lispm-agents-state-file)
              (let ((state (read (current-buffer))))
                (when (and (plistp state) (listp (plist-get state :archived))
                           (seq-every-p #'stringp (plist-get state :archived)))
                  (cons (plist-get state :archived) t)))))
        (error nil)))))

(defun lispm-session-tools--check-claude-sidecar (file)
  "Reject an unbounded subagent metadata sidecar beside FILE.
The shared Claude metadata reader reads the whole sidecar.  Check its
size even when the transcript's own metadata is already cached."
  (when (equal (file-name-nondirectory
                (directory-file-name (file-name-directory file))) "subagents")
    (let* ((sidecar (concat (file-name-sans-extension file) ".meta.json"))
           (attributes (file-attributes sidecar)))
      (when attributes
        (unless (null (file-attribute-type attributes))
          (error "Claude subagent metadata sidecar is not a regular file"))
        (when (> (file-attribute-size attributes) 65536)
          (error "Claude subagent metadata sidecar exceeds 65536 bytes"))))))

(defun lispm-session-tools--metadata (entry)
  "Read bounded metadata for discovered file ENTRY, or nil."
  (pcase-let ((`(,provider ,file ,time ,archived) entry))
    (when (eq provider 'claude)
      (lispm-session-tools--check-claude-sidecar file))
    (let* ((lispm-agents-codex-prompt-bytes 65536)
           (raw (if (eq provider 'lispm)
                    (lispm--session-description file)
                  (lispm-agents-cached-session
                   file (apply-partially (if (eq provider 'claude)
                                            #'lispm-agents-claude--session
                                          #'lispm-agents-codex--session)
                                        file))))
           (record (copy-sequence raw)))
      (when (and (stringp (plist-get raw :id)) (<= (length (plist-get raw :id)) 256)
                 (string-match-p "\\`[[:alnum:]_.:-]+\\'" (plist-get raw :id))
                 (stringp (if (eq provider 'lispm) (plist-get raw :root) (plist-get raw :cwd))))
        (when (eq provider 'lispm)
          (setq record (plist-put record :cwd (plist-get raw :root))))
        (setq record (plist-put record :agent provider)
              record (plist-put record :time time)
              record (plist-put record :archived-file archived))
        ;; Cached readers may know the user's chosen title.  Asking them
        ;; does not start a scan or read an unbounded title index.
        (when-let* ((known (lispm-agents-known-session provider (plist-get record :id)))
                    (title (plist-get known :title)))
          (setq record (plist-put record :title title)))
        record))))

(defun lispm-session-tools--text (content)
  "Text in CONTENT, excluding images and other non-text blocks."
  (cond ((stringp content) content)
        ((listp content)
         (mapconcat (lambda (part)
                      (let ((text (and (listp part)
                                       (member (plist-get part :type) '("text" "input_text" "output_text"))
                                       (plist-get part :text))))
                        (if (stringp text) text "")))
                    content "\n"))
        (t "")))

(defun lispm-session-tools--json (value)
  "Render VALUE for a tool-history message without running anything."
  (if (stringp value) value
    (condition-case nil (lispm--json-encode value) (error "[unreadable input]"))))

(defun lispm-session-tools--external-events (provider objects)
  "Conversation events of PROVIDER in bounded transcript OBJECTS."
  (let (events)
    (cl-labels ((add (role text &optional tool origin)
                 (when (and (stringp text) (not (string-empty-p text)))
                   ;; Codex records a message both as an event and a
                   ;; response item; collapse only adjacent cross-format copies.
                   (unless (and origin (car events)
                                (not (equal origin (plist-get (car events) :origin)))
                                (equal role (plist-get (car events) :role))
                                (equal text (plist-get (car events) :text)))
                     (push (list :role role :text text :tool tool :origin origin) events)))))
      (dolist (object objects)
        (let ((type (plist-get object :type)))
          (if (eq provider 'claude)
              (when (and (member type '("user" "assistant")) (not (plist-get object :isMeta)))
                (let ((content (plist-get (plist-get object :message) :content)))
                  (if (stringp content) (add type content)
                    (dolist (part (and (listp content) content))
                      (pcase (plist-get part :type)
                        ("text" (add type (plist-get part :text)))
                        ("tool_use" (add "tool" (lispm-session-tools--json (plist-get part :input))
                                         (plist-get part :name)))
                        ("tool_result" (add "tool" (lispm-session-tools--text (plist-get part :content)))))))))
            (let* ((payload (plist-get object :payload))
                   (kind (plist-get payload :type)))
              (cond
               ((equal type "response_item")
                (pcase kind
                  ("message" (when (and (member (plist-get payload :role) '("user" "assistant"))
                                        (not (equal (plist-get payload :channel) "analysis")))
                               (add (plist-get payload :role)
                                    (lispm-session-tools--text (plist-get payload :content)) nil type)))
                  ((or "function_call" "custom_tool_call")
                   (add "tool" (lispm-session-tools--json
                                (or (plist-get payload :arguments) (plist-get payload :input)))
                        (plist-get payload :name)))
                  ((or "function_call_output" "custom_tool_call_output")
                   (add "tool" (lispm-session-tools--text (plist-get payload :output))))))
               ((equal type "event_msg")
                (pcase kind
                  ("user_message" (add "user" (plist-get payload :message) nil type))
                  ("agent_message" (add "assistant" (plist-get payload :message) nil type)))))))))
      (nreverse events))))

(defun lispm-session-tools--native-events ()
  "Conversation events in the current bounded Org transcript buffer."
  (let (events)
    (dolist (section (lispm-transcript-sections))
      (unless (plist-get section :ignore)
        (if (eq (plist-get section :kind) 'assistant)
            (dolist (part (lispm-transcript-parts section))
              (pcase (plist-get part :kind)
                ('text (push (list :role "assistant" :text (plist-get part :text)) events))
                ('tool (push (list :role "tool" :tool (plist-get part :name)
                                   :text (concat (or (plist-get part :state) "") " "
                                                 (lispm-session-tools--json (plist-get part :input))
                                                 "\n" (or (plist-get part :result) ""))) events))))
          (when-let* ((text (lispm-transcript-section-input section)))
            (push (list :role "user" :text text) events)))))
    (nreverse events)))

(defun lispm-session-tools--events (record)
  "Return (:events :tail-only :source) for RECORD, reading a bounded tail.
Native transcripts prefer unsaved buffer text.  A leading partial Org
section or JSON line is omitted.  Invalid JSON lines are counted."
  (let* ((file (plist-get record :file))
         (provider (plist-get record :agent))
         (live (and (eq provider 'lispm) (find-buffer-visiting file)))
         (size (if live (with-current-buffer live
                         (save-restriction (widen) (1- (position-bytes (point-max)))))
                 (file-attribute-size (file-attributes file))))
         (start (max 0 (- size lispm-session-tools-tail-bytes))))
    (with-temp-buffer
      (if live
          (insert (with-current-buffer live
                    (save-restriction
                      (widen)
                      (let ((byte (1+ start)))
                        (while (not (byte-to-position byte)) (cl-incf byte))
                        (buffer-substring-no-properties (byte-to-position byte) (point-max))))))
        (insert-file-contents file nil start size))
      (goto-char (point-min))
      (when (> start 0)
        (if (eq provider 'lispm)
            (if (re-search-forward "^\\* " nil t)
                (delete-region (point-min) (line-beginning-position))
              (erase-buffer))
          (forward-line 1)
          (delete-region (point-min) (point))))
      (if (eq provider 'lispm)
          (list :events (lispm-session-tools--native-events) :tail-only (> start 0)
                :source (if live "buffer" "file") :invalid-lines 0)
        (goto-char (point-min))
        (let ((lines (count-lines (point-min) (point-max)))
              (objects (seq-filter (lambda (object) (and (consp object) (keywordp (car object))))
                                   (lispm-agents--parse-json-lines nil))))
          (list :events (lispm-session-tools--external-events provider objects)
                :tail-only (> start 0) :source "file" :invalid-lines (- lines (length objects))))))))

(defun lispm-session-tools--record (record archive)
  "Public metadata of RECORD, with ARCHIVE keys and their known status."
  `((provider . ,(symbol-name (plist-get record :agent)))
    (id . ,(plist-get record :id))
    (title . ,(truncate-string-to-width (or (plist-get record :title) "") 200 nil nil "…"))
    (project . ,(truncate-string-to-width (or (plist-get record :cwd) "") 1000 nil nil "…"))
    (updated_at . ,(format-time-string "%FT%TZ" (seconds-to-time (plist-get record :time)) t))
    (archived . ,(cond ((or (plist-get record :archived-file)
                            (member (lispm-agents--session-key record) (car archive))) t)
                       ((cdr archive) :false) (t :null)))))

(defun lispm-session-tools--excerpt (text &optional query)
  "Bound TEXT around its first literal QUERY match, if any."
  (let* ((match (and query (string-match (regexp-quote (downcase query)) (downcase text))))
         (start (max 0 (- (or match 0) 120)))
         (end (min (length text) (+ start lispm-session-tools-text-limit))))
    (concat (if (> start 0) "…" "") (substring text start end)
            (if (< end (length text)) "…" ""))))

(defun lispm-session-tools--encode (data)
  "Encode DATA within the tool's output budget, keeping valid JSON.
Drop older result rows or messages before shortening the last excerpt;
output_truncated says that this output budget discarded information."
  (let* ((budget lispm-tool-output-limit)
         (encoded (lispm--json-encode data)))
    (while (> (length encoded) budget)
      (setf (alist-get 'output_truncated data) t)
      (let* ((sessions (append (alist-get 'sessions data) nil))
             (session (car sessions))
             (messages (append (alist-get 'messages session) nil)))
        (cond
         ((cdr sessions) (setf (alist-get 'sessions data) (vconcat (butlast sessions))))
         ((cdr messages) (setf (alist-get 'messages session) (vconcat (cdr messages))))
         ((or (and (car messages) (> (length (alist-get 'text (car messages))) 20))
              (> (length (or (alist-get 'snippet session) "")) 20))
          (let* ((cell (if messages (assq 'text (car messages)) (assq 'snippet session)))
                 (text (cdr cell)))
            (setcdr cell (concat (substring text 0 (/ (length text) 2)) "…"))))
         ((> (length (alist-get 'errors data)) 0) (setf (alist-get 'errors data) []))
         (t (setq data '((output_truncated . t) (error . "Output limit is too small for a session record."))))))
      (setq encoded (lispm--json-encode data))
      (when (and (> (length encoded) budget) (not (assq 'sessions data)))
        (setq encoded "{}")))
    encoded))

(cl-defun lispm-session-tools--scan (done &key query project provider since role limit id)
  "Read session records in slices and call DONE once with a JSON result.
QUERY is literal, PROJECT an exact project root, SINCE a modification
time.  ID selects one session; without it this is a bounded search."
  (unless (and (integerp lispm-tool-output-limit) (>= lispm-tool-output-limit 2))
    (error "Tool output limit must be an integer of at least 2"))
  (let* ((limit (lispm-session-tools--limit limit (if id 20 10)))
         (since (lispm-session-tools--since since))
         (project (and project (directory-file-name (expand-file-name project))))
         (query (and query (not (string-empty-p query)) query))
         (archive (lispm-session-tools--archived))
         (discovery (lispm-session-tools--files provider))
         (all (car discovery))
         (files (seq-take all lispm-session-tools-scan-limit))
         (scan-truncated (> (length all) lispm-session-tools-scan-limit))
         (errors (cdr discovery))
         (seen (make-hash-table :test #'equal))
         (agent lispm-current-agent)
         (call-id lispm-current-call)
         (call (and agent call-id (gethash call-id (lispm-agent-pending agent))))
         (buffer (and agent (lispm-agent-buffer agent)))
         (scanned 0) (skipped 0) (matched 0) (tail-only 0) (invalid 0) results found)
    (cl-labels
        ((finish ()
           (if (and id (not found))
               (funcall done (format "No readable %s session with id %s in the bounded scan (%d files; %d errors)."
                                     provider id scanned (length errors)) t)
             (funcall done
                      (lispm-session-tools--encode
                       `((scope . "Recent transcript tails and titles; reasoning, images and system prompts excluded.")
                         (scanned_files . ,scanned)
                         (skipped_files . ,skipped)
                         (scan_limit . ,lispm-session-tools-scan-limit)
                         (tail_bytes . ,lispm-session-tools-tail-bytes)
                         (scan_truncated . ,(if scan-truncated t :false))
                         (tail_only_files . ,tail-only)
                         (invalid_json_lines . ,invalid)
                         (results_truncated . ,(if (> matched limit) t :false))
                         (output_truncated . :false)
                         (errors_total . ,(length errors))
                         (errors_truncated . ,(if (> (length errors) 5) t :false))
                         (errors . ,(vconcat (mapcar (lambda (text) (truncate-string-to-width text 250 nil nil "…"))
                                                    (seq-take (nreverse errors) 5))))
                         (sessions . ,(vconcat (nreverse results))))))))
         (step ()
           (cond
            ((and call (or (not (eq call (gethash call-id (lispm-agent-pending agent))))
                           (and buffer (not (buffer-live-p buffer))))) nil)
            ((or (null files) (and id found)) (finish))
            (t
             (let ((entry (pop files)))
               (cl-incf scanned)
               (condition-case err
                   (when-let* ((record (or (lispm-session-tools--metadata entry)
                                          (progn (cl-incf skipped) nil)))
                               (key (lispm-agents--session-key record))
                               ((not (gethash key seen))))
                     (puthash key t seen)
                     (when (and (or (not id) (equal id (plist-get record :id)))
                                (or (not since) (>= (plist-get record :time) since))
                                (or (not project)
                                    (equal project (directory-file-name
                                                    (expand-file-name (or (plist-get record :cwd) ""))))))
                       (let* ((read (lispm-session-tools--events record))
                              (events (seq-filter (lambda (event) (or (not role) (equal role (plist-get event :role))))
                                                  (plist-get read :events)))
                              (hit (seq-find (lambda (event)
                                               (or (not query) (string-search (downcase query)
                                                                             (downcase (plist-get event :text)))))
                                             events))
                              (title-hit (and (not role) query
                                              (string-search (downcase query) (downcase (or (plist-get record :title) ""))))))
                         (when (plist-get read :tail-only) (cl-incf tail-only))
                         (cl-incf invalid (plist-get read :invalid-lines))
                         (when (or id hit title-hit (and (not query) (not role)))
                           (cl-incf matched)
                           (when (or id (<= matched limit))
                             (let ((public (lispm-session-tools--record record archive)))
                               (if id
                                   (let* ((recent (last events limit))
                                          (messages (mapcar (lambda (event)
                                                              `((role . ,(plist-get event :role))
                                                                (tool . ,(or (plist-get event :tool) :null))
                                                                (text_truncated . ,(if (> (length (plist-get event :text)) lispm-session-tools-text-limit) t :false))
                                                                (text . ,(lispm-session-tools--excerpt (plist-get event :text)))))
                                                            recent)))
                                     (setq found t matched (length events))
                                     (push (append public `((source . ,(plist-get read :source))
                                                            (tail_only . ,(if (plist-get read :tail-only) t :false))
                                                            (messages . ,(vconcat messages)))) results))
                                 (push (append public `((source . ,(plist-get read :source))
                                                        (match_role . ,(if hit (plist-get hit :role) "title"))
                                                        (snippet . ,(lispm-session-tools--excerpt
                                                                     (if hit (plist-get hit :text) (or (plist-get record :title) "")) query))
                                                        (tail_only . ,(if (plist-get read :tail-only) t :false)))) results))))))))
                 (error (push (format "%s:%s: %s" (car entry) (file-name-nondirectory (nth 1 entry))
                                      (error-message-string err)) errors)))
               (run-at-time 0 nil #'step))))))
      (run-at-time 0 nil #'step))))

(lispm-deftool session_search
    ((query string "Literal text to find in titles and recent messages; omit to list recent sessions." :optional t)
     (project string "Exact project directory; omit to search all projects." :optional t)
     (provider string "Session provider; omit for all." :optional t :enum ("lispm" "claude" "codex"))
     (since string "Transcript files modified since this ISO date (UTC midnight) or timestamp." :optional t)
     (role string "Message role to search; omitting it also searches titles." :optional t :enum ("user" "assistant" "tool"))
     (limit integer "Maximum sessions to return, 1–30; default 10." :optional t))
  "Search native LispM, Claude Code and Codex session history, including
archived sessions.  Reads only recent transcript tails, with explicit
scan and truncation counts.  Returns stable provider and id for
session_read; never starts, resumes or changes a session."
  :permission read
  :deferred t
  :keywords '("session" "history" "archive" "archived" "Claude" "Codex" "conversation" "search")
  :async t
  (lispm-session-tools--scan done :query query :project project :provider provider
                             :since since :role role :limit limit))

(lispm-deftool session_read
    ((provider string "The session's provider, as session_search returned." :enum ("lispm" "claude" "codex"))
     (id string "The session's opaque id, as session_search returned; never a file path.")
     (role string "Only this message role; omit for user, assistant and tool messages." :optional t :enum ("user" "assistant" "tool"))
     (limit integer "Maximum recent messages to return, 1–30; default 20." :optional t))
  "Read recent messages and tool calls from a native LispM, Claude Code
or Codex session, including archived history.  Uses provider plus id,
not a path; native current-session text includes unsaved changes.
The bounded tail, message count and shortened text are explicit."
  :permission read
  :deferred t
  :keywords '("session" "history" "archive" "archived" "Claude" "Codex" "conversation" "transcript" "tail")
  :async t
  (unless (and (not (string-empty-p id)) (<= (length id) 256)
               (string-match-p "\\`[[:alnum:]_.:-]+\\'" id))
    (error "Use the opaque session id returned by session_search, not a path"))
  (lispm-session-tools--scan done :provider provider :id id :role role :limit limit))

(provide 'lispm-session-tools)
;;; lispm-session-tools.el ends here
