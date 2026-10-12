;;; lispm-transcript.el --- Org transcripts as agent context  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A session is an org file, and that file *is* the agent's context:
;; every request is built by parsing the buffer, so whatever the user
;; folds, annotates, deletes or tags in it changes what the model sees.
;;
;; Layout of a session buffer:
;;
;;   #+title: coder 2026-10-12 17:30
;;   #+lispm_agent: coder
;;   #+todo: WAITING(w) APPROVED(a) RUNNING(r) | DONE(d) DENIED(n) FAILED(f)
;;   * System                       the system prompt, frozen per session
;;   * User                         a user message
;;   * Assistant                    a model response; one child per block
;;   ** Reply                       a text block
;;   ** Thinking: ...               any other block, stored as raw JSON
;;   ** WAITING edit_file  a.el     a tool call; its TODO state is its
;;   :PROPERTIES:                   approval state, so approving a call
;;   :TOOL_ID:  toolu_...           is just C-c C-t (or doing it from
;;   :END:                          the org agenda)
;;   #+begin_src json ... #+end_src the call's input
;;   *** Preview                    diff shown while awaiting approval
;;   *** Result                     the tool's output (or *** Error)
;;   * Message from reviewer        input from another agent
;;   * Event: after-save-hook       input from a hook the user set up
;;
;; Any other top-level heading (say "* Notes") is the user's own and is
;; not sent; neither is any message tagged :ignore:, nor reasoning text
;; from Chat Completions servers (a "reasoning" block), which is shown
;; but never sent back.
;;
;; Text written by the model round-trips byte for byte: models bind
;; their reasoning to the exact conversation prefix, so a transcript
;; that re-renders a reply even slightly differently would invalidate
;; it on every turn.  Bodies are therefore escaped the way org escapes
;; source blocks (a comma before lines starting with "*" or "#+") and
;; end in exactly one newline the parser strips again.

;;; Code:

(require 'org)
(require 'lispm-core)

(defconst lispm-transcript-todo-line
  "#+todo: WAITING(w) APPROVED(a) RUNNING(r) | DONE(d) DENIED(n) FAILED(f)\n"
  "TODO keywords of a session: the life cycle of a tool call.")

(defconst lispm-transcript--state-re
  "\\(WAITING\\|APPROVED\\|RUNNING\\|DONE\\|DENIED\\|FAILED\\)"
  "Regexp matching a tool-call state keyword.")

;;;; Rendering

(defun lispm-transcript-header (agent-name)
  "File header of a new session of the agent AGENT-NAME."
  (concat (format "#+title: %s %s\n" agent-name
                  (format-time-string "%Y-%m-%d %H:%M"))
          (format "#+lispm_agent: %s\n" agent-name)
          lispm-transcript-todo-line))

(defun lispm-transcript--body (text)
  "TEXT as a section body that `lispm-transcript--text' reads back exactly."
  (concat (org-escape-code-in-string text) "\n"))

(defun lispm-transcript-section (title text)
  "A top-level section with heading TITLE and body TEXT."
  (concat "* " title "\n" (lispm-transcript--body text)))

(defun lispm-transcript--summary (input)
  "A short label for a tool call with INPUT: its main string argument."
  (when (consp input)
    (let ((value (or (cl-loop for key in '(path buffer command query title name to agent id)
                              for value = (alist-get key input)
                              when (stringp value) return value)
                     (cl-loop for (key . value) in input
                              when (and (stringp value) (not (eq key 'why)))
                              return value))))
      (and value (lispm--oneline value 60)))))

(defun lispm-transcript--tool-use (block state)
  "Section text for the tool_use BLOCK, in TODO STATE."
  (let* ((name (alist-get 'name block))
         (input (alist-get 'input block))
         (why (and (consp input) (alist-get 'why input)))
         (summary (lispm-transcript--summary input)))
    (concat "** " state " " name (if summary (concat "  " summary) "") "\n"
            ":PROPERTIES:\n"
            ":TOOL_ID:  " (alist-get 'id block) "\n"
            ":TOOL:     " name "\n"
            ":END:\n"
            (if (stringp why) (concat "- why :: " (lispm--oneline why 200) "\n") "")
            "#+begin_src json\n"
            (lispm-transcript--body (lispm--json-pretty input))
            "#+end_src\n")))

(defun lispm-transcript--raw-title (block)
  "Heading for a content BLOCK that is kept as raw JSON."
  (let ((type (alist-get 'type block)))
    (pcase type
      ("thinking"
       (let ((text (alist-get 'thinking block)))
         (if (and (stringp text) (not (string-blank-p text)))
             (concat "Thinking: " (lispm--oneline text 70))
           "Thinking")))
      ("redacted_thinking" "Redacted thinking")
      ("reasoning" (concat "Reasoning: " (lispm--oneline (alist-get 'text block) 70)))
      ("fallback"
       (format "Fallback: %s -> %s"
               (alist-get 'model (alist-get 'from block))
               (alist-get 'model (alist-get 'to block))))
      (_ (capitalize (replace-regexp-in-string "_" " " (format "%s" type)))))))

(defun lispm-transcript--raw (block)
  "Section text for a content BLOCK kept verbatim as JSON."
  (concat "** " (lispm-transcript--raw-title block) "\n"
          ":PROPERTIES:\n"
          ":BLOCK:    " (format "%s" (alist-get 'type block)) "\n"
          ":END:\n"
          "#+begin_src json\n"
          (lispm-transcript--body (lispm--json-encode block))
          "#+end_src\n"))

(defun lispm-transcript-result (text errorp)
  "Section text for a tool result TEXT; ERRORP marks a failure."
  (concat "*** " (if errorp "Error" "Result") "\n"
          "#+begin_example\n"
          (lispm-transcript--body text)
          "#+end_example\n"))

(defun lispm-transcript-preview (diff)
  "Section text showing DIFF while a call awaits approval."
  (concat "*** Preview\n#+begin_src diff\n"
          (lispm-transcript--body diff)
          "#+end_src\n"))

(defun lispm-transcript-fold (pos)
  "Fold the subtree whose heading starts at POS, ignoring failures."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (org-fold-subtree t))))

(defun lispm-transcript-fold-drawers (beg end)
  "Hide the property drawers between BEG and END."
  (ignore-errors
    (save-restriction
      (narrow-to-region beg end)
      (org-fold-hide-drawer-all))))

(defun lispm-transcript-insert-blocks (content state-function)
  "Insert the assistant CONTENT blocks at point, one section each.
STATE-FUNCTION maps a tool_use block to its initial TODO keyword.
Return an alist (TOOL-ID . MARKER) locating each tool-call heading."
  (let ((beg (point)) calls folds)
    (dolist (block content)
      (let ((start (point)))
        (pcase (alist-get 'type block)
          ("text"
           (insert "** Reply\n" (lispm-transcript--body (alist-get 'text block))))
          ("tool_use"
           (insert (lispm-transcript--tool-use block (funcall state-function block)))
           ;; Results of earlier calls are inserted right where this
           ;; heading starts: the marker must move ahead of them.
           (push (cons (alist-get 'id block) (copy-marker start t)) calls))
          (_
           (push start folds)
           (insert (lispm-transcript--raw block))))))
    (lispm-transcript-fold-drawers beg (point))
    (mapc #'lispm-transcript-fold folds)
    (nreverse calls)))

(defun lispm-transcript-set-state (pos state)
  "Change the TODO keyword of the tool-call heading at POS to STATE."
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((case-fold-search nil))
        (when (looking-at (concat "\\*+ " lispm-transcript--state-re "\\_>"))
          (replace-match state t t nil 1))))))

(defun lispm-transcript-subtree-end (pos level)
  "End of the LEVEL subtree whose heading starts at POS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (if (re-search-forward (format "^\\*\\{1,%d\\} " level) nil t)
        (match-beginning 0)
      (point-max))))

;;;; Parsing

(defun lispm-transcript--sections (level beg end)
  "Sections of exactly LEVEL between BEG and END.
Each is a list (START BODY END): the heading start, the line after
it, and where the section ends."
  (save-excursion
    (goto-char beg)
    (let ((re (format "^\\*\\{%d\\} " level))
          starts)
      (while (re-search-forward re end t)
        (push (match-beginning 0) starts))
      (cl-loop for (start . rest) on (nreverse starts)
               collect (list start
                             (progn (goto-char start)
                                    (min end (1+ (line-end-position))))
                             (or (car rest) end))))))

(defun lispm-transcript--heading (pos)
  "Parse the heading at POS into a plist (:todo :title :tags)."
  (save-excursion
    (goto-char pos)
    (let ((line (buffer-substring-no-properties pos (line-end-position)))
          (case-fold-search nil))
      (when (string-match
             (concat "\\`\\*+ +\\(?:" lispm-transcript--state-re " +\\)?"
                     "\\(.*?\\)\\(?:[ \t]+\\(:\\(?:[[:alnum:]_@#%]+:\\)+\\)\\)?[ \t]*\\'")
             line)
        (list :todo (match-string 1 line)
              :title (match-string 2 line)
              :tags (and (match-string 3 line)
                         (split-string (match-string 3 line) ":" t)))))))

(defun lispm-transcript--text (beg end)
  "Read back text stored by `lispm-transcript--body' between BEG and END."
  (let ((s (buffer-substring-no-properties beg end)))
    (org-unescape-code-in-string
     (if (string-suffix-p "\n" s) (substring s 0 -1) s))))

(defun lispm-transcript--properties (beg end)
  "Alist of the properties in the first drawer between BEG and END."
  (save-excursion
    (goto-char beg)
    (let (props)
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" end t)
        (let ((drawer-end (save-excursion
                            (re-search-forward "^[ \t]*:END:[ \t]*$" end t))))
          (while (re-search-forward "^[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
                                    drawer-end t)
            (unless (equal (match-string 1) "END")
              (push (cons (match-string-no-properties 1)
                          (match-string-no-properties 2))
                    props)))))
      (nreverse props))))

(defun lispm-transcript--block (beg end)
  "Contents of the first #+begin_ block between BEG and END, or nil."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+begin_\\(\\S-+\\).*\n" end t)
        (let ((content-beg (point))
              (kind (match-string 1)))
          (when (re-search-forward
                 (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote kind)) end t)
            (lispm-transcript--text content-beg (match-beginning 0))))))))

(defun lispm-transcript--role (title)
  "Role of a top-level section titled TITLE.
Return `system', `assistant', (user . PREFIX) or nil for the user's
own notes."
  (let ((case-fold-search t))
    (cond ((string-match-p "\\`system\\'" title) 'system)
          ((string-match-p "\\`user\\'" title) '(user . ""))
          ((string-match-p "\\`assistant\\'" title) 'assistant)
          ((string-match "\\`message from \\(.+\\)\\'" title)
           (cons 'user (format "[Message from agent %s]\n" (match-string 1 title))))
          ((string-match "\\`event: *\\(.+\\)\\'" title)
           (cons 'user (format "[Event: %s]\n" (match-string 1 title)))))))

(defun lispm-transcript--text-block (text)
  "A text content block holding TEXT."
  `((type . "text") (text . ,text)))

(defun lispm-transcript--tool-result (id text errorp)
  "A tool_result block for the call ID with TEXT; ERRORP marks failure."
  `((type . "tool_result") (tool_use_id . ,id) (content . ,text)
    ,@(when errorp '((is_error . t)))))

(defun lispm-transcript--parse-assistant (beg end)
  "Parse an assistant section body between BEG and END.
Return (ASSISTANT-BLOCKS RESULT-BLOCKS PENDING-IDS)."
  (let* ((children (lispm-transcript--sections 2 beg end))
         (lead (string-trim (buffer-substring-no-properties
                             beg (if children (caar children) end))))
         blocks results pending)
    (unless (string-empty-p lead)
      (push (lispm-transcript--text-block (org-unescape-code-in-string lead))
            blocks))
    (pcase-dolist (`(,hbeg ,bbeg ,cend) children)
      (let* ((heading (lispm-transcript--heading hbeg))
             (kids (lispm-transcript--sections 3 bbeg cend))
             (own-end (if kids (caar kids) cend))
             (props (lispm-transcript--properties bbeg own-end)))
        (cond
         ((assoc "TOOL_ID" props)
          (let* ((id (cdr (assoc "TOOL_ID" props)))
                 (json (lispm-transcript--block bbeg own-end))
                 (input (and json (ignore-errors (lispm--json-decode json))))
                 (result (cl-loop for (kbeg kbody kend) in kids
                                  for title = (plist-get (lispm-transcript--heading kbeg)
                                                         :title)
                                  when (member title '("Result" "Error"))
                                  return (cons (or (lispm-transcript--block kbody kend) "")
                                               (equal title "Error")))))
            (push `((type . "tool_use") (id . ,id)
                    (name . ,(cdr (assoc "TOOL" props)))
                    (input . ,(if (consp input) input nil)))
                  blocks)
            (cond (result
                   (push (lispm-transcript--tool-result id (car result) (cdr result))
                         results))
                  ((member (plist-get heading :todo) '("WAITING" "APPROVED" "RUNNING"))
                   (push id pending))
                  (t
                   (push (lispm-transcript--tool-result id "(no result recorded)" t)
                         results)))))
         ((equal (cdr (assoc "BLOCK" props)) "reasoning"))
         ((assoc "BLOCK" props)
          (when-let* ((json (lispm-transcript--block bbeg own-end))
                      (raw (ignore-errors (lispm--json-decode json))))
            (push raw blocks)))
         ((equal (plist-get heading :title) "Reply")
          (push (lispm-transcript--text-block (lispm-transcript--text bbeg own-end))
                blocks)))))
    (list (nreverse blocks) (nreverse results) (nreverse pending))))

(defun lispm-transcript--normalize (messages)
  "Merge consecutive MESSAGES of one role; drop leading non-user ones."
  (let (out)
    (dolist (m messages)
      (if (and out (equal (alist-get 'role (car out)) (alist-get 'role m)))
          (setcdr (assq 'content (car out))
                  (vconcat (alist-get 'content (car out)) (alist-get 'content m)))
        (push (copy-alist m) out)))
    (setq out (nreverse out))
    (while (and out (not (equal (alist-get 'role (car out)) "user")))
      (pop out))
    out))

(defun lispm-transcript-parse ()
  "Parse the current session buffer.
Return a plist: :system (the system prompt), :messages (Messages API
messages, oldest first), :pending (ids of tool calls without a result
yet) and :last-role."
  (save-excursion
    (save-restriction
      (widen)
      (let (system messages pending)
        (pcase-dolist (`(,hbeg ,bbeg ,end) (lispm-transcript--sections 1 (point-min) (point-max)))
          (let* ((heading (lispm-transcript--heading hbeg))
                 (role (and heading
                            (not (member "ignore" (plist-get heading :tags)))
                            (lispm-transcript--role (plist-get heading :title)))))
            (pcase role
              ('system (setq system (lispm-transcript--text bbeg end)))
              (`(user . ,prefix)
               (let ((text (string-trim (org-unescape-code-in-string
                                         (buffer-substring-no-properties bbeg end)))))
                 (unless (string-empty-p text)
                   (push `((role . "user")
                           (content . ,(vector (lispm-transcript--text-block
                                                (concat prefix text)))))
                         messages))))
              ('assistant
               (pcase-let ((`(,blocks ,results ,ids)
                            (lispm-transcript--parse-assistant bbeg end)))
                 (when blocks
                   (push `((role . "assistant") (content . ,(vconcat blocks))) messages))
                 (when results
                   (push `((role . "user") (content . ,(vconcat results))) messages))
                 (setq pending (append pending ids)))))))
        (setq messages (lispm-transcript--normalize (nreverse messages)))
        (list :system system
              :messages messages
              :pending pending
              :last-role (alist-get 'role (car (last messages))))))))

(provide 'lispm-transcript)
;;; lispm-transcript.el ends here
