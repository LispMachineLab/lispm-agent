;;; lispm-util.el --- Core utilities, hooks, envelopes, and errors -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared Layer 1 foundation for `lispm-core' and higher modules:
;; customization group, dynamic turn/hook variables, structured error
;; hierarchy, provider-neutral API envelopes/thought blocks, idle work
;; scheduling, JSON sanitization/coding, SSE framing, diff, and buffer helpers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-exec)

(defgroup lispm nil
  "An Emacs-native agent runtime."
  :group 'tools
  :prefix "lispm-")

(defcustom lispm-idle-delay 5
  "Seconds Emacs is idle before LispM does its background work.
That is handing a long session over (`lispm-compact-idle'), keeping
memory within its budget (`lispm-memory-max-entries') and rolling up
episodes (`lispm-episodes'), each once a turn has ended."
  :type 'number)

;;;; Dynamic turn state and hooks

(defvar lispm-current-agent nil
  "The agent on whose behalf the running code acts.
Bound while a tool executes, so tools, and hooks they trigger, know
who is acting.")

(defvar lispm-turn-started-at nil
  "Wall-clock `float-time' when the current turn started, or nil.")

(defvar lispm-turn-deadline nil
  "Wall-clock `float-time' deadline for the current turn, or nil.
When bound to a positive number, long-running tools such as `run_shell'
clamp their per-command timeout so they finish before the deadline.")

(defvar lispm-tool-result-note-function nil
  "Optional function appending a one-time note to a completed tool result.
Called by `lispm-turns-tool-done' with (AGENT CALL TEXT ERRORP) before TEXT
is written into the session transcript.  Because the note is written
once when the tool finishes and never mutated on later turns, it
preserves the prompt-cache prefix.")

(defvar lispm-state-change-functions nil
  "Abnormal hook run with an agent whenever its state changes.")

(defvar lispm-system-prompt-functions nil
  "Functions contributing sections to a new session's system prompt.
Each is called with the agent and returns a string or nil.  The
prompt is frozen when the session starts: changing it later would
invalidate the model's cached prefix.")

(defvar lispm-request-context-functions nil
  "Functions giving live scenario context for an agent's current turn.
Each is called with the agent and returns a string describing the live
usage scenario (such as the active UI entrypoint, open desktop windows,
selected buffer or recent Listener commands), or nil.  The combined
context is captured once per turn and attached to the turn's user
message in requests without altering the transcript buffer or earlier
turns' cached prefix.")

(defvar lispm-session-end-functions nil
  "Abnormal hook run with an agent whose session is closing.")

(defvar lispm-tools-changed-functions nil
  "Abnormal hook run with an agent whose tools changed in mid-session.
That happens when deferred tools are loaded for it: tools search_tools
found, or tools its own code or a skill it loaded defined.")

(defvar lispm-turn-end-functions nil
  "Abnormal hook run with an agent and its final reply after each turn.")

(defvar lispm-handover-functions nil
  "Abnormal hook run when an agent's session was handed over.
Each is called with the agent, the summary the new session starts
from, and the file of the session handed over.")

(defvar lispm-agent-declared-functions nil
  "Abnormal hook run with a standing agent once it is declared.
Declarations come from the user's configuration, as Emacs starts:
what the agent was doing when Emacs last stopped can be taken up.")

(defvar lispm-tool-input-functions nil
  "Abnormal hook run while the model writes a tool call.
Each is called with the agent and a plist: the call's :id and :name,
and :json, its input as received so far, the beginning of a JSON
object.  It runs whenever more of the input arrives.")

(defvar lispm-response-delta-functions nil
  "Abnormal hook run while a response streams in.
Each is called with the agent, the kind of what arrived (`text',
`thinking' or `tool', the name of a tool whose call the model starts
writing) and its text.  `lispm-live-segments' has all of it so far.")

(defvar lispm-response-functions nil
  "Abnormal hook run when a model response is complete.
Each is called with the agent and the message alist.")

(defvar lispm-buffer-change-functions nil
  "Abnormal hook run after a tool changed a buffer's text.
Each is called with the buffer, its old text and its new text;
`lispm-current-agent' is the agent that made the change, if any.")

(defvar lispm-tool-call-functions nil
  "Abnormal hook run when a tool call starts or finishes.
Each is called with (AGENT PHASE TOOL INPUT RESULT ERRORP), where
PHASE is `start' before TOOL runs (RESULT and ERRORP nil) or `finish'
once TOOL has finished.")

;;;; Utilities

(defvar lispm--idle-work (make-hash-table :test #'equal)
  "Idle timers of the background work waiting to be done, by key.")

(defun lispm-cancel-idle (key)
  "Give up the background work waiting under KEY, if any."
  (when-let* ((timer (gethash key lispm--idle-work)))
    (cancel-timer timer)
    (remhash key lispm--idle-work)))

(defun lispm-when-idle (key function &rest args)
  "Call FUNCTION with ARGS once Emacs has been idle for `lispm-idle-delay' seconds.
Work waiting under KEY already, such as the same work for the same
agent, is replaced.  Errors are reported, not raised."
  (lispm-cancel-idle key)
  (puthash key
           (run-with-idle-timer lispm-idle-delay nil
                                (lambda ()
                                  (remhash key lispm--idle-work)
                                  (with-demoted-errors "LispM: background work: %S"
                                    (apply function args))))
           lispm--idle-work))

(defconst lispm--json-invalid-char-regexp
  (string ?\[ #xd800 ?- #xdfff #x110000 ?- #x3fff7f #x3fff80 ?- #x3fffff ?\])
  "Regexp matching surrogates, non-Unicode chars and raw 8-bit bytes.")

(defun lispm--json-sanitize-string (s)
  "Return S with any non-Unicode or surrogate characters replaced by U+FFFD."
  (let ((case-fold-search nil)
        (m (if (multibyte-string-p s) s (decode-coding-string s 'utf-8))))
    (if (string-match-p lispm--json-invalid-char-regexp m)
        (replace-regexp-in-string lispm--json-invalid-char-regexp "\uFFFD" m t t)
      m)))

(defun lispm--json-sanitize (object)
  "Sanitize strings in OBJECT so `json-serialize' accepts them."
  (cond
   ((stringp object) (lispm--json-sanitize-string object))
   ((consp object)
    (cons (lispm--json-sanitize (car object))
          (lispm--json-sanitize (cdr object))))
   ((vectorp object)
    (vconcat (mapcar #'lispm--json-sanitize object)))
   ((hash-table-p object)
    (let ((copy (copy-hash-table object)))
      (maphash (lambda (k v)
                 (puthash k (lispm--json-sanitize v) copy))
               object)
      copy))
   (t object)))

(defun lispm-json-serialize (object &rest args)
  "Like `json-serialize', sanitizing raw bytes on `json-value-p' error."
  (condition-case nil
      (apply #'json-serialize object args)
    (wrong-type-argument
     (apply #'json-serialize (lispm--json-sanitize object) args))))

(defun lispm-json-encode (object)
  "Encode OBJECT as a JSON string (decoded text, not bytes)."
  (decode-coding-string (lispm-json-serialize object) 'utf-8))

(defun lispm-json-decode (string)
  "Decode the JSON STRING.
Objects become alists with symbol keys, arrays vectors, null `:null'
and false `:false', so a decoded value encodes back to the same JSON."
  (json-parse-string string :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun lispm-json-pretty (object &optional indent)
  "Encode OBJECT as indented JSON, starting at column INDENT."
  (let* ((indent (or indent 0))
         (pad (make-string (+ indent 2) ?\s))
         (close (make-string indent ?\s)))
    (cond
     ((and (consp object) (consp (car object)))
      (concat "{\n"
              (mapconcat (lambda (kv)
                           (concat pad (lispm-json-encode (symbol-name (car kv)))
                                   ": " (lispm-json-pretty (cdr kv) (+ indent 2))))
                         object ",\n")
              "\n" close "}"))
     ((null object) "{}")
     ((and (vectorp object)
           (cl-some (lambda (x) (or (consp x) (vectorp x))) object))
      (concat "[\n"
              (mapconcat (lambda (x) (concat pad (lispm-json-pretty x (+ indent 2))))
                         object ",\n")
              "\n" close "]"))
     ((vectorp object) (lispm-json-encode object))
     (t (let ((s (lispm-json-encode (vector object))))
          (substring s 1 -1))))))

(defun lispm-sse-split (text)
  "Split TEXT, the unparsed part of a server-sent event stream, into events.
Return (PAYLOADS . REST): the data of each complete event, in order,
and the incomplete remainder."
  (let ((start 0) payloads)
    (while (string-match "\r?\n\r?\n" text start)
      (let ((event (substring text start (match-beginning 0))))
        (setq start (match-end 0))
        (let ((data (mapconcat (lambda (line) (string-remove-prefix " " (substring line 5)))
                               (seq-filter (lambda (line) (string-prefix-p "data:" line))
                                           (split-string event "\r?\n"))
                               "\n")))
          (unless (string-empty-p data)
            (push data payloads)))))
    (cons (nreverse payloads) (substring text start))))

(defun lispm-truncate (string limit)
  "Shorten STRING to about LIMIT characters, keeping its head and tail."
  (if (<= (length string) limit)
      string
    (let ((head (floor (* limit 0.6)))
          (tail (floor (* limit 0.4))))
      (concat (substring string 0 head)
              (format "\n\n[... %d characters omitted ...]\n\n"
                      (- (length string) head tail))
              (substring string (- (length string) tail))))))

(defun lispm-oneline (string &optional width)
  "STRING collapsed to a single line of at most WIDTH (default 72) columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim (or string "")))
   (or width 72) nil nil "…"))

(defun lispm-buffer-current-p (buffer)
  "Non-nil if BUFFER holds the text to go by for the file it visits.
It does unless the file changed on disk since and BUFFER has no
unsaved changes: then it is out of date, and the file holds the text.
A buffer with unsaved changes holds what the user sees, and so does
one visiting no file."
  (or (buffer-modified-p buffer)
      (verify-visited-file-modtime buffer)))

(defun lispm-file-text (file)
  "Contents of FILE, from its buffer if one visits it and is not out of date.
See `lispm-buffer-current-p'.  The empty string if FILE cannot be read."
  (if-let* ((buf (find-buffer-visiting file))
            ((lispm-buffer-current-p buf)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defvar lispm-inhibit-triggers)

(defvar lispm--atomic-write-seq 0
  "Monotonic counter for unique temporary filenames in atomic writes.")

(defun lispm-file-sha256 (file)
  "Return the SHA-256 hex digest of FILE on disk, or nil when missing."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun lispm-write-file-atomic (file content &optional expected-hash temp-dir)
  "Atomically write CONTENT to FILE via a unique temp file and `rename-file'.
When EXPECTED-HASH is `:missing', signal an error if FILE already exists.
When EXPECTED-HASH is a string, signal an error unless `lispm-file-sha256'
matches EXPECTED-HASH.  When TEMP-DIR is non-nil, stage the temporary
file inside TEMP-DIR instead of FILE's parent directory."
  (let* ((target (expand-file-name file))
         (parent (file-name-directory target))
         (stage-dir (file-name-as-directory (expand-file-name (or temp-dir parent)))))
    (make-directory parent t)
    (make-directory stage-dir t)
    (when expected-hash
      (let ((actual (lispm-file-sha256 target)))
        (cond
         ((eq expected-hash :missing)
          (when actual
            (error "CAS conflict on %s: file already exists" (abbreviate-file-name target))))
         ((not (equal actual expected-hash))
          (error "CAS conflict on %s: expected %s, found %s"
                 (abbreviate-file-name target) expected-hash (or actual "missing"))))))
    (let* ((tmp (expand-file-name
                 (format ".tmp-%d-%d-%06x"
                         (emacs-pid) (cl-incf lispm--atomic-write-seq) (random #x1000000))
                 stage-dir))
           (renamed nil))
      (unwind-protect
          (progn
            (let ((coding-system-for-write (or coding-system-for-write 'utf-8-unix))
                  (inhibit-message t)
                  (lispm-inhibit-triggers t))
              (write-region content nil tmp nil 'silent))
            (if (eq expected-hash :missing)
                (condition-case nil
                    (progn
                      (add-name-to-file tmp target nil)
                      (ignore-errors (delete-file tmp))
                      (setq renamed t))
                  (file-already-exists
                   (error "CAS conflict on %s: file already exists"
                          (abbreviate-file-name target)))
                  (file-error
                   (condition-case nil
                       (progn
                         (rename-file tmp target nil)
                         (setq renamed t))
                     (file-already-exists
                      (error "CAS conflict on %s: file already exists"
                             (abbreviate-file-name target))))))
              (rename-file tmp target t)
              (setq renamed t))
            target)
        (unless renamed
          (ignore-errors (delete-file tmp)))))))

(defun lispm-git-environment ()
  "`process-environment' without the variables that steer git from outside.
Git gives them to the commands it runs, such as hooks and those of
rebase -x: GIT_DIR and the like name its repository, GIT_AUTHOR_* and
GIT_COMMITTER_* the author and date of commits, GIT_CONFIG_* its `git -c'
settings.  Git run by an Emacs started so would commit to that
repository, as that author, not to the one in `default-directory'."
  (seq-remove (lambda (entry)
                (string-match-p
                 (concat "\\`GIT_\\(?:DIR\\|WORK_TREE\\|INDEX_FILE\\|OBJECT_DIRECTORY\\|ALTERNATE_OBJECT_DIRECTORIES"
                         "\\|COMMON_DIR\\|CEILING_DIRECTORIES\\|NAMESPACE\\|PREFIX"
                         "\\|\\(?:AUTHOR\\|COMMITTER\\)_\\(?:NAME\\|EMAIL\\|DATE\\)"
                         "\\|CONFIG_\\(?:PARAMETERS\\|COUNT\\)\\)=")
                 entry))
              process-environment))

(defun lispm-diff (old new label &optional callback)
  "Unified diff from string OLD to string NEW, with file LABEL.
With CALLBACK, return nil, and call CALLBACK with the diff instead,
once `diff' has run in the background (see `lispm-exec'), or with nil
and a sentence saying why it did not: later, never before this returns."
  (if (equal old new)
      (if callback (ignore (run-at-time 0 nil callback "(no changes)")) "(no changes)")
    (let* ((a (make-temp-file "lispm-a"))
           (b (make-temp-file "lispm-b"))
           (args (list "-u" "--label" (concat "a/" label) "--label" (concat "b/" label) a b))
           ;; The files are named in full: a directory that is remote,
           ;; or gone, would only keep diff from running.
           (default-directory temporary-file-directory)
           (later nil))
      (unwind-protect
          (let ((coding-system-for-write 'utf-8))
            (write-region old nil a nil 'silent)
            (write-region new nil b nil 'silent)
            (if (not callback)
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8))
                    (apply #'call-process "diff" nil t nil args))
                  (buffer-string))
              (lispm-exec "diff" args
                          (lambda (exit output errors)
                            (delete-file a)
                            (delete-file b)
                            ;; 1 when the files differ, 0 when they do not.
                            (if (memq exit '(0 1))
                                (funcall callback output)
                              (funcall callback nil (if (stringp exit) exit (string-trim errors))))))
              (setq later t)
              nil))
        (unless later
          (delete-file a)
          (delete-file b))))))

(defun lispm-disable-line-numbers ()
  "Keep line numbers off in the current LispM buffer, even under
`global-display-line-numbers-mode'."
  (setq-local display-line-numbers nil)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (bound-and-true-p display-line-numbers-mode)
                (display-line-numbers-mode -1))
              (setq-local display-line-numbers nil))
            90 t))

;;;; API envelopes and canonical content blocks

(cl-defstruct (lispm-api-envelope (:constructor lispm-api-envelope--create)
                                  (:copier nil))
  "Provider-agnostic HTTP request envelope for a model API call.
BODY is the JSON-serializable payload alist.
HEADERS is an optional list of provider-specific extra HTTP header
strings (excluding authorization/API key headers).
META is an optional plist of provider-specific metadata for inspection."
  body
  headers
  meta)

(cl-defun lispm-api-make-envelope (body &key headers meta)
  "Build a `lispm-api-envelope' for BODY with optional HEADERS and META."
  (lispm-api-envelope--create :body body :headers headers :meta meta))

(defun lispm-thought-block (text &optional signature ephemeral)
  "Construct a canonical provider-neutral thought content block for TEXT.
SIGNATURE is an optional provider verification signature.  EPHEMERAL,
when non-nil, marks a display-only thought block never replayed back to
model APIs (for example Chat Completions reasoning text)."
  `((type . "thinking")
    (thinking . ,text)
    ,@(when signature `((signature . ,signature)))
    ,@(when ephemeral '((ephemeral . t)))))

(defun lispm-thought-block-p (block)
  "Return non-nil if BLOCK is a thought/reasoning content block.
Recognizes canonical \"thinking\" blocks and legacy \"reasoning\" blocks
read from older transcripts."
  (and (consp block)
       (member (alist-get 'type block) '("thinking" "reasoning"))
       t))

(defun lispm-thought-block-text (block)
  "Return non-blank thought text of BLOCK, or nil."
  (when (lispm-thought-block-p block)
    (let ((text (or (alist-get 'thinking block)
                    (alist-get 'text block))))
      (and (stringp text) (not (string-blank-p text)) text))))

(defun lispm-block-replayable-p (block)
  "Return non-nil if raw transcript BLOCK should be replayed to model APIs.
Drops legacy \"reasoning\" blocks and canonical ephemeral thought blocks."
  (and (consp block)
       (not (equal (alist-get 'type block) "reasoning"))
       (not (alist-get 'ephemeral block))))

;;;; Error hierarchy, observable hooks, and UI dispatch

(define-error 'lispm-error "LispM error")
(define-error 'lispm-api-error "LispM API error" 'lispm-error)
(define-error 'lispm-tool-error "LispM tool error" 'lispm-error)
(define-error 'lispm-mcp-error "LispM MCP error" 'lispm-error)
(define-error 'lispm-session-error "LispM session error" 'lispm-error)
(define-error 'lispm-collab-error "LispM collaboration error" 'lispm-error)

(defvar lispm-change-functions nil
  "Abnormal hook run with (SUBJECT FIELDS EVENT) on agent state changes.
SUBJECT is an agent, buffer, (KIND SESSION-ID) key, or nil.
FIELDS is a list of changed field symbols; EVENT is an optional plist.")

(defun lispm-emit-change (subject fields &optional event)
  "Notify `lispm-change-functions' observers of SUBJECT's FIELDS change."
  (when lispm-change-functions
    (run-hook-with-args 'lispm-change-functions subject fields event)))

(defvar lispm-model-change-functions nil
  "Abnormal hook run with (AGENT) when AGENT switches backend or model.")

(defvar lispm-show-agent-function nil
  "Function called with (AGENT) to open or focus AGENT's primary UI view.")

(defvar lispm-open-transcript-function nil
  "Function called with (AGENT) to display AGENT's transcript buffer.")

(defvar lispm-open-dashboard-function nil
  "Function called with no arguments to open the LispM dashboard.")

(defvar lispm-read-session-function nil
  "Function called with (PROMPT) to prompt for a saved session file.")

(defun lispm-content-text (content)
  "Return the joined, trimmed text blocks of API message CONTENT."
  (string-trim
   (mapconcat (lambda (b) (if (equal (alist-get 'type b) "text") (alist-get 'text b) ""))
              content "")))

(defalias 'lispm--reply-text #'lispm-content-text)

(provide 'lispm-util)
;;; lispm-util.el ends here
