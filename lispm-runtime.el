;;; lispm-runtime.el --- The agent execution loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The loop that keeps agents running inside the Emacs event loop.
;; Nothing here blocks: a model request is a curl process whose output
;; streams into the session buffer, tool calls run as ordinary Lisp
;; (or as processes, for shell commands), and approvals are org TODO
;; state changes the user makes whenever they like.  Several agents
;; can therefore be mid-turn at once, interleaved by the command loop.
;;
;; A turn:
;;
;;   input (typed, or from the inbox: another agent, a hook, a timer)
;;     -> request: parse the session buffer, stream the response into it
;;     -> response with tool calls: each call runs, waits for approval
;;        (TODO state WAITING) or is refused, per the agent's policy;
;;        when every call has a result, request again
;;     -> response without tool calls: the turn ends, the session is
;;        saved (and committed), turn callbacks and hooks run, and the
;;        next inbox item, if any, starts a new turn.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)

(defcustom lispm-save-sessions t
  "Whether to save session files at the end of every turn."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-approval-functions nil
  "Hook run with an agent, a tool name and its input when a call awaits approval.
Use it for notifications, e.g. with the alert package."
  :type 'hook
  :group 'lispm)

(defvar lispm-inhibit-triggers nil
  "Non-nil while the runtime itself causes hook runs that are not events.")

(defvar-local lispm-session-agent nil
  "The agent whose session this buffer holds.")

(defvar-local lispm--live nil
  "Markers (START . END) of the response being streamed in, or nil.")

(defvar-local lispm--live-kind nil
  "Kind of the content last streamed into the live region.")

(defvar org-state)

;;;; System prompt

(defun lispm--environment-prompt (agent)
  "The part of the system prompt describing AGENT's situation."
  (concat
   "# Your environment\n"
   "- Your root directory is " (abbreviate-file-name (lispm-agent-root agent))
   ". Relative paths are resolved against it.\n"
   "- This conversation is an org-mode buffer the user can read, fold, edit and"
   " annotate while you work. What it contains is what you remember of this session.\n"
   "- Calls that change files or run code may wait for the user's approval. Such"
   " calls take a `why` argument: one plain sentence the user reads when deciding."
   " If a call is denied, adapt; do not repeat it unchanged.\n"
   "- Files can be open in Emacs with unsaved changes. read_file and the edit tools"
   " work on the live buffer, so you see what the user sees; every edit you make is"
   " one undo step for them.\n"
   "- Input beginning with \"[Message from agent X]\" comes from another agent, and"
   " \"[Event: ...]\" from a hook or timer the user set up. Treat them as requests"
   " from colleagues: anything consequential still needs the user's intent.\n\n"
   "# Working style\n"
   "- Before your first tool call in a turn, say in one sentence what you are about"
   " to do. End the turn with a short summary: what you did, what you found, and"
   " anything the user needs to do.\n"
   "- Read before you edit; prefer small, verifiable steps.\n"
   "- Keep durable knowledge (user preferences, project conventions, lessons) in"
   " your long-term memory, and work that spans sessions in your task list."))

(defun lispm-system-prompt (agent)
  "System prompt of a new session of AGENT."
  (string-join
   (seq-remove
    (lambda (s) (or (null s) (string-blank-p s)))
    (append
     (list (format "You are \"%s\", an AI agent living inside a running GNU Emacs %s session, working alongside its human user. You share one programmable environment with them: files, buffers, processes and Emacs itself."
                   (lispm-agent-name agent) emacs-version)
           (lispm-agent-instructions agent)
           (lispm--environment-prompt agent))
     (mapcar (lambda (f) (funcall f agent)) lispm-system-prompt-functions)))
   "\n\n"))

;;;; Sessions

(defvar-keymap lispm-session-mode-map
  :doc "Keymap of `lispm-session-mode'."
  "C-c RET" #'lispm-send
  "C-c C-k" #'lispm-abort
  "C-c C-y" #'lispm-review)

(defun lispm--keyword-agent ()
  "The agent named by the #+lispm_agent keyword of the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+lispm_agent: *\\(\\S-+\\)" 4000 t)
        (lispm-get-agent (intern (match-string-no-properties 1)) t)))))

(define-minor-mode lispm-session-mode
  "Minor mode of agent session transcripts.
Write under the last \"* User\" heading and press \\[lispm-send].
Tool calls awaiting approval are WAITING headings: review them with
\\[lispm-review], or approve and deny by changing their TODO state.

\\{lispm-session-mode-map}"
  :lighter " λ"
  (if lispm-session-mode
      (progn
        (unless lispm-session-agent
          (setq lispm-session-agent (lispm--keyword-agent)))
        (add-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed nil t)
        (setq-local header-line-format '(:eval (lispm--header-line)))
        (let ((agent lispm-session-agent))
          (when (and agent (not (lispm-agent-busy-p agent)))
            (setf (lispm-agent-buffer agent) (current-buffer)))))
    (remove-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed t)
    (kill-local-variable 'header-line-format)))

(defun lispm-session-maybe-enable ()
  "Enable `lispm-session-mode' in org buffers that are agent sessions."
  (when (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (re-search-forward "^#\\+lispm_agent:" 4000 t)))
    (lispm-session-mode 1)))

(defun lispm--header-line ()
  "Header line of a session buffer."
  (let ((agent lispm-session-agent))
    (if (not agent)
        " λ no agent: is it defined with `lispm-defagent'?"
      (let ((state (lispm-agent-state agent)))
        (concat " λ " (symbol-name (lispm-agent-name agent)) "  "
                (propertize (symbol-name state)
                            'face (if (eq state 'waiting) 'warning 'success))
                (if (lispm-agent-status agent)
                    (concat ": " (lispm-agent-status agent))
                  "")
                "    "
                (propertize (pcase state
                              ('idle "C-c RET send")
                              ('waiting "C-c C-y review · C-c C-k abort")
                              (_ "C-c C-k abort"))
                            'face 'shadow))))))

(defun lispm--new-session-file (dir)
  "A fresh session file name in DIR."
  (let* ((stem (format-time-string "%Y%m%d-%H%M%S"))
         (file (expand-file-name (concat stem ".org") dir))
         (n 1))
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d.org" stem (cl-incf n)) dir)))
    file))

(defun lispm-session-buffer (agent &optional new)
  "AGENT's session buffer; with NEW, start a fresh session."
  (let ((buffer (lispm-agent-buffer agent)))
    (if (and (buffer-live-p buffer) (not new))
        buffer
      (when (lispm-agent-busy-p agent)
        (user-error "%s is busy" (lispm-agent-name agent)))
      (let* ((dir (expand-file-name "sessions/" (lispm-agent-directory agent)))
             (file (progn (make-directory dir t) (lispm--new-session-file dir)))
             (coding-system-for-write 'utf-8))
        (write-region (concat (lispm-transcript-header (symbol-name (lispm-agent-name agent)))
                              (lispm-transcript-section "System" (lispm-system-prompt agent))
                              "* User\n")
                      nil file nil 'silent)
        (setq buffer (find-file-noselect file))
        (with-current-buffer buffer
          (setq lispm-session-agent agent)
          (lispm-session-mode 1)
          (goto-char (point-min))
          (when (re-search-forward "^\\* System" nil t)
            (lispm-transcript-fold (match-beginning 0)))
          (goto-char (point-max)))
        (setf (lispm-agent-buffer agent) buffer)
        buffer))))

(defmacro lispm--in-session (agent &rest body)
  "Run BODY in AGENT's session buffer, at its end.
Windows that showed the end of the buffer keep following it."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer"))
        (followers (make-symbol "followers"))
        (at-end (make-symbol "at-end")))
    `(let ((,buffer (lispm-agent-buffer ,agent)))
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           (let* ((inhibit-read-only t)
                  (,followers (seq-filter (lambda (w) (>= (window-point w) (point-max)))
                                          (get-buffer-window-list ,buffer nil t)))
                  (,at-end (>= (point) (point-max))))
             (prog1 (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-max))
                        ,@body))
               (when ,at-end (goto-char (point-max)))
               (dolist (w ,followers) (set-window-point w (point-max))))))))))

(defun lispm--newline ()
  "Make sure point is at the beginning of a line."
  (unless (bolp) (insert "\n")))

(defun lispm--input-heading-p ()
  "Non-nil if the last section of the buffer is an empty \"* User\"."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^\\* " nil t)
         (looking-at "\\* User[ \t]*\\(?:\n[ \t\n]*\\)?\\'")
         (point))))

(defun lispm--save-session (agent)
  "Save AGENT's session file."
  (let ((buffer (lispm-agent-buffer agent)))
    (when (and lispm-save-sessions (buffer-live-p buffer) (buffer-file-name buffer))
      (with-current-buffer buffer
        (let ((inhibit-message t)
              (lispm-inhibit-triggers t))
          (save-buffer))))))

(defun lispm--record-event (agent title text)
  "Note TEXT as an event in AGENT's session without starting a turn.
The model reads it with the next input."
  (lispm--in-session agent
    (let ((input (lispm--input-heading-p)))
      (if input
          (goto-char input)
        (lispm--newline))
      (insert (lispm-transcript-section (concat "Event: " title) text)))))

;;;; Turns

(defun lispm--entry-title (entry)
  "Heading of the section holding inbox ENTRY."
  (cond ((plist-get entry :from) (format "Message from %s" (plist-get entry :from)))
        ((plist-get entry :event) (format "Event: %s" (plist-get entry :event)))
        (t "User")))

(cl-defun lispm-tell (agent text &key from event callback)
  "Give AGENT the input TEXT; it starts working as soon as it is idle.
FROM names the agent sending it; EVENT describes the hook or timer
that produced it (pending events with the same description are
coalesced).  CALLBACK, if given, is called with the agent's final
reply when the turn handling TEXT ends."
  (let* ((agent (lispm-get-agent agent))
         (inbox (lispm-agent-inbox agent))
         (same (and event (seq-find (lambda (e) (equal (plist-get e :event) event)) inbox))))
    (lispm-session-buffer agent)
    (if same
        (plist-put same :text text)
      (setf (lispm-agent-inbox agent)
            (append inbox (list (list :text text :from from :event event
                                      :callback callback)))))
    (run-at-time 0 nil #'lispm--drain-inbox agent)
    agent))

(defun lispm--drain-inbox (agent)
  "Start a turn on AGENT's next inbox item if it is idle."
  (when (and (not (lispm-agent-busy-p agent)) (lispm-agent-inbox agent))
    (let ((entry (pop (lispm-agent-inbox agent))))
      (lispm-session-buffer agent)
      (lispm--begin-turn agent entry))))

(defun lispm--begin-turn (agent &optional entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed."
  (cl-incf (lispm-agent-turn agent))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-last-reply agent) nil
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback))))
  (when entry
    (lispm--in-session agent
      (let ((input (lispm--input-heading-p)))
        (when input (delete-region input (point-max))))
      (lispm--newline)
      (insert (lispm-transcript-section (lispm--entry-title entry) (plist-get entry :text)))))
  (lispm--request agent))

(defun lispm-request-body (agent)
  "Request for AGENT's next model call, as (BODY . BETAS).
Signal a `user-error' if the session cannot be sent as it is."
  (let ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse))))
    (when (plist-get parsed :pending)
      (user-error "Tool calls still await a result: %s"
                  (string-join (plist-get parsed :pending) ", ")))
    (unless (equal (plist-get parsed :last-role) "user")
      (user-error "Nothing to send: write under a \"* User\" heading"))
    (lispm-api-request (lispm-agent-backend* agent)
                       (lispm-agent-model* agent)
                       (lispm-agent-effort* agent)
                       (plist-get parsed :system)
                       (mapcar #'lispm-tool-schema (lispm-agent-tools* agent))
                       (plist-get parsed :messages))))

(defun lispm--request (agent)
  "Ask the model for AGENT's next step."
  (cond
   ((not (buffer-live-p (lispm-agent-buffer agent)))
    (lispm-set-state agent 'idle "session buffer was killed"))
   ((>= (lispm-agent-steps agent) lispm-max-steps)
    (lispm--finish-turn agent (format "Stopped after %d model requests (`lispm-max-steps')."
                                      lispm-max-steps)))
   (t
    (pcase (condition-case err
               (lispm-request-body agent)
             (error (error-message-string err)))
      ((and (pred stringp) problem) (lispm--finish-turn agent problem))
      (`(,body . ,betas)
       (cl-incf (lispm-agent-steps agent))
       (lispm--send agent body betas 0))))))

(defun lispm--send (agent body betas attempt)
  "Send the request BODY with BETAS for AGENT; ATTEMPT counts retries."
  (lispm-set-state agent 'requesting
                   (if (> attempt 0) (format "retry %d" attempt) "thinking"))
  (lispm--live-begin agent)
  (let ((proc (funcall lispm-api-function (lispm-agent-backend* agent) body betas
                       :on-delta (lambda (kind text) (lispm--live-insert agent kind text))
                       :on-done (lambda (message) (lispm--on-message agent message))
                       :on-error (lambda (err) (lispm--on-error agent err body betas attempt)))))
    (when (processp proc)
      (process-put proc 'lispm-agent agent))
    (setf (lispm-agent-process agent) proc)))

(defun lispm--live-begin (agent)
  "Open the region where AGENT's response streams in."
  (lispm--in-session agent
    (lispm--newline)
    (let ((start (point-marker)))
      (insert "* Assistant\n")
      (setq lispm--live (cons start (copy-marker (point) t))
            lispm--live-kind nil))))

(defun lispm--live-insert (agent kind text)
  "Show streamed content TEXT of KIND (text, thinking or tool) for AGENT."
  (lispm--in-session agent
    (when lispm--live
      (goto-char (cdr lispm--live))
      (unless (eq kind lispm--live-kind)
        (lispm--newline)
        (setq lispm--live-kind kind))
      (pcase kind
        ('text (insert text))
        ('thinking (insert (propertize text 'face 'shadow)))
        ('tool (insert (propertize (format "-> %s ...\n" text) 'face 'shadow))))))
  (when (eq kind 'tool)
    (lispm-set-state agent 'requesting (format "writing a %s call" text))))

(defun lispm--live-delete (agent)
  "Remove AGENT's streamed region; the final message replaces it."
  (lispm--in-session agent
    (when lispm--live
      (delete-region (car lispm--live) (max (car lispm--live) (cdr lispm--live)))
      (set-marker (car lispm--live) nil)
      (set-marker (cdr lispm--live) nil)
      (setq lispm--live nil))))

(defun lispm--add-usage (agent usage)
  "Add the token USAGE of a response to AGENT's totals."
  (let ((totals (lispm-agent-usage agent)))
    (cl-loop for key in '(input_tokens output_tokens cache_read_input_tokens)
             for i from 0
             for n = (alist-get key usage)
             when (numberp n) do (cl-incf (nth i totals) n))))

(defun lispm--reply-text (content)
  "Text blocks of CONTENT joined."
  (string-trim
   (mapconcat (lambda (b) (if (equal (alist-get 'type b) "text") (alist-get 'text b) ""))
              content "")))

(defun lispm--stop-note (stop message)
  "A note explaining stop reason STOP of MESSAGE, or nil if it is normal."
  (pcase stop
    ((or "end_turn" "stop_sequence" "tool_use" 'nil :null) nil)
    ("max_tokens" "The response hit the output limit (`lispm-max-tokens').")
    ("refusal"
     (let ((details (alist-get 'stop_details message)))
       (format "The model declined to continue%s."
               (if (and (consp details) (stringp (alist-get 'category details)))
                   (format " (%s)" (alist-get 'category details))
                 ""))))
    ("pause_turn" "The model paused; send any message to let it continue.")
    (_ (format "The response stopped: %s." stop))))

(defun lispm--decide (agent block invalid stopped)
  "Decide what to do with the tool_use BLOCK of AGENT.
INVALID lists calls with malformed input; STOPPED is the stop reason
if the response ended abnormally.  Return (STATE ACTION MESSAGE)."
  (let* ((name (alist-get 'name block))
         (tool (lispm-get-tool name)))
    (cond
     (stopped
      (list "FAILED" 'fail (format "Not executed: the response stopped (%s) before the call was complete." stopped)))
     ((member (alist-get 'id block) invalid)
      (list "FAILED" 'fail "INVALID_JSON: the input of this call was not valid JSON. Make the call again."))
     ((not (memq tool (lispm-agent-tools* agent)))
      (list "FAILED" 'fail (format "There is no tool named %s available to you." name)))
     (t
      (pcase (lispm-tool-action agent tool (alist-get 'input block))
        ('auto (list "RUNNING" 'run))
        ('ask (list "WAITING" 'ask))
        (_ (list "DENIED" 'deny
                 (format "Denied by policy: %s may not use %s tools."
                         (lispm-agent-name agent) (lispm-tool-permission tool)))))))))

(defun lispm--on-message (agent message)
  "Handle the complete response MESSAGE for AGENT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm--add-usage agent (alist-get 'usage message))
    (lispm--live-delete agent)
    (let* ((content (append (alist-get 'content message) nil))
           (stop (alist-get 'stop_reason message))
           (uses (seq-filter (lambda (b) (equal (alist-get 'type b) "tool_use")) content))
           ;; Some gateways report end_turn after tool calls; only a
           ;; response cut short leaves its calls unexecuted.
           (proceed (not (member stop '("max_tokens" "refusal" "model_context_window_exceeded"))))
           (decisions (mapcar (lambda (b)
                                (cons (alist-get 'id b)
                                      (lispm--decide agent b (alist-get 'lispm-invalid message)
                                                     (unless proceed stop))))
                              uses))
           calls)
      (setf (lispm-agent-last-reply agent) (lispm--reply-text content))
      (when content
        (setq calls (lispm--in-session agent
                      (lispm--newline)
                      (insert "* Assistant\n")
                      (lispm-transcript-insert-blocks
                       content
                       (lambda (b) (car (alist-get (alist-get 'id b) decisions nil nil #'equal)))))))
      (when uses
        (lispm--dispatch agent uses decisions calls proceed))
      (unless (and uses proceed)
        (lispm--finish-turn agent (lispm--stop-note stop message))))))

(defun lispm--dispatch (agent uses decisions calls proceed)
  "Act on the tool calls USES of AGENT per DECISIONS.
CALLS locates their headings.  If PROCEED, request the next step
once every call has a result."
  (let ((pending (lispm-agent-pending agent)))
    (setf (lispm-agent-dispatching agent) t)
    (unwind-protect
        (progn
          ;; Register every call before running any, so that the first
          ;; result cannot resume the loop while others are unregistered.
          (dolist (b uses)
            (let ((id (alist-get 'id b)))
              (puthash id (list :status (nth 1 (alist-get id decisions nil nil #'equal))
                                :name (alist-get 'name b)
                                :tool (lispm-get-tool (alist-get 'name b))
                                :input (alist-get 'input b)
                                :marker (alist-get id calls nil nil #'equal))
                       pending)))
          (lispm-set-state agent 'acting)
          (dolist (b uses)
            (let* ((id (alist-get 'id b))
                   (decision (alist-get id decisions nil nil #'equal)))
              (pcase (nth 1 decision)
                ('run (lispm--run-tool agent id))
                ('ask (lispm--ask-approval agent id))
                (_ (lispm--tool-done agent id (nth 2 decision) t (car decision)))))))
      (setf (lispm-agent-dispatching agent) nil))
    (lispm--update-state agent)
    (when proceed
      (lispm--maybe-continue agent))))

(defun lispm--run-tool (agent id)
  "Run AGENT's pending tool call ID."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (plist-put call :status 'run)
    (lispm--in-session agent
      (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
    (lispm-set-state agent 'acting (format "running %s" (plist-get call :name)))
    (let ((lispm-current-agent agent)
          (default-directory (lispm-agent-root agent)))
      (lispm-tool-invoke (plist-get call :tool) (plist-get call :input)
                         (lambda (text errorp) (lispm--tool-done agent id text errorp))))))

(defun lispm--tool-done (agent id text errorp &optional state)
  "Record TEXT as the result of AGENT's call ID; ERRORP marks failure.
STATE overrides the call's final TODO keyword."
  (when-let* ((call (gethash id (lispm-agent-pending agent))))
    (remhash id (lispm-agent-pending agent))
    (let ((marker (plist-get call :marker)))
      (lispm--in-session agent
        (when (and marker (marker-buffer marker))
          (lispm-transcript-set-state marker (or state (if errorp "FAILED" "DONE")))
          (goto-char (lispm-transcript-subtree-end marker 2))
          (let ((start (point)))
            (insert (lispm-transcript-result text errorp))
            (when (> (cl-count ?\n text) 12)
              (lispm-transcript-fold start))))))
    (unless (lispm-agent-dispatching agent)
      (lispm--update-state agent)
      (lispm--maybe-continue agent))))

(defun lispm--update-state (agent)
  "Reflect AGENT's pending calls in its state."
  (when (memq (lispm-agent-state agent) '(acting waiting))
    (let ((waiting 0) (running nil))
      (maphash (lambda (_id call)
                 (if (eq (plist-get call :status) 'ask)
                     (cl-incf waiting)
                   (setq running (plist-get call :name))))
               (lispm-agent-pending agent))
      (cond ((> waiting 0)
             (lispm-set-state agent 'waiting
                              (format "%d call%s await%s approval" waiting
                                      (if (= waiting 1) "" "s") (if (= waiting 1) "s" ""))))
            (running (lispm-set-state agent 'acting (format "running %s" running)))))))

(defun lispm--maybe-continue (agent)
  "Request AGENT's next step once all its tool calls have results."
  (when (and (memq (lispm-agent-state agent) '(acting waiting))
             (zerop (hash-table-count (lispm-agent-pending agent))))
    (lispm--request agent)))

(defun lispm--finish-turn (agent &optional note)
  "End AGENT's turn, adding NOTE to the transcript if non-nil."
  (let ((callbacks (lispm-agent-turn-callbacks agent))
        (reply (or (lispm-agent-last-reply agent) "")))
    (setf (lispm-agent-turn-callbacks agent) nil
          (lispm-agent-process agent) nil
          (lispm-agent-waiting-on agent) nil)
    (clrhash (lispm-agent-pending agent))
    (lispm--in-session agent
      (lispm--newline)
      (when note
        (insert (lispm-transcript-section "Note" note)))
      (insert "* User\n"))
    (lispm-set-state agent 'idle)
    (lispm--save-session agent)
    (let ((answer (string-trim (concat reply (if note (concat "\n\n" note) "")))))
      (dolist (callback callbacks)
        (with-demoted-errors "lispm turn callback: %S"
          (funcall callback (if (string-empty-p answer) "(no reply)" answer)))))
    (run-hook-with-args 'lispm-turn-end-functions agent reply)
    (when (lispm-agent-inbox agent)
      (run-at-time 0 nil #'lispm--drain-inbox agent))))

(defun lispm--on-error (agent err body betas attempt)
  "Handle the failure ERR of AGENT's request BODY (BETAS), try ATTEMPT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm--live-delete agent)
    (if (and (lispm-api-retryable-p err) (< attempt lispm-api-max-retries))
        (let ((delay (* 2 (expt 2 attempt))))
          (lispm-set-state agent 'requesting
                           (format "%s; retrying in %ds" (plist-get err :type) delay))
          (run-at-time delay nil
                       (lambda ()
                         (when (eq (lispm-agent-state agent) 'requesting)
                           (lispm--send agent body betas (1+ attempt))))))
      (lispm--finish-turn
       agent (format "The request failed%s: %s: %s"
                     (if (plist-get err :status) (format " (HTTP %s)" (plist-get err :status)) "")
                     (plist-get err :type) (plist-get err :message))))))

;;;; Commands

(defun lispm--buffer-agent ()
  "The agent of the current session buffer."
  (or lispm-session-agent
      (user-error "Not in an agent session (or its agent is not defined)")))

(defun lispm-send ()
  "Send what you wrote under the last \"* User\" heading."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (when (lispm-agent-busy-p agent)
      (user-error "%s is busy (%s); C-c C-k aborts"
                  (lispm-agent-name agent) (lispm-agent-state agent)))
    (setf (lispm-agent-buffer agent) (current-buffer))
    (let ((parsed (lispm-transcript-parse)))
      (when (plist-get parsed :pending)
        (user-error "Some tool calls have no result yet"))
      (unless (equal (plist-get parsed :last-role) "user")
        (user-error "Write your message under a \"* User\" heading first")))
    (lispm--begin-turn agent)))

(defun lispm-abort (&optional agent)
  "Stop AGENT's turn: cancel the request, the running and waiting calls,
and its queued inputs."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (unless (lispm-agent-busy-p agent)
      (user-error "%s is idle" (lispm-agent-name agent)))
    (lispm-set-state agent 'aborting)
    (dolist (p (process-list))
      (when (eq (process-get p 'lispm-agent) agent)
        (process-put p 'lispm-aborted t)
        (delete-process p)))
    (lispm--live-delete agent)
    (let (ids)
      (maphash (lambda (id _) (push id ids)) (lispm-agent-pending agent))
      (dolist (id ids)
        (lispm--tool-done agent id "Interrupted: the user aborted the turn." t "FAILED")))
    (let ((inbox (lispm-agent-inbox agent)))
      (setf (lispm-agent-inbox agent) nil)
      (dolist (entry inbox)
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback (format "(%s was stopped before handling this)"
                                    (lispm-agent-name agent))))))
    (lispm--finish-turn agent "Aborted by the user.")))

;;;; Approval

(defun lispm-resolve (agent id decision &optional reason)
  "Settle AGENT's call ID awaiting approval: DECISION is `approve' or `deny'.
REASON, for a denial, is passed on to the model."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (when (and call (eq (plist-get call :status) 'ask))
      (pcase decision
        ('approve (lispm--run-tool agent id))
        ('deny (lispm--tool-done
                agent id
                (concat "The user denied this call."
                        (if (and reason (not (string-blank-p reason)))
                            (concat " Their reason: " reason)
                          " Do not repeat it unchanged; adapt, or ask the user."))
                t "DENIED"))))))

(defun lispm--ask-approval (agent id)
  "Show AGENT's call ID for approval: a preview, a notification."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (preview (let ((lispm-current-agent agent)
                        (default-directory (lispm-agent-root agent)))
                    (lispm-tool-preview-text (plist-get call :tool) (plist-get call :input)))))
    (when (and preview (plist-get call :marker))
      (lispm--in-session agent
        (goto-char (lispm-transcript-subtree-end (plist-get call :marker) 2))
        (insert (lispm-transcript-preview preview))))
    (run-hook-with-args 'lispm-approval-functions agent (plist-get call :name)
                        (plist-get call :input))
    (message "λ %s wants to run %s: review it with C-c C-y in %s"
             (lispm-agent-name agent) (plist-get call :name)
             (buffer-name (lispm-agent-buffer agent)))))

(defun lispm--todo-state-changed ()
  "Approve or deny a call whose heading the user moved to a new TODO state.
This makes `org-todo', and the org agenda, an approval interface."
  (when-let* ((agent lispm-session-agent)
              (id (org-entry-get nil "TOOL_ID"))
              (call (gethash id (lispm-agent-pending agent))))
    (when (eq (plist-get call :status) 'ask)
      (pcase org-state
        ((or "APPROVED" "RUNNING" "DONE")
         (run-at-time 0 nil #'lispm-resolve agent id 'approve))
        ((or "DENIED" "FAILED")
         (run-at-time 0 nil #'lispm-resolve agent id 'deny))))))

(defun lispm--waiting-calls (agent)
  "Ids of AGENT's calls awaiting approval, in transcript order."
  (let (ids)
    (maphash (lambda (id call)
               (when (eq (plist-get call :status) 'ask)
                 (push (cons id (and (plist-get call :marker)
                                     (marker-position (plist-get call :marker))))
                       ids)))
             (lispm-agent-pending agent))
    (mapcar #'car (sort ids (lambda (a b) (< (or (cdr a) 0) (or (cdr b) 0)))))))

(defun lispm--call-at-point (agent)
  "Id of AGENT's call awaiting approval at point, if any."
  (when-let* ((id (ignore-errors (org-entry-get nil "TOOL_ID" t)))
              (call (gethash id (lispm-agent-pending agent))))
    (and (eq (plist-get call :status) 'ask) id)))

(defun lispm-review ()
  "Review the call at point, or the first awaiting approval, and decide."
  (interactive)
  (let* ((agent (lispm--buffer-agent))
         (id (or (lispm--call-at-point agent)
                 (car (lispm--waiting-calls agent))
                 (user-error "No call awaits approval")))
         (call (gethash id (lispm-agent-pending agent)))
         (name (plist-get call :name)))
    (when (plist-get call :marker)
      (goto-char (plist-get call :marker)))
    (ignore-errors (org-fold-show-subtree))
    (recenter 0)
    (pcase (car (read-multiple-choice
                 (format "%s wants to run %s" (lispm-agent-name agent) name)
                 '((?y "yes" "Run this call")
                   (?n "no" "Deny it; you may give a reason")
                   (?a "always" "Run it, and every later call of this tool, this Emacs session")
                   (?q "later" "Decide later"))))
      (?y (lispm-resolve agent id 'approve))
      (?n (lispm-resolve agent id 'deny (read-string "Reason (optional): ")))
      (?a (push name (lispm-agent-allowed agent))
          (dolist (other (lispm--waiting-calls agent))
            (when (equal (plist-get (gethash other (lispm-agent-pending agent)) :name) name)
              (lispm-resolve agent other 'approve)))))))

(defun lispm-approve ()
  "Approve the call at point, or the first one awaiting approval."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (lispm-resolve agent (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))
                             (user-error "No call awaits approval"))
                   'approve)))

(defun lispm-deny (reason)
  "Deny the call at point, or the first one awaiting approval, for REASON."
  (interactive "sReason (optional): ")
  (let ((agent (lispm--buffer-agent)))
    (lispm-resolve agent (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))
                             (user-error "No call awaits approval"))
                   'deny reason)))

;;;; Triggers

(defun lispm--runtime-buffer-p ()
  "Non-nil if the current buffer holds one of the runtime's own files."
  (and buffer-file-name
       (file-in-directory-p buffer-file-name lispm-directory)))

(cl-defun lispm-watch (agent hook &key when prompt (debounce 1.0) name)
  "Wake AGENT whenever HOOK runs and WHEN approves.
WHEN and PROMPT are called with the hook's arguments (in the buffer
current when it ran); PROMPT, a function or a string, gives the
input AGENT receives as an event.  Runs within DEBOUNCE seconds are
collapsed into one.  Hook runs the agent caused itself are ignored,
as are saves of the runtime's own files.  NAME (default: HOOK's name)
identifies the trigger for `lispm-unwatch'.

  (lispm-watch \\='coder \\='compilation-finish-functions
    :when (lambda (_buffer status) (string-match-p \"abnormally\" status))
    :prompt \"The compilation failed. Read *compilation* and fix the cause.\")"
  (let* ((agent (lispm-get-agent agent))
         (name (or name (symbol-name hook)))
         (timer nil)
         (function
          (lambda (&rest args)
            (unless (or lispm-inhibit-triggers
                        (eq lispm-current-agent agent)
                        (lispm--runtime-buffer-p))
              (with-demoted-errors "lispm-watch: %S"
                (when (or (null when) (apply when args))
                  (let ((text (if (functionp prompt) (apply prompt args) prompt)))
                    (when timer (cancel-timer timer))
                    (setq timer (run-at-time debounce nil
                                             (lambda ()
                                               (setq timer nil)
                                               (lispm-tell agent text :event name)))))))))))
    (lispm-unwatch agent name)
    (add-hook hook function)
    (push (list :name name :hook hook :function function) (lispm-agent-triggers agent))
    name))

(defun lispm-every (agent seconds prompt &optional name)
  "Wake AGENT every SECONDS with PROMPT (a string or a function).
NAME identifies the trigger for `lispm-unwatch'."
  (let* ((agent (lispm-get-agent agent))
         (name (or name (format "every %ds" seconds))))
    (lispm-unwatch agent name)
    (push (list :name name
                :timer (run-at-time seconds seconds
                                    (lambda ()
                                      (lispm-tell agent (if (functionp prompt) (funcall prompt) prompt)
                                                  :event name))))
          (lispm-agent-triggers agent))
    name))

(defun lispm-unwatch (agent &optional name)
  "Remove AGENT's trigger NAME, or all of them."
  (let ((agent (lispm-get-agent agent)))
    (setf (lispm-agent-triggers agent)
          (seq-remove (lambda (trigger)
                        (when (or (null name) (equal name (plist-get trigger :name)))
                          (when (plist-get trigger :hook)
                            (remove-hook (plist-get trigger :hook) (plist-get trigger :function)))
                          (when (plist-get trigger :timer)
                            (cancel-timer (plist-get trigger :timer)))
                          t))
                      (lispm-agent-triggers agent)))))

;;;; Journal: audit and rollback

(defun lispm--journal-entry (agent &optional turn)
  "Journal (TURN . SNAPSHOTS) of AGENT for TURN, or its latest one with edits."
  (or (if turn
          (assq turn (lispm-agent-journal agent))
        (seq-find #'cdr (lispm-agent-journal agent)))
      (user-error "%s made no edits%s" (lispm-agent-name agent)
                  (if turn (format " in turn %d" turn) ""))))

(defun lispm--snapshot-now (snapshot)
  "Current text of the buffer or file in SNAPSHOT."
  (let ((buffer (car snapshot))
        (file (plist-get (cdr snapshot) :file)))
    (cond ((buffer-live-p buffer) (lispm-tools--buffer-text buffer))
          (file (lispm--file-text file))
          (t ""))))

(defun lispm--read-turn ()
  "Read a turn number when called with a prefix argument."
  (and current-prefix-arg (read-number "Turn: ")))

(defun lispm-turn-diff (agent &optional turn)
  "Show every change AGENT made in TURN (default: its latest edits)."
  (interactive (list (lispm--buffer-agent) (lispm--read-turn)))
  (pcase-let ((`(,turn . ,snapshots) (lispm--journal-entry agent turn)))
    (with-current-buffer (get-buffer-create "*lispm-diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Changes by %s in turn %d\n\n" (lispm-agent-name agent) turn))
        (dolist (s snapshots)
          (insert (lispm--diff (plist-get (cdr s) :text) (lispm--snapshot-now s)
                               (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                  "\n")))
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer "*lispm-diff*")))

(defun lispm-revert-turn (agent &optional turn)
  "Undo every change AGENT made to buffers and files in TURN.
Default: its latest turn with edits."
  (interactive (list (lispm--buffer-agent) (lispm--read-turn)))
  (pcase-let ((`(,turn . ,snapshots) (lispm--journal-entry agent turn)))
    (unless (or (not (called-interactively-p 'any))
                (yes-or-no-p (format "Revert %d buffer(s) %s changed in turn %d? "
                                     (length snapshots) (lispm-agent-name agent) turn)))
      (user-error "Cancelled"))
    (dolist (s snapshots)
      (let* ((buffer (car s))
             (props (cdr s))
             (file (plist-get props :file))
             (lispm-current-agent nil))
        (cond
         ((and file (not (plist-get props :existed)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (when (and (file-exists-p file)
                     (or (not (called-interactively-p 'any))
                         (yes-or-no-p (format "Delete %s, which %s created? "
                                              file (lispm-agent-name agent)))))
            (delete-file file)))
         ((buffer-live-p buffer) (lispm-tools--set-text buffer (plist-get props :text)))
         (file (let ((coding-system-for-write 'utf-8))
                 (write-region (plist-get props :text) nil file nil 'silent))))))
    (setf (lispm-agent-journal agent) (assq-delete-all turn (lispm-agent-journal agent)))
    (lispm--record-event
     agent "revert"
     (format "The user reverted the changes you made in turn %d to: %s."
             turn (mapconcat (lambda (s) (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                             snapshots ", ")))
    (message "Reverted turn %d of %s" turn (lispm-agent-name agent))))

(provide 'lispm-runtime)
;;; lispm-runtime.el ends here
