;;; lispm-turns.el --- Turn loop, streaming live response, and tool dispatch  -*- lexical-binding: t; -*-

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

;; Inbox draining, API request lifecycle, streaming sections,
;; tool call queuing and execution, and turn completion.

;;; Code:

(require 'org)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-inbox)
(require 'lispm-journal)
(require 'lispm-session)

(defvar lispm-kernel-exec-owner)
(declare-function lispm-kernel-exec-release-owner "lispm-kernel-exec" (owner))

(defun lispm-turns--release-kernel-owner (agent)
  "Release kernel owner quota state when AGENT's session ends."
  (when (and (lispm-agent-p agent) (fboundp 'lispm-kernel-exec-release-owner))
    (lispm-kernel-exec-release-owner (lispm-agent-id agent))))

(add-hook 'lispm-session-end-functions #'lispm-turns--release-kernel-owner)

(defvar-local lispm--live nil
  "Live section bounds while streaming.")

(defvar-local lispm--live-kind nil
  "Kind of live section being streamed: `thinking' or `text'.")

;;;; Turns

(defun lispm-turns-roster-entry-relations (agent entry)
  "Report the request-id relation of AGENT's mailbox ENTRY changing.
The sender's request ids change with the recipient's mailbox.  When
its key is unknown, compare relations globally rather than inventing
an endpoint.  Ordinary messages without a request id need no scan."
  (when (and (or (plist-get entry :request-id) (plist-get entry :reply-to))
             lispm-change-functions)
    (lispm-emit-change agent '(request-ids)
                       (list :type 'mailbox-relation :from (plist-get entry :from)))))

(defun lispm-reply-value (reply)
  "The value a turn returned with REPLY, as a turn callback gets it; or nil.
An agent returns one with return_value: data, decoded from JSON."
  (and (stringp reply) (> (length reply) 0) (get-text-property 0 'lispm-value reply)))

(defun lispm--entry-received-p (agent id)
  "Non-nil if AGENT has already received the inbox entry ID.
Queued and running inputs are checked first, then receipt properties
in its live transcript and earlier continuation files.  Only callers
supplying a stable input id need this history lookup."
  (or (equal id (plist-get (lispm-agent-entry agent) :id))
      (seq-some (lambda (entry) (equal id (plist-get entry :id)))
                (append (lispm-agent-inbox agent) (lispm-agent-suspended agent)))
      (let ((buffer (lispm-agent-buffer agent)))
        (cl-labels ((received ()
                     (save-restriction
                       (widen)
                       (seq-some
                        (lambda (section)
                          (and (memq (plist-get section :kind) '(user message event))
                               (equal id (cdr (assoc "ENTRY"
                                                    (lispm-transcript-properties
                                                     (plist-get section :body)
                                                     (plist-get section :end)))))))
                        (lispm-transcript-sections)))))
          (or (and (buffer-live-p buffer)
                   (with-current-buffer buffer (received)))
              (seq-some
               (lambda (file)
                 (let ((visiting (find-buffer-visiting file)))
                   (unless (eq visiting buffer)
                     (if visiting
                         (with-current-buffer visiting (received))
                       (with-temp-buffer
                         (insert-file-contents file)
                         (received))))))
               (lispm-session-files (lispm-agent-id agent))))))))

(cl-defun lispm-tell (agent text &key from event callback reply-to request-id id answer-to)
  "Give AGENT the input TEXT; it starts working as soon as it is idle.
FROM names the agent sending it; EVENT describes the hook or timer
that produced it (pending events with the same description are
coalesced).  CALLBACK, if given, is called with the agent's final
reply when the turn handling TEXT ends, and a value the turn returned
with it, if any (see `lispm-reply-value').  REPLY-TO, a string, says who
waits for that reply in a way that outlives Emacs: the input is kept
in the project's inbox file until its turn ends, and if Emacs stops
before, `lispm-inbox-callback-function' makes a callback of it again.
REQUEST-ID identifies the request for data observers and is kept in
the inbox; it is separate from the routing information in REPLY-TO.
ID, when supplied, is a stable inbox entry id: repeated delivery is
ignored, including after the input was handled or the session was
continued.  Such inputs keep a receipt and do not coalesce by EVENT.
ANSWER-TO identifies the background request this input answers."
  (let* ((agent (lispm-get-agent agent))
         (same nil)
         entry)
    ;; Opening the session first takes back what was queued for it.
    (lispm-session-buffer agent)
    (when (and id (lispm--entry-received-p agent id))
      (cl-return-from lispm-tell agent))
    (setq same (and (not id) event (seq-find (lambda (e) (equal (plist-get e :event) event))
                                    (lispm-agent-inbox agent))))
    (if same
        (setq entry (plist-put same :text text))
      (setq entry (list :id (or id (lispm-inbox-new-id)) :receipt (and id t)
                        :text text :from from :event event
                        :callback callback :reply-to reply-to :request-id request-id :answer-to answer-to
                        :queued (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (setf (lispm-agent-inbox agent)
            (append (lispm-agent-inbox agent) (list entry))))
    (lispm-emit-change
     agent '(mailbox)
     (unless same
       (list :type 'message-queued :id (plist-get entry :id) :from from
             :to (list 'lispm (lispm-agent-id agent)) :request-id (or request-id reply-to))))
    (lispm-turns-roster-entry-relations agent entry)
    (lispm-inbox-save agent)
    (run-at-time 0 nil #'lispm-turns-drain-inbox agent)
    agent))

(defun lispm-await-reply (agent id &optional peer)
  "Keep request ID as a dependency of AGENT's current input.
PEER names an external agent whose request cannot survive Emacs.
This records no blocking state: other inputs remain free to run."
  (when-let* ((entry (lispm-agent-entry agent)))
    (setf (lispm-agent-entry agent)
          (plist-put entry :awaiting
                     (append (plist-get entry :awaiting) (list (vector id (or peer :null))))))
    (lispm-inbox-save agent)))

(defun lispm--take-reply (agent entry)
  "Continue the suspended request answered by ENTRY, if any.
Only its matching reply inherits the upstream route and remaining
dependencies; unrelated user inputs and requests keep their own turns."
  (let* ((id (plist-get entry :answer-to))
         (parent (and id (seq-find
                          (lambda (input)
                            (seq-some (lambda (request) (equal id (aref request 0)))
                                      (plist-get input :awaiting)))
                          (lispm-agent-suspended agent)))))
    (when parent
      (setq entry (copy-sequence entry))
      (dolist (key '(:reply-to :callback))
        (setq entry (plist-put entry key (plist-get parent key))))
      (when (plist-member parent :value)
        (setq entry (plist-put entry :value (plist-get parent :value))))
      (setq entry (plist-put entry :awaiting
                             (seq-remove (lambda (request) (equal id (aref request 0)))
                                         (plist-get parent :awaiting))))
      (setf (lispm-agent-suspended agent) (delq parent (lispm-agent-suspended agent))))
    entry))

(defun lispm-turns-suspend-request (agent entry)
  "Save ENTRY until its dependencies reply, without holding AGENT busy."
  (setf (lispm-agent-suspended agent)
        (append (lispm-agent-suspended agent) (list entry))))

(defun lispm-turns-drain-inbox (agent)
  "Start a turn on AGENT's next inbox item if it is idle.
Not once AGENT has ended: input left for it then is dropped."
  (when (and (memq agent (lispm-agents))
             (not (lispm-agent-busy-p agent))
             (not (lispm-agent-compacting agent))
             (lispm-agent-inbox agent))
    ;; Opening a standing agent's session can take up calls it left
    ;; awaiting approval; the inbox waits until they are settled.
    (lispm-session-buffer agent)
    (unless (lispm-agent-busy-p agent)
      (lispm-turns-begin agent (pop (lispm-agent-inbox agent))))))

(defun lispm-turns-begin (agent &optional entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed.
A session grown past `lispm-compact-threshold' first makes room by
clearing the output of older calls; if that is not enough, it is
handed over to a continuation, and the turn starts there."
  (unless (and lispm-runtime-before-turn-function
               (funcall lispm-runtime-before-turn-function agent entry))
    (lispm-turns-start agent entry)))

(defun lispm-turns-start (agent entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed.
The input is recorded as the turn's (see `lispm-agent-entry'), in the
project's inbox file too, until the turn ends, and the session is
saved: if Emacs stops in the middle of the turn, reopening the session
takes it up (see `lispm--resume-calls').  A handover at idle under way
is given up: the session goes on."
  (setq entry (lispm--take-reply agent entry))
  (when (and (gethash agent lispm-session-idle-handovers)
             (not (plist-get entry :idle))
             lispm-runtime-give-up-idle-handover-function)
    (funcall lispm-runtime-give-up-idle-handover-function agent))
  (cl-incf (lispm-agent-turn agent))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-last-reply agent) nil
        (lispm-agent-value agent) (plist-get entry :value)
        (lispm-agent-auto-restarts agent) nil
        (lispm-agent-max-tokens agent) nil
        ;; Another agent's message or an event: nobody may be watching.
        (lispm-agent-attended agent) (not (or (plist-get entry :from) (plist-get entry :event)))
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback)))
        (lispm-agent-entry agent) (if (plist-get entry :id)
                                      entry
                                    (plist-put (copy-sequence entry) :id (lispm-inbox-new-id))))
  (lispm-in-session agent
    (when (markerp (cdr lispm-session-turn-start)) (set-marker (cdr lispm-session-turn-start) nil))
    (setq lispm-session-turn-start (cons (lispm-agent-turn agent) nil)
          lispm-session-turn-context nil))
  (when entry
    (lispm-in-session agent
      (let ((input (lispm-session-input-heading-p)))
        (when input (delete-region input (point-max))))
      (lispm-session-newline)
      (setcdr lispm-session-turn-start (copy-marker (point)))
      (insert (lispm-transcript-section (lispm-transcript-input-title entry) (plist-get entry :text)))
      (when (plist-get entry :receipt)
        (save-excursion
          (goto-char (cdr lispm-session-turn-start))
          (forward-line 1)
          (insert (format ":PROPERTIES:\n:ENTRY: %s\n:END:\n" (plist-get entry :id)))))))
  (unless entry
    (lispm-in-session agent
      (when (re-search-backward (lispm-transcript-heading-regexp 'user) nil t)
        (setcdr lispm-session-turn-start (copy-marker (match-beginning 0))))))
  (lispm-inbox-save agent)
  (lispm-emit-change
   agent '(mailbox turn)
   (list :type 'message-taken :id (plist-get (lispm-agent-entry agent) :id)
         :from (plist-get entry :from) :to (list 'lispm (lispm-agent-id agent))
         :request-id (or (plist-get entry :request-id) (plist-get entry :reply-to))))
  (with-demoted-errors "LispM: taking a snapshot: %S"
    (lispm-journal-begin agent))
  (lispm-turns-request agent))

(defun lispm-turns-wake-waiting (agent &optional turn steps)
  "Resume AGENT's waiting model request for TURN and STEPS, if still current."
  (when (and (lispm-agent-p agent)
             (buffer-live-p (lispm-agent-buffer agent))
             (eq (lispm-agent-state agent) 'requesting)
             (eq (lispm-agent-activity agent) 'waiting)
             (or (null turn) (eql turn (lispm-agent-turn agent)))
             (or (null steps) (eql steps (lispm-agent-steps agent))))
    (lispm-turns-request agent)))

(defun lispm--journal-wait (agent)
  "What AGENT's request waits for, if a snapshot of its project is being taken.
Registers an event-driven readiness callback and returns `(event . DESC)'
so `lispm-turns-request' avoids polling."
  (when (lispm-journal-busy-p agent)
    (let ((turn (lispm-agent-turn agent))
          (steps (lispm-agent-steps agent)))
      (lispm-journal-on-ready
       agent (lambda () (lispm-turns-wake-waiting agent turn steps))))
    (cons 'event "taking a snapshot of the project")))

(add-hook 'lispm-request-wait-functions #'lispm--journal-wait)

(defun lispm-session-tools (agent)
  "Schemas of the tools AGENT's session offers the model.
Like the system prompt, they are frozen: taken when the session first
needs them and kept while it is open, so a tool defined or redefined
later, or a role redefined with other tools, does not change the
request prefix, which the prompt cache and the model's thinking are
bound to.  A new session, or a handover, takes the tools as they are
then.  Tools whose descriptions depend on the agent see it as
`lispm-current-agent'."
  (with-current-buffer (lispm-agent-buffer agent)
    (or lispm-session-agent-tools
        (setq lispm-session-agent-tools (let ((lispm-current-agent agent))
                             (mapcar #'lispm-tool-schema (lispm-agent-tools* agent)))))))

(defun lispm--extend-session-tools (agent)
  "Offer AGENT's session the tools it gained, after those it had.
Run from `lispm-tools-changed-functions' when an agent's tools grow in
the middle of a session, as when it finds deferred tools, loads a
skill or defines a tool itself.  The earlier tools keep their place;
the request prefix changes once."
  (when (lispm-agent-p agent)
    (let ((buffer (lispm-agent-buffer agent)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when lispm-session-agent-tools
            (let ((names (mapcar (lambda (schema) (alist-get 'name schema)) lispm-session-agent-tools)))
              (dolist (tool (lispm-agent-tools* agent))
                (unless (member (lispm-tool-name tool) names)
                  (setq lispm-session-agent-tools
                        (append lispm-session-agent-tools
                                (list (let ((lispm-current-agent agent))
                                        (lispm-tool-schema tool))))))))))))))

(add-hook 'lispm-tools-changed-functions #'lispm--extend-session-tools)

(defun lispm--offers-tool-p (agent name)
  "Non-nil if AGENT's session offers the model the tool NAME."
  (seq-some (lambda (schema) (equal (alist-get 'name schema) name))
            (lispm-session-tools agent)))

(defun lispm--last-user-text-index (messages)
  "Return the index of the last user message in MESSAGES holding a text block."
  (let ((idx (1- (length messages)))
        (found nil))
    (while (and (>= idx 0) (not found))
      (let* ((m (nth idx messages))
             (content (and (equal (alist-get 'role m) "user")
                           (alist-get 'content m))))
        (if (and (vectorp content)
                 (seq-some (lambda (b) (equal (alist-get 'type b) "text")) content))
            (setq found idx)
          (setq idx (1- idx)))))
    found))

(defun lispm--request-context (agent &optional messages)
  "Return the live scenario context string for AGENT's current turn, or nil.
Captured once per turn from `lispm-request-context-functions' so every
request step within the same turn keeps an identical message prefix."
  (when lispm-request-context-functions
    (let ((turn (lispm-agent-turn agent))
          (buffer (lispm-agent-buffer agent)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (consp lispm-session-turn-context)
                   (eql (car lispm-session-turn-context) turn))
              (cddr lispm-session-turn-context)
            (let* ((parts (seq-remove
                           (lambda (s) (or (null s) (string-blank-p s)))
                           (mapcar (lambda (fn)
                                     (with-demoted-errors "LispM: request context: %S"
                                       (funcall fn agent)))
                                   lispm-request-context-functions)))
                   (text (and parts
                              (concat "[Current LispM scenario]\n"
                                      (string-join (mapcar #'string-trim parts) "\n"))))
                   (idx (and messages (lispm--last-user-text-index messages))))
              (setq lispm-session-turn-context (cons turn (cons idx text)))
              text)))))))

(defun lispm--attach-request-context (messages context &optional target-index)
  "Return MESSAGES with CONTEXT appended to the turn's user text message.
When TARGET-INDEX is a valid user text message index in MESSAGES, attach
CONTEXT there so mid-turn continuations preserve earlier steps' prefix;
otherwise attach to the last user text message.  Does not mutate MESSAGES."
  (if (or (null context) (string-empty-p context) (null messages))
      messages
    (let ((idx (if (and (integerp target-index)
                        (>= target-index 0)
                        (< target-index (length messages))
                        (let* ((m (nth target-index messages))
                               (c (and (equal (alist-get 'role m) "user")
                                       (alist-get 'content m))))
                          (and (vectorp c)
                               (seq-some (lambda (b) (equal (alist-get 'type b) "text")) c))))
                   target-index
                 (lispm--last-user-text-index messages))))
      (if (null idx)
          messages
        (cl-loop for m in messages
                 for i from 0
                 collect (if (= i idx)
                             (let ((content (alist-get 'content m)))
                               `((role . "user")
                                 (content . ,(vconcat content
                                                      (list `((type . "text")
                                                              (text . ,context)))))))
                           m))))))

(defun lispm-request-body (agent)
  "Request envelope for AGENT's next model call, a `lispm-api-envelope'.
Signal a `user-error' if the session cannot be sent as it is."
  (let ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse))))
    (when (plist-get parsed :pending)
      (user-error "Tool calls still await a result: %s"
                  (string-join (plist-get parsed :pending) ", ")))
    (unless (or (equal (plist-get parsed :last-role) "user")
                ;; A paused response goes back as it is, for the model to resume.
                (and (lispm-agent-continuing agent)
                     (equal (plist-get parsed :last-role) "assistant")))
      (user-error "Nothing to send: write under a \"* User\" heading"))
    (let* ((raw-messages (plist-get parsed :messages))
           (context (lispm--request-context agent raw-messages))
           (target-idx (with-current-buffer (lispm-agent-buffer agent)
                         (and (consp lispm-session-turn-context)
                              (cadr lispm-session-turn-context))))
           (messages (lispm--attach-request-context raw-messages context target-idx)))
      (lispm-api-request (let ((backend (lispm-agent-backend* agent)))
                           (if-let* ((limit (lispm-agent-max-tokens agent)))
                               (plist-put (copy-sequence backend) :max-tokens limit)
                             backend))
                         (lispm-agent-model* agent)
                         (lispm-agent-effort* agent)
                         (plist-get parsed :system)
                         (lispm-session-tools agent)
                         messages))))

(defun lispm-turns-max-tokens (agent)
  "The output limit of AGENT's requests now."
  (or (lispm-agent-max-tokens agent)
      (plist-get (lispm-agent-backend* agent) :max-tokens)
      lispm-max-tokens))

(defvar lispm-request-wait-functions nil
  "Functions that can hold back an agent's request for a moment.
Each is called with the agent before a request is made, and returns
a short description of what the request waits for, or nil.  While
one does, the request is tried again shortly; MCP servers still
starting hold requests back so that their tools are there.")

(defvar lispm-turns-control-plane-function nil
  "Optional function `(AGENT REQUEST)' routing turns to a kernel control plane.
When non-nil and returning non-nil, AGENT's multi-step turn executes inside
`lispm-kernel' instead of `lispm--send'.")

(defun lispm-turns-request (agent)
  "Ask the model for AGENT's next step."
  (cond
   ((not (buffer-live-p (lispm-agent-buffer agent)))
    (lispm-set-state agent 'idle "session buffer was killed"))
   ((when-let* ((waiting (run-hook-with-args-until-success 'lispm-request-wait-functions agent)))
      (let* ((event-p (and (consp waiting) (eq (car waiting) 'event)))
             (reason (if event-p (cdr waiting) waiting))
             (turn (lispm-agent-turn agent))
             (steps (lispm-agent-steps agent)))
        (lispm-set-state agent 'requesting reason 'waiting)
        (unless event-p
          (run-at-time 0.2 nil #'lispm-turns-wake-waiting agent turn steps)))
      t))
   ((>= (lispm-agent-steps agent) lispm-max-steps)
    (funcall lispm-runtime-signal-function agent 'step-limit
             (format "Stopped after %d model requests (`lispm-max-steps')." lispm-max-steps)))
   (t
    ;; A turn can outgrow the threshold in the middle: before handing
    ;; the session over, which waits for the turn's end, the output of
    ;; older calls makes room.
    (when lispm-runtime-trim-request-function
      (funcall lispm-runtime-trim-request-function agent))
    (pcase (condition-case err
               (lispm-request-body agent)
             (error (error-message-string err)))
      ((and (pred stringp) problem)
       (funcall lispm-runtime-signal-function agent 'request-failed problem))
      (request
       (setf (lispm-agent-continuing agent) nil)
       (cl-incf (lispm-agent-steps agent))
       (setf (lispm-agent-request-bytes agent)
             (if lispm-runtime-session-bytes-function
                 (funcall lispm-runtime-session-bytes-function agent)
               0))
       ;; What the request carries is on disk, should Emacs stop now.
       (lispm-save-session agent)
       (unless (and lispm-turns-control-plane-function
                    (funcall lispm-turns-control-plane-function agent request))
         (lispm--send agent request 0)))))))

(defun lispm-turns-abort-request (agent &optional message interrupted)
  "Clean up AGENT's failed or interrupted request and leave `requesting'.
When INTERRUPTED is non-nil, finish the turn cleanly as idle;
otherwise signal `request-failed' with MESSAGE via
`lispm-runtime-signal-function', or finish the turn when unset."
  (setf (lispm-agent-process agent) nil)
  (lispm-turns-live-delete agent)
  (cond
   (interrupted
    (lispm-turns-finish agent (or message "Interrupted.")))
   (lispm-runtime-signal-function
    (funcall lispm-runtime-signal-function
             agent 'request-failed (or message "Request failed.")))
   (t
    (lispm-turns-finish agent (or message "Request failed.")))))

(defun lispm--send (agent request attempt)
  "Send the REQUEST envelope for AGENT; ATTEMPT counts retries."
  (lispm-set-state agent 'requesting
                   (if (> attempt 0) (format "retry %d" attempt) "thinking")
                   (if (> attempt 0) 'retry 'thinking))
  (lispm--live-begin agent)
  (condition-case err
      (let* ((lispm-kernel-exec-owner (lispm-agent-id agent))
             (proc (funcall lispm-api-function (lispm-agent-backend* agent) request
                            :on-delta (lambda (kind text)
                                        (if (eq kind 'tool-input)
                                            (with-demoted-errors "LispM: %S"
                                              (run-hook-with-args 'lispm-tool-input-functions
                                                                  agent text))
                                          (lispm--live-insert agent kind text)))
                            :on-done (lambda (message)
                                       (condition-case done-err
                                           (lispm--on-message agent message)
                                         (error
                                          (when (and (memq (lispm-agent-state agent)
                                                           '(requesting acting))
                                                     (null (lispm-agent-condition agent)))
                                            (lispm-turns-abort-request
                                             agent (error-message-string done-err))))))
                            :on-error (lambda (err) (lispm--on-error agent err request attempt)))))
        (when (processp proc)
          (process-put proc 'lispm-agent agent))
        (setf (lispm-agent-process agent) proc))
    (quit
     (lispm-turns-abort-request agent "Interrupted." t)
     nil)
    (error
     (lispm-turns-abort-request agent (error-message-string err))
     nil)))

(defmacro lispm--unsaved (&rest body)
  "Run BODY, which changes the current buffer, leaving it as modified as it was.
A response streaming into a session is shown, never saved: until it
is complete, the file holds what the request was made from, which is
what a session opened again after Emacs stopped takes up."
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
           (create-lockfiles nil))
       (unwind-protect
           (progn ,@body)
         (restore-buffer-modified-p ,modified)))))

(defun lispm--live-begin (agent)
  "Open the region where AGENT's response streams in."
  (lispm-in-session agent
    (lispm--unsaved
      (lispm-session-newline)
      (let ((start (point-marker)))
        (insert (lispm-transcript-heading 'assistant))
        (setq lispm--live (cons start (copy-marker (point) t))
              lispm--live-kind nil)))))

(defun lispm--live-insert (agent kind text)
  "Show streamed content TEXT of KIND (text, thinking or tool) for AGENT."
  (lispm-in-session agent
    (when lispm--live
      (lispm--unsaved
        (goto-char (cdr lispm--live))
        (unless (eq kind lispm--live-kind)
          (lispm-session-newline)
          (setq lispm--live-kind kind))
        ;; Each piece says what it is, see `lispm-live-segments'.
        (pcase kind
          ('text (insert (propertize text 'lispm-live 'text)))
          ('thinking (insert (propertize text 'face 'shadow 'lispm-live 'thinking)))
          ('tool (insert (propertize (format "-> %s ...\n" text) 'face 'shadow
                                     'lispm-live (cons 'tool text))))))))
  (when (eq kind 'tool)
    (lispm-set-state agent 'requesting (format "writing a %s call" text)
                     (cons 'writing text)))
  (with-demoted-errors "LispM: %S"
    (run-hook-with-args 'lispm-response-delta-functions agent kind text)))

(defun lispm-live-region (buffer)
  "Where the response streaming into the session BUFFER is, or nil:
\(START . END), START where its heading starts."
  (let ((live (buffer-local-value 'lispm--live buffer)))
    (and live (cons (marker-position (car live))
                    (max (marker-position (car live)) (marker-position (cdr live)))))))

(defun lispm-live-segments (buffer)
  "What has streamed into the session BUFFER's response so far, in order.
Each is a plist: (:kind text :text TEXT), (:kind thinking :text TEXT) or
\(:kind tool :name NAME), a call being written."
  (when-let* ((region (lispm-live-region buffer)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let ((pos (progn (goto-char (car region)) (forward-line 1) (min (point) (cdr region))))
                (end (cdr region))
                segments)
            (while (< pos end)
              (let ((next (next-single-property-change pos 'lispm-live nil end))
                    (kind (get-text-property pos 'lispm-live)))
                (pcase kind
                  (`(tool . ,name) (push (list :kind 'tool :name name) segments))
                  ((pred symbolp)
                   (let ((text (string-trim (buffer-substring-no-properties pos next))))
                     (when (and kind (not (string-empty-p text)))
                       (push (list :kind kind :text text) segments)))))
                (setq pos next)))
            (nreverse segments)))))))

(defun lispm-turns-live-begin (agent)
  "Open the streamed live region in AGENT's session buffer."
  (lispm--live-begin agent))

(defun lispm-turns-live-insert (agent kind text)
  "Insert streamed TEXT of KIND (`text', `thinking', or `tool') for AGENT."
  (lispm--live-insert agent kind text))

(defun lispm-turns-live-delete (agent)
  "Remove AGENT's streamed region; the final message replaces it."
  (lispm-in-session agent
    (when lispm--live
      (lispm--unsaved
        (delete-region (car lispm--live) (max (car lispm--live) (cdr lispm--live))))
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

(defun lispm-turns-record-usage (agent usage)
  "Add response USAGE to AGENT's totals and update context size metrics."
  (lispm--add-usage agent usage)
  (lispm--note-context agent usage))

(defun lispm--note-context (agent usage)
  "Record the size of AGENT's session that the USAGE of a response reports.
Every token of the request counts, cached or not; the size is paired
with the session's size in bytes when the request was made."
  (let ((tokens (cl-loop for key in '(input_tokens cache_creation_input_tokens
                                                   cache_read_input_tokens)
                         for n = (alist-get key usage)
                         when (numberp n) sum n)))
    (when (and (> tokens 0) (lispm-agent-request-bytes agent))
      (setf (lispm-agent-context agent) (cons tokens (lispm-agent-request-bytes agent))))))

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
    ("pause_turn" "The model paused its response.")
    (_ (format "The response stopped: %s." stop))))

(defun lispm--decide (agent block invalid stopped)
  "Decide what to do with the tool_use BLOCK of AGENT.
INVALID lists calls with malformed input; STOPPED is the stop reason
if the response ended abnormally.  Return (STATE ACTION MESSAGE), or
\(STATE `ask' REASONS) for a call to await approval, REASONS those
`lispm-tool-decision' gives."
  (let* ((name (alist-get 'name block))
         (tool (lispm-get-tool name)))
    (cond
     (stopped
      (list "FAILED" 'fail (format "Not executed: the response stopped (%s) before the call was complete." stopped)))
     ((member (alist-get 'id block) invalid)
      (list "FAILED" 'fail "INVALID_JSON: the input of this call was not valid JSON. Make the call again."))
     ((not (and tool (lispm--offers-tool-p agent name)))
      (list "FAILED" 'fail (format "There is no tool named %s available to you." name)))
     (t
      (pcase (lispm-tool-decision agent tool (alist-get 'input block))
        (`(auto . ,_) (list "RUNNING" 'run))
        (`(ask . ,reasons) (list "WAITING" 'ask reasons))
        (`(,_ . ,reasons) (list "DENIED" 'deny (lispm-tools-denial agent tool reasons))))))))

(defconst lispm--stop-conditions
  '(("max_tokens" . output-limit)
    ("model_context_window_exceeded" . context-full)
    ("refusal" . refused)
    ("pause_turn" . paused))
  "Conditions a turn stops at when a response stops for these reasons.")

(defun lispm-turns-stop-outcome (stop message)
  "Return `(NOTE . CONDITION-TYPE)' for STOP reason on MESSAGE."
  (let ((note (lispm--stop-note stop message)))
    (cons note (and note (cdr (assoc stop lispm--stop-conditions))))))

(defun lispm--on-message (agent message)
  "Handle the complete response MESSAGE for AGENT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm--add-usage agent (alist-get 'usage message))
    (lispm--note-context agent (alist-get 'usage message))
    (with-demoted-errors "LispM: %S"
      (run-hook-with-args 'lispm-response-functions agent message))
    (lispm-turns-live-delete agent)
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
      (setf (lispm-agent-last-reply agent) (lispm-content-text content))
      (when content
        (setq calls (lispm-in-session agent
                      (lispm-session-newline)
                      (insert (lispm-transcript-heading 'assistant))
                      (lispm-transcript-insert-blocks
                       content
                       (lambda (b) (car (alist-get (alist-get 'id b) decisions nil nil #'equal)))))))
      (when uses
        (lispm--dispatch agent uses decisions calls proceed))
      (unless (and uses proceed)
        (let ((note (lispm--stop-note stop message)))
          (pcase (and note (cdr (assoc stop lispm--stop-conditions)))
            ('nil (lispm-turns-finish agent note))
            (type (funcall lispm-runtime-signal-function agent type note))))))))

(defmacro lispm-turns-dispatching (agent &rest body)
  "Run BODY with AGENT marked as dispatching tool calls.
While dispatching, `lispm-turns-tool-done' records results without
requesting the next turn step until BODY finishes.  Reentrant across
nested dispatch waves."
  (declare (indent 1) (debug t))
  (let ((ag (make-symbol "agent"))
        (was (make-symbol "was")))
    `(let* ((,ag ,agent)
            (,was (lispm-agent-dispatching ,ag)))
       (setf (lispm-agent-dispatching ,ag) t)
       (unwind-protect
           (progn ,@body)
         (setf (lispm-agent-dispatching ,ag) ,was)))))

(defun lispm--ensure-call-concurrency (agent call)
  "Compute and cache CALL's conflict spec under AGENT when absent."
  (or (plist-get call :concurrency)
      (let* ((lispm-current-agent agent)
             (default-directory (lispm-agent-root agent))
             (tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name))))
             (spec (lispm-tool-concurrency-spec tool (plist-get call :input))))
        (plist-put call :concurrency spec)
        spec)))

(defun lispm--active-calls (agent)
  "Return active started tool call plists for AGENT."
  (let (active)
    (maphash (lambda (_id call)
               (when (and (eq (plist-get call :status) 'run)
                          (plist-get call :started))
                 (lispm--ensure-call-concurrency agent call)
                 (push call active)))
             (lispm-agent-pending agent))
    active))

(defun lispm--call-conflicts-p (call blockers)
  "Return non-nil when CALL's concurrency spec conflicts with any of BLOCKERS."
  (let ((spec (plist-get call :concurrency)))
    (seq-some (lambda (other)
                (lispm-tool-concurrency-conflict-p spec (plist-get other :concurrency)))
              blockers)))

(defun lispm--call-conflicts-active-p (agent call)
  "Return non-nil when CALL conflicts with any currently running call of AGENT."
  (lispm--ensure-call-concurrency agent call)
  (lispm--call-conflicts-p call (lispm--active-calls agent)))

(defun lispm--dispatch (agent uses decisions calls proceed)
  "Act on the tool calls USES of AGENT per DECISIONS.
CALLS locates their headings.  If PROCEED, request the next step
once every call has a result."
  (let ((pending (lispm-agent-pending agent))
        (seq 0))
    (lispm-turns-dispatching agent
      ;; Register every call before running any, so that the first
      ;; result cannot resume the loop while others are unregistered.
      (dolist (b uses)
        (let* ((id (alist-get 'id b))
               (name (alist-get 'name b))
               (tool (lispm-get-tool name))
               (input (alist-get 'input b))
               (spec (let ((lispm-current-agent agent)
                           (default-directory (lispm-agent-root agent)))
                       (lispm-tool-concurrency-spec tool input))))
          (pcase-let ((`(,_ ,status ,reasons) (alist-get id decisions nil nil #'equal)))
            (puthash id (list :status status
                              :seq seq
                              :turn (lispm-agent-turn agent)
                              :name name
                              :tool tool
                              :input input
                              :concurrency spec
                              :marker (alist-get id calls nil nil #'equal)
                              :reasons (and (eq status 'ask) reasons))
                     pending)
            (cl-incf seq))))
      (lispm-set-state agent 'acting)
      (dolist (b uses)
        (let* ((id (alist-get 'id b))
               (decision (alist-get id decisions nil nil #'equal)))
          (pcase (nth 1 decision)
            ('run (plist-put (gethash id pending) :queued t))
            ('ask (when lispm-runtime-ask-approval-function
                    (funcall lispm-runtime-ask-approval-function agent id)))
            (_ (lispm-turns-tool-done agent id (nth 2 decision) t (car decision))))))
      (lispm--run-next-queued-tool agent))
    (lispm-turns-update-state agent)
    ;; The calls, awaiting approval or running, are on disk.
    (lispm-save-session agent)
    (when proceed
      (lispm--maybe-continue agent))))

(defun lispm--running-tool-p (agent)
  "Non-nil if AGENT currently has an active started tool call in `run' state."
  (and (lispm--active-calls agent) t))

(defun lispm--launch-unblocked-tools (agent)
  "Start all queued `run' calls for AGENT not blocked by earlier or active calls.
Return non-nil if any tool call started."
  (let (pending-entries has-queued started)
    (maphash (lambda (id call)
               (let ((status (plist-get call :status)))
                 (cond
                  ((and (eq status 'run) (plist-get call :queued))
                   (setq has-queued t)
                   (push (cons id call) pending-entries))
                  ((memq status '(ask stopped))
                   (push (cons id call) pending-entries)))))
             (lispm-agent-pending agent))
    (when has-queued
      (setq pending-entries
            (sort pending-entries
                  (lambda (a b)
                    (< (or (plist-get (cdr a) :seq) 0)
                       (or (plist-get (cdr b) :seq) 0)))))
      (let ((blockers (lispm--active-calls agent)))
        (lispm-turns-dispatching agent
          (dolist (entry pending-entries)
            (let ((id (car entry))
                  (call (cdr entry)))
              (lispm--ensure-call-concurrency agent call)
              (cond
               ((memq (plist-get call :status) '(ask stopped))
                ;; Earlier unapproved/stopped calls act as conflict barriers.
                (push call blockers))
               ((lispm--call-conflicts-p call blockers)
                ;; Keep FIFO ordering among conflicting resource/editor calls.
                (push call blockers))
               (t
                (plist-put call :queued nil)
                (push call blockers)
                (lispm-turns-run-tool agent id)
                (setq started t))))))))
    started))

(defun lispm--run-next-queued-tool (agent)
  "Start queued `run' tool calls for AGENT whose concurrency specs do not conflict."
  (while (lispm--launch-unblocked-tools agent)))

(defvar lispm-current-call nil
  "Id of the call whose tool is starting, among the acting agent's pending calls.
A tool that makes calls in turn, such as run_lisp, keeps it to ask
for their approval (see `lispm-call-ask').")

(defun lispm-turns-run-tool (agent id)
  "Run AGENT's pending tool call ID when no conflicting call is active.
A call taken up from a reopened session gets its tool now, by name:
tools of MCP servers appear only once the servers have started."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name)))))
    (cond
     ((not tool)
      (lispm-turns-tool-done agent id (format "There is no tool named %s available to you."
                                              (plist-get call :name))
                             t "FAILED"))
     ((and (not (plist-get call :started))
           (lispm--call-conflicts-active-p agent call))
      (plist-put call :status 'run)
      (plist-put call :queued t)
      (lispm-turns-update-state agent))
     (t
      (plist-put call :status 'run)
      (plist-put call :started t)
      (lispm-in-session agent
        (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
      (lispm-turns-update-state agent)
      (let ((lispm-current-agent agent)
            (lispm-current-call id)
            (lispm-kernel-exec-owner (lispm-agent-id agent))
            (default-directory (lispm-agent-root agent)))
        (lispm-tool-invoke tool (plist-get call :input)
                           (lambda (text errorp &optional images)
                             (lispm-call-finished agent id text errorp images))))))))

(defcustom lispm-debug-tool-errors nil
  "Whether a tool's failure stops its call, for the user to choose how to go on.
nil, the default, gives the model the error as the call's result, as
it is usually the model's to handle; t stops the failures of every
tool; a list of tool names, those of these tools.  A stopped call
waits at a `tool-error' condition, as `debug-on-error' enters the
debugger: report the error to the model, run the call again, give the
model a result of your own, or stop the turn.  A call interrupted
with C-g is never stopped."
  :type '(choice (const :tag "Never" nil) (const :tag "Every tool" t)
                 (repeat :tag "These tools" string))
  :group 'lispm)

(defun lispm--debug-tool-error-p (name)
  "Non-nil if a failure of the tool NAME stops its call.
See `lispm-debug-tool-errors'."
  (or (eq lispm-debug-tool-errors t)
      (and (listp lispm-debug-tool-errors)
           (member name (mapcar (lambda (tool) (format "%s" tool)) lispm-debug-tool-errors)))))

(defun lispm-call-finished (agent id text errorp images)
  "Take the result TEXT of AGENT's call ID; ERRORP marks failure.
IMAGES are files the result holds besides TEXT.  A failure that
`lispm-debug-tool-errors' covers stops the call instead."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (if (and errorp call
             (lispm--debug-tool-error-p (plist-get call :name))
             (not (equal text lispm-tools-interrupted)))
        (if lispm-runtime-stop-call-function
            (funcall lispm-runtime-stop-call-function agent id text images)
          (lispm-turns-tool-done agent id text errorp nil images))
      (lispm-turns-tool-done agent id text errorp nil images))))

(defun lispm-turns-tool-done (agent id text errorp &optional state images)
  "Record TEXT as the result of AGENT's call ID; ERRORP marks failure.
STATE overrides the call's final TODO keyword.  IMAGES are files the
result holds besides TEXT."
  (when-let* ((call (gethash id (lispm-agent-pending agent))))
    (remhash id (lispm-agent-pending agent))
    (lispm-emit-change agent '(calls))
    (let* ((marker (plist-get call :marker))
           (note (and (functionp lispm-tool-result-note-function)
                      (ignore-errors
                        (funcall lispm-tool-result-note-function
                                 agent call text errorp))))
           (full-text (if (and (stringp note) (not (string-empty-p note)))
                          (concat text
                                  (unless (string-suffix-p "\n" text) "\n")
                                  note)
                        text)))
      (lispm-in-session agent
        (when (and marker (marker-buffer marker))
          (lispm-transcript-set-state marker (or state (if errorp "FAILED" "DONE")))
          (goto-char (lispm-transcript-subtree-end marker 2))
          (let ((start (point)))
            (insert (lispm-transcript-result full-text errorp images
                                             (and buffer-file-name
                                                  (file-name-directory buffer-file-name))))
            (when (> (cl-count ?\n full-text) 12)
              (lispm-transcript-fold start))))))
    (unless (lispm-agent-dispatching agent)
      (lispm--run-next-queued-tool agent)
      (lispm-turns-update-state agent)
      (lispm--maybe-continue agent))))

(defun lispm-turns-update-state (agent)
  "Reflect AGENT's pending calls in its state."
  (when (memq (lispm-agent-state agent) '(acting waiting))
    (let ((waiting 0) (stopped 0) running)
      (maphash (lambda (_id call)
                 (pcase (plist-get call :status)
                   ('stopped (cl-incf stopped))
                   ('ask (cl-incf waiting))
                   (_ (push (plist-get call :name) running))))
               (lispm-agent-pending agent))
      (setq running (nreverse running))
      (cond ((> (+ waiting stopped) 0)
             (lispm-set-state
              agent 'waiting
              (string-join
               (delq nil (list (and (> stopped 0)
                                    (format "%d call%s stopped at an error" stopped
                                            (if (= stopped 1) "" "s")))
                               (and (> waiting 0)
                                    (format "%d call%s await%s approval" waiting
                                            (if (= waiting 1) "" "s")
                                            (if (= waiting 1) "s" "")))))
               ", ")))
            ((cdr running)
             (lispm-set-state agent 'acting
                              (format "running %d tools (%s, …)" (length running) (car running))
                              (cons 'running (car running))))
            (running
             (lispm-set-state agent 'acting (format "running %s" (car running))
                              (cons 'running (car running))))))))

(defun lispm-agent-status-record (agent)
  "Return what AGENT is doing as a record (:state STATE :kind KIND :msg
MSG), as `lispm-agents-status-functions' describe an agent's buffer:
`blocked' while it waits for its user, on a call awaiting approval
\(KIND `permission') or at a condition awaiting a restart (`question');
`error' while calls of its stopped at errors and nothing else holds it;
`working' in the rest of a turn; else `idle'.  MSG is AGENT's status."
  (let ((status (lispm-agent-status agent)))
    (pcase (lispm-agent-state agent)
      ('idle (list :state 'idle :msg status))
      ('waiting
       (let ((asked 0) (stopped 0))
         (maphash (lambda (_id call)
                    (pcase (plist-get call :status)
                      ('ask (cl-incf asked))
                      ('stopped (cl-incf stopped))))
                  (lispm-agent-pending agent))
         (cond ((lispm-agent-condition agent) (list :state 'blocked :kind 'question :msg status))
               ((> asked 0) (list :state 'blocked :kind 'permission :msg status))
               ((> stopped 0) (list :state 'error :msg status))
               (t (list :state 'blocked :msg status)))))
      (_ (list :state 'working :msg status)))))

(defun lispm-agent-buffer-status-record (buffer)
  "Return the status record of BUFFER's LispM agent, or nil.
This supplies `lispm-agents-buffer-status' independently of roster
observers, as the runtime's agent status does for other interfaces."
  (when-let* ((agent (buffer-local-value 'lispm-session-agent buffer)))
    (lispm-agent-status-record agent)))

(with-eval-after-load 'lispm-agents
  (add-hook 'lispm-agents-status-functions #'lispm-agent-buffer-status-record t))

(defun lispm--maybe-continue (agent)
  "Request AGENT's next step once all its tool calls have results."
  (when (and (memq (lispm-agent-state agent) '(acting waiting))
             (zerop (hash-table-count (lispm-agent-pending agent))))
    (lispm-turns-request agent)))

(defun lispm-turns-finish (agent &optional note)
  "End AGENT's turn, adding NOTE to the transcript if non-nil."
  (let ((callbacks (lispm-agent-turn-callbacks agent))
        (waiting-on (lispm-agent-waiting-on agent))
        (entry (lispm-agent-entry agent))
        (reply (or (lispm-agent-last-reply agent) "")))
    (when (and note (plist-get entry :awaiting))
      (setq entry (plist-put entry :awaiting nil))
      (setf (lispm-agent-entry agent) entry)
      (lispm-inbox-save agent))
    (when (and (not note) (plist-get entry :reply-to) (plist-get entry :awaiting))
      (lispm-turns-suspend-request agent entry)
      (setq callbacks nil))
    (setf (lispm-agent-turn-callbacks agent) nil
          (lispm-agent-process agent) nil
          (lispm-agent-waiting-on agent) nil
          (lispm-agent-condition agent) nil)
    (when waiting-on
      (lispm-emit-change agent '(waits-on request-ids))
      (dolist (target waiting-on)
        (lispm-emit-change target '(waited-on-by request-ids))))
    (clrhash (lispm-agent-pending agent))
    (lispm-in-session agent
      (lispm-session-newline)
      (when note
        (insert (lispm-transcript-section (lispm-transcript-title 'note) note)))
      (insert (lispm-transcript-heading 'user)))
    (lispm-set-state agent 'idle)
    (lispm-save-session agent)
    (with-demoted-errors "LispM: taking a snapshot: %S"
      (lispm-journal-end agent))
    (let* ((answer (string-trim (concat reply (if note (concat "\n\n" note) ""))))
           (answer (if (string-empty-p answer) "(no reply)" answer)))
      ;; A value the turn returned goes with the reply, as its property.
      (when (lispm-agent-value agent)
        (setq answer (propertize answer 'lispm-value (lispm-agent-value agent))))
      (dolist (callback callbacks)
        (with-demoted-errors "lispm turn callback: %S"
          (funcall callback answer))))
    ;; Only once whoever waited has the reply: if Emacs stopped before,
    ;; the entry would tell them again.
    (setf (lispm-agent-entry agent) nil)
    (lispm-turns-roster-entry-relations agent entry)
    (lispm-inbox-save agent)
    (run-hook-with-args 'lispm-turn-end-functions agent reply)
    (when (lispm-agent-inbox agent)
      (run-at-time 0 nil #'lispm-turns-drain-inbox agent))))

(defun lispm--on-error (agent err request attempt)
  "Handle the failure ERR of AGENT's REQUEST envelope, try ATTEMPT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm-turns-live-delete agent)
    (cond
     ((and (eql (plist-get err :status) 413)
           (buffer-live-p (lispm-agent-buffer agent))
           (> (buffer-local-value 'lispm-context-images (lispm-agent-buffer agent)) 0))
      (lispm-in-session agent
        (setq-local lispm-context-images 0))
      (cl-decf (lispm-agent-steps agent))
      (lispm-turns-request agent))
     ((and (lispm-api-retryable-p err) (< attempt lispm-api-max-retries))
      (let ((delay (* 2 (expt 2 attempt))))
        (lispm-set-state agent 'requesting
                         (format "%s; retrying in %ds" (plist-get err :type) delay)
                         'retry)
        (run-at-time delay nil
                     (lambda ()
                       (when (eq (lispm-agent-state agent) 'requesting)
                         (lispm--send agent request (1+ attempt)))))))
     (t
      (let ((raised (lispm-agent-max-tokens agent)))
        ;; The server may refuse a limit raised past what the model
        ;; allows: lower it again, or every retry fails alike.
        (when (and raised (not (lispm-api-retryable-p err)))
          (setf (lispm-agent-max-tokens agent) nil))
        (funcall lispm-runtime-signal-function
         agent 'request-failed
         (concat (format "The request failed%s: %s: %s"
                         (if (plist-get err :status) (format " (HTTP %s)" (plist-get err :status)) "")
                         (plist-get err :type) (plist-get err :message))
                 (if (and raised (null (lispm-agent-max-tokens agent)))
                     (format "\nThe output limit raised to %d for this turn is back to %d."
                             raised (lispm-turns-max-tokens agent))
                   ""))))))))


(setq lispm-runtime-live-delete-function #'lispm-turns-live-delete)

(provide 'lispm-turns)

;;; lispm-turns.el ends here
