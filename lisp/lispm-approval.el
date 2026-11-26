;;; lispm-approval.el --- Session commands, call approvals, and crash recovery  -*- lexical-binding: t; -*-

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

;; User commands (`lispm-send', `lispm-abort', `lispm-review'),
;; tool approval resolution, Org TODO integration, and interrupted
;; session call restoration.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-tools)
(require 'lispm-inbox)
(require 'lispm-session)
(require 'lispm-turns)
(require 'lispm-conditions)

;;;; Commands

(defvar lispm-abort-functions nil
  "Functions called with `(AGENT)' when `lispm--stop' aborts an agent.")

(defvar lispm-resolve-functions nil
  "Functions called with `(AGENT ID DECISION REASON)' on approval settlement.")

(defvar org-state)

(defun lispm-send ()
  "Send what you wrote under the last \"* User\" heading."
  (interactive)
  (let ((agent (lispm-buffer-agent)))
    (if (and (lispm-agent-condition agent) (not (lispm--open-calls agent)))
        (lispm--send-past-condition agent)
      (when (lispm-agent-busy-p agent)
        (user-error "%s" (lispm-tr "%s is busy (%s); C-c C-k aborts"
                                   (lispm-agent-name agent) (lispm-state-name (lispm-agent-state agent)))))
      (setf (lispm-agent-buffer agent) (current-buffer))
      (let ((parsed (lispm-transcript-parse)))
        (when (plist-get parsed :pending)
          (user-error "%s" (lispm-tr "Some tool calls have no result yet")))
        (unless (equal (plist-get parsed :last-role) "user")
          (user-error "%s" (lispm-tr "Write your message under a \"* User\" heading first"))))
      (lispm-turns-begin agent))))

(defun lispm--send-past-condition (agent)
  "Send what the user wrote after the condition AGENT's turn waits at.
That stops the turn there, as writing after a failed turn always did.
The text is under a \"* User\" heading the user added after the
condition's; without one, say how to go on."
  (let* ((condition (lispm-agent-condition agent))
         (input (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-max))
                    (and (re-search-backward (lispm-transcript-heading-regexp 'user) nil t)
                         (> (point) (plist-get condition :marker))
                         (string-trim (buffer-substring-no-properties
                                       (min (point-max) (1+ (line-end-position)))
                                       (point-max))))))))
    (unless (and input (not (string-empty-p input)))
      (user-error "%s" (lispm-tr "%s waits at a condition: choose a restart with C-c C-y, or write \
under a new \"* User\" heading after it" (lispm-agent-name agent))))
    (funcall lispm-runtime-take-input-function agent)
    (lispm-stop-at-condition agent)
    (lispm-tell agent input)))

(defun lispm-abort (&optional agent)
  "Stop AGENT's turn: cancel the request, the running and waiting calls,
and its queued inputs."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm-buffer-agent))))
    (unless (or (lispm-agent-busy-p agent) (lispm-agent-suspended agent))
      (user-error "%s" (lispm-tr "%s is idle" (lispm-agent-name agent))))
    (lispm--stop agent "Aborted by the user.")))

(defun lispm--stop (agent note &optional keep-inbox)
  "Stop AGENT's turn, its calls and queued inputs, recording NOTE.
A condition the turn waits at is closed, its message joining NOTE; one
between turns ends no turn, and NOTE is recorded as an event.  With
KEEP-INBOX, queued inputs stay, to be taken up next."
  (let ((between nil))
    (when-let* ((condition (lispm-agent-condition agent)))
      (setf (lispm-agent-condition agent) nil
            between (lispm-conditions-between-turns-p condition))
      (lispm-conditions-close agent condition 'abort)
      (setq note (concat (plist-get condition :message) "\n" note)))
    (setf (lispm-agent-compacting agent) nil)
    (when (and (gethash agent lispm-session-idle-handovers)
               lispm-runtime-give-up-idle-handover-function)
      (funcall lispm-runtime-give-up-idle-handover-function agent))
    (lispm-set-state agent 'aborting)
    (when-let* ((proc (lispm-agent-process agent)))
      (setf (lispm-agent-process agent) nil)
      (when (functionp proc)
        (lispm-exec-delete proc)))
    (run-hook-with-args 'lispm-abort-functions agent)
    (dolist (p (process-list))
      (when (eq (process-get p 'lispm-agent) agent)
        (process-put p 'lispm-aborted t)
        (lispm-exec-delete p)))
    (lispm-turns-live-delete agent)
    (let (ids)
      (maphash (lambda (id _) (push id ids)) (lispm-agent-pending agent))
      (dolist (id ids)
        (when-let* ((condition (lispm-conditions-call agent id)))
          (lispm-conditions-settle-call agent condition 'abort))
        (lispm-turns-tool-done agent id "Interrupted: the user aborted the turn." t "FAILED")))
    (let ((inbox (unless keep-inbox
                   (append (lispm-agent-inbox agent) (lispm-agent-suspended agent)))))
      (unless keep-inbox
        (setf (lispm-agent-inbox agent) nil (lispm-agent-suspended agent) nil))
      (when inbox
        (lispm-emit-change agent '(mailbox)))
      (dolist (entry inbox)
        (lispm-turns-roster-entry-relations agent entry)
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback (format "(%s was stopped before handling this)"
                                    (lispm-agent-name agent))))))
    (if (not between)
        (lispm-turns-finish agent note)
      (lispm-session-record-event agent "stopped" note)
      (lispm-set-state agent 'idle)
      (lispm-save-session agent)
      (lispm-inbox-save agent))))

;;;; Approval

(defun lispm--denial (reason)
  "What a call the user denied for REASON (nil if none given) results in."
  (concat "The user denied this call."
          (if (and reason (not (string-blank-p reason)))
              (concat " Their reason: " reason)
            " Do not repeat it unchanged; adapt, or ask the user.")))

(defun lispm--approval-event (agent id event &optional reason source)
  "Record and save an approval EVENT for AGENT's call ID.
EVENT is `requested', `resumed', `approve', `allow' or `deny'.
REASON is a denial's explanation; SOURCE names who settled the call,
`user' by default or `allowance' when an earlier allowance settles it.
The call's input is already in its heading or its Asks subsection."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              (marker (plist-get call :marker))
              ((eq (marker-buffer marker) (lispm-agent-buffer agent))))
    (let* ((asked (lispm-asked call))
           (scope (and (eq event 'allow) (lispm-call-allowance agent id)))
           (record `((time . ,(format-time-string "%FT%TZ" nil t))
                     (event . ,(symbol-name event))
                     (call_id . ,id)
                     (tool . ,(plist-get asked :name))
                     ,@(when (memq event '(requested resumed))
                         `((reasons . ,(vconcat (plist-get asked :reasons)))))
                     ,@(when (memq event '(approve allow deny))
                         `((source . ,(symbol-name (or source 'user)))))
                     ,@(when (eq event 'approve) '((scope . "once")))
                     ,@(when scope
                         (if (consp scope)
                             `((scope . "custom") (allowance . ,(cdr scope)))
                           `((scope . ,(symbol-name scope)))))
                     ,@(when reason `((reason . ,reason))))))
      (lispm-in-session agent
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-approval record)))
      (lispm-save-session agent))))

(defun lispm-resolve (agent id decision &optional reason source)
  "Settle AGENT's call ID awaiting approval: DECISION is `approve' or `deny'.
REASON, for a denial, is passed on to the model.  For a call that
awaits approval of a call it makes (see `lispm-call-ask'), that call
is approved or denied, and the call that made it goes on.  DECISION
may also be `allow', from `lispm-allow': approved for good, which the
call that made such a call learns.  SOURCE is `allowance' when another
call's remembered approval settles this one; otherwise it is `user'."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (when (and call (eq (plist-get call :status) 'ask))
      (lispm--approval-event agent id decision reason source)
      (if-let* ((asking (plist-get call :asking)))
          (progn
            (plist-put call :asking nil)
            (plist-put call :status 'run)
            (lispm-in-session agent
              (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
            (lispm-turns-update-state agent)
            (funcall (plist-get asking :continue) decision
                     (and (eq decision 'deny) (lispm--denial reason))))
        (pcase decision
          ((or 'approve 'allow) (lispm-turns-run-tool agent id))
          ('deny (lispm-turns-tool-done agent id (lispm--denial reason) t "DENIED")))))
    (run-hook-with-args 'lispm-resolve-functions agent id decision reason)))

(defun lispm-call-ask (agent id tool input continue &optional always reasons)
  "Have AGENT's running call ID await approval of a call of TOOL on INPUT.
The running call makes that call in turn, as a Lisp program does; it
waits meanwhile, and the call is shown, approved and denied as if the
model had made it, the transcript recording it under the running
call's heading.  Once the user decides, CONTINUE is called with
`approve', `allow' if they approved it for good, or `deny' and what
the denial results in.  Approving it for good allows AGENT calls like
it, as `lispm-allow' says, unless ALWAYS is non-nil: a sentence saying
what it means instead, shown with the choice, which CONTINUE then
carries out.  REASONS, a list of strings, are why it asks, shown with
it, as `lispm-tool-decision' gives them.  Return nil, and ask nothing,
if ID is not running."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              ((eq (plist-get call :status) 'run)))
    (plist-put call :asking (list :tool tool :name (lispm-tool-name tool) :input input
                                  :continue continue :always always :reasons reasons))
    (plist-put call :status 'ask)
    (when-let* ((marker (plist-get call :marker))
                ((marker-buffer marker)))
      (lispm-in-session agent
        (lispm-transcript-set-state marker "WAITING")
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-asks (lispm-tool-name tool) input))))
    (lispm-turns-update-state agent)
    (lispm--ask-approval agent id)
    t))

(defun lispm-allow (agent id)
  "Approve AGENT's call ID, and calls like it for the rest of the Emacs session.
Like it are later calls of its tool, or for a tool that reaches Emacs,
such as one of class `exec', calls with the same input (see
`lispm-tools-allowance').  AGENT's other calls awaiting approval that
this allows are approved too.  A call that a call makes may mean
something else by it (see `lispm-call-ask'): then the call that made it
is told, and nothing more is allowed."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              (asked (lispm-asked call))
              (tool (plist-get asked :tool)))
    (unless (plist-get asked :always)
      (push (lispm-tools-allowance tool (plist-get asked :input) agent) (lispm-agent-allowed agent)))
    (lispm-resolve agent id 'allow)
    ;; Not those a function of `lispm-tool-action-functions' still asks about.
    (dolist (other (lispm-waiting-calls agent))
      (let ((asked (lispm-asked (gethash other (lispm-agent-pending agent)))))
        (when (and (plist-get asked :tool)
                   (lispm-tools-invocation-allowed-p agent (plist-get asked :tool) (plist-get asked :input))
                   (eq (lispm-tool-action agent (plist-get asked :tool) (plist-get asked :input))
                       'auto))
          (lispm-resolve agent other 'approve nil 'allowance))))))

(defconst lispm-interrupted-result
  "The call was interrupted: Emacs stopped before it finished, so whether it took effect is unknown.  Check before repeating it."
  "Result of a call that was running when its session was last saved.")

(defvar lispm-resume-call-functions nil
  "Functions that can take up a call left running when its session was saved.
Each is called with the agent and the call, a plist (:id :name :input
:properties) as `lispm-transcript-parse' gives it, while the session
is opened again after Emacs stopped.  One that returns non-nil has
taken the call up: it stays pending, running, until its result comes
\(see `lispm-call-finished').  Otherwise the call is recorded as
interrupted.  Delegation takes up ask_agent calls this way: the agent
asked can still reply.")

(defun lispm--resume-calls (agent)
  "Take up the turn AGENT's session was in when it was last saved, if any.
A session saved in the middle of a turn and opened again, after Emacs
stopped, records what the turn had come to, and the project's inbox
file the input it handled (see `lispm-inbox-take'):

- calls awaiting approval await it again, and once they are settled
  the turn goes on; calls that were running are recorded as
  interrupted, their processes gone, unless a function of
  `lispm-resume-call-functions' takes them up;
- a condition awaiting a restart is waited at again;
- a turn with nothing left to wait for, its calls interrupted or its
  request lost with Emacs, waits at an `interrupted' condition: go on
  with it, or stop there.

Inputs queued for the agent are its again, and taken up once it is
free.  This runs while the session opens, before
`lispm-session-mode-hook' has started its MCP servers: tools are
looked up when a call runs."
  (lispm-conditions-restore-model agent)
  (when (and (eq (lispm-agent-state agent) 'idle)
             (zerop (hash-table-count (lispm-agent-pending agent))))
    (pcase-let* ((`(,queued . ,running) (lispm-inbox-take agent))
                 (parsed (lispm-transcript-parse))
                 (calls (plist-get parsed :pending-calls))
                 (condition (plist-get parsed :condition)))
      (dolist (entry queued)
        (unless (seq-find (lambda (e) (equal (plist-get e :id) (plist-get entry :id)))
                          (lispm-agent-inbox agent))
          (setf (lispm-agent-inbox agent) (append (lispm-agent-inbox agent) (list entry)))))
      (when queued
        (lispm-emit-change agent '(mailbox))
        (dolist (entry queued) (lispm-turns-roster-entry-relations agent entry)))
      (cond ((or calls condition (and running (not (lispm-session-input-heading-p))))
             (lispm--resume-turn agent running calls condition))
            (running
             ;; The turn ended, but Emacs stopped before its input was
             ;; done with: whoever waited for the reply may not have it.
             (lispm--resume-ended-turn agent running parsed)))
      ;; Peer requests cannot survive Emacs, but their callers must still
      ;; receive an answer and finish their suspended upstream requests.
      (run-at-time 0 nil #'lispm--resume-peer-replies agent running)
      (when (lispm-agent-inbox agent)
        (run-at-time 0 nil #'lispm-turns-drain-inbox agent)))))

(defun lispm--resume-peer-replies (agent running)
  "Answer the lost peer dependencies of AGENT's restored requests.
RUNNING is its input from the inbox at restoration time."
  (when (memq agent (lispm-agents))
    (dolist (entry (cons running (lispm-agent-suspended agent)))
      (dolist (request (plist-get entry :awaiting))
        (when-let* ((peer (aref request 1)) ((stringp peer)))
          (let ((id (aref request 0)))
            (lispm-tell agent
                        (format "[Reply to request %s]\n%s's request was interrupted by the Emacs restart."
                                id peer)
                        :from peer :request-id id :answer-to id :id (concat "reply-" id))))))))

(defun lispm--resume-turn (agent entry calls condition)
  "Take up AGENT's turn on ENTRY, which Emacs stopped in the middle of.
ENTRY is the input it handled, from the inbox file, or nil if that is
not known (a session saved before inputs were kept there).  CALLS are
the calls the transcript leaves without a result, CONDITION the
condition it waits at, if any; see `lispm--resume-calls'."
  (cl-incf (lispm-agent-turn agent))
  (lispm-in-session agent
    (when (markerp (cdr lispm-session-turn-start)) (set-marker (cdr lispm-session-turn-start) nil))
    (setq lispm-session-turn-start (cons (lispm-agent-turn agent) nil)
          lispm-session-turn-context nil))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-auto-restarts agent) nil
        (lispm-agent-entry agent) entry
        (lispm-agent-value agent) (lispm--recorded-turn-value entry)
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback)))
        ;; Whoever started it, the user is the one reopening it, unless
        ;; another agent or an event did: then someone waits for it.
        (lispm-agent-attended agent) (not (or (plist-get entry :from) (plist-get entry :event))))
  (with-demoted-errors "LispM: taking a snapshot: %S"
    (lispm-journal-begin agent t))
  (cond
   ((and condition (not calls))
    (lispm-conditions-wait-at agent (append (list :marker (copy-marker (plist-get condition :pos)))
                                  condition)))
   (calls
    (dolist (call calls)
      (let* ((marker (copy-marker (plist-get call :pos) t))
             (condition (and (plist-get call :stopped)
                             (equal (plist-get call :state) "WAITING")
                             (lispm-conditions-tool-error (plist-get call :id) (plist-get call :name)
                                                (plist-get call :stopped) marker))))
        (puthash (plist-get call :id)
                 (list :turn (lispm-agent-turn agent)
                       :status (cond (condition 'stopped)
                                     ((member (plist-get call :state) '("WAITING" "APPROVED"))
                                      'ask)
                                     (t 'run))
                       :name (plist-get call :name)
                       :input (plist-get call :input)
                       :marker marker
                       :condition condition
                       :reasons (plist-get call :reasons))
                 (lispm-agent-pending agent))))
    (lispm-set-state agent 'acting)
    (lispm-turns-dispatching agent
      (dolist (call calls)
        (let ((id (plist-get call :id)))
          (pcase (plist-get call :state)
            ("WAITING"
             (when (eq (plist-get (gethash id (lispm-agent-pending agent)) :status) 'ask)
               (lispm--approval-event agent id 'resumed)))
            ;; Approved, but never run: ask again rather than run it
            ;; while the session is being opened.
            ("APPROVED"
             (lispm-in-session agent
               (lispm-transcript-set-state
                (plist-get (gethash id (lispm-agent-pending agent)) :marker) "WAITING"))
             (lispm--approval-event agent id 'resumed))
            (_ (unless (run-hook-with-args-until-success 'lispm-resume-call-functions agent call)
                 (lispm-turns-tool-done agent id lispm-interrupted-result t)))))))
    (lispm-turns-update-state agent)
    ;; Calls stopped at an error wait for a restart again; the
    ;; handlers are asked afresh.
    (maphash (lambda (_id call)
               (when-let* ((condition (plist-get call :condition)))
                 (lispm-conditions-offer agent condition)))
             (lispm-agent-pending agent))
    (when (zerop (hash-table-count (lispm-agent-pending agent)))
      (lispm-signal agent 'interrupted
                    "Emacs stopped in the middle of this turn, while calls ran; \
they are recorded as interrupted.")))
   (t
    (lispm-signal agent 'interrupted
                  "Emacs stopped in the middle of this turn, while the model was answering."))))

(cl-defun lispm--recorded-turn-value (entry)
  "The value successfully returned in the turn handling ENTRY so far.
Read recorded tool inputs and results, never execute them.  A receipt
identifies the input when available; older sessions use the last
nonempty user or agent message, or the event that started the turn.
New inbox entries store the actual value, including values set by
nested tool calls and before a continuation, so prefer that snapshot."
  (when (plist-member entry :value)
    (cl-return-from lispm--recorded-turn-value (plist-get entry :value)))
  (let* ((sections (lispm-transcript-sections))
         (id (plist-get entry :id))
         (input
          (or (and id
                   (seq-find
                    (lambda (section)
                      (and (memq (plist-get section :kind) '(user message event))
                           (equal id (cdr (assoc "ENTRY"
                                                (lispm-transcript-properties
                                                 (plist-get section :body)
                                                 (plist-get section :end)))))))
                    (reverse sections)))
              (seq-find
               (lambda (section)
                 (and (or (memq (plist-get section :kind) '(user message))
                          (and (eq (plist-get section :kind) 'event)
                               (equal (plist-get section :arg) (plist-get entry :event))))
                      (not (string-empty-p (lispm-transcript-section-text section)))))
               (reverse sections))))
         value)
    (when input
      (dolist (section (cdr (memq input sections)))
        (when (and (eq (plist-get section :kind) 'assistant)
                   (not (plist-get section :ignore)))
          (dolist (part (lispm-transcript-parts section))
            (when (and (eq (plist-get part :kind) 'tool)
                       (equal (plist-get part :name) "return_value")
                       (plist-member part :result)
                       (not (plist-get part :error)))
              (setq value (alist-get 'value (plist-get part :input))))))))
    value))

(defun lispm--resume-ended-turn (agent entry parsed)
  "Finish with ENTRY, the input of AGENT's turn that ended as Emacs stopped.
The turn's reply, the text of the last response in PARSED, the
session as `lispm-transcript-parse' gives it, goes to ENTRY's callback,
if it has one; the inbox file then no longer holds ENTRY."
  (if (and (plist-get entry :reply-to) (plist-get entry :awaiting))
      (lispm-turns-suspend-request agent entry)
    (when-let* ((callback (plist-get entry :callback)))
      (let* ((last (seq-find (lambda (m) (equal (alist-get 'role m) "assistant"))
                             (reverse (plist-get parsed :messages))))
             (text (string-trim (lispm-content-text (append (alist-get 'content last) nil))))
             (reply (if (string-empty-p text) "(no reply)" text))
             (value (lispm--recorded-turn-value entry)))
        (when value
          (setq reply (propertize reply 'lispm-value value)))
        (with-demoted-errors "lispm turn callback: %S"
          (funcall callback reply)))))
  (lispm-inbox-save agent))

(defun lispm--ask-approval (agent id)
  "Show AGENT's call ID for approval: a preview, why it asks, a notification.
They are shown once the preview is made: for a diff, by a program Emacs
does not wait for (see `lispm-tool-preview-text'), if the call still
awaits the same approval by then."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (asked (lispm-asked call))
         (asking (plist-get call :asking))
         (announce (lambda (preview)
                     (when (and (eq (gethash id (lispm-agent-pending agent)) call)
                                (eq (plist-get call :status) 'ask)
                                (eq (plist-get call :asking) asking))
                       (lispm--announce-approval agent call asked preview)))))
    (lispm--approval-event agent id 'requested)
    (if (plist-get asked :tool)
        (let ((lispm-current-agent agent)
              (default-directory (lispm-agent-root agent)))
          (lispm-tool-preview-text (plist-get asked :tool) (plist-get asked :input) announce))
      (funcall announce nil))))

(defun lispm--announce-approval (agent call asked preview)
  "Show AGENT's CALL, awaiting approval of ASKED (see `lispm-asked'), with PREVIEW.
PREVIEW, a string or nil, and why it asks go in the transcript; then
`lispm-approval-functions' run, and a message says where to answer."
  (let ((reasons (plist-get asked :reasons)))
    (when (and (or preview reasons) (plist-get call :marker))
      (lispm-in-session agent
        (goto-char (lispm-transcript-subtree-end (plist-get call :marker) 2))
        (insert (lispm-transcript-preview preview reasons))))
    (run-hook-with-args 'lispm-approval-functions agent (plist-get asked :name)
                        (plist-get asked :input))
    (let ((said (run-hook-with-args-until-success 'lispm-approval-message-functions
                                                  agent (plist-get asked :name))))
      (unless (eq said t)
        (message "%s" (cond (said)
                            ((buffer-live-p (lispm-agent-view agent))
                             (lispm-tr "λ %s wants to run %s: answer in %s" (lispm-agent-name agent)
                                       (plist-get asked :name) (buffer-name (lispm-agent-view agent))))
                            (t (lispm-tr "λ %s wants to run %s: review it with C-c C-y in %s"
                                         (lispm-agent-name agent) (plist-get asked :name)
                                         (buffer-name (lispm-agent-buffer agent))))))))))

(defun lispm--todo-state-changed ()
  "Settle a call or condition whose heading the user moved to a new TODO state.
A call is approved or denied; a condition, its turn's or one a call
stopped at, gets its default restart or the one that stops there.
This makes `org-todo', and the org agenda, an approval interface."
  (when-let* ((agent lispm-session-agent))
    (let* ((id (org-entry-get nil "TOOL_ID"))
           (call (and id (gethash id (lispm-agent-pending agent))))
           (condition (if id
                          (plist-get call :condition)
                        (let ((turn (lispm-agent-condition agent)))
                          (and (equal (org-entry-get nil "CONDITION_ID") (plist-get turn :id))
                               turn)))))
      (cond
       (condition
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (car (plist-get condition :restarts))))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (lispm-stop-restart condition)))))
       ((eq (plist-get call :status) 'ask)
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-resolve agent id 'approve))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-resolve agent id 'deny))))))))

(defun lispm--calls (agent statuses)
  "Ids of AGENT's calls with one of STATUSES, in transcript order."
  (let (ids)
    (maphash (lambda (id call)
               (when (memq (plist-get call :status) statuses)
                 (push (cons id (and (plist-get call :marker)
                                     (marker-position (plist-get call :marker))))
                       ids)))
             (lispm-agent-pending agent))
    (mapcar #'car (sort ids (lambda (a b) (< (or (cdr a) 0) (or (cdr b) 0)))))))

(defun lispm-waiting-calls (agent)
  "Ids of AGENT's calls awaiting approval, in transcript order."
  (lispm--calls agent '(ask)))

(defun lispm--open-calls (agent)
  "Ids of AGENT's calls awaiting an answer, in transcript order.
That is approval, or for a call stopped at an error, a restart."
  (lispm--calls agent '(ask stopped)))

(defun lispm--call-at-point (agent)
  "Id of AGENT's call awaiting an answer at point, if any."
  (when-let* ((id (ignore-errors (org-entry-get nil "TOOL_ID" t)))
              (call (gethash id (lispm-agent-pending agent))))
    (and (memq (plist-get call :status) '(ask stopped)) id)))

(defun lispm--answered-call (agent)
  "Id of AGENT's call at point awaiting an answer, or of its first one."
  (or (lispm--call-at-point agent) (car (lispm--open-calls agent))))

(defun lispm-review ()
  "Review the call at point, or the first awaiting approval, and decide.
For a call stopped at an error, or with none, the condition the turn
waits at, choose a restart."
  (interactive)
  (let* ((agent (lispm-buffer-agent))
         (id (lispm--answered-call agent))
         (condition (if id (lispm-conditions-call agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-choose-restart agent condition))
          (id (lispm-review-call agent id))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm-call-allowance (agent id)
  "What approving AGENT's call ID for good allows from then on.
\(always . TEXT) when the call says, in TEXT; `exact' when it allows
only this exact call again, see `lispm-tools-allowance'; else `tool',
every later call of its tool."
  (let ((asked (lispm-asked (gethash id (lispm-agent-pending agent)))))
    (cond ((plist-get asked :always) (cons 'always (plist-get asked :always)))
          ((and (plist-get asked :tool)
                (consp (lispm-tools-allowance (plist-get asked :tool) (plist-get asked :input) agent)))
           'exact)
          (t 'tool))))

(defun lispm-review-call (agent id)
  "Review AGENT's call ID awaiting approval, and decide."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (asked (lispm-asked call)))
    (when (plist-get call :marker)
      (goto-char (plist-get call :marker)))
    (ignore-errors (org-fold-show-subtree))
    (recenter 0)
    (pcase (car (read-multiple-choice
                 (lispm-tr "%s wants to run %s" (lispm-agent-name agent) (plist-get asked :name))
                 `((?y ,(lispm-tr-in "answer" "yes") ,(lispm-tr "Run this call"))
                   (?n ,(lispm-tr-in "answer" "no") ,(lispm-tr "Deny it; you may give a reason"))
                   (?a ,(lispm-tr-in "answer" "always")
                       ,(pcase (lispm-call-allowance agent id)
                          (`(always . ,text) (lispm-tr text))
                          ('exact (lispm-tr "Run it, and this exact call again, this Emacs session"))
                          (_ (lispm-tr "Run it, and every later call of this tool, this Emacs session"))))
                   (?q ,(lispm-tr-in "answer" "later") ,(lispm-tr "Decide later")))))
      (?y (lispm-resolve agent id 'approve))
      (?n (lispm-resolve agent id 'deny (read-string (lispm-tr "Reason (optional): "))))
      (?a (lispm-allow agent id)))))

(defun lispm-approve-for (agent &optional id)
  "Approve AGENT's call ID, by default its first awaiting an answer.
For a call stopped at an error, or with none, choose the default restart
of the condition it waits at."
  (let* ((id (or id (car (lispm--open-calls agent))))
         (condition (if id (lispm-conditions-call agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-restart agent (plist-get condition :id)
                                    (car (plist-get condition :restarts))))
          (id (lispm-resolve agent id 'approve))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm--approval-snapshot (agent)
  "AGENT's current turn's permission calls, in transcript order.
Each entry is (ID CALL ASKING), retaining the call and the approval
it awaits so an answer cannot approve a later call in its place."
  (cl-loop for id in (lispm-waiting-calls agent)
           for call = (gethash id (lispm-agent-pending agent))
           when (or (null (plist-get call :turn))
                    (eql (plist-get call :turn) (lispm-agent-turn agent)))
           collect (list id call (plist-get call :asking))))

(defun lispm--approve-snapshot (agent turn calls)
  "Approve CALLS of AGENT's TURN that still await the same permission.
CALLS is an approval snapshot; return the number approved.  A call
may finish synchronously, start another turn or ask another permission:
none of those new permissions is included."
  (let ((count 0))
    (pcase-dolist (`(,id ,call ,asking) calls)
      (when (and (eql turn (lispm-agent-turn agent))
                 (eq call (gethash id (lispm-agent-pending agent)))
                 (eq (plist-get call :status) 'ask)
                 (eq asking (plist-get call :asking)))
        (cl-incf count)
        (lispm-resolve agent id 'approve)))
    count))

(defun lispm-approve-all-for (agent)
  "Approve every pending permission call of AGENT's current turn.
Return the number approved.  Conditions and calls stopped at errors
are left for their own restart; no permission of another agent, a
later turn or a call that starts while approving these is approved."
  (lispm--approve-snapshot agent (lispm-agent-turn agent) (lispm--approval-snapshot agent)))

(defun lispm-approve-all ()
  "List and approve every pending permission call of this agent's turn."
  (interactive)
  (let* ((agent (lispm-buffer-agent))
         (turn (lispm-agent-turn agent))
         (calls (lispm--approval-snapshot agent)))
    (unless calls
      (user-error "%s" (lispm-tr "No call awaits approval")))
    (when (yes-or-no-p
           (lispm-tr "Approve these calls for %s: %s? "
                     (lispm-agent-name agent)
                     (mapconcat (pcase-lambda (`(,id ,call ,_))
                                  (format "%s (%s)" id (plist-get (lispm-asked call) :name)))
                                calls ", ")))
      (lispm--approve-snapshot agent turn calls))))

(defun lispm-deny-for (agent reason &optional id)
  "Deny AGENT's call ID, by default its first awaiting an answer, for REASON.
For a call stopped at an error, or with none, stop at the condition it
waits at (see `lispm-stop-at-condition')."
  (let* ((id (or id (car (lispm--open-calls agent))))
         (condition (if id (lispm-conditions-call agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-restart agent (plist-get condition :id)
                                    (lispm-stop-restart condition)))
          (id (lispm-resolve agent id 'deny reason))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm-approve ()
  "Approve the call at point, or the first one awaiting approval.
With none, choose the default restart of the condition the turn waits at."
  (interactive)
  (let ((agent (lispm-buffer-agent)))
    (lispm-approve-for agent (lispm--answered-call agent))))

(defun lispm-deny (reason)
  "Deny the call at point, or the first one awaiting approval, for REASON.
With none, stop at the condition the turn waits at (see
`lispm-stop-at-condition')."
  (interactive
   (let* ((agent (lispm-buffer-agent))
          (id (lispm--answered-call agent)))
     (list (and id (not (lispm-conditions-call agent id))
                (read-string (lispm-tr "Reason (optional): "))))))
  (let ((agent (lispm-buffer-agent)))
    (lispm-deny-for agent reason (lispm--answered-call agent))))


(keymap-set lispm-session-mode-map "C-c RET" #'lispm-send)
(keymap-set lispm-session-mode-map "C-c C-k" #'lispm-abort)
(keymap-set lispm-session-mode-map "C-c C-y" #'lispm-review)

(defun lispm-approval--session-mode-setup ()
  "Wire approval hooks and resume pending calls in `lispm-session-mode'."
  (if lispm-session-mode
      (progn
        (add-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed nil t)
        (let ((agent lispm-session-agent))
          (when (and agent (eq (lispm-agent-buffer agent) (current-buffer)))
            (lispm--resume-calls agent))))
    (remove-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed t)))

(add-hook 'lispm-session-mode-hook #'lispm-approval--session-mode-setup)

(setq lispm-runtime-ask-approval-function #'lispm--ask-approval
      lispm-runtime-open-calls-function #'lispm--open-calls
      lispm-runtime-stop-function #'lispm--stop)

(provide 'lispm-approval)

;;; lispm-approval.el ends here
