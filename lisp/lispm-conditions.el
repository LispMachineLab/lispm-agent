;;; lispm-conditions.el --- Runtime conditions, restarts, and event/timer triggers  -*- lexical-binding: t; -*-

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

;; Condition signaling, interactive and handler-driven restarts,
;; model switching, and `lispm-watch'/`lispm-every' triggers.

;;; Code:

(require 'org)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-tools)
(require 'lispm-session)
(require 'lispm-turns)

;;;; Conditions and restarts
;;
;; A turn that stops early does not end: it waits at a condition, as
;; a call waits for approval, until a restart is chosen, as on a Lisp
;; Machine an error waits in the debugger until a proceed option is.
;; The condition is a heading in the transcript (a WAITING "Condition:"
;; section, never sent to the model), so it survives a restart of
;; Emacs; `lispm-condition-handlers' may choose a restart at once, else
;; the user does, in the chat, the org transcript or the agenda.
;; `abort' ends the turn as it ended before: a note, nothing more.

(defvar lispm-condition-types
  `((request-failed
     :title ,(lispm-tr-noop "Request failed")
     :restarts ((retry ,(lispm-tr-noop "Retry the request") lispm--restart-retry)
                (use-model ,(lispm-tr-noop "Retry with another backend or model")
                           lispm--restart-use-model lispm-read-model)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (step-limit
     :title ,(lispm-tr-noop "Step limit reached")
     :restarts ((continue ,(lispm-tr-noop "Continue, for as many requests again") lispm--restart-continue)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (output-limit
     :title ,(lispm-tr-noop "Response cut short")
     :restarts ((retry-larger ,(lispm-tr-noop "Ask again, with twice the output limit")
                              lispm--restart-retry-larger)
                (keep ,(lispm-tr-noop "Keep what it wrote, and let it go on") lispm--restart-keep)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (context-full
     :title ,(lispm-tr-noop "Context window full")
     :restarts ((compact ,(lispm-tr-noop "Compact the conversation, then go on") lispm--restart-compact)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (refused
     :title ,(lispm-tr-noop "Response refused")
     :restarts ((retry ,(lispm-tr-noop "Ask again") lispm--restart-ask-again)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (paused
     :title ,(lispm-tr-noop "Response paused")
     :restarts ((continue ,(lispm-tr-noop "Let it go on") lispm--restart-go-on)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (compaction-failed
     :title ,(lispm-tr-noop "Compaction failed")
     :restarts ((retry ,(lispm-tr-noop "Try to compact again") lispm--restart-compaction-retry)
                (skip ,(lispm-tr-noop "Carry on without compacting") lispm--restart-compaction-skip)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-compaction-abort)))
    (tool-error
     :title ,(lispm-tr-noop "Call failed")
     :restarts ((report ,(lispm-tr-noop "Tell the model it failed") lispm--restart-report)
                (retry ,(lispm-tr-noop "Run it again") lispm--restart-rerun)
                (use-value ,(lispm-tr-noop "Give the model a result of your own")
                           lispm--restart-use-value lispm--read-result)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-stop-call)))
    (interrupted
     :title ,(lispm-tr-noop "Turn interrupted")
     :restarts ((continue ,(lispm-tr-noop "Go on with the turn") lispm--restart-retry)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort))))
  "Kinds of conditions a turn can stop at, and how it can go on.
Each entry is (TYPE :title TITLE :restarts RESTARTS).  TITLE says what
happened.  Each restart is (NAME DESCRIPTION FUNCTION [READER]), the
first being the default: FUNCTION is called with the agent, the
condition (a plist: :id :type :title :message :restarts) and a value,
which READER, if any, asks the user for when they choose it.  TITLE
and DESCRIPTION are English, as the transcript records them, and
translated where shown (see lispm-i18n.el).")

(defcustom lispm-condition-handlers '((paused continue :times 10))
  "Restarts chosen without asking, by condition type, in turns the user started.
Each entry is (TYPE RESTART . PROPS): a condition of TYPE gets RESTART
at once, or after :after seconds, at most :times times in a turn (1
by default), or for a call stopped at an error, in that call; after
that, :then names the restart to choose, or the user chooses.  :value
is the value RESTART needs, if any.  A call stopped at an error that
a handler runs again runs only if the policy would run it unasked;
else it awaits approval again.  For instance, to retry a failed
request after a minute, twice:

  ((request-failed retry :after 60 :times 2))

By default a paused response goes on, ten times in a turn at most.
Turns started by another agent's message or by an event go by
`lispm-unattended-condition-handlers' instead."
  :type '(alist :key-type symbol :value-type (cons symbol plist))
  :group 'lispm)

(defcustom lispm-unattended-condition-handlers
  '((request-failed retry :after 60 :times 2 :then abort)
    (step-limit abort)
    (output-limit retry-larger :then abort)
    (context-full compact :then abort)
    (refused abort)
    (paused continue :times 10 :then abort)
    (compaction-failed skip)
    (tool-error report)
    (interrupted continue :then abort))
  "Restarts chosen without asking in turns nobody may be watching.
Those are turns started by another agent's message (a helper's, say)
or by an event (a hook, a timer), which, left to wait for the user,
would hold up whoever waits for them, or the next event.  Entries are
as in `lispm-condition-handlers'.  By default a failed request is
tried again twice, a minute apart, and then the turn stops, as does
one that reaches `lispm-max-steps' or is refused; a response cut
short is asked for again with a larger limit, or the session handed
over, once; a paused one goes on; a failed handover is skipped; a
call stopped at an error reports it to the model; a turn Emacs
stopped in the middle of goes on, once its session is open again.
Meanwhile the user can choose."
  :type '(alist :key-type symbol :value-type (cons symbol plist))
  :group 'lispm)

(defvar lispm-condition-functions nil
  "Abnormal hook run with an agent and a condition its turn waits at.
The condition is a plist (:id :type :title :message :restarts).  Use
it for notifications, as `lispm-approval-functions' for approvals.")

(defun lispm--condition-spec (type)
  "The entry of `lispm-condition-types' for TYPE."
  (or (alist-get type lispm-condition-types)
      (error "No condition type %s" type)))

(defun lispm--restart-spec (condition restart)
  "The restart RESTART of CONDITION, as `lispm-condition-types' gives it."
  (or (assq restart (plist-get (lispm--condition-spec (plist-get condition :type)) :restarts))
      (error "%s is not a restart of %s" restart (plist-get condition :type))))

(defun lispm-signal (agent type message &rest props)
  "Stop AGENT's turn at a condition of TYPE, saying MESSAGE.
The turn waits there until a restart is chosen (see `lispm-restart'):
by `lispm-condition-handlers', or by the user.  PROPS: :resume
non-nil if a turn waits in the middle, for a condition signaled
between turns (`compaction-failed')."
  (let* ((spec (lispm--condition-spec type))
         (restarts (mapcar #'car (plist-get spec :restarts)))
         (id (lispm-in-session agent
               (format "c%d" (1+ (count-matches "^:CONDITION_ID:" (point-min) (point-max))))))
         (resume (plist-get props :resume))
         (condition (list :id id :type type :title (plist-get spec :title)
                          :message message :restarts restarts :resume resume)))
    (setf (lispm-agent-process agent) nil)
    (lispm-in-session agent
      ;; Before the input heading a turn ended with, if any: the next
      ;; turn takes its place.
      (if-let* ((input (lispm-session-input-heading-p)))
          (goto-char input)
        (lispm-session-newline))
      (setq condition (plist-put condition :marker (point-marker)))
      (insert (lispm-transcript-condition (plist-get spec :title) id type restarts message
                                          resume)))
    (lispm-save-session agent)
    (lispm-conditions-wait-at agent condition)
    condition))

(defun lispm-conditions-wait-at (agent condition)
  "Make AGENT wait at CONDITION; let the handlers choose a restart, if any."
  (setf (lispm-agent-condition agent) condition)
  (lispm-set-state agent 'waiting
                   (format "%s: %s" (plist-get condition :title)
                           (lispm-oneline (plist-get condition :message) 80)))
  (lispm-conditions-offer agent condition))

(defun lispm-conditions-call (agent id)
  "The condition AGENT's call ID stopped at, if it did."
  (plist-get (gethash id (lispm-agent-pending agent)) :condition))

(defun lispm--find-condition (agent id)
  "The condition AGENT waits at whose id is ID: its turn's, or a call's."
  (let ((turn (lispm-agent-condition agent)))
    (if (equal (plist-get turn :id) id)
        turn
      (lispm-conditions-call agent id))))

(defun lispm-asked-condition (agent)
  "The condition AGENT asks about first, if any.
That of its first call awaiting an answer, if that call stopped at an
error; else, when no call awaits one, the condition its turn waits at."
  (if-let* ((id (and lispm-runtime-open-calls-function
                     (car (funcall lispm-runtime-open-calls-function agent)))))
      (lispm-conditions-call agent id)
    (lispm-agent-condition agent)))

(defun lispm-conditions-offer (agent condition)
  "Tell whoever listens that AGENT waits at CONDITION; let the handlers choose."
  (run-hook-with-args 'lispm-condition-functions agent condition)
  (unless (lispm--handle-condition agent condition)
    (message "%s" (if (buffer-live-p (lispm-agent-view agent))
                      (lispm-tr "λ %s: %s; choose what to do in %s" (lispm-agent-name agent)
                                (lispm-tr (plist-get condition :title))
                                (buffer-name (lispm-agent-view agent)))
                    (lispm-tr "λ %s: %s; choose what to do with C-c C-y in %s" (lispm-agent-name agent)
                              (lispm-tr (plist-get condition :title))
                              (buffer-name (lispm-agent-buffer agent)))))))

(defvar lispm--handler-restart nil
  "Non-nil while a handler, rather than the user, chooses a restart.")

(defun lispm--handle-condition (agent condition)
  "Choose a restart for AGENT's CONDITION by the handlers of its turn.
Those are `lispm-condition-handlers', or for a turn the user did not
start, `lispm-unattended-condition-handlers'.  Return non-nil if one
is chosen, now or after a delay."
  (pcase-let* ((type (plist-get condition :type))
               (`(,first . ,props) (alist-get type (if (lispm-agent-attended agent)
                                                         lispm-condition-handlers
                                                       lispm-unattended-condition-handlers)))
               ;; A call's condition counts for that call alone.
               (key (if-let* ((call (plist-get condition :call))) (cons type call) type))
               (count (alist-get key (lispm-agent-auto-restarts agent) 0 nil #'equal))
               (again (< count (or (plist-get props :times) 1)))
               (restart (if again first (plist-get props :then))))
    (when (and restart (memq restart (plist-get condition :restarts)))
      (when again
        (setf (alist-get key (lispm-agent-auto-restarts agent) nil nil #'equal) (1+ count)))
      (let ((after (if again (or (plist-get props :after) 0) 0))
            (value (plist-get props :value))
            (id (plist-get condition :id)))
        (when (> after 0)
          (lispm-set-state agent 'waiting
                           (format "%s; %s in %ds" (plist-get condition :title) restart after)))
        (run-at-time after nil
                     (lambda ()
                       ;; Unless someone chose meanwhile: this very
                       ;; condition, since an id can be given again
                       ;; once its heading is gone.
                       (when (eq (lispm--find-condition agent id) condition)
                         (let ((lispm--handler-restart t))
                           (lispm-restart agent id restart value)))))
        t))))

(defun lispm-restart (agent id restart &optional value)
  "Go on with AGENT's turn, stopped at the condition ID, by RESTART.
VALUE is what the restart needs, if anything: one with a reader in
`lispm-condition-types' needs one, such as `use-model', a cons
\(BACKEND . MODEL).  The condition's heading records the choice."
  (let ((condition (or (lispm--find-condition agent id)
                       (user-error "%s" (lispm-tr "%s waits at no condition %s" (lispm-agent-name agent) id)))))
    (let ((spec (lispm--restart-spec condition restart)))
      (when (and (nth 3 spec) (null value))
        (user-error "%s" (lispm-tr "The restart %s needs a value" restart)))
      (if (plist-get condition :call)
          (lispm-conditions-settle-call agent condition restart)
        (setf (lispm-agent-condition agent) nil)
        (lispm-conditions-close agent condition restart value))
      (funcall (nth 2 spec) agent condition value))))

(defun lispm-conditions-settle-call (agent condition restart)
  "Note that the call CONDITION stopped AGENT at is settled by RESTART.
The call is no longer stopped; its heading loses :CONDITION:, and its
Stopped subsection names the restart."
  (let ((call (gethash (plist-get condition :call) (lispm-agent-pending agent))))
    (plist-put call :condition nil)
    (when-let* ((marker (plist-get call :marker))
                ((marker-buffer marker)))
      (lispm-in-session agent
        (lispm-transcript-set-property marker "CONDITION" nil)
        (goto-char marker)
        (when (re-search-forward "^\\*\\*\\* Stopped[ \t]*$"
                                 (lispm-transcript-subtree-end marker 2) t)
          (replace-match (format "*** Stopped: %s" restart) t t))))))

(defun lispm-stop-at-condition (agent)
  "Stop AGENT's turn at the condition it asks about, if any.
That is restart `abort', or for a condition without it, the last of
its restarts.  What the user asks of AGENT next goes past it this way."
  (when-let* ((condition (lispm-asked-condition agent)))
    (lispm-restart agent (plist-get condition :id) (lispm-stop-restart condition))))

(defun lispm-stop-restart (condition)
  "The restart of CONDITION that stops there: `abort', or its last one."
  (let ((restarts (plist-get condition :restarts)))
    (if (memq 'abort restarts) 'abort (car (last restarts)))))

(defun lispm-conditions-between-turns-p (condition)
  "Non-nil if CONDITION was signaled between turns, not in one.
Only a failed handover is: it comes before the input that asked for it."
  (and (eq (plist-get condition :type) 'compaction-failed)
       (not (plist-get condition :resume))))

(defun lispm-awaiting (agent)
  "What AGENT awaits an answer to: its calls awaiting one, then its condition."
  (append (and lispm-runtime-open-calls-function
               (funcall lispm-runtime-open-calls-function agent))
          (and (lispm-agent-condition agent) (list (lispm-agent-condition agent)))))

(defun lispm-restart-description (condition restart)
  "How RESTART of CONDITION is described to the user, in the language
spoken: its name if unknown."
  (if-let* ((spec (assq restart (plist-get (alist-get (plist-get condition :type)
                                                       lispm-condition-types)
                                            :restarts))))
      (lispm-tr (nth 1 spec))
    (symbol-name restart)))

(defun lispm-conditions-close (agent condition restart &optional value)
  "Record in AGENT's transcript that CONDITION was settled by RESTART.
Its heading becomes DONE and names the restart, and the VALUE given
it, if any; `abort' removes it, since the note the turn ends with
says the same."
  (when-let* ((marker (plist-get condition :marker))
              ((marker-buffer marker)))
    (lispm-in-session agent
      (goto-char marker)
      ;; Changing its TODO state in org may have chosen the restart.
      (when (looking-at (concat "\\* " lispm-transcript-state-re " Condition: "))
        (if (eq restart 'abort)
            (delete-region marker (lispm-transcript-subtree-end marker 1))
          (lispm-transcript-set-state marker "DONE")
          (lispm-transcript-set-property marker "RESTART" restart)
          (when value
            (lispm-transcript-set-property marker "VALUE" (let ((print-escape-newlines t))
                                                             (prin1-to-string value)))))))))

(defun lispm-choose-restart (&optional agent condition)
  "Choose a restart for CONDITION, by default the one AGENT asks about.
Interactively, the agent of the current session or chat."
  (interactive)
  (let* ((agent (or agent (lispm-buffer-agent)))
         (condition (or condition
                        (lispm-asked-condition agent)
                        (user-error "%s" (lispm-tr "%s waits at no condition" (lispm-agent-name agent)))))
         (keys nil)
         (choices
          (mapcar (lambda (name)
                    (let* ((letter (aref (symbol-name name) 0))
                           (key (if (or (memq letter keys) (eq letter ?q))
                                    (+ ?1 (length keys))
                                  letter)))
                      (push key keys)
                      (list key (symbol-name name) (lispm-restart-description condition name))))
                  (plist-get condition :restarts)))
         (answer (read-multiple-choice
                  (format "%s: %s" (lispm-tr (plist-get condition :title)) (plist-get condition :message))
                  (append choices `((?q ,(lispm-tr-in "answer" "later") ,(lispm-tr "Decide later")))))))
    (unless (eq (car answer) ?q)
      (lispm-choose-restart-named agent (intern (nth 1 answer)) condition))))

(defun lispm-choose-restart-named (agent restart &optional condition)
  "Choose RESTART for CONDITION, by default the one AGENT asks about.
If the restart needs a value, ask the user for it."
  (let* ((condition (or condition
                        (lispm-asked-condition agent)
                        (user-error "%s" (lispm-tr "%s waits at no condition" (lispm-agent-name agent)))))
         (reader (nth 3 (lispm--restart-spec condition restart)))
         (value (and reader (funcall reader agent))))
    ;; Reading took time: the condition may be settled meanwhile.
    (unless (eq condition (lispm--find-condition agent (plist-get condition :id)))
      (user-error "%s" (lispm-tr "%s no longer waits at that condition" (lispm-agent-name agent))))
    (lispm-restart agent (plist-get condition :id) restart value)))

(defun lispm--restart-retry (agent _condition _value)
  "Send AGENT's request again."
  (lispm-turns-request agent))

(defun lispm--restart-continue (agent _condition _value)
  "Go on with AGENT's turn for another `lispm-max-steps' requests."
  (setf (lispm-agent-steps agent) 0)
  (lispm-turns-request agent))

(defun lispm--restart-use-model (agent _condition value)
  "Retry AGENT's request with the backend and model in VALUE, (BACKEND . MODEL)."
  (lispm-use-model agent (car value) (cdr value))
  (run-hook-with-args 'lispm-model-change-functions agent)
  (lispm-turns-request agent))

(defun lispm-use-model (agent backend model)
  "Have AGENT use BACKEND (a key of `lispm-backends', nil for the default)
and MODEL (nil for the backend's), for the rest of its session."
  (when (and backend (not (lispm-backend backend)))
    (user-error "%s" (lispm-tr "Unknown backend %s (see `lispm-backends')" backend)))
  (setf (lispm-agent-backend agent) backend
        (lispm-agent-model agent) model))

(defun lispm-conditions-restore-model (agent)
  "Have AGENT use the model its session last switched to with `use-model'.
The condition's heading records it, so it outlives a restart of Emacs."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (when (re-search-backward "^:RESTART: use-model\n:VALUE: \\(.*\\)$" nil t)
        (when-let* ((value (condition-case nil
                               (car (read-from-string (match-string 1)))
                             ((invalid-read-syntax end-of-file) nil)))
                    ((consp value))
                    ((or (null (car value)) (lispm-backend (car value)))))
          (setf (lispm-agent-backend agent) (car value)
                (lispm-agent-model agent) (cdr value)))))))

(defun lispm--restart-abort (agent condition _value)
  "End AGENT's turn at CONDITION, as a turn stopped early used to end."
  (lispm-turns-finish agent (plist-get condition :message)))

(defun lispm-conditions-tool-error (id name text marker &optional images)
  "The condition of the call ID, of the tool NAME, stopped at the error TEXT.
MARKER is where the call's heading is; IMAGES are files the failure
holds besides TEXT."
  (let ((spec (lispm--condition-spec 'tool-error)))
    (list :id id :call id :type 'tool-error :title (plist-get spec :title)
          :message (format "%s failed: %s" name text) :error text :images images
          :restarts (mapcar #'car (plist-get spec :restarts)) :marker marker)))

(defun lispm--stop-call (agent id text images)
  "Stop AGENT's call ID at the error TEXT, instead of giving it to the model.
IMAGES are files the failure holds besides TEXT.  The call waits at a
`tool-error' condition of its own: its heading goes back to WAITING,
with :CONDITION: and a Stopped subsection holding TEXT, so that a
session reopened later takes it up again (without IMAGES)."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (marker (plist-get call :marker))
         (condition (lispm-conditions-tool-error id (plist-get call :name) text marker images)))
    (plist-put call :status 'stopped)
    (plist-put call :condition condition)
    (when (and marker (marker-buffer marker))
      (lispm-in-session agent
        (lispm-transcript-set-state marker "WAITING")
        (lispm-transcript-set-property marker "CONDITION" "tool-error")
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-stopped text))))
    (lispm-save-session agent)
    (unless (lispm-agent-dispatching agent)
      (lispm-turns-update-state agent))
    (lispm-conditions-offer agent condition)))

(defun lispm--restart-report (agent condition _value)
  "Give the model the error the call stopped at CONDITION, as its result."
  (lispm-turns-tool-done agent (plist-get condition :call) (plist-get condition :error) t
                    nil (plist-get condition :images)))

(defun lispm--restart-rerun (agent condition _value)
  "Run the call stopped at CONDITION again.
Chosen by a handler rather than the user, it runs only if the policy
would run it unasked; else it awaits approval again."
  (let* ((id (plist-get condition :call))
         (call (gethash id (lispm-agent-pending agent)))
         (tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name))))
         (decision (and lispm--handler-restart tool
                        (lispm-tool-decision agent tool (plist-get call :input)))))
    (if (and decision (not (eq (car decision) 'auto)))
        (progn
          (plist-put call :status 'ask)
          (plist-put call :reasons (cdr decision))
          (lispm-turns-update-state agent)
          (when lispm-runtime-ask-approval-function
            (funcall lispm-runtime-ask-approval-function agent id)))
      (lispm-turns-run-tool agent id))))

(defun lispm--restart-use-value (agent condition value)
  "Give the model VALUE as the result of the call stopped at CONDITION."
  (lispm-turns-tool-done agent (plist-get condition :call) value nil))

(defun lispm--read-result (_agent)
  "Ask the user for the result to give the model."
  (let ((result (read-string (lispm-tr "Result to give the model: "))))
    (when (string-blank-p result)
      (user-error "%s" (lispm-tr "No result given")))
    result))

(defun lispm--restart-stop-call (agent condition _value)
  "Stop AGENT's turn at the call stopped at CONDITION.
The call gets its error as its result, the other calls stop, and the
turn ends with the error; inputs waiting for AGENT stay."
  (let ((id (plist-get condition :call)))
    ;; Its result must not ask the model again.
    (lispm-turns-dispatching agent
      (lispm-turns-tool-done agent id (plist-get condition :error) t
                        nil (plist-get condition :images)))
    (when lispm-runtime-stop-function
      (funcall lispm-runtime-stop-function agent (plist-get condition :message) t))))

(defun lispm--drop-response (agent condition)
  "Leave out of AGENT's requests the response CONDITION stopped at.
That is the \"* Assistant\" section right before CONDITION's heading,
if there is one (a response may have had nothing in it): tagged
:ignore:, it stays in the transcript but is no longer sent."
  (when-let* ((marker (plist-get condition :marker))
              ((marker-buffer marker)))
    (lispm-in-session agent
      (goto-char marker)
      (when (and (re-search-backward "^\\* " nil t)
                 (looking-at (lispm-transcript-heading-regexp 'assistant)))
        (end-of-line)
        (insert " :ignore:")))))

(defun lispm--restart-ask-again (agent condition _value)
  "Leave out the response AGENT's CONDITION stopped at, and ask again."
  (lispm--drop-response agent condition)
  (lispm-turns-request agent))

(defun lispm--restart-retry-larger (agent condition _value)
  "Leave out the response cut short at CONDITION; ask again with twice the limit.
The limit stays raised for the rest of AGENT's turn."
  (setf (lispm-agent-max-tokens agent) (* 2 (lispm-turns-max-tokens agent)))
  (lispm--restart-ask-again agent condition nil))

(defun lispm-conditions-tell-for-user (agent text)
  "Tell AGENT's model TEXT, as LispM passing on the user's choice; ask it again."
  (lispm-session-add-section agent (lispm-transcript-title 'from-lispm) text)
  (lispm-turns-request agent))

(defun lispm--restart-keep (agent _condition _value)
  "Keep the response AGENT's turn stopped at, and let the model go on."
  (lispm-conditions-tell-for-user
   agent "Your last response was cut short by the output limit before it was complete; \
calls in it were not run.  Go on from where it stopped."))

(defun lispm--restart-go-on (agent _condition _value)
  "Let AGENT's model resume a response it paused.
The response goes back as it is, which is how the API resumes it."
  (setf (lispm-agent-continuing agent) t)
  (lispm-turns-request agent))

(defun lispm--restart-compact (agent condition _value)
  "Leave out the response cut short at CONDITION, hand AGENT's session over,
and go on with the turn in the continuation."
  (lispm--drop-response agent condition)
  (setf (lispm-agent-compacting agent) t)
  (funcall lispm-runtime-summarize-function agent t))

(defun lispm--restart-compaction-retry (agent condition _value)
  "Try again to hand AGENT's session over, as CONDITION says it was being."
  (setf (lispm-agent-compacting agent) t)
  (funcall lispm-runtime-summarize-function agent (plist-get condition :resume)))

(defun lispm--restart-compaction-abort (agent condition _value)
  "Stop at CONDITION, AGENT's failed handover.
In the middle of a turn, the turn ends; between turns, the input that
asked for the handover is dropped, and whoever sent it told."
  (if (plist-get condition :resume)
      (lispm-turns-finish agent (plist-get condition :message))
    (when-let* ((entry (pop (lispm-agent-inbox agent))))
      (when-let* ((callback (plist-get entry :callback)))
        (funcall callback (format "(%s stopped before handling this: %s)"
                                  (lispm-agent-name agent) (plist-get condition :message)))))
    (lispm-session-record-event agent "compaction" (plist-get condition :message))
    (lispm-set-state agent 'idle)
    (lispm-save-session agent)
    (lispm-inbox-save agent)
    (lispm-turns-drain-inbox agent)))

(defun lispm--restart-compaction-skip (agent condition _value)
  "Carry on in AGENT's long session, without handing it over.
In the middle of a turn, the turn goes on; between turns, the input
that asked for the handover starts the next one."
  (if (plist-get condition :resume)
      (lispm-turns-request agent)
    ;; Do not try again before the input that asked for it.
    (when (lispm-agent-inbox agent)
      (setcar (lispm-agent-inbox agent)
              (plist-put (car (lispm-agent-inbox agent)) :no-compact t)))
    (lispm-set-state agent 'idle)
    (lispm-turns-drain-inbox agent)))

(defun lispm-read-model (agent)
  "Ask for a backend and a model for AGENT; return (BACKEND . MODEL)."
  (let* ((current (lispm-backend-name agent))
         (own (when-let* ((character (lispm-agent-character* agent)))
                (lispm-character-backend-name character)))
         (name (completing-read (format-prompt (lispm-tr "Backend") current)
                                (delete-dups
                                 (append (and own (lispm-backend own) (list (symbol-name own)))
                                         (mapcar (lambda (b) (symbol-name (car b))) lispm-backends)))
                                nil t nil nil (and current (symbol-name current))))
         (backend (if (string-empty-p name)
                      (user-error "%s" (lispm-tr "No backend chosen"))
                    (intern name)))
         (default (plist-get (lispm-backend backend) :model))
         (model (read-string (format-prompt (lispm-tr "Model") default) nil nil default)))
    (cons backend (and model (not (string-empty-p model)) model))))

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


(setq lispm-runtime-unwatch-function #'lispm-unwatch
      lispm-runtime-signal-function #'lispm-signal
      lispm-runtime-stop-call-function #'lispm--stop-call)

(provide 'lispm-conditions)

;;; lispm-conditions.el ends here
