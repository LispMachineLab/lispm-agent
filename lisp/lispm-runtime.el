;;; lispm-runtime.el --- The agent execution loop  -*- lexical-binding: t; -*-

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
;;        a running call may wait, too, for approval of a call it makes
;;        in turn, as a Lisp program does (`lispm-call-ask');
;;        when every call has a result, request again
;;     -> response without tool calls: the turn ends, the session is
;;        saved (and committed), turn callbacks and hooks run, and the
;;        next inbox item, if any, starts a new turn.
;;
;; A turn that stops early (a request fails, the step limit is reached)
;; does not end: it waits at a condition, recorded in the transcript,
;; until a restart is chosen, by a handler or by the user, as a call
;; waits for approval.  See "Conditions and restarts" below.
;;
;; None of it is lost when Emacs stops.  The session is saved as a
;; turn goes, and the inbox in the project's inbox file
;; (lispm-inbox.el); opening a session again takes up the turn it was
;; in (`lispm--resume-calls'), and what was queued for it.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'project)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-inbox)
(require 'lispm-journal)
(require 'lispm-session)
(require 'lispm-turns)
(require 'lispm-conditions)
(require 'lispm-approval)

;; Compaction saves memory through lispm-memory; loaded before a turn triggers it.
(declare-function lispm-git-commit "lispm-memory" (agent message))

;;;; Handing over: compaction

(defconst lispm--save-memory-prompt
  "Your context is long and will be compacted: most of this conversation will be replaced by a summary. Before that, save what should outlive it: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Do not summarize the conversation now. Reply in one sentence when done."
  "Asked of an agent before its session is handed over.")

(defconst lispm--resumed-memory-prompt
  "Your context filled up in the middle of this turn and was compacted: the summary above is all that is left of the earlier conversation. Save what should outlive it, if you have not already: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Then go on with the turn."
  "Told an agent whose session was handed over in the middle of a turn.")

(defconst lispm--summary-prompt
  "Write a handoff summary of this conversation for yourself: a fresh session will hold this summary in place of the conversation and continue from it. Cover the user's goals and requests, the decisions made and why, the current state of the work (files changed, what was verified), and the open questions and next steps. Be specific: names, paths, values. Leave out what is already saved in memory or tasks. Reply with the summary only."
  "Asked of an agent to summarize its session for the continuation.")

(defconst lispm--summary-kept-prompt
  "The latest turns, from the input that begins \"%s\", will follow the summary as they are: summarize what comes before them, and say little about them."
  "Added to `lispm--summary-prompt' when a handover keeps the latest turns.")

(defun lispm--session-bytes (agent)
  "Size in bytes of AGENT's session."
  (let ((buffer (lispm-agent-buffer agent)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (1- (position-bytes (point-max)))))
      0)))

(defun lispm--estimated-tokens (agent)
  "Size in tokens of AGENT's session, as its next request would carry it.
That is the size the latest response reported, plus a token for every
three bytes written since; before any response, the whole session's
bytes by three.  Bytes, not characters: a token is about four
characters of English text, but about one character of Chinese, which
takes three bytes."
  (let ((bytes (lispm--session-bytes agent))
        (known (lispm-agent-context agent)))
    (if known
        (+ (car known) (/ (max 0 (- bytes (cdr known))) 3))
      (/ bytes 3))))

(defun lispm--result-bulky-p (result)
  "Non-nil when transcript RESULT exceeds `lispm-compact-bulky-result-bytes'."
  (>= (cadr result) lispm-compact-bulky-result-bytes))

(defun lispm--clear-results (agent)
  "Clear the output of AGENT's older tool calls, if that frees enough.
Results older than the latest `lispm-compact-keep-results' are candidates:
if clearing bulky ones alone frees at least `lispm-compact-clear-at-least'
tokens, smaller older reads stay intact; otherwise all candidates clear.
Return the number of tokens freed, or nil."
  (when lispm-compact-keep-results
    (let* ((old (seq-remove (lambda (result) (nth 2 result))
                            (butlast (lispm-in-session agent (lispm-transcript-results))
                                     lispm-compact-keep-results)))
           (bulky (seq-filter #'lispm--result-bulky-p old))
           (bulky-freed (/ (apply #'+ (mapcar #'cadr bulky)) 3))
           (chosen (if (and bulky (>= bulky-freed (max 1 lispm-compact-clear-at-least)))
                       bulky
                     old))
           (freed (/ (apply #'+ (mapcar #'cadr chosen)) 3)))
      (when (and chosen (>= freed lispm-compact-clear-at-least))
        (let ((tokens (- (lispm--estimated-tokens agent) freed)))
          (lispm-in-session agent
            ;; The latest first: tags move what comes after them.
            (dolist (result (reverse chosen))
              (lispm-transcript-tag (car result) "cleared")))
          (setf (lispm-agent-context agent) (cons tokens (lispm--session-bytes agent)))
          freed)))))

(defun lispm--kept-turns (agent)
  "The latest turns of AGENT's session that a handover keeps, or nil.
Return (TEXT . INPUT): the text of their sections, conditions left
out, and the beginning of the input the first of them handled.  See
`lispm-compact-keep-turns'."
  (lispm-in-session agent
    (let* ((end (or (lispm-session-input-heading-p) (point-max)))
           (sections (seq-filter (lambda (section) (< (plist-get section :start) end))
                                 (lispm-transcript-sections)))
           (inputs (seq-filter (lambda (section) (memq (plist-get section :kind) '(user message event)))
                               sections))
           ;; A quarter of the threshold, in bytes; with none, of its default.
           (limit (* 3 (/ (or lispm-compact-threshold
                              lispm-session-default-compact-threshold)
                          4))))
      ;; The turn that saved memory for this handover goes with the rest.
      (when-let* ((last (car (last inputs)))
                  ((eq (plist-get last :kind) 'event))
                  ((equal (plist-get last :arg) "compaction")))
        (setq end (plist-get last :start)
              inputs (butlast inputs)))
      (cl-loop for n from (min lispm-compact-keep-turns (1- (length inputs))) downto 1
               for first = (nth (- (length inputs) n) inputs)
               for text = (mapconcat
                           (lambda (section)
                             (if (or (< (plist-get section :start) (plist-get first :start))
                                     (>= (plist-get section :start) end)
                                     (eq (plist-get section :kind) 'condition))
                                 ""
                               (buffer-substring-no-properties (plist-get section :start)
                                                               (min (plist-get section :end) end))))
                           sections "")
               when (<= (string-bytes text) limit)
               return (cons (if (string-suffix-p "\n" text) text (concat text "\n"))
                            (lispm-oneline (lispm-transcript-section-text first) 60))))))

(defun lispm--has-history-p (agent)
  "Non-nil if AGENT's session holds a response: something to hand over."
  (with-current-buffer (lispm-agent-buffer agent)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (lispm-transcript-heading-regexp 'assistant) nil t))))

(defun lispm--take-input (agent)
  "Remove the text the user typed under AGENT's last \"* User\" and return it."
  (lispm-in-session agent
    (when (re-search-backward (lispm-transcript-heading-regexp 'user) nil t)
      (let ((text (string-trim (buffer-substring-no-properties
                                (min (point-max) (1+ (line-end-position))) (point-max)))))
        (delete-region (point) (point-max))
        text))))

(defun lispm-compact (&optional agent)
  "Hand AGENT's session over to a continuation holding a summary of it.
Interactively, the agent of the current session.  Memory saved for a
handover since the last one is not saved again, and a summary being
written at idle is taken (see `lispm-compact-idle')."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm-buffer-agent))))
    (when (or (lispm-agent-busy-p agent) (lispm-agent-compacting agent))
      (user-error "%s" (lispm-tr "%s is busy" (lispm-agent-name agent))))
    (setf (lispm-agent-compacting agent) t)
    (cond
     ((lispm--adopt-idle-handover agent))
     ((and lispm-compact-save-memory (not (lispm--memory-saved-p agent)))
      (lispm-turns-start agent (list :event "compaction" :text lispm--save-memory-prompt
                                     :no-compact t
                                     :callback (lambda (_)
                                                 (when (lispm-agent-compacting agent)
                                                   (lispm--note-memory-saved agent))
                                                 (lispm--summarize agent)))))
     (t (lispm--summarize agent)))))

(defun lispm--summary-request (agent)
  "The request for a summary of AGENT's session, and the turns kept after it.
Return (REQUEST . KEPT), KEPT being what `lispm--kept-turns' returns."
  (let* ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse)))
         (kept (lispm--kept-turns agent))
         (prompt (if kept
                     (concat lispm--summary-prompt " "
                             (format lispm--summary-kept-prompt (cdr kept)))
                   lispm--summary-prompt))
         (messages (lispm-transcript-normalize
                    (append (plist-get parsed :messages)
                            (list `((role . "user")
                                    (content . [((type . "text")
                                                 (text . ,prompt))])))))))
    (cons (lispm-api-request (lispm-agent-backend* agent) (lispm-agent-model* agent)
                             (lispm-agent-effort* agent) (plist-get parsed :system)
                             nil messages)
          kept)))

(defun lispm--summarize (agent &optional resume)
  "Ask the model for a summary of AGENT's session, then continue from it.
The continuation holds the summary and the latest turns, which the
summary need not cover (see `lispm-compact-keep-turns').  RESUME
non-nil goes on with the turn in the continuation."
  (when (lispm-agent-compacting agent)
    (lispm-set-state agent 'compacting "writing a summary")
    (condition-case err
        (let* ((summary-request (lispm--summary-request agent))
               (request (car summary-request))
               (kept (cdr summary-request))
               (proc (funcall lispm-api-function (lispm-agent-backend* agent) request
                              :on-done (lambda (message)
                                         (setf (lispm-agent-process agent) nil)
                                         (lispm--continue agent (lispm-content-text
                                                                 (append (alist-get 'content message) nil))
                                                          resume (car kept)))
                              :on-error (lambda (err)
                                          (setf (lispm-agent-process agent) nil)
                                          (lispm--compaction-failed
                                           agent (format "%s: %s" (plist-get err :type)
                                                         (plist-get err :message))
                                           resume)))))
          (when (processp proc) (process-put proc 'lispm-agent agent))
          (setf (lispm-agent-process agent) proc))
      (error (lispm--compaction-failed agent (error-message-string err) resume)))))

(defun lispm--continue (agent summary &optional resume kept)
  "Carry AGENT's conversation on in a new session file holding SUMMARY.
KEPT, if non-nil, is the text of the latest turns, which follow it.
RESUME non-nil goes on with the turn there."
  (when (lispm-agent-compacting agent)
    (if (string-blank-p summary)
        (lispm--compaction-failed agent "the summary came back empty" resume)
      (let* ((old (lispm-agent-buffer agent))
             (old-file (buffer-file-name old))
             (file (lispm-session-create-file agent old-file summary kept)))
        (lispm-in-session agent
          (let ((input (lispm-session-input-heading-p)))
            (when input (delete-region input (point-max))))
          (lispm-session-newline)
          (insert (lispm-transcript-section
                   (lispm-transcript-title 'note)
                   (format "Continued in [[file:%s]]." (abbreviate-file-name file)))))
        (lispm-save-session agent)
        (let ((new (lispm-session-visit agent file)))
          (dolist (window (get-buffer-window-list old nil t))
            (set-window-buffer window new)
            (set-window-point window (with-current-buffer new (point-max))))
          (with-current-buffer old (setq lispm-session-agent nil))
          (kill-buffer old))
        ;; Sizes reported for the old session say nothing of this one.
        (setf (lispm-agent-context agent) nil)
        (lispm-save-session agent)
        (when (fboundp 'lispm-git-commit)
          (lispm-git-commit agent (format "%s: hand over to %s" (lispm-agent-name agent)
                                          (file-name-nondirectory file))))
        (with-demoted-errors "LispM: after a handover: %S"
          (run-hook-with-args 'lispm-handover-functions agent summary old-file))
        (setf (lispm-agent-compacting agent) nil)
        (if resume
            (progn
              ;; The turn goes on from the summary, not from an input,
              ;; after saving what should outlive the session, as a
              ;; handover between turns does before it.
              (lispm-in-session agent
                (when-let* ((input (lispm-session-input-heading-p)))
                  (delete-region input (point-max))))
              (if lispm-compact-save-memory
                  (lispm-conditions-tell-for-user agent lispm--resumed-memory-prompt)
                (lispm-turns-request agent)))
          (lispm-set-state agent 'idle)
          (lispm-turns-drain-inbox agent))))))

(defun lispm--compaction-failed (agent reason &optional resume)
  "Stop at a condition: AGENT's session could not be handed over, for REASON.
RESUME non-nil if a turn waits for the handover in its middle."
  (when (lispm-agent-compacting agent)
    (setf (lispm-agent-compacting agent) nil)
    (unless resume
      ;; Between turns: attended if the input waiting for it is the user's.
      (let ((entry (car (lispm-agent-inbox agent))))
        (setf (lispm-agent-attended agent)
              (not (or (plist-get entry :from) (plist-get entry :event))))))
    (lispm-signal agent 'compaction-failed (format "Compaction failed: %s" reason)
                  :resume resume)))

;;;; Handing over at idle
;;
;; A handover at idle costs what one when a turn starts does: a turn
;; saving memory, and a request carrying the whole session.  It is
;; tried once per session: given up, as when input comes while the
;; summary is written, it is left to the next turn's start, which then
;; takes the memory saved, or the summary on its way, as its own.

(defvar lispm--memory-saved (make-hash-table :test #'eq :weakness 'key)
  "Agents to the session buffer they saved memory in for a handover.")

(defvar lispm--idle-given-up (make-hash-table :test #'eq :weakness 'key)
  "Agents to the session buffer whose handover at idle was given up.")

(defun lispm--near-threshold-p (agent)
  "Non-nil if AGENT's session is past `lispm-compact-idle' of the threshold."
  (and lispm-compact-threshold lispm-compact-idle
       (> (lispm--estimated-tokens agent) (* lispm-compact-idle lispm-compact-threshold))))

(defun lispm--free-p (agent)
  "Non-nil if AGENT is idle with nothing to do, and nothing written to send.
Text the user is writing under the last \"* User\" heading stays theirs."
  (and (memq agent (lispm-agents))
       (eq (lispm-agent-state agent) 'idle)
       (not (lispm-agent-compacting agent))
       (not (lispm-agent-condition agent))
       (null (lispm-agent-inbox agent))
       (buffer-live-p (lispm-agent-buffer agent))
       (lispm-in-session agent (lispm-session-input-heading-p))))

(defun lispm--note-memory-saved (agent)
  "Record that AGENT saved memory for the handover of its session."
  (puthash agent (lispm-agent-buffer agent) lispm--memory-saved))

(defun lispm--memory-saved-p (agent)
  "Non-nil if AGENT saved memory for a handover since the last handover.
A handover makes a new session buffer."
  (eq (gethash agent lispm--memory-saved) (lispm-agent-buffer agent)))

(defun lispm--idle-handover-p (agent)
  "Non-nil if AGENT's session may still be handed over at idle.
It may not once that was given up, until the session is handed over."
  (not (eq (gethash agent lispm--idle-given-up) (lispm-agent-buffer agent))))

(defun lispm--compact-when-idle (agent _reply)
  "Once AGENT's turn has ended, compact its session at idle if it is large.
See `lispm-compact-idle'."
  (when (and (lispm--near-threshold-p agent)
             (lispm--idle-handover-p agent)
             (not (gethash agent lispm-session-idle-handovers)))
    (lispm-when-idle (cons 'compact (lispm-agent-id agent)) #'lispm--compact-idle agent)))

(add-hook 'lispm-turn-end-functions #'lispm--compact-when-idle)

(defun lispm--compact-idle (agent)
  "Make room in AGENT's session, Emacs being idle, if it is still large.
Older tool output is cleared; if that is not enough, memory is saved
and a summary written in the background (see `lispm-compact-idle')."
  (when (and (lispm--free-p agent)
             (lispm--idle-handover-p agent)
             (lispm--near-threshold-p agent))
    (lispm--clear-results agent)
    (when (and (lispm--near-threshold-p agent) (lispm--has-history-p agent))
      (if (and lispm-compact-save-memory (not (lispm--memory-saved-p agent)))
          (progn
            (puthash agent 'saving lispm-session-idle-handovers)
            (lispm-turns-start
             agent (list :event "compaction" :text lispm--save-memory-prompt
                         :no-compact t :idle t
                         :callback (lambda (_)
                                     ;; Unless the turn was stopped.
                                     (when (eq (gethash agent lispm-session-idle-handovers) 'saving)
                                       (remhash agent lispm-session-idle-handovers)
                                       (lispm--note-memory-saved agent)
                                       (if (lispm--free-p agent)
                                           (lispm--summarize-idle agent)
                                         ;; Input came meanwhile: it goes first.
                                         (lispm--give-up-idle-handover agent)))))))
        (lispm--summarize-idle agent)))))

(defun lispm--summarize-idle (agent)
  "Have a summary of AGENT's session written while the agent stays free.
When it comes, the session is handed over to a continuation, unless
it changed meanwhile or the agent is busy: then the handover at idle
is given up.  A handover the next turn waits for takes it (see
`lispm--adopt-idle-handover')."
  (when (and (lispm--free-p agent) (not (gethash agent lispm-session-idle-handovers)))
    (condition-case err
        (pcase-let* ((`(,request . ,kept) (lispm--summary-request agent))
                     (buffer (lispm-agent-buffer agent))
                     ;; (BUFFER TICK PROCESS ADOPTED), see `lispm-session-idle-handovers'.
                     (token (list buffer (buffer-chars-modified-tick buffer) nil nil))
                     (current (lambda () (eq (gethash agent lispm-session-idle-handovers) token))))
          (puthash agent token lispm-session-idle-handovers)
          (let ((proc (funcall
                       lispm-api-function (lispm-agent-backend* agent) request
                       :on-done
                       (lambda (message)
                         (when (funcall current)
                           (remhash agent lispm-session-idle-handovers)
                           (let ((summary (lispm-content-text (append (alist-get 'content message) nil))))
                             (cond
                              ((nth 3 token)
                               (if (string-blank-p summary)
                                   (lispm--compaction-failed agent "the summary came back empty")
                                 (lispm--continue agent summary nil (car kept))))
                              ((and (not (string-blank-p summary))
                                    (lispm--free-p agent)
                                    (eq (lispm-agent-buffer agent) buffer)
                                    (= (buffer-chars-modified-tick buffer) (nth 1 token)))
                               (setf (lispm-agent-compacting agent) t)
                               (condition-case err
                                   (lispm--continue agent summary nil (car kept))
                                 (error
                                  (setf (lispm-agent-compacting agent) nil)
                                  (lispm-set-state agent 'idle)
                                  (lispm--give-up-idle-handover agent)
                                  (message "%s" (lispm-tr "LispM: could not compact %s: %s"
                                                          (lispm-agent-name agent)
                                                          (error-message-string err))))))
                              (t (lispm--give-up-idle-handover agent))))))
                       :on-error
                       (lambda (err)
                         (when (funcall current)
                           (remhash agent lispm-session-idle-handovers)
                           (if (nth 3 token)
                               (lispm--compaction-failed
                                agent (format "%s: %s" (plist-get err :type) (plist-get err :message)))
                             (lispm--give-up-idle-handover agent)))))))
            (when (funcall current)
              (when (processp proc)
                (process-put proc 'lispm-agent agent))
              (setf (nth 2 token) proc))))
      (error (lispm--give-up-idle-handover agent)
             (message "%s" (lispm-tr "LispM: could not compact %s: %s"
                                     (lispm-agent-name agent) (error-message-string err)))))))

(defun lispm--adopt-idle-handover (agent)
  "Have the handover AGENT now waits for take the summary written at idle.
Return non-nil if one is being written: the handover goes on when it
comes, as one a turn waits for."
  (when-let* ((token (gethash agent lispm-session-idle-handovers))
              ((consp token)))
    (setf (nth 3 token) t)
    (lispm-set-state agent 'compacting "writing a summary")
    t))

(defun lispm--cancel-idle-handover (agent)
  "Stop handing AGENT's session over at idle, and the summary being written."
  (when-let* ((token (gethash agent lispm-session-idle-handovers)))
    (remhash agent lispm-session-idle-handovers)
    (when-let* ((proc (and (consp token) (nth 2 token))))
      (cond ((functionp proc)
             (lispm-exec-delete proc))
            ((process-live-p proc)
             (process-put proc 'lispm-aborted t)
             (delete-process proc))))))

(defun lispm--give-up-idle-handover (agent)
  "Give up handing AGENT's session over at idle, until it is handed over.
That is left to the start of a turn, past `lispm-compact-threshold'."
  (lispm--cancel-idle-handover agent)
  (when (buffer-live-p (lispm-agent-buffer agent))
    (puthash agent (lispm-agent-buffer agent) lispm--idle-given-up)))

(defun lispm--forget-idle-work (agent)
  "Drop the background work waiting for AGENT, whose session ends."
  (when (lispm-agent-p agent)
    (lispm-cancel-idle (cons 'compact (lispm-principal-id agent)))
    (lispm--cancel-idle-handover agent)))

(add-hook 'lispm-session-end-functions #'lispm--forget-idle-work)

;;;; Journal: audit and rollback
;;
;; A turn's changes are known two ways (see lispm-journal.el): the
;; text buffers had before a tool first changed them, in memory, and,
;; for a project in git, snapshots of its work tree as the turn began
;; and ended, which hold what shell commands and Lisp changed too and
;; outlive Emacs.  Files are taken from the snapshots where there are
;; any, buffers that visit no file, and edits not saved, from memory.

(defun lispm--journal-turns (agent)
  "The turns of AGENT with changes on record, newest first, as plists.
Each has :label, :journal, the snapshots of buffers taken in memory,
and :record, the turn's snapshots in git (`lispm-journal-records'),
either of which may be nil, and :turn, its number, for a turn of this
Emacs session."
  (let* ((records (lispm-journal-records agent))
         (numbers (sort (delete-dups
                         (append (mapcar #'car (seq-filter #'cdr (lispm-agent-journal agent)))
                                 (delq nil (mapcar (lambda (r) (plist-get r :turn)) records))))
                        #'>)))
    (append
     (mapcar (lambda (turn)
               (list :turn turn :label (format "turn %d" turn)
                     :journal (cdr (assq turn (lispm-agent-journal agent)))
                     :record (seq-find (lambda (r) (eql (plist-get r :turn) turn)) records)))
             numbers)
     (mapcar (lambda (record)
               (list :record record
                     :label (format "%s, %s" (plist-get record :subject)
                                    (replace-regexp-in-string
                                     "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\).*"
                                     "\\1-\\2-\\3 \\4:\\5" (plist-get record :time)))))
             (seq-remove (lambda (r) (plist-get r :turn)) records)))))

(defun lispm--turn-label (turn)
  "TURN, one of `lispm--journal-turns', as the user is shown it: its
:label, said in the language spoken."
  (if-let* ((number (plist-get turn :turn)))
      (lispm-tr "turn %d" number)
    (plist-get turn :label)))

(defun lispm--journal-turn (agent &optional turn)
  "AGENT's TURN as `lispm--journal-turns' describes it.
TURN is a turn's number in this Emacs session, one of those plists, or
nil for the latest turn with changes."
  (cond ((consp turn) turn)
        ((seq-find (lambda (entry) (or (null turn) (eql (plist-get entry :turn) turn)))
                   (lispm--journal-turns agent)))
        (t (user-error "%s" (if turn
                                (lispm-tr "%s made no edits in turn %d" (lispm-agent-name agent) turn)
                              (lispm-tr "%s made no edits" (lispm-agent-name agent)))))))

(defun lispm--snapshot-now (snapshot)
  "Current text of the buffer or file in SNAPSHOT.
A file's is its buffer's, unless that is out of date with it (see
`lispm-file-text')."
  (let ((buffer (car snapshot))
        (file (plist-get (cdr snapshot) :file)))
    (cond (file (lispm-file-text file))
          ((buffer-live-p buffer) (lispm-tools-buffer-text buffer))
          (t ""))))

(defun lispm--journal-parts (turn)
  "What TURN changed, as (FILES . SNAPSHOTS).
FILES are the (STATUS . FILE) its snapshots in git show, SNAPSHOTS the
buffers it changed whose text, to revert to, is in memory: those that
visit no file, or a file the snapshots do not show changed."
  (let* ((files (and (plist-get turn :record) (lispm-journal-changes (plist-get turn :record))))
         (snapshots (seq-remove (lambda (s)
                                  (when-let* ((file (plist-get (cdr s) :file)))
                                    (assoc file (mapcar (lambda (c) (cons (cdr c) c)) files))))
                                (plist-get turn :journal))))
    (cons files snapshots)))

(defun lispm--read-turn (agent)
  "With a prefix argument, read one of AGENT's turns with changes."
  (when current-prefix-arg
    (let* ((turns (lispm--journal-turns agent))
           (labels (mapcar (lambda (turn) (cons (lispm--turn-label turn) turn)) turns)))
      (unless turns (user-error "%s" (lispm-tr "%s made no edits" (lispm-agent-name agent))))
      (cdr (assoc (completing-read (lispm-tr "Turn: ") labels nil t nil nil (caar labels)) labels)))))

(defun lispm-turn-diff (agent &optional turn)
  "Show every change AGENT made in TURN (default: its latest edits).
With a prefix argument, choose the turn."
  (interactive (let ((agent (lispm-buffer-agent))) (list agent (lispm--read-turn agent))))
  (let* ((turn (lispm--journal-turn agent turn))
         (parts (lispm--journal-parts turn))
         ;; Where the diff's file names are relative to.
         (directory (or (plist-get (plist-get turn :record) :top) (lispm-agent-root agent)))
         (text (let ((default-directory directory))
                 (concat (lispm-tr "Changes by %s in %s" (lispm-agent-name agent) (lispm--turn-label turn)) "\n\n"
                         (if (car parts) (concat (lispm-journal-diff (plist-get turn :record)) "\n") "")
                         (mapconcat (lambda (s)
                                      (concat (lispm-diff (plist-get (cdr s) :text) (lispm--snapshot-now s)
                                                           (or (plist-get (cdr s) :file)
                                                               (plist-get (cdr s) :name)))
                                              "\n"))
                                    (cdr parts) "")))))
    (with-current-buffer (get-buffer-create "*lispm-diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (diff-mode)
      (setq default-directory directory)
      (goto-char (point-min)))
    (display-buffer "*lispm-diff*")))

(defun lispm-revert-turn (agent &optional turn)
  "Undo every change AGENT made to buffers and files in TURN.
Default: its latest turn with edits; with a prefix argument, choose
the turn.  In a project in git, that is every change its files went
through in the turn, made by tools, shell commands or Lisp, since the
turn's snapshots show them, even from before Emacs restarted."
  (interactive (let ((agent (lispm-buffer-agent))) (list agent (lispm--read-turn agent))))
  (let* ((turn (lispm--journal-turn agent turn))
         (parts (lispm--journal-parts turn))
         (record (plist-get turn :record))
         (names (append (mapcar (lambda (c) (lispm-tools-label (cdr c))) (car parts))
                        (mapcar (lambda (s) (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                                (cdr parts)))))
    ;; All or nothing: a buffer with unsaved changes whose file changed
    ;; on disk too is for the user to settle first.
    (when-let* ((conflicts (seq-filter (lambda (buffer)
                                         (memq (lispm-tools-sync buffer) '(conflict held)))
                                       (delq nil (append (mapcar (lambda (c) (find-buffer-visiting (cdr c)))
                                                                 (car parts))
                                                         (seq-filter #'buffer-live-p
                                                                     (mapcar #'car (cdr parts))))))))
      (user-error "%s" (lispm-tr "Nothing reverted: %s changed on disk, and in Emacs too; save or revert first"
                                 (mapconcat #'buffer-name conflicts ", "))))
    (unless (or (not (called-interactively-p 'any))
                (yes-or-no-p (lispm-tr-n "Revert %d file or buffer %s changed in %s? "
                                         "Revert %d files and buffers %s changed in %s? "
                                         (length names)
                                         (length names) (lispm-agent-name agent) (lispm--turn-label turn))))
      (user-error "%s" (lispm-tr "Cancelled")))
    (let ((lispm-current-agent nil))
      (pcase-dolist (`(,status . ,file) (car parts))
        (let ((buffer (find-buffer-visiting file)))
          (if (equal status "A")
              (progn
                (when buffer
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (kill-buffer buffer))
                (when (and (file-exists-p file)
                           (or (not (called-interactively-p 'any))
                               (yes-or-no-p (lispm-tr "Delete %s, which %s created? "
                                                      file (lispm-agent-name agent)))))
                  (delete-file file)))
            (if buffer
                (lispm-tools-set-text buffer (lispm-journal-text-before record file))
              (lispm-journal-restore record file)))))
      (dolist (s (cdr parts))
        (let* ((buffer (car s))
               (props (cdr s))
               (file (plist-get props :file)))
          (cond
           ((and file (not (plist-get props :existed)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))
            (when (and (file-exists-p file)
                       (or (not (called-interactively-p 'any))
                           (yes-or-no-p (lispm-tr "Delete %s, which %s created? "
                                                  file (lispm-agent-name agent)))))
              (delete-file file)))
           ((buffer-live-p buffer) (lispm-tools-set-text buffer (plist-get props :text)))
           (file (let ((coding-system-for-write 'utf-8))
                   (write-region (plist-get props :text) nil file nil 'silent)))))))
    (when-let* ((number (plist-get turn :turn)))
      (setf (lispm-agent-journal agent) (assq-delete-all number (lispm-agent-journal agent))))
    (when record
      (lispm-journal-forget record))
    (lispm-session-record-event
     agent "revert"
     (format "The user reverted the changes you made in %s to: %s."
             (plist-get turn :label) (string-join names ", ")))
    (message "%s" (lispm-tr "Reverted %s of %s" (lispm--turn-label turn) (lispm-agent-name agent)))))


(defun lispm-runtime--before-turn (agent entry)
  "Compact or clear old results before AGENT starts a turn on ENTRY, if needed."
  (when (and lispm-compact-threshold
             (not (plist-get entry :no-compact))
             (> (lispm--estimated-tokens agent) lispm-compact-threshold)
             (not (and (lispm--clear-results agent)
                       (<= (lispm--estimated-tokens agent) lispm-compact-threshold)))
             (lispm--has-history-p agent))
    (push (or entry (list :id (lispm-inbox-new-id) :text (lispm--take-input agent)))
          (lispm-agent-inbox agent))
    (lispm-emit-change agent '(mailbox))
    (lispm-inbox-save agent)
    (lispm-compact agent)
    t))

(defun lispm-runtime--trim-request (agent)
  "Clear old bulky results for AGENT when over `lispm-compact-threshold'."
  (when (and lispm-compact-threshold
             (> (lispm--estimated-tokens agent) lispm-compact-threshold))
    (lispm--clear-results agent)))

(setq lispm-runtime-before-turn-function #'lispm-runtime--before-turn
      lispm-runtime-trim-request-function #'lispm-runtime--trim-request
      lispm-runtime-session-bytes-function #'lispm--session-bytes
      lispm-runtime-give-up-idle-handover-function #'lispm--give-up-idle-handover
      lispm-runtime-take-input-function #'lispm--take-input
      lispm-runtime-summarize-function #'lispm--summarize)

(provide 'lispm-runtime)

;;; lispm-runtime.el ends here
