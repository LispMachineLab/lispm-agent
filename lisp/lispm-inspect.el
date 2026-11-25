;;; lispm-inspect.el --- Presentations and the agent inspector  -*- lexical-binding: t; -*-

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

;; On a Lisp Machine what the screen showed was the objects themselves:
;; Genera's presentations made a pathname, an instance or a symbol on
;; the screen the object it stood for, with commands by its type.  Text
;; LispM shows does the same.  A presentation is a stretch of text whose
;; `lispm-object' property is the object it shows, one of
;;
;;   (file FILE LINE)   a file, and a line in it or nil
;;   (buffer NAME)      a buffer
;;   (agent NAME)       an agent
;;   (symbol SYMBOL)    a function, variable or face
;;   (tool NAME)        a tool
;;   (call AGENT ID)    a tool call, in the session of AGENT
;;
;; and RET or a click acts on it as its type says: visit the file at the
;; line, show the buffer, inspect the agent, describe the symbol, go to
;; the tool's definition, show the call in the transcript.  The chat
;; presents what it draws: the arguments of calls, the file:line places
;; and org links in replies and results, the code in them that names a
;; symbol, the agents named.  Code naming what this Emacs lacks, a
;; symbol nothing defines or a file that is not there, is marked
;; instead; see lispm-ground.el.
;;
;; The inspector is a buffer showing one agent whole, as Genera's
;; inspector showed an object: its identity, state, model, policy and
;; ceiling, the calls it has pending, its inbox, whom it waits on and
;; who waits on it, its journal, its tools and triggers, all of them
;; presentations.  Agents have it too, as the tool inspect_agent.

;;; Code:

(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-presentations)

(declare-function lispm-agents-work-input-summary "lispm-agents-sessions" (input))

;;;; The inspector

(defvar-local lispm-inspect-agent nil
  "The agent the inspector in this buffer shows.")

(defvar-local lispm-inspect-viewspec 'full
  "The viewspec controlling the level of detail in this inspector buffer.
One of `full' (all sections), `compact' (identity, state, calls, inbox,
delegation) or `summary' (identity, state and pending calls only).")

(defun lispm-inspect--line (label value)
  "A line of the inspector: LABEL, then VALUE."
  (concat (propertize (truncate-string-to-width label 12 nil ?\s) 'face 'shadow) " " value))

(defun lispm-inspect--agent-name (agent)
  "AGENT's name, presented."
  (lispm-present (symbol-name (lispm-agent-name agent))
                 (list 'agent (symbol-name (lispm-agent-name agent)))))

(defun lispm-inspect--policy (policy)
  "POLICY, an alist of classes and actions, on one line."
  (mapconcat (pcase-lambda (`(,class . ,action)) (format "%s %s" class action)) policy ", "))

(defun lispm-inspect--entry (entry)
  "An inbox ENTRY, on one line."
  (concat (cond ((plist-get entry :from) (lispm-tr "[from %s] " (plist-get entry :from)))
                ((plist-get entry :event) (format "[%s] " (plist-get entry :event)))
                (t ""))
          (lispm-oneline (or (plist-get entry :text) "") 70)))

(defun lispm-inspect-data (agent)
  "The sources the inspector shares with data readers, for AGENT.
Return a plist with :pending, an alist of call id to call in transcript
order; :entry and :inbox, the input being handled and queued inputs;
:waiting-on and :waited-on-by, agents on either side of a delegation;
and :journal, the turn to snapshot alist.  These are the agent's own
objects: readers must not change them.  No transcript or diary is read."
  (let (pending)
    (maphash (lambda (id call) (push (cons id call) pending))
             (lispm-agent-pending agent))
    (list :pending (sort pending (lambda (a b)
                                  (< (or (let ((m (plist-get (cdr a) :marker))) (and (markerp m) (marker-position m))) 0)
                                     (or (let ((m (plist-get (cdr b) :marker))) (and (markerp m) (marker-position m))) 0))))
          :entry (lispm-agent-entry agent)
          :inbox (lispm-agent-inbox agent)
          :waiting-on (delete-dups (copy-sequence (lispm-agent-waiting-on agent)))
          :waited-on-by (seq-filter (lambda (other) (memq agent (lispm-agent-waiting-on other)))
                                    (lispm-agents))
          :journal (lispm-agent-journal agent))))

(defun lispm-inspect--section (title &rest lines)
  "Return inspector section (TITLE . NON-NIL-LINES), or nil when empty."
  (when-let* ((kept (delq nil lines)))
    (cons title kept)))

(defun lispm-inspect--model-summary (agent)
  "Return AGENT's model summary line, or \"no backend\" when none is configured."
  (if-let* ((backend (lispm-backend-name agent))
            ((lispm-backend backend)))
      (lispm-tr "%s via %s, effort %s"
                (lispm-agent-model* agent)
                backend
                (lispm-agent-effort* agent))
    (lispm-tr "no backend")))

(defun lispm-inspect--pending-line (agent item)
  "Format pending call ITEM `(ID . CALL)' of AGENT for the inspector."
  (pcase-let* ((`(,id . ,call) item)
               (asked (lispm-asked call))
               (summary (lispm-transcript-summary (plist-get asked :input))))
    (concat (lispm-present (plist-get call :name)
                           (list 'call (symbol-name (lispm-agent-name agent)) id))
            (if summary (concat " " summary) "")
            (propertize (format "  %s%s" (plist-get call :status)
                                (if (plist-get call :asking)
                                    (concat ", " (lispm-tr "asks for %s" (plist-get asked :name)))
                                  ""))
                        'face 'shadow))))

(defun lispm-inspect--journal-line (entry)
  "Format journal ENTRY `(TURN . SNAPSHOTS)' for the inspector."
  (pcase-let ((`(,turn . ,snapshots) entry))
    (lispm-inspect--line
     (lispm-tr "turn %d" turn)
     (mapconcat (lambda (s)
                  (let ((file (plist-get (cdr s) :file)))
                    (if file
                        (lispm-present (lispm-tools-label file) (list 'file file nil))
                      (lispm-present (plist-get (cdr s) :name)
                                     (list 'buffer (plist-get (cdr s) :name))))))
                snapshots ", "))))

(defun lispm-inspect--agent-section (agent summaryp)
  "Return the \"Agent\" identity section for AGENT; SUMMARYP omits detail lines."
  (let* ((buffer (lispm-agent-buffer agent))
         (file (and (buffer-live-p buffer) (buffer-file-name buffer))))
    (lispm-inspect--section
     (lispm-tr "Agent")
     (lispm-inspect--line (lispm-tr "name") (lispm-inspect--agent-name agent))
     (lispm-inspect--line (lispm-tr "role")
                          (format "%s%s" (lispm-agent-role agent)
                                  (if (lispm-agent-standing agent)
                                      (concat ", " (lispm-tr "standing"))
                                    "")))
     (lispm-inspect--line (lispm-tr "project")
                          (lispm-present (abbreviate-file-name (lispm-agent-root agent))
                                         (list 'file (lispm-agent-root agent) nil)))
     (and (not summaryp)
          (lispm-inspect--line (lispm-tr "session") (lispm-agent-id agent)))
     (and (not summaryp) file
          (lispm-inspect--line (lispm-tr "file")
                               (lispm-present (abbreviate-file-name file)
                                              (list 'file file nil))))
     (and (not summaryp) (lispm-agent-description agent)
          (lispm-inspect--line (lispm-tr "description") (lispm-agent-description agent))))))

(defun lispm-inspect--state-section (agent summaryp)
  "Return the \"State\" runtime section for AGENT; SUMMARYP omits detail lines."
  (let ((condition (lispm-agent-condition agent))
        (usage (lispm-agent-usage agent)))
    (lispm-inspect--section
     (lispm-tr "State")
     (lispm-inspect--line (lispm-tr "state")
                          (format "%s%s" (lispm-state-name (lispm-agent-state agent))
                                  (if (lispm-agent-status agent)
                                      (concat ": " (lispm-agent-status agent))
                                    "")))
     (lispm-inspect--line (lispm-tr "turn")
                          (lispm-tr-n "%d, %d request" "%d, %d requests"
                                      (lispm-agent-steps agent)
                                      (lispm-agent-turn agent)
                                      (lispm-agent-steps agent)))
     (and condition
          (lispm-inspect--line (lispm-tr "condition")
                               (lispm-tr "%s: %s (restarts: %s)"
                                         (lispm-tr (plist-get condition :title))
                                         (lispm-oneline (plist-get condition :message) 70)
                                         (mapconcat #'symbol-name
                                                    (plist-get condition :restarts) ", "))))
     (and (not summaryp)
          (lispm-inspect--line (lispm-tr "model") (lispm-inspect--model-summary agent)))
     (and (not summaryp)
          (lispm-inspect--line (lispm-tr "usage")
                               (lispm-tr "%d in, %d out, %d from cache"
                                         (nth 0 usage) (nth 1 usage) (nth 2 usage)))))))

(defun lispm-inspect-sections (agent &optional viewspec)
  "What the inspector shows of AGENT: a list of (TITLE . LINES).
VIEWSPEC controls the level of detail (`full', `compact' or `summary';
by default `lispm-inspect-viewspec' or `full').
LINES are strings, whose objects are presented."
  (let* ((viewspec (or viewspec (bound-and-true-p lispm-inspect-viewspec) 'full))
         (summaryp (eq viewspec 'summary))
         (fullp (eq viewspec 'full))
         (data (lispm-inspect-data agent))
         (pending (plist-get data :pending))
         (entry (plist-get data :entry))
         (inbox (plist-get data :inbox))
         (waiting-on (plist-get data :waiting-on))
         (waited-by (plist-get data :waited-on-by))
         (journal (plist-get data :journal)))
    (delq nil
          (list
           (lispm-inspect--agent-section agent summaryp)
           (lispm-inspect--state-section agent summaryp)
           (and fullp
                (lispm-inspect--section
                 (lispm-tr "Policy")
                 (lispm-inspect--line (lispm-tr "policy") (lispm-inspect--policy (lispm-policy agent)))
                 (and (lispm-agent-ceiling agent)
                      (lispm-inspect--line (lispm-tr "ceiling") (lispm-inspect--policy (lispm-agent-ceiling agent))))
                 (and (lispm-agent-allowed agent)
                      (lispm-inspect--line (lispm-tr "allowed")
                                           (mapconcat (lambda (a)
                                                        (if (consp a) (lispm-tr "%s (that call)" (car a)) a))
                                                      (lispm-agent-allowed agent) ", ")))))
           (and pending
                (cons (lispm-tr "Pending calls")
                      (mapcar (apply-partially #'lispm-inspect--pending-line agent) pending)))
           (and (not summaryp) (or entry inbox)
                (apply #'lispm-inspect--section (lispm-tr "Inbox")
                       (and entry (lispm-inspect--line (lispm-tr "handling") (lispm-inspect--entry entry)))
                       (mapcar (lambda (item) (lispm-inspect--line (lispm-tr "queued") (lispm-inspect--entry item)))
                               inbox)))
           (and (not summaryp) (or waiting-on waited-by)
                (lispm-inspect--section
                 (lispm-tr "Delegation")
                 (and waiting-on
                      (lispm-inspect--line (lispm-tr "waits on")
                                           (mapconcat #'lispm-inspect--agent-name waiting-on ", ")))
                 (and waited-by
                      (lispm-inspect--line (lispm-tr "waited on by")
                                           (mapconcat #'lispm-inspect--agent-name waited-by ", ")))))
           (and fullp (seq-some #'cdr journal)
                (cons (lispm-tr "Journal")
                      (mapcar #'lispm-inspect--journal-line (seq-filter #'cdr journal))))
           (and fullp (or (lispm-agent-loaded-tools agent) (lispm-agent-triggers agent))
                (lispm-inspect--section
                 (lispm-tr "Extensions")
                 (and (lispm-agent-loaded-tools agent)
                      (lispm-inspect--line (lispm-tr "tools loaded")
                                           (mapconcat (lambda (name) (lispm-present name (list 'tool name)))
                                                      (lispm-agent-loaded-tools agent) ", ")))
                 (and (lispm-agent-triggers agent)
                      (lispm-inspect--line (lispm-tr "triggers")
                                           (mapconcat (lambda (tr) (plist-get tr :name))
                                                      (lispm-agent-triggers agent) ", ")))))))))

(defun lispm-inspect-copy-data (data)
  "Copy plain DATA, including the strings in its lists and vectors.
Buffers, numbers and symbols keep their identities.  Sources shared
with the runtime can then be returned without exposing mutable text."
  (cond ((stringp data) (substring-no-properties data))
        ((consp data) (cons (lispm-inspect-copy-data (car data))
                           (lispm-inspect-copy-data (cdr data))))
        ((vectorp data) (apply #'vector (mapcar #'lispm-inspect-copy-data data)))
        (t data)))

(defalias 'lispm-inspect--copy-data #'lispm-inspect-copy-data)

(defconst lispm-inspect--recent-call-limit 20
  "Maximum number of completed calls a roster inspector returns.")

(defun lispm-inspect-call-data (id call)
  "The public work record of the pending CALL with ID.
The summary is of the input it awaits approval for, when it asks on
another call's behalf, as in `lispm-inspect-sections'."
  (list :id id :name (plist-get call :name)
        :input-summary (lispm-agents-work-input-summary (plist-get (lispm-asked call) :input))
        :status (plist-get call :status)))

(defun lispm-inspect-recent-calls (agent)
  "AGENT's latest completed calls of this turn, in transcript order.
Read only the current turn's transcript, when an inspector asks for
it, and return at most `lispm-inspect--recent-call-limit' records.
The runtime marks the turn's start: later events and an unsent draft
are not new turns.  Nil when that exact boundary is unknown, as for
a turn recovered from a session written without it."
  (when-let* (((> (lispm-agent-turn agent) 0))
              (buffer (lispm-agent-buffer agent))
              ((buffer-live-p buffer))
              (turn-start (lispm-turn-start buffer))
              ((eql (car turn-start) (lispm-agent-turn agent)))
              (marker (cdr turn-start))
              ((and (markerp marker) (eq (marker-buffer marker) buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let (calls)
            (dolist (section (lispm-transcript-sections marker))
              (when (and (eq (plist-get section :kind) 'assistant)
                         (not (plist-get section :ignore)))
                (dolist (part (lispm-transcript-parts section))
                  (when (and (eq (plist-get part :kind) 'tool)
                             (not (gethash (plist-get part :id) (lispm-agent-pending agent))))
                    (push (list :id (plist-get part :id) :name (plist-get part :name)
                                :input-summary (lispm-agents-work-input-summary (plist-get part :input))
                                :status (pcase (plist-get part :state)
                                          ("DONE" 'done) ("FAILED" 'failed) ("DENIED" 'denied)))
                          calls)))))
            (nreverse (seq-take calls lispm-inspect--recent-call-limit))))))))

(defun lispm-inspect--mailbox-entry (entry stored state)
  "The public fields of mailbox ENTRY, supplemented by STORED, in STATE.
STORED is its persisted inbox record, when known.  Runtime callbacks
and transcript positions never form part of the returned record."
  (when entry
    (let ((record (list :state state)))
      (dolist (field '(:id :agent :session :from :to :event :reply-to :request-id :queued :text))
        (setq record (plist-put record field (or (plist-get entry field) (plist-get stored field)))))
      record)))

(defun lispm-inspect-mailbox (agent data)
  "The handling and queued entries of AGENT from its inspector DATA.
Read the project's inbox only here, to fill metadata unknown in
memory.  A running entry's text is held in memory, not in that file."
  (let* ((entry (plist-get data :entry))
         (inbox (plist-get data :inbox))
         (stored (and (or entry inbox)
                      (condition-case nil
                          (lispm-inbox-entries (lispm-agent-root agent))
                        (file-error nil)))))
    (cl-flet ((record (entry state)
                 (lispm-inspect--mailbox-entry
                  entry (seq-find (lambda (other)
                                    (equal (plist-get entry :id) (plist-get other :id)))
                                  stored)
                  state)))
      (list :handling (record entry 'running)
            :queued (mapcar (lambda (entry) (record entry 'queued)) inbox)))))

(defun lispm-inspect-text (agent &optional viewspec)
  "What the inspector shows of AGENT, as plain text, in English: what an
agent reads of it.  VIEWSPEC is `full' (default), `compact' or `summary'."
  (lispm-in-english
    (mapconcat (pcase-lambda (`(,title . ,lines))
                 (concat "# " title "\n" (mapconcat #'substring-no-properties lines "\n")))
               (lispm-inspect-sections agent viewspec) "\n\n")))

(defun lispm-inspect--draw ()
  "Draw the inspector in the current buffer again."
  (let ((agent lispm-inspect-agent)
        (inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (if (not (memq agent (lispm-agents)))
        (insert (lispm-tr "%s has ended." (lispm-agent-name agent)) "\n")
      (insert (mapconcat (pcase-lambda (`(,title . ,lines))
                           (concat (propertize title 'face 'bold) "\n"
                                   (mapconcat (lambda (l) (concat "  " l)) lines "\n")
                                   "\n"))
                         (lispm-inspect-sections agent) "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar lispm-inspect--timers nil
  "Inspector buffers to draw again shortly, with their timers.")

(defun lispm-inspect--changed (agent)
  "Draw again, shortly, the inspectors showing AGENT."
  (dolist (buffer (buffer-list))
    (when (and (eq (buffer-local-value 'lispm-inspect-agent buffer) agent)
               (get-buffer-window buffer t)
               (not (assq buffer lispm-inspect--timers)))
      (push (cons buffer (run-at-time 0.2 nil
                                      (lambda ()
                                        (setq lispm-inspect--timers (assq-delete-all buffer lispm-inspect--timers))
                                        (when (buffer-live-p buffer)
                                          (with-current-buffer buffer (lispm-inspect--draw))))))
            lispm-inspect--timers))))

(add-hook 'lispm-state-change-functions #'lispm-inspect--changed)

(defun lispm-inspect--language-changed ()
  "Draw every inspector again in the language now spoken."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'lispm-inspect-agent buffer)
      (with-current-buffer buffer (lispm-inspect--draw)))))

(add-hook 'lispm-language-changed-hook #'lispm-inspect--language-changed)

(defun lispm-inspect-refresh ()
  "Draw the inspector again."
  (interactive)
  (lispm-inspect--draw))

(defun lispm-inspect-chat ()
  "Show the inspected agent's conversation."
  (interactive)
  (if lispm-show-agent-function
      (funcall lispm-show-agent-function lispm-inspect-agent)
    (user-error "Chat display is not available")))

(defun lispm-inspect-transcript ()
  "Show the inspected agent's org transcript."
  (interactive)
  (pop-to-buffer (or (and (buffer-live-p (lispm-agent-buffer lispm-inspect-agent))
                          (lispm-agent-buffer lispm-inspect-agent))
                     (user-error "%s" (lispm-tr "%s's session is closed" (lispm-agent-name lispm-inspect-agent))))))

(defun lispm-inspect-abort ()
  "Stop the inspected agent's turn."
  (interactive)
  (lispm-abort lispm-inspect-agent))

(defun lispm-inspect-diff ()
  "Show what the inspected agent changed in its latest turn with changes."
  (interactive)
  (lispm-turn-diff lispm-inspect-agent))

(defun lispm-inspect-cycle-viewspec ()
  "Cycle the inspector's viewspec (`full' -> `compact' -> `summary' -> `full')."
  (interactive)
  (setq lispm-inspect-viewspec
        (pcase lispm-inspect-viewspec
          ('full 'compact)
          ('compact 'summary)
          (_ 'full)))
  (lispm-inspect--draw))

(defvar-keymap lispm-inspect-mode-map
  :doc "Keymap of `lispm-inspect-mode'."
  :parent special-mode-map
  "RET" #'lispm-act-at-point
  "g" #'lispm-inspect-refresh
  "v" #'lispm-inspect-cycle-viewspec
  "c" #'lispm-inspect-chat
  "t" #'lispm-inspect-transcript
  "d" #'lispm-inspect-diff
  "k" #'lispm-inspect-abort
  "TAB" #'lispm-inspect-next
  "<backtab>" #'lispm-inspect-previous)

(defun lispm-inspect-next ()
  "Move to the next object presented."
  (interactive)
  (let ((pos (point)))
    (when (lispm-object-at pos)
      (setq pos (or (next-single-property-change pos 'lispm-object) (point-max))))
    (let ((next (if (lispm-object-at pos) pos (next-single-property-change pos 'lispm-object))))
      (if next (goto-char next) (message "%s" (lispm-tr "No more objects"))))))

(defun lispm-inspect-previous ()
  "Move to the previous object presented."
  (interactive)
  (let ((pos (point)))
    (when (and (> pos (point-min)) (lispm-object-at (1- pos)) (lispm-object-at pos))
      (setq pos (or (previous-single-property-change pos 'lispm-object) (point-min))))
    (let ((previous (previous-single-property-change pos 'lispm-object)))
      (when (and previous (not (lispm-object-at previous)))
        (setq previous (previous-single-property-change previous 'lispm-object)))
      (if previous (goto-char previous) (message "%s" (lispm-tr "No more objects"))))))

(define-derived-mode lispm-inspect-mode special-mode "λ Inspect"
  "An agent, shown whole: its state, calls, inbox, delegations and journal.
RET on an object acts on it: a file is visited, an agent inspected, a
call shown in the transcript.

\\{lispm-inspect-mode-map}"
  :group 'lispm
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-inspect--draw))))

(defun lispm-inspect (agent)
  "Show the inspector of AGENT: the agent, whole, with its objects to act on.
Interactively, the agent of the current session or chat, or one read
with `lispm-accept' (click an agent presentation on screen or complete)."
  (interactive (list (or (bound-and-true-p lispm-inspect-agent)
                         lispm-session-agent (bound-and-true-p lispm-chat-agent)
                         (let ((names (mapcar (lambda (a) (symbol-name (lispm-agent-name a)))
                                              (lispm-agents))))
                           (unless names (user-error "%s" (lispm-tr "No agent is running")))
                           (lispm-get-agent (lispm-accept 'agent (lispm-tr "Inspect agent: ") names nil t))))))
  (let ((buffer (get-buffer-create (format "*lispm inspect %s*" (lispm-agent-name agent)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-inspect-mode) (lispm-inspect-mode))
      (setq lispm-inspect-agent agent
            default-directory (lispm-agent-root agent))
      (lispm-inspect--draw))
    (pop-to-buffer buffer)
    buffer))

(lispm-deftool inspect_agent
    ((agent string "The agent to inspect (see list_agents); yourself if not given." :optional t)
     (viewspec string "Level of detail to display: full (default), compact or summary."
               :optional t :enum ("full" "compact" "summary")))
  "Inspect a LispM agent's permissions, approval policy and pending requests.
Read its state, the calls it has pending and
what they wait for, its inbox, whom it waits on and who waits on it,
its policy and ceiling, remembered allowances, and files its turns changed.
Use this to diagnose repeated permission prompts or requests for full access.
This only reports LispM's policy; it does not grant access or change settings,
and does not inspect macOS privacy permissions."
  :permission comm
  :deferred t
  :keywords '("LispM permissions" "approval policy" "full access" "repeated prompts" "allowance")
  (lispm-inspect-text (if agent
                          (or (lispm-get-agent agent t)
                              (error "No agent named %s; list_agents shows them" agent))
                        lispm-current-agent)
                      (and viewspec (intern viewspec))))


(setq lispm-presentations-inspect-function #'lispm-inspect)

(provide 'lispm-inspect)

;;; lispm-inspect.el ends here
