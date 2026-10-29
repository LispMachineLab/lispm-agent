;;; lispm-inbox.el --- Inboxes that outlive Emacs  -*- lexical-binding: t; -*-

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

;; An agent's inbox holds the inputs waiting for it: messages from
;; other agents, events of hooks and timers, what the user wrote while
;; it was busy.  In memory it is a list of plists; on disk it is an org
;; file per project, projects/<project>/inbox.org under
;; `lispm-directory', which the runtime keeps in step with every agent
;; of the project, so a restart of Emacs loses none of it:
;;
;;   #+title: Inbox of ~/src/lispm/
;;   #+todo: QUEUED RUNNING | DONE
;;   * QUEUED Message from reviewer@lispm   waiting for the agent
;;   :PROPERTIES:
;;   :ENTRY:    i20261019-182000-ab12      the entry's id
;;   :AGENT:    coder@lispm                to whom, and to which session
;;   :SESSION:  20261019-120000-9f3a
;;   :FROM:     reviewer@lispm             who sent it, or :EVENT:
;;   :REPLY_TO: ...                        who waits for the reply, if anyone
;;   :QUEUED:   [2026-10-19 Mon 18:20]
;;   :END:
;;   The message.
;;   * RUNNING User                         the input the turn under way
;;   :PROPERTIES:                           handles; its text is in the
;;   ...                                    session
;;
;; An entry is QUEUED until its turn starts, RUNNING while the turn is
;; under way, and SUSPENDED when it yields for background replies.
;; ANSWER_TO identifies the reply that continues a suspended request;
;; AWAITING holds its remaining dependencies and VALUE its returned data.
;; Once the request finishes, its entry is removed.  A session opened again takes its
;; queued entries back into the agent's inbox; a RUNNING one tells that
;; Emacs stopped in the middle of that turn.  Whoever waits for the
;; reply is told by REPLY_TO, a string `lispm-inbox-callback-function'
;; turns back into the callback the entry had: delegation uses it, so
;; that a helper's reply still reaches its caller after a restart.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-tools)

(defvar lispm-inhibit-triggers)

(defvar lispm-inbox-callback-function nil
  "Function turning a persisted entry's reply-to back into its callback.
Called with the agent the entry is for and the entry's :reply-to
string; returns a function of the agent's final reply, or nil.  An
entry taken back after a restart of Emacs gets that callback, its own
having been lost with Emacs.")

(defun lispm-inbox-file (root)
  "The inbox file of the project at ROOT."
  (expand-file-name "inbox.org" (lispm-project-directory root)))

(defun lispm-inbox-new-id ()
  "A fresh id for an inbox entry."
  (concat "i" (lispm--new-id)))

(defun lispm-inbox--render (agent entry state)
  "Section of AGENT's inbox ENTRY in STATE, `queued' or `running'.
A running entry's text is in the session already, and is left out."
  (concat "* " (upcase (symbol-name state)) " " (lispm-transcript-input-title entry) "\n"
          ":PROPERTIES:\n"
          ":ENTRY:    " (plist-get entry :id) "\n"
          (if (plist-get entry :receipt) ":RECEIPT:  t\n" "")
          ":AGENT:    " (symbol-name (lispm-agent-name agent)) "\n"
          ":SESSION:  " (lispm-agent-id agent) "\n"
          (if (plist-get entry :from) (format ":FROM:     %s\n" (plist-get entry :from)) "")
          (if (plist-get entry :event) (format ":EVENT:    %s\n" (plist-get entry :event)) "")
          (if (plist-get entry :reply-to) (format ":REPLY_TO: %s\n" (plist-get entry :reply-to)) "")
          (if (plist-get entry :request-id) (format ":REQUEST_ID: %s\n" (plist-get entry :request-id)) "")
          (if (plist-get entry :answer-to) (format ":ANSWER_TO: %s\n" (plist-get entry :answer-to)) "")
          (if (plist-get entry :awaiting)
              (format ":AWAITING: %s\n" (lispm--json-encode (vconcat (plist-get entry :awaiting)))) "")
          (if (plist-member entry :value)
              (format ":VALUE: %s\n" (lispm--json-encode (or (plist-get entry :value) :null))) "")
          ":QUEUED:   " (or (plist-get entry :queued) (format-time-string "[%Y-%m-%d %a %H:%M]")) "\n"
          ":END:\n"
          (if (and (memq state '(queued suspended)) (plist-get entry :text))
              (lispm-transcript-body (plist-get entry :text))
            "")))

(defun lispm-inbox--parse ()
  "The entries of the inbox file in the current buffer, as plists.
Each has :state (`queued', `running' or `suspended'), routing and
receipt metadata, :awaiting dependencies, :value and :value-saved,
:text, and :beg and :end, its bounds."
  (save-excursion
    (save-restriction
      (widen)
      (let (entries)
        (dolist (section (lispm-transcript-sections))
          ;; RUNNING is a keyword of sessions too, and reads as one.
          (let ((state (cond ((equal (plist-get section :todo) "RUNNING") 'running)
                             ((string-prefix-p "SUSPENDED " (plist-get section :title)) 'suspended)
                             ((string-prefix-p "QUEUED " (plist-get section :title)) 'queued)))
                (body (plist-get section :body))
                (end (plist-get section :end)))
            (when state
              (let* ((props (lispm-transcript-properties body end))
                     (get (lambda (name)
                            (let ((value (cdr (assoc name props))))
                              (and value (not (string-empty-p value)) value)))))
                (push (list :state state
                            :id (funcall get "ENTRY")
                            :receipt (equal (funcall get "RECEIPT") "t")
                            :agent (funcall get "AGENT")
                            :session (funcall get "SESSION")
                            :from (funcall get "FROM")
                            :event (funcall get "EVENT")
                            :reply-to (funcall get "REPLY_TO")
                            :request-id (funcall get "REQUEST_ID")
                            :answer-to (funcall get "ANSWER_TO")
                            :awaiting (when-let* ((json (funcall get "AWAITING")))
                                        (append (lispm--json-decode json) nil))
                            ;; A separate flag distinguishes an explicit null
                            ;; from an old inbox with no saved turn value.
                            :value-saved (and (funcall get "VALUE") t)
                            :value (when-let* ((json (funcall get "VALUE")))
                                     (lispm--json-decode json))
                            :queued (funcall get "QUEUED")
                            :text (lispm-transcript-text-after-drawer body end)
                            :beg (plist-get section :start) :end end)
                      entries)))))
        (nreverse entries)))))

(defun lispm-inbox-entries (root)
  "The entries of the inbox of the project at ROOT; see `lispm-inbox--parse'."
  (let ((file (lispm-inbox-file root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (lispm-inbox--parse)))))

(defun lispm-inbox--for-p (agent entry)
  "Non-nil if the inbox ENTRY is for AGENT.
It is if it names AGENT's session, or for a standing agent, which
keeps its conversation across sessions, its name."
  (or (equal (plist-get entry :session) (lispm-agent-id agent))
      (and (lispm-agent-standing agent)
           (equal (plist-get entry :agent) (symbol-name (lispm-agent-name agent))))))

(defun lispm-inbox--rewrite (root function)
  "Rewrite the inbox file of the project at ROOT with FUNCTION.
FUNCTION is called in a buffer holding the file, created with a header
if need be, and changes it; the file is written only if it changed.
A buffer the user visits the file in is brought up to date; if it has
unsaved changes, the file is left as it is, and a message says so."
  (let* ((file (lispm-inbox-file root))
         (visiting (find-buffer-visiting file)))
    (if (and visiting (buffer-modified-p visiting))
        (message "%s" (lispm-tr "LispM: %s not updated: its buffer has unsaved changes"
                                (abbreviate-file-name file)))
      (with-temp-buffer
        (if (file-exists-p file)
            (insert-file-contents file)
          (insert (format "#+title: Inbox of %s\n#+todo: QUEUED RUNNING | DONE\n"
                          (abbreviate-file-name root))))
        (let ((old (buffer-string)))
          (funcall function)
          (unless (equal old (buffer-string))
            (let ((coding-system-for-write 'utf-8)
                  (lispm-inhibit-triggers t))
              (write-region nil nil file nil 'silent)))))
      (when visiting
        (lispm-tools--sync visiting)))))

(defun lispm-inbox--delete (predicate)
  "Delete the entries of the inbox in the current buffer that PREDICATE holds for."
  (dolist (entry (reverse (lispm-inbox--parse)))
    (when (funcall predicate entry)
      (delete-region (plist-get entry :beg) (plist-get entry :end)))))

(defun lispm-inbox-save (agent)
  "Write AGENT's inbox to its project's inbox file.
That is the entries queued for it and the one its turn under way
handles, if any, in place of what the file held for it (see
`lispm-inbox--for-p'), which it took back as its session opened.
Nothing is written for an agent without a session buffer, nor when
`lispm-save-sessions' is nil: then sessions are not kept either.
Failing to write is reported, never signaled: the inbox in memory goes
on working."
  (when (and (bound-and-true-p lispm-save-sessions)
             (buffer-live-p (lispm-agent-buffer agent)))
    (let ((root (lispm-agent-root agent))
          (running (lispm-agent-entry agent))
          (queued (lispm-agent-inbox agent))
          (suspended (lispm-agent-suspended agent)))
      (when (or running queued suspended (file-exists-p (lispm-inbox-file root)))
        (with-demoted-errors "LispM: saving an inbox: %S"
          (lispm-inbox--rewrite
           root
           (lambda ()
             (lispm-inbox--delete (lambda (entry) (lispm-inbox--for-p agent entry)))
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (when running
               (insert (lispm-inbox--render agent running 'running)))
             (dolist (entry suspended)
               (insert (lispm-inbox--render agent entry 'suspended)))
             (dolist (entry queued)
               (insert (lispm-inbox--render agent entry 'queued))))))))))

(defun lispm-inbox-forget (agent &optional id)
  "Remove from the inbox file the entries of AGENT's session ID (default: its own)."
  (let ((id (or id (lispm-agent-id agent)))
        (root (lispm-agent-root agent)))
    (when (file-exists-p (lispm-inbox-file root))
      (with-demoted-errors "LispM: saving an inbox: %S"
        (lispm-inbox--rewrite
         root
         (lambda ()
           (lispm-inbox--delete (lambda (entry) (equal (plist-get entry :session) id)))))))))

(defun lispm-inbox--entry (entry)
  "The inbox entry, as the runtime holds it, that the persisted ENTRY records."
  (append (list :id (plist-get entry :id)
        :receipt (plist-get entry :receipt)
        :text (plist-get entry :text)
        :from (plist-get entry :from)
        :event (plist-get entry :event)
        :reply-to (plist-get entry :reply-to)
        :request-id (plist-get entry :request-id)
        :answer-to (plist-get entry :answer-to)
        :awaiting (plist-get entry :awaiting)
        :queued (plist-get entry :queued))
          (when (plist-get entry :value-saved) (list :value (plist-get entry :value)))))

(defun lispm-inbox-take (agent)
  "The entries of the inbox file for AGENT, as the runtime holds entries.
Restore suspended requests in AGENT and return (QUEUED . RUNNING):
the entries queued for it, oldest first,
and the one its last turn handled when Emacs stopped, if any.  An
entry with a :reply-to gets its callback again (see
`lispm-inbox-callback-function')."
  (let (queued running suspended)
    (dolist (persisted (lispm-inbox-entries (lispm-agent-root agent)))
      (when (lispm-inbox--for-p agent persisted)
        (let ((entry (lispm-inbox--entry persisted)))
          (when-let* ((reply-to (plist-get entry :reply-to))
                      (callback (and lispm-inbox-callback-function
                                     (funcall lispm-inbox-callback-function agent reply-to))))
            (setq entry (plist-put entry :callback callback)))
          (pcase (plist-get persisted :state)
            ('running (setq running entry))
            ('suspended (push entry suspended))
            (_ (push entry queued))))))
    (setf (lispm-agent-suspended agent) (nreverse suspended))
    (cons (nreverse queued) running)))

(defun lispm-inbox-pending (agent)
  "Non-nil if the inbox file holds entries for AGENT."
  (seq-some (lambda (entry) (lispm-inbox--for-p agent entry))
            (lispm-inbox-entries (lispm-agent-root agent))))

(defun lispm-inbox-sessions ()
  "The sessions the inbox files hold entries for, one each, as plists.
Their keys are :session, :agent and :root; the files are those of
every project under `lispm-directory'."
  (let (sessions)
    (dolist (file (file-expand-wildcards (expand-file-name "projects/*/inbox.org" lispm-directory)))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((root (expand-file-name
                     (or (when (re-search-forward "^#\\+title: Inbox of \\(.*\\)$" nil t)
                           (match-string 1))
                         "~"))))
          (dolist (entry (lispm-inbox--parse))
            (unless (seq-find (lambda (s) (equal (plist-get s :session) (plist-get entry :session)))
                              sessions)
              (push (list :session (plist-get entry :session) :agent (plist-get entry :agent)
                          :root root)
                    sessions))))))
    (nreverse sessions)))

(provide 'lispm-inbox)
;;; lispm-inbox.el ends here
