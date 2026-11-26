;;; lispm-inbox.el --- Partitioned atomic inboxes that outlive Emacs  -*- lexical-binding: t; -*-

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
;; it was busy.  On disk it is partitioned per session using a lock-free
;; Maildir-style directory layout under `lispm-directory':
;;
;;   projects/<project>/inbox/<session-id>/
;;     tmp/        staging files for atomic rename
;;     queued/     immutable QUEUED message files (<order>-<pid>-<id>.eld)
;;     running/    currently executing input entry
;;     suspended/  entries suspended waiting for async replies
;;
;; Every write creates a unique sibling temp file and atomically replaces
;; or publishes via POSIX `rename-file', avoiding shared file locks or
;; full-file read-modify-write contention across concurrent agents or
;; Emacs processes.  `lispm-inbox-export-org' renders an on-demand
;; read-only Org summary, and existing legacy `inbox.org' files are
;; lazily migrated one-way to `.migrated' on first access.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-tools)

(defvar lispm-inbox-callback-function nil
  "Function turning a persisted entry's reply-to back into its callback.
Called with the agent the entry is for and the entry's :reply-to
string; returns a function of the agent's final reply, or nil.  An
entry taken back after a restart of Emacs gets that callback, its own
having been lost with Emacs.")

(defconst lispm-inbox--states '(running suspended queued)
  "Valid persistent mailbox subdirectories in recovery precedence order.")

(defvar lispm-inbox--seq 0
  "Monotonic per-process counter ordering inbox message files.")

(defvar lispm-inbox--known (make-hash-table :test #'eq :weakness 'key)
  "Agent -> hash table mapping persisted entry ID -> (STATE FILE HASH ORDER).")

(defun lispm-inbox--safe-name (value)
  "Convert VALUE into a safe single path component string."
  (let* ((raw (cond ((symbolp value) (symbol-name value))
                    ((stringp value) value)
                    (t (format "%s" (or value "")))))
         (clean (replace-regexp-in-string "[^[:alnum:]._-]" "_" raw)))
    (if (member clean '("" "." "..")) "_" clean)))

(defun lispm-inbox-file (root)
  "Legacy single-file inbox path of the project at ROOT."
  (expand-file-name "inbox.org" (lispm-project-directory root)))

(defun lispm-inbox-directory (root &optional session-id)
  "Return the partitioned inbox directory of ROOT, or SESSION-ID under it."
  (let ((base (expand-file-name "inbox/" (lispm-project-directory root))))
    (if session-id
        (expand-file-name (concat (lispm-inbox--safe-name session-id) "/") base)
      base)))

(defun lispm-inbox-new-id ()
  "A fresh id for an inbox entry."
  (concat "i" (lispm-new-id)))

(defun lispm-inbox--next-order ()
  "Return a lexicographically sortable monotonic order token."
  (format "%016d-%06d"
          (truncate (* (float-time) 1000000))
          (cl-incf lispm-inbox--seq)))

(defun lispm-inbox--state-dir (session-dir state)
  "Return the subdirectory of SESSION-DIR holding entries in STATE."
  (unless (memq state lispm-inbox--states)
    (error "Unknown inbox state: %S" state))
  (expand-file-name (concat (symbol-name state) "/") session-dir))

(defun lispm-inbox--entry-record (root session-id agent-name entry state order)
  "Build canonical persisted plist for ENTRY in STATE under ROOT/SESSION-ID."
  (let ((agent-str (cond ((symbolp agent-name) (symbol-name agent-name))
                         ((stringp agent-name) agent-name)
                         (t ""))))
    (nconc (list :version 1
                 :state state
                 :id (or (plist-get entry :id) (lispm-inbox-new-id))
                 :order order
                 :root (expand-file-name root)
                 :agent agent-str
                 :session session-id
                 :receipt (and (plist-get entry :receipt) t)
                 :from (plist-get entry :from)
                 :event (plist-get entry :event)
                 :reply-to (plist-get entry :reply-to)
                 :request-id (plist-get entry :request-id)
                 :answer-to (plist-get entry :answer-to)
                 :awaiting (plist-get entry :awaiting)
                 :queued (or (plist-get entry :queued)
                             (format-time-string "[%Y-%m-%d %a %H:%M]"))
                 :text (plist-get entry :text))
           (when (or (plist-get entry :value-saved)
                     (plist-member entry :value))
             (list :value-saved t :value (plist-get entry :value))))))

(defun lispm-inbox--serialize-record (record)
  "Serialize inbox RECORD plist deterministically as readable Elisp."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil))
    (prin1-to-string record)))

(defun lispm-inbox--record-file (session-dir state order id)
  "Return path inside SESSION-DIR/STATE for entry ID ordered at ORDER."
  (expand-file-name (format "%s-%05d-%s.eld"
                            order (mod (emacs-pid) 100000) (lispm-inbox--safe-name id))
                    (lispm-inbox--state-dir session-dir state)))

(defun lispm-inbox--write-record (session-dir state record)
  "Atomically write serialized RECORD into SESSION-DIR/STATE and return path."
  (let* ((tmp-dir (expand-file-name "tmp/" session-dir))
         (target (lispm-inbox--record-file
                  session-dir state (plist-get record :order) (plist-get record :id))))
    (lispm-write-file-atomic target (lispm-inbox--serialize-record record) nil tmp-dir)
    target))

(defun lispm-inbox-enqueue (root session-id agent-name entry &optional state)
  "Atomically enqueue ENTRY for SESSION-ID (AGENT-NAME) under project ROOT.
STATE defaults to `queued' and may be `queued', `running' or `suspended'.
Returns the written event file path without modifying any shared index."
  (let* ((state (or state 'queued))
         (session-dir (lispm-inbox-directory root session-id))
         (order (lispm-inbox--next-order))
         (record (lispm-inbox--entry-record root session-id agent-name entry state order)))
    (lispm-inbox--write-record session-dir state record)))

;;;; Legacy Org parsing & one-way migration

(defun lispm-inbox--render (agent entry state)
  "Org section of AGENT's inbox ENTRY in STATE (`queued', `running', `suspended').
AGENT may be a `lispm-agent' struct or an agent name string."
  (let ((agent-name (cond ((lispm-agent-p agent) (symbol-name (lispm-agent-name agent)))
                          ((symbolp agent) (symbol-name agent))
                          ((stringp agent) agent)
                          (t (or (plist-get entry :agent) ""))))
        (session-id (if (lispm-agent-p agent)
                        (lispm-agent-id agent)
                      (or (plist-get entry :session) ""))))
    (concat "* " (upcase (symbol-name state)) " " (lispm-transcript-input-title entry) "\n"
            ":PROPERTIES:\n"
            ":ENTRY:    " (plist-get entry :id) "\n"
            (if (plist-get entry :receipt) ":RECEIPT:  t\n" "")
            ":AGENT:    " agent-name "\n"
            ":SESSION:  " session-id "\n"
            (if (plist-get entry :from) (format ":FROM:     %s\n" (plist-get entry :from)) "")
            (if (plist-get entry :event) (format ":EVENT:    %s\n" (plist-get entry :event)) "")
            (if (plist-get entry :reply-to) (format ":REPLY_TO: %s\n" (plist-get entry :reply-to)) "")
            (if (plist-get entry :request-id) (format ":REQUEST_ID: %s\n" (plist-get entry :request-id)) "")
            (if (plist-get entry :answer-to) (format ":ANSWER_TO: %s\n" (plist-get entry :answer-to)) "")
            (if (plist-get entry :awaiting)
                (format ":AWAITING: %s\n" (lispm-json-encode (vconcat (plist-get entry :awaiting)))) "")
            (if (or (plist-get entry :value-saved) (plist-member entry :value))
                (format ":VALUE: %s\n" (lispm-json-encode (or (plist-get entry :value) :null))) "")
            ":QUEUED:   " (or (plist-get entry :queued) (format-time-string "[%Y-%m-%d %a %H:%M]")) "\n"
            ":END:\n"
            (if (and (memq state '(queued suspended)) (plist-get entry :text))
                (lispm-transcript-body (plist-get entry :text))
              ""))))

(defun lispm-inbox--parse ()
  "The entries of the legacy inbox file in the current buffer, as plists."
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
                                        (append (lispm-json-decode json) nil))
                            ;; A separate flag distinguishes an explicit null
                            ;; from an old inbox with no saved turn value.
                            :value-saved (and (funcall get "VALUE") t)
                            :value (when-let* ((json (funcall get "VALUE")))
                                     (lispm-json-decode json))
                            :queued (funcall get "QUEUED")
                            :text (lispm-transcript-text-after-drawer body end)
                            :beg (plist-get section :start) :end end)
                      entries)))))
        (nreverse entries)))))

(defun lispm-inbox--session-has-entry-p (session-dir id)
  "Return non-nil when SESSION-DIR already persists an entry with ID."
  (let ((pattern (format "*-%s.eld" (lispm-inbox--safe-name id))))
    (cl-some (lambda (state)
               (let ((dir (lispm-inbox--state-dir session-dir state)))
                 (and (file-directory-p dir)
                      (directory-files dir nil
                                       (wildcard-to-regexp pattern)
                                       t))))
             lispm-inbox--states)))

(defun lispm-inbox--migrate-legacy-file (file &optional root)
  "Migrate legacy `inbox.org' at FILE into partitioned `.eld' files, once."
  (when (file-exists-p file)
    (let (parsed-root entries)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (setq parsed-root
                  (or root
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^#\\+title: Inbox of \\(.*\\)$" nil t)
                          (expand-file-name (match-string 1))))
                      (file-name-directory file))
                  entries (lispm-inbox--parse)))
        (file-missing (setq entries nil)))
      (dolist (entry entries)
        (let ((sid (or (plist-get entry :session) "legacy"))
              (id (or (plist-get entry :id) (lispm-inbox-new-id)))
              (state (or (plist-get entry :state) 'queued)))
          (let ((sdir (lispm-inbox-directory parsed-root sid)))
            (unless (lispm-inbox--session-has-entry-p sdir id)
              (lispm-inbox-enqueue
               parsed-root sid (plist-get entry :agent)
               (plist-put (copy-sequence entry) :id id) state)))))
      (ignore-errors (rename-file file (concat file ".migrated") t)))))

(defun lispm-inbox--migrate-legacy (root)
  "Migrate legacy `inbox.org' of ROOT if present."
  (let ((file (lispm-inbox-file root)))
    (when (file-exists-p file)
      (lispm-inbox--migrate-legacy-file file root))))

;;;; Partitioned reader, sync & recovery

(defun lispm-inbox--state-rank (state)
  "Precedence rank of STATE for deduplication and ordering."
  (pcase state
    ('running 0)
    ('suspended 1)
    (_ 2)))

(defun lispm-inbox--read-file (file)
  "Read one partitioned inbox `.eld' record FILE, or nil if missing or invalid."
  (condition-case nil
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (let* ((read-circle nil)
               (record (read (current-buffer))))
          (when (and (listp record) (plist-get record :id) (plist-get record :state))
            (plist-put record :file file)
            record)))
    ((file-missing error) nil)))

(defun lispm-inbox--read-directory (inbox-dir &optional root)
  "Read deduplicated, ordered entry plists from partitioned INBOX-DIR.
When ROOT is non-nil, only return entries matching ROOT or unset."
  (when (file-directory-p inbox-dir)
    (let ((by-id (make-hash-table :test #'equal))
          (canon-root (and root (file-name-as-directory (expand-file-name root))))
          records)
      (dolist (sdir (directory-files inbox-dir t "\\`[^.]" t))
        (when (file-directory-p sdir)
          (dolist (state lispm-inbox--states)
            (let ((stdir (lispm-inbox--state-dir sdir state)))
              (when (file-directory-p stdir)
                (dolist (file (directory-files stdir t "\\.eld\\'" t))
                  (when-let* ((rec (lispm-inbox--read-file file)))
                    (when (or (not canon-root)
                              (not (plist-get rec :root))
                              (equal (file-name-as-directory (expand-file-name (plist-get rec :root)))
                                     canon-root))
                      (let* ((id (plist-get rec :id))
                             (prev (gethash id by-id)))
                        (when (or (null prev)
                                  (< (lispm-inbox--state-rank (plist-get rec :state))
                                     (lispm-inbox--state-rank (plist-get prev :state)))
                                  (and (eq (plist-get rec :state) (plist-get prev :state))
                                       (string> (or (plist-get rec :order) "")
                                                (or (plist-get prev :order) ""))))
                          (puthash id rec by-id)))))))))))
      (maphash (lambda (_id rec) (push rec records)) by-id)
      (sort records
            (lambda (a b)
              (let ((ra (lispm-inbox--state-rank (plist-get a :state)))
                    (rb (lispm-inbox--state-rank (plist-get b :state))))
                (if (/= ra rb)
                    (< ra rb)
                  (string< (or (plist-get a :order) "")
                           (or (plist-get b :order) "")))))))))

(defun lispm-inbox-entries (root)
  "The entries of the partitioned inbox of project ROOT."
  (lispm-inbox--migrate-legacy root)
  (lispm-inbox--read-directory (lispm-inbox-directory root) root))

(defun lispm-inbox--for-p (agent entry)
  "Non-nil if the inbox ENTRY is for AGENT.
It is if it names AGENT's session, or for a standing agent, which
keeps its conversation across sessions, its name."
  (or (equal (plist-get entry :session) (lispm-agent-id agent))
      (and (lispm-agent-standing agent)
           (equal (plist-get entry :agent) (symbol-name (lispm-agent-name agent))))))

(defun lispm-inbox--agent-known (agent)
  "Return the per-agent persisted-entry tracking table for AGENT."
  (or (gethash agent lispm-inbox--known)
      (let ((tbl (make-hash-table :test #'equal)))
        (puthash agent tbl lispm-inbox--known)
        tbl)))

(defun lispm-inbox--sync-desired (agent root session-dir known desired)
  "Persist AGENT's DESIRED `(STATE . ENTRY)' items under ROOT into SESSION-DIR."
  (let ((agent-name (symbol-name (lispm-agent-name agent)))
        (session-id (lispm-agent-id agent)))
    (pcase-dolist (`(,state . ,entry) desired)
      (let* ((id (or (plist-get entry :id) (lispm-inbox-new-id)))
             (prev (gethash id known))
             (prev-state (nth 0 prev))
             (prev-file (nth 1 prev))
             (prev-hash (nth 2 prev))
             (prev-order (nth 3 prev))
             (same-dir (and prev-file (file-in-directory-p prev-file session-dir)))
             (order (if (and prev-order same-dir (eq prev-state state))
                        prev-order
                      (lispm-inbox--next-order)))
             (record (lispm-inbox--entry-record
                      root session-id agent-name
                      (if (plist-get entry :id) entry (plist-put (copy-sequence entry) :id id))
                      state order))
             (payload (lispm-inbox--serialize-record record))
             (digest (secure-hash 'sha256 payload)))
        (unless (and (eq prev-state state)
                     same-dir
                     (equal prev-hash digest)
                     prev-file
                     (file-exists-p prev-file))
          (let ((new-file (lispm-inbox--write-record session-dir state record)))
            (when (and prev-file (not (equal prev-file new-file)))
              (ignore-errors (delete-file prev-file)))
            (puthash id (list state new-file digest order) known)))))))

(defun lispm-inbox-save (agent)
  "Persist AGENT's queued, running and suspended entries in its session partition.
Nothing is written for an agent without a session buffer, nor when
`lispm-save-sessions' is nil.  Unowned files concurrently written by
other agents or processes in the same inbox directory are never touched."
  (when (and (bound-and-true-p lispm-save-sessions)
             (buffer-live-p (lispm-agent-buffer agent)))
    (let* ((root (lispm-agent-root agent))
           (session-id (lispm-agent-id agent))
           (session-dir (lispm-inbox-directory root session-id))
           (running (lispm-agent-entry agent))
           (queued (lispm-agent-inbox agent))
           (suspended (lispm-agent-suspended agent))
           (known (lispm-inbox--agent-known agent))
           desired desired-ids)
      (lispm-inbox--migrate-legacy root)
      (when running
        (push (cons 'running running) desired))
      (dolist (entry suspended)
        (push (cons 'suspended entry) desired))
      (dolist (entry queued)
        (push (cons 'queued entry) desired))
      (setq desired (nreverse desired))
      (when (or desired (> (hash-table-count known) 0) (file-directory-p session-dir))
        (with-demoted-errors "LispM: saving an inbox: %S"
          (setq desired-ids (make-hash-table :test #'equal))
          (dolist (item desired)
            (when-let* ((id (plist-get (cdr item) :id)))
              (puthash id t desired-ids)))
          (when desired
            (lispm-inbox--sync-desired agent root session-dir known desired))
          (let (stale)
            (maphash (lambda (id info)
                       (unless (gethash id desired-ids)
                         (push (cons id (nth 1 info)) stale)))
                     known)
            (dolist (item stale)
              (when (cdr item)
                (ignore-errors (delete-file (cdr item))))
              (remhash (car item) known))))))))

(defun lispm-inbox-forget (agent &optional id)
  "Remove persisted inbox entries of AGENT's session ID (default: its own)."
  (let* ((id (or id (lispm-agent-id agent)))
         (root (lispm-agent-root agent))
         (session-dir (lispm-inbox-directory root id)))
    (lispm-inbox--migrate-legacy root)
    (when (file-directory-p session-dir)
      (with-demoted-errors "LispM: saving an inbox: %S"
        (dolist (state lispm-inbox--states)
          (let ((stdir (lispm-inbox--state-dir session-dir state)))
            (when (file-directory-p stdir)
              (dolist (file (directory-files stdir t "\\.eld\\'" t))
                (ignore-errors (delete-file file))))))))
    (when (equal id (lispm-agent-id agent))
      (when-let* ((known (gethash agent lispm-inbox--known)))
        (clrhash known)))))

(defun lispm-inbox--entry (entry)
  "The inbox entry, as the runtime holds it, that persisted ENTRY records."
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
  "The entries of the inbox for AGENT, as the runtime holds entries.
Restore suspended requests in AGENT and return (QUEUED . RUNNING):
the entries queued for it, oldest first, and the one its last turn
handled when Emacs stopped, if any."
  (let ((known (lispm-inbox--agent-known agent))
        queued running suspended)
    (dolist (persisted (lispm-inbox-entries (lispm-agent-root agent)))
      (when (lispm-inbox--for-p agent persisted)
        (let ((entry (lispm-inbox--entry persisted))
              (state (plist-get persisted :state)))
          (when-let* ((id (plist-get entry :id)))
            (puthash id (list state (plist-get persisted :file) nil (plist-get persisted :order))
                     known))
          (when-let* ((reply-to (plist-get entry :reply-to))
                      (callback (and lispm-inbox-callback-function
                                     (funcall lispm-inbox-callback-function agent reply-to))))
            (setq entry (plist-put entry :callback callback)))
          (pcase state
            ('running (setq running entry))
            ('suspended (push entry suspended))
            (_ (push entry queued))))))
    (setf (lispm-agent-suspended agent) (nreverse suspended))
    (cons (nreverse queued) running)))

(defun lispm-inbox-pending (agent)
  "Non-nil if the inbox holds entries for AGENT."
  (seq-some (lambda (entry) (lispm-inbox--for-p agent entry))
            (lispm-inbox-entries (lispm-agent-root agent))))

(defun lispm-inbox-sessions ()
  "The sessions the partitioned inboxes hold entries for, one each, as plists.
Their keys are :session, :agent and :root; scanned across every project
under `lispm-directory'."
  (dolist (legacy (file-expand-wildcards (expand-file-name "projects/*/inbox.org" lispm-directory)))
    (lispm-inbox--migrate-legacy-file legacy))
  (let (sessions)
    (dolist (inbox-dir (file-expand-wildcards (expand-file-name "projects/*/inbox" lispm-directory)))
      (when (file-directory-p inbox-dir)
        (dolist (entry (lispm-inbox--read-directory inbox-dir))
          (unless (seq-find (lambda (s) (equal (plist-get s :session) (plist-get entry :session)))
                            sessions)
            (push (list :session (plist-get entry :session)
                        :agent (plist-get entry :agent)
                        :root (expand-file-name (or (plist-get entry :root) "~")))
                  sessions)))))
    (nreverse sessions)))

;;;; On-demand read-only Org export & view

(defun lispm-inbox-export-org (root)
  "Render project ROOT's current partitioned inbox as an Org string, or nil.
This is a pure read-only projection and never modifies `inbox.org'."
  (let ((entries (lispm-inbox-entries root)))
    (when (or entries (file-directory-p (lispm-inbox-directory root)))
      (concat (format "#+title: Inbox of %s\n#+todo: QUEUED RUNNING | DONE\n"
                      (abbreviate-file-name root))
              (mapconcat (lambda (entry)
                           (lispm-inbox--render (plist-get entry :agent)
                                                entry
                                                (plist-get entry :state)))
                         entries "")))))

(defun lispm-inbox-view (root)
  "Display project ROOT's partitioned inbox in a read-only Org buffer."
  (interactive "DProject root: ")
  (let* ((name (format "*lispm-inbox: %s*" (abbreviate-file-name root)))
         (buf (get-buffer-create name))
         (text (or (lispm-inbox-export-org root)
                   (format "#+title: Inbox of %s\n(empty)\n" (abbreviate-file-name root)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (org-mode)
      (read-only-mode 1)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'lispm-inbox)
;;; lispm-inbox.el ends here
