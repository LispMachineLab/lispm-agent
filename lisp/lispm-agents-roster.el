;;; lispm-agents-roster.el --- Live agent data and change notifications  -*- lexical-binding: t; -*-

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

;; A roster reads only live buffers and already known session metadata.
;; The inspector reads work and mailbox contents separately, on demand.
;; Changes collect until the next event-loop turn; message events keep
;; their order even when several changes cause just one notification.

;;; Code:

(require 'lispm-agents)
(require 'lispm-inspect)
(require 'lispm-agents-bridge)

(cl-defstruct (lispm-agents-roster--index
               (:constructor lispm-agents-roster--make-index) (:copier nil))
  "The identity and relation indexes shared by one snapshot."
  buffers                    ; live buffers, most recently used first
  keys                       ; buffer -> stable key, or nil while unknown
  by-key                     ; stable key -> buffer
  agents                     ; runtime agent, including bridge proxies -> buffer
  names                      ; agent name, peer name or LispM session id -> buffer
  parents                    ; bridge proxy session id -> terminal buffer
  sessions                   ; buffer -> already known session metadata
  archived                   ; effective archive keys for this live graph
  waits)                     ; buffer -> wait and request plist

(defvar lispm-agents-roster--index nil
  "The index shared during a snapshot, or nil outside it.")

(defvar-local lispm-agents-roster--terminal-data nil
  "The last status and mailbox summary observed for this terminal.")

(defun lispm-agents-roster--agent (buffer)
  "Return the LispM agent acting in BUFFER, if any.
A terminal's bridge agent carries its delegations, but is never a
separate roster entry."
  (or (buffer-local-value 'lispm-session-agent buffer)
      (buffer-local-value 'lispm-agents-bridge--agent buffer)))

(defun lispm-agents-roster--buffer-key (buffer)
  "Return BUFFER's known stable key without consulting a session reader."
  (let* ((agent (buffer-local-value 'lispm-session-agent buffer))
         (kind (lispm-agents-of-buffer buffer))
         (id (if agent (lispm-agent-id agent)
               (buffer-local-value 'lispm-agents-buffer-session-id buffer))))
    (when (and kind id) (list kind id))))

(defun lispm-agents-roster-key (subject)
  "Return SUBJECT's stable (KIND SESSION-ID) key, or nil if unknown.
SUBJECT is an agent, buffer or a key already formed.  A fresh terminal
has no key until its session id is learned; no temporary identity is
invented for it.  This never asks a session reader to scan files."
  (cond ((lispm-agent-p subject)
         (let ((buffer (if lispm-agents-roster--index
                           (gethash subject (lispm-agents-roster--index-agents lispm-agents-roster--index))
                         (or (lispm-agent-buffer subject)
                             (seq-find (lambda (buffer)
                                         (eq subject (buffer-local-value 'lispm-agents-bridge--agent buffer)))
                                       (lispm-agents-live-buffers))))))
           (if (buffer-live-p buffer) (lispm-agents-roster-key buffer)
             (when (lispm-agent-id subject) (list 'lispm (lispm-agent-id subject))))))
        ((buffer-live-p subject)
         (if lispm-agents-roster--index
             (gethash subject (lispm-agents-roster--index-keys lispm-agents-roster--index))
           (lispm-agents-roster--buffer-key subject)))
        ((and (consp subject) (symbolp (car subject))
              (stringp (cadr subject)) (null (cddr subject)))
         (copy-sequence subject))))

(defun lispm-agents-roster-buffer (key)
  "Return the live, unarchived buffer identified by KEY, or nil."
  (when key
    (let* ((index (or lispm-agents-roster--index
                      (lispm-agents-roster--build-index (lispm-agents-live-buffers))))
           (buffer (gethash key (lispm-agents-roster--index-by-key index))))
      (unless (gethash (lispm-agents--session-key (list :agent (car key) :id (cadr key)))
                       (lispm-agents-roster--index-archived index))
        buffer))))

(defun lispm-agents-roster--named-key (name)
  "Return the known key of agent or peer NAME for a source notification."
  (when name
    (if lispm-agents-roster--index
        (when-let* ((buffer (gethash (format "%s" name)
                                     (lispm-agents-roster--index-names lispm-agents-roster--index))))
          (lispm-agents-roster-key buffer))
      (if-let* ((agent (lispm-get-agent name t)))
          (lispm-agents-roster-key agent)
        (when-let* ((buffer (seq-find
                            (lambda (buffer)
                              (equal (format "%s" name)
                                     (buffer-local-value 'lispm-agents-bridge--name buffer)))
                            (lispm-agents-live-buffers))))
          (lispm-agents-roster-key buffer))))))

(defun lispm-agents-roster--build-index (buffers)
  "Index the identities and relations of BUFFERS once for a snapshot."
  (let* ((index (lispm-agents-roster--make-index
                 :buffers buffers :keys (make-hash-table :test #'eq)
                 :by-key (make-hash-table :test #'equal) :agents (make-hash-table :test #'eq)
                 :names (make-hash-table :test #'equal) :parents (make-hash-table :test #'equal)
                 :sessions (make-hash-table :test #'eq)
                 :waits (make-hash-table :test #'eq)))
         (names (lispm-agents-roster--index-names index)))
    (dolist (buffer buffers)
      (let ((key (lispm-agents-roster--buffer-key buffer))
            (agent (lispm-agents-roster--agent buffer)))
        (puthash buffer key (lispm-agents-roster--index-keys index))
        (when key
          (puthash key buffer (lispm-agents-roster--index-by-key index))
          (puthash buffer (or (lispm-agents-known-session (car key) (cadr key))
                              (buffer-local-value 'lispm-agents-buffer-session buffer))
                   (lispm-agents-roster--index-sessions index)))
        (when agent
          (puthash agent buffer (lispm-agents-roster--index-agents index))
          (puthash (symbol-name (lispm-agent-name agent)) buffer names)
          (when (lispm-agent-id agent) (puthash (lispm-agent-id agent) buffer names)))
        (when-let* ((name (buffer-local-value 'lispm-agents-bridge--name buffer)))
          (puthash name buffer names))
        (when-let* ((proxy (buffer-local-value 'lispm-agents-bridge--agent buffer)))
          (puthash (lispm-agent-id proxy) buffer (lispm-agents-roster--index-parents index)))))
    (setf (lispm-agents-roster--index-archived index) (lispm-agents-roster--archive-index index))
    (lispm-agents-roster--build-wait-index index)
    index))

(defun lispm-agents-roster--archive-index (index)
  "Resolve archives from INDEX's live parents without scanning transcripts.
Overlay known live relations on the last full session graph for this
snapshot only; a partial roster must not erase historical parent links."
  (let ((lispm-agents--session-parents lispm-agents--session-parents)
        (lispm-agents--effective-archive nil)
        sessions bridges)
    (dolist (buffer (lispm-agents-roster--index-buffers index))
      (when-let* ((key (gethash buffer (lispm-agents-roster--index-keys index))))
        (let* ((agent (buffer-local-value 'lispm-session-agent buffer))
               (session (gethash buffer (lispm-agents-roster--index-sessions index)))
               (parent (if agent (lispm-agent-parent agent) (plist-get session :parent)))
               (proxy (and parent agent (gethash parent (lispm-agents-roster--index-parents index))))
               (parent-key (and proxy (gethash proxy (lispm-agents-roster--index-keys index)))))
          ;; Unknown terminal metadata cannot clear a cached relation.
          (when (or agent session)
            (push (list :agent (car key) :id (cadr key) :parent parent) sessions))
          (when parent-key
            (push (cons (lispm-agents--session-key (list :agent (car key) :id (cadr key)))
                        (lispm-agents--session-key (list :agent (car parent-key) :id (cadr parent-key))))
                  bridges)))))
    (lispm-agents--index-session-parents sessions t)
    (dolist (edge bridges)
      (puthash (car edge) (cdr edge) (cdr lispm-agents--session-parents)))
    (lispm-agents--effective-archive)))

(defun lispm-agents-roster--add-wait (index buffer field value)
  "Add VALUE to BUFFER's relation FIELD in INDEX, ignoring unknown values."
  (when (and buffer value)
    (let* ((table (lispm-agents-roster--index-waits index))
           (record (gethash buffer table))
           (values (plist-get record field)))
      (unless (member value values)
        (puthash buffer (plist-put record field (cons value values)) table)))))

(defun lispm-agents-roster--build-wait-index (index)
  "Index each delegation, inbox and peer request once in INDEX."
  (let ((lispm-agents-roster--index index)
        (names (lispm-agents-roster--index-names index))
        (agents (lispm-agents-roster--index-agents index)))
    (dolist (buffer (lispm-agents-roster--index-buffers index))
      (when-let* ((agent (lispm-agents-roster--agent buffer)))
        (dolist (target (lispm-agent-waiting-on agent))
          (lispm-agents-roster--add-wait index buffer :waits-on (lispm-agents-roster-key target))
          (lispm-agents-roster--add-wait index (gethash target agents) :waited-on-by
                                        (lispm-agents-roster-key buffer)))
        (when (buffer-local-value 'lispm-session-agent buffer)
          (dolist (entry (cons (lispm-agent-entry agent)
                              (append (lispm-agent-inbox agent) (lispm-agent-suspended agent))))
            (when-let* ((id (or (plist-get entry :request-id) (plist-get entry :reply-to))))
              (lispm-agents-roster--add-wait index buffer :request-ids id)
              (lispm-agents-roster--add-wait
               index (gethash (format "%s" (plist-get entry :from)) names) :request-ids id))))))
    (dolist (request lispm-collab--requests)
      (let* ((metadata (nth 3 request))
             (from (gethash (format "%s" (plist-get metadata :from)) names))
             (to (gethash (format "%s" (or (plist-get metadata :to) (nth 1 request))) names)))
        (lispm-agents-roster--add-wait index from :request-ids (car request))
        (lispm-agents-roster--add-wait index to :request-ids (car request))
        (when to (lispm-agents-roster--add-wait index from :waits-on (lispm-agents-roster-key to)))
        (when from (lispm-agents-roster--add-wait index to :waited-on-by (lispm-agents-roster-key from)))))
    (maphash (lambda (buffer record)
               (dolist (field '(:waits-on :waited-on-by :request-ids))
                 (setq record (plist-put record field (nreverse (plist-get record field)))))
               (puthash buffer record (lispm-agents-roster--index-waits index)))
             (lispm-agents-roster--index-waits index))))

(defun lispm-agents-roster-waits (buffer)
  "Return BUFFER's :waits-on, :waited-on-by and :request-ids.
A snapshot builds the relation index once.  A standalone inspector
builds a fresh index so settled requests never remain cached."
  (let ((index (or lispm-agents-roster--index
                   (lispm-agents-roster--build-index (lispm-agents-live-buffers)))))
    (or (gethash buffer (lispm-agents-roster--index-waits index))
        (list :waits-on nil :waited-on-by nil :request-ids nil))))

(defun lispm-agents-roster--record (buffer)
  "Return the cheap roster record of live agent BUFFER, or nil if archived."
  (let* ((kind (lispm-agents-of-buffer buffer))
         (key (lispm-agents-roster-key buffer))
         (agent (buffer-local-value 'lispm-session-agent buffer))
         (session (gethash buffer (lispm-agents-roster--index-sessions lispm-agents-roster--index)))
         (parent (if agent (lispm-agent-parent agent) (plist-get session :parent)))
         (parent-proxy (and parent agent
                            (gethash parent (lispm-agents-roster--index-parents lispm-agents-roster--index))))
         (character (and agent (lispm-agent-character agent)
                         (lispm-get-character (lispm-agent-character agent) t)))
         (props (lispm-agents-props kind))
         (status (lispm-agents-buffer-status buffer))
         (mailbox (if agent
                      (list :handling (and (lispm-agent-entry agent) t)
                            :queued (length (lispm-agent-inbox agent)))
                    (lispm-agents-bridge-mailbox-summary buffer))))
    (unless (and key (gethash (lispm-agents--session-key (list :agent kind :id (cadr key)))
                              (lispm-agents-roster--index-archived lispm-agents-roster--index)))
      (when (and lispm-agents-roster-change-functions (not agent)
                 (null (buffer-local-value 'lispm-agents-roster--terminal-data buffer)))
        (with-current-buffer buffer
          (setq lispm-agents-roster--terminal-data
                (lispm-inspect--copy-data (list :status status :mailbox mailbox)))))
      (append
       (list :key key :buffer buffer
             :name (if agent (symbol-name (lispm-agent-name agent))
                     (lispm-agents-buffer-label buffer))
             :kind kind
             :root (if agent (lispm-agent-root agent)
                     (or (plist-get session :cwd) (buffer-local-value 'default-directory buffer)))
             :parent (if parent-proxy (lispm-agents-roster-key parent-proxy)
                       (and parent (list kind parent)))
             :alive (and (lispm-agents-alive-p buffer) t)
             :running (lispm-agents-running-p buffer)
             :status status
             :character (and agent (lispm-agent-character agent))
             :character-name (and character (lispm-character-title character))
             :app-character (and character (lispm-character-skill character) t)
             :avatar (and character (lispm-character-avatar character))
             :mailbox mailbox
             :actions (if agent '(tell send approve approve-all deny abort open-chat)
                        (append (when (or (plist-get props :ready) (plist-get props :deliver))
                                  '(send))
                                '(open-chat))))
       (lispm-agents-roster-waits buffer)))))

(defun lispm-agents-roster ()
  "Return a plain snapshot of alive, unarchived agents, most recently used first.
Each plist has :key (KIND SESSION-ID), :buffer, :name, :kind, :root,
:parent (a key), :alive, :running, :status, :character (its name symbol),
:character-name (its display name), :app-character, :avatar, :mailbox
\(:handling boolean, :queued count), :waits-on and :waited-on-by (keys),
:request-ids and :actions (supported action symbols).  Unknown data is
nil, including a fresh terminal's key before it learns its session id.
No transcript, inbox file or diary is read; terminal metadata is already
known to a session reader or retained when sessions were listed.
Read work and mail contents lazily
with `lispm-agents-roster-details'.  Returned data may be changed by the
caller without changing agents; buffer objects still identify live buffers."
  (let ((lispm-agents-roster--index (lispm-agents-roster--build-index (lispm-agents-live-buffers))))
    (lispm-inspect--copy-data
     (delq nil (mapcar #'lispm-agents-roster--record
                       (lispm-agents-roster--index-buffers lispm-agents-roster--index))))))

;;;; Changes

(defvar lispm-agents-roster--timer nil
  "The single pending flush timer, or nil when there are no changes.")

(defvar lispm-agents-roster--keys nil
  "Stable keys explicitly changed before the next flush.")

(defvar lispm-agents-roster--fields nil
  "Record fields explicitly changed before the next flush.")

(defvar lispm-agents-roster--events nil
  "One-shot events collected before the next flush, newest first.")

(defvar lispm-agents-roster--sequence 0
  "Sequence number of the last one-shot event collected.")

(defvar lispm-agents-roster--previous nil
  "The snapshot delivered by the last flush, for appearance and departure.")

(defvar lispm-agents-roster--pending-buffers nil
  "Buffers explicitly changed before the next flush, including unknown ids.")

(defvar lispm-agents-roster--all nil
  "Non-nil when a source without affected identities requires a full snapshot.")

(defun lispm-agents-roster--identity (record)
  "Return RECORD's internal identity, its stable key or its unknown-id buffer."
  (or (plist-get record :key) (plist-get record :buffer)))

(defun lispm-agents-roster--collect (subject fields event)
  "Collect SUBJECT's FIELDS and EVENT for one event-loop notification."
  (when lispm-agents-roster-change-functions
    ;; A flush that ran after the last observer closed did no work.  Its
    ;; expired timer must not prevent a newly opened observer from updating.
    (when (and lispm-agents-roster--timer (not (memq lispm-agents-roster--timer timer-list)))
      (setq lispm-agents-roster--timer nil lispm-agents-roster--keys nil
            lispm-agents-roster--fields nil lispm-agents-roster--events nil
            lispm-agents-roster--pending-buffers nil lispm-agents-roster--all nil))
    (when-let* ((key (lispm-agents-roster-key subject)))
      (cl-pushnew (lispm-inspect--copy-data key) lispm-agents-roster--keys :test #'equal))
    (when-let* ((buffer (cond ((bufferp subject) subject)
                              ((lispm-agent-p subject) (lispm-agent-buffer subject)))))
      (cl-pushnew buffer lispm-agents-roster--pending-buffers))
    (when (or (null subject) (memq 'agents fields)) (setq lispm-agents-roster--all t))
    (dolist (field fields) (cl-pushnew field lispm-agents-roster--fields))
    (when event
      (push (plist-put (lispm-inspect--copy-data event) :sequence
                       (cl-incf lispm-agents-roster--sequence))
            lispm-agents-roster--events))
    (unless lispm-agents-roster--timer
      (setq lispm-agents-roster--timer (run-at-time 0 nil #'lispm-agents-roster--flush)))))

(defun lispm-agents-roster--flush ()
  "Publish one batch, indexing old records and rebuilding only affected ones."
  (when lispm-agents-roster-change-functions
    (when lispm-agents-roster--timer (cancel-timer lispm-agents-roster--timer))
    (setq lispm-agents-roster--timer nil)
    (let* ((keys (nreverse lispm-agents-roster--keys))
           (fields (nreverse lispm-agents-roster--fields))
           (events (nreverse lispm-agents-roster--events))
           (pending lispm-agents-roster--pending-buffers)
           (all (or lispm-agents-roster--all (null lispm-agents-roster--previous)))
           (previous (make-hash-table :test #'equal))
           (by-buffer (make-hash-table :test #'eq))
           (changed (make-hash-table :test #'equal))
           (lispm-agents-roster--index (lispm-agents-roster--build-index (lispm-agents-live-buffers))))
      ;; Clear before running observers: their changes belong to the next batch.
      (setq lispm-agents-roster--keys nil lispm-agents-roster--fields nil
            lispm-agents-roster--events nil lispm-agents-roster--pending-buffers nil
            lispm-agents-roster--all nil)
      (dolist (record lispm-agents-roster--previous)
        (puthash (lispm-agents-roster--identity record) record previous)
        (puthash (plist-get record :buffer) record by-buffer))
      (let ((current (if all (make-hash-table :test #'equal) (copy-hash-table previous))))
        (if all
            (setq pending (lispm-agents-roster--index-buffers lispm-agents-roster--index))
          (dolist (key keys)
            (when-let* ((buffer (or (gethash key (lispm-agents-roster--index-by-key lispm-agents-roster--index))
                                    (plist-get (gethash key previous) :buffer))))
              (cl-pushnew buffer pending))))
        (dolist (buffer pending)
          (when-let* ((old (gethash buffer by-buffer)))
            (let ((id (lispm-agents-roster--identity old)))
              (remhash id current) (puthash id t changed)))
          (unless (eq (gethash buffer (lispm-agents-roster--index-keys lispm-agents-roster--index) 'absent) 'absent)
            (when-let* ((record (lispm-agents-roster--record buffer)))
              (let ((id (lispm-agents-roster--identity record)))
                (puthash id (lispm-inspect--copy-data record) current)
                (puthash id t changed)))))
        (when all (maphash (lambda (id _) (puthash id t changed)) previous))
        (maphash
         (lambda (id _)
           (let ((old (gethash id previous)) (new (gethash id current)))
             (unless (equal old new)
               (when-let* ((key (plist-get (or new old) :key)))
                 (cl-pushnew key keys :test #'equal))
               (if (and old new)
                   (let ((rest new))
                     (while rest
                       (unless (equal (plist-get old (car rest)) (cadr rest))
                         (cl-pushnew (intern (substring (symbol-name (car rest)) 1)) fields))
                       (setq rest (cddr rest))))
                 (cl-pushnew 'agents fields)))))
         changed)
        (setq lispm-agents-roster--previous
              (delq nil (mapcar (lambda (buffer)
                                  (gethash (or (lispm-agents-roster-key buffer) buffer) current))
                                (lispm-agents-roster--index-buffers lispm-agents-roster--index))))
        (when (or keys fields events)
          (run-hook-with-args 'lispm-agents-roster-change-functions keys fields
                              (and events (list :events events))))))))

(defun lispm-agents-roster--changed (&rest _)
  "Compare live agent data after a source announces a global change."
  (when lispm-agents-roster-change-functions (lispm-agents-roster-notify nil '(agents))))

(defun lispm-agents-roster--opened ()
  "Collect the appearance of the current LispM session buffer."
  (when lispm-agents-roster-change-functions
    (lispm-agents-roster-notify (current-buffer) '(agents))))

(defun lispm-agents-roster--state (agent &rest _)
  "Collect AGENT's state, work and mailbox changes."
  (when lispm-agents-roster-change-functions
    (lispm-agents-roster-notify agent '(status running calls mailbox turn))))

(defun lispm-agents-roster--input (agent &rest _)
  "Collect streamed call input from AGENT."
  (when lispm-agents-roster-change-functions (lispm-agents-roster-notify agent '(calls))))

(defun lispm-agents-roster--title (agent &rest _)
  "Collect the change to AGENT's display name."
  (when lispm-agents-roster-change-functions (lispm-agents-roster-notify agent '(name))))

(defun lispm-agents-roster--character (name &rest _)
  "Collect identity changes for the live conversations of character NAME."
  (when lispm-agents-roster-change-functions
    (dolist (agent (lispm-agents))
      (when (eq name (lispm-agent-character agent))
        (lispm-agents-roster-notify agent '(character character-name app-character avatar))))))

(defun lispm-agents-roster--sessions (&rest _)
  "Compare sessions and work after a transcript reader reports newer data."
  (when lispm-agents-roster-change-functions (lispm-agents-roster-notify nil '(agents calls))))

(defun lispm-agents-roster--terminal (&rest _)
  "Report only changes to the current terminal's status or mailbox summary."
  (when lispm-agents-roster-change-functions
    (when (lispm-agents-terminal-agent (current-buffer))
      (let* ((status (lispm-agents-buffer-status (current-buffer)))
             (old lispm-agents-roster--terminal-data)
             (_ (when (and (not (equal status (plist-get old :status)))
                            (memq (plist-get status :state) '(idle done)))
                  (lispm-agents-bridge-observe (current-buffer))))
             (mailbox (lispm-agents-bridge-mailbox-summary (current-buffer)))
             (data (list :status status :mailbox mailbox))
             fields)
        (unless (equal data old)
          (setq lispm-agents-roster--terminal-data (lispm-inspect--copy-data data))
          (when old
            (unless (equal status (plist-get old :status)) (push 'status fields))
            (unless (equal mailbox (plist-get old :mailbox)) (push 'mailbox fields))
            (lispm-agents-roster-notify (current-buffer) fields)))))))

(defun lispm-agents-roster--exited (process)
  "Collect the departure of the agent whose PROCESS exited."
  (when lispm-agents-roster-change-functions
    (lispm-agents-roster--closing (process-buffer process))))

(defun lispm-agents-roster--closing (&optional buffer &rest _)
  "Collect the departure of BUFFER, or the current buffer when absent."
  (when lispm-agents-roster-change-functions
    (let ((buffer (or buffer (current-buffer))))
      (when (and (buffer-live-p buffer) (lispm-agents-buffer-p buffer))
        (lispm-agents-roster-notify buffer '(alive agents))))))

(add-hook 'lispm-state-change-functions #'lispm-agents-roster--state)
(add-hook 'lispm-turn-end-functions #'lispm-agents-roster--state)
(add-hook 'lispm-tool-input-functions #'lispm-agents-roster--input)
(add-hook 'lispm-response-delta-functions #'lispm-agents-roster--input)
(add-hook 'lispm-session-mode-hook #'lispm-agents-roster--opened)
(add-hook 'lispm-session-end-functions #'lispm-agents-roster--state)
(add-hook 'lispm-title-change-functions #'lispm-agents-roster--title)
(add-hook 'lispm-agents-changed-hook #'lispm-agents-roster--changed)
(add-hook 'lispm-agents-sessions-changed-hook #'lispm-agents-roster--changed)
(add-hook 'lispm-agents-sessions-arrived-hook #'lispm-agents-roster--sessions)
(add-hook 'lispm-character-change-functions #'lispm-agents-roster--character)
(add-hook 'lispm-agents-context-release-functions #'lispm-agents-roster--closing)
(add-hook 'eat-update-hook #'lispm-agents-roster--terminal)
(add-hook 'eat-exit-hook #'lispm-agents-roster--exited)
(add-hook 'kill-buffer-hook #'lispm-agents-roster--closing)

(provide 'lispm-agents-roster)
;;; lispm-agents-roster.el ends here
