;;; lispm-office-scope.el --- Roster projection and identity for the Office view  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-agent
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; Bridges `lispm-agents-roster' and `lispm-agents-work' snapshots into
;; normalized scene records for `lispm-ui-scene', resolving buffers and
;; project scopes through public runtime APIs only.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-scene-layout)
(require 'lispm-core)
(require 'lispm-agents-core)
(require 'lispm-agents-terminals)
(require 'lispm-session)
(require 'lispm-agents-roster)
(require 'lispm-chat)
(require 'lispm-agents-work)

(defun lispm-office-scope-normalize (directory)
  "Normalize DIRECTORY string for scope comparison, or return nil."
  (when (and (stringp directory) (not (string-empty-p directory)))
    (directory-file-name (expand-file-name directory))))

(defun lispm-office-scope-label (scope)
  "Return human-readable label for project SCOPE directory or nil."
  (let ((home (directory-file-name (expand-file-name "~"))))
    (cond
     ((null scope) "All Projects")
     ((equal scope home) "Home")
     (t (let ((leaf (file-name-nondirectory scope)))
          (if (string-empty-p leaf) scope leaf))))))

(defun lispm-office-scope-buffer-directory (&optional buffer)
  "Return normalized project directory for BUFFER or nil if none applies."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (lispm-office-scope-normalize
         (or (and (boundp 'lispm-office-scope) lispm-office-scope)
             (and (boundp 'lispm-agents-work-directory) lispm-agents-work-directory)
             (when-let* ((agent (or (and (boundp 'lispm-chat-agent) lispm-chat-agent)
                                    (and (boundp 'lispm-session-agent) lispm-session-agent))))
               (lispm-agent-root agent))
             (when (lispm-agents-buffer-p buffer) default-directory)))))))

(defun lispm-office-scope--record-key (record)
  "Return valid `lispm-ui-scene-key-p' identity for roster RECORD, or nil."
  (let ((key (plist-get record :key))
        (buffer (plist-get record :buffer)))
    (cond
     ((lispm-ui-scene-key-p key) key)
     ((buffer-live-p buffer)
      (list (or (plist-get record :kind)
                (lispm-agents-of-buffer buffer)
                'agent)
            (format "buffer:%s" (buffer-name buffer)))))))

(defun lispm-office-scope--activity-text (record)
  "Return single-line activity summary derived from RECORD."
  (let* ((status (plist-get record :status))
         (msg (plist-get status :msg)))
    (if (and (stringp msg) (not (string-empty-p (string-trim msg))))
        msg
      (pcase (plist-get status :state)
        ('working "Working")
        ('blocked "Waiting for input")
        ('error "Stopped on error")
        ('done "Task completed")
        ('idle "Idle at desk")
        (_ "Active")))))

(defun lispm-office-scope-scene-record (record)
  "Convert roster RECORD into an immutable `lispm-ui-scene' node plist."
  (when-let* ((key (lispm-office-scope--record-key record)))
    (let* ((parent (plist-get record :parent))
           (character (plist-get record :character))
           (char-obj (and character (lispm-get-character character t)))
           (avatar (or (plist-get record :avatar)
                       (and char-obj (lispm-character-avatar char-obj)))))
      (list :key key
            :id (cadr key)
            :kind (or (plist-get record :kind) (car key))
            :name (or (plist-get record :character-name)
                      (plist-get record :name)
                      "Agent")
            :root (lispm-ui-scene-normalize-root (plist-get record :root))
            :parent (and (lispm-ui-scene-key-p parent) parent)
            :waits-on (seq-filter #'lispm-ui-scene-key-p
                                  (plist-get record :waits-on))
            :status (or (plist-get record :status) '(:state idle))
            :mailbox (or (plist-get record :mailbox) '(:queued 0 :unread 0))
            :activity (lispm-office-scope--activity-text record)
            :avatar avatar
            :actions (plist-get record :actions)
            :buffer (plist-get record :buffer)
            :alive (and (plist-get record :alive) t)))))

(defun lispm-office-scope-records (&optional scope snapshot)
  "Return scene records filtered by SCOPE directory from optional SNAPSHOT.
When SCOPE is nil, return all live agents across all projects."
  (let* ((all (or snapshot (lispm-agents-roster)))
         (filtered (if scope
                       (lispm-agents-work-filter all scope)
                     (seq-filter (lambda (r) (plist-get r :alive)) all)))
         (seen (make-hash-table :test #'equal))
         (result nil))
    (dolist (record filtered (nreverse result))
      (when-let* ((scene-rec (lispm-office-scope-scene-record record))
                  (key (plist-get scene-rec :key))
                  ((not (gethash key seen))))
        (puthash key t seen)
        (push scene-rec result)))))

(defun lispm-office-scope-find-record (key &optional records)
  "Find scene record matching KEY in RECORDS (defaulting to current roster)."
  (when key
    (seq-find (lambda (rec) (equal key (plist-get rec :key)))
              (or records (lispm-office-scope-records)))))

(defun lispm-office-scope-live-buffer (key &optional records)
  "Return live primary agent buffer for KEY using RECORDS or `lispm-agents-roster'."
  (let ((rec (lispm-office-scope-find-record key records)))
    (or (let ((buf (plist-get rec :buffer)))
          (and (buffer-live-p buf) buf))
        (let ((buf (lispm-agents-roster-buffer key)))
          (and (buffer-live-p buf) buf)))))

(defun lispm-office-scope-live-agent (key &optional records)
  "Return native `lispm-agent' struct for KEY, or nil if external or ended."
  (when-let* ((buffer (lispm-office-scope-live-buffer key records)))
    (or (buffer-local-value 'lispm-session-agent buffer)
        (buffer-local-value 'lispm-chat-agent buffer))))

(defun lispm-office-scope-conversation-buffer (key &optional records)
  "Return user-facing conversation buffer (chat or terminal) for KEY."
  (when-let* ((buffer (lispm-office-scope-live-buffer key records)))
    (if (eq (lispm-agents-emacs-buffer-agent buffer) 'lispm)
        (if-let* ((agent (lispm-office-scope-live-agent key records)))
            (lispm-chat agent)
          buffer)
      buffer)))

(provide 'lispm-office-scope)
;;; lispm-office-scope.el ends here
