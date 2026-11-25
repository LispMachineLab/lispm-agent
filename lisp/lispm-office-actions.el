;;; lispm-office-actions.el --- Agent actions triggered from Office views  -*- lexical-binding: t; -*-

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

;; Dispatches user actions (open chat, inspect transcript, send message,
;; approve, approve all, deny, abort, view diff) for agents addressed by
;; stable `(KIND SESSION-ID)' key.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-util)
(require 'lispm-core)
(require 'lispm-session)
(require 'lispm-turns)
(require 'lispm-conditions)
(require 'lispm-approval)
(require 'lispm-runtime)
(require 'lispm-agents-core)
(require 'lispm-agents-terminals)
(require 'lispm-chat-view)
(require 'lispm-chat)
(require 'lispm-office-scope)

(defvar lispm-office-action-inspect-function nil
  "Function called with KEY to inspect an agent from the Office view.")

(defun lispm-office-actions--require-agent (key)
  "Return live native `lispm-agent' for KEY, or signal a `user-error'."
  (or (lispm-office-scope-live-agent key)
      (user-error "This action requires a live native LispM agent")))

(defun lispm-office-action-open-chat (key)
  "Show the live conversation buffer for KEY."
  (let ((buffer (lispm-office-scope-conversation-buffer key)))
    (unless (buffer-live-p buffer)
      (user-error "Agent session is no longer active"))
    (lispm-agents-show buffer)
    buffer))

(defun lispm-office-action-open-transcript (key)
  "Open the underlying Org transcript buffer for native agent KEY."
  (let ((agent (lispm-office-actions--require-agent key)))
    (if lispm-open-transcript-function
        (funcall lispm-open-transcript-function agent)
      (lispm-agents-show (lispm-session-buffer agent)))))

(defun lispm-office-action-send-message (key &optional text)
  "Send user message TEXT to KEY, prompting interactively when TEXT is nil."
  (let* ((records (lispm-office-scope-records))
         (rec (lispm-office-scope-find-record key records))
         (buffer (lispm-office-scope-live-buffer key records))
         (agent (lispm-office-scope-live-agent key records))
         (name (or (plist-get rec :name) "Agent"))
         (body (string-trim
                (or text (read-string (format "Message %s: " name))))))
    (when (string-empty-p body)
      (user-error "Message cannot be empty"))
    (cond
     (agent
      (lispm-stop-at-condition agent)
      (lispm-tell agent body)
      (when (buffer-live-p (lispm-agent-view agent))
        (lispm-chat-schedule agent))
      agent)
     ((and (buffer-live-p buffer) (lispm-agents-terminal-p buffer)
           (lispm-agents-alive-p buffer))
      (lispm-agents-terminal-submit buffer body)
      buffer)
     (t (user-error "Agent cannot accept messages right now")))))

(defun lispm-office-action-approve (key)
  "Approve the first pending tool call or condition for native agent KEY."
  (lispm-approve-for (lispm-office-actions--require-agent key)))

(defun lispm-office-action-approve-all (key)
  "Approve every pending permission call in native agent KEY's current turn."
  (lispm-approve-all-for (lispm-office-actions--require-agent key)))

(defun lispm-office-action-deny (key &optional reason)
  "Deny the first pending call for native agent KEY with optional REASON."
  (let ((agent (lispm-office-actions--require-agent key))
        (why (or reason (read-string "Deny reason: "))))
    (lispm-deny-for agent why)))

(defun lispm-office-action-abort (key)
  "Abort the active turn for native agent KEY."
  (lispm-abort (lispm-office-actions--require-agent key)))

(defun lispm-office-action-diff (key)
  "Display the latest turn diff for native agent KEY."
  (lispm-turn-diff (lispm-office-actions--require-agent key)))

(defun lispm-office-action-dispatch (action key &optional inspect-fn)
  "Dispatch scene ACTION for KEY, using optional INSPECT-FN for `inspect'."
  (pcase action
    ('open-chat (lispm-office-action-open-chat key))
    ('transcript (lispm-office-action-open-transcript key))
    ('send (lispm-office-action-send-message key))
    ('approve (lispm-office-action-approve key))
    ('approve-all (lispm-office-action-approve-all key))
    ('deny (lispm-office-action-deny key))
    ('abort (lispm-office-action-abort key))
    ('diff (lispm-office-action-diff key))
    ('inspect
     (let ((fn (or inspect-fn lispm-office-action-inspect-function)))
       (unless fn
         (user-error "Inspector handler is not registered"))
       (funcall fn key)))
    (_ (user-error "Unknown office action: %S" action))))

(provide 'lispm-office-actions)
;;; lispm-office-actions.el ends here
