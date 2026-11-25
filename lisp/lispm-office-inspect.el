;;; lispm-office-inspect.el --- Property-sheet inspector for Office agents  -*- lexical-binding: t; -*-

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

;; Renders an interactive tabbed property sheet (`Overview', `Mailbox',
;; `Calls', `Activity') using `lispm-ui-sheet' over lazy snapshots from
;; `lispm-agents-roster-details'.  Attaches roster hooks only while open.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-controls)
(require 'lispm-ui-scene)
(require 'lispm-ui-sheet)
(require 'lispm-core)
(require 'lispm-agents-roster)
(require 'lispm-inspect)
(require 'lispm-office-scope)
(require 'lispm-office-actions)

(defconst lispm-office-inspect-buffer-name "*Office Inspector*"
  "Name of the reusable Office agent property-sheet buffer.")

(defconst lispm-office-inspect--tabs
  '((overview . "Overview")
    (mailbox . "Mailbox")
    (calls . "Calls")
    (activity . "Activity"))
  "Ordered inspector tabs.")

(defvar lispm-office-inspect--buffers nil
  "Live inspector buffers sharing a single lazy roster observer.")

(defvar-local lispm-office-inspect--key nil
  "Stable (KIND SESSION-ID) key inspected in this buffer.")

(defvar-local lispm-office-inspect--tab 'overview
  "Active inspector tab symbol in this buffer.")

(defvar-local lispm-office-inspect--dirty t
  "Non-nil when this inspector needs redraw when visible.")

(defun lispm-office-inspect--visible ()
  "Redraw dirty visible inspector buffers without polling hidden ones."
  (dolist (buffer lispm-office-inspect--buffers)
    (when (and (buffer-live-p buffer)
               (get-buffer-window buffer 'visible)
               (buffer-local-value 'lispm-office-inspect--dirty buffer))
      (with-current-buffer buffer
        (lispm-office-inspect-refresh)))))

(defun lispm-office-inspect--changed (&rest _args)
  "Mark live inspector buffers dirty on roster changes."
  (dolist (buffer lispm-office-inspect--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lispm-office-inspect--dirty t))))
  (lispm-office-inspect--visible))

(defun lispm-office-inspect--detach ()
  "Remove current buffer from tracking and stop hooks when the last closes."
  (setq lispm-office-inspect--buffers
        (delq (current-buffer) lispm-office-inspect--buffers))
  (unless lispm-office-inspect--buffers
    (remove-hook 'lispm-agents-roster-change-functions #'lispm-office-inspect--changed)
    (remove-hook 'window-state-change-hook #'lispm-office-inspect--visible)))

(defun lispm-office-inspect--require-key ()
  "Return active inspected key in current buffer or signal a `user-error'."
  (or lispm-office-inspect--key
      (user-error "No agent selected in this inspector")))

(defun lispm-office-inspect--insert-kv (label value)
  "Insert aligned property row with LABEL and inert VALUE."
  (insert (propertize (format "%-11s " (concat label ":")) 'face 'shadow)
          (lispm-ui-sheet-oneline value) "\n"))

(defun lispm-office-inspect--insert-actions (record details width)
  "Insert action push buttons for RECORD and DETAILS within WIDTH."
  (let* ((key (plist-get record :key))
         (work (plist-get details :work))
         (pending (plist-get work :pending-calls))
         (state (plist-get (plist-get record :status) :state))
         (kind (plist-get (plist-get record :status) :kind))
         (flow (lispm-ui-sheet-flow-create :width 0))
         (refresh #'lispm-office-inspect-refresh))
    (lispm-ui-sheet-insert-button
     "Chat" #'lispm-office-action-open-chat (list key) width flow)
    (lispm-ui-sheet-insert-button
     "Message" #'lispm-office-action-send-message (list key) width flow refresh)
    (when (eq (car-safe key) 'lispm)
      (when (or pending (and (eq state 'blocked) (memq kind '(permission nil))))
        (lispm-ui-sheet-insert-button
         "Approve" #'lispm-office-action-approve (list key) width flow refresh)
        (lispm-ui-sheet-insert-button
         "Approve All" #'lispm-office-action-approve-all (list key) width flow refresh)
        (lispm-ui-sheet-insert-button
         "Deny" #'lispm-office-action-deny (list key) width flow refresh))
      (when (memq state '(working blocked))
        (lispm-ui-sheet-insert-button
         "Abort" #'lispm-office-action-abort (list key) width flow refresh))
      (lispm-ui-sheet-insert-button
       "Transcript" #'lispm-office-action-open-transcript (list key) width flow)
      (lispm-ui-sheet-insert-button
       "Diff" #'lispm-office-action-diff (list key) width flow))
    (unless (bolp) (insert "\n"))))

(defun lispm-office-inspect--render-overview (record details width)
  "Insert Overview tab body for RECORD and DETAILS within WIDTH."
  (let ((work (plist-get details :work))
        (mailbox (plist-get details :mailbox))
        (waits (plist-get record :waits-on))
        (waiters (plist-get record :waited-on-by)))
    (lispm-ui-sheet-heading "Identity" width)
    (lispm-office-inspect--insert-kv "Kind" (plist-get record :kind))
    (lispm-office-inspect--insert-kv "Session" (plist-get record :id))
    (lispm-office-inspect--insert-kv "Project" (abbreviate-file-name
                                                (or (plist-get record :root) "~")))
    (when-let* ((turn (plist-get work :turn)))
      (lispm-office-inspect--insert-kv "Turn" turn))
    (when-let* ((parent (plist-get record :parent)))
      (lispm-office-inspect--insert-kv "Parent" (format "%S" parent)))
    (when waits
      (lispm-office-inspect--insert-kv
       "Waits on" (mapconcat (lambda (w) (format "%S" w)) waits ", ")))
    (when waiters
      (lispm-office-inspect--insert-kv
       "Waited by" (mapconcat (lambda (w) (format "%S" w)) waiters ", ")))
    (lispm-ui-sheet-heading "Actions" width)
    (lispm-office-inspect--insert-actions record details width)
    (when-let* ((queued (plist-get mailbox :queued))
                ((or (listp queued) (and (numberp queued) (> queued 0)))))
      (lispm-ui-sheet-heading "Inbox" width)
      (insert (format "%d queued message(s)\n"
                      (if (listp queued) (length queued) queued))))))

(defun lispm-office-inspect--render-mailbox (details width)
  "Insert Mailbox tab body from DETAILS within WIDTH."
  (let* ((mailbox (plist-get details :mailbox))
         (handling (plist-get mailbox :handling))
         (queued (plist-get mailbox :queued)))
    (lispm-ui-sheet-heading "Handling" width)
    (if (listp handling)
        (insert (format "%s: %s\n"
                        (or (plist-get handling :from) "User")
                        (lispm-ui-sheet-oneline (plist-get handling :text))))
      (lispm-ui-sheet-empty (if handling "Processing current input" "Idle")))
    (lispm-ui-sheet-heading "Queued" width)
    (if (and (listp queued) queued)
        (dolist (entry queued)
          (insert (format "- [%s] %s\n"
                          (or (plist-get entry :from) "User")
                          (lispm-ui-sheet-oneline (plist-get entry :text)))))
      (lispm-ui-sheet-empty "No queued messages"))))

(defun lispm-office-inspect--render-calls (details width)
  "Insert Calls and Files tab body from DETAILS within WIDTH."
  (let* ((work (plist-get details :work))
         (pending (plist-get work :pending-calls))
         (recent (plist-get work :recent-calls))
         (files (plist-get work :files)))
    (lispm-ui-sheet-heading "Pending Calls" width)
    (if pending
        (dolist (call pending)
          (insert (format "[%s] %s  %s\n"
                          (or (plist-get call :status) 'ask)
                          (lispm-ui-sheet-inert (or (plist-get call :name) "?"))
                          (lispm-ui-sheet-oneline (plist-get call :input-summary)))))
      (lispm-ui-sheet-empty "No pending tool calls"))
    (lispm-ui-sheet-heading "Recent Calls" width)
    (if recent
        (dolist (call recent)
          (insert (format "[%s] %s  %s\n"
                          (or (plist-get call :status) 'done)
                          (lispm-ui-sheet-inert (or (plist-get call :name) "?"))
                          (lispm-ui-sheet-oneline (plist-get call :input-summary)))))
      (lispm-ui-sheet-empty "No recent calls in this turn"))
    (lispm-ui-sheet-heading "Turn Files" width)
    (if files
        (dolist (file files) (insert (format "- %s\n" (lispm-ui-sheet-inert file))))
      (lispm-ui-sheet-empty "No files recorded yet"))))

(defun lispm-office-inspect--render-activity (key details width)
  "Insert Activity summary tab body for KEY and DETAILS within WIDTH."
  (lispm-ui-sheet-heading "Session Summary" width)
  (let ((agent (lispm-office-scope-live-agent key)))
    (if agent
        (insert (lispm-ui-sheet-inert (lispm-inspect-text agent 'compact)) "\n")
      (if-let* ((work (plist-get details :work)))
          (insert (lispm-ui-sheet-inert (format "%S" work)) "\n")
        (lispm-ui-sheet-empty "Detailed summary is only available for native agents")))))

(defun lispm-office-inspect--render ()
  "Redraw the active inspector buffer from current runtime details."
  (let* ((inhibit-read-only t)
         (key lispm-office-inspect--key)
         (record (lispm-office-scope-find-record key))
         (details (and key (lispm-agents-roster-details key)))
         (width (lispm-ui-sheet-available-width 2 16)))
    (setq lispm-office-inspect--dirty nil)
    (erase-buffer)
    (if (null record)
        (progn
          (lispm-ui-sheet-header "Agent Offline" key nil "Session has ended" width)
          (lispm-ui-sheet-empty "Select another agent in the Office view."))
      (lispm-ui-sheet-header (plist-get record :name) record
                             (plist-get record :status)
                             (plist-get record :activity) width)
      (insert "\n")
      (lispm-ui-sheet-tabs lispm-office-inspect--tabs lispm-office-inspect--tab
                           width #'lispm-office-inspect-select-tab)
      (pcase lispm-office-inspect--tab
        ('overview (lispm-office-inspect--render-overview record details width))
        ('mailbox (lispm-office-inspect--render-mailbox details width))
        ('calls (lispm-office-inspect--render-calls details width))
        ('activity (lispm-office-inspect--render-activity key details width))))
    (goto-char (point-min))))

(defun lispm-office-inspect-select-tab (tab)
  "Switch inspector to TAB symbol and redraw."
  (interactive
   (list (intern (completing-read "Tab: " (mapcar (lambda (x) (symbol-name (car x)))
                                                  lispm-office-inspect--tabs)
                                  nil t))))
  (setq lispm-office-inspect--tab tab)
  (lispm-office-inspect--render))

(defun lispm-office-inspect-refresh ()
  "Refresh the current Office inspector buffer."
  (interactive)
  (lispm-office-inspect--render))

(defvar-keymap lispm-office-inspect-mode-map
  :doc "Keymap for `lispm-office-inspect-mode'."
  :parent special-mode-map
  "g" #'lispm-office-inspect-refresh
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "1" (lambda () (interactive) (lispm-office-inspect-select-tab 'overview))
  "2" (lambda () (interactive) (lispm-office-inspect-select-tab 'mailbox))
  "3" (lambda () (interactive) (lispm-office-inspect-select-tab 'calls))
  "4" (lambda () (interactive) (lispm-office-inspect-select-tab 'activity))
  "c" (lambda () (interactive) (lispm-office-action-open-chat (lispm-office-inspect--require-key)))
  "t" (lambda () (interactive) (lispm-office-action-open-transcript (lispm-office-inspect--require-key)))
  "m" (lambda () (interactive)
        (lispm-office-action-send-message (lispm-office-inspect--require-key))
        (lispm-office-inspect-refresh))
  "y" (lambda () (interactive)
        (lispm-office-action-approve (lispm-office-inspect--require-key))
        (lispm-office-inspect-refresh))
  "x" (lambda () (interactive)
        (lispm-office-action-abort (lispm-office-inspect--require-key))
        (lispm-office-inspect-refresh))
  "d" (lambda () (interactive) (lispm-office-action-diff (lispm-office-inspect--require-key))))

(define-derived-mode lispm-office-inspect-mode special-mode "Office-Inspector"
  "Interactive property sheet inspecting one agent in the Office."
  (setq-local truncate-lines t
              cursor-type nil)
  (add-hook 'post-command-hook #'lispm-ui-show-focus nil t)
  (add-hook 'kill-buffer-hook #'lispm-office-inspect--detach nil t)
  (add-hook 'change-major-mode-hook #'lispm-office-inspect--detach nil t)
  (cl-pushnew (current-buffer) lispm-office-inspect--buffers)
  (add-hook 'lispm-agents-roster-change-functions #'lispm-office-inspect--changed)
  (add-hook 'window-state-change-hook #'lispm-office-inspect--visible))

;;;###autoload
(defun lispm-office-inspect-open (key &optional tab)
  "Display or update the Office inspector property sheet for KEY and TAB."
  (let ((buffer (get-buffer-create lispm-office-inspect-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-office-inspect-mode)
        (lispm-office-inspect-mode))
      (setq lispm-office-inspect--key key
            lispm-office-inspect--tab (or tab lispm-office-inspect--tab 'overview))
      (lispm-office-inspect--render))
    (display-buffer buffer)
    buffer))

(defun lispm-office-inspect-sync (key)
  "Update any visible Office inspector to reflect newly selected KEY."
  (when-let* ((buffer (get-buffer lispm-office-inspect-buffer-name))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-office-inspect-mode)
        (setq lispm-office-inspect--key key)
        (if (get-buffer-window buffer 'visible)
            (lispm-office-inspect--render)
          (setq lispm-office-inspect--dirty t))))))

(defun lispm-office-inspect-unload-function ()
  "Close live inspector buffers and remove roster hooks on unload."
  (dolist (buffer (copy-sequence lispm-office-inspect--buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (lispm-office-inspect--detach)
        (fundamental-mode))))
  nil)

(setq lispm-office-action-inspect-function #'lispm-office-inspect-open)

(provide 'lispm-office-inspect)
;;; lispm-office-inspect.el ends here
