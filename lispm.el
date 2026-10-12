;;; lispm.el --- LispM: a Lisp Machine for agents, inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LispM runs AI agents inside Emacs instead of beside it.  The parts
;; an agent harness usually provides are Emacs objects here:
;;
;;   context        an org buffer (lispm-transcript.el), read on every
;;                  request, so editing the buffer edits the context;
;;   tools          Emacs Lisp functions over buffers, files, processes
;;                  and Emacs itself (lispm-tools.el);
;;   permissions    tool calls are org TODO entries: WAITING until the
;;                  user changes their state (lispm-runtime.el);
;;   memory, tasks  org files under git (lispm-memory.el);
;;   execution      the Emacs event loop: processes, hooks and timers
;;                  wake agents up (lispm-runtime.el);
;;   collaboration  agents message and delegate to each other
;;                  (lispm-collab.el);
;;   audit, undo    every edit is an undo step and a journal entry
;;                  (`lispm-turn-diff', `lispm-revert-turn'); every turn
;;                  is a git commit.
;;
;; Start with M-x lispm.  See README.org and DESIGN.org.

;;; Code:

(require 'tabulated-list)
(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-memory)
(require 'lispm-runtime)
(require 'lispm-collab)

(add-hook 'org-mode-hook #'lispm-session-maybe-enable)

(lispm-defagent assistant
  "A general assistant for whatever the user is doing in Emacs."
  :root "~/")

(defvar lispm--last-agent nil "Name of the agent last chosen.")

(defun lispm--read-agent (prompt)
  "Read an agent name with PROMPT."
  (unless lispm--agents (user-error "No agents; define one with `lispm-defagent'"))
  (let* ((names (mapcar (lambda (a) (symbol-name (lispm-agent-name a))) (lispm-agents)))
         (default (if (member lispm--last-agent names) lispm--last-agent (car names)))
         (name (completing-read (format-prompt prompt default) names nil t nil nil default)))
    (setq lispm--last-agent name)
    (lispm-get-agent name)))

(defun lispm--current-or-read (prompt)
  "The agent of the current session buffer, or one read with PROMPT."
  (or lispm-session-agent (lispm--read-agent prompt)))

;;;###autoload
(defun lispm (agent)
  "Switch to AGENT's session, starting one if needed."
  (interactive (list (lispm--read-agent "Agent")))
  (pop-to-buffer (lispm-session-buffer (lispm-get-agent agent)))
  (goto-char (point-max)))

;;;###autoload
(defun lispm-new-session (agent)
  "Start a fresh session of AGENT; the old one stays on disk."
  (interactive (list (lispm--current-or-read "New session of")))
  (pop-to-buffer (lispm-session-buffer (lispm-get-agent agent) t))
  (goto-char (point-max)))

;;;###autoload
(defun lispm-ask (agent text)
  "Give AGENT the request TEXT without leaving the current buffer."
  (interactive (let ((agent (lispm--read-agent "Ask agent")))
                 (list agent (read-string (format "Ask %s: " (lispm-agent-name agent))))))
  (lispm-tell agent text)
  (display-buffer (lispm-agent-buffer (lispm-get-agent agent))))

;;;###autoload
(defun lispm-ask-region (agent beg end text)
  "Ask AGENT about the region from BEG to END: TEXT plus the region."
  (interactive (let ((agent (lispm--read-agent "Ask agent")))
                 (list agent (region-beginning) (region-end)
                       (read-string (format "Ask %s about the region: "
                                            (lispm-agent-name agent))))))
  (lispm-ask agent
             (format "%s\n\nThis concerns lines %d-%d of %s:\n\n%s"
                     text (line-number-at-pos beg) (line-number-at-pos end)
                     (if buffer-file-name
                         (format "the file %s" (abbreviate-file-name buffer-file-name))
                       (format "the buffer %s" (buffer-name)))
                     (buffer-substring-no-properties beg end))))

(defun lispm-show-request (agent)
  "Show the request AGENT would send next: exactly what the model sees."
  (interactive (list (lispm--current-or-read "Show request of")))
  (let* ((agent (lispm-get-agent agent))
         (request (condition-case err
                      (lispm-request-body agent)
                    (user-error
                     ;; Show what there is even if it cannot be sent yet.
                     (let ((parsed (with-current-buffer (lispm-session-buffer agent)
                                     (lispm-transcript-parse))))
                       (cons `((note . ,(error-message-string err))
                               (system . ,(or (plist-get parsed :system) ""))
                               (messages . ,(vconcat (plist-get parsed :messages))))
                             nil))))))
    (with-current-buffer (get-buffer-create (format "*lispm-request: %s*" (lispm-agent-name agent)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (cdr request)
          (insert (format "// anthropic-beta: %s\n" (string-join (cdr request) ","))))
        (insert (lispm--json-pretty (car request)) "\n"))
      (if (fboundp 'js-json-mode) (js-json-mode) (js-mode))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun lispm-history ()
  "Show the git log of all agent memory, tasks and sessions."
  (interactive)
  (let ((default-directory (file-name-as-directory (expand-file-name lispm-directory))))
    (unless (file-directory-p ".git") (user-error "No history yet"))
    (if (fboundp 'magit-log-current)
        (magit-log-current nil)
      (vc-print-root-log))))

(defun lispm-agenda-files ()
  "Task files of all agents, for `org-agenda-files'."
  (mapcar #'lispm-tasks-file (lispm-agents)))

(defun lispm-agenda ()
  "Show the tool calls awaiting approval in every live session."
  (interactive)
  (let ((org-agenda-files
         (delq nil (mapcar (lambda (a)
                             (let ((b (lispm-agent-buffer a)))
                               (and (buffer-live-p b) (buffer-file-name b))))
                           (lispm-agents)))))
    (unless org-agenda-files (user-error "No live sessions"))
    (mapc (lambda (a) (lispm--save-session a)) (lispm-agents))
    (org-todo-list "WAITING")))

;;;; Dashboard

(defun lispm--dashboard-entries ()
  "Rows of the agent dashboard."
  (mapcar
   (lambda (a)
     (let ((waiting (length (lispm--waiting-calls a)))
           (usage (lispm-agent-usage a)))
       (list (lispm-agent-name a)
             (vector (symbol-name (lispm-agent-name a))
                     (propertize (symbol-name (lispm-agent-state a))
                                 'face (if (> waiting 0) 'warning 'default))
                     (or (lispm-agent-status a) "")
                     (if (> waiting 0) (number-to-string waiting) "")
                     (let ((n (length (lispm-agent-inbox a)))) (if (> n 0) (number-to-string n) ""))
                     (format "%d/%d/%d" (nth 0 usage) (nth 1 usage) (nth 2 usage))
                     (lispm-agent-model* a)
                     (or (lispm-agent-description a) "")))))
   (lispm-agents)))

(defvar-keymap lispm-dashboard-mode-map
  :doc "Keymap of `lispm-dashboard-mode'."
  "RET" #'lispm-dashboard-visit
  "k" #'lispm-dashboard-abort
  "g" #'revert-buffer)

(define-derived-mode lispm-dashboard-mode tabulated-list-mode "λ Agents"
  "List of agents and what they are doing."
  (setq tabulated-list-format [("Agent" 12 t) ("State" 10 t) ("Activity" 28 nil)
                               ("Wait" 5 t) ("Inbox" 6 t) ("Tokens in/out/cached" 22 nil)
                               ("Model" 16 t) ("Role" 0 nil)]
        tabulated-list-entries #'lispm--dashboard-entries)
  (tabulated-list-init-header))

(defun lispm-dashboard ()
  "Show all agents and what they are doing."
  (interactive)
  (with-current-buffer (get-buffer-create "*lispm-agents*")
    (lispm-dashboard-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun lispm-dashboard-visit ()
  "Visit the session of the agent at point."
  (interactive)
  (lispm (tabulated-list-get-id)))

(defun lispm-dashboard-abort ()
  "Abort the turn of the agent at point."
  (interactive)
  (lispm-abort (tabulated-list-get-id))
  (tabulated-list-print t))

(defun lispm--refresh-dashboard (&rest _)
  "Refresh the dashboard if it is shown."
  (when-let* ((buffer (get-buffer "*lispm-agents*")))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer (tabulated-list-print t)))))

(add-hook 'lispm-state-change-functions #'lispm--refresh-dashboard)

;;;; Mode line

(defun lispm--mode-line ()
  "Mode line summary of the agents that are not idle."
  (let ((busy (seq-filter #'lispm-agent-busy-p (lispm-agents))))
    (when busy
      (concat " λ["
              (mapconcat (lambda (a)
                           (let ((waiting (eq (lispm-agent-state a) 'waiting)))
                             (propertize (format "%s%s" (lispm-agent-name a) (if waiting "!" "…"))
                                         'face (if waiting 'warning 'shadow))))
                         busy " ")
              "]"))))

;;;###autoload
(define-minor-mode lispm-mode-line-mode
  "Show working agents in the mode line; \"!\" marks one awaiting approval."
  :global t
  :group 'lispm
  (let ((entry '(:eval (lispm--mode-line))))
    (unless (listp global-mode-string)
      (setq global-mode-string (list global-mode-string)))
    (if lispm-mode-line-mode
        (add-to-list 'global-mode-string entry t)
      (setq global-mode-string (delete entry global-mode-string)))))

(provide 'lispm)
;;; lispm.el ends here
