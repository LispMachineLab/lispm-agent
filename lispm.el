;;; lispm.el --- LispM: a Lisp Machine for agents, inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (mcp "0.1"))
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

;; LispM runs AI agents inside Emacs instead of beside it.  They are
;; used as Claude Code and Codex are: M-x lispm starts an agent in the
;; current project, one conversation each, several at once if you like,
;; and M-x lispm-resume brings back any recorded session.  The parts an
;; agent harness usually provides are Emacs objects here:
;;
;;   agents         one session of a role in a project (lispm-core.el);
;;                  long ones are handed over to a summary (lispm-runtime.el);
;;   context        an org buffer (lispm-transcript.el), read on every
;;                  request, so editing the buffer edits the context;
;;   interface      a chat laid out like Claude Code's, drawn from that
;;                  org buffer (lispm-chat.el);
;;   tools          Emacs Lisp functions over buffers, files, processes
;;                  and Emacs itself (lispm-tools.el);
;;   permissions    tool calls are org TODO entries: WAITING until the
;;                  user changes their state (lispm-runtime.el);
;;   memory, tasks  org files under git, per project and per role
;;                  (lispm-memory.el);
;;   execution      the Emacs event loop: processes, hooks and timers
;;                  wake agents up (lispm-runtime.el);
;;   collaboration  agents message and delegate to each other
;;                  (lispm-collab.el);
;;   MCP            the tools of Model Context Protocol servers, such as
;;                  Peekaboo's for using the computer (lispm-mcp.el);
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
(require 'lispm-mcp)
(require 'lispm-title)
(require 'lispm-chat)

(add-hook 'org-mode-hook #'lispm-session-maybe-enable)

;;;; Roles

(lispm-defrole assistant
  "A general assistant for whatever the user is doing in Emacs.")

;;;; Starting, resuming and switching

(defcustom lispm-interface 'chat
  "How LispM shows you an agent.
`chat' is a conversation laid out like Claude Code's terminal
interface, drawn from the session's org document, which stays out of
sight (/transcript shows it).  `org' shows that org document itself:
you write under its last heading and send with C-c RET."
  :type '(choice (const :tag "Chat" chat) (const :tag "Org transcript" org))
  :group 'lispm)

(defun lispm-agent-display-buffer (agent)
  "The buffer showing AGENT, as `lispm-interface' says."
  (if (eq lispm-interface 'chat)
      (lispm-chat agent)
    (lispm-session-buffer agent)))

(defun lispm-show (agent)
  "Show AGENT, ready for your next message; return its buffer."
  (let ((buffer (lispm-agent-display-buffer agent)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    buffer))

(defvar lispm--last-role nil "Name of the role last chosen.")

(defun lispm--read-role (prompt)
  "Read a role name with PROMPT; return the symbol."
  (let* ((names (mapcar (lambda (r) (symbol-name (lispm-role-name r))) (lispm-roles)))
         (default (if (member lispm--last-role names) lispm--last-role
                    (symbol-name lispm-default-role))))
    (setq lispm--last-role (completing-read (format-prompt prompt default) names nil t nil nil default))
    (intern lispm--last-role)))

(defun lispm--read-agent (prompt)
  "Read the name of a running agent with PROMPT; return the agent."
  (let ((names (mapcar (lambda (a) (symbol-name (lispm-agent-name a))) (lispm-agents))))
    (unless names (user-error "No agent is running; start one with M-x lispm"))
    (lispm-get-agent (completing-read prompt names nil t))))

(defun lispm--current-or-read (prompt)
  "The agent of the current session buffer, or one read with PROMPT."
  (or lispm-session-agent lispm-chat-agent (lispm--read-agent prompt)))

(defun lispm--project-agent ()
  "A running agent working in the current project, the most recent first."
  (let ((root (file-name-as-directory (expand-file-name (lispm--project-root)))))
    (seq-find (lambda (a) (equal (lispm-agent-root a) root)) (reverse (lispm-agents)))))

;;;###autoload
(defun lispm (&optional role directory)
  "Start a new agent in the current project, as starting Claude Code there would.
The agent has `lispm-default-role'; with a prefix argument, choose
ROLE.  DIRECTORY defaults to the current project's root."
  (interactive (list (and current-prefix-arg (lispm--read-role "Role"))))
  (lispm-show (lispm-start role directory)))

;;;###autoload
(defun lispm-resume (session)
  "Resume a recorded SESSION, sessions of the current project first."
  (interactive (list (lispm--read-session "Resume session: ")))
  (lispm-show (lispm-resume-file (plist-get session :file))))

(defun lispm--age (time)
  "A short age for TIME, float seconds."
  (let ((seconds (- (float-time) time)))
    (cond ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(defun lispm--read-session (prompt)
  "Read a recorded session with PROMPT; return its description."
  (let* ((root (file-name-as-directory (expand-file-name (lispm--project-root))))
         (sessions (lispm-sessions))
         (sessions (append (seq-filter (lambda (s) (equal (file-name-as-directory (plist-get s :root)) root)) sessions)
                           (seq-remove (lambda (s) (equal (file-name-as-directory (plist-get s :root)) root)) sessions)))
         (table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (s)
                           (let ((label (format "%-16s %-5s %s  %s"
                                                (truncate-string-to-width
                                                 (lispm--project-name (plist-get s :root)) 16)
                                                (lispm--age (plist-get s :time))
                                                (lispm--oneline (plist-get s :title) 60)
                                                (propertize (plist-get s :id) 'face 'shadow))))
                             (puthash label s table)
                             label))
                         sessions)))
    (unless labels (user-error "No recorded sessions"))
    (gethash (completing-read prompt
                              (lambda (string pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity))
                                  (complete-with-action action labels string pred)))
                              nil t)
             table)))

;;;###autoload
(defun lispm-switch (agent)
  "Switch to the session of a running AGENT."
  (interactive (list (lispm--read-agent "Switch to agent: ")))
  (lispm-show agent))

(defun lispm-new-session ()
  "Start a fresh session like the current one: same role, same project.
A standing agent starts a new conversation; otherwise a new agent starts."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (cond ((eq lispm-interface 'chat) (lispm-chat-clear agent nil))
          ((lispm-agent-standing agent)
           (pop-to-buffer (lispm-session-buffer agent t)))
          (t (lispm (lispm-agent-role agent) (lispm-agent-root agent))))))

;;;###autoload
(defun lispm-ask (agent text)
  "Give AGENT the request TEXT without leaving the current buffer.
AGENT is the running agent of the current project, or a new one."
  (interactive (let ((agent (or (lispm--project-agent) (lispm-start))))
                 (list agent (read-string (format "Ask %s: " (lispm-agent-name agent))))))
  (lispm-tell agent text)
  (display-buffer (lispm-agent-display-buffer agent)))

;;;###autoload
(defun lispm-ask-region (beg end text)
  "Ask the current project's agent about the region from BEG to END."
  (interactive (list (region-beginning) (region-end)
                     (read-string "Ask about the region: ")))
  (lispm-ask (or (lispm--project-agent) (lispm-start))
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
  "Task files of all projects, for `org-agenda-files'."
  (file-expand-wildcards (expand-file-name "projects/*/tasks.org" lispm-directory)))

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
                     (symbol-name (lispm-agent-role a))
                     (lispm--project-name (lispm-agent-root a))
                     (propertize (symbol-name (lispm-agent-state a))
                                 'face (if (> waiting 0) 'warning 'default))
                     (or (lispm-agent-status a) "")
                     (if (> waiting 0) (number-to-string waiting) "")
                     (let ((n (length (lispm-agent-inbox a)))) (if (> n 0) (number-to-string n) ""))
                     (format "%d/%d/%d" (nth 0 usage) (nth 1 usage) (nth 2 usage))
                     (or (lispm-agent-model* a) "")
                     (or (lispm-agent-description a) "")))))
   (lispm-agents)))

(defvar-keymap lispm-dashboard-mode-map
  :doc "Keymap of `lispm-dashboard-mode'."
  "RET" #'lispm-dashboard-visit
  "k" #'lispm-dashboard-abort
  "g" #'revert-buffer)

(define-derived-mode lispm-dashboard-mode tabulated-list-mode "λ Agents"
  "List of agents and what they are doing."
  (setq tabulated-list-format [("Agent" 20 t) ("Role" 10 t) ("Project" 14 t)
                               ("State" 10 t) ("Activity" 28 nil)
                               ("Wait" 5 t) ("Inbox" 6 t) ("Tokens in/out/cached" 22 nil)
                               ("Model" 16 t) ("Description" 0 nil)]
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
  (lispm-switch (lispm-get-agent (tabulated-list-get-id))))

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
