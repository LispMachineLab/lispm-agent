;;; lispm-desktop-lispm.el --- LispM agents beside Claude Code, Codex and Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
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

;; LispM runs agents inside Emacs rather than as CLIs in terminals.  You
;; talk to one in a chat buffer laid out like Claude Code's; its session is
;; stored as an org document out of sight (/transcript shows it).  This
;; module registers it with the agent tools of lispm-desktop-ai, so it works as the
;; others do:
;;
;; - C-c a l (and l in the agents sidebar) starts a LispM agent at the
;;   current project root; C-u C-c a l asks for its role;
;; - its sessions join the session list (C-c a s) and the agents sidebar,
;;   grouped by project, and resume from there;
;; - C-c a r puts a reference to the region into its input, as for any
;;   running agent;
;; - its chats are laid out as terminals, on the left of the main area,
;;   with the files they open on the right.
;;
;; LispM is the one in this repository, lisp/ beside desktop/, unless a
;; copy of it is already on `load-path'.

;;; Code:

(require 'lispm-desktop-ai)
(require 'package)

(defvar lispm-desktop-agent-map)
(defvar lispm-desktop-conversation-modes)
(defvar lispm-desktop-agent-list-mode-map)
(defvar lispm-interface)
(defvar lispm-chat-agent)
(defvar lispm-session-agent)
(declare-function lispm-desktop-agent-list-refresh "lispm-desktop-agent-list" ())
(declare-function lispm-sessions "lispm-runtime" ())
(declare-function lispm-agent-display-buffer "lispm" (agent))
(declare-function lispm-start "lispm-runtime" (&optional role root))
(declare-function lispm-resume-file "lispm-runtime" (file))
(declare-function lispm-rename-session "lispm-runtime" (file title))
(declare-function lispm-session-files "lispm-runtime" (id))
(declare-function lispm-agent-id "lispm-core" (agent))
(declare-function lispm-agent-name "lispm-core" (agent))
(declare-function lispm-agent-view "lispm-core" (agent))
(declare-function lispm--read-role "lispm" (prompt))
(declare-function lispm-mode-line-mode "lispm" (&optional arg))

(defconst lispm-desktop-lispm-directory
  (expand-file-name "../lisp" lispm-desktop-directory)
  "LispM's sources: lisp/ of the repository holding the desktop.")

(defun lispm-desktop-lispm--requirements (file)
  "Return the packages FILE, a package's main file, requires and lacks."
  (seq-remove (lambda (dep)
                (or (eq (car dep) 'emacs)
                    (package-installed-p (car dep) (cadr dep))))
              (package-desc-reqs
               (with-temp-buffer
                 (insert-file-contents file)
                 (package-buffer-info)))))

(defun lispm-desktop-lispm-ensure ()
  "Make LispM loadable, installing what it depends on when missing.
A copy already on `load-path' is used as it is; else the repository's
own, `lispm-desktop-lispm-directory', joins it.  The packages LispM's
Package-Requires names are installed as package.el would install them
for the package."
  (unless (locate-library "lispm.el" t)
    (add-to-list 'load-path lispm-desktop-lispm-directory))
  (when-let* ((file (locate-library "lispm.el" t))
              (missing (lispm-desktop-lispm--requirements file)))
    (package-refresh-contents)
    (dolist (dep missing)
      (package-install (car dep)))))

;;;; Sessions as lispm-desktop-ai sees them

(defun lispm-desktop-lispm--session (session)
  "Return the LispM SESSION description as a lispm-desktop-ai session plist."
  (list :agent 'lispm
        :id (plist-get session :id)
        :cwd (plist-get session :root)
        :title (plist-get session :title)
        :time (plist-get session :time)
        :file (plist-get session :file)))

(defun lispm-desktop-lispm-sessions ()
  "Return every recorded LispM session as lispm-desktop-ai session plists."
  (mapcar #'lispm-desktop-lispm--session (lispm-sessions)))

(defun lispm-desktop-lispm--compose (buffer)
  "Put point where BUFFER's next message is written; return BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max)))
  buffer)

(defun lispm-desktop-lispm-new (directory &optional role)
  "Start a LispM agent of ROLE in DIRECTORY; return its chat."
  (lispm-desktop-lispm--compose (lispm-agent-display-buffer (lispm-start role directory))))

(defun lispm-desktop-lispm-resume (session)
  "Resume the LispM SESSION; return its chat."
  (lispm-desktop-lispm--compose
   (lispm-agent-display-buffer (lispm-resume-file (plist-get session :file)))))

(defun lispm-desktop-lispm-buffer-p (buffer)
  "Return non-nil when BUFFER is where a live LispM agent is talked to."
  (and (buffer-live-p buffer)
       (if (eq lispm-interface 'chat)
           (and (eq (buffer-local-value 'major-mode buffer) 'lispm-chat-mode)
                (buffer-local-value 'lispm-chat-agent buffer))
         (and (buffer-local-value 'lispm-session-mode buffer)
              (buffer-local-value 'lispm-session-agent buffer)))
       t))

(defun lispm-desktop-lispm-send (buffer text)
  "Put TEXT at the end of the input of LispM BUFFER; RET sends it."
  (with-current-buffer buffer
    (goto-char (point-max))
    (unless (or (bolp) (eq major-mode 'lispm-chat-mode)) (insert "\n"))
    (insert text)))

(defun lispm-desktop-lispm-rename (session title)
  "Give the LispM SESSION the TITLE shown in session lists."
  (lispm-rename-session (plist-get session :file) title))

(defun lispm-desktop-lispm-files (session)
  "Return the files recording the LispM SESSION."
  (lispm-session-files (plist-get session :id)))

;;;; Buffers

(defun lispm-desktop-lispm--buffer-agent ()
  "The LispM agent of the current buffer, if it is where one is talked to."
  (and (lispm-desktop-lispm-buffer-p (current-buffer))
       (if (eq major-mode 'lispm-chat-mode) lispm-chat-agent lispm-session-agent)))

(defun lispm-desktop-lispm--tag-buffer ()
  "Tag a LispM chat or session buffer: its session id, title and layout.
lispm-desktop-ai reads them."
  (when-let* ((agent (lispm-desktop-lispm--buffer-agent)))
    (setq lispm-desktop-agent-buffer-session-id (lispm-agent-id agent)
          lispm-desktop-agent-buffer-title (or lispm-desktop-agent-buffer-title
                                               (symbol-name (lispm-agent-name agent))))
    (setq-local lispm-desktop-conversation-buffer t)))

(defvar lispm-desktop-lispm--refresh-timer nil
  "Pending refresh of the agents sidebar.")

(defun lispm-desktop-lispm--changed (agent)
  "Keep AGENT's buffer tagged, and refresh the agents sidebar soon."
  (let ((view (lispm-agent-view agent)))
    ;; /clear gives a standing agent a new session in the same chat.
    (when (buffer-live-p view)
      (with-current-buffer view (lispm-desktop-lispm--tag-buffer))))
  (unless (timerp lispm-desktop-lispm--refresh-timer)
    (setq lispm-desktop-lispm--refresh-timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (setq lispm-desktop-lispm--refresh-timer nil)
                                 (when (fboundp 'lispm-desktop-agent-list-refresh)
                                   (lispm-desktop-agent-list-refresh)))))))

;;;; Commands

(defun lispm-desktop-agent-lispm-new (&optional role)
  "Start a new LispM agent at the current project root.
With a prefix argument, choose its ROLE."
  (interactive (list (and current-prefix-arg (lispm--read-role "Role"))))
  (if role
      (lispm-desktop-agent--show (lispm-desktop-lispm-new (with-selected-window (lispm-desktop-agent--main-window)
                                                          (lispm-desktop-agent--project-directory))
                                              role))
    (lispm-desktop-agent-new 'lispm)))

(defun lispm-desktop-setup-lispm ()
  "Load LispM and register its agents with the agent tools."
  (lispm-desktop-lispm-ensure)
  (require 'lispm)
  (setf (alist-get 'lispm lispm-desktop-agent-emacs-agents)
        (list :name "LispM"
              :icon '(lispm-desktop-agent-image-icon "lispm-lambda.png" nil)
              :command #'lispm-desktop-agent-lispm-new
              :sessions #'lispm-desktop-lispm-sessions
              :new #'lispm-desktop-lispm-new
              :resume #'lispm-desktop-lispm-resume
              :buffer-p #'lispm-desktop-lispm-buffer-p
              :send #'lispm-desktop-lispm-send
              :rename #'lispm-desktop-lispm-rename
              :files #'lispm-desktop-lispm-files))

  ;; A chat keeps to the session column from the moment it is created.
  (add-to-list 'lispm-desktop-conversation-modes 'lispm-chat-mode)
  (add-hook 'lispm-chat-mode-hook #'lispm-desktop-lispm--tag-buffer)
  (add-hook 'lispm-session-mode-hook #'lispm-desktop-lispm--tag-buffer)
  (add-hook 'lispm-state-change-functions #'lispm-desktop-lispm--changed)
  (add-hook 'lispm-title-change-functions #'lispm-desktop-lispm--changed)
  (keymap-set lispm-desktop-agent-map "l" #'lispm-desktop-agent-lispm-new)
  (with-eval-after-load 'lispm-desktop-agent-list
    (keymap-set lispm-desktop-agent-list-mode-map "l" #'lispm-desktop-agent-lispm-new))
  (lispm-mode-line-mode 1))

(provide 'lispm-desktop-lispm)
;;; lispm-desktop-lispm.el ends here
