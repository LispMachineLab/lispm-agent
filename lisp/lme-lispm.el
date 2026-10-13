;;; lme-lispm.el --- LispM agents beside Claude Code, Codex and Hermes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm-emacs
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
;; module registers it with the agent tools of lme-ai, so it works as the
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
;; LispM comes from wherever it is found: a checkout already on
;; `load-path', or else the package installed from `lme-lispm-url'.

;;; Code:

(require 'lme-ai)
(require 'package)

(defvar lme-agent-map)
(defvar lme-conversation-modes)
(defvar lme-agent-list-mode-map)
(defvar lispm-interface)
(defvar lispm-chat-agent)
(defvar lispm-session-agent)
(declare-function lme-agent-list-refresh "lme-agent-list" ())
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

(defcustom lme-lispm-url "https://github.com/RyanKung/lispm"
  "Where LispM is installed from when no copy of it is on `load-path'."
  :type 'string
  :group 'lispm-emacs)

(defun lme-lispm--requirements (file)
  "Return the packages FILE, a package's main file, requires and lacks."
  (seq-remove (lambda (dep)
                (or (eq (car dep) 'emacs)
                    (package-installed-p (car dep) (cadr dep))))
              (package-desc-reqs
               (with-temp-buffer
                 (insert-file-contents file)
                 (package-buffer-info)))))

(defun lme-lispm-ensure ()
  "Make LispM loadable, installing it or what it depends on when missing.
A checkout on `load-path' is used as it is, and the packages its
Package-Requires names are installed as package.el would install them
for the package; with no copy anywhere, LispM is installed from
`lme-lispm-url'."
  (if-let* ((file (locate-library "lispm.el" t)))
      (when-let* ((missing (lme-lispm--requirements file)))
        (package-refresh-contents)
        (dolist (dep missing)
          (package-install (car dep))))
    (package-vc-install lme-lispm-url)))

;;;; Sessions as lme-ai sees them

(defun lme-lispm--session (session)
  "Return the LispM SESSION description as an lme-ai session plist."
  (list :agent 'lispm
        :id (plist-get session :id)
        :cwd (plist-get session :root)
        :title (plist-get session :title)
        :time (plist-get session :time)
        :file (plist-get session :file)))

(defun lme-lispm-sessions ()
  "Return every recorded LispM session as lme-ai session plists."
  (mapcar #'lme-lispm--session (lispm-sessions)))

(defun lme-lispm--compose (buffer)
  "Put point where BUFFER's next message is written; return BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max)))
  buffer)

(defun lme-lispm-new (directory &optional role)
  "Start a LispM agent of ROLE in DIRECTORY; return its chat."
  (lme-lispm--compose (lispm-agent-display-buffer (lispm-start role directory))))

(defun lme-lispm-resume (session)
  "Resume the LispM SESSION; return its chat."
  (lme-lispm--compose
   (lispm-agent-display-buffer (lispm-resume-file (plist-get session :file)))))

(defun lme-lispm-buffer-p (buffer)
  "Return non-nil when BUFFER is where a live LispM agent is talked to."
  (and (buffer-live-p buffer)
       (if (eq lispm-interface 'chat)
           (and (eq (buffer-local-value 'major-mode buffer) 'lispm-chat-mode)
                (buffer-local-value 'lispm-chat-agent buffer))
         (and (buffer-local-value 'lispm-session-mode buffer)
              (buffer-local-value 'lispm-session-agent buffer)))
       t))

(defun lme-lispm-send (buffer text)
  "Put TEXT at the end of the input of LispM BUFFER; RET sends it."
  (with-current-buffer buffer
    (goto-char (point-max))
    (unless (or (bolp) (eq major-mode 'lispm-chat-mode)) (insert "\n"))
    (insert text)))

(defun lme-lispm-rename (session title)
  "Give the LispM SESSION the TITLE shown in session lists."
  (lispm-rename-session (plist-get session :file) title))

(defun lme-lispm-files (session)
  "Return the files recording the LispM SESSION."
  (lispm-session-files (plist-get session :id)))

;;;; Buffers

(defun lme-lispm--buffer-agent ()
  "The LispM agent of the current buffer, if it is where one is talked to."
  (and (lme-lispm-buffer-p (current-buffer))
       (if (eq major-mode 'lispm-chat-mode) lispm-chat-agent lispm-session-agent)))

(defun lme-lispm--tag-buffer ()
  "Tag a LispM chat or session buffer for lme-ai: its session id, title and layout."
  (when-let* ((agent (lme-lispm--buffer-agent)))
    (setq lme-agent-buffer-session-id (lispm-agent-id agent)
          lme-agent-buffer-title (or lme-agent-buffer-title
                                    (symbol-name (lispm-agent-name agent))))
    (setq-local lme-conversation-buffer t)))

(defvar lme-lispm--refresh-timer nil
  "Pending refresh of the agents sidebar.")

(defun lme-lispm--changed (agent)
  "Keep AGENT's buffer tagged, and refresh the agents sidebar soon."
  (let ((view (lispm-agent-view agent)))
    ;; /clear gives a standing agent a new session in the same chat.
    (when (buffer-live-p view)
      (with-current-buffer view (lme-lispm--tag-buffer))))
  (unless (timerp lme-lispm--refresh-timer)
    (setq lme-lispm--refresh-timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (setq lme-lispm--refresh-timer nil)
                                 (when (fboundp 'lme-agent-list-refresh)
                                   (lme-agent-list-refresh)))))))

;;;; Commands

(defun lme-agent-lispm-new (&optional role)
  "Start a new LispM agent at the current project root.
With a prefix argument, choose its ROLE."
  (interactive (list (and current-prefix-arg (lispm--read-role "Role"))))
  (if role
      (lme-agent--show (lme-lispm-new (with-selected-window (lme-agent--main-window)
                                      (lme-agent--project-directory))
                                    role))
    (lme-agent-new 'lispm)))

(defun lme-setup-lispm ()
  "Load LispM and register its agents with the agent tools."
  (lme-lispm-ensure)
  (require 'lispm)
  (setf (alist-get 'lispm lme-agent-emacs-agents)
        (list :name "LispM"
              :icon '(lme-agent-image-icon "lispm-lambda.png" nil)
              :command #'lme-agent-lispm-new
              :sessions #'lme-lispm-sessions
              :new #'lme-lispm-new
              :resume #'lme-lispm-resume
              :buffer-p #'lme-lispm-buffer-p
              :send #'lme-lispm-send
              :rename #'lme-lispm-rename
              :files #'lme-lispm-files))

  ;; A chat keeps to the session column from the moment it is created.
  (add-to-list 'lme-conversation-modes 'lispm-chat-mode)
  (add-hook 'lispm-chat-mode-hook #'lme-lispm--tag-buffer)
  (add-hook 'lispm-session-mode-hook #'lme-lispm--tag-buffer)
  (add-hook 'lispm-state-change-functions #'lme-lispm--changed)
  (add-hook 'lispm-title-change-functions #'lme-lispm--changed)
  (keymap-set lme-agent-map "l" #'lme-agent-lispm-new)
  (with-eval-after-load 'lme-agent-list
    (keymap-set lme-agent-list-mode-map "l" #'lme-agent-lispm-new))
  (lispm-mode-line-mode 1))

(provide 'lme-lispm)
;;; lme-lispm.el ends here
