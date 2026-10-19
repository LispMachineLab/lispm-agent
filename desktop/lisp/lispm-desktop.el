;;; lispm-desktop.el --- LispM's Emacs distribution  -*- lexical-binding: t; -*-

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

;; lispm-desktop is LispM's Emacs distribution: Emacs set up as a workspace
;; for coding agents, ready from the first start.
;;
;;   menu bar       the λ menu, a title per sidebar view, and the workspaces
;;                  in the Application menu, laid out as Mac OS 9's
;;   windows        each with a Mac OS 9 title bar: close, zoom, collapse
;;   left sidebar   files, agents, shells, open buffers, as Finder lists
;;   main area      agent chats and terminals on the left, files on the right
;;   workspaces     one per project, owning that project's buffers
;;   coherence      windows out on the computer's screen, the desktop in
;;                  the Dock (C-c h)
;;   agents         LispM's, running inside Emacs, and Claude Code, Codex
;;                  and Hermes, each when its CLI is on PATH (C-c a)
;;
;; Its init.el runs `lispm-desktop-setup' and then the user's own file,
;; `lispm-desktop-user-file'.  An existing configuration can use it the same way:
;;
;;   (add-to-list 'load-path "~/src/lispm-desktop/lisp")
;;   (require 'lispm-desktop)
;;   (lispm-desktop-setup)
;;
;; The first start asks for the editing style (`lispm-desktop-editing-style') and
;; the appearance (`lispm-desktop-theme'), and offers to install the icon font.

;;; Code:

(require 'lispm-desktop-core)
(require 'lispm-desktop-keys)
(require 'lispm-desktop-packages)
(require 'lispm-desktop-interface)
(require 'lispm-desktop-term)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-sidebar)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-coherence)
(require 'lispm-desktop-browser)
(require 'lispm-desktop-browser-places)
(require 'lispm-desktop-browser-hints)
(require 'lispm-desktop-browser-extensions)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-editor)
(require 'lispm-desktop-buffer-manager)
(require 'lispm-desktop-workspaces)
(require 'lispm-desktop-finder)
(require 'lispm-desktop-settings)
(require 'lispm-desktop-strip)
(require 'lispm-desktop-exwm)
(require 'lispm-desktop-languages)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-agent-list)
(require 'lispm-desktop-lispm)

(defvar nerd-icons-font-family)
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional pfx))

(defcustom lispm-desktop-startup-view 'agents
  "Sidebar view shown when Emacs starts, or nil for none.
A key of `lispm-desktop-sidebar-views'."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'lispm-desktop)

(defun lispm-desktop--read-editing-style ()
  "Ask how text is to be edited; return `vim' or `emacs'."
  (pcase (car (read-multiple-choice
               "Welcome to LispM.  How do you edit text?"
               '((?v "vim" "Vim keys (Evil) in files, SPC as the leader key")
                 (?e "emacs" "Emacs keys everywhere"))))
    (?v 'vim)
    (_ 'emacs)))

(defun lispm-desktop--read-theme ()
  "Ask for the colour theme; return `dracula' or `platinum'."
  (pcase (car (read-multiple-choice
               "And which appearance?"
               '((?d "dracula" "Dracula: light text on a dark background")
                 (?p "platinum" "Platinum: Mac OS 9's greys, black on white"))))
    (?p 'platinum)
    (_ 'dracula)))

(defun lispm-desktop--first-start ()
  "Settle what the first start asks; return non-nil when this is it.
The editing style and the appearance are each asked once and saved with
Customize.  A batch Emacs asks nothing: Emacs keys, Dracula."
  (let ((first nil))
    (unless lispm-desktop-editing-style
      (setq first t)
      (if noninteractive
          (setq lispm-desktop-editing-style 'emacs)
        (customize-save-variable 'lispm-desktop-editing-style (lispm-desktop--read-editing-style))))
    (unless lispm-desktop-theme
      (setq first t)
      (if noninteractive
          (setq lispm-desktop-theme 'dracula)
        (customize-save-variable 'lispm-desktop-theme (lispm-desktop--read-theme))))
    first))

(defun lispm-desktop--icon-font-installed-p ()
  "Return non-nil when the icon font of nerd-icons is installed."
  (find-font (font-spec :family nerd-icons-font-family)))

(defun lispm-desktop-install-fonts ()
  "Install the icon font the sidebar, tabs and mode line draw with."
  (interactive)
  (require 'nerd-icons)
  (nerd-icons-install-fonts t))

(defun lispm-desktop--offer-fonts ()
  "On a graphical display, offer to install the icon font when it is missing."
  (when (and (display-graphic-p) (not noninteractive))
    (require 'nerd-icons)
    (unless (or (lispm-desktop--icon-font-installed-p)
                (not (y-or-n-p "Install the icon font the sidebar and tabs use? ")))
      (lispm-desktop-install-fonts))))

(defun lispm-desktop--welcome ()
  "Say how to start, once the first start is done."
  (message "%s"
           (substitute-command-keys
            (concat "LispM is ready.  \\`C-c a l' starts an agent in this project, "
                    "\\`C-c a a' shows the agents, \\`C-c a s' resumes a session."))))

(defun lispm-desktop--show-startup-view ()
  "Show `lispm-desktop-startup-view' in the sidebar of the first frame."
  (when (and lispm-desktop-startup-view (not noninteractive)
             (not (lispm-desktop-sidebar-current-view)))
    (lispm-desktop-sidebar-select lispm-desktop-startup-view)))

(defun lispm-desktop-setup ()
  "Set Emacs up as lispm-desktop."
  (let ((first (lispm-desktop--first-start)))
    (lispm-desktop-setup-common-packages)
    (lispm-desktop-setup-clipboard)
    (lispm-desktop-setup-keymapping)
    (lispm-desktop-setup-interface)
    (lispm-desktop-setup-term)
    (lispm-desktop-setup-platinum)
    (lispm-desktop-popup-enable)
    (lispm-desktop-setup-apps)
    (lispm-desktop-setup-coherence)
    (lispm-desktop-setup-desktop)
    (lispm-desktop-setup-editor)
    (lispm-desktop-setup-workspaces)
    (lispm-desktop-setup-finder)
    (lispm-desktop-setup-browser)
    (lispm-desktop-setup-browser-places)
    (lispm-desktop-setup-browser-hints)
    (lispm-desktop-setup-settings)
    (lispm-desktop-setup-strip)
    (setq lispm-desktop--set-up t)
    (add-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
    (lispm-desktop-setup-langs)
    (lispm-desktop-setup-ai)
    (lispm-desktop-setup-lispm)
    ;; After init, so personal.el can still set `lispm-desktop-window-manager'.
    (add-hook 'window-setup-hook #'lispm-desktop-setup-exwm)
    ;; After init too, so personal.el can still turn extensions off, and
    ;; before anything opens a page, which would run none.
    (add-hook 'window-setup-hook #'lispm-desktop-setup-browser-extensions -10)
    (add-hook 'window-setup-hook #'lispm-desktop--show-startup-view)
    (when first
      (lispm-desktop--offer-fonts)
      (add-hook 'window-setup-hook #'lispm-desktop--welcome 90))))


(provide 'lispm-desktop)
;;; lispm-desktop.el ends here
