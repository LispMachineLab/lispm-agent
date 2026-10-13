;;; lme.el --- LispM's Emacs distribution  -*- lexical-binding: t; -*-

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

;; lispm-emacs is LispM's Emacs distribution: Emacs set up as a workspace
;; for coding agents, ready from the first start.
;;
;;   menu bar       the λ menu, a title per sidebar view, and the workspaces
;;                  in the Application menu, laid out as Mac OS 9's
;;   windows        each with a Mac OS 9 title bar: close, zoom, collapse
;;   left sidebar   files, agents, shells, open buffers, as Finder lists
;;   main area      agent chats and terminals on the left, files on the right
;;   workspaces     one per project, owning that project's buffers
;;   agents         LispM's, running inside Emacs, and Claude Code, Codex
;;                  and Hermes, each when its CLI is on PATH (C-c a)
;;
;; Its init.el runs `lme-setup' and then the user's own file,
;; `lme-user-file'.  An existing configuration can use it the same way:
;;
;;   (add-to-list 'load-path "~/src/lispm-emacs/lisp")
;;   (require 'lme)
;;   (lme-setup)
;;
;; The first start asks for the editing style (`lme-editing-style') and
;; the appearance (`lme-theme'), and offers to install the icon font.

;;; Code:

(require 'lme-core)
(require 'lme-keys)
(require 'lme-packages)
(require 'lme-interface)
(require 'lme-platinum)
(require 'lme-popup)
(require 'lme-sidebar)
(require 'lme-browser)
(require 'lme-desktop)
(require 'lme-languages)
(require 'lme-ai)
(require 'lme-agent-list)
(require 'lme-lispm)

(defvar nerd-icons-font-family)
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional pfx))

(defcustom lme-startup-view 'agents
  "Sidebar view shown when Emacs starts, or nil for none.
A key of `lme-sidebar-views'."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'lispm-emacs)

(defun lme--read-editing-style ()
  "Ask how text is to be edited; return `vim' or `emacs'."
  (pcase (car (read-multiple-choice
               "Welcome to LispM.  How do you edit text?"
               '((?v "vim" "Vim keys (Evil) in files, SPC as the leader key")
                 (?e "emacs" "Emacs keys everywhere"))))
    (?v 'vim)
    (_ 'emacs)))

(defun lme--read-theme ()
  "Ask for the colour theme; return `dracula' or `platinum'."
  (pcase (car (read-multiple-choice
               "And which appearance?"
               '((?d "dracula" "Dracula: light text on a dark background")
                 (?p "platinum" "Platinum: Mac OS 9's greys, black on white"))))
    (?p 'platinum)
    (_ 'dracula)))

(defun lme--first-start ()
  "Settle what the first start asks; return non-nil when this is it.
The editing style and the appearance are each asked once and saved with
Customize.  A batch Emacs asks nothing: Emacs keys, Dracula."
  (let ((first nil))
    (unless lme-editing-style
      (setq first t)
      (if noninteractive
          (setq lme-editing-style 'emacs)
        (customize-save-variable 'lme-editing-style (lme--read-editing-style))))
    (unless lme-theme
      (setq first t)
      (if noninteractive
          (setq lme-theme 'dracula)
        (customize-save-variable 'lme-theme (lme--read-theme))))
    first))

(defun lme--icon-font-installed-p ()
  "Return non-nil when the icon font of nerd-icons is installed."
  (find-font (font-spec :family nerd-icons-font-family)))

(defun lme-install-fonts ()
  "Install the icon font the sidebar, tabs and mode line draw with."
  (interactive)
  (require 'nerd-icons)
  (nerd-icons-install-fonts t))

(defun lme--offer-fonts ()
  "On a graphical display, offer to install the icon font when it is missing."
  (when (and (display-graphic-p) (not noninteractive))
    (require 'nerd-icons)
    (unless (or (lme--icon-font-installed-p)
                (not (y-or-n-p "Install the icon font the sidebar and tabs use? ")))
      (lme-install-fonts))))

(defun lme--welcome ()
  "Say how to start, once the first start is done."
  (message "%s"
           (substitute-command-keys
            (concat "LispM is ready.  \\`C-c a l' starts an agent in this project, "
                    "\\`C-c a a' shows the agents, \\`C-c a s' resumes a session."))))

(defun lme--show-startup-view ()
  "Show `lme-startup-view' in the sidebar of the first frame."
  (when (and lme-startup-view (not noninteractive)
             (not (lme-sidebar-current-view)))
    (lme-sidebar-select lme-startup-view)))

(defun lme-setup ()
  "Set Emacs up as lispm-emacs."
  (let ((first (lme--first-start)))
    (lme-setup-common-packages)
    (lme-setup-clipboard)
    (lme-setup-keymapping)
    (lme-setup-interface)
    (lme-setup-platinum)
    (lme-popup-enable)
    (lme-setup-desktop)
    (lme-setup-langs)
    (lme-setup-ai)
    (lme-setup-lispm)
    (add-hook 'window-setup-hook #'lme--show-startup-view)
    (when first
      (lme--offer-fonts)
      (add-hook 'window-setup-hook #'lme--welcome 90))))


(provide 'lme)
;;; lme.el ends here
