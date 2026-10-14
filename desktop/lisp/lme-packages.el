;;; lme-packages.el --- The packages every session runs  -*- lexical-binding: t; -*-

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

;; The packages every session runs: completion (Vertico, Orderless,
;; Consult, Embark, Corfu), the theme and mode line, tab-bar workspaces,
;; and Evil when the editing style is vim (see `lme-editing-style').

;;; Code:

(require 'lme-core)

(defun lme-setup-common-packages ()
  "Set up common packages."
  (setq make-backup-files nil
        auto-save-default nil)

  ;; A package is installed even when the `:if' of its declaration is
  ;; false, so a declaration that may not apply goes inside a `when'.
  (when (and (lme-macos-p) (lme-gui-p))
    (use-package exec-path-from-shell
      :init
      (setq exec-path-from-shell-check-startup-files nil)
      (exec-path-from-shell-initialize)))

  (use-package which-key
    :init (which-key-mode 1))

  (use-package magit
    :commands magit-status)

  (use-package vertico
    :init (vertico-mode 1))

  (use-package savehist
    :ensure nil
    :init (savehist-mode 1))

  (use-package marginalia
    :init (marginalia-mode 1))

  (use-package orderless
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package consult
    :bind (("M-s l" . consult-line)
           ("C-x b" . consult-buffer)
           ("C-x C-r" . consult-recent-file)
           ("M-y" . consult-yank-pop)
           ("M-g g" . consult-goto-line)
           ("M-g i" . consult-imenu)
           ("M-g o" . consult-outline)
           ("M-s r" . consult-ripgrep)
           ("M-s f" . consult-find)))

  (use-package embark
    :bind (("C-." . embark-act)
           ("C-;" . embark-dwim)
           ("C-h B" . embark-bindings))
    :init
    (setq prefix-help-command #'embark-prefix-help-command))

  (use-package embark-consult
    :after (embark consult)
    :hook (embark-collect-mode . consult-preview-at-point-mode))

  (use-package corfu
    :init
    (setq corfu-cycle t
          corfu-auto t
          corfu-auto-prefix 2
          corfu-auto-delay 0.1)
    (global-corfu-mode 1)
    :config
    (when (fboundp 'corfu-popupinfo-mode)
      (corfu-popupinfo-mode 1)))

  (use-package cape
    :after corfu
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (use-package recentf
    :ensure nil
    :init
    (setq recentf-max-saved-items 200)
    (recentf-mode 1))

  (use-package saveplace
    :ensure nil
    :init (save-place-mode 1))

  (use-package flymake
    :ensure nil
    :hook (prog-mode . flymake-mode)
    :init
    ;; Emacs runs no checker that executes a file's code unless the file is
    ;; trusted.  Only the configuration's own Lisp is: lispm-emacs, the init
    ;; files and the user's file; installed packages stay untrusted.  A
    ;; directory is matched as a prefix of the abbreviated file name, so it
    ;; is given abbreviated too.
    (setq trusted-content
          (append (list (abbreviate-file-name (expand-file-name "lisp/" lme-directory)))
                  (delq nil (list user-init-file early-init-file lme-user-file))
                  trusted-content))
    (setq flymake-no-changes-timeout nil
          flymake-show-diagnostics-at-end-of-line t))

  (use-package project
    :ensure nil)

  ;; Both themes are at hand, so the Appearance menu switches between them.
  (use-package dracula-theme
    :defer t)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" lme-directory))
  (lme-load-theme lme-theme)
  (setq inhibit-compacting-font-caches t)

  (when (eq lme-editing-style 'vim)
    (use-package evil
      :init
      ;; Evil edits files only: buffers whose major mode derives from
      ;; `prog-mode', `text-mode' or `conf-mode' start in Normal state, and every
      ;; other buffer (shells, terminals, dired, treemacs, magit, help, ...)
      ;; starts in Emacs state so Evil passes all key and mouse events through.
      ;; Evil resolves a buffer's state along the derived-mode chain.
      (setq evil-want-abbrev-expand-on-insert-exit nil
            evil-undo-system 'undo-redo
            evil-default-state 'emacs
            evil-normal-state-modes '(prog-mode text-mode conf-mode)
            evil-insert-state-modes nil
            evil-motion-state-modes nil
            evil-emacs-state-modes nil)
      (evil-mode 1)
      :config
      (dolist (map (list evil-normal-state-map
                         evil-visual-state-map
                         evil-insert-state-map
                         evil-emacs-state-map))
        (define-key map (kbd "M-x") #'execute-extended-command)
        (define-key map (kbd "M-c") #'lme-copy-region)
        (define-key map (kbd "M-v") #'lme-paste))))

  (use-package yasnippet
    :init (yas-global-mode 1))

  ;; The tab bar is the menu bar; its items come from `lme-setup-platinum'.
  (use-package tab-bar
    :ensure nil
    :init
    (setq tab-bar-show t
          tab-bar-close-button-show nil
          tab-bar-tab-name-function #'lme-tab-bar-tab-name)
    (tab-bar-mode 1)
    (tab-bar-history-mode 1))

  ;; Each tab is a workspace owning its own buffers; switching projects opens
  ;; the project in a workspace of its own.  A title bar's buffer menu and
  ;; `other-buffer' keep to the workspace's buffers; `C-x b' lists them all,
  ;; as in Emacs.
  (use-package tabspaces
    :demand t
    :init
    (setq tabspaces-default-tab "Home"
          tabspaces-use-filtered-buffers-as-default t
          tabspaces-project-switch-opens-workspace t
          tabspaces-initialize-project-with-todo nil
          tabspaces-include-buffers '("*scratch*" "*Messages*")
          ;; A project's saved session goes with Emacs's own state, not into
          ;; the project, where it would show up as an untracked file.
          tabspaces-session-project-session-store (locate-user-emacs-file "tabspaces/"))
    (make-directory tabspaces-session-project-session-store t)
    :config
    (tabspaces-mode 1)
    ;; The first workspace is the default one; named explicitly, it no longer
    ;; takes the name of whatever buffer its main window shows.
    (tab-bar-rename-tab tabspaces-default-tab)))


(provide 'lme-packages)
;;; lme-packages.el ends here
