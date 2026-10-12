;;; init.el --- The Emacs of the LispM demo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; demo/demo.tape starts `emacs -nw -Q -l demo/init.el' in a scratch
;; HOME (see demo/setup.sh), so the recording shows LispM as it comes,
;; without anybody's configuration, and leaves no trace in yours.

;;; Code:

(defconst lispm-demo-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "The LispM checkout being shown.")

(require 'package)
(setq package-user-dir (expand-file-name ".deps" lispm-demo-root))
(package-initialize)
(add-to-list 'load-path lispm-demo-root)
(require 'lispm)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      ring-bell-function #'ignore
      vc-follow-symlinks t)
(menu-bar-mode -1)
(load-theme 'modus-vivendi-tinted t)
(lispm-mode-line-mode 1)

;; The chat opens below the file being shown, with room for a diff.
(add-to-list 'display-buffer-alist
             '("\\`\\*lispm " display-buffer-below-selected (window-height . 31)))

;; Quick answers keep the recording short.
(setq lispm-effort "low")

;; demo.tape waits for this message to know that a turn is over.
(add-hook 'lispm-turn-end-functions
          (lambda (agent _reply) (message "λ %s is done" (lispm-agent-name agent))))

;;; init.el ends here
