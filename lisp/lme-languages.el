;;; lme-languages.el --- Programming and writing modes  -*- lexical-binding: t; -*-

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

;; Language support: tree-sitter modes, Eglot, Org and Markdown.

;;; Code:

(defun lme-setup-langs ()
  "Set up language environments."
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (use-package treesit
    :ensure nil
    :init
    (setq treesit-auto-install-grammar 'always)
    (setopt treesit-enabled-modes '(typescript-ts-mode
                                    tsx-ts-mode
                                    js-ts-mode
                                    go-ts-mode
                                    go-mod-ts-mode
                                    yaml-ts-mode
                                    json-ts-mode)))

  (use-package eldoc
    :ensure nil
    :init (global-eldoc-mode 1))

  ;; Spelling needs a checker program; without one, Flyspell would report
  ;; an error in every text buffer.
  (use-package flyspell
    :ensure nil
    :if (progn (require 'ispell) (executable-find ispell-program-name))
    :hook ((text-mode . flyspell-mode)
           (prog-mode . flyspell-prog-mode)))

  (use-package eglot
    :ensure nil
    :hook ((python-mode . eglot-ensure)
           (rust-mode . eglot-ensure)
           (c-mode . eglot-ensure)
           (c++-mode . eglot-ensure)
           (typescript-ts-mode . eglot-ensure)
           (tsx-ts-mode . eglot-ensure)
           (js-ts-mode . eglot-ensure)
           (go-ts-mode . eglot-ensure))
    :init
    (setq eglot-autoshutdown t
          eldoc-echo-area-use-multiline-p t)
    :config
    ;; Prefer basedpyright over a stale pyls that eglot would otherwise find first.
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

  (use-package lisp-mode
    :ensure nil
    :config (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)))

  (use-package markdown-mode
    :mode (("\\.mkd\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)))

  (use-package org
    :ensure nil
    :config
    (setq org-src-fontify-natively t)
    (add-to-list 'org-structure-template-alist '("s" . "src"))))


(provide 'lme-languages)
;;; lme-languages.el ends here
