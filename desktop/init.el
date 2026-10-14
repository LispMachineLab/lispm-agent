;;; init.el --- lispm-desktop  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs runs this file when desktop/ of LispM's repository is its init
;; directory: linked from ~/.emacs.d, or named with emacs --init-directory.
;; Your own settings go in personal.el beside it (`lispm-desktop-user-file'),
;; which git ignores.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

(setq custom-file (locate-user-emacs-file "custom.el")
      use-package-always-ensure t)
(when (file-exists-p custom-file)
  (load custom-file nil t))

(require 'lispm-desktop)
(lispm-desktop-setup)

(when (file-exists-p lispm-desktop-user-file)
  (load lispm-desktop-user-file nil t))

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
