;;; init.el --- lispm-emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs runs this file when this checkout is its init directory: cloned
;; to ~/.emacs.d, or named with emacs --init-directory.  Your own settings
;; go in personal.el beside it (`lme-user-file'), which git ignores.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

(setq custom-file (locate-user-emacs-file "custom.el")
      use-package-always-ensure t)
(when (file-exists-p custom-file)
  (load custom-file nil t))

(require 'lme)
(lme-setup)

(when (file-exists-p lme-user-file)
  (load lme-user-file nil t))

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
