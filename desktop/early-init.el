;;; early-init.el --- lispm-emacs, before the first frame  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs runs this file when this checkout is its init directory: cloned
;; to ~/.emacs.d, or named with emacs --init-directory.

;;; Code:

(load (expand-file-name "lisp/lme-early" (file-name-directory load-file-name)) nil t)

;;; early-init.el ends here
