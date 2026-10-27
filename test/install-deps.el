;;; install-deps.el --- Install LispM's dependencies for make  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; `make deps' runs this: the packages lispm.el requires (its
;; Package-Requires header) are installed from GNU ELPA and MELPA into
;; .deps/, which the other targets use, but for those on the load path
;; already: LispM's own libraries, as LispM UI, which only git holds,
;; are checkouts the Makefile puts there.  As a package, LispM gets them
;; the usual way, from package.el.

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name ".deps")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(let ((missing (seq-remove (lambda (dep)
                             (or (eq (car dep) 'emacs)
                                 (package-installed-p (car dep) (cadr dep))
                                 (locate-library (concat (symbol-name (car dep)) ".el") t)))
                           (package-desc-reqs
                            (with-temp-buffer
                              (insert-file-contents "lisp/lispm.el")
                              (package-buffer-info))))))
  (when missing
    (package-refresh-contents)
    (dolist (dep missing)
      (package-install (car dep)))))

;;; install-deps.el ends here
