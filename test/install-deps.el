;;; install-deps.el --- Install LispM's dependencies for make  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; `make deps' runs this: the packages lispm.el requires (its
;; Package-Requires header) are installed from GNU ELPA and MELPA into
;; .deps/, which the other targets use, and so is Hyperbole, which LispM
;; does not need, for the tests of lispm-hyperbole.el; LispM Kit, which
;; no archive holds, is the checkout LISPM_KIT puts on the load path.
;; As a package, LispM gets them the usual way, from package.el.

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name ".deps")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      ;; Its releases, not MELPA's snapshots.
      package-pinned-packages '((hyperbole . "gnu")))
(package-initialize)
(let ((missing (seq-remove (lambda (dep)
                             (or (eq (car dep) 'emacs)
                                 (package-installed-p (car dep) (cadr dep))
                                 ;; LispM's own, as lispm-kit, are in no
                                 ;; archive: a checkout on the load path.
                                 (locate-library (symbol-name (car dep)))))
                           (append (package-desc-reqs
                                    (with-temp-buffer
                                      (insert-file-contents "lisp/lispm.el")
                                      (package-buffer-info)))
                                   '((hyperbole (9 0)))))))
  (when missing
    (package-refresh-contents)
    (dolist (dep missing)
      (package-install (car dep)))))

;;; install-deps.el ends here
