;;; lispm-elisp-worker.el --- Elisp in a disposable Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The kernel starts a clean batch Emacs with this file and calls
;; `lispm-elisp-worker-main'.  Standard input holds one printed Lisp
;; string: the code to evaluate.  The code's output goes to standard
;; output, followed by its last value; errors go to standard error.
;; Nothing is loaded from the parent's running image or the user's init.

;;; Code:

(require 'lisp-mode)

(defun lispm-elisp-worker--forms (code)
  "Read every Lisp form in CODE, rejecting an unfinished final form."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert code)
    (goto-char (point-min))
    (let (forms)
      (forward-comment (point-max))
      (while (not (eobp))
        (push (read (current-buffer)) forms)
        (forward-comment (point-max)))
      (nreverse forms))))

(defun lispm-elisp-worker--input ()
  "Read standard input and return the single code string it holds."
  (with-temp-buffer
    (condition-case nil
        (while t
          (insert (read-from-minibuffer "") "\n"))
      (end-of-file nil))
    (let ((forms (lispm-elisp-worker--forms (buffer-string))))
      (unless (and (= (length forms) 1) (stringp (car forms)))
        (error "Expected one printed Lisp string on standard input"))
      (car forms))))

(defun lispm-elisp-worker-main ()
  "Evaluate the code string on standard input, then exit this batch Emacs.
Printed output is followed by a newline, => and the printed last value.
Malformed input, evaluation errors and quits exit with status 1."
  (unless noninteractive
    (user-error "The Elisp worker runs only in batch Emacs"))
  (condition-case err
      (let (value)
        ;; Parse everything before running any of it: a broken final form
        ;; must not leave earlier forms evaluated without saying so.
        (dolist (form (lispm-elisp-worker--forms (lispm-elisp-worker--input)))
          (setq value (eval form t)))
        (let ((print-length 100) (print-level 10) (print-circle t)
              (standard-output t))
          (princ "\n=> ")
          (prin1 value)
          (terpri))
        (kill-emacs 0))
    ((error quit)
     (princ (format "Elisp worker: %s\n" (error-message-string err))
            #'external-debugging-output)
     (kill-emacs 1))))

(provide 'lispm-elisp-worker)
;;; lispm-elisp-worker.el ends here
