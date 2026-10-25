;;; lispm-exec.el --- Programs LispM runs without waiting for them  -*- lexical-binding: t; -*-

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

;; A program Emacs waits for, as with `call-process' or `process-lines',
;; freezes it until the program exits: git, ps, a diff.  LispM runs the
;; programs whose answer can come later with `lispm-exec', which returns
;; at once and calls back:
;;
;;   (lispm-exec "git" '("rev-parse" "HEAD")
;;               (lambda (exit output errors) ...))
;;
;; How they run is `lispm-exec-function''s business: by default as
;; processes of Emacs's own, `lispm-exec-locally', which Emacs still
;; forks and reads.  LispM's kernel, a program of its own beside Emacs,
;; advises it to run them there instead (lispm-kernel-exec-mode, in
;; lispm-kernel); nothing here knows of it.  LispM Desktop and the apps
;; run their programs through `lispm-exec' too.

;;; Code:

(require 'cl-lib)

(defvar lispm-exec-function #'lispm-exec-locally
  "The function `lispm-exec' runs programs with, called as `lispm-exec' is.
By default `lispm-exec-locally'.  Something else that can run programs,
as LispM's kernel, advises it with `add-function', running them itself
when it can and leaving the rest to the function it advises.")

(cl-defun lispm-exec (program args callback &key input)
  "Run PROGRAM with ARGS in the background, then call CALLBACK with how it went.
CALLBACK gets PROGRAM's exit code, an integer, or a string saying why it
did not run or exit, then what it printed to its standard output and its
standard error, strings read as UTF-8.  It is called later, never before
this returns.  PROGRAM is found in `exec-path', and runs in
`default-directory' with `process-environment', reading INPUT, a string,
when given, else nothing; ARGS are strings.  See `lispm-exec-function'."
  (funcall lispm-exec-function program args callback :input input)
  nil)

(defun lispm-exec--call (callback &rest arguments)
  "Call CALLBACK with ARGUMENTS, saying what error it signals."
  (with-demoted-errors "lispm-exec: %S"
    (apply callback arguments)))

(cl-defun lispm-exec-locally (program args callback &key input)
  "Run PROGRAM with ARGS as a process of Emacs's own, as `lispm-exec' says."
  (let ((output (generate-new-buffer " *lispm-exec*" t))
        (errors (generate-new-buffer " *lispm-exec errors*" t))
        stderr)
    (condition-case err
        (let ((process
               (make-process
                :name program :buffer output :command (cons program args)
                :connection-type 'pipe :noquery t :coding 'utf-8
                :stderr (setq stderr (make-pipe-process :name (concat program " errors") :buffer errors
                                                        :noquery t :coding 'utf-8 :sentinel #'ignore))
                :sentinel
                (lambda (process _event)
                  (unless (process-live-p process)
                    ;; What it said last, read before its pipe goes.
                    (while (accept-process-output stderr 0 nil t))
                    (delete-process stderr)
                    (let ((exit (if (eq (process-status process) 'exit)
                                    (process-exit-status process)
                                  (format "%s ended by signal %s" program (process-exit-status process))))
                          (printed (with-current-buffer output (buffer-string)))
                          (said (with-current-buffer errors (buffer-string))))
                      (kill-buffer output)
                      (kill-buffer errors)
                      (lispm-exec--call callback exit printed said)))))))
          (when input
            (process-send-string process input))
          (process-send-eof process))
      (error (when (process-live-p stderr) (delete-process stderr))
             (kill-buffer output)
             (kill-buffer errors)
             (run-at-time 0 nil #'lispm-exec--call callback (error-message-string err) "" "")))
    nil))

(provide 'lispm-exec)
;;; lispm-exec.el ends here
