;;; lispm-elisp-kernel-test.el --- Elisp workers through a real kernel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Run with `make test-kernel LISPM_KERNEL=/path/to/lispm-kernel', after
;; building that kernel.  The target sets LISPM_KERNEL_PROGRAM and load-path. These
;; tests start real kernels and child Emacsen; all files are fixtures.

;;; Code:

(require 'lispm-test)
(require 'lispm-elisp)
(require 'lispm-kernel-exec)

(defconst lispm-elisp-kernel-test--program
  (let ((program (getenv "LISPM_KERNEL_PROGRAM")))
    (unless (and program (file-name-absolute-p program) (file-executable-p program))
      (error "Set LISPM_KERNEL_PROGRAM to the absolute path of an executable kernel"))
    program)
  "The real kernel program these tests require; none of them is skipped.")

(defvar lispm-elisp-kernel-test--sentinel 'parent
  "A variable whose child binding must not change the parent Emacs.")

(defun lispm-elisp-kernel-test--wait (runtime predicate &optional seconds)
  "Dispatch RUNTIME until PREDICATE holds, for at most SECONDS (default 10)."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (lispm-kernel-dispatch runtime))
    (funcall predicate)))

(defmacro lispm-elisp-kernel-test--with (runtime &rest body)
  "Run BODY with a real RUNTIME, cancelling its workers and stopping it after."
  (declare (indent 1))
  `(let* ((,runtime (lispm-kernel-start lispm-elisp-kernel-test--program))
          (lispm-kernel-exec-runtime-function (lambda () ,runtime))
          (lispm-elisp--jobs nil)
          (lispm-state-change-functions (copy-sequence lispm-state-change-functions))
          (lispm-session-end-functions (copy-sequence lispm-session-end-functions)))
     (unwind-protect
         (progn
           (should (lispm-elisp-kernel-test--wait
                    ,runtime (lambda () (lispm-kernel-runtime-hello ,runtime))))
           (should (memq :exec (plist-get (lispm-kernel-runtime-hello ,runtime) :jobs)))
           ,@body)
       (mapc #'lispm-elisp--cancel (copy-sequence lispm-elisp--jobs))
       (lispm-elisp-kernel-test--wait
        ,runtime (lambda () (zerop (hash-table-count (lispm-kernel-runtime-requests ,runtime)))) 5)
       (lispm-kernel-stop ,runtime))))

(defun lispm-elisp-kernel-test--call (runtime agent code &optional timeout)
  "Call run_elisp for AGENT with CODE and TIMEOUT; return (TEXT . ERRORP)."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke
     (lispm-get-tool "run_elisp") `((code . ,code) (timeout . ,(or timeout 5)))
     (lambda (text errorp &rest _) (setq result (cons text errorp))))
    (should (lispm-elisp-kernel-test--wait runtime (lambda () result)))
    result))

(defun lispm-elisp-kernel-test--value (result)
  "Read the final Lisp value from a successful worker RESULT."
  (should-not (cdr result))
  (let ((text (car result)))
    (should (string-match "\n=> " text))
    (car (read-from-string text (match-end 0)))))

(defun lispm-elisp-kernel-test--pid (file)
  "Return the child PID written to fixture FILE, or nil before it is complete."
  (when (file-readable-p file)
    (let ((text (lispm-test--disk file)))
      (when (string-match-p "\\`[1-9][0-9]*\\'" text)
        (string-to-number text)))))

(defun lispm-elisp-kernel-test--busy-code (pid-file &optional seconds)
  "Code writing its PID to PID-FILE, then computing for SECONDS or forever."
  (prin1-to-string
   `(progn
      (with-temp-file ,pid-file (insert (number-to-string (emacs-pid))))
      ,(if seconds
           `(let ((deadline (+ (float-time) ,seconds)))
              (while (< (float-time) deadline)))
         '(while t))
      'worker-finished)))

(ert-deftest lispm-elisp-kernel-test-isolated-jsonl ()
  "The kernel starts a different Emacs, which reads local JSONL and computes."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let ((agent (lispm-defagent tester :root project))
            (lispm-elisp-kernel-test--sentinel 'parent))
        (make-directory (expand-file-name "sessions" project))
        (with-temp-file (expand-file-name "sessions/a.jsonl" project)
          (insert "{\"role\":\"user\",\"text\":\"release ignored\"}\n"
                  "malformed line\n"
                  "{\"role\":\"assistant\",\"text\":\"release λ\"}\n"))
        (with-temp-file (expand-file-name "sessions/b.jsonl" project)
          (insert "{\"role\":\"assistant\",\"text\":\"unrelated\"}\n"))
        (let* ((code
                (prin1-to-string
                 '(progn
                    (setq lispm-elisp-kernel-test--sentinel 'child)
                    (let* ((prefix "release")
                           (matches-p (lambda (text) (string-match-p prefix text)))
                           matches)
                      (dolist (file (file-expand-wildcards "sessions/*.jsonl"))
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (while (not (eobp))
                            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                                   (record (ignore-errors (json-parse-string line :object-type 'plist))))
                              (when (and (equal (plist-get record :role) "assistant")
                                         (funcall matches-p (plist-get record :text)))
                                (push (plist-get record :text) matches)))
                            (forward-line 1))))
                      (princ "scanned\n")
                      (list (emacs-pid) (file-truename default-directory) (nreverse matches)
                            (funcall (let ((offset 2)) (lambda (n) (+ offset n))) 40)
                            lispm-elisp-kernel-test--sentinel)))))
               (result (lispm-elisp-kernel-test--call runtime agent code))
               (value (lispm-elisp-kernel-test--value result)))
          (should (string-match-p "scanned\n" (car result)))
          (should (integerp (car value)))
          (should-not (= (car value) (emacs-pid)))
          (should-not (= (car value) (plist-get (lispm-kernel-runtime-hello runtime) :pid)))
          (should (equal (cdr value) (list (file-truename project) '("release λ") 42 'child)))
          (should (eq lispm-elisp-kernel-test--sentinel 'parent))
          (should-not lispm-elisp--jobs))))))

(ert-deftest lispm-elisp-kernel-test-errors ()
  "Malformed code never runs its valid prefix; runtime errors are failures."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (marker (expand-file-name "must-not-exist" project))
             (syntax (lispm-elisp-kernel-test--call
                      runtime agent (format "(with-temp-file %S (insert \"bad\")) (" marker)))
             (failed (lispm-elisp-kernel-test--call runtime agent "(error \"synthetic failure\")")))
        (should (cdr syntax))
        (should (string-match-p "Worker exit 1" (car syntax)))
        (should (string-match-p "Elisp worker: End of file" (car syntax)))
        (should-not (file-exists-p marker))
        (should (cdr failed))
        (should (string-match-p "Elisp worker: synthetic failure" (car failed)))
        (should-not lispm-elisp--jobs)))))

(ert-deftest lispm-elisp-kernel-test-parent-remains-responsive ()
  "A parent timer runs while the worker is in a CPU loop."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-current-agent agent)
             (pid-file (expand-file-name "worker.pid" project))
             result fired timer)
        (lispm-tool-invoke
         (lispm-get-tool "run_elisp")
         `((code . ,(lispm-elisp-kernel-test--busy-code pid-file 1.5)) (timeout . 5))
         (lambda (text errorp &rest _) (setq result (cons text errorp))))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () (lispm-elisp-kernel-test--pid pid-file))))
        (should (process-attributes (lispm-elisp-kernel-test--pid pid-file)))
        (unwind-protect
            (progn
              (setq timer (run-at-time 0.03 nil (lambda () (setq fired t))))
              (should (lispm-elisp-kernel-test--wait runtime (lambda () fired) 1))
              (should-not result)
              (should (lispm-elisp-kernel-test--wait runtime (lambda () result) 5))
              (should (eq (lispm-elisp-kernel-test--value result) 'worker-finished)))
          (when timer (cancel-timer timer)))))))

(ert-deftest lispm-elisp-kernel-test-timeout-kills-worker ()
  "The kernel's timeout ends the real worker and reports a failed tool call."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-current-agent agent)
             (pid-file (expand-file-name "worker.pid" project))
             result pid)
        (lispm-tool-invoke
         (lispm-get-tool "run_elisp")
         `((code . ,(lispm-elisp-kernel-test--busy-code pid-file)) (timeout . 2))
         (lambda (text errorp &rest _) (setq result (cons text errorp))))
        (should (lispm-elisp-kernel-test--wait
                 runtime (lambda () (setq pid (lispm-elisp-kernel-test--pid pid-file)))))
        (should (process-attributes pid))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () result) 5))
        (should (cdr result))
        (should (string-match-p "did not finish" (car result)))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () (not (process-attributes pid))) 5))
        (should-not lispm-elisp--jobs)
        (should (lispm-kernel-live-p runtime))))))

(ert-deftest lispm-elisp-kernel-test-abort-kills-worker ()
  "Aborting a native turn kills its child without a late result or model call."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project :policy '((exec . auto))))
             (pid-file (expand-file-name "worker.pid" project))
             pid)
        (setq lispm-test--scripts
              `((tester ,(lispm-test--msg
                          "tool_use"
                          (lispm-test--tool "worker-1" "run_elisp"
                                            `((code . ,(lispm-elisp-kernel-test--busy-code pid-file))
                                              (timeout . 15))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "Must not be requested.")))))
        (lispm-tell agent "Run the fixture computation.")
        (should (lispm-elisp-kernel-test--wait
                 runtime (lambda () (setq pid (lispm-elisp-kernel-test--pid pid-file)))))
        (should (process-attributes pid))
        (should (= (length lispm-test--requests) 1))
        (lispm-abort agent)
        (let ((transcript (lispm-test--transcript agent)))
          (should (string-match-p "Interrupted: the user aborted the turn" transcript))
          (should (lispm-elisp-kernel-test--wait
                   runtime (lambda () (and (not (process-attributes pid))
                                           (zerop (hash-table-count (lispm-kernel-runtime-requests runtime)))))
                   5))
          (should (equal (lispm-test--transcript agent) transcript))
          (should (= (length lispm-test--requests) 1))
          (should (funcall (lispm-test--idle-p agent)))
          (should-not lispm-elisp--jobs)
          ;; The shared kernel is still usable after cancelling this child.
          (should (= (lispm-elisp-kernel-test--value
                      (lispm-elisp-kernel-test--call runtime agent "(+ 8 9)")) 17)))))))

(provide 'lispm-elisp-kernel-test)
;;; lispm-elisp-kernel-test.el ends here
