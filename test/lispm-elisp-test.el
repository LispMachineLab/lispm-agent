;;; lispm-elisp-test.el --- Tests of process-based Elisp tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Run after lispm-test.el.  The kernel is replaced by a recorded
;; submission: no editor process or external kernel is needed here.

;;; Code:

(require 'lispm-test)
(require 'lispm-elisp)

(defvar lispm-kernel-exec-runtime-function)

(defmacro lispm-elisp-test--with-kernel (&rest body)
  "Run BODY with a kernel that records submissions and cancellation.
KERNEL-SUBMISSIONS holds (RUNTIME JOB INPUT KEYS), newest first;
KERNEL-CALLBACKS maps request ids to callbacks."
  (declare (indent 0))
  `(let ((lispm-elisp--jobs nil)
         (lispm-state-change-functions (copy-sequence lispm-state-change-functions))
         (lispm-session-end-functions (copy-sequence lispm-session-end-functions))
         (lispm-agents-context-release-functions
          (copy-sequence lispm-agents-context-release-functions))
         (kernel-submissions nil)
         (kernel-callbacks nil)
         (kernel-cancelled nil)
         (kernel-next-id 0))
     (cl-letf (((symbol-function 'lispm-elisp--runtime) (lambda () 'test-kernel))
               ((symbol-function 'lispm-kernel-submit)
                (lambda (runtime job input &rest keys)
                  (push (list runtime job input keys) kernel-submissions)
                  (let ((id (cl-incf kernel-next-id)))
                    (push (cons id (plist-get keys :on-result)) kernel-callbacks)
                    id)))
               ((symbol-function 'lispm-kernel-cancel)
                (lambda (runtime id) (push (list runtime id) kernel-cancelled)))
               ;; A strict kernel tool never retries through an editor-owned
               ;; process, even if the kernel refuses the request.
               ((symbol-function 'lispm-exec) (lambda (&rest _) (ert-fail "Local fallback")))
               ((symbol-function 'lispm-exec-locally) (lambda (&rest _) (ert-fail "Local fallback")))
               ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Editor-owned process"))))
       ,@body)))

(ert-deftest lispm-elisp-test-structured-submission ()
  "Code travels as data on stdin, with the agent's directory and environment."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let* ((agent (lispm-defagent tester :root project))
             (code "(progn (princ \"λ; $(touch should-not-exist)\\n\")\n  (+ 1 2))")
             (process-environment (cons "LISPM_ELISP_TEST=sent" process-environment))
             results)
        (should-not (lispm-elisp--start agent code 7
                                       (lambda (text errorp) (push (list text errorp) results))))
        (should (= (length kernel-submissions) 1))
        (should-not results)
        (pcase-let* ((`(,runtime ,job ,input ,keys) (car kernel-submissions))
                     (args (plist-get input :args))
                     (stdin (plist-get input :stdin)))
          (should (eq runtime 'test-kernel))
          (should (eq job 'exec))
          (should (file-name-absolute-p (plist-get input :program)))
          (should (file-executable-p (plist-get input :program)))
          (should (equal (seq-take args 2) '("-Q" "--batch")))
          (should (equal (nth 2 args) "--load"))
          (should (file-name-absolute-p (nth 3 args)))
          (should (file-readable-p (nth 3 args)))
          (should (equal (nthcdr 4 args) '("--funcall" "lispm-elisp-worker-main")))
          (should-not (member code args))
          (should (equal (car (read-from-string stdin)) code))
          (should (= (cl-count ?\n stdin) 1))
          (should (equal (plist-get input :cwd) (directory-file-name project)))
          (should (member "LISPM_ELISP_TEST=sent" (plist-get input :env)))
          (should (= (plist-get input :timeout) 7))
          (should (= (plist-get keys :timeout) 17)))
        (funcall (alist-get 1 kernel-callbacks) 'success '(:exit 0 :stderr ""))
        (should (= (length results) 1))
        (should-not (cadar results))
        (should-not lispm-elisp--jobs)))))

(ert-deftest lispm-elisp-test-default-timeout ()
  "An omitted timeout still gives the child a finite execution deadline."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)))
        (lispm-elisp--start agent "1" nil #'ignore)
        (should (= (plist-get (nth 2 (car kernel-submissions)) :timeout) 120))
        (should (= (plist-get (nth 3 (car kernel-submissions)) :timeout) 130))
        (funcall (alist-get 1 kernel-callbacks) 'success '(:exit 0 :stderr ""))))))

(ert-deftest lispm-elisp-test-invalid-input-does-not-submit ()
  "Bad deadlines and remote roots cannot become a local kernel process."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)))
        (dolist (timeout '(0 -1 86401 1.5 "5"))
          (should-error (lispm-elisp--start agent "1" timeout #'ignore)))
        (setf (lispm-agent-root agent) "/ssh:example.invalid:/work/")
        (should-error (lispm-elisp--start agent "1" 1 #'ignore))
        (should-not kernel-submissions)
        (should-not lispm-elisp--jobs)))))

(ert-deftest lispm-elisp-test-runtime-requires-live-exec-kernel ()
  "Having a kernel library alone does not make process execution available."
  (let ((original-require (symbol-function 'require)))
    (dolist (case '((nil test-kernel t (:exec))
                    (t nil t (:exec))
                    (t test-kernel nil (:exec))
                    (t test-kernel t (:files))
                    (t test-kernel t (:exec))))
      (pcase-let* ((`(,library ,runtime ,live ,jobs) case)
                   (lispm-kernel-exec-runtime-function (lambda () runtime)))
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'lispm-kernel-exec) library
                       (funcall original-require feature filename noerror))))
                  ((symbol-function 'lispm-kernel-live-p) (lambda (_) live))
                  ((symbol-function 'lispm-kernel-runtime-hello) (lambda (_) (list :jobs jobs))))
          (if (and library runtime live (memq :exec jobs))
              (should (eq (lispm-elisp--runtime) runtime))
            (should-error (lispm-elisp--runtime))))))))

(ert-deftest lispm-elisp-test-unavailable-kernel-does-not-fallback ()
  "Kernel absence or submission failure never runs code in the editor."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)))
        (cl-letf (((symbol-function 'lispm-elisp--runtime)
                   (lambda () (error "Kernel unavailable"))))
          (should-error (lispm-elisp--start agent "1" 1 #'ignore)))
        (cl-letf (((symbol-function 'lispm-kernel-submit)
                   (lambda (&rest _) (error "Kernel disappeared"))))
          (should-error (lispm-elisp--start agent "1" 1 #'ignore)))
        (should-not kernel-submissions)
        (should-not lispm-elisp--jobs)))))

(ert-deftest lispm-elisp-test-quit-during-submit-releases-bookkeeping ()
  "A submission which signals quit leaves no job or lifecycle hook behind."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project))
            callback results caught)
        (cl-letf (((symbol-function 'lispm-kernel-submit)
                   (lambda (_runtime _job _input &rest keys)
                     (setq callback (plist-get keys :on-result))
                     (signal 'quit nil))))
          (condition-case nil
              (lispm-elisp--start agent "1" 1
                                  (lambda (text errorp) (push (list text errorp) results)))
            (quit (setq caught t))))
        (should caught)
        (should-not kernel-cancelled)
        (should-not lispm-elisp--jobs)
        (should-not (memq #'lispm-elisp--state-changed lispm-state-change-functions))
        (should-not (memq #'lispm-elisp--session-ended lispm-session-end-functions))
        (should-not (memq #'lispm-elisp--terminal-ended lispm-agents-context-release-functions))
        (funcall callback 'success '(:exit 0 :stderr ""))
        (should-not results)))))

(ert-deftest lispm-elisp-test-pending-quit-cancels-submitted-id ()
  "C-g during submission is deferred until the child's id can be cancelled."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project))
            (quit-flag nil)
            callback results caught protected)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'lispm-kernel-submit)
                         (lambda (_runtime _job _input &rest keys)
                           (setq callback (plist-get keys :on-result)
                                 protected inhibit-quit)
                           (prog1 42 (setq quit-flag t)))))
                (condition-case nil
                    (let ((inhibit-quit nil))
                      (lispm-elisp--start agent "1" 1
                                          (lambda (text errorp) (push (list text errorp) results))))
                  (quit (setq quit-flag nil caught t))))
              (should protected)
              (should caught)
              (should (equal kernel-cancelled '((test-kernel 42))))
              (should-not lispm-elisp--jobs)
              (should-not (memq #'lispm-elisp--state-changed lispm-state-change-functions))
              (should-not (memq #'lispm-elisp--session-ended lispm-session-end-functions))
              (should-not (memq #'lispm-elisp--terminal-ended lispm-agents-context-release-functions))
              (funcall callback 'cancelled nil)
              (funcall callback 'success '(:exit 0 :stderr ""))
              (should-not results))
          (setq quit-flag nil))))))

(ert-deftest lispm-elisp-test-success-and-late-results ()
  "Ordered chunks reach the caller once; duplicate or late events do not."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)) results)
        (lispm-elisp--start agent "(+ 1 2)" 1
                            (lambda (text errorp) (push (list text errorp) results)))
        (let ((callback (alist-get 1 kernel-callbacks)))
          (funcall callback 'chunk "first λ\n")
          (funcall callback 'chunk "=> 3\n")
          (should-not results)
          (funcall callback 'success '(:exit 0 :stderr ""))
          (should (= (length results) 1))
          (should (string-match-p (regexp-quote "first λ\n=> 3\n") (caar results)))
          (should-not (cadar results))
          (should-not lispm-elisp--jobs)
          (let ((saved (copy-tree results)))
            (funcall callback 'chunk "late")
            (funcall callback 'success '(:exit 0 :stderr ""))
            (funcall callback 'lost "gone")
            (should (equal results saved))))))))

(ert-deftest lispm-elisp-test-child-failure-retains-diagnostics ()
  "Nonzero exits and signals preserve stdout and stderr and fail the tool."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)))
        (dolist (outcome '((:exit 7 :stderr "evaluation failed")
                           (:signal 9 :stderr "worker terminated")))
          (let (results)
            (lispm-elisp--start agent "(error \"bad\")" 1
                                (lambda (text errorp) (push (list text errorp) results)))
            (let ((callback (alist-get kernel-next-id kernel-callbacks)))
              (funcall callback 'chunk "before failure\n")
              (funcall callback 'success outcome))
            (should (= (length results) 1))
            (should (cadar results))
            (should (string-match-p "before failure" (caar results)))
            (should (string-match-p (regexp-quote (plist-get outcome :stderr)) (caar results)))
            (should-not lispm-elisp--jobs)))))))

(ert-deftest lispm-elisp-test-kernel-terminal-failures ()
  "Every terminal kernel failure settles once without a local retry."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)))
        (dolist (event '((failed . "backend failure") (rejected . capacity)
                         (lost . "connection closed") (timeout) (cancelled)))
          (let ((before (length kernel-submissions)) results)
            (lispm-elisp--start agent "1" 1
                                (lambda (text errorp) (push (list text errorp) results)))
            (let ((callback (alist-get kernel-next-id kernel-callbacks)))
              (funcall callback 'chunk "partial result\n")
              (funcall callback (car event) (cdr event))
              (funcall callback 'success '(:exit 0 :stderr "")))
            (should (= (length kernel-submissions) (1+ before)))
            (should (= (length results) 1))
            (should (cadar results))
            (should (string-match-p "partial result" (caar results)))
            (should-not lispm-elisp--jobs)))))))

(ert-deftest lispm-elisp-test-output-is-bounded-during-execution ()
  "A noisy worker cannot accumulate unbounded chunks before it exits."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project))
            (lispm-tool-output-limit 80)
            results)
        (lispm-elisp--start agent "1" 1
                            (lambda (text errorp) (push (list text errorp) results)))
        (let ((callback (alist-get 1 kernel-callbacks))
              (job (car lispm-elisp--jobs)))
          (funcall callback 'chunk (make-string 1000 ?a))
          (dotimes (_ 4) (funcall callback 'chunk (make-string 1000 ?b)))
          (should (<= (length (apply #'concat (lispm-elisp--job-chunks job))) 80))
          (funcall callback 'success '(:exit 0 :stderr ""))
          (should (= (length results) 1))
          (should-not (cadar results))
          (should (string-prefix-p (make-string 80 ?a) (caar results)))
          (should (< (length (caar results)) 400))
          (should-not lispm-elisp--jobs))))))

(ert-deftest lispm-elisp-test-abort-cancels-only-own-job ()
  "Stopping one agent cancels its child and ignores that child's late reply."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((first (lispm-defagent first :root project))
            (second (lispm-defagent second :root project))
            first-results second-results)
        (lispm-elisp--start first "1" 1
                            (lambda (text errorp) (push (list text errorp) first-results)))
        (lispm-elisp--start second "2" 1
                            (lambda (text errorp) (push (list text errorp) second-results)))
        (setf (lispm-agent-state first) 'acting)
        (lispm-elisp--state-changed first)
        (should-not kernel-cancelled)
        (setf (lispm-agent-state first) 'aborting)
        (lispm-elisp--state-changed first)
        (lispm-elisp--state-changed first)
        (should (equal kernel-cancelled '((test-kernel 1))))
        (should (= (length lispm-elisp--jobs) 1))
        (funcall (alist-get 1 kernel-callbacks) 'cancelled nil)
        (funcall (alist-get 1 kernel-callbacks) 'chunk "late")
        (funcall (alist-get 1 kernel-callbacks) 'success '(:exit 0 :stderr ""))
        (should-not first-results)
        (funcall (alist-get 2 kernel-callbacks) 'chunk "=> 2\n")
        (funcall (alist-get 2 kernel-callbacks) 'success '(:exit 0 :stderr ""))
        (should (= (length second-results) 1))
        (should-not (cadar second-results))
        (should-not lispm-elisp--jobs)))))

(ert-deftest lispm-elisp-test-session-close-cancels-all-jobs ()
  "Closing a session cancels all its workers without reviving its callbacks."
  (lispm-test-env
    (lispm-elisp-test--with-kernel
      (let ((agent (lispm-defagent tester :root project)) results)
        (dotimes (_ 2)
          (lispm-elisp--start agent "1" 1
                              (lambda (text errorp) (push (list text errorp) results))))
        (lispm-elisp--session-ended agent)
        (should (= (length kernel-cancelled) 2))
        (should (member '(test-kernel 1) kernel-cancelled))
        (should (member '(test-kernel 2) kernel-cancelled))
        (should-not lispm-elisp--jobs)
        (dolist (entry kernel-callbacks)
          (funcall (cdr entry) 'cancelled nil)
          (funcall (cdr entry) 'success '(:exit 0 :stderr "")))
        (should-not results)))))

(ert-deftest lispm-elisp-test-terminal-release-cancels-only-its-worker ()
  "The real terminal-release route cancels a bridge agent's child only."
  (lispm-test-with-peers
    (lispm-elisp-test--with-kernel
      (let ((first-terminal (generate-new-buffer " *elisp-first-terminal*"))
            (second-terminal (generate-new-buffer " *elisp-second-terminal*"))
            (unused-terminal (generate-new-buffer " *elisp-unused-terminal*"))
            first-results second-results)
        (unwind-protect
            (progn
              (dolist (entry (list (cons first-terminal 'codex)
                                   (cons second-terminal 'hermes)))
                (with-current-buffer (car entry)
                  (setq default-directory project
                        lispm-agents-buffer-agent (cdr entry))))
              (dolist (terminal (list first-terminal second-terminal unused-terminal))
                (lispm-agents-context-bind (lispm-agents-context-presence-make) terminal))
              (let ((first (lispm-agents-bridge-agent first-terminal))
                    (second (lispm-agents-bridge-agent second-terminal)))
                (lispm-elisp--start first "1" 1
                                    (lambda (text errorp) (push (list text errorp) first-results)))
                (lispm-elisp--start second "2" 1
                                    (lambda (text errorp) (push (list text errorp) second-results))))
              (should (memq #'lispm-elisp--terminal-ended lispm-agents-context-release-functions))
              ;; A terminal which never called a tool has no bridge agent.
              (lispm-agents-context-release unused-terminal)
              (should-not kernel-cancelled)
              (lispm-agents-context-release first-terminal)
              (lispm-agents-context-release first-terminal)
              (should (equal kernel-cancelled '((test-kernel 1))))
              (should (= (length lispm-elisp--jobs) 1))
              (should (lispm-agents-context-servers second-terminal))
              (should-not (lispm-agents-context-servers first-terminal))
              (funcall (alist-get 1 kernel-callbacks) 'cancelled nil)
              (funcall (alist-get 1 kernel-callbacks) 'success '(:exit 0 :stderr ""))
              (should-not first-results)
              (should-not second-results)
              (funcall (alist-get 2 kernel-callbacks) 'success '(:exit 0 :stderr ""))
              (should (= (length second-results) 1))
              (should-not (cadar second-results))
              (should-not lispm-elisp--jobs)
              (should-not (memq #'lispm-elisp--state-changed lispm-state-change-functions))
              (should-not (memq #'lispm-elisp--session-ended lispm-session-end-functions))
              (should-not (memq #'lispm-elisp--terminal-ended lispm-agents-context-release-functions))
              (lispm-agents-context-release second-terminal)
              (should (equal kernel-cancelled '((test-kernel 1)))))
          (dolist (terminal (list first-terminal second-terminal unused-terminal))
            (when (buffer-live-p terminal) (kill-buffer terminal))))))))

(ert-deftest lispm-elisp-test-policy-and-preview ()
  "Process evaluation retains code approval and previews with a timeout argument."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project))
           (tool (lispm-get-tool "run_elisp"))
           (read '((code . "(+ 1 2)") (timeout . 7) (why . "Compute a result")))
           (write '((code . "(with-temp-file \"probe\" (insert \"x\"))") (timeout . 7))))
      (should (eq (lispm-tool-permission tool) 'exec))
      (should (eq (lispm-tool-action agent tool read) 'ask))
      (should (equal (lispm-tools-allowance tool read agent)
                     '("run_elisp" (code . "(+ 1 2)") (timeout . 7))))
      (should-not (lispm-tool-preview-text tool read))
      (let ((preview (lispm-tool-preview-text tool write)))
        (should (stringp preview))
        (should (string-match-p "changes buffers or files" preview)))
      (should-not (file-exists-p (expand-file-name "probe" project))))))

(provide 'lispm-elisp-test)
;;; lispm-elisp-test.el ends here
