;;; lispm-process-tools-test.el --- Tests of argv process tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'lispm-test)
(require 'lispm-process-tools)

(defmacro lispm-process-tools-test--with-runs (&rest body)
  "Run BODY with isolated program ownership and restore all lifecycle hooks."
  (declare (indent 0))
  `(let ((lispm-process-tools--runs nil)
         (lispm-sandbox-level 'none)
         (lispm-state-change-functions (copy-sequence lispm-state-change-functions))
         (lispm-session-end-functions (copy-sequence lispm-session-end-functions))
         (lispm-agents-context-release-functions
          (copy-sequence lispm-agents-context-release-functions)))
     (unwind-protect (progn ,@body)
       (mapc #'lispm-process-tools--cleanup (copy-sequence lispm-process-tools--runs)))))

(defun lispm-process-tools-test--call (agent input)
  "Invoke run_program for AGENT on INPUT, returning (JSON . ERRORP)."
  (let ((lispm-current-agent agent) results)
    (lispm-tool-invoke (lispm-get-tool "run_program") input
                       (lambda (text errorp) (push (cons text errorp) results)))
    (should (lispm-test--wait (lambda () results) 5))
    (should (= (length results) 1))
    (car results)))

(defun lispm-process-tools-test--released-p (run)
  "Non-nil when RUN owns no live processes, deadline or lifecycle entry."
  (and (lispm-process-tools--run-finished run)
       (not (memq run lispm-process-tools--runs))
       (not (process-live-p (lispm-process-tools--run-process run)))
       (not (process-live-p (lispm-process-tools--run-errors run)))
       (not (memq (lispm-process-tools--run-timer run) timer-list))))

(ert-deftest lispm-process-tools-test-argv-cwd-and-streams ()
  "Arguments are data, cwd is explicit, and stderr stays separate from stdout."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let* ((agent (lispm-defagent tester :root project))
             (cwd (expand-file-name "with spaces/" project))
             (literal "λ $(touch marker); * ' \" & |")
             (argv (vector "-c" "printf '%s\\n' \"$@\"; pwd -P; printf diagnostic >&2"
                           "test" literal "")))
        (make-directory cwd)
        (pcase-let* ((`(,text . ,errorp)
                      (lispm-process-tools-test--call
                       agent `((program . "/bin/sh") (args . ,argv) (cwd . "with spaces"))))
                     (result (lispm--json-decode text)))
          (should-not errorp)
          (should (equal (alist-get 'status result) "exited"))
          (should (eql (alist-get 'exit_code result) 0))
          (should (equal (alist-get 'stdout result)
                         (format "%s\n\n%s\n" literal (directory-file-name (file-truename cwd)))))
          (should (equal (alist-get 'stderr result) "diagnostic"))
          (should (eq (alist-get 'truncated result) :false))
          (should-not (file-exists-p (expand-file-name "marker" cwd)))
          (should (equal (aref argv 3) literal))
          (should-not lispm-process-tools--runs))))))

(ert-deftest lispm-process-tools-test-nonzero-and-no-stdin ()
  "A nonzero exit fails with diagnostics; a stdin reader receives EOF."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let ((agent (lispm-defagent tester :root project)))
        (pcase-let* ((`(,text . ,errorp)
                      (lispm-process-tools-test--call
                       agent '((program . "/bin/sh")
                               (args . ["-c" "cat; printf partial; printf failed >&2; exit 7"]))))
                     (result (lispm--json-decode text)))
          (should errorp)
          (should (eql (alist-get 'exit_code result) 7))
          (should (equal (alist-get 'stdout result) "partial"))
          (should (equal (alist-get 'stderr result) "failed")))))))

(ert-deftest lispm-process-tools-test-output-bounded-and-valid-json ()
  "Noisy output is bounded while running and encoded without breaking JSON."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-tool-output-limit 256)
             (lispm-current-agent agent)
             results run)
        (lispm-tool-invoke
         (lispm-get-tool "run_program")
         '((program . "/bin/sh")
           (args . ["-c" "i=0; while [ $i -lt 1000 ]; do printf '\"\\\\\\n'; i=$((i+1)); done; sleep 1"]))
         (lambda (text errorp) (push (cons text errorp) results)))
        (setq run (car lispm-process-tools--runs))
        (should (lispm-test--wait (lambda () (lispm-process-tools--run-truncated run))))
        (should (<= (lispm-process-tools--run-size run) 256))
        (should (<= (apply #'+ (mapcar #'length (lispm-process-tools--run-output run))) 256))
        (should (lispm-test--wait (lambda () results)))
        (should-not (cdar results))
        (should (<= (length (caar results)) 256))
        (should (eq (alist-get 'truncated (lispm--json-decode (caar results))) t))
        (should (lispm-process-tools-test--released-p run))))))

(ert-deftest lispm-process-tools-test-timeout-and-late-events ()
  "The deadline kills both pipes and reports partial output exactly once."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-current-agent agent) results)
        (lispm-tool-invoke (lispm-get-tool "run_program")
                           '((program . "/bin/sh") (args . ["-c" "printf began; exec sleep 10"])
                             (timeout . 1))
                           (lambda (text errorp) (push (cons text errorp) results)))
        (let ((run (car lispm-process-tools--runs)))
          (should (lispm-test--wait (lambda () results) 4))
          (should (cdar results))
          (should (equal (alist-get 'status (lispm--json-decode (caar results))) "timed_out"))
          (should (equal (alist-get 'stdout (lispm--json-decode (caar results))) "began"))
          (should (lispm-process-tools-test--released-p run))
          (lispm-process-tools--collect run nil "late")
          (lispm-process-tools--finish run "exited" 0)
          (should (= (length results) 1)))))))

(ert-deftest lispm-process-tools-test-cancellation-owns-processes ()
  "Aborting an agent or closing its session stops direct invocations too."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let ((first (lispm-defagent first :root project))
            (second (lispm-defagent second :root project))
            results)
        (lispm-process-tools--start first "/bin/sh" ["-c" "exec sleep 10"] nil 15
                                   (lambda (&rest args) (push args results)))
        (let ((one (car lispm-process-tools--runs)))
          (lispm-process-tools--start second "/bin/sh" ["-c" "exec sleep 10"] nil 15
                                     (lambda (&rest args) (push args results)))
          (let ((two (car lispm-process-tools--runs)))
            (should (eq (process-get (lispm-process-tools--run-process one) 'lispm-agent) first))
            (lispm-set-state first 'aborting)
            (should (lispm-process-tools-test--released-p one))
            (should (process-live-p (lispm-process-tools--run-process two)))
            (run-hook-with-args 'lispm-session-end-functions second)
            (should (lispm-process-tools-test--released-p two))
            (should-not results)
            (should-not (memq #'lispm-process-tools--state-changed lispm-state-change-functions))))))))

(ert-deftest lispm-process-tools-test-terminal-close-and-spawn-failure ()
  "Bridge closure and failed startup leave no stderr pipes or lifecycle hooks."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let ((agent (lispm-defagent tester :root project))
            captured)
        (with-temp-buffer
          (setq-local lispm-agents-bridge--agent agent)
          (lispm-process-tools--start agent "/bin/sh" ["-c" "exec sleep 10"] nil 15 #'ignore)
          (let ((run (car lispm-process-tools--runs)))
            (run-hook-with-args 'lispm-agents-context-release-functions (current-buffer))
            (should (lispm-process-tools-test--released-p run))))
        (let ((original (symbol-function 'make-process)))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (when (equal (plist-get args :name) "lispm-program")
                         (setq captured (car lispm-process-tools--runs))
                         (error "Process startup failed"))
                       (apply original args))))
            (should-error (lispm-process-tools--start agent "/bin/sh" [] nil 1 #'ignore))))
        (should captured)
        (should (lispm-process-tools-test--released-p captured))
        (should-not lispm-process-tools--runs)))))

(ert-deftest lispm-process-tools-test-validation-and-policy ()
  "Invalid argv cannot spawn, and arbitrary executables always use shell policy."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let ((agent (lispm-defagent tester :root project))
            (tool (lispm-get-tool "run_program")))
        (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Unexpected process"))))
          (dolist (args '(["ok" 2] ["nul\0byte"] ("not" "vector")))
            (should-error (lispm-process-tools--start agent "printf" args nil 1 #'ignore)))
          (dolist (timeout '(0 -1 86401 1.5))
            (should-error (lispm-process-tools--start agent "printf" [] nil timeout #'ignore)))
          (should-error (lispm-process-tools--start agent "" [] nil 1 #'ignore))
          (should-error (lispm-process-tools--start agent "/lispm-missing-program" [] nil 1 #'ignore))
          (let ((lispm-tool-output-limit 1))
            (should-error (lispm-process-tools--start agent "printf" [] nil 1 #'ignore)))
          (should-error (lispm-process-tools--start agent "printf" [] "missing-directory" 1 #'ignore))
          (should-error (lispm-process-tools--start agent "printf" [] "/ssh:example.invalid:/work" 1 #'ignore))
          (setf (lispm-agent-root agent) "/ssh:example.invalid:/work/")
          (should-error (lispm-process-tools--start agent "printf" [] "/tmp" 1 #'ignore)))
        (setf (lispm-agent-root agent) project)
        (dolist (input '(((program . "git") (args . ["status"]))
                         ((program . "python3") (args . ["-c" "print(1)"]))
                         ((program . "/bin/sh") (args . ["-c" "true"]))))
          (should (eq (lispm-tool-call-permission tool input) 'shell))
          (should (eq (lispm-tool-action agent tool input) 'ask)))
        (setf (lispm-agent-policy agent) '((shell . deny)))
        (should (eq (lispm-tool-action agent tool '((program . "git") (args . ["status"]))) 'deny))))))

(ert-deftest lispm-process-tools-test-allowance-keeps-exact-argv ()
  "Remembered argv tolerates metadata changes, never a different operation."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let* ((agent (lispm-defagent tester :root project))
             (tool (lispm-get-tool "run_program"))
             (input '((program . "make") (args . ["test"]) (cwd . ".")
                      (timeout . 30) (why . "Verify"))))
        (push (lispm-tools-allowance tool input agent) (lispm-agent-allowed agent))
        (should (eq (lispm-tool-action agent tool
                                      '((timeout . 60) (cwd . ".") (args . ["test"])
                                        (program . "make") (why . "Again")))
                    'auto))
        (dolist (other '(((program . "make") (args . ["install"]) (cwd . "."))
                         ((program . "make") (args . ["test"]) (cwd . "../other"))
                         ((program . "other") (args . ["test"]) (cwd . "."))))
          (should (eq (lispm-tool-action agent tool other) 'ask)))
        (let ((lispm-tool-action-functions (list (lambda (&rest _) '(ask . "Review it.")))))
          (should (eq (lispm-tool-action agent tool input) 'ask)))))))

(ert-deftest lispm-process-tools-test-sandbox-routing ()
  "Exact argv goes through the configured sandbox and releases its control pipe."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (lispm-test-with-fake-srt log
        (let ((agent (lispm-defagent tester :root project))
              (lispm-sandbox-level 'middle))
          (pcase-let* ((`(,text . ,errorp)
                        (lispm-process-tools-test--call
                         agent '((program . "/bin/sh")
                                 (args . ["-c" "printf '%s' \"$1\"" "test" "a ; $(unknown)"]))))
                       (result (lispm--json-decode text)))
            (should-not errorp)
            (should (equal (alist-get 'stdout result) "a ; $(unknown)"))
            (should (file-exists-p (expand-file-name "settings.json" log)))
            (should-not lispm-sandbox--runs))
          (should-error (lispm-process-tools--start agent "/bin/sh" [] "/tmp" 1 #'ignore))
          (let ((lispm-sandbox-srt-program "/no/srt"))
            (should-error (lispm-process-tools--start agent "/bin/sh" [] nil 1 #'ignore)
                          :type 'lispm-sandbox-error))
          (should-not lispm-process-tools--runs))))))

(ert-deftest lispm-process-tools-test-sandbox-cwd-keeps-root-rules ()
  "The sandbox sees project-relative rules while the program uses its cwd."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (lispm-test-with-fake-srt log
        (let* ((agent (lispm-defagent tester :root project))
               (lispm-sandbox-level 'custom)
               (lispm-sandbox-custom '(:deny-read ("private") :allow-write (".")
                                      :deny-write ("locked")))
               (cwd (expand-file-name "sub $(unknown); space" project))
               (bin (expand-file-name "bin/" project))
               (program (expand-file-name "-program" bin))
               (exec-path (cons bin exec-path))
               (literal "a ; $(unknown) ' \""))
          (make-directory cwd)
          (make-directory bin)
          (with-temp-file program
            (insert "#!/bin/sh\nprintf '%s\\n' \"$1\"; pwd -P; printf done > written\n"))
          (set-file-modes program #o755)
          (with-temp-buffer
            (insert-file-contents lispm-sandbox-srt-program)
            (goto-char (point-min))
            (forward-line 2)
            (insert "pwd -P > \"$log/cwd\"\n")
            (write-region (point-min) (point-max) lispm-sandbox-srt-program nil 'silent))
          (pcase-let* ((`(,text . ,errorp)
                        (lispm-process-tools-test--call
                         agent `((program . "-program") (cwd . ,cwd)
                                 (args . ,(vector literal)))))
                       (result (lispm--json-decode text))
                       (settings (lispm-test--json-file (expand-file-name "settings.json" log))))
            (should-not errorp)
            (should (equal (alist-get 'stdout result)
                           (format "%s\n%s\n" literal (file-truename cwd))))
            (should (file-exists-p (expand-file-name "written" cwd)))
            (should-not (file-exists-p (expand-file-name "written" project)))
            (should (equal (with-temp-buffer
                             (insert-file-contents (expand-file-name "cwd" log))
                             (string-trim (buffer-string)))
                           (directory-file-name (file-truename project))))
            (let-alist settings
              (should (member "private" .filesystem.denyRead))
              (should (member "." .filesystem.allowWrite))
              (should (member "locked" .filesystem.denyWrite)))
            (should-not lispm-sandbox--runs)))))))

(ert-deftest lispm-process-tools-test-composes-with-run-lisp ()
  "A program's JSON result can be inspected by a run_lisp composition."
  (lispm-test-env
    (lispm-process-tools-test--with-runs
      (let* ((agent (lispm-defagent tester :root project :policy '((shell . auto))))
             (lispm-current-agent agent) result)
        (lispm-tool-invoke
         (lispm-get-tool "run_lisp")
         '((code . "(alist-get 'exit_code (json-parse-string (run_program :program \"/bin/sh\" :args [\"-c\" \"printf ok\"]) :object-type 'alist))"))
         (lambda (text errorp) (setq result (cons text errorp))))
        (should (lispm-test--wait (lambda () result)))
        (should-not (cdr result))
        (should (string-match-p "0" (car result)))))))

(provide 'lispm-process-tools-test)
;;; lispm-process-tools-test.el ends here
