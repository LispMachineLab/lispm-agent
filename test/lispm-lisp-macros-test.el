;;; lispm-lisp-macros-test.el --- Tests of macro tools and lispm-deftool -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests witnessing compile-time validation and `:edit' expansion in
;; `lispm-deftool', `lispm-tools-with-async-start', compile-time tool-call
;; checking in `run_lisp', and the macro tools in `lispm-lisp-macros.el'.

;;; Code:

(require 'lispm-test)
(require 'lispm-elisp)
(require 'lispm-elisp-test)
(require 'lispm-lisp-macros)

(ert-deftest lispm-lisp-macros-test-deftool-compile-checks ()
  "Macro `lispm-deftool' rejects invalid options and parameter specs at expansion time."
  (should-error
   (macroexpand-1 '(lispm-deftool bad_opt () "Doc." :unknown t "ok")))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_type ((x symbol "Doc.")) "Doc." x)))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_param_opt ((x string "Doc." :bogus 1)) "Doc." x)))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_enum ((x string "Doc." :enum "ab")) "Doc." x)))
  (should-error
   (macroexpand-1 '(lispm-deftool empty_body () "Doc." :permission read)))
  (should-error
   (macroexpand-1 '(lispm-deftool dup_param ((a-b string "Doc.") (a_b string "Doc.")) "Doc." a-b)))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_edit_opt ((path string "Path.")) "Doc."
                     :permission write
                     :edit (:file path :creates t :transform (s (cons s nil))) "ok")))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_buf_create ((buffer string "Buf.")) "Doc."
                     :permission write
                     :edit (:buffer buffer :create t :transform (s (cons s nil))) "ok")))
  (should-error
   (macroexpand-1 '(lispm-deftool bad_edit ((path string "Path.")) "Doc."
                     :permission write :diff #'ignore
                     :edit (:file path :transform (s (cons s nil))) "ok")))
  (should (get 'lispm-deftool 'edebug-form-spec))
  (let* ((tool (lispm-get-tool "edit_file"))
         (schema (lispm-tool-schema tool))
         (props (alist-get 'properties (alist-get 'input_schema schema))))
    (should (lispm-tool-param-schema tool))
    (should (assq 'why props))
    (should (assq 'old_text props))))

(ert-deftest lispm-lisp-macros-test-deftool-edit-clause-and-targets ()
  "Declarative `:edit' synthesizes `:diff', `:changes', `:targets' and write body."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (outside (expand-file-name "../outside-edit.txt" project)))
      (lispm-deftool lispm_test_upcase_target
          ((target string "File to uppercase."))
        "Uppercase TARGET."
        :permission write
        :edit (:file target
               :transform (text (cons (upcase text) (length text)))
               :bind (len label))
        (format "Uppercased %d chars in %s." len label))
      (unwind-protect
          (let ((lispm-current-agent agent)
                (tool (lispm-get-tool "lispm_test_upcase_target"))
                (file (expand-file-name "up.txt" project))
                result)
            (lispm-test--write file "hello\n")
            ;; Synthesized `:targets' allows inside-root auto writes, asks outside root.
            (should (eq (lispm-tool-action agent tool '((why . "w") (target . "up.txt"))) 'auto))
            (should (eq (lispm-tool-action agent tool `((why . "w") (target . ,outside))) 'ask))
            (should (equal (funcall (lispm-tool-diff tool) "up.txt")
                           '("hello\n" "HELLO\n" "up.txt")))
            (let ((change (lispm-tool-would-change tool '((target . "up.txt")))))
              (should (bufferp (car change)))
              (should (equal (cdr change) "HELLO\n")))
            (lispm-tool-invoke tool '((why . "test") (target . "up.txt"))
                               (lambda (text errorp &rest _)
                                 (setq result (cons text errorp))))
            (should (equal result '("Uppercased 6 chars in up.txt.")))
            (should (equal (lispm-test--file project "up.txt") "HELLO\n")))
        (ignore-errors (delete-file outside))
        (remhash "lispm_test_upcase_target" lispm-tools--registry)))))

(ert-deftest lispm-lisp-macros-test-async-start-guard ()
  "Macro `lispm-tools-with-async-start' rolls back on error or pending `C-g'."
  (let ((cleaned 0)
        (quit-flag nil))
    (should-error
     (lispm-tools-with-async-start (cl-incf cleaned)
       (error "Boom")))
    (should (= cleaned 1))
    (let (caught)
      (condition-case nil
          (lispm-tools-with-async-start (cl-incf cleaned)
            (setq quit-flag t))
        (quit (setq caught t)))
      (should caught)
      (should-not quit-flag)
      (should (= cleaned 2)))))

(ert-deftest lispm-lisp-macros-test-static-tool-call-validation ()
  "Invalid tool parameters in `run_lisp' fail at prepare time before any call runs."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (ran 0))
      (lispm-deftool lispm_test_touch () "Record side effect."
        (cl-incf ran)
        "touched")
      (unwind-protect
          (progn
            (lispm-tools-load agent '("lispm_test_touch"))
            (dolist (bad '("(progn (lispm_test_touch) (read_file \"a.txt\"))"
                           "(progn (lispm_test_touch) (read_file :bogus \"a.txt\"))"
                           "(progn (lispm_test_touch) (read_file :path \"a.txt\" :path \"b.txt\"))"
                           "(progn (lispm_test_touch) (read_file :path 'nil))"
                           "(progn (lispm_test_touch) (edit_file :path \"a.txt\" :old-text \"x\"))"
                           "(progn (lispm_test_touch) (read_file :path 42))"
                           "(progn (lispm_test_touch) (apropos :pattern \"x\" :kind \"bogus\"))"))
              (let ((res (lispm-test--lisp agent bad)))
                (should (cdr res))
                (should (string-prefix-p "Not allowed in run_lisp:" (car res)))
                (should (= ran 0)))))
        (remhash "lispm_test_touch" lispm-tools--registry)))))

(ert-deftest lispm-lisp-macros-test-denial-and-fallback ()
  "Macros `ignoring_call_denied' and `with_tool_fallback' handle only tool failures."
  (lispm-test-env
    (lispm-test--write (expand-file-name "ok.txt" project) "fallback-ok\n")
    (let ((agent (lispm-defagent tester :root project :policy '((write . deny)))))
      (should (equal (lispm-test--lisp
                      agent
                      "(list (ignoring_call_denied (write_file :path \"x.txt\" :content \"no\")) 'kept)")
                     '("Tool calls:\n- write_file x.txt (denied)\n\n=> (nil kept)")))
      (should (equal (lispm-test--lisp
                      agent
                      "(with_tool_fallback (read_file :path \"missing.txt\")
                         (string-trim (substring (read_file :path \"ok.txt\") 7)))")
                     '("Tool calls:\n- read_file missing.txt (failed)\n- read_file ok.txt\n\n=> fallback-ok")))
      ;; Plain Lisp type errors are not swallowed by `with_tool_fallback'.
      (let ((res (lispm-test--lisp agent "(with_tool_fallback (+ 'not-a-number 1) 'no)")))
        (should (cdr res))))))

(ert-deftest lispm-lisp-macros-test-with-terminal-replay-and-retry ()
  "Macro `with_terminal' skips `terminal_leave' on pause and invalidates unwound retry logs."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "old\n")
    (let ((agent (lispm-defagent tester :root project))
          (pre-runs 0)
          (events nil))
      (lispm-deftool lispm_test_pre () "Step before terminal."
        :deferred t
        (cl-incf pre-runs)
        "pre")
      (lispm-deftool terminal_enter () "Enter terminal."
        :deferred t
        (when (eq events 'fail-enter) (error "Cannot enter"))
        (push 'enter events)
        "entered")
      (lispm-deftool terminal_leave () "Leave terminal."
        :deferred t
        (push 'leave events)
        "left")
      (unwind-protect
          (progn
            (lispm-tools-load agent '("lispm_test_pre" "terminal_enter" "terminal_leave"))
            ;; When `terminal_enter' itself fails, `terminal_leave' must NOT run.
            (setq events 'fail-enter)
            (let ((fail-res (lispm-test--lisp agent "(with_terminal 'body)")))
              (should (cdr fail-res))
              (should (eq events 'fail-enter)))
            (setq events nil)
            ;; Failure inside `with_terminal' unwinds via `terminal_leave', and a `:lisp-log'
            ;; retry keeps `lispm_test_pre' cached while re-running `terminal_enter'.
            (let* ((call (list :id "c1" :name "run_lisp"))
                   (code "(list (lispm_test_pre)
                                (with_terminal (read_file :path \"later.txt\")))")
                   (lispm-current-agent agent)
                   (lispm-current-call "c1")
                   res1 res2)
              (puthash "c1" call (lispm-agent-pending agent))
              (lispm-tool-invoke (lispm-get-tool "run_lisp") `((code . ,code))
                                 (lambda (txt err &rest _) (setq res1 (cons txt err))))
              (should (lispm-test--wait (lambda () res1)))
              (should (cdr res1))
              (should (= pre-runs 1))
              (should (equal (nreverse events) '(enter leave)))
              (lispm-test--write (expand-file-name "later.txt" project) "arrived\n")
              (setq events nil)
              (lispm-tool-invoke (lispm-get-tool "run_lisp") `((code . ,code))
                                 (lambda (txt err &rest _) (setq res2 (cons txt err))))
              (should (lispm-test--wait (lambda () res2)))
              (should-not (cdr res2))
              (should (= pre-runs 1))
              (should (equal (nreverse events) '(enter leave)))
              (remhash "c1" (lispm-agent-pending agent)))
            ;; Approval suspension mid-body does not trigger `terminal_leave' early.
            (setq events nil)
            (setq lispm-test--scripts
                  `((tester
                     ,(lispm-test--msg
                       "tool_use"
                       (lispm-test--tool
                        "t1" "run_lisp"
                        '((code . "(with_terminal
                                     (edit_file :path \"a.txt\" :old-text \"old\" :new-text \"new\")
                                     'done)"))))
                     ,(lispm-test--msg "end_turn" (lispm-test--text "Finished.")))))
            (lispm-tell agent "edit in terminal")
            (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
            (should (equal events '(enter)))
            (lispm-resolve agent "t1" 'approve)
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (should (equal (nreverse events) '(enter leave)))
            (should (equal (lispm-test--file project "a.txt") "new\n")))
        (remhash "lispm_test_pre" lispm-tools--registry)
        (remhash "terminal_enter" lispm-tools--registry)
        (remhash "terminal_leave" lispm-tools--registry)))))

(ert-deftest lispm-lisp-macros-test-with-worktree-order-and-hygiene ()
  "Macro `with_worktree' preserves source evaluation order and rejects duplicate options."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (calls nil))
      (lispm-deftool worktree_ask
          ((role string "Role.")
           (request string "Request.")
           (name string "Name." :optional t)
           (base string "Base." :optional t))
        "Stub worktree_ask."
        :permission write :deferred t
        (when (equal role "missing-role") (error "No role"))
        (push (list 'ask role request name base) calls)
        (format "work in %s" name))
      (lispm-deftool worktree_remove
          ((name string "Name.")
           (force boolean "Force." :optional t))
        "Stub worktree_remove."
        :permission write :deferred t
        (push (list 'remove name force) calls)
        "removed")
      (unwind-protect
          (progn
            (lispm-tools-load agent '("worktree_ask" "worktree_remove"))
            ;; Failed `worktree_ask' never invokes `worktree_remove'.
            (let ((bad-ask (lispm-test--lisp
                            agent
                            "(with_worktree (wt :role \"missing-role\" :request \"q\") wt)")))
              (should (cdr bad-ask))
              (should-not calls))
            ;; Duplicate `:role' is refused at compile time.
            (let ((dup (lispm-test--lisp
                        agent
                        "(with_worktree (wt :role \"a\" :role \"b\" :request \"q\") wt)")))
              (should (cdr dup))
              (should (string-search "duplicate option :role" (car dup))))
            ;; Left-to-right order: `:request' written before `:role' evaluates first.
            (let ((res (lispm-test--lisp
                        agent
                        "(let (order)
                           (with_worktree (wt :request (progn (push 'req order) \"fix bug\")
                                              :role (progn (push 'role order) \"coder\"))
                             (list (nreverse order) (plist-get wt :branch) (plist-get wt :reply))))")))
              (should-not (cdr res))
              (pcase-let ((`((ask "coder" "fix bug" ,asked-name nil)
                             (remove ,removed-name t))
                           (nreverse calls)))
                (should (equal asked-name removed-name))
                (should (string-search (format "((req role) \"lispm/%s\" \"work in %s\")"
                                               asked-name asked-name)
                                       (car res)))))
            (setq calls nil)
            (let ((res (lispm-test--lisp
                        agent
                        "(with_worktree (_wt :role \"coder\" :request \"keep it\" :name \"kept\" :keep t)
                           'kept-ok)")))
              (should-not (cdr res))
              (should (equal calls '((ask "coder" "keep it" "kept" nil))))))
        (remhash "worktree_ask" lispm-tools--registry)
        (remhash "worktree_remove" lispm-tools--registry)))))

(ert-deftest lispm-lisp-macros-test-elisp-forms-order-and-conditions ()
  "Code-quoting macros preserve option order, structured conditions and `cl-flet' hygiene."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((exec . auto)))))
      ;; Duplicate `:bind' is refused at prepare time.
      (let ((dup (lispm-test--lisp agent "(eval_elisp_forms (:bind () :bind ()) 1)")))
        (should (cdr dup))
        (should (string-search "duplicate option :bind" (car dup))))
      ;; Structured condition `lispm-lisp-serialization-error' can be caught by type.
      (should (equal (lispm-test--lisp
                      agent
                      "(with-temp-buffer
                         (let ((b (current-buffer)))
                           (condition-case nil
                               (eval_elisp_forms (:bind (b)) b)
                             (lispm-lisp-serialization-error 'caught-structured))))")
                     '("=> caught-structured")))
      ;; Inside `cl-flet', `:bind' calls local functions while `let' variable names,
      ;; backquoted `pcase' patterns and direct `cl-function' keep exact Lisp hygiene.
      (let ((res (lispm-test--lisp
                  agent
                  "(cl-flet ((shout (s) (concat (upcase s) \"!\")))
                     (let ((with_terminal 42))
                       (list with_terminal
                             (pcase '(transform_file 7) (`(transform_file ,n) n))
                             (funcall (cl-function
                                       (lambda ()
                                         (eval_elisp_forms (:bind ((msg (shout \"ok\"))))
                                           (with-temp-buffer (insert msg) (buffer-string)))))))))")))
        (should-not (cdr res))
        (should (string-search "(42 7 \"=> OK!\")" (car res))))
      ;; Shadowing a macro tool in `cl-flet' to smuggle `eval-when-compile' is refused.
      (let ((smuggle (lispm-test--lisp
                      agent
                      "(cl-flet ((eval_elisp_forms (x) x))
                         (eval_elisp_forms (eval-when-compile (delete-file \"a\"))))")))
        (should (cdr smuggle))
        (should (string-prefix-p "Not allowed in run_lisp: redefining macro eval_elisp_forms"
                                 (car smuggle))))
      ;; Left-to-right option order in `run_elisp_forms': `:timeout' before `:bind'.
      (lispm-elisp-test--with-kernel
        (let ((lispm-current-agent agent)
              result)
          (lispm-tool-invoke
           (lispm-get-tool "run_lisp")
           '((code . "(let (order)
                        (list
                         (run_elisp_forms (:timeout (progn (push 'timeout order) 15)
                                           :bind ((xs (progn (push 'bind order) '(10 20 30)))))
                           (with-temp-file \"ignored\" (ignore))
                           (apply #'+ xs))
                         (nreverse order)))"))
           (lambda (text errorp &rest _) (setq result (cons text errorp))))
          (should (= (length kernel-submissions) 1))
          (let ((callback (cdr (assq 1 kernel-callbacks))))
            (funcall callback 'chunk "=> 60")
            (funcall callback 'success '(:exit 0 :stderr "")))
          (should (lispm-test--wait (lambda () result)))
          (should-not (cdr result))
          (should (string-search "(\"=> 60\" (timeout bind))" (car result))))))))

(ert-deftest lispm-lisp-macros-test-transform-file-roundtrip-and-flet ()
  "Macro `transform_file' preserves exact newlines, handles `cl-flet' and media fallback."
  (lispm-test-env
    (lispm-test--write (expand-file-name "no-nl.txt" project) "hello")
    (lispm-test--write (expand-file-name "blank-nl.txt" project) "\n")
    (lispm-test--write (expand-file-name "empty.txt" project) "")
    (lispm-test--write (expand-file-name "pic.png" project) "\x89PNG\r\n\x1a\n")
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto)))))
      ;; File without trailing newline: identity is a no-op; replacement succeeds via `edit_file'.
      (should (equal (lispm-test--lisp agent "(transform_file (s \"no-nl.txt\") s)")
                     '("Tool calls:\n- read_file no-nl.txt\n\n=> nil")))
      (should (equal (lispm-test--lisp
                      agent
                      "(cl-flet ((s (x) (upcase x)))
                         (transform_file (s \"no-nl.txt\") (s s)))")
                     '("Tool calls:\n- read_file no-nl.txt\n- edit_file no-nl.txt\n\n=> t")))
      (should (equal (lispm-test--file project "no-nl.txt") "HELLO"))
      ;; Single-newline file `"\n"' transformed to `""' is detected and written.
      (should (equal (lispm-test--lisp agent "(transform_file (_ \"blank-nl.txt\") \"\")")
                     '("Tool calls:\n- read_file blank-nl.txt\n- edit_file blank-nl.txt\n\n=> t")))
      (should (equal (lispm-test--file project "blank-nl.txt") ""))
      ;; Empty file uses `write_file'.
      (should (equal (lispm-test--lisp agent "(transform_file (_ \"empty.txt\") \"seeded\\n\")")
                     '("Tool calls:\n- read_file empty.txt\n- write_file empty.txt\n\n=> t")))
      ;; Org line reference `[[file:PATH::LINE]]' still reads and edits the whole file from line 1.
      (lispm-test--write (expand-file-name "lines.txt" project) "one\ntwo\nthree\n")
      (should (equal (lispm-test--lisp
                      agent
                      "(transform_file (s \"[[file:lines.txt::3]]\")
                         (string-replace \"one\" \"ONE\" s))")
                     '("Tool calls:\n- read_file [[file:lines.txt::3]]\n- edit_file [[file:lines.txt::3]]\n\n=> t")))
      (should (equal (lispm-test--file project "lines.txt") "ONE\ntwo\nthree\n"))
      ;; Media file signals `lispm-call-failed' (catchable by `with_tool_fallback'),
      ;; and non-string body return signals structured `lispm-lisp-type-error'.
      (should (equal (lispm-test--lisp
                      agent
                      "(with_tool_fallback (transform_file (s \"pic.png\") s) 'media-skipped)")
                     '("Tool calls:\n- read_file pic.png\n\n=> media-skipped")))
      (should (equal (lispm-test--lisp
                      agent
                      "(condition-case nil (transform_file (_ \"no-nl.txt\") 123)
                         (lispm-lisp-type-error 'type-caught))")
                     '("Tool calls:\n- read_file no-nl.txt\n\n=> type-caught"))))))

(provide 'lispm-lisp-macros-test)
;;; lispm-lisp-macros-test.el ends here
