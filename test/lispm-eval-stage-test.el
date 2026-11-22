;;; lispm-eval-stage-test.el --- Tests for Lisp, ERT and web stage views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests witnessing `eval_elisp' / `run_lisp' worksheet rendering,
;; `eval_definition' source spotlighting, `ert_run' pass/fail scoreboards,
;; and `web_fetch' / `web_act' stage rendering with clickable URLs.

;;; Code:

(require 'ert)
(require 'lispm)
(require 'lispm-test)

(defmacro lispm-eval-stage-test--with-env (&rest body)
  "Run BODY in `lispm-test-env' with all stage visualizers active."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-edits--in-batch t)
           (lispm-edits--shows nil)
           (lispm-edits--proposals nil)
           (lispm-edits--writing (make-hash-table :test #'equal))
           (lispm-spotlight--in-batch t)
           (lispm-spotlight--beams nil)
           (lispm-spotlight--steps (make-hash-table :test #'eq))
           (lispm-spotlight--ephemeral-buffers nil)
           (lispm-spotlight--scroll-lock nil)
           (lispm-shell-stage--in-batch t)
           (lispm-shell-stage--sessions (make-hash-table :test #'eq))
           (lispm-eval-stage--in-batch t)
           (lispm-eval-stage--sessions (make-hash-table :test #'eq)))
       (unwind-protect
           (progn ,@body)
         (clrhash lispm-eval-stage--sessions)
         (lispm-eval-stage--tick)
         (lispm-edits--tick 1e10)
         (lispm-spotlight--tick 1e10)))))

(ert-deftest lispm-eval-stage-test-pure-parsers ()
  "ERT report and web result parsers extract structured metadata."
  (should (equal (lispm-eval-stage--parse-ert-summary
                  "Ran 8 tests in 0.02 s: 6 passed, 2 failed.\n\nFAILED  foo")
                 '(:total 8 :passed 6 :failed 2)))
  (should (equal (lispm-eval-stage--parse-web-result
                  "Title: Example\nURL: https://example.org/login\nAction: Clicked [2] button\n\nWelcome")
                 '(:title "Example"
                   :url "https://example.org/login"
                   :action "Clicked [2] button"
                   :body "Welcome"))))

(ert-deftest lispm-eval-stage-test-eval-elisp-and-run-lisp ()
  "Lisp evaluations show syntax-highlighted code and values on stage."
  (lispm-eval-stage-test--with-env
    (let* ((agent (lispm-defagent hacker "Hacker." :root project))
           (chat (lispm-chat agent)))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer chat)
        (lispm-test--call-async
         agent "eval_elisp"
         '((code . "(let ((xs '(1 2 3))) (princ \"sum:\") (apply #'+ xs))")))
        (let ((buf (lispm-eval-stage--buffer agent 'lisp)))
          (should (get-buffer-window buf))
          (with-current-buffer buf
            (should-not display-line-numbers)
            (should (string-search ";; Code" (buffer-string)))
            (should (string-search "sum:" (buffer-string)))
            (should (string-search "=> 6" (buffer-string)))
            (should (string-search "✓" lispm-eval-stage--header))))
        ;; `run_lisp' error renders in fail face and marks header with ✗.
        (lispm-test--call-async agent "eval_elisp"
                                '((code . "(error \"boom\")")))
        (with-current-buffer (lispm-eval-stage--buffer agent 'lisp)
          (should (string-search "boom" (buffer-string)))
          (should (string-search "✗" lispm-eval-stage--header)))))))

(ert-deftest lispm-eval-stage-test-eval-definition-and-ert-scoreboard ()
  "Evaluating a definition lights its source; `ert_run' renders a dot bar."
  (lispm-eval-stage-test--with-env
    (let* ((src (expand-file-name "calc.el" project))
           (tests (expand-file-name "calc-test.el" project))
           (agent (lispm-defagent dev "Developer." :root project))
           (chat (lispm-chat agent)))
      (lispm-test--write
       src
       ";;; calc.el --- calc  -*- lexical-binding: t; -*-\n\n(defun lispm-eval-stage-sample-add (a b)\n  \"Add A and B.\"\n  (+ a b))\n\n(provide 'calc)\n")
      (lispm-test--write
       tests
       ";;; calc-test.el --- tests  -*- lexical-binding: t; -*-\n(require 'ert)\n(ert-deftest lispm-eval-stage-sample-pass () (should (= 2 2)))\n(ert-deftest lispm-eval-stage-sample-fail () (should (= 2 3)))\n")
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer chat)
            ;; 1. `eval_definition' illuminates the `defun' form in calc.el.
            (lispm-test--call agent "eval_definition"
                              '((name . "lispm-eval-stage-sample-add")
                                (path . "calc.el")))
            (let ((calc-buf (find-buffer-visiting src)))
              (should (get-buffer-window calc-buf))
              (with-current-buffer calc-buf
                (let ((tag-ov (seq-find (lambda (o)
                                          (and (overlay-get o 'lispm-spotlight)
                                               (overlay-get o 'before-string)))
                                        (overlays-in (point-min) (point-max)))))
                  (should tag-ov)
                  (should (string-match-p
                           "evaluated lispm-eval-stage-sample-add"
                           (substring-no-properties
                            (overlay-get tag-ov 'before-string)))))))
            ;; 2. `ert_run' shows ●✗ scoreboard and clickable failed test symbol.
            (lispm-test--call agent "ert_run"
                              '((selector . "^lispm-eval-stage-sample-")
                                (file . "calc-test.el")))
            (let ((buf (lispm-eval-stage--buffer agent 'lisp)))
              (should (get-buffer-window buf))
              (with-current-buffer buf
                (should (string-search "●✗" (buffer-string)))
                (should (string-search "✗" lispm-eval-stage--header))
                (goto-char (point-min))
                (should (search-forward "lispm-eval-stage-sample-fail" nil t))
                (should (equal (get-text-property (match-beginning 0) 'lispm-object)
                               '(symbol lispm-eval-stage-sample-fail))))))
        (fmakunbound 'lispm-eval-stage-sample-add)
        (put 'lispm-eval-stage-sample-pass 'ert--test nil)
        (put 'lispm-eval-stage-sample-fail 'ert--test nil)))))

(ert-deftest lispm-eval-stage-test-web-stage-view ()
  "Browser tool calls render title, action banner, and clickable URL links."
  (lispm-eval-stage-test--with-env
    (let* ((agent (lispm-defagent surfer "Browser agent." :root project))
           (chat (lispm-chat agent))
           (fake-tool (lispm-tool--create :name "web_act"
                                          :description "Fake web_act"
                                          :permission 'web)))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer chat)
        (lispm-eval-stage--on-tool-call
         agent 'start fake-tool
         '((url . "https://example.org") (action . "click") (target . "2"))
         nil nil)
        (let ((web-buf (lispm-eval-stage--buffer agent 'web)))
          (should (get-buffer-window web-buf))
          (lispm-eval-stage--on-tool-call
           agent 'finish fake-tool
           '((url . "https://example.org") (action . "click") (target . "2"))
           "Title: LispM Portal\nURL: https://example.org/dashboard\nAction: Clicked [2] button \"Sign in\"\n\nHello LispM!\n[1] link \"Docs\" -> https://example.org/docs"
           nil)
          (with-current-buffer web-buf
            (should (string-search "LispM Portal" (buffer-string)))
            (should (string-search "⚡ Clicked [2] button \"Sign in\"" (buffer-string)))
            (goto-char (point-min))
            (should (search-forward "https://example.org/dashboard" nil t))
            (should (equal (get-text-property (match-beginning 0) 'lispm-object)
                           '(url "https://example.org/dashboard")))))))))

(provide 'lispm-eval-stage-test)
;;; lispm-eval-stage-test.el ends here
