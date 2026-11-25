;;; lispm-layers-test.el --- Architecture layer and error hierarchy tests (#191)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Regression and invariant tests for Phase 2 PR 5 (#191):
;;   1. `lispm-error' structured condition hierarchy and run_lisp uncatchable guard.
;;   2. Observable state/model hooks (`lispm-emit-change', `lispm-model-change-functions')
;;      and UI dispatch indirection replacing upward calls.
;;   3. Strict 5-layer DAG compliance via `lispm-check-boundaries-report'.

;;; Code:

(require 'ert)
(require 'lispm-test)
(require 'check-boundaries)

;;;; 1. Structured error hierarchy

(ert-deftest lispm-layers-test-error-hierarchy ()
  "LispM condition symbols inherit from structured parent conditions."
  (dolist (pair '((lispm-api-error . lispm-error)
                  (lispm-tool-error . lispm-error)
                  (lispm-tool-argument-error . lispm-tool-error)
                  (lispm-call-failed . lispm-tool-error)
                  (lispm-call-denied . lispm-call-failed)
                  (lispm-lisp-serialization-error . lispm-tool-error)
                  (lispm-lisp-type-error . lispm-tool-error)
                  (lispm-mcp-error . lispm-error)
                  (lispm-sandbox-error . lispm-error)
                  (lispm-sandbox-error . file-error)
                  (lispm-session-error . lispm-error)
                  (lispm-collab-error . lispm-error)))
    (should (memq (cdr pair) (get (car pair) 'error-conditions))))
  ;; `lispm-tool-argument-error' is caught by both `lispm-tool-error' and `lispm-error'.
  (should (eq 'caught
              (condition-case nil
                  (signal 'lispm-tool-argument-error '("bad"))
                (lispm-tool-error 'caught))))
  (should (eq 'caught
              (condition-case nil
                  (signal 'lispm-call-denied '("denied"))
                (lispm-error 'caught))))
  ;; `lispm-lisp-refused' must stay outside `error' so sandboxed Lisp programs cannot catch it.
  (should-not (memq 'error (get 'lispm-lisp-refused 'error-conditions))))

;;;; 2. Observable hooks and UI dispatch indirection

(ert-deftest lispm-layers-test-hooks-and-ui-dispatch ()
  "Runtime emits observable changes and UI dispatch avoids upward coupling."
  (let (changes)
    (cl-letf ((lispm-change-functions
               (list (lambda (subject fields &optional event)
                       (push (list subject fields event) changes)))))
      (lispm-emit-change 'agent-1 '(parent) '(:type test))
      (should (equal changes '((agent-1 (parent) (:type test))))))
    (let ((lispm-change-functions nil))
      (lispm-emit-change 'agent-1 '(parent))
      (should (= (length changes) 1))))
  ;; Model change hook triggers chat redraw when a live view exists.
  (lispm-test-env
    (let* ((agent (lispm-defagent hooker :root project))
           (chat (lispm-chat agent))
           (redrawn nil))
      (ignore chat)
      (cl-letf (((symbol-function 'lispm-chat-redraw-now)
                 (lambda () (setq redrawn t))))
        (lispm--restart-use-model agent nil (cons nil (lispm-agent-model* agent)))
        (should redrawn))))
  ;; Public `lispm-content-text' and `lispm-asked' live in lower layers.
  (should (equal (lispm-content-text [((type . "text") (text . " hello "))]) "hello"))
  (let ((asked (lispm-asked '(:name "read_file" :input ((path . "a.txt"))))))
    (should (equal (plist-get asked :name) "read_file"))
    (should (eq (plist-get asked :tool) (lispm-get-tool "read_file"))))
  ;; Upper UI dispatch functions are wired by `lispm' and `lispm-chat'.
  (should (functionp lispm-show-agent-function))
  (should (functionp lispm-open-transcript-function))
  (should (functionp lispm-open-dashboard-function))
  (should (functionp lispm-read-session-function)))

;;;; 3. Strict DAG boundaries

(ert-deftest lispm-layers-test-boundaries-report ()
  "Every LispM module respects the 5-layer DAG with zero cycles or upward deps."
  (let ((report (lispm-check-boundaries-report
                 (file-name-directory (directory-file-name lispm-test--dir)))))
    (should (plist-get report :ok))
    (should (null (plist-get report :cycles)))
    (should (null (plist-get report :errors)))
    (should (>= (plist-get report :modules) 60))))

(ert-deftest lispm-layers-test-private-encapsulation ()
  "Boundary verifier catches cross-module `--' leaks and duplicate `--' defs."
  (let* ((root (make-temp-file "lispm-bounds-" t))
         (lisp-dir (expand-file-name "lisp" root))
         (lispm-boundaries-layers '((0 . ("lispm-mod-a"))
                                    (1 . ("lispm-mod-b")))))
    (unwind-protect
        (progn
          (make-directory lisp-dir t)
          (with-temp-file (expand-file-name "Makefile" root)
            (insert "ELS = lisp/lispm-mod-a.el lisp/lispm-mod-b.el\n"))
          (with-temp-file (expand-file-name "lispm-mod-a.el" lisp-dir)
            (insert "(defun lispm-mod-a--secret () t)\n"
                    "(defvar lispm-shared--var 1)\n"
                    "(defun lispm-shared--var () 2)\n"
                    "(provide 'lispm-mod-a)\n"))
          (with-temp-file (expand-file-name "lispm-mod-b.el" lisp-dir)
            (insert "(require 'lispm-mod-a)\n"
                    "(declare-function lispm-mod-a--secret \"lispm-mod-a\" ())\n"
                    "(defvar lispm-shared--var 9)\n"
                    "(defun lispm-mod-b-use () (lispm-mod-a--secret))\n"
                    "(provide 'lispm-mod-b)\n"))
          (let* ((report (lispm-check-boundaries-report root))
                 (errors (plist-get report :errors)))
            (should-not (plist-get report :ok))
            (should (seq-some (lambda (e) (string-match-p "Cross-module private reference" e))
                              errors))
            (should (seq-some (lambda (e) (string-match-p "Duplicate private definition.*lispm-shared--var" e))
                              errors))))
      (delete-directory root t))))

(provide 'lispm-layers-test)
;;; lispm-layers-test.el ends here
