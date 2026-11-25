;;; lispm-unify-test.el --- Architecture unification law tests (#190)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Regression and invariant tests for Phase 1 PR 4 (#190):
;;   1. `lispm-reply-route' wire bijection and unified delegation dispatch.
;;   2. Provider-agnostic `lispm-api-envelope' and canonical thought blocks.
;;   3. Shared `lispm-stage-runner' controller and decoupled stage modules.
;;   4. Canonical `lispm-tool-spec' metadata across LispM and MCP/IDE bridges.
;;   5. Clean `lispm-settings-mcp-*' namespace free of `lispm-mcp-server-*' collisions.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lispm-test)
(require 'lispm-mcp-server)

;;;; 1. Structured reply route bijection and delegation

(ert-deftest lispm-unify-test-reply-route-bijection ()
  "Structured reply routes round-trip bijectively across all modes and flags."
  (dolist (mode '(call inbox))
    (dolist (retire '(nil t))
      (let* ((route (lispm-reply-route--create
                     :caller-id "sess-42" :request-id "call-9"
                     :mode mode :retire retire))
             (wire (lispm-reply-route-encode route))
             (decoded (lispm-reply-route-decode wire)))
        (should (stringp wire))
        (should (lispm-reply-route-p decoded))
        (should (equal (lispm-reply-route-caller-id decoded) "sess-42"))
        (should (equal (lispm-reply-route-request-id decoded) "call-9"))
        (should (eq (lispm-reply-route-mode decoded) mode))
        (should (eq (lispm-reply-route-retire decoded) retire))
        (should (equal (lispm-reply-route-encode wire) wire))
        (should (eq (lispm-reply-route-decode route) route)))))
  (should-not (lispm-reply-route-encode nil))
  (should-not (lispm-reply-route-decode nil))
  (should-not (lispm-reply-route-decode ""))
  (should-not (lispm-reply-route-decode "only-one-token"))
  (should-not (lispm-reply-route-decode 123)))

(ert-deftest lispm-unify-test-answering-p-and-dispatch ()
  "`lispm-collab--answering-p' decodes routes without prefix false positives."
  (let* ((caller (lispm-make-principal 'boss "/tmp" :id "sess-1" :role 'assistant))
         (target (lispm-agent--create
                  :name 'worker :id "sess-2" :root "/tmp"
                  :entry '(:text "hi" :reply-to "sess-1 c10 retire")
                  :inbox (list '(:text "q" :reply-to "sess-1 c2 inbox")))))
    (should-not (lispm-collab--answering-p target caller "c1"))
    (should (lispm-collab--answering-p target caller "c10"))
    (should (lispm-collab--answering-p target caller "c2"))
    (setf (lispm-agent-entry target)
          (list :text "struct"
                :reply-to (lispm-reply-route--create
                           :caller-id "sess-1" :request-id "c99" :mode 'call)))
    (should (lispm-collab--answering-p target caller "c99"))))

;;;; 2. Provider-agnostic API envelope and canonical thought block

(ert-deftest lispm-unify-test-api-envelope-and-thought-block ()
  "Requests return `lispm-api-envelope' and thoughts normalize canonically."
  (let* ((anthropic-env (lispm-api-request
                         (alist-get 'anthropic lispm-backends)
                         "claude-opus-5-5" "medium" "sys" nil
                         '(((role . "user") (content . [((type . "text") (text . "hi"))])))))
         (openai-env (lispm-api-request
                      '(:api openai :features (streaming))
                      "gpt-5" "low" "sys" nil
                      '(((role . "user") (content . [((type . "text") (text . "hi"))]))))))
    (should (lispm-api-envelope-p anthropic-env))
    (should (lispm-api-envelope-p openai-env))
    (should (lispm-api-envelope-headers anthropic-env))
    (should-not (lispm-api-envelope-headers openai-env))
    (should (equal (lispm-anthropic-headers "k" (lispm-api-envelope-headers anthropic-env))
                   (append '("content-type: application/json"
                             "anthropic-version: 2023-06-01"
                             "x-api-key: k")
                           (lispm-api-envelope-headers anthropic-env))))
    (should (equal (lispm-openai-headers "k" '("x-extra: 1"))
                   '("content-type: application/json"
                     "authorization: Bearer k"
                     "x-extra: 1"))))
  (let* ((oai-msg (lispm-openai-whole
                   '((choices . [((finish_reason . "stop")
                                  (message . ((reasoning_content . "Private thought")
                                              (content . "Public answer"))))])
                     (usage . ((prompt_tokens . 3) (completion_tokens . 2))))))
         (oai-thought (aref (alist-get 'content oai-msg) 0))
         (signed (lispm-thought-block "Signed" "sig-1"))
         (legacy '((type . "reasoning") (text . "Legacy thought"))))
    (should (equal oai-thought '((type . "thinking") (thinking . "Private thought") (ephemeral . t))))
    (should (lispm-thought-block-p oai-thought))
    (should (lispm-thought-block-p signed))
    (should (lispm-thought-block-p legacy))
    (should (equal (lispm-thought-block-text legacy) "Legacy thought"))
    (should-not (lispm-block-replayable-p oai-thought))
    (should-not (lispm-block-replayable-p legacy))
    (should (lispm-block-replayable-p signed))
    (should (lispm-block-replayable-p '((type . "redacted_thinking") (data . "abc")))))
  ;; Both canonical ephemeral thinking and legacy on-disk reasoning render in chat, never replay.
  (with-temp-buffer
    (insert "#+title: Test\n\n* User\nhi\n\n* Assistant\n"
            (lispm-transcript--raw (lispm-thought-block "Canon" nil t))
            (lispm-transcript--raw '((type . "reasoning") (text . "Legacy")))
            (lispm-transcript--raw (lispm-thought-block "Replay" "sig"))
            "** Reply\nDone\n")
    (let* ((parsed (lispm-transcript-parse))
           (assistant-msg (nth 1 (plist-get parsed :messages)))
           (blocks (append (alist-get 'content assistant-msg) nil))
           (sections (lispm-transcript-sections))
           (assistant-sec (nth 1 sections))
           (chat-items (lispm-chat--assistant-items assistant-sec)))
      (should (= (length blocks) 2))
      (should (equal (alist-get 'thinking (car blocks)) "Replay"))
      (should (= (length (seq-filter (lambda (it) (eq (plist-get it :kind) 'thought)) chat-items)) 3)))))

;;;; 3. Shared stage runner and decoupled stage modules

(ert-deftest lispm-unify-test-stage-runner ()
  "Stage runner centralizes headers, buffers, timers, and decouples eval from shell."
  (let* ((principal (lispm-make-principal 'runner-p "/tmp" :role 'assistant))
         (hdr (lispm-stage-runner-header principal "cmd arg" "ok" 'lispm-stage-runner-ok))
         (buf (lispm-stage-runner-buffer "unit" principal 'lispm-eval-stage-mode))
         (sessions (make-hash-table :test #'eq))
         (lispm-eval-stage--ticker nil)
         (ticks 0))
    (unwind-protect
        (progn
          (should (string-search "runner-p" hdr))
          (should (string-search "cmd arg" hdr))
          (should (equal (buffer-name buf) "*lispm-stage-unit: runner-p*"))
          (with-current-buffer buf
            (should (derived-mode-p 'lispm-eval-stage-mode))
            (lispm-stage-runner-set-header buf 'lispm-eval-stage-mode 'lispm-eval-stage--header hdr)
            (should (equal lispm-eval-stage--header hdr)))
          (puthash principal 'fake-session sessions)
          (lispm-stage-runner-ensure-ticker sessions 'lispm-eval-stage--ticker #'ignore 60)
          (should (timerp lispm-eval-stage--ticker))
          (lispm-stage-runner-tick sessions 'lispm-eval-stage--ticker
                                   (lambda (s _now)
                                     (when (eq s 'fake-session)
                                       (cl-incf ticks))))
          (should (= ticks 1))
          (clrhash sessions)
          (lispm-stage-runner-tick sessions 'lispm-eval-stage--ticker #'ignore)
          (should-not lispm-eval-stage--ticker))
      (when (timerp lispm-eval-stage--ticker) (cancel-timer lispm-eval-stage--ticker))
      (when (buffer-live-p buf) (kill-buffer buf))))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "../lisp/lispm-eval-stage.el" lispm-test--dir))
    (should (search-forward "(require 'lispm-stage-runner)" nil t))
    (goto-char (point-min))
    (should-not (search-forward "(require 'lispm-shell-stage)" nil t))))

;;;; 4. Unified tool metadata specs

(ert-deftest lispm-unify-test-tool-spec-unification ()
  "Built-in tools, context tools, IDE tools, and bridge tools share `lispm-tool-spec'."
  (dolist (group (list lispm-agents-context-tools
                       lispm-agents-mcp-tools
                       lispm-agents-ide-tools
                       lispm-agents-bridge-tools))
    (should group)
    (should (cl-every #'lispm-tool-spec-p group)))
  ;; Zero-param and param-based LispM tools convert cleanly and serialize without `false'.
  (let* ((zero-tool (lispm-get-tool "list_buffers"))
         (zero-spec (lispm-tool-to-spec zero-tool))
         (zero-json (json-serialize (lispm-tool-spec-resolved-schema zero-spec)))
         (edit-tool (lispm-get-tool "edit_file"))
         (mcp-spec (lispm-mcp-server--tool edit-tool))
         (legacy-spec (lispm-tool-spec-normalize
                       (list "ping" #'ignore nil nil "Ping tool."))))
    (should (lispm-tool-spec-p zero-spec))
    (should (string-search "\"properties\":{}" zero-json))
    (should-not (string-search "\"properties\":false" zero-json))
    (should (lispm-tool-spec-p mcp-spec))
    (should (equal (lispm-tool-spec-name mcp-spec) "edit_file"))
    (should (lispm-tool-spec-p legacy-spec))
    (should (equal (lispm-tool-spec-name legacy-spec) "ping"))
    (should (equal (lispm-tool-spec-resolved-description legacy-spec) "Ping tool."))))

;;;; 5. Clean lispm-settings-mcp-* namespace

(ert-deftest lispm-unify-test-settings-mcp-namespace ()
  "MCP client config helpers live under `lispm-settings-mcp-*', not `lispm-mcp-server-*'."
  (dolist (sym '(lispm-settings-mcp-get
                 lispm-settings-mcp-status
                 lispm-settings-mcp-add
                 lispm-settings-mcp-change
                 lispm-settings-mcp-remove))
    (should (fboundp sym)))
  (dolist (sym '(lispm-mcp-server-get
                 lispm-mcp-server-status
                 lispm-mcp-server-add
                 lispm-mcp-server-change
                 lispm-mcp-server-remove))
    (should-not (fboundp sym))))

(provide 'lispm-unify-test)
;;; lispm-unify-test.el ends here
