;;; lispm-basis-test.el --- Formal basis and architecture law tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Witnesses for the Phase 1 minimal orthogonal basis refactoring (#189):
;;   1. `lispm-principal' subtyping, external client parity, and system guard.
;;   2. Canonical `lispm-agent-scope', mutual exclusivity, and core system agent.
;;   3. `lispm-job' cancellation lifecycle and non-destructive `lispm-chunk-collector'.
;;   4. Extensible `lispm-character' capability architecture without core edits.

;;; Code:

(require 'cl-lib)
(require 'lispm-test)
(require 'lispm-mcp-server)

;;;; 1. Principal subtyping, parity, and ghost elimination

(ert-deftest lispm-basis-test-principal-subtyping-and-external-clients ()
  "External bridge and MCP clients are bare principals sharing policy semantics."
  (lispm-test-with-peers
    (let* ((agent (lispm-defagent native-one :root project :role 'assistant))
           (ext (lispm-make-principal 'ext-one project :id "ext-1" :role 'assistant))
           (fake-lispm (lispm-make-principal lispm-system-agent-name project
                                             :id "fake-sys" :role 'assistant))
           (terminal (lispm-test--peer 'codex project))
           (bridge (lispm-agents-bridge-agent terminal))
           (session (lispm-mcp-server--session-make
                     :id "mcp-1" :name "claude-code" :kind "Claude Code"
                     :directory project :used 0))
           (mcp-principal (lispm-agents-bridge-server-agent session)))
      ;; Subtyping hierarchy.
      (should (lispm-principal-p agent))
      (should (lispm-agent-p agent))
      (should (lispm-principal-p ext))
      (should-not (lispm-agent-p ext))
      ;; Bridge and MCP construct bare principals, never ghost native agents.
      (should (lispm-principal-p bridge))
      (should-not (lispm-agent-p bridge))
      (should (lispm-principal-p mcp-principal))
      (should-not (lispm-agent-p mcp-principal))
      ;; An external principal named `lispm' can never satisfy `lispm-system-agent-p'.
      (should-not (lispm-system-agent-p fake-lispm))
      ;; Busy predicates and journal snapshots safely handle non-agent principals.
      (should-not (lispm-agent-busy-p ext))
      (should-not (lispm-agent-working-p ext))
      (with-temp-buffer
        (insert "scratch")
        (let ((lispm-current-agent ext))
          (should-not (lispm-journal-snapshot (current-buffer)))))
      ;; Identical role, policy, root and allowance yield identical tool decisions.
      (setf (lispm-principal-policy agent) '((write . deny) (shell . ask))
            (lispm-principal-policy ext) '((write . deny) (shell . ask))
            (lispm-principal-allowed agent) '("edit_file")
            (lispm-principal-allowed ext) '("edit_file"))
      (should (equal (lispm-policy agent) (lispm-policy ext)))
      (should (equal (mapcar #'lispm-tool-name (lispm-agent-tools* agent))
                     (mapcar #'lispm-tool-name (lispm-agent-tools* ext))))
      (dolist (perm '(read write shell exec))
        (should (eq (lispm-policy-action agent perm) (lispm-policy-action ext perm))))
      (should (lispm-tools--in-root-p ext (expand-file-name "sub/file.el" project)))
      (should-not (lispm-tools--in-root-p ext "/tmp/outside.el"))
      (let ((read-tool (lispm-get-tool "read_file"))
            (write-tool (lispm-get-tool "write_file"))
            (shell-tool (lispm-get-tool "run_shell")))
        (should (eq (lispm-tool-action agent read-tool '((path . "a.el")))
                    (lispm-tool-action ext read-tool '((path . "a.el")))))
        (should (eq (lispm-tool-action agent write-tool '((path . "a.el") (content . "x")))
                    (lispm-tool-action ext write-tool '((path . "a.el") (content . "x")))))
        (should (eq (lispm-tool-action agent shell-tool '((command . "pwd")))
                    (lispm-tool-action ext shell-tool '((command . "pwd")))))))))

;;;; 2. Canonical scope and core system agent ownership

(ert-deftest lispm-basis-test-canonical-scope-and-core-system-agent ()
  "Canonical agent scopes are total and system/character scopes are exclusive."
  (lispm-test-env
    (let* ((sys (lispm-system-agent))
           (char (lispm-create-character "Ada" :description "Reviewer"))
           (char-agent (lispm-character-agent char))
           (standing (lispm-defagent keeper :root project))
           (child (lispm-start 'assistant project))
           (session (lispm-start 'assistant project)))
      (lispm-set-parent child (lispm-principal-id standing))
      ;; Canonical scope classification.
      (should (eq (lispm-agent-scope sys) 'system))
      (should (eq (lispm-agent-scope char-agent) 'character))
      (should (eq (lispm-agent-scope standing) 'standing))
      (should (eq (lispm-agent-scope child) 'child))
      (should (eq (lispm-agent-scope session) 'session))
      ;; System + character simultaneous scoping is rejected.
      (should-error (lispm-make-agent 'assistant project :system t :character 'ada))
      (should-error (lispm-register-agent 'bad-scope :root project :system t :character 'ada))
      ;; Core ownership and alias preservation.
      (should (lispm-system-agent-p sys))
      (should (string-match-p "lispm-core\\.elc?\\'"
                              (symbol-file 'lispm-system-agent 'defun)))
      (should (string-match-p "lispm-core\\.elc?\\'"
                              (symbol-file 'lispm-system-agent-p 'defun)))
      (should (equal lispm-bar--instructions lispm-system-agent-instructions)))))

;;;; 3. Unified job handle and chunk collector laws

(ert-deftest lispm-basis-test-job-and-chunk-collector-laws ()
  "Unified job cancellation is idempotent and owner-isolated; chunk collector is non-destructive."
  (let ((c (lispm-chunk-collector-create 10)))
    (lispm-chunk-collector-push c "hello ")
    (should-not (lispm-chunk-collector-truncated-p c))
    (lispm-chunk-collector-push c "world!")
    (should (lispm-chunk-collector-truncated-p c))
    (let ((before (copy-sequence (lispm-chunk-collector-chunks c))))
      (should (equal (lispm-chunk-collector-string c) "hello worl"))
      (should (equal (lispm-chunk-collector-string c) "hello worl"))
      (should (equal (lispm-chunk-collector-chunks c) before))
      (should (<= (length (lispm-chunk-collector-string c)) 10))))
  (let ((zero (lispm-chunk-collector-create -5)))
    (lispm-chunk-collector-push zero "abc")
    (should (lispm-chunk-collector-truncated-p zero))
    (should (equal (lispm-chunk-collector-string zero) "")))
  (lispm-test-env
    (let* ((a1 (lispm-defagent job-a1 :root project))
           (a2 (lispm-defagent job-a2 :root project))
           (proxy (lispm-make-principal 'ext project :id "p1" :role 'assistant))
           (terminal (generate-new-buffer " *basis-terminal*"))
           (cancels-a1 0) (cancels-a2 0) (cancels-proxy 0) (cancels-finished 0))
      (unwind-protect
          (progn
            (with-current-buffer terminal
              (setq-local lispm-agents-bridge--agent proxy))
            (let ((j1 (lispm-job-register a1 'test (lambda () (cl-incf cancels-a1))))
                  (j2 (lispm-job-register a1 'test (lambda () (cl-incf cancels-finished))))
                  (j3 (lispm-job-register a2 'test (lambda () (cl-incf cancels-a2))))
                  (jp (lispm-job-register proxy 'test (lambda () (cl-incf cancels-proxy)))))
              (should (memq #'lispm-job--state-changed lispm-state-change-functions))
              (should (memq #'lispm-job--session-ended lispm-session-end-functions))
              (should (memq #'lispm-job--terminal-ended lispm-agents-context-release-functions))
              (should (= (lispm-job-active-count) 4))
              ;; Finishing suppresses subsequent cancel.
              (should (eq (lispm-job-finish j2) t))
              (should-not (lispm-job-finish j2))
              (should-not (lispm-job-cancel j2))
              (should (= cancels-finished 0))
              ;; Cancel idempotency.
              (should (lispm-job-cancel j1))
              (should-not (lispm-job-cancel j1))
              (should (= cancels-a1 1))
              ;; Terminal release cancels only the proxy owner's job.
              (run-hook-with-args 'lispm-agents-context-release-functions terminal)
              (should (= cancels-proxy 1))
              (should (= cancels-a2 0))
              (should (= (lispm-job-active-count a2) 1))
              ;; Aborting native agent cancels remaining job and drains hooks.
              (lispm-set-state a2 'aborting)
              (should (= cancels-a2 1))
              (should (= (lispm-job-active-count) 0))
              (should-not (memq #'lispm-job--state-changed lispm-state-change-functions))
              (should-not (memq #'lispm-job--session-ended lispm-session-end-functions))
              (should-not (memq #'lispm-job--terminal-ended lispm-agents-context-release-functions))))
        (when (buffer-live-p terminal) (kill-buffer terminal))))))

;;;; 4. Character extension architecture laws

(ert-deftest lispm-basis-test-character-extensions-laws ()
  "Characters store optional capabilities in extensions without hardcoded core slots."
  (let ((slots (mapcar #'car (cdr (cl-struct-slot-info 'lispm-character)))))
    (should (memq 'extensions slots))
    (dolist (removed '(rag comfyui appearance workflow knowledge embedding-model))
      (should-not (memq removed slots))))
  (lispm-test-env
    (let ((lispm-character-extensions (copy-tree lispm-character-extensions))
          (ada (lispm-create-character "Ada" :description "Helper" :rag t)))
      (should (lispm-character-rag ada))
      (setf (lispm-character-extension ada :badge) "core")
      (should (equal (lispm-character-extension ada :badge) "core"))
      ;; Custom extension integrates into file I/O and form without modifying core or characters.el.
      (lispm-character-register-extension
       'custom-badge
       :keys '(:badge)
       :read (lambda (line _bool _sym) (list :badge (funcall line "badge")))
       :format (lambda (props line-fn) `(("badge" . ,(funcall line-fn (plist-get props :badge)))))
       :form-values (lambda (character) `((badge . ,(and character (lispm-character-extension character :badge)))))
       :form-props (lambda (get) (list :badge (funcall get 'badge)))
       :form-fields (lambda () '((badge :type text :label "Badge"))))
      (let* ((zoe (lispm-create-character "Zoe" :badge "architect"))
             (file (lispm-character-file zoe)))
        (should (string-search "#+badge: architect\n" (lispm-file-text file)))
        (let ((lispm--characters nil))
          (lispm-load-characters)
          (should (equal (lispm-character-extension (lispm-get-character 'zoe) :badge)
                         "architect")))
        (with-current-buffer (lispm-character-form "Zoe")
          (should (equal (alist-get 'badge (lispm-ui-form-values)) "architect"))
          (lispm-test--form-type 'badge "lead-")
          (lispm-ui-form-submit))
        (should (equal (lispm-character-extension (lispm-get-character 'zoe) :badge)
                       "lead-architect"))))))

(provide 'lispm-basis-test)
;;; lispm-basis-test.el ends here
