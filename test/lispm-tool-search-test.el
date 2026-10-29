;;; lispm-tool-search-test.el --- Tool discovery tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Discovery includes available tools, without changing their identity,
;; permission checks, or the way a session restores deferred tools.

;;; Code:

(require 'lispm-test)

(defmacro lispm-tool-search-test--registry (&rest body)
  "Run BODY with only search_tools registered and no external sources."
  (declare (indent 0))
  `(let* ((search-tool (lispm-get-tool "search_tools"))
          (lispm-tools--registry (make-hash-table :test #'equal))
          (lispm-tool-sources nil)
          (lispm-tool-search-functions nil))
     (lispm-register-tool search-tool)
     ,@body))

(ert-deftest lispm-tool-search-test-rediscovery ()
  "Core and loaded tools can be found repeatedly; only deferred tools load."
  (lispm-test-env
    (lispm-tool-search-test--registry
      (lispm-deftool core_status () "Read the current status." "status")
      (lispm-deftool deferred_status () "Read a deferred status." :deferred t "status")
      (let ((agent (lispm-defagent tester :root project))
            (changed 0)
            (lispm-tools-changed-functions nil))
        (push (lambda (_) (cl-incf changed)) lispm-tools-changed-functions)
        (dotimes (_ 2)
          (let ((result (lispm-test--call agent "search_tools"
                                         '((query . "select:core_status,deferred_status")))))
            (should-not (cdr result))
            (should (string-prefix-p "Tools now available: core_status, deferred_status.\n" (car result)))
            (should (equal (lispm-agent-loaded-tools agent) '("deferred_status")))
            (should (= changed 1))))
        (lispm-tools-load agent '("core_status" "missing" "deferred_status"))
        (should (= changed 1))
        (should (equal (lispm-agent-loaded-tools agent) '("deferred_status")))
        ;; A transcript may name both, but restores only the deferred one.
        (with-temp-buffer
          (insert "Tools now available: core_status, deferred_status.\n")
          (let ((lispm-session-mode t)
                (lispm-session-agent agent))
            (setf (lispm-agent-loaded-tools agent) nil)
            (lispm-tools--restore)
            (should (equal (lispm-agent-loaded-tools agent) '("deferred_status")))))))))

(ert-deftest lispm-tool-search-test-role-filter ()
  "Loaded status never bypasses a role or a tool's agent predicate."
  (lispm-test-env
    (lispm-tool-search-test--registry
      (lispm-deftool visible_status () "Read status." "ok")
      (lispm-deftool hidden_status () "Read status." :deferred t "hidden")
      (lispm-deftool personal_status () "Read status." :for (lambda (_) nil) "personal")
      (let ((agent (lispm-defagent tester :root project :tools '(search_tools visible_status personal_status))))
        (setf (lispm-agent-loaded-tools agent) '("hidden_status" "personal_status"))
        (dolist (query '("status" "select:visible_status,hidden_status,personal_status"))
          (should (equal (mapcar #'lispm-tool-name (car (lispm-tools--discover agent query 5)))
                         '("visible_status"))))
        (let ((result (car (lispm-test--call agent "search_tools" '((query . "select:hidden_status"))))))
          (should (string-match-p "No accessible tool named hidden_status" result))
          (should-not (string-match-p "- hidden_status:" result)))
        (let ((result (car (lispm-test--call agent "search_tools" '((query . "zzzzzz"))))))
          (should (string-match-p "you have loaded: none" result))
          (should-not (string-match-p "hidden_status\\|personal_status" result)))))))

(ert-deftest lispm-tool-search-test-full-description-and-keywords ()
  "Multiline descriptions and declared terms are searchable without aliases."
  (lispm-tool-search-test--registry
    (lispm-deftool inspect_policy ()
      "Inspect this agent.\nRead its effective LispM permission and approval policy."
      :keywords '("full access" "native policy")
      "policy")
    (let ((tool (lispm-get-tool "inspect_policy")))
      (should (equal (lispm-tool-keywords tool) '("full access" "native policy")))
      (should (equal (lispm-tools--description tool) "Inspect this agent."))
      (dolist (query '("permission approval" "full access" "native policy"))
        (should (equal (lispm-tools--find query (list tool) 5) (list tool))))
      (should-not (alist-get 'keywords (lispm-tool-schema tool)))
      (should-not (lispm-get-tool "full access")))))

(ert-deftest lispm-tool-search-test-older-compiled-definition ()
  "A tool compiled before keyword metadata was added remains searchable."
  (let* ((tool (lispm-tool--create :name "legacy_status" :description "Read legacy status."))
         (older (apply #'record 'lispm-tool
                       (cl-loop for i from 1 below (1- (length tool)) collect (aref tool i)))))
    (should (lispm-tool-p older))
    (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "legacy status" (list older) 1))
                   '("legacy_status")))))

(ert-deftest lispm-tool-search-test-permission-ranking ()
  "Native policy searches do not mistake macOS permission checks for policy."
  (let ((tools (list
                (lispm-tool--create
                 :name "mcp__desktop__permissions"
                 :description "Check permissions.\nInspect macOS Accessibility and Screen Recording permissions.")
                (lispm-tool--create
                 :name "inspect_agent"
                 :description "Inspect a LispM agent.\nRead its permission ceiling, approval policy and pending calls."
                 :keywords '("LispM permissions" "approval policy" "full access")))))
    (dolist (query '("permission approval" "full access" "LispM permissions"))
      (should (equal (lispm-tool-name (car (lispm-tools--find query tools 1))) "inspect_agent")))
    (should (equal (lispm-tool-name (car (lispm-tools--find "macOS screen recording permissions" tools 1)))
                   "mcp__desktop__permissions"))))

(ert-deftest lispm-tool-search-test-selector-suggestions ()
  "Unknown selectors suggest canonical names without implicitly loading them."
  (lispm-test-env
    (lispm-tool-search-test--registry
      (lispm-deftool describe_symbol () "Describe an Emacs symbol and its definition." "definition")
      (lispm-deftool xref_definitions () "Find definitions of a symbol." :deferred t "definitions")
      (let* ((agent (lispm-defagent tester :root project))
             (result (car (lispm-test--call agent "search_tools"
                                           '((query . "select:describe_definition"))))))
        (should (string-match-p "No accessible tool named describe_definition" result))
        (should (string-match-p "Similar tools: .*describe_symbol" result))
        (should-not (string-match-p "^Tools now available:" result))
        (should-not (lispm-agent-loaded-tools agent))
        (should-not (lispm-get-tool "describe_definition"))
        (setq result (car (lispm-test--call agent "search_tools"
                                          '((query . "select:xref_definitions,describe_definition")))))
        (should (string-match-p "^Tools now available: xref_definitions\\.$" result))
        (should (equal (lispm-agent-loaded-tools agent) '("xref_definitions")))))))

(ert-deftest lispm-tool-search-test-skill-held-tools ()
  "A skill holds its unloaded tools; loaded tools can be selected directly."
  (lispm-test-env
    (lispm-tool-search-test--registry
      (lispm-deftool music_play () "Play music." :deferred t "playing")
      (let ((agent (lispm-defagent tester :root project))
            (lispm-tool-search-functions
             (list (lambda (_)
                     '((:fields ((3 . "music play")) :line "Load music with skill."
                                :tools ("music_play" "music_seek")))))))
        (let ((found (lispm-tools--discover agent "select:music_play" 5)))
          (should-not (car found))
          (should (cdr found))
          (should-not (lispm-tools--selection-help agent "select:music_play" (car found) (cdr found))))
        (lispm-tools-load agent '("music_play"))
        (let ((found (lispm-tools--discover agent "select:music_play,music_seek" 5)))
          (should (equal (mapcar #'lispm-tool-name (car found)) '("music_play")))
          (should (cdr found)))
        (let ((found (lispm-tools--discover agent "select:music_play" 5)))
          (should (equal (mapcar #'lispm-tool-name (car found)) '("music_play")))
          (should-not (cdr found)))))))

(ert-deftest lispm-tool-search-test-core-only-source-filter ()
  "Discovery remains available with only core tools and honors named sources."
  (lispm-test-env
    (lispm-tool-search-test--registry
      (lispm-deftool core_status () "Read status." "status")
      (let ((agent (lispm-defagent tester :root project)))
        (should (lispm-tools--searchable-p))
        (should (member "search_tools" (mapcar #'lispm-tool-name (lispm-agent-tools* agent))))
        (should-not (car (lispm-tools--discover agent "tools" 5)))
        (should (equal (mapcar #'lispm-tool-name (car (lispm-tools--discover agent "select:search_tools" 5)))
                       '("search_tools")))
        (lispm-deftool mcp__test__status () "Read status." :deferred t "status")
        (setf (lispm-agent-loaded-tools agent) '("mcp__test__status"))
        (let ((lispm-tool-sources
               (list (lambda ()
                       (list (list :name "test" :provides
                                   (lambda (name) (string-prefix-p "mcp__test__" name))))))))
          (should (equal (mapcar #'lispm-tool-name
                                (car (lispm-tools--discover agent "test status" 5)))
                         '("mcp__test__status"))))))))

(provide 'lispm-tool-search-test)
;;; lispm-tool-search-test.el ends here
