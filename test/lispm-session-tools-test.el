;;; lispm-session-tools-test.el --- Recorded conversation tool tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'lispm-session-tools)

(defmacro lispm-session-tools-test-env (&rest body)
  "Run BODY with isolated native, Claude and Codex session stores."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-agents-claude-projects-directory (expand-file-name "claude/" tmp))
           (lispm-agents-codex-sessions-directory (expand-file-name "codex/sessions/" tmp))
           (lispm-agents-codex-index-file (expand-file-name "codex/index.jsonl" tmp))
           (lispm-agents-state-file (expand-file-name "sessions.eld" tmp))
           (lispm-agents--archived nil)
           (lispm-agents--session-cache (make-hash-table :test #'equal))
           (lispm--session-cache (make-hash-table :test #'equal))
           (lispm-agents-session-lookup-functions nil))
       ,@body)))

(defun lispm-session-tools-test--native (id project text)
  "Create native session ID for PROJECT containing TEXT."
  (let ((file (expand-file-name (format "projects/test/sessions/%s.org" id) lispm-directory)))
    (lispm-test--write file
                       (concat (format "#+lispm_id: %s\n#+lispm_agent: assistant@test\n#+lispm_role: assistant\n#+lispm_root: %s\n#+lispm_title: Native history\n" id project)
                               "* System\nSYSTEM SECRET\n* User\n" text
                               "\n* Assistant\n** Reply\nAnswer needle\n"
                               "** DONE read_file\n:PROPERTIES:\n:TOOL_ID: native-call\n:TOOL: read_file\n:END:\n"
                               "#+begin_src json\n{\"path\":\"a.el\"}\n#+end_src\n"
                               "*** Result\n#+begin_example\nNative tool output\n#+end_example\n"))
    file))

(defun lispm-session-tools-test--jsonl (file objects)
  "Create FILE with JSON OBJECTS, one per line."
  (lispm-test--write file (concat (mapconcat #'lispm--json-encode objects "\n") "\n"))
  file)

(defun lispm-session-tools-test--claude (id project)
  "Create a Claude transcript for ID in PROJECT."
  (lispm-session-tools-test--jsonl
   (expand-file-name (format "project/%s.jsonl" id) lispm-agents-claude-projects-directory)
   (list `(:type "user" :cwd ,project :message (:content "User needle"))
         '(:type "assistant" :message (:content [(:type "thinking" :thinking "PRIVATE REASONING")
                                                 (:type "text" :text "Claude answer")
                                                 (:type "tool_use" :name "read_file" :input (:path "a.el"))]))
         '(:type "user" :message (:content [(:type "tool_result" :content "Tool output needle")])))))

(defun lispm-session-tools-test--codex (id project &optional archived)
  "Create a Codex transcript for ID in PROJECT, optionally ARCHIVED."
  (lispm-session-tools-test--jsonl
   (expand-file-name (format "%s/rollout-%s.jsonl" (if archived "../archived_sessions" "2026/10/29") id)
                     lispm-agents-codex-sessions-directory)
   (list `(:type "session_meta" :payload (:id ,id :cwd ,project :timestamp "2026-10-29T01:00:00Z"))
         '(:type "event_msg" :payload (:type "user_message" :message "User needle"))
         '(:type "response_item" :payload (:type "message" :role "user" :content [(:type "input_text" :text "User needle")]))
         '(:type "response_item" :payload (:type "message" :role "assistant" :channel "analysis" :content [(:type "output_text" :text "HIDDEN ANALYSIS")]))
         '(:type "response_item" :payload (:type "message" :role "assistant" :content [(:type "output_text" :text "Codex answer")]))
         '(:type "event_msg" :payload (:type "agent_message" :message "Codex answer"))
         '(:type "response_item" :payload (:type "function_call" :name "exec_command" :arguments "{\"cmd\":\"pwd\"}"))
         '(:type "response_item" :payload (:type "function_call_output" :output "Tool output needle")))))

(defun lispm-session-tools-test--call (name input &optional raw)
  "Invoke session tool NAME with INPUT; decode JSON unless RAW."
  (let (result)
    (lispm-tool-invoke (lispm-get-tool name) input
                       (lambda (text errorp) (setq result (cons text errorp))))
    (should (lispm-test--wait (lambda () result)))
    (if raw result
      (should-not (cdr result))
      (json-parse-string (car result) :object-type 'alist :array-type 'list))))

(ert-deftest lispm-session-tools-test-provider-ids-and-archives ()
  (lispm-session-tools-test-env
    (lispm-session-tools-test--native "shared" project "User needle")
    (lispm-session-tools-test--claude "shared" project)
    (lispm-session-tools-test--codex "shared" project)
    (lispm-session-tools-test--codex "archived" project t)
    (setq lispm-agents--archived '("lispm:shared"))
    (let* ((result (lispm-session-tools-test--call "session_search" '((query . "NEEDLE"))))
           (sessions (alist-get 'sessions result)))
      (should (= (length sessions) 4))
      (should (equal (sort (mapcar (lambda (s) (concat (alist-get 'provider s) ":" (alist-get 'id s))) sessions)
                           #'string<)
                     '("claude:shared" "codex:archived" "codex:shared" "lispm:shared")))
      (should (eq (alist-get 'scan_truncated result) :false))
      (should (eq (alist-get 'archived (seq-find (lambda (s) (equal (alist-get 'provider s) "lispm")) sessions)) t))
      (should (eq (alist-get 'archived (seq-find (lambda (s) (equal (alist-get 'id s) "archived")) sessions)) t))
      (should (eq (alist-get 'archived (seq-find (lambda (s) (and (equal (alist-get 'provider s) "codex")
                                                                               (equal (alist-get 'id s) "shared"))) sessions)) :false)))
    (let* ((result (lispm-session-tools-test--call "session_read" '((provider . "codex") (id . "shared"))))
           (session (car (alist-get 'sessions result)))
           (messages (alist-get 'messages session)))
      (should (equal (alist-get 'provider session) "codex"))
      (should (= (length messages) 4))
      (should-not (string-search "HIDDEN ANALYSIS" (format "%S" result)))
      (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                     '("user" "assistant" "tool" "tool"))))))

(ert-deftest lispm-session-tools-test-filters-and-limit ()
  (lispm-session-tools-test-env
    (let ((native (lispm-session-tools-test--native "native" project "Old query"))
          (claude (lispm-session-tools-test--claude "claude" (expand-file-name "other" tmp))))
      (set-file-times native (encode-time 0 0 0 1 1 2025 t))
      (set-file-times claude (encode-time 0 0 0 29 10 2026 t))
      (let ((result (lispm-session-tools-test--call "session_search"
                                                  '((query . "needle") (role . "tool") (since . "2026-10-01")))))
        (should (= (length (alist-get 'sessions result)) 1))
        (should (equal (alist-get 'provider (car (alist-get 'sessions result))) "claude")))
      (let ((result (lispm-session-tools-test--call "session_search" `((project . ,project)))))
        (should (= (length (alist-get 'sessions result)) 1))
        (should (equal (alist-get 'provider (car (alist-get 'sessions result))) "lispm")))
      (let ((result (lispm-session-tools-test--call "session_search" '((limit . 1)))))
        (should (= (length (alist-get 'sessions result)) 1))
        (should (eq (alist-get 'results_truncated result) t)))
      (let ((lispm-session-tools-scan-limit 1))
        (should (eq (alist-get 'scan_truncated (lispm-session-tools-test--call "session_search" nil)) t)))
      (should (cdr (lispm-session-tools-test--call "session_search" '((limit . 0)) t)))
      (should (cdr (lispm-session-tools-test--call "session_search" '((since . "yesterday")) t))))))

(ert-deftest lispm-session-tools-test-codex-metadata-identity ()
  (lispm-session-tools-test-env
    (let* ((old (lispm-session-tools-test--codex "shared-id" project))
           (new (expand-file-name "2026/10/29/rollout-different-filename.jsonl"
                                   lispm-agents-codex-sessions-directory)))
      (lispm-session-tools-test--jsonl
       new (list `(:type "session_meta" :payload (:id "shared-id" :cwd ,project))
                 '(:type "event_msg" :payload (:type "user_message" :message "Newest fork content"))))
      (set-file-times old (encode-time 0 0 0 1 10 2026 t))
      (set-file-times new (encode-time 0 0 0 29 10 2026 t))
      (let* ((search (lispm-session-tools-test--call "session_search" '((provider . "codex"))))
             (session (car (alist-get 'sessions search)))
             (read (lispm-session-tools-test--call
                    "session_read" `((provider . ,(alist-get 'provider session))
                                     (id . ,(alist-get 'id session))))))
        (should (= (length (alist-get 'sessions search)) 1))
        (should (equal (alist-get 'id session) "shared-id"))
        (should (equal (alist-get 'snippet session) "Newest fork content"))
        (should (equal (alist-get 'text (car (alist-get 'messages (car (alist-get 'sessions read)))))
                       "Newest fork content")))
      (delete-file old)
      (let ((read (lispm-session-tools-test--call "session_read" '((provider . "codex") (id . "shared-id")))))
        (should (equal (alist-get 'id (car (alist-get 'sessions read))) "shared-id"))))))

(ert-deftest lispm-session-tools-test-native-live-and-hidden-blocks ()
  (lispm-session-tools-test-env
    (let* ((file (lispm-session-tools-test--native "native" project "User text"))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "* User\nUnsaved latest message：權限與 λ\n")
        (narrow-to-region (point-min) (+ (point-min) 20)))
      (let* ((result (lispm-session-tools-test--call "session_read" '((provider . "lispm") (id . "native"))))
             (session (car (alist-get 'sessions result)))
             (messages (alist-get 'messages session)))
        (should (equal (alist-get 'source session) "buffer"))
        (should (equal (alist-get 'text (car (last messages))) "Unsaved latest message：權限與 λ"))
        (should (seq-some (lambda (message)
                            (and (equal (alist-get 'role message) "tool")
                                 (equal (alist-get 'tool message) "read_file")
                                 (string-search "Native tool output" (alist-get 'text message))))
                          messages))
        (should-not (string-search "SYSTEM SECRET" (format "%S" result))))
      (lispm-session-tools-test--claude "claude" project)
      (should-not (string-search "PRIVATE REASONING"
                                 (format "%S" (lispm-session-tools-test--call
                                               "session_read" '((provider . "claude") (id . "claude")))))))))

(ert-deftest lispm-session-tools-test-bounded-malformed-and-missing ()
  (lispm-session-tools-test-env
    (let ((file (lispm-session-tools-test--claude "tail" project))
          (lispm-session-tools-tail-bytes 256))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (insert (json-serialize `(:type "user" :message (:content ,(make-string 1000 ?x)))) "\n")
        (insert "not-json\n")
        (insert "{\"type\":\"assistant\",\"message\":{\"content\":\"Latest tail\"}}\n")
        (insert "{\"partial\":")
        (write-region (point-min) (point-max) file nil 'silent))
      (let* ((result (lispm-session-tools-test--call "session_read" '((provider . "claude") (id . "tail"))))
             (session (car (alist-get 'sessions result))))
        (should (eq (alist-get 'tail_only session) t))
        (should (= (alist-get 'invalid_json_lines result) 2))
        (should (equal (alist-get 'text (car (last (alist-get 'messages session)))) "Latest tail")))
      (lispm-test--write (expand-file-name "projects/test/sessions/bad.org" lispm-directory) "No session metadata")
      (should (= (alist-get 'skipped_files (lispm-session-tools-test--call "session_search" nil)) 1))
      (should (cdr (lispm-session-tools-test--call "session_read" '((provider . "claude") (id . "missing")) t)))
      (should (cdr (lispm-session-tools-test--call "session_read" '((provider . "claude") (id . "../secret")) t)))
      (delete-file file)
      (should (cdr (lispm-session-tools-test--call "session_read" '((provider . "claude") (id . "tail")) t))))))

(ert-deftest lispm-session-tools-test-read-policy-and-corrupt-archive ()
  (lispm-session-tools-test-env
    (let ((agent (lispm-make-agent 'assistant project)))
      (dolist (name '("session_search" "session_read"))
        (should (eq (lispm-tool-permission (lispm-get-tool name)) 'read))
        (should (lispm-tool-deferred (lispm-get-tool name)))
        (setf (lispm-agent-policy agent) '((read . deny)))
        (should (eq (lispm-tool-action agent (lispm-get-tool name) nil) 'deny)))
      (lispm-session-tools-test--native "native" project "text")
      (lispm-test--write lispm-agents-state-file "not a state file")
      (setq lispm-agents--archived 'unloaded)
      (let ((result (lispm-session-tools-test--call "session_search" nil)))
        (should (eq (alist-get 'archived (car (alist-get 'sessions result))) :null)))
      (should (equal (lispm-test--file tmp "sessions.eld") "not a state file"))
      (should-not (file-exists-p (concat lispm-agents-state-file ".unreadable"))))))

(ert-deftest lispm-session-tools-test-output-budget-and-cancellation ()
  (lispm-session-tools-test-env
    (dolist (lispm-tool-output-limit '(0 1))
      (should (cdr (lispm-session-tools-test--call "session_search" nil t))))
    (let ((file (lispm-session-tools-test--native "large" project (make-string 3000 ?\n)))
          (lispm-tool-output-limit 1200))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "* User\n" (make-string 3000 ?\") "\n")
        (write-region (point-min) (point-max) file nil 'silent))
      (let* ((raw (lispm-session-tools-test--call "session_read" '((provider . "lispm") (id . "large")) t))
             (parsed (json-parse-string (car raw) :object-type 'alist)))
        (should-not (cdr raw))
        (should (<= (length (car raw)) lispm-tool-output-limit))
        (should (eq (alist-get 'output_truncated parsed) t))))
    (let* ((agent (lispm-make-agent 'assistant project))
           (lispm-current-agent agent)
           (lispm-current-call "cancelled")
           (call (list :name "session_search"))
           called)
      (puthash "cancelled" call (lispm-agent-pending agent))
      (lispm-tool-invoke (lispm-get-tool "session_search") nil
                         (lambda (&rest _) (setq called t)))
      (remhash "cancelled" (lispm-agent-pending agent))
      (accept-process-output nil 0.1)
      (should-not called))))

(ert-deftest lispm-session-tools-test-bounded-claude-sidecar ()
  (lispm-session-tools-test-env
    (let* ((file (expand-file-name "project/parent/subagents/agent-big.jsonl"
                                   lispm-agents-claude-projects-directory))
           (sidecar (concat (file-name-sans-extension file) ".meta.json"))
           (reader (symbol-function 'insert-file-contents))
           opened)
      (lispm-session-tools-test--jsonl
       file (list `(:type "user" :cwd ,project :message (:content "Subagent task"))))
      (lispm-test--write sidecar (lispm--json-encode `(:description ,(make-string 66000 ?x))))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (path &rest args)
                   (when (equal path sidecar) (setq opened t))
                   (apply reader path args))))
        (let ((result (lispm-session-tools-test--call "session_search" '((provider . "claude")))))
          (should-not opened)
          (should-not (alist-get 'sessions result))
          (should (= (alist-get 'errors_total result) 1))
          (should (string-search "sidecar exceeds 65536 bytes" (car (alist-get 'errors result))))))
      (lispm-test--write sidecar "{\"description\":\"Bounded task\"}")
      (let ((result (lispm-session-tools-test--call "session_search" '((provider . "claude")))))
        (should (= (length (alist-get 'sessions result)) 1))
        (should (= (alist-get 'errors_total result) 0))))))

(provide 'lispm-session-tools-test)
;;; lispm-session-tools-test.el ends here
