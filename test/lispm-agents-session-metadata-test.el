;;; lispm-agents-session-metadata-test.el --- Session chronology and parents -*- lexical-binding: t; -*-

(require 'ert)
(require 'lispm-agents-claude)
(require 'lispm-agents-codex)
(require 'lispm-runtime)

(ert-deftest lispm-test-claude-session-start-keeps-first-timestamp ()
  "Responses change the transcript time without changing its start."
  (let ((file (make-temp-file "lispm-claude-start-" nil ".jsonl"))
        (lispm-agents--session-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"type\":\"queue-operation\",\"timestamp\":\"2026-10-20T08:00:00Z\"}\n"
                    "{\"type\":\"user\",\"timestamp\":\"2026-10-20T08:00:01Z\",\"cwd\":\"/tmp\",\"message\":{\"content\":\"Fix the build\"}}\n"))
          (set-file-times file (seconds-to-time 1000))
          (let ((before (car (lispm-agents-file-sessions
                             (list file) #'lispm-agents-claude-session))))
            (should (= (plist-get before :started)
                       (float-time (parse-iso8601-time-string "2026-10-20T08:00:00Z"))))
            (write-region "{\"type\":\"assistant\",\"timestamp\":\"2026-10-29T10:00:00Z\"}\n"
                          nil file t 'silent)
            (set-file-times file (seconds-to-time 2000))
            (let ((after (car (lispm-agents-file-sessions
                              (list file) #'lispm-agents-claude-session))))
              (should (> (plist-get after :time) (plist-get before :time)))
              (should (= (plist-get before :started) (plist-get after :started))))))
      (delete-file file))))

(ert-deftest lispm-test-claude-session-without-start-does-not-use-tail ()
  "A recent response cannot stand in for missing creation metadata."
  (let ((session (lispm-agents-claude-session
                  "/tmp/lispm-missing-start.jsonl"
                  '(((:type "user" :cwd "/tmp" :message (:content "Work")))
                    (:type "assistant" :timestamp "2026-10-29T10:00:00Z")))))
    (should session)
    (should-not (plist-get session :started))))

(ert-deftest lispm-test-codex-session-nested-spawn-metadata ()
  "Older Codex source metadata supplies the real parent and task title."
  (let* ((payload '(:id "child" :cwd "/tmp" :timestamp "2026-10-20T08:00:00Z"
                   :source (:subagent (:thread_spawn (:parent_thread_id "parent"
                                                     :agent_path "/root/check_build"
                                                     :agent_nickname "Kuhn")))))
         (session (lispm-agents-codex-session
                   "/tmp/lispm-nested-spawn.jsonl"
                   (list (list (list :type "session_meta" :payload payload))))))
    (should (equal (plist-get session :parent) "parent"))
    (should (equal (plist-get session :title) "check_build (Kuhn)"))
    (should (numberp (plist-get session :started)))))

(ert-deftest lispm-test-codex-session-parent-shapes ()
  "Flat metadata wins, and non-subagent source shapes have no parent."
  (let ((nested '(:subagent (:thread_spawn (:parent_thread_id "nested")))))
    (should (equal (lispm-agents-codex--parent
                    (list :parent_thread_id "flat" :source nested)) "flat"))
    (should (equal (lispm-agents-codex--parent
                    (list :parent_thread_id " " :source nested)) "nested")))
  (dolist (source '(nil "cli" "vscode" (:subagent "review")
                       (:subagent (:thread_spawn "unknown"))
                       (:subagent (:thread_spawn (:parent_thread_id " ")))))
    (should-not (lispm-agents-codex--parent (list :source source)))))

(ert-deftest lispm-test-codex-session-excludes-exec-and-untitled-subagents ()
  "Non-interactive `codex exec' runs and untitled review subagents are excluded,
while subagents named in the session index are preserved."
  (let* ((exec-payload '(:id "exec-1" :cwd "/tmp" :originator "codex_exec" :source "exec"
                         :timestamp "2026-10-20T08:00:00Z"))
         (review-payload '(:id "rev-1" :cwd "/tmp" :parent_thread_id "exec-1"
                           :source (:subagent "review") :thread_source "subagent"
                           :timestamp "2026-10-20T08:00:00Z"))
         (prompt '(:type "event_msg" :payload (:type "user_message" :message "Review the code"))))
    (should-not (lispm-agents-codex-session
                 "/tmp/exec.jsonl"
                 (list (list (list :type "session_meta" :payload exec-payload) prompt))))
    (let ((rev (lispm-agents-codex-session
                "/tmp/review.jsonl"
                (list (list (list :type "session_meta" :payload review-payload) prompt))))
          (names (make-hash-table :test #'equal)))
      (should-not (plist-get rev :title))
      (should-not (lispm-agents-codex--named rev names))
      (puthash "rev-1" "Named review subagent" names)
      (should (equal (plist-get (lispm-agents-codex--named rev names) :title)
                     "Named review subagent"))))
  ;; A >4 KiB `session_meta` line (including multibyte UTF-8 `cwd` + `base_instructions`)
  ;; still classifies `codex_exec` and untitled subagents accurately via
  ;; `lispm-agents-codex--file-state', and legacy boolean cache entries are migrated.
  (let ((file (make-temp-file "lispm-codex-large-meta-" nil ".jsonl"))
        (lispm-agents-codex--meta-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file file
              (insert (decode-coding-string
                       (json-serialize
                        `(:type "session_meta"
                          :payload (:id "exec-big" :cwd "/tmp/中文目录"
                                    :base_instructions ,(make-string 5000 ?x)
                                    :originator "codex_exec" :source "exec")))
                       'utf-8)
                      "\n")))
          (should (eq (lispm-agents-codex--file-state file) 'drop))
          ;; Legacy `(mtime . t)' or `(mtime . nil)' cache entries from v0.18.15 are recomputed.
          (with-temp-file file
            (insert "{\"type\":\"session_meta\",\"payload\":{\"id\":\"cli-1\",\"cwd\":\"/tmp\",\"source\":\"cli\"}}\n"))
          (let ((mtime (lispm-agents-file-time file)))
            (puthash file (cons mtime t) lispm-agents-codex--meta-cache)
            (should (equal (mapcar (lambda (s) (plist-get s :id))
                                   (lispm-agents-codex--normalize-sessions
                                    (list (list :id "cli-1" :file file :time mtime))))
                           '("cli-1")))))
      (delete-file file))))

(ert-deftest lispm-test-claude-session-recovers-custom-title-without-head-prompt ()
  "A Claude session with a custom-title.json is kept even when head/tail has no prompt,
and clearing or changing custom-title.json takes effect without transcript cache staleness."
  (let* ((lispm-agents-claude-projects-directory (make-temp-file "lispm-claude-custom-" t))
         (dir (expand-file-name "-tmp" lispm-agents-claude-projects-directory))
         (file (expand-file-name "0199.jsonl" dir))
         (title-file (expand-file-name "0199/custom-title.json" dir))
         (sub-dir (expand-file-name "0199/subagents" dir))
         (sub-file (expand-file-name "agent-a1.jsonl" sub-dir))
         (lispm-agents--session-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (make-directory sub-dir t)
          (with-temp-file title-file
            (insert "{\"customTitle\":\"Custom Parent Title\"}"))
          (with-temp-file file
            (insert "{\"type\":\"queue-operation\",\"cwd\":\"/tmp\",\"timestamp\":\"2026-10-20T08:00:00Z\"}\n"))
          (with-temp-file sub-file
            (insert "{\"type\":\"user\",\"cwd\":\"/tmp\",\"message\":{\"content\":\"Subtask\"}}\n"))
          (let* ((sessions (lispm-agents-claude-sessions))
                 (parent (seq-find (lambda (s) (equal (plist-get s :id) "0199")) sessions))
                 (sub (list :agent 'claude :id "agent-a1" :parent "0199" :cwd "/tmp"
                            :title "Subtask" :file sub-file :time 10.0))
                 (normalized (lispm-agents-claude--normalize-sessions (list sub))))
            (should (= (length sessions) 2))
            (should (equal (plist-get parent :title) "Custom Parent Title"))
            (should (= (length normalized) 2))
            (should (seq-find (lambda (s) (equal (plist-get s :id) "0199")) normalized)))
          ;; Clearing custom-title.json without modifying 0199.jsonl drops the untitled parent.
          (with-temp-file title-file
            (insert "{\"customTitle\":\"   \"}"))
          (let ((sessions (lispm-agents-claude-sessions)))
            (should (= (length sessions) 1))
            (should-not (seq-find (lambda (s) (equal (plist-get s :id) "0199")) sessions))))
      (delete-directory lispm-agents-claude-projects-directory t))))

(ert-deftest lispm-test-native-session-start-survives-continuation ()
  "A LispM session's original id dates every continuation consistently."
  (let* ((directory (make-temp-file "lispm-native-start-" t))
         (first (expand-file-name "first.org" directory))
         (next (expand-file-name "continued.org" directory))
         (id "20261020-160000-abcd")
         (lispm--session-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (dolist (file (list first next))
            (with-temp-file file
              (insert (lispm-transcript-header
                       "Session" `(("agent" . "builder") ("id" . ,id)
                                   ("root" . "/tmp") ("title" . "Fix the build"))))))
          (set-file-times first (seconds-to-time 1000))
          (set-file-times next (seconds-to-time 2000))
          (let ((one (lispm-session-description first))
                (two (lispm-session-description next)))
            (should (= (plist-get one :started) (plist-get two :started)))
            (should (= (plist-get one :started) (float-time (encode-time 0 0 16 20 10 2026))))
            (should (< (plist-get one :time) (plist-get two :time))))
      (delete-directory directory t)))
  (should-not (lispm-session-started "custom-session"))
  (should-not (lispm-session-started "20261340-160000-abcd"))))

(provide 'lispm-agents-session-metadata-test)
;;; lispm-agents-session-metadata-test.el ends here
