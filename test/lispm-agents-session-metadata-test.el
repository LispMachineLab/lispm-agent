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
                             (list file) #'lispm-agents-claude--session))))
            (should (= (plist-get before :started)
                       (float-time (parse-iso8601-time-string "2026-10-20T08:00:00Z"))))
            (write-region "{\"type\":\"assistant\",\"timestamp\":\"2026-10-29T10:00:00Z\"}\n"
                          nil file t 'silent)
            (set-file-times file (seconds-to-time 2000))
            (let ((after (car (lispm-agents-file-sessions
                              (list file) #'lispm-agents-claude--session))))
              (should (> (plist-get after :time) (plist-get before :time)))
              (should (= (plist-get before :started) (plist-get after :started))))))
      (delete-file file))))

(ert-deftest lispm-test-claude-session-without-start-does-not-use-tail ()
  "A recent response cannot stand in for missing creation metadata."
  (let ((session (lispm-agents-claude--session
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
         (session (lispm-agents-codex--session
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
          (let ((one (lispm--session-description first))
                (two (lispm--session-description next)))
            (should (= (plist-get one :started) (plist-get two :started)))
            (should (= (plist-get one :started) (float-time (encode-time 0 0 16 20 10 2026))))
            (should (< (plist-get one :time) (plist-get two :time))))
      (delete-directory directory t)))
  (should-not (lispm--session-started "custom-session"))
  (should-not (lispm--session-started "20261340-160000-abcd"))))

(provide 'lispm-agents-session-metadata-test)
;;; lispm-agents-session-metadata-test.el ends here
