;;; lispm-roster-bridge-test.el --- Terminal roster tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after the fixtures in lispm-test.el.  Terminals and transcripts
;; here belong to the scratch environment, never to an installed agent.

;;; Code:

(ert-deftest lispm-test-roster-bridge-mailbox ()
  "The private FIFO keeps message identity, sender, time and request metadata.
Only the delivery text reaches the terminal; mailbox snapshots own their data."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (lispm-agents-bridge-settle-time 3600)
           (changes nil))
      (push terminal lispm-test--busy)
      (cl-letf (((symbol-function 'lispm-agents-roster-notify)
                 (lambda (subject fields &optional event)
                   (push (list subject fields event) changes))))
        (should-not (lispm-agents-bridge-deliver
                     terminal "[Request R7 from boss]\nOne."
                     '(:id "m1" :from "boss" :to "codex" :reply-to "R7" :queued 12.5 :text "One.")))
        (should-not (lispm-agents-bridge-deliver terminal "Two."))
        (let* ((mailbox (lispm-agents-bridge-mailbox terminal))
               (entries (plist-get mailbox :queued))
               (first (car entries)))
          (should (= (plist-get (lispm-agents-bridge-mailbox-summary terminal) :queued) 2))
          (should-not (plist-get mailbox :handling))
          (should (equal first '(:id "m1" :from "boss" :to "codex" :reply-to "R7"
                                :request-id "R7" :queued 12.5 :text "One.")))
          (should (stringp (plist-get (cadr entries) :id)))
          (should (numberp (plist-get (cadr entries) :queued)))
          (should-not (plist-get (cadr entries) :from))
          (setf (plist-get first :from) "changed")
          (should (equal (plist-get (car (plist-get (lispm-agents-bridge-mailbox terminal) :queued)) :from)
                         "boss")))
        (setq lispm-test--busy nil)
        (lispm-agents-bridge-flush)
        (should (equal (car (lispm-test--sent terminal 1)) "[Request R7 from boss]\nOne.⏎"))
        (let ((mailbox (lispm-agents-bridge-mailbox terminal)))
          (should (equal (plist-get (plist-get mailbox :handling) :id) "m1"))
          (should (= (length (plist-get mailbox :queued)) 1))
          (should-not (plist-member (plist-get mailbox :handling) :delivered)))
        (should (equal (mapcar (lambda (change) (plist-get (nth 2 change) :type))
                              (reverse changes))
                       '(message-queued message-queued message-delivered)))
        (should (equal (plist-get (nth 2 (car changes)) :request-id) "R7"))))))

(ert-deftest lispm-test-roster-bridge-sender-and-settlement ()
  "Peer delivery preserves the collaboration envelope and settles handling
only once the session reader records a completed turn after delivery."
  (lispm-test-with-peers
    (let* ((boss (lispm-defagent boss :root project))
           (terminal (lispm-test--peer 'codex project))
           (ended nil)
           (lispm-agents-session-lookup-functions
            (list (lambda (_kind _id)
                    (list :time (+ (float-time) 10) :turn-ended ended)))))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "s"))
      (push terminal lispm-test--busy)
      (lispm-collab-send boss "codex" "Hello.")
      (let ((entry (car (plist-get (lispm-agents-bridge-mailbox terminal) :queued))))
        (should (equal (plist-get entry :from) "boss"))
        (should (equal (plist-get entry :to) "codex"))
        (should (equal (plist-get entry :text) "Hello."))
        (should (numberp (plist-get entry :queued))))
      (setq lispm-test--busy nil)
      (lispm-agents-bridge-flush)
      (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
      (lispm-agents-bridge--sessions-arrived)
      (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
      (setq ended t)
      (lispm-agents-bridge--sessions-arrived)
      (should-not (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling)))))

(ert-deftest lispm-test-roster-bridge-background-settlement-needs-turn-evidence ()
  "A newer metadata write must not settle a newly delivered message.
Only fresh, explicit work or a post-delivery turn time proves its turn;
background settlement reads cached metadata even without observers."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (lispm-agents-roster-change-functions nil)
           session
           (lispm-agents-session-lookup-functions (list (lambda (_kind _id) session))))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "s"))
      (lispm-agents-bridge-deliver terminal "New message")
      (let ((delivered (plist-get (buffer-local-value 'lispm-agents-bridge--handling terminal) :delivered)))
        (cl-letf (((symbol-function 'lispm-agents-read-json-tail)
                   (lambda (&rest _) (ert-fail "Background settlement read a transcript")))
                  ((symbol-function 'lispm-agents-bridge-observe)
                   (lambda (&rest _) (ert-fail "Background settlement requested lazy work"))))
          ;; A token-count or metadata append after delivery leaves the old
          ;; completed turn as the reader's latest turn.
          (setq session (list :time (1+ delivered) :turn-time (1- delivered) :turn-ended t))
          (lispm-agents-bridge--sessions-arrived)
          (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
          ;; An unknown turn state and a stale working row prove no work.
          (setq session (list :time (1+ delivered)))
          (lispm-agents-bridge--sessions-arrived)
          (setq session (list :time (1- delivered) :turn-ended nil))
          (lispm-agents-bridge--sessions-arrived)
          (should (buffer-local-value 'lispm-agents-bridge--given terminal))
          (setq session (list :time (1+ delivered) :turn-ended t))
          (lispm-agents-bridge--sessions-arrived)
          (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
          ;; Explicit fresh work followed by completion can settle even when
          ;; the reader does not know the turn's own timestamp.
          (setq session (list :time (1+ delivered) :turn-ended nil))
          (lispm-agents-bridge--sessions-arrived)
          (should-not (buffer-local-value 'lispm-agents-bridge--given terminal))
          (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
          (setq session (list :time (+ delivered 2) :turn-ended t))
          (lispm-agents-bridge--sessions-arrived)
          (should-not (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling)))))))

(ert-deftest lispm-test-roster-bridge-observes-completed-work ()
  "A completion settles handling without a reader, but an older turn cannot.
Observation follows real output or lazy details; mailbox redraw reads memory."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (work nil)
           (lispm-agents-bridge-settle-time 3600))
      (setf (plist-get (alist-get 'codex lispm-agents-defined) :work-details)
            (lambda (_) work))
      (lispm-agents-bridge-deliver terminal "Hello.")
      (let ((delivered (plist-get (buffer-local-value 'lispm-agents-bridge--handling terminal) :delivered)))
        (setq work (list :time (1- delivered) :turn-ended t))
        (lispm-agents-bridge-observe terminal)
        (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
        ;; A metadata write after delivery does not make an old turn new.
        (setq work (list :time (1+ delivered) :turn-time (1- delivered) :turn-ended t))
        (lispm-agents-bridge-observe terminal)
        (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
        (setq work (list :time (1+ delivered) :turn-ended t))
        (lispm-agents-bridge-observe terminal)
        (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
        (setq work (list :time (1+ delivered) :turn-time (1+ delivered) :turn-ended nil))
        (lispm-agents-bridge-observe terminal)
        (should (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling))
        (setf (plist-get work :turn-ended) t)
        (lispm-agents-bridge-observe terminal)
        (should-not (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling)))
      ;; The private FIFO's existing readiness checks can observe a whole
      ;; turn too, without starting a new polling timer for the roster.
      (lispm-agents-bridge-deliver terminal "Again.")
      (should-not (lispm-agents-bridge--free-p terminal))
      (push terminal lispm-test--busy)
      (should-not (lispm-agents-bridge--free-p terminal))
      (setq lispm-test--busy nil)
      (should (lispm-agents-bridge--free-p terminal))
      (should-not (plist-get (lispm-agents-bridge-mailbox-summary terminal) :handling)))))

(ert-deftest lispm-test-roster-terminal-work ()
  "Claude and Codex work is bounded to the latest observed turn; results
settle calls and no files or permission decisions are inferred."
  (let* ((claude
          (lispm-agents-claude--work
           '((:type "user" :uuid "old" :message (:content "Old turn"))
             (:type "assistant" :message (:content ((:type "tool_use" :id "old" :name "old"))))
             (:type "user" :uuid "new" :message (:content "New turn"))
             (:type "assistant" :message
                    (:content ((:type "tool_use" :id "c1" :name "Read" :input (:path "a.el"))
                               (:type "tool_use" :id "c2" :name "Write" :input (:path "b.el")))))
             (:type "user" :message
                    (:content ((:type "tool_result" :tool_use_id "c1" :is_error t)))))))
         (codex
          (lispm-agents-codex--work
           '((:type "event_msg" :payload (:type "task_started" :turn_id "old"))
             (:type "response_item" :payload (:type "function_call" :call_id "old" :name "old"))
             (:type "turn_context" :payload (:turn_id "new"))
             (:type "response_item" :payload
                    (:type "function_call" :call_id "x1" :name "exec_command" :arguments "{\"cmd\":\"pwd\"}"))
             (:type "response_item" :payload
                    (:type "function_call_output" :call_id "x1" :output "ok"))
             (:type "response_item" :payload
                    (:type "custom_tool_call" :call_id "x2" :name "apply_patch" :input "patch"))))))
    (dolist (work (list claude codex))
      (should (equal (plist-get work :turn) "new"))
      (should (= (length (plist-get work :pending-calls)) 1))
      (should (= (length (plist-get work :recent-calls)) 1))
      (should (eq (plist-get (car (plist-get work :pending-calls)) :status) 'running))
      (should (stringp (plist-get (car (plist-get work :pending-calls)) :input-summary)))
      (should-not (plist-get work :files)))
    (should (eq (plist-get (car (plist-get claude :recent-calls)) :status) 'error))
    (should (eq (plist-get (car (plist-get codex :recent-calls)) :status) 'done))
    (let ((claude-unknown (lispm-agents-claude--work
                           '((:type "assistant" :message
                              (:content ((:type "tool_use" :id "c" :name "Read")))))))
          (codex-unknown (lispm-agents-codex--work
                          '((:type "response_item" :payload
                             (:type "function_call" :call_id "x" :name "read"))))))
      (should-not (plist-get claude-unknown :pending-calls))
      (should-not (plist-get codex-unknown :pending-calls)))))

(ert-deftest lispm-test-roster-terminal-work-cache ()
  "Details cache a bounded tail by size and mtime; recent calls are bounded.
A same-time append invalidates the cache by size, without a directory scan."
  (lispm-test-env
    (let ((file (expand-file-name "work.jsonl" project))
          (terminal (generate-new-buffer " *lispm-roster-work*"))
          (reads 0)
          (read-tail (symbol-function 'lispm-agents-read-json-tail)))
      (unwind-protect
          (progn
            (lispm-agents-append-json-line file '(:type "turn_context" :payload (:turn_id "t")))
            (dotimes (n 25)
              (let ((id (format "c%d" n)))
                (lispm-agents-append-json-line
                 file (list :type "response_item" :payload
                            (list :type "function_call" :call_id id :name "read" :arguments "{}")))
                (lispm-agents-append-json-line
                 file (list :type "response_item" :payload
                            (list :type "function_call_output" :call_id id :output "ok")))))
            (cl-letf (((symbol-function 'lispm-agents-codex--terminal-rollout) (lambda (_) file))
                      ((symbol-function 'lispm-agents-read-json-tail)
                       (lambda (path) (cl-incf reads) (funcall read-tail path))))
              (let* ((time (file-attribute-modification-time (file-attributes file)))
                     (work (lispm-agents-codex--work-details terminal)))
                (should (= (length (plist-get work :recent-calls)) 20))
                (should (equal (plist-get (lispm-agents-codex--work-details terminal) :turn) "t"))
                (should (= reads 1))
                (lispm-agents-append-json-line
                 file '(:type "response_item" :payload
                        (:type "function_call" :call_id "pending" :name "write" :arguments "{}")))
                (set-file-times file time)
                (should (= (length (plist-get (lispm-agents-codex--work-details terminal) :pending-calls)) 1))
                (should (= reads 2)))))
        (kill-buffer terminal)))))

(provide 'lispm-roster-bridge-test)
;;; lispm-roster-bridge-test.el ends here
