;;; lispm-collab-async-test.el --- Nonblocking delegation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after lispm-test.el's fixtures.  Requests use the scripted model;
;; completing a hanging request through `lispm--on-message' lets the tests
;; choose the order in which independent agents finish.

;;; Code:

(defun lispm-test-async--request-id (agent)
  "The durable request id of AGENT's current input."
  (nth 1 (split-string (plist-get (lispm-agent-entry agent) :reply-to))))

(defun lispm-test-async--reply (agent text)
  "Finish AGENT's pending fake-model request with TEXT."
  (should (eq (lispm-agent-state agent) 'requesting))
  (lispm--on-message agent (lispm-test--msg "end_turn" (lispm-test--text text))))

(defun lispm-test-async--crash-all ()
  "Crash agents without delivering the replies that orderly shutdown would.
The regular crash fixture restores files after killing buffers; an
async reply during that cleanup could also reopen its caller's buffer."
  (dolist (agent (lispm-agents))
    (setf (lispm-agent-turn-callbacks agent) nil)
    (dolist (entry (append (lispm-agent-inbox agent) (lispm-agent-suspended agent)))
      (plist-put entry :callback nil)))
  (lispm-test--crash-all))

(ert-deftest lispm-test-async-ask-approval-does-not-block-caller ()
  "A helper's approval does not hold user input or progress in its caller."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes." :policy '((write . auto)))
    (let ((boss (lispm-defagent boss :root project)))
      (setf (lispm-agent-policy boss) '((write . ask)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                     (lispm-test--tool "ask" "ask_agent"
                                                       '((agent . "reviewer")
                                                         (request . "Write the review."))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Review requested."))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "I can still answer you."))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Progress received."))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Review received.")))
              (reviewer@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "write" "write_file"
                                                   '((path . "review.txt") (content . "Good."))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Review completed.")))))
      (lispm-tell boss "Ask a reviewer")
      (should (lispm-test--wait
               (lambda ()
                 (and (funcall (lispm-test--idle-p boss))
                      (when-let* ((helper (lispm-get-agent "reviewer@project" t)))
                        (equal (lispm-waiting-calls helper) '("write")))))))
      (let ((helper (lispm-get-agent "reviewer@project")))
        (should (zerop (hash-table-count (lispm-agent-pending boss))))
        (should-not (lispm-agent-waiting-on boss))
        (should (eq (lispm-policy-action helper 'write) 'ask))
        (should-not (file-exists-p (expand-file-name "review.txt" project)))
        ;; This was the reported failure: user messages stayed queued for
        ;; the entire lifetime of ask_agent, including hidden approvals.
        (lispm-tell boss "Check progress")
        (should (lispm-test--wait (lispm-test--idle-p boss)))
        (should (equal (lispm-agent-last-reply boss) "I can still answer you."))
        (should (equal (lispm-waiting-calls helper) '("write")))
        (should-not (cdr (lispm-test--call
                          helper "send_message"
                          '((to . "boss") (text . "Progress: review ready, waiting for approval.")))))
        (should (lispm-test--wait (lispm-test--idle-p boss)))
        (should (equal (lispm-agent-last-reply boss) "Progress received."))
        (should (equal (lispm-waiting-calls helper) '("write")))
        ;; Approval remains local to the helper.  Its final reply becomes
        ;; a new caller turn, after the caller's intervening conversations.
        (lispm-approve-for helper "write")
        (should (lispm-test--wait
                 (lambda ()
                   (and (funcall (lispm-test--idle-p boss))
                        (equal (lispm-agent-last-reply boss) "Review received.")
                        (not (memq helper (lispm-agents)))))))
        (should (equal (lispm-test--file project "review.txt") "Good.\n"))
        (should (string-search "Review completed." (lispm-test--transcript boss)))))))

(ert-deftest lispm-test-async-replies-keep-identity-and-arrival-order ()
  "Independent replies queue while the caller works and retain their ids."
  (lispm-test-env
    (let ((boss (lispm-defagent boss :root project))
          (first (lispm-defagent first :root project))
          (second (lispm-defagent second :root project)))
      (lispm-session-buffer boss)
      (setq lispm-test--scripts
            `((boss hang
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Second received."))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "First received.")))
              (first hang) (second hang)))
      (let ((one (lispm-test--call boss "ask_agent" '((agent . "first") (request . "First task"))))
            (two (lispm-test--call boss "ask_agent" '((agent . "second") (request . "Second task")))))
        (should (stringp (car one)))
        (should (stringp (car two)))
        (should-not (cdr one))
        (should-not (cdr two))
        (should (lispm-test--wait
                 (lambda () (and (eq (lispm-agent-state first) 'requesting)
                                 (eq (lispm-agent-state second) 'requesting)))))
        (let ((first-id (lispm-test-async--request-id first))
              (second-id (lispm-test-async--request-id second)))
          (should first-id)
          (should second-id)
          (should-not (equal first-id second-id))
          (should (string-search first-id (car one)))
          (should (string-search second-id (car two)))
          (should-not (lispm-agent-waiting-on boss))
          (lispm-tell boss "Do my own work")
          (should (lispm-test--wait (lambda () (eq (lispm-agent-state boss) 'requesting))))
          (lispm-test-async--reply second "Second task result.")
          (lispm-test-async--reply first "First task result.")
          (should (eq (lispm-agent-state boss) 'requesting))
          (should (equal (mapcar (lambda (entry) (plist-get entry :request-id))
                                 (lispm-agent-inbox boss))
                         (list second-id first-id)))
          (should (string-search "Second task result." (plist-get (car (lispm-agent-inbox boss)) :text)))
          (should (string-search "First task result." (plist-get (cadr (lispm-agent-inbox boss)) :text)))
          (lispm-test-async--reply boss "My own work is done.")
          (should (lispm-test--wait (lispm-test--idle-p boss)))
          (should (equal (lispm-agent-last-reply boss) "First received."))
          (let ((text (lispm-test--transcript boss)))
            (should (< (string-search "Second task result." text)
                       (string-search "First task result." text)))))))))

(ert-deftest lispm-test-async-reply-replay-is-idempotent ()
  "Replayed callbacks cannot duplicate a running, consumed or saved reply."
  (lispm-test-env
    (let* ((boss (lispm-defagent boss :root project))
           (helper (lispm-defagent helper :root project))
           (boss-file (buffer-file-name (lispm-session-buffer boss))))
      (setq lispm-test--scripts '((boss hang) (helper hang)))
      (should-not (cdr (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "Review")))))
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state helper) 'requesting))))
      (let* ((route (plist-get (lispm-agent-entry helper) :reply-to))
             (helper-file (buffer-file-name (lispm-agent-buffer helper)))
             (callback (lispm-collab--callback helper route)))
        (should callback)
        (lispm-test-async--reply helper "One result only.")
        ;; Before the inbox drains, this is the exact same delivery.
        (funcall callback "One result only.")
        (should (= (length (lispm-agent-inbox boss)) 1))
        (should (lispm-test--wait (lambda () (eq (lispm-agent-state boss) 'requesting))))
        (funcall callback "One result only.")
        (should-not (lispm-agent-inbox boss))
        (lispm-test-async--reply boss "Recorded.")
        (funcall callback "One result only.")
        (should-not (lispm-agent-inbox boss))
        (should (= (lispm-test--count "One result only\\." (lispm-test--transcript boss)) 1))
        ;; A summary may omit the reply altogether.  Its receipt in the
        ;; earlier continuation must still suppress callback replay.
        (setf (lispm-agent-compacting boss) t)
        (lispm--continue boss "The previous review has been handled.")
        (setq boss-file (buffer-file-name (lispm-agent-buffer boss)))
        (should-not (string-search "One result only." (lispm-test--transcript boss)))
        (funcall callback "One result only.")
        (should-not (lispm-agent-inbox boss))
        (lispm-test-async--crash-all)
        (let* ((revived (lispm-resume-file boss-file))
               (again (lispm-resume-file helper-file)))
          (funcall (lispm-collab--callback again route) "One result only.")
          (should-not (lispm-agent-inbox revived))
          (should (eq (lispm-agent-state revived) 'idle))
          (should-not (string-search "One result only." (lispm-test--transcript revived))))))))

(ert-deftest lispm-test-async-completed-reply-restarts-with-value ()
  "A completed child with an unacknowledged inbox entry replays text and value."
  (lispm-test-env
    (let* ((boss (lispm-defagent boss :root project))
           (helper (lispm-defagent helper :root project))
           (boss-id (progn (lispm-session-buffer boss) (lispm-agent-id boss))))
      (setq lispm-test--scripts
            `((helper ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "value" "return_value"
                                                         '((value . ["[[file:result.el::7]]"]))))
                      hang)))
      (should-not (cdr (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "Find the result")))))
      (should (lispm-test--wait
               (lambda () (and (eq (lispm-agent-state helper) 'requesting)
                               (equal (lispm-agent-value helper) ["[[file:result.el::7]]"])))))
      (let ((helper-file (buffer-file-name (lispm-agent-buffer helper)))
            (running-inbox (lispm-test--inbox project)))
        ;; Model the crash window after the completed transcript was saved
        ;; but before its reply was delivered and RUNNING entry removed.
        (setf (lispm-agent-turn-callbacks helper) nil)
        (lispm-test-async--reply helper "Found the result.")
        (let ((coding-system-for-write 'utf-8))
          (write-region running-inbox nil (lispm-inbox-file project) nil 'silent))
        (lispm-test-async--crash-all)
        (setq lispm-test--requests nil
              lispm-test--scripts
              `((boss ,(lispm-test--msg "end_turn" (lispm-test--text "Result restored.")))))
        (lispm-resume-file helper-file)
        (should (lispm-test--wait
                 (lambda ()
                   (when-let* ((revived (lispm-get-agent boss-id t)))
                     (and (funcall (lispm-test--idle-p revived))
                          (equal (lispm-agent-last-reply revived) "Result restored."))))))
        (should (= (length lispm-test--requests) 1))
        (let ((messages (format "%s" (alist-get 'messages (car lispm-test--requests)))))
          (should (string-search "Found the result." messages))
          (should (string-search "Value:" messages))
          (should (string-search "[[file:result.el::7]]" messages)))))))

(ert-deftest lispm-test-async-active-request-resumes-independently ()
  "Restoring active agents delivers an async reply to its already-idle caller."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes.")
    (let* ((default-directory project)
           (boss (lispm-start))
           (boss-id (lispm-agent-id boss)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "ask" "ask_agent"
                                                   '((agent . "reviewer") (request . "Review this."))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Review requested.")))
              (reviewer@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "value" "return_value" '((value . [17 23]))))
               hang)))
      (lispm-tell boss "Request a review")
      (should (lispm-test--wait
               (lambda ()
                 (and (funcall (lispm-test--idle-p boss))
                      (when-let* ((helper (lispm-get-agent "reviewer@project" t)))
                        (and (eq (lispm-agent-state helper) 'requesting)
                             (equal (lispm-agent-value helper) [17 23])))))))
      (let* ((helper (lispm-get-agent "reviewer@project"))
             (id (lispm-test-async--request-id helper)))
        (should-not (lispm-agent-waiting-on boss))
        (should (zerop (hash-table-count (lispm-agent-pending boss))))
        (lispm-test-async--crash-all)
        (setq lispm-test--requests nil
              lispm-test--scripts
              `((reviewer@project ,(lispm-test--msg "end_turn" (lispm-test--text "Restored review.")))
                (assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Restored reply received.")))))
        (lispm-resume-agents)
        (should (lispm-test--wait
                 (lambda ()
                   (when-let* ((revived (lispm-get-agent boss-id t)))
                     (and (funcall (lispm-test--idle-p revived))
                          (equal (lispm-agent-last-reply revived) "Restored reply received.")
                          (not (lispm-get-agent "reviewer@project" t)))))))
        (let ((revived (lispm-get-agent boss-id)))
          (should-not (lispm-agent-waiting-on revived))
          (should (string-search id (lispm-test--transcript revived)))
          (should (string-search "Restored review." (lispm-test--transcript revived)))
          (should (string-search "Value:" (lispm-test--transcript revived)))
          (should (string-search "[17,23]" (lispm-test--transcript revived))))))))

(ert-deftest lispm-test-async-helper-keeps-a-second-queued-request ()
  "A helper's retirement timer cannot discard a follow-up agent request."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes.")
    (let ((boss (lispm-defagent boss :root project)))
      (lispm-session-buffer boss)
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "end_turn" (lispm-test--text "First received."))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Second received.")))
              (reviewer@project hang hang)))
      (should-not (cdr (lispm-test--call boss "ask_agent"
                                       '((agent . "reviewer") (request . "First review")))))
      (should (lispm-test--wait
               (lambda ()
                 (when-let* ((helper (lispm-get-agent "reviewer@project" t)))
                   (eq (lispm-agent-state helper) 'requesting)))))
      (let ((helper (lispm-get-agent "reviewer@project")))
        (should-not (cdr (lispm-test--call boss "ask_agent"
                                         '((agent . "reviewer@project") (request . "Second review")))))
        (should (= (length (lispm-agent-inbox helper)) 1))
        (lispm-test-async--reply helper "First review done.")
        (should (lispm-test--wait
                 (lambda ()
                   (and (memq helper (lispm-agents))
                        (eq (lispm-agent-state helper) 'requesting)
                        (equal (plist-get (lispm-agent-entry helper) :text) "Second review")))))
        ;; Also exercise a timer firing after the second turn started and
        ;; drained the queue, when checking only the queue would retire it.
        (lispm-collab--retire helper)
        (should (memq helper (lispm-agents)))
        (lispm-test-async--reply helper "Second review done.")
        (should (lispm-test--wait
                 (lambda () (and (funcall (lispm-test--idle-p boss))
                                 (equal (lispm-agent-last-reply boss) "Second received.")))))
        (should (string-search "First review done." (lispm-test--transcript boss)))
        (should (string-search "Second review done." (lispm-test--transcript boss)))))))

(defun lispm-test-async--nested (restart)
  "Exercise nested requests, optionally restoring them after RESTART."
  (lispm-test-env
    (lispm-defrole middle "Combines delegated answers.")
    (let* ((boss (lispm-defagent boss :root project))
           (first (lispm-defagent first :root project))
           (second (lispm-defagent second :root project)))
      (lispm-session-buffer boss)
      (let ((boss-id (lispm-agent-id boss)))
        (setq lispm-test--scripts
              `((boss ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "ask" "ask_agent"
                                                         '((agent . "middle") (request . "Combine both answers."))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Requested."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Combined result received.")))
                (middle@project
                 ,(lispm-test--msg "tool_use"
                                   (lispm-test--tool "one" "ask_agent" '((agent . "first") (request . "First value")))
                                   (lispm-test--tool "two" "ask_agent" '((agent . "second") (request . "Second value"))))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Both workers requested."))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Still working; I can answer you."))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "One worker remains."))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Combined final answer: 40.")))
                (first hang) (second hang)))
        ;; This helper runs twice; the fake API consumes scripts by
        ;; mutating their tails, including backquote's constant tail.
        (setq lispm-test--scripts (copy-tree lispm-test--scripts))
        (lispm-tell boss "Delegate the calculation")
        (should (lispm-test--wait
                 (lambda ()
                   (when-let* ((middle (lispm-get-agent "middle@project" t)))
                     (and (funcall (lispm-test--idle-p boss))
                          (funcall (lispm-test--idle-p middle))
                          (= (length (lispm-agent-suspended middle)) 1)
                          (eq (lispm-agent-state first) 'requesting)
                          (eq (lispm-agent-state second) 'requesting))))))
        (let* ((middle (lispm-get-agent "middle@project"))
               (middle-id (lispm-agent-id middle))
               (first-id (lispm-agent-id first))
               (second-id (lispm-agent-id second)))
          ;; A status question must not consume the suspended request's
          ;; callback.  The helper's retirement check must keep it alive.
          (lispm-tell middle "Status?")
          (should (lispm-test--wait (lispm-test--idle-p middle)))
          (should (equal (lispm-agent-last-reply middle) "Still working; I can answer you."))
          (should (equal (lispm-agent-last-reply boss) "Requested."))
          (lispm-collab--retire middle)
          (should (memq middle (lispm-agents)))
          (when restart
            (lispm-test-async--crash-all)
            (setq lispm-test--scripts
                  `((boss ,(lispm-test--msg "end_turn" (lispm-test--text "Combined result received.")))
                    (middle@project
                     ,(lispm-test--msg "end_turn" (lispm-test--text "One worker remains."))
                     ,(lispm-test--msg "end_turn" (lispm-test--text "Combined final answer: 40.")))
                    (first hang) (second hang)))
            (setq lispm-test--scripts (copy-tree lispm-test--scripts))
            (lispm-resume-agents)
            (setq boss (lispm-resume-session boss-id)
                  middle (lispm-get-agent middle-id)
                  first (lispm-get-agent first-id)
                  second (lispm-get-agent second-id))
            (should (lispm-test--wait
                     (lambda () (and (eq (lispm-agent-state first) 'requesting)
                                     (eq (lispm-agent-state second) 'requesting))))))
          ;; Replies deliberately arrive out of order.  The first reply
          ;; yields again; only the second completes the upstream request.
          (lispm-test-async--reply second "23")
          (should (lispm-test--wait
                   (lambda () (and (funcall (lispm-test--idle-p middle))
                                   (equal (lispm-agent-last-reply middle) "One worker remains.")))))
          (should (= (length (lispm-agent-suspended middle)) 1))
          (should (equal (lispm-agent-last-reply boss) (unless restart "Requested.")))
          (lispm-test-async--reply first "17")
          (should (lispm-test--wait
                   (lambda ()
                     (and (funcall (lispm-test--idle-p boss))
                          (equal (lispm-agent-last-reply boss) "Combined result received.")
                          (not (lispm-get-agent middle-id t))))))
          (should (string-search "Combined final answer: 40." (lispm-test--transcript boss)))
          (should-not (string-search "Both workers requested." (lispm-test--transcript boss)))
          (should-not (string-search "One worker remains." (lispm-test--transcript boss))))))))

(ert-deftest lispm-test-async-nested-requests-deliver-final-answer ()
  "Nested requests yield without completing their upstream request early."
  (lispm-test-async--nested nil))

(ert-deftest lispm-test-async-nested-requests-survive-restart ()
  "A suspended upstream route and its dependencies survive Emacs."
  (lispm-test-async--nested t))

(ert-deftest lispm-test-async-request-ids-ignore-session-id-collisions ()
  "Session and inbox id collisions cannot alias two independent requests."
  (lispm-test-env
    (let ((boss (lispm-defagent boss :root project))
          (first (lispm-defagent first :root project))
          (second (lispm-defagent second :root project)))
      (dolist (agent (list boss first second)) (lispm-session-buffer agent))
      (setq lispm-test--scripts '((first hang) (second hang)))
      (cl-letf (((symbol-function 'lispm--new-id) (lambda () "20261029-222247-d3cd")))
        (lispm-test--call boss "ask_agent" '((agent . "first") (request . "First")))
        (lispm-test--call boss "ask_agent" '((agent . "second") (request . "Second"))))
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state first) 'requesting)
                                              (eq (lispm-agent-state second) 'requesting)))))
      (should-not (equal (lispm-test-async--request-id first) (lispm-test-async--request-id second)))
      (setf (lispm-agent-state boss) 'requesting)
      (lispm-test-async--reply first "First result")
      (lispm-test-async--reply second "Second result")
      (should (= (length (lispm-agent-inbox boss)) 2)))))

(ert-deftest lispm-test-async-nested-return-value-survives-restart ()
  "A value set through run_lisp replaces an earlier direct value after restart."
  (lispm-test-env
    (let ((boss (lispm-defagent boss :root project))
          (helper (lispm-defagent helper :root project)))
      (setf (lispm-agent-policy helper) '((comm . auto) (lisp . auto)))
      (setq lispm-test--scripts
            `((helper ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "first" "return_value" '((value . [1]))))
                      ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "nested" "run_lisp"
                                                         '((code . "(return_value :value [2])"))))
                      hang)))
      (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "Return the final value")))
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state helper) 'requesting)
                                              (equal (lispm-agent-value helper) [2])))))
      (let ((file (buffer-file-name (lispm-agent-buffer helper)))
            (boss-id (lispm-agent-id boss)))
        (lispm-test-async--crash-all)
        (setq lispm-test--scripts
              `((helper hang)
                (boss ,(lispm-test--msg "end_turn" (lispm-test--text "Restored value received.")))))
        (let ((revived (lispm-resume-file file)))
          (should (equal (lispm-agent-value revived) [2]))
          (should (lispm-test--wait (lambda () (eq (lispm-agent-state revived) 'requesting))))
          (lispm-test-async--reply revived "Final value."))
        (should (lispm-test--wait
                 (lambda () (when-let* ((revived-boss (lispm-get-agent boss-id t)))
                              (equal (lispm-agent-last-reply revived-boss) "Restored value received.")))))
        (should (string-search "Value:\n[2]" (lispm-test--transcript (lispm-get-agent boss-id))))))))

(ert-deftest lispm-test-async-compacted-return-value-survives-restart ()
  "A value survives restart even when its tool call is in an old continuation."
  (lispm-test-env
    (let ((boss (lispm-defagent boss :root project))
          (helper (lispm-defagent helper :root project))
          (lispm-compact-save-memory nil))
      (setq lispm-test--scripts
            `((helper ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "value" "return_value" '((value . [4]))))
                      hang hang)))
      (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "Return the final value")))
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state helper) 'requesting)
                                              (equal (lispm-agent-value helper) [4])))))
      (setf (lispm-agent-compacting helper) t)
      (lispm--continue helper "The calculation is almost finished." t)
      (let ((file (buffer-file-name (lispm-agent-buffer helper)))
            (boss-id (lispm-agent-id boss)))
        (lispm-test-async--crash-all)
        (setq lispm-test--scripts
              `((helper hang)
                (boss ,(lispm-test--msg "end_turn" (lispm-test--text "Restored value received.")))))
        (let ((revived (lispm-resume-file file)))
          (should (equal (lispm-agent-value revived) [4]))
          (should (lispm-test--wait (lambda () (eq (lispm-agent-state revived) 'requesting))))
          (lispm-test-async--reply revived "Final value."))
        (should (lispm-test--wait
                 (lambda () (when-let* ((revived-boss (lispm-get-agent boss-id t)))
                              (equal (lispm-agent-last-reply revived-boss) "Restored value received.")))))
        (should (string-search "Value:\n[4]" (lispm-test--transcript (lispm-get-agent boss-id))))))))

(ert-deftest lispm-test-async-interleaved-upstream-requests-stay-separate ()
  "Two suspended requests in one agent keep their own upstream recipients."
  (lispm-test-env
    (let ((one (lispm-defagent one :root project))
          (two (lispm-defagent two :root project))
          (middle (lispm-defagent middle :root project))
          (first (lispm-defagent first :root project))
          (second (lispm-defagent second :root project)))
      (setq lispm-test--scripts
            `((one ,(lispm-test--msg "end_turn" (lispm-test--text "First final received.")))
              (two ,(lispm-test--msg "end_turn" (lispm-test--text "Second final received.")))
              (middle
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "first" "ask_agent" '((agent . "first") (request . "Value one"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "First task yielded."))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "second" "ask_agent" '((agent . "second") (request . "Value two"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Second task yielded."))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Second final result."))
               ,(lispm-test--msg "end_turn" (lispm-test--text "First final result.")))
              (first hang) (second hang)))
      (lispm-test--call one "ask_agent" '((agent . "middle") (request . "First task")))
      (lispm-test--call two "ask_agent" '((agent . "middle") (request . "Second task")))
      (should (lispm-test--wait
               (lambda () (and (funcall (lispm-test--idle-p middle))
                               (= (length (lispm-agent-suspended middle)) 2)))))
      (lispm-test-async--reply second "Value two")
      (should (lispm-test--wait
               (lambda () (equal (lispm-agent-last-reply two) "Second final received."))))
      (should-not (lispm-agent-last-reply one))
      (lispm-test-async--reply first "Value one")
      (should (lispm-test--wait
               (lambda () (equal (lispm-agent-last-reply one) "First final received."))))
      (should-not (lispm-agent-suspended middle))
      (should-not (string-search "Second final result." (lispm-test--transcript one)))
      (should-not (string-search "First final result." (lispm-test--transcript two))))))

(ert-deftest lispm-test-async-suspended-peer-request-fails-after-restart ()
  "Losing a peer request at restart still completes its upstream request."
  (lispm-test-with-peers
    (lispm-defrole middle "Delegates to a peer.")
    (let* ((boss (lispm-defagent boss :root project))
           (peer (lispm-test--peer 'codex project)))
      (should (buffer-live-p peer))
      (setq lispm-test--scripts
            `((middle@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "peer" "ask_agent" '((agent . "codex") (request . "Compute"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Peer requested.")))))
      (lispm-test--call boss "ask_agent" '((agent . "middle") (request . "Compute through peer")))
      (should (lispm-test--wait
               (lambda () (when-let* ((middle (lispm-get-agent "middle@project" t)))
                            (and (funcall (lispm-test--idle-p middle))
                                 (lispm-agent-suspended middle))))))
      (let ((boss-id (lispm-agent-id boss)))
        (lispm-test-async--crash-all)
        (setq lispm-collab--requests nil
              lispm-test--scripts
              `((middle@project ,(lispm-test--msg "end_turn" (lispm-test--text "Peer request was lost on restart.")))
                (boss ,(lispm-test--msg "end_turn" (lispm-test--text "Failure received.")))))
        (lispm-resume-agents)
        (should (lispm-test--wait
                 (lambda () (when-let* ((revived (lispm-get-agent boss-id t)))
                              (equal (lispm-agent-last-reply revived) "Failure received.")))))
        (should-not (lispm-get-agent "middle@project" t))))))

(provide 'lispm-collab-async-test)
;;; lispm-collab-async-test.el ends here
