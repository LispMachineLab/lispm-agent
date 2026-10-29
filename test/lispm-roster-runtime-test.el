;;; lispm-roster-runtime-test.el --- Roster runtime tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded by lispm-test.el, whose fixtures keep sessions and projects in
;; scratch directories and replace the model with scripted replies.

;;; Code:

(ert-deftest lispm-test-roster-approve-all-one-agent ()
  "Approve All runs this agent's permissions, leaving another's waiting."
  (lispm-test-env
    (let ((first (lispm-defagent first :root project))
          (second (lispm-defagent second :root project)))
      (setq lispm-test--scripts
            `((first ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "a" "write_file"
                                                         '((path . "a.txt") (content . "a")))
                                       (lispm-test--tool "b" "write_file"
                                                         '((path . "b.txt") (content . "b"))))
                     ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))
              (second ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "c" "write_file"
                                                          '((path . "c.txt") (content . "c")))))))
      (lispm-tell first "Write a and b")
      (lispm-tell second "Write c")
      (should (lispm-test--wait
               (lambda () (and (= (length (lispm-waiting-calls first)) 2)
                               (= (length (lispm-waiting-calls second)) 1)))))
      (should (= (lispm-approve-all-for first) 2))
      (should (lispm-test--wait (lispm-test--idle-p first)))
      (should (file-exists-p (expand-file-name "a.txt" project)))
      (should (file-exists-p (expand-file-name "b.txt" project)))
      (should-not (file-exists-p (expand-file-name "c.txt" project)))
      (should (equal (lispm-waiting-calls second) '("c")))
      (should-not (lispm-agent-allowed first)))))

(ert-deftest lispm-test-roster-approve-all-snapshot ()
  "New permissions, older turns and stopped calls are outside Approve All."
  (with-temp-buffer
    (insert "abcd")
    (let* ((agent (lispm-agent--create :turn 2))
           (pending (lispm-agent-pending agent))
           (asking (list :name "write_file"))
           (approved nil))
      (puthash "a" (list :status 'ask :turn 2 :marker (copy-marker 1)) pending)
      (puthash "b" (list :status 'ask :turn 2 :asking asking :marker (copy-marker 2)) pending)
      (puthash "old" (list :status 'ask :turn 1) pending)
      (puthash "stopped" (list :status 'stopped :turn 2) pending)
      (cl-letf (((symbol-function 'lispm-resolve)
		 (lambda (_agent id decision &optional _reason)
                   (should (eq decision 'approve))
                   (push id approved)
                   (remhash id pending)
                   (puthash "new" (list :status 'ask :turn 2) pending)
                   ;; The first call's completion replaced a nested approval.
                   (plist-put (gethash "b" pending) :asking (list :name "run_shell")))))
	(should (= (lispm-approve-all-for agent) 1)))
      (should (equal approved '("a")))
      (should (equal (plist-get (gethash "b" pending) :asking) '(:name "run_shell")))
      (should (gethash "old" pending))
      (should (gethash "stopped" pending))
      (should (gethash "new" pending)))))

(ert-deftest lispm-test-roster-approve-all-lists-before-acting ()
  "The command lists its snapshot and never approves a later turn."
  (let* ((agent (lispm-agent--create :name 'Ada :turn 1))
         (lispm-session-agent agent)
         (prompt nil)
         (approved nil))
    (puthash "a" (list :status 'ask :turn 1 :name "write_file") (lispm-agent-pending agent))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (text)
                 (setq prompt text)
                 (cl-incf (lispm-agent-turn agent))
                 t))
              ((symbol-function 'lispm-resolve)
               (lambda (&rest _) (setq approved t))))
      (lispm-approve-all))
    (should (string-search "Ada" prompt))
    (should (string-search "a (write_file)" prompt))
    (should-not approved)))

(ert-deftest lispm-test-roster-runtime-mail-events ()
  "Enqueue and taking a message retain its identity and sender."
  (lispm-test-env
    (let ((agent (lispm-defagent worker :root project))
          (events nil))
      (setq lispm-test--scripts '((worker hang)))
      (cl-letf (((symbol-function 'lispm-agents-roster-notify)
                 (lambda (_subject _fields &optional event)
                   (when event (push event events)))))
        (lispm-tell agent "Read this" :from "sender" :reply-to "session call")
        (let ((queued (seq-find (lambda (event) (eq (plist-get event :type) 'message-queued)) events)))
          (should (equal (plist-get queued :from) "sender"))
          (should (equal (plist-get queued :to) (list 'lispm (lispm-agent-id agent))))
          (should (equal (plist-get queued :request-id) "session call"))
          (should (equal (plist-get queued :id) (plist-get (car (lispm-agent-inbox agent)) :id)))
          (should (lispm-test--wait (lambda () (lispm-agent-entry agent))))
          (let ((taken (seq-find (lambda (event) (eq (plist-get event :type) 'message-taken)) events)))
            (should (equal (plist-get queued :id) (plist-get taken :id)))))))))

(ert-deftest lispm-test-roster-peer-request-metadata ()
  "Peer envelopes and outstanding requests retain their endpoints."
  (let* ((metadata nil)
         (events nil)
         (answers nil)
         (lispm-collab--requests nil)
         (lispm-collab--request-count 0)
         (peer (lispm-peer-create
                :name "codex" :kind "Codex" :root "/tmp/"
                :deliver (lambda (_text)
                           (setq metadata (copy-tree lispm-collab-delivery-metadata))))))
    (cl-letf (((symbol-function 'lispm-agents-roster-notify)
               (lambda (_subject _fields &optional event)
                 (when event (push event events)))))
      (lispm-tell-peer peer "Hello" "Ada")
      (should (equal (plist-get metadata :from) "Ada"))
      (should (equal (plist-get metadata :text) "Hello"))
      (should (numberp (plist-get metadata :queued)))
      (let ((message (plist-get metadata :id))
            (id (lispm-ask-peer peer "Review" "Ada" (lambda (&rest reply) (push reply answers)))))
        (should-not (equal message (plist-get metadata :id)))
        (should (equal (plist-get metadata :request-id) id))
        (should (equal (plist-get metadata :reply-to) id))
        (should (equal (plist-get (nth 3 (assoc id lispm-collab--requests)) :from) "Ada"))
        (should (equal (plist-get (nth 3 (assoc id lispm-collab--requests)) :to) "codex"))
        (lispm-peer-reply peer id "Reviewed")
        (should-not lispm-collab--requests)
        (should (equal answers '(("Reviewed"))))
        (should (equal (plist-get (car events) :request-id) id))
        (should (eq (plist-get (car events) :type) 'request-settled))))))

(ert-deftest lispm-test-roster-peer-asks-lispm ()
  "A peer's Rn follows the request into LispM's mailbox and its reply event."
  (lispm-test-with-peers
    (let* ((worker (lispm-defagent worker :root project))
           (terminal (lispm-test--peer 'codex project))
           (peer (lispm-agents-bridge--peer terminal))
           (caller (lispm-agents-bridge-agent terminal))
           (lispm-agents--archived nil)
           (events nil))
      (with-current-buffer terminal
        (setq lispm-agents-buffer-session-id "codex-session"))
      (setq lispm-test--scripts '((worker hang)))
      (cl-letf (((symbol-function 'lispm-agents-roster-notify)
                 (lambda (_subject _fields &optional event)
                   (when event (push event events)))))
        (let* ((id (lispm-collab-ask-for-peer caller peer 'worker "Review"))
               (key (list 'lispm (lispm-agent-id worker))))
          (should (equal (plist-get (car (lispm-agent-inbox worker)) :request-id) id))
          (should-not (plist-get (car (lispm-agent-inbox worker)) :reply-to))
          (should (equal (plist-get
                          (car (plist-get (plist-get (lispm-agents-roster-details key) :mailbox) :queued))
                          :request-id)
                         id))
          (should (memq worker (lispm-agent-waiting-on caller)))
          (should (lispm-test--wait (lambda () (lispm-agent-entry worker))))
          (should (equal (plist-get (lispm-agent-entry worker) :request-id) id))
          (should (equal (plist-get
                          (plist-get (plist-get (lispm-agents-roster-details key) :mailbox) :handling)
                          :request-id)
                         id))
          (setf (lispm-agent-last-reply worker) "Reviewed")
          (lispm--finish-turn worker)
          (should-not (lispm-agent-waiting-on caller))
          (let ((settled (seq-find (lambda (event) (eq (plist-get event :type) 'request-settled)) events)))
            (should (equal (plist-get settled :request-id) id))))))))

(ert-deftest lispm-test-roster-turn-end-invalidates-reverse-waits ()
  "Ending a caller's turn refreshes the old target's reverse wait too."
  (lispm-test-env
    (let* ((caller (lispm-start 'assistant project))
           (target (lispm-start 'assistant project))
           (caller-key (list 'lispm (lispm-agent-id caller)))
           (target-key (list 'lispm (lispm-agent-id target)))
           (lispm-agents--archived nil))
      (setf (lispm-agent-state caller) 'acting
            (lispm-agent-waiting-on caller) (list target))
      (should (equal (plist-get (lispm-agents-roster-waits (lispm-agent-buffer target)) :waited-on-by)
                     (list caller-key)))
      (lispm-test--roster-observer
        (lispm--finish-turn caller)
        (lispm-agents-roster--flush)
        (should (= (length changes) 1))
        (should (member caller-key (caar changes)))
        (should (member target-key (caar changes)))
        (should (memq 'waited-on-by (cadar changes)))
        (should-not (plist-get (lispm-agents-roster-waits (lispm-agent-buffer target)) :waited-on-by))))))

(ert-deftest lispm-test-roster-mail-removal-invalidates-request-sender ()
  "Removing a handled request refreshes its sender without a delegate callback."
  (lispm-test-env
    (let* ((sender (lispm-start 'assistant project))
           (target (lispm-start 'assistant project))
           (sender-key (list 'lispm (lispm-agent-id sender)))
           (lispm-agents--archived nil))
      (setq lispm-test--scripts (list (list (lispm-agent-name target) 'hang)))
      (lispm-tell target "Review" :from (lispm-agent-name sender) :request-id "R1")
      (lispm--drain-inbox target)
      (should (equal (plist-get (lispm-agents-roster-waits (lispm-agent-buffer sender)) :request-ids)
                     '("R1")))
      (lispm-test--roster-observer
        (lispm--finish-turn target)
        (lispm-agents-roster--flush)
        (should (= (length changes) 1))
        (should (member sender-key (caar changes)))
        (should (memq 'request-ids (cadar changes)))
        (should-not (plist-get (lispm-agents-roster-waits (lispm-agent-buffer sender)) :request-ids))))))

(provide 'lispm-roster-runtime-test)
;;; lispm-roster-runtime-test.el ends here
