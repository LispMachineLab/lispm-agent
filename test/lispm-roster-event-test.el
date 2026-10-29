;;; lispm-roster-event-test.el --- Roster event cost and isolation tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after the fixtures in lispm-test.el.  Notifications follow
;; source events; the absence of an observer must leave those sources free
;; of roster work, and one local change must not rebuild every agent.

;;; Code:

(ert-deftest lispm-test-roster-does-not-watch-every-command ()
  "Ordinary Emacs commands do not cause global roster comparisons."
  (should-not (memq #'lispm-agents-roster--changed (default-value 'post-command-hook))))

(ert-deftest lispm-test-roster-no-observer-does-no-work ()
  "Every event entry point returns before doing work without an observer.
This also protects process and terminal callbacks whose arguments would
otherwise require inspection."
  (let ((lispm-agents-roster-change-functions nil)
        (lispm-agents-roster--timer nil)
        (fail (lambda (&rest _) (ert-fail "Roster work without an observer"))))
    (cl-letf (((symbol-function 'lispm-agents-roster--collect) fail))
      (lispm-agents-roster-notify 'unused '(status) '(:type unused)))
    (cl-letf (((symbol-function 'lispm-agents-roster-notify) fail)
              ((symbol-function 'lispm-agents-roster-key) fail)
              ((symbol-function 'lispm-inspect--copy-data) fail)
              ((symbol-function 'lispm-agents-buffer-status) fail)
              ((symbol-function 'lispm-agents-terminal-agent) fail)
              ((symbol-function 'lispm-agents-buffer-p) fail)
              ((symbol-function 'lispm-agents-bridge-observe) fail)
              ((symbol-function 'lispm-agents-roster) fail)
              ((symbol-function 'lispm-agents) fail)
              ((symbol-function 'run-at-time) fail)
              ((symbol-function 'cancel-timer) fail)
              ((symbol-function 'process-buffer) fail))
      (lispm-agents-roster--changed)
      (lispm-agents-roster--opened)
      (lispm-agents-roster--title 'unused)
      (lispm-agents-roster--character 'unused)
      (lispm-agents-roster--state 'unused)
      (lispm-agents-roster--input 'unused)
      (lispm-agents-roster--sessions)
      (lispm-agents-roster--terminal)
      (lispm-agents-roster--exited 'unused)
      (lispm-agents-roster--closing)
      (lispm-agents-roster--collect 'unused '(status) '(:type unused))
      (lispm-agents-roster--flush))))

(ert-deftest lispm-test-roster-observer-can-reattach-after-expired-flush ()
  "A disabled observer's spent timer cannot prevent later notifications.
Events left over when it closed are not replayed to the new observer."
  (lispm-test-env
    (let ((agent (lispm-start 'assistant project)))
      (lispm-test--roster-observer
        (let ((observer lispm-agents-roster-change-functions))
          (lispm-agents-roster-notify agent '(status) '(:type message-queued :id "closed"))
          (let ((expired lispm-agents-roster--timer))
            (setq lispm-agents-roster-change-functions nil)
            (should (lispm-test--wait (lambda () (not (memq expired timer-list)))))
            (should-not changes)
            (setq lispm-agents-roster-change-functions observer)
            (lispm-agents-roster-notify agent '(name) '(:type message-queued :id "current"))
            (should (lispm-test--wait (lambda () changes)))
            (should (= (length changes) 1))
            (should (memq 'name (cadar changes)))
            (should-not (memq 'status (cadar changes)))
            (should (equal (mapcar (lambda (event) (plist-get event :id))
                                  (plist-get (nth 2 (car changes)) :events))
                           '("current")))))))))

(ert-deftest lispm-test-roster-terminal-chunks-report-data-changes ()
  "Terminal text alone emits nothing; status and mailbox changes emit once.
An output chunk is no evidence that a tool call changed."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (status '(:state idle :progress nil :msg nil))
           (notices nil)
           (lispm-agents-roster-change-functions (list #'ignore)))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "chunk-test"))
      (cl-letf (((symbol-function 'lispm-agents-buffer-status) (lambda (_) status))
                ((symbol-function 'lispm-agents-roster-notify)
                 (lambda (subject fields &optional event)
                   (push (list subject fields event) notices))))
        (with-current-buffer terminal
          ;; The first observation establishes a baseline, not a change.
          (lispm-agents-roster--terminal)
          (should-not notices)
          (insert "Ordinary terminal output\n")
          (lispm-agents-roster--terminal)
          (should-not notices)
          (setq status '(:state working :progress 10 :msg "Reading"))
          (lispm-agents-roster--terminal)
          (should (= (length notices) 1))
          (should (eq (caar notices) terminal))
          (should (memq 'status (cadar notices)))
          (should-not (memq 'calls (cadar notices)))
          (setq notices nil)
          (lispm-agents-roster--terminal)
          (should-not notices)
          (setq lispm-agents-bridge--queue '((:id "queued" :text "Next")))
          (lispm-agents-roster--terminal)
          (should (= (length notices) 1))
          (should (memq 'mailbox (cadar notices)))
          (should-not (memq 'calls (cadar notices)))
          (setq notices nil)
          (lispm-agents-roster--terminal)
          (should-not notices))))))

(ert-deftest lispm-test-roster-builds-wait-index-once ()
  "One snapshot indexes delegations, inbox requests and peer waits once.
Every record then reads its own relations from the shared index."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (first (lispm-start 'assistant project))
           (second (lispm-start 'assistant project))
           (peer-name (lispm-agents-bridge-name terminal))
           (build (symbol-function 'lispm-agents-roster--build-wait-index))
           (builds 0)
           (lispm-collab--requests
            (list (list "R8" peer-name #'ignore
                        (list :from (symbol-name (lispm-agent-name first)) :to peer-name)))))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "indexed"))
      (setf (lispm-agent-waiting-on first) (list second)
            (lispm-agent-inbox second)
            (list (list :from (lispm-agent-name first) :request-id "R9" :text "Review")))
      (cl-letf (((symbol-function 'lispm-agents-roster--build-wait-index)
                 (lambda (&rest arguments)
                   (cl-incf builds)
                   (apply build arguments))))
        (let* ((records (lispm-agents-roster))
               (caller (seq-find (lambda (entry) (eq (plist-get entry :buffer) (lispm-agent-buffer first)))
                                  records))
               (callee (seq-find (lambda (entry) (eq (plist-get entry :buffer) (lispm-agent-buffer second)))
                                  records))
               (peer (seq-find (lambda (entry) (eq (plist-get entry :buffer) terminal)) records)))
          (should (= builds 1))
          (should (member (lispm-agents-roster-key second) (plist-get caller :waits-on)))
          (should (member '(codex "indexed") (plist-get caller :waits-on)))
          (should (member (lispm-agents-roster-key first) (plist-get callee :waited-on-by)))
          (should (member (lispm-agents-roster-key first) (plist-get peer :waited-on-by)))
          (should (member "R8" (plist-get caller :request-ids)))
          (should (member "R9" (plist-get caller :request-ids))))
        ;; A direct inspector query is fresh rather than retaining the
        ;; prior snapshot's now-settled peer request.
        (setq lispm-collab--requests nil)
        (let ((waits (lispm-agents-roster-waits (lispm-agent-buffer first))))
          (should-not (member '(codex "indexed") (plist-get waits :waits-on)))
          (should (member (lispm-agents-roster-key second) (plist-get waits :waits-on))))))))

(ert-deftest lispm-test-roster-snapshot-keeps-unreported-terminal-change ()
  "Reading another snapshot cannot swallow a terminal's pending status change."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (status '(:state idle))
           (notices nil)
           (lispm-agents-roster-change-functions (list #'ignore)))
      (cl-letf (((symbol-function 'lispm-agents-buffer-status) (lambda (_) status))
                ((symbol-function 'lispm-agents-roster-notify)
                 (lambda (_subject fields &optional _event) (push fields notices))))
        (lispm-agents-roster)
        (setq status '(:state working))
        (lispm-agents-roster)
        (with-current-buffer terminal (lispm-agents-roster--terminal))
        (should (= (length notices) 1))
        (should (memq 'status (car notices)))))))

(ert-deftest lispm-test-roster-partial-flush-rebuilds-only-dirty-agent ()
  "A local status notification recomputes only its agent's record.
The other conversation remains in the cached snapshot without being read."
  (lispm-test-env
    (let* ((first (lispm-start 'assistant project))
           (second (lispm-start 'assistant project))
           (first-buffer (lispm-agent-buffer first))
           (second-key (lispm-agents-roster-key second))
           (record (symbol-function 'lispm-agents-roster--record))
           (read nil))
      (lispm-test--roster-observer
        (let ((second-before (seq-find (lambda (entry) (equal (plist-get entry :key) second-key))
                                       lispm-agents-roster--previous)))
          (cl-letf (((symbol-function 'lispm-agents-roster--record)
                     (lambda (buffer &rest arguments)
                       (push buffer read)
                       (apply record buffer arguments)))
                    ((symbol-function 'lispm-agents-roster)
                     (lambda () (ert-fail "A local change rebuilt the complete roster"))))
            (setf (lispm-agent-status first) "Changed")
            (lispm-agents-roster-notify first '(status))
            (lispm-agents-roster--flush))
          (should (equal read (list first-buffer)))
          (should (= (length changes) 1))
          (should (equal (caar changes) (list (lispm-agents-roster-key first))))
          (should (equal second-before
                         (seq-find (lambda (entry) (equal (plist-get entry :key) second-key))
                                   lispm-agents-roster--previous))))))))

(ert-deftest lispm-test-roster-full-flush-keeps-unknown-identities-separate ()
  "Topology changes remove known and unknown agents without losing a peer.
A remaining terminal can then learn its stable session key."
  (lispm-test-with-peers
    (let ((remaining (lispm-test--peer 'codex project))
          (unknown (lispm-test--peer 'codex project))
          (known (lispm-test--peer 'codex project)))
      (with-current-buffer known (setq lispm-agents-buffer-session-id "departed"))
      (lispm-test--roster-observer
        (kill-buffer unknown)
        (kill-buffer known)
        (lispm-agents-roster--flush)
        (should (= (length changes) 1))
        (should (member '(codex "departed") (caar changes)))
        (should (memq 'agents (cadar changes)))
        (should-not (memq nil (caar changes)))
        (should (= (length lispm-agents-roster--previous) 1))
        (should (eq (plist-get (car lispm-agents-roster--previous) :buffer) remaining))
        (should-not (plist-get (car lispm-agents-roster--previous) :key))
        (setq changes nil)
        (with-current-buffer remaining (setq lispm-agents-buffer-session-id "learned"))
        (lispm-agents-roster-notify nil '(agents))
        (lispm-agents-roster--flush)
        (should (= (length changes) 1))
        (should (member '(codex "learned") (caar changes)))
        (should (memq 'agents (cadar changes)))
        (should (= (length lispm-agents-roster--previous) 1))
        (should (equal (plist-get (car lispm-agents-roster--previous) :key) '(codex "learned")))))))

(provide 'lispm-roster-event-test)
;;; lispm-roster-event-test.el ends here
