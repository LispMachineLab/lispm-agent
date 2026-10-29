;;; lispm-roster-test.el --- Live roster tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after lispm-test.el's fixtures.  Snapshots must not read the
;; filesystem for work, and observers must receive each envelope once.

;;; Code:

(defmacro lispm-test--roster-observer (&rest body)
  "Run BODY with an isolated observer collecting batches in `changes'."
  (declare (indent 0))
  `(let* ((changes nil)
          (lispm-agents--archived (unless (eq lispm-agents--archived 'unloaded)
                                    lispm-agents--archived))
          (lispm-agents-roster--timer nil)
          (lispm-agents-roster--keys nil)
          (lispm-agents-roster--fields nil)
          (lispm-agents-roster--events nil)
          (lispm-agents-roster--pending-buffers nil)
          (lispm-agents-roster--all nil)
          (lispm-agents-roster--sequence 0)
          (lispm-agents-roster--previous (lispm-agents-roster))
          (lispm-agents-roster-change-functions
           (list (lambda (keys fields event) (push (list keys fields event) changes)))))
     (unwind-protect (progn ,@body)
       (when lispm-agents-roster--timer (cancel-timer lispm-agents-roster--timer)))))

(ert-deftest lispm-test-roster-snapshot ()
  "Alive LispM and terminal sessions have stable keys and plain data.
Archives are absent; snapshots neither scan transcripts nor expose strings."
  (lispm-test-env
    (lispm-test--with-agents '((codex :name "Codex" :program "emacs" :ready ignore))
      (let* ((agent (lispm-start 'assistant project))
             (terminal (generate-new-buffer "*codex:roster*"))
             (archived (generate-new-buffer "*codex:archived*"))
             (lispm-agents-terminal-modes '(fundamental-mode))
             (lispm-agents-terminal-live-p-function #'buffer-live-p)
             (lispm-agents-session-lookup-functions
              (list (lambda (kind id)
                      (when (and (eq kind 'codex) (equal id "child"))
                        (list :parent "parent" :cwd project)))))
             (lispm-agents--archived '("codex:old")))
        (unwind-protect
            (progn
              (lispm-register-character 'ada :title "Ada" :skill "review" :avatar "/tmp/ada.png")
              (setf (lispm-agent-character agent) 'ada
                    (lispm-agent-parent agent) "boss"
                    (lispm-agent-entry agent) '(:id "handling" :text "Current")
                    (lispm-agent-inbox agent) '((:id "one" :text "Next") (:id "two" :text "Later")))
              (with-current-buffer terminal
                (setq default-directory project lispm-agents-buffer-agent 'codex
                      lispm-agents-buffer-session-id "child" lispm-agents-buffer-title "Worker"
                      lispm-agents-bridge--queue '((:id "queued" :text "Read"))))
              (with-current-buffer archived
                (setq lispm-agents-buffer-agent 'codex lispm-agents-buffer-session-id "old"))
              (cl-letf (((symbol-function 'lispm-agents-sessions-list) (lambda (&rest _) (ert-fail "Session scan")))
                        ((symbol-function 'lispm-agents-terminal-session-id) (lambda (&rest _) (ert-fail "Session claim")))
                        ((symbol-function 'lispm-inbox-entries) (lambda (&rest _) (ert-fail "Inbox read")))
                        ((symbol-function 'lispm-transcript-sections) (lambda (&rest _) (ert-fail "Transcript scan"))))
                (let* ((roster (lispm-agents-roster))
                       (lisp (seq-find (lambda (record) (eq (plist-get record :kind) 'lispm)) roster))
                       (cli (seq-find (lambda (record) (eq (plist-get record :kind) 'codex)) roster)))
                  (should (= (length roster) 2))
                  (should (equal (plist-get lisp :key) (list 'lispm (lispm-agent-id agent))))
                  (should (equal (plist-get lisp :parent) '(lispm "boss")))
                  (should (plist-get lisp :alive))
                  (should-not (plist-get lisp :running))
                  (should (eq (plist-get (plist-get lisp :status) :state) 'idle))
                  (should (equal (plist-get lisp :character-name) "Ada"))
                  (should (plist-get lisp :app-character))
                  (should (equal (plist-get lisp :avatar) "/tmp/ada.png"))
                  (should (equal (plist-get lisp :mailbox) '(:handling t :queued 2)))
                  (should (memq 'approve-all (plist-get lisp :actions)))
                  (should (equal (plist-get cli :key) '(codex "child")))
                  (should (equal (plist-get cli :parent) '(codex "parent")))
                  (should (plist-get cli :running))
                  (should (equal (plist-get cli :mailbox) '(:handling nil :queued 1)))
                  (should (equal (plist-get cli :actions) '(send open-chat)))
                  (aset (cadr (plist-get lisp :key)) 0 ?X)
                  (should-not (equal (cadr (plist-get lisp :key)) (lispm-agent-id agent))))))
          (kill-buffer terminal)
          (kill-buffer archived))))))

(ert-deftest lispm-test-roster-unknown-terminal ()
  "A fresh terminal has no invented identity or unsupported actions."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'mute project))
           (record (car (lispm-agents-roster))))
      (should (eq (plist-get record :buffer) terminal))
      (should-not (plist-get record :key))
      (should-not (plist-get record :status))
      (should (equal (plist-get record :actions) '(open-chat)))
      (should-not (lispm-agents-roster-details nil)))))

(ert-deftest lispm-test-roster-inherits-archive-without-session-list ()
  "Live descendants inherit archives before any sidebar scans sessions."
  (lispm-test-env
    (let* ((parent (lispm-start 'assistant project))
           (child (lispm-start 'assistant project))
           (grandchild (lispm-start 'assistant project))
           (other (lispm-start 'assistant project))
           (lispm-agents--session-parents nil)
           (lispm-agents--effective-archive nil)
           (lispm-agents--archived (list (format "lispm:%s" (lispm-agent-id parent)))))
      (lispm-set-parent child (lispm-agent-id parent))
      (lispm-set-parent grandchild (lispm-agent-id child))
      (cl-letf (((symbol-function 'lispm-agents-sessions-list)
                 (lambda (&rest _) (ert-fail "Unexpected transcript scan")))
                ((symbol-function 'lispm-agents-sessions-snapshot)
                 (lambda (&rest _) (ert-fail "Unexpected session snapshot"))))
        (should (equal (mapcar (lambda (record) (plist-get record :key)) (lispm-agents-roster))
                       (list (list 'lispm (lispm-agent-id other)))))
        (should-not (lispm-agents-roster-buffer (list 'lispm (lispm-agent-id grandchild))))
        (should (eq (lispm-agents-roster-buffer (list 'lispm (lispm-agent-id other)))
                    (lispm-agent-buffer other))))
      (should-not lispm-agents--session-parents))))

(ert-deftest lispm-test-roster-coalesces ()
  "Several changes in one command yield one batch and ordered envelopes."
  (lispm-test-env
    (let ((lispm-agents--archived nil)
          (agent (lispm-start 'assistant project)))
      (lispm-test--roster-observer
        (let ((key (lispm-agents-roster-key agent)))
          (lispm-agents-roster-notify agent '(mailbox) '(:type message-queued :id "one" :from "a" :to "b"))
          (lispm-agents-roster-notify agent '(mailbox calls) '(:type message-queued :id "two" :request-id "R1"))
          (lispm-agents-roster-notify agent '(status))
          (should-not changes)
          (should (lispm-test--wait (lambda () changes)))
          (should (= (length changes) 1))
          (should (equal (caar changes) (list key)))
          (should (seq-every-p (lambda (field) (memq field (cadar changes))) '(mailbox calls status)))
          (let ((events (plist-get (nth 2 (car changes)) :events)))
            (should (equal (mapcar (lambda (event) (plist-get event :id)) events) '("one" "two")))
            (should (equal (mapcar (lambda (event) (plist-get event :sequence)) events) '(1 2))))
          (lispm-agents-roster-notify agent '(root))
          (should (lispm-test--wait (lambda () (= (length changes) 2))))
          (should-not (nth 2 (car changes))))))))

(ert-deftest lispm-test-roster-terminal-relations ()
  "Parsed parent metadata and bridge delegations retain terminal identities."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (proxy (lispm-agents-bridge-agent terminal))
           (helper (lispm-start 'assistant project))
           (lispm-agents-session-lookup-functions nil))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "terminal"))
      (lispm-agents-refresh-buffer-titles
       (list (list :agent 'codex :id "terminal" :title "Worker" :parent "parent" :cwd project)))
      (lispm-set-parent helper (lispm-agent-id proxy))
      (setf (lispm-agent-waiting-on proxy) (list helper)
            (lispm-agent-entry helper) (list :from (lispm-agent-name proxy) :request-id "R7" :text "Review"))
      (let* ((roster (lispm-agents-roster))
             (cli (seq-find (lambda (record) (eq (plist-get record :buffer) terminal)) roster))
             (child (seq-find (lambda (record) (eq (plist-get record :buffer) (lispm-agent-buffer helper))) roster)))
        (should (equal (lispm-agents-roster-key proxy) '(codex "terminal")))
        (should (equal (plist-get cli :parent) '(codex "parent")))
        (should (equal (plist-get child :parent) '(codex "terminal")))
        (should (equal (plist-get cli :waits-on) (list (lispm-agents-roster-key helper))))
        (should (equal (plist-get child :waited-on-by) '((codex "terminal"))))
        (should (equal (plist-get cli :request-ids) '("R7")))
        (should (equal (plist-get child :request-ids) '("R7")))))))

(ert-deftest lispm-test-roster-fresh-terminal-departure ()
  "Two terminals without known ids can independently arrive and depart."
  (lispm-test-with-peers
    (let ((first (lispm-test--peer 'codex project))
          (second (lispm-test--peer 'codex project)))
      (lispm-test--roster-observer
        (kill-buffer second)
        (should (lispm-test--wait (lambda () changes)))
        (should (memq 'agents (cadar changes)))
        (should (= (length (lispm-agents-roster)) 1))
        (should (eq (plist-get (car (lispm-agents-roster)) :buffer) first))))))

(ert-deftest lispm-test-roster-peer-edges-refresh ()
  "Peer endpoints resolve anew as session ids and previously unknown peers arrive."
  (lispm-test-with-peers
    (let* ((terminal (lispm-test--peer 'codex project))
           (boss (lispm-start 'assistant project))
           (name (lispm-agents-bridge-name terminal))
           (lispm-collab--requests
            (list (list "R8" name #'ignore (list :from (symbol-name (lispm-agent-name boss)) :to name))
                  (list "R9" name #'ignore (list :from "unseen" :to name)))))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "first"))
      (let* ((roster (lispm-agents-roster))
             (caller (seq-find (lambda (record) (eq (plist-get record :buffer) (lispm-agent-buffer boss))) roster))
             (cli (seq-find (lambda (record) (eq (plist-get record :buffer) terminal)) roster)))
        (should (equal (plist-get caller :waits-on) '((codex "first"))))
        (should (equal (plist-get caller :request-ids) '("R8")))
        (should (equal (plist-get cli :waited-on-by) (list (lispm-agents-roster-key boss))))
        (should (equal (plist-get cli :request-ids) '("R8" "R9"))))
      (with-current-buffer terminal (setq lispm-agents-buffer-session-id "second"))
      (with-current-buffer (lispm-test--peer 'codex project)
        (setq lispm-agents-bridge--name "unseen" lispm-agents-buffer-session-id "new"))
      (let* ((roster (lispm-agents-roster))
             (caller (seq-find (lambda (record) (eq (plist-get record :buffer) (lispm-agent-buffer boss))) roster))
             (cli (seq-find (lambda (record) (eq (plist-get record :buffer) terminal)) roster)))
        (should (equal (plist-get caller :waits-on) '((codex "second"))))
        (should (member '(codex "new") (plist-get cli :waited-on-by))))
      (setq lispm-collab--requests nil)
      (should-not (plist-get (lispm-agents-roster-waits (lispm-agent-buffer boss)) :waits-on)))))

(ert-deftest lispm-test-roster-source-hooks ()
  "Appearance, parent, state, character, archive and departure notify observers."
  (lispm-test-env
    (lispm-test--with-agents nil
      (lispm-test--roster-observer
        (let* ((agent (lispm-start 'assistant project))
               (key (lispm-agents-roster-key agent)))
          (should (lispm-test--wait (lambda () changes)))
          (should (member key (caar changes)))
          (setq changes nil)
          (lispm-set-parent agent "parent")
          (lispm-set-state agent 'requesting "working")
          (should (lispm-test--wait (lambda () changes)))
          (should (memq 'parent (cadar changes)))
          (should (memq 'status (cadar changes)))
          (setq changes nil)
          (setf (lispm-agent-character agent) 'ada)
          (lispm-register-character 'ada :title "Ada")
          (should (lispm-test--wait (lambda () changes)))
          (should (memq 'character (cadar changes)))
          (setq changes nil)
          (lispm-agents-toggle-archive-session (list :agent 'lispm :id (lispm-agent-id agent)))
          (should (lispm-test--wait (lambda () changes)))
          (should (member key (caar changes)))
          (should-not (lispm-agents-roster))
          (setq lispm-agents--archived nil changes nil)
          (kill-buffer (lispm-agent-buffer agent))
          (should (lispm-test--wait (lambda () changes)))
          (should (member key (caar changes))))))))

(provide 'lispm-roster-test)
;;; lispm-roster-test.el ends here
