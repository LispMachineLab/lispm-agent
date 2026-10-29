;;; lispm-roster-details-test.el --- Roster inspector tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded by lispm-test.el, whose fixtures keep all agent files in
;; scratch directories and git commands outside the user's repository.

;;; Code:

(require 'lispm-agents-roster)

(defmacro lispm-test--roster-details-env (&rest body)
  "Run BODY with agent data and session state in temporary directories."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-agents-roster-change-functions nil)
           (lispm-agents-session-lookup-functions nil)
           (lispm-collab--requests nil))
       (lispm-test--with-agents
           '((lispm :name "LispM" :buffer-p
                    (lambda (buffer) (buffer-local-value 'lispm-session-agent buffer))))
         ,@body))))

(ert-deftest lispm-test-roster-details-work-and-mailbox ()
  "One inspector reads this turn's calls and files, and its full mailbox."
  (lispm-test--roster-details-env
    (let* ((default-directory project)
           (agent (lispm-start))
           (other (lispm-start))
           (handling (list :id "handling" :from "caller" :reply-to "R1"
                           :queued "[2026-10-29 Thu 10:00]" :text (copy-sequence "Handle this")))
           (queued (list :id "queued" :from "sender" :reply-to "R2"
                         :queued "[2026-10-29 Thu 10:01]" :text (copy-sequence "Read this")
                         :callback #'ignore))
           (file (expand-file-name "current.el" project)))
      (setf (lispm-agent-turn agent) 2
            (lispm-agent-entry agent) handling
            (lispm-agent-inbox agent) (list queued)
            (lispm-agent-waiting-on agent) (list other)
            (lispm-agent-journal agent)
            `((1 (old :file ,(expand-file-name "old.el" project)))
              (2 (first :file ,file) (second :file ,file) (scratch :name "scratch"))))
      (lispm-inbox-save agent)
      ;; Metadata can still be known by the durable inbox after an older
      ;; runtime entry lost it.  The running entry's text remains in memory.
      (plist-put queued :queued nil)
      (with-current-buffer (lispm-agent-buffer agent)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "* User\nEarlier\n* Assistant\n")
          (lispm-transcript-insert-blocks (list (lispm-test--tool "old" "read_file" '((path . "old.el"))))
                                          (lambda (_) "DONE"))
          (setq lispm--turn-start (cons 2 (point-marker)))
          (insert "* User\nCurrent\n* Assistant\n")
          (lispm-transcript-insert-blocks (list (lispm-test--tool "done" "read_file" '((path . "current.el"))))
                                          (lambda (_) "DONE"))
          (let ((calls (lispm-transcript-insert-blocks
                        (list (lispm-test--tool "pending" "write_file" '((path . "pending.el"))))
                        (lambda (_) "WAITING"))))
            (puthash "pending" (list :name "write_file" :input '((path . "pending.el"))
                                    :status 'ask :marker (cdar calls))
                     (lispm-agent-pending agent)))
          ;; Events recorded without a turn and an unsent draft must not
          ;; change which turn the inspector's calls belong to.
          (insert "* Event: stopped\nAn event between turns.\n* User\nAn unsent draft.\n")))
      (let* ((key (lispm-agents-roster-key agent))
             (details (lispm-agents-roster-details key))
             (work (plist-get details :work))
             (mailbox (plist-get details :mailbox))
             (pending (car (plist-get work :pending-calls)))
             (mail (car (plist-get mailbox :queued))))
        (should (equal (plist-get details :key) key))
        (should (= (plist-get work :turn) 2))
        (should (equal (plist-get work :files) (list file)))
        (should (equal (mapcar (lambda (call) (plist-get call :id)) (plist-get work :recent-calls))
                       '("done")))
        (should (equal (plist-get pending :id) "pending"))
        (should (equal (plist-get pending :input-summary)
                       (lispm-agents-work-input-summary '((path . "pending.el")))))
        (should (eq (plist-get pending :status) 'ask))
        (should-not (plist-member pending :marker))
        (should (equal (plist-get (plist-get mailbox :handling) :text) "Handle this"))
        (should (equal (plist-get mail :from) "sender"))
        (should (equal (plist-get mail :reply-to) "R2"))
        (should (equal (plist-get mail :queued) "[2026-10-29 Thu 10:01]"))
        (should-not (plist-member mail :callback))
        (should (equal (plist-get details :waits-on) (list (lispm-agents-roster-key other))))
        (should (equal (plist-get (lispm-agents-roster-details (lispm-agents-roster-key other)) :waited-on-by)
                       (list key)))
        ;; Mutating an inspector's strings cannot alter the live agent.
        (aset (plist-get mail :text) 0 ?X)
        (aset (plist-get pending :name) 0 ?X)
        (should (equal (plist-get queued :text) "Read this"))
        (should (equal (plist-get (gethash "pending" (lispm-agent-pending agent)) :name) "write_file"))))))

(ert-deftest lispm-test-roster-details-recent-calls-bounded ()
  "Only the last twenty completed calls of the current turn are returned."
  (lispm-test--roster-details-env
    (let* ((default-directory project)
           (agent (lispm-start)))
      (setf (lispm-agent-turn agent) 1)
      (with-current-buffer (lispm-agent-buffer agent)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq lispm--turn-start (cons 1 (point-marker)))
          (insert "* User\nCurrent\n* Assistant\n")
          (dotimes (n 25)
            (lispm-transcript-insert-blocks
             (list (lispm-test--tool (format "call-%d" n) "read_file" '((path . "file.el"))))
             (lambda (_) "DONE")))))
      (let* ((details (lispm-agents-roster-details (lispm-agents-roster-key agent)))
             (calls (plist-get (plist-get details :work) :recent-calls)))
        (should (= (length calls) 20))
        (should (equal (plist-get (car calls) :id) "call-5"))
        (should (equal (plist-get (car (last calls)) :id) "call-24")))))
  (lispm-test--roster-details-env
    (should-not (lispm-agents-roster-details '(lispm "absent-session")))))

(ert-deftest lispm-test-roster-details-keeps-the-runtime-turn ()
  "A finished turn's calls remain visible through later events and drafts."
  (lispm-test--roster-details-env
    (let* ((default-directory project)
           (agent (lispm-start)))
      (with-temp-file (expand-file-name "file.el" project) (insert "Read me.\n"))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use" (lispm-test--tool "read" "read_file" '((path . "file.el"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Read file.el.")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm--record-event agent "stopped" "A note after the turn.")
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-max))
        (insert "An unsent draft."))
      (let ((work (plist-get (lispm-agents-roster-details (lispm-agents-roster-key agent)) :work)))
        (should (= (plist-get work :turn) 1))
        (should (equal (mapcar (lambda (call) (plist-get call :id)) (plist-get work :recent-calls))
                       '("read")))))))

(ert-deftest lispm-test-roster-details-terminal-is-lazy ()
  "A terminal's work reader runs only for its inspector."
  (lispm-test--roster-details-env
    (let* ((buffer (generate-new-buffer " *roster-details-terminal*"))
           (reads 0)
           (lispm-agents-defined
            `((fake :program "sh" :work-details
                    ,(lambda (_buffer)
                       (cl-incf reads)
                       '(:turn nil :pending-calls nil
                         :recent-calls ((:id "c1" :name "shell" :input-summary "pwd" :status done))
                         :files nil)))))
           (lispm-agents-terminal-modes '(fundamental-mode))
           (lispm-agents-terminal-live-p-function (lambda (candidate) (eq candidate buffer))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq default-directory project
                    lispm-agents-buffer-agent 'fake
                    lispm-agents-buffer-session-id "terminal-session"))
            (lispm-agents-roster)
            (should (= reads 0))
            (cl-letf (((symbol-function 'lispm-agents-bridge-mailbox)
                       (lambda (_buffer) '(:handling nil :queued ((:id "m1" :from "caller" :text "Hello"))))))
              (let* ((details (lispm-agents-roster-details '(fake "terminal-session")))
                     (work (plist-get details :work)))
                (should (= reads 1))
                (should-not (plist-get work :turn))
                (should (equal (plist-get (car (plist-get work :recent-calls)) :name) "shell"))
                (should (equal (plist-get (car (plist-get (plist-get details :mailbox) :queued)) :from)
                               "caller")))))
        (kill-buffer buffer)))))

(provide 'lispm-roster-details-test)
;;; lispm-roster-details-test.el ends here
