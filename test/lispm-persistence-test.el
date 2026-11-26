;;; lispm-persistence-test.el --- Tests for Phase 1 partitioned persistence and CAS  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests verifying `#204' lock-free persistence invariants:
;;   - partitioned Maildir-style atomic mailboxes (in-process & cross-process)
;;   - legacy `inbox.org' one-way migration and read-only Org projection
;;   - zero-write `lispm-memory-recall' and SHA-256 CAS memory/task saves
;;   - non-blocking Git archival shutdown, partitioned repos, and lock backoff
;;   - per-agent MVCC/CAS `write_file' / `edit_file' conflict detection
;;   - scoped buffer reverts and `(buffer, agent, call-id)' edit isolation

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm)
(require 'lispm-test)

(defun lispm-persistence-test--spawn-inbox-writer (tag home project-dir count)
  "Spawn a batch Emacs child writing COUNT messages tagged TAG into SHARED-SESS."
  (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
         (timeout-bin (executable-find "timeout"))
         (load-args (cl-loop for dir in load-path append (list "-L" dir)))
         (script (prin1-to-string
                  `(progn
                     (require 'lispm)
                     (setq lispm-directory ,home)
                     (dotimes (i ,count)
                       (lispm-inbox-enqueue
                        ,project-dir "shared-sess" "worker"
                        (list :id (format "%s-%03d" ,tag i)
                              :from ,tag
                              :text (format "msg %s %d" ,tag i)))))))
         (cmd (append (and timeout-bin (list timeout-bin "25"))
                      (list emacs-bin "-Q" "--batch")
                      load-args
                      (list "--eval" script)))
         (buf (generate-new-buffer (format " *lispm-writer-%s*" tag))))
    (make-process :name (format "writer-%s" tag)
                  :buffer buf
                  :noquery t
                  :command cmd)))

(ert-deftest lispm-persistence-test-inbox-multi-writer-cross-process ()
  "Concurrent in-process and cross-process inbox writers lose zero messages."
  (lispm-test-env
    (let* ((saved-save lispm-save-sessions)
           (lispm-save-sessions t)
           (agent (lispm-defagent standing :root project))
           (_ (lispm-chat agent))
           (sid "shared-sess"))
      (ignore saved-save)
      ;; Seed one entry owned by `standing' in its own session, then save alongside
      ;; concurrent external writers targeting BOTH `shared-sess' and `standing'.
      (setf (lispm-agent-inbox agent)
            (list (list :id "owned-1" :from "user" :text "owned msg")))
      (lispm-inbox-save agent)
      (let* ((p1 (lispm-persistence-test--spawn-inbox-writer "proc-a" lispm-directory project 20))
             (p2 (lispm-persistence-test--spawn-inbox-writer "proc-b" lispm-directory project 20)))
        (unwind-protect
            (progn
              (dotimes (i 20)
                (lispm-inbox-enqueue project sid "worker"
                                     (list :id (format "parent-%03d" i)
                                           :from "parent"
                                           :text (format "msg parent %d" i)))
                (when (zerop (mod i 5))
                  (lispm-inbox-save agent)))
              ;; Also write an unowned external entry directly for `standing'.
              (lispm-inbox-enqueue project (lispm-agent-id agent) "standing"
                                   (list :id "ext-1" :from "external" :text "do not clobber"))
              ;; Saving `standing' after an external enqueue must never delete `ext-1'.
              (lispm-inbox-save agent)
              (should (lispm-test--wait
                       (lambda ()
                         (and (not (process-live-p p1))
                              (not (process-live-p p2))))
                       20))
              (should (= (process-exit-status p1) 0))
              (should (= (process-exit-status p2) 0))
              (let* ((all (lispm-inbox-entries project))
                     (shared (seq-filter (lambda (e) (equal (plist-get e :session) sid)) all))
                     (shared-ids (mapcar (lambda (e) (plist-get e :id)) shared)))
                (should (= (length shared) 60))
                (should (= (length (delete-dups (copy-sequence shared-ids))) 60))
                (should (seq-find (lambda (e) (equal (plist-get e :id) "owned-1")) all))
                (should (seq-find (lambda (e) (equal (plist-get e :id) "ext-1")) all))
                (should-not (file-exists-p (lispm-inbox-file project)))))
          (dolist (p (list p1 p2))
            (when (process-live-p p) (delete-process p))
            (when (buffer-live-p (process-buffer p))
              (kill-buffer (process-buffer p)))))))))

(ert-deftest lispm-persistence-test-inbox-legacy-migration-and-export ()
  "Legacy `inbox.org' migrates one-way without data loss and exports read-only."
  (lispm-test-env
    (let* ((legacy (lispm-inbox-file project))
           (migrated (concat legacy ".migrated")))
      (make-directory (file-name-directory legacy) t)
      (with-temp-file legacy
        (insert (format "#+title: Inbox of %s\n#+todo: QUEUED RUNNING | DONE\n"
                        (abbreviate-file-name project))
                "* RUNNING User\n:PROPERTIES:\n:ENTRY:    i-run\n:AGENT:    coder\n"
                ":SESSION:  s-1\n:QUEUED:   [2026-11-26 Thu 10:00]\n:END:\n"
                "* SUSPENDED User\n:PROPERTIES:\n:ENTRY:    i-susp\n:AGENT:    coder\n"
                ":SESSION:  s-1\n:AWAITING: [\"sub-1\"]\n:VALUE:    [42]\n"
                ":QUEUED:   [2026-11-26 Thu 10:01]\n:END:\nsuspended body\n"
                "* QUEUED Message from boss\n:PROPERTIES:\n:ENTRY:    i-q2\n"
                ":AGENT:    reviewer\n:SESSION:  s-2\n:FROM:     boss\n"
                ":QUEUED:   [2026-11-26 Thu 10:02]\n:END:\nplease review\n"))
      (let ((entries (lispm-inbox-entries project)))
        (should (= (length entries) 3))
        (should-not (file-exists-p legacy))
        (should (file-exists-p migrated))
        (let ((susp (seq-find (lambda (e) (equal (plist-get e :id) "i-susp")) entries))
              (q2 (seq-find (lambda (e) (equal (plist-get e :id) "i-q2")) entries)))
          (should (eq (plist-get susp :state) 'suspended))
          (should (equal (plist-get susp :awaiting) '("sub-1")))
          (should (equal (plist-get susp :value) [42]))
          (should (equal (plist-get q2 :from) "boss"))
          (should (equal (plist-get q2 :text) "please review")))
        (let ((exported (lispm-inbox-export-org project)))
          (should (string-match-p "^\\* RUNNING User" exported))
          (should (string-match-p "please review" exported))
          (should-not (file-exists-p legacy)))
        (should (= (length (lispm-inbox-sessions)) 2))))))

(ert-deftest lispm-persistence-test-memory-recall-zero-disk-writes-and-cas ()
  "Memory recall never writes disk; CAS protects memory updates against concurrent writers."
  (lispm-test-env
    (let* ((agent (lispm-defagent keeper :root project))
           (_ (lispm-test--call agent "remember"
                                '((title . "Lockfree") (content . "Zero shared locks."))))
           (file (lispm-memory-file agent))
           (buf (find-buffer-visiting file))
           (before-text (lispm-file-text file))
           (before-sha (lispm-file-sha256 file)))
      (dotimes (_ 3)
        (should (string-match-p "Zero shared locks\\."
                                (car (lispm-test--call agent "recall" '((query . "locks")))))))
      ;; Recall performed 0 disk writes and left the file byte-for-byte intact.
      (should (equal (lispm-file-text file) before-text))
      (should (equal (lispm-file-sha256 file) before-sha))
      (should-not (buffer-modified-p buf))
      ;; Overlaid telemetry reflects all 3 recalls and folds on next explicit write.
      (let ((e (car (lispm-memory-entries buf))))
        (should (= (plist-get e :recalls) 3)))
      (lispm-test--call agent "remember"
                        '((title . "Lockfree") (content . "Zero shared locks v2.")))
      (should (string-match-p ":RECALLS: +3" (lispm-file-text file)))
      ;; CAS conflict on external modification restores buffer cleanly without clobbering disk.
      (let ((cur-buf (lispm-memory-buffer agent 'project nil t))
            (external-text "#+title: External\n* External fact\n:PROPERTIES:\n:CREATED:  [2026-11-26 Thu 11:00]\n:END:\nWinner\n"))
        (should-error
         (lispm-memory-commit-buffer
          cur-buf
          (lambda ()
            (goto-char (point-max))
            (insert "* Stale loser\n")
            (lispm-write-file-atomic file external-text))))
        (should (equal (lispm-file-text file) external-text))
        (should-not (buffer-modified-p cur-buf))))))

(ert-deftest lispm-persistence-test-git-nonblocking-shutdown-and-partitioned-scope ()
  "Shutdown does not wait on slow Git processes; partitioned repos avoid index.lock."
  (lispm-test-env
    ;; 1. Non-blocking kill-emacs hook while a slow process and timer are active.
    (let* ((lispm-git (executable-find "git"))
           (started (float-time))
           (slow (make-process :name "lispm-git" :buffer nil :noquery t
                               :command '("sleep" "15"))))
      (setq lispm-git--process slow
            lispm-git--timer (run-at-time 5 nil #'ignore)
            lispm-git--queue '(("tester" "queued-on-exit")))
      (unwind-protect
          (progn
            (lispm-git--on-kill-emacs)
            (should (< (- (float-time) started) 0.25))
            (should-not lispm-git--timer))
        (when (process-live-p slow) (delete-process slow))
        (setq lispm-git--process nil lispm-git--queue nil)))
    ;; 2. Partitioned Git archival commits each project in its own repo & backs off index.lock.
    (when-let* ((git (executable-find "git")))
      (let* ((lispm-git git)
             (lispm-git-scope 'partitioned)
             (lispm-git-debounce 0.05)
             (dir-a (expand-file-name "proj-a/" project))
             (dir-b (expand-file-name "proj-b/" project))
             (_ (progn (make-directory dir-a t) (make-directory dir-b t)))
             (agent-a (lispm-defagent keeper-a :root dir-a))
             (agent-b (lispm-defagent keeper-b :root dir-b))
             (repo-a (lispm-project-directory dir-a))
             (repo-b (lispm-project-directory dir-b)))
        (lispm-test--call agent-a "remember" '((title . "A") (content . "Alpha")))
        (lispm-test--call agent-b "remember" '((title . "B") (content . "Beta")))
        (should (lispm-test--wait #'lispm-git-settled-p 10))
        (should (file-directory-p (expand-file-name ".git" repo-a)))
        (should (file-directory-p (expand-file-name ".git" repo-b)))
        (should-not (file-exists-p (expand-file-name ".git" lispm-directory)))
        ;; Transient `.git/index.lock' triggers retry and still commits once cleared.
        (let ((lock (expand-file-name ".git/index.lock" repo-a)))
          (with-temp-file lock (insert "held"))
          (lispm-test--call agent-a "remember" '((title . "A2") (content . "Locked first")))
          (accept-process-output nil 0.15)
          (delete-file lock)
          (should (lispm-test--wait #'lispm-git-settled-p 10))
          (let ((default-directory repo-a))
            (should (string-match-p "A2" (shell-command-to-string "git log -1 --format=%s")))))))))

(ert-deftest lispm-persistence-test-mvcc-cas-write-file-and-edit-file ()
  "Concurrent `write_file' / `edit_file' rejects stale writers via MVCC/CAS."
  (lispm-test-env
    (let* ((file (expand-file-name "shared.txt" project))
           (_ (lispm-test--write file "v1\n"))
           (alpha (lispm-defagent alpha :root project :policy '((read . auto) (write . auto))))
           (beta (lispm-defagent beta :root project :policy '((read . auto) (write . auto)))))
      ;; Both agents observe v1.
      (should-not (cdr (lispm-test--call alpha "read_file" '((path . "shared.txt")))))
      (should-not (cdr (lispm-test--call beta "read_file" '((path . "shared.txt")))))
      ;; Alpha wins the race; Beta must get a CAS conflict without clobbering Alpha.
      (should-not (cdr (lispm-test--call alpha "write_file"
                                         '((path . "shared.txt") (content . "v2 alpha\n")))))
      (let ((loser (lispm-test--call beta "write_file"
                                     '((path . "shared.txt") (content . "v2 beta\n")))))
        (should (cdr loser))
        (should (string-match-p "CAS conflict" (car loser)))
        (should (equal (lispm-test--file project "shared.txt") "v2 alpha\n")))
      ;; Re-reading refreshes Beta's observed revision so retry succeeds.
      (should-not (cdr (lispm-test--call beta "read_file" '((path . "shared.txt")))))
      (should-not (cdr (lispm-test--call beta "write_file"
                                         '((path . "shared.txt") (content . "v3 beta\n")))))
      (should (equal (lispm-test--file project "shared.txt") "v3 beta\n"))
      ;; Explicit expected_hash precondition on write_file and edit_file.
      (let* ((good-hash (lispm-tools-content-hash "v3 beta\n"))
             (bad-write (lispm-test--call alpha "write_file"
                                          '((path . "shared.txt") (content . "nope\n")
                                            (expected_hash . "missing"))))
             (bad-edit (lispm-test--call alpha "edit_file"
                                         '((path . "shared.txt") (old_text . "v3")
                                           (new_text . "v4") (expected_hash . "deadbeef"))))
             (good-edit (lispm-test--call alpha "edit_file"
                                          `((path . "shared.txt") (old_text . "v3")
                                            (new_text . "v4") (expected_hash . ,good-hash)))))
        (should (cdr bad-write))
        (should (string-match-p "CAS conflict" (car bad-write)))
        (should (cdr bad-edit))
        (should (string-match-p "CAS conflict" (car bad-edit)))
        (should-not (cdr good-edit))
        (should (equal (lispm-test--file project "shared.txt") "v4 beta\n")))
      ;; External deletion after read triggers CAS conflict until re-read observes missing.
      (should-not (cdr (lispm-test--call alpha "read_file" '((path . "shared.txt")))))
      (delete-file file)
      (let ((stale-del (lispm-test--call alpha "write_file"
                                         '((path . "shared.txt") (content . "clobber\n")))))
        (should (cdr stale-del))
        (should (string-match-p "deleted since you last read it" (car stale-del)))
        (should-not (file-exists-p file)))
      (let ((missing-read (lispm-test--call alpha "read_file" '((path . "shared.txt")))))
        (should (cdr missing-read))
        (should (string-match-p "No such file" (car missing-read))))
      (should-not (cdr (lispm-test--call alpha "write_file"
                                         '((path . "shared.txt") (content . "recreated\n")))))
      (should (equal (lispm-test--file project "shared.txt") "recreated\n"))
      ;; Distinguish an existing 0-byte file from :missing.
      (lispm-test--write (expand-file-name "empty.txt" project) "")
      (let ((empty-clash (lispm-test--call alpha "write_file"
                                           '((path . "empty.txt") (content . "x\n")
                                             (expected_hash . "missing")))))
        (should (cdr empty-clash))
        (should (string-match-p "CAS conflict" (car empty-clash))))
      (should (cdr (lispm-test--call alpha "read_file" '((path . "race.txt")))))
      (lispm-test--write (expand-file-name "race.txt" project) "")
      (let ((race-clash (lispm-test--call alpha "write_file"
                                          '((path . "race.txt") (content . "from-alpha\n")))))
        (should (cdr race-clash))
        (should (string-match-p "created since you last checked it" (car race-clash)))
        (should (equal (lispm-test--file project "race.txt") "")))
      ;; Rejected `write_file' leaves neither parent directory nor visiting buffer behind.
      (let* ((orphan (expand-file-name "no-orphan/x.txt" project))
             (rejected (lispm-test--call alpha "write_file"
                                         '((path . "no-orphan/x.txt") (content . "x\n")
                                           (expected_hash . "deadbeef")))))
        (should (cdr rejected))
        (should (string-match-p "CAS conflict" (car rejected)))
        (should-not (file-exists-p (expand-file-name "no-orphan" project)))
        (should-not (find-buffer-visiting orphan))))))

(ert-deftest lispm-persistence-test-scoped-revert-and-isolated-edit-overlays ()
  "Sibling agent proposals prevent unrelated buffer reverts and isolate by `(buffer, agent, id)'."
  (lispm-test-with-edits
    (let* ((file-a (expand-file-name "a.txt" project))
           (file-b (expand-file-name "b.txt" project))
           (_ (lispm-test--write file-a "a1\n"))
           (_ (lispm-test--write file-b "b1\n"))
           (buf-b (find-file-noselect file-b))
           (agent-a (lispm-defagent agent-a :root project :policy '((write . auto) (shell . auto))))
           (agent-b (lispm-defagent agent-b :root project :policy '((write . ask)))))
      ;; Agent B proposes an edit on b.txt (awaiting approval).
      (setq lispm-test--scripts
            `((agent-b ,(lispm-test--msg "tool_use"
                                         (lispm-test--tool "shared-call-id" "edit_file"
                                                           '((path . "b.txt") (old_text . "b1")
                                                             (new_text . "b2"))))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Proposed.")))))
      (lispm-tell agent-b "propose b")
      (should (lispm-test--wait (lambda () (lispm-edits--proposal "shared-call-id" agent-b))))
      ;; Agent A writes a.txt AND modifies b.txt on disk via shell; buf-b must stay un-reverted.
      (lispm-test--call agent-a "write_file" '((path . "a.txt") (content . "a2\n")))
      (lispm-test--call-async agent-a "run_shell" '((command . "printf 'b-ext\\n' > b.txt")))
      (should (equal (with-current-buffer buf-b (buffer-string)) "b1\n"))
      (should (lispm-edits--proposal "shared-call-id" agent-b))
      (lispm-resolve agent-b "shared-call-id" 'deny "stop")
      (should (lispm-test--wait (lispm-test--idle-p agent-b)))
      ;; Two agents streaming the exact same call ID on distinct files keep isolated state.
      (lispm-test--write (expand-file-name "f1.txt" project) "orig-1\n")
      (lispm-test--write (expand-file-name "f2.txt" project) "orig-2\n")
      (let ((fake-a (lispm-defagent s-a :root project))
            (fake-b (lispm-defagent s-b :root project)))
        (setf (lispm-agent-state fake-a) 'requesting
              (lispm-agent-state fake-b) 'requesting)
        (lispm-edits--input fake-a '(:id "c1" :name "write_file" :json "{\"path\": \"f1.txt\", \"content\": \"sa\""))
        (lispm-edits--input fake-b '(:id "c1" :name "write_file" :json "{\"path\": \"f2.txt\", \"content\": \"sb\""))
        (should (= (hash-table-count lispm-edits--writing) 2))
        (lispm-edits--written (cons fake-a "c1"))
        (lispm-edits--written (cons fake-b "c1"))
        (should (lispm-edits--proposal "c1" fake-a))
        (should (lispm-edits--proposal "c1" fake-b))
        (setf (lispm-agent-state fake-a) 'idle
              (lispm-agent-state fake-b) 'idle)
        (lispm-edits--update fake-a)
        (lispm-edits--update fake-b)))))

(provide 'lispm-persistence-test)
;;; lispm-persistence-test.el ends here
