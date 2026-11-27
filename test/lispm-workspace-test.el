;;; lispm-workspace-test.el --- Tests for isolated workspaces and reconciliation -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Phase 2 tests (#205):
;;   1. Isolated concurrent agent workspaces (git-worktree) and per-workspace journals.
;;   2. Non-git CoW clone workspaces with non-git turn journals and atomic CAS reconcile.
;;   3. Event-driven journal readiness wakeup without polling and off-hot-path GC.
;;   4. Lock-free 3-way git merge-tree reconciliation with CAS retry and clean main tree.
;;   5. Conflicting merge diagnostics without dirty conflict markers in main checkout.
;;   6. Non-blocking worktree_ask inbox delegation while caller keeps working.
;;   7. Multi-instance MCP server registry discovery and bin/lispm-tool routing.

;;; Code:

(require 'ert)
(require 'lispm-test)
(require 'lispm-workspace)
(require 'lispm-mcp-server)

(defvar lispm-worktree-directory)

(defun lispm-workspace-test--await (fn)
  "Invoke async FN with a callback and wait up to 10s for its single result."
  (let ((done nil)
        (result nil))
    (funcall fn (lambda (&rest args)
                  (setq result args
                        done t)))
    (should (lispm-test--wait (lambda () done) 10))
    result))

(ert-deftest lispm-workspace-test-concurrent-isolated-agents ()
  "N concurrent coding agents get isolated worktrees and independent journals."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (ws-root (expand-file-name "../ws-store/" main))
           (ws1 nil) (ws2 nil) (ws3 nil))
      (lispm-test--write (expand-file-name "shared.txt" main) "base\n")
      (lispm-test--git main "add" "shared.txt")
      (lispm-test--git main "commit" "-q" "-m" "Add shared.txt")
      (pcase-let ((`(,w1 ,e1) (lispm-workspace-test--await
                               (lambda (cb)
                                 (lispm-workspace-create main cb :kind 'git-worktree
                                                         :name "agent1" :directory ws-root))))
                  (`(,w2 ,e2) (lispm-workspace-test--await
                               (lambda (cb)
                                 (lispm-workspace-create main cb :kind 'git-worktree
                                                         :name "agent2" :directory ws-root))))
                  (`(,w3 ,e3) (lispm-workspace-test--await
                               (lambda (cb)
                                 (lispm-workspace-create main cb :kind 'git-worktree
                                                         :name "agent3" :directory ws-root)))))
        (should-not e1) (should-not e2) (should-not e3)
        (setq ws1 w1 ws2 w2 ws3 w3)
        (should-not (equal (lispm-workspace-journal-dir ws1)
                           (lispm-workspace-journal-dir ws2)))
        (should-not (equal (lispm-workspace-journal-dir ws2)
                           (lispm-workspace-journal-dir ws3)))
        (dolist (ws (list ws1 ws2 ws3))
          (should (string-search "/workspaces/" (lispm-workspace-journal-dir ws))))
        (let ((a1 (lispm-start 'assistant (lispm-workspace-effective-root ws1)))
              (a2 (lispm-start 'assistant (lispm-workspace-effective-root ws2)))
              (a3 (lispm-start 'assistant (lispm-workspace-effective-root ws3))))
          (lispm-workspace-attach a1 ws1)
          (lispm-workspace-attach a2 ws2)
          (lispm-workspace-attach a3 ws3)
          (dolist (a (list a1 a2 a3))
            (setf (lispm-agent-policy a) '((write . auto))))
          (setq lispm-test--scripts
                `((,(lispm-agent-name a1)
                   ,(lispm-test--msg "tool_use"
                                     (lispm-test--tool "t1" "write_file"
                                                       '((why . "edit 1") (path . "shared.txt")
                                                         (content . "from agent 1\n"))))
                   ,(lispm-test--msg "end_turn" (lispm-test--text "Agent 1 done.")))
                  (,(lispm-agent-name a2)
                   ,(lispm-test--msg "tool_use"
                                     (lispm-test--tool "t2" "write_file"
                                                       '((why . "edit 2") (path . "shared.txt")
                                                         (content . "from agent 2\n"))))
                   ,(lispm-test--msg "end_turn" (lispm-test--text "Agent 2 done.")))
                  (,(lispm-agent-name a3)
                   ,(lispm-test--msg "tool_use"
                                     (lispm-test--tool "t3" "write_file"
                                                       '((why . "edit 3") (path . "shared.txt")
                                                         (content . "from agent 3\n"))))
                   ,(lispm-test--msg "end_turn" (lispm-test--text "Agent 3 done.")))))
          (lispm-tell a1 "edit shared.txt")
          (lispm-tell a2 "edit shared.txt")
          (lispm-tell a3 "edit shared.txt")
          (should (lispm-test--wait
                   (lambda ()
                     (cl-every (lambda (a)
                                 (and (funcall (lispm-test--idle-p a))
                                      (not (lispm-journal-busy-p a))))
                               (list a1 a2 a3)))
                   15))
          (should (equal (lispm-test--file (lispm-workspace-path ws1) "shared.txt") "from agent 1\n"))
          (should (equal (lispm-test--file (lispm-workspace-path ws2) "shared.txt") "from agent 2\n"))
          (should (equal (lispm-test--file (lispm-workspace-path ws3) "shared.txt") "from agent 3\n"))
          (should (equal (lispm-test--file main "shared.txt") "base\n"))
          (should (equal (lispm-journal--directory-path (lispm-agent-root a1) (lispm-agent-workspace a1))
                         (lispm-workspace-journal-dir ws1)))
          (should (= (length (lispm-journal-records a1)) 1))
          (should (= (length (lispm-journal-records a2)) 1))
          (should (= (length (lispm-journal-records a3)) 1))
          ;; Reverting Agent 1 restores only Agent 1's workspace; A2, A3, and main stay intact.
          (lispm-revert-turn a1)
          (should (equal (lispm-test--file (lispm-workspace-path ws1) "shared.txt") "base\n"))
          (should (equal (lispm-test--file (lispm-workspace-path ws2) "shared.txt") "from agent 2\n"))
          (should (equal (lispm-test--file (lispm-workspace-path ws3) "shared.txt") "from agent 3\n"))
          (should (equal (lispm-test--file main "shared.txt") "base\n"))
          ;; Workspace metadata and isolated journal survive simulated restart.
          (let ((f2 (buffer-file-name (lispm-agent-buffer a2))))
            (lispm-test--crash-all)
            (clrhash lispm-workspace--registry)
            (let ((revived (lispm-resume-file f2)))
              (should (lispm-workspace-get (lispm-workspace-id ws2)))
              (should (= (length (lispm-journal-records revived)) 1)))))))))

(ert-deftest lispm-workspace-test-cow-clone-isolation-and-reconcile ()
  "CoW clone workspaces isolate non-git projects, journals, and CAS reconcile."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let ((plain (file-name-as-directory (expand-file-name "plain-dir/" tmp)))
          (store (file-name-as-directory (expand-file-name "cow-store/" tmp))))
      (make-directory plain t)
      (lispm-test--write (expand-file-name "a.txt" plain) "orig-a\n")
      (lispm-test--write (expand-file-name "b.txt" plain) "orig-b\n")
      (pcase-let ((`(,w1 ,e1) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create plain cb :kind 'cow-clone
                                                                    :name "c1" :directory store))))
                  (`(,w2 ,e2) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create plain cb :kind 'cow-clone
                                                                    :name "c2" :directory store)))))
        (should-not e1) (should-not e2)
        (should (eq (lispm-workspace-kind w1) 'cow-clone))
        ;; Non-git CoW workspace supports isolated turn journal snapshots and reverts.
        (let ((a1 (lispm-start 'assistant (lispm-workspace-effective-root w1))))
          (lispm-workspace-attach a1 w1)
          (setf (lispm-agent-policy a1) '((write . auto)))
          (setq lispm-test--scripts
                `((,(lispm-agent-name a1)
                   ,(lispm-test--msg "tool_use"
                                     (lispm-test--tool "t1" "write_file"
                                                       '((why . "edit a") (path . "a.txt")
                                                         (content . "ws1-a\n"))))
                   ,(lispm-test--msg "end_turn" (lispm-test--text "Edited a.txt.")))))
          (lispm-tell a1 "edit a")
          (should (lispm-test--wait
                   (lambda () (and (funcall (lispm-test--idle-p a1))
                                   (not (lispm-journal-busy-p a1))))
                   10))
          (should (= (length (lispm-journal-records a1)) 1))
          (lispm-revert-turn a1)
          (should (equal (lispm-test--file (lispm-workspace-path w1) "a.txt") "orig-a\n")))
        ;; Direct edit in ws1 + clean CAS reconcile into origin.
        (lispm-test--write (expand-file-name "a.txt" (lispm-workspace-path w1)) "reconciled-a\n")
        (lispm-test--write (expand-file-name "added.txt" (lispm-workspace-path w1)) "new-file\n")
        (should (equal (lispm-test--file plain "a.txt") "orig-a\n"))
        (let ((res1 (car (lispm-workspace-test--await
                          (lambda (cb) (lispm-workspace-reconcile w1 cb))))))
          (should (eq (plist-get res1 :status) 'ok))
          (should (equal (lispm-test--file plain "a.txt") "reconciled-a\n"))
          (should (equal (lispm-test--file plain "added.txt") "new-file\n")))
        ;; Concurrent origin edit causes conflict detection on ws2 without overwriting origin.
        (lispm-test--write (expand-file-name "b.txt" plain) "origin-edited-b\n")
        (lispm-test--write (expand-file-name "b.txt" (lispm-workspace-path w2)) "ws2-edited-b\n")
        (let ((res2 (car (lispm-workspace-test--await
                          (lambda (cb) (lispm-workspace-reconcile w2 cb))))))
          (should (eq (plist-get res2 :status) 'conflict))
          (should (member "b.txt" (plist-get res2 :conflicts)))
          (should (equal (lispm-test--file plain "b.txt") "origin-edited-b\n")))))))

(ert-deftest lispm-workspace-test-journal-event-driven-wakeup ()
  "Waiting for journal readiness uses events instead of polling; GC runs off hot path."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (should-not (string-search "gc --auto" lispm-journal--script))
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (agent (lispm-defagent worker :root main)))
      (setq lispm-test--scripts
            `((worker ,(lispm-test--msg "end_turn" (lispm-test--text "Awake.")))))
      (lispm-session-buffer agent)
      (cl-incf (lispm-agent-turn agent))
      (lispm-in-session agent
        (lispm-session-newline)
        (insert "* User\nhello\n"))
      ;; Force an initial snapshot so journal is busy right when `lispm-turns-request' runs.
      (lispm-journal-begin agent)
      (should (lispm-journal-busy-p agent))
      (let ((wait-val (lispm--journal-wait agent)))
        (should (equal (car-safe wait-val) 'event)))
      (let ((timers-before (copy-sequence timer-list)))
        (should (eq (lispm-turns-request agent) t))
        (should (eq (lispm-agent-state agent) 'requesting))
        (should (eq (lispm-agent-activity agent) 'waiting))
        ;; Event-driven journal wait must not add 0.2s poll timers to `timer-list'.
        (should (= (length timer-list) (length timers-before)))
        (should-not (seq-some (lambda (tm) (eq (timer--function tm) #'lispm-turns-wake-waiting))
                              timer-list)))
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p agent))
                                                (not (lispm-journal-busy-p agent))))
                                10))
      (should (string-search "Awake." (lispm-test--transcript agent)))
      ;; Stale workspace metadata whose directory vanished is pruned by `lispm-workspace-gc'.
      (let ((store (expand-file-name "../gc-store/" main)))
        (pcase-let ((`(,ws ,err) (lispm-workspace-test--await
                                  (lambda (cb) (lispm-workspace-create main cb :kind 'cow-clone
                                                                       :name "gone" :directory store)))))
          (should-not err)
          (delete-directory (lispm-workspace-path ws) t)
          (pcase-let ((`(,pruned ,gerr)
                       (lispm-workspace-test--await
                        (lambda (cb)
                          (lispm-workspace-gc main cb
                                              (list (lispm-workspace-resolve-journal-dir main)))))))
            (should-not gerr)
            (should (>= pruned 1))
            (should-not (lispm-workspace-get (lispm-workspace-id ws)))))))))

(ert-deftest lispm-workspace-test-reconcile-lock-free-and-cas-retry ()
  "Disjoint parallel branches reconcile lock-free with CAS retry and clean main tree."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (store (expand-file-name "../merge-store/" main))
           (head-branch (string-trim (lispm-test--git main "symbolic-ref" "--short" "HEAD"))))
      (pcase-let ((`(,w1 ,e1) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create main cb :kind 'git-worktree
                                                                    :name "left" :directory store))))
                  (`(,w2 ,e2) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create main cb :kind 'git-worktree
                                                                    :name "right" :directory store)))))
        (should-not e1) (should-not e2)
        (lispm-test--write (expand-file-name "left.txt" (lispm-workspace-path w1)) "left\n")
        (lispm-test--git (lispm-workspace-path w1) "add" "left.txt")
        (lispm-test--git (lispm-workspace-path w1) "commit" "-q" "-m" "Left change")
        (lispm-test--write (expand-file-name "right.txt" (lispm-workspace-path w2)) "right\n")
        (lispm-test--git (lispm-workspace-path w2) "add" "right.txt")
        (lispm-test--git (lispm-workspace-path w2) "commit" "-q" "-m" "Right change")
        ;; Keep uncommitted changes in the main checkout; lock-free reconcile must never touch them.
        (lispm-test--write (expand-file-name "README" main) "dirty local edit\n")
        (lispm-test--write (expand-file-name "untracked.txt" main) "keep me\n")
        ;; Launch both reconciliations simultaneously to exercise CAS retry on the same target ref.
        (let (res1 res2)
          (lispm-workspace-reconcile w1 (lambda (r) (setq res1 r)) :target-ref head-branch)
          (lispm-workspace-reconcile w2 (lambda (r) (setq res2 r)) :target-ref head-branch)
          (should (lispm-test--wait (lambda () (and res1 res2)) 15))
          (should (eq (plist-get res1 :status) 'ok))
          (should (eq (plist-get res2 :status) 'ok)))
        (should (equal (string-trim (lispm-test--git main "show" (concat head-branch ":left.txt"))) "left"))
        (should (equal (string-trim (lispm-test--git main "show" (concat head-branch ":right.txt"))) "right"))
        (should (equal (lispm-test--file main "README") "dirty local edit\n"))
        (should (equal (lispm-test--file main "untracked.txt") "keep me\n"))
        (should-not (file-exists-p (expand-file-name "left.txt" main)))
        (let ((noop (car (lispm-workspace-test--await
                          (lambda (cb) (lispm-workspace-reconcile w2 cb :target-ref head-branch))))))
          (should (eq (plist-get noop :status) 'noop)))))))

(ert-deftest lispm-workspace-test-reconcile-conflict-diagnostics ()
  "Conflicting parallel edits report exact conflict paths without dirtying main tree."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (store (expand-file-name "../conflict-store/" main))
           (head-branch (string-trim (lispm-test--git main "symbolic-ref" "--short" "HEAD"))))
      (lispm-test--write (expand-file-name "conflict.txt" main) "line1\nline2\n")
      (lispm-test--git main "add" "conflict.txt")
      (lispm-test--git main "commit" "-q" "-m" "Add conflict.txt")
      (pcase-let ((`(,w1 ,e1) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create main cb :kind 'git-worktree
                                                                    :name "c-one" :directory store))))
                  (`(,w2 ,e2) (lispm-workspace-test--await
                               (lambda (cb) (lispm-workspace-create main cb :kind 'git-worktree
                                                                    :name "c-two" :directory store)))))
        (should-not e1) (should-not e2)
        (lispm-test--write (expand-file-name "conflict.txt" (lispm-workspace-path w1)) "ours-1\nline2\n")
        (lispm-test--git (lispm-workspace-path w1) "add" "conflict.txt")
        (lispm-test--git (lispm-workspace-path w1) "commit" "-q" "-m" "Edit conflict from 1")
        (lispm-test--write (expand-file-name "conflict.txt" (lispm-workspace-path w2)) "theirs-2\nline2\n")
        (lispm-test--git (lispm-workspace-path w2) "add" "conflict.txt")
        (lispm-test--git (lispm-workspace-path w2) "commit" "-q" "-m" "Edit conflict from 2")
        (let* ((r1 (car (lispm-workspace-test--await
                         (lambda (cb) (lispm-workspace-reconcile w1 cb :target-ref head-branch)))))
               (head-after-r1 (string-trim (lispm-test--git main "rev-parse" "HEAD")))
               (status-after-r1 (lispm-test--git main "status" "--porcelain"))
               (r2 (car (lispm-workspace-test--await
                         (lambda (cb) (lispm-workspace-reconcile w2 cb :target-ref head-branch))))))
          (should (eq (plist-get r1 :status) 'ok))
          (should (equal (string-trim (lispm-test--git main "show" (concat head-branch ":conflict.txt")))
                         "ours-1\nline2"))
          (should (eq (plist-get r2 :status) 'conflict))
          (should (member "conflict.txt" (plist-get r2 :conflicts)))
          (should (not (string-empty-p (or (plist-get r2 :messages) ""))))
          ;; Conflicting reconcile leaves HEAD, status, and working tree completely untouched.
          (should (equal (string-trim (lispm-test--git main "rev-parse" "HEAD")) head-after-r1))
          (should (equal (lispm-test--git main "status" "--porcelain") status-after-r1))
          (should (equal (lispm-test--file main "conflict.txt") "line1\nline2\n"))
          (should-not (string-search "<<<<<<" (lispm-test--file main "conflict.txt"))))))))

(ert-deftest lispm-workspace-test-worktree-ask-nonblocking-inbox ()
  "worktree_ask does not block caller, which handles concurrent turns mid-flight."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (lispm-defrole coder "Writes code." :policy '((write . auto)))
    (let* ((project (lispm-test--git-repo (file-truename project)))
           (boss (lispm-defagent boss :root project :policy '((write . auto))))
           (trees (expand-file-name "../nb-worktrees/" project)))
      (should (string-match-p "worktree_ask" (car (lispm-test--call boss "skill" '((name . "worktree"))))))
      (unwind-protect
          (let ((lispm-worktree-directory trees))
            (setq lispm-test--scripts
                  `((boss ,(lispm-test--msg "tool_use"
                                            (lispm-test--tool "t1" "worktree_ask"
                                                              '((why . "bg") (role . "coder")
                                                                (request . "Build feature")
                                                                (name . "feat"))))
                          ,(lispm-test--msg "end_turn" (lispm-test--text "Dispatched feat."))
                          ,(lispm-test--msg "end_turn" (lispm-test--text "Handled concurrent turn."))
                          ,(lispm-test--msg "end_turn" (lispm-test--text "Received feat reply.")))
                    (coder@feat hang)))
            (lispm-tell boss "start feature in worktree")
            (should (lispm-test--wait
                     (lambda ()
                       (and (funcall (lispm-test--idle-p boss))
                            (lispm-get-agent 'coder@feat t)))
                     10))
            (should (string-search "Dispatched feat." (lispm-test--transcript boss)))
            (should-not (lispm-agent-waiting-on boss))
            ;; While coder@feat is mid-turn, boss remains free to run another turn.
            (lispm-tell boss "quick concurrent question")
            (should (lispm-test--wait
                     (lambda ()
                       (and (funcall (lispm-test--idle-p boss))
                            (string-search "Handled concurrent turn." (lispm-test--transcript boss))))
                     10))
            ;; Now let coder@feat finish and commit a change.
            (let ((helper (lispm-get-agent 'coder@feat)))
              (lispm-test--write (expand-file-name "feat.txt" (lispm-agent-root helper)) "done\n")
              (setf (lispm-agent-last-reply helper) "Feature ready.")
              (lispm-turns-finish helper))
            (should (lispm-test--wait
                     (lambda ()
                       (and (funcall (lispm-test--idle-p boss))
                            (string-search "Received feat reply." (lispm-test--transcript boss))))
                     10))
            (should (string-search "coder@feat replied:\nFeature ready." (lispm-test--transcript boss)))
            (should (string-search "feat.txt" (lispm-test--transcript boss))))
        (dolist (name '("worktree_ask" "worktree_list" "worktree_remove" "worktree_reconcile"))
          (remhash name lispm-tools--registry))))))

(ert-deftest lispm-workspace-test-multi-instance-mcp-discovery ()
  "MCP registry selects longest matching root, prunes dead PIDs, and routes lispm-tool."
  (skip-unless (and (executable-find "curl") (file-executable-p lispm-agents-mcp-tool-program)))
  (lispm-test-with-mcp-server
    (let* ((reg (lispm-mcp-server-registry-directory))
           (ws-a (file-name-as-directory (expand-file-name "proj-a/" tmp)))
           (ws-b (file-name-as-directory (expand-file-name "proj-a/nested/" tmp)))
           (stale-file (expand-file-name "999999-deadbeef.server" reg))
           (env-list (list (concat "LISPM_MCP_SERVERS=" (directory-file-name reg))
                           (concat "LISPM_PEERS=" (expand-file-name "no-peers" tmp)))))
      (make-directory ws-a t)
      (make-directory ws-b t)
      (make-directory reg t)
      ;; Stale descriptor from dead PID 999999 + synthetic broader vs narrower live entries.
      (lispm-test--write stale-file (format "http://127.0.0.1:1\nstale\n999999\n%s\n"
                                            (directory-file-name ws-a)))
      (lispm-test--write (expand-file-name (format "%d-broad.server" (emacs-pid)) reg)
                         (format "http://127.0.0.1:1\nwrong\n%d\n%s\n"
                                 (emacs-pid) (directory-file-name ws-a)))
      (lispm-test--write (expand-file-name (format "%d-nested.server" (emacs-pid)) reg)
                         (format "http://127.0.0.1:%d\n%s\n%d\n%s\n"
                                 port token (emacs-pid) (directory-file-name ws-b)))
      (let ((match (lispm-mcp-server-for-directory (expand-file-name "deep/" ws-b))))
        (should (equal (plist-get match :token) token)))
      ;; Stale descriptor is removed by `lispm-mcp-server-prune-stale'.
      (should (>= (lispm-mcp-server-prune-stale) 1))
      (should-not (file-exists-p stale-file))
      ;; bin/lispm-tool routes via LISPM_MCP_SERVERS (no LISPM_MCP_SERVER override).
      (let* ((default-directory ws-b)
             (output (generate-new-buffer " *lispm-tool-reg*"))
             (process-environment
              (append env-list
                      (seq-remove (lambda (e) (string-prefix-p "LISPM_MCP_SERVER=" e))
                                  process-environment)))
             (proc (make-process :name "lispm-tool-reg" :buffer output :noquery t
                                 :connection-type 'pipe :sentinel #'ignore
                                 :command (list lispm-agents-mcp-tool-program))))
        (unwind-protect
            (progn
              (should (lispm-test--wait (lambda () (not (process-live-p proc))) 15))
              (should (= (process-exit-status proc) 0))
              (should (string-search "eval_elisp"
                                     (with-current-buffer output (buffer-string)))))
          (when (process-live-p proc) (delete-process proc))
          (kill-buffer output))))))

(provide 'lispm-workspace-test)
;;; lispm-workspace-test.el ends here
