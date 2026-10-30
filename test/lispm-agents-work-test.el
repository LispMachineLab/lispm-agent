;;; lispm-agents-work-test.el --- Project work view tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after lispm-test.el's fixtures.  Exercise the real live roster
;; as well as supplied event snapshots, without a renderer or host.

;;; Code:

(require 'lispm-agents-work)

(defmacro lispm-test--work-views (&rest body)
  "Run BODY with isolated native views and remove them afterwards."
  (declare (indent 0))
  `(let ((lispm-agents-work--buffers nil)
         (lispm-agents-roster-change-functions nil)
         (lispm-language-changed-hook nil)
         (window-state-change-hook nil)
         (lispm-agents-open-project-functions nil)
         (lispm-agents-work-display-function #'ignore)
         (lispm-agents--root-cache (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (dolist (buffer (copy-sequence lispm-agents-work--buffers))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun lispm-test--work-record (id root &optional parent)
  "Return one alive test record with ID, ROOT and optional PARENT key."
  (list :key (and id (list 'lispm id)) :root root :parent parent
        :name (or id "Starting") :alive t :status '(:state idle)))

(ert-deftest lispm-test-work-project-and-exact-directory ()
  "Project scope shares the sidebar's real Git and worktree roots."
  (let* ((base (make-temp-file "lispm-work-" t))
         (main (expand-file-name "project" base))
         (sub (expand-file-name "src" main))
         (worktree (expand-file-name "worktree" base))
         (other (expand-file-name "project-other" base))
         (lispm-agents--root-cache (make-hash-table :test #'equal))
         (lispm-agents-defined nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git/worktrees/branch" main) t)
          (make-directory sub t)
          (make-directory worktree t)
          (make-directory other t)
          (write-region (format "gitdir: %s/.git/worktrees/branch\n" main)
                        nil (expand-file-name ".git" worktree) nil 'silent)
          (let* ((a (lispm-test--work-record "a" main))
                 (b (lispm-test--work-record "b" sub))
                 (c (lispm-test--work-record "c" worktree))
                 (d (lispm-test--work-record "d" other))
                 (records (list a b c d))
                 (before (copy-tree records)))
            (cl-letf (((symbol-function 'lispm-agents-roster)
                       (lambda () (ert-fail "Filtering reread the roster")))
                      ((symbol-function 'lispm-agents-sessions-list)
                       (lambda (&rest _) (ert-fail "Filtering scanned sessions"))))
              (should (equal (lispm-agents-work-filter records main) (list a b c)))
              (should (equal (lispm-agents-work-filter records sub) (list a b c)))
              (should (equal (lispm-agents-work-filter records worktree) (list a b c)))
              (should (equal (lispm-agents-work-filter records (concat main "/") 'directory)
                             (list a)))
              (should (equal (lispm-agents-work-filter records sub 'directory) (list b)))
              (should (equal records before)))))
      (delete-directory base t))))

(ert-deftest lispm-test-work-live-descendants-and-starting-agents ()
  "Idle, done and nil-key starting agents remain visible, with live descendants."
  (let* ((root "/tmp/lispm-work-project")
         (other "/tmp/lispm-work-other")
         (parent (lispm-test--work-record "parent" root))
         (child (lispm-test--work-record "child" other '(lispm "parent")))
         (grandchild (lispm-test--work-record "grandchild" other '(codex "child")))
         (starting (lispm-test--work-record nil root))
         (other-starting (lispm-test--work-record nil other))
         (ended (lispm-test--work-record "ended" root))
         (orphan (lispm-test--work-record "orphan" other '(lispm "missing"))))
    (plist-put child :key '(codex "child"))
    (plist-put child :status '(:state done))
    (plist-put ended :alive nil)
    (cl-letf (((symbol-function 'lispm-agents-project-root) #'identity))
      (let ((records (list grandchild child starting other-starting ended orphan parent)))
        (should (equal (lispm-agents-work-filter records root)
                       (list grandchild child starting parent)))
        (should (equal (lispm-agents-work-filter records other 'directory)
                       (list grandchild child other-starting orphan)))
        (should-not (plist-get starting :key))
        (should-error (lispm-agents-work-filter records root 'invalid))))))

(ert-deftest lispm-test-work-cycle-and-chats ()
  "Cycles cannot hide records and the home folder represents shared Chats."
  (let* ((home (expand-file-name "~"))
         (chat (expand-file-name "agent-chats/example" temporary-file-directory))
         (lispm-agents-defined `((example :chats (,(file-name-directory chat)))))
         (lispm-agents--root-cache (make-hash-table :test #'equal))
         (a (lispm-test--work-record "a" home '(lispm "b")))
         (b (lispm-test--work-record "b" "/tmp/elsewhere" '(lispm "a")))
         (c (lispm-test--work-record "c" chat)))
    (should (equal (lispm-agents-work-filter (list a b c) "~") (list a b c)))
    (should (equal (lispm-agents-work-filter (list a b c) home 'directory) (list a)))))

(ert-deftest lispm-test-work-long-descendant-chain ()
  "A large roster follows parent projects without consuming evaluator depth."
  (let (records)
    (dotimes (index 500)
      (push (lispm-test--work-record
             (number-to-string index)
             (if (zerop index) "/tmp/root-project" "/tmp/child-project")
             (unless (zerop index) (list 'lispm (number-to-string (1- index)))))
            records))
    (cl-letf (((symbol-function 'lispm-agents-project-root) #'identity))
      (should (equal (lispm-agents-work-filter records "/tmp/root-project") records)))))

(ert-deftest lispm-test-work-renderer-chain ()
  "An optional renderer handles only its request and first success wins."
  (lispm-test--work-views
    (let* ((called nil)
          (lispm-agents-open-project-functions
           (list (lambda (directory) (push directory called) nil)
                 (lambda (directory) (push directory called) 'handled)
                 (lambda (_) (ert-fail "Renderer after first success")))))
      (should (eq (lispm-agents-open-project "/tmp/example/") 'handled))
      (should (equal called '("/tmp/example" "/tmp/example")))
      (should-not lispm-agents-work--buffers)
      (should-not lispm-agents-roster-change-functions))))

(ert-deftest lispm-test-work-native-real-roster ()
  "The no-renderer command lists real idle and starting agents without scans."
  (lispm-test-with-peers
    (lispm-test--work-views
      (let* ((agent (lispm-start 'assistant project))
             (terminal (lispm-test--peer 'mute project))
             (shown nil)
             (lispm-agents-work-display-function (lambda (buffer) (setq shown buffer))))
        (cl-letf (((symbol-function 'lispm-agents-sessions-list)
                   (lambda (&rest _) (ert-fail "Opening a project scanned sessions")))
                  ((symbol-function 'lispm-agents-resume)
                   (lambda (&rest _) (ert-fail "Opening a project resumed a session"))))
          (let ((view (lispm-agents-open-project project)))
            (should (eq view shown))
            (with-current-buffer view
              (should (derived-mode-p 'lispm-agents-work-mode))
              (should (equal lispm-agents-work-directory (directory-file-name project)))
              (should (= (length tabulated-list-entries) 2))
              (should (assq terminal tabulated-list-entries))
              (should (assq (lispm-agent-buffer agent) tabulated-list-entries)))
            (should (eq view (lispm-agents-open-project project)))))))))

(ert-deftest lispm-test-work-observer-lifecycle ()
  "Views share observations, skip hidden redraws, and detach on close or mode change."
  (lispm-test--work-views
    (let ((reads 0) (visible nil))
      (cl-letf (((symbol-function 'lispm-agents-roster) (lambda () (cl-incf reads) nil))
                ((symbol-function 'get-buffer-window)
                 (lambda (buffer &optional frames)
                   (should (eq frames 'visible)) (memq buffer visible))))
        (let ((one (lispm-agents-open-project "/tmp/project-one"))
              (two (lispm-agents-open-project "/tmp/project-two")))
          (should (= reads 2))
          (should (= (length lispm-agents-roster-change-functions) 1))
          (lispm-agents-work--changed nil nil nil)
          (should (= reads 2))
          (setq visible (list one two))
          (run-hooks 'window-state-change-hook)
          (should (= reads 3))
          (kill-buffer one)
          (should lispm-agents-roster-change-functions)
          (with-current-buffer two (fundamental-mode))
          (should-not lispm-agents-work--buffers)
          (should-not lispm-agents-roster-change-functions)
          (should-not window-state-change-hook)
          (kill-buffer two))))))

(ert-deftest lispm-test-work-open-rechecks-membership-and-keeps-selection ()
  "Changing row order retains selection; stale rows cannot revive an archived agent."
  (lispm-test--work-views
    (let* ((a (generate-new-buffer " *work-a*"))
           (b (generate-new-buffer " *work-b*"))
           (ra (append (lispm-test--work-record "a" "/tmp/work") (list :buffer a)))
           (rb (append (lispm-test--work-record "b" "/tmp/work") (list :buffer b)))
           (records (list ra rb)) shown)
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-agents-roster) (lambda () records))
                    ((symbol-function 'lispm-agents-show) (lambda (buffer) (setq shown buffer))))
            (with-current-buffer (lispm-agents-open-project "/tmp/work")
              (goto-char (point-min))
              (should (eq (tabulated-list-get-id) a))
              (setq records (list rb ra))
              (lispm-agents-work-refresh)
              (should (eq (tabulated-list-get-id) a))
              (lispm-agents-work-open)
              (should (eq shown a))
              (setq records (list rb) shown nil)
              (should-error (lispm-agents-work-open) :type 'user-error)
              (should-not shown)
              (should (= (length tabulated-list-entries) 1))))
        (kill-buffer a)
        (kill-buffer b)))))

(provide 'lispm-agents-work-test)
;;; lispm-agents-work-test.el ends here
