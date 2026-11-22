;;; lispm-shell-stage-test.el --- Tests for shell and Git stage views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests witnessing shell/Git command classification, porcelain status
;; and unified diff views, ANSI colour rendering, clickable `FILE:LINE:'
;; presentations, fast-probe debouncing, and disk-revert diff animations.

;;; Code:

(require 'ert)
(require 'lispm)
(require 'lispm-test)

(defmacro lispm-shell-stage-test--with-env (&rest body)
  "Run BODY in `lispm-test-env' with edits, spotlights and shell stage active."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-sandbox-level 'none)
           (lispm-edits--in-batch t)
           (lispm-edits--shows nil)
           (lispm-edits--proposals nil)
           (lispm-edits--writing (make-hash-table :test #'equal))
           (lispm-spotlight--in-batch t)
           (lispm-spotlight--beams nil)
           (lispm-spotlight--steps (make-hash-table :test #'eq))
           (lispm-spotlight--ephemeral-buffers nil)
           (lispm-spotlight--scroll-lock nil)
           (lispm-shell-stage--in-batch t)
           (lispm-shell-stage--sessions (make-hash-table :test #'eq)))
       (unwind-protect
           (progn ,@body)
         (maphash (lambda (agent _s) (lispm-shell-stage--retire-session agent))
                  (copy-hash-table lispm-shell-stage--sessions))
         (when (timerp lispm-shell-stage--ticker)
           (cancel-timer lispm-shell-stage--ticker)
           (setq lispm-shell-stage--ticker nil))
         (lispm-edits--tick 1e10)
         (lispm-spotlight--tick 1e10)))))

(ert-deftest lispm-shell-stage-test-pure-classifiers-and-parsers ()
  "Command classifiers unwrap wrappers and parse Git status/diff outputs."
  (let ((s1 (lispm-shell-stage--classify-shell
             "cd /tmp/repo && /opt/homebrew/bin/timeout 5 git -C sub diff HEAD~1"))
        (s2 (lispm-shell-stage--classify-program "gh" ["pr" "diff" "12"]))
        (s3 (lispm-shell-stage--classify-shell "pwd"))
        (s4 (lispm-shell-stage--classify-shell "pwd | cat")))
    (should (equal (lispm-shell-stage-spec-git-subcommand s1) "diff"))
    (should-not (lispm-shell-stage-spec-probe-p s1))
    (should (equal (lispm-shell-stage-spec-git-subcommand s2) "diff"))
    (should (lispm-shell-stage-spec-probe-p s3))
    (should-not (lispm-shell-stage-spec-probe-p s4)))
  (should (equal (lispm-shell-stage--diff-stats
                  "diff --git a/a.el b/a.el\n--- a/a.el\n+++ b/a.el\n@@ -1,2 +1,3 @@\n-old\n+new1\n+new2\n")
                 '(1 2 1)))
  (let ((st (lispm-shell-stage--parse-porcelain-status
             "## main...origin/main\nM  staged.el\n M mod.el\n?? new.el\n")))
    (should (equal (plist-get st :branch) "main...origin/main"))
    (should (equal (mapcar (lambda (i) (cons (plist-get i :kind) (plist-get i :path)))
                           (plist-get st :items))
                   '((staged . "staged.el")
                     (modified . "mod.el")
                     (untracked . "new.el"))))))

(ert-deftest lispm-shell-stage-test-git-status-and-diff-views ()
  "Git status and diff commands render semantic views with file presentations."
  (lispm-shell-stage-test--with-env
    (let ((file (expand-file-name "mod.el" project))
          (agent (lispm-defagent coder "Coder." :root project)))
      (lispm-test--write file "(defun mod () 1)\n")
      (call-process "git" nil nil nil "-C" project "init" "-q")
      (call-process "git" nil nil nil "-C" project "config" "user.email" "t@example.com")
      (call-process "git" nil nil nil "-C" project "config" "user.name" "Test")
      (call-process "git" nil nil nil "-C" project "add" "mod.el")
      (call-process "git" nil nil nil "-C" project "commit" "-q" "-m" "init")
      (lispm-test--write file "(defun mod () 2)\n")
      (let ((chat (lispm-chat agent)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer chat)
          ;; 1. `git status -sb' via `run_program' renders branch + clickable file.
          (lispm-test--call-async agent "run_program"
                                  '((program . "git") (args . ["status" "-sb"])))
          (let ((stage-buf (lispm-shell-stage--buffer agent)))
            (should (get-buffer-window stage-buf))
            (with-current-buffer stage-buf
              (should-not display-line-numbers)
              (should (string-search "mod.el" (buffer-string)))
              (goto-char (point-min))
              (should (search-forward "mod.el" nil t))
              (should (equal (get-text-property (match-beginning 0) 'lispm-object)
                             (list 'file file nil)))))
          ;; 2. `git diff' via `run_shell' renders summary and diff faces.
          (lispm-test--call-async agent "run_shell" '((command . "git diff")))
          (let ((stage-buf (lispm-shell-stage--buffer agent)))
            (with-current-buffer stage-buf
              (should (string-search "+1 -1" (buffer-string)))
              (goto-char (point-min))
              (should (search-forward "+(defun mod () 2)" nil t))
              (should (memq 'diff-added
                            (ensure-list (get-text-property (match-beginning 0) 'face))))
              (should (string-search "✓" lispm-shell-stage--header)))))))))

(ert-deftest lispm-shell-stage-test-ansi-links-and-probe-debounce ()
  "ANSI output colours, `FILE:LINE:' links work, and probes do not flicker."
  (lispm-shell-stage-test--with-env
    (let ((file (expand-file-name "src.el" project))
          (agent (lispm-defagent runner "Runner." :root project)))
      (lispm-test--write file "line-1\nline-2\nline-3\n")
      (let ((chat (lispm-chat agent)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer chat)
          ;; Show `src.el' on stage first via `read_file'.
          (lispm-test--call agent "read_file" '((path . "src.el")))
          (let ((src-buf (find-buffer-visiting file)))
            (should (get-buffer-window src-buf))
            ;; Fast `pwd' probe succeeds without replacing `src.el' on stage.
            (let ((lispm-shell-stage-probe-delay 2.0))
              (lispm-test--call-async agent "run_shell" '((command . "pwd")))
              (should (get-buffer-window src-buf)))
            ;; Real build/check command with ANSI escape and `src.el:2:' replaces stage.
            (lispm-test--call-async
             agent "run_shell"
             '((command . "printf '\\033[31msrc.el:2: broken\\033[0m\\n'; exit 2")))
            (let ((stage-buf (lispm-shell-stage--buffer agent)))
              (should (get-buffer-window stage-buf))
              (with-current-buffer stage-buf
                (should-not (string-search "\033[" (buffer-string)))
                (should (string-search "✗" lispm-shell-stage--header))
                (goto-char (point-min))
                (forward-line 1)
                (should (search-forward "src.el:2" nil t))
                (should (equal (get-text-property (match-beginning 0) 'lispm-object)
                               (list 'file file 2)))))))))))

(ert-deftest lispm-shell-stage-test-disk-revert-triggers-edits ()
  "When a shell command modifies an open file on disk, `lispm-edits' shows the diff."
  (lispm-shell-stage-test--with-env
    (let* ((file (expand-file-name "disk.txt" project))
           (agent (lispm-defagent sh-editor "Shell editor." :root project)))
      (lispm-test--write file "before\n")
      (let ((buf (find-file-noselect file))
            (chat (lispm-chat agent)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer chat)
          (set-file-times file (time-subtract (current-time) 2))
          (with-current-buffer buf (revert-buffer t t t))
          (lispm-test--call-async agent "run_shell"
                                  '((command . "printf 'after\\n' > disk.txt")))
          ;; Buffer reverted and animated on stage instead of overwritten by shell log.
          (should (equal (with-current-buffer buf (buffer-string)) "after\n"))
          (should (get-buffer-window buf))
          (should (lispm-edits-active-p buf)))))))

(provide 'lispm-shell-stage-test)
;;; lispm-shell-stage-test.el ends here
