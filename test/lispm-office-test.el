;;; lispm-office-test.el --- Built-in Office view tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after `lispm-test.el'.  Covers `lispm-office-scope',
;; `lispm-office-actions', `lispm-office-inspect', and `lispm-office',
;; including default-on dispatch, header tabs, hit-testing, and zero idle cost.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-office)

(setq native-comp-enable-subr-trampolines nil)
(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))

(defmacro lispm-office-test--isolated (&rest body)
  "Run BODY with isolated Office buffers, layout file, and hooks."
  (declare (indent 0))
  `(let ((saved-enabled lispm-office-enabled)
         (saved-tabs lispm-office-show-tabs)
         (lispm-office-layout-file nil)
         (lispm-office-display-function #'ignore)
         (lispm-agents-work-display-function #'ignore)
         (lispm-agents-show-function #'ignore)
         (lispm-agents-root-cache (make-hash-table :test #'equal))
         (lispm-agents-roster-change-functions nil)
         (window-state-change-hook nil))
     (unwind-protect
         (progn
           (setq lispm-office-enabled t
                 lispm-office-show-tabs t)
           (lispm-office--apply-enabled)
           ,@body)
       (dolist (buf (copy-sequence lispm-office--buffers))
         (when (buffer-live-p buf) (kill-buffer buf)))
       (dolist (buf (copy-sequence lispm-office-inspect--buffers))
         (when (buffer-live-p buf) (kill-buffer buf)))
       (when (timerp lispm-office--timer)
         (cancel-timer lispm-office--timer)
         (setq lispm-office--timer nil))
       (setq lispm-office-enabled saved-enabled
             lispm-office-show-tabs saved-tabs)
       (lispm-office--apply-enabled))))

(ert-deftest lispm-office-test-scope-and-identities ()
  "Scope filtering shares work semantics and yields valid scene records."
  (lispm-test-with-peers
    (lispm-office-test--isolated
      (let* ((agent (lispm-start 'assistant project))
             (other (lispm-start 'assistant (expand-file-name "~")))
             (chat (lispm-chat agent))
             (terminal (lispm-test--peer 'mute project))
             (unclaimed (generate-new-buffer "*codex:unclaimed*"))
             (lispm-agents-terminal-modes '(fundamental-mode eat-mode))
             (lispm-agents-terminal-live-p-function #'buffer-live-p))
        (unwind-protect
            (progn
              (with-current-buffer unclaimed
                (setq default-directory project
                      lispm-agents-buffer-agent 'codex))
              (lispm-register-character 'office-bot :title "Office Bot")
              (setf (lispm-agent-character agent) 'office-bot)
              (let* ((scoped (lispm-office-scope-records
                              (directory-file-name project)))
                     (all (lispm-office-scope-records nil))
                     (agent-key (list 'lispm (lispm-agent-id agent)))
                     (other-key (list 'lispm (lispm-agent-id other)))
                     (rec (lispm-office-scope-find-record agent-key scoped)))
                (should rec)
                (should (lispm-ui-scene-key-p (plist-get rec :key)))
                (should (equal (plist-get rec :name) "Office Bot"))
                (should (eq (lispm-office-scope-conversation-buffer agent-key scoped) chat))
                (should-not (lispm-office-scope-find-record other-key scoped))
                (should (lispm-office-scope-find-record other-key all))
                (should (seq-some (lambda (r) (eq (plist-get r :buffer) terminal)) scoped))
                (should (seq-every-p (lambda (r) (lispm-ui-scene-key-p (plist-get r :key)))
                                     scoped))))
          (when (buffer-live-p unclaimed) (kill-buffer unclaimed)))))))

(ert-deftest lispm-office-test-actions-dispatch ()
  "Office action handlers validate targets and delegate cleanly."
  (lispm-test-with-peers
    (lispm-office-test--isolated
      (let* ((agent (lispm-start 'assistant project))
             (key (list 'lispm (lispm-agent-id agent)))
             (terminal (lispm-test--peer 'mute project))
             (tkey (lispm-agents-roster-key terminal))
             (shown nil)
             (told nil)
             (approved nil)
             (aborted nil)
             (inspected nil)
             (pasted nil))
        (cl-letf (((symbol-function 'lispm-agents-show)
                   (lambda (buf) (setq shown buf)))
                  ((symbol-function 'lispm-tell)
                   (lambda (target text &rest _) (setq told (list target text))))
                  ((symbol-function 'lispm-approve-for)
                   (lambda (target &optional _) (setq approved target)))
                  ((symbol-function 'lispm-abort)
                   (lambda (&optional target) (setq aborted target)))
                  ((symbol-function 'lispm-agents-terminal-submit)
                   (lambda (buf text) (setq pasted (list buf text)))))
          (lispm-office-action-dispatch 'open-chat key)
          (should (eq shown (lispm-chat agent)))
          (lispm-office-action-send-message key "Run checks")
          (should (equal told (list agent "Run checks")))
          (lispm-office-action-dispatch 'approve key)
          (should (eq approved agent))
          (lispm-office-action-dispatch 'abort key)
          (should (eq aborted agent))
          (lispm-office-action-dispatch 'inspect key (lambda (k) (setq inspected k)))
          (should (equal inspected key))
          (when tkey
            (lispm-office-action-send-message tkey "Status?")
            (should (equal pasted (list terminal "Status?")))))
        (should-error (lispm-office-action-open-chat '(lispm "missing-id"))
                      :type 'user-error)))))

(ert-deftest lispm-office-test-inspector-tabs-and-lazy-observer ()
  "Inspector renders all four tabs and detaches its observer on buffer kill."
  (lispm-test-env
    (lispm-office-test--isolated
      (let* ((agent (lispm-start 'assistant project))
             (key (list 'lispm (lispm-agent-id agent))))
        (should-not lispm-agents-roster-change-functions)
        (let ((buf (lispm-office-inspect-open key)))
          (should (buffer-live-p buf))
          (should (memq #'lispm-office-inspect--changed
                        lispm-agents-roster-change-functions))
          (with-current-buffer buf
            (should (derived-mode-p 'lispm-office-inspect-mode))
            (should (string-match-p "Kind:[[:space:]]+lispm" (buffer-string)))
            (dolist (spec '((mailbox . "No queued messages")
                            (calls . "No pending tool calls")
                            (activity . "#")
                            (overview . "Session:")))
              (lispm-office-inspect-select-tab (car spec))
              (should (eq lispm-office-inspect--tab (car spec)))
              (should (string-match-p (cdr spec) (buffer-string)))))
          (kill-buffer buf)
          (should-not (memq #'lispm-office-inspect--changed
                            lispm-agents-roster-change-functions)))))))

(ert-deftest lispm-office-test-mode-frame-persistence-and-lifecycle ()
  "Office composes immutable frames, toggles scope, persists, and cleans up."
  (lispm-test-env
    (let* ((dir (make-temp-file "lispm-office-seats-" t))
           (seats-file (expand-file-name "seats.eld" dir)))
      (unwind-protect
          (lispm-office-test--isolated
            (let* ((lispm-office-layout-file seats-file)
                   (agent (lispm-start 'assistant project))
                   (key (list 'lispm (lispm-agent-id agent)))
                   (buf nil))
              (setf (lispm-agent-state agent) 'acting)
              (setq buf (lispm-agents-open-project project))
              (should (buffer-live-p buf))
              (with-current-buffer buf
                (should (derived-mode-p 'lispm-office-mode))
                (should (equal lispm-office-scope (directory-file-name project)))
                (should (equal lispm-agents-work-directory (directory-file-name project)))
                (should (lispm-ui-scene-frame-p lispm-office--frame))
                (should (equal (lispm-ui-scene-hit-test
                                lispm-office--frame
                                (let* ((proj (lispm-ui-scene-frame-projection lispm-office--frame))
                                       (approve (car (lispm-ui-scene-frame-docket-hits
                                                      lispm-office--frame))))
                                  (if approve
                                      (* (plist-get proj :scale)
                                         (- (+ (plist-get approve :x) 2) (plist-get proj :x)))
                                    -1))
                                (let* ((proj (lispm-ui-scene-frame-projection lispm-office--frame))
                                       (approve (car (lispm-ui-scene-frame-docket-hits
                                                      lispm-office--frame))))
                                  (if approve
                                      (* (plist-get proj :scale)
                                         (- (+ (plist-get approve :y) 2) (plist-get proj :y)))
                                    -1)))
                               (when-let* ((first (car (lispm-ui-scene-frame-docket-hits
                                                        lispm-office--frame))))
                                 (list :action (plist-get first :action) :key key))))
                ;; Scope toggles to All Projects and back cleanly even under another default-directory.
                (let ((default-directory temporary-file-directory))
                  (lispm-office-toggle-scope)
                  (should (null lispm-office-scope))
                  (lispm-office-toggle-scope)
                  (should (equal lispm-office-scope (directory-file-name project))))
                (lispm-office-compact))
              (should (file-exists-p seats-file))
              (should (lispm-ui-scene-read seats-file))
              (should (timerp lispm-office--timer))
              (should (memq #'lispm-office--roster-changed
                            lispm-agents-roster-change-functions))
              (kill-buffer buf)
              (should-not lispm-office--buffers)
              (should-not lispm-office--timer)
              (should-not (memq #'lispm-office--roster-changed
                                lispm-agents-roster-change-functions))))
        (delete-directory dir t)))))

(ert-deftest lispm-office-test-default-on-tabs-and-zero-idle-overhead ()
  "Office is enabled by default, attaches chat tabs passively, and toggles off."
  (lispm-test-env
    (lispm-office-test--isolated
      (should (eq (default-value 'lispm-office-enabled) t))
      (let* ((agent (lispm-start 'assistant project))
             (chat (lispm-chat agent)))
        (with-current-buffer chat
          (should (equal header-line-format '(:eval (lispm-office--header-line))))
          (let ((row (lispm-office--header-line)))
            (should (stringp row))
            (should (not (string-empty-p row)))))
        ;; Zero idle overhead while only conversations exist.
        (should-not lispm-agents-roster-change-functions)
        (should-not lispm-office--timer)
        ;; Never clobbers host-owned header-line-format.
        (with-temp-buffer
          (setq-local header-line-format "host-bar")
          (lispm-office--maybe-attach-header)
          (should (equal header-line-format "host-bar")))
        ;; Toggling off restores fallback to native `lispm-agents-work-mode'.
        (let ((lispm-agents-work--buffers nil)
              (opened nil))
          (unwind-protect
              (progn
                (should-not (lispm-office-toggle))
                (setq opened (lispm-agents-open-project project))
                (should (with-current-buffer opened
                          (derived-mode-p 'lispm-agents-work-mode)))
                (should-not (with-current-buffer chat header-line-format))
                (should (lispm-office-toggle))
                (let ((office (lispm-agents-open-project project)))
                  (should (with-current-buffer office
                            (derived-mode-p 'lispm-office-mode)))))
            (when (buffer-live-p opened) (kill-buffer opened))))))))

(provide 'lispm-office-test)
;;; lispm-office-test.el ends here
