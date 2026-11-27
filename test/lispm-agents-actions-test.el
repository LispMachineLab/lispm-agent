;;; lispm-agents-actions-test.el --- Tests for contextual code-region actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests covering `lispm-agents-actions'.

;;; Code:

(require 'ert)
(require 'lispm-agents)
(require 'lispm-agents-actions)

(ert-deftest lispm-agents-actions-test-constructor-invariants ()
  "Smart constructor enforces non-empty symbol and string slots."
  (let ((act (lispm-agents-make-code-action
              :id 'explain
              :label "Explain"
              :prompt "Explain:"
              :command 'lispm-agents-explain-region
              :key "e")))
    (should (lispm-agents-code-action-p act))
    (should (eq (lispm-agents-code-action-id act) 'explain)))
  (should-error
   (lispm-agents-make-code-action
    :id nil :label "Explain" :prompt "Explain:" :command 'ignore)
   :type 'lispm-agents-action-error)
  (should-error
   (lispm-agents-make-code-action
    :id 'explain :label "" :prompt "Explain:" :command 'ignore)
   :type 'lispm-agents-action-error))

(ert-deftest lispm-agents-actions-test-compose-and-extract-preserve-state ()
  "Region extraction preserves point and narrowing and rejects blank regions."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; header\n(defun demo (x)\n  (1+ x))\n")
    (goto-char 12)
    (let* ((beg (line-beginning-position))
           (end (point-max))
           (saved-pt (point))
           (ref (lispm-agents-extract-region-reference (current-buffer) beg end))
           (prompt (lispm-agents-compose-region-prompt "Explain:" ref)))
      (should (= (point) saved-pt))
      (should (string-match-p ":2-3\n```emacs-lisp\n(defun demo" ref))
      (should (string-prefix-p "Explain:\n\n" prompt)))
    (should-error
     (lispm-agents-extract-region-reference (current-buffer) 1 1)
     :type 'lispm-agents-empty-region-error)))

(ert-deftest lispm-agents-actions-test-context-menu-gating ()
  "Context menu items appear only on non-empty selections in user code buffers."
  ;; Hidden space-prefixed buffers (`with-temp-buffer') are excluded.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun hidden () 1)\n")
    (let ((hidden-menu (make-sparse-keymap)))
      (lispm-agents-populate-region-menu hidden-menu (current-buffer) 1 (point-max))
      (should-not (lookup-key hidden-menu [lispm-agents-explain]))))
  ;; User code buffers receive all 4 verbs + Send Region to Agent when selected.
  (let ((buf (generate-new-buffer "alpha.el")))
    (unwind-protect
        (with-current-buffer buf
          (emacs-lisp-mode)
          (insert "(defun alpha () 42)\n")
          (let ((empty-menu (make-sparse-keymap)))
            (deactivate-mark)
            (lispm-agents-populate-region-menu empty-menu buf)
            (should-not (lookup-key empty-menu [lispm-agents-explain])))
          (let ((menu (make-sparse-keymap)))
            (lispm-agents-populate-region-menu menu buf 1 (point-max))
            (should (commandp (lookup-key menu [lispm-agents-explain])))
            (should (commandp (lookup-key menu [lispm-agents-refactor])))
            (should (commandp (lookup-key menu [lispm-agents-implement])))
            (should (commandp (lookup-key menu [lispm-agents-fix])))
            (should (commandp (lookup-key menu [lispm-agents-stage-region])))))
      (kill-buffer buf))))

(ert-deftest lispm-agents-actions-test-keymap-bindings-and-idempotency ()
  "Keymap bindings preserve existing keys, menu population is idempotent, and dead buffers fail safely."
  (should (eq (keymap-lookup lispm-agents-map "e") #'lispm-agents-explain-region))
  (should (eq (keymap-lookup lispm-agents-map "R") #'lispm-agents-refactor-region))
  (should (eq (keymap-lookup lispm-agents-map "i") #'lispm-agents-implement-region))
  (should (eq (keymap-lookup lispm-agents-map "f") #'lispm-agents-fix-region))
  (should (eq (keymap-lookup lispm-agents-map "r") #'lispm-agents-send-region))
  (should (eq (keymap-lookup lispm-agents-map "s") #'lispm-agents-sessions))
  (let ((buf (generate-new-buffer "beta.el")))
    (unwind-protect
        (with-current-buffer buf
          (emacs-lisp-mode)
          (insert "(defun beta () 1)\n")
          (let ((menu (make-sparse-keymap)))
            (lispm-agents-populate-region-menu menu buf 1 (point-max))
            (let ((before (copy-tree menu)))
              (lispm-agents-populate-region-menu menu buf 1 (point-max))
              (should (equal menu before)))))
      (kill-buffer buf))
    (should-not (buffer-live-p buf))
    (let ((dead-menu (make-sparse-keymap)))
      (lispm-agents-populate-region-menu dead-menu buf 1 5)
      (should-not (lookup-key dead-menu [lispm-agents-explain])))
    (should-error
     (lispm-agents-act-on-region 'explain 1 5 buf)
     :type 'lispm-agents-action-error)
    (should-error
     (lispm-agents-act-on-region 'unknown-verb 1 5 buf)
     :type 'lispm-agents-action-error))
  (let ((was (and (boundp 'lispm-agents-actions-mode) lispm-agents-actions-mode)))
    (unwind-protect
        (progn
          (lispm-agents-actions-mode 1)
          (should (memq #'lispm-agents-context-menu context-menu-functions))
          (lispm-agents-actions-mode -1)
          (should-not (memq #'lispm-agents-context-menu context-menu-functions)))
      (lispm-agents-actions-mode (if was 1 -1)))))

(ert-deftest lispm-agents-actions-test-dispatch-starts-or-reuses-project-agent ()
  "Acting on a region prepares host view, starts a local agent when needed, and submits."
  (let* ((proj-a (file-name-as-directory (make-temp-file "lispm-act-a-" t)))
         (proj-b (file-name-as-directory (make-temp-file "lispm-act-b-" t)))
         (other-buf (generate-new-buffer "*other-proj-agent*"))
         (created-buf nil)
         (prepared nil)
         (displayed nil)
         (submitted nil)
         (staged nil))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory proj-a
                buffer-file-name (expand-file-name "demo.el" proj-a))
          (emacs-lisp-mode)
          (insert "(defun greet (name)\n  ;; TODO\n  name)\n")
          (with-current-buffer other-buf
            (setq default-directory proj-b))
          (let ((lispm-agents-region-action-prepare-function
                 (lambda (src) (push (buffer-name src) prepared)))
                (lispm-agents-region-action-display-function
                 (lambda (agent-buf src-buf pos)
                   (setq displayed (list agent-buf src-buf pos)))))
            (cl-letf (((symbol-function 'lispm-agents-project-directory)
                       (lambda () proj-a))
                      ((symbol-function 'lispm-agents-live-buffers)
                       (lambda ()
                         (delq nil (list other-buf created-buf))))
                      ((symbol-function 'lispm-agents-available)
                       (lambda () '((mock :name "Mock"))))
                      ((symbol-function 'lispm-agents-new)
                       (lambda (_agent dir)
                         (setq created-buf (generate-new-buffer "*mock-agent*"))
                         (with-current-buffer created-buf
                           (setq default-directory dir))
                         created-buf))
                      ((symbol-function 'lispm-agents-of-buffer)
                       (lambda (_buf) 'mock))
                      ((symbol-function 'lispm-agents-props)
                       (lambda (_agent)
                         (list :submit (lambda (target text)
                                         (setq submitted (list target text)))
                               :send (lambda (target text)
                                       (setq staged (list target text))))))
                      ((symbol-function 'lispm-agents-buffer-status)
                       (lambda (_buf) '(:state idle))))
              ;; First call: starts a new agent in proj-a (ignores proj-b's agent).
              (lispm-agents-act-on-region 'implement 1 (point-max) (current-buffer))
              (should (buffer-live-p created-buf))
              (should (eq (car displayed) created-buf))
              (should (eq (nth 1 displayed) (current-buffer)))
              (should (= (nth 2 displayed) 1))
              (should (eq (car submitted) created-buf))
              (should (string-search "demo.el:1-3" (nth 1 submitted)))
              ;; Second call: reuses created-buf without creating another buffer.
              (let ((first-created created-buf))
                (lispm-agents-stage-region 1 (point-max) (current-buffer))
                (should (eq created-buf first-created))
                (should (eq (car staged) first-created))
                (should (string-search "(defun greet" (nth 1 staged)))))))
      (set-buffer-modified-p nil)
      (when (buffer-live-p other-buf) (kill-buffer other-buf))
      (when (buffer-live-p created-buf) (kill-buffer created-buf))
      (delete-directory proj-a t)
      (delete-directory proj-b t))))

(provide 'lispm-agents-actions-test)
;;; lispm-agents-actions-test.el ends here
