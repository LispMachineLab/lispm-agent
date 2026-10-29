;;; lispm-mcp-lifecycle-test.el --- Standing MCP ownership tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'lispm-test)
(require 'lispm-process-tools)
(require 'lispm-elisp)

(defmacro lispm-mcp-lifecycle-test--env (&rest body)
  "Run BODY with isolated sessions, descriptor and async tool ownership."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-mcp-server-file (expand-file-name "mcp-server" tmp))
           (lispm-mcp-server--owned-file nil)
           (lispm-mcp-server--process nil)
           (lispm-mcp-server--token nil)
           (lispm-mcp-server--sessions (make-hash-table :test #'equal))
           (lispm-process-tools--runs nil)
           (lispm-elisp--jobs nil)
           (lispm-sandbox-level 'none)
           (lispm-state-change-functions (copy-sequence lispm-state-change-functions))
           (lispm-session-end-functions (copy-sequence lispm-session-end-functions))
           (lispm-agents-context-release-functions
            (copy-sequence lispm-agents-context-release-functions))
           (kill-emacs-hook (copy-sequence kill-emacs-hook)))
       (unwind-protect (progn ,@body)
         (lispm-mcp-server--stop)
         (mapc #'lispm-process-tools--cleanup (copy-sequence lispm-process-tools--runs))
         (mapc #'lispm-elisp--cancel (copy-sequence lispm-elisp--jobs))))))

(defun lispm-mcp-lifecycle-test--session (directory &optional id)
  "Keep a session with an independent proxy agent rooted in DIRECTORY."
  (let* ((id (or id "outside"))
         (agent (lispm-agent--create :name 'outside :id id :root directory :tools t))
         (session (lispm-mcp-server--session-make
                   :id id :name "outside" :kind "Outside" :directory directory
                   :agent agent :used 0)))
    (lispm-mcp-server--add id session)))

(defun lispm-mcp-lifecycle-test--end (how session directory)
  "End SESSION by HOW, using DIRECTORY for a replacement on eviction."
  (pcase how
    ('delete
     (should (equal (lispm-mcp-server--respond
                     "DELETE" "/mcp" `(("mcp-session-id" . ,(lispm-mcp-server-session-id session))) "")
                    '("200 OK"))))
    ('evict
     (let ((lispm-mcp-server-max-sessions 1))
       (lispm-mcp-lifecycle-test--session directory "replacement")))
    ('stop (lispm-mcp-server--stop))))

(ert-deftest lispm-mcp-lifecycle-test-close-cancels-workers ()
  "DELETE, eviction and stop cancel processes and the session's kernel job."
  (dolist (how '(delete evict stop))
    (lispm-mcp-lifecycle-test--env
      (let* ((session (lispm-mcp-lifecycle-test--session project))
             (agent (lispm-mcp-server-session-agent session))
             cancelled results)
        (cl-letf (((symbol-function 'lispm-elisp--runtime) (lambda () 'kernel))
                  ((symbol-function 'lispm-kernel-submit) (lambda (&rest _) 42))
                  ((symbol-function 'lispm-kernel-cancel)
                   (lambda (runtime id) (push (list runtime id) cancelled))))
          (lispm-process-tools--start agent (executable-find "sleep") ["30"] nil 60 #'ignore)
          (lispm-elisp--start agent "(sleep-for 30)" 60 #'ignore)
          (let* ((run (car lispm-process-tools--runs))
                 (job (car lispm-elisp--jobs))
                 (answer (lispm-agents-bridge-server-answer
                          session (lambda (&rest result) (push result results)))))
            (should (process-live-p (lispm-process-tools--run-process run)))
            (lispm-mcp-lifecycle-test--end how session project)
            (should (lispm-mcp-server-session-closed session))
            (should-not (gethash "outside" lispm-mcp-server--sessions))
            (should-not (process-live-p (lispm-process-tools--run-process run)))
            (should-not (process-live-p (lispm-process-tools--run-errors run)))
            (should-not (memq (lispm-process-tools--run-timer run) timer-list))
            (should-not lispm-process-tools--runs)
            (should-not lispm-elisp--jobs)
            (should (lispm-elisp--job-finished job))
            (should (equal cancelled '((kernel 42))))
            ;; Late results and another close cannot revive the caller.
            (funcall answer '("late") nil)
            (lispm-mcp-server--close "outside" session)
            (should (equal results '((("Cancelled: the session has ended.") t))))
            (should-not (lispm-mcp-server-session-pending session))))))))

(ert-deftest lispm-mcp-lifecycle-test-close-cancels-shell ()
  "Closing a proxy also cancels run_shell's native agent-tagged process."
  (lispm-mcp-lifecycle-test--env
    (let* ((session (lispm-mcp-lifecycle-test--session project))
           (agent (lispm-mcp-server-session-agent session))
           (lispm-current-agent agent)
           results)
      (lispm-tool-invoke
       (lispm-get-tool "run_shell") '((command . "exec sleep 30"))
       (let ((answer (lispm-agents-bridge-server-answer
                      session (lambda (&rest result) (push result results)))))
         (lambda (text errorp) (funcall answer (list text) errorp))))
      (let ((process (seq-find (lambda (p) (eq (process-get p 'lispm-agent) agent)) (process-list))))
        (should (process-live-p process))
        (lispm-mcp-lifecycle-test--end 'delete session project)
        (should-not (process-live-p process))
        (should-not (memq (process-get process 'timer) timer-list))
        (should (equal results '((("Cancelled: the session has ended.") t))))))))

(ert-deftest lispm-mcp-lifecycle-test-approval-stages ()
  "A close before the prompt, during preview or during confirmation never runs."
  (dolist (stage '(queued preview confirmation))
    (lispm-mcp-lifecycle-test--env
      (let* ((session (lispm-mcp-lifecycle-test--session project))
             (prompts 0) (executions 0) results
             (lispm-agents-confirm-function
              (lambda (&rest _)
                (cl-incf prompts)
                (when (eq stage 'confirmation)
                  (lispm-mcp-lifecycle-test--end 'delete session project))
                t))
             (lispm-agents-context-answer (lambda (&rest result) (push result results))))
        (cl-letf (((symbol-function 'lispm-tool-invoke) (lambda (&rest _) (cl-incf executions)))
                  ((symbol-function 'lispm-agents-bridge--detail)
                   (lambda (&rest _)
                     (when (eq stage 'preview)
                       (lispm-mcp-lifecycle-test--end 'delete session project))
                     "Preview")))
          (lispm-agents-bridge--tool-call session '((name . "eval_elisp") (input . ((code . "(+ 1 2)")))) 1)
          (when (eq stage 'queued) (lispm-mcp-lifecycle-test--end 'delete session project))
          (sleep-for 0.03)
          (should (= prompts (if (eq stage 'confirmation) 1 0)))
          (should (= executions 0))
          (should (equal results '((("Cancelled: the session has ended.") t))))
          (should-not (lispm-mcp-server-session-pending session)))))))

(ert-deftest lispm-mcp-lifecycle-test-invalid-call-does-not-pend ()
  "A rejected call leaves no callback for a later DELETE to answer again."
  (lispm-mcp-lifecycle-test--env
    (let ((session (lispm-mcp-lifecycle-test--session project))
          (lispm-agents-context-answer #'ignore))
      (dolist (arguments '(((name . "does_not_exist"))
                          ((name . "run_lisp"))
                          ((name . "eval_elisp") (input . 3))))
        (should-error (lispm-agents-bridge--tool-call session arguments 1))
        (should-not (lispm-mcp-server-session-pending session))))))

(ert-deftest lispm-mcp-lifecycle-test-search-close ()
  "Deferred discovery is answered on close, without applying a late result."
  (lispm-mcp-lifecycle-test--env
    (let ((session (lispm-mcp-lifecycle-test--session project))
          prepared results
          (discoveries 0))
      (let ((lispm-agents-context-answer (lambda (&rest result) (push result results))))
        (cl-letf (((symbol-function 'lispm-tools--prepare)
                   (lambda (_sources done) (setq prepared done)))
                  ((symbol-function 'lispm-tools--discover)
                   (lambda (&rest _) (cl-incf discoveries) nil)))
          (lispm-agents-bridge--tool-search session '((query . "buffers")) 1)
          (lispm-mcp-lifecycle-test--end 'delete session project)
          (funcall prepared nil)
          (should (= discoveries 0))
          (should (equal results '((("Cancelled: the session has ended.") t)))))))))

(ert-deftest lispm-mcp-lifecycle-test-cleanup-errors ()
  "A failing extension or disconnected answer cannot strand other cleanup."
  (lispm-mcp-lifecycle-test--env
    (let* ((first (lispm-mcp-lifecycle-test--session project "first"))
           (second (lispm-mcp-lifecycle-test--session project "second"))
           (ended nil) (answers 0)
           (lispm-session-end-functions
            (list (lambda (_) (error "Extension failed"))
                  (lambda (agent) (push agent ended)))))
      (dolist (session (list first second))
        (lispm-agents-bridge-server-answer session (lambda (&rest _) (cl-incf answers)))
        (lispm-agents-bridge-server-answer session (lambda (&rest _) (error "Connection gone"))))
      (lispm-mcp-server--stop)
      (should (= (length ended) 2))
      (should (= answers 2))
      (should (= (hash-table-count lispm-mcp-server--sessions) 0)))))

(ert-deftest lispm-mcp-lifecycle-test-publish-and-failed-start ()
  "Descriptor replacement is atomic and private; failed startup releases ownership."
  (lispm-mcp-lifecycle-test--env
    (let ((old "http://127.0.0.1:1\nstale\n0\n")
          (rename (symbol-function 'rename-file))
          (announced nil))
      (with-temp-file lispm-mcp-server-file (insert old))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional replace)
                   (should (equal (with-temp-buffer (insert-file-contents to) (buffer-string)) old))
                   (should (= (file-modes from) #o600))
                   (should (= (length (with-temp-buffer
                                       (insert-file-contents from)
                                       (split-string (buffer-string) "\n" t))) 3))
                   (setq announced t)
                   (funcall rename from to replace))))
        (should (lispm-mcp-server--start)))
      (should announced)
      (lispm-mcp-server--stop)
      (cl-letf (((symbol-function 'lispm-mcp-server--announce)
                 (lambda () (error "Publication failed"))))
        (should-error (lispm-mcp-server--start)))
      (should-not lispm-mcp-server--process)
      (should-not lispm-mcp-server--owned-file)
      (should-not (file-locked-p (concat lispm-mcp-server-file ".owner")))
      (should (lispm-mcp-server--start)))))

(defun lispm-mcp-lifecycle-test--child (directory file index)
  "Start a batch Emacs INDEX competing for FILE, synchronized in DIRECTORY."
  (let* ((script (expand-file-name (format "child-%d.el" index) directory))
         (ready (expand-file-name (format "ready-%d" index) directory))
         (result (expand-file-name (format "result-%d" index) directory))
         (gate (expand-file-name "go" directory))
         (release (expand-file-name "release" directory)))
    (with-temp-file script
      (prin1 `(progn
                (setq load-path ',load-path)
                (require 'lispm)
                (setq lispm-mcp-server-file ,file create-lockfiles nil)
                (with-temp-file ,ready (insert "ready"))
                (while (not (file-exists-p ,gate)) (sleep-for 0.01))
                (unwind-protect
                    (progn
                      (let ((started (lispm-mcp-server--start)))
                        (with-temp-file ,result (prin1 started (current-buffer))))
                      (while (not (file-exists-p ,release)) (sleep-for 0.01)))
                  (lispm-mcp-server--stop)))
             (current-buffer)))
    (make-process :name (format "lispm-owner-test-%d" index) :noquery t
                  :command (list (expand-file-name invocation-name invocation-directory)
                                 "-Q" "--batch" "-l" script)
                  :buffer (get-buffer-create (format " *lispm-owner-test-%d*" index)))))

(ert-deftest lispm-mcp-lifecycle-test-concurrent-and-stale-owner ()
  "Two simultaneous Emacsen elect one owner; a crashed owner's lock is recovered."
  (lispm-mcp-lifecycle-test--env
    (let ((children nil))
      (unwind-protect
          (progn
            (dotimes (index 2)
              (push (lispm-mcp-lifecycle-test--child tmp lispm-mcp-server-file index) children))
            (should (lispm-test--wait
                     (lambda () (and (file-exists-p (expand-file-name "ready-0" tmp))
                                     (file-exists-p (expand-file-name "ready-1" tmp)))) 15))
            (with-temp-file (expand-file-name "go" tmp) (insert "go"))
            (should (lispm-test--wait
                     (lambda () (and (file-exists-p (expand-file-name "result-0" tmp))
                                     (file-exists-p (expand-file-name "result-1" tmp)))) 15))
            (let ((results (mapcar (lambda (index)
                                    (with-temp-buffer
                                      (insert-file-contents (expand-file-name (format "result-%d" index) tmp))
                                      (read (current-buffer)))) '(0 1))))
              (should (= (cl-count t results) 1))
              (should (= (cl-count nil results) 1)))
            (let* ((descriptor (lispm-mcp-server--read-file))
                   (pid (string-to-number (nth 2 descriptor)))
                   (owner (seq-find (lambda (process) (= (process-id process) pid)) children)))
              (should (= (length descriptor) 3))
              (should owner)
              (should (= (file-modes lispm-mcp-server-file) #o600))
              ;; SIGKILL leaves both the native Emacs lock and descriptor stale.
              (signal-process pid 9)
              (should (lispm-test--wait (lambda () (not (process-live-p owner)))))
              (should (lispm-mcp-server--start))
              (should (= (string-to-number (nth 2 (lispm-mcp-server--read-file))) (emacs-pid))))
            ;; The losing contender never releases this Emacs's ownership.
            (with-temp-file (expand-file-name "release" tmp) (insert "release"))
            (should (lispm-test--wait (lambda () (not (seq-some #'process-live-p children)))))
            (should (equal (nth 1 (lispm-mcp-server--read-file)) lispm-mcp-server--token)))
        (dolist (process children)
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process))))))))

(provide 'lispm-mcp-lifecycle-test)
;;; lispm-mcp-lifecycle-test.el ends here
