;;; lispm-elisp.el --- Elisp workers launched by LispM Kernel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; run_elisp asks the optional LispM Kernel to start a fresh batch Emacs.
;; Only code and text cross the process boundary; the user's live Emacs
;; remains the domain of eval_elisp.  Keep the kernel request's id so
;; aborting a turn cancels that worker, never the shared kernel.

;;; Code:

(require 'lispm-tools)

(declare-function lispm-kernel-live-p "lispm-kernel" (runtime))
(declare-function lispm-kernel-runtime-hello "lispm-kernel" (runtime))
(declare-function lispm-kernel-submit "lispm-kernel" (runtime job input &rest keys))
(declare-function lispm-kernel-cancel "lispm-kernel" (runtime id))
(defvar lispm-kernel-exec-runtime-function)

(defcustom lispm-elisp-program (expand-file-name invocation-name invocation-directory)
  "Emacs executable LispM Kernel starts for run_elisp."
  :type 'file
  :group 'lispm)

(defconst lispm-elisp--worker
  (expand-file-name "lispm-elisp-worker.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Bootstrap loaded by the worker, without the user's init file.")

(cl-defstruct (lispm-elisp--job (:constructor lispm-elisp--job-create) (:copier nil))
  "One agent's kernel request, and the bounded output received so far."
  agent runtime id done chunks (size 0) limit truncated finished)

(defvar lispm-elisp--jobs nil
  "Elisp workers whose kernel requests have not finished.")

(defun lispm-elisp--runtime ()
  "Return a running kernel that accepts exec jobs, or signal an error.
No worker is started in Emacs when the kernel is unavailable."
  (unless (require 'lispm-kernel-exec nil t)
    (error "run_elisp requires LispM Kernel; install and enable it first"))
  (let ((runtime (and lispm-kernel-exec-runtime-function
                      (funcall lispm-kernel-exec-runtime-function))))
    (unless (and runtime (lispm-kernel-live-p runtime)
                 (memq :exec (plist-get (lispm-kernel-runtime-hello runtime) :jobs)))
      (error "run_elisp requires a running LispM Kernel with exec support; enable it and retry"))
    runtime))

(defun lispm-elisp--environment (directory)
  "The worker's environment in DIRECTORY, respecting Emacs's overrides."
  (let ((seen (make-hash-table :test #'equal))
        (entries (list (concat "PWD=" (directory-file-name directory)))))
    (puthash "PWD" t seen)
    (dolist (entry process-environment)
      (let* ((equals (string-search "=" entry))
             (name (if equals (substring entry 0 equals) entry)))
        (unless (gethash name seen)
          (puthash name t seen)
          (when equals (push entry entries)))))
    (nreverse entries)))

(defun lispm-elisp--forget (job)
  "Finish JOB's bookkeeping before calling back or asking it to stop."
  (setf (lispm-elisp--job-finished job) t)
  (setq lispm-elisp--jobs (delq job lispm-elisp--jobs))
  (unless lispm-elisp--jobs
    (remove-hook 'lispm-state-change-functions #'lispm-elisp--state-changed)
    (remove-hook 'lispm-session-end-functions #'lispm-elisp--session-ended)
    (remove-hook 'lispm-agents-context-release-functions #'lispm-elisp--terminal-ended)))

(defun lispm-elisp--cancel (job)
  "Cancel JOB alone and ignore any later messages about it."
  (unless (lispm-elisp--job-finished job)
    (lispm-elisp--forget job)
    (when (lispm-elisp--job-id job)
      (ignore-errors
        (lispm-kernel-cancel (lispm-elisp--job-runtime job) (lispm-elisp--job-id job))))))

(defun lispm-elisp--session-ended (agent)
  "Cancel all workers belonging to AGENT."
  (dolist (job (copy-sequence lispm-elisp--jobs))
    (when (eq agent (lispm-elisp--job-agent job))
      (lispm-elisp--cancel job))))

(defun lispm-elisp--state-changed (agent)
  "Cancel AGENT's workers when its turn is aborted."
  (when (eq (lispm-agent-state agent) 'aborting)
    (lispm-elisp--session-ended agent)))

(defun lispm-elisp--terminal-ended (terminal)
  "Cancel the workers of the external agent whose TERMINAL is closing."
  (when (and (buffer-live-p terminal)
             (local-variable-p 'lispm-agents-bridge--agent terminal))
    (lispm-elisp--session-ended
     (buffer-local-value 'lispm-agents-bridge--agent terminal))))

(defun lispm-elisp--result (job kind value)
  "Take one kernel message, KIND and VALUE, for JOB."
  (unless (lispm-elisp--job-finished job)
    (if (eq kind 'chunk)
        (let ((room (max 0 (- (lispm-elisp--job-limit job) (lispm-elisp--job-size job)))))
          (when (> (length value) room)
            (setf (lispm-elisp--job-truncated job) t))
          (when (> room 0)
            (let ((text (substring value 0 (min room (length value)))))
              (push text (lispm-elisp--job-chunks job))
              (cl-incf (lispm-elisp--job-size job) (length text)))))
      (lispm-elisp--forget job)
      (let* ((output (apply #'concat (nreverse (lispm-elisp--job-chunks job))))
             (errors (and (eq kind 'success) (plist-get value :stderr)))
             (ok (and (eq kind 'success) (eql (plist-get value :exit) 0)))
             (status (unless ok
                       (if (eq kind 'success)
                           (format "Worker %s %s"
                                   (if (plist-member value :signal) "signal" "exit")
                                   (or (plist-get value :signal) (plist-get value :exit)))
                         (format "Kernel worker %s: %s" kind value)))))
        (funcall (lispm-elisp--job-done job)
                 (string-join
                  (delq nil (list status output
                                  (and (lispm-elisp--job-truncated job) "[Worker output truncated]")
                                  (and errors (not (string-empty-p errors)) errors)))
                  "\n")
                 (not ok))))))

(defun lispm-elisp--start (agent code timeout done)
  "Run CODE for AGENT in a kernel child for TIMEOUT seconds; call DONE."
  (let ((timeout (or timeout 120))
        (directory (lispm-agent-root agent)))
    (unless (and (integerp timeout) (<= 1 timeout 86400))
      (error "timeout must be an integer from 1 to 86400 seconds"))
    (when (file-remote-p directory)
      (error "run_elisp runs local workers; it cannot use a remote agent root"))
    (unless (file-directory-p directory)
      (error "No worker directory: %s" directory))
    (unless (and (file-name-absolute-p lispm-elisp-program)
                 (file-executable-p lispm-elisp-program))
      (error "No Emacs executable for run_elisp: %s" lispm-elisp-program))
    (let* ((runtime (lispm-elisp--runtime))
           (job (lispm-elisp--job-create :agent agent :runtime runtime :done done
                                         :limit (max 0 lispm-tool-output-limit)))
           (print-escape-newlines t)
           (print-escape-control-characters t))
      (condition-case err
          (progn
            ;; Keep the id before C-g can unwind a submission that reached
            ;; the kernel.  The error/quit handler can then cancel it.
            (let ((inhibit-quit t))
              (push job lispm-elisp--jobs)
              (add-hook 'lispm-state-change-functions #'lispm-elisp--state-changed)
              (add-hook 'lispm-session-end-functions #'lispm-elisp--session-ended)
              (add-hook 'lispm-agents-context-release-functions #'lispm-elisp--terminal-ended)
              (setf (lispm-elisp--job-id job)
                    (lispm-kernel-submit
                     runtime 'exec
                     (list :program lispm-elisp-program
                           :args (list "-Q" "--batch" "--load" lispm-elisp--worker
                                       "--funcall" "lispm-elisp-worker-main")
                           :cwd (directory-file-name directory)
                           :env (lispm-elisp--environment directory)
                           :stdin (concat (prin1-to-string code) "\n")
                           :timeout timeout)
                     :timeout (+ timeout 10)
                     :on-result (lambda (kind value) (lispm-elisp--result job kind value)))))
            (when quit-flag (signal 'quit nil)))
        ((error quit)
         (let ((inhibit-quit t)) (lispm-elisp--cancel job))
         (signal (car err) (cdr err))))))
  nil)

(lispm-deftool run_elisp
    ((code string "Emacs Lisp forms to run in a fresh kernel-managed worker.")
     (timeout integer "Seconds before the kernel stops the worker (default 120, maximum 86400)."
              :optional t))
  "Run Emacs Lisp in a NEW PROCESS launched by LispM Kernel and return printed
output and the last value. Use this for standalone file discovery, JSON/JSONL
parsing, text filtering and computation, keeping the user's Emacs responsive.
Each call starts a clean batch Emacs with lexical binding in your local root.
It has built-in libraries, but none of the user's live buffers, variables,
loaded packages or agent tools; require any other libraries explicitly.
Use eval_elisp only when you need the live editor, and run_lisp for short
compositions of agent tools. Keep file changes in the dedicated edit tools.
Requires a running LispM Kernel with exec support. A missing or busy kernel
fails the call; there is no local execution fallback. Aborting a native LispM
turn or closing its session or an external agent's terminal cancels the worker.
This is process isolation, not a sandbox."
  :permission exec
  :async t
  :preview (lambda (code &optional _timeout) (lispm-tools-code-writes-note code))
  (lispm-elisp--start (or lispm-current-agent (error "run_elisp needs an agent")) code timeout done))

(defun lispm-elisp-unload-function ()
  "Stop workers before unloading their callbacks."
  (mapc #'lispm-elisp--cancel (copy-sequence lispm-elisp--jobs))
  (remhash "run_elisp" lispm-tools--registry)
  nil)

(provide 'lispm-elisp)
;;; lispm-elisp.el ends here
