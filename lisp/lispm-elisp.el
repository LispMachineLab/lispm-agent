;;; lispm-elisp.el --- Elisp workers launched by LispM Kernel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; run_elisp asks the optional LispM Kernel to start a fresh batch Emacs.
;; Only code and text cross the process boundary; the user's live Emacs
;; remains the domain of eval_elisp.  Keep the kernel request's id so
;; aborting a turn cancels that worker, never the shared kernel.

;;; Code:

(require 'lispm-job)
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
  agent runtime id done collector handle finished)

(defun lispm-elisp--active-jobs ()
  "Return active `lispm-elisp--job' records managed by `lispm-job'."
  (mapcar #'lispm-job-data (lispm-job-active nil 'elisp)))

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
  (let* ((remote (file-remote-p directory))
         (cwd (directory-file-name (if remote temporary-file-directory directory)))
         (seen (make-hash-table :test #'equal))
         (entries (if remote
                      (list (concat "LISPM_ELISP_ROOT=" (file-name-as-directory directory))
                            (concat "PWD=" cwd))
                    (list (concat "PWD=" cwd)))))
    (puthash "PWD" t seen)
    (puthash "LISPM_ELISP_ROOT" t seen)
    (dolist (entry process-environment)
      (let* ((equals (string-search "=" entry))
             (name (if equals (substring entry 0 equals) entry)))
        (unless (gethash name seen)
          (puthash name t seen)
          (when equals (push entry entries)))))
    (nreverse entries)))

(defun lispm-elisp--stop-kernel (job)
  "Abort JOB's kernel request if one was submitted."
  (setf (lispm-elisp--job-finished job) t)
  (when (lispm-elisp--job-id job)
    (ignore-errors
      (lispm-kernel-cancel (lispm-elisp--job-runtime job) (lispm-elisp--job-id job)))))

(defun lispm-elisp--cancel (job)
  "Cancel JOB alone and ignore any later messages about it."
  (if-let* ((handle (lispm-elisp--job-handle job)))
      (lispm-job-cancel handle)
    (lispm-elisp--stop-kernel job)))

(defun lispm-elisp--result (job kind value)
  "Take one kernel message, KIND and VALUE, for JOB."
  (unless (lispm-elisp--job-finished job)
    (if (eq kind 'chunk)
        (lispm-chunk-collector-push (lispm-elisp--job-collector job) value)
      (when (if-let* ((handle (lispm-elisp--job-handle job)))
                (lispm-job-finish handle)
              t)
        (setf (lispm-elisp--job-finished job) t)
        (let* ((collector (lispm-elisp--job-collector job))
               (output (lispm-chunk-collector-string collector))
               (errors (and (eq kind 'success)
                            (lispm-tools-strip-benign-stderr (plist-get value :stderr))))
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
                                    (and (lispm-chunk-collector-truncated-p collector)
                                         "[Worker output truncated]")
                                    (and errors (not (string-empty-p errors)) errors)))
                    "\n")
                   (not ok)))))))

(defun lispm-elisp--start (agent code timeout done)
  "Run CODE for AGENT in a kernel child for TIMEOUT seconds; call DONE."
  (let* ((timeout (or timeout 120))
         (directory (lispm-principal-root agent))
         (remote (file-remote-p directory))
         (cwd (directory-file-name (if remote temporary-file-directory directory))))
    (unless (and (integerp timeout) (<= 1 timeout 86400))
      (error "timeout must be an integer from 1 to 86400 seconds"))
    (unless (file-directory-p cwd)
      (error "No worker directory: %s" directory))
    (unless (and (file-name-absolute-p lispm-elisp-program)
                 (file-executable-p lispm-elisp-program))
      (error "No Emacs executable for run_elisp: %s" lispm-elisp-program))
    (let* ((runtime (lispm-elisp--runtime))
           (collector (lispm-chunk-collector-create (max 0 lispm-tool-output-limit)))
           (job (lispm-elisp--job-create :agent agent :runtime runtime :done done
                                         :collector collector))
           (print-escape-newlines t)
           (print-escape-control-characters t))
      ;; Keep the id before C-g can unwind a submission that reached
      ;; the kernel.  The error/quit handler can then cancel it.
      (lispm-tools-with-async-start (lispm-elisp--cancel job)
        (setf (lispm-elisp--job-handle job)
              (lispm-job-register agent 'elisp
                                  (lambda () (lispm-elisp--stop-kernel job))
                                  job))
        (setf (lispm-elisp--job-id job)
              (lispm-kernel-submit
               runtime 'exec
               (list :program lispm-elisp-program
                     :args (list "-Q" "--batch" "--load" lispm-elisp--worker
                                 "--funcall" "lispm-elisp-worker-main")
                     :cwd cwd
                     :env (lispm-elisp--environment directory)
                     :stdin (concat (prin1-to-string code) "\n")
                     :timeout timeout)
               :timeout (+ timeout 10)
               :on-result (lambda (kind value) (lispm-elisp--result job kind value)))))))
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
  (lispm-job-cancel-kind 'elisp)
  (remhash "run_elisp" lispm-tools--registry)
  nil)

(provide 'lispm-elisp)
;;; lispm-elisp.el ends here
