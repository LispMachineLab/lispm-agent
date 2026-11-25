;;; lispm-process-tools.el --- Programs called with explicit arguments -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; run_program is run_shell's argv-shaped counterpart.  It uses the same
;; sandbox, but does not construct or classify shell text.  A running call
;; owns its process, stderr pipe and deadline, all released on completion
;; or when its agent leaves.  No persistent jobs are exposed to the model.

;;; Code:

(require 'lispm-job)
(require 'lispm-tools)

(cl-defstruct (lispm-process-tools--run
               (:constructor lispm-process-tools--run-create) (:copier nil))
  "An agent's process and the bounded output collected from its pipes."
  agent process errors timer sandbox done handle output error-output
  (size 0) limit truncated output-truncated error-truncated finished)

(defun lispm-process-tools--active-runs ()
  "Return active `lispm-process-tools--run' records managed by `lispm-job'."
  (mapcar #'lispm-job-data (lispm-job-active nil 'process)))

(defvar lispm-process-output-functions nil
  "Abnormal hook run as `run_program' receives output chunks.
Each is called with (AGENT ERRORP TEXT).")

;;; Law: For budget B >= 0 and strings X, Y with (>= (length X) B),
;;;      (equal (lispm-process-tools--window (concat (lispm-process-tools--window X B) Y) B)
;;;             (lispm-process-tools--window (concat X Y) B)).
(defun lispm-process-tools--window (text budget)
  "Keep at most BUDGET characters of TEXT: 30% from the head and 70% from the tail."
  (cond ((<= budget 0) "")
        ((<= (length text) budget) text)
        (t (let* ((head (/ (* budget 3) 10))
                  (tail (- budget head)))
             (concat (substring text 0 head)
                     (substring text (- (length text) tail)))))))

(defun lispm-process-tools--splice-marker (text budget marker)
  "Bound TEXT to BUDGET, splicing MARKER between 30% head and 70% tail."
  (cond ((<= budget 0) "")
        ((< budget (+ (length marker) 2))
         (lispm-process-tools--window text budget))
        (t (let* ((kept (- budget (length marker)))
                  (head (/ (* kept 3) 10))
                  (tail (- kept head)))
             (concat (substring text 0 head)
                     marker
                     (substring text (- (length text) tail)))))))

(defun lispm-process-tools--budgets (stdout-len stderr-len limit)
  "Partition LIMIT fairly as (OUT-CAP . ERR-CAP) for STDOUT-LEN and STDERR-LEN."
  (let ((half (/ limit 2)))
    (cond ((<= stderr-len half) (cons (- limit stderr-len) stderr-len))
          ((<= stdout-len (- limit half)) (cons stdout-len (- limit stdout-len)))
          (t (cons (- limit half) half)))))

(defun lispm-process-tools--collect (run errorp text)
  "Collect TEXT from RUN's stdout, or stderr when ERRORP, up to its limit.
Once output exceeds the limit, retain a 30% head + 70% tail window per stream."
  (unless (or (lispm-process-tools--run-finished run) (string-empty-p text))
    (let ((limit (lispm-process-tools--run-limit run)))
      (if (and (not (lispm-process-tools--run-truncated run))
               (<= (+ (lispm-process-tools--run-size run) (length text)) limit))
          (progn
            (if errorp
                (push text (lispm-process-tools--run-error-output run))
              (push text (lispm-process-tools--run-output run)))
            (cl-incf (lispm-process-tools--run-size run) (length text)))
        (let* ((stdout (apply #'concat (nreverse (lispm-process-tools--run-output run))))
               (stderr (apply #'concat (nreverse (lispm-process-tools--run-error-output run))))
               (next-out (if errorp stdout (concat stdout text)))
               (next-err (if errorp (concat stderr text) stderr)))
          (setf (lispm-process-tools--run-truncated run) t)
          (pcase-let ((`(,out-cap . ,err-cap)
                       (lispm-process-tools--budgets (length next-out) (length next-err) limit)))
            (when (> (length next-out) out-cap)
              (setf (lispm-process-tools--run-output-truncated run) t)
              (setq next-out (lispm-process-tools--window next-out out-cap)))
            (when (> (length next-err) err-cap)
              (setf (lispm-process-tools--run-error-truncated run) t)
              (setq next-err (lispm-process-tools--window next-err err-cap)))
            (setf (lispm-process-tools--run-output run)
                  (and (not (string-empty-p next-out)) (list next-out))
                  (lispm-process-tools--run-error-output run)
                  (and (not (string-empty-p next-err)) (list next-err))
                  (lispm-process-tools--run-size run)
                  (+ (length next-out) (length next-err)))))))
    (when lispm-process-output-functions
      (with-demoted-errors "LispM: process output hook: %S"
        (run-hook-with-args 'lispm-process-output-functions
                            (lispm-process-tools--run-agent run) errorp text)))))

(defun lispm-process-tools--release (run)
  "Release RUN's processes, timer and sandbox, suppressing late callbacks."
  (setf (lispm-process-tools--run-finished run) t)
  (when-let* ((timer (lispm-process-tools--run-timer run))) (cancel-timer timer))
  (when-let* ((process (lispm-process-tools--run-process run)))
    (lispm-exec-delete process))
  (when-let* ((errors (lispm-process-tools--run-errors run)))
    (when (processp errors) (ignore-errors (delete-process errors))))
  ;; A spawn failure may precede sandbox-attach and its sentinel.
  (when-let* ((sandbox (lispm-process-tools--run-sandbox run)))
    (ignore-errors (lispm-sandbox-forget sandbox))))

(defun lispm-process-tools--cleanup (run)
  "Cancel RUN through `lispm-job' and release its resources."
  (if-let* ((handle (lispm-process-tools--run-handle run)))
      (lispm-job-cancel handle)
    (lispm-process-tools--release run)))

(defun lispm-process-tools--format-stream (raw budget truncatedp marker)
  "Format RAW stream within BUDGET, adding MARKER when TRUNCATEDP or shrunk."
  (if (or truncatedp (< budget (length raw)))
      (lispm-process-tools--splice-marker raw (min (length raw) budget) marker)
    raw))

(defun lispm-process-tools--encode-envelope (fields limit)
  "Encode alist FIELDS as multi-line JSON when within LIMIT, else compact JSON."
  (let ((pretty (lispm-json-pretty fields)))
    (if (<= (length pretty) limit) pretty (lispm-json-encode fields))))

(defun lispm-process-tools--shrink-budget (budget)
  "Reduce positive integer BUDGET conservatively so JSON escaping converges."
  (if (<= budget 4) 0 (/ (* budget 3) 4)))

(defun lispm-process-tools--result (run status exit signal)
  "RUN's STATUS, EXIT, SIGNAL and captured output as bounded JSON.
Truncate from the middle (30% head + 70% tail) before encoding, so the
normal tool limit cannot cut JSON or discard trailing failure summaries."
  (let* ((raw-out (apply #'concat (nreverse (lispm-process-tools--run-output run))))
         (raw-err (or (lispm-tools-strip-benign-stderr
                       (apply #'concat (nreverse (lispm-process-tools--run-error-output run))))
                      ""))
         (limit (lispm-process-tools--run-limit run))
         (truncated (lispm-process-tools--run-truncated run))
         (out-trunc (lispm-process-tools--run-output-truncated run))
         (err-trunc (lispm-process-tools--run-error-truncated run))
         (out-budget (length raw-out))
         (err-budget (length raw-err))
         result)
    (while
        (let ((output (lispm-process-tools--format-stream
                       raw-out out-budget out-trunc "\n...[stdout truncated]...\n"))
              (errors (lispm-process-tools--format-stream
                       raw-err err-budget err-trunc "\n...[stderr truncated]...\n")))
          (setq result
                (lispm-process-tools--encode-envelope
                 `((status . ,status) (exit_code . ,(or exit :null))
                   (signal . ,(or signal :null))
                   (stdout . ,output) (stderr . ,errors)
                   (truncated . ,(if truncated t :false)))
                 limit))
          (and (> (length result) limit)
               (or (> out-budget 0) (> err-budget 0))))
      (setq out-budget (lispm-process-tools--shrink-budget out-budget)
            err-budget (lispm-process-tools--shrink-budget err-budget)
            truncated t))
    (if (<= (length result) limit)
        result
      ;; A deliberately tiny global limit cannot hold the full envelope.
      (let ((small (lispm-json-encode `((status . ,status) (truncated . t)))))
        (if (<= (length small) limit) small "{}")))))

(defun lispm-process-tools--finish (run status &optional exit signal)
  "Finish RUN once, returning STATUS, EXIT and SIGNAL to its caller."
  (unless (lispm-process-tools--run-finished run)
    (when (if-let* ((handle (lispm-process-tools--run-handle run)))
              (lispm-job-finish handle)
            t)
      (let ((result (lispm-process-tools--result run status exit signal)))
        (lispm-process-tools--release run)
        (funcall (lispm-process-tools--run-done run) result
                 (not (and (equal status "exited") (eql exit 0))))))))

(defun lispm-process-tools--sentinel (run process)
  "Complete RUN when PROCESS has exited, draining its bounded stderr first."
  (unless (or (process-live-p process) (lispm-process-tools--run-finished run))
    (let ((errors (lispm-process-tools--run-errors run)))
      ;; Bound drain iterations while allowing final stderr summaries into the tail window.
      (cl-loop repeat 64
               while (and (processp errors) (accept-process-output errors 0 nil t))))
    (if (eq (process-status process) 'exit)
        (lispm-process-tools--finish run "exited" (process-exit-status process))
      (lispm-process-tools--finish run "signalled" nil (process-exit-status process)))))

(defun lispm-process-tools--string-p (value)
  "Non-nil when VALUE is a string that an OS argument can represent."
  (and (stringp value) (not (string-search (string 0) value))))

(defun lispm-process-tools--start (agent program args cwd timeout done)
  "Run PROGRAM with ARGS for AGENT in CWD for TIMEOUT seconds; call DONE."
  (unless (and (lispm-process-tools--string-p program) (not (string-empty-p program))
               (not (file-remote-p program)))
    (error "program must name a local executable"))
  (unless (and (or (null args) (vectorp args))
               (seq-every-p #'lispm-process-tools--string-p args))
    (error "args must be an array of strings without NUL characters"))
  (unless (or (null cwd) (lispm-process-tools--string-p cwd))
    (error "cwd must be a directory name without NUL characters"))
  (unless (and (integerp lispm-tool-output-limit) (>= lispm-tool-output-limit 2))
    (error "run_program needs a tool output limit of at least 2 characters"))
  (let* ((root (lispm-principal-root agent))
         (timeout (or timeout 120))
         (directory (file-name-as-directory (expand-file-name (or cwd root) root))))
    (unless (and (integerp timeout) (<= 1 timeout 86400))
      (error "timeout must be an integer from 1 to 86400 seconds"))
    (when (or (file-remote-p root) (file-remote-p directory))
      (error "run_program runs locally; use run_shell for remote commands"))
    (unless (file-directory-p directory) (error "No program directory: %s" directory))
    (unless (or (eq lispm-sandbox-level 'none)
                (equal (file-truename directory) (file-truename root))
                (file-in-directory-p directory root))
      (error "Sandboxed cwd must stay within the agent's root"))
    (let* ((executable (let ((default-directory directory))
                         (or (when-let* ((found (executable-find program)))
                               (expand-file-name found))
                             (error "Program not found: %s" program))))
           (confined (not (eq lispm-sandbox-level 'none)))
           (default-directory (if confined root directory))
           (command (cons executable (append args nil)))
           (run (lispm-process-tools--run-create
                 :agent agent :done done :limit lispm-tool-output-limit)))
      ;; Keep relative sandbox rules rooted at the project.  This fixed
      ;; wrapper changes only the target's cwd; no argument becomes code.
      (when (and confined (not (equal directory (file-name-as-directory root))))
        (setq command
              (append (list "/bin/sh" "-c" "cd -- \"$1\" || exit; shift; exec \"$@\""
                            "lispm-program-cwd" directory)
                      command)))
      (lispm-tools-with-async-start (lispm-process-tools--cleanup run)
        (setf (lispm-process-tools--run-handle run)
              (lispm-job-register agent 'process
                                  (lambda () (lispm-process-tools--release run))
                                  run))
        (setf (lispm-process-tools--run-sandbox run)
              (lispm-sandbox-prepare command nil (format "the program %s" program)))
        (setf (lispm-process-tools--run-errors run)
              (make-pipe-process
               :name "lispm-program-stderr" :buffer nil :noquery t :coding 'utf-8
               :filter (lambda (_process text) (lispm-process-tools--collect run t text))
               :sentinel #'ignore))
        (setf (lispm-process-tools--run-process run)
              (make-process
               :name "lispm-program" :buffer nil :noquery t :coding 'utf-8
               :connection-type 'pipe :stderr (lispm-process-tools--run-errors run)
               :command (lispm-sandbox-run-command (lispm-process-tools--run-sandbox run))
               :filter (lambda (_process text) (lispm-process-tools--collect run nil text))
               :sentinel (lambda (process _event) (lispm-process-tools--sentinel run process))))
        (process-put (lispm-process-tools--run-process run) 'lispm-agent agent)
        (lispm-sandbox-attach (lispm-process-tools--run-sandbox run)
                              (lispm-process-tools--run-process run))
        (unless (lispm-process-tools--run-finished run)
          (setf (lispm-process-tools--run-timer run)
                (run-at-time timeout nil #'lispm-process-tools--finish run "timed_out")))
        (ignore-errors (process-send-eof (lispm-process-tools--run-process run))))))
  nil)

(lispm-deftool run_program
    ((program string "Local executable name or path; no shell expansion.")
     (args array "Exact argv strings, without the executable itself." :optional t
           :items ((type . "string")))
     (cwd string "Working directory, relative to your root by default; local only." :optional t)
     (timeout integer "Execution deadline in seconds (default 120, maximum 86400)." :optional t))
  "Run a local external program or CLI asynchronously with explicit argv.
Use this for commands, builds and tests that need no shell operators: each
argument arrives unchanged, including spaces, quotes and shell punctuation.
Returns JSON with status, exit_code, signal, stdout, stderr and truncated.
Output is bounded by the tool output limit; nonzero exits and timeouts fail.
The process reads no stdin. It follows your shell permission policy and
sandbox; when sandboxed, cwd must remain within your root. Aborting the turn,
closing its session or external-agent terminal stops its processes.
Use run_lisp to compose calls, run_shell for pipelines or remote commands,
and run_elisp for standalone Lisp/JSON processing in a kernel worker."
  :permission shell
  :async t
  :keywords '("process" "command" "CLI" "argv" "build" "test" "exit" "stdout" "stderr")
  (lispm-process-tools--start (or lispm-current-agent (error "run_program needs an agent"))
                              program args cwd timeout done))

(defun lispm-process-tools-unload-function ()
  "Stop running programs before unloading their callbacks."
  (lispm-job-cancel-kind 'process)
  (lispm-tools-unregister "run_program")
  nil)

(provide 'lispm-process-tools)
;;; lispm-process-tools.el ends here
