;;; lispm-process-tools.el --- Programs called with explicit arguments -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; run_program is run_shell's argv-shaped counterpart.  It uses the same
;; sandbox, but does not construct or classify shell text.  A running call
;; owns its process, stderr pipe and deadline, all released on completion
;; or when its agent leaves.  No persistent jobs are exposed to the model.

;;; Code:

(require 'lispm-tools)

(cl-defstruct (lispm-process-tools--run
               (:constructor lispm-process-tools--run-create) (:copier nil))
  "An agent's process and the bounded output collected from its pipes."
  agent process errors timer sandbox done output error-output
  (size 0) limit truncated finished)

(defvar lispm-process-tools--runs nil
  "Program calls that have not completed or been cancelled.")

(defun lispm-process-tools--collect (run errorp text)
  "Collect TEXT from RUN's stdout, or stderr when ERRORP, up to its limit."
  (unless (lispm-process-tools--run-finished run)
    (let ((room (max 0 (- (lispm-process-tools--run-limit run)
                         (lispm-process-tools--run-size run)))))
      (when (> (length text) room)
        (setf (lispm-process-tools--run-truncated run) t))
      (when (> room 0)
        (let ((part (substring text 0 (min room (length text)))))
          (if errorp
              (push part (lispm-process-tools--run-error-output run))
            (push part (lispm-process-tools--run-output run)))
          (cl-incf (lispm-process-tools--run-size run) (length part)))))))

(defun lispm-process-tools--cleanup (run)
  "Release RUN's resources, suppressing any late process callbacks."
  (setf (lispm-process-tools--run-finished run) t)
  (setq lispm-process-tools--runs (delq run lispm-process-tools--runs))
  (when-let* ((timer (lispm-process-tools--run-timer run))) (cancel-timer timer))
  (dolist (process (list (lispm-process-tools--run-process run)
                         (lispm-process-tools--run-errors run)))
    (when (processp process) (ignore-errors (delete-process process))))
  ;; A spawn failure may precede sandbox-attach and its sentinel.
  (when-let* ((sandbox (lispm-process-tools--run-sandbox run)))
    (ignore-errors (lispm-sandbox--forget sandbox)))
  (unless lispm-process-tools--runs
    (remove-hook 'lispm-state-change-functions #'lispm-process-tools--state-changed)
    (remove-hook 'lispm-session-end-functions #'lispm-process-tools--session-ended)
    (remove-hook 'lispm-agents-context-release-functions #'lispm-process-tools--terminal-ended)))

(defun lispm-process-tools--result (run status exit signal)
  "RUN's STATUS, EXIT, SIGNAL and captured output as bounded JSON.
Truncate strings before encoding, so the normal tool limit cannot cut JSON."
  (let* ((output (apply #'concat (nreverse (lispm-process-tools--run-output run))))
         (errors (apply #'concat (nreverse (lispm-process-tools--run-error-output run))))
         (limit (lispm-process-tools--run-limit run))
         (truncated (lispm-process-tools--run-truncated run))
         result)
    (while
        (progn
          (setq result
                (lispm--json-encode
                 `((status . ,status) (exit_code . ,(or exit :null))
                   (signal . ,(or signal :null))
                   (stdout . ,output) (stderr . ,errors)
                   (truncated . ,(if truncated t :false)))))
          (and (> (length result) limit)
               (or (not (string-empty-p output)) (not (string-empty-p errors)))))
      (setq output (substring output 0 (/ (length output) 2))
            errors (substring errors 0 (/ (length errors) 2))
            truncated t))
    (if (<= (length result) limit)
        result
      ;; A deliberately tiny global limit cannot hold the full envelope.
      (let ((small (lispm--json-encode `((status . ,status) (truncated . t)))))
        (if (<= (length small) limit) small "{}")))))

(defun lispm-process-tools--finish (run status &optional exit signal)
  "Finish RUN once, returning STATUS, EXIT and SIGNAL to its caller."
  (unless (lispm-process-tools--run-finished run)
    (let ((result (lispm-process-tools--result run status exit signal)))
      (lispm-process-tools--cleanup run)
      (funcall (lispm-process-tools--run-done run) result
               (not (and (equal status "exited") (eql exit 0)))))))

(defun lispm-process-tools--sentinel (run process)
  "Complete RUN when PROCESS has exited, draining its bounded stderr first."
  (unless (or (process-live-p process) (lispm-process-tools--run-finished run))
    (let ((errors (lispm-process-tools--run-errors run)))
      ;; Read once even at the cap, so unread diagnostics mark truncation.
      (while (and (processp errors) (accept-process-output errors 0 nil t)
                  (< (lispm-process-tools--run-size run) (lispm-process-tools--run-limit run)))))
    (if (eq (process-status process) 'exit)
        (lispm-process-tools--finish run "exited" (process-exit-status process))
      (lispm-process-tools--finish run "signalled" nil (process-exit-status process)))))

(defun lispm-process-tools--session-ended (agent)
  "Cancel the programs of AGENT without reviving its closing caller."
  (dolist (run (copy-sequence lispm-process-tools--runs))
    (when (eq agent (lispm-process-tools--run-agent run))
      (lispm-process-tools--cleanup run))))

(defun lispm-process-tools--state-changed (agent)
  "Stop AGENT's programs when its turn is being aborted."
  (when (eq (lispm-agent-state agent) 'aborting)
    (lispm-process-tools--session-ended agent)))

(defun lispm-process-tools--terminal-ended (terminal)
  "Stop the programs belonging to the external agent in TERMINAL."
  (when (and (buffer-live-p terminal)
             (local-variable-p 'lispm-agents-bridge--agent terminal))
    (lispm-process-tools--session-ended
     (buffer-local-value 'lispm-agents-bridge--agent terminal))))

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
  (let* ((root (lispm-agent-root agent))
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
      (condition-case err
          (progn
            (let ((inhibit-quit t))
              (push run lispm-process-tools--runs)
              (add-hook 'lispm-state-change-functions #'lispm-process-tools--state-changed)
              (add-hook 'lispm-session-end-functions #'lispm-process-tools--session-ended)
              (add-hook 'lispm-agents-context-release-functions #'lispm-process-tools--terminal-ended)
              (setf (lispm-process-tools--run-sandbox run)
                    (lispm-sandbox-prepare command nil
                                           (format "the program %s" program)))
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
              (ignore-errors (process-send-eof (lispm-process-tools--run-process run))))
            (when quit-flag (signal 'quit nil)))
        ((error quit)
         (let ((inhibit-quit t)) (lispm-process-tools--cleanup run))
         (signal (car err) (cdr err))))))
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
  (mapc #'lispm-process-tools--cleanup (copy-sequence lispm-process-tools--runs))
  (remhash "run_program" lispm-tools--registry)
  nil)

(provide 'lispm-process-tools)
;;; lispm-process-tools.el ends here
