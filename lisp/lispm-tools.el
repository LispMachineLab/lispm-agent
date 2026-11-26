;;; lispm-tools.el --- Tools: Emacs actions an agent can take  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A tool is an Emacs Lisp function the model may call, declared with
;; `lispm-deftool'.  The declaration gives each tool a permission class
;; (read, memory, comm, write, shell or exec), from which an agent's policy
;; decides whether a call runs at once, waits for the user, or is
;; refused; and optionally a preview function that shows the user what
;; a call would change (a diff) before they approve it.
;;
;; The built-in tools work on live Emacs state rather than on disk:
;; reading a file that is open returns the buffer, unsaved edits
;; included; edits go through the visiting buffer, as one undo step,
;; so the user sees them happen, can undo them with C-/, and every
;; buffer touched is snapshotted in the agent's journal for
;; `lispm-revert-turn'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-agents-context)
(require 'lispm-job)
(require 'lispm-sandbox)
(require 'lispm-tool-core)
(require 'lispm-tool-permissions)

(defvar lispm-current-call)

(defun lispm-tools-shell-log-buffer-name (agent &optional call-id)
  "Return the shell log buffer name for AGENT and optional CALL-ID."
  (let ((label (if agent (lispm-principal-name agent) "?")))
    (if call-id
        (format "*lispm-shell: %s (%s)*" label call-id)
      (format "*lispm-shell: %s*" label))))
(require 'lispm-tool-search)
(require 'lispm-tool-builtins)
(require 'lispm-shell-classify)

;;;; Built-in tools: execution

(defmacro lispm-tools-with-async-start (cleanup &rest body)
  "Run BODY with quitting inhibited; run CLEANUP on any error or `quit'.
If `C-g' was pressed while BODY registered an asynchronous job, signal
`quit' once BODY finishes so CLEANUP cancels the job atomically."
  (declare (indent 1) (debug (form body)))
  (let ((err (make-symbol "err")))
    `(condition-case ,err
         (progn
           (let ((inhibit-quit t))
             ,@body)
           (when quit-flag
             (setq quit-flag nil)
             (signal 'quit nil)))
       ((error quit)
        (let ((inhibit-quit t))
          ,cleanup)
        (signal (car ,err) (cdr ,err))))))

(defvar lispm-tools--shell-seq 0
  "Monotonic counter for unique `run_shell' process names.")

(defun lispm-tools--effective-shell-timeout (timeout)
  "Effective shell command timeout in seconds for requested TIMEOUT.
When `lispm-turn-deadline' is bound to a positive `float-time', clamp
the timeout so a single shell command cannot consume the entire remaining
wall-clock budget."
  (let ((requested (max 1 (ceiling (or timeout lispm-shell-timeout)))))
    (if (and (numberp lispm-turn-deadline) (> lispm-turn-deadline 0))
        (let* ((remaining (- lispm-turn-deadline (float-time)))
               (total (and (numberp lispm-turn-started-at)
                           (- lispm-turn-deadline lispm-turn-started-at)))
               (reserve (if (and total (>= total 300))
                            (min 90.0 (max 30.0 (* 0.04 total)))
                          0.0))
               (budget (max 5 (floor (- remaining reserve)))))
          (min requested budget))
      requested)))

(defun lispm-tools--run-shell-via-kernel (agent command timeout log-buffer done fallback)
  "Run unsandboxed COMMAND in the kernel when `lispm-kernel-exec-mode' is on.
Return non-nil when the kernel took the command, or nil to run FALLBACK."
  (when (and (bound-and-true-p lispm-kernel-exec-mode)
             (or (file-remote-p default-directory)
                 (eq lispm-sandbox-level 'none))
             (fboundp 'lispm-kernel-exec--submit))
    (let* ((remote (file-remote-p default-directory))
           (cmd (lispm-exec-shell-command
                 (if remote "/bin/sh" shell-file-name)
                 (if remote "-c" shell-command-switch)
                 command nil))
           (effective-timeout (lispm-tools--effective-shell-timeout timeout))
           (cancel nil)
           (job (lispm-job-register
                 agent 'kernel-shell
                 (lambda () (when (functionp cancel) (funcall cancel))))))
      (setq cancel
            (condition-case err
                (lispm-kernel-exec--submit
                 (car cmd) (cdr cmd)
                 (lambda (exit printed said)
                   (when (lispm-job-finish job)
                     (let* ((output (concat printed said))
                            (timed-out (and (stringp exit)
                                            (or (string-match-p "did not finish" exit)
                                                (string-match-p "timed out" exit))
                                            t))
                            (status (cond ((integerp exit) exit)
                                          ((and (stringp exit)
                                                (string-match "ended by signal \\([0-9]+\\)" exit))
                                           (+ 128 (string-to-number (match-string 1 exit))))
                                          (t 1))))
                       (run-at-time
                        0 nil done
                        (concat output
                                "\n["
                                (if timed-out
                                    "killed: timeout"
                                  (format "exit status %d" status))
                                "]")
                        timed-out))))
                 (lambda (&rest _)
                   (when (lispm-job-finish job)
                     (funcall fallback)))
                 :timeout effective-timeout
                 :combine-error t
                 :on-output (lambda (chunk)
                              (when (buffer-live-p log-buffer)
                                (with-current-buffer log-buffer
                                  (save-excursion
                                    (goto-char (point-max))
                                    (insert chunk))))))
              (error
               (lispm-job-finish job)
               (signal (car err) (cdr err)))))
      (if (not cancel)
          (progn (lispm-job-finish job) nil)
        (when (buffer-live-p log-buffer)
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert (format "\n$ %s\n" command))))
        (when quit-flag
          (setq quit-flag nil)
          (lispm-job-cancel job)
          (signal 'quit nil))
        t))))

(defun lispm-tools--shell-append-log (log-buffer text)
  "Append TEXT to LOG-BUFFER when live."
  (when (buffer-live-p log-buffer)
    (with-current-buffer log-buffer
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun lispm-tools--shell-sentinel (proc-buffer output-text done note)
  "Return a process sentinel finishing via DONE with OUTPUT-TEXT and NOTE."
  (lambda (p _event)
    (unless (or (process-live-p p) (process-get p 'interrupted)
                (process-get p 'lispm-done))
      (process-put p 'lispm-done t)
      ;; Drain any bytes buffered in the pipe before reading `output-text'.
      (cl-loop repeat 64 while (ignore-errors (accept-process-output p 0 nil t)))
      (when-let* ((timer (process-get p 'timer))) (cancel-timer timer))
      (let ((output (funcall output-text)))
        (when (and (buffer-live-p proc-buffer) (string-empty-p output))
          (let ((pre (with-current-buffer proc-buffer (buffer-string))))
            (unless (string-empty-p pre) (setq output pre))))
        (when (buffer-live-p proc-buffer) (kill-buffer proc-buffer))
        (let ((status (process-exit-status p))
              (timed-out (process-get p 'timed-out)))
          (run-at-time 0 nil done
                       (concat (lispm-tools-strip-benign-stderr output)
                               "\n["
                               (if timed-out "killed: timeout" (format "exit status %d" status))
                               (if note (concat "; " note) "")
                               "]")
                       timed-out))))))

(defun lispm-tools--shell-attach-timer (proc timeout)
  "Attach TIMEOUT watchdog timer to PROC if live."
  (when (and (process-live-p proc) (not (process-get proc 'lispm-done)))
    (process-put proc 'timer
                 (run-at-time (lispm-tools--effective-shell-timeout timeout) nil
                              (lambda ()
                                (unless (process-get proc 'lispm-done)
                                  (if (process-live-p proc)
                                      (progn
                                        (process-put proc 'timed-out t)
                                        (lispm-exec-kill proc)
                                        (when (and (not (process-live-p proc))
                                                   (not (process-get proc 'lispm-done)))
                                          (funcall (process-sentinel proc) proc "killed\n")))
                                    (funcall (process-sentinel proc) proc "finished\n"))))))))

(defun lispm-tools--shell-finish-start (proc proc-buffer timeout)
  "Finish starting PROC using PROC-BUFFER and TIMEOUT, or handle C-g abort."
  (if (and (not (process-live-p proc)) (not (process-get proc 'lispm-done)))
      (funcall (process-sentinel proc) proc "finished\n")
    (lispm-tools--shell-attach-timer proc timeout))
  (when quit-flag
    (setq quit-flag nil)
    (when-let* ((timer (process-get proc 'timer))) (cancel-timer timer))
    (process-put proc 'interrupted t)
    (lispm-exec-delete proc)
    (when (buffer-live-p proc-buffer) (kill-buffer proc-buffer))
    (signal 'quit nil)))

(defun lispm-tools--shell-run-locally (agent command timeout log-buffer done)
  "Run shell COMMAND for AGENT into LOG-BUFFER, calling DONE."
  (let* ((start-dir default-directory)
         (proc-buffer (generate-new-buffer
                       (format " *lispm-shell-proc: %s*" (if agent (lispm-principal-name agent) "?"))
                       t))
         (proc-name (format "lispm-shell-%d" (cl-incf lispm-tools--shell-seq)))
         (output-chunks nil)
         (output-text (lambda () (apply #'concat (reverse output-chunks))))
         (cancelled nil)
         (spawned-proc nil)
         (job nil)
         (raw-cmd (with-connection-local-variables
                   (lispm-exec-shell-command
                    shell-file-name shell-command-switch command
                    (file-remote-p start-dir))))
         (in-start t)
         (started nil))
    (setq job (lispm-job-register
               agent 'shell
               (lambda ()
                 (setq cancelled t)
                 (when (process-live-p spawned-proc)
                   (lispm-exec-delete spawned-proc))
                 (when (buffer-live-p proc-buffer)
                   (kill-buffer proc-buffer)))))
    (unwind-protect
        (progn
          (lispm-sandbox-prepare-async
           raw-cmd
           (lambda (run err-msg)
             (cond
              (cancelled
               (when run (ignore-errors (lispm-sandbox-forget run))))
              ((not run)
               (lispm-job-finish job)
               (when (buffer-live-p proc-buffer) (kill-buffer proc-buffer))
               (funcall done (or err-msg "Sandbox unavailable") t))
              (t
               (condition-case err
                   (let* ((default-directory start-dir)
                          (proc (make-process
                                 :name proc-name :buffer proc-buffer :noquery t
                                 :connection-type 'pipe :file-handler t
                                 :command (lispm-sandbox-run-command run)
                                 :filter (lambda (_p chunk)
                                           (push chunk output-chunks)
                                           (lispm-tools--shell-append-log log-buffer chunk))
                                 :sentinel (lispm-tools--shell-sentinel
                                            proc-buffer output-text
                                            (lambda (text errorp)
                                              (lispm-job-finish job)
                                              (funcall done text errorp))
                                            (lispm-sandbox-run-note run)))))
                     (setq spawned-proc proc)
                     (lispm-sandbox-attach run proc)
                     (lispm-tools--shell-append-log log-buffer (format "\n$ %s\n" command))
                     (when (and (buffer-live-p proc-buffer) (null output-chunks))
                       (let ((pre (with-current-buffer proc-buffer (buffer-string))))
                         (unless (string-empty-p pre)
                           (push pre output-chunks)
                           (lispm-tools--shell-append-log log-buffer pre))))
                     (process-put proc 'lispm-agent agent)
                     (lispm-tools--shell-finish-start proc proc-buffer timeout))
                 (error
                  (lispm-job-finish job)
                  (ignore-errors (lispm-sandbox-forget run))
                  (when (buffer-live-p proc-buffer) (kill-buffer proc-buffer))
                  (if in-start
                      (signal (car err) (cdr err))
                    (funcall done (error-message-string err) t)))))))
           nil (format "the command `%s'" (truncate-string-to-width command 30 nil nil "…")))
          (setq in-start nil
                started t))
      (unless started
        (setq cancelled t)
        (lispm-job-finish job)
        (when (buffer-live-p proc-buffer) (kill-buffer proc-buffer))))))

(lispm-deftool run_shell
    ((command string "Shell command, run in your root directory.")
     (timeout integer "Seconds before the command is killed (default 120)." :optional t))
  "Run a shell command and return its output and exit status.  Use this
for external CLIs, builds, tests, project runtimes and commands that
must run remotely.  For standalone local file, JSON/JSONL or text processing,
prefer LispM's dedicated tools or run_elisp, whose worker LispM Kernel starts.
The output also streams into a buffer the user can watch.  If your root is
remote, the command runs on its machine.  The user may have the
command run in a sandbox: its result then says what the sandbox
allows."
  :permission shell
  ;; A command that only reads is decided as reading is.
  :class (lambda (command &optional _timeout)
           (and (lispm-shell-read-only-p command) 'read))
  :async t
  ;; Setting up must not be cut short by C-g: a process left without
  ;; its timer and its agent could be neither timed out nor aborted.
  ;; A C-g meanwhile stops the command once it is set up.
  (let* ((inhibit-quit t)
         (agent lispm-current-agent)
         (log-buffer (get-buffer-create
                      (lispm-tools-shell-log-buffer-name
                       agent (bound-and-true-p lispm-current-call))))
         (default-directory (lispm-tools-root))
         (fallback (lambda ()
                     (lispm-tools--shell-run-locally
                      agent command timeout log-buffer done))))
    (unless (lispm-tools--run-shell-via-kernel
             agent command timeout log-buffer done fallback)
      (funcall fallback))))

(defun lispm-tools-read-forms (code)
  "All Lisp forms in the string CODE."
  (with-temp-buffer
    (with-syntax-table emacs-lisp-mode-syntax-table
      (insert code)
      (goto-char (point-min))
      (let (forms)
        (while (progn (forward-comment (point-max))
                      (not (eobp)))
          (push (read (current-buffer)) forms))
        (nreverse forms)))))

(defcustom lispm-tools-file-writers
  '(save-buffer save-some-buffers write-file write-region append-to-file
    with-temp-file delete-file rename-file copy-file make-directory
    delete-directory make-symbolic-link set-file-modes)
  "Functions that change files on disk.
Code naming one of them changes files: see `lispm-tools-code-writes-p'."
  :type '(repeat symbol)
  :group 'lispm)

(defcustom lispm-tools-buffer-writers
  '(insert insert-before-markers delete-region erase-buffer delete-char
    kill-region kill-line kill-buffer replace-regexp replace-string
    replace-regexp-in-region replace-string-in-region indent-region
    comment-region uncomment-region upcase-region downcase-region
    set-buffer-modified-p)
  "Functions that change the text of a buffer.
Code naming one of them changes buffers, unless it names it inside a
`with-temp-buffer' form, whose buffer is gone once it ends: see
`lispm-tools-code-writes-p'."
  :type '(repeat symbol)
  :group 'lispm)

(defun lispm-tools--walk-writes (form temporary writers strings seen)
  "Collect what FORM names into WRITERS and STRINGS, two cons cells.
TEMPORARY non-nil while inside a `with-temp-buffer' form, where only
`lispm-tools-file-writers' count; WRITERS and STRINGS hold the symbols
and strings found, newest first, in their cdr.  SEEN, a hash table, holds
the conses and vectors walked: a form read with circular syntax, such as
#1=(insert . #1#), is walked once and the walk ends."
  (cond ((symbolp form)
         (when (and form
                    (or (memq form lispm-tools-file-writers)
                        (and (not temporary) (memq form lispm-tools-buffer-writers)))
                    (not (memq form (cdr writers))))
           (push form (cdr writers))))
        ((stringp form)
         (unless (member form (cdr strings)) (push form (cdr strings))))
        ((gethash form seen))
        ((vectorp form)
         (puthash form t seen)
         (mapc (lambda (f) (lispm-tools--walk-writes f temporary writers strings seen)) form))
        ((consp form)
         (let ((temporary (or temporary (eq (car-safe form) 'with-temp-buffer))))
           ;; A dotted tail ends the list; a cons seen before, a circle.
           (while (and (consp form) (not (gethash form seen)))
             (puthash form t seen)
             (lispm-tools--walk-writes (pop form) temporary writers strings seen))
           (when (and form (not (consp form)))
             (lispm-tools--walk-writes form temporary writers strings seen))))))

(defun lispm-tools--code-writes (code)
  "What the Lisp CODE, a string, names: (WRITERS . STRINGS), in order.
WRITERS are the symbols of `lispm-tools-file-writers' and
`lispm-tools-buffer-writers' it names (see `lispm-tools-code-writes-p'),
STRINGS its strings.  Where CODE stops reading, the rest is searched for
the writers' names as words, and gives no strings."
  (let ((writers (list nil)) (strings (list nil)) (seen (make-hash-table :test #'eq))
        (at 0) (end (length code)))
    (while (< at end)
      (condition-case nil
          (pcase-let ((`(,form . ,next) (read-from-string code at)))
            (lispm-tools--walk-writes form nil writers strings seen)
            (setq at next))
        ;; The rest does not read: whitespace, or an unbalanced form.
        (error
         (dolist (name (append lispm-tools-file-writers lispm-tools-buffer-writers))
           (when (and (not (memq name (cdr writers)))
                      (string-match-p (concat "\\_<" (regexp-quote (symbol-name name)) "\\_>")
                                      (substring code at)))
             (push name (cdr writers))))
         (setq at end))))
    (cons (nreverse (cdr writers)) (nreverse (cdr strings)))))

(defun lispm-tools-code-writes-p (code)
  "The functions changing buffers or files that the Lisp CODE names, or nil.
CODE is a string of forms.  Those of `lispm-tools-file-writers' count
wherever they appear; those of `lispm-tools-buffer-writers' outside a
`with-temp-buffer' form.  A quoted name counts as much as a call, so
\(mapc #\\='delete-file files) does.  Code that does not read is searched
for the names as words."
  (car (lispm-tools--code-writes code)))

(defun lispm-tools-code-files (code)
  "The strings in the Lisp CODE that look like file names, in order.
A string with no whitespace, naming a directory (a slash, not of a
URL) or ending in a file extension."
  (seq-filter (lambda (s)
                (and (> (length s) 1)
                     (not (string-match-p "[[:space:]\\\\]\\|://" s))
                     (or (string-search "/" s)
                         (string-match-p "\\`[^.]+\\.[A-Za-z0-9]\\{1,5\\}\\'" s))))
              (cdr (lispm-tools--code-writes code))))

(defun lispm-tools-code-writes-note (code)
  "What the user is told of the Lisp CODE if it changes buffers or files.
Nil if it names none of `lispm-tools-file-writers' and
`lispm-tools-buffer-writers' (see `lispm-tools-code-writes-p'); else
that it does, that no diff shows it, and the files it names (see
`lispm-tools-code-files')."
  (when (lispm-tools-code-writes-p code)
    (let ((files (lispm-tools-code-files code)))
      (concat (lispm-tr "This code changes buffers or files; it shows no diff, as the edit tools would.")
              (if files
                  (concat " " (lispm-tr "It names %s." (string-join files ", ")))
                "")))))

(defcustom lispm-eval-timeout 120
  "Seconds eval_elisp waits for a value promised with `lispm-async'."
  :type 'integer
  :group 'lispm)

(defcustom lispm-eval-backtrace t
  "Whether an error of the code eval_elisp evaluates comes with its backtrace.
The backtrace shows the arguments of the calls between the code and
the error: values the model may not have seen otherwise, such as a
password read with `auth-source'.  Turn it off if those should not
leave Emacs."
  :type 'boolean
  :group 'lispm)

(cl-defstruct (lispm-async (:constructor lispm-async--create) (:copier nil))
  "A value computed asynchronously: see `lispm-async'."
  settled value failed waiters)

(defun lispm-async (function)
  "Return a promise of the value FUNCTION computes asynchronously.
FUNCTION is called at once with a function RESOLVE, to call later with
the value, and non-nil as second argument if it is a failure.  Code
that eval_elisp evaluates can end with such a promise, and the agent
gets the value once it is resolved:

  (lispm-async
   (lambda (resolve)
     (url-retrieve URL (lambda (_) (funcall resolve (buffer-string))))))"
  (let ((promise (lispm-async--create)))
    (funcall function
             (lambda (value &optional failed)
               (unless (lispm-async-settled promise)
                 (setf (lispm-async-settled promise) t
                       (lispm-async-value promise) value
                       (lispm-async-failed promise) failed)
                 (dolist (waiter (lispm-async-waiters promise))
                   (funcall waiter value failed)))))
    promise))

(defun lispm-async-then (promise function)
  "Call FUNCTION with PROMISE's value and failure flag once it is settled."
  (if (lispm-async-settled promise)
      (funcall function (lispm-async-value promise) (lispm-async-failed promise))
    (push function (lispm-async-waiters promise))))

(defun lispm-tools-eval-result (printed value)
  "What eval_elisp returns: PRINTED output, then VALUE."
  (let ((print-length 100) (print-level 10))
    (concat (if (string-empty-p printed) "" (concat printed "\n"))
            "=> " (if (stringp value) value (prin1-to-string value)))))

(defconst lispm-tools--backtrace-frames 20
  "How many frames of a backtrace eval_elisp returns with an error, at most.")

(defvar lispm-tools--eval-form nil
  "The form eval_elisp is evaluating: where its backtraces stop.")

(defvar lispm-tools--backtrace nil
  "Frames of the last error the code eval_elisp evaluates signaled.
Each is (FUNCTION . ARGS), innermost first.")

(defun lispm-tools--note-backtrace (_error)
  "Keep the frames of an error the code eval_elisp evaluates signals.
A `handler-bind' handler: it runs where the error is signaled, before
the stack unwinds, and lets the error go on.  As it runs for errors
the code catches too, it only collects the frames up to the `eval' of
`lispm-tools--eval-form', one more than `lispm-tools--backtrace-frames'
at most: they are printed if the error reaches eval_elisp."
  (when lispm-eval-backtrace
    (let ((frames nil) (count 0))
      (catch 'done
        (mapbacktrace
         (lambda (_evald function args _flags)
           (when (and (eq function 'eval) (eq (car-safe args) lispm-tools--eval-form))
             (throw 'done nil))
           (unless (eq function #'lispm-tools--note-backtrace)
             (push (cons function args) frames)
             (when (> (cl-incf count) lispm-tools--backtrace-frames)
               (throw 'done nil))))
         #'lispm-tools--note-backtrace))
      (setq lispm-tools--backtrace (nreverse frames)))))

(defun lispm-tools-backtrace-text (frames)
  "FRAMES, each (FUNCTION . ARGS), innermost first, as text.
Past `lispm-tools--backtrace-frames' of them, \"...\" stands for the rest."
  (let ((print-length 6) (print-level 3) (print-escape-newlines t))
    (lispm-truncate
     (string-join
      (append (mapcar (lambda (frame)
                        (concat "  " (truncate-string-to-width (prin1-to-string frame)
                                                               200 nil nil "…")))
                      (seq-take frames lispm-tools--backtrace-frames))
              (and (nthcdr lispm-tools--backtrace-frames frames) (list "  ...")))
      "\n")
     4000)))

(defun lispm-tools--defined-note (names)
  "What eval_elisp adds to its result about the tools NAMES it defined."
  (if names
      (concat "\n\n" lispm-tools-loaded-prefix (string-join names ", ") ".\n"
              "They are yours from your next step on, until Emacs exits; "
              "to keep them for later sessions, write a skill (write_skill).")
    ""))

(lispm-deftool eval_elisp
    ((code string "One or more Emacs Lisp forms, evaluated in order with lexical binding."))
  "Evaluate Emacs Lisp in the running Emacs and return the last value,
plus anything printed.  This reaches all of Emacs: buffers, windows,
processes, the network, org, magit, any installed package.  Use this when
you need the live editor; keep its work short because it can block the UI.
Use run_elisp for standalone file/JSON processing or heavy computation in
a separate process launched by LispM Kernel. A timer or lispm-async does
not offload computation. Find functions with apropos, and the skills of this
system's parts and apps with search_tools; learn functions with
describe_symbol, and from the manuals with read_info, which search_tools finds.
To wait for something asynchronous (a process, a page loading, a
timer), end with (lispm-async (lambda (resolve) ...)) and call
\(funcall resolve VALUE) once the value is there.  An error comes back
with the backtrace of the code, innermost call first.  A tool you
define here with lispm-deftool is yours from your next step on, and
other agents can find it with search_tools; it lasts until Emacs
exits, unless you keep it in a skill with write_skill, which
search_tools finds.
Files and buffers are changed with the edit tools (edit_file,
write_file, edit_buffer, edit_definition), which show the user a diff
to approve and can be undone as one step.  Lisp that saves or rewrites
a file gets no such review: it is for what the edit tools cannot do."
  :permission exec
  :concurrency 'editor
  :async t
  :preview #'lispm-tools-code-writes-note
  (let* ((agent lispm-current-agent)
         (output (generate-new-buffer " *lispm-eval*" t))
         (standard-output output)
         (default-directory (lispm-tools-root))
         (before (lispm-tools-snapshot))
         value printed failure note)
    (unwind-protect
        (condition-case err
            (dolist (form (lispm-tools-read-forms code))
              (let ((lispm-tools--eval-form form)
                    (lispm-tools--backtrace nil))
                (condition-case err
                    (setq value (handler-bind ((error #'lispm-tools--note-backtrace))
                                  (eval form t)))
                  (error
                   (if (not lispm-tools--backtrace)
                       (signal (car err) (cdr err))
                     (error "%s\n\nBacktrace, innermost call first:\n%s"
                            (error-message-string err)
                            (lispm-tools-backtrace-text lispm-tools--backtrace)))))))
          (error (setq failure (error-message-string err))))
      (setq printed (with-current-buffer output (buffer-string)))
      (kill-buffer output)
      ;; Tools the code defined, also before an error or a C-g, are
      ;; the agent's, deferred: in no other agent's core layer.
      (setq note (lispm-tools--defined-note (lispm-tools-adopt agent before))))
    (cond
     (failure (error "%s%s" failure note))
     ((not (lispm-async-p value))
      (funcall done (concat (lispm-tools-eval-result printed value) note)))
     (t
      (let ((timer (run-at-time
                    lispm-eval-timeout nil
                    (lambda ()
                      (funcall done (format "Timed out after %d seconds waiting for the promised value.%s"
                                            lispm-eval-timeout note)
                               t)))))
        (lispm-async-then value
                          (lambda (result failed)
                            (cancel-timer timer)
                            (funcall done (concat (lispm-tools-eval-result printed result) note)
                                     failed))))))))

(provide 'lispm-tools)
;;; lispm-tools.el ends here
