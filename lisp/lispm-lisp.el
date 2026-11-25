;;; lispm-lisp.el --- Lisp under policy: programs that call tools  -*- lexical-binding: t; -*-

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

;; An agent acts through tool calls, each under its policy, or through
;; eval_elisp, which reaches all of Emacs and so is outside the policy:
;; Lisp evaluated that way can change the policy itself.  run_lisp is
;; the way between, Lisp as the agent's language with the runtime still
;; mediating its effects, as on a Lisp Machine the user's language and
;; the system's were one.  A program calls tools as functions, with
;; keyword arguments:
;;
;;   (dolist (file (directory-files "lisp" t "\\.el\\'"))
;;     (when (string-search "Copyright (C) 2025" (read_file :path file))
;;       (edit_file :path file :old-text "Copyright (C) 2025"
;;                  :new-text "Copyright (C) 2026")))
;;
;; Each call goes through the agent's policy as if the model had made
;; it: reads run at once, a write awaits the user's approval, shown as
;; any other is, and a denial signals an error the program can handle
;; (`lispm-call-denied').
;;
;; Otherwise a program only computes.  It is checked before it runs: it
;; may use the special forms and common macros, and call the functions
;; Emacs declares free of side effects (`side-effect-free', `pure') and
;; a few more of the kind; it binds and sets only variables of its own.
;; What cannot be checked before is checked as it runs: the functions
;; it hands to mapcar and the like must be its own lambdas or allowed
;; ones; it changes only buffers and hash tables it made, and moves
;; point elsewhere only inside `save-excursion'; destructive list
;; functions work on a copy.  The check is an allowlist, so what it
;; does not know is refused, with a word on why.
;;
;; Emacs Lisp evaluation cannot stop in the middle to wait for an
;; approval or an asynchronous tool, so a program is run again instead.
;; A call that must wait ends the run; once its result is there, the
;; program runs from the start, and the calls it made before get the
;; results they had, without running again.  Its own computations
;; being free of side effects, a run replays the one before; if what it
;; reads changed meanwhile, the calls it then makes are new calls,
;; under the policy like the first.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-lisp-check)

;;;; Checks as it runs

(defvar lispm-lisp--owned (make-hash-table :test #'eq :weakness 'key)
  "Objects programs made, which they may call or change: closures, buffers, tables.")

(defvar lispm-lisp--run nil
  "The run of the program being evaluated, while it is.")

(defun lispm-lisp-current-run ()
  "Return the currently executing `lispm-lisp-run', or nil."
  lispm-lisp--run)

(defvar lispm-lisp--pass-tag nil
  "The catch tag that ends the pass under way of a program.")

(defun lispm-lisp-closure (closure)
  "Note that the program made CLOSURE, from a lambda checked; return it."
  (puthash closure 'closure lispm-lisp--owned)
  closure)

(defun lispm-lisp--function (function &optional optional)
  "FUNCTION, as a function the program may pass on to be called.
A lambda of the program's; a function it may call, wrapped in its
checks if it has any; or a tool of its, called as a program calls it.
If OPTIONAL, FUNCTION is an argument that may be something else than
a function, which is passed on as it is: a string, nil, a keyword."
  (cond ((eq (gethash function lispm-lisp--owned) 'closure) function)
        ((and optional (not (functionp function))
              (or (not (symbolp function)) (null function) (keywordp function)))
         function)
        ((and (symbolp function) (memq function lispm-lisp-tools))
         (lambda (&rest args) (apply #'lispm-lisp--call function args)))
        ((and (symbolp function) (assq function lispm-lisp-guarded))
         (lambda (&rest args) (apply #'lispm-lisp-apply function args)))
        ((and function (lispm-lisp-allowed-p function)) function)
        (t (lispm-lisp-refuse "calling %S" function))))

(defun lispm-lisp--own (object)
  "Note that the running program made OBJECT, a buffer or a hash table; return it."
  (when object
    (puthash object lispm-lisp--run lispm-lisp--owned)
    (when (bufferp object)
      (push object (lispm-lisp-run-buffers lispm-lisp--run))))
  object)

(defun lispm-lisp--owned-p (object)
  "Non-nil if the running program made OBJECT."
  (and object lispm-lisp--run (eq (gethash object lispm-lisp--owned) lispm-lisp--run)))

(defun lispm-lisp--check-buffer (kind function)
  "Refuse FUNCTION, of KIND, unless the program may use it in this buffer."
  (unless (or (lispm-lisp--owned-p (current-buffer))
              (and (eq kind 'motion) (memq (current-buffer) lispm-lisp-excursions))
              (and (eq kind 'narrow) (memq (current-buffer) lispm-lisp-restrictions)))
    (lispm-lisp-refuse "%s in %s, a buffer the program did not make%s" function (buffer-name)
                        (pcase kind
                          ('motion " (other than inside save-excursion)")
                          ('narrow " (other than inside save-restriction)")
                          (_ "")))))

(defun lispm-lisp-apply (function &rest args)
  "Call FUNCTION on ARGS with the checks `lispm-lisp-guarded' gives it."
  (let ((spec (alist-get function lispm-lisp-guarded)))
    (dolist (n (plist-get spec :functions))
      (when (nthcdr n args)
        (setf (nth n args) (lispm-lisp--function (nth n args) t))))
    (when-let* ((keys (plist-get spec :keys)))
      (let ((tail (nthcdr (car keys) args)))
        (while tail
          (when (and (memq (car tail) (cdr keys)) (cdr tail))
            (setcar (cdr tail) (lispm-lisp--function (cadr tail) t)))
          (setq tail (cdr tail)))))
    (when-let* ((kind (plist-get spec :buffer)))
      (unless (and (plist-get spec :unless) (nth (plist-get spec :unless) args))
        (lispm-lisp--check-buffer kind function)))
    (when-let* ((n (plist-get spec :buffer-arg)))
      (let ((buffer (if (nth n args) (get-buffer (nth n args)) (current-buffer))))
        (unless (and buffer (lispm-lisp--owned-p buffer))
          (lispm-lisp-refuse "%s on a buffer the program did not make" function))))
    (when-let* ((n (plist-get spec :table)))
      (unless (lispm-lisp--owned-p (nth n args))
        (lispm-lisp-refuse "%s on a hash table the program did not make" function)))
    (when (and (plist-get spec :throw) (not (memq (car args) lispm-lisp-tags)))
      (lispm-lisp-refuse "throwing to %S, which the program did not catch" (car args)))
    (dolist (n (plist-get spec :nil))
      (when (nth n args)
        (lispm-lisp-refuse "%s with argument %d: it would print elsewhere than to the \
program's output, or change the object given" function (1+ n))))
    (let ((value (apply (or (plist-get spec :use) function) args)))
      (if (plist-get spec :owned) (lispm-lisp--own value) value))))

(defun lispm-lisp-delete-dups (list)
  "LIST without its duplicates, LIST unchanged."
  (delete-dups (copy-sequence list)))

(defun lispm-lisp-set-match-data (list &optional _reseat)
  "Set the match data from LIST, its markers unchanged."
  (set-match-data list))

(defun lispm-lisp-plist-put (plist &rest args)
  "A copy of PLIST with what `plist-put' with ARGS puts in it; PLIST unchanged."
  (apply #'plist-put (copy-sequence plist) args))

(defun lispm-lisp-mapcan (function sequence)
  "The results of FUNCTION on SEQUENCE's elements, appended; none changed."
  (apply #'append (mapcar function sequence)))

(defun lispm-lisp-set-buffer (buffer-or-name)
  "Select BUFFER-OR-NAME if owned by the program or a non-internal buffer."
  (let ((buffer (and buffer-or-name (get-buffer buffer-or-name))))
    (unless (and buffer
                 (or (lispm-lisp--owned-p buffer)
                     (not (string-prefix-p " " (buffer-name buffer)))))
      (lispm-lisp-refuse "set-buffer on %S, an internal buffer" buffer-or-name))
    (set-buffer buffer)))

(defun lispm-lisp-sort (sequence &rest args)
  "A sorted copy of SEQUENCE, as `sort' with ARGS sorts."
  (apply #'sort (copy-sequence sequence) args))

;;;; Calling tools


(defun lispm-lisp--input (tool args)
  "The input of a call of TOOL with the keyword ARGS, as the model gives one.
A nil value leaves its argument out."
  (let (input)
    (while args
      (let ((key (pop args)))
        (unless (and (keywordp key) args)
          (error "Call %s with keyword arguments, as (%s :path \"a.el\")" tool tool))
        (let ((value (pop args)))
          (when value
            (push (cons (lispm-tools-key (intern (substring (symbol-name key) 1)))
                        (lispm-lisp-json value tool))
                  input)))))
    (nreverse input)))

(defun lispm-lisp--outcome (entry &optional index)
  "What the call ENTRY (at INDEX) of a program's log gives the program."
  (pcase-let ((`(,name ,_input ,outcome ,text) entry))
    (pcase outcome
      ('value text)
      ('denied (signal 'lispm-call-denied
                       (list (propertize (format "%s: %s" name text)
                                         'lispm-call-index index))))
      (_ (signal 'lispm-call-failed
                 (list (propertize (format "%s: %s" name text)
                                   'lispm-call-index index)))))))

(defun lispm-lisp--record (run index name input outcome text)
  "Record in RUN's log that its call INDEX, of NAME on INPUT, had OUTCOME and TEXT."
  (let ((log (lispm-lisp-run-log run))
        (entry (list name input outcome text)))
    (if (< index (length log))
        (setf (nth index (lispm-lisp-run-log run)) entry)
      (setf (lispm-lisp-run-log run)
            (append (seq-take log index) (list entry))))))

(defun lispm-lisp--invoke (run tool input callback)
  "Run TOOL on INPUT as a call of RUN's agent; CALLBACK gets TEXT and ERRORP.
The call runs as one the model made does, whatever the program bound."
  (let ((lispm-current-agent (lispm-lisp-run-agent run))
        (lispm-current-call nil)
        (default-directory (lispm-agent-root (lispm-lisp-run-agent run)))
        (file-name-handler-alist (lispm-lisp-run-handlers run))
        (standard-output t)
        (lispm-lisp--run nil))
    (lispm-tool-invoke tool input (lambda (text errorp &optional _images)
                                    (funcall callback text errorp)))))

(defun lispm-lisp--stop (run how)
  "End RUN's pass under way at a call, as HOW says (see `lispm-lisp--pass')."
  (setf (lispm-lisp-run-stopping run) how)
  (throw lispm-lisp--pass-tag how))

(defun lispm-lisp-stopping-p ()
  "Non-nil when the current `run_lisp' pass is suspending for approval or wait."
  (and lispm-lisp--run (lispm-lisp-run-stopping lispm-lisp--run) t))

(defun lispm-lisp-current-index ()
  "Return the current tool-call index in `lispm-lisp--run', or nil."
  (and lispm-lisp--run (lispm-lisp-run-index lispm-lisp--run)))

(defun lispm-lisp-note-unwind (from-index)
  "Mark calls from FROM-INDEX on as unwound so retry re-enters the bracket."
  (when (and lispm-lisp--run (integerp from-index))
    (let ((prev (lispm-lisp-run-unwound-from lispm-lisp--run)))
      (setf (lispm-lisp-run-unwound-from lispm-lisp--run)
            (if prev (min prev from-index) from-index)))))

(defun lispm-lisp--call-input (name tool input)
  "Execute or replay call of TOOL (named NAME) with decoded JSON INPUT."
  (let* ((run (or lispm-lisp--run (error "No program is running")))
         (_ (when-let* ((how (lispm-lisp-run-stopping run)))
              (throw lispm-lisp--pass-tag how)))
         (index (lispm-lisp-run-index run))
         (entry (nth index (lispm-lisp-run-log run))))
    (when (>= index lispm-lisp-max-calls)
      (lispm-lisp-refuse "more than %d tool calls" lispm-lisp-max-calls))
    (setf (lispm-lisp-run-index run) (1+ index))
    (if (and entry (equal (car entry) name) (equal (nth 1 entry) input)
             (not (eq (nth 2 entry) 'retry)))
        (lispm-lisp--outcome entry index)
      ;; If the program takes a different branch than before, discard the old tail.
      (unless (and entry (equal (car entry) name) (equal (nth 1 entry) input))
        (setf (lispm-lisp-run-log run) (seq-take (lispm-lisp-run-log run) index)))
      (let ((agent (lispm-lisp-run-agent run)))
        (pcase (lispm-tool-decision agent tool input)
          (`(auto . ,_)
           (let ((running t) result)
             (lispm-lisp--invoke run tool input
                                 (lambda (text errorp)
                                   (if running
                                       (setq result (list text errorp))
                                     (lispm-lisp--settle run index name input text errorp))))
             (setq running nil)
             (cond ((null result) (lispm-lisp--stop run '(:wait)))
                   ((equal (car result) lispm-tools-interrupted) (signal 'quit nil))
                   (t (lispm-lisp--record run index name input
                                          (if (nth 1 result) 'failed 'value) (car result))
                      (lispm-lisp--outcome (nth index (lispm-lisp-run-log run)) index)))))
          (`(ask . ,reasons) (lispm-lisp--stop run (list :ask index tool input reasons)))
          (`(,_ . ,reasons)
           (lispm-lisp--record run index name input 'denied (lispm-tools-denial agent tool reasons))
           (lispm-lisp--outcome (nth index (lispm-lisp-run-log run)) index)))))))

(defun lispm-lisp-call-pairs (name pairs)
  "Call tool NAME from precompiled `(JSON-KEY . VALUE)' PAIRS."
  (let* ((run (or lispm-lisp--run (error "No program is running")))
         (_ (when-let* ((how (lispm-lisp-run-stopping run)))
              (throw lispm-lisp--pass-tag how)))
         (tool (or (lispm-get-tool name) (error "No tool %s" name)))
         (tname (lispm-tool-name tool))
         input)
    (dolist (pair pairs)
      (when (cdr pair)
        (push (cons (car pair) (lispm-lisp-json (cdr pair) tname)) input)))
    (lispm-lisp--call-input tname tool (nreverse input))))

(defun lispm-lisp--call (name &rest args)
  "Call the tool NAME with the keyword ARGS, from the running program.
Its result text is returned, or a failure signaled; a call made by
the run before gets the result it had then."
  (let* ((run (or lispm-lisp--run (error "No program is running")))
         ;; Cleanups of the program, as its pass ends at a call, make
         ;; no more calls.
         (_ (when-let* ((how (lispm-lisp-run-stopping run)))
              (throw lispm-lisp--pass-tag how)))
         (tool (or (lispm-get-tool name) (error "No tool %s" name)))
         (tname (lispm-tool-name tool)))
    (lispm-lisp--call-input tname tool (lispm-lisp--input tname args))))

;;;; Running a program

(defun lispm-lisp--pass (run)
  "Evaluate RUN's program once, the calls it made before getting their results.
Return (:value VALUE) if it ran to its end, (:ask INDEX TOOL INPUT
REASONS) if it stopped at a call awaiting approval, REASONS as
`lispm-tool-decision' gives them, or (:wait) at one under way.  The
program's errors are signaled."
  (let ((output (generate-new-buffer " *lispm-lisp*" t))
        (tag (list 'lispm-lisp-pass)))
    (setf (lispm-lisp-run-index run) 0
          (lispm-lisp-run-buffers run) nil
          (lispm-lisp-run-stopping run) nil
          (lispm-lisp-run-unwound-from run) nil)
    (unwind-protect
        (catch tag
          (let ((lispm-lisp--run run)
                (lispm-lisp--pass-tag tag)
                (lispm-lisp-tools (lispm-lisp-run-tools run))
                (lispm-lisp-tags nil)
                (lispm-lisp-excursions nil)
                (lispm-lisp-restrictions nil)
                (standard-output output)
                (file-name-handler-alist nil)
                (default-directory (lispm-agent-root (lispm-lisp-run-agent run)))
                value)
            (save-current-buffer
              (save-match-data
                (dolist (form (lispm-lisp-run-forms run))
                  (setq value (eval form t)))))
            (list :value value)))
      (setf (lispm-lisp-run-output run)
            (with-current-buffer output (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output)
      (dolist (buffer (lispm-lisp-run-buffers run))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun lispm-lisp--live-p (run)
  "Non-nil if RUN goes on: unfinished, and its call not stopped."
  (and (not (lispm-lisp-run-finished run))
       (or (null (lispm-lisp-run-id run))
           (eq (gethash (lispm-lisp-run-id run)
                        (lispm-agent-pending (lispm-lisp-run-agent run)))
               (lispm-lisp-run-call run)))))

(defun lispm-lisp--calls-text (run)
  "The calls RUN's program made, a line each, as its result lists them."
  (when-let* ((log (lispm-lisp-run-log run)))
    (concat "Tool calls:\n"
            (mapconcat (pcase-lambda (`(,name ,input ,outcome ,_text))
                         (let ((summary (lispm-transcript-summary input)))
                           (format "- %s%s%s" name (if summary (concat " " summary) "")
                                   (pcase outcome ('value "") (_ (format " (%s)" outcome))))))
                       (seq-take log 30) "\n")
            (if (nthcdr 30 log) (format "\n- ... %d more" (- (length log) 30)) "")
            "\n\n")))

(defun lispm-lisp--saved-log (run orig-index)
  "Saved call log for RUN when an error originated at call ORIG-INDEX."
  (let* ((unwound (lispm-lisp-run-unwound-from run))
         (cap (if unwound
                  (min unwound (lispm-lisp-run-index run))
                (lispm-lisp-run-index run)))
         (log (mapcar #'copy-sequence (seq-take (lispm-lisp-run-log run) cap)))
         (len (length log)))
    (cond
     ((and unwound (or (null orig-index) (>= orig-index unwound))) log)
     ((null orig-index) log)
     ((= orig-index (1- len)) (butlast log))
     ((and (>= orig-index 0) (< orig-index len))
      (setf (nth 2 (nth orig-index log)) 'retry)
      log)
     (t log))))

(defun lispm-lisp--finish (run text errorp &optional saved-log)
  "End RUN with the result TEXT; ERRORP if it failed.
When RUN fails in a turn, store SAVED-LOG on its pending call record
\(`:lisp-log') so a `retry' restart replays earlier completed and
handled steps without repeating their side effects."
  (setf (lispm-lisp-run-finished run) t)
  (when-let* ((call (lispm-lisp-run-call run)))
    (plist-put call :lisp-log (and errorp saved-log)))
  (funcall (lispm-lisp-run-done run) (substring-no-properties text) errorp))

(defun lispm-lisp--step (run)
  "Run RUN's program, again if it ran before, and go on as it ends."
  (when (lispm-lisp--live-p run)
    (pcase (condition-case err
               (lispm-tools-interruptibly #'lispm-lisp--pass (list run))
             ((lispm-lisp-refused error)
              (let ((orig (and (memq (car err) '(lispm-call-failed lispm-call-denied))
                               (stringp (cadr err))
                               (get-text-property 0 'lispm-call-index (cadr err)))))
                (list :error (lispm-lisp-error-text err) orig)))
             (quit (list :quit)))
      (`(:value ,value)
       (setf (lispm-lisp-run-log run)
             (seq-take (lispm-lisp-run-log run) (lispm-lisp-run-index run)))
       (lispm-lisp--finish run (concat (lispm-lisp--calls-text run)
                                       (lispm-tools-eval-result
                                        (lispm-lisp-run-output run)
                                        (if (stringp value) (substring-no-properties value) value)))
                           nil))
      (`(:ask ,index ,tool ,input ,reasons) (lispm-lisp--ask run index tool input reasons))
      (`(:wait))
      (`(:error ,message ,orig)
       (setf (lispm-lisp-run-log run)
             (seq-take (lispm-lisp-run-log run) (lispm-lisp-run-index run)))
       (lispm-lisp--finish run (concat (lispm-lisp--calls-text run) message)
                           t (lispm-lisp--saved-log run orig)))
      (_ (lispm-lisp--finish run lispm-tools-interrupted t)))))

(defun lispm-lisp--again (run)
  "Run RUN's program again soon, from the event loop."
  (run-at-time 0 nil #'lispm-lisp--step run))

(defun lispm-lisp--settle (run index name input text errorp)
  "Take the result TEXT of RUN's call INDEX, of NAME on INPUT, that ran on.
ERRORP marks a failure.  Then the program runs again."
  (when (lispm-lisp--live-p run)
    (if (equal text lispm-tools-interrupted)
        (lispm-lisp--finish run text t)
      (lispm-lisp--record run index name input (if errorp 'failed 'value) text)
      (lispm-lisp--again run))))

(defun lispm-lisp--ask (run index tool input &optional reasons)
  "Have RUN's call INDEX, of TOOL on INPUT, await the user's approval.
REASONS are why it asks, if a check said (see `lispm-tool-decision').
Once approved it runs, once denied it fails, and the program runs again."
  (let ((name (lispm-tool-name tool)))
    (unless (and (lispm-lisp-run-id run)
                 (lispm-call-ask
                  (lispm-lisp-run-agent run) (lispm-lisp-run-id run) tool input
                  (lambda (decision denial)
                    (when (lispm-lisp--live-p run)
                      (if (memq decision '(approve allow))
                          (lispm-lisp--invoke run tool input
                                              (lambda (text errorp)
                                                (lispm-lisp--settle run index name input
                                                                    text errorp)))
                        (lispm-lisp--record run index name input 'denied denial)
                        (lispm-lisp--again run))))
                  nil reasons))
      (lispm-lisp--record run index name input 'denied
                          "It needs the user's approval, which is asked only in a turn.")
      (lispm-lisp--again run))))

(defun lispm-lisp--tool-names (agent)
  "The tools AGENT's programs may call, as symbols: those its session offers."
  (mapcar #'intern
          (delete "run_lisp"
                  (if (buffer-live-p (lispm-agent-buffer agent))
                      (mapcar (lambda (schema) (alist-get 'name schema))
                              (lispm-session-tools agent))
                    (mapcar #'lispm-tool-name (lispm-agent-tools* agent))))))

(lispm-deftool run_lisp
    ((code string "Emacs Lisp forms, evaluated in order with lexical binding."))
  "Run an Emacs Lisp program that calls your tools as functions, to do in
one step what takes many calls: read ten files, filter them, edit three.
Call a tool with keyword arguments: (read_file :path \"a.el\"),
\(edit_file :path \"a.el\" :old-text \"x\" :new-text \"y\").  It returns
the tool's result text, or signals lispm-call-failed (lispm-call-denied
when refused), which condition-case can handle.  Tool macros handle
scoped lifecycles, code quoting, fallbacks and edits:
- (ignoring_call_denied BODY...) and (with_tool_fallback PRIMARY FALLBACK...)
- (transform_file (TEXT PATH) BODY...): read PATH unnumbered into TEXT and
  edit_file only when BODY returns changed text
- (run_elisp_forms [(:bind (VAR (K EXPR)) :timeout SEC)] BODY...) and
  (eval_elisp_forms [(:bind ...)] BODY...): pass S-expressions without string escaping
- (with_terminal BODY...) and (with_worktree (VAR :role R :request Q ...) BODY...)
Each call goes through your permission policy as if you made it: reads run
at once, a write waits for the user's approval.  Otherwise a program only
computes: the special forms, common macros (let, dolist, pcase, cl-loop,
when-let*, with-temp-buffer), and functions free of side effects (strings,
lists, regexps, seq-*, file names, reading buffers and directories).
Functions passed to mapcar and the like must be lambdas or such functions;
it may not set global variables or change data in place, and changes only
buffers it makes.  Use eval_elisp for anything else.  Returns the calls
made, what the program printed and its last value.
Computation still runs in the user's Emacs: keep it small and bounded.
Prefer direct parallel edit_file tool calls over sequential edit_file
calls in run_lisp when making multiple edits to one file, as run_lisp
does not roll back earlier edits if a later call fails.
Use run_elisp for standalone processing in a separate process launched by
LispM Kernel; this tool does not time-slice Lisp loops."
  :permission lisp
  :async t
  (let ((agent (or lispm-current-agent (error "run_lisp runs programs of agents"))))
    (condition-case err
        (let* ((tools (lispm-lisp--tool-names agent))
               (forms (let ((lispm-lisp-tools tools)) (lispm-lisp-prepare code)))
               (id lispm-current-call)
               (call (and id (gethash id (lispm-agent-pending agent)))))
          (lispm-lisp--step
           (lispm-lisp-run-create
            :agent agent :id id :call call
            :log (and call (plist-get call :lisp-log))
            :forms forms :tools tools :handlers file-name-handler-alist :done done)))
      (lispm-lisp-refused (funcall done (lispm-lisp-error-text err) t)))))

(provide 'lispm-lisp)
;;; lispm-lisp.el ends here
