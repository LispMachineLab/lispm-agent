;;; lispm-lisp-check.el --- Static checker and allowlist for run_lisp  -*- lexical-binding: t; -*-

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

;; Conditions, allowlists, AST checking/rewriting, and JSON
;; argument serialization for `run_lisp' programs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'byte-opt)                     ; the `side-effect-free' declarations
(require 'lispm-core)
(require 'lispm-tools)

(defcustom lispm-lisp-max-calls 200
  "Most tool calls a program run with run_lisp may make."
  :type 'integer
  :group 'lispm)

(define-error 'lispm-call-failed "Tool call failed" 'lispm-tool-error)
(define-error 'lispm-call-denied "Tool call denied" 'lispm-call-failed)
(define-error 'lispm-lisp-serialization-error "Cannot serialize value for tool" 'lispm-tool-error)
(define-error 'lispm-lisp-type-error "Invalid value returned to macro tool" 'lispm-tool-error)

;; Not an `error': a program cannot handle it, and goes no further.
(put 'lispm-lisp-refused 'error-conditions '(lispm-lisp-refused))
(put 'lispm-lisp-refused 'error-message "Not allowed in run_lisp")

(defun lispm-lisp-refuse (format &rest args)
  "Refuse to run a program, for the reason FORMAT and ARGS make."
  (signal 'lispm-lisp-refused (list (apply #'format-message format args))))

(defun lispm-lisp-error-text (err)
  "The error ERR of a program, as its result says it."
  (if (memq (car err) '(lispm-lisp-refused lispm-call-failed lispm-call-denied))
      (format "%s: %s" (get (car err) 'error-message) (cadr err))
    (error-message-string err)))

(cl-defstruct (lispm-lisp-run (:constructor lispm-lisp-run-create) (:copier nil))
  "A program run with run_lisp: its AGENT, its call ID (the call plist
CALL), its FORMS, checked, the TOOLS it may call.  LOG holds the calls
it made, each (NAME INPUT OUTCOME TEXT), OUTCOME being `value',
`failed' or `denied'; INDEX counts the calls of the pass under way.
HANDLERS are the file name handlers it found, given back to tools;
BUFFERS are those it made in the pass under way; STOPPING is how the
pass under way ends, once it stops at a call; OUTPUT is what its last
pass printed; DONE gets its result, once it is FINISHED."
  agent id call forms tools log (index 0) handlers buffers output done finished
  stopping unwound-from)

(defvar lispm-lisp-tags nil
  "Catch tags of the running program's own catches, innermost first.")

(defvar lispm-lisp-excursions nil
  "Buffers the running program is in a `save-excursion' of.")

(defvar lispm-lisp-restrictions nil
  "Buffers the running program is in a `save-restriction' of.")


;;;; What a program may do

(defconst lispm-lisp--macros
  '(when unless dolist dotimes push pop lambda prog2 \` letrec
    if-let if-let* when-let when-let* and-let* while-let
    pcase pcase-let pcase-let* pcase-dolist pcase-exhaustive pcase-setq seq-let seq-doseq
    cl-incf cl-decf cl-callf incf decf setf ignore-errors ignore-error save-match-data
    with-temp-buffer with-current-buffer let-alist thread-first thread-last
    cl-loop cl-case cl-ecase cl-dolist cl-dotimes cl-block cl-return cl-return-from
    cl-destructuring-bind cl-function cl-flet cl-flet* cl-labels named-let rx)
  "Macros a program may use.  Their expansions are checked in turn.")

(defconst lispm-lisp--early-macros
  '(cl-function cl-flet cl-flet* cl-labels rx
    setf push pop cl-incf cl-decf cl-callf incf decf
    pcase pcase-let pcase-let* pcase-dolist pcase-exhaustive pcase-setq cl-loop)
  "Macros of `lispm-lisp--macros' expanding subforms during macroexpansion.
Before one is expanded, its forms are scanned for disallowed macros and places.")

(defconst lispm-lisp--pcase-patterns
  '(\` cl-struct cl-type let map rx seq)
  "Built-in `pcase' pattern macroexpanders allowed in `run_lisp'.")

(defconst lispm-lisp--never '(eval eval-when-compile eval-and-compile cl-eval-when
                              load-time-value cl-load-time-value)
  "Heads of forms that evaluate code as they are expanded or run.")

(defconst lispm-lisp--functions
  '(;; Strings and regexps: they set the match data, as searches do.
    string-match split-string string-trim string-trim-left string-remove-prefix
    string-remove-suffix string-greaterp truncate-string-to-width looking-at looking-back
    intern
    ;; Lists and JSON: Emacs declares these only from 31 on.
    caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    cl-oddp cl-evenp json-parse-string json-serialize
    ;; Sequences.
    seqp seq-elt seq-length seq-first seq-rest seq-empty-p seq-take seq-drop seq-subseq
    seq-reverse seq-concatenate seq-into seq-partition seq-split seq-min seq-max
    seq-random-elt seq--elt-safe
    ;; File names, and reading directories: name handlers are off.
    expand-file-name file-name-directory file-name-nondirectory file-name-extension
    file-name-sans-extension file-name-base file-relative-name file-name-as-directory
    directory-file-name file-name-split file-name-parent-directory abbreviate-file-name
    file-truename file-attributes directory-files file-expand-wildcards get-file-buffer
    file-attribute-type file-attribute-size file-attribute-modification-time
    file-size-human-readable
    ;; Errors.
    error user-error signal)
  "Functions a program may call besides those declared free of side effects.")

(defconst lispm-lisp--unsafe-functions
  '(;; They run code, or make functions of it.
    documentation apply-partially make-byte-code
    ;; They read environment, globals, processes, secrets, or hidden buffers.
    getenv getenv-internal symbol-value default-value default-toplevel-value
    buffer-local-value buffer-local-variables symbol-plist
    process-list get-process process-command process-contact process-plist
    process-get process-buffer get-buffer-process
    network-interface-list network-interface-info all-threads
    system-name user-login-name user-real-login-name user-original-login-name
    user-full-name user-uid user-real-uid group-gid group-real-gid command-line-args
    symbol-function
    frame-parameters frame-parameter terminal-parameters terminal-parameter
    current-kill gui-get-selection x-get-selection-internal
    obarray-get mapatoms buffer-list
    ;; They tell what the user types.
    recent-keys this-command-keys this-command-keys-vector this-single-command-keys
    this-single-command-raw-keys minibuffer-contents minibuffer-contents-no-properties
    current-message
    ;; It makes text that runs code when displayed.
    propertize
    ;; It holds Emacs up.
    sleep-for)
  "Functions declared free of side effects that a program may not call.")

(defconst lispm-lisp--bindable
  '(case-fold-search print-length print-level print-escape-newlines print-circle print-quoted)
  "Variables of Emacs's a program may bind: they change only how it computes.")

(defconst lispm-lisp--readable-globals
  '(most-positive-fixnum most-negative-fixnum float-e float-pi
    default-directory system-type major-mode buffer-file-name)
  "Global constants and benign buffer-context symbols a program may read.")

(defconst lispm-lisp--unhandled '(t quit minibuffer-quit debug lispm-lisp-refused)
  "Conditions a program may not handle: C-g must stop it, and refusals end it.")

(defconst lispm-lisp-guarded
  '(;; Higher-order: the arguments at these positions are called.
    (funcall :functions (0)) (apply :functions (0))
    (mapcar :functions (0)) (mapc :functions (0)) (mapconcat :functions (0))
    (maphash :functions (0))
    (seq-map :functions (0)) (seq-mapn :functions (0)) (seq-map-indexed :functions (0))
    (seq-do :functions (0)) (seq-filter :functions (0)) (seq-remove :functions (0))
    (seq-keep :functions (0)) (seq-reduce :functions (0)) (seq-find :functions (0))
    (seq-some :functions (0)) (seq-every-p :functions (0)) (seq-count :functions (0))
    (seq-take-while :functions (0)) (seq-drop-while :functions (0))
    (seq-mapcat :functions (0)) (seq-group-by :functions (0))
    (seq-sort :functions (0)) (seq-sort-by :functions (0 1))
    (seq-uniq :functions (1)) (seq-contains-p :functions (2)) (seq-position :functions (2))
    (seq-positions :functions (2)) (seq-difference :functions (2))
    (seq-union :functions (2)) (seq-intersection :functions (2))
    (seq-set-equal-p :functions (2))
    (assoc :functions (2)) (alist-get :functions (4)) (assoc-default :functions (2))
    (plist-get :functions (2)) (plist-member :functions (2))
    (replace-regexp-in-string :functions (1)) (locate-dominating-file :functions (1))
    (find-buffer-visiting :functions (1))
    ;; Destructive: they work on a copy.
    (nreverse :use reverse) (nconc :use append) (delete :use remove) (delq :use remq)
    (nbutlast :use butlast) (ntake :use take)
    (delete-dups :use lispm-lisp-delete-dups)
    (plist-put :functions (3) :use lispm-lisp-plist-put)
    (match-data :nil (1)) (set-match-data :use lispm-lisp-set-match-data)
    (mapcan :functions (0) :use lispm-lisp-mapcan)
    (sort :functions (1) :keys (1 :key :lessp) :use lispm-lisp-sort)
    ;; Objects it makes, it may change.
    (generate-new-buffer :owned t) (make-hash-table :owned t) (copy-hash-table :owned t)
    (kill-buffer :buffer-arg 0)
    (puthash :table 2) (remhash :table 1) (clrhash :table 0)
    (insert :buffer change) (insert-char :buffer change)
    (insert-buffer-substring :buffer change) (delete-region :buffer change)
    (delete-char :buffer change) (erase-buffer :buffer change)
    (delete-and-extract-region :buffer change) (replace-match :buffer change :unless 3)
    (goto-char :buffer motion) (forward-line :buffer motion) (forward-char :buffer motion)
    (backward-char :buffer motion) (beginning-of-line :buffer motion)
    (end-of-line :buffer motion) (forward-word :buffer motion) (backward-word :buffer motion)
    (re-search-forward :buffer motion) (re-search-backward :buffer motion)
    (search-forward :buffer motion) (search-backward :buffer motion)
    (skip-chars-forward :buffer motion) (skip-chars-backward :buffer motion)
    (narrow-to-region :buffer narrow) (widen :buffer narrow)
    ;; Control and output: to its own catches, and its own output.
    (set-buffer :use lispm-lisp-set-buffer)
    (throw :throw t) (cl--block-throw :throw t)
    (print :nil (1)) (prin1 :nil (1)) (princ :nil (1)) (terpri :nil (0)))
  "Functions a program may call with checks as it runs, and the checks.
Each entry is (FUNCTION . PROPS):
  :functions POSITIONS  the arguments at POSITIONS are called: they
                        must be functions the program may call
  :keys (FROM . KEYS)   so are the values of KEYS, from argument FROM on
  :use FUNCTION         call FUNCTION instead, which copies what the
                        other changes
  :owned t              the program made the value, and may change it
  :buffer KIND          the current buffer must be the program's own;
                        for `motion', or one in a `save-excursion' of
                        the program's, for `narrow', in a
                        `save-restriction'; :unless N, unless the Nth
                        argument is given
  :buffer-arg N         the Nth argument (by default the current buffer)
                        must be a buffer the program made
  :table N              the Nth argument must be a hash table it made
  :throw t              the tag must be caught by the program
  :nil POSITIONS        the arguments at POSITIONS must be nil: where to
                        print, a list to fill in")

(defun lispm-lisp-allowed-p (function)
  "Non-nil if a program may call FUNCTION, a symbol, without checks."
  (and (symbolp function) (fboundp function)
       (not (memq function lispm-lisp--unsafe-functions))
       (or (memq function lispm-lisp--functions)
           (and (not (string-prefix-p "lispm-" (symbol-name function)))
                (or (function-get function 'side-effect-free)
                    (function-get function 'pure)
                    (eq (get function 'safe-function) t))))))

;;;; Checking a program

(defvar lispm-lisp-tools nil
  "Names of the tools the program being checked, or run, may call, as symbols.")

(defconst lispm-lisp--internal (make-symbol "lispm-lisp-internal")
  "Unforgeable head marking runtime helper calls emitted by `lispm-lisp-defmacro'.")

(defun lispm-lisp-internal-call (fn &rest args)
  "Construct an unforgeable internal helper call to FN with ARGS."
  `(,lispm-lisp--internal ,fn ,@args))

(defun lispm-lisp-require-tools (macro required)
  "Refuse MACRO unless every symbol in REQUIRED is allowed in this run."
  (dolist (tool required)
    (unless (memq tool lispm-lisp-tools)
      (lispm-lisp-refuse "%s requires the tool %s" macro tool))))

(defvar lispm-lisp--macro-tools (make-hash-table :test #'eq)
  "Macro tools in `run_lisp': symbol -> pure syntax expander `(ARGS -> FORM)'.")

(defmacro lispm-lisp-defmacro (name arglist docstring &rest body)
  "Define a macro tool NAME for `run_lisp' with ARGLIST `(ARGS)' and BODY."
  (declare (indent 2) (doc-string 3)
           (debug (&define name sexp stringp def-body)))
  `(puthash ',name (lambda ,arglist ,docstring ,@body) lispm-lisp--macro-tools))

(defun lispm-lisp--null-form-p (form)
  "Non-nil if FORM is literally `nil' or `(quote nil)'."
  (or (null form) (equal form '(quote nil))))

(defun lispm-lisp--constant-p (form)
  "Non-nil if FORM is a self-evaluating literal or a quoted datum."
  (or (stringp form) (numberp form) (vectorp form) (memq form '(t :false nil))
      (and (consp form) (eq (car form) 'quote) (consp (cdr form)) (null (cddr form)))))

(defun lispm-lisp--check-tool-arg (head param key val-form)
  "Validate literal VAL-FORM against PARAM (`( _ TYPE _ . OPTS)') of tool HEAD."
  (when (and param (lispm-lisp--constant-p val-form))
    (let ((lit (if (consp val-form) (cadr val-form) val-form)))
      (when lit
        (pcase-let ((`(,_arg ,type ,_doc . ,opts) param))
          (condition-case err
              (lispm-tools-argument key type (lispm-lisp-json lit head) opts)
            (error (lispm-lisp-refuse "%s: %s" head (error-message-string err)))))))))

(defun lispm-lisp--expand-tool-call (head args scope)
  "Validate and rewrite a call `(HEAD . ARGS)' of tool HEAD in SCOPE."
  (unless (and (proper-list-p args) (cl-evenp (length args)))
    (lispm-lisp-refuse "calling %s without keyword arguments, as (%s :path \"a.el\")"
                        head head))
  (let* ((tool (lispm-get-tool head))
         (schema-p (and tool (lispm-tool-input-schema tool)))
         (params (and tool (not schema-p) (lispm-tool-params tool)))
         (param-alist (mapcar (lambda (p) (cons (lispm-tools-key (car p)) p)) params))
         (tail args)
         given non-null pairs)
    (while tail
      (let ((kw (pop tail))
            (val-form (pop tail)))
        (unless (keywordp kw)
          (lispm-lisp-refuse "calling %s without keyword arguments, as (%s :path \"a.el\")"
                              head head))
        (let* ((key (lispm-tools-key (intern (substring (symbol-name kw) 1))))
               (param (assq key param-alist)))
          (when (memq key given)
            (lispm-lisp-refuse "%s: duplicate parameter %s" head kw))
          (push key given)
          (when (and tool (not schema-p) (not param) (not (eq key 'why)))
            (lispm-lisp-refuse "%s has no parameter %s" head kw))
          (lispm-lisp--check-tool-arg head (cdr param) key val-form)
          (unless (lispm-lisp--null-form-p val-form)
            (push key non-null))
          (push `(cons ',key ,(lispm-lisp--check val-form scope)) pairs))))
    (when (and tool (not schema-p))
      (dolist (p params)
        (unless (or (plist-get (cdddr p) :optional)
                    (memq (lispm-tools-key (car p)) non-null))
          (lispm-lisp-refuse "%s: missing required parameter :%s" head (car p)))))
    `(lispm-lisp-call-pairs ',head (list ,@(nreverse pairs)))))

(defun lispm-lisp--macro-env (&optional blocked)
  "Return a `macroexpand-all-environment' alist for early macro expansion.
Includes `lispm-lisp--macro-tools', refuses `lispm-lisp--never' plus BLOCKED
symbols found inside skipped code-tool forms, and guards `rx' against
eval smuggling inside enclosing early macros."
  (let (env)
    (maphash (lambda (name expander)
               (push (cons name (lambda (&rest args) (funcall expander args))) env))
             lispm-lisp--macro-tools)
    (dolist (sym (append lispm-lisp--never blocked))
      (let ((s sym))
        (push (cons s (lambda (&rest _) (lispm-lisp-refuse "%s" s))) env)))
    (push (cons 'rx (lambda (&rest args)
                      (let ((form (cons 'rx args)))
                        (lispm-lisp--scan form t)
                        (let ((macroexpand-all-environment nil))
                          (macroexpand-1 form)))))
          env)
    env))

(defun lispm-lisp--check-place (place)
  "Refuse PLACE unless it is a safe generalized variable place for mutation."
  (cond
   ((and (symbolp place) (not (memq place '(nil t))) (not (keywordp place)))
    place)
   ((and (consp place) (eq (car place) 'gethash)
         (proper-list-p place) (<= 3 (length place) 4))
    place)
   (t (lispm-lisp-refuse "place %S" place))))

(defun lispm-lisp--check-places (head args)
  "Refuse place-mutating macro `(HEAD . ARGS)' if any place is unsafe."
  (pcase head
    ('setf
     (unless (and (proper-list-p args) (cl-evenp (length args)))
       (lispm-lisp-refuse "%S" (cons head args)))
     (let ((tail args))
       (while tail
         (lispm-lisp--check-place (car tail))
         (setq tail (cddr tail)))))
    ('push
     (unless (and (proper-list-p args) (= (length args) 2))
       (lispm-lisp-refuse "%S" (cons head args)))
     (lispm-lisp--check-place (cadr args)))
    ('cl-callf
     (unless (and (proper-list-p args) (>= (length args) 2))
       (lispm-lisp-refuse "%S" (cons head args)))
     (lispm-lisp--check-place (cadr args)))
    ((or 'pop 'cl-incf 'cl-decf 'incf 'decf)
     (unless (and (proper-list-p args) (<= 1 (length args) 2))
       (lispm-lisp-refuse "%S" (cons head args)))
     (lispm-lisp--check-place (car args)))))

(defun lispm-lisp--scan (form &optional strict)
  "Refuse FORM if a disallowed macro is in it, and return skipped blocked symbols.
When STRICT is non-nil, do not skip `run_elisp_forms' or `eval_elisp_forms'."
  (let ((seen (make-hash-table :test #'eq))
        (count 0)
        skipped)
    (cl-labels ((walk (x in-skip)
                  (while (consp x)
                    (when (gethash x seen)
                      (lispm-lisp-refuse "a circular form"))
                    (puthash x t seen)
                    (when (> (cl-incf count) 100000)
                      (lispm-lisp-refuse "a program this large"))
                    (let ((head (car x)))
                      (cond
                       ((and (memq head '(cl-flet cl-flet* cl-labels))
                             (consp (cdr x)) (proper-list-p (cadr x)))
                        (unless in-skip
                          (dolist (def (cadr x))
                            (when (and (consp def) (symbolp (car def))
                                       (gethash (car def) lispm-lisp--macro-tools))
                              (lispm-lisp-refuse "redefining macro %s" (car def)))))
                        (walk head in-skip)
                        (setq x (cdr x)))
                       ((and (not strict) (not in-skip)
                             (memq head '(run_elisp_forms eval_elisp_forms))
                             (proper-list-p x))
                        ;; Scan any leading option plist strictly; record blocked
                        ;; macros in the worker body so outer early macros still refuse them.
                        (when (and (consp (cdr x)) (consp (cadr x)) (keywordp (caadr x)))
                          (walk (cadr x) nil))
                        (walk (if (and (consp (cdr x)) (consp (cadr x)) (keywordp (caadr x)))
                                  (cddr x)
                                (cdr x))
                              t)
                        (setq x nil))
                       (t
                        (when (and (symbolp head)
                                   (or (memq head lispm-lisp--never)
                                       (and (macrop head) (not (memq head lispm-lisp--macros))
                                            (not (and (not strict)
                                                      (gethash head lispm-lisp--macro-tools))))
                                       (and (get head 'pcase-macroexpander)
                                            (not (memq head lispm-lisp--pcase-patterns)))))
                          (if in-skip
                              (push head skipped)
                            (lispm-lisp-refuse "%s" head)))
                        (walk head in-skip)
                        (setq x (cdr x))))))))
      (walk form nil)
      (delete-dups skipped))))

(defun lispm-lisp-check-binder (symbol)
  "SYMBOL, if a program may bind it; else refuse."
  (unless (and symbol (symbolp symbol) (not (keywordp symbol)) (not (eq symbol t)))
    (lispm-lisp-refuse "binding %S" symbol))
  (when (and (special-variable-p symbol) (not (memq symbol lispm-lisp--bindable)))
    (lispm-lisp-refuse "binding %s, a variable of Emacs's: give yours another name" symbol))
  symbol)

(defun lispm-lisp--check-body (forms scope)
  "FORMS, checked as `lispm-lisp--check' does, with SCOPE."
  (unless (proper-list-p forms)
    (lispm-lisp-refuse "%S, which is not a list of forms" forms))
  (mapcar (lambda (form) (lispm-lisp--check form scope)) forms))

(defun lispm-lisp--check-lambda (lambda scope)
  "The (lambda ARGS . BODY) form LAMBDA, checked with SCOPE."
  (pcase lambda
    (`(lambda ,(and args (pred proper-list-p)) . ,body)
     (let ((vars (seq-remove (lambda (arg) (memq arg '(&optional &rest))) args)))
       (mapc #'lispm-lisp-check-binder vars)
       `(lambda ,args ,@(lispm-lisp--check-body body (append vars scope)))))
    (_ (lispm-lisp-refuse "the function %S" lambda))))

(defun lispm-lisp--check-let (head bindings body scope)
  "The form (HEAD BINDINGS . BODY), a `let' or `let*', checked with SCOPE."
  (unless (proper-list-p bindings)
    (lispm-lisp-refuse "the bindings %S" bindings))
  (let ((inner scope) checked)
    (dolist (binding bindings)
      (pcase binding
        ((pred symbolp)
         (push (lispm-lisp-check-binder binding) checked)
         (push binding inner))
        (`(,symbol . ,(and rest (guard (and (proper-list-p rest) (<= (length rest) 1)))))
         (lispm-lisp-check-binder symbol)
         (push (list symbol (lispm-lisp--check (car rest) (if (eq head 'let*) inner scope)))
               checked)
         (push symbol inner))
        (_ (lispm-lisp-refuse "the binding %S" binding))))
    `(,head ,(nreverse checked) ,@(lispm-lisp--check-body body inner))))

(defun lispm-lisp--check-setq (pairs scope)
  "The form (setq . PAIRS), checked with SCOPE: it sets the program's variables."
  (unless (cl-evenp (length pairs))
    (lispm-lisp-refuse "setq with an odd number of arguments"))
  (let (checked)
    (while pairs
      (let ((symbol (pop pairs))
            (value (pop pairs)))
        (unless (memq symbol scope)
          (lispm-lisp-refuse "setting %s, a variable the program did not bind: bind it with let"
                              symbol))
        (push symbol checked)
        (push (lispm-lisp--check value scope) checked)))
    `(setq ,@(nreverse checked))))

(defun lispm-lisp--check-condition-case (var body handlers scope)
  "The form (condition-case VAR BODY . HANDLERS), checked with SCOPE."
  (when var (lispm-lisp-check-binder var))
  `(condition-case ,var ,(lispm-lisp--check body scope)
     ,@(mapcar (lambda (handler)
                 (unless (and (consp handler) (proper-list-p handler))
                   (lispm-lisp-refuse "the handler %S" handler))
                 (dolist (condition (ensure-list (car handler)))
                   (when (memq condition lispm-lisp--unhandled)
                     (lispm-lisp-refuse "handling %s" condition)))
                 (cons (car handler)
                       (lispm-lisp--check-body (cdr handler) (if var (cons var scope) scope))))
               handlers)))

(defun lispm-lisp--check (form scope)
  "FORM, rewritten as a program runs it; refuse it if it may not run.
SCOPE lists the variables the program bound around FORM."
  (cond
   ((atom form)
    (when (and (symbolp form)
               (not (memq form '(nil t :false :null)))
               (not (keywordp form))
               (not (memq form scope))
               (not (memq form lispm-lisp--bindable))
               (not (memq form lispm-lisp--readable-globals)))
      (lispm-lisp-refuse "reading %s, a variable the program did not bind" form))
    form)
   ((not (proper-list-p form)) (lispm-lisp-refuse "%S" form))
   (t
    (let ((head (car form))
          (args (cdr form)))
      (cond
       ((not (symbolp head))
        (lispm-lisp-refuse "calling %S: name a function, or use funcall" head))
       ((eq head 'quote) form)
       ((eq head 'function)
        (pcase args
          (`(,(and name (pred symbolp))) `(function ,name))
          (`(,lambda)
           `(lispm-lisp-closure (function ,(lispm-lisp--check-lambda lambda scope))))
          (_ (lispm-lisp-refuse "%S" form))))
       ((memq head '(progn prog1 and or if while unwind-protect save-current-buffer))
        (cons head (lispm-lisp--check-body args scope)))
       ((eq head 'save-excursion)
        `(save-excursion
           (let ((lispm-lisp-excursions (cons (current-buffer) lispm-lisp-excursions)))
             ,@(lispm-lisp--check-body args scope))))
       ((eq head 'save-restriction)
        `(save-restriction
           (let ((lispm-lisp-restrictions (cons (current-buffer) lispm-lisp-restrictions)))
             ,@(lispm-lisp--check-body args scope))))
       ((eq head 'cond)
        (cons 'cond (mapcar (lambda (clause) (lispm-lisp--check-body clause scope)) args)))
       ((memq head '(let let*))
        (lispm-lisp--check-let head (car args) (cdr args) scope))
       ((eq head 'setq) (lispm-lisp--check-setq args scope))
       ((eq head 'condition-case)
        (lispm-lisp--check-condition-case (car args) (cadr args) (cddr args) scope))
       ((eq head 'catch)
        (let ((tag (make-symbol "tag")))
          `(let ((,tag ,(lispm-lisp--check (car args) scope)))
             (catch ,tag
               (let ((lispm-lisp-tags (cons ,tag lispm-lisp-tags)))
                 ,@(lispm-lisp--check-body (cdr args) scope))))))
       ((eq head lispm-lisp--internal)
        (cons (car args) (lispm-lisp--check-body (cdr args) scope)))
       ((gethash head lispm-lisp--macro-tools)
        (lispm-lisp--check (funcall (gethash head lispm-lisp--macro-tools) args) scope))
       ((memq head lispm-lisp-tools)
        (lispm-lisp--expand-tool-call head args scope))
       ((macrop head)
        (unless (memq head lispm-lisp--macros)
          (lispm-lisp-refuse "the macro %s" head))
        (let* ((early-p (memq head lispm-lisp--early-macros))
               (blocked (and early-p (lispm-lisp--scan form)))
               (_ (when early-p (lispm-lisp--check-places head args)))
               (env (and early-p (lispm-lisp--macro-env blocked)))
               (macroexpand-all-environment env)
               (expansion (macroexpand-1 form env)))
          (when (eq expansion form)
            (lispm-lisp-refuse "%S" form))
          (lispm-lisp--check expansion scope)))
       ((special-form-p head) (lispm-lisp-refuse "the special form %s" head))
       ((assq head lispm-lisp-guarded)
        `(lispm-lisp-apply ',head ,@(lispm-lisp--check-body args scope)))
       ((lispm-lisp-allowed-p head)
        (cons head (lispm-lisp--check-body args scope)))
       ((not (fboundp head))
        (lispm-lisp-refuse "%s, which is neither a function nor a tool you have" head))
       (t
        (lispm-lisp-refuse "%s: it is not known to be free of side effects; \
call a tool for that, or use eval_elisp" head)))))))

(defun lispm-lisp-prepare (code)
  "The forms of the program CODE, checked and rewritten to run."
  (mapcar (lambda (form)
            (when (and (consp form) (memq (car form) lispm-lisp--never))
              (lispm-lisp-refuse "%s" (car form)))
            (lispm-lisp--check form nil))
          (lispm-tools-read-forms code)))


(defun lispm-lisp-json (value tool)
  "VALUE, given to TOOL, as decoded JSON is: strings, numbers, vectors, alists."
  (cond ((stringp value) (substring-no-properties value))
        ((or (numberp value) (memq value '(t :false))) value)
        ((null value) :false)
        ((symbolp value) (symbol-name value))
        ((vectorp value) (vconcat (mapcar (lambda (v) (lispm-lisp-json v tool)) value)))
        ((and (proper-list-p value)
              (seq-every-p (lambda (e) (and (consp e) (or (symbolp (car e)) (stringp (car e)))))
                           value))
         (mapcar (lambda (e) (cons (if (stringp (car e)) (intern (car e)) (car e))
                                   (lispm-lisp-json (cdr e) tool)))
                 value))
        ((proper-list-p value) (vconcat (mapcar (lambda (v) (lispm-lisp-json v tool)) value)))
        (t (error "Cannot give %s %S: give strings, numbers, t, lists or alists" tool value))))

(provide 'lispm-lisp-check)

;;; lispm-lisp-check.el ends here
