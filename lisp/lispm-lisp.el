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
(require 'byte-opt)                     ; the `side-effect-free' declarations
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

(defcustom lispm-lisp-max-calls 200
  "Most tool calls a program run with run_lisp may make."
  :type 'integer
  :group 'lispm)

(define-error 'lispm-call-failed "Tool call failed")
(define-error 'lispm-call-denied "Tool call denied" 'lispm-call-failed)

;; Not an `error': a program cannot handle it, and goes no further.
(put 'lispm-lisp-refused 'error-conditions '(lispm-lisp-refused))
(put 'lispm-lisp-refused 'error-message "Not allowed in run_lisp")

(defun lispm-lisp--refuse (format &rest args)
  "Refuse to run a program, for the reason FORMAT and ARGS make."
  (signal 'lispm-lisp-refused (list (apply #'format-message format args))))

(defun lispm-lisp--error-text (err)
  "The error ERR of a program, as its result says it."
  (if (memq (car err) '(lispm-lisp-refused lispm-call-failed lispm-call-denied))
      (format "%s: %s" (get (car err) 'error-message) (cadr err))
    (error-message-string err)))

(cl-defstruct (lispm-lisp--run (:constructor lispm-lisp--run-create) (:copier nil))
  "A program run with run_lisp: its AGENT, its call ID (the call plist
CALL), its FORMS, checked, the TOOLS it may call.  LOG holds the calls
it made, each (NAME INPUT OUTCOME TEXT), OUTCOME being `value',
`failed' or `denied'; INDEX counts the calls of the pass under way.
HANDLERS are the file name handlers it found, given back to tools;
BUFFERS are those it made in the pass under way; STOPPING is how the
pass under way ends, once it stops at a call; OUTPUT is what its last
pass printed; DONE gets its result, once it is FINISHED."
  agent id call forms tools log (index 0) handlers buffers output done finished
  stopping)

;;;; What a program may do

(defconst lispm-lisp--macros
  '(when unless dolist dotimes push pop lambda prog2 \` letrec
    if-let if-let* when-let when-let* and-let* while-let
    pcase pcase-let pcase-let* pcase-dolist pcase-exhaustive pcase-setq seq-let seq-doseq
    cl-incf cl-decf setf ignore-errors ignore-error save-match-data
    with-temp-buffer with-current-buffer let-alist thread-first thread-last
    cl-loop cl-case cl-ecase cl-dolist cl-dotimes cl-block cl-return cl-return-from
    cl-destructuring-bind cl-flet cl-flet* cl-labels named-let rx)
  "Macros a program may use.  Their expansions are checked in turn.")

(defconst lispm-lisp--early-macros '(cl-flet cl-flet* cl-labels rx)
  "Macros of `lispm-lisp--macros' that expand the forms in them while expanding.
Before one is expanded, its forms are searched for macros a program
may not use, which would run then, unchecked.")

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
    ;; Errors, and buffers to read.
    error user-error signal set-buffer)
  "Functions a program may call besides those declared free of side effects.")

(defconst lispm-lisp--unsafe-functions
  '(;; They run code, or make functions of it.
    documentation apply-partially make-byte-code
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

(defconst lispm-lisp--unhandled '(t quit minibuffer-quit debug lispm-lisp-refused)
  "Conditions a program may not handle: C-g must stop it, and refusals end it.")

(defconst lispm-lisp--guarded
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
    (delete-dups :use lispm-lisp--delete-dups)
    (plist-put :functions (3) :use lispm-lisp--plist-put)
    (match-data :nil (1)) (set-match-data :use lispm-lisp--set-match-data)
    (mapcan :functions (0) :use lispm-lisp--mapcan)
    (sort :functions (1) :keys (1 :key :lessp) :use lispm-lisp--sort)
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

(defun lispm-lisp--allowed-p (function)
  "Non-nil if a program may call FUNCTION, a symbol, without checks."
  (and (symbolp function) (fboundp function)
       (not (memq function lispm-lisp--unsafe-functions))
       (or (memq function lispm-lisp--functions)
           (function-get function 'side-effect-free)
           (function-get function 'pure)
           (eq (get function 'safe-function) t))))

;;;; Checking a program

(defvar lispm-lisp--tools nil
  "Names of the tools the program being checked, or run, may call, as symbols.")

(defun lispm-lisp--scan (form)
  "Refuse FORM if a macro a program may not use is in it, anywhere.
Quoted data too: FORM is about to be expanded by a macro that expands
what is in it, whatever it is."
  (let ((seen (make-hash-table :test #'eq))
        (count 0))
    (cl-labels ((walk (x)
                  (while (consp x)
                    (when (gethash x seen)
                      (lispm-lisp--refuse "a circular form"))
                    (puthash x t seen)
                    (when (> (cl-incf count) 100000)
                      (lispm-lisp--refuse "a program this large"))
                    (let ((head (car x)))
                      (when (and (symbolp head)
                                 (or (memq head lispm-lisp--never)
                                     (and (macrop head) (not (memq head lispm-lisp--macros)))))
                        (lispm-lisp--refuse "%s" head))
                      (walk head))
                    (setq x (cdr x)))))
      (walk form))))

(defun lispm-lisp--binder (symbol)
  "SYMBOL, if a program may bind it; else refuse."
  (unless (and symbol (symbolp symbol) (not (keywordp symbol)) (not (eq symbol t)))
    (lispm-lisp--refuse "binding %S" symbol))
  (when (and (special-variable-p symbol) (not (memq symbol lispm-lisp--bindable)))
    (lispm-lisp--refuse "binding %s, a variable of Emacs's: give yours another name" symbol))
  symbol)

(defun lispm-lisp--check-body (forms scope)
  "FORMS, checked as `lispm-lisp--check' does, with SCOPE."
  (unless (proper-list-p forms)
    (lispm-lisp--refuse "%S, which is not a list of forms" forms))
  (mapcar (lambda (form) (lispm-lisp--check form scope)) forms))

(defun lispm-lisp--check-lambda (lambda scope)
  "The (lambda ARGS . BODY) form LAMBDA, checked with SCOPE."
  (pcase lambda
    (`(lambda ,(and args (pred proper-list-p)) . ,body)
     (let ((vars (seq-remove (lambda (arg) (memq arg '(&optional &rest))) args)))
       (mapc #'lispm-lisp--binder vars)
       `(lambda ,args ,@(lispm-lisp--check-body body (append vars scope)))))
    (_ (lispm-lisp--refuse "the function %S" lambda))))

(defun lispm-lisp--check-let (head bindings body scope)
  "The form (HEAD BINDINGS . BODY), a `let' or `let*', checked with SCOPE."
  (unless (proper-list-p bindings)
    (lispm-lisp--refuse "the bindings %S" bindings))
  (let ((inner scope) checked)
    (dolist (binding bindings)
      (pcase binding
        ((pred symbolp)
         (push (lispm-lisp--binder binding) checked)
         (push binding inner))
        (`(,symbol . ,(and rest (guard (and (proper-list-p rest) (<= (length rest) 1)))))
         (lispm-lisp--binder symbol)
         (push (list symbol (lispm-lisp--check (car rest) (if (eq head 'let*) inner scope)))
               checked)
         (push symbol inner))
        (_ (lispm-lisp--refuse "the binding %S" binding))))
    `(,head ,(nreverse checked) ,@(lispm-lisp--check-body body inner))))

(defun lispm-lisp--check-setq (pairs scope)
  "The form (setq . PAIRS), checked with SCOPE: it sets the program's variables."
  (unless (cl-evenp (length pairs))
    (lispm-lisp--refuse "setq with an odd number of arguments"))
  (let (checked)
    (while pairs
      (let ((symbol (pop pairs))
            (value (pop pairs)))
        (unless (memq symbol scope)
          (lispm-lisp--refuse "setting %s, a variable the program did not bind: bind it with let"
                              symbol))
        (push symbol checked)
        (push (lispm-lisp--check value scope) checked)))
    `(setq ,@(nreverse checked))))

(defun lispm-lisp--check-condition-case (var body handlers scope)
  "The form (condition-case VAR BODY . HANDLERS), checked with SCOPE."
  (when var (lispm-lisp--binder var))
  `(condition-case ,var ,(lispm-lisp--check body scope)
     ,@(mapcar (lambda (handler)
                 (unless (and (consp handler) (proper-list-p handler))
                   (lispm-lisp--refuse "the handler %S" handler))
                 (dolist (condition (ensure-list (car handler)))
                   (when (memq condition lispm-lisp--unhandled)
                     (lispm-lisp--refuse "handling %s" condition)))
                 (cons (car handler)
                       (lispm-lisp--check-body (cdr handler) (if var (cons var scope) scope))))
               handlers)))

(defun lispm-lisp--check (form scope)
  "FORM, rewritten as a program runs it; refuse it if it may not run.
SCOPE lists the variables the program bound around FORM."
  (cond
   ((atom form) form)
   ((not (proper-list-p form)) (lispm-lisp--refuse "%S" form))
   (t
    (let ((head (car form))
          (args (cdr form)))
      (cond
       ((not (symbolp head))
        (lispm-lisp--refuse "calling %S: name a function, or use funcall" head))
       ((eq head 'quote) form)
       ((eq head 'function)
        (pcase args
          (`(,(and name (pred symbolp))) `(function ,name))
          (`(,lambda)
           `(lispm-lisp--closure (function ,(lispm-lisp--check-lambda lambda scope))))
          (_ (lispm-lisp--refuse "%S" form))))
       ((memq head '(progn prog1 and or if while unwind-protect save-current-buffer))
        (cons head (lispm-lisp--check-body args scope)))
       ((eq head 'save-excursion)
        `(save-excursion
           (let ((lispm-lisp--excursions (cons (current-buffer) lispm-lisp--excursions)))
             ,@(lispm-lisp--check-body args scope))))
       ((eq head 'save-restriction)
        `(save-restriction
           (let ((lispm-lisp--restrictions (cons (current-buffer) lispm-lisp--restrictions)))
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
               (let ((lispm-lisp--tags (cons ,tag lispm-lisp--tags)))
                 ,@(lispm-lisp--check-body (cdr args) scope))))))
       ((memq head lispm-lisp--tools)
        `(lispm-lisp--call ',head ,@(lispm-lisp--check-body args scope)))
       ((macrop head)
        (unless (memq head lispm-lisp--macros)
          (lispm-lisp--refuse "the macro %s" head))
        (when (memq head lispm-lisp--early-macros)
          (lispm-lisp--scan form))
        (let ((expansion (macroexpand-1 form)))
          (when (eq expansion form)
            (lispm-lisp--refuse "%S" form))
          (lispm-lisp--check expansion scope)))
       ((special-form-p head) (lispm-lisp--refuse "the special form %s" head))
       ((assq head lispm-lisp--guarded)
        `(lispm-lisp--apply ',head ,@(lispm-lisp--check-body args scope)))
       ((lispm-lisp--allowed-p head)
        (cons head (lispm-lisp--check-body args scope)))
       ((not (fboundp head))
        (lispm-lisp--refuse "%s, which is neither a function nor a tool you have" head))
       (t
        (lispm-lisp--refuse "%s: it is not known to be free of side effects; \
call a tool for that, or use eval_elisp" head)))))))

(defun lispm-lisp--prepare (code)
  "The forms of the program CODE, checked and rewritten to run."
  (mapcar (lambda (form)
            (when (and (consp form) (memq (car form) lispm-lisp--never))
              (lispm-lisp--refuse "%s" (car form)))
            (lispm-lisp--check form nil))
          (lispm-tools--read-forms code)))

;;;; Checks as it runs

(defvar lispm-lisp--owned (make-hash-table :test #'eq :weakness 'key)
  "Objects programs made, which they may call or change: closures, buffers, tables.")

(defvar lispm-lisp--tags nil
  "Catch tags of the running program's own catches, innermost first.")

(defvar lispm-lisp--excursions nil
  "Buffers the running program is in a `save-excursion' of.")

(defvar lispm-lisp--restrictions nil
  "Buffers the running program is in a `save-restriction' of.")

(defvar lispm-lisp--run nil
  "The run of the program being evaluated, while it is.")

(defvar lispm-lisp--pass-tag nil
  "The catch tag that ends the pass under way of a program.")

(defun lispm-lisp--closure (closure)
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
        ((and (symbolp function) (memq function lispm-lisp--tools))
         (lambda (&rest args) (apply #'lispm-lisp--call function args)))
        ((and (symbolp function) (assq function lispm-lisp--guarded))
         (lambda (&rest args) (apply #'lispm-lisp--apply function args)))
        ((and function (lispm-lisp--allowed-p function)) function)
        (t (lispm-lisp--refuse "calling %S" function))))

(defun lispm-lisp--own (object)
  "Note that the running program made OBJECT, a buffer or a hash table; return it."
  (when object
    (puthash object lispm-lisp--run lispm-lisp--owned)
    (when (bufferp object)
      (push object (lispm-lisp--run-buffers lispm-lisp--run))))
  object)

(defun lispm-lisp--owned-p (object)
  "Non-nil if the running program made OBJECT."
  (and object lispm-lisp--run (eq (gethash object lispm-lisp--owned) lispm-lisp--run)))

(defun lispm-lisp--check-buffer (kind function)
  "Refuse FUNCTION, of KIND, unless the program may use it in this buffer."
  (unless (or (lispm-lisp--owned-p (current-buffer))
              (and (eq kind 'motion) (memq (current-buffer) lispm-lisp--excursions))
              (and (eq kind 'narrow) (memq (current-buffer) lispm-lisp--restrictions)))
    (lispm-lisp--refuse "%s in %s, a buffer the program did not make%s" function (buffer-name)
                        (pcase kind
                          ('motion " (other than inside save-excursion)")
                          ('narrow " (other than inside save-restriction)")
                          (_ "")))))

(defun lispm-lisp--apply (function &rest args)
  "Call FUNCTION on ARGS with the checks `lispm-lisp--guarded' gives it."
  (let ((spec (alist-get function lispm-lisp--guarded)))
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
          (lispm-lisp--refuse "%s on a buffer the program did not make" function))))
    (when-let* ((n (plist-get spec :table)))
      (unless (lispm-lisp--owned-p (nth n args))
        (lispm-lisp--refuse "%s on a hash table the program did not make" function)))
    (when (and (plist-get spec :throw) (not (memq (car args) lispm-lisp--tags)))
      (lispm-lisp--refuse "throwing to %S, which the program did not catch" (car args)))
    (dolist (n (plist-get spec :nil))
      (when (nth n args)
        (lispm-lisp--refuse "%s with argument %d: it would print elsewhere than to the \
program's output, or change the object given" function (1+ n))))
    (let ((value (apply (or (plist-get spec :use) function) args)))
      (if (plist-get spec :owned) (lispm-lisp--own value) value))))

(defun lispm-lisp--delete-dups (list)
  "LIST without its duplicates, LIST unchanged."
  (delete-dups (copy-sequence list)))

(defun lispm-lisp--set-match-data (list &optional _reseat)
  "Set the match data from LIST, its markers unchanged."
  (set-match-data list))

(defun lispm-lisp--plist-put (plist &rest args)
  "A copy of PLIST with what `plist-put' with ARGS puts in it; PLIST unchanged."
  (apply #'plist-put (copy-sequence plist) args))

(defun lispm-lisp--mapcan (function sequence)
  "The results of FUNCTION on SEQUENCE's elements, appended; none changed."
  (apply #'append (mapcar function sequence)))

(defun lispm-lisp--sort (sequence &rest args)
  "A sorted copy of SEQUENCE, as `sort' with ARGS sorts."
  (apply #'sort (copy-sequence sequence) args))

;;;; Calling tools

(defun lispm-lisp--json (value tool)
  "VALUE, given to TOOL, as decoded JSON is: strings, numbers, vectors, alists."
  (cond ((stringp value) (substring-no-properties value))
        ((or (numberp value) (memq value '(t :false))) value)
        ((null value) :false)
        ((symbolp value) (symbol-name value))
        ((vectorp value) (vconcat (mapcar (lambda (v) (lispm-lisp--json v tool)) value)))
        ((and (proper-list-p value)
              (seq-every-p (lambda (e) (and (consp e) (or (symbolp (car e)) (stringp (car e)))))
                           value))
         (mapcar (lambda (e) (cons (if (stringp (car e)) (intern (car e)) (car e))
                                   (lispm-lisp--json (cdr e) tool)))
                 value))
        ((proper-list-p value) (vconcat (mapcar (lambda (v) (lispm-lisp--json v tool)) value)))
        (t (error "Cannot give %s %S: give strings, numbers, t, lists or alists" tool value))))

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
            (push (cons (lispm-tools--key (intern (substring (symbol-name key) 1)))
                        (lispm-lisp--json value tool))
                  input)))))
    (nreverse input)))

(defun lispm-lisp--outcome (entry)
  "What the call ENTRY of a program's log gives the program."
  (pcase-let ((`(,name ,_input ,outcome ,text) entry))
    (pcase outcome
      ('value text)
      ('denied (signal 'lispm-call-denied (list (format "%s: %s" name text))))
      (_ (signal 'lispm-call-failed (list (format "%s: %s" name text)))))))

(defun lispm-lisp--record (run index name input outcome text)
  "Record in RUN's log that its call INDEX, of NAME on INPUT, had OUTCOME and TEXT."
  (setf (lispm-lisp--run-log run)
        (append (seq-take (lispm-lisp--run-log run) index)
                (list (list name input outcome text)))))

(defun lispm-lisp--invoke (run tool input callback)
  "Run TOOL on INPUT as a call of RUN's agent; CALLBACK gets TEXT and ERRORP.
The call runs as one the model made does, whatever the program bound."
  (let ((lispm-current-agent (lispm-lisp--run-agent run))
        (lispm-current-call nil)
        (default-directory (lispm-agent-root (lispm-lisp--run-agent run)))
        (file-name-handler-alist (lispm-lisp--run-handlers run))
        (standard-output t)
        (lispm-lisp--run nil))
    (lispm-tool-invoke tool input (lambda (text errorp &optional _images)
                                    (funcall callback text errorp)))))

(defun lispm-lisp--stop (run how)
  "End RUN's pass under way at a call, as HOW says (see `lispm-lisp--pass')."
  (setf (lispm-lisp--run-stopping run) how)
  (throw lispm-lisp--pass-tag how))

(defun lispm-lisp--call (name &rest args)
  "Call the tool NAME with the keyword ARGS, from the running program.
Its result text is returned, or a failure signaled; a call made by
the run before gets the result it had then."
  (let* ((run (or lispm-lisp--run (error "No program is running")))
         ;; Cleanups of the program, as its pass ends at a call, make
         ;; no more calls.
         (_ (when-let* ((how (lispm-lisp--run-stopping run)))
              (throw lispm-lisp--pass-tag how)))
         (tool (or (lispm-get-tool name) (error "No tool %s" name)))
         (name (lispm-tool-name tool))
         (input (lispm-lisp--input name args))
         (index (lispm-lisp--run-index run))
         (entry (nth index (lispm-lisp--run-log run))))
    (when (>= index lispm-lisp-max-calls)
      (error "The program made more than %d tool calls" lispm-lisp-max-calls))
    (setf (lispm-lisp--run-index run) (1+ index))
    (if (and entry (equal (car entry) name) (equal (nth 1 entry) input))
        (lispm-lisp--outcome entry)
      ;; The program does something else than it did before.
      (setf (lispm-lisp--run-log run) (seq-take (lispm-lisp--run-log run) index))
      (let ((agent (lispm-lisp--run-agent run)))
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
                   ((equal (car result) lispm-tools--interrupted) (signal 'quit nil))
                   (t (lispm-lisp--record run index name input
                                          (if (nth 1 result) 'failed 'value) (car result))
                      (lispm-lisp--outcome (nth index (lispm-lisp--run-log run)))))))
          (`(ask . ,reasons) (lispm-lisp--stop run (list :ask index tool input reasons)))
          (`(,_ . ,reasons)
           (lispm-lisp--record run index name input 'denied (lispm-tools-denial agent tool reasons))
           (lispm-lisp--outcome (nth index (lispm-lisp--run-log run)))))))))

;;;; Running a program

(defun lispm-lisp--pass (run)
  "Evaluate RUN's program once, the calls it made before getting their results.
Return (:value VALUE) if it ran to its end, (:ask INDEX TOOL INPUT
REASONS) if it stopped at a call awaiting approval, REASONS as
`lispm-tool-decision' gives them, or (:wait) at one under way.  The
program's errors are signaled."
  (let ((output (generate-new-buffer " *lispm-lisp*" t))
        (tag (list 'lispm-lisp-pass)))
    (setf (lispm-lisp--run-index run) 0
          (lispm-lisp--run-buffers run) nil
          (lispm-lisp--run-stopping run) nil)
    (unwind-protect
        (catch tag
          (let ((lispm-lisp--run run)
                (lispm-lisp--pass-tag tag)
                (lispm-lisp--tools (lispm-lisp--run-tools run))
                (lispm-lisp--tags nil)
                (lispm-lisp--excursions nil)
                (lispm-lisp--restrictions nil)
                (standard-output output)
                (file-name-handler-alist nil)
                (default-directory (lispm-agent-root (lispm-lisp--run-agent run)))
                value)
            (save-current-buffer
              (save-match-data
                (dolist (form (lispm-lisp--run-forms run))
                  (setq value (eval form t)))))
            (list :value value)))
      (setf (lispm-lisp--run-output run)
            (with-current-buffer output (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output)
      (dolist (buffer (lispm-lisp--run-buffers run))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun lispm-lisp--live-p (run)
  "Non-nil if RUN goes on: unfinished, and its call not stopped."
  (and (not (lispm-lisp--run-finished run))
       (or (null (lispm-lisp--run-id run))
           (eq (gethash (lispm-lisp--run-id run)
                        (lispm-agent-pending (lispm-lisp--run-agent run)))
               (lispm-lisp--run-call run)))))

(defun lispm-lisp--calls-text (run)
  "The calls RUN's program made, a line each, as its result lists them."
  (when-let* ((log (lispm-lisp--run-log run)))
    (concat "Tool calls:\n"
            (mapconcat (pcase-lambda (`(,name ,input ,outcome ,_text))
                         (let ((summary (lispm-transcript-summary input)))
                           (format "- %s%s%s" name (if summary (concat " " summary) "")
                                   (pcase outcome ('value "") (_ (format " (%s)" outcome))))))
                       (seq-take log 30) "\n")
            (if (nthcdr 30 log) (format "\n- ... %d more" (- (length log) 30)) "")
            "\n\n")))

(defun lispm-lisp--finish (run text errorp)
  "End RUN with the result TEXT; ERRORP if it failed."
  (setf (lispm-lisp--run-finished run) t)
  (funcall (lispm-lisp--run-done run) (substring-no-properties text) errorp))

(defun lispm-lisp--step (run)
  "Run RUN's program, again if it ran before, and go on as it ends."
  (when (lispm-lisp--live-p run)
    (pcase (condition-case err
               (lispm-tools--interruptibly #'lispm-lisp--pass (list run))
             ((lispm-lisp-refused error) (list :error (lispm-lisp--error-text err)))
             (quit (list :quit)))
      (`(:value ,value)
       (lispm-lisp--finish run (concat (lispm-lisp--calls-text run)
                                       (lispm-tools--eval-result
                                        (lispm-lisp--run-output run)
                                        (if (stringp value) (substring-no-properties value) value)))
                           nil))
      (`(:ask ,index ,tool ,input ,reasons) (lispm-lisp--ask run index tool input reasons))
      (`(:wait))
      (`(:error ,message)
       (lispm-lisp--finish run (concat (lispm-lisp--calls-text run) message) t))
      (_ (lispm-lisp--finish run lispm-tools--interrupted t)))))

(defun lispm-lisp--again (run)
  "Run RUN's program again soon, from the event loop."
  (run-at-time 0 nil #'lispm-lisp--step run))

(defun lispm-lisp--settle (run index name input text errorp)
  "Take the result TEXT of RUN's call INDEX, of NAME on INPUT, that ran on.
ERRORP marks a failure.  Then the program runs again."
  (when (lispm-lisp--live-p run)
    (if (equal text lispm-tools--interrupted)
        (lispm-lisp--finish run text t)
      (lispm-lisp--record run index name input (if errorp 'failed 'value) text)
      (lispm-lisp--again run))))

(defun lispm-lisp--ask (run index tool input &optional reasons)
  "Have RUN's call INDEX, of TOOL on INPUT, await the user's approval.
REASONS are why it asks, if a check said (see `lispm-tool-decision').
Once approved it runs, once denied it fails, and the program runs again."
  (let ((name (lispm-tool-name tool)))
    (unless (and (lispm-lisp--run-id run)
                 (lispm-call-ask
                  (lispm-lisp--run-agent run) (lispm-lisp--run-id run) tool input
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
when refused), which condition-case can handle.  Each call goes through
your permission policy as if you made it: reads run at once, a write
waits for the user's approval.  Otherwise a program only computes: the
special forms, common macros (let, dolist, pcase, cl-loop, when-let*,
with-temp-buffer), and functions free of side effects (strings, lists,
regexps, seq-*, file names, reading buffers and directories).  Functions
passed to mapcar and the like must be lambdas or such functions; it may
not set global variables or change data in place, and changes only
buffers it makes.  Use eval_elisp for anything else.  Returns the calls
made, what the program printed and its last value."
  :permission lisp
  :async t
  (let ((agent (or lispm-current-agent (error "run_lisp runs programs of agents"))))
    (condition-case err
        (let* ((tools (lispm-lisp--tool-names agent))
               (forms (let ((lispm-lisp--tools tools)) (lispm-lisp--prepare code)))
               (id lispm-current-call))
          (lispm-lisp--step
           (lispm-lisp--run-create
            :agent agent :id id :call (and id (gethash id (lispm-agent-pending agent)))
            :forms forms :tools tools :handlers file-name-handler-alist :done done)))
      (lispm-lisp-refused (funcall done (lispm-lisp--error-text err) t)))))

(provide 'lispm-lisp)
;;; lispm-lisp.el ends here
