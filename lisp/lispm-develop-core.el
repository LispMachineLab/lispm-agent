;;; lispm-develop-core.el --- Code-object history, caller DB, finding, editing, and eval  -*- lexical-binding: t; -*-

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

;; In-memory caller database and definition edit history, symbol
;; location via `find-func', structural definition editing, and
;; `eval_definition'.

;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'project)
(require 'lispm-core)
(require 'lispm-tools)

(defcustom lispm-xref-max-results 200
  "Most lines xref_definitions and xref_references return."
  :type 'integer
  :group 'lispm)

(defcustom lispm-diagnostics-timeout 30
  "Seconds flymake_diagnostics waits for the checkers of a buffer to report."
  :type 'integer
  :group 'lispm)

(defconst lispm-develop-tools
  '("edit_definition" "eval_definition" "ert_run"
    "xref_definitions" "xref_references" "flymake_diagnostics")
  "The tools of this file.")

;;;; Code-object history and in-memory caller database (Genera Figure A & SCT)

(defvar lispm-develop-history nil
  "Granular code-object edit history, newest first.
Each entry is a plist `(:agent AGENT :turn TURN :symbol SYMBOL :name NAME
:type TYPE :file FILE :old-form OLD-FORM :new-form NEW-FORM
:lexical-binding BOOL)'.")

(defvar lispm-develop--callers (make-hash-table :test #'eq)
  "In-memory caller database: maps CALLEE symbol to a list of CALLER symbols.")

(defvar lispm-develop--callees (make-hash-table :test #'equal)
  "In-memory callee database: maps `(FILE . CALLER)' to the symbols it calls.")

(defvar lispm-develop--file-callers (make-hash-table :test #'equal)
  "Maps source FILE to the list of CALLER symbols last indexed from it.")

(defconst lispm-develop--non-callable-heads
  '(nil t progn prog1 prog2 if when unless and or save-excursion save-restriction
        save-current-buffer unwind-protect while catch defvar defcustom defface)
  "Special forms and macros whose head symbol is not indexed as a function call.")

(defconst lispm-develop--definers-with-arglist
  '(defun defmacro cl-defun cl-defmacro defsubst cl-defsubst ert-deftest lispm-deftool)
  "Top-level definers whose second element is a parameter list, not an expression.")

(defun lispm-develop--extract-callees (form)
  "Symbols called or referenced as `#'SYMBOL' inside FORM.
Handles circular structures and special forms (`let', `lambda', `cond',
`condition-case') without mistaking parameter lists or bindings for calls."
  (let ((seen (make-hash-table :test #'eq))
        callees)
    (cl-labels
        ((walk-chain (list fn)
           (while (and (consp list) (not (gethash list seen)))
             (puthash list t seen)
             (funcall fn (car list))
             (setq list (cdr list))))
         (walk-list (list)
           (walk-chain list #'walk))
         (walk-bindings (bindings single-ok)
           (if (and single-ok (consp bindings) (symbolp (car bindings))
                    (consp (cdr bindings)) (null (cddr bindings)))
               ;; Single-binding syntax: (when-let (x (car xs)) ...)
               (walk-list (cdr bindings))
             (walk-chain bindings
                         (lambda (b)
                           (when (and (consp b) (not (gethash b seen)))
                             (puthash b t seen)
                             (walk-list (cdr b)))))))
         (walk (x)
           (when (and (consp x) (not (gethash x seen)))
             (puthash x t seen)
             (let ((head (car x))
                   (rest (cdr x)))
               (cond
                ((memq head '(quote declare interactive)) nil)
                ((eq head 'function)
                 (when (consp rest)
                   (if (symbolp (car rest))
                       (push (car rest) callees)
                     (walk (car rest)))))
                ((eq head 'lambda)
                 (when (consp rest)
                   (walk-list (cdr rest))))
                ((memq head lispm-develop--definers-with-arglist)
                 (when (and (consp rest) (consp (cdr rest)))
                   (walk-list (cddr rest))))
                ((memq head '(let let* when-let* if-let* and-let*))
                 (when (consp rest)
                   (walk-bindings (car rest) nil)
                   (walk-list (cdr rest))))
                ((memq head '(when-let if-let while-let))
                 (when (consp rest)
                   (walk-bindings (car rest) t)
                   (walk-list (cdr rest))))
                ((memq head '(dolist dotimes cl-dolist cl-dotimes))
                 (when (consp rest)
                   (when (and (consp (car rest)) (not (gethash (car rest) seen)))
                     (puthash (car rest) t seen)
                     (walk-list (cdar rest)))
                   (walk-list (cdr rest))))
                ((eq head 'cond)
                 (walk-chain rest #'walk-list))
                ((eq head 'condition-case)
                 (when (and (consp rest) (consp (cdr rest)))
                   (walk (cadr rest))
                   (walk-chain (cddr rest)
                               (lambda (handler)
                                 (when (consp handler)
                                   (walk-list (cdr handler)))))))
                ((eq head 'setq)
                 (let ((val-p nil))
                   (walk-chain rest
                               (lambda (item)
                                 (when val-p
                                   (walk item))
                                 (setq val-p (not val-p))))))
                (t
                 (if (and (symbolp head) (not (keywordp head)))
                     (unless (memq head lispm-develop--non-callable-heads)
                       (push head callees))
                   (walk head))
                 (walk-list rest)))))))
      (walk form)
      (delete-dups (nreverse callees)))))

(defun lispm-develop--caller-still-calls-p (caller callee)
  "Non-nil if CALLER still calls CALLEE in any indexed file."
  (let (found)
    (maphash (lambda (key callees)
               (when (and (eq (cdr key) caller) (memq callee callees))
                 (setq found t)))
             lispm-develop--callees)
    found))

(defun lispm-develop--remove-caller (caller &optional file)
  "Remove CALLER's outgoing edges (from FILE) in the `who-calls' database."
  (let* ((key (cons file caller))
         (old-callees (gethash key lispm-develop--callees)))
    (remhash key lispm-develop--callees)
    (dolist (old old-callees)
      (unless (lispm-develop--caller-still-calls-p caller old)
        (puthash old (delq caller (gethash old lispm-develop--callers))
                 lispm-develop--callers)))))

(defconst lispm-develop--callable-definers
  '(defun defmacro cl-defun cl-defmacro defsubst cl-defsubst
    ert-deftest lispm-deftool)
  "Top-level `(DEFINER NAME ARGLIST . BODY)' forms indexed in `who-calls'.")

(defun lispm-develop--quoted-symbol (arg)
  "Literal symbol quoted by ARG (`'SYM' or `#'SYM'), or nil."
  (when (and (consp arg)
             (memq (car arg) '(quote function))
             (consp (cdr arg))
             (null (cddr arg))
             (symbolp (cadr arg))
             (cadr arg))
    (cadr arg)))

(defun lispm-develop--defalias-body (def)
  "Body forms to walk for the definition argument DEF of `defalias'."
  (cond ((lispm-develop--quoted-symbol def)
         (list (list 'function (lispm-develop--quoted-symbol def))))
        ((and (consp def)
              (memq (car def) '(quote function))
              (consp (cdr def))
              (null (cddr def))
              (consp (cadr def))
              (eq (caadr def) 'lambda))
         (list (cadr def)))
        (t (list def))))

(defun lispm-develop-index-form (form &optional caller file)
  "Update the in-memory `who-calls' database for the top-level FORM.
CALLER, if not given, is taken from FORM when its head is in
`lispm-develop--callable-definers' or `defalias' (so `defvar',
`defvar-local', `defcustom', `defface', etc. never overwrite a
same-named function); FILE is the source file (by default
`buffer-file-name')."
  (let* ((file (or file buffer-file-name))
         (head (and (consp form) (symbolp (car form)) (car form)))
         (caller (or caller
                     (and head (consp (cdr form))
                          (cond
                           ((and (memq head lispm-develop--callable-definers)
                                 (symbolp (cadr form)))
                            (cadr form))
                           ((eq head 'defalias)
                            (lispm-develop--quoted-symbol (cadr form))))))))
    (when (and caller (symbolp caller))
      (lispm-develop--remove-caller caller file)
      (let* ((body (cond
                    ((memq head lispm-develop--callable-definers)
                     (and (consp (cddr form)) (cdddr form)))
                    ((eq head 'defalias)
                     (and (consp (cddr form))
                          (lispm-develop--defalias-body (caddr form))))
                    ((consp (cddr form))
                     (cddr form))
                    (t (list form))))
             (callees (delq caller
                            (lispm-develop--extract-callees (cons 'progn body)))))
        (puthash (cons file caller) callees lispm-develop--callees)
        (when file
          (let ((fcallers (gethash file lispm-develop--file-callers)))
            (unless (memq caller fcallers)
              (puthash file (cons caller fcallers) lispm-develop--file-callers))))
        (dolist (callee callees)
          (let ((existing (gethash callee lispm-develop--callers)))
            (unless (memq caller existing)
              (puthash callee (cons caller existing) lispm-develop--callers)))))
      caller)))

(defun lispm-develop-index-buffer (&optional buffer file)
  "Index all top-level definitions in BUFFER into the `who-calls' database.
When FILE (or `buffer-file-name') is known, replaces any previous caller
definitions recorded for that file after reading the whole buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((file (or file buffer-file-name))
          forms indexed)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (with-syntax-table emacs-lisp-mode-syntax-table
            (let ((parse-sexp-ignore-comments t))
              (condition-case err
                  (while (progn (forward-comment (buffer-size)) (not (eobp)))
                    (push (read (current-buffer)) forms))
                (error (error "Cannot read %s: %s"
                              (if file (lispm-tools-label file) (buffer-name))
                              (error-message-string err))))))))
      (when file
        (let ((old-callers (gethash file lispm-develop--file-callers)))
          (remhash file lispm-develop--file-callers)
          (dolist (old-caller old-callers)
            (lispm-develop--remove-caller old-caller file))))
      (dolist (form (nreverse forms))
        (when-let* ((sym (lispm-develop-index-form form nil file)))
          (push sym indexed)))
      (setq indexed (delete-dups (nreverse indexed)))
      (when file
        (puthash file indexed lispm-develop--file-callers))
      indexed)))

(defun lispm-develop-callees (caller &optional file)
  "Symbols recorded in the `who-calls' database as called by CALLER.
When FILE is given, only edges from that file; otherwise across all files."
  (let ((sym (if (symbolp caller) caller (intern-soft (format "%s" caller))))
        result)
    (when sym
      (if file
          (setq result (copy-sequence (gethash (cons file sym) lispm-develop--callees)))
        (maphash (lambda (key callees)
                   (when (eq (cdr key) sym)
                     (dolist (c callees)
                       (unless (memq c result)
                         (push c result)))))
                 lispm-develop--callees)
        (setq result (nreverse result))))
    result))

(defun lispm-develop-who-calls (symbol)
  "Symbols recorded in the in-memory `who-calls' database as calling SYMBOL."
  (let ((sym (if (symbolp symbol) symbol (intern-soft (format "%s" symbol)))))
    (and sym
         (sort (copy-sequence (gethash sym lispm-develop--callers))
               (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))

;;;; Finding definitions
;;
;; A definition is found as `find-function' finds it: its file from
;; `find-function-library' or `symbol-file', the form in it by the
;; searches of `find-function-regexp-alist'.  But the search is made in
;; the text the tool goes by (a buffer brought up to date, or for a
;; preview the file's text, see `lispm-file-text'), never by visiting
;; the file with `find-file-noselect', which would ask about a buffer
;; out of date; and never by expanding every macro of the file, which
;; is find-function's last resort and would run the file's code.

(defconst lispm-develop-kinds '(("function") ("variable" . defvar) ("face" . defface))
  "The kinds of definitions, each with its type in `find-function-regexp-alist'.")

(defun lispm-develop--form-at-line (regexp definer)
  "Where the first match for REGEXP in the current buffer begins its line.
Only a match whose line starts, after whitespace, a form outside strings
and comments counts; with DEFINER non-nil, only one whose head has
\"def\" in its name, so that it defines something."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward regexp nil t)
      (let ((line (save-excursion (goto-char (match-beginning 0)) (line-beginning-position))))
        (save-excursion
          (goto-char line)
          (skip-syntax-forward " ")
          (when (and (eq (char-after) ?\()
                     (not (nth 8 (parse-partial-sexp (point-min) (point))))
                     (or (not definer)
                         (progn (forward-char 1) (looking-at "[^ \t\n()]*def"))))
            (throw 'found line)))))
    nil))

(defun lispm-develop-search (symbol type)
  "Bounds (BEG . END) of the form defining SYMBOL as TYPE in the current buffer.
Nil if there is none.  TYPE is nil for a function, `defvar' for a
variable and `defface' for a face, or another type of
`find-function-regexp-alist'.  The form is found as
`find-function-search-for-symbol' finds it, by the search that alist
names, else by a form whose head names a definition and is followed by
SYMBOL; but in the text as it is, and never by expanding macros."
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))
  (let* ((entry (or (and (symbolp symbol) (alist-get type (get symbol 'find-function-type-alist)))
                    (alist-get type find-function-regexp-alist)))
         (search (if (consp entry) (car entry) entry)))
    (save-excursion
      (save-restriction
        (widen)
        (with-syntax-table emacs-lisp-mode-syntax-table
          (let* ((case-fold-search nil)
                 (parse-sexp-ignore-comments t)
                 (name (and (symbolp symbol) (regexp-quote (symbol-name symbol))))
                 (line (cond ((functionp search)
                              (goto-char (point-min))
                              (and (funcall search symbol) (line-beginning-position)))
                             (name
                              (or (and (boundp search) (stringp (symbol-value search))
                                       (lispm-develop--form-at-line
                                        (format (symbol-value search) (concat "\\\\?" name)) nil))
                                  (lispm-develop--form-at-line
                                   (concat "^([^ ]+" find-function-space-re "['(]?" name "\\_>")
                                   t))))))
            (when line
              (goto-char line)
              (skip-syntax-forward " ")
              (when (eq (char-after) ?\()
                (condition-case nil
                    (cons (point) (scan-sexps (point) 1))
                  (scan-error nil))))))))))

(defun lispm-develop--find (symbol types)
  "Where the current buffer defines SYMBOL as one of TYPES: (TYPE BEG . END).
The first of TYPES the buffer has a definition of, in order; nil if none."
  (cl-loop for type in types
           for bounds = (lispm-develop-search symbol type)
           when bounds return (cons type bounds)))

(defun lispm-develop--source (symbol type)
  "The file Emacs has the definition of SYMBOL as TYPE from: (SYMBOL . FILE).
Nil if Emacs has no such definition, or knows no file of it.  The file
is found as `find-function' finds it: for a function, from
`find-function-library', which follows an alias to the function it
names, so that the SYMBOL returned is that function; for a variable or
a face, from `symbol-file'.  A function built into Emacs is an error."
  (pcase-let ((`(,symbol . ,library)
               (pcase type
                 ('nil (and (fboundp symbol) (find-function-library symbol t)))
                 ('defvar (and (boundp symbol) (cons symbol (symbol-file symbol 'defvar))))
                 (_ (and (facep symbol) (cons symbol (symbol-file symbol type)))))))
    (when-let* ((file (and (stringp library)
                           (condition-case nil (find-library-name library) (file-error nil)))))
      (cons symbol file))))

(defun lispm-develop--definition (name kind path)
  "Where to find the definition of the symbol NAME: (SYMBOL FILE TYPES).
KIND is \"function\", \"variable\" or \"face\", or nil for any, in
that order.  Given PATH, the definition is looked for in that file, as
for one not evaluated yet; else in the file Emacs has it from (see
`lispm-develop--source').  SYMBOL is the symbol whose definition is
meant, NAME's or that of the function NAME is an alias of; TYPES are
the types of definition to look for in FILE, in order (see
`lispm-develop-search').  Nothing is visited."
  (let ((types (if kind (list (cdr (assoc kind lispm-develop-kinds))) '(nil defvar defface)))
        (symbol (intern-soft name)))
    (if path
        (list (or symbol (make-symbol name)) (lispm-tools-file path) types)
      (or (and symbol
               (cl-loop for type in types
                        for source = (lispm-develop--source symbol type)
                        when source return (list (car source) (cdr source) (list type))))
          (error "Emacs knows no file that defines %s%s; if a file not evaluated yet \
defines it, give that file as path"
                 name (if kind (concat " as a " kind) ""))))))

(defun lispm-develop-located (symbol types file)
  "Where the current buffer, holding FILE, defines SYMBOL: (TYPE BEG . END).
TYPES are the types of definition to look for.  An error if there is none."
  (or (lispm-develop--find symbol types)
      (error "%s has no definition of %s; read it to see where it went"
             (lispm-tools-label file) symbol)))

(defun lispm-develop-buffer-of (file)
  "A buffer holding FILE's text as the user sees it, for reading.
The buffer visiting FILE, brought up to date unless it has unsaved
changes, or else FILE, visited now.  An error if FILE is not there."
  (or (lispm-tools-visiting file t)
      (and (file-regular-p file) (lispm-tools-find-file file))
      (error "No such file: %s" (lispm-tools-label file))))

(defun lispm-develop-in-buffer (file function)
  "Call FUNCTION in a buffer holding FILE; return its value.
With FILE nil, in an Emacs Lisp buffer of no file, whose directory is
the acting agent's root.  A buffer visited for the call is killed after
it: tools that only read leave no buffers behind."
  (if (null file)
      (with-temp-buffer
        (delay-mode-hooks (emacs-lisp-mode))
        (setq default-directory (file-name-as-directory (lispm-tools-root)))
        (funcall function))
    (let* ((open (find-buffer-visiting file))
           (buffer (lispm-develop-buffer-of file)))
      (unwind-protect
          (with-current-buffer buffer
            (funcall function))
        (unless (or open (not (buffer-live-p buffer)) (buffer-modified-p buffer))
          (kill-buffer buffer))))))

(defun lispm-develop-line (position)
  "The line POSITION of the current buffer is on."
  (save-restriction
    (widen)
    (line-number-at-pos position)))

;;;; Editing definitions

(defun lispm-develop--new-form (text symbol type)
  "TEXT, trimmed, once checked to be one Lisp form defining SYMBOL as TYPE.
Comments may come before and after the form."
  (let ((text (string-trim text)))
    (with-temp-buffer
      (insert text)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (let ((parse-sexp-ignore-comments t))
          (goto-char (point-min))
          (forward-comment (buffer-size))
          (when (eobp)
            (error "new_text holds no Lisp form"))
          (let ((start (point)))
            (condition-case err
                (read (current-buffer))
              (end-of-file (error "new_text is not a complete Lisp form: its parentheses or strings do not close"))
              (invalid-read-syntax (error "new_text cannot be read as Lisp: %s"
                                          (error-message-string err))))
            (forward-comment (buffer-size))
            (unless (eobp)
              (error "new_text holds more than one form; give only the new definition"))
            (unless (eql (car (lispm-develop-search symbol type)) start)
              (error "new_text does not define %s as a %s; to rename or remove a definition, use edit_file"
                     symbol (car (rassq type lispm-develop-kinds))))))))
    text))

(defun lispm-develop-edit (symbol types file new-text)
  "The current buffer's text, holding FILE, with SYMBOL's definition replaced.
NEW-TEXT replaces it, trimmed; TYPES are the types of definition to look
for.  Return (TEXT BEG END TYPE): the whole new text, where the new form
is in it, and what type of definition it replaced."
  (pcase-let* ((`(,type ,beg . ,end) (lispm-develop-located symbol types file))
               (old (lispm-tools-buffer-text (current-buffer)))
               (new (string-trim new-text)))
    (list (concat (substring old 0 (1- beg)) new (substring old (1- end)))
          beg (+ beg (length new)) type)))

(defun lispm-develop--edit-diff (name new-text &optional kind path)
  "NAME's file as it is and as edit_definition would leave it, and its label.
The definition is replaced with NEW-TEXT; KIND and PATH are
edit_definition's arguments."
  (pcase-let* ((`(,symbol ,file ,types) (lispm-develop--definition name kind path))
               (old (lispm-file-text file)))
    (with-temp-buffer
      (insert old)
      (pcase-let ((`(,text ,_beg ,_end ,type) (lispm-develop-edit symbol types file new-text)))
        (lispm-develop--new-form new-text symbol type)
        (list old text (lispm-tools-label file))))))

(lispm-deftool edit_definition
    ((name string "Name of the function, macro, variable or face to redefine.")
     (new-text string "The whole new definition: one form, such as (defun NAME ...), replacing the old.")
     (kind string "Which definition of NAME (default: its function, else its variable, else its face)."
           :optional t :enum ("function" "variable" "face"))
     (path string "File to look in, for a definition not evaluated yet (default: the file Emacs has it from)."
           :optional t))
  "Redefine an Emacs Lisp function, macro, variable or face in its source file.
Give its name and the whole new form: the old one is found as
find-function finds it, wherever Emacs loaded it from, and replaced, as
one undoable step, then saved; you never copy the old text.  Emacs goes
on running the old definition until you evaluate the new one with
eval_definition; then run the tests with ert_run."
  :permission write
  :deferred t
  :targets (lambda (name _new-text &optional kind path)
             (list (nth 1 (lispm-develop--definition name kind path))))
  :diff #'lispm-develop--edit-diff
  :changes (lambda (name new-text &optional kind path)
             (pcase-let ((`(,symbol ,file ,types) (lispm-develop--definition name kind path)))
               ;; No buffer is visited to show a file that cannot be written.
               (lispm-tools-check-writable file)
               (when-let* ((buffer (lispm-tools-to-show file)))
                 (with-current-buffer buffer
                   (cons buffer (car (lispm-develop-edit symbol types file new-text)))))))
  (pcase-let* ((`(,symbol ,file ,types) (lispm-develop--definition name kind path))
               (buffer (lispm-tools-existing file)))
    (with-current-buffer buffer
      (pcase-let* ((`(,_otype ,obeg . ,oend) (lispm-develop-located symbol types file))
                   (old-form (save-restriction
                               (widen)
                               (buffer-substring-no-properties obeg oend)))
                   (`(,text ,beg ,end ,type) (lispm-develop-edit symbol types file new-text))
                   (clean-new (lispm-develop--new-form new-text symbol type))
                   (canonical (intern (symbol-name symbol))))
        (lispm-tools-set-text buffer text)
        (push (list :agent lispm-current-agent
                    :turn (and lispm-current-agent (lispm-agent-turn lispm-current-agent))
                    :symbol canonical :name (symbol-name canonical) :type type :file file
                    :old-form old-form :new-form clean-new
                    :lexical-binding (bound-and-true-p lexical-binding))
              lispm-develop-history)
        (setq lispm-develop-history (seq-take lispm-develop-history 100))
        (format "Replaced the definition of %s%s in %s, now lines %d-%d.  Emacs still runs \
the old one: evaluate this one with eval_definition."
                symbol
                (if (equal (symbol-name symbol) name) "" (format " (%s is an alias of it)" name))
                (lispm-tools-label file)
                (lispm-develop-line beg) (lispm-develop-line (1- end)))))))

;;;; Evaluating definitions

(defvar lispm-develop--value nil
  "The value of the form `lispm-develop--evaluate' evaluates.")

(defun lispm-develop--evaluate (beg)
  "Evaluate the top-level form around BEG in the current buffer, as C-M-x does.
That is as `eval-defun' evaluates it, and by the same means: a
`defvar', `defcustom' or `defface' is reset to what it says
\(`elisp--eval-defun-1'), with the buffer's `lexical-binding', and
`eval-region' records that the buffer's file defines what it defines.
Also updates the in-memory `who-calls' database for the form.
But the value is returned, not printed in the echo area."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (error "%s is in %s, not in Emacs Lisp mode" (buffer-name) mode-name))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char beg)
      (end-of-defun)
      (beginning-of-defun)
      (let* ((start (point))
             (raw-form (funcall load-read-function (current-buffer)))
             (end (point))
             (_ (ignore-errors (lispm-develop-index-form raw-form)))
             (form (eval-sexp-add-defvars (elisp--eval-defun-1 (macroexpand raw-form)) start))
             (lispm-develop--value nil))
        (eval-region start end nil (lambda (_stream)
                                     ;; Read once: at END, `eval-region' is done.
                                     (goto-char end)
                                     `(setq lispm-develop--value ,form)))
        lispm-develop--value))))

(lispm-deftool eval_definition
    ((name string "Name of the function, macro, variable or face whose definition to evaluate.")
     (kind string "Which definition of NAME (default: its function, else its variable, else its face)."
           :optional t :enum ("function" "variable" "face"))
     (path string "File to find it in, for a definition not evaluated yet (default: the file Emacs has it from)."
           :optional t))
  "Evaluate a definition into the running Emacs, from its source file.
This is what C-M-x (eval-defun) does in the file's buffer: with the
file's lexical binding, and a defvar, defcustom or defface is reset to
the value it now gives.  After edit_definition, or an edit_file of a
definition, this makes Emacs run the new code; then run its tests with
ert_run.  Returns the value: the name defined, or a variable's value."
  :permission exec
  :concurrency 'editor
  :deferred t
  :spotlight (lambda (result name &optional _kind path)
               (list :kind 'eval-definition
                     :subject name
                     :path path
                     :result result))
  :preview (lambda (name &optional kind path)
             (pcase-let ((`(,symbol ,file ,types) (lispm-develop--definition name kind path)))
               (with-temp-buffer
                 (insert (lispm-file-text file))
                 (pcase-let ((`(,_type ,beg . ,end) (lispm-develop-located symbol types file)))
                   (format "%s:%d\n%s" (lispm-tools-label file) (lispm-develop-line beg)
                           (buffer-substring-no-properties beg end))))))
  (pcase-let ((`(,symbol ,file ,types) (lispm-develop--definition name kind path)))
    (lispm-develop-in-buffer
     file
     (lambda ()
       (pcase-let* ((`(,type ,beg . ,_end) (lispm-develop-located symbol types file))
                    (old-arity (and (null type) (fboundp symbol)
                                    (condition-case nil (func-arity (indirect-function symbol))
                                      ((void-function file-error) nil))))
                    (value (lispm-develop--evaluate beg))
                    (new-arity (and (null type) (fboundp symbol)
                                    (condition-case nil (func-arity (indirect-function symbol))
                                      ((void-function file-error) nil))))
                    (callers (and old-arity new-arity (not (equal old-arity new-arity))
                                  (lispm-develop-who-calls symbol))))
         (concat (format "Evaluated the definition of %s in %s, line %d.\n%s"
                         symbol (lispm-tools-label file) (lispm-develop-line beg)
                         (lispm-tools-eval-result "" value))
                 (if callers
                     (format "\nCaller notice: arity changed from %S to %S; called by: %s"
                             old-arity new-arity
                             (mapconcat #'symbol-name callers ", "))
                   "")))))))


(provide 'lispm-develop-core)

;;; lispm-develop-core.el ends here
