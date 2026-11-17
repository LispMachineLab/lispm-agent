;;; lispm-develop.el --- Developing Lisp in the running image  -*- lexical-binding: t; -*-

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

;; On a Lisp Machine a program grew in the world it ran in: edit one
;; definition, evaluate it into the running image, try it, and the
;; editor knew where every definition was and who used it.  These
;; tools give agents that loop for Emacs Lisp:
;;
;;   edit_definition   replaces the form defining a function, macro,
;;                     variable or face in its file, found as
;;                     `find-function' finds it, so that the model
;;                     writes the new form and never the old one;
;;   eval_definition   evaluates it into Emacs from the file's buffer,
;;                     as C-M-x does: a defvar, defcustom or defface is
;;                     reset to what it now says;
;;   ert_run           runs ERT tests in Emacs and reports the failures.
;;
;; and, for code in any language Emacs knows about:
;;
;;   xref_definitions, xref_references
;;                     ask the xref backend of the code's buffer:
;;                     Emacs Lisp's own, a language server's with eglot;
;;   flymake_diagnostics
;;                     runs the flymake backends of a buffer: for Emacs
;;                     Lisp the byte compiler and checkdoc.
;;
;; They are deferred tools, which search_tools finds (their source is
;; called "lisp"): most conversations need none of them.
;;
;; edit_definition changes a file as edit_file does: as one undo step,
;; journaled, saved, previewed as a diff and shown in place.  Its file
;; is found from the symbol, wherever Emacs loaded the definition from,
;; Emacs's own sources included; its :targets says which file that is,
;; so that a write outside the agent's root asks, whatever the policy.
;; eval_definition and ert_run run code: their class is `exec'.

;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'xref)
(require 'flymake)
(require 'ert)
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

(defconst lispm-develop--tools
  '("edit_definition" "eval_definition" "ert_run"
    "xref_definitions" "xref_references" "flymake_diagnostics")
  "The tools of this file.")

;;;; Code-object history and in-memory caller database (Genera Figure A & SCT)

(defvar lispm-develop--history nil
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

(defun lispm-develop--extract-callees (form)
  "Symbols called or referenced as `#'SYMBOL' inside FORM.
Handles circular structures and special forms (`let', `lambda', `cond',
`condition-case') without mistaking parameter lists or bindings for calls."
  (let ((seen (make-hash-table :test #'eq))
        callees)
    (cl-labels
        ((walk-list (list)
           (while (and (consp list) (not (gethash list seen)))
             (puthash list t seen)
             (walk (car list))
             (setq list (cdr list))))
         (walk-bindings (bindings single-ok)
           (if (and single-ok (consp bindings) (symbolp (car bindings))
                    (consp (cdr bindings)) (null (cddr bindings)))
               ;; Single-binding syntax: (when-let (x (car xs)) ...)
               (walk-list (cdr bindings))
             (while (and (consp bindings) (not (gethash bindings seen)))
               (puthash bindings t seen)
               (let ((b (car bindings)))
                 (when (and (consp b) (not (gethash b seen)))
                   (puthash b t seen)
                   (walk-list (cdr b))))
               (setq bindings (cdr bindings)))))
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
                ((memq head '(defun defmacro cl-defun defsubst))
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
                 (while (and (consp rest) (not (gethash rest seen)))
                   (puthash rest t seen)
                   (walk-list (car rest))
                   (setq rest (cdr rest))))
                ((eq head 'condition-case)
                 (when (and (consp rest) (consp (cdr rest)))
                   (walk (cadr rest))
                   (let ((handlers (cddr rest)))
                     (while (and (consp handlers) (not (gethash handlers seen)))
                       (puthash handlers t seen)
                       (when (consp (car handlers))
                         (walk-list (cdar handlers)))
                       (setq handlers (cdr handlers))))))
                ((eq head 'setq)
                 (let ((pairs rest))
                   (while (and (consp pairs) (not (gethash pairs seen)))
                     (puthash pairs t seen)
                     (setq pairs (cdr pairs))
                     (when (and (consp pairs) (not (gethash pairs seen)))
                       (puthash pairs t seen)
                       (walk (car pairs))
                       (setq pairs (cdr pairs))))))
                (t
                 (if (and (symbolp head)
                          (not (keywordp head)))
                     (unless (memq head '(nil t progn prog1 prog2 if when unless
                                              and or save-excursion save-restriction
                                              save-current-buffer unwind-protect
                                              while catch defvar defcustom defface))
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
    cl-defmethod cl-defgeneric defalias
    define-derived-mode define-minor-mode define-globalized-minor-mode
    ert-deftest lispm-deftool)
  "Top-level defining forms whose symbol is indexed in the `who-calls' graph.")

(defun lispm-develop-index-form (form &optional caller file)
  "Update the in-memory `who-calls' database for the top-level FORM.
CALLER, if not given, is taken from FORM when its head is in
`lispm-develop--callable-definers' (so `defvar', `defvar-local', `defcustom',
`defface', etc. never overwrite a same-named function); FILE is the source
file (by default `buffer-file-name')."
  (let* ((file (or file buffer-file-name))
         (head (and (consp form) (symbolp (car form)) (car form)))
         (caller (or caller
                     (and head
                          (memq head lispm-develop--callable-definers)
                          (consp (cdr form))
                          (symbolp (cadr form))
                          (cadr form)))))
    (when (and caller (symbolp caller))
      (lispm-develop--remove-caller caller file)
      (let* ((body (cond
                    ((memq head '(defun defmacro cl-defun defsubst))
                     (and (consp (cddr form)) (cdddr form)))
                    ((and (consp (cddr form)))
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
                              (if file (lispm-tools--label file) (buffer-name))
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
;; preview the file's text, see `lispm--file-text'), never by visiting
;; the file with `find-file-noselect', which would ask about a buffer
;; out of date; and never by expanding every macro of the file, which
;; is find-function's last resort and would run the file's code.

(defconst lispm-develop--kinds '(("function") ("variable" . defvar) ("face" . defface))
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

(defun lispm-develop--search (symbol type)
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
           for bounds = (lispm-develop--search symbol type)
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
                           (ignore-errors (find-library-name library)))))
      (cons symbol file))))

(defun lispm-develop--definition (name kind path)
  "Where to find the definition of the symbol NAME: (SYMBOL FILE TYPES).
KIND is \"function\", \"variable\" or \"face\", or nil for any, in
that order.  Given PATH, the definition is looked for in that file, as
for one not evaluated yet; else in the file Emacs has it from (see
`lispm-develop--source').  SYMBOL is the symbol whose definition is
meant, NAME's or that of the function NAME is an alias of; TYPES are
the types of definition to look for in FILE, in order (see
`lispm-develop--search').  Nothing is visited."
  (let ((types (if kind (list (cdr (assoc kind lispm-develop--kinds))) '(nil defvar defface)))
        (symbol (intern-soft name)))
    (if path
        (list (or symbol (make-symbol name)) (lispm-tools--file path) types)
      (or (and symbol
               (cl-loop for type in types
                        for source = (lispm-develop--source symbol type)
                        when source return (list (car source) (cdr source) (list type))))
          (error "Emacs knows no file that defines %s%s; if a file not evaluated yet \
defines it, give that file as path"
                 name (if kind (concat " as a " kind) ""))))))

(defun lispm-develop--located (symbol types file)
  "Where the current buffer, holding FILE, defines SYMBOL: (TYPE BEG . END).
TYPES are the types of definition to look for.  An error if there is none."
  (or (lispm-develop--find symbol types)
      (error "%s has no definition of %s; read it to see where it went"
             (lispm-tools--label file) symbol)))

(defun lispm-develop--buffer-of (file)
  "A buffer holding FILE's text as the user sees it, for reading.
The buffer visiting FILE, brought up to date unless it has unsaved
changes, or else FILE, visited now.  An error if FILE is not there."
  (or (lispm-tools--visiting file t)
      (and (file-regular-p file) (lispm-tools--find-file file))
      (error "No such file: %s" (lispm-tools--label file))))

(defun lispm-develop--in-buffer (file function)
  "Call FUNCTION in a buffer holding FILE; return its value.
With FILE nil, in an Emacs Lisp buffer of no file, whose directory is
the acting agent's root.  A buffer visited for the call is killed after
it: tools that only read leave no buffers behind."
  (if (null file)
      (with-temp-buffer
        (delay-mode-hooks (emacs-lisp-mode))
        (setq default-directory (file-name-as-directory (lispm-tools--root)))
        (funcall function))
    (let* ((open (find-buffer-visiting file))
           (buffer (lispm-develop--buffer-of file)))
      (unwind-protect
          (with-current-buffer buffer
            (funcall function))
        (unless (or open (not (buffer-live-p buffer)) (buffer-modified-p buffer))
          (kill-buffer buffer))))))

(defun lispm-develop--line (position)
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
            (unless (eql (car (lispm-develop--search symbol type)) start)
              (error "new_text does not define %s as a %s; to rename or remove a definition, use edit_file"
                     symbol (car (rassq type lispm-develop--kinds))))))))
    text))

(defun lispm-develop--edit (symbol types file new-text)
  "The current buffer's text, holding FILE, with SYMBOL's definition replaced.
NEW-TEXT replaces it, trimmed; TYPES are the types of definition to look
for.  Return (TEXT BEG END TYPE): the whole new text, where the new form
is in it, and what type of definition it replaced."
  (pcase-let* ((`(,type ,beg . ,end) (lispm-develop--located symbol types file))
               (old (lispm-tools--buffer-text (current-buffer)))
               (new (string-trim new-text)))
    (list (concat (substring old 0 (1- beg)) new (substring old (1- end)))
          beg (+ beg (length new)) type)))

(defun lispm-develop--edit-diff (name new-text &optional kind path)
  "NAME's file as it is and as edit_definition would leave it, and its label.
The definition is replaced with NEW-TEXT; KIND and PATH are
edit_definition's arguments."
  (pcase-let* ((`(,symbol ,file ,types) (lispm-develop--definition name kind path))
               (old (lispm--file-text file)))
    (with-temp-buffer
      (insert old)
      (pcase-let ((`(,text ,_beg ,_end ,type) (lispm-develop--edit symbol types file new-text)))
        (lispm-develop--new-form new-text symbol type)
        (list old text (lispm-tools--label file))))))

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
               (lispm-tools--check-writable file)
               (when-let* ((buffer (lispm-tools--to-show file)))
                 (with-current-buffer buffer
                   (cons buffer (car (lispm-develop--edit symbol types file new-text)))))))
  (pcase-let* ((`(,symbol ,file ,types) (lispm-develop--definition name kind path))
               (buffer (lispm-tools--existing file)))
    (with-current-buffer buffer
      (pcase-let* ((`(,_otype ,obeg . ,oend) (lispm-develop--located symbol types file))
                   (old-form (save-restriction
                               (widen)
                               (buffer-substring-no-properties obeg oend)))
                   (`(,text ,beg ,end ,type) (lispm-develop--edit symbol types file new-text))
                   (clean-new (lispm-develop--new-form new-text symbol type))
                   (canonical (intern (symbol-name symbol))))
        (lispm-tools--set-text buffer text)
        (push (list :agent lispm-current-agent
                    :turn (and lispm-current-agent (lispm-agent-turn lispm-current-agent))
                    :symbol canonical :name (symbol-name canonical) :type type :file file
                    :old-form old-form :new-form clean-new
                    :lexical-binding (bound-and-true-p lexical-binding))
              lispm-develop--history)
        (setq lispm-develop--history (seq-take lispm-develop--history 100))
        (format "Replaced the definition of %s%s in %s, now lines %d-%d.  Emacs still runs \
the old one: evaluate this one with eval_definition."
                symbol
                (if (equal (symbol-name symbol) name) "" (format " (%s is an alias of it)" name))
                (lispm-tools--label file)
                (lispm-develop--line beg) (lispm-develop--line (1- end)))))))

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
  :deferred t
  :preview (lambda (name &optional kind path)
             (pcase-let ((`(,symbol ,file ,types) (lispm-develop--definition name kind path)))
               (with-temp-buffer
                 (insert (lispm--file-text file))
                 (pcase-let ((`(,_type ,beg . ,end) (lispm-develop--located symbol types file)))
                   (format "%s:%d\n%s" (lispm-tools--label file) (lispm-develop--line beg)
                           (buffer-substring-no-properties beg end))))))
  (pcase-let ((`(,symbol ,file ,types) (lispm-develop--definition name kind path)))
    (lispm-develop--in-buffer
     file
     (lambda ()
       (pcase-let* ((`(,type ,beg . ,_end) (lispm-develop--located symbol types file))
                    (old-arity (and (null type) (fboundp symbol)
                                    (ignore-errors (func-arity (indirect-function symbol)))))
                    (value (lispm-develop--evaluate beg))
                    (new-arity (and (null type) (fboundp symbol)
                                    (ignore-errors (func-arity (indirect-function symbol)))))
                    (callers (and old-arity new-arity (not (equal old-arity new-arity))
                                  (lispm-develop-who-calls symbol))))
         (concat (format "Evaluated the definition of %s in %s, line %d.\n%s"
                         symbol (lispm-tools--label file) (lispm-develop--line beg)
                         (lispm-tools--eval-result "" value))
                 (if callers
                     (format "\nCaller notice: arity changed from %S to %S; called by: %s"
                             old-arity new-arity
                             (mapconcat #'symbol-name callers ", "))
                   "")))))))

;;;; Tests

(defun lispm-develop--selector (text)
  "The ERT selector TEXT stands for.
Lisp, if it starts with a parenthesis, a colon or a double quote, or is
t; else the name of a test, if one has that name; else a regexp
matching the names of tests."
  (let ((text (string-trim text)))
    (cond ((string-empty-p text) (error "Say which tests to run"))
          ((string-match-p "\\`\\(?:[(:\"]\\|t\\'\\)" text)
           (pcase-let ((`(,selector . ,end)
                        (condition-case nil
                            (read-from-string text)
                          (error (error "Cannot read the selector %s" text)))))
             (unless (string-blank-p (substring text end))
               (error "The selector %s holds more than one form" text))
             selector))
          ((when-let* ((symbol (intern-soft text)))
             (and (ert-test-boundp symbol) symbol)))
          (t text))))

(defun lispm-develop--ert-listener (event &rest arguments)
  "Follow a run of ert_run, as ERT calls a listener with EVENT and ARGUMENTS.
ERT records a test that C-g quits and goes on to the next; this stops
the run instead, so that C-g interrupts the call."
  (when (and (eq event 'test-ended) (ert-test-quit-p (nth 2 arguments)))
    (signal 'quit nil)))

(defun lispm-develop--ert-frames (test result)
  "The innermost calls of the backtrace of RESULT, a result of TEST.
Each is (FUNCTION . ARGUMENTS), innermost first: the calls made from
TEST's body, without the special forms and without the `signal' and
`ert-fail' that report a failed `should'."
  (let ((body (ert-test-body test)) frames)
    (catch 'body
      (dolist (frame (ert-test-result-with-condition-backtrace result))
        (when (eq (backtrace-frame-fun frame) body)
          (throw 'body nil))
        (when (backtrace-frame-evald frame)
          (push (cons (backtrace-frame-fun frame) (backtrace-frame-args frame)) frames))))
    (seq-drop-while (lambda (frame) (memq (car frame) '(signal ert-fail)))
                    (nreverse frames))))

(defun lispm-develop--ert-failure (test result)
  "What went wrong in RESULT, a result of TEST, as lines of text."
  (let ((print-length 12) (print-level 6) (print-escape-newlines t))
    (if (not (ert-test-result-with-condition-p result))
        (list "  It exited without a result, by a throw or the like.")
      (let ((frames (lispm-develop--ert-frames test result)))
        (append
         (mapcar (lambda (info) (format "  %s%s" (car info) (cdr info)))
                 (ert-test-result-with-condition-infos result))
         (list (concat "  " (lispm--truncate
                             (prin1-to-string (ert-test-result-with-condition-condition result))
                             1500)))
         (and frames
              (cons "  Backtrace, innermost call first:"
                    (mapcar (lambda (line) (concat "  " line))
                            (split-string (lispm-tools--backtrace-text (seq-take frames 8))
                                          "\n")))))))))

(defun lispm-develop--ert-report (tests seconds)
  "The result of ert_run, which ran TESTS in SECONDS."
  (let ((passed 0) (expected-failures 0) failures skipped surprises)
    (dolist (test tests)
      (let ((result (ert-test-most-recent-result test))
            (name (ert-test-name test)))
        (cond ((null result)
               (push (list (format "NOT RUN %s" name)) failures))
              ((ert-test-skipped-p result)
               (push (format "SKIPPED %s: %s" name
                             (let ((print-length 8) (print-level 4))
                               (lispm--oneline (prin1-to-string
                                                (cadr (ert-test-result-with-condition-condition result)))
                                               200)))
                     skipped))
              ((ert-test-result-expected-p test result)
               (if (ert-test-passed-p result) (cl-incf passed) (cl-incf expected-failures)))
              ((ert-test-passed-p result)
               (push (format "PASSED %s, which was expected to fail" name) surprises))
              (t
               (push (cons (format "FAILED %s" name) (lispm-develop--ert-failure test result))
                     failures)))))
    (string-join
     (append
      (list (format "Ran %d test%s in %.2f s: %s."
                    (length tests) (if (cdr tests) "s" "") seconds
                    (string-join
                     (delq nil (list (format "%d passed" passed)
                                     (format "%d failed" (length failures))
                                     (and skipped (format "%d skipped" (length skipped)))
                                     (and (> expected-failures 0)
                                          (format "%d failed as expected" expected-failures))
                                     (and surprises (format "%d passed unexpectedly"
                                                            (length surprises)))))
                     ", ")))
      (mapcar (lambda (lines) (concat "\n" (string-join lines "\n"))) (nreverse failures))
      (and surprises (cons "" (nreverse surprises)))
      (and skipped (cons "" (nreverse skipped))))
     "\n")))

(lispm-deftool ert_run
    ((selector string "Which tests: a regexp matching their names, a test's name, or an ERT selector in Lisp such as t (all), (tag NAME) or (not (tag slow)).")
     (file string "A file of tests to load first, relative to your root, when they are not in Emacs yet or have changed."
           :optional t))
  "Run ERT tests in this Emacs and report how they went.
Returns a summary, then each failure with its condition and the
innermost calls of its backtrace.  Run the tests of what you changed
once eval_definition has loaded it; give file to load tests first."
  :permission exec
  :deferred t
  (when file
    (let ((file (lispm-tools--file file)))
      (condition-case err
          (load file nil t t)
        (error (error "Loading %s failed: %s" (lispm-tools--label file) (error-message-string err))))))
  (let* ((selector (lispm-develop--selector selector))
         (tests (ert-select-tests selector t)))
    (if (null tests)
        (format "No tests match %S%s." selector
                (if file "" "; if they are in a file Emacs has not loaded, give it as file"))
      (let ((start (float-time)))
        (ert-run-tests `(member ,@tests) #'lispm-develop--ert-listener)
        (lispm-develop--ert-report tests (- (float-time) start))))))

;;;; Cross-references

(defun lispm-develop--xref-line (location)
  "The line of the xref LOCATION, or nil if it cannot be told without visiting.
An Emacs Lisp definition's line is found in its file's text as
edit_definition finds it."
  (cond ((xref-location-line location))
        ((xref-elisp-location-p location)
         (let ((file (xref-location-group location)))
           (when (and (stringp file) (file-readable-p file))
             (with-temp-buffer
               (insert (lispm--file-text file))
               (when-let* ((bounds (lispm-develop--search (xref-elisp-location-symbol location)
                                                          (xref-elisp-location-type location))))
                 (line-number-at-pos (car bounds)))))))
        ((xref-buffer-location-p location)
         (let ((marker (xref-location-marker location)))
           (with-current-buffer (marker-buffer marker)
             (lispm-develop--line marker))))))

(defun lispm-develop--xref-lines (items)
  "ITEMS, xref items, as text: a line FILE:LINE: SUMMARY each.
FILE is relative to the acting agent's root inside it; lines alike are
given once, and at most `lispm-xref-max-results' of them."
  (let (lines)
    (dolist (item items)
      (let* ((location (xref-item-location item))
             (group (xref-location-group location))
             (line (ignore-errors (lispm-develop--xref-line location)))
             (text (format "%s:%s %s"
                           (if (and (stringp group) (file-name-absolute-p group))
                               (lispm-tools--label group)
                             group)
                           (if line (format "%d:" line) "")
                           (lispm--oneline (substring-no-properties (xref-item-summary item)) 200))))
        (unless (member text lines)
          (push text lines))))
    (setq lines (nreverse lines))
    (concat (string-join (seq-take lines lispm-xref-max-results) "\n")
            (if (> (length lines) lispm-xref-max-results)
                (format "\n[... %d more]" (- (length lines) lispm-xref-max-results))
              ""))))

(defun lispm-develop--xref (identifier path line find)
  "What FIND finds for IDENTIFIER, as text (see `lispm-develop--xref-lines').
FIND is called with an xref backend and an identifier, and returns xref
items.  It is called in the buffer of the file PATH, or for Emacs Lisp
in the running Emacs if PATH is nil.  Point is put on IDENTIFIER first:
on LINE of PATH if given, where the backend is asked about the
identifier at point, else at its first occurrence, as a language server
answers about a position."
  (lispm-develop--in-buffer
   (and path (lispm-tools--file path))
   (lambda ()
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (let ((regexp (concat "\\_<" (regexp-quote identifier) "\\_>"))
               (case-fold-search nil))
           (cond (line
                  (unless (and (> line 0) (zerop (forward-line (1- line))))
                    (error "%s has no line %d" path line))
                  (unless (re-search-forward regexp (line-end-position) t)
                    (error "%s is not on line %d of %s" identifier line path))
                  (goto-char (match-beginning 0)))
                 ((re-search-forward regexp nil t)
                  (goto-char (match-beginning 0)))))
         (let* ((backend (or (xref-find-backend)
                             (error "Emacs has no xref backend for %s (%s)" (buffer-name) mode-name)))
                (id (or (and line (xref-backend-identifier-at-point backend)) identifier))
                (inhibit-message t)
                (items (funcall find backend id)))
           (if items
               (lispm-develop--xref-lines items)
             (format "Nothing found for %s." identifier))))))))

(lispm-deftool xref_definitions
    ((identifier string "The name to look up, such as an Emacs Lisp symbol.")
     (path string "A file of the code, whose language decides how to look (default: Emacs Lisp, in this Emacs)."
           :optional t)
     (line integer "The line of path where the identifier is, to ask about that occurrence (default: its first)."
           :optional t))
  "Find where an identifier is defined, through Emacs's xref: file:line: summary.
For Emacs Lisp (the default) from the definitions in this Emacs, of
functions, variables, faces and more; for another language, give a file
of it as path: its xref backend answers, a language server's with eglot."
  :permission read
  :deferred t
  (lispm-develop--xref identifier path line #'xref-backend-definitions))

(lispm-deftool xref_references
    ((identifier string "The name to look up, such as an Emacs Lisp symbol.")
     (path string "A file of the code, whose language decides how to look (default: Emacs Lisp, in this Emacs)."
           :optional t)
     (line integer "The line of path where the identifier is, to ask about that occurrence (default: its first)."
           :optional t)
     (everywhere boolean "Search the project's external roots too, for Emacs Lisp all of load-path: slow."
                 :optional t))
  "Find the uses of an identifier, through Emacs's xref: file:line: the line.
Who calls a function, who reads a variable: before changing a
definition, see what depends on it.  For Emacs Lisp (the default) the
project's files are searched; for another language, give a file of it
as path: its xref backend answers, a language server's with eglot."
  :permission read
  :deferred t
  (lispm-develop--xref
   identifier path line
   (lambda (backend id)
     ;; The project is the buffer's, never one asked for; outside one,
     ;; the buffer's directory stands for it.
     (let ((project-current-directory-override default-directory)
           (project-vc-external-roots-function
            (if everywhere project-vc-external-roots-function #'ignore)))
       (xref-backend-references backend id)))))

;;;; Diagnostics

(defun lispm-develop--severity (type)
  "The severity of the flymake diagnostic TYPE: error, warning or note."
  (pcase (or (get type 'flymake-category) type)
    ('flymake-error "error")
    ('flymake-warning "warning")
    ('flymake-note "note")
    (_ (string-remove-prefix ":" (format "%s" type)))))

(defun lispm-develop--diagnostic-line (diagnostic)
  "The flymake DIAGNOSTIC of the current buffer as LINE:COLUMN: TYPE: MESSAGE."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (flymake-diagnostic-beg diagnostic))
      (format "%d:%d: %s: %s" (line-number-at-pos) (1+ (current-column))
              (lispm-develop--severity (flymake-diagnostic-type diagnostic))
              (lispm--oneline (flymake-diagnostic-text diagnostic) 500)))))

(defun lispm-develop--not-run (backend)
  "Why the flymake BACKEND of the current buffer did not run, as a note."
  (if (and (eq backend 'elisp-flymake-byte-compile)
           (fboundp 'trusted-content-p)
           (not (trusted-content-p)))
      (format "[The byte compiler did not check %s: Emacs does not trust its content \
\(see `trusted-content'), and compiling it would run its code.  Nothing was said about \
compiler warnings; if the user trusts the file, they add it to `trusted-content'.]"
              (buffer-name))
    (let ((why (ignore-errors (flymake--state-disabled (gethash backend flymake--state)))))
      (format "[%s did not run: %s]" backend
              (cond ((stringp why) why)
                    ((and (consp why) (symbolp (car why))) (error-message-string why))
                    (why (format "%S" why))
                    (t "it disabled itself"))))))

(defun lispm-develop--diagnostics (label waiting)
  "What flymake found in the current buffer, as flymake_diagnostics says it.
LABEL names the buffer's file; WAITING are the backends that have not
reported yet."
  (let* ((diagnostics (sort (flymake-diagnostics)
                            (lambda (a b) (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))
         (disabled (flymake-disabled-backends))
         (ran (seq-difference (flymake-running-backends) waiting))
         (counts (mapcar (lambda (severity)
                           (let ((n (cl-count severity diagnostics
                                              :key (lambda (d) (lispm-develop--severity
                                                                (flymake-diagnostic-type d)))
                                              :test #'equal)))
                             (and (> n 0) (format "%d %s%s" n severity (if (= n 1) "" "s")))))
                         '("error" "warning" "note"))))
    (string-join
     (append
      (list (format "%s: %s, from %s." label
                    (if (delq nil counts) (string-join (delq nil counts) ", ") "no diagnostics")
                    (if ran (mapconcat #'symbol-name ran ", ") "no checker")))
      (mapcar #'lispm-develop--diagnostic-line diagnostics)
      (mapcar #'lispm-develop--not-run disabled)
      (and waiting (list (format "[Still running after %d s, not reported: %s]"
                                 lispm-diagnostics-timeout
                                 (mapconcat #'symbol-name waiting ", ")))))
     "\n")))

(defun lispm-develop--diagnose (buffer created done)
  "Run the flymake backends of BUFFER and call DONE with what they report.
BUFFER is left as it was found: `flymake-mode', if it was off there, is
turned on for the check and off again; and with CREATED non-nil, BUFFER
was visited for the check and is killed after it.  The backends report
asynchronously; the agent's turn in which the check started may end
meanwhile, and then no result is given."
  (let* ((agent lispm-current-agent)
         (turn (and agent (lispm-agent-busy-p agent) (lispm-agent-turn agent)))
         (label (if (buffer-file-name buffer) (lispm-tools--label (buffer-file-name buffer))
                  (buffer-name buffer)))
         (was-on (buffer-local-value 'flymake-mode buffer))
         (deadline (+ (float-time) lispm-diagnostics-timeout))
         timer)
    (cl-labels
        ((restore ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and flymake-mode (not was-on))
                 (flymake-mode -1)))
             (when (and created (not (buffer-modified-p buffer)))
               (kill-buffer buffer))))
         (finish (text &optional errorp)
           (when timer (cancel-timer timer))
           (restore)
           (unless (and turn (or (not (lispm-agent-busy-p agent))
                                 (/= turn (lispm-agent-turn agent))))
             (funcall done text errorp)))
         (check ()
           (condition-case err
               (if (not (buffer-live-p buffer))
                   (finish "The buffer was killed before its checkers reported." t)
                 (with-current-buffer buffer
                   (let ((waiting (seq-difference (flymake-running-backends)
                                                  (flymake-reporting-backends))))
                     (when (or (null waiting) (> (float-time) deadline))
                       (finish (lispm-develop--diagnostics label waiting))))))
             (error (finish (error-message-string err) t)))))
      (condition-case err
          (with-current-buffer buffer
            (unless (run-hook-wrapped 'flymake-diagnostic-functions (lambda (_backend) t))
              (error "Flymake has no checker for %s (%s)" label mode-name))
            (unless was-on
              (let ((flymake-start-on-flymake-mode nil))
                (flymake-mode 1)))
            (let ((inhibit-message t))
              ;; Disabled backends too: they say why they are.
              (flymake-start nil t)))
        (error (restore)
               (signal (car err) (cdr err))))
      (setq timer (run-at-time 0.05 0.1 #'check)))))

(lispm-deftool flymake_diagnostics
    ((path string "File to check, absolute or relative to your root." :optional t)
     (buffer string "A buffer to check instead, by name." :optional t))
  "Check a file or buffer with flymake, without building: line:col: type: message.
The checkers are those Emacs runs on it: for Emacs Lisp the byte
compiler (in a file the user trusts) and checkdoc, with eglot the
language server.  Check code after changing it."
  :async t
  :deferred t
  (cond (buffer (lispm-develop--diagnose (lispm-tools--buffer buffer) nil done))
        (path (let* ((file (lispm-tools--file path))
                     (open (find-buffer-visiting file))
                     (visited (lispm-develop--buffer-of file)))
                (lispm-develop--diagnose visited (not open) done)))
        (t (error "Give a path or a buffer to check"))))

;;;; In-memory caller lookup, granular definition revert, and SCT system definition

(defun lispm-develop--el-files (root)
  "Emacs Lisp files under ROOT, pruning `.git', `.deps' and cache directories."
  (directory-files-recursively
   root "\\.el\\'" nil
   (lambda (dir)
     (not (member (file-name-nondirectory (directory-file-name dir))
                  '(".git" ".deps" "eln-cache" "node_modules"))))))

(defun lispm-develop--index-path (path)
  "Index Emacs Lisp file or directory PATH into the `who-calls' database."
  (let ((target (lispm-tools--file path)))
    (cond
     ((file-directory-p target)
      (dolist (file (lispm-develop--el-files target))
        (with-temp-buffer
          (insert (lispm--file-text file))
          (lispm-develop-index-buffer (current-buffer) file))))
     ((file-regular-p target)
      (with-temp-buffer
        (insert (lispm--file-text target))
        (lispm-develop-index-buffer (current-buffer) target)))
     (t (error "No such file or directory: %s" (lispm-tools--label target))))))

(defun lispm-develop--history-entry (name &optional kind path agent)
  "Latest history entry for NAME (matching KIND, PATH and AGENT when given)."
  (let* ((name-str (if (symbolp name) (symbol-name name) (format "%s" name)))
         (target-type (and kind (cdr (assoc kind lispm-develop--kinds))))
         (target-file (and path (lispm-tools--file path))))
    (seq-find (lambda (h)
                (and (equal (or (plist-get h :name)
                                (symbol-name (plist-get h :symbol)))
                            name-str)
                     (or (null kind) (eq (plist-get h :type) target-type))
                     (or (null target-file) (equal (plist-get h :file) target-file))
                     (or (null agent) (eq (plist-get h :agent) agent))))
              lispm-develop--history)))

(defun lispm-develop--revert-latest-p (entry)
  "Non-nil if ENTRY is still in history and is the latest edit for its target."
  (and entry
       (memq entry lispm-develop--history)
       (eq entry
           (lispm-develop--history-entry
            (or (plist-get entry :name) (symbol-name (plist-get entry :symbol)))
            (car (rassq (plist-get entry :type) lispm-develop--kinds))
            (plist-get entry :file)
            nil))))

(defun lispm-revert-definition (name &optional kind path agent entry)
  "Restore the previous code-object definition of NAME from history.
Unlike full-file revert, this replaces only NAME's form in its file
\(leaving concurrent edits to other definitions intact).  KIND and PATH
disambiguate which definition of NAME to restore; AGENT filters history
by agent; ENTRY, when given, is the exact pinned history record.
Does not evaluate code: use `eval_definition' to load the restored
definition into Emacs."
  (let ((entry (or entry (lispm-develop--history-entry name kind path agent))))
    (unless entry
      (error "No recorded definition edit for %s%s"
             name (if path (format " in %s" path) "")))
    (unless (lispm-develop--revert-latest-p entry)
      (error "The definition edit for %s was already reverted or superseded" name))
    (let* ((symbol (plist-get entry :symbol))
           (type (plist-get entry :type))
           (file (plist-get entry :file))
           (old-form (plist-get entry :old-form))
           (new-form (plist-get entry :new-form))
           (buffer (lispm-tools--existing file)))
      (with-current-buffer buffer
        (pcase-let* ((`(,_t ,cbeg . ,cend) (lispm-develop--located symbol (list type) file))
                     (current (string-trim (save-restriction
                                             (widen)
                                             (buffer-substring-no-properties cbeg cend)))))
          (when (and new-form (not (equal current (string-trim new-form))))
            (error "The definition of %s in %s has changed since that edit"
                   symbol (lispm-tools--label file)))
          (pcase-let ((`(,text ,beg ,end ,_t)
                       (lispm-develop--edit symbol (list type) file old-form)))
            (lispm-tools--set-text buffer text)
            (setq lispm-develop--history (delq entry lispm-develop--history))
            (format "Restored the definition of %s in %s (lines %d-%d).  If the newer \
definition was evaluated, evaluate this one with eval_definition."
                    symbol (lispm-tools--label file)
                    (lispm-develop--line beg) (lispm-develop--line (1- end)))))))))

(defun lispm-develop--module-deps (text)
  "Extract `(PROVIDES . REQUIRES)' from top-level Lisp forms in TEXT.
Reads forms without evaluating them so strings and comments are ignored."
  (let (provides requires)
    (cl-labels
        ((walk (form)
           (when (consp form)
             (pcase form
               (`(require (quote ,(and feat (pred symbolp))) . ,_)
                (push feat requires))
               (`(provide (quote ,(and feat (pred symbolp))) . ,_)
                (push feat provides))
               (`(,(or 'progn 'eval-when-compile 'eval-and-compile
                       'when 'unless 'if 'with-eval-after-load)
                  . ,body)
                (dolist (sub (if (proper-list-p body) body nil))
                  (walk sub)))
               (`(cond . ,clauses)
                (dolist (clause (if (proper-list-p clauses) clauses nil))
                  (when (proper-list-p clause)
                    (mapc #'walk clause))))))))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (walk (read (current-buffer))))
          (end-of-file nil)
          (error nil))))
    (cons (nreverse (delete-dups provides))
          (nreverse (delete-dups requires)))))

(defun lispm-system-definition (&optional root)
  "Inspect the Emacs Lisp system structure under ROOT (as Genera's SCT did).
Return a plist `(:root ROOT :modules MODULES :modified MODIFIED)' where
each module has `:file', `:provides', `:requires' and `:loaded'."
  (let* ((root (file-name-as-directory
                (if root (lispm-tools--file root) (lispm-tools--root))))
         (files (lispm-develop--el-files root))
         modules)
    (dolist (file files)
      (pcase-let ((`(,provides . ,requires)
                   (lispm-develop--module-deps (lispm--file-text file))))
        (push (list :file (file-relative-name file root)
                    :provides provides
                    :requires requires
                    :loaded (and provides (seq-every-p #'featurep provides) t))
              modules)))
    (let ((modified
           (delete-dups
            (delq nil
                  (mapcar (lambda (h)
                            (and (file-in-directory-p (plist-get h :file) root)
                                 (plist-get h :symbol)))
                          lispm-develop--history)))))
      (list :root root :modules (nreverse modules) :modified modified))))

(defun lispm-develop--patch-form (text lexical)
  "Format definition TEXT for an incremental patch with LEXICAL binding.
When TEXT defines a variable or face (`defvar', `defcustom', `defface'),
applies `elisp--eval-defun-1' so loading the patch resets an already-bound
definition just as `eval_definition' (`C-M-x') does."
  (let* ((parsed (car (read-from-string text)))
         (resetp (and (consp parsed)
                      (memq (car parsed) '(defvar defcustom defface))))
         (effective (if resetp
                        (elisp--eval-defun-1 (macroexpand parsed))
                      parsed)))
    (cond ((not lexical)
           (format "(eval '%S nil)" effective))
          (resetp
           (format "%S" effective))
          (t text))))

(defun lispm-develop-patch (&optional root agent)
  "Generate a Genera-style incremental code-object patch for ROOT (and AGENT).
Includes the latest modified definition per `(FILE NAME TYPE)' from
`lispm-develop--history', preserving each file's lexical or dynamic
binding semantics."
  (let* ((root (and root (file-name-as-directory (lispm-tools--file root))))
         (seen (make-hash-table :test #'equal))
         entries)
    (dolist (h lispm-develop--history)
      (let ((key (list (plist-get h :file)
                       (or (plist-get h :name) (symbol-name (plist-get h :symbol)))
                       (plist-get h :type))))
        (when (and (not (gethash key seen))
                   (or (null root) (file-in-directory-p (plist-get h :file) root))
                   (or (null agent) (eq (plist-get h :agent) agent)))
          (puthash key t seen)
          (push h entries))))
    (if (null entries)
        ";;; Incremental patch: no modified definitions recorded.\n"
      (concat ";;; Incremental Patch --- Code objects modified in image  -*- lexical-binding: t; -*-\n\n"
              (mapconcat
               (lambda (h)
                 (let ((form (plist-get h :new-form))
                       (lexical (plist-get h :lexical-binding)))
                   (format ";; %s (%s%s)\n%s\n"
                           (plist-get h :symbol)
                           (if root
                               (file-relative-name (plist-get h :file) root)
                             (abbreviate-file-name (plist-get h :file)))
                           (if lexical "" ", lexical-binding: nil")
                           (lispm-develop--patch-form form lexical))))
               entries
               "\n")))))

;;;; The source of these tools

(defun lispm-develop--sources ()
  "The tools of this file, as a source of deferred tools for search_tools."
  (list (list :name "lisp"
              :description (concat "edit, evaluate and test Emacs Lisp definitions in this Emacs; "
                                   "definitions, references and diagnostics of code in any "
                                   "language Emacs has xref or flymake for")
              :pending nil
              :prepare (lambda (callback) (funcall callback nil))
              :provides (lambda (name) (member name lispm-develop--tools)))))

(add-hook 'lispm-tool-sources #'lispm-develop--sources)

(provide 'lispm-develop)
;;; lispm-develop.el ends here
