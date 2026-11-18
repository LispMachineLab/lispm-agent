;;; lispm-ground.el --- Code in replies, checked against this Emacs  -*- lexical-binding: t; -*-

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

;; A model writes a name with the same confidence whether or not there
;; is anything by that name: a function no library defines reads as
;; well as one that exists, and asking the model again asks the source
;; of the mistake.  The Emacs it talks to knows.  SWI-Prolog's PceEmacs
;; colours a call to an undefined predicate as it is typed, by asking
;; the loaded program itself; LispM asks the running image the same of
;; the code in a reply:
;;
;;   `lispm-chat-frob'     a symbol no loaded code mentions, in the
;;                         namespace of a loaded library (lispm-, org-,
;;                         cl- of cl-lib, ...)
;;   `(lispm-frob x)'      a call of a function nothing defines
;;   `(car x y)'           a call giving a function more or fewer
;;                         arguments than it takes
;;   `lisp/nothere.el'     a path to a file that is not there
;;
;; A symbol some loaded code mentions is a word this Emacs knows (a text
;; property, a plist key) and passes unless it is called; so do a name
;; the reply defines itself in a code block, and the names made from
;; it (a mode's -hook, a struct's accessors), a library on `load-path',
;; and words of other languages written with dashes, text-align say,
;; which no library's namespace holds.  What is found is data,
;; (undefined NAME), (arity SYMBOL GIVEN ARITY) or (missing FILE); the
;; chat shows such code in `lispm-ungrounded' and says why under the
;; mouse (or with C-h .).  Only what is not there is found: a function
;; that exists but does something else passes, as it would in PceEmacs.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)

(defface lispm-ungrounded
  '((((supports :underline (:style wave))) :underline (:style wave :color "orange"))
    (t :underline t))
  "Face added to code in replies that names what this Emacs lacks."
  :group 'lispm)

(defvar lispm-ground-change-hook nil
  "Hook run after `lispm-ground-code' is set, to draw replies again.")

(defcustom lispm-ground-code t
  "Whether code in replies is checked against this Emacs.
Code naming a symbol nothing defines, calling a function with the wrong
number of arguments or naming a file that is not there is then shown in
`lispm-ungrounded'; see lispm-ground.el.  Set it with `setopt' or
Customize for the chats open to follow."
  :type 'boolean
  :group 'lispm
  :set (lambda (symbol value)
         (set-default symbol value)
         (run-hooks 'lispm-ground-change-hook)))

(defcustom lispm-ground-layer-check nil
  "Whether `lispm-ground-check' warns when code crosses package layer boundaries.
In an open system without kernel barriers (as Walker, Moon, Weinreb and
McMahon noted of Symbolics Genera), layer boundaries are invisible unless
explicitly checked.  When non-nil (`t' for all loaded namespaces, or a
list of package prefix strings such as \\='(\"lispm\" \"org\")), references
to internal `PKG--NAME' symbols that the reply itself does not define
are reported as `(internal SYMBOL PUBLIC-ALTERNATIVES)'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "All namespaces" t)
                 (repeat :tag "Protected prefixes" string))
  :group 'lispm
  :set (lambda (symbol value)
         (set-default symbol value)
         (run-hooks 'lispm-ground-change-hook)))

(defconst lispm-ground--symbol-re
  "\\`\\(?:#?'\\)?\\([a-z][a-z0-9]*\\(?:--?[a-z0-9]+\\)+[?!*]?\\)\\'"
  "Code that looks like the symbol of a package: lowercase words joined
by dashes, quoted or not.")

(defconst lispm-ground--definition-re
  "(\\(?:[a-z0-9-]+-\\)?def[a-z-]*\\*?[ \t\n]+'?\\([^][ \t\n()'\"]+\\)"
  "A definition, `defun', `cl-defmethod' or `lispm-deftool' alike, and
the name it defines.")

(defvar lispm-ground--libraries nil
  "The `load-path' names were last looked up on, and a hash table from
those names to whether a library there has the name.")

(defun lispm-ground--namespace-p (name)
  "Whether NAME is in the namespace of a library loaded.
That is, it starts with the name of a feature and a dash, as org-agenda
does; or with the prefix of a feature called PREFIX-lib, as cl-incf
does with cl-lib's.  The feature `emacs' has no namespace."
  (seq-some (lambda (feature)
              (let ((library (symbol-name feature)))
                (and (not (eq feature 'emacs))
                     (string-prefix-p (concat (string-remove-suffix "-lib" library) "-") name))))
            features))

(defun lispm-ground--library-p (name)
  "Whether a library on `load-path' is called NAME."
  (unless (eq (car lispm-ground--libraries) load-path)
    (setq lispm-ground--libraries (cons load-path (make-hash-table :test #'equal))))
  (let* ((libraries (cdr lispm-ground--libraries))
         (found (gethash name libraries 'unknown)))
    (when (eq found 'unknown)
      (setq found (and (locate-library name) t))
      (puthash name found libraries))
    found))

(defun lispm-ground--defined-by (name defined)
  "Whether NAME is among DEFINED, the names the reply defines, or made
from one: the -hook of a mode, the accessor of a struct."
  (seq-some (lambda (definition)
              (or (equal name definition) (string-prefix-p (concat definition "-") name)))
            defined))

(defun lispm-ground--undefined-p (name defined)
  "Whether NAME, the name of a symbol, names nothing in this Emacs.
No loaded code mentions it, the reply does not define it (DEFINED are
the names it does), no library has the name, and it is in the namespace
of a library loaded: words of other languages, text-align or a branch
name say, are not taken for symbols."
  (and (not (intern-soft name))
       (not (lispm-ground--defined-by name defined))
       (lispm-ground--namespace-p name)
       (not (lispm-ground--library-p name))))

(defun lispm-ground--read (code)
  "The form CODE is, or nil.
Its symbols are read into an obarray of their own: reading a reply
leaves the names it makes up unknown to this Emacs."
  (let ((obarray (obarray-make)))
    (condition-case nil
        (pcase-let ((`(,form . ,end) (read-from-string code)))
          (and (= end (length code)) form))
      (error nil))))

(defun lispm-ground--internal-check (symbol defined)
  "Check whether SYMBOL crosses an internal package boundary (`PKG--NAME').
When `lispm-ground-layer-check' covers SYMBOL's package and DEFINED does
not define SYMBOL, return `(internal SYMBOL PUBLIC-ALTERNATIVES)'."
  (when (and lispm-ground-layer-check symbol)
    (let ((name (symbol-name symbol)))
      (when (and (not (lispm-ground--defined-by name defined))
                 (string-match "\\`\\([a-z0-9-]+?\\)--\\([a-z0-9-]+\\)\\'" name))
        (let ((pkg (match-string 1 name))
              (stem (match-string 2 name)))
          (when (or (eq lispm-ground-layer-check t)
                    (and (listp lispm-ground-layer-check)
                         (seq-some (lambda (prefix)
                                     (or (equal pkg prefix)
                                         (string-prefix-p (concat prefix "-") pkg)))
                                   lispm-ground-layer-check)))
            (let* ((direct (intern-soft (concat pkg "-" stem)))
                   (public (and direct (or (fboundp direct) (boundp direct))
                                (list direct))))
              (list 'internal symbol public))))))))

(defun lispm-ground--call (form defined)
  "What is wrong with FORM, a call written as code, or nil.
Its function is a name nothing defines, one taking more or fewer
arguments than FORM gives it, or an internal symbol when
`lispm-ground-layer-check' is active.  DEFINED are the names the reply defines."
  (when (and (consp form) (symbolp (car form)) (proper-list-p form))
    (let* ((name (symbol-name (car form)))
           (symbol (intern-soft name)))
      (cond ((not (and symbol (fboundp symbol)))
             ;; Called, a name some code mentions is no less wanting:
             ;; the call of a made-up function an agent tried, say.
             (and (string-match-p lispm-ground--symbol-re name)
                  (not (lispm-ground--defined-by name defined))
                  (lispm-ground--namespace-p name)
                  (list 'undefined name)))
            (t
             (let ((function (indirect-function symbol))
                   (given (1- (length form))))
               ;; An autoload's arity is known only once its file is
               ;; loaded, and a macro or special form takes its
               ;; arguments as it pleases.
               (or (unless (or (autoloadp function) (macrop function) (special-form-p function))
                     (let ((arity (ignore-errors (func-arity function))))
                       (when (and arity (or (< given (car arity))
                                            (and (natnump (cdr arity)) (> given (cdr arity)))))
                         (list 'arity symbol given arity))))
                   (lispm-ground--internal-check symbol defined))))))))

(defun lispm-ground--path (code &optional root)
  "The path CODE is, without a line after a colon, if it looks like one.
That is, it starts at the home or the current or parent directory, at
the root with a directory there is (on ROOT's machine when ROOT is
remote), or with a directory, no dot in it, and ends with an extension;
and it has no space, wildcard, variable, scheme or remote host."
  (let ((path (if (string-match "\\`\\(.+?\\)\\(?::L?\\|#L\\)[0-9]+\\(?:[-–—,:L0-9]*[0-9]\\)?\\'" code)
                  (match-string 1 code)
                code))
        (remote (file-remote-p (or root default-directory))))
    (and (not (string-match-p "[][ \t\n*?{}$<>:\"'`|;&=,()#%\\]" path))
         (cond ((string-match "\\`/\\([^/]+\\)/[^/]" path)
                ;; Not the path of a URL, /api/v1/users say.
                (and (file-directory-p (concat (or remote "") "/" (match-string 1 path))) path))
               ((string-match-p "\\`\\(?:~\\|\\.\\.?\\)/" path) path)
               ((string-match-p "\\`[[:alpha:]_][^/.]*/\\(?:.*/\\)?[^/]*\\.[[:alnum:]]+\\'" path)
                path)))))

(defun lispm-ground-definitions (text)
  "The names TEXT defines in code, as strings."
  (let ((case-fold-search nil)
        (start 0)
        names)
    (while (string-match lispm-ground--definition-re text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    names))

(defun lispm-ground-check (code &optional root defined)
  "What CODE, set as code in a reply, names that this Emacs lacks, or nil.
That is (undefined NAME), a symbol nothing defines, see lispm-ground.el;
\(arity SYMBOL GIVEN ARITY), a call giving GIVEN arguments to a function
whose `func-arity' is ARITY; (internal SYMBOL PUBLIC), an internal
`PKG--NAME' symbol when `lispm-ground-layer-check' is active; or
\(missing FILE), a path, relative to ROOT (by default
`default-directory'), to a file that is not there.
DEFINED are the names the reply defines."
  (let* ((code (string-trim (substring-no-properties code)))
         (dir (or root default-directory))
         (remote (file-remote-p dir))
         (case-fold-search nil))
    (cond ((string-prefix-p "(" code)
           (lispm-ground--call (lispm-ground--read code) defined))
          ((string-match lispm-ground--symbol-re code)
           (let ((name (match-string 1 code)))
             (if (lispm-ground--undefined-p name defined)
                 (list 'undefined name)
               (lispm-ground--internal-check (intern-soft name) defined))))
          ((when-let* ((path (lispm-ground--path code dir))
                       (file (expand-file-name
                              (if (and remote (file-name-absolute-p path) (not (file-remote-p path)))
                                  (concat remote path)
                                path)
                              dir)))
             (unless (file-exists-p file) (list 'missing file)))))))

(defun lispm-ground--arguments (arity)
  "How many arguments ARITY, as `func-arity' gives it, allows, in words."
  (pcase-let ((`(,min . ,max) arity))
    (cond ((eql min max) (lispm-tr-n "%d argument" "%d arguments" min))
          ((natnump max) (lispm-tr "%d to %d arguments" min max))
          (t (lispm-tr-n "at least %d argument" "at least %d arguments" min)))))

(defun lispm-ground-describe (finding)
  "FINDING of `lispm-ground-check', in words."
  (pcase finding
    (`(undefined ,name) (lispm-tr "%s is not defined in this Emacs" name))
    (`(arity ,symbol ,given ,arity)
     (lispm-tr "%s takes %s; %d given here" symbol (lispm-ground--arguments arity) given))
    (`(internal ,symbol ,public)
     (if public
         (lispm-tr "%s is internal; prefer public interface: %s"
                   symbol (mapconcat #'symbol-name public ", "))
       (lispm-tr "%s is an internal symbol" symbol)))
    (`(missing ,file) (lispm-tr "No such file: %s" (abbreviate-file-name file)))))

(defun lispm-ground-mark (string finding)
  "STRING, as code naming what this Emacs lacks, as FINDING says.
Its face gains `lispm-ungrounded', its property `lispm-ground' is
FINDING, and what it shows under the mouse says why."
  (let* ((string (copy-sequence string))
         (end (length string))
         (echo (get-text-property 0 'help-echo string)))
    (add-face-text-property 0 end 'lispm-ungrounded t string)
    (add-text-properties 0 end
                         (list 'lispm-ground finding
                               'help-echo (concat (and (stringp echo) (concat echo "\n"))
                                                  (lispm-ground-describe finding)))
                         string)
    string))

(provide 'lispm-ground)
;;; lispm-ground.el ends here
