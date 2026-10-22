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
;;   `lispm-chat-frob'     a symbol no loaded code mentions, in a
;;                         namespace this Emacs has (lispm-, org-, ...)
;;   `(car x y)'           a call giving a function more or fewer
;;                         arguments than it takes
;;   `lisp/nothere.el'     a path to a file that is not there
;;
;; A symbol some loaded code mentions is a word this Emacs knows (a text
;; property, a plist key) and passes, defined or not; so does a name the
;; reply defines itself, in a code block, or a library on `load-path'.
;; What is found is data, (undefined NAME), (arity SYMBOL GIVEN ARITY)
;; or (missing FILE); the chat shows such code in `lispm-ungrounded' and
;; says why under the mouse.  Only what is not there is found: a
;; function that exists but does something else passes, as it would in
;; PceEmacs.

;;; Code:

(require 'subr-x)
(require 'lispm-i18n)

(defface lispm-ungrounded
  '((((supports :underline (:style wave))) :underline (:style wave :color "orange"))
    (t :underline t))
  "Face added to code in replies that names what this Emacs lacks."
  :group 'lispm)

(defcustom lispm-ground-code t
  "Whether code in replies is checked against this Emacs.
Code naming a symbol nothing defines, calling a function with the wrong
number of arguments or naming a file that is not there is then shown in
`lispm-ungrounded'; see lispm-ground.el."
  :type 'boolean
  :group 'lispm)

(defconst lispm-ground--symbol-re
  "\\`\\(?:#?'\\)?\\([a-z][a-z0-9]*\\(?:--?[a-z0-9]+\\)+[?!*]?\\)\\'"
  "Code that looks like the symbol of a package: lowercase words joined
by dashes, quoted or not.")

(defconst lispm-ground--definition-re
  "(\\(?:[a-z0-9-]+-\\)?def[a-z-]*\\*?[ \t\n]+'?\\([^][ \t\n()'\"]+\\)"
  "A definition, `defun', `cl-defmethod' or `lispm-deftool' alike, and
the name it defines.")

(defvar lispm-ground--namespaces (make-hash-table :test #'equal)
  "Prefixes, a word and a dash, of the symbols this Emacs defines, as
they are found.")

(defvar lispm-ground--libraries (make-hash-table :test #'equal)
  "Names looked up on `load-path', to whether a library has the name.")

(defun lispm-ground--defined-p (symbol)
  "Whether SYMBOL is a function, a variable, a face or a feature."
  (or (fboundp symbol) (boundp symbol) (facep symbol) (featurep symbol)))

(defun lispm-ground--namespace-p (name)
  "Whether symbols this Emacs defines start as NAME does, to its first dash."
  (let ((prefix (substring name 0 (1+ (string-search "-" name)))))
    (or (gethash prefix lispm-ground--namespaces)
        (and (try-completion prefix obarray #'lispm-ground--defined-p)
             (puthash prefix t lispm-ground--namespaces)))))

(defun lispm-ground--library-p (name)
  "Whether a library on `load-path' is called NAME."
  (let ((found (gethash name lispm-ground--libraries 'unknown)))
    (when (eq found 'unknown)
      (setq found (and (locate-library name) t))
      (puthash name found lispm-ground--libraries))
    found))

(defun lispm-ground--undefined-p (name defined)
  "Whether NAME, the name of a symbol, names nothing in this Emacs.
No loaded code mentions it, it is not among DEFINED, the names the reply
defines, nor a library's; yet symbols this Emacs defines start as it
does.  Words with no namespace here, a branch name say, are not taken
for symbols."
  (and (not (intern-soft name))
       (not (member name defined))
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

(defun lispm-ground--call (form defined)
  "What is wrong with FORM, a call written as code, or nil.
Its function is a name nothing defines, or one taking more or fewer
arguments than FORM gives it.  DEFINED are the names the reply defines."
  (when (and (consp form) (symbolp (car form)) (proper-list-p form))
    (let* ((name (symbol-name (car form)))
           (symbol (intern-soft name)))
      (cond ((null symbol)
             (and (string-match-p lispm-ground--symbol-re name)
                  (lispm-ground--undefined-p name defined)
                  (list 'undefined name)))
            ((fboundp symbol)
             (let ((function (indirect-function symbol))
                   (given (1- (length form))))
               ;; An autoload's arity is known only once its file is
               ;; loaded, and a macro or special form takes its
               ;; arguments as it pleases.
               (unless (or (autoloadp function) (macrop function) (special-form-p function))
                 (let ((arity (ignore-errors (func-arity function))))
                   (when (and arity (or (< given (car arity))
                                        (and (natnump (cdr arity)) (> given (cdr arity)))))
                     (list 'arity symbol given arity))))))))))

(defun lispm-ground--path (code)
  "The path CODE is, without a line after a colon, if it looks like one.
That is, it starts at the home or the current or parent directory, at
the root with a directory there is, or with a directory, no dot in it,
and ends with an extension; and it has no space, wildcard, variable,
scheme or remote host."
  (let ((path (if (string-match "\\`\\(.+\\):[0-9]+\\'" code) (match-string 1 code) code)))
    (and (not (string-match-p "[][ \t\n*?{}$<>:\"'`|;&=,()#%\\]" path))
         (cond ((string-match "\\`/\\([^/]+\\)/[^/]" path)
                ;; Not the path of a URL, /api/v1/users say.
                (and (file-directory-p (concat "/" (match-string 1 path))) path))
               ((string-match-p "\\`\\(?:~\\|\\.\\.?\\)/" path) path)
               ((string-match-p "\\`[[:alpha:]_][^/.]*/\\(?:.*/\\)?[^/]*\\.[[:alnum:]]+\\'" path)
                path)))))

(defun lispm-ground-definitions (text)
  "The names TEXT defines in code, as strings."
  (let (names (start 0))
    (while (string-match lispm-ground--definition-re text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    names))

(defun lispm-ground-check (code &optional root defined)
  "What CODE, set as code in a reply, names that this Emacs lacks, or nil.
That is (undefined NAME), a symbol nothing defines, see lispm-ground.el;
\(arity SYMBOL GIVEN ARITY), a call giving GIVEN arguments to a function
whose `func-arity' is ARITY; or (missing FILE), a path, relative to
ROOT (by default `default-directory'), to a file that is not there.
DEFINED are the names the reply defines."
  (let ((code (string-trim (substring-no-properties code))))
    (cond ((string-prefix-p "(" code)
           (lispm-ground--call (lispm-ground--read code) defined))
          ((string-match lispm-ground--symbol-re code)
           (let ((name (match-string 1 code)))
             (and (lispm-ground--undefined-p name defined) (list 'undefined name))))
          ((when-let* ((path (lispm-ground--path code))
                       (file (expand-file-name path (or root default-directory))))
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
