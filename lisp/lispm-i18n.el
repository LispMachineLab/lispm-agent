;;; lispm-i18n.el --- The language LispM Agent speaks  -*- lexical-binding: t; -*-

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

;; LispM Agent's words are written in English in the code, and shown in
;; the language `lispm-current-language' names: `lispm-language' if set,
;; else the language of the program LispM Agent runs in, if it says one
;; (see `lispm-language-functions'), else the computer's own.
;; Every word a person reads passes through one of:
;;
;;   (lispm-tr MESSAGE ARGS...)      MESSAGE translated, then formatted
;;                                   with ARGS, if any
;;   (lispm-tr-n SINGULAR PLURAL COUNT ARGS...)
;;                                   the form for COUNT, formatted with
;;                                   ARGS, or with COUNT
;;   (lispm-tr-in CONTEXT MESSAGE ARGS...)
;;                                   MESSAGE as meant in CONTEXT, where
;;                                   the same English says two things
;;
;; Words kept in a list made as a file loads stay English there, each
;; marked with `lispm-tr-noop', and are translated where shown, so that a
;; change of language shows at once; `(lispm-tr-noop MESSAGE CONTEXT)'
;; keeps its context with it.  A translation may take the arguments in
;; another order with `%1$s'.
;;
;; What models read stays English: tool descriptions and results, system
;; prompts, transcripts.  A function shared by the user's commands and
;; the tools says its words in English while a tool runs, see
;; `lispm-in-english'.
;;
;; The engine is LispM Kit's, lispm-kit-i18n.el, which every LispM
;; program shares: LispM Agent keeps only its words, as the domain
;; `lispm', and their translations, the catalogs of i18n/, one per
;; language, named by its tag, as zh-Hans.eld (see lispm-kit-i18n.el
;; for what a catalog holds).  The English catalogs hold only what their
;; spelling changes: the code's English spells as Britain does.

;;; Code:

(require 'lispm-kit-i18n)

(defalias 'lispm-tr-noop #'lispm-kit-tr-noop
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-i18n.el.  MESSAGE as meant in
CONTEXT carries it, and `lispm-tr' translates it so.

\(fn MESSAGE &optional CONTEXT)")

(defconst lispm-languages lispm-kit-languages
  "The languages LispM Agent speaks: (TAG ENGLISH-NAME).  TAG names the
catalog of i18n/.")

(defcustom lispm-language nil
  "The language LispM Agent speaks: a tag of `lispm-languages', or nil.
Nil follows the program LispM Agent runs in, if it says its language
\(see `lispm-language-functions'), else the computer's (see
`lispm-system-language').  A new value shows at once."
  :type `(choice (const :tag "The computer's language, or that of the program around" nil)
                 ,@(mapcar (pcase-lambda (`(,tag ,name)) `(const :tag ,name ,tag))
                           lispm-languages))
  :group 'lispm)

(defconst lispm-i18n-directory
  (expand-file-name "../i18n" (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the catalogs, one per language: i18n/ of LispM
Agent's checkout.  This file requires nothing of LispM Agent's, so that
every other can require it.")

(defvar lispm-language-functions nil
  "Functions saying the language of the program LispM Agent runs in, for
it to speak while `lispm-language' is nil.  Each is called with no
arguments and returns a language tag, or nil; the first tag of
`lispm-languages' returned is spoken, else the computer's language.  A
program adding one calls `lispm-language-changed' once its language
changes.")

(defvar lispm-language-changed-hook nil
  "Hook run once LispM Agent speaks another language, to draw again what
shows words.")

(lispm-kit-i18n-define 'lispm
  :directory lispm-i18n-directory
  :language 'lispm-language
  :language-functions 'lispm-language-functions
  :changed-hook 'lispm-language-changed-hook)

(defalias 'lispm-system-language #'lispm-kit-system-language)

(defalias 'lispm-i18n-language-of #'lispm-kit-i18n-language-of)

(defalias 'lispm-i18n-forget #'lispm-kit-i18n-forget)

(defun lispm-current-language ()
  "Return the language LispM Agent speaks now, a tag: `lispm-language',
else the first a function of `lispm-language-functions' returns that it
speaks, else the computer's."
  (lispm-kit-current-language 'lispm))

;;;; Catalogs

(defun lispm-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (lispm-kit-i18n-catalog-file 'lispm language))

(defun lispm-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file; see `lispm-kit-i18n-read-catalog'."
  (lispm-kit-i18n-read-catalog 'lispm language))

(defun lispm-i18n-catalog (&optional language)
  "Return LANGUAGE's catalog, by default the current language's, read
once."
  (lispm-kit-i18n-catalog 'lispm language))

(defun lispm-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (lispm-kit-language-name 'lispm language))

;;;; Translating

(defmacro lispm-in-english (&rest body)
  "Run BODY saying every word as written, in English, and return its value.
What models read is said so: a tool's result, and the error of a
function the tool shares with the user's commands."
  (declare (indent 0) (debug t))
  `(lispm-kit-in-english ,@body))

(defun lispm-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the current
language, formatted with ARGS when there are any."
  (apply #'lispm-kit-tr-in 'lispm context message args))

(defun lispm-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when there
are any.  MESSAGE is the English written in the code; see lispm-i18n.el.
A MESSAGE marked with a context by `lispm-tr-noop' is translated as
meant in it."
  (apply #'lispm-kit-tr 'lispm message args))

(defun lispm-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language,
formatted with ARGS, or with COUNT when there are none."
  (apply #'lispm-kit-tr-n 'lispm singular plural count args))

(defun lispm-format-time (format &optional time)
  "Return TIME, by default now, as FORMAT, translated, says it, the names
of months and days in the current language; see `format-time-string'."
  (lispm-kit-format-time 'lispm format time))

;;;; Changing the language

(defun lispm-language-changed ()
  "Draw again what shows words, now that the language a function of
`lispm-language-functions' returns has changed, unless `lispm-language'
chooses one: a program saying its language calls this once it speaks
another."
  (lispm-kit-language-changed 'lispm))

(provide 'lispm-i18n)
;;; lispm-i18n.el ends here
