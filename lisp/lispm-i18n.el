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
;; The translations are the catalogs of i18n/, one per language, named by
;; its tag, as zh-Hans.eld:
;;
;;   (:name "NAME" :messages ((MESSAGE . TRANSLATION) ...))
;;
;; NAME is the language's name in itself.  A MESSAGE said in a context is
;; (CONTEXT . MESSAGE); a plural's TRANSLATION is a list of its forms, as
;; many as the language has, see `lispm-i18n-plural-forms'.  A message
;; missing from the catalog shows as written.  The English catalogs hold
;; only what their spelling changes: the code's English spells as
;; Britain does.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun lispm-tr-noop (message &optional context)
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-i18n.el.  MESSAGE as meant in
CONTEXT carries it, and `lispm-tr' translates it so."
  (if context (propertize message 'lispm-i18n-context context) message))

(defconst lispm-languages
  `((en-US ,(lispm-tr-noop "English (US)")) (en-GB ,(lispm-tr-noop "English (UK)"))
    (de ,(lispm-tr-noop "German")) (fr ,(lispm-tr-noop "French"))
    (ja ,(lispm-tr-noop "Japanese"))
    (zh-Hans ,(lispm-tr-noop "Chinese, Simplified"))
    (zh-Hant ,(lispm-tr-noop "Chinese, Traditional")))
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
Agent's checkout.  This file requires nothing of LispM's, so that every
other can require it.")

(defconst lispm-i18n-plural-forms
  '((de . lispm-i18n--one-else-other)
    (en-GB . lispm-i18n--one-else-other)
    (en-US . lispm-i18n--one-else-other)
    (fr . lispm-i18n--up-to-one-else-other))
  "How each language picks the plural form of a count: (TAG . FUNCTION),
FUNCTION returning the index of the form for a count.  A language not
listed has one form for every count, as Japanese and Chinese.")

(defun lispm-i18n--one-else-other (count)
  "Return the plural form for COUNT where one is singular: 0 or 1."
  (if (= count 1) 0 1))

(defun lispm-i18n--up-to-one-else-other (count)
  "Return the plural form for COUNT where 0 and 1 are singular, as in
French: 0 or 1."
  (if (< (abs count) 2) 0 1))

;;;; The computer's language

(defvar lispm-i18n--system-languages 'unread
  "The computer's preferred languages, most preferred first, once read.")

(defvar lispm-i18n--system-language '(unread)
  "The computer's language once found: (LANGUAGES . LANGUAGE), LANGUAGE
the one LispM speaks of the preferred LANGUAGES it was found among.")

(defun lispm-i18n--read-system-languages ()
  "Return the computer's preferred languages, most preferred first, as
language tags or locale names: macOS's AppleLanguages, else the
LANGUAGE, LC_ALL, LC_MESSAGES and LANG variables."
  (or (when (eq system-type 'darwin)
        (with-temp-buffer
          (when (eq 0 (ignore-errors (call-process "defaults" nil t nil
                                                   "read" "-g" "AppleLanguages")))
            (goto-char (point-min))
            (let ((tags nil))
              (while (re-search-forward "\"?\\([A-Za-z]+\\(?:[-_][A-Za-z0-9]+\\)*\\)\"?[,\n]" nil t)
                (push (match-string 1) tags))
              (nreverse tags)))))
      (seq-mapcat (lambda (variable)
                    (when-let* ((value (getenv variable)))
                      (split-string value ":" t)))
                  '("LANGUAGE" "LC_ALL" "LC_MESSAGES" "LANG"))))

(defun lispm-i18n-language-of (tag)
  "Return the language of `lispm-languages' TAG asks for, or nil.
TAG is a language tag, as zh-Hant-HK, or a locale, as zh_TW.UTF-8.
Chinese without a script is Simplified, except as written in Hong Kong,
Macao and Taiwan; English is American in the United States and without
a region, else British."
  (save-match-data
    (lispm-i18n--language-of tag)))

(defun lispm-i18n--language-of (tag)
  "Return the language of `lispm-languages' TAG asks for, or nil, the
match data changed; see `lispm-i18n-language-of'."
  (let* ((parts (split-string (car (split-string tag "[.@]")) "[-_]" t))
         (language (downcase (or (car parts) "")))
         (rest (mapcar (lambda (part) (if (= (length part) 4) (capitalize part) (upcase part)))
                       (cdr parts)))
         (script (seq-find (lambda (part) (= (length part) 4)) rest))
         (region (seq-find (lambda (part) (/= (length part) 4)) rest)))
    (pcase language
      ("zh" (cond ((equal script "Hant") 'zh-Hant)
                  ((equal script "Hans") 'zh-Hans)
                  ((member region '("HK" "MO" "TW")) 'zh-Hant)
                  (t 'zh-Hans)))
      ("en" (if (member region '(nil "US")) 'en-US 'en-GB))
      ("ja" 'ja)
      ("fr" 'fr)
      ("de" 'de))))

(defun lispm-system-language ()
  "Return the computer's language, the first of its preferred languages
that LispM speaks, else American English.  It is found once, as every
word shown asks for it while no language is chosen; the caller's match
data is kept."
  (when (eq lispm-i18n--system-languages 'unread)
    (setq lispm-i18n--system-languages
          (save-match-data (lispm-i18n--read-system-languages))))
  (let ((languages lispm-i18n--system-languages))
    (unless (eq (car lispm-i18n--system-language) languages)
      (setq lispm-i18n--system-language
            (cons languages (or (seq-some #'lispm-i18n-language-of languages) 'en-US)))))
  (cdr lispm-i18n--system-language))

(defvar lispm-language-functions nil
  "Functions saying the language of the program LispM Agent runs in, for
it to speak while `lispm-language' is nil.  Each is called with no
arguments and returns a language tag, or nil; the first tag of
`lispm-languages' returned is spoken, else the computer's language.  A
program adding one calls `lispm-language-changed' once its language
changes.")

(defun lispm-current-language ()
  "Return the language LispM Agent speaks now, a tag: `lispm-language',
else the first a function of `lispm-language-functions' returns that it
speaks, else the computer's."
  (or lispm-language
      (run-hook-wrapped 'lispm-language-functions
                        (lambda (function) (car (assq (funcall function) lispm-languages))))
      (lispm-system-language)))

;;;; Catalogs

(defvar lispm-i18n--catalogs nil
  "The catalogs read so far: (TAG . CATALOG), CATALOG the file's plist
with its :messages in a hash table.")

(defun lispm-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (expand-file-name (format "%s.eld" language) lispm-i18n-directory))

(defun lispm-i18n--key (message context)
  "Return the key MESSAGE in CONTEXT has in a catalog's table."
  (if context (concat context "\4" message) message))

(defun lispm-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file: its plist, with its :messages,
\(MESSAGE . TRANSLATION), in a hash table keyed by `lispm-i18n--key'.
A language without a file has an empty one."
  (let ((file (lispm-i18n-catalog-file language))
        (table (make-hash-table :test #'equal)))
    (let ((catalog (when (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer))))))
      (dolist (entry (plist-get catalog :messages))
        (let ((message (car entry)))
          (puthash (if (consp message)
                       (lispm-i18n--key (cdr message) (car message))
                     message)
                   (cdr entry) table)))
      (plist-put (copy-sequence catalog) :messages table))))

(defun lispm-i18n-catalog (&optional language)
  "Return LANGUAGE's catalog, by default the current language's, read
once."
  (let ((language (or language (lispm-current-language))))
    (or (alist-get language lispm-i18n--catalogs)
        (setf (alist-get language lispm-i18n--catalogs)
              (lispm-i18n-read-catalog language)))))

(defun lispm-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (or (plist-get (lispm-i18n-catalog language) :name)
      (cadr (assq language lispm-languages))
      (symbol-name language)))

;;;; Translating

(defvar lispm-i18n--english nil
  "Non-nil while words are said as written, in English; see
`lispm-in-english'.")

(defmacro lispm-in-english (&rest body)
  "Run BODY saying every word as written, in English, and return its value.
What models read is said so: a tool's result, and the error of a
function the tool shares with the user's commands."
  (declare (indent 0) (debug t))
  `(let ((lispm-i18n--english t)) ,@body))

(defun lispm-i18n--translation (message context)
  "Return the translation of MESSAGE in CONTEXT, or nil."
  (unless lispm-i18n--english
    (gethash (lispm-i18n--key message context)
             (plist-get (lispm-i18n-catalog) :messages))))

(defun lispm-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the current
language, formatted with ARGS when there are any."
  (let ((translation (lispm-i18n--translation message context)))
    (let ((text (if (stringp translation) translation message)))
      (if args (apply #'format text args) text))))

(defun lispm-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when there
are any.  MESSAGE is the English written in the code; see lispm-i18n.el.
A MESSAGE marked with a context by `lispm-tr-noop' is translated as
meant in it."
  (apply #'lispm-tr-in
         (and (> (length message) 0) (get-text-property 0 'lispm-i18n-context message))
         message args))

(defun lispm-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language,
formatted with ARGS, or with COUNT when there are none."
  (let* ((translation (lispm-i18n--translation singular nil))
         (rule (alist-get (lispm-current-language) lispm-i18n-plural-forms))
         (text (cond ((stringp translation) translation)
                     (translation (or (nth (if rule (funcall rule count) 0) translation)
                                      (car (last translation))))
                     ((= count 1) singular)
                     (t plural))))
    (apply #'format text (or args (list count)))))

;;;; Changing the language

(defvar lispm-language-changed-hook nil
  "Hook run once LispM Agent speaks another language, to draw again what
shows words.")

(defun lispm-i18n-forget ()
  "Forget the catalogs read and the computer's languages, so they are read
again when next needed."
  (setq lispm-i18n--catalogs nil
        lispm-i18n--system-languages 'unread
        lispm-i18n--system-language '(unread)))

(defvar lispm-i18n--spoken nil
  "The language spoken before a language setting was set, while that
waits to be compared with the language spoken once the command setting
it ends; else nil.")

(defun lispm-i18n--changed (_symbol _value operation where)
  "Run `lispm-language-changed-hook' once a language setting is set, as
the command setting it ends, when the language spoken changes: once for
all the settings it set.  This is a variable watcher of `lispm-language';
OPERATION and WHERE are those of `add-variable-watcher'."
  (when (and (eq operation 'set) (null where) (null lispm-i18n--spoken))
    (setq lispm-i18n--spoken (lispm-current-language))
    (run-at-time 0 nil #'lispm-i18n--speak)))

(defun lispm-i18n--speak ()
  "Run `lispm-language-changed-hook' if the language spoken is another
than `lispm-i18n--spoken'."
  (let ((from lispm-i18n--spoken))
    (setq lispm-i18n--spoken nil)
    (unless (eq from (lispm-current-language))
      (run-hooks 'lispm-language-changed-hook))))

(defun lispm-language-changed ()
  "Draw again what shows words, now that the language a function of
`lispm-language-functions' returns has changed, unless `lispm-language'
chooses one: a program saying its language calls this once it speaks
another."
  (unless (or lispm-language lispm-i18n--spoken)
    (run-hooks 'lispm-language-changed-hook)))

(add-variable-watcher 'lispm-language #'lispm-i18n--changed)

(provide 'lispm-i18n)
;;; lispm-i18n.el ends here
