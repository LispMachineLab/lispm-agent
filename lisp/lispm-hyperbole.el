;;; lispm-hyperbole.el --- Hyperbole's Action Key on LispM's objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-agent
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; GNU Hyperbole acts on what text names wherever the text is: its
;; Action Key, M-RET, visits a path, shows a commit, opens an Info
;; node, each recognized in context by an implicit button type.  LispM
;; presents the objects in the text it draws instead, and knows them
;; (lispm-inspect.el).  Once Hyperbole is loaded, lispm-inspect.el
;; loads this file, and each takes the other's:
;;
;;   M-RET on an object LispM presents    acts on it as RET does, as the
;;                                        implicit button type
;;                                        `lispm-presentation', tried
;;                                        before Hyperbole's own;
;;   RET on text presenting nothing, in   activates the implicit button
;;   the chat or the inspector            Hyperbole recognizes there,
;;                                        while Hyperbole mode is on.
;;
;; Hyperbole mode's M-RET comes before the chat's, which starts a new
;; line in the input; there it still does.  LispM does not need
;; Hyperbole, and this file compiles without it.

;;; Code:

(require 'lispm-inspect)

(defvar hkey-alist)
(defvar hyperbole-mode)
(declare-function ibut:at-p "hbut" (&optional name-key-only))
(declare-function hbut:act "hbut" (&optional hbut))
(declare-function lispm-chat--in-input-p "lispm-chat" ())
(declare-function lispm-chat-newline "lispm-chat" ())

(defun lispm-hyperbole--presented ()
  "The object presented at point and where its text is, or nil.
That is (OBJECT START END)."
  (when-let* ((object (lispm-object-at)))
    (list object
          (previous-single-property-change (1+ (point)) 'lispm-object nil (point-min))
          (next-single-property-change (point) 'lispm-object nil (point-max)))))

;; `defib' is Hyperbole's macro, expanded once Hyperbole is there: this
;; file compiles without it.  Defined last, the type is tried first.
(eval '(defib lispm-presentation ()
         "An object LispM presents, acted on as RET acts on it.
See lispm-inspect.el."
         (pcase (lispm-hyperbole--presented)
           (`(,object ,start ,end)
            (ibut:label-set (buffer-substring-no-properties start end) start end)
            (hact #'lispm-act object))))
      t)

(defun lispm-hyperbole-act ()
  "Activate the implicit button Hyperbole recognizes at point, if any.
Only while Hyperbole mode is on.  Return non-nil if there was one."
  (when (and hyperbole-mode (ibut:at-p))
    (hbut:act 'hbut:current)
    t))

(add-hook 'lispm-act-functions #'lispm-hyperbole-act)

;; In the chat's input, M-RET starts a new line, Hyperbole mode or not.
;; The Action Mouse Key keeps its own list.
(add-to-list 'hkey-alist
             '((and (derived-mode-p 'lispm-chat-mode) (lispm-chat--in-input-p))
               . ((lispm-chat-newline) . (lispm-chat-newline))))

(provide 'lispm-hyperbole)
;;; lispm-hyperbole.el ends here
