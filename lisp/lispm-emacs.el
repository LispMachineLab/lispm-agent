;;; lispm-emacs.el --- Emacs itself, as agents find and learn it  -*- lexical-binding: t; -*-

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

;; Emacs is the agent's operating system: whatever Emacs can do, an
;; agent can do with eval_elisp, from reading mail to browsing the web.
;; What it lacks is knowing what there is.  These tools let it find out
;; as a person would: apropos finds the functions, commands, variables
;; and faces with a name about something, describe_symbol (in
;; lispm-tools.el) documents one, and read_info reads the manuals of
;; Emacs and its packages, by node or through their indexes.

;;; Code:

(require 'info)
(require 'lispm-core)
(require 'lispm-tools)

(defcustom lispm-apropos-results 60
  "How many symbols apropos lists at most."
  :type 'integer
  :group 'lispm)

(defun lispm-emacs--kind (symbol)
  "What SYMBOL names, for apropos: command, function, variable or face."
  (cond ((commandp symbol) "command")
        ((fboundp symbol) "function")
        ((boundp symbol) "variable")
        ((facep symbol) "face")))

(defun lispm-emacs--summary (symbol kind)
  "The first line of the documentation of SYMBOL, a KIND."
  (let ((doc (ignore-errors
               (pcase kind
                 ((or "command" "function") (documentation symbol t))
                 ("variable" (documentation-property symbol 'variable-documentation t))
                 ("face" (face-documentation symbol))))))
    (if (stringp doc) (car (split-string doc "\n")) "")))

(lispm-deftool apropos
    ((pattern string "Words that must all be in a name, such as \"webkit url\", or a regexp.")
     (kind string "What to find (default: anything)." :optional t
           :enum ("any" "command" "function" "variable" "face")))
  "Find what this Emacs can do: the functions, commands, variables and
faces whose names match PATTERN, each with the first line of its
documentation.  Then learn one with describe_symbol, or its manual
with read_info, which search_tools finds, and use it with eval_elisp.
For the parts and apps of this system, their skills say more:
search_tools finds them too."
  :permission read
  (let* ((words (split-string pattern "[ \t]+" t))
         (regexp (and (null (cdr words))
                      (string-match-p "[][^$*+?\\\\(){}|.]" pattern)
                      (lispm-tools--emacs-regexp pattern)))
         (kind (if (member kind '(nil "any")) nil kind))
         found)
    (mapatoms
     (lambda (symbol)
       (let ((name (symbol-name symbol))
             (symbol-kind (lispm-emacs--kind symbol)))
         (when (and symbol-kind
                    (or (null kind) (equal kind symbol-kind)
                        (and (equal kind "function") (equal symbol-kind "command")))
                    (if regexp
                        (string-match-p regexp name)
                      (seq-every-p (lambda (word) (string-search (downcase word) name)) words)))
           (push (cons symbol symbol-kind) found)))))
    ;; Shorter names first: they are the more general.
    (setq found (sort found (lambda (a b)
                              (let ((la (length (symbol-name (car a))))
                                    (lb (length (symbol-name (car b)))))
                                (if (= la lb)
                                    (string< (car a) (car b))
                                  (< la lb))))))
    (if (null found)
        (format "Nothing matches %S." pattern)
      (concat (mapconcat (lambda (entry)
                           (format "%s (%s) %s" (car entry) (cdr entry)
                                   (lispm-emacs--summary (car entry) (cdr entry))))
                         (seq-take found lispm-apropos-results) "\n")
              (if (> (length found) lispm-apropos-results)
                  (format "\n[... %d more; narrow the pattern]"
                          (- (length found) lispm-apropos-results))
                "")))))

(lispm-deftool read_info
    ((manual string "Manual, such as emacs, elisp, eww, org, cl; dir lists them all." :optional t)
     (node string "Node to read (default: Top)." :optional t)
     (search string "A term to look up in the manual's index instead of a node." :optional t))
  "Read the Info manuals of Emacs and its packages: how things work,
with examples.  Start at a manual's Top node, follow the names in its
menus, or look a term up in its index."
  :permission read
  :deferred t
  (with-temp-buffer
    (Info-mode)
    (let ((manual (or manual (if search "emacs" "dir"))))
      (Info-find-node manual (if (equal manual "dir") "Top" (or node "Top")) t)
      (when search
        (Info-index search)))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (concat (format "(%s)%s\n" (file-name-base (or Info-current-file "dir")) Info-current-node)
              (lispm--truncate text 30000)))))

(provide 'lispm-emacs)
;;; lispm-emacs.el ends here
