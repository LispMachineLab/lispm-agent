;;; lispm-skills.el --- Skills: knowing how to do things with Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A skill is what it takes to do a kind of thing with Emacs, written
;; down: which functions, in which steps, with which pitfalls.  It is
;; an org file:
;;
;;   #+title: Browse the web
;;   #+description: Search the web and read pages, with Emacs's WebKit.
;;
;;   Instructions, for the model to read ...
;;
;;   #+begin_src emacs-lisp :lispm load
;;   (defun ...)          ; helpers, and tools (lispm-deftool) if useful
;;   #+end_src
;;
;; in a directory of `lispm-skill-directories': skills/ of LispM itself,
;; ~/.lispm/skills/ for the user's own, and .lispm/skills/ in a project.
;; A skill is NAME.org, or NAME/SKILL.org with files of its own beside.
;;
;; Agents see each skill's name and description in the description of
;; the skill tool, and load one when a request calls for it: its text
;; becomes the call's result, so the conversation holds it from then
;; on, and its code is evaluated, so its functions exist and its tools
;; are the agent's.  The code of a project's skills is not evaluated:
;; a project may come from anywhere.  A resumed session evaluates again
;; the code of the skills it had loaded.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-transcript)

(defconst lispm-skills--builtin
  (let ((here (file-name-directory (or load-file-name buffer-file-name))))
    ;; skills/ is beside lisp/ in a checkout, and beside the Lisp files
    ;; in a package built from it (as MELPA builds them).
    (seq-find #'file-directory-p
              (list (expand-file-name "../skills/" here) (expand-file-name "skills/" here))
              (expand-file-name "../skills/" here)))
  "The skills of LispM itself.")

(defcustom lispm-skill-directories (list lispm-skills--builtin "~/.lispm/skills/")
  "Directories of skills, a later one's skill replacing an earlier one's.
After these come the .lispm/skills/ of the agent's project, whose
skills' code is never evaluated."
  :type '(repeat directory)
  :group 'lispm)

(defun lispm-skills--read (file)
  "The skill in FILE: a plist (:name :title :description :file)."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (let ((keywords (let ((case-fold-search t) keywords)
                      (while (re-search-forward "^#\\+\\(title\\|description\\):[ \t]*\\(.*?\\)[ \t]*$" nil t)
                        (push (cons (downcase (match-string 1)) (match-string 2)) keywords))
                      keywords))
          (name (if (equal (file-name-nondirectory file) "SKILL.org")
                    (file-name-nondirectory (directory-file-name (file-name-directory file)))
                  (file-name-base file))))
      (list :name name
            :title (or (cdr (assoc "title" keywords)) name)
            :description (or (cdr (assoc "description" keywords)) "")
            :file file))))

(defun lispm-skills--in (directory)
  "The skills in DIRECTORY."
  (when (file-directory-p directory)
    (mapcar #'lispm-skills--read
            (append (directory-files directory t "\\`[^.].*\\.org\\'")
                    (seq-filter #'file-exists-p
                                (mapcar (lambda (dir) (expand-file-name "SKILL.org" dir))
                                        (seq-filter #'file-directory-p
                                                    (directory-files directory t "\\`[^.]"))))))))

(defun lispm-skills (&optional agent)
  "The skills AGENT can load, by name: a list of plists.
Each has :name, :title, :description, :file and :trusted, nil for a
project's skill, whose code is not evaluated."
  (let ((skills nil))
    (dolist (entry (append (mapcar (lambda (dir) (cons dir t)) lispm-skill-directories)
                           (and agent (list (cons (expand-file-name ".lispm/skills/"
                                                                    (lispm-agent-root agent))
                                                  nil)))))
      (dolist (skill (lispm-skills--in (expand-file-name (car entry))))
        (setf (alist-get (plist-get skill :name) skills nil nil #'equal)
              (plist-put skill :trusted (cdr entry)))))
    (sort (mapcar #'cdr skills)
          (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defun lispm-skills--code (file)
  "The Lisp forms of the blocks of FILE marked :lispm load."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((case-fold-search t) forms)
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+emacs-lisp\\([^\n]*\\)\n" nil t)
        (let ((args (match-string 1))
              (start (point)))
          (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (when (string-match-p ":lispm[ \t]+load" args)
              (setq forms (append forms (lispm-tools--read-forms
                                         (org-unescape-code-in-string
                                          (buffer-substring-no-properties start (match-beginning 0))))))))))
      forms)))

(defun lispm-skills--evaluate (skill agent)
  "Evaluate the code of SKILL for AGENT; return the names of the tools it defines.
Those become deferred tools, loaded for AGENT."
  (let ((before (hash-table-keys lispm-tools--registry))
        (default-directory (file-name-directory (plist-get skill :file))))
    (dolist (form (lispm-skills--code (plist-get skill :file)))
      (eval form t))
    (let ((tools (seq-remove (lambda (name) (member name before))
                             (hash-table-keys lispm-tools--registry))))
      (dolist (name tools)
        (setf (lispm-tool-deferred (lispm-get-tool name)) t))
      (setq tools (sort (append tools
                                (seq-filter (lambda (name)
                                              (equal (lispm-tool-skill-of name) (plist-get skill :name)))
                                            before))
                        #'string<))
      (dolist (name tools)
        (put (intern name) 'lispm-skill (plist-get skill :name)))
      (when agent (lispm-tools-load agent tools))
      tools)))

(defun lispm-tool-skill-of (name)
  "The skill that defined the tool NAME, or nil."
  (get (intern name) 'lispm-skill))

(defun lispm-skills--text (file)
  "The text of the skill in FILE, without its #+ keywords."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (looking-at "#\\+\\(title\\|description\\):.*\n") (replace-match ""))
    (string-trim (buffer-string))))

(lispm-register-tool
 (lispm-tool--create
  :name "skill"
  :description
  (lambda ()
    (concat "Load a skill: how to do a kind of thing with Emacs, which is your operating system. "
            "Load one when a request calls for it; its instructions are then yours, and its tools "
            "too, from your next step on.\n"
            "Skills:\n"
            (mapconcat (lambda (skill)
                         (format "- %s: %s" (plist-get skill :name) (plist-get skill :description)))
                       (lispm-skills lispm-current-agent) "\n")))
  :input-schema
  '((type . "object")
    (properties (name (type . "string") (description . "The skill to load.")))
    (required . ["name"]))
  :permission 'read
  :function
  (lambda (input)
    (let* ((agent lispm-current-agent)
           (name (alist-get 'name input))
           (skill (seq-find (lambda (s) (equal (plist-get s :name) name)) (lispm-skills agent))))
      (unless skill
        (error "No skill named %s; there are: %s" name
               (mapconcat (lambda (s) (plist-get s :name)) (lispm-skills agent) ", ")))
      (let ((tools (and (plist-get skill :trusted) (lispm-skills--evaluate skill agent))))
        (concat (format "Skill %s loaded%s.\n\n" name
                        (cond (tools (format "; tools now available: %s" (string-join tools ", ")))
                              ((plist-get skill :trusted) "")
                              (t " (a project's skill: its code was not evaluated)")))
                (lispm--truncate (lispm-skills--text (plist-get skill :file)) 30000)))))))

(defvar lispm-session-mode)
(defvar lispm-session-agent)

(defun lispm-skills--loaded ()
  "Names of the skills the session in the current buffer loaded."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil) names)
        (while (re-search-forward "^Skill \\([^ \n]+\\) loaded[;.]" nil t)
          (push (match-string-no-properties 1) names))
        (delete-dups (nreverse names))))))

(defun lispm-skills--restore ()
  "Evaluate again the code of the skills the session in this buffer loaded."
  (when-let* ((agent (and (bound-and-true-p lispm-session-mode) lispm-session-agent)))
    (dolist (name (lispm-skills--loaded))
      (when-let* ((skill (seq-find (lambda (s) (equal (plist-get s :name) name)) (lispm-skills agent))))
        (when (plist-get skill :trusted)
          (condition-case err
              (lispm-skills--evaluate skill agent)
            (error (message "LispM: skill %s: %s" name (error-message-string err)))))))))

(add-hook 'lispm-session-mode-hook #'lispm-skills--restore)

(provide 'lispm-skills)
;;; lispm-skills.el ends here
