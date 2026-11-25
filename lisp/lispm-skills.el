;;; lispm-skills.el --- Skills: knowing how to do things with Emacs  -*- lexical-binding: t; -*-

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
;; Other programs bring skills too, the manuals of their parts and
;; apps: they say where theirs are through `lispm-skill-sources'.
;;
;; Agents find skills with search_tools, which reads every skill's
;; title, description, headings and the tools its code defines, without
;; evaluating anything (`lispm-skills-index'), and load one with the
;; skill tool when a request calls for it: its text becomes the call's
;; result, so the conversation holds it from then on, and its code is
;; evaluated, so its functions exist and its tools are the agent's:
;; deferred tools, loaded for it.  A resumed session evaluates again
;; the code of the skills it had loaded.
;;
;; A project may come from anywhere, so the code of its skills is
;; evaluated only once the user has seen it and agreed, asked when an
;; agent loads one: once, or for good.  For good pins the code by its
;; hash (`lispm-trusted-skills'): while it stays as it was, it is
;; evaluated without asking; changed, it is asked about again.
;;
;; Agents write skills too.  What an agent defines in the running
;; Emacs lasts until Emacs exits; write_skill proposes a skill holding
;; it, which the user reviews as the org file it would be.  Approved,
;; its code is evaluated and the file saved in the user's skills
;; (`lispm-write-skill-directory'), from where later sessions load it.
;;
;; Skills from elsewhere join the user's with `lispm-install-skill',
;; once the user has seen their code, and `lispm-delete-skill' deletes
;; them again.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-skills-core)

;;;; Writing skills

(defun lispm-skills--check-name (name)
  "Signal an error unless NAME is a name for a skill.
That is lowercase letters, digits, - and _, at most 64 of them."
  (unless (and (stringp name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z0-9][a-z0-9_-]\\{0,63\\}\\'" name)))
    (error "The name of a skill is lowercase letters, digits, - and _, at most 64: not %S" name)))

(defun lispm-skills--target (name agent)
  "The file write_skill saves the skill NAME in, for AGENT.
An error if NAME is no name for a skill (lowercase letters, digits, -
and _), or if the skill of that name AGENT would load is not in
`lispm-write-skill-directory': one of LispM's own, say, or of AGENT's
project, which the skill saved would not replace."
  (lispm-skills--check-name name)
  (let ((directory (lispm-skills-write-directory))
        (existing (lispm-skills-find name agent)))
    (cond ((null existing) (expand-file-name (concat name ".org") directory))
          ((file-in-directory-p (plist-get existing :file) directory) (plist-get existing :file))
          (t (error "There is a skill named %s already, in %s, which a skill saved in %s would \
not replace; choose another name"
                    name (abbreviate-file-name (file-name-directory (plist-get existing :file)))
                    (abbreviate-file-name directory))))))

(defun lispm-skills--compose (name description instructions code title)
  "The org file of the skill NAME: DESCRIPTION, INSTRUCTIONS, CODE and TITLE.
CODE and TITLE may be nil.  See write_skill."
  (let ((line (lambda (text) (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim text)))))
    (when (string-empty-p (funcall line description))
      (error "Say in its description what the skill is for"))
    (concat "#+title: " (funcall line (if (and title (not (string-blank-p title))) title name)) "\n"
            "#+description: " (funcall line description) "\n\n"
            (string-trim instructions) "\n"
            (if (and code (not (string-blank-p code)))
                (concat "\n* The code\n\n#+begin_src emacs-lisp :lispm load\n"
                        (org-escape-code-in-string (string-trim code)) "\n#+end_src\n")
              ""))))

(defun lispm-skills--write-diff (name description instructions &optional code title)
  "The skill file there now, the one write_skill would write, and its label.
NAME, DESCRIPTION, INSTRUCTIONS, CODE and TITLE are its arguments."
  (let ((file (lispm-skills--target name lispm-current-agent)))
    (list (lispm-file-text file)
          (lispm-skills--compose name description instructions code title)
          (lispm-tools-label file))))

(lispm-deftool write_skill
    ((name string "The skill's name: lowercase letters, digits, - and _, such as \"org-tables\".")
     (description string "One line saying what the skill is for: agents load it by this.")
     (instructions string "Its text, in org: how to do the thing, which functions and tools to use, in which steps, with which pitfalls.")
     (code string "Emacs Lisp evaluated whenever the skill is loaded: helper functions, and tools defined with lispm-deftool." :optional t)
     (title string "A title (default: the name)." :optional t))
  "Keep what you worked out for later sessions: write a skill.
Tools and functions you define with eval_elisp last until Emacs exits;
know-how, until the conversation ends.  A skill holds both: any agent
can load it with the skill tool, in this session and every later one.
The user sees it as the org file it would be, and approves it or not;
then its code is evaluated, and only if that succeeds is it saved, in
the user's skills, its tools yours from your next step on.  Writing a
skill of the same name you wrote before replaces it.  Test the code
with eval_elisp first."
  :permission exec
  :deferred t
  :always-ask t
  :diff #'lispm-skills--write-diff
  (let* ((agent lispm-current-agent)
         (file (lispm-skills--target name agent))
         (text (lispm-skills--compose name description instructions code title))
         (forms (condition-case err
                    (lispm-skills-forms text)
                  (error (error "Skill %s was not saved: %s" name (error-message-string err)))))
         (open (find-buffer-visiting file)))
    (when (and open (buffer-modified-p open))
      (error "%s is open in Emacs with unsaved changes; ask the user to save or discard them"
             (abbreviate-file-name file)))
    (lispm-tools-check-writable file)
    (lispm-tools-check-new-file file)
    ;; The code runs there, as it will when the skill is loaded.
    (make-directory (file-name-directory file) t)
    (let ((tools (condition-case err
                     (lispm-skills-evaluate (list :name name :file file) agent forms)
                   (error (error "The code of skill %s failed, so the skill was not saved: %s
What the code defined before the error stays in Emacs until it exits; fix the code and \
write the skill again"
                                 name (error-message-string err))))))
      (let ((coding-system-for-write 'utf-8))
        (write-region text nil file nil 'silent))
      (when open
        (lispm-tools-sync open))
      (run-hooks 'lispm-skills-change-hook)
      (concat (format "Skill %s loaded%s.\n" name
                      (if tools (format "; tools now available: %s" (string-join tools ", ")) ""))
              (format "Saved as %s: agents load it from now on, in later sessions too."
                      (abbreviate-file-name file))))))

;;;; Installing skills

(defun lispm-skills--source (source)
  "The skill file of SOURCE: NAME.org, or SKILL.org/SKILL.md in NAME/.
A file SKILL.org or SKILL.md stands for its directory.  An error if
SOURCE is neither."
  (let ((source (expand-file-name source)))
    (cond ((file-directory-p source)
           (let ((file (seq-find #'file-regular-p
                                 (list (expand-file-name "SKILL.org" source)
                                       (expand-file-name "SKILL.md" source)))))
             (unless file
               (user-error "%s" (lispm-tr "%s holds no SKILL.org: it is no skill" (abbreviate-file-name source))))
             file))
          ((not (file-regular-p source))
           (user-error "%s" (lispm-tr "No skill file %s" (abbreviate-file-name source))))
          ((or (string-suffix-p ".org" source)
               (member (file-name-nondirectory source) '("SKILL.org" "SKILL.md")))
           source)
          (t (user-error "%s" (lispm-tr "A skill is NAME.org, or a directory holding SKILL.org: not %s"
                                        (abbreviate-file-name source)))))))

(defun lispm-skills--label (name file)
  "FILE, a file of the skill NAME, as shown in diffs."
  (if (member (file-name-nondirectory file) '("SKILL.org" "SKILL.md"))
      (concat name "/" (file-name-nondirectory file))
    (concat name ".org")))

(defun lispm-skills--install-diff (name old new text)
  "The diff from the skill NAME installed in OLD to the one in NEW.
OLD and NEW are skill files, and TEXT is NEW's text; the files beside
them are compared too."
  (let* ((beside (lambda (file)
                   (mapcar (lambda (other) (cons (file-relative-name other (file-name-directory file)) other))
                           (lispm-skills-beside file))))
         (old-beside (funcall beside old))
         (new-beside (funcall beside new))
         (text-of (lambda (files relative)
                    (if-let* ((file (cdr (assoc relative files))))
                        (lispm-skills-file-text file)
                      "")))
         (diffs (delete "(no changes)"
                        (cons (lispm-diff (lispm-skills-file-text old) text (lispm-skills--label name new))
                              (mapcar (lambda (relative)
                                        (lispm-diff (funcall text-of old-beside relative)
                                                     (funcall text-of new-beside relative)
                                                     (concat name "/" relative)))
                                      (sort (seq-uniq (mapcar #'car (append old-beside new-beside)))
                                            #'string<))))))
    (if diffs (string-join diffs) "(no changes)")))

(defun lispm-skills--show-diff (diff)
  "Show DIFF, what installing a skill would change, in a buffer."
  (with-current-buffer (get-buffer-create "*LispM skill diff*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff)
      (diff-mode)
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer (current-buffer))))

;;;###autoload
(defun lispm-install-skill (source &optional noconfirm)
  "Install the skill SOURCE among the user's skills, and return its name.
SOURCE is a skill file, NAME.org, or a skill directory, NAME/ holding
SKILL.org and files of its own.  It is copied into
`lispm-write-skill-directory', where write_skill saves skills, and so
its code is evaluated, unasked, by every agent that loads it:
installing a skill is trusting it.  Unless NOCONFIRM, its code is
shown, or the diff against the skill it replaces, and you are asked
first; what is installed is what was shown.  An error if SOURCE is no
skill, if its code does not read, or if NAME is no name for a skill
or one that another skill has, such as one of LispM's own, which the
skill installed would not replace."
  (interactive (list (read-file-name (lispm-tr "Install the skill (NAME.org, or a directory with SKILL.org): ")
                                     nil nil t)))
  (let* ((file (lispm-skills--source source))
         (entry-name (file-name-nondirectory file))
         (directory-p (member entry-name '("SKILL.org" "SKILL.md")))
         (name (plist-get (lispm-skills-read file) :name))
         (text (lispm-skills-file-text file))
         (hash (lispm-skills-hash file text))
         (installed (let ((existing (lispm-skills--target name nil)))
                      (and (file-exists-p existing) existing)))
         (directory (lispm-skills-write-directory))
         (target (expand-file-name (concat name (if directory-p "/" ".org")) directory))
         (target-file (if directory-p (expand-file-name entry-name target) target))
         (other (expand-file-name (concat name (if directory-p ".org" "/")) directory)))
    (unless (lispm-skills-md-p file)
      (condition-case err
          (lispm-skills-forms text)
        (error (user-error "%s" (lispm-tr "Skill %s was not installed: %s" name (error-message-string err))))))
    (cond
     ((lispm-skills-same-file-p file target-file))
     ((and directory-p (or (file-in-directory-p target (file-name-directory file))
                           (file-in-directory-p (file-name-directory file) target)))
      (user-error "%s" (lispm-tr "Skill %s is not installed in %s from inside it, nor from a directory holding it"
                                 name (abbreviate-file-name target))))
     (t
      (dolist (location (list target-file
                              (if directory-p
                                  other
                                (or (seq-find #'file-exists-p
                                              (list (expand-file-name "SKILL.org" other)
                                                    (expand-file-name "SKILL.md" other)))
                                    (expand-file-name "SKILL.org" other)))))
        (when-let* ((open (find-buffer-visiting location)))
          (when (buffer-modified-p open)
            (user-error "%s" (lispm-tr "%s is open in Emacs with unsaved changes; save or discard them first"
                                       (abbreviate-file-name location))))))
      (unless noconfirm
        (save-window-excursion
          (if installed
              (lispm-skills--show-diff (lispm-skills--install-diff name installed file text))
            (lispm-skills-show-code file))
          (unless (yes-or-no-p (if installed
                                   (lispm-tr "Replace the skill %s, as shown? " name)
                                 (lispm-tr "Install the skill %s, as shown? " name)))
            (user-error "%s" (lispm-tr "Skill %s was not installed" name))))
        (unless (equal (lispm-skills-hash file) hash)
          (user-error "%s" (lispm-tr "The code of skill %s changed while you were asked; install it again to see it \
as it is now" name))))
      (make-directory directory t)
      (let ((coding-system-for-write 'utf-8))
        (if (not directory-p)
            (write-region text nil target nil 'silent)
          ;; Copied whole beside it first, then put in place.
          (let ((copy (file-name-as-directory
                       (make-temp-file (expand-file-name (concat "." name "-") directory) t))))
            (condition-case err
                (progn
                  (copy-directory (file-name-directory file) copy nil nil t)
                  (write-region text nil (expand-file-name entry-name copy) nil 'silent)
                  (when (file-directory-p target)
                    (delete-directory target t))
                  (rename-file (directory-file-name copy) (directory-file-name target)))
              (error (when (file-directory-p copy)
                       (delete-directory copy t))
                     (signal (car err) (cdr err)))))))
      ;; The skill as it was, if it was the other kind.
      (cond ((and directory-p (file-regular-p other))
             (delete-file other))
            ((and (not directory-p)
                  (or (file-exists-p (expand-file-name "SKILL.org" other))
                      (file-exists-p (expand-file-name "SKILL.md" other))))
             (delete-directory other t)))
      (when-let* ((open (find-buffer-visiting target-file)))
        (lispm-tools-sync open))
      (run-hooks 'lispm-skills-change-hook)
      (when (called-interactively-p 'any)
        (message "%s" (lispm-tr "LispM: skill %s installed in %s" name (abbreviate-file-name directory))))))
    name))

;;;###autoload
(defun lispm-create-skill (name description &optional instructions title no-open)
  "Create a new skill NAME in `lispm-write-skill-directory', and return its file.
NAME is lowercase letters, digits, - and _; DESCRIPTION is one line
saying what the skill is for; INSTRUCTIONS and TITLE are optional.
Unless NO-OPEN is non-nil, open the skill file in a buffer for editing."
  (interactive
   (list (read-string "Skill name (lowercase letters, digits, - and _): ")
         (read-string "Description: ")))
  (let* ((name (string-trim (or name "")))
         (file (lispm-skills--target name nil))
         (instructions (if (and instructions (not (string-blank-p instructions)))
                           instructions
                         "* Instructions\n\nDescribe how to do the task and which tools or functions to use."))
         (text (condition-case err
                   (lispm-skills--compose name description instructions nil title)
                 (error (user-error "%s" (error-message-string err))))))
    (when (file-exists-p file)
      (user-error "Skill %s already exists in %s"
                  name (abbreviate-file-name file)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil file nil 'silent))
    (run-hooks 'lispm-skills-change-hook)
    (unless no-open
      (pop-to-buffer (find-file-noselect file)))
    file))

;;;###autoload
(defun lispm-delete-skill (name)
  "Delete the skill NAME from the user's skills.
Those are the skills in `lispm-write-skill-directory'; one of LispM's
own, of a project or of another of `lispm-skill-directories' is not
deleted.  Agents that loaded it keep what its code defined until Emacs
exits.  Interactively, you are asked first."
  (interactive
   (list (completing-read (lispm-tr "Delete the skill: ")
                          (or (mapcar (lambda (skill) (plist-get skill :name))
                                      (lispm-skills-in (lispm-skills-write-directory)))
                              (user-error "%s" (lispm-tr "No skills in %s"
                                                         (abbreviate-file-name (lispm-skills-write-directory)))))
                          nil t)))
  (lispm-skills--check-name name)
  (let* ((directory (lispm-skills-write-directory))
         (file (expand-file-name (concat name ".org") directory))
         (skill-directory (expand-file-name (concat name "/") directory))
         (dir-p (or (file-regular-p (expand-file-name "SKILL.org" skill-directory))
                    (file-regular-p (expand-file-name "SKILL.md" skill-directory)))))
    (when (or (file-in-directory-p directory lispm-skills-builtin)
              (not (or (file-regular-p file) dir-p)))
      (let ((other (lispm-skills-find name (or (bound-and-true-p lispm-session-agent)
                                                (bound-and-true-p lispm-chat-agent)))))
        (cond ((null other)
               (user-error "%s" (lispm-tr "No skill named %s in %s" name (abbreviate-file-name directory))))
              ((file-in-directory-p (plist-get other :file) lispm-skills-builtin)
               (user-error "%s" (lispm-tr "Skill %s is one of LispM's own; it is not deleted" name)))
              ((plist-get other :project)
               (user-error "%s" (lispm-tr "Skill %s is the project's, in %s; it is not deleted" name
                                          (abbreviate-file-name (file-name-directory (plist-get other :file))))))
              (t
               (user-error "%s" (lispm-tr "Skill %s is in %s, not among the skills installed in %s; it is not deleted"
                                          name (abbreviate-file-name (file-name-directory (plist-get other :file)))
                                          (abbreviate-file-name directory)))))))
    (when (and (called-interactively-p 'any)
               (not (yes-or-no-p (lispm-tr "Delete the skill %s? " name))))
      (user-error "%s" (lispm-tr "Skill %s was not deleted" name)))
    (when (file-regular-p file)
      (delete-file file t))
    (when dir-p
      (delete-directory skill-directory t t))
    (run-hooks 'lispm-skills-change-hook)
    name))

;;;; Resumed sessions

(defun lispm-skills--loaded ()
  "Names of the skills loaded by `skill' or `write_skill' calls."
  (save-excursion
    (save-restriction
      (widen)
      (let (names)
        (dolist (section (lispm-transcript-sections))
          (when (and (eq (plist-get section :kind) 'assistant)
                     (not (plist-get section :ignore)))
            (dolist (part (lispm-transcript-parts section))
              (when (and (eq (plist-get part :kind) 'tool)
                         (member (plist-get part :name) '("skill" "write_skill"))
                         (equal (plist-get part :state) "DONE")
                         (not (plist-get part :error)))
                (when-let* ((input (plist-get part :input))
                            (name (alist-get 'name input))
                            ((stringp name))
                            ((or (not (plist-member part :result))
                                 (plist-get part :cleared)
                                 (string-prefix-p (format "Skill %s loaded" name)
                                                  (or (plist-get part :result) "")))))
                  (push name names))))))
        (delete-dups (nreverse names))))))

(defun lispm-skills--restore ()
  "Evaluate again the code of the skills the session in this buffer loaded.
That of a project's skill only while the user trusts it as it is now."
  (when-let* ((agent (and (bound-and-true-p lispm-session-mode) lispm-session-agent)))
    (dolist (name (lispm-skills--loaded))
      (when-let* ((skill (lispm-skills-find name agent)))
        (condition-case err
            (let* ((file (plist-get skill :file))
                   (text (lispm-skills-file-text file)))
              (if (lispm-skill-trusted-p skill text)
                  (lispm-skills-evaluate skill agent (lispm-skills-forms text file))
                (message "%s" (lispm-tr "LispM: skill %s: a project's skill whose code you have not trusted as \
it is now, so it was not evaluated again (see `lispm-trust-skill')" name))))
          (error (message "%s" (lispm-tr "LispM: skill %s: %s" name (error-message-string err)))))))))

(add-hook 'lispm-session-mode-hook #'lispm-skills--restore)

(provide 'lispm-skills)
;;; lispm-skills.el ends here
