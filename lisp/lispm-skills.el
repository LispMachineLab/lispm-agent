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
;; Agents see each skill's name and description in the description of
;; the skill tool, and load one when a request calls for it: its text
;; becomes the call's result, so the conversation holds it from then
;; on, and its code is evaluated, so its functions exist and its tools
;; are the agent's: deferred tools, loaded for it.  A resumed session
;; evaluates again the code of the skills it had loaded.
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
skills' code is evaluated only once the user trusts it (see
`lispm-trusted-skills')."
  :type '(repeat directory)
  :group 'lispm)

(defcustom lispm-write-skill-directory nil
  "Directory where write_skill saves the skills agents write.
nil means the last of `lispm-skill-directories', ~/.lispm/skills/ by
default.  Its skills are found even if it is not one of those."
  :type '(choice (const :tag "The last of `lispm-skill-directories'" nil) directory)
  :group 'lispm)

(defcustom lispm-trusted-skills nil
  "The skills of projects whose code the user trusts, as it was then.
An alist of (FILE . HASH): HASH is the sha256 of the skill's blocks
marked :lispm load and, for a skill that is a directory, of the other
files in it.  While the hash matches, an agent loading the skill has
its code evaluated without asking; once the code changes, the user is
asked again.  Answering that an agent may evaluate it for good adds an
entry, as `lispm-trust-skill' does; both save it with Customize."
  :type '(alist :key-type file :value-type string)
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

(defun lispm-skills--write-directory ()
  "The directory write_skill saves skills in (see `lispm-write-skill-directory')."
  (file-name-as-directory
   (expand-file-name (or lispm-write-skill-directory (car (last lispm-skill-directories))
                         "~/.lispm/skills/"))))

(defun lispm-skills--directories (agent)
  "The directories of the skills AGENT can load, in order: (DIRECTORY . PROJECT).
Those of `lispm-skill-directories', the one write_skill saves in, then
AGENT's project's .lispm/skills/, PROJECT non-nil for that one alone.
It is left out if it is one of the others, as it is for an agent
working in the home directory: what the user keeps there is theirs."
  (let* ((directories (mapcar (lambda (directory) (file-name-as-directory (expand-file-name directory)))
                              lispm-skill-directories))
         (directories (if (member (lispm-skills--write-directory) directories)
                          directories
                        (append directories (list (lispm-skills--write-directory)))))
         (project (and agent (expand-file-name ".lispm/skills/" (lispm-agent-root agent)))))
    (append (mapcar (lambda (directory) (cons directory nil)) directories)
            (and project (not (member project directories)) (list (cons project t))))))

(defun lispm-skills (&optional agent)
  "The skills AGENT can load, by name: a list of plists.
Each has :name, :title, :description, :file and :project, non-nil for
a skill of AGENT's project, whose code is evaluated only once the user
trusts it (see `lispm-skill-trusted-p')."
  (let ((skills nil))
    (pcase-dolist (`(,directory . ,project) (lispm-skills--directories agent))
      (dolist (skill (lispm-skills--in directory))
        (setf (alist-get (plist-get skill :name) skills nil nil #'equal)
              (plist-put skill :project project))))
    (sort (mapcar #'cdr skills)
          (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defun lispm-skills--find (name agent)
  "The skill called NAME that AGENT can load, or nil."
  (seq-find (lambda (skill) (equal (plist-get skill :name) name)) (lispm-skills agent)))

;;;; Code

(defun lispm-skills--file-text (file)
  "The text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun lispm-skills--blocks (text)
  "The code of the source blocks of the skill TEXT marked :lispm load, as strings."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t) blocks)
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+emacs-lisp\\([^\n]*\\)\n" nil t)
        (let ((args (match-string 1))
              (start (point)))
          (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (when (string-match-p ":lispm[ \t]+load" args)
              (push (org-unescape-code-in-string
                     (buffer-substring-no-properties start (match-beginning 0)))
                    blocks)))))
      (nreverse blocks))))

(defun lispm-skills--read-code (code)
  "The Lisp forms in the string CODE; an error if it does not read whole.
A form left open is an error, not the end of the code: a skill's
code is evaluated as written or not at all."
  (with-temp-buffer
    (with-syntax-table emacs-lisp-mode-syntax-table
      (insert code)
      (goto-char (point-min))
      (let (forms)
        (while (progn (forward-comment (buffer-size)) (not (eobp)))
          (let ((line (line-number-at-pos)))
            (push (condition-case nil
                      (read (current-buffer))
                    (end-of-file
                     (error "The code does not read: the form at its line %d is not closed" line)))
                  forms)))
        (nreverse forms)))))

(defun lispm-skills--forms (text)
  "The Lisp forms of the blocks of the skill TEXT marked :lispm load."
  (mapcan #'lispm-skills--read-code (lispm-skills--blocks text)))

(defun lispm-skills--evaluate (skill agent forms)
  "Evaluate FORMS, the code of SKILL, for AGENT; return the tools it defines.
Their names, that is: they become deferred tools, loaded for AGENT,
also those defined before an error in the code, which goes on (see
`lispm-tools--adopt').  The code is that of the text the caller read
and checked: SKILL's file is not read again."
  (let ((name (plist-get skill :name))
        (default-directory (file-name-directory (plist-get skill :file)))
        (before (lispm-tools--snapshot))
        tools)
    (unwind-protect
        (dolist (form forms)
          (eval form t))
      (let ((defined (lispm-tools--adopt agent before))
            ;; Those it defined when loaded before, if not again now.
            (earlier (seq-filter (lambda (tool)
                                   (and (equal (lispm-tool-skill-of tool) name)
                                        (lispm-get-tool tool)
                                        (lispm-tool-deferred (lispm-get-tool tool))))
                                 (hash-table-keys before))))
        (setq tools (sort (seq-uniq (append defined earlier)) #'string<))
        (dolist (tool tools)
          (put (intern tool) 'lispm-skill name))
        (when agent
          (lispm-tools-load agent earlier))))
    tools))

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

(defun lispm-skills--loaded-text (skill tools &optional unevaluated extra)
  "The result of loading SKILL: that it was, and its text.
TOOLS are those its code defined.  UNEVALUATED, if non-nil, says why
its code was not evaluated; EXTRA, if non-nil, follows."
  (concat (format "Skill %s loaded%s.%s\n\n" (plist-get skill :name)
                  (cond (unevaluated (format " (%s)" unevaluated))
                        (tools (format "; tools now available: %s" (string-join tools ", ")))
                        (t ""))
                  (or extra ""))
          (lispm--truncate (lispm-skills--text (plist-get skill :file)) 30000)))

;;;; Trusting a project's skills

(defun lispm-skills--beside (file)
  "The files beside FILE, if it is a skill that is a directory, sorted."
  (when (equal (file-name-nondirectory file) "SKILL.org")
    (let ((file (expand-file-name file)))
      (sort (seq-remove (lambda (other) (equal other file))
                        (directory-files-recursively (file-name-directory file) ""))
            #'string<))))

(defun lispm-skills--hash (file &optional text)
  "The hash that pins the code of the skill in FILE, whose text is TEXT.
TEXT defaults to FILE's.  The sha256 of its blocks marked :lispm load
and, for a skill that is a directory (NAME/SKILL.org), of the names
and contents of the other files in it, which its code may load.  Each
part is preceded by its length, so that no two skills hash alike."
  (let ((directory (file-name-directory (expand-file-name file))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((part (lambda (bytes) (insert (format "%d:" (length bytes)) bytes))))
        (dolist (block (lispm-skills--blocks (or text (lispm-skills--file-text file))))
          (funcall part (encode-coding-string block 'utf-8)))
        (dolist (other (lispm-skills--beside file))
          (funcall part (encode-coding-string (file-relative-name other directory) 'utf-8))
          (funcall part (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally other)
                          (buffer-string)))))
      (secure-hash 'sha256 (current-buffer)))))

(defun lispm-skills--same-file-p (a b)
  "Non-nil if the file names A and B name the same file."
  (equal (expand-file-name a) (expand-file-name b)))

(defun lispm-skill-trusted-p (skill &optional text)
  "Non-nil if the code of SKILL may be evaluated without asking.
It may unless SKILL is a project's: then only while the user trusts
its code as it is now (see `lispm-trusted-skills').  A project's skill
without code has nothing to trust.  TEXT is that of SKILL's file, read
again unless given."
  (or (not (plist-get skill :project))
      (let* ((file (plist-get skill :file))
             (text (or text (lispm-skills--file-text file))))
        (or (null (lispm-skills--blocks text))
            (equal (cdr (seq-find (lambda (entry) (lispm-skills--same-file-p (car entry) file))
                                  lispm-trusted-skills))
                   (lispm-skills--hash file text))))))

(defun lispm-skills--trust (file hash)
  "Trust the code of the skill in FILE while its hash is HASH, or no longer.
HASH nil withdraws the trust.  The change is saved with Customize."
  (customize-save-variable
   'lispm-trusted-skills
   (append (and hash (list (cons (abbreviate-file-name (expand-file-name file)) hash)))
           (seq-remove (lambda (entry) (lispm-skills--same-file-p (car entry) file))
                       lispm-trusted-skills))))

(defun lispm-skills--trust-preview (input)
  "The code of the skill INPUT names, which the user is asked to let run.
As a diff that adds it all, as a new file's would."
  (let ((file (expand-file-name (alist-get 'file input))))
    (lispm--diff "" (string-join (lispm-skills--blocks (lispm-skills--file-text file)) "\n")
                 (lispm-tools--label file))))

(defconst lispm-skills--trust-tool
  (lispm-tool--create
   :name "trust_skill"
   :description "Evaluate the code of a project's skill."
   :input-schema '((type . "object")
                   (properties (name (type . "string")) (file (type . "string"))
                               (hash (type . "string"))
                               (files (type . "array") (items (type . "string")))))
   :permission 'exec
   :preview #'lispm-skills--trust-preview)
  "What loading a project's skill asks the user: to evaluate its code.
No agent calls it: the skill tool asks for it (see `lispm-call-ask'),
and the transcript records it so, with its input: the skill's name,
its file, the hash of its code and the files beside it.")

(defun lispm-skills--reason (denial)
  "The reason the user gave in DENIAL, as a sentence to pass on, or nil."
  (and denial (string-match "Their reason: \\(\\(?:.\\|\n\\)+\\)" denial)
       (concat " The user's reason: " (match-string 1 denial))))

(defun lispm-skills--load-asking (agent id skill done)
  "Load the project's SKILL for AGENT, asking the user to let its code run.
ID is AGENT's call of the skill tool, which waits meanwhile, and DONE
gets its result.  The user may let the code run once, or trust it for
good (see `lispm-trusted-skills'); what is evaluated is the code they
saw.  Refused, or with no call to ask in, SKILL is loaded without its
code."
  (let* ((file (plist-get skill :file))
         (hash (lispm-skills--hash file))
         (text nil)
         (beside (mapcar (lambda (other) (file-relative-name other (file-name-directory file)))
                         (lispm-skills--beside file)))
         (input `((name . ,(plist-get skill :name))
                  (file . ,(abbreviate-file-name file))
                  (hash . ,hash)
                  ,@(and beside `((files . ,(vconcat beside))))))
         (unevaluated (lambda (why &optional extra)
                        (funcall done (lispm-skills--loaded-text
                                       skill nil (concat "a project's skill: " why) extra)))))
    (unless (and agent id
                 (lispm-call-ask
                  agent id lispm-skills--trust-tool input
                  (lambda (decision denial)
                    (cond
                     ((eq decision 'deny)
                      (funcall unevaluated "the user did not let its code run, so it was not evaluated"
                               (lispm-skills--reason denial)))
                     ((not (equal (lispm-skills--hash file (setq text (lispm-skills--file-text file)))
                                  hash))
                      (funcall unevaluated "its code changed while the user was asked, so it was not \
evaluated; load the skill again for them to see the new code"))
                     (t
                      (when (eq decision 'allow)
                        (lispm-skills--trust file hash))
                      (condition-case err
                          ;; The code the user saw, as the hash says.
                          (funcall done (lispm-skills--loaded-text
                                         skill (let ((lispm-current-agent agent))
                                                 (lispm-skills--evaluate
                                                  skill agent (lispm-skills--forms text)))))
                        (error (funcall done (format "The code of skill %s failed: %s"
                                                     (plist-get skill :name) (error-message-string err))
                                        t))))))
                  "Yes, and trust this code until it changes"))
      (funcall unevaluated "its code was not evaluated: that needs the user's approval, which is \
asked only in a turn, or M-x lispm-trust-skill"))))

(defvar lispm-chat-agent)

(defun lispm-skills--show-code (file)
  "Show the code of the skill in FILE, and the files beside it, in a buffer."
  (with-current-buffer (get-buffer-create "*LispM skill code*")
    (let ((inhibit-read-only t)
          (beside (lispm-skills--beside file))
          (blocks (lispm-skills--blocks (lispm-skills--file-text file))))
      (erase-buffer)
      ;; Lisp data mode, not Emacs Lisp mode: no hook of the user's,
      ;; such as one byte-compiling the buffer, runs on code not trusted.
      (lisp-data-mode)
      (insert ";; The code of " (abbreviate-file-name file) ", its blocks marked :lispm load.\n"
              (if beside
                  (format ";; It may load the files beside it: %s.\n"
                          (mapconcat (lambda (other)
                                       (file-relative-name other (file-name-directory file)))
                                     beside ", "))
                "")
              (if blocks "" ";; It has none: the skill is its text alone.\n")
              "\n"
              (string-join blocks "\n"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer (current-buffer))))

(defun lispm-skills--read-trust (distrust)
  "Read a skill of this project for `lispm-trust-skill'; DISTRUST is passed on."
  (let* ((agent (or (bound-and-true-p lispm-session-agent) (bound-and-true-p lispm-chat-agent)))
         (root (if agent (lispm-agent-root agent) (lispm--project-root)))
         (skills (lispm-skills--in (expand-file-name ".lispm/skills/" root))))
    (unless skills
      (user-error "No skills in %s" (abbreviate-file-name (expand-file-name ".lispm/skills/" root))))
    (let ((name (completing-read (if distrust "Trust no longer the skill: " "Trust the code of the skill: ")
                                 (mapcar (lambda (skill) (plist-get skill :name)) skills) nil t)))
      (list (plist-get (seq-find (lambda (skill) (equal (plist-get skill :name) name)) skills) :file)
            distrust))))

;;;###autoload
(defun lispm-trust-skill (file &optional distrust)
  "Trust the code of the project's skill in FILE, as it is now.
Agents loading it then have its code evaluated without asking, until
it changes (see `lispm-trusted-skills').  With DISTRUST, interactively
a prefix argument, trust it no longer: its code is asked about again.
Interactively, the skill is one of the current project's, and its code
is shown before you decide."
  (interactive (lispm-skills--read-trust current-prefix-arg))
  (let ((name (plist-get (lispm-skills--read file) :name)))
    (if distrust
        (progn
          (lispm-skills--trust file nil)
          (message "LispM: the code of skill %s is asked about again" name))
      (let ((hash (lispm-skills--hash file)))
        (when (called-interactively-p 'any)
          (save-window-excursion
            (lispm-skills--show-code file)
            (unless (yes-or-no-p (format "Trust the code of the skill %s, as shown? " name))
              (user-error "The code of skill %s is not trusted" name))))
        (lispm-skills--trust file hash)
        (message "LispM: the code of skill %s is trusted, until it changes" name)))))

;;;; The skill tool

(lispm-register-tool
 (lispm-tool--create
  :name "skill"
  :description
  (lambda ()
    (concat "Load a skill: how to do a kind of thing with Emacs, which is your operating system. "
            "Load one when a request calls for it; its instructions are then yours, and its tools "
            "too, from your next step on. The code of a skill of your project (.lispm/skills/ in "
            "your root) runs only if the user lets it: else you get the instructions alone. To keep "
            "what you worked out, such as a tool you defined, for later sessions, write a skill "
            "with write_skill.\n"
            "Skills:\n"
            (mapconcat (lambda (skill)
                         (format "- %s: %s" (plist-get skill :name) (plist-get skill :description)))
                       (lispm-skills lispm-current-agent) "\n")))
  :input-schema
  '((type . "object")
    (properties (name (type . "string") (description . "The skill to load.")))
    (required . ["name"]))
  :permission 'read
  :async t
  :function
  (lambda (done input)
    (let* ((agent lispm-current-agent)
           (name (alist-get 'name input))
           (skill (lispm-skills--find name agent)))
      (unless skill
        (error "No skill named %s; there are: %s" name
               (mapconcat (lambda (s) (plist-get s :name)) (lispm-skills agent) ", ")))
      (let* ((text (lispm-skills--file-text (plist-get skill :file)))
             (problem nil)
             (forms (condition-case err
                        (lispm-skills--forms text)
                      (error (setq problem (error-message-string err))))))
        (cond
         ;; Its text may still serve.
         (problem
          (funcall done (lispm-skills--loaded-text
                         skill nil (concat "its code was not evaluated: " problem))))
         ((lispm-skill-trusted-p skill text)
          (funcall done (lispm-skills--loaded-text skill (lispm-skills--evaluate skill agent forms))))
         (t (lispm-skills--load-asking agent lispm-current-call skill done))))))))

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
  (let ((directory (lispm-skills--write-directory))
        (existing (lispm-skills--find name agent)))
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

(defun lispm-skills--write-preview (name description instructions &optional code title)
  "The skill file write_skill would write, as a diff against the file there now.
NAME, DESCRIPTION, INSTRUCTIONS, CODE and TITLE are its arguments."
  (let ((file (lispm-skills--target name lispm-current-agent)))
    (lispm--diff (lispm--file-text file)
                 (lispm-skills--compose name description instructions code title)
                 (lispm-tools--label file))))

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
  :always-ask t
  :preview #'lispm-skills--write-preview
  (let* ((agent lispm-current-agent)
         (file (lispm-skills--target name agent))
         (text (lispm-skills--compose name description instructions code title))
         (forms (condition-case err
                    (lispm-skills--forms text)
                  (error (error "Skill %s was not saved: %s" name (error-message-string err)))))
         (open (find-buffer-visiting file)))
    (when (and open (buffer-modified-p open))
      (error "%s is open in Emacs with unsaved changes; ask the user to save or discard them"
             (abbreviate-file-name file)))
    (lispm-tools--check-writable file)
    (lispm-tools--check-new-file file)
    ;; The code runs there, as it will when the skill is loaded.
    (make-directory (file-name-directory file) t)
    (let ((tools (condition-case err
                     (lispm-skills--evaluate (list :name name :file file) agent forms)
                   (error (error "The code of skill %s failed, so the skill was not saved: %s
What the code defined before the error stays in Emacs until it exits; fix the code and \
write the skill again"
                                 name (error-message-string err))))))
      (let ((coding-system-for-write 'utf-8))
        (write-region text nil file nil 'silent))
      (when open
        (lispm-tools--sync open))
      (concat (format "Skill %s loaded%s.\n" name
                      (if tools (format "; tools now available: %s" (string-join tools ", ")) ""))
              (format "Saved as %s: agents load it from now on, in later sessions too."
                      (abbreviate-file-name file))))))

;;;; Installing skills

(defun lispm-skills--source (source)
  "The skill file of SOURCE: NAME.org, or SKILL.org in the directory NAME/.
A file SKILL.org stands for its directory.  An error if SOURCE is
neither."
  (let ((source (expand-file-name source)))
    (cond ((file-directory-p source)
           (let ((file (expand-file-name "SKILL.org" source)))
             (unless (file-regular-p file)
               (user-error "%s holds no SKILL.org: it is no skill" (abbreviate-file-name source)))
             file))
          ((not (file-regular-p source))
           (user-error "No skill file %s" (abbreviate-file-name source)))
          ((string-suffix-p ".org" source) source)
          (t (user-error "A skill is NAME.org, or a directory holding SKILL.org: not %s"
                         (abbreviate-file-name source))))))

(defun lispm-skills--label (name file)
  "FILE, a file of the skill NAME, as shown in diffs."
  (if (equal (file-name-nondirectory file) "SKILL.org")
      (concat name "/SKILL.org")
    (concat name ".org")))

(defun lispm-skills--install-diff (name old new text)
  "The diff from the skill NAME installed in OLD to the one in NEW.
OLD and NEW are skill files, and TEXT is NEW's text; the files beside
them are compared too."
  (let* ((beside (lambda (file)
                   (mapcar (lambda (other) (cons (file-relative-name other (file-name-directory file)) other))
                           (lispm-skills--beside file))))
         (old-beside (funcall beside old))
         (new-beside (funcall beside new))
         (text-of (lambda (files relative)
                    (if-let* ((file (cdr (assoc relative files))))
                        (lispm-skills--file-text file)
                      "")))
         (diffs (delete "(no changes)"
                        (cons (lispm--diff (lispm-skills--file-text old) text (lispm-skills--label name new))
                              (mapcar (lambda (relative)
                                        (lispm--diff (funcall text-of old-beside relative)
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
  (interactive (list (read-file-name "Install the skill (NAME.org, or a directory with SKILL.org): "
                                     nil nil t)))
  (let* ((file (lispm-skills--source source))
         (directory-p (equal (file-name-nondirectory file) "SKILL.org"))
         (name (plist-get (lispm-skills--read file) :name))
         (text (lispm-skills--file-text file))
         (hash (lispm-skills--hash file text))
         (installed (let ((existing (lispm-skills--target name nil)))
                      (and (file-exists-p existing) existing)))
         (directory (lispm-skills--write-directory))
         (target (expand-file-name (concat name (if directory-p "/" ".org")) directory))
         (target-file (if directory-p (expand-file-name "SKILL.org" target) target))
         (other (expand-file-name (concat name (if directory-p ".org" "/")) directory)))
    (condition-case err
        (lispm-skills--forms text)
      (error (user-error "Skill %s was not installed: %s" name (error-message-string err))))
    (cond
     ((lispm-skills--same-file-p file target-file))
     ((and directory-p (or (file-in-directory-p target (file-name-directory file))
                           (file-in-directory-p (file-name-directory file) target)))
      (user-error "Skill %s is not installed in %s from inside it, nor from a directory holding it"
                  name (abbreviate-file-name target)))
     (t
      (dolist (location (list target-file (if directory-p other (expand-file-name "SKILL.org" other))))
        (when-let* ((open (find-buffer-visiting location)))
          (when (buffer-modified-p open)
            (user-error "%s is open in Emacs with unsaved changes; save or discard them first"
                        (abbreviate-file-name location)))))
      (unless noconfirm
        (save-window-excursion
          (if installed
              (lispm-skills--show-diff (lispm-skills--install-diff name installed file text))
            (lispm-skills--show-code file))
          (unless (yes-or-no-p (format "%s the skill %s, as shown? " (if installed "Replace" "Install") name))
            (user-error "Skill %s was not installed" name)))
        (unless (equal (lispm-skills--hash file) hash)
          (user-error "The code of skill %s changed while you were asked; install it again to see it \
as it is now" name)))
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
                  (write-region text nil (expand-file-name "SKILL.org" copy) nil 'silent)
                  (when (file-directory-p target)
                    (delete-directory target t))
                  (rename-file (directory-file-name copy) (directory-file-name target)))
              (error (when (file-directory-p copy)
                       (delete-directory copy t))
                     (signal (car err) (cdr err)))))))
      ;; The skill as it was, if it was the other kind.
      (cond ((and directory-p (file-regular-p other))
             (delete-file other))
            ((and (not directory-p) (file-exists-p (expand-file-name "SKILL.org" other)))
             (delete-directory other t)))
      (when-let* ((open (find-buffer-visiting target-file)))
        (lispm-tools--sync open))
      (when (called-interactively-p 'any)
        (message "LispM: skill %s installed in %s" name (abbreviate-file-name directory)))))
    name))

;;;###autoload
(defun lispm-delete-skill (name)
  "Delete the skill NAME from the user's skills.
Those are the skills in `lispm-write-skill-directory'; one of LispM's
own, of a project or of another of `lispm-skill-directories' is not
deleted.  Agents that loaded it keep what its code defined until Emacs
exits.  Interactively, you are asked first."
  (interactive
   (list (completing-read "Delete the skill: "
                          (or (mapcar (lambda (skill) (plist-get skill :name))
                                      (lispm-skills--in (lispm-skills--write-directory)))
                              (user-error "No skills in %s"
                                          (abbreviate-file-name (lispm-skills--write-directory))))
                          nil t)))
  (lispm-skills--check-name name)
  (let* ((directory (lispm-skills--write-directory))
         (file (expand-file-name (concat name ".org") directory))
         (skill-directory (expand-file-name (concat name "/") directory))
         (dir-p (file-regular-p (expand-file-name "SKILL.org" skill-directory))))
    (when (or (file-in-directory-p directory lispm-skills--builtin)
              (not (or (file-regular-p file) dir-p)))
      (let ((other (lispm-skills--find name (or (bound-and-true-p lispm-session-agent)
                                                (bound-and-true-p lispm-chat-agent)))))
        (cond ((null other)
               (user-error "No skill named %s in %s" name (abbreviate-file-name directory)))
              ((file-in-directory-p (plist-get other :file) lispm-skills--builtin)
               (user-error "Skill %s is one of LispM's own; it is not deleted" name))
              ((plist-get other :project)
               (user-error "Skill %s is the project's, in %s; it is not deleted" name
                           (abbreviate-file-name (file-name-directory (plist-get other :file)))))
              (t
               (user-error "Skill %s is in %s, not among the skills installed in %s; it is not deleted"
                           name (abbreviate-file-name (file-name-directory (plist-get other :file)))
                           (abbreviate-file-name directory))))))
    (when (and (called-interactively-p 'any)
               (not (yes-or-no-p (format "Delete the skill %s? " name))))
      (user-error "Skill %s was not deleted" name))
    (when (file-regular-p file)
      (delete-file file t))
    (when dir-p
      (delete-directory skill-directory t t))
    name))

;;;; Resumed sessions

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
  "Evaluate again the code of the skills the session in this buffer loaded.
That of a project's skill only while the user trusts it as it is now."
  (when-let* ((agent (and (bound-and-true-p lispm-session-mode) lispm-session-agent)))
    (dolist (name (lispm-skills--loaded))
      (when-let* ((skill (lispm-skills--find name agent)))
        (condition-case err
            (let ((text (lispm-skills--file-text (plist-get skill :file))))
              (if (lispm-skill-trusted-p skill text)
                  (lispm-skills--evaluate skill agent (lispm-skills--forms text))
                (message "LispM: skill %s: a project's skill whose code you have not trusted as \
it is now, so it was not evaluated again (see `lispm-trust-skill')" name)))
          (error (message "LispM: skill %s: %s" name (error-message-string err))))))))

(add-hook 'lispm-session-mode-hook #'lispm-skills--restore)

(provide 'lispm-skills)
;;; lispm-skills.el ends here
