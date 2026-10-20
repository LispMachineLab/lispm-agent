;;; lispm-roles.el --- Role files: roles written in org  -*- lexical-binding: t; -*-

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

;; A role can be written in an org file instead of with `lispm-defrole':
;;
;;   #+title: Reviewer
;;   #+description: Reviews changes for bugs; never edits.
;;   #+tools: (read_file search_files list_directory recall)
;;   #+policy: ((write . deny) (exec . deny))
;;   #+effort: high
;;
;;   Instructions, for the model to read ...
;;
;; NAME.org in a directory of `lispm-role-directories', ~/.lispm/roles/
;; by default, defines the role NAME.  The values of #+tools, #+policy
;; and #+effort are Lisp data, as `lispm-register-role' takes them;
;; without #+tools the role has every tool.  The text after the
;; keywords is the role's instructions.  A role file names no backend
;; or model: its agents use the user's, so that a role file can be
;; passed on.
;;
;; The roles of these files are registered when LispM loads
;; (`lispm-load-roles').  `lispm-install-role' installs a role file
;; from elsewhere, once the user has seen it, and `lispm-delete-role'
;; deletes one.  A role defined in Lisp is neither replaced by a role
;; file nor deleted.

;;; Code:

(require 'lispm-core)

(defcustom lispm-role-directories (list "~/.lispm/roles/")
  "Directories of role files: NAME.org defines the role NAME.
A role file in an earlier directory hides one of the same name in a
later one; `lispm-install-role' installs role files in the first.
Changed with Customize, the roles are loaded again (see
`lispm-load-roles')."
  :type '(repeat directory)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'lispm-load-roles)
           (lispm-load-roles)))
  :group 'lispm)

(defconst lispm-roles--builtin '(assistant)
  "The roles LispM itself defines, in Lisp.")

(defun lispm-roles--file-text (file)
  "The text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun lispm-roles--value (where keyword string)
  "The Lisp object STRING reads as, the value of KEYWORD in the role file WHERE.
An error if STRING is not one Lisp object."
  (let ((read (condition-case nil
                  (read-from-string string)
                ((end-of-file invalid-read-syntax)
                 (user-error "%s: #+%s: is not Lisp data: %s" where keyword string)))))
    (unless (string-blank-p (substring string (cdr read)))
      (user-error "%s: #+%s: is more than one Lisp object: %s" where keyword string))
    (car read)))

(defun lispm-roles--read (file &optional text)
  "The role in FILE, whose text is TEXT: a plist.
TEXT defaults to FILE's.  The plist has :name, the role's name (a
symbol), :title and :file, and the properties `lispm-register-role'
takes: :description, :instructions, :tools if the file names them,
:policy and :effort.  An error if FILE is no role file."
  (let ((where (abbreviate-file-name file))
        (name (file-name-base file))
        keywords body)
    (unless (string-suffix-p ".org" file)
      (user-error "A role file is NAME.org: not %s" where))
    (unless (let ((case-fold-search nil))
              (string-match-p "\\`[a-z0-9][a-z0-9_-]\\{0,63\\}\\'" name))
      (user-error "The name of a role is lowercase letters, digits, - and _, at most 64: not %S"
                  name))
    (with-temp-buffer
      (insert (or text (lispm-roles--file-text file)))
      (goto-char (point-min))
      ;; The keywords and blank lines it starts with; the rest is the body.
      (let ((case-fold-search t))
        (while (and (not (eobp))
                    (looking-at "#\\+\\([[:alnum:]_-]+\\):[ \t]*\\(.*?\\)[ \t]*$\\|[ \t]*$"))
          (when-let* ((keyword (match-string 1)))
            (setq keyword (downcase keyword))
            (when (assoc keyword keywords)
              (user-error "%s: #+%s: is there twice" where keyword))
            (push (cons keyword (match-string 2)) keywords))
          (forward-line 1)))
      (setq body (string-trim (buffer-substring-no-properties (point) (point-max)))))
    (dolist (keyword '("backend" "model"))
      (when (assoc keyword keywords)
        (user-error "%s: #+%s: a role file names no backend or model, its agents use yours; \
a role with its own is defined in Lisp, with `lispm-defrole'" where keyword)))
    (let* ((value (lambda (keyword)
                    (when-let* ((string (cdr (assoc keyword keywords))))
                      (lispm-roles--value where keyword string))))
           (line (lambda (keyword)
                   (when-let* ((string (cdr (assoc keyword keywords))))
                     (and (not (string-empty-p string)) string))))
           (tools (funcall value "tools"))
           (policy (funcall value "policy"))
           (effort (funcall value "effort")))
      (unless (or (eq tools t)
                  (and (proper-list-p tools)
                       (seq-every-p (lambda (tool)
                                      (or (stringp tool) (and tool (symbolp tool) (not (eq tool t)))))
                                    tools)))
        (user-error "%s: #+tools: is t, for every tool, or a list of tool names: not %S" where tools))
      (unless (and (proper-list-p policy)
                   (seq-every-p (lambda (entry)
                                  (and (consp entry) (car entry) (symbolp (car entry))
                                       (memq (cdr entry) lispm--policy-actions)))
                                policy))
        (user-error "%s: #+policy: is an alist from permission classes to auto, ask or deny, \
such as ((write . deny)): not %S" where policy))
      (setq effort (cond ((null (assoc "effort" keywords)) nil)
                         ((and (stringp effort) (not (string-empty-p effort))) effort)
                         ((and effort (symbolp effort) (not (eq effort t))) (symbol-name effort))
                         (t (user-error "%s: #+effort: is an effort level, such as high: not %S"
                                        where effort))))
      `(:name ,(intern name)
        :title ,(or (funcall line "title") name)
        :file ,(expand-file-name file)
        :description ,(funcall line "description")
        :instructions ,(and (not (string-empty-p body)) body)
        ,@(and (assoc "tools" keywords) (list :tools tools))
        :policy ,policy
        :effort ,effort))))

(defun lispm-roles--register (role)
  "Register ROLE, a plist from `lispm-roles--read', as the role of its file."
  (apply #'lispm-register-role (plist-get role :name)
         :description (plist-get role :description)
         :instructions (plist-get role :instructions)
         :policy (plist-get role :policy)
         :effort (plist-get role :effort)
         :file (plist-get role :file)
         (and (plist-member role :tools) (list :tools (plist-get role :tools)))))

(defun lispm-roles--check-free (name)
  "Signal an error if the role NAME is defined in Lisp: no role file replaces it."
  (when (or (memq name lispm-roles--builtin)
            (and (lispm-get-role name t) (not (lispm-role-file name))))
    (user-error "There is a role named %s already, defined in Lisp, which a role file \
would not replace; choose another name" name)))

(defun lispm-roles--files ()
  "The role files of `lispm-role-directories', by name: an alist (NAME . FILE).
Of files of the same name, that in the earliest directory."
  (let (files)
    (dolist (directory lispm-role-directories)
      (when (file-directory-p directory)
        (dolist (file (directory-files (expand-file-name directory) t "\\`[^.].*\\.org\\'"))
          (let ((name (intern (file-name-base file))))
            (when (and (file-regular-p file) (not (assq name files)))
              (push (cons name file) files))))))
    (nreverse files)))

(defun lispm-load-roles ()
  "Register the roles of the files in `lispm-role-directories'; return their names.
A role whose file is gone is removed.  A file that is no role file,
or that names a role defined in Lisp, is left out, with a message.
LispM calls this when it loads."
  (interactive)
  (let ((files (lispm-roles--files))
        loaded)
    (pcase-dolist (`(,name . ,_) lispm--role-files)
      (unless (assq name files)
        (lispm-unregister-role name)))
    (pcase-dolist (`(,name . ,file) files)
      (condition-case err
          (let ((role (lispm-roles--read file)))
            (lispm-roles--check-free name)
            (lispm-roles--register role)
            (push name loaded))
        (error (let ((where (abbreviate-file-name file)))
                 (message "LispM: role file %s left out: %s" where
                          (string-remove-prefix (concat where ": ") (error-message-string err)))))))
    (setq loaded (nreverse loaded))
    (when (called-interactively-p 'any)
      (message "LispM: %s" (if loaded
                               (format "roles loaded: %s" (mapconcat #'symbol-name loaded ", "))
                             "no role files")))
    loaded))

(defun lispm-roles--show (old new label)
  "Show the role file NEW, a string; if OLD is non-nil, as the diff from it.
LABEL names the file in the diff."
  (with-current-buffer (get-buffer-create "*LispM role*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Fundamental mode, not Org mode: no hook of the user's runs on
      ;; text from elsewhere.
      (fundamental-mode)
      (if (not old)
          (insert new)
        (insert (lispm--diff old new label))
        (diff-mode))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer (current-buffer))))

;;;###autoload
(defun lispm-install-role (file &optional noconfirm)
  "Install the role file FILE, and return the name of its role.
FILE is NAME.org (see lispm-roles.el); it is copied into the first of
`lispm-role-directories', and the role NAME registered, at once and
whenever LispM loads.  Unless NOCONFIRM, FILE is shown, as the diff
against the role file it replaces if there is one, and you are asked
first; what is installed is the text shown.  An error if FILE is no
role file, or if NAME is a role defined in Lisp."
  (interactive (list (read-file-name "Install the role file: " nil nil t)))
  (let* ((text (lispm-roles--file-text file))
         (role (lispm-roles--read file text))
         (name (plist-get role :name))
         (directory (file-name-as-directory
                     (expand-file-name (or (car lispm-role-directories)
                                           (user-error "No directory to install roles in: \
`lispm-role-directories' is empty")))))
         (target (expand-file-name (format "%s.org" name) directory))
         (old (seq-find (lambda (file) (and file (file-regular-p file)))
                        (list (lispm-role-file name) target)))
         (open (find-buffer-visiting target)))
    (lispm-roles--check-free name)
    (when (and open (buffer-modified-p open))
      (user-error "%s is open in Emacs with unsaved changes; save or discard them first"
                  (abbreviate-file-name target)))
    (unless noconfirm
      (save-window-excursion
        (lispm-roles--show (and old (lispm-roles--file-text old)) text (format "%s.org" name))
        (unless (yes-or-no-p (format "%s the role %s, as shown? " (if old "Replace" "Install") name))
          (user-error "The role %s was not installed" name))))
    (make-directory directory t)
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil target nil 'silent))
    (lispm-roles--register (plist-put role :file target))
    (when (called-interactively-p 'any)
      (message "LispM: role %s installed in %s" name (abbreviate-file-name directory)))
    name))

;;;###autoload
(defun lispm-delete-role (name)
  "Delete the role NAME, a symbol or a string: its role file, and the role.
Its file is deleted from every one of `lispm-role-directories'.  Live
agents of the role keep what they had from it (see
`lispm-unregister-role').  A role defined in Lisp, such as the built-in
assistant, is not deleted, nor is `lispm-default-role'.
Interactively, you are asked first."
  (interactive
   (list (completing-read "Delete the role: "
                          (or (mapcar (lambda (entry) (symbol-name (car entry))) lispm--role-files)
                              (user-error "No role is from a role file"))
                          nil t)))
  (let* ((name (if (stringp name) (intern name) name))
         (files (delete-dups
                 (seq-filter (lambda (file) (and file (file-exists-p file)))
                             (cons (lispm-role-file name)
                                   (mapcar (lambda (directory)
                                             (expand-file-name (format "%s.org" name) directory))
                                           lispm-role-directories))))))
    (cond ((memq name lispm-roles--builtin)
           (user-error "The role %s is LispM's own; it is not deleted" name))
          ((and (lispm-get-role name t) (not (lispm-role-file name)))
           (user-error "The role %s is defined in Lisp, not in a role file; it is not deleted" name))
          ((eq name lispm-default-role)
           (user-error "The role %s is `lispm-default-role'; choose another one first" name))
          ((not (or files (lispm-get-role name t)))
           (user-error "No role named %s" name)))
    (when (and (called-interactively-p 'any)
               (not (yes-or-no-p (format "Delete the role %s? " name))))
      (user-error "The role %s was not deleted" name))
    (dolist (file files)
      (delete-file file t))
    (lispm-unregister-role name)
    name))

(provide 'lispm-roles)
;;; lispm-roles.el ends here
