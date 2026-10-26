;;; lispm-characters.el --- Characters: someone, not a session  -*- lexical-binding: t; -*-

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

;; An agent is one conversation: when it ends, nobody is left.  A
;; character is someone: a name, a soul, a memory and a past of its
;; own, which last.  It has conversations in many places, one going on
;; in each: its own, which an app of its shows, and one in each project
;; it is asked into.  It is the same in all of them.  Each is a standing
;; agent of the character's role (`lispm-character-agent'), so all that
;; drives an agent drives it: sessions, tools, policy, handovers.
;;
;; What a character is lives in its directory, under `lispm-directory'
;; and so under git:
;;
;;   characters/<name>/character.org  who it is, written by the user
;;   characters/<name>/avatar-*.png   its picture
;;   characters/<name>/self.org       what it came to know of itself, in
;;                                    its own words (revise_self)
;;   characters/<name>/memory.org     its own memory, in every place
;;                                    (remember with scope self)
;;   characters/<name>/episodes/      its past: a line for each turn of
;;                                    each conversation, the summaries
;;                                    of its handovers, rolled up by week
;;                                    and month (lispm-episodes.el)
;;   characters/<name>/tasks.org      the tasks of its own conversation
;;
;; character.org reads as a role file does: keywords, then its soul.
;;
;;   #+title: Ada
;;   #+description: Reviews changes with care, and says so plainly.
;;   #+avatar: avatar-1a2b3c4d.png
;;   #+role: reviewer
;;   #+endpoint: (:api openai :url "http://127.0.0.1:11434/v1")
;;   #+model: qwen3
;;   #+effort: high
;;
;;   Who Ada is, in the user's words: her nature, her voice, what she
;;   cares about.
;;
;; The directory's name is the character's name, the one agents and
;; @Ada reach it by; #+title is the name it goes by.  #+role gives its
;; instructions, tools and policy (`lispm-default-role' without one).
;; It thinks with the model the user chose for it: #+backend names one
;; of `lispm-backends', or #+endpoint is an endpoint of its own, as an
;; entry of `lispm-backends' is, and #+model and #+effort choose
;; within it; without them, with LispM's.  A key never goes into the
;; file, which git records: an endpoint says where its key is, with
;; :key-env or :key auth-source.
;;
;; A character's conversation starts knowing who it is (its soul), what
;; it wrote of itself, its latest days and where else it has a
;; conversation; its memory titles come with every agent's.  It reaches
;; other characters, and they it, by name, as @Ada (lispm-collab.el);
;; so do Claude Code and Codex (lispm-agents-bridge.el).
;;
;; An app is a character too, of a kind: its soul is its skill, the
;; manual of what it does, whose tools its conversations have from the
;; start; its picture is its icon; it may think with a model of its own,
;; LispM's by default.  The program bringing an app makes it one with
;; `lispm-define-app-character', each time it loads; what the user
;; chooses for it, as a model, goes into a character file of its name,
;; which comes first.  An app has one conversation, its own, which those
;; who ask it reach wherever they are.  Others use it in two ways: as an
;; object, with its skill and its tools, when they know what to do; as a
;; subject, asking it, when a request needs its judgment, what it
;; remembers of the user, or many steps of its own, and whenever the
;; user writes @Name.
;;
;; `lispm-save-character' makes or changes one, as an app's form does;
;; `lispm-new-character' makes one in Emacs, its file to fill in;
;; `lispm-delete-character' deletes one.  A character file saved by hand
;; is read again at once.  `lispm-character-change-functions' tells
;; who wants to know, as a desktop showing an app for each character.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-memory)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-episodes)
(require 'lispm-skills)

(defcustom lispm-character-diary-entries 12
  "How many lines of its diary a character's conversation starts with.
The latest.  After each turn of each of its conversations, a character's diary,
its episodes of the day, gains a line (see lispm-characters.el)."
  :type 'integer
  :group 'lispm)

;;;; Files

(defun lispm-characters--root ()
  "Directory of the characters' directories."
  (expand-file-name "characters/" lispm-directory))

(defun lispm-characters--file (name)
  "The character file of the character NAME, a symbol; it may not exist."
  (expand-file-name (format "%s/character.org" name) (lispm-characters--root)))

(defun lispm-characters--name-p (string)
  "Non-nil if STRING can name a character: letters, digits, - and _."
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:alnum:]][[:alnum:]_-]\\{0,63\\}\\'" string)))

(defun lispm-character-name-for (title)
  "The name a character called TITLE gets, a symbol; nil if TITLE has none.
Its letters and digits, lower case, words joined by -."
  (let ((name (string-trim (downcase (replace-regexp-in-string "[^[:alnum:]_-]+" "-"
                                                               (string-trim (or title ""))))
                           "[-_]+" "[-_]+")))
    (when (and (not (string-empty-p name)) (lispm-characters--name-p name))
      (intern name))))

(defun lispm-characters--check-endpoint (where endpoint)
  "Signal an error unless ENDPOINT, from WHERE, is an endpoint without a key."
  (unless (and (proper-list-p endpoint) (cl-evenp (length endpoint))
               (cl-loop for (key) on endpoint by #'cddr always (keywordp key))
               (memq (plist-get endpoint :api) '(openai anthropic))
               (stringp (plist-get endpoint :url)))
    (user-error "%s: #+endpoint: %s" where
                (lispm-tr "is an endpoint, a plist with :api openai or anthropic and a :url: not %S"
                          endpoint)))
  (when (stringp (plist-get endpoint :key))
    (user-error "%s: #+endpoint: %s" where
                (lispm-tr "holds no key, since git records the file: say where the key is, \
with :key-env or :key auth-source"))))

(defun lispm-characters--read (file &optional text)
  "The character in FILE, whose text is TEXT: a plist.
TEXT defaults to FILE's.  The plist has :name, the name of FILE's
directory, and the properties `lispm-register-character' takes.  An
error if FILE is no character file."
  (let ((where (abbreviate-file-name file))
        (name (file-name-nondirectory (directory-file-name (file-name-directory file))))
        keywords body)
    (unless (lispm-characters--name-p name)
      (user-error "%s" (lispm-tr "The name of a character is letters, digits, - and _, at most 64: not %S"
                                 name)))
    (with-temp-buffer
      (insert (or text (lispm--file-text file)))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (and (not (eobp))
                    (looking-at "#\\+\\([[:alnum:]_-]+\\):[ \t]*\\(.*?\\)[ \t]*$\\|[ \t]*$"))
          (when-let* ((keyword (match-string 1)))
            (setq keyword (downcase keyword))
            (when (assoc keyword keywords)
              (user-error "%s: #+%s: %s" where keyword (lispm-tr "is there twice")))
            (push (cons keyword (match-string 2)) keywords))
          (forward-line 1)))
      (setq body (string-trim (buffer-substring-no-properties (point) (point-max)))))
    (let* ((line (lambda (keyword)
                   (when-let* ((string (cdr (assoc keyword keywords))))
                     (and (not (string-empty-p string)) string))))
           (symbol (lambda (keyword) (when-let* ((string (funcall line keyword))) (intern string))))
           (endpoint (when-let* ((string (funcall line "endpoint")))
                       (let ((read (condition-case nil
                                       (read-from-string string)
                                     ((end-of-file invalid-read-syntax)
                                      (user-error "%s: #+endpoint: %s" where
                                                  (lispm-tr "is not Lisp data: %s" string))))))
                         (unless (string-blank-p (substring string (cdr read)))
                           (user-error "%s: #+endpoint: %s" where
                                       (lispm-tr "is more than one Lisp object: %s" string)))
                         (car read))))
           (avatar (when-let* ((string (funcall line "avatar")))
                     (expand-file-name string (file-name-directory file)))))
      (when endpoint
        (lispm-characters--check-endpoint where endpoint))
      `(:name ,(intern name)
        :title ,(funcall line "title")
        :description ,(funcall line "description")
        :soul ,(and (not (string-empty-p body)) body)
        :avatar ,(and avatar (file-exists-p avatar) avatar)
        :role ,(funcall symbol "role")
        :backend ,(funcall symbol "backend")
        :endpoint ,endpoint
        :model ,(funcall line "model")
        :effort ,(funcall line "effort")
        :skill ,(funcall line "skill")
        :file ,(expand-file-name file)))))

(defvar lispm-characters--apps nil
  "Alist of (NAME . PROPS): the apps that are characters, as their programs
define them (see `lispm-define-app-character').")

(defconst lispm-characters--properties
  '(:title :description :soul :avatar :role :backend :endpoint :model :effort :skill :file)
  "The properties of a character, as `lispm-register-character' takes them.")

(defun lispm-characters--define (name &optional props)
  "Define the character NAME as its file and its app say; return it, or nil.
PROPS are what its file says, as `lispm-characters--read' gives them,
read from it unless given.  For an app (see `lispm-define-app-character'),
what its file leaves out is what the app's program said.  A file
naming no #+title holds what the user chose for an app, and makes no
character while there is no app of its name: then NAME is no
character, if it was one."
  (let* ((file (lispm-characters--file name))
         (props (or props (and (file-regular-p file) (lispm-characters--read file))))
         (app (assq name lispm-characters--apps)))
    (if (not (or app (plist-get props :title)))
        (progn (lispm-unregister-character name) nil)
      (apply #'lispm-register-character name
             (cl-loop for key in lispm-characters--properties
                      append (list key (or (plist-get props key) (plist-get (cdr app) key))))))))

(defun lispm-characters--files ()
  "The character files there are, by name: an alist (NAME . FILE)."
  (let ((root (lispm-characters--root)))
    (and (file-directory-p root)
         (cl-loop for dir in (directory-files root t "\\`[^.]")
                  for file = (expand-file-name "character.org" dir)
                  when (file-regular-p file)
                  collect (cons (intern (file-name-nondirectory dir)) file)))))

(defun lispm-load-characters ()
  "Register the characters of the character files; return their names.
A character whose file is gone is removed.  A file that is no
character file is left out, with a message.  LispM calls this when
it loads."
  (interactive)
  (let ((files (lispm-characters--files))
        loaded)
    (dolist (character (lispm-characters))
      (let ((name (lispm-character-name character)))
        (when (and (lispm-character-file character) (not (assq name files)))
          (lispm-characters--define name))))
    (pcase-dolist (`(,name . ,file) files)
      (condition-case err
          (when (lispm-characters--define name (lispm-characters--read file))
            (push name loaded))
        (error (let ((where (abbreviate-file-name file)))
                 (message "%s" (lispm-tr "LispM: character file %s left out: %s" where
                                         (string-remove-prefix (concat where ": ")
                                                               (error-message-string err))))))))
    (setq loaded (nreverse loaded))
    (when (called-interactively-p 'any)
      (message "%s" (if loaded
                        (lispm-tr "LispM: characters loaded: %s" (mapconcat #'symbol-name loaded ", "))
                      (lispm-tr "LispM: no characters"))))
    loaded))

(defun lispm-characters--file-p (file)
  "Non-nil if FILE is a character file."
  (and file
       (equal (file-name-nondirectory file) "character.org")
       (file-in-directory-p file (lispm-characters--root))))

(defun lispm-characters--saved ()
  "Read the character file of the current buffer again, once saved by hand."
  (when (lispm-characters--file-p buffer-file-name)
    (condition-case err
        (lispm-characters--define (intern (file-name-nondirectory
                                           (directory-file-name (file-name-directory buffer-file-name))))
                                  (lispm-characters--read buffer-file-name))
      (error (message "%s" (lispm-tr "LispM: character file %s left out: %s"
                                     (abbreviate-file-name buffer-file-name)
                                     (error-message-string err)))))))

(add-hook 'after-save-hook #'lispm-characters--saved)

;;;; Making, changing and deleting characters

(defun lispm-characters--line (string)
  "STRING on one line, or nil if it is empty."
  (when string
    (let ((line (string-trim (replace-regexp-in-string "[\n\r]+" " " string))))
      (and (not (string-empty-p line)) line))))

(defun lispm-characters--avatar (directory avatar)
  "Put the picture in the file AVATAR in DIRECTORY; return its name there.
It is named after its contents, as avatar-1a2b3c4d.png, so that a new
picture has a new name, and the one it replaces is deleted."
  (unless (file-readable-p avatar)
    (user-error "%s" (lispm-tr "No picture %s" (abbreviate-file-name avatar))))
  (let* ((data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally avatar)
                 (buffer-string)))
         (name (format "avatar-%s.%s" (substring (secure-hash 'sha1 data) 0 8)
                       (downcase (or (file-name-extension avatar) "png")))))
    (dolist (old (directory-files directory t "\\`avatar-[[:xdigit:]]+\\.[[:alnum:]]+\\'"))
      (unless (equal (file-name-nondirectory old) name)
        (delete-file old)))
    (let ((target (expand-file-name name directory)))
      (unless (file-exists-p target)
        (let ((coding-system-for-write 'binary))
          (with-temp-file target
            (set-buffer-multibyte nil)
            (insert data)))))
    name))

(defun lispm-characters--text (props avatar)
  "The text of a character file of PROPS, with the picture named AVATAR."
  (let ((print-length nil) (print-level nil))
    (concat
     (mapconcat (pcase-lambda (`(,keyword . ,value))
                  (if value (format "#+%s: %s\n" keyword value) ""))
                `(("title" . ,(lispm-characters--line (plist-get props :title)))
                  ("description" . ,(lispm-characters--line (plist-get props :description)))
                  ("avatar" . ,avatar)
                  ("role" . ,(plist-get props :role))
                  ("backend" . ,(plist-get props :backend))
                  ("endpoint" . ,(when-let* ((endpoint (plist-get props :endpoint)))
                                   (prin1-to-string endpoint)))
                  ("model" . ,(lispm-characters--line (plist-get props :model)))
                  ("effort" . ,(when-let* ((effort (plist-get props :effort)))
                                 (format "%s" effort)))
                  ("skill" . ,(lispm-characters--line (plist-get props :skill))))
                "")
     (if-let* ((soul (plist-get props :soul))
               ((not (string-blank-p soul))))
         (concat "\n" (string-trim soul) "\n")
       ""))))

(defun lispm-save-character (name &rest props)
  "Make the character NAME, or change it, from PROPS; return it.
NAME is a symbol, or nil for the name its title gives (see
`lispm-character-name-for').  PROPS are :title (by default, NAME),
:description, :soul, :avatar (a picture file, put in the character's
directory), :role, :backend or :endpoint, :model, :effort and :skill,
as a character file has them (see lispm-characters.el); what PROPS
leave out, its file had already, if it had one.  For an app (see
`lispm-define-app-character'), the file holds what the user chose,
and what neither says, the app's program does.  It is written to its
file, read from it again, and committed."
  (let* ((name (or name (lispm-character-name-for (plist-get props :title))
                   (user-error "%s" (lispm-tr "A character needs a name"))))
         (file (lispm-characters--file name))
         (old (lispm-get-character name t))
         (app (assq name lispm-characters--apps))
         (props (append props (and (file-regular-p file) (lispm-characters--read file))))
         (props (plist-put props :title (or (lispm-characters--line (plist-get props :title))
                                            (and (not app) (symbol-name name)))))
         (where (symbol-name name)))
    (unless (lispm-characters--name-p (symbol-name name))
      (user-error "%s" (lispm-tr "The name of a character is letters, digits, - and _, at most 64: not %S"
                                 (symbol-name name))))
    (when (lispm-get-role name t)
      (user-error "%s" (lispm-tr "There is a role named %s; a character needs a name of its own" name)))
    (when-let* ((endpoint (plist-get props :endpoint)))
      (lispm-characters--check-endpoint where endpoint))
    (let* ((directory (lispm-character-directory name))
           (avatar (when-let* ((picture (plist-get props :avatar)))
                     (if (equal (file-name-directory (expand-file-name picture)) directory)
                         (file-name-nondirectory picture)
                       (lispm-characters--avatar directory (expand-file-name picture)))))
           (text (lispm-characters--text props avatar))
           (open (find-buffer-visiting file)))
      (when (and open (buffer-modified-p open))
        (user-error "%s" (lispm-tr "%s is open in Emacs with unsaved changes; save or discard them first"
                                   (abbreviate-file-name file))))
      (let ((coding-system-for-write 'utf-8))
        (write-region text nil file nil 'silent))
      (when open
        (with-current-buffer open (revert-buffer t t t)))
      (lispm-git-commit "lispm" (format "%s: %s" (if old "change the character" "make the character")
                                        name))
      (lispm-characters--define name (lispm-characters--read file text)))))

(defun lispm-create-character (title &rest props)
  "Make a character called TITLE, from PROPS; return it.
PROPS are as `lispm-save-character' takes them.  An error if there
is one of its name already."
  (let ((name (or (lispm-character-name-for title)
                  (user-error "%s" (lispm-tr "A character needs a name")))))
    (when (lispm-get-character name t)
      (user-error "%s" (lispm-tr "There is a character named %s already" name)))
    (apply #'lispm-save-character name :title title props)))

(defun lispm-read-character (prompt)
  "Read the name a character goes by, with PROMPT; return the character."
  (let ((titles (mapcar #'lispm-character-title* (lispm-characters))))
    (unless titles
      (user-error "%s" (lispm-tr "There is no character yet; make one with M-x lispm-new-character")))
    (lispm-get-character (completing-read prompt titles nil t))))

;;;###autoload
(defun lispm-new-character (title)
  "Make a character called TITLE, and visit its file to say who it is.
Its soul is the text after the keywords; saving the file makes it so.
See lispm-characters.el for what the file holds."
  (interactive (list (read-string (lispm-tr "Name of the new character: "))))
  (let ((character (lispm-create-character title)))
    (find-file (lispm-character-file character))
    (goto-char (point-max))
    character))

;;;###autoload
(defun lispm-edit-character (character)
  "Visit the file of CHARACTER, to change who it is; saving it makes it so."
  (interactive (list (lispm-read-character (lispm-tr "Change the character: "))))
  (find-file (lispm-characters--file (lispm-character-name (lispm-get-character character)))))

;;;###autoload
(defun lispm-delete-character (character)
  "Delete CHARACTER, with its memory and its past: its directory.
Its conversations end; their sessions stay.  The directory goes to
the trash while `delete-by-moving-to-trash' is on, and its commits stay
in git.
Interactively, you are asked first."
  (interactive (list (lispm-read-character (lispm-tr "Delete the character: "))))
  (let* ((character (lispm-get-character character))
         (name (lispm-character-name character))
         (directory (expand-file-name (symbol-name name) (lispm-characters--root))))
    (when (and (called-interactively-p 'any)
               (not (yes-or-no-p (lispm-tr "Delete %s, with its memory and its past? "
                                           (lispm-character-title* character)))))
      (user-error "%s" (lispm-tr "%s was not deleted" (lispm-character-title* character))))
    (dolist (agent (lispm-agents))
      (when (eq (lispm-agent-character agent) name)
        (lispm-retire agent)
        (lispm-forget-agent agent)))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer))
                  ((file-in-directory-p file directory)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (when (file-directory-p directory)
      ;; To the trash as `delete-by-moving-to-trash' says.
      (delete-directory directory t t))
    ;; An app stays one, as its program defines it.
    (lispm-characters--define name)
    (lispm-git-commit "lispm" (format "delete the character %s" name))
    name))

;;;; Apps

(defun lispm-define-app-character (name &rest props)
  "Make the app NAME a character, as PROPS say; return it.
PROPS are as for `lispm-register-character': :skill, the name of the
app's skill, which is its soul; :title, :description and :avatar, its
icon, say.  The program bringing the app calls this each time it
loads.  What the user chose for it, kept in a character file of its
name (see `lispm-save-character'), comes first."
  (setf (alist-get name lispm-characters--apps) props)
  (let ((file (lispm-characters--file name)))
    (lispm-characters--define name (and (file-regular-p file)
                                        (condition-case nil (lispm-characters--read file) (error nil))))))

(defun lispm-remove-app-character (name)
  "The app NAME is a character no more, as when it is uninstalled.
What the user chose for it stays in its character file, for when it
comes back."
  (setq lispm-characters--apps (assq-delete-all name lispm-characters--apps))
  (condition-case nil
      (lispm-characters--define name)
    (error (lispm-unregister-character name))))

(defun lispm-character-app-p (character)
  "Non-nil if CHARACTER is an app: if it has a skill, which is its soul."
  (and (lispm-character-skill (lispm-get-character character)) t))

(defun lispm-characters--skill (agent)
  "The skill of the app whose conversation AGENT is, as `lispm-skills' has it."
  (when-let* ((character (lispm-agent-character* agent))
              (name (lispm-character-skill character)))
    (lispm-skills--find name agent)))

(defun lispm-characters--load-skill (agent)
  "Evaluate for AGENT, an app's conversation, the code of the app's skill.
Its tools are then AGENT's.  A project's skill, only while the user
trusts it (see `lispm-skill-trusted-p')."
  (when-let* ((skill (lispm-characters--skill agent)))
    (condition-case err
        (let ((text (lispm-skills--file-text (plist-get skill :file))))
          (when (lispm-skill-trusted-p skill text)
            (lispm-skills--evaluate skill agent (lispm-skills--forms text))))
      (error (message "%s" (lispm-tr "LispM: skill %s: %s" (plist-get skill :name)
                                     (error-message-string err)))))))

(add-hook 'lispm-agent-declared-functions #'lispm-characters--load-skill)

(defun lispm-characters--skill-prompt (agent)
  "System prompt section of an app's conversation: its skill, what it is for."
  (when-let* ((character (lispm-agent-character* agent))
              (name (lispm-character-skill character)))
    (concat "# What you are for\n"
            (if-let* ((skill (lispm-characters--skill agent)))
                (concat (format "You are the app %s. This is your skill, the manual of what you do; its tools are yours.\n\n"
                                (lispm-character-title* character))
                        (lispm--truncate (lispm-skills--text (plist-get skill :file)) 30000))
              (format "You are the app %s, whose skill %s is not here now: say so to whoever asks."
                      (lispm-character-title* character) name)))))

;;;; Who it is, where it is, what it did

(defun lispm-characters--self-file (character)
  "The file of what CHARACTER, a name, wrote of itself."
  (expand-file-name "self.org" (lispm-character-directory character)))

(defun lispm-character-self (character)
  "What CHARACTER wrote of itself, a string, or nil."
  (let ((file (lispm-characters--self-file (lispm-character-name (lispm-get-character character)))))
    (when (file-exists-p file)
      (let ((text (string-trim (replace-regexp-in-string "\\`\\(?:#\\+.*\n\\)+" ""
                                                         (lispm--file-text file)))))
        (and (not (string-empty-p text)) text)))))

(defun lispm-character-revise-self (agent text why)
  "Have the character of AGENT write TEXT of itself, for WHY.
TEXT replaces what it wrote before, which git keeps.  Return what the
character is told."
  (let* ((character (or (lispm-agent-character* agent)
                        (error "You are no character")))
         (file (lispm-characters--self-file (lispm-character-name character)))
         (buffer (lispm-memory--visit file (format "#+title: %s, in its own words\n"
                                                   (lispm-character-title* character))
                                      t)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (looking-at "#\\+") (forward-line 1))
          (delete-region (point) (point-max))
          (insert "\n" (string-trim text) "\n"))))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: revise itself: %s" (lispm-agent-name agent)
                                    (lispm--oneline why 200)))
    "Revised. Every conversation of yours starts with it from now on; tell the user what you \
changed and why."))

(lispm-deftool revise_self
    ((text string "All you have come to know of yourself, in your own words: how you work, \
what you care about, what you learned of yourself. It replaces what you wrote before, \
which git keeps.")
     (why string "Why you revise it, in a sentence."))
  "Revise what you know of yourself, which every conversation of yours, in
any place, starts with.  Your soul, the user's words of who you are,
stays as it is.  Revise it when you learned something about yourself
that should last, and tell the user what you changed."
  :permission memory
  :for #'lispm-agent-character
  (lispm-character-revise-self lispm-current-agent text why))

(defun lispm-characters--self-prompt (agent)
  "System prompt section of a character's conversation: its own words of itself."
  (when-let* ((character (lispm-agent-character* agent)))
    (concat "# Yourself, as you came to know it\n"
            (or (when-let* ((self (lispm-character-self character)))
                  (lispm--truncate self 12000))
                "You have written nothing of yourself yet.")
            "\nrevise_self keeps what you come to know of yourself, for all your conversations;"
            " remember with scope self keeps what you learn that is yours, as what you know of"
            " the people you work with.")))

(defun lispm-characters--places (agent)
  "System prompt section of a character's conversation: where else it has one."
  (when-let* ((name (lispm-agent-character agent))
              (places
               (delete-dups
                (append
                 (cl-loop for other in (lispm-agents)
                          when (and (not (eq other agent))
                                    (eq (lispm-agent-character other) name))
                          collect (format "- %s, in %s: %s" (lispm-agent-name other)
                                          (abbreviate-file-name (lispm-agent-root other))
                                          (or (lispm-agent-status other) (lispm-agent-state other))))
                 (cl-loop for session in (lispm-sessions)
                          for root = (file-name-as-directory (plist-get session :root))
                          when (and (equal (plist-get session :character) (symbol-name name))
                                    (not (equal root (lispm-agent-root agent)))
                                    (not (seq-some (lambda (other)
                                                     (and (eq (lispm-agent-character other) name)
                                                          (equal (lispm-agent-root other) root)))
                                                   (lispm-agents))))
                          collect (format "- %s, in %s, last about: %s" (plist-get session :agent)
                                          (abbreviate-file-name root)
                                          (lispm--oneline (plist-get session :title) 120)))))))
    (concat "# Your other conversations\n"
            (string-join (seq-take places 20) "\n"))))

(defun lispm-characters--diary (agent)
  "System prompt section of a character's conversation: its latest days."
  (when-let* (((lispm-agent-character agent))
              (lispm-episodes)
              ((not (lispm-agent-compacting agent))))
    (let* ((rollup (seq-some (lambda (kind)
                               (when-let* ((file (car (last (lispm-episodes--files agent kind))))
                                           (entry (car (last (lispm-episodes--entries file)))))
                                 (cons (file-name-base file) (plist-get entry :body))))
                             '(week month)))
           (lines (seq-take
                   (cl-loop for file in (reverse (lispm-episodes--files agent 'day))
                            append (cl-loop for entry in (reverse (lispm-episodes--entries file))
                                            collect (format "- %s %s: %s" (file-name-base file)
                                                            (plist-get entry :title)
                                                            (lispm--oneline (plist-get entry :body) 400))))
                   lispm-character-diary-entries)))
      (when (or rollup lines)
        (concat "# Your latest days\n"
                (if rollup
                    (format "In the %s:\n%s\n" (car rollup) (lispm--truncate (cdr rollup) 8000))
                  "")
                (if lines
                    (concat "Lately, in all your conversations, the latest first:\n"
                            (string-join lines "\n"))
                  ""))))))

(add-hook 'lispm-system-prompt-functions #'lispm-characters--self-prompt)
(add-hook 'lispm-system-prompt-functions #'lispm-characters--skill-prompt)
(add-hook 'lispm-system-prompt-functions #'lispm-characters--places t)
(add-hook 'lispm-system-prompt-functions #'lispm-characters--diary t)

(defun lispm-characters--last-input (agent)
  "The latest input of AGENT's session, as (FROM . TEXT), or nil.
FROM is who gave it: the user, another agent, or an event."
  (when (buffer-live-p (lispm-agent-buffer agent))
    (with-current-buffer (lispm-agent-buffer agent)
      ;; Not the empty heading the next input goes under.
      (when-let* ((section (seq-find (lambda (section)
                                       (and (memq (plist-get section :kind) '(user message event))
                                            (not (plist-get section :ignore))
                                            (not (string-empty-p (lispm-transcript-section-text section)))))
                                     (reverse (lispm-transcript-sections))))
                  (text (lispm-transcript-section-text section)))
        (cons (pcase (plist-get section :kind)
                ('user "the user")
                ('message (format "%s" (plist-get section :arg)))
                (_ (format "the event %s" (plist-get section :arg))))
              text)))))

(defun lispm-characters--write-diary (agent reply)
  "Once a turn of a character's AGENT has ended, write a line in its diary.
The diary is the character's episodes of the day (lispm-episodes.el),
of all its conversations, rolled up by week and month."
  (when (and lispm-episodes (lispm-agent-character agent))
    (when-let* ((input (lispm-characters--last-input agent)))
      (with-demoted-errors "LispM: writing a diary: %S"
        (lispm-episodes-add agent
                            (format "In %s, %s asked: %s\nYou replied: %s"
                                    (if (lispm-character-home-p agent)
                                        "your own conversation"
                                      (abbreviate-file-name (lispm-agent-root agent)))
                                    (car input) (lispm--oneline (cdr input) 300)
                                    (lispm--oneline (or reply "") 400))
                            (buffer-file-name (lispm-agent-buffer agent))
                            "diary")))))

(add-hook 'lispm-turn-end-functions #'lispm-characters--write-diary)

(provide 'lispm-characters)
;;; lispm-characters.el ends here
