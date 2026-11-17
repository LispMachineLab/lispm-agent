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
;; `lispm-character-form' is the form making or changing one, which
;; `lispm-new-character' and `lispm-edit-character' show, and so may an
;; app; `lispm-save-character' does it from Lisp, and
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
(require 'lispm-ui-form)

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
        :appearance ,(funcall line "appearance")
        :workflow ,(funcall line "workflow")
        :knowledge ,(funcall line "knowledge")
        :embedding-model ,(funcall line "embedding-model")
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
  '(:title :description :soul :avatar :appearance :workflow :knowledge :embedding-model
    :role :backend :endpoint :model :effort :skill :file)
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
                  ("appearance" . ,(lispm-characters--line (plist-get props :appearance)))
                  ("workflow" . ,(lispm-characters--line (plist-get props :workflow)))
                  ("knowledge" . ,(lispm-characters--line (plist-get props :knowledge)))
                  ("embedding-model" . ,(lispm-characters--line (plist-get props :embedding-model)))
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

;;;; The form

(defun lispm-characters--role-choices ()
  "The roles a character may work with, for its form."
  (cons (cons nil (lispm-tr "Default (%s)" lispm-default-role))
        (mapcar (lambda (role) (cons (lispm-role-name role) (symbol-name (lispm-role-name role))))
                (lispm-roles))))

(defun lispm-characters--backend-choices ()
  "The backends a character may think with, for its form."
  (mapcar (lambda (entry) (cons (car entry) (symbol-name (car entry)))) lispm-backends))

(defun lispm-characters--effort-choices ()
  "The effort levels a character may think at, for its form."
  (cons (cons nil (lispm-tr "Default (%s)" lispm-effort))
        (mapcar (lambda (effort) (cons effort effort)) '("low" "medium" "high" "xhigh" "max"))))

(defun lispm-characters--thinking (kind)
  "A :when of the form: non-nil while the character thinks with KIND."
  (lambda (values) (eq (alist-get 'thinks values) kind)))

(defun lispm-characters--form-values (character)
  "The values of the form of CHARACTER, nil for a new one: an alist."
  (let ((endpoint (and character (lispm-character-endpoint character))))
    `((title . ,(and character (lispm-character-title* character)))
      (description . ,(and character (lispm-character-description character)))
      (avatar . ,(and character (lispm-character-avatar character)))
      (appearance . ,(and character (lispm-character-appearance character)))
      (workflow . ,(and character (lispm-character-workflow character)))
      (soul . ,(and character (lispm-character-soul character)))
      (knowledge . ,(and character (lispm-character-knowledge character)))
      (role . ,(and character (lispm-character-role character)))
      (thinks . ,(cond (endpoint 'endpoint)
                       ((and character (lispm-character-backend character)) 'backend)
                       (t 'lispm)))
      (backend . ,(and character (lispm-character-backend character)))
      (api . ,(or (plist-get endpoint :api) 'openai))
      (url . ,(plist-get endpoint :url))
      (key-env . ,(plist-get endpoint :key-env))
      (auth-source . ,(eq (plist-get endpoint :key) 'auth-source))
      (model . ,(and character (lispm-character-model character)))
      (effort . ,(and character (lispm-character-effort character)))
      (embedding-model . ,(and character (lispm-character-embedding-model character))))))

(defun lispm-characters--form-props (values)
  "What the values of a character's form, an alist, make it: a plist.
Those of who it is only if the form has them; an app's has not."
  (let* ((get (lambda (key) (alist-get key values)))
         (thinks (funcall get 'thinks)))
    (append
     (when (assq 'title values)
       (list :title (funcall get 'title) :description (funcall get 'description)
             :avatar (funcall get 'avatar)
             :appearance (funcall get 'appearance)
             :workflow (funcall get 'workflow)
             :soul (funcall get 'soul)
             :knowledge (funcall get 'knowledge)
             :role (funcall get 'role)))
     (list :backend (and (eq thinks 'backend) (funcall get 'backend))
           :endpoint (and (eq thinks 'endpoint)
                          `(:api ,(funcall get 'api) :url ,(funcall get 'url)
                            ,@(when-let* ((variable (funcall get 'key-env))) (list :key-env variable))
                            ,@(when (funcall get 'auth-source) (list :key 'auth-source))))
           :model (funcall get 'model)
           :effort (funcall get 'effort)
           :embedding-model (funcall get 'embedding-model)))))

(defun lispm-character-form (&optional character)
  "The form making a character, or changing CHARACTER; return its buffer.
That of an app chooses its model alone: who it is, its skill says.
Submitted, the character is made or changed (`lispm-save-character')."
  (let* ((character (and character (lispm-get-character character)))
         (app (and character (lispm-character-skill character))))
    (lispm-ui-form
     (if character (lispm-character-title* character) (lispm-tr "New Character"))
     (append
      (unless app
        `((who :type heading :label ,(lispm-tr "Character"))
          (title :type text :label ,(lispm-tr "Name") :required t)
          (description :type text :label ,(lispm-tr "Description")
                       :help ,(lispm-tr "What other agents are told of it."))
          (avatar :type picture :label ,(lispm-tr "Picture"))
          (appearance :type text :label ,(lispm-tr "Appearance")
                      :help ,(lispm-tr "What it looks like, for ComfyUI."))
          (workflow :type text :label ,(lispm-tr "ComfyUI workflow")
                    :help ,(lispm-tr "Workflow name or file; empty for the open workflow."))
          (soul :type text-area :label ,(lispm-tr "Soul") :lines 7
                :help ,(lispm-tr "Its nature, its voice, what it cares about."))
          (knowledge :type text :label ,(lispm-tr "Knowledge")
                     :help ,(lispm-tr "Files or folders of its knowledge base, separated by commas."))))
      `((thinking :type heading :label ,(lispm-tr "How it thinks"))
        ,@(unless app
            `((role :type choice :label ,(lispm-tr "Role") :choices lispm-characters--role-choices
                    :help ,(lispm-tr "Its instructions, tools and permissions."))))
        (thinks :type radio :label ,(lispm-tr "Model")
                :choices ((lispm . ,(lispm-tr "LispM's"))
                          (backend . ,(lispm-tr "A backend"))
                          (endpoint . ,(lispm-tr "An endpoint of its own"))))
        (backend :type choice :label ,(lispm-tr "Backend") :required t
                 :choices lispm-characters--backend-choices
                 :when ,(lispm-characters--thinking 'backend))
        (api :type radio :label ,(lispm-tr "API")
             :choices ((openai . "OpenAI") (anthropic . "Anthropic"))
             :when ,(lispm-characters--thinking 'endpoint))
        (url :type text :label ,(lispm-tr "URL") :required t
             :when ,(lispm-characters--thinking 'endpoint))
        (key-env :type text :label ,(lispm-tr "Key")
                 :help ,(lispm-tr "The variable holding its key: never the key itself.")
                 :when ,(lispm-characters--thinking 'endpoint))
        (auth-source :type check :text ,(lispm-tr "Look its key up in auth-source")
                     :when ,(lispm-characters--thinking 'endpoint))
        (model :type text :label ,(lispm-tr "Model name")
               :help ,(lispm-tr "Empty: the backend's own."))
        (effort :type choice :label ,(lispm-tr "Effort") :choices lispm-characters--effort-choices)
        (embedding-model :type text :label ,(lispm-tr "Embedding model")
                         :help ,(lispm-tr "For /v1/embeddings vector search; empty for local BM25."))))
     :values (lispm-characters--form-values character)
     :submit (if character (lispm-tr "Save") (lispm-tr "Create"))
     :on-submit (lambda (values)
                  (let ((props (lispm-characters--form-props values)))
                    (if character
                        (apply #'lispm-save-character (lispm-character-name character) props)
                      (apply #'lispm-create-character (plist-get props :title)
                             (cl-loop for (key value) on props by #'cddr
                                      unless (eq key :title) append (list key value)))))))))

;;;###autoload
(defun lispm-new-character ()
  "Make a character, in its form."
  (interactive)
  (pop-to-buffer (lispm-character-form)))

;;;###autoload
(defun lispm-edit-character (character)
  "Change CHARACTER, in its form."
  (interactive (list (lispm-read-character (lispm-tr "Change the character: "))))
  (pop-to-buffer (lispm-character-form character)))

;;;###autoload
(defun lispm-visit-character-file (character)
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

(defun lispm-character-conversations (character)
  "CHARACTER's conversations, one in each place it has one, the live first.
Each is a plist: :agent, its name; :root; :home, non-nil for the
character's own; :live, the agent, while it is; :state and :status, its
state and what it does, while it is; :title, what it was last about, and
:time, when, as float seconds, once recorded."
  (let* ((name (lispm-character-name (lispm-get-character character)))
         ;; Those declared only, never opened, have no conversation yet.
         (live (seq-filter (lambda (agent) (and (eq (lispm-agent-character agent) name)
                                                (buffer-live-p (lispm-agent-buffer agent))))
                           (lispm-agents)))
         (recorded (seq-filter (lambda (session)
                                 (equal (plist-get session :character) (symbol-name name)))
                               (lispm-sessions)))
         (roots (delete-dups
                 (append (mapcar #'lispm-agent-root live)
                         (mapcar (lambda (session) (file-name-as-directory (plist-get session :root)))
                                 recorded)))))
    (mapcar (lambda (root)
              (let ((agent (seq-find (lambda (agent) (equal (lispm-agent-root agent) root)) live))
                    (session (seq-find (lambda (session)
                                         (equal (file-name-as-directory (plist-get session :root)) root))
                                       recorded)))
                (list :agent (if agent (lispm-agent-name agent) (intern (plist-get session :agent)))
                      :root root
                      :home (lispm--character-root-p root)
                      :live agent
                      :state (and agent (lispm-agent-state agent))
                      :status (and agent (lispm-agent-status agent))
                      :title (plist-get session :title)
                      :time (plist-get session :time))))
            roots)))

(defun lispm-character-days (character &optional count)
  "The latest COUNT lines of CHARACTER's diary, the latest first.
COUNT is `lispm-character-diary-entries' by default.  Each is a plist:
:day, as 2026-10-27; :title, the time and the conversation, as \"14:02
ada@lispm\"; :text, what happened."
  (let* ((directory (expand-file-name "episodes/" (lispm-character-directory
                                                   (lispm-character-name (lispm-get-character character)))))
         (days (and (file-directory-p directory)
                    (directory-files directory t "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'"))))
    (seq-take (cl-loop for file in (reverse days)
                       append (cl-loop for entry in (reverse (lispm-episodes--entries file))
                                       collect (list :day (file-name-base file)
                                                     :title (plist-get entry :title)
                                                     :text (plist-get entry :body))))
              (or count lispm-character-diary-entries))))

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

;;;; Hybrid RAG knowledge base (Embeddings /v1/embeddings + local BM25 + Lorebook triggers)

(defcustom lispm-embedding-model nil
  "Default embedding model name for `/v1/embeddings' vector search, or nil.
When nil, character RAG uses local BM25 and Lorebook keyword matching
alone unless a character sets its own `#+embedding-model:'."
  :type '(choice (const :tag "Local BM25 only" nil) string)
  :group 'lispm)

(defcustom lispm-character-rag-top-k 4
  "How many knowledge chunks a character's conversation injects by default."
  :type 'integer
  :group 'lispm)

(defcustom lispm-character-rag-chunk-size 1000
  "Maximum characters per knowledge chunk before splitting by paragraphs."
  :type 'integer
  :group 'lispm)

(defvar lispm-embedding-function #'lispm-characters--fetch-embeddings
  "Function fetching embedding vectors for `(BACKEND MODEL TEXTS)'.
Returns a list of float vectors, one per string in TEXTS, or nil on failure.")

(defun lispm-character-knowledge-directory (character)
  "The `knowledge/' directory of CHARACTER, where its lorebook and documents live."
  (let ((name (lispm-character-name (lispm-get-character character))))
    (expand-file-name "knowledge/" (lispm-character-directory name))))

(defun lispm-characters--knowledge-paths (character)
  "Extra knowledge paths configured on CHARACTER via `#+knowledge:'."
  (let ((raw (lispm-character-knowledge (lispm-get-character character)))
        (base (file-name-directory
               (or (lispm-character-file (lispm-get-character character))
                   (expand-file-name "character.org"
                                     (lispm-character-directory
                                      (lispm-character-name (lispm-get-character character))))))))
    (cond ((null raw) nil)
          ((listp raw) (mapcar (lambda (p) (expand-file-name p base)) raw))
          ((stringp raw)
           (mapcar (lambda (part) (expand-file-name (string-trim part) base))
                   (split-string raw "," t "[ \t\n]+"))))))

(defun lispm-character-knowledge-files (character)
  "All readable `.org', `.md' and `.txt' knowledge files of CHARACTER."
  (let* ((character (lispm-get-character character))
         (dir (lispm-character-knowledge-directory character))
         (sources (append (and (file-directory-p dir) (list dir))
                          (lispm-characters--knowledge-paths character)))
         files)
    (dolist (source sources)
      (cond ((and (file-regular-p source) (file-readable-p source))
             (push (expand-file-name source) files))
            ((file-directory-p source)
             (dolist (file (directory-files-recursively
                            source "\\.\\(?:org\\|md\\|txt\\)\\'" nil))
               (when (and (file-regular-p file) (file-readable-p file)
                          (not (string-prefix-p "." (file-name-nondirectory file))))
                 (push (expand-file-name file) files))))))
    (delete-dups (nreverse files))))

(defun lispm-characters--rag-tokenize (text)
  "Tokenize TEXT into lowercase ASCII words and CJK character bigrams/unigrams."
  (let ((lower (downcase (or text "")))
        tokens)
    (save-match-data
      (let ((pos 0))
        (while (string-match "[a-z0-9_]+\\|[^[:ascii:] \t\n\r.,;:!?()\\[\\]{}\"'`~@#$%^&*+=<>|/\\-]+" lower pos)
          (let ((seg (match-string 0 lower)))
            (setq pos (match-end 0))
            (if (string-match-p "\\`[a-z0-9_]+\\'" seg)
                (when (> (length seg) 1)
                  (push seg tokens))
              (let ((len (length seg)))
                (dotimes (i len)
                  (push (substring seg i (1+ i)) tokens)
                  (when (< (1+ i) len)
                    (push (substring seg i (+ i 2)) tokens)))))))))
    (nreverse tokens)))

(defun lispm-characters--split-paragraphs (text max-size)
  "Split TEXT into pieces of at most MAX-SIZE characters along paragraphs."
  (let ((paras (split-string (string-trim (or text "")) "\n[ \t]*\n+" t))
        chunks current)
    (dolist (para paras)
      (setq para (string-trim para))
      (cond ((string-empty-p para) nil)
            ((null current) (setq current para))
            ((<= (+ (length current) 2 (length para)) max-size)
             (setq current (concat current "\n\n" para)))
            (t (push current chunks)
               (setq current para))))
    (when (and current (not (string-empty-p current)))
      (push current chunks))
    (nreverse chunks)))

(defun lispm-characters--file-chunks (file)
  "Split knowledge FILE into chunks: plists with :id, :file, :title,
:keywords, :text and :tokens."
  (let ((text (lispm--file-text file))
        (org-p (string-suffix-p ".org" (downcase file)))
        sections chunks)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (looking-at "#\\+\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)$")
        (forward-line 1))
      (let ((heading-re (if org-p "^\\(\\*+\\)[ \t]+\\(.*\\)$" "^\\(#+\\)[ \t]+\\(.*\\)$"))
            (start (point))
            (title (file-name-base file))
            (keywords nil))
        (while (re-search-forward heading-re nil t)
          (let ((body (string-trim (buffer-substring-no-properties start (match-beginning 0))))
                (next-line (match-string 2)))
            (when (not (string-empty-p body))
              (push (list title keywords body) sections))
            (if (and org-p (string-match "\\(.*?\\)[ \t]+:\\([[:alnum:]_@#%:]+\\):[ \t]*\\'" next-line))
                (setq title (string-trim (match-string 1 next-line))
                      keywords (split-string (match-string 2 next-line) ":" t))
              (setq title (string-trim next-line)
                    keywords nil))
            (forward-line 1)
            (when org-p
              (while (looking-at "^[ \t]*:\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)$")
                (when (string-equal-ignore-case (match-string 1) "KEYWORDS")
                  (setq keywords (append keywords
                                         (split-string (match-string 2) "[, \t]+" t))))
                (forward-line 1)))
            (setq start (point))))
        (let ((body (string-trim (buffer-substring-no-properties start (point-max)))))
          (when (not (string-empty-p body))
            (push (list title keywords body) sections)))))
    (pcase-dolist (`(,title ,keywords ,body) (nreverse sections))
      (dolist (piece (lispm-characters--split-paragraphs body lispm-character-rag-chunk-size))
        (let* ((full (concat title "\n" piece))
               (id (substring (secure-hash 'sha1 (concat file "\0" full)) 0 16)))
          (push (list :id id
                      :file file
                      :title title
                      :keywords (mapcar #'downcase keywords)
                      :text piece
                      :tokens (lispm-characters--rag-tokenize full))
                chunks))))
    (nreverse chunks)))

(defun lispm-character-rag-chunks (character)
  "All RAG chunks across CHARACTER's knowledge files."
  (mapcan #'lispm-characters--file-chunks (lispm-character-knowledge-files character)))

(defun lispm-characters--rag-bm25-scores (query chunks)
  "Return an alist of (CHUNK-ID . SCORE) for CHUNKS against QUERY
using BM25 and Lorebook keyword triggers."
  (let* ((q-tokens (delete-dups (lispm-characters--rag-tokenize query)))
         (q-lower (downcase (or query "")))
         (n (max 1 (length chunks)))
         (avgdl (/ (float (max 1 (cl-loop for c in chunks sum (length (plist-get c :tokens))))) n))
         (df (make-hash-table :test #'equal))
         (k1 1.5)
         (b 0.75))
    (dolist (c chunks)
      (dolist (tok (delete-dups (copy-sequence (plist-get c :tokens))))
        (puthash tok (1+ (gethash tok df 0)) df)))
    (mapcar
     (lambda (c)
       (let* ((tokens (plist-get c :tokens))
              (dl (length tokens))
              (tf (make-hash-table :test #'equal))
              (score 0.0))
         (dolist (tok tokens)
           (puthash tok (1+ (gethash tok tf 0)) tf))
         (dolist (qt q-tokens)
           (let ((f (gethash qt tf 0)))
             (when (> f 0)
               (let* ((doc-freq (gethash qt df 0))
                      (idf (log (+ 1.0 (/ (+ (- n doc-freq) 0.5) (+ doc-freq 0.5)))))
                      (num (* f (+ k1 1.0)))
                      (den (+ f (* k1 (+ (- 1.0 b) (* b (/ (float dl) avgdl)))))))
                 (setq score (+ score (* idf (/ num den))))))))
         ;; SillyTavern Lorebook / World Info keyword and title trigger boost.
         (dolist (kw (plist-get c :keywords))
           (when (and (not (string-empty-p kw)) (string-search kw q-lower))
             (setq score (+ score 5.0))))
         (let ((title-lower (downcase (or (plist-get c :title) ""))))
           (when (and (not (string-empty-p title-lower))
                      (or (string-search title-lower q-lower)
                          (string-search q-lower title-lower)))
             (setq score (+ score 2.0))))
         (cons (plist-get c :id) score)))
     chunks)))

(defun lispm-characters--cosine-similarity (v1 v2)
  "Cosine similarity between numeric sequences V1 and V2."
  (let ((dot 0.0) (n1 0.0) (n2 0.0)
        (len (min (length v1) (length v2))))
    (when (> len 0)
      (dotimes (i len)
        (let ((a (float (elt v1 i)))
              (b (float (elt v2 i))))
          (setq dot (+ dot (* a b))
                n1 (+ n1 (* a a))
                n2 (+ n2 (* b b)))))
      (if (or (zerop n1) (zerop n2))
          0.0
        (/ dot (* (sqrt n1) (sqrt n2)))))))

(defun lispm-characters--character-backend (character)
  "Backend plist for CHARACTER, falling back to `lispm-backend'."
  (let ((character (lispm-get-character character)))
    (or (lispm-character-endpoint character)
        (and (lispm-character-backend character)
             (cdr (assq (lispm-character-backend character) lispm-backends)))
        (cdr (assq lispm-backend lispm-backends)))))

(defun lispm-characters--fetch-embeddings (backend model texts)
  "Fetch embeddings from BACKEND's `/embeddings' endpoint for MODEL and TEXTS."
  (when-let* ((url (plist-get backend :url))
              (endpoint (concat (string-remove-suffix "/" url) "/embeddings"))
              (body (json-serialize `((model . ,model)
                                      (input . ,(vconcat texts))))))
    (let ((key (ignore-errors (lispm-api--key backend))))
      (with-temp-buffer
        (let* ((args (append (list "-sS" "--max-time" "15"
                                   "-H" "Content-Type: application/json")
                             (when key (list "-H" (concat "Authorization: Bearer " key)))
                             (list "-d" body endpoint)))
               (status (apply #'call-process lispm-curl-program nil t nil args)))
          (when (eq status 0)
            (goto-char (point-min))
            (when-let* ((json (ignore-errors
                                (json-parse-buffer :object-type 'alist :array-type 'list)))
                        (data (alist-get 'data json)))
              (mapcar (lambda (item) (alist-get 'embedding item))
                      (sort (copy-sequence data)
                            (lambda (a b) (< (or (alist-get 'index a) 0)
                                             (or (alist-get 'index b) 0))))))))))))

(defun lispm-characters--rag-index-file (character)
  "File caching embedding vectors for CHARACTER."
  (expand-file-name ".rag-index.eld"
                    (lispm-character-directory (lispm-character-name (lispm-get-character character)))))

(defun lispm-characters--rag-embedding-scores (character query chunks)
  "Return an alist of (CHUNK-ID . COSINE-SCORE) when an embedding model
is set, or nil."
  (let* ((character (lispm-get-character character))
         (model (or (lispm-character-embedding-model character) lispm-embedding-model))
         (backend (and model (lispm-characters--character-backend character))))
    (when (and model backend chunks)
      (let* ((index-file (lispm-characters--rag-index-file character))
             (cache (or (and (file-readable-p index-file)
                             (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents index-file)
                                 (read (current-buffer)))))
                        (make-hash-table :test #'equal)))
             (missing (seq-filter (lambda (c)
                                    (not (gethash (concat model ":" (plist-get c :id)) cache)))
                                  chunks))
             (dirty nil))
        (when missing
          (when-let* ((vecs (funcall lispm-embedding-function
                                     backend model
                                     (mapcar (lambda (c)
                                               (concat (plist-get c :title) "\n" (plist-get c :text)))
                                             missing))))
            (cl-loop for c in missing
                     for v in vecs
                     when v
                     do (puthash (concat model ":" (plist-get c :id)) v cache)
                        (setq dirty t))))
        (when dirty
          (ignore-errors
            (let ((print-length nil) (print-level nil))
              (with-temp-file index-file
                (prin1 cache (current-buffer))))))
        (when-let* ((q-vec (car (funcall lispm-embedding-function backend model (list query)))))
          (cl-loop for c in chunks
                   for v = (gethash (concat model ":" (plist-get c :id)) cache)
                   when v
                   collect (cons (plist-get c :id)
                                 (max 0.0 (lispm-characters--cosine-similarity q-vec v)))))))))

(defun lispm-character-rag-search (character query &optional limit)
  "Search CHARACTER's knowledge base for QUERY using hybrid RAG.
Return at most LIMIT (default `lispm-character-rag-top-k') matching
chunk plists, each with `:score'."
  (let* ((character (lispm-get-character character))
         (chunks (lispm-character-rag-chunks character))
         (limit (or limit lispm-character-rag-top-k)))
    (when chunks
      (let* ((bm25 (lispm-characters--rag-bm25-scores query chunks))
             (max-bm25 (max 1.0 (apply #'max 0.0 (mapcar #'cdr bm25))))
             (emb (lispm-characters--rag-embedding-scores character query chunks))
             (scored
              (cl-loop for c in chunks
                       for id = (plist-get c :id)
                       for b-score = (or (cdr (assoc id bm25)) 0.0)
                       for e-score = (cdr (assoc id emb))
                       for total = (if e-score
                                       (+ (* 0.6 e-score) (* 0.4 (/ b-score max-bm25)))
                                     b-score)
                       when (> total 0.0)
                       collect (plist-put (copy-sequence c) :score total))))
        (seq-take (sort scored (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
                  limit)))))

(defun lispm-character-rag-add (character title content &optional keywords)
  "Add or update a lorebook entry TITLE with CONTENT and KEYWORDS
in CHARACTER's knowledge base."
  (let* ((character (lispm-get-character character))
         (dir (lispm-character-knowledge-directory character))
         (file (expand-file-name "lorebook.org" dir))
         (tags (and keywords
                    (if (listp keywords)
                        keywords
                      (split-string keywords "[, \t:]+" t))))
         (tag-str (if tags (concat "  :" (string-join tags ":") ":") ""))
         (clean-title (lispm--oneline title 120)))
    (make-directory dir t)
    (let ((buffer (lispm-memory--visit
                   file (format "#+title: Knowledge of %s\n" (lispm-character-title* character))
                   t)))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((re (concat "^\\*+ +\\(?:" (regexp-quote clean-title) "\\)\\(?:[ \t]+:.*:\\)?[ \t]*$")))
              (if (re-search-forward re nil t)
                  (let ((beg (line-beginning-position))
                        (end (if (re-search-forward "^\\*+ " nil t)
                                 (line-beginning-position)
                               (point-max))))
                    (delete-region beg end)
                    (goto-char beg))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))))
            (insert "* " clean-title tag-str "\n" (string-trim content) "\n")))
        (lispm-memory--save buffer)))
    (lispm-git-commit (symbol-name (lispm-character-name character))
                      (format "%s: knowledge: %s" (lispm-character-name character) clean-title))
    file))

(defun lispm-characters--rag-format (chunks)
  "Format RAG CHUNKS for display in a prompt or tool result."
  (mapconcat (lambda (c)
               (format "[%s · %s]\n%s"
                       (file-name-nondirectory (plist-get c :file))
                       (plist-get c :title)
                       (plist-get c :text)))
             chunks "\n\n"))

(defun lispm-characters--rag-prompt (agent)
  "System prompt section injecting relevant knowledge from a character's RAG store."
  (when-let* ((character (lispm-agent-character* agent))
              (files (lispm-character-knowledge-files character)))
    (let* ((input (cdr (lispm-characters--last-input agent)))
           (hits (and input (not (string-empty-p input))
                      (lispm-character-rag-search character input lispm-character-rag-top-k))))
      (concat "# Your knowledge base (RAG)\n"
              (format "Sources (%d): %s. Use `rag_search' to query your knowledge base and `knowledge_add' to record new lorebook entries.\n"
                      (length files)
                      (mapconcat #'file-name-nondirectory (seq-take files 10) ", "))
              (if hits
                  (concat "\nRelevant passages for this turn:\n"
                          (lispm--truncate (lispm-characters--rag-format hits) 6000))
                "")))))

(add-hook 'lispm-system-prompt-functions #'lispm-characters--rag-prompt t)

(lispm-deftool rag_search
    ((query string "Words, concepts or names to look up in your knowledge base.")
     (limit integer "Maximum passages to return, 4 by default." :optional t))
  "Search your knowledge base (RAG lorebook and attached documents) using
hybrid vector embeddings and BM25 keyword retrieval."
  :permission read
  :for #'lispm-agent-character
  (let* ((character (or (lispm-agent-character* lispm-current-agent)
                        (error "You are no character")))
         (hits (lispm-character-rag-search character query (or limit lispm-character-rag-top-k))))
    (if hits
        (lispm-characters--rag-format hits)
      (if (lispm-character-knowledge-files character)
          "No matching passages found in your knowledge base."
        "Your knowledge base is empty; add entries with knowledge_add or place files in your knowledge/ directory."))))

(lispm-deftool knowledge_add
    ((title string "Heading of the knowledge or lorebook entry.")
     (content string "The knowledge content to store for RAG retrieval.")
     (keywords string "Comma-separated trigger keywords or tags for Lorebook matching." :optional t))
  "Add or update an entry in your personal RAG knowledge base (`knowledge/lorebook.org')."
  :permission memory
  :for #'lispm-agent-character
  (let ((character (or (lispm-agent-character* lispm-current-agent)
                       (error "You are no character"))))
    (lispm-character-rag-add character title content keywords)
    (format "Saved \"%s\" in your knowledge base." title)))

;;;; ComfyUI image generation for characters

(declare-function lispm-comfyui--canvas-buffers "lispm-comfyui" ())
(declare-function lispm-comfyui--need-types "lispm-comfyui" ())
(declare-function lispm-comfyui-open "lispm-comfyui" (name))
(declare-function lispm-comfyui-run "lispm-comfyui" (&optional callback))
(declare-function lispm-comfyui-graph-nodes "lispm-comfyui-graph" (graph))
(declare-function lispm-comfyui-node-type "lispm-comfyui-graph" (node))
(declare-function lispm-comfyui-node-title "lispm-comfyui-graph" (node))
(declare-function lispm-comfyui-set-widget "lispm-comfyui-graph" (node types name value))
(declare-function lispm-comfyui-types-of-server "lispm-comfyui-client" (callback &optional again))
(declare-function lispm-comfyui-queue "lispm-comfyui-client" (prompt workflow callback))
(declare-function lispm-comfyui-history "lispm-comfyui-client" (prompt-id callback))
(declare-function lispm-comfyui-fetch-image "lispm-comfyui-client" (image callback))

(defvar lispm-comfyui-generate-function #'lispm-characters--comfyui-generate
  "Function running a ComfyUI image generation request.
Called as `(FUNC PROMPT &key NEGATIVE-PROMPT WORKFLOW CALLBACK)' where
CALLBACK receives `(FILES ERROR)'.")

(defun lispm-characters--comfyui-set-prompts (buffer positive &optional negative)
  "Set POSITIVE and optional NEGATIVE prompt texts in ComfyUI canvas BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'lispm-comfyui--root) lispm-comfyui--root)
      (let* ((types (lispm-comfyui--need-types))
             (nodes (lispm-comfyui-graph-nodes lispm-comfyui--root))
             (encoders (seq-filter (lambda (n)
                                     (member (lispm-comfyui-node-type n)
                                             '("CLIPTextEncode" "CLIPTextEncodeSDXL"
                                               "CLIPTextEncodeFlux" "PrimitiveStringMultiline")))
                                   nodes))
             (neg-node (seq-find (lambda (n)
                                   (string-match-p "neg" (downcase (or (lispm-comfyui-node-title n) ""))))
                                 encoders))
             (pos-node (or (seq-find (lambda (n) (not (eq n neg-node))) encoders)
                           (car encoders))))
        (when pos-node
          (ignore-errors (lispm-comfyui-set-widget pos-node types "text" positive)))
        (when (and negative (or neg-node (cadr encoders)))
          (ignore-errors (lispm-comfyui-set-widget (or neg-node (cadr encoders))
                                                   types "text" negative)))))))

(defun lispm-characters--comfyui-default-prompt (checkpoint positive negative &optional seed)
  "Build a 7-node ComfyUI API prompt alist using CHECKPOINT, POSITIVE and NEGATIVE."
  (let ((seed (or seed (random 1000000000))))
    `((1 . ((class_type . "CheckpointLoaderSimple")
            (inputs . ((ckpt_name . ,checkpoint)))))
      (2 . ((class_type . "CLIPTextEncode")
            (inputs . ((text . ,positive)
                       (clip . ["1" 1])))))
      (3 . ((class_type . "CLIPTextEncode")
            (inputs . ((text . ,(or negative "low quality, blurry, deformed"))
                       (clip . ["1" 1])))))
      (4 . ((class_type . "EmptyLatentImage")
            (inputs . ((width . 512) (height . 512) (batch_size . 1)))))
      (5 . ((class_type . "KSampler")
            (inputs . ((seed . ,seed)
                       (steps . 20)
                       (cfg . 7.0)
                       (sampler_name . "euler")
                       (scheduler . "normal")
                       (denoise . 1.0)
                       (model . ["1" 0])
                       (positive . ["2" 0])
                       (negative . ["3" 0])
                       (latent_image . ["4" 0])))))
      (6 . ((class_type . "VAEDecode")
            (inputs . ((samples . ["5" 0])
                       (vae . ["1" 2])))))
      (7 . ((class_type . "SaveImage")
            (inputs . ((filename_prefix . "lispm_character")
                       (images . ["6" 0]))))))))

(defun lispm-characters--comfyui-First-checkpoint (types)
  "First checkpoint filename offered by the ComfyUI server's TYPES hash table."
  (when (hash-table-p types)
    (when-let* ((def (gethash "CheckpointLoaderSimple" types))
                (input (alist-get 'input def))
                (req (alist-get 'required input))
                (ckpt (alist-get 'ckpt_name req))
                (choices (car ckpt)))
      (cond ((vectorp choices) (and (> (length choices) 0) (aref choices 0)))
            ((listp choices) (car choices))))))

(defun lispm-characters--comfyui-poll-history (prompt-id attempts callback)
  "Poll ComfyUI `/history/PROMPT-ID' up to ATTEMPTS times, then call
CALLBACK with `(FILES ERROR)'."
  (if (<= attempts 0)
      (funcall callback nil "ComfyUI timed out waiting for image generation")
    (lispm-comfyui-history
     prompt-id
     (lambda (entry error)
       (cond
        (error (funcall callback nil error))
        ((null entry)
         (run-at-time 0.5 nil #'lispm-characters--comfyui-poll-history
                      prompt-id (1- attempts) callback))
        (t
         (let (images)
           (pcase-dolist (`(,_node . ,out) (alist-get 'outputs entry))
             (let ((imgs (alist-get 'images out)))
               (dolist (img (if (vectorp imgs) (append imgs nil) imgs))
                 (push img images))))
           (setq images (nreverse images))
           (if (null images)
               (funcall callback nil "ComfyUI finished without producing an image")
             (let ((pending (length images))
                   files failed)
               (dolist (img images)
                 (lispm-comfyui-fetch-image
                  img
                  (lambda (file err)
                    (if err (setq failed err) (push file files))
                    (when (zerop (cl-decf pending))
                      (if failed
                          (funcall callback nil failed)
                        (funcall callback (nreverse files) nil)))))))))))))))

(cl-defun lispm-characters--comfyui-generate (prompt &key negative-prompt workflow callback)
  "Generate an image with ComfyUI for PROMPT; call CALLBACK with `(FILES ERROR)'."
  (unless (require 'lispm-comfyui nil t)
    (funcall callback nil "LispM-ComfyUI (`lispm-comfyui') is not installed")
    (cl-return-from lispm-characters--comfyui-generate nil))
  (condition-case err
      (let ((buffer (car (lispm-comfyui--canvas-buffers))))
        (when (and workflow (not (string-empty-p workflow)) (not buffer))
          (save-window-excursion (lispm-comfyui-open workflow))
          (setq buffer (car (lispm-comfyui--canvas-buffers))))
        (if buffer
            (progn
              (lispm-characters--comfyui-set-prompts buffer prompt negative-prompt)
              (with-current-buffer buffer
                (lispm-comfyui-run
                 (lambda (run)
                   (if (eq (plist-get run :status) 'done)
                       (funcall callback (plist-get run :files) nil)
                     (funcall callback nil (or (plist-get run :message) "ComfyUI workflow run failed")))))))
          (lispm-comfyui-types-of-server
           (lambda (types error)
             (if error
                 (funcall callback nil error)
               (let ((ckpt (lispm-characters--comfyui-First-checkpoint types)))
                 (if (not ckpt)
                     (funcall callback nil "No checkpoint model found on the ComfyUI server")
                   (lispm-comfyui-queue
                    (lispm-characters--comfyui-default-prompt ckpt prompt negative-prompt)
                    nil
                    (lambda (answer q-err)
                      (if q-err
                          (funcall callback nil q-err)
                        (lispm-characters--comfyui-poll-history
                         (alist-get 'prompt_id answer) 120 callback)))))))))))
    (error (funcall callback nil (error-message-string err)))))

(cl-defun lispm-character-generate-image (character prompt &key negative-prompt set-avatar callback)
  "Generate an image for CHARACTER via ComfyUI with PROMPT.
Combines CHARACTER's `#+appearance:' with PROMPT when present.  When
SET-AVATAR is non-nil, saves the first generated image as CHARACTER's
`:avatar'.  Calls CALLBACK with `(FILES ERROR)'."
  (let* ((character (lispm-get-character character))
         (name (lispm-character-name character))
         (appearance (lispm-character-appearance character))
         (full-prompt (if (and appearance (not (string-empty-p appearance)))
                          (if (and prompt (not (string-empty-p prompt)))
                              (concat appearance ", " prompt)
                            appearance)
                        (or prompt (lispm-character-description character)
                            (lispm-character-title* character))))
         (workflow (lispm-character-workflow character)))
    (funcall lispm-comfyui-generate-function
             full-prompt
             :negative-prompt negative-prompt
             :workflow workflow
             :callback
             (lambda (files error)
               (when (and (not error) set-avatar (car files) (file-readable-p (car files)))
                 (lispm-save-character name :avatar (car files)))
               (when callback
                 (funcall callback files error))))))

;;;###autoload
(defun lispm-character-generate-avatar (character &optional prompt callback)
  "Generate a new avatar picture for CHARACTER with ComfyUI, and save it."
  (interactive (list (lispm-read-character (lispm-tr "Change the character: "))))
  (let* ((character (lispm-get-character character))
         (title (lispm-character-title* character)))
    (lispm-character-generate-image
     character
     (or prompt "portrait avatar, centered face, clean background")
     :set-avatar t
     :callback (lambda (files error)
                 (if error
                     (message "ComfyUI: %s" error)
                   (message "Updated avatar of %s." title))
                 (when callback (funcall callback files error))))))

(lispm-deftool generate_image
    ((prompt string "What to draw: scene, expression, or subject. Your `#+appearance:' is automatically prepended.")
     (negative_prompt string "What to avoid in the image." :optional t)
     (set_avatar boolean "If true, also set the generated image as your avatar." :optional t))
  "Generate an image with ComfyUI using your ComfyUI workflow (or the server's
checkpoint) and return the generated image so the user sees it in the chat."
  :permission web
  :async t
  :for #'lispm-agent-character
  (let ((character (or (lispm-agent-character* lispm-current-agent)
                       (error "You are no character"))))
    (lispm-character-generate-image
     character prompt
     :negative-prompt negative_prompt
     :set-avatar set_avatar
     :callback (lambda (files error)
                 (if error
                     (funcall done (format "ComfyUI image generation failed: %s" error) t)
                   (funcall done
                            (format "Generated %d image%s%s: %s"
                                    (length files)
                                    (if (= (length files) 1) "" "s")
                                    (if set_avatar " (and updated your avatar)" "")
                                    (mapconcat #'file-name-nondirectory files ", "))
                            nil files))))))

(provide 'lispm-characters)
;;; lispm-characters.el ends here
