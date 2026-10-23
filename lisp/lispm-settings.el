;;; lispm-settings.el --- LispM's settings, for a window of settings  -*- lexical-binding: t; -*-

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

;; LispM's core settings as a window of settings shows and changes them:
;;
;;   Model        the backend and model agents use unless they name one,
;;                their effort, and how the backend stands;
;;   Permissions  what the default policy lets tools of each class do,
;;                and the project skills whose code is trusted;
;;   Memory       how large a conversation grows before it is summed up,
;;                whether memory is saved first, git's history, and the
;;                memory files themselves;
;;   Roles and skills  the role agents start with, and the directories
;;                of skills.
;;
;; The settings are the variables of `lispm-settings-variables', set as
;; Customize sets them.  Each function here takes effect at once and,
;; given SAVE, keeps the change for later starts; names and notes come
;; in the language spoken.  However a setting changes, here, in
;; Customize or with setq, `lispm-settings-change-hook' runs, so that a
;; window showing it can draw it again.  How a window looks is its own
;; business: nothing here draws one.
;;
;; Programs providing models, a gateway's, say, hook in here rather than
;; into a window: `lispm-model-choices-functions' offer models to choose
;; from, `lispm-backend-status-functions' tell how a backend stands, and
;; `lispm-backend-action-functions' offer what can be done about one,
;; such as setting it up or giving it a key.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'lispm-core)
(require 'lispm-roles)
(require 'lispm-runtime)
(require 'lispm-memory)
(require 'lispm-skills)

(defconst lispm-settings-variables
  '(lispm-backends lispm-backend lispm-model lispm-effort
    lispm-default-policy lispm-trusted-skills
    lispm-compact-threshold lispm-compact-save-memory lispm-git
    lispm-default-role lispm-role-directories lispm-skill-directories)
  "LispM's settings, as `lispm-settings-set' sets them and
`lispm-settings-change-hook' watches them.")

(defvar lispm-settings-change-hook nil
  "Normal hook run after a setting of `lispm-settings-variables' changed.
It runs once the change is made, by the functions here, in Customize or
with setq, and once for changes made together.")

(defvar lispm-settings--timer nil
  "Timer running `lispm-settings-change-hook' once a change is made.")

(defun lispm-settings--changed ()
  "Run `lispm-settings-change-hook', a change being made."
  (setq lispm-settings--timer nil)
  (run-hooks 'lispm-settings-change-hook))

(defun lispm-settings--watch (_symbol _value operation where)
  "Have `lispm-settings-change-hook' run once a setting changed.
OPERATION and WHERE are those of `add-variable-watcher'."
  (when (and (memq operation '(set makunbound)) (null where) (null lispm-settings--timer))
    (setq lispm-settings--timer (run-at-time 0 nil #'lispm-settings--changed))))

(dolist (variable lispm-settings-variables)
  (add-variable-watcher variable #'lispm-settings--watch))

(defun lispm-settings-set (variable value &optional save)
  "Set VARIABLE, one of `lispm-settings-variables', to VALUE, as Customize
sets it; SAVE it for later starts too."
  (unless (memq variable lispm-settings-variables)
    (error "%s is none of LispM's settings" variable))
  (if save
      (customize-save-variable variable value)
    (customize-set-variable variable value)))

;;;; Model

(defun lispm-settings--call (function &rest arguments)
  "Call FUNCTION, of another program's hook, with ARGUMENTS: an error of
its is shown as a message and answers nil, so that it breaks no window
of settings."
  (condition-case err
      (apply function arguments)
    (error (message "%s: %s" function (error-message-string err))
           nil)))

(defvar lispm-model-choices-functions nil
  "Functions offering models to choose from, besides the backends' own.
Each is called with no arguments and returns a list of plists:
  :backend  a key of `lispm-backends'
  :model    the model's name, as the backend takes it
  :label    optionally, what to call it instead
A program providing models adds one, a gateway listing those it serves.")

(defvar lispm-backend-status-functions nil
  "Functions telling how a backend stands, before LispM guesses.
Each is called with the backend's name and plist, both nil when there is
no backend, and returns nil, leaving it to the next, or a plist with
:state and :message, as `lispm-model-status' gives them.")

(defvar lispm-backend-action-functions nil
  "Functions offering what can be done about the backend agents use.
Each is called with the backend's name, nil when there is none, and
returns a list of actions (LABEL FUNCTION HELP): LABEL names the action
in the language spoken, FUNCTION, called with no arguments, does it, and
HELP, if given, says more.  A window of settings shows each as a button:
setting a provider up, giving it a key.")

(defconst lispm-efforts
  `(("low" . ,(lispm-tr-noop "Low" "effort"))
    ("medium" . ,(lispm-tr-noop "Medium" "effort"))
    ("high" . ,(lispm-tr-noop "High" "effort"))
    ("xhigh" . ,(lispm-tr-noop "Extra high" "effort"))
    ("max" . ,(lispm-tr-noop "Max" "effort")))
  "The effort levels, the least first, as `lispm-effort' takes them, each
with its name in English; see `lispm-effort-name'.")

(defun lispm-effort-name (effort)
  "EFFORT, a level of `lispm-efforts', named in the language spoken."
  (if-let* ((name (cdr (assoc effort lispm-efforts))))
      (lispm-tr name)
    effort))

(defun lispm-default-model ()
  "The model agents that name none use, nil when there is no backend."
  (or lispm-model (plist-get (alist-get (lispm-backend-name) lispm-backends) :model)))

(defun lispm-model-choices ()
  "The models offered to choose from, as plists (:backend NAME :model
MODEL [:label LABEL]): each backend's own model, those its :models
names, those of `lispm-model-choices-functions', and the one chosen."
  (let ((choices (mapcan (pcase-lambda (`(,name . ,backend))
                           (mapcar (lambda (model) (list :backend name :model model))
                                   (delete-dups (delq nil (cons (plist-get backend :model)
                                                                (copy-sequence (plist-get backend :models)))))))
                         lispm-backends)))
    (run-hook-wrapped 'lispm-model-choices-functions
                      (lambda (function)
                        (setq choices (append choices (lispm-settings--call function)))
                        nil))
    (when-let* ((name (lispm-backend-name))
                (model (lispm-default-model)))
      (setq choices (append choices (list (list :backend name :model model)))))
    (seq-uniq (seq-filter (lambda (choice) (assq (plist-get choice :backend) lispm-backends)) choices)
              (lambda (a b) (and (eq (plist-get a :backend) (plist-get b :backend))
                                 (equal (plist-get a :model) (plist-get b :model)))))))

(defun lispm-set-model (backend model &optional save)
  "Have agents that name no model use MODEL of BACKEND, and SAVE it.
BACKEND is a key of `lispm-backends', nil for the first whose key is in
the environment (see `lispm-backend').  MODEL nil, or the backend's own,
follows the backend's own.  Running agents take it from their next
request on, unless their role names a model or they switched to one."
  (when (and backend (not (assq backend lispm-backends)))
    (user-error "%s" (lispm-tr "Unknown backend %s (see `lispm-backends')" backend)))
  (lispm-settings-set 'lispm-backend backend save)
  ;; The backend's own model, once chosen: with BACKEND nil, the one the
  ;; environment then picks.
  (let ((own (plist-get (alist-get (lispm-backend-name) lispm-backends) :model)))
    (lispm-settings-set 'lispm-model (unless (equal model own) model) save)))

(defun lispm-set-effort (effort &optional save)
  "Have agents that name no effort level use EFFORT, and SAVE it.
EFFORT is a level of `lispm-efforts'."
  (unless (assoc effort lispm-efforts)
    (user-error "%s" (lispm-tr "No effort level %s: it is one of %s"
                               effort (mapconcat #'car lispm-efforts ", "))))
  (lispm-settings-set 'lispm-effort effort save))

(defun lispm-settings--backend-status (name backend)
  "How the backend NAME, of plist BACKEND, stands as far as LispM can tell
without asking anyone for its key: a plist of :state and :message, see
`lispm-model-status'."
  (let* ((key (plist-get backend :key))
         (variable (lispm--key-variable backend))
         (value (and variable (getenv variable)))
         (host (or (nth 2 (assq key lispm-key-sources))
                   (ignore-errors (url-host (url-generic-parse-url (plist-get backend :url)))))))
    (pcase-let
        ((`(,state ,message)
          (cond
           ((null name)
            (list 'none (lispm-tr "No backend: set OPENAI_API_KEY or ANTHROPIC_API_KEY, or add a backend of your own.")))
           ((null backend)
            (list 'unknown (lispm-tr "Unknown backend %s (see `lispm-backends')" name)))
           ((and value (not (string-empty-p value)))
            (list 'ready (lispm-tr "Ready; its key comes from %s." variable)))
           ((plist-get backend :key-env)
            (list 'no-key (lispm-tr "No key: set %s." variable)))
           ((null key) (list 'ready (lispm-tr "Ready; it needs no key.")))
           ((stringp key) (list 'ready (lispm-tr "Ready; its key is set.")))
           ((or (assq key lispm-key-sources) (eq key 'auth-source))
            (list 'unchecked
                  (if variable
                      (lispm-tr "%s is not set: its key is looked up in auth-source, for %s, as an agent first asks."
                                variable host)
                    (lispm-tr "Its key is looked up in auth-source, for %s, as an agent first asks." host))))
           ((functionp key) (list 'unchecked (lispm-tr "Its key is looked up as an agent first asks.")))
           (t (list 'no-key (lispm-tr "Bad :key in backend: %S" key))))))
      (list :state state :message message))))

(defun lispm-model-status ()
  "How the model agents that name none use stands, as a plist:
  :backend  the backend's name, nil when there is none
  :model    the model, nil when there is no backend
  :effort   the effort level, see `lispm-efforts'
  :state    `ready' when its key is at hand or it needs none;
            `unchecked' when its key is looked up as a request is
            made, in auth-source or by a function; `no-key' when it has
            none; `none' when no backend is chosen and no key is in the
            environment; `unknown' when `lispm-backend' names none of
            `lispm-backends'; or a state of a program's own
  :message  the state, said in the language spoken
The first of `lispm-backend-status-functions' answering tells the
state.  Nothing here asks for a key: auth-source may want a passphrase."
  (let* ((name (lispm-backend-name))
         (backend (alist-get name lispm-backends))
         (status (or (run-hook-wrapped 'lispm-backend-status-functions
                                       (lambda (function)
                                         (lispm-settings--call function name backend)))
                     (lispm-settings--backend-status name backend))))
    (list :backend name :model (and backend (lispm-default-model)) :effort lispm-effort
          :state (plist-get status :state)
          :message (or (plist-get status :message) (format "%s" (plist-get status :state))))))

(defun lispm-backend-actions ()
  "What can be done about the backend agents use, as the functions of
`lispm-backend-action-functions' offer it: (LABEL FUNCTION HELP) each."
  (let ((name (lispm-backend-name))
        (actions nil))
    (run-hook-wrapped 'lispm-backend-action-functions
                      (lambda (function)
                        (setq actions (append actions (lispm-settings--call function name)))
                        nil))
    actions))

;;;; Permissions

(defconst lispm-permission-classes
  `((read . ,(lispm-tr-noop "Read files, buffers and Emacs's state"))
    (memory . ,(lispm-tr-noop "Keep their own memory and tasks"))
    (comm . ,(lispm-tr-noop "Talk to other agents"))
    (lisp . ,(lispm-tr-noop "Run Lisp programs of tool calls"))
    (write . ,(lispm-tr-noop "Change files and buffers"))
    (shell . ,(lispm-tr-noop "Run shell commands"))
    (exec . ,(lispm-tr-noop "Evaluate any Lisp in Emacs"))
    (mcp . ,(lispm-tr-noop "Use the tools of MCP servers"))
    (web . ,(lispm-tr-noop "Search the web and fetch pages")))
  "The classes of tools a policy gives an action, as `lispm-default-policy'
names them, each with what its tools do, in English; see
`lispm-permission-class-description'.")

(defconst lispm-permission-actions
  `((auto . ,(lispm-tr-noop "Allow" "permission"))
    (ask . ,(lispm-tr-noop "Ask" "permission"))
    (deny . ,(lispm-tr-noop "Deny" "permission")))
  "The actions a policy gives a class of tools, the most permissive
first, each named in English; see `lispm-permission-action-name'.")

(defun lispm-permission-class-description (class)
  "What the tools of CLASS, one of `lispm-permission-classes', do, in the
language spoken."
  (if-let* ((description (alist-get class lispm-permission-classes)))
      (lispm-tr description)
    (symbol-name class)))

(defun lispm-permission-action-name (action)
  "ACTION, one of `lispm-permission-actions', named in the language spoken."
  (if-let* ((name (alist-get action lispm-permission-actions)))
      (lispm-tr name)
    (symbol-name action)))

(defun lispm-default-permission (class)
  "The action the default policy gives tools of CLASS: `auto', `ask' or
`deny'.  See `lispm-default-policy'."
  (or (lispm--policy-lookup class lispm-default-policy) 'ask))

(defun lispm-set-default-permission (class action &optional save)
  "Have the default policy give tools of CLASS the ACTION, and SAVE it.
CLASS is one of `lispm-permission-classes', ACTION `auto', `ask' or
`deny'.  A role's policy still overrides it for the role's agents."
  (unless (assq class lispm-permission-classes)
    (user-error "%s" (lispm-tr "No permission class %s: it is one of %s"
                               class (mapconcat (lambda (entry) (symbol-name (car entry)))
                                                lispm-permission-classes ", "))))
  (unless (assq action lispm-permission-actions)
    (user-error "%s" (lispm-tr "No policy action %s: it is auto, ask or deny" action)))
  (lispm-settings-set 'lispm-default-policy
                      (if (assq class lispm-default-policy)
                          (mapcar (lambda (entry) (if (eq (car entry) class) (cons class action) entry))
                                  lispm-default-policy)
                        (append lispm-default-policy (list (cons class action))))
                      save))

(defun lispm-trusted-skill-files ()
  "The files of the projects' skills whose code is trusted, see
`lispm-trusted-skills'."
  (mapcar #'car lispm-trusted-skills))

(defun lispm-untrust-skill (file)
  "Trust the code of the project's skill in FILE no longer, for good: an
agent loading it asks again.  FILE may be gone already."
  (lispm-skills--trust file nil))

;;;; Memory

(defconst lispm-compact-sizes
  `((small 60000 ,(lispm-tr-noop "Small" "conversation size")
           ,(lispm-tr-noop "summed up past 60,000 tokens: quicker, cheaper turns"))
    (medium 120000 ,(lispm-tr-noop "Medium" "conversation size")
            ,(lispm-tr-noop "summed up past 120,000 tokens"))
    (large 240000 ,(lispm-tr-noop "Large" "conversation size")
           ,(lispm-tr-noop "summed up past 240,000 tokens, for models with long contexts"))
    (asked nil ,(lispm-tr-noop "Only when asked" "conversation size")
           ,(lispm-tr-noop "summed up with /compact")))
  "The sizes a conversation may grow to before it is summed up, as a
window of settings offers them: (SIZE TOKENS NAME NOTE), TOKENS the
value of `lispm-compact-threshold', NAME and NOTE in English; see
`lispm-compact-size-name' and `lispm-compact-size-note'.")

(defun lispm-compact-size ()
  "The size of `lispm-compact-sizes' `lispm-compact-threshold' is, or nil
when it is one of the user's own."
  (car (seq-find (lambda (size) (equal (nth 1 size) lispm-compact-threshold))
                 lispm-compact-sizes)))

(defun lispm-compact-size-name (size)
  "SIZE, one of `lispm-compact-sizes', named in the language spoken."
  (lispm-tr (nth 2 (assq size lispm-compact-sizes))))

(defun lispm-compact-size-note (size)
  "What SIZE, one of `lispm-compact-sizes', means, in the language spoken."
  (lispm-tr (nth 3 (assq size lispm-compact-sizes))))

(defun lispm-set-compact-size (size &optional save)
  "Have conversations summed up past SIZE, one of `lispm-compact-sizes',
and SAVE it.  See `lispm-compact-threshold'."
  (unless (assq size lispm-compact-sizes)
    (user-error "%s" (lispm-tr "No size %s: it is one of %s"
                               size (mapconcat (lambda (entry) (symbol-name (car entry)))
                                               lispm-compact-sizes ", "))))
  (lispm-settings-set 'lispm-compact-threshold (nth 1 (assq size lispm-compact-sizes)) save))

(defun lispm-settings--title (file)
  "The title of the org FILE, or its name in `lispm-directory'."
  (let ((line (lispm--first-line file)))
    (if (and line (string-match "\\`#\\+title:[ \t]*\\(.+\\)" line))
        (match-string 1 line)
      (file-relative-name file lispm-directory))))

(defun lispm-memory-files ()
  "The memory files and task lists there are under `lispm-directory', as
\(TITLE . FILE), the one changed last first: projects' memory and tasks,
roles' memory."
  (let ((files (mapcan (lambda (pattern) (file-expand-wildcards (expand-file-name pattern lispm-directory)))
                       '("projects/*/memory.org" "projects/*/tasks.org" "roles/*/memory.org"))))
    (mapcar (lambda (file) (cons (lispm-settings--title file) file))
            (sort files (lambda (a b)
                          (time-less-p (file-attribute-modification-time (file-attributes b))
                                       (file-attribute-modification-time (file-attributes a))))))))

(defun lispm-read-memory-file (&optional prompt)
  "Read one of `lispm-memory-files' by its title, with PROMPT; return its file."
  (let ((files (or (lispm-memory-files)
                   (user-error "%s" (lispm-tr "No memory yet: agents write it as they learn")))))
    (cdr (assoc (completing-read (or prompt (lispm-tr "Memory: ")) files nil t) files))))

;;;###autoload
(defun lispm-open-memory (file)
  "Open FILE, one of LispM's memory files or task lists.
Interactively, it is chosen by its title, the one changed last first."
  (interactive (list (lispm-read-memory-file)))
  (pop-to-buffer (find-file-noselect file)))

;;;; Roles and skills

(defun lispm-default-role-choices ()
  "The roles agents may start with by default: every role but `system',
which only LispM itself has (see `lispm-system-agent')."
  (seq-remove (lambda (role) (eq (lispm-role-name role) 'system)) (lispm-roles)))

(defun lispm-set-default-role (role &optional save)
  "Have agents started without choosing a role be of ROLE, and SAVE it."
  (lispm-settings-set 'lispm-default-role (lispm-role-name (lispm-get-role role)) save))

(defun lispm-skill-directory-builtin-p (directory)
  "Non-nil when DIRECTORY holds LispM's own skills."
  (lispm-skills--same-file-p (file-name-as-directory directory)
                             (file-name-as-directory lispm-skills--builtin)))

(defun lispm-skill-directory-count (directory)
  "How many skills DIRECTORY holds, without reading them."
  (if (file-directory-p directory)
      (+ (length (directory-files directory nil "\\`[^.].*\\.org\\'"))
         (seq-count (lambda (dir) (file-exists-p (expand-file-name "SKILL.org" dir)))
                    (seq-filter #'file-directory-p (directory-files directory t "\\`[^.]"))))
    0))

(defun lispm-add-skill-directory (directory &optional save)
  "Add DIRECTORY to `lispm-skill-directories', last, so that its skills
replace those of the same name in the others; SAVE it."
  (let ((directory (file-name-as-directory (abbreviate-file-name (expand-file-name directory)))))
    (when (seq-find (lambda (other) (lispm-skills--same-file-p (file-name-as-directory other) directory))
                    lispm-skill-directories)
      (user-error "%s" (lispm-tr "%s is already a directory of skills" directory)))
    (lispm-settings-set 'lispm-skill-directories (append lispm-skill-directories (list directory)) save)))

(defun lispm-remove-skill-directory (directory &optional save)
  "Take DIRECTORY out of `lispm-skill-directories', and SAVE it.
Its skills are left where they are."
  (lispm-settings-set 'lispm-skill-directories
                      (seq-remove (lambda (other)
                                    (lispm-skills--same-file-p (file-name-as-directory other)
                                                               (file-name-as-directory directory)))
                                  lispm-skill-directories)
                      save))

(provide 'lispm-settings)
;;; lispm-settings.el ends here
