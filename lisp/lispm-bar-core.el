;;; lispm-bar-core.el --- LispM bar sources, readiness, and asking logic  -*- lexical-binding: t; -*-

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

;; Group, faces, search sources, backend readiness detection, and
;; evaluation/asking entrypoints backing `lispm-bar'.

;;; Code:

(require 'lispm-core)
(require 'lispm-runtime)
(require 'lispm-transcript)
(require 'lispm-tools)
(require 'lispm-skills)
(require 'lispm-chat)

(declare-function lispm-setup-model "lispm-backend-form" ())

(defgroup lispm-bar nil
  "The LispM bar: one field to find what is installed and to ask LispM."
  :group 'lispm
  :prefix "lispm-bar-")

(defface lispm-bar-selected '((t :inherit highlight :extend t))
  "Face of the line Return acts on."
  :group 'lispm-bar)

(defface lispm-bar-dim '((t :inherit lispm-chat-dim))
  "Face of what the bar says on the side: kinds, details, hints."
  :group 'lispm-bar)

(defface lispm-bar-heading '((t :inherit bold))
  "Face of the headings of the bar opened out."
  :group 'lispm-bar)

(defface lispm-bar-activity '((t :inherit lispm-chat-accent))
  "Face of the line saying what LispM is doing."
  :group 'lispm-bar)

(defface lispm-bar-question '((t :inherit lispm-chat-pending))
  "Face of what LispM asks: a call awaiting approval, a way to go on."
  :group 'lispm-bar)

;;;; LispM itself

;;;###autoload
(defun lispm-ai-chat ()
  "Show the conversation with LispM itself in a chat, all of it.
The bar shows the same conversation, its latest reply."
  (interactive)
  (unless lispm-show-agent-function
    (user-error "Chat display is not available"))
  (funcall lispm-show-agent-function (lispm-system-agent)))

;;;; What the bar finds

(defcustom lispm-bar-results 5
  "How many of the things it finds the bar shows.
Opened out, it shows three times as many."
  :type 'natnum
  :group 'lispm-bar)

(defvar lispm-bar-source-functions (list #'lispm-bar-skills #'lispm-bar-tools)
  "Functions giving the bar what it can find as the user types.
Each is called with no argument, as the bar opens, and returns a list
of plists, one for each thing:
  :kind    what it is: a symbol of `lispm-bar-kinds', as `application'
  :label   what it is, shown beside it, for a kind not there
  :name    its name, as shown
  :icon    a string shown before the name, as an icon, optional
  :detail  a line saying more of it, optional
  :fields  what it is found by, as `lispm-tools-search-score' takes
           them; by default, its name
  :open    a function of no argument opening it, for a thing Return
           opens, as an application: the bar closes, then it runs
  :ask     for another, a sentence for LispM about it, which Return
           adds to what the user asks, as \"Use the skill notes.\"
Things of the sources earlier in the list come first among those the
words match as well.  LispM's own are its skills and tools; another
package may add more, as a desktop its applications.")

(defconst lispm-bar-kinds
  `((application ,(lispm-tr-noop "Application") ,(lispm-tr-noop "Applications"))
    (skill ,(lispm-tr-noop "Skill") ,(lispm-tr-noop "Skills"))
    (tool ,(lispm-tr-noop "Tool") ,(lispm-tr-noop "Tools")))
  "The kinds of things the bar finds: (KIND SINGULAR PLURAL).
The names are English, translated where shown.")

(defun lispm-bar-kind-label (thing &optional plural)
  "What THING is, as the bar shows it beside it; with PLURAL, as a heading."
  (if-let* ((names (alist-get (plist-get thing :kind) lispm-bar-kinds)))
      (lispm-tr (if plural (nth 1 names) (car names)))
    (or (plist-get thing :label) "")))

(defun lispm-bar-skills ()
  "The skills LispM itself can load, for the bar to find.
Return asks LispM, naming the skill chosen."
  (let ((agent (lispm-system-agent)))
    (when (lispm-tools-allowed-p agent "skill")
      (mapcar (lambda (skill)
                (list :kind 'skill
                      :name (or (plist-get skill :title) (plist-get skill :name))
                      :detail (plist-get skill :description)
                      :fields (lispm-skills-search-fields skill)
                      :ask (format "Use the skill %s." (plist-get skill :name))))
              (lispm-skills agent)))))

(defun lispm-bar-tools ()
  "The tools LispM itself may call, offered or deferred, for the bar to find.
Return asks LispM, naming the tool chosen."
  (let ((agent (lispm-system-agent))
        tools)
    (dolist (tool (lispm-tools-registered))
      (let ((name (lispm-tool-name tool)))
        (when (lispm-tools-allowed-p agent name)
          (push (list :kind 'tool
                      :name name
                      :detail (lispm-tools-description tool)
                      :fields (lispm-tools-fields tool)
                      :ask (format "Use the tool %s." name))
                tools))))
    (sort tools (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defvar lispm-bar-things nil
  "What the bar can find, as its sources gave it when it opened.")

(defun lispm-bar-gather ()
  "Ask the sources what the bar can find, and keep it."
  (setq lispm-bar-things
        (mapcan (lambda (source)
                  (condition-case err
                      (copy-sequence (funcall source))
                    (error (message "LispM bar: %s: %s" source (error-message-string err))
                           nil)))
                lispm-bar-source-functions)))

(defun lispm-bar-find (text &optional limit)
  "What the bar finds for TEXT, best first, at most LIMIT things.
The things are plists, as `lispm-bar-source-functions' give them, ranked
as search_tools ranks what it finds; none for TEXT without a word.
While the bar is open, they are what the sources gave as it opened."
  (when-let* ((words (lispm-tools-search-words text)))
    (lispm-tools-rank words (if (and (lispm-bar-open-p) lispm-bar-things)
                                 lispm-bar-things
                               (lispm-bar-gather))
                       (or limit lispm-bar-results)
                       (lambda (thing)
                         (or (plist-get thing :fields) `((3 . ,(plist-get thing :name))))))))

;;;; A model to ask

(defvar lispm-bar-changed-function nil
  "Hook called with no arguments when the bar's asked/eval state changes.")

(defun lispm-bar--notify-changed ()
  "Invoke `lispm-bar-changed-function' if bound."
  (when lispm-bar-changed-function
    (funcall lispm-bar-changed-function)))

(defvar lispm-bar-ready-function #'lispm-bar-model-ready-p
  "Function telling whether LispM has a model to ask.
It is called with the system agent and returns non-nil when it has.
A setup of models of its own sets it, with `lispm-bar-setup-function'.")

(defvar lispm-bar-setup-function #'lispm-bar-setup-model
  "Function helping the user set up a model, called with no argument.
The bar calls it, once closed, when asked something while no model
works (see `lispm-bar-ready-function'); what was asked waits, and is
asked once `lispm-bar-model-ready' is called.")

(defun lispm-bar-model-ready-p (agent)
  "Non-nil if AGENT has a backend to ask, as far as can be told.
Without asking it, that is: a backend of `lispm-backends' (see
`lispm-backend'), whose key, when it reads one from an environment
variable or the secret store, is set there."
  (when-let* ((name (lispm-backend-name agent))
              (backend (lispm-backend name)))
    (let ((variable (plist-get backend :key-env))
          (key (plist-get backend :key)))
      (cond (variable (not (string-empty-p (or (getenv variable) ""))))
            ((and (consp key) (eq (car key) 'secret))
             (ignore-errors (lispm-secret-exists-p (cdr key))))
            (t t)))))

(defun lispm-bar-setup-model ()
  "Open the form setting up a model backend for LispM."
  (require 'lispm-backend-form)
  (lispm-setup-model))

(defvar lispm-bar-open nil
  "Non-nil while the bar is open: the window it was opened from.")

(defvar lispm-bar-ready t
  "Whether LispM had a model to ask when the bar last looked.")

(defvar lispm-bar-waiting nil
  "What the user asked while no model worked, to ask once one does.
\(TEXT . REQUEST): what they typed, and what LispM is to be asked.")

(defun lispm-bar-model-ready ()
  "Tell the bar that a model works now: what waited for one is asked.
A setup of models calls it once it is done.  The bar shows the reply
when it opens next."
  (when-let* ((waiting lispm-bar-waiting))
    (setq lispm-bar-waiting nil)
    (lispm-bar-ask (cdr waiting) (car waiting))
    (unless lispm-bar-open
      (message "%s" (lispm-tr "LispM is answering “%s”: %s shows the reply"
                              (lispm-oneline (car waiting) 40)
                              (substitute-command-keys "\\[lispm-ai]"))))))

(defun lispm-bar--settings-changed ()
  "Ask what waited for a model once a setting change makes one ready."
  (when (and lispm-bar-waiting (lispm-bar-ready-p))
    (lispm-bar-model-ready)))

(add-hook 'lispm-settings-change-hook #'lispm-bar--settings-changed)

;;;; Asking

(defvar lispm-bar-asked nil
  "What was last asked from the bar: (TEXT . ENTRY-ID), or nil.
TEXT is what the user typed; ENTRY-ID the id of the input it is in
the system agent's inbox, then of its turn.")

(defvar lispm-bar-entrypoint nil
  "Symbol naming the UI surface that called `lispm-bar-ask' most recently.
For example, `bar' when asked from the LispM bar field, or
`genera-listener' when asked from `Dynamic Lisp Listener 1'.")

(defvar lispm-bar-reply nil
  "The reply of the turn that answered what was last asked, once it ended.")

(defvar lispm-bar-evaluated nil
  "What was last evaluated as Emacs Lisp from the bar: (TEXT . OUTPUT), or nil.")

(defun lispm-bar-eval (text)
  "Evaluate Emacs Lisp form TEXT and record its formatted output for the bar."
  (let ((output
         (condition-case err
             (let ((val (eval (car (read-from-string text)) t)))
               (format "=> %S" val))
           (error
            (format "error: %s" (error-message-string err))))))
    (setq lispm-bar-evaluated (cons text output)
          lispm-bar-asked nil
          lispm-bar-reply nil)
    (lispm-bar--notify-changed)
    output))

(defun lispm-bar-ask (request &optional text)
  "Ask LispM itself REQUEST; the bar shows its reply.
TEXT is what the user typed, if REQUEST says more; the bar shows it."
  (let* ((agent (lispm-system-agent))
         (asked (cons (or text request) nil)))
    (setq lispm-bar-asked asked
          lispm-bar-reply nil
          lispm-bar-evaluated nil)
    (lispm-tell agent request
                :callback (lambda (reply &rest _)
                            (when (eq lispm-bar-asked asked)
                              (setq lispm-bar-reply reply)
                              (lispm-bar--notify-changed))))
    (setcdr asked (plist-get (car (last (lispm-agent-inbox agent))) :id))
    (lispm-bar--notify-changed)
    agent))

(defun lispm-bar-asked-state (agent)
  "How far AGENT is with what was last asked.
Nil if nothing was, else `queued', `working' or `done'."
  (when-let* ((asked lispm-bar-asked))
    (cond ((seq-find (lambda (entry) (equal (plist-get entry :id) (cdr asked)))
                     (lispm-agent-inbox agent))
           'queued)
          ((equal (plist-get (lispm-agent-entry agent) :id) (cdr asked)) 'working)
          (lispm-bar-reply 'done))))

(defun lispm-bar-open-p ()
  "Non-nil while the bar is open."
  (and lispm-bar-open t))

(defun lispm-bar-ready-p ()
  "Non-nil if LispM has a model to ask, see `lispm-bar-ready-function'."
  (setq lispm-bar-ready (and (funcall lispm-bar-ready-function (lispm-system-agent)) t)))

(provide 'lispm-bar-core)

;;; lispm-bar-core.el ends here
