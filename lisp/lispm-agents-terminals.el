;;; lispm-agents-terminals.el --- Agent terminal launching, directory picker, and live status  -*- lexical-binding: t; -*-

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

;; Launching external agent terminals, modal and minibuffer directory
;; selection, project root classification, status hooks, and icons.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-ui-ask)
(require 'lispm-ui-form)
(require 'lispm-finder nil t)
(require 'lispm-agents-core)
(require 'lispm-agents-sessions)

(declare-function project-root "project" (project))
(declare-function project-known-project-roots "project" ())
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(defvar project-current-directory-override)
(defvar lispm-agents-work-directory)
(defvar lispm-finder--directory)
(defvar lispm-finder--had-focus)

;;;; Terminals (effects)


(defun lispm-agents-running-p (buffer)
  "Return non-nil when the agent of BUFFER runs: a terminal whose process
lives, whatever its program does, or an agent inside Emacs that works or
waits for its user, see `lispm-agents-buffer-status'.  The buffer of an
agent inside Emacs lives on while the agent idles between turns: that
agent is alive, see `lispm-agents-alive-p', not running."
  (and (lispm-agents-alive-p buffer)
       (or (not (lispm-agents-emacs-buffer-agent buffer))
           (memq (plist-get (lispm-agents-buffer-status buffer) :state)
                 '(working blocked error)))
       t))

(defvar-local lispm-agents-buffer-session nil
  "Metadata last read for this terminal's session, without rereading files.")
(put 'lispm-agents-buffer-session 'permanent-local t)

(defvar-local lispm-agents-buffer-title nil
  "Human-readable title of the session this agent terminal runs.
Tab labels show it instead of the terminal's machine-oriented buffer name.")
(put 'lispm-agents-buffer-title 'permanent-local t)

(defconst lispm-agents-session-marker-regexp
  (concat "\\`\\(CLAUDECODE\\|CLAUDE_PID\\|CLAUDE_EFFORT"
          "\\|CLAUDE_CODE_[A-Z0-9_]*\\|CLAUDE_AGENT_SDK_[A-Z0-9_]*"
          "\\|CLAUDE_PREVIEW_[A-Z0-9_]*\\)=")
  "Environment prefixes associated with an enclosing Claude Code session.
An Emacs started from inside Claude Code inherits them; a Claude launched with
them believes it is that session's child, disables its own transcript and
reuses the parent's session id and messaging socket.  The context-window
configuration exception is in `lispm-agents-independent-environment'.")

(defun lispm-agents-independent-environment (environment)
  "Return ENVIRONMENT without enclosing Claude Code session markers.
Agents and shells started from Emacs are independent top-level sessions.
Keep CLAUDE_CODE_MAX_CONTEXT_TOKENS: it configures a custom model's context
window rather than tying the child to its parent's session.  ENVIRONMENT
is not modified."
  (seq-remove (lambda (entry)
                (and (string-match-p lispm-agents-session-marker-regexp entry)
                     (not (string-prefix-p "CLAUDE_CODE_MAX_CONTEXT_TOKENS=" entry))))
              environment))

(defun lispm-agents-launch (agent directory name args &optional id title)
  "Run AGENT with ARGS in DIRECTORY inside the terminal NAME; return it.
The terminal is what `lispm-agents-terminal-make' makes.
The terminal records the session ID and TITLE for labels and lists.
The agent runs as an independent session whatever environment Emacs itself
inherited, and with its :context servers showing it Emacs's state and its
:arguments, see `lispm-agents-define'.  A running terminal with the
same NAME is returned instead of restarted."
  (let ((props (lispm-agents-check-available agent)))
    (unless (file-directory-p directory)
      (user-error (lispm-tr "Working directory no longer exists: %s") directory))
    (let* ((default-directory (file-name-as-directory directory))
           (running (lispm-agents-running-p (get-buffer (format "*%s*" name))))
           ;; A new agent gets servers showing it Emacs's state; a running
           ;; terminal returned as is keeps those it has.
           (servers (unless running
                      (when-let* ((context (plist-get props :context)))
                        (delq nil (ensure-list
                                   (if (memq (cdr (func-arity context)) '(0 1))
                                       (funcall context default-directory)
                                     (funcall context default-directory id)))))))
           (process-environment (append (seq-mapcat #'lispm-agents-context-environment servers)
                                        (lispm-agents-independent-environment process-environment)))
           (buffer (apply #'lispm-agents-terminal-make name (plist-get props :program)
                          (append (seq-mapcat #'lispm-agents-context-arguments servers)
                                  (when-let* ((arguments (plist-get props :arguments)))
                                    (funcall arguments default-directory))
                                  args))))
      (dolist (server servers)
        (lispm-agents-context-bind server buffer))
      (with-current-buffer buffer
        (setq lispm-agents-buffer-agent agent
              lispm-agents-buffer-session-id (if running
                                                 (or id lispm-agents-buffer-session-id)
                                               id)
              lispm-agents-buffer-title (if running (or title lispm-agents-buffer-title) title)
              lispm-agents-buffer-launched (if running lispm-agents-buffer-launched
                                            (float-time)))
        (unless running
          (setq lispm-agents-buffer-session nil)
          (kill-local-variable 'lispm-agents--claimed-at)
          (lispm-agents-session-opened agent lispm-agents-buffer-session-id
                                       lispm-agents-buffer-launched)))
      ;; Agents record their sessions as they go, so closing the terminal
      ;; loses nothing a resume cannot restore: kill it without asking.
      (when-let* ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil))
      (lispm-agents-roster-notify buffer '(agents))
      buffer)))

(defun lispm-agents-buffer-label (buffer)
  "Return the tab label of BUFFER when it is an agent terminal, else nil."
  (when (lispm-agents-buffer-p buffer)
    (or (buffer-local-value 'lispm-agents-buffer-title buffer)
        (string-trim (buffer-name buffer) "\\*" "\\*"))))

(defconst lispm-agents-claim-slack 5
  "Seconds a session may seem to start before its terminal did.
The terminal's launch is stamped by Emacs and the session's start by the
agent; the slack absorbs their clocks' rounding.")

(defun lispm-agents-claim-sessions (sessions)
  "Tie running agent terminals that do not know their session to SESSIONS.
Codex assigns its session id itself, so a terminal started fresh learns it
from the transcripts: its session is the earliest one of its agent that
began in the terminal's directory at or after its launch and that no other
terminal runs.  Terminals claim in launch order, so two started in one
directory take the sessions in the order they began."
  (let* ((running (lispm-agents-live-buffers))
         (claimed (delq nil (mapcar (lambda (buffer)
                                      (buffer-local-value 'lispm-agents-buffer-session-id buffer))
                                    running)))
         (candidates (sort (seq-filter (lambda (session)
                                         (and (plist-get session :started)
                                              (not (plist-get session :parent))))
                                       sessions)
                           (lambda (a b) (< (plist-get a :started) (plist-get b :started))))))
    (dolist (buffer (sort (seq-filter
                           (lambda (buffer)
                             (and (null (buffer-local-value 'lispm-agents-buffer-session-id buffer))
                                  (buffer-local-value 'lispm-agents-buffer-launched buffer)))
                           running)
                          (lambda (a b) (< (buffer-local-value 'lispm-agents-buffer-launched a)
                                           (buffer-local-value 'lispm-agents-buffer-launched b)))))
      (with-current-buffer buffer
        (let ((directory (directory-file-name (expand-file-name default-directory)))
              (agent (lispm-agents-of-buffer buffer)))
          (when-let* ((session
                       (seq-find
                        (lambda (session)
                          (and (eq (plist-get session :agent) agent)
                               (not (member (plist-get session :id) claimed))
                               (>= (plist-get session :started)
                                   (- lispm-agents-buffer-launched lispm-agents-claim-slack))
                               (equal (directory-file-name
                                       (expand-file-name (plist-get session :cwd)))
                                      directory)))
                        candidates)))
            (setq lispm-agents-buffer-session-id (plist-get session :id)
                  lispm-agents-buffer-session session)
            (lispm-agents-session-opened agent lispm-agents-buffer-session-id
                                         lispm-agents-buffer-launched)
            (lispm-agents-roster-notify buffer '(agents key parent root))
            (push (plist-get session :id) claimed)))))))

(defun lispm-agents-opened-buffer (agent buffer time &optional id)
  "Record custom AGENT BUFFER opened at TIME, optionally with session ID.
Preserve an opening the callback already recorded while creating BUFFER."
  (with-current-buffer buffer
    (let* ((id (or id lispm-agents-buffer-session-id))
           (recorded (and id (gethash (lispm-agents-session-key (list :agent agent :id id))
                                     (lispm-agents-opened-times)))))
      (setq lispm-agents-buffer-launched
            (max time (or lispm-agents-buffer-launched 0) (or recorded 0)))
      (unless (equal recorded lispm-agents-buffer-launched)
        (lispm-agents-session-opened agent id lispm-agents-buffer-launched))))
  buffer)

(defvar-local lispm-agents--claimed-at nil
  "When this terminal last had the sessions listed to learn its own.")
(put 'lispm-agents--claimed-at 'permanent-local t)

(defun lispm-agents-terminal-session-id (terminal)
  "Return the id of the session the agent TERMINAL runs, or nil if unknown.
A terminal started fresh for an agent that assigns its own ids learns it
once the agent recorded the session, from a list of the sessions read a
while ago, see `lispm-agents-claim-sessions'; one is made at most every
ten seconds, or once a session reader has newer sessions."
  (with-current-buffer terminal
    (unless (or lispm-agents-buffer-session-id
                (and lispm-agents--claimed-at (< (- (float-time) lispm-agents--claimed-at) 10)))
      (setq lispm-agents--claimed-at (float-time))
      (lispm-agents-sessions-list t))
    lispm-agents-buffer-session-id))

(defun lispm-agents--claim-again ()
  "Let the terminals not knowing their session look at once when next
asked, see `lispm-agents-terminal-session-id': a session reader has
newer sessions.  This is on `lispm-agents-sessions-arrived-hook'."
  (dolist (buffer (buffer-list))
    (when (and (buffer-local-value 'lispm-agents--claimed-at buffer)
               (not (buffer-local-value 'lispm-agents-buffer-session-id buffer)))
      (with-current-buffer buffer
        (setq lispm-agents--claimed-at nil)))))

(add-hook 'lispm-agents-sessions-arrived-hook #'lispm-agents--claim-again)

(defun lispm-agents-refresh-buffer-titles (sessions)
  "Update the titles of running agent terminals from SESSIONS.
Terminals first claim their sessions, see
`lispm-agents-claim-sessions'.  A new session gains its real title
once its first prompt is recorded, and a renamed session follows its new name."
  (lispm-agents-claim-sessions sessions)
  (let ((known (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (list (plist-get session :agent) (plist-get session :id)) session known))
    (dolist (buffer (lispm-agents-live-buffers))
      (with-current-buffer buffer
        (when-let* ((session (and lispm-agents-buffer-session-id
                                  (gethash (list (lispm-agents-of-buffer buffer)
                                                 lispm-agents-buffer-session-id) known))))
          (unless (equal session lispm-agents-buffer-session)
            (setq lispm-agents-buffer-session session)
            (lispm-agents-roster-notify buffer '(name parent root)))
          (when-let* ((title (plist-get session :title)))
            (setq lispm-agents-buffer-title title)))))))

(defun lispm-agents-show (buffer)
  "Show the agent's BUFFER and select it, as `lispm-agents-show-function'
does; who lists the sessions shows them anew, so the agent appears among
those running."
  (funcall lispm-agents-show-function buffer)
  (run-hooks 'lispm-agents-sessions-changed-hook)
  buffer)

(defun lispm-agents--splash-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a welcome, desktop or system-agent buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (or (and (not buffer-file-name)
               (member (buffer-name buffer)
                       '("*GNU Emacs*" "*scratch*"
                         "*Desktop*" " *Desktop*" " *Desktop Background*"
                         "*Setup Assistant*" " *Setup Assistant*" " *LispM Setup Assistant*"
                         "*LispM Guide*" " *LispM Guide*" " *LispM Emacs*"
                         "*Agents*")))
          (let ((agent (or (bound-and-true-p lispm-chat-agent)
                           (bound-and-true-p lispm-session-agent))))
            (and agent (fboundp 'lispm-system-agent-p) (lispm-system-agent-p agent)))))))

(defvar lispm-agents-default-directory-function nil
  "Optional function returning the default directory for a new session or shell.
When non-nil and returning a directory string, `lispm-agents-default-directory'
uses it before falling back to the main window's buffer.")

(defun lispm-agents-project-directory ()
  "Return the current project root, or `default-directory' outside projects.
Welcome and surface buffers resolve to the home directory."
  (cond
   ((and (bound-and-true-p lispm-agents-work-directory)
         (stringp lispm-agents-work-directory)
         (file-directory-p lispm-agents-work-directory))
    (file-name-as-directory (expand-file-name lispm-agents-work-directory)))
   ((and (bound-and-true-p lispm-finder--directory)
         (stringp lispm-finder--directory)
         (file-directory-p lispm-finder--directory))
    (file-name-as-directory (expand-file-name lispm-finder--directory)))
   ((lispm-agents--splash-buffer-p)
    (file-name-as-directory (expand-file-name "~")))
   ((if-let* ((project (project-current)))
        (project-root project)
      default-directory))))

(defun lispm-agents-default-directory ()
  "Return where a new session or shell starts: the project root of the main
window's buffer, see `lispm-agents-main-window-function'."
  (or (let ((dir (and lispm-agents-default-directory-function
                      (funcall lispm-agents-default-directory-function))))
        (and (stringp dir) (not (string-empty-p dir))
             (file-name-as-directory (expand-file-name dir))))
      (let* ((selected (selected-window))
             (main (funcall lispm-agents-main-window-function))
             (window (if (with-selected-window selected
                           (or (and (bound-and-true-p lispm-agents-work-directory)
                                    (stringp lispm-agents-work-directory))
                               (and (bound-and-true-p lispm-finder--directory)
                                    (stringp lispm-finder--directory))))
                         selected
                       main)))
        (with-selected-window window
          (if (and (lispm-agents--splash-buffer-p)
                   (bound-and-true-p lispm-finder--had-focus)
                   (get-buffer " *Finder*")
                   (with-current-buffer " *Finder*"
                     (and (bound-and-true-p lispm-finder--directory)
                          (stringp lispm-finder--directory)
                          (file-directory-p lispm-finder--directory))))
              (with-current-buffer " *Finder*"
                (file-name-as-directory (expand-file-name lispm-finder--directory)))
            (lispm-agents-project-directory))))))

(defvar lispm-agents-read-directory-function nil
  "Optional function overriding `lispm-agents-read-directory'.
When non-nil, called with (&optional AGENT DEFAULT) and returns the
chosen directory.")

(defcustom lispm-agents-max-known-directories 16
  "Maximum number of directories offered in the New Agent Project menu."
  :type 'natnum
  :group 'lispm-agents)

(defun lispm-agents--ignored-project-directory-p (directory)
  "Return non-nil when DIRECTORY is a temporary or package-cache path."
  (let ((expanded (file-name-as-directory (expand-file-name directory)))
        (temp (and (boundp 'temporary-file-directory)
                   (stringp temporary-file-directory)
                   (file-name-as-directory (expand-file-name temporary-file-directory)))))
    (or (string-prefix-p "/tmp/" expanded)
        (string-prefix-p "/private/tmp/" expanded)
        (string-prefix-p "/var/folders/" expanded)
        (string-prefix-p "/private/var/folders/" expanded)
        (and temp (string-prefix-p temp expanded))
        (string-search "/elpa/" expanded))))

(defun lispm-agents-known-directories (&optional default)
  "Return abbreviated existing project and session directories, DEFAULT first."
  (let ((dirs nil)
        (cached-sessions nil))
    (catch 'done
      (cl-flet ((add (dir &optional allow-ignored)
                  (when (>= (length dirs) lispm-agents-max-known-directories)
                    (throw 'done nil))
                  (when (and (stringp dir)
                             (not (string-empty-p dir)))
                    (let ((expanded (expand-file-name dir)))
                      (when (and (file-directory-p expanded)
                                 (or allow-ignored
                                     (not (lispm-agents--ignored-project-directory-p expanded))))
                        (let ((abbr (abbreviate-file-name (file-name-as-directory expanded))))
                          (unless (member abbr dirs)
                            (push abbr dirs)
                            (when (>= (length dirs) lispm-agents-max-known-directories)
                              (throw 'done nil)))))))))
        (add default t)
        (dolist (buffer (lispm-agents-live-buffers))
          (when-let* ((cwd (buffer-local-value 'default-directory buffer))
                      (root (lispm-agents-project-root cwd))
                      ((stringp root)))
            (add root)))
        (dolist (entry lispm-agents-session-readers)
          (let ((res (ignore-errors (funcall (cdr entry)))))
            (when (listp res)
              (dolist (session res)
                (push session cached-sessions)))))
        (maphash (lambda (_file entry)
                   (when-let* ((session (cdr entry)))
                     (push session cached-sessions)))
                 lispm-agents-session-cache)
        (dolist (session (sort cached-sessions
                               (lambda (a b)
                                 (> (or (plist-get a :time) 0)
                                    (or (plist-get b :time) 0)))))
          (when-let* ((cwd (plist-get session :cwd))
                      (root (lispm-agents-project-root cwd))
                      ((stringp root)))
            (add root)))
        (when (fboundp 'project-known-project-roots)
          (dolist (root (ignore-errors (project-known-project-roots)))
            (add root)))
        (add "~" t)))
    (nreverse dirs)))

(defun lispm-agents--directory-choice-label (directory)
  "Return a display label for DIRECTORY in a project choice menu."
  (let* ((trimmed (directory-file-name directory))
         (home (directory-file-name (expand-file-name "~")))
         (expanded (directory-file-name (expand-file-name directory)))
         (name (file-name-nondirectory trimmed)))
    (cond ((string= expanded home)
           (format "%s (%s)" (lispm-ui-tr "Home") directory))
          ((and (not (string-empty-p name)) (not (equal name trimmed)))
           (format "%s (%s)" name trimmed))
          (t directory))))

(defun lispm-agents--directory-resolver (base-dir)
  "Return a function resolving a user input string relative to BASE-DIR."
  (lambda (val)
    (when (and (stringp val) (not (string-empty-p (string-trim val))))
      (let ((raw (expand-file-name val base-dir))
            (trimmed (expand-file-name (string-trim val) base-dir)))
        (if (file-directory-p raw) raw trimmed)))))

(defun lispm-agents--directory-validator (resolve)
  "Return a field validation function composed with RESOLVE."
  (lambda (val)
    (let ((expanded (funcall resolve val)))
      (unless (and expanded (file-directory-p expanded))
        (lispm-tr "Working directory no longer exists: %s" (or val ""))))))

(defun lispm-agents--choose-directory (start)
  "Prompt for a directory starting at START, preserving the selected modal frame."
  (let ((modal-frame (selected-frame)))
    (condition-case nil
        (unwind-protect
            (lispm-ui-choose-file
             :title (lispm-tr "Directory")
             :directory (if (file-directory-p start)
                            (file-name-as-directory start)
                          (file-name-as-directory (expand-file-name "~")))
             :predicate #'file-directory-p)
          (when (frame-live-p modal-frame)
            (select-frame-set-input-focus modal-frame)))
      (quit nil))))

(defun lispm-agents--setup-modal-form ()
  "Configure the current `lispm-ui-form' buffer as a modal dialog."
  (when (boundp 'lispm-ui-form--props)
    (set 'lispm-ui-form--props
         (plist-put (symbol-value 'lispm-ui-form--props) :modal t)))
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key [remap keyboard-quit] #'lispm-ui-form-cancel)
  (local-set-key (kbd "ESC") #'lispm-ui-form-cancel)
  (local-set-key (kbd "<escape>") #'lispm-ui-form-cancel))

(defun lispm-agents--directory-browse-buttons (buffer-fn resolve base-dir initial choice-pairs)
  "Return the Browse buttons alist for the directory picker modal form."
  `((,(lispm-ui-tr "Choose…") .
     ,(lambda ()
        (let ((buffer (funcall buffer-fn)))
          (when (buffer-live-p buffer)
            (let* ((current (with-current-buffer buffer
                              (or (lispm-ui-form-value :directory) initial)))
                   (start (or (funcall resolve current)
                              (expand-file-name initial base-dir)))
                   (chosen (lispm-agents--choose-directory start)))
              (when (and chosen (buffer-live-p buffer))
                (let ((abbr (abbreviate-file-name
                             (file-name-as-directory (expand-file-name chosen base-dir)))))
                  (with-current-buffer buffer
                    (lispm-ui-form-set
                     :directory abbr
                     :preset (and (assoc abbr choice-pairs) abbr))))))))))))

(defun lispm-agents--directory-form-fields (initial validate choice-pairs browse-buttons buffer-fn)
  "Build the field specifications for `lispm-agents--read-directory-form'."
  `((:directory :type text :label ,(lispm-tr "Directory")
                :value ,initial :required t :submit t
                :validate ,validate)
    ,@(when choice-pairs
        `((:preset :type choice :label ,(lispm-tr "Project")
                   :value ,(and (assoc initial choice-pairs) initial)
                   :choices ,choice-pairs
                   ,@(when (bound-and-true-p lispm-ui-form-choice-buttons-supported)
                       `(:buttons ,browse-buttons :buttons-key :browse))
                   :on-change ,(lambda (val)
                                 (let ((buffer (funcall buffer-fn)))
                                   (when (and val (buffer-live-p buffer))
                                     (with-current-buffer buffer
                                       (lispm-ui-form-set :directory val))))))))
    ,@(when (or (not choice-pairs)
                (not (bound-and-true-p lispm-ui-form-choice-buttons-supported)))
        `((:browse :type buttons :buttons ,browse-buttons)))))

(defun lispm-agents--read-directory-form (title initial)
  "Ask for a session directory in a modal form titled TITLE.
INITIAL is pre-filled in the directory field."
  (let* ((base-dir (file-name-as-directory (expand-file-name default-directory)))
         (resolve (lispm-agents--directory-resolver base-dir))
         (validate (lispm-agents--directory-validator resolve))
         (choice-pairs (mapcar (lambda (dir)
                                 (cons dir (lispm-agents--directory-choice-label dir)))
                               (lispm-agents-known-directories initial)))
         (submitted nil)
         (closing nil)
         (result nil)
         (buffer nil)
         (cleanup nil)
         (depth (recursion-depth))
         (exit-with (lambda (ok &optional val)
                      (when (<= (recursion-depth) (1+ depth))
                        (setq closing t submitted ok result val)
                        (when (= (recursion-depth) (1+ depth))
                          (exit-recursive-edit)))))
         (buffer-fn (lambda () buffer))
         (browse-buttons (lispm-agents--directory-browse-buttons
                          buffer-fn resolve base-dir initial choice-pairs))
         (fields (lispm-agents--directory-form-fields
                  initial validate choice-pairs browse-buttons buffer-fn)))
    (setq buffer
          (lispm-ui-form
           title fields
           :on-submit (lambda (values)
                        (funcall exit-with t (or (alist-get :directory values) initial)))
           :on-cancel (apply-partially exit-with nil)
           :setup #'lispm-agents--setup-modal-form))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook (lambda () (unless closing (funcall exit-with nil))) nil t))
    (unwind-protect
        (progn
          (setq cleanup (funcall lispm-ui-form-modal-show-function buffer))
          (with-current-buffer buffer
            (if (fboundp 'lispm-ui-form-first-field)
                (lispm-ui-form-first-field)
              (lispm-ui-form--first-field)))
          (let ((lispm-ui-in-gui-action nil)
                (lispm-ui--command-from-mouse nil))
            (recursive-edit)))
      (setq closing t)
      (when (functionp cleanup) (ignore-errors (funcall cleanup)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (unless submitted (signal 'quit nil))
    (file-name-as-directory (or (funcall resolve result)
                                (expand-file-name result base-dir)))))

(defun lispm-agents-read-directory (&optional agent default)
  "Ask for the working directory of a new session of AGENT.
DEFAULT, by default `lispm-agents-default-directory', is pre-filled in
the prompt.  A GUI action opens a modal form with a text field, known
projects and a button to browse for a folder; keyboard commands read the
directory in the minibuffer."
  (when (and agent (lispm-agents-props agent))
    (lispm-agents-check-available agent))
  (if lispm-agents-read-directory-function
      (file-name-as-directory
       (expand-file-name (funcall lispm-agents-read-directory-function agent default)))
    (let* ((initial (abbreviate-file-name
                     (file-name-as-directory
                      (expand-file-name (or default (lispm-agents-default-directory))))))
           (title (if-let* ((name (and agent (plist-get (lispm-agents-props agent) :name))))
                      (lispm-tr "New %s session" name)
                    (lispm-tr "New session"))))
      (if (lispm-ui-gui-action-p)
          (lispm-agents--read-directory-form title initial)
        (let ((lispm-ui-in-gui-action nil)
              (lispm-ui--command-from-mouse nil))
          (file-name-as-directory
           (expand-file-name
            (read-directory-name (lispm-tr "New session in: ") initial initial t))))))))

(defun lispm-agents-fresh-name (agent directory)
  "Return an unused terminal name for a new AGENT session in DIRECTORY."
  (let* ((base (format "%s:%s" agent
                       (file-name-nondirectory (directory-file-name directory))))
         (name base)
         (n 1))
    (while (lispm-agents-running-p (get-buffer (concat "*" name "*")))
      (setq n (1+ n)
            name (format "%s<%d>" base n)))
    name))


;;;; Projects, running sessions and their marks

(defun lispm-agents--directory-key (directory)
  "Return DIRECTORY expanded, without trailing slash, abbreviated for display."
  (abbreviate-file-name (directory-file-name (expand-file-name directory))))

(defun lispm-agents--chat-directory-p (directory)
  "Return non-nil when DIRECTORY holds project-less chat sessions.
The home directory itself is a chat location; so is anything under an
agent's :chats directories, see `lispm-agents-define'."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (or (string= dir (directory-file-name (expand-file-name "~")))
        (seq-some (lambda (entry)
                    (seq-some (lambda (chats)
                                (string-prefix-p (file-name-as-directory (expand-file-name chats))
                                                 (file-name-as-directory dir)))
                              (plist-get (cdr entry) :chats)))
                  lispm-agents-defined))))

(defun lispm-agents--worktree (directory)
  "Return where DIRECTORY belongs when it is in one of an agent's worktrees,
as that agent's :worktree says, see `lispm-agents-define'; else nil."
  (seq-some (lambda (entry)
              (when-let* ((worktree (plist-get (cdr entry) :worktree)))
                (funcall worktree directory)))
            lispm-agents-defined))

(defun lispm-agents--git-main-root (directory)
  "Return the main checkout root of the git repository containing DIRECTORY.
A linked worktree's `.git' is a file naming MAIN/.git/worktrees/NAME; follow
it to MAIN.  Roots are absolute, including when Emacs abbreviates a path
under the home directory.  Return nil outside any existing repository."
  (when-let* (((file-directory-p directory))
              (root (locate-dominating-file directory ".git")))
    (let ((dotgit (expand-file-name ".git" root)))
      (or (and (file-regular-p dotgit)
               (with-temp-buffer
                 (insert-file-contents dotgit)
                 (when (re-search-forward "^gitdir: \\(.+?\\)/\\.git/worktrees/" nil t)
                   (match-string 1))))
          (expand-file-name root)))))

(defun lispm-agents-project-root (directory)
  "Return the project root of working DIRECTORY, or `chats' for chat sessions.
An agent's worktree known to belong to a repository resolves to it, git
checkouts and their subdirectories to the main checkout root; other
directories stand alone."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (with-memoization (gethash dir lispm-agents-root-cache)
      (if (lispm-agents--chat-directory-p dir)
          'chats
        (let ((base (or (plist-get (lispm-agents--worktree dir) :root) dir)))
          (directory-file-name (or (lispm-agents--git-main-root base) base)))))))

(defun lispm-agents-location-label (directory root)
  "Return where DIRECTORY sits inside project ROOT, or nil at the root itself.
Worktrees are named by their worktree, subdirectories by their relative path."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (cond ((or (eq root 'chats) (string-equal-ignore-case dir root)) nil)
          ((plist-get (lispm-agents--worktree dir) :label))
          ((string-prefix-p (file-name-as-directory root) (file-name-as-directory dir))
           (file-relative-name dir root))
          (t (abbreviate-file-name dir)))))

(defun lispm-agents-resolve-roots (directories)
  "Return an alist mapping each of DIRECTORIES to its project root.
Roots come from `lispm-agents-project-root'.  An agent's worktree
known only by its repository's name, as a Codex worktree that no longer
exists and so cannot be followed through git, joins the known project
whose directory name is that name, when there is one."
  (let* ((roots (mapcar (lambda (dir) (cons dir (lispm-agents-project-root dir)))
                        directories))
         (by-name (make-hash-table :test #'equal)))
    (pcase-dolist (`(,_ . ,root) roots)
      (when (and (stringp root) (not (plist-get (lispm-agents--worktree root) :name)))
        (unless (gethash (file-name-nondirectory root) by-name)
          (puthash (file-name-nondirectory root) root by-name))))
    (mapcar (pcase-lambda (`(,dir . ,root))
              (cons dir (or (and (stringp root)
                                 (when-let* ((name (plist-get (lispm-agents--worktree root) :name)))
                                   (gethash name by-name)))
                            root)))
            roots)))

(defun lispm-agents-group-by-directory (sessions running)
  "Group SESSIONS and RUNNING terminals by project.
A project is the root from `lispm-agents-resolve-roots', so worktrees
and subdirectories join their repository and chat locations share one `chats'
group.  Return a list of plists
  (:key KEY :dir DIR :chats CHATS :running BUFFERS :waiting WAITING
   :sessions SESSIONS :time T :opened OPENED)
where KEY folds case (macOS paths are case-insensitive), DIR is the root for
display, WAITING non-nil when one of BUFFERS waits for the user, see
`lispm-agents-buffer-waiting-p', T is the latest activity, and OPENED is
the latest launch or resume.  Projects sort by OPENED, with chats last.
Descendants join their root session's project even across directories.
SESSIONS keep their order inside each group."
  (let* ((directory-of (lambda (item)
                         (if (bufferp item)
                             (buffer-local-value 'default-directory item)
                           (plist-get item :cwd))))
         (roots (lispm-agents-resolve-roots
                 (delete-dups (mapcar directory-of (append running sessions)))))
         (session-roots (make-hash-table :test #'equal))
         (groups (make-hash-table :test #'equal))
         (order nil))
    (cl-labels ((assign (tree root)
                  (puthash (lispm-agents-session-key (car tree)) root session-roots)
                  (dolist (child (cdr tree)) (assign child root))))
      (dolist (tree (lispm-agents-session-tree sessions))
        (assign tree (cdr (assoc (plist-get (car tree) :cwd) roots)))))
    (cl-flet ((group (item)
                (let* ((session (if (bufferp item)
                                    (list :agent (lispm-agents-of-buffer item)
                                          :id (buffer-local-value 'lispm-agents-buffer-session-id item))
                                  item))
                       (root (or (gethash (lispm-agents-session-key session) session-roots)
                                 (cdr (assoc (funcall directory-of item) roots))))
                       (key (if (eq root 'chats) 'chats (downcase root))))
                  (or (gethash key groups)
                      (progn
                        (push key order)
                        (puthash key
                                 (list :key key
                                       :dir (if (eq root 'chats) "~"
                                              (lispm-agents--directory-key root))
                                       :chats (eq root 'chats)
                                       :running nil :waiting nil :sessions nil :time 0 :opened 0)
                                 groups))))))
      (dolist (buffer running)
        (let ((group (group buffer)))
          (plist-put group :running (cons buffer (plist-get group :running)))
          (plist-put group :opened (max (plist-get group :opened)
                                        (or (buffer-local-value 'lispm-agents-buffer-launched buffer) 0)))
          (when (lispm-agents-buffer-waiting-p buffer)
            (plist-put group :waiting t))))
      (dolist (session sessions)
        (let ((group (group session)))
          (plist-put group :sessions (cons session (plist-get group :sessions)))
          (plist-put group :opened (max (plist-get group :opened)
                                        (lispm-agents-session-opened-time session)))
          (plist-put group :time (max (plist-get group :time)
                                      (plist-get session :time))))))
    (sort (mapcar (lambda (key)
                    (let ((group (gethash key groups)))
                      (plist-put group :running (nreverse (plist-get group :running)))
                      (plist-put group :sessions (nreverse (plist-get group :sessions)))))
                  order)
          (lambda (a b)
            (let ((ca (plist-get a :chats)) (cb (plist-get b :chats)))
              (cond ((not (eq ca cb)) cb)
                    ((= (plist-get a :opened) (plist-get b :opened))
                     (string< (plist-get a :dir) (plist-get b :dir)))
                    (t (> (plist-get a :opened) (plist-get b :opened)))))))))

(defvar lispm-agents-status-functions nil
  "Functions saying what an agent's buffer is doing.
Each is called with a buffer of an agent, see `lispm-agents-buffer-p',
and returns a record (:state STATE :kind KIND :progress PROGRESS :msg
MSG), as a terminal's program reports itself, or nil when it does not
know the buffer; the first to answer does.  STATE is `idle', `working',
`done', `blocked', the agent waiting for its user to answer, or
`error'; KIND, why it is blocked, `permission', `question' or `auth';
PROGRESS a percentage; MSG what the agent says of it.  What is not
known is nil.  Lists of the sessions show the record without changing
their opening order.")

(defun lispm-agents-buffer-status (buffer)
  "Return what the agent of BUFFER is doing, as the first of
`lispm-agents-status-functions' to know BUFFER says, or nil."
  (run-hook-with-args-until-success 'lispm-agents-status-functions buffer))

(defun lispm-agents-buffer-waiting-p (buffer)
  "Return non-nil when the agent of BUFFER waits for its user, see
`lispm-agents-buffer-status': blocked on an answer, or failed."
  (and (memq (plist-get (lispm-agents-buffer-status buffer) :state) '(blocked error)) t))

(defun lispm-agents-waiting-buffers ()
  "Return the live agent buffers whose agents wait for their user, the
most urgent first: those blocked on an answer, then those failed, see
`lispm-agents-buffer-status'; among a state, the most recently used
first."
  (let ((buffers (lispm-agents-running-buffers)))
    (seq-mapcat (lambda (state)
                  (seq-filter (lambda (buffer)
                                (eq (plist-get (lispm-agents-buffer-status buffer) :state) state))
                              buffers))
                '(blocked error))))

(defun lispm-agents-live-buffers ()
  "Return the agent buffers whose agents can be talked to, see
`lispm-agents-alive-p', most recently used first."
  (seq-filter (lambda (buffer)
                (and (lispm-agents-buffer-p buffer) (lispm-agents-alive-p buffer)))
              (buffer-list)))

(defun lispm-agents-running-buffers ()
  "Return the agent buffers whose agents run, see `lispm-agents-running-p',
most recently used first."
  (seq-filter #'lispm-agents-running-p (lispm-agents-live-buffers)))

(defun lispm-agents-running-ids (buffers)
  "Return the ids of the sessions agent terminal BUFFERS run, where known.
Ids are compared whole: Codex ids are time-ordered, so sessions begun within
about a minute of each other share their leading characters."
  (delq nil (mapcar (lambda (buffer)
                      (buffer-local-value 'lispm-agents-buffer-session-id buffer))
                    buffers)))

(defun lispm-agents-session-buffer (id)
  "Return the live agent buffer of the session with ID, or nil: the one
its agent is talked to in, running or idle, see `lispm-agents-alive-p'."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'lispm-agents-buffer-session-id buffer) id))
            (lispm-agents-live-buffers)))

(defun lispm-agents-image-icon (file &rest args)
  "Return FILE, an image, as a one-line icon of an agent.
FILE is absolute, or relative to `default-directory'.  ARGS are the
nerd-icons keyword arguments: :height scales the icon relative to the
frame's line height, :face and :v-adjust are accepted for the sake of
callers that pass them to every icon function.  Where images cannot be
shown, a feather stands in for the image, a star without nerd-icons."
  (let ((path (expand-file-name file)))
    (if (and (display-images-p) (file-readable-p path))
        (propertize " " 'display
                    (create-image path nil nil
                                  :height (round (* (or (plist-get args :height) 1.0)
                                                    (frame-char-height)))
                                  :ascent 'center))
      (if (require 'nerd-icons nil t)
          (apply #'nerd-icons-mdicon "nf-md-feather" args)
        "✦"))))

(defun lispm-agents--text-icon (agent)
  "Return AGENT's icon on a text terminal: its :text-icon, else the first
letter of its name; a robot's head for no agent known."
  (let ((props (lispm-agents-props agent)))
    (or (plist-get props :text-icon)
        (when-let* ((name (plist-get props :name))
                    ((not (string-empty-p name))))
          (substring name 0 1))
        "◉")))

(defun lispm-agents--icon-spec (agent)
  "Return (FUNCTION ICON FACE) of AGENT's icon, its :icon, see
`lispm-agents-define'; a robot for no agent known."
  (or (plist-get (lispm-agents-props agent) :icon)
      '(nerd-icons-codicon "nf-cod-robot" nil)))

(defun lispm-agents-icon (agent)
  "Return the brand icon of AGENT.
On a text terminal, whose font is seldom a Nerd Font, or without the
package nerd-icons, it is AGENT's :text-icon, see `lispm-agents-define'."
  (pcase-let* ((`(,function ,name ,face) (lispm-agents--icon-spec agent))
               (icon (copy-sequence (if (or (and (not noninteractive) (not (display-graphic-p)))
                                            (not (require 'nerd-icons nil t)))
                                        (lispm-agents--text-icon agent)
                                      (funcall function name)))))
    ;; Layer the colour under the icon's own face so its Nerd Font family
    ;; survives; replacing the face would fall back to an arbitrary CJK font.
    (when face
      (add-face-text-property 0 (length icon) face t icon))
    icon))

(defun lispm-agents-of-buffer (buffer)
  "Return the agent running in agent BUFFER, or nil when it is none.
A terminal knows its agent; an in-Emacs agent recognizes its own."
  (or (lispm-agents-emacs-buffer-agent buffer)
      (lispm-agents-terminal-agent buffer)))


(setq lispm-agents-refresh-buffer-titles-function #'lispm-agents-refresh-buffer-titles)

(provide 'lispm-agents-terminals)

;;; lispm-agents-terminals.el ends here
