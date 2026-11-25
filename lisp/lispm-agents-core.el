;;; lispm-agents-core.el --- External coding agent registry, Eat backend, and keymaps  -*- lexical-binding: t; -*-

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

;; Definition registry (`lispm-agents-define'), Eat terminal primitives,
;; buffer-local identity vars, roster change notifications, and agent
;; keybindings.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-agents-context)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-ui-ask)

(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-term-send-string-as-yank "eat" (terminal args))
(declare-function eat-term-send-string "eat" (terminal string))
(defvar eat-terminal)
(defvar lispm-session-agent)

(defvar lispm-agents-refresh-buffer-titles-function nil
  "Function called with SESSIONS to update live terminal buffer titles.")

(defvar lispm-agents-terminal-make-function #'lispm-agents-eat-make
  "Function running a program in a new terminal: called with NAME,
PROGRAM and its ARGS, it returns the buffer *NAME* it runs in.  By
default an Eat terminal; a program with terminals of its own sets this,
`lispm-agents-terminal-live-p-function',
`lispm-agents-terminal-paste-function',
`lispm-agents-terminal-send-function' and
`lispm-agents-terminal-modes' together.")

(defvar lispm-agents-terminal-live-p-function #'lispm-agents-process-live-p
  "Function of a terminal's buffer returning non-nil while its program
runs.")

(defvar lispm-agents-terminal-paste-function #'lispm-agents-eat-paste
  "Function of a terminal's buffer and a text pasting the text into its
program, as pasted.")

(defvar lispm-agents-terminal-send-function #'lispm-agents-eat-send
  "Function of a terminal's buffer and a text sending the text to its
program, as typed: \"\\r\" is the Return key.")

(defvar lispm-agents-terminal-modes '(eat-mode)
  "Major modes of the terminals agents run in.")

(defvar lispm-agents-show-function #'pop-to-buffer
  "Function showing an agent's buffer and selecting it, called with the
buffer; see `lispm-agents-show'.")

(defvar lispm-agents-main-window-function #'selected-window
  "Function returning the window files are worked on in: a new session
starts at the project of its buffer, and a session's transcript or
directory opens there.")

(defvar lispm-agents-confirm-function #'lispm-ui-confirm
  "Function asking whether to go on with something that cannot be undone,
as deleting a session.  It is called with a question, a detail and the
name of the action, as \"Delete\", and returns non-nil to go on.
By default LispM UI's `lispm-ui-confirm' asks: in the alert of the
interface drawing LispM UI's, see `lispm-ui-alert-function', else with
`yes-or-no-p'.")

(define-obsolete-function-alias 'lispm-agents-yes-or-no-p #'lispm-ui-confirm "0.12.0")


(defvar-local lispm-agents-buffer-agent nil
  "The agent this terminal runs, a key of `lispm-agents-defined'.")
(put 'lispm-agents-buffer-agent 'permanent-local t)

(defun lispm-agents-terminal-agent (buffer)
  "Return the agent the terminal BUFFER runs, or nil when it runs none.
A terminal an agent was started in says so, see `lispm-agents-launch';
any other is taken by its name, \"*AGENT:...*\", as agent terminals are named."
  (and (lispm-agents-terminal-p buffer)
       (let ((agent (or (buffer-local-value 'lispm-agents-buffer-agent buffer)
                        ;; Asked while tab labels are drawn: the match data stays.
                        (save-match-data
                          (and (string-match "\\`\\*\\([a-z]+\\):" (buffer-name buffer))
                               (intern (match-string 1 (buffer-name buffer))))))))
         (and (plist-get (lispm-agents-props agent) :program) agent))))

(defun lispm-agents-buffer-p (buffer)
  "Return non-nil when BUFFER holds an agent session.
That is a terminal an agent runs in, or the buffer of a session of an
agent running inside Emacs."
  (and (buffer-live-p buffer)
       (or (lispm-agents-terminal-agent buffer)
           (lispm-agents-emacs-buffer-agent buffer))
       t))

(defun lispm-agents-alive-p (buffer)
  "Return non-nil when BUFFER holds an agent that can still be talked to:
a terminal whose process lives, or the live buffer of a session of an
agent running inside Emacs.  An alive agent need not be doing anything,
see `lispm-agents-running-p'."
  (if (and buffer (lispm-agents-emacs-buffer-agent buffer))
      (buffer-live-p buffer)
    (and buffer (lispm-agents-terminal-live-p buffer))))


(defvar-local lispm-agents-buffer-session-id nil
  "Id of the session this agent terminal runs, when known.")
(put 'lispm-agents-buffer-session-id 'permanent-local t)


(defvar-local lispm-agents-buffer-launched nil
  "Time, float seconds, this agent terminal was started.")
(put 'lispm-agents-buffer-launched 'permanent-local t)


(defvar lispm-agents-root-cache (make-hash-table :test #'equal)
  "Memoized project roots keyed by working directory.")


;;;; Terminals of Eat

(defun lispm-agents-eat-make (name program &rest args)
  "Run PROGRAM with ARGS in an Eat terminal named *NAME*; return its buffer."
  (unless (require 'eat nil t)
    (user-error (lispm-tr "Agents run in Eat's terminals; install the package eat")))
  (apply #'eat-make name program nil args))

(defun lispm-agents-process-live-p (buffer)
  "Return non-nil while the process of BUFFER runs."
  (when-let* ((process (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (process-live-p process)))

(defun lispm-agents-eat-paste (buffer text)
  "Paste TEXT into the program of the Eat terminal BUFFER, as pasted."
  (with-current-buffer buffer
    (eat-term-send-string-as-yank eat-terminal (list text))))

(defun lispm-agents-eat-send (buffer text)
  "Send TEXT to the program of the Eat terminal BUFFER, as typed."
  (with-current-buffer buffer
    (eat-term-send-string eat-terminal text)))

(defun lispm-agents-terminal-make (name program &rest args)
  "Run PROGRAM with ARGS in a terminal named *NAME*; return its buffer.
The terminal is what `lispm-agents-terminal-make-function' makes."
  (apply lispm-agents-terminal-make-function name program args))

(defun lispm-agents-terminal-live-p (buffer)
  "Return non-nil while the program of terminal BUFFER runs."
  (funcall lispm-agents-terminal-live-p-function buffer))

(defun lispm-agents-terminal-paste (buffer text)
  "Paste TEXT into the program of terminal BUFFER, as pasted."
  (funcall lispm-agents-terminal-paste-function buffer text))

(defun lispm-agents-terminal-send (buffer text)
  "Send TEXT to the program of terminal BUFFER, as typed."
  (funcall lispm-agents-terminal-send-function buffer text))

(defvar lispm-agents-submit-delay 0.3
  "Seconds between pasting a message into an agent and pressing Return.
An agent's composer takes a paste in as it comes; a Return arriving with
it would be a line of the text.")

(defun lispm-agents-terminal-submit (buffer text)
  "Give the agent of terminal BUFFER TEXT as its user's next message.
TEXT is pasted into its composer, then sent with Return; an agent at
work keeps it for when it is done, as a message typed meanwhile."
  (lispm-agents-terminal-paste buffer text)
  (run-at-time lispm-agents-submit-delay nil
               (lambda ()
                 (when (lispm-agents-terminal-live-p buffer)
                   (lispm-agents-terminal-send buffer "\r")))))

(defun lispm-agents-terminal-ready-p (terminal)
  "Return non-nil when the agent of TERMINAL waits for its user's next
message, as its :ready says; nil when it has no :ready, see
`lispm-agents-define'."
  (when-let* ((agent (or (buffer-local-value 'lispm-agents-buffer-agent terminal)
                         (lispm-agents-terminal-agent terminal)))
              (ready (plist-get (lispm-agents-props agent) :ready)))
    (and (lispm-agents-terminal-live-p terminal)
         (funcall ready terminal)
         t)))

(defun lispm-agents-terminal-p (buffer)
  "Return non-nil when BUFFER is a terminal agents run in, its program
running or not; see `lispm-agents-terminal-modes'."
  (and (buffer-live-p buffer)
       (memq (buffer-local-value 'major-mode buffer) lispm-agents-terminal-modes)
       t))



;;;; Agents

(defvar lispm-agents-defined nil
  "The agents, in the order they were defined: (AGENT . PROPERTIES).
See `lispm-agents-define'.")

(defvar-keymap lispm-agents-map
  :doc "Commands for coding agents, to bind under a key, as \\`C-c a'.
Each agent adds the key starting it, see `lispm-agents-define'.")

(defun lispm-agents-define (agent &rest properties)
  "Make AGENT, a symbol, one of the editor's agents, as PROPERTIES say.
Every agent has these:

  :name        its name, \"Claude Code\"
  :icon        (FUNCTION ICON FACE): FUNCTION makes a one-line icon of
               ICON, as `nerd-icons-codicon' or `lispm-agents-image-icon'
               do; FACE, when non-nil, is layered under it
  :text-icon   a string one cell wide standing for the icon on a text
               terminal, in FACE; by default the first letter of :name
  :command     the command starting a session at the current project root
  :key         the key of :command in the keymaps of `lispm-agents-key-maps'
  :sessions    function returning its recorded sessions, as plists
               (:agent :id :cwd :title :started :time :file), :started
               the creation time and :time transcript freshness, both
               float seconds; :started also ties new terminals to ids,
               see `lispm-agents-claim-sessions'; a session another of
               the agent's sessions started, as its subagent's, adds
               :parent, that session's id, and lists of the sessions
               draw it under its parent, see
               `lispm-agents-session-tree'
  :rename      function of a session and a title

and these when they apply:

  :built-in    non-nil for an agent the editor ships with; those come
               first wherever agents are offered
  :files       function of a session returning the files and directories
               recording it, which deleting it moves to the Trash
  :delete      function deleting a session from the agent's own store,
               for an agent whose sessions have no files of their own
  :transcript  function of a session returning a file of its transcript;
               by default, its :file
  :resumable   predicate on a session: whether it can be resumed, which
               every session can without one; one that cannot is opened
               as its transcript instead, see
               `lispm-agents-resume-session'
  :worktree    function of a directory returning nil, or where that
               worktree of the agent's belongs: (:root REPOSITORY :label
               LABEL), or (:name NAME :label LABEL) when only the
               repository's name is known; LABEL is shown as it is,
               in the language spoken
  :chats      directories whose sessions are chats, not project work
  :short-id    function of a session's id returning the short form that
               tells it from others; by default its first eight characters
  :work-details function of a live buffer returning a lazy work plist
                (:turn :pending-calls :recent-calls :files); calls have
                :id, :name, :input-summary and :status.  Unknown fields
                are nil.  Never called by `lispm-agents-roster'

An agent run as a CLI in a terminal has these:

  :program     its executable; while it is not on PATH, the agent takes
               part nowhere
  :new-arguments     function of no argument returning (ID . ARGUMENTS)
                     starting a new session; ID is the session's id when
                     the agent takes one it is given, else nil
  :resume-arguments  function of a session's id returning ARGUMENTS
                     resuming it
  :model-arguments   optional function of a model name returning arguments
                     selecting it; enables resuming with a chosen model
  :arguments   function of a directory returning the arguments of every
               run there
  :context     function of a directory returning a server showing the
               agent Emacs's state, see `lispm-agents-context', a list
               of servers, or nil; one taking a second argument gets
               the session's id too, nil for a fresh session whose
               agent assigns its own
  :ready       function of the agent's terminal returning non-nil when
               the agent waits for its user's next message: its session
               has begun and its latest turn has ended, so what is typed
               goes to its composer, not to a dialog.  Other agents'
               messages wait for it; an agent without one is given none,
               unless it has a :deliver
  :deliver     function of the agent's terminal and a text giving the
               agent the text as its user's next message through its
               own program, rather than its composer: the program takes
               it at once or keeps it until the agent is free.  It
               returns non-nil if the agent took it at once.  Other
               agents' messages then go this way, the agent free or not
  :answer      function of the agent's terminal, a text and a function
               of a text: it gives the agent the text as :deliver does,
               and calls the function with the agent's final reply to
               it once its turn ends.  For an agent served no tools of
               LispM's (see lispm-agents-bridge.el), whose reply to a
               request is then its answer

An agent running inside Emacs has these instead:

  :new         function of a directory starting a session there; returns
               its buffer
  :resume      function of a session resuming it; returns its buffer
  :buffer-p    predicate on buffers holding one of its sessions
  :send        function of a buffer and text putting the text into the
               session's input, without sending it

Defining AGENT again replaces what it was.  Return AGENT."
  (if-let* ((entry (assq agent lispm-agents-defined)))
      (progn (lispm-agents--unbind-key agent (cdr entry))
             (setcdr entry properties))
    (setq lispm-agents-defined (append lispm-agents-defined (list (cons agent properties)))))
  (lispm-agents--bind-key agent properties)
  (lispm-agents--changed)
  agent)

(defun lispm-agents-undefine (agent)
  "Make AGENT none of the editor's agents any more.
Its terminals go on running, but the servers showing them Emacs's state
stop now, while the code stopping them is still loaded: an extension
undefines its agent as it is unloaded.  Nothing offers AGENT any more,
nor lists its sessions."
  (when-let* ((entry (assq agent lispm-agents-defined)))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'lispm-agents-buffer-agent buffer) agent)
        (lispm-agents-context-release buffer)))
    (setq lispm-agents-defined (delq entry lispm-agents-defined))
    (lispm-agents--unbind-key agent (cdr entry))
    (lispm-agents--changed)))

(defvar lispm-agents-changed-hook nil
  "Hook run when what the agents list shows may have changed by itself: an
agent was defined or removed, or an agent changed its sessions, as a LispM
agent does.  A list of the agents shows them anew once Emacs is idle.")

(defvar lispm-agents-sessions-changed-hook nil
  "Hook run once a session was shown, renamed, archived or deleted.
A list of the sessions reads them anew and shows them at once, see
`lispm-agents-reread-sessions-hook'.")

(defvar lispm-agents-roster-change-functions nil
  "Abnormal hook run with (KEYS FIELDS EVENT) after roster changes.
KEYS are stable (KIND SESSION-ID) keys, including departed agents;
FIELDS name changed record fields or lazy work, such as `calls' and
`turn'.  EVENT is nil or a plist with :events,
the ordered events collected together, each with a unique :sequence.
Message events carry :id, :from, :to and :request-id when known.
Changes in one command, timer or process output are collected by a
single zero-delay timer, never an idle or repeating timer.  Observers
read `lispm-agents-roster' again, or its lazy details for an inspector.")

(defun lispm-agents-roster-notify (subject fields &optional event)
  "Report changes to SUBJECT's FIELDS, with optional one-shot EVENT.
SUBJECT is an agent, buffer, stable (KIND SESSION-ID) key, or nil when
the affected agents must be found by comparing snapshots.  FIELDS is
a list of symbols naming record fields; nil asks only for comparison.
EVENT is a plist, as `lispm-agents-roster-change-functions' describes.
Delegates to `lispm-emit-change' for registered observers."
  (when lispm-agents-roster-change-functions
    (lispm-emit-change subject fields event)))

(defun lispm-agents--changed ()
  "Forget what was worked out from the agents, and tell who shows them."
  (clrhash lispm-agents-root-cache)
  (run-hooks 'lispm-agents-changed-hook))

(defun lispm-agents--key-binding (agent properties)
  "Return the binding of AGENT's key, of PROPERTIES: its command while it
can run, nothing otherwise."
  ;; The item's name is a form, evaluated where a menu shows it, so it
  ;; speaks the language spoken then.
  `(menu-item (lispm-tr "New %s session" ,(plist-get properties :name))
              ,(plist-get properties :command)
              :filter ,(lambda (command) (and (lispm-agents-available-p agent) command))))

(defvar lispm-agents--keys nil
  "The keys bound to start agents, as (KEY . AGENT).")

(defvar lispm-agents-key-maps '(lispm-agents-map)
  "Variables holding the keymaps agents' keys are bound in:
`lispm-agents-map', and those added with `lispm-agents-add-key-map',
as a list of the agents adds its own.")

(defun lispm-agents--key-maps ()
  "Return the keymaps agents' keys are bound in, see
`lispm-agents-key-maps'."
  (mapcar #'symbol-value lispm-agents-key-maps))

(defun lispm-agents--key-taken-p (key)
  "Return non-nil when KEY starts a command of its own, not an agent, in a
keymap of `lispm-agents--key-maps'; what a keymap's parent binds
does not count."
  (let ((event (aref (key-parse key) 0))
        (taken nil))
    (dolist (map (lispm-agents--key-maps))
      (map-keymap-internal (lambda (bound binding)
                             (when (and binding (eq bound event))
                               (setq taken t)))
                           map))
    taken))

(defun lispm-agents--bind-key (agent properties)
  "Bind AGENT's key, of PROPERTIES, in the keymaps of
`lispm-agents-key-maps'; one another agent has is taken over, one
starting another command is left to it."
  (when-let* ((key (plist-get properties :key)))
    (if (and (not (assoc key lispm-agents--keys))
             (lispm-agents--key-taken-p key))
        (message (lispm-tr "%s gets no key: %s is taken") (plist-get properties :name) key)
      (setf (alist-get key lispm-agents--keys nil nil #'equal) agent)
      (dolist (map (lispm-agents--key-maps))
        (keymap-set map key (lispm-agents--key-binding agent properties))))))

(defun lispm-agents--unbind-key (agent properties)
  "Unbind the key of AGENT, of PROPERTIES, when it is AGENT's; an agent
still defined that wants it gets it."
  (when-let* ((key (plist-get properties :key))
              ((eq (alist-get key lispm-agents--keys nil nil #'equal) agent)))
    (setq lispm-agents--keys (assoc-delete-all key lispm-agents--keys))
    (dolist (map (lispm-agents--key-maps))
      (keymap-unset map key t))
    (when-let* ((other (seq-find (lambda (entry)
                                   (and (not (eq (car entry) agent))
                                        (equal (plist-get (cdr entry) :key) key)))
                                 lispm-agents-defined)))
      (lispm-agents--bind-key (car other) (cdr other)))))

(defun lispm-agents-add-key-map (variable)
  "Bind the agents' keys in the keymap VARIABLE holds too, from now on, as
a list of the agents does for its own keymap."
  (add-to-list 'lispm-agents-key-maps variable t)
  (pcase-dolist (`(,key . ,agent) lispm-agents--keys)
    (keymap-set (symbol-value variable) key
                (lispm-agents--key-binding agent (lispm-agents-props agent)))))

(defun lispm-agents-props (agent)
  "Return the properties of AGENT, or nil when it is no agent."
  (alist-get agent lispm-agents-defined))

(defun lispm-agents-name (agent)
  "Return the name of AGENT."
  (or (plist-get (lispm-agents-props agent) :name)
      (capitalize (symbol-name agent))))

(defun lispm-agents-available-p (agent)
  "Return non-nil when AGENT is an agent that can run here.
One run as a CLI can while its program is on PATH."
  (when-let* ((props (lispm-agents-props agent)))
    (let ((program (plist-get props :program)))
      (or (null program) (and (executable-find program) t)))))

(defun lispm-agents-ordered ()
  "Return the agents, as (AGENT . PROPERTIES): those built in first, then
the others, each in the order they were defined."
  (append (seq-filter (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-agents-defined)
          (seq-remove (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-agents-defined)))

(defun lispm-agents-available ()
  "Return the agents that can run here, as (AGENT . PROPERTIES): those
built in first, then the others in the order they were defined."
  (seq-filter (lambda (entry) (lispm-agents-available-p (car entry)))
              (lispm-agents-ordered)))

(defun lispm-agents-check-available (agent)
  "Return the properties of AGENT; signal a `user-error' unless it can run."
  (let ((props (or (lispm-agents-props agent)
                   (user-error (lispm-tr "There is no agent %s") agent))))
    (unless (lispm-agents-available-p agent)
      (user-error (lispm-tr "%s is not installed: `%s' is not on PATH")
                  (plist-get props :name) (plist-get props :program)))
    props))

(defun lispm-agents-emacs-buffer-agent (buffer)
  "Return the in-Emacs agent kind whose session BUFFER holds, or nil.
LispM's own session buffers are recognised even without a host having
registered session-list commands for the `lispm' kind.  A registered
:buffer-p instead chooses which view represents a live native session."
  (or (car (seq-find (lambda (entry)
                      (when-let* ((buffer-p (plist-get (cdr entry) :buffer-p)))
                        (funcall buffer-p buffer)))
                    lispm-agents-defined))
      (and (not (plist-get (lispm-agents-props 'lispm) :buffer-p))
           (boundp 'lispm-session-agent)
           (buffer-local-value 'lispm-session-agent buffer)
           'lispm)))

(defconst lispm-agents-transcript-head-bytes 65536
  "Bytes read from the start of a transcript to find its metadata and first prompt.")

(defconst lispm-agents-transcript-tail-bytes 16384
  "Bytes read from the end of a transcript to find a title set late in the session.")

(defconst lispm-agents-title-width 72
  "Maximum width of a session title in the session list.")


(provide 'lispm-agents-core)

;;; lispm-agents-core.el ends here
