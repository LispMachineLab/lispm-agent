;;; lispm-agents.el --- Coding agents, in terminals and in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-agent
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; Coding agents run as their native CLIs inside terminals, or inside
;; Emacs: the editor's agents.  Two come with this package, Claude Code
;; (lispm-agents-claude.el) and Codex (lispm-agents-codex.el); others
;; are packages of their own, as Hermes (LispMachineLab/lispm-hermes).
;; Every agent tells `lispm-agents-define' how to list, start, resume,
;; rename and delete its sessions, and then takes part in all this module
;; adds around it:
;;
;; - a unified, newest-first list of every recorded session of the agents,
;;   resuming the chosen one in its original working directory
;;   (`lispm-agents-sessions');
;; - commands that start a fresh agent at the current project root, in
;;   `lispm-agents-map', which you bind where you like, as under C-c a;
;; - sending the active region (with its location) to a running agent;
;; - a server per terminal showing the agent Emacs's state
;;   (`lispm-agents-context');
;; - what an interface around them needs: sessions grouped by project,
;;   running terminals, archiving, icons.
;;
;; The agents know no interface but Emacs's.  A program drawing one
;; around them says how through the variables of the section Interface
;; below: the terminals agents run in (Eat's by
;; default), how an agent's buffer is shown, which window is the main
;; one, how a deletion is confirmed.
;;
;; An agent run as a CLI takes no part anywhere while its program is not
;; on PATH: no menu, button or key offers it and none of its sessions are
;; listed.
;;
;; A session is a plist (:agent :id :cwd :title :time :file).  Agents that
;; keep JSONL transcripts read them with `lispm-agents-cached-session',
;; which memoizes the parse per (file . mtime), so reopening the list
;; rereads only files that changed since the last listing:
;;
;;   transcript file --(head/tail bytes)--> JSON objects --(parser)--> session

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-agents-context)
(require 'lispm-i18n)

(declare-function project-root "project" (project))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-term-send-string-as-yank "eat" (terminal args))
(declare-function eat-term-send-string "eat" (terminal string))
(defvar eat-terminal)
(defvar embark-keymap-alist)
(defvar lispm-agents--root-cache)

;;;; Interface
;;
;; The agents know no interface but Emacs's; a program drawing one around
;; them sets these.

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

(defvar lispm-agents-confirm-function #'lispm-agents-yes-or-no-p
  "Function asking whether to go on with something that cannot be undone,
as deleting a session.  It is called with a question, a detail and the
name of the action, as \"Delete\", and returns non-nil to go on.")

(defun lispm-agents-yes-or-no-p (question detail _action)
  "Ask QUESTION, with DETAIL, with `yes-or-no-p'."
  (yes-or-no-p (format "%s %s " question detail)))

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
Each agent adds the key starting it, see `lispm-agents-define'."
  "s" #'lispm-agents-sessions
  "r" #'lispm-agents-send-region)

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
               (:agent :id :cwd :title :time :file); an agent that
               assigns its own ids adds :started, see
               `lispm-agents-claim-sessions'
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
  :worktree    function of a directory returning nil, or where that
               worktree of the agent's belongs: (:root REPOSITORY :label
               LABEL), or (:name NAME :label LABEL) when only the
               repository's name is known; LABEL is shown as it is,
               in the language spoken
  :chats      directories whose sessions are chats, not project work
  :short-id    function of a session's id returning the short form that
               tells it from others; by default its first eight characters

An agent run as a CLI in a terminal has these:

  :program     its executable; while it is not on PATH, the agent takes
               part nowhere
  :new-arguments     function of no argument returning (ID . ARGUMENTS)
                     starting a new session; ID is the session's id when
                     the agent takes one it is given, else nil
  :resume-arguments  function of a session's id returning ARGUMENTS
                     resuming it
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

(defun lispm-agents--changed ()
  "Forget what was worked out from the agents, and tell who shows them."
  (clrhash lispm-agents--root-cache)
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

(defun lispm-agents--ordered ()
  "Return the agents, as (AGENT . PROPERTIES): those built in first, then
the others, each in the order they were defined."
  (append (seq-filter (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-agents-defined)
          (seq-remove (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-agents-defined)))

(defun lispm-agents-available ()
  "Return the agents that can run here, as (AGENT . PROPERTIES): those
built in first, then the others in the order they were defined."
  (seq-filter (lambda (entry) (lispm-agents-available-p (car entry)))
              (lispm-agents--ordered)))

(defun lispm-agents--check-available (agent)
  "Return the properties of AGENT; signal a `user-error' unless it can run."
  (let ((props (or (lispm-agents-props agent)
                   (user-error (lispm-tr "There is no agent %s") agent))))
    (unless (lispm-agents-available-p agent)
      (user-error (lispm-tr "%s is not installed: `%s' is not on PATH")
                  (plist-get props :name) (plist-get props :program)))
    props))

(defun lispm-agents-emacs-buffer-agent (buffer)
  "Return the in-Emacs agent whose session BUFFER holds, or nil."
  (car (seq-find (lambda (entry)
                   (when-let* ((buffer-p (plist-get (cdr entry) :buffer-p)))
                     (funcall buffer-p buffer)))
                 lispm-agents-defined)))

(defconst lispm-agents-transcript-head-bytes 65536
  "Bytes read from the start of a transcript to find its metadata and first prompt.")

(defconst lispm-agents-transcript-tail-bytes 16384
  "Bytes read from the end of a transcript to find a title set late in the session.")

(defconst lispm-agents-title-width 72
  "Maximum width of a session title in the session list.")

;;;; Transcript reading (effects)

(defun lispm-agents-read-json-region (file beg end)
  "Return the JSON objects on the complete lines of FILE between BEG and END.
A line cut by BEG or END (when END stops short of the file's size) is skipped,
as is any line that is not valid JSON.  Objects are plists with list arrays."
  (let ((truncated (< end (file-attribute-size (file-attributes file)))))
    (with-temp-buffer
      (insert-file-contents file nil beg end)
      (goto-char (point-min))
      (when (> beg 0) (forward-line 1))
      (lispm-agents--parse-json-lines truncated))))

(defun lispm-agents--parse-json-lines (truncated)
  "Return the JSON objects on the lines from point to the end of the buffer.
When TRUNCATED, the last line is partial and is skipped."
  (let (objects)
    (while (not (eobp))
      (let ((line (unless (and truncated (= (line-end-position) (point-max)))
                    (buffer-substring-no-properties (point) (line-end-position)))))
        (when-let* ((object (and line
                                 (ignore-errors
                                   (json-parse-string line
                                                      :object-type 'plist
                                                      :array-type 'list
                                                      :null-object nil
                                                      :false-object nil)))))
          (push object objects)))
      (forward-line 1))
    (nreverse objects)))

(defconst lispm-agents-tail-bytes (* 256 1024)
  "Bytes at the end of a transcript `lispm-agents-read-json-tail' reads.")

(defun lispm-agents-read-json-tail (file)
  "Return the JSON objects on the complete lines at the end of FILE.
They are those of its last `lispm-agents-tail-bytes', oldest first."
  (let ((size (file-attribute-size (file-attributes file))))
    (lispm-agents-read-json-region file (max 0 (- size lispm-agents-tail-bytes)) size)))

(defun lispm-agents-read-transcript (file)
  "Return the JSON objects at the head and tail of transcript FILE as a cons."
  (let ((size (file-attribute-size (file-attributes file))))
    (cons (lispm-agents-read-json-region
           file 0 (min size lispm-agents-transcript-head-bytes))
          (when (> size lispm-agents-transcript-head-bytes)
            (lispm-agents-read-json-region
             file (max lispm-agents-transcript-head-bytes
                       (- size lispm-agents-transcript-tail-bytes))
             size)))))

(defun lispm-agents-file-time (file)
  "Return the modification time of FILE as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

(defun lispm-agents-append-json-line (file object)
  "Append OBJECT, a plist, to JSON Lines FILE as one UTF-8 line."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (decode-coding-string (json-serialize object) 'utf-8) "\n")
                  nil file t 'silent)))

;;;; Transcript parsing (pure)

(defun lispm-agents-human-text-p (text)
  "Return non-nil when TEXT reads as a prompt a person typed.
Agents inject context, command echoes and reminders as tagged blocks that
start with `<', and Codex injects project guidance headed `# AGENTS.md';
those are not prompts."
  (and (stringp text)
       (let ((trimmed (string-trim-left text)))
         (not (or (string-empty-p trimmed)
                  (string-prefix-p "<" trimmed)
                  (string-prefix-p "# AGENTS.md" trimmed))))))

(defun lispm-agents-content-text (content)
  "Return the text of message CONTENT, a string or a list of typed parts."
  (if (stringp content)
      content
    (mapconcat (lambda (part) (or (plist-get part :text) "")) content "")))

;;;; Session discovery

(defvar lispm-agents--session-cache (make-hash-table :test #'equal)
  "Memoized sessions keyed by transcript file, valued (MTIME . SESSION-OR-NIL).")

(defun lispm-agents-cached-session (file parse)
  "Return the session of transcript FILE, calling PARSE when it changed.
PARSE maps the head and tail objects of FILE, as
`lispm-agents-read-transcript' returns them, to a session plist or
nil; the session's :time is FILE's modification time."
  (let ((time (lispm-agents-file-time file))
        (cached (gethash file lispm-agents--session-cache)))
    (if (and cached (= (car cached) time))
        (cdr cached)
      (let ((session (when-let* ((parsed (funcall parse (lispm-agents-read-transcript file))))
                       (plist-put parsed :time time))))
        (puthash file (cons time session) lispm-agents--session-cache)
        session))))

(defun lispm-agents-file-sessions (files parse &optional finish)
  "Return the sessions recorded in transcript FILES, as PARSE finds them.
PARSE is called with a file and its head and tail objects, through
`lispm-agents-cached-session'.  FINISH, when given, is called with
each session found and returns it as shown, or nil to leave it out; the
sessions are memoized, so it must not modify them."
  (delq nil (mapcar (lambda (file)
                      (when-let* ((session (lispm-agents-cached-session
                                            file (apply-partially parse file))))
                        (if finish (funcall finish session) session)))
                    files)))

(defun lispm-agents--newest-per-id (sessions)
  "Return SESSIONS keeping only the most recent transcript of each agent id.
Codex writes forked threads to separate files that share the session id."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((key (cons (plist-get session :agent) (plist-get session :id)))
             (seen (gethash key latest)))
        (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
          (puthash key session latest))))
    (hash-table-values latest)))

(defun lispm-agents--agent-sessions (props &optional reader)
  "Return the sessions of the agent of PROPS, as its :sessions reads them,
or as READER, its session reader, has them; READER may say `pending'
instead, which is returned as it is.  An agent whose sessions cannot be
read lists none, and says so."
  (condition-case err
      (let ((sessions (funcall (or reader (plist-get props :sessions)))))
        (if (and reader (eq sessions 'pending))
            'pending
          (delq nil (copy-sequence sessions))))
    (error (message (lispm-tr "%s sessions unavailable: %s") (plist-get props :name)
                    (error-message-string err))
           nil)))

(defun lispm-agents--sort-sessions (sessions)
  "Return SESSIONS, the newest transcript of each id only, newest first."
  (sort (lispm-agents--newest-per-id sessions)
        (lambda (a b) (> (plist-get a :time) (plist-get b :time)))))

(defun lispm-agents-sessions-list ()
  "Return every recorded session of the agents that can run here, newest
first, see `lispm-agents-available'.  An agent whose sessions cannot
be read lists none, and says so.  Each agent's :sessions reads them as
they are now; session readers are never asked, so the list is complete,
see `lispm-agents-sessions-snapshot'.  The terminals running sessions
learn theirs from them, see `lispm-agents-refresh-buffer-titles'."
  (let ((sessions (lispm-agents--sort-sessions
                   (mapcan (pcase-lambda (`(,_ . ,props)) (lispm-agents--agent-sessions props))
                           (lispm-agents-available)))))
    (lispm-agents-refresh-buffer-titles sessions)
    sessions))

;;;;; Session readers
;;
;; Reading every transcript can take seconds.  An agent may have a reader
;; besides its :sessions that never waits, its sessions read in the
;; background, off Emacs's thread: a list of the sessions draws what the
;; readers have, and again as they get more; the session picker still
;; reads every session as it is.

(defvar lispm-agents--session-readers nil
  "The agents' session readers, as (AGENT . READER).
See `lispm-agents-add-session-reader'.")

(defvar lispm-agents-reread-sessions-hook nil
  "Hook run when the sessions must be read anew, before a list of them
draws them: when it is asked to, as by a Refresh button, and once a
session was shown, renamed, archived or deleted,
see `lispm-agents-sessions-changed-hook'.  A session reader, see
`lispm-agents-add-session-reader', forgets what it has and reads
again.  A list drawn again for anything else, an agent's status, an
agent exiting, the agents changing, keeps what the readers have.")

(defvar lispm-agents-sessions-arrived-hook nil
  "Hook run when a session reader has newer sessions, see
`lispm-agents-sessions-arrived'.  A list of the sessions shows them
once Emacs is idle.")

(defun lispm-agents-sessions-arrived ()
  "Say that a session reader has newer sessions than lists of them show.
A reader, see `lispm-agents-add-session-reader', calls this once
what it read in the background is in: a list of the sessions draws
them again once Emacs is idle, shown or not, and no reader is asked
to read anew.  This runs `lispm-agents-sessions-arrived-hook'."
  (run-hooks 'lispm-agents-sessions-arrived-hook))

(defun lispm-agents-add-session-reader (agent reader)
  "Make READER the reader of AGENT's sessions for lists of the sessions.
READER is called with no argument and returns at once, never waiting:
AGENT's sessions as its :sessions returns them, see
`lispm-agents-define', or the symbol `pending' while it has none
to give yet, and a list then says it is still reading.  A reader
reading in the background calls `lispm-agents-sessions-arrived'
when it has newer sessions, and reads anew on
`lispm-agents-reread-sessions-hook'.

Only such lists ask readers, see `lispm-agents-sessions-snapshot';
`lispm-agents-sessions-list', which the session picker reads,
always reads the sessions themselves.  AGENT has one reader at most:
adding another replaces it.  Lists draw the sessions again once Emacs
is idle.  Return AGENT."
  (setf (alist-get agent lispm-agents--session-readers) reader)
  (lispm-agents-sessions-arrived)
  agent)

(defun lispm-agents-remove-session-reader (agent)
  "Remove the session reader of AGENT, see
`lispm-agents-add-session-reader'.  Lists read AGENT's
sessions with its :sessions again, once Emacs is idle."
  (when (assq agent lispm-agents--session-readers)
    (setq lispm-agents--session-readers
          (seq-remove (lambda (entry) (eq (car entry) agent))
                      lispm-agents--session-readers))
    (lispm-agents-sessions-arrived)))

(defun lispm-agents-sessions-snapshot ()
  "Return the sessions as they are known now, (SESSIONS . PENDING).
SESSIONS are as `lispm-agents-sessions-list' returns them, but an
agent with a session reader lists what its reader has, without waiting,
see `lispm-agents-add-session-reader'; PENDING are the agents
whose readers have nothing yet, in the order agents are offered.  Agents
without a reader are read as they are now.  A list of the sessions
draws this."
  (let ((pending nil))
    (cons (lispm-agents--sort-sessions
           (mapcan (pcase-lambda (`(,agent . ,props))
                     (let ((sessions (lispm-agents--agent-sessions
                                      props (alist-get agent lispm-agents--session-readers))))
                       (if (eq sessions 'pending)
                           (progn (push agent pending) nil)
                         sessions)))
                   (lispm-agents-available)))
          (nreverse pending))))

;;;; Presentation (pure)

(defun lispm-agents-age (time now)
  "Return a short human age for TIME relative to NOW, both float seconds,
in the language spoken: 2m, 3h or 4d in English.  Past a month it is the
date, short: 5/13/26."
  (let ((seconds (- now time)))
    (cond ((< seconds 3600)
           (let ((minutes (truncate seconds 60)))
             (lispm-tr "%dm" minutes)))
          ((< seconds 86400)
           (let ((hours (truncate seconds 3600)))
             (lispm-tr "%dh" hours)))
          ((< seconds (* 30 86400))
           (let ((days (truncate seconds 86400)))
             (lispm-tr "%dd" days)))
          (t (lispm-format-time "%-m/%-d/%y" time)))))

(defun lispm-agents--short-id (session)
  "Return the short form of SESSION's id that tells sessions apart: what
its agent's :short-id makes of it, else its first eight characters, as
of a UUID."
  (let ((id (plist-get session :id)))
    (if-let* ((short (plist-get (lispm-agents-props (plist-get session :agent)) :short-id)))
        (funcall short id)
      (substring id 0 (min 8 (length id))))))

(defconst lispm-agents-label-faces
  '(font-lock-function-name-face font-lock-keyword-face font-lock-type-face
    font-lock-constant-face font-lock-variable-name-face)
  "Faces telling the agents apart in the session list, one per agent in turn.")

(defun lispm-agents--label-face (agent)
  "Return the face of AGENT's name in the session list: the agents built
in keep theirs, whichever extensions were loaded before them."
  (let ((index (or (seq-position (mapcar #'car (lispm-agents--ordered)) agent) 0)))
    (nth (mod index (length lispm-agents-label-faces)) lispm-agents-label-faces)))

(defun lispm-agents-one-line (text width)
  "Return TEXT collapsed to a single line and truncated to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
   width nil nil "…"))

(defun lispm-agents--session-label (session now)
  "Return the completion label for SESSION, with ages relative to NOW.
The short id suffix keeps labels unique across sessions with equal titles."
  (format "%s %s %s %s %s"
          (propertize (format "%-6s" (plist-get session :agent))
                      'face (lispm-agents--label-face (plist-get session :agent)))
          (propertize (format "%-18s" (truncate-string-to-width
                                       (file-name-nondirectory
                                        (directory-file-name (plist-get session :cwd)))
                                       18))
                      'face 'font-lock-constant-face)
          ;; Padded by its width: an age may be written in Chinese.
          (propertize (let ((age (lispm-agents-age (plist-get session :time) now)))
                        (concat age (make-string (max 0 (- 10 (string-width age))) ?\s)))
                      'face 'shadow)
          (lispm-agents-one-line (plist-get session :title) lispm-agents-title-width)
          (propertize (lispm-agents--short-id session) 'face 'shadow)))

;;;; Terminals (effects)

(defvar-local lispm-agents-buffer-agent nil
  "The agent this terminal runs, a key of `lispm-agents-defined'.")
(put 'lispm-agents-buffer-agent 'permanent-local t)

(defun lispm-agents-terminal-agent (buffer)
  "Return the agent the terminal BUFFER runs, or nil when it runs none.
A terminal an agent was started in says so, see `lispm-agents--launch';
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

(defun lispm-agents-running-p (buffer)
  "Return non-nil when BUFFER exists and still runs a live process.
The session of an agent running inside Emacs runs while its buffer lives."
  (if (and buffer (lispm-agents-emacs-buffer-agent buffer))
      (buffer-live-p buffer)
    (and buffer (lispm-agents-terminal-live-p buffer))))

(defvar-local lispm-agents-buffer-session-id nil
  "Id of the session this agent terminal runs, when known.")
(put 'lispm-agents-buffer-session-id 'permanent-local t)

(defvar-local lispm-agents-buffer-launched nil
  "Time, float seconds, this agent terminal was started.")
(put 'lispm-agents-buffer-launched 'permanent-local t)

(defvar-local lispm-agents-buffer-title nil
  "Human-readable title of the session this agent terminal runs.
Tab labels show it instead of the terminal's machine-oriented buffer name.")
(put 'lispm-agents-buffer-title 'permanent-local t)

(defun lispm-agents-uuid ()
  "Return a random version 4 UUID string."
  (let ((bytes (mapcar (lambda (_) (random 256)) (number-sequence 1 16))))
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
           bytes)))

(defconst lispm-agents-session-marker-regexp
  (concat "\\`\\(CLAUDECODE\\|CLAUDE_PID\\|CLAUDE_EFFORT"
          "\\|CLAUDE_CODE_[A-Z0-9_]*\\|CLAUDE_AGENT_SDK_[A-Z0-9_]*"
          "\\|CLAUDE_PREVIEW_[A-Z0-9_]*\\)=")
  "Environment entries that tie a process to an enclosing Claude Code session.
An Emacs started from inside Claude Code inherits them; a Claude launched with
them believes it is that session's child, disables its own transcript and
reuses the parent's session id and messaging socket.")

(defun lispm-agents-independent-environment (environment)
  "Return ENVIRONMENT without enclosing Claude Code session markers.
Agents and shells started from Emacs are independent top-level sessions."
  (seq-remove (lambda (entry) (string-match-p lispm-agents-session-marker-regexp entry))
              environment))

(defun lispm-agents--launch (agent directory name args &optional id title)
  "Run AGENT with ARGS in DIRECTORY inside the terminal NAME; return it.
The terminal is what `lispm-agents-terminal-make' makes.
The terminal records the session ID and TITLE for labels and lists.
The agent runs as an independent session whatever environment Emacs itself
inherited, and with its :context servers showing it Emacs's state and its
:arguments, see `lispm-agents-define'.  A running terminal with the
same NAME is returned instead of restarted."
  (let ((props (lispm-agents--check-available agent)))
    (unless (file-directory-p directory)
      (user-error (lispm-tr "Working directory no longer exists: %s") directory))
    (let* ((default-directory (file-name-as-directory directory))
           ;; A new agent gets servers showing it Emacs's state; a running
           ;; terminal returned as is keeps those it has.
           (servers (unless (lispm-agents-running-p (get-buffer (format "*%s*" name)))
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
              lispm-agents-buffer-session-id (or id lispm-agents-buffer-session-id)
              lispm-agents-buffer-title (or title lispm-agents-buffer-title)
              lispm-agents-buffer-launched (or lispm-agents-buffer-launched (float-time))))
      ;; Agents record their sessions as they go, so closing the terminal
      ;; loses nothing a resume cannot restore: kill it without asking.
      (when-let* ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil))
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
  (let* ((running (lispm-agents-running-buffers))
         (claimed (delq nil (mapcar (lambda (buffer)
                                      (buffer-local-value 'lispm-agents-buffer-session-id buffer))
                                    running)))
         (candidates (sort (seq-filter (lambda (session) (plist-get session :started))
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
            (setq lispm-agents-buffer-session-id (plist-get session :id))
            (push (plist-get session :id) claimed)))))))

(defvar-local lispm-agents--claimed-at nil
  "When this terminal last had the sessions listed to learn its own.")
(put 'lispm-agents--claimed-at 'permanent-local t)

(defun lispm-agents-terminal-session-id (terminal)
  "Return the id of the session the agent TERMINAL runs, or nil if unknown.
A terminal started fresh for an agent that assigns its own ids learns it
once the agent recorded the session, from a list of the sessions, see
`lispm-agents-claim-sessions'; one is made at most every ten seconds."
  (with-current-buffer terminal
    (unless (or lispm-agents-buffer-session-id
                (and lispm-agents--claimed-at (< (- (float-time) lispm-agents--claimed-at) 10)))
      (setq lispm-agents--claimed-at (float-time))
      (lispm-agents-sessions-list))
    lispm-agents-buffer-session-id))

(defun lispm-agents-refresh-buffer-titles (sessions)
  "Update the titles of running agent terminals from SESSIONS.
Terminals first claim their sessions, see
`lispm-agents-claim-sessions'.  A new session gains its real title
once its first prompt is recorded, and a renamed session follows its new name."
  (lispm-agents-claim-sessions sessions)
  (let ((titles (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (plist-get session :id) (plist-get session :title) titles))
    (dolist (buffer (lispm-agents-running-buffers))
      (with-current-buffer buffer
        (when-let* ((title (and lispm-agents-buffer-session-id
                                (gethash lispm-agents-buffer-session-id titles))))
          (setq lispm-agents-buffer-title title))))))

(defun lispm-agents-show (buffer)
  "Show the agent's BUFFER and select it, as `lispm-agents-show-function'
does; who lists the sessions shows them anew, so the agent appears among
those running."
  (funcall lispm-agents-show-function buffer)
  (run-hooks 'lispm-agents-sessions-changed-hook)
  buffer)

(defun lispm-agents-project-directory ()
  "Return the current project root, or `default-directory' outside projects."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun lispm-agents-default-directory ()
  "Return where a new session or shell starts: the project root of the main
window's buffer, see `lispm-agents-main-window-function'."
  (with-selected-window (funcall lispm-agents-main-window-function)
    (lispm-agents-project-directory)))

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

;;;; Commands

(defvar lispm-agents--candidates nil
  "Session plists of the last session listing, keyed by their labels.")

(defun lispm-agents--session-at (label)
  "Return the session plist listed under LABEL."
  (or (and lispm-agents--candidates (gethash label lispm-agents--candidates))
      (user-error (lispm-tr "Unknown agent session: %s") label)))

(defun lispm-agents--read-session (prompt)
  "Read a recorded agent session with PROMPT and return its plist."
  (let* ((now (float-time))
         (table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (session)
                           (let ((label (lispm-agents--session-label session now)))
                             (puthash label session table)
                             label))
                         (lispm-agents-sessions-list))))
    (setq lispm-agents--candidates table)
    (lispm-agents--session-at
     (completing-read prompt
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata (category . lispm-agents-session)
                                       (display-sort-function . identity))
                          (complete-with-action action labels string pred)))
                      nil t))))

(defun lispm-agents-resume-session (session)
  "Resume SESSION in its original working directory.
A session already running in a terminal is shown there instead: resuming it
again would run a second agent on the same session."
  (let ((agent (plist-get session :agent))
        (id (plist-get session :id))
        (cwd (plist-get session :cwd)))
    (lispm-agents-show
     (or (lispm-agents-session-buffer id)
         (let ((props (lispm-agents--check-available agent)))
           (if-let* ((resume (plist-get props :resume)))
               (funcall resume session)
             (lispm-agents--launch agent cwd
                                   (format "%s:%s:%s" agent
                                           (file-name-nondirectory (directory-file-name cwd))
                                           (lispm-agents--short-id session))
                                   (funcall (plist-get props :resume-arguments) id)
                                   id
                                   (plist-get session :title))))))))

(defun lispm-agents-sessions ()
  "Choose a recorded session of any agent and resume it."
  (interactive)
  (lispm-agents-resume-session (lispm-agents--read-session (lispm-tr "Agent session: "))))

(defun lispm-agents-new (agent &optional directory)
  "Start a new AGENT session in DIRECTORY.
DIRECTORY defaults to the project root of the main window's buffer."
  (let* ((props (lispm-agents--check-available agent))
         (directory (or directory (lispm-agents-default-directory))))
    (lispm-agents-show
     (if-let* ((new (plist-get props :new)))
         (funcall new directory)
       (pcase-let ((`(,id . ,args) (when-let* ((new-arguments (plist-get props :new-arguments)))
                                     (funcall new-arguments))))
         (lispm-agents--launch agent directory (lispm-agents-fresh-name agent directory)
                               args id
                               (lispm-tr "New %s · %s" (plist-get props :name)
                                         (file-name-nondirectory (directory-file-name directory)))))))))

(defun lispm-agents--pick-terminal ()
  "Return the agent terminal to send to, preferring the current project.
Ask when several running agent terminals qualify."
  (let* ((root (lispm-agents-project-directory))
         (running (seq-filter (lambda (buffer)
                                (and (lispm-agents-buffer-p buffer)
                                     (lispm-agents-running-p buffer)))
                              (buffer-list)))
         (local (seq-filter (lambda (buffer)
                              (file-in-directory-p
                               (buffer-local-value 'default-directory buffer) root))
                            running))
         (candidates (or local running)))
    (pcase candidates
      ('nil (if-let* ((keys (delq nil (mapcar (lambda (entry)
                                                 (when-let* ((key (plist-get (cdr entry) :key)))
                                                   (concat "C-c a " key)))
                                               (lispm-agents-available)))))
                (user-error (lispm-tr "No running agent; start one with %s")
                            (string-join keys ", "))
              (user-error (lispm-tr "No running agent; start one first"))))
      (`(,only) only)
      (_ (get-buffer (completing-read (lispm-tr "Send to agent: ")
                                      (mapcar #'buffer-name candidates) nil t))))))

(defun lispm-agents--region-reference (beg end)
  "Return a fenced reference to the text between BEG and END of this buffer.
The header names the file relative to the project root and the line range."
  (let* ((file (if buffer-file-name
                   (file-relative-name buffer-file-name (lispm-agents-project-directory))
                 (buffer-name)))
         (first (line-number-at-pos beg))
         (last (line-number-at-pos (if (and (> end beg) (= (char-before end) ?\n))
                                       (1- end)
                                     end)))
         (lang (string-remove-suffix
                "-ts" (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (format "%s:%d-%d\n```%s\n%s\n```\n"
            file first last lang
            (string-trim-right (buffer-substring-no-properties beg end)))))

(defun lispm-agents-send-region (beg end)
  "Send the region between BEG and END, with its location, to an agent terminal.
Without an active region, send a reference to the current file and line.
The text is pasted into the agent's composer without submitting it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((text (if beg
                  (lispm-agents--region-reference beg end)
                (format "%s:%d "
                        (if buffer-file-name
                            (file-relative-name buffer-file-name
                                                (lispm-agents-project-directory))
                          (buffer-name))
                        (line-number-at-pos))))
        (target (lispm-agents--pick-terminal)))
    (deactivate-mark)
    (if-let* ((send (plist-get (lispm-agents-props (lispm-agents-emacs-buffer-agent target))
                               :send)))
        (funcall send target text)
      (lispm-agents-terminal-paste target text))
    (pop-to-buffer target)))

;;;; Session management

(defconst lispm-agents-state-file (locate-user-emacs-file "agent-sessions.eld")
  "File holding Emacs-side session state, currently the archived sessions.
Archiving hides a session in Emacs only; it never alters agent data.")

(defvar lispm-agents--archived 'unloaded
  "Keys of archived sessions, or `unloaded' before the state file is read.")

(defun lispm-agents--session-key (session)
  "Return the key identifying SESSION across agents, \"AGENT:ID\"."
  (format "%s:%s" (plist-get session :agent) (plist-get session :id)))

(defun lispm-agents--read-state ()
  "Return the plist the state file holds, or nil without one.
A file holding no such plist is moved aside, to the same name ending in
.unreadable, and said so, for the next write would replace it."
  (let ((file lispm-agents-state-file))
    (when (file-readable-p file)
      (let ((state (with-temp-buffer
                     (insert-file-contents file)
                     (condition-case nil (read (current-buffer)) (error 'unreadable)))))
        (if (and (plistp state) (listp (plist-get state :archived)))
            state
          (let ((aside (concat file ".unreadable")))
            (rename-file file aside t)
            (message (lispm-tr "Cannot read %s; moved to %s")
                     (abbreviate-file-name file) (abbreviate-file-name aside)))
          nil)))))

(defun lispm-agents--archived-keys ()
  "Return the archived session keys, reading the state file on first use."
  (when (eq lispm-agents--archived 'unloaded)
    (setq lispm-agents--archived
          (plist-get (lispm-agents--read-state) :archived)))
  lispm-agents--archived)

(defun lispm-agents-archived-p (session)
  "Return non-nil when SESSION is archived."
  (member (lispm-agents--session-key session) (lispm-agents--archived-keys)))

(defun lispm-agents--set-archived (session archived)
  "Mark SESSION ARCHIVED or not and persist the archived set."
  (let ((key (lispm-agents--session-key session))
        (keys (lispm-agents--archived-keys)))
    (setq lispm-agents--archived (if archived
                                     (cons key (delete key keys))
                                  (delete key keys)))
    (let ((part (concat lispm-agents-state-file ".part")))
      (with-temp-file part
        (let ((print-length nil) (print-level nil))
          (prin1 (list :archived lispm-agents--archived) (current-buffer))))
      ;; Renamed into place, so the file is there whole or not at all.
      (rename-file part lispm-agents-state-file t))))

(defun lispm-agents--rename (session title)
  "Rename SESSION to TITLE the way its own agent records names, see the
:rename of `lispm-agents-define'.  This list then shows TITLE."
  (let ((agent (plist-get session :agent)))
    (funcall (or (plist-get (lispm-agents-props agent) :rename)
                 (user-error (lispm-tr "%s sessions cannot be renamed") (lispm-agents-name agent)))
             session title)))

(defun lispm-agents--session-files (session)
  "Return every file and directory recording SESSION, as its agent's :files
says; none for an agent keeping its sessions in a store of its own."
  (when-let* ((files (plist-get (lispm-agents-props (plist-get session :agent)) :files)))
    (funcall files session)))

(defun lispm-agents--session-transcript (session)
  "Return a file holding SESSION's transcript: what its agent's :transcript
makes, else its :file."
  (if-let* ((transcript (plist-get (lispm-agents-props (plist-get session :agent)) :transcript)))
      (funcall transcript session)
    (plist-get session :file)))

(defun lispm-agents--session-running-p (session)
  "Return non-nil when SESSION runs in a live agent terminal."
  (lispm-agents-session-buffer (plist-get session :id)))

(defun lispm-agents-rename-session (session)
  "Prompt for a new title of SESSION and rename it."
  (let ((title (string-trim (read-string (lispm-tr "Rename session: ")
                                         (plist-get session :title)))))
    (when (string-empty-p title)
      (user-error (lispm-tr "Title must not be empty")))
    (lispm-agents--rename session title)
    (run-hooks 'lispm-agents-sessions-changed-hook)))

(defun lispm-agents-toggle-archive-session (session)
  "Archive SESSION, or unarchive it when it is archived."
  (lispm-agents--set-archived session (not (lispm-agents-archived-p session)))
  (run-hooks 'lispm-agents-sessions-changed-hook))

(defun lispm-agents-delete-session (session)
  "Delete SESSION after confirmation.
The files recording it go to the system trash; an agent keeping its
sessions in a store of its own deletes it there itself, see the :delete of
`lispm-agents-define'.  A session that is running in a terminal
cannot be deleted.  `lispm-agents-confirm-function' asks."
  (when (lispm-agents--session-running-p session)
    (user-error (lispm-tr "Session is running; exit its terminal first")))
  (let* ((agent (plist-get session :agent))
         (name (lispm-agents-name agent))
         (delete (plist-get (lispm-agents-props agent) :delete))
         (files (unless delete (lispm-agents--session-files session)))
         (title (lispm-agents-one-line (plist-get session :title) 40)))
    (when (funcall lispm-agents-confirm-function
                   (if delete
                       (lispm-tr "Delete the %s session “%s”?" name title)
                     (lispm-tr "Move the %s session “%s” to the Trash?" name title))
                   (if delete
                       (lispm-tr "%s removes it from its store; this cannot be undone." name)
                     (lispm-tr-n "Its %d file goes to the Trash." "Its %d files go to the Trash."
                                 (length files)))
                   (if delete (lispm-tr "Delete") (lispm-tr "Move to Trash")))
      (if delete
          (funcall delete session)
        (dolist (file files)
          (move-file-to-trash file)
          (remhash file lispm-agents--session-cache)))
      (lispm-agents--set-archived session nil)
      (run-hooks 'lispm-agents-sessions-changed-hook))))

(defun lispm-agents--in-main-window (function &rest args)
  "Call FUNCTION with ARGS in the main window, see
`lispm-agents-main-window-function'."
  (select-window (funcall lispm-agents-main-window-function))
  (apply function args))

(defun lispm-agents-session-act (action session)
  "Perform the context menu ACTION on SESSION."
  (pcase action
    ('resume (lispm-agents-resume-session session))
    ('rename (lispm-agents-rename-session session))
    ('archive (lispm-agents-toggle-archive-session session))
    ('delete (lispm-agents-delete-session session))
    ('copy (kill-new (plist-get session :id))
           (message (lispm-tr "Copied %s") (plist-get session :id)))
    ('transcript (lispm-agents--in-main-window #'find-file (lispm-agents--session-transcript session)))
    ('directory (lispm-agents--in-main-window #'dired (plist-get session :cwd)))))

;;;; Embark actions on listed sessions

(defun lispm-agents-session-copy-id (label)
  "Copy the id of the agent session listed under LABEL."
  (lispm-agents-session-act 'copy (lispm-agents--session-at label)))

(defun lispm-agents-session-open-transcript (label)
  "Open the transcript of the agent session listed under LABEL."
  (lispm-agents-session-act 'transcript (lispm-agents--session-at label)))

(defun lispm-agents-session-open-directory (label)
  "Open the working directory of the agent session listed under LABEL."
  (lispm-agents-session-act 'directory (lispm-agents--session-at label)))

(defun lispm-agents-session-resume (label)
  "Resume the agent session listed under LABEL."
  (lispm-agents-session-act 'resume (lispm-agents--session-at label)))

(defvar-keymap lispm-agents-session-embark-map
  :doc "Embark actions for agent sessions in the session list."
  "RET" #'lispm-agents-session-resume
  "w" #'lispm-agents-session-copy-id
  "f" #'lispm-agents-session-open-transcript
  "d" #'lispm-agents-session-open-directory)

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
it to MAIN.  Return nil when DIRECTORY is missing or outside any repository."
  (when-let* (((file-directory-p directory))
              (root (locate-dominating-file directory ".git")))
    (let ((dotgit (expand-file-name ".git" root)))
      (or (and (file-regular-p dotgit)
               (with-temp-buffer
                 (insert-file-contents dotgit)
                 (when (re-search-forward "^gitdir: \\(.+?\\)/\\.git/worktrees/" nil t)
                   (match-string 1))))
          root))))

(defvar lispm-agents--root-cache (make-hash-table :test #'equal)
  "Memoized project roots keyed by working directory.")

(defun lispm-agents-project-root (directory)
  "Return the project root of working DIRECTORY, or `chats' for chat sessions.
An agent's worktree known to belong to a repository resolves to it, git
checkouts and their subdirectories to the main checkout root; other
directories stand alone."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (with-memoization (gethash dir lispm-agents--root-cache)
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

(defun lispm-agents--resolve-roots (directories)
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
A project is the root from `lispm-agents--resolve-roots', so worktrees
and subdirectories join their repository and chat locations share one `chats'
group.  Return a list of plists
  (:key KEY :dir DIR :chats CHATS :running BUFFERS :sessions SESSIONS :time T)
where KEY folds case (macOS paths are case-insensitive), DIR is the root for
display, and T is the latest activity.  Projects with running terminals come
first, then by latest activity, and the chats group last.  SESSIONS are
newest first and keep that order inside each group."
  (let* ((directory-of (lambda (item)
                         (if (bufferp item)
                             (buffer-local-value 'default-directory item)
                           (plist-get item :cwd))))
         (roots (lispm-agents--resolve-roots
                 (delete-dups (mapcar directory-of (append running sessions)))))
         (groups (make-hash-table :test #'equal))
         (order nil))
    (cl-flet ((group (item)
                (let* ((root (cdr (assoc (funcall directory-of item) roots)))
                       (key (if (eq root 'chats) 'chats (downcase root))))
                  (or (gethash key groups)
                      (progn
                        (push key order)
                        (puthash key
                                 (list :key key
                                       :dir (if (eq root 'chats) "~"
                                              (lispm-agents--directory-key root))
                                       :chats (eq root 'chats)
                                       :running nil :sessions nil :time 0)
                                 groups))))))
      (dolist (buffer running)
        (let ((group (group buffer)))
          (plist-put group :running (cons buffer (plist-get group :running)))))
      (dolist (session sessions)
        (let ((group (group session)))
          (plist-put group :sessions (cons session (plist-get group :sessions)))
          (plist-put group :time (max (plist-get group :time)
                                      (plist-get session :time))))))
    (sort (mapcar (lambda (key)
                    (let ((group (gethash key groups)))
                      (plist-put group :running (nreverse (plist-get group :running)))
                      (plist-put group :sessions (nreverse (plist-get group :sessions)))))
                  order)
          (lambda (a b)
            (let ((ca (plist-get a :chats)) (cb (plist-get b :chats))
                  (ra (and (plist-get a :running) t))
                  (rb (and (plist-get b :running) t)))
              (cond ((not (eq ca cb)) cb)
                    ((not (eq ra rb)) ra)
                    (t (> (plist-get a :time) (plist-get b :time)))))))))

(defun lispm-agents-running-buffers ()
  "Return the live agent terminals, most recently used first."
  (seq-filter (lambda (buffer)
                (and (lispm-agents-buffer-p buffer) (lispm-agents-running-p buffer)))
              (buffer-list)))

(defun lispm-agents-running-ids (buffers)
  "Return the ids of the sessions agent terminal BUFFERS run, where known.
Ids are compared whole: Codex ids are time-ordered, so sessions begun within
about a minute of each other share their leading characters."
  (delq nil (mapcar (lambda (buffer)
                      (buffer-local-value 'lispm-agents-buffer-session-id buffer))
                    buffers)))

(defun lispm-agents-session-buffer (id)
  "Return the running agent terminal of the session with ID, or nil."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'lispm-agents-buffer-session-id buffer) id))
            (lispm-agents-running-buffers)))

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

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(lispm-agents-session . lispm-agents-session-embark-map)))

(provide 'lispm-agents)
;;; lispm-agents.el ends here
