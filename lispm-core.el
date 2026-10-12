;;; lispm-core.el --- Agent object model for lispm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The object model.  A role is a kind of agent: instructions, tools, a
;; permission policy saying which tool classes run freely, need approval
;; or are forbidden, and a model.  An agent is one conversation (one
;; session) carried on by a role in one project: a value inside the
;; running Emacs, not a process outside it.  It has
;;
;;   - an identity: name, session id, role and project root;
;;   - the capabilities of its role;
;;   - runtime state: its session buffer (an org transcript), the
;;     request in flight, an inbox of pending inputs, tool calls awaiting
;;     approval, and a journal of the edits it made, per turn.
;;
;; Nothing in this file talks to the network or touches buffers; see
;; lispm-runtime.el for the execution loop.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup lispm nil
  "An Emacs-native agent runtime."
  :group 'tools
  :prefix "lispm-")

(defcustom lispm-directory (locate-user-emacs-file "lispm/")
  "Directory holding agent memory, task lists and session transcripts.
It is kept under git (see `lispm-git'), so every change an agent
makes to its memory, and every turn of every session, is a commit."
  :type 'directory)

(defcustom lispm-backends
  '((rotom :api openai :url "http://127.0.0.1:14550/v1" :model "gpt-5.5"
           :features (streaming reasoning-effort))
    (rotom-messages :api anthropic :url "http://127.0.0.1:14550" :model "gpt-5.5"
                    :features (streaming tool-errors))
    (openai :api openai :url "https://api.openai.com/v1" :model "gpt-5.5"
            :key openai
            :features (streaming reasoning-effort max-completion-tokens stream-usage))
    (anthropic :api anthropic :url "https://api.anthropic.com" :model "claude-opus-5-5"
               :key anthropic
               :features (streaming adaptive-thinking fallbacks eager-input tool-errors)))
  "Model endpoints, by name.
Each entry is (NAME . PLIST) with keys
  :api         the API shape the endpoint speaks: `openai' (Chat
               Completions; requests go to URL/chat/completions) or
               `anthropic' (Messages; requests go to URL/v1/messages)
  :url         base URL, as the respective official SDKs take it
  :model       default model for agents that do not name one
  :max-tokens  output limit, overriding `lispm-max-tokens'
  :key         nil (no key), a string, a function returning one,
               `openai' (OPENAI_API_KEY or auth-source),
               `anthropic' (ANTHROPIC_API_KEY or auth-source) or
               `auth-source' (looked up by the URL's host)
  :key-env     name of an environment variable holding the key
  :features    what to use of the endpoint.  For both shapes:
               `streaming' (server-sent events; without it responses
               arrive whole).  For `openai': `reasoning-effort' (send
               the agent's effort), `max-completion-tokens' (name the
               output limit so, as OpenAI's own API requires) and
               `stream-usage' (ask for token usage while streaming).
               For `anthropic': `adaptive-thinking' (progress-update
               thinking blocks, preserved-thinking binding),
               `fallbacks' (server-side refusal fallbacks),
               `eager-input' (tool input streamed unbuffered) and
               `tool-errors' (the is_error flag on tool results;
               without it, failures are marked in the text).
The default, `rotom', is a local gateway serving many upstream models
\(GPT, Grok, ...) behind both shapes: `rotom' is its Chat Completions
side, `rotom-messages' its Messages side.  Use rotom 1.5.15 or later;
earlier versions lose the arguments of parallel tool calls when
streaming Messages (RyanKung/rotom#18) and reject is_error (#19)."
  :type '(alist :key-type symbol :value-type plist))

(defcustom lispm-backend 'rotom
  "Backend for agents that do not name one; a key of `lispm-backends'."
  :type 'symbol)

(defcustom lispm-model nil
  "Default model for agents that do not name one.
nil means the backend's own :model."
  :type '(choice (const :tag "Backend default" nil) string))

(defcustom lispm-effort "medium"
  "Default effort level.
Effort is the main control over how much the model thinks, and so
over latency and cost."
  :type '(choice (const "low") (const "medium") (const "high")
                 (const "xhigh") (const "max")))

(defcustom lispm-max-tokens 64000
  "Maximum output tokens per model response, thinking included."
  :type 'integer)

(defcustom lispm-max-steps 60
  "Maximum number of model requests within one turn."
  :type 'integer)

(defcustom lispm-default-policy
  '((read . auto) (memory . auto) (comm . auto) (write . ask) (exec . ask))
  "Default approval policy: an alist from permission class to action.
Classes are `read' (inspect files, buffers and Emacs state),
`memory' (the agent's own memory and tasks), `comm' (talk to other
agents), `write' (change files or buffers) and `exec' (run shell
commands or evaluate Lisp).  Actions are `auto' (run at once), `ask'
\(wait for the user's approval) and `deny'.  An agent's :policy
overrides entries here."
  :type '(alist :key-type symbol
                :value-type (choice (const auto) (const ask) (const deny))))

(defvar lispm-current-agent nil
  "The agent on whose behalf the running code acts.
Bound while a tool executes, so tools, and hooks they trigger, know
who is acting.")

(defvar lispm-state-change-functions nil
  "Abnormal hook run with an agent whenever its state changes.")

(defvar lispm-system-prompt-functions nil
  "Functions contributing sections to a new session's system prompt.
Each is called with the agent and returns a string or nil.  The
prompt is frozen when the session starts: changing it later would
invalidate the model's cached prefix.")

(defvar lispm-session-end-functions nil
  "Abnormal hook run with an agent whose session is closing.")

(defvar lispm-turn-end-functions nil
  "Abnormal hook run with an agent and its final reply after each turn.")

;;;; Roles and agents
;;
;; A role is a kind of agent: instructions, tools, permission policy and
;; model.  An agent is one conversation (one session) carried on by a
;; role in one project, the way a process runs a program.  Starting an
;; agent in a project is what starting Claude Code or Codex there is;
;; several can run at once, and a recorded one can be resumed.  Agents
;; declared with `lispm-defagent' are standing: they keep one
;; conversation going across Emacs sessions, and hooks and timers wake
;; them up.

(defcustom lispm-default-role 'assistant
  "Role of agents started without choosing one."
  :type 'symbol)

(cl-defstruct (lispm-role (:constructor lispm-role--create)
                          (:copier nil))
  "A kind of agent: the template agents are started from."
  name description instructions tools policy backend model effort)

(defvar lispm--roles nil
  "Alist of (NAME . ROLE) in definition order.")

(cl-defstruct (lispm-agent (:constructor lispm-agent--create)
                           (:copier nil))
  "An agent: one conversation, in one project, carried on by a role."
  ;; Identity.
  name                       ; symbol, unique among live agents
  id                         ; session id, kept across continuation files
  role                       ; name of the role it was started from
  root                       ; project directory it works in
  standing                   ; non-nil for agents declared with `lispm-defagent'
  ;; Capabilities, copied from its role.
  description instructions tools policy backend model effort
  ;; Runtime state.
  (state 'idle)              ; idle, requesting, acting, waiting
  status                     ; short description of the current activity
  buffer                     ; session buffer: the org transcript
  view                       ; chat buffer showing the session, if any
  process                    ; request in flight
  inbox                      ; FIFO of pending inputs (plists)
  (pending (make-hash-table :test #'equal)) ; tool-use id -> call plist
  dispatching                ; non-nil while a response's calls are dispatched
  turn-callbacks             ; called with the final reply when the turn ends
  last-reply                 ; text of the latest assistant message
  (steps 0)                  ; model requests in the current turn
  (turn 0)                   ; turn counter
  journal                    ; alist TURN -> edit snapshots
  allowed                    ; tool names approved for this Emacs session
  waiting-on                 ; agent this one awaits (ask_agent)
  compacting                 ; non-nil while the session is handed over
  triggers                   ; hook and timer subscriptions
  (usage (list 0 0 0)))      ; input, output, cache-read tokens

(defvar lispm--agents nil
  "Alist of (NAME . AGENT) of the live agents, oldest first.")

(defun lispm-roles ()
  "Return all defined roles."
  (mapcar #'cdr lispm--roles))

(defun lispm-get-role (role &optional noerror)
  "Return the role designated by ROLE: a role, a symbol or a string.
Signal an error if there is none, unless NOERROR."
  (cond ((lispm-role-p role) role)
        ((alist-get (if (stringp role) (intern role) role) lispm--roles))
        (noerror nil)
        (t (user-error "No role named %s" role))))

(defun lispm-agents ()
  "Return the live agents."
  (mapcar #'cdr lispm--agents))

(defun lispm-get-agent (agent &optional noerror)
  "Return the agent designated by AGENT: an agent, a name or a session id.
Signal an error if there is none, unless NOERROR."
  (cond ((lispm-agent-p agent) agent)
        ((and (symbolp agent) (alist-get agent lispm--agents)))
        ((and (stringp agent)
              (or (alist-get (intern agent) lispm--agents)
                  (seq-find (lambda (a) (equal (lispm-agent-id a) agent)) (lispm-agents)))))
        (noerror nil)
        (t (user-error "No agent named %s" agent))))

(defun lispm--apply-role (agent role)
  "Give AGENT the capabilities of ROLE."
  (setf (lispm-agent-role agent) (lispm-role-name role)
        (lispm-agent-description agent) (lispm-role-description role)
        (lispm-agent-instructions agent) (lispm-role-instructions role)
        (lispm-agent-tools agent) (lispm-role-tools role)
        (lispm-agent-policy agent) (lispm-role-policy role)
        (lispm-agent-backend agent) (lispm-role-backend role)
        (lispm-agent-model agent) (lispm-role-model role)
        (lispm-agent-effort agent) (lispm-role-effort role))
  agent)

(defun lispm-register-role (name &rest props)
  "Define or redefine the role NAME from PROPS.
PROPS are :description, :instructions, :tools (a list of tool names,
or t for all), :policy, :backend, :model and :effort.  Live agents of
the role take on the new definition at once."
  (let ((role (or (alist-get name lispm--roles)
                  (let ((new (lispm-role--create :name name)))
                    (setq lispm--roles (append lispm--roles (list (cons name new))))
                    new))))
    (setf (lispm-role-description role) (plist-get props :description)
          (lispm-role-instructions role) (plist-get props :instructions)
          (lispm-role-tools role) (if (plist-member props :tools) (plist-get props :tools) t)
          (lispm-role-policy role) (plist-get props :policy)
          (lispm-role-backend role) (plist-get props :backend)
          (lispm-role-model role) (plist-get props :model)
          (lispm-role-effort role) (plist-get props :effort))
    (dolist (agent (lispm-agents))
      (when (eq (lispm-agent-role agent) name)
        (lispm--apply-role agent role)))
    role))

(defmacro lispm-defrole (name &rest props)
  "Define a role NAME.
An optional leading string is its one-line description.  The other
PROPS are as for `lispm-register-role'.

  (lispm-defrole reviewer
    \"Reviews changes for bugs; never edits.\"
    :tools \='(read_file search_files list_directory recall))"
  (declare (indent 1) (doc-string 2))
  (when (stringp (car props))
    (setq props (cons :description props)))
  `(lispm-register-role ',name ,@props))

(defun lispm--new-id ()
  "A fresh session id: the time, and random digits."
  (format "%s-%04x" (format-time-string "%Y%m%d-%H%M%S") (random #x10000)))

(defun lispm--project-name (root)
  "Short name of the project at ROOT."
  (let ((dir (directory-file-name (expand-file-name root))))
    (if (equal dir (directory-file-name (expand-file-name "~")))
        "~"
      (file-name-nondirectory dir))))

(defun lispm--free-name (base)
  "BASE, or BASE<N>, whichever no live agent is called; a symbol."
  (let ((name base) (n 1))
    (while (alist-get (intern name) lispm--agents)
      (setq name (format "%s<%d>" base (cl-incf n))))
    (intern name)))

(cl-defun lispm-make-agent (role root &key name id standing)
  "Make a live agent of ROLE working in ROOT, and return it.
NAME defaults to ROLE@PROJECT, made unique; ID to a fresh session id."
  (let* ((role (lispm-get-role role))
         (agent (lispm-agent--create
                 :name (or name (lispm--free-name
                                 (format "%s@%s" (lispm-role-name role)
                                         (lispm--project-name root))))
                 :id (or id (lispm--new-id))
                 :root (file-name-as-directory (expand-file-name root))
                 :standing standing)))
    (lispm--apply-role agent role)
    (setq lispm--agents (append lispm--agents
                                (list (cons (lispm-agent-name agent) agent))))
    agent))

(defun lispm-forget-agent (agent)
  "Remove AGENT from the live agents."
  (setq lispm--agents (seq-remove (lambda (cell) (eq (cdr cell) agent)) lispm--agents)))

(defun lispm-register-agent (name &rest props)
  "Declare the standing agent NAME and return it.
PROPS are :root (default home) and either :role, the name of a role,
or role properties as for `lispm-register-role', which then define a
role NAME.  Redeclaring a live agent keeps its runtime state."
  (let* ((role (if (plist-get props :role)
                   (lispm-get-role (plist-get props :role))
                 (apply #'lispm-register-role name
                        (cl-loop for (key value) on props by #'cddr
                                 unless (memq key '(:role :root))
                                 append (list key value)))))
         (root (file-name-as-directory (expand-file-name (or (plist-get props :root) "~/"))))
         (agent (or (alist-get name lispm--agents)
                    (lispm-make-agent role root :name name :standing t))))
    (setf (lispm-agent-root agent) root
          (lispm-agent-standing agent) t)
    (lispm--apply-role agent role)))

(defmacro lispm-defagent (name &rest props)
  "Declare a standing agent NAME: one conversation that goes on.
An optional leading string is its one-line description.  The other
PROPS are as for `lispm-register-agent'.

  (lispm-defagent coder
    \"Writes and fixes Emacs Lisp.\"
    :root \"~/src/lispm/\"
    :instructions \"Keep changes small; run the tests after editing.\"
    :policy \='((write . auto)))"
  (declare (indent 1) (doc-string 2))
  (when (stringp (car props))
    (setq props (cons :description props)))
  `(lispm-register-agent ',name ,@props))

(defun lispm-agent-backend* (agent)
  "Plist of the backend AGENT uses."
  (let ((name (or (lispm-agent-backend agent) lispm-backend)))
    (or (alist-get name lispm-backends)
        (user-error "Unknown backend %s (see `lispm-backends')" name))))

(defun lispm-agent-model* (agent)
  "Model AGENT uses."
  (or (lispm-agent-model agent) lispm-model
      (plist-get (lispm-agent-backend* agent) :model)))

(defun lispm-agent-effort* (agent)
  "Effort level AGENT uses."
  (or (lispm-agent-effort agent) lispm-effort))

(defun lispm--slug (directory)
  "A file name standing for DIRECTORY: its path with separators replaced."
  (replace-regexp-in-string "[^[:alnum:]._]" "-"
                            (directory-file-name (expand-file-name directory))))

(defun lispm-project-directory (root)
  "Directory of the sessions, tasks and memory of the project at ROOT.
Created on demand, under `lispm-directory', not in the project."
  (let ((dir (expand-file-name (concat "projects/" (lispm--slug root) "/") lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-role-directory (role)
  "Directory of the memory of ROLE (a name), created on demand."
  (let ((dir (expand-file-name (format "roles/%s/" role) lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-policy-action (agent permission)
  "Action AGENT's policy prescribes for tools of class PERMISSION."
  (or (alist-get permission (lispm-agent-policy agent))
      (alist-get permission lispm-default-policy)
      'ask))

(defun lispm-set-state (agent state &optional status)
  "Set AGENT's STATE and STATUS, and tell whoever is watching."
  (setf (lispm-agent-state agent) state
        (lispm-agent-status agent) status)
  (run-hook-with-args 'lispm-state-change-functions agent)
  (force-mode-line-update t))

(defun lispm-agent-busy-p (agent)
  "Non-nil if AGENT is in the middle of a turn."
  (not (eq (lispm-agent-state agent) 'idle)))

;;;; Utilities

(defun lispm--json-encode (object)
  "Encode OBJECT as a JSON string (decoded text, not bytes)."
  (decode-coding-string (json-serialize object) 'utf-8))

(defun lispm--json-decode (string)
  "Decode the JSON STRING.
Objects become alists with symbol keys, arrays vectors, null `:null'
and false `:false', so a decoded value encodes back to the same JSON."
  (json-parse-string string :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun lispm--json-pretty (object &optional indent)
  "Encode OBJECT as indented JSON, starting at column INDENT."
  (let* ((indent (or indent 0))
         (pad (make-string (+ indent 2) ?\s))
         (close (make-string indent ?\s)))
    (cond
     ((and (consp object) (consp (car object)))
      (concat "{\n"
              (mapconcat (lambda (kv)
                           (concat pad (lispm--json-encode (symbol-name (car kv)))
                                   ": " (lispm--json-pretty (cdr kv) (+ indent 2))))
                         object ",\n")
              "\n" close "}"))
     ((null object) "{}")
     ((and (vectorp object)
           (cl-some (lambda (x) (or (consp x) (vectorp x))) object))
      (concat "[\n"
              (mapconcat (lambda (x) (concat pad (lispm--json-pretty x (+ indent 2))))
                         object ",\n")
              "\n" close "]"))
     ((vectorp object) (lispm--json-encode object))
     (t (let ((s (lispm--json-encode (vector object))))
          (substring s 1 -1))))))

(defun lispm--sse-split (text)
  "Split TEXT, the unparsed part of a server-sent event stream, into events.
Return (PAYLOADS . REST): the data of each complete event, in order,
and the incomplete remainder."
  (let ((start 0) payloads)
    (while (string-match "\r?\n\r?\n" text start)
      (let ((event (substring text start (match-beginning 0))))
        (setq start (match-end 0))
        (let ((data (mapconcat (lambda (line) (string-remove-prefix " " (substring line 5)))
                               (seq-filter (lambda (line) (string-prefix-p "data:" line))
                                           (split-string event "\r?\n"))
                               "\n")))
          (unless (string-empty-p data)
            (push data payloads)))))
    (cons (nreverse payloads) (substring text start))))

(defun lispm--truncate (string limit)
  "Shorten STRING to about LIMIT characters, keeping its head and tail."
  (if (<= (length string) limit)
      string
    (let ((head (floor (* limit 0.6)))
          (tail (floor (* limit 0.4))))
      (concat (substring string 0 head)
              (format "\n\n[... %d characters omitted ...]\n\n"
                      (- (length string) head tail))
              (substring string (- (length string) tail))))))

(defun lispm--oneline (string &optional width)
  "STRING collapsed to a single line of at most WIDTH (default 72) columns."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim (or string "")))
   (or width 72) nil nil "…"))

(defun lispm--file-text (file)
  "Contents of FILE, from its buffer if one visits it."
  (if-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun lispm--diff (old new label)
  "Unified diff from string OLD to string NEW, with file LABEL."
  (if (equal old new)
      "(no changes)"
    (let ((a (make-temp-file "lispm-a"))
          (b (make-temp-file "lispm-b"))
          (coding-system-for-write 'utf-8)
          (coding-system-for-read 'utf-8))
      (unwind-protect
          (progn
            (write-region old nil a nil 'silent)
            (write-region new nil b nil 'silent)
            (with-temp-buffer
              (call-process "diff" nil t nil "-u"
                            "--label" (concat "a/" label)
                            "--label" (concat "b/" label) a b)
              (buffer-string)))
        (delete-file a)
        (delete-file b)))))

(provide 'lispm-core)
;;; lispm-core.el ends here
