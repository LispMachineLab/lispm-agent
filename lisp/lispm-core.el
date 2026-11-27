;;; lispm-core.el --- Agent object model for lispm  -*- lexical-binding: t; -*-

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
(require 'lispm-util)
(require 'lispm-secrets)

(defcustom lispm-directory (locate-user-emacs-file "lispm/")
  "Directory holding agent memory, task lists and session transcripts.
It is kept under git (see `lispm-git'), so every change an agent
makes to its memory, and every turn of every session, is a commit."
  :type 'directory
  :group 'lispm)

(defcustom lispm-backends
  '((openai :api openai :url "https://api.openai.com/v1" :model "gpt-5.5"
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
  :models      other models it serves, offered where a model is chosen
               (see `lispm-model-choices')
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
The presets are the two first-party APIs.  Any other server speaking
either shape, local (Ollama, vLLM, LM Studio, llama.cpp) or hosted
\(OpenRouter, a gateway), takes an entry of its own."
  :type '(alist :key-type symbol :value-type plist)
  :group 'lispm)

(defcustom lispm-backend nil
  "Backend for agents that do not name one; a key of `lispm-backends'.
nil means the first of `lispm-backends' whose key is at hand: a literal
one, one in the environment (OPENAI_API_KEY for `openai',
ANTHROPIC_API_KEY for `anthropic', or the variable an entry names with
:key-env), or one in the secret store (see `lispm-secret-get').  A
backend that needs no key, such as a server on this machine, must be
named."
  :type '(choice (const :tag "The first with a key at hand" nil)
                 symbol)
  :group 'lispm)

(defconst lispm-key-sources
  '((openai "OPENAI_API_KEY" "api.openai.com")
    (anthropic "ANTHROPIC_API_KEY" "api.anthropic.com"))
  "The :key values standing for a first-party API's key: where to find it.
Each entry is (KEY VARIABLE HOST): the environment VARIABLE, else the
auth-source entry for HOST.")

(defcustom lispm-model nil
  "Default model for agents that do not name one.
nil means the backend's own :model."
  :type '(choice (const :tag "Backend default" nil) string)
  :group 'lispm)

(defcustom lispm-effort "medium"
  "Default effort level.
Effort is the main control over how much the model thinks, and so
over latency and cost."
  :type '(choice (const "low") (const "medium") (const "high")
                 (const "xhigh") (const "max"))
  :group 'lispm)

(defcustom lispm-max-tokens 64000
  "Maximum output tokens per model response, thinking included."
  :type 'integer
  :group 'lispm)

(defcustom lispm-max-steps 60
  "Maximum number of model requests within one turn."
  :type 'integer
  :group 'lispm)

(defcustom lispm-default-policy
  '((read . auto) (memory . auto) (comm . auto) (lisp . auto) (write . ask) (shell . ask)
    (exec . ask) (mcp . ask) (web . ask))
  "Default approval policy: an alist from permission class to action.
Classes are `read' (inspect files, buffers and Emacs state),
`memory' (the agent's own memory and tasks), `comm' (talk to other
agents), `lisp' (run Lisp programs whose effects are tool calls, each
under the policy: run_lisp), `write' (change files or buffers),
`shell' (run shell commands, on the machine of the agent's root),
`exec' (evaluate any Lisp in Emacs), `mcp' (tools of MCP servers,
unless `lispm-mcp-servers' gives a server's tools another class) and
`web' \(search the web and fetch pages).  Actions are `auto' (run at
once), `ask' \(wait for the user's approval) and `deny'.  An agent's
:policy overrides entries here.  A policy that does not name `shell'
gives it the action it gives `exec', the class of shell commands
until they had one of their own (see `lispm--permission-fallbacks')."
  :type '(alist :key-type symbol
                :value-type (choice (const auto) (const ask) (const deny)))
  :group 'lispm)

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
;;
;; A character is someone, not a kind: a name, a soul, a memory and a
;; past of its own, which last.  It has conversations in many places,
;; one going on in each, and is the same in all of them: each is a
;; standing agent of its role, the character's agent at that place
;; (`lispm-character-agent'), whose identity is the character's.

(defcustom lispm-default-role 'assistant
  "Role of agents started without choosing one."
  :type 'symbol
  :group 'lispm)

(cl-defstruct (lispm-role (:constructor lispm-role--create)
                          (:copier nil))
  "A kind of agent: the template agents are started from."
  name description instructions tools policy backend model effort)

(defvar lispm--roles nil
  "Alist of (NAME . ROLE) in definition order.")

(defvar lispm--role-files nil
  "Alist of (NAME . FILE): the roles read from role files (lispm-roles.el).
A role defined again in Lisp is no longer one of them.")

(cl-defstruct (lispm-character (:constructor lispm-character--create)
                               (:copier nil))
  "Someone: an identity that lasts, with a conversation in each place it is.
Its conversations are agents (see `lispm-character-agent'); its
memory, its past and what it learned of itself are in its directory
\(see `lispm-character-directory')."
  name                       ; symbol naming its directory and its agents, as `ada'
  title                      ; the name it goes by, as "Ada"
  description                ; one line: what other agents are told of it
  soul                       ; who it is: nature, voice, values; the user's words
  avatar                     ; its picture, a file, or nil
  role                       ; role whose instructions, tools and policy it works with
  backend                    ; a backend of `lispm-backends' it uses, or nil
  endpoint                   ; a model endpoint of its own, as an entry of `lispm-backends'
  model effort
  skill                      ; for an app: the name of its skill, which is what it is for
  file                       ; the file it was read from, if any
  extensions)                ; plist of optional capability properties

(defconst lispm-character--core-keys
  '(:title :description :soul :avatar :role :backend :endpoint :model :effort :skill :file)
  "Property keys stored directly on `lispm-character' core slots.")

(defun lispm-character-extension (character key)
  "Return extension property KEY from CHARACTER's extension plist."
  (plist-get (lispm-character-extensions character) key))

(defun lispm-character--set-extension (character key value)
  "Set extension property KEY on CHARACTER to VALUE and return VALUE."
  (let ((plist (copy-sequence (lispm-character-extensions character))))
    (setf (lispm-character-extensions character) (plist-put plist key value))
    value))

(gv-define-setter lispm-character-extension (value character key)
  `(lispm-character--set-extension ,character ,key ,value))

(defvar lispm--characters nil
  "Alist of (NAME . CHARACTER) in definition order.")

(defvar lispm-character-change-functions nil
  "Abnormal hook run when a character is defined, changed or removed.
Each is called with the character's name and `defined' or `removed'.")

;; Basis: (name id role root tools policy ceiling allowed loaded-tools character waiting-on)
;; Span: All tool-execution and policy subjects -- both native conversation sessions
;;       (`lispm-agent') and external bridge / MCP clients (`lispm-principal').
;; Minimality: Every slot is required for tool lookup, root confinement, policy/ceiling
;;             evaluation, session allowance, deferred tool loading, character identity,
;;             or delegation wait-graph tracking; none can be derived from the others.
;; Tightness: External principals cannot hold native conversation buffers, inbox queues,
;;            or turn counters; native `lispm-agent' subtypes extend this basis via
;;            `(:include lispm-principal)'.
(cl-defstruct (lispm-principal (:constructor lispm-principal--create)
                               (:copier nil))
  "A policy and tool-execution principal: native session or external client."
  name id role root tools policy ceiling allowed loaded-tools character waiting-on)

(cl-defstruct (lispm-agent (:include lispm-principal)
                           (:constructor lispm-agent--create)
                           (:copier nil))
  "An agent: one conversation, in one project, carried on by a role."
  ;; Identity (inherits name id role root character waiting-on from `lispm-principal').
  standing                   ; non-nil for agents declared with `lispm-defagent'
  ;; Capabilities, copied from its role (inherits tools policy ceiling from `lispm-principal').
  description instructions backend model effort
  ;; Runtime state (inherits allowed loaded-tools from `lispm-principal').
  (state 'idle)              ; idle, requesting, acting, waiting
  status                     ; short description of the current activity
  activity                   ; that activity as data: VERB or (VERB . OBJECT)
  buffer                     ; session buffer: the org transcript
  view                       ; chat buffer showing the session, if any
  process                    ; request in flight
  inbox                      ; FIFO of pending inputs (plists)
  suspended                  ; requests yielded while their replies are pending
  entry                      ; the input the turn under way handles
  value                      ; a value the turn returns with its reply (return_value)
  (pending (make-hash-table :test #'equal)) ; tool-use id -> call plist
  dispatching                ; non-nil while a response's calls are dispatched
  turn-callbacks             ; called with the final reply when the turn ends
  last-reply                 ; text of the latest assistant message
  (steps 0)                  ; model requests in the current turn
  (turn 0)                   ; turn counter
  journal                    ; alist TURN -> edit snapshots
  snapshots                  ; alist TURN -> snapshots of its project (lispm-journal.el)
  condition                  ; condition its turn stopped at, awaiting a restart
  auto-restarts              ; alist TYPE or (TYPE . CALL) -> restarts chosen by handlers this turn
  attended                   ; non-nil if the user started the current turn
  max-tokens                 ; output limit for the rest of the turn, if raised
  continuing                 ; non-nil: the next request resumes a paused response
  compacting                 ; non-nil while the session is handed over
  context                    ; (TOKENS . BYTES): session size the latest response reported, bytes then
  request-bytes              ; session size in bytes the request in flight was made from
  triggers                   ; hook and timer subscriptions
  (usage (list 0 0 0))       ; input, output, cache-read tokens
  ;; Last, so that code compiled against fewer slots still reads the others.
  system                     ; non-nil for an agent of the whole system, in no project
  parent                     ; session id of the agent that started it, for a helper or an agent started for another
  workspace)                 ; `lispm-workspace' attached to this agent, or nil

(defvar lispm--agents nil
  "Alist of (NAME . AGENT) of the live agents, oldest first.")

(defvar lispm--retired-agents nil
  "Alist of (NAME . SESSION-ID) of retired agents, newest first.")

(defun lispm-roles ()
  "Return all defined roles."
  (mapcar #'cdr lispm--roles))

(defun lispm-get-role (role &optional noerror)
  "Return the role designated by ROLE: a role, a symbol or a string.
Signal an error if there is none, unless NOERROR."
  (cond ((lispm-role-p role) role)
        ((alist-get (if (stringp role) (intern role) role) lispm--roles))
        (noerror nil)
        (t (user-error "%s" (lispm-tr "No role named %s" role)))))

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
        (t (user-error "%s" (lispm-tr "No agent named %s" agent)))))

(defun lispm-principal-apply-role (principal role)
  "Give PRINCIPAL the policy and tool capabilities of ROLE.
When PRINCIPAL is a native `lispm-agent', also copy conversation
settings and apply character overrides, if any."
  (setf (lispm-principal-role principal) (lispm-role-name role)
        (lispm-principal-tools principal) (lispm-role-tools role)
        (lispm-principal-policy principal) (lispm-role-policy role))
  (when (lispm-agent-p principal)
    (setf (lispm-agent-description principal) (lispm-role-description role)
          (lispm-agent-instructions principal) (lispm-role-instructions role)
          (lispm-agent-backend principal) (lispm-role-backend role)
          (lispm-agent-model principal) (lispm-role-model role)
          (lispm-agent-effort principal) (lispm-role-effort role))
    (when-let* ((character (lispm-agent-character* principal)))
      (setf (lispm-agent-description principal) (or (lispm-character-description character)
                                                    (lispm-agent-description principal))
            (lispm-agent-backend principal) (or (lispm-character-backend-name character)
                                                (lispm-agent-backend principal))
            (lispm-agent-model principal) (or (lispm-character-model character)
                                              (lispm-agent-model principal))
            (lispm-agent-effort principal) (or (lispm-character-effort character)
                                               (lispm-agent-effort principal)))))
  principal)

(defun lispm--apply-role (agent role)
  "Give AGENT the capabilities of ROLE, and those of its character, if any.
What a character says of itself, its description and its model,
overrides what its role says."
  (lispm-principal-apply-role agent role))

(cl-defun lispm-make-principal (name root &key id role ceiling character)
  "Create and return a policy principal NAME rooted in ROOT.
ID is its session identifier, ROLE defaults to `lispm-default-role',
CEILING bounds its effective policy, and CHARACTER names an associated
character, if any.  Unlike `lispm-make-agent', this does not register a
live conversation or allocate session state."
  (let* ((role-obj (lispm-get-role (or role lispm-default-role)))
         (principal (lispm-principal--create
                     :name name
                     :id id
                     :root (file-name-as-directory (expand-file-name root))
                     :ceiling ceiling
                     :character character)))
    (lispm-principal-apply-role principal role-obj)))

(defun lispm-register-role (name &rest props)
  "Define or redefine the role NAME from PROPS.
PROPS are :description, :instructions, :tools (a list of tool names,
or t for all), :policy, :backend, :model and :effort, and :file, the
role file it was read from, if it was (see `lispm-role-file').  Live
agents of the role take on the new definition at once."
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
    (setq lispm--role-files (append (and (plist-get props :file)
                                         (list (cons name (plist-get props :file))))
                                    (seq-remove (lambda (entry) (eq (car entry) name))
                                                lispm--role-files)))
    (dolist (agent (lispm-agents))
      (when (eq (lispm-agent-role agent) name)
        (lispm--apply-role agent role)))
    role))

(defun lispm-unregister-role (name)
  "Remove the role NAME, a symbol or a string; return it, or nil if there was none.
Live agents of the role keep what they had from it and go on.  A
session of the role is resumed only once a role of that name is
defined again."
  (let* ((name (if (stringp name) (intern name) name))
         (role (alist-get name lispm--roles))
         (other (lambda (entry) (not (eq (car entry) name)))))
    (setq lispm--roles (seq-filter other lispm--roles)
          lispm--role-files (seq-filter other lispm--role-files))
    role))

(defun lispm-role-file (role)
  "The role file ROLE was read from, or nil if it was defined in Lisp.
ROLE is a role, or the name of one: a symbol or a string."
  (alist-get (cond ((lispm-role-p role) (lispm-role-name role))
                   ((stringp role) (intern role))
                   (t role))
             lispm--role-files))

(defun lispm-role-files ()
  "Return the alist of file-backed roles as `(NAME . FILE)'."
  lispm--role-files)

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

(defun lispm-new-id ()
  "A fresh session id: the time, and random digits."
  (format "%s-%04x" (format-time-string "%Y%m%d-%H%M%S") (random #x10000)))

(defun lispm-project-name (root)
  "Short name of the project at ROOT."
  (let ((dir (directory-file-name (expand-file-name root))))
    (if (equal dir (directory-file-name (expand-file-name "~")))
        "~"
      (file-name-nondirectory dir))))

(defun lispm-free-name (base)
  "BASE, or BASE<N>, whichever no live agent is called; a symbol."
  (let ((name base) (n 1))
    (while (alist-get (intern name) lispm--agents)
      (setq name (format "%s<%d>" base (cl-incf n))))
    (intern name)))

(defun lispm-agent-scope (agent)
  "Return AGENT's canonical ownership scope tag.
One of `system', `character', `standing', `child', or `session'."
  (cond ((lispm-agent-system agent) 'system)
        ((lispm-agent-character agent) 'character)
        ((lispm-agent-standing agent) 'standing)
        ((lispm-agent-parent agent) 'child)
        (t 'session)))

(cl-defun lispm-make-agent (role root &key name id standing system character)
  "Make a live agent of ROLE working in ROOT, and return it.
NAME defaults to ROLE@PROJECT, made unique; ID to a fresh session id.
STANDING non-nil makes it a standing agent, as `lispm-defagent' does.
SYSTEM non-nil makes it an agent of the whole system, as the agent of
a system-wide prompt is: it works in no project, and its memory, tasks
and episodes are the system's (see `lispm-system-directory').
CHARACTER, the name of a character, makes it that character's
conversation (see `lispm-character-agent')."
  (when (and system character)
    (error "An agent cannot be both system and character scoped"))
  (let* ((role (lispm-get-role role))
         (agent (lispm-agent--create
                 :name (or name (lispm-free-name
                                 (format "%s@%s" (lispm-role-name role)
                                         (lispm-project-name root))))
                 :id (or id (lispm-new-id))
                 :root (file-name-as-directory (expand-file-name root))
                 :standing standing
                 :system system
                 :character character)))
    (lispm--apply-role agent role)
    (setq lispm--agents (append lispm--agents
                                (list (cons (lispm-agent-name agent) agent))))
    (lispm-emit-change agent '(agents))
    agent))

(defun lispm-forget-agent (agent)
  "Remove AGENT from the live agents."
  (when (and (lispm-agent-name agent) (lispm-agent-id agent))
    (setf (alist-get (symbol-name (lispm-agent-name agent))
                     lispm--retired-agents nil nil #'equal)
          (lispm-agent-id agent)))
  (setq lispm--agents (seq-remove (lambda (cell) (eq (cdr cell) agent)) lispm--agents))
  (lispm-emit-change agent '(agents)))

(defun lispm-retired-agent-id (name)
  "Return the recorded session id of retired agent NAME, or nil."
  (and (stringp name)
       (alist-get name lispm--retired-agents nil nil #'equal)))

(defun lispm-remember-retired-agent (name id)
  "Record session ID for retired agent NAME and return ID."
  (when (and (stringp name) (stringp id))
    (setf (alist-get name lispm--retired-agents nil nil #'equal) id)))

(defun lispm-rename-agent (agent name)
  "Rename live AGENT to NAME when no other live agent holds that name."
  (let ((sym (cond ((symbolp name) name)
                   ((stringp name) (intern name)))))
    (when (and sym (not (alist-get sym lispm--agents)))
      (when-let* ((cell (rassq agent lispm--agents)))
        (setcar cell sym))
      (setf (lispm-agent-name agent) sym)))
  agent)

(defun lispm-register-agent (name &rest props)
  "Declare the standing agent NAME and return it.
PROPS are :root (default home), :system, non-nil for an agent of the
whole system (see `lispm-make-agent'), and either :role, the name of a
role, or role properties as for `lispm-register-role', which then
define a role NAME.  Redeclaring a live agent keeps its runtime state."
  (let* ((existing (alist-get name lispm--agents))
         (sys (and (plist-get props :system) t)))
    (when (and sys (or (plist-get props :character)
                       (and existing (lispm-agent-character existing))))
      (error "An agent cannot be both system and character scoped"))
    (let* ((role (if (plist-get props :role)
                     (lispm-get-role (plist-get props :role))
                   (apply #'lispm-register-role name
                          (cl-loop for (key value) on props by #'cddr
                                   unless (memq key '(:role :root :system))
                                   append (list key value)))))
           (root (file-name-as-directory (expand-file-name (or (plist-get props :root) "~/"))))
           (agent (or existing
                      (lispm-make-agent role root :name name :standing t :system sys))))
      (setf (lispm-agent-root agent) root
            (lispm-agent-standing agent) t
            (lispm-agent-system agent) sys)
      (lispm--apply-role agent role)
      (lispm-emit-change agent '(root))
      (run-hook-with-args 'lispm-agent-declared-functions agent)
      agent)))

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

(defcustom lispm-system-agent-name 'lispm
  "Name of LispM itself, the system's own agent that the bar talks to."
  :type 'symbol
  :group 'lispm)

(defcustom lispm-system-agent-root "~/"
  "Directory LispM itself works in: relative paths are relative to it."
  :type 'directory
  :group 'lispm)

(defvaralias 'lispm-bar--instructions 'lispm-system-agent-instructions)

(defconst lispm-system-agent-instructions
  "You are LispM itself: the agent of this whole computer and of the Emacs it runs, not of one project. The user reaches you from the LispM bar (a one-line field with a small window for your reply), from a Listener command prompt, from a chat window, or from the LispM terminal; all share this one conversation. Check `[Current LispM scenario]' when present on a turn to know which surface the user is currently using, what windows or buffers are active, and what recent commands ran. You act for the user across Emacs and the computer: find what the system and what is installed can do (search_tools finds their skills, the manuals with their tools), open and change things, introspect live Emacs state when asked about UI behavior, and answer questions. When answering in the LispM bar or inline in a Listener, lead with the answer or with what you did, and keep it short unless asked for more."
  "What LispM itself is told it is, beyond what every agent is told.")

(lispm-defrole system
  "LispM itself: the agent of the whole system, which the LispM bar talks to."
  :instructions lispm-system-agent-instructions)

(defun lispm-system-agent ()
  "LispM itself: the standing agent of the whole system.
It is declared when first needed.  It works in `lispm-system-agent-root',
the home directory, with the role `system', and keeps one conversation
going across Emacs sessions, which the bar and the chat both show.  It
is not one of a project's agents: its memory, tasks and episodes are
the system's (see `lispm-system-directory')."
  (let ((agent (lispm-get-agent lispm-system-agent-name t)))
    (if (and agent (lispm-agent-standing agent))
        agent
      (lispm-register-agent lispm-system-agent-name :role 'system :root lispm-system-agent-root
                            :system t))))

(defun lispm-system-agent-p (agent)
  "Non-nil if AGENT is LispM itself, see `lispm-system-agent'."
  (and (lispm-agent-p agent)
       (lispm-agent-standing agent)
       (eq (lispm-principal-name agent) lispm-system-agent-name)))

;;;; Characters

(defcustom lispm-character-home "~/"
  "Directory a character's own conversation works in.
That is the conversation it has of its own, not in a project: the
one an app of its shows, say.  Its memory and its tasks there are its
own (see `lispm-home-directory')."
  :type 'directory
  :group 'lispm)

(defun lispm-characters ()
  "Return the characters."
  (mapcar #'cdr lispm--characters))

(defun lispm-get-character (character &optional noerror)
  "Return the character CHARACTER designates, or nil.
CHARACTER is a character, a name (a symbol), or a string: its name or
the name it goes by, in any case, with or without a leading @, as
@Ada.  Signal an error if there is none, unless NOERROR."
  (cond ((lispm-character-p character) character)
        ((and (symbolp character) (alist-get character lispm--characters)))
        ((and (stringp character)
              (let ((name (string-remove-prefix "@" (string-trim character))))
                (seq-find (lambda (c)
                            (or (string-equal-ignore-case name (symbol-name (lispm-character-name c)))
                                (and (lispm-character-title c)
                                     (string-equal-ignore-case name (lispm-character-title c)))))
                          (lispm-characters)))))
        (noerror nil)
        (t (user-error "%s" (lispm-tr "No character named %s" character)))))

(defun lispm-agent-character* (agent)
  "The character whose conversation AGENT is, or nil."
  (when-let* ((name (lispm-principal-character agent)))
    (lispm-get-character name t)))

(defun lispm-character-title* (character)
  "The name CHARACTER goes by."
  (or (lispm-character-title character) (symbol-name (lispm-character-name character))))

(defun lispm-character-backend-name (character)
  "Name of the backend CHARACTER's conversations use, or nil for the default.
A character with an endpoint of its own uses it, by the name @NAME."
  (cond ((lispm-character-endpoint character)
         (intern (format "@%s" (lispm-character-name character))))
        ((lispm-character-backend character))))

(defun lispm-character-directory (character)
  "Directory of what CHARACTER is: its file, memory, past; created on demand.
CHARACTER is a character or a name."
  (let ((dir (expand-file-name (format "characters/%s/"
                                       (if (lispm-character-p character)
                                           (lispm-character-name character)
                                         character))
                               lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-register-character (name &rest props)
  "Define or redefine the character NAME from PROPS, and return it.
PROPS are :title, :description, :soul, :avatar, :role (the name of a
role, nil for `lispm-default-role'), :backend, :endpoint, :model,
:effort, :skill and :file (see `lispm-character').  Its live
conversations take on the new definition at once."
  (let ((character (or (alist-get name lispm--characters)
                       (let ((new (lispm-character--create :name name)))
                         (setq lispm--characters (append lispm--characters (list (cons name new))))
                         new))))
    (setf (lispm-character-title character) (plist-get props :title)
          (lispm-character-description character) (plist-get props :description)
          (lispm-character-soul character) (plist-get props :soul)
          (lispm-character-avatar character) (plist-get props :avatar)
          (lispm-character-role character) (plist-get props :role)
          (lispm-character-backend character) (plist-get props :backend)
          (lispm-character-endpoint character) (plist-get props :endpoint)
          (lispm-character-model character) (plist-get props :model)
          (lispm-character-effort character) (plist-get props :effort)
          (lispm-character-skill character) (plist-get props :skill)
          (lispm-character-file character) (plist-get props :file))
    (let (ext seen)
      (cl-loop for (key value) on props by #'cddr
               unless (memq key seen)
               do (push key seen)
                  (when (and value (not (memq key lispm-character--core-keys)))
                    (setq ext (plist-put ext key value))))
      (setf (lispm-character-extensions character) ext))
    (dolist (agent (lispm-agents))
      (when (eq (lispm-agent-character agent) name)
        (when-let* ((role (lispm-get-role (or (lispm-character-role character)
                                              (lispm-agent-role agent))
                                          t)))
          (lispm--apply-role agent role))))
    (run-hook-with-args 'lispm-character-change-functions name 'defined)
    character))

(defun lispm-unregister-character (name)
  "Remove the character NAME, a symbol or a string; return it, or nil.
Its live conversations go on with what they had from it."
  (let* ((name (if (stringp name) (intern name) name))
         (character (alist-get name lispm--characters)))
    (when character
      (setq lispm--characters (seq-remove (lambda (entry) (eq (car entry) name)) lispm--characters))
      (run-hook-with-args 'lispm-character-change-functions name 'removed))
    character))

(defun lispm-character-root-p (root)
  "Non-nil if ROOT is `lispm-character-home', where characters are at home."
  (equal (file-name-as-directory (expand-file-name root))
         (file-name-as-directory (expand-file-name lispm-character-home))))

(defun lispm-character-home-p (agent)
  "Non-nil if AGENT is a character's own conversation, in no project."
  (and (lispm-agent-character agent) (lispm-character-root-p (lispm-agent-root agent))))

(defun lispm-character-agent (character &optional root)
  "CHARACTER's conversation in ROOT: a standing agent, declared if need be.
ROOT nil, or `lispm-character-home', is its own conversation, named as
the character is; elsewhere it is NAME@PROJECT.  It works with the
character's role, or `lispm-default-role', and resumes the
conversation it had there, if any."
  (let* ((character (lispm-get-character character))
         (name (lispm-character-name character))
         (root (file-name-as-directory (expand-file-name (or root lispm-character-home)))))
    (or (seq-find (lambda (agent)
                    (and (eq (lispm-agent-character agent) name)
                         (lispm-agent-standing agent)
                         (equal (lispm-agent-root agent) root)))
                  (lispm-agents))
        (let ((agent (lispm-make-agent
                      (or (lispm-get-role (lispm-character-role character) t) lispm-default-role)
                      root
                      :name (lispm-free-name
                             (if (lispm-character-root-p root)
                                 (symbol-name name)
                               (format "%s@%s" name (lispm-project-name root))))
                      :standing t
                      :character name)))
          (run-hook-with-args 'lispm-agent-declared-functions agent)
          agent))))

(defun lispm-key-variable (backend)
  "The environment variable that holds BACKEND's API key, or nil."
  (or (plist-get backend :key-env)
      (nth 1 (assq (plist-get backend :key) lispm-key-sources))))

(defun lispm-backend-key-at-hand-p (backend)
  "Non-nil when BACKEND has a key at hand, without asking anyone for it.
A backend that needs no key does not count: it must be named."
  (let ((key (plist-get backend :key))
        (variable (lispm-key-variable backend)))
    (cond
     (variable (and (getenv variable) (not (string-empty-p (getenv variable)))))
     ((null key) nil)
     ((stringp key) (not (string-empty-p key)))
     ((and (consp key) (eq (car key) 'secret))
      (condition-case nil
          (lispm-secret-exists-p (cdr key))
        (error nil)))
     (t nil))))

(defun lispm-backend-name (&optional agent)
  "Name of the backend AGENT uses, or that agents use by default; or nil.
See `lispm-backend'."
  (or (and (lispm-agent-p agent) (lispm-agent-backend agent))
      lispm-backend
      (car (seq-find (lambda (entry) (lispm-backend-key-at-hand-p (cdr entry)))
                     lispm-backends))))

(defun lispm-backend (name)
  "Plist of the backend NAME, or nil if there is none.
An entry of `lispm-backends', or a character's own endpoint, named
@NAME after it (see `lispm-character-backend-name')."
  (or (alist-get name lispm-backends)
      (when-let* ((string (and name (symbolp name) (symbol-name name)))
                  ((string-prefix-p "@" string))
                  (character (alist-get (intern (substring string 1)) lispm--characters)))
        (lispm-character-endpoint character))))

(defun lispm-agent-backend* (agent)
  "Plist of the backend AGENT uses."
  (let ((name (lispm-backend-name agent)))
    (cond ((null name)
           (user-error "No model backend: add one to `lispm-backends', or set \
`lispm-backend' to one of them"))
          ((lispm-backend name))
          (t (user-error "Unknown backend %s (see `lispm-backends')" name)))))

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

(defun lispm-first-line (file)
  "The first line of FILE, trimmed; nil unless FILE is a regular file."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (string-trim (buffer-substring (point-min) (line-end-position))))))

(defun lispm-expand-file-name (file directory)
  "FILE expanded against DIRECTORY, on DIRECTORY's machine.
As `expand-file-name', except that on a remote DIRECTORY (a TRAMP file
name), an absolute FILE that names no machine of its own, such as
/etc/hosts or ~/notes, names a file on DIRECTORY's machine: to a program
there, a shell command or git, that is the file meant."
  (let ((remote (file-remote-p directory)))
    (expand-file-name (if (and remote (file-name-absolute-p file) (not (file-remote-p file)))
                          (concat remote file)
                        file)
                      directory)))

(defun lispm-main-worktree (directory)
  "DIRECTORY, or its counterpart in the main worktree if it is in a linked one.
The worktrees `git worktree add' links to a repository are one project
with its main worktree.  A linked worktree's .git is a file naming its
own git directory, whose commondir file names the repository's, so
telling needs no git process.  On a remote DIRECTORY, the names in them
are names on its machine."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (top (when-let* ((top (locate-dominating-file directory ".git")))
                (file-name-as-directory (expand-file-name top))))
         (common (when-let* ((top)
                             (gitdir (lispm-first-line (expand-file-name ".git" top)))
                             ((string-match "\\`gitdir:[ \t]*\\(.+\\)" gitdir))
                             (private (lispm-expand-file-name (match-string 1 gitdir) top))
                             (common (lispm-first-line (expand-file-name "commondir" private))))
                   (file-name-as-directory (lispm-expand-file-name common private)))))
    (if (not common)
        directory
      (file-name-as-directory
       (expand-file-name (file-relative-name directory top)
                         ;; A bare repository has no main worktree; it stands for itself.
                         (if (equal (file-name-nondirectory (directory-file-name common)) ".git")
                             (file-name-parent-directory common)
                           common))))))

(defun lispm-project-directory (root)
  "Directory of the sessions, tasks and memory of the project at ROOT.
Created on demand, under `lispm-directory', not in the project.  A
linked git worktree has its main worktree's."
  (let ((dir (expand-file-name (concat "projects/" (lispm--slug (lispm-main-worktree root)) "/")
                               lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-role-directory (role)
  "Directory of the memory of ROLE (a name), created on demand."
  (let ((dir (expand-file-name (format "roles/%s/" role) lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-system-directory ()
  "Directory of the memory, tasks and episodes of the system, created on demand.
They are those of agents of the whole system (see `lispm-make-agent'),
which work in no project."
  (let ((dir (expand-file-name "system/" lispm-directory)))
    (make-directory dir t)
    dir))

(defun lispm-home-directory (agent)
  "Directory of AGENT's own memory, tasks and episodes.
The system's for an agent of the whole system, the character's for a
character's own conversation (see `lispm-character-home-p'), else its
project's."
  (cond ((lispm-agent-system agent) (lispm-system-directory))
        ((lispm-character-home-p agent) (lispm-character-directory (lispm-agent-character agent)))
        (t (lispm-project-directory (lispm-agent-root agent)))))

(defconst lispm-policy-actions '(deny ask auto)
  "Policy actions, from the strictest to the most permissive.")

(defun lispm-stricter (a b)
  "The stricter of the policy actions A and B.
An unknown action counts as the strictest, as `deny' does."
  (let ((rank (lambda (action) (or (cl-position action lispm-policy-actions) 0))))
    (if (<= (funcall rank a) (funcall rank b)) a b)))

(defconst lispm--implied-permissions '((lisp read))
  "Classes whose tools also do what tools of other classes do.
Each entry is (CLASS . OTHERS): a Lisp program reads Emacs state
itself, so an agent that may not read may not run one either.")

(defconst lispm--permission-fallbacks '((shell . exec))
  "Classes split from another, each (CLASS . FROM).
A policy that does not name CLASS gives it the action it gives FROM, so
that policies written before the split mean what they meant: shell
commands were of class `exec', as Lisp is, until they had a class of
their own.")

(defun lispm-policy-lookup (permission policy)
  "The action POLICY, an alist, gives the class PERMISSION, or nil.
A class split from another that POLICY does not name gets the action
POLICY gives that one (see `lispm--permission-fallbacks')."
  (or (alist-get permission policy)
      (when-let* ((from (alist-get permission lispm--permission-fallbacks)))
        (lispm-policy-lookup from policy))))

(defun lispm-policy-action (agent permission)
  "Action AGENT's policy prescribes for tools of class PERMISSION.
An agent with a ceiling gets the stricter of its own policy's action
and the ceiling's.  A class that implies others (see
`lispm--implied-permissions') gets the strictest of their actions.
Each policy looked at gives a class it does not name the action of the
class it was split from, if any (see `lispm--permission-fallbacks')."
  (let ((action (or (lispm-policy-lookup permission (lispm-principal-policy agent))
                    (lispm-policy-lookup permission lispm-default-policy)
                    'ask))
        (ceiling (lispm-principal-ceiling agent)))
    (dolist (other (alist-get permission lispm--implied-permissions))
      (setq action (lispm-stricter action (lispm-policy-action agent other))))
    (if ceiling
        (lispm-stricter action (or (lispm-policy-lookup permission ceiling) 'ask))
      action)))

(defun lispm-policy (agent)
  "AGENT's effective policy: an alist from each permission class to its action."
  (mapcar (lambda (class) (cons class (lispm-policy-action agent class)))
          (delete-dups (mapcar #'car (append lispm-default-policy
                                             lispm--permission-fallbacks
                                             (lispm-principal-policy agent)
                                             (lispm-principal-ceiling agent))))))

(defconst lispm--state-names
  `((idle . ,(lispm-tr-noop "idle" "agent state"))
    (requesting . ,(lispm-tr-noop "requesting" "agent state"))
    (acting . ,(lispm-tr-noop "acting" "agent state"))
    (waiting . ,(lispm-tr-noop "waiting" "agent state"))
    (compacting . ,(lispm-tr-noop "compacting" "agent state"))
    (aborting . ,(lispm-tr-noop "aborting" "agent state")))
  "The states of agents as views name them to the user, in English,
translated where shown.")

(defun lispm-state-name (state)
  "STATE, an agent's, as views name it to the user, in the language spoken."
  (if-let* ((name (alist-get state lispm--state-names)))
      (lispm-tr name)
    (symbol-name state)))

(defun lispm-set-state (agent state &optional status activity)
  "Set AGENT's STATE and STATUS, and tell whoever is watching.
ACTIVITY is what STATUS says as data, for views that word it their own
way: `thinking', `retry' or `waiting' (for what STATUS names), or
\(running . TOOL) or (writing . TOOL), a call of TOOL being written."
  (setf (lispm-agent-state agent) state
        (lispm-agent-status agent) status
        (lispm-agent-activity agent) activity)
  (run-hook-with-args 'lispm-state-change-functions agent)
  (force-mode-line-update t))

(defun lispm-agent-busy-p (agent)
  "Non-nil if AGENT is in the middle of a turn."
  (and (lispm-agent-p agent)
       (not (eq (lispm-agent-state agent) 'idle))))

(defun lispm-agent-working-p (agent)
  "Non-nil while AGENT works: in a turn, and not waiting for an answer."
  (and (lispm-agent-p agent)
       (not (memq (lispm-agent-state agent) '(idle waiting)))))

(provide 'lispm-core)
;;; lispm-core.el ends here
