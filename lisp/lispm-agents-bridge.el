;;; lispm-agents-bridge.el --- LispM for the editor's agents  -*- lexical-binding: t; -*-

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

;; The editor's agents, as Claude Code, Codex and Hermes in their
;; terminals (lispm-agents.el), work beside LispM's own.  This module
;; makes each a peer of LispM's agents (lispm-collab.el) and serves it,
;; over its terminal's MCP server (lispm-agents-mcp.el), what LispM's
;; agents have:
;;
;;   list_agents send_message ask_agent reply   talking with every agent
;;   search_tools skill call_tool               LispM's skills and tools
;;
;; What comes to a peer is pasted into its terminal and sent, as its
;; user's next message (`lispm-agents-terminal-submit'), once it waits
;; for one: its agent's :ready says when (`lispm-agents-define').  Till
;; then messages queue, so that Return never lands in a dialog, as an
;; approval it would answer.  An agent whose own program takes messages
;; for it, as Holon's daemon does, has a :deliver instead, and is given
;; them at once.  An agent with neither takes no messages.
;;
;; An agent with a shell but no MCP client, as Holon, calls the same tools
;; with bin/lispm-tool (see lispm-agents-mcp.el).  One served no tools at
;; all, as an OpenClaw whose Gateway runs elsewhere, has a server serving
;; nothing (`lispm-agents-context-presence-make') and an :answer: it is
;; still a peer, which takes messages and answers requests with its final
;; reply.
;;
;; In LispM, a peer acts as an agent of `lispm-agents-bridge-role' in its
;; terminal's directory, which is none of LispM's running agents.  Its
;; calls of LispM's tools go by that agent's policy, as LispM's agents'
;; do (`lispm-tool-decision'); one the policy holds for approval asks
;; the user with `lispm-agents-confirm-function'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-agents)
(require 'lispm-agents-mcp)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-collab)
(require 'lispm-skills)

(defcustom lispm-agents-bridge-role nil
  "Role whose tools and policy the editor's agents have in LispM.
Nil for `lispm-default-role'."
  :type '(choice (const :tag "The default role" nil) symbol)
  :group 'lispm)

(defconst lispm-agents-bridge-unserved-tools
  '("list_agents" "send_message" "ask_agent" "return_value" "search_tools" "skill" "run_lisp")
  "LispM tools call_tool does not run: the editor's agents have their own
of them, or they work in a LispM session only.")

(defconst lispm-agents-bridge-unserved-permissions '(memory)
  "Permission classes of the LispM tools call_tool does not run: those
keeping a LispM agent's own memory and tasks.")

;;;; Peers

(defvar-local lispm-agents-bridge--name nil
  "The name the agent of this terminal has among the agents.")
(put 'lispm-agents-bridge--name 'permanent-local t)

(defvar-local lispm-agents-bridge--agent nil
  "The LispM agent the agent of this terminal acts as.")
(put 'lispm-agents-bridge--agent 'permanent-local t)

(defvar-local lispm-agents-bridge--queue nil
  "Messages waiting for the agent of this terminal to be free, oldest first.
Each is a plist with :id, :from, :to, :reply-to, :request-id, :queued,
:text and :delivery-text, the text as the terminal receives it.")
(put 'lispm-agents-bridge--queue 'permanent-local t)

(defvar-local lispm-agents-bridge--handling nil
  "Message last given to this terminal, until its turn is known to have ended.")
(put 'lispm-agents-bridge--handling 'permanent-local t)

(defvar-local lispm-agents-bridge--given nil
  "When the agent of this terminal was last given a message, as a float
time, until it is seen at work on it.")
(put 'lispm-agents-bridge--given 'permanent-local t)

(defvar lispm-agents-bridge-poll-interval 1.0
  "Seconds between looks at whether agents with messages waiting are free.")

(defvar lispm-agents-bridge-settle-time 30
  "Seconds an agent given a message may take to be seen at work on it.
Till then, or till it is, it is not given another: its transcript may
not tell of the message yet.")

(defvar lispm-agents-bridge--timer nil
  "Timer of the next look at agents with messages waiting.")

(defun lispm-agents-bridge--server (terminal)
  "Return the MCP server of the agent TERMINAL, or nil."
  (seq-find #'lispm-agents-mcp-p (lispm-agents-context-servers terminal)))

(defun lispm-agents-bridge--prop (terminal property)
  "Return PROPERTY of the agent of TERMINAL, see `lispm-agents-define'."
  (when-let* ((agent (or (buffer-local-value 'lispm-agents-buffer-agent terminal)
                         (lispm-agents-terminal-agent terminal))))
    (plist-get (lispm-agents-props agent) property)))

(defun lispm-agents-bridge-terminals ()
  "Return the live agent terminals that are peers, oldest first: those
LispM's tools are served to, and those whose agent takes messages through
its own program."
  (seq-filter (lambda (terminal)
                (and (or (lispm-agents-bridge--server terminal)
                         (lispm-agents-bridge--prop terminal :deliver))
                     (lispm-agents-terminal-live-p terminal)))
              (delete-dups (mapcar #'car lispm-agents-context--servers))))

(defun lispm-agents-bridge-name (terminal)
  "Return the name the agent of TERMINAL has among the agents.
It is the agent's, as `claude', or that followed by a number when an
agent, a role or another terminal has it already."
  (or (buffer-local-value 'lispm-agents-bridge--name terminal)
      (let* ((base (format "%s" (or (buffer-local-value 'lispm-agents-buffer-agent terminal)
                                    (lispm-agents-terminal-agent terminal)
                                    "agent")))
             (taken (append (mapcar (lambda (agent) (symbol-name (lispm-agent-name agent)))
                                    (lispm-agents))
                            (mapcar (lambda (role) (symbol-name (lispm-role-name role)))
                                    (lispm-roles))
                            (delq nil (mapcar (lambda (other)
                                                (buffer-local-value 'lispm-agents-bridge--name other))
                                              (lispm-agents-bridge-terminals)))))
             (name base)
             (n 1))
        (while (member name taken)
          (setq n (1+ n)
                name (format "%s-%d" base n)))
        (with-current-buffer terminal
          (setq lispm-agents-bridge--name name)))))

(defun lispm-agents-bridge--root (terminal)
  "Return the directory the agent of TERMINAL works in."
  (if-let* ((server (lispm-agents-bridge--server terminal)))
      (lispm-agents-mcp-directory server)
    (buffer-local-value 'default-directory terminal)))

(defun lispm-agents-bridge--kind (terminal)
  "Return what the agent of TERMINAL is, as \"Claude Code\"."
  (if-let* ((agent (or (buffer-local-value 'lispm-agents-buffer-agent terminal)
                       (lispm-agents-terminal-agent terminal))))
      (lispm-agents-name agent)
    "Agent"))

(defun lispm-agents-bridge--takes-messages-p (terminal)
  "Non-nil if the agent of TERMINAL takes messages: its program does, or
it can say when it is free for one."
  (or (lispm-agents-bridge--prop terminal :deliver)
      (lispm-agents-bridge--prop terminal :ready)))

(defun lispm-agents-bridge--free-p (terminal)
  "Non-nil if the agent of TERMINAL is free for a message now.
It waits for its user's, and has been seen at work on the last message
it was given, or has had `lispm-agents-bridge-settle-time' to be."
  (let ((given (buffer-local-value 'lispm-agents-bridge--given terminal)))
    (if (not (lispm-agents-terminal-ready-p terminal))
        (progn (when given
                 (with-current-buffer terminal (setq lispm-agents-bridge--given nil)))
               nil)
      ;; Seeing work after delivery, and then readiness, settles that
      ;; message even without a background session reader.
      (when (and (null given) (buffer-local-value 'lispm-agents-bridge--handling terminal))
        (with-current-buffer terminal (setq lispm-agents-bridge--handling nil))
        (lispm-agents-roster-notify terminal '(mailbox)))
      (or (null given) (> (- (float-time) given) lispm-agents-bridge-settle-time)))))

(defun lispm-agents-bridge-mailbox-summary (terminal)
  "Return the known mailbox counts of TERMINAL without copying its texts.
The result has :handling, non-nil while a delivered message's turn has
not been seen to end, and :queued, the number waiting.  A program with
:deliver owns its queue, so both fields are nil.  No transcript is read."
  (unless (lispm-agents-bridge--prop terminal :deliver)
    (list :handling (and (buffer-local-value 'lispm-agents-bridge--handling terminal) t)
          :queued (length (buffer-local-value 'lispm-agents-bridge--queue terminal)))))

(defun lispm-agents-bridge-mailbox (terminal)
  "Return the known mailbox of TERMINAL, or nil for a program's own queue.
The result has :handling, the entry whose turn has not been seen to end,
and :queued, entries oldest first.  Entries have :id, :from, :to,
:reply-to, :request-id, :queued (float seconds) and :text.  This only
copies memory; it neither reads a transcript nor asks whether the
terminal is ready.  A :deliver program owns its mailbox, which is unknown."
  (unless (lispm-agents-bridge--prop terminal :deliver)
    (let ((public (lambda (entry)
                    (when entry
                      (let ((copy (copy-tree entry)))
                        (cl-remf copy :delivery-text)
                        (cl-remf copy :delivered)
                        copy)))))
      (list :handling (funcall public (buffer-local-value 'lispm-agents-bridge--handling terminal))
            :queued (mapcar public (buffer-local-value 'lispm-agents-bridge--queue terminal))))))

(defun lispm-agents-bridge--message-change (terminal entry type)
  "Report the message ENTRY of TERMINAL as TYPE to roster observers."
  (lispm-agents-roster-notify
   terminal '(mailbox)
   (list :type type :id (plist-get entry :id)
         :from (plist-get entry :from) :to (plist-get entry :to)
         :request-id (plist-get entry :request-id))))

(defun lispm-agents-bridge--settle (terminal)
  "Forget TERMINAL's handling entry when its reader knows its turn ended.
A transcript older than delivery cannot say that the message was handled."
  (when-let* ((entry (buffer-local-value 'lispm-agents-bridge--handling terminal))
              (kind (buffer-local-value 'lispm-agents-buffer-agent terminal))
              (id (buffer-local-value 'lispm-agents-buffer-session-id terminal))
              (session (lispm-agents-known-session kind id))
              (time (plist-get session :time))
              ((numberp time))
              (delivered (plist-get entry :delivered))
              ((numberp delivered))
              ((> time delivered))
              ((plist-get session :turn-ended)))
    (with-current-buffer terminal
      (setq lispm-agents-bridge--handling nil))
    (lispm-agents-roster-notify terminal '(mailbox))))

(defun lispm-agents-bridge-observe-work (terminal work)
  "Settle TERMINAL's handling message when WORK records its completed turn.
WORK is the bounded, cached work a provider read lazily or after terminal
output.  An older transcript cannot settle a newly delivered message.
The turn's own time must follow delivery, or the bridge must have
observed work since delivery.  A metadata write alone proves neither."
  (when-let* ((entry (buffer-local-value 'lispm-agents-bridge--handling terminal))
              (delivered (plist-get entry :delivered))
              ((numberp delivered))
              (time (plist-get work :time))
              ((numberp time))
              ((> time delivered))
              ((or (when-let* ((turn-time (plist-get work :turn-time)))
                     (>= turn-time delivered))
                   (null (buffer-local-value 'lispm-agents-bridge--given terminal))))
              ((plist-get work :turn-ended)))
    (with-current-buffer terminal (setq lispm-agents-bridge--handling nil))
    (lispm-agents-roster-notify terminal '(mailbox))))

(defun lispm-agents-bridge-observe (terminal)
  "Observe TERMINAL's mailbox after actual output, without a polling timer.
Only a terminal with a handling entry reads work, through its provider's
bounded cache.  Roster snapshots never call this function."
  (when (buffer-local-value 'lispm-agents-bridge--handling terminal)
    (when-let* ((details (lispm-agents-bridge--prop terminal :work-details)))
      (lispm-agents-bridge-observe-work terminal (funcall details terminal)))))

(defun lispm-agents-bridge-flush ()
  "Give each agent free for one the first message waiting for it.
Look again later while messages wait."
  (when lispm-agents-bridge--timer
    (cancel-timer lispm-agents-bridge--timer)
    (setq lispm-agents-bridge--timer nil))
  (let ((waiting nil))
    (dolist (terminal (lispm-agents-bridge-terminals))
      (when (buffer-local-value 'lispm-agents-bridge--queue terminal)
        (when (with-demoted-errors "lispm-agents-bridge: %S"
                (lispm-agents-bridge--free-p terminal))
          (with-current-buffer terminal
            (let ((entry (pop lispm-agents-bridge--queue)))
              (setq lispm-agents-bridge--given (float-time)
                    lispm-agents-bridge--handling
                    (plist-put entry :delivered lispm-agents-bridge--given))
              (lispm-agents-terminal-submit terminal (plist-get entry :delivery-text))
              (lispm-agents-bridge--message-change terminal entry 'message-delivered))))
        (when (buffer-local-value 'lispm-agents-bridge--queue terminal)
          (setq waiting t))))
    (when waiting
      (setq lispm-agents-bridge--timer
            (run-at-time lispm-agents-bridge-poll-interval nil #'lispm-agents-bridge-flush)))))

(defun lispm-agents-bridge--sessions-arrived ()
  "Look at once whether agents with messages waiting are free: a session
reader has read the transcripts anew, which may say so.
This is on `lispm-agents-sessions-arrived-hook'."
  (dolist (terminal (lispm-agents-bridge-terminals))
    (lispm-agents-bridge--settle terminal))
  (when lispm-agents-bridge--timer
    (cancel-timer lispm-agents-bridge--timer)
    (setq lispm-agents-bridge--timer (run-at-time 0 nil #'lispm-agents-bridge-flush))))

(defun lispm-agents-bridge-deliver (terminal text &optional metadata)
  "Put TEXT to the agent of TERMINAL as its user's next message, once free.
METADATA, else `lispm-collab-delivery-metadata', preserves the message's
:id, :from, :to, :reply-to, :request-id, :queued and original :text.
Unknown fields are nil; a missing id or queued time is assigned here.
Return non-nil if TEXT was given at once.  An error if the agent has
ended, or takes no messages."
  (let* ((name (lispm-agents-bridge-name terminal))
         (metadata (or metadata lispm-collab-delivery-metadata))
         (entry (list :id (or (plist-get metadata :id) (lispm-agents-uuid))
                      :from (plist-get metadata :from)
                      :to (or (plist-get metadata :to) name)
                      :reply-to (plist-get metadata :reply-to)
                      :request-id (or (plist-get metadata :request-id)
                                      (plist-get metadata :reply-to))
                      :queued (or (plist-get metadata :queued) (float-time))
                      :text (or (plist-get metadata :text) text)
                      :delivery-text text)))
    (unless (lispm-agents-terminal-live-p terminal)
      (error "%s has ended" name))
    (unless (lispm-agents-bridge--takes-messages-p terminal)
      (error "%s takes no messages: LispM cannot tell when it waits for one" name))
    (if-let* ((deliver (lispm-agents-bridge--prop terminal :deliver)))
        (let ((given (funcall deliver terminal text)))
          (lispm-agents-bridge--message-change
           terminal entry (if given 'message-delivered 'message-queued))
          given)
      (with-current-buffer terminal
        (setq lispm-agents-bridge--queue (append lispm-agents-bridge--queue (list entry))))
      (lispm-agents-bridge--message-change terminal entry 'message-queued)
      (lispm-agents-bridge-flush)
      (not (memq entry (buffer-local-value 'lispm-agents-bridge--queue terminal))))))

(defun lispm-agents-bridge--state (terminal)
  "What the agent of TERMINAL is doing, as list_agents says it."
  (cond ((not (lispm-agents-bridge--takes-messages-p terminal)) "takes no messages")
        ((not (lispm-agents-bridge--prop terminal :ready)) "takes messages")
        ((lispm-agents-terminal-ready-p terminal) "idle")
        (t "busy")))

(defun lispm-agents-bridge--answer (terminal)
  "Return how the agent of TERMINAL answers a request, if it is served no
tools to reply with: a function of a text and a function of its reply."
  (unless (lispm-agents-bridge--server terminal)
    (when-let* ((answer (lispm-agents-bridge--prop terminal :answer)))
      (lambda (text callback)
        (let ((name (lispm-agents-bridge-name terminal)))
          (unless (lispm-agents-terminal-live-p terminal)
            (error "%s has ended" name))
          (funcall answer terminal text callback))))))

(defun lispm-agents-bridge--peer (terminal)
  "Return the agent of TERMINAL as a peer of LispM's agents."
  (lispm-peer-create :name (lispm-agents-bridge-name terminal)
                     :kind (lispm-agents-bridge--kind terminal)
                     :root (lispm-agents-bridge--root terminal)
                     :state (lambda () (lispm-agents-bridge--state terminal))
                     :deliver (lambda (text) (lispm-agents-bridge-deliver terminal text))
                     :answer (lispm-agents-bridge--answer terminal)))

(defun lispm-agents-bridge-peers ()
  "Return the agents of the terminals LispM's tools are served to, as peers.
This is one of `lispm-peer-functions'."
  (mapcar #'lispm-agents-bridge--peer (lispm-agents-bridge-terminals)))

(defun lispm-agents-bridge-agent (terminal)
  "Return the LispM agent the agent of TERMINAL acts as, made at first need.
It has the role `lispm-agents-bridge-role' names, and works in the
terminal's directory; it is none of LispM's running agents."
  (or (buffer-local-value 'lispm-agents-bridge--agent terminal)
      (let ((agent (lispm--apply-role
                    (lispm-agent--create :name (intern (lispm-agents-bridge-name terminal))
                                         :id (lispm-agents-uuid)
                                         :root (lispm-agents-bridge--root terminal))
                    (lispm-get-role (or lispm-agents-bridge-role lispm-default-role)))))
        (with-current-buffer terminal
          (setq lispm-agents-bridge--agent agent)))))

(defun lispm-agents-bridge--ended (terminal)
  "Settle what was asked of the agent of TERMINAL, whose servers stop.
This is one of `lispm-agents-context-release-functions'."
  (when-let* ((name (buffer-local-value 'lispm-agents-bridge--name terminal)))
    (lispm-collab-peer-ended name)))

;;;; Talking with the agents

(defun lispm-agents-bridge--terminal (server)
  "Return the agent terminal SERVER serves; an error if none."
  (or (lispm-agents-context-terminal server)
      (error "This server serves no agent terminal")))

(defun lispm-agents-bridge--self (server)
  "Return the peer SERVER's agent is."
  (lispm-agents-bridge--peer (lispm-agents-bridge--terminal server)))

(defun lispm-agents-bridge--string (arguments key)
  "Return the string ARGUMENTS hold under KEY; an error if they hold none."
  (let ((value (alist-get key arguments)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (error "Give %s, a string" key))
    value))

(defun lispm-agents-bridge--tool-list-agents (server _arguments _id)
  "Tell SERVER's agent who is here."
  (list (lispm-collab-agents-text (lispm-peer-name (lispm-agents-bridge--self server)))))

(defun lispm-agents-bridge--tool-send-message (server arguments _id)
  "Send the message ARGUMENTS give, from SERVER's agent."
  (list (lispm-collab-send (lispm-peer-name (lispm-agents-bridge--self server))
                           (lispm-agents-bridge--string arguments 'to)
                           (lispm-agents-bridge--string arguments 'text))))

(defun lispm-agents-bridge--tool-ask-agent (server arguments _id)
  "Ask what ARGUMENTS give for SERVER's agent; the reply comes as a message."
  (let* ((terminal (lispm-agents-bridge--terminal server))
         (target (lispm-agents-bridge--string arguments 'agent))
         (id (lispm-collab-ask-for-peer (lispm-agents-bridge-agent terminal)
                                        (lispm-agents-bridge--peer terminal)
                                        target
                                        (lispm-agents-bridge--string arguments 'request))))
    (list (format "Asked %s (request %s). The reply will come to you as a message: \
[Reply from %s to request %s]. Go on meanwhile, or end your turn to wait."
                  target id target id))))

(defun lispm-agents-bridge--tool-reply (server arguments _id)
  "Settle the request ARGUMENTS name with the reply they give."
  (let ((self (lispm-agents-bridge--self server))
        (request (lispm-agents-bridge--string arguments 'request)))
    (lispm-peer-reply self request (lispm-agents-bridge--string arguments 'text))
    (list (format "Your reply to request %s is delivered." request))))

;;;; Skills and tools

(defun lispm-agents-bridge--served-p (tool)
  "Non-nil if call_tool runs TOOL."
  (not (or (member (lispm-tool-name tool) lispm-agents-bridge-unserved-tools)
           (memq (lispm-tool-permission tool) lispm-agents-bridge-unserved-permissions))))

(defun lispm-agents-bridge--tool-line (tool)
  "TOOL, its description and its input's schema, for an agent to call."
  (format "- %s: %s\n  input: %s" (lispm-tool-name tool) (lispm-tools--description tool)
          (condition-case nil
              (json-serialize (or (alist-get 'input_schema (lispm-tool-schema tool))
                                  '((type . "object"))))
            (error "an object"))))

(defun lispm-agents-bridge--tool-search (server arguments _id)
  "Find LispM's skills and tools as ARGUMENTS say, for SERVER's agent."
  (let* ((agent (lispm-agents-bridge-agent (lispm-agents-bridge--terminal server)))
         (query (lispm-agents-bridge--string arguments 'query))
         (limit (alist-get 'max_results arguments))
         (answer lispm-agents-context-answer))
    (lispm-tools--prepare
     (seq-filter (lambda (source) (plist-get source :pending)) (lispm-tools--sources))
     (lambda (problems)
       (condition-case err
           (pcase-let ((`(,found . ,others)
                        (lispm-tools--discover agent query
                                               (if (integerp limit) limit lispm-tool-search-results)
                                               #'lispm-agents-bridge--served-p)))
             (let ((help (lispm-tools--selection-help
                          agent query found others #'lispm-agents-bridge--served-p)))
               (funcall answer
                        (list (concat
                               (if (or found others)
                                   (string-join
                                    (delq nil
                                          (list (and others
                                                     (concat "Skills, which you load with skill:\n"
                                                             (mapconcat (lambda (other) (plist-get other :line))
                                                                        others "\n")))
                                                (and found
                                                     (concat "Tools, which you call with call_tool:\n"
                                                             (mapconcat #'lispm-agents-bridge--tool-line
                                                                        found "\n")))))
                                    "\n")
                                 (format "Nothing matches %S." query))
                               (when help (concat "\n" help))
                               (if problems (concat "\n" (string-join problems "\n")) ""))))))
         (error (funcall answer (list (error-message-string err)) t)))))
    'deferred))

(defun lispm-agents-bridge--skill-tools (skill defined)
  "The tools of SKILL an agent calls with call_tool: DEFINED, those its code
defined, and those its file names, as are registered."
  (seq-filter (lambda (tool) (and tool (lispm-agents-bridge--served-p tool)))
              (mapcar #'lispm-get-tool
                      (seq-uniq (append defined
                                        (mapcar #'car (plist-get (lispm-skills--index skill) :tools)))))))

(defun lispm-agents-bridge--tool-skill (server arguments _id)
  "Load the skill ARGUMENTS name for SERVER's agent; return its text.
Its tools follow, with their inputs' schemas.  The code of a skill of a
project runs only while the user trusts it, as for LispM's agents; else
the agent gets the text alone."
  (let* ((agent (lispm-agents-bridge-agent (lispm-agents-bridge--terminal server)))
         (name (lispm-agents-bridge--string arguments 'name))
         (skill (lispm-skills--find name agent)))
    (unless skill
      (let ((nearest (lispm-skills--nearest name agent)))
        (error "No skill named %s%s; search_tools finds skills by what they do" name
               (if nearest (format " (nearest: %s)" (string-join nearest ", ")) ""))))
    (let* ((text (lispm-skills--file-text (plist-get skill :file)))
           (problem nil)
           (forms (condition-case err
                      (lispm-skills--forms text)
                    (error (setq problem (error-message-string err)) nil)))
           (unevaluated (cond (problem (concat "its code was not evaluated: " problem))
                              ((not (lispm-skill-trusted-p skill text))
                               "its code was not evaluated: the user has not trusted the code of \
this project's skill (M-x lispm-trust-skill)")))
           (defined (unless unevaluated
                      (let ((lispm-current-agent agent))
                        (lispm-skills--evaluate skill agent forms))))
           (tools (lispm-agents-bridge--skill-tools skill defined)))
      (list (lispm-skills--loaded-text
             skill defined unevaluated
             (when tools
               (concat "Call its tools with call_tool:\n"
                       (mapconcat #'lispm-agents-bridge--tool-line tools "\n") "\n\n")))))))

(defun lispm-agents-bridge--input (arguments)
  "The input of the call ARGUMENTS ask for, an alist; an error if malformed.
An agent may give it as an object, or as the text of one."
  (let ((input (alist-get 'input arguments)))
    (cond ((null input) nil)
          ((stringp input)
           (condition-case nil
               (json-parse-string input :object-type 'alist :null-object nil :false-object nil)
             (error (error "The input is not a JSON object: %s" input))))
          ((listp input) input)
          (t (error "The input is a JSON object")))))

(defun lispm-agents-bridge--run (agent tool input answer)
  "Run AGENT's call of TOOL on INPUT; give ANSWER the result."
  (let ((lispm-current-agent agent)
        (lispm-current-call nil)
        (default-directory (lispm-agent-root agent)))
    (lispm-tool-invoke tool input
                       (lambda (text errorp &optional images)
                         (funcall answer
                                  (cons text (mapcar (lambda (file) (format "Image: %s" file)) images))
                                  errorp)))))

(defun lispm-agents-bridge--detail (agent tool input reasons)
  "What the user is shown of AGENT's call of TOOL on INPUT, held for REASONS."
  (let ((preview (let ((lispm-current-agent agent)
                       (default-directory (lispm-agent-root agent)))
                   (lispm-tool-preview-text tool input))))
    (lispm--truncate (string-join (delq nil (append reasons
                                                    (list (or preview
                                                              (and input
                                                                   (condition-case nil
                                                                       (json-serialize input)
                                                                     (error (prin1-to-string input))))))))
                                  "\n")
                     2000)))

(defun lispm-agents-bridge--ask (terminal agent tool input reasons answer)
  "Ask the user about AGENT's call of TOOL on INPUT, for TERMINAL's agent.
REASONS say why it is asked.  Run it if they approve; else, or should
asking fail, give ANSWER the denial."
  (if (condition-case nil
          (funcall lispm-agents-confirm-function
                   (lispm-tr "%s wants to run %s" (lispm-agents-bridge--kind terminal)
                             (lispm-tool-name tool))
                   (lispm-agents-bridge--detail agent tool input reasons)
                   (lispm-tr-in "permission" "Allow"))
        ((error quit) nil))
      (lispm-agents-bridge--run agent tool input answer)
    (funcall answer (list "Denied: the user did not approve this call.") t)))

(defun lispm-agents-bridge--tool-call (server arguments _id)
  "Run the LispM tool ARGUMENTS name, on their input, for SERVER's agent.
It runs at once, waits for the user's approval or is refused, as the
policy of the agent acting in LispM decides."
  (let* ((terminal (lispm-agents-bridge--terminal server))
         (agent (lispm-agents-bridge-agent terminal))
         (name (lispm-agents-bridge--string arguments 'name))
         (tool (lispm-get-tool name))
         (input (lispm-agents-bridge--input arguments))
         (answer lispm-agents-context-answer))
    (cond ((not tool)
           (error "There is no LispM tool named %s; search_tools finds them" name))
          ((member name lispm-agents-bridge-unserved-tools)
           (error "%s is not called with call_tool%s" name
                  (if (member name '("list_agents" "send_message" "ask_agent" "search_tools" "skill"))
                      ": it is a tool of your own"
                    "")))
          ((not (lispm-agents-bridge--served-p tool))
           (error "%s keeps a LispM agent's own memory: use your own" name))
          ((not (lispm-tools-allowed-p agent name))
           (error "The role you have in LispM does not give you %s" name)))
    (pcase-let ((`(,action . ,reasons) (lispm-tool-decision agent tool input)))
      (pcase action
        ('deny (funcall answer (list (lispm-tools-denial agent tool reasons)) t))
        ('ask (run-at-time 0 nil #'lispm-agents-bridge--ask terminal agent tool input reasons answer))
        (_ (lispm-agents-bridge--run agent tool input answer))))
    'deferred))

;;;; Serving them

(defconst lispm-agents-bridge-tools
  '(("list_agents" lispm-agents-bridge--tool-list-agents nil nil
     "List the agents working in this Emacs: LispM's own agents (role, project, state), \
the agents of other programs, such as you, and the roles LispM starts helpers from.")
    ("send_message" lispm-agents-bridge--tool-send-message
     ((to . ((type . "string") (description . "Name of the receiving agent or LispM character, as \
list_agents gives it.")))
      (text . ((type . "string")
               (description . "The message, self-contained: they cannot see your conversation. \
Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))))
     ["to" "text"]
     "Send a message to another agent without waiting for an answer.  It reaches them as soon \
as they are free.  Use ask_agent when you need an answer.")
    ("ask_agent" lispm-agents-bridge--tool-ask-agent
     ((agent . ((type . "string")
                (description . "A running agent, a LispM character (by its name, as Ada), or a role to \
start a helper from: list_agents shows them.")))
      (request . ((type . "string")
                  (description . "What you need, self-contained: they cannot see your conversation. \
Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))))
     ["agent" "request"]
     "Ask another agent for something.  Its reply comes to you later as a message, \
[Reply from NAME to request ID].  Naming a role starts a LispM helper of that role in your \
project, which retires once it has replied.")
    ("reply" lispm-agents-bridge--tool-reply
     ((request . ((type . "string") (description . "The request's id, as R1.")))
      (text . ((type . "string") (description . "Your answer, self-contained."))))
     ["request" "text"]
     "Answer a request another agent sent you, [Request ID from NAME]: your text becomes \
their reply.  They wait for it.")
    ("search_tools" lispm-agents-bridge--tool-search
     ((query . ((type . "string")
                (description . "Keywords, as \"play music\" or \"sticky note\"; a word may be \
part of a longer one, or misspelt.")))
      (max_results . ((type . "integer")
                      (description . "How many skills and tools to find (default 5)."))))
     ["query"]
     "Find what LispM can do for you: its skills, the manuals of the parts and apps of this \
system, each with its tools, and its tools.  Search whenever a request needs something you \
lack in Emacs or its apps.  Load a skill with skill; call a tool with call_tool.")
    ("skill" lispm-agents-bridge--tool-skill
     ((name . ((type . "string") (description . "The skill to load, as search_tools names it."))))
     ["name"]
     "Load a LispM skill that search_tools found: how to do a kind of thing in this system, \
and its tools with their inputs, which you call with call_tool.")
    ("call_tool" lispm-agents-bridge--tool-call
     ((name . ((type . "string") (description . "The LispM tool, as search_tools or a skill names it.")))
      (input . ((type . "object") (description . "Its input, as its schema says."))))
     ["name"]
     "Run a LispM tool.  It runs under LispM's permission policy: reading runs at once, while \
changing files, running shell commands and the like wait for the user's approval in Emacs, \
which may take a while."))
  "The tools LispM serves the editor's agents, in the form of
`lispm-agents-context-tools'.")

(cl-defmethod lispm-agents-context-instructions ((server lispm-agents-mcp))
  "Tell SERVER's agent that LispM is here, and how to work with it."
  (when-let* ((terminal (lispm-agents-context-terminal server)))
    (format "This Emacs runs LispM. Its agents and other programs' agents, such as you, work \
side by side here; among them you are %s. list_agents shows who is here, send_message and \
ask_agent reach them, and what they send comes to you as your user's message: [Message from \
NAME]; [Request ID from NAME], which you answer with reply; [Reply from NAME to request ID]. \
search_tools finds LispM's skills, the manuals of the parts and apps of this system, and its \
tools: load a skill with skill, call a tool with call_tool. A call that changes things may \
wait for the user's approval in Emacs. LispM's characters are someone, not tools: when your \
user writes @Name for one of them, as \"tell @Ada\" or \"have @Ada look at this\", pass it \
on with send_message or ask_agent to Name, and it takes it up in a conversation of its own in \
your directory.%s"
            (lispm-agents-bridge-name terminal)
            (if-let* ((characters (seq-remove #'lispm-character-skill (lispm-characters))))
                (concat " The characters now: "
                        (mapconcat #'lispm-character-title* characters ", ")
                        "; apps are characters too, which list_agents names.")
              ""))))

(setq lispm-agents-mcp-extra-tools
      (append (seq-remove (lambda (tool) (assoc (car tool) lispm-agents-bridge-tools))
                          lispm-agents-mcp-extra-tools)
              lispm-agents-bridge-tools))

(add-hook 'lispm-peer-functions #'lispm-agents-bridge-peers)
(add-hook 'lispm-agents-context-release-functions #'lispm-agents-bridge--ended)
(add-hook 'lispm-agents-sessions-arrived-hook #'lispm-agents-bridge--sessions-arrived)

(provide 'lispm-agents-bridge)
;;; lispm-agents-bridge.el ends here
