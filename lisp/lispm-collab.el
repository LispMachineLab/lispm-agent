;;; lispm-collab.el --- Agents working together  -*- lexical-binding: t; -*-

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

;; Several agents share one Emacs: the same buffers, files and event
;; loop.  They coordinate through four tools:
;;
;;   list_agents   who else is here, what they are doing, and which
;;                 roles helpers can be started from;
;;   send_message  fire and forget: the text lands in the other agent's
;;                 inbox and becomes a "* Message from X" turn;
;;   ask_agent     delegation: returns a request id immediately.  The
;;                 other agent's final reply arrives as a new message,
;;                 with a value it returned (return_value), if any;
;;                 the caller can keep working and answer new input.
;;                 Asking a role rather than an agent starts a helper
;;                 of that role in the caller's project, which retires
;;                 once it has replied: a subagent.  A helper may do no
;;                 more than its caller: for each permission class it
;;                 gets the stricter of its role's and its caller's
;;                 policy;
;;   start_agent   an agent of a role, started in the caller's project
;;                 or another, that works on its own: nobody waits for
;;                 its reply, and it stays once it has replied, for the
;;                 user and the other agents to talk to.  It is bounded
;;                 by its caller as a helper is; started outside the
;;                 caller's project, where it may do what the caller may
;;                 do in its own, it is started only once the user
;;                 approved it.
;;
;; A character is reached by its name too, as @Ada: the request goes to
;; its conversation in the caller's project, which is started there if
;; it has none, and goes on from then on (see `lispm-character-agent').
;; An app that is a character has one conversation, its own, wherever
;; it is asked from.
;;
;; inspect_agent (lispm-inspect.el) shows another agent in depth.
;; Places and buffers pass between agents as org links,
;; [[file:PATH::LINE]] and [[buffer:NAME]], which tools resolve (see
;; `lispm-tools-reference').
;;
;; Delegating is `lispm-delegate', for other tools too: the worktree
;; skill's worktree_ask starts its helper in a git worktree of its own.
;;
;; A delegation outlives Emacs.  The caller's call records whom it
;; asked (:ASKED:, their session), and the request, in the inbox file
;; until its turn ends, whom to reply to (its reply-to).  So when
;; Emacs stops before the reply, resuming agents with pending inbox
;; entries takes the delegation up: the one asked goes on with the request, or gets
;; it, and its reply reaches the caller's inbox.  Older delegations
;; and tools using `lispm-delegate' still settle the waiting call.
;; A helper stays bounded by its caller, whose ceiling its session records.
;;
;; Shared state needs no locks: the event loop is single-threaded, and
;; edits are exact-text replacements, which fail instead of clobbering
;; when someone else changed the text first (optimistic concurrency).
;; Tools using `lispm-delegate' to wait refuse cycles.  Background
;; requests do not create wait edges: either agent remains free to
;; answer the other while its own request is outstanding.
;;
;; Agents of other programs work in the same Emacs too, as Claude Code
;; and Codex do in their terminals (lispm-agents-bridge.el): peers.  The
;; three tools reach them as they reach LispM's agents, and peers reach
;; every agent in turn.  What comes to a peer is put to it as its user's
;; next message: [Message from X], [Request R1 from X], which it answers
;; with a reply of its own, and [Reply from X to request R1].  A peer
;; with no tools to reply with, as an OpenClaw whose Gateway cannot reach
;; Emacs, answers a request with its final reply to it instead.  Requests
;; asked of peers live as long as Emacs.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

(declare-function lispm-agents-roster-notify "lispm-agents" (subject fields &optional event))

;;;; Peers: agents of other programs

(cl-defstruct (lispm-peer (:constructor lispm-peer-create)
                          (:copier nil))
  "An agent of another program working in this Emacs, as Claude Code.
NAME, a string, names it to the agents; KIND says what it is, \"Claude
Code\"; ROOT is the directory it works in; STATE, a function, returns
what it is doing, as \"idle\"; DELIVER is a function of a text putting
the text to it as its user's next message, once it is free for it,
which returns non-nil if it was given the text at once.  ANSWER, for a
peer with no tools to reply with, is a function of a text and a function
of a text: it puts the text to the peer as DELIVER does and calls the
function with the peer's final reply to it."
  name kind root state deliver answer)

(defvar lispm-peer-functions nil
  "Functions returning peers, the agents of other programs in this Emacs.
Each is called with no argument and returns a list of `lispm-peer's,
whose names are those of no agent of LispM's.")

(defun lispm-peers ()
  "Return the peers working in this Emacs."
  (seq-mapcat #'funcall lispm-peer-functions))

(defun lispm-get-peer (name)
  "Return the peer called NAME, a string or a symbol, or nil."
  (let ((name (format "%s" name)))
    (seq-find (lambda (peer) (equal (lispm-peer-name peer) name)) (lispm-peers))))

(defvar lispm-collab--requests nil
  "Requests asked of peers, awaiting their reply: (ID PEER CALLBACK METADATA).
PEER is the name of the peer asked; CALLBACK takes its reply and, when
it ended without one, non-nil.  METADATA holds :from, :to and :queued.")

(defvar lispm-collab-delivery-metadata nil
  "The message being given to a peer, as a plist, or nil.
Bound around a peer's delivery function, without changing its calling
convention.  It holds :id, :from, :to, :queued, :text and, for a request
or reply, :reply-to and :request-id.  A delivery queue may retain it.")

(defvar lispm-collab--request-count 0
  "Requests asked by or of peers since Emacs started.")

(defun lispm-collab-new-request-id ()
  "Return the id of a new request asked by or of a peer: R1, R2...."
  (format "R%d" (cl-incf lispm-collab--request-count)))

(defun lispm-tell-peer (peer text from)
  "Give PEER the message TEXT from FROM, the name of an agent or a peer.
Return non-nil if PEER was given it at once, rather than once free."
  (let ((lispm-collab-delivery-metadata
         (list :id (lispm-inbox-new-id) :from from :to (lispm-peer-name peer)
               :queued (float-time) :text text)))
    (funcall (lispm-peer-deliver peer)
           (if (lispm-peer-answer peer)
               (format "[Message from %s]\n%s" from text)
             (format "[Message from %s]\n%s\n\n(Answer, if need be, with your send_message tool, to %s.)"
                     from text from)))))

(cl-defun lispm-ask-peer (peer request from callback &key id)
  "Ask PEER REQUEST on behalf of FROM, a name; return the request's id.
CALLBACK is called with PEER's reply, or with a text saying it ended
without one and non-nil.  ID is the request's id, a new one by default.
A peer with no tools to reply with answers with its final reply."
  (let* ((id (or id (lispm-collab-new-request-id)))
         (answer (lispm-peer-answer peer))
         (lispm-collab-delivery-metadata
          (list :id (lispm-inbox-new-id) :from from :to (lispm-peer-name peer)
                :reply-to id :request-id id :queued (float-time) :text request)))
    (push (list id (lispm-peer-name peer) callback
                (list :from from :to (lispm-peer-name peer)
                      :queued (plist-get lispm-collab-delivery-metadata :queued)))
          lispm-collab--requests)
    (when (fboundp 'lispm-agents-roster-notify)
      (lispm-agents-roster-notify nil '(waits)))
    (condition-case err
        (if answer
            (funcall answer
                     (format "[Request %s from %s]\n%s\n\n(%s waits for your answer: your reply to this message is it.)"
                             id from request from)
                     (lambda (reply)
                       (when (assoc id lispm-collab--requests)
                         (lispm-peer-reply peer id reply))))
          (funcall (lispm-peer-deliver peer)
                   (format "[Request %s from %s]\n%s\n\n(%s waits for your answer: give it with your reply tool, request %s.)"
                           id from request from id)))
      (error (setq lispm-collab--requests (cl-remove id lispm-collab--requests :key #'car :test #'equal))
             (when (fboundp 'lispm-agents-roster-notify)
               (lispm-agents-roster-notify nil '(waits)))
             (signal (car err) (cdr err))))
    id))

(defun lispm-peer-reply (peer id text)
  "Settle the request ID asked of PEER with its reply TEXT.
An error if no request ID awaits PEER's reply."
  (let ((entry (assoc id lispm-collab--requests)))
    (unless (and entry (equal (nth 1 entry) (lispm-peer-name peer)))
      (error "No request %s awaits your reply" id))
    (setq lispm-collab--requests (delq entry lispm-collab--requests))
    (when (fboundp 'lispm-agents-roster-notify)
      (lispm-agents-roster-notify
       nil '(waits) (list :type 'request-settled :request-id id
                         :from (lispm-peer-name peer) :to (plist-get (nth 3 entry) :from))))
    (funcall (nth 2 entry) text)))

(defun lispm-collab-peer-ended (name)
  "Settle the requests asked of the peer NAME, which ended without replying."
  (dolist (entry (seq-filter (lambda (entry) (equal (nth 1 entry) name)) lispm-collab--requests))
    (setq lispm-collab--requests (delq entry lispm-collab--requests))
    (when (fboundp 'lispm-agents-roster-notify)
      (lispm-agents-roster-notify
       nil '(waits) (list :type 'request-settled :request-id (car entry)
                         :from name :to (plist-get (nth 3 entry) :from) :ended t)))
    (with-demoted-errors "lispm request callback: %S"
      (funcall (nth 2 entry) (format "%s ended without replying." name) t))))

(defun lispm-collab--name (sender)
  "The name of SENDER, an agent or the name of a peer, as a string."
  (if (lispm-agent-p sender) (symbol-name (lispm-agent-name sender)) sender))

(defun lispm-collab--place (sender)
  "Where SENDER, an agent or a peer's name, works, for a character it asks:
its project's root, or nil for none, as for an agent of the whole system."
  (cond ((lispm-agent-p sender) (unless (lispm-agent-system sender) (lispm-agent-root sender)))
        ((lispm-get-peer sender) (lispm-peer-root (lispm-get-peer sender)))))

(defun lispm-collab--character-agent (name sender)
  "The conversation of the character NAME designates where SENDER works; or nil.
SENDER is an agent or a peer's name.  NAME is a character's name, or
the name it goes by, with or without @ (see `lispm-get-character').
An app's is its own, wherever SENDER works."
  (when-let* ((character (and (or (stringp name) (and name (symbolp name)))
                              (lispm-get-character (format "%s" name) t))))
    (lispm-character-agent character (unless (lispm-character-skill character)
                                       (lispm-collab--place sender)))))

(defun lispm-collab-send (from to text)
  "Send TEXT from FROM, an agent or a peer's name, to the agent or peer TO.
TO may be a character, by its name: its conversation where FROM works
gets TEXT.  Return what the sender is told."
  (let ((agent (or (lispm-collab--character-agent to from) (lispm-get-agent to t)))
        (name (lispm-collab--name from)))
    (cond
     ((or (eq agent from) (equal (format "%s" to) name)) (error "That is you"))
     (agent
      (lispm-tell agent text :from name)
      (format "Delivered to %s%s." to
              (if (lispm-agent-busy-p agent) ", who is busy and will read it next" "")))
     ((lispm-get-peer to)
      (if (lispm-tell-peer (lispm-get-peer to) text name)
          (format "Delivered to %s." to)
        (format "%s is busy: it gets your message as soon as it is free." to)))
     (t (error "No agent named %s; list_agents, which search_tools finds, shows them" to)))))

(defun lispm-collab-ask-for-peer (caller peer target request)
  "Ask TARGET REQUEST for PEER, acting in LispM as CALLER; return its id.
TARGET is an agent, a role or a peer, by name.  The reply comes to PEER
as a message: [Reply from X to request ID]."
  (let* ((id (lispm-collab-new-request-id))
         (answer (lambda (from reply)
                   (let ((lispm-collab-delivery-metadata
                          (list :id (lispm-inbox-new-id) :from from :to (lispm-peer-name peer)
                                :reply-to id :request-id id :queued (float-time) :text reply)))
                     (funcall (lispm-peer-deliver peer)
                              (format "[Reply from %s to request %s]\n%s" from id reply))))))
    (cond
     ((equal (format "%s" target) (lispm-peer-name peer)) (error "That is you"))
     ((and (not (lispm-get-agent target t)) (lispm-get-peer target))
      (lispm-ask-peer (lispm-get-peer target) request (lispm-peer-name peer)
                      (lambda (reply &optional _ended) (funcall answer target reply))
                      :id id))
     (t (lispm-delegate caller target request
                        (lambda (agent reply)
                          (funcall answer (symbol-name (lispm-agent-name agent)) reply))
                        :request-id id)))
    id))

(defun lispm-collab--peer-lines (&optional self)
  "Lines naming the peers, SELF, a peer's name, marked as the reader; or nil."
  (when-let* ((peers (lispm-peers)))
    (concat "\nAgents of other programs, reached the same way:\n"
            (mapconcat (lambda (peer)
                         (format "- %s%s (%s, in %s) [%s]" (lispm-peer-name peer)
                                 (if (equal (lispm-peer-name peer) self) " (you)" "")
                                 (lispm-peer-kind peer)
                                 (abbreviate-file-name (lispm-peer-root peer))
                                 (funcall (lispm-peer-state peer))))
                       peers "\n"))))

(defun lispm-collab-agents-text (&optional self)
  "What list_agents tells SELF, an agent or a peer's name, of who is here."
  (concat "Running agents:\n"
          (mapconcat (lambda (a)
                       (format "- %s%s (%s, in %s) [%s%s]" (lispm-agent-name a)
                               (if (eq a self) " (you)" "")
                               (lispm-agent-role a)
                               (abbreviate-file-name (lispm-agent-root a))
                               (lispm-agent-state a)
                               (if (lispm-agent-status a) (concat ", " (lispm-agent-status a)) "")))
                     (lispm-agents) "\n")
          (or (lispm-collab--peer-lines (and (stringp self) self)) "")
          (if-let* ((characters (lispm-collab--characters self t)))
              (concat "\nCharacters, reached by name, as @Ada, in a conversation of their own where you work:\n"
                      characters)
            "")
          "\nRoles:\n" (lispm-collab--roles)))

(defun lispm-collab--target (name &optional caller)
  "The agent NAME designates, which must not be CALLER (default: the acting agent).
NAME is as for `lispm-get-agent', or names a character, whose
conversation where CALLER works it then designates."
  (let ((agent (or (lispm-collab--character-agent name (or caller lispm-current-agent))
                   (lispm-get-agent name t)
                   (error "No agent or role named %s; list_agents, which search_tools finds, shows them" name))))
    (when (eq agent (or caller lispm-current-agent))
      (error "That is you"))
    agent))

(defun lispm-collab--cycle-p (from to)
  "Non-nil if FROM waiting on TO would close a cycle of waiting agents."
  (let ((seen nil))
    (cl-labels ((reaches (agent)
                  (or (eq agent from)
                      (unless (memq agent seen)
                        (push agent seen)
                        (seq-some #'reaches (lispm-agent-waiting-on agent))))))
      (reaches to))))

(defun lispm-collab--wait-changed (caller target &optional event)
  "Report the wait between CALLER and TARGET changing, with optional EVENT.
Both endpoints have data to refresh: CALLER's outgoing wait and
TARGET's reverse wait, as well as their request ids."
  (when (fboundp 'lispm-agents-roster-notify)
    (lispm-agents-roster-notify caller '(waits-on request-ids) event)
    (lispm-agents-roster-notify target '(waited-on-by request-ids))))

(defun lispm-collab--recipient (caller agent &optional root)
  "Resolve AGENT for CALLER; return (TARGET . HELPER-P).
A role starts a helper in ROOT or CALLER's root, bounded by CALLER.
An existing agent or character keeps its own conversation."
  (let* ((role (cond ((lispm-role-p agent) agent)
                     ((and (stringp agent) (not (lispm-get-agent agent t))
                           (not (lispm-get-character agent t)))
                      (lispm-get-role agent t))))
         (helper (and role (lispm-start role (or root (lispm-agent-root caller)))))
         (target (or helper (lispm-collab--target agent caller))))
    (when helper
      (lispm-set-ceiling helper (lispm-policy caller))
      (lispm-set-parent helper (lispm-agent-id caller)))
    (cons target (and helper t))))

(cl-defun lispm-delegate (caller agent request callback &key root call request-id)
  "Hand REQUEST from CALLER to AGENT, and call CALLBACK when it has replied.
AGENT is a running agent or its name, or a role or the name of one no
running agent has: a helper of that role then starts in ROOT (default:
CALLER's root), may do no more than CALLER, and retires once it has
replied.  CALLBACK gets the agent that replied and its final reply.
CALL, the id of CALLER's call whose result the reply is, if it is the
reply as `lispm-collab--replied' words it, makes the delegation outlive
Emacs: should Emacs stop before the reply, it still settles the call.
REQUEST-ID identifies a peer's request for mailbox and wait observers;
it does not replace CALL or change the durable reply-to route.
Return the agent asked."
  (pcase-let* ((`(,target . ,helper-p) (lispm-collab--recipient caller agent root)))
    (when (lispm-collab--cycle-p caller target)
      (error "%s is (indirectly) waiting on you; asking it would deadlock" (lispm-agent-name target)))
    (push target (lispm-agent-waiting-on caller))
    (lispm-collab--wait-changed caller target)
    (when call
      (lispm-collab--record-asked caller call target))
    (lispm-tell target request
                :from (lispm-agent-name caller)
                :request-id request-id
                :reply-to (and call (concat (lispm-agent-id caller) " " call (if helper-p " retire" "")))
                :callback (lambda (reply)
                            (setf (lispm-agent-waiting-on caller)
                                  (cl-remove target (lispm-agent-waiting-on caller) :count 1))
                            (lispm-collab--wait-changed
                             caller target
                             (when request-id
                               (list :type 'request-settled :request-id request-id
                                     :from (lispm-agent-name target)
                                     :to (lispm-agent-name caller))))
                            (when helper-p
                              (run-at-time 0 nil #'lispm-collab--retire target))
                            (funcall callback target reply)))
    target))

(defun lispm-collab--accepted (id name &optional session)
  "Receipt for the request ID accepted by NAME, optionally SESSION."
  (format "Request %s accepted by %s%s. Its final reply will arrive as a message. Continue other work, or end this turn if you have nothing else to do; do not poll for the reply."
          id name (if session (format " (%s)" session) "")))

(defun lispm-collab--reply-message (caller-id id from text)
  "Deliver request ID's reply TEXT from FROM to session CALLER-ID once.
Its stable inbox id also identifies a reply replayed after Emacs stops
between delivering the answer and finishing the recipient's turn."
  (let ((caller (or (lispm-resume-session caller-id)
                    (error "Cannot deliver request %s: session %s is unavailable" id caller-id))))
    (lispm-tell caller (format "[Reply to request %s]\n%s" id text)
                :from from :request-id id :id (concat "reply-" id))))

(defun lispm-collab-request (caller agent request &optional call)
  "Ask AGENT REQUEST for CALLER in the background; return an acceptance.
AGENT is a native agent, character, role or peer.  The final reply is
a new input to CALLER, never a pending tool result.  CALL, when given,
records a native recipient on the originating tool call for inspection.
Native requests and replies survive Emacs through the inbox; peer
requests, as with `lispm-ask-peer', last only while Emacs runs."
  ;; A standing caller may take up an older session when first opened;
  ;; capture that session's identity for the durable reply route.
  (lispm-session-buffer caller)
  (let ((id (concat "q" (lispm--new-id)))
        (caller-id (lispm-agent-id caller)))
    (if-let* ((peer (and (not (lispm-get-agent agent t))
                         (not (lispm-get-character agent t))
                         (lispm-get-peer agent))))
        (progn
          (lispm-ask-peer
           peer request (symbol-name (lispm-agent-name caller))
           (lambda (reply &optional ended)
             (lispm-collab--reply-message
              caller-id id (lispm-peer-name peer)
              (if ended reply (format "%s replied:\n%s" (lispm-peer-name peer) reply))))
           :id id)
          (lispm-collab--accepted id (lispm-peer-name peer)))
      (pcase-let* ((`(,target . ,helper-p) (lispm-collab--recipient caller agent))
                   (route (concat caller-id " " id " inbox" (if helper-p " retire" ""))))
        (lispm-tell target request :from (lispm-agent-name caller)
                    :request-id id :reply-to route
                    :callback (lispm-collab--callback target route))
        (when call
          (lispm-collab--record-asked caller call target))
        (lispm-collab--accepted id (lispm-agent-name target) (lispm-agent-id target))))))

(defun lispm-collab--replied (agent reply)
  "The text delivered when AGENT replied REPLY.
A value AGENT returned with it (see `lispm-reply-value') follows, as JSON."
  (concat (format "%s replied:\n%s" (lispm-agent-name agent) (substring-no-properties reply))
          (if-let* ((value (lispm-reply-value reply)))
              (concat "\n\nValue:\n" (lispm--json-pretty value))
            "")))

(defun lispm-collab--record-asked (caller call target)
  "Record on the heading of CALLER's CALL that it asked TARGET."
  (when-let* ((marker (plist-get (gethash call (lispm-agent-pending caller)) :marker))
              ((marker-buffer marker)))
    (lispm--in-session caller
      (lispm-transcript-set-property marker "ASKED" (lispm-agent-id target)))))

;;;; Agents working on their own

(defun lispm-collab-root (caller directory)
  "Root of the project around DIRECTORY, which is relative to CALLER's root.
Nil if DIRECTORY is not a directory."
  (let ((directory (file-name-as-directory (expand-file-name directory (lispm-agent-root caller)))))
    (and (file-directory-p directory) (lispm--project-root directory))))

(defun lispm-collab-start (caller role request &optional root)
  "Start an agent of ROLE in ROOT for CALLER, hand it REQUEST, and return it.
ROLE is a role or its name; ROOT defaults to CALLER's root.  Unlike a
helper (see `lispm-delegate'), the agent works on its own: nobody waits
for its reply, and it stays once it has replied, until the user ends
it.  Like a helper, it may do no more than CALLER, an agent: for each
permission class it gets the stricter of its role's and CALLER's
policy, for the rest of its life."
  (let ((agent (lispm-start (or (lispm-get-role role t)
                                (error "No role named %s; list_agents, which search_tools finds, shows the roles"
                                       role))
                            (or root (lispm-agent-root caller)))))
    (lispm-set-ceiling agent (lispm-policy caller))
    (lispm-set-parent agent (lispm-agent-id caller))
    (lispm-tell agent request :from (lispm-agent-name caller))
    agent))

(defun lispm-collab--start-elsewhere (agent tool input)
  "Make AGENT's call of TOOL on INPUT ask if it starts an agent elsewhere.
That is outside AGENT's project, where the agent started may do what
AGENT may do in its own, as AGENT's writes there, which always ask
\(see `lispm-tool-action'), could not."
  (when-let* (((equal (lispm-tool-name tool) "start_agent"))
              (project (and (listp input) (alist-get 'project input)))
              ((stringp project))
              (root (lispm-collab-root agent project))
              ((not (file-in-directory-p root (lispm-agent-root agent)))))
    (cons 'ask (format "It starts an agent in %s, outside the project of %s, that may do there what %s may do in its own."
                       (abbreviate-file-name root) (lispm-agent-name agent) (lispm-agent-name agent)))))

(add-hook 'lispm-tool-action-functions #'lispm-collab--start-elsewhere)

;;;; Delegations across restarts of Emacs

(defun lispm-collab--deliver (caller-id call agent reply)
  "Settle the call CALL of the session CALLER-ID with AGENT's REPLY.
The caller's session is opened again if it is not open: Emacs stopped
since it asked, and AGENT took the request up again."
  (when-let* ((caller (lispm-resume-session caller-id)))
    (setf (lispm-agent-waiting-on caller)
          (cl-remove agent (lispm-agent-waiting-on caller) :count 1))
    (lispm-collab--wait-changed caller agent)
    (when (eq (plist-get (gethash call (lispm-agent-pending caller)) :status) 'run)
      (lispm--call-finished caller call (lispm-collab--replied agent reply) nil nil))))

(defun lispm-collab--callback (agent reply-to)
  "The callback of AGENT's input asked for by REPLY-TO, after Emacs restarted.
REPLY-TO names the caller's session, request or call, and whether AGENT
is a helper, to retire once it has replied.  An `inbox' flag routes a
background request to a new message; old routes settle a pending call."
  (pcase (split-string reply-to)
    (`(,caller-id ,call . ,flags)
     (lambda (reply)
       (if (member "inbox" flags)
           (lispm-collab--reply-message caller-id call (lispm-agent-name agent)
                                       (lispm-collab--replied agent reply))
         (lispm-collab--deliver caller-id call agent reply))
       (when (member "retire" flags)
         (run-at-time 0 nil #'lispm-collab--retire agent))))))

(setq lispm-inbox-callback-function #'lispm-collab--callback)

(defun lispm-collab--answering-p (agent caller call)
  "Non-nil if AGENT has the request of CALLER's CALL, queued or under way."
  (let ((prefix (concat (lispm-agent-id caller) " " call)))
    (seq-some (lambda (entry)
                (when-let* ((reply-to (plist-get entry :reply-to)))
                  (or (equal reply-to prefix) (string-prefix-p (concat prefix " ") reply-to))))
              (cons (lispm-agent-entry agent) (lispm-agent-inbox agent)))))

(defun lispm-collab--follow (caller call id)
  "Have CALLER's CALL, which asked the session ID, wait for its reply again.
That session is opened again if it is not open, which takes the
request up; if it holds the request no longer, the reply was lost with
Emacs, and the call is recorded as interrupted."
  (when (eq (plist-get (gethash call (lispm-agent-pending caller)) :status) 'run)
    (let ((target (lispm-resume-session id)))
      (if (and target (lispm-collab--answering-p target caller call))
          (progn
            (push target (lispm-agent-waiting-on caller))
            (lispm-collab--wait-changed caller target))
        (lispm--call-finished caller call lispm--interrupted-result t nil)))))

(defun lispm-collab--take-up (caller call)
  "Take up CALLER's CALL of ask_agent, left running when Emacs stopped.
It is taken up if its heading records whom it asked: their reply,
once they have taken the request up again, settles it."
  (when-let* ((id (cdr (assoc "ASKED" (plist-get call :properties))))
              ((not (string-empty-p id))))
    (run-at-time 0 nil #'lispm-collab--follow caller (plist-get call :id) id)
    t))

(add-hook 'lispm-resume-call-functions #'lispm-collab--take-up)

(defun lispm-collab--retire (helper)
  "Retire HELPER after its reply, unless it has more work.
Follow-up input from the user or another agent keeps it alive, as
does a new turn that started before this retirement timer ran."
  (when (and (not (lispm-agent-busy-p helper))
             (null (lispm-agent-inbox helper)))
    (lispm-retire helper)))

(defun lispm-collab--characters (&optional self apps)
  "Lines naming the characters, but that of SELF, an agent; or nil.
Apps among them only with APPS non-nil, marked as such; else a line
says how many there are."
  (let* ((characters (seq-remove (lambda (character)
                                   (and (lispm-agent-p self)
                                        (eq (lispm-character-name character)
                                            (lispm-agent-character self))))
                                 (lispm-characters)))
         (left-out (if apps 0 (seq-count #'lispm-character-skill characters))))
    (when characters
      (string-join
       (append
        (mapcar (lambda (character)
                  (format "- %s%s: %s" (lispm-character-title* character)
                          (if (lispm-character-skill character) " (app)" "")
                          (or (lispm-character-description character) "(no description)")))
                (if apps characters (seq-remove #'lispm-character-skill characters)))
        (unless (zerop left-out)
          (list (format "- and %d apps, which list_agents names" left-out))))
       "\n"))))

(defun lispm-collab--roles ()
  "Description of the roles helpers can be started from."
  (mapconcat (lambda (role)
               (format "- %s: %s" (lispm-role-name role)
                       (or (lispm-role-description role) "(no description)")))
             (lispm-roles) "\n"))

(defun lispm-collab--prompt (agent)
  "System prompt section introducing AGENT's colleagues."
  (let ((others (remq agent (lispm-agents))))
    (concat "# Other agents\n"
            "ask_agent delegates to a running agent, or to a helper it starts from a"
            " role, returning a request id immediately; its final reply arrives as a new message."
            " Continue other work, or end your turn to await it; do not poll."
            " send_message notifies a running agent; start_agent, which"
            " search_tools finds, starts an agent of a role that works on its own and stays.\n"
            (if others
                (concat "Running:\n"
                        (mapconcat (lambda (a)
                                     (format "- %s (%s, in %s): %s" (lispm-agent-name a)
                                             (lispm-agent-role a)
                                             (abbreviate-file-name (lispm-agent-root a))
                                             (or (lispm-agent-description a) "")))
                                   others "\n")
                        "\n")
              "")
            "Roles:\n" (lispm-collab--roles)
            (if-let* ((characters (lispm-collab--characters agent)))
                (concat "\nCharacters, who are someone, not a role: reach one by its name, as @Ada,"
                        " with ask_agent or send_message, and it takes the request up in a"
                        " conversation of its own in your project, which goes on. When the user"
                        " addresses one with @, pass the request on to it rather than doing it yourself."
                        (if (seq-some #'lispm-character-skill (lispm-characters))
                            (concat " Apps are characters too, of two faces: their skill and its tools,"
                                    " which you use yourself when you know what to do (search_tools"
                                    " finds them), and themselves, whom you ask when a request needs"
                                    " their judgment, what they remember of the user, or many steps"
                                    " of their own.")
                          "")
                        "\n" characters)
              "")
            "\nName places as [[file:PATH::LINE]] and buffers as [[buffer:NAME]]:"
            " the tools of whoever reads them take these as files and buffers.")))

(add-hook 'lispm-system-prompt-functions #'lispm-collab--prompt)

(lispm-deftool list_agents ()
  "List the running agents (role, project, state, activity), the agents
of other programs in this Emacs, such as Claude Code, and the roles
helpers can be started from."
  :permission comm
  :deferred t
  (lispm-collab-agents-text lispm-current-agent))

(lispm-deftool send_message
    ((to string "Name of the receiving agent, or of a character.")
     (text string "The message, self-contained: they cannot see your transcript.  Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))
  "Send a message to another agent without waiting for an answer.  It
reaches them as a new turn as soon as they are free.  Use ask_agent
when you need a final reply delivered back automatically."
  :permission comm
  (lispm-collab-send lispm-current-agent to text))

(lispm-deftool ask_agent
    ((agent string "A running agent, a character (by its name, as Ada), or a role to start a helper from (list_agents, which search_tools finds, shows them).")
     (request string "What you need, self-contained: they cannot see your transcript.  Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))
  "Delegate a request in the background and immediately receive its
request id.  The final reply arrives as a new message with that id;
you remain free to work and answer the user.  Continue other work, or
end your turn if none remains; do not poll for the reply.
Ask a running agent better placed for it, or
name a role to start a helper of that role in your project; the
helper retires once it has replied.  When you need data back, such
as a list of places, ask for it: they can return a value with their
reply (return_value), which follows it as JSON."
  :permission comm
  (lispm-collab-request lispm-current-agent agent request lispm-current-call))

(lispm-deftool start_agent
    ((role string "The role to start the agent from (list_agents, which search_tools finds, shows the roles).")
     (request string "Its first task, self-contained: it cannot see your transcript.  Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]].")
     (project string "Directory of the project it works in, yours or another; default: yours." :optional t))
  "Start a native LispM agent of a role that works on its own, in your project or
another, and hand it a first request.  Nobody waits for its reply, and
it stays once it has replied, for the user and the other agents to
talk to: for work that goes on apart from yours.  If you need to hear
back, ask it in the request to send_message you; for an automatic final
reply to your inbox, use ask_agent.  The result names the agent, which
send_message, ask_agent and inspect_agent then reach.  It may do no
more than you."
  :permission comm
  :deferred t
  (let ((agent (lispm-collab-start
                lispm-current-agent role request
                (and project
                     (or (lispm-collab-root lispm-current-agent project)
                         (error "No directory %s" project))))))
    (format "Started %s (%s, in %s).  It works on its own and stays once it has replied: send_message and ask_agent reach it."
            (lispm-agent-name agent) (lispm-agent-role agent)
            (abbreviate-file-name (lispm-agent-root agent)))))

(lispm-register-tool
 (lispm-tool--create
  :name "return_value"
  :description
  "Return a value with your reply to the agent waiting for it, who asked you for something \(a [Message from agent X] request): data it can use as it is, such as a list of places as [[file:PATH::LINE]] links, the text of an org subtree, or any JSON.  Your reply still says what it is; a later call replaces the value."
  :input-schema '((type . "object")
                  (properties (value (description . "The value, any JSON.")))
                  (required . ["value"]))
  :permission 'comm
  :function
  (lambda (input)
    (let ((agent lispm-current-agent))
      (unless (lispm-agent-turn-callbacks agent)
        (error "Nobody waits for your reply in this turn; just reply"))
      (setf (lispm-agent-value agent) (alist-get 'value input))
      "The value will go with your reply."))))

(provide 'lispm-collab)
;;; lispm-collab.el ends here
