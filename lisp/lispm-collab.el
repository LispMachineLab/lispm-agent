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
;; loop.  They coordinate through three tools:
;;
;;   list_agents   who else is here, what they are doing, and which
;;                 roles helpers can be started from;
;;   send_message  fire and forget: the text lands in the other agent's
;;                 inbox and becomes a "* Message from X" turn;
;;   ask_agent     delegation: the caller's call stays pending (other
;;                 agents and the user keep working) until the other
;;                 agent's turn ends; its final reply is the result,
;;                 with a value it returned (return_value), if any.
;;                 Asking a role rather than an agent starts a helper
;;                 of that role in the caller's project, which retires
;;                 once it has replied: a subagent.  A helper may do no
;;                 more than its caller: for each permission class it
;;                 gets the stricter of its role's and its caller's
;;                 policy.
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
;; Emacs stops before the reply, opening either session again takes
;; the delegation up: the one asked goes on with the request, or gets
;; it, and its reply settles the call; a helper stays bounded by its
;; caller, whose ceiling its session records.
;;
;; Shared state needs no locks: the event loop is single-threaded, and
;; edits are exact-text replacements, which fail instead of clobbering
;; when someone else changed the text first (optimistic concurrency).
;; What does need care is waiting: ask_agent refuses a request that
;; would make agents wait on each other in a cycle.  An agent that asks
;; several in one response waits on all of them at once.
;;
;; Agents of other programs work in the same Emacs too, as Claude Code
;; and Codex do in their terminals (lispm-agents-bridge.el): peers.  The
;; three tools reach them as they reach LispM's agents, and peers reach
;; every agent in turn.  What comes to a peer is put to it as its user's
;; next message: [Message from X], [Request R1 from X], which it answers
;; with a reply of its own, and [Reply from X to request R1].  Requests
;; asked of peers live as long as Emacs.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

;;;; Peers: agents of other programs

(cl-defstruct (lispm-peer (:constructor lispm-peer-create)
                          (:copier nil))
  "An agent of another program working in this Emacs, as Claude Code.
NAME, a string, names it to the agents; KIND says what it is, \"Claude
Code\"; ROOT is the directory it works in; STATE, a function, returns
what it is doing, as \"idle\"; DELIVER is a function of a text putting
the text to it as its user's next message, once it is free for it,
which returns non-nil if it was given the text at once."
  name kind root state deliver)

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
  "Requests asked of peers, awaiting their reply: (ID PEER CALLBACK).
PEER is the name of the peer asked; CALLBACK takes its reply and, when
it ended without one, non-nil.")

(defvar lispm-collab--request-count 0
  "Requests asked by or of peers since Emacs started.")

(defun lispm-collab-new-request-id ()
  "Return the id of a new request asked by or of a peer: R1, R2...."
  (format "R%d" (cl-incf lispm-collab--request-count)))

(defun lispm-tell-peer (peer text from)
  "Give PEER the message TEXT from FROM, the name of an agent or a peer.
Return non-nil if PEER was given it at once, rather than once free."
  (funcall (lispm-peer-deliver peer)
           (format "[Message from %s]\n%s\n\n(Answer, if need be, with your send_message tool, to %s.)"
                   from text from)))

(cl-defun lispm-ask-peer (peer request from callback &key id)
  "Ask PEER REQUEST on behalf of FROM, a name; return the request's id.
CALLBACK is called with PEER's reply, or with a text saying it ended
without one and non-nil.  ID is the request's id, a new one by default."
  (let ((id (or id (lispm-collab-new-request-id))))
    (push (list id (lispm-peer-name peer) callback) lispm-collab--requests)
    (condition-case err
        (funcall (lispm-peer-deliver peer)
                 (format "[Request %s from %s]\n%s\n\n(%s waits for your answer: give it with your reply tool, request %s.)"
                         id from request from id))
      (error (setq lispm-collab--requests (cl-remove id lispm-collab--requests :key #'car :test #'equal))
             (signal (car err) (cdr err))))
    id))

(defun lispm-peer-reply (peer id text)
  "Settle the request ID asked of PEER with its reply TEXT.
An error if no request ID awaits PEER's reply."
  (let ((entry (assoc id lispm-collab--requests)))
    (unless (and entry (equal (nth 1 entry) (lispm-peer-name peer)))
      (error "No request %s awaits your reply" id))
    (setq lispm-collab--requests (delq entry lispm-collab--requests))
    (funcall (nth 2 entry) text)))

(defun lispm-collab-peer-ended (name)
  "Settle the requests asked of the peer NAME, which ended without replying."
  (dolist (entry (seq-filter (lambda (entry) (equal (nth 1 entry) name)) lispm-collab--requests))
    (setq lispm-collab--requests (delq entry lispm-collab--requests))
    (with-demoted-errors "lispm request callback: %S"
      (funcall (nth 2 entry) (format "%s ended without replying." name) t))))

(defun lispm-collab--name (sender)
  "The name of SENDER, an agent or the name of a peer, as a string."
  (if (lispm-agent-p sender) (symbol-name (lispm-agent-name sender)) sender))

(defun lispm-collab-send (from to text)
  "Send TEXT from FROM, an agent or a peer's name, to the agent or peer TO.
Return what the sender is told."
  (let ((agent (lispm-get-agent to t))
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
                   (funcall (lispm-peer-deliver peer)
                            (format "[Reply from %s to request %s]\n%s" from id reply)))))
    (cond
     ((equal (format "%s" target) (lispm-peer-name peer)) (error "That is you"))
     ((and (not (lispm-get-agent target t)) (lispm-get-peer target))
      (lispm-ask-peer (lispm-get-peer target) request (lispm-peer-name peer)
                      (lambda (reply &optional _ended) (funcall answer target reply))
                      :id id))
     (t (lispm-delegate caller target request
                        (lambda (agent reply)
                          (funcall answer (symbol-name (lispm-agent-name agent)) reply)))))
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
          "\nRoles:\n" (lispm-collab--roles)))

(defun lispm-collab--target (name &optional caller)
  "The agent NAME designates, which must not be CALLER (default: the acting agent).
NAME is as for `lispm-get-agent'."
  (let ((agent (or (lispm-get-agent name t)
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

(cl-defun lispm-delegate (caller agent request callback &key root call)
  "Hand REQUEST from CALLER to AGENT, and call CALLBACK when it has replied.
AGENT is a running agent or its name, or a role or the name of one no
running agent has: a helper of that role then starts in ROOT (default:
CALLER's root), may do no more than CALLER, and retires once it has
replied.  CALLBACK gets the agent that replied and its final reply.
CALL, the id of CALLER's call whose result the reply is, if it is the
reply as `lispm-collab--replied' words it, makes the delegation outlive
Emacs: should Emacs stop before the reply, it still settles the call.
Return the agent asked."
  (let* ((role (cond ((lispm-role-p agent) agent)
                     ((and (stringp agent) (not (lispm-get-agent agent t)))
                      (lispm-get-role agent t))))
         (helper (and role (lispm-start role (or root (lispm-agent-root caller)))))
         (target (or helper (lispm-collab--target agent caller))))
    (when (lispm-collab--cycle-p caller target)
      (error "%s is (indirectly) waiting on you; asking it would deadlock" (lispm-agent-name target)))
    ;; Delegating must not widen what the caller may do.
    (when helper
      (lispm-set-ceiling helper (lispm-policy caller)))
    (push target (lispm-agent-waiting-on caller))
    (when call
      (lispm-collab--record-asked caller call target))
    (lispm-tell target request
                :from (lispm-agent-name caller)
                :reply-to (and call (concat (lispm-agent-id caller) " " call (if helper " retire" "")))
                :callback (lambda (reply)
                            (setf (lispm-agent-waiting-on caller)
                                  (cl-remove target (lispm-agent-waiting-on caller) :count 1))
                            (when helper
                              (run-at-time 0 nil #'lispm-collab--retire helper))
                            (funcall callback target reply)))
    target))

(defun lispm-collab--replied (agent reply)
  "What ask_agent returns when AGENT replied REPLY.
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

;;;; Delegations across restarts of Emacs

(defun lispm-collab--deliver (caller-id call agent reply)
  "Settle the call CALL of the session CALLER-ID with AGENT's REPLY.
The caller's session is opened again if it is not open: Emacs stopped
since it asked, and AGENT took the request up again."
  (when-let* ((caller (lispm-resume-session caller-id)))
    (setf (lispm-agent-waiting-on caller)
          (cl-remove agent (lispm-agent-waiting-on caller) :count 1))
    (when (eq (plist-get (gethash call (lispm-agent-pending caller)) :status) 'run)
      (lispm--call-finished caller call (lispm-collab--replied agent reply) nil nil))))

(defun lispm-collab--callback (agent reply-to)
  "The callback of AGENT's input asked for by REPLY-TO, after Emacs restarted.
REPLY-TO, as `lispm-delegate' wrote it, names the caller's session and
call, and whether AGENT is a helper, to retire once it has replied."
  (pcase (split-string reply-to)
    (`(,caller-id ,call . ,flags)
     (lambda (reply)
       (lispm-collab--deliver caller-id call agent reply)
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
          (push target (lispm-agent-waiting-on caller))
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
  "Retire HELPER, which has replied, unless the user has since written to it.
Then it stays, to answer them, as an agent of its own."
  (unless (seq-some (lambda (entry) (not (or (plist-get entry :from) (plist-get entry :event))))
                    (lispm-agent-inbox helper))
    (lispm-retire helper)))

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
            " role; send_message notifies a running agent.\n"
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
    ((to string "Name of the receiving agent.")
     (text string "The message, self-contained: they cannot see your transcript.  Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))
  "Send a message to another agent without waiting for an answer.  It
reaches them as a new turn as soon as they are free.  Use ask_agent
when you need the answer to continue."
  :permission comm
  (lispm-collab-send lispm-current-agent to text))

(lispm-deftool ask_agent
    ((agent string "A running agent, or a role to start a helper from (list_agents, which search_tools finds, shows them).")
     (request string "What you need, self-contained: they cannot see your transcript.  Name places as [[file:PATH::LINE]], buffers as [[buffer:NAME]]."))
  "Delegate a request and wait for the final reply, which becomes the
result of this call.  Ask a running agent better placed for it, or
name a role to start a helper of that role in your project; the
helper retires once it has replied.  When you need data back, such
as a list of places, ask for it: they can return a value with their
reply (return_value), which follows it as JSON."
  :permission comm
  :async t
  (if-let* ((peer (and (not (lispm-get-agent agent t)) (lispm-get-peer agent))))
      (lispm-ask-peer peer request (symbol-name (lispm-agent-name lispm-current-agent))
                      (lambda (reply &optional ended)
                        (funcall done (if ended reply (format "%s replied:\n%s" agent reply))
                                 ended)))
    (lispm-delegate lispm-current-agent agent request
                    (lambda (target reply)
                      (funcall done (lispm-collab--replied target reply)))
                    ;; A call the model made, not one a Lisp program does.
                    :call lispm-current-call))
  nil)

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
