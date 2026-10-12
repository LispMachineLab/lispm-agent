;;; lispm-collab.el --- Agents working together  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Several agents share one Emacs: the same buffers, files and event
;; loop.  They coordinate through three tools:
;;
;;   list_agents   who else is here, and what they are doing;
;;   send_message  fire and forget: the text lands in the other agent's
;;                 inbox and becomes a "* Message from X" turn;
;;   ask_agent     delegation: the caller's call stays pending (other
;;                 agents and the user keep working) until the other
;;                 agent's turn ends; its final reply is the result.
;;
;; Shared state needs no locks: the event loop is single-threaded, and
;; edits are exact-text replacements, which fail instead of clobbering
;; when someone else changed the text first (optimistic concurrency).
;; What does need care is waiting: ask_agent refuses a request that
;; would make agents wait on each other in a cycle.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

(defun lispm-collab--target (name)
  "The agent called NAME, which must not be the acting agent."
  (let ((agent (or (lispm-get-agent name t)
                   (error "No agent named %s; list_agents shows who exists" name))))
    (when (eq agent lispm-current-agent)
      (error "That is you"))
    agent))

(defun lispm-collab--cycle-p (from to)
  "Non-nil if FROM waiting on TO would close a cycle of waiting agents."
  (let ((agent to) (seen nil))
    (while (and agent (not (eq agent from)) (not (memq agent seen)))
      (push agent seen)
      (setq agent (lispm-agent-waiting-on agent)))
    (eq agent from)))

(defun lispm-collab--prompt (agent)
  "System prompt section introducing AGENT's colleagues."
  (let ((others (remq agent (lispm-agents))))
    (when others
      (concat "# Other agents in this Emacs\n"
              "Delegate with ask_agent, notify with send_message.\n"
              (mapconcat (lambda (a)
                           (format "- %s: %s" (lispm-agent-name a)
                                   (or (lispm-agent-description a) "(no description)")))
                         others "\n")))))

(add-hook 'lispm-system-prompt-functions #'lispm-collab--prompt)

(lispm-deftool list_agents ()
  "List the agents in this Emacs: their role, state and current activity."
  :permission comm
  (mapconcat (lambda (a)
               (format "%s%s: %s [%s%s]" (lispm-agent-name a)
                       (if (eq a lispm-current-agent) " (you)" "")
                       (or (lispm-agent-description a) "no description")
                       (lispm-agent-state a)
                       (if (lispm-agent-status a) (concat ", " (lispm-agent-status a)) "")))
             (lispm-agents) "\n"))

(lispm-deftool send_message
    ((to string "Name of the receiving agent.")
     (text string "The message, self-contained: they cannot see your transcript."))
  "Send a message to another agent without waiting for an answer.  It
reaches them as a new turn as soon as they are free.  Use ask_agent
when you need the answer to continue."
  :permission comm
  (let ((target (lispm-collab--target to)))
    (lispm-tell target text :from (lispm-agent-name lispm-current-agent))
    (format "Delivered to %s%s." to
            (if (lispm-agent-busy-p target) ", who is busy and will read it next" ""))))

(lispm-deftool ask_agent
    ((agent string "Name of the agent to ask.")
     (request string "What you need, self-contained: they cannot see your transcript."))
  "Delegate a request to another agent and wait for its final reply,
which becomes the result of this call.  Use it to get work done by
an agent better placed for it (see list_agents)."
  :permission comm
  :async t
  (let ((me lispm-current-agent)
        (target (lispm-collab--target agent)))
    (when (lispm-collab--cycle-p me target)
      (error "%s is (indirectly) waiting on you; asking it would deadlock" agent))
    (setf (lispm-agent-waiting-on me) target)
    (lispm-tell target request
                :from (lispm-agent-name me)
                :callback (lambda (reply)
                            (when (eq (lispm-agent-waiting-on me) target)
                              (setf (lispm-agent-waiting-on me) nil))
                            (funcall done (format "%s replied:\n%s" agent reply))))
    nil))

(provide 'lispm-collab)
;;; lispm-collab.el ends here
