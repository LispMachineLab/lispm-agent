;;; lispm-collab.el --- Agents working together  -*- lexical-binding: t; -*-

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

;; Several agents share one Emacs: the same buffers, files and event
;; loop.  They coordinate through three tools:
;;
;;   list_agents   who else is here, what they are doing, and which
;;                 roles helpers can be started from;
;;   send_message  fire and forget: the text lands in the other agent's
;;                 inbox and becomes a "* Message from X" turn;
;;   ask_agent     delegation: the caller's call stays pending (other
;;                 agents and the user keep working) until the other
;;                 agent's turn ends; its final reply is the result.
;;                 Asking a role rather than an agent starts a helper
;;                 of that role in the caller's project, which retires
;;                 once it has replied: a subagent.  A helper may do no
;;                 more than its caller: for each permission class it
;;                 gets the stricter of its role's and its caller's
;;                 policy.
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
                   (error "No agent or role named %s; list_agents shows them" name))))
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
            "Roles:\n" (lispm-collab--roles))))

(add-hook 'lispm-system-prompt-functions #'lispm-collab--prompt)

(lispm-deftool list_agents ()
  "List the running agents (role, project, state, activity) and the roles
helpers can be started from."
  :permission comm
  (concat "Running agents:\n"
          (mapconcat (lambda (a)
                       (format "- %s%s (%s, in %s) [%s%s]" (lispm-agent-name a)
                               (if (eq a lispm-current-agent) " (you)" "")
                               (lispm-agent-role a)
                               (abbreviate-file-name (lispm-agent-root a))
                               (lispm-agent-state a)
                               (if (lispm-agent-status a) (concat ", " (lispm-agent-status a)) "")))
                     (lispm-agents) "\n")
          "\nRoles:\n" (lispm-collab--roles)))

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
    ((agent string "A running agent, or a role to start a helper from (see list_agents).")
     (request string "What you need, self-contained: they cannot see your transcript."))
  "Delegate a request and wait for the final reply, which becomes the
result of this call.  Ask a running agent better placed for it, or
name a role to start a helper of that role in your project; the
helper retires once it has replied."
  :permission comm
  :async t
  (let* ((me lispm-current-agent)
         (helper (and (not (lispm-get-agent agent t)) (lispm-get-role agent t)
                      (lispm-start (intern agent) (lispm-agent-root me))))
         (target (or helper (lispm-collab--target agent))))
    (when (lispm-collab--cycle-p me target)
      (error "%s is (indirectly) waiting on you; asking it would deadlock" agent))
    ;; Delegating must not widen what the caller may do.
    (when helper
      (setf (lispm-agent-ceiling helper) (lispm-policy me)))
    (setf (lispm-agent-waiting-on me) target)
    (lispm-tell target request
                :from (lispm-agent-name me)
                :callback (lambda (reply)
                            (when (eq (lispm-agent-waiting-on me) target)
                              (setf (lispm-agent-waiting-on me) nil))
                            (when helper
                              (run-at-time 0 nil #'lispm-retire helper))
                            (funcall done (format "%s replied:\n%s"
                                                  (lispm-agent-name target) reply))))
    nil))

(provide 'lispm-collab)
;;; lispm-collab.el ends here
