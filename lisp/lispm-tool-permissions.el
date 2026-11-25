;;; lispm-tool-permissions.el --- Tool permission decisions and session allowances  -*- lexical-binding: t; -*-

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

;; Permission policy evaluation, per-session call allowances, target
;; extraction, and outside-root write checks for tool calls.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-agents-context)
(require 'lispm-sandbox)
(require 'lispm-tool-core)

;;;; Permissions

(defun lispm-tools--in-root-p (agent file)
  "Non-nil if FILE lies under AGENT's root directory."
  (file-in-directory-p (expand-file-name file (lispm-principal-root agent))
                       (lispm-principal-root agent)))

(defvar tramp-local-host-regexp)

(defun lispm-tools-elsewhere-p (directory)
  "Non-nil if DIRECTORY is on another machine than Emacs.
That is a remote DIRECTORY (a TRAMP file name, such as /ssh:vm:/work/
or /docker:box:/work/) whose host is not this one: not one of the
names of the local host TRAMP knows (`tramp-local-host-regexp'), as
those of /sudo::/ and /su::/ are.  A shell command there cannot reach
Emacs, unless that machine reaches this one's files, as a container
does that mounts the user's home directory.  Without a host name, or
with no names of the local host to compare with, it is taken to be
this machine."
  (when-let* ((host (file-remote-p directory 'host)))
    (require 'tramp)
    (and (stringp tramp-local-host-regexp)
         (not (string-empty-p host))
         (not (string-match-p tramp-local-host-regexp host)))))

(defun lispm-tools-apart-p (agent)
  "Non-nil if AGENT's shell commands cannot reach Emacs.
They run on another machine (see `lispm-tools-elsewhere-p'), or on this
one in the sandbox, which keeps Emacs's server socket and configuration
from them (see `lispm-sandbox-confines-p').  Such commands cannot change
the agent's policy, so the user may allow every later one at once."
  (when agent
    (let ((root (lispm-principal-root agent)))
      (if (file-remote-p root)
          (lispm-tools-elsewhere-p root)
        (lispm-sandbox-confines-p)))))

(defun lispm-tools--call-key (input &optional tool)
  "INPUT as an exact-call approval for TOOL remembers it, without `why'.
Distinct top-level parameter names are sorted: their order does not
change the call.  Duplicate names, malformed objects and nested values
are kept as given.  For run_shell and run_program, `timeout' only limits the
execution time; the command, not that limit, is what was approved."
  (if (not (proper-list-p input))
      input
    (let ((key (seq-remove (lambda (entry) (eq (car-safe entry) 'why)) input))
          names)
      (if (not (seq-every-p (lambda (entry)
                             (and (consp entry) (symbolp (car entry))
                                  (not (memq (car entry) names))
                                  (push (car entry) names)))
                           key))
          key
        (when (and tool (member (lispm-tool-name tool) '("run_shell" "run_program"))
                   (eq (lispm-tool-permission tool) 'shell)
                   (let ((timeout (alist-get 'timeout key)))
                     (or (integerp timeout) (memq timeout '(nil :null)))))
          (setq key (assq-delete-all 'timeout key)))
        (sort key (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))))

(defun lispm-tools-allowance (tool input &optional agent)
  "What approving AGENT's call of TOOL on INPUT for good adds to its allowed.
The tool's name: every later call of it runs at once.  For a tool of
class `exec', which reaches all of Emacs, or a write outside the
agent's root, this exact call: (NAME . KEY), KEY from
`lispm-tools--call-key'.  Allowing every later call would put the agent
past its policy, which such a call can change.  So for a tool of class
`shell', since a shell command reaches Emacs too, unless AGENT's
commands are kept apart from Emacs (see `lispm-tools-apart-p'): on
another machine, or in the sandbox, they cannot change the policy, and
the tool is allowed.  Approving one call of a tool that always asks is
never approving the next."
  (if (or (lispm-tool-always-ask tool)
          (and agent (lispm-tools--outside-write-p agent tool input))
          (pcase (lispm-tool-permission tool)
            ('exec t)
            ('shell (not (lispm-tools-apart-p agent)))))
      (cons (lispm-tool-name tool) (lispm-tools--call-key input tool))
    (lispm-tool-name tool)))

(defun lispm-tools-invocation-allowed-p (agent tool input)
  "Non-nil if the user allowed AGENT this call of TOOL on INPUT for good.
A tool of class `shell' allowed as a whole was allowed while AGENT's
commands could not reach Emacs (see `lispm-tools-allowance'), and
holds only while that is so: with the sandbox off again, each command
asks, as it did before.  Writes outside AGENT's root and `always-ask'
tools require a per-call allowance rather than a bare tool name."
  (let ((allowed (lispm-principal-allowed agent))
        (name (lispm-tool-name tool))
        (key (lispm-tools--call-key input tool)))
    (or (and (member name allowed)
             (not (lispm-tool-always-ask tool))
             (not (lispm-tools--outside-write-p agent tool input))
             (or (not (eq (lispm-tool-permission tool) 'shell))
                 (lispm-tools-apart-p agent)))
        ;; Earlier approvals kept parameter order and shell timeouts.
        ;; Compare their keys too, without changing the stored entries.
        (seq-some (lambda (entry)
                    (and (consp entry) (equal (car entry) name)
                         (equal (lispm-tools--call-key (cdr entry) tool) key)))
                  allowed))))

(defun lispm-tools--outside-write-p (agent tool input)
  "Non-nil if AGENT's call of TOOL with INPUT writes outside AGENT's root."
  (and (eq (lispm-tool-permission tool) 'write)
       (let ((targets (lispm-tools--targets agent tool input)))
         (or (eq targets t)
             (seq-some (lambda (file) (not (lispm-tools--in-root-p agent file)))
                       targets)))))

(defun lispm-tools--targets (agent tool input)
  "Files AGENT's call of TOOL with INPUT changes, as far as can be told before.
Those TOOL's targets function returns, if it has one (see
`lispm-deftool'); else those a `path' argument names, and the file of
the buffer a `buffer' argument names, either of them a reference (see
`lispm-tools-reference').  t if the targets function fails: the call may
change any file."
  (if-let* ((targets (lispm-tool-targets tool)))
      (condition-case nil
          (let ((lispm-current-agent agent)
                (default-directory (lispm-principal-root agent)))
            (apply targets (lispm-tools-arguments tool input)))
        (error t))
    (when (listp input)
      (let ((path (alist-get 'path input))
            (buffer (and (stringp (alist-get 'buffer input))
                         (get-buffer (lispm-tools-buffer-name (alist-get 'buffer input))))))
        ;; The file a path names is found as the tool finds it.
        (condition-case nil
            (delq nil (list (and (stringp path)
                                 (let ((lispm-current-agent agent))
                                   (lispm-tools-file path)))
                            (and buffer (buffer-file-name buffer))))
          (error t))))))

(defvar lispm-tool-action-functions nil
  "Functions that may hold a tool call back for approval, or refuse it.
Each is called with the agent, the tool (a `lispm-tool') and the input
of a call LispM's own rules do not deny (see `lispm-tool-action'), and
returns nil, to leave the call as those rules decide it; `ask', for it
to await the user's approval; or `deny', to refuse it.  Either may
come with a reason, as (ask . REASON) or (deny . REASON), REASON a
sentence: the approval shows it, and a denial tells it the model.

The strictest answer wins, so a function can make a call ask or be
refused, never let one through that would ask or be refused: what
the user allowed for good is asked about again while a function asks.
A function that signals an error, or answers anything else, asks:
an error never lets a call through.  Functions are called for every
call before it is decided, and again whenever it is decided anew, so
they should be quick and answer alike for the same call.")

(defun lispm-tools--answer (function agent tool input)
  "What FUNCTION of `lispm-tool-action-functions' answers for AGENT's call
of TOOL on INPUT: nil, or (ACTION . REASON).  An answer without a reason
gets one naming FUNCTION."
  (let ((name (if (symbolp function)
                  (format "`%s'" function)
                "A function of `lispm-tool-action-functions'")))
    (condition-case err
        (pcase (funcall function agent tool input)
          ('nil nil)
          ((or (and (or 'ask 'deny) action (let reason nil))
               (and `(,(and (or 'ask 'deny) action) . ,reason)
                    (guard (or (null reason) (stringp reason)))))
           (cons action (or reason (format (if (eq action 'ask) "%s asks about it." "%s refused it.")
                                           name))))
          (answer (cons 'ask (format "%s answered %S, which is neither ask nor deny."
                                     name answer))))
      (error (cons 'ask (format "%s failed: %s." name (error-message-string err)))))))

(defun lispm-tool-decision (agent tool input)
  "Whether AGENT may run TOOL on INPUT, and why not: (ACTION . REASONS).
ACTION is `auto', `ask' or `deny', what LispM's own rules decide (see
`lispm-tool-action') or, if stricter, what a function of
`lispm-tool-action-functions' answers.  REASONS are the reasons of the
functions answering ACTION, a list of strings; LispM's own rules give
none."
  (let ((action (lispm-tools--action agent tool input))
        answers)
    (unless (eq action 'deny)
      (run-hook-wrapped 'lispm-tool-action-functions
                        (lambda (function)
                          (when-let* ((answer (lispm-tools--answer function agent tool input)))
                            (push answer answers)
                            (setq action (lispm-stricter action (car answer))))
                          nil)))
    (cons action (delq nil (mapcar (lambda (answer) (and (eq (car answer) action) (cdr answer)))
                                   (nreverse answers))))))

(defun lispm-tools-denial (agent tool reasons)
  "What the model is told of AGENT's call of TOOL, denied for REASONS.
REASONS as `lispm-tool-decision' gives them; with none, the policy
denied it."
  (if reasons
      (concat "Denied: " (string-join reasons " "))
    (format "Denied by policy: %s may not use %s tools."
            (lispm-principal-name agent) (lispm-tool-permission tool))))

(defun lispm-tool-action (agent tool input)
  "Whether AGENT may run TOOL on INPUT: `auto', `ask' or `deny'.
The policy decides by the class of the call, the tool's unless the
call does less (see `lispm-tool-call-permission').  Calls the user
allowed for this Emacs session run at once (see
`lispm-tools-allowance').  Writes outside the agent's root always ask,
whatever the policy says: to files named by a `path' argument, visited
by a buffer a `buffer' argument names, or that the tool's targets
function returns (see `lispm-deftool'), as a write whose files that
function cannot tell does.  So do the calls of a tool that always asks
(see `lispm-deftool'), such as write_skill.  Then the functions of
`lispm-tool-action-functions' may have a call ask, or deny it; see
`lispm-tool-decision' for why."
  (car (lispm-tool-decision agent tool input)))

(defun lispm-tool-call-permission (tool input)
  "The class of TOOL's call on INPUT: the tool's, unless the call does less.
A tool with a class function (`:class' in `lispm-deftool') has it say
which class the call belongs to, as run_shell says `read' of a command
that only reads (see `lispm-shell-read-only-p'); the policy then
decides the call as it decides that class.  A function that answers
nil, or fails, leaves the call in the tool's class."
  (or (when-let* ((class (lispm-tool-class tool)))
        (condition-case nil
            (apply class (lispm-tools-arguments tool input))
          (error nil)))
      (lispm-tool-permission tool)))

(defvar lispm-auto-approve-all nil
  "When non-nil, calls whose policy is `auto' never ask for approval.
This lets unattended batch runs use `always-ask' tools and write
outside the agent's root when the policy for their class is `auto'.")

(defun lispm-tools--action (agent tool input)
  "What LispM's own rules decide of AGENT's call of TOOL on INPUT.
See `lispm-tool-action'."
  (let ((action (lispm-policy-action agent (lispm-tool-call-permission tool input))))
    (cond ((eq action 'deny) 'deny)
          ((lispm-tools-invocation-allowed-p agent tool input) 'auto)
          ((and (eq action 'auto) (not lispm-auto-approve-all)
                (lispm-tool-always-ask tool))
           'ask)
          ((and (eq action 'auto) (not lispm-auto-approve-all)
                (lispm-tools--outside-write-p agent tool input))
           'ask)
          (t action))))


(provide 'lispm-tool-permissions)

;;; lispm-tool-permissions.el ends here
