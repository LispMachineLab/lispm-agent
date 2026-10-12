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

;; The agent object model.  An agent is a long-lived value inside the
;; running Emacs, not a process outside it.  It has
;;
;;   - an identity: name, one-line description, role instructions and
;;     a root directory it works in;
;;   - a capability set: the tools it may call and a permission policy
;;     saying which tool classes run freely, need approval, or are
;;     forbidden;
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
    (rotom-messages :api anthropic :url "http://127.0.0.1:14550" :model "gpt-5.5")
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
\(GPT, Grok, ...) behind both shapes.  Its Chat Completions side
streams faithfully; its Messages side, `rotom-messages', gets plain
requests, as its translation loses the arguments of parallel tool
calls when streaming (RyanKung/rotom#18) and rejects is_error (#19)."
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

(defvar lispm-turn-end-functions nil
  "Abnormal hook run with an agent and its final reply after each turn.")

(cl-defstruct (lispm-agent (:constructor lispm-agent--create)
                           (:copier nil))
  "An agent living in this Emacs."
  ;; Identity and capabilities, set by `lispm-defagent'.
  name description instructions root tools policy backend model effort
  ;; Runtime state.
  (state 'idle)              ; idle, requesting, acting, waiting
  status                     ; short description of the current activity
  buffer                     ; current session buffer
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
  triggers                   ; hook and timer subscriptions
  (usage (list 0 0 0)))      ; input, output, cache-read tokens

(defvar lispm--agents nil
  "Alist of (NAME . AGENT) in definition order.")

(defun lispm-agents ()
  "Return all defined agents."
  (mapcar #'cdr lispm--agents))

(defun lispm-get-agent (agent &optional noerror)
  "Return the agent designated by AGENT: an agent, a symbol or a string.
Signal an error if there is none, unless NOERROR."
  (cond ((lispm-agent-p agent) agent)
        ((alist-get (if (stringp agent) (intern agent) agent) lispm--agents))
        (noerror nil)
        (t (user-error "No agent named %s" agent))))

(defun lispm-register-agent (name &rest props)
  "Define or redefine the agent NAME from PROPS.
PROPS are :description, :instructions, :root, :tools (a list of tool
names, or t for all), :policy, :backend, :model and :effort.  Redefining a
live agent keeps its runtime state."
  (let ((agent (or (alist-get name lispm--agents)
                   (let ((new (lispm-agent--create :name name)))
                     (setq lispm--agents
                           (append lispm--agents (list (cons name new))))
                     new))))
    (setf (lispm-agent-description agent) (plist-get props :description)
          (lispm-agent-instructions agent) (plist-get props :instructions)
          (lispm-agent-root agent) (file-name-as-directory
                                    (expand-file-name
                                     (or (plist-get props :root) "~/")))
          (lispm-agent-tools agent) (if (plist-member props :tools)
                                        (plist-get props :tools)
                                      t)
          (lispm-agent-policy agent) (plist-get props :policy)
          (lispm-agent-backend agent) (plist-get props :backend)
          (lispm-agent-model agent) (plist-get props :model)
          (lispm-agent-effort agent) (plist-get props :effort))
    agent))

(defmacro lispm-defagent (name &rest props)
  "Define an agent NAME.
An optional leading string is its one-line description.  The other
PROPS are as for `lispm-register-agent'.

  (lispm-defagent coder
    \"Writes and fixes Emacs Lisp.\"
    :root \"~/src/lispm/\"
    :instructions \"Keep changes small; run the tests after editing.\"
    :policy \\='((write . auto)))"
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

(defun lispm-agent-directory (agent)
  "Directory of AGENT's memory, tasks and sessions, created on demand."
  (let ((dir (expand-file-name (format "agents/%s/" (lispm-agent-name agent))
                               lispm-directory)))
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
