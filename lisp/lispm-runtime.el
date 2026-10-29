;;; lispm-runtime.el --- The agent execution loop  -*- lexical-binding: t; -*-

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

;; The loop that keeps agents running inside the Emacs event loop.
;; Nothing here blocks: a model request is a curl process whose output
;; streams into the session buffer, tool calls run as ordinary Lisp
;; (or as processes, for shell commands), and approvals are org TODO
;; state changes the user makes whenever they like.  Several agents
;; can therefore be mid-turn at once, interleaved by the command loop.
;;
;; A turn:
;;
;;   input (typed, or from the inbox: another agent, a hook, a timer)
;;     -> request: parse the session buffer, stream the response into it
;;     -> response with tool calls: each call runs, waits for approval
;;        (TODO state WAITING) or is refused, per the agent's policy;
;;        a running call may wait, too, for approval of a call it makes
;;        in turn, as a Lisp program does (`lispm-call-ask');
;;        when every call has a result, request again
;;     -> response without tool calls: the turn ends, the session is
;;        saved (and committed), turn callbacks and hooks run, and the
;;        next inbox item, if any, starts a new turn.
;;
;; A turn that stops early (a request fails, the step limit is reached)
;; does not end: it waits at a condition, recorded in the transcript,
;; until a restart is chosen, by a handler or by the user, as a call
;; waits for approval.  See "Conditions and restarts" below.
;;
;; None of it is lost when Emacs stops.  The session is saved as a
;; turn goes, and the inbox in the project's inbox file
;; (lispm-inbox.el); opening a session again takes up the turn it was
;; in (`lispm--resume-calls'), and what was queued for it.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'project)
(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-inbox)
(require 'lispm-journal)

(declare-function lispm-git-commit "lispm-memory" (agent message))

(defcustom lispm-save-sessions t
  "Whether to save session files at the end of every turn."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-compact-threshold 120000
  "Size, in tokens, past which a session makes room or is handed over.
The size is the one the model's latest response reported, plus an
estimate of what was written since.  Past it, the output of older
tool calls is cleared first (see `lispm-compact-keep-results').  If
that is not enough, then before the next turn starts the agent saves
what should outlive the conversation to memory and tasks, writes a
summary, and the conversation goes on in a continuation file holding
the summary and the latest turns (see `lispm-compact-keep-turns').
nil does neither, and hands over only on \\[lispm-compact]."
  :type '(choice (const :tag "Only on demand" nil) integer)
  :group 'lispm)

(defcustom lispm-compact-save-memory t
  "Whether an agent saves memory and tasks before its session is handed over."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-compact-keep-results 10
  "How many of a session's latest tool results are never cleared.
When a session grows past `lispm-compact-threshold', the output of
older calls is cleared before the session is handed over: requests
carry a line saying so in its place, and the result's heading is
tagged :cleared: (remove the tag to send it again).  nil never clears."
  :type '(choice (const :tag "Never clear" nil) integer)
  :group 'lispm)

(defcustom lispm-compact-clear-at-least 10000
  "Fewest tokens clearing old tool results must free to be done.
Clearing changes an early part of the conversation, so the model's
cache of it is built again; a small gain is not worth that, and the
session is handed over instead."
  :type 'integer
  :group 'lispm)

(defcustom lispm-compact-keep-turns 2
  "How many of the latest turns a handover keeps as they were.
They follow the summary in the continuation, unless they would take
more than a quarter of `lispm-compact-threshold'; then fewer are
kept.  The turn that saved memory for the handover is never kept, and
at least one turn is always left to the summary.  0 keeps none."
  :type 'integer
  :group 'lispm)

(defcustom lispm-compact-idle 0.75
  "Fraction of `lispm-compact-threshold' past which a session is compacted at idle.
When a turn ends with the session past it, LispM makes room once Emacs
has been idle for `lispm-idle-delay' seconds, rather than when the
next request comes: the output of older tool calls is cleared, as past
the threshold, and if the session is still past this fraction, it is
handed over.  The agent first saves memory, as before any handover
\(`lispm-compact-save-memory'), in a turn of its own; then the summary
is written in the background while the agent is free.  Input given it
meanwhile is taken up at once, in the session as it is, and the
summary, out of date, is thrown away; so is the handover at idle, for
that session: it is tried once.  A session past the threshold when a
turn starts is still handed over then, taking the summary on its way,
if any, and not saving memory again.  nil compacts only then."
  :type '(choice (const :tag "Only when a turn starts" nil) number)
  :group 'lispm)

(defcustom lispm-approval-functions nil
  "Hook run with an agent, a tool name and its input when a call awaits approval.
Use it for notifications, e.g. with the alert package."
  :type 'hook
  :group 'lispm)

(defvar lispm-approval-message-functions nil
  "Functions saying where the user answers a call awaiting approval.
Each is called with the agent and the tool's name, and returns the
message to give, t to give none, as when the call is in sight, or nil
to leave it to the others; without one, the chat or the session is
named.")

(defvar lispm-inhibit-triggers nil
  "Non-nil while the runtime itself causes hook runs that are not events.")

(defvar lispm--idle-handovers (make-hash-table :test #'eq :weakness 'key)
  "Agents handing their session over at idle, to the step they are at.
That is `saving' while the agent saves memory, then, while the summary
is written in the background, (BUFFER TICK PROCESS ADOPTED): the
session buffer and its `buffer-chars-modified-tick' when the summary
was asked for, the request's process, if any, and non-nil once a
handover the next turn waits for takes it.  Another turn starting,
or the agent stopped, gives the handover up.")

(defvar-local lispm-session-agent nil
  "The agent whose session this buffer holds.")

(defface lispm-placeholder '((t :inherit shadow :slant italic))
  "Face of the hint shown where the next message is written."
  :group 'lispm)

(defvar-local lispm--placeholder nil
  "Overlay of the hint after an empty input heading, if shown.")

(defvar-local lispm--live nil
  "Markers (START . END) of the response being streamed in, or nil.")

(defvar-local lispm--tools nil
  "Schemas of the tools the session in this buffer offers the model.
See `lispm-session-tools'.")
(put 'lispm--tools 'permanent-local t)

(defvar-local lispm--live-kind nil
  "Kind of the content last streamed into the live region.")

(defvar org-state)

;;;; System prompt

(defun lispm--environment-prompt (agent)
  "The part of the system prompt describing AGENT's situation."
  (concat
   "# Your environment\n"
   "- Your root directory is " (abbreviate-file-name (lispm-agent-root agent))
   ". Relative paths are resolved against it.\n"
   (if (lispm-agent-system agent)
       (concat "- You work for the whole system, in no project: your memory and tasks are"
               " the system's, shared by no project's agents.\n")
     "")
   (if (file-remote-p (lispm-agent-root agent))
       (concat "- Your root is on another machine, which Emacs reaches through TRAMP"
               " (the prefix " (file-remote-p (lispm-agent-root agent)) "). Your shell commands"
               " run there, and absolute paths you give tools, such as /etc/hosts or ~/notes,"
               " name files there. Emacs Lisp still runs in the user's Emacs, on this machine.\n")
     "")
   "- This conversation is an org-mode buffer the user can read, fold, edit and"
   " annotate while you work. What it contains is what you remember of this session.\n"
   "- Calls that change files or run code may wait for the user's approval. Such"
   " calls take a `why` argument: one plain sentence the user reads when deciding."
   " If a call is denied, adapt; do not repeat it unchanged.\n"
   "- Files can be open in Emacs with unsaved changes. read_file and the edit tools"
   " work on the live buffer, so you see what the user sees; every edit you make is"
   " one undo step for them.\n"
   "- What this system and its apps can do is written in skills, their manuals, each"
   " with tools to do it; search_tools finds them, and the tools you are not offered"
   " yet. When a request needs something you do not have, search before you say you"
   " cannot do it.\n"
   "- Input beginning with \"[Message from agent X]\" comes from another agent, and"
   " \"[Event: ...]\" from a hook or timer the user set up. Treat them as requests"
   " from colleagues: anything consequential still needs the user's intent.\n"
   "- Input beginning with \"[From LispM, for the user]\" passes on a choice the user"
   " made, such as to go on with a response that was cut short: follow it as theirs.\n\n"
   "# Working style\n"
   "- Before your first tool call in a turn, say in one sentence what you are about"
   " to do. End the turn with a short summary: what you did, what you found, and"
   " anything the user needs to do.\n"
   "- Read before you edit; prefer small, verifiable steps.\n"
   "- Keep durable knowledge (user preferences, project conventions, lessons) in"
   " your long-term memory, and work that spans sessions in your task list."))

(defcustom lispm-project-instruction-files '("AGENTS.md" "CLAUDE.md")
  "Files in a project's root whose contents join its agents' system prompts.
The same files Codex and Claude Code read, so a project set up for them
needs nothing more."
  :type '(repeat string)
  :group 'lispm)

(defun lispm--project-instructions (agent)
  "System prompt section with the instruction files of AGENT's project."
  (let (sections seen)
    (dolist (name lispm-project-instruction-files)
      (let ((file (expand-file-name name (lispm-agent-root agent))))
        (when (file-readable-p file)
          (let ((text (string-trim (lispm--file-text file))))
            (unless (or (string-empty-p text) (member text seen))
              (push text seen)
              (push (format "# Project instructions (%s)\n%s" name (lispm--truncate text 20000))
                    sections))))))
    (and sections (string-join (nreverse sections) "\n\n"))))

(defun lispm--identity-prompt (agent)
  "The opening of AGENT's system prompt: who it is."
  (if-let* ((character (lispm-agent-character* agent)))
      (concat
       (format "You are \"%s\": not a session but someone, who lives inside a running GNU Emacs %s session, working alongside its human user. You share one programmable environment with them: files, buffers, processes and Emacs itself. You have conversations in many places, one going on in each, and you are the same %s in all of them, with one memory and one past. %s"
               (lispm-character-title* character) emacs-version (lispm-character-title* character)
               (if (lispm-character-home-p agent)
                   "This conversation is your own, in no project: people come to you here."
                 (format "This conversation is in the project at %s, where your work is: %s is your name here."
                         (abbreviate-file-name (lispm-agent-root agent)) (lispm-agent-name agent))))
       (when-let* ((soul (lispm-character-soul character))
                   ((not (string-blank-p soul))))
         (concat "\n\n# Who you are\n" (string-trim soul))))
    (format "You are \"%s\", an AI agent (role: %s) living inside a running GNU Emacs %s session, working alongside its human user. You share one programmable environment with them: files, buffers, processes and Emacs itself."
            (lispm-agent-name agent) (lispm-agent-role agent) emacs-version)))

(defun lispm-system-prompt (agent)
  "System prompt of a new session of AGENT."
  (string-join
   (seq-remove
    (lambda (s) (or (null s) (string-blank-p s)))
    (append
     (list (lispm--identity-prompt agent)
           (lispm-agent-instructions agent)
           (lispm--environment-prompt agent)
           (lispm--project-instructions agent))
     (mapcar (lambda (f) (funcall f agent)) lispm-system-prompt-functions)))
   "\n\n"))

;;;; Sessions

(defvar-keymap lispm-session-mode-map
  :doc "Keymap of `lispm-session-mode'."
  "C-c RET" #'lispm-send
  "C-c C-k" #'lispm-abort
  "C-c C-y" #'lispm-review)

(defvar lispm--visiting nil
  "The agent whose session file is being visited, while it is.")

(defun lispm--file-keywords (file)
  "The #+lispm_ keywords of session FILE, read without visiting it."
  (with-temp-buffer
    (insert-file-contents file nil 0 8192)
    (lispm-transcript-keywords)))

(defun lispm--session-agent ()
  "The agent of the session in the current buffer, resumed if need be.
A live agent with the session's id is it; a standing agent named in
the session, without a session of its own open, adopts it; otherwise
the session's agent is started again from its role."
  (let* ((keywords (lispm-transcript-keywords))
         (id (cdr (assoc "id" keywords)))
         (name (cdr (assoc "agent" keywords)))
         (standing (and name (lispm-get-agent (intern name) t))))
    (when name
      (cond
       ((and id (seq-find (lambda (a) (equal (lispm-agent-id a) id)) (lispm-agents))))
       ((and standing (lispm-agent-standing standing)
             (not (lispm-agent-character standing))
             (not (buffer-live-p (lispm-agent-buffer standing))))
        (when id (setf (lispm-agent-id standing) id))
        standing)
       ;; A character's conversation goes on in the agent it has there.
       ((when-let* ((character (lispm-get-character (or (cdr (assoc "character" keywords)) "") t))
                    (root (cdr (assoc "root" keywords)))
                    (agent (lispm-character-agent character root)))
          (if (buffer-live-p (lispm-agent-buffer agent))
              ;; An earlier conversation of it there, while it has another open.
              (lispm-make-agent (lispm-agent-role agent) root
                                :name (lispm--free-name name) :id id
                                :character (lispm-character-name character))
            (when id (setf (lispm-agent-id agent) id))
            agent)))
       ((lispm-get-role (intern (or (cdr (assoc "role" keywords)) "")) t)
        (let ((agent (lispm-make-agent (intern (cdr (assoc "role" keywords)))
                                       (or (cdr (assoc "root" keywords)) default-directory)
                                       :name (lispm--free-name name) :id id
                                       :system (equal (cdr (assoc "system" keywords)) "t"))))
          ;; A helper stays bounded by its caller.
          (when-let* ((ceiling (assoc "ceiling" keywords)))
            (setf (lispm-agent-ceiling agent) (lispm--read-ceiling (cdr ceiling))))
          (setf (lispm-agent-parent agent) (cdr (assoc "parent" keywords)))
          agent))
       (t (message "%s" (lispm-tr "LispM: no role %s to resume this session with"
                                  (cdr (assoc "role" keywords))))
          nil)))))

(defun lispm--read-ceiling (text)
  "The ceiling a session's #+lispm_ceiling TEXT records: a policy.
TEXT is an alist of permission classes and actions, as printed.  One
that cannot be read stands for asking for everything, rather than for
no ceiling at all."
  (let ((ceiling (ignore-errors (car (read-from-string text)))))
    (if (and (consp ceiling)
             (seq-every-p (lambda (entry)
                            (and (consp entry) (symbolp (car entry))
                                 (memq (cdr entry) lispm--policy-actions)))
                          ceiling))
        ceiling
      (mapcar (lambda (entry) (cons (car entry) 'ask)) lispm-default-policy))))

(defun lispm--set-keyword (name value)
  "Set the #+lispm_NAME keyword of the session in the current buffer to VALUE."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((line (format "#+lispm_%s: %s" name value)))
        (if (re-search-forward (format "^#\\+lispm_%s:.*$" (regexp-quote name)) 4000 t)
            (replace-match line t t)
          (goto-char (point-min))
          (while (looking-at "#\\+") (forward-line 1))
          (insert line "\n"))))))

(defun lispm-set-ceiling (agent ceiling)
  "Bound what AGENT may do by CEILING, a policy, for the rest of its life.
AGENT's session records it (#+lispm_ceiling:), so that the session
opened again, after Emacs stopped or by hand, is bounded alike."
  (setf (lispm-agent-ceiling agent) ceiling)
  (when (buffer-live-p (lispm-agent-buffer agent))
    (with-current-buffer (lispm-agent-buffer agent)
      (let ((print-length nil) (print-level nil))
        (lispm--set-keyword "ceiling" (prin1-to-string ceiling))))
    (lispm--save-session agent)))

(defun lispm-set-parent (agent parent)
  "Record that AGENT was started by the agent whose session id is PARENT.
AGENT's session records it (#+lispm_parent:), so that lists of the
sessions show AGENT's under PARENT's, see `lispm-sessions'."
  (setf (lispm-agent-parent agent) parent)
  (when (buffer-live-p (lispm-agent-buffer agent))
    (with-current-buffer (lispm-agent-buffer agent)
      (lispm--set-keyword "parent" parent))
    (lispm--save-session agent)))

(define-minor-mode lispm-session-mode
  "Minor mode of agent session transcripts.
Write under the last \"* User\" heading and press \\[lispm-send].
Tool calls awaiting approval are WAITING headings: review them with
\\[lispm-review], or approve and deny by changing their TODO state.

\\{lispm-session-mode-map}"
  :lighter " λ"
  (if lispm-session-mode
      (progn
        (unless lispm-session-agent
          (setq lispm-session-agent (or lispm--visiting (lispm--session-agent))))
        (add-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed nil t)
        (add-hook 'kill-buffer-hook #'lispm--session-killed nil t)
        (add-hook 'after-change-functions #'lispm--update-placeholder nil t)
        (lispm--update-placeholder)
        (setq-local header-line-format '(:eval (lispm--header-line)))
        (let ((agent lispm-session-agent))
          (when agent
            ;; The session lives in the project: commands run from it, and
            ;; tools grouping buffers by directory, see the project.
            (setq default-directory (lispm-agent-root agent)))
          (when (and agent (not (buffer-live-p (lispm-agent-buffer agent))))
            (setf (lispm-agent-buffer agent) (current-buffer)))
          (when (and agent (eq (lispm-agent-buffer agent) (current-buffer)))
            (lispm--resume-calls agent))))
    (remove-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed t)
    (remove-hook 'kill-buffer-hook #'lispm--session-killed t)
    (remove-hook 'after-change-functions #'lispm--update-placeholder t)
    (when (overlayp lispm--placeholder) (delete-overlay lispm--placeholder))
    (kill-local-variable 'header-line-format)))

(defun lispm--update-placeholder (&rest _)
  "Show a hint after the input heading while nothing is written under it."
  ;; A change function must keep the match data: `replace-match', in
  ;; `org-todo' for one, reads it again after the change to fix the
  ;; replacement's case, and would upcase from wherever we matched.
  (save-match-data
    (lispm--update-placeholder-1)))

(defun lispm--update-placeholder-1 ()
  "Show or hide the input placeholder; see `lispm--update-placeholder'."
  (let ((input (and lispm-session-mode (lispm--input-heading-p))))
    (if (not input)
        (when (overlayp lispm--placeholder)
          (delete-overlay lispm--placeholder)
          (setq lispm--placeholder nil))
      (let ((end (save-excursion (goto-char input) (line-end-position))))
        (unless (overlayp lispm--placeholder)
          (setq lispm--placeholder (make-overlay end end nil t nil))
          (overlay-put lispm--placeholder 'after-string
                       (propertize (concat "   " (lispm-tr "type your message below, then C-c RET to send"))
                                   'face 'lispm-placeholder)))
        (move-overlay lispm--placeholder end end)))))

(defun lispm-session-maybe-enable ()
  "Enable `lispm-session-mode' in org buffers that are agent sessions."
  (when (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (re-search-forward "^#\\+lispm_agent:" 4000 t)))
    (lispm-session-mode 1)))

(defun lispm--header-line ()
  "Header line of a session buffer."
  (let ((agent lispm-session-agent))
    (if (not agent)
        (concat " λ " (lispm-tr "no agent: is its role defined?"))
      (let ((state (lispm-agent-state agent)))
        (concat " λ " (symbol-name (lispm-agent-name agent))
                (propertize (format " (%s)" (lispm-agent-role agent)) 'face 'shadow)
                "  "
                (propertize (lispm--state-name state)
                            'face (if (eq state 'waiting) 'warning 'success))
                (if (lispm-agent-status agent)
                    (concat ": " (lispm-agent-status agent))
                  "")
                "    "
                (propertize (cond ((eq state 'idle) (lispm-tr "C-c RET send"))
                                  ((lispm-agent-condition agent)
                                   (lispm-tr "C-c C-y choose · a new * User and C-c RET stops here and sends"))
                                  ((eq state 'waiting) (lispm-tr "C-c C-y review · C-c C-k abort"))
                                  (t (lispm-tr "C-c C-k abort")))
                            'face 'shadow))))))

(defun lispm-image-directory (agent)
  "Directory of the images of AGENT's session, next to its session files.
Session files are under git (see `lispm-git'); images are kept out."
  (let ((dir (expand-file-name (concat (lispm-agent-id agent) ".images/")
                               (lispm--sessions-directory (lispm-agent-root agent)))))
    (make-directory dir t)
    dir))

(defun lispm-save-image (agent data &optional type)
  "Save the image DATA (base64) in AGENT's session; return the file.
TYPE is its media type, image/png by default.  Without an agent, the
image goes to a temporary file."
  (let* ((extension (or (car (rassoc (or type "image/png") lispm-transcript--image-types)) "png"))
         (file (if agent
                   (let ((dir (lispm-image-directory agent)))
                     (expand-file-name (format "%04d.%s" (1+ (length (directory-files
                                                                      dir nil "\\`[0-9]+\\.")))
                                               extension)
                                       dir))
                 (make-temp-file "lispm-image-" nil (concat "." extension))))
         (coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string data)))
    file))

(defun lispm--sessions-directory (root)
  "Directory of the session files of the project at ROOT."
  (let ((dir (expand-file-name "sessions/" (lispm-project-directory root))))
    (make-directory dir t)
    dir))

(defun lispm--create-session-file (agent &optional previous summary kept)
  "Write a new session file for AGENT and return its name.
PREVIOUS is the file this one continues, SUMMARY what it hands over,
and KEPT the text of the latest turns there, which follow SUMMARY;
the continuation keeps the title the conversation is listed under."
  (let* ((dir (lispm--sessions-directory (lispm-agent-root agent)))
         (id (lispm-agent-id agent))
         (file (expand-file-name (concat id ".org") dir))
         (n 1)
         (coding-system-for-write 'utf-8))
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d.org" id (cl-incf n)) dir)))
    (write-region
     (concat (lispm-transcript-header
              (format "%s %s" (lispm-agent-name agent) (format-time-string "%Y-%m-%d %H:%M"))
              `(("agent" . ,(lispm-agent-name agent))
                ("id" . ,id)
                ("role" . ,(lispm-agent-role agent))
                ("root" . ,(abbreviate-file-name (lispm-agent-root agent)))
                ,@(and previous `(("previous" . ,(format "[[file:%s]]" previous))))
                ,@(and (lispm-agent-system agent) '(("system" . "t")))
                ,@(when-let* ((character (lispm-agent-character agent)))
                    `(("character" . ,character)))
                ,@(when-let* ((parent (lispm-agent-parent agent)))
                    `(("parent" . ,parent)))
                ,@(when-let* ((ceiling (lispm-agent-ceiling agent)))
                    `(("ceiling" . ,(let ((print-length nil) (print-level nil))
                                      (prin1-to-string ceiling)))))
                ,@(when-let* ((title (and previous
                                          (plist-get (lispm--session-description previous) :title))))
                    `(("title" . ,(lispm--oneline title 200))))))
             (lispm-transcript-section (lispm-transcript-title 'system) (lispm-system-prompt agent))
             (if summary (lispm-transcript-section (lispm-transcript-title 'summary) summary) "")
             (or kept "")
             (lispm-transcript-heading 'user))
     nil file nil 'silent)
    file))

(defun lispm--visit-session (agent file)
  "Make FILE AGENT's session buffer; return the buffer."
  (let ((buffer (let ((lispm--visiting agent)) (find-file-noselect file))))
    (with-current-buffer buffer
      (setq lispm-session-agent agent)
      (lispm-session-mode 1)
      (goto-char (point-min))
      (when (re-search-forward (lispm-transcript-heading-regexp 'system) nil t)
        (lispm-transcript-fold (match-beginning 0)))
      (goto-char (point-max)))
    (setf (lispm-agent-buffer agent) buffer)
    buffer))

(defun lispm--session-files-in (pattern)
  "Session files matching the wildcard PATTERN under `lispm-directory'.
Emacs's lock files (.#NAME) and other dot files are left out."
  (seq-filter (lambda (file)
                (and (not (string-prefix-p "." (file-name-nondirectory file)))
                     (file-regular-p file)))
              (file-expand-wildcards (expand-file-name pattern lispm-directory))))

(defun lispm--latest-session-file (agent)
  "The newest file of the sessions standing AGENT had in its project.
A character's are those of its conversation in AGENT's root, whatever
the agent was called then."
  (let ((name (symbol-name (lispm-agent-name agent)))
        (character (lispm-agent-character agent))
        (root (lispm-agent-root agent)))
    (car (sort (seq-filter
                (lambda (file)
                  (let ((keywords (lispm--file-keywords file)))
                    (if character
                        (and (equal (cdr (assoc "character" keywords)) (symbol-name character))
                             (equal (file-name-as-directory
                                     (expand-file-name (or (cdr (assoc "root" keywords)) "~")))
                                    root))
                      (equal (cdr (assoc "agent" keywords)) name))))
                (lispm--session-files-in
                 (expand-file-name "*.org" (lispm--sessions-directory (lispm-agent-root agent)))))
               (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                          (file-attribute-modification-time (file-attributes a))))))))

(defun lispm-session-buffer (agent &optional new)
  "AGENT's session buffer.
A standing agent resumes its latest session.  With NEW, or when there
is none, a fresh session starts."
  (let ((buffer (lispm-agent-buffer agent)))
    (if (and (buffer-live-p buffer) (not new))
        buffer
      (when (lispm-agent-busy-p agent)
        (user-error "%s" (lispm-tr "%s is busy" (lispm-agent-name agent))))
      (let ((file (and (not new) (lispm-agent-standing agent)
                       (lispm--latest-session-file agent))))
        ;; A standing agent goes on with its session: its id is the session's.
        (when-let* ((id (and file (cdr (assoc "id" (lispm--file-keywords file))))))
          (setf (lispm-agent-id agent) id))
        (unless file
          (when new
            ;; What waits for the agent waits for it in the new session.
            (lispm-inbox-forget agent)
            (setf (lispm-agent-id agent) (lispm--new-id)))
          (setq file (lispm--create-session-file agent)))
        (prog1 (lispm--visit-session agent file)
          (when new (lispm-inbox-save agent)))))))

(defun lispm--project-root (&optional directory)
  "Root of the project around DIRECTORY (default: `default-directory')."
  (let ((default-directory (or directory default-directory)))
    (if-let* ((project (project-current)))
        (project-root project)
      default-directory)))

(defun lispm-start (&optional role root)
  "Start a new agent of ROLE working in ROOT; return it.
ROLE defaults to `lispm-default-role', ROOT to the current project."
  (let ((agent (lispm-make-agent (or role lispm-default-role)
                                 (or root (lispm--project-root)))))
    (lispm-session-buffer agent)
    agent))

(defun lispm-resume-file (file)
  "Resume the session recorded in FILE; return its agent."
  (let ((buffer (find-file-noselect file)))
    (or (buffer-local-value 'lispm-session-agent buffer)
        (user-error "%s" (lispm-tr "%s is not a session LispM can resume" file)))))

(defun lispm-resume-session (id)
  "The agent of the session ID: a live one, or the session resumed.
Resuming opens its newest file.  Nil if there is no such session, or
it cannot be resumed (its role is not defined, say)."
  (or (seq-find (lambda (agent) (equal (lispm-agent-id agent) id)) (lispm-agents))
      (when-let* ((files (lispm-session-files id))
                  (newest (car (sort files (lambda (a b)
                                             (time-less-p
                                              (file-attribute-modification-time (file-attributes b))
                                              (file-attribute-modification-time (file-attributes a))))))))
        (ignore-errors (lispm-resume-file newest)))))

(defun lispm-retire (agent)
  "End AGENT: save its session and close its buffer.
A standing agent stays declared and resumes its session when needed."
  (let ((buffer (lispm-agent-buffer agent)))
    (lispm--save-session agent)
    (if (buffer-live-p buffer)
        (kill-buffer buffer)
      (run-hook-with-args 'lispm-session-end-functions agent)
      (unless (lispm-agent-standing agent)
        (lispm-forget-agent agent)))))

(defun lispm--session-killed ()
  "Stop the agent of a session buffer being killed, and let it go."
  (when-let* ((agent lispm-session-agent))
    (when (eq (lispm-agent-buffer agent) (current-buffer))
      (when (lispm-agent-busy-p agent)
        (lispm--stop agent "Stopped: the session buffer was closed."))
      (setf (lispm-agent-buffer agent) nil)
      (run-hook-with-args 'lispm-session-end-functions agent)
      (unless (lispm-agent-standing agent)
        (lispm-unwatch agent)
        (lispm-forget-agent agent)))))

(defun lispm--take-up (agent)
  "Take up what the standing AGENT was doing when Emacs last stopped, if anything.
If the project's inbox file holds inputs for it, its session is
opened: what was queued is handled, and a turn under way when Emacs
stopped is taken up (see `lispm--resume-calls').  Run once AGENT is
declared, as Emacs starts."
  (run-at-time 0 nil
               (lambda ()
                 (when (and (memq agent (lispm-agents))
                            (not (buffer-live-p (lispm-agent-buffer agent)))
                            (lispm-inbox-pending agent))
                   (lispm-session-buffer agent)))))

(add-hook 'lispm-agent-declared-functions #'lispm--take-up)

(defun lispm--save-sessions ()
  "Save the session of every live agent, as Emacs exits.
A response streaming in is left out: the session on disk holds what
its request was made from, for the turn to be taken up again."
  (dolist (agent (lispm-agents))
    (with-demoted-errors "LispM: saving a session: %S"
      (when (buffer-live-p (lispm-agent-buffer agent))
        (lispm--live-delete agent)
        (lispm--save-session agent)))))

(add-hook 'kill-emacs-hook #'lispm--save-sessions)

;;;; Recorded sessions

(defvar lispm--session-cache (make-hash-table :test #'equal)
  "Session descriptions keyed by file, valued (MTIME . DESCRIPTION).")

(defun lispm--first-prompt ()
  "First line of the first input in the current session text, or nil."
  (when-let* ((input (seq-find (lambda (section) (memq (plist-get section :kind) '(user message event)))
                               (lispm-transcript-sections)))
              (text (lispm-transcript-section-text input))
              ((not (string-empty-p text))))
    (car (split-string text "\n"))))

(defun lispm--session-description (file)
  "Describe the session recorded in FILE, or nil if it holds none.
Descriptions are remembered until FILE changes."
  (let ((time (float-time (file-attribute-modification-time (file-attributes file))))
        (cached (gethash file lispm--session-cache)))
    (if (and cached (= (car cached) time))
        (cdr cached)
      (let ((description
             (with-temp-buffer
               (insert-file-contents file nil 0 65536)
               (let* ((keywords (lispm-transcript-keywords))
                      (title (or (cdr (assoc "title" keywords)) (lispm--first-prompt))))
                 (when (and title (assoc "id" keywords))
                   (list :id (cdr (assoc "id" keywords))
                         :agent (cdr (assoc "agent" keywords))
                         :character (cdr (assoc "character" keywords))
                         :role (cdr (assoc "role" keywords))
                         :root (expand-file-name (or (cdr (assoc "root" keywords)) "~"))
                         :title title
                         :time time
                         :file file
                         :parent (cdr (assoc "parent" keywords))))))))
        (puthash file (cons time description) lispm--session-cache)
        description))))

(defun lispm-sessions ()
  "Every recorded session with some input, newest first.
Each is a plist (:id :agent :character :role :root :title :time :file
:parent), :character nil but in a character's, :parent the id of the
session of the agent that started this one, see `lispm-set-parent',
else nil; a session continued in several files is listed once, by its
newest file."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (file (lispm--session-files-in "projects/*/sessions/*.org"))
      (when-let* ((session (lispm--session-description file)))
        (let ((seen (gethash (plist-get session :id) latest)))
          (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
            (puthash (plist-get session :id) session latest)))))
    (sort (hash-table-values latest)
          (lambda (a b) (> (plist-get a :time) (plist-get b :time))))))

(defun lispm-session-files (id)
  "Every file recording the session ID."
  (seq-filter (lambda (file) (equal (cdr (assoc "id" (lispm--file-keywords file))) id))
              (lispm--session-files-in (format "projects/*/sessions/%s*.org" id))))

(defun lispm-rename-session (file title)
  "Give the session recorded in FILE the TITLE shown in session lists."
  (let* ((visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file))))
    (with-current-buffer buffer
      (lispm--set-keyword "title" title)
      (let ((inhibit-message t) (lispm-inhibit-triggers t)) (save-buffer)))
    (unless visiting (kill-buffer buffer))))

(defmacro lispm--in-session (agent &rest body)
  "Run BODY in AGENT's session buffer, at its end.
Windows that showed the end of the buffer keep following it."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer"))
        (followers (make-symbol "followers"))
        (at-end (make-symbol "at-end")))
    `(let ((,buffer (lispm-agent-buffer ,agent)))
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           (let* ((inhibit-read-only t)
                  (,followers (seq-filter (lambda (w) (>= (window-point w) (point-max)))
                                          (get-buffer-window-list ,buffer nil t)))
                  (,at-end (>= (point) (point-max))))
             (prog1 (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-max))
                        ,@body))
               (when ,at-end (goto-char (point-max)))
               (dolist (w ,followers) (set-window-point w (point-max))))))))))

(defun lispm--newline ()
  "Make sure point is at the beginning of a line."
  (unless (bolp) (insert "\n")))

(defun lispm--input-heading-p ()
  "Non-nil if the last section of the buffer is an empty \"* User\"."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^\\* " nil t)
         (looking-at (concat (lispm-transcript-heading-regexp 'user) "[ \t\n]*\\'"))
         (point))))

(defun lispm--save-session (agent)
  "Save AGENT's session file."
  (let ((buffer (lispm-agent-buffer agent)))
    (when (and lispm-save-sessions (buffer-live-p buffer) (buffer-file-name buffer))
      (with-current-buffer buffer
        (let ((inhibit-message t)
              (lispm-inhibit-triggers t))
          (save-buffer))))))

(defun lispm--add-section (agent title text)
  "Add a section titled TITLE, holding TEXT, to AGENT's session.
It goes before the input heading a turn ended with, if any."
  (lispm--in-session agent
    (let ((input (lispm--input-heading-p)))
      (if input
          (goto-char input)
        (lispm--newline))
      (insert (lispm-transcript-section title text)))))

(defun lispm--record-event (agent title text)
  "Note TEXT as an event in AGENT's session without starting a turn.
The event's heading is \"Event: TITLE\".  The model reads it with
the next input."
  (lispm--add-section agent (lispm-transcript-title 'event title) text))

;;;; Turns

(defun lispm-reply-value (reply)
  "The value a turn returned with REPLY, as a turn callback gets it; or nil.
An agent returns one with return_value: data, decoded from JSON."
  (and (stringp reply) (> (length reply) 0) (get-text-property 0 'lispm-value reply)))

(cl-defun lispm-tell (agent text &key from event callback reply-to)
  "Give AGENT the input TEXT; it starts working as soon as it is idle.
FROM names the agent sending it; EVENT describes the hook or timer
that produced it (pending events with the same description are
coalesced).  CALLBACK, if given, is called with the agent's final
reply when the turn handling TEXT ends, and a value the turn returned
with it, if any (see `lispm-reply-value').  REPLY-TO, a string, says who
waits for that reply in a way that outlives Emacs: the input is kept
in the project's inbox file until its turn ends, and if Emacs stops
before, `lispm-inbox-callback-function' makes a callback of it again."
  (let* ((agent (lispm-get-agent agent))
         (same nil))
    ;; Opening the session first takes back what was queued for it.
    (lispm-session-buffer agent)
    (setq same (and event (seq-find (lambda (e) (equal (plist-get e :event) event))
                                    (lispm-agent-inbox agent))))
    (if same
        (plist-put same :text text)
      (setf (lispm-agent-inbox agent)
            (append (lispm-agent-inbox agent)
                    (list (list :id (lispm-inbox-new-id) :text text :from from :event event
                                :callback callback :reply-to reply-to
                                :queued (format-time-string "[%Y-%m-%d %a %H:%M]"))))))
    (lispm-inbox-save agent)
    (run-at-time 0 nil #'lispm--drain-inbox agent)
    agent))

(defun lispm--drain-inbox (agent)
  "Start a turn on AGENT's next inbox item if it is idle.
Not once AGENT has ended: input left for it then is dropped."
  (when (and (memq agent (lispm-agents))
             (not (lispm-agent-busy-p agent))
             (not (lispm-agent-compacting agent))
             (lispm-agent-inbox agent))
    ;; Opening a standing agent's session can take up calls it left
    ;; awaiting approval; the inbox waits until they are settled.
    (lispm-session-buffer agent)
    (unless (lispm-agent-busy-p agent)
      (lispm--begin-turn agent (pop (lispm-agent-inbox agent))))))

(defun lispm--begin-turn (agent &optional entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed.
A session grown past `lispm-compact-threshold' first makes room by
clearing the output of older calls; if that is not enough, it is
handed over to a continuation, and the turn starts there."
  (if (and lispm-compact-threshold
           (not (plist-get entry :no-compact))
           (> (lispm--estimated-tokens agent) lispm-compact-threshold)
           (not (and (lispm--clear-results agent)
                     (<= (lispm--estimated-tokens agent) lispm-compact-threshold)))
           (lispm--has-history-p agent))
      (progn
        (push (or entry (list :id (lispm-inbox-new-id) :text (lispm--take-input agent)))
              (lispm-agent-inbox agent))
        (lispm-inbox-save agent)
        (lispm-compact agent))
    (lispm--start-turn agent entry)))

(defun lispm--start-turn (agent entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed.
The input is recorded as the turn's (see `lispm-agent-entry'), in the
project's inbox file too, until the turn ends, and the session is
saved: if Emacs stops in the middle of the turn, reopening the session
takes it up (see `lispm--resume-calls').  A handover at idle under way
is given up: the session goes on."
  (when (and (gethash agent lispm--idle-handovers) (not (plist-get entry :idle)))
    (lispm--give-up-idle-handover agent))
  (cl-incf (lispm-agent-turn agent))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-last-reply agent) nil
        (lispm-agent-value agent) nil
        (lispm-agent-auto-restarts agent) nil
        (lispm-agent-max-tokens agent) nil
        ;; Another agent's message or an event: nobody may be watching.
        (lispm-agent-attended agent) (not (or (plist-get entry :from) (plist-get entry :event)))
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback)))
        (lispm-agent-entry agent) (if (plist-get entry :id)
                                      entry
                                    (plist-put (copy-sequence entry) :id (lispm-inbox-new-id))))
  (when entry
    (lispm--in-session agent
      (let ((input (lispm--input-heading-p)))
        (when input (delete-region input (point-max))))
      (lispm--newline)
      (insert (lispm-transcript-section (lispm-transcript-input-title entry) (plist-get entry :text)))))
  (lispm-inbox-save agent)
  (with-demoted-errors "LispM: taking a snapshot: %S"
    (lispm-journal-begin agent))
  (lispm--request agent))

(defun lispm--journal-wait (agent)
  "What AGENT's request waits for, if a snapshot of its project is being taken."
  (and (lispm-journal-busy-p agent) "taking a snapshot of the project"))

(add-hook 'lispm-request-wait-functions #'lispm--journal-wait)

(defun lispm-session-tools (agent)
  "Schemas of the tools AGENT's session offers the model.
Like the system prompt, they are frozen: taken when the session first
needs them and kept while it is open, so a tool defined or redefined
later, or a role redefined with other tools, does not change the
request prefix, which the prompt cache and the model's thinking are
bound to.  A new session, or a handover, takes the tools as they are
then.  Tools whose descriptions depend on the agent see it as
`lispm-current-agent'."
  (with-current-buffer (lispm-agent-buffer agent)
    (or lispm--tools
        (setq lispm--tools (let ((lispm-current-agent agent))
                             (mapcar #'lispm-tool-schema (lispm-agent-tools* agent)))))))

(defun lispm--extend-session-tools (agent)
  "Offer AGENT's session the tools it gained, after those it had.
Run from `lispm-tools-changed-functions' when an agent's tools grow in
the middle of a session, as when it finds deferred tools, loads a
skill or defines a tool itself.  The earlier tools keep their place;
the request prefix changes once."
  (let ((buffer (lispm-agent-buffer agent)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when lispm--tools
          (let ((names (mapcar (lambda (schema) (alist-get 'name schema)) lispm--tools)))
            (dolist (tool (lispm-agent-tools* agent))
              (unless (member (lispm-tool-name tool) names)
                (setq lispm--tools
                      (append lispm--tools
                              (list (let ((lispm-current-agent agent))
                                      (lispm-tool-schema tool)))))))))))))

(add-hook 'lispm-tools-changed-functions #'lispm--extend-session-tools)

(defun lispm--offers-tool-p (agent name)
  "Non-nil if AGENT's session offers the model the tool NAME."
  (seq-some (lambda (schema) (equal (alist-get 'name schema) name))
            (lispm-session-tools agent)))

(defun lispm-request-body (agent)
  "Request for AGENT's next model call, as (BODY . BETAS).
Signal a `user-error' if the session cannot be sent as it is."
  (let ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse))))
    (when (plist-get parsed :pending)
      (user-error "Tool calls still await a result: %s"
                  (string-join (plist-get parsed :pending) ", ")))
    (unless (or (equal (plist-get parsed :last-role) "user")
                ;; A paused response goes back as it is, for the model to resume.
                (and (lispm-agent-continuing agent)
                     (equal (plist-get parsed :last-role) "assistant")))
      (user-error "Nothing to send: write under a \"* User\" heading"))
    (setf (lispm-agent-continuing agent) nil)
    (lispm-api-request (let ((backend (lispm-agent-backend* agent)))
                         (if-let* ((limit (lispm-agent-max-tokens agent)))
                             (plist-put (copy-sequence backend) :max-tokens limit)
                           backend))
                       (lispm-agent-model* agent)
                       (lispm-agent-effort* agent)
                       (plist-get parsed :system)
                       (lispm-session-tools agent)
                       (plist-get parsed :messages))))

(defun lispm--max-tokens (agent)
  "The output limit of AGENT's requests now."
  (or (lispm-agent-max-tokens agent)
      (plist-get (lispm-agent-backend* agent) :max-tokens)
      lispm-max-tokens))

(defvar lispm-request-wait-functions nil
  "Functions that can hold back an agent's request for a moment.
Each is called with the agent before a request is made, and returns
a short description of what the request waits for, or nil.  While
one does, the request is tried again shortly; MCP servers still
starting hold requests back so that their tools are there.")

(defun lispm--request (agent)
  "Ask the model for AGENT's next step."
  (cond
   ((not (buffer-live-p (lispm-agent-buffer agent)))
    (lispm-set-state agent 'idle "session buffer was killed"))
   ((when-let* ((waiting (run-hook-with-args-until-success 'lispm-request-wait-functions agent)))
      (lispm-set-state agent 'requesting waiting 'waiting)
      (let ((turn (lispm-agent-turn agent))
            (steps (lispm-agent-steps agent)))
        (run-at-time 0.2 nil (lambda ()
                               ;; Unless the turn was stopped meanwhile.
                               (when (and (eq (lispm-agent-state agent) 'requesting)
                                          (eql turn (lispm-agent-turn agent))
                                          (eql steps (lispm-agent-steps agent)))
                                 (lispm--request agent)))))
      t))
   ((>= (lispm-agent-steps agent) lispm-max-steps)
    (lispm-signal agent 'step-limit
                  (format "Stopped after %d model requests (`lispm-max-steps')." lispm-max-steps)))
   (t
    ;; A turn can outgrow the threshold in the middle: before handing
    ;; the session over, which waits for the turn's end, the output of
    ;; older calls makes room.
    (when (and lispm-compact-threshold
               (> (lispm--estimated-tokens agent) lispm-compact-threshold))
      (lispm--clear-results agent))
    (pcase (condition-case err
               (lispm-request-body agent)
             (error (error-message-string err)))
      ((and (pred stringp) problem) (lispm-signal agent 'request-failed problem))
      (`(,body . ,betas)
       (cl-incf (lispm-agent-steps agent))
       (setf (lispm-agent-request-bytes agent) (lispm--session-bytes agent))
       ;; What the request carries is on disk, should Emacs stop now.
       (lispm--save-session agent)
       (lispm--send agent body betas 0))))))

(defun lispm--send (agent body betas attempt)
  "Send the request BODY with BETAS for AGENT; ATTEMPT counts retries."
  (lispm-set-state agent 'requesting
                   (if (> attempt 0) (format "retry %d" attempt) "thinking")
                   (if (> attempt 0) 'retry 'thinking))
  (lispm--live-begin agent)
  (let ((proc (funcall lispm-api-function (lispm-agent-backend* agent) body betas
                       :on-delta (lambda (kind text)
                                   (if (eq kind 'tool-input)
                                       (with-demoted-errors "LispM: %S"
                                         (run-hook-with-args 'lispm-tool-input-functions
                                                             agent text))
                                     (lispm--live-insert agent kind text)))
                       :on-done (lambda (message) (lispm--on-message agent message))
                       :on-error (lambda (err) (lispm--on-error agent err body betas attempt)))))
    (when (processp proc)
      (process-put proc 'lispm-agent agent))
    (setf (lispm-agent-process agent) proc)))

(defmacro lispm--unsaved (&rest body)
  "Run BODY, which changes the current buffer, leaving it as modified as it was.
A response streaming into a session is shown, never saved: until it
is complete, the file holds what the request was made from, which is
what a session opened again after Emacs stopped takes up."
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
           (create-lockfiles nil))
       (prog1 (progn ,@body)
         (restore-buffer-modified-p ,modified)))))

(defun lispm--live-begin (agent)
  "Open the region where AGENT's response streams in."
  (lispm--in-session agent
    (lispm--unsaved
      (lispm--newline)
      (let ((start (point-marker)))
        (insert (lispm-transcript-heading 'assistant))
        (setq lispm--live (cons start (copy-marker (point) t))
              lispm--live-kind nil)))))

(defun lispm--live-insert (agent kind text)
  "Show streamed content TEXT of KIND (text, thinking or tool) for AGENT."
  (lispm--in-session agent
    (when lispm--live
      (lispm--unsaved
        (goto-char (cdr lispm--live))
        (unless (eq kind lispm--live-kind)
          (lispm--newline)
          (setq lispm--live-kind kind))
        ;; Each piece says what it is, see `lispm-live-segments'.
        (pcase kind
          ('text (insert (propertize text 'lispm-live 'text)))
          ('thinking (insert (propertize text 'face 'shadow 'lispm-live 'thinking)))
          ('tool (insert (propertize (format "-> %s ...\n" text) 'face 'shadow
                                     'lispm-live (cons 'tool text))))))))
  (when (eq kind 'tool)
    (lispm-set-state agent 'requesting (format "writing a %s call" text)
                     (cons 'writing text)))
  (with-demoted-errors "LispM: %S"
    (run-hook-with-args 'lispm-response-delta-functions agent kind text)))

(defun lispm-live-region (buffer)
  "Where the response streaming into the session BUFFER is, or nil:
\(START . END), START where its heading starts."
  (let ((live (buffer-local-value 'lispm--live buffer)))
    (and live (cons (marker-position (car live))
                    (max (marker-position (car live)) (marker-position (cdr live)))))))

(defun lispm-live-segments (buffer)
  "What has streamed into the session BUFFER's response so far, in order.
Each is a plist: (:kind text :text TEXT), (:kind thinking :text TEXT) or
\(:kind tool :name NAME), a call being written."
  (when-let* ((region (lispm-live-region buffer)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let ((pos (progn (goto-char (car region)) (forward-line 1) (min (point) (cdr region))))
                (end (cdr region))
                segments)
            (while (< pos end)
              (let ((next (next-single-property-change pos 'lispm-live nil end))
                    (kind (get-text-property pos 'lispm-live)))
                (pcase kind
                  (`(tool . ,name) (push (list :kind 'tool :name name) segments))
                  ((pred symbolp)
                   (let ((text (string-trim (buffer-substring-no-properties pos next))))
                     (when (and kind (not (string-empty-p text)))
                       (push (list :kind kind :text text) segments)))))
                (setq pos next)))
            (nreverse segments)))))))

(defun lispm--live-delete (agent)
  "Remove AGENT's streamed region; the final message replaces it."
  (lispm--in-session agent
    (when lispm--live
      (lispm--unsaved
        (delete-region (car lispm--live) (max (car lispm--live) (cdr lispm--live))))
      (set-marker (car lispm--live) nil)
      (set-marker (cdr lispm--live) nil)
      (setq lispm--live nil))))

(defun lispm--add-usage (agent usage)
  "Add the token USAGE of a response to AGENT's totals."
  (let ((totals (lispm-agent-usage agent)))
    (cl-loop for key in '(input_tokens output_tokens cache_read_input_tokens)
             for i from 0
             for n = (alist-get key usage)
             when (numberp n) do (cl-incf (nth i totals) n))))

(defun lispm--note-context (agent usage)
  "Record the size of AGENT's session that the USAGE of a response reports.
Every token of the request counts, cached or not; the size is paired
with the session's size in bytes when the request was made."
  (let ((tokens (cl-loop for key in '(input_tokens cache_creation_input_tokens
                                                   cache_read_input_tokens)
                         for n = (alist-get key usage)
                         when (numberp n) sum n)))
    (when (and (> tokens 0) (lispm-agent-request-bytes agent))
      (setf (lispm-agent-context agent) (cons tokens (lispm-agent-request-bytes agent))))))

(defun lispm--reply-text (content)
  "Text blocks of CONTENT joined."
  (string-trim
   (mapconcat (lambda (b) (if (equal (alist-get 'type b) "text") (alist-get 'text b) ""))
              content "")))

(defun lispm--stop-note (stop message)
  "A note explaining stop reason STOP of MESSAGE, or nil if it is normal."
  (pcase stop
    ((or "end_turn" "stop_sequence" "tool_use" 'nil :null) nil)
    ("max_tokens" "The response hit the output limit (`lispm-max-tokens').")
    ("refusal"
     (let ((details (alist-get 'stop_details message)))
       (format "The model declined to continue%s."
               (if (and (consp details) (stringp (alist-get 'category details)))
                   (format " (%s)" (alist-get 'category details))
                 ""))))
    ("pause_turn" "The model paused its response.")
    (_ (format "The response stopped: %s." stop))))

(defun lispm--decide (agent block invalid stopped)
  "Decide what to do with the tool_use BLOCK of AGENT.
INVALID lists calls with malformed input; STOPPED is the stop reason
if the response ended abnormally.  Return (STATE ACTION MESSAGE), or
\(STATE `ask' REASONS) for a call to await approval, REASONS those
`lispm-tool-decision' gives."
  (let* ((name (alist-get 'name block))
         (tool (lispm-get-tool name)))
    (cond
     (stopped
      (list "FAILED" 'fail (format "Not executed: the response stopped (%s) before the call was complete." stopped)))
     ((member (alist-get 'id block) invalid)
      (list "FAILED" 'fail "INVALID_JSON: the input of this call was not valid JSON. Make the call again."))
     ((not (and tool (lispm--offers-tool-p agent name)))
      (list "FAILED" 'fail (format "There is no tool named %s available to you." name)))
     (t
      (pcase (lispm-tool-decision agent tool (alist-get 'input block))
        (`(auto . ,_) (list "RUNNING" 'run))
        (`(ask . ,reasons) (list "WAITING" 'ask reasons))
        (`(,_ . ,reasons) (list "DENIED" 'deny (lispm-tools-denial agent tool reasons))))))))

(defconst lispm--stop-conditions
  '(("max_tokens" . output-limit)
    ("model_context_window_exceeded" . context-full)
    ("refusal" . refused)
    ("pause_turn" . paused))
  "Conditions a turn stops at when a response stops for these reasons.")

(defun lispm--on-message (agent message)
  "Handle the complete response MESSAGE for AGENT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm--add-usage agent (alist-get 'usage message))
    (lispm--note-context agent (alist-get 'usage message))
    (lispm--live-delete agent)
    (let* ((content (append (alist-get 'content message) nil))
           (stop (alist-get 'stop_reason message))
           (uses (seq-filter (lambda (b) (equal (alist-get 'type b) "tool_use")) content))
           ;; Some gateways report end_turn after tool calls; only a
           ;; response cut short leaves its calls unexecuted.
           (proceed (not (member stop '("max_tokens" "refusal" "model_context_window_exceeded"))))
           (decisions (mapcar (lambda (b)
                                (cons (alist-get 'id b)
                                      (lispm--decide agent b (alist-get 'lispm-invalid message)
                                                     (unless proceed stop))))
                              uses))
           calls)
      (setf (lispm-agent-last-reply agent) (lispm--reply-text content))
      (when content
        (setq calls (lispm--in-session agent
                      (lispm--newline)
                      (insert (lispm-transcript-heading 'assistant))
                      (lispm-transcript-insert-blocks
                       content
                       (lambda (b) (car (alist-get (alist-get 'id b) decisions nil nil #'equal)))))))
      (when uses
        (lispm--dispatch agent uses decisions calls proceed))
      (unless (and uses proceed)
        (let ((note (lispm--stop-note stop message)))
          (pcase (and note (cdr (assoc stop lispm--stop-conditions)))
            ('nil (lispm--finish-turn agent note))
            (type (lispm-signal agent type note))))))))

(defun lispm--dispatch (agent uses decisions calls proceed)
  "Act on the tool calls USES of AGENT per DECISIONS.
CALLS locates their headings.  If PROCEED, request the next step
once every call has a result."
  (let ((pending (lispm-agent-pending agent)))
    (setf (lispm-agent-dispatching agent) t)
    (unwind-protect
        (progn
          ;; Register every call before running any, so that the first
          ;; result cannot resume the loop while others are unregistered.
          (dolist (b uses)
            (let ((id (alist-get 'id b)))
              (pcase-let ((`(,_ ,status ,reasons) (alist-get id decisions nil nil #'equal)))
                (puthash id (list :status status
                                  :name (alist-get 'name b)
                                  :tool (lispm-get-tool (alist-get 'name b))
                                  :input (alist-get 'input b)
                                  :marker (alist-get id calls nil nil #'equal)
                                  :reasons (and (eq status 'ask) reasons))
                         pending))))
          (lispm-set-state agent 'acting)
          (dolist (b uses)
            (let* ((id (alist-get 'id b))
                   (decision (alist-get id decisions nil nil #'equal)))
              (pcase (nth 1 decision)
                ('run (lispm--run-tool agent id))
                ('ask (lispm--ask-approval agent id))
                (_ (lispm--tool-done agent id (nth 2 decision) t (car decision)))))))
      (setf (lispm-agent-dispatching agent) nil))
    (lispm--update-state agent)
    ;; The calls, awaiting approval or running, are on disk.
    (lispm--save-session agent)
    (when proceed
      (lispm--maybe-continue agent))))

(defvar lispm-current-call nil
  "Id of the call whose tool is starting, among the acting agent's pending calls.
A tool that makes calls in turn, such as run_lisp, keeps it to ask
for their approval (see `lispm-call-ask').")

(defun lispm--run-tool (agent id)
  "Run AGENT's pending tool call ID.
A call taken up from a reopened session gets its tool now, by name:
tools of MCP servers appear only once the servers have started."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name)))))
    (if (not tool)
        (lispm--tool-done agent id (format "There is no tool named %s available to you."
                                           (plist-get call :name))
                          t "FAILED")
      (plist-put call :status 'run)
      (lispm--in-session agent
        (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
      (lispm-set-state agent 'acting (format "running %s" (plist-get call :name))
                       (cons 'running (plist-get call :name)))
      (let ((lispm-current-agent agent)
            (lispm-current-call id)
            (default-directory (lispm-agent-root agent)))
        (lispm-tool-invoke tool (plist-get call :input)
                           (lambda (text errorp &optional images)
                             (lispm--call-finished agent id text errorp images)))))))

(defcustom lispm-debug-tool-errors nil
  "Whether a tool's failure stops its call, for the user to choose how to go on.
nil, the default, gives the model the error as the call's result, as
it is usually the model's to handle; t stops the failures of every
tool; a list of tool names, those of these tools.  A stopped call
waits at a `tool-error' condition, as `debug-on-error' enters the
debugger: report the error to the model, run the call again, give the
model a result of your own, or stop the turn.  A call interrupted
with C-g is never stopped."
  :type '(choice (const :tag "Never" nil) (const :tag "Every tool" t)
                 (repeat :tag "These tools" string))
  :group 'lispm)

(defun lispm--debug-tool-error-p (name)
  "Non-nil if a failure of the tool NAME stops its call.
See `lispm-debug-tool-errors'."
  (or (eq lispm-debug-tool-errors t)
      (and (listp lispm-debug-tool-errors)
           (member name (mapcar (lambda (tool) (format "%s" tool)) lispm-debug-tool-errors)))))

(defun lispm--call-finished (agent id text errorp images)
  "Take the result TEXT of AGENT's call ID; ERRORP marks failure.
IMAGES are files the result holds besides TEXT.  A failure that
`lispm-debug-tool-errors' covers stops the call instead."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (if (and errorp call
             (lispm--debug-tool-error-p (plist-get call :name))
             (not (equal text lispm-tools--interrupted)))
        (lispm--stop-call agent id text images)
      (lispm--tool-done agent id text errorp nil images))))

(defun lispm--tool-done (agent id text errorp &optional state images)
  "Record TEXT as the result of AGENT's call ID; ERRORP marks failure.
STATE overrides the call's final TODO keyword.  IMAGES are files the
result holds besides TEXT."
  (when-let* ((call (gethash id (lispm-agent-pending agent))))
    (remhash id (lispm-agent-pending agent))
    (let ((marker (plist-get call :marker)))
      (lispm--in-session agent
        (when (and marker (marker-buffer marker))
          (lispm-transcript-set-state marker (or state (if errorp "FAILED" "DONE")))
          (goto-char (lispm-transcript-subtree-end marker 2))
          (let ((start (point)))
            (insert (lispm-transcript-result text errorp images
                                             (and buffer-file-name
                                                  (file-name-directory buffer-file-name))))
            (when (> (cl-count ?\n text) 12)
              (lispm-transcript-fold start))))))
    (unless (lispm-agent-dispatching agent)
      (lispm--update-state agent)
      (lispm--maybe-continue agent))))

(defun lispm--update-state (agent)
  "Reflect AGENT's pending calls in its state."
  (when (memq (lispm-agent-state agent) '(acting waiting))
    (let ((waiting 0) (stopped 0) (running nil))
      (maphash (lambda (_id call)
                 (pcase (plist-get call :status)
                   ('stopped (cl-incf stopped))
                   ('ask (cl-incf waiting))
                   (_ (setq running (plist-get call :name)))))
               (lispm-agent-pending agent))
      (cond ((> (+ waiting stopped) 0)
             (lispm-set-state
              agent 'waiting
              (string-join
               (delq nil (list (and (> stopped 0)
                                    (format "%d call%s stopped at an error" stopped
                                            (if (= stopped 1) "" "s")))
                               (and (> waiting 0)
                                    (format "%d call%s await%s approval" waiting
                                            (if (= waiting 1) "" "s")
                                            (if (= waiting 1) "s" "")))))
               ", ")))
            (running (lispm-set-state agent 'acting (format "running %s" running)
                                      (cons 'running running)))))))

(defun lispm-agent-status-record (agent)
  "Return what AGENT is doing as a record (:state STATE :kind KIND :msg
MSG), as `lispm-agents-status-functions' describe an agent's buffer:
`blocked' while it waits for its user, on a call awaiting approval
\(KIND `permission') or at a condition awaiting a restart (`question');
`error' while calls of its stopped at errors and nothing else holds it;
`working' in the rest of a turn; else `idle'.  MSG is AGENT's status."
  (let ((status (lispm-agent-status agent)))
    (pcase (lispm-agent-state agent)
      ('idle (list :state 'idle :msg status))
      ('waiting
       (let ((asked 0) (stopped 0))
         (maphash (lambda (_id call)
                    (pcase (plist-get call :status)
                      ('ask (cl-incf asked))
                      ('stopped (cl-incf stopped))))
                  (lispm-agent-pending agent))
         (cond ((lispm-agent-condition agent) (list :state 'blocked :kind 'question :msg status))
               ((> asked 0) (list :state 'blocked :kind 'permission :msg status))
               ((> stopped 0) (list :state 'error :msg status))
               (t (list :state 'blocked :msg status)))))
      (_ (list :state 'working :msg status)))))

(defun lispm--maybe-continue (agent)
  "Request AGENT's next step once all its tool calls have results."
  (when (and (memq (lispm-agent-state agent) '(acting waiting))
             (zerop (hash-table-count (lispm-agent-pending agent))))
    (lispm--request agent)))

(defun lispm--finish-turn (agent &optional note)
  "End AGENT's turn, adding NOTE to the transcript if non-nil."
  (let ((callbacks (lispm-agent-turn-callbacks agent))
        (reply (or (lispm-agent-last-reply agent) "")))
    (setf (lispm-agent-turn-callbacks agent) nil
          (lispm-agent-process agent) nil
          (lispm-agent-waiting-on agent) nil
          (lispm-agent-condition agent) nil)
    (clrhash (lispm-agent-pending agent))
    (lispm--in-session agent
      (lispm--newline)
      (when note
        (insert (lispm-transcript-section (lispm-transcript-title 'note) note)))
      (insert (lispm-transcript-heading 'user)))
    (lispm-set-state agent 'idle)
    (lispm--save-session agent)
    (with-demoted-errors "LispM: taking a snapshot: %S"
      (lispm-journal-end agent))
    (let* ((answer (string-trim (concat reply (if note (concat "\n\n" note) ""))))
           (answer (if (string-empty-p answer) "(no reply)" answer)))
      ;; A value the turn returned goes with the reply, as its property.
      (when (lispm-agent-value agent)
        (setq answer (propertize answer 'lispm-value (lispm-agent-value agent))))
      (dolist (callback callbacks)
        (with-demoted-errors "lispm turn callback: %S"
          (funcall callback answer))))
    ;; Only once whoever waited has the reply: if Emacs stopped before,
    ;; the entry would tell them again.
    (setf (lispm-agent-entry agent) nil)
    (lispm-inbox-save agent)
    (run-hook-with-args 'lispm-turn-end-functions agent reply)
    (when (lispm-agent-inbox agent)
      (run-at-time 0 nil #'lispm--drain-inbox agent))))

(defun lispm--on-error (agent err body betas attempt)
  "Handle the failure ERR of AGENT's request BODY (BETAS), try ATTEMPT."
  (when (eq (lispm-agent-state agent) 'requesting)
    (setf (lispm-agent-process agent) nil)
    (lispm--live-delete agent)
    (if (and (lispm-api-retryable-p err) (< attempt lispm-api-max-retries))
        (let ((delay (* 2 (expt 2 attempt))))
          (lispm-set-state agent 'requesting
                           (format "%s; retrying in %ds" (plist-get err :type) delay)
                           'retry)
          (run-at-time delay nil
                       (lambda ()
                         (when (eq (lispm-agent-state agent) 'requesting)
                           (lispm--send agent body betas (1+ attempt))))))
      (let ((raised (lispm-agent-max-tokens agent)))
        ;; The server may refuse a limit raised past what the model
        ;; allows: lower it again, or every retry fails alike.
        (when (and raised (not (lispm-api-retryable-p err)))
          (setf (lispm-agent-max-tokens agent) nil))
        (lispm-signal
         agent 'request-failed
         (concat (format "The request failed%s: %s: %s"
                         (if (plist-get err :status) (format " (HTTP %s)" (plist-get err :status)) "")
                         (plist-get err :type) (plist-get err :message))
                 (if (and raised (null (lispm-agent-max-tokens agent)))
                     (format "\nThe output limit raised to %d for this turn is back to %d."
                             raised (lispm--max-tokens agent))
                   "")))))))

;;;; Commands

(defvar lispm-chat-agent)

(defun lispm--buffer-agent ()
  "The agent of the current session or chat buffer."
  (or lispm-session-agent
      (bound-and-true-p lispm-chat-agent)
      (user-error "%s" (lispm-tr "Not in an agent session (or its agent is not defined)"))))

(defun lispm-send ()
  "Send what you wrote under the last \"* User\" heading."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (if (and (lispm-agent-condition agent) (not (lispm--open-calls agent)))
        (lispm--send-past-condition agent)
      (when (lispm-agent-busy-p agent)
        (user-error "%s" (lispm-tr "%s is busy (%s); C-c C-k aborts"
                                   (lispm-agent-name agent) (lispm--state-name (lispm-agent-state agent)))))
      (setf (lispm-agent-buffer agent) (current-buffer))
      (let ((parsed (lispm-transcript-parse)))
        (when (plist-get parsed :pending)
          (user-error "%s" (lispm-tr "Some tool calls have no result yet")))
        (unless (equal (plist-get parsed :last-role) "user")
          (user-error "%s" (lispm-tr "Write your message under a \"* User\" heading first"))))
      (lispm--begin-turn agent))))

(defun lispm--send-past-condition (agent)
  "Send what the user wrote after the condition AGENT's turn waits at.
That stops the turn there, as writing after a failed turn always did.
The text is under a \"* User\" heading the user added after the
condition's; without one, say how to go on."
  (let* ((condition (lispm-agent-condition agent))
         (input (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-max))
                    (and (re-search-backward (lispm-transcript-heading-regexp 'user) nil t)
                         (> (point) (plist-get condition :marker))
                         (string-trim (buffer-substring-no-properties
                                       (min (point-max) (1+ (line-end-position)))
                                       (point-max))))))))
    (unless (and input (not (string-empty-p input)))
      (user-error "%s" (lispm-tr "%s waits at a condition: choose a restart with C-c C-y, or write \
under a new \"* User\" heading after it" (lispm-agent-name agent))))
    (lispm--take-input agent)
    (lispm-stop-at-condition agent)
    (lispm-tell agent input)))

(defun lispm-abort (&optional agent)
  "Stop AGENT's turn: cancel the request, the running and waiting calls,
and its queued inputs."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (unless (lispm-agent-busy-p agent)
      (user-error "%s" (lispm-tr "%s is idle" (lispm-agent-name agent))))
    (lispm--stop agent "Aborted by the user.")))

(defun lispm--stop (agent note &optional keep-inbox)
  "Stop AGENT's turn, its calls and queued inputs, recording NOTE.
A condition the turn waits at is closed, its message joining NOTE; one
between turns ends no turn, and NOTE is recorded as an event.  With
KEEP-INBOX, queued inputs stay, to be taken up next."
  (let ((between nil))
    (when-let* ((condition (lispm-agent-condition agent)))
      (setf (lispm-agent-condition agent) nil
            between (lispm--between-turns-p condition))
      (lispm--close-condition agent condition 'abort)
      (setq note (concat (plist-get condition :message) "\n" note)))
    (setf (lispm-agent-compacting agent) nil)
    (when (gethash agent lispm--idle-handovers)
      (lispm--give-up-idle-handover agent))
    (lispm-set-state agent 'aborting)
    (dolist (p (process-list))
      (when (eq (process-get p 'lispm-agent) agent)
        (process-put p 'lispm-aborted t)
        (delete-process p)))
    (lispm--live-delete agent)
    (let (ids)
      (maphash (lambda (id _) (push id ids)) (lispm-agent-pending agent))
      (dolist (id ids)
        (when-let* ((condition (lispm--call-condition agent id)))
          (lispm--settle-call agent condition 'abort))
        (lispm--tool-done agent id "Interrupted: the user aborted the turn." t "FAILED")))
    (let ((inbox (unless keep-inbox (lispm-agent-inbox agent))))
      (unless keep-inbox (setf (lispm-agent-inbox agent) nil))
      (dolist (entry inbox)
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback (format "(%s was stopped before handling this)"
                                    (lispm-agent-name agent))))))
    (if (not between)
        (lispm--finish-turn agent note)
      (lispm--record-event agent "stopped" note)
      (lispm-set-state agent 'idle)
      (lispm--save-session agent)
      (lispm-inbox-save agent))))

;;;; Approval

(defun lispm--denial (reason)
  "What a call the user denied for REASON (nil if none given) results in."
  (concat "The user denied this call."
          (if (and reason (not (string-blank-p reason)))
              (concat " Their reason: " reason)
            " Do not repeat it unchanged; adapt, or ask the user.")))

(defun lispm--approval-event (agent id event &optional reason source)
  "Record and save an approval EVENT for AGENT's call ID.
EVENT is `requested', `resumed', `approve', `allow' or `deny'.
REASON is a denial's explanation; SOURCE names who settled the call,
`user' by default or `allowance' when an earlier allowance settles it.
The call's input is already in its heading or its Asks subsection."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              (marker (plist-get call :marker))
              ((eq (marker-buffer marker) (lispm-agent-buffer agent))))
    (let* ((asked (lispm-asked call))
           (scope (and (eq event 'allow) (lispm-call-allowance agent id)))
           (record `((time . ,(format-time-string "%FT%TZ" nil t))
                     (event . ,(symbol-name event))
                     (call_id . ,id)
                     (tool . ,(plist-get asked :name))
                     ,@(when (memq event '(requested resumed))
                         `((reasons . ,(vconcat (plist-get asked :reasons)))))
                     ,@(when (memq event '(approve allow deny))
                         `((source . ,(symbol-name (or source 'user)))))
                     ,@(when (eq event 'approve) '((scope . "once")))
                     ,@(when scope
                         (if (consp scope)
                             `((scope . "custom") (allowance . ,(cdr scope)))
                           `((scope . ,(symbol-name scope)))))
                     ,@(when reason `((reason . ,reason))))))
      (lispm--in-session agent
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-approval record)))
      (lispm--save-session agent))))

(defun lispm-resolve (agent id decision &optional reason source)
  "Settle AGENT's call ID awaiting approval: DECISION is `approve' or `deny'.
REASON, for a denial, is passed on to the model.  For a call that
awaits approval of a call it makes (see `lispm-call-ask'), that call
is approved or denied, and the call that made it goes on.  DECISION
may also be `allow', from `lispm-allow': approved for good, which the
call that made such a call learns.  SOURCE is `allowance' when another
call's remembered approval settles this one; otherwise it is `user'."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (when (and call (eq (plist-get call :status) 'ask))
      (lispm--approval-event agent id decision reason source)
      (if-let* ((asking (plist-get call :asking)))
          (progn
            (plist-put call :asking nil)
            (plist-put call :status 'run)
            (lispm--in-session agent
              (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
            (lispm--update-state agent)
            (funcall (plist-get asking :continue) decision
                     (and (eq decision 'deny) (lispm--denial reason))))
        (pcase decision
          ((or 'approve 'allow) (lispm--run-tool agent id))
          ('deny (lispm--tool-done agent id (lispm--denial reason) t "DENIED")))))))

(defun lispm-asked (call)
  "What CALL awaits approval for, as a plist (:tool :name :input :reasons).
The call itself; or for a call that makes calls in turn, as a Lisp
program does, the call of its that waits (see `lispm-call-ask'), with
:always, what approving it for good means, if it says.  :reasons are
why functions of `lispm-tool-action-functions' had it ask, if they
did: a list of strings."
  (or (plist-get call :asking)
      (list :tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name)))
            :name (plist-get call :name)
            :input (plist-get call :input)
            :reasons (plist-get call :reasons))))

(defun lispm-call-ask (agent id tool input continue &optional always reasons)
  "Have AGENT's running call ID await approval of a call of TOOL on INPUT.
The running call makes that call in turn, as a Lisp program does; it
waits meanwhile, and the call is shown, approved and denied as if the
model had made it, the transcript recording it under the running
call's heading.  Once the user decides, CONTINUE is called with
`approve', `allow' if they approved it for good, or `deny' and what
the denial results in.  Approving it for good allows AGENT calls like
it, as `lispm-allow' says, unless ALWAYS is non-nil: a sentence saying
what it means instead, shown with the choice, which CONTINUE then
carries out.  REASONS, a list of strings, are why it asks, shown with
it, as `lispm-tool-decision' gives them.  Return nil, and ask nothing,
if ID is not running."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              ((eq (plist-get call :status) 'run)))
    (plist-put call :asking (list :tool tool :name (lispm-tool-name tool) :input input
                                  :continue continue :always always :reasons reasons))
    (plist-put call :status 'ask)
    (when-let* ((marker (plist-get call :marker))
                ((marker-buffer marker)))
      (lispm--in-session agent
        (lispm-transcript-set-state marker "WAITING")
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-asks (lispm-tool-name tool) input))))
    (lispm--update-state agent)
    (lispm--ask-approval agent id)
    t))

(defun lispm-allow (agent id)
  "Approve AGENT's call ID, and calls like it for the rest of the Emacs session.
Like it are later calls of its tool, or for a tool that reaches Emacs,
such as one of class `exec', calls with the same input (see
`lispm-tools-allowance').  AGENT's other calls awaiting approval that
this allows are approved too.  A call that a call makes may mean
something else by it (see `lispm-call-ask'): then the call that made it
is told, and nothing more is allowed."
  (when-let* ((call (gethash id (lispm-agent-pending agent)))
              (asked (lispm-asked call))
              (tool (plist-get asked :tool)))
    (unless (plist-get asked :always)
      (push (lispm-tools-allowance tool (plist-get asked :input) agent) (lispm-agent-allowed agent)))
    (lispm-resolve agent id 'allow)
    ;; Not those a function of `lispm-tool-action-functions' still asks about.
    (dolist (other (lispm-waiting-calls agent))
      (let ((asked (lispm-asked (gethash other (lispm-agent-pending agent)))))
        (when (and (plist-get asked :tool)
                   (lispm-tools--allowed-p agent (plist-get asked :tool) (plist-get asked :input))
                   (eq (lispm-tool-action agent (plist-get asked :tool) (plist-get asked :input))
                       'auto))
          (lispm-resolve agent other 'approve nil 'allowance))))))

(defconst lispm--interrupted-result
  "The call was interrupted: Emacs stopped before it finished, so whether it took effect is unknown.  Check before repeating it."
  "Result of a call that was running when its session was last saved.")

(defvar lispm-resume-call-functions nil
  "Functions that can take up a call left running when its session was saved.
Each is called with the agent and the call, a plist (:id :name :input
:properties) as `lispm-transcript-parse' gives it, while the session
is opened again after Emacs stopped.  One that returns non-nil has
taken the call up: it stays pending, running, until its result comes
\(see `lispm--call-finished').  Otherwise the call is recorded as
interrupted.  Delegation takes up ask_agent calls this way: the agent
asked can still reply.")

(defun lispm--resume-calls (agent)
  "Take up the turn AGENT's session was in when it was last saved, if any.
A session saved in the middle of a turn and opened again, after Emacs
stopped, records what the turn had come to, and the project's inbox
file the input it handled (see `lispm-inbox-take'):

- calls awaiting approval await it again, and once they are settled
  the turn goes on; calls that were running are recorded as
  interrupted, their processes gone, unless a function of
  `lispm-resume-call-functions' takes them up;
- a condition awaiting a restart is waited at again;
- a turn with nothing left to wait for, its calls interrupted or its
  request lost with Emacs, waits at an `interrupted' condition: go on
  with it, or stop there.

Inputs queued for the agent are its again, and taken up once it is
free.  This runs while the session opens, before
`lispm-session-mode-hook' has started its MCP servers: tools are
looked up when a call runs."
  (lispm--restore-model agent)
  (when (and (eq (lispm-agent-state agent) 'idle)
             (zerop (hash-table-count (lispm-agent-pending agent))))
    (pcase-let* ((`(,queued . ,running) (lispm-inbox-take agent))
                 (parsed (lispm-transcript-parse))
                 (calls (plist-get parsed :pending-calls))
                 (condition (plist-get parsed :condition)))
      (dolist (entry queued)
        (unless (seq-find (lambda (e) (equal (plist-get e :id) (plist-get entry :id)))
                          (lispm-agent-inbox agent))
          (setf (lispm-agent-inbox agent) (append (lispm-agent-inbox agent) (list entry)))))
      (cond ((or calls condition (and running (not (lispm--input-heading-p))))
             (lispm--resume-turn agent running calls condition))
            (running
             ;; The turn ended, but Emacs stopped before its input was
             ;; done with: whoever waited for the reply may not have it.
             (lispm--resume-ended-turn agent running parsed)))
      (when (lispm-agent-inbox agent)
        (run-at-time 0 nil #'lispm--drain-inbox agent)))))

(defun lispm--resume-turn (agent entry calls condition)
  "Take up AGENT's turn on ENTRY, which Emacs stopped in the middle of.
ENTRY is the input it handled, from the inbox file, or nil if that is
not known (a session saved before inputs were kept there).  CALLS are
the calls the transcript leaves without a result, CONDITION the
condition it waits at, if any; see `lispm--resume-calls'."
  (cl-incf (lispm-agent-turn agent))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-auto-restarts agent) nil
        (lispm-agent-entry agent) entry
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback)))
        ;; Whoever started it, the user is the one reopening it, unless
        ;; another agent or an event did: then someone waits for it.
        (lispm-agent-attended agent) (not (or (plist-get entry :from) (plist-get entry :event))))
  (with-demoted-errors "LispM: taking a snapshot: %S"
    (lispm-journal-begin agent t))
  (cond
   ((and condition (not calls))
    (lispm--wait-at agent (append (list :marker (copy-marker (plist-get condition :pos)))
                                  condition)))
   (calls
    (dolist (call calls)
      (let* ((marker (copy-marker (plist-get call :pos) t))
             (condition (and (plist-get call :stopped)
                             (equal (plist-get call :state) "WAITING")
                             (lispm--tool-error (plist-get call :id) (plist-get call :name)
                                                (plist-get call :stopped) marker))))
        (puthash (plist-get call :id)
                 (list :status (cond (condition 'stopped)
                                     ((member (plist-get call :state) '("WAITING" "APPROVED"))
                                      'ask)
                                     (t 'run))
                       :name (plist-get call :name)
                       :input (plist-get call :input)
                       :marker marker
                       :condition condition
                       :reasons (plist-get call :reasons))
                 (lispm-agent-pending agent))))
    (lispm-set-state agent 'acting)
    (setf (lispm-agent-dispatching agent) t)
    (unwind-protect
        (dolist (call calls)
          (let ((id (plist-get call :id)))
            (pcase (plist-get call :state)
              ("WAITING"
               (when (eq (plist-get (gethash id (lispm-agent-pending agent)) :status) 'ask)
                 (lispm--approval-event agent id 'resumed)))
              ;; Approved, but never run: ask again rather than run it
              ;; while the session is being opened.
              ("APPROVED"
               (lispm--in-session agent
                 (lispm-transcript-set-state
                  (plist-get (gethash id (lispm-agent-pending agent)) :marker) "WAITING"))
               (lispm--approval-event agent id 'resumed))
              (_ (unless (run-hook-with-args-until-success 'lispm-resume-call-functions agent call)
                   (lispm--tool-done agent id lispm--interrupted-result t))))))
      (setf (lispm-agent-dispatching agent) nil))
    (lispm--update-state agent)
    ;; Calls stopped at an error wait for a restart again; the
    ;; handlers are asked afresh.
    (maphash (lambda (_id call)
               (when-let* ((condition (plist-get call :condition)))
                 (lispm--offer-condition agent condition)))
             (lispm-agent-pending agent))
    (when (zerop (hash-table-count (lispm-agent-pending agent)))
      (lispm-signal agent 'interrupted
                    "Emacs stopped in the middle of this turn, while calls ran; \
they are recorded as interrupted.")))
   (t
    (lispm-signal agent 'interrupted
                  "Emacs stopped in the middle of this turn, while the model was answering."))))

(defun lispm--resume-ended-turn (agent entry parsed)
  "Finish with ENTRY, the input of AGENT's turn that ended as Emacs stopped.
The turn's reply, the text of the last response in PARSED, the
session as `lispm-transcript-parse' gives it, goes to ENTRY's callback,
if it has one; the inbox file then no longer holds ENTRY."
  (when-let* ((callback (plist-get entry :callback)))
    (let* ((last (seq-find (lambda (m) (equal (alist-get 'role m) "assistant"))
                           (reverse (plist-get parsed :messages))))
           (reply (string-trim (lispm--reply-text (append (alist-get 'content last) nil)))))
      (with-demoted-errors "lispm turn callback: %S"
        (funcall callback (if (string-empty-p reply) "(no reply)" reply)))))
  (lispm-inbox-save agent))

(defun lispm--ask-approval (agent id)
  "Show AGENT's call ID for approval: a preview, why it asks, a notification.
They are shown once the preview is made: for a diff, by a program Emacs
does not wait for (see `lispm-tool-preview-text'), if the call still
awaits the same approval by then."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (asked (lispm-asked call))
         (asking (plist-get call :asking))
         (announce (lambda (preview)
                     (when (and (eq (gethash id (lispm-agent-pending agent)) call)
                                (eq (plist-get call :status) 'ask)
                                (eq (plist-get call :asking) asking))
                       (lispm--announce-approval agent call asked preview)))))
    (lispm--approval-event agent id 'requested)
    (if (plist-get asked :tool)
        (let ((lispm-current-agent agent)
              (default-directory (lispm-agent-root agent)))
          (lispm-tool-preview-text (plist-get asked :tool) (plist-get asked :input) announce))
      (funcall announce nil))))

(defun lispm--announce-approval (agent call asked preview)
  "Show AGENT's CALL, awaiting approval of ASKED (see `lispm-asked'), with PREVIEW.
PREVIEW, a string or nil, and why it asks go in the transcript; then
`lispm-approval-functions' run, and a message says where to answer."
  (let ((reasons (plist-get asked :reasons)))
    (when (and (or preview reasons) (plist-get call :marker))
      (lispm--in-session agent
        (goto-char (lispm-transcript-subtree-end (plist-get call :marker) 2))
        (insert (lispm-transcript-preview preview reasons))))
    (run-hook-with-args 'lispm-approval-functions agent (plist-get asked :name)
                        (plist-get asked :input))
    (let ((said (run-hook-with-args-until-success 'lispm-approval-message-functions
                                                  agent (plist-get asked :name))))
      (unless (eq said t)
        (message "%s" (cond (said)
                            ((buffer-live-p (lispm-agent-view agent))
                             (lispm-tr "λ %s wants to run %s: answer in %s" (lispm-agent-name agent)
                                       (plist-get asked :name) (buffer-name (lispm-agent-view agent))))
                            (t (lispm-tr "λ %s wants to run %s: review it with C-c C-y in %s"
                                         (lispm-agent-name agent) (plist-get asked :name)
                                         (buffer-name (lispm-agent-buffer agent))))))))))

(defun lispm--todo-state-changed ()
  "Settle a call or condition whose heading the user moved to a new TODO state.
A call is approved or denied; a condition, its turn's or one a call
stopped at, gets its default restart or the one that stops there.
This makes `org-todo', and the org agenda, an approval interface."
  (when-let* ((agent lispm-session-agent))
    (let* ((id (org-entry-get nil "TOOL_ID"))
           (call (and id (gethash id (lispm-agent-pending agent))))
           (condition (if id
                          (plist-get call :condition)
                        (let ((turn (lispm-agent-condition agent)))
                          (and (equal (org-entry-get nil "CONDITION_ID") (plist-get turn :id))
                               turn)))))
      (cond
       (condition
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (car (plist-get condition :restarts))))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (lispm-stop-restart condition)))))
       ((eq (plist-get call :status) 'ask)
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-resolve agent id 'approve))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-resolve agent id 'deny))))))))

(defun lispm--calls (agent statuses)
  "Ids of AGENT's calls with one of STATUSES, in transcript order."
  (let (ids)
    (maphash (lambda (id call)
               (when (memq (plist-get call :status) statuses)
                 (push (cons id (and (plist-get call :marker)
                                     (marker-position (plist-get call :marker))))
                       ids)))
             (lispm-agent-pending agent))
    (mapcar #'car (sort ids (lambda (a b) (< (or (cdr a) 0) (or (cdr b) 0)))))))

(defun lispm-waiting-calls (agent)
  "Ids of AGENT's calls awaiting approval, in transcript order."
  (lispm--calls agent '(ask)))

(defun lispm--open-calls (agent)
  "Ids of AGENT's calls awaiting an answer, in transcript order.
That is approval, or for a call stopped at an error, a restart."
  (lispm--calls agent '(ask stopped)))

(defun lispm--call-at-point (agent)
  "Id of AGENT's call awaiting an answer at point, if any."
  (when-let* ((id (ignore-errors (org-entry-get nil "TOOL_ID" t)))
              (call (gethash id (lispm-agent-pending agent))))
    (and (memq (plist-get call :status) '(ask stopped)) id)))

(defun lispm--answered-call (agent)
  "Id of AGENT's call at point awaiting an answer, or of its first one."
  (or (lispm--call-at-point agent) (car (lispm--open-calls agent))))

(defun lispm-review ()
  "Review the call at point, or the first awaiting approval, and decide.
For a call stopped at an error, or with none, the condition the turn
waits at, choose a restart."
  (interactive)
  (let* ((agent (lispm--buffer-agent))
         (id (lispm--answered-call agent))
         (condition (if id (lispm--call-condition agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-choose-restart agent condition))
          (id (lispm--review-call agent id))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm-call-allowance (agent id)
  "What approving AGENT's call ID for good allows from then on.
\(always . TEXT) when the call says, in TEXT; `exact' when it allows
only this exact call again, see `lispm-tools-allowance'; else `tool',
every later call of its tool."
  (let ((asked (lispm-asked (gethash id (lispm-agent-pending agent)))))
    (cond ((plist-get asked :always) (cons 'always (plist-get asked :always)))
          ((and (plist-get asked :tool)
                (consp (lispm-tools-allowance (plist-get asked :tool) (plist-get asked :input) agent)))
           'exact)
          (t 'tool))))

(defun lispm--review-call (agent id)
  "Review AGENT's call ID awaiting approval, and decide."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (asked (lispm-asked call)))
    (when (plist-get call :marker)
      (goto-char (plist-get call :marker)))
    (ignore-errors (org-fold-show-subtree))
    (recenter 0)
    (pcase (car (read-multiple-choice
                 (lispm-tr "%s wants to run %s" (lispm-agent-name agent) (plist-get asked :name))
                 `((?y ,(lispm-tr-in "answer" "yes") ,(lispm-tr "Run this call"))
                   (?n ,(lispm-tr-in "answer" "no") ,(lispm-tr "Deny it; you may give a reason"))
                   (?a ,(lispm-tr-in "answer" "always")
                       ,(pcase (lispm-call-allowance agent id)
                          (`(always . ,text) (lispm-tr text))
                          ('exact (lispm-tr "Run it, and this exact call again, this Emacs session"))
                          (_ (lispm-tr "Run it, and every later call of this tool, this Emacs session"))))
                   (?q ,(lispm-tr-in "answer" "later") ,(lispm-tr "Decide later")))))
      (?y (lispm-resolve agent id 'approve))
      (?n (lispm-resolve agent id 'deny (read-string (lispm-tr "Reason (optional): "))))
      (?a (lispm-allow agent id)))))

(defun lispm-approve-for (agent &optional id)
  "Approve AGENT's call ID, by default its first awaiting an answer.
For a call stopped at an error, or with none, choose the default restart
of the condition it waits at."
  (let* ((id (or id (car (lispm--open-calls agent))))
         (condition (if id (lispm--call-condition agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-restart agent (plist-get condition :id)
                                    (car (plist-get condition :restarts))))
          (id (lispm-resolve agent id 'approve))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm-deny-for (agent reason &optional id)
  "Deny AGENT's call ID, by default its first awaiting an answer, for REASON.
For a call stopped at an error, or with none, stop at the condition it
waits at (see `lispm-stop-at-condition')."
  (let* ((id (or id (car (lispm--open-calls agent))))
         (condition (if id (lispm--call-condition agent id) (lispm-agent-condition agent))))
    (cond (condition (lispm-restart agent (plist-get condition :id)
                                    (lispm-stop-restart condition)))
          (id (lispm-resolve agent id 'deny reason))
          (t (user-error "%s" (lispm-tr "No call awaits approval"))))))

(defun lispm-approve ()
  "Approve the call at point, or the first one awaiting approval.
With none, choose the default restart of the condition the turn waits at."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (lispm-approve-for agent (lispm--answered-call agent))))

(defun lispm-deny (reason)
  "Deny the call at point, or the first one awaiting approval, for REASON.
With none, stop at the condition the turn waits at (see
`lispm-stop-at-condition')."
  (interactive
   (let* ((agent (lispm--buffer-agent))
          (id (lispm--answered-call agent)))
     (list (and id (not (lispm--call-condition agent id))
                (read-string (lispm-tr "Reason (optional): "))))))
  (let ((agent (lispm--buffer-agent)))
    (lispm-deny-for agent reason (lispm--answered-call agent))))

;;;; Conditions and restarts
;;
;; A turn that stops early does not end: it waits at a condition, as
;; a call waits for approval, until a restart is chosen, as on a Lisp
;; Machine an error waits in the debugger until a proceed option is.
;; The condition is a heading in the transcript (a WAITING "Condition:"
;; section, never sent to the model), so it survives a restart of
;; Emacs; `lispm-condition-handlers' may choose a restart at once, else
;; the user does, in the chat, the org transcript or the agenda.
;; `abort' ends the turn as it ended before: a note, nothing more.

(defvar lispm-condition-types
  `((request-failed
     :title ,(lispm-tr-noop "Request failed")
     :restarts ((retry ,(lispm-tr-noop "Retry the request") lispm--restart-retry)
                (use-model ,(lispm-tr-noop "Retry with another backend or model")
                           lispm--restart-use-model lispm--read-model)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (step-limit
     :title ,(lispm-tr-noop "Step limit reached")
     :restarts ((continue ,(lispm-tr-noop "Continue, for as many requests again") lispm--restart-continue)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (output-limit
     :title ,(lispm-tr-noop "Response cut short")
     :restarts ((retry-larger ,(lispm-tr-noop "Ask again, with twice the output limit")
                              lispm--restart-retry-larger)
                (keep ,(lispm-tr-noop "Keep what it wrote, and let it go on") lispm--restart-keep)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (context-full
     :title ,(lispm-tr-noop "Context window full")
     :restarts ((compact ,(lispm-tr-noop "Compact the conversation, then go on") lispm--restart-compact)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (refused
     :title ,(lispm-tr-noop "Response refused")
     :restarts ((retry ,(lispm-tr-noop "Ask again") lispm--restart-ask-again)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (paused
     :title ,(lispm-tr-noop "Response paused")
     :restarts ((continue ,(lispm-tr-noop "Let it go on") lispm--restart-go-on)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort)))
    (compaction-failed
     :title ,(lispm-tr-noop "Compaction failed")
     :restarts ((retry ,(lispm-tr-noop "Try to compact again") lispm--restart-compaction-retry)
                (skip ,(lispm-tr-noop "Carry on without compacting") lispm--restart-compaction-skip)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-compaction-abort)))
    (tool-error
     :title ,(lispm-tr-noop "Call failed")
     :restarts ((report ,(lispm-tr-noop "Tell the model it failed") lispm--restart-report)
                (retry ,(lispm-tr-noop "Run it again") lispm--restart-rerun)
                (use-value ,(lispm-tr-noop "Give the model a result of your own")
                           lispm--restart-use-value lispm--read-result)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-stop-call)))
    (interrupted
     :title ,(lispm-tr-noop "Turn interrupted")
     :restarts ((continue ,(lispm-tr-noop "Go on with the turn") lispm--restart-retry)
                (abort ,(lispm-tr-noop "Stop here") lispm--restart-abort))))
  "Kinds of conditions a turn can stop at, and how it can go on.
Each entry is (TYPE :title TITLE :restarts RESTARTS).  TITLE says what
happened.  Each restart is (NAME DESCRIPTION FUNCTION [READER]), the
first being the default: FUNCTION is called with the agent, the
condition (a plist: :id :type :title :message :restarts) and a value,
which READER, if any, asks the user for when they choose it.  TITLE
and DESCRIPTION are English, as the transcript records them, and
translated where shown (see lispm-i18n.el).")

(defcustom lispm-condition-handlers '((paused continue :times 10))
  "Restarts chosen without asking, by condition type, in turns the user started.
Each entry is (TYPE RESTART . PROPS): a condition of TYPE gets RESTART
at once, or after :after seconds, at most :times times in a turn (1
by default), or for a call stopped at an error, in that call; after
that, :then names the restart to choose, or the user chooses.  :value
is the value RESTART needs, if any.  A call stopped at an error that
a handler runs again runs only if the policy would run it unasked;
else it awaits approval again.  For instance, to retry a failed
request after a minute, twice:

  ((request-failed retry :after 60 :times 2))

By default a paused response goes on, ten times in a turn at most.
Turns started by another agent's message or by an event go by
`lispm-unattended-condition-handlers' instead."
  :type '(alist :key-type symbol :value-type (cons symbol plist))
  :group 'lispm)

(defcustom lispm-unattended-condition-handlers
  '((request-failed retry :after 60 :times 2 :then abort)
    (step-limit abort)
    (output-limit retry-larger :then abort)
    (context-full compact :then abort)
    (refused abort)
    (paused continue :times 10 :then abort)
    (compaction-failed skip)
    (tool-error report)
    (interrupted continue :then abort))
  "Restarts chosen without asking in turns nobody may be watching.
Those are turns started by another agent's message (a helper's, say)
or by an event (a hook, a timer), which, left to wait for the user,
would hold up whoever waits for them, or the next event.  Entries are
as in `lispm-condition-handlers'.  By default a failed request is
tried again twice, a minute apart, and then the turn stops, as does
one that reaches `lispm-max-steps' or is refused; a response cut
short is asked for again with a larger limit, or the session handed
over, once; a paused one goes on; a failed handover is skipped; a
call stopped at an error reports it to the model; a turn Emacs
stopped in the middle of goes on, once its session is open again.
Meanwhile the user can choose."
  :type '(alist :key-type symbol :value-type (cons symbol plist))
  :group 'lispm)

(defvar lispm-condition-functions nil
  "Abnormal hook run with an agent and a condition its turn waits at.
The condition is a plist (:id :type :title :message :restarts).  Use
it for notifications, as `lispm-approval-functions' for approvals.")

(defun lispm--condition-spec (type)
  "The entry of `lispm-condition-types' for TYPE."
  (or (alist-get type lispm-condition-types)
      (error "No condition type %s" type)))

(defun lispm--restart-spec (condition restart)
  "The restart RESTART of CONDITION, as `lispm-condition-types' gives it."
  (or (assq restart (plist-get (lispm--condition-spec (plist-get condition :type)) :restarts))
      (error "%s is not a restart of %s" restart (plist-get condition :type))))

(defun lispm-signal (agent type message &rest props)
  "Stop AGENT's turn at a condition of TYPE, saying MESSAGE.
The turn waits there until a restart is chosen (see `lispm-restart'):
by `lispm-condition-handlers', or by the user.  PROPS: :resume
non-nil if a turn waits in the middle, for a condition signaled
between turns (`compaction-failed')."
  (let* ((spec (lispm--condition-spec type))
         (restarts (mapcar #'car (plist-get spec :restarts)))
         (id (lispm--in-session agent
               (format "c%d" (1+ (count-matches "^:CONDITION_ID:" (point-min) (point-max))))))
         (resume (plist-get props :resume))
         (condition (list :id id :type type :title (plist-get spec :title)
                          :message message :restarts restarts :resume resume)))
    (setf (lispm-agent-process agent) nil)
    (lispm--in-session agent
      ;; Before the input heading a turn ended with, if any: the next
      ;; turn takes its place.
      (if-let* ((input (lispm--input-heading-p)))
          (goto-char input)
        (lispm--newline))
      (setq condition (plist-put condition :marker (point-marker)))
      (insert (lispm-transcript-condition (plist-get spec :title) id type restarts message
                                          resume)))
    (lispm--save-session agent)
    (lispm--wait-at agent condition)
    condition))

(defun lispm--wait-at (agent condition)
  "Make AGENT wait at CONDITION; let the handlers choose a restart, if any."
  (setf (lispm-agent-condition agent) condition)
  (lispm-set-state agent 'waiting
                   (format "%s: %s" (plist-get condition :title)
                           (lispm--oneline (plist-get condition :message) 80)))
  (lispm--offer-condition agent condition))

(defun lispm--call-condition (agent id)
  "The condition AGENT's call ID stopped at, if it did."
  (plist-get (gethash id (lispm-agent-pending agent)) :condition))

(defun lispm--find-condition (agent id)
  "The condition AGENT waits at whose id is ID: its turn's, or a call's."
  (let ((turn (lispm-agent-condition agent)))
    (if (equal (plist-get turn :id) id)
        turn
      (lispm--call-condition agent id))))

(defun lispm-asked-condition (agent)
  "The condition AGENT asks about first, if any.
That of its first call awaiting an answer, if that call stopped at an
error; else, when no call awaits one, the condition its turn waits at."
  (if-let* ((id (car (lispm--open-calls agent))))
      (lispm--call-condition agent id)
    (lispm-agent-condition agent)))

(defun lispm--offer-condition (agent condition)
  "Tell whoever listens that AGENT waits at CONDITION; let the handlers choose."
  (run-hook-with-args 'lispm-condition-functions agent condition)
  (unless (lispm--handle-condition agent condition)
    (message "%s" (if (buffer-live-p (lispm-agent-view agent))
                      (lispm-tr "λ %s: %s; choose what to do in %s" (lispm-agent-name agent)
                                (lispm-tr (plist-get condition :title))
                                (buffer-name (lispm-agent-view agent)))
                    (lispm-tr "λ %s: %s; choose what to do with C-c C-y in %s" (lispm-agent-name agent)
                              (lispm-tr (plist-get condition :title))
                              (buffer-name (lispm-agent-buffer agent)))))))

(defvar lispm--handler-restart nil
  "Non-nil while a handler, rather than the user, chooses a restart.")

(defun lispm--handle-condition (agent condition)
  "Choose a restart for AGENT's CONDITION by the handlers of its turn.
Those are `lispm-condition-handlers', or for a turn the user did not
start, `lispm-unattended-condition-handlers'.  Return non-nil if one
is chosen, now or after a delay."
  (pcase-let* ((type (plist-get condition :type))
               (`(,first . ,props) (alist-get type (if (lispm-agent-attended agent)
                                                         lispm-condition-handlers
                                                       lispm-unattended-condition-handlers)))
               ;; A call's condition counts for that call alone.
               (key (if-let* ((call (plist-get condition :call))) (cons type call) type))
               (count (alist-get key (lispm-agent-auto-restarts agent) 0 nil #'equal))
               (again (< count (or (plist-get props :times) 1)))
               (restart (if again first (plist-get props :then))))
    (when (and restart (memq restart (plist-get condition :restarts)))
      (when again
        (setf (alist-get key (lispm-agent-auto-restarts agent) nil nil #'equal) (1+ count)))
      (let ((after (if again (or (plist-get props :after) 0) 0))
            (value (plist-get props :value))
            (id (plist-get condition :id)))
        (when (> after 0)
          (lispm-set-state agent 'waiting
                           (format "%s; %s in %ds" (plist-get condition :title) restart after)))
        (run-at-time after nil
                     (lambda ()
                       ;; Unless someone chose meanwhile: this very
                       ;; condition, since an id can be given again
                       ;; once its heading is gone.
                       (when (eq (lispm--find-condition agent id) condition)
                         (let ((lispm--handler-restart t))
                           (lispm-restart agent id restart value)))))
        t))))

(defun lispm-restart (agent id restart &optional value)
  "Go on with AGENT's turn, stopped at the condition ID, by RESTART.
VALUE is what the restart needs, if anything: one with a reader in
`lispm-condition-types' needs one, such as `use-model', a cons
\(BACKEND . MODEL).  The condition's heading records the choice."
  (let ((condition (or (lispm--find-condition agent id)
                       (user-error "%s" (lispm-tr "%s waits at no condition %s" (lispm-agent-name agent) id)))))
    (let ((spec (lispm--restart-spec condition restart)))
      (when (and (nth 3 spec) (null value))
        (user-error "%s" (lispm-tr "The restart %s needs a value" restart)))
      (if (plist-get condition :call)
          (lispm--settle-call agent condition restart)
        (setf (lispm-agent-condition agent) nil)
        (lispm--close-condition agent condition restart value))
      (funcall (nth 2 spec) agent condition value))))

(defun lispm--settle-call (agent condition restart)
  "Note that the call CONDITION stopped AGENT at is settled by RESTART.
The call is no longer stopped; its heading loses :CONDITION:, and its
Stopped subsection names the restart."
  (let ((call (gethash (plist-get condition :call) (lispm-agent-pending agent))))
    (plist-put call :condition nil)
    (when-let* ((marker (plist-get call :marker))
                ((marker-buffer marker)))
      (lispm--in-session agent
        (lispm-transcript-set-property marker "CONDITION" nil)
        (goto-char marker)
        (when (re-search-forward "^\\*\\*\\* Stopped[ \t]*$"
                                 (lispm-transcript-subtree-end marker 2) t)
          (replace-match (format "*** Stopped: %s" restart) t t))))))

(defun lispm-stop-at-condition (agent)
  "Stop AGENT's turn at the condition it asks about, if any.
That is restart `abort', or for a condition without it, the last of
its restarts.  What the user asks of AGENT next goes past it this way."
  (when-let* ((condition (lispm-asked-condition agent)))
    (lispm-restart agent (plist-get condition :id) (lispm-stop-restart condition))))

(defun lispm-stop-restart (condition)
  "The restart of CONDITION that stops there: `abort', or its last one."
  (let ((restarts (plist-get condition :restarts)))
    (if (memq 'abort restarts) 'abort (car (last restarts)))))

(defun lispm--between-turns-p (condition)
  "Non-nil if CONDITION was signaled between turns, not in one.
Only a failed handover is: it comes before the input that asked for it."
  (and (eq (plist-get condition :type) 'compaction-failed)
       (not (plist-get condition :resume))))

(defun lispm-awaiting (agent)
  "What AGENT awaits an answer to: its calls awaiting one, then its condition."
  (append (lispm--open-calls agent)
          (and (lispm-agent-condition agent) (list (lispm-agent-condition agent)))))

(defun lispm-restart-description (condition restart)
  "How RESTART of CONDITION is described to the user, in the language
spoken: its name if unknown."
  (if-let* ((spec (assq restart (plist-get (alist-get (plist-get condition :type)
                                                       lispm-condition-types)
                                            :restarts))))
      (lispm-tr (nth 1 spec))
    (symbol-name restart)))

(defun lispm--close-condition (agent condition restart &optional value)
  "Record in AGENT's transcript that CONDITION was settled by RESTART.
Its heading becomes DONE and names the restart, and the VALUE given
it, if any; `abort' removes it, since the note the turn ends with
says the same."
  (when-let* ((marker (plist-get condition :marker))
              ((marker-buffer marker)))
    (lispm--in-session agent
      (goto-char marker)
      ;; Changing its TODO state in org may have chosen the restart.
      (when (looking-at (concat "\\* " lispm-transcript--state-re " Condition: "))
        (if (eq restart 'abort)
            (delete-region marker (lispm-transcript-subtree-end marker 1))
          (lispm-transcript-set-state marker "DONE")
          (lispm-transcript-set-property marker "RESTART" restart)
          (when value
            (lispm-transcript-set-property marker "VALUE" (let ((print-escape-newlines t))
                                                             (prin1-to-string value)))))))))

(defun lispm-choose-restart (&optional agent condition)
  "Choose a restart for CONDITION, by default the one AGENT asks about.
Interactively, the agent of the current session or chat."
  (interactive)
  (let* ((agent (or agent (lispm--buffer-agent)))
         (condition (or condition
                        (lispm-asked-condition agent)
                        (user-error "%s" (lispm-tr "%s waits at no condition" (lispm-agent-name agent)))))
         (keys nil)
         (choices
          (mapcar (lambda (name)
                    (let* ((letter (aref (symbol-name name) 0))
                           (key (if (or (memq letter keys) (eq letter ?q))
                                    (+ ?1 (length keys))
                                  letter)))
                      (push key keys)
                      (list key (symbol-name name) (lispm-restart-description condition name))))
                  (plist-get condition :restarts)))
         (answer (read-multiple-choice
                  (format "%s: %s" (lispm-tr (plist-get condition :title)) (plist-get condition :message))
                  (append choices `((?q ,(lispm-tr-in "answer" "later") ,(lispm-tr "Decide later")))))))
    (unless (eq (car answer) ?q)
      (lispm-choose-restart-named agent (intern (nth 1 answer)) condition))))

(defun lispm-choose-restart-named (agent restart &optional condition)
  "Choose RESTART for CONDITION, by default the one AGENT asks about.
If the restart needs a value, ask the user for it."
  (let* ((condition (or condition
                        (lispm-asked-condition agent)
                        (user-error "%s" (lispm-tr "%s waits at no condition" (lispm-agent-name agent)))))
         (reader (nth 3 (lispm--restart-spec condition restart)))
         (value (and reader (funcall reader agent))))
    ;; Reading took time: the condition may be settled meanwhile.
    (unless (eq condition (lispm--find-condition agent (plist-get condition :id)))
      (user-error "%s" (lispm-tr "%s no longer waits at that condition" (lispm-agent-name agent))))
    (lispm-restart agent (plist-get condition :id) restart value)))

(defun lispm--restart-retry (agent _condition _value)
  "Send AGENT's request again."
  (lispm--request agent))

(defun lispm--restart-continue (agent _condition _value)
  "Go on with AGENT's turn for another `lispm-max-steps' requests."
  (setf (lispm-agent-steps agent) 0)
  (lispm--request agent))

(defun lispm--restart-use-model (agent _condition value)
  "Retry AGENT's request with the backend and model in VALUE, (BACKEND . MODEL)."
  (lispm-use-model agent (car value) (cdr value))
  (lispm--request agent))

(defun lispm-use-model (agent backend model)
  "Have AGENT use BACKEND (a key of `lispm-backends', nil for the default)
and MODEL (nil for the backend's), for the rest of its session."
  (when (and backend (not (lispm-backend backend)))
    (user-error "%s" (lispm-tr "Unknown backend %s (see `lispm-backends')" backend)))
  (setf (lispm-agent-backend agent) backend
        (lispm-agent-model agent) model))

(defun lispm--restore-model (agent)
  "Have AGENT use the model its session last switched to with `use-model'.
The condition's heading records it, so it outlives a restart of Emacs."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (when (re-search-backward "^:RESTART: use-model\n:VALUE: \\(.*\\)$" nil t)
        (when-let* ((value (ignore-errors (car (read-from-string (match-string 1)))))
                    ((consp value))
                    ((or (null (car value)) (lispm-backend (car value)))))
          (setf (lispm-agent-backend agent) (car value)
                (lispm-agent-model agent) (cdr value)))))))

(defun lispm--restart-abort (agent condition _value)
  "End AGENT's turn at CONDITION, as a turn stopped early used to end."
  (lispm--finish-turn agent (plist-get condition :message)))

(defun lispm--tool-error (id name text marker &optional images)
  "The condition of the call ID, of the tool NAME, stopped at the error TEXT.
MARKER is where the call's heading is; IMAGES are files the failure
holds besides TEXT."
  (let ((spec (lispm--condition-spec 'tool-error)))
    (list :id id :call id :type 'tool-error :title (plist-get spec :title)
          :message (format "%s failed: %s" name text) :error text :images images
          :restarts (mapcar #'car (plist-get spec :restarts)) :marker marker)))

(defun lispm--stop-call (agent id text images)
  "Stop AGENT's call ID at the error TEXT, instead of giving it to the model.
IMAGES are files the failure holds besides TEXT.  The call waits at a
`tool-error' condition of its own: its heading goes back to WAITING,
with :CONDITION: and a Stopped subsection holding TEXT, so that a
session reopened later takes it up again (without IMAGES)."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (marker (plist-get call :marker))
         (condition (lispm--tool-error id (plist-get call :name) text marker images)))
    (plist-put call :status 'stopped)
    (plist-put call :condition condition)
    (when (and marker (marker-buffer marker))
      (lispm--in-session agent
        (lispm-transcript-set-state marker "WAITING")
        (lispm-transcript-set-property marker "CONDITION" "tool-error")
        (goto-char (lispm-transcript-subtree-end marker 2))
        (insert (lispm-transcript-stopped text))))
    (lispm--save-session agent)
    (unless (lispm-agent-dispatching agent)
      (lispm--update-state agent))
    (lispm--offer-condition agent condition)))

(defun lispm--restart-report (agent condition _value)
  "Give the model the error the call stopped at CONDITION, as its result."
  (lispm--tool-done agent (plist-get condition :call) (plist-get condition :error) t
                    nil (plist-get condition :images)))

(defun lispm--restart-rerun (agent condition _value)
  "Run the call stopped at CONDITION again.
Chosen by a handler rather than the user, it runs only if the policy
would run it unasked; else it awaits approval again."
  (let* ((id (plist-get condition :call))
         (call (gethash id (lispm-agent-pending agent)))
         (tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name))))
         (decision (and lispm--handler-restart tool
                        (lispm-tool-decision agent tool (plist-get call :input)))))
    (if (and decision (not (eq (car decision) 'auto)))
        (progn
          (plist-put call :status 'ask)
          (plist-put call :reasons (cdr decision))
          (lispm--update-state agent)
          (lispm--ask-approval agent id))
      (lispm--run-tool agent id))))

(defun lispm--restart-use-value (agent condition value)
  "Give the model VALUE as the result of the call stopped at CONDITION."
  (lispm--tool-done agent (plist-get condition :call) value nil))

(defun lispm--read-result (_agent)
  "Ask the user for the result to give the model."
  (let ((result (read-string (lispm-tr "Result to give the model: "))))
    (when (string-blank-p result)
      (user-error "%s" (lispm-tr "No result given")))
    result))

(defun lispm--restart-stop-call (agent condition _value)
  "Stop AGENT's turn at the call stopped at CONDITION.
The call gets its error as its result, the other calls stop, and the
turn ends with the error; inputs waiting for AGENT stay."
  (let ((id (plist-get condition :call)))
    ;; Its result must not ask the model again.
    (setf (lispm-agent-dispatching agent) t)
    (unwind-protect
        (lispm--tool-done agent id (plist-get condition :error) t
                          nil (plist-get condition :images))
      (setf (lispm-agent-dispatching agent) nil))
    (lispm--stop agent (plist-get condition :message) t)))

(defun lispm--drop-response (agent condition)
  "Leave out of AGENT's requests the response CONDITION stopped at.
That is the \"* Assistant\" section right before CONDITION's heading,
if there is one (a response may have had nothing in it): tagged
:ignore:, it stays in the transcript but is no longer sent."
  (when-let* ((marker (plist-get condition :marker))
              ((marker-buffer marker)))
    (lispm--in-session agent
      (goto-char marker)
      (when (and (re-search-backward "^\\* " nil t)
                 (looking-at (lispm-transcript-heading-regexp 'assistant)))
        (end-of-line)
        (insert " :ignore:")))))

(defun lispm--restart-ask-again (agent condition _value)
  "Leave out the response AGENT's CONDITION stopped at, and ask again."
  (lispm--drop-response agent condition)
  (lispm--request agent))

(defun lispm--restart-retry-larger (agent condition _value)
  "Leave out the response cut short at CONDITION; ask again with twice the limit.
The limit stays raised for the rest of AGENT's turn."
  (setf (lispm-agent-max-tokens agent) (* 2 (lispm--max-tokens agent)))
  (lispm--restart-ask-again agent condition nil))

(defun lispm--tell-for-user (agent text)
  "Tell AGENT's model TEXT, as LispM passing on the user's choice; ask it again."
  (lispm--add-section agent (lispm-transcript-title 'from-lispm) text)
  (lispm--request agent))

(defun lispm--restart-keep (agent _condition _value)
  "Keep the response AGENT's turn stopped at, and let the model go on."
  (lispm--tell-for-user
   agent "Your last response was cut short by the output limit before it was complete; \
calls in it were not run.  Go on from where it stopped."))

(defun lispm--restart-go-on (agent _condition _value)
  "Let AGENT's model resume a response it paused.
The response goes back as it is, which is how the API resumes it."
  (setf (lispm-agent-continuing agent) t)
  (lispm--request agent))

(defun lispm--restart-compact (agent condition _value)
  "Leave out the response cut short at CONDITION, hand AGENT's session over,
and go on with the turn in the continuation."
  (lispm--drop-response agent condition)
  (setf (lispm-agent-compacting agent) t)
  (lispm--summarize agent t))

(defun lispm--restart-compaction-retry (agent condition _value)
  "Try again to hand AGENT's session over, as CONDITION says it was being."
  (setf (lispm-agent-compacting agent) t)
  (lispm--summarize agent (plist-get condition :resume)))

(defun lispm--restart-compaction-abort (agent condition _value)
  "Stop at CONDITION, AGENT's failed handover.
In the middle of a turn, the turn ends; between turns, the input that
asked for the handover is dropped, and whoever sent it told."
  (if (plist-get condition :resume)
      (lispm--finish-turn agent (plist-get condition :message))
    (when-let* ((entry (pop (lispm-agent-inbox agent))))
      (when-let* ((callback (plist-get entry :callback)))
        (funcall callback (format "(%s stopped before handling this: %s)"
                                  (lispm-agent-name agent) (plist-get condition :message)))))
    (lispm--record-event agent "compaction" (plist-get condition :message))
    (lispm-set-state agent 'idle)
    (lispm--save-session agent)
    (lispm-inbox-save agent)
    (lispm--drain-inbox agent)))

(defun lispm--restart-compaction-skip (agent condition _value)
  "Carry on in AGENT's long session, without handing it over.
In the middle of a turn, the turn goes on; between turns, the input
that asked for the handover starts the next one."
  (if (plist-get condition :resume)
      (lispm--request agent)
    ;; Do not try again before the input that asked for it.
    (when (lispm-agent-inbox agent)
      (setcar (lispm-agent-inbox agent)
              (plist-put (car (lispm-agent-inbox agent)) :no-compact t)))
    (lispm-set-state agent 'idle)
    (lispm--drain-inbox agent)))

(defun lispm--read-model (agent)
  "Ask for a backend and a model for AGENT; return (BACKEND . MODEL)."
  (let* ((current (lispm-backend-name agent))
         (own (when-let* ((character (lispm-agent-character* agent)))
                (lispm-character-backend-name character)))
         (name (completing-read (format-prompt (lispm-tr "Backend") current)
                                (delete-dups
                                 (append (and own (lispm-backend own) (list (symbol-name own)))
                                         (mapcar (lambda (b) (symbol-name (car b))) lispm-backends)))
                                nil t nil nil (and current (symbol-name current))))
         (backend (if (string-empty-p name)
                      (user-error "%s" (lispm-tr "No backend chosen"))
                    (intern name)))
         (default (plist-get (lispm-backend backend) :model))
         (model (read-string (format-prompt (lispm-tr "Model") default) nil nil default)))
    (cons backend (and model (not (string-empty-p model)) model))))

;;;; Triggers

(defun lispm--runtime-buffer-p ()
  "Non-nil if the current buffer holds one of the runtime's own files."
  (and buffer-file-name
       (file-in-directory-p buffer-file-name lispm-directory)))

(cl-defun lispm-watch (agent hook &key when prompt (debounce 1.0) name)
  "Wake AGENT whenever HOOK runs and WHEN approves.
WHEN and PROMPT are called with the hook's arguments (in the buffer
current when it ran); PROMPT, a function or a string, gives the
input AGENT receives as an event.  Runs within DEBOUNCE seconds are
collapsed into one.  Hook runs the agent caused itself are ignored,
as are saves of the runtime's own files.  NAME (default: HOOK's name)
identifies the trigger for `lispm-unwatch'.

  (lispm-watch \\='coder \\='compilation-finish-functions
    :when (lambda (_buffer status) (string-match-p \"abnormally\" status))
    :prompt \"The compilation failed. Read *compilation* and fix the cause.\")"
  (let* ((agent (lispm-get-agent agent))
         (name (or name (symbol-name hook)))
         (timer nil)
         (function
          (lambda (&rest args)
            (unless (or lispm-inhibit-triggers
                        (eq lispm-current-agent agent)
                        (lispm--runtime-buffer-p))
              (with-demoted-errors "lispm-watch: %S"
                (when (or (null when) (apply when args))
                  (let ((text (if (functionp prompt) (apply prompt args) prompt)))
                    (when timer (cancel-timer timer))
                    (setq timer (run-at-time debounce nil
                                             (lambda ()
                                               (setq timer nil)
                                               (lispm-tell agent text :event name)))))))))))
    (lispm-unwatch agent name)
    (add-hook hook function)
    (push (list :name name :hook hook :function function) (lispm-agent-triggers agent))
    name))

(defun lispm-every (agent seconds prompt &optional name)
  "Wake AGENT every SECONDS with PROMPT (a string or a function).
NAME identifies the trigger for `lispm-unwatch'."
  (let* ((agent (lispm-get-agent agent))
         (name (or name (format "every %ds" seconds))))
    (lispm-unwatch agent name)
    (push (list :name name
                :timer (run-at-time seconds seconds
                                    (lambda ()
                                      (lispm-tell agent (if (functionp prompt) (funcall prompt) prompt)
                                                  :event name))))
          (lispm-agent-triggers agent))
    name))

(defun lispm-unwatch (agent &optional name)
  "Remove AGENT's trigger NAME, or all of them."
  (let ((agent (lispm-get-agent agent)))
    (setf (lispm-agent-triggers agent)
          (seq-remove (lambda (trigger)
                        (when (or (null name) (equal name (plist-get trigger :name)))
                          (when (plist-get trigger :hook)
                            (remove-hook (plist-get trigger :hook) (plist-get trigger :function)))
                          (when (plist-get trigger :timer)
                            (cancel-timer (plist-get trigger :timer)))
                          t))
                      (lispm-agent-triggers agent)))))

;;;; Handing over: compaction

(defconst lispm--save-memory-prompt
  "Your context is long and will be compacted: most of this conversation will be replaced by a summary. Before that, save what should outlive it: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Do not summarize the conversation now. Reply in one sentence when done."
  "Asked of an agent before its session is handed over.")

(defconst lispm--resumed-memory-prompt
  "Your context filled up in the middle of this turn and was compacted: the summary above is all that is left of the earlier conversation. Save what should outlive it, if you have not already: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Then go on with the turn."
  "Told an agent whose session was handed over in the middle of a turn.")

(defconst lispm--summary-prompt
  "Write a handoff summary of this conversation for yourself: a fresh session will hold this summary in place of the conversation and continue from it. Cover the user's goals and requests, the decisions made and why, the current state of the work (files changed, what was verified), and the open questions and next steps. Be specific: names, paths, values. Leave out what is already saved in memory or tasks. Reply with the summary only."
  "Asked of an agent to summarize its session for the continuation.")

(defconst lispm--summary-kept-prompt
  "The latest turns, from the input that begins \"%s\", will follow the summary as they are: summarize what comes before them, and say little about them."
  "Added to `lispm--summary-prompt' when a handover keeps the latest turns.")

(defun lispm--session-bytes (agent)
  "Size in bytes of AGENT's session."
  (let ((buffer (lispm-agent-buffer agent)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (1- (position-bytes (point-max)))))
      0)))

(defun lispm--estimated-tokens (agent)
  "Size in tokens of AGENT's session, as its next request would carry it.
That is the size the latest response reported, plus a token for every
three bytes written since; before any response, the whole session's
bytes by three.  Bytes, not characters: a token is about four
characters of English text, but about one character of Chinese, which
takes three bytes."
  (let ((bytes (lispm--session-bytes agent))
        (known (lispm-agent-context agent)))
    (if known
        (+ (car known) (/ (max 0 (- bytes (cdr known))) 3))
      (/ bytes 3))))

(defun lispm--clear-results (agent)
  "Clear the output of AGENT's older tool calls, if that frees enough.
All results but the latest `lispm-compact-keep-results' are tagged
:cleared:, provided that frees `lispm-compact-clear-at-least' tokens.
Return the number of tokens freed, or nil."
  (when lispm-compact-keep-results
    (let* ((old (seq-remove (lambda (result) (nth 2 result))
                            (butlast (lispm--in-session agent (lispm-transcript-results))
                                     lispm-compact-keep-results)))
           (freed (/ (apply #'+ (mapcar #'cadr old)) 3)))
      (when (and old (>= freed lispm-compact-clear-at-least))
        (let ((tokens (- (lispm--estimated-tokens agent) freed)))
          (lispm--in-session agent
            ;; The latest first: tags move what comes after them.
            (dolist (result (reverse old))
              (lispm-transcript-tag (car result) "cleared")))
          (setf (lispm-agent-context agent) (cons tokens (lispm--session-bytes agent)))
          freed)))))

(defun lispm--kept-turns (agent)
  "The latest turns of AGENT's session that a handover keeps, or nil.
Return (TEXT . INPUT): the text of their sections, conditions left
out, and the beginning of the input the first of them handled.  See
`lispm-compact-keep-turns'."
  (lispm--in-session agent
    (let* ((end (or (lispm--input-heading-p) (point-max)))
           (sections (seq-filter (lambda (section) (< (plist-get section :start) end))
                                 (lispm-transcript-sections)))
           (inputs (seq-filter (lambda (section) (memq (plist-get section :kind) '(user message event)))
                               sections))
           ;; A quarter of the threshold, in bytes; with none, of its default.
           (limit (* 3 (/ (or lispm-compact-threshold
                              (eval (car (get 'lispm-compact-threshold 'standard-value)) t))
                          4))))
      ;; The turn that saved memory for this handover goes with the rest.
      (when-let* ((last (car (last inputs)))
                  ((eq (plist-get last :kind) 'event))
                  ((equal (plist-get last :arg) "compaction")))
        (setq end (plist-get last :start)
              inputs (butlast inputs)))
      (cl-loop for n from (min lispm-compact-keep-turns (1- (length inputs))) downto 1
               for first = (nth (- (length inputs) n) inputs)
               for text = (mapconcat
                           (lambda (section)
                             (if (or (< (plist-get section :start) (plist-get first :start))
                                     (>= (plist-get section :start) end)
                                     (eq (plist-get section :kind) 'condition))
                                 ""
                               (buffer-substring-no-properties (plist-get section :start)
                                                               (min (plist-get section :end) end))))
                           sections "")
               when (<= (string-bytes text) limit)
               return (cons (if (string-suffix-p "\n" text) text (concat text "\n"))
                            (lispm--oneline (lispm-transcript-section-text first) 60))))))

(defun lispm--has-history-p (agent)
  "Non-nil if AGENT's session holds a response: something to hand over."
  (with-current-buffer (lispm-agent-buffer agent)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (lispm-transcript-heading-regexp 'assistant) nil t))))

(defun lispm--take-input (agent)
  "Remove the text the user typed under AGENT's last \"* User\" and return it."
  (lispm--in-session agent
    (when (re-search-backward (lispm-transcript-heading-regexp 'user) nil t)
      (let ((text (string-trim (buffer-substring-no-properties
                                (min (point-max) (1+ (line-end-position))) (point-max)))))
        (delete-region (point) (point-max))
        text))))

(defun lispm-compact (&optional agent)
  "Hand AGENT's session over to a continuation holding a summary of it.
Interactively, the agent of the current session.  Memory saved for a
handover since the last one is not saved again, and a summary being
written at idle is taken (see `lispm-compact-idle')."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (when (or (lispm-agent-busy-p agent) (lispm-agent-compacting agent))
      (user-error "%s" (lispm-tr "%s is busy" (lispm-agent-name agent))))
    (setf (lispm-agent-compacting agent) t)
    (cond
     ((lispm--adopt-idle-handover agent))
     ((and lispm-compact-save-memory (not (lispm--memory-saved-p agent)))
      (lispm--start-turn agent (list :event "compaction" :text lispm--save-memory-prompt
                                     :no-compact t
                                     :callback (lambda (_)
                                                 (when (lispm-agent-compacting agent)
                                                   (lispm--note-memory-saved agent))
                                                 (lispm--summarize agent)))))
     (t (lispm--summarize agent)))))

(defun lispm--summary-request (agent)
  "The request for a summary of AGENT's session, and the turns kept after it.
Return (REQUEST . KEPT), KEPT being what `lispm--kept-turns' returns."
  (let* ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse)))
         (kept (lispm--kept-turns agent))
         (prompt (if kept
                     (concat lispm--summary-prompt " "
                             (format lispm--summary-kept-prompt (cdr kept)))
                   lispm--summary-prompt))
         (messages (lispm-transcript--normalize
                    (append (plist-get parsed :messages)
                            (list `((role . "user")
                                    (content . [((type . "text")
                                                 (text . ,prompt))])))))))
    (cons (lispm-api-request (lispm-agent-backend* agent) (lispm-agent-model* agent)
                             (lispm-agent-effort* agent) (plist-get parsed :system)
                             nil messages)
          kept)))

(defun lispm--summarize (agent &optional resume)
  "Ask the model for a summary of AGENT's session, then continue from it.
The continuation holds the summary and the latest turns, which the
summary need not cover (see `lispm-compact-keep-turns').  RESUME
non-nil goes on with the turn in the continuation."
  (when (lispm-agent-compacting agent)
    (lispm-set-state agent 'compacting "writing a summary")
    (condition-case err
        (let* ((summary-request (lispm--summary-request agent))
               (request (car summary-request))
               (kept (cdr summary-request))
               (proc (funcall lispm-api-function (lispm-agent-backend* agent)
                              (car request) (cdr request)
                              :on-done (lambda (message)
                                         (lispm--continue agent (lispm--reply-text
                                                                 (append (alist-get 'content message) nil))
                                                          resume (car kept)))
                              :on-error (lambda (err)
                                          (lispm--compaction-failed
                                           agent (format "%s: %s" (plist-get err :type)
                                                         (plist-get err :message))
                                           resume)))))
          (when (processp proc) (process-put proc 'lispm-agent agent)))
      (error (lispm--compaction-failed agent (error-message-string err) resume)))))

(defun lispm--continue (agent summary &optional resume kept)
  "Carry AGENT's conversation on in a new session file holding SUMMARY.
KEPT, if non-nil, is the text of the latest turns, which follow it.
RESUME non-nil goes on with the turn there."
  (when (lispm-agent-compacting agent)
    (if (string-blank-p summary)
        (lispm--compaction-failed agent "the summary came back empty" resume)
      (let* ((old (lispm-agent-buffer agent))
             (old-file (buffer-file-name old))
             (file (lispm--create-session-file agent old-file summary kept)))
        (lispm--in-session agent
          (let ((input (lispm--input-heading-p)))
            (when input (delete-region input (point-max))))
          (lispm--newline)
          (insert (lispm-transcript-section
                   (lispm-transcript-title 'note)
                   (format "Continued in [[file:%s]]." (abbreviate-file-name file)))))
        (lispm--save-session agent)
        (let ((new (lispm--visit-session agent file)))
          (dolist (window (get-buffer-window-list old nil t))
            (set-window-buffer window new)
            (set-window-point window (with-current-buffer new (point-max))))
          (with-current-buffer old (setq lispm-session-agent nil))
          (kill-buffer old))
        ;; Sizes reported for the old session say nothing of this one.
        (setf (lispm-agent-context agent) nil)
        (lispm--save-session agent)
        (when (fboundp 'lispm-git-commit)
          (lispm-git-commit agent (format "%s: hand over to %s" (lispm-agent-name agent)
                                          (file-name-nondirectory file))))
        (with-demoted-errors "LispM: after a handover: %S"
          (run-hook-with-args 'lispm-handover-functions agent summary old-file))
        (setf (lispm-agent-compacting agent) nil)
        (if resume
            (progn
              ;; The turn goes on from the summary, not from an input,
              ;; after saving what should outlive the session, as a
              ;; handover between turns does before it.
              (lispm--in-session agent
                (when-let* ((input (lispm--input-heading-p)))
                  (delete-region input (point-max))))
              (if lispm-compact-save-memory
                  (lispm--tell-for-user agent lispm--resumed-memory-prompt)
                (lispm--request agent)))
          (lispm-set-state agent 'idle)
          (lispm--drain-inbox agent))))))

(defun lispm--compaction-failed (agent reason &optional resume)
  "Stop at a condition: AGENT's session could not be handed over, for REASON.
RESUME non-nil if a turn waits for the handover in its middle."
  (when (lispm-agent-compacting agent)
    (setf (lispm-agent-compacting agent) nil)
    (unless resume
      ;; Between turns: attended if the input waiting for it is the user's.
      (let ((entry (car (lispm-agent-inbox agent))))
        (setf (lispm-agent-attended agent)
              (not (or (plist-get entry :from) (plist-get entry :event))))))
    (lispm-signal agent 'compaction-failed (format "Compaction failed: %s" reason)
                  :resume resume)))

;;;; Handing over at idle
;;
;; A handover at idle costs what one when a turn starts does: a turn
;; saving memory, and a request carrying the whole session.  It is
;; tried once per session: given up, as when input comes while the
;; summary is written, it is left to the next turn's start, which then
;; takes the memory saved, or the summary on its way, as its own.

(defvar lispm--memory-saved (make-hash-table :test #'eq :weakness 'key)
  "Agents to the session buffer they saved memory in for a handover.")

(defvar lispm--idle-given-up (make-hash-table :test #'eq :weakness 'key)
  "Agents to the session buffer whose handover at idle was given up.")

(defun lispm--near-threshold-p (agent)
  "Non-nil if AGENT's session is past `lispm-compact-idle' of the threshold."
  (and lispm-compact-threshold lispm-compact-idle
       (> (lispm--estimated-tokens agent) (* lispm-compact-idle lispm-compact-threshold))))

(defun lispm--free-p (agent)
  "Non-nil if AGENT is idle with nothing to do, and nothing written to send.
Text the user is writing under the last \"* User\" heading stays theirs."
  (and (memq agent (lispm-agents))
       (eq (lispm-agent-state agent) 'idle)
       (not (lispm-agent-compacting agent))
       (not (lispm-agent-condition agent))
       (null (lispm-agent-inbox agent))
       (buffer-live-p (lispm-agent-buffer agent))
       (lispm--in-session agent (lispm--input-heading-p))))

(defun lispm--note-memory-saved (agent)
  "Record that AGENT saved memory for the handover of its session."
  (puthash agent (lispm-agent-buffer agent) lispm--memory-saved))

(defun lispm--memory-saved-p (agent)
  "Non-nil if AGENT saved memory for a handover since the last handover.
A handover makes a new session buffer."
  (eq (gethash agent lispm--memory-saved) (lispm-agent-buffer agent)))

(defun lispm--idle-handover-p (agent)
  "Non-nil if AGENT's session may still be handed over at idle.
It may not once that was given up, until the session is handed over."
  (not (eq (gethash agent lispm--idle-given-up) (lispm-agent-buffer agent))))

(defun lispm--compact-when-idle (agent _reply)
  "Once AGENT's turn has ended, compact its session at idle if it is large.
See `lispm-compact-idle'."
  (when (and (lispm--near-threshold-p agent)
             (lispm--idle-handover-p agent)
             (not (gethash agent lispm--idle-handovers)))
    (lispm-when-idle (cons 'compact (lispm-agent-id agent)) #'lispm--compact-idle agent)))

(add-hook 'lispm-turn-end-functions #'lispm--compact-when-idle)

(defun lispm--compact-idle (agent)
  "Make room in AGENT's session, Emacs being idle, if it is still large.
Older tool output is cleared; if that is not enough, memory is saved
and a summary written in the background (see `lispm-compact-idle')."
  (when (and (lispm--free-p agent)
             (lispm--idle-handover-p agent)
             (lispm--near-threshold-p agent))
    (lispm--clear-results agent)
    (when (and (lispm--near-threshold-p agent) (lispm--has-history-p agent))
      (if (and lispm-compact-save-memory (not (lispm--memory-saved-p agent)))
          (progn
            (puthash agent 'saving lispm--idle-handovers)
            (lispm--start-turn
             agent (list :event "compaction" :text lispm--save-memory-prompt
                         :no-compact t :idle t
                         :callback (lambda (_)
                                     ;; Unless the turn was stopped.
                                     (when (eq (gethash agent lispm--idle-handovers) 'saving)
                                       (remhash agent lispm--idle-handovers)
                                       (lispm--note-memory-saved agent)
                                       (if (lispm--free-p agent)
                                           (lispm--summarize-idle agent)
                                         ;; Input came meanwhile: it goes first.
                                         (lispm--give-up-idle-handover agent)))))))
        (lispm--summarize-idle agent)))))

(defun lispm--summarize-idle (agent)
  "Have a summary of AGENT's session written while the agent stays free.
When it comes, the session is handed over to a continuation, unless
it changed meanwhile or the agent is busy: then the handover at idle
is given up.  A handover the next turn waits for takes it (see
`lispm--adopt-idle-handover')."
  (when (and (lispm--free-p agent) (not (gethash agent lispm--idle-handovers)))
    (condition-case err
        (pcase-let* ((`(,request . ,kept) (lispm--summary-request agent))
                     (buffer (lispm-agent-buffer agent))
                     ;; (BUFFER TICK PROCESS ADOPTED), see `lispm--idle-handovers'.
                     (token (list buffer (buffer-chars-modified-tick buffer) nil nil))
                     (current (lambda () (eq (gethash agent lispm--idle-handovers) token))))
          (puthash agent token lispm--idle-handovers)
          (let ((proc (funcall
                       lispm-api-function (lispm-agent-backend* agent) (car request) (cdr request)
                       :on-done
                       (lambda (message)
                         (when (funcall current)
                           (remhash agent lispm--idle-handovers)
                           (let ((summary (lispm--reply-text (append (alist-get 'content message) nil))))
                             (cond
                              ((nth 3 token)
                               (if (string-blank-p summary)
                                   (lispm--compaction-failed agent "the summary came back empty")
                                 (lispm--continue agent summary nil (car kept))))
                              ((and (not (string-blank-p summary))
                                    (lispm--free-p agent)
                                    (eq (lispm-agent-buffer agent) buffer)
                                    (= (buffer-chars-modified-tick buffer) (nth 1 token)))
                               (setf (lispm-agent-compacting agent) t)
                               (condition-case err
                                   (lispm--continue agent summary nil (car kept))
                                 (error
                                  (setf (lispm-agent-compacting agent) nil)
                                  (lispm-set-state agent 'idle)
                                  (lispm--give-up-idle-handover agent)
                                  (message "%s" (lispm-tr "LispM: could not compact %s: %s"
                                                          (lispm-agent-name agent)
                                                          (error-message-string err))))))
                              (t (lispm--give-up-idle-handover agent))))))
                       :on-error
                       (lambda (err)
                         (when (funcall current)
                           (remhash agent lispm--idle-handovers)
                           (if (nth 3 token)
                               (lispm--compaction-failed
                                agent (format "%s: %s" (plist-get err :type) (plist-get err :message)))
                             (lispm--give-up-idle-handover agent)))))))
            (when (and (processp proc) (funcall current))
              (process-put proc 'lispm-agent agent)
              (setf (nth 2 token) proc))))
      (error (lispm--give-up-idle-handover agent)
             (message "%s" (lispm-tr "LispM: could not compact %s: %s"
                                     (lispm-agent-name agent) (error-message-string err)))))))

(defun lispm--adopt-idle-handover (agent)
  "Have the handover AGENT now waits for take the summary written at idle.
Return non-nil if one is being written: the handover goes on when it
comes, as one a turn waits for."
  (when-let* ((token (gethash agent lispm--idle-handovers))
              ((consp token)))
    (setf (nth 3 token) t)
    (lispm-set-state agent 'compacting "writing a summary")
    t))

(defun lispm--cancel-idle-handover (agent)
  "Stop handing AGENT's session over at idle, and the summary being written."
  (when-let* ((token (gethash agent lispm--idle-handovers)))
    (remhash agent lispm--idle-handovers)
    (when-let* ((proc (and (consp token) (nth 2 token))))
      (when (process-live-p proc)
        (process-put proc 'lispm-aborted t)
        (delete-process proc)))))

(defun lispm--give-up-idle-handover (agent)
  "Give up handing AGENT's session over at idle, until it is handed over.
That is left to the start of a turn, past `lispm-compact-threshold'."
  (lispm--cancel-idle-handover agent)
  (when (buffer-live-p (lispm-agent-buffer agent))
    (puthash agent (lispm-agent-buffer agent) lispm--idle-given-up)))

(defun lispm--forget-idle-work (agent)
  "Drop the background work waiting for AGENT, whose session ends."
  (lispm-cancel-idle (cons 'compact (lispm-agent-id agent)))
  (lispm--cancel-idle-handover agent))

(add-hook 'lispm-session-end-functions #'lispm--forget-idle-work)

;;;; Journal: audit and rollback
;;
;; A turn's changes are known two ways (see lispm-journal.el): the
;; text buffers had before a tool first changed them, in memory, and,
;; for a project in git, snapshots of its work tree as the turn began
;; and ended, which hold what shell commands and Lisp changed too and
;; outlive Emacs.  Files are taken from the snapshots where there are
;; any, buffers that visit no file, and edits not saved, from memory.

(defun lispm--journal-turns (agent)
  "The turns of AGENT with changes on record, newest first, as plists.
Each has :label, :journal, the snapshots of buffers taken in memory,
and :record, the turn's snapshots in git (`lispm-journal-records'),
either of which may be nil, and :turn, its number, for a turn of this
Emacs session."
  (let* ((records (lispm-journal-records agent))
         (numbers (sort (delete-dups
                         (append (mapcar #'car (seq-filter #'cdr (lispm-agent-journal agent)))
                                 (delq nil (mapcar (lambda (r) (plist-get r :turn)) records))))
                        #'>)))
    (append
     (mapcar (lambda (turn)
               (list :turn turn :label (format "turn %d" turn)
                     :journal (cdr (assq turn (lispm-agent-journal agent)))
                     :record (seq-find (lambda (r) (eql (plist-get r :turn) turn)) records)))
             numbers)
     (mapcar (lambda (record)
               (list :record record
                     :label (format "%s, %s" (plist-get record :subject)
                                    (replace-regexp-in-string
                                     "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\).*"
                                     "\\1-\\2-\\3 \\4:\\5" (plist-get record :time)))))
             (seq-remove (lambda (r) (plist-get r :turn)) records)))))

(defun lispm--turn-label (turn)
  "TURN, one of `lispm--journal-turns', as the user is shown it: its
:label, said in the language spoken."
  (if-let* ((number (plist-get turn :turn)))
      (lispm-tr "turn %d" number)
    (plist-get turn :label)))

(defun lispm--journal-turn (agent &optional turn)
  "AGENT's TURN as `lispm--journal-turns' describes it.
TURN is a turn's number in this Emacs session, one of those plists, or
nil for the latest turn with changes."
  (cond ((consp turn) turn)
        ((seq-find (lambda (entry) (or (null turn) (eql (plist-get entry :turn) turn)))
                   (lispm--journal-turns agent)))
        (t (user-error "%s" (if turn
                                (lispm-tr "%s made no edits in turn %d" (lispm-agent-name agent) turn)
                              (lispm-tr "%s made no edits" (lispm-agent-name agent)))))))

(defun lispm--snapshot-now (snapshot)
  "Current text of the buffer or file in SNAPSHOT.
A file's is its buffer's, unless that is out of date with it (see
`lispm--file-text')."
  (let ((buffer (car snapshot))
        (file (plist-get (cdr snapshot) :file)))
    (cond (file (lispm--file-text file))
          ((buffer-live-p buffer) (lispm-tools--buffer-text buffer))
          (t ""))))

(defun lispm--journal-parts (turn)
  "What TURN changed, as (FILES . SNAPSHOTS).
FILES are the (STATUS . FILE) its snapshots in git show, SNAPSHOTS the
buffers it changed whose text, to revert to, is in memory: those that
visit no file, or a file the snapshots do not show changed."
  (let* ((files (and (plist-get turn :record) (lispm-journal-changes (plist-get turn :record))))
         (snapshots (seq-remove (lambda (s)
                                  (when-let* ((file (plist-get (cdr s) :file)))
                                    (assoc file (mapcar (lambda (c) (cons (cdr c) c)) files))))
                                (plist-get turn :journal))))
    (cons files snapshots)))

(defun lispm--read-turn (agent)
  "With a prefix argument, read one of AGENT's turns with changes."
  (when current-prefix-arg
    (let* ((turns (lispm--journal-turns agent))
           (labels (mapcar (lambda (turn) (cons (lispm--turn-label turn) turn)) turns)))
      (unless turns (user-error "%s" (lispm-tr "%s made no edits" (lispm-agent-name agent))))
      (cdr (assoc (completing-read (lispm-tr "Turn: ") labels nil t nil nil (caar labels)) labels)))))

(defun lispm-turn-diff (agent &optional turn)
  "Show every change AGENT made in TURN (default: its latest edits).
With a prefix argument, choose the turn."
  (interactive (let ((agent (lispm--buffer-agent))) (list agent (lispm--read-turn agent))))
  (let* ((turn (lispm--journal-turn agent turn))
         (parts (lispm--journal-parts turn))
         ;; Where the diff's file names are relative to.
         (directory (or (plist-get (plist-get turn :record) :top) (lispm-agent-root agent)))
         (text (let ((default-directory directory))
                 (concat (lispm-tr "Changes by %s in %s" (lispm-agent-name agent) (lispm--turn-label turn)) "\n\n"
                         (if (car parts) (concat (lispm-journal-diff (plist-get turn :record)) "\n") "")
                         (mapconcat (lambda (s)
                                      (concat (lispm--diff (plist-get (cdr s) :text) (lispm--snapshot-now s)
                                                           (or (plist-get (cdr s) :file)
                                                               (plist-get (cdr s) :name)))
                                              "\n"))
                                    (cdr parts) "")))))
    (with-current-buffer (get-buffer-create "*lispm-diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (diff-mode)
      (setq default-directory directory)
      (goto-char (point-min)))
    (display-buffer "*lispm-diff*")))

(defun lispm-revert-turn (agent &optional turn)
  "Undo every change AGENT made to buffers and files in TURN.
Default: its latest turn with edits; with a prefix argument, choose
the turn.  In a project in git, that is every change its files went
through in the turn, made by tools, shell commands or Lisp, since the
turn's snapshots show them, even from before Emacs restarted."
  (interactive (let ((agent (lispm--buffer-agent))) (list agent (lispm--read-turn agent))))
  (let* ((turn (lispm--journal-turn agent turn))
         (parts (lispm--journal-parts turn))
         (record (plist-get turn :record))
         (names (append (mapcar (lambda (c) (lispm-tools--label (cdr c))) (car parts))
                        (mapcar (lambda (s) (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                                (cdr parts)))))
    ;; All or nothing: a buffer with unsaved changes whose file changed
    ;; on disk too is for the user to settle first.
    (when-let* ((conflicts (seq-filter (lambda (buffer)
                                         (memq (lispm-tools--sync buffer) '(conflict held)))
                                       (delq nil (append (mapcar (lambda (c) (find-buffer-visiting (cdr c)))
                                                                 (car parts))
                                                         (seq-filter #'buffer-live-p
                                                                     (mapcar #'car (cdr parts))))))))
      (user-error "%s" (lispm-tr "Nothing reverted: %s changed on disk, and in Emacs too; save or revert first"
                                 (mapconcat #'buffer-name conflicts ", "))))
    (unless (or (not (called-interactively-p 'any))
                (yes-or-no-p (lispm-tr-n "Revert %d file or buffer %s changed in %s? "
                                         "Revert %d files and buffers %s changed in %s? "
                                         (length names)
                                         (length names) (lispm-agent-name agent) (lispm--turn-label turn))))
      (user-error "%s" (lispm-tr "Cancelled")))
    (let ((lispm-current-agent nil))
      (pcase-dolist (`(,status . ,file) (car parts))
        (let ((buffer (find-buffer-visiting file)))
          (if (equal status "A")
              (progn
                (when buffer
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (kill-buffer buffer))
                (when (and (file-exists-p file)
                           (or (not (called-interactively-p 'any))
                               (yes-or-no-p (lispm-tr "Delete %s, which %s created? "
                                                      file (lispm-agent-name agent)))))
                  (delete-file file)))
            (if buffer
                (lispm-tools--set-text buffer (lispm-journal-text-before record file))
              (lispm-journal-restore record file)))))
      (dolist (s (cdr parts))
        (let* ((buffer (car s))
               (props (cdr s))
               (file (plist-get props :file)))
          (cond
           ((and file (not (plist-get props :existed)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))
            (when (and (file-exists-p file)
                       (or (not (called-interactively-p 'any))
                           (yes-or-no-p (lispm-tr "Delete %s, which %s created? "
                                                  file (lispm-agent-name agent)))))
              (delete-file file)))
           ((buffer-live-p buffer) (lispm-tools--set-text buffer (plist-get props :text)))
           (file (let ((coding-system-for-write 'utf-8))
                   (write-region (plist-get props :text) nil file nil 'silent)))))))
    (when-let* ((number (plist-get turn :turn)))
      (setf (lispm-agent-journal agent) (assq-delete-all number (lispm-agent-journal agent))))
    (when record
      (lispm-journal-forget record))
    (lispm--record-event
     agent "revert"
     (format "The user reverted the changes you made in %s to: %s."
             (plist-get turn :label) (string-join names ", ")))
    (message "%s" (lispm-tr "Reverted %s of %s" (lispm--turn-label turn) (lispm-agent-name agent)))))

(provide 'lispm-runtime)
;;; lispm-runtime.el ends here
