;;; lispm-session.el --- Agent sessions, transcripts, system prompt, and lifecycle hooks  -*- lexical-binding: t; -*-

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

;; Session buffer mode, transcript file creation and discovery,
;; system prompt construction, recorded session metadata, and runtime
;; upward dispatch hooks.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'project)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-inbox)
(require 'lispm-journal)

(defcustom lispm-save-sessions t
  "Whether to save session files at the end of every turn."
  :type 'boolean
  :group 'lispm)

(defconst lispm-session-default-compact-threshold 120000
  "Default size, in tokens, for `lispm-compact-threshold'.")

(defcustom lispm-compact-threshold lispm-session-default-compact-threshold
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

(defcustom lispm-compact-keep-results 30
  "How many of a session's latest tool results are never cleared.
When a session grows past `lispm-compact-threshold', the output of
older calls is cleared before the session is handed over: requests
carry a line saying so in its place, and the result's heading is
tagged :cleared: (remove the tag to send it again).  nil never clears."
  :type '(choice (const :tag "Never clear" nil) integer)
  :group 'lispm)

(defcustom lispm-compact-bulky-result-bytes 12000
  "Byte size from which an older tool result counts as bulky during clearing.
When clearing bulky older results alone frees at least
`lispm-compact-clear-at-least' tokens, smaller older results such as
modest file reads stay in context rather than being re-read repeatedly."
  :type 'integer
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

(defconst lispm-default-compact-idle 0.75
  "Default fraction for `lispm-compact-idle'.")

(defcustom lispm-compact-idle lispm-default-compact-idle
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

(defvar lispm-session-idle-handovers (make-hash-table :test #'eq :weakness 'key)
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

(defvar-local lispm-session-turn-start nil
  "The turn begun in this buffer and its input heading: (TURN . MARKER).
The marker records the exact boundary when a turn starts, so later
events or a draft for the next turn cannot move the work inspector's
boundary.  A resumed turn has a nil marker: its exact boundary was
not recorded, and must not be guessed from intervening events.")

(defun lispm-turn-start (&optional buffer)
  "Return BUFFER's current turn-start cons `(TURN . MARKER)', or nil."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (buffer-local-value 'lispm-session-turn-start buf))))

(defvar-local lispm-session-agent-tools nil
  "Schemas of the tools the session in this buffer offers the model.
See `lispm-session-tools'.")
(put 'lispm-session-agent-tools 'permanent-local t)

(defvar-local lispm-session-turn-context nil
  "Live scenario context captured for the current turn: (TURN . TEXT), or nil.")


(defvar lispm-runtime-live-delete-function nil
  "Hook called with AGENT to clear live streaming UI overlays before saving.")

(defvar lispm-runtime-unwatch-function nil
  "Hook called with AGENT and optional NAME to remove triggers on kill.")

(defvar lispm-runtime-signal-function nil
  "Hook called with (AGENT TYPE MESSAGE . PROPS) to raise a runtime condition.")

(defvar lispm-runtime-stop-call-function nil
  "Hook called with (AGENT ID TEXT IMAGES) when a call stops on error.")

(defvar lispm-runtime-ask-approval-function nil
  "Hook called with (AGENT ID) to ask the user to approve a pending call.")

(defvar lispm-runtime-open-calls-function nil
  "Hook called with AGENT returning pending calls awaiting answer.")

(defvar lispm-runtime-stop-function nil
  "Hook called with (AGENT NOTE &optional KEEP-INBOX) to stop an active turn.")

(defvar lispm-runtime-before-turn-function nil
  "Hook called with (AGENT ENTRY); non-nil means compaction handled start.")

(defvar lispm-runtime-trim-request-function nil
  "Hook called with AGENT before sending to clear bulky results if needed.")

(defvar lispm-runtime-session-bytes-function nil
  "Hook called with AGENT returning current session byte size.")

(defvar lispm-runtime-give-up-idle-handover-function nil
  "Hook called with AGENT to cancel an active idle handover.")

(defvar lispm-runtime-take-input-function nil
  "Hook called with AGENT to consume trailing user input from transcript.")

(defvar lispm-runtime-summarize-function nil
  "Hook called with (AGENT &optional RESUME) to start compaction summary.")

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
               " name files there. Emacs Lisp (including run_elisp) still runs on this machine.\n")
     "")
   "- This conversation is an org-mode buffer the user can read, fold, edit and"
   " annotate while you work. What it contains is what you remember of this session.\n"
   "- You have self-introspection over this live Emacs session and your own UI"
   " scenario: when a turn includes a `[Current LispM scenario]' block, it tells"
   " you which surface the user is talking to you from and the current window and"
   " buffer state. Never guess which UI surface the user is using when the"
   " scenario block or live Emacs state tells you; use eval_elisp,"
   " describe_symbol, apropos, runtime_status or skills to inspect live state"
   " whenever needed.\n"
   "- Calls that change files or run code may wait for the user's approval. Such"
   " calls take a `why` argument: one plain sentence the user reads when deciding."
   " If a call is denied, adapt; do not repeat it unchanged.\n"
   "- Files can be open in Emacs with unsaved changes. read_file and the edit tools"
   " work on the live buffer, so you see what the user sees; every edit you make is"
   " one undo step for them.\n"
   "- What this system and its apps can do is written in skills, their manuals, each"
   " with tools to do it; search_tools finds them, including tools already available."
   " When a request needs something you do not have, search before you say you"
   " cannot do it.\n"
   "- Input beginning with \"[Message from agent X]\" comes from another agent, and"
   " \"[Event: ...]\" from a hook or timer the user set up. Treat them as requests"
   " from colleagues: anything consequential still needs the user's intent.\n"
   "- Input beginning with \"[From LispM, for the user]\" passes on a choice the user"
   " made, such as to go on with a response that was cut short: follow it as theirs.\n\n"
   "# Working style\n"
   "- Before your first tool call in a turn, say in one sentence what you are about"
   " to do. Batch independent tool calls in a single response. End the turn with a"
   " short summary of what changed and how it was verified.\n"
   "- Read before you edit; prefer small, verifiable steps. Edit existing files"
   " rather than creating new ones unless required. Put scratch test binaries and"
   " one-off verification scripts in /tmp, and remove any temporary files,"
   " scratch build outputs or scratch tests before finishing (never delete"
   " required deliverables or in-place compiled artifacts such as .so extensions,"
   " .vo proofs or target binaries).\n"
   "- Prefer LispM's dedicated tools. For standalone file discovery, JSON/JSONL"
   " parsing, text filtering and computation, use run_elisp: LispM Kernel starts"
   " a fresh Emacs process and returns its output. The worker has no live editor"
   " state or agent tools. A missing kernel is an error, not a reason to move"
   " heavy work into this Emacs. Follow the user's explicit choice of language.\n"
   "- Use run_lisp for short compositions of agent tools and eval_elisp when you"
   " need the live editor's buffers, variables or loaded packages. Both run here"
   " and can block the UI; timers and promises do not offload computation. Find"
   " existing functions with apropos and describe_symbol. Keep file and buffer"
   " changes in the edit tools; for multiple edits, issue direct edit_file tool"
   " calls in parallel rather than chaining sequential edit_file calls inside"
   " run_lisp, where a later mismatch leaves earlier edits applied.\n"
   "- Use run_shell for external CLIs, builds, tests, project runtimes or work that"
   " must run on a remote host. Follow the user's explicit choice of language or tool.\n"
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
          (let ((text (string-trim (lispm-file-text file))))
            (unless (or (string-empty-p text) (member text seen))
              (push text seen)
              (push (format "# Project instructions (%s)\n%s" name (lispm-truncate text 20000))
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
  :doc "Keymap of `lispm-session-mode'.")

(defvar lispm--visiting nil
  "The agent whose session file is being visited, while it is.")

(defvar lispm--inhibit-session-opening nil
  "Non-nil while visiting a transcript only to edit its metadata.")

(defun lispm--record-session-opening (agent &optional time)
  "Record AGENT's actual session opening at TIME, defaulting to now.
Merely displaying an existing session or continuing it in another file
does not call this.  No desktop or background reader is needed."
  (unless lispm--inhibit-session-opening
    (let ((time (or time (float-time))))
      (with-current-buffer (lispm-agent-buffer agent)
        (setq lispm-agents-buffer-session-id (lispm-agent-id agent)
              lispm-agents-buffer-launched time))
      (lispm-agents-session-opened 'lispm (lispm-agent-id agent) time))))

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
                                :name (lispm-free-name name) :id id
                                :character (lispm-character-name character))
            (when id (setf (lispm-agent-id agent) id))
            agent)))
       ((lispm-get-role (intern (or (cdr (assoc "role" keywords)) "")) t)
        (let ((agent (lispm-make-agent (intern (cdr (assoc "role" keywords)))
                                       (or (cdr (assoc "root" keywords)) default-directory)
                                       :name (lispm-free-name name) :id id
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
  (let ((ceiling (condition-case nil
                     (car (read-from-string text))
                   ((invalid-read-syntax end-of-file) nil))))
    (if (and (consp ceiling)
             (seq-every-p (lambda (entry)
                            (and (consp entry) (symbolp (car entry))
                                 (memq (cdr entry) lispm-policy-actions)))
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
    (lispm-save-session agent)))

(defun lispm-set-parent (agent parent)
  "Record that AGENT was started by the agent whose session id is PARENT.
AGENT's session records it (#+lispm_parent:), so that lists of the
sessions show AGENT's under PARENT's, see `lispm-sessions'."
  (setf (lispm-agent-parent agent) parent)
  (lispm-emit-change agent '(parent))
  (when (buffer-live-p (lispm-agent-buffer agent))
    (with-current-buffer (lispm-agent-buffer agent)
      (lispm--set-keyword "parent" parent))
    (lispm-save-session agent)))

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
            (setf (lispm-agent-buffer agent) (current-buffer))
            ;; Explicit visits are recorded by `lispm-session-buffer';
            ;; opening a transcript directly adopts and records it here.
            (unless lispm--visiting (lispm--record-session-opening agent)))))
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
  (let ((input (and lispm-session-mode (lispm-session-input-heading-p))))
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
                (propertize (lispm-state-name state)
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
  (let* ((extension (or (car (rassoc (or type "image/png") lispm-transcript-image-types)) "png"))
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

(defun lispm-session-create-file (agent &optional previous summary kept)
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
                                          (plist-get (lispm-session-description previous) :title))))
                    `(("title" . ,(lispm-oneline title 200))))))
             (lispm-transcript-section (lispm-transcript-title 'system) (lispm-system-prompt agent))
             (if summary (lispm-transcript-section (lispm-transcript-title 'summary) summary) "")
             (or kept "")
             (lispm-transcript-heading 'user))
     nil file nil 'silent)
    file))

(defun lispm-session-visit (agent file)
  "Make FILE AGENT's session buffer; return the buffer."
  (let* ((lispm--visiting agent)
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (setq lispm-session-agent agent)
      (lispm-session-mode 1)
      (goto-char (point-min))
      (when (re-search-forward (lispm-transcript-heading-regexp 'system) nil t)
        (lispm-transcript-fold (match-beginning 0)))
      (goto-char (point-max)))
    (setf (lispm-agent-buffer agent) buffer)
    buffer))

(defun lispm-session-files-in (pattern)
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
                (lispm-session-files-in
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
      (let ((opened (float-time))
            (file (and (not new) (lispm-agent-standing agent)
                       (lispm--latest-session-file agent))))
        ;; A standing agent goes on with its session: its id is the session's.
        (when-let* ((id (and file (cdr (assoc "id" (lispm--file-keywords file))))))
          (setf (lispm-agent-id agent) id))
        (unless file
          (when new
            ;; What waits for the agent waits for it in the new session.
            (lispm-inbox-forget agent)
            (setf (lispm-agent-id agent) (lispm-new-id)))
          (setq file (lispm-session-create-file agent)))
        (prog1 (lispm-session-visit agent file)
          (lispm--record-session-opening agent opened)
          (when new (lispm-inbox-save agent)))))))

(defun lispm-project-root (&optional directory)
  "Root of the project around DIRECTORY (default: `default-directory')."
  (let ((default-directory (or directory default-directory)))
    (if-let* ((project (project-current))
              (root (project-root project))
              ((or (null directory) (file-in-directory-p default-directory root))))
        root
      default-directory)))

(defun lispm-start (&optional role root)
  "Start a new agent of ROLE working in ROOT; return it.
ROLE defaults to `lispm-default-role', ROOT to the current project."
  (let ((agent (lispm-make-agent (or role lispm-default-role)
                                 (or root (lispm-project-root)))))
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
    (lispm-save-session agent)
    (if (buffer-live-p buffer)
        (kill-buffer buffer)
      (run-hook-with-args 'lispm-session-end-functions agent)
      (unless (lispm-agent-standing agent)
        (lispm-forget-agent agent)))))

(defun lispm--session-killed ()
  "Stop the agent of a session buffer being killed, and let it go."
  (when-let* ((agent lispm-session-agent))
    (when (eq (lispm-agent-buffer agent) (current-buffer))
      (when (or (lispm-agent-busy-p agent) (lispm-agent-suspended agent))
        (when lispm-runtime-stop-function
          (funcall lispm-runtime-stop-function agent "Stopped: the session buffer was closed.")))
      (setf (lispm-agent-buffer agent) nil)
      (run-hook-with-args 'lispm-session-end-functions agent)
      (unless (lispm-agent-standing agent)
        (when lispm-runtime-unwatch-function
          (funcall lispm-runtime-unwatch-function agent))
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
        (when lispm-runtime-live-delete-function
          (funcall lispm-runtime-live-delete-function agent))
        (lispm-save-session agent)))))

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

(defun lispm-session-started (id)
  "Return the creation time encoded in session ID, or nil when unknown.
`lispm-new-id' records local time as YYYYMMDD-HHMMSS before its suffix.
Unlike a transcript's modification time, this survives later responses
and continuation files."
  (when (and (stringp id)
             (string-match-p "\\`[0-9]\\{8\\}-[0-9]\\{6\\}-[[:xdigit:]]\\{4\\}\\'" id))
    (ignore-errors
      (let ((time (encode-time (string-to-number (substring id 13 15))
                               (string-to-number (substring id 11 13))
                               (string-to-number (substring id 9 11))
                               (string-to-number (substring id 6 8))
                               (string-to-number (substring id 4 6))
                               (string-to-number (substring id 0 4)))))
        (when (equal (format-time-string "%Y%m%d-%H%M%S" time) (substring id 0 15))
          (float-time time))))))

(defun lispm-session-description (file)
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
                         :started (lispm-session-started (cdr (assoc "id" keywords)))
                         :time time
                         :file file
                         :parent (cdr (assoc "parent" keywords))))))))
        (puthash file (cons time description) lispm--session-cache)
        description))))

(defun lispm-sessions ()
  "Every recorded session with some input, most recently opened first.
Each is a plist (:id :agent :character :role :root :title :started :time :file
:parent), :character nil but in a character's, :parent the id of the
session of the agent that started this one, see `lispm-set-parent',
else nil; a session continued in several files is listed once, by its
newest file."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (file (lispm-session-files-in "projects/*/sessions/*.org"))
      (when-let* ((session (lispm-session-description file)))
        (let ((seen (gethash (plist-get session :id) latest)))
          (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
            (puthash (plist-get session :id) session latest)))))
    (cl-flet ((opened (session)
                ;; Native :agent names the individual, not the provider.
                (lispm-agents-session-opened-time
                 (list :agent 'lispm :id (plist-get session :id)
                       :started (plist-get session :started)))))
      (sort (hash-table-values latest)
            (lambda (a b)
              (let ((ta (opened a)) (tb (opened b)))
                (if (= ta tb) (string< (plist-get a :id) (plist-get b :id))
                  (> ta tb))))))))

(defun lispm-session-files (id)
  "Every file recording the session ID."
  (seq-filter (lambda (file) (equal (cdr (assoc "id" (lispm--file-keywords file))) id))
              (lispm-session-files-in (format "projects/*/sessions/%s*.org" id))))

(defun lispm-rename-session (file title)
  "Give the session recorded in FILE the TITLE shown in session lists."
  (let* ((lispm--inhibit-session-opening t)
         (visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file))))
    (with-current-buffer buffer
      (lispm--set-keyword "title" title)
      (let ((inhibit-message t)
            (lispm-inhibit-triggers t)
            (select-safe-coding-system-function nil)
            (coding-system-for-write (or coding-system-for-write 'utf-8-unix)))
        (save-buffer)))
    (unless visiting (kill-buffer buffer))))

(defmacro lispm-in-session (agent &rest body)
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

(defun lispm-session-newline ()
  "Make sure point is at the beginning of a line."
  (unless (bolp) (insert "\n")))

(defun lispm-session-input-heading-p ()
  "Non-nil if the last section of the buffer is an empty \"* User\"."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^\\* " nil t)
         (looking-at (concat (lispm-transcript-heading-regexp 'user) "[ \t\n]*\\'"))
         (point))))

(defun lispm-save-session (agent)
  "Save AGENT's session file."
  (let ((buffer (lispm-agent-buffer agent)))
    (when (and lispm-save-sessions (buffer-live-p buffer) (buffer-file-name buffer))
      (with-current-buffer buffer
        (let ((inhibit-message t)
              (lispm-inhibit-triggers t)
              (select-safe-coding-system-function nil)
              (coding-system-for-write (or coding-system-for-write 'utf-8-unix)))
          (save-buffer))))))

(defun lispm-session-add-section (agent title text)
  "Add a section titled TITLE, holding TEXT, to AGENT's session.
It goes before the input heading a turn ended with, if any."
  (lispm-in-session agent
    (let ((input (lispm-session-input-heading-p)))
      (if input
          (goto-char input)
        (lispm-session-newline))
      (insert (lispm-transcript-section title text)))))

(defun lispm-session-record-event (agent title text)
  "Note TEXT as an event in AGENT's session without starting a turn.
The event's heading is \"Event: TITLE\".  The model reads it with
the next input."
  (lispm-session-add-section agent (lispm-transcript-title 'event title) text))


(defvar lispm-chat-agent)

(defun lispm-buffer-agent ()
  "The agent of the current session or chat buffer."
  (or lispm-session-agent
      (bound-and-true-p lispm-chat-agent)
      (user-error "%s" (lispm-tr "Not in an agent session (or its agent is not defined)"))))


(provide 'lispm-session)

;;; lispm-session.el ends here
