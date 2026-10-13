;;; lispm-runtime.el --- The agent execution loop  -*- lexical-binding: t; -*-

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
;;        when every call has a result, request again
;;     -> response without tool calls: the turn ends, the session is
;;        saved (and committed), turn callbacks and hooks run, and the
;;        next inbox item, if any, starts a new turn.
;;
;; A turn that stops early (a request fails, the step limit is reached)
;; does not end: it waits at a condition, recorded in the transcript,
;; until a restart is chosen, by a handler or by the user, as a call
;; waits for approval.  See "Conditions and restarts" below.

;;; Code:

(require 'org)
(require 'diff-mode)
(require 'project)
(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-tools)

(declare-function lispm-git-commit "lispm-memory" (agent message))

(defcustom lispm-save-sessions t
  "Whether to save session files at the end of every turn."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-compact-threshold 120000
  "Estimated size, in tokens, past which a session is handed over.
Before the next turn starts, the agent saves what should outlive the
conversation to memory and tasks, writes a summary, and the
conversation goes on in a continuation file holding only the summary.
nil hands over only on \\[lispm-compact]."
  :type '(choice (const :tag "Only on demand" nil) integer)
  :group 'lispm)

(defcustom lispm-compact-save-memory t
  "Whether an agent saves memory and tasks before its session is handed over."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-approval-functions nil
  "Hook run with an agent, a tool name and its input when a call awaits approval.
Use it for notifications, e.g. with the alert package."
  :type 'hook
  :group 'lispm)

(defvar lispm-inhibit-triggers nil
  "Non-nil while the runtime itself causes hook runs that are not events.")

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
   "- This conversation is an org-mode buffer the user can read, fold, edit and"
   " annotate while you work. What it contains is what you remember of this session.\n"
   "- Calls that change files or run code may wait for the user's approval. Such"
   " calls take a `why` argument: one plain sentence the user reads when deciding."
   " If a call is denied, adapt; do not repeat it unchanged.\n"
   "- Files can be open in Emacs with unsaved changes. read_file and the edit tools"
   " work on the live buffer, so you see what the user sees; every edit you make is"
   " one undo step for them.\n"
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

(defun lispm-system-prompt (agent)
  "System prompt of a new session of AGENT."
  (string-join
   (seq-remove
    (lambda (s) (or (null s) (string-blank-p s)))
    (append
     (list (format "You are \"%s\", an AI agent (role: %s) living inside a running GNU Emacs %s session, working alongside its human user. You share one programmable environment with them: files, buffers, processes and Emacs itself."
                   (lispm-agent-name agent) (lispm-agent-role agent) emacs-version)
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
             (not (buffer-live-p (lispm-agent-buffer standing))))
        (when id (setf (lispm-agent-id standing) id))
        standing)
       ((lispm-get-role (intern (or (cdr (assoc "role" keywords)) "")) t)
        (lispm-make-agent (intern (cdr (assoc "role" keywords)))
                          (or (cdr (assoc "root" keywords)) default-directory)
                          :name (lispm--free-name name) :id id))
       (t (message "LispM: no role %s to resume this session with"
                   (cdr (assoc "role" keywords)))
          nil)))))

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
                       (propertize "   type your message below, then C-c RET to send"
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
        " λ no agent: is its role defined?"
      (let ((state (lispm-agent-state agent)))
        (concat " λ " (symbol-name (lispm-agent-name agent))
                (propertize (format " (%s)" (lispm-agent-role agent)) 'face 'shadow)
                "  "
                (propertize (symbol-name state)
                            'face (if (eq state 'waiting) 'warning 'success))
                (if (lispm-agent-status agent)
                    (concat ": " (lispm-agent-status agent))
                  "")
                "    "
                (propertize (cond ((eq state 'idle) "C-c RET send")
                                  ((lispm-agent-condition agent)
                                   "C-c C-y choose · a new * User and C-c RET stops here and sends")
                                  ((eq state 'waiting) "C-c C-y review · C-c C-k abort")
                                  (t "C-c C-k abort"))
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

(defun lispm--create-session-file (agent &optional previous summary)
  "Write a new session file for AGENT and return its name.
PREVIOUS is the file this one continues, SUMMARY what it hands over;
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
                ,@(when-let* ((title (and previous
                                          (plist-get (lispm--session-description previous) :title))))
                    `(("title" . ,(lispm--oneline title 200))))))
             (lispm-transcript-section "System" (lispm-system-prompt agent))
             (if summary (lispm-transcript-section "Summary" summary) "")
             "* User\n")
     nil file nil 'silent)
    file))

(defun lispm--visit-session (agent file)
  "Make FILE AGENT's session buffer; return the buffer."
  (let ((buffer (let ((lispm--visiting agent)) (find-file-noselect file))))
    (with-current-buffer buffer
      (setq lispm-session-agent agent)
      (lispm-session-mode 1)
      (goto-char (point-min))
      (when (re-search-forward "^\\* System" nil t)
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
  "The newest file of the sessions standing AGENT had in its project."
  (let ((name (symbol-name (lispm-agent-name agent))))
    (car (sort (seq-filter
                (lambda (file)
                  (equal (cdr (assoc "agent" (lispm--file-keywords file))) name))
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
        (user-error "%s is busy" (lispm-agent-name agent)))
      (let ((file (and (not new) (lispm-agent-standing agent)
                       (lispm--latest-session-file agent))))
        (unless file
          (when new (setf (lispm-agent-id agent) (lispm--new-id)))
          (setq file (lispm--create-session-file agent)))
        (lispm--visit-session agent file)))))

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
        (user-error "%s is not a session LispM can resume" file))))

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

;;;; Recorded sessions

(defvar lispm--session-cache (make-hash-table :test #'equal)
  "Session descriptions keyed by file, valued (MTIME . DESCRIPTION).")

(defun lispm--first-prompt ()
  "First line of the first input in the current session text, or nil."
  (goto-char (point-min))
  (when (re-search-forward "^\\* \\(?:User\\|Message from .*\\|Event: .*\\)[ \t]*\n" nil t)
    (let* ((start (point))
           (end (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max)))
           (text (string-trim (org-unescape-code-in-string
                               (buffer-substring-no-properties start end)))))
      (unless (string-empty-p text)
        (car (split-string text "\n"))))))

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
                         :role (cdr (assoc "role" keywords))
                         :root (expand-file-name (or (cdr (assoc "root" keywords)) "~"))
                         :title title
                         :time time
                         :file file))))))
        (puthash file (cons time description) lispm--session-cache)
        description))))

(defun lispm-sessions ()
  "Every recorded session with some input, newest first.
Each is a plist (:id :agent :role :root :title :time :file); a session
continued in several files is listed once, by its newest file."
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
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((line (format "#+lispm_title: %s" title)))
            (if (re-search-forward "^#\\+lispm_title:.*$" 4000 t)
                (replace-match line t t)
              (goto-char (point-min))
              (while (looking-at "#\\+") (forward-line 1))
              (insert line "\n")))))
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
         (looking-at "\\* User[ \t]*\\(?:\n[ \t\n]*\\)?\\'")
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
  (lispm--add-section agent (concat "Event: " title) text))

;;;; Turns

(defun lispm--entry-title (entry)
  "Heading of the section holding inbox ENTRY."
  (cond ((plist-get entry :from) (format "Message from %s" (plist-get entry :from)))
        ((plist-get entry :event) (format "Event: %s" (plist-get entry :event)))
        (t "User")))

(cl-defun lispm-tell (agent text &key from event callback)
  "Give AGENT the input TEXT; it starts working as soon as it is idle.
FROM names the agent sending it; EVENT describes the hook or timer
that produced it (pending events with the same description are
coalesced).  CALLBACK, if given, is called with the agent's final
reply when the turn handling TEXT ends."
  (let* ((agent (lispm-get-agent agent))
         (inbox (lispm-agent-inbox agent))
         (same (and event (seq-find (lambda (e) (equal (plist-get e :event) event)) inbox))))
    (lispm-session-buffer agent)
    (if same
        (plist-put same :text text)
      (setf (lispm-agent-inbox agent)
            (append inbox (list (list :text text :from from :event event
                                      :callback callback)))))
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
A session grown past `lispm-compact-threshold' is handed over to a
continuation first, and the turn starts there."
  (if (and lispm-compact-threshold
           (not (plist-get entry :no-compact))
           (> (lispm--estimated-tokens agent) lispm-compact-threshold)
           (lispm--has-history-p agent))
      (progn
        (push (or entry (list :text (lispm--take-input agent))) (lispm-agent-inbox agent))
        (lispm-compact agent))
    (lispm--start-turn agent entry)))

(defun lispm--start-turn (agent entry)
  "Start a turn of AGENT on inbox ENTRY, or on what the user typed."
  (cl-incf (lispm-agent-turn agent))
  (setf (lispm-agent-steps agent) 0
        (lispm-agent-last-reply agent) nil
        (lispm-agent-auto-restarts agent) nil
        (lispm-agent-max-tokens agent) nil
        ;; Another agent's message or an event: nobody may be watching.
        (lispm-agent-attended agent) (not (or (plist-get entry :from) (plist-get entry :event)))
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback))))
  (when entry
    (lispm--in-session agent
      (let ((input (lispm--input-heading-p)))
        (when input (delete-region input (point-max))))
      (lispm--newline)
      (insert (lispm-transcript-section (lispm--entry-title entry) (plist-get entry :text)))))
  (lispm--request agent))

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
the middle of a session, as when it turns on a toolset it was offered
on demand.  The earlier tools keep their place; the request prefix
changes once."
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
      (lispm-set-state agent 'requesting waiting)
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
    (pcase (condition-case err
               (lispm-request-body agent)
             (error (error-message-string err)))
      ((and (pred stringp) problem) (lispm-signal agent 'request-failed problem))
      (`(,body . ,betas)
       (cl-incf (lispm-agent-steps agent))
       (lispm--send agent body betas 0))))))

(defun lispm--send (agent body betas attempt)
  "Send the request BODY with BETAS for AGENT; ATTEMPT counts retries."
  (lispm-set-state agent 'requesting
                   (if (> attempt 0) (format "retry %d" attempt) "thinking"))
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

(defun lispm--live-begin (agent)
  "Open the region where AGENT's response streams in."
  (lispm--in-session agent
    (lispm--newline)
    (let ((start (point-marker)))
      (insert "* Assistant\n")
      (setq lispm--live (cons start (copy-marker (point) t))
            lispm--live-kind nil))))

(defun lispm--live-insert (agent kind text)
  "Show streamed content TEXT of KIND (text, thinking or tool) for AGENT."
  (lispm--in-session agent
    (when lispm--live
      (goto-char (cdr lispm--live))
      (unless (eq kind lispm--live-kind)
        (lispm--newline)
        (setq lispm--live-kind kind))
      (pcase kind
        ('text (insert text))
        ('thinking (insert (propertize text 'face 'shadow)))
        ('tool (insert (propertize (format "-> %s ...\n" text) 'face 'shadow))))))
  (when (eq kind 'tool)
    (lispm-set-state agent 'requesting (format "writing a %s call" text))))

(defun lispm--live-delete (agent)
  "Remove AGENT's streamed region; the final message replaces it."
  (lispm--in-session agent
    (when lispm--live
      (delete-region (car lispm--live) (max (car lispm--live) (cdr lispm--live)))
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
if the response ended abnormally.  Return (STATE ACTION MESSAGE)."
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
      (pcase (lispm-tool-action agent tool (alist-get 'input block))
        ('auto (list "RUNNING" 'run))
        ('ask (list "WAITING" 'ask))
        (_ (list "DENIED" 'deny
                 (format "Denied by policy: %s may not use %s tools."
                         (lispm-agent-name agent) (lispm-tool-permission tool)))))))))

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
                      (insert "* Assistant\n")
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
              (puthash id (list :status (nth 1 (alist-get id decisions nil nil #'equal))
                                :name (alist-get 'name b)
                                :tool (lispm-get-tool (alist-get 'name b))
                                :input (alist-get 'input b)
                                :marker (alist-get id calls nil nil #'equal))
                       pending)))
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
    (when proceed
      (lispm--maybe-continue agent))))

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
      (lispm-set-state agent 'acting (format "running %s" (plist-get call :name)))
      (let ((lispm-current-agent agent)
            (default-directory (lispm-agent-root agent)))
        (lispm-tool-invoke tool (plist-get call :input)
                           (lambda (text errorp &optional images)
                             (lispm--tool-done agent id text errorp nil images)))))))

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
    (let ((waiting 0) (running nil))
      (maphash (lambda (_id call)
                 (if (eq (plist-get call :status) 'ask)
                     (cl-incf waiting)
                   (setq running (plist-get call :name))))
               (lispm-agent-pending agent))
      (cond ((> waiting 0)
             (lispm-set-state agent 'waiting
                              (format "%d call%s await%s approval" waiting
                                      (if (= waiting 1) "" "s") (if (= waiting 1) "s" ""))))
            (running (lispm-set-state agent 'acting (format "running %s" running)))))))

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
        (insert (lispm-transcript-section "Note" note)))
      (insert "* User\n"))
    (lispm-set-state agent 'idle)
    (lispm--save-session agent)
    (let ((answer (string-trim (concat reply (if note (concat "\n\n" note) "")))))
      (dolist (callback callbacks)
        (with-demoted-errors "lispm turn callback: %S"
          (funcall callback (if (string-empty-p answer) "(no reply)" answer)))))
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
                           (format "%s; retrying in %ds" (plist-get err :type) delay))
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
      (user-error "Not in an agent session (or its agent is not defined)")))

(defun lispm-send ()
  "Send what you wrote under the last \"* User\" heading."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (if (and (lispm-agent-condition agent) (not (lispm--waiting-calls agent)))
        (lispm--send-past-condition agent)
      (when (lispm-agent-busy-p agent)
        (user-error "%s is busy (%s); C-c C-k aborts"
                    (lispm-agent-name agent) (lispm-agent-state agent)))
      (setf (lispm-agent-buffer agent) (current-buffer))
      (let ((parsed (lispm-transcript-parse)))
        (when (plist-get parsed :pending)
          (user-error "Some tool calls have no result yet"))
        (unless (equal (plist-get parsed :last-role) "user")
          (user-error "Write your message under a \"* User\" heading first")))
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
                    (and (re-search-backward "^\\* User[ \t]*$" nil t)
                         (> (point) (plist-get condition :marker))
                         (string-trim (buffer-substring-no-properties
                                       (min (point-max) (1+ (line-end-position)))
                                       (point-max))))))))
    (unless (and input (not (string-empty-p input)))
      (user-error "%s waits at a condition: choose a restart with C-c C-y, or write \
under a new \"* User\" heading after it" (lispm-agent-name agent)))
    (lispm--take-input agent)
    (lispm-stop-at-condition agent)
    (lispm-tell agent input)))

(defun lispm-abort (&optional agent)
  "Stop AGENT's turn: cancel the request, the running and waiting calls,
and its queued inputs."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (unless (lispm-agent-busy-p agent)
      (user-error "%s is idle" (lispm-agent-name agent)))
    (lispm--stop agent "Aborted by the user.")))

(defun lispm--stop (agent note)
  "Stop AGENT's turn, its calls and queued inputs, recording NOTE.
A condition the turn waits at is closed, its message joining NOTE; one
between turns ends no turn, and NOTE is recorded as an event."
  (let ((between nil))
    (when-let* ((condition (lispm-agent-condition agent)))
      (setf (lispm-agent-condition agent) nil
            between (lispm--between-turns-p condition))
      (lispm--close-condition agent condition 'abort)
      (setq note (concat (plist-get condition :message) "\n" note)))
    (setf (lispm-agent-compacting agent) nil)
    (lispm-set-state agent 'aborting)
    (dolist (p (process-list))
      (when (eq (process-get p 'lispm-agent) agent)
        (process-put p 'lispm-aborted t)
        (delete-process p)))
    (lispm--live-delete agent)
    (let (ids)
      (maphash (lambda (id _) (push id ids)) (lispm-agent-pending agent))
      (dolist (id ids)
        (lispm--tool-done agent id "Interrupted: the user aborted the turn." t "FAILED")))
    (let ((inbox (lispm-agent-inbox agent)))
      (setf (lispm-agent-inbox agent) nil)
      (dolist (entry inbox)
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback (format "(%s was stopped before handling this)"
                                    (lispm-agent-name agent))))))
    (if (not between)
        (lispm--finish-turn agent note)
      (lispm--record-event agent "stopped" note)
      (lispm-set-state agent 'idle)
      (lispm--save-session agent))))

;;;; Approval

(defun lispm-resolve (agent id decision &optional reason)
  "Settle AGENT's call ID awaiting approval: DECISION is `approve' or `deny'.
REASON, for a denial, is passed on to the model."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (when (and call (eq (plist-get call :status) 'ask))
      (pcase decision
        ('approve (lispm--run-tool agent id))
        ('deny (lispm--tool-done
                agent id
                (concat "The user denied this call."
                        (if (and reason (not (string-blank-p reason)))
                            (concat " Their reason: " reason)
                          " Do not repeat it unchanged; adapt, or ask the user."))
                t "DENIED"))))))

(defconst lispm--interrupted-result
  "The call was interrupted: Emacs stopped before it finished, so whether it took effect is unknown.  Check before repeating it."
  "Result of a call that was running when its session was last saved.")

(defun lispm--resume-calls (agent)
  "Take up the tool calls AGENT's session left without a result.
A session saved in the middle of a turn and opened again, after
Emacs restarted, records them as headings, which hold all a call
needs: calls awaiting approval await it again, and calls that were
running are recorded as interrupted, their processes gone.  Once the
calls awaiting approval are settled, the turn goes on; if there are
none, it ends here, without a request to the model.  A condition
awaiting a restart is taken up likewise: the agent waits at it again.
This runs while the session opens, before `lispm-session-mode-hook'
has started its MCP servers: tools are looked up when a call runs."
  (lispm--restore-model agent)
  (let* ((parsed (and (eq (lispm-agent-state agent) 'idle)
                      (zerop (hash-table-count (lispm-agent-pending agent)))
                      (lispm-transcript-parse)))
         (calls (plist-get parsed :pending-calls)))
    (when-let* (((not calls))
                (condition (plist-get parsed :condition)))
      (cl-incf (lispm-agent-turn agent))
      (setf (lispm-agent-steps agent) 0
            (lispm-agent-auto-restarts agent) nil
            ;; Whoever started it, the user is the one reopening it.
            (lispm-agent-attended agent) t)
      (lispm--wait-at agent (append (list :marker (copy-marker (plist-get condition :pos)))
                                    condition)))
    (when calls
      (cl-incf (lispm-agent-turn agent))
      (setf (lispm-agent-steps agent) 0)
      (dolist (call calls)
        (puthash (plist-get call :id)
                 (list :status (if (member (plist-get call :state) '("WAITING" "APPROVED"))
                                   'ask
                                 'run)
                       :name (plist-get call :name)
                       :input (plist-get call :input)
                       :marker (copy-marker (plist-get call :pos) t))
                 (lispm-agent-pending agent)))
      (lispm-set-state agent 'acting)
      (setf (lispm-agent-dispatching agent) t)
      (unwind-protect
          (dolist (call calls)
            (let ((id (plist-get call :id)))
              (pcase (plist-get call :state)
                ("WAITING")
                ;; Approved, but never run: ask again rather than run it
                ;; while the session is being opened.
                ("APPROVED"
                 (lispm--in-session agent
                   (lispm-transcript-set-state
                    (plist-get (gethash id (lispm-agent-pending agent)) :marker) "WAITING")))
                (_ (lispm--tool-done agent id lispm--interrupted-result t)))))
        (setf (lispm-agent-dispatching agent) nil))
      (lispm--update-state agent)
      (unless (eq (lispm-agent-state agent) 'waiting)
        (lispm--in-session agent
          (lispm--newline)
          (insert "* User\n"))
        (lispm-set-state agent 'idle)))))

(defun lispm--ask-approval (agent id)
  "Show AGENT's call ID for approval: a preview, a notification."
  (let* ((call (gethash id (lispm-agent-pending agent)))
         (preview (let ((lispm-current-agent agent)
                        (default-directory (lispm-agent-root agent)))
                    (lispm-tool-preview-text (plist-get call :tool) (plist-get call :input)))))
    (when (and preview (plist-get call :marker))
      (lispm--in-session agent
        (goto-char (lispm-transcript-subtree-end (plist-get call :marker) 2))
        (insert (lispm-transcript-preview preview))))
    (run-hook-with-args 'lispm-approval-functions agent (plist-get call :name)
                        (plist-get call :input))
    (message "λ %s wants to run %s: %s" (lispm-agent-name agent) (plist-get call :name)
             (if (buffer-live-p (lispm-agent-view agent))
                 (format "answer in %s" (buffer-name (lispm-agent-view agent)))
               (format "review it with C-c C-y in %s" (buffer-name (lispm-agent-buffer agent)))))))

(defun lispm--todo-state-changed ()
  "Settle a call or condition whose heading the user moved to a new TODO state.
A call is approved or denied; a condition gets its default restart or
`abort'.  This makes `org-todo', and the org agenda, an approval
interface."
  (when-let* ((agent lispm-session-agent))
    (let ((id (org-entry-get nil "TOOL_ID"))
          (condition (lispm-agent-condition agent)))
      (cond
       ((and id (eq (plist-get (gethash id (lispm-agent-pending agent)) :status) 'ask))
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-resolve agent id 'approve))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-resolve agent id 'deny))))
       ((and condition (equal (org-entry-get nil "CONDITION_ID") (plist-get condition :id)))
        (pcase org-state
          ((or "APPROVED" "RUNNING" "DONE")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (car (plist-get condition :restarts))))
          ((or "DENIED" "FAILED")
           (run-at-time 0 nil #'lispm-restart agent (plist-get condition :id)
                        (lispm--stop-restart condition)))))))))

(defun lispm--waiting-calls (agent)
  "Ids of AGENT's calls awaiting approval, in transcript order."
  (let (ids)
    (maphash (lambda (id call)
               (when (eq (plist-get call :status) 'ask)
                 (push (cons id (and (plist-get call :marker)
                                     (marker-position (plist-get call :marker))))
                       ids)))
             (lispm-agent-pending agent))
    (mapcar #'car (sort ids (lambda (a b) (< (or (cdr a) 0) (or (cdr b) 0)))))))

(defun lispm--call-at-point (agent)
  "Id of AGENT's call awaiting approval at point, if any."
  (when-let* ((id (ignore-errors (org-entry-get nil "TOOL_ID" t)))
              (call (gethash id (lispm-agent-pending agent))))
    (and (eq (plist-get call :status) 'ask) id)))

(defun lispm-review ()
  "Review the call at point, or the first awaiting approval, and decide.
With none, choose a restart for the condition the turn waits at."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (if (and (lispm-agent-condition agent)
             (not (lispm--call-at-point agent))
             (not (lispm--waiting-calls agent)))
        (lispm-choose-restart agent)
      (lispm--review-call agent))))

(defun lispm--review-call (agent)
  "Review AGENT's call at point, or the first awaiting approval, and decide."
  (let* ((id (or (lispm--call-at-point agent)
                 (car (lispm--waiting-calls agent))
                 (user-error "No call awaits approval")))
         (call (gethash id (lispm-agent-pending agent)))
         (name (plist-get call :name)))
    (when (plist-get call :marker)
      (goto-char (plist-get call :marker)))
    (ignore-errors (org-fold-show-subtree))
    (recenter 0)
    (pcase (car (read-multiple-choice
                 (format "%s wants to run %s" (lispm-agent-name agent) name)
                 '((?y "yes" "Run this call")
                   (?n "no" "Deny it; you may give a reason")
                   (?a "always" "Run it, and every later call of this tool, this Emacs session")
                   (?q "later" "Decide later"))))
      (?y (lispm-resolve agent id 'approve))
      (?n (lispm-resolve agent id 'deny (read-string "Reason (optional): ")))
      (?a (push name (lispm-agent-allowed agent))
          (dolist (other (lispm--waiting-calls agent))
            (when (equal (plist-get (gethash other (lispm-agent-pending agent)) :name) name)
              (lispm-resolve agent other 'approve)))))))

(defun lispm-approve ()
  "Approve the call at point, or the first one awaiting approval.
With none, choose the default restart of the condition the turn waits at."
  (interactive)
  (let* ((agent (lispm--buffer-agent))
         (id (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))))
         (condition (lispm-agent-condition agent)))
    (cond (id (lispm-resolve agent id 'approve))
          (condition (lispm-restart agent (plist-get condition :id)
                                    (car (plist-get condition :restarts))))
          (t (user-error "No call awaits approval")))))

(defun lispm-deny (reason)
  "Deny the call at point, or the first one awaiting approval, for REASON.
With none, stop at the condition the turn waits at (see
`lispm-stop-at-condition')."
  (interactive
   (let ((agent (lispm--buffer-agent)))
     (list (and (or (lispm--call-at-point agent) (lispm--waiting-calls agent))
                (read-string "Reason (optional): ")))))
  (let* ((agent (lispm--buffer-agent))
         (id (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))))
         (condition (lispm-agent-condition agent)))
    (cond (id (lispm-resolve agent id 'deny reason))
          (condition (lispm-stop-at-condition agent))
          (t (user-error "No call awaits approval")))))

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
  '((request-failed
     :title "Request failed"
     :restarts ((retry "Retry the request" lispm--restart-retry)
                (use-model "Retry with another backend or model"
                           lispm--restart-use-model lispm--read-model)
                (abort "Stop here" lispm--restart-abort)))
    (step-limit
     :title "Step limit reached"
     :restarts ((continue "Continue, for as many requests again" lispm--restart-continue)
                (abort "Stop here" lispm--restart-abort)))
    (output-limit
     :title "Response cut short"
     :restarts ((retry-larger "Ask again, with twice the output limit" lispm--restart-retry-larger)
                (keep "Keep what it wrote, and let it go on" lispm--restart-keep)
                (abort "Stop here" lispm--restart-abort)))
    (context-full
     :title "Context window full"
     :restarts ((compact "Compact the conversation, then go on" lispm--restart-compact)
                (abort "Stop here" lispm--restart-abort)))
    (refused
     :title "Response refused"
     :restarts ((retry "Ask again" lispm--restart-ask-again)
                (abort "Stop here" lispm--restart-abort)))
    (paused
     :title "Response paused"
     :restarts ((continue "Let it go on" lispm--restart-go-on)
                (abort "Stop here" lispm--restart-abort)))
    (compaction-failed
     :title "Compaction failed"
     :restarts ((retry "Try to compact again" lispm--restart-compaction-retry)
                (skip "Carry on without compacting" lispm--restart-compaction-skip)
                (abort "Stop here" lispm--restart-compaction-abort))))
  "Kinds of conditions a turn can stop at, and how it can go on.
Each entry is (TYPE :title TITLE :restarts RESTARTS).  TITLE says what
happened.  Each restart is (NAME DESCRIPTION FUNCTION [READER]), the
first being the default: FUNCTION is called with the agent, the
condition (a plist: :id :type :title :message :restarts) and a value,
which READER, if any, asks the user for when they choose it.")

(defcustom lispm-condition-handlers '((paused continue :times 10))
  "Restarts chosen without asking, by condition type, in turns the user started.
Each entry is (TYPE RESTART . PROPS): a condition of TYPE gets RESTART
at once, or after :after seconds, at most :times times in a turn (1
by default); after that, :then names the restart to choose, or the
user chooses.  :value is the value RESTART needs, if any.  For
instance, to retry a failed request after a minute, twice:

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
    (compaction-failed skip))
  "Restarts chosen without asking in turns nobody may be watching.
Those are turns started by another agent's message (a helper's, say)
or by an event (a hook, a timer), which, left to wait for the user,
would hold up whoever waits for them, or the next event.  Entries are
as in `lispm-condition-handlers'.  By default a failed request is
tried again twice, a minute apart, and then the turn stops, as does
one that reaches `lispm-max-steps' or is refused; a response cut
short is asked for again with a larger limit, or the session handed
over, once; a paused one goes on; a failed handover is skipped.
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
  (run-hook-with-args 'lispm-condition-functions agent condition)
  (unless (lispm--handle-condition agent condition)
    (message "λ %s: %s; %s" (lispm-agent-name agent) (plist-get condition :title)
             (if (buffer-live-p (lispm-agent-view agent))
                 (format "choose what to do in %s" (buffer-name (lispm-agent-view agent)))
               (format "choose what to do with C-c C-y in %s"
                       (buffer-name (lispm-agent-buffer agent)))))))

(defun lispm--handle-condition (agent condition)
  "Choose a restart for AGENT's CONDITION by the handlers of its turn.
Those are `lispm-condition-handlers', or for a turn the user did not
start, `lispm-unattended-condition-handlers'.  Return non-nil if one
is chosen, now or after a delay."
  (pcase-let* ((type (plist-get condition :type))
               (`(,first . ,props) (alist-get type (if (lispm-agent-attended agent)
                                                         lispm-condition-handlers
                                                       lispm-unattended-condition-handlers)))
               (count (alist-get type (lispm-agent-auto-restarts agent) 0))
               (again (< count (or (plist-get props :times) 1)))
               (restart (if again first (plist-get props :then))))
    (when (and restart (memq restart (plist-get condition :restarts)))
      (when again
        (setf (alist-get type (lispm-agent-auto-restarts agent)) (1+ count)))
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
                       (when (eq (lispm-agent-condition agent) condition)
                         (lispm-restart agent id restart value))))
        t))))

(defun lispm-restart (agent id restart &optional value)
  "Go on with AGENT's turn, stopped at the condition ID, by RESTART.
VALUE is what the restart needs, if anything: one with a reader in
`lispm-condition-types' needs one, such as `use-model', a cons
\(BACKEND . MODEL).  The condition's heading records the choice."
  (let ((condition (lispm-agent-condition agent)))
    (unless (and condition (equal (plist-get condition :id) id))
      (user-error "%s waits at no condition %s" (lispm-agent-name agent) id))
    (let ((spec (lispm--restart-spec condition restart)))
      (when (and (nth 3 spec) (null value))
        (user-error "The restart %s needs a value" restart))
      (setf (lispm-agent-condition agent) nil)
      (lispm--close-condition agent condition restart value)
      (funcall (nth 2 spec) agent condition value))))

(defun lispm-stop-at-condition (agent)
  "Stop AGENT's turn at the condition it waits at, if any.
That is restart `abort', or for a condition without it, the last of
its restarts.  What the user asks of AGENT next goes past it this way."
  (when-let* ((condition (lispm-agent-condition agent)))
    (lispm-restart agent (plist-get condition :id) (lispm--stop-restart condition))))

(defun lispm--stop-restart (condition)
  "The restart of CONDITION that stops there: `abort', or its last one."
  (let ((restarts (plist-get condition :restarts)))
    (if (memq 'abort restarts) 'abort (car (last restarts)))))

(defun lispm--between-turns-p (condition)
  "Non-nil if CONDITION was signaled between turns, not in one.
Only a failed handover is: it comes before the input that asked for it."
  (and (eq (plist-get condition :type) 'compaction-failed)
       (not (plist-get condition :resume))))

(defun lispm--awaiting (agent)
  "What AGENT awaits an answer to: its calls awaiting approval, then its condition."
  (append (lispm--waiting-calls agent)
          (and (lispm-agent-condition agent) (list (lispm-agent-condition agent)))))

(defun lispm-restart-description (condition restart)
  "How RESTART of CONDITION is described to the user: its name if unknown."
  (if-let* ((spec (assq restart (plist-get (alist-get (plist-get condition :type)
                                                       lispm-condition-types)
                                            :restarts))))
      (nth 1 spec)
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
          (when (re-search-forward "^:END:$" (lispm-transcript-subtree-end marker 1) t)
            (beginning-of-line)
            (insert (format ":RESTART: %s\n" restart)
                    (if value
                        (format ":VALUE: %s\n" (let ((print-escape-newlines t))
                                                  (prin1-to-string value)))
                      ""))))))))

(defun lispm-choose-restart (&optional agent)
  "Choose a restart for the condition AGENT's turn waits at.
Interactively, the agent of the current session or chat."
  (interactive)
  (let* ((agent (or agent (lispm--buffer-agent)))
         (condition (or (lispm-agent-condition agent)
                        (user-error "%s waits at no condition" (lispm-agent-name agent))))
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
                  (format "%s: %s" (plist-get condition :title) (plist-get condition :message))
                  (append choices '((?q "later" "Decide later"))))))
    (unless (eq (car answer) ?q)
      (lispm-choose-restart-named agent (intern (nth 1 answer))))))

(defun lispm-choose-restart-named (agent restart)
  "Choose RESTART for the condition AGENT waits at.
If the restart needs a value, ask the user for it."
  (let* ((condition (or (lispm-agent-condition agent)
                        (user-error "%s waits at no condition" (lispm-agent-name agent))))
         (reader (nth 3 (lispm--restart-spec condition restart)))
         (value (and reader (funcall reader agent))))
    ;; Reading took time: the condition may be settled meanwhile.
    (unless (eq condition (lispm-agent-condition agent))
      (user-error "%s no longer waits at that condition" (lispm-agent-name agent)))
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
  (when (and backend (not (assq backend lispm-backends)))
    (user-error "Unknown backend %s (see `lispm-backends')" backend))
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
                    ((or (null (car value)) (assq (car value) lispm-backends))))
          (setf (lispm-agent-backend agent) (car value)
                (lispm-agent-model agent) (cdr value)))))))

(defun lispm--restart-abort (agent condition _value)
  "End AGENT's turn at CONDITION, as a turn stopped early used to end."
  (lispm--finish-turn agent (plist-get condition :message)))

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
                 (looking-at "\\* Assistant[ \t]*$"))
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
  (lispm--add-section agent "From LispM" text)
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
         (name (completing-read (format-prompt "Backend" current)
                                (mapcar (lambda (b) (symbol-name (car b))) lispm-backends)
                                nil t nil nil (and current (symbol-name current))))
         (backend (if (string-empty-p name)
                      (user-error "No backend chosen")
                    (intern name)))
         (default (plist-get (alist-get backend lispm-backends) :model))
         (model (read-string (format-prompt "Model" default) nil nil default)))
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
  "Your context is long and will be compacted: this conversation will be replaced by a summary. Before that, save what should outlive it: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Do not summarize the conversation now. Reply in one sentence when done."
  "Asked of an agent before its session is handed over.")

(defconst lispm--resumed-memory-prompt
  "Your context filled up in the middle of this turn and was compacted: the summary above is all that is left of the conversation. Save what should outlive it, if you have not already: durable knowledge with `remember`, unfinished work with `task_add` or `task_update`. Then go on with the turn."
  "Told an agent whose session was handed over in the middle of a turn.")

(defconst lispm--summary-prompt
  "Write a handoff summary of this conversation for yourself: a fresh session will hold only this summary and continue from it. Cover the user's goals and requests, the decisions made and why, the current state of the work (files changed, what was verified), and the open questions and next steps. Be specific: names, paths, values. Leave out what is already saved in memory or tasks. Reply with the summary only."
  "Asked of an agent to summarize its session for the continuation.")

(defun lispm--estimated-tokens (agent)
  "Rough number of tokens of AGENT's session."
  (let ((buffer (lispm-agent-buffer agent)))
    (if (buffer-live-p buffer)
        (/ (buffer-size buffer) 3)
      0)))

(defun lispm--has-history-p (agent)
  "Non-nil if AGENT's session holds a response: something to hand over."
  (with-current-buffer (lispm-agent-buffer agent)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\\* Assistant[ \t]*$" nil t))))

(defun lispm--take-input (agent)
  "Remove the text the user typed under AGENT's last \"* User\" and return it."
  (lispm--in-session agent
    (when (re-search-backward "^\\* User[ \t]*$" nil t)
      (let ((text (string-trim (buffer-substring-no-properties
                                (min (point-max) (1+ (line-end-position))) (point-max)))))
        (delete-region (point) (point-max))
        text))))

(defun lispm-compact (&optional agent)
  "Hand AGENT's session over to a continuation holding a summary of it.
Interactively, the agent of the current session."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (when (or (lispm-agent-busy-p agent) (lispm-agent-compacting agent))
      (user-error "%s is busy" (lispm-agent-name agent)))
    (setf (lispm-agent-compacting agent) t)
    (if lispm-compact-save-memory
        (lispm--start-turn agent (list :event "compaction" :text lispm--save-memory-prompt
                                       :no-compact t
                                       :callback (lambda (_) (lispm--summarize agent))))
      (lispm--summarize agent))))

(defun lispm--summarize (agent &optional resume)
  "Ask the model for a summary of AGENT's session, then continue from it.
RESUME non-nil goes on with the turn in the continuation."
  (when (lispm-agent-compacting agent)
    (lispm-set-state agent 'compacting "writing a summary")
    (condition-case err
        (let* ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse)))
               (messages (lispm-transcript--normalize
                          (append (plist-get parsed :messages)
                                  (list `((role . "user")
                                          (content . [((type . "text")
                                                       (text . ,lispm--summary-prompt))]))))))
               (request (lispm-api-request (lispm-agent-backend* agent) (lispm-agent-model* agent)
                                           (lispm-agent-effort* agent) (plist-get parsed :system)
                                           nil messages))
               (proc (funcall lispm-api-function (lispm-agent-backend* agent)
                              (car request) (cdr request)
                              :on-done (lambda (message)
                                         (lispm--continue agent (lispm--reply-text
                                                                 (append (alist-get 'content message) nil))
                                                          resume))
                              :on-error (lambda (err)
                                          (lispm--compaction-failed
                                           agent (format "%s: %s" (plist-get err :type)
                                                         (plist-get err :message))
                                           resume)))))
          (when (processp proc) (process-put proc 'lispm-agent agent)))
      (error (lispm--compaction-failed agent (error-message-string err) resume)))))

(defun lispm--continue (agent summary &optional resume)
  "Carry AGENT's conversation on in a new session file holding SUMMARY.
RESUME non-nil goes on with the turn there."
  (when (lispm-agent-compacting agent)
    (if (string-blank-p summary)
        (lispm--compaction-failed agent "the summary came back empty" resume)
      (let* ((old (lispm-agent-buffer agent))
             (old-file (buffer-file-name old))
             (file (lispm--create-session-file agent old-file summary)))
        (lispm--in-session agent
          (let ((input (lispm--input-heading-p)))
            (when input (delete-region input (point-max))))
          (lispm--newline)
          (insert (lispm-transcript-section
                   "Note" (format "Continued in [[file:%s]]." (abbreviate-file-name file)))))
        (lispm--save-session agent)
        (let ((new (lispm--visit-session agent file)))
          (dolist (window (get-buffer-window-list old nil t))
            (set-window-buffer window new)
            (set-window-point window (with-current-buffer new (point-max))))
          (with-current-buffer old (setq lispm-session-agent nil))
          (kill-buffer old))
        (lispm--save-session agent)
        (when (fboundp 'lispm-git-commit)
          (lispm-git-commit agent (format "%s: hand over to %s" (lispm-agent-name agent)
                                          (file-name-nondirectory file))))
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

;;;; Journal: audit and rollback

(defun lispm--journal-entry (agent &optional turn)
  "Journal (TURN . SNAPSHOTS) of AGENT for TURN, or its latest one with edits."
  (or (if turn
          (assq turn (lispm-agent-journal agent))
        (seq-find #'cdr (lispm-agent-journal agent)))
      (user-error "%s made no edits%s" (lispm-agent-name agent)
                  (if turn (format " in turn %d" turn) ""))))

(defun lispm--snapshot-now (snapshot)
  "Current text of the buffer or file in SNAPSHOT.
A file's is its buffer's, unless that is out of date with it (see
`lispm--file-text')."
  (let ((buffer (car snapshot))
        (file (plist-get (cdr snapshot) :file)))
    (cond (file (lispm--file-text file))
          ((buffer-live-p buffer) (lispm-tools--buffer-text buffer))
          (t ""))))

(defun lispm--read-turn ()
  "Read a turn number when called with a prefix argument."
  (and current-prefix-arg (read-number "Turn: ")))

(defun lispm-turn-diff (agent &optional turn)
  "Show every change AGENT made in TURN (default: its latest edits)."
  (interactive (list (lispm--buffer-agent) (lispm--read-turn)))
  (pcase-let ((`(,turn . ,snapshots) (lispm--journal-entry agent turn)))
    (with-current-buffer (get-buffer-create "*lispm-diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Changes by %s in turn %d\n\n" (lispm-agent-name agent) turn))
        (dolist (s snapshots)
          (insert (lispm--diff (plist-get (cdr s) :text) (lispm--snapshot-now s)
                               (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                  "\n")))
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer "*lispm-diff*")))

(defun lispm-revert-turn (agent &optional turn)
  "Undo every change AGENT made to buffers and files in TURN.
Default: its latest turn with edits."
  (interactive (list (lispm--buffer-agent) (lispm--read-turn)))
  (pcase-let ((`(,turn . ,snapshots) (lispm--journal-entry agent turn)))
    ;; All or nothing: a buffer with unsaved changes whose file changed
    ;; on disk too is for the user to settle first.
    (when-let* ((conflicts (seq-filter (lambda (s)
                                         (and (buffer-live-p (car s))
                                              (memq (lispm-tools--sync (car s)) '(conflict held))))
                                       snapshots)))
      (user-error "Nothing reverted: %s changed on disk, and in Emacs too; save or revert first"
                  (mapconcat (lambda (s) (buffer-name (car s))) conflicts ", ")))
    (unless (or (not (called-interactively-p 'any))
                (yes-or-no-p (format "Revert %d buffer(s) %s changed in turn %d? "
                                     (length snapshots) (lispm-agent-name agent) turn)))
      (user-error "Cancelled"))
    (dolist (s snapshots)
      (let* ((buffer (car s))
             (props (cdr s))
             (file (plist-get props :file))
             (lispm-current-agent nil))
        (cond
         ((and file (not (plist-get props :existed)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (when (and (file-exists-p file)
                     (or (not (called-interactively-p 'any))
                         (yes-or-no-p (format "Delete %s, which %s created? "
                                              file (lispm-agent-name agent)))))
            (delete-file file)))
         ((buffer-live-p buffer) (lispm-tools--set-text buffer (plist-get props :text)))
         (file (let ((coding-system-for-write 'utf-8))
                 (write-region (plist-get props :text) nil file nil 'silent))))))
    (setf (lispm-agent-journal agent) (assq-delete-all turn (lispm-agent-journal agent)))
    (lispm--record-event
     agent "revert"
     (format "The user reverted the changes you made in turn %d to: %s."
             turn (mapconcat (lambda (s) (or (plist-get (cdr s) :file) (plist-get (cdr s) :name)))
                             snapshots ", ")))
    (message "Reverted turn %d of %s" turn (lispm-agent-name agent))))

(provide 'lispm-runtime)
;;; lispm-runtime.el ends here
