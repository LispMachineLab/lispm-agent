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
   " from colleagues: anything consequential still needs the user's intent.\n\n"
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
            (setf (lispm-agent-buffer agent) (current-buffer)))))
    (remove-hook 'org-after-todo-state-change-hook #'lispm--todo-state-changed t)
    (remove-hook 'kill-buffer-hook #'lispm--session-killed t)
    (remove-hook 'after-change-functions #'lispm--update-placeholder t)
    (when (overlayp lispm--placeholder) (delete-overlay lispm--placeholder))
    (kill-local-variable 'header-line-format)))

(defun lispm--update-placeholder (&rest _)
  "Show a hint after the input heading while nothing is written under it."
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
                (propertize (pcase state
                              ('idle "C-c RET send")
                              ('waiting "C-c C-y review · C-c C-k abort")
                              (_ "C-c C-k abort"))
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
                 (format "projects/%s/sessions/*.org" (lispm--slug (lispm-agent-root agent)))))
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

(defun lispm--record-event (agent title text)
  "Note TEXT as an event in AGENT's session without starting a turn.
The model reads it with the next input."
  (lispm--in-session agent
    (let ((input (lispm--input-heading-p)))
      (if input
          (goto-char input)
        (lispm--newline))
      (insert (lispm-transcript-section (concat "Event: " title) text)))))

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
  "Start a turn on AGENT's next inbox item if it is idle."
  (when (and (not (lispm-agent-busy-p agent))
             (not (lispm-agent-compacting agent))
             (lispm-agent-inbox agent))
    (let ((entry (pop (lispm-agent-inbox agent))))
      (lispm-session-buffer agent)
      (lispm--begin-turn agent entry))))

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
        (lispm-agent-turn-callbacks agent) (delq nil (list (plist-get entry :callback))))
  (when entry
    (lispm--in-session agent
      (let ((input (lispm--input-heading-p)))
        (when input (delete-region input (point-max))))
      (lispm--newline)
      (insert (lispm-transcript-section (lispm--entry-title entry) (plist-get entry :text)))))
  (lispm--request agent))

(defun lispm-request-body (agent)
  "Request for AGENT's next model call, as (BODY . BETAS).
Signal a `user-error' if the session cannot be sent as it is."
  (let ((parsed (with-current-buffer (lispm-agent-buffer agent) (lispm-transcript-parse))))
    (when (plist-get parsed :pending)
      (user-error "Tool calls still await a result: %s"
                  (string-join (plist-get parsed :pending) ", ")))
    (unless (equal (plist-get parsed :last-role) "user")
      (user-error "Nothing to send: write under a \"* User\" heading"))
    (lispm-api-request (lispm-agent-backend* agent)
                       (lispm-agent-model* agent)
                       (lispm-agent-effort* agent)
                       (plist-get parsed :system)
                       (mapcar #'lispm-tool-schema (lispm-agent-tools* agent))
                       (plist-get parsed :messages))))

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
    (lispm--finish-turn agent (format "Stopped after %d model requests (`lispm-max-steps')."
                                      lispm-max-steps)))
   (t
    (pcase (condition-case err
               (lispm-request-body agent)
             (error (error-message-string err)))
      ((and (pred stringp) problem) (lispm--finish-turn agent problem))
      (`(,body . ,betas)
       (cl-incf (lispm-agent-steps agent))
       (lispm--send agent body betas 0))))))

(defun lispm--send (agent body betas attempt)
  "Send the request BODY with BETAS for AGENT; ATTEMPT counts retries."
  (lispm-set-state agent 'requesting
                   (if (> attempt 0) (format "retry %d" attempt) "thinking"))
  (lispm--live-begin agent)
  (let ((proc (funcall lispm-api-function (lispm-agent-backend* agent) body betas
                       :on-delta (lambda (kind text) (lispm--live-insert agent kind text))
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
    ("pause_turn" "The model paused; send any message to let it continue.")
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
     ((not (memq tool (lispm-agent-tools* agent)))
      (list "FAILED" 'fail (format "There is no tool named %s available to you." name)))
     (t
      (pcase (lispm-tool-action agent tool (alist-get 'input block))
        ('auto (list "RUNNING" 'run))
        ('ask (list "WAITING" 'ask))
        (_ (list "DENIED" 'deny
                 (format "Denied by policy: %s may not use %s tools."
                         (lispm-agent-name agent) (lispm-tool-permission tool)))))))))

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
        (lispm--finish-turn agent (lispm--stop-note stop message))))))

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
  "Run AGENT's pending tool call ID."
  (let ((call (gethash id (lispm-agent-pending agent))))
    (plist-put call :status 'run)
    (lispm--in-session agent
      (lispm-transcript-set-state (plist-get call :marker) "RUNNING"))
    (lispm-set-state agent 'acting (format "running %s" (plist-get call :name)))
    (let ((lispm-current-agent agent)
          (default-directory (lispm-agent-root agent)))
      (lispm-tool-invoke (plist-get call :tool) (plist-get call :input)
                         (lambda (text errorp &optional images)
                           (lispm--tool-done agent id text errorp nil images))))))

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
          (lispm-agent-waiting-on agent) nil)
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
      (lispm--finish-turn
       agent (format "The request failed%s: %s: %s"
                     (if (plist-get err :status) (format " (HTTP %s)" (plist-get err :status)) "")
                     (plist-get err :type) (plist-get err :message))))))

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
    (when (lispm-agent-busy-p agent)
      (user-error "%s is busy (%s); C-c C-k aborts"
                  (lispm-agent-name agent) (lispm-agent-state agent)))
    (setf (lispm-agent-buffer agent) (current-buffer))
    (let ((parsed (lispm-transcript-parse)))
      (when (plist-get parsed :pending)
        (user-error "Some tool calls have no result yet"))
      (unless (equal (plist-get parsed :last-role) "user")
        (user-error "Write your message under a \"* User\" heading first")))
    (lispm--begin-turn agent)))

(defun lispm-abort (&optional agent)
  "Stop AGENT's turn: cancel the request, the running and waiting calls,
and its queued inputs."
  (interactive)
  (let ((agent (if agent (lispm-get-agent agent) (lispm--buffer-agent))))
    (unless (lispm-agent-busy-p agent)
      (user-error "%s is idle" (lispm-agent-name agent)))
    (lispm--stop agent "Aborted by the user.")))

(defun lispm--stop (agent note)
  "Stop AGENT's turn, its calls and queued inputs, recording NOTE."
  (progn
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
    (lispm--finish-turn agent note)))

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
  "Approve or deny a call whose heading the user moved to a new TODO state.
This makes `org-todo', and the org agenda, an approval interface."
  (when-let* ((agent lispm-session-agent)
              (id (org-entry-get nil "TOOL_ID"))
              (call (gethash id (lispm-agent-pending agent))))
    (when (eq (plist-get call :status) 'ask)
      (pcase org-state
        ((or "APPROVED" "RUNNING" "DONE")
         (run-at-time 0 nil #'lispm-resolve agent id 'approve))
        ((or "DENIED" "FAILED")
         (run-at-time 0 nil #'lispm-resolve agent id 'deny))))))

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
  "Review the call at point, or the first awaiting approval, and decide."
  (interactive)
  (let* ((agent (lispm--buffer-agent))
         (id (or (lispm--call-at-point agent)
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
  "Approve the call at point, or the first one awaiting approval."
  (interactive)
  (let ((agent (lispm--buffer-agent)))
    (lispm-resolve agent (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))
                             (user-error "No call awaits approval"))
                   'approve)))

(defun lispm-deny (reason)
  "Deny the call at point, or the first one awaiting approval, for REASON."
  (interactive "sReason (optional): ")
  (let ((agent (lispm--buffer-agent)))
    (lispm-resolve agent (or (lispm--call-at-point agent) (car (lispm--waiting-calls agent))
                             (user-error "No call awaits approval"))
                   'deny reason)))

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

(defun lispm--summarize (agent)
  "Ask the model for a summary of AGENT's session, then continue from it."
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
                                                                 (append (alist-get 'content message) nil))))
                              :on-error (lambda (err)
                                          (lispm--compaction-failed
                                           agent (format "%s: %s" (plist-get err :type)
                                                         (plist-get err :message)))))))
          (when (processp proc) (process-put proc 'lispm-agent agent)))
      (error (lispm--compaction-failed agent (error-message-string err))))))

(defun lispm--continue (agent summary)
  "Carry AGENT's conversation on in a new session file holding SUMMARY."
  (when (lispm-agent-compacting agent)
    (if (string-blank-p summary)
        (lispm--compaction-failed agent "the summary came back empty")
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
        (lispm-set-state agent 'idle)
        (lispm--drain-inbox agent)))))

(defun lispm--compaction-failed (agent reason)
  "Give up handing AGENT's session over, for REASON, and carry on in it."
  (when (lispm-agent-compacting agent)
    (setf (lispm-agent-compacting agent) nil)
    (lispm-set-state agent 'idle)
    (lispm--record-event agent "compaction" (format "Compaction failed: %s" reason))
    ;; Do not try again before the input that asked for it.
    (when (lispm-agent-inbox agent)
      (setcar (lispm-agent-inbox agent)
              (plist-put (car (lispm-agent-inbox agent)) :no-compact t)))
    (message "LispM: compaction of %s failed: %s" (lispm-agent-name agent) reason)
    (lispm--drain-inbox agent)))

;;;; Journal: audit and rollback

(defun lispm--journal-entry (agent &optional turn)
  "Journal (TURN . SNAPSHOTS) of AGENT for TURN, or its latest one with edits."
  (or (if turn
          (assq turn (lispm-agent-journal agent))
        (seq-find #'cdr (lispm-agent-journal agent)))
      (user-error "%s made no edits%s" (lispm-agent-name agent)
                  (if turn (format " in turn %d" turn) ""))))

(defun lispm--snapshot-now (snapshot)
  "Current text of the buffer or file in SNAPSHOT."
  (let ((buffer (car snapshot))
        (file (plist-get (cdr snapshot) :file)))
    (cond ((buffer-live-p buffer) (lispm-tools--buffer-text buffer))
          (file (lispm--file-text file))
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
