;;; lispm-agents.el --- Coding agents, in terminals and in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-agent
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; Coding agents run as their native CLIs inside terminals, or inside
;; Emacs: the editor's agents.  Two come with this package, Claude Code
;; (lispm-agents-claude.el) and Codex (lispm-agents-codex.el); others
;; are packages of their own, as Hermes (LispMachineLab/lispm-hermes).
;; Every agent tells `lispm-agents-define' how to list, start, resume,
;; rename and delete its sessions, and then takes part in all this module
;; adds around it:
;;
;; - a unified list of every recorded session, most recently opened first,
;;   resuming the chosen one in its original working directory
;;   (`lispm-agents-sessions');
;; - commands that start a fresh agent at the current project root, in
;;   `lispm-agents-map', which you bind where you like, as under C-c a;
;; - sending the active region (with its location) to a running agent;
;; - a server per terminal showing the agent Emacs's state
;;   (`lispm-agents-context');
;; - what an interface around them needs: sessions grouped by project,
;;   running terminals, archiving, icons.
;;
;; The agents know no interface but Emacs's.  A program drawing one
;; around them says how through the variables of the section Interface
;; below: the terminals agents run in (Eat's by
;; default), how an agent's buffer is shown, which window is the main
;; one, how a deletion is confirmed (by default with LispM UI's
;; `lispm-ui-confirm', which such a program may draw).
;;
;; An agent run as a CLI takes no part anywhere while its program is not
;; on PATH: no menu, button or key offers it and none of its sessions are
;; listed.
;;
;; A session is a plist (:agent :id :cwd :title :time :file).  Agents that
;; keep JSONL transcripts read them with `lispm-agents-cached-session',
;; which memoizes the parse per (file . mtime), so reopening the list
;; rereads only files that changed since the last listing:
;;
;;   transcript file --(head/tail bytes)--> JSON objects --(parser)--> session

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-agents-core)
(require 'lispm-agents-sessions)
(require 'lispm-agents-terminals)

(autoload 'lispm-agents-open-project "lispm-agents-work"
  "Show already active agents belonging to DIRECTORY's project." t)

(declare-function project-known-project-roots "project" ())
(defvar project-current-directory-override)
(defvar embark-keymap-alist)
(defvar lispm-chat-agent)
(defvar lispm-session-agent)

;;;; Commands

(defvar lispm-agents--candidates nil
  "Session plists of the last session listing, keyed by their labels.")

(defun lispm-agents--session-at (label)
  "Return the session plist listed under LABEL."
  (or (and lispm-agents--candidates (gethash label lispm-agents--candidates))
      (user-error (lispm-tr "Unknown agent session: %s") label)))

(defun lispm-agents--read-session (prompt)
  "Read a recorded agent session with PROMPT and return its plist."
  (let* ((now (float-time))
         (table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (session)
                           (let ((label (lispm-agents-session-label session now)))
                             (puthash label session table)
                             label))
                         (lispm-agents-sessions-list))))
    (setq lispm-agents--candidates table)
    (lispm-agents--session-at
     (completing-read prompt
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata (category . lispm-agents-session)
                                       (display-sort-function . identity))
                          (complete-with-action action labels string pred)))
                      nil t))))

(defun lispm-agents-session-resumable-p (session)
  "Return non-nil when SESSION can be resumed, as its agent's :resumable
says, see `lispm-agents-define'; every session can without one."
  (if-let* ((resumable (plist-get (lispm-agents-props (plist-get session :agent)) :resumable)))
      (funcall resumable session)
    t))

(defun lispm-agents-session-model-resumable-p (session)
  "Return non-nil when SESSION's CLI can resume with a chosen model.
Its agent must supply :model-arguments and :resume-arguments, see
`lispm-agents-define'.  This does not check whether SESSION is running."
  (let ((props (lispm-agents-props (plist-get session :agent))))
    (and (plist-get props :program)
         (plist-get props :resume-arguments)
         (plist-get props :model-arguments)
         (not (plist-get props :resume))
         (lispm-agents-session-resumable-p session))))

(defun lispm-agents-resume-session (session &optional model)
  "Resume SESSION in its original working directory.
A session already running in a terminal is shown there instead: resuming it
again would run a second agent on the same session.  One that cannot be
resumed, see `lispm-agents-session-resumable-p', is opened as its
transcript.
With MODEL, resume with that model using the agent's :model-arguments.
MODEL must not be blank; a running session or one that cannot resume with
a model signals a user error instead."
  (let ((agent (plist-get session :agent))
        (id (plist-get session :id))
        (cwd (plist-get session :cwd)))
    (when model
      (setq model (string-trim model))
      (when (string-empty-p model)
        (user-error (lispm-tr "Model must not be empty")))
      (unless (lispm-agents-session-model-resumable-p session)
        (user-error (lispm-tr "This session cannot be resumed with a different model")))
      (when (lispm-agents-session-buffer id)
        (user-error (lispm-tr "Session is running; exit its terminal first"))))
    (lispm-agents-show
     (or (lispm-agents-session-buffer id)
         (unless (lispm-agents-session-resumable-p session)
           (lispm-agents--in-main-window #'find-file (lispm-agents--session-transcript session)))
         (let ((props (lispm-agents-check-available agent)))
           (if-let* ((resume (plist-get props :resume)))
               (let ((time (float-time)))
                 (lispm-agents-opened-buffer agent (funcall resume session) time id))
             (lispm-agents-launch agent cwd
                                   (format "%s:%s:%s" agent
                                           (file-name-nondirectory (directory-file-name cwd))
                                           (lispm-agents-short-id session))
                                   (append (funcall (plist-get props :resume-arguments) id)
                                           (when model
                                             (funcall (plist-get props :model-arguments) model)))
                                   id
                                   (plist-get session :title))))))))

(defvar lispm-agents-model-history nil
  "Model names entered when resuming agent sessions.")

(defun lispm-agents-resume-session-with-model (session)
  "Prompt for a model name and resume SESSION with it."
  (lispm-agents-resume-session
   session (read-string (lispm-tr "Resume with model: ") nil 'lispm-agents-model-history)))

(defun lispm-agents-sessions ()
  "Choose a recorded session of any agent and resume it."
  (interactive)
  (lispm-agents-resume-session (lispm-agents--read-session (lispm-tr "Agent session: "))))

(defun lispm-agents-new (agent &optional directory)
  "Start a new AGENT session in DIRECTORY.
When DIRECTORY is nil, ask for it with `lispm-agents-read-directory'
\(defaulting to `lispm-agents-default-directory' in batch mode)."
  (let* ((props (lispm-agents-check-available agent))
         (directory (file-name-as-directory
                     (expand-file-name
                      (or directory
                          (if (and noninteractive (not lispm-agents-read-directory-function))
                              (lispm-agents-default-directory)
                            (lispm-agents-read-directory agent)))))))
    (lispm-agents-show
     (if-let* ((new (plist-get props :new)))
         (let ((time (float-time)))
           (lispm-agents-opened-buffer agent (funcall new directory) time))
       (pcase-let ((`(,id . ,args)
                    (when-let* ((new-arguments (plist-get props :new-arguments)))
                      (let ((default-directory directory)
                            (project-current-directory-override directory))
                        (if (memq (cdr (func-arity new-arguments)) '(0))
                            (funcall new-arguments)
                          (funcall new-arguments directory))))))
         (lispm-agents-launch agent directory (lispm-agents-fresh-name agent directory)
                               args id
                               (lispm-tr "New %s · %s" (plist-get props :name)
                                         (file-name-nondirectory (directory-file-name directory)))))))))

(defun lispm-agents--pick-terminal ()
  "Return the agent buffer to send to, preferring the current project:
one whose agent can be talked to, idle or not, see `lispm-agents-alive-p'.
Ask when several qualify."
  (let* ((root (lispm-agents-project-directory))
         (running (lispm-agents-live-buffers))
         (local (seq-filter (lambda (buffer)
                              (file-in-directory-p
                               (buffer-local-value 'default-directory buffer) root))
                            running))
         (candidates (or local running)))
    (pcase candidates
      ('nil (if-let* ((keys (delq nil (mapcar (lambda (entry)
                                                 (when-let* ((key (plist-get (cdr entry) :key)))
                                                   (concat "C-c a " key)))
                                               (lispm-agents-available)))))
                (user-error (lispm-tr "No running agent; start one with %s")
                            (string-join keys ", "))
              (user-error (lispm-tr "No running agent; start one first"))))
      (`(,only) only)
      (_ (get-buffer (completing-read (lispm-tr "Send to agent: ")
                                      (mapcar #'buffer-name candidates) nil t))))))

(defun lispm-agents--region-reference (beg end)
  "Return a fenced reference to the text between BEG and END of this buffer.
The header names the file relative to the project root and the line range."
  (let* ((file (if buffer-file-name
                   (file-relative-name buffer-file-name (lispm-agents-project-directory))
                 (buffer-name)))
         (first (line-number-at-pos beg))
         (last (line-number-at-pos (if (and (> end beg) (= (char-before end) ?\n))
                                       (1- end)
                                     end)))
         (lang (string-remove-suffix
                "-ts" (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (format "%s:%d-%d\n```%s\n%s\n```\n"
            file first last lang
            (string-trim-right (buffer-substring-no-properties beg end)))))

(defun lispm-agents-send-region (beg end)
  "Send the region between BEG and END, with its location, to an agent terminal.
Without an active region, send a reference to the current file and line.
The text is pasted into the agent's composer without submitting it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((text (if beg
                  (lispm-agents--region-reference beg end)
                (format "%s:%d "
                        (if buffer-file-name
                            (file-relative-name buffer-file-name
                                                (lispm-agents-project-directory))
                          (buffer-name))
                        (line-number-at-pos))))
        (target (lispm-agents--pick-terminal)))
    (deactivate-mark)
    (if-let* ((send (plist-get (lispm-agents-props (lispm-agents-emacs-buffer-agent target))
                               :send)))
        (funcall send target text)
      (lispm-agents-terminal-paste target text))
    (pop-to-buffer target)))


(defun lispm-agents--rename (session title)
  "Rename SESSION to TITLE the way its own agent records names, see the
:rename of `lispm-agents-define'.  This list then shows TITLE."
  (let ((agent (plist-get session :agent)))
    (funcall (or (plist-get (lispm-agents-props agent) :rename)
                 (user-error (lispm-tr "%s sessions cannot be renamed") (lispm-agents-name agent)))
             session title)))

(defun lispm-agents--session-files (session)
  "Return every file and directory recording SESSION, as its agent's :files
says; none for an agent keeping its sessions in a store of its own."
  (when-let* ((files (plist-get (lispm-agents-props (plist-get session :agent)) :files)))
    (funcall files session)))

(defun lispm-agents--session-transcript (session)
  "Return a file holding SESSION's transcript: what its agent's :transcript
makes, else its :file."
  (if-let* ((transcript (plist-get (lispm-agents-props (plist-get session :agent)) :transcript)))
      (funcall transcript session)
    (plist-get session :file)))

(defun lispm-agents--session-running-p (session)
  "Return non-nil when SESSION is held by a live agent buffer, its agent
running or idle, see `lispm-agents-session-buffer'."
  (lispm-agents-session-buffer (plist-get session :id)))

(defun lispm-agents-rename-session (session)
  "Prompt for a new title of SESSION and rename it."
  (let ((title (string-trim (read-string (lispm-tr "Rename session: ")
                                         (plist-get session :title)))))
    (when (string-empty-p title)
      (user-error (lispm-tr "Title must not be empty")))
    (lispm-agents--rename session title)
    (run-hooks 'lispm-agents-sessions-changed-hook)))

(defun lispm-agents-toggle-archive-session (session)
  "Archive SESSION's subtree, or undo its own explicit archive choice.
A session inheriting its archive state must be restored through its parent."
  (let* ((key (lispm-agents-session-key session))
         (explicit (member key (lispm-agents-archived-keys))))
    (when (remove key (lispm-agents-archived-ancestors session))
      (user-error (lispm-tr "This session is archived with its parent; unarchive the parent first")))
    (lispm-agents-set-archived session (not explicit)))
  (run-hooks 'lispm-agents-sessions-changed-hook))

(defun lispm-agents-user-wrote (agent id)
  "Say the user wrote to the session ID of AGENT.
A session the user writes to again is in use, so an archived one leaves
the archive along with any archived ancestors hiding it.  Independently
archived siblings stay archived.  One merely looked at stays archived.
Return non-nil when archive choices changed."
  (let ((session (list :agent agent :id id)))
    (when-let* ((id)
                (keys (lispm-agents-archived-ancestors session)))
      (lispm-agents-save-archived
       (seq-difference (lispm-agents-archived-keys) keys #'equal))
      (run-hooks 'lispm-agents-sessions-changed-hook)
      t)))

(defun lispm-agents-delete-session (session)
  "Delete SESSION after confirmation.
The files recording it go to the system trash; an agent keeping its
sessions in a store of its own deletes it there itself, see the :delete of
`lispm-agents-define'.  A session that is running in a terminal
cannot be deleted.  `lispm-agents-confirm-function' asks."
  (when (lispm-agents--session-running-p session)
    (user-error (lispm-tr "Session is running; exit its terminal first")))
  (let* ((agent (plist-get session :agent))
         (name (lispm-agents-name agent))
         (delete (plist-get (lispm-agents-props agent) :delete))
         (files (unless delete (lispm-agents--session-files session)))
         (title (lispm-agents-one-line (plist-get session :title) 40)))
    (when (funcall lispm-agents-confirm-function
                   (if delete
                       (lispm-tr "Delete the %s session “%s”?" name title)
                     (lispm-tr "Move the %s session “%s” to the Trash?" name title))
                   (if delete
                       (lispm-tr "%s removes it from its store; this cannot be undone." name)
                     (lispm-tr-n "Its %d file goes to the Trash." "Its %d files go to the Trash."
                                 (length files)))
                   (if delete (lispm-tr "Delete") (lispm-tr "Move to Trash")))
      (if delete
          (funcall delete session)
        (dolist (file files)
          (move-file-to-trash file)
          (remhash file lispm-agents-session-cache)))
      (lispm-agents-set-archived session nil)
      (run-hooks 'lispm-agents-sessions-changed-hook))))

(defun lispm-agents--in-main-window (function &rest args)
  "Call FUNCTION with ARGS in the main window, see
`lispm-agents-main-window-function'."
  (select-window (funcall lispm-agents-main-window-function))
  (apply function args))

(defun lispm-agents-session-act (action session)
  "Perform the context menu ACTION on SESSION."
  (pcase action
    ('resume (lispm-agents-resume-session session))
    ('resume-with (lispm-agents-resume-session-with-model session))
    ('rename (lispm-agents-rename-session session))
    ('archive (lispm-agents-toggle-archive-session session))
    ('delete (lispm-agents-delete-session session))
    ('copy (kill-new (plist-get session :id))
           (message (lispm-tr "Copied %s") (plist-get session :id)))
    ('transcript (lispm-agents--in-main-window #'find-file (lispm-agents--session-transcript session)))
    ('directory (lispm-agents--in-main-window #'dired (plist-get session :cwd)))))

;;;; Embark actions on listed sessions

(defun lispm-agents-session-copy-id (label)
  "Copy the id of the agent session listed under LABEL."
  (lispm-agents-session-act 'copy (lispm-agents--session-at label)))

(defun lispm-agents-session-open-transcript (label)
  "Open the transcript of the agent session listed under LABEL."
  (lispm-agents-session-act 'transcript (lispm-agents--session-at label)))

(defun lispm-agents-session-open-directory (label)
  "Open the working directory of the agent session listed under LABEL."
  (lispm-agents-session-act 'directory (lispm-agents--session-at label)))

(defun lispm-agents-session-resume (label)
  "Resume the agent session listed under LABEL."
  (lispm-agents-session-act 'resume (lispm-agents--session-at label)))

(defvar-keymap lispm-agents-session-embark-map
  :doc "Embark actions for agent sessions in the session list."
  "RET" #'lispm-agents-session-resume
  "w" #'lispm-agents-session-copy-id
  "f" #'lispm-agents-session-open-transcript
  "d" #'lispm-agents-session-open-directory)


(keymap-set lispm-agents-map "s" #'lispm-agents-sessions)
(keymap-set lispm-agents-map "r" #'lispm-agents-send-region)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(lispm-agents-session . lispm-agents-session-embark-map)))

(provide 'lispm-agents)

;;; lispm-agents.el ends here
