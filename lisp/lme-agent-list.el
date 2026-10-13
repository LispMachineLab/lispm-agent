;;; lme-agent-list.el --- The agent session sidebar  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm-emacs
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; The agents view of the left sidebar: recorded and running Claude Code,
;; Codex and Hermes sessions, and those of the agents running inside Emacs
;; (`lme-agent-emacs-agents'), grouped by project, with per-session actions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lme-core)

(declare-function lme-agent-installed-p "lme-ai" (agent))

(defvar lme-agent-emacs-agents)
(declare-function lme-agent--emacs-agent "lme-ai" (agent))
(declare-function lme-agent--emacs-buffer-agent "lme-ai" (buffer))
(declare-function lme-sidebar--insert-entry "lme-sidebar"
                  (icon title meta action help &optional properties depth))
(declare-function lme-sidebar--insert-group "lme-sidebar" (open name meta action help))
(declare-function lme-sidebar--insert-columns "lme-sidebar" (name meta))
(declare-function lme-sidebar--count "lme-sidebar" (n singular &optional detail))
(defvar lme-platinum-info)
(declare-function lme-sidebar-set-toolbar "lme-sidebar"
                  (view refresh actions &optional title-actions))
(declare-function lme-sidebar--setup-buffer "lme-sidebar" ())
(declare-function lme-sidebar--display "lme-sidebar" (buffer))
(declare-function lme-sidebar--hide-buffer "lme-sidebar" (name))

;;;; Agent session sidebar

(defun lme-agent--directory-key (directory)
  "Return DIRECTORY expanded, without trailing slash, abbreviated for display."
  (abbreviate-file-name (directory-file-name (expand-file-name directory))))

(defconst lme-agent-chat-directories
  (list (expand-file-name "~") (expand-file-name "~/Documents/Codex/"))
  "Directories whose sessions are project-less chats rather than project work.
The home directory itself is a chat location; so is anything under the Codex
app's dated scratch tree.")

(defun lme-agent--chat-directory-p (directory)
  "Return non-nil when DIRECTORY holds project-less chat sessions."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (or (string= dir (car lme-agent-chat-directories))
        (string-prefix-p (cadr lme-agent-chat-directories) (file-name-as-directory dir)))))

(defun lme-agent--claude-worktree-parent (directory)
  "Return the repository owning DIRECTORY when it is a Claude Code worktree.
Claude Code places worktrees under REPO/.claude/worktrees/NAME."
  (when (string-match "\\`\\(.+?\\)/\\.claude/worktrees/[^/]+" directory)
    (match-string 1 directory)))

(defun lme-agent--codex-worktree-name (directory)
  "Return the repository name of DIRECTORY when it is a Codex worktree.
Codex places worktrees under ~/.codex/worktrees/ID/NAME, which does not name
the main checkout, so the caller matches NAME against known projects."
  (when (string-match "/\\.codex/worktrees/[^/]+/\\([^/]+\\)" directory)
    (match-string 1 directory)))

(defun lme-agent--git-main-root (directory)
  "Return the main checkout root of the git repository containing DIRECTORY.
A linked worktree's `.git' is a file naming MAIN/.git/worktrees/NAME; follow
it to MAIN.  Return nil when DIRECTORY is missing or outside any repository."
  (when-let* (((file-directory-p directory))
              (root (locate-dominating-file directory ".git")))
    (let ((dotgit (expand-file-name ".git" root)))
      (or (and (file-regular-p dotgit)
               (with-temp-buffer
                 (insert-file-contents dotgit)
                 (when (re-search-forward "^gitdir: \\(.+?\\)/\\.git/worktrees/" nil t)
                   (match-string 1))))
          root))))

(defvar lme-agent--root-cache (make-hash-table :test #'equal)
  "Memoized project roots keyed by working directory.")

(defun lme-agent--project-root (directory)
  "Return the project root of working DIRECTORY, or `chats' for chat sessions.
Claude worktrees resolve to their repository, git checkouts and their
subdirectories to the main checkout root; other directories stand alone."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (with-memoization (gethash dir lme-agent--root-cache)
      (if (lme-agent--chat-directory-p dir)
          'chats
        (let ((base (or (lme-agent--claude-worktree-parent dir) dir)))
          (directory-file-name (or (lme-agent--git-main-root base) base)))))))

(defun lme-agent--location-label (directory root)
  "Return where DIRECTORY sits inside project ROOT, or nil at the root itself.
Worktrees are named by their worktree, subdirectories by their relative path."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (cond ((or (eq root 'chats) (string-equal-ignore-case dir root)) nil)
          ((lme-agent--claude-worktree-parent dir)
           (concat "⎇ " (file-name-nondirectory dir)))
          ((lme-agent--codex-worktree-name dir) "⎇ codex worktree")
          ((string-prefix-p (file-name-as-directory root) (file-name-as-directory dir))
           (file-relative-name dir root))
          (t (abbreviate-file-name dir)))))

(defun lme-agent--resolve-roots (directories)
  "Return an alist mapping each of DIRECTORIES to its project root.
Roots come from `lme-agent--project-root'.  A Codex worktree that no longer
exists cannot be followed through git, so it joins the known project whose
directory name equals the worktree's repository name, when there is one."
  (let* ((roots (mapcar (lambda (dir) (cons dir (lme-agent--project-root dir)))
                        directories))
         (by-name (make-hash-table :test #'equal)))
    (pcase-dolist (`(,_ . ,root) roots)
      (when (and (stringp root) (not (lme-agent--codex-worktree-name root)))
        (unless (gethash (file-name-nondirectory root) by-name)
          (puthash (file-name-nondirectory root) root by-name))))
    (mapcar (pcase-lambda (`(,dir . ,root))
              (cons dir (or (and (stringp root)
                                 (when-let* ((name (lme-agent--codex-worktree-name root)))
                                   (gethash name by-name)))
                            root)))
            roots)))

(defun lme-agent--group-by-directory (sessions running)
  "Group SESSIONS and RUNNING terminals by project.
A project is the root from `lme-agent--resolve-roots', so worktrees and
subdirectories join their repository and chat locations share one `chats'
group.  Return a list of plists
  (:key KEY :dir DIR :chats CHATS :running BUFFERS :sessions SESSIONS :time T)
where KEY folds case (macOS paths are case-insensitive), DIR is the root for
display, and T is the latest activity.  Projects with running terminals come
first, then by latest activity, and the chats group last.  SESSIONS are
newest first and keep that order inside each group."
  (let* ((directory-of (lambda (item)
                         (if (bufferp item)
                             (buffer-local-value 'default-directory item)
                           (plist-get item :cwd))))
         (roots (lme-agent--resolve-roots
                 (delete-dups (mapcar directory-of (append running sessions)))))
         (groups (make-hash-table :test #'equal))
         (order nil))
    (cl-flet ((group (item)
                (let* ((root (cdr (assoc (funcall directory-of item) roots)))
                       (key (if (eq root 'chats) 'chats (downcase root))))
                  (or (gethash key groups)
                      (progn
                        (push key order)
                        (puthash key
                                 (list :key key
                                       :dir (if (eq root 'chats) "~"
                                              (lme-agent--directory-key root))
                                       :chats (eq root 'chats)
                                       :running nil :sessions nil :time 0)
                                 groups))))))
      (dolist (buffer running)
        (let ((group (group buffer)))
          (plist-put group :running (cons buffer (plist-get group :running)))))
      (dolist (session sessions)
        (let ((group (group session)))
          (plist-put group :sessions (cons session (plist-get group :sessions)))
          (plist-put group :time (max (plist-get group :time)
                                      (plist-get session :time))))))
    (sort (mapcar (lambda (key)
                    (let ((group (gethash key groups)))
                      (plist-put group :running (nreverse (plist-get group :running)))
                      (plist-put group :sessions (nreverse (plist-get group :sessions)))))
                  order)
          (lambda (a b)
            (let ((ca (plist-get a :chats)) (cb (plist-get b :chats))
                  (ra (and (plist-get a :running) t))
                  (rb (and (plist-get b :running) t)))
              (cond ((not (eq ca cb)) cb)
                    ((not (eq ra rb)) ra)
                    (t (> (plist-get a :time) (plist-get b :time)))))))))

(defun lme-agent--running-buffers ()
  "Return the live agent terminals, most recently used first."
  (seq-filter (lambda (buffer)
                (and (lme-agent--buffer-p buffer) (lme-agent--running-p buffer)))
              (buffer-list)))

(defun lme-agent--running-ids (buffers)
  "Return the ids of the sessions agent terminal BUFFERS run, where known.
Ids are compared whole: Codex ids are time-ordered, so sessions begun within
about a minute of each other share their leading characters."
  (delq nil (mapcar (lambda (buffer)
                      (buffer-local-value 'lme-agent-buffer-session-id buffer))
                    buffers)))

(defun lme-agent--session-buffer (id)
  "Return the running agent terminal of the session with ID, or nil."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'lme-agent-buffer-session-id buffer) id))
            (lme-agent--running-buffers)))

(defface lme-agent-claude-icon '((t :foreground "#D97757"))
  "Face of Claude's mark: its terra cotta.
A named face, as tab lines inherit an icon's face by name."
  :group 'convenience)

(defconst lme-agent-icon-directory
  (expand-file-name "icons" lme-directory)
  "Directory of the image icons named in `lme-agent-icons'.")

(defun lme-agent-image-icon (file &rest args)
  "Return FILE, an image in `lme-agent-icon-directory', as a one-line icon.
ARGS are the nerd-icons keyword arguments: :height scales the icon relative
to the frame's line height, :face and :v-adjust are accepted for the sake of
callers that pass them to every icon function.  Where images cannot be
shown, a feather stands in for the image."
  (require 'nerd-icons)
  (let ((path (expand-file-name file lme-agent-icon-directory)))
    (if (and (display-images-p) (file-readable-p path))
        (propertize " " 'display
                    (create-image path nil nil
                                  :height (round (* (or (plist-get args :height) 1.0)
                                                    (frame-char-height)))
                                  :ascent 'center))
      (apply #'nerd-icons-mdicon "nf-md-feather" args))))

(defconst lme-agent-icons
  '((claude nerd-icons-codicon "nf-cod-claude" lme-agent-claude-icon)
    (codex nerd-icons-codicon "nf-cod-openai" nil)
    (hermes lme-agent-image-icon "hermes.png" nil))
  "Icon function, icon and face of each agent: Claude's mark in its terra
cotta; OpenAI's mark, the maker of Codex, in the surrounding text's colour;
and the Hermes desktop app's icon, the girl with the N choker, as an image.")

(defun lme-agent--icon-spec (agent)
  "Return (FUNCTION ICON FACE) of AGENT's icon, as in `lme-agent-icons'.
An agent running inside Emacs names its own, see `lme-agent-emacs-agents'."
  (or (alist-get agent lme-agent-icons)
      (plist-get (lme-agent--emacs-agent agent) :icon)))

(defun lme-agent--icon (agent)
  "Return the brand icon of AGENT."
  (require 'nerd-icons)
  (pcase-let* ((`(,function ,name ,face) (lme-agent--icon-spec agent))
               (icon (copy-sequence (funcall function name))))
    ;; Layer the colour under the icon's own face so its Nerd Font family
    ;; survives; replacing the face would fall back to an arbitrary CJK font.
    (when face
      (add-face-text-property 0 (length icon) face t icon))
    icon))

(defun lme-agent--buffer-agent (buffer)
  "Return the agent running in agent BUFFER.
A terminal is named by its agent; an in-Emacs agent recognizes its own."
  (let ((name (buffer-name buffer)))
    (cond ((lme-agent--emacs-buffer-agent buffer))
          ((string-match "\\`\\*\\([a-z]+\\):" name)
           (intern (match-string 1 name)))
          (t 'claude))))

(defvar-local lme-agent-list--folds nil
  "Alist of directory keys to an explicit folded state chosen by the user.
Directories absent from it use `lme-agent-list--default-folded-p'.")

(defvar-local lme-agent-list--expanded nil
  "Directory keys whose groups list every session instead of a preview.")

(defun lme-agent-list--default-folded-p (group now)
  "Return non-nil when GROUP starts folded at time NOW.
Groups without running terminals and idle for `lme-agent-list-fold-age' fold."
  (and (null (plist-get group :running))
       (> (- now (plist-get group :time)) lme-agent-list-fold-age)))

(defun lme-agent-list--folded-p (group now)
  "Return non-nil when GROUP is folded at time NOW."
  (let ((choice (assoc (plist-get group :key) lme-agent-list--folds)))
    (if choice (cdr choice) (lme-agent-list--default-folded-p group now))))

(defun lme-agent-list--toggle-fold (group)
  "Fold GROUP when it is unfolded and unfold it otherwise, then re-render."
  (setq lme-agent-list--folds
        (cons (cons (plist-get group :key)
                    (not (lme-agent-list--folded-p group (float-time))))
              (assoc-delete-all (plist-get group :key) lme-agent-list--folds)))
  (lme-agent-list-refresh))

(defun lme-agent-list--toggle-expanded (directory)
  "List every session of DIRECTORY's group, or only a preview again."
  (setq lme-agent-list--expanded
        (if (member directory lme-agent-list--expanded)
            (delete directory lme-agent-list--expanded)
          (cons directory lme-agent-list--expanded)))
  (lme-agent-list-refresh))

(defun lme-agent-list--insert-project (group now running-ids)
  "Insert project GROUP, its running terminals and a preview of its sessions.
NOW dates the sessions; RUNNING-IDS are the ids of sessions live terminals
run.  The project is a row with a disclosure triangle, as a folder in a
Finder list; its sessions are rows one level in, newest first."
  (let* ((folded (lme-agent-list--folded-p group now))
         (directory (plist-get group :dir))
         (chats (plist-get group :chats))
         (sessions (plist-get group :sessions)))
    (lme-sidebar--insert-group
     (not folded)
     (if chats "Chats" (file-name-nondirectory directory))
     (lme-agent--age (plist-get group :time) now)
     (apply-partially #'lme-agent-list--toggle-fold group)
     (format "%s %s" (if folded "Open" "Close")
             (if chats "project-less conversations" directory)))
    (unless folded
      ;; A terminal whose session is already listed shows as that session's
      ;; running row; only terminals without a recorded session list here.
      (dolist (buffer (seq-remove
                       (lambda (buffer)
                         (when-let* ((id (buffer-local-value 'lme-agent-buffer-session-id buffer)))
                           (seq-some (lambda (session) (equal (plist-get session :id) id))
                                     sessions)))
                       (plist-get group :running)))
        (lme-sidebar--insert-entry
         (lme-agent--icon (lme-agent--buffer-agent buffer))
         (lme-agent-buffer-label buffer)
         (propertize "running" 'face 'success)
         (apply-partially #'lme-agent--show buffer)
         "Show this terminal"
         nil 1))
      (let* ((expanded (member (plist-get group :key) lme-agent-list--expanded))
             (shown (if expanded sessions
                      (seq-take sessions lme-agent-list-project-preview))))
        (dolist (session shown)
          (let* ((id (plist-get session :id))
                 (archived (lme-agent--archived-p session))
                 (location (lme-agent--location-label
                            (plist-get session :cwd)
                            (if chats 'chats (expand-file-name directory)))))
            (lme-sidebar--insert-entry
             (lme-agent--icon (plist-get session :agent))
             (if archived
                 (propertize (plist-get session :title) 'face '(:inherit (shadow lme-platinum-list)))
               (plist-get session :title))
             (if (member id running-ids)
                 (propertize "running" 'face 'success)
               (lme-agent--age (plist-get session :time) now))
             (apply-partially #'lme-agent-resume-session session)
             (format "Resume %s session %s%s%s (right-click for more)"
                     (plist-get session :agent) id
                     (if location (format ", in %s" location) "")
                     (if archived ", archived" ""))
             (list 'lme-agent-session session 'keymap lme-agent-list-entry-map)
             1)))
        (when (or expanded (> (length sessions) (length shown)))
          (lme-sidebar--insert-entry
           ""
           (propertize (if expanded "Fewer"
                         (format "%d more…" (- (length sessions) (length shown))))
                       'face '(:inherit (shadow lme-platinum-list)))
           nil
           (apply-partially #'lme-agent-list--toggle-expanded (plist-get group :key))
           "Toggle the full session list of this project"
           nil 1))))))

(defvar-local lme-agent-list--show-archived nil
  "Non-nil when this sidebar lists archived sessions too.")

(defun lme-agent-list-toggle-show-archived ()
  "Show archived sessions in the sidebar, or hide them again."
  (interactive)
  (with-current-buffer lme-agent-list-buffer-name
    (setq lme-agent-list--show-archived (not lme-agent-list--show-archived)))
  (lme-agent-list-refresh))

(defun lme-agent-list--render ()
  "Render the agent sidebar, grouped by working directory, into this buffer.
Archived sessions are left out unless `lme-agent-list--show-archived'."
  (let* ((inhibit-read-only t)
         (now (float-time))
         (all (lme-agent-sessions-list))
         ;; Terminals claim their sessions first, so a session a new
         ;; terminal just claimed already counts as running below.
         (_ (lme-agent--refresh-buffer-titles all))
         (running (lme-agent--running-buffers))
         (running-ids (lme-agent--running-ids running))
         (archived (seq-count #'lme-agent--archived-p all))
         (sessions (if lme-agent-list--show-archived all
                     (seq-remove #'lme-agent--archived-p all))))
    (require 'nerd-icons)
    (erase-buffer)
    (lme-sidebar-set-toolbar
     'agents #'lme-agent-list-refresh
     ;; A new-session button per agent: the CLIs that are installed, then the
     ;; agents running inside Emacs.
     (append
      (mapcan (pcase-lambda (`(,agent ,command ,help))
                (when (lme-agent-installed-p agent)
                  (list (list (concat (lme-agent--icon agent) "+") command help))))
              '((claude lme-agent-claude-new "New Claude Code session in the current project")
                (codex lme-agent-codex-new "New Codex session in the current project")
                (hermes lme-agent-hermes-new "New Hermes session in the current project")))
      (mapcar (pcase-lambda (`(,agent . ,props))
                (list (concat (lme-agent--icon agent) "+") (plist-get props :command)
                      (format "New %s session in the current project"
                              (plist-get props :name))))
              lme-agent-emacs-agents))
     ;; The archive toggle shows the archived count, pressed while archived
     ;; sessions are listed.
     (when (> archived 0)
       `((,(format "%d Archived" archived)
          lme-agent-list-toggle-show-archived
          ,(if lme-agent-list--show-archived "Hide archived sessions"
             "Show archived sessions")
          ,lme-agent-list--show-archived))))
    (setq lme-platinum-info
          (lme-sidebar--count (length sessions) "session"
                              (when running (format "%d running" (length running)))))
    (lme-sidebar--insert-columns "Name" "Modified")
    (dolist (group (lme-agent--group-by-directory sessions running))
      (lme-agent-list--insert-project group now running-ids))))

(defun lme-agent-list-refresh ()
  "Re-render the agent sidebar, keeping point on the same line."
  (interactive)
  (when-let* ((buffer (get-buffer lme-agent-list-buffer-name)))
    (with-current-buffer buffer
      (let ((line (line-number-at-pos))
            (window (get-buffer-window buffer)))
        (lme-agent-list--render)
        (goto-char (point-min))
        (forward-line (1- line))
        (when window (set-window-point window (point)))))))

(defun lme-agent-list-refresh-on-exit (_process)
  "Refresh the agent sidebar after an Eat terminal's process exits."
  (lme-agent-list-refresh))

(defvar-keymap lme-agent-list-mode-map
  :doc "Keymap for the agent sidebar."
  :parent special-mode-map
  "g" #'lme-agent-list-refresh
  "c" #'lme-agent-claude-new
  "x" #'lme-agent-codex-new
  "h" #'lme-agent-hermes-new
  "r" #'lme-agent-list-rename
  "a" #'lme-agent-list-toggle-archive
  "A" #'lme-agent-list-toggle-show-archived
  "D" #'lme-agent-list-delete
  "w" #'lme-agent-list-copy-id
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lme-agent-list-mode special-mode "Agents"
  "Sidebar listing running and recorded Claude Code, Codex and Hermes sessions.
\\{lme-agent-list-mode-map}"
  (lme-sidebar--setup-buffer))

(defun lme-agent-list-show ()
  "Show the agent sidebar on the left of this frame and return its window."
  (let ((buffer (get-buffer-create lme-agent-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lme-agent-list-mode)
        (lme-agent-list-mode))
      (lme-agent-list--render)
      (goto-char (point-min)))
    (lme-sidebar--display buffer)))

(defun lme-agent-list-visible-p ()
  "Return non-nil when the agent sidebar is shown in the selected frame."
  (and (get-buffer-window lme-agent-list-buffer-name) t))

(defun lme-agent-list-hide ()
  "Hide the agent sidebar in the selected frame."
  (lme-sidebar--hide-buffer lme-agent-list-buffer-name))


(provide 'lme-agent-list)
;;; lme-agent-list.el ends here
