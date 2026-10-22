;;; lispm-agents-claude.el --- Claude Code, an agent of the editor  -*- lexical-binding: t; -*-

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

;; Claude Code as one of the editor's agents, built in: it takes part
;; while `claude' is on PATH (`lispm-agents-define').
;;
;; - Its sessions are the JSONL transcripts under ~/.claude/projects/, a
;;   directory per project, titled by the custom title Claude keeps beside
;;   each, else its summary, else the first prompt.
;; - It is given its session's id from the start, so its terminal knows
;;   the session at once.
;; - It sees Emacs's state through its IDE protocol, lispm-agents-ide.el.
;; - It looks light or dark as Emacs does, unless its user chose a theme.
;; - Its worktrees, REPO/.claude/worktrees/NAME, join REPO's project.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-agents)
(require 'lispm-agents-ide)

(defface lispm-agents-claude-icon '((t :foreground "#D97757"))
  "Face of Claude's mark: its terra cotta.
A named face, as tab lines inherit an icon's face by name."
  :group 'convenience)

(defconst lispm-agents-claude-projects-directory
  (expand-file-name "~/.claude/projects/")
  "Directory where Claude Code stores one subdirectory of transcripts per project.")

;;;; Sessions

(defun lispm-agents-claude--session (file objects)
  "Return the Claude session recorded in FILE given its head/tail OBJECTS.
The title is the last custom title or summary, else the first human prompt;
a custom title file overrides it later, see
`lispm-agents-claude--titled'.  Return nil for a transcript without
any prompt or title."
  (let* ((all (append (car objects) (cdr objects)))
         (title (seq-some (lambda (object)
                            (or (plist-get object :customTitle)
                                (and (equal (plist-get object :type) "summary")
                                     (plist-get object :summary))))
                          (reverse all)))
         (prompt (seq-some (lambda (object)
                             (and (equal (plist-get object :type) "user")
                                  (not (plist-get object :isMeta))
                                  (let ((text (lispm-agents-content-text
                                               (plist-get (plist-get object :message)
                                                          :content))))
                                    (and (lispm-agents-human-text-p text) text))))
                           (car objects)))
         (cwd (seq-some (lambda (object) (plist-get object :cwd)) all)))
    (when (and cwd (or title prompt))
      (list :agent 'claude
            :id (file-name-base file)
            :cwd cwd
            :title (or title prompt)
            :file file))))

(defun lispm-agents-claude--json-string (file key)
  "Return the string KEY holds in the JSON object of FILE, or nil when FILE
cannot be read or KEY holds no string, or a blank one."
  (when (file-readable-p file)
    (when-let* ((value (ignore-errors
                         (with-temp-buffer
                           (insert-file-contents file)
                           (plist-get (json-parse-buffer :object-type 'plist) key))))
                ((stringp value))
                ((not (string-blank-p value))))
      value)))

(defun lispm-agents-claude--title-file (session)
  "Return the file holding Claude SESSION's custom title.
Claude Code keeps a session's chosen name beside its transcript, in the
session's own directory, rather than in the transcript, which it rewrites."
  (expand-file-name "custom-title.json"
                    (expand-file-name (plist-get session :id)
                                      (file-name-directory (plist-get session :file)))))

(defun lispm-agents-claude--custom-title (session)
  "Return the custom title chosen for Claude SESSION, or nil when none is."
  (lispm-agents-claude--json-string (lispm-agents-claude--title-file session)
                                   :customTitle))

(defun lispm-agents-claude--titled (session)
  "Return SESSION titled by its custom title, when one was chosen.
The title file changes without the transcript, so it is no part of the
transcript memo and is read after it; SESSION, shared with the memo, is
not modified."
  (when session
    (if-let* ((title (lispm-agents-claude--custom-title session)))
        (plist-put (copy-sequence session) :title title)
      session)))

(defun lispm-agents-claude--files ()
  "Return every top-level Claude Code transcript file."
  (when (file-directory-p lispm-agents-claude-projects-directory)
    (file-expand-wildcards
     (expand-file-name "*/*.jsonl" lispm-agents-claude-projects-directory))))

(defun lispm-agents-claude-sessions ()
  "Return every recorded Claude Code session."
  (lispm-agents-file-sessions (lispm-agents-claude--files)
                              #'lispm-agents-claude--session
                              #'lispm-agents-claude--titled))

(defun lispm-agents-claude--rename (session title)
  "Give Claude SESSION the TITLE, in its custom title file.
A record appended to the transcript itself would be lost when a running
Claude rewrites it."
  (let ((file (lispm-agents-claude--title-file session))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (decode-coding-string (json-serialize (list :customTitle title)) 'utf-8)
                  nil file nil 'silent)))

(defun lispm-agents-claude--session-files (session)
  "Return the files recording Claude SESSION: its transcript, and the
sibling directory of its subagents' transcripts when there is one."
  (let* ((file (plist-get session :file))
         (sibling (file-name-sans-extension file)))
    (if (file-directory-p sibling) (list file sibling) (list file))))

(defun lispm-agents-claude--worktree (directory)
  "Return where DIRECTORY belongs when it is in a Claude Code worktree.
Claude Code places worktrees under REPO/.claude/worktrees/NAME."
  (when (string-match "\\`\\(.+?\\)/\\.claude/worktrees/[^/]+" directory)
    (list :root (match-string 1 directory)
          :label (concat "⎇ " (file-name-nondirectory (directory-file-name directory))))))

;;;; Running it

(defun lispm-agents-claude--chosen-theme (directory)
  "Return the theme the user chose for Claude Code run in DIRECTORY, or nil.
It is chosen in Claude's user, project or local settings, or in its global
configuration of old, where the default \"dark\" means none was chosen."
  (let ((theme (lambda (file) (lispm-agents-claude--json-string file :theme))))
    (or (seq-some theme (list (expand-file-name "~/.claude/settings.json")
                              (expand-file-name ".claude/settings.json" directory)
                              (expand-file-name ".claude/settings.local.json" directory)))
        (let ((legacy (funcall theme (expand-file-name "~/.claude.json"))))
          (unless (equal legacy "dark") legacy)))))

(defun lispm-agents-claude--arguments (directory)
  "Return the arguments that make Claude Code, run in DIRECTORY, look light
or dark as Emacs does.  A terminal answering a program that asks its
colours with Emacs's lets Claude follow them; Claude asks only with
its theme \"auto\", which it is given unless its user chose a theme: its
default is dark whatever the terminal."
  (unless (lispm-agents-claude--chosen-theme directory)
    (list "--settings" "{\"theme\":\"auto\"}")))

(defun lispm-agents-claude--new-arguments ()
  "Return a new session's id and the arguments starting it: Claude takes
the id it is given, so its terminal knows its session from the start."
  (let ((id (lispm-agents-uuid)))
    (cons id (list "--session-id" id))))

(defun lispm-agents-claude-new ()
  "Start a new Claude Code session at the current project root."
  (interactive)
  (lispm-agents-new 'claude))

(defun lispm-agents-setup-claude ()
  "Make Claude Code one of the editor's agents."
  (lispm-agents-define
   'claude
   :name "Claude Code"
   :icon '(nerd-icons-codicon "nf-cod-claude" lispm-agents-claude-icon)
   :text-icon "✻"
   :command #'lispm-agents-claude-new
   :key "c"
   :built-in t
   :program "claude"
   :sessions #'lispm-agents-claude-sessions
   :rename #'lispm-agents-claude--rename
   :files #'lispm-agents-claude--session-files
   :worktree #'lispm-agents-claude--worktree
   :new-arguments #'lispm-agents-claude--new-arguments
   :resume-arguments (lambda (id) (list "--resume" id))
   :arguments #'lispm-agents-claude--arguments
   :context #'lispm-agents-ide-start))

(provide 'lispm-agents-claude)
;;; lispm-agents-claude.el ends here
