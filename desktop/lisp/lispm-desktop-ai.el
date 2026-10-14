;;; lispm-desktop-ai.el --- Coding agents, in terminals and in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
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

;; Coding agents (Claude Code, Codex, Hermes) run as their native CLIs inside
;; Eat terminals.  This module adds these things around them:
;;
;; - a unified, newest-first list of every recorded session of the agents,
;;   resuming the chosen one in its original working directory;
;; - a left sidebar view that groups sessions and running terminals by
;;   project and shows the chosen one in the main window;
;; - commands that start a fresh agent at the current project root;
;; - sending the active region (with its location) to a running agent;
;; - a server per terminal showing the agent Emacs's state
;;   (`lispm-desktop-agent-context'): Claude's IDE protocol, plain MCP for Codex and
;;   Hermes.
;;
;; Session discovery is a pure parse of the agents' own records.  Claude and
;; Codex keep JSONL transcripts; Hermes keeps a SQLite store:
;;
;;   transcript file --(head/tail bytes)--> JSON objects --(parser)--> session
;;   SQLite store --(one query)--> rows --(parser)--> sessions
;;
;; A session is a plist (:agent :id :cwd :title :time :file).  Parsed sessions
;; are memoized per (file . mtime), so reopening the list rereads only files
;; that changed since the last listing; the Hermes store is reread when it or
;; its write-ahead log changed.
;;
;; Agents can also run inside Emacs rather than as CLIs: such an agent
;; registers in `lispm-desktop-agent-emacs-agents' how to list, start, resume and feed
;; its sessions, and then takes part in everything above (see lispm-desktop-lispm).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-agent-ide)
(require 'lispm-desktop-agent-mcp)

(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(declare-function lispm-desktop-terminal-live-p "lispm-desktop-term" (buffer))
(declare-function lispm-desktop-terminal-paste "lispm-desktop-term" (buffer text))
(declare-function lispm-desktop-term-waiting-buffers "lispm-desktop-term" ())
(declare-function project-root "project" (project))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(declare-function lispm-desktop-sidebar-agents "lispm-desktop-sidebar" ())
(declare-function lispm-desktop-sidebar--one-line "lispm-desktop-sidebar" (text width))
(declare-function lispm-desktop-sidebar-show-in-main "lispm-desktop-sidebar" (buffer))
(declare-function lispm-desktop-main-area--windows "lispm-desktop-interface" ())
(declare-function lispm-desktop-workspace-enter "lispm-desktop-interface" (name &optional directory))
(declare-function lispm-desktop-workspace-show-alone "lispm-desktop-interface" (buffer))
(declare-function lispm-desktop-agent--buffer-agent "lispm-desktop-agent-list" (buffer))
(declare-function lispm-desktop-agent--running-buffers "lispm-desktop-agent-list" ())
(declare-function lispm-desktop-agent--session-buffer "lispm-desktop-agent-list" (id))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup--frame-xy "lispm-desktop-popup" (position))
(defvar tabspaces-default-tab)
(defvar eat-terminal)
(defvar embark-keymap-alist)

;;;; Agents

(defconst lispm-desktop-agent-programs
  '((claude . "claude")
    (codex . "codex")
    (hermes . "hermes"))
  "Executable for each supported agent.")

(defconst lispm-desktop-agent-names
  '((claude . "Claude")
    (codex . "Codex")
    (hermes . "Hermes"))
  "Display name of each supported agent.")

(defun lispm-desktop-agent-installed-p (agent)
  "Return non-nil when the CLI of AGENT is on PATH.
AGENT is a key of `lispm-desktop-agent-programs'.
An agent whose CLI is missing takes no part anywhere: no command offers
it and none of its recorded sessions are listed."
  (executable-find (alist-get agent lispm-desktop-agent-programs)))

(defun lispm-desktop-agent--check-installed (agent)
  "Signal a `user-error' unless the CLI of AGENT is installed."
  (unless (lispm-desktop-agent-installed-p agent)
    (user-error "%s is not installed: `%s' is not on PATH"
                (alist-get agent lispm-desktop-agent-names)
                (alist-get agent lispm-desktop-agent-programs))))

(defvar lispm-desktop-agent-emacs-agents nil
  "Agents running inside Emacs rather than as CLIs in terminals, by symbol.
Each value is a plist:
  :name      display name
  :icon      (FUNCTION ICON FACE), as in `lispm-desktop-agent-icons'
  :command   command starting a session at the current project root
  :sessions  function returning the recorded sessions, as session plists
  :new       function of a directory starting a session there; returns
             its buffer
  :resume    function of a session resuming it; returns its buffer
  :buffer-p  predicate on buffers holding one of its sessions
  :send      function of a buffer and text putting the text into the
             session's input, without sending it
  :rename    function of a session and a title
  :files     function of a session returning the files recording it")

(defun lispm-desktop-agent--emacs-agent (agent)
  "Return the plist of AGENT when it runs inside Emacs, else nil."
  (alist-get agent lispm-desktop-agent-emacs-agents))

(defun lispm-desktop-agent--emacs-buffer-agent (buffer)
  "Return the in-Emacs agent whose session BUFFER holds, or nil."
  (car (seq-find (lambda (entry) (funcall (plist-get (cdr entry) :buffer-p) buffer))
                 lispm-desktop-agent-emacs-agents)))

(defconst lispm-desktop-agent-claude-projects-directory
  (expand-file-name "~/.claude/projects/")
  "Directory where Claude Code stores one subdirectory of transcripts per project.")

(defconst lispm-desktop-agent-codex-sessions-directory
  (expand-file-name "~/.codex/sessions/")
  "Directory where Codex stores its dated rollout transcripts.")

(defconst lispm-desktop-agent-codex-index-file
  (expand-file-name "~/.codex/session_index.jsonl")
  "Codex index mapping session ids to their thread names.")

(defconst lispm-desktop-agent-hermes-state-file (expand-file-name "~/.hermes/state.db")
  "SQLite store where Hermes keeps every session and its messages.")

(defconst lispm-desktop-agent-hermes-sources '("cli" "desktop")
  "Sources of the Hermes sessions listed: those a person began at a
terminal or in the desktop app, not those of messaging gateways or of
other programs driving Hermes.")

(defconst lispm-desktop-agent-hermes-query
  "SELECT s.id, s.cwd, s.git_repo_root, s.title, s.started_at,
          (SELECT MAX(m.timestamp) FROM messages m WHERE m.session_id = s.id),
          (SELECT m.content FROM messages m
            WHERE m.session_id = s.id AND m.role = 'user' AND m.active = 1
            ORDER BY m.timestamp, m.id LIMIT 1)
     FROM sessions s
    WHERE s.hidden = 0 AND s.archived = 0 AND s.source IN (%s)
      AND NOT EXISTS (SELECT 1 FROM sessions c WHERE c.parent_session_id = s.id)"
  "Query listing Hermes sessions: id, directory, repository root, title,
start time, last message time and first prompt.  Sessions Hermes hides or
archives are left out, as is one that another session carries on after
compressing it.  %s takes a placeholder per listed source.")

(defconst lispm-desktop-agent-transcript-head-bytes 65536
  "Bytes read from the start of a transcript to find its metadata and first prompt.")

(defconst lispm-desktop-agent-transcript-tail-bytes 16384
  "Bytes read from the end of a transcript to find a title set late in the session.")

(defconst lispm-desktop-agent-title-width 72
  "Maximum width of a session title in the session list.")

(defconst lispm-desktop-agent-list-buffer-name "*Agents*"
  "Name of the agent sidebar buffer.")

(defvar lispm-desktop-sidebar-width)               ; lispm-desktop-core: shared by all sidebar views

(defconst lispm-desktop-agent-list-project-preview 5
  "Number of recorded sessions an unexpanded project group shows.")

(defconst lispm-desktop-agent-list-fold-age (* 7 86400)
  "Seconds of inactivity after which a project group starts folded.")

;;;; Transcript reading (effects)

(defun lispm-desktop-agent--read-json-region (file beg end)
  "Return the JSON objects on the complete lines of FILE between BEG and END.
A line cut by BEG or END (when END stops short of the file's size) is skipped,
as is any line that is not valid JSON.  Objects are plists with list arrays."
  (let ((truncated (< end (file-attribute-size (file-attributes file)))))
    (with-temp-buffer
      (insert-file-contents file nil beg end)
      (goto-char (point-min))
      (when (> beg 0) (forward-line 1))
      (lispm-desktop-agent--parse-json-lines truncated))))

(defun lispm-desktop-agent--parse-json-lines (truncated)
  "Return the JSON objects on the lines from point to the end of the buffer.
When TRUNCATED, the last line is partial and is skipped."
  (let (objects)
    (while (not (eobp))
      (let ((line (unless (and truncated (= (line-end-position) (point-max)))
                    (buffer-substring-no-properties (point) (line-end-position)))))
        (when-let* ((object (and line
                                 (ignore-errors
                                   (json-parse-string line
                                                      :object-type 'plist
                                                      :array-type 'list
                                                      :null-object nil
                                                      :false-object nil)))))
          (push object objects)))
      (forward-line 1))
    (nreverse objects)))

(defun lispm-desktop-agent--read-transcript (file)
  "Return the JSON objects at the head and tail of transcript FILE as a cons."
  (let ((size (file-attribute-size (file-attributes file))))
    (cons (lispm-desktop-agent--read-json-region
           file 0 (min size lispm-desktop-agent-transcript-head-bytes))
          (when (> size lispm-desktop-agent-transcript-head-bytes)
            (lispm-desktop-agent--read-json-region
             file (max lispm-desktop-agent-transcript-head-bytes
                       (- size lispm-desktop-agent-transcript-tail-bytes))
             size)))))

(defun lispm-desktop-agent--file-time (file)
  "Return the modification time of FILE as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

;;;; Transcript parsing (pure)

(defun lispm-desktop-agent--human-text-p (text)
  "Return non-nil when TEXT reads as a prompt a person typed.
Agents inject context, command echoes and reminders as tagged blocks that
start with `<', and Codex injects project guidance headed `# AGENTS.md';
those are not prompts."
  (and (stringp text)
       (let ((trimmed (string-trim-left text)))
         (not (or (string-empty-p trimmed)
                  (string-prefix-p "<" trimmed)
                  (string-prefix-p "# AGENTS.md" trimmed))))))

(defun lispm-desktop-agent--content-text (content)
  "Return the text of message CONTENT, a string or a list of typed parts."
  (if (stringp content)
      content
    (mapconcat (lambda (part) (or (plist-get part :text) "")) content "")))

(defun lispm-desktop-agent--claude-session (file objects)
  "Return the Claude session recorded in FILE given its head/tail OBJECTS.
The title is the last custom title or summary, else the first human prompt;
a custom title file overrides it later, see `lispm-desktop-agent--name-session'.
Return nil for a transcript without any prompt or title."
  (let* ((all (append (car objects) (cdr objects)))
         (title (seq-some (lambda (object)
                            (or (plist-get object :customTitle)
                                (and (equal (plist-get object :type) "summary")
                                     (plist-get object :summary))))
                          (reverse all)))
         (prompt (seq-some (lambda (object)
                             (and (equal (plist-get object :type) "user")
                                  (not (plist-get object :isMeta))
                                  (let ((text (lispm-desktop-agent--content-text
                                               (plist-get (plist-get object :message)
                                                          :content))))
                                    (and (lispm-desktop-agent--human-text-p text) text))))
                           (car objects)))
         (cwd (seq-some (lambda (object) (plist-get object :cwd)) all)))
    (when (and cwd (or title prompt))
      (list :agent 'claude
            :id (file-name-base file)
            :cwd cwd
            :title (or title prompt)
            :file file))))

(defun lispm-desktop-agent--codex-prompt (object)
  "Return the human prompt carried by Codex transcript OBJECT, if any."
  (let ((payload (plist-get object :payload)))
    (pcase (list (plist-get object :type) (plist-get payload :type))
      (`("event_msg" "user_message")
       (let ((text (plist-get payload :message)))
         (and (lispm-desktop-agent--human-text-p text) text)))
      (`("response_item" "message")
       (when (equal (plist-get payload :role) "user")
         (let ((text (lispm-desktop-agent--content-text (plist-get payload :content))))
           (and (lispm-desktop-agent--human-text-p text) text)))))))

(defun lispm-desktop-agent--codex-session (file objects)
  "Return the Codex session recorded in FILE given its head/tail OBJECTS.
Its title is the first prompt, or nil when there is none; a thread name from
the Codex index overrides it later, see `lispm-desktop-agent--name-session',
since the index changes without the transcript and so is no part of what is
memoized per transcript.  Its :started time, float seconds, is when the
session began, which ties a freshly started terminal to it, see
`lispm-desktop-agent--claim-sessions'.  Return nil for a transcript without
metadata."
  (when-let* ((meta (seq-find (lambda (object)
                                (equal (plist-get object :type) "session_meta"))
                              (car objects)))
              (payload (plist-get meta :payload))
              (id (plist-get payload :id))
              (cwd (plist-get payload :cwd)))
    (list :agent 'codex :id id :cwd cwd
          :title (seq-some #'lispm-desktop-agent--codex-prompt (car objects))
          :started (when-let* ((stamp (plist-get payload :timestamp)))
                     (ignore-errors (float-time (parse-iso8601-time-string stamp))))
          :file file)))

;;;; Hermes session parsing (pure)

(defun lispm-desktop-agent--hermes-prompt (content)
  "Return the prompt a person typed in Hermes message CONTENT, or nil.
CONTENT is text, or a JSON array of typed parts when it carried images."
  (let ((text (if (and (stringp content) (string-prefix-p "[" content))
                  (lispm-desktop-agent--content-text
                   (ignore-errors
                     (json-parse-string content :object-type 'plist :array-type 'list
                                        :null-object nil :false-object nil)))
                content)))
    (and (lispm-desktop-agent--human-text-p text) text)))

(defun lispm-desktop-agent--hermes-session (row)
  "Return the Hermes session of ROW, a row of `lispm-desktop-agent-hermes-query'.
Its title is the one Hermes gave or was given, else the first prompt; its
directory is the recorded one, else the repository, else home, which makes it
a chat.  Its :started time ties a freshly started terminal to it, see
`lispm-desktop-agent--claim-sessions'.  Return nil for a session with no title
and no prompt."
  (pcase-let ((`(,id ,cwd ,root ,title ,started ,last ,content) row))
    (when-let* ((title (or (and (stringp title) (not (string-blank-p title)) title)
                           (lispm-desktop-agent--hermes-prompt content))))
      (list :agent 'hermes
            :id id
            :cwd (or (and cwd (not (string-empty-p cwd)) cwd)
                     (and root (not (string-empty-p root)) root)
                     (expand-file-name "~"))
            :title title
            :time (or last started)
            :started started))))

(defun lispm-desktop-agent--claude-title-file (session)
  "Return the file holding Claude SESSION's custom title.
Claude Code keeps a session's chosen name beside its transcript, in the
session's own directory, rather than in the transcript, which it rewrites."
  (expand-file-name "custom-title.json"
                    (expand-file-name (plist-get session :id)
                                      (file-name-directory (plist-get session :file)))))

(defun lispm-desktop-agent--claude-custom-title (session)
  "Return the custom title chosen for Claude SESSION, or nil when none is."
  (let ((file (lispm-desktop-agent--claude-title-file session)))
    (when (file-readable-p file)
      (when-let* ((title (ignore-errors
                           (plist-get (json-parse-string
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string))
                                       :object-type 'plist)
                                      :customTitle)))
                  ((stringp title))
                  ((not (string-blank-p title))))
        title))))

(defun lispm-desktop-agent--name-session (session names)
  "Return SESSION titled by the name chosen for it, if one was.
A Codex session's name is its thread name in NAMES, the Codex index read by
`lispm-desktop-agent--codex-thread-names'; a Claude session's is its custom
title file.  Both live outside the transcript and change without it, so they
are no part of the transcript memo and are applied after it.  SESSION is not
modified: it is shared with the memo.  Return nil when SESSION is nil or has
neither a name nor a title of its own to show."
  (when-let* ((session)
              (title (or (pcase (plist-get session :agent)
                           ('codex (gethash (plist-get session :id) names))
                           ('claude (lispm-desktop-agent--claude-custom-title session)))
                         (plist-get session :title))))
    (plist-put (copy-sequence session) :title title)))

(defun lispm-desktop-agent--codex-thread-names ()
  "Return a hash table from Codex session id to its latest thread name."
  (let ((names (make-hash-table :test #'equal)))
    (when (file-readable-p lispm-desktop-agent-codex-index-file)
      (dolist (entry (lispm-desktop-agent--read-json-region
                      lispm-desktop-agent-codex-index-file 0
                      (file-attribute-size
                       (file-attributes lispm-desktop-agent-codex-index-file))))
        (when-let* ((id (plist-get entry :id))
                    (name (plist-get entry :thread_name)))
          (puthash id name names))))
    names))

;;;; Session discovery

(defvar lispm-desktop-agent--session-cache (make-hash-table :test #'equal)
  "Memoized sessions keyed by transcript file, valued (MTIME . SESSION-OR-NIL).")

(defun lispm-desktop-agent--cached-session (file parse)
  "Return the session of FILE, calling PARSE on its transcript when stale.
PARSE maps the head/tail objects of FILE to a session plist or nil."
  (let ((time (lispm-desktop-agent--file-time file))
        (cached (gethash file lispm-desktop-agent--session-cache)))
    (if (and cached (= (car cached) time))
        (cdr cached)
      (let ((session (when-let* ((parsed (funcall parse (lispm-desktop-agent--read-transcript file))))
                       (plist-put parsed :time time))))
        (puthash file (cons time session) lispm-desktop-agent--session-cache)
        session))))

(defun lispm-desktop-agent--claude-files ()
  "Return every top-level Claude Code transcript file."
  (when (file-directory-p lispm-desktop-agent-claude-projects-directory)
    (file-expand-wildcards
     (expand-file-name "*/*.jsonl" lispm-desktop-agent-claude-projects-directory))))

(defun lispm-desktop-agent--codex-files ()
  "Return every Codex rollout transcript file."
  (when (file-directory-p lispm-desktop-agent-codex-sessions-directory)
    (directory-files-recursively lispm-desktop-agent-codex-sessions-directory
                                 "\\`rollout-.*\\.jsonl\\'")))

(defun lispm-desktop-agent--newest-per-id (sessions)
  "Return SESSIONS keeping only the most recent transcript of each agent id.
Codex writes forked threads to separate files that share the session id."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((key (cons (plist-get session :agent) (plist-get session :id)))
             (seen (gethash key latest)))
        (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
          (puthash key session latest))))
    (hash-table-values latest)))

(defun lispm-desktop-agent--hermes-rows ()
  "Return the rows of `lispm-desktop-agent-hermes-query' from Hermes's store.
Return nil without the store, or without SQLite in this Emacs."
  (when (and (sqlite-available-p) (file-readable-p lispm-desktop-agent-hermes-state-file))
    (let ((db (sqlite-open lispm-desktop-agent-hermes-state-file)))
      (unwind-protect
          (sqlite-select db
                         (format lispm-desktop-agent-hermes-query
                                 (mapconcat (lambda (_) "?") lispm-desktop-agent-hermes-sources ", "))
                         lispm-desktop-agent-hermes-sources)
        (sqlite-close db)))))

(defvar lispm-desktop-agent--hermes-cache nil
  "Memoized Hermes sessions as (STAMP . SESSIONS).
STAMP is the modification times of the store and its write-ahead log,
which Hermes updates in place.")

(defun lispm-desktop-agent--hermes-stamp ()
  "Return the modification times of Hermes's store and its write-ahead log."
  (mapcar (lambda (file) (and (file-exists-p file) (lispm-desktop-agent--file-time file)))
          (list lispm-desktop-agent-hermes-state-file (concat lispm-desktop-agent-hermes-state-file "-wal"))))

(defun lispm-desktop-agent-hermes-sessions ()
  "Return every recorded Hermes session, rereading the store when it changed.
A store that cannot be read lists no sessions, and says so."
  (let ((stamp (lispm-desktop-agent--hermes-stamp)))
    (unless (and lispm-desktop-agent--hermes-cache (equal (car lispm-desktop-agent--hermes-cache) stamp))
      (setq lispm-desktop-agent--hermes-cache
            (cons stamp
                  (condition-case err
                      (delq nil (mapcar #'lispm-desktop-agent--hermes-session (lispm-desktop-agent--hermes-rows)))
                    (error (message "Hermes sessions unavailable: %s"
                                    (error-message-string err))
                           nil)))))
    (cdr lispm-desktop-agent--hermes-cache)))

(defun lispm-desktop-agent--emacs-sessions ()
  "Return the recorded sessions of the agents running inside Emacs.
An agent whose sessions cannot be read lists none, and says so."
  (mapcan (lambda (entry)
            (condition-case err
                (copy-sequence (funcall (plist-get (cdr entry) :sessions)))
              (error (message "%s sessions unavailable: %s" (plist-get (cdr entry) :name)
                              (error-message-string err))
                     nil)))
          lispm-desktop-agent-emacs-agents))

(defun lispm-desktop-agent-sessions-list ()
  "Return every recorded session of every agent, newest first.
Claude Code, Codex and Hermes sessions, of those whose CLI is installed, and
those of the agents running inside Emacs, see
`lispm-desktop-agent-emacs-agents'."
  (let ((names (when (lispm-desktop-agent-installed-p 'codex) (lispm-desktop-agent--codex-thread-names))))
    (sort (lispm-desktop-agent--newest-per-id
           (delq nil
                 (append
                  (when (lispm-desktop-agent-installed-p 'claude)
                    (mapcar (lambda (file)
                              (lispm-desktop-agent--name-session
                               (lispm-desktop-agent--cached-session
                                file (apply-partially #'lispm-desktop-agent--claude-session file))
                               names))
                            (lispm-desktop-agent--claude-files)))
                  (when (lispm-desktop-agent-installed-p 'codex)
                    (mapcar (lambda (file)
                              (lispm-desktop-agent--name-session
                               (lispm-desktop-agent--cached-session
                                file (apply-partially #'lispm-desktop-agent--codex-session file))
                               names))
                            (lispm-desktop-agent--codex-files)))
                  (when (lispm-desktop-agent-installed-p 'hermes)
                    (lispm-desktop-agent-hermes-sessions))
                  (lispm-desktop-agent--emacs-sessions))))
          (lambda (a b) (> (plist-get a :time) (plist-get b :time))))))

;;;; Presentation (pure)

(defun lispm-desktop-agent--age (time now)
  "Return a short human age for TIME relative to NOW, both float seconds.
Past a month it is the date, short as Mac OS 9's Finder wrote it: 5/13/26."
  (let ((seconds (- now time)))
    (cond ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          ((< seconds (* 30 86400)) (format "%dd" (/ seconds 86400)))
          (t (format-time-string "%-m/%-d/%y" time)))))

(defun lispm-desktop-agent--short-id (session)
  "Return the short form of SESSION's id that tells sessions apart.
Claude and Codex ids are UUIDs, distinct from their first characters on;
Hermes ids begin with the start time and end in random characters."
  (let ((id (plist-get session :id)))
    (cond ((eq (plist-get session :agent) 'hermes)
           (car (last (split-string id "_"))))
          ;; In-Emacs agents date their ids too and end them in random digits.
          ((lispm-desktop-agent--emacs-agent (plist-get session :agent))
           (car (last (split-string id "-"))))
          (t (substring id 0 8)))))

(defun lispm-desktop-agent--session-label (session now)
  "Return the completion label for SESSION, with ages relative to NOW.
The short id suffix keeps labels unique across sessions with equal titles."
  (format "%s %s %s %s %s"
          (propertize (format "%-6s" (plist-get session :agent))
                      'face (pcase (plist-get session :agent)
                              ('claude 'font-lock-keyword-face)
                              ('codex 'font-lock-type-face)
                              (_ 'font-lock-function-name-face)))
          (propertize (format "%-18s" (truncate-string-to-width
                                       (file-name-nondirectory
                                        (directory-file-name (plist-get session :cwd)))
                                       18))
                      'face 'font-lock-constant-face)
          (propertize (format "%-10s" (lispm-desktop-agent--age (plist-get session :time) now))
                      'face 'shadow)
          (lispm-desktop-sidebar--one-line (plist-get session :title) lispm-desktop-agent-title-width)
          (propertize (lispm-desktop-agent--short-id session) 'face 'shadow)))

;;;; Terminals (effects)

(defun lispm-desktop-agent--buffer-p (buffer)
  "Return non-nil when BUFFER holds an agent session.
That is an Eat terminal created for an agent, or the buffer of a session
of an agent running inside Emacs."
  (and (buffer-live-p buffer)
       (or (and (memq (buffer-local-value 'major-mode buffer) '(eat-mode lispm-desktop-term-mode))
                (string-match-p "\\`\\*\\(claude\\|codex\\|hermes\\):" (buffer-name buffer)))
           (and (lispm-desktop-agent--emacs-buffer-agent buffer) t))))

(defun lispm-desktop-agent--running-p (buffer)
  "Return non-nil when BUFFER exists and still runs a live process.
The session of an agent running inside Emacs runs while its buffer lives."
  (if (and buffer (lispm-desktop-agent--emacs-buffer-agent buffer))
      (buffer-live-p buffer)
    (and buffer (lispm-desktop-terminal-live-p buffer))))

(defvar-local lispm-desktop-agent-buffer-session-id nil
  "Id of the session this agent terminal runs, when known.")
(put 'lispm-desktop-agent-buffer-session-id 'permanent-local t)

(defvar-local lispm-desktop-agent-buffer-launched nil
  "Time, float seconds, this agent terminal was started.")
(put 'lispm-desktop-agent-buffer-launched 'permanent-local t)

(defvar-local lispm-desktop-agent-buffer-title nil
  "Human-readable title of the session this agent terminal runs.
Tab labels show it instead of the terminal's machine-oriented buffer name.")
(put 'lispm-desktop-agent-buffer-title 'permanent-local t)

(defun lispm-desktop-agent--uuid ()
  "Return a random version 4 UUID string."
  (let ((bytes (mapcar (lambda (_) (random 256)) (number-sequence 1 16))))
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
           bytes)))

(defconst lispm-desktop-agent-session-marker-regexp
  (concat "\\`\\(CLAUDECODE\\|CLAUDE_PID\\|CLAUDE_EFFORT"
          "\\|CLAUDE_CODE_[A-Z0-9_]*\\|CLAUDE_AGENT_SDK_[A-Z0-9_]*"
          "\\|CLAUDE_PREVIEW_[A-Z0-9_]*\\)=")
  "Environment entries that tie a process to an enclosing Claude Code session.
An Emacs started from inside Claude Code inherits them; a Claude launched with
them believes it is that session's child, disables its own transcript and
reuses the parent's session id and messaging socket.")

(defun lispm-desktop-agent--independent-environment (environment)
  "Return ENVIRONMENT without enclosing Claude Code session markers.
Agents started from Emacs are independent top-level sessions."
  (seq-remove (lambda (entry) (string-match-p lispm-desktop-agent-session-marker-regexp entry))
              environment))

(defun lispm-desktop-agent--launch (agent directory name args &optional id title)
  "Run AGENT with ARGS in DIRECTORY inside the terminal NAME; return it.
The terminal is a lispm-desktop-term once its module is built, see
`lispm-desktop-terminal-make', else an Eat terminal.
The terminal records the session ID and TITLE for labels and the sidebar.
The agent runs as an independent session whatever environment Emacs itself
inherited.  A running terminal with the same NAME is returned instead of
restarted."
  (lispm-desktop-agent--check-installed agent)
  (unless (file-directory-p directory)
    (user-error "Working directory no longer exists: %s" directory))
  (let* ((default-directory (file-name-as-directory directory))
         ;; A new agent gets a server showing it Emacs's state; a running
         ;; terminal returned as is keeps the one it has.
         (server (unless (lispm-desktop-agent--running-p (get-buffer (format "*%s*" name)))
                   (pcase agent
                     ('claude (lispm-desktop-agent-ide-start default-directory))
                     ((or 'codex 'hermes) (lispm-desktop-agent-mcp-start agent default-directory)))))
         (process-environment (append (and server (lispm-desktop-agent-context-environment server))
                                      (lispm-desktop-agent--independent-environment process-environment)))
         (buffer (apply #'lispm-desktop-terminal-make name (alist-get agent lispm-desktop-agent-programs)
                        (append (and server (lispm-desktop-agent-context-arguments server)) args))))
    (when server
      (lispm-desktop-agent-context-bind server buffer)
      (when (and (eq agent 'hermes) (not (lispm-desktop-agent-mcp-hermes-configured-p)))
        (message "Hermes cannot see Emacs's state until %s runs once"
                 (substitute-command-keys "\\[lispm-desktop-agent-hermes-setup]"))))
    (with-current-buffer buffer
      (setq lispm-desktop-agent-buffer-session-id (or id lispm-desktop-agent-buffer-session-id)
            lispm-desktop-agent-buffer-title (or title lispm-desktop-agent-buffer-title)
            lispm-desktop-agent-buffer-launched (or lispm-desktop-agent-buffer-launched (float-time))))
    ;; Agents record their sessions as they go, so closing the terminal
    ;; loses nothing a resume cannot restore: kill it without asking.
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-query-on-exit-flag process nil))
    buffer))

(defun lispm-desktop-agent-buffer-label (buffer)
  "Return the tab label of BUFFER when it is an agent terminal, else nil."
  (when (lispm-desktop-agent--buffer-p buffer)
    (or (buffer-local-value 'lispm-desktop-agent-buffer-title buffer)
        (string-trim (buffer-name buffer) "\\*" "\\*"))))

(defconst lispm-desktop-agent-claim-slack 5
  "Seconds a session may seem to start before its terminal did.
The terminal's launch is stamped by Emacs and the session's start by the
agent; the slack absorbs their clocks' rounding.")

(defun lispm-desktop-agent--claim-sessions (sessions)
  "Tie running agent terminals that do not know their session to SESSIONS.
Codex assigns its session id itself, so a terminal started fresh learns it
from the transcripts: its session is the earliest one of its agent that
began in the terminal's directory at or after its launch and that no other
terminal runs.  Terminals claim in launch order, so two started in one
directory take the sessions in the order they began."
  (let* ((running (lispm-desktop-agent--running-buffers))
         (claimed (delq nil (mapcar (lambda (buffer)
                                      (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer))
                                    running)))
         (candidates (sort (seq-filter (lambda (session) (plist-get session :started))
                                       sessions)
                           (lambda (a b) (< (plist-get a :started) (plist-get b :started))))))
    (dolist (buffer (sort (seq-filter
                           (lambda (buffer)
                             (and (null (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer))
                                  (buffer-local-value 'lispm-desktop-agent-buffer-launched buffer)))
                           running)
                          (lambda (a b) (< (buffer-local-value 'lispm-desktop-agent-buffer-launched a)
                                           (buffer-local-value 'lispm-desktop-agent-buffer-launched b)))))
      (with-current-buffer buffer
        (let ((directory (directory-file-name (expand-file-name default-directory)))
              (agent (lispm-desktop-agent--buffer-agent buffer)))
          (when-let* ((session
                       (seq-find
                        (lambda (session)
                          (and (eq (plist-get session :agent) agent)
                               (not (member (plist-get session :id) claimed))
                               (>= (plist-get session :started)
                                   (- lispm-desktop-agent-buffer-launched lispm-desktop-agent-claim-slack))
                               (equal (directory-file-name
                                       (expand-file-name (plist-get session :cwd)))
                                      directory)))
                        candidates)))
            (setq lispm-desktop-agent-buffer-session-id (plist-get session :id))
            (push (plist-get session :id) claimed)))))))

(defun lispm-desktop-agent--refresh-buffer-titles (sessions)
  "Update the titles of running agent terminals from SESSIONS.
Terminals first claim their sessions, see
`lispm-desktop-agent--claim-sessions'.  A new session gains its real title
once its first prompt is recorded, and a renamed session follows its new name."
  (lispm-desktop-agent--claim-sessions sessions)
  (let ((titles (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (plist-get session :id) (plist-get session :title) titles))
    (dolist (buffer (lispm-desktop-agent--running-buffers))
      (with-current-buffer buffer
        (when-let* ((title (and lispm-desktop-agent-buffer-session-id
                                (gethash lispm-desktop-agent-buffer-session-id titles))))
          (setq lispm-desktop-agent-buffer-title title))))))

(defun lispm-desktop-agent--main-window ()
  "Return the most recently used window of the main area of this frame."
  (or (car (sort (lispm-desktop-main-area--windows)
                 (lambda (a b) (> (window-use-time a) (window-use-time b)))))
      (selected-window)))

(defun lispm-desktop-agent--workspace-name (directory)
  "Return the workspace owning sessions run in DIRECTORY.
Sessions belong to their project root, so worktrees and subdirectories share
the repository's workspace; project-less chats share the default workspace."
  (let ((root (lispm-desktop-agent--project-root directory)))
    (if (eq root 'chats)
        (cons tabspaces-default-tab nil)
      (cons (file-name-nondirectory root) root))))

(defun lispm-desktop-agent--show (buffer)
  "Show agent terminal BUFFER in its project's workspace and select it.
In a workspace just created for it, BUFFER is shown alone; otherwise it is
placed as `lispm-desktop-sidebar-show-in-main' places terminals.  The agent
sidebar is refreshed, so the terminal appears in its running list."
  (pcase-let ((`(,name . ,root) (lispm-desktop-agent--workspace-name
                                 (buffer-local-value 'default-directory buffer))))
    (if (lispm-desktop-workspace-enter name root)
        (lispm-desktop-workspace-show-alone buffer)
      (lispm-desktop-sidebar-show-in-main buffer)))
  (lispm-desktop-agent-list-refresh)
  buffer)

(defun lispm-desktop-agent-next-waiting ()
  "Show the next terminal whose program waits for you.
Programs blocked on an answer come first, then those that failed, then
those that finished, as they report themselves, see
`lispm-desktop-term-waiting-buffers'; repeating the command goes on to
the next one."
  (interactive)
  (let* ((waiting (lispm-desktop-term-waiting-buffers))
         (next (or (cadr (memq (current-buffer) waiting)) (car waiting))))
    (unless next
      (user-error "No terminal is waiting for you"))
    (if (lispm-desktop-agent--buffer-p next)
        (lispm-desktop-agent--show next)
      (lispm-desktop-sidebar-show-in-main next))))

(defun lispm-desktop-agent--project-directory ()
  "Return the current project root, or `default-directory' outside projects."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun lispm-desktop-agent--fresh-name (agent directory)
  "Return an unused terminal name for a new AGENT session in DIRECTORY."
  (let* ((base (format "%s:%s" agent
                       (file-name-nondirectory (directory-file-name directory))))
         (name base)
         (n 1))
    (while (lispm-desktop-agent--running-p (get-buffer (concat "*" name "*")))
      (setq n (1+ n)
            name (format "%s<%d>" base n)))
    name))

;;;; Commands

(defvar lispm-desktop-agent--candidates nil
  "Session plists of the last session listing, keyed by their labels.")

(defun lispm-desktop-agent--session-at (label)
  "Return the session plist listed under LABEL."
  (or (and lispm-desktop-agent--candidates (gethash label lispm-desktop-agent--candidates))
      (user-error "Unknown agent session: %s" label)))

(defun lispm-desktop-agent--read-session (prompt)
  "Read a recorded agent session with PROMPT and return its plist."
  (let* ((now (float-time))
         (table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (session)
                           (let ((label (lispm-desktop-agent--session-label session now)))
                             (puthash label session table)
                             label))
                         (lispm-desktop-agent-sessions-list))))
    (setq lispm-desktop-agent--candidates table)
    (lispm-desktop-agent--session-at
     (completing-read prompt
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata (category . lispm-desktop-agent-session)
                                       (display-sort-function . identity))
                          (complete-with-action action labels string pred)))
                      nil t))))

(defun lispm-desktop-agent-resume-session (session)
  "Resume SESSION in its original working directory.
A session already running in a terminal is shown there instead: resuming it
again would run a second agent on the same session."
  (let ((agent (plist-get session :agent))
        (id (plist-get session :id))
        (cwd (plist-get session :cwd)))
    (lispm-desktop-agent--show
     (or (lispm-desktop-agent--session-buffer id)
         (when-let* ((emacs (lispm-desktop-agent--emacs-agent agent)))
           (funcall (plist-get emacs :resume) session))
         (lispm-desktop-agent--launch agent cwd
                                      (format "%s:%s:%s" agent
                                              (file-name-nondirectory (directory-file-name cwd))
                                              (lispm-desktop-agent--short-id session))
                                      (pcase agent
                                        ('claude (list "--resume" id))
                                        ('codex (list "resume" id))
                                        ('hermes (list "chat" "--resume" id)))
                                      id
                                      (plist-get session :title))))))

(defun lispm-desktop-agent-sessions ()
  "Choose a recorded Claude Code, Codex or Hermes session and resume it."
  (interactive)
  (lispm-desktop-agent-resume-session (lispm-desktop-agent--read-session "Agent session: ")))

(defun lispm-desktop-agent-new (agent &optional directory)
  "Start a new AGENT session in DIRECTORY.
DIRECTORY defaults to the project root of the main window's buffer."
  (let* ((directory (or directory
                        (with-selected-window (lispm-desktop-agent--main-window)
                          (lispm-desktop-agent--project-directory))))
         ;; Claude accepts a chosen session id, so the terminal knows its
         ;; session from the start; Codex and Hermes assign their own.
         (id (when (eq agent 'claude) (lispm-desktop-agent--uuid))))
    (lispm-desktop-agent--show
     (if-let* ((emacs (lispm-desktop-agent--emacs-agent agent)))
         (funcall (plist-get emacs :new) directory)
       (lispm-desktop-agent--launch agent directory (lispm-desktop-agent--fresh-name agent directory)
                                    (pcase agent
                                      ('claude (list "--session-id" id))
                                      ('hermes (list "chat")))
                                    id
                                    (format "New %s · %s"
                                            (alist-get agent lispm-desktop-agent-names)
                                            (file-name-nondirectory (directory-file-name directory))))))))

(defun lispm-desktop-agent-claude-new ()
  "Start a new Claude Code session at the current project root."
  (interactive)
  (lispm-desktop-agent-new 'claude))

(defun lispm-desktop-agent-codex-new ()
  "Start a new Codex session at the current project root."
  (interactive)
  (lispm-desktop-agent-new 'codex))

(defun lispm-desktop-agent-hermes-new ()
  "Start a new Hermes session at the current project root."
  (interactive)
  (lispm-desktop-agent-new 'hermes))

(defun lispm-desktop-agent-hermes-setup ()
  "Have Hermes add Emacs as an MCP server, in a terminal, once.
Hermes takes MCP servers from its config file alone and adds one only
interactively: the terminal runs its add command, which tries the bridge,
finds it serving no tools outside an agent terminal, and asks whether to
save the server anyway.  Answer yes.  Every Hermes terminal started from
Emacs then sees Emacs's state."
  (interactive)
  (lispm-desktop-agent--check-installed 'hermes)
  (pop-to-buffer
   (let ((process-environment (lispm-desktop-agent--independent-environment process-environment)))
     (apply #'lispm-desktop-terminal-make "hermes setup" (alist-get 'hermes lispm-desktop-agent-programs)
            (lispm-desktop-agent-mcp-hermes-setup-arguments)))))

(defun lispm-desktop-agent--pick-terminal ()
  "Return the agent terminal to send to, preferring the current project.
Ask when several running agent terminals qualify."
  (let* ((root (lispm-desktop-agent--project-directory))
         (running (seq-filter (lambda (buffer)
                                (and (lispm-desktop-agent--buffer-p buffer)
                                     (lispm-desktop-agent--running-p buffer)))
                              (buffer-list)))
         (local (seq-filter (lambda (buffer)
                              (file-in-directory-p
                               (buffer-local-value 'default-directory buffer) root))
                            running))
         (candidates (or local running)))
    (pcase candidates
      ('nil (user-error "No running agent; start one with C-c a c, C-c a x, C-c a h or C-c a l"))
      (`(,only) only)
      (_ (get-buffer (completing-read "Send to agent: "
                                      (mapcar #'buffer-name candidates) nil t))))))

(defun lispm-desktop-agent--region-reference (beg end)
  "Return a fenced reference to the text between BEG and END of this buffer.
The header names the file relative to the project root and the line range."
  (let* ((file (if buffer-file-name
                   (file-relative-name buffer-file-name (lispm-desktop-agent--project-directory))
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

(defun lispm-desktop-agent-send-region (beg end)
  "Send the region between BEG and END, with its location, to an agent terminal.
Without an active region, send a reference to the current file and line.
The text is pasted into the agent's composer without submitting it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((text (if beg
                  (lispm-desktop-agent--region-reference beg end)
                (format "%s:%d "
                        (if buffer-file-name
                            (file-relative-name buffer-file-name
                                                (lispm-desktop-agent--project-directory))
                          (buffer-name))
                        (line-number-at-pos))))
        (target (lispm-desktop-agent--pick-terminal)))
    (deactivate-mark)
    (if-let* ((emacs (lispm-desktop-agent--emacs-agent (lispm-desktop-agent--emacs-buffer-agent target))))
        (funcall (plist-get emacs :send) target text)
      (lispm-desktop-terminal-paste target text))
    (pop-to-buffer target)))

;;;; Session management

(defconst lispm-desktop-agent-state-file (locate-user-emacs-file "agent-sessions.eld")
  "File holding Emacs-side session state, currently the archived sessions.
Archiving hides a session in Emacs only; it never alters agent data.")

(defvar lispm-desktop-agent--archived 'unloaded
  "Keys of archived sessions, or `unloaded' before the state file is read.")

(defun lispm-desktop-agent--session-key (session)
  "Return the key identifying SESSION across agents, \"AGENT:ID\"."
  (format "%s:%s" (plist-get session :agent) (plist-get session :id)))

(defun lispm-desktop-agent--archived-keys ()
  "Return the archived session keys, reading the state file on first use."
  (when (eq lispm-desktop-agent--archived 'unloaded)
    (setq lispm-desktop-agent--archived
          (when (file-readable-p lispm-desktop-agent-state-file)
            (plist-get (with-temp-buffer
                         (insert-file-contents lispm-desktop-agent-state-file)
                         (ignore-errors (read (current-buffer))))
                       :archived))))
  lispm-desktop-agent--archived)

(defun lispm-desktop-agent--archived-p (session)
  "Return non-nil when SESSION is archived."
  (member (lispm-desktop-agent--session-key session) (lispm-desktop-agent--archived-keys)))

(defun lispm-desktop-agent--set-archived (session archived)
  "Mark SESSION ARCHIVED or not and persist the archived set."
  (let ((key (lispm-desktop-agent--session-key session))
        (keys (lispm-desktop-agent--archived-keys)))
    (setq lispm-desktop-agent--archived (if archived
                                            (cons key (delete key keys))
                                         (delete key keys)))
    (with-temp-file lispm-desktop-agent-state-file
      (let ((print-length nil) (print-level nil))
        (prin1 (list :archived lispm-desktop-agent--archived) (current-buffer))))))

(defun lispm-desktop-agent--append-json-line (file object)
  "Append OBJECT, a plist, to JSON Lines FILE as one UTF-8 line."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (decode-coding-string (json-serialize object) 'utf-8) "\n")
                  nil file t 'silent)))

(defun lispm-desktop-agent--hermes-run (&rest args)
  "Run Hermes with ARGS and return its output.
Hermes's store is its own to change, so renaming, deleting and exporting
go through Hermes, and the memoized sessions are dropped.  Signal a user
error with Hermes's output when it fails."
  (with-temp-buffer
    (let ((status (apply #'call-process (alist-get 'hermes lispm-desktop-agent-programs)
                         nil t nil args)))
      (setq lispm-desktop-agent--hermes-cache nil)
      (unless (eq status 0)
        (user-error "hermes %s: %s" (string-join args " ")
                    (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun lispm-desktop-agent--rename (session title)
  "Rename SESSION to TITLE the way its own agent records names.
Claude Code keeps a custom title file beside the transcript, see
`lispm-desktop-agent--claude-title-file'; a record appended to the transcript
itself would be lost when a running Claude rewrites it.  Codex appends a
thread name to its session index.  Hermes renames in its store, where titles
are unique, so a title already in use is refused.  This list then shows TITLE."
  (let ((id (plist-get session :id)))
    (pcase (plist-get session :agent)
      ('claude (let ((file (lispm-desktop-agent--claude-title-file session))
                     (coding-system-for-write 'utf-8-unix))
                 (make-directory (file-name-directory file) t)
                 (write-region (decode-coding-string
                                (json-serialize (list :customTitle title)) 'utf-8)
                               nil file nil 'silent)))
      ('codex (lispm-desktop-agent--append-json-line
               lispm-desktop-agent-codex-index-file
               (list :id id :thread_name title
                     :updated_at (format-time-string "%FT%T.%6NZ" nil t))))
      ('hermes (lispm-desktop-agent--hermes-run "sessions" "rename" id title))
      (agent (funcall (plist-get (lispm-desktop-agent--emacs-agent agent) :rename) session title)))))

(defun lispm-desktop-agent--session-files (session)
  "Return every file and directory recording SESSION.
A Claude transcript may have a sibling directory of subagent transcripts; a
Codex session may span several rollout files that share its id.  A Hermes
session has no file of its own: it lives in the store."
  (pcase (plist-get session :agent)
    ('claude (let* ((file (plist-get session :file))
                    (sibling (file-name-sans-extension file)))
               (if (file-directory-p sibling) (list file sibling) (list file))))
    ('codex (directory-files-recursively
             lispm-desktop-agent-codex-sessions-directory
             (regexp-quote (plist-get session :id))))
    (agent (when-let* ((emacs (lispm-desktop-agent--emacs-agent agent)))
             (funcall (plist-get emacs :files) session)))))

(defun lispm-desktop-agent--session-transcript (session)
  "Return a file holding SESSION's transcript.
A Claude or Codex session is its transcript file; a Hermes session is
exported from the store to Markdown in a fresh temporary directory."
  (if (eq (plist-get session :agent) 'hermes)
      (let ((directory (make-temp-file "hermes-session-" t)))
        (lispm-desktop-agent--hermes-run "sessions" "export" "--format" "md" "--yes"
                                         "--session-id" (plist-get session :id) directory)
        (or (car (directory-files directory t "\\.md\\'"))
            (user-error "Hermes exported nothing for session %s" (plist-get session :id))))
    (plist-get session :file)))

(defun lispm-desktop-agent--session-running-p (session)
  "Return non-nil when SESSION runs in a live agent terminal."
  (lispm-desktop-agent--session-buffer (plist-get session :id)))

(defun lispm-desktop-agent-rename-session (session)
  "Prompt for a new title of SESSION and rename it."
  (let ((title (string-trim (read-string "Rename session: "
                                         (plist-get session :title)))))
    (when (string-empty-p title)
      (user-error "Title must not be empty"))
    (lispm-desktop-agent--rename session title)
    (lispm-desktop-agent-list-refresh)))

(defun lispm-desktop-agent-toggle-archive-session (session)
  "Archive SESSION, or unarchive it when it is archived."
  (lispm-desktop-agent--set-archived session (not (lispm-desktop-agent--archived-p session)))
  (lispm-desktop-agent-list-refresh))

(defun lispm-desktop-agent-delete-session (session)
  "Delete SESSION after confirmation.
The files recording a Claude or Codex session go to the system trash; a
Hermes session is deleted from the store by Hermes itself.  A session that
is running in a terminal cannot be deleted."
  (when (lispm-desktop-agent--session-running-p session)
    (user-error "Session is running; exit its terminal first"))
  (let* ((agent (plist-get session :agent))
         (files (lispm-desktop-agent--session-files session))
         (title (lispm-desktop-sidebar--one-line (plist-get session :title) 40)))
    (when (eq 'delete
              (lispm-desktop-popup-alert
               'stop
               (if (eq agent 'hermes)
                   (format "Delete the Hermes session “%s”?" title)
                 (format "Move the %s session “%s” to the Trash?"
                         (alist-get agent lispm-desktop-agent-names (capitalize (symbol-name agent)))
                         title))
               `(("Cancel" cancel cancel)
                 (,(if (eq agent 'hermes) "Delete" "Move to Trash") delete default))
               (if (eq agent 'hermes)
                   "Hermes removes it from its store; this cannot be undone."
                 (format "Its %d file%s go%s to the Trash." (length files)
                         (if (cdr files) "s" "") (if (cdr files) "" "es")))))
      (if (eq agent 'hermes)
          (lispm-desktop-agent--hermes-run "sessions" "delete" "--yes" (plist-get session :id))
        (dolist (file files)
          (move-file-to-trash file)
          (remhash file lispm-desktop-agent--session-cache)))
      (lispm-desktop-agent--set-archived session nil)
      (lispm-desktop-agent-list-refresh))))

(defun lispm-desktop-agent--in-main-window (function &rest args)
  "Call FUNCTION with ARGS in the main window of this frame."
  (select-window (lispm-desktop-agent--main-window))
  (apply function args))

(defun lispm-desktop-agent-session-act (action session)
  "Perform the context menu ACTION on SESSION."
  (pcase action
    ('resume (lispm-desktop-agent-resume-session session))
    ('rename (lispm-desktop-agent-rename-session session))
    ('archive (lispm-desktop-agent-toggle-archive-session session))
    ('delete (lispm-desktop-agent-delete-session session))
    ('copy (kill-new (plist-get session :id))
           (message "Copied %s" (plist-get session :id)))
    ('transcript (lispm-desktop-agent--in-main-window #'find-file (lispm-desktop-agent--session-transcript session)))
    ('directory (lispm-desktop-agent--in-main-window #'dired (plist-get session :cwd)))))

(defun lispm-desktop-agent-list-context-menu (event)
  "Pop up the actions for the session clicked by mouse EVENT.
It is drawn as Mac OS 9's contextual menus were, see `lispm-desktop-popup-menu'."
  (interactive "e")
  (let* ((posn (event-start event))
         (session (with-current-buffer (window-buffer (posn-window posn))
                    (get-text-property (posn-point posn) 'lispm-desktop-agent-session))))
    (when session
      (pcase-let ((`(,x . ,y) (lispm-desktop-popup--frame-xy event)))
        (lispm-desktop-popup-menu
         (mapcar (lambda (entry)
                   (if (eq entry 'separator)
                       entry
                     (list :label (car entry)
                           :action (lambda () (lispm-desktop-agent-session-act (cdr entry) session)))))
                 `(("Resume" . resume)
                   ("Rename…" . rename)
                   (,(if (lispm-desktop-agent--archived-p session) "Unarchive" "Archive") . archive)
                   ("Delete…" . delete)
                   separator
                   ("Copy Session ID" . copy)
                   ("Open Transcript" . transcript)
                   ("Open Directory" . directory)))
         (1+ x) (1+ y))))))

(defun lispm-desktop-agent-list--session-at-point ()
  "Return the session of the sidebar entry at point."
  (or (get-text-property (point) 'lispm-desktop-agent-session)
      (user-error "No session at point")))

(defun lispm-desktop-agent-list-rename ()
  "Rename the session at point."
  (interactive)
  (lispm-desktop-agent-rename-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-toggle-archive ()
  "Archive or unarchive the session at point."
  (interactive)
  (lispm-desktop-agent-toggle-archive-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-delete ()
  "Move the session at point to the system trash."
  (interactive)
  (lispm-desktop-agent-delete-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-copy-id ()
  "Copy the id of the session at point."
  (interactive)
  (lispm-desktop-agent-session-act 'copy (lispm-desktop-agent-list--session-at-point)))

(defvar-keymap lispm-desktop-agent-list-entry-map
  :doc "Keymap on session entries: button keys plus the context menu."
  :parent button-map
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'lispm-desktop-agent-list-context-menu)

;;;; Embark actions on listed sessions

(defun lispm-desktop-agent-session-copy-id (label)
  "Copy the id of the agent session listed under LABEL."
  (let ((id (plist-get (lispm-desktop-agent--session-at label) :id)))
    (kill-new id)
    (message "Copied %s" id)))

(defun lispm-desktop-agent-session-open-transcript (label)
  "Open the transcript of the agent session listed under LABEL."
  (find-file (lispm-desktop-agent--session-transcript (lispm-desktop-agent--session-at label))))

(defun lispm-desktop-agent-session-open-directory (label)
  "Open the working directory of the agent session listed under LABEL."
  (dired (plist-get (lispm-desktop-agent--session-at label) :cwd)))

(defun lispm-desktop-agent-session-resume (label)
  "Resume the agent session listed under LABEL."
  (lispm-desktop-agent-resume-session (lispm-desktop-agent--session-at label)))

(defvar-keymap lispm-desktop-agent-session-embark-map
  :doc "Embark actions for agent sessions in the session list."
  "RET" #'lispm-desktop-agent-session-resume
  "w" #'lispm-desktop-agent-session-copy-id
  "f" #'lispm-desktop-agent-session-open-transcript
  "d" #'lispm-desktop-agent-session-open-directory)

(defvar-keymap lispm-desktop-agent-map
  :doc "Commands for coding agents, bound under \\`C-c a'."
  "a" #'lispm-desktop-sidebar-agents
  "s" #'lispm-desktop-agent-sessions
  "c" #'lispm-desktop-agent-claude-new
  "x" #'lispm-desktop-agent-codex-new
  "h" #'lispm-desktop-agent-hermes-new
  "n" #'lispm-desktop-agent-next-waiting
  "r" #'lispm-desktop-agent-send-region)

(defun lispm-desktop-setup-ai ()
  "Set up AI assistants."
  (keymap-global-set "C-c a" lispm-desktop-agent-map)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(lispm-desktop-agent-session . lispm-desktop-agent-session-embark-map)))
  (add-hook 'eat-exit-hook #'lispm-desktop-agent-list-refresh-on-exit)
  (add-hook 'lispm-desktop-term-exit-functions #'lispm-desktop-agent-list-refresh-on-exit)
  (add-hook 'lispm-desktop-term-status-functions #'lispm-desktop-agent-list-refresh-on-status))


(provide 'lispm-desktop-ai)
;;; lispm-desktop-ai.el ends here
