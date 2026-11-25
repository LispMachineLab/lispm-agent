;;; lispm-agents-sessions.el --- External agent session discovery, metadata, and archive state  -*- lexical-binding: t; -*-

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

;; JSONL transcript tail parsing, cached metadata and work details,
;; session hierarchy and ordering, background session readers, and
;; persistent archive/opened-time state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-agents-core)

;;;; Transcript reading (effects)

(defun lispm-agents-read-json-region (file beg end)
  "Return the JSON objects on the complete lines of FILE between BEG and END.
A line cut by BEG or END (when END stops short of the file's size) is skipped,
as is any line that is not valid JSON.  Objects are plists with list arrays."
  (let ((truncated (< end (file-attribute-size (file-attributes file)))))
    (with-temp-buffer
      (insert-file-contents file nil beg end)
      (goto-char (point-min))
      (when (> beg 0) (forward-line 1))
      (lispm-agents-parse-json-lines truncated))))

(defun lispm-agents-parse-json-lines (truncated)
  "Return the JSON objects on the lines from point to the end of the buffer.
When TRUNCATED, the last line is partial and is skipped."
  (let (objects)
    (while (not (eobp))
      (let ((line (unless (and truncated (= (line-end-position) (point-max)))
                    (buffer-substring-no-properties (point) (line-end-position)))))
        (when-let* ((object (and line
                                 (condition-case nil
                                     (json-parse-string line
                                                        :object-type 'plist
                                                        :array-type 'list
                                                        :null-object nil
                                                        :false-object nil)
                                   (json-error nil)))))
          (push object objects)))
      (forward-line 1))
    (nreverse objects)))

(defconst lispm-agents-tail-bytes (* 256 1024)
  "Bytes at the end of a transcript `lispm-agents-read-json-tail' reads.")

(defun lispm-agents-read-json-tail (file)
  "Return the JSON objects on the complete lines at the end of FILE.
They are those of its last `lispm-agents-tail-bytes', oldest first."
  (let ((size (file-attribute-size (file-attributes file))))
    (lispm-agents-read-json-region file (max 0 (- size lispm-agents-tail-bytes)) size)))

(defun lispm-agents-read-transcript (file)
  "Return the JSON objects at the head and tail of transcript FILE as a cons."
  (let ((size (file-attribute-size (file-attributes file))))
    (cons (lispm-agents-read-json-region
           file 0 (min size lispm-agents-transcript-head-bytes))
          (when (> size lispm-agents-transcript-head-bytes)
            (lispm-agents-read-json-region
             file (max lispm-agents-transcript-head-bytes
                       (- size lispm-agents-transcript-tail-bytes))
             size)))))

(defun lispm-agents-file-time (file)
  "Return the modification time of FILE as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

(defun lispm-agents-append-json-line (file object)
  "Append OBJECT, a plist, to JSON Lines FILE as one UTF-8 line."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (decode-coding-string (json-serialize object) 'utf-8) "\n")
                  nil file t 'silent)))

;;;; Transcript parsing (pure)

(defun lispm-agents-human-text-p (text)
  "Return non-nil when TEXT reads as a prompt a person typed.
Agents inject context, command echoes and reminders as tagged blocks that
start with `<', and Codex injects project guidance headed `# AGENTS.md';
those are not prompts."
  (and (stringp text)
       (let ((trimmed (string-trim-left text)))
         (not (or (string-empty-p trimmed)
                  (string-prefix-p "<" trimmed)
                  (string-prefix-p "# AGENTS.md" trimmed))))))

(defun lispm-agents-content-text (content)
  "Return the text of message CONTENT, a string or a list of typed parts."
  (if (stringp content)
      content
    (mapconcat (lambda (part) (or (plist-get part :text) "")) content "")))

;;;; Session discovery

(defvar lispm-agents-session-cache (make-hash-table :test #'equal)
  "Memoized sessions keyed by transcript file, valued (MTIME . SESSION-OR-NIL).")

(defun lispm-agents-cached-session (file parse)
  "Return the session of transcript FILE, calling PARSE when it changed.
PARSE maps the head and tail objects of FILE, as
`lispm-agents-read-transcript' returns them, to a session plist or
nil; the session's :time is FILE's modification time."
  (let ((time (lispm-agents-file-time file))
        (cached (gethash file lispm-agents-session-cache)))
    (if (and cached (= (car cached) time))
        (cdr cached)
      (let ((session (when-let* ((parsed (funcall parse (lispm-agents-read-transcript file))))
                       (plist-put parsed :time time))))
        (puthash file (cons time session) lispm-agents-session-cache)
        session))))

(defvar lispm-agents--work-cache (make-hash-table :test #'equal)
  "Work details keyed by transcript file and parser, stamped by size and time.")

(defun lispm-agents-cached-work (file parse)
  "Return FILE's bounded work details, parsed by PARSE only when changed.
PARSE takes `lispm-agents-read-json-tail' objects.  Both file size and
modification time invalidate the cache.  This is for lazy inspectors,
never for a roster redraw; an unreadable file has no known details.
The cached source must not be changed; public snapshots copy it."
  (when-let* ((attributes (condition-case nil (file-attributes file) (file-error nil))))
    (let* ((stamp (list (file-attribute-size attributes)
                        (file-attribute-modification-time attributes)))
           (key (list file parse))
           (cached (gethash key lispm-agents--work-cache)))
      (if (equal (car cached) stamp)
          (cdr cached)
        (let ((work (when-let* ((parsed (condition-case nil
                                           (funcall parse (lispm-agents-read-json-tail file))
                                         (error nil))))
                      (plist-put parsed :time (float-time (file-attribute-modification-time attributes))))))
          (puthash key (cons stamp work) lispm-agents--work-cache)
          work)))))

(defun lispm-agents-work-input-summary (input)
  "Return a bounded, single-line summary of a call's INPUT, or nil."
  (when input
    (lispm-agents-one-line
     (if (stringp input) input
       (condition-case nil (json-serialize input)
         (error (let ((print-length 20) (print-level 4)) (prin1-to-string input)))))
     160)))

(defun lispm-agents-file-sessions (files parse &optional finish)
  "Return the sessions recorded in transcript FILES, as PARSE finds them.
PARSE is called with a file and its head and tail objects, through
`lispm-agents-cached-session'.  FINISH, when given, is called with
each session found and returns it as shown, or nil to leave it out; the
sessions are memoized, so it must not modify them."
  (delq nil (mapcar (lambda (file)
                      (when-let* ((session (lispm-agents-cached-session
                                            file (apply-partially parse file))))
                        (if finish (funcall finish session) session)))
                    files)))

(defun lispm-agents--newest-per-id (sessions)
  "Return SESSIONS keeping only the most recent transcript of each agent id.
Codex writes forked threads to separate files that share the session id."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((key (cons (plist-get session :agent) (plist-get session :id)))
             (seen (gethash key latest)))
        (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
          (puthash key session latest))))
    (hash-table-values latest)))

(defvar lispm-agents-read-sessions-function #'lispm-agents-read-sessions
  "Function reading an agent's sessions as they are now.
`lispm-agents-sessions-list', which lists every session, as for the
session picker or for a terminal to learn its session from, calls it
with the agent, its :sessions, see `lispm-agents-define', and RECENT,
and it returns what that returns.  RECENT non-nil says that sessions
read a while ago will do, or none yet: who asks asks again once a
session reader has newer ones, see `lispm-agents-sessions-arrived'.
A host that reads the sessions in the background may wrap this with
`add-function' and answer with what it read when that is new enough,
calling the function it wraps otherwise.")

(defun lispm-agents-read-sessions (_agent sessions &optional _recent)
  "Return what SESSIONS, the :sessions of an agent, returns."
  (funcall sessions))

(defun lispm-agents--agent-sessions (agent props &optional reader recent)
  "Return the sessions of AGENT, whose properties are PROPS, as its
:sessions reads them through `lispm-agents-read-sessions-function',
RECENT passed on, or as READER, its session reader, has them; READER
may say `pending' instead, which is returned as it is.  An agent whose
sessions cannot be read lists none, and says so."
  (condition-case err
      (let ((sessions (if reader
                          (funcall reader)
                        (funcall lispm-agents-read-sessions-function
                                 agent (plist-get props :sessions) recent))))
        (if (and reader (eq sessions 'pending))
            'pending
          (let ((sessions (delq nil (copy-sequence sessions))))
            (if-let* ((normalize (plist-get props :normalize)))
                (funcall normalize sessions)
              sessions))))
    (error (message (lispm-tr "%s sessions unavailable: %s") (plist-get props :name)
                    (error-message-string err))
           nil)))

(defun lispm-agents--sort-sessions (sessions)
  "Return SESSIONS, one transcript per id, most recently opened first.
Transcript modification times select the freshest copy, never its position."
  (mapcar #'cddr
          (sort (mapcar (lambda (s)
                          (cons (lispm-agents-session-opened-time s)
                                (cons (lispm-agents-session-key s) s)))
                        (lispm-agents--newest-per-id sessions))
                (lambda (a b)
                  (if (= (car a) (car b))
                      (string< (cadr a) (cadr b))
                    (> (car a) (car b)))))))

(defun lispm-agents-session-tree (sessions)
  "Return SESSIONS as a tree: a list of (SESSION . CHILDREN), SESSION
one that no other of SESSIONS started, CHILDREN those it started, as
their :parent says, see `lispm-agents-define', in turn as subtrees.
Both keep the order of SESSIONS.  A session whose parent is not among
SESSIONS, nor among those under it, stands on its own."
  (let ((children (make-hash-table :test #'equal))
        (ids (make-hash-table :test #'equal))
        (parents (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (cons (plist-get session :agent) (plist-get session :id)) t ids))
    (dolist (session sessions)
      (let* ((key (cons (plist-get session :agent) (plist-get session :id)))
             (parent (cons (plist-get session :agent) (plist-get session :parent))))
        (when (and (cdr parent) (not (equal key parent)) (gethash parent ids))
          (puthash key parent parents))))
    ;; Broken metadata must not hide a whole cycle of sessions.  Cut one
    ;; edge per cycle before recursively building the forest.
    (dolist (session sessions)
      (let ((key (cons (plist-get session :agent) (plist-get session :id))) path)
        (while (and key (not (gethash key visited)))
          (puthash key 'visiting visited)
          (push key path)
          (setq key (gethash key parents)))
        (when (and key (eq (gethash key visited) 'visiting))
          (remhash key parents))
        (dolist (seen path) (puthash seen 'done visited))))
    (cl-labels ((key (session parent)
                  (cons (plist-get session :agent) parent))
                (subtree (session)
                  (cons session (mapcar #'subtree
                                        (nreverse (gethash (key session (plist-get session :id))
                                                           children))))))
      (let ((roots nil))
        (dolist (session sessions)
          (let ((parent (gethash (key session (plist-get session :id)) parents)))
            (if parent
                (push session (gethash parent children))
              (push session roots))))
        (mapcar #'subtree (nreverse roots))))))

(defun lispm-agents-sessions-list (&optional recent)
  "Return every recorded session of the agents that can run here, newest
first, see `lispm-agents-available'.  An agent whose sessions cannot
be read lists none, and says so.  Each agent's :sessions reads them as
they are now, through `lispm-agents-read-sessions-function'; session
readers are never asked, so the list is complete, see
`lispm-agents-sessions-snapshot'.  RECENT non-nil takes sessions read a
while ago, or none yet, as that function says.  The terminals running
sessions learn theirs from them, see `lispm-agents-refresh-buffer-titles'."
  (let ((sessions (lispm-agents--newest-per-id
                   (mapcan (pcase-lambda (`(,agent . ,props))
                             (lispm-agents--agent-sessions agent props nil recent))
                           (lispm-agents-available)))))
    (lispm-agents--index-session-parents sessions)
    (when lispm-agents-refresh-buffer-titles-function
      (funcall lispm-agents-refresh-buffer-titles-function sessions))
    (lispm-agents--sort-sessions sessions)))

;;;;; Session readers
;;
;; Reading every transcript can take seconds.  An agent may have a reader
;; besides its :sessions that never waits, its sessions read in the
;; background, off Emacs's thread: a list of the sessions draws what the
;; readers have, and again as they get more; the session picker still
;; reads every session as it is.

(defvar lispm-agents-session-readers nil
  "The agents' session readers, as (AGENT . READER).
See `lispm-agents-add-session-reader'.")

(defvar lispm-agents-reread-sessions-hook nil
  "Hook run when the sessions must be read anew, before a list of them
draws them: when it is asked to, as by a Refresh button, and once a
session was shown, renamed, archived or deleted,
see `lispm-agents-sessions-changed-hook'.  A session reader, see
`lispm-agents-add-session-reader', forgets what it has and reads
again.  A list drawn again for anything else, an agent's status, an
agent exiting, the agents changing, keeps what the readers have.")

(defvar lispm-agents-sessions-arrived-hook nil
  "Hook run when a session reader has newer sessions, see
`lispm-agents-sessions-arrived'.  A list of the sessions shows them
once Emacs is idle.")

(defun lispm-agents-sessions-arrived ()
  "Say that a session reader has newer sessions than lists of them show.
A reader, see `lispm-agents-add-session-reader', calls this once
what it read in the background is in: a list of the sessions draws
them again once Emacs is idle, shown or not, and no reader is asked
to read anew.  This runs `lispm-agents-sessions-arrived-hook'."
  (run-hooks 'lispm-agents-sessions-arrived-hook))

(defun lispm-agents-add-session-reader (agent reader)
  "Make READER the reader of AGENT's sessions for lists of the sessions.
READER is called with no argument and returns at once, never waiting:
AGENT's sessions as its :sessions returns them, see
`lispm-agents-define', or the symbol `pending' while it has none
to give yet, and a list then says it is still reading.  A reader
reading in the background calls `lispm-agents-sessions-arrived'
when it has newer sessions, and reads anew on
`lispm-agents-reread-sessions-hook'.

Only such lists ask readers, see `lispm-agents-sessions-snapshot';
`lispm-agents-sessions-list', which the session picker reads,
always reads the sessions themselves.  AGENT has one reader at most:
adding another replaces it.  Lists draw the sessions again once Emacs
is idle.  Return AGENT."
  (setf (alist-get agent lispm-agents-session-readers) reader)
  (lispm-agents-sessions-arrived)
  agent)

(defun lispm-agents-remove-session-reader (agent)
  "Remove the session reader of AGENT, see
`lispm-agents-add-session-reader'.  Lists read AGENT's
sessions with its :sessions again, once Emacs is idle."
  (when (assq agent lispm-agents-session-readers)
    (setq lispm-agents-session-readers
          (seq-remove (lambda (entry) (eq (car entry) agent))
                      lispm-agents-session-readers))
    (lispm-agents-sessions-arrived)))

(defvar lispm-agents-session-lookup-functions nil
  "Functions looking a session up in what was read in the background.
Each is called with an agent and the id of one of its sessions, and
returns the session as the agent's :sessions lists it, see
`lispm-agents-define', or nil when it does not know it, which may be too
new for it; the first to know it answers.  None reads a transcript or
waits.  The session may add :turn-ended: non-nil when its latest turn
had ended, nil when it had not, as its transcript was at its :time, see
`lispm-agents-known-turn'.  The session must not be modified.")

(defun lispm-agents-known-session (agent id)
  "Return the session ID of AGENT as what was read in the background has
it, see `lispm-agents-session-lookup-functions', or nil."
  (run-hook-with-args-until-success 'lispm-agents-session-lookup-functions agent id))

(defun lispm-agents-known-turn (agent id &optional file)
  "Return how the latest turn of AGENT's session ID stands, as what was
read in the background knows, see `lispm-agents-known-session': `ended'
when it had ended, `working' when it had not, or when the session's
transcript changed since, as it does while the agent works; nil when
that does not say.  FILE, when given, is the transcript the caller would
read, which the session's must be."
  (when-let* ((session (lispm-agents-known-session agent id))
              ((plist-member session :turn-ended))
              (transcript (plist-get session :file))
              (time (plist-get session :time))
              ((numberp time))
              ((or (null file) (equal (expand-file-name file) (expand-file-name transcript))))
              (attributes (file-attributes transcript)))
    (if (and (= time (float-time (file-attribute-modification-time attributes)))
             (plist-get session :turn-ended))
        'ended
      'working)))

(defun lispm-agents-sessions-snapshot ()
  "Return the sessions as they are known now, (SESSIONS . PENDING).
SESSIONS are as `lispm-agents-sessions-list' returns them, but an
agent with a session reader lists what its reader has, without waiting,
see `lispm-agents-add-session-reader'; PENDING are the agents
whose readers have nothing yet, in the order agents are offered.  Agents
without a reader are read as they are now.  A list of the sessions
draws this."
  (let* ((pending nil)
         (sessions
          (lispm-agents--newest-per-id
           (mapcan (pcase-lambda (`(,agent . ,props))
                     (let ((sessions (lispm-agents--agent-sessions
                                      agent props (alist-get agent lispm-agents-session-readers))))
                       (if (eq sessions 'pending)
                           (progn (push agent pending) nil)
                         sessions)))
                   (lispm-agents-available)))))
    (lispm-agents--index-session-parents sessions)
    (when lispm-agents-refresh-buffer-titles-function
      (funcall lispm-agents-refresh-buffer-titles-function sessions))
    (cons (lispm-agents--sort-sessions sessions) (nreverse pending))))

;;;; Presentation (pure)

(defun lispm-agents-age (time now)
  "Return a short human age for TIME relative to NOW, both float seconds,
in the language spoken: 2m, 3h or 4d in English.  Past a month it is the
date, short: 5/13/26."
  (let ((seconds (- now time)))
    (cond ((< seconds 3600)
           (let ((minutes (truncate seconds 60)))
             (lispm-tr "%dm" minutes)))
          ((< seconds 86400)
           (let ((hours (truncate seconds 3600)))
             (lispm-tr "%dh" hours)))
          ((< seconds (* 30 86400))
           (let ((days (truncate seconds 86400)))
             (lispm-tr "%dd" days)))
          (t (lispm-format-time "%-m/%-d/%y" time)))))

(defun lispm-agents-short-id (session)
  "Return the short form of SESSION's id that tells sessions apart: what
its agent's :short-id makes of it, else its first eight characters, as
of a UUID."
  (let ((id (plist-get session :id)))
    (if-let* ((short (plist-get (lispm-agents-props (plist-get session :agent)) :short-id)))
        (funcall short id)
      (substring id 0 (min 8 (length id))))))

(defconst lispm-agents-label-faces
  '(font-lock-function-name-face font-lock-keyword-face font-lock-type-face
    font-lock-constant-face font-lock-variable-name-face)
  "Faces telling the agents apart in the session list, one per agent in turn.")

(defun lispm-agents--label-face (agent)
  "Return the face of AGENT's name in the session list: the agents built
in keep theirs, whichever extensions were loaded before them."
  (let ((index (or (seq-position (mapcar #'car (lispm-agents-ordered)) agent) 0)))
    (nth (mod index (length lispm-agents-label-faces)) lispm-agents-label-faces)))

(defun lispm-agents-one-line (text width)
  "Return TEXT collapsed to a single line and truncated to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
   width nil nil "…"))

(defun lispm-agents-session-label (session now)
  "Return the completion label for SESSION, with ages relative to NOW.
The short id suffix keeps labels unique across sessions with equal titles."
  (format "%s %s %s %s %s"
          (propertize (format "%-6s" (plist-get session :agent))
                      'face (lispm-agents--label-face (plist-get session :agent)))
          (propertize (format "%-18s" (truncate-string-to-width
                                       (file-name-nondirectory
                                        (directory-file-name (plist-get session :cwd)))
                                       18))
                      'face 'font-lock-constant-face)
          ;; Padded by its width: an age may be written in Chinese.
          (propertize (let ((age (lispm-agents-age (plist-get session :time) now)))
                        (concat age (make-string (max 0 (- 10 (string-width age))) ?\s)))
                      'face 'shadow)
          (lispm-agents-one-line (concat (and (plist-get session :parent) "↳ ")
                                         (plist-get session :title))
                                 lispm-agents-title-width)
          (propertize (lispm-agents-short-id session) 'face 'shadow)))


;;;; Session management

(defconst lispm-agents-state-file (locate-user-emacs-file "agent-sessions.eld")
  "File holding Emacs-side archives and session opening times.
Archiving hides a session in Emacs only; it never alters agent data.")

(defvar lispm-agents--archived 'unloaded
  "Keys of explicitly archived sessions, or `unloaded' before reading state.")

(defvar lispm-agents--session-parents nil
  "Parent index as (STATE-FILE . HASH-TABLE), built before filtering sessions.
Keys include the provider, just as the persisted archive keys do.")

(defvar lispm-agents--effective-archive nil
  "Cached inherited archive state as (STATE-FILE KEYS PARENTS HASH-TABLE).
KEYS and PARENTS are the explicit archive list and parent index used.")

(defvar lispm-agents-opened-times nil
  "Cached opening times as (STATE-FILE . HASH-TABLE), keyed by session.")

(defun lispm-agents-session-key (session)
  "Return the key identifying SESSION across agents, \"AGENT:ID\"."
  (format "%s:%s" (plist-get session :agent) (plist-get session :id)))

(defun lispm-agents--index-session-parents (sessions &optional merge)
  "Index parent links from unfiltered SESSIONS, including absent parents.
Archive inheritance is independent of project directories and of whether
the parent transcript itself can still be read.  Each new reading replaces
the index, so changed parent metadata cannot leave stale relationships.
With MERGE, copy the known graph and update only supplied rows, for callers
with partial records.  Binding the index and effective cache lets a caller
use such a graph locally without replacing a complete session reading."
  (let ((parents (if (and merge (equal (car lispm-agents--session-parents) lispm-agents-state-file))
                     (copy-hash-table (cdr lispm-agents--session-parents))
                   (make-hash-table :test #'equal))))
    (dolist (session sessions)
      (let ((key (lispm-agents-session-key session)))
        (if-let* ((parent (plist-get session :parent)))
            (puthash key (lispm-agents-session-key (list :agent (plist-get session :agent) :id parent))
                     parents)
          (remhash key parents))))
    (setq lispm-agents--session-parents (cons lispm-agents-state-file parents))))

(defun lispm-agents--effective-archive ()
  "Return a hash of explicit archive keys and all their known descendants.
Build it once per reading or archive change, in linear time.  Missing
parents still convey their archive state; cycles terminate at visited keys."
  (let* ((keys (lispm-agents-archived-keys))
         (parents (and (equal (car lispm-agents--session-parents) lispm-agents-state-file)
                       (cdr lispm-agents--session-parents)))
         (cached lispm-agents--effective-archive))
    (unless (and (equal (car cached) lispm-agents-state-file)
                 (eq (nth 1 cached) keys)
                 (eq (nth 2 cached) parents))
      (let ((children (make-hash-table :test #'equal))
            (archived (make-hash-table :test #'equal))
            (pending (copy-sequence keys)))
        (when parents
          (maphash (lambda (child parent)
                     (puthash parent (cons child (gethash parent children)) children))
                   parents))
        (while pending
          (let ((key (pop pending)))
            (unless (gethash key archived)
              (puthash key t archived)
              (dolist (child (gethash key children))
                (push child pending)))))
        (setq lispm-agents--effective-archive
              (list lispm-agents-state-file keys parents archived))))
    (nth 3 lispm-agents--effective-archive)))

(defun lispm-agents--read-state ()
  "Return the plist the state file holds, or nil without one.
A file holding no such plist is moved aside, to the same name ending in
.unreadable, and said so, for the next write would replace it."
  (let ((file lispm-agents-state-file))
    (when (file-readable-p file)
      (let ((state (with-temp-buffer
                     (insert-file-contents file)
                     (condition-case nil (read (current-buffer)) (error 'unreadable)))))
        (if (and (plistp state) (listp (plist-get state :archived))
                 (listp (plist-get state :opened)))
            state
          (let ((aside (concat file ".unreadable")))
            (rename-file file aside t)
            (message (lispm-tr "Cannot read %s; moved to %s")
                     (abbreviate-file-name file) (abbreviate-file-name aside)))
          nil)))))

(defun lispm-agents-archived-keys ()
  "Return the archived session keys, reading the state file on first use."
  (when (eq lispm-agents--archived 'unloaded)
    (setq lispm-agents--archived
          (plist-get (lispm-agents--read-state) :archived)))
  lispm-agents--archived)

(defun lispm-agents-opened-times ()
  "Return session opening times, reading the current state file once."
  (unless (equal (car lispm-agents-opened-times) lispm-agents-state-file)
    (let ((times (make-hash-table :test #'equal)))
      (dolist (entry (plist-get (lispm-agents--read-state) :opened))
        (when (and (consp entry) (stringp (car entry)) (numberp (cdr entry)))
          (puthash (car entry) (cdr entry) times)))
      (setq lispm-agents-opened-times (cons lispm-agents-state-file times))))
  (cdr lispm-agents-opened-times))

(defun lispm-agents-session-opened-time (session)
  "Return when SESSION was last opened, or originally started.
Sessions without either timestamp sort last.  :time remains transcript
freshness and is never used for ordering."
  (or (gethash (lispm-agents-session-key session) (lispm-agents-opened-times))
      (plist-get session :started)
      0))

(defun lispm-agents--write-state (state)
  "Atomically save STATE without replacing unrelated session metadata."
  (let ((part (concat lispm-agents-state-file ".part")))
    (make-directory (file-name-directory lispm-agents-state-file) t)
    (with-temp-file part
      (let ((print-length nil) (print-level nil))
        (prin1 state (current-buffer))))
    (rename-file part lispm-agents-state-file t)))

(defun lispm-agents-session-opened (agent id &optional time)
  "Record that AGENT's session ID was launched or resumed at TIME.
TIME defaults to now.  Merely selecting an already live session must
not call this.  The ordering survives closing Emacs and archiving."
  (when id
    (let* ((key (lispm-agents-session-key (list :agent agent :id id)))
           (time (or time (float-time)))
           (times (lispm-agents-opened-times))
           (state (lispm-agents--read-state)))
      (puthash key time times)
      (lispm-agents--write-state
       (plist-put state :opened
                  (cons (cons key time) (assoc-delete-all key (plist-get state :opened))))))))

(defun lispm-agents-archived-p (session &optional archived-map)
  "Return non-nil when SESSION or one of its ancestors is archived.
Session lists index every parent link before callers filter their rows.
Only explicit archive choices are persisted; descendants inherit them.
Optional ARCHIVED-MAP overrides the effective archive hash table."
  (let ((archived (or archived-map (lispm-agents--effective-archive))))
    (or (gethash (lispm-agents-session-key session) archived)
        (when-let* ((parent (plist-get session :parent)))
          (gethash (lispm-agents-session-key (list :agent (plist-get session :agent) :id parent))
                   archived)))))

(defun lispm-agents-archived-snapshot ()
  "Read archive keys as `(KEYS . KNOWN)' without mutating the state file.
Malformed or oversized state files return nil so KNOWN is nil."
  (if (not (eq lispm-agents--archived 'unloaded))
      (cons lispm-agents--archived t)
    (if (not (file-exists-p lispm-agents-state-file))
        (cons nil t)
      (condition-case nil
          (when (<= (file-attribute-size (file-attributes lispm-agents-state-file)) 65536)
            (with-temp-buffer
              (insert-file-contents lispm-agents-state-file)
              (let ((state (read (current-buffer))))
                (when (and (plistp state) (listp (plist-get state :archived))
                           (seq-every-p #'stringp (plist-get state :archived)))
                  (cons (plist-get state :archived) t)))))
        ((file-error invalid-read-syntax end-of-file) nil)))))

(cl-defun lispm-agents-archive-map (sessions &key extra-edges (archived nil archived-p))
  "Build an inherited archive hash table from SESSIONS without mutating globals.
Optional EXTRA-EDGES is an alist of `(CHILD-KEY . PARENT-KEY)' links.
When ARCHIVED is supplied, resolve against those explicit keys instead."
  (let ((lispm-agents--session-parents lispm-agents--session-parents)
        (lispm-agents--effective-archive nil))
    (cl-progv (when archived-p '(lispm-agents--archived))
        (when archived-p (list archived))
      (lispm-agents--index-session-parents sessions t)
      (dolist (edge extra-edges)
        (puthash (car edge) (cdr edge) (cdr lispm-agents--session-parents)))
      (lispm-agents--effective-archive))))

(defun lispm-agents-set-archived (session archived)
  "Mark SESSION explicitly ARCHIVED or not and persist the archive choices.
Inherited descendants are not written individually, so restoring a parent
restores them without changing any independently archived child."
  (let ((key (lispm-agents-session-key session))
        (keys (lispm-agents-archived-keys)))
    (lispm-agents-save-archived (if archived
                                   (cons key (remove key keys))
                                 (remove key keys)))))

(defun lispm-agents-save-archived (keys)
  "Persist the explicit archive KEYS and invalidate inherited state."
  (setq lispm-agents--archived keys
        lispm-agents--effective-archive nil)
  (lispm-agents--write-state
   (plist-put (lispm-agents--read-state) :archived keys)))

(defun lispm-agents-archived-ancestors (session)
  "Return the explicitly archived keys of SESSION and its known ancestors.
The same-provider parent index is walked only for this user action, never
once per row.  A cycle visits each key at most once."
  (let ((parents (and (equal (car lispm-agents--session-parents) lispm-agents-state-file)
                      (cdr lispm-agents--session-parents)))
        (explicit (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (key (lispm-agents-session-key session))
        (parent (when-let* ((id (plist-get session :parent)))
                  (lispm-agents-session-key (list :agent (plist-get session :agent) :id id))))
        result)
    (dolist (archived (lispm-agents-archived-keys))
      (puthash archived t explicit))
    (while (and key (not (gethash key seen)))
      (puthash key t seen)
      (when (gethash key explicit) (push key result))
      (setq key (or parent (and parents (gethash key parents)))
            parent nil))
    result))


(provide 'lispm-agents-sessions)

;;; lispm-agents-sessions.el ends here
