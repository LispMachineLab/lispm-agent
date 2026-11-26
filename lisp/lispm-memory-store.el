;;; lispm-memory-store.el --- Org memory storage, task lists, git commits, and prompt  -*- lexical-binding: t; -*-

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

;; Persistent memory and task storage backed by Org files under git,
;; git commit batching for `lispm-directory', and memory sections for
;; agent system prompts.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'lispm-core)

(defcustom lispm-git t
  "Whether to commit memory, tasks and finished turns to git.
The repository is `lispm-directory' itself."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-memory-max-entries 200
  "Most entries a memory holds before it consolidates itself.
Past this, or past `lispm-memory-max-tokens', a memory (a project's, a
role's or the system's) is consolidated once a turn of an agent using
it has ended and Emacs is idle (`lispm-idle-delay'): a model brings it
back to three quarters of its budget, merging entries that say the
same, folding old entries seldom recalled into summary entries, and
dropping stale ones.  Each consolidation is a commit, which says what
was merged, folded or dropped, and why: `git revert' undoes it; so
memory is consolidated only while it is under git (`lispm-git').  A
memory is not tried again before it says something new, nor for some
hours after a plan that failed or freed nothing.  A system prompt
lists this many titles at most, of all the memories it lists, those
used most lately, and says how many more there are.  nil sets no
limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'lispm)

(defcustom lispm-memory-max-tokens 30000
  "Most tokens a memory holds before it consolidates itself.
A token is counted for every three bytes of its file.  See
`lispm-memory-max-entries'.  nil sets no limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'lispm)

(defcustom lispm-memory-model nil
  "Model consolidating memory and rolling up episodes, on the agent's backend.
A small, cheap model does; nil is the agent's own model."
  :type '(choice (const :tag "The agent's model" nil) string)
  :group 'lispm)

(defun lispm-memory-home (agent)
  "The scope of AGENT's own memory: `system', `self' or `project'.
An agent of the whole system (see `lispm-make-agent') works in no
project; its memory is the system's.  Nor does a character's own
conversation (see `lispm-character-home-p'), whose memory is the
character's."
  (cond ((lispm-agent-system agent) 'system)
        ((lispm-character-home-p agent) 'self)
        (t 'project)))

(defun lispm-memory-scope (agent scope)
  "SCOPE as AGENT has it: nil and `project' stand for its own memory.
An error for `self' if AGENT is no character's."
  (cond ((memq scope '(nil project)) (lispm-memory-home agent))
        ((and (eq scope 'self) (not (lispm-agent-character agent)))
         (error "You are no character, with a memory of your own: use project or role"))
        (t scope)))

(defun lispm-memory-scopes (agent)
  "The scopes of the memory AGENT reads, its own first: a list."
  (delete-dups (mapcar (lambda (scope) (lispm-memory-scope agent scope))
                       (if (lispm-agent-character agent) '(self project role) '(project role)))))

(defun lispm-memory-file (agent &optional scope role)
  "File of AGENT's memory in SCOPE.
SCOPE `project' (the default) is AGENT's own: its project's, shared by
the agents of the project, or, for an agent of the whole system, the
system's, as SCOPE `system' is; `role' is shared by the agents of its
role, or of ROLE, across projects; `self' is its character's, in all
of the character's conversations."
  (expand-file-name "memory.org"
                    (pcase (lispm-memory-scope agent scope)
                      ('role (lispm-role-directory (or role (lispm-agent-role agent))))
                      ('system (lispm-system-directory))
                      ('self (lispm-character-directory (lispm-agent-character agent)))
                      (_ (lispm-project-directory (lispm-agent-root agent))))))

(defun lispm-tasks-file (agent)
  "File of the tasks of AGENT's project, or of the system's for a system agent."
  (expand-file-name "tasks.org" (lispm-home-directory agent)))

(defun lispm-memory-visit (file header &optional to-change)
  "Buffer visiting FILE, created with HEADER if new.
The user may change the file too, by hand or with git, so a buffer out
of date is brought up to date first (see `lispm-tools-sync').  One
with unsaved changes while the file changed on disk too is read as it
is, being what the user sees, but cannot be changed: with TO-CHANGE,
that is an error."
  (let ((buffer (lispm-tools-visit file (not to-change))))
    (with-current-buffer buffer
      ;; Not over a file changed on disk: initialize atomically when absent.
      (when (and (zerop (buffer-size)) (eq (lispm-tools-state buffer) 'current))
        (insert header)
        (org-mode)
        (if (file-exists-p file)
            (lispm-memory-save buffer)
          (condition-case nil
              (progn
                (let ((coding-system-for-write (or coding-system-for-write 'utf-8-unix)))
                  (lispm-write-file-atomic file header :missing))
                (set-visited-file-modtime)
                (set-buffer-modified-p nil))
            (error (lispm-tools-sync buffer))))))
    buffer))

(defalias 'lispm-memory--visit #'lispm-memory-visit)

(defun lispm-memory-buffer (agent &optional scope role to-change)
  "Buffer of AGENT's memory in SCOPE, or of ROLE's (see `lispm-memory-file').
TO-CHANGE is as for `lispm-memory-visit'."
  (lispm-memory-visit (lispm-memory-file agent scope role)
                       (pcase (lispm-memory-scope agent scope)
                         ('role (format "#+title: Memory of the %s role\n"
                                        (or role (lispm-agent-role agent))))
                         ('system "#+title: Memory of the system\n")
                         ('self (format "#+title: Memory of %s\n"
                                        (if-let* ((character (lispm-agent-character* agent)))
                                            (lispm-character-title* character)
                                          (lispm-agent-character agent))))
                         (_ (format "#+title: Memory of %s\n"
                                    (abbreviate-file-name (lispm-agent-root agent)))))
                       to-change))

(defun lispm-memory--scope-entries (agent scope &optional role)
  "Entries of AGENT's memory in SCOPE (or ROLE's), without creating its file.
Each has its :scope (`project', `role', `system' or `self') and its
:file too."
  (let ((file (lispm-memory-file agent scope role))
        (scope (lispm-memory-scope agent scope)))
    (and (file-exists-p file)
         (mapcar (lambda (e) (plist-put (plist-put e :scope scope) :file file))
                 (lispm-memory-entries (lispm-memory-buffer agent scope role))))))

(defun lispm-tasks--buffer (agent &optional to-change)
  "Buffer of the tasks of AGENT's project, or of the system's.
TO-CHANGE is as for `lispm-memory-visit'."
  (lispm-memory-visit (lispm-tasks-file agent)
                       (format "#+title: Tasks of %s\n#+todo: TODO NEXT BLOCKED | DONE CANCELLED\n"
                               (if (lispm-agent-system agent)
                                   "the system"
                                 (abbreviate-file-name (lispm-agent-root agent))))
                       to-change))

(defvar lispm-memory--recall-stats (make-hash-table :test #'equal)
  "Canonical memory file path -> table of downcased title -> (DELTA . STAMP).")

(defun lispm-memory--recall-key (title)
  "Return canonical lookup key for memory TITLE."
  (downcase (string-trim (or title ""))))

(defun lispm-memory-recall-stat (file title)
  "Return pending in-memory `(DELTA . STAMP)' telemetry for FILE and TITLE."
  (when-let* ((canon (and file (expand-file-name file)))
              (tbl (gethash canon lispm-memory--recall-stats)))
    (gethash (lispm-memory--recall-key title) tbl)))

(defun lispm-memory-clear-recall-stats (file &optional title)
  "Clear pending in-memory recall telemetry for FILE, or just TITLE within it."
  (when-let* ((canon (and file (expand-file-name file)))
              (tbl (gethash canon lispm-memory--recall-stats)))
    (if title
        (progn
          (remhash (lispm-memory--recall-key title) tbl)
          (when (zerop (hash-table-count tbl))
            (remhash canon lispm-memory--recall-stats)))
      (remhash canon lispm-memory--recall-stats))))

(defun lispm-memory--overlay-recalls (file entries)
  "Return ENTRIES with pending in-memory recall telemetry overlaid for FILE."
  (if (not (and file (gethash (expand-file-name file) lispm-memory--recall-stats)))
      entries
    (mapcar (lambda (entry)
              (if-let* ((stat (lispm-memory-recall-stat file (plist-get entry :title))))
                  (let* ((copy (copy-sequence entry))
                         (old-recalls (or (plist-get copy :recalls) 0))
                         (old-recalled (plist-get copy :recalled))
                         (new-recalled (cdr stat)))
                    (plist-put copy :recalls (+ old-recalls (car stat)))
                    (when (or (null old-recalled) (string> new-recalled old-recalled))
                      (plist-put copy :recalled new-recalled))
                    copy)
                entry))
            entries)))

(defun lispm-memory-entries (buffer)
  "Top-level entries of the org BUFFER, as plists.
Keys: :title :tags :todo :id :body :beg :end, and the properties
:created :updated :recalled (timestamps, or nil) and :recalls (a
number)."
  (let ((entries
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (let (starts)
                 (while (re-search-forward "^\\* " nil t)
                   (push (match-beginning 0) starts))
                 (cl-loop for (beg . rest) on (nreverse starts)
                          for end = (or (car rest) (point-max))
                          collect
                          (progn
                            (goto-char beg)
                            (list :title (org-get-heading t t t t)
                                  :tags (org-get-tags nil t)
                                  :todo (org-get-todo-state)
                                  :id (org-entry-get nil "ID")
                                  :created (org-entry-get nil "CREATED")
                                  :updated (org-entry-get nil "UPDATED")
                                  :recalled (org-entry-get nil "RECALLED")
                                  :recalls (string-to-number (or (org-entry-get nil "RECALLS") "0"))
                                  :body (progn (org-end-of-meta-data t)
                                               (string-trim
                                                (org-unescape-code-in-string
                                                 (buffer-substring-no-properties
                                                  (min (point) end) end))))
                                  :beg beg :end end)))))))))
    (lispm-memory--overlay-recalls (buffer-file-name buffer) entries)))

(defalias 'lispm-memory--entries #'lispm-memory-entries)

(defun lispm-memory-save (buffer &optional expected-hash)
  "Atomically write BUFFER to disk, checking EXPECTED-HASH when non-nil."
  (with-current-buffer buffer
    (let* ((file (or buffer-file-name
                     (error "Buffer %s visits no file" (buffer-name buffer))))
           (base (and buffer-file-coding-system
                      (coding-system-base buffer-file-coding-system)))
           (coding (if (memq base '(nil undecided us-ascii))
                       'utf-8-unix
                     buffer-file-coding-system))
           (text (save-restriction
                   (widen)
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (let ((coding-system-for-write (or coding-system-for-write coding)))
        (lispm-write-file-atomic file text expected-hash))
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))))

(defun lispm-memory-commit-buffer (buffer fn)
  "Run FN mutating BUFFER and save atomically with SHA-256 CAS verification.
Restore BUFFER's previous text without advancing modtime on CAS conflict."
  (with-current-buffer buffer
    (let* ((file (or buffer-file-name
                     (error "Buffer %s visits no file" (buffer-name buffer))))
           (expected (or (lispm-file-sha256 file) :missing))
           (snapshot (save-restriction
                       (widen)
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (cl-letf (((symbol-function 'ask-user-about-supersession-threat) #'ignore))
        (condition-case err
            (prog1 (funcall fn)
              (lispm-memory-save buffer expected))
          (error
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (save-restriction
                 (widen)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert snapshot)))
               (set-buffer-modified-p nil)))
           (signal (car err) (cdr err))))))))

(defun lispm-memory--tag (tag)
  "TAG made valid for org."
  (replace-regexp-in-string "[^[:alnum:]_@#%]" "_" (format "%s" tag)))

(defun lispm-memory-stamp (&optional time)
  "An inactive org timestamp for TIME, or for now."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun lispm-memory-entry-text (title tags properties content)
  "An entry of a memory file: TITLE with TAGS, PROPERTIES and CONTENT.
PROPERTIES is an alist (NAME . VALUE); those whose VALUE is nil are
left out."
  (concat "* " (lispm-oneline title 120)
          (if tags (format "  :%s:" (mapconcat #'lispm-memory--tag tags ":")) "")
          "\n:PROPERTIES:\n"
          (mapconcat (pcase-lambda (`(,name . ,value))
                       (if value
                           (format ":%s:%s%s\n" name
                                   (make-string (max 1 (- 9 (length name))) ?\s) value)
                         ""))
                     properties "")
          ":END:\n"
          (org-escape-code-in-string (string-trim content)) "\n"))

;;;; Memory

(defun lispm-memory-remember (agent title content &optional tags scope)
  "Store CONTENT under TITLE in AGENT's memory in SCOPE.
An entry of that title is replaced; when it was saved and recalled
stays on record."
  (let* ((scope (lispm-memory-scope agent scope))
         (buffer (lispm-memory-buffer agent scope nil t))
         (file (buffer-file-name buffer))
         (title (lispm-oneline title 120))
         (existing (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                             (lispm-memory-entries buffer)))
         (stat (and existing (lispm-memory-recall-stat file title)))
         ;; The past of the entry replaced: when it was saved, recalled, merged.
         (past (and existing
                    (with-current-buffer buffer
                      (save-restriction
                        (widen)
                        (mapcar (lambda (name) (cons name (org-entry-get (plist-get existing :beg) name)))
                                '("CREATED" "RECALLED" "RECALLS" "MERGED")))))))
    (when stat
      (let* ((old-recalls (string-to-number (or (cdr (assoc "RECALLS" past)) "0")))
             (total-recalls (+ old-recalls (car stat)))
             (old-recalled (cdr (assoc "RECALLED" past)))
             (latest-recalled (if (and old-recalled (string> old-recalled (cdr stat)))
                                  old-recalled
                                (cdr stat))))
        (setf (alist-get "RECALLS" past nil nil #'equal) (number-to-string total-recalls))
        (setf (alist-get "RECALLED" past nil nil #'equal) latest-recalled)))
    (lispm-memory-commit-buffer
     buffer
     (lambda ()
       (save-excursion
         (save-restriction
           (widen)
           (if existing
               (progn (delete-region (plist-get existing :beg) (plist-get existing :end))
                      (goto-char (plist-get existing :beg)))
             (goto-char (point-max))
             (unless (bolp) (insert "\n")))
           (insert (lispm-memory-entry-text
                    title tags
                    `(("CREATED" . ,(or (cdr (assoc "CREATED" past)) (lispm-memory-stamp)))
                      ("UPDATED" . ,(and existing (lispm-memory-stamp)))
                      ,@(assoc-delete-all "CREATED" past))
                    content))))))
    (lispm-memory-clear-recall-stats file title)
    (lispm-git-commit agent (format "%s: %s \"%s\"" (lispm-agent-name agent)
                                    (if existing "update memory" "remember") title)
                      (list file))
    (format "%s %s memory \"%s\"." (if existing "Updated" "Saved") scope title)))

(defun lispm-memory--label (entry)
  "Title of a memory ENTRY, with its tags and, for role memory and a
character's own, its scope."
  (concat (plist-get entry :title)
          (if (plist-get entry :tags)
              (format " [%s]" (string-join (plist-get entry :tags) ", "))
            "")
          (pcase (plist-get entry :scope)
            ('role " (role memory)")
            ('self " (your own)")
            (_ ""))))

(defun lispm-memory--format (entry)
  "ENTRY of a memory file as text for the model."
  (concat "## " (lispm-memory--label entry) "\n" (plist-get entry :body)))

(defun lispm-memory--note-recalls (entries)
  "Record in memory that ENTRIES were recalled now, without touching disk.
Telemetry accumulates lock-free per file/title and is folded into disk
only during explicit writes or asynchronous consolidation."
  (let ((stamp (lispm-memory-stamp)))
    (dolist (entry entries)
      (when-let* ((raw-file (plist-get entry :file))
                  (file (expand-file-name raw-file))
                  (title (plist-get entry :title)))
        (let* ((tbl (or (gethash file lispm-memory--recall-stats)
                        (let ((new (make-hash-table :test #'equal)))
                          (puthash file new lispm-memory--recall-stats)
                          new)))
               (key (lispm-memory--recall-key title))
               (prev (gethash key tbl)))
          (puthash key (cons (1+ (or (car prev) 0)) stamp) tbl))))))

(defun lispm-memory-recall (agent query &optional role)
  "Entries of AGENT's memory matching every word of QUERY.
AGENT's own memory comes first, then that of its role, or of ROLE.  A
character's own memory comes before all.  The entries returned are
noted as recalled."
  (let* ((entries (mapcan (lambda (scope)
                            (lispm-memory--scope-entries
                             agent scope
                             (and role (eq scope 'role) (lispm-role-name (lispm-get-role role)))))
                          (lispm-memory-scopes agent)))
         (words (split-string (downcase query) "[ \t\n,]+" t))
         (titles (lambda (es) (mapconcat (lambda (e) (concat "- " (lispm-memory--label e))) es "\n")))
         (hits (seq-filter
                (lambda (e)
                  (let ((haystack (downcase (concat (plist-get e :title) " "
                                                    (string-join (plist-get e :tags) " ")
                                                    " " (plist-get e :body)))))
                    (seq-every-p (lambda (w) (string-search w haystack)) words)))
                entries)))
    (cond ((null entries) "Memory is empty.")
          ((null words) (concat "All memories:\n" (funcall titles entries)))
          ((null hits) (concat "Nothing matches. All memories:\n" (funcall titles entries)))
          (t (let ((shown (seq-take hits 20)))
               (lispm-memory--note-recalls shown)
               (mapconcat #'lispm-memory--format shown "\n\n"))))))

(defun lispm-memory-forget (agent title)
  "Delete the entry TITLE from AGENT's own memory, or else its role memory.
A character's own memory comes first."
  (let* ((entry (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                          (mapcan (lambda (scope) (lispm-memory--scope-entries agent scope))
                                  (lispm-memory-scopes agent))))
         (buffer (and entry (lispm-memory-buffer agent (plist-get entry :scope) nil t))))
    (unless entry (error "No memory titled \"%s\"" title))
    (let ((file (buffer-file-name buffer)))
      (lispm-memory-commit-buffer
       buffer
       (lambda ()
         (save-restriction
           (widen)
           (delete-region (plist-get entry :beg) (plist-get entry :end)))))
      (lispm-memory-clear-recall-stats file title)
      (lispm-git-commit agent (format "%s: forget \"%s\"" (lispm-agent-name agent) title)
                        (list file)))
    (format "Forgot \"%s\" (it remains in git history)." title)))

;;;; Tasks

(defconst lispm-tasks--states '("TODO" "NEXT" "BLOCKED" "DONE" "CANCELLED"))

(defun lispm-tasks--find (agent id)
  "The task of AGENT with ID, as an entry plist."
  (or (seq-find (lambda (e) (equal (plist-get e :id) id))
                (lispm-memory-entries (lispm-tasks--buffer agent)))
      (error "No task with ID %s" id)))

(defun lispm-tasks-add (agent title &optional notes)
  "Add a TODO titled TITLE (with NOTES) to AGENT's tasks; return its ID."
  (let* ((buffer (lispm-tasks--buffer agent t))
         (file (buffer-file-name buffer))
         (ids (mapcar (lambda (e) (plist-get e :id)) (lispm-memory-entries buffer)))
         (id (cl-loop for id = (format "T%04x" (random #x10000))
                      unless (member id ids) return id)))
    (lispm-memory-commit-buffer
     buffer
     (lambda ()
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* TODO " (lispm-oneline title 120) "\n"
                   ":PROPERTIES:\n:ID:       " id "\n"
                   ":CREATED:  " (lispm-memory-stamp) "\n:END:\n"
                   (if (and notes (not (string-blank-p notes)))
                       (concat (org-escape-code-in-string (string-trim notes)) "\n")
                     ""))))))
    (lispm-git-commit agent (format "%s: add task %s" (lispm-agent-name agent) id)
                      (list file))
    id))

(defun lispm-tasks-update (agent id &optional state note)
  "Set task ID of AGENT to STATE and/or append NOTE."
  (when (and state (not (member state lispm-tasks--states)))
    (error "State must be one of %s" (string-join lispm-tasks--states ", ")))
  (let* ((buffer (lispm-tasks--buffer agent t))
         (file (buffer-file-name buffer))
         (task (lispm-tasks--find agent id)))
    (lispm-memory-commit-buffer
     buffer
     (lambda ()
       (save-excursion
         (save-restriction
           (widen)
           (when note
             (goto-char (plist-get task :end))
             (unless (bolp) (insert "\n"))
             (insert "- " (lispm-memory-stamp) " " (lispm-oneline note 500) "\n"))
           (when state
             (goto-char (plist-get task :beg))
             (let ((case-fold-search nil))
               (if (looking-at "\\* \\(?:\\(TODO\\|NEXT\\|BLOCKED\\|DONE\\|CANCELLED\\) \\)?")
                   (replace-match (concat "* " state " ") t t)
                 (error "Malformed task heading"))))))))
    (lispm-git-commit agent (format "%s: task %s%s" (lispm-agent-name agent) id
                                    (if state (concat " -> " state) " note"))
                      (list file))
    (format "Task %s updated." id)))

(defun lispm-tasks-list (agent &optional include-done)
  "AGENT's tasks as text; closed ones only if INCLUDE-DONE."
  (let ((tasks (seq-filter (lambda (e)
                             (or include-done
                                 (not (member (plist-get e :todo) '("DONE" "CANCELLED")))))
                           (and (file-exists-p (lispm-tasks-file agent))
                                (lispm-memory-entries (lispm-tasks--buffer agent))))))
    (if tasks
        (mapconcat (lambda (e)
                     (format "%s %s %s%s" (plist-get e :id) (plist-get e :todo)
                             (plist-get e :title)
                             (if (string-empty-p (plist-get e :body)) ""
                               (concat "\n  " (replace-regexp-in-string
                                               "\n" "\n  " (plist-get e :body))))))
                   tasks "\n")
      "No tasks.")))

;;;; Git

(defcustom lispm-git-scope 'directory
  "Repository partitioning scope for archival commits under `lispm-directory'.
`directory' uses a single repository at `lispm-directory'.
`partitioned' creates independent per-scope Git repos for each project,
role, character, or system directory so writes never share `index.lock'."
  :type '(choice (const :tag "Single lispm-directory repo" directory)
                 (const :tag "Independent per-scope repos" partitioned))
  :group 'lispm)

(defcustom lispm-git-debounce 0.2
  "Seconds to batch explicit memory, task, or consolidation Git commits."
  :type 'number
  :group 'lispm)

(defcustom lispm-git-turn-delay 1.5
  "Seconds to coalesce low-priority turn-end archival snapshots."
  :type 'number
  :group 'lispm)

(defvar lispm-git--queue nil "Changes not yet committed: `(AUTHOR MESSAGE . PATHS)'.")
(defvar lispm-git--timer nil "Timer of the next commit.")
(defvar lispm-git--deadline nil "Epoch timestamp when `lispm-git--timer' is scheduled to fire.")
(defvar lispm-git--process nil "The git process running, if any.")
(defvar lispm-git--retries 0 "Consecutive `.git/index.lock' retry count for default repo.")
(defvar lispm-git--partitions (make-hash-table :test #'equal)
  "Partition repo root -> state plist (:queue :timer :deadline :process :retries).")

(defun lispm-git--default-repo ()
  "Return the canonical default archival repository root."
  (file-name-as-directory (expand-file-name lispm-directory)))

(defun lispm-git--repo-root (agent paths)
  "Select the Git repository root for AGENT and modified PATHS."
  (let ((base (lispm-git--default-repo)))
    (if (not (eq lispm-git-scope 'partitioned))
        base
      (or (when-let* ((first (car paths))
                      (canon (expand-file-name first))
                      ((file-in-directory-p canon base))
                      (rel (file-relative-name canon base)))
            (cond
             ((string-match "\\`\\(\\(?:projects\\|roles\\|characters\\)/[^/]+/\\)" rel)
              (expand-file-name (match-string 1 rel) base))
             ((string-prefix-p "system/" rel)
              (expand-file-name "system/" base))))
          (when (lispm-agent-p agent)
            (file-name-as-directory (expand-file-name (lispm-home-directory agent))))
          base))))

(defun lispm-git--repo-slot (repo)
  "Return mutable state plist for REPO."
  (if (equal repo (lispm-git--default-repo))
      nil
    (or (gethash repo lispm-git--partitions)
        (let ((slot (list :queue nil :timer nil :deadline nil :process nil :retries 0)))
          (puthash repo slot lispm-git--partitions)
          slot))))

(defun lispm-git--slot-get (repo key)
  "Get KEY (`:queue', `:timer', `:deadline', `:process', or `:retries') for REPO."
  (if (equal repo (lispm-git--default-repo))
      (pcase key
        (:queue lispm-git--queue)
        (:timer lispm-git--timer)
        (:deadline lispm-git--deadline)
        (:process lispm-git--process)
        (:retries lispm-git--retries))
    (plist-get (gethash repo lispm-git--partitions) key)))

(defun lispm-git--slot-set (repo key value)
  "Set KEY to VALUE on REPO's state."
  (if (equal repo (lispm-git--default-repo))
      (pcase key
        (:queue (setq lispm-git--queue value))
        (:timer (setq lispm-git--timer value))
        (:deadline (setq lispm-git--deadline value))
        (:process (setq lispm-git--process value))
        (:retries (setq lispm-git--retries value)))
    (let ((slot (lispm-git--repo-slot repo)))
      (puthash repo (plist-put slot key value) lispm-git--partitions)))
  value)

(defun lispm-git--schedule (repo delay &optional urgent)
  "Schedule an async flush for REPO after DELAY seconds.
When URGENT is non-nil and an existing timer fires later, replace it."
  (let* ((existing (lispm-git--slot-get repo :timer))
         (existing-dl (lispm-git--slot-get repo :deadline))
         (wait (max 0.01 (or delay lispm-git-debounce)))
         (target (+ (float-time) wait)))
    (when (and existing urgent existing-dl (< target existing-dl))
      (cancel-timer existing)
      (lispm-git--slot-set repo :timer nil)
      (lispm-git--slot-set repo :deadline nil)
      (setq existing nil))
    (unless existing
      (lispm-git--slot-set repo :deadline target)
      (lispm-git--slot-set
       repo :timer (run-at-time wait nil #'lispm-git--flush repo)))))

(defun lispm-git-p ()
  "Non-nil if changes to `lispm-directory' are committed to git.
See `lispm-git'."
  (and lispm-git (executable-find "git") t))

(defun lispm-git-settled-p ()
  "Non-nil when every queued Git archival commit has finished."
  (and (null lispm-git--queue)
       (null lispm-git--timer)
       (not (process-live-p lispm-git--process))
       (catch 'busy
         (maphash (lambda (_repo slot)
                    (when (or (plist-get slot :queue)
                              (plist-get slot :timer)
                              (process-live-p (plist-get slot :process)))
                      (throw 'busy nil)))
                  lispm-git--partitions)
         t)))

(defun lispm-git-commit (agent message &optional paths delay)
  "Record a change by AGENT, described by MESSAGE, for the next commit.
Optional PATHS lists files modified by this change; when every entry in a
batch specifies paths inside the repository root, only those paths are staged.
Optional DELAY overrides the batching delay (default `lispm-git-debounce')."
  (when (lispm-git-p)
    (let* ((repo (lispm-git--repo-root agent paths))
           (author (format "%s" (if (lispm-agent-p agent) (lispm-agent-name agent) agent)))
           (entry (cons author (cons message paths))))
      (lispm-git--slot-set repo :queue
                           (append (lispm-git--slot-get repo :queue) (list entry)))
      (lispm-git--schedule repo (or delay lispm-git-debounce) (null delay)))))

(defun lispm-git--pathspecs (repo entries)
  "Return relative pathspecs inside REPO when every entry lists paths."
  (let (all-paths
        (complete t))
    (dolist (e entries)
      (let ((ps (cddr e)))
        (if (null ps)
            (setq complete nil)
          (dolist (p ps)
            (let ((canon (expand-file-name p)))
              (if (file-in-directory-p canon repo)
                  (push (file-relative-name canon repo) all-paths)
                (setq complete nil)))))))
    (when complete
      (delete-dups (nreverse all-paths)))))

(defun lispm-git--flush (&optional repo)
  "Commit queued changes for REPO (default `lispm-directory') asynchronously."
  (let ((repo (file-name-as-directory (expand-file-name (or repo (lispm-git--default-repo))))))
    (lispm-git--slot-set repo :timer nil)
    (lispm-git--slot-set repo :deadline nil)
    (let ((proc (lispm-git--slot-get repo :process))
          (queue (lispm-git--slot-get repo :queue)))
      (cond
       ((process-live-p proc)
        (lispm-git--schedule repo lispm-git-debounce))
       (queue
        (let* ((entries queue)
               (authors (delete-dups (mapcar #'car entries)))
               (author (if (cdr authors) "lispm" (car authors)))
               (message (if (cdr entries)
                            (concat (format "%s: %d changes\n\n" author (length entries))
                                    (mapconcat #'cadr entries "\n"))
                          (cadar entries)))
               (pathspecs (lispm-git--pathspecs repo entries))
               (add-cmd (if pathspecs
                            (concat "git add -A -- "
                                    (mapconcat #'shell-quote-argument pathspecs " "))
                          "git add -A"))
               (default-directory repo))
          (lispm-git--slot-set repo :queue nil)
          (make-directory repo t)
          (lispm-git--slot-set
           repo :process
           (let ((process-environment (lispm-git-environment))
                 (pname (if (equal repo (lispm-git--default-repo))
                            "lispm-git"
                          (format "lispm-git:%s" (file-name-nondirectory
                                                  (directory-file-name repo))))))
             (make-process
              :name pname :buffer (get-buffer-create " *lispm-git*") :noquery t
              :command
              (list "sh" "-c"
                    (concat "{ [ -d .git ] || git init -q; } && "
                            "for p in '*.images/' 'journal.git/' 'inbox/' 'inbox.org' 'inbox.org.migrated' 'sandbox/' '.rag-index.eld'; do"
                            " grep -qxF \"$p\" .git/info/exclude 2>/dev/null"
                            " || { mkdir -p .git/info && echo \"$p\" >> .git/info/exclude; }; done && "
                            "if [ -e .git/index.lock ]; then exit 75; fi && "
                            "{ " add-cmd " && "
                            "{ git diff --cached --quiet || git"
                            " -c user.name=" (shell-quote-argument author)
                            " -c user.email=" (shell-quote-argument (concat author "@lispm.invalid"))
                            " commit -q -m " (shell-quote-argument message) "; }; } "
                            "|| { rc=$?; if [ -e .git/index.lock ]; then exit 75; fi; exit $rc; }"))
              :sentinel
              (lambda (p _event)
                (unless (process-live-p p)
                  (when (eq (lispm-git--slot-get repo :process) p)
                    (lispm-git--slot-set repo :process nil))
                  (let ((status (process-exit-status p))
                        (retries (or (lispm-git--slot-get repo :retries) 0)))
                    (if (and (= status 75) (< retries 5))
                        (progn
                          (lispm-git--slot-set
                           repo :queue (append entries (lispm-git--slot-get repo :queue)))
                          (lispm-git--slot-set repo :retries (1+ retries))
                          (lispm-git--schedule
                           repo (* lispm-git-debounce (expt 2 retries))))
                      (lispm-git--slot-set repo :retries 0)
                      (when (lispm-git--slot-get repo :queue)
                        (lispm-git--schedule repo lispm-git-debounce)))))))))))))))

(defun lispm-git--on-kill-emacs ()
  "Cancel pending archival timers cleanly without blocking Emacs shutdown."
  (when lispm-git--timer
    (cancel-timer lispm-git--timer)
    (setq lispm-git--timer nil
          lispm-git--deadline nil))
  (maphash (lambda (repo slot)
             (when-let* ((timer (plist-get slot :timer)))
               (cancel-timer timer)
               (setq slot (plist-put slot :timer nil))
               (puthash repo (plist-put slot :deadline nil)
                        lispm-git--partitions)))
           lispm-git--partitions))

(add-hook 'kill-emacs-hook #'lispm-git--on-kill-emacs)

(defun lispm-git--commit-turn (agent _reply)
  "Schedule low-priority archival commit for AGENT's finished turn."
  (let* ((buf (lispm-agent-buffer agent))
         (file (and (buffer-live-p buf) (buffer-file-name buf))))
    (lispm-git-commit agent
                      (format "%s: turn %d" (lispm-agent-name agent)
                              (lispm-agent-turn agent))
                      (and file (list file))
                      lispm-git-turn-delay)))

(add-hook 'lispm-turn-end-functions #'lispm-git--commit-turn)

;;;; System prompt

(defun lispm-memory--time (stamp)
  "The org timestamp STAMP as a float time; 0 if it is nil or unreadable."
  (or (and stamp (ignore-errors (float-time (org-time-string-to-time stamp)))) 0))

(defun lispm-memory--last-used (entry)
  "When ENTRY was last used: recalled, updated or saved, as a float time."
  (max (lispm-memory--time (plist-get entry :recalled))
       (lispm-memory--time (plist-get entry :updated))
       (lispm-memory--time (plist-get entry :created))))

(defun lispm-memory--listing (memories)
  "Lines listing the titles of MEMORIES, lists of entries, for a system prompt.
Past `lispm-memory-max-entries' in all, those used least lately are
not listed, and a line after each memory says how many of its own
there are: none is left out unseen."
  (let* ((limit lispm-memory-max-entries)
         (all (apply #'append memories))
         (kept (if (and limit (> (length all) limit))
                   (seq-take (sort (copy-sequence all)
                                   (lambda (a b) (> (lispm-memory--last-used a)
                                                    (lispm-memory--last-used b))))
                             limit)
                 all)))
    (mapconcat
     (lambda (entries)
       (let* ((shown (seq-filter (lambda (e) (memq e kept)) entries))
              (hidden (- (length entries) (length shown))))
         (concat (mapconcat (lambda (e) (concat "- " (lispm-memory--label e))) shown "\n")
                 (cond ((zerop hidden) "")
                       (shown (format "\n- (%d more, used less lately, not listed here: `recall` finds them)"
                                      hidden))
                       (t (format "- (%d, used less lately, not listed here: `recall` finds them)"
                                  hidden))))))
     memories "\n")))

(defun lispm-memory--prompt (agent)
  "System prompt section listing AGENT's memories and its open tasks."
  (let ((memories (delq nil (mapcar (lambda (scope) (lispm-memory--scope-entries agent scope))
                                    (lispm-memory-scopes agent))))
        (tasks (lispm-tasks-list agent)))
    (string-join
     (delq nil
           (list (when memories
                   (concat "# Your long-term memory\n"
                           "Titles of what was saved in earlier sessions; `recall` reads them.\n"
                           (lispm-memory--listing memories)))
                 (unless (equal tasks "No tasks.")
                   (concat (if (lispm-agent-system agent)
                               "# Open tasks\n"
                             "# Open tasks of this project\n")
                           tasks))))
     "\n\n")))

(add-hook 'lispm-system-prompt-functions #'lispm-memory--prompt)


(provide 'lispm-memory-store)

;;; lispm-memory-store.el ends here
