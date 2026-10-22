;;; lispm-memory.el --- Agent memory and tasks: org files under git  -*- lexical-binding: t; -*-

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

;; Memory has two levels, like the project and user instructions of
;; other agent tools, both org files under `lispm-directory':
;;
;;   projects/<project>/memory.org  what the agents of a project learn
;;                                  about it, shared by all of them;
;;   roles/<role>/memory.org        what agents of a role learn that holds
;;                                  everywhere, such as the user's tastes.
;;
;; An agent of the whole system, such as the agent of a system-wide
;; prompt, works in no project: its own memory is the system's,
;; system/memory.org, in place of a project's.
;;
;; One heading per fact, with tags and CREATED/UPDATED properties, and
;; RECALLED/RECALLS: when `recall' last returned it, and how often.  A
;; memory past its budget (`lispm-memory-max-entries',
;; `lispm-memory-max-tokens') consolidates itself at idle: a model
;; merges entries that say the same, folds old ones seldom recalled
;; into summary entries, and drops stale ones, each change a commit
;; saying what went where and why.  Each project, and the system, also
;; has a task list, tasks.org (TODO NEXT BLOCKED | DONE CANCELLED),
;; whose entries have short IDs.
;;
;; All are ordinary org files: the user can read and edit them, put the
;; task lists in `org-agenda-files' to see the agents' work in their own
;; agenda, or hand agents a task by adding a TODO.  The whole
;; directory is a git repository; every memory change and every
;; finished turn is a commit authored by the agent, so `git log' is
;; the audit trail of what each agent learned and did, and a bad
;; memory is one `git revert' away.

;;; Code:

(require 'org)
(require 'seq)
(require 'lispm-core)
(require 'lispm-api)
(require 'lispm-tools)

(declare-function lispm--reply-text "lispm-runtime" (content))

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
was merged, folded or dropped, and why: `git revert' undoes it.  A
system prompt lists this many titles of each memory at most, those
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
  "The scope of AGENT's own memory: `system' or `project'.
An agent of the whole system (see `lispm-make-agent') works in no
project; its memory is the system's."
  (if (lispm-agent-system agent) 'system 'project))

(defun lispm-memory--scope (agent scope)
  "SCOPE as AGENT has it: nil and `project' stand for its own memory."
  (if (memq scope '(nil project)) (lispm-memory-home agent) scope))

(defun lispm-memory-file (agent &optional scope role)
  "File of AGENT's memory in SCOPE.
SCOPE `project' (the default) is AGENT's own: its project's, shared by
the agents of the project, or, for an agent of the whole system, the
system's, as SCOPE `system' is; `role' is shared by the agents of its
role, or of ROLE, across projects."
  (expand-file-name "memory.org"
                    (pcase (lispm-memory--scope agent scope)
                      ('role (lispm-role-directory (or role (lispm-agent-role agent))))
                      ('system (lispm-system-directory))
                      (_ (lispm-project-directory (lispm-agent-root agent))))))

(defun lispm-tasks-file (agent)
  "File of the tasks of AGENT's project, or of the system's for a system agent."
  (expand-file-name "tasks.org" (lispm-home-directory agent)))

(defun lispm-memory--visit (file header &optional to-change)
  "Buffer visiting FILE, created with HEADER if new.
The user may change the file too, by hand or with git, so a buffer out
of date is brought up to date first (see `lispm-tools--sync').  One
with unsaved changes while the file changed on disk too is read as it
is, being what the user sees, but cannot be changed: with TO-CHANGE,
that is an error."
  (let ((buffer (lispm-tools--visit file (not to-change))))
    (with-current-buffer buffer
      ;; Not over a file changed on disk: saving would ask.
      (when (and (zerop (buffer-size)) (eq (lispm-tools--state buffer) 'current))
        (insert header)
        (org-mode)
        (let ((inhibit-message t)) (save-buffer))))
    buffer))

(defun lispm-memory--buffer (agent &optional scope role to-change)
  "Buffer of AGENT's memory in SCOPE, or of ROLE's (see `lispm-memory-file').
TO-CHANGE is as for `lispm-memory--visit'."
  (lispm-memory--visit (lispm-memory-file agent scope role)
                       (pcase (lispm-memory--scope agent scope)
                         ('role (format "#+title: Memory of the %s role\n"
                                        (or role (lispm-agent-role agent))))
                         ('system "#+title: Memory of the system\n")
                         (_ (format "#+title: Memory of %s\n"
                                    (abbreviate-file-name (lispm-agent-root agent)))))
                       to-change))

(defun lispm-memory--scope-entries (agent scope &optional role)
  "Entries of AGENT's memory in SCOPE (or ROLE's), without creating its file.
Each has its :scope (`project', `role' or `system') and its :file too."
  (let ((file (lispm-memory-file agent scope role))
        (scope (lispm-memory--scope agent scope)))
    (and (file-exists-p file)
         (mapcar (lambda (e) (plist-put (plist-put e :scope scope) :file file))
                 (lispm-memory--entries (lispm-memory--buffer agent scope role))))))

(defun lispm-tasks--buffer (agent &optional to-change)
  "Buffer of the tasks of AGENT's project, or of the system's.
TO-CHANGE is as for `lispm-memory--visit'."
  (lispm-memory--visit (lispm-tasks-file agent)
                       (format "#+title: Tasks of %s\n#+todo: TODO NEXT BLOCKED | DONE CANCELLED\n"
                               (if (lispm-agent-system agent)
                                   "the system"
                                 (abbreviate-file-name (lispm-agent-root agent))))
                       to-change))

(defun lispm-memory--entries (buffer)
  "Top-level entries of the org BUFFER, as plists.
Keys: :title :tags :todo :id :body :beg :end, and the properties
:created :updated :recalled (timestamps, or nil) and :recalls (a
number)."
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
                           :beg beg :end end))))))))

(defun lispm-memory--save (buffer)
  "Save BUFFER quietly."
  (with-current-buffer buffer
    (let ((inhibit-message t)) (save-buffer))))

(defun lispm-memory--tag (tag)
  "TAG made valid for org."
  (replace-regexp-in-string "[^[:alnum:]_@#%]" "_" (format "%s" tag)))

(defun lispm-memory--stamp (&optional time)
  "An inactive org timestamp for TIME, or for now."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun lispm-memory--entry-text (title tags properties content)
  "An entry of a memory file: TITLE with TAGS, PROPERTIES and CONTENT.
PROPERTIES is an alist (NAME . VALUE); those whose VALUE is nil are
left out."
  (concat "* " (lispm--oneline title 120)
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
  (let* ((scope (lispm-memory--scope agent scope))
         (buffer (lispm-memory--buffer agent scope nil t))
         (title (lispm--oneline title 120))
         (existing (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                             (lispm-memory--entries buffer)))
         ;; The past of the entry replaced: when it was saved, recalled, merged.
         (past (and existing
                    (with-current-buffer buffer
                      (mapcar (lambda (name) (cons name (org-entry-get (plist-get existing :beg) name)))
                              '("CREATED" "RECALLED" "RECALLS" "MERGED"))))))
    (with-current-buffer buffer
      (save-excursion
        (if existing
            (progn (delete-region (plist-get existing :beg) (plist-get existing :end))
                   (goto-char (plist-get existing :beg)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))
        (insert (lispm-memory--entry-text
                 title tags
                 `(("CREATED" . ,(or (cdr (assoc "CREATED" past)) (lispm-memory--stamp)))
                   ("UPDATED" . ,(and existing (lispm-memory--stamp)))
                   ,@(assoc-delete-all "CREATED" past))
                 content))))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: %s \"%s\"" (lispm-agent-name agent)
                                    (if existing "update memory" "remember") title))
    (format "%s %s memory \"%s\"." (if existing "Updated" "Saved") scope title)))

(defun lispm-memory--label (entry)
  "Title of a memory ENTRY, with its tags and, for role memory, its scope."
  (concat (plist-get entry :title)
          (if (plist-get entry :tags)
              (format " [%s]" (string-join (plist-get entry :tags) ", "))
            "")
          (if (eq (plist-get entry :scope) 'role) " (role memory)" "")))

(defun lispm-memory--format (entry)
  "ENTRY of a memory file as text for the model."
  (concat "## " (lispm-memory--label entry) "\n" (plist-get entry :body)))

(defun lispm-memory--note-recalls (entries)
  "Record that ENTRIES were recalled now, in their RECALLED and RECALLS.
That is what consolidation goes by (see `lispm-memory-max-entries'),
and not a change of what is known: it is committed with the turn.  A
memory file whose buffer has changes of the user's is left as it is."
  (pcase-dolist (`(,file . ,recalled) (seq-group-by (lambda (e) (plist-get e :file)) entries))
    (when-let* ((buffer (and file (find-buffer-visiting file)))
                ((not (buffer-modified-p buffer)))
                ((eq (lispm-tools--state buffer) 'current)))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            ;; The latest first: a property added moves what follows.
            (dolist (e (sort (copy-sequence recalled)
                             (lambda (a b) (> (plist-get a :beg) (plist-get b :beg)))))
              (goto-char (plist-get e :beg))
              (org-entry-put nil "RECALLED" (lispm-memory--stamp))
              (org-entry-put nil "RECALLS" (number-to-string (1+ (plist-get e :recalls))))))))
      (lispm-memory--save buffer))))

(defun lispm-memory-recall (agent query &optional role)
  "Entries of AGENT's memory matching every word of QUERY.
AGENT's own memory comes first, then that of its role, or of ROLE.
The entries returned are noted as recalled."
  (let* ((entries (append (lispm-memory--scope-entries agent 'project)
                          (lispm-memory--scope-entries
                           agent 'role (and role (lispm-role-name (lispm-get-role role))))))
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
  "Delete the entry TITLE from AGENT's own memory, or else its role memory."
  (let* ((entry (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                          (append (lispm-memory--scope-entries agent 'project)
                                  (lispm-memory--scope-entries agent 'role))))
         (buffer (and entry (lispm-memory--buffer agent (plist-get entry :scope) nil t))))
    (unless entry (error "No memory titled \"%s\"" title))
    (with-current-buffer buffer
      (delete-region (plist-get entry :beg) (plist-get entry :end)))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: forget \"%s\"" (lispm-agent-name agent) title))
    (format "Forgot \"%s\" (it remains in git history)." title)))

;;;; Tasks

(defconst lispm-tasks--states '("TODO" "NEXT" "BLOCKED" "DONE" "CANCELLED"))

(defun lispm-tasks--find (agent id)
  "The task of AGENT with ID, as an entry plist."
  (or (seq-find (lambda (e) (equal (plist-get e :id) id))
                (lispm-memory--entries (lispm-tasks--buffer agent)))
      (error "No task with ID %s" id)))

(defun lispm-tasks-add (agent title &optional notes)
  "Add a TODO titled TITLE (with NOTES) to AGENT's tasks; return its ID."
  (let* ((buffer (lispm-tasks--buffer agent t))
         (ids (mapcar (lambda (e) (plist-get e :id)) (lispm-memory--entries buffer)))
         (id (cl-loop for id = (format "T%04x" (random #x10000))
                      unless (member id ids) return id)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* TODO " (lispm--oneline title 120) "\n"
                ":PROPERTIES:\n:ID:       " id "\n"
                ":CREATED:  " (lispm-memory--stamp) "\n:END:\n"
                (if (and notes (not (string-blank-p notes)))
                    (concat (org-escape-code-in-string (string-trim notes)) "\n")
                  ""))))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: add task %s" (lispm-agent-name agent) id))
    id))

(defun lispm-tasks-update (agent id &optional state note)
  "Set task ID of AGENT to STATE and/or append NOTE."
  (when (and state (not (member state lispm-tasks--states)))
    (error "State must be one of %s" (string-join lispm-tasks--states ", ")))
  (let* ((buffer (lispm-tasks--buffer agent t))
         (task (lispm-tasks--find agent id)))
    (with-current-buffer buffer
      (save-excursion
        (when note
          (goto-char (plist-get task :end))
          (unless (bolp) (insert "\n"))
          (insert "- " (lispm-memory--stamp) " " (lispm--oneline note 500) "\n"))
        (when state
          (goto-char (plist-get task :beg))
          (let ((case-fold-search nil))
            (if (looking-at "\\* \\(?:\\(TODO\\|NEXT\\|BLOCKED\\|DONE\\|CANCELLED\\) \\)?")
                (replace-match (concat "* " state " ") t t)
              (error "Malformed task heading"))))))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: task %s%s" (lispm-agent-name agent) id
                                    (if state (concat " -> " state) " note")))
    (format "Task %s updated." id)))

(defun lispm-tasks-list (agent &optional include-done)
  "AGENT's tasks as text; closed ones only if INCLUDE-DONE."
  (let ((tasks (seq-filter (lambda (e)
                             (or include-done
                                 (not (member (plist-get e :todo) '("DONE" "CANCELLED")))))
                           (and (file-exists-p (lispm-tasks-file agent))
                                (lispm-memory--entries (lispm-tasks--buffer agent))))))
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

(defvar lispm-git--queue nil "Changes not yet committed: (AUTHOR . MESSAGE).")
(defvar lispm-git--timer nil "Timer of the next commit.")
(defvar lispm-git--process nil "The git process running, if any.")

(defun lispm-git-commit (agent message)
  "Record a change by AGENT, described by MESSAGE, for the next commit.
Changes made within a fraction of a second, such as several tool
calls of one response, become one commit listing each of them."
  (when (and lispm-git (executable-find "git"))
    (setq lispm-git--queue
          (append lispm-git--queue
                  (list (cons (format "%s" (if (lispm-agent-p agent) (lispm-agent-name agent) agent))
                              message))))
    (unless lispm-git--timer
      (setq lispm-git--timer (run-at-time 0.2 nil #'lispm-git--flush)))))

(defun lispm-git--flush ()
  "Commit everything under `lispm-directory' with the queued messages."
  (setq lispm-git--timer nil)
  (cond
   ((process-live-p lispm-git--process)
    (setq lispm-git--timer (run-at-time 0.2 nil #'lispm-git--flush)))
   (lispm-git--queue
    (let* ((entries lispm-git--queue)
           (authors (delete-dups (mapcar #'car entries)))
           (author (if (cdr authors) "lispm" (car authors)))
           (message (if (cdr entries)
                        (concat (format "%s: %d changes\n\n" author (length entries))
                                (mapconcat #'cdr entries "\n"))
                      (cdar entries)))
           (default-directory (file-name-as-directory (expand-file-name lispm-directory))))
      (setq lispm-git--queue nil)
      (make-directory default-directory t)
      (setq lispm-git--process
            (let ((process-environment (lispm--git-environment)))
              (make-process
               :name "lispm-git" :buffer (get-buffer-create " *lispm-git*") :noquery t
               :command
               (list "sh" "-c"
                     (concat "{ [ -d .git ] || git init -q; } && "
                             ;; Images of sessions stay out of the history, and
                             ;; so do the state of inboxes, the snapshots of
                             ;; projects, a repository of their own, and the
                             ;; sandbox's srt and settings.
                             "for p in '*.images/' 'journal.git/' 'inbox.org' 'sandbox/'; do"
                             " grep -qxF \"$p\" .git/info/exclude 2>/dev/null"
                             " || { mkdir -p .git/info && echo \"$p\" >> .git/info/exclude; }; done && "
                             "git add -A && "
                             "{ git diff --cached --quiet || git"
                             " -c user.name=" (shell-quote-argument author)
                             " -c user.email=" (shell-quote-argument (concat author "@lispm.invalid"))
                             " commit -q -m " (shell-quote-argument message) "; }")))))))))

(defun lispm-git--commit-turn (agent _reply)
  "Commit AGENT's session after a turn."
  (lispm-git-commit agent (format "%s: turn %d" (lispm-agent-name agent)
                                  (lispm-agent-turn agent))))

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

(defun lispm-memory--listing (entries)
  "Lines listing the titles of ENTRIES, one memory's, for a system prompt.
Past `lispm-memory-max-entries', those used least lately are not
listed, and a line says how many there are: none is left out unseen."
  (let* ((limit lispm-memory-max-entries)
         (shown (if (and limit (> (length entries) limit))
                    (let ((kept (seq-take (sort (copy-sequence entries)
                                                (lambda (a b) (> (lispm-memory--last-used a)
                                                                 (lispm-memory--last-used b))))
                                          limit)))
                      (seq-filter (lambda (e) (memq e kept)) entries))
                  entries))
         (hidden (- (length entries) (length shown))))
    (concat (mapconcat (lambda (e) (concat "- " (lispm-memory--label e))) shown "\n")
            (if (> hidden 0)
                (format "\n- (%d more, used less lately, not listed here: `recall` finds them)"
                        hidden)
              ""))))

(defun lispm-memory--prompt (agent)
  "System prompt section listing AGENT's memories and its open tasks."
  (let ((listings (delq nil (mapcar (lambda (scope)
                                      (when-let* ((entries (lispm-memory--scope-entries agent scope)))
                                        (lispm-memory--listing entries)))
                                    '(project role))))
        (tasks (lispm-tasks-list agent)))
    (string-join
     (delq nil
           (list (when listings
                   (concat "# Your long-term memory\n"
                           "Titles of what was saved in earlier sessions; `recall` reads them.\n"
                           (string-join listings "\n")))
                 (unless (equal tasks "No tasks.")
                   (concat (if (lispm-agent-system agent)
                               "# Open tasks\n"
                             "# Open tasks of this project\n")
                           tasks))))
     "\n\n")))

(add-hook 'lispm-system-prompt-functions #'lispm-memory--prompt)

;;;; Consolidation

(defconst lispm-memory--consolidate-prompt
  "You keep the long-term memory of an AI agent within its budget. The memory is a list of \
entries, each a durable fact, preference or lesson, numbered, with its title, tags, when it \
was saved and last used, and how often it was recalled. Make it fit in %s by:
- merging entries that say the same thing, or overlap, into one;
- folding old entries that are seldom recalled into fewer summary entries, one per topic, \
keeping every specific that may still matter: names, paths, values, commands;
- dropping entries that are stale: contradicted by a newer entry, or about something finished \
or gone.
Keep entries recalled often or lately as they are. Leave every entry you keep unchanged out \
of your plan. Reply with JSON only, in this form:
{\"replace\": [{\"ids\": [numbers of the entries it replaces], \"title\": \"...\", \
\"tags\": [\"...\"], \"content\": \"...\"}],
 \"drop\": [{\"id\": number, \"reason\": \"why it is stale\"}]}"
  "System prompt of the request consolidating a memory; %s is its budget.")

(defvar lispm-memory--consolidating (make-hash-table :test #'equal)
  "Memory files being consolidated now.")

(defvar lispm-memory--tried (make-hash-table :test #'equal)
  "Memory files consolidation was tried on, to the hash of their text then.
A memory is not tried again before it changes.")

(defun lispm-memory--text (buffer)
  "The whole text of BUFFER, without properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lispm-memory--size (text)
  "Entries and tokens of a memory file holding TEXT, as (ENTRIES . TOKENS)."
  (cons (cl-loop with start = 0
                 while (string-match "^\\* " text start)
                 count t do (setq start (match-end 0)))
        (/ (string-bytes text) 3)))

(defun lispm-memory--over-budget-p (text)
  "Non-nil if a memory file holding TEXT is past its budget.
See `lispm-memory-max-entries' and `lispm-memory-max-tokens'."
  (pcase-let ((`(,entries . ,tokens) (lispm-memory--size text)))
    (or (and lispm-memory-max-entries (> entries lispm-memory-max-entries))
        (and lispm-memory-max-tokens (> tokens lispm-memory-max-tokens)))))

(defun lispm-memory--scope-name (agent scope)
  "SCOPE of AGENT's memory, named for a commit message."
  (pcase (lispm-memory--scope agent scope)
    ('role (format "role memory (%s)" (lispm-agent-role agent)))
    (scope (format "%s memory" scope))))

(defun lispm-memory--maintain (agent _reply)
  "Once AGENT's turn has ended, consolidate its memories past their budget at idle."
  (when (lispm-backend-name agent)
    (dolist (scope '(project role))
      (let ((file (lispm-memory-file agent scope)))
        (when (and (file-exists-p file)
                   (not (gethash file lispm-memory--consolidating))
                   (let ((text (lispm--file-text file)))
                     (and (lispm-memory--over-budget-p text)
                          (not (equal (gethash file lispm-memory--tried)
                                      (secure-hash 'sha1 text))))))
          (lispm-when-idle (cons 'consolidate file) #'lispm-memory-consolidate agent scope))))))

(add-hook 'lispm-turn-end-functions #'lispm-memory--maintain)

(defun lispm-memory--budget ()
  "Three quarters of a memory's budget, in words for the model."
  (string-join
   (delq nil (list (and lispm-memory-max-entries
                        (format "%d entries" (/ (* 3 lispm-memory-max-entries) 4)))
                   (and lispm-memory-max-tokens
                        (format "%d tokens (a token is about four characters of English)"
                                (/ (* 3 lispm-memory-max-tokens) 4)))))
   " and "))

(defun lispm-memory--day (stamp)
  "The date of the org timestamp STAMP, or \"unknown\"."
  (if (and stamp (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" stamp))
      (match-string 0 stamp)
    "unknown"))

(defun lispm-memory--describe (entries)
  "ENTRIES of a memory, numbered from 1, as text for the model."
  (cl-loop for e in entries
           for id from 1
           concat (format "[%d] %s%s\nsaved %s, last used %s, recalled %d time%s\n%s\n\n"
                          id (plist-get e :title)
                          (if (plist-get e :tags)
                              (format " [tags: %s]" (string-join (plist-get e :tags) ", "))
                            "")
                          (lispm-memory--day (plist-get e :created))
                          (lispm-memory--day (or (plist-get e :recalled) (plist-get e :updated)
                                                 (plist-get e :created)))
                          (plist-get e :recalls) (if (= (plist-get e :recalls) 1) "" "s")
                          (plist-get e :body))))

(defun lispm-memory-consolidate (agent &optional scope)
  "Bring AGENT's memory in SCOPE back within its budget, as a model plans.
SCOPE is as for `lispm-memory-file'.  The plan comes back later; it
is carried out, as one commit, only if the memory did not change
meanwhile.  See `lispm-memory-max-entries'."
  (let ((file (lispm-memory-file agent scope)))
    (when (and (file-exists-p file) (not (gethash file lispm-memory--consolidating)))
      (let* ((buffer (lispm-memory--buffer agent scope))
             (text (lispm-memory--text buffer))
             (hash (secure-hash 'sha1 text))
             (entries (lispm-memory--entries buffer))
             (backend (lispm-agent-backend* agent))
             (request (lispm-api-request
                       backend (or lispm-memory-model (lispm-agent-model* agent)) "low"
                       (format lispm-memory--consolidate-prompt (lispm-memory--budget)) nil
                       `(((role . "user")
                          (content . [((type . "text")
                                       (text . ,(format "Today is %s. The memory, %d entries, about %d tokens:\n\n%s"
                                                        (format-time-string "%Y-%m-%d")
                                                        (length entries)
                                                        (cdr (lispm-memory--size text))
                                                        (lispm-memory--describe entries))))])))))
             (failed (lambda (reason)
                       (remhash file lispm-memory--consolidating)
                       (message "%s" (lispm-tr "LispM: could not consolidate %s: %s"
                                               (abbreviate-file-name file) reason)))))
        (puthash file hash lispm-memory--tried)
        (puthash file t lispm-memory--consolidating)
        (condition-case err
            (funcall lispm-api-function backend (car request) (cdr request)
                     :on-done
                     (lambda (message)
                       (condition-case err
                           (progn
                             (lispm-memory--carry-out
                              agent scope hash entries
                              (lispm-memory--read-plan
                               (lispm--reply-text (append (alist-get 'content message) nil))
                               (length entries)))
                             (remhash file lispm-memory--consolidating))
                         (error (funcall failed (error-message-string err)))))
                     :on-error
                     (lambda (err) (funcall failed (plist-get err :message))))
          (error (funcall failed (error-message-string err))))))))

(defun lispm-memory--read-plan (reply count)
  "The consolidation plan in REPLY, for a memory of COUNT entries, checked.
Return (REPLACE . DROP): REPLACE a list of (IDS TITLE TAGS CONTENT),
DROP of (ID . REASON).  Every ID is an entry's number, from 1 to
COUNT, and is in one of them at most; an entry dropped has a reason.
A plan that is not so is an error, and nothing is changed."
  (let* ((start (string-search "{" reply))
         (end (cl-position ?} reply :from-end t))
         (plan (if (and start end (< start end))
                   (lispm--json-decode (substring reply start (1+ end)))
                   (error "No plan in the reply")))
         (strings (lambda (value) (and (vectorp value) (seq-filter #'stringp value))))
         (text (lambda (value) (and (stringp value) (not (string-blank-p value)) value)))
         seen replace drop)
    (cl-flet ((take (id)
                (unless (and (integerp id) (<= 1 id count))
                  (error "No entry %s" id))
                (when (memq id seen)
                  (error "Entry %d is in the plan twice" id))
                (push id seen)))
      (seq-doseq (group (if (vectorp (alist-get 'replace plan)) (alist-get 'replace plan) []))
        (let ((ids (append (alist-get 'ids group) nil))
              (title (funcall text (alist-get 'title group)))
              (content (funcall text (alist-get 'content group))))
          (unless (and ids title content)
            (error "An entry of the plan lacks ids, a title or content"))
          (mapc #'take ids)
          (push (list ids title (funcall strings (alist-get 'tags group)) content) replace)))
      (seq-doseq (dropped (if (vectorp (alist-get 'drop plan)) (alist-get 'drop plan) []))
        (let ((id (alist-get 'id dropped))
              (reason (funcall text (alist-get 'reason dropped))))
          (take id)
          (unless reason
            (error "Entry %d is dropped without a reason" id))
          (push (cons id reason) drop))))
    (cons (nreverse replace) (nreverse drop))))

(defun lispm-memory--merged (sources title tags content)
  "The entry replacing the entries SOURCES, as text: TITLE, TAGS, CONTENT.
It was saved when the first of them was, was last recalled when the
latest of them was, as often as all of them together, and names them
in its MERGED property."
  (let ((stamps (lambda (key) (sort (delq nil (mapcar (lambda (e) (plist-get e key)) sources))
                                    #'string<)))
        (recalls (apply #'+ (mapcar (lambda (e) (plist-get e :recalls)) sources))))
    (lispm-memory--entry-text
     title tags
     `(("CREATED" . ,(or (car (funcall stamps :created)) (lispm-memory--stamp)))
       ("UPDATED" . ,(lispm-memory--stamp))
       ("RECALLED" . ,(car (last (funcall stamps :recalled))))
       ("RECALLS" . ,(and (> recalls 0) (number-to-string recalls)))
       ("MERGED" . ,(mapconcat (lambda (e) (lispm--oneline (plist-get e :title) 120))
                               sources " | ")))
     content)))

(defun lispm-memory--carry-out (agent scope hash entries plan)
  "Carry out the consolidation PLAN of AGENT's memory in SCOPE.
ENTRIES are those of the memory whose text had HASH when the plan was
made; if it changed since, that is an error, and nothing is changed.
The entries a plan does not name stay as they are, in their place; an
entry replacing others takes the place of the first of them.  The
change is one commit, which names every entry merged or dropped."
  (pcase-let* ((`(,replace . ,drop) plan)
               (buffer (lispm-memory--buffer agent scope nil t))
               (entries (vconcat entries))
               (line (lambda (id) (format "\"%s\"" (plist-get (aref entries (1- id)) :title))))
               (parts nil))
    (unless (equal (secure-hash 'sha1 (lispm-memory--text buffer)) hash)
      (error "The memory changed meanwhile"))
    (when (or replace drop)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (push (buffer-substring-no-properties
                 (point-min) (if (> (length entries) 0) (plist-get (aref entries 0) :beg) (point-max)))
                parts)
          (cl-loop for e across entries
                   for id from 1
                   for group = (seq-find (lambda (g) (memq id (car g))) replace)
                   do (cond ((assq id drop))
                            ((and group (= id (apply #'min (car group))))
                             (push (pcase-let ((`(,ids ,title ,tags ,content) group))
                                     (lispm-memory--merged
                                      (mapcar (lambda (i) (aref entries (1- i))) (sort (copy-sequence ids) #'<))
                                      title tags content))
                                   parts))
                            (group)
                            (t (push (let ((text (buffer-substring-no-properties
                                                  (plist-get e :beg) (plist-get e :end))))
                                       (if (string-suffix-p "\n" text) text (concat text "\n")))
                                     parts))))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (apply #'concat (nreverse parts))))))
      (lispm-memory--save buffer)
      (puthash (buffer-file-name buffer) (secure-hash 'sha1 (lispm-memory--text buffer))
               lispm-memory--tried)
      (lispm-git-commit
       agent
       (concat (format "%s: consolidate %s, %d entries to %d\n\n"
                       (lispm-agent-name agent) (lispm-memory--scope-name agent scope)
                       (length entries) (car (lispm-memory--size (lispm-memory--text buffer))))
               (mapconcat (pcase-lambda (`(,ids ,title . ,_))
                            (format "merge %s into \"%s\"" (mapconcat line ids ", ") title))
                          replace "\n")
               (if (and replace drop) "\n" "")
               (mapconcat (pcase-lambda (`(,id . ,reason))
                            (format "drop %s: %s" (funcall line id) (lispm--oneline reason 200)))
                          drop "\n"))))))

;;;; Tools

(lispm-deftool remember
    ((title string "Short, specific title, one line.")
     (content string "The fact, preference or lesson, self-contained.")
     (tags array "Optional tags, e.g. [\"build\", \"preference\"]."
           :optional t :items ((type . "string")))
     (scope string "project (default): about where you work, shared with every agent working there: this project, or, for an agent of the whole system, the system. role: true everywhere, e.g. the user's general preferences; shared with agents of your role in all projects."
            :optional t :enum ("project" "role")))
  "Save something to long-term memory, org files that persist across
sessions (versioned in git).  Use it for durable knowledge: project
conventions, the user's preferences, lessons learned.  Saving under an
existing title replaces that entry.  Do not store transient state of
the current task."
  :permission memory
  (lispm-memory-remember lispm-current-agent title content (append tags nil)
                         (if (equal scope "role") 'role 'project)))

(lispm-deftool recall
    ((query string "Words to look for; entries containing all of them are returned. An empty string lists all titles.")
     (role string "Also read the role memory of another role instead of yours." :optional t))
  "Search long-term memory: this project's (or the system's), then your role's."
  :permission memory
  (lispm-memory-recall lispm-current-agent query role))

(lispm-deftool forget
    ((title string "Exact title of the memory to delete."))
  "Delete an entry from long-term memory, e.g. when it is wrong or obsolete."
  :permission memory
  :deferred t
  (lispm-memory-forget lispm-current-agent title))

(lispm-deftool task_add
    ((title string "What needs doing, one line.")
     (notes string "Details, context, acceptance criteria." :optional t))
  "Add a task to this project's task list, which persists across sessions,
is shared by the project's agents and shows in the user's org agenda.  Use it for work that will not be
finished in this turn.  Returns the task ID."
  :permission memory
  (format "Added task %s." (lispm-tasks-add lispm-current-agent title notes)))

(lispm-deftool task_update
    ((id string "Task ID, e.g. T3f2a.")
     (state string "New state." :optional t
            :enum ("TODO" "NEXT" "BLOCKED" "DONE" "CANCELLED"))
     (note string "A progress note to append." :optional t))
  "Change a task's state and/or append a progress note."
  :permission memory
  (lispm-tasks-update lispm-current-agent id state note))

(lispm-deftool task_list
    ((include-done boolean "Also list DONE and CANCELLED tasks." :optional t))
  "List your tasks with their IDs and states."
  :permission memory
  (lispm-tasks-list lispm-current-agent include-done))

(provide 'lispm-memory)
;;; lispm-memory.el ends here
