;;; lispm-memory.el --- Agent memory and tasks: org files under git  -*- lexical-binding: t; -*-

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

;; Each agent keeps two org files in its directory under
;; `lispm-directory':
;;
;;   memory.org  durable knowledge, one heading per fact, with tags and
;;               CREATED/UPDATED properties;
;;   tasks.org   a TODO list (TODO NEXT BLOCKED | DONE CANCELLED) whose
;;               entries have short IDs the agent refers to.
;;
;; Both are ordinary org files: the user can read and edit them, put
;; tasks.org in `org-agenda-files' to see an agent's work in their own
;; agenda, or hand an agent a task by adding a TODO.  The whole
;; directory is a git repository; every memory change and every
;; finished turn is a commit authored by the agent, so `git log' is
;; the audit trail of what each agent learned and did, and a bad
;; memory is one `git revert' away.

;;; Code:

(require 'org)
(require 'seq)
(require 'lispm-core)
(require 'lispm-tools)

(defcustom lispm-git t
  "Whether to commit memory, tasks and finished turns to git.
The repository is `lispm-directory' itself."
  :type 'boolean
  :group 'lispm)

(defun lispm-memory-file (agent)
  "AGENT's memory file."
  (expand-file-name "memory.org" (lispm-agent-directory agent)))

(defun lispm-tasks-file (agent)
  "AGENT's task file."
  (expand-file-name "tasks.org" (lispm-agent-directory agent)))

(defun lispm-memory--visit (file header)
  "Buffer visiting FILE, created with HEADER if new."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (when (zerop (buffer-size))
        (insert header)
        (org-mode)
        (let ((inhibit-message t)) (save-buffer))))
    buffer))

(defun lispm-memory--buffer (agent)
  "Buffer of AGENT's memory."
  (lispm-memory--visit (lispm-memory-file agent)
                       (format "#+title: Memory of %s\n" (lispm-agent-name agent))))

(defun lispm-tasks--buffer (agent)
  "Buffer of AGENT's tasks."
  (lispm-memory--visit (lispm-tasks-file agent)
                       (format "#+title: Tasks of %s\n#+todo: TODO NEXT BLOCKED | DONE CANCELLED\n"
                               (lispm-agent-name agent))))

(defun lispm-memory--entries (buffer)
  "Top-level entries of the org BUFFER, as plists.
Keys: :title :tags :todo :id :body :beg :end."
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

(defun lispm-memory--stamp ()
  "An inactive org timestamp for now."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

;;;; Memory

(defun lispm-memory-remember (agent title content &optional tags)
  "Store CONTENT under TITLE in AGENT's memory, replacing an entry of that title."
  (let* ((buffer (lispm-memory--buffer agent))
         (title (lispm--oneline title 120))
         (existing (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                             (lispm-memory--entries buffer)))
         (created (and existing
                       (with-current-buffer buffer
                         (org-entry-get (plist-get existing :beg) "CREATED")))))
    (with-current-buffer buffer
      (save-excursion
        (if existing
            (progn (delete-region (plist-get existing :beg) (plist-get existing :end))
                   (goto-char (plist-get existing :beg)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))
        (insert "* " title
                (if tags (format "  :%s:" (mapconcat #'lispm-memory--tag tags ":")) "")
                "\n:PROPERTIES:\n"
                ":CREATED:  " (or created (lispm-memory--stamp)) "\n"
                (if existing (concat ":UPDATED:  " (lispm-memory--stamp) "\n") "")
                ":END:\n"
                (org-escape-code-in-string (string-trim content)) "\n")))
    (lispm-memory--save buffer)
    (lispm-git-commit agent (format "%s: %s \"%s\"" (lispm-agent-name agent)
                                    (if existing "update memory" "remember") title))
    (format "%s memory \"%s\"." (if existing "Updated" "Saved") title)))

(defun lispm-memory--format (entry)
  "ENTRY of a memory file as text for the model."
  (concat "## " (plist-get entry :title)
          (if (plist-get entry :tags)
              (format " [%s]" (string-join (plist-get entry :tags) ", "))
            "")
          "\n" (plist-get entry :body)))

(defun lispm-memory-recall (agent query)
  "Entries of AGENT's memory matching every word of QUERY."
  (let* ((entries (and (file-exists-p (lispm-memory-file agent))
                       (lispm-memory--entries (lispm-memory--buffer agent))))
         (words (split-string (downcase query) "[ \t\n,]+" t))
         (titles (lambda (es) (mapconcat (lambda (e) (concat "- " (plist-get e :title))) es "\n")))
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
          (t (mapconcat #'lispm-memory--format (seq-take hits 20) "\n\n")))))

(defun lispm-memory-forget (agent title)
  "Delete the entry TITLE from AGENT's memory."
  (let* ((buffer (lispm-memory--buffer agent))
         (entry (seq-find (lambda (e) (string-equal-ignore-case (plist-get e :title) title))
                          (lispm-memory--entries buffer))))
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
  (let* ((buffer (lispm-tasks--buffer agent))
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
  (let* ((buffer (lispm-tasks--buffer agent))
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
            (make-process
             :name "lispm-git" :buffer (get-buffer-create " *lispm-git*") :noquery t
             :command
             (list "sh" "-c"
                   (concat "{ [ -d .git ] || git init -q; } && git add -A && "
                           "{ git diff --cached --quiet || git"
                           " -c user.name=" (shell-quote-argument author)
                           " -c user.email=" (shell-quote-argument (concat author "@lispm.invalid"))
                           " commit -q -m " (shell-quote-argument message) "; }"))))))))

(defun lispm-git--commit-turn (agent _reply)
  "Commit AGENT's session after a turn."
  (lispm-git-commit agent (format "%s: turn %d" (lispm-agent-name agent)
                                  (lispm-agent-turn agent))))

(add-hook 'lispm-turn-end-functions #'lispm-git--commit-turn)

;;;; System prompt

(defun lispm-memory--prompt (agent)
  "System prompt section listing AGENT's memories and open tasks."
  (let ((memories (and (file-exists-p (lispm-memory-file agent))
                       (lispm-memory--entries (lispm-memory--buffer agent))))
        (tasks (lispm-tasks-list agent)))
    (string-join
     (delq nil
           (list (when memories
                   (concat "# Your long-term memory\n"
                           "Titles of what you saved in earlier sessions; `recall` reads them.\n"
                           (mapconcat (lambda (e)
                                        (concat "- " (plist-get e :title)
                                                (if (plist-get e :tags)
                                                    (format " [%s]" (string-join (plist-get e :tags) ", "))
                                                  "")))
                                      (seq-take memories 200) "\n")))
                 (unless (equal tasks "No tasks.")
                   (concat "# Your open tasks\n" tasks))))
     "\n\n")))

(add-hook 'lispm-system-prompt-functions #'lispm-memory--prompt)

;;;; Tools

(lispm-deftool remember
    ((title string "Short, specific title, one line.")
     (content string "The fact, preference or lesson, self-contained.")
     (tags array "Optional tags, e.g. [\"project\", \"preference\"]."
           :optional t :items ((type . "string"))))
  "Save something to your long-term memory, an org file that persists
across sessions (versioned in git).  Use it for durable knowledge:
the user's preferences, project conventions, lessons learned.  Saving
under an existing title replaces that entry.  Do not store transient
state of the current task."
  :permission memory
  (lispm-memory-remember lispm-current-agent title content (append tags nil)))

(lispm-deftool recall
    ((query string "Words to look for; entries containing all of them are returned. An empty string lists all titles.")
     (agent string "Read another agent's memory instead of yours." :optional t))
  "Search long-term memory: yours, or another agent's."
  :permission memory
  (lispm-memory-recall (if agent (lispm-get-agent agent) lispm-current-agent) query))

(lispm-deftool forget
    ((title string "Exact title of the memory to delete."))
  "Delete an entry from your long-term memory, e.g. when it is wrong or obsolete."
  :permission memory
  (lispm-memory-forget lispm-current-agent title))

(lispm-deftool task_add
    ((title string "What needs doing, one line.")
     (notes string "Details, context, acceptance criteria." :optional t))
  "Add a task to your task list, which persists across sessions and
shows in the user's org agenda.  Use it for work that will not be
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
