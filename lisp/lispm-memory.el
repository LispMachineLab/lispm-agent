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
;; A character has a memory of its own besides, which goes with it to
;; every place it has a conversation in:
;;
;;   characters/<name>/memory.org   what it learned that is its own, as
;;                                  what it knows of the people it works
;;                                  with, wherever it learned it.
;;
;; Its own conversation, in no project, has that memory for its own.
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
(require 'lispm-job)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-memory-store)

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

;; A pass costs a request holding the whole memory, so it is not made
;; again for a memory as it is: not before the memory says something
;; new, and, after a pass that failed or freed nothing, not for
;; `lispm-memory-retry-after' seconds.  When an entry was recalled is
;; not something new.

(defconst lispm-memory-retry-after (* 6 3600)
  "Seconds before a memory is tried again, after a consolidation that failed.")

(defvar lispm-memory--consolidating (make-hash-table :test #'equal)
  "Memory files being consolidated now.")

(defvar lispm-memory--tried (make-hash-table :test #'equal)
  "Memory files consolidated or tried, to (HASH . NOT-BEFORE).
HASH is that of what the memory said then (`lispm-memory--hash'), and
NOT-BEFORE the time before which it is not tried again.")

(defun lispm-memory--text (buffer)
  "The whole text of BUFFER, without properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lispm-memory--hash (text)
  "Hash of what a memory file holding TEXT says.
When its entries were last recalled, and how often, is left out."
  (secure-hash 'sha1 (replace-regexp-in-string "^:RECALL\\(?:ED\\|S\\):.*\n" "" text)))

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

(defun lispm-memory--progress-p (old new)
  "Non-nil if a memory of size NEW is nearer its budget than one of size OLD.
Sizes are (ENTRIES . TOKENS).  Whatever part of the budget OLD is
past, NEW must be smaller in; within the budget, smaller in either."
  (let ((past (delq nil (list (and lispm-memory-max-entries
                                   (> (car old) lispm-memory-max-entries) #'car)
                              (and lispm-memory-max-tokens
                                   (> (cdr old) lispm-memory-max-tokens) #'cdr)))))
    (if past
        (seq-every-p (lambda (size) (< (funcall size new) (funcall size old))) past)
      (or (< (car new) (car old)) (< (cdr new) (cdr old))))))

(defun lispm-memory--due-p (file)
  "Non-nil if the memory in FILE is to be consolidated now.
That is while memory is under git (see `lispm-git-p'), if it is past
its budget, not being consolidated, and says something new since it
was last tried, once the time to wait after that is over."
  (and (lispm-git-p)
       (file-exists-p file)
       (not (gethash file lispm-memory--consolidating))
       (let ((text (lispm-file-text file))
             (tried (gethash file lispm-memory--tried)))
         (and (lispm-memory--over-budget-p text)
              (or (null tried)
                  (and (not (equal (car tried) (lispm-memory--hash text)))
                       (time-less-p (cdr tried) nil)))))))

(defun lispm-memory--scope-name (agent scope)
  "SCOPE of AGENT's memory, named for a commit message."
  (pcase (lispm-memory-scope agent scope)
    ('role (format "role memory (%s)" (lispm-agent-role agent)))
    (scope (format "%s memory" scope))))

(defun lispm-memory--maintain (agent _reply)
  "Once AGENT's turn has ended, consolidate its memories past their budget at idle.
Only while memory is under git: a consolidation drops and rewrites
entries, and its commit is what keeps them."
  (when (lispm-backend-name agent)
    (dolist (scope (lispm-memory-scopes agent))
      (let ((file (lispm-memory-file agent scope)))
        (when (lispm-memory--due-p file)
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
is carried out, as one commit, if the memory still says what it said
then and the plan brings it nearer its budget.  Not while memory is
not under git (see `lispm-git-p'), nor once AGENT has ended.  See
`lispm-memory-max-entries'."
  (let ((file (lispm-memory-file agent scope)))
    (when (and (memq agent (lispm-agents))
               (lispm-git-p)
               (file-exists-p file)
               (not (gethash file lispm-memory--consolidating)))
      (let* ((buffer (lispm-memory-buffer agent scope))
             (text (lispm-memory--text buffer))
             (hash (lispm-memory--hash text))
             (entries (lispm-memory-entries buffer))
             (failed (lambda (reason)
                       (remhash file lispm-memory--consolidating)
                       (puthash file (cons hash (time-add nil lispm-memory-retry-after))
                                lispm-memory--tried)
                       (message "%s" (lispm-tr "LispM: could not consolidate %s: %s"
                                               (abbreviate-file-name file) reason)))))
        (puthash file (cons hash 0) lispm-memory--tried)
        (puthash file t lispm-memory--consolidating)
        (condition-case err
            (let* ((backend (lispm-agent-backend* agent))
                   (request (lispm-api-request
                             backend (or lispm-memory-model (lispm-agent-model* agent)) "low"
                             (format lispm-memory--consolidate-prompt (lispm-memory--budget)) nil
                             `(((role . "user")
                                (content . [((type . "text")
                                             (text . ,(format "Today is %s. The memory, %d entries, about %d tokens:\n\n%s"
                                                              (format-time-string "%Y-%m-%d")
                                                              (length entries)
                                                              (cdr (lispm-memory--size text))
                                                              (lispm-memory--describe entries))))]))))))
              (when (buffer-modified-p buffer)
                (error "Its buffer has unsaved changes"))
              (let (handle job)
                (setq job (lispm-job-register
                           agent 'memory-consolidate
                           (lambda ()
                             (remhash file lispm-memory--consolidating)
                             (cond ((functionp handle) (funcall handle))
                                   ((and (processp handle) (process-live-p handle))
                                    (process-put handle 'lispm-aborted t)
                                    (lispm-exec-delete handle))))))
                (condition-case err
                    (setq handle
                          (funcall lispm-api-function backend request
                                   :on-done
                                   (lambda (message)
                                     (when (lispm-job-finish job)
                                       (condition-case err
                                           (progn
                                             (lispm-memory--carry-out
                                              agent scope hash
                                              (lispm-memory--read-plan
                                               (lispm-content-text
                                                (append (alist-get 'content message) nil))
                                               (length entries)))
                                             (remhash file lispm-memory--consolidating))
                                         (error (funcall failed (error-message-string err))))))
                                   :on-error
                                   (lambda (err)
                                     (when (lispm-job-finish job)
                                       (funcall failed (plist-get err :message))))))
                  (error
                   (lispm-job-finish job)
                   (signal (car err) (cdr err))))))
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
                   (lispm-json-decode (substring reply start (1+ end)))
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
    (lispm-memory-entry-text
     title tags
     `(("CREATED" . ,(or (car (funcall stamps :created)) (lispm-memory-stamp)))
       ("UPDATED" . ,(lispm-memory-stamp))
       ("RECALLED" . ,(car (last (funcall stamps :recalled))))
       ("RECALLS" . ,(and (> recalls 0) (number-to-string recalls)))
       ("MERGED" . ,(mapconcat (lambda (e) (lispm-oneline (plist-get e :title) 120))
                               sources " | ")))
     content)))

(defun lispm-memory--apply-recall-properties (entry-text entry)
  "Return ENTRY-TEXT with ENTRY's overlaid recall metadata applied."
  (with-temp-buffer
    (insert entry-text)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (when-let* ((recalled (plist-get entry :recalled)))
      (org-entry-put nil "RECALLED" recalled))
    (when-let* ((recalls (plist-get entry :recalls))
                ((> recalls 0)))
      (org-entry-put nil "RECALLS" (number-to-string recalls)))
    (buffer-string)))

(defun lispm-memory--carry-out (agent scope hash plan)
  "Carry out the consolidation PLAN of AGENT's memory in SCOPE.
The plan was made when what the memory said had HASH (see
`lispm-memory--hash'); if it says something else now, or its buffer
has unsaved changes, that is an error, and nothing is changed.  So is
a plan leaving two entries of one title, or one not bringing the
memory nearer its budget.  The entries a plan does not name stay as
they are, in their place; an entry replacing others takes the place
of the first of them.  The change is one commit, which names every
entry merged or dropped."
  (pcase-let* ((`(,replace . ,drop) plan)
               (buffer (lispm-memory-buffer agent scope nil t))
               (file (buffer-file-name buffer))
               (expected (or (lispm-file-sha256 file) :missing))
               (old (lispm-memory--text buffer))
               (entries (vconcat (lispm-memory-entries buffer)))
               (line (lambda (id) (format "\"%s\"" (plist-get (aref entries (1- id)) :title))))
               (parts nil)
               (titles nil))
    (unless (equal (lispm-memory--hash old) hash)
      (error "The memory changed meanwhile"))
    (when (buffer-modified-p buffer)
      (error "Its buffer has unsaved changes"))
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
                           (push (nth 1 group) titles)
                           (push (pcase-let ((`(,ids ,title ,tags ,content) group))
                                   (lispm-memory--merged
                                    (mapcar (lambda (i) (aref entries (1- i))) (sort (copy-sequence ids) #'<))
                                    title tags content))
                                 parts))
                          (group)
                          (t (push (plist-get e :title) titles)
                             (push (let* ((text (buffer-substring-no-properties
                                                 (plist-get e :beg) (plist-get e :end)))
                                          (text (if (lispm-memory-recall-stat file (plist-get e :title))
                                                    (lispm-memory--apply-recall-properties text e)
                                                  text)))
                                     (if (string-suffix-p "\n" text) text (concat text "\n")))
                                   parts))))))
    (let ((new (apply #'concat (nreverse parts))))
      (when-let* ((twice (seq-find (lambda (title)
                                     (< 1 (seq-count (lambda (other) (string-equal-ignore-case other title))
                                                     titles)))
                                   titles)))
        (error "The plan leaves two entries titled \"%s\"" twice))
      (unless (lispm-memory--progress-p (lispm-memory--size old) (lispm-memory--size new))
        (error "The plan does not bring the memory nearer its budget"))
      (lispm-memory-commit-buffer
       buffer
       (lambda ()
         (save-restriction
           (widen)
           (let ((point (point))
                 (inhibit-read-only t))
             (erase-buffer)
             (insert new)
             (goto-char (min point (point-max))))))
       expected)
      (lispm-memory-clear-recall-stats file)
      (puthash file (cons (lispm-memory--hash new) 0) lispm-memory--tried)
      (lispm-git-commit
       agent
       (concat (format "%s: consolidate %s, %d entries to %d\n\n"
                       (lispm-agent-name agent) (lispm-memory--scope-name agent scope)
                       (length entries) (car (lispm-memory--size new)))
               (mapconcat (pcase-lambda (`(,ids ,title . ,_))
                            (format "merge %s into \"%s\"" (mapconcat line ids ", ") title))
                          replace "\n")
               (if (and replace drop) "\n" "")
               (mapconcat (pcase-lambda (`(,id . ,reason))
                            (format "drop %s: %s" (funcall line id) (lispm-oneline reason 200)))
                          drop "\n"))
       (list file)))))

;;;; Tools

(lispm-deftool remember
    ((title string "Short, specific title, one line.")
     (content string "The fact, preference or lesson, self-contained.")
     (tags array "Optional tags, e.g. [\"build\", \"preference\"]."
           :optional t :items ((type . "string")))
     (scope string "project (default): about where you work, shared with every agent working there: this project, or, for an agent of the whole system, the system. role: true everywhere, e.g. the user's general preferences; shared with agents of your role in all projects. self, for a character only: your own, in every conversation you have, e.g. what you know of the people you work with."
            :optional t :enum ("project" "role" "self")))
  "Save something to long-term memory, org files that persist across
sessions (versioned in git).  Use it for durable knowledge: project
conventions, the user's preferences, lessons learned.  Saving under an
existing title replaces that entry.  Do not store transient state of
the current task."
  :permission memory
  (lispm-memory-remember lispm-current-agent title content (append tags nil)
                         (pcase scope ("role" 'role) ("self" 'self) (_ 'project))))

(lispm-deftool recall
    ((query string "Words to look for; entries containing all of them are returned. An empty string lists all titles.")
     (role string "Also read the role memory of another role instead of yours." :optional t))
  "Search long-term memory: this project's (or the system's), then your role's.
A character's own memory is searched first."
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
