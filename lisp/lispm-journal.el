;;; lispm-journal.el --- Snapshots of a project, turn by turn, in git  -*- lexical-binding: t; -*-

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

;; The journal of an agent's turn has two parts.  In memory, the text
;; every buffer had before a tool first changed it (see
;; `lispm-journal-snapshot'): it covers buffers that visit no file, and
;; edits not saved, but only the edits of tools, and only until Emacs
;; exits.  On disk, for a project in a git work tree, snapshots of the
;; whole work tree taken as each turn starts and ends: they cover what
;; shell commands, Lisp and MCP servers changed too, and outlive Emacs.
;;
;; The snapshots are commits in a repository of LispM's own, beside the
;; project's memory (projects/<project>/journal.git under
;; `lispm-directory'), whose work tree is the project's: the project's
;; repository gets no ref and no object from LispM.  A snapshot holds
;; the files git would, tracked ones and the others its ignore rules
;; let through, up to `lispm-journal-max-file-size' each.  A turn that
;; changed something is the ref refs/turns/SESSION/TIME, a commit of
;; the work tree as the turn ended, whose parent is the work tree as it
;; began; a turn Emacs stopped in the middle of is left at its
;; beginning, and taken up as one turn when its session opens again.
;; Snapshots are taken asynchronously, from the project's own file
;; list (`git ls-files'), and a turn's first request waits for its
;; first snapshot, so that no call changes anything before it is taken.

;;; Code:

(require 'lispm-core)

(defcustom lispm-journal-git t
  "Whether to take snapshots of a project's git work tree at every turn.
They let `lispm-revert-turn' and `lispm-turn-diff' cover the files
shell commands and Lisp change, and turns from before Emacs restarted.
They are kept in a repository of LispM's own (see lispm-journal.el)."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-journal-turns 20
  "How many of a session's latest turns with changes their snapshots keep."
  :type 'integer
  :group 'lispm)

(defcustom lispm-journal-max-file-size (* 1024 1024)
  "Size in bytes past which a file not tracked by git is left out of snapshots."
  :type 'integer
  :group 'lispm)

(defun lispm-journal--top (root)
  "The top of the git work tree ROOT is in, or nil."
  (when-let* ((top (locate-dominating-file root ".git")))
    (file-name-as-directory (expand-file-name top))))

(defun lispm-journal--directory (root)
  "The journal repository of the project at ROOT, made if need be; or nil."
  (let ((dir (expand-file-name "journal.git/" (lispm-project-directory root))))
    (if (file-exists-p (expand-file-name "HEAD" dir))
        dir
      (let ((process-environment (lispm--git-environment)))
        (when (zerop (call-process "git" nil nil nil "init" "-q" "--bare" dir))
          dir)))))

(defun lispm-journal--environment (dir top)
  "The environment of git commands on the journal DIR whose work tree is TOP."
  (append (list (concat "GIT_DIR=" (directory-file-name dir))
                (concat "GIT_WORK_TREE=" (directory-file-name top)))
          (lispm--git-environment)))

(defun lispm-journal--git (dir top &rest args)
  "Run git with ARGS on the journal DIR, work tree TOP; return its output or nil."
  (let ((process-environment (lispm-journal--environment dir top))
        (default-directory top))
    (with-temp-buffer
      (when (zerop (apply #'call-process "git" nil '(t nil) nil args))
        (buffer-string)))))

(defconst lispm-journal--script
  "set -e
git add -u >/dev/null 2>&1 || true
if [ -n \"$LISPM_FILES\" ]; then
  git add -f -A --pathspec-from-file=- --pathspec-file-nul >/dev/null
fi
tree=$(git write-tree)
if [ -n \"$LISPM_SAME\" ] && [ \"$tree\" = \"$LISPM_SAME\" ]; then
  git update-ref -d \"$LISPM_REF\" 2>/dev/null || true
  echo \"$tree\"
  exit 0
fi
commit=$(git -c commit.gpgSign=false -c user.name=\"$LISPM_AUTHOR\" \\
  -c user.email=\"$LISPM_AUTHOR@lispm.invalid\" commit-tree --no-gpg-sign \"$tree\" $LISPM_PARENT \\
  -m \"$LISPM_MESSAGE\")
git update-ref \"$LISPM_REF\" \"$commit\"
git for-each-ref --sort=-refname --format='%(refname)' \"$LISPM_REFS\" |
  tail -n +$((LISPM_KEEP + 1)) | while read -r ref; do git update-ref -d \"$ref\"; done
git gc --auto --quiet >/dev/null 2>&1 || true
echo \"$tree $commit\""
  "Shell script taking a snapshot of a work tree in a journal repository.
The files to add come on its standard input; the environment says the
rest (see `lispm-journal--snapshot').  It prints the snapshot's tree
and commit, or only the tree when that is LISPM_SAME, the tree a turn
began with: a turn that changed nothing leaves no ref.")

(cl-defun lispm-journal--snapshot (top dir &key index ref message author parent same callback)
  "Take a snapshot of the work tree TOP in the journal DIR, asynchronously.
INDEX is the index file to use, which keeps what files were like at
the last snapshot, so that only those changed since are read again.
The commit's MESSAGE and AUTHOR name the turn, PARENT is the commit
of its beginning, if it is its end; REF then points at it.  A tree
equal to SAME, that of the beginning, makes no commit and removes REF.
CALLBACK gets the tree and the commit, the commit nil if none was
made, or nothing at all (nil) if the snapshot failed."
  (let ((files nil)
        (process-environment (lispm--git-environment)))
    (make-process
     :name "lispm-journal-files" :buffer (generate-new-buffer " *lispm-journal*") :noquery t
     :connection-type 'pipe
     :command (list "git" "-C" top "ls-files" "-z" "--cached" "--others" "--exclude-standard")
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((ok (zerop (process-exit-status process)))
               (buffer (process-buffer process)))
           (when ok
             (setq files (seq-filter
                          (lambda (file)
                            (let ((attributes (file-attributes (expand-file-name file top))))
                              (and attributes (not (file-attribute-type attributes))
                                   (<= (file-attribute-size attributes) lispm-journal-max-file-size))))
                          (split-string (with-current-buffer buffer (buffer-string)) "\0" t))))
           (kill-buffer buffer)
           (if (not ok)
               (funcall callback nil)
             (lispm-journal--commit top dir index files ref message author parent same callback))))))))

(defun lispm-journal--commit (top dir index files ref message author parent same callback)
  "Add FILES of TOP to the journal DIR, and commit them.
INDEX, REF, MESSAGE, AUTHOR, PARENT, SAME and CALLBACK are as for
`lispm-journal--snapshot'."
  (let* ((process-environment
          (append (list (concat "GIT_INDEX_FILE=" index)
                        (concat "LISPM_FILES=" (if files "1" ""))
                        (concat "LISPM_SAME=" (or same ""))
                        (concat "LISPM_REF=" ref)
                        (concat "LISPM_REFS=" (file-name-directory ref))
                        (concat "LISPM_KEEP=" (number-to-string lispm-journal-turns))
                        (concat "LISPM_AUTHOR=" author)
                        (concat "LISPM_MESSAGE=" message)
                        (concat "LISPM_PARENT=" (if parent (concat "-p " parent) "")))
                  (lispm-journal--environment dir top)))
         (default-directory top)
         (process
          (make-process
           :name "lispm-journal" :buffer (generate-new-buffer " *lispm-journal*") :noquery t
           :connection-type 'pipe
           :command (list "sh" "-c" lispm-journal--script)
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let* ((buffer (process-buffer process))
                      (words (and (zerop (process-exit-status process))
                                  (split-string (with-current-buffer buffer (buffer-string))))))
                 (kill-buffer buffer)
                 (funcall callback (car words) (cadr words))))))))
    (process-send-string process (mapconcat (lambda (file) (concat file "\0")) files ""))
    (process-send-eof process)
    process))

(defun lispm-journal--stamp ()
  "A name for a turn's ref: the time, to the millisecond, so they sort."
  (format-time-string "%Y%m%dT%H%M%S.%3N"))

(defun lispm-journal--current (agent)
  "The snapshot of AGENT's turn under way, a plist, or nil."
  (cdr (assq (lispm-agent-turn agent) (lispm-agent-snapshots agent))))

(defun lispm-journal--latest-begun (dir session)
  "The record of SESSION's latest turn if Emacs stopped in its middle, or nil.
DIR is the journal repository."
  (let ((record (car (lispm-journal--records dir session))))
    (and record (not (plist-get record :finished)) record)))

(defun lispm-journal--index (dir agent)
  "The index file of AGENT's snapshots in the journal DIR.
Each agent has its own, so that agents of one project take theirs at
the same time without waiting for git's lock."
  (expand-file-name (concat "index-" (replace-regexp-in-string
                                      "[^[:alnum:]_.-]" "_" (symbol-name (lispm-agent-name agent))))
                    dir))

(defun lispm-journal--take (agent snapshot start)
  "Mark SNAPSHOT of AGENT's as being taken, and call START to take it.
START is called at once, or, while another snapshot of AGENT's is being
taken, once that is: they share an index file, and the beginning of a
turn must not be taken before the end of the one before."
  (let ((other (seq-find (lambda (entry)
                           (and (plist-get (cdr entry) :busy) (not (eq (cdr entry) snapshot))))
                         (lispm-agent-snapshots agent))))
    (plist-put snapshot :busy t)
    (if other
        (plist-put (cdr other) :then (append (plist-get (cdr other) :then) (list start)))
      (funcall start))))

(defun lispm-journal--taken (snapshot)
  "Note that SNAPSHOT is taken; take those that waited for it."
  (let ((then (plist-get snapshot :then)))
    (plist-put snapshot :busy nil)
    (plist-put snapshot :then nil)
    (mapc #'funcall then)))

(defun lispm-journal-begin (agent &optional resumed)
  "Take a snapshot of AGENT's project as its turn begins, if it is in git.
RESUMED non-nil if the turn is one Emacs stopped in the middle of:
then it goes on from the snapshot taken as it first began, if any."
  (when-let* (((and lispm-journal-git (executable-find "git")))
              (top (lispm-journal--top (lispm-agent-root agent)))
              (dir (lispm-journal--directory (lispm-agent-root agent))))
    (let* ((session (lispm-agent-id agent))
           (begun (and resumed (lispm-journal--latest-begun dir session)))
           (snapshot (list :top top :dir dir
                           :ref (or (plist-get begun :ref)
                                    (format "refs/turns/%s/%s" session (lispm-journal--stamp))))))
      (setf (alist-get (lispm-agent-turn agent) (lispm-agent-snapshots agent)) snapshot)
      (setf (lispm-agent-snapshots agent) (seq-take (lispm-agent-snapshots agent) lispm-journal-turns))
      (if begun
          (plist-put snapshot :before (plist-get begun :commit))
        (lispm-journal--take
         agent snapshot
         (lambda ()
           (lispm-journal--snapshot
            top dir :index (lispm-journal--index dir agent) :ref (plist-get snapshot :ref)
            :message (format "%s turn %d begins" (lispm-agent-name agent) (lispm-agent-turn agent))
            :author (symbol-name (lispm-agent-name agent))
            :callback (lambda (_tree commit)
                        (plist-put snapshot :before commit)
                        (lispm-journal--taken snapshot)))))))))

(defun lispm-journal-busy-p (agent)
  "Non-nil while a snapshot of AGENT's is being taken.
That of the turn under way, or of the end of the one before, which
must see the files as that turn left them."
  (seq-some (lambda (entry) (plist-get (cdr entry) :busy))
            (lispm-agent-snapshots agent)))

(defun lispm-journal-end (agent)
  "Take a snapshot of AGENT's project as its turn ends.
A turn that changed nothing leaves no snapshot."
  (when-let* ((snapshot (lispm-journal--current agent))
              (before (plist-get snapshot :before)))
    (let ((same (string-trim (or (lispm-journal--git (plist-get snapshot :dir) (plist-get snapshot :top)
                                                     "rev-parse" (concat before "^{tree}"))
                                 ""))))
      (lispm-journal--take
       agent snapshot
       (lambda ()
         (lispm-journal--snapshot
          (plist-get snapshot :top) (plist-get snapshot :dir)
          :index (lispm-journal--index (plist-get snapshot :dir) agent)
          :ref (plist-get snapshot :ref)
          :message (format "%s turn %d" (lispm-agent-name agent) (lispm-agent-turn agent))
          :author (symbol-name (lispm-agent-name agent))
          :parent before :same same
          :callback (lambda (_tree commit)
                      (plist-put snapshot :after commit)
                      (lispm-journal--taken snapshot))))))))

(defun lispm-journal--records (dir session)
  "The turns of SESSION the journal DIR holds, newest first, as plists.
Each has :ref, :commit, :subject, :time (its ref's name) and
:finished, nil for a turn Emacs stopped in the middle of, whose
:commit is then its beginning."
  (when-let* ((output (let ((process-environment (cons (concat "GIT_DIR=" (directory-file-name dir))
                                                       (lispm--git-environment))))
                        (with-temp-buffer
                          (when (zerop (call-process "git" nil '(t nil) nil "for-each-ref" "--sort=-refname"
                                                     "--format=%(refname)%09%(objectname)%09%(subject)"
                                                     (format "refs/turns/%s/" session)))
                            (buffer-string))))))
    (mapcar (lambda (line)
              (pcase-let ((`(,ref ,commit ,subject) (split-string line "\t")))
                (list :ref ref :commit commit :subject subject
                      :time (file-name-nondirectory ref)
                      :finished (not (string-suffix-p " begins" subject)))))
            (split-string output "\n" t))))

(defun lispm-journal-records (agent)
  "The turns with changes of AGENT's session its project's journal holds.
Newest first; each record is a plist as `lispm-journal--records'
gives it, with :dir and :top, and :turn, the turn's number, for a
turn of this Emacs session.  Only finished turns are listed."
  (when-let* (((and lispm-journal-git (executable-find "git")))
              (top (lispm-journal--top (lispm-agent-root agent)))
              (dir (let ((dir (expand-file-name "journal.git/"
                                                (lispm-project-directory (lispm-agent-root agent)))))
                     (and (file-exists-p dir) dir))))
    (mapcar (lambda (record)
              (append record
                      (list :dir dir :top top
                            :turn (car (seq-find (lambda (entry)
                                                   (equal (plist-get (cdr entry) :ref)
                                                          (plist-get record :ref)))
                                                 (lispm-agent-snapshots agent))))))
            (seq-filter (lambda (record) (plist-get record :finished))
                        (lispm-journal--records dir (lispm-agent-id agent))))))

(defun lispm-journal-changes (record)
  "The files RECORD's turn changed: a list of (STATUS . FILE).
STATUS is A (added), D (deleted) or M (changed); FILE is absolute."
  (when-let* ((output (lispm-journal--git (plist-get record :dir) (plist-get record :top)
                                          "diff" "--no-renames" "--name-status" "-z"
                                          (concat (plist-get record :commit) "^")
                                          (plist-get record :commit))))
    (let ((fields (split-string output "\0" t)) changes)
      (while fields
        (let ((status (substring (pop fields) 0 1))
              (file (pop fields)))
          (push (cons status (expand-file-name file (plist-get record :top))) changes)))
      (nreverse changes))))

(defun lispm-journal-text-before (record file)
  "The text FILE had as RECORD's turn began, or nil if it did not exist."
  (lispm-journal--git (plist-get record :dir) (plist-get record :top)
                      "cat-file" "blob"
                      (concat (plist-get record :commit) "^:"
                              (file-relative-name file (plist-get record :top)))))

(defun lispm-journal-diff (record)
  "The diff of what RECORD's turn changed."
  (or (lispm-journal--git (plist-get record :dir) (plist-get record :top)
                          "diff" "--no-color" "--no-renames"
                          (concat (plist-get record :commit) "^") (plist-get record :commit))
      ""))

(defun lispm-journal-restore (record file)
  "Give FILE back, on disk, the contents it had as RECORD's turn began."
  (lispm-journal--git (plist-get record :dir) (plist-get record :top)
                      "checkout" (concat (plist-get record :commit) "^") "--"
                      (file-relative-name file (plist-get record :top))))

(defun lispm-journal-forget (record)
  "Remove RECORD's turn from the journal, once it is reverted."
  (lispm-journal--git (plist-get record :dir) (plist-get record :top)
                      "update-ref" "-d" (plist-get record :ref)))

(provide 'lispm-journal)
;;; lispm-journal.el ends here
