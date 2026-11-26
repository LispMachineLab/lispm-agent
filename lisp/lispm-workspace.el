;;; lispm-workspace.el --- Isolated workspaces and lock-free reconciliation  -*- lexical-binding: t; -*-

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

;; First-class isolated execution workspaces for parallel agents:
;;
;;   - `git-worktree'  isolated checkout on a branch of its own
;;   - `cow-clone'     copy-on-write clone for non-git trees (APFS / Btrfs)
;;   - `shared'        shared tree without copying files
;;
;; Every workspace gets an isolated journal repository path under the
;; project directory (`lispm-workspace-resolve-journal-dir'), keeping
;; turn snapshots and reverts independent across concurrent agents.
;; Metadata lives under `lispm-directory' so workspace checkouts never
;; acquire untracked files.
;;
;; Disjoint and conflicting branches reconcile lock-free against any
;; target ref via `git merge-tree --write-tree' and compare-and-swap
;; `git update-ref', without touching the user's working tree.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-exec)
(require 'lispm-util)
(require 'lispm-core)

(defcustom lispm-workspace-directory "~/.lispm/workspaces/"
  "Default directory holding agent workspaces outside git worktrees."
  :type 'directory
  :group 'lispm)

(defvar lispm-workspace--registry (make-hash-table :test #'equal)
  "Registry of live and loaded `lispm-workspace' structs by id.")

;; Basis: (id kind origin path branch base-ref journal-dir owner-id created-at metadata)
;; Span: All execution-workspace backends (`git-worktree', `cow-clone', `shared').
;; Minimality: Every slot uniquely identifies backend identity, directory topology,
;;             journal isolation, merge baseline, owner binding, or backend metadata.
(cl-defstruct (lispm-workspace (:constructor lispm-workspace--create)
                               (:copier nil))
  "Isolated execution workspace bound to an agent session."
  id
  kind
  origin
  path
  branch
  base-ref
  journal-dir
  owner-id
  created-at
  metadata)

;;;; Canonical Paths & Persistence

(defun lispm-workspace--canon-dir (directory)
  "Return canonicalized directory name for DIRECTORY, resolving symlinks."
  (let* ((expanded (directory-file-name (expand-file-name directory)))
         (tail nil)
         (cur expanded))
    (while (and cur (not (file-exists-p cur)))
      (let ((parent (directory-file-name (file-name-directory cur))))
        (if (equal parent cur)
            (setq cur nil)
          (push (file-name-nondirectory cur) tail)
          (setq cur parent))))
    (let ((base (if cur (directory-file-name (file-truename cur)) expanded)))
      (file-name-as-directory
       (if tail (expand-file-name (string-join tail "/") base) base)))))

(defun lispm-workspace-path-slug (directory)
  "Return a deterministic filesystem-safe slug for DIRECTORY."
  (let* ((canon (directory-file-name (lispm-workspace--canon-dir directory)))
         (leaf (replace-regexp-in-string "[^[:alnum:]._-]" "-"
                                         (file-name-nondirectory canon)))
         (digest (substring (secure-hash 'sha256 canon) 0 12)))
    (format "%s-%s" (if (string-empty-p leaf) "root" leaf) digest)))

(defun lispm-workspace--store-directory ()
  "Return the directory holding workspace metadata files."
  (expand-file-name "workspaces/" lispm-directory))

(defun lispm-workspace--store-file (id)
  "Return the metadata file path for workspace ID."
  (expand-file-name (format "%s.eld" id) (lispm-workspace--store-directory)))

(defun lispm-workspace--serialize (workspace)
  "Convert WORKSPACE struct into a serializable plist."
  (list :id (lispm-workspace-id workspace)
        :kind (lispm-workspace-kind workspace)
        :origin (lispm-workspace-origin workspace)
        :path (lispm-workspace-path workspace)
        :branch (lispm-workspace-branch workspace)
        :base-ref (lispm-workspace-base-ref workspace)
        :journal-dir (lispm-workspace-journal-dir workspace)
        :owner-id (lispm-workspace-owner-id workspace)
        :created-at (lispm-workspace-created-at workspace)
        :metadata (lispm-workspace-metadata workspace)))

(defun lispm-workspace--deserialize (plist)
  "Reconstruct a `lispm-workspace' struct from PLIST."
  (when (and (listp plist) (stringp (plist-get plist :id)))
    (lispm-workspace--create
     :id (plist-get plist :id)
     :kind (plist-get plist :kind)
     :origin (plist-get plist :origin)
     :path (plist-get plist :path)
     :branch (plist-get plist :branch)
     :base-ref (plist-get plist :base-ref)
     :journal-dir (plist-get plist :journal-dir)
     :owner-id (plist-get plist :owner-id)
     :created-at (plist-get plist :created-at)
     :metadata (plist-get plist :metadata))))

(defun lispm-workspace--persist (workspace)
  "Register WORKSPACE in memory and write its metadata atomically."
  (puthash (lispm-workspace-id workspace) workspace lispm-workspace--registry)
  (let ((print-length nil)
        (print-level nil))
    (lispm-write-file-atomic (lispm-workspace--store-file (lispm-workspace-id workspace))
                             (concat (prin1-to-string (lispm-workspace--serialize workspace)) "\n")))
  workspace)

(defun lispm-workspace--load-file (file)
  "Load and cache a `lispm-workspace' from metadata FILE, or return nil."
  (when (file-readable-p file)
    (condition-case nil
        (let* ((plist (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer))))
               (ws (lispm-workspace--deserialize plist)))
          (when ws
            (puthash (lispm-workspace-id ws) ws lispm-workspace--registry))
          ws)
      (error nil))))

(defun lispm-workspace--load-all ()
  "Load all persisted workspaces from `lispm-directory' into the registry."
  (let ((dir (lispm-workspace--store-directory)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.eld\\'" t))
        (let ((id (file-name-sans-extension (file-name-nondirectory file))))
          (unless (gethash id lispm-workspace--registry)
            (lispm-workspace--load-file file)))))))

(defun lispm-workspace-for-worktree (origin path branch &optional name)
  "Construct a transient `git-worktree' handle for ORIGIN, PATH, and BRANCH.
Useful for historical worktrees not present in the live registry; optional
NAME records the short worktree name."
  (lispm-workspace--create
   :id (or name (file-name-nondirectory (directory-file-name path)))
   :kind 'git-worktree
   :origin (file-name-as-directory (expand-file-name origin))
   :path (file-name-as-directory (expand-file-name path))
   :branch branch
   :metadata (when name (list :name name))))

(defun lispm-workspace-get (id)
  "Return the `lispm-workspace' identified by ID, loading from disk if needed."
  (when (stringp id)
    (or (gethash id lispm-workspace--registry)
        (lispm-workspace--load-file (lispm-workspace--store-file id)))))

(defun lispm-workspace-find-by-path (root)
  "Return the registered workspace containing ROOT, or nil."
  (when (and (stringp root) (not (file-remote-p root)))
    (lispm-workspace--load-all)
    (let ((canon (lispm-workspace--canon-dir root))
          best
          (best-len 0))
      (maphash (lambda (_id ws)
                 (when-let* ((wpath (lispm-workspace-path ws))
                             ((not (eq (lispm-workspace-kind ws) 'shared)))
                             (wcanon (lispm-workspace--canon-dir wpath)))
                   (when (and (or (equal canon wcanon)
                                  (file-in-directory-p canon wcanon))
                              (> (length wcanon) best-len))
                     (setq best ws
                           best-len (length wcanon)))))
               lispm-workspace--registry)
      best)))

(defun lispm-workspace-list (&optional origin)
  "Return all registered `lispm-workspace' objects, optionally filtered by ORIGIN."
  (lispm-workspace--load-all)
  (let ((canon-origin (and origin (lispm-workspace--canon-dir origin)))
        result)
    (maphash (lambda (_id ws)
               (when (or (null canon-origin)
                         (and (lispm-workspace-origin ws)
                              (equal (lispm-workspace--canon-dir (lispm-workspace-origin ws))
                                     canon-origin)))
                 (push ws result)))
             lispm-workspace--registry)
    (nreverse result)))

(defun lispm-workspace-effective-root (workspace)
  "Return the root directory an agent operates in inside WORKSPACE."
  (let ((prefix (plist-get (lispm-workspace-metadata workspace) :prefix))
        (base (lispm-workspace-path workspace)))
    (file-name-as-directory
     (if (and (stringp prefix) (not (string-empty-p prefix)))
         (expand-file-name prefix base)
       (expand-file-name base)))))

(defun lispm-workspace-resolve-journal-dir (root &optional workspace)
  "Return the isolated journal repository path for ROOT and optional WORKSPACE.
Linked git worktrees and registered workspaces get isolated repositories
under the project's `workspaces/' subdirectory; the main checkout keeps
its historical `journal.git/' path for backward compatibility."
  (let ((ws (or (and (lispm-workspace-p workspace) workspace)
                (lispm-workspace-find-by-path root))))
    (cond
     ((and ws (lispm-workspace-journal-dir ws))
      (lispm-workspace-journal-dir ws))
     ((and (not (file-remote-p root)))
      (let* ((top (when-let* ((found (locate-dominating-file root ".git")))
                    (lispm-workspace--canon-dir found)))
             (main (when top (lispm-workspace--canon-dir (lispm-main-worktree top)))))
        (if (and top main (not (equal top main)))
            (expand-file-name (format "workspaces/%s/journal.git/"
                                      (lispm-workspace-path-slug top))
                              (lispm-project-directory main))
          (expand-file-name "journal.git/" (lispm-project-directory root)))))
     (t (expand-file-name "journal.git/" (lispm-project-directory root))))))

(defun lispm-workspace-root-for-path (root)
  "Return non-git CoW workspace top directory containing ROOT, or nil."
  (when-let* ((ws (lispm-workspace-find-by-path root))
              ((eq (lispm-workspace-kind ws) 'cow-clone)))
    (file-name-as-directory (expand-file-name (lispm-workspace-path ws)))))

;;;; Attachment

(defun lispm-workspace-attach (agent workspace)
  "Attach WORKSPACE to AGENT and persist ownership; return WORKSPACE."
  (unless (lispm-agent-p agent)
    (error "Expected a lispm-agent, got %S" agent))
  (unless (lispm-workspace-p workspace)
    (error "Expected a lispm-workspace, got %S" workspace))
  (setf (lispm-agent-workspace agent) workspace
        (lispm-workspace-owner-id workspace) (lispm-principal-id agent))
  (lispm-workspace--persist workspace)
  workspace)

(defun lispm-workspace-detach (agent)
  "Detach any workspace attached to AGENT."
  (when-let* ((ws (and (lispm-agent-p agent) (lispm-agent-workspace agent))))
    (setf (lispm-agent-workspace agent) nil)
    (when (equal (lispm-workspace-owner-id ws) (lispm-principal-id agent))
      (setf (lispm-workspace-owner-id ws) nil)
      (when (file-exists-p (lispm-workspace--store-file (lispm-workspace-id ws)))
        (lispm-workspace--persist ws))))
  nil)

;;;; Async Git Helpers

(defun lispm-workspace--git-raw (directory args callback)
  "Run git ARGS asynchronously in DIRECTORY and pass (EXIT OUTPUT ERR) to CALLBACK."
  (let ((default-directory (file-name-as-directory (expand-file-name directory)))
        (process-environment (lispm-git-environment)))
    (lispm-exec "git" args callback)))

(defun lispm-workspace-git-output (directory args callback)
  "Run git ARGS asynchronously in DIRECTORY and call CALLBACK with (OUTPUT ERR)."
  (lispm-workspace--git-raw
   directory args
   (lambda (exit output errors)
     (if (eql exit 0)
         (funcall callback (string-trim-right output) nil)
       (let ((msg (string-trim (if (string-empty-p errors) (format "%s" output) errors))))
         (funcall callback nil (format "git %s: %s" (car args) msg)))))))

(defun lispm-workspace-repository (root callback)
  "Resolve git repository `(TOP . PREFIX)' for ROOT without blocking.
Calls CALLBACK with `(REPO ERR)'."
  (lispm-workspace-git-output
   root '("rev-parse" "--show-toplevel" "--show-prefix")
   (lambda (output err)
     (if err
         (funcall callback nil (format "%s is not in a git repository" (abbreviate-file-name root)))
       (let ((lines (split-string output "\n")))
         (funcall callback (cons (file-name-as-directory (car lines)) (or (cadr lines) "")) nil))))))

;;;; CoW Clone Helpers & Content Hashing

(defun lispm-workspace--scan-hashes (root)
  "Return sorted alist of `(REL-PATH . SHA256)' for regular files under ROOT."
  (let* ((canon-root (file-name-as-directory (expand-file-name root)))
         (files (seq-filter
                 (lambda (file)
                   (and (file-regular-p file)
                        (not (file-symlink-p file))
                        (let ((rel (file-relative-name file canon-root)))
                          (not (or (equal rel ".git")
                                   (string-prefix-p ".git/" rel))))))
                 (directory-files-recursively canon-root "" nil
                                              (lambda (dir)
                                                (not (equal (file-name-nondirectory
                                                             (directory-file-name dir))
                                                            ".git"))))))
         alist)
    (dolist (file files)
      (push (cons (file-relative-name file canon-root) (lispm-file-sha256 file)) alist))
    (sort alist (lambda (a b) (string< (car a) (car b))))))

(defun lispm-workspace--copy-cow (origin target callback)
  "Copy ORIGIN contents into TARGET asynchronously using CoW when available.
Calls CALLBACK with nil on success, or an error string."
  (let* ((src (concat (directory-file-name (expand-file-name origin)) "/."))
         (dst (directory-file-name (expand-file-name target)))
         (primary (if (eq system-type 'darwin)
                      (list "-cRp" src dst)
                    (list "-a" "--reflink=auto" src dst)))
         (fallback (list "-Rp" src dst)))
    (make-directory target t)
    (lispm-exec "cp" primary
                (lambda (exit _out err)
                  (if (eql exit 0)
                      (funcall callback nil)
                    (lispm-exec "cp" fallback
                                (lambda (exit2 _out2 err2)
                                  (if (eql exit2 0)
                                      (funcall callback nil)
                                    (funcall callback
                                             (let ((msg (string-trim (if (string-empty-p err2) err err2))))
                                               (if (string-empty-p msg) "Failed to copy workspace" msg)))))))))))

;;;; Workspace Creation

(defun lispm-workspace--new-id (name)
  "Generate a unique workspace identifier incorporating NAME."
  (format "%s-%s-%04x" name (format-time-string "%Y%m%d-%H%M%S") (random #x10000)))

(cl-defun lispm-workspace-create (origin callback &key kind name base branch-prefix directory id)
  "Create an isolated workspace from ORIGIN asynchronously.
Passes `(WS ERR)' to CALLBACK.  KIND is `auto', `git-worktree',
`cow-clone', or `shared'.  NAME names the workspace, BASE picks the git
starting ref, BRANCH-PREFIX prefixes the branch, DIRECTORY overrides
`lispm-workspace-directory', and ID sets an explicit workspace id."
  (let* ((kind (or kind 'auto))
         (origin-dir (file-name-as-directory (expand-file-name origin)))
         (name (or name (format "ws-%04x" (random #x10000))))
         (branch-prefix (or branch-prefix "lispm/"))
         (branch (concat branch-prefix name))
         (base (or base "HEAD"))
         (ws-id (or id (lispm-workspace--new-id name)))
         (storage (file-name-as-directory (expand-file-name (or directory lispm-workspace-directory)))))
    (unless (string-match-p "\\`[[:alnum:]][[:alnum:]._-]*\\'" name)
      (error "A worktree name is letters, digits, dots, dashes and underscores"))
    (cl-labels
        ((make-worktree ()
           (lispm-workspace-repository
            origin-dir
            (lambda (repository repo-err)
              (if repo-err
                  (funcall callback nil repo-err)
                (let* ((main-top (lispm-main-worktree (car repository)))
                       (path (expand-file-name (format "%s/%s/" (lispm-project-name main-top) name)
                                               storage)))
                  (if (file-exists-p path)
                      (funcall callback nil (format "%s exists already; choose another name"
                                                    (abbreviate-file-name path)))
                    (lispm-workspace-git-output
                     origin-dir (list "rev-parse" "--verify" (concat base "^{commit}"))
                     (lambda (sha rev-err)
                       (if rev-err
                           (funcall callback nil (format "No commit %s here" base))
                         (make-directory (file-name-parent-directory path) t)
                         (lispm-workspace-git-output
                          origin-dir (list "worktree" "add" "-b" branch path sha)
                          (lambda (_out add-err)
                            (if add-err
                                (funcall callback nil add-err)
                              (let* ((jdir (expand-file-name
                                            (format "workspaces/%s/journal.git/"
                                                    (lispm-workspace-path-slug path))
                                            (lispm-project-directory main-top)))
                                     (ws (lispm-workspace--create
                                          :id ws-id
                                          :kind 'git-worktree
                                          :origin (file-name-as-directory (expand-file-name (car repository)))
                                          :path (file-name-as-directory (expand-file-name path))
                                          :branch branch
                                          :base-ref sha
                                          :journal-dir jdir
                                          :created-at (float-time)
                                          :metadata (list :name name
                                                          :prefix (cdr repository)
                                                          :base base))))
                                (lispm-workspace--persist ws)
                                (funcall callback ws nil))))))))))))))
         (make-cow ()
           (let ((path (file-name-as-directory
                        (expand-file-name (format "%s/%s/" (lispm-project-name origin-dir) name)
                                          storage))))
             (cond
              ((not (file-directory-p origin-dir))
               (funcall callback nil (format "Origin directory does not exist: %s"
                                             (abbreviate-file-name origin-dir))))
              ((file-exists-p path)
               (funcall callback nil (format "%s exists already; choose another name"
                                             (abbreviate-file-name path))))
              (t
               (lispm-workspace--copy-cow
                origin-dir path
                (lambda (cp-err)
                  (if cp-err
                      (progn
                        (ignore-errors (delete-directory path t))
                        (funcall callback nil cp-err))
                    (let* ((baseline (lispm-workspace--scan-hashes path))
                           (jdir (expand-file-name
                                  (format "workspaces/%s/journal.git/"
                                          (lispm-workspace-path-slug path))
                                  (lispm-project-directory origin-dir)))
                           (ws (lispm-workspace--create
                                :id ws-id
                                :kind 'cow-clone
                                :origin origin-dir
                                :path path
                                :journal-dir jdir
                                :created-at (float-time)
                                :metadata (list :name name :baseline baseline))))
                      (lispm-workspace--persist ws)
                      (funcall callback ws nil)))))))))
         (make-shared ()
           (let ((ws (lispm-workspace--create
                      :id ws-id
                      :kind 'shared
                      :origin origin-dir
                      :path origin-dir
                      :journal-dir (lispm-workspace-resolve-journal-dir origin-dir)
                      :created-at (float-time)
                      :metadata (list :name name))))
             (lispm-workspace--persist ws)
             (funcall callback ws nil))))
      (pcase kind
        ('auto (lispm-workspace-repository
                origin-dir
                (lambda (repo _err)
                  (if repo (make-worktree) (make-cow)))))
        ('git-worktree (make-worktree))
        ('cow-clone (make-cow))
        ('shared (make-shared))
        (_ (error "Unknown workspace kind: %S" kind))))))

;;;; Workspace Inspection & Listing

(defun lispm-workspace-buffers (path)
  "Return live buffers visiting files inside PATH."
  (let ((canon (lispm-workspace--canon-dir path))
        (raw (file-name-as-directory (expand-file-name path))))
    (seq-filter (lambda (buffer)
                  (when-let* ((file (buffer-file-name buffer)))
                    (or (file-in-directory-p file raw)
                        (file-in-directory-p (expand-file-name file) canon))))
                (buffer-list))))

(defun lispm-workspace-list-worktrees (root directory callback)
  "List git worktrees of ROOT's repository under DIRECTORY asynchronously.
Calls CALLBACK with `(TREES ERR)', each element `(NAME PATH BRANCH)'."
  (lispm-workspace-git-output
   root '("worktree" "list" "--porcelain")
   (lambda (output err)
     (if err
         (funcall callback nil err)
       (let ((under (expand-file-name directory))
             (canon-under (lispm-workspace--canon-dir directory))
             trees)
         (dolist (entry (split-string output "\n\n" t))
           (when (string-match "^worktree \\(.+\\)$" entry)
             (let* ((raw-path (file-name-as-directory (match-string 1 entry)))
                    (canon-path (lispm-workspace--canon-dir raw-path)))
               (when (or (file-in-directory-p raw-path under)
                         (file-in-directory-p canon-path canon-under))
                 (push (list (file-name-nondirectory (directory-file-name raw-path))
                             raw-path
                             (and (string-match "^branch refs/heads/\\(.+\\)$" entry)
                                  (match-string 1 entry)))
                       trees)))))
         (funcall callback (nreverse trees) nil))))))

(defun lispm-workspace--cow-changes (workspace)
  "Compute `(ADDED MODIFIED DELETED)' relative paths for a `cow-clone' WORKSPACE."
  (let* ((base (plist-get (lispm-workspace-metadata workspace) :baseline))
         (current (lispm-workspace--scan-hashes (lispm-workspace-path workspace)))
         added modified deleted)
    (pcase-dolist (`(,rel . ,hash) current)
      (if-let* ((prev (assoc rel base)))
          (unless (equal (cdr prev) hash)
            (push rel modified))
        (push rel added)))
    (pcase-dolist (`(,rel . ,_hash) base)
      (unless (assoc rel current)
        (push rel deleted)))
    (list (nreverse added) (nreverse modified) (nreverse deleted))))

(defun lispm-workspace-state (workspace callback &optional base-ref)
  "Inspect changes in WORKSPACE against BASE-REF asynchronously.
Calls CALLBACK with `(STATE ERR)', where STATE is nil when clean."
  (let* ((ws (if (lispm-workspace-p workspace) workspace (lispm-workspace-get workspace)))
         (path (and ws (lispm-workspace-path ws)))
         (base (or base-ref (and ws (lispm-workspace-base-ref ws)) "HEAD"))
         (unsaved (and path (seq-filter #'buffer-modified-p (lispm-workspace-buffers path)))))
    (cond
     ((null ws)
      (funcall callback nil "Workspace not found"))
     ((eq (lispm-workspace-kind ws) 'cow-clone)
      (pcase-let ((`(,added ,modified ,deleted) (lispm-workspace--cow-changes ws)))
        (if (and (null added) (null modified) (null deleted) (null unsaved))
            (funcall callback nil nil)
          (funcall callback
                   (concat (when added (format "Added:\n%s\n" (mapconcat (lambda (f) (concat "A " f)) added "\n")))
                           (when modified (format "Modified:\n%s\n" (mapconcat (lambda (f) (concat "M " f)) modified "\n")))
                           (when deleted (format "Deleted:\n%s\n" (mapconcat (lambda (f) (concat "D " f)) deleted "\n")))
                           (when unsaved (format "Unsaved buffers: %s\n" (mapconcat #'buffer-name unsaved ", "))))
                   nil))))
     ((not (file-directory-p path))
      (funcall callback nil (format "Workspace directory missing: %s" (abbreviate-file-name path))))
     (t
      (lispm-workspace-git-output
       path (list "log" "--oneline" (concat base "..HEAD"))
       (lambda (commits err1)
         (if err1
             (funcall callback nil err1)
           (lispm-workspace-git-output
            path '("status" "--porcelain")
            (lambda (status err2)
              (if err2
                  (funcall callback nil err2)
                (if (and (string-empty-p commits) (string-empty-p status) (null unsaved))
                    (funcall callback nil nil)
                  (lispm-workspace-git-output
                   path (list "diff" "--stat" base)
                   (lambda (stat err3)
                     (if err3
                         (funcall callback nil err3)
                       (funcall
                        callback
                        (concat (unless (string-empty-p commits) (format "Commits:\n%s\n" commits))
                                (unless (string-empty-p status) (format "Uncommitted:\n%s\n" status))
                                (when unsaved
                                  (format "Unsaved buffers: %s\n"
                                          (mapconcat #'buffer-name unsaved ", ")))
                                (unless (string-empty-p stat)
                                  (format "Changes from %s:\n%s\n"
                                          (substring base 0 (min 7 (length base))) stat)))
                        nil)))))))))))))))

(defun lispm-workspace-diff (workspace callback &optional base-ref)
  "Compute patch diff of WORKSPACE against BASE-REF asynchronously.
Calls CALLBACK with `(DIFF ERR)'."
  (let* ((ws (if (lispm-workspace-p workspace) workspace (lispm-workspace-get workspace)))
         (base (or base-ref (and ws (lispm-workspace-base-ref ws)) "HEAD")))
    (cond
     ((null ws) (funcall callback nil "Workspace not found"))
     ((eq (lispm-workspace-kind ws) 'cow-clone)
      (pcase-let ((`(,added ,modified ,deleted) (lispm-workspace--cow-changes ws)))
        (funcall callback
                 (string-join (append (mapcar (lambda (f) (concat "A " f)) added)
                                      (mapcar (lambda (f) (concat "M " f)) modified)
                                      (mapcar (lambda (f) (concat "D " f)) deleted))
                              "\n")
                 nil)))
     (t
      (lispm-workspace-git-output (lispm-workspace-path ws)
                                  (list "diff" "--no-color" base)
                                  callback)))))

;;;; Removal & GC

(defun lispm-workspace--drop-buffers (path force)
  "Drop live buffers visiting files in PATH unless unsaved and FORCE is nil."
  (let ((buffers (lispm-workspace-buffers path)))
    (when (and (not force) (seq-some #'buffer-modified-p buffers))
      (error "Buffers visiting files in %s have unsaved changes" (abbreviate-file-name path)))
    (dolist (buffer buffers)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(defun lispm-workspace-remove-worktree (root path branch force callback)
  "Remove git worktree at PATH and optional BRANCH, running git in ROOT.
FORCE discards uncommitted changes and deletes unmerged branches.
CALLBACK gets `(REPORT ERRORP)'."
  (lispm-workspace--drop-buffers path force)
  (lispm-workspace-git-output
   root `("worktree" "remove" ,@(and force '("--force")) ,path)
   (lambda (_out err)
     (cond
      (err (funcall callback err t))
      ((null branch)
       (funcall callback (format "Removed the worktree %s." (abbreviate-file-name path)) nil))
      (t
       (lispm-workspace-git-output
        root (list "branch" (if force "-D" "-d") branch)
        (lambda (_out2 berr)
          (if (null berr)
              (funcall callback (format "Removed the worktree %s and its branch %s."
                                        (abbreviate-file-name path) branch)
                       nil)
            (funcall callback
                     (format "Removed the worktree %s; its branch %s stays, \
not being merged (force deletes it)." (abbreviate-file-name path) branch)
                     nil)))))))))

(defun lispm-workspace--cleanup-metadata (ws)
  "Remove isolated journal directory and registry entry for WS."
  (when ws
    (remhash (lispm-workspace-id ws) lispm-workspace--registry)
    (let ((store (lispm-workspace--store-file (lispm-workspace-id ws)))
          (jdir (lispm-workspace-journal-dir ws)))
      (when (file-exists-p store)
        (ignore-errors (delete-file store)))
      (when (and (stringp jdir)
                 (string-match-p "/workspaces/" jdir)
                 (file-exists-p jdir))
        (ignore-errors (delete-directory jdir t))))))

(cl-defun lispm-workspace-remove (workspace callback &key force)
  "Remove WORKSPACE (struct or id) cleanly; call CALLBACK with `(REPORT ERRORP)'."
  (let ((ws (if (lispm-workspace-p workspace) workspace (lispm-workspace-get workspace))))
    (cond
     ((null ws)
      (funcall callback "Workspace not found" t))
     ((eq (lispm-workspace-kind ws) 'git-worktree)
      (lispm-workspace-remove-worktree
       (lispm-workspace-origin ws)
       (lispm-workspace-path ws)
       (lispm-workspace-branch ws)
       force
       (lambda (report errorp)
         (unless errorp
           (lispm-workspace--cleanup-metadata ws))
         (funcall callback report errorp))))
     ((eq (lispm-workspace-kind ws) 'cow-clone)
      (let ((path (lispm-workspace-path ws)))
        (lispm-workspace--drop-buffers path force)
        (when (file-exists-p path)
          (delete-directory path t))
        (lispm-workspace--cleanup-metadata ws)
        (funcall callback (format "Removed the workspace %s." (abbreviate-file-name path)) nil)))
     (t
      (lispm-workspace--cleanup-metadata ws)
      (funcall callback "Removed shared workspace handle." nil)))))

(defun lispm-workspace-finalize-reply (workspace _agent-name reply-text callback)
  "Format post-reply worktree status for WORKSPACE appended to REPLY-TEXT.
Removes WORKSPACE if it left no changes, then passes final text to CALLBACK."
  (let* ((ws (if (lispm-workspace-p workspace) workspace (lispm-workspace-get workspace)))
         (path (and ws (lispm-workspace-path ws)))
         (branch (and ws (lispm-workspace-branch ws)))
         (base (and ws (or (lispm-workspace-base-ref ws) "HEAD")))
         (name (or (plist-get (and ws (lispm-workspace-metadata ws)) :name)
                   (and path (file-name-nondirectory (directory-file-name path)))
                   "workspace")))
    (if (null ws)
        (funcall callback reply-text)
      (lispm-workspace-state
       ws
       (lambda (state err)
         (cond
          (err (funcall callback (concat reply-text "\n\n" err)))
          (state
           (funcall callback
                    (format "%s\n\nIts work is in the worktree %s, on the branch %s, made from %s.\n%s\
Take it from there (the worktree skill says how); worktree_remove %s removes it."
                            reply-text
                            (abbreviate-file-name path)
                            (or branch "(detached)")
                            (substring base 0 (min 7 (length base)))
                            state name)))
          (t
           (lispm-workspace-remove
            ws
            (lambda (report &optional problem)
              (funcall callback
                       (concat reply-text "\n\n"
                               (if problem
                                   report
                                 "It changed nothing, so its worktree and branch are removed."))))
            :force t))))))))

(defun lispm-workspace-gc (origin callback &optional journal-dirs)
  "Prune stale workspace metadata in ORIGIN and GC JOURNAL-DIRS.
Runs asynchronously and passes `(PRUNED-COUNT ERR)' to CALLBACK."
  (lispm-workspace--load-all)
  (let ((pruned 0))
    (maphash (lambda (_id ws)
               (when (and (not (eq (lispm-workspace-kind ws) 'shared))
                          (lispm-workspace-path ws)
                          (not (file-exists-p (lispm-workspace-path ws))))
                 (cl-incf pruned)
                 (lispm-workspace--cleanup-metadata ws)))
             lispm-workspace--registry)
    (cl-labels
        ((gc-journals (dirs)
           (if (null dirs)
               (funcall callback pruned nil)
             (let ((jdir (car dirs)))
               (if (and (stringp jdir) (file-exists-p (expand-file-name "HEAD" jdir)))
                   (let ((process-environment (cons (concat "GIT_DIR=" (directory-file-name jdir))
                                                    (lispm-git-environment))))
                     (lispm-exec "git" '("gc" "--auto" "--quiet")
                                 (lambda (_exit _out _err)
                                   (gc-journals (cdr dirs)))))
                 (gc-journals (cdr dirs)))))))
      (if (and (stringp origin) (file-directory-p origin))
          (lispm-workspace-repository
           origin
           (lambda (repo _err)
             (if repo
                 (lispm-workspace-git-output (car repo) '("worktree" "prune")
                                             (lambda (_o _e) (gc-journals journal-dirs)))
               (gc-journals journal-dirs))))
        (gc-journals journal-dirs)))))

;;;; Lock-Free 3-Way Merge Reconciliation

(defun lispm-workspace--parse-merge-tree-conflict (output)
  "Parse `git merge-tree --write-tree --name-only --messages' conflict OUTPUT.
Return `(:tree TREE :conflicts LIST :messages STRING)'."
  (let* ((lines (split-string (or output "") "\n"))
         (tree (string-trim (or (pop lines) "")))
         conflicts)
    (while (and lines (not (string-empty-p (car lines))))
      (push (pop lines) conflicts))
    (when (and lines (string-empty-p (car lines)))
      (pop lines))
    (list :tree tree
          :conflicts (delete-dups (nreverse conflicts))
          :messages (string-trim (string-join lines "\n")))))

(defun lispm-workspace--read-raw-bytes (file)
  "Read raw unibyte contents of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun lispm-workspace--reconcile-cow (ws callback)
  "Reconcile `cow-clone' workspace WS into its origin atomically via SHA-256 CAS."
  (pcase-let* ((id (lispm-workspace-id ws))
               (origin (lispm-workspace-origin ws))
               (wspath (lispm-workspace-path ws))
               (base (plist-get (lispm-workspace-metadata ws) :baseline))
               (`(,added ,modified ,deleted) (lispm-workspace--cow-changes ws))
               (conflicts nil)
               (to-write nil))
    (if (and (null added) (null modified) (null deleted))
        (funcall callback (list :status 'noop :workspace id))
      ;; Verify every target file matches the workspace baseline before mutating origin.
      (dolist (rel added)
        (let ((orig-hash (lispm-file-sha256 (expand-file-name rel origin)))
              (ws-hash (lispm-file-sha256 (expand-file-name rel wspath))))
          (cond
           ((null orig-hash) (push (cons rel :missing) to-write))
           ((not (equal orig-hash ws-hash)) (push rel conflicts)))))
      (dolist (rel modified)
        (let ((expected (cdr (assoc rel base)))
              (actual (lispm-file-sha256 (expand-file-name rel origin))))
          (if (equal actual expected)
              (push (cons rel expected) to-write)
            (push rel conflicts))))
      (dolist (rel deleted)
        (let ((expected (cdr (assoc rel base)))
              (actual (lispm-file-sha256 (expand-file-name rel origin))))
          (unless (equal actual expected)
            (push rel conflicts))))
      (if conflicts
          (let ((sorted (sort (delete-dups conflicts) #'string<)))
            (funcall callback
                     (list :status 'conflict
                           :conflicts sorted
                           :messages (format "Origin changed concurrently on: %s"
                                             (string-join sorted ", "))
                           :workspace id)))
        (condition-case err
            (let ((applied nil))
              (pcase-dolist (`(,rel . ,expected) (nreverse to-write))
                (let* ((dst (expand-file-name rel origin))
                       (src (expand-file-name rel wspath))
                       (coding-system-for-write 'binary))
                  (lispm-write-file-atomic dst (lispm-workspace--read-raw-bytes src) expected)
                  (push rel applied)))
              (dolist (rel deleted)
                (let ((dst (expand-file-name rel origin)))
                  (when (file-exists-p dst)
                    (delete-file dst))
                  (push rel applied)))
              (funcall callback (list :status 'ok :files (nreverse applied) :workspace id)))
          (error
           (funcall callback (list :status 'conflict
                                   :conflicts (append added modified deleted)
                                   :messages (error-message-string err)
                                   :workspace id))))))))

(defun lispm-workspace--cas-update-ref (origin resolved-ref new-oid target-oid
                                              msg tree-oid source-oid fast-forward
                                              id attempts-left retry-fn callback)
  "Atomically advance RESOLVED-REF in ORIGIN from TARGET-OID to NEW-OID."
  (lispm-workspace-git-output
   origin (list "update-ref" "-m" msg resolved-ref new-oid target-oid)
   (lambda (_uout uerr)
     (cond
      ((null uerr)
       (funcall callback
                (list :status 'ok :commit new-oid :tree tree-oid
                      :target-ref resolved-ref :old-target target-oid
                      :source source-oid :fast-forward fast-forward
                      :workspace id)))
      ((> attempts-left 0)
       (funcall retry-fn (1- attempts-left)))
      (t
       (funcall callback
                (list :status 'error :cas-failed t
                      :error uerr :workspace id)))))))

(defun lispm-workspace--merge-tree (origin target-oid source-oid resolved-ref
                                           msg id attempts-left retry-fn callback)
  "Compute a lock-free 3-way merge of TARGET-OID and SOURCE-OID in ORIGIN."
  (lispm-workspace--git-raw
   origin (list "merge-tree" "--write-tree" "--name-only" "--messages"
                target-oid source-oid)
   (lambda (mt-exit mt-out mt-err)
     (cond
      ((eql mt-exit 0)
       (let ((tree-oid (car (split-string mt-out "\n" t))))
         (lispm-workspace-git-output
          origin (list "-c" "commit.gpgSign=false"
                       "-c" "user.name=lispm"
                       "-c" "user.email=lispm@lispm.invalid"
                       "commit-tree" "--no-gpg-sign" tree-oid
                       "-p" target-oid "-p" source-oid "-m" msg)
          (lambda (new-commit cerr)
            (if cerr
                (funcall callback (list :status 'error :error cerr :workspace id))
              (lispm-workspace--cas-update-ref
               origin resolved-ref new-commit target-oid msg tree-oid source-oid
               nil id attempts-left retry-fn callback))))))
      ((eql mt-exit 1)
       (let ((parsed (lispm-workspace--parse-merge-tree-conflict mt-out)))
         (funcall callback
                  (append (list :status 'conflict
                                :target-ref resolved-ref
                                :target-oid target-oid
                                :source source-oid
                                :workspace id)
                          parsed))))
      (t
       (funcall callback
                (list :status 'error
                      :error (string-trim (if (string-empty-p mt-err) mt-out mt-err))
                      :workspace id)))))))

(defun lispm-workspace--cas-reconcile (origin source-oid resolved-ref msg id
                                            attempts-left callback)
  "Run one CAS reconciliation round for SOURCE-OID onto RESOLVED-REF in ORIGIN."
  (let ((retry (lambda (next)
                 (lispm-workspace--cas-reconcile
                  origin source-oid resolved-ref msg id next callback))))
    (lispm-workspace-git-output
     origin (list "rev-parse" "--verify" (concat resolved-ref "^{commit}"))
     (lambda (target-oid ref-err)
       (if ref-err
           (funcall callback (list :status 'error :error ref-err :workspace id))
         (lispm-workspace--git-raw
          origin (list "merge-base" "--is-ancestor" source-oid target-oid)
          (lambda (anc-exit _out _err)
            (cond
             ((eql anc-exit 0)
              (funcall callback
                       (list :status 'noop :commit target-oid
                             :target-ref resolved-ref :source source-oid
                             :workspace id)))
             ((not (memq anc-exit '(0 1)))
              (funcall callback (list :status 'error
                                      :error "merge-base check failed"
                                      :workspace id)))
             (t
              (lispm-workspace--git-raw
               origin (list "merge-base" "--is-ancestor" target-oid source-oid)
               (lambda (ff-exit _out2 _err2)
                 (if (eql ff-exit 0)
                     (lispm-workspace--cas-update-ref
                      origin resolved-ref source-oid target-oid msg nil source-oid
                      t id attempts-left retry callback)
                   (lispm-workspace--merge-tree
                    origin target-oid source-oid resolved-ref msg id
                    attempts-left retry callback)))))))))))))

(defun lispm-workspace--prepare-source (wspath commit-uncommitted msg id callback)
  "Prepare WORKSPACE at WSPATH and pass `(SOURCE-OID ERR)' to CALLBACK.
COMMIT-UNCOMMITTED stages and commits dirty changes using MSG; ID identifies
the workspace for error context."
  (let ((read-head
         (lambda ()
           (lispm-workspace-git-output wspath '("rev-parse" "--verify" "HEAD^{commit}")
                                       callback))))
    (lispm-workspace-git-output
     wspath '("status" "--porcelain")
     (lambda (status st-err)
       (cond
        (st-err (funcall callback nil st-err))
        ((string-empty-p status) (funcall read-head))
        ((not commit-uncommitted)
         (funcall callback nil
                  "Workspace has uncommitted changes; commit them or pass :commit-uncommitted t"))
        (t
         (lispm-workspace-git-output
          wspath '("add" "-A")
          (lambda (_aout aerr)
            (if aerr
                (funcall callback nil aerr)
              (lispm-workspace-git-output
               wspath (list "-c" "commit.gpgSign=false"
                            "-c" "user.name=lispm"
                            "-c" "user.email=lispm@lispm.invalid"
                            "commit" "-q" "-m" (format "%s (auto-commit)" msg))
               (lambda (_cout cerr)
                 (if cerr
                     (funcall callback nil cerr)
                   (ignore id)
                   (funcall read-head)))))))))))))

(cl-defun lispm-workspace-reconcile (workspace callback &key target-ref message commit-uncommitted max-retries)
  "Reconcile WORKSPACE into TARGET-REF without locking origin.
Uses `git merge-tree --write-tree' and CAS `git update-ref' for
`git-worktree', or SHA-256 CAS writes for `cow-clone'.  MESSAGE sets
the commit message; COMMIT-UNCOMMITTED auto-commits dirty files in
WORKSPACE; MAX-RETRIES limits CAS retries.  Calls CALLBACK with a
result plist keyed by `:status' (`ok', `noop', `conflict', or `error')."
  (let* ((ws (if (lispm-workspace-p workspace) workspace (lispm-workspace-get workspace)))
         (max-retries (or max-retries 4)))
    (cond
     ((null ws)
      (funcall callback (list :status 'error :error "Workspace not found")))
     ((eq (lispm-workspace-kind ws) 'shared)
      (funcall callback (list :status 'noop :workspace (lispm-workspace-id ws))))
     ((eq (lispm-workspace-kind ws) 'cow-clone)
      (lispm-workspace--reconcile-cow ws callback))
     (t
      (let* ((id (lispm-workspace-id ws))
             (origin (lispm-workspace-origin ws))
             (wspath (lispm-workspace-path ws))
             (branch (lispm-workspace-branch ws))
             (msg (or message (format "Merge %s" (or branch id)))))
        (lispm-workspace--prepare-source
         wspath commit-uncommitted msg id
         (lambda (source-oid serr)
           (if serr
               (funcall callback (list :status 'error :error serr :workspace id))
             (if target-ref
                 (let ((resolved (if (string-prefix-p "refs/" target-ref)
                                     target-ref
                                   (concat "refs/heads/" target-ref))))
                   (lispm-workspace--cas-reconcile
                    origin source-oid resolved msg id max-retries callback))
               (lispm-workspace-git-output
                origin '("symbolic-ref" "-q" "HEAD")
                (lambda (head-ref herr)
                  (if herr
                      (funcall callback
                               (list :status 'error
                                     :error "Origin HEAD is detached; specify :target-ref"
                                     :workspace id))
                    (lispm-workspace--cas-reconcile
                     origin source-oid head-ref msg id max-retries callback)))))))))))))

(provide 'lispm-workspace)
;;; lispm-workspace.el ends here
