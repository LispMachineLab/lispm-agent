;;; lispm-agents-work.el --- Project views of live agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Project selection, scope and the native list belong to the agents.
;; Renderers may handle a project through a hook, and a host may choose
;; the window displaying the native list.  Neither is needed in Emacs.
;; Only the live roster is read: opening a project never resumes a session.

;;; Code:

(require 'tabulated-list)
(require 'lispm-kit-commands)
(require 'lispm-agents-roster)

(defvar lispm-agents-open-project-functions nil
  "Functions optionally displaying the live agents of a project.
Each receives an absolute DIRECTORY string and returns non-nil when it
handled the request.  The first success wins; otherwise
`lispm-agents-open-project' displays the native live list.  A renderer
uses `lispm-agents-work-filter' to share project membership semantics.")

(defvar lispm-agents-work-display-function #'pop-to-buffer-same-window
  "Function displaying a native work view, called with its BUFFER.
The default reuses the selected window.  A host may choose its own window here;
opening an actual conversation uses `lispm-agents-show-function'.")

(defvar-local lispm-agents-work-directory nil
  "Absolute selected directory of this work view, or nil outside one.")

(defvar-local lispm-agents-work-scope 'project
  "Scope of this work view: `project' or exact `directory'.")

(defvar-local lispm-agents-work--dirty t
  "Non-nil when this view needs the next visible roster refresh.")

(defvar lispm-agents-work--buffers nil
  "Native work views sharing one roster observer while any are open.")

(defun lispm-agents-work--directory (directory)
  "Normalize DIRECTORY without accessing session files or following symlinks."
  (directory-file-name (expand-file-name directory)))

(defun lispm-agents-work--project-key (root)
  "Return ROOT's case-folded grouping key, preserving the `chats' symbol."
  (if (stringp root) (downcase root) root))

(defun lispm-agents-work--project-roots (records directory)
  "Map each of RECORDS to its project root, also resolving DIRECTORY.
Use the same root resolver as the session folders.  Parent edges in this
snapshot make descendants join their root agent's project, even across
providers or directories.  Unknown parents stand alone.  Cut cycles in
snapshot order, just as the session tree does, so damaged metadata
cannot hide an entire group.  Starting agents need no invented key."
  (let* ((directories (delete-dups
                       (cons directory (delq nil (mapcar (lambda (record)
                                                          (plist-get record :root))
                                                        records)))))
         (roots (lispm-agents--resolve-roots directories))
         (by-key (make-hash-table :test #'equal))
         (parents (make-hash-table :test #'eq))
         (visited (make-hash-table :test #'eq))
         (resolved (make-hash-table :test #'eq)))
    (dolist (record records)
      (when-let* ((key (plist-get record :key)))
        (puthash key record by-key)))
    (dolist (record records)
      (when-let* ((parent (gethash (plist-get record :parent) by-key))
                  ((not (eq parent record))))
        (puthash record parent parents)))
    (dolist (record records)
      (let ((next record) path)
        (while (and next (not (gethash next visited)))
          (puthash next 'visiting visited)
          (push next path)
          (setq next (gethash next parents)))
        (when (eq (gethash next visited) 'visiting)
          (remhash next parents))
        (dolist (seen path) (puthash seen 'done visited))))
    ;; Walk and memoize iteratively: a long delegation chain must not
    ;; consume the evaluator's recursion allowance merely to draw a view.
    (dolist (record records)
      (let ((next record) path)
        (while (and next (eq (gethash next resolved 'unresolved) 'unresolved))
          (push next path)
          (setq next (gethash next parents)))
        (let ((root (if next (gethash next resolved)
                      (cdr (assoc (plist-get (car path) :root) roots)))))
          (dolist (seen path) (puthash seen root resolved)))))
    (cons (cdr (assoc directory roots)) resolved)))

(defun lispm-agents-work-filter (records directory &optional scope)
  "Return alive RECORDS belonging to DIRECTORY in SCOPE, in their input order.
RECORDS is an already available `lispm-agents-roster' snapshot; this
function never rereads it or scans a session index or transcript.
Default SCOPE `project' groups checkout subdirectories, linked worktrees
and known live descendants together.  Home and configured chat locations
share the Chats group.  SCOPE `directory' instead matches the exact
normalized working directory, without parent inheritance.  Paths fold
case, matching session folder grouping.  Idle, done and starting agents
are included; :running and a known session id are not requirements.
The result shares its records with RECORDS and does not mutate them."
  (let* ((directory (lispm-agents-work--directory directory))
         (scope (or scope 'project))
         (live (seq-filter (lambda (record) (plist-get record :alive)) records)))
    (pcase scope
      ('directory
       (seq-filter (lambda (record)
                     (when-let* ((root (plist-get record :root)))
                       (string-equal-ignore-case
                        directory (lispm-agents-work--directory root))))
                   live))
      ('project
       (pcase-let* ((`(,root . ,roots) (lispm-agents-work--project-roots live directory))
                    (key (lispm-agents-work--project-key root)))
         (seq-filter (lambda (record)
                       (equal key (lispm-agents-work--project-key (gethash record roots))))
                     live)))
      (_ (error "Unknown agent work scope: %S" scope)))))

(defun lispm-agents-work-snapshot (directory &optional scope)
  "Return the live roster in DIRECTORY and SCOPE.
See `lispm-agents-work-filter' for scope and membership.  Renderers that
already hold a roster snapshot should filter that snapshot directly."
  (lispm-agents-work-filter (lispm-agents-roster) directory scope))

(defun lispm-agents-work--state (record)
  "Return the translated status of RECORD, leaving unknown status empty."
  (pcase (plist-get (plist-get record :status) :state)
    ('working (lispm-tr "Working"))
    ('idle (lispm-tr "Idle"))
    ('done (lispm-tr "Done"))
    ('blocked (lispm-tr "Blocked"))
    ('error (lispm-tr "Error"))
    (_ "")))

(defun lispm-agents-work--activate (button)
  "Open the live agent on BUTTON's row after checking current membership."
  (goto-char (button-start button))
  (lispm-agents-work-open))

(defun lispm-agents-work--render (records)
  "Redraw the current native view from RECORDS, retaining the selected agent."
  (let ((selected (tabulated-list-get-id)))
    (setq tabulated-list-format
          (vector (list (lispm-tr "Name") 30 t)
                  (list (lispm-tr "State") 12 t)
                  (list (lispm-tr "Directory") 0 t)))
    (setq tabulated-list-entries
          (mapcar (lambda (record)
                    (let ((buffer (plist-get record :buffer)))
                      (list buffer
                            (vector (list (or (plist-get record :name) "")
                                          'follow-link t 'action #'lispm-agents-work--activate)
                                    (lispm-agents-work--state record)
                                    (if-let* ((root (plist-get record :root)))
                                        (abbreviate-file-name root) "")))))
                  (lispm-agents-work-filter records lispm-agents-work-directory
                                            lispm-agents-work-scope)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when selected
      (goto-char (point-min))
      (while (and (not (eobp)) (not (eq selected (tabulated-list-get-id))))
        (forward-line))
      (when (eobp) (goto-char (point-min))))
    (setq lispm-agents-work--dirty nil)))

(defun lispm-agents-work-refresh ()
  "Refresh this work view from live agents without opening any session."
  (interactive)
  (lispm-agents-work--render (lispm-agents-roster)))

(defun lispm-agents-work-open ()
  "Show the selected agent's existing conversation, if it is still active.
Recheck live membership before displaying a buffer; an archived or ended
session is never resumed by a stale row."
  (interactive)
  (let* ((buffer (tabulated-list-get-id))
         (records (lispm-agents-work-snapshot lispm-agents-work-directory
                                              lispm-agents-work-scope)))
    (if (and (buffer-live-p buffer)
             (seq-some (lambda (record) (eq buffer (plist-get record :buffer))) records))
        (lispm-agents-show buffer)
      (lispm-agents-work-refresh)
      (user-error "%s" (lispm-tr "No session here")))))

(defun lispm-agents-work--visible ()
  "Refresh dirty visible views from a single shared snapshot.
Hidden views remain dirty until displayed, without polling or reading
the roster in the meantime."
  (let (records read)
    (dolist (buffer lispm-agents-work--buffers)
      (when (and (buffer-live-p buffer) (get-buffer-window buffer 'visible)
                 (buffer-local-value 'lispm-agents-work--dirty buffer))
        (unless read
          (setq records (lispm-agents-roster) read t))
        (with-current-buffer buffer (lispm-agents-work--render records))))))

(defun lispm-agents-work--changed (&rest _change)
  "Mark views dirty after a coalesced roster change or a language change."
  (dolist (buffer lispm-agents-work--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lispm-agents-work--dirty t))))
  (lispm-agents-work--visible))

(defun lispm-agents-work--detach ()
  "Remove this view and stop shared observers after the last one closes."
  (setq lispm-agents-work--buffers (delq (current-buffer) lispm-agents-work--buffers))
  (unless lispm-agents-work--buffers
    (remove-hook 'lispm-agents-roster-change-functions #'lispm-agents-work--changed)
    (remove-hook 'lispm-language-changed-hook #'lispm-agents-work--changed)
    (remove-hook 'window-state-change-hook #'lispm-agents-work--visible)))

(define-derived-mode lispm-agents-work-mode tabulated-list-mode "Agents"
  "List already active agents in a project, in graphical or terminal Emacs.
The directory and scope are public buffer-local metadata.  All commands
and keys are declared through Kit so any host can present the same actions."
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-agents-work-refresh)))
  (add-hook 'kill-buffer-hook #'lispm-agents-work--detach nil t)
  (add-hook 'change-major-mode-hook #'lispm-agents-work--detach nil t)
  (cl-pushnew (current-buffer) lispm-agents-work--buffers)
  (add-hook 'lispm-agents-roster-change-functions #'lispm-agents-work--changed)
  (add-hook 'lispm-language-changed-hook #'lispm-agents-work--changed)
  (add-hook 'window-state-change-hook #'lispm-agents-work--visible))

(lispm-kit-define-commands lispm-agents-work-mode
  :menu "Agents"
  (lispm-agents-work-open :key "RET" :label "Open Conversation"
                          :help "Show the selected active conversation")
  (lispm-agents-work-refresh :key "g" :label "Refresh"
                             :help "Refresh the live agents in this project"))

(defun lispm-agents-work--native (directory)
  "Display and return the reusable native project view for DIRECTORY."
  (let ((buffer (or (seq-find
                     (lambda (candidate)
                       (and (buffer-live-p candidate)
                            (equal directory (buffer-local-value
                                              'lispm-agents-work-directory candidate))))
                     lispm-agents-work--buffers)
                    (generate-new-buffer
                     (format "*%s: %s*" (lispm-tr "Agents")
                             (abbreviate-file-name directory))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-agents-work-mode) (lispm-agents-work-mode))
      (setq lispm-agents-work-directory directory
            lispm-agents-work-scope 'project)
      (when (file-directory-p directory)
        (setq default-directory (file-name-as-directory directory)))
      (lispm-agents-work-refresh))
    (funcall lispm-agents-work-display-function buffer)
    buffer))

;;;###autoload
(defun lispm-agents-open-project (directory)
  "Show already active agents belonging to DIRECTORY's project.
Optional renderers in `lispm-agents-open-project-functions' are tried
first.  With no renderer, show the native live list in plain Emacs.
The request only selects a view; it never launches or resumes agents."
  (interactive (list (read-directory-name (lispm-tr "Project: "))))
  (let ((directory (lispm-agents-work--directory directory)))
    (or (run-hook-with-args-until-success 'lispm-agents-open-project-functions directory)
        (lispm-agents-work--native directory))))

(defun lispm-agents-work-unload-function ()
  "Remove native views and their observers when unloading this feature."
  (dolist (buffer (copy-sequence lispm-agents-work--buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (lispm-agents-work--detach)
        (fundamental-mode))))
  nil)

(provide 'lispm-agents-work)
;;; lispm-agents-work.el ends here
