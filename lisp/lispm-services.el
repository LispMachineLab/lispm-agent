;;; lispm-services.el --- Background services packages offer, in the sandbox  -*- lexical-binding: t; -*-

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

;; A service is a program running in the background while Emacs runs,
;; for a package that needs it, as Holon's daemon is for lispm-holon.
;; The package defines it with `lispm-define-service': its name, the
;; program and where it runs, and what it needs of the sandbox on top of
;; a level's rules, its own files, sockets and domains.  Whoever needs
;; the service then starts it with `lispm-service-start', the package
;; itself or a list of what starts with Emacs, as a desktop's startup
;; items, and asks after it with `lispm-service-status'.
;;
;; A service runs in the sandbox (lispm-sandbox.el), at the level
;; `lispm-service-levels' gives it, else at `lispm-sandbox-level' as
;; agents' commands do, and at every level but none with the rules it
;; needs on top.  A service given a level of its own there is one you
;; have LispM run: a package that could start its program another way,
;; outside the sandbox, starts the service instead (see
;; `lispm-service-managed-p').  Its output goes to a buffer of its own,
;; and it stops as Emacs exits.
;;
;; M-x lispm-service-start, lispm-service-stop and lispm-service-restart
;; act on one; `lispm-services-changed-hook' runs as services are
;; defined, start and stop, for whatever shows them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-sandbox)

(defvar lispm-services nil
  "The services packages offer, as (ID . PROPERTIES).
See `lispm-define-service'.")

(defvar lispm-services-changed-hook nil
  "Hook run when a service is defined or undefined, starts or stops, or
is given another level.")

(defcustom lispm-service-levels nil
  "The sandbox level of each service LispM runs, as (ID . LEVEL).
LEVEL is a level of `lispm-sandbox-levels'.  A service listed here is
one LispM runs: a package that could start its program another way,
outside the sandbox, starts the service instead.  A service not listed
runs at `lispm-sandbox-level', as agents' commands do, when it runs.
Set it with `lispm-service-set-level'."
  :type '(alist :key-type (symbol :tag "Service")
                :value-type (choice (const none) (const low) (const middle)
                                    (const high) (const custom)))
  :group 'lispm-sandbox)

(defconst lispm-service-stop-seconds 5
  "Seconds a service has to stop when asked, before it is killed.")

(defvar lispm-service--processes nil
  "The processes of the services running, as (ID . PROCESS).")

(defvar lispm-service--ended nil
  "How each service that ran last ended, as (ID . (exited . CODE)), or
(ID . (failed . WHY)) for one that could not start.")

;;;; Defining

(defun lispm-define-service (id &rest properties)
  "Make ID, a symbol, a service, as PROPERTIES say:

  :name          its name, \"Holon\"
  :description   what it does, a sentence
  :command       the program and its arguments, a list, or a function of
                 no argument returning one
  :directory     where it runs, a directory or a function of no argument
                 returning one, by default the home directory; relative
                 file names in the sandbox's rules are relative to it
  :environment   a function of no argument returning entries
                 \"VARIABLE=VALUE\" for the program's environment
  :sandbox       a function of no argument returning the rules the
                 program needs on top of its level's, a plist as
                 `lispm-sandbox-custom' describes
  :before-start  a function of no argument called before it starts, as
                 one stopping the program where it runs another way

Defining ID again replaces what it was; a service running goes on.
Return ID."
  (setf (alist-get id lispm-services) properties)
  (run-hooks 'lispm-services-changed-hook)
  id)

(defun lispm-undefine-service (id)
  "Make ID a service no more, stopping it if it runs."
  (when (assq id lispm-services)
    (lispm-service-stop id)
    (setq lispm-services (assq-delete-all id lispm-services))
    (run-hooks 'lispm-services-changed-hook)))

(defun lispm-service-props (id)
  "Return the properties of the service ID; signal when there is none."
  (or (alist-get id lispm-services)
      (user-error (lispm-tr "No service %s") id)))

(defun lispm-service-name (id)
  "Return the name of the service ID."
  (or (plist-get (alist-get id lispm-services) :name) (symbol-name id)))

(defun lispm-service-description (id)
  "Return what the service ID does, or nil."
  (plist-get (alist-get id lispm-services) :description))

(defun lispm-service--value (value)
  "Return VALUE, or what it returns when it is a function."
  (if (functionp value) (funcall value) value))

;;;; Levels

(defun lispm-service-level (id)
  "Return the sandbox level the service ID runs at: its own, see
`lispm-service-levels', else `lispm-sandbox-level'."
  (or (alist-get id lispm-service-levels) lispm-sandbox-level))

(defun lispm-service-managed-p (id)
  "Return non-nil when the service ID is one LispM runs, having a level
of its own, see `lispm-service-levels'."
  (and (assq id lispm-service-levels) t))

(defun lispm-service-set-level (id level &optional save)
  "Have LispM run the service ID at the sandbox LEVEL, or, LEVEL nil, run
it no more as its own.  SAVE keeps it for later starts.  A running
service starts again, as the rules of files hold from a start."
  (unless (or (null level) (assq level lispm-sandbox-levels))
    (user-error (lispm-tr "No sandbox level %s: it is one of %s")
                level (mapconcat (pcase-lambda (`(,level . ,_)) (lispm-sandbox-level-name level))
                                 lispm-sandbox-levels ", ")))
  (let ((levels (assq-delete-all id (copy-alist lispm-service-levels))))
    (when level
      (setq levels (append levels (list (cons id level)))))
    (if save
        (customize-save-variable 'lispm-service-levels levels)
      (customize-set-variable 'lispm-service-levels levels)))
  (when (lispm-service-process id)
    (lispm-service-restart id))
  (run-hooks 'lispm-services-changed-hook))

;;;; Running

(defun lispm-service-buffer-name (id)
  "Return the name of the buffer holding the output of the service ID."
  (format "*Service: %s*" (lispm-service-name id)))

(defun lispm-service-process (id)
  "Return the process of the service ID while it runs, else nil."
  (let ((process (alist-get id lispm-service--processes)))
    (and (process-live-p process) process)))

(defun lispm-service-status (id)
  "Return how the service ID stands: `running'; (exited . CODE) once it
stopped; (failed . WHY) when it could not start; nil before it ran."
  (if (lispm-service-process id)
      'running
    (alist-get id lispm-service--ended)))

(defun lispm-service--note (id text)
  "Add TEXT, a line, to the output buffer of the service ID; return it."
  (let ((buffer (get-buffer-create (lispm-service-buffer-name id))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (concat (format-time-string "%F %T ") text "\n") 'face 'shadow))))
    buffer))

(defun lispm-service--sentinel (id process)
  "Note that PROCESS, that of the service ID, stopped, and how."
  (unless (process-live-p process)
    (when (eq (alist-get id lispm-service--processes) process)
      (setf (alist-get id lispm-service--processes nil t) nil
            (alist-get id lispm-service--ended) (cons 'exited (process-exit-status process))))
    (when (buffer-live-p (process-buffer process))
      (lispm-service--note id (format "stopped, exit code %d" (process-exit-status process))))
    (run-hooks 'lispm-services-changed-hook)))

(defun lispm-service--start (id props)
  "Start the service ID, of PROPS; return its process."
  (when-let* ((before (plist-get props :before-start)))
    (funcall before))
  (let* ((name (lispm-service-name id))
         (level (alist-get id lispm-service-levels))
         (directory (file-name-as-directory
                     (expand-file-name (or (lispm-service--value (plist-get props :directory)) "~"))))
         (default-directory directory)
         (command (lispm-service--value (plist-get props :command)))
         (process-environment (append (lispm-service--value (plist-get props :environment))
                                      process-environment))
         (run (lispm-sandbox-prepare
               command level name
               ;; Its rules matter only in the sandbox.
               (unless (eq (or level lispm-sandbox-level) 'none)
                 (lispm-service--value (plist-get props :sandbox)))))
         (buffer (lispm-service--note
                  id (concat (combine-and-quote-strings command)
                             (if-let* ((note (lispm-sandbox-run-note run))) (format " (%s)" note) ""))))
         (process (make-process :name name :buffer buffer
                                :command (lispm-sandbox-run-command run)
                                :connection-type 'pipe :noquery t
                                :sentinel (lambda (process _event)
                                            (lispm-service--sentinel id process)))))
    (with-current-buffer buffer
      (setq default-directory directory))
    (lispm-sandbox-attach run process)
    (setf (alist-get id lispm-service--processes) process
          (alist-get id lispm-service--ended nil t) nil)
    process))

(defun lispm-service--read ()
  "Read the name of a service, and return its id."
  (let ((names (or (mapcar (lambda (entry) (cons (lispm-service-name (car entry)) (car entry)))
                           lispm-services)
                   (user-error (lispm-tr "No package offers a service")))))
    (cdr (assoc (completing-read (lispm-tr "Service: ") names nil t) names))))

;;;###autoload
(defun lispm-service-start (id &optional show)
  "Start the service ID unless it runs, in the sandbox at its level, see
`lispm-service-level'; return its process.  SHOW, as interactively,
shows its output.  Signal why it cannot start, as when the sandbox
cannot be had: it never runs outside it then."
  (interactive (list (lispm-service--read) t))
  (prog1 (or (lispm-service-process id)
             (let ((props (lispm-service-props id)))
               (condition-case err
                   (prog1 (lispm-service--start id props)
                     (run-hooks 'lispm-services-changed-hook))
                 (error
                  (setf (alist-get id lispm-service--ended) (cons 'failed (error-message-string err)))
                  (lispm-service--note id (format "could not start: %s" (error-message-string err)))
                  (run-hooks 'lispm-services-changed-hook)
                  (signal (car err) (cdr err))))))
    (when show
      (display-buffer (lispm-service-buffer-name id)))))

;;;###autoload
(defun lispm-service-stop (id)
  "Stop the service ID if it runs: ask it to, and kill it after
`lispm-service-stop-seconds'.  Return non-nil if it ran."
  (interactive (list (lispm-service--read)))
  (when-let* ((process (lispm-service-process id)))
    (signal-process process 'TERM)
    (let ((deadline (+ (float-time) lispm-service-stop-seconds)))
      (while (and (process-live-p process) (< (float-time) deadline))
        (accept-process-output process 0.1)))
    (when (process-live-p process)
      (delete-process process))
    t))

;;;###autoload
(defun lispm-service-restart (id &optional show)
  "Stop the service ID if it runs, and start it again; return its process.
SHOW, as interactively, shows its output."
  (interactive (list (lispm-service--read) t))
  (lispm-service-stop id)
  (lispm-service-start id show))

(provide 'lispm-services)
;;; lispm-services.el ends here
