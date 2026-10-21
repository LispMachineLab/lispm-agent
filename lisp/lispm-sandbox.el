;;; lispm-sandbox.el --- Agents' processes in an operating-system sandbox  -*- lexical-binding: t; -*-

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

;; Agents' shell commands (run_shell) and MCP servers can run in
;; Anthropic's sandbox runtime, srt
;; (https://github.com/anthropics/sandbox-runtime).  The operating
;; system then enforces what they may read, write and reach on the
;; network (sandbox-exec on macOS, bubblewrap on Linux), for them and
;; for every process they start.  Emacs itself stays outside: it holds
;; the configuration, and turns `lispm-sandbox-level' into srt's
;; settings whenever it starts one of those processes.
;;
;; There are four levels, from none to the strictest:
;;
;;   none    no sandbox, the default: commands may do anything you can;
;;   low     read everything but secrets (`lispm-sandbox-secrets'); write
;;           the project, temporary files and package caches; reach
;;           package registries, code hosts and the domains of
;;           `lispm-sandbox-allowed-domains';
;;   middle  read everything but secrets; write the project and temporary
;;           files; reach only `lispm-sandbox-allowed-domains';
;;   high    read only the project, git's configuration and the system;
;;           write only the project; no network.
;;
;; Choose one with M-x lispm-sandbox-set-level, or /sandbox in a chat.
;; Processes started afterwards run at the new level.  Running ones
;; get its network rules at once, through srt's --control-fd; their
;; file rules were fixed when they started, so they keep those until
;; they are started again.  An MCP server's :sandbox gives it a level
;; of its own (see `lispm-mcp-servers').
;;
;; The first time a level other than none is used, LispM installs
;; version `lispm-sandbox-srt-version' of srt with npm (Node.js 22.12 or
;; later) into the sandbox/ directory of `lispm-directory'; set
;; `lispm-sandbox-srt-program' to run an srt of your own instead.  On
;; Linux srt also needs bubblewrap, socat and ripgrep.  Until srt is
;; there, commands that should be sandboxed fail rather than run
;; outside it.
;;
;; Emacs Lisp is not confined.  eval_elisp (permission class `exec')
;; can do whatever Emacs can, change the level included, so keep that
;; class at ask or deny when the sandbox matters.  Sandboxed commands
;; may never write where Emacs and LispM keep their configuration and
;; records (`lispm-sandbox--protected'), but a project they may write
;; can hold files Emacs loads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-core)

(defgroup lispm-sandbox nil
  "Agents' shell commands and MCP servers in an operating-system sandbox."
  :group 'lispm
  :prefix "lispm-sandbox-")

;; A kind of file error, so that its message reads as a sentence: the
;; reason follows unquoted (see `lispm-sandbox--fail').
(define-error 'lispm-sandbox-error "Sandbox unavailable" 'file-error)

(defun lispm-sandbox--fail (why)
  "Signal that the sandbox cannot be had, because of WHY."
  (signal 'lispm-sandbox-error (list "Sandbox unavailable" why)))

(defconst lispm-sandbox-levels
  '((none . "no sandbox")
    (low . "reads all but secrets; writes the project, temporary files and package caches; reaches package registries, code hosts and allowed domains")
    (middle . "reads all but secrets; writes the project and temporary files; reaches only allowed domains")
    (high . "reads only the project and the system; writes only the project; no network"))
  "The sandbox levels, from none to the strictest, with what each allows.")

(defcustom lispm-sandbox-level 'none
  "How strictly agents' shell commands and MCP servers are confined.
`none', the default, runs them as Emacs runs anything: they may do
whatever you can.  `low', `middle' and `high' run them in srt, each
stricter than the one before (see `lispm-sandbox-levels').  Set it
with `lispm-sandbox-set-level' or /sandbox in a chat; running
processes take a new level's network rules at once."
  :type '(choice (const :tag "None: no sandbox" none)
                 (const :tag "Low" low)
                 (const :tag "Middle" middle)
                 (const :tag "High" high)))

(defcustom lispm-sandbox-secrets
  '("~/.ssh" "~/.gnupg" "~/.aws" "~/.azure" "~/.kube" "~/.docker" "~/.config/gcloud"
    "~/.config/gh" "~/.password-store" "~/.netrc" "~/.authinfo" "~/.authinfo.gpg"
    "~/.git-credentials" "~/.npmrc" "~/.pypirc" "~/.cargo/credentials.toml"
    "~/Library/Keychains")
  "Files and directories sandboxed commands may not read, at levels low and middle.
Entries are srt's: file names, or globs such as **/.env."
  :type '(repeat string))

(defcustom lispm-sandbox-caches
  '("~/.npm" "~/.cache" "~/Library/Caches" "~/.cargo/registry" "~/.cargo/git"
    "~/go/pkg/mod" "~/.m2/repository" "~/.gradle/caches")
  "Package caches sandboxed commands may write at level low."
  :type '(repeat string))

(defcustom lispm-sandbox-registries
  '("github.com" "*.github.com" "*.githubusercontent.com" "gitlab.com" "codeberg.org"
    "bitbucket.org" "registry.npmjs.org" "registry.yarnpkg.com" "pypi.org"
    "files.pythonhosted.org" "crates.io" "*.crates.io" "static.rust-lang.org"
    "proxy.golang.org" "sum.golang.org" "rubygems.org" "repo.maven.apache.org"
    "elpa.gnu.org" "elpa.nongnu.org" "melpa.org")
  "Package registries and code hosts sandboxed commands may reach at level low.
Entries are srt's: example.com, *.example.com (its subdomains) or
example.com:443."
  :type '(repeat string))

(defcustom lispm-sandbox-allowed-domains nil
  "Domains sandboxed commands may reach at levels low and middle.
At level low they come on top of `lispm-sandbox-registries'; at
middle they are all there is.  Entries are as for the registries."
  :type '(repeat string))

(defcustom lispm-sandbox-allow-write nil
  "Further places sandboxed commands may write at levels low and middle."
  :type '(repeat string))

(defcustom lispm-sandbox-srt-program nil
  "The srt program to run, or nil to run the one LispM installs.
LispM installs version `lispm-sandbox-srt-version' (see
`lispm-sandbox-install')."
  :type '(choice (const :tag "Installed by LispM" nil) file))

(defconst lispm-sandbox-srt-version "0.0.81"
  "The version of srt, npm's @anthropic-ai/sandbox-runtime, that LispM installs.")

(defconst lispm-sandbox--package "@anthropic-ai/sandbox-runtime"
  "The npm package of srt.")

(defconst lispm-sandbox--retry-after 300
  "Seconds before an installation of srt that failed is tried again on its own.")

(defvar lispm-sandbox-change-hook nil
  "Normal hook run after the sandbox level, or what a level allows, changed.")

;;;; Places

(defun lispm-sandbox--directory (&optional name)
  "The sandbox directory in `lispm-directory', or its subdirectory NAME.
It is made if need be, readable by you alone."
  (let ((dir (file-name-as-directory
              (expand-file-name (concat "sandbox/" (or name "")) lispm-directory))))
    (unless (file-directory-p dir)
      (with-file-modes #o700 (make-directory dir t)))
    dir))

(defun lispm-sandbox--tmpdir ()
  "The temporary directory of sandboxed commands, writable at every level."
  (let ((dir (file-name-as-directory
              (expand-file-name (format "lispm-sandbox-%d" (user-uid)) temporary-file-directory))))
    (unless (file-directory-p dir)
      (with-file-modes #o700 (make-directory dir t)))
    (file-truename dir)))

(defun lispm-sandbox--temporary ()
  "The directories of temporary files, as given and as they really are."
  (delete-dups (list "/tmp" (file-truename "/tmp")
                     (directory-file-name temporary-file-directory)
                     (directory-file-name (file-truename temporary-file-directory)))))

(defun lispm-sandbox--protected ()
  "What sandboxed commands may never write.
Emacs's configuration and LispM's records, srt among them: a command
that changed them would get past the sandbox the next time Emacs
loads them or runs srt."
  (delete-dups
   (mapcar (lambda (file) (directory-file-name (expand-file-name file)))
           (delq nil (list user-emacs-directory lispm-directory custom-file
                           "~/.emacs" "~/.emacs.el" "~/.config/emacs")))))

;;;; Settings

(defun lispm-sandbox-level-name (level)
  "LEVEL as the user sees it: None, Low, Middle or High."
  (capitalize (symbol-name level)))

(defun lispm-sandbox-settings (&optional level)
  "srt's settings for LEVEL, by default `lispm-sandbox-level'.
A plist for `json-serialize', or nil for `none'.  Relative file
names are relative to a sandboxed command's directory: its project."
  (let ((level (or level lispm-sandbox-level)))
    (unless (assq level lispm-sandbox-levels)
      (lispm-sandbox--fail (format "no sandbox level %s" level)))
    (unless (eq level 'none)
      (pcase-let* ((tmp (lispm-sandbox--tmpdir))
                   (`(,deny-read ,allow-read ,allow-write ,domains)
                    (pcase level
                      ('low (list lispm-sandbox-secrets nil
                                  (append '(".") (lispm-sandbox--temporary)
                                          lispm-sandbox-caches lispm-sandbox-allow-write)
                                  (append lispm-sandbox-registries lispm-sandbox-allowed-domains)))
                      ('middle (list lispm-sandbox-secrets nil
                                     (append '(".") (lispm-sandbox--temporary) lispm-sandbox-allow-write)
                                     lispm-sandbox-allowed-domains))
                      ('high (list '("~") '("." "~/.gitconfig" "~/.config/git") '(".") nil)))))
        `(:network (:allowedDomains ,(vconcat (delete-dups (copy-sequence domains)))
                    :deniedDomains [])
          :filesystem (:denyRead ,(vconcat deny-read)
                       :allowRead ,(vconcat allow-read)
                       :allowWrite ,(vconcat (delete-dups (append allow-write (list tmp))))
                       :denyWrite ,(vconcat (lispm-sandbox--protected))))))))

(defun lispm-sandbox--settings-file (json)
  "A file holding the settings JSON, named after what it holds."
  (let* ((dir (lispm-sandbox--directory "settings"))
         (file (expand-file-name (concat (secure-hash 'sha1 json) ".json") dir)))
    (unless (file-exists-p file)
      (let ((new (make-temp-file (expand-file-name "new-" dir)))
            (coding-system-for-write 'no-conversion))
        (write-region json nil new nil 'silent)
        (rename-file new file t)))
    file))

;;;; srt

(defvar lispm-sandbox--installer nil
  "The npm process installing srt, while it runs.")

(defvar lispm-sandbox--install-failure nil
  "Why srt could not be installed, and when: (TIME . WHY), or nil.")

(defun lispm-sandbox--package-file (file)
  "FILE of the srt package LispM installs."
  (expand-file-name (concat "node_modules/" lispm-sandbox--package "/" file)
                    (lispm-sandbox--directory "srt")))

(defun lispm-sandbox--installed ()
  "The version of the srt LispM installed, or nil."
  (let ((file (lispm-sandbox--package-file "package.json")))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (alist-get 'version (ignore-errors (json-parse-buffer :object-type 'alist)))))))

(defun lispm-sandbox--srt ()
  "The command that runs srt, a list, or nil while there is none."
  (if lispm-sandbox-srt-program
      (when-let* ((program (executable-find lispm-sandbox-srt-program)))
        (list program))
    (when-let* (((equal (lispm-sandbox--installed) lispm-sandbox-srt-version))
                (node (executable-find "node")))
      (list node (lispm-sandbox--package-file "dist/cli.js")))))

(defun lispm-sandbox--install-failed (why)
  "Note and say that srt could not be installed, because of WHY."
  (setq lispm-sandbox--install-failure (cons (float-time) why))
  (message "LispM: could not install srt for the sandbox: %s" why))

;;;###autoload
(defun lispm-sandbox-install ()
  "Install the srt that LispM's sandbox runs, in the background.
Version `lispm-sandbox-srt-version' comes from npm (Node.js 22.12 or
later) into the sandbox directory of `lispm-directory', which
sandboxed commands cannot change.  This happens on its own the first
time a level other than none is used."
  (interactive)
  (unless (process-live-p lispm-sandbox--installer)
    (setq lispm-sandbox--install-failure nil)
    (if-let* ((npm (executable-find "npm")))
        (let ((buffer (get-buffer-create " *lispm-sandbox-install*")))
          (with-current-buffer buffer (erase-buffer))
          (message "LispM: installing srt %s for the sandbox..." lispm-sandbox-srt-version)
          (setq lispm-sandbox--installer
                (make-process
                 :name "lispm-sandbox-install" :buffer buffer :noquery t
                 :connection-type 'pipe
                 :command (list npm "install" "--prefix" (lispm-sandbox--directory "srt")
                                "--no-fund" "--no-audit" "--loglevel=error"
                                (format "%s@%s" lispm-sandbox--package lispm-sandbox-srt-version))
                 :sentinel
                 (lambda (process _event)
                   (unless (process-live-p process)
                     (cond ((lispm-sandbox--srt)
                            (message "LispM: the sandbox is ready (srt %s)" lispm-sandbox-srt-version))
                           ((zerop (process-exit-status process))
                            (lispm-sandbox--install-failed "node, from Node.js 22.12 or later, is not found"))
                           (t (lispm-sandbox--install-failed
                               (with-current-buffer (process-buffer process)
                                 (string-trim (buffer-substring
                                               (max (point-min) (- (point-max) 300))
                                               (point-max))))))))))))
      (lispm-sandbox--install-failed "it needs npm, from Node.js 22.12 or later"))))

(defun lispm-sandbox--ensure (&optional level)
  "Start installing srt if LEVEL needs it and it is not there.
LEVEL defaults to `lispm-sandbox-level'."
  (unless (or (eq (or level lispm-sandbox-level) 'none) lispm-sandbox-srt-program
              (process-live-p lispm-sandbox--installer)
              (and lispm-sandbox--install-failure
                   (< (- (float-time) (car lispm-sandbox--install-failure))
                      lispm-sandbox--retry-after))
              (lispm-sandbox--srt))
    (lispm-sandbox-install)))

(defun lispm-sandbox--ready ()
  "The command that runs srt; or signal why it cannot be run now."
  (or (lispm-sandbox--srt)
      (progn
        (lispm-sandbox--ensure 'low)
        (lispm-sandbox--fail
         (cond (lispm-sandbox-srt-program
                (format "the srt program %s is not found" lispm-sandbox-srt-program))
               ((process-live-p lispm-sandbox--installer)
                "srt is being installed; try again in a minute")
               (t (format "srt could not be installed: %s"
                          (cdr lispm-sandbox--install-failure))))))))

;;;; Processes

(cl-defstruct (lispm-sandbox-run (:constructor lispm-sandbox--make-run)
                                 (:copier nil))
  "A process started by way of the sandbox, confined or not."
  (name nil :documentation "What runs, for messages.")
  (level nil :documentation "Its own level, or nil to follow `lispm-sandbox-level'.")
  (started nil :documentation "The level it started at.")
  (settings nil :documentation "srt's settings it started with; nil if not confined.")
  (command nil :documentation "The program and arguments that start it.")
  (fifo nil :documentation "The named pipe of its control channel.")
  (writer nil :documentation "The process writing to that pipe.")
  (process nil :documentation "The process."))

(defvar lispm-sandbox--runs nil
  "The runs whose processes are alive.")

(defun lispm-sandbox--sweep ()
  "Delete named pipes left behind, an hour old, of runs no longer alive."
  (let ((live (delq nil (mapcar #'lispm-sandbox-run-fifo lispm-sandbox--runs)))
        (old (- (float-time) 3600)))
    (dolist (file (directory-files (lispm-sandbox--directory "run") t "\\`control-"))
      (when-let* (((not (member file live)))
                  (attributes (file-attributes file))
                  ((< (float-time (file-attribute-modification-time attributes)) old)))
        (ignore-errors (delete-file file))))))

(defun lispm-sandbox--fifo ()
  "A new named pipe, for a process's control channel."
  (lispm-sandbox--sweep)
  (let ((fifo (make-temp-name (expand-file-name "control-" (lispm-sandbox--directory "run")))))
    (unless (eql 0 (ignore-errors (call-process "mkfifo" nil nil nil "-m" "600" fifo)))
      (lispm-sandbox--fail (format "could not make the named pipe %s" fifo)))
    fifo))

(defun lispm-sandbox-prepare (command &optional level name)
  "Get COMMAND, a program and its arguments, ready to run in the sandbox.
LEVEL is the level to run at; nil follows `lispm-sandbox-level', now
and as it changes.  NAME says what runs, for messages.  Return a run:
start the process with its `lispm-sandbox-run-command', then give it
to `lispm-sandbox-attach'.  When the sandbox cannot be had, signal
`lispm-sandbox-error' rather than let COMMAND run outside it."
  (let* ((started (or level lispm-sandbox-level))
         (settings (lispm-sandbox-settings started))
         (run (lispm-sandbox--make-run :name name :level level :started started
                                       :settings settings :command command)))
    (when settings
      (when (file-remote-p default-directory)
        (lispm-sandbox--fail "it confines processes of this machine, and the directory is remote"))
      (let ((srt (lispm-sandbox--ready))
            (file (lispm-sandbox--settings-file (json-serialize settings)))
            (fifo (lispm-sandbox--fifo)))
        (setf (lispm-sandbox-run-fifo run) fifo
              ;; srt reads level changes from descriptor 3, the pipe's
              ;; reading end.  Its temporary directory is ours.
              (lispm-sandbox-run-command run)
              (append (list "/bin/sh" "-c"
                            (format "CLAUDE_CODE_TMPDIR=%s; export CLAUDE_CODE_TMPDIR; exec \"$@\" 3<%s"
                                    (shell-quote-argument (lispm-sandbox--tmpdir))
                                    (shell-quote-argument fifo))
                            "lispm-sandbox")
                      srt
                      (list "--settings" file "--control-fd" "3" "--")
                      command))))
    run))

(defun lispm-sandbox-attach (run process)
  "Note that PROCESS was started for RUN, made by `lispm-sandbox-prepare'.
Open its control channel, through which changes of level reach it,
and forget it once PROCESS has exited."
  (setf (lispm-sandbox-run-process run) process)
  (when-let* ((fifo (lispm-sandbox-run-fifo run)))
    (setf (lispm-sandbox-run-writer run)
          (make-process :name "lispm-sandbox-control" :buffer nil :noquery t
                        :connection-type 'pipe :coding 'binary
                        :command (list "/bin/sh" "-c" (concat "exec cat > " (shell-quote-argument fifo)))
                        :sentinel #'ignore)))
  (push run lispm-sandbox--runs)
  (add-function :after (process-sentinel process)
                (lambda (process _event)
                  (unless (process-live-p process)
                    (lispm-sandbox--forget run))))
  run)

(defun lispm-sandbox--forget (run)
  "Close RUN's control channel, its process having exited."
  (setq lispm-sandbox--runs (delq run lispm-sandbox--runs))
  (when-let* ((writer (lispm-sandbox-run-writer run)))
    (delete-process writer))
  (when-let* ((fifo (lispm-sandbox-run-fifo run)))
    (ignore-errors (delete-file fifo))))

(defun lispm-sandbox-run-note (run)
  "What RUN's sandbox allowed, for an agent reading its output; nil if none."
  (when (lispm-sandbox-run-settings run)
    (let ((level (lispm-sandbox-run-started run)))
      (format "sandbox %s: %s" (lispm-sandbox-level-name level)
              (alist-get level lispm-sandbox-levels)))))

;;;; Changes

(defvar lispm-sandbox--change-timer nil
  "Timer bringing running processes up to a change, once it is made.")

(defun lispm-sandbox--send (run json)
  "Give RUN's process srt's settings JSON; non-nil if it has a channel for them."
  (let ((writer (lispm-sandbox-run-writer run)))
    (when (process-live-p writer)
      (process-send-string writer (concat json "\n"))
      t)))

(defun lispm-sandbox--changed (&optional verbose)
  "Bring running processes up to the level now set, as far as it can be done.
Say what was done and what needs a restart; if VERBOSE, also when
nothing ran."
  (when lispm-sandbox--change-timer
    (cancel-timer lispm-sandbox--change-timer)
    (setq lispm-sandbox--change-timer nil))
  (lispm-sandbox--ensure)
  (let* ((settings (lispm-sandbox-settings))
         (json (and settings (json-serialize settings)))
         sent restart)
    (dolist (run lispm-sandbox--runs)
      (unless (lispm-sandbox-run-level run)
        (when (and json (lispm-sandbox--send run json))
          (push run sent))
        ;; File rules are fixed when a process starts, and so is
        ;; whether it is confined at all.
        (unless (equal (plist-get settings :filesystem)
                       (plist-get (lispm-sandbox-run-settings run) :filesystem))
          (push run restart))))
    (run-hooks 'lispm-sandbox-change-hook)
    (when (or verbose sent restart)
      (message "LispM: sandbox %s (%s)%s%s"
               (lispm-sandbox-level-name lispm-sandbox-level)
               (alist-get lispm-sandbox-level lispm-sandbox-levels)
               (if sent
                   (format "; %d running process%s took its network rules"
                           (length sent) (if (cdr sent) "es" ""))
                 "")
               (if restart
                   (format "; restart %s to apply all of it"
                           (mapconcat (lambda (run) (or (lispm-sandbox-run-name run) "a process"))
                                      (nreverse restart) ", "))
                 "")))))

(defun lispm-sandbox--watch (_symbol _value operation where)
  "Bring running processes up to a change of a sandbox variable, once made.
OPERATION and WHERE are those of `add-variable-watcher'."
  (when (and (eq operation 'set) (null where) (null lispm-sandbox--change-timer))
    (setq lispm-sandbox--change-timer (run-at-time 0 nil #'lispm-sandbox--changed))))

(dolist (variable '(lispm-sandbox-level lispm-sandbox-secrets lispm-sandbox-caches
                    lispm-sandbox-registries lispm-sandbox-allowed-domains
                    lispm-sandbox-allow-write))
  (add-variable-watcher variable #'lispm-sandbox--watch))

(defun lispm-sandbox-read-level (&optional prompt)
  "Read a sandbox level in the minibuffer, with PROMPT."
  (let* ((completion-ignore-case t)
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (concat "  " (alist-get (intern (downcase name)) lispm-sandbox-levels)))))
         (current (lispm-sandbox-level-name lispm-sandbox-level))
         (name (completing-read (or prompt (format "Sandbox level (now %s): " current))
                                (mapcar (lambda (entry) (lispm-sandbox-level-name (car entry)))
                                        lispm-sandbox-levels)
                                nil t nil nil current)))
    (intern (downcase name))))

;;;###autoload
(defun lispm-sandbox-set-level (level &optional save)
  "Run agents' shell commands and MCP servers at sandbox LEVEL.
LEVEL is `none', `low', `middle' or `high' (see
`lispm-sandbox-levels').  Processes started from now on run at it;
running ones take its network rules at once.  With a prefix argument,
SAVE it for future sessions too."
  (interactive (list (lispm-sandbox-read-level) current-prefix-arg))
  (unless (assq level lispm-sandbox-levels)
    (user-error "No sandbox level %s: it is one of none, low, middle and high" level))
  (if save
      (customize-save-variable 'lispm-sandbox-level level)
    (customize-set-variable 'lispm-sandbox-level level))
  (lispm-sandbox--changed t))

;; A level chosen in the init file gets its srt while Emacs starts.
(lispm-sandbox--ensure)

(provide 'lispm-sandbox)
;;; lispm-sandbox.el ends here
