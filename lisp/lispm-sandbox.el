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
;; There are four levels, from none to the strictest, and your own:
;;
;;   none    no sandbox, the default: commands may do anything you can;
;;   low     read everything but secrets (`lispm-sandbox-secrets'); write
;;           the project, temporary files and package caches; reach
;;           package registries, code hosts and the domains of
;;           `lispm-sandbox-allowed-domains';
;;   middle  read everything but secrets; write the project and temporary
;;           files; reach only `lispm-sandbox-allowed-domains';
;;   high    read only the project, git's configuration and the system;
;;           write only the project; no network;
;;   custom  the rules of `lispm-sandbox-custom': what may be read and
;;           written, which domains reached, and whether local ports,
;;           Unix sockets, terminals and Apple Events may be used.  M-x
;;           lispm-sandbox-customize edits them, starting from a level's.
;;
;; Choose one with M-x lispm-sandbox-set-level, or /sandbox in a chat.
;; Processes started afterwards run at the new level.  Running ones
;; get its network rules at once, through srt's --control-fd; their
;; file rules were fixed when they started, so they keep those until
;; they are started again.  An MCP server's :sandbox gives it a level
;; of its own (see `lispm-mcp-servers'), and a service its level and
;; the rules it needs on top of the level's (see lispm-services.el).
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
  `((none . ,(lispm-tr-noop "no sandbox"))
    (low . ,(lispm-tr-noop "reads all but secrets; writes the project, temporary files and package caches; reaches package registries, code hosts and allowed domains"))
    (middle . ,(lispm-tr-noop "reads all but secrets; writes the project and temporary files; reaches only allowed domains"))
    (high . ,(lispm-tr-noop "reads only the project and the system; writes only the project; no network"))
    (custom . ,(lispm-tr-noop "your own rules")))
  "The sandbox levels, from none to the strictest and then your own.
Each comes with what it allows, in English; `lispm-sandbox-level-description'
says it in the language spoken, and that of `custom' from its rules.")

(defconst lispm-sandbox--level-names
  `((none . ,(lispm-tr-noop "None" "sandbox level")) (low . ,(lispm-tr-noop "Low" "sandbox level"))
    (middle . ,(lispm-tr-noop "Middle" "sandbox level")) (high . ,(lispm-tr-noop "High" "sandbox level"))
    (custom . ,(lispm-tr-noop "Custom" "sandbox level")))
  "The names of the sandbox levels, in English; see `lispm-sandbox-level-name'.")

(defcustom lispm-sandbox-level 'none
  "How strictly agents' shell commands and MCP servers are confined.
`none', the default, runs them as Emacs runs anything: they may do
whatever you can.  `low', `middle' and `high' run them in srt, each
stricter than the one before (see `lispm-sandbox-levels'), and
`custom' by the rules of `lispm-sandbox-custom'.  Set it with
`lispm-sandbox-set-level' or /sandbox in a chat; running processes
take a new level's network rules at once."
  :type '(choice (const :tag "None: no sandbox" none)
                 (const :tag "Low" low)
                 (const :tag "Middle" middle)
                 (const :tag "High" high)
                 (const :tag "Custom: `lispm-sandbox-custom'" custom)))

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

(defcustom lispm-sandbox-custom nil
  "The rules of the sandbox level `custom', or nil for those of `middle'.
A plist, each of whose keys may be left out:

  :deny-read        files and directories commands may not read;
  :allow-read       places inside those that they may read after all;
  :allow-write      where they may write, and nowhere else;
  :deny-write       places inside those that they may not write;
  :allowed-domains  the domains they may reach, and no others;
  :denied-domains   domains refused even when allowed;
  :local-binding    non-nil to let them serve on local ports and reach
                    the local ones of this machine;
  :unix-sockets     Unix sockets they may connect to, such as
                    /var/run/docker.sock, which may well let them out;
  :pty              non-nil to let them use pseudo-terminals (macOS);
  :apple-events     non-nil to let them send Apple Events and open
                    applications (macOS), which then run outside the
                    sandbox.

Entries are srt's: file names relative to the project, or globs
such as **/.env in the read rules; domains such as example.com,
*.example.com (its subdomains) or example.com:443.  Whatever the
rules, commands may write their temporary directory and never
where Emacs and LispM keep their files.  `lispm-sandbox-customize'
edits the rules, starting from those of a level."
  :type '(plist :key-type symbol
                :options ((:deny-read (repeat :tag "Files" string))
                          (:allow-read (repeat :tag "Files" string))
                          (:allow-write (repeat :tag "Files" string))
                          (:deny-write (repeat :tag "Files" string))
                          (:allowed-domains (repeat :tag "Domains" string))
                          (:denied-domains (repeat :tag "Domains" string))
                          (:local-binding boolean)
                          (:unix-sockets (repeat :tag "Sockets" string))
                          (:pty boolean)
                          (:apple-events boolean))))

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
  "Normal hook run after the sandbox level, what a level allows, or srt changed.
srt changes as LispM starts installing it and once that is done (see
`lispm-sandbox-srt-status').")

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
  "LEVEL as the user sees it: None, Low, Middle, High or Custom, in the
language spoken (see lispm-i18n.el)."
  (if-let* ((name (alist-get level lispm-sandbox--level-names)))
      (lispm-tr name)
    (capitalize (symbol-name level))))

(defun lispm-sandbox-level-named (name)
  "The level of `lispm-sandbox-levels' NAME names, or nil.
NAME is the level's name as shown, or its symbol's, case ignored."
  (seq-some (pcase-lambda (`(,level . ,_))
              (and (member (downcase (string-trim name))
                           (list (downcase (lispm-sandbox-level-name level)) (symbol-name level)))
                   level))
            lispm-sandbox-levels))

(defconst lispm-sandbox--rule-keys
  '(:deny-read :allow-read :allow-write :deny-write :allowed-domains :denied-domains
    :local-binding :unix-sockets :pty :apple-events)
  "The keys of a level's rules (see `lispm-sandbox-custom').")

(defconst lispm-sandbox--flag-keys '(:local-binding :pty :apple-events)
  "The keys of a level's rules that allow something or not; the others list.")

(defun lispm-sandbox-merge-rules (rules extra)
  "RULES with EXTRA's on top: what either lists, and what either allows.
Both are plists as `lispm-sandbox-custom' describes.  RULES nil, those
of the level none, stays nil: no rules make a sandbox of none."
  (when rules
    (let ((merged (copy-sequence rules)))
      (cl-loop for (key value) on extra by #'cddr
               do (setq merged (plist-put merged key
                                         (if (memq key lispm-sandbox--flag-keys)
                                             (or (plist-get merged key) value)
                                           (append (plist-get merged key) value)))))
      merged)))

(defun lispm-sandbox-rules (&optional level)
  "The rules of LEVEL, by default `lispm-sandbox-level'; nil for `none'.
They are a plist, as `lispm-sandbox-custom' describes; those of
`custom' are its value, or Middle's while it has none."
  (pcase (or level lispm-sandbox-level)
    ('none nil)
    ('low (list :deny-read lispm-sandbox-secrets
                :allow-write (append '(".") (lispm-sandbox--temporary)
                                     lispm-sandbox-caches lispm-sandbox-allow-write)
                :allowed-domains (append lispm-sandbox-registries lispm-sandbox-allowed-domains)))
    ('middle (list :deny-read lispm-sandbox-secrets
                   :allow-write (append '(".") (lispm-sandbox--temporary) lispm-sandbox-allow-write)
                   :allowed-domains lispm-sandbox-allowed-domains))
    ('high (list :deny-read '("~") :allow-read '("." "~/.gitconfig" "~/.config/git")
                 :allow-write '(".")))
    ('custom (or lispm-sandbox-custom (lispm-sandbox-rules 'middle)))
    (other (lispm-sandbox--fail (format "no sandbox level %s" other)))))

(defun lispm-sandbox--check (rules)
  "Signal what is wrong with RULES, which `lispm-sandbox-custom' describes."
  (unless (and (proper-list-p rules) (cl-evenp (length rules)))
    (lispm-sandbox--fail "the rules of `lispm-sandbox-custom' are not a plist"))
  (cl-loop for key in rules by #'cddr
           unless (memq key lispm-sandbox--rule-keys)
           do (lispm-sandbox--fail (format "%s is no sandbox rule (see `lispm-sandbox-custom')" key)))
  (when (member "*" (plist-get rules :allowed-domains))
    (lispm-sandbox--fail "srt cannot allow every domain: \"*\" may only be denied")))

(defun lispm-sandbox-settings (&optional level extra)
  "srt's settings for LEVEL, by default `lispm-sandbox-level', with the
rules EXTRA, a plist, on top (see `lispm-sandbox-merge-rules').
A plist for `json-serialize', or nil for `none'.  Relative file
names are relative to a sandboxed command's directory: its project."
  (when-let* ((rules (lispm-sandbox-merge-rules (lispm-sandbox-rules level) extra)))
    (lispm-sandbox--check rules)
    (let ((array (lambda (&rest lists) (vconcat (seq-uniq (apply #'append lists))))))
      `(:network (:allowedDomains ,(funcall array (plist-get rules :allowed-domains))
                  :deniedDomains ,(funcall array (plist-get rules :denied-domains))
                  ,@(and (plist-get rules :local-binding) '(:allowLocalBinding t))
                  ,@(and (plist-get rules :unix-sockets)
                         (list :allowUnixSockets (funcall array (plist-get rules :unix-sockets)))))
        :filesystem (:denyRead ,(funcall array (plist-get rules :deny-read))
                     :allowRead ,(funcall array (plist-get rules :allow-read))
                     :allowWrite ,(funcall array (plist-get rules :allow-write)
                                           (list (lispm-sandbox--tmpdir)))
                     :denyWrite ,(funcall array (plist-get rules :deny-write)
                                          (lispm-sandbox--protected)))
        ,@(and (plist-get rules :pty) '(:allowPty t))
        ,@(and (plist-get rules :apple-events) '(:allowAppleEvents t))))))

(defun lispm-sandbox--some (items)
  "The first few of ITEMS, strings, and how many more there are."
  (let ((shown (string-join (seq-take items 3) ", ")))
    (if (> (length items) 3)
        (lispm-tr "%s and %d more" shown (- (length items) 3))
      shown)))

(defun lispm-sandbox-level-description (&optional level)
  "What LEVEL, by default `lispm-sandbox-level', allows, in a few words, in
the language spoken.  That of `custom' is said from its rules."
  (let ((level (or level lispm-sandbox-level)))
    (if (not (eq level 'custom))
        (lispm-tr (alist-get level lispm-sandbox-levels))
      (let ((rules (lispm-sandbox-rules 'custom)))
        (string-join
         (delq nil
               (list (if-let* ((hidden (plist-get rules :deny-read)))
                         (if-let* ((again (plist-get rules :allow-read)))
                             (lispm-tr "reads all but %s (except %s)"
                                       (lispm-sandbox--some hidden) (lispm-sandbox--some again))
                           (lispm-tr "reads all but %s" (lispm-sandbox--some hidden)))
                       (lispm-tr "reads everything"))
                     (if-let* ((places (plist-get rules :allow-write)))
                         (lispm-tr "writes %s" (lispm-sandbox--some places))
                       (lispm-tr "writes only temporary files"))
                     (if-let* ((domains (plist-get rules :allowed-domains)))
                         (lispm-tr "reaches %s" (lispm-sandbox--some domains))
                       (lispm-tr "no network"))
                     (and (plist-get rules :local-binding) (lispm-tr "uses local ports"))
                     (and (plist-get rules :unix-sockets)
                          (lispm-tr "connects to %s" (lispm-sandbox--some (plist-get rules :unix-sockets))))
                     (and (plist-get rules :pty) (lispm-tr "uses terminals"))
                     (and (plist-get rules :apple-events) (lispm-tr "opens applications"))))
         (lispm-tr-in "between the parts of a list" "; "))))))

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

(defun lispm-sandbox-srt-status ()
  "Whether srt can run, for those who show it.
`ready' when it can; `installing' while LispM installs it; (failed
. WHY) when it cannot be had, WHY saying why; nil when it is not there
and nothing needed it yet."
  (cond ((lispm-sandbox--srt) 'ready)
        (lispm-sandbox-srt-program
         (cons 'failed (format "the srt program %s is not found" lispm-sandbox-srt-program)))
        ((process-live-p lispm-sandbox--installer) 'installing)
        (lispm-sandbox--install-failure
         (cons 'failed (format "srt could not be installed: %s" (cdr lispm-sandbox--install-failure))))))

(defun lispm-sandbox--install-failed (why)
  "Note and say that srt could not be installed, because of WHY."
  (setq lispm-sandbox--install-failure (cons (float-time) why))
  (message "%s" (lispm-tr "LispM: could not install srt for the sandbox: %s" why)))

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
          (message "%s" (lispm-tr "LispM: installing srt %s for the sandbox..." lispm-sandbox-srt-version))
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
                            (message "%s" (lispm-tr "LispM: the sandbox is ready (srt %s)"
                                                    lispm-sandbox-srt-version)))
                           ((zerop (process-exit-status process))
                            (lispm-sandbox--install-failed "node, from Node.js 22.12 or later, is not found"))
                           (t (lispm-sandbox--install-failed
                               (with-current-buffer (process-buffer process)
                                 (string-trim (buffer-substring
                                               (max (point-min) (- (point-max) 300))
                                               (point-max)))))))
                     (run-hooks 'lispm-sandbox-change-hook))))))
      (lispm-sandbox--install-failed "it needs npm, from Node.js 22.12 or later"))
    (run-hooks 'lispm-sandbox-change-hook)))

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
         (pcase (lispm-sandbox-srt-status)
           ('installing "srt is being installed; try again in a minute")
           (`(failed . ,why) why)
           (_ "srt is not installed"))))))

;;;; Processes

(cl-defstruct (lispm-sandbox-run (:constructor lispm-sandbox--make-run)
                                 (:copier nil))
  "A process started by way of the sandbox, confined or not."
  (name nil :documentation "What runs, for messages.")
  (level nil :documentation "Its own level, or nil to follow `lispm-sandbox-level'.")
  (started nil :documentation "The level it started at.")
  (extra nil :documentation "Rules on top of its level's, see `lispm-sandbox-merge-rules'.")
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

(defun lispm-sandbox-prepare (command &optional level name extra)
  "Get COMMAND, a program and its arguments, ready to run in the sandbox.
LEVEL is the level to run at; nil follows `lispm-sandbox-level', now
and as it changes.  NAME says what runs, for messages.  EXTRA are
rules on top of the level's, which COMMAND needs to do its work, such
as a service's own files and ports (see `lispm-sandbox-merge-rules');
they hold at every level but none.  Return a run: start the process
with its `lispm-sandbox-run-command', then give it to
`lispm-sandbox-attach'.  When the sandbox cannot be had, signal
`lispm-sandbox-error' rather than let COMMAND run outside it."
  (let* ((started (or level lispm-sandbox-level))
         (settings (lispm-sandbox-settings started extra))
         (run (lispm-sandbox--make-run :name name :level level :started started
                                       :extra extra :settings settings :command command)))
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
      (lispm-in-english
        (format "sandbox %s: %s" (lispm-sandbox-level-name level)
                (lispm-sandbox-level-description level))))))

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
  (let (sent restart)
    (dolist (run lispm-sandbox--runs)
      (unless (lispm-sandbox-run-level run)
        (let ((settings (lispm-sandbox-settings nil (lispm-sandbox-run-extra run))))
          (when (and settings (lispm-sandbox--send run (json-serialize settings)))
            (push run sent))
          ;; File rules are fixed when a process starts, and so is
          ;; whether it is confined at all.
          (unless (equal (plist-get settings :filesystem)
                         (plist-get (lispm-sandbox-run-settings run) :filesystem))
            (push run restart)))))
    (run-hooks 'lispm-sandbox-change-hook)
    (when (or verbose sent restart)
      (message "%s"
               (concat (lispm-tr "LispM: sandbox %s (%s)"
                                 (lispm-sandbox-level-name lispm-sandbox-level)
                                 (lispm-sandbox-level-description))
                       (if sent
                           (lispm-tr-n "; %d running process took its network rules"
                                       "; %d running processes took their network rules"
                                       (length sent))
                         "")
                       (if restart
                           (lispm-tr "; restart %s to apply all of it"
                                     (mapconcat (lambda (run) (or (lispm-sandbox-run-name run) (lispm-tr "a process")))
                                                (nreverse restart) ", "))
                         ""))))))

(defun lispm-sandbox--watch (_symbol _value operation where)
  "Bring running processes up to a change of a sandbox variable, once made.
OPERATION and WHERE are those of `add-variable-watcher'."
  (when (and (eq operation 'set) (null where) (null lispm-sandbox--change-timer))
    (setq lispm-sandbox--change-timer (run-at-time 0 nil #'lispm-sandbox--changed))))

(dolist (variable '(lispm-sandbox-level lispm-sandbox-secrets lispm-sandbox-caches
                    lispm-sandbox-registries lispm-sandbox-allowed-domains
                    lispm-sandbox-allow-write lispm-sandbox-custom))
  (add-variable-watcher variable #'lispm-sandbox--watch))

(defun lispm-sandbox-read-level (&optional prompt)
  "Read a sandbox level in the minibuffer, with PROMPT."
  (let* ((completion-ignore-case t)
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (concat "  " (lispm-sandbox-level-description (lispm-sandbox-level-named name))))))
         (current (lispm-sandbox-level-name lispm-sandbox-level))
         (name (completing-read (or prompt (lispm-tr "Sandbox level (now %s): " current))
                                (mapcar (lambda (entry) (lispm-sandbox-level-name (car entry)))
                                        lispm-sandbox-levels)
                                nil t nil nil current)))
    (lispm-sandbox-level-named name)))

;;;###autoload
(defun lispm-sandbox-set-level (level &optional save)
  "Run agents' shell commands and MCP servers at sandbox LEVEL.
LEVEL is `none', `low', `middle', `high' or `custom' (see
`lispm-sandbox-levels').  Processes started from now on run at it;
running ones take its network rules at once.  With a prefix argument,
SAVE it for future sessions too."
  (interactive (list (lispm-sandbox-read-level) current-prefix-arg))
  (unless (assq level lispm-sandbox-levels)
    (user-error "%s" (lispm-tr "No sandbox level %s: it is one of %s"
                               level (mapconcat (pcase-lambda (`(,level . ,_)) (lispm-sandbox-level-name level))
                                                lispm-sandbox-levels ", "))))
  (if save
      (customize-save-variable 'lispm-sandbox-level level)
    (customize-set-variable 'lispm-sandbox-level level))
  (lispm-sandbox--changed t))

;;;###autoload
(defun lispm-sandbox-customize (&optional from)
  "Edit the rules of the sandbox level `custom' in Customize.
Rules it has not got yet start as those of the level FROM, `middle'
by default; with a prefix argument, choose FROM, whose rules then
replace those it has.  Choose the level itself with
`lispm-sandbox-set-level'."
  (interactive
   (list (when current-prefix-arg
           (let ((completion-ignore-case t))
             (lispm-sandbox-level-named
              (completing-read (lispm-tr "Start from the rules of: ")
                               (mapcar #'lispm-sandbox-level-name '(low middle high)) nil t))))))
  (when (or from (null lispm-sandbox-custom))
    (customize-set-variable 'lispm-sandbox-custom
                            (copy-tree (lispm-sandbox-rules (or from 'middle)))))
  (customize-variable 'lispm-sandbox-custom))

;; A level chosen in the init file gets its srt while Emacs starts.
(lispm-sandbox--ensure)

(provide 'lispm-sandbox)
;;; lispm-sandbox.el ends here
