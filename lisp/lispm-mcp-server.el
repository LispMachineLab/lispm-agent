;;; lispm-mcp-server.el --- This Emacs as an MCP server for agents outside LispM  -*- lexical-binding: t; -*-

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

;; The editor's agents LispM starts in its terminals are shown Emacs over
;; servers of their own (lispm-agents-mcp.el).  An agent started anywhere
;; else, as Claude Code in another terminal, Codex, an editor's assistant
;; or a script, reaches this Emacs over one standing MCP server, which
;; `lispm-mcp-server-mode' runs:
;;
;;   mode on --> server on a free local port, with a token of its own
;;           --> `lispm-mcp-server-file': its address and token, readable
;;               by its owner only
;;   agent --> bin/lispm-mcp, an MCP server over stdio
;;         --> POST /mcp with "Authorization: Bearer TOKEN"
;;
;; Any MCP client runs bin/lispm-mcp as a server of its own, as
;;
;;   claude mcp add lispm -- ~/.lispm/lispm-agent/bin/lispm-mcp
;;
;; or speaks streamable HTTP to the address in the file.  Each client
;; connecting opens a session (Mcp-Session-Id) of an agent outside
;; LispM, in the directory bin/lispm-mcp runs in.  In LispM, it acts as
;; an agent of `lispm-mcp-server-role' there, which is none of LispM's
;; running agents, and it has
;;
;;   the read tools of `lispm-agents-mcp-tools'   what is open in Emacs,
;;                    the selection, diagnostics and unsaved changes;
;;   LispM's tools    those an agent of its role has from the start, as
;;                    eval_elisp, read_buffer, edit_buffer and run_shell,
;;                    each under its own name;
;;   search_tools skill call_tool   the rest of LispM's tools, and its
;;                    skills (lispm-agents-bridge.el).
;;
;; Every call of a LispM tool goes by that agent's policy, as LispM's
;; agents' calls do (`lispm-tool-decision'): one the policy holds for
;; approval asks the user with `lispm-agents-confirm-function', and a
;; shell command runs in the sandbox (lispm-sandbox.el).  Messages between
;; the agents reach no agent outside LispM, so none is served the tools
;; talking with them.
;;
;; The server starts with LispM in an interactive Emacs, unless
;; `lispm-mcp-server-autostart' is nil; a batch Emacs starts none.  The
;; first Emacs to serve keeps the file while it runs: another started
;; meanwhile, as an Emacs trying a change, serves nothing.  bin/lispm-tool,
;; the command of agents with a shell but no MCP client, calls the same
;; tools when it finds no server of its own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-local-http)
(require 'lispm-agents-context)
(require 'lispm-agents-mcp)
(require 'lispm-agents-bridge)
(require 'lispm-core)
(require 'lispm-tools)

(defcustom lispm-mcp-server-autostart t
  "Non-nil to serve agents outside LispM once LispM is loaded in an
interactive Emacs, see `lispm-mcp-server-mode'."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-mcp-server-role nil
  "Role whose tools and policy agents outside LispM have in it.
Nil for that of the editor's agents, `lispm-agents-bridge-role'."
  :type '(choice (const :tag "That of the editor's agents" nil) symbol)
  :group 'lispm)

(defvar lispm-mcp-server-file "~/.lispm/mcp-server"
  "File naming the server to agents outside LispM, readable by its owner
only: its address, its token and the process id of the Emacs serving, a
line each.  bin/lispm-mcp and bin/lispm-tool read it, or the file
$LISPM_MCP_SERVER names instead.")

(defconst lispm-mcp-server-program
  (expand-file-name "../bin/lispm-mcp" (file-name-directory (or load-file-name buffer-file-name)))
  "The command an MCP client runs to reach this Emacs, over stdio.")

(defvar lispm-mcp-server-max-sessions 32
  "Sessions the server keeps; one more connecting ends that used least lately.")

(defconst lispm-mcp-server-unserved-tools '("list_agents" "send_message" "ask_agent" "reply")
  "Tools of `lispm-agents-mcp-extra-tools' not served to agents outside
LispM: those talking with the agents, whose messages reach none of them.")

(defvar lispm-mcp-server--process nil
  "The HTTP server, while `lispm-mcp-server-mode' is on.")

(defvar lispm-mcp-server--token nil
  "The token agents outside LispM connect with, while the server runs.")

(defvar lispm-mcp-server--owned-file nil
  "Absolute descriptor name whose ownership lock this Emacs holds.")

(defvar lispm-mcp-server--sessions (make-hash-table :test #'equal)
  "The sessions of agents outside LispM: their Mcp-Session-Id, or (shell
. DIRECTORY) for the commands of a directory, to the session.")

(cl-defstruct (lispm-mcp-server-session (:include lispm-agents-mcp)
                                        (:constructor lispm-mcp-server--session-make))
  "The session of an agent outside LispM, working in DIRECTORY.
ID is its Mcp-Session-Id; NAME and KIND are what its client calls itself;
AGENT is the LispM agent it acts as; USED is when it last asked anything.
CLOSED invalidates queued calls; PENDING holds answers not yet delivered."
  id name kind agent used closed pending)

;;;; Sessions

(defun lispm-mcp-server--close (key session)
  "End SESSION kept under KEY, cancelling its work and pending answers."
  (when (eq (gethash key lispm-mcp-server--sessions) session)
    (remhash key lispm-mcp-server--sessions))
  (unless (lispm-mcp-server-session-closed session)
    ;; Invalidate callbacks before cleanup can yield to a pending approval.
    (setf (lispm-mcp-server-session-closed session) t)
    (when-let* ((agent (lispm-mcp-server-session-agent session)))
      (run-hook-wrapped
       'lispm-session-end-functions
       (lambda (function)
         (condition-case err (funcall function agent)
           ((error quit) (message "LispM MCP session cleanup: %s" (error-message-string err))))
         nil))
      ;; run_shell and other subprocess tools also use the native agent tag.
      (dolist (process (process-list))
        (when (eq (process-get process 'lispm-agent) agent)
          (process-put process 'lispm-aborted t)
          (ignore-errors (delete-process process)))))
    (dolist (answer (lispm-mcp-server-session-pending session))
      (condition-case err (funcall answer '("Cancelled: the session has ended.") t)
        ((error quit) (message "LispM MCP pending answer: %s" (error-message-string err)))))))

(defun lispm-mcp-server--name (client)
  "The name of the agent of CLIENT, its clientInfo: its client's name, in
letters, digits, dots, dashes and underscores."
  (let ((name (string-trim (replace-regexp-in-string
                            "[^A-Za-z0-9._-]+" "-" (format "%s" (or (alist-get 'name client) "")))
                           "-+" "-+")))
    (if (string-empty-p name) "agent" name)))

(defun lispm-mcp-server--directory (headers)
  "The directory the agent whose request has HEADERS works in: the one
X-LispM-Directory names, if a local one, else the home directory."
  (let ((directory (when-let* ((value (cdr (assoc "x-lispm-directory" headers))))
                     (decode-coding-string value 'utf-8))))
    (file-name-as-directory
     (expand-file-name
      (if (and directory
               (file-name-absolute-p directory)
               ;; A remote name would have TRAMP connect.
               (not (file-remote-p directory))
               (file-directory-p directory))
          directory
        "~")))))

(defun lispm-mcp-server--add (key session)
  "Keep SESSION under KEY, ending the session used least lately when
`lispm-mcp-server-max-sessions' are kept already."
  (when (>= (hash-table-count lispm-mcp-server--sessions) lispm-mcp-server-max-sessions)
    (let (oldest)
      (maphash (lambda (key session)
                 (when (or (null oldest)
                           (< (lispm-mcp-server-session-used session)
                              (lispm-mcp-server-session-used (cdr oldest))))
                   (setq oldest (cons key session))))
               lispm-mcp-server--sessions)
      (lispm-mcp-server--close (car oldest) (cdr oldest))))
  (puthash key session lispm-mcp-server--sessions)
  session)

(defun lispm-mcp-server--make (name kind directory)
  "Return a new session of an agent NAME, of KIND, working in DIRECTORY."
  (lispm-mcp-server--session-make
   :server lispm-mcp-server--process
   :port (process-contact lispm-mcp-server--process :service)
   :token lispm-mcp-server--token
   :directory directory
   :id (lispm-agents-uuid)
   :name name
   :kind (lispm-tr "%s (outside LispM)" kind)
   :used (float-time)))

(defun lispm-mcp-server--open (headers message)
  "Open the session of the agent asking to initialize with MESSAGE, its
request having HEADERS; return it."
  (let* ((client (alist-get 'clientInfo (alist-get 'params message)))
         (name (lispm-mcp-server--name client))
         (title (alist-get 'title client))
         (session (lispm-mcp-server--make name (if (and (stringp title) (not (string-empty-p title)))
                                                   title
                                                 name)
                                          (lispm-mcp-server--directory headers))))
    (lispm-mcp-server--add (lispm-mcp-server-session-id session) session)))

(defun lispm-mcp-server--find (key)
  "Return the session kept under KEY, noting its use, or nil."
  (when-let* ((session (gethash key lispm-mcp-server--sessions)))
    (setf (lispm-mcp-server-session-used session) (float-time))
    session))

(defun lispm-mcp-server--shell (headers)
  "Return the session of the commands, bin/lispm-tool's, of the directory
a request with HEADERS names, opened at first need."
  (let ((directory (lispm-mcp-server--directory headers)))
    (or (lispm-mcp-server--find (cons 'shell directory))
        (lispm-mcp-server--add (cons 'shell directory)
                               (lispm-mcp-server--make "lispm-tool" "lispm-tool" directory)))))

(cl-defmethod lispm-agents-bridge-server-agent ((session lispm-mcp-server-session))
  "The LispM agent SESSION's agent acts as, made at first need: of the
role `lispm-mcp-server-role' names, in SESSION's directory."
  (or (lispm-mcp-server-session-agent session)
      (setf (lispm-mcp-server-session-agent session)
            (lispm--apply-role
             (lispm-agent--create :name (intern (lispm-mcp-server-session-name session))
                                  :id (lispm-mcp-server-session-id session)
                                  :root (lispm-mcp-server-session-directory session))
             (lispm-get-role (or lispm-mcp-server-role lispm-agents-bridge-role lispm-default-role))))))

(cl-defmethod lispm-agents-bridge-server-kind ((session lispm-mcp-server-session))
  "What SESSION's agent is, as its client says, outside LispM."
  (lispm-mcp-server-session-kind session))

(cl-defmethod lispm-agents-bridge-server-live-p ((session lispm-mcp-server-session))
  "Whether SESSION has not been ended by DELETE, eviction or server stop."
  (not (lispm-mcp-server-session-closed session)))

(cl-defmethod lispm-agents-bridge-server-answer ((session lispm-mcp-server-session) answer)
  "Settle ANSWER once, including when SESSION ends before its tool does."
  (let (done callback)
    (setq callback
          (lambda (text &optional errorp)
            (unless done
              (setq done t)
              (setf (lispm-mcp-server-session-pending session)
                    (delq callback (lispm-mcp-server-session-pending session)))
              (funcall answer
                       (if (lispm-mcp-server-session-closed session)
                           '("Cancelled: the session has ended.")
                         text)
                       (or (lispm-mcp-server-session-closed session) errorp)))))
    (if (lispm-mcp-server-session-closed session)
        (funcall callback '("Cancelled: the session has ended.") t)
      (push callback (lispm-mcp-server-session-pending session)))
    callback))

;;;; Tools

(defun lispm-mcp-server--tool (tool)
  "LispM's TOOL, served under its own name: a call runs as call_tool runs it."
  (let ((name (lispm-tool-name tool))
        (schema (alist-get 'input_schema (lispm-tool-schema tool))))
    (list name
          (lambda (session arguments id)
            (lispm-agents-bridge--tool-call session `((name . ,name) (input . ,arguments)) id))
          (alist-get 'properties schema)
          (alist-get 'required schema)
          (lispm-tools--description tool))))

(cl-defmethod lispm-agents-mcp-served-tools ((session lispm-mcp-server-session))
  "The tools SESSION's agent is served: the read tools, LispM's tools an
agent of its role has from the start, and the bridge's, but for those
talking with the agents."
  (let* ((bridge (seq-remove (lambda (tool) (member (car tool) lispm-mcp-server-unserved-tools))
                             lispm-agents-mcp-extra-tools))
         (taken (append lispm-agents-mcp-tools bridge)))
    (append lispm-agents-mcp-tools
            (mapcar #'lispm-mcp-server--tool
                    (seq-filter (lambda (tool) (and (lispm-agents-bridge--served-p tool)
                                                    (not (assoc (lispm-tool-name tool) taken))))
                                (lispm-agent-tools* (lispm-agents-bridge-server-agent session))))
            bridge)))

(cl-defmethod lispm-agents-context-instructions ((session lispm-mcp-server-session))
  "Tell SESSION's agent what this Emacs is, and how to work with it."
  (format "This is the user's Emacs, running LispM; you reach it from outside, as %s, working in \
%s. These tools act in it, where the user works. getOpenEditors, getCurrentSelection, getDiagnostics \
and checkDocumentDirty tell what the user has open, has selected and has not saved, and openFile \
shows them a file. LispM's own tools, listed beside them, work on Emacs's buffers, files and Lisp: \
reading buffers, editing them, evaluating Emacs Lisp, running shell commands. search_tools finds \
the rest of LispM's tools, which you call with call_tool, and its skills, the manuals of the parts \
and apps of this system, which you load with skill. Every call goes by LispM's permission policy: \
reading runs at once, while changing files or buffers, running shell commands and evaluating Lisp \
may wait for the user's approval in Emacs, which may take a while. A denied call is the user's \
answer: do not try to get round it."
          (lispm-mcp-server-session-name session)
          (abbreviate-file-name (directory-file-name (lispm-mcp-server-session-directory session)))))

;;;; Requests

(defun lispm-mcp-server--authorized-p (headers)
  "Non-nil if a request with HEADERS carries the server's token and comes
from no web page, whose requests carry an Origin."
  (and lispm-mcp-server--token
       (equal (cdr (assoc "authorization" headers)) (concat "Bearer " lispm-mcp-server--token))
       (not (assoc "origin" headers))))

(defun lispm-mcp-server--refusal (status message text)
  "The response of STATUS to MESSAGE, which is not answered, saying TEXT:
a JSON-RPC error when MESSAGE is a request, else nothing."
  (if-let* ((id (and (consp message) (alist-get 'id message))))
      (list status (json-serialize (lispm-agents-context-error id -32600 text)))
    (list status)))

(defun lispm-mcp-server--respond (method path headers body)
  "Answer the request METHOD PATH, with HEADERS and BODY, of an agent
outside LispM.  POST /mcp answers MCP in the session its Mcp-Session-Id
names, an initialize opening one; DELETE /mcp ends it.  GET /tools and
POST /call/NAME answer bin/lispm-tool."
  (let* ((path (car (split-string path "?")))
         (key (cdr (assoc "mcp-session-id" headers)))
         (session (and key (lispm-mcp-server--find key))))
    (cond
     ((equal path "/mcp")
      (let ((message (and (equal method "POST") (lispm-agents-context-parse body))))
        (cond
         ((equal method "DELETE")
          (if session
              (progn (lispm-mcp-server--close key session) '("200 OK"))
            '("404 Not Found")))
         ((not (equal method "POST")) '("405 Method Not Allowed"))
         ((and (consp message) (equal (alist-get 'method message) "initialize"))
          (let ((session (lispm-mcp-server--open headers message)))
            (pcase-let ((`(,status ,answer ,type) (lispm-agents-mcp--respond session method path body)))
              (list status answer type
                    `(("Mcp-Session-Id" . ,(lispm-mcp-server-session-id session)))))))
         (session (lispm-agents-mcp--respond session method path body))
         ((not key)
          (lispm-mcp-server--refusal "400 Bad Request" message "Initialize a session first"))
         (t (lispm-mcp-server--refusal "404 Not Found" message
                                       "No such session: LispM has started again since; connect again")))))
     ((or (equal path "/tools") (string-prefix-p "/call/" path))
      (lispm-agents-mcp--respond (lispm-mcp-server--shell headers) method path body))
     (t '("404 Not Found")))))

;;;; The server

(defun lispm-mcp-server--read-file ()
  "The lines of `lispm-mcp-server-file': address, token and process id."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (expand-file-name lispm-mcp-server-file))
      (split-string (buffer-string) "\n" t))))

(defun lispm-mcp-server--emacs-p (pid)
  "Non-nil if PID is the process id of a running Emacs, LispM.app's too."
  (when-let* ((attributes (process-attributes pid)))
    (string-match-p "emacs\\|lispm" (downcase (or (alist-get 'comm attributes) "")))))

(defun lispm-mcp-server-holder ()
  "Return the process id of another Emacs serving agents outside LispM,
which keeps `lispm-mcp-server-file' while it runs, or nil."
  (when-let* ((pid (string-to-number (or (nth 2 (lispm-mcp-server--read-file)) "")))
              ((> pid 0))
              ((/= pid (emacs-pid)))
              ((lispm-mcp-server--emacs-p pid)))
    pid))

(defun lispm-mcp-server--announce ()
  "Name the server in `lispm-mcp-server-file', readable by its owner only.
The file holds the token, so one left by a crash is replaced rather than
written with whatever permissions it had."
  (let* ((file lispm-mcp-server--owned-file)
         (temporary (make-temp-file (concat file "."))))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (write-region (format "http://127.0.0.1:%d\n%s\n%d\n"
                                (process-contact lispm-mcp-server--process :service)
                                lispm-mcp-server--token (emacs-pid))
                        nil temporary nil 'silent)
          ;; Readers see the complete old descriptor or the complete new one.
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun lispm-mcp-server--claim ()
  "Claim descriptor ownership atomically, returning nil when already held.
Emacs's file locks identify the owning process and recover dead owners.
Use a dedicated name, so visiting the descriptor cannot release this lock."
  (let* ((file (expand-file-name lispm-mcp-server-file))
         (owner (concat file ".owner"))
         (create-lockfiles t))
    (with-file-modes #o700
      (make-directory (file-name-directory file) t))
    (catch 'held
      (cl-letf (((symbol-function 'ask-user-about-lock)
                 (lambda (&rest _) (throw 'held nil))))
        (lock-file owner))
      ;; Some filesystems cannot create locks.  Never publish without one.
      (unless (eq (file-locked-p owner) t)
        (error "Could not claim LispM MCP server ownership: %s" file))
      (setq lispm-mcp-server--owned-file file)
      t)))

(defun lispm-mcp-server--start ()
  "Start the server and name it in `lispm-mcp-server-file'.
Return non-nil if it started: it does not while another Emacs serves."
  (when (lispm-mcp-server--claim)
    (let (started)
      (unwind-protect
          ;; Also respect a running server from before ownership locks existed.
          (if-let* ((holder (lispm-mcp-server-holder)))
              (message "%s" (lispm-tr "LispM: Emacs %d serves agents outside LispM; this one serves none"
                                       holder))
            (setq lispm-mcp-server--token (lispm-agents-uuid)
                  lispm-mcp-server--process (lispm-local-http-serve "lispm-mcp-server"
                                                                  #'lispm-mcp-server--authorized-p
                                                                  #'lispm-mcp-server--respond))
            (lispm-mcp-server--announce)
            (add-hook 'kill-emacs-hook #'lispm-mcp-server--stop)
            (setq started t))
        (unless started (lispm-mcp-server--stop)))
      started)))

(defun lispm-mcp-server--stop ()
  "Stop the server, end its sessions, and delete `lispm-mcp-server-file'
if it still names this server."
  (unwind-protect
      (maphash #'lispm-mcp-server--close lispm-mcp-server--sessions)
    (when lispm-mcp-server--process
      (let ((lispm-mcp-server-file (or lispm-mcp-server--owned-file lispm-mcp-server-file)))
        (when (equal (nth 1 (lispm-mcp-server--read-file)) lispm-mcp-server--token)
          (ignore-errors (delete-file (expand-file-name lispm-mcp-server-file)))))
      (delete-process lispm-mcp-server--process))
    (setq lispm-mcp-server--process nil
          lispm-mcp-server--token nil)
    (when lispm-mcp-server--owned-file
      (unlock-file (concat lispm-mcp-server--owned-file ".owner"))
      (setq lispm-mcp-server--owned-file nil))
    (remove-hook 'kill-emacs-hook #'lispm-mcp-server--stop)))

;;;###autoload
(define-minor-mode lispm-mcp-server-mode
  "Serve this Emacs over MCP to agents outside LispM.
An agent LispM did not start, as Claude Code in another terminal or
Codex, connects with the command `lispm-mcp-server-program', which any
MCP client runs as a server of its own:

  claude mcp add lispm -- ~/.lispm/lispm-agent/bin/lispm-mcp

It is then an agent of `lispm-mcp-server-role' in its directory: it has
that role's tools and LispM's read tools, and its calls go by that
role's policy, asking the user when the policy says so.  Only the
user's own programs can read the token it connects with, in
`lispm-mcp-server-file'.  While another Emacs serves, this one does not."
  :global t
  :group 'lispm
  (if lispm-mcp-server-mode
      (unless (or lispm-mcp-server--process (lispm-mcp-server--start))
        (setq lispm-mcp-server-mode nil))
    (lispm-mcp-server--stop)))

(defun lispm-mcp-server-maybe-start ()
  "Serve agents outside LispM, if `lispm-mcp-server-autostart' says so
and Emacs is interactive."
  (when (and lispm-mcp-server-autostart (not noninteractive) (not lispm-mcp-server-mode))
    (lispm-mcp-server-mode 1)))

(provide 'lispm-mcp-server)
;;; lispm-mcp-server.el ends here
