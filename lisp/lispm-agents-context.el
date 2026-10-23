;;; lispm-agents-context.el --- Emacs state served to coding agents -*- lexical-binding: t; -*-

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

;; What an agent cannot read from disk or Git exists only in Emacs: the
;; selection and cursor, which files are open and unsaved, and Flymake's
;; diagnostics.  This module serves it as MCP tools to the agents' own
;; servers, one per agent terminal:
;;
;;   lispm-agents-ide  Claude Code's IDE protocol, over a WebSocket
;;   lispm-agents-mcp  plain MCP over HTTP, for the others, as Codex
;;
;;   terminal --launch--> server --bind--> stops with its terminal
;;   request --`lispm-agents-context-handle'--> tool (directory, arguments)
;;
;; Every tool here is read-only, and an agent sees only files under the
;; directory its terminal runs in.  A terminal may have several servers,
;; as Claude's IDE server and its MCP server for LispM's tools
;; (lispm-agents-bridge.el); a tool may answer later, see
;; `lispm-agents-context-answer'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function url-unhex-string "url-util" (str &optional allow-newlines))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function evil-visual-state-p "evil-states" (&optional state))
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar flymake-mode)

;;;; Servers bound to terminals

(cl-defgeneric lispm-agents-context-environment (server)
  "Return the environment entries that point an agent at SERVER."
  (ignore server))

(cl-defgeneric lispm-agents-context-arguments (server)
  "Return the command-line arguments that point an agent at SERVER."
  (ignore server))

(cl-defgeneric lispm-agents-context-stop (server)
  "Stop SERVER and release what it holds.")

(cl-defgeneric lispm-agents-context-instructions (server)
  "Return what an agent is told of SERVER when it connects, or nil.
MCP clients may show it the model, as Claude Code does."
  (ignore server))

(defvar lispm-agents-context--servers nil
  "Live servers, as (TERMINAL . SERVER), a terminal's first bound first.")

(defvar-local lispm-agents-context-server nil
  "The first server bound to this agent terminal.")
(put 'lispm-agents-context-server 'permanent-local t)

(defvar lispm-agents-context-answer nil
  "While a tool runs, a function answering its request later.
It takes the text items of the result and, when the tool failed,
non-nil.  A tool that answers later keeps it and returns `deferred'.")

(defun lispm-agents-context-bind (server terminal)
  "Bind SERVER to the agent TERMINAL buffer: it stops when the terminal's
process exits, when the terminal is killed, or when Emacs exits."
  (setq lispm-agents-context--servers
        (append lispm-agents-context--servers (list (cons terminal server))))
  (with-current-buffer terminal
    (unless lispm-agents-context-server
      (setq lispm-agents-context-server server))
    (add-hook 'kill-buffer-hook #'lispm-agents-context--terminal-killed nil t))
  (add-hook 'eat-exit-hook #'lispm-agents-context--terminal-exited)
  (add-hook 'kill-emacs-hook #'lispm-agents-context-release-all)
  (add-hook 'post-command-hook #'lispm-agents-context--note-selection))

(defvar lispm-agents-context-release-functions nil
  "Functions called with an agent terminal whose servers are about to stop:
its program exited, or it was killed.")

(defun lispm-agents-context-release (terminal)
  "Stop the servers bound to TERMINAL, if any."
  (when-let* ((entries (seq-filter (lambda (entry) (eq (car entry) terminal))
                                   lispm-agents-context--servers)))
    (with-demoted-errors "lispm-agents: releasing a terminal: %S"
      (run-hook-with-args 'lispm-agents-context-release-functions terminal))
    (setq lispm-agents-context--servers
          (seq-difference lispm-agents-context--servers entries #'eq))
    (when (buffer-live-p terminal)
      (with-current-buffer terminal (setq lispm-agents-context-server nil)))
    (unless lispm-agents-context--servers
      (remove-hook 'post-command-hook #'lispm-agents-context--note-selection))
    (dolist (entry entries)
      (lispm-agents-context-stop (cdr entry)))))

(defun lispm-agents-context-terminal (server)
  "Return the agent terminal SERVER is bound to, or nil."
  (car (rassq server lispm-agents-context--servers)))

(defun lispm-agents-context-servers (terminal)
  "Return the servers bound to the agent TERMINAL, the first bound first."
  (mapcar #'cdr (seq-filter (lambda (entry) (eq (car entry) terminal))
                            lispm-agents-context--servers)))

(defun lispm-agents-context-release-all ()
  "Stop every agent server."
  (mapc #'lispm-agents-context-release (mapcar #'car lispm-agents-context--servers)))

(defun lispm-agents-context--terminal-killed ()
  "Stop the server of the terminal being killed."
  (lispm-agents-context-release (current-buffer)))

(defun lispm-agents-context--terminal-exited (process)
  "Stop the server of the Eat terminal whose PROCESS exited."
  (lispm-agents-context-release (process-buffer process)))

(defun lispm-agents-context-terminal-exited (buffer &rest _)
  "Stop the server of the terminal BUFFER, whose program exited.  Terminals
other than Eat's call this, see `lispm-agents-terminal-make-function'."
  (lispm-agents-context-release buffer))

;;;; Scope

(defun lispm-agents-context-path (uri)
  "Return the absolute file name of URI, a file URI or a path."
  (require 'url-util)
  (expand-file-name (if (string-prefix-p "file://" uri)
                        (url-unhex-string (substring uri (length "file://")))
                      uri)))

(defun lispm-agents-context-visible-p (directory file)
  "Return non-nil when FILE lies under DIRECTORY."
  (and file (file-in-directory-p file directory)))

(defun lispm-agents-context--buffers (directory)
  "Return the buffers visiting files under DIRECTORY."
  (seq-filter (lambda (buffer)
                (lispm-agents-context-visible-p directory (buffer-file-name buffer)))
              (buffer-list)))

(defun lispm-agents-context--position (position)
  "Return POSITION of the current buffer as a zero-based line and character."
  (save-excursion
    (goto-char position)
    `((line . ,(1- (line-number-at-pos position t)))
      (character . ,(- position (line-beginning-position))))))

;;;; Selection

(defvar lispm-agents-context-selection nil
  "The latest selection in a file, as the IDE protocol describes it.")

(defvar lispm-agents-context-selection-functions nil
  "Functions called with no argument when the selection recorded changed.
The selection is `lispm-agents-context-selection'.")

(defvar lispm-agents-context--selection-key nil
  "What the latest selection was computed from: buffer, point and mark.")

(defvar lispm-agents-context--selection-timer nil
  "Timer that records the selection once the user pauses.")

(defun lispm-agents-context--region ()
  "Return the selected region of the current buffer as (START . END), or nil.
In Evil's visual state the region is Evil's, which spans whole lines in
line selection."
  (cond ((and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
         (cons (marker-position evil-visual-beginning) (marker-position evil-visual-end)))
        ((use-region-p) (cons (region-beginning) (region-end)))))

(defun lispm-agents-context--current-selection ()
  "Return the selection, or the cursor, of the current file buffer."
  (let* ((region (lispm-agents-context--region))
         (start (or (car region) (point)))
         (end (or (cdr region) (point))))
    `((text . ,(if region (buffer-substring-no-properties start end) ""))
      (filePath . ,buffer-file-name)
      (fileUrl . ,(concat "file://" buffer-file-name))
      (selection . ((start . ,(lispm-agents-context--position start))
                    (end . ,(lispm-agents-context--position end))
                    (isEmpty . ,(if region :false t)))))))

(defun lispm-agents-context--note-selection ()
  "Record the selection soon when a command moved it in a file buffer.
Runs after every command while an agent server lives, so it only compares
point and mark; the selection itself is computed once the user pauses."
  (when buffer-file-name
    (let ((key (list (current-buffer) (point) (and (lispm-agents-context--region) (mark t)))))
      (unless (equal key lispm-agents-context--selection-key)
        (setq lispm-agents-context--selection-key key)
        (when lispm-agents-context--selection-timer
          (cancel-timer lispm-agents-context--selection-timer))
        (setq lispm-agents-context--selection-timer
              (run-with-idle-timer 0.1 nil #'lispm-agents-context-record-selection
                                   (current-buffer)))))))

(defun lispm-agents-context-record-selection (buffer)
  "Record BUFFER's selection and run the hook telling of its change.
The hook is `lispm-agents-context-selection-functions'."
  (setq lispm-agents-context--selection-timer nil)
  (when (buffer-live-p buffer)
    (setq lispm-agents-context-selection
          (with-current-buffer buffer (lispm-agents-context--current-selection)))
    (run-hooks 'lispm-agents-context-selection-functions)))

(defun lispm-agents-context-selection (directory)
  "Return the latest selection when its file lies under DIRECTORY, else nil."
  (when (lispm-agents-context-visible-p directory (alist-get 'filePath lispm-agents-context-selection))
    lispm-agents-context-selection))

;;;; Tools

(defun lispm-agents-context--tool-selection (directory _arguments)
  "Return the latest selection in a file under DIRECTORY."
  (list (json-serialize
         (or (lispm-agents-context-selection directory)
             '((text . "") (filePath . "")
               (selection . ((start . ((line . 0) (character . 0)))
                             (end . ((line . 0) (character . 0)))
                             (isEmpty . t))))))))

(defun lispm-agents-context--tool-open-editors (directory _arguments)
  "Return the files open in Emacs under DIRECTORY."
  (let ((current (window-buffer (selected-window))))
    (list (json-serialize
           `((tabs . ,(vconcat
                       (mapcar (lambda (buffer)
                                 `((uri . ,(concat "file://" (buffer-file-name buffer)))
                                   (label . ,(buffer-name buffer))
                                   (isActive . ,(if (eq buffer current) t :false))
                                   (isDirty . ,(if (buffer-modified-p buffer) t :false))))
                               (lispm-agents-context--buffers directory)))))))))

(defun lispm-agents-context--tool-workspace-folders (directory _arguments)
  "Return DIRECTORY as the only workspace folder."
  (let ((root (directory-file-name directory)))
    (list (json-serialize
           `((success . t)
             (folders . [((name . ,(file-name-nondirectory root))
                          (uri . ,(concat "file://" root))
                          (path . ,root))])
             (rootPath . ,root))))))

(defun lispm-agents-context--diagnostics (buffer)
  "Return Flymake's diagnostics of BUFFER as the IDE protocol describes them."
  (with-current-buffer buffer
    (when (bound-and-true-p flymake-mode)
      (vconcat
       (mapcar (lambda (diagnostic)
                 `((range . ((start . ,(lispm-agents-context--position
                                        (flymake-diagnostic-beg diagnostic)))
                             (end . ,(lispm-agents-context--position
                                      (flymake-diagnostic-end diagnostic)))))
                   (severity . ,(pcase (flymake-diagnostic-type diagnostic)
                                  ((or :error 'eglot-error) "Error")
                                  ((or :warning 'eglot-warning) "Warning")
                                  (_ "Information")))
                   (message . ,(flymake-diagnostic-text diagnostic))
                   (source . ,(format "%s" (flymake-diagnostic-backend diagnostic)))))
               (flymake-diagnostics))))))

(defun lispm-agents-context--tool-diagnostics (directory arguments)
  "Return the diagnostics of the file ARGUMENTS name, or of every open file.
Only files under DIRECTORY are reported."
  (let* ((uri (alist-get 'uri arguments))
         (file (and uri (lispm-agents-context-path uri)))
         (buffers (if file
                      (when-let* (((lispm-agents-context-visible-p directory file))
                                  (buffer (find-buffer-visiting file)))
                        (list buffer))
                    (lispm-agents-context--buffers directory))))
    (list (json-serialize
           (vconcat
            (delq nil
                  (mapcar (lambda (buffer)
                            (when-let* ((diagnostics (lispm-agents-context--diagnostics buffer)))
                              `((uri . ,(concat "file://" (buffer-file-name buffer)))
                                (diagnostics . ,diagnostics))))
                          buffers)))))))

(defun lispm-agents-context--tool-dirty (directory arguments)
  "Return whether the file ARGUMENTS name has unsaved changes in Emacs."
  (let* ((file (lispm-agents-context-path (alist-get 'uri arguments)))
         (buffer (and (lispm-agents-context-visible-p directory file)
                      (find-buffer-visiting file))))
    (list (json-serialize
           (if buffer
               `((success . t) (filePath . ,file)
                 (isDirty . ,(if (buffer-modified-p buffer) t :false))
                 (isUntitled . :false))
             `((success . :false) (message . ,(format "Document not open: %s" file))))))))

(defvar lispm-agents-context-display-action '(nil (inhibit-same-window . t))
  "The action showing a file an agent opens, or a change it proposes, see
`display-buffer'.")

(defun lispm-agents-context-display (buffer)
  "Show BUFFER where `lispm-agents-context-display-action' says, keeping
the selection."
  (display-buffer buffer lispm-agents-context-display-action))

(defun lispm-agents-context--tool-open-file (_directory arguments)
  "Show the file ARGUMENTS name, at its start text when given, without focus."
  (let ((file (lispm-agents-context-path (or (alist-get 'filePath arguments)
                                             (alist-get 'uri arguments)))))
    (unless (file-exists-p file)
      (error "File not found: %s" file))
    (let ((buffer (find-file-noselect file))
          (text (alist-get 'startText arguments)))
      (when (and text (not (string-empty-p text)))
        (with-current-buffer buffer
          (when-let* ((position (save-excursion
                                  (goto-char (point-min))
                                  (search-forward text nil t))))
            (goto-char (- position (length text))))))
      (lispm-agents-context-display buffer)
      (list (format "Opened file: %s" file)))))

(defconst lispm-agents-context--uri-schema
  '((uri . ((type . "string") (description . "The file URI or path"))))
  "Input properties of a tool that takes one file.")

(defconst lispm-agents-context-tools
  `(("getCurrentSelection" lispm-agents-context--tool-selection nil nil
     "Get the text the user has selected in Emacs, or their cursor position, \
in the file they were last in.  Call it when the user refers to what they \
are looking at or have selected.")
    ("getLatestSelection" lispm-agents-context--tool-selection nil nil
     "Get the latest text selection in any file.")
    ("getOpenEditors" lispm-agents-context--tool-open-editors nil nil
     "List the files open in Emacs, marking the active one and those with \
unsaved changes.")
    ("getWorkspaceFolders" lispm-agents-context--tool-workspace-folders nil nil
     "Get the workspace folders.")
    ("getDiagnostics" lispm-agents-context--tool-diagnostics ,lispm-agents-context--uri-schema nil
     "Get the language server's diagnostics (errors, warnings) Emacs holds \
for one open file, or for every open file when no URI is given.")
    ("checkDocumentDirty" lispm-agents-context--tool-dirty ,lispm-agents-context--uri-schema ["uri"]
     "Check whether a file has unsaved changes in Emacs.  Check before \
editing a file the user has open.")
    ("openFile" lispm-agents-context--tool-open-file
     ((filePath . ((type . "string") (description . "Path of the file to open")))
      (startText . ((type . "string") (description . "Text to place the cursor at"))))
     ["filePath"]
     "Show a file to the user in Emacs."))
  "Read tools: (NAME FUNCTION PROPERTIES REQUIRED DESCRIPTION).
FUNCTION takes the agent's directory and the arguments and returns the
text items of its result.")

(defun lispm-agents-context-tools (directory-of &optional names)
  "Return the read tools named NAMES, or all, for servers of DIRECTORY-OF.
DIRECTORY-OF maps a server to its directory.  The tools take the form
servers dispatch: their function takes the server, the arguments and the
request id."
  (mapcar (pcase-lambda (`(,name ,function . ,description))
            `(,name ,(lambda (server arguments _id)
                       (funcall function (funcall directory-of server) arguments))
                    ,@description))
          (if names
              (seq-filter (lambda (tool) (member (car tool) names)) lispm-agents-context-tools)
            lispm-agents-context-tools)))

;;;; MCP

(defun lispm-agents-context-content (texts)
  "Return TEXTS as MCP text content items."
  (vconcat (mapcar (lambda (text) `((type . "text") (text . ,text))) texts)))

(defun lispm-agents-context-result (id result)
  "Return the JSON-RPC response to request ID carrying RESULT."
  `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(defun lispm-agents-context-tool-result (id texts &optional errorp)
  "Return the response to the tool call ID whose result is TEXTS.
ERRORP, non-nil, marks the call failed."
  (lispm-agents-context-result id `((content . ,(lispm-agents-context-content texts))
                                    ,@(when errorp '((isError . t))))))

(defun lispm-agents-context-error (id code text)
  "Return the JSON-RPC error response to request ID with CODE and TEXT."
  `((jsonrpc . "2.0") (id . ,id) (error . ((code . ,code) (message . ,text)))))

(defun lispm-agents-context-parse (text)
  "Return the JSON-RPC message TEXT as an alist, or nil when malformed."
  (ignore-errors
    (json-parse-string text :object-type 'alist :null-object nil :false-object nil)))

(defun lispm-agents-context-handle (server tools message)
  "Answer the MCP MESSAGE, an alist, for SERVER offering TOOLS.
Return the JSON-RPC response; nil for a notification or a method the
caller handles; or `deferred' when a tool answers later itself."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (condition-case err
        (pcase method
          ("initialize"
           (lispm-agents-context-result
            id `((protocolVersion . ,(or (alist-get 'protocolVersion params) "2025-06-18"))
                 (capabilities . ((tools . ((listChanged . :false)))))
                 (serverInfo . ((name . "Emacs") (version . ,emacs-version)))
                 ,@(when-let* ((instructions (lispm-agents-context-instructions server)))
                     `((instructions . ,instructions))))))
          ("tools/list"
           (lispm-agents-context-result
            id `((tools . ,(vconcat
                            (mapcar (pcase-lambda (`(,name ,_ ,properties ,required ,description))
                                      `((name . ,name)
                                        (description . ,description)
                                        (inputSchema
                                         . ((type . "object")
                                            (properties . ,(or properties (make-hash-table)))
                                            ,@(when required `((required . ,required)))))))
                                    tools))))))
          ("tools/call"
           (let ((tool (assoc (alist-get 'name params) tools)))
             (if (not tool)
                 (lispm-agents-context-error id -32602
                                             (format "Unknown tool: %s" (alist-get 'name params)))
               (let ((result (funcall (nth 1 tool) server (alist-get 'arguments params) id)))
                 (if (eq result 'deferred)
                     'deferred
                   (lispm-agents-context-tool-result id result))))))
          ("ping" (lispm-agents-context-result id (make-hash-table)))
          (_ (when (and id method)
               (lispm-agents-context-error id -32601 (format "Method not found: %s" method)))))
      (error (when id
               (lispm-agents-context-error id -32603 (error-message-string err)))))))

(provide 'lispm-agents-context)
;;; lispm-agents-context.el ends here
