;;; lme-agent-context.el --- Emacs state served to coding agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm-emacs
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; What an agent cannot read from disk or Git exists only in Emacs: the
;; selection and cursor, which files are open and unsaved, and Flymake's
;; diagnostics.  This module serves it as MCP tools to the agents' own
;; servers, one per agent terminal:
;;
;;   lme-agent-ide  Claude Code's IDE protocol, over a WebSocket
;;   lme-agent-mcp  plain MCP over HTTP, for Codex and Hermes
;;
;;   terminal --launch--> server --bind--> stops with its terminal
;;   request --`lme-agent-context-handle'--> tool (directory, arguments)
;;
;; Every tool is read-only, and an agent sees only files under the
;; directory its terminal runs in.

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

(cl-defgeneric lme-agent-context-environment (server)
  "Return the environment entries that point an agent at SERVER."
  (ignore server))

(cl-defgeneric lme-agent-context-arguments (server)
  "Return the command-line arguments that point an agent at SERVER."
  (ignore server))

(cl-defgeneric lme-agent-context-stop (server)
  "Stop SERVER and release what it holds.")

(defvar lme-agent-context--servers nil
  "Live servers, as (TERMINAL . SERVER).")

(defvar-local lme-agent-context-server nil
  "The server bound to this agent terminal.")
(put 'lme-agent-context-server 'permanent-local t)

(defun lme-agent-context-bind (server terminal)
  "Bind SERVER to the agent TERMINAL buffer: it stops when the terminal's
process exits, when the terminal is killed, or when Emacs exits."
  (push (cons terminal server) lme-agent-context--servers)
  (with-current-buffer terminal
    (setq lme-agent-context-server server)
    (add-hook 'kill-buffer-hook #'lme-agent-context--terminal-killed nil t))
  (add-hook 'eat-exit-hook #'lme-agent-context--terminal-exited)
  (add-hook 'lme-term-exit-functions #'lme-agent-context--term-exited)
  (add-hook 'kill-emacs-hook #'lme-agent-context-release-all)
  (add-hook 'post-command-hook #'lme-agent-context--note-selection))

(defun lme-agent-context-release (terminal)
  "Stop the server bound to TERMINAL, if any."
  (when-let* ((entry (assq terminal lme-agent-context--servers)))
    (setq lme-agent-context--servers (delq entry lme-agent-context--servers))
    (when (buffer-live-p terminal)
      (with-current-buffer terminal (setq lme-agent-context-server nil)))
    (unless lme-agent-context--servers
      (remove-hook 'post-command-hook #'lme-agent-context--note-selection))
    (lme-agent-context-stop (cdr entry))))

(defun lme-agent-context-release-all ()
  "Stop every agent server."
  (mapc #'lme-agent-context-release (mapcar #'car lme-agent-context--servers)))

(defun lme-agent-context--terminal-killed ()
  "Stop the server of the terminal being killed."
  (lme-agent-context-release (current-buffer)))

(defun lme-agent-context--terminal-exited (process)
  "Stop the server of the Eat terminal whose PROCESS exited."
  (lme-agent-context-release (process-buffer process)))

(defun lme-agent-context--term-exited (buffer &rest _)
  "Stop the server of the lme-term BUFFER, whose program exited."
  (lme-agent-context-release buffer))

;;;; Scope

(defun lme-agent-context-path (uri)
  "Return the absolute file name of URI, a file URI or a path."
  (require 'url-util)
  (expand-file-name (if (string-prefix-p "file://" uri)
                        (url-unhex-string (substring uri (length "file://")))
                      uri)))

(defun lme-agent-context-visible-p (directory file)
  "Return non-nil when FILE lies under DIRECTORY."
  (and file (file-in-directory-p file directory)))

(defun lme-agent-context--buffers (directory)
  "Return the buffers visiting files under DIRECTORY."
  (seq-filter (lambda (buffer)
                (lme-agent-context-visible-p directory (buffer-file-name buffer)))
              (buffer-list)))

(defun lme-agent-context--position (position)
  "Return POSITION of the current buffer as a zero-based line and character."
  (save-excursion
    (goto-char position)
    `((line . ,(1- (line-number-at-pos position t)))
      (character . ,(- position (line-beginning-position))))))

;;;; Selection

(defvar lme-agent-context-selection nil
  "The latest selection in a file, as the IDE protocol describes it.")

(defvar lme-agent-context-selection-functions nil
  "Functions called with no argument after `lme-agent-context-selection' changed.")

(defvar lme-agent-context--selection-key nil
  "What the latest selection was computed from: buffer, point and mark.")

(defvar lme-agent-context--selection-timer nil
  "Timer that records the selection once the user pauses.")

(defun lme-agent-context--region ()
  "Return the selected region of the current buffer as (START . END), or nil.
In Evil's visual state the region is Evil's, which spans whole lines in
line selection."
  (cond ((and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
         (cons (marker-position evil-visual-beginning) (marker-position evil-visual-end)))
        ((use-region-p) (cons (region-beginning) (region-end)))))

(defun lme-agent-context--current-selection ()
  "Return the selection, or the cursor, of the current file buffer."
  (let* ((region (lme-agent-context--region))
         (start (or (car region) (point)))
         (end (or (cdr region) (point))))
    `((text . ,(if region (buffer-substring-no-properties start end) ""))
      (filePath . ,buffer-file-name)
      (fileUrl . ,(concat "file://" buffer-file-name))
      (selection . ((start . ,(lme-agent-context--position start))
                    (end . ,(lme-agent-context--position end))
                    (isEmpty . ,(if region :false t)))))))

(defun lme-agent-context--note-selection ()
  "Record the selection soon when a command moved it in a file buffer.
Runs after every command while an agent server lives, so it only compares
point and mark; the selection itself is computed once the user pauses."
  (when buffer-file-name
    (let ((key (list (current-buffer) (point) (and (lme-agent-context--region) (mark t)))))
      (unless (equal key lme-agent-context--selection-key)
        (setq lme-agent-context--selection-key key)
        (when lme-agent-context--selection-timer
          (cancel-timer lme-agent-context--selection-timer))
        (setq lme-agent-context--selection-timer
              (run-with-idle-timer 0.1 nil #'lme-agent-context-record-selection
                                   (current-buffer)))))))

(defun lme-agent-context-record-selection (buffer)
  "Record BUFFER's selection and tell `lme-agent-context-selection-functions'."
  (setq lme-agent-context--selection-timer nil)
  (when (buffer-live-p buffer)
    (setq lme-agent-context-selection
          (with-current-buffer buffer (lme-agent-context--current-selection)))
    (run-hooks 'lme-agent-context-selection-functions)))

(defun lme-agent-context-selection (directory)
  "Return the latest selection when its file lies under DIRECTORY, else nil."
  (when (lme-agent-context-visible-p directory (alist-get 'filePath lme-agent-context-selection))
    lme-agent-context-selection))

;;;; Tools

(defun lme-agent-context--tool-selection (directory _arguments)
  "Return the latest selection in a file under DIRECTORY."
  (list (json-serialize
         (or (lme-agent-context-selection directory)
             '((text . "") (filePath . "")
               (selection . ((start . ((line . 0) (character . 0)))
                             (end . ((line . 0) (character . 0)))
                             (isEmpty . t))))))))

(defun lme-agent-context--tool-open-editors (directory _arguments)
  "Return the files open in Emacs under DIRECTORY."
  (let ((current (window-buffer (selected-window))))
    (list (json-serialize
           `((tabs . ,(vconcat
                       (mapcar (lambda (buffer)
                                 `((uri . ,(concat "file://" (buffer-file-name buffer)))
                                   (label . ,(buffer-name buffer))
                                   (isActive . ,(if (eq buffer current) t :false))
                                   (isDirty . ,(if (buffer-modified-p buffer) t :false))))
                               (lme-agent-context--buffers directory)))))))))

(defun lme-agent-context--tool-workspace-folders (directory _arguments)
  "Return DIRECTORY as the only workspace folder."
  (let ((root (directory-file-name directory)))
    (list (json-serialize
           `((success . t)
             (folders . [((name . ,(file-name-nondirectory root))
                          (uri . ,(concat "file://" root))
                          (path . ,root))])
             (rootPath . ,root))))))

(defun lme-agent-context--diagnostics (buffer)
  "Return Flymake's diagnostics of BUFFER as the IDE protocol describes them."
  (with-current-buffer buffer
    (when (bound-and-true-p flymake-mode)
      (vconcat
       (mapcar (lambda (diagnostic)
                 `((range . ((start . ,(lme-agent-context--position
                                        (flymake-diagnostic-beg diagnostic)))
                             (end . ,(lme-agent-context--position
                                      (flymake-diagnostic-end diagnostic)))))
                   (severity . ,(pcase (flymake-diagnostic-type diagnostic)
                                  ((or :error 'eglot-error) "Error")
                                  ((or :warning 'eglot-warning) "Warning")
                                  (_ "Information")))
                   (message . ,(flymake-diagnostic-text diagnostic))
                   (source . ,(format "%s" (flymake-diagnostic-backend diagnostic)))))
               (flymake-diagnostics))))))

(defun lme-agent-context--tool-diagnostics (directory arguments)
  "Return the diagnostics of the file ARGUMENTS name, or of every open file.
Only files under DIRECTORY are reported."
  (let* ((uri (alist-get 'uri arguments))
         (file (and uri (lme-agent-context-path uri)))
         (buffers (if file
                      (when-let* (((lme-agent-context-visible-p directory file))
                                  (buffer (find-buffer-visiting file)))
                        (list buffer))
                    (lme-agent-context--buffers directory))))
    (list (json-serialize
           (vconcat
            (delq nil
                  (mapcar (lambda (buffer)
                            (when-let* ((diagnostics (lme-agent-context--diagnostics buffer)))
                              `((uri . ,(concat "file://" (buffer-file-name buffer)))
                                (diagnostics . ,diagnostics))))
                          buffers)))))))

(defun lme-agent-context--tool-dirty (directory arguments)
  "Return whether the file ARGUMENTS name has unsaved changes in Emacs."
  (let* ((file (lme-agent-context-path (alist-get 'uri arguments)))
         (buffer (and (lme-agent-context-visible-p directory file)
                      (find-buffer-visiting file))))
    (list (json-serialize
           (if buffer
               `((success . t) (filePath . ,file)
                 (isDirty . ,(if (buffer-modified-p buffer) t :false))
                 (isUntitled . :false))
             `((success . :false) (message . ,(format "Document not open: %s" file))))))))

(defun lme-agent-context-display (buffer)
  "Show BUFFER in the main area's file column, keeping the selection."
  (display-buffer buffer '(lme-main-area-display-file)))

(defun lme-agent-context--tool-open-file (_directory arguments)
  "Show the file ARGUMENTS name, at its start text when given, without focus."
  (let ((file (lme-agent-context-path (or (alist-get 'filePath arguments)
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
      (lme-agent-context-display buffer)
      (list (format "Opened file: %s" file)))))

(defconst lme-agent-context--uri-schema
  '((uri . ((type . "string") (description . "The file URI or path"))))
  "Input properties of a tool that takes one file.")

(defconst lme-agent-context-tools
  `(("getCurrentSelection" lme-agent-context--tool-selection nil nil
     "Get the text the user has selected in Emacs, or their cursor position, \
in the file they were last in.  Call it when the user refers to what they \
are looking at or have selected.")
    ("getLatestSelection" lme-agent-context--tool-selection nil nil
     "Get the latest text selection in any file.")
    ("getOpenEditors" lme-agent-context--tool-open-editors nil nil
     "List the files open in Emacs, marking the active one and those with \
unsaved changes.")
    ("getWorkspaceFolders" lme-agent-context--tool-workspace-folders nil nil
     "Get the workspace folders.")
    ("getDiagnostics" lme-agent-context--tool-diagnostics ,lme-agent-context--uri-schema nil
     "Get the language server's diagnostics (errors, warnings) Emacs holds \
for one open file, or for every open file when no URI is given.")
    ("checkDocumentDirty" lme-agent-context--tool-dirty ,lme-agent-context--uri-schema ["uri"]
     "Check whether a file has unsaved changes in Emacs.  Check before \
editing a file the user has open.")
    ("openFile" lme-agent-context--tool-open-file
     ((filePath . ((type . "string") (description . "Path of the file to open")))
      (startText . ((type . "string") (description . "Text to place the cursor at"))))
     ["filePath"]
     "Show a file to the user in Emacs."))
  "Read tools: (NAME FUNCTION PROPERTIES REQUIRED DESCRIPTION).
FUNCTION takes the agent's directory and the arguments and returns the
text items of its result.")

(defun lme-agent-context-tools (directory-of &optional names)
  "Return the read tools named NAMES, or all, for servers of DIRECTORY-OF.
DIRECTORY-OF maps a server to its directory.  The tools take the form
servers dispatch: their function takes the server, the arguments and the
request id."
  (mapcar (pcase-lambda (`(,name ,function . ,description))
            `(,name ,(lambda (server arguments _id)
                       (funcall function (funcall directory-of server) arguments))
                    ,@description))
          (if names
              (seq-filter (lambda (tool) (member (car tool) names)) lme-agent-context-tools)
            lme-agent-context-tools)))

;;;; MCP

(defun lme-agent-context-content (texts)
  "Return TEXTS as MCP text content items."
  (vconcat (mapcar (lambda (text) `((type . "text") (text . ,text))) texts)))

(defun lme-agent-context-result (id result)
  "Return the JSON-RPC response to request ID carrying RESULT."
  `((jsonrpc . "2.0") (id . ,id) (result . ,result)))

(defun lme-agent-context-error (id code text)
  "Return the JSON-RPC error response to request ID with CODE and TEXT."
  `((jsonrpc . "2.0") (id . ,id) (error . ((code . ,code) (message . ,text)))))

(defun lme-agent-context-parse (text)
  "Return the JSON-RPC message TEXT as an alist, or nil when malformed."
  (ignore-errors
    (json-parse-string text :object-type 'alist :null-object nil :false-object nil)))

(defun lme-agent-context-handle (server tools message)
  "Answer the MCP MESSAGE, an alist, for SERVER offering TOOLS.
Return the JSON-RPC response; nil for a notification or a method the
caller handles; or `deferred' when a tool answers later itself."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (condition-case err
        (pcase method
          ("initialize"
           (lme-agent-context-result
            id `((protocolVersion . ,(or (alist-get 'protocolVersion params) "2025-06-18"))
                 (capabilities . ((tools . ((listChanged . :false)))))
                 (serverInfo . ((name . "Emacs") (version . ,emacs-version))))))
          ("tools/list"
           (lme-agent-context-result
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
                 (lme-agent-context-error id -32602
                                         (format "Unknown tool: %s" (alist-get 'name params)))
               (let ((result (funcall (nth 1 tool) server (alist-get 'arguments params) id)))
                 (if (eq result 'deferred)
                     'deferred
                   (lme-agent-context-result
                    id `((content . ,(lme-agent-context-content result)))))))))
          ("ping" (lme-agent-context-result id (make-hash-table)))
          (_ (when (and id method)
               (lme-agent-context-error id -32601 (format "Method not found: %s" method)))))
      (error (when id
               (lme-agent-context-error id -32603 (error-message-string err)))))))

(provide 'lme-agent-context)
;;; lme-agent-context.el ends here
