;;; lispm-mcp.el --- MCP servers as tools of LispM agents  -*- lexical-binding: t; -*-

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

;; Agents use the tools of Model Context Protocol servers as their own.
;; The protocol is spoken by mcp.el; this file starts the servers of
;; `lispm-mcp-servers', registers each tool of theirs as a LispM tool
;; named mcp__SERVER__TOOL, and turns results into text and images.
;;
;;   (setq lispm-mcp-servers
;;         '(("peekaboo" :command "npx" :args ("-y" "@steipete/peekaboo" "mcp"))))
;;
;; gives agents Peekaboo's tools: they see the macOS screen and use its
;; applications (computer use).  Servers start when a session opens; a
;; request waits for those still starting, so that a session has their
;; tools from its first request on.  Their tools have the permission
;; class `mcp', which asks for approval by default; a server's
;; :permission gives its tools another class.

;;; Code:

(require 'mcp)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

(defgroup lispm-mcp nil
  "MCP servers as tools of LispM agents."
  :group 'lispm
  :prefix "lispm-mcp-")

(defcustom lispm-mcp-servers nil
  "MCP servers whose tools agents can use.
An alist from a server's name to a plist describing it, as for
`mcp-hub-servers': :command and :args (and :env, a plist of
environment variables) start a server speaking over stdio; :url
\(with :token and :headers) reaches one over HTTP.  LispM adds
:permission, the permission class of the server's tools, `mcp' by
default (see `lispm-default-policy')."
  :type '(alist :key-type string :value-type plist))

(defcustom lispm-mcp-autostart t
  "Non-nil to start the servers of `lispm-mcp-servers' when a session opens."
  :type 'boolean)

(defcustom lispm-mcp-start-timeout 120
  "Seconds a request waits for MCP servers that are still starting.
The first start of a server run with npx or uvx downloads it."
  :type 'integer)

(defconst lispm-mcp--connect-keys
  '(:command :args :env :url :token :headers :roots :timeout)
  "Keys of a server's description that `mcp-connect-server' takes.")

(defconst lispm-mcp--retry-after 300
  "Seconds before a server that failed to start is started again on its own.")

(defvar lispm-mcp--starting (make-hash-table :test #'equal)
  "Servers starting: name -> when they were started.")

(defvar lispm-mcp--failed (make-hash-table :test #'equal)
  "Servers that failed to start: name -> when.")

(defvar lispm-mcp--tools (make-hash-table :test #'equal)
  "Tools registered for each server: name -> their LispM names.")

;;;; JSON

(defun lispm-mcp--json-in (value)
  "VALUE, decoded by mcp.el (plists, :json-false), as LispM decodes JSON."
  (cond ((eq value :json-false) :false)
        ((vectorp value) (vconcat (mapcar #'lispm-mcp--json-in value)))
        ((and (consp value) (keywordp (car value)))
         (cl-loop for (key val) on value by #'cddr
                  collect (cons (intern (substring (symbol-name key) 1))
                                (lispm-mcp--json-in val))))
        (t value)))

(defun lispm-mcp--json-out (value)
  "VALUE, decoded by LispM (alists, :false, :null), as mcp.el encodes JSON."
  (cond ((eq value :false) :json-false)
        ((eq value :null) :json-null)
        ((vectorp value) (vconcat (mapcar #'lispm-mcp--json-out value)))
        ((consp value)
         (mapcar (lambda (pair) (cons (car pair) (lispm-mcp--json-out (cdr pair)))) value))
        (t value)))

;;;; Tools

(defun lispm-mcp--tool-name (server tool)
  "The LispM name of SERVER's TOOL: mcp__SERVER__TOOL, as APIs allow names."
  (let ((name (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" (format "mcp__%s__%s" server tool))))
    (substring name 0 (min 64 (length name)))))

(defun lispm-mcp--register (server tools)
  "Make SERVER's TOOLS, as listed by the server, tools of LispM agents."
  (lispm-mcp--unregister server)
  (let ((permission (or (plist-get (alist-get server lispm-mcp-servers nil nil #'equal)
                                   :permission)
                        'mcp))
        names)
    (seq-doseq (tool tools)
      (let* ((tool-name (plist-get tool :name))
             (name (lispm-mcp--tool-name server tool-name))
             (schema (lispm-mcp--json-in (or (plist-get tool :inputSchema) '(:type "object")))))
        (unless (assq 'properties schema)
          (setq schema (append schema '((properties)))))
        (push name names)
        (lispm-register-tool
         (lispm-tool--create
          :name name
          :description (or (plist-get tool :description) (format "%s of %s" tool-name server))
          :input-schema schema
          :permission permission
          :async t
          :function (lambda (done input) (lispm-mcp--call server tool-name input done))))))
    (puthash server (reverse names) lispm-mcp--tools)
    (remhash server lispm-mcp--starting)
    (message "LispM: %d tools from MCP server %s" (length names) server)))

(defun lispm-mcp--unregister (server)
  "Remove SERVER's tools from those of LispM agents."
  (dolist (name (gethash server lispm-mcp--tools))
    (remhash name lispm-tools--registry))
  (remhash server lispm-mcp--tools))

(defun lispm-mcp--result (agent result)
  "The tool RESULT of an MCP server as (TEXT ERRORP IMAGES).
Images are saved in AGENT's session."
  (let (texts images)
    (seq-doseq (item (plist-get result :content))
      (pcase (plist-get item :type)
        ("text" (push (plist-get item :text) texts))
        ("image" (push (lispm-save-image agent (plist-get item :data) (plist-get item :mimeType))
                       images))
        ("resource" (let ((resource (plist-get item :resource)))
                      (push (or (plist-get resource :text)
                                (format "[resource %s]" (plist-get resource :uri)))
                            texts)))
        ("resource_link" (push (format "[resource %s]" (plist-get item :uri)) texts))
        (type (push (format "[%s content left out]" type) texts))))
    (when (and (null texts) (plist-get result :structuredContent))
      (push (lispm--json-pretty (lispm-mcp--json-in (plist-get result :structuredContent)))
            texts))
    (list (string-join (nreverse texts) "\n")
          (eq (plist-get result :isError) t)
          (nreverse images))))

(defun lispm-mcp--call (server tool input done)
  "Call SERVER's TOOL with INPUT; call DONE with the result."
  (let ((agent lispm-current-agent)
        (connection (gethash server mcp-server-connections)))
    (unless (and connection (jsonrpc-running-p connection))
      (error "The MCP server %s is not running; M-x lispm-mcp-start starts it" server))
    (mcp-async-call-tool connection tool (lispm-mcp--json-out input)
                         (lambda (result)
                           (apply done (condition-case err
                                           (lispm-mcp--result agent result)
                                         (error (list (format "Unreadable result: %s"
                                                              (error-message-string err))
                                                      t)))))
                         (lambda (code message)
                           (funcall done (format "MCP error %s: %s" code message) t)))))

;;;; Servers

(defun lispm-mcp--live-p (server)
  "Non-nil if SERVER is running or starting."
  (or (gethash server lispm-mcp--starting)
      (when-let* ((connection (gethash server mcp-server-connections)))
        (and (jsonrpc-running-p connection) (gethash server lispm-mcp--tools)))))

(defun lispm-mcp--failed (server why)
  "Note that SERVER failed to start, because of WHY."
  (remhash server lispm-mcp--starting)
  (puthash server (float-time) lispm-mcp--failed)
  (message "LispM: MCP server %s failed to start: %s" server why))

(defun lispm-mcp--connect (server config)
  "Start SERVER as CONFIG describes, and take its tools when it lists them."
  (remhash server lispm-mcp--failed)
  (puthash server (float-time) lispm-mcp--starting)
  (let ((connection (gethash server mcp-server-connections)))
    (if (and connection (jsonrpc-running-p connection))
        ;; Started already, by mcp.el's hub for instance.
        (mcp-async-list-tools connection
                              (lambda (_connection tools) (lispm-mcp--register server tools))
                              (lambda (code message)
                                (lispm-mcp--failed server (format "%s %s" code message))))
      (apply #'mcp-connect-server server
             :initial-callback
             (lambda (_connection)
               ;; A server without tools never lists any: stop waiting.
               (run-at-time (+ mcp-server-wait-initial-time 15) nil
                            (lambda () (remhash server lispm-mcp--starting))))
             :tools-callback (lambda (_connection tools) (lispm-mcp--register server tools))
             :error-callback (lambda (code message)
                               (lispm-mcp--failed server (format "%s %s" code message)))
             (cl-loop for (key value) on config by #'cddr
                      when (memq key lispm-mcp--connect-keys) append (list key value))))))

;;;###autoload
(defun lispm-mcp-start (&optional server)
  "Start the servers of `lispm-mcp-servers' not running, or only SERVER.
Interactively, start them all; a server that failed is tried again."
  (interactive)
  (pcase-dolist (`(,name . ,config) lispm-mcp-servers)
    (when (and (or (null server) (equal server name))
               (not (lispm-mcp--live-p name))
               (or (called-interactively-p 'any) server
                   (> (- (float-time) (gethash name lispm-mcp--failed 0)) lispm-mcp--retry-after)))
      (lispm-mcp--connect name config))))

(defun lispm-mcp-stop (&optional server)
  "Stop the servers of `lispm-mcp-servers', or only SERVER; drop their tools."
  (interactive)
  (pcase-dolist (`(,name . ,_) lispm-mcp-servers)
    (when (or (null server) (equal server name))
      (lispm-mcp--unregister name)
      (remhash name lispm-mcp--starting)
      (when (gethash name mcp-server-connections)
        (mcp-stop-server name)))))

(defun lispm-mcp-status ()
  "Say which MCP servers run and how many tools each gives."
  (interactive)
  (message "%s"
           (if lispm-mcp-servers
               (mapconcat (lambda (entry)
                            (let ((name (car entry)))
                              (format "%s: %s" name
                                      (cond ((gethash name lispm-mcp--starting) "starting")
                                            ((gethash name lispm-mcp--tools)
                                             (format "%d tools" (length (gethash name lispm-mcp--tools))))
                                            ((gethash name lispm-mcp--failed) "failed")
                                            (t "stopped")))))
                          lispm-mcp-servers "; ")
             "No MCP servers: see `lispm-mcp-servers'")))

(defun lispm-mcp--autostart ()
  "Start the MCP servers, if they are to start on their own."
  (when lispm-mcp-autostart
    (lispm-mcp-start)))

(defun lispm-mcp--wait (_agent)
  "What a request waits for: MCP servers still starting, or nil."
  (lispm-mcp--autostart)
  (let ((now (float-time)) waiting late)
    (maphash (lambda (name started)
               (if (> (- now started) lispm-mcp-start-timeout) (push name late) (push name waiting)))
             lispm-mcp--starting)
    (dolist (name late)
      (lispm-mcp--failed name (format "no tools within %d seconds" lispm-mcp-start-timeout)))
    (and waiting (format "waiting for MCP server%s %s"
                         (if (cdr waiting) "s" "") (string-join waiting ", ")))))

(add-hook 'lispm-session-mode-hook #'lispm-mcp--autostart)
(add-hook 'lispm-request-wait-functions #'lispm-mcp--wait)

(provide 'lispm-mcp)
;;; lispm-mcp.el ends here
