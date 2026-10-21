;;; lispm-mcp.el --- MCP servers as tools of LispM agents  -*- lexical-binding: t; -*-

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

;; Agents use the tools of Model Context Protocol servers as their own.
;; The protocol is spoken by mcp.el; this file starts the servers of
;; `lispm-mcp-servers', registers each tool of theirs as a LispM tool
;; named mcp__SERVER__TOOL, and turns results into text and images.
;;
;;   (setq lispm-mcp-servers
;;         '(("peekaboo" :command "npx" :args ("-y" "@steipete/peekaboo" "mcp")
;;            :description "See the macOS screen and use its applications.")))
;;
;; gives agents Peekaboo's tools: they see the screen and use the Mac's
;; applications (computer use).  A server's tools are deferred: an
;; agent finds the ones it needs with search_tools (see
;; `lispm-tool-sources'), which starts the server, and only the tools it
;; loads take room in its requests.  A server with :defer nil starts
;; when a session opens instead, and all its tools are always there; a
;; request waits for it while it starts.  The tools have the permission
;; class `mcp', which asks for approval by default; a server's
;; :permission gives its tools another class.
;;
;; A server started over stdio runs in the sandbox, at
;; `lispm-sandbox-level' (none by default) or at its own :sandbox
;; level.  One that has to reach what no level allows, Peekaboo using
;; the screen and other applications for one, takes :sandbox none.

;;; Code:

(require 'mcp)
(require 'lispm-core)
(require 'lispm-sandbox)
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
:description, what the server's tools are for, which the model reads
to decide when to search them; :defer, nil for a server to start with
each session and all its tools to be offered, rather than its tools
being deferred, found with search_tools (the default); and
:permission, the permission class of the server's tools, `mcp' by
default (see `lispm-default-policy'); and :sandbox, the sandbox level
a server started over stdio runs at: `none', `low', `middle' or
`high', or nil (the default) to follow `lispm-sandbox-level'."
  :type '(alist :key-type string :value-type plist))

(defcustom lispm-mcp-autostart t
  "Non-nil to start the servers with :defer nil when a session opens."
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
  (let* ((config (alist-get server lispm-mcp-servers nil nil #'equal))
         (permission (or (plist-get config :permission) 'mcp))
         (deferred (lispm-mcp--deferred-p config))
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
          :deferred deferred
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

(defun lispm-mcp--deferred-p (config)
  "Non-nil if the server CONFIG describes is off until switched on."
  (if (plist-member config :defer) (plist-get config :defer) t))

(defun lispm-mcp--running-p (server)
  "Non-nil if SERVER's process runs."
  (when-let* ((connection (gethash server mcp-server-connections)))
    (jsonrpc-running-p connection)))

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
  "Start SERVER as CONFIG describes, and take its tools when it lists them.
A server started over stdio runs in the sandbox, at its :sandbox
level or at `lispm-sandbox-level'."
  (remhash server lispm-mcp--failed)
  (puthash server (float-time) lispm-mcp--starting)
  (let ((connection (gethash server mcp-server-connections)))
    (if (and connection (jsonrpc-running-p connection))
        ;; Started already, by mcp.el's hub for instance.
        (mcp-async-list-tools connection
                              (lambda (_connection tools) (lispm-mcp--register server tools))
                              (lambda (code message)
                                (lispm-mcp--failed server (format "%s %s" code message))))
      (condition-case err
          (let* ((run (when-let* ((command (plist-get config :command)))
                        (lispm-sandbox-prepare (cons command (plist-get config :args))
                                               (plist-get config :sandbox)
                                               (format "the MCP server %s" server))))
                 (command (and run (lispm-sandbox-run-command run))))
            (apply #'mcp-connect-server server
                   :initial-callback
                   (lambda (_connection)
                     ;; A server without tools never lists any: stop waiting.
                     (run-at-time (+ mcp-server-wait-initial-time 15) nil
                                  (lambda () (remhash server lispm-mcp--starting))))
                   :tools-callback (lambda (_connection tools) (lispm-mcp--register server tools))
                   :error-callback (lambda (code message)
                                     (lispm-mcp--failed server (format "%s %s" code message)))
                   ;; The first :command and :args are the ones taken.
                   (append (and command (list :command (car command) :args (cdr command)))
                           (cl-loop for (key value) on config by #'cddr
                                    when (memq key lispm-mcp--connect-keys) append (list key value))))
            (when-let* ((run)
                        (connection (gethash server mcp-server-connections)))
              (lispm-sandbox-attach run (jsonrpc--process connection))))
        (lispm-sandbox-error
         (lispm-mcp--failed server (error-message-string err)))))))

;;;###autoload
(defun lispm-mcp-start (&optional server)
  "Start the servers of `lispm-mcp-servers' that are not running, or SERVER.
Deferred tools stay so: search_tools finds them."
  (interactive)
  (pcase-dolist (`(,name . ,config) lispm-mcp-servers)
    (when (and (or (null server) (equal server name))
               (not (lispm-mcp--live-p name)))
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
  "Start the servers with :defer nil, if `lispm-mcp-autostart' says so."
  (when lispm-mcp-autostart
    (pcase-dolist (`(,name . ,config) lispm-mcp-servers)
      (unless (or (lispm-mcp--deferred-p config)
                  (lispm-mcp--live-p name)
                  (< (- (float-time) (gethash name lispm-mcp--failed 0)) lispm-mcp--retry-after))
        (lispm-mcp--connect name config)))))

(defun lispm-mcp--activate (server callback)
  "Ready SERVER's tools, starting it if need be; then call CALLBACK.
CALLBACK gets nil when the tools are there, or what went wrong."
  (if (and (gethash server lispm-mcp--tools) (lispm-mcp--running-p server))
      (funcall callback nil)
    (let ((since (float-time)) timer)
      (unless (gethash server lispm-mcp--starting)
        (lispm-mcp-start server))
      (setq timer
            (run-at-time
             0.2 0.2
             (lambda ()
               (let ((problem
                      (cond ((gethash server lispm-mcp--starting) 'wait)
                            ((gethash server lispm-mcp--tools) nil)
                            ((>= (gethash server lispm-mcp--failed 0) since)
                             (format "The MCP server %s failed to start; see *Messages*." server))
                            (t (format "The MCP server %s has no tools." server)))))
                 (unless (eq problem 'wait)
                   (cancel-timer timer)
                   (funcall callback problem)))))))))

(defun lispm-mcp--sources ()
  "The servers whose tools are deferred, as sources of tools for search_tools."
  (cl-loop for (name . config) in lispm-mcp-servers
           when (lispm-mcp--deferred-p config)
           collect (let ((prefix (lispm-mcp--tool-name name "")))
                     (list :name name
                           :description (or (plist-get config :description)
                                            (format "the tools of the MCP server %s" name))
                           :pending (not (and (gethash name lispm-mcp--tools)
                                              (lispm-mcp--running-p name)))
                           :prepare (lambda (callback) (lispm-mcp--activate name callback))
                           :provides (lambda (tool) (string-prefix-p prefix tool))))))

(defun lispm-mcp--wait (_agent)
  "What a request waits for: MCP servers still starting, or nil.
Servers whose tools are deferred start only when searched."
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
(add-hook 'lispm-tool-sources #'lispm-mcp--sources)
(add-hook 'lispm-request-wait-functions #'lispm-mcp--wait)

(provide 'lispm-mcp)
;;; lispm-mcp.el ends here
