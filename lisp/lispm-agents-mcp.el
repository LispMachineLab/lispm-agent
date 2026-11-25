;;; lispm-agents-mcp.el --- Emacs state as an MCP server for agent terminals -*- lexical-binding: t; -*-

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

;; Agents without an editor protocol of their own, as Codex and Hermes,
;; call MCP servers.  Each of their terminals gets a streamable HTTP MCP
;; server of its own, offering the read tools of `lispm-agents-context':
;;
;;   launch --> server on a free port, with a token of its own
;;   agent --> POST /mcp with "Authorization: Bearer TOKEN"
;;
;; How an agent is pointed at its server is the agent's: a type including
;; `lispm-agents-mcp' says it with its methods of
;; `lispm-agents-context-environment' and
;; `lispm-agents-context-arguments', see lispm-agents-codex.el.
;; The server answers each request with its JSON response, at once or,
;; for a tool that takes a while, once it is done; it opens no event
;; stream.  Besides the read tools, it serves `lispm-agents-mcp-extra-tools',
;; LispM's when LispM runs (lispm-agents-bridge.el).
;;
;; An agent with a shell but no MCP client, as Holon, calls the same tools
;; with the command bin/lispm-tool, which speaks plain HTTP to its server:
;;
;;   GET /tools         what the agent is told of the server, and its tools
;;   POST /call/NAME    the tool NAME, on the JSON object of the body; the
;;                      result comes back as text, with 422 if it failed
;;
;; The command finds its server by its environment.  The agent's package
;; names the server with `lispm-agents-mcp-announce': a file of
;; `lispm-agents-mcp-peer-directory', readable by its owner only, named
;; after a variable the agent's commands have and its value, as
;; HOLON_AGENT_ID=demo, holds the server's address and token.  An agent
;; LispM did not start has no such server: it reaches the one LispM runs
;; for agents outside it, see lispm-mcp-server.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-local-http)
(require 'lispm-agents-context)

(cl-defstruct (lispm-agents-mcp (:constructor nil))
  "The MCP server of one agent terminal; DIRECTORY bounds what it shows.
CLOSED is non-nil once stopped; PENDING holds in-flight answer callbacks."
  server port token directory closed pending)

(defun lispm-agents-mcp-start (name make directory)
  "Serve MCP to an agent terminal running in DIRECTORY; return the server,
to bind with `lispm-agents-context-bind'.  MAKE is the constructor
of a type including `lispm-agents-mcp', taking :token and
:directory; NAME names the server's process."
  (let ((server (funcall make
                         :token (lispm-agents-uuid)
                         :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lispm-agents-mcp-server server)
          (lispm-local-http-serve
           name
           (lambda (headers)
             (and (not (assoc "origin" headers))
                  (equal (cdr (assoc "authorization" headers))
                         (concat "Bearer " (lispm-agents-mcp-token server)))))
           (lambda (method path _headers body)
             (lispm-agents-mcp-respond server method path body))))
    (setf (lispm-agents-mcp-port server) (process-contact (lispm-agents-mcp-server server) :service))
    server))

(cl-defmethod lispm-agents-context-stop ((server lispm-agents-mcp))
  "Stop SERVER, settle pending callbacks, and forget announced names."
  (setf (lispm-agents-mcp-closed server) t)
  (let ((callbacks (lispm-agents-mcp-pending server)))
    (setf (lispm-agents-mcp-pending server) nil)
    (dolist (callback callbacks)
      (condition-case nil
          (funcall callback '("Cancelled: the session has ended.") t)
        ((error quit) nil))))
  (lispm-agents-mcp--unannounce server)
  (when-let* ((proc (lispm-agents-mcp-server server)))
    (when (processp proc)
      (delete-process proc))))

;;;; Servers named to bin/lispm-tool

(defconst lispm-agents-mcp-tool-program
  (expand-file-name "../bin/lispm-tool" (file-name-directory (or load-file-name buffer-file-name)))
  "The command calling the tools of an agent's server, for an agent with a
shell but no MCP client; see `lispm-agents-mcp-announce'.")

(defvar lispm-agents-mcp-peer-directory "~/.lispm/peers/"
  "Directory of the files naming agents' servers to bin/lispm-tool, which
looks in $LISPM_PEERS instead when it is set.")

(defvar lispm-agents-mcp--announced nil
  "Files naming live servers to bin/lispm-tool, as (SERVER . FILE).")

(defun lispm-agents-mcp-announce (server variable value)
  "Have bin/lispm-tool reach SERVER from a command whose environment sets
VARIABLE to VALUE, as the commands of the agent it serves do.
The file VARIABLE=VALUE of `lispm-agents-mcp-peer-directory', readable
by its owner only, holds SERVER's address and token; it is deleted when
SERVER stops.  Return the file."
  (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" variable)
    (error "Not the name of an environment variable: %s" variable))
  (unless (string-match-p "\\`[A-Za-z0-9._:@+-]+\\'" value)
    (error "Not a value bin/lispm-tool can find: %s" value))
  (let* ((directory (file-name-as-directory (expand-file-name lispm-agents-mcp-peer-directory)))
         (file (expand-file-name (concat variable "=" value) directory))
         temporary)
    (with-file-modes #o700
      (make-directory directory t))
    (setq temporary (make-temp-file (concat file ".")))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (write-region (format "http://127.0.0.1:%d\n%s\n"
                                (lispm-agents-mcp-port server) (lispm-agents-mcp-token server))
                        nil temporary nil 'silent)
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary)) (delete-file temporary)))
    (push (cons server file) lispm-agents-mcp--announced)
    file))

(defun lispm-agents-mcp--unannounce (server)
  "Delete the files naming SERVER to bin/lispm-tool."
  (dolist (entry lispm-agents-mcp--announced)
    (when (eq (car entry) server)
      ;; Another server may have been announced by the same name since.
      (unless (seq-some (lambda (other) (and (not (eq (car other) server))
                                             (equal (cdr other) (cdr entry))))
                        lispm-agents-mcp--announced)
        (ignore-errors (delete-file (cdr entry))))))
  (setq lispm-agents-mcp--announced
        (seq-remove (lambda (entry) (eq (car entry) server)) lispm-agents-mcp--announced)))

(defun lispm-agents-mcp-bypass-proxy (variable)
  "Return the entry of the proxy exclusion VARIABLE with loopback added.
An agent's HTTP client that follows the system proxy otherwise sends
it the requests to these servers too, which it refuses."
  (format "%s=%s" variable
          (string-join (delete-dups (append (split-string (or (getenv variable) "") "," t)
                                            '("127.0.0.1")))
                       ",")))

;;;; Requests

(defconst lispm-agents-mcp-tools
  (lispm-agents-context-tools #'lispm-agents-mcp-directory
                              '("getCurrentSelection" "getOpenEditors" "getDiagnostics"
                                "checkDocumentDirty" "openFile"))
  "The read tools served to the agents.")

(defvar lispm-agents-mcp-extra-tools nil
  "Tools served to the agents besides the read tools.
A list of `lispm-tool-spec' structs whose handler functions take the
server, the arguments and the request id.  LispM adds its own when it
runs; see lispm-agents-bridge.el.")

(cl-defgeneric lispm-agents-mcp-served-tools (server)
  "Return the tools SERVER serves: the read tools and the extra ones."
  (ignore server)
  (append lispm-agents-mcp-tools lispm-agents-mcp-extra-tools))

(defun lispm-agents-mcp--text (status text)
  "The response of STATUS carrying TEXT, as plain text."
  (list status (encode-coding-string text 'utf-8) "text/plain; charset=utf-8"))

(defun lispm-agents-mcp--tools-text (server)
  "What GET /tools tells SERVER's agent: what it is told of SERVER, and the
tools, each with its input's schema."
  (concat (if-let* ((instructions (lispm-agents-context-instructions server)))
              (concat instructions "\n\n")
            "")
          "Tools:\n"
          (mapconcat (lambda (entry)
                       (let ((spec (lispm-tool-spec-normalize entry)))
                         (format "- %s: %s\n  input: %s"
                                 (lispm-tool-spec-name spec)
                                 (lispm-tool-spec-resolved-description spec)
                                 (json-serialize (lispm-tool-spec-resolved-schema spec)))))
                     (lispm-agents-mcp-served-tools server) "\n")
          "\n"))

(defun lispm-agents-mcp--call (server name body)
  "Answer POST /call/NAME for SERVER: call the tool NAME on the arguments
of the JSON object BODY, empty for none; return its result as text."
  (let ((arguments (if (string-blank-p body)
                       nil
                     (condition-case nil
                         (json-parse-string body :object-type 'alist :null-object nil :false-object :false)
                       (error 'malformed))))
        (tool (lispm-tool-spec-find name (lispm-agents-mcp-served-tools server)))
        (respond lispm-local-http-respond)
        (text (lambda (texts) (concat (string-join texts "\n") "\n"))))
    (cond
     ((not tool) (lispm-agents-mcp--text "404 Not Found" (format "There is no tool %s.\n" name)))
     ((not (and (listp arguments) (not (eq arguments 'malformed))))
      (lispm-agents-mcp--text "400 Bad Request" "The input is not a JSON object.\n"))
     (t
      (let* ((lispm-agents-context-answer
              (lambda (texts &optional errorp)
                (funcall respond (lispm-agents-mcp--text
                                  (if errorp "422 Unprocessable Entity" "200 OK")
                                  (funcall text texts)))))
             (failed nil)
             (result (condition-case err
                         (funcall (lispm-tool-spec-function tool) server arguments nil)
                       (error (setq failed t)
                              (list (error-message-string err))))))
        (if (eq result 'deferred)
            'deferred
          (lispm-agents-mcp--text (if failed "422 Unprocessable Entity" "200 OK")
                                  (funcall text result))))))))

(defun lispm-agents-mcp-respond (server method path body)
  "Answer the HTTP request METHOD PATH carrying the JSON-RPC BODY for SERVER.
A request gets its response, or `deferred' when a tool answers later; a
notification is acknowledged without one.  GET /tools and POST
/call/NAME answer bin/lispm-tool in plain text."
  (let ((message (and (equal method "POST") (lispm-agents-context-parse body)))
        (path (car (split-string path "?"))))
    (cond
     ((equal path "/tools")
      (if (equal method "GET")
          (lispm-agents-mcp--text "200 OK" (lispm-agents-mcp--tools-text server))
        '("405 Method Not Allowed")))
     ((string-prefix-p "/call/" path)
      (if (equal method "POST")
          (lispm-agents-mcp--call server (substring path (length "/call/")) body)
        '("405 Method Not Allowed")))
     ((not (equal path "/mcp")) '("404 Not Found"))
     ((not (equal method "POST")) '("405 Method Not Allowed"))
     ((not (and message (listp message))) '("400 Bad Request"))
     (t (let* ((respond lispm-local-http-respond)
               (id (alist-get 'id message))
               (lispm-agents-context-answer
                (lambda (texts &optional errorp)
                  (funcall respond
                           (list "200 OK" (json-serialize
                                           (lispm-agents-context-tool-result id texts errorp))))))
               (response (lispm-agents-context-handle
                          server (lispm-agents-mcp-served-tools server) message)))
          (cond ((eq response 'deferred) 'deferred)
                ((consp response) (list "200 OK" (json-serialize response)))
                (t '("202 Accepted"))))))))

(provide 'lispm-agents-mcp)
;;; lispm-agents-mcp.el ends here
