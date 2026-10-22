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
;; The server answers each request with its JSON response; it opens no
;; event stream.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-local-http)
(require 'lispm-agents-context)

(cl-defstruct (lispm-agents-mcp (:constructor nil))
  "The MCP server of one agent terminal; DIRECTORY bounds what it shows."
  server port token directory)

(defun lispm-agents-mcp-start (name make directory)
  "Serve MCP to an agent terminal running in DIRECTORY; return the server,
to bind with `lispm-agents-context-bind'.  MAKE is the constructor
of a type including `lispm-agents-mcp', taking :token and
:directory; NAME names the server's process."
  (let ((server (funcall make
                         :token (car (process-lines "uuidgen"))
                         :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lispm-agents-mcp-server server)
          (lispm-local-http-serve
           name
           (lambda (headers)
             (equal (cdr (assoc "authorization" headers))
                    (concat "Bearer " (lispm-agents-mcp-token server))))
           (lambda (method path _headers body)
             (lispm-agents-mcp--respond server method path body))))
    (setf (lispm-agents-mcp-port server) (process-contact (lispm-agents-mcp-server server) :service))
    server))

(cl-defmethod lispm-agents-context-stop ((server lispm-agents-mcp))
  "Stop SERVER."
  (delete-process (lispm-agents-mcp-server server)))

;;;; Requests

(defconst lispm-agents-mcp-tools
  (lispm-agents-context-tools #'lispm-agents-mcp-directory
                              '("getCurrentSelection" "getOpenEditors" "getDiagnostics"
                                "checkDocumentDirty" "openFile"))
  "Tools served to the agents.")

(defun lispm-agents-mcp--respond (server method path body)
  "Answer the HTTP request METHOD PATH carrying the JSON-RPC BODY for SERVER.
A request gets its response; a notification is acknowledged without one."
  (let ((message (and (equal method "POST") (lispm-agents-context-parse body))))
    (cond
     ((not (equal (car (split-string path "?")) "/mcp")) '("404 Not Found"))
     ((not (equal method "POST")) '("405 Method Not Allowed"))
     ((not (and message (listp message))) '("400 Bad Request"))
     (t (let ((response (lispm-agents-context-handle server lispm-agents-mcp-tools message)))
          (if (consp response)
              (list "200 OK" (json-serialize response))
            '("202 Accepted")))))))

(provide 'lispm-agents-mcp)
;;; lispm-agents-mcp.el ends here
