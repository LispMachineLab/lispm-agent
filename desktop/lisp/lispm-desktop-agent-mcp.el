;;; lispm-desktop-agent-mcp.el --- Emacs state as an MCP server for Codex and Hermes -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
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

;; Codex and Hermes have no editor protocol, but they call MCP servers.
;; Each of their terminals gets a streamable HTTP MCP server of its own,
;; offering the read tools of `lispm-desktop-agent-context':
;;
;;   launch --> server on a free port
;;   codex -c mcp_servers.emacs.url=... -c ...bearer_token_env_var=VAR
;;         --> POST /mcp with "Authorization: Bearer TOKEN"
;;   hermes, with LISPM_DESKTOP_EMACS_MCP_URL and LISPM_DESKTOP_EMACS_MCP_TOKEN_FILE set
;;         --> bin/emacs-mcp, the stdio server named once in Hermes's
;;             config (`lispm-desktop-agent-hermes-setup') --> POST /mcp likewise
;;
;; Codex's token reaches it in an environment variable that no file holds
;; and that Codex is told to withhold from the commands it runs.  Hermes
;; takes MCP servers from its config file alone and passes its environment
;; on to the commands it runs, so its token is written to a file readable
;; by its owner only, and only the file's name travels in the environment.
;; The server answers each request with its JSON response; it opens no
;; event stream.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-local-http)
(require 'lispm-desktop-agent-context)

(defconst lispm-desktop-agent-mcp-token-variable "LISPM_DESKTOP_EMACS_MCP_TOKEN"
  "Environment variable that hands Codex the server's token.")

(defconst lispm-desktop-agent-mcp-token-directory
  (expand-file-name "lispm-desktop-agent-mcp/" temporary-file-directory)
  "Directory of the token files written for Hermes terminals.
It lies under the user's own temporary directory, which others cannot
enter.")

(defconst lispm-desktop-agent-mcp-hermes-bridge
  (expand-file-name "bin/emacs-mcp" lispm-desktop-directory)
  "The stdio MCP server Hermes runs, which forwards to a terminal's server.")

(defconst lispm-desktop-agent-mcp-hermes-config-file (expand-file-name "~/.hermes/config.yaml")
  "Hermes's configuration file, the only place it takes MCP servers from.")

(cl-defstruct (lispm-desktop-agent-mcp (:constructor nil))
  "The MCP server of one agent terminal; DIRECTORY bounds what it shows."
  server port token directory)

(cl-defstruct (lispm-desktop-agent-mcp-codex (:include lispm-desktop-agent-mcp)
                                             (:constructor lispm-desktop-agent-mcp-codex--make))
  "The MCP server of a Codex terminal.")

(cl-defstruct (lispm-desktop-agent-mcp-hermes (:include lispm-desktop-agent-mcp)
                                              (:constructor lispm-desktop-agent-mcp-hermes--make))
  "The MCP server of a Hermes terminal; TOKEN-FILE holds its token.")

(defun lispm-desktop-agent-mcp-start (agent directory)
  "Serve MCP to a terminal of AGENT, `codex' or `hermes', running in DIRECTORY.
Return the server, to bind with `lispm-desktop-agent-context-bind'."
  (let ((server (funcall (pcase agent
                           ('codex #'lispm-desktop-agent-mcp-codex--make)
                           ('hermes #'lispm-desktop-agent-mcp-hermes--make))
                         :token (car (process-lines "uuidgen"))
                         :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lispm-desktop-agent-mcp-server server)
          (lispm-desktop-local-http-serve
           (format "%s-mcp" agent)
           (lambda (headers)
             (equal (cdr (assoc "authorization" headers))
                    (concat "Bearer " (lispm-desktop-agent-mcp-token server))))
           (lambda (method path _headers body)
             (lispm-desktop-agent-mcp--respond server method path body))))
    (setf (lispm-desktop-agent-mcp-port server) (process-contact (lispm-desktop-agent-mcp-server server) :service))
    (when (lispm-desktop-agent-mcp-hermes-p server)
      (lispm-desktop-agent-mcp--write-token-file server))
    server))

(cl-defmethod lispm-desktop-agent-context-stop ((server lispm-desktop-agent-mcp))
  "Stop SERVER."
  (delete-process (lispm-desktop-agent-mcp-server server)))

;;;; Codex

(defun lispm-desktop-agent-mcp--bypass-proxy (variable)
  "Return the entry of the proxy exclusion VARIABLE with loopback added.
Codex's HTTP client follows the system proxy, which otherwise receives
the requests to this server too and refuses them."
  (format "%s=%s" variable
          (string-join (delete-dups (append (split-string (or (getenv variable) "") "," t)
                                            '("127.0.0.1")))
                       ",")))

(cl-defmethod lispm-desktop-agent-context-environment ((server lispm-desktop-agent-mcp-codex))
  "Hand Codex SERVER's token, and reach SERVER without a proxy."
  (list (format "%s=%s" lispm-desktop-agent-mcp-token-variable (lispm-desktop-agent-mcp-token server))
        (lispm-desktop-agent-mcp--bypass-proxy "NO_PROXY")
        (lispm-desktop-agent-mcp--bypass-proxy "no_proxy")))

(cl-defmethod lispm-desktop-agent-context-arguments ((server lispm-desktop-agent-mcp-codex))
  "Configure SERVER as Codex's MCP server `emacs' for this run only.
Codex passes its whole environment to the commands it runs unless told
otherwise, so the token variable is excluded from them.  The exclusion
replaces any `shell_environment_policy.exclude' of config.toml."
  (list "-c" (format "mcp_servers.emacs.url=\"http://127.0.0.1:%d/mcp\""
                     (lispm-desktop-agent-mcp-port server))
        "-c" (format "mcp_servers.emacs.bearer_token_env_var=\"%s\""
                     lispm-desktop-agent-mcp-token-variable)
        "-c" (format "shell_environment_policy.exclude=[\"%s\"]"
                     lispm-desktop-agent-mcp-token-variable)))

;;;; Hermes

(defun lispm-desktop-agent-mcp--token-file (server)
  "Return the file holding SERVER's token for Hermes's bridge."
  (expand-file-name (format "%d.token" (lispm-desktop-agent-mcp-port server))
                    lispm-desktop-agent-mcp-token-directory))

(defun lispm-desktop-agent-mcp--write-token-file (server)
  "Write SERVER's token to a file only its owner can read.
A stale file from a crash is replaced rather than reused with whatever
permissions it had."
  (let ((file (lispm-desktop-agent-mcp--token-file server)))
    (with-file-modes #o700
      (make-directory lispm-desktop-agent-mcp-token-directory t))
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region (lispm-desktop-agent-mcp-token server) nil file nil 'silent))))

(cl-defmethod lispm-desktop-agent-context-environment ((server lispm-desktop-agent-mcp-hermes))
  "Point Hermes's bridge at SERVER and its token file.
The bridge reaches loopback without a proxy on its own."
  (list (format "LISPM_DESKTOP_EMACS_MCP_URL=http://127.0.0.1:%d/mcp" (lispm-desktop-agent-mcp-port server))
        (format "LISPM_DESKTOP_EMACS_MCP_TOKEN_FILE=%s" (lispm-desktop-agent-mcp--token-file server))))

(cl-defmethod lispm-desktop-agent-context-stop ((server lispm-desktop-agent-mcp-hermes))
  "Stop SERVER and remove its token file."
  (cl-call-next-method)
  (ignore-errors (delete-file (lispm-desktop-agent-mcp--token-file server))))

(defun lispm-desktop-agent-mcp-hermes-configured-p ()
  "Return non-nil when Hermes's config names an MCP server `emacs'.
Only the key is checked: the entry is Hermes's to write, see
`lispm-desktop-agent-hermes-setup'."
  (when (file-readable-p lispm-desktop-agent-mcp-hermes-config-file)
    (with-temp-buffer
      (insert-file-contents lispm-desktop-agent-mcp-hermes-config-file)
      (goto-char (point-min))
      (when (re-search-forward "^mcp_servers:" nil t)
        (let ((end (save-excursion
                     (if (re-search-forward "^[^ \t\n#]" nil t) (match-beginning 0) (point-max)))))
          (and (re-search-forward "^[ \t]+emacs:" end t) t))))))

(defun lispm-desktop-agent-mcp-hermes-setup-arguments ()
  "Return the arguments that have Hermes add the bridge as its server `emacs'.
The environment entries are placeholders Hermes fills from its environment
at each start, so one entry serves every terminal."
  (list "mcp" "add" "emacs"
        "--command" lispm-desktop-agent-mcp-hermes-bridge
        "--env" "LISPM_DESKTOP_EMACS_MCP_URL=${LISPM_DESKTOP_EMACS_MCP_URL}"
        "LISPM_DESKTOP_EMACS_MCP_TOKEN_FILE=${LISPM_DESKTOP_EMACS_MCP_TOKEN_FILE}"))

;;;; Requests

(defconst lispm-desktop-agent-mcp-tools
  (lispm-desktop-agent-context-tools #'lispm-desktop-agent-mcp-directory
                                     '("getCurrentSelection" "getOpenEditors" "getDiagnostics"
                                       "checkDocumentDirty" "openFile"))
  "Tools served to Codex and Hermes.")

(defun lispm-desktop-agent-mcp--respond (server method path body)
  "Answer the HTTP request METHOD PATH carrying the JSON-RPC BODY for SERVER.
A request gets its response; a notification is acknowledged without one."
  (let ((message (and (equal method "POST") (lispm-desktop-agent-context-parse body))))
    (cond
     ((not (equal (car (split-string path "?")) "/mcp")) '("404 Not Found"))
     ((not (equal method "POST")) '("405 Method Not Allowed"))
     ((not (and message (listp message))) '("400 Bad Request"))
     (t (let ((response (lispm-desktop-agent-context-handle server lispm-desktop-agent-mcp-tools message)))
          (if (consp response)
              (list "200 OK" (json-serialize response))
            '("202 Accepted")))))))

(provide 'lispm-desktop-agent-mcp)
;;; lispm-desktop-agent-mcp.el ends here
