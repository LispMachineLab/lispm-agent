;;; lme-agent-mcp.el --- Emacs state as an MCP server for Codex and Hermes -*- lexical-binding: t; -*-

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

;; Codex and Hermes have no editor protocol, but they call MCP servers.
;; Each of their terminals gets a streamable HTTP MCP server of its own,
;; offering the read tools of `lme-agent-context':
;;
;;   launch --> server on a free port
;;   codex -c mcp_servers.emacs.url=... -c ...bearer_token_env_var=VAR
;;         --> POST /mcp with "Authorization: Bearer TOKEN"
;;   hermes, with LME_EMACS_MCP_URL and LME_EMACS_MCP_TOKEN_FILE set
;;         --> bin/emacs-mcp, the stdio server named once in Hermes's
;;             config (`lme-agent-hermes-setup') --> POST /mcp likewise
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
(require 'lme-core)
(require 'lme-local-http)
(require 'lme-agent-context)

(defconst lme-agent-mcp-token-variable "LME_EMACS_MCP_TOKEN"
  "Environment variable that hands Codex the server's token.")

(defconst lme-agent-mcp-token-directory
  (expand-file-name "lme-agent-mcp/" temporary-file-directory)
  "Directory of the token files written for Hermes terminals.
It lies under the user's own temporary directory, which others cannot
enter.")

(defconst lme-agent-mcp-hermes-bridge
  (expand-file-name "bin/emacs-mcp" lme-directory)
  "The stdio MCP server Hermes runs, which forwards to a terminal's server.")

(defconst lme-agent-mcp-hermes-config-file (expand-file-name "~/.hermes/config.yaml")
  "Hermes's configuration file, the only place it takes MCP servers from.")

(cl-defstruct (lme-agent-mcp (:constructor nil))
  "The MCP server of one agent terminal; DIRECTORY bounds what it shows."
  server port token directory)

(cl-defstruct (lme-agent-mcp-codex (:include lme-agent-mcp)
                                  (:constructor lme-agent-mcp-codex--make))
  "The MCP server of a Codex terminal.")

(cl-defstruct (lme-agent-mcp-hermes (:include lme-agent-mcp)
                                   (:constructor lme-agent-mcp-hermes--make))
  "The MCP server of a Hermes terminal; TOKEN-FILE holds its token.")

(defun lme-agent-mcp-start (agent directory)
  "Serve MCP to a terminal of AGENT, `codex' or `hermes', running in DIRECTORY.
Return the server, to bind with `lme-agent-context-bind'."
  (let ((server (funcall (pcase agent
                           ('codex #'lme-agent-mcp-codex--make)
                           ('hermes #'lme-agent-mcp-hermes--make))
                         :token (car (process-lines "uuidgen"))
                         :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lme-agent-mcp-server server)
          (lme-local-http-serve
           (format "%s-mcp" agent)
           (lambda (headers)
             (equal (cdr (assoc "authorization" headers))
                    (concat "Bearer " (lme-agent-mcp-token server))))
           (lambda (method path _headers body)
             (lme-agent-mcp--respond server method path body))))
    (setf (lme-agent-mcp-port server) (process-contact (lme-agent-mcp-server server) :service))
    (when (lme-agent-mcp-hermes-p server)
      (lme-agent-mcp--write-token-file server))
    server))

(cl-defmethod lme-agent-context-stop ((server lme-agent-mcp))
  "Stop SERVER."
  (delete-process (lme-agent-mcp-server server)))

;;;; Codex

(defun lme-agent-mcp--bypass-proxy (variable)
  "Return the entry of the proxy exclusion VARIABLE with loopback added.
Codex's HTTP client follows the system proxy, which otherwise receives
the requests to this server too and refuses them."
  (format "%s=%s" variable
          (string-join (delete-dups (append (split-string (or (getenv variable) "") "," t)
                                            '("127.0.0.1")))
                       ",")))

(cl-defmethod lme-agent-context-environment ((server lme-agent-mcp-codex))
  "Hand Codex SERVER's token, and reach SERVER without a proxy."
  (list (format "%s=%s" lme-agent-mcp-token-variable (lme-agent-mcp-token server))
        (lme-agent-mcp--bypass-proxy "NO_PROXY")
        (lme-agent-mcp--bypass-proxy "no_proxy")))

(cl-defmethod lme-agent-context-arguments ((server lme-agent-mcp-codex))
  "Configure SERVER as Codex's MCP server `emacs' for this run only.
Codex passes its whole environment to the commands it runs unless told
otherwise, so the token variable is excluded from them.  The exclusion
replaces any `shell_environment_policy.exclude' of config.toml."
  (list "-c" (format "mcp_servers.emacs.url=\"http://127.0.0.1:%d/mcp\""
                     (lme-agent-mcp-port server))
        "-c" (format "mcp_servers.emacs.bearer_token_env_var=\"%s\""
                     lme-agent-mcp-token-variable)
        "-c" (format "shell_environment_policy.exclude=[\"%s\"]"
                     lme-agent-mcp-token-variable)))

;;;; Hermes

(defun lme-agent-mcp--token-file (server)
  "Return the file holding SERVER's token for Hermes's bridge."
  (expand-file-name (format "%d.token" (lme-agent-mcp-port server))
                    lme-agent-mcp-token-directory))

(defun lme-agent-mcp--write-token-file (server)
  "Write SERVER's token to a file only its owner can read.
A stale file from a crash is replaced rather than reused with whatever
permissions it had."
  (let ((file (lme-agent-mcp--token-file server)))
    (with-file-modes #o700
      (make-directory lme-agent-mcp-token-directory t))
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region (lme-agent-mcp-token server) nil file nil 'silent))))

(cl-defmethod lme-agent-context-environment ((server lme-agent-mcp-hermes))
  "Point Hermes's bridge at SERVER and its token file.
The bridge reaches loopback without a proxy on its own."
  (list (format "LME_EMACS_MCP_URL=http://127.0.0.1:%d/mcp" (lme-agent-mcp-port server))
        (format "LME_EMACS_MCP_TOKEN_FILE=%s" (lme-agent-mcp--token-file server))))

(cl-defmethod lme-agent-context-stop ((server lme-agent-mcp-hermes))
  "Stop SERVER and remove its token file."
  (cl-call-next-method)
  (ignore-errors (delete-file (lme-agent-mcp--token-file server))))

(defun lme-agent-mcp-hermes-configured-p ()
  "Return non-nil when Hermes's config names an MCP server `emacs'.
Only the key is checked: the entry is Hermes's to write, see
`lme-agent-hermes-setup'."
  (when (file-readable-p lme-agent-mcp-hermes-config-file)
    (with-temp-buffer
      (insert-file-contents lme-agent-mcp-hermes-config-file)
      (goto-char (point-min))
      (when (re-search-forward "^mcp_servers:" nil t)
        (let ((end (save-excursion
                     (if (re-search-forward "^[^ \t\n#]" nil t) (match-beginning 0) (point-max)))))
          (and (re-search-forward "^[ \t]+emacs:" end t) t))))))

(defun lme-agent-mcp-hermes-setup-arguments ()
  "Return the arguments that have Hermes add the bridge as its server `emacs'.
The environment entries are placeholders Hermes fills from its environment
at each start, so one entry serves every terminal."
  (list "mcp" "add" "emacs"
        "--command" lme-agent-mcp-hermes-bridge
        "--env" "LME_EMACS_MCP_URL=${LME_EMACS_MCP_URL}"
        "LME_EMACS_MCP_TOKEN_FILE=${LME_EMACS_MCP_TOKEN_FILE}"))

;;;; Requests

(defconst lme-agent-mcp-tools
  (lme-agent-context-tools #'lme-agent-mcp-directory
                          '("getCurrentSelection" "getOpenEditors" "getDiagnostics"
                            "checkDocumentDirty" "openFile"))
  "Tools served to Codex and Hermes.")

(defun lme-agent-mcp--respond (server method path body)
  "Answer the HTTP request METHOD PATH carrying the JSON-RPC BODY for SERVER.
A request gets its response; a notification is acknowledged without one."
  (let ((message (and (equal method "POST") (lme-agent-context-parse body))))
    (cond
     ((not (equal (car (split-string path "?")) "/mcp")) '("404 Not Found"))
     ((not (equal method "POST")) '("405 Method Not Allowed"))
     ((not (and message (listp message))) '("400 Bad Request"))
     (t (let ((response (lme-agent-context-handle server lme-agent-mcp-tools message)))
          (if (consp response)
              (list "200 OK" (json-serialize response))
            '("202 Accepted")))))))

(provide 'lme-agent-mcp)
;;; lme-agent-mcp.el ends here
