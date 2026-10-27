;;; lispm-agents-ide.el --- Claude Code's IDE protocol for agent terminals -*- lexical-binding: t; -*-

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

;; Claude Code talks to an editor over the protocol its VS Code and
;; JetBrains extensions speak: MCP (JSON-RPC 2.0) over a local WebSocket.
;; Each Claude terminal gets a server of its own:
;;
;;   launch --> server on a free port --> ~/.claude/ide/PORT.lock (token)
;;   claude, started with CLAUDE_CODE_SSE_PORT=PORT --> connects, sending
;;   the token in X-Claude-Code-Ide-Authorization
;;
;; The lock file is readable by its owner only, and a client without the
;; token is refused at the handshake.
;;
;; Besides the read tools of `lispm-agents-context', the server pushes the
;; selection as it changes and shows proposed edits as diffs.  Emacs never
;; writes a file for the agent: accepting a diff hands the content back and
;; Claude writes the file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-websocket)
(require 'lispm-agents-context)
(require 'lispm-i18n)

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))

(defconst lispm-agents-ide-lock-directory (expand-file-name "~/.claude/ide/")
  "Directory where Claude Code looks for editors' lock files.")

(cl-defstruct (lispm-agents-ide (:constructor lispm-agents-ide--make))
  "The IDE server of one Claude terminal.
DIRECTORY bounds what the agent sees; CONNECTION is Claude's WebSocket
while it is connected; DIFFS maps a diff's tab name to its buffer."
  server port token directory connection
  (diffs (make-hash-table :test 'equal)))

(defvar lispm-agents-ide--sessions nil
  "IDE servers of the live Claude terminals.")

(defvar lispm-agents-ide-tools)

;;;; Lifecycle

(defun lispm-agents-ide-start (directory)
  "Serve the IDE protocol to a Claude terminal running in DIRECTORY.
Return the server, to bind with `lispm-agents-context-bind'."
  (let ((session (lispm-agents-ide--make
                  :token (lispm-agents-uuid)
                  :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lispm-agents-ide-server session)
          (lispm-websocket-serve
           "claude-ide"
           (lambda (headers) (lispm-agents-ide--authorized-p session headers))
           (lambda (connection) (setf (lispm-agents-ide-connection session) connection))
           (lambda (_connection message) (lispm-agents-ide--receive session message))
           (lambda (_connection) (lispm-agents-ide--disconnected session))))
    (setf (lispm-agents-ide-port session)
          (process-contact (lispm-agents-ide-server session) :service))
    (lispm-agents-ide--write-lock session)
    (push session lispm-agents-ide--sessions)
    (add-hook 'lispm-agents-context-selection-functions #'lispm-agents-ide--notify-selection-all)
    session))

(cl-defmethod lispm-agents-context-environment ((session lispm-agents-ide))
  "Point Claude at SESSION through its port."
  (list (format "CLAUDE_CODE_SSE_PORT=%d" (lispm-agents-ide-port session))
        "ENABLE_IDE_INTEGRATION=true"))

(cl-defmethod lispm-agents-context-stop ((session lispm-agents-ide))
  "Stop SESSION: its diffs, connection, server and lock file."
  (setq lispm-agents-ide--sessions (delq session lispm-agents-ide--sessions))
  (lispm-agents-ide--close-all-diffs session)
  (when-let* ((connection (lispm-agents-ide-connection session)))
    (setf (lispm-agents-ide-connection session) nil)
    (lispm-websocket-close connection))
  (delete-process (lispm-agents-ide-server session))
  (ignore-errors (delete-file (lispm-agents-ide--lock-file session)))
  (unless lispm-agents-ide--sessions
    (remove-hook 'lispm-agents-context-selection-functions #'lispm-agents-ide--notify-selection-all)))

(defun lispm-agents-ide--lock-file (session)
  "Return the lock file advertising SESSION."
  (expand-file-name (format "%d.lock" (lispm-agents-ide-port session))
                    lispm-agents-ide-lock-directory))

(defun lispm-agents-ide--write-lock (session)
  "Advertise SESSION in a lock file only its owner can read.
The file holds the token, so a stale file from a crash is replaced rather
than reused with whatever permissions it had."
  (let ((file (lispm-agents-ide--lock-file session)))
    (make-directory lispm-agents-ide-lock-directory t)
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region
       (json-serialize
        `((pid . ,(emacs-pid))
          (workspaceFolders . [,(directory-file-name (lispm-agents-ide-directory session))])
          (ideName . "Emacs")
          (transport . "ws")
          (runningInWindows . :false)
          (authToken . ,(lispm-agents-ide-token session))))
       nil file nil 'silent))))

(defun lispm-agents-ide--authorized-p (session headers)
  "Return non-nil when HEADERS carry SESSION's token and no client holds it."
  (and (null (lispm-agents-ide-connection session))
       (equal (cdr (assoc "x-claude-code-ide-authorization" headers))
              (lispm-agents-ide-token session))))

(defun lispm-agents-ide--disconnected (session)
  "Forget SESSION's client; diffs waiting on it can no longer be answered."
  (setf (lispm-agents-ide-connection session) nil)
  (lispm-agents-ide--close-all-diffs session))

;;;; Messages

(defun lispm-agents-ide--send (session message)
  "Send the JSON-RPC MESSAGE, an alist, to SESSION's client."
  (when-let* ((connection (lispm-agents-ide-connection session)))
    (lispm-websocket-send connection (json-serialize message))))

(defun lispm-agents-ide--receive (session text)
  "Act on the JSON-RPC message TEXT from SESSION's client.
Once Claude reports itself connected, it is sent the current selection."
  (let* ((message (lispm-agents-context-parse text))
         (response (lispm-agents-context-handle session lispm-agents-ide-tools message)))
    (when (equal (alist-get 'method message) "ide_connected")
      (lispm-agents-ide--notify-selection session))
    (when (consp response)
      (lispm-agents-ide--send session response))))

(defun lispm-agents-ide--notify-selection (session)
  "Send the latest selection to SESSION's client when it may see the file."
  (when-let* ((selection (lispm-agents-context-selection (lispm-agents-ide-directory session))))
    (lispm-agents-ide--send session `((jsonrpc . "2.0")
                                     (method . "selection_changed")
                                     (params . ,selection)))))

(defun lispm-agents-ide--notify-selection-all ()
  "Send the latest selection to every Claude that may see the file."
  (mapc #'lispm-agents-ide--notify-selection lispm-agents-ide--sessions))

;;;; Diffs

(defvar-local lispm-agents-ide--diff nil
  "This diff's (SESSION TAB-NAME REQUEST-ID CONTENTS), until it is answered.")

(defvar lispm-agents-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lispm-agents-ide-accept-diff)
    (define-key map (kbd "C-c C-k") #'lispm-agents-ide-reject-diff)
    (define-key map (kbd "q") #'lispm-agents-ide-reject-diff)
    map)
  "Keys answering a proposed change.")

(define-minor-mode lispm-agents-ide-diff-mode
  "A change Claude proposes: \\[lispm-agents-ide-accept-diff] accepts it, \
\\[lispm-agents-ide-reject-diff] rejects it."
  :lighter " Proposed"
  :keymap lispm-agents-ide-diff-mode-map)

(defun lispm-agents-ide--diff-buffer (file contents tab)
  "Return a buffer showing CONTENTS proposed for FILE as a diff named after TAB."
  (require 'diff)
  (let ((old (generate-new-buffer " *lispm-agents-ide old*"))
        (new (generate-new-buffer " *lispm-agents-ide new*"))
        (buffer (get-buffer-create (format "*Proposed: %s*" tab))))
    (unwind-protect
        (progn
          (with-current-buffer old
            (when (file-exists-p file) (insert-file-contents file)))
          (with-current-buffer new (insert contents))
          (with-current-buffer buffer
            (let ((inhibit-read-only t)) (erase-buffer)))
          (diff-no-select old new "-u" t buffer))
      (kill-buffer old)
      (kill-buffer new))
    buffer))

(defun lispm-agents-ide--tool-open-diff (session arguments id)
  "Show the change ARGUMENTS propose and answer request ID once decided."
  (let* ((file (lispm-agents-context-path (alist-get 'old_file_path arguments)))
         (tab (alist-get 'tab_name arguments))
         (contents (alist-get 'new_file_contents arguments))
         (visiting (find-buffer-visiting file)))
    (lispm-agents-ide--close-diff session tab)
    (let ((buffer (lispm-agents-ide--diff-buffer file contents tab)))
      (with-current-buffer buffer
        (setq lispm-agents-ide--diff (list session tab id contents))
        (setq header-line-format
              (concat " " (lispm-tr "Claude proposes a change to %s." (abbreviate-file-name file))
                      "  "
                      (lispm-tr "%s accept, %s reject"
                                (substitute-command-keys
                                 "\\<lispm-agents-ide-diff-mode-map>\\[lispm-agents-ide-accept-diff]")
                                (substitute-command-keys
                                 "\\<lispm-agents-ide-diff-mode-map>\\[lispm-agents-ide-reject-diff]"))
                      (if (and visiting (buffer-modified-p visiting))
                          (concat "  " (propertize (lispm-tr "(the file has unsaved changes in Emacs)")
                                                   'face 'warning))
                        "")))
        (lispm-agents-ide-diff-mode 1)
        (add-hook 'kill-buffer-hook #'lispm-agents-ide--diff-killed nil t))
      (puthash tab buffer (lispm-agents-ide-diffs session))
      (lispm-agents-context-display buffer))
    'deferred))

(defun lispm-agents-ide--answer-diff (texts)
  "Answer the current diff buffer's request with TEXTS, then close it."
  (pcase-let ((`(,session ,tab ,id ,_) lispm-agents-ide--diff))
    (setq lispm-agents-ide--diff nil)
    (lispm-agents-ide--send session (lispm-agents-context-result
                                    id `((content . ,(lispm-agents-context-content texts)))))
    (lispm-agents-ide--close-diff session tab)))

(defun lispm-agents-ide-accept-diff ()
  "Accept the proposed change; Claude writes it."
  (interactive)
  (unless lispm-agents-ide--diff (user-error (lispm-tr "No change is waiting here")))
  (lispm-agents-ide--answer-diff (list "FILE_SAVED" (nth 3 lispm-agents-ide--diff))))

(defun lispm-agents-ide-reject-diff ()
  "Reject the proposed change."
  (interactive)
  (unless lispm-agents-ide--diff (user-error (lispm-tr "No change is waiting here")))
  (lispm-agents-ide--answer-diff (list "DIFF_REJECTED" (nth 1 lispm-agents-ide--diff))))

(defun lispm-agents-ide--diff-killed ()
  "Reject the change of a diff buffer killed before it was answered."
  (when lispm-agents-ide--diff
    (pcase-let ((`(,session ,tab ,id ,_) lispm-agents-ide--diff))
      (setq lispm-agents-ide--diff nil)
      (remhash tab (lispm-agents-ide-diffs session))
      (lispm-agents-ide--send session
                              (lispm-agents-context-result
                               id `((content . ,(lispm-agents-context-content
                                                 (list "DIFF_REJECTED" tab)))))))))

(defun lispm-agents-ide--close-diff (session tab)
  "Close SESSION's diff TAB without answering it, restoring its window."
  (when-let* ((buffer (gethash tab (lispm-agents-ide-diffs session))))
    (remhash tab (lispm-agents-ide-diffs session))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lispm-agents-ide--diff nil))
      (dolist (window (get-buffer-window-list buffer nil t))
        (quit-window nil window))
      (kill-buffer buffer))))

(defun lispm-agents-ide--close-all-diffs (session)
  "Close every diff of SESSION without answering them."
  (dolist (tab (hash-table-keys (lispm-agents-ide-diffs session)))
    (lispm-agents-ide--close-diff session tab)))

(defun lispm-agents-ide--tool-close-tab (session arguments _id)
  "Close the diff ARGUMENTS name: Claude settled it in the terminal."
  (lispm-agents-ide--close-diff session (alist-get 'tab_name arguments))
  (list "TAB_CLOSED"))

(defun lispm-agents-ide--tool-close-all-diffs (session _arguments _id)
  "Close every diff of SESSION."
  (lispm-agents-ide--close-all-diffs session)
  (list "CLOSED_ALL_DIFF_TABS"))

(defconst lispm-agents-ide-tools
  (append
   (lispm-agents-context-tools #'lispm-agents-ide-directory)
   '(("openDiff" lispm-agents-ide--tool-open-diff
      ((old_file_path . ((type . "string")))
       (new_file_path . ((type . "string")))
       (new_file_contents . ((type . "string")))
       (tab_name . ((type . "string"))))
      ["old_file_path" "new_file_path" "new_file_contents" "tab_name"]
      "Show a proposed change as a diff for the user to accept or reject.")
     ("close_tab" lispm-agents-ide--tool-close-tab ((tab_name . ((type . "string")))) ["tab_name"]
      "Close a diff.")
     ("closeAllDiffTabs" lispm-agents-ide--tool-close-all-diffs nil nil
      "Close every diff.")))
  "Tools served to Claude: the read tools and the diff tools.")

(provide 'lispm-agents-ide)
;;; lispm-agents-ide.el ends here
