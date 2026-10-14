;;; lispm-desktop-agent-ide.el --- Claude Code's IDE protocol for agent terminals -*- lexical-binding: t; -*-

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
;; Besides the read tools of `lispm-desktop-agent-context', the server pushes the
;; selection as it changes and shows proposed edits as diffs.  Emacs never
;; writes a file for the agent: accepting a diff hands the content back and
;; Claude writes the file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-websocket)
(require 'lispm-desktop-agent-context)

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))

(defconst lispm-desktop-agent-ide-lock-directory (expand-file-name "~/.claude/ide/")
  "Directory where Claude Code looks for editors' lock files.")

(cl-defstruct (lispm-desktop-agent-ide (:constructor lispm-desktop-agent-ide--make))
  "The IDE server of one Claude terminal.
DIRECTORY bounds what the agent sees; CONNECTION is Claude's WebSocket
while it is connected; DIFFS maps a diff's tab name to its buffer."
  server port token directory connection
  (diffs (make-hash-table :test 'equal)))

(defvar lispm-desktop-agent-ide--sessions nil
  "IDE servers of the live Claude terminals.")

(defvar lispm-desktop-agent-ide-tools)

;;;; Lifecycle

(defun lispm-desktop-agent-ide-start (directory)
  "Serve the IDE protocol to a Claude terminal running in DIRECTORY.
Return the server, to bind with `lispm-desktop-agent-context-bind'."
  (let ((session (lispm-desktop-agent-ide--make
                  :token (car (process-lines "uuidgen"))
                  :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lispm-desktop-agent-ide-server session)
          (lispm-desktop-websocket-serve
           "claude-ide"
           (lambda (headers) (lispm-desktop-agent-ide--authorized-p session headers))
           (lambda (connection) (setf (lispm-desktop-agent-ide-connection session) connection))
           (lambda (_connection message) (lispm-desktop-agent-ide--receive session message))
           (lambda (_connection) (lispm-desktop-agent-ide--disconnected session))))
    (setf (lispm-desktop-agent-ide-port session)
          (process-contact (lispm-desktop-agent-ide-server session) :service))
    (lispm-desktop-agent-ide--write-lock session)
    (push session lispm-desktop-agent-ide--sessions)
    (add-hook 'lispm-desktop-agent-context-selection-functions #'lispm-desktop-agent-ide--notify-selection-all)
    session))

(cl-defmethod lispm-desktop-agent-context-environment ((session lispm-desktop-agent-ide))
  "Point Claude at SESSION through its port."
  (list (format "CLAUDE_CODE_SSE_PORT=%d" (lispm-desktop-agent-ide-port session))
        "ENABLE_IDE_INTEGRATION=true"))

(cl-defmethod lispm-desktop-agent-context-stop ((session lispm-desktop-agent-ide))
  "Stop SESSION: its diffs, connection, server and lock file."
  (setq lispm-desktop-agent-ide--sessions (delq session lispm-desktop-agent-ide--sessions))
  (lispm-desktop-agent-ide--close-all-diffs session)
  (when-let* ((connection (lispm-desktop-agent-ide-connection session)))
    (setf (lispm-desktop-agent-ide-connection session) nil)
    (lispm-desktop-websocket-close connection))
  (delete-process (lispm-desktop-agent-ide-server session))
  (ignore-errors (delete-file (lispm-desktop-agent-ide--lock-file session)))
  (unless lispm-desktop-agent-ide--sessions
    (remove-hook 'lispm-desktop-agent-context-selection-functions #'lispm-desktop-agent-ide--notify-selection-all)))

(defun lispm-desktop-agent-ide--lock-file (session)
  "Return the lock file advertising SESSION."
  (expand-file-name (format "%d.lock" (lispm-desktop-agent-ide-port session))
                    lispm-desktop-agent-ide-lock-directory))

(defun lispm-desktop-agent-ide--write-lock (session)
  "Advertise SESSION in a lock file only its owner can read.
The file holds the token, so a stale file from a crash is replaced rather
than reused with whatever permissions it had."
  (let ((file (lispm-desktop-agent-ide--lock-file session)))
    (make-directory lispm-desktop-agent-ide-lock-directory t)
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region
       (json-serialize
        `((pid . ,(emacs-pid))
          (workspaceFolders . [,(directory-file-name (lispm-desktop-agent-ide-directory session))])
          (ideName . "Emacs")
          (transport . "ws")
          (runningInWindows . :false)
          (authToken . ,(lispm-desktop-agent-ide-token session))))
       nil file nil 'silent))))

(defun lispm-desktop-agent-ide--authorized-p (session headers)
  "Return non-nil when HEADERS carry SESSION's token and no client holds it."
  (and (null (lispm-desktop-agent-ide-connection session))
       (equal (cdr (assoc "x-claude-code-ide-authorization" headers))
              (lispm-desktop-agent-ide-token session))))

(defun lispm-desktop-agent-ide--disconnected (session)
  "Forget SESSION's client; diffs waiting on it can no longer be answered."
  (setf (lispm-desktop-agent-ide-connection session) nil)
  (lispm-desktop-agent-ide--close-all-diffs session))

;;;; Messages

(defun lispm-desktop-agent-ide--send (session message)
  "Send the JSON-RPC MESSAGE, an alist, to SESSION's client."
  (when-let* ((connection (lispm-desktop-agent-ide-connection session)))
    (lispm-desktop-websocket-send connection (json-serialize message))))

(defun lispm-desktop-agent-ide--receive (session text)
  "Act on the JSON-RPC message TEXT from SESSION's client.
Once Claude reports itself connected, it is sent the current selection."
  (let* ((message (lispm-desktop-agent-context-parse text))
         (response (lispm-desktop-agent-context-handle session lispm-desktop-agent-ide-tools message)))
    (when (equal (alist-get 'method message) "ide_connected")
      (lispm-desktop-agent-ide--notify-selection session))
    (when (consp response)
      (lispm-desktop-agent-ide--send session response))))

(defun lispm-desktop-agent-ide--notify-selection (session)
  "Send the latest selection to SESSION's client when it may see the file."
  (when-let* ((selection (lispm-desktop-agent-context-selection (lispm-desktop-agent-ide-directory session))))
    (lispm-desktop-agent-ide--send session `((jsonrpc . "2.0")
                                            (method . "selection_changed")
                                            (params . ,selection)))))

(defun lispm-desktop-agent-ide--notify-selection-all ()
  "Send the latest selection to every Claude that may see the file."
  (mapc #'lispm-desktop-agent-ide--notify-selection lispm-desktop-agent-ide--sessions))

;;;; Diffs

(defvar-local lispm-desktop-agent-ide--diff nil
  "This diff's (SESSION TAB-NAME REQUEST-ID CONTENTS), until it is answered.")

(defvar lispm-desktop-agent-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lispm-desktop-agent-ide-accept-diff)
    (define-key map (kbd "C-c C-k") #'lispm-desktop-agent-ide-reject-diff)
    (define-key map (kbd "q") #'lispm-desktop-agent-ide-reject-diff)
    map)
  "Keys answering a proposed change.")

(define-minor-mode lispm-desktop-agent-ide-diff-mode
  "A change Claude proposes: \\[lispm-desktop-agent-ide-accept-diff] accepts it, \
\\[lispm-desktop-agent-ide-reject-diff] rejects it."
  :lighter " Proposed"
  :keymap lispm-desktop-agent-ide-diff-mode-map)

(defun lispm-desktop-agent-ide--diff-buffer (file contents tab)
  "Return a buffer showing CONTENTS proposed for FILE as a diff named after TAB."
  (require 'diff)
  (let ((old (generate-new-buffer " *lispm-desktop-agent-ide old*"))
        (new (generate-new-buffer " *lispm-desktop-agent-ide new*"))
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

(defun lispm-desktop-agent-ide--tool-open-diff (session arguments id)
  "Show the change ARGUMENTS propose and answer request ID once decided."
  (let* ((file (lispm-desktop-agent-context-path (alist-get 'old_file_path arguments)))
         (tab (alist-get 'tab_name arguments))
         (contents (alist-get 'new_file_contents arguments))
         (visiting (find-buffer-visiting file)))
    (lispm-desktop-agent-ide--close-diff session tab)
    (let ((buffer (lispm-desktop-agent-ide--diff-buffer file contents tab)))
      (with-current-buffer buffer
        (setq lispm-desktop-agent-ide--diff (list session tab id contents))
        (setq header-line-format
              (concat (format " Claude proposes a change to %s.  " (abbreviate-file-name file))
                      (substitute-command-keys
                       "\\<lispm-desktop-agent-ide-diff-mode-map>\\[lispm-desktop-agent-ide-accept-diff] accept, \
\\[lispm-desktop-agent-ide-reject-diff] reject")
                      (if (and visiting (buffer-modified-p visiting))
                          (propertize "  (the file has unsaved changes in Emacs)" 'face 'warning)
                        "")))
        (lispm-desktop-agent-ide-diff-mode 1)
        (add-hook 'kill-buffer-hook #'lispm-desktop-agent-ide--diff-killed nil t))
      (puthash tab buffer (lispm-desktop-agent-ide-diffs session))
      (lispm-desktop-agent-context-display buffer))
    'deferred))

(defun lispm-desktop-agent-ide--answer-diff (texts)
  "Answer the current diff buffer's request with TEXTS, then close it."
  (pcase-let ((`(,session ,tab ,id ,_) lispm-desktop-agent-ide--diff))
    (setq lispm-desktop-agent-ide--diff nil)
    (lispm-desktop-agent-ide--send session (lispm-desktop-agent-context-result
                                           id `((content . ,(lispm-desktop-agent-context-content texts)))))
    (lispm-desktop-agent-ide--close-diff session tab)))

(defun lispm-desktop-agent-ide-accept-diff ()
  "Accept the proposed change; Claude writes it."
  (interactive)
  (unless lispm-desktop-agent-ide--diff (user-error "No change is waiting here"))
  (lispm-desktop-agent-ide--answer-diff (list "FILE_SAVED" (nth 3 lispm-desktop-agent-ide--diff))))

(defun lispm-desktop-agent-ide-reject-diff ()
  "Reject the proposed change."
  (interactive)
  (unless lispm-desktop-agent-ide--diff (user-error "No change is waiting here"))
  (lispm-desktop-agent-ide--answer-diff (list "DIFF_REJECTED" (nth 1 lispm-desktop-agent-ide--diff))))

(defun lispm-desktop-agent-ide--diff-killed ()
  "Reject the change of a diff buffer killed before it was answered."
  (when lispm-desktop-agent-ide--diff
    (pcase-let ((`(,session ,tab ,id ,_) lispm-desktop-agent-ide--diff))
      (setq lispm-desktop-agent-ide--diff nil)
      (remhash tab (lispm-desktop-agent-ide-diffs session))
      (lispm-desktop-agent-ide--send session
                                     (lispm-desktop-agent-context-result
                                      id `((content . ,(lispm-desktop-agent-context-content
                                                        (list "DIFF_REJECTED" tab)))))))))

(defun lispm-desktop-agent-ide--close-diff (session tab)
  "Close SESSION's diff TAB without answering it, restoring its window."
  (when-let* ((buffer (gethash tab (lispm-desktop-agent-ide-diffs session))))
    (remhash tab (lispm-desktop-agent-ide-diffs session))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lispm-desktop-agent-ide--diff nil))
      (dolist (window (get-buffer-window-list buffer nil t))
        (quit-window nil window))
      (kill-buffer buffer))))

(defun lispm-desktop-agent-ide--close-all-diffs (session)
  "Close every diff of SESSION without answering them."
  (dolist (tab (hash-table-keys (lispm-desktop-agent-ide-diffs session)))
    (lispm-desktop-agent-ide--close-diff session tab)))

(defun lispm-desktop-agent-ide--tool-close-tab (session arguments _id)
  "Close the diff ARGUMENTS name: Claude settled it in the terminal."
  (lispm-desktop-agent-ide--close-diff session (alist-get 'tab_name arguments))
  (list "TAB_CLOSED"))

(defun lispm-desktop-agent-ide--tool-close-all-diffs (session _arguments _id)
  "Close every diff of SESSION."
  (lispm-desktop-agent-ide--close-all-diffs session)
  (list "CLOSED_ALL_DIFF_TABS"))

(defconst lispm-desktop-agent-ide-tools
  (append
   (lispm-desktop-agent-context-tools #'lispm-desktop-agent-ide-directory)
   '(("openDiff" lispm-desktop-agent-ide--tool-open-diff
      ((old_file_path . ((type . "string")))
       (new_file_path . ((type . "string")))
       (new_file_contents . ((type . "string")))
       (tab_name . ((type . "string"))))
      ["old_file_path" "new_file_path" "new_file_contents" "tab_name"]
      "Show a proposed change as a diff for the user to accept or reject.")
     ("close_tab" lispm-desktop-agent-ide--tool-close-tab ((tab_name . ((type . "string")))) ["tab_name"]
      "Close a diff.")
     ("closeAllDiffTabs" lispm-desktop-agent-ide--tool-close-all-diffs nil nil
      "Close every diff.")))
  "Tools served to Claude: the read tools and the diff tools.")

(provide 'lispm-desktop-agent-ide)
;;; lispm-desktop-agent-ide.el ends here
