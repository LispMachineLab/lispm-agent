;;; lme-agent-ide.el --- Claude Code's IDE protocol for agent terminals -*- lexical-binding: t; -*-

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
;; Besides the read tools of `lme-agent-context', the server pushes the
;; selection as it changes and shows proposed edits as diffs.  Emacs never
;; writes a file for the agent: accepting a diff hands the content back and
;; Claude writes the file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lme-websocket)
(require 'lme-agent-context)

(declare-function diff-no-select "diff" (old new &optional switches no-async buf))

(defconst lme-agent-ide-lock-directory (expand-file-name "~/.claude/ide/")
  "Directory where Claude Code looks for editors' lock files.")

(cl-defstruct (lme-agent-ide (:constructor lme-agent-ide--make))
  "The IDE server of one Claude terminal.
DIRECTORY bounds what the agent sees; CONNECTION is Claude's WebSocket
while it is connected; DIFFS maps a diff's tab name to its buffer."
  server port token directory connection
  (diffs (make-hash-table :test 'equal)))

(defvar lme-agent-ide--sessions nil
  "IDE servers of the live Claude terminals.")

(defvar lme-agent-ide-tools)

;;;; Lifecycle

(defun lme-agent-ide-start (directory)
  "Serve the IDE protocol to a Claude terminal running in DIRECTORY.
Return the server, to bind with `lme-agent-context-bind'."
  (let ((session (lme-agent-ide--make
                  :token (car (process-lines "uuidgen"))
                  :directory (file-name-as-directory (expand-file-name directory)))))
    (setf (lme-agent-ide-server session)
          (lme-websocket-serve
           "claude-ide"
           (lambda (headers) (lme-agent-ide--authorized-p session headers))
           (lambda (connection) (setf (lme-agent-ide-connection session) connection))
           (lambda (_connection message) (lme-agent-ide--receive session message))
           (lambda (_connection) (lme-agent-ide--disconnected session))))
    (setf (lme-agent-ide-port session)
          (process-contact (lme-agent-ide-server session) :service))
    (lme-agent-ide--write-lock session)
    (push session lme-agent-ide--sessions)
    (add-hook 'lme-agent-context-selection-functions #'lme-agent-ide--notify-selection-all)
    session))

(cl-defmethod lme-agent-context-environment ((session lme-agent-ide))
  "Point Claude at SESSION through its port."
  (list (format "CLAUDE_CODE_SSE_PORT=%d" (lme-agent-ide-port session))
        "ENABLE_IDE_INTEGRATION=true"))

(cl-defmethod lme-agent-context-stop ((session lme-agent-ide))
  "Stop SESSION: its diffs, connection, server and lock file."
  (setq lme-agent-ide--sessions (delq session lme-agent-ide--sessions))
  (lme-agent-ide--close-all-diffs session)
  (when-let* ((connection (lme-agent-ide-connection session)))
    (setf (lme-agent-ide-connection session) nil)
    (lme-websocket-close connection))
  (delete-process (lme-agent-ide-server session))
  (ignore-errors (delete-file (lme-agent-ide--lock-file session)))
  (unless lme-agent-ide--sessions
    (remove-hook 'lme-agent-context-selection-functions #'lme-agent-ide--notify-selection-all)))

(defun lme-agent-ide--lock-file (session)
  "Return the lock file advertising SESSION."
  (expand-file-name (format "%d.lock" (lme-agent-ide-port session))
                    lme-agent-ide-lock-directory))

(defun lme-agent-ide--write-lock (session)
  "Advertise SESSION in a lock file only its owner can read.
The file holds the token, so a stale file from a crash is replaced rather
than reused with whatever permissions it had."
  (let ((file (lme-agent-ide--lock-file session)))
    (make-directory lme-agent-ide-lock-directory t)
    (ignore-errors (delete-file file))
    (with-file-modes #o600
      (write-region
       (json-serialize
        `((pid . ,(emacs-pid))
          (workspaceFolders . [,(directory-file-name (lme-agent-ide-directory session))])
          (ideName . "Emacs")
          (transport . "ws")
          (runningInWindows . :false)
          (authToken . ,(lme-agent-ide-token session))))
       nil file nil 'silent))))

(defun lme-agent-ide--authorized-p (session headers)
  "Return non-nil when HEADERS carry SESSION's token and no client holds it."
  (and (null (lme-agent-ide-connection session))
       (equal (cdr (assoc "x-claude-code-ide-authorization" headers))
              (lme-agent-ide-token session))))

(defun lme-agent-ide--disconnected (session)
  "Forget SESSION's client; diffs waiting on it can no longer be answered."
  (setf (lme-agent-ide-connection session) nil)
  (lme-agent-ide--close-all-diffs session))

;;;; Messages

(defun lme-agent-ide--send (session message)
  "Send the JSON-RPC MESSAGE, an alist, to SESSION's client."
  (when-let* ((connection (lme-agent-ide-connection session)))
    (lme-websocket-send connection (json-serialize message))))

(defun lme-agent-ide--receive (session text)
  "Act on the JSON-RPC message TEXT from SESSION's client.
Once Claude reports itself connected, it is sent the current selection."
  (let* ((message (lme-agent-context-parse text))
         (response (lme-agent-context-handle session lme-agent-ide-tools message)))
    (when (equal (alist-get 'method message) "ide_connected")
      (lme-agent-ide--notify-selection session))
    (when (consp response)
      (lme-agent-ide--send session response))))

(defun lme-agent-ide--notify-selection (session)
  "Send the latest selection to SESSION's client when it may see the file."
  (when-let* ((selection (lme-agent-context-selection (lme-agent-ide-directory session))))
    (lme-agent-ide--send session `((jsonrpc . "2.0")
                                  (method . "selection_changed")
                                  (params . ,selection)))))

(defun lme-agent-ide--notify-selection-all ()
  "Send the latest selection to every Claude that may see the file."
  (mapc #'lme-agent-ide--notify-selection lme-agent-ide--sessions))

;;;; Diffs

(defvar-local lme-agent-ide--diff nil
  "This diff's (SESSION TAB-NAME REQUEST-ID CONTENTS), until it is answered.")

(defvar lme-agent-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lme-agent-ide-accept-diff)
    (define-key map (kbd "C-c C-k") #'lme-agent-ide-reject-diff)
    (define-key map (kbd "q") #'lme-agent-ide-reject-diff)
    map)
  "Keys answering a proposed change.")

(define-minor-mode lme-agent-ide-diff-mode
  "A change Claude proposes: \\[lme-agent-ide-accept-diff] accepts it, \
\\[lme-agent-ide-reject-diff] rejects it."
  :lighter " Proposed"
  :keymap lme-agent-ide-diff-mode-map)

(defun lme-agent-ide--diff-buffer (file contents tab)
  "Return a buffer showing CONTENTS proposed for FILE as a diff named after TAB."
  (require 'diff)
  (let ((old (generate-new-buffer " *lme-agent-ide old*"))
        (new (generate-new-buffer " *lme-agent-ide new*"))
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

(defun lme-agent-ide--tool-open-diff (session arguments id)
  "Show the change ARGUMENTS propose and answer request ID once decided."
  (let* ((file (lme-agent-context-path (alist-get 'old_file_path arguments)))
         (tab (alist-get 'tab_name arguments))
         (contents (alist-get 'new_file_contents arguments))
         (visiting (find-buffer-visiting file)))
    (lme-agent-ide--close-diff session tab)
    (let ((buffer (lme-agent-ide--diff-buffer file contents tab)))
      (with-current-buffer buffer
        (setq lme-agent-ide--diff (list session tab id contents))
        (setq header-line-format
              (concat (format " Claude proposes a change to %s.  " (abbreviate-file-name file))
                      (substitute-command-keys
                       "\\<lme-agent-ide-diff-mode-map>\\[lme-agent-ide-accept-diff] accept, \
\\[lme-agent-ide-reject-diff] reject")
                      (if (and visiting (buffer-modified-p visiting))
                          (propertize "  (the file has unsaved changes in Emacs)" 'face 'warning)
                        "")))
        (lme-agent-ide-diff-mode 1)
        (add-hook 'kill-buffer-hook #'lme-agent-ide--diff-killed nil t))
      (puthash tab buffer (lme-agent-ide-diffs session))
      (lme-agent-context-display buffer))
    'deferred))

(defun lme-agent-ide--answer-diff (texts)
  "Answer the current diff buffer's request with TEXTS, then close it."
  (pcase-let ((`(,session ,tab ,id ,_) lme-agent-ide--diff))
    (setq lme-agent-ide--diff nil)
    (lme-agent-ide--send session (lme-agent-context-result
                                 id `((content . ,(lme-agent-context-content texts)))))
    (lme-agent-ide--close-diff session tab)))

(defun lme-agent-ide-accept-diff ()
  "Accept the proposed change; Claude writes it."
  (interactive)
  (unless lme-agent-ide--diff (user-error "No change is waiting here"))
  (lme-agent-ide--answer-diff (list "FILE_SAVED" (nth 3 lme-agent-ide--diff))))

(defun lme-agent-ide-reject-diff ()
  "Reject the proposed change."
  (interactive)
  (unless lme-agent-ide--diff (user-error "No change is waiting here"))
  (lme-agent-ide--answer-diff (list "DIFF_REJECTED" (nth 1 lme-agent-ide--diff))))

(defun lme-agent-ide--diff-killed ()
  "Reject the change of a diff buffer killed before it was answered."
  (when lme-agent-ide--diff
    (pcase-let ((`(,session ,tab ,id ,_) lme-agent-ide--diff))
      (setq lme-agent-ide--diff nil)
      (remhash tab (lme-agent-ide-diffs session))
      (lme-agent-ide--send session
                          (lme-agent-context-result
                           id `((content . ,(lme-agent-context-content
                                             (list "DIFF_REJECTED" tab)))))))))

(defun lme-agent-ide--close-diff (session tab)
  "Close SESSION's diff TAB without answering it, restoring its window."
  (when-let* ((buffer (gethash tab (lme-agent-ide-diffs session))))
    (remhash tab (lme-agent-ide-diffs session))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lme-agent-ide--diff nil))
      (dolist (window (get-buffer-window-list buffer nil t))
        (quit-window nil window))
      (kill-buffer buffer))))

(defun lme-agent-ide--close-all-diffs (session)
  "Close every diff of SESSION without answering them."
  (dolist (tab (hash-table-keys (lme-agent-ide-diffs session)))
    (lme-agent-ide--close-diff session tab)))

(defun lme-agent-ide--tool-close-tab (session arguments _id)
  "Close the diff ARGUMENTS name: Claude settled it in the terminal."
  (lme-agent-ide--close-diff session (alist-get 'tab_name arguments))
  (list "TAB_CLOSED"))

(defun lme-agent-ide--tool-close-all-diffs (session _arguments _id)
  "Close every diff of SESSION."
  (lme-agent-ide--close-all-diffs session)
  (list "CLOSED_ALL_DIFF_TABS"))

(defconst lme-agent-ide-tools
  (append
   (lme-agent-context-tools #'lme-agent-ide-directory)
   '(("openDiff" lme-agent-ide--tool-open-diff
      ((old_file_path . ((type . "string")))
       (new_file_path . ((type . "string")))
       (new_file_contents . ((type . "string")))
       (tab_name . ((type . "string"))))
      ["old_file_path" "new_file_path" "new_file_contents" "tab_name"]
      "Show a proposed change as a diff for the user to accept or reject.")
     ("close_tab" lme-agent-ide--tool-close-tab ((tab_name . ((type . "string")))) ["tab_name"]
      "Close a diff.")
     ("closeAllDiffTabs" lme-agent-ide--tool-close-all-diffs nil nil
      "Close every diff.")))
  "Tools served to Claude: the read tools and the diff tools.")

(provide 'lme-agent-ide)
;;; lme-agent-ide.el ends here
