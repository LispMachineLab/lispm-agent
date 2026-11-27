;;; lispm-agents-actions.el --- Contextual agent actions on selected code  -*- lexical-binding: t; -*-

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

;; Contextual verbs acting on a selected code region (`Explain', `Refactor',
;; `Implement', `Fix', plus staging the region into an agent's composer).
;;
;; Selecting a block of code in any editable buffer and opening the right-click
;; context menu (or typing `C-c a e', `C-c a R', `C-c a i', `C-c a f') formats
;; a project-relative fenced reference, ensures a live project agent session
;; exists (starting the default available agent at the project root when none
;; is running), lets the host workspace arrange the conversation beside the
;; source file, and submits the request immediately.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-agents)

(declare-function evil-exit-visual-state "evil-states" (&optional buffer))
(defvar lispm-session-mode)
(defvar evil-local-mode)
(defvar evil-state)

;;;; Conditions & Domain Model

(define-error 'lispm-agents-action-error
  "Agent code action error" 'user-error)

(define-error 'lispm-agents-empty-region-error
  "Empty or inactive code region" 'lispm-agents-action-error)

;;; Invariant: (and (symbolp id) (not (null id))
;;;                 (stringp label) (not (string-empty-p label))
;;;                 (stringp prompt) (not (string-empty-p prompt))
;;;                 (symbolp command) (not (null command)))
(cl-defstruct (lispm-agents-code-action
               (:copier nil)
               (:constructor lispm-agents--make-code-action))
  "A contextual verb dispatching a selected code region to an agent."
  (id nil :type symbol :read-only t)
  (label "" :type string :read-only t)
  (prompt "" :type string :read-only t)
  (command nil :type symbol :read-only t)
  (key nil :type (or string null) :read-only t))

(defun lispm-agents-make-code-action (&rest plist)
  "Construct a validated `lispm-agents-code-action' from PLIST."
  (let ((id (plist-get plist :id))
        (label (plist-get plist :label))
        (prompt (plist-get plist :prompt))
        (command (plist-get plist :command))
        (key (plist-get plist :key)))
    (cl-check-type id symbol)
    (cl-check-type label string)
    (cl-check-type prompt string)
    (cl-check-type command symbol)
    (cl-check-type key (or string null))
    (when (or (null id) (null command)
              (string-empty-p label) (string-empty-p prompt))
      (signal 'lispm-agents-action-error (list "Invalid code action spec" plist)))
    (lispm-agents--make-code-action
     :id id :label label :prompt prompt :command command :key key)))

(defconst lispm-agents-code-actions
  (list
   (lispm-agents-make-code-action
    :id 'explain
    :label (lispm-tr-noop "Explain")
    :prompt "Explain what this code does, its invariants, and how it fits into the surrounding module:"
    :command 'lispm-agents-explain-region
    :key "e")
   (lispm-agents-make-code-action
    :id 'refactor
    :label (lispm-tr-noop "Refactor")
    :prompt "Refactor and simplify this code in place while preserving its behavior:"
    :command 'lispm-agents-refactor-region
    :key "R")
   (lispm-agents-make-code-action
    :id 'implement
    :label (lispm-tr-noop "Implement")
    :prompt "Implement the functionality described or stubbed in this code directly in the file:"
    :command 'lispm-agents-implement-region
    :key "i")
   (lispm-agents-make-code-action
    :id 'fix
    :label (lispm-tr-noop "Fix")
    :prompt "Find and fix any bugs, edge cases, or warnings in this code:"
    :command 'lispm-agents-fix-region
    :key "f"))
  "Ordered catalog of contextual agent verbs offered on selected code.")

(defun lispm-agents-code-action-find (id)
  "Return the `lispm-agents-code-action' with symbol ID, or signal an error."
  (or (seq-find (lambda (act) (eq (lispm-agents-code-action-id act) id))
                lispm-agents-code-actions)
      (signal 'lispm-agents-action-error
              (list (format "Unknown code action: %S" id)))))

;;;; Host Interface Hooks

(defvar lispm-agents-region-action-prepare-function nil
  "Optional host function called with SOURCE-BUFFER before resolving an agent.
A graphical desktop uses this to switch out of pure file or shell views
into its agent workspace before creating or displaying the session.")

(defvar lispm-agents-region-action-display-function nil
  "Optional host function called with (AGENT-BUFFER SOURCE-BUFFER POSITION).
When nil, `lispm-agents-show' displays AGENT-BUFFER.")

;;;; Semantic Predicates & Pure Formatting

(defun lispm-agents-code-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an editable source or text buffer."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (not (minibufferp buf))
         (not (string-prefix-p " " (buffer-name buf)))
         (not (lispm-agents-buffer-p buf))
         (not (lispm-agents-splash-buffer-p buf))
         (with-current-buffer buf
           (and (not (bound-and-true-p lispm-session-mode))
                (or buffer-file-name
                    (derived-mode-p 'prog-mode 'text-mode 'conf-mode)))))))

(defun lispm-agents-nonempty-region-p (beg end &optional buffer)
  "Return non-nil when BEG..END spans non-whitespace text in BUFFER."
  (and (integer-or-marker-p beg)
       (integer-or-marker-p end)
       (let ((lo (min beg end))
             (hi (max beg end))
             (buf (or buffer (current-buffer))))
         (and (< lo hi)
              (buffer-live-p buf)
              (with-current-buffer buf
                (save-restriction
                  (widen)
                  (not (string-blank-p
                        (buffer-substring-no-properties
                         (max (point-min) lo) (min (point-max) hi))))))))))

(defun lispm-agents-buffer-idle-p (buffer)
  "Return non-nil when live agent BUFFER is not actively running a turn."
  (and (buffer-live-p buffer)
       (not (eq (plist-get (lispm-agents-buffer-status buffer) :state) 'working))))

(defun lispm-agents-local-session-p (buffer root)
  "Return non-nil when session BUFFER works inside project directory ROOT."
  (and (buffer-live-p buffer)
       (stringp root)
       (let ((cwd (buffer-local-value 'default-directory buffer)))
         (and (stringp cwd) (file-in-directory-p cwd root)))))

;;; Pre: (and (stringp instruction) (stringp reference))
;;; Post: (and (stringp result) (string-suffix-p "\n" result))
(defun lispm-agents-compose-region-prompt (instruction reference)
  "Compose INSTRUCTION and fenced code REFERENCE into an agent user message."
  (concat (string-trim-right instruction) "\n\n" (string-trim-right reference) "\n"))

;;;; Boundary Operations: Extraction, Session Resolution & Submission

(defun lispm-agents-extract-region-reference (buffer beg end)
  "Extract a fenced project-relative code reference for BEG..END in BUFFER."
  (unless (lispm-agents-nonempty-region-p beg end buffer)
    (signal 'lispm-agents-empty-region-error
            (list (lispm-tr "Select a code region first"))))
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((lo (max (point-min) (min beg end)))
              (hi (min (point-max) (max beg end))))
          (lispm-agents-region-reference lo hi))))))

(defun lispm-agents--clear-selection (buffer)
  "Deactivate the mark and exit visual state in BUFFER when active."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (deactivate-mark)
      (when (and (bound-and-true-p evil-local-mode)
                 (eq (bound-and-true-p evil-state) 'visual)
                 (fboundp 'evil-exit-visual-state))
        (evil-exit-visual-state)))))

(defun lispm-agents-ensure-project-buffer (source-buffer)
  "Return a live agent buffer for SOURCE-BUFFER's project, starting one if needed."
  (let* ((root (with-current-buffer source-buffer
                 (lispm-agents-project-directory)))
         (local (seq-filter (lambda (buf) (lispm-agents-local-session-p buf root))
                            (lispm-agents-live-buffers))))
    (or (seq-find #'lispm-agents-buffer-idle-p local)
        (car local)
        (if-let* ((default-agent (caar (lispm-agents-available))))
            (lispm-agents-new default-agent root)
          (user-error "%s" (lispm-tr "No running agent; start one first"))))))

(defun lispm-agents-display-for-region (agent-buffer source-buffer position)
  "Display AGENT-BUFFER alongside SOURCE-BUFFER at POSITION."
  (if lispm-agents-region-action-display-function
      (funcall lispm-agents-region-action-display-function
               agent-buffer source-buffer position)
    (lispm-agents-show agent-buffer)))

(defun lispm-agents-paste-to-buffer (target text)
  "Place TEXT into agent buffer TARGET's composer without submitting."
  (let ((props (lispm-agents-props (lispm-agents-of-buffer target))))
    (if-let* ((send (plist-get props :send)))
        (funcall send target text)
      (lispm-agents-terminal-paste target text))))

(defun lispm-agents-submit-to-buffer (target text)
  "Submit TEXT immediately as the user's next message to agent buffer TARGET."
  (let ((props (lispm-agents-props (lispm-agents-of-buffer target))))
    (cond
     ((plist-get props :submit)
      (funcall (plist-get props :submit) target text))
     ((plist-get props :deliver)
      (funcall (plist-get props :deliver) target text))
     ((plist-get props :send)
      (funcall (plist-get props :send) target text))
     (t
      (lispm-agents-terminal-submit target text)))))

;;;; Interactive Dispatch & Context Menu

(defun lispm-agents--dispatch-region (buffer beg end deliver-fn &optional instruction)
  "Extract BEG..END from BUFFER, open a project agent, and call DELIVER-FN.
When INSTRUCTION is non-nil, prepend it before delivering; otherwise deliver
only the fenced code reference.  DELIVER-FN receives `(AGENT-BUFFER PAYLOAD)'."
  (let ((source (or buffer (current-buffer))))
    (unless (buffer-live-p source)
      (signal 'lispm-agents-action-error (list "Source buffer is not live")))
    (let* ((pos (min beg end))
           (ref (lispm-agents-extract-region-reference source beg end))
           (payload (if instruction
                        (lispm-agents-compose-region-prompt instruction ref)
                      ref)))
      (lispm-agents--clear-selection source)
      (when lispm-agents-region-action-prepare-function
        (funcall lispm-agents-region-action-prepare-function source))
      (let ((target (lispm-agents-ensure-project-buffer source)))
        (unless (buffer-live-p target)
          (signal 'lispm-agents-action-error (list "Agent buffer is not live")))
        (lispm-agents-display-for-region target source pos)
        (funcall deliver-fn target payload)
        target))))

(defun lispm-agents-act-on-region (action-id beg end &optional buffer)
  "Run code ACTION-ID (`explain', `refactor', `implement', `fix') on BEG..END.
Reads from BUFFER (defaulting to `current-buffer'), switches to or starts
a project agent session, and submits the prompt."
  (let ((action (lispm-agents-code-action-find action-id)))
    (lispm-agents--dispatch-region
     buffer beg end #'lispm-agents-submit-to-buffer
     (lispm-agents-code-action-prompt action))))

(defun lispm-agents-stage-region (beg end &optional buffer)
  "Stage BEG..END of BUFFER into a project agent's composer without submitting."
  (lispm-agents--dispatch-region
   buffer beg end #'lispm-agents-paste-to-buffer))

(defun lispm-agents--interactive-region ()
  "Return `(BEG END)' of the active region, or signal an empty-region error."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (signal 'lispm-agents-empty-region-error
            (list (lispm-tr "Select a code region first")))))

;;;###autoload
(defun lispm-agents-explain-region (beg end)
  "Ask the project agent to explain the code between BEG and END."
  (interactive (lispm-agents--interactive-region))
  (lispm-agents-act-on-region 'explain beg end))

;;;###autoload
(defun lispm-agents-refactor-region (beg end)
  "Ask the project agent to refactor the code between BEG and END."
  (interactive (lispm-agents--interactive-region))
  (lispm-agents-act-on-region 'refactor beg end))

;;;###autoload
(defun lispm-agents-implement-region (beg end)
  "Ask the project agent to implement what is stubbed between BEG and END."
  (interactive (lispm-agents--interactive-region))
  (lispm-agents-act-on-region 'implement beg end))

;;;###autoload
(defun lispm-agents-fix-region (beg end)
  "Ask the project agent to fix issues in the code between BEG and END."
  (interactive (lispm-agents--interactive-region))
  (lispm-agents-act-on-region 'fix beg end))

(defun lispm-agents--region-action-command (action-id buffer beg end)
  "Return an interactive closure running ACTION-ID on BUFFER's BEG..END."
  (lambda ()
    (interactive)
    (lispm-agents-act-on-region action-id beg end buffer)))

(defun lispm-agents--stage-region-command (buffer beg end)
  "Return an interactive closure staging BUFFER's BEG..END into an agent."
  (lambda ()
    (interactive)
    (lispm-agents-stage-region beg end buffer)))

(defun lispm-agents-populate-region-menu (menu &optional buffer beg end)
  "Add code-region agent actions to keymap MENU for BUFFER's BEG..END.
Does nothing when BUFFER is dead, no non-empty code region is active,
or MENU already has them."
  (let ((buf (or buffer (current-buffer))))
    (when (and (keymapp menu)
               (buffer-live-p buf)
               (lispm-agents-code-buffer-p buf))
      (let ((lo (or beg (with-current-buffer buf
                          (and (use-region-p) (region-beginning)))))
            (hi (or end (with-current-buffer buf
                          (and (use-region-p) (region-end))))))
        (when (and (lispm-agents-nonempty-region-p lo hi buf)
                   (not (lookup-key menu [lispm-agents-explain])))
          (dolist (act lispm-agents-code-actions)
            (let ((sym (intern (format "lispm-agents-%s"
                                       (lispm-agents-code-action-id act)))))
              (define-key-after menu (vector sym)
                `(menu-item ,(lispm-tr (lispm-agents-code-action-label act))
                            ,(lispm-agents--region-action-command
                              (lispm-agents-code-action-id act) buf lo hi)
                            :keys ,(format "\\[%s]"
                                           (lispm-agents-code-action-command act))))))
          (define-key-after menu [lispm-agents-stage-region]
            `(menu-item ,(lispm-tr "Send Region to Agent")
                        ,(lispm-agents--stage-region-command buf lo hi)
                        :keys "\\[lispm-agents-send-region]"))
          (define-key-after menu [lispm-agents-region-sep] menu-bar-separator)))))
  menu)

(defun lispm-agents-context-menu (menu _click)
  "Populate right-click context MENU with agent verbs when a region is active."
  (lispm-agents-populate-region-menu menu))

(defun lispm-agents-bind-action-keys (&optional keymap)
  "Bind `lispm-agents-code-actions' keys in KEYMAP (default `lispm-agents-map')."
  (let ((map (or keymap (and (boundp 'lispm-agents-map) lispm-agents-map))))
    (when (keymapp map)
      (dolist (act lispm-agents-code-actions)
        (when-let* ((key (lispm-agents-code-action-key act)))
          (keymap-set map key (lispm-agents-code-action-command act)))))))

;;;###autoload
(define-minor-mode lispm-agents-actions-mode
  "Toggle contextual agent code actions on selected regions.
When enabled, add `Explain', `Refactor', `Implement', `Fix', and stage
items to right-click context menus whenever a code region is selected."
  :global t
  :group 'lispm
  (if lispm-agents-actions-mode
      (add-hook 'context-menu-functions #'lispm-agents-context-menu -50)
    (remove-hook 'context-menu-functions #'lispm-agents-context-menu)))

(lispm-agents-bind-action-keys lispm-agents-map)

(provide 'lispm-agents-actions)
;;; lispm-agents-actions.el ends here
