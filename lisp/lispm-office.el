;;; lispm-office.el --- Spatial Office view for live LispM agents  -*- lexical-binding: t; -*-

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

;; Renders live LispM and external terminal agents in an interactive
;; spatial Office (SVG in GUI frames, accessible text in terminals),
;; built atop `lispm-ui-scene' and `lispm-ui-tabs'.  Enabled by default
;; via `lispm-agents-open-project-functions' and chat `header-line-format'
;; tabs, with zero roster observers or timers until an Office opens.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-kit-commands)
(require 'lispm-ui-base)
(require 'lispm-ui-draw)
(require 'lispm-ui-palette)
(require 'lispm-ui-tabs)
(require 'lispm-ui-scene-layout)
(require 'lispm-ui-scene-sprites)
(require 'lispm-ui-scene)
(require 'lispm-agents-core)
(require 'lispm-agents-terminals)
(require 'lispm-agents-roster)
(require 'lispm-chat)
(require 'lispm-agents-work)
(require 'lispm-office-scope)
(require 'lispm-office-actions)
(require 'lispm-office-inspect)

;;;; Forward declarations resolved in this file

(defvar lispm-office--buffers nil
  "Live `lispm-office-mode' buffers sharing one lazy roster observer.")

(defvar lispm-office--timer nil
  "Periodic animation timer active only while Office buffers exist.")

(defvar-local lispm-office-scope nil
  "Normalized project directory displayed in this buffer, or nil for all.")

(defvar-local lispm-office--project-directory nil
  "Project directory remembered when toggling between project and all.")

(defvar-local lispm-office--origin-buffer nil
  "Conversation or work buffer that opened this Office view.")

(defvar-local lispm-office--selected nil
  "Currently selected `(KIND SESSION-ID)' key in this Office buffer.")

(defvar-local lispm-office--scale 1.0
  "Requested zoom factor for the current Office viewport.")

(defvar-local lispm-office--room-id nil
  "Focused room identifier, or nil for the all-rooms overview.")

(defvar-local lispm-office--view-mode 'auto
  "Render mode preference: `auto', `svg', or `text'.")

(defvar-local lispm-office--phase 0
  "Current animation tick counter for worker sprites.")

(defvar-local lispm-office--events nil
  "Recent collaboration message events overlaid on this scene.")

(defvar-local lispm-office--visits nil
  "Per-buffer hash table tracking collaborator walk start timestamps.")

(defvar-local lispm-office--layout nil
  "Current reconciled seating state in this Office buffer.")

(defvar-local lispm-office--persisted-layout nil
  "Last seating state read from or written to disk.")

(defvar-local lispm-office--plan nil
  "Latest immutable scene plan rendered in this Office buffer.")

(defvar-local lispm-office--frame nil
  "Latest immutable `lispm-ui-scene-frame' coupling SVG and hit regions.")

(defvar-local lispm-office--dirty t
  "Non-nil when this Office buffer needs redraw when visible.")

;;;; Customization & enablement

(defun lispm-office--apply-enabled ()
  "Install or remove Office entry hooks according to `lispm-office-enabled'."
  (if (and (boundp 'lispm-office-enabled) lispm-office-enabled)
      (progn
        (add-hook 'lispm-agents-open-project-functions #'lispm-office-open-project)
        (add-hook 'lispm-chat-mode-hook #'lispm-office--maybe-attach-header))
    (remove-hook 'lispm-agents-open-project-functions #'lispm-office-open-project)
    (remove-hook 'lispm-chat-mode-hook #'lispm-office--maybe-attach-header)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'lispm-chat-mode)
                     (equal header-line-format '(:eval (lispm-office--header-line))))
            (kill-local-variable 'header-line-format)))))))

(defgroup lispm-office nil
  "Spatial Office view for live LispM agents."
  :group 'lispm)

(defcustom lispm-office-enabled t
  "When non-nil, route `lispm-agents-open-project' and chat tabs to Office."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (lispm-office--apply-enabled))
  :group 'lispm-office)

(defcustom lispm-office-show-tabs t
  "When non-nil, show Conversation | Office tabs in chat and Office headers."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (dolist (buf (buffer-list))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (derived-mode-p 'lispm-chat-mode 'lispm-office-mode)
                 (if (and val (null header-line-format) lispm-office-enabled)
                     (setq-local header-line-format '(:eval (lispm-office--header-line)))
                   (when (and (not val)
                              (equal header-line-format '(:eval (lispm-office--header-line))))
                     (kill-local-variable 'header-line-format))))
               (force-mode-line-update)))))
  :group 'lispm-office)

(defcustom lispm-office-layout-file
  (locate-user-emacs-file "lispm-office-seats.eld")
  "File persisting stable room and desk assignments, or nil to disable."
  :type '(choice (const :tag "Do not persist" nil) file)
  :group 'lispm-office)

(defcustom lispm-office-display-function nil
  "Display function called with an Office buffer, or nil for work default."
  :type '(choice (const :tag "Use `lispm-agents-work-display-function'" nil)
                 function)
  :group 'lispm-office)

(defcustom lispm-office-animate-interval 0.45
  "Seconds between animation ticks while visible Office agents are active."
  :type 'number
  :group 'lispm-office)

;;;; Header-line Conversation | Office tabs

(defvar-keymap lispm-office--header-tab-map
  :doc "Mouse keymap for Conversation | Office header-line tabs."
  "<header-line> <mouse-1>" #'lispm-office--click-header-tab
  "<header-line> <down-mouse-1>" #'ignore)

(defun lispm-office--switch-to-office ()
  "Switch from the current conversation buffer to its project Office view."
  (interactive)
  (let* ((origin (current-buffer))
         (scope (lispm-office-scope-buffer-directory origin))
         (office (lispm-office scope)))
    (when (buffer-live-p office)
      (with-current-buffer office
        (setq lispm-office--origin-buffer origin)))
    office))

(defun lispm-office--switch-to-conversation ()
  "Switch from the current Office buffer back to a live conversation."
  (interactive)
  (let* ((records (lispm-office-scope-records lispm-office-scope))
         (selected-buf (and lispm-office--selected
                            (lispm-office-scope-conversation-buffer
                             lispm-office--selected records)))
         (target (or (and (buffer-live-p selected-buf) selected-buf)
                     (and (buffer-live-p lispm-office--origin-buffer)
                          lispm-office--origin-buffer)
                     (seq-some (lambda (rec)
                                 (let ((buf (lispm-office-scope-conversation-buffer
                                             (plist-get rec :key) records)))
                                   (and (buffer-live-p buf) buf)))
                               records))))
    (if (buffer-live-p target)
        (lispm-agents-show target)
      (message "No active conversation in this Office scope"))))

(defun lispm-office--click-header-tab (event)
  "Handle mouse click EVENT on Conversation | Office header-line tabs."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (win (posn-window posn))
              (str (posn-string posn))
              (idx (cdr str))
              ((and (stringp (car str)) (integerp idx) (< idx (length (car str)))))
              (tab (get-text-property idx 'lispm-office-tab (car str))))
    (with-selected-window win
      (pcase tab
        ('conversation (lispm-office--switch-to-conversation))
        ('office (lispm-office--switch-to-office))))))

(defun lispm-office--header-line ()
  "Render Conversation | Office header tabs for the selected window."
  (unless lispm-ui-current-palette
    (lispm-ui-palette-follow-theme))
  (let* ((tty (lispm-ui-tty-p))
         (width (max 16 (window-body-width nil (not tty))))
         (in-office (derived-mode-p 'lispm-office-mode)))
    (lispm-ui-tabs
     `(("Conversation" ,(not in-office)
        lispm-office-tab conversation
        action lispm-office--switch-to-conversation
        keymap ,lispm-office--header-tab-map
        follow-link t
        mouse-face highlight
        help-echo "View agent conversation")
       ("Office" ,in-office
        lispm-office-tab office
        action lispm-office--switch-to-office
        keymap ,lispm-office--header-tab-map
        follow-link t
        mouse-face highlight
        help-echo "View spatial Office"))
     width)))

(defun lispm-office--maybe-attach-header ()
  "Attach Conversation | Office header tabs when chat opens without a custom bar."
  (when (and lispm-office-enabled
             lispm-office-show-tabs
             (null header-line-format))
    (setq-local header-line-format '(:eval (lispm-office--header-line)))))

;;;; Persistence & lazy observer/timer lifecycle

(defun lispm-office--read-layout ()
  "Read persisted seating state from `lispm-office-layout-file', or return nil."
  (when (and lispm-office-layout-file (file-exists-p lispm-office-layout-file))
    (condition-case err
        (lispm-ui-scene-read lispm-office-layout-file)
      (lispm-ui-scene-error
       (message "Office layout unreadable (%s); using fresh seating"
                (error-message-string err))
       nil))))

(defun lispm-office--write-layout (state)
  "Persist STATE safely when `lispm-office-layout-file' is configured."
  (when (and lispm-office-layout-file
             (not (equal state lispm-office--persisted-layout)))
    (condition-case err
        (progn
          (lispm-ui-scene-write state lispm-office-layout-file)
          (setq lispm-office--persisted-layout (copy-tree state)))
      (lispm-ui-scene-error
       (message "Skipping Office layout save: %s" (error-message-string err))))))

(defun lispm-office--graphic-p ()
  "Return non-nil when this Office buffer should draw SVG rather than text."
  (let ((svg-ok (and (fboundp 'image-type-available-p)
                     (image-type-available-p 'svg)
                     t)))
    (pcase lispm-office--view-mode
      ('text nil)
      ('svg svg-ok)
      (_ (and (display-graphic-p) svg-ok)))))

(defun lispm-office--animated-p ()
  "Return non-nil when any agent in the current plan has active motion."
  (or lispm-office--events
      (seq-some (lambda (node)
                  (or (plist-get node :waits-on)
                      (eq (lispm-ui-scene-node-state node) 'working)))
                (plist-get lispm-office--plan :nodes))))

(defun lispm-office--prune-events (now)
  "Discard transient message events older than 1.5 seconds as of NOW."
  (setq lispm-office--events
        (seq-filter (lambda (ev)
                      (< (- now (or (plist-get ev :at) 0.0)) 1.5))
                    lispm-office--events)))

(defun lispm-office--visible ()
  "Redraw dirty visible Office buffers from one shared roster snapshot."
  (let ((snapshot nil)
        (loaded nil))
    (dolist (buffer lispm-office--buffers)
      (when (and (buffer-live-p buffer)
                 (get-buffer-window buffer 'visible)
                 (buffer-local-value 'lispm-office--dirty buffer))
        (unless loaded
          (setq snapshot (lispm-agents-roster)
                loaded t))
        (with-current-buffer buffer
          (lispm-office--render snapshot))))))

(defun lispm-office--tick ()
  "Advance animation phase only in visible Office buffers with active work."
  (dolist (buffer lispm-office--buffers)
    (when (and (buffer-live-p buffer)
               (get-buffer-window buffer 'visible))
      (with-current-buffer buffer
        (when (lispm-office--animated-p)
          (cl-incf lispm-office--phase)
          (lispm-office--draw-current))))))

(defun lispm-office--roster-changed (_keys _fields &optional event)
  "Mark Office buffers dirty and enqueue transient scene EVENT envelopes."
  (let ((now (float-time))
        (animated-ev (and (plist-get event :type)
                          (memq (plist-get event :type)
                                '(message-queued message-taken message-delivered))
                          (list :type (plist-get event :type)
                                :from (plist-get event :from)
                                :to (plist-get event :to)
                                :at (float-time)
                                :progress 0.1))))
    (dolist (buffer lispm-office--buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq lispm-office--dirty t)
          (when animated-ev
            (push (copy-sequence animated-ev) lispm-office--events)
            (setq lispm-office--events (seq-take lispm-office--events 24)))
          (ignore now)))))
  (lispm-office--visible))

(defun lispm-office--attach ()
  "Register buffer locally and activate shared observers and animation timer."
  (cl-pushnew (current-buffer) lispm-office--buffers)
  (add-hook 'lispm-agents-roster-change-functions #'lispm-office--roster-changed)
  (add-hook 'window-state-change-hook #'lispm-office--visible)
  (unless (timerp lispm-office--timer)
    (setq lispm-office--timer
          (run-at-time lispm-office-animate-interval lispm-office-animate-interval
                       #'lispm-office--tick))))

(defun lispm-office--detach ()
  "Unregister current buffer and stop shared observers and timer when idle."
  (setq lispm-office--buffers (delq (current-buffer) lispm-office--buffers))
  (unless lispm-office--buffers
    (when (timerp lispm-office--timer)
      (cancel-timer lispm-office--timer)
      (setq lispm-office--timer nil))
    (remove-hook 'lispm-agents-roster-change-functions #'lispm-office--roster-changed)
    (remove-hook 'window-state-change-hook #'lispm-office--visible)))

;;;; Rendering, selection, and hit-testing

(defvar-keymap lispm-office--scene-map
  :doc "Mouse keymap over the rendered Office SVG scene."
  "<mouse-1>" #'lispm-office-click
  "<double-mouse-1>" #'lispm-office-double-click
  "<mouse-3>" #'lispm-office-mouse-inspect)

(defun lispm-office--target-size ()
  "Return pixel `(WIDTH . HEIGHT)' of the window displaying this Office."
  (if-let* ((window (get-buffer-window (current-buffer) 'visible)))
      (cons (max 240 (window-body-width window t))
            (max 200 (window-body-height window t)))
    '(880 . 560)))

(defun lispm-office--compose-inputs (plan)
  "Compute `(DISPLAY-PLAN . SCALE)' for PLAN matching window and zoom state."
  (let* ((size (lispm-office--target-size))
         (fitted (and lispm-office--room-id
                      (lispm-ui-scene-fit-plan
                       plan lispm-office--room-id (car size) (cdr size)))))
    (if fitted
        (cons (plist-get fitted :plan)
              (min lispm-office--scale (plist-get fitted :scale)))
      (let* ((pw (max 1.0 (or (plist-get plan :width) 450)))
             (ph (max 1.0 (or (plist-get plan :height) 454)))
             (auto-scale (min (max 0.22 (min (/ (float (car size)) pw)
                                             (/ (float (cdr size)) ph)))
                              lispm-office--scale)))
        (cons plan auto-scale)))))

(defun lispm-office--draw-current ()
  "Draw the already-reconciled `lispm-office--plan' into this buffer."
  (let* ((inhibit-read-only t)
         (now (float-time))
         (plan lispm-office--plan)
         (visits (or lispm-office--visits
                     (setq lispm-office--visits (make-hash-table :test #'equal))))
         (inputs (lispm-office--compose-inputs plan))
         (display-plan (car inputs))
         (scale (cdr inputs))
         (events (progn (lispm-office--prune-events now)
                        (mapcar (lambda (ev)
                                  (let ((age (- now (or (plist-get ev :at) now))))
                                    (plist-put (copy-sequence ev) :progress
                                               (min 1.0 (max 0.0 (/ age 1.2))))))
                                lispm-office--events)))
         (frame (lispm-ui-scene-compose
                 display-plan :selected lispm-office--selected
                 :scale scale :phase lispm-office--phase
                 :room-id lispm-office--room-id :events events
                 :visits visits :now now)))
    (setq lispm-office--frame frame
          lispm-office--dirty nil
          mode-line-process (format " [%s]" (lispm-office-scope-label lispm-office-scope)))
    (erase-buffer)
    (insert (propertize
             (format "LispM Office — %s  (%d agents)\n"
                     (lispm-office-scope-label lispm-office-scope)
                     (length (plist-get plan :nodes)))
             'face 'bold))
    (cond
     ((null (plist-get plan :nodes))
      (insert (propertize
               "No live agents in this scope yet. Press `a' for All Projects or start an agent.\n"
               'face 'shadow)))
     ((lispm-office--graphic-p)
      (insert (propertize " "
                          'display (lispm-ui-svg-image (lispm-ui-scene-frame-svg frame))
                          'keymap lispm-office--scene-map
                          'pointer 'hand))
      (insert "\n"))
     (t
      (lispm-ui-scene-render-text
       display-plan lispm-office--selected lispm-office--room-id
       'lispm-office-key 'lispm-office-room)))
    (goto-char (point-min))))

(defun lispm-office--render (&optional snapshot)
  "Reconcile live agents from optional SNAPSHOT and redraw this Office view."
  (let* ((records (lispm-office-scope-records lispm-office-scope snapshot))
         (base (or lispm-office--layout
                   (setq lispm-office--layout (lispm-office--read-layout))))
         (state (lispm-ui-scene-reconcile base records))
         (plan (lispm-ui-scene-plan state records)))
    (setq lispm-office--layout state
          lispm-office--plan plan)
    (when (and lispm-office--selected
               (not (lispm-office-scope-find-record lispm-office--selected records)))
      (setq lispm-office--selected nil))
    (unless lispm-office--selected
      (setq lispm-office--selected (plist-get (car (plist-get plan :nodes)) :key)))
    (when (and (null lispm-office--room-id)
               (= (length (plist-get plan :rooms)) 1))
      (setq lispm-office--room-id (plist-get (car (plist-get plan :rooms)) :id)))
    (lispm-office--write-layout state)
    (lispm-office--draw-current)))

(defun lispm-office--key-at-point ()
  "Return scene key under point in text mode or currently selected key."
  (or (get-text-property (point) 'lispm-office-key)
      lispm-office--selected
      (user-error "No agent selected")))

;;;; Interactive commands

(defun lispm-office-refresh ()
  "Refresh live agents and redraw the Office view."
  (interactive)
  (lispm-office--render (lispm-agents-roster)))

(defun lispm-office--move (dx dy)
  "Select nearest desk from current selection in unit direction (DX, DY)."
  (let* ((nodes (plist-get (or (when lispm-office--frame
                                 (lispm-ui-scene-frame-plan lispm-office--frame))
                               lispm-office--plan)
                           :nodes))
         (current (seq-find (lambda (n) (equal lispm-office--selected (plist-get n :key)))
                            nodes))
         (next (or (lispm-ui-scene-nearest-node nodes current dx dy)
                   (car nodes))))
    (when next
      (setq lispm-office--selected (plist-get next :key))
      (lispm-office--draw-current)
      (lispm-office-inspect-sync lispm-office--selected))))

(defun lispm-office-next ()
  "Move selection right or to the next agent desk."
  (interactive)
  (lispm-office--move 1 0))

(defun lispm-office-previous ()
  "Move selection left or to the previous agent desk."
  (interactive)
  (lispm-office--move -1 0))

(defun lispm-office-down ()
  "Move selection downward to a nearby desk."
  (interactive)
  (lispm-office--move 0 1))

(defun lispm-office-up ()
  "Move selection upward to a nearby desk."
  (interactive)
  (lispm-office--move 0 -1))

(defun lispm-office-click (event)
  "Handle single click EVENT inside the SVG Office scene."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (win (posn-window posn))
              (xy (posn-object-x-y posn)))
    (with-selected-window win
      (when-let* ((hit (and lispm-office--frame
                            (lispm-ui-scene-hit-test
                             lispm-office--frame (car xy) (cdr xy)))))
        (cond
         ((plist-get hit :action)
          (setq lispm-office--selected (plist-get hit :key))
          (lispm-office-action-dispatch (plist-get hit :action) (plist-get hit :key))
          (lispm-office-refresh))
         ((plist-get hit :key)
          (let ((key (plist-get hit :key)))
            (if (equal key lispm-office--selected)
                (lispm-office-inspect-open key)
              (setq lispm-office--selected key)
              (lispm-office--draw-current)
              (lispm-office-inspect-sync key))))
         ((plist-get hit :room)
          (setq lispm-office--room-id (plist-get hit :room)
                lispm-office--scale 1.0)
          (lispm-office--draw-current)))))))

(defun lispm-office-double-click (event)
  "Open the clicked agent's conversation on double click EVENT."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (win (posn-window posn))
              (xy (posn-object-x-y posn)))
    (with-selected-window win
      (when-let* ((hit (and lispm-office--frame
                            (lispm-ui-scene-hit-test
                             lispm-office--frame (car xy) (cdr xy))))
                  (key (plist-get hit :key)))
        (setq lispm-office--selected key)
        (lispm-office-action-open-chat key)))))

(defun lispm-office-mouse-inspect (event)
  "Open property-sheet inspector for the agent clicked at EVENT."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (win (posn-window posn))
              (xy (posn-object-x-y posn)))
    (with-selected-window win
      (when-let* ((hit (and lispm-office--frame
                            (lispm-ui-scene-hit-test
                             lispm-office--frame (car xy) (cdr xy))))
                  (key (plist-get hit :key)))
        (setq lispm-office--selected key)
        (lispm-office--draw-current)
        (lispm-office-inspect-open key)))))

(defun lispm-office-open ()
  "Inspect the selected agent (or room under point in text mode)."
  (interactive)
  (let ((key (lispm-office--key-at-point)))
    (setq lispm-office--selected key)
    (lispm-office--draw-current)
    (lispm-office-inspect-open key)))

(defun lispm-office-inspect ()
  "Open the property-sheet inspector for the selected agent."
  (interactive)
  (lispm-office-inspect-open (lispm-office--key-at-point)))

(defun lispm-office-chat ()
  "Open the selected agent's live conversation buffer."
  (interactive)
  (lispm-office-action-open-chat (lispm-office--key-at-point)))

(defun lispm-office-transcript ()
  "Open the selected native agent's Org transcript."
  (interactive)
  (lispm-office-action-open-transcript (lispm-office--key-at-point)))

(defun lispm-office-send-message ()
  "Prompt for and deliver a message to the selected agent."
  (interactive)
  (lispm-office-action-send-message (lispm-office--key-at-point))
  (lispm-office-refresh))

(defun lispm-office-approve ()
  "Approve the selected native agent's pending call or condition."
  (interactive)
  (lispm-office-action-approve (lispm-office--key-at-point))
  (lispm-office-refresh))

(defun lispm-office-abort ()
  "Abort the selected native agent's running turn."
  (interactive)
  (lispm-office-action-abort (lispm-office--key-at-point))
  (lispm-office-refresh))

(defun lispm-office-diff ()
  "Show the selected native agent's latest turn diff."
  (interactive)
  (lispm-office-action-diff (lispm-office--key-at-point)))

(defun lispm-office-zoom-in ()
  "Zoom closer into the Office floor plan."
  (interactive)
  (setq lispm-office--scale (min 1.5 (* lispm-office--scale 1.25)))
  (lispm-office--draw-current))

(defun lispm-office-zoom-out ()
  "Zoom out toward the building overview."
  (interactive)
  (setq lispm-office--scale (max 0.25 (* lispm-office--scale 0.8)))
  (when (< lispm-office--scale 0.45)
    (setq lispm-office--room-id nil))
  (lispm-office--draw-current))

(defun lispm-office-zoom-reset ()
  "Reset zoom to Overview across all visible rooms."
  (interactive)
  (setq lispm-office--room-id nil
        lispm-office--scale 1.0)
  (lispm-office--draw-current))

(defun lispm-office-toggle-scope ()
  "Toggle between current project scope and All Projects."
  (interactive)
  (let ((project (or lispm-office--project-directory
                     (lispm-office-scope-normalize default-directory))))
    (setq lispm-office-scope (if lispm-office-scope nil project)
          lispm-agents-work-directory lispm-office-scope
          lispm-office--room-id nil)
    (lispm-office-refresh)))

(defun lispm-office-toggle-view ()
  "Toggle between SVG graphics (when available) and accessible text."
  (interactive)
  (setq lispm-office--view-mode (if (lispm-office--graphic-p) 'text 'svg))
  (lispm-office--draw-current))

(defun lispm-office-compact ()
  "Compact empty reserved desks in the current Office seating plan."
  (interactive)
  (let* ((records (lispm-office-scope-records nil (lispm-agents-roster)))
         (state (lispm-ui-scene-compact lispm-office--layout records)))
    (setq lispm-office--layout state)
    (lispm-office--write-layout state)
    (lispm-office--render)))

;;;; Mode & entrypoints

(defvar-keymap lispm-office-mode-map
  :doc "Keymap for `lispm-office-mode'."
  :parent special-mode-map
  "RET" #'lispm-office-open
  "i" #'lispm-office-inspect
  "c" #'lispm-office-chat
  "t" #'lispm-office-transcript
  "m" #'lispm-office-send-message
  "y" #'lispm-office-approve
  "x" #'lispm-office-abort
  "d" #'lispm-office-diff
  "n" #'lispm-office-next
  "p" #'lispm-office-previous
  "<right>" #'lispm-office-next
  "<left>" #'lispm-office-previous
  "<down>" #'lispm-office-down
  "<up>" #'lispm-office-up
  "+" #'lispm-office-zoom-in
  "=" #'lispm-office-zoom-in
  "-" #'lispm-office-zoom-out
  "0" #'lispm-office-zoom-reset
  "a" #'lispm-office-toggle-scope
  "v" #'lispm-office-toggle-view
  "C" #'lispm-office-compact
  "g" #'lispm-office-refresh)

(define-derived-mode lispm-office-mode special-mode "Office"
  "Spatial floor-plan view of live LispM and terminal agents."
  (setq-local truncate-lines t
              cursor-type nil
              revert-buffer-function (lambda (&rest _) (lispm-office-refresh)))
  (when lispm-office-show-tabs
    (setq-local header-line-format '(:eval (lispm-office--header-line))))
  (add-hook 'kill-buffer-hook #'lispm-office--detach nil t)
  (add-hook 'change-major-mode-hook #'lispm-office--detach nil t)
  (lispm-office--attach))

(lispm-kit-define-commands lispm-office-mode
  :menu "Office"
  (lispm-office-open :key "RET" :label "Inspect Agent"
                     :help "Inspect the selected agent")
  (lispm-office-chat :key "c" :label "Open Chat"
                     :help "Switch to the selected agent's conversation")
  (lispm-office-send-message :key "m" :label "Message"
                             :help "Send a message to the selected agent")
  (lispm-office-approve :key "y" :label "Approve"
                        :help "Approve pending permission or restart")
  (lispm-office-toggle-scope :key "a" :label "All / Project"
                             :help "Toggle between current project and all projects")
  (lispm-office-toggle-view :key "v" :label "Text / Scene"
                            :help "Toggle between graphical scene and accessible text")
  (lispm-office-refresh :key "g" :label "Refresh"
                        :help "Refresh live Office view"))

(defun lispm-office--display-buffer (directory)
  "Create or reuse, render, and return the Office buffer for DIRECTORY."
  (let* ((scope (lispm-office-scope-normalize directory))
         (buffer (or (seq-find
                      (lambda (buf)
                        (and (buffer-live-p buf)
                             (equal scope (buffer-local-value 'lispm-office-scope buf))))
                      lispm-office--buffers)
                     (generate-new-buffer
                      (format "*Office: %s*" (lispm-office-scope-label scope))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-office-mode)
        (lispm-office-mode))
      (setq lispm-office-scope scope
            lispm-agents-work-directory scope)
      (when scope
        (setq lispm-office--project-directory scope))
      (when (and scope (file-directory-p scope))
        (setq default-directory (file-name-as-directory scope)))
      (lispm-office--render))
    (funcall (or lispm-office-display-function
                 lispm-agents-work-display-function
                 #'pop-to-buffer-same-window)
             buffer)
    buffer))

;;;###autoload
(defun lispm-office (&optional directory)
  "Open the spatial Office view for DIRECTORY, or all projects when nil."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Office project: "))))
  (lispm-office--display-buffer directory))

;;;###autoload
(defun lispm-office-open-project (directory)
  "Open the spatial Office view for project DIRECTORY."
  (interactive (list (read-directory-name "Project: ")))
  (lispm-office--display-buffer directory))

;;;###autoload
(defun lispm-office-toggle ()
  "Toggle built-in Office integration on or off globally."
  (interactive)
  (setq lispm-office-enabled (not lispm-office-enabled))
  (lispm-office--apply-enabled)
  (cond
   ((and (not lispm-office-enabled) (derived-mode-p 'lispm-office-mode))
    (when-let* ((dir lispm-office-scope))
      (lispm-agents-open-project dir)))
   ((and lispm-office-enabled (derived-mode-p 'lispm-agents-work-mode))
    (lispm-office-open-project lispm-agents-work-directory)))
  lispm-office-enabled)

(defun lispm-office-unload-function ()
  "Detach all Office buffers, observers, timers, and chat header-line hooks."
  (let ((lispm-office-enabled nil))
    (lispm-office--apply-enabled))
  (dolist (buffer (copy-sequence lispm-office--buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (lispm-office--detach)
        (fundamental-mode))))
  (lispm-office-inspect-unload-function)
  nil)

(lispm-office--apply-enabled)

(provide 'lispm-office)
;;; lispm-office.el ends here
