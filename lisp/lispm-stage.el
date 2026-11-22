;;; lispm-stage.el --- Reactive agent stage adapter over lispm-ui-stage  -*- lexical-binding: t; -*-

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

;; Bridges LispM agent sessions, subagent lineage, and host workspace modes
;; onto the generic `lispm-ui-stage' FRP component:
;;
;; - Maps each root conversation agent to a reactive stage subject.
;; - Switches stage tiles dynamically when the user selects another agent,
;;   and closes stage panes whenever the selected agent is idle.
;; - Suspends stage projection reactively via `lispm-stage-enabled-signal'
;;   whenever a host workspace enters pure file or shell mode.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-ui-frp)
(require 'lispm-ui-stage)

(declare-function lispm-agent-view "lispm-core" (agent))
(declare-function lispm-agent-busy-p "lispm-core" (agent))
(declare-function lispm-agents-buffer-p "lispm-agents" (buffer))

(defvar lispm-stage-enabled-signal (lispm-ui-signal t #'eq)
  "Reactive boolean signal enabling (t) or suspending (nil) stage panes.")

(defvar lispm-stage-enabled-predicate nil
  "Optional host predicate returning non-nil when stage panes are allowed.")

(defvar lispm-stage-conversation-buffer-predicate nil
  "Optional host predicate `(fn BUFFER)' returning non-nil for session buffers.")

(defvar lispm-stage-excluded-window-predicate nil
  "Optional host predicate `(fn WINDOW)' returning non-nil for backdrop windows.")

(defvar lispm-stage-frame-function nil
  "Optional host function returning the preferred splittable editor frame.")

(defgroup lispm-stage nil
  "Adaptive multi-pane stage layout and focus zoom for agents."
  :group 'lispm
  :prefix "lispm-stage-")

(defcustom lispm-stage-min-sizes
  '((code  . (70 . 12))
    (shell . (56 . 8))
    (lisp  . (56 . 8))
    (web   . (68 . 12)))
  "Minimum `(WIDTH . HEIGHT)' in character cells for each stage view kind."
  :type '(alist :key-type symbol
                :value-type (cons (integer :tag "Min columns")
                                  (integer :tag "Min lines")))
  :group 'lispm-stage)

(defcustom lispm-stage-chat-min-width 52
  "Minimum width in columns preserved for the conversation window."
  :type 'integer
  :group 'lispm-stage)

(defcustom lispm-stage-max-tiles 4
  "Maximum number of simultaneous tiled panes in the stage area."
  :type 'integer
  :group 'lispm-stage)

(defcustom lispm-stage-zoom-timeout 10
  "Seconds of inactivity in a focused stage pane before tiling resumes.
Clicking back into the conversation prompt resumes tiling immediately.
When nil, the stage stays zoomed until the conversation is selected."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Until conversation selected" nil))
  :group 'lispm-stage)

(defvar-local lispm-ui-stage-managed nil
  "Non-nil in ephemeral or synthetic buffers owned by the agent stage.")
(put 'lispm-ui-stage-managed 'permanent-local t)

;;;; Agent lineage & window predicates

(defun lispm-stage-agent-by-id (session-id)
  "Return the live `lispm-agent' whose `:id' equals SESSION-ID, or nil."
  (when (stringp session-id)
    (cdr (seq-find (lambda (cell)
                     (equal (lispm-agent-id (cdr cell)) session-id))
                   lispm--agents))))

(defun lispm-stage--parent-agent (agent)
  "Resolve AGENT's parent to a live `lispm-agent', or nil."
  (let ((parent (and agent (lispm-agent-parent agent))))
    (cond
     ((lispm-agent-p parent) parent)
     ((stringp parent) (lispm-stage-agent-by-id parent))
     (t nil))))

(defun lispm-stage-root-agent (agent)
  "Return the top-level ancestor `lispm-agent' of AGENT."
  (let ((cur agent)
        (seen nil)
        next)
    (while (and cur
                (not (memq cur seen))
                (setq next (lispm-stage--parent-agent cur)))
      (push cur seen)
      (setq cur next))
    cur))

(defun lispm-stage-descendant-p (agent ancestor)
  "Return non-nil if AGENT is ANCESTOR or a subagent descendant of ANCESTOR."
  (and agent ancestor
       (let ((cur agent)
             (seen nil)
             (found nil))
         (while (and cur (not found) (not (memq cur seen)))
           (if (eq cur ancestor)
               (setq found t)
             (push cur seen)
             (setq cur (lispm-stage--parent-agent cur))))
         found)))

(defun lispm-stage-agent-tree-busy-p (root)
  "Return non-nil when ROOT or any live subagent of ROOT is busy."
  (and (lispm-agent-p root)
       (or (lispm-agent-busy-p root)
           (seq-some (lambda (cell)
                       (let ((candidate (cdr cell)))
                         (and (lispm-agent-busy-p candidate)
                              (lispm-stage-descendant-p candidate root))))
                     lispm--agents))))

(defun lispm-stage--subject-key (agent)
  "Return the reactive subject key for AGENT's root session."
  (if-let* ((root (and (lispm-agent-p agent) (lispm-stage-root-agent agent))))
      (or (lispm-agent-id root) (lispm-agent-name root))
    ':default))

(defun lispm-stage-conversation-window-p (window)
  "Return non-nil if WINDOW displays an agent conversation or session."
  (and (window-live-p window)
       (let ((buffer (window-buffer window)))
         (or (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                      'lispm-chat-mode)
             (and (local-variable-p 'lispm-session-mode buffer)
                  (buffer-local-value 'lispm-session-mode buffer))
             (and (fboundp 'lispm-agents-buffer-p)
                  (lispm-agents-buffer-p buffer))
             (and (functionp lispm-stage-conversation-buffer-predicate)
                  (funcall lispm-stage-conversation-buffer-predicate buffer))))))

(defun lispm-stage-backdrop-window-p (window)
  "Return non-nil if WINDOW is excluded by `lispm-stage-excluded-window-predicate'."
  (and (window-live-p window)
       (functionp lispm-stage-excluded-window-predicate)
       (funcall lispm-stage-excluded-window-predicate window)))

(defun lispm-stage-splittable-frame-p (frame)
  "Return non-nil if FRAME can host tiled stage windows."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'unsplittable))
       (seq-some (lambda (window)
                   (and (not (window-dedicated-p window))
                        (not (window-parameter window 'window-side))
                        (not (lispm-stage-backdrop-window-p window))))
                 (window-list frame 'nomini))))

(defun lispm-stage--agent-own-frame (agent)
  "Return the visible splittable frame directly showing AGENT, or nil."
  (when (lispm-agent-p agent)
    (let* ((view (and (fboundp 'lispm-agent-view) (lispm-agent-view agent)))
           (session (lispm-agent-buffer agent))
           (win (or (and (buffer-live-p view) (get-buffer-window view 'visible))
                    (and (buffer-live-p session)
                         (get-buffer-window session 'visible))))
           (frame (and win (window-frame win))))
      (and frame (lispm-stage-splittable-frame-p frame) frame))))

(defun lispm-stage-agent-frame (agent)
  "Return the splittable frame showing AGENT or its nearest ancestor."
  (let ((cur agent)
        (seen nil)
        found)
    (while (and cur (not found) (not (memq cur seen)))
      (setq found (lispm-stage--agent-own-frame cur))
      (push cur seen)
      (unless found
        (setq cur (lispm-stage--parent-agent cur))))
    found))

(defun lispm-stage-target-frame (&optional agent)
  "Return the frame in which stage panes for AGENT are arranged."
  (let ((selected (selected-frame))
        (host-frame (and (functionp lispm-stage-frame-function)
                         (funcall lispm-stage-frame-function))))
    (or (and (lispm-agent-p agent) (lispm-stage-agent-frame agent))
        (and (lispm-stage-splittable-frame-p selected) selected)
        (and host-frame
             (eq (frame-visible-p host-frame) t)
             (lispm-stage-splittable-frame-p host-frame)
             host-frame)
        (seq-find (lambda (frame)
                    (and (eq (frame-visible-p frame) t)
                         (lispm-stage-splittable-frame-p frame)))
                  (frame-list))
        selected)))

;;;; Pure geometry aliases (delegating to `lispm-ui-stage')

(defun lispm-stage-min-size (kind)
  "Return `(MIN-WIDTH . MIN-HEIGHT)' required for stage KIND."
  (lispm-ui-stage-min-size kind lispm-stage-min-sizes))

(defalias 'lispm-stage--plan-split #'lispm-ui-stage-plan-split)
(defalias 'lispm-stage--plan-initial-split #'lispm-ui-stage-plan-initial-split)

;;;; Conversation bottom anchor

(defun lispm-stage--conversation-at-bottom-p (window)
  "Return non-nil if conversation WINDOW currently shows its bottom prompt."
  (and (lispm-stage-conversation-window-p window)
       (with-current-buffer (window-buffer window)
         (let ((prompt (if (and (local-variable-p 'lispm-chat--prompt-start
                                                  (current-buffer))
                                (markerp (buffer-local-value
                                          'lispm-chat--prompt-start
                                          (current-buffer))))
                           (marker-position
                            (buffer-local-value 'lispm-chat--prompt-start
                                                (current-buffer)))
                         (point-max))))
           (or (>= (window-point window) prompt)
               (pos-visible-in-window-p prompt window)
               (>= (or (window-end window) (point-max)) prompt))))))

(defun lispm-stage--anchor-conversation (window phase-or-state)
  "Capture or restore bottom scroll on conversation WINDOW via PHASE-OR-STATE."
  (if (eq phase-or-state 'capture)
      (lispm-stage--conversation-at-bottom-p window)
    (when (and phase-or-state (lispm-stage-conversation-window-p window))
      (with-selected-frame (window-frame window)
        (with-selected-window window
          (goto-char (point-max))
          (recenter -3))))))

;;;; Reactive `lispm-ui-stage' controller

(defvar lispm-stage--controller nil
  "Singleton `lispm-ui-stage' FRP controller for agent sessions.")

(defvar lispm-stage--enabled-effect nil
  "Reactive `lispm-ui-effect' observing `lispm-stage-enabled-signal'.")

(defvar lispm-stage--reap-hooks nil
  "Functions run after stage reconciliation closes panes.")

(defun lispm-stage-set-enabled (enabled)
  "Enable (t) or suspend (nil) reactive stage projection per ENABLED."
  (lispm-ui-signal-set lispm-stage-enabled-signal (and enabled t)))

(defun lispm-stage--sync-config (ctrl)
  "Propagate user custom variables onto `lispm-ui-stage' CTRL."
  (setf (lispm-ui-stage-min-sizes ctrl) lispm-stage-min-sizes
        (lispm-ui-stage-primary-min-width ctrl) lispm-stage-chat-min-width
        (lispm-ui-stage-max-tiles ctrl) lispm-stage-max-tiles
        (lispm-ui-stage-zoom-timeout ctrl) lispm-stage-zoom-timeout))

(defun lispm-stage--mode-allows-stage-p (&optional signal-enabled)
  "Return non-nil when SIGNAL-ENABLED and host predicate permit stage panes."
  (let ((on (if (null signal-enabled)
                (lispm-ui-signal-peek lispm-stage-enabled-signal)
              (eq signal-enabled 'enabled))))
    (and on
         (or (not (functionp lispm-stage-enabled-predicate))
             (funcall lispm-stage-enabled-predicate)))))

(defun lispm-stage--ensure-enabled-binding (ctrl)
  "Bind CTRL's `enabled' state reactively to `lispm-stage-enabled-signal'."
  (when (and (lispm-ui-signal-p lispm-stage-enabled-signal)
             (not (lispm-ui-effect-p lispm-stage--enabled-effect)))
    (setq lispm-stage--enabled-effect
          (lispm-ui-effect
           lispm-stage-enabled-signal
           (lambda (enabled)
             (lispm-ui-stage-set-enabled
              ctrl (lispm-stage--mode-allows-stage-p
                    (if enabled 'enabled 'disabled))))
           t))))

(defun lispm-stage--controller ()
  "Return the initialized `lispm-ui-stage' controller."
  (unless (lispm-ui-stage-p lispm-stage--controller)
    (setq lispm-stage--controller
          (lispm-ui-stage-create
           :min-sizes lispm-stage-min-sizes
           :primary-min-width lispm-stage-chat-min-width
           :max-tiles lispm-stage-max-tiles
           :zoom-timeout lispm-stage-zoom-timeout
           :primary-window-p #'lispm-stage-conversation-window-p
           :excluded-window-p #'lispm-stage-backdrop-window-p
           :resolve-frame (lambda (_subj) (lispm-stage-target-frame nil))
           :anchor-primary-fn #'lispm-stage--anchor-conversation
           :on-reconcile (lambda (spec _frame)
                           (when (memq (plist-get spec :mode) '(closed disabled))
                             (run-hooks 'lispm-stage--reap-hooks))))))
  (lispm-stage--sync-config lispm-stage--controller)
  (lispm-stage--ensure-enabled-binding lispm-stage--controller)
  lispm-stage--controller)

(defun lispm-stage-reset ()
  "Dispose and recreate the reactive stage controller (for test isolation)."
  (when (lispm-ui-effect-p lispm-stage--enabled-effect)
    (lispm-ui-effect-dispose lispm-stage--enabled-effect)
    (setq lispm-stage--enabled-effect nil))
  (when (lispm-ui-stage-p lispm-stage--controller)
    (lispm-ui-stage-dispose lispm-stage--controller)
    (setq lispm-stage--controller nil))
  (lispm-ui-signal-set lispm-stage-enabled-signal t))

;;;; Window & agent state synchronization

(defun lispm-stage--window-chat-agent (window)
  "Return the `lispm-agent' bound to conversation WINDOW, or nil."
  (when (lispm-stage-conversation-window-p window)
    (let ((buf (window-buffer window)))
      (cond
       ((and (local-variable-p 'lispm-chat-agent buf)
             (buffer-local-value 'lispm-chat-agent buf)))
       ((and (local-variable-p 'lispm-session-agent buf)
             (buffer-local-value 'lispm-session-agent buf)))))))

(defun lispm-stage--primary-conversation-window (frame)
  "Return the active or most recently used conversation window on FRAME."
  (when (frame-live-p frame)
    (let ((sel (frame-selected-window frame)))
      (if (lispm-stage-conversation-window-p sel)
          sel
        (car (sort (seq-filter #'lispm-stage-conversation-window-p
                               (window-list frame 'nomini))
                   (lambda (a b) (> (window-use-time a) (window-use-time b)))))))))

(defun lispm-stage--frame-subject-info (frame &optional fallback-agent)
  "Determine `(SUBJECT-KEY . ROOT-AGENT)' displayed on FRAME.
When no conversation window exists on FRAME, fall back to FALLBACK-AGENT."
  (if-let* ((chat-win (lispm-stage--primary-conversation-window frame)))
      (if-let* ((agent (lispm-stage--window-chat-agent chat-win))
                (root (lispm-stage-root-agent agent)))
          (cons (lispm-stage--subject-key root) root)
        (let ((buf (window-buffer chat-win)))
          (if (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                       'lispm-chat-mode)
              (cons ':default nil)
            (cons ':external-session nil))))
    (if fallback-agent
        (let ((root (lispm-stage-root-agent fallback-agent)))
          (cons (lispm-stage--subject-key root) root))
      (cons nil nil))))

(defun lispm-stage-lineage-busy-p (&optional frame)
  "Return non-nil if any conversation agent on FRAME (or its subagent) is busy."
  (let* ((f (or frame (selected-frame)))
         (roots (delq nil (mapcar #'lispm-stage--window-chat-agent
                                  (window-list f 'nomini)))))
    (if roots
        (seq-some #'lispm-stage-agent-tree-busy-p roots)
      (seq-some (lambda (cell) (lispm-agent-busy-p (cdr cell)))
                lispm--agents))))

(defun lispm-stage--windows (frame)
  "Return live stage windows on FRAME."
  (lispm-ui-stage-windows (lispm-stage--controller) frame))

(defun lispm-stage-zoomed-p (&optional _frame)
  "Return non-nil if the stage is currently in active focus-zoom mode."
  (lispm-ui-stage-zoomed-p (lispm-stage--controller)))

(defun lispm-stage-retains-buffer-p (buffer)
  "Return non-nil if BUFFER is retained in zoom or active stage tiles."
  (lispm-ui-stage-retains-buffer-p (lispm-stage--controller) buffer))

(defun lispm-stage-zoom (window)
  "Maximize stage WINDOW across the stage column and freeze switches."
  (lispm-ui-stage-zoom (lispm-stage--controller) window))

(defun lispm-stage-unzoom (&optional _frame move-focus-p)
  "Leave focus-zoom; re-tile only if the active agent is still working.
When MOVE-FOCUS-P is non-nil, return focus to the conversation window."
  (lispm-ui-stage-unzoom (lispm-stage--controller) move-focus-p))

(defun lispm-stage-set-zoom-last-active (timestamp)
  "Set the active zoom's `last-active' clock to TIMESTAMP (used in tests)."
  (let* ((ctrl (lispm-stage--controller))
         (zoom (lispm-ui-signal-peek (lispm-ui-stage-zoom-signal ctrl))))
    (when (lispm-ui-stage-zoom-p zoom)
      (setf (lispm-ui-stage-zoom-last-active zoom) timestamp))))

(defun lispm-stage--check-zoom-timeout ()
  "Check whether the active zoom has exceeded `lispm-stage-zoom-timeout'."
  (let ((ctrl (lispm-stage--controller)))
    (lispm-stage--sync-config ctrl)
    (lispm-ui-stage--check-timeout ctrl)))

;;;; Stage window presentation entry point

(defun lispm-stage-window (buffer &optional follow agent kind)
  "Return a stage window showing BUFFER, tiling or reusing panes if FOLLOW.
AGENT anchors the subject lineage; KIND (`code', `shell', `lisp', `web')
sets minimum pane dimensions."
  (let* ((ctrl (lispm-stage--controller))
         (kind (or kind 'code))
         (root (and (lispm-agent-p agent) (lispm-stage-root-agent agent)))
         (subj (if root (lispm-stage--subject-key root) ':default))
         (frame (lispm-stage-target-frame root))
         (allowed (lispm-stage--mode-allows-stage-p)))
    (when (and (buffer-live-p buffer)
               (not (buffer-file-name buffer))
               (string-prefix-p "*lispm-stage-" (buffer-name buffer)))
      (with-current-buffer buffer
        (setq-local lispm-ui-stage-managed t)))
    (lispm-ui-frp-batch
      (lispm-ui-stage-set-enabled ctrl allowed)
      (pcase-let ((`(,frame-subj . ,_)
                   (lispm-stage--frame-subject-info frame root)))
        (when (and frame-subj (not (equal frame-subj (lispm-ui-stage-subject ctrl))))
          (lispm-ui-stage-set-subject ctrl frame-subj))))
    (if (not follow)
        (and (lispm-ui-stage-enabled-p ctrl)
             (get-buffer-window buffer 'visible))
      (lispm-ui-stage-present ctrl subj buffer kind t))))

;;;; Event hooks feeding the reactive graph

(defun lispm-stage--on-agent-state (agent)
  "Feed AGENT's busy/idle transition into the reactive stage controller."
  (when (lispm-agent-p agent)
    (let* ((ctrl (lispm-stage--controller))
           (root (lispm-stage-root-agent agent))
           (subj (lispm-stage--subject-key root))
           (busy (lispm-stage-agent-tree-busy-p root)))
      (lispm-ui-stage-set-busy ctrl subj busy t))))

(defun lispm-stage--on-session-end (agent)
  "Remove AGENT's root session from the stage controller when retired."
  (when (lispm-agent-p agent)
    (let ((root (lispm-stage-root-agent agent)))
      (when (eq agent root)
        (lispm-ui-stage-remove-subject
         (lispm-stage--controller) (lispm-stage--subject-key root))))))

(defun lispm-stage--sync-frame-subject (frame)
  "Synchronize FRAME's currently displayed conversation agent into stage."
  (let ((ctrl (lispm-stage--controller)))
    (unless (or (lispm-ui-stage-inhibit-hooks ctrl)
                (not (frame-live-p frame))
                (not (lispm-stage-splittable-frame-p frame)))
      (pcase-let* ((`(,subj . ,root) (lispm-stage--frame-subject-info frame))
                   (prev (lispm-ui-stage-subject ctrl))
                   (allowed (lispm-stage--mode-allows-stage-p)))
        (lispm-ui-frp-batch
          (lispm-ui-stage-set-enabled ctrl allowed)
          (when (and subj (not (equal subj prev)))
            (when root
              (lispm-ui-stage-set-busy
               ctrl subj (lispm-stage-agent-tree-busy-p root)))
            (lispm-ui-stage-set-subject ctrl subj))
          (when (and (null subj) prev
                     (not (lispm-stage--primary-conversation-window frame))
                     (null (lispm-ui-stage-windows ctrl frame)))
            (lispm-ui-stage-set-subject ctrl nil)))))))

(defun lispm-stage--on-window-buffer-change (window-or-frame)
  "React to buffer switches on WINDOW-OR-FRAME to switch or close stage."
  (unless noninteractive
    (let ((frame (if (framep window-or-frame)
                     window-or-frame
                   (and (window-live-p window-or-frame)
                        (window-frame window-or-frame)))))
      (when frame
        (lispm-stage--sync-frame-subject frame)))))

(defun lispm-stage--on-window-selection-change (frame)
  "Synchronize subject, busy state, and zoom selection on FRAME."
  (let ((ctrl (lispm-stage--controller)))
    (unless (or noninteractive
                (lispm-ui-stage-inhibit-hooks ctrl)
                (not (frame-live-p frame)))
      (pcase-let ((`(,subj . ,root) (lispm-stage--frame-subject-info frame)))
        (lispm-ui-frp-batch
          (lispm-ui-stage-set-enabled ctrl (lispm-stage--mode-allows-stage-p))
          (when root
            (lispm-ui-stage-set-busy
             ctrl subj (lispm-stage-agent-tree-busy-p root)))
          (when (and subj (not (equal subj (lispm-ui-stage-subject ctrl))))
            (lispm-ui-stage-set-subject ctrl subj)))
        (lispm-ui-stage-on-selection-change ctrl frame)))))

(defun lispm-stage--touch-zoom ()
  "Refresh zoom activity timestamp while editing inside a zoomed stage pane."
  (when (lispm-ui-stage-p lispm-stage--controller)
    (lispm-ui-stage-touch-zoom lispm-stage--controller)))

(add-hook 'lispm-state-change-functions #'lispm-stage--on-agent-state)
(add-hook 'lispm-session-end-functions #'lispm-stage--on-session-end)
(add-hook 'window-buffer-change-functions #'lispm-stage--on-window-buffer-change)
(add-hook 'window-selection-change-functions #'lispm-stage--on-window-selection-change)
(add-hook 'post-command-hook #'lispm-stage--touch-zoom)

(provide 'lispm-stage)
;;; lispm-stage.el ends here
