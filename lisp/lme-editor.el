;;; lme-editor.el --- Emacs, the editor, as an application  -*- lexical-binding: t; -*-

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

;; On the desktop, Emacs the editor is an application, as the browser is:
;; its icon opens it, the Application menu brings it forward, and while it
;; is in front the menu bar holds its menus, File, Edit, View, Project and
;; Window.  Its workspaces, one per project, live inside it.  A title
;; strip runs across its top: the editor's title bar, naming the
;; workspace, over a row of tabs choosing the sidebar's view (Files,
;; Agents, Shells).
;;
;; The editor is drawn in one of two ways, and its zoom box moves it from
;; one to the other:
;;
;;   full screen   in the frame's own windows, under the menu bar, the
;;                 desktop's workspace out of sight
;;   floating      in a child frame over the desktop, moved by its title
;;                 bar and resized from its grow box or its edges
;;
;; Moving carries every workspace along: each is a tab, and goes to the
;; other frame as its window state.  The title strip and the sidebar are
;; side windows, which a window state does not carry, so they come off
;; before the move and are put back after it.
;;
;; Documents opened while the desktop or the browser is in front, files,
;; folders, terminals and agents, open in the editor, which comes forward.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lme-core)
(require 'lme-platinum)
(require 'lme-apps)
(require 'lme-desktop)

(declare-function lme-sidebar-current-view "lme-sidebar" ())
(declare-function lme-sidebar-select "lme-sidebar" (view))
(declare-function lme-terminal-buffer-p "lme-interface" (buffer))
(declare-function lme-main-area--windows "lme-interface" ())
(declare-function lme-buffer-manager "lme-buffer-manager" ())
(declare-function lme-popup--keyboard-p "lme-popup" (keys))
(declare-function tabspaces--current-tab-name "tabspaces" ())
(declare-function tabspaces-local-buffer-list "tabspaces" (&optional frame tabnum))
(declare-function tabspaces--set-buffer-predicate "tabspaces" (frame))
(defvar lme-sidebar-views)
(defvar lme-browser-workspace-name)
(defvar tabspaces-default-tab)

(defvar lme-editor--frame nil
  "The floating editor's child frame, or nil while the editor is full screen.")

(defvar lme-editor--geometry nil
  "Where the floating editor was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lme-editor--last-workspace nil
  "Name of the workspace the full-screen editor showed when it was left.")

(defvar lme-editor--moving nil
  "Non-nil while the editor moves between full screen and floating.")

(defvar lme-editor--shade nil
  "While the floating editor is rolled up: its windows and text height.")

(defvar lme-editor--closed nil
  "Non-nil once the floating editor was closed, until it is opened again.
A floating editor hidden only while the browser was in front shows again
with the desktop; a closed one does not.")

(defun lme-editor-floating-p ()
  "Return non-nil while the editor floats in a window of its own."
  (frame-live-p lme-editor--frame))

(defun lme-editor-frame ()
  "Return the frame the editor is drawn in."
  (if (lme-editor-floating-p) lme-editor--frame (lme-apps-screen)))

;;;; Workspaces

(defun lme-editor--workspace-tab-p (tab)
  "Return non-nil when TAB is one of the editor's workspaces.
The desktop's and the browser's workspaces are not."
  (not (member (alist-get 'name tab)
               (list lme-desktop-tab-name (bound-and-true-p lme-browser-workspace-name)))))

(defun lme-editor--in-workspace-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, shows an editor
workspace."
  (lme-editor--workspace-tab-p (tab-bar--current-tab nil frame)))

(defun lme-editor-workspaces ()
  "Return the names of the editor's workspaces, in order."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (seq-filter #'lme-editor--workspace-tab-p
                      (funcall tab-bar-tabs-function (lme-editor-frame)))))

(defun lme-editor--frame-p (frame)
  "Return non-nil when FRAME draws the editor: its floating window, or the
screen while one of its workspaces is in front.  Popups, child frames that
are no application's window, go with the editor."
  (cond ((eq frame lme-editor--frame) t)
        ((frame-parent frame) (not (frame-parameter frame 'lme-window)))
        (t (lme-editor--in-workspace-p frame))))

(defun lme-editor--current-workspace ()
  "Return the name of the editor's workspace in front, or nil."
  (let ((frame (lme-editor-frame)))
    (when (lme-editor--in-workspace-p frame)
      (alist-get 'name (tab-bar--current-tab nil frame)))))

(defun lme-editor--remember-workspace (from-tab _to-tab)
  "Remember FROM-TAB when the full screen editor's workspace is left.
This is a `tab-bar-tab-post-select-functions' function."
  (when (and (not lme-editor--moving) (lme-editor--workspace-tab-p from-tab))
    (setq lme-editor--last-workspace (alist-get 'name from-tab))))

;;;; The title strip

(defconst lme-editor-strip-buffer-name " *Emacs*"
  "Name of the buffer of the editor's title strip.")

(defun lme-editor--active-p (_window)
  "Return non-nil while the editor is in front: its title bar is then active."
  (eq (lme-apps-frontmost) 'editor))

(defun lme-editor--title ()
  "Return the editor's title: Emacs, and the workspace in front."
  (if-let* ((workspace (lme-editor--current-workspace)))
      (format "Emacs — %s" workspace)
    "Emacs"))

(defvar-keymap lme-editor-tab-map
  :doc "Keymap of a tab of the editor's title strip."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lme-editor-tab-click)

(defun lme-editor-tab-click (event)
  "Show the sidebar view whose tab EVENT clicked, or hide it when shown."
  (interactive "e")
  (let* ((position (event-start event))
         (view (get-text-property (posn-point position) 'lme-editor-view
                                  (window-buffer (posn-window position)))))
    (when view
      (with-selected-frame (window-frame (posn-window position))
        (lme-sidebar-select view)))))

(defun lme-editor--tab (view selected)
  "Return the strip's tab for sidebar VIEW, pushed in when SELECTED."
  (let* ((spec (cdr view))
         (label (concat " " (lme-platinum-icon (plist-get spec :icon)) " "
                        (plist-get spec :title) " ")))
    (add-face-text-property 0 (length label)
                            (if selected 'lme-platinum-tab-selected 'lme-platinum-tab)
                            t label)
    (propertize label 'lme-editor-view (car view) 'keymap lme-editor-tab-map
                'help-echo (plist-get spec :help) 'pointer 'arrow)))

(defun lme-editor--render-strip ()
  "Draw the title strip: its title, and a tab per sidebar view."
  (when-let* ((buffer (get-buffer lme-editor-strip-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (shown (with-selected-frame (lme-editor-frame) (lme-sidebar-current-view))))
        (erase-buffer)
        (insert (propertize " " 'display '(space :width (8) :height (30) :ascent 65))
                (mapconcat (lambda (view) (lme-editor--tab view (eq (car view) shown)))
                           lme-sidebar-views " "))
        (setq lme-platinum-title (lme-editor--title)
              ;; Only a floating window rolls up into its title bar.
              lme-platinum-box-functions
              `(:close lme-editor--close-box :zoom lme-editor--zoom-box
                ,@(when (lme-editor-floating-p) '(:collapse lme-editor--collapse-box))))
        (force-mode-line-update)))))

(defun lme-editor--close-box (_window)
  "Close the editor, from its title bar's close box."
  (lme-editor-close))

(defun lme-editor--zoom-box (_window)
  "Move the editor between full screen and floating, from its zoom box."
  (lme-editor-toggle-floating))

(defun lme-editor--collapse-box (_window)
  "Roll the floating editor up to its title strip, or down again."
  (lme-editor-shade))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lme-editor-strip-mode special-mode "Emacs"
  "Major mode of the editor's title strip: its title bar over its tabs."
  (setq-local mode-line-format nil
              header-line-format nil
              cursor-type nil
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              lme-platinum-icon (lme-platinum-icon 'emacs)
              lme-platinum-box-functions (list :close #'lme-editor--close-box
                                               :zoom #'lme-editor--zoom-box)
              lme-platinum-active-function #'lme-editor--active-p)
  ;; Its height is its title bar's and tabs': a frame growing, as on a tall
  ;; screen, gives its height to the windows below, the strip fitted again.
  (add-hook 'window-size-change-functions #'lme-editor--refit-strip nil t)
  (face-remap-add-relative 'default 'lme-platinum-toolbar)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lme-editor--strip-buffer ()
  "Return the title strip's buffer, made when missing."
  (or (get-buffer lme-editor-strip-buffer-name)
      (with-current-buffer (get-buffer-create lme-editor-strip-buffer-name)
        (lme-editor-strip-mode)
        (current-buffer))))

(defun lme-editor--fit-strip ()
  "Fit the title strip to its title bar and tabs, in pixels, and keep it so:
a frame growing, as on a tall screen, gives its height to the windows
below, never to the strip."
  (when-let* ((window (get-buffer-window lme-editor-strip-buffer-name)))
    (lme-editor--refit-strip window)))

(defun lme-editor--refit-strip (window)
  "Fit the strip's WINDOW to its title bar and tabs again once its size
changed, unless it is alone in its frame, as in a rolled-up editor.
This is a buffer-local `window-size-change-functions' function."
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t)
          (fit-window-to-buffer-horizontally nil))
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lme-editor--show-strip ()
  "Put the title strip across the top of the selected frame, once."
  (let ((buffer (lme-editor--strip-buffer)))
    (lme-editor--render-strip)
    (unless (get-buffer-window buffer)
      (display-buffer-in-side-window
       buffer '((side . top) (slot . 0) (dedicated . t)
                (window-parameters (no-delete-other-windows . t)
                                   (no-other-window . t))))
      (lme-editor--fit-strip)
      ;; A new window's lines are measured right once it was drawn.
      (run-at-time 0 nil #'lme-editor--fit-strip))))

(defun lme-editor--delete-side-windows ()
  "Delete the selected frame's side windows: the title strip and the sidebar."
  (dolist (window (window-list nil 'no-mini))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window))))

(defvar lme-editor--views nil
  "The sidebar view of each workspace taken off for a move: (NAME . VIEW).
Each is put back when its workspace is next in front.")

(defun lme-editor--restore-chrome ()
  "Put the title strip on the workspace in front, and the sidebar view it
showed before the editor last moved."
  (lme-editor--show-strip)
  (when-let* ((entry (assoc (alist-get 'name (tab-bar--current-tab)) lme-editor--views)))
    (setq lme-editor--views (delq entry lme-editor--views))
    (when (and (cdr entry) (not (eq (cdr entry) (lme-sidebar-current-view))))
      (let ((lme-editor--moving nil))
        (lme-sidebar-select (cdr entry))))))

(defun lme-editor--keep-chrome ()
  "Keep the title strip on the editor's workspaces, and off any other.
Run after every command, so a workspace made or switched to by any means
gets it; the check is cheap.  Popups, child frames other than the
floating editor, are left alone."
  (let ((frame (selected-frame)))
    (unless (or lme-editor--moving (minibufferp) lme-editor--shade
                (and (frame-parent frame) (not (eq frame lme-editor--frame))))
      (let ((strip (get-buffer-window lme-editor-strip-buffer-name)))
        (cond ((lme-editor--in-workspace-p)
               (unless strip (lme-editor--restore-chrome)))
              (strip (delete-window strip)))))))

(defun lme-editor--keep-floating-chrome ()
  "Put the title strip on the floating editor, when it is missing."
  (when (and (lme-editor-floating-p) (not lme-editor--moving)
             (not (get-buffer-window lme-editor-strip-buffer-name lme-editor--frame)))
    (with-selected-frame lme-editor--frame
      (lme-editor--show-strip))))

;;;; Bringing the editor in front, and closing it

(defun lme-editor-activate ()
  "Bring the editor in front: floating over the desktop, or full screen in
the workspace it showed last."
  (interactive)
  (lme-apps-set-frontmost 'editor)
  (setq lme-editor--closed nil)
  (let ((screen (lme-apps-screen)))
    (if (lme-editor-floating-p)
        (progn
          (with-selected-frame screen
            (lme-desktop--select-tab))
          (make-frame-visible lme-editor--frame)
          (select-frame-set-input-focus lme-editor--frame)
          (lme-editor--keep-floating-chrome))
      (unless (eq (selected-frame) screen)
        (select-frame-set-input-focus screen))
      (unless (lme-editor--in-workspace-p)
        (let ((workspace (or (car (member lme-editor--last-workspace (lme-editor-workspaces)))
                             (car (lme-editor-workspaces))
                             (bound-and-true-p tabspaces-default-tab)
                             "Home")))
          (tab-bar-switch-to-tab workspace)))
      (lme-editor--show-strip)))
  (lme-editor--render-strip))

(defun lme-editor-hide ()
  "Hide the floating editor, out of the way of the application in front."
  (when (lme-editor-floating-p)
    (make-frame-invisible lme-editor--frame)
    (select-frame-set-input-focus (frame-parent lme-editor--frame))))

(defun lme-editor--show-with-desktop ()
  "Show the floating editor again over the desktop, unless it was closed."
  (when (and (lme-editor-floating-p) (not lme-editor--closed))
    (make-frame-visible lme-editor--frame)))

(defun lme-editor-close ()
  "Close the editor, leaving the desktop in front.  Nothing in it is lost:
it opens again as it was."
  (interactive)
  (setq lme-editor--closed t)
  (if (lme-editor-floating-p)
      (progn (make-frame-invisible lme-editor--frame)
             (select-frame-set-input-focus (frame-parent lme-editor--frame)))
    (when-let* ((workspace (lme-editor--current-workspace)))
      (setq lme-editor--last-workspace workspace)))
  (lme-desktop-activate))

(defun lme-editor--focus ()
  "Bring the editor in front unless it is, and select its frame."
  (cond ((lme-editor-floating-p)
         (unless (eq (selected-frame) lme-editor--frame)
           (lme-apps-set-frontmost 'editor)
           (make-frame-visible lme-editor--frame)
           (select-frame-set-input-focus lme-editor--frame)))
        ((not (lme-editor--in-workspace-p))
         (lme-editor-activate))))

(defun lme-editor--refocus ()
  "Give the floating editor the keyboard back after a menu bar menu closes.
A click on the menu bar focuses the frame under it; the editor stays in
front, so it takes the focus again."
  (when (and (eq (lme-apps-frontmost) 'editor) (lme-editor-floating-p)
             (frame-visible-p lme-editor--frame)
             (not (eq (selected-frame) lme-editor--frame)))
    (select-frame-set-input-focus lme-editor--frame)))

;;;; Documents opened from elsewhere

(defun lme-editor--document-p (buffer)
  "Return non-nil when BUFFER belongs in the editor: anything but the
desktop, the browser's pages and Emacs's own internal buffers."
  (let ((name (buffer-name buffer)))
    (not (or (string-prefix-p " " name)
             (minibufferp buffer)
             (member name '("*Completions*"))
             (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                      'xwidget-webkit-mode)))))

(defvar lme-editor--displaying nil
  "Non-nil while `lme-editor-display' shows a document in the editor.")

(defun lme-editor--astray-p (buffer-or-name _action)
  "Return non-nil when a document is about to show outside the editor.
That is any of `lme-editor--document-p' while another application is in
front: the desktop, the Finder window or the browser, outside the
minibuffer.  This is the `display-buffer-alist' condition sending
documents to the editor."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (not lme-editor--moving)
         (not lme-editor--displaying)
         (not (active-minibuffer-window))
         (not (lme-editor--frame-p (selected-frame)))
         (lme-editor--document-p buffer))))

(defun lme-editor-display (buffer alist)
  "Display BUFFER in the editor, brought in front first, per ALIST.
This is a `display-buffer' action function."
  (lme-editor-activate)
  (let ((lme-editor--displaying t))
    (display-buffer buffer (cons nil alist))))

;;;; Full screen and floating

(declare-function treemacs-get-local-buffer "treemacs-scope" ())

(defun lme-editor--take-off-chrome (frame)
  "Take the title strip and the sidebar off FRAME's editor workspaces.
The view each showed is kept in `lme-editor--views', to be put back.  The
project tree of FRAME goes too: treemacs keeps one per frame, and the
editor's next frame builds its own."
  (with-selected-frame frame
    (let ((current (tab-bar--current-tab-index))
          (views nil))
      (seq-do-indexed (lambda (tab index)
                        (when (lme-editor--workspace-tab-p tab)
                          (unless (= index (tab-bar--current-tab-index))
                            (tab-bar-select-tab (1+ index)))
                          (push (cons (alist-get 'name tab) (lme-sidebar-current-view)) views)
                          (lme-editor--delete-side-windows)))
                      (funcall tab-bar-tabs-function))
      (setq lme-editor--views views)
      (unless (= current (tab-bar--current-tab-index))
        (tab-bar-select-tab (1+ current)))
      (when-let* (((fboundp 'treemacs-get-local-buffer))
                  (tree (treemacs-get-local-buffer)))
        (kill-buffer tree)))))

(defun lme-editor--move-workspaces (from to)
  "Move the editor's workspaces from frame FROM to the end of frame TO's tabs.
The one in front in FROM goes as its present window state; each goes
without the window configuration that ties it to FROM."
  (let* ((tabs (funcall tab-bar-tabs-function from))
         (moving (seq-filter #'lme-editor--workspace-tab-p tabs))
         ;; Stored before FROM loses them: the one in front is turned into
         ;; a stored tab from FROM's windows while they are still its own.
         (stored (with-selected-frame from
                   (mapcar (lambda (tab)
                             (assq-delete-all
                              'wc (copy-sequence (if (eq (car tab) 'current-tab)
                                                     (tab-bar--tab from)
                                                   tab))))
                           moving))))
    (tab-bar-tabs-set (seq-remove (lambda (tab) (memq tab moving)) tabs) from)
    (tab-bar-tabs-set (append (funcall tab-bar-tabs-function to) stored) to)))

(defun lme-editor--default-geometry (screen)
  "Return where the editor floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (tab-bar-height screen t) 20))
         (left (round (* width 0.06))))
    (list left top (- width (* 2 left)) (- height top 40))))

(defun lme-editor--make-frame (screen)
  "Return the floating editor's child frame of SCREEN, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lme-editor--geometry (lme-editor--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Emacs") (lme-editor . t) (lme-app . editor)
                           (lme-window . t) (top-visible . 30)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 2) (child-frame-border-width . 1)
                           (drag-internal-border . t)
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height)))))))
    (set-face-attribute 'internal-border frame :background (lme-platinum--color :chrome))
    (when (fboundp 'tabspaces--set-buffer-predicate)
      (tabspaces--set-buffer-predicate frame))
    frame))

(defun lme-editor--geometry-of (frame)
  "Return where floating FRAME is: (LEFT TOP WIDTH HEIGHT) in pixels."
  (pcase-let ((`(,left . ,top) (frame-position frame)))
    (list left top (frame-text-width frame) (frame-text-height frame))))

(defun lme-editor-float ()
  "Float the editor in a window of its own over the desktop."
  (interactive)
  (unless (lme-editor-floating-p)
    (let* ((screen (lme-apps-screen))
           (lme-editor--moving t)
           (_ (lme-editor--take-off-chrome screen))
           (workspace (or (with-selected-frame screen (lme-editor--current-workspace))
                          lme-editor--last-workspace))
           (frame (lme-editor--make-frame screen)))
      (with-selected-frame screen
        (lme-desktop--select-tab))
      (lme-editor--move-workspaces screen frame)
      (setq lme-editor--frame frame)
      ;; The frame's own first tab, made with it, holds nothing: the
      ;; workspaces follow it, and once one is selected it is dropped.
      (with-selected-frame frame
        (let ((names (mapcar (lambda (tab) (alist-get 'name tab))
                             (cdr (funcall tab-bar-tabs-function frame)))))
          (if (not names)
              (tab-bar-rename-tab (or (bound-and-true-p tabspaces-default-tab) "Home"))
            (tab-bar-select-tab (+ 2 (or (seq-position names workspace) 0)))
            (tab-bar-tabs-set (cdr (funcall tab-bar-tabs-function frame)) frame))))
      (make-frame-visible frame)
      (lme-apps-keep-on-desktop frame)
      (select-frame-set-input-focus frame)
      (let ((lme-editor--moving nil))
        (with-selected-frame frame
          (lme-editor--restore-chrome)))
      (lme-apps-set-frontmost 'editor))))

(defun lme-editor-full-screen ()
  "Bring the floating editor back to full screen, under the menu bar."
  (interactive)
  (when (lme-editor-floating-p)
    (let* ((frame lme-editor--frame)
           (screen (frame-parent frame))
           (lme-editor--moving t)
           (_ (lme-editor--take-off-chrome frame))
           (workspace (with-selected-frame frame (lme-editor--current-workspace))))
      (lme-editor--unshade)
      (setq lme-editor--geometry (lme-editor--geometry-of frame))
      (lme-editor--move-workspaces frame screen)
      (setq lme-editor--frame nil)
      (select-frame-set-input-focus screen)
      (tab-bar-switch-to-tab workspace)
      (delete-frame frame)
      (let ((lme-editor--moving nil))
        (lme-editor--restore-chrome))
      (lme-apps-set-frontmost 'editor))))

(defun lme-editor-toggle-floating ()
  "Move the editor between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lme-editor-floating-p) (lme-editor-full-screen) (lme-editor-float)))

(defun lme-editor--unshade ()
  "Roll the floating editor down again, when it is rolled up."
  (when lme-editor--shade
    (pcase-let ((`(,config . ,height) lme-editor--shade))
      (setq lme-editor--shade nil)
      (set-frame-height lme-editor--frame height nil t)
      (set-window-configuration config))))

(defun lme-editor-shade ()
  "Roll the floating editor up to its title strip, or down again, as Mac OS 9
rolled a window up into its title bar."
  (interactive)
  (when (lme-editor-floating-p)
    (if lme-editor--shade
        (lme-editor--unshade)
      (with-selected-frame lme-editor--frame
        (setq lme-editor--shade (cons (current-window-configuration)
                                      (frame-text-height lme-editor--frame)))
        ;; The strip's height is measured before its window goes: the one
        ;; window left fills the frame until the frame shrinks to it.
        (let* ((strip (get-buffer-window lme-editor-strip-buffer-name))
               (height (if strip (window-pixel-height strip) 60))
               (window (car (lme-main-area--windows)))
               (ignore-window-parameters t))
          (delete-other-windows window)
          (set-window-dedicated-p window nil)
          (set-window-buffer window (lme-editor--strip-buffer))
          (set-frame-height lme-editor--frame height nil t))))))

;;;; Menus

(defun lme-editor--key (command)
  "Return the keys of COMMAND typed on the keyboard, as a menu shows them."
  (when-let* ((keys (seq-find #'lme-popup--keyboard-p (where-is-internal command))))
    (key-description keys)))

(defun lme-editor--item (label command &rest properties)
  "Return the menu item LABEL running COMMAND in the editor, and PROPERTIES.
The editor comes forward first: a menu bar click focuses the desktop
under it."
  `(:label ,label
    :action ,(lambda ()
               (lme-editor--focus)
               (if (commandp command) (call-interactively command) (funcall command)))
    ,@(when (and (symbolp command) (lme-editor--key command))
        (list :key (lme-editor--key command)))
    ,@properties))

(defun lme-editor--in-editor (function)
  "Call FUNCTION with the editor's frame and its selected window selected."
  (with-selected-frame (lme-editor-frame)
    (funcall function)))

(defun lme-editor--file-items ()
  "Return the items of the editor's File menu."
  (let ((buffer (lme-editor--in-editor (lambda () (window-buffer (selected-window))))))
    (list (lme-editor--item "New File…" #'find-file)
          (lme-editor--item "Open Project…" #'project-switch-project)
          (lme-editor--item "Open Recent…" (if (fboundp 'recentf-open) #'recentf-open
                                              #'find-file))
          'separator
          (lme-editor--item "Save" #'save-buffer
                            :enabled (and (buffer-file-name buffer) (buffer-modified-p buffer) t))
          (lme-editor--item "Save As…" #'write-file)
          (lme-editor--item "Revert" #'revert-buffer
                            :enabled (and (buffer-file-name buffer) t))
          'separator
          (lme-editor--item "Close Window" (lambda () (lme-platinum-close-window (selected-window))))
          (lme-editor--item "Close Emacs" #'lme-editor-close))))

(defun lme-editor--edit-items ()
  "Return the items of the editor's Edit menu."
  (list (lme-editor--item "Undo" #'undo)
        (lme-editor--item "Redo" #'undo-redo)
        'separator
        (lme-editor--item "Cut" #'lme-cut-region)
        (lme-editor--item "Copy" #'lme-copy-region)
        (lme-editor--item "Paste" #'lme-paste)
        (lme-editor--item "Select All" #'mark-whole-buffer)
        'separator
        (lme-editor--item "Find…" #'isearch-forward)
        (lme-editor--item "Replace…" #'query-replace)))

(defun lme-editor--view-items ()
  "Return the items of the editor's View menu: the sidebar's views, and
the editor full screen or floating."
  (let ((shown (lme-editor--in-editor #'lme-sidebar-current-view)))
    `(,@(mapcar (lambda (view)
                  (lme-editor--item (plist-get (cdr view) :title)
                                    (lambda () (lme-sidebar-select (car view)))
                                    :checked (eq (car view) shown)))
                lme-sidebar-views)
      separator
      ,(lme-editor--item "Full Screen" #'lme-editor-full-screen
                         :checked (not (lme-editor-floating-p)))
      ,(lme-editor--item "Floating Window" #'lme-editor-float
                         :checked (lme-editor-floating-p)))))

(defun lme-editor--window-items ()
  "Return the items of the editor's Window menu: the buffer manager, then
the workspace's buffers, the one shown checked, as Mac OS 9's Window menus
listed an application's windows."
  (lme-editor--in-editor
   (lambda ()
     (let ((shown (window-buffer (selected-window))))
       `(,(lme-editor--item "Buffer Manager" #'lme-buffer-manager)
         separator
         ,@(mapcar (lambda (buffer)
                     (lme-editor--item (truncate-string-to-width
                                        (lme-platinum--buffer-title buffer) 48 nil nil "…")
                                       (lambda () (switch-to-buffer buffer))
                                       :checked (eq buffer shown)))
                   (seq-take (lme-platinum--window-buffers (selected-window)) 30)))))))

(defun lme-editor-menus ()
  "Return the editor's menus, for the menu bar while it is in front."
  '((lme-editor-file "File" lme-editor--file-items)
    (lme-editor-edit "Edit" lme-editor--edit-items)
    (lme-editor-view "View" lme-editor--view-items)
    ;; The workspaces show as thumbnails, see lme-workspaces.el.
    (lme-editor-project "Project" lme-workspaces-menu-panel)
    (lme-editor-window "Window" lme-editor--window-items)))

;;;; Setup

(defconst lme-editor-splash-buffer-name "*GNU Emacs*"
  "Name of the buffer of Emacs's own startup screen.")

(defun lme-editor-splash-buffer ()
  "Return the buffer of Emacs's own startup screen, its big logo over the
links to start with, made when missing without being shown."
  (or (get-buffer lme-editor-splash-buffer-name)
      (save-window-excursion
        (display-startup-screen)
        (get-buffer lme-editor-splash-buffer-name))))

(defun lme-editor--new-workspace-buffer ()
  "Show Emacs's startup screen in a new workspace's window; return nil.
This is the editor's `tab-bar-new-tab-choice'.  The screen is put in the
window rather than returned: a returned buffer is shown by
`switch-to-buffer', which obeys `display-buffer-alist', and while the new
workspace is made the old one is still in front, so a workspace made from
the desktop would send its first buffer to the editor's last workspace."
  (set-window-buffer (selected-window) (lme-editor-splash-buffer))
  (set-window-start (selected-window) 1)
  nil)

(defun lme-editor--show-splash (window)
  "Show Emacs's startup screen in WINDOW when it is in an editor workspace
holding nothing else, and return non-nil; else return nil.
This is the editor's `lme-desktop-empty-function'."
  (when (lme-editor--in-workspace-p (window-frame window))
    (set-window-buffer window (lme-editor-splash-buffer))
    (set-window-start window 1)
    t))

(defun lme-editor--prepare-view ()
  "Bring the editor in front before a sidebar view is shown outside it."
  (unless (lme-editor--frame-p (selected-frame))
    (lme-editor-activate)
    (select-frame (lme-editor-frame))))

(defvar lme-sidebar-prepare-function)
(defvar lme-sidebar-change-hook)

(defun lme-setup-editor ()
  "Make Emacs the editor an application on the desktop."
  (setq lme-sidebar-prepare-function #'lme-editor--prepare-view
        lme-desktop-empty-function #'lme-editor--show-splash
        ;; The editor opens on Emacs's own startup screen: Emacs starts in
        ;; its first workspace, and a new workspace starts there too.
        initial-buffer-choice #'lme-editor-splash-buffer
        tab-bar-new-tab-choice #'lme-editor--new-workspace-buffer)
  (add-hook 'lme-sidebar-change-hook #'lme-editor--render-strip)
  (add-hook 'post-command-hook #'lme-editor--keep-chrome)
  (add-hook 'tab-bar-tab-post-select-functions #'lme-editor--remember-workspace)
  (add-hook 'tab-bar-tab-post-select-functions
            (lambda (&rest _) (lme-editor--render-strip)) 90)
  (add-hook 'lme-platinum-menu-closed-hook #'lme-editor--refocus)
  (add-hook 'lme-desktop-activate-hook #'lme-editor--show-with-desktop)
  (add-to-list 'display-buffer-alist '(lme-editor--astray-p lme-editor-display)))

(provide 'lme-editor)
;;; lme-editor.el ends here
