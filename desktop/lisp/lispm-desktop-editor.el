;;; lispm-desktop-editor.el --- Emacs, the editor, as an application  -*- lexical-binding: t; -*-

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
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-surface)

(declare-function lispm-desktop-show-desktop "lispm-desktop-coherence" (&optional screen))
(declare-function lispm-desktop-coherence-bring-in "lispm-desktop-coherence" (frame))
(declare-function lispm-desktop-sidebar-current-view "lispm-desktop-sidebar" ())
(declare-function lispm-desktop-sidebar-select "lispm-desktop-sidebar" (view))
(declare-function lispm-desktop-terminal-buffer-p "lispm-desktop-interface" (buffer))
(declare-function lispm-desktop-main-area--windows "lispm-desktop-interface" ())
(declare-function lispm-desktop-buffer-manager "lispm-desktop-buffer-manager" ())
(declare-function lispm-desktop-popup--keyboard-p "lispm-desktop-popup" (keys))
(declare-function tabspaces--current-tab-name "tabspaces" ())
(declare-function tabspaces-local-buffer-list "tabspaces" (&optional frame tabnum))
(declare-function tabspaces--set-buffer-predicate "tabspaces" (frame))
(defvar lispm-desktop-sidebar-views)
(defvar lispm-desktop-browser-workspace-name)
(defvar tabspaces-default-tab)

(defvar lispm-desktop-editor--frame nil
  "The floating editor's child frame, or nil while the editor is full screen.")

(defvar lispm-desktop-editor--geometry nil
  "Where the floating editor was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lispm-desktop-editor--last-workspace nil
  "Name of the workspace the full-screen editor showed when it was left.")

(defvar lispm-desktop-editor--moving nil
  "Non-nil while the editor moves between full screen and floating.")

(defvar lispm-desktop-editor--shade nil
  "While the floating editor is rolled up: its windows and text height.")

(defvar lispm-desktop-editor--closed nil
  "Non-nil once the floating editor was closed, until it is opened again.
A floating editor hidden only while the browser was in front shows again
with the desktop; a closed one does not.")

(defun lispm-desktop-editor-floating-p ()
  "Return non-nil while the editor floats in a window of its own."
  (frame-live-p lispm-desktop-editor--frame))

(defun lispm-desktop-editor-frame ()
  "Return the frame the editor is drawn in."
  (if (lispm-desktop-editor-floating-p) lispm-desktop-editor--frame (lispm-desktop-apps-screen)))

;;;; Workspaces

(defun lispm-desktop-editor--workspace-tab-p (tab)
  "Return non-nil when TAB is one of the editor's workspaces.
The desktop's and the browser's workspaces are not."
  (not (member (alist-get 'name tab)
               (list lispm-desktop-tab-name (bound-and-true-p lispm-desktop-browser-workspace-name)))))

(defun lispm-desktop-editor--in-workspace-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, shows an editor
workspace."
  (lispm-desktop-editor--workspace-tab-p (tab-bar--current-tab nil frame)))

(defun lispm-desktop-editor-workspaces ()
  "Return the names of the editor's workspaces, in order."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (seq-filter #'lispm-desktop-editor--workspace-tab-p
                      (funcall tab-bar-tabs-function (lispm-desktop-editor-frame)))))

(defun lispm-desktop-editor--frame-p (frame)
  "Return non-nil when FRAME draws the editor: its floating window, or the
screen while one of its workspaces is in front.  Popups, child frames that
are no application's window, go with the editor."
  (cond ((eq frame lispm-desktop-editor--frame) t)
        ;; Another application's window, over the desktop or out on the
        ;; computer's screen.
        ((frame-parameter frame 'lispm-desktop-window) nil)
        ;; The frame drawing a window's frame, or an outline.
        ((lispm-desktop-platinum-chrome-frame-p frame) nil)
        ((frame-parent frame) t)
        ((lispm-desktop-apps-screen-p frame) (lispm-desktop-editor--in-workspace-p frame))))

(defun lispm-desktop-editor--current-workspace ()
  "Return the name of the editor's workspace in front, or nil."
  (let ((frame (lispm-desktop-editor-frame)))
    (when (lispm-desktop-editor--in-workspace-p frame)
      (alist-get 'name (tab-bar--current-tab nil frame)))))

(defun lispm-desktop-editor--remember-workspace (from-tab _to-tab)
  "Remember FROM-TAB when the full screen editor's workspace is left.
This is a `tab-bar-tab-post-select-functions' function."
  (when (and (not lispm-desktop-editor--moving) (lispm-desktop-editor--workspace-tab-p from-tab))
    (setq lispm-desktop-editor--last-workspace (alist-get 'name from-tab))))

;;;; The title strip

(defconst lispm-desktop-editor-strip-buffer-name " *Emacs*"
  "Name of the buffer of the editor's title strip.")

(defun lispm-desktop-editor--active-p (_window)
  "Return non-nil while the editor is in front: its title bar is then active."
  (eq (lispm-desktop-apps-frontmost) 'editor))

(defun lispm-desktop-editor--title ()
  "Return the editor's title: Emacs, and the workspace in front."
  (if-let* ((workspace (lispm-desktop-editor--current-workspace)))
      (format "Emacs — %s" workspace)
    "Emacs"))

(defvar-keymap lispm-desktop-editor-tab-map
  :doc "Keymap of a tab of the editor's title strip."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lispm-desktop-editor-tab-click)

(defun lispm-desktop-editor-tab-click (event)
  "Show the sidebar view whose tab EVENT clicked, or hide it when shown."
  (interactive "e")
  (let* ((position (event-start event))
         (view (get-text-property (posn-point position) 'lispm-desktop-editor-view
                                  (window-buffer (posn-window position)))))
    (when view
      (with-selected-frame (window-frame (posn-window position))
        (lispm-desktop-sidebar-select view)))))

(defun lispm-desktop-editor--tab (view selected)
  "Return the strip's tab for sidebar VIEW, in front when SELECTED, as
`lispm-desktop-platinum-tabs' takes it."
  (let ((spec (cdr view)))
    (list (concat (lispm-desktop-platinum-icon (plist-get spec :icon)) " " (plist-get spec :title))
          selected
          'lispm-desktop-editor-view (car view) 'keymap lispm-desktop-editor-tab-map
          'help-echo (plist-get spec :help) 'pointer 'arrow)))

(defvar-local lispm-desktop-editor--strip-width nil
  "The width in pixels the title strip's tabs were last drawn for.")

(defun lispm-desktop-editor--render-strip ()
  "Draw the title strip: its title, and a tab per sidebar view, standing on
the edge of the windows below, which runs across the strip."
  (when-let* ((buffer (get-buffer lispm-desktop-editor-strip-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (frame (lispm-desktop-editor-frame))
             (shown (with-selected-frame frame (lispm-desktop-sidebar-current-view)))
             (window (get-buffer-window buffer frame))
             ;; A line as wide as its window would wrap.
             (width (1- (if window (window-body-width window t) (frame-inner-width frame)))))
        (setq lispm-desktop-editor--strip-width width)
        (erase-buffer)
        (insert (lispm-desktop-platinum-tabs
                 (mapcar (lambda (view) (lispm-desktop-editor--tab view (eq (car view) shown)))
                         lispm-desktop-sidebar-views)
                 width))
        (setq lispm-desktop-platinum-title (lispm-desktop-editor--title)
              ;; Only a floating window rolls up into its title bar.
              lispm-desktop-platinum-box-functions
              `(:close lispm-desktop-editor--close-box :zoom lispm-desktop-editor--zoom-box
                ,@(when (lispm-desktop-editor-floating-p) '(:collapse lispm-desktop-editor--collapse-box))))
        (force-mode-line-update)
        ;; The tabs may have changed the strip's height.
        (when window
          (lispm-desktop-editor--refit-strip window))))))

(defun lispm-desktop-editor--close-box (_window)
  "Close the editor, from its title bar's close box."
  (lispm-desktop-editor-close))

(defun lispm-desktop-editor--zoom-box (_window)
  "Move the editor between full screen and floating, from its zoom box."
  (lispm-desktop-editor-toggle-floating))

(defun lispm-desktop-editor--collapse-box (_window)
  "Roll the floating editor up to its title strip, or down again."
  (lispm-desktop-editor-shade))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lispm-desktop-editor-strip-mode special-mode "Emacs"
  "Major mode of the editor's title strip: its title bar over its tabs."
  (setq-local mode-line-format nil
              header-line-format nil
              cursor-type nil
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon 'emacs)
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-editor--close-box
                                                         :zoom #'lispm-desktop-editor--zoom-box)
              lispm-desktop-platinum-active-function #'lispm-desktop-editor--active-p)
  ;; Its height is its title bar's and tabs': a frame growing, as on a tall
  ;; screen, gives its height to the windows below, the strip fitted again.
  (add-hook 'window-size-change-functions #'lispm-desktop-editor--refit-strip nil t)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-toolbar)
  ;; The tabs' row is one line, never wrapped, whatever visual line mode
  ;; is on globally.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-editor--strip-buffer ()
  "Return the title strip's buffer, made when missing."
  (or (get-buffer lispm-desktop-editor-strip-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-editor-strip-buffer-name)
        (lispm-desktop-editor-strip-mode)
        (current-buffer))))

(defun lispm-desktop-editor--fit-strip ()
  "Fit the title strip to its title bar and tabs, in pixels, and keep it so:
a frame growing, as on a tall screen, gives its height to the windows
below, never to the strip."
  (when-let* ((window (get-buffer-window lispm-desktop-editor-strip-buffer-name)))
    (lispm-desktop-editor--refit-strip window)))

(defun lispm-desktop-editor--refit-strip (window)
  "Fit the strip's WINDOW to its title bar and tabs again once its size
changed, unless it is alone in its frame, as in a rolled-up editor; its
tabs are drawn again when its width changed.  This is a buffer-local
`window-size-change-functions' function."
  (unless (eql (1- (window-body-width window t)) lispm-desktop-editor--strip-width)
    (lispm-desktop-editor--render-strip))
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t)
          (fit-window-to-buffer-horizontally nil)
          ;; The strip is its title bar over one line of tabs, shorter
          ;; than the four lines a window keeps at the least.
          (window-min-height 1))
      ;; The height kept from the last fit would hold the window at it.
      (window-preserve-size window nil nil)
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lispm-desktop-editor--show-strip ()
  "Put the title strip across the top of the selected frame, once."
  (let ((buffer (lispm-desktop-editor--strip-buffer)))
    (lispm-desktop-editor--render-strip)
    (unless (get-buffer-window buffer)
      (display-buffer-in-side-window
       buffer '((side . top) (slot . 0) (dedicated . t)
                (window-parameters (no-delete-other-windows . t)
                                   (no-other-window . t))))
      (lispm-desktop-editor--fit-strip)
      ;; A new window's lines are measured right once it was drawn.
      (run-at-time 0 nil #'lispm-desktop-editor--fit-strip))))

(defun lispm-desktop-editor--delete-side-windows ()
  "Delete the selected frame's side windows: the title strip and the sidebar."
  (dolist (window (window-list nil 'no-mini))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window))))

(defvar lispm-desktop-editor--views nil
  "The sidebar view of each workspace taken off for a move: (NAME . VIEW).
Each is put back when its workspace is next in front.")

(defun lispm-desktop-editor--restore-chrome ()
  "Put the title strip on the workspace in front, and the sidebar view it
showed before the editor last moved."
  (lispm-desktop-editor--show-strip)
  (when-let* ((entry (assoc (alist-get 'name (tab-bar--current-tab)) lispm-desktop-editor--views)))
    (setq lispm-desktop-editor--views (delq entry lispm-desktop-editor--views))
    (when (and (cdr entry) (not (eq (cdr entry) (lispm-desktop-sidebar-current-view))))
      (let ((lispm-desktop-editor--moving nil))
        (lispm-desktop-sidebar-select (cdr entry))))))

(defun lispm-desktop-editor--keep-chrome ()
  "Keep the title strip on the editor's workspaces, and off any other.
Run after every command, so a workspace made or switched to by any means
gets it; the check is cheap.  Popups, child frames other than the
floating editor, are left alone."
  (let ((frame (selected-frame)))
    ;; Another application's window, out on the computer's screen too,
    ;; is no screen, whose workspaces the strip goes on.
    (unless (or lispm-desktop-editor--moving (minibufferp) lispm-desktop-editor--shade
                (and (not (lispm-desktop-apps-screen-p frame)) (not (eq frame lispm-desktop-editor--frame))))
      (let ((strip (get-buffer-window lispm-desktop-editor-strip-buffer-name)))
        (cond ((lispm-desktop-editor--in-workspace-p)
               (unless strip (lispm-desktop-editor--restore-chrome)))
              (strip (delete-window strip)))))))

(defun lispm-desktop-editor--keep-floating-chrome ()
  "Put the title strip on the floating editor, when it is missing."
  (when (and (lispm-desktop-editor-floating-p) (not lispm-desktop-editor--moving)
             (not (get-buffer-window lispm-desktop-editor-strip-buffer-name lispm-desktop-editor--frame)))
    (with-selected-frame lispm-desktop-editor--frame
      (lispm-desktop-editor--show-strip))))

;;;; Bringing the editor in front, and closing it

(defun lispm-desktop-editor-activate ()
  "Bring the editor in front: floating over the desktop, or full screen in
the workspace it showed last."
  (interactive)
  (lispm-desktop-apps-set-frontmost 'editor)
  (setq lispm-desktop-editor--closed nil)
  ;; With the desktop hidden, the editor floats out on the computer's screen.
  (when (and (lispm-desktop-apps-hidden-p) (not (lispm-desktop-editor-floating-p)))
    (lispm-desktop-editor-float))
  (let ((screen (lispm-desktop-apps-screen)))
    (if (lispm-desktop-editor-floating-p)
        (progn
          (unless (lispm-desktop-apps-hidden-p screen)
            (with-selected-frame screen
              (lispm-desktop--select-tab)))
          (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)
          (select-frame-set-input-focus lispm-desktop-editor--frame)
          (lispm-desktop-editor--keep-floating-chrome))
      (unless (eq (selected-frame) screen)
        (select-frame-set-input-focus screen))
      (unless (lispm-desktop-editor--in-workspace-p)
        (let ((workspace (or (car (member lispm-desktop-editor--last-workspace (lispm-desktop-editor-workspaces)))
                             (car (lispm-desktop-editor-workspaces))
                             (bound-and-true-p tabspaces-default-tab)
                             "Home")))
          (tab-bar-switch-to-tab workspace)))
      (lispm-desktop-editor--show-strip)))
  (lispm-desktop-editor--render-strip))

(defun lispm-desktop-editor-hide ()
  "Hide the floating editor, out of the way of the application in front."
  (when (and (lispm-desktop-editor-floating-p)
             ;; Out on the computer's screen, it stays where it is.
             (not (lispm-desktop-apps-out-p lispm-desktop-editor--frame)))
    (lispm-desktop-platinum-hide-window lispm-desktop-editor--frame)
    (select-frame-set-input-focus (lispm-desktop-platinum-window-parent lispm-desktop-editor--frame))))

(defun lispm-desktop-editor--show-with-desktop ()
  "Show the floating editor again over the desktop, unless it was closed."
  (when (and (lispm-desktop-editor-floating-p) (not lispm-desktop-editor--closed))
    (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)))

(defun lispm-desktop-editor-close ()
  "Close the editor, leaving the desktop in front.  Nothing in it is lost:
it opens again as it was."
  (interactive)
  (setq lispm-desktop-editor--closed t)
  (if (lispm-desktop-editor-floating-p)
      (progn (lispm-desktop-apps-focus-after lispm-desktop-editor--frame)
             (lispm-desktop-platinum-hide-window lispm-desktop-editor--frame))
    (when-let* ((workspace (lispm-desktop-editor--current-workspace)))
      (setq lispm-desktop-editor--last-workspace workspace)))
  ;; With the desktop hidden, the windows left out keep the screen.
  (unless (lispm-desktop-apps-hidden-p)
    (lispm-desktop-activate)))

(defun lispm-desktop-editor--focus ()
  "Bring the editor in front unless it is, and select its frame."
  (cond ((lispm-desktop-editor-floating-p)
         (unless (eq (selected-frame) lispm-desktop-editor--frame)
           (lispm-desktop-apps-set-frontmost 'editor)
           (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)
           (select-frame-set-input-focus lispm-desktop-editor--frame)))
        ((not (lispm-desktop-editor--in-workspace-p))
         (lispm-desktop-editor-activate))))

(defun lispm-desktop-editor--refocus ()
  "Give the floating editor the keyboard back after a menu bar menu closes.
A click on the menu bar focuses the frame under it; the editor stays in
front, so it takes the focus again."
  (when (and (eq (lispm-desktop-apps-frontmost) 'editor) (lispm-desktop-editor-floating-p)
             (frame-visible-p lispm-desktop-editor--frame)
             (not (eq (selected-frame) lispm-desktop-editor--frame)))
    (select-frame-set-input-focus lispm-desktop-editor--frame)))

;;;; Documents opened from elsewhere

(defun lispm-desktop-editor--document-p (buffer)
  "Return non-nil when BUFFER belongs in the editor: anything but the
desktop, the browser's pages and Emacs's own internal buffers."
  (let ((name (buffer-name buffer)))
    (not (or (string-prefix-p " " name)
             (minibufferp buffer)
             (member name '("*Completions*"))
             (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                      'xwidget-webkit-mode)))))

(defvar lispm-desktop-editor--displaying nil
  "Non-nil while `lispm-desktop-editor-display' shows a document in the editor.")

(defun lispm-desktop-editor--astray-p (buffer-or-name _action)
  "Return non-nil when a document is about to show outside the editor.
That is any of `lispm-desktop-editor--document-p' while another application is
in front: the desktop, the Finder window or the browser, outside the
minibuffer.  This is the `display-buffer-alist' condition sending documents to
the editor."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (not lispm-desktop-editor--moving)
         (not lispm-desktop-editor--displaying)
         (not (active-minibuffer-window))
         (not (lispm-desktop-editor--frame-p (selected-frame)))
         (lispm-desktop-editor--document-p buffer))))

(defun lispm-desktop-editor-display (buffer alist)
  "Display BUFFER in the editor, brought in front first, per ALIST.
This is a `display-buffer' action function."
  (lispm-desktop-editor-activate)
  (let ((lispm-desktop-editor--displaying t))
    (display-buffer buffer (cons nil alist))))

;;;; Full screen and floating

(declare-function treemacs-get-local-buffer "treemacs-scope" ())

(defun lispm-desktop-editor--take-off-chrome (frame)
  "Take the title strip and the sidebar off FRAME's editor workspaces.
The view each showed is kept in `lispm-desktop-editor--views', to be put back.
The project tree of FRAME goes too: treemacs keeps one per frame, and the
editor's next frame builds its own."
  (with-selected-frame frame
    (let ((current (tab-bar--current-tab-index))
          (views nil))
      (seq-do-indexed (lambda (tab index)
                        (when (lispm-desktop-editor--workspace-tab-p tab)
                          (unless (= index (tab-bar--current-tab-index))
                            (tab-bar-select-tab (1+ index)))
                          (push (cons (alist-get 'name tab) (lispm-desktop-sidebar-current-view)) views)
                          (lispm-desktop-editor--delete-side-windows)))
                      (funcall tab-bar-tabs-function))
      (setq lispm-desktop-editor--views views)
      (unless (= current (tab-bar--current-tab-index))
        (tab-bar-select-tab (1+ current)))
      (when-let* (((fboundp 'treemacs-get-local-buffer))
                  (tree (treemacs-get-local-buffer)))
        (kill-buffer tree)))))

(defun lispm-desktop-editor--move-workspaces (from to)
  "Move the editor's workspaces from frame FROM to the end of frame TO's tabs.
The one in front in FROM goes as its present window state; each goes
without the window configuration that ties it to FROM."
  (let* ((tabs (funcall tab-bar-tabs-function from))
         (moving (seq-filter #'lispm-desktop-editor--workspace-tab-p tabs))
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

(defun lispm-desktop-editor--default-geometry (screen)
  "Return where the editor floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (tab-bar-height screen t) 20))
         (left (round (* width 0.06))))
    (list left top (- width (* 2 left)) (- height top 40))))

(defun lispm-desktop-editor--make-frame (screen)
  "Return the floating editor's child frame of SCREEN, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lispm-desktop-editor--geometry (lispm-desktop-editor--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Emacs") (lispm-desktop-editor . t) (lispm-desktop-app . editor)
                           (lispm-desktop-window . t) (top-visible . 30)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 0)
                           (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height)))))))
    (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum--color :chrome))
    (when (fboundp 'tabspaces--set-buffer-predicate)
      (tabspaces--set-buffer-predicate frame))
    frame))

(defun lispm-desktop-editor--geometry-of (frame)
  "Return where floating FRAME is: (LEFT TOP WIDTH HEIGHT) in pixels, over
its screen."
  (lispm-desktop-apps--geometry frame))

(defun lispm-desktop-editor-float ()
  "Float the editor in a window of its own over the desktop."
  (interactive)
  (unless (lispm-desktop-editor-floating-p)
    (let* ((screen (lispm-desktop-apps-screen))
           (lispm-desktop-editor--moving t)
           (_ (lispm-desktop-editor--take-off-chrome screen))
           (workspace (or (with-selected-frame screen (lispm-desktop-editor--current-workspace))
                          lispm-desktop-editor--last-workspace))
           (frame (lispm-desktop-editor--make-frame screen)))
      (with-selected-frame screen
        (lispm-desktop--select-tab))
      (lispm-desktop-editor--move-workspaces screen frame)
      (setq lispm-desktop-editor--frame frame)
      ;; The frame's own first tab, made with it, holds nothing: the
      ;; workspaces follow it, and once one is selected it is dropped.
      (with-selected-frame frame
        (let ((names (mapcar (lambda (tab) (alist-get 'name tab))
                             (cdr (funcall tab-bar-tabs-function frame)))))
          (if (not names)
              (tab-bar-rename-tab (or (bound-and-true-p tabspaces-default-tab) "Home"))
            (tab-bar-select-tab (+ 2 (or (seq-position names workspace) 0)))
            (tab-bar-tabs-set (cdr (funcall tab-bar-tabs-function frame)) frame))))
      (lispm-desktop-platinum-show-window frame)
      (lispm-desktop-apps-keep-on-desktop frame)
      (select-frame-set-input-focus frame)
      (let ((lispm-desktop-editor--moving nil))
        (with-selected-frame frame
          (lispm-desktop-editor--restore-chrome)))
      (lispm-desktop-apps-set-frontmost 'editor))))

(defun lispm-desktop-editor-full-screen ()
  "Bring the floating editor back to full screen, under the menu bar."
  (interactive)
  ;; Full screen is the desktop's: it shows first.
  (when (and (lispm-desktop-apps-hidden-p) (fboundp 'lispm-desktop-show-desktop))
    (lispm-desktop-show-desktop))
  (when (lispm-desktop-editor-floating-p)
    (let* ((frame lispm-desktop-editor--frame)
           (_ (when (lispm-desktop-apps-out-p frame) (lispm-desktop-coherence-bring-in frame)))
           (screen (lispm-desktop-platinum-window-parent frame))
           (lispm-desktop-editor--moving t)
           (_ (lispm-desktop-editor--take-off-chrome frame))
           (workspace (with-selected-frame frame (lispm-desktop-editor--current-workspace))))
      (lispm-desktop-editor--unshade)
      (setq lispm-desktop-editor--geometry (lispm-desktop-editor--geometry-of frame))
      (lispm-desktop-editor--move-workspaces frame screen)
      (setq lispm-desktop-editor--frame nil)
      (select-frame-set-input-focus screen)
      (tab-bar-switch-to-tab workspace)
      (delete-frame frame)
      (let ((lispm-desktop-editor--moving nil))
        (lispm-desktop-editor--restore-chrome))
      (lispm-desktop-apps-set-frontmost 'editor))))

(defun lispm-desktop-editor-toggle-floating ()
  "Move the editor between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lispm-desktop-editor-floating-p) (lispm-desktop-editor-full-screen) (lispm-desktop-editor-float)))

(defun lispm-desktop-editor--unshade ()
  "Roll the floating editor down again, when it is rolled up."
  (when lispm-desktop-editor--shade
    (pcase-let ((`(,config . ,height) lispm-desktop-editor--shade))
      (setq lispm-desktop-editor--shade nil)
      (set-frame-height lispm-desktop-editor--frame height nil t)
      (set-window-configuration config))))

(defun lispm-desktop-editor-shade ()
  "Roll the floating editor up to its title strip, or down again, as Mac OS 9
rolled a window up into its title bar."
  (interactive)
  (when (lispm-desktop-editor-floating-p)
    (if lispm-desktop-editor--shade
        (lispm-desktop-editor--unshade)
      (with-selected-frame lispm-desktop-editor--frame
        (setq lispm-desktop-editor--shade (cons (current-window-configuration)
                                                (frame-text-height lispm-desktop-editor--frame)))
        ;; The strip's height is measured before its window goes: the one
        ;; window left fills the frame until the frame shrinks to it.
        (let* ((strip (get-buffer-window lispm-desktop-editor-strip-buffer-name))
               (height (if strip (window-pixel-height strip) 60))
               (window (car (lispm-desktop-main-area--windows)))
               (ignore-window-parameters t))
          (delete-other-windows window)
          (set-window-dedicated-p window nil)
          (set-window-buffer window (lispm-desktop-editor--strip-buffer))
          (set-frame-height lispm-desktop-editor--frame height nil t))))))

;;;; Menus

(defun lispm-desktop-editor--key (command)
  "Return the keys of COMMAND typed on the keyboard, as a menu shows them."
  (when-let* ((keys (seq-find #'lispm-desktop-popup--keyboard-p (where-is-internal command))))
    (key-description keys)))

(defun lispm-desktop-editor--item (label command &rest properties)
  "Return the menu item LABEL running COMMAND in the editor, and PROPERTIES.
The editor comes forward first: a menu bar click focuses the desktop
under it."
  `(:label ,label
    :action ,(lambda ()
               (lispm-desktop-editor--focus)
               (if (commandp command) (call-interactively command) (funcall command)))
    ,@(when (and (symbolp command) (lispm-desktop-editor--key command))
        (list :key (lispm-desktop-editor--key command)))
    ,@properties))

(defun lispm-desktop-editor--in-editor (function)
  "Call FUNCTION with the editor's frame and its selected window selected."
  (with-selected-frame (lispm-desktop-editor-frame)
    (funcall function)))

(defun lispm-desktop-editor--file-items ()
  "Return the items of the editor's File menu."
  (let ((buffer (lispm-desktop-editor--in-editor (lambda () (window-buffer (selected-window))))))
    (list (lispm-desktop-editor--item "New File…" #'find-file)
          (lispm-desktop-editor--item "Open Project…" #'project-switch-project)
          ;; What C-x C-r runs, so the menu shows that key.
          (lispm-desktop-editor--item "Open Recent…" (cond ((fboundp 'consult-recent-file)
                                                            #'consult-recent-file)
                                                           ((fboundp 'recentf-open) #'recentf-open)
                                                           (t #'find-file)))
          'separator
          (lispm-desktop-editor--item "Save" #'save-buffer
                                      :enabled (and (buffer-file-name buffer) (buffer-modified-p buffer) t))
          (lispm-desktop-editor--item "Save As…" #'write-file)
          (lispm-desktop-editor--item "Revert" #'revert-buffer
                                      :enabled (and (buffer-file-name buffer) t))
          'separator
          (lispm-desktop-editor--item "Close Window" (lambda () (lispm-desktop-platinum-close-window (selected-window))))
          (lispm-desktop-editor--item "Close Emacs" #'lispm-desktop-editor-close))))

(defun lispm-desktop-editor--edit-items ()
  "Return the items of the editor's Edit menu."
  (list (lispm-desktop-editor--item "Undo" #'undo)
        (lispm-desktop-editor--item "Redo" #'undo-redo)
        'separator
        (lispm-desktop-editor--item "Cut" #'kill-region)
        (lispm-desktop-editor--item "Copy" #'lispm-desktop-copy-region)
        (lispm-desktop-editor--item "Paste" #'lispm-desktop-paste)
        (lispm-desktop-editor--item "Select All" #'mark-whole-buffer)
        'separator
        (lispm-desktop-editor--item "Find…" #'isearch-forward)
        (lispm-desktop-editor--item "Replace…" #'query-replace)))

(defun lispm-desktop-editor--view-items ()
  "Return the items of the editor's View menu: the sidebar's views, and
the editor full screen or floating."
  (let ((shown (lispm-desktop-editor--in-editor #'lispm-desktop-sidebar-current-view)))
    `(,@(mapcar (lambda (view)
                  (lispm-desktop-editor--item (plist-get (cdr view) :title)
                                              (lambda () (lispm-desktop-sidebar-select (car view)))
                                              :checked (eq (car view) shown)))
                lispm-desktop-sidebar-views)
      separator
      ,(lispm-desktop-editor--item "Full Screen" #'lispm-desktop-editor-full-screen
                                   :checked (not (lispm-desktop-editor-floating-p)))
      ,(lispm-desktop-editor--item "Floating Window" #'lispm-desktop-editor-float
                                   :checked (lispm-desktop-editor-floating-p)))))

(defun lispm-desktop-editor--window-items ()
  "Return the items of the editor's Window menu: the buffer manager, then
the workspace's buffers, the one shown checked, as Mac OS 9's Window menus
listed an application's windows."
  (lispm-desktop-editor--in-editor
   (lambda ()
     (let ((shown (window-buffer (selected-window))))
       `(,(lispm-desktop-editor--item "Buffer Manager" #'lispm-desktop-buffer-manager)
         separator
         ,@(mapcar (lambda (buffer)
                     (lispm-desktop-editor--item (truncate-string-to-width
                                                  (lispm-desktop-platinum--buffer-title buffer) 48 nil nil "…")
                                                 (lambda () (switch-to-buffer buffer))
                                                 :checked (eq buffer shown)))
                   (seq-take (lispm-desktop-platinum--window-buffers (selected-window)) 30)))))))

(defun lispm-desktop-editor-menus ()
  "Return the editor's menus, for the menu bar while it is in front."
  '((lispm-desktop-editor-file "File" lispm-desktop-editor--file-items)
    (lispm-desktop-editor-edit "Edit" lispm-desktop-editor--edit-items)
    (lispm-desktop-editor-view "View" lispm-desktop-editor--view-items)
    ;; The workspaces show as thumbnails, see lispm-desktop-workspaces.el.
    (lispm-desktop-editor-project "Project" lispm-desktop-workspaces-menu-panel)
    (lispm-desktop-editor-window "Window" lispm-desktop-editor--window-items)))

;;;; Setup

(defconst lispm-desktop-editor-splash-buffer-name "*GNU Emacs*"
  "Name of the buffer of Emacs's own startup screen.")

(defun lispm-desktop-editor-splash-buffer ()
  "Return the buffer of Emacs's own startup screen, its big logo over the
links to start with, made when missing without being shown."
  (or (get-buffer lispm-desktop-editor-splash-buffer-name)
      (save-window-excursion
        (display-startup-screen)
        (get-buffer lispm-desktop-editor-splash-buffer-name))))

(defun lispm-desktop-editor--new-workspace-buffer ()
  "Show Emacs's startup screen in a new workspace's window; return nil.
This is the editor's `tab-bar-new-tab-choice'; the screen is put in the
window itself, see `lispm-desktop-show-in-new-tab'."
  (lispm-desktop-show-in-new-tab (lispm-desktop-editor-splash-buffer)))

(defun lispm-desktop-editor--show-splash (window)
  "Show Emacs's startup screen in WINDOW when it is in an editor workspace
holding nothing else, and return non-nil; else return nil.
This is the editor's `lispm-desktop-empty-function'."
  (when (lispm-desktop-editor--in-workspace-p (window-frame window))
    (set-window-buffer window (lispm-desktop-editor-splash-buffer))
    (set-window-start window 1)
    t))

(defun lispm-desktop-editor--prepare-view ()
  "Bring the editor in front before a sidebar view is shown outside it."
  (unless (lispm-desktop-editor--frame-p (selected-frame))
    (lispm-desktop-editor-activate)
    (select-frame (lispm-desktop-editor-frame))))

(defvar lispm-desktop-sidebar-prepare-function)
(defvar lispm-desktop-sidebar-change-hook)

(defun lispm-desktop-setup-editor ()
  "Make Emacs the editor an application on the desktop."
  (setq lispm-desktop-sidebar-prepare-function #'lispm-desktop-editor--prepare-view
        lispm-desktop-empty-function #'lispm-desktop-editor--show-splash
        ;; The editor opens on Emacs's own startup screen: Emacs starts in
        ;; its first workspace, and a new workspace starts there too.
        initial-buffer-choice #'lispm-desktop-editor-splash-buffer
        tab-bar-new-tab-choice #'lispm-desktop-editor--new-workspace-buffer)
  (add-hook 'lispm-desktop-sidebar-change-hook #'lispm-desktop-editor--render-strip)
  (add-hook 'post-command-hook #'lispm-desktop-editor--keep-chrome)
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-editor--remember-workspace)
  (add-hook 'tab-bar-tab-post-select-functions
            (lambda (&rest _) (lispm-desktop-editor--render-strip)) 90)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-editor--refocus)
  (add-hook 'lispm-desktop-activate-hook #'lispm-desktop-editor--show-with-desktop)
  (add-to-list 'display-buffer-alist '(lispm-desktop-editor--astray-p lispm-desktop-editor-display)))

(provide 'lispm-desktop-editor)
;;; lispm-desktop-editor.el ends here
