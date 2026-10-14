;;; lme-apps.el --- The applications on the desktop  -*- lexical-binding: t; -*-

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

;; Mac OS 9 ran applications over its desktop, one of them in front at a
;; time: its menus filled the menu bar after the Apple menu, and the
;; Application menu at the right end named it and listed the others.
;; Here the applications are the desktop itself, Emacs the editor and the
;; browser:
;;
;;   desktop   the desktop's workspace: the desktop alone
;;   editor    the project workspaces, full screen or in a floating window
;;   browser   the browser's workspace
;;
;; Which one is in front is kept here, as the menu bar is drawn from it;
;; clicking the menu bar, which focuses the frame under it, does not change
;; it.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lme-platinum)

(defvar lme-apps
  '((desktop :name "Desktop" :icon "disk" :menus lme-desktop-menus
             :activate lme-desktop-activate)
    (editor :name "Emacs" :icon emacs :menus lme-editor-menus
            :activate lme-editor-activate)
    (browser :name "Browser" :icon "globe" :menus lme-browser-menus
             :activate lme-browser-show :visible lme-xwidget-available-p))
  "The applications, in the Application menu's order: (KEY . PROPERTIES).
:name and :icon (see `lme-platinum-icon-file') name an application, :menus is a
function returning its menus as `lme-platinum-menus-function' does,
:activate brings it in front, and :visible, when given, is a function
deciding whether it can run here.")

(defvar lme-apps--frontmost nil
  "The application in front, a key of `lme-apps', or nil before the first.")

(defun lme-apps-frontmost ()
  "Return the application in front, a key of `lme-apps'."
  (or lme-apps--frontmost 'desktop))

(defvar lme-apps-frontmost-functions nil
  "Functions called with the application in front whenever it is set,
changed or not, as windows follow it: the desktop's Finder window shows
with the desktop and hides from the full-screen editor.")

(defun lme-apps-set-frontmost (app)
  "Put APP, a key of `lme-apps', in front, and redraw the menu bar for it."
  (unless (eq app lme-apps--frontmost)
    (setq lme-apps--frontmost app)
    (force-mode-line-update t))
  (run-hook-with-args 'lme-apps-frontmost-functions app))

(defun lme-apps-screen ()
  "Return the frame the desktop and the applications are drawn in: the
selected frame's outermost parent."
  (let ((frame (selected-frame)))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defconst lme-apps-window-reach 80
  "Pixels of a floating window that stay on the desktop however it is moved,
so its title bar can always be caught again.")

(defun lme-apps-windows (&optional screen)
  "Return the floating windows over SCREEN: child frames marked `lme-window'."
  (let ((screen (or screen (lme-apps-screen))))
    (seq-filter (lambda (frame)
                  (and (eq (frame-parent frame) screen)
                       (frame-parameter frame 'lme-window)))
                (frame-list))))

(defun lme-apps-keep-on-desktop (frame)
  "Keep floating FRAME on its screen's desktop, as Mac OS 9 kept a window's
title bar on the screen: under the menu bar, never wider or taller than
the desktop, and at least `lme-apps-window-reach' pixels of it in sight.
Placing it again also mends a position the window system left stale."
  (when-let* (((frame-live-p frame))
              (screen (frame-parent frame)))
    (let* ((top-min (tab-bar-height screen t))
           (screen-width (frame-native-width screen))
           (screen-height (frame-native-height screen))
           (width (frame-pixel-width frame))
           (height (frame-pixel-height frame))
           (fit-width (min width screen-width))
           (fit-height (min height (- screen-height top-min))))
      (unless (and (= fit-width width) (= fit-height height))
        (set-frame-size frame
                        (max 200 (- (frame-text-width frame) (- width fit-width)))
                        (max 100 (- (frame-text-height frame) (- height fit-height)))
                        t)
        (setq width (frame-pixel-width frame)))
      (pcase-let ((`(,left . ,top) (frame-position frame)))
        (set-frame-position
         frame
         (max (- lme-apps-window-reach width)
              (min left (- screen-width lme-apps-window-reach)))
         (max top-min (min top (- screen-height lme-platinum-title-height))))))))

(defun lme-apps--screen-resized (frame)
  "Place the floating windows of FRAME again when it changed size, as going
full screen and back does.  This is a `window-size-change-functions'
function."
  (unless (frame-parent frame)
    (mapc #'lme-apps-keep-on-desktop (lme-apps-windows frame))))

(defun lme-apps--available ()
  "Return the applications that can run here."
  (seq-filter (lambda (app)
                (let ((visible (plist-get (cdr app) :visible)))
                  (or (not visible) (and (fboundp visible) (funcall visible)))))
              lme-apps))

(defun lme-apps-activate (app)
  "Bring APP, a key of `lme-apps', in front."
  (funcall (plist-get (alist-get app lme-apps) :activate)))

(defun lme-apps-menus ()
  "Return the frontmost application's menus, for the menu bar."
  (let ((menus (plist-get (alist-get (lme-apps-frontmost) lme-apps) :menus)))
    (and menus (fboundp menus) (funcall menus))))

(defun lme-apps-application ()
  "Return the frontmost application as (ICON . NAME), for the Application menu."
  (let ((spec (alist-get (lme-apps-frontmost) lme-apps)))
    (cons (lme-platinum-icon (plist-get spec :icon)) (plist-get spec :name))))

(defun lme-apps-application-items ()
  "Return the Application menu's items: every application, the front one
checked, as Mac OS 9's listed the running applications."
  (mapcar (pcase-lambda (`(,key . ,spec))
            (list :label (plist-get spec :name)
                  :action (lambda () (lme-apps-activate key))
                  :checked (eq key (lme-apps-frontmost))))
          (lme-apps--available)))

(defvar lme-desktop-tab-name)
(defvar lme-browser-workspace-name)
(defvar lme-editor--moving)

(defun lme-apps--follow-workspace (&rest _)
  "Put in front the application whose workspace was just selected.
The desktop's and the browser's workspaces are theirs; any other, and
any workspace of the floating editor, is the editor's.  This is a
`tab-bar-tab-post-select-functions' function."
  (unless (bound-and-true-p lme-editor--moving)
    (let ((name (alist-get 'name (tab-bar--current-tab))))
      (lme-apps-set-frontmost
       (cond ((frame-parent (selected-frame)) 'editor)
             ((equal name (bound-and-true-p lme-desktop-tab-name)) 'desktop)
             ((equal name (bound-and-true-p lme-browser-workspace-name)) 'browser)
             (t 'editor))))))

(defun lme-setup-apps ()
  "Draw the menu bar and the Application menu from the application in front."
  (setq lme-platinum-menus-function #'lme-apps-menus
        lme-platinum-application-function #'lme-apps-application
        lme-platinum-applications-function #'lme-apps-application-items)
  (add-hook 'tab-bar-tab-post-select-functions #'lme-apps--follow-workspace)
  (add-hook 'window-size-change-functions #'lme-apps--screen-resized)
  (add-hook 'lme-platinum-frame-moved-functions #'lme-apps-keep-on-desktop))

(provide 'lme-apps)
;;; lme-apps.el ends here
