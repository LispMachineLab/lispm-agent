;;; lispm-desktop-apps.el --- The applications on the desktop  -*- lexical-binding: t; -*-

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
(require 'lispm-desktop-platinum)

(defvar lispm-desktop-apps
  '((desktop :name "Desktop" :icon "disk" :menus lispm-desktop-menus
             :activate lispm-desktop-activate)
    (editor :name "Emacs" :icon emacs :menus lispm-desktop-editor-menus
            :activate lispm-desktop-editor-activate)
    (browser :name "Browser" :icon "netscape" :menus lispm-desktop-browser-menus
             :activate lispm-desktop-browser-show :visible lispm-desktop-xwidget-available-p
             :running lispm-desktop-browser--buffers)
    (settings :name "Control Panels" :icon "control-panels" :menus lispm-desktop-settings-menus
              :activate lispm-desktop-settings-show :visible lispm-desktop-settings-open-p))
  "The applications, in the Application menu's order: (KEY . PROPERTIES).
:name and :icon (see `lispm-desktop-platinum-icon-file') name an application,
:menus is a function returning its menus as
`lispm-desktop-platinum-menus-function' does, :activate brings it in front,
:visible, when given, is a function deciding whether it can run here, and
:running one deciding whether it runs now; without it, an application runs
whenever it can.")

(defvar lispm-desktop-apps--frontmost nil
  "The application in front, a key of `lispm-desktop-apps', or nil at first.")

(defun lispm-desktop-apps-frontmost ()
  "Return the application in front, a key of `lispm-desktop-apps'."
  (or lispm-desktop-apps--frontmost 'desktop))

(defvar lispm-desktop-apps-frontmost-functions nil
  "Functions called with the application in front whenever it is set,
changed or not, as windows follow it: the desktop's Finder window shows
with the desktop and hides from the full-screen editor.")

(defun lispm-desktop-apps-set-frontmost (app)
  "Put APP, a key of `lispm-desktop-apps', in front; redraw the menu bar."
  (unless (eq app lispm-desktop-apps--frontmost)
    (setq lispm-desktop-apps--frontmost app)
    (force-mode-line-update t))
  (run-hook-with-args 'lispm-desktop-apps-frontmost-functions app))

(defun lispm-desktop-apps-screen ()
  "Return the frame the desktop and the applications are drawn in: the
selected frame's outermost parent."
  (let ((frame (selected-frame)))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defun lispm-desktop-apps-windows (&optional screen)
  "Return the floating windows over SCREEN: child frames marked as windows.
They carry the frame parameter `lispm-desktop-window'."
  (let ((screen (or screen (lispm-desktop-apps-screen))))
    (seq-filter (lambda (frame)
                  (and (eq (frame-parent frame) screen)
                       (frame-parameter frame 'lispm-desktop-window)))
                (frame-list))))

(defun lispm-desktop-apps--geometry (frame)
  "Return where FRAME is: (LEFT TOP TEXT-WIDTH TEXT-HEIGHT) in pixels."
  (pcase-let ((`(,left . ,top) (frame-position frame)))
    (list left top (frame-text-width frame) (frame-text-height frame))))

(defun lispm-desktop-apps--put (frame place)
  "Put floating FRAME at PLACE, kept whole on its screen's desktop: under
the menu bar, and no larger than the desktop.  PLACE is a value of
`lispm-desktop-apps--geometry'.
On macOS a child frame is not clipped to its parent, so a window let past
the desktop's edge would hang out of Emacs's frame over the screen."
  (when-let* (((frame-live-p frame))
              (screen (frame-parent frame)))
    (pcase-let* ((`(,left ,top ,width ,height) place)
                 (top-min (tab-bar-height screen t))
                 (screen-width (frame-native-width screen))
                 (screen-height (frame-native-height screen))
                 ;; What the frame adds around its text: borders, mostly.
                 (extra-width (- (frame-pixel-width frame) (frame-text-width frame)))
                 (extra-height (- (frame-pixel-height frame) (frame-text-height frame)))
                 (fit-width (max 200 (min width (- screen-width extra-width))))
                 (fit-height (max 100 (min height (- screen-height top-min extra-height)))))
      (unless (and (= fit-width (frame-text-width frame))
                   (= fit-height (frame-text-height frame)))
        (set-frame-size frame fit-width fit-height t))
      (set-frame-position
       frame
       (max 0 (min left (- screen-width fit-width extra-width)))
       (max top-min (min top (- screen-height fit-height extra-height)))))))

(defun lispm-desktop-apps-keep-on-desktop (frame)
  "Keep floating FRAME on the desktop where it was just put or moved, and
remember that place: the window system moves child frames on its own when
their screen changes size, so they are put back there afterwards."
  (when (frame-live-p frame)
    (lispm-desktop-apps--put frame (lispm-desktop-apps--geometry frame))
    (set-frame-parameter frame 'lispm-desktop-window-place (lispm-desktop-apps--geometry frame))))

(defun lispm-desktop-apps--place-windows (screen &optional again)
  "Put the floating windows of SCREEN back in their places, each kept on the
desktop and given back its own size where there is room.  AGAIN forces
the window system to place them anew and shows them again: going full
screen or back, macOS moves child windows its own way and may leave them
out of sight while Emacs takes them for shown."
  (when (frame-live-p screen)
    (dolist (window (lispm-desktop-apps-windows screen))
      (let ((place (or (frame-parameter window 'lispm-desktop-window-place)
                       (lispm-desktop-apps--geometry window))))
        (when again
          ;; A move to where Emacs takes the window to be would be skipped.
          (set-frame-position window (1+ (car place)) (cadr place)))
        (lispm-desktop-apps--put window place)
        (when (and again (frame-visible-p window))
          (make-frame-invisible window t)
          (make-frame-visible window))))))

(defun lispm-desktop-apps--screen-resized (frame)
  "Put the floating windows of FRAME back in their places when it changed
size, and once more after it went full screen or back, when the window
system is done with them.  This is a `window-size-change-functions'
function."
  (unless (frame-parent frame)
    (lispm-desktop-apps--place-windows frame)
    (let ((fullscreen (frame-parameter frame 'fullscreen)))
      (unless (equal fullscreen (frame-parameter frame 'lispm-desktop-apps-fullscreen))
        (set-frame-parameter frame 'lispm-desktop-apps-fullscreen fullscreen)
        (run-at-time 1 nil #'lispm-desktop-apps--place-windows frame t)))))

(defun lispm-desktop-apps--available ()
  "Return the applications that can run here."
  (seq-filter (lambda (app)
                (let ((visible (plist-get (cdr app) :visible)))
                  (or (not visible) (and (fboundp visible) (funcall visible)))))
              lispm-desktop-apps))

(defun lispm-desktop-apps-activate (app)
  "Bring APP, a key of `lispm-desktop-apps', in front."
  (funcall (plist-get (alist-get app lispm-desktop-apps) :activate)))

(defun lispm-desktop-apps--running ()
  "Return the applications running now, in the Application menu's order."
  (seq-filter (lambda (app)
                (let ((running (plist-get (cdr app) :running)))
                  (or (not running) (and (fboundp running) (funcall running)))))
              (lispm-desktop-apps--available)))

(defun lispm-desktop-apps-next ()
  "Bring the next running application in front, as the Application menu
would, wrapping around: the desktop, the editor, the browser while it
has a page open, Control Panels while they are open."
  (interactive)
  (let* ((apps (mapcar #'car (lispm-desktop-apps--running)))
         (index (or (seq-position apps (lispm-desktop-apps-frontmost)) -1)))
    (when apps
      (lispm-desktop-apps-activate (nth (mod (1+ index) (length apps)) apps)))))

(defun lispm-desktop-apps-menus ()
  "Return the frontmost application's menus, for the menu bar."
  (let ((menus (plist-get (alist-get (lispm-desktop-apps-frontmost) lispm-desktop-apps) :menus)))
    (and menus (fboundp menus) (funcall menus))))

(defun lispm-desktop-apps-application ()
  "Return the frontmost application as (ICON . NAME), for the Application menu."
  (let ((spec (alist-get (lispm-desktop-apps-frontmost) lispm-desktop-apps)))
    (cons (lispm-desktop-platinum-icon (plist-get spec :icon)) (plist-get spec :name))))

(defun lispm-desktop-apps-application-items ()
  "Return the Application menu's items: every application, the front one
checked, as Mac OS 9's listed the running applications."
  (mapcar (pcase-lambda (`(,key . ,spec))
            (list :label (plist-get spec :name)
                  :action (lambda () (lispm-desktop-apps-activate key))
                  :checked (eq key (lispm-desktop-apps-frontmost))))
          (lispm-desktop-apps--available)))

(defvar lispm-desktop-tab-name)
(defvar lispm-desktop-browser-workspace-name)
(defvar lispm-desktop-editor--moving)

(defun lispm-desktop-apps--follow-workspace (&rest _)
  "Put in front the application whose workspace was just selected.
The desktop's and the browser's workspaces are theirs; any other, and
any workspace of the floating editor, is the editor's.  This is a
`tab-bar-tab-post-select-functions' function."
  (unless (bound-and-true-p lispm-desktop-editor--moving)
    (let ((name (alist-get 'name (tab-bar--current-tab))))
      (lispm-desktop-apps-set-frontmost
       (cond ((frame-parent (selected-frame)) 'editor)
             ((equal name (bound-and-true-p lispm-desktop-tab-name)) 'desktop)
             ((equal name (bound-and-true-p lispm-desktop-browser-workspace-name)) 'browser)
             (t 'editor))))))

(defun lispm-desktop-apps--follow-window (&rest _)
  "Put in front the application whose floating window was just selected,
as the frame's `lispm-desktop-app' parameter names it, and raise the window over
the other floating windows.  A click in a window selects
its frame; the focus cannot tell, as macOS reports every child frame
focused along with its parent.  The screen names no application: a click
on the menu bar selects it, and the application in front stays so.
This is a `window-selection-change-functions' function."
  (let ((frame (selected-frame)))
    (when-let* ((app (frame-parameter frame 'lispm-desktop-app)))
      (unless (eq app lispm-desktop-apps--frontmost)
        (lispm-desktop-apps-set-frontmost app))
      ;; The window clicked comes over the other floating windows, as on
      ;; Mac OS 9; macOS leaves a child window where it was.
      (when (frame-parameter frame 'lispm-desktop-window)
        (raise-frame frame)))))

(defun lispm-desktop-setup-apps ()
  "Draw the menu bar and the Application menu from the application in front."
  (setq lispm-desktop-platinum-menus-function #'lispm-desktop-apps-menus
        lispm-desktop-platinum-application-function #'lispm-desktop-apps-application
        lispm-desktop-platinum-applications-function #'lispm-desktop-apps-application-items)
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-apps--follow-workspace)
  (add-hook 'window-selection-change-functions #'lispm-desktop-apps--follow-window)
  (add-hook 'window-size-change-functions #'lispm-desktop-apps--screen-resized)
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-apps-keep-on-desktop)
  ;; The next application from the keyboard, as Command-Tab is the
  ;; system's.
  (keymap-global-set "C-c o" #'lispm-desktop-apps-next))

(provide 'lispm-desktop-apps)
;;; lispm-desktop-apps.el ends here
