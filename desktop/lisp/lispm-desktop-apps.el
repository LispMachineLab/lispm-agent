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
;;
;; The applications' windows float over the desktop, child frames of the
;; screen, Emacs's frame the desktop is drawn in; one can also be out on
;; the screen of the computer, a frame of its own that still belongs to
;; the desktop, see lispm-desktop-coherence.el.

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

(defun lispm-desktop-apps-out-p (frame)
  "Return the screen FRAME belongs to when FRAME is out on the computer's
screen, a frame of its own rather than a child of the screen, or nil.
Windows out carry it as their frame parameter `lispm-desktop-out', and
so does the minibuffer they share."
  (let ((screen (frame-parameter frame 'lispm-desktop-out)))
    (and (frame-live-p screen) screen)))

(defun lispm-desktop-apps-screen-p (frame)
  "Return non-nil when FRAME is a screen, the frame the desktop is drawn
in: one of its own that is neither a floating window out on the
computer's screen nor the minibuffer they share."
  (not (or (frame-parent frame) (frame-parameter frame 'lispm-desktop-window)
           (lispm-desktop-apps-out-p frame) (lispm-desktop-platinum-chrome-frame-p frame))))

(defun lispm-desktop-apps-window-screen (frame)
  "Return the screen FRAME, a floating window, belongs to: the one it floats
over, or the screen it left, when it is out."
  (or (lispm-desktop-platinum-window-parent frame) (lispm-desktop-apps-out-p frame)))

(defun lispm-desktop-apps-screen ()
  "Return the frame the desktop and the applications are drawn in: the
selected frame's outermost parent, or the screen a window out belongs to."
  (let ((frame (selected-frame)))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    (or (lispm-desktop-apps-out-p frame) frame)))

(defun lispm-desktop-apps-hidden-p (&optional screen)
  "Return non-nil while SCREEN, by default the selected one, is hidden in the
Dock, its windows out on the computer's screen."
  (frame-parameter (or screen (lispm-desktop-apps-screen)) 'lispm-desktop-hidden))

(defun lispm-desktop-apps-windows (&optional screen)
  "Return the floating windows of SCREEN: child frames marked as windows,
and those of its windows that are out.  They carry the frame parameter
`lispm-desktop-window'."
  (let ((screen (or screen (lispm-desktop-apps-screen))))
    (seq-filter (lambda (frame)
                  (and (eq (lispm-desktop-apps-window-screen frame) screen)
                       (frame-parameter frame 'lispm-desktop-window)))
                (frame-list))))

(defun lispm-desktop-apps--screen-origin (screen)
  "Return where SCREEN's inside starts on the computer's screen: (X . Y).
A child frame's position counts from there."
  (pcase-let ((`(,left ,top . ,_) (frame-edges screen 'native-edges)))
    (cons left top)))

(defun lispm-desktop-apps--geometry (frame)
  "Return where FRAME is: (LEFT TOP TEXT-WIDTH TEXT-HEIGHT) in pixels, over
its screen, whether it floats there or is out."
  (pcase-let ((`(,left . ,top) (lispm-desktop-platinum-window-position frame)))
    (when-let* ((screen (and (not (lispm-desktop-platinum-window-parent frame))
                             (lispm-desktop-apps-out-p frame))))
      (pcase-let ((`(,x . ,y) (lispm-desktop-apps--screen-origin screen)))
        (setq left (- left x) top (- top y))))
    (list left top (frame-text-width frame) (frame-text-height frame))))

(defun lispm-desktop-apps--workarea (frame)
  "Return the part of the computer's screen windows may use where FRAME is:
\(X Y WIDTH HEIGHT), the menu bar and the Dock left out."
  (or (frame-monitor-workarea frame) (list 0 0 (display-pixel-width) (display-pixel-height))))

(defun lispm-desktop-apps--keep-on-screen (frame)
  "Keep FRAME, a window out, where its title bar can still be reached: its
top under the menu bar, and some of it left on the screen's sides.  The
frame carrying it moves, see `lispm-desktop-platinum-window-outer'."
  (pcase-let* ((outer (lispm-desktop-platinum-window-outer frame))
               (`(,x ,y ,w ,h) (lispm-desktop-apps--workarea frame))
               (`(,left . ,top) (frame-position outer))
               (width (frame-pixel-width outer))
               (keep 80))
    (let ((new-left (max (- (+ x keep) width) (min left (- (+ x w) keep))))
          (new-top (max y (min top (- (+ y h) 30)))))
      (unless (and (= new-left left) (= new-top top))
        (set-frame-position outer new-left new-top)))))

(declare-function lispm-desktop-coherence-take-out "lispm-desktop-coherence" (frame))

(defun lispm-desktop-apps-windows-out-in-order (screen)
  "Return the windows of SCREEN out on the computer's screen, the one in
front first.  The window system orders the frames carrying them."
  (let ((windows (lispm-desktop-apps-windows screen)))
    (seq-filter (lambda (frame) (memq frame windows))
                (mapcar (lambda (frame) (or (lispm-desktop-platinum-window-frame-p frame) frame))
                        (frame-list-z-order)))))

(defun lispm-desktop-apps-focus-after (frame)
  "Give the keyboard to what comes after FRAME, a floating window going
away: its screen, or while the screen is hidden, the window over the
others out on the computer's screen."
  (let ((screen (lispm-desktop-apps-window-screen frame)))
    (cond ((not (frame-live-p screen)))
          ((lispm-desktop-apps-hidden-p screen)
           (when-let* ((next (seq-find (lambda (other)
                                         (and (not (eq other frame)) (frame-visible-p other)))
                                       (lispm-desktop-apps-windows-out-in-order screen))))
             (select-frame-set-input-focus next)))
          (t (select-frame-set-input-focus screen)))))

(defun lispm-desktop-apps--put (frame place)
  "Put floating FRAME at PLACE, kept whole on its screen's desktop: under
the menu bar, and no larger than the desktop.  PLACE is a value of
`lispm-desktop-apps--geometry'.
On macOS a child frame is not clipped to its parent, so a window let past
the desktop's edge would hang out of Emacs's frame over the screen."
  (when-let* (((frame-live-p frame))
              (screen (lispm-desktop-platinum-window-parent frame)))
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
        (set-frame-size frame fit-width fit-height t)
        (lispm-desktop-platinum--frame-update frame))
      (let ((x (max 0 (min left (- screen-width fit-width extra-width))))
            (y (max top-min (min top (- screen-height fit-height extra-height)))))
        (unless (equal (cons x y) (lispm-desktop-platinum-window-position frame))
          (lispm-desktop-platinum-move-window frame x y))))))

(defun lispm-desktop-apps-keep-on-desktop (frame)
  "Keep floating FRAME on the desktop where it was just put or moved, and
remember that place: the window system moves child frames on its own when
their screen changes size, so they are put back there afterwards.  A
window out is kept on the computer's screen instead, and one shown while
its screen is hidden goes out there at once."
  (when (frame-live-p frame)
    (when (and (lispm-desktop-platinum-window-parent frame)
               (lispm-desktop-apps-hidden-p (lispm-desktop-platinum-window-parent frame))
               (fboundp 'lispm-desktop-coherence-take-out))
      (lispm-desktop-coherence-take-out frame))
    (if (lispm-desktop-apps-out-p frame)
        (lispm-desktop-apps--keep-on-screen frame)
      (lispm-desktop-apps--put frame (lispm-desktop-apps--geometry frame))
      (set-frame-parameter frame 'lispm-desktop-window-place (lispm-desktop-apps--geometry frame)))))

(defun lispm-desktop-apps--place-windows (screen &optional again)
  "Put the floating windows of SCREEN back in their places, each kept on the
desktop and given back its own size where there is room.  AGAIN forces
the window system to place them anew and shows them again: going full
screen or back, macOS moves child windows its own way and may leave them
out of sight while Emacs takes them for shown."
  (when (frame-live-p screen)
    (dolist (window (seq-filter #'lispm-desktop-platinum-window-parent
                                (lispm-desktop-apps-windows screen)))
      (let ((place (or (frame-parameter window 'lispm-desktop-window-place)
                       (lispm-desktop-apps--geometry window))))
        (when again
          ;; A move to where Emacs takes the window to be would be skipped.
          (lispm-desktop-platinum-move-window window (1+ (car place)) (cadr place)))
        (lispm-desktop-apps--put window place)
        (when (and again (frame-visible-p window))
          (lispm-desktop-platinum-hide-window window)
          (lispm-desktop-platinum-show-window window))))))

(defun lispm-desktop-apps--screen-resized (frame)
  "Put the floating windows of FRAME back in their places when it changed
size, and once more after it went full screen or back, when the window
system is done with them.  This is a `window-size-change-functions'
function."
  (unless (or (frame-parent frame) (lispm-desktop-platinum-chrome-frame-p frame))
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

(declare-function lispm-desktop-coherence-available-p "lispm-desktop-coherence" (&optional screen))

(defun lispm-desktop-apps-application-items ()
  "Return the Application menu's items: Hide Desktop or Show Desktop, as
Mac OS 9's began with Hide and Show, then every application, the front
one checked, as Mac OS 9's listed the running applications."
  `(,@(when (and (fboundp 'lispm-desktop-coherence-available-p)
                 (lispm-desktop-coherence-available-p (lispm-desktop-apps-screen)))
        (if (lispm-desktop-apps-hidden-p)
            '((:label "Show Desktop" :action lispm-desktop-show-desktop :key "C-c h") separator)
          '((:label "Hide Desktop" :action lispm-desktop-hide-desktop :key "C-c h") separator)))
    ,@(mapcar (pcase-lambda (`(,key . ,spec))
                (list :label (plist-get spec :name)
                      :action (lambda () (lispm-desktop-apps-activate key))
                      :checked (eq key (lispm-desktop-apps-frontmost))))
              (lispm-desktop-apps--available))))

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
        (lispm-desktop-platinum-raise-window frame)))))

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
