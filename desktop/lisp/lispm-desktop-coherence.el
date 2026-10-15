;;; lispm-desktop-coherence.el --- Windows out on the screen, the desktop in the Dock  -*- lexical-binding: t; -*-

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

;; Parallels' Coherence and VMware's Unity show a virtual machine's
;; windows on the computer's own desktop, the guest's desktop out of
;; sight.  The desktop's floating windows, the Finder's, the editor's,
;; the browser's and the control panels', can leave it so:
;;
;;   - a window dropped mostly past the desktop's edge goes out on the
;;     computer's screen, a frame of its own where it was dropped, and
;;     dropped back over the desktop, it goes in again;
;;   - Hide Desktop, in the Application menu or C-c h, takes every window
;;     out, the full-screen editor and browser floated first, and puts
;;     the desktop in the Dock; Show Desktop, C-c h again or a click on
;;     its tile in the Dock brings it back, the windows over it going in
;;     and the applications full screen as they were.
;;
;; A window out is still the desktop's: it keeps its title bar, its boxes
;; and its grow box, and moves among the computer's windows as they do.
;; The desktop's minibuffer is the screen's, in the Dock while it is
;; hidden, so the windows out share one of their own, a small window that
;; shows at the top of the computer's screen while it is read from, and
;; for a few seconds when it echoes a message.
;;
;; macOS minimizes no window without a title bar: a desktop drawn
;; undecorated is given one for as long as it is in the Dock.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-apps)

(declare-function lispm-desktop-editor-float "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-full-screen "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(declare-function lispm-desktop-browser-float "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-full-screen "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-floating-p "lispm-desktop-browser" ())
(declare-function lispm-desktop-activate "lispm-desktop-surface" ())
(declare-function lispm-desktop-platinum--color "lispm-desktop-platinum" (key))
(defvar lispm-desktop-platinum-frame-moved-functions)

(defgroup lispm-desktop-coherence nil
  "The desktop's windows out on the computer's screen."
  :group 'lispm-desktop)

(defcustom lispm-desktop-coherence-out-fraction 0.5
  "How much of a window dropped past the desktop's edge must be outside it
for the window to go out on the computer's screen; dropped with as much
of it over the desktop, a window out goes in again."
  :type 'number
  :group 'lispm-desktop-coherence)

(defun lispm-desktop-coherence-available-p ()
  "Return non-nil when windows can go out on the computer's screen: on
macOS, whose window server shows a child frame made a frame of its own
where it was."
  (and (eq (framep (selected-frame)) 'ns) (not noninteractive)))

;;;; Geometry

(defun lispm-desktop-coherence--outer-rectangle (frame)
  "Return FRAME's outer rectangle on the computer's screen: (X Y WIDTH HEIGHT).
A child frame's edges count from its parent's inside."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges frame 'outer-edges)))
    (when-let* ((parent (frame-parent frame)))
      (pcase-let ((`(,x . ,y) (lispm-desktop-apps--screen-origin parent)))
        (setq left (+ left x) right (+ right x) top (+ top y) bottom (+ bottom y))))
    (list left top (- right left) (- bottom top))))

(defun lispm-desktop-coherence-inside-fraction (rectangle area)
  "Return how much of RECTANGLE lies inside AREA, from 0 to 1.
Both are (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,x ,y ,w ,h) rectangle)
              (`(,ax ,ay ,aw ,ah) area))
    (if (or (<= w 0) (<= h 0))
        0
      (let ((iw (max 0 (- (min (+ x w) (+ ax aw)) (max x ax))))
            (ih (max 0 (- (min (+ y h) (+ ay ah)) (max y ay)))))
        (/ (* iw ih 1.0) (* w h))))))

(defun lispm-desktop-coherence--screen-rectangle (screen)
  "Return SCREEN's inside on the computer's screen: (X Y WIDTH HEIGHT)."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges screen 'native-edges)))
    (list left top (- right left) (- bottom top))))

;;;; The minibuffer of the windows out
;;
;; A frame of its own minibuffer, rather than a minibuffer-only frame:
;; there Vertico loses track of point while it reads a file's name.  Its
;; one other window shows which window asks, as a title over the field.

(defvar lispm-desktop-coherence--minibuffers nil
  "The minibuffer frames of the screens' windows out: (SCREEN . FRAME).")

(defconst lispm-desktop-coherence--title-buffer-name " *lispm-desktop-minibuffer-title*"
  "Name of the buffer of the title over the minibuffer of the windows out.")

(defun lispm-desktop-coherence--minibuffer (screen)
  "Return the minibuffer frame SCREEN's windows out read from, made when
missing.  It shows when Emacs reads from it, over the top of the
computer's screen as Spotlight's field, and hides again once done."
  (let ((frame (alist-get screen lispm-desktop-coherence--minibuffers)))
    (if (frame-live-p frame)
        frame
      (let ((frame (let ((after-make-frame-functions nil)
                         (before-make-frame-hook nil))
                     (make-frame
                      `((minibuffer . t) (name . "Minibuffer") (lispm-desktop-out . ,screen)
                        (undecorated . t) (visibility . nil) (unsplittable . t) (z-group . above)
                        ;; Keys typed in the asking window come to it; shown,
                        ;; it takes no focus from that window.
                        (no-accept-focus . t) (no-focus-on-map . t)
                        (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                        (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                        (internal-border-width . 6) (vertical-scroll-bars . nil)
                        (left-fringe . 0) (right-fringe . 0)
                        (width . 90) (height . 3)))))
            (title (get-buffer-create lispm-desktop-coherence--title-buffer-name)))
        (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum--color :chrome))
        (with-current-buffer title
          (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                      cursor-type nil cursor-in-non-selected-windows nil truncate-lines t
                      ;; One line is all the title needs.
                      window-min-height 1)
          (face-remap-add-relative 'default :background (lispm-desktop-platinum--color :chrome)))
        (let ((window (frame-root-window frame)))
          (set-window-buffer window title)
          (set-window-dedicated-p window t)
          (set-window-fringes window 0 0))
        (setf (alist-get screen lispm-desktop-coherence--minibuffers) frame)
        frame))))

(defun lispm-desktop-coherence--minibuffer-frame-p (frame)
  "Return non-nil when FRAME is the minibuffer of a screen's windows out."
  (rassq frame lispm-desktop-coherence--minibuffers))

(defun lispm-desktop-coherence--fit-minibuffer ()
  "Make the minibuffer of the windows out as tall as what it shows, the
field and the candidates under it, under its title."
  (when-let* ((frame (window-frame (minibuffer-window)))
              ((lispm-desktop-coherence--minibuffer-frame-p frame)))
    (let* ((mini (minibuffer-window frame))
           (title (frame-root-window frame))
           (wanted (+ (cdr (window-text-pixel-size mini nil nil 4000 4000))
                      (- (window-pixel-height mini) (window-body-height mini t))))
           (height (+ (window-min-size title nil nil t) wanted)))
      (unless (= (frame-text-height frame) height)
        (set-frame-height frame height nil t))
      ;; The title keeps its least, the field and its candidates the rest.
      (let ((delta (- wanted (window-pixel-height mini))))
        (unless (zerop delta)
          (window-resize mini delta nil nil t))))))

(defun lispm-desktop-coherence--title-minibuffer (frame asking)
  "Title FRAME, the minibuffer of the windows out, with the name of ASKING,
the window it reads or echoes for, and put it over the top of the
computer's screen that window is on."
  (let ((name (or (frame-parameter asking 'name) "Emacs"))
        (title (frame-root-window frame)))
    (with-current-buffer lispm-desktop-coherence--title-buffer-name
      (setq-local cursor-type nil cursor-in-non-selected-windows nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (make-string (max 0 (/ (- (window-body-width title) (string-width name)) 2)) ?\s)
                (propertize name 'face 'bold))))
    (pcase-let ((`(,x ,y ,w ,_) (lispm-desktop-apps--workarea asking)))
      (set-frame-position frame (+ x (/ (- w (frame-pixel-width frame)) 2)) (+ y 60)))))

(defun lispm-desktop-coherence--place-minibuffer ()
  "Show the minibuffer of the windows out over the top of the computer's
screen the asking window is on, titled with the window's name, and keep
it as tall as what it shows.  This is a `minibuffer-setup-hook' function,
run after Vertico's."
  (let ((frame (window-frame (minibuffer-window))))
    (when (lispm-desktop-coherence--minibuffer-frame-p frame)
      (lispm-desktop-coherence--stop-echo)
      (add-hook 'post-command-hook #'lispm-desktop-coherence--fit-minibuffer 90 t)
      (lispm-desktop-coherence--fit-minibuffer)
      (lispm-desktop-coherence--title-minibuffer frame (window-frame (minibuffer-selected-window)))
      (make-frame-visible frame)
      ;; Once Vertico has drawn its candidates.
      (run-at-time 0 nil #'lispm-desktop-coherence--fit-minibuffer))))

(defun lispm-desktop-coherence--hide-minibuffer ()
  "Hide the minibuffer of the windows out once nothing is read from it.
This is a `minibuffer-exit-hook' function."
  (run-at-time 0 nil
               (lambda ()
                 (when (zerop (minibuffer-depth))
                   (dolist (entry lispm-desktop-coherence--minibuffers)
                     (when (and (frame-live-p (cdr entry)) (frame-visible-p (cdr entry)))
                       (make-frame-invisible (cdr entry) t)))))))

;;;; Messages

(defcustom lispm-desktop-coherence-echo-seconds 4
  "Seconds a message shows in the minibuffer of the windows out."
  :type 'number
  :group 'lispm-desktop-coherence)

(defvar lispm-desktop-coherence--echo-timer nil
  "Timer hiding the minibuffer of the windows out after a message.")

(defun lispm-desktop-coherence--stop-echo ()
  "Let the minibuffer of the windows out stay, as it is read from now."
  (when (timerp lispm-desktop-coherence--echo-timer)
    (cancel-timer lispm-desktop-coherence--echo-timer))
  (setq lispm-desktop-coherence--echo-timer nil))

(defun lispm-desktop-coherence--echo (message)
  "Show MESSAGE, echoed for a window out, under the window's name in the
minibuffer of the windows out, for `lispm-desktop-coherence-echo-seconds'.
Emacs shows the frame a message goes to by itself; this titles it, sizes
it to the message and hides it again.  This is one of
`set-message-functions'; it lets the message be displayed as usual."
  (let ((asking (selected-frame)))
    (when-let* (((not (active-minibuffer-window)))
                (screen (lispm-desktop-apps-out-p asking))
                (frame (alist-get screen lispm-desktop-coherence--minibuffers))
                ((frame-live-p frame))
                ((eq (window-frame (minibuffer-window asking)) frame)))
      (lispm-desktop-coherence--title-minibuffer frame asking)
      (set-frame-height frame (+ 1 (min 5 (1+ (cl-count ?\n message)))))
      (lispm-desktop-coherence--stop-echo)
      (setq lispm-desktop-coherence--echo-timer
            (run-with-timer lispm-desktop-coherence-echo-seconds nil
                            (lambda ()
                              (setq lispm-desktop-coherence--echo-timer nil)
                              (when (and (frame-live-p frame) (not (active-minibuffer-window)))
                                (make-frame-invisible frame t)))))))
  nil)

;;;; Out and in

(defun lispm-desktop-coherence--move (frame x y)
  "Move FRAME, just given a new parent or none, to X, Y.
Emacs takes a frame reparented to be where it should be, whereas the
window server leaves it where it was: a move to that place would be
skipped, so FRAME is moved by a pixel first."
  (set-frame-position frame (1+ x) y)
  (set-frame-position frame x y))

(defun lispm-desktop-coherence-take-out (frame)
  "Take FRAME, a floating window over its screen, out on the computer's
screen: a frame of its own, where it was, reading from the minibuffer
of the windows out."
  (when-let* ((screen (frame-parent frame))
              ((frame-parameter frame 'lispm-desktop-window)))
    (pcase-let ((`(,x ,y . ,_) (lispm-desktop-coherence--outer-rectangle frame))
                (visible (frame-visible-p frame)))
      (set-frame-parameter frame 'minibuffer (minibuffer-window (lispm-desktop-coherence--minibuffer screen)))
      (modify-frame-parameters frame `((parent-frame . nil) (lispm-desktop-out . ,screen)))
      (lispm-desktop-coherence--move frame x y)
      (when visible
        (make-frame-visible frame)
        (raise-frame frame))
      (lispm-desktop-apps--keep-on-screen frame))))

(defun lispm-desktop-coherence-bring-in (frame)
  "Bring FRAME, a window out, back over its screen, where it is now."
  (when-let* ((screen (and (not (frame-parent frame)) (lispm-desktop-apps-out-p frame))))
    (pcase-let ((`(,x ,y . ,_) (lispm-desktop-coherence--outer-rectangle frame))
                (`(,ox . ,oy) (lispm-desktop-apps--screen-origin screen))
                (visible (frame-visible-p frame)))
      (modify-frame-parameters frame `((parent-frame . ,screen) (lispm-desktop-out . nil)))
      (set-frame-parameter frame 'minibuffer (minibuffer-window screen))
      (lispm-desktop-coherence--move frame (- x ox) (- y oy))
      (when visible
        (make-frame-visible frame)
        (raise-frame frame))
      (lispm-desktop-apps-keep-on-desktop frame))))

(defun lispm-desktop-coherence--dropped (frame)
  "Take FRAME out, or bring it in, by where its title bar or grow box left
it: mostly past its screen's edge, it goes out; mostly over the screen
shown, it comes in.  This is a `lispm-desktop-platinum-frame-moved-functions'
function, run before the window is kept in place."
  (when (and (lispm-desktop-coherence-available-p) (frame-parameter frame 'lispm-desktop-window))
    (let* ((screen (lispm-desktop-apps-window-screen frame))
           (inside (and (frame-live-p screen)
                        (lispm-desktop-coherence-inside-fraction
                         (lispm-desktop-coherence--outer-rectangle frame)
                         (lispm-desktop-coherence--screen-rectangle screen)))))
      (cond ((not inside))
            ((and (frame-parent frame) (< inside lispm-desktop-coherence-out-fraction))
             (lispm-desktop-coherence-take-out frame))
            ((and (not (frame-parent frame)) (not (lispm-desktop-apps-hidden-p screen))
                  (>= inside lispm-desktop-coherence-out-fraction))
             (lispm-desktop-coherence-bring-in frame))))))

;;;; Hiding the desktop

(defvar lispm-desktop-coherence--watch nil
  "Timer looking whether a hidden screen came back from the Dock.")

(defun lispm-desktop-coherence--hidden-screens ()
  "Return the screens hidden in the Dock."
  (seq-filter (lambda (frame) (and (not (frame-parent frame)) (frame-parameter frame 'lispm-desktop-hidden)))
              (frame-list)))

(defun lispm-desktop-coherence--watch ()
  "Show the desktop again when its screen came back from the Dock: its
tile was clicked, or Emacs showed it to read from its minibuffer."
  (let ((hidden (lispm-desktop-coherence--hidden-screens)))
    (dolist (screen hidden)
      (when (eq (frame-visible-p screen) t)
        (lispm-desktop-show-desktop screen)))
    (unless (lispm-desktop-coherence--hidden-screens)
      (when (timerp lispm-desktop-coherence--watch)
        (cancel-timer lispm-desktop-coherence--watch))
      (setq lispm-desktop-coherence--watch nil))))

(defun lispm-desktop-hide-desktop ()
  "Hide the desktop in the Dock, its windows out on the computer's screen.
The full-screen editor and browser float first, and go full screen again
when the desktop shows; so does the window in front keep the keyboard."
  (interactive)
  (unless (lispm-desktop-coherence-available-p)
    (user-error "Windows can go out on the screen only on macOS"))
  (let* ((screen (lispm-desktop-apps-screen))
         (front (lispm-desktop-apps-frontmost))
         (full (seq-filter
                #'identity
                (list (and (eq front 'editor) (not (lispm-desktop-editor-floating-p))
                           (progn (lispm-desktop-editor-float) 'editor))
                      (and (eq front 'browser) (not (lispm-desktop-browser-floating-p))
                           (progn (lispm-desktop-browser-float) 'browser)))))
         (selected (selected-frame)))
    (unless (lispm-desktop-apps-hidden-p screen)
      (pcase-let ((`(,left . ,top) (frame-position screen)))
        (set-frame-parameter screen 'lispm-desktop-hidden
                             (list :left left :top top :width (frame-text-width screen)
                                   :height (frame-text-height screen) :front front :full full
                                   :undecorated (frame-parameter screen 'undecorated))))
      (dolist (window (lispm-desktop-apps-windows screen))
        (when (and (frame-parent window) (frame-visible-p window))
          (lispm-desktop-coherence-take-out window)))
      (when (frame-parameter screen 'undecorated)
        (set-frame-parameter screen 'undecorated nil))
      (iconify-frame screen)
      (when-let* ((keyboard (if (lispm-desktop-apps-out-p selected)
                                selected
                              (seq-find (lambda (frame) (and (frame-visible-p frame)
                                                             (memq frame (lispm-desktop-apps-windows screen))))
                                        (frame-list-z-order)))))
        (select-frame-set-input-focus keyboard))
      (unless lispm-desktop-coherence--watch
        (setq lispm-desktop-coherence--watch (run-with-timer 0.5 0.5 #'lispm-desktop-coherence--watch))))))

(defun lispm-desktop-show-desktop (&optional screen)
  "Bring the desktop hidden in the Dock back, SCREEN or the selected one.
Its windows out over it go in again; those dropped elsewhere stay out.
The applications full screen before go full screen again, and the one in
front then comes in front."
  (interactive)
  (let ((screen (or screen (lispm-desktop-apps-screen))))
    (when-let* ((hidden (lispm-desktop-apps-hidden-p screen)))
      (set-frame-parameter screen 'lispm-desktop-hidden nil)
      (make-frame-visible screen)
      (when (plist-get hidden :undecorated)
        (set-frame-parameter screen 'undecorated t)
        (set-frame-position screen (plist-get hidden :left) (plist-get hidden :top))
        (set-frame-size screen (plist-get hidden :width) (plist-get hidden :height) t))
      (dolist (window (lispm-desktop-apps-windows screen))
        (when (and (not (frame-parent window))
                   (>= (lispm-desktop-coherence-inside-fraction
                        (lispm-desktop-coherence--outer-rectangle window)
                        (lispm-desktop-coherence--screen-rectangle screen))
                       lispm-desktop-coherence-out-fraction))
          (lispm-desktop-coherence-bring-in window)))
      (select-frame-set-input-focus screen)
      (let ((full (plist-get hidden :full)))
        (when (memq 'editor full) (lispm-desktop-editor-full-screen))
        (when (memq 'browser full) (lispm-desktop-browser-full-screen))
        (pcase (plist-get hidden :front)
          ('desktop (lispm-desktop-activate))
          ((and app (guard (not (memq app full)))) (lispm-desktop-apps-activate app)))))))

(defun lispm-desktop-toggle-desktop ()
  "Hide the desktop in the Dock, or show it again when it is hidden."
  (interactive)
  (if (lispm-desktop-apps-hidden-p)
      (lispm-desktop-show-desktop)
    (lispm-desktop-hide-desktop)))

;;;; Setup

(defun lispm-desktop-setup-coherence ()
  "Let the desktop's windows go out on the computer's screen."
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-coherence--dropped -50)
  (add-hook 'minibuffer-setup-hook #'lispm-desktop-coherence--place-minibuffer 90)
  (add-hook 'minibuffer-exit-hook #'lispm-desktop-coherence--hide-minibuffer)
  (add-hook 'set-message-functions #'lispm-desktop-coherence--echo)
  ;; Hide Desktop and Show Desktop, from the keyboard.
  (keymap-global-set "C-c h" #'lispm-desktop-toggle-desktop))

(provide 'lispm-desktop-coherence)
;;; lispm-desktop-coherence.el ends here
