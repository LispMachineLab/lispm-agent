;;; lispm-desktop-strip.el --- The Control Strip, as Mac OS 9's  -*- lexical-binding: t; -*-

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

;; Mac OS 9's Control Strip floats in the desktop's bottom-left corner,
;; over the windows: a collapse box, an arrow scrolling left, a row of
;; modules, an arrow scrolling right and a tab.  A module is a sixteen
;; pixel icon; pressing it darkens it and pops its menu up, in the small
;; system font, with its panel's quick choices and the panel itself:
;;
;;   Appearance  the theme
;;   Desktop     its colour, its picture and how it covers it, Arrange Icons
;;   Keyboard    Vim or Emacs keys
;;   Internet    the home page, the history and the bookmarks
;;   Battery     its charge and power source, where there is a battery
;;   Load        the load average and the processes
;;   Mail        read, compose and check mail
;;
;; The load, the mail and the battery were at the menu bar's right end;
;; as on Mac OS 9, the menu bar keeps the clock alone.
;;
;; The collapse box leaves only the tab, and the tab brings the strip back.
;; Dragging the tab sets how many modules show, the arrows scrolling to the
;; rest.  Dragging the tab with Command held moves the strip along the
;; desktop's edge, as Mac OS 9 moved it with Option (which lispm-desktop
;; leaves to macOS for typing): an outline follows the mouse, and the strip
;; moves once the mouse is let go, to the left or the right edge, whichever
;; half of the desktop the mouse is in.  On the right edge the tab is at the
;; strip's left end and the collapse box at its right, as Mac OS 9 drew it.
;; All of it is kept for the next start; C-c B hides the strip and shows it
;; again, as the Control Strip control panel's hot key did.
;;
;; From the keyboard, C-c b goes to the strip: C-f and C-b move between its
;; modules, Return or Space pops the module's menu up, which then works as
;; every menu does (C-f and C-b going on to the next module's), C-p and C-n
;; move the strip up and down, C-a and C-e to the left and the right edge,
;; and C-g or Escape leaves.  The strip is a child frame that takes no
;; focus, kept above the floating windows, as Mac OS 9 kept it above every
;; window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-settings)

(defvar lispm-desktop-color)
(defvar lispm-desktop-picture)
(defvar lispm-desktop-picture-position)
(defvar lispm-desktop-browser-home-url)
(defvar battery-echo-area-format)
(declare-function battery-format "battery" (format alist))
(declare-function lispm-desktop--color "lispm-desktop-surface" ())
(declare-function lispm-desktop-choose-picture "lispm-desktop-surface" (file))
(declare-function lispm-desktop-remove-picture "lispm-desktop-surface" ())
(declare-function lispm-desktop-arrange-icons "lispm-desktop-surface" ())
(declare-function lispm-desktop-browser-clear-history "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-edit-bookmarks "lispm-desktop-browser-places" ())

(defgroup lispm-desktop-strip nil
  "The Control Strip, as Mac OS 9's."
  :group 'lispm-desktop
  :prefix "lispm-desktop-strip-")

(defcustom lispm-desktop-strip-shown t
  "Non-nil shows the Control Strip along the desktop's edge.
C-c B (`lispm-desktop-strip-toggle') sets it, for the next start too."
  :type 'boolean)

(defcustom lispm-desktop-strip-edge 'left
  "The edge of the desktop the Control Strip lies along, `left' or `right'.
Dragging its tab with Command held sets it, for the next start too."
  :type '(choice (const left) (const right)))

(defcustom lispm-desktop-strip-raised 0
  "How far above the desktop's bottom the Control Strip lies, in pixels.
Dragging its tab with Command held sets it, for the next start too."
  :type 'natnum)

(defcustom lispm-desktop-strip-collapsed nil
  "Non-nil when the Control Strip is collapsed to its tab.
Its collapse box and its tab set it, for the next start too."
  :type 'boolean)

(defcustom lispm-desktop-strip-width nil
  "How many modules the Control Strip shows at once, or nil for all.
Dragging its tab sets it, for the next start too."
  :type '(choice (const :tag "All" nil) natnum))

(defcustom lispm-desktop-strip-modules '(appearance desktop keyboard internet battery load mail)
  "The Control Strip's modules, left to right: keys of
`lispm-desktop-strip-module-alist'."
  :type '(repeat symbol))

;;;; Modules

(defun lispm-desktop-strip--panel-item (panel)
  "Return the menu item opening the control panel PANEL, as \"Open PANEL…\"."
  (list :label (format "Open %s…" (nth 1 (assq panel lispm-desktop-settings-panels)))
        :action (lambda () (lispm-desktop-settings-show panel))))

(defun lispm-desktop-strip--appearance-items ()
  "Return the Appearance module's menu: the themes, and the panel."
  `(,@(mapcar (lambda (theme)
                (list :label (capitalize (symbol-name theme))
                      :checked (eq lispm-desktop-theme theme)
                      :action (lambda () (lispm-desktop-set-theme theme))))
              (delete-dups (delq nil (list 'dracula 'platinum lispm-desktop-theme))))
    (:label "Other Theme…" :action lispm-desktop-settings--other-theme)
    separator
    ,(lispm-desktop-strip--panel-item 'themes)))

(defun lispm-desktop-strip--desktop-items ()
  "Return the Desktop module's menu: the colour, the picture and how it
covers the desktop, Arrange Icons, and the panel."
  `((:label "Colour from the Theme" :checked ,(null lispm-desktop-color)
            :action ,(lambda () (lispm-desktop-settings--save 'lispm-desktop-color nil)))
    (:label "Your Own Colour…" :checked ,(and lispm-desktop-color t)
            :action lispm-desktop-settings--choose-color)
    separator
    (:label "Choose Picture…" :action ,(lambda () (call-interactively #'lispm-desktop-choose-picture)))
    (:label "No Picture" :enabled ,(and lispm-desktop-picture t) :action lispm-desktop-remove-picture)
    separator
    ,@(mapcar (pcase-lambda (`(,position . ,label))
                (list :label label :checked (eq lispm-desktop-picture-position position)
                      :action (lambda ()
                                (lispm-desktop-settings--save 'lispm-desktop-picture-position position))))
              '((fill . "Fill") (fit . "Fit") (center . "Center") (tile . "Tile")))
    separator
    (:label "Arrange Icons" :action lispm-desktop-arrange-icons)
    separator
    ,(lispm-desktop-strip--panel-item 'desktop)))

(defun lispm-desktop-strip--keyboard-items ()
  "Return the Keyboard module's menu: Vim or Emacs keys, and the panel."
  `(,@(mapcar (pcase-lambda (`(,style . ,label))
                (list :label label :checked (eq lispm-desktop-editing-style style)
                      :action (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style style))))
              '((vim . "Vim Keys") (emacs . "Emacs Keys")))
    separator
    ,(lispm-desktop-strip--panel-item 'keyboard)))

(defun lispm-desktop-strip--internet-items ()
  "Return the Internet module's menu: the home page, the history, the
bookmarks, and the panel."
  `((:label "Change Home Page…"
            :action ,(lambda () (lispm-desktop-settings--change 'lispm-desktop-browser-home-url "Home page: ")))
    (:label "Clear History…" :action lispm-desktop-browser-clear-history)
    (:label "Edit Bookmarks…" :action lispm-desktop-browser-edit-bookmarks)
    separator
    ,(lispm-desktop-strip--panel-item 'browser)))

(defun lispm-desktop-strip--battery-icon ()
  "Return the battery module's icon: the battery, filled to its charge."
  (lispm-desktop-platinum-draw-battery lispm-desktop-platinum--palette
                                       (lispm-desktop-platinum-battery-percent)
                                       (lispm-desktop-platinum-battery-charging-p)))

(defun lispm-desktop-strip--battery-help ()
  "Return what the battery module says of itself."
  (battery-format battery-echo-area-format lispm-desktop-platinum--battery))

(defun lispm-desktop-strip--load-icon ()
  "Return the load module's icon: the meter, filled to the load."
  (lispm-desktop-platinum-draw-meter lispm-desktop-platinum--palette
                                     (lispm-desktop-platinum-load-fraction)))

(defun lispm-desktop-strip--time-p ()
  "Return non-nil when Emacs keeps the load and the mail, with the time."
  (bound-and-true-p display-time-mode))

(defvar lispm-desktop-strip-module-alist
  `((appearance :icon "appearance" :help "Appearance" :items lispm-desktop-strip--appearance-items)
    (desktop :icon "desktop-picture" :help "Desktop" :items lispm-desktop-strip--desktop-items)
    (keyboard :icon "keyboard" :help "Keyboard" :items lispm-desktop-strip--keyboard-items)
    (internet :icon "globe" :help "Internet" :items lispm-desktop-strip--internet-items
              :visible lispm-desktop-settings--browser-p)
    (battery :icon lispm-desktop-strip--battery-icon :help lispm-desktop-strip--battery-help
             :items lispm-desktop-platinum-battery-items :visible lispm-desktop-platinum-battery-percent)
    (load :icon lispm-desktop-strip--load-icon
          :help ,(lambda () (format "Load average %s" lispm-desktop-platinum--load))
          :items lispm-desktop-platinum-load-items :visible lispm-desktop-strip--time-p)
    (mail :icon ,(lambda () (if lispm-desktop-platinum--mail "mail-new" "mail"))
          :help ,(lambda () (if lispm-desktop-platinum--mail "You have new mail" "Mail"))
          :items lispm-desktop-platinum-mail-items :visible lispm-desktop-strip--time-p))
  "The Control Strip's modules: (KEY :icon ICON :help HELP :items ITEMS
[:visible VISIBLE]).  ICON names an icon of icons/os9, or is a function
returning such a name or an SVG drawing sixteen pixels square; HELP is
the module's Balloon Help, a string or a function returning one; ITEMS
returns its menu, as `lispm-desktop-popup-menu' takes it; VISIBLE, when
given, decides whether it shows.")

(defun lispm-desktop-strip--value (value)
  "Return VALUE, or what it returns when it is a function."
  (if (functionp value) (funcall value) value))

(defun lispm-desktop-strip--modules ()
  "Return the keys of the modules to show, left to right."
  (seq-filter (lambda (key)
                (when-let* ((module (alist-get key lispm-desktop-strip-module-alist)))
                  (or (not (plist-member module :visible))
                      (funcall (plist-get module :visible)))))
              lispm-desktop-strip-modules))

(defun lispm-desktop-strip--icon (key)
  "Return the icon of module KEY: an SVG file, or an SVG drawing."
  (let ((icon (lispm-desktop-strip--value
               (plist-get (alist-get key lispm-desktop-strip-module-alist) :icon))))
    (if (stringp icon) (lispm-desktop-platinum-icon-file icon) icon)))

(defun lispm-desktop-strip--items (key)
  "Return the menu of module KEY."
  (funcall (plist-get (alist-get key lispm-desktop-strip-module-alist) :items)))

;;;; Layout

(defvar lispm-desktop-strip--frame nil
  "The Control Strip's frame, while it is shown.")

(defvar lispm-desktop-strip--first 0
  "The index of the first module shown, as the arrows scrolled them.")

(defvar lispm-desktop-strip--pressed nil
  "The module whose menu is up, drawn pressed.")

(defvar lispm-desktop-strip--focus nil
  "The module the keyboard is on, ringed, or nil.")

(defconst lispm-desktop-strip-buffer-name " *Control Strip*"
  "Name of the buffer the Control Strip is drawn in.")

(defun lispm-desktop-strip--shown (count)
  "Return how many of COUNT modules show at once."
  (max (min 1 count) (min count (or lispm-desktop-strip-width count))))

(defun lispm-desktop-strip--side ()
  "Return the edge the strip lies along, `left' or `right'."
  (if (eq lispm-desktop-strip-edge 'right) 'right 'left))

(defun lispm-desktop-strip--parts ()
  "Return the strip's parts, left to right: `collapse', `left', a
\(module . KEY) each for the modules shown, `right' and `tab'; on the
right edge the tab first and the collapse box last, as Mac OS 9 had them;
or the tab alone while the strip is collapsed."
  (if lispm-desktop-strip-collapsed
      '(tab)
    (let* ((modules (lispm-desktop-strip--modules))
           (shown (lispm-desktop-strip--shown (length modules)))
           (right (eq (lispm-desktop-strip--side) 'right)))
      (setq lispm-desktop-strip--first
            (max 0 (min lispm-desktop-strip--first (- (length modules) shown))))
      `(,(if right 'tab 'collapse)
        left
        ,@(mapcar (lambda (key) (cons 'module key))
                  (seq-subseq modules lispm-desktop-strip--first
                              (+ lispm-desktop-strip--first shown)))
        right
        ,(if right 'collapse 'tab)))))

(defun lispm-desktop-strip--kind (part)
  "Return the kind of segment PART is drawn as."
  (if (consp part) 'module part))

(defun lispm-desktop-strip--x (part)
  "Return how far into the strip PART starts, in pixels, or nil."
  (let ((x 0))
    (catch 'found
      (dolist (each (lispm-desktop-strip--parts))
        (when (equal each part) (throw 'found x))
        (setq x (+ x (lispm-desktop-platinum-strip-segment-width (lispm-desktop-strip--kind each))))))))

(defun lispm-desktop-strip--state (part)
  "Return the state PART is drawn in: `pressed', `disabled' or nil."
  (let* ((modules (length (lispm-desktop-strip--modules)))
         (shown (lispm-desktop-strip--shown modules)))
    (pcase part
      (`(module . ,key) (and (eq key lispm-desktop-strip--pressed) 'pressed))
      ('left (and (<= lispm-desktop-strip--first 0) 'disabled))
      ('right (and (>= (+ lispm-desktop-strip--first shown) modules) 'disabled)))))

(defun lispm-desktop-strip--help (part)
  "Return the Balloon Help of PART."
  (pcase part
    (`(module . ,key)
     (lispm-desktop-strip--value (plist-get (alist-get key lispm-desktop-strip-module-alist) :help)))
    ('collapse "Collapse the Control Strip to its tab")
    ('left "Scroll the modules to the left")
    ('right "Scroll the modules to the right")
    ('tab (if lispm-desktop-strip-collapsed
              "Show the Control Strip; Command-drag to move it"
            "Drag to show more or fewer modules, Command-drag to move the strip; click to collapse"))))

(defun lispm-desktop-strip--segment (part)
  "Return the image of PART, memoized."
  (let* ((kind (lispm-desktop-strip--kind part))
         (state (lispm-desktop-strip--state part))
         (icon (and (consp part) (lispm-desktop-strip--icon (cdr part))))
         (focused (and (consp part) (eq (cdr part) lispm-desktop-strip--focus)))
         ;; The strip lies on the desktop: no frame shows what is behind
         ;; it, so the tab's end is set off against the desktop's colour.
         (behind (and (eq kind 'tab) (fboundp 'lispm-desktop--color) (lispm-desktop--color)))
         (side (lispm-desktop-strip--side)))
    (lispm-desktop-platinum--image
     (list 'strip kind state icon focused behind side)
     (lambda ()
       (lispm-desktop-platinum-draw-strip-segment lispm-desktop-platinum--palette kind
                                                  :state state :icon icon :menu (consp part)
                                                  :focused focused :behind behind :side side)))))

(defvar-keymap lispm-desktop-strip-part-map
  :doc "Keymap of the Control Strip's parts.
Command is Meta in lispm-desktop, and the strip moves by a drag with it;
one with Option, Alt or Super moves it too, where a key gives those."
  "<down-mouse-1>" #'lispm-desktop-strip-press
  "<mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'ignore
  "M-<down-mouse-1>" #'lispm-desktop-strip-move-press
  "M-<mouse-1>" #'ignore
  "M-<drag-mouse-1>" #'ignore
  "A-<down-mouse-1>" #'lispm-desktop-strip-move-press
  "A-<mouse-1>" #'ignore
  "A-<drag-mouse-1>" #'ignore
  "s-<down-mouse-1>" #'lispm-desktop-strip-move-press
  "s-<mouse-1>" #'ignore
  "s-<drag-mouse-1>" #'ignore)

(defun lispm-desktop-strip--line ()
  "Return the strip's line: its parts' images, each knowing its part."
  (mapconcat (lambda (part)
               (propertize " " 'display (lispm-desktop-strip--segment part)
                           'lispm-desktop-strip-part part
                           'keymap lispm-desktop-strip-part-map
                           'help-echo (lispm-desktop-strip--help part)
                           'pointer 'arrow))
             (lispm-desktop-strip--parts) ""))

(defun lispm-desktop-strip--width ()
  "Return the strip's width in pixels."
  (apply #'+ (mapcar (lambda (part)
                       (lispm-desktop-platinum-strip-segment-width (lispm-desktop-strip--kind part)))
                     (lispm-desktop-strip--parts))))

;;;; The strip's frame

(defun lispm-desktop-strip--live-p ()
  "Return non-nil while the Control Strip is shown."
  (frame-live-p lispm-desktop-strip--frame))

(defun lispm-desktop-strip--buffer ()
  "Return the buffer the Control Strip is drawn in, made when missing."
  (or (get-buffer lispm-desktop-strip-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-strip-buffer-name)
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil
                    cursor-type nil
                    cursor-in-non-selected-windows nil
                    display-line-numbers nil
                    auto-hscroll-mode nil
                    truncate-lines t
                    line-spacing 0
                    buffer-read-only t)
        ;; Its frame keeps the strip's size as the font changes.
        (lispm-desktop-popup--hold-size)
        ;; The line is as tall as the strip's images, whatever the font.
        (face-remap-add-relative 'default :height 10)
        (current-buffer))))

(defun lispm-desktop-strip--make-frame (screen)
  "Return the Control Strip's frame over SCREEN, not yet shown."
  (let ((frame (let ((after-make-frame-functions nil)
                     (before-make-frame-hook nil))
                 (make-frame
                  `((parent-frame . ,screen) (name . "Control Strip")
                    (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                    (undecorated . t) (unsplittable . t) (no-other-frame . t)
                    (skip-taskbar . t) (desktop-dont-save . t)
                    (child-frame-border-width . 0) (internal-border-width . 0)
                    (left-fringe . 0) (right-fringe . 0)
                    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                    (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                    (visibility . nil)
                    (background-color . ,(lispm-desktop-platinum--color :strip))
                    (width . 20) (height . 1))))))
    (let ((root (frame-root-window frame)))
      (set-window-buffer root (lispm-desktop-strip--buffer))
      (set-window-dedicated-p root t)
      (set-window-fringes root 0 0))
    frame))

(defun lispm-desktop-strip--bounds (screen)
  "Return where the strip may lie on SCREEN, (LEFT TOP RIGHT BOTTOM) in
pixels: under the menu bar, down to the bottom over the echo area."
  ;; The root window of a screen split in windows is not live, and only a
  ;; live window has a divider: the bottom windows' is the one.
  (pcase-let ((`(,left ,top ,right ,bottom) (window-pixel-edges (frame-root-window screen))))
    (list left top right
          (- bottom (window-bottom-divider-width (car (window-at-side-list screen 'bottom)))))))

(defun lispm-desktop-strip--position (bounds width side raised)
  "Return where a strip WIDTH pixels wide lies within BOUNDS, as
`lispm-desktop-strip--bounds' gives them, on edge SIDE and RAISED pixels
above the bottom: (X . Y), kept within the bounds."
  (pcase-let ((`(,left ,top ,right ,bottom) bounds))
    (cons (if (eq side 'right) (- right width) left)
          (max top (- bottom lispm-desktop-platinum-strip-height (max 0 raised))))))

(defun lispm-desktop-strip--place ()
  "Put the strip on its screen's edge, as high as it was put."
  (when (lispm-desktop-strip--live-p)
    (pcase-let ((`(,x . ,y) (lispm-desktop-strip--position
                             (lispm-desktop-strip--bounds (frame-parent lispm-desktop-strip--frame))
                             (lispm-desktop-strip--width) (lispm-desktop-strip--side)
                             lispm-desktop-strip-raised)))
      (set-frame-position lispm-desktop-strip--frame x y))))

(defun lispm-desktop-strip--render ()
  "Draw the strip again, sized to its parts and placed in its corner."
  (when (lispm-desktop-strip--live-p)
    (let ((window (frame-root-window lispm-desktop-strip--frame)))
      (with-current-buffer (lispm-desktop-strip--buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (lispm-desktop-strip--line))))
      (set-window-start window 1)
      (set-window-hscroll window 0))
    ;; Only a new colour is set: NS Emacs clears a frame whenever its
    ;; background is set, to the same colour too, and a strip cleared
    ;; just before an update of the screen shows as an empty bar.
    (let ((color (lispm-desktop-platinum--color :strip)))
      (unless (equal (frame-parameter lispm-desktop-strip--frame 'background-color) color)
        (set-frame-parameter lispm-desktop-strip--frame 'background-color color)))
    (lispm-desktop-popup--size lispm-desktop-strip--frame (lispm-desktop-strip--width)
                               lispm-desktop-platinum-strip-height)
    (lispm-desktop-strip--place)))

(defvar lispm-desktop-strip--raising nil
  "Non-nil while the strip is being raised, so raising it raises nothing more.")

(defun lispm-desktop-strip--popup-up-p ()
  "Return non-nil while a menu or an alert is up, which stays over the strip."
  (seq-some (lambda (frame)
              (and (frame-visible-p frame)
                   (string-prefix-p " *lispm-desktop-popup*"
                                    (buffer-name (window-buffer (frame-root-window frame))))))
            (frame-list)))

(defun lispm-desktop-strip--keep-above ()
  "Raise the strip over the floating windows, though not over a menu."
  (when (and (lispm-desktop-strip--live-p) (not lispm-desktop-strip--raising)
             (frame-visible-p lispm-desktop-strip--frame)
             (not (lispm-desktop-strip--popup-up-p)))
    (let ((lispm-desktop-strip--raising t))
      (raise-frame lispm-desktop-strip--frame))))

(defun lispm-desktop-strip--selected (&rest _)
  "Keep the strip above the floating window just selected, which macOS
brings to the front.  This is a `window-selection-change-functions'
function."
  (when (frame-parameter (selected-frame) 'lispm-desktop-window)
    (lispm-desktop-strip--keep-above)))

(defun lispm-desktop-strip--follow (frame &rest _)
  "Keep the strip above FRAME, just raised or shown, when FRAME is a
floating window.  This is advice after `raise-frame' and
`make-frame-visible'."
  (when (and (framep frame) (frame-parameter frame 'lispm-desktop-window))
    (lispm-desktop-strip--keep-above)))

(defun lispm-desktop-strip--screen-changed (frame)
  "Put the strip back in its corner once its screen FRAME changed size.
This is a `window-size-change-functions' function."
  (when (and (lispm-desktop-strip--live-p) (eq frame (frame-parent lispm-desktop-strip--frame)))
    (lispm-desktop-strip--place)))

(defun lispm-desktop-strip-show ()
  "Show the Control Strip in the desktop's bottom-left corner."
  (interactive)
  (when (display-graphic-p)
    (unless (lispm-desktop-strip--live-p)
      (setq lispm-desktop-strip--frame (lispm-desktop-strip--make-frame (lispm-desktop-apps-screen))))
    (lispm-desktop-strip--render)
    (make-frame-visible lispm-desktop-strip--frame)
    (lispm-desktop-strip--keep-above)))

(defun lispm-desktop-strip-hide ()
  "Take the Control Strip away."
  (interactive)
  (when (lispm-desktop-strip--live-p)
    (delete-frame lispm-desktop-strip--frame))
  (setq lispm-desktop-strip--frame nil))

(defun lispm-desktop-strip-toggle ()
  "Hide the Control Strip, or show it again, for the next start too, as
the hot key of Mac OS 9's Control Strip control panel did."
  (interactive)
  (let ((shown (not (lispm-desktop-strip--live-p))))
    (lispm-desktop-settings--save 'lispm-desktop-strip-shown shown)
    (if shown (lispm-desktop-strip-show) (lispm-desktop-strip-hide))))

(defun lispm-desktop-strip-refresh (&rest _)
  "Draw the strip again, as its modules' state or the theme changed."
  (lispm-desktop-strip--render))

(defun lispm-desktop-strip--desktop-changed (&rest _)
  "Draw the strip again once the desktop's colour changed, which the tab's
end is set off against.  A variable watcher runs before the change, so
the strip is drawn right after."
  (run-at-time 0 nil #'lispm-desktop-strip-refresh))

;;;; Using the strip

(defun lispm-desktop-strip--save (variable value)
  "Set VARIABLE to VALUE, for the next start too, and draw the strip again."
  (lispm-desktop-settings--save variable value)
  (lispm-desktop-strip--render))

(defun lispm-desktop-strip-collapse (&optional expand)
  "Collapse the Control Strip to its tab, or EXPAND it again."
  (interactive "P")
  (lispm-desktop-strip--save 'lispm-desktop-strip-collapsed (not expand)))

(defun lispm-desktop-strip-scroll (step)
  "Scroll the modules STEP places, to the right when STEP is positive."
  (setq lispm-desktop-strip--first (max 0 (+ lispm-desktop-strip--first step)))
  (lispm-desktop-strip--render))

(defun lispm-desktop-strip--menu-place (key items)
  "Return where the menu ITEMS of module KEY opens, as (X . Y) in the
strip's screen: its outline on the module's left edge, its bottom two
pixels into the strip, as Mac OS 9's module menus popped up."
  (pcase-let* ((`(,left . ,top) (frame-position lispm-desktop-strip--frame))
               (height (let ((lispm-desktop-popup-small t))
                         (cdr (lispm-desktop-popup--rows items)))))
    (cons (+ left (lispm-desktop-strip--x (cons 'module key)) -1)
          (+ top 1 (- height)))))

(defun lispm-desktop-strip--open (key)
  "Pop the menu of module KEY up from it, pressed meanwhile, and run the
item chosen once the module is let go; return the item, or nil."
  (let ((items (lispm-desktop-strip--items key))
        (chosen nil))
    (if (not (lispm-desktop-popup-available-p))
        (lispm-desktop-popup-menu items)
      (unwind-protect
          (progn
            (setq lispm-desktop-strip--pressed key)
            (lispm-desktop-strip--render)
            (redisplay t)
            (pcase-let ((`(,x . ,y) (lispm-desktop-strip--menu-place key items)))
              (let ((lispm-desktop-popup-small t)
                    (lispm-desktop-popup-parent (frame-parent lispm-desktop-strip--frame)))
                (setq chosen (lispm-desktop-popup-choose items x y)))))
        (setq lispm-desktop-strip--pressed nil)
        (lispm-desktop-strip--render))
      (lispm-desktop-popup--run chosen))))

(defun lispm-desktop-strip--drag-tab ()
  "Follow the mouse dragging the tab: the modules shown follow it, the tab
going away from the strip's edge showing more.  A click that does not
drag collapses the strip."
  (let* ((start (car (mouse-absolute-pixel-position)))
         (count (length (lispm-desktop-strip--modules)))
         (from (lispm-desktop-strip--shown count))
         (step (lispm-desktop-platinum-strip-segment-width 'module))
         (away (if (eq (lispm-desktop-strip--side) 'right) -1 1))
         (moved nil))
    (track-mouse
      (while (mouse-movement-p (let ((echo-keystrokes 0)) (read-event)))
        (let ((dx (* away (- (car (mouse-absolute-pixel-position)) start))))
          (when (> (abs dx) 3) (setq moved t))
          (let ((shown (max 1 (min count (+ from (round dx step))))))
            (unless (= shown (lispm-desktop-strip--shown count))
              (setq lispm-desktop-strip-width shown)
              (lispm-desktop-strip--render)
              (redisplay t))))))
    (if moved
        (lispm-desktop-strip--save 'lispm-desktop-strip-width
                                   (and (< (lispm-desktop-strip--shown count) count)
                                        (lispm-desktop-strip--shown count)))
      (lispm-desktop-strip-collapse))))

(defun lispm-desktop-strip--moved (bounds start pointer)
  "Return where the strip moves within BOUNDS, as `lispm-desktop-strip--bounds'
gives them, dragged from its top START with the mouse now at POINTER,
\(X . DY) in the screen's pixels: X where the mouse is, DY how far it
went down.  The value is (SIDE . RAISED): the edge whose half of the
screen the mouse is in, and how far above the bottom the strip then lies,
kept under the menu bar and above the bottom."
  (pcase-let* ((`(,left ,top ,right ,bottom) bounds)
               (side (if (>= (car pointer) (/ (+ left right) 2)) 'right 'left))
               (y (max top (min (- bottom lispm-desktop-platinum-strip-height)
                                (+ start (cdr pointer))))))
    (cons side (- bottom lispm-desktop-platinum-strip-height y))))

(defun lispm-desktop-strip--move-to (side raised)
  "Put the strip on edge SIDE, RAISED pixels above the bottom, for the next
start too."
  (lispm-desktop-settings--save 'lispm-desktop-strip-edge side)
  (lispm-desktop-settings--save 'lispm-desktop-strip-raised (max 0 raised))
  (lispm-desktop-strip--render))

(defun lispm-desktop-strip--drag-move ()
  "Move the strip by an outline the mouse drags, as Mac OS 9 moved it by
an Option-drag of its tab: the outline follows the mouse up and down the
edge and over to the other edge past the screen's middle, and the strip
moves once the mouse is let go.  Escape or C-g leaves it where it was.
Return non-nil when it moved."
  (let* ((frame lispm-desktop-strip--frame)
         (screen (frame-parent frame))
         (origin (lispm-desktop-platinum--origin screen))
         (bounds (lispm-desktop-strip--bounds screen))
         (width (frame-pixel-width frame))
         (start (cdr (frame-position frame)))
         (ink (lispm-desktop-platinum-inverse (lispm-desktop-platinum--desktop-color)))
         (moved (lambda (delta)
                  (lispm-desktop-strip--moved
                   bounds start
                   (cons (- (car (mouse-absolute-pixel-position)) (car origin)) (cdr delta)))))
         (result
          (unwind-protect
              (lispm-desktop-platinum--track
               (lambda (delta)
                 (pcase-let* ((`(,side . ,raised) (funcall moved delta))
                              (`(,x . ,y) (lispm-desktop-strip--position bounds width side raised)))
                   (lispm-desktop-platinum--show-outline
                    screen (lispm-desktop-platinum-strip-outline-lines x y width side ink) origin))))
            (lispm-desktop-platinum--hide-outline))))
    (when (and (car result) (lispm-desktop-platinum--moved-p (cdr result)))
      (pcase-let ((`(,side . ,raised) (funcall moved (cdr result))))
        (lispm-desktop-strip--move-to side raised))
      t)))

(defun lispm-desktop-strip-move-press (event)
  "Move the Control Strip when EVENT pressed its tab with Command held,
see `lispm-desktop-strip--drag-move'; elsewhere act as a press.  A press
let go where it was does nothing."
  (interactive "e")
  (let* ((position (event-start event))
         (part (and (windowp (posn-window position)) (posn-point position)
                    (get-text-property (posn-point position) 'lispm-desktop-strip-part
                                       (window-buffer (posn-window position))))))
    (if (eq part 'tab)
        (lispm-desktop-strip--drag-move)
      (lispm-desktop-strip-press event))))

(defconst lispm-desktop-strip-step lispm-desktop-platinum-strip-height
  "How far C-p and C-n move the strip, in pixels: its own height.")

(defun lispm-desktop-strip--nudge (step)
  "Move the strip STEP pixels up the edge, down when STEP is negative."
  (when (lispm-desktop-strip--live-p)
    (pcase-let* ((screen (frame-parent lispm-desktop-strip--frame))
                 (`(,_ ,top ,_ ,bottom) (lispm-desktop-strip--bounds screen)))
      (lispm-desktop-strip--move-to
       (lispm-desktop-strip--side)
       (max 0 (min (- bottom top lispm-desktop-platinum-strip-height)
                   (+ lispm-desktop-strip-raised step)))))))

(defun lispm-desktop-strip-press (event)
  "Act on the part of the Control Strip EVENT pressed."
  (interactive "e")
  (let* ((position (event-start event))
         (part (and (windowp (posn-window position)) (posn-point position)
                    (get-text-property (posn-point position) 'lispm-desktop-strip-part
                                       (window-buffer (posn-window position))))))
    (pcase part
      (`(module . ,key) (lispm-desktop-strip--open key))
      ('collapse (lispm-desktop-strip-collapse))
      ('left (lispm-desktop-strip-scroll -1))
      ('right (lispm-desktop-strip-scroll 1))
      ('tab (if lispm-desktop-strip-collapsed
                (lispm-desktop-strip-collapse t)
              (lispm-desktop-strip--drag-tab))))))

(defun lispm-desktop-strip--focus-on (key)
  "Ring module KEY as the keyboard's, scrolled into view."
  (let* ((modules (lispm-desktop-strip--modules))
         (index (seq-position modules key))
         (shown (lispm-desktop-strip--shown (length modules))))
    (setq lispm-desktop-strip--focus key)
    (cond ((< index lispm-desktop-strip--first)
           (setq lispm-desktop-strip--first index))
          ((>= index (+ lispm-desktop-strip--first shown))
           (setq lispm-desktop-strip--first (1+ (- index shown)))))
    (lispm-desktop-strip--render)
    (redisplay t)))

(defun lispm-desktop-strip--walk (modules index)
  "Pop up the menu of the module at INDEX of MODULES, Left and Right in it
going on to the menus beside, as along the menu bar.  Return (CHOSEN .
INDEX): the item chosen, or nil, and the module whose menu closed."
  (let (step chosen)
    (while (setq step (catch 'lispm-desktop-popup-menu-bar-step
                        (let ((lispm-desktop-popup-menu-bar-step t))
                          (lispm-desktop-strip--focus-on (nth index modules))
                          (setq chosen (lispm-desktop-strip--open (nth index modules))))
                        nil))
      (setq index (mod (+ index step) (length modules))))
    (cons chosen index)))

(defun lispm-desktop-strip-select ()
  "Go to the Control Strip from the keyboard.
The first module is ringed: C-f and C-b move between the modules, Return
or Space pops the ringed one's menu up, C-p and C-n move the strip up and
down its edge, C-a and C-e to the left and the right edge, and C-g or
Escape leaves.  A collapsed strip shows whole meanwhile.  Any other key
leaves the strip and does what it does."
  (interactive)
  (unless (lispm-desktop-strip--live-p)
    (lispm-desktop-strip-show))
  (let ((collapsed lispm-desktop-strip-collapsed)
        (modules (lispm-desktop-strip--modules))
        (index 0))
    (when (and modules (lispm-desktop-strip--live-p))
      (unwind-protect
          (progn
            (setq lispm-desktop-strip-collapsed nil)
            (catch 'done
              (while t
                (lispm-desktop-strip--focus-on (nth index modules))
                (let ((event (lispm-desktop-popup--read)))
                  (cond ((lispm-desktop-popup--cancel-p event) (throw 'done nil))
                        ((memq event '(right ?\C-f))
                         (setq index (mod (1+ index) (length modules))))
                        ((memq event '(left ?\C-b))
                         (setq index (mod (1- index) (length modules))))
                        ((memq event '(up ?\C-p)) (lispm-desktop-strip--nudge lispm-desktop-strip-step))
                        ((memq event '(down ?\C-n)) (lispm-desktop-strip--nudge (- lispm-desktop-strip-step)))
                        ((eq event ?\C-a)
                         (lispm-desktop-strip--move-to 'left lispm-desktop-strip-raised))
                        ((eq event ?\C-e)
                         (lispm-desktop-strip--move-to 'right lispm-desktop-strip-raised))
                        ((or (lispm-desktop-popup--confirm-p event) (eq event ?\s))
                         (pcase-let ((`(,chosen . ,at) (lispm-desktop-strip--walk modules index)))
                           (setq index at)
                           (when chosen (throw 'done nil))))
                        (t (push event unread-command-events)
                           (throw 'done nil)))))))
        (setq lispm-desktop-strip--focus nil
              lispm-desktop-strip-collapsed collapsed)
        (lispm-desktop-strip--render)))))

;;;; Setup

(defun lispm-desktop-setup-strip ()
  "Show the Control Strip once the first frame is up, keep it above the
floating windows and on its edge, go to it with C-c b, and hide it and
show it again with C-c B, C-c keys, which terminals leave to Emacs too."
  (dolist (function '(raise-frame make-frame-visible))
    (advice-add function :after #'lispm-desktop-strip--follow))
  (add-hook 'window-selection-change-functions #'lispm-desktop-strip--selected)
  (add-hook 'window-size-change-functions #'lispm-desktop-strip--screen-changed)
  (add-hook 'enable-theme-functions #'lispm-desktop-strip-refresh 95)
  (add-hook 'display-time-hook #'lispm-desktop-strip-refresh 90)
  (add-hook 'battery-update-functions #'lispm-desktop-strip-refresh 90)
  (add-variable-watcher 'lispm-desktop-color #'lispm-desktop-strip--desktop-changed)
  (keymap-global-set "C-c b" #'lispm-desktop-strip-select)
  (keymap-global-set "C-c B" #'lispm-desktop-strip-toggle)
  (when lispm-desktop-strip-shown
    (add-hook 'window-setup-hook #'lispm-desktop-strip-show 95)))

(provide 'lispm-desktop-strip)
;;; lispm-desktop-strip.el ends here
