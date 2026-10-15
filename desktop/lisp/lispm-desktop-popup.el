;;; lispm-desktop-popup.el --- Mac OS 9's menus and alerts  -*- lexical-binding: t; -*-

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

;; Mac OS 9 drew its own menus and alerts, and so does lispm-desktop, each
;; in a child frame over the frame it belongs to:
;;
;;   menu    a platinum panel dropped from its menu bar title: items in
;;           the system face, the one under the mouse in the accent
;;           colour, check marks, separators, key equivalents on the right
;;   alert   a panel over the frame's upper middle: the caution, stop or
;;           note icon, the message, and push buttons at the bottom right,
;;           the default one ringed
;;
;; Both are modal, as Mac OS 9's were: Emacs reads the mouse and the keys
;; itself until a choice is made.  In a menu the arrows move and Return
;; chooses; in an alert Return presses the default button; Escape, C-g and
;; Command-period cancel either.  Where no child frame can be shown (a
;; text terminal, batch), menus fall back to `completing-read' and alerts
;; to `read-multiple-choice'.
;;
;; Drawing (the text of menus and alerts; push buttons are
;; `lispm-desktop-platinum-draw-push-button') is separate from the modal
;; loops, which only read events and redraw.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)

(declare-function lispm-desktop-tab-bar--blend "lispm-desktop-interface" (from to ratio))

;;;; Popup frames

(defun lispm-desktop-popup-available-p ()
  "Return non-nil when popups can be drawn: a graphical, interactive Emacs."
  (and (display-graphic-p) (not noninteractive) lispm-desktop-platinum--palette))

(defun lispm-desktop-popup--frame (parent)
  "Return a hidden child frame of PARENT with a buffer of its own.
It takes no focus, and hooks run for new frames do not see it."
  (let* ((buffer (generate-new-buffer " *lispm-desktop-popup*"))
         (frame (let ((after-make-frame-functions nil)
                      (before-make-frame-hook nil))
                  (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                     (undecorated . t) (unsplittable . t) (no-other-frame . t)
                     (child-frame-border-width . 1) (internal-border-width . 0)
                     (left-fringe . 0) (right-fringe . 0)
                     (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                     (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                     (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                     (desktop-dont-save . t) (visibility . nil)
                     (background-color . ,(lispm-desktop-platinum--color :chrome))
                     (foreground-color . ,(lispm-desktop-platinum--color :text))
                     (width . 20) (height . 2))))))
    (with-current-buffer buffer
      (setq-local mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  display-line-numbers nil
                  auto-hscroll-mode nil
                  truncate-lines t
                  line-spacing 0))
    (let ((root (frame-root-window frame)))
      (set-window-buffer root buffer)
      (set-window-dedicated-p root t)
      (set-window-fringes root 0 0))
    frame))

(defun lispm-desktop-popup--dispose (frame)
  "Delete popup FRAME, its shadow and its buffer."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame)))
          (shadow (frame-parameter frame 'lispm-desktop-popup-shadow)))
      (when (frame-live-p shadow)
        (delete-frame shadow t))
      (delete-frame frame t)
      (kill-buffer buffer))))

(defun lispm-desktop-popup--shadow (frame kind)
  "Show the shadow of popup FRAME, a frame of its size a pixel down and right.
KIND is `menu', whose shadow is a dark grey line along its right and
bottom, or `alert', whose shadow is black.  The shadow is shown before
FRAME, so FRAME stands over it."
  (let* ((parent (frame-parent frame))
         (color (if (eq kind 'alert)
                    (lispm-desktop-platinum--color :outline)
                  (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #x22)))
         (shadow (let ((after-make-frame-functions nil)
                       (before-make-frame-hook nil))
                   (make-frame
                    `((parent-frame . ,parent)
                      (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                      (undecorated . t) (unsplittable . t) (no-other-frame . t)
                      (child-frame-border-width . 0) (internal-border-width . 0)
                      (left-fringe . 0) (right-fringe . 0)
                      (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                      (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                      (cursor-type . nil) (no-special-glyphs . t)
                      (desktop-dont-save . t) (visibility . nil)
                      (background-color . ,color)
                      (width . 2) (height . 1))))))
    (set-window-buffer (frame-root-window shadow) (get-buffer-create " *lispm-desktop-shadow*"))
    (with-current-buffer " *lispm-desktop-shadow*"
      (setq-local mode-line-format nil header-line-format nil tab-line-format nil cursor-type nil))
    (lispm-desktop-popup--size shadow (frame-pixel-width frame) (frame-pixel-height frame))
    (pcase-let ((`(,x . ,y) (frame-position frame)))
      (set-frame-position shadow (1+ x) (1+ y)))
    (set-frame-parameter frame 'lispm-desktop-popup-shadow shadow)
    (make-frame-visible shadow)))

(defun lispm-desktop-popup--fill (frame text)
  "Replace popup FRAME's text with TEXT, scrolled to its start."
  (let ((window (frame-root-window frame)))
    (with-current-buffer (window-buffer window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)))
    (set-window-start window 1)
    (set-window-point window 1)
    (set-window-hscroll window 0)))

(defun lispm-desktop-popup--size (frame width height)
  "Make popup FRAME's text area WIDTH by HEIGHT pixels."
  (let ((window-min-width 1)
        (window-safe-min-width 1)
        (window-min-height 1)
        (window-safe-min-height 1)
        (frame-resize-pixelwise t))
    (set-frame-size frame width height t)))

(defun lispm-desktop-popup--show (frame x y &optional shadow)
  "Show popup FRAME with its top left at X, Y of its parent, kept inside it.
SHADOW, `menu' or `alert', casts its shadow, see `lispm-desktop-popup--shadow'."
  (let ((parent (frame-parent frame)))
    (set-frame-position frame
                        (max 0 (min x (- (frame-pixel-width parent) (frame-pixel-width frame) 2)))
                        (max 0 (min y (- (frame-pixel-height parent)
                                         (frame-pixel-height frame) 2))))
    (when shadow
      (lispm-desktop-popup--shadow frame shadow))
    (make-frame-visible frame)
    (redisplay t)))

(defun lispm-desktop-popup--read ()
  "Read the next event, a quit included, without quitting.
The click that opened the popup is not echoed as an unfinished key."
  (let* ((inhibit-quit t)
         (event (let ((echo-keystrokes 0)) (read-event))))
    (when (eq event ?\C-g)
      (setq quit-flag nil))
    event))

(defun lispm-desktop-popup--cancel-p (event)
  "Return non-nil when EVENT cancels a popup: Escape, C-g, Command-period."
  (memq event '(escape ?\e ?\C-g ?\M-.)))

(defun lispm-desktop-popup--confirm-p (event)
  "Return non-nil when EVENT chooses: Return or Enter."
  (memq event '(return ?\r ?\n kp-enter)))

(defun lispm-desktop-popup--press-p (event)
  "Return non-nil when EVENT presses the first mouse button down."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (memq 'down (event-modifiers event))))

(defun lispm-desktop-popup--release-p (event)
  "Return non-nil when EVENT lets the first mouse button go."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (not (memq 'down (event-modifiers event)))))

(defun lispm-desktop-popup--over (event frame)
  "Return EVENT's position when it is over popup FRAME, else nil."
  (let ((position (event-end event)))
    (when (and (windowp (posn-window position))
               (eq (window-frame (posn-window position)) frame))
      position)))

;;;; Menus

(defface lispm-desktop-popup-item '((t :inherit lispm-desktop-platinum-menu-title))
  "Face of a menu item."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-popup-item-highlight '((t :inherit lispm-desktop-platinum-menu-title-active))
  "Face of the menu item under the mouse."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-popup-item-disabled '((t :inherit lispm-desktop-popup-item))
  "Face of a menu item that cannot be chosen: dimmed, as Mac OS 9 greyed it."
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-popup-apply-faces (&rest _)
  "Fit the menu item faces to the active theme: disabled items are dimmed
in the theme's dim colour, Platinum's grey for them."
  (when lispm-desktop-platinum--palette
    (set-face-attribute 'lispm-desktop-popup-item-disabled nil
                        :foreground (lispm-desktop-platinum--color :dim))))

(defconst lispm-desktop-popup-item-height 16
  "Height in pixels of a menu item, Mac OS 9's in the system font.")

(defconst lispm-desktop-popup-separator-height 6
  "Height in pixels of a menu's separator.")

(defconst lispm-desktop-popup-label-x 21
  "Pixels from a menu's inside left edge to its labels: 22 from its
outline, as Mac OS 9 left room for a check mark.")

(defconst lispm-desktop-popup-key-margin 16
  "Pixels from the end of a menu's key equivalents to its inside right edge.")

(defvar lispm-desktop-popup-small nil
  "Non-nil while menus pop up small, as the Control Strip's modules popped
theirs up on Mac OS 9: in the small system font, items 13 pixels tall,
labels 15 pixels in.  Bind it around `lispm-desktop-popup-menu'.")

(defvar lispm-desktop-popup-parent nil
  "The frame menus pop up over, or nil for the selected frame.
The Control Strip binds it to the screen, so its menus open there while
the window in front keeps the keyboard.")

(defun lispm-desktop-popup--item-height ()
  "Return the height in pixels of a menu item, see `lispm-desktop-popup-small'."
  (if lispm-desktop-popup-small 13 lispm-desktop-popup-item-height))

(defun lispm-desktop-popup--label-x ()
  "Return how far in a menu's labels start, see `lispm-desktop-popup-small'."
  (if lispm-desktop-popup-small 15 lispm-desktop-popup-label-x))

(defun lispm-desktop-popup--key-margin ()
  "Return the blank pixels after a menu's longest label or key, see
`lispm-desktop-popup-small'."
  (if lispm-desktop-popup-small 11 lispm-desktop-popup-key-margin))

(defun lispm-desktop-popup--face (enabled)
  "Return the face of a menu item's label, ENABLED or not."
  (cond (lispm-desktop-popup-small 'lispm-desktop-platinum-small)
        (enabled 'lispm-desktop-popup-item)
        (t 'lispm-desktop-popup-item-disabled)))

(defun lispm-desktop-popup--separator-p (item)
  "Return non-nil when menu ITEM is a separator."
  (eq item 'separator))

(defun lispm-desktop-popup--enabled-p (item)
  "Return non-nil when menu ITEM can be chosen.
It needs an action, a submenu or a value to return; an item without
:enabled is enabled, one with :enabled nil is not."
  (and (not (lispm-desktop-popup--separator-p item))
       (or (plist-get item :action) (plist-get item :submenu) (plist-member item :value))
       (or (not (plist-member item :enabled)) (plist-get item :enabled))))

(defun lispm-desktop-popup--super-key-p ()
  "Return non-nil when a key of the keyboard is Super.
On macOS lispm-desktop makes Command Meta, which leaves Super to no key."
  (or (not (eq window-system 'ns))
      (seq-some (lambda (variable) (and (boundp variable) (eq (symbol-value variable) 'super)))
                '(ns-command-modifier ns-right-command-modifier ns-alternate-modifier
                  ns-right-alternate-modifier ns-control-modifier ns-function-modifier))))

(defconst lispm-desktop-popup-typed-keys '(return tab escape backspace delete)
  "The keys besides characters that a menu shows beside its items.
lispm-desktop is used from an HHKB, which has keys for characters and
these alone: arrows, function keys and the like are Fn chords there,
or missing.")

(defun lispm-desktop-popup--keyboard-p (keys)
  "Return non-nil when KEYS is typed on the keyboard alone, as a menu
shows it beside an item: characters, with modifiers or not, and the keys of
`lispm-desktop-popup-typed-keys'.  Mouse and menu bar events do not count, nor
arrows or function keys, nor Super keys where no key is Super."
  (let ((super (lispm-desktop-popup--super-key-p)))
    (seq-every-p (lambda (event)
                   (let ((base (event-basic-type event)))
                     (and (or (integerp base) (memq base lispm-desktop-popup-typed-keys))
                          (or super (not (memq 'super (event-modifiers event)))))))
                 keys)))

(defun lispm-desktop-popup--key (item)
  "Return the key equivalent shown for menu ITEM, or nil.
It is the first key sequence typed on the keyboard that runs the item's
command, as Mac OS 9 showed Command keys beside items; an item holding a
submenu shows the triangle pointing to it."
  (or (and (plist-get item :submenu) "▸")
      (plist-get item :key)
      (let ((action (plist-get item :action)))
        (when (and (symbolp action) (commandp action))
          (when-let* ((keys (seq-find #'lispm-desktop-popup--keyboard-p (where-is-internal action))))
            (key-description keys))))))

(defun lispm-desktop-popup--label-width (label)
  "Return the width in pixels of LABEL in a menu item's face."
  (lispm-desktop-platinum--content-width label (lispm-desktop-popup--face t)))

(defun lispm-desktop-popup--menu-width (items)
  "Return the pixel width a menu of ITEMS needs, its edges included."
  (let ((label 0) (key 0))
    (dolist (item items)
      (unless (lispm-desktop-popup--separator-p item)
        (setq label (max label (lispm-desktop-popup--label-width (plist-get item :label))))
        (setq key (max key (cond ((plist-get item :submenu) 6)
                                 ((lispm-desktop-popup--key item)
                                  (lispm-desktop-popup--label-width (lispm-desktop-popup--key item)))
                                 (t 0))))))
    (+ (lispm-desktop-popup--label-x) label (if (> key 0) (+ 24 key) 0) (lispm-desktop-popup--key-margin))))

(defun lispm-desktop-popup--rows (items)
  "Return where the rows of a menu of ITEMS start, as (TOPS . HEIGHT).
TOPS lists each item's top in pixels; HEIGHT is the menu's."
  (let ((y 0) (tops nil))
    (dolist (item items)
      (push y tops)
      (setq y (+ y (if (lispm-desktop-popup--separator-p item)
                       lispm-desktop-popup-separator-height
                     (lispm-desktop-popup--item-height)))))
    (cons (nreverse tops) y)))

(defun lispm-desktop-popup-draw-menu (palette items width highlight)
  "Return an SVG of a menu WIDTH pixels wide listing ITEMS, in PALETTE.
It is drawn as Mac OS 9 drew its menus, inside the outline the menu's
frame draws: lit along its top and left, shaded along its bottom and
right; items sixteen pixels tall, or 13 in a small menu (see
`lispm-desktop-popup-small'), a check mark before the checked ones,
their key equivalents flush right and a black triangle on those holding
a submenu; separators engraved across the whole width; disabled items
greyed.  HIGHLIGHT is the index of the item under the mouse, drawn in the
variation colour, light on dark, its edges lit and shaded in it too."
  (pcase-let* ((`(,tops . ,height) (lispm-desktop-popup--rows items))
               (svg (svg-create width height))
               (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
               (right (1- width))
               (bottom (1- height)))
    (lispm-desktop-platinum--fill svg 0 0 width height (funcall grey #xDD))
    (lispm-desktop-platinum--row svg 0 (1- right) 0 (funcall grey #xFF))
    (lispm-desktop-platinum--column svg 0 0 (1- bottom) (funcall grey #xFF))
    (lispm-desktop-platinum--column svg right 0 bottom (funcall grey #x99))
    (lispm-desktop-platinum--row svg 0 right bottom (funcall grey #x99))
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-desktop-popup--separator-p item)
             (progn
               (lispm-desktop-platinum--row svg 0 right (+ top 2) (funcall grey #x88))
               (lispm-desktop-platinum--row svg 0 right (+ top 3) (funcall grey #xFF)))
           (let* ((h (lispm-desktop-popup--item-height))
                  (lit (eql index highlight))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             ((lispm-desktop-popup--enabled-p item) (plist-get palette :text))
                             ;; Platinum's #777777; a dark theme's own
                             ;; dim colour, which its menus show.
                             (t (plist-get palette :dim))))
                  (face (lispm-desktop-popup--face (or lit (lispm-desktop-popup--enabled-p item))))
                  (key (and (not (plist-get item :submenu)) (lispm-desktop-popup--key item))))
             (when lit
               (lispm-desktop-platinum--fill svg 0 top width h (plist-get palette :highlight))
               (lispm-desktop-platinum--column svg 0 top (+ top h -1) (plist-get palette :thumb-dark))
               (lispm-desktop-platinum--column svg right top (+ top h -1) (plist-get palette :highlight-dark))
               (when (= top 0)
                 (lispm-desktop-platinum--row svg 0 (1- right) 0 (plist-get palette :thumb-dark)))
               (when (= (+ top h) height)
                 (lispm-desktop-platinum--row svg 0 right bottom (plist-get palette :highlight-dark))))
             (when (plist-get item :checked)
               (lispm-desktop-platinum--draw-content svg "✓" (if lispm-desktop-popup-small 3 4) top h ink face))
             (lispm-desktop-platinum--draw-content svg (plist-get item :label) (lispm-desktop-popup--label-x)
                                                   top h ink face)
             (cond ((plist-get item :submenu)
                    (lispm-desktop-platinum--draw-map
                     svg (- width 12) (+ top 2)
                     '("#" "##" "###" "####" "#####" "######" "#####" "####" "###" "##" "#")
                     `((?# . ,ink))))
                   (key
                    (lispm-desktop-platinum--draw-content
                     svg key (- width (lispm-desktop-popup--key-margin) (lispm-desktop-popup--label-width key))
                     top h ink face)))))))
     items)
    svg))

(defun lispm-desktop-popup--menu-image (items width highlight)
  "Return the image of a menu of ITEMS, see `lispm-desktop-popup-draw-menu'."
  (svg-image (lispm-desktop-popup-draw-menu lispm-desktop-platinum--palette items width highlight)
             :ascent 100 :scale 1))

(defun lispm-desktop-popup-open-menu (items &optional x y align)
  "Show a menu of ITEMS placed by X, Y and ALIGN, see `lispm-desktop-popup-menu'.
Return its frame; nothing is read from the user."
  (let* ((popup (lispm-desktop-popup--frame (or lispm-desktop-popup-parent (selected-frame))))
         (width (lispm-desktop-popup--menu-width items)))
    (lispm-desktop-popup--fill popup (propertize " " 'display (lispm-desktop-popup--menu-image items width nil)))
    (lispm-desktop-popup--size popup width (cdr (lispm-desktop-popup--rows items)))
    (pcase-let ((`(,mx . ,my) (or (and x y (cons x y)) (cdr (mouse-pixel-position)))))
      (lispm-desktop-popup--show popup (if (eq align 'right) (- mx width) mx) my 'menu))
    popup))

(defun lispm-desktop-popup--submenu (item)
  "Return the items of ITEM's submenu, or nil when it has none.
A submenu given as a function is asked for its items only now."
  (let ((submenu (plist-get item :submenu)))
    (if (functionp submenu) (funcall submenu) submenu)))

(defun lispm-desktop-popup--step (items from direction)
  "Return the index of the next enabled item of ITEMS from FROM in DIRECTION."
  (let* ((count (length items))
         (index (or from (if (> direction 0) -1 count))))
    (cl-loop repeat count
             do (setq index (mod (+ index direction) count))
             when (lispm-desktop-popup--enabled-p (nth index items)) return index)))

;; An open menu is a level: its frame, items, width, the item under the
;; mouse, and its place in the parent frame.  A submenu opens as another
;; level beside the item that holds it; the deepest level comes first.

(defun lispm-desktop-popup--level (items x y &optional align)
  "Open a menu level of ITEMS at X, Y, flush right at X when ALIGN is `right'."
  (let* ((frame (lispm-desktop-popup-open-menu items x y align))
         (position (frame-position frame)))
    (list :frame frame :items items :width (lispm-desktop-popup--menu-width items) :highlight nil
          :x (car position) :y (cdr position))))

(defun lispm-desktop-popup--draw-level (level)
  "Redraw menu LEVEL with its item under the mouse highlighted."
  (lispm-desktop-popup--fill (plist-get level :frame)
                             (propertize " " 'display
                                         (lispm-desktop-popup--menu-image (plist-get level :items)
                                                                          (plist-get level :width)
                                                                          (plist-get level :highlight)))))

(defun lispm-desktop-popup--row-y (level index)
  "Return how far down menu LEVEL's frame its item INDEX starts, in pixels."
  (nth index (car (lispm-desktop-popup--rows (plist-get level :items)))))

(defun lispm-desktop-popup--index-at (items y)
  "Return the index of the enabled item of a menu of ITEMS Y pixels down, or nil."
  (let ((index (seq-position (car (lispm-desktop-popup--rows items)) y
                             (lambda (top y) (> top y)))))
    (setq index (1- (or index (length items))))
    (when (and (< -1 index (length items)) (< y (cdr (lispm-desktop-popup--rows items)))
               (lispm-desktop-popup--enabled-p (nth index items)))
      index)))

(defun lispm-desktop-popup--level-index (event level)
  "Return the index of the enabled item of LEVEL under EVENT, or nil."
  (when-let* ((frame (plist-get level :frame))
              (position (lispm-desktop-popup--over event frame))
              (xy (posn-x-y position)))
    (lispm-desktop-popup--index-at (plist-get level :items) (cdr xy))))

(defvar lispm-desktop-popup-menu-bar-step nil
  "Non-nil while the menu bar is walked from the keyboard.
Left in a menu's top level, and Right on an item without a submenu,
then leave the menu for the one beside it in the menu bar: the menu
throws `lispm-desktop-popup-menu-bar-step' with -1 or 1.")

(defun lispm-desktop-popup--item-starting (items from char)
  "Return the index of the next enabled item of ITEMS after FROM whose
label starts with CHAR, ignoring case, or nil."
  (let ((count (length items))
        (prefix (downcase (char-to-string char))))
    (cl-loop for step from 1 to count
             for index = (mod (+ (or from -1) step) count)
             for item = (nth index items)
             when (and (lispm-desktop-popup--enabled-p item)
                       (string-prefix-p prefix (downcase (string-trim-left
                                                          (plist-get item :label)))))
             return index)))

(defun lispm-desktop-popup-menu (items &optional x y align)
  "Pop up ITEMS as a Mac OS 9 menu and run the action of the one chosen.
X and Y place the menu in the selected frame, by default under the
mouse; ALIGN `right' puts its right edge at X.  Each item is a plist
\(:label STRING :action FUNCTION [:enabled BOOL] [:checked BOOL]
[:key STRING] [:submenu ITEMS-OR-FUNCTION]) or the symbol `separator'.
An item with a submenu opens it beside itself, as Mac OS 9's
hierarchical menus did, when the mouse rests on it or Right is typed.
Return the item chosen, or nil when the menu was cancelled."
  (if (not (lispm-desktop-popup-available-p))
      (lispm-desktop-popup--menu-fallback items)
    (lispm-desktop-popup--run (lispm-desktop-popup-choose items x y align))))

(defun lispm-desktop-popup-choose (items &optional x y align)
  "Pop up ITEMS as `lispm-desktop-popup-menu' does, and return the item chosen.
Nothing is run; nil means the menu was cancelled, or had no items."
  (when items
    (let ((levels nil))
      (unwind-protect
          (progn
            (push (lispm-desktop-popup--level items x y align) levels)
            (lispm-desktop-popup--menu-loop (lambda () levels) (lambda (new) (setq levels new))))
        (dolist (level levels)
          (lispm-desktop-popup--dispose (plist-get level :frame)))))))

(defun lispm-desktop-popup--menu-loop (get set)
  "Read events for the open menu levels until one item is chosen; return it.
GET returns the levels, deepest first, and SET replaces them; a level
closed is disposed of here.  The keys act on the level in focus, which
the mouse takes along and Right and Left move into a submenu and out:
moving through a menu shows a submenu beside its item, as Mac OS 9 did,
without entering it."
  (let ((focus (car (funcall get))))
    (cl-labels
        ((levels () (funcall get))
         (redraw (level) (lispm-desktop-popup--draw-level level) (redisplay t))
         (close-to (level)
           ;; Close every level deeper than LEVEL.
           (let ((open (levels)))
             (while (and open (not (eq (car open) level)))
               (lispm-desktop-popup--dispose (plist-get (car open) :frame))
               (setq open (cdr open)))
             (funcall set open)))
         (highlight (level index)
           (unless (eql (plist-get level :highlight) index)
             (close-to level)
             (plist-put level :highlight index)
             (redraw level)
             (let ((item (and index (nth index (plist-get level :items)))))
               (when-let* ((submenu (and item (lispm-desktop-popup--submenu item))))
                 (funcall set (cons (lispm-desktop-popup--level
                                     submenu
                                     (+ (plist-get level :x) (plist-get level :width) -2)
                                     (+ (plist-get level :y) (lispm-desktop-popup--row-y level index) -1))
                                    (levels)))))))
         (item (level)
           (when-let* ((index (plist-get level :highlight)))
             (nth index (plist-get level :items))))
         (child (level)
           ;; The level opened from LEVEL's highlighted item, if any.
           (let ((open (levels)) (found nil))
             (while (and open (cdr open))
               (when (eq (cadr open) level) (setq found (car open)))
               (setq open (cdr open)))
             found))
         (enter (level)
           ;; Move the focus into the submenu LEVEL's item opened.
           (when-let* ((inner (child level)))
             (setq focus inner)
             (highlight inner (lispm-desktop-popup--step (plist-get inner :items) nil 1))))
         (under (event)
           (seq-find (lambda (level) (lispm-desktop-popup--over event (plist-get level :frame)))
                     (levels))))
      ;; A menu opened from the keyboard starts on its first item, so
      ;; Return chooses at once.
      (when (lispm-desktop-typed-event-p last-input-event)
        (highlight focus (lispm-desktop-popup--step (plist-get focus :items) nil 1)))
      (track-mouse
        (catch 'done
          (while t
            (let ((event (lispm-desktop-popup--read)))
              (cond
               ((lispm-desktop-popup--cancel-p event) (throw 'done nil))
               ((memq event '(down ?\C-n))
                (highlight focus (lispm-desktop-popup--step (plist-get focus :items)
                                                            (plist-get focus :highlight) 1)))
               ((memq event '(up ?\C-p))
                (highlight focus (lispm-desktop-popup--step (plist-get focus :items)
                                                            (plist-get focus :highlight) -1)))
               ((memq event '(right ?\C-f))
                (cond ((child focus) (enter focus))
                      (lispm-desktop-popup-menu-bar-step (throw 'lispm-desktop-popup-menu-bar-step 1))))
               ((memq event '(left ?\C-b))
                (let ((parent (cadr (memq focus (levels)))))
                  (cond (parent
                         (close-to focus)
                         (plist-put focus :highlight nil)
                         (redraw focus)
                         (setq focus parent))
                        (lispm-desktop-popup-menu-bar-step (throw 'lispm-desktop-popup-menu-bar-step -1)))))
               ((and (characterp event) (>= event ?\s) (/= event ?\d))
                ;; Typing an item's first letter goes to it, as in lists.
                (when-let* ((index (lispm-desktop-popup--item-starting
                                    (plist-get focus :items) (plist-get focus :highlight) event)))
                  (highlight focus index)))
               ((lispm-desktop-popup--confirm-p event)
                (let ((chosen (item focus)))
                  (cond ((null chosen))
                        ((lispm-desktop-popup--submenu chosen) (enter focus))
                        (t (throw 'done chosen)))))
               ((mouse-movement-p event)
                (when-let* ((level (under event)))
                  (setq focus level)
                  (highlight level (lispm-desktop-popup--level-index event level))))
               ((lispm-desktop-popup--press-p event)
                (unless (under event)
                  (throw 'done nil)))
               ((lispm-desktop-popup--release-p event)
                (when-let* ((level (under event))
                            (index (lispm-desktop-popup--level-index event level))
                            (chosen (nth index (plist-get level :items))))
                  (unless (lispm-desktop-popup--submenu chosen)
                    ;; As Mac OS 9 did, the chosen item blinks once.
                    (plist-put level :highlight nil) (redraw level) (sit-for 0.05)
                    (plist-put level :highlight index) (redraw level) (sit-for 0.05)
                    (throw 'done chosen))))))))))))

(defun lispm-desktop-popup--run (item)
  "Run the action of menu ITEM, when there is one; return ITEM.
A menu the action pops up is its own, not a step of a menu bar walk."
  (when-let* ((action (and item (plist-get item :action))))
    (let ((lispm-desktop-popup-menu-bar-step nil)
          (lispm-desktop-popup-small nil)
          (lispm-desktop-popup-parent nil))
      (if (commandp action) (call-interactively action) (funcall action))))
  item)

(defun lispm-desktop-popup--menu-fallback (items)
  "Choose among ITEMS in the minibuffer, as a menu where none can be drawn.
Submenus are followed down, their items' labels joined with \" › \"."
  (let ((choices nil))
    (cl-labels ((collect (items prefix)
                  (dolist (item items)
                    (cond ((lispm-desktop-popup--submenu item)
                           (collect (lispm-desktop-popup--submenu item)
                                    (concat prefix (plist-get item :label) " › ")))
                          ((lispm-desktop-popup--enabled-p item)
                           (push (cons (concat prefix (plist-get item :label)) item)
                                 choices))))))
      (collect items ""))
    (let ((label (completing-read "Menu: " (reverse choices) nil t)))
      (lispm-desktop-popup--run (cdr (assoc label choices))))))

;;;; Emacs's menus

(defun lispm-desktop-popup--eval (form)
  "Return FORM's value, nil when it signals: a menu item's condition."
  (condition-case nil (eval form t) (error nil)))

(defun lispm-desktop-popup--keymap-item (binding &optional path)
  "Return the menu item of BINDING, an entry of a menu keymap, or nil.
PATH is the list of keys reaching BINDING, kept in the item's :path.  A
separator becomes `separator'; an item hidden by its :visible form, or an
entry that is not a menu item, nothing."
  (pcase binding
    ((pred stringp)
     (when (string-prefix-p "--" binding) 'separator))
    (`(menu-item ,name . ,rest)
     (let ((name (if (stringp name) name (lispm-desktop-popup--eval name)))
           (real (car rest))
           (props (cdr rest)))
       (cond
        ((and (plist-member props :visible)
              (not (lispm-desktop-popup--eval (plist-get props :visible))))
         nil)
        ((and (stringp name) (string-prefix-p "--" name)) 'separator)
        ((not (stringp name)) nil)
        (t
         (let* ((real (if-let* ((filter (plist-get props :filter)))
                          (funcall filter real)
                        real))
                (submenu (and real (keymapp real) (not (commandp real))
                              (lispm-desktop-popup--keymap-map real)))
                (button (plist-get props :button)))
           `(:label ,name
             :action ,(unless submenu real)
             :path ,path
             ,@(when submenu
                 `(:submenu ,(let ((map submenu))
                               (lambda () (lispm-desktop-popup-keymap-items map path)))))
             :enabled ,(and (or submenu real)
                            (or (not (plist-member props :enable))
                                (lispm-desktop-popup--eval (plist-get props :enable)))
                            t)
             ,@(when button `(:checked ,(and (lispm-desktop-popup--eval (cdr button)) t)))
             ,@(when-let* ((keys (plist-get props :keys)))
                 `(:key ,(substitute-command-keys keys)))))))))
    (`(,(and name (pred stringp)) . ,rest)
     (if (string-prefix-p "--" name)
         'separator
       (let ((real (if (stringp (car-safe rest)) (cdr rest) rest)))
         (lispm-desktop-popup--keymap-item `(menu-item ,name ,real) path))))))

(defun lispm-desktop-popup--keymap-map (binding)
  "Return BINDING as a keymap, following a symbol to its function."
  (let ((map (indirect-function binding)))
    (and (keymapp map) map)))

(defun lispm-desktop-popup-keymap-items (keymap &optional prefix)
  "Return the items of menu KEYMAP, as `lispm-desktop-popup-menu' takes them.
Items come in the keymap's order, as Emacs shows them, each with the
:path of keys reaching it after PREFIX; separators are not doubled nor
left at either end."
  (let ((items nil) (seen nil))
    (map-keymap (lambda (key binding)
                  (unless (memq key seen)
                    (push key seen)
                    (when-let* ((item (lispm-desktop-popup--keymap-item
                                       binding (append prefix (list key)))))
                      (push item items))))
                (lispm-desktop-popup--keymap-map keymap))
    (let ((cleaned nil))
      (dolist (item (nreverse items))
        (unless (and (eq item 'separator)
                     (or (null cleaned) (eq (car cleaned) 'separator)))
          (push item cleaned)))
      (when (eq (car cleaned) 'separator) (pop cleaned))
      (nreverse cleaned))))

(defun lispm-desktop-popup--frame-xy (position)
  "Return POSITION as (X . Y) in its frame: an event, a posn, or nil (the mouse)."
  (let ((posn (cond ((posnp position) position)
                    ((eventp position) (event-start position))
                    ((integerp position) (posn-at-point position))
                    (t nil))))
    (if-let* ((window (and posn (posn-window posn)))
              ((windowp window))
              (xy (posn-x-y posn)))
        (let ((edges (window-pixel-edges window))
              (body (window-body-pixel-edges window)))
          (cons (+ (car body) (car xy))
                (+ (if (memq (posn-area posn) '(mode-line header-line tab-line))
                       (nth 1 edges)
                     (nth 1 body))
                   (cdr xy))))
      (cdr (mouse-pixel-position)))))

(defun lispm-desktop-popup-keymap-menu (keymap &optional position)
  "Pop up menu KEYMAP at POSITION as a Mac OS 9 menu and run what is chosen.
POSITION is an event, a posn, a buffer position or nil for the mouse."
  (pcase-let ((`(,x . ,y) (lispm-desktop-popup--frame-xy position)))
    (lispm-desktop-popup-menu (lispm-desktop-popup-keymap-items keymap) (1+ x) (1+ y))))

(defun lispm-desktop-popup--local-right-click-p (position)
  "Return non-nil when a right click at POSITION has a binding of its own.
That is a [mouse-3] command of the buffer or text there, such as the
project tree's or the agents sidebar's menu, which then opens instead."
  (let ((binding (key-binding [mouse-3] t nil position)))
    (and binding (not (eq binding (keymap-lookup global-map "<mouse-3>"))))))

(defun lispm-desktop-popup-context-menu (event)
  "Pop up the context menu for mouse EVENT, as Mac OS 9's contextual menus.
The menu is Emacs's own, `context-menu-map', drawn as a Platinum menu.
Where the click has a right-click command of its own, that runs on the
release instead; where the context menu is empty, nothing opens."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (lispm-desktop-popup--local-right-click-p position)
      (when (windowp window)
        (select-window window))
      (lispm-desktop-popup-keymap-menu (context-menu-map event) event))))

(defvar-local lispm-desktop-context-menu-function nil
  "Function popping up this buffer's right-click menu from the keyboard, or nil.
Windows whose items are drawn as images, the desktop's and the Finder's,
set it: their menu is of the item selected, which point does not mark.")

(defun lispm-desktop-popup-context-menu-at-point ()
  "Pop up the right-click menu of what point is on, as a right click there
would: the place's own menu where it has one, such as an agent's in the
sidebar, else Emacs's context menu.  Right clicks need the mouse, and no
key of the keyboard lispm-desktop is used from opens one."
  (interactive)
  (if lispm-desktop-context-menu-function
      (funcall lispm-desktop-context-menu-function)
    (let* ((position (or (posn-at-point) (user-error "Point is not in sight")))
           (local (and (lispm-desktop-popup--local-right-click-p position)
                       (key-binding [mouse-3] t nil position))))
      (if (commandp local)
          (call-interactively local nil (vector (list 'mouse-3 position)))
        (lispm-desktop-popup-keymap-menu (context-menu-map (list 'down-mouse-3 position)) position)))))

(defun lispm-desktop-popup--pane-items (menu)
  "Return the items of MENU, an `x-popup-menu' menu of panes.
MENU is (TITLE (PANE-TITLE ITEM...)...); an ITEM (STRING . VALUE)
returns VALUE when chosen, a lone STRING is a line that cannot be, and
one beginning with -- a separator.  Panes are parted by separators, and
titled when there are several."
  (let ((panes (cdr menu))
        (items nil))
    (dolist (pane panes)
      (when items (push 'separator items))
      (when (and (cdr panes) (stringp (car pane)) (not (string-empty-p (car pane))))
        (push (list :label (car pane) :enabled nil) items))
      (dolist (entry (cdr pane))
        (push (pcase entry
                ((pred stringp)
                 (if (or (string-empty-p entry) (string-prefix-p "--" entry))
                     'separator
                   (list :label entry :enabled nil)))
                (`(,(and label (pred stringp)) . ,value)
                 (list :label label :value value)))
              items)))
    (nreverse items)))

(defun lispm-desktop-popup--answer-paths (items)
  "Return ITEMS, from a menu keymap, each answering with its key path.
The items of their submenus answer the same way, when they open."
  (dolist (item items items)
    (when (consp item)
      (if-let* ((inner (copy-sequence item))
                ((plist-get item :submenu)))
          (plist-put item :submenu
                     (lambda () (lispm-desktop-popup--answer-paths (lispm-desktop-popup--submenu inner))))
        (plist-put item :value (plist-get item :path))))))

(defun lispm-desktop-popup--x-menu-p (menu)
  "Return non-nil when MENU, as `x-popup-menu' takes it, can be drawn here:
a keymap, a list of keymaps, or a title and its panes."
  (or (keymapp menu)
      (and (consp menu) (keymapp (car menu)))
      (and (consp menu) (stringp (car menu)) (seq-every-p #'consp (cdr menu)))))

(defun lispm-desktop-popup--x-items (menu)
  "Return the items of MENU as `x-popup-menu' takes it.
Keymap items answer with their key path, pane items with their value."
  (cond ((keymapp menu)
         (lispm-desktop-popup--answer-paths (lispm-desktop-popup-keymap-items menu)))
        ((keymapp (car menu))
         (let ((items nil))
           (dolist (map menu)
             (let ((more (lispm-desktop-popup--x-items map)))
               (when (and items more) (push 'separator items))
               (setq items (append (reverse more) items))))
           (nreverse items)))
        (t (lispm-desktop-popup--pane-items menu))))

(defun lispm-desktop-popup--x-position (position)
  "Return POSITION, as `x-popup-menu' takes it, as (X . Y) in its frame.
Offsets from a window are taken from its text area, where `popup-menu'
measures a click or point."
  (pcase position
    ('t (cdr (mouse-pixel-position)))
    (`((,x ,y) ,(and window (pred windowp)))
     (let ((edges (window-body-pixel-edges window)))
       (cons (+ (car edges) x) (+ (nth 1 edges) y))))
    (`((,x ,y) ,(pred framep)) (cons x y))
    (_ (lispm-desktop-popup--frame-xy position))))

(defun lispm-desktop-popup--x-popup-menu (x-popup-menu position menu)
  "Draw MENU as a Mac OS 9 menu where `x-popup-menu' would draw a native one.
X-POPUP-MENU is the original, called without a POSITION (it then only
computes key equivalents), where no popup can be drawn, and for menus
it cannot read.  The value is what the original returns: the key path of
the item chosen from a keymap, the value of one chosen from panes, nil
when cancelled."
  (if (not (and position (lispm-desktop-popup-available-p) (lispm-desktop-popup--x-menu-p menu)))
      (funcall x-popup-menu position menu)
    (pcase-let ((`(,x . ,y) (lispm-desktop-popup--x-position position)))
      (plist-get (lispm-desktop-popup-choose (lispm-desktop-popup--x-items menu) (1+ x) (1+ y)) :value))))

(defun lispm-desktop-popup-enable ()
  "Draw right-click menus, and every menu `x-popup-menu' pops up, as Mac
OS 9's menus.  A right click opens Emacs's context menu, or the right-click
command of the place clicked."
  ;; The palette exists only now, after `lispm-desktop-setup-platinum': at load
  ;; time and at the first theme's loading, the item faces kept the menu
  ;; bar's taller padding.
  (lispm-desktop-popup-apply-faces)
  (keymap-global-set "<down-mouse-3>" #'lispm-desktop-popup-context-menu)
  (keymap-global-set "<mouse-3>" #'ignore)
  (keymap-global-set "C-c c" #'lispm-desktop-popup-context-menu-at-point)
  (advice-add 'x-popup-menu :around #'lispm-desktop-popup--x-popup-menu))

;;;; Alerts

(defconst lispm-desktop-popup-alert-width 364
  "Width in pixels of an alert inside its outline, Mac OS 9's.")

(defconst lispm-desktop-popup-alert-text-x 79
  "Pixels from an alert's inside left edge to its message: 80 from its outline.")

(defconst lispm-desktop-popup-button-gap 10
  "Pixels between neighbouring buttons of an alert, ring to outline.")

(defun lispm-desktop-popup--wrap (text width face)
  "Return TEXT broken into lines no wider than WIDTH pixels in FACE.
Lines break between words; a word wider than WIDTH stands alone."
  (let ((lines nil) (line nil))
    (dolist (word (split-string text "[ \t\n]+" t))
      (let ((longer (if line (concat line " " word) word)))
        (if (or (null line)
                (<= (lispm-desktop-platinum--content-width longer face) width))
            (setq line longer)
          (push line lines)
          (setq line word))))
    (nreverse (if line (cons line lines) lines))))

(defun lispm-desktop-popup--alert-layout (message detail buttons)
  "Return where an alert of MESSAGE, DETAIL and BUTTONS draws its parts.
The value is a plist: :height of the alert inside its outline,
:message and :detail as lists of lines, and :buttons as (LABEL VALUE
ROLE X Y WIDTH).  Laid out as Mac OS 9 laid out its alerts: the caution,
stop or note icon at the top left, the message to its right in the
system font, the detail under it; the default button at the bottom
right, Cancel to its left, and a button such as Don't Save, which comes
before Cancel, standing apart at the left, under the message."
  (let* ((w lispm-desktop-popup-alert-width)
         (text-width (- w lispm-desktop-popup-alert-text-x 20))
         (lines (lispm-desktop-popup--wrap message text-width 'lispm-desktop-platinum-title))
         (details (and detail (lispm-desktop-popup--wrap detail text-width 'lispm-desktop-platinum-list)))
         (text-bottom (+ 15 (* 16 (length lines)) (if details (+ 6 (* 16 (length details))) 0)))
         (top (+ (max text-bottom 47) 29))
         (cancel (seq-position buttons 'cancel (lambda (button role) (eq (nth 2 button) role))))
         (apart (lambda (index) (and cancel (< index cancel) (null (nth 2 (nth index buttons))))))
         (placed nil))
    ;; The buttons at the right, from the right: the default's ring three
    ;; pixels about it and twelve from the edge.
    (let ((right (- w 13)) (previous nil))
      (dolist (index (number-sequence (1- (length buttons)) 0 -1))
        (unless (funcall apart index)
          (pcase-let* ((`(,label ,value ,role) (nth index buttons))
                       (width (lispm-desktop-platinum-push-button-width label))
                       (ring (if (eq role 'default) 3 0)))
            (when previous
              (setq right (- right lispm-desktop-popup-button-gap 1)))
            (push (list label value role (- right ring width -1) top width) placed)
            (setq right (- right ring width -1 ring) previous t)))))
    ;; Those standing apart, from the message's left.
    (let ((left (- lispm-desktop-popup-alert-text-x 4)))
      (seq-do-indexed
       (lambda (button index)
         (when (funcall apart index)
           (pcase-let* ((`(,label ,value ,role) button)
                        (width (lispm-desktop-platinum-push-button-width label)))
             (push (list label value role left top width) placed)
             (setq left (+ left width lispm-desktop-popup-button-gap)))))
       buttons))
    (list :height (+ top lispm-desktop-platinum-button-height 15)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(defun lispm-desktop-popup-draw-alert (palette icon message detail buttons pressed focused)
  "Return an SVG of an alert of ICON, MESSAGE, DETAIL and BUTTONS, in PALETTE.
It is drawn as Mac OS 9 drew its alerts, inside the outline the alert's
frame draws: a light red ring lit along the top and left and darker along
the bottom and right, then a white line and a grey one, round a platinum
body; the parts placed by `lispm-desktop-popup--alert-layout'.  PRESSED
is the VALUE of the button held down, FOCUSED that of the one the
keyboard is on."
  (let* ((layout (lispm-desktop-popup--alert-layout message detail buttons))
         (w lispm-desktop-popup-alert-width)
         (h (plist-get layout :height))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (y 15))
    (lispm-desktop-platinum--fill svg 0 0 w h (plist-get palette :alert-dark))
    (lispm-desktop-platinum--fill svg 0 0 (1- w) (1- h) (plist-get palette :alert))
    (lispm-desktop-platinum--fill svg 1 1 (- w 2) (- h 2) (funcall grey #x99))
    (lispm-desktop-platinum--fill svg 1 1 (- w 3) (- h 3) (funcall grey #xFF))
    (lispm-desktop-platinum--fill svg 2 2 (- w 4) (- h 4) (funcall grey #xDD))
    (lispm-desktop-platinum--embed-icon svg (expand-file-name (format "icons/os9/%s.svg" icon)
                                                              lispm-desktop-directory)
                                        25 15 32)
    (dolist (line (plist-get layout :message))
      (lispm-desktop-platinum--draw-content svg line lispm-desktop-popup-alert-text-x y 16
                                            (plist-get palette :text) 'lispm-desktop-platinum-title)
      (setq y (+ y 16)))
    (when (plist-get layout :detail)
      (setq y (+ y 6))
      (dolist (line (plist-get layout :detail))
        (lispm-desktop-platinum--draw-content svg line lispm-desktop-popup-alert-text-x y 16
                                              (plist-get palette :text) 'lispm-desktop-platinum-list)
        (setq y (+ y 16))))
    (pcase-dolist (`(,label ,value ,role ,x ,top ,width) (plist-get layout :buttons))
      (lispm-desktop-platinum--draw-push-button svg x top palette label width
                                                :state (and (equal pressed value) 'pressed)
                                                :default (eq role 'default)
                                                :focused (and focused (equal focused value))))
    svg))

(defun lispm-desktop-popup--draw-alert (frame icon message detail buttons pressed focused)
  "Show the alert of ICON, MESSAGE, DETAIL and BUTTONS in popup FRAME.
PRESSED and FOCUSED are as `lispm-desktop-popup-draw-alert' takes them.
The buttons' places are kept in FRAME's `lispm-desktop-popup-buttons'."
  (lispm-desktop-popup--fill
   frame (propertize " " 'display
                     (svg-image (lispm-desktop-popup-draw-alert lispm-desktop-platinum--palette icon message
                                                                detail buttons pressed focused)
                                :ascent 100 :scale 1)))
  (set-frame-parameter frame 'lispm-desktop-popup-buttons
                       (plist-get (lispm-desktop-popup--alert-layout message detail buttons) :buttons)))

(defun lispm-desktop-popup--button-at (event frame)
  "Return the value of the alert button under EVENT in FRAME, or nil."
  (when-let* ((position (lispm-desktop-popup--over event frame))
              (xy (posn-x-y position)))
    (pcase-let ((`(,x . ,y) xy))
      (seq-some (pcase-lambda (`(,_ ,value ,_ ,left ,top ,width))
                  (and (<= left x (+ left width -1))
                       (<= top y (+ top lispm-desktop-platinum-button-height -1))
                       value))
                (frame-parameter frame 'lispm-desktop-popup-buttons)))))

(defun lispm-desktop-popup-open-alert (icon message buttons &optional detail)
  "Show an alert of ICON, MESSAGE, BUTTONS and DETAIL.
See `lispm-desktop-popup-alert'.  It sits over the upper middle of the
selected frame, as Mac OS 9's alerts did, outlined and casting a black
shadow.  Return its frame; nothing is read from the user."
  (let* ((parent (selected-frame))
         (popup (lispm-desktop-popup--frame parent))
         (layout (lispm-desktop-popup--alert-layout message detail buttons)))
    (lispm-desktop-popup--draw-alert popup icon message detail buttons nil nil)
    (lispm-desktop-popup--size popup lispm-desktop-popup-alert-width (plist-get layout :height))
    (lispm-desktop-popup--show popup (/ (- (frame-pixel-width parent) lispm-desktop-popup-alert-width) 2)
                               (/ (frame-pixel-height parent) 5)
                               'alert)
    popup))

(defun lispm-desktop-popup-alert (icon message buttons &optional detail)
  "Show a Mac OS 9 alert and return the value of the button chosen.
ICON is `caution', `stop' or `note'; MESSAGE is the question, DETAIL a
line or two under it.  BUTTONS is a list of (LABEL VALUE ROLE KEYS...):
ROLE `default' marks the button Return presses, `cancel' the one Escape
does, and without one Escape dismisses the alert by its default; KEYS
are events pressing a button too, as Command-D pressed Don't Save."
  (let ((default (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'default)) buttons)))
        (cancel (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'cancel)) buttons))))
    (if (not (lispm-desktop-popup-available-p))
        (lispm-desktop-popup--alert-fallback message buttons detail cancel)
      (let* ((popup (lispm-desktop-popup-open-alert icon message buttons detail))
             (values (mapcar #'cadr buttons))
             (pressed nil) (held nil) (focused nil) (choice cancel))
        (unwind-protect
            (cl-flet ((redraw ()
                        (lispm-desktop-popup--draw-alert popup icon message detail buttons pressed focused)
                        (redisplay t))
                      (focus (step)
                        ;; Tab goes from no button to the first, as from the
                        ;; message to the controls.
                        (let ((index (seq-position values focused)))
                          (setq focused (nth (mod (if index (+ index step) (if (> step 0) 0 -1))
                                                  (length values))
                                             values)))))
              (track-mouse
                (catch 'done
                  (while t
                    (let ((event (lispm-desktop-popup--read)))
                      (cond
                       ((seq-find (lambda (button) (memql event (nthcdr 3 button))) buttons)
                        (setq pressed (nth 1 (seq-find (lambda (button)
                                                         (memql event (nthcdr 3 button)))
                                                       buttons)))
                        (redraw) (sit-for 0.12)
                        (setq choice pressed) (throw 'done nil))
                       ((and (lispm-desktop-popup--cancel-p event) (or cancel default))
                        ;; An alert with no Cancel, only OK, is dismissed.
                        (setq choice (or cancel default)) (throw 'done nil))
                       ((and (lispm-desktop-popup--confirm-p event) default)
                        ;; The default button blinks as it is pressed.
                        (setq pressed default) (redraw) (sit-for 0.12)
                        (setq choice default) (throw 'done nil))
                       ((memq event '(tab ?\t ?\C-f right))
                        (focus 1) (redraw))
                       ((memq event '(backtab S-tab ?\C-b left))
                        (focus -1) (redraw))
                       ((and focused (eq event ?\s))
                        ;; Space presses the button the keyboard is on.
                        (setq pressed focused) (redraw) (sit-for 0.12)
                        (setq choice focused) (throw 'done nil))
                       ((lispm-desktop-popup--press-p event)
                        (when (setq held (lispm-desktop-popup--button-at event popup))
                          (setq pressed held) (redraw)))
                       ((and held (mouse-movement-p event))
                        (let ((over (equal (lispm-desktop-popup--button-at event popup) held)))
                          (unless (eq over (and pressed t))
                            (setq pressed (and over held)) (redraw))))
                       ((lispm-desktop-popup--release-p event)
                        (when (and held (equal (lispm-desktop-popup--button-at event popup) held))
                          (setq choice held) (throw 'done nil))
                        (setq held nil pressed nil) (redraw))))))))
          (lispm-desktop-popup--dispose popup))
        choice))))

(defun lispm-desktop-popup--alert-fallback (message buttons detail cancel)
  "Ask MESSAGE in the minibuffer, offering BUTTONS; return the value chosen.
DETAIL follows MESSAGE; quitting returns CANCEL."
  (condition-case nil
      (let* ((keys (let ((used nil))
                     (mapcar (pcase-lambda (`(,label ,value ,_))
                               (let ((key (seq-find (lambda (c) (not (memq c used)))
                                                    (downcase label))))
                                 (push key used)
                                 (list key label value)))
                             buttons)))
             (answer (read-multiple-choice
                      (concat message (when detail (concat "  " detail)))
                      (mapcar (pcase-lambda (`(,key ,label ,_)) (list key label)) keys))))
        (nth 2 (assq (car answer) keys)))
    (quit cancel)))

(lispm-desktop-popup-apply-faces)
(add-hook 'enable-theme-functions #'lispm-desktop-popup-apply-faces 95)

(provide 'lispm-desktop-popup)
;;; lispm-desktop-popup.el ends here
