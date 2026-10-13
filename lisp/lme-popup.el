;;; lme-popup.el --- Mac OS 9's menus and alerts  -*- lexical-binding: t; -*-

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

;; Mac OS 9 drew its own menus and alerts, and so does lispm-emacs, each
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
;; Drawing (`lme-popup-draw-button', the text of menus and alerts) is
;; separate from the modal loops, which only read events and redraw.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lme-core)
(require 'lme-platinum)

(declare-function lme-tab-bar--blend "lme-interface" (from to ratio))

;;;; Popup frames

(defun lme-popup-available-p ()
  "Return non-nil when popups can be drawn: a graphical, interactive Emacs."
  (and (display-graphic-p) (not noninteractive) lme-platinum--palette))

(defun lme-popup--frame (parent)
  "Return a hidden child frame of PARENT with a buffer of its own.
It takes no focus, and hooks run for new frames do not see it."
  (let* ((buffer (generate-new-buffer " *lme-popup*"))
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
                     (background-color . ,(lme-platinum--color :chrome))
                     (foreground-color . ,(lme-platinum--color :text))
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

(defun lme-popup--dispose (frame)
  "Delete popup FRAME and its buffer."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame))))
      (delete-frame frame t)
      (kill-buffer buffer))))

(defun lme-popup--fill (frame text)
  "Replace popup FRAME's text with TEXT, scrolled to its start."
  (let ((window (frame-root-window frame)))
    (with-current-buffer (window-buffer window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)))
    (set-window-start window 1)
    (set-window-point window 1)
    (set-window-hscroll window 0)))

(defun lme-popup--size (frame width height)
  "Make popup FRAME's text area WIDTH by HEIGHT pixels."
  (let ((window-min-width 1)
        (window-safe-min-width 1)
        (window-min-height 1)
        (window-safe-min-height 1)
        (frame-resize-pixelwise t))
    (set-frame-size frame width height t)))

(defun lme-popup--show (frame x y)
  "Show popup FRAME with its top left at X, Y of its parent, kept inside it."
  (let ((parent (frame-parent frame)))
    (set-frame-position frame
                        (max 0 (min x (- (frame-pixel-width parent) (frame-pixel-width frame) 2)))
                        (max 0 (min y (- (frame-pixel-height parent)
                                         (frame-pixel-height frame) 2))))
    (make-frame-visible frame)
    (redisplay t)))

(defun lme-popup--read ()
  "Read the next event, a quit included, without quitting.
The click that opened the popup is not echoed as an unfinished key."
  (let* ((inhibit-quit t)
         (event (let ((echo-keystrokes 0)) (read-event))))
    (when (eq event ?\C-g)
      (setq quit-flag nil))
    event))

(defun lme-popup--cancel-p (event)
  "Return non-nil when EVENT cancels a popup: Escape, C-g, Command-period."
  (memq event '(escape ?\e ?\C-g ?\M-.)))

(defun lme-popup--confirm-p (event)
  "Return non-nil when EVENT chooses: Return or Enter."
  (memq event '(return ?\r ?\n kp-enter)))

(defun lme-popup--press-p (event)
  "Return non-nil when EVENT presses the first mouse button down."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (memq 'down (event-modifiers event))))

(defun lme-popup--release-p (event)
  "Return non-nil when EVENT lets the first mouse button go."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (not (memq 'down (event-modifiers event)))))

(defun lme-popup--over (event frame)
  "Return EVENT's position when it is over popup FRAME, else nil."
  (let ((position (event-end event)))
    (when (and (windowp (posn-window position))
               (eq (window-frame (posn-window position)) frame))
      position)))

;;;; Menus

(defface lme-popup-item '((t :inherit lme-platinum-menu-title))
  "Face of a menu item."
  :group 'lme-platinum)

(defface lme-popup-item-highlight '((t :inherit lme-platinum-menu-title-active))
  "Face of the menu item under the mouse."
  :group 'lme-platinum)

(defconst lme-popup-item-padding 2
  "Pixels above and below a menu item's label, about as Mac OS 9 spaced them.")

(defface lme-popup-item-disabled '((t :inherit lme-popup-item))
  "Face of a menu item that cannot be chosen: dimmed, as Mac OS 9 greyed it."
  :group 'lme-platinum)

(defun lme-popup-apply-faces (&rest _)
  "Fit the menu item faces to the active theme.
Items are padded less than menu bar titles; disabled ones are dimmed in
the theme's muted colour."
  (when lme-platinum--palette
    (let ((chrome (lme-platinum--color :chrome))
          (accent (lme-platinum--color :accent)))
      (set-face-attribute 'lme-popup-item nil
                          :box `(:line-width (1 . ,lme-popup-item-padding) :color ,chrome))
      (set-face-attribute 'lme-popup-item-highlight nil
                          :box `(:line-width (1 . ,lme-popup-item-padding) :color ,accent))
      (set-face-attribute 'lme-popup-item-disabled nil
                          :foreground (lme-platinum--color :muted)
                          :box `(:line-width (1 . ,lme-popup-item-padding) :color ,chrome)))))

(defconst lme-popup-menu-padding '(22 . 18)
  "Pixels before a menu item's label (room for a check mark) and after the
key equivalent.")

(defun lme-popup--separator-p (item)
  "Return non-nil when menu ITEM is a separator."
  (eq item 'separator))

(defun lme-popup--enabled-p (item)
  "Return non-nil when menu ITEM can be chosen.
It needs an action, a submenu or a value to return; an item without
:enabled is enabled, one with :enabled nil is not."
  (and (not (lme-popup--separator-p item))
       (or (plist-get item :action) (plist-get item :submenu) (plist-member item :value))
       (or (not (plist-member item :enabled)) (plist-get item :enabled))))

(defun lme-popup--keyboard-p (keys)
  "Return non-nil when KEYS is typed on the keyboard alone.
Mouse, wheel, touch and menu bar events do not count."
  (seq-every-p (lambda (event)
                 (not (and (symbolp event)
                           (string-match-p (concat "mouse\\|wheel\\|drag\\|touch\\|pinch"
                                                   "\\|swipe\\|menu-bar\\|tab-bar\\|tool-bar")
                                           (symbol-name event)))))
               keys))

(defun lme-popup--key (item)
  "Return the key equivalent shown for menu ITEM, or nil.
It is the first key sequence typed on the keyboard that runs the item's
command, as Mac OS 9 showed Command keys beside items; an item holding a
submenu shows the triangle pointing to it."
  (or (and (plist-get item :submenu) "▸")
      (plist-get item :key)
      (let ((action (plist-get item :action)))
        (when (and (symbolp action) (commandp action))
          (when-let* ((keys (seq-find #'lme-popup--keyboard-p (where-is-internal action))))
            (key-description keys))))))

(defun lme-popup--menu-width (items)
  "Return the pixel width a menu of ITEMS needs."
  (let ((label 0) (key 0))
    (dolist (item items)
      (unless (lme-popup--separator-p item)
        (setq label (max label (string-pixel-width
                                (propertize (plist-get item :label) 'face 'lme-popup-item))))
        (when-let* ((equivalent (lme-popup--key item)))
          (setq key (max key (string-pixel-width
                              (propertize equivalent 'face 'lme-popup-item)))))))
    (+ (car lme-popup-menu-padding) label (if (> key 0) (+ 28 key) 0)
       (cdr lme-popup-menu-padding))))

(defun lme-popup-menu-text (items width highlight)
  "Return the text of a menu WIDTH pixels wide listing ITEMS.
HIGHLIGHT is the index of the item under the mouse, or nil.  Each item
is one line: a check mark when checked, its label, and its key
equivalent flush right; a separator is a thin rule.  Every line ends in
a newline of its own face, so a separator line is as short as its rule
both when drawn and when measured."
  (let ((index -1)
        (last (1- (length items))))
    (mapconcat
     (lambda (item)
       (cl-incf index)
       (let ((newline (if (= index last) "" "\n")))
         (if (lme-popup--separator-p item)
             (let ((face `(:strike-through ,(lme-platinum--color :shade) :height 0.6
                           :background ,(lme-platinum--color :chrome))))
               (concat (propertize " " 'display `(space :align-to (,width)) 'face face)
                       (propertize newline 'face face)))
           (let* ((face (cond ((eql index highlight) 'lme-popup-item-highlight)
                              ((lme-popup--enabled-p item) 'lme-popup-item)
                              (t 'lme-popup-item-disabled)))
                  (key (lme-popup--key item))
                  (key-width (if key (string-pixel-width (propertize key 'face face)) 0)))
             (propertize
              (concat (propertize " " 'display '(space :width (7)))
                      (if (plist-get item :checked) "✓" "")
                      (propertize " " 'display `(space :align-to (,(car lme-popup-menu-padding))))
                      (plist-get item :label)
                      (propertize " " 'display
                                  `(space :align-to (,(- width key-width
                                                         (cdr lme-popup-menu-padding)))))
                      (or key "")
                      (propertize " " 'display `(space :align-to (,width)))
                      newline)
              'face face 'lme-popup-index index)))))
     items "")))

(defun lme-popup-open-menu (items &optional x y align)
  "Show a menu of ITEMS placed by X, Y and ALIGN, see `lme-popup-menu'.
Return its frame; nothing is read from the user."
  (let* ((popup (lme-popup--frame (selected-frame)))
         (window (frame-root-window popup))
         (width (lme-popup--menu-width items)))
    (lme-popup--fill popup (lme-popup-menu-text items width nil))
    ;; Measured once the frame is as wide as the menu, so no item wraps.
    (lme-popup--size popup width 400)
    (lme-popup--size popup width (cdr (buffer-text-pixel-size (window-buffer window) window)))
    (pcase-let ((`(,mx . ,my) (or (and x y (cons x y)) (cdr (mouse-pixel-position)))))
      (lme-popup--show popup (if (eq align 'right) (- mx width) mx) my))
    popup))

(defun lme-popup--submenu (item)
  "Return the items of ITEM's submenu, or nil when it has none.
A submenu given as a function is asked for its items only now."
  (let ((submenu (plist-get item :submenu)))
    (if (functionp submenu) (funcall submenu) submenu)))

(defun lme-popup--step (items from direction)
  "Return the index of the next enabled item of ITEMS from FROM in DIRECTION."
  (let* ((count (length items))
         (index (or from (if (> direction 0) -1 count))))
    (cl-loop repeat count
             do (setq index (mod (+ index direction) count))
             when (lme-popup--enabled-p (nth index items)) return index)))

;; An open menu is a level: its frame, items, width, the item under the
;; mouse, and its place in the parent frame.  A submenu opens as another
;; level beside the item that holds it; the deepest level comes first.

(defun lme-popup--level (items x y &optional align)
  "Open a menu level of ITEMS at X, Y, flush right at X when ALIGN is `right'."
  (let* ((frame (lme-popup-open-menu items x y align))
         (position (frame-position frame)))
    (list :frame frame :items items :width (lme-popup--menu-width items) :highlight nil
          :x (car position) :y (cdr position))))

(defun lme-popup--draw-level (level)
  "Redraw menu LEVEL with its item under the mouse highlighted."
  (lme-popup--fill (plist-get level :frame)
                   (lme-popup-menu-text (plist-get level :items) (plist-get level :width)
                                        (plist-get level :highlight))))

(defun lme-popup--row-y (level index)
  "Return how far down menu LEVEL's frame its item INDEX starts, in pixels."
  (let ((window (frame-root-window (plist-get level :frame))))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char (point-min))
        (forward-line index)
        (or (cdr (posn-x-y (posn-at-point (point) window))) 0)))))

(defun lme-popup--level-index (event level)
  "Return the index of the enabled item of LEVEL under EVENT, or nil."
  (when-let* ((frame (plist-get level :frame))
              (position (lme-popup--over event frame))
              (point (posn-point position)))
    (let ((index (with-current-buffer (window-buffer (posn-window position))
                   (1- (line-number-at-pos point))))
          (items (plist-get level :items)))
      (when (and (< -1 index (length items)) (lme-popup--enabled-p (nth index items)))
        index))))

(defun lme-popup-menu (items &optional x y align)
  "Pop up ITEMS as a Mac OS 9 menu and run the action of the one chosen.
X and Y place the menu in the selected frame, by default under the
mouse; ALIGN `right' puts its right edge at X.  Each item is a plist
\(:label STRING :action FUNCTION [:enabled BOOL] [:checked BOOL]
[:key STRING] [:submenu ITEMS-OR-FUNCTION]) or the symbol `separator'.
An item with a submenu opens it beside itself, as Mac OS 9's
hierarchical menus did, when the mouse rests on it or Right is typed.
Return the item chosen, or nil when the menu was cancelled."
  (if (not (lme-popup-available-p))
      (lme-popup--menu-fallback items)
    (lme-popup--run (lme-popup-choose items x y align))))

(defun lme-popup-choose (items &optional x y align)
  "Pop up ITEMS as `lme-popup-menu' does, and return the item chosen.
Nothing is run; nil means the menu was cancelled, or had no items."
  (when items
    (let ((levels nil))
      (unwind-protect
          (progn
            (push (lme-popup--level items x y align) levels)
            (lme-popup--menu-loop (lambda () levels) (lambda (new) (setq levels new))))
        (dolist (level levels)
          (lme-popup--dispose (plist-get level :frame)))))))

(defun lme-popup--menu-loop (get set)
  "Read events for the open menu levels until one item is chosen; return it.
GET returns the levels, deepest first, and SET replaces them; a level
closed is disposed of here.  The keys act on the level in focus, which
the mouse takes along and Right and Left move into a submenu and out:
moving through a menu shows a submenu beside its item, as Mac OS 9 did,
without entering it."
  (let ((focus (car (funcall get))))
    (cl-labels
        ((levels () (funcall get))
         (redraw (level) (lme-popup--draw-level level) (redisplay t))
         (close-to (level)
           ;; Close every level deeper than LEVEL.
           (let ((open (levels)))
             (while (and open (not (eq (car open) level)))
               (lme-popup--dispose (plist-get (car open) :frame))
               (setq open (cdr open)))
             (funcall set open)))
         (highlight (level index)
           (unless (eql (plist-get level :highlight) index)
             (close-to level)
             (plist-put level :highlight index)
             (redraw level)
             (let ((item (and index (nth index (plist-get level :items)))))
               (when-let* ((submenu (and item (lme-popup--submenu item))))
                 (funcall set (cons (lme-popup--level
                                     submenu
                                     (+ (plist-get level :x) (plist-get level :width) -2)
                                     (+ (plist-get level :y) (lme-popup--row-y level index) -1))
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
             (highlight inner (lme-popup--step (plist-get inner :items) nil 1))))
         (under (event)
           (seq-find (lambda (level) (lme-popup--over event (plist-get level :frame)))
                     (levels))))
      (track-mouse
        (catch 'done
          (while t
            (let ((event (lme-popup--read)))
              (cond
               ((lme-popup--cancel-p event) (throw 'done nil))
               ((memq event '(down ?\C-n))
                (highlight focus (lme-popup--step (plist-get focus :items)
                                                  (plist-get focus :highlight) 1)))
               ((memq event '(up ?\C-p))
                (highlight focus (lme-popup--step (plist-get focus :items)
                                                  (plist-get focus :highlight) -1)))
               ((memq event '(right ?\C-f))
                (enter focus))
               ((memq event '(left ?\C-b))
                (let ((parent (cadr (memq focus (levels)))))
                  (when parent
                    (close-to focus)
                    (plist-put focus :highlight nil)
                    (redraw focus)
                    (setq focus parent))))
               ((lme-popup--confirm-p event)
                (let ((chosen (item focus)))
                  (cond ((null chosen))
                        ((lme-popup--submenu chosen) (enter focus))
                        (t (throw 'done chosen)))))
               ((mouse-movement-p event)
                (when-let* ((level (under event)))
                  (setq focus level)
                  (highlight level (lme-popup--level-index event level))))
               ((lme-popup--press-p event)
                (unless (under event)
                  (throw 'done nil)))
               ((lme-popup--release-p event)
                (when-let* ((level (under event))
                            (index (lme-popup--level-index event level))
                            (chosen (nth index (plist-get level :items))))
                  (unless (lme-popup--submenu chosen)
                    ;; As Mac OS 9 did, the chosen item blinks once.
                    (plist-put level :highlight nil) (redraw level) (sit-for 0.05)
                    (plist-put level :highlight index) (redraw level) (sit-for 0.05)
                    (throw 'done chosen))))))))))))

(defun lme-popup--run (item)
  "Run the action of menu ITEM, when there is one; return ITEM."
  (when-let* ((action (and item (plist-get item :action))))
    (if (commandp action) (call-interactively action) (funcall action)))
  item)

(defun lme-popup--menu-fallback (items)
  "Choose among ITEMS in the minibuffer, as a menu where none can be drawn.
Submenus are followed down, their items' labels joined with \" › \"."
  (let ((choices nil))
    (cl-labels ((collect (items prefix)
                  (dolist (item items)
                    (cond ((lme-popup--submenu item)
                           (collect (lme-popup--submenu item)
                                    (concat prefix (plist-get item :label) " › ")))
                          ((lme-popup--enabled-p item)
                           (push (cons (concat prefix (plist-get item :label)) item)
                                 choices))))))
      (collect items ""))
    (let ((label (completing-read "Menu: " (reverse choices) nil t)))
      (lme-popup--run (cdr (assoc label choices))))))

;;;; Emacs's menus

(defun lme-popup--eval (form)
  "Return FORM's value, nil when it signals: a menu item's condition."
  (condition-case nil (eval form t) (error nil)))

(defun lme-popup--keymap-item (binding &optional path)
  "Return the menu item of BINDING, an entry of a menu keymap, or nil.
PATH is the list of keys reaching BINDING, kept in the item's :path.  A
separator becomes `separator'; an item hidden by its :visible form, or an
entry that is not a menu item, nothing."
  (pcase binding
    ((pred stringp)
     (when (string-prefix-p "--" binding) 'separator))
    (`(menu-item ,name . ,rest)
     (let ((name (if (stringp name) name (lme-popup--eval name)))
           (real (car rest))
           (props (cdr rest)))
       (cond
        ((and (plist-member props :visible)
              (not (lme-popup--eval (plist-get props :visible))))
         nil)
        ((and (stringp name) (string-prefix-p "--" name)) 'separator)
        ((not (stringp name)) nil)
        (t
         (let* ((real (if-let* ((filter (plist-get props :filter)))
                          (funcall filter real)
                        real))
                (submenu (and real (keymapp real) (not (commandp real))
                              (lme-popup--keymap-map real)))
                (button (plist-get props :button)))
           `(:label ,name
             :action ,(unless submenu real)
             :path ,path
             ,@(when submenu
                 `(:submenu ,(let ((map submenu))
                               (lambda () (lme-popup-keymap-items map path)))))
             :enabled ,(and (or submenu real)
                            (or (not (plist-member props :enable))
                                (lme-popup--eval (plist-get props :enable)))
                            t)
             ,@(when button `(:checked ,(and (lme-popup--eval (cdr button)) t)))
             ,@(when-let* ((keys (plist-get props :keys)))
                 `(:key ,(substitute-command-keys keys)))))))))
    (`(,(and name (pred stringp)) . ,rest)
     (if (string-prefix-p "--" name)
         'separator
       (let ((real (if (stringp (car-safe rest)) (cdr rest) rest)))
         (lme-popup--keymap-item `(menu-item ,name ,real) path))))))

(defun lme-popup--keymap-map (binding)
  "Return BINDING as a keymap, following a symbol to its function."
  (let ((map (indirect-function binding)))
    (and (keymapp map) map)))

(defun lme-popup-keymap-items (keymap &optional prefix)
  "Return the items of menu KEYMAP, as `lme-popup-menu' takes them.
Items come in the keymap's order, as Emacs shows them, each with the
:path of keys reaching it after PREFIX; separators are not doubled nor
left at either end."
  (let ((items nil) (seen nil))
    (map-keymap (lambda (key binding)
                  (unless (memq key seen)
                    (push key seen)
                    (when-let* ((item (lme-popup--keymap-item
                                       binding (append prefix (list key)))))
                      (push item items))))
                (lme-popup--keymap-map keymap))
    (let ((cleaned nil))
      (dolist (item (nreverse items))
        (unless (and (eq item 'separator)
                     (or (null cleaned) (eq (car cleaned) 'separator)))
          (push item cleaned)))
      (when (eq (car cleaned) 'separator) (pop cleaned))
      (nreverse cleaned))))

(defun lme-popup--frame-xy (position)
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

(defun lme-popup-keymap-menu (keymap &optional position)
  "Pop up menu KEYMAP at POSITION as a Mac OS 9 menu and run what is chosen.
POSITION is an event, a posn, a buffer position or nil for the mouse."
  (pcase-let ((`(,x . ,y) (lme-popup--frame-xy position)))
    (lme-popup-menu (lme-popup-keymap-items keymap) (1+ x) (1+ y))))

(defun lme-popup--local-right-click-p (position)
  "Return non-nil when a right click at POSITION has a binding of its own.
That is a [mouse-3] command of the buffer or text there, such as the
project tree's or the agents sidebar's menu, which then opens instead."
  (let ((binding (key-binding [mouse-3] t nil position)))
    (and binding (not (eq binding (keymap-lookup global-map "<mouse-3>"))))))

(defun lme-popup-context-menu (event)
  "Pop up the context menu for mouse EVENT, as Mac OS 9's contextual menus.
The menu is Emacs's own, `context-menu-map', drawn as a Platinum menu.
Where the click has a right-click command of its own, that runs on the
release instead; where the context menu is empty, nothing opens."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (lme-popup--local-right-click-p position)
      (when (windowp window)
        (select-window window))
      (lme-popup-keymap-menu (context-menu-map event) event))))

(defun lme-popup--pane-items (menu)
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

(defun lme-popup--answer-paths (items)
  "Return ITEMS, from a menu keymap, each answering with its key path.
The items of their submenus answer the same way, when they open."
  (dolist (item items items)
    (when (consp item)
      (if-let* ((inner (copy-sequence item))
                ((plist-get item :submenu)))
          (plist-put item :submenu
                     (lambda () (lme-popup--answer-paths (lme-popup--submenu inner))))
        (plist-put item :value (plist-get item :path))))))

(defun lme-popup--x-menu-p (menu)
  "Return non-nil when MENU, as `x-popup-menu' takes it, can be drawn here:
a keymap, a list of keymaps, or a title and its panes."
  (or (keymapp menu)
      (and (consp menu) (keymapp (car menu)))
      (and (consp menu) (stringp (car menu)) (seq-every-p #'consp (cdr menu)))))

(defun lme-popup--x-items (menu)
  "Return the items of MENU as `x-popup-menu' takes it.
Keymap items answer with their key path, pane items with their value."
  (cond ((keymapp menu)
         (lme-popup--answer-paths (lme-popup-keymap-items menu)))
        ((keymapp (car menu))
         (let ((items nil))
           (dolist (map menu)
             (let ((more (lme-popup--x-items map)))
               (when (and items more) (push 'separator items))
               (setq items (append (reverse more) items))))
           (nreverse items)))
        (t (lme-popup--pane-items menu))))

(defun lme-popup--x-position (position)
  "Return POSITION, as `x-popup-menu' takes it, as (X . Y) in its frame.
Offsets from a window are taken from its text area, where `popup-menu'
measures a click or point."
  (pcase position
    ('t (cdr (mouse-pixel-position)))
    (`((,x ,y) ,(and window (pred windowp)))
     (let ((edges (window-body-pixel-edges window)))
       (cons (+ (car edges) x) (+ (nth 1 edges) y))))
    (`((,x ,y) ,(pred framep)) (cons x y))
    (_ (lme-popup--frame-xy position))))

(defun lme-popup--x-popup-menu (x-popup-menu position menu)
  "Draw MENU as a Mac OS 9 menu where `x-popup-menu' would draw a native one.
X-POPUP-MENU is the original, called without a POSITION (it then only
computes key equivalents), where no popup can be drawn, and for menus
it cannot read.  The value is what the original returns: the key path of
the item chosen from a keymap, the value of one chosen from panes, nil
when cancelled."
  (if (not (and position (lme-popup-available-p) (lme-popup--x-menu-p menu)))
      (funcall x-popup-menu position menu)
    (pcase-let ((`(,x . ,y) (lme-popup--x-position position)))
      (plist-get (lme-popup-choose (lme-popup--x-items menu) (1+ x) (1+ y)) :value))))

(defun lme-popup-enable ()
  "Draw right-click menus, and every menu `x-popup-menu' pops up, as Mac
OS 9's menus.  A right click opens Emacs's context menu, or the right-click
command of the place clicked."
  ;; The palette exists only now, after `lme-setup-platinum': at load
  ;; time and at the first theme's loading, the item faces kept the menu
  ;; bar's taller padding.
  (lme-popup-apply-faces)
  (keymap-global-set "<down-mouse-3>" #'lme-popup-context-menu)
  (keymap-global-set "<mouse-3>" #'ignore)
  (advice-add 'x-popup-menu :around #'lme-popup--x-popup-menu))

;;;; Alerts

(defconst lme-popup-alert-width 420
  "Width in pixels of an alert.")

(defun lme-popup-draw-button (palette label width &optional default pressed)
  "Return an SVG of a push button WIDTH pixels wide reading LABEL in PALETTE.
A DEFAULT button is ringed, as the one Return presses; a PRESSED one is
drawn pushed in."
  (let* ((ring (if default 4 0))
         (height (+ 22 (* 2 ring)))
         (svg (svg-create (+ width (* 2 ring)) height))
         (chrome (plist-get palette :chrome))
         (fill (if pressed (lme-tab-bar--blend chrome (plist-get palette :shade) 0.5) chrome)))
    (when default
      (svg-rectangle svg 1.5 1.5 (+ width (* 2 ring) -3) (- height 3) :rx 6
                     :fill "none" :stroke (plist-get palette :frame) :stroke-width 3))
    (svg-rectangle svg (+ ring 0.5) (+ ring 0.5) (1- width) 21 :rx 4
                   :fill fill :stroke (plist-get palette :frame) :stroke-width 1)
    (svg-line svg (+ ring 4) (+ ring 1.5) (+ ring width -4) (+ ring 1.5)
              :stroke (plist-get palette (if pressed :shade :light)) :stroke-width 1)
    (svg-line svg (+ ring 4) (+ ring 20.5) (+ ring width -4) (+ ring 20.5)
              :stroke (plist-get palette (if pressed :light :shade)) :stroke-width 1)
    (svg-text svg label
              :x (+ ring (/ width 2.0)) :y (+ ring 15.5) :text-anchor "middle"
              :font-family (or (face-attribute 'lme-platinum-title :family nil t) "Geneva")
              :font-size 12 :font-weight "bold"
              :fill (plist-get palette :text))
    svg))

(defun lme-popup--button-width (label)
  "Return the width of the push button reading LABEL: room for it, at least 70."
  (max 70 (+ 28 (string-pixel-width (propertize label 'face 'lme-platinum-title)))))

(defun lme-popup-alert-text (icon message detail buttons pressed)
  "Return the text of an alert: ICON, MESSAGE, DETAIL and its BUTTONS.
BUTTONS is a list of (LABEL VALUE ROLE), ROLE `default' or `cancel' or
nil, laid left to right ending at the alert's right; PRESSED is the
VALUE of the one held down."
  (let* ((images (mapcar (pcase-lambda (`(,label ,value ,role))
                           (let ((width (lme-popup--button-width label)))
                             (cons (propertize
                                    " " 'display
                                    (svg-image (lme-popup-draw-button
                                                lme-platinum--palette label width
                                                (eq role 'default) (equal pressed value))
                                               :ascent 'center :scale 1)
                                    'lme-popup-button value)
                                   (+ width (if (eq role 'default) 8 0)))))
                         buttons))
         (row (+ (apply #'+ (mapcar #'cdr images)) (* 10 (1- (length images))))))
    (concat (propertize " " 'display '(space :height (10))) "\n"
            ;; The icon stands in the left margin, inset from the frame by
            ;; its own margin.
            (propertize " " 'display
                        `((margin left-margin)
                          ,(create-image (expand-file-name (format "icons/os9/%s.svg" icon)
                                                           lme-directory)
                                         nil nil :ascent 80 :scale 1 :margin '(16 . 0))))
            (propertize message 'face '(:inherit lme-platinum-title :weight bold))
            "\n"
            (when detail
              (concat (propertize " " 'display '(space :height (6))) "\n"
                      (propertize detail 'face 'lme-platinum-list) "\n"))
            (propertize " " 'display '(space :height (16))) "\n"
            (propertize " " 'display `(space :align-to (- right (,(+ row 4)))))
            (mapconcat #'car images (propertize " " 'display '(space :width (10))))
            "\n"
            (propertize " " 'display '(space :height (12))))))

(defun lme-popup--button-at (event frame)
  "Return the value of the alert button under EVENT in FRAME, or nil."
  (when-let* ((position (lme-popup--over event frame))
              (point (posn-point position)))
    (with-current-buffer (window-buffer (posn-window position))
      (get-text-property point 'lme-popup-button))))

(defun lme-popup-open-alert (icon message buttons &optional detail)
  "Show an alert of ICON, MESSAGE, BUTTONS and DETAIL, see `lme-popup-alert'.
It sits over the upper middle of the selected frame, as Mac OS 9's alerts
did.  Return its frame; nothing is read from the user."
  (let* ((parent (selected-frame))
         (popup (lme-popup--frame parent))
         (window (frame-root-window popup)))
    (with-current-buffer (window-buffer window)
      (setq-local word-wrap t truncate-lines nil left-margin-width 9 right-margin-width 2))
    (set-window-margins window 9 2)
    (lme-popup--fill popup (lme-popup-alert-text icon message detail buttons nil))
    (lme-popup--size popup lme-popup-alert-width 400)
    (lme-popup--size popup lme-popup-alert-width
                     (cdr (window-text-pixel-size window nil nil nil nil)))
    (lme-popup--show popup (/ (- (frame-pixel-width parent) lme-popup-alert-width) 2)
                     (/ (frame-pixel-height parent) 5))
    popup))

(defun lme-popup-alert (icon message buttons &optional detail)
  "Show a Mac OS 9 alert and return the value of the button chosen.
ICON is `caution', `stop' or `note'; MESSAGE is the question, DETAIL a
line or two under it.  BUTTONS is a list of (LABEL VALUE ROLE [KEY]):
ROLE `default' marks the button Return presses, `cancel' the one Escape
does; KEY is an event pressing it too, as Command-D pressed Don't Save."
  (let ((default (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'default)) buttons)))
        (cancel (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'cancel)) buttons))))
    (if (not (lme-popup-available-p))
        (lme-popup--alert-fallback message buttons detail cancel)
      (let* ((popup (lme-popup-open-alert icon message buttons detail))
             (pressed nil) (held nil) (choice cancel))
        (unwind-protect
            (cl-flet ((redraw ()
                        (lme-popup--fill popup (lme-popup-alert-text icon message detail
                                                                     buttons pressed))
                        (redisplay t)))
              (track-mouse
                (catch 'done
                  (while t
                    (let ((event (lme-popup--read)))
                      (cond
                       ((seq-find (lambda (button) (eql (nth 3 button) event)) buttons)
                        (setq pressed (nth 1 (seq-find (lambda (button)
                                                         (eql (nth 3 button) event))
                                                       buttons)))
                        (redraw) (sit-for 0.12)
                        (setq choice pressed) (throw 'done nil))
                       ((and (lme-popup--cancel-p event) cancel)
                        (setq choice cancel) (throw 'done nil))
                       ((and (lme-popup--confirm-p event) default)
                        ;; The default button blinks as it is pressed.
                        (setq pressed default) (redraw) (sit-for 0.12)
                        (setq choice default) (throw 'done nil))
                       ((lme-popup--press-p event)
                        (when (setq held (lme-popup--button-at event popup))
                          (setq pressed held) (redraw)))
                       ((and held (mouse-movement-p event))
                        (let ((over (equal (lme-popup--button-at event popup) held)))
                          (unless (eq over (and pressed t))
                            (setq pressed (and over held)) (redraw))))
                       ((lme-popup--release-p event)
                        (when (and held (equal (lme-popup--button-at event popup) held))
                          (setq choice held) (throw 'done nil))
                        (setq held nil pressed nil) (redraw))))))))
          (lme-popup--dispose popup))
        choice))))

(defun lme-popup--alert-fallback (message buttons detail cancel)
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

(lme-popup-apply-faces)
(add-hook 'enable-theme-functions #'lme-popup-apply-faces 95)

(provide 'lme-popup)
;;; lme-popup.el ends here
