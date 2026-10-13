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
  "Read the next event, a quit included, without quitting."
  (let* ((inhibit-quit t)
         (event (read-event)))
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
An item without :enabled is enabled; one with :enabled nil is not."
  (and (not (lme-popup--separator-p item))
       (plist-get item :action)
       (or (not (plist-member item :enabled)) (plist-get item :enabled))))

(defun lme-popup--keyboard-p (keys)
  "Return non-nil when KEYS is typed on the keyboard alone.
Mouse, wheel and menu bar events do not count."
  (seq-every-p (lambda (event)
                 (not (and (symbolp event)
                           (string-match-p "mouse\\|wheel\\|drag\\|menu-bar\\|tab-bar\\|tool-bar"
                                           (symbol-name event)))))
               keys))

(defun lme-popup--key (item)
  "Return the key equivalent shown for menu ITEM, or nil.
It is the first key sequence typed on the keyboard that runs the item's
command, as Mac OS 9 showed Command keys beside items."
  (or (plist-get item :key)
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

(defun lme-popup--menu-index (event frame items)
  "Return the index of the enabled item of ITEMS under EVENT in menu FRAME."
  (when-let* ((position (lme-popup--over event frame))
              (point (posn-point position)))
    (let ((index (with-current-buffer (window-buffer (posn-window position))
                   (1- (line-number-at-pos point)))))
      (when (and (< -1 index (length items)) (lme-popup--enabled-p (nth index items)))
        index))))

(defun lme-popup--step (items from direction)
  "Return the index of the next enabled item of ITEMS from FROM in DIRECTION."
  (let* ((count (length items))
         (index (or from (if (> direction 0) -1 count))))
    (cl-loop repeat count
             do (setq index (mod (+ index direction) count))
             when (lme-popup--enabled-p (nth index items)) return index)))

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

(defun lme-popup-menu (items &optional x y align)
  "Pop up ITEMS as a Mac OS 9 menu and run the action of the one chosen.
X and Y place the menu in the selected frame, by default under the
mouse; ALIGN `right' puts its right edge at X.  Each item is a plist
\(:label STRING :action FUNCTION [:enabled BOOL] [:checked BOOL]
[:key STRING]) or the symbol `separator'.  Return the item chosen, or
nil when the menu was cancelled."
  (if (not (lme-popup-available-p))
      (lme-popup--menu-fallback items)
    (let* ((popup (lme-popup-open-menu items x y align))
           (width (lme-popup--menu-width items))
           (highlight nil)
           (choice nil))
      (unwind-protect
          (progn
            (cl-flet ((redraw ()
                        (lme-popup--fill popup (lme-popup-menu-text items width highlight))
                        (redisplay t)))
              (track-mouse
                (catch 'done
                  (while t
                    (let ((event (lme-popup--read)))
                      (cond
                       ((lme-popup--cancel-p event) (throw 'done nil))
                       ((lme-popup--confirm-p event)
                        (when highlight (setq choice (nth highlight items)))
                        (throw 'done nil))
                       ((memq event '(down ?\C-n))
                        (setq highlight (lme-popup--step items highlight 1)) (redraw))
                       ((memq event '(up ?\C-p))
                        (setq highlight (lme-popup--step items highlight -1)) (redraw))
                       ((mouse-movement-p event)
                        (let ((index (lme-popup--menu-index event popup items)))
                          (unless (eql index highlight)
                            (setq highlight index)
                            (redraw))))
                       ((lme-popup--press-p event)
                        (unless (lme-popup--over event popup)
                          (throw 'done nil)))
                       ((lme-popup--release-p event)
                        (when-let* ((index (lme-popup--menu-index event popup items)))
                          ;; As Mac OS 9 did, the chosen item blinks once.
                          (setq highlight nil) (redraw) (sit-for 0.05)
                          (setq highlight index) (redraw) (sit-for 0.05)
                          (setq choice (nth index items))
                          (throw 'done nil))))))))))
        (lme-popup--dispose popup))
      (lme-popup--run choice))))

(defun lme-popup--run (item)
  "Run the action of menu ITEM, when there is one; return ITEM."
  (when-let* ((action (and item (plist-get item :action))))
    (if (commandp action) (call-interactively action) (funcall action)))
  item)

(defun lme-popup--menu-fallback (items)
  "Choose among ITEMS in the minibuffer, as a menu where none can be drawn."
  (let* ((choices (seq-filter #'lme-popup--enabled-p items))
         (label (completing-read "Menu: " (mapcar (lambda (item) (plist-get item :label)) choices)
                                 nil t)))
    (lme-popup--run (seq-find (lambda (item) (equal (plist-get item :label) label)) choices))))

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
