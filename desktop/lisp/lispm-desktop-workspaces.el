;;; lispm-desktop-workspaces.el --- Moving between the editor's workspaces  -*- lexical-binding: t; -*-

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

;; The editor's workspaces, one per project, are chosen from a floating
;; panel of thumbnails, each drawing a workspace as it was last seen: its
;; title strip, its sidebar and its windows, every window's text as lines
;; in their own colours.  Nothing is captured from the screen; a thumbnail
;; is drawn from the workspace's window state and its buffers' text:
;;
;;   workspace --window state--> boxes --buffer text--> thumbnail (SVG)
;;
;; The editor's Project menu opens the panel, as does Command-` (M-`);
;; in it Tab, Command-`, C-f, C-b, C-n, C-p and the arrows move, a digit
;; or Return or a click opens the workspace chosen, Escape keeps the one
;; in front.  C-<tab> and
;; C-S-<tab> go straight to the next and the previous workspace, and so do
;; Emacs's own commands for the next and previous tab, skipping the
;; desktop's and the browser's.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-editor)

(declare-function tabspaces-switch-or-create-workspace "tabspaces" (&optional workspace))
(declare-function tabspaces-close-workspace "tabspaces" ())

(defconst lispm-desktop-workspaces-thumbnail-width 220
  "Width in pixels of a workspace's thumbnail.")

(defconst lispm-desktop-workspaces-columns 4
  "Thumbnails a row of the panel holds.")

(defconst lispm-desktop-workspaces-gap 20
  "Pixels between the thumbnails, and around them.")

;;;; Going to a workspace

(defun lispm-desktop-workspaces-select (name)
  "Bring the editor in front with workspace NAME."
  (lispm-desktop-editor--focus)
  (with-selected-frame (lispm-desktop-editor-frame)
    (unless (equal name (lispm-desktop-editor--current-workspace))
      (tab-bar-switch-to-tab name))))

(defun lispm-desktop-workspaces--step (step)
  "Go STEP workspaces on from the one in front, wrapping around."
  (let* ((names (lispm-desktop-editor-workspaces))
         (index (or (seq-position names (lispm-desktop-editor--current-workspace)) 0)))
    (when names
      (lispm-desktop-workspaces-select (nth (mod (+ index step) (length names)) names)))))

(defun lispm-desktop-workspaces-next (&optional count)
  "Go to the next workspace of the editor, or the COUNTth."
  (interactive "p")
  (lispm-desktop-workspaces--step (or count 1)))

(defun lispm-desktop-workspaces-previous (&optional count)
  "Go to the previous workspace of the editor, or the COUNTth back."
  (interactive "p")
  (lispm-desktop-workspaces--step (- (or count 1))))

;;;; Layouts

(defun lispm-desktop-workspaces--state-boxes (node x y)
  "Return the windows of window state NODE, its top left at X, Y, as boxes.
A box is (X Y WIDTH HEIGHT BUFFER-NAME START SIDE), in pixels of the
frame the state was taken from."
  (let ((props (cdr node)))
    (pcase (car node)
      ('leaf
       (let ((buffer (alist-get 'buffer props)))
         (list (list x y (alist-get 'pixel-width props) (alist-get 'pixel-height props)
                     (car buffer) (alist-get 'start (cdr buffer))
                     (alist-get 'window-side (alist-get 'parameters props))))))
      ((and type (or 'vc 'hc))
       (let ((offset 0))
         (mapcan (lambda (child)
                   (prog1 (lispm-desktop-workspaces--state-boxes
                           child
                           (if (eq type 'hc) (+ x offset) x)
                           (if (eq type 'vc) (+ y offset) y))
                     (setq offset (+ offset (alist-get (if (eq type 'hc) 'pixel-width 'pixel-height)
                                                       (cdr child))))))
                 (seq-filter (lambda (entry) (memq (car-safe entry) '(leaf vc hc))) props)))))))

(defun lispm-desktop-workspaces--live-layout (frame)
  "Return the layout of FRAME's windows as they are.
See `lispm-desktop-workspaces--layout'."
  (pcase-let* ((`(,left ,top ,right ,bottom) (window-pixel-edges (frame-root-window frame))))
    (cl-list* (- right left) (- bottom top)
              (mapcar (lambda (window)
                        (pcase-let ((`(,l ,tp ,r ,b) (window-pixel-edges window)))
                          (list (- l left) (- tp top) (- r l) (- b tp)
                                (buffer-name (window-buffer window)) (window-start window)
                                (window-parameter window 'window-side))))
                      (window-list frame 'no-mini)))))

(defun lispm-desktop-workspaces--layout (tab frame)
  "Return (WIDTH HEIGHT . BOXES): workspace TAB of FRAME, laid out.
The one in front is read from its windows, any other from its window state."
  (if (eq (car tab) 'current-tab)
      (lispm-desktop-workspaces--live-layout frame)
    (when-let* ((root (cdr (alist-get 'ws tab))))
      (cl-list* (alist-get 'pixel-width (cdr root)) (alist-get 'pixel-height (cdr root))
                (lispm-desktop-workspaces--state-boxes root 0 0)))))

;;;; Thumbnails

(defun lispm-desktop-workspaces--face-color (face)
  "Return the foreground FACE gives text, or nil: a face, a list of them, or
an anonymous face."
  (cond ((null face) nil)
        ((and (symbolp face) (facep face)) (face-foreground face nil t))
        ((and (consp face) (keywordp (car face))) (plist-get face :foreground))
        ((consp face) (seq-some #'lispm-desktop-workspaces--face-color face))))

(defun lispm-desktop-workspaces--lines (buffer start count)
  "Return COUNT lines of BUFFER from START as (INDENT LENGTH COLOR).
INDENT and LENGTH are in columns; COLOR is that of the line's first text."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (max (point-min) (min (or start 1) (point-max))))
        (forward-line 0)
        (when (bound-and-true-p font-lock-mode)
          (ignore-errors
            (font-lock-ensure (point) (save-excursion (forward-line count) (point)))))
        (let ((default (face-foreground 'default nil t))
              (lines nil))
          (dotimes (_ count)
            (unless (eobp)
              (let* ((end (line-end-position))
                     (indent (progn (skip-chars-forward " \t" end) (current-column)))
                     (text (point))
                     (length (- (progn (goto-char end) (current-column)) indent)))
                (push (list indent length
                            (or (lispm-desktop-workspaces--face-color (get-char-property text 'face))
                                default))
                      lines))
              (forward-line 1)))
          (nreverse lines))))))

(defun lispm-desktop-workspaces--draw-title (svg x y width height)
  "Draw a title bar WIDTH by HEIGHT at X, Y: platinum, pinstriped, titled."
  (svg-rectangle svg x y width height :fill (lispm-desktop-platinum--color :chrome))
  (when (> height 3)
    (dolist (row '(0.35 0.65))
      (svg-line svg (+ x 3) (+ y (* height row)) (- (+ x width) 3) (+ y (* height row))
                :stroke (lispm-desktop-platinum--color :shade) :stroke-width 0.6)))
  (let ((title (min (* width 0.4) 70)))
    (svg-rectangle svg (+ x (/ (- width title) 2)) y title height
                   :fill (lispm-desktop-platinum--color :chrome))
    (svg-rectangle svg (+ x (/ (- width title) 2) 4) (+ y (* height 0.35))
                   (- title 8) (max 1 (* height 0.3)) :fill (lispm-desktop-platinum--color :text))))

(defun lispm-desktop-workspaces--draw-box (svg box scale)
  "Draw window BOX of a workspace into SVG at SCALE."
  (pcase-let* ((`(,x ,y ,w ,h ,name ,start ,_side) box)
               (buffer (get-buffer name))
               (x (* x scale)) (y (* y scale)) (w (* w scale)) (h (* h scale))
               (bar (max 2.5 (* lispm-desktop-platinum-title-height scale)))
               (mode (and buffer (buffer-local-value 'major-mode buffer))))
    (cond
     ((or (null buffer) (provided-mode-derived-p mode 'lispm-desktop-mode)
          (provided-mode-derived-p mode 'lispm-desktop-empty-mode))
      (svg-rectangle svg x y w h :fill (lispm-desktop-platinum--color :desktop)))
     ((provided-mode-derived-p mode 'lispm-desktop-editor-strip-mode)
      (lispm-desktop-workspaces--draw-title svg x y w bar)
      (svg-rectangle svg x (+ y bar) w (- h bar) :fill (lispm-desktop-platinum--color :header))
      (dotimes (i 3)
        (svg-rectangle svg (+ x 3 (* i 22 scale 4)) (+ y bar 2) (* 18 scale 4) (- h bar 4)
                       :fill (lispm-desktop-platinum--color (if (zerop i) :content :chrome)))))
     ((provided-mode-derived-p mode 'xwidget-webkit-mode)
      (svg-rectangle svg x y w h :fill (lispm-desktop-platinum--color :content))
      (svg-rectangle svg (+ x (* w 0.1)) (+ y (* h 0.15)) (* w 0.8) (* h 0.12)
                     :fill (lispm-desktop-platinum--color :chrome))
      (svg-rectangle svg (+ x (* w 0.1)) (+ y (* h 0.35)) (* w 0.8) (* h 0.5)
                     :fill (lispm-desktop-platinum--color :header)))
     (t
      (svg-rectangle svg x y w h :fill (lispm-desktop-platinum--color :content))
      (lispm-desktop-workspaces--draw-title svg x y w bar)
      (svg-rectangle svg x (- (+ y h) bar) w bar :fill (lispm-desktop-platinum--color :chrome))
      (let* ((line (* (frame-char-height) scale))
             (column (* (frame-char-width) scale))
             (rows (max 0 (floor (/ (- h (* 2 bar) 2) line)))))
        (seq-do-indexed
         (pcase-lambda (`(,indent ,length ,color) index)
           (let ((left (+ x 2 (* indent column)))
                 (right (min (+ x w -2) (+ x 2 (* (+ indent length) column)))))
             (when (> right left)
               (svg-rectangle svg left (+ y bar 1 (* index line) (* line 0.3))
                              (- right left) (max 0.8 (* line 0.45))
                              :fill (or color (lispm-desktop-platinum--color :text))))))
         (lispm-desktop-workspaces--lines buffer start rows)))))
    (svg-rectangle svg x y w h :fill "none" :stroke (lispm-desktop-platinum--color :frame)
                   :stroke-width 0.6)))

(defun lispm-desktop-workspaces--thumbnail (layout selected)
  "Return the thumbnail of workspace LAYOUT, ringed when SELECTED.
LAYOUT is a value of `lispm-desktop-workspaces--layout'."
  (pcase-let* ((`(,width ,height . ,boxes) layout)
               (scale (/ (float lispm-desktop-workspaces-thumbnail-width) (max 1 width)))
               (thumb-height (round (* height scale)))
               (ring 4)
               (svg (svg-create (+ lispm-desktop-workspaces-thumbnail-width (* 2 ring))
                                (+ thumb-height (* 2 ring)))))
    (when selected
      (svg-rectangle svg 0.5 0.5 (+ lispm-desktop-workspaces-thumbnail-width (* 2 ring) -1)
                     (+ thumb-height (* 2 ring) -1)
                     :fill "none" :stroke (lispm-desktop-platinum--color :accent) :stroke-width 3
                     :rx 4))
    (let ((inner (svg-node svg 'g :transform (format "translate(%d %d)" ring ring))))
      (svg-rectangle inner 0 0 lispm-desktop-workspaces-thumbnail-width thumb-height
                     :fill (lispm-desktop-platinum--color :chrome))
      (dolist (box boxes)
        (lispm-desktop-workspaces--draw-box inner box scale))
      (svg-rectangle inner 0 0 lispm-desktop-workspaces-thumbnail-width thumb-height
                     :fill "none" :stroke (lispm-desktop-platinum--color :frame) :stroke-width 1))
    svg))

;;;; The panel

(defvar lispm-desktop-workspaces--images nil
  "The panel's thumbnails while it is open: (INDEX . SELECTED) to an image.")

(defun lispm-desktop-workspaces--image (index layout selected)
  "Return the thumbnail image of workspace INDEX, of LAYOUT, maybe SELECTED."
  (let ((key (cons index selected)))
    (or (cdr (assoc key lispm-desktop-workspaces--images))
        (let ((image (svg-image (lispm-desktop-workspaces--thumbnail layout selected)
                                :ascent 'center :scale 1)))
          (push (cons key image) lispm-desktop-workspaces--images)
          image))))

(defconst lispm-desktop-workspaces--actions
  '(("Open Project…" project-switch-project)
    ("New Workspace…" tabspaces-switch-or-create-workspace)
    ("Close Workspace" tabspaces-close-workspace))
  "The panel's buttons: (LABEL COMMAND), as the Project menu held them.")

(defun lispm-desktop-workspaces--column-left (column)
  "Return the pixel left of the panel's thumbnail COLUMN."
  (+ lispm-desktop-workspaces-gap (* column (+ lispm-desktop-workspaces-thumbnail-width 8 lispm-desktop-workspaces-gap))))

(defun lispm-desktop-workspaces--label (name current selected)
  "Return the label under workspace NAME's thumbnail.
The workspace in front is checked; the one SELECTED is inverted."
  (propertize (concat " " (if current "✓ " "") name " ")
              'face (if selected 'lispm-desktop-label-selected 'lispm-desktop-label)))

(defun lispm-desktop-workspaces--text (names layouts current selected width)
  "Return the panel's text: the thumbnails of NAMES in rows, their labels,
and the buttons.  LAYOUTS are their layouts, CURRENT the name of the one
in front, SELECTED the index chosen, WIDTH the panel's in pixels."
  (let ((rows (seq-partition (seq-map-indexed #'cons names) lispm-desktop-workspaces-columns)))
    (concat
     (lispm-desktop-platinum--draw-title-bar width t (lispm-desktop-platinum-icon 'emacs) "Workspaces" nil
                                             '(off off off))
     "\n"
     (propertize " " 'display `(space :height (,lispm-desktop-workspaces-gap))) "\n"
     (mapconcat
      (lambda (row)
        (concat
         (mapconcat
          (pcase-lambda (`(,_name . ,index))
            (concat (propertize " " 'display
                                `(space :align-to (,(lispm-desktop-workspaces--column-left
                                                     (mod index lispm-desktop-workspaces-columns)))))
                    (propertize " " 'display (lispm-desktop-workspaces--image index (nth index layouts)
                                                                              (= index selected))
                                'lispm-desktop-workspace index 'pointer 'hand)))
          row "")
         "\n"
         (propertize " " 'display '(space :height (6))) "\n"
         (mapconcat
          (pcase-lambda (`(,name . ,index))
            (let ((label (lispm-desktop-workspaces--label name (equal name current) (= index selected))))
              (concat (propertize " " 'display
                                  `(space :align-to
                                          (,(- (+ (lispm-desktop-workspaces--column-left
                                                   (mod index lispm-desktop-workspaces-columns))
                                                  (/ (+ lispm-desktop-workspaces-thumbnail-width 8) 2))
                                               (/ (string-pixel-width label) 2)))))
                      (propertize label 'lispm-desktop-workspace index 'pointer 'hand))))
          row "")
         "\n"
         (propertize " " 'display `(space :height (,lispm-desktop-workspaces-gap))) "\n"))
      rows "")
     (propertize " " 'display `(space :align-to (,lispm-desktop-workspaces-gap)))
     (lispm-desktop-workspaces--buttons)
     "\n"
     (propertize " " 'display '(space :height (6))) "\n"
     (propertize " " 'display `(space :align-to (,lispm-desktop-workspaces-gap)))
     (lispm-desktop-workspaces--hint)
     "\n"
     (propertize " " 'display '(space :height (12))))))

(defun lispm-desktop-workspaces--buttons ()
  "Return the panel's row of buttons."
  (mapconcat (pcase-lambda (`(,label ,command))
               (propertize (concat " " label " ")
                           'face 'lispm-desktop-platinum-button 'lispm-desktop-workspace-action command
                           'pointer 'hand))
             lispm-desktop-workspaces--actions " "))

(defun lispm-desktop-workspaces--hint ()
  "Return the line telling the panel's keys."
  (propertize "⇥ next   ⏎ open   1–9 choose   esc keep" 'face 'lispm-desktop-platinum-toolbar-label))

(defun lispm-desktop-workspaces--width (columns)
  "Return the panel's width in pixels for COLUMNS of thumbnails: wide
enough for its buttons and its keys too."
  (max (lispm-desktop-workspaces--column-left columns)
       (+ (* 2 lispm-desktop-workspaces-gap)
          (max (string-pixel-width (lispm-desktop-workspaces--buttons))
               (string-pixel-width (lispm-desktop-workspaces--hint))))))

(defun lispm-desktop-workspaces--target (event frame)
  "Return what EVENT is over in panel FRAME: (workspace . INDEX), (action .
COMMAND), or nil."
  (when-let* ((position (lispm-desktop-popup--over event frame))
              (point (posn-point position)))
    (with-current-buffer (window-buffer (posn-window position))
      (cond ((get-text-property point 'lispm-desktop-workspace)
             (cons 'workspace (get-text-property point 'lispm-desktop-workspace)))
            ((get-text-property point 'lispm-desktop-workspace-action)
             (cons 'action (get-text-property point 'lispm-desktop-workspace-action)))))))

(defun lispm-desktop-workspaces--move (index step count)
  "Return INDEX moved STEP places among COUNT, wrapping around."
  (mod (+ index step) count))

(defun lispm-desktop-workspaces-choose (&optional step x y)
  "Show the workspaces' panel and return what was chosen, or nil.
The value is (workspace . NAME) or (action . COMMAND).  The workspace
STEP places from the one in front is chosen first.  The panel drops at
X, Y of the screen frame, or stands over its upper middle."
  (let* ((screen (lispm-desktop-apps-screen))
         (frame (lispm-desktop-editor-frame))
         (tabs (seq-filter #'lispm-desktop-editor--workspace-tab-p (funcall tab-bar-tabs-function frame)))
         (names (mapcar (lambda (tab) (alist-get 'name tab)) tabs))
         (layouts (mapcar (lambda (tab) (or (lispm-desktop-workspaces--layout tab frame) '(1 1)))
                          tabs))
         (current (lispm-desktop-editor--current-workspace))
         (count (length names))
         (selected (lispm-desktop-workspaces--move (or (seq-position names current) 0) (or step 0)
                                                   (max 1 count)))
         (columns (min lispm-desktop-workspaces-columns (max 1 count)))
         (width (lispm-desktop-workspaces--width columns))
         (lispm-desktop-workspaces--images nil)
         (popup (lispm-desktop-popup--frame screen))
         (choice nil))
    (when names
      (unwind-protect
          (cl-flet ((redraw ()
                      (lispm-desktop-popup--fill popup (lispm-desktop-workspaces--text names layouts current
                                                                                       selected width))
                      (redisplay t)))
            (redraw)
            (lispm-desktop-popup--size popup width 400)
            (lispm-desktop-popup--size popup width (cdr (window-text-pixel-size
                                                         (frame-root-window popup) nil nil nil nil)))
            (lispm-desktop-popup--show popup (or x (/ (- (frame-pixel-width screen) width) 2))
                                       (or y (/ (frame-pixel-height screen) 6)))
            (track-mouse
              (catch 'done
                (while t
                  (let ((event (lispm-desktop-popup--read)))
                    (cond
                     ((lispm-desktop-popup--cancel-p event) (throw 'done nil))
                     ((or (lispm-desktop-popup--confirm-p event) (eq event ?\s))
                      (setq choice (cons 'workspace (nth selected names)))
                      (throw 'done nil))
                     ;; Walking the menu bar, Left and Right go to the
                     ;; menus beside; Tab still moves among workspaces.
                     ((and lispm-desktop-popup-menu-bar-step (memq event '(right ?\C-f)))
                      (throw 'lispm-desktop-popup-menu-bar-step 1))
                     ((and lispm-desktop-popup-menu-bar-step (memq event '(left ?\C-b)))
                      (throw 'lispm-desktop-popup-menu-bar-step -1))
                     ((memq event '(tab ?\t C-tab ?\M-` right ?\C-f))
                      (setq selected (lispm-desktop-workspaces--move selected 1 count)) (redraw))
                     ((memq event '(backtab S-tab C-S-tab ?\M-~ left ?\C-b))
                      (setq selected (lispm-desktop-workspaces--move selected -1 count)) (redraw))
                     ((memq event '(down ?\C-n))
                      (setq selected (lispm-desktop-workspaces--move selected columns count)) (redraw))
                     ((memq event '(up ?\C-p))
                      (setq selected (lispm-desktop-workspaces--move selected (- columns) count))
                      (redraw))
                     ((and (characterp event) (<= ?1 event ?9) (<= (- event ?0) count))
                      (setq choice (cons 'workspace (nth (- event ?1) names)))
                      (throw 'done nil))
                     ((mouse-movement-p event)
                      (pcase (lispm-desktop-workspaces--target event popup)
                        (`(workspace . ,index)
                         (unless (= index selected)
                           (setq selected index) (redraw)))))
                     ((lispm-desktop-popup--press-p event)
                      (unless (lispm-desktop-popup--over event popup)
                        (throw 'done nil)))
                     ((lispm-desktop-popup--release-p event)
                      (pcase (lispm-desktop-workspaces--target event popup)
                        (`(workspace . ,index)
                         (setq choice (cons 'workspace (nth index names)))
                         (throw 'done nil))
                        (`(action . ,command)
                         (setq choice (cons 'action command))
                         (throw 'done nil))))
                     ((and (consp event) (memq (event-basic-type event)
                                               '(wheel-up wheel-down mouse-4 mouse-5))))
                     ((lispm-desktop-typed-event-p event)
                      ;; Any other key closes the panel and goes where it
                      ;; was typed.  Other events, such as the area a
                      ;; mouse event comes from, are passed over: the
                      ;; release of the click that dropped the panel from
                      ;; the menu bar comes as `tab-bar', then the release.
                      (push event unread-command-events)
                      (throw 'done nil))))))))
        (lispm-desktop-popup--dispose popup)))
    choice))

(defun lispm-desktop-workspaces--open (step &optional x y)
  "Choose a workspace from the panel, STEP from the one in front, and go."
  (if (not (lispm-desktop-popup-available-p))
      (lispm-desktop-workspaces-select (completing-read "Workspace: " (lispm-desktop-editor-workspaces) nil t))
    (pcase (lispm-desktop-workspaces-choose step x y)
      (`(workspace . ,name) (lispm-desktop-workspaces-select name))
      (`(action . ,command)
       (lispm-desktop-editor--focus)
       (call-interactively command)))))

(defun lispm-desktop-workspaces-switch ()
  "Choose a workspace of the editor from the panel of thumbnails, the next
one chosen first."
  (interactive)
  (lispm-desktop-workspaces--open 1))

(defun lispm-desktop-workspaces-menu-panel (key left top)
  "Drop the workspaces' panel from the menu bar title KEY at LEFT, TOP.
The editor's Project menu shows it instead of a list."
  (lispm-desktop-platinum--with-open-menu key
    (lispm-desktop-workspaces--open 0 left top)))
(put 'lispm-desktop-workspaces-menu-panel 'lispm-desktop-platinum-panel t)

(defun lispm-desktop-setup-workspaces ()
  "Bind the keys moving between workspaces."
  (keymap-global-set "<remap> <tab-next>" #'lispm-desktop-workspaces-next)
  (keymap-global-set "<remap> <tab-bar-switch-to-next-tab>" #'lispm-desktop-workspaces-next)
  (keymap-global-set "<remap> <tab-previous>" #'lispm-desktop-workspaces-previous)
  (keymap-global-set "<remap> <tab-bar-switch-to-prev-tab>" #'lispm-desktop-workspaces-previous)
  (keymap-global-set "M-`" #'lispm-desktop-workspaces-switch)
  (keymap-global-set "C-c w" #'lispm-desktop-workspaces-switch))

(provide 'lispm-desktop-workspaces)
;;; lispm-desktop-workspaces.el ends here
