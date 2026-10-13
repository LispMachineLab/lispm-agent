;;; lme-platinum.el --- Mac OS 9's layout and forms, in the theme's colours  -*- lexical-binding: t; -*-

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

;; The frame is laid out as Mac OS 9 laid out its desktop, and drawn in the
;; forms of its Platinum appearance; every colour still comes from the
;; theme.
;;
;;   menu bar     the tab bar: the λ menu at its left end, then a title per
;;                sidebar view; at its right end the Application menu,
;;                which lists the workspaces
;;   windows      every window wears a title bar: a close box, its title
;;                between pinstripes, a zoom box and a collapse box.  Only
;;                the active window shows stripes and boxes, so it is the
;;                one that stands out
;;   lists        the sidebar is a Finder window in list view: a header
;;                holding its buttons, column headings, disclosure
;;                triangles and one row per item
;;   bevels       controls stand one pixel proud: lit along the top and
;;                left edges, shaded along the bottom and right
;;   type         title bars and menus in Charcoal, or the nearest
;;                installed face; lists in Geneva
;;
;; Platinum's greys are not used.  Bevels, stripes and frames are shades of
;; the theme's own background and text, derived again whenever a theme is
;; enabled (`lme-platinum-apply-faces').
;;
;; Boxes, stripes and triangles are drawn as SVG.  The drawing functions are
;; pure: palette and geometry in, an SVG out; drawn images are memoized.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lme-core)

(declare-function lme-tab-bar--blend "lme-interface" (from to ratio))
(declare-function lme-tab-bar--bar-color "lme-interface" ())
(declare-function lme-tab-bar--muted-color "lme-interface" ())
(declare-function lme-main-area--windows "lme-interface" ())
(declare-function lme-agent-buffer-label "lme-ai" (buffer))
(declare-function lme-agent--buffer-agent "lme-agent-list" (buffer))
(declare-function lme-agent--buffer-p "lme-ai" (buffer))
(declare-function lme-agent--icon "lme-agent-list" (agent))
(declare-function lme-agent-installed-p "lme-ai" (agent))
(declare-function lme-sidebar-current-view "lme-sidebar" ())
(declare-function lme-sidebar-select "lme-sidebar" (view))
(declare-function lme-browser-workspace-p "lme-browser" ())
(declare-function nerd-icons-icon-for-buffer "nerd-icons" ())
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(defvar lme-sidebar-views)
(defvar lme-agent-emacs-agents)

(defgroup lme-platinum nil
  "Mac OS 9's layout and forms for lispm-emacs."
  :group 'lispm-emacs
  :prefix "lme-platinum-")

(defcustom lme-platinum-system-families
  '("Charcoal" "Charcoal CY" "ChicagoFLF" "Chicago" "Geneva" "Lucida Grande")
  "Font families for title bars and menus, the first installed one winning.
Mac OS 9 drew them in Charcoal."
  :type '(repeat string))

(defcustom lme-platinum-list-families '("Geneva" "Lucida Grande")
  "Font families for list views, the first installed one winning.
Mac OS 9's Finder listed files in Geneva."
  :type '(repeat string))

(defconst lme-platinum-title-height 22
  "Height in pixels of a title bar.")

(defconst lme-platinum-box-size 13
  "Side in pixels of a title bar box.")

;;;; Palette

(defvar lme-platinum--palette nil
  "The chrome's colours, a plist derived from the theme.
See `lme-platinum--derive-palette'.")

(defvar lme-platinum--generation 0
  "Count of palettes derived so far; memoized drawings carry it in their key.")

(defun lme-platinum--derive-palette (content text dim accent)
  "Return the chrome's palette for a theme drawing CONTENT, TEXT, DIM and ACCENT.
CONTENT and TEXT are the editor's background and foreground, DIM its
dimmed text, ACCENT its highlight colour.  The chrome is the content a
shade darker; a bevel's lit edge leans toward the text, its shaded edge
toward black, and frames are darker still."
  (let ((chrome (lme-tab-bar--blend content "#000000" 0.25)))
    (list :content content :text text :dim dim :accent accent
          :chrome chrome
          :header (lme-tab-bar--blend chrome content 0.5)
          :light (lme-tab-bar--blend chrome text 0.3)
          :shade (lme-tab-bar--blend chrome "#000000" 0.6)
          :frame (lme-tab-bar--blend content "#000000" 0.65)
          :muted (lme-tab-bar--blend dim text 0.35))))

(defun lme-platinum--color (name)
  "Return the palette colour NAME."
  (plist-get lme-platinum--palette name))

;;;; Drawing (pure)

(defvar lme-platinum--images (make-hash-table :test #'equal)
  "Drawn images by key: what was drawn, its size and the palette generation.")

(defun lme-platinum--image (key draw)
  "Return the image of KEY, calling DRAW to make its SVG the first time."
  (let ((key (cons lme-platinum--generation key)))
    (or (gethash key lme-platinum--images)
        (progn
          (when (> (hash-table-count lme-platinum--images) 512)
            (clrhash lme-platinum--images))
          (puthash key (svg-image (funcall draw) :ascent 'center :scale 1)
                   lme-platinum--images)))))

(defun lme-platinum--fill (svg x y width height color)
  "Fill the rectangle of WIDTH by HEIGHT at X, Y of SVG with COLOR.
Lines are drawn as one-pixel rectangles, so they fall on whole pixels."
  (svg-rectangle svg x y width height :fill color :stroke "none"))

(defun lme-platinum-draw-stripes (palette width height)
  "Return an SVG of WIDTH by HEIGHT pixels holding a title bar's pinstripes.
Six ridges, each a lit line over a shaded one, centred vertically on the
chrome of PALETTE."
  (let ((svg (svg-create width height))
        (top (/ (- height 12) 2)))
    (lme-platinum--fill svg 0 0 width height (plist-get palette :chrome))
    (dotimes (i 6)
      (lme-platinum--fill svg 0 (+ top (* 2 i)) width 1 (plist-get palette :light))
      (lme-platinum--fill svg 0 (+ top (* 2 i) 1) width 1 (plist-get palette :shade)))
    svg))

(defun lme-platinum-draw-box (palette kind height &optional modified disabled)
  "Return an SVG of a title bar box of KIND, `close', `zoom' or `collapse'.
The box is `lme-platinum-box-size' square, framed and bevelled in PALETTE,
centred in HEIGHT pixels.  MODIFIED puts a dot in a close box; DISABLED
draws an empty frame in the dimmed colour."
  (let* ((s lme-platinum-box-size)
         (svg (svg-create s height))
         (y (/ (- height s) 2))
         (frame (plist-get palette (if disabled :muted :frame))))
    (lme-platinum--fill svg 0 0 s height (plist-get palette :chrome))
    (lme-platinum--fill svg 0 y s s frame)
    (lme-platinum--fill svg 1 (1+ y) (- s 2) (- s 2) (plist-get palette :chrome))
    (unless disabled
      (lme-platinum--fill svg 1 (1+ y) (- s 2) 1 (plist-get palette :light))
      (lme-platinum--fill svg 1 (1+ y) 1 (- s 2) (plist-get palette :light))
      (lme-platinum--fill svg 1 (+ y s -2) (- s 2) 1 (plist-get palette :shade))
      (lme-platinum--fill svg (- s 2) (1+ y) 1 (- s 2) (plist-get palette :shade))
      (pcase kind
        ('zoom
         (lme-platinum--fill svg 3 (+ y 3) 6 1 frame)
         (lme-platinum--fill svg 3 (+ y 3) 1 6 frame)
         (lme-platinum--fill svg 3 (+ y 8) 6 1 frame)
         (lme-platinum--fill svg 8 (+ y 3) 1 6 frame))
        ('collapse
         (lme-platinum--fill svg 3 (+ y 5) (- s 6) 1 frame)
         (lme-platinum--fill svg 3 (+ y 7) (- s 6) 1 frame)))
      (when (and modified (eq kind 'close))
        (svg-circle svg (/ s 2.0) (+ y (/ s 2.0)) 2 :fill (plist-get palette :text))))
    svg))

(defun lme-platinum-draw-triangle (palette open)
  "Return an SVG of a list's disclosure triangle in PALETTE.
OPEN points it down, at a disclosed group; otherwise it points right."
  (let ((svg (svg-create 12 12)))
    (svg-polygon svg (if open '((1 . 3) (11 . 3) (6 . 9)) '((3 . 1) (9 . 6) (3 . 11)))
                 :fill (plist-get palette :dim) :stroke "none")
    svg))

(defun lme-platinum--stripes (width)
  "Return the pinstripes image WIDTH pixels wide."
  (lme-platinum--image (list 'stripes width)
                       (lambda () (lme-platinum-draw-stripes lme-platinum--palette width
                                                              lme-platinum-title-height))))

(defun lme-platinum--strut (width)
  "Return a blank image WIDTH pixels wide, as tall as a title bar.
Every title bar opens with one, so active and inactive bars are equally
tall and the content below never shifts when the focus moves."
  (lme-platinum--image (list 'strut width)
                       (lambda ()
                         (let ((svg (svg-create width lme-platinum-title-height)))
                           (lme-platinum--fill svg 0 0 width lme-platinum-title-height
                                               (plist-get lme-platinum--palette :chrome))
                           svg))))

(defun lme-platinum--box (kind &optional modified disabled)
  "Return the image of a title bar box of KIND, see `lme-platinum-draw-box'."
  (lme-platinum--image (list 'box kind modified disabled)
                       (lambda () (lme-platinum-draw-box lme-platinum--palette kind
                                                          lme-platinum-title-height
                                                          modified disabled))))

(defun lme-platinum-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN."
  (propertize " " 'display (lme-platinum--image
                            (list 'triangle open)
                            (lambda () (lme-platinum-draw-triangle lme-platinum--palette open)))))

;;;; Title bars

(defvar-local lme-platinum-title nil
  "Title the window of this buffer shows instead of the buffer's own name.")

(defvar-local lme-platinum-icon nil
  "Icon the window of this buffer shows before its title, a string.")

(defun lme-platinum-no-title-bar ()
  "Give the current buffer's windows no title bar, as for transient popups."
  (setq-local tab-line-format nil))

(defun lme-platinum--buffer-title (buffer)
  "Return the title of BUFFER's window."
  (or (buffer-local-value 'lme-platinum-title buffer)
      (and (fboundp 'lme-agent-buffer-label) (lme-agent-buffer-label buffer))
      (buffer-name buffer)))

(defun lme-platinum--buffer-icon (buffer)
  "Return the proxy icon of BUFFER's window, or nil."
  (or (buffer-local-value 'lme-platinum-icon buffer)
      (when (and (fboundp 'lme-agent--buffer-p) (lme-agent--buffer-p buffer))
        (lme-agent--icon (lme-agent--buffer-agent buffer)))
      (when (require 'nerd-icons nil t)
        (with-current-buffer buffer
          (let ((icon (nerd-icons-icon-for-buffer)))
            (and (stringp icon) icon))))))

(defun lme-platinum--window-command (command)
  "Return a mouse command calling COMMAND with the window clicked."
  (lambda (event)
    (interactive "e")
    (funcall command (posn-window (event-start event)))))

(defun lme-platinum--clickable (string command help)
  "Return STRING calling COMMAND on the clicked window, described by HELP.
The press is bound too: unbound, it would reach `mouse-drag-tab-line',
which tracks the mouse and swallows the release."
  (propertize string
              'help-echo help
              'pointer 'hand
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>" #'ignore)
                        (keymap-set map "<tab-line> <mouse-1>"
                                    (lme-platinum--window-command command))
                        map)))

(defun lme-platinum--space (width)
  "Return a blank WIDTH pixels wide."
  (propertize " " 'display `(space :width (,(max 0 width)))))

(defun lme-platinum--fit (string width)
  "Return STRING shortened with an ellipsis to fit WIDTH pixels.
The ellipsis takes the face of the text it follows."
  (if (<= (string-pixel-width string) width)
      string
    (let ((n (length string))
          (ellipsis (propertize "…" 'face (get-text-property (max 0 (1- (length string)))
                                                              'face string))))
      (while (and (> n 1)
                  (> (string-pixel-width (concat (substring string 0 n) ellipsis)) width))
        (setq n (max 1 (min (1- n)
                            (floor (* n (/ (float width)
                                           (max 1 (string-pixel-width
                                                   (substring string 0 n))))))))))
      (concat (substring string 0 n) ellipsis))))

(defun lme-platinum-title-layout (width title-width)
  "Return how a title bar WIDTH pixels wide lays out a title TITLE-WIDTH wide.
The value is (LEFT-STRIPES TITLE RIGHT-STRIPES): the widths of the
stripes before the title, of the title as it fits, and of the stripes
after it.  The title is centred in the bar unless that would push it into
the boxes; a title too wide is narrowed so that at least a short run of
stripes stays on either side."
  (let* ((s lme-platinum-box-size)
         (left (+ 8 s 6))                ; margin, close box, gap
         (right (+ 6 s 4 s 8))           ; gap, zoom box, gap, collapse box, margin
         (pad 8)                         ; blank either side of the title
         (min-stripes 12)
         (room (- width left right (* 2 (+ pad min-stripes))))
         (title (max 0 (min title-width room)))
         (start (max (+ left min-stripes pad)
                     (min (/ (- width title) 2)
                          (- width right min-stripes pad title)))))
    (list (- start left pad)
          title
          (- width right (+ start title pad)))))

(defun lme-platinum--window-controls (window)
  "Return the title bar boxes of WINDOW as (CLOSE ZOOM COLLAPSE).
Each is `on', `off' (drawn disabled) or nil.  A side window, such as the
sidebar, closes but neither zooms nor collapses; a window collapses only
when another shares its column."
  (let ((side (window-parameter window 'window-side)))
    (list 'on
          (if side 'off 'on)
          (if (and (not side) (window-combined-p window)) 'on 'off))))

(defun lme-platinum--title-bar (window)
  "Return the title bar of WINDOW, drawn for its current state."
  (let* ((buffer (window-buffer window))
         (active (mode-line-window-selected-p))
         (width (- (window-pixel-width window)
                   (window-right-divider-width window)
                   (window-scroll-bar-width window)))
         (icon (lme-platinum--buffer-icon buffer))
         (title (lme-platinum--buffer-title buffer))
         (modified (and (buffer-file-name buffer) (buffer-modified-p buffer)))
         (controls (lme-platinum--window-controls window))
         (key (list lme-platinum--generation width active icon title modified controls))
         (cached (window-parameter window 'lme-platinum-title-bar)))
    (if (equal (car cached) key)
        (cdr cached)
      (let ((bar (lme-platinum--draw-title-bar width active icon title modified controls)))
        (set-window-parameter window 'lme-platinum-title-bar (cons key bar))
        bar))))

(defun lme-platinum--draw-title-bar (width active icon title modified controls)
  "Return a title bar WIDTH pixels wide for ICON and TITLE.
ACTIVE draws the stripes and boxes; MODIFIED marks the close box;
CONTROLS is a value of `lme-platinum--window-controls'."
  (let* ((face (if active 'lme-platinum-title 'lme-platinum-title-inactive))
         (label (concat (when icon (concat icon " "))
                        (propertize title 'face face)))
         (s lme-platinum-box-size)
         (layout (lme-platinum-title-layout width (string-pixel-width label)))
         (label (lme-platinum--fit label (nth 1 layout))))
    (pcase-let ((`(,close ,zoom ,collapse) controls)
                (`(,left ,_ ,right) layout))
      (cl-flet ((box (kind state command help)
                  (cond ((not active) (lme-platinum--space s))
                        ((eq state 'off)
                         (propertize " " 'display (lme-platinum--box kind nil t)))
                        (t (lme-platinum--clickable
                            (propertize " " 'display
                                        (lme-platinum--box kind (and (eq kind 'close) modified)))
                            command help))))
                (stripes (pixels)
                  (if active
                      (propertize " " 'display (lme-platinum--stripes (max 1 pixels)))
                    (lme-platinum--space pixels))))
        (concat (propertize " " 'display (lme-platinum--strut 8))
                (box 'close close #'lme-platinum-close-window "Close this window")
                (lme-platinum--space 6)
                (stripes left)
                (lme-platinum--space 8)
                (lme-platinum--clickable label #'lme-platinum-title-menu
                                         "Click for this window's buffers")
                (lme-platinum--space (+ 8 (- (nth 1 layout) (string-pixel-width label))))
                (stripes right)
                (lme-platinum--space 6)
                (box 'zoom zoom #'lme-platinum-zoom-window "Zoom this window")
                (lme-platinum--space 4)
                (box 'collapse collapse #'lme-platinum-collapse-window
                     "Collapse this window to its title bar")
                (lme-platinum--space 8))))))

(defun lme-platinum-title-bar ()
  "The tab line of every window: its title bar, see `lme-platinum--title-bar'."
  (lme-platinum--title-bar (selected-window)))

;;;; Title bar commands

(defun lme-platinum-close-window (window)
  "Close WINDOW as Mac OS closed a window: put its document away.
A sidebar is hidden.  Otherwise WINDOW's buffer is killed, with Emacs's
usual questions about unsaved files and running processes, and WINDOW
goes too when other windows remain in the main area."
  (if (window-parameter window 'window-side)
      (when-let* ((view (lme-sidebar-current-view)))
        (lme-sidebar-select view))
    (let ((buffer (window-buffer window)))
      (when (kill-buffer buffer)
        (when (and (window-live-p window)
                   (cdr (lme-main-area--windows)))
          (delete-window window))))))

(defun lme-platinum-zoom-window (window)
  "Zoom WINDOW to the whole main area, or back to where it was.
The sidebar stays, as `delete-other-windows' leaves side windows be."
  (let ((saved (frame-parameter nil 'lme-platinum-unzoom)))
    (if (and saved (eq (car saved) window) (not (cdr (lme-main-area--windows))))
        (progn (set-frame-parameter nil 'lme-platinum-unzoom nil)
               (set-window-configuration (cdr saved)))
      (set-frame-parameter nil 'lme-platinum-unzoom
                           (cons window (current-window-configuration)))
      (delete-other-windows window))))

(defun lme-platinum-collapse-window (window)
  "Roll WINDOW up to its title bar, or down again, as a window shade.
Only a window sharing its column with another can roll up."
  (if-let* ((height (window-parameter window 'lme-platinum-shade)))
      (progn (set-window-parameter window 'lme-platinum-shade nil)
             (window-resize window (- height (window-total-height window)) nil t))
    (when (window-combined-p window)
      (set-window-parameter window 'lme-platinum-shade (window-total-height window))
      (window-resize window (- (window-min-size window nil t)
                               (window-total-height window))
                     nil t))))

(declare-function tabspaces--buffer-list "tabspaces" (&optional frame tabnum))

(defun lme-platinum--window-buffers (window)
  "Return the buffers WINDOW could show: its workspace's, minus hidden ones."
  (seq-remove (lambda (buffer) (string-prefix-p " " (buffer-name buffer)))
              (if (bound-and-true-p tabspaces-mode)
                  (tabspaces--buffer-list (window-frame window))
                (buffer-list (window-frame window)))))

(defun lme-platinum-title-menu (window)
  "Pop up the buffers WINDOW could show, and show the one chosen in it.
As Mac OS 9 popped up a window's path from its title, the title pops up
the workspace's buffers, the one WINDOW shows checked."
  (popup-menu
   (cons "Buffers"
         (mapcar (lambda (buffer)
                   (vector (lme-platinum--buffer-title buffer)
                           (lambda ()
                             (interactive)
                             (set-window-buffer window buffer)
                             (select-window window))
                           :style 'radio
                           :selected (eq buffer (window-buffer window))))
                 (seq-take (lme-platinum--window-buffers window) 30)))))

;;;; Menu bar

(defconst lme-platinum-menu-padding 10
  "Blank pixels on either side of a menu bar title.")

(defun lme-platinum--menu-title (key label command help &optional active)
  "Return the menu bar item KEY showing LABEL, which runs COMMAND.
HELP describes it; ACTIVE draws it highlighted, as an open menu.  The
title is padded with blanks in its own face, so a highlight spans the
padding, as Mac OS 9 inverted a whole title; the face is layered under
LABEL's own, so an icon keeps its font."
  (let* ((pad (propertize " " 'display `(space :width (,lme-platinum-menu-padding))))
         (title (concat pad label pad)))
    (add-face-text-property 0 (length title)
                            (if active 'lme-platinum-menu-title-active
                              'lme-platinum-menu-title)
                            t title)
    `(,key menu-item ,title ,command :help ,help)))

(defun lme-platinum-lambda-menu ()
  "Pop up the λ menu, LispM's counterpart of the Apple menu."
  (interactive)
  (popup-menu
   `("LispM"
     ["New LispM Agent" lme-agent-lispm-new :visible (fboundp 'lme-agent-lispm-new)]
     ["New Claude Code Session" lme-agent-claude-new :visible (lme-agent-installed-p 'claude)]
     ["New Codex Session" lme-agent-codex-new :visible (lme-agent-installed-p 'codex)]
     ["New Hermes Session" lme-agent-hermes-new :visible (lme-agent-installed-p 'hermes)]
     ["Resume Session…" lme-agent-sessions]
     ["Send Region to Agent" lme-agent-send-region :active (use-region-p)]
     "---"
     ["New Shell" lme-shell-new]
     ["Browser" lme-browser :visible (fboundp 'lme-browser)]
     ["Magit Status" magit-status]
     "---"
     ["Editing Style…" (customize-variable 'lme-editing-style)]
     ["Customize lispm-emacs…" (customize-group 'lispm-emacs)])))

(defun lme-platinum-menu-bar ()
  "Return the left end of the menu bar: the λ menu, then a title per view.
A view's title is highlighted while the sidebar shows it, and the Web
title while the browser workspace is selected."
  (let ((shown (lme-sidebar-current-view)))
    (append
     (list (lme-platinum--menu-title 'lme-lambda "λ" #'lme-platinum-lambda-menu "LispM"))
     (mapcar (lambda (view)
               (let ((spec (cdr view)))
                 (lme-platinum--menu-title (intern (format "lme-view-%s" (car view)))
                                           (plist-get spec :title)
                                           (plist-get spec :command)
                                           (plist-get spec :help)
                                           (eq (car view) shown))))
             lme-sidebar-views)
     (when (fboundp 'lme-browser)
       (list (lme-platinum--menu-title 'lme-view-web "Web" 'lme-browser "Browser"
                                       (lme-browser-workspace-p)))))))

(defun lme-platinum-workspace-menu ()
  "Pop up the Application menu: the workspaces, the current one checked."
  (interactive)
  (let ((current (tab-bar--current-tab-index)))
    (popup-menu
     `("Workspaces"
       ,@(seq-map-indexed
          (lambda (tab index)
            (vector (alist-get 'name tab)
                    (lambda () (interactive) (tab-bar-select-tab (1+ index)))
                    :style 'radio :selected (= index current)))
          (funcall tab-bar-tabs-function))
       "---"
       ["New Workspace" tab-bar-new-tab]
       ["Close Workspace" tab-bar-close-tab :active (cdr (funcall tab-bar-tabs-function))]))))

(defun lme-platinum-application-menu ()
  "Return the right end of the menu bar: the Application menu.
It names the current workspace, as Mac OS 9's named the frontmost
application, and lists every workspace when clicked."
  (require 'nerd-icons)
  (let ((name (alist-get 'name (tab-bar--current-tab))))
    (list (lme-platinum--menu-title
           'lme-application
           (concat (nerd-icons-codicon "nf-cod-window") " " name " ▾")
           #'lme-platinum-workspace-menu "Workspaces"))))

;;;; Lists

(defun lme-platinum-columns (name meta)
  "Return a list's column headings NAME and META as one line of bevels.
META, the column the list is sorted by, is shaded as Mac OS 9 shaded it."
  (concat (propertize (concat " " name)
                      'face 'lme-platinum-column)
          (propertize " " 'display '(space :align-to (- right 14))
                      'face 'lme-platinum-column)
          (propertize (concat " " meta " ")
                      'face 'lme-platinum-column-sorted)
          (propertize " " 'display '(space :align-to right)
                      'face 'lme-platinum-column-sorted)
          "\n"))

;;;; Faces

(defface lme-platinum-title '((t :weight bold))
  "Face of the active window's title."
  :group 'lme-platinum)

(defface lme-platinum-title-inactive '((t :weight bold :inherit shadow))
  "Face of an inactive window's title."
  :group 'lme-platinum)

(defface lme-platinum-menu-title '((t :weight bold))
  "Face of a menu bar title."
  :group 'lme-platinum)

(defface lme-platinum-menu-title-active '((t :weight bold :inverse-video t))
  "Face of a highlighted menu bar title, as of an open menu."
  :group 'lme-platinum)

(defface lme-platinum-list nil
  "Face of list views: the sidebar's rows."
  :group 'lme-platinum)

(defface lme-platinum-header nil
  "Face of a list window's header, below its title bar."
  :group 'lme-platinum)

(defface lme-platinum-button '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a bevel button."
  :group 'lme-platinum)

(defface lme-platinum-button-pressed '((t :box (:line-width (1 . 1) :style pressed-button)))
  "Face of a bevel button pushed in: a toggle that is on."
  :group 'lme-platinum)

(defface lme-platinum-column '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a column heading."
  :group 'lme-platinum)

(defface lme-platinum-column-sorted '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of the heading of the column a list is sorted by."
  :group 'lme-platinum)

(defun lme-platinum--family (families)
  "Return the first of FAMILIES installed, or nil."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed)) families)))

(defun lme-platinum-apply-faces (&rest _)
  "Derive the palette and every chrome face from the active theme.
Colours come only from faces this function never sets, so re-running
it, as on every theme change, is idempotent."
  (let ((content (face-background 'default nil t))
        (text (face-foreground 'default nil t))
        (dim (face-foreground 'shadow nil t))
        (accent (face-foreground 'font-lock-constant-face nil t)))
    (when (and content text dim accent (color-name-to-rgb content))
      (setq lme-platinum--palette (lme-platinum--derive-palette content text dim accent)
            lme-platinum--generation (1+ lme-platinum--generation))
      (let ((chrome (lme-platinum--color :chrome))
            (header (lme-platinum--color :header))
            (system (and (display-graphic-p) (lme-platinum--family lme-platinum-system-families)))
            (list-family (and (display-graphic-p)
                              (lme-platinum--family lme-platinum-list-families))))
        (set-face-attribute 'tab-line nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil :height 'unspecified)
        (set-face-attribute 'tab-bar nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil)
        (dolist (face '(lme-platinum-title lme-platinum-title-inactive
                        lme-platinum-menu-title lme-platinum-menu-title-active))
          (set-face-attribute face nil :family (or system 'unspecified) :height 120
                              :weight 'bold))
        (set-face-attribute 'lme-platinum-title nil :foreground text)
        (set-face-attribute 'lme-platinum-title-inactive nil
                            :foreground (lme-platinum--color :muted) :inherit 'unspecified)
        ;; A box in each title's own fill pads it vertically, so the bar is
        ;; as tall as Mac OS 9's and a highlight fills its height.
        (set-face-attribute 'lme-platinum-menu-title nil
                            :foreground text :background chrome :inverse-video nil
                            :box `(:line-width (1 . 5) :color ,chrome))
        (set-face-attribute 'lme-platinum-menu-title-active nil
                            :foreground (lme-platinum--color :content) :background accent
                            :inverse-video nil
                            :box `(:line-width (1 . 5) :color ,accent))
        (set-face-attribute 'lme-platinum-list nil :family (or list-family 'unspecified))
        (set-face-attribute 'lme-platinum-header nil
                            :inherit 'lme-platinum-list :background header :foreground dim
                            :box `(:line-width (1 . 4) :color ,header) :underline nil)
        (set-face-attribute 'lme-platinum-button nil
                            :inherit 'lme-platinum-list :background chrome :foreground text
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lme-platinum-button-pressed nil
                            :inherit 'lme-platinum-list :background (lme-platinum--color :shade)
                            :foreground (lme-platinum--color :accent)
                            :box '(:line-width (1 . 1) :style pressed-button))
        (set-face-attribute 'lme-platinum-column nil
                            :inherit 'lme-platinum-list :background header :foreground text
                            :height 0.9
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lme-platinum-column-sorted nil
                            :inherit 'lme-platinum-list :background chrome :foreground text
                            :height 0.9
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'window-divider nil :foreground (lme-platinum--color :frame))
        (set-face-attribute 'window-divider-first-pixel nil
                            :foreground (lme-platinum--color :frame))
        (set-face-attribute 'window-divider-last-pixel nil
                            :foreground (lme-platinum--color :frame))))))

;;;; Setup

(defun lme-setup-platinum ()
  "Lay the frame out as Mac OS 9 did, in the active theme's colours."
  (setq-default tab-line-format '(:eval (lme-platinum-title-bar)))
  (setq tab-bar-format '(lme-platinum-menu-bar
                         tab-bar-format-align-right
                         tab-bar-format-global
                         lme-platinum-application-menu))
  (setq window-divider-default-places t
        window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  (window-divider-mode 1)
  (lme-platinum-apply-faces)
  (add-hook 'enable-theme-functions #'lme-platinum-apply-faces)
  (add-hook 'which-key-init-buffer-hook #'lme-platinum-no-title-bar)
  (add-hook 'transient-setup-buffer-hook #'lme-platinum-no-title-bar))


(provide 'lme-platinum)
;;; lme-platinum.el ends here
