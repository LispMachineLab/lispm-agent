;;; lispm-desktop-platinum.el --- Mac OS 9's layout and forms, in the theme's colours  -*- lexical-binding: t; -*-

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
;;   bottoms      every window's mode line is its bottom edge: placards
;;                for the buffer, its position and its mode, and the grow
;;                box in the corner of the active window
;;   bevels       controls stand one pixel proud: lit along the top and
;;                left edges, shaded along the bottom and right
;;   type         title bars and menus in Charcoal, or the nearest
;;                installed face; lists in Geneva
;;
;; Colours come from the theme.  Bevels, stripes and frames are shades of
;; its background and text, derived again whenever a theme is enabled
;; (`lispm-desktop-platinum-apply-faces'); a theme can name them outright in
;; `lispm-desktop-platinum-colors', as the platinum theme gives Platinum's own greys.
;;
;; Boxes, stripes and triangles are drawn as SVG.  The drawing functions are
;; pure: palette and geometry in, an SVG out; drawn images are memoized.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-core)

(declare-function lispm-desktop-tab-bar--blend "lispm-desktop-interface" (from to ratio))
(declare-function lispm-desktop-tab-bar--rgb "lispm-desktop-interface" (color))
(declare-function lispm-desktop-tab-bar--bar-color "lispm-desktop-interface" ())
(declare-function lispm-desktop-tab-bar--muted-color "lispm-desktop-interface" ())
(declare-function lispm-desktop-main-area--windows "lispm-desktop-interface" ())
(declare-function lispm-desktop-agent-buffer-label "lispm-desktop-ai" (buffer))
(declare-function lispm-desktop-term-status "lispm-desktop-term" (&optional buffer))
(declare-function lispm-desktop-term-status-label "lispm-desktop-term" (record &optional brief))
(declare-function lispm-desktop-agent--buffer-agent "lispm-desktop-agent-list" (buffer))
(declare-function lispm-desktop-agent--buffer-p "lispm-desktop-ai" (buffer))
(declare-function lispm-desktop-agent--icon "lispm-desktop-agent-list" (agent))
(declare-function lispm-desktop-agent-installed-p "lispm-desktop-ai" (agent))
(declare-function lispm-desktop-sidebar-current-view "lispm-desktop-sidebar" ())
(declare-function lispm-desktop-sidebar-select "lispm-desktop-sidebar" (view))
(declare-function lispm-desktop-browser-workspace-p "lispm-desktop-browser" ())
(defvar lispm-desktop-sidebar-views)
(defvar lispm-desktop-agent-emacs-agents)

(defgroup lispm-desktop-platinum nil
  "Mac OS 9's layout and forms for lispm-desktop."
  :group 'lispm-desktop
  :prefix "lispm-desktop-platinum-")

(defcustom lispm-desktop-platinum-system-families
  '("Charcoal" "Charcoal CY" "ChicagoFLF" "Chicago" "Geneva" "Lucida Grande")
  "Font families for title bars and menus, the first installed one winning.
Mac OS 9 drew them in Charcoal."
  :type '(repeat string))

(defcustom lispm-desktop-platinum-list-families '("Geneva" "Lucida Grande")
  "Font families for list views, the first installed one winning.
Mac OS 9's Finder listed files in Geneva."
  :type '(repeat string))

(defcustom lispm-desktop-platinum-colors nil
  "Chrome colours a theme names outright, a plist.
Keys are those of `lispm-desktop-platinum--derive-palette' (:chrome, :header,
:light, :shade, :frame, :muted, :accent, :outline, :highlight, :thumb,
:focus, :strip, ...); a colour left out is derived from the theme's
faces.  Themes set it, as the platinum theme does; users rarely need to."
  :type '(plist :value-type color))

(defconst lispm-desktop-platinum-title-height 22
  "Height in pixels of a title bar, and of a window's bottom edge.")

(defconst lispm-desktop-platinum-menu-bar-height 20
  "Height in pixels of the menu bar, Mac OS 9's.")

(defconst lispm-desktop-platinum-box-size 13
  "Side in pixels of a title bar box.")

;;;; Palette

(defvar lispm-desktop-platinum--palette nil
  "The chrome's colours, a plist derived from the theme.
See `lispm-desktop-platinum--derive-palette'.")

(defvar lispm-desktop-platinum--generation 0
  "Count of palettes derived so far; memoized drawings carry it in their key.")

(defun lispm-desktop-platinum--derive-palette (content text dim accent)
  "Return the chrome's palette for a theme drawing CONTENT, TEXT, DIM and ACCENT.
CONTENT and TEXT are the editor's background and foreground, DIM its
dimmed text, ACCENT its highlight colour.  The chrome is the content a
shade darker; a bevel's lit edge leans toward the text, its shaded edge
toward black, and frames are darker still; controls are outlined in
near black.  The desktop behind the windows is the content tinted with
the accent.

As Mac OS 9's variation colour was a ramp, so is the accent: menus are
highlighted in it darkened, scroll thumbs are it lightened, and the
keyboard focus is ringed in it.  The Control Strip has greys of its own,
a little darker than the chrome; alerts are framed in a light red."
  (let* ((chrome (lispm-desktop-tab-bar--blend content "#000000" 0.25))
         (light (lispm-desktop-tab-bar--blend chrome text 0.3))
         (outline (lispm-desktop-tab-bar--blend content "#000000" 0.85))
         (highlight (lispm-desktop-tab-bar--blend accent "#000000" 0.4))
         (palette (list :content content :text text :dim dim :accent accent
                        :chrome chrome :light light :outline outline)))
    (append
     palette
     (list :desktop (lispm-desktop-tab-bar--blend content accent 0.25)
           :header (lispm-desktop-tab-bar--blend chrome content 0.5)
           :shade (lispm-desktop-tab-bar--blend chrome "#000000" 0.6)
           :frame (lispm-desktop-tab-bar--blend content "#000000" 0.65)
           :muted (lispm-desktop-tab-bar--blend dim text 0.35)
           :highlight highlight
           :highlight-dark (lispm-desktop-tab-bar--blend highlight "#000000" 0.45)
           :highlight-text (lispm-desktop-platinum--legible highlight text content)
           :thumb (lispm-desktop-tab-bar--blend accent "#FFFFFF" 0.35)
           :thumb-light (lispm-desktop-tab-bar--blend accent "#FFFFFF" 0.65)
           :thumb-dark accent
           :focus accent
           :alert (lispm-desktop-tab-bar--blend light "#FF0000" 0.4)
           :alert-dark (lispm-desktop-tab-bar--blend light "#FF0000" 0.6)
           :strip (lispm-desktop-platinum-grey palette #xC0)
           :strip-light (lispm-desktop-platinum-grey palette #xFF)
           :strip-shade (lispm-desktop-platinum-grey palette #xA0)
           :strip-dark (lispm-desktop-platinum-grey palette #x80)))))

(defun lispm-desktop-platinum-grey (palette level)
  "Return the colour of PALETTE standing where Platinum's grey LEVEL stands.
LEVEL is one of Mac OS 9's greys as a byte, #xDD its chrome, #xFF its
lit edges and #x00 its outlines: the greys between the chrome and white
are taken between PALETTE's :chrome and :light, those below the chrome
between its :chrome and :outline.  With Platinum's own palette every
level comes back as itself, so a control is drawn with the greys Mac OS 9
drew it with, and a theme of other colours gets their counterparts."
  (let ((chrome #xDD))
    (if (>= level chrome)
        (lispm-desktop-tab-bar--blend (plist-get palette :chrome) (plist-get palette :light)
                                      (/ (- level chrome) (float (- #xFF chrome))))
      (lispm-desktop-tab-bar--blend (plist-get palette :chrome) (plist-get palette :outline)
                                    (/ (- chrome level) (float chrome))))))

(defun lispm-desktop-platinum--luminance (color)
  "Return the relative luminance of COLOR, between 0 and 1."
  (pcase-let ((`(,r ,g ,b) (or (lispm-desktop-tab-bar--rgb color) '(0 0 0))))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun lispm-desktop-platinum--legible (background &rest colors)
  "Return whichever of COLORS stands out most from BACKGROUND."
  (let ((ground (lispm-desktop-platinum--luminance background)))
    (car (seq-sort-by (lambda (color) (abs (- (lispm-desktop-platinum--luminance color) ground)))
                      #'> colors))))

(defun lispm-desktop-platinum--merge-palette (derived named)
  "Return the palette DERIVED with the colours NAMED put in their place."
  (let ((palette (copy-sequence derived)))
    (cl-loop for (key value) on named by #'cddr
             do (setq palette (plist-put palette key value)))
    palette))

(defun lispm-desktop-platinum--color (name)
  "Return the palette colour NAME."
  (plist-get lispm-desktop-platinum--palette name))

;;;; Drawing (pure)

(defvar lispm-desktop-platinum--images (make-hash-table :test #'equal)
  "Drawn images by key: what was drawn, its size and the palette generation.")

(defun lispm-desktop-platinum--image (key draw)
  "Return the image of KEY, calling DRAW to make its SVG the first time."
  (let ((key (cons lispm-desktop-platinum--generation key)))
    (or (gethash key lispm-desktop-platinum--images)
        (progn
          (when (> (hash-table-count lispm-desktop-platinum--images) 512)
            (clrhash lispm-desktop-platinum--images))
          (puthash key (lispm-desktop-platinum--svg-image (funcall draw))
                   lispm-desktop-platinum--images)))))

(defun lispm-desktop-platinum--svg-image (svg)
  "Return the image of SVG, centred on its line and never scaled.
An Emacs built without SVG gets the image all the same, which it cannot
show: the text under it shows instead, where `svg-image' would signal."
  (if (image-type-available-p 'svg)
      (svg-image svg :ascent 'center :scale 1)
    (list 'image :type 'svg :data (with-temp-buffer (svg-print svg) (buffer-string))
          :ascent 'center :scale 1)))

(defun lispm-desktop-platinum--fill (svg x y width height color)
  "Fill the rectangle of WIDTH by HEIGHT at X, Y of SVG with COLOR.
Lines are drawn as one-pixel rectangles, so they fall on whole pixels."
  (svg-rectangle svg x y width height :fill color :stroke "none"))

(defun lispm-desktop-platinum--dot (svg x y color)
  "Colour the pixel at X, Y of SVG with COLOR."
  (lispm-desktop-platinum--fill svg x y 1 1 color))

(defun lispm-desktop-platinum--row (svg from to y color)
  "Draw a line along row Y of SVG in COLOR, from column FROM to TO, both in."
  (when (<= from to)
    (lispm-desktop-platinum--fill svg from y (1+ (- to from)) 1 color)))

(defun lispm-desktop-platinum--column (svg x from to color)
  "Draw a line down column X of SVG in COLOR, from row FROM to TO, both in."
  (when (<= from to)
    (lispm-desktop-platinum--fill svg x from 1 (1+ (- to from)) color)))

(defconst lispm-desktop-platinum-title-bar-height 21
  "Height in pixels of a title bar: its lit top line to the content's
black line, both included.  The outline above it is drawn by what is
above: the menu bar's black line, a window divider, a window's frame.")

(defun lispm-desktop-platinum--draw-title-ground (svg x width palette active)
  "Draw a title bar's ground on SVG from X, WIDTH pixels wide.
An ACTIVE bar is platinum, lit along its top, shaded along its bottom
over the content's black line; an inactive one lies flat, over a grey
line."
  (let ((grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-desktop-platinum--fill svg x 0 width h (funcall grey (if active #xCC #xDD)))
    (if (not active)
        (lispm-desktop-platinum--fill svg x (1- h) width 1 (funcall grey #x55))
      (lispm-desktop-platinum--fill svg x 0 width 1 (funcall grey #xFF))
      (lispm-desktop-platinum--fill svg x (- h 2) width 1 (funcall grey #x99))
      (lispm-desktop-platinum--fill svg x (1- h) width 1 (plist-get palette :outline)))))

(defun lispm-desktop-platinum--draw-ridges (svg from to palette)
  "Draw a title bar's six ridges on SVG from column FROM to TO, both in.
Each ridge is a lit line over a shaded one, the lit line starting a
pixel further left and the shaded one ending a pixel further right, so
their ends look engraved."
  (when (< from to)
    (dotimes (i 6)
      (let ((y (+ 3 (* 2 i))))
        (lispm-desktop-platinum--row svg from (1- to) y (lispm-desktop-platinum-grey palette #xFF))
        (lispm-desktop-platinum--row svg (1+ from) to (1+ y) (lispm-desktop-platinum-grey palette #x77))))))

(defun lispm-desktop-platinum-draw-box (palette kind &optional modified pressed)
  "Return an SVG of a title bar box of KIND, `close', `zoom' or `collapse'.
The box is `lispm-desktop-platinum-box-size' square on an active title
bar's ground, as Mac OS 9 drew it: a recessed well round a dark frame,
inside it a bevel round a square shaded diagonally from grey to white.
The zoom box's glyph is a square sharing the frame's top left corner,
the collapse box's two lines across it.  MODIFIED puts a dot in a close
box; PRESSED draws the box held down, its square darkened and its bevel
turned over."
  (let* ((s lispm-desktop-platinum-box-size)
         (svg (svg-create s lispm-desktop-platinum-title-bar-height))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (y 3)
         (ink (funcall grey #x22))
         (ramp (if pressed '(#x55 #x66 #x77 #x88 #x99 #xAA #xBB)
                 '(#x99 #xAA #xBB #xCC #xDD #xEE #xFF))))
    (cl-flet ((dot (x dy color) (lispm-desktop-platinum--dot svg x (+ y dy) color))
              (row (from to dy color) (lispm-desktop-platinum--row svg from to (+ y dy) color))
              (column (x from to color) (lispm-desktop-platinum--column svg x (+ y from) (+ y to) color)))
      (lispm-desktop-platinum--draw-title-ground svg 0 s palette t)
      ;; The well.
      (row 0 11 0 (funcall grey #x88))
      (column 0 0 11 (funcall grey #x88))
      (row 1 12 12 (funcall grey #xFF))
      (column 12 1 12 (funcall grey #xFF))
      ;; The frame.
      (row 1 11 1 ink)
      (row 1 11 11 ink)
      (column 1 1 11 ink)
      (column 11 1 11 ink)
      ;; The bevel, lit from the top left, or from below when pressed.
      (row 2 10 2 (funcall grey (if pressed #x88 #xCC)))
      (column 2 3 10 (funcall grey (if pressed #x88 #xCC)))
      (dot 2 2 (funcall grey (if pressed #x77 #xFF)))
      (column 10 3 10 (funcall grey (if pressed #xCC #x88)))
      (row 3 10 10 (funcall grey (if pressed #xCC #x88)))
      ;; The square, shaded along its diagonal.
      (dotimes (j 7)
        (dotimes (i 7)
          (dot (+ 3 i) (+ 3 j) (funcall grey (nth (/ (+ i j) 2) ramp)))))
      (pcase kind
        ('zoom
         (column 7 1 7 ink)
         (row 1 7 7 ink))
        ('collapse
         (row 1 11 5 ink)
         (row 1 11 7 ink)))
      (when (and modified (eq kind 'close))
        (svg-circle svg (/ s 2.0) (+ y (/ s 2.0)) 2 :fill (plist-get palette :outline))))
    svg))

(cl-defun lispm-desktop-platinum-draw-title-part (palette width &key active ridges label label-x
                                                          lead trail)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE.
It shows the ground of an ACTIVE or inactive bar, the RIDGES, a list of
\(FROM . TO) columns, and LABEL, a string as Emacs would show it in the
titles' face, its icon included, from column LABEL-X.  LEAD draws the
lit line down the bar's left end, TRAIL the shaded one down its right."
  (let ((svg (svg-create width lispm-desktop-platinum-title-bar-height))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-desktop-platinum--draw-title-ground svg 0 width palette active)
    (when active
      (when lead
        (lispm-desktop-platinum--column svg 0 0 (- h 3) (lispm-desktop-platinum-grey palette #xFF)))
      (when trail
        (lispm-desktop-platinum--column svg (1- width) 1 (- h 3) (lispm-desktop-platinum-grey palette #x99)))
      (pcase-dolist (`(,from . ,to) ridges)
        (lispm-desktop-platinum--draw-ridges svg from to palette)))
    (when label
      (lispm-desktop-platinum--draw-content
       svg label label-x 0 (- h 2)
       (if active (plist-get palette :text) (lispm-desktop-platinum-grey palette #x77))
       (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive)))
    svg))

(defconst lispm-desktop-platinum--triangle
  '("O" "OO" "OLO" "OLFO" "OLFFO" "OLFFDO" "OLFDOSs" "OLDOSs" "OFOSs" "OOSs" "OSs" " s")
  "Mac OS 9's disclosure triangle pointing right, a row of pixels a string.
O is the outline, L, F and D the light, face and dark of the variation
colour, S and s the shadow falling to the lower right.")

(defun lispm-desktop-platinum-draw-triangle (palette open)
  "Return an SVG of a list's disclosure triangle in PALETTE.
OPEN points it down, at a disclosed group; otherwise it points right.
It is drawn as Mac OS 9 drew it: outlined, in the variation colour lit
from the left, casting a shadow; pointing down, it is the same triangle
turned over its diagonal."
  (let ((svg (svg-create 12 12))
        (colors `((?O . ,(plist-get palette :outline)) (?L . ,(plist-get palette :thumb-light))
                  (?F . ,(plist-get palette :thumb)) (?D . ,(plist-get palette :thumb-dark))
                  (?S . ,(lispm-desktop-platinum-grey palette #x88))
                  (?s . ,(lispm-desktop-platinum-grey palette #xBB)))))
    (seq-do-indexed
     (lambda (row y)
       (seq-do-indexed
        (lambda (pixel x)
          (when-let* ((color (alist-get pixel colors)))
            (if open
                (lispm-desktop-platinum--dot svg y (+ x 3) color)
              (lispm-desktop-platinum--dot svg (+ x 3) y color))))
        row))
     lispm-desktop-platinum--triangle)
    svg))

(defun lispm-desktop-platinum--strut (width)
  "Return a blank image WIDTH pixels wide, as tall as a title bar.
Every title bar opens with one, so active and inactive bars are equally
tall and the content below never shifts when the focus moves."
  (lispm-desktop-platinum--image (list 'strut width)
                                 (lambda ()
                                   (let ((svg (svg-create width lispm-desktop-platinum-title-height)))
                                     (lispm-desktop-platinum--fill svg 0 0 width lispm-desktop-platinum-title-height
                                                                   (plist-get lispm-desktop-platinum--palette :chrome))
                                     svg))))

(defconst lispm-desktop-platinum-header-strip-height 22
  "Height in pixels of a window's header strip, as the Finder window's.")

(defun lispm-desktop-platinum-draw-header-strip (palette width &optional label)
  "Return an SVG of a window's header strip WIDTH pixels wide, in PALETTE.
It runs under the title bar as the Finder window's did, lit along its
top, platinum, shaded along its bottom over a black line, LABEL centred
in the small system font, as \"9 items, 705 MB available\"."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum--fill svg 0 0 width h (funcall grey #xDD))
    (lispm-desktop-platinum--row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-desktop-platinum--row svg 0 (1- width) (- h 2) (funcall grey #xAA))
    (lispm-desktop-platinum--row svg 0 (1- width) (1- h) (plist-get palette :outline))
    (when label
      (lispm-desktop-platinum--draw-content
       svg label (/ (- width (lispm-desktop-platinum--content-width label 'lispm-desktop-platinum-small)) 2)
       1 (- h 3) (plist-get palette :text) 'lispm-desktop-platinum-small))
    svg))

(defun lispm-desktop-platinum-header-strip (label)
  "Return the header strip of the window being drawn, showing LABEL.
It is a string to put in the window's header line; see
`lispm-desktop-platinum-draw-header-strip'."
  (let ((width (- (window-pixel-width) (window-right-divider-width) (window-scroll-bar-width))))
    (propertize " " 'display
                (lispm-desktop-platinum--image
                 (list 'header-strip label width)
                 (lambda () (lispm-desktop-platinum-draw-header-strip lispm-desktop-platinum--palette
                                                                      width label))))))

(defconst lispm-desktop-platinum-group-title-height 18
  "Height in pixels of a group box's top edge, which carries its title.")

(defun lispm-desktop-platinum-draw-group-edge (palette width &optional title bottom)
  "Return an SVG of a group box's top edge WIDTH pixels wide, in PALETTE.
Mac OS 9 framed a group of controls in an engraved line, a grey line
with a lit one along it, TITLE breaking the top line near its left end
in the system font.  BOTTOM draws the bottom edge instead, closing the
frame's sides at the top of a line as tall as a line of text."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (dark (lispm-desktop-platinum-grey palette #x88))
         (lit (lispm-desktop-platinum-grey palette #xFF)))
    (if bottom
        (progn
          (lispm-desktop-platinum--column svg 0 0 y dark)
          (lispm-desktop-platinum--column svg 1 0 (1- y) lit)
          (lispm-desktop-platinum--column svg (- width 2) 0 y dark)
          (lispm-desktop-platinum--column svg (1- width) 0 (1+ y) lit))
      (lispm-desktop-platinum--column svg 0 y (1- h) dark)
      (lispm-desktop-platinum--column svg 1 (1+ y) (1- h) lit)
      (lispm-desktop-platinum--column svg (- width 2) y (1- h) dark)
      (lispm-desktop-platinum--column svg (1- width) y (1- h) lit))
    (lispm-desktop-platinum--row svg 0 (- width 2) y dark)
    (lispm-desktop-platinum--row svg 1 (1- width) (1+ y) lit)
    (when title
      (let ((title-width (lispm-desktop-platinum--content-width title 'lispm-desktop-platinum-title)))
        (lispm-desktop-platinum--fill svg 8 0 (+ title-width 8) h (lispm-desktop-platinum-grey palette #xEE))
        (lispm-desktop-platinum--draw-content svg title 12 0 h (plist-get palette :text)
                                              'lispm-desktop-platinum-title)))
    svg))

(defun lispm-desktop-platinum-group-edge (width &optional title bottom)
  "Return a group box's edge WIDTH pixels wide as a string displaying it.
See `lispm-desktop-platinum-draw-group-edge'."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'group-edge width title bottom)
               (lambda () (lispm-desktop-platinum-draw-group-edge lispm-desktop-platinum--palette
                                                                  width title bottom)))))

(defun lispm-desktop-platinum-group-side (height)
  "Return a group box's side for a line HEIGHT pixels tall, as a string
displaying it: the grey line and the lit one beside it."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'group-side height)
               (lambda ()
                 (let ((svg (svg-create 2 height)))
                   (lispm-desktop-platinum--fill svg 0 0 1 height
                                                 (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #x88))
                   (lispm-desktop-platinum--fill svg 1 0 1 height
                                                 (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xFF))
                   svg)))))

(defun lispm-desktop-platinum--box (kind &optional modified pressed)
  "Return the image of a title bar box of KIND.
See `lispm-desktop-platinum-draw-box'."
  (lispm-desktop-platinum--image (list 'box kind modified pressed)
                                 (lambda () (lispm-desktop-platinum-draw-box lispm-desktop-platinum--palette kind
                                                                             modified pressed))))

(defun lispm-desktop-platinum--title-part (width &rest options)
  "Return a stretch of title bar WIDTH pixels wide as a string displaying it.
OPTIONS are those of `lispm-desktop-platinum-draw-title-part'."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'title-part width
                     (plist-get options :active) (plist-get options :ridges)
                     (and (plist-get options :label)
                          (lispm-desktop-platinum--content-key (plist-get options :label)))
                     (plist-get options :label-x) (plist-get options :lead) (plist-get options :trail))
               (lambda () (apply #'lispm-desktop-platinum-draw-title-part lispm-desktop-platinum--palette
                                 width options)))))

(defun lispm-desktop-platinum-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN."
  (propertize " " 'display (lispm-desktop-platinum--image
                            (list 'triangle open)
                            (lambda () (lispm-desktop-platinum-draw-triangle lispm-desktop-platinum--palette open)))))

;;;; Controls (pure)
;;
;; Mac OS 9's controls as its Appearance Manager drew them in Platinum,
;; measured pixel by pixel on Mac OS 9.0.4: push buttons and the ring of
;; the default one, bevel buttons, tabs, placards, radio buttons, check
;; boxes, focus rings, window frames, scroll bars and the Control Strip's
;; segments.  Each draws with `lispm-desktop-platinum-grey', so Platinum's
;; greys come out as themselves and another theme gets their
;; counterparts.  Labels are SVG text in the system font, as heavy as the
;; titles and menus Emacs draws (`lispm-desktop-platinum-svg-font').

(defconst lispm-desktop-platinum-button-height 20
  "Height in pixels of a push button, its outline included.")

(defconst lispm-desktop-platinum-tab-height 21
  "Height in pixels of a tab, its outline included.
The edge of the pane the tabs stand on takes three rows more.")

(defconst lispm-desktop-platinum-strip-height 24
  "Height in pixels of the Control Strip, its outline included.")

(defvar lispm-desktop-platinum--svg-fonts (make-hash-table :test #'equal)
  "Fonts of SVG labels by palette generation and face, see
`lispm-desktop-platinum-svg-font'; faces are set again with every palette.")

(defun lispm-desktop-platinum--synthetic-bold-p (face)
  "Return non-nil when Emacs draws bold FACE by thickening its regular glyphs.
A family without a bold face of its own, as Geneva, is drawn so; its
bold text then runs exactly as wide as its regular, which a real bold
face hardly ever does."
  (with-memoization (gethash (list lispm-desktop-platinum--generation 'synthetic face)
                             lispm-desktop-platinum--svg-fonts)
    (let ((sample "Don’t Save Cancel OK")
          (spec (if (symbolp face) (list :inherit face) face)))
      (= (string-pixel-width (propertize sample 'face `(:weight bold ,@spec)))
         (string-pixel-width (propertize sample 'face `(:weight normal ,@spec)))))))

(defun lispm-desktop-platinum-svg-font (&optional face)
  "Return the font of SVG labels matching FACE, by default the titles' face.
The value is a plist: :family and :size, in pixels, are FACE's; :weight
is bold when FACE is.  A family without a bold face Emacs makes bold
itself, drawing every glyph a second time half a pixel to the right;
:synthetic is then non-nil and labels are drawn the same way, so a label
drawn as SVG is as heavy as the titles and menus Emacs draws beside it."
  (let ((face (or face 'lispm-desktop-platinum-title)))
    (with-memoization (gethash (cons lispm-desktop-platinum--generation face)
                               lispm-desktop-platinum--svg-fonts)
      (let* ((family (face-attribute face :family nil t))
             (height (face-attribute face :height nil t))
             (bold (memq (face-attribute face :weight nil t)
                         '(semi-bold bold extra-bold ultra-bold heavy black)))
             (synthetic (and bold (display-graphic-p)
                             (lispm-desktop-platinum--synthetic-bold-p face))))
        (list :family (concat (if (stringp family) family "Geneva") ", sans-serif")
              :size (if (numberp height) (/ height 10.0) 12)
              :weight (if (and bold (not synthetic)) "bold" "normal")
              :synthetic synthetic)))))

(defun lispm-desktop-platinum--label (svg text x y color font &optional anchor)
  "Write TEXT on SVG at X, its baseline at Y, in COLOR and FONT.
FONT is a value of `lispm-desktop-platinum-svg-font'; ANCHOR is SVG's
text anchor, by default the middle."
  (dolist (dx (if (plist-get font :synthetic) '(0 0.5) '(0)))
    ;; Spaces are kept, as Emacs shows them, at a run's ends too.
    (svg-text svg (substring-no-properties text) :x (+ x dx) :y y :text-anchor (or anchor "middle")
              :xml:space "preserve"
              :font-family (plist-get font :family) :font-size (plist-get font :size)
              :font-weight (plist-get font :weight) :fill color)))

(defun lispm-desktop-platinum--draw-map (svg x y rows colors)
  "Draw ROWS, strings of pixels, on SVG from X, Y.
COLORS maps each character to its colour; a character it leaves out is
left clear."
  (seq-do-indexed
   (lambda (row dy)
     (seq-do-indexed
      (lambda (pixel dx)
        (when-let* ((color (alist-get pixel colors)))
          (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) color)))
      row))
   rows))

;;;;; Labels
;;
;; A control's label may be any text Emacs shows: words in a face, glyphs
;; of an icon font, images.  Drawn into the control's SVG run by run, it
;; keeps the widths Emacs measures for it, so a control is as wide as its
;; label would be on the line.

(defvar lispm-desktop-platinum--icon-doms (make-hash-table :test #'equal)
  "SVG documents read from files or data, by file or data.")

(defun lispm-desktop-platinum--escape-xml (string)
  "Return STRING with the characters XML reserves written as references."
  (replace-regexp-in-string "[&<>\"]"
                            (lambda (char)
                              (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
                            string t t))

(defun lispm-desktop-platinum--clean-dom (node)
  "Return NODE, parsed XML, ready to be printed again by `svg-print':
comments dropped, text and attribute values escaped, which `svg-print'
writes as they are."
  (cond ((stringp node) (lispm-desktop-platinum--escape-xml node))
        ((memq (car-safe node) '(comment top)) nil)
        ((consp node)
         `(,(car node)
           ,(mapcar (lambda (attribute)
                      (cons (car attribute)
                            (if (stringp (cdr attribute))
                                (lispm-desktop-platinum--escape-xml (cdr attribute))
                              (cdr attribute))))
                    (nth 1 node))
           ,@(delq nil (mapcar #'lispm-desktop-platinum--clean-dom (nthcdr 2 node)))))))

(defun lispm-desktop-platinum--svg-dom (file &optional data)
  "Return the SVG document of FILE, or of the string DATA, or nil.
Nil too where Emacs cannot parse XML.  The document is cleaned to be
printed again, see `lispm-desktop-platinum--clean-dom'."
  (when (fboundp 'libxml-parse-xml-region)
    (with-memoization (gethash (or data file) lispm-desktop-platinum--icon-doms)
      (when (or data (file-readable-p file))
        (with-temp-buffer
          (if data (insert data) (insert-file-contents file))
          (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
            ;; A document with a declaration or comments around its root
            ;; comes back as a `top' node holding them.
            (when (eq (car-safe dom) 'top)
              (setq dom (seq-find (lambda (node) (eq (car-safe node) 'svg)) (nthcdr 2 dom))))
            (and dom (lispm-desktop-platinum--clean-dom dom))))))))

(defun lispm-desktop-platinum--embed-svg (svg dom x y width height)
  "Draw the SVG document DOM on SVG at X, Y, WIDTH by HEIGHT pixels."
  (let ((copy (copy-tree dom)))
    (dom-set-attribute copy 'x x)
    (dom-set-attribute copy 'y y)
    (dom-set-attribute copy 'width width)
    (dom-set-attribute copy 'height height)
    (dom-append-child svg copy)))

(defun lispm-desktop-platinum--embed-icon (svg file x y size)
  "Draw the SVG icon FILE on SVG at X, Y, SIZE pixels square."
  (when-let* ((icon (lispm-desktop-platinum--svg-dom file)))
    (lispm-desktop-platinum--embed-svg svg icon x y size size)))

(defun lispm-desktop-platinum--image-size (spec)
  "Return the size in pixels of image SPEC, as (WIDTH . HEIGHT).
Where no image can be shown its :width and :height are taken, or 16."
  (or (ignore-errors (image-size spec t))
      (cons (or (image-property spec :width) 16) (or (image-property spec :height) 16))))

(defun lispm-desktop-platinum--draw-image (svg spec x y width height)
  "Draw the Emacs image SPEC on SVG at X, Y, WIDTH by HEIGHT pixels.
SVG images are drawn as SVG; others are embedded as they are."
  (let* ((file (image-property spec :file))
         (data (image-property spec :data))
         (type (or (image-property spec :type)
                   (and file (image-supported-file-p file))
                   (and data (image-type-from-data data)))))
    (if (eq type 'svg)
        (when-let* ((dom (lispm-desktop-platinum--svg-dom file data)))
          (lispm-desktop-platinum--embed-svg svg dom x y width height))
      (when (or data (and file (file-readable-p file)))
        (svg-embed svg (or data file) (format "image/%s" (if (eq type 'jpeg) "jpeg" (or type 'png)))
                   (and data t) :x x :y y :width width :height height)))))

(defun lispm-desktop-platinum--face-attribute (faces attribute)
  "Return ATTRIBUTE of FACES, a face property's value, or nil.
The first face naming it wins, as in Emacs's own merging."
  (catch 'found
    (dolist (face (if (or (symbolp faces) (keywordp (car-safe faces))) (list faces) faces))
      (let ((value (cond ((and (symbolp face) (facep face)) (face-attribute face attribute nil t))
                         ((consp face) (or (plist-get face attribute)
                                           (when-let* ((parent (plist-get face :inherit)))
                                             (lispm-desktop-platinum--face-attribute parent attribute)))))))
        (unless (or (null value) (eq value 'unspecified))
          (throw 'found value))))
    nil))

(defun lispm-desktop-platinum--draw-content (svg content x top height color face)
  "Draw CONTENT, a string as Emacs would show it, on SVG from X.
Its runs are centred in the HEIGHT rows from TOP: words and icon glyphs
in their faces layered over FACE, images as they are, stretches of space
as wide as they say.  Text without a colour of its own is drawn in COLOR.
Return the x where CONTENT ends."
  (let ((content (copy-sequence content))
        (start 0))
    (add-face-text-property 0 (length content) face t content)
    (while (< start (length content))
      (let* ((end (or (next-property-change start content) (length content)))
             (run (substring content start end))
             (width (string-pixel-width run))
             (display (get-text-property 0 'display run))
             (image (and (eq (car-safe display) 'image) display)))
        (cond
         (image
          ;; As wide as Emacs lays it out, which `image-size' can exceed
          ;; on a display that scales images up.
          (pcase-let* ((`(,iw . ,ih) (lispm-desktop-platinum--image-size image))
                       (w (if (> width 0) width iw))
                       (h (round (* ih (/ (float w) (max 1 iw))))))
            (lispm-desktop-platinum--draw-image svg image x (+ top (/ (- height h) 2)) w h)))
         ;; A stretch of space, or text shown as something else.
         ((or (eq (car-safe display) 'space) (stringp display)))
         ((not (string-blank-p run))
          (let* ((faces (get-text-property 0 'face run))
                 (family (lispm-desktop-platinum--face-attribute faces :family))
                 (base (face-attribute face :height nil t))
                 (scale (seq-reduce (lambda (size face)
                                      (let ((h (and (consp face) (plist-get face :height))))
                                        (if (floatp h) (* size h) size)))
                                    (ensure-list faces) 1.0))
                 (size (* scale (/ (if (numberp base) base 120) 10.0)))
                 (bold (memq (lispm-desktop-platinum--face-attribute faces :weight)
                             '(semi-bold bold extra-bold ultra-bold heavy black)))
                 (synthetic (and bold (display-graphic-p)
                                 (lispm-desktop-platinum--synthetic-bold-p `(:family ,family))))
                 (ink (or (lispm-desktop-platinum--face-attribute faces :foreground) color)))
            (lispm-desktop-platinum--label
             svg run x (+ top (round (/ (+ height (* 0.7 size)) 2.0))) ink
             (list :family (concat (if (stringp family) family "Geneva") ", sans-serif")
                   :size size :weight (if (and bold (not synthetic)) "bold" "normal")
                   :synthetic synthetic)
             "start"))))
        (setq x (+ x width)
              start end)))
    x))

(defun lispm-desktop-platinum--content-key (content)
  "Return what tells CONTENT apart from other contents, for memoizing:
its text and its properties, which `equal' alone ignores."
  (cons (substring-no-properties content) (object-intervals content)))

;;;;; Push buttons

(defun lispm-desktop-platinum--push-button-colors (palette state)
  "Return the colours of a push button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its :outline, the
:edge pixels rounding its corners, the outer and inner lines lit along
its top and left (:lit, :lit-inner), its :face, the inner and outer
lines shaded along its bottom and right (:shade-inner, :shade), the
pixels where lit and shaded meet (:corner, :corner-low, :corner-inner)
and its :label."
  (cl-flet ((g (level) (lispm-desktop-platinum-grey palette level)))
    (pcase state
      ('pressed
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #x44) :lit-inner (g #x55) :face (g #x66)
             :shade-inner (g #x77) :shade (g #x88)
             :corner (g #x44) :corner-low (g #x77) :corner-inner (g #x44)
             :label (lispm-desktop-platinum--legible (g #x66) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xDD)))
         (list :outline (g #x88) :edge (g #x88)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :corner face :corner-low face :corner-inner face
               :label (g #x77))))
      (_
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #xDD) :lit-inner (g #xFF) :face (g #xDD)
             :shade-inner (g #xAA) :shade (g #x77)
             :corner (g #xBB) :corner-low (g #xBB) :corner-inner (g #xFF)
             :label (plist-get palette :text))))))

(defun lispm-desktop-platinum--draw-push-frame (svg x y width colors)
  "Draw a push button WIDTH pixels wide from X, Y of SVG, without its label.
COLORS is a value of `lispm-desktop-platinum--push-button-colors'.  The
outline is rounded by stepping in a pixel on each of its first two rows;
inside it, two lines are lit along the top and left and two shaded along
the bottom and right.  The pixels its corners cut off are left clear."
  (let ((w width)
        (h lispm-desktop-platinum-button-height))
    (cl-flet ((dot (dx dy part)
                (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) (plist-get colors part)))
              (row (from to dy part)
                (lispm-desktop-platinum--row svg (+ x from) (+ x to) (+ y dy) (plist-get colors part)))
              (column (dx from to part)
                (lispm-desktop-platinum--column svg (+ x dx) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-desktop-platinum--fill svg (+ x 1) (+ y 1) (- w 2) (- h 2) (plist-get colors :face))
      ;; The outline.
      (row 3 (- w 4) 0 :outline)
      (row 3 (- w 4) (- h 1) :outline)
      (column 0 3 (- h 4) :outline)
      (column (- w 1) 3 (- h 4) :outline)
      (pcase-dolist (`(,dx . ,dy) `((1 . 1) (,(- w 2) . 1) (1 . ,(- h 2)) (,(- w 2) . ,(- h 2))))
        (dot dx dy :outline))
      (pcase-dolist (`(,dx . ,dy) `((2 . 0) (,(- w 3) . 0) (0 . 2) (,(- w 1) . 2)
                                    (0 . ,(- h 3)) (,(- w 1) . ,(- h 3))
                                    (2 . ,(- h 1)) (,(- w 3) . ,(- h 1))))
        (dot dx dy :edge))
      ;; Lit along the top and left.
      (row 3 (- w 4) 1 :lit)
      (row 3 (- w 4) 2 :lit-inner)
      (column 1 3 (- h 4) :lit)
      (column 2 3 (- h 4) :lit-inner)
      (dot 3 3 :lit-inner)
      ;; Shaded along the bottom and right.
      (column (- w 3) 3 (- h 4) :shade-inner)
      (column (- w 2) 3 (- h 4) :shade)
      (dot (- w 4) (- h 4) :shade-inner)
      (row 3 (- w 4) (- h 3) :shade-inner)
      (row 3 (- w 3) (- h 2) :shade)
      (dot (- w 3) (- h 3) :shade)
      (dot (- w 2) (- h 3) :shade)
      ;; Where lit meets shaded.
      (dot 2 1 :corner)
      (dot (- w 3) 1 :corner)
      (dot 1 2 :corner)
      (dot 1 (- h 3) :corner)
      (dot 2 2 :corner-inner)
      (dot (- w 2) 2 :corner-low)
      (dot 2 (- h 2) :corner-low)
      (dot (- w 3) 2 :face)
      (dot 2 (- h 3) :face))))

(defun lispm-desktop-platinum--draw-default-ring (svg x y width palette &optional disabled)
  "Draw the ring of a default push button WIDTH pixels wide, from X, Y of SVG.
The ring stands three pixels about the button: an outline rounded at its
corners, then a band lit along the top and left and shaded along the
bottom and right, which shows too where the button's corners are cut.
A DISABLED button's ring is a flat grey outline."
  (let* ((w (+ width 6))
         (h (+ lispm-desktop-platinum-button-height 6))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (if disabled (funcall grey #x88) (plist-get palette :outline)))
         (edge (if disabled outline (funcall grey #x22)))
         (lit (funcall grey #xDD))
         (lit-inner (if disabled lit (funcall grey #xAA)))
         (shade-inner (if disabled lit (funcall grey #xAA)))
         (shade (if disabled lit (funcall grey #x77)))
         (meet (if disabled lit (funcall grey #x88))))
    (cl-flet ((dot (dx dy color) (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) color))
              (row (from to dy color)
                (lispm-desktop-platinum--row svg (+ x from) (+ x to) (+ y dy) color))
              (column (dx from to color)
                (lispm-desktop-platinum--column svg (+ x dx) (+ y from) (+ y to) color)))
      ;; The band.
      (row 3 (- w 4) 1 lit)
      (row 2 3 2 lit)
      (row 4 (- w 5) 2 lit-inner)
      (column 1 3 (- h 4) lit)
      (column 2 3 3 lit)
      (column 2 4 (- h 5) lit-inner)
      (column (- w 3) 4 (- h 5) shade-inner)
      (column (- w 2) 3 (- h 4) shade)
      (row 4 (- w 5) (- h 3) shade-inner)
      (row 3 (- w 4) (- h 2) shade)
      (dot (- w 3) (- h 4) meet)
      (dot (- w 4) (- h 3) meet)
      (dot (- w 3) (- h 3) shade)
      ;; The corners the button's outline cuts off.
      (pcase-dolist (`(,cx ,cy ,sx ,sy) `((3 3 1 1) (,(- w 4) 3 -1 1)
                                          (3 ,(- h 4) 1 -1) (,(- w 4) ,(- h 4) -1 -1)))
        (dot cx cy shade-inner)
        (dot (+ cx sx) cy shade)
        (dot cx (+ cy sy) shade))
      ;; The outline.
      (row 4 (- w 5) 0 outline)
      (row 4 (- w 5) (- h 1) outline)
      (column 0 4 (- h 5) outline)
      (column (- w 1) 4 (- h 5) outline)
      (pcase-dolist (`(,dx . ,dy) `((2 . 1) (1 . 2) (,(- w 3) . 1) (,(- w 2) . 2)
                                    (1 . ,(- h 3)) (2 . ,(- h 2))
                                    (,(- w 2) . ,(- h 3)) (,(- w 3) . ,(- h 2))))
        (dot dx dy outline))
      (pcase-dolist (`(,dx . ,dy) `((3 . 0) (,(- w 4) . 0) (0 . 3) (,(- w 1) . 3)
                                    (0 . ,(- h 4)) (,(- w 1) . ,(- h 4))
                                    (3 . ,(- h 1)) (,(- w 4) . ,(- h 1))))
        (dot dx dy edge)))))

(defun lispm-desktop-platinum--draw-focus-ring (svg x y width height radius palette)
  "Draw on SVG the keyboard focus ring about the WIDTH by HEIGHT box at X, Y.
The ring is two pixels in PALETTE's focus colour just outside the box,
its corners rounded by RADIUS, as Mac OS 9 ringed the control the
keyboard was on."
  (svg-rectangle svg (- x 1) (- y 1) (+ width 2) (+ height 2)
                 :rx (+ radius 1) :fill "none"
                 :stroke (plist-get palette :focus) :stroke-width 2))

(defun lispm-desktop-platinum-push-button-width (label)
  "Return the width of a push button reading LABEL: room for it, at least 60.
LABEL is measured in the titles' face, the system font."
  (max 60 (+ 24 (string-pixel-width (propertize label 'face 'lispm-desktop-platinum-title)))))

(cl-defun lispm-desktop-platinum-draw-push-button (palette label width
                                                          &key state default focused (pad 0) font)
  "Return an SVG of a push button WIDTH pixels wide reading LABEL, in PALETTE.
The button is `lispm-desktop-platinum-button-height' tall, drawn as Mac
OS 9 drew it: rounded, outlined, lit along the top and left, its label
centred in the system font.  STATE is nil; `pressed', held down, dark
and lit from below, its label light; or `disabled', outlined in grey,
flat, its label dimmed.

DEFAULT rings the button, as the one Return presses; the ring takes
three pixels about it.  FOCUSED rings it, or its default ring, in the
focus colour, two pixels more, as the button Space presses: LispM's own
addition, for the keyboard.  PAD is the room left clear about the
button, raised to what its rings need, so buttons drawn with the same
PAD keep their places as the focus moves.  FONT is a value of
`lispm-desktop-platinum-svg-font', by default the system font's."
  (let* ((ring (if default 3 0))
         (pad (max pad (+ ring (if focused 2 0))))
         (h lispm-desktop-platinum-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad)))))
    (lispm-desktop-platinum--draw-push-button svg pad pad palette label width
                                              :state state :default default :focused focused :font font)
    svg))

(cl-defun lispm-desktop-platinum--draw-push-button (svg x y palette label width
                                                        &key state default focused font)
  "Draw on SVG at X, Y a push button WIDTH pixels wide reading LABEL.
Its rings, when DEFAULT or FOCUSED, are drawn about it; see
`lispm-desktop-platinum-draw-push-button' for the rest."
  (let ((ring (if default 3 0))
        (h lispm-desktop-platinum-button-height)
        (colors (lispm-desktop-platinum--push-button-colors palette state)))
    (when focused
      (lispm-desktop-platinum--draw-focus-ring svg (- x ring) (- y ring)
                                               (+ width (* 2 ring)) (+ h (* 2 ring))
                                               (+ 3 ring) palette))
    (when default
      (lispm-desktop-platinum--draw-default-ring svg (- x 3) (- y 3) width palette
                                                 (eq state 'disabled)))
    (lispm-desktop-platinum--draw-push-frame svg x y width colors)
    (lispm-desktop-platinum--label svg label (+ x (/ width 2.0)) (+ y 14)
                                   (plist-get colors :label)
                                   (or font (lispm-desktop-platinum-svg-font)))))

;;;;; Bevel buttons

(defun lispm-desktop-platinum--bevel-button-colors (palette state)
  "Return the colours of a bevel button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its outline lit
along the top and left (:outline-lit) and shaded along the bottom and
right (:outline-shade), its two lit lines (:lit, :lit-inner), its :face,
its two shaded lines (:shade-inner, :shade) and its :label."
  (cl-flet ((g (level) (lispm-desktop-platinum-grey palette level)))
    (pcase state
      ('pressed
       (list :outline-lit (g #x11) :outline-shade (g #x44)
             :lit (g #x44) :lit-inner (g #x66) :face (g #x88)
             :shade-inner (g #x99) :shade (g #xBB)
             :label (lispm-desktop-platinum--legible (g #x88) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xCC)))
         (list :outline-lit (g #x99) :outline-shade (g #x99)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :label (g #x77))))
      (_
       (list :outline-lit (g #x66) :outline-shade (g #x33)
             :lit (g #xCC) :lit-inner (g #xFF) :face (g #xCC)
             :shade-inner (g #x99) :shade (g #x77)
             :label (plist-get palette :text))))))

(cl-defun lispm-desktop-platinum-draw-bevel-button (palette width height &key state label
                                                            (face 'lispm-desktop-platinum-list))
  "Return an SVG of a bevel button WIDTH by HEIGHT pixels, in PALETTE.
Bevel buttons are square, as Mac OS 9's toolbars held them: an outline
darker along the bottom and right, two lines lit along the top and left
and two shaded along the bottom and right.  STATE is nil, `pressed'
\(darkened, its bevel turned over) or `disabled' (flat, its outline and
label greyed).  LABEL, when given, is centred: a string as Emacs would
show it in FACE, its icons and images included, see
`lispm-desktop-platinum--draw-content'."
  (let ((svg (svg-create width height))
        (colors (lispm-desktop-platinum--bevel-button-colors palette state))
        (w width)
        (h height))
    (cl-flet ((row (from to y part) (lispm-desktop-platinum--row svg from to y (plist-get colors part)))
              (column (x from to part) (lispm-desktop-platinum--column svg x from to (plist-get colors part))))
      (lispm-desktop-platinum--fill svg 0 0 w h (plist-get colors :face))
      (row 1 (- w 2) 1 :lit)
      (column 1 1 (- h 2) :lit)
      (row 2 (- w 3) 2 :lit-inner)
      (column 2 2 (- h 3) :lit-inner)
      (column (- w 3) 2 (- h 3) :shade-inner)
      (row 2 (- w 3) (- h 3) :shade-inner)
      (column (- w 2) 1 (- h 2) :shade)
      (row 1 (- w 2) (- h 2) :shade)
      (row 0 (- w 1) 0 :outline-lit)
      (column 0 0 (- h 1) :outline-lit)
      (column (- w 1) 1 (- h 1) :outline-shade)
      (row 1 (- w 1) (- h 1) :outline-shade))
    (when label
      (lispm-desktop-platinum--draw-content
       svg label (/ (- w (lispm-desktop-platinum--content-width label face)) 2) 0 h
       (plist-get colors :label) face))
    svg))

(defun lispm-desktop-platinum--content-width (content face)
  "Return the width in pixels of CONTENT shown in FACE."
  (let ((content (copy-sequence content)))
    (add-face-text-property 0 (length content) face t content)
    (string-pixel-width content)))

;;;;; Tabs

(defconst lispm-desktop-platinum--tab-slant
  '((10 . t) (8 . t) (7 . t) (6 . t) (6) (5 . t) (5) (5) (4 . t) (4) (4)
    (3 . t) (3) (3) (2 . t) (2) (2) (1 . t) (1) (1) (0 . t))
  "Where a tab's left side runs, row by row from its top, as (COLUMN . STEP).
COLUMN holds the outline; STEP is non-nil on a row where the outline
steps out, which is softened there.  The right side is the mirror.")

(cl-defun lispm-desktop-platinum-draw-tab (palette label width &key front
                                                   (face 'lispm-desktop-platinum-title))
  "Return an SVG of a tab WIDTH pixels wide reading LABEL, in PALETTE.
Tabs are trapezoids, their sides slanting out a pixel every three rows
from top corners rounded off, and stand on the edge of the pane they
choose among: the SVG is `lispm-desktop-platinum-tab-height' tall, and
three rows more for that edge.  The tab in FRONT is the pane's colour
and opens into it; a tab behind is darker, lit along its top and left,
and the pane's outline runs under it.  LABEL is a string as Emacs would
show it in FACE, by default the system font's, its icons included; every
label is black, none heavier than the others."
  (let* ((h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (plist-get palette :outline))
         (edge (funcall grey #x22))
         (body (funcall grey (if front #xEE #xCC)))
         (between (funcall grey #xCC))
         (lit (funcall grey (if front #xFF #xDD)))
         (soft (funcall grey (if front #xBB #xAA)))
         (shade (funcall grey #x88))
         (shade-soft (funcall grey #x44))
         (shade-lit (funcall grey (if front #xDD #xCC))))
    (cl-flet ((dot (x y color) (lispm-desktop-platinum--dot svg x y color))
              (row (from to y color) (lispm-desktop-platinum--row svg from to y color)))
      (lispm-desktop-platinum--fill svg 0 0 w h (plist-get palette :chrome))
      (seq-do-indexed
       (pcase-lambda (`(,left . ,step) y)
         (let ((right (- w 1 left)))
           (pcase y
             (0 (row (1+ left) (1- right) 0 outline)
                (dot left 0 edge) (dot right 0 edge))
             (1 (row (+ left 2) (- right 2) 1 between)
                (dot left 1 edge) (dot (1+ left) 1 outline) (dot (+ left 2) 1 soft)
                (dot right 1 edge) (dot (1- right) 1 outline))
             (2 (row (+ left 3) (- right 3) 2 lit)
                (dot left 2 edge) (dot (1+ left) 2 soft) (dot (+ left 2) 2 between)
                (dot right 2 edge) (dot (1- right) 2 shade) (dot (- right 2) 2 shade-lit))
             (_ (row (1+ left) (1- right) y body)
                (dot left y (if step edge outline))
                (dot right y (if step edge outline))
                (if (= y 3)
                    (progn (dot (1+ left) y soft) (dot (+ left 2) y between) (dot (+ left 3) y lit))
                  (dot (1+ left) y (if step soft between))
                  (dot (+ left 2) y lit))
                (dot (1- right) y (if step shade-soft shade))
                (when front (dot (- right 2) y shade-lit))))))
       lispm-desktop-platinum--tab-slant)
      ;; The pane's edge: its outline, then a lit band, under the tabs
      ;; behind; the tab in front opens into the pane.
      (if front
          (progn
            (row 0 (1- w) h body)
            (row 0 (1- w) (+ h 1) body)
            (row 0 (1- w) (+ h 2) body)
            (dot 0 h outline) (dot 1 h between) (dot 2 h lit)
            (dot (- w 1) h outline) (dot (- w 2) h soft) (dot (- w 3) h shade-lit)
            (row 0 1 (+ h 1) between) (row (- w 2) (- w 1) (+ h 1) between)
            (dot 0 (+ h 2) (funcall grey #xFF)) (dot (- w 1) (+ h 2) (funcall grey #xFF)))
        (row 0 (1- w) h outline)
        (row 0 (1- w) (+ h 1) between)
        (row 0 (1- w) (+ h 2) (funcall grey #xFF))))
    (lispm-desktop-platinum--draw-content
     svg label (/ (- w (lispm-desktop-platinum--content-width label face)) 2) 0 h
     (plist-get palette :text) face)
    svg))

(defun lispm-desktop-platinum-draw-tab-edge (palette width)
  "Return an SVG of the pane's edge WIDTH pixels wide, beside or between tabs.
It is as tall as a tab and the pane's edge under it, see
`lispm-desktop-platinum-draw-tab': the window's body above, the pane's
outline and lit band below, in PALETTE."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-desktop-platinum--fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-platinum--row svg 0 (1- width) h (plist-get palette :outline))
    (lispm-desktop-platinum--row svg 0 (1- width) (+ h 1) (lispm-desktop-platinum-grey palette #xCC))
    (lispm-desktop-platinum--row svg 0 (1- width) (+ h 2) (lispm-desktop-platinum-grey palette #xFF))
    svg))

;;;;; Placards, radio buttons, check boxes

(cl-defun lispm-desktop-platinum-draw-placard (palette width height &key label font)
  "Return an SVG of a placard WIDTH by HEIGHT pixels, in PALETTE.
A placard is the plate Mac OS 9 showed status on, as the Finder window
counted its items on one: outlined, lit along the top and left, shaded
along the bottom and right.  LABEL, when given, is centred in FONT, a
value of `lispm-desktop-platinum-svg-font', by default the list's."
  (let ((svg (svg-create width height))
        (w width)
        (h height))
    (lispm-desktop-platinum--fill svg 0 0 w h (plist-get palette :outline))
    (lispm-desktop-platinum--fill svg 1 1 (- w 2) (- h 2) (lispm-desktop-platinum-grey palette #xDD))
    (lispm-desktop-platinum--row svg 1 (- w 2) 1 (lispm-desktop-platinum-grey palette #xFF))
    (lispm-desktop-platinum--column svg 1 1 (- h 2) (lispm-desktop-platinum-grey palette #xFF))
    (lispm-desktop-platinum--row svg 2 (- w 2) (- h 2) (lispm-desktop-platinum-grey palette #xAA))
    (lispm-desktop-platinum--column svg (- w 2) 2 (- h 2) (lispm-desktop-platinum-grey palette #xAA))
    (when label
      (let ((font (or font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))))
        (lispm-desktop-platinum--label svg label (/ w 2.0)
                                       (round (+ (/ h 2.0) (* 0.35 (plist-get font :size))))
                                       (plist-get palette :text) font)))
    svg))

(defun lispm-desktop-platinum--gradient (svg id from to)
  "Define on SVG the gradient ID, from colour FROM at the top left to TO at
the bottom right."
  (let ((defs (or (car (dom-by-tag svg 'defs))
                  (let ((defs (dom-node 'defs)))
                    (dom-add-child-before svg defs)
                    defs))))
    (dom-append-child defs (dom-node 'linearGradient
                                     `((id . ,id) (x1 . 0) (y1 . 0) (x2 . 1) (y2 . 1))
                                     (dom-node 'stop `((offset . "0%") (stop-color . ,from)))
                                     (dom-node 'stop `((offset . "100%") (stop-color . ,to)))))))

(cl-defun lispm-desktop-platinum-draw-radio (palette on &key state focused (pad 0))
  "Return an SVG of a radio button in PALETTE, ON or off.
It is a twelve-pixel sphere, outlined, shaded from white at its top left
to grey at its bottom right, with a dot when ON.  STATE is nil,
`pressed' (darkened) or `disabled' (flat and grey).  FOCUSED rings it in
the focus colour, two pixels out; PAD is the room left about it, raised
to two pixels for the ring."
  (let* ((pad (max pad (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (disabled (eq state 'disabled))
         (from (funcall grey (if (eq state 'pressed) #x88 #xFF)))
         (to (funcall grey (if (eq state 'pressed) #x55 #x88))))
    (lispm-desktop-platinum--gradient svg "sphere" from to)
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :focus) :stroke-width 2))
    (svg-circle svg middle middle 5.5 :fill (if disabled (funcall grey #xDD) "url(#sphere)")
                :stroke (if disabled (funcall grey #x88) (plist-get palette :outline))
                :stroke-width 1)
    (when on
      (svg-circle svg middle middle 2
                  :fill (if disabled
                            (funcall grey #x88)
                          (lispm-desktop-platinum--legible
                           (lispm-desktop-tab-bar--blend from to 0.5)
                           (plist-get palette :outline) (plist-get palette :text)))))
    svg))

(cl-defun lispm-desktop-platinum-draw-check-box (palette on &key state)
  "Return an SVG of a check box in PALETTE, checked when ON.
The box is twelve pixels square, outlined, lit along its top and left
and shaded along its bottom and right; the check mark breaks out past
its top right corner, as Mac OS 9's did, so the SVG is fourteen pixels
square, the box at its bottom left.  STATE is nil, `pressed' (darkened)
or `disabled' (flat and grey)."
  (let* ((svg (svg-create 14 14))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (disabled (eq state 'disabled))
         (pressed (eq state 'pressed))
         (ink (if disabled (funcall grey #x88) (plist-get palette :outline))))
    (lispm-desktop-platinum--fill svg 0 2 12 12 ink)
    (lispm-desktop-platinum--fill svg 1 3 10 10 (funcall grey (if pressed #xAA #xDD)))
    (unless disabled
      (lispm-desktop-platinum--row svg 1 9 3 (funcall grey (if pressed #x88 #xFF)))
      (lispm-desktop-platinum--column svg 1 3 11 (funcall grey (if pressed #x88 #xFF)))
      (lispm-desktop-platinum--row svg 2 10 12 (funcall grey (if pressed #xDD #x88)))
      (lispm-desktop-platinum--column svg 10 4 12 (funcall grey (if pressed #xDD #x88))))
    (when on
      (svg-polyline svg '((2.5 . 7.5) (5.5 . 10.5) (12.5 . 1.5))
                    :fill "none" :stroke ink :stroke-width 2 :stroke-linecap "square"))
    svg))

;;;;; Window frames

(defconst lispm-desktop-platinum-frame-inset '(6 . 1)
  "Where a floating window stands inside its frame, as (LEFT . TOP) pixels:
past the outline and the four-pixel band and the content's line on the
left, right and bottom, and right under the outline at the top, where
its title bar runs.")

(cl-defun lispm-desktop-platinum-draw-window-frame (palette width height &key active)
  "Return an SVG of a window's frame WIDTH by HEIGHT pixels, in PALETTE.
The SVG takes in the outline and the drop shadow, a pixel along the
right and bottom; the window stands inside it at
`lispm-desktop-platinum-frame-inset', its title bar under the outline.
An ACTIVE window is outlined in black and framed in a band lit from the
top left (white, two of platinum, a shade), then the content's own
black line; beside the title bar the band takes the bar's colours, so
the bar reads as running from edge to edge.  An inactive window is
outlined in a dark grey, its frame flat.  The middle is left clear."
  (let* ((w width)
         (h height)
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (if active (plist-get palette :outline) (funcall grey #x55)))
         (title lispm-desktop-platinum-title-bar-height)
         (right (- w 2))
         (bottom (- h 2)))
    (cl-flet ((row (from to y color) (lispm-desktop-platinum--row svg from to y color))
              (column (x from to color) (lispm-desktop-platinum--column svg x from to color)))
      ;; The shadow.
      (column (1- w) 2 (1- h) (plist-get palette :outline))
      (row 2 (1- w) (1- h) (plist-get palette :outline))
      ;; The band, flat to begin with.
      (lispm-desktop-platinum--fill svg 1 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-desktop-platinum--fill svg (- w 7) 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-desktop-platinum--fill svg 1 (- h 7) (- w 3) 5 (funcall grey (if active #xCC #xDD)))
      (if (not active)
          (progn
            (column 5 (1+ title) (- h 7) outline)
            (column (- w 7) (1+ title) (- h 7) outline)
            (row 5 (- w 7) (- h 7) outline)
            (row 1 5 title outline)
            (row (- w 7) (- w 3) title outline))
        ;; Lit from the top left: white on the outer line there, on the
        ;; inner one along the bottom and right; shaded likewise.
        (column 1 1 (- h 4) (funcall grey #xFF))
        (row 1 (- w 4) (- h 3) (funcall grey #x99))
        (column (- w 3) 2 (- h 3) (funcall grey #x99))
        (column 4 (1+ title) (- h 7) (funcall grey #x99))
        (column (- w 6) (1+ title) (- h 6) (funcall grey #xFF))
        (row 4 (- w 6) (- h 6) (funcall grey #xFF))
        ;; The content's black line.
        (column 5 title (- h 7) outline)
        (column (- w 7) title (- h 7) outline)
        (row 5 (- w 7) (- h 7) outline)
        ;; Beside the title bar, the bar's own lines.
        (row 1 5 1 (funcall grey #xFF))
        (row (- w 7) (- w 4) 1 (funcall grey #xFF))
        (row 2 5 (1- title) (funcall grey #x99))
        (row (- w 7) (- w 4) (1- title) (funcall grey #x99))
        (row 1 5 title outline)
        (row (- w 7) (- w 3) title outline))
      ;; The outline.
      (row 0 right 0 outline)
      (row 0 right bottom outline)
      (column 0 0 bottom outline)
      (column right 0 bottom outline))
    svg))

;;;;; The Control Strip

(defconst lispm-desktop-platinum--strip-arrow
  '("    l   " "   ll   " "  ljllll" " ljjjjjl" "ljjjjjjl" "ljjjjjjl" " ljjjjjl" "  ljllll"
    "   ll   " "    l   ")
  "The Control Strip's left arrow, a row of pixels a string: l its outline,
j its fill.  The right arrow is its mirror.")

(defconst lispm-desktop-platinum--strip-collapse
  '("llllllll" "lccccccc" "lcrrrrqb" "lcrrrrqb" "lcrrrrqb" "lcrrrrqb" "lcqqqqqb" "lcbbbbbb")
  "The glyph of the Control Strip's collapse box, a row of pixels a string.")

(defconst lispm-desktop-platinum--strip-tab
  '("#########........" "#fffffff##......." "#fwwwwwww##......" "#fwfffffff##....."
    "#fwwfffffff##...." "#fwfgfffffff##..." "#fwfffwffffff##.." "#fwffffgff##ff##."
    "#fwwffffff#w#ff##" "#fwfgfffff#ww#f8#" "#fwfffwfff#ww#f8#" "#fwffffgff#ww#f8#"
    "#fwwffffff#ww#f8#" "#fwfgfffff#ww#f8#" "#fwfffwfff#ww#f8#" "#fwffffgff#w#ff##"
    "#fwwffffff##ff##." "#fwfgffffffff##.." "#fwfffffffff##..." "#fwffffffff##...."
    "#fwfffffff##....." "#f8888888##......" "#8444444##......." "#########........")
  "The Control Strip's tab, a row of pixels a string: its outline (#), a
platinum face (f) lit along its top and left (w), dotted with a grip
(w over g), shaded along its bottom (8, 4), and a pointed end holding a
handle.")

(defun lispm-desktop-platinum-strip-segment-width (kind)
  "Return the width in pixels of a Control Strip segment of KIND, see
`lispm-desktop-platinum-draw-strip-segment'."
  (pcase kind
    ('collapse 15)
    ((or 'left 'right) 14)
    ('module 28)
    ('tab 17)))

(cl-defun lispm-desktop-platinum-draw-strip-segment (palette kind &key state icon menu focused behind)
  "Return an SVG of a segment of the Control Strip of KIND, in PALETTE.
KIND is `collapse', the box at the strip's left end, which also draws the
strip's left outline; `left' or `right', the arrows scrolling the
modules; `module', a module showing ICON, a sixteen-pixel SVG file or
SVG drawing, and a small triangle when it has a MENU; or `tab', the
strip's right end.  Every segment but the tab ends in the black line
parting it from the next.  STATE is nil, `pressed' (darkened, lit from
below) or `disabled' (sunk, its arrow greyed); FOCUSED rings a module
in the focus colour, as the keyboard reaches it.  BEHIND is the colour
round the tab's pointed end, what the strip lies on.  The segment is
`lispm-desktop-platinum-strip-height' tall, its outline included, and
drawn in the strip's own greys, :strip and its light and shades."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (plist-get palette :outline))
         (face (plist-get palette :strip))
         (light (plist-get palette :strip-light))
         (shade (plist-get palette :strip-shade))
         (dark (plist-get palette :strip-dark))
         ;; Arrows and triangles black on Platinum's greys; on a dark
         ;; theme's, its text colour.
         (ink (lispm-desktop-platinum--legible face outline (plist-get palette :text))))
    (if (eq kind 'tab)
        (progn
          (when behind
            (lispm-desktop-platinum--fill svg 0 0 w h behind))
          (lispm-desktop-platinum--draw-map
           svg 0 0 lispm-desktop-platinum--strip-tab
         `((?# . ,outline) (?f . ,(funcall grey #xCC)) (?w . ,light)
           (?g . ,(funcall grey #x55)) (?8 . ,(funcall grey #x88)) (?4 . ,(funcall grey #x44)))))
      (let* ((left (if (eq kind 'collapse) 1 0))
             (right (- w 2))
             (pressed (eq state 'pressed))
             (sunk (or pressed (eq state 'disabled))))
        (lispm-desktop-platinum--fill svg 0 0 w h outline)
        (lispm-desktop-platinum--fill svg left 1 (- right left -1) (- h 2) (if pressed dark face))
        (cl-flet ((band (top-left bottom-right width)
                    (dotimes (i width)
                      (lispm-desktop-platinum--row svg (+ left i) (- right i) (+ 1 i) (nth i top-left))
                      (lispm-desktop-platinum--column svg (+ left i) (+ 1 i) (- h 2 i) (nth i top-left))
                      (lispm-desktop-platinum--row svg (+ left i 1) (- right i) (- h 2 i)
                                                   (nth i bottom-right))
                      (lispm-desktop-platinum--column svg (- right i) (+ 1 i) (- h 2 i)
                                                      (nth i bottom-right)))))
          (cond (pressed
                 (let ((deep (lispm-desktop-tab-bar--blend dark outline 0.5)))
                   (band (list deep deep) (list face face) 2)))
                (sunk (band (list dark) (list light) 1))
                (t (band (list face light) (list dark shade) 2))))
        (pcase kind
          ('collapse
           (lispm-desktop-platinum--draw-map
            svg 3 8 lispm-desktop-platinum--strip-collapse
            `((?l . ,(funcall grey #x55)) (?c . ,(plist-get palette :thumb-light))
              (?r . ,(funcall grey #x88)) (?q . ,(funcall grey #x44)) (?b . ,light))))
          ((or 'left 'right)
           (let ((colors (if (eq state 'disabled)
                             `((?l . ,(funcall grey #x55)) (?j . ,(funcall grey #xDD)))
                           `((?l . ,ink) (?j . ,ink)))))
             (lispm-desktop-platinum--draw-map
              svg 2 7 (if (eq kind 'left) lispm-desktop-platinum--strip-arrow
                        (mapcar #'reverse lispm-desktop-platinum--strip-arrow))
              colors)))
          ('module
           (when focused
             (dotimes (i 2)
               (lispm-desktop-platinum--row svg (+ left i) (- right i) (+ 1 i) (plist-get palette :focus))
               (lispm-desktop-platinum--row svg (+ left i) (- right i) (- h 2 i) (plist-get palette :focus))
               (lispm-desktop-platinum--column svg (+ left i) (+ 1 i) (- h 2 i) (plist-get palette :focus))
               (lispm-desktop-platinum--column svg (- right i) (+ 1 i) (- h 2 i) (plist-get palette :focus))))
           (cond ((stringp icon) (lispm-desktop-platinum--embed-icon svg icon 2 3 16))
                 (icon (lispm-desktop-platinum--embed-svg svg icon 2 3 16 16)))
           (when menu
             (lispm-desktop-platinum--draw-map
              svg 21 8 '("#" "##" "###" "####" "####" "###" "##" "#")
              `((?# . ,ink))))))))
    svg))

;;;; Controls as text
;;
;; The controls above as strings to put on a line, of a buffer or of a
;; mode, header or tab line: a character displaying the control's image,
;; memoized, which the caller gives its keymap, help and target.

(defconst lispm-desktop-platinum-bevel-padding 8
  "Pixels between a bevel button's sides and its label.")

(defconst lispm-desktop-platinum-tab-padding 13
  "Pixels between a tab's sides, where they meet the pane, and its label.")

(defun lispm-desktop-platinum-bevel-button (label &optional state height face)
  "Return a bevel button reading LABEL, as a string displaying it.
LABEL is shown as Emacs would show it in FACE, by default the list's,
its icons and images included; STATE is nil, `pressed' or `disabled';
HEIGHT defaults to 20 pixels.  See `lispm-desktop-platinum-draw-bevel-button'."
  (let* ((face (or face 'lispm-desktop-platinum-list))
         (height (or height 20))
         (width (+ (lispm-desktop-platinum--content-width label face)
                   (* 2 lispm-desktop-platinum-bevel-padding))))
    (propertize " " 'display
                (lispm-desktop-platinum--image
                 (list 'bevel (lispm-desktop-platinum--content-key label) state height face width)
                 (lambda ()
                   (lispm-desktop-platinum-draw-bevel-button lispm-desktop-platinum--palette width height
                                                             :state state :label label :face face))))))

(cl-defun lispm-desktop-platinum-push-button (label &key state default focused pad)
  "Return a push button reading LABEL, as a string displaying it.
STATE, DEFAULT, FOCUSED and PAD are as `lispm-desktop-platinum-draw-push-button'
takes them; PAD defaults to two pixels, room for the focus ring, so a
button keeps its place as the keyboard comes and goes."
  (let ((width (lispm-desktop-platinum-push-button-width label))
        (pad (or pad (if default 5 2))))
    (propertize " " 'display
                (lispm-desktop-platinum--image
                 (list 'push label width state default focused pad)
                 (lambda ()
                   (lispm-desktop-platinum-draw-push-button lispm-desktop-platinum--palette label width
                                                            :state state :default default
                                                            :focused focused :pad pad))))))

(defun lispm-desktop-platinum--tab-edge (width)
  "Return the pane's edge WIDTH pixels wide, as a string displaying it."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'tab-edge width)
               (lambda () (lispm-desktop-platinum-draw-tab-edge lispm-desktop-platinum--palette width)))))

(defun lispm-desktop-platinum-tabs (tabs width &optional face)
  "Return a row of TABS standing on their pane's edge, WIDTH pixels wide.
Each tab is (LABEL FRONT . PROPERTIES): LABEL shown as Emacs would show
it in FACE, by default the system font's; FRONT non-nil for the tab in
front; PROPERTIES, text properties the tab's string carries, such as its
keymap and help.  The row opens with a short run of the pane's edge and
runs it on to WIDTH, as Mac OS 9 drew a tab control along its pane."
  (let* ((face (or face 'lispm-desktop-platinum-title))
         (lead 8)
         (used lead)
         (parts
          (mapcar (pcase-lambda (`(,label ,front . ,properties))
                    (let ((tab-width (+ (lispm-desktop-platinum--content-width label face)
                                        (* 2 lispm-desktop-platinum-tab-padding))))
                      (setq used (+ used tab-width 2))
                      (concat (apply #'propertize " " 'display
                                     (lispm-desktop-platinum--image
                                      (list 'tab (lispm-desktop-platinum--content-key label) front face tab-width)
                                      (lambda ()
                                        (lispm-desktop-platinum-draw-tab lispm-desktop-platinum--palette
                                                                         label tab-width :front front
                                                                         :face face)))
                                     properties)
                              (lispm-desktop-platinum--tab-edge 2))))
                  tabs)))
    (concat (lispm-desktop-platinum--tab-edge lead)
            (apply #'concat parts)
            (when (> width used)
              (lispm-desktop-platinum--tab-edge (- width used))))))

(defun lispm-desktop-platinum-radio (on &optional state focused)
  "Return a radio button, ON or off, in STATE, as a string displaying it.
FOCUSED rings it; either way it leaves room for the ring, so it keeps its
place as the keyboard comes and goes.  See `lispm-desktop-platinum-draw-radio'."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'radio on state focused)
               (lambda () (lispm-desktop-platinum-draw-radio lispm-desktop-platinum--palette on
                                                             :state state :focused focused :pad 2)))))

(defun lispm-desktop-platinum-check-box (on &optional state)
  "Return a check box, checked when ON, in STATE, as a string displaying it.
See `lispm-desktop-platinum-draw-check-box'."
  (propertize " " 'display
              (lispm-desktop-platinum--image
               (list 'check-box on state)
               (lambda () (lispm-desktop-platinum-draw-check-box lispm-desktop-platinum--palette on
                                                                 :state state)))))

;;;; Controls in buffers
;;
;; Push buttons and radio buttons inserted in a buffer, as Control Panels
;; holds them, are buttons of button.el: Tab and Shift-Tab go from one to
;; the next, Return presses the one point is on.  Point itself is not
;; shown in such a buffer, so the control it is on is drawn ringed in the
;; focus colour, as Mac OS 9 ringed the control the keyboard was on; a
;; push button pressed, from the keyboard or with the mouse, is drawn
;; pushed in until it is let go.

(defvar-local lispm-desktop-platinum--focus nil
  "Position of the control drawn focused in this buffer, or nil.")

(defun lispm-desktop-platinum--swap-image (position image)
  "Show IMAGE for the control at POSITION of the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property position (1+ position) 'display image))))

(defun lispm-desktop-platinum-show-focus ()
  "Draw the control at point focused, and the one focused before plain.
Controls carry their images as the text properties
`lispm-desktop-platinum-plain' and `lispm-desktop-platinum-focused'.
Buffers holding controls run this from `post-command-hook', and after
drawing their controls again."
  (let ((here (and (get-text-property (point) 'lispm-desktop-platinum-focused) (point)))
        (before lispm-desktop-platinum--focus))
    (unless (eql here before)
      (when (and before (< before (point-max))
                 (get-text-property before 'lispm-desktop-platinum-plain))
        (lispm-desktop-platinum--swap-image before (get-text-property before 'lispm-desktop-platinum-plain)))
      (when here
        (lispm-desktop-platinum--swap-image here (get-text-property here 'lispm-desktop-platinum-focused)))
      (setq lispm-desktop-platinum--focus here))))

(defun lispm-desktop-platinum--press-at (position)
  "Draw the push button at POSITION pushed in, until it is let go.
From the mouse, it is let go when the button rises, and pressed only if
it rises over the push button, as on Mac OS 9; from the keyboard, it
blinks.  Return non-nil when the button was pressed."
  (let ((pressed (get-text-property position 'lispm-desktop-platinum-pressed))
        (shown (get-text-property position 'display)))
    (lispm-desktop-platinum--swap-image position pressed)
    (redisplay t)
    (prog1 (if (not (mouse-event-p last-input-event))
               (progn (sit-for 0.1) t)
             (let ((release (let ((echo-keystrokes 0)) (read-event))))
               (and (mouse-event-p release)
                    (eq (posn-window (event-end release)) (posn-window (event-start last-input-event)))
                    (eql (posn-point (event-end release)) position))))
      (lispm-desktop-platinum--swap-image position shown))))

(defun lispm-desktop-platinum-push (&optional position)
  "Press the push button at POSITION, by default point's, as Return and
Space do, or as a press of the mouse does; other buttons are pushed as
`push-button' pushes them."
  (interactive)
  (let ((position (or position (point))))
    (if (and (get-text-property position 'lispm-desktop-platinum-pressed)
             (not (lispm-desktop-platinum--press-at position)))
        nil
      (push-button position))))

(defun lispm-desktop-platinum--mouse-push (event)
  "Press the push button EVENT went down on, see `lispm-desktop-platinum-push'."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (lispm-desktop-platinum-push (posn-point position)))))

(defvar-keymap lispm-desktop-platinum-push-button-map
  :doc "Keymap of a push button in a buffer."
  :parent button-map
  "RET" #'lispm-desktop-platinum-push
  "SPC" #'lispm-desktop-platinum-push
  "<down-mouse-1>" #'lispm-desktop-platinum--mouse-push
  "<mouse-1>" #'ignore)

(defun lispm-desktop-platinum--image-of (control)
  "Return the image CONTROL, a string displaying one, shows."
  (get-text-property 0 'display control))

(cl-defun lispm-desktop-platinum-insert-push-button (label action &key help default)
  "Insert a push button reading LABEL, which calls ACTION with no arguments.
HELP describes it; DEFAULT rings it, as the button Return presses.  The
button is drawn focused while point is on it, see
`lispm-desktop-platinum-show-focus'; its text, drawn in its image, is
kept in its `lispm-desktop-platinum-label' property."
  (insert-text-button " "
                      'display (lispm-desktop-platinum--image-of
                                (lispm-desktop-platinum-push-button label :default default))
                      'lispm-desktop-platinum-plain (lispm-desktop-platinum--image-of
                                                     (lispm-desktop-platinum-push-button label :default default))
                      'lispm-desktop-platinum-focused (lispm-desktop-platinum--image-of
                                                       (lispm-desktop-platinum-push-button
                                                        label :default default :focused t))
                      'lispm-desktop-platinum-pressed (lispm-desktop-platinum--image-of
                                                       (lispm-desktop-platinum-push-button
                                                        label :default default :state 'pressed))
                      'action (lambda (_) (funcall action))
                      'lispm-desktop-platinum-label label
                      'face nil 'keymap lispm-desktop-platinum-push-button-map
                      'help-echo (or help label)))

;;;; Icons
;;
;; Finder's small icons, drawn as sixteen-pixel SVGs in icons/os9: a
;; folder (badged when it holds a git repository), a hard disk, documents
;; (text, code, Lisp, Markdown, Org, picture, blank), a compact Macintosh
;; standing for a terminal, a small robot for the agents and LispM's own
;; application diamond.
;; Unlike the chrome, icons keep their own colours, as Mac OS 9's did
;; whatever the appearance.

(defvar lispm-desktop-platinum--icons (make-hash-table :test #'equal)
  "Icon images by name.")

(defun lispm-desktop-platinum-icon-file (name)
  "Return the file of icon NAME: a file of icons/os9 without its extension,
`lambda' for LispM's rainbow λ, or `emacs' for the icon Emacs ships with,
read from this Emacs's own data directory."
  (pcase name
    ('lambda (expand-file-name "icons/lispm-lambda.png" lispm-desktop-directory))
    ('emacs (let ((file (expand-file-name "images/icons/hicolor/scalable/apps/emacs.svg"
                                          data-directory)))
              (if (file-readable-p file)
                  file
                (lispm-desktop-platinum-icon-file "document"))))
    (_ (expand-file-name (format "icons/os9/%s.svg" name) lispm-desktop-directory))))

(defun lispm-desktop-platinum-icon (name)
  "Return the Finder icon NAME as a string one icon wide.
NAME is as `lispm-desktop-platinum-icon-file' takes it.  Where images cannot be
shown, a blank stands in."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    ;; The icon carries a face of its own, so a button it sits in, such as
    ;; a list row, does not underline it.
    (if (and (display-images-p) (file-readable-p file))
        (propertize " " 'face 'lispm-desktop-platinum-list
                    'display (or (gethash file lispm-desktop-platinum--icons)
                                 (puthash file (create-image file nil nil :height 16
                                                             :ascent 'center)
                                          lispm-desktop-platinum--icons)))
      (propertize " " 'face 'lispm-desktop-platinum-list 'display '(space :width (16))))))

(defconst lispm-desktop-platinum-lisp-extensions '("el" "lisp" "lsp" "cl" "scm" "ss" "rkt" "clj" "cljs"
                                                   "cljc" "edn" "fnl")
  "Extensions of files shown with the Lisp document icon.")

(defconst lispm-desktop-platinum-code-extensions
  '("c" "h" "cc" "cpp" "hpp" "m" "mm" "swift" "rs" "go" "zig" "py" "rb" "js" "jsx" "ts" "tsx"
    "java" "kt" "scala" "hs" "ml" "ex" "exs" "erl" "lua" "php" "sh" "bash" "zsh" "fish" "nix"
    "sol" "sql" "css" "scss" "html" "xml" "json" "yaml" "yml" "toml" "proto" "mk" "cmake")
  "Extensions of files shown with the code document icon.")

(defun lispm-desktop-platinum-file-icon-name (file)
  "Return the name of the document icon of FILE, by its kind."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((string-match-p (image-file-name-regexp) file) "document-image")
          ((member extension lispm-desktop-platinum-lisp-extensions) "document-lisp")
          ((member extension '("md" "markdown" "mkd")) "document-markdown")
          ((equal extension "org") "document-org")
          ((member extension lispm-desktop-platinum-code-extensions) "document-code")
          (t "document"))))

(defun lispm-desktop-platinum-folder-icon (directory)
  "Return the folder icon of DIRECTORY: badged when it is a git repository."
  (lispm-desktop-platinum-icon (if (and directory
                                        (file-exists-p (expand-file-name ".git" directory)))
                                   "folder-git"
                                 "folder")))

(defvar lispm-directory)

(defvar-local lispm-desktop-platinum--icon-cache nil
  "This buffer's icon as (KEY PALETTE . ICON): what it was chosen from.")

(defun lispm-desktop-platinum-buffer-icon (buffer)
  "Return the Finder icon of BUFFER.
An agent shows its own mark; a terminal, a compact Macintosh; a
directory, a folder; LispM's own records, LispM's diamond; a file, the
document of its kind; any other buffer, a blank document.  Title bars
ask at every redisplay, so the icon is kept until what it was chosen
from changes."
  (with-current-buffer buffer
    (let ((key (list major-mode buffer-file-name default-directory (buffer-name))))
      (if (and (equal (car lispm-desktop-platinum--icon-cache) key)
               (eq (cadr lispm-desktop-platinum--icon-cache) lispm-desktop-platinum--palette))
          (cddr lispm-desktop-platinum--icon-cache)
        (cddr (setq lispm-desktop-platinum--icon-cache
                    (cons key (cons lispm-desktop-platinum--palette
                                    (lispm-desktop-platinum--choose-icon buffer)))))))))

(defun lispm-desktop-platinum--choose-icon (buffer)
  "Return the Finder icon of BUFFER, see `lispm-desktop-platinum-buffer-icon'."
  (with-current-buffer buffer
    (cond ((and (fboundp 'lispm-desktop-agent--buffer-p) (lispm-desktop-agent--buffer-p buffer))
           (lispm-desktop-agent--icon (lispm-desktop-agent--buffer-agent buffer)))
          ((derived-mode-p 'eat-mode 'lispm-desktop-term-mode 'term-mode 'shell-mode 'eshell-mode)
           (lispm-desktop-platinum-icon "computer"))
          ((derived-mode-p 'dired-mode) (lispm-desktop-platinum-folder-icon default-directory))
          ((and buffer-file-name (boundp 'lispm-directory)
                (file-in-directory-p buffer-file-name lispm-directory))
           (lispm-desktop-platinum-icon "lispm"))
          (buffer-file-name
           (lispm-desktop-platinum-icon
            (let ((name (lispm-desktop-platinum-file-icon-name buffer-file-name)))
              (cond ((not (equal name "document")) name)
                    ((derived-mode-p 'lisp-data-mode) "document-lisp")
                    ((derived-mode-p 'prog-mode) "document-code")
                    (t name)))))
          (t (lispm-desktop-platinum-icon "document-blank")))))

;;;; Title bars

(defvar-local lispm-desktop-platinum-title nil
  "Title the window of this buffer shows instead of the buffer's own name.")

(defvar-local lispm-desktop-platinum-icon nil
  "Icon the window of this buffer shows before its title, a string.")

(defvar-local lispm-desktop-platinum-box-functions nil
  "What the title bar boxes of this buffer's window do instead of their own.
A plist of :close, :zoom and :collapse functions, each called with the
window, for chrome whose boxes act on what it frames, such as the
browser's address window or the editor's title strip.  Where the plist is
set, a box without a function is drawn disabled.")

(defvar-local lispm-desktop-platinum-active-function nil
  "Function deciding whether this buffer's title bar is drawn active, or nil.
Called with the window, it lets chrome look active along with what it
frames, as the editor's title strip does while the editor is in front.")

(defun lispm-desktop-platinum--box-function (window box)
  "Return the function WINDOW's buffer gives title bar BOX, or nil.
BOX is :close, :zoom or :collapse; see `lispm-desktop-platinum-box-functions'."
  (plist-get (buffer-local-value 'lispm-desktop-platinum-box-functions (window-buffer window)) box))

(defun lispm-desktop-platinum-no-title-bar ()
  "Give the current buffer's windows no title bar, as for transient popups."
  (setq-local tab-line-format nil))

(defun lispm-desktop-platinum--buffer-title (buffer)
  "Return the title of BUFFER's window."
  (or (buffer-local-value 'lispm-desktop-platinum-title buffer)
      (and (fboundp 'lispm-desktop-agent-buffer-label) (lispm-desktop-agent-buffer-label buffer))
      (buffer-name buffer)))

(defun lispm-desktop-platinum--buffer-icon (buffer)
  "Return the proxy icon of BUFFER's window, as Finder's windows showed one."
  (or (buffer-local-value 'lispm-desktop-platinum-icon buffer)
      (lispm-desktop-platinum-buffer-icon buffer)))

(defun lispm-desktop-platinum--window-command (command)
  "Return a mouse command calling COMMAND with the window clicked."
  (lambda (event)
    (interactive "e")
    (funcall command (posn-window (event-start event)))))

(defun lispm-desktop-platinum-press (position pressed-p command)
  "Hold a button down from the press at POSITION until the mouse button rises.
The button is drawn pushed in, through the window parameter
`lispm-desktop-platinum-pressed' that the drawing reads, until the release;
COMMAND then runs, with no arguments, only when PRESSED-P accepts the
release's position: as on Mac OS 9, moving off a button before letting go
cancels."
  (let ((window (posn-window position))
        (target (lispm-desktop-platinum--press-target position)))
    (set-window-parameter window 'lispm-desktop-platinum-pressed target)
    (force-mode-line-update t)
    (redisplay t)
    (let ((release (let ((echo-keystrokes 0)) (read-event))))
      (set-window-parameter window 'lispm-desktop-platinum-pressed nil)
      (force-mode-line-update t)
      (when (and (consp release)
                 (eq (event-basic-type release) 'mouse-1)
                 (funcall pressed-p (event-end release)))
        (funcall command)))))

(defun lispm-desktop-platinum--press-target (position)
  "Return what the press at POSITION is on: the target named there.
That is the text property `lispm-desktop-platinum-target'."
  (when-let* ((string (posn-string position)))
    (get-text-property (cdr string) 'lispm-desktop-platinum-target (car string))))

(defun lispm-desktop-platinum--same-target-p (position)
  "Return a predicate true of positions on the same target as POSITION."
  (let ((window (posn-window position))
        (area (posn-area position))
        (target (lispm-desktop-platinum--press-target position)))
    (lambda (release)
      (and (eq (posn-window release) window)
           (eq (posn-area release) area)
           (equal (lispm-desktop-platinum--press-target release) target)))))

(defun lispm-desktop-platinum--pressed-p (target)
  "Return non-nil while TARGET is held down in the window being drawn."
  (equal (window-parameter (selected-window) 'lispm-desktop-platinum-pressed) target))

(defun lispm-desktop-platinum--box-button (string kind command help)
  "Return STRING as title bar box KIND, which runs COMMAND with its window.
HELP describes it.  It is pressed rather than clicked, see
`lispm-desktop-platinum-press'."
  (propertize string
              'help-echo help
              'lispm-desktop-platinum-target kind
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>"
                                    (lambda (event)
                                      (interactive "e")
                                      (let ((start (event-start event)))
                                        (lispm-desktop-platinum-press
                                         start (lispm-desktop-platinum--same-target-p start)
                                         (lambda () (funcall command (posn-window start)))))))
                        (keymap-set map "<tab-line> <mouse-1>" #'ignore)
                        map)))

(defvar lispm-desktop-platinum-frame-moved-functions nil
  "Functions called with a child frame its title bar or grow box moved or
resized, so it can be kept in place.")

(defun lispm-desktop-platinum--movable-p (frame)
  "Return non-nil when FRAME moves by its title bar: a child frame, or a
floating window out on the computer's screen."
  (or (frame-parent frame) (frame-parameter frame 'lispm-desktop-window)))

(defun lispm-desktop-platinum--title-press (event)
  "Drag the child frame whose top title bar EVENT pressed, as a window.
A window at the top of a child frame, such as the floating editor's
title strip, moves the frame, as Mac OS 9 moved a window by its title;
elsewhere the press does nothing and the release selects the window."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (lispm-desktop-platinum--movable-p frame) (window-at-side-p window 'top))
      (mouse-drag-frame-move event)
      (run-hook-with-args 'lispm-desktop-platinum-frame-moved-functions frame))))

(defvar lispm-desktop-platinum--title-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'lispm-desktop-platinum--title-press)
    (keymap-set map "<tab-line> <mouse-1>"
                (lispm-desktop-platinum--window-command #'select-window))
    (keymap-set map "<tab-line> <double-down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-mouse-1>"
                (lispm-desktop-platinum--window-command #'lispm-desktop-platinum-shade-or-zoom))
    (keymap-set map "<tab-line> <drag-mouse-1>" #'lispm-desktop-platinum-drag-window)
    (keymap-set map "<tab-line> M-<down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> M-<mouse-1>"
                (lispm-desktop-platinum--window-command #'lispm-desktop-platinum-title-menu))
    map)
  "Keymap of a title bar outside its boxes, as Mac OS 9's title bars
behaved: a click brings the window forward, a double click rolls it up,
a drag moves the window, and a Command click (Meta) pops up its buffers.")

(defconst lispm-desktop-platinum--title-help
  (format "Drag onto another window to swap them; double-click to roll up; %s-click for \
the window's buffers"
          (if (eq system-type 'darwin) "⌘" "M"))
  "Help shown over a title bar.")

(declare-function lispm-desktop-main-area--window-p "lispm-desktop-interface" (window))
(declare-function lispm-desktop-main-area--terminal-p "lispm-desktop-interface" (window))

(defun lispm-desktop-platinum-drag-window (event)
  "Move the window whose title bar EVENT dragged to where it was dropped.
Tiled windows cannot float as Mac OS 9's did, so a window dropped on
another trades places with it.  The main area's columns hold: a session
trades places only with a session, anything else only with another
window that is not a session."
  (interactive "e")
  (let ((from (posn-window (event-start event)))
        (to (posn-window (event-end event))))
    (when (and (windowp to) (window-live-p to) (not (eq from to))
               (lispm-desktop-main-area--window-p from) (lispm-desktop-main-area--window-p to))
      (if (eq (and (lispm-desktop-main-area--terminal-p from) t)
              (and (lispm-desktop-main-area--terminal-p to) t))
          (progn (window-swap-states from to)
                 (select-window to))
        (message "Sessions keep to the left, everything else to the right")))))

(defun lispm-desktop-platinum--space (width)
  "Return a blank WIDTH pixels wide."
  (propertize " " 'display `(space :width (,(max 0 width)))))

(defun lispm-desktop-platinum--fit (string width)
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

(defun lispm-desktop-platinum-title-layout (width title-width &optional controls)
  "Return how a title bar WIDTH pixels wide lays out a title TITLE-WIDTH wide.
CONTROLS, as `lispm-desktop-platinum--window-controls' returns them,
says which boxes the bar holds, by default all three.  The value is a
plist: the title stretch, between the boxes, runs from :start to :end;
the title, as wide as :title, starts at :label-x; :ridges lists the
\(FROM . TO) columns of the ridges either side.  Spacing is Mac OS 9's:
the close box three pixels in from the edge, four pixels to the ridges,
four from the ridges to the zoom box, three to the collapse box and
three to the edge.  The title is centred in the bar unless that would
push it into the boxes; a title too wide is narrowed so that a short run
of ridges stays on either side."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (s lispm-desktop-platinum-box-size)
               (start (if close (+ 3 s) 0))
               (end (- width 3 (if zoom s 0) (if (and zoom collapse) 3 0) (if collapse s 0)))
               (from (+ start (if close 4 7)))
               (to (- end (if (or zoom collapse) 5 8)))
               (pad 6)
               (least 12)
               (title (max 0 (min title-width (- to from (* 2 (+ pad least))))))
               (label-x (max (+ from least pad)
                             (min (/ (- width title) 2) (- to least pad title)))))
    (list :start start :end end :title title :label-x label-x
          :ridges (list (cons from (- label-x pad)) (cons (+ label-x title pad) to)))))

(defun lispm-desktop-platinum--fixed-p (window)
  "Return non-nil when WINDOW keeps its place and size: a side window, such
as the sidebar, or chrome, such as the browser's address window."
  (or (window-parameter window 'window-side)
      (window-parameter window 'lispm-desktop-chrome)))

(defun lispm-desktop-platinum--window-controls (window)
  "Return the title bar boxes of WINDOW as (CLOSE ZOOM COLLAPSE).
Each is `on' or nil, a box the window lacks being left out, as Mac OS 9
left it out.  A side window or chrome closes but neither zooms nor
collapses; a window collapses only when another shares its column."
  (let ((fixed (lispm-desktop-platinum--fixed-p window))
        (functions (buffer-local-value 'lispm-desktop-platinum-box-functions (window-buffer window))))
    (if functions
        (mapcar (lambda (box) (and (plist-get functions box) 'on))
                '(:close :zoom :collapse))
      (list 'on
            (and (not fixed) 'on)
            (and (not fixed) (window-combined-p window) 'on)))))

(defun lispm-desktop-platinum--title-bar (window)
  "Return the title bar of WINDOW, drawn for its current state."
  (let* ((buffer (window-buffer window))
         ;; Chrome wearing another window's title bar, as the browser's
         ;; address window wears the page's, is active with that window.
         (active (or (mode-line-window-selected-p)
                     (eq (window-parameter window 'lispm-desktop-platinum-title-for)
                         (old-selected-window))
                     (when-let* ((active (buffer-local-value 'lispm-desktop-platinum-active-function
                                                             buffer)))
                       (funcall active window))))
         (pressed (window-parameter window 'lispm-desktop-platinum-pressed))
         (width (- (window-pixel-width window)
                   (window-right-divider-width window)
                   (window-scroll-bar-width window)))
         (icon (lispm-desktop-platinum--buffer-icon buffer))
         (title (lispm-desktop-platinum--buffer-title buffer))
         (modified (and (buffer-file-name buffer) (buffer-modified-p buffer)))
         (controls (lispm-desktop-platinum--window-controls window))
         ;; What a terminal's program says it is doing, see lispm-desktop-term.
         (status (when-let* (((fboundp 'lispm-desktop-term-status))
                             (record (lispm-desktop-term-status buffer)))
                   (lispm-desktop-term-status-label record)))
         ;; A window's frame drawn about its floating frame lights and
         ;; shades the bar's ends already.
         (framed (lispm-desktop-platinum--framed-p (window-frame window)))
         (edges (cons (not (and framed (window-at-side-p window 'left)))
                      (not (and framed (window-at-side-p window 'right)))))
         (key (list lispm-desktop-platinum--generation width active icon title modified controls
                    pressed status edges))
         (cached (window-parameter window 'lispm-desktop-platinum-title-bar)))
    (if (equal (car cached) key)
        (cdr cached)
      (let ((bar (lispm-desktop-platinum--draw-title-bar width active icon title modified controls
                                                         pressed status edges)))
        (set-window-parameter window 'lispm-desktop-platinum-title-bar (cons key bar))
        bar))))

(defun lispm-desktop-platinum--draw-title-bar (width active icon title modified controls
                                                     &optional pressed status edges)
  "Return a title bar WIDTH pixels wide for ICON and TITLE.
ACTIVE draws the ridges and boxes; an inactive bar lies flat, its title
greyed.  MODIFIED marks the close box; CONTROLS is a value of
`lispm-desktop-platinum--window-controls'; PRESSED names the box held
down, if any; STATUS, a coloured label, follows the title.  EDGES,
\(LEAD . TRAIL), says whether the bar lights its left end and shades its
right, by default both.  The bar is drawn as images, a stretch each
between its boxes, so its lines run under the title too."
  (pcase-let* ((label (concat (when icon (concat icon " ")) title
                              (when status (concat "  " status))))
               (face (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))
               (`(,close ,zoom ,collapse) controls)
               (layout (lispm-desktop-platinum-title-layout
                        width (lispm-desktop-platinum--content-width label face) controls))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum--fit label (plist-get layout :title))))
               (start (plist-get layout :start))
               (end (plist-get layout :end))
               (lead (if edges (car edges) t))
               (trail (if edges (cdr edges) t))
               (title-part
                (lambda (from to &rest options)
                  (propertize (apply #'lispm-desktop-platinum--title-part (- to from) :active active
                                     :label label :label-x (- (plist-get layout :label-x) from)
                                     options)
                              'keymap lispm-desktop-platinum--title-map
                              'help-echo lispm-desktop-platinum--title-help))))
    (if (not active)
        (funcall title-part 0 width)
      (cl-flet ((box (kind command help)
                  (lispm-desktop-platinum--box-button
                   (propertize " " 'display (lispm-desktop-platinum--box kind (and (eq kind 'close) modified)
                                                                        (eq pressed kind)))
                   kind command help))
                (plain (width &rest options)
                  (apply #'lispm-desktop-platinum--title-part width :active t options)))
        (concat
         (when close
           (concat (plain 3 :lead lead)
                   (box 'close #'lispm-desktop-platinum-close-window "Close this window")))
         (funcall title-part start end :lead (and lead (not close))
                  :ridges (mapcar (pcase-lambda (`(,from . ,to)) (cons (- from start) (- to start)))
                                  (plist-get layout :ridges)))
         (when zoom
           (box 'zoom #'lispm-desktop-platinum-zoom-window "Zoom this window"))
         (when (and zoom collapse)
           (plain 3))
         (when collapse
           (box 'collapse #'lispm-desktop-platinum-collapse-window "Collapse this window to its title bar"))
         (plain 3 :trail trail))))))

(defun lispm-desktop-platinum-title-bar ()
  "The tab line of every window: its title bar.
See `lispm-desktop-platinum--title-bar'."
  (lispm-desktop-platinum--title-bar (selected-window)))

;;;; Title bar commands

(declare-function lispm-desktop-show "lispm-desktop-surface" (&optional window))

(defun lispm-desktop-platinum-close-window (window)
  "Close WINDOW as Mac OS closed a window: put its document away.
A sidebar is hidden.  Otherwise WINDOW's buffer is killed, once an
alert has asked about unsaved changes or a running process, and WINDOW
goes too when other windows remain in the main area; the last one shows
the desktop."
  (cond
   ((lispm-desktop-platinum--box-function window :close)
    (funcall (lispm-desktop-platinum--box-function window :close) window))
   ((window-parameter window 'window-side)
    (when-let* ((view (lispm-desktop-sidebar-current-view)))
      (lispm-desktop-sidebar-select view)))
   (t
    (let ((buffer (window-buffer window)))
      (when (lispm-desktop-platinum--confirm-close buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        ;; The main area's last window uncovers the desktop, as closing
        ;; Mac OS 9's last window did; any other window goes.
        (when (window-live-p window)
          (if (cdr (lispm-desktop-main-area--windows))
              (delete-window window)
            (when (fboundp 'lispm-desktop-show)
              (lispm-desktop-show window)))))))))

(defun lispm-desktop-platinum--confirm-close (buffer)
  "Ask, as Mac OS 9 asked, whether BUFFER may close; return non-nil if so.
A modified file asks to be saved first, and is saved when told to; a
buffer whose process still runs asks before the process is stopped."
  (let ((name (lispm-desktop-platinum--buffer-title buffer))
        (process (get-buffer-process buffer)))
    (cond
     ((and (buffer-file-name buffer) (buffer-modified-p buffer))
      (pcase (lispm-desktop-popup-alert
              'caution (format "Save changes to the document “%s” before closing?" name)
              '(("Don’t Save" discard nil ?\M-d) ("Cancel" cancel cancel) ("Save" save default))
              "If you don’t save, your changes will be lost.")
        ('save (with-current-buffer buffer (save-buffer)) t)
        ('discard (with-current-buffer buffer (set-buffer-modified-p nil)) t)))
     ((and process (process-live-p process))
      (eq 'close (lispm-desktop-popup-alert
                  'caution (format "Close “%s”?" name)
                  '(("Cancel" cancel cancel) ("Close" close default))
                  "What runs in it will be stopped.")))
     (t t))))

(defun lispm-desktop-platinum--yes-or-no (prompt &rest _)
  "Ask PROMPT, a question answered yes or no, in an alert; non-nil for Yes."
  (eq 'yes (lispm-desktop-popup-alert
            'caution
            (replace-regexp-in-string "\\s-*([^()]*)\\s-*\\'" "" (string-trim prompt))
            '(("No" no cancel ?n) ("Yes" yes default ?y)))))

(defun lispm-desktop-platinum--names (names)
  "Return NAMES as a phrase: the first three quoted, then a count of the rest."
  (let ((quoted (mapcar (lambda (name) (format "“%s”" (string-trim name "\\*" "\\*")))
                        (seq-take names 3)))
        (rest (- (length names) 3)))
    (cond ((> rest 0)
           (format "%s and %d other%s" (string-join quoted ", ") rest (if (= rest 1) "" "s")))
          ((cdr quoted)
           (concat (string-join (butlast quoted) ", ") " and " (car (last quoted))))
          (t (car quoted)))))

(defun lispm-desktop-platinum--running-programs ()
  "Return the titles of the buffers whose programs quitting Emacs stops:
those of Emacs's processes that ask before they are killed, and of
lispm-desktop-term's terminals, whose programs are not Emacs's processes."
  (delete-dups
   (append
    (delq nil (mapcar (lambda (process)
                        (when (and (memq (process-status process) '(run stop open listen))
                                   (process-query-on-exit-flag process))
                          (if (buffer-live-p (process-buffer process))
                              (lispm-desktop-platinum--buffer-title (process-buffer process))
                            (process-name process))))
                      (process-list)))
    (mapcar #'lispm-desktop-platinum--buffer-title
            (and (fboundp 'lispm-desktop-term-running-buffers) (lispm-desktop-term-running-buffers))))))

(defun lispm-desktop-platinum--unsaved-documents ()
  "Return the buffers `save-buffers-kill-emacs' offers to save."
  (if (fboundp 'files--buffers-needing-to-be-saved)
      (files--buffers-needing-to-be-saved t)
    (seq-filter (lambda (buffer) (and (buffer-file-name buffer) (buffer-modified-p buffer)))
                (buffer-list))))

(defun lispm-desktop-platinum--quit-documents (documents restart)
  "Ask of each of DOCUMENTS whether to save it before quitting, or
RESTART'ing, as Mac OS 9 asked; return nil if one was cancelled."
  (catch 'cancel
    (dolist (buffer documents)
      (pcase (lispm-desktop-popup-alert
              'caution (format "Save changes to the document “%s” before %s?"
                               (lispm-desktop-platinum--buffer-title buffer)
                               (if restart "restarting" "quitting"))
              ;; y and n answer as `save-some-buffers' takes them.
              '(("Don’t Save" discard nil ?\M-d ?n) ("Cancel" cancel cancel)
                ("Save" save default ?y))
              "If you don’t save, your changes will be lost.")
        ('save (with-current-buffer buffer (save-buffer)))
        ('cancel (throw 'cancel nil))))
    t))

(defun lispm-desktop-platinum--quit (quit &optional arg restart)
  "Quit Emacs as QUIT, `save-buffers-kill-emacs', does, asking in alerts
instead of the minibuffer: whether to save each modified document, and
whether to stop the programs still running, agents in lispm-desktop-term's
terminals among them.  ARG saves every document unasked; RESTART starts
Emacs again.  Where alerts cannot be drawn, QUIT asks."
  (if (not (lispm-desktop-popup-available-p))
      (funcall quit arg restart)
    (cl-letf (((symbol-function 'yes-or-no-p) #'lispm-desktop-platinum--yes-or-no)
              ((symbol-function 'y-or-n-p) #'lispm-desktop-platinum--yes-or-no))
      (let* ((verb (if restart "Restart" "Quit"))
             (documents (lispm-desktop-platinum--unsaved-documents))
             (programs (and confirm-kill-processes (lispm-desktop-platinum--running-programs))))
        (when (and (if arg
                       (progn (save-some-buffers t t) t)
                     (lispm-desktop-platinum--quit-documents documents restart))
                   ;; Abbrevs and the like, which ask yes or no.
                   (progn (dolist (function save-some-buffers-functions)
                            (funcall function nil arg))
                          t)
                   (or (null programs)
                       (eq 'quit (lispm-desktop-popup-alert
                                  'caution (format "%s Emacs while programs are running?" verb)
                                  `(("Cancel" cancel cancel) (,verb quit default))
                                  (format "%s stops %s." (if restart "Restarting" "Quitting")
                                          (lispm-desktop-platinum--names programs)))))
                   (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                   ;; As `save-buffers-kill-emacs', confirm only when nothing
                   ;; was asked already.
                   (or (null confirm-kill-emacs) documents programs
                       (funcall confirm-kill-emacs
                                (format "Really %s Emacs? " (downcase verb)))))
          (kill-emacs nil restart))))))

(defun lispm-desktop-platinum-zoom-window (window)
  "Zoom WINDOW to the whole main area, or back to where it was.
The sidebar stays, as `delete-other-windows' leaves side windows be."
  (let ((saved (frame-parameter nil 'lispm-desktop-platinum-unzoom)))
    (cond
     ((lispm-desktop-platinum--box-function window :zoom)
      (funcall (lispm-desktop-platinum--box-function window :zoom) window))
     ((and saved (eq (car saved) window) (not (cdr (lispm-desktop-main-area--windows))))
      (set-frame-parameter nil 'lispm-desktop-platinum-unzoom nil)
      (set-window-configuration (cdr saved)))
     (t
      (set-frame-parameter nil 'lispm-desktop-platinum-unzoom
                           (cons window (current-window-configuration)))
      (delete-other-windows window)))))

(defun lispm-desktop-platinum-collapse-window (window)
  "Roll WINDOW up to its title bar, or down again, as a window shade.
Only a window sharing its column with another can roll up."
  (cond
   ((lispm-desktop-platinum--box-function window :collapse)
    (funcall (lispm-desktop-platinum--box-function window :collapse) window))
   ((window-parameter window 'lispm-desktop-platinum-shade)
    (let ((height (window-parameter window 'lispm-desktop-platinum-shade)))
      (set-window-parameter window 'lispm-desktop-platinum-shade nil)
      (window-resize window (- height (window-total-height window)) nil t)))
   ((window-combined-p window)
    (set-window-parameter window 'lispm-desktop-platinum-shade (window-total-height window))
    (window-resize window (- (window-min-size window nil t)
                             (window-total-height window))
                   nil t))))

(defconst lispm-desktop-platinum--rolled-up-locals
  '((window-min-height . 1) (window-safe-min-height . 0)
    (window-resize-pixelwise . t) (window-scroll-functions . nil))
  "The values the buffer of a rolled-up window holds as its own.
A window keeps `window-min-height' lines and, under its tab line, a line
of `window-safe-min-height', rounded up to whole lines unless resized
pixelwise; with these, it may be as short as its tab line.  With no line
of its text in sight, Emacs scrolls it at every redisplay, so no
`window-scroll-functions' run on it: the scroll bars' would have Emacs
redisplay again at once, and again.")

(defun lispm-desktop-platinum-roll-up (frame height)
  "Roll floating window FRAME up to HEIGHT pixels, as Mac OS 9's
WindowShade rolled a window up into its title bar.  FRAME's one window
may be as short as its title bar, its tab line: its buffer holds the
values of `lispm-desktop-platinum--rolled-up-locals' until
`lispm-desktop-platinum-roll-down'.  FRAME stays a pixel taller than a
line of its font, as Emacs drops the tab line of a window no taller."
  (let ((buffer (window-buffer (frame-root-window frame))))
    (with-current-buffer buffer
      (pcase-dolist (`(,variable . ,value) lispm-desktop-platinum--rolled-up-locals)
        (set (make-local-variable variable) value)))
    (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up
                         (cons buffer (frame-text-height frame)))
    (set-frame-height frame (max height (1+ (frame-char-height frame))) nil t)))

(defun lispm-desktop-platinum-rolled-up-height (frame)
  "Return the height in pixels of FRAME's text before it was rolled up, or
nil when FRAME is not rolled up."
  (cdr (frame-parameter frame 'lispm-desktop-platinum-rolled-up)))

(defun lispm-desktop-platinum-roll-down (frame height)
  "Roll floating window FRAME, rolled up by `lispm-desktop-platinum-roll-up',
down again to HEIGHT pixels."
  (when-let* ((buffer (car (frame-parameter frame 'lispm-desktop-platinum-rolled-up)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (dolist (local lispm-desktop-platinum--rolled-up-locals)
        (kill-local-variable (car local)))))
  (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)
  (set-frame-height frame height nil t))

(declare-function tabspaces--buffer-list "tabspaces" (&optional frame tabnum))

(declare-function lispm-desktop-sidebar-buffer-view "lispm-desktop-sidebar" (buffer))

(defun lispm-desktop-platinum--window-buffers (window)
  "Return the buffers WINDOW could show: its workspace's.
Hidden buffers and the sidebar's lists are left out."
  (seq-remove (lambda (buffer)
                (or (string-prefix-p " " (buffer-name buffer))
                    (lispm-desktop-sidebar-buffer-view buffer)))
              (if (bound-and-true-p tabspaces-mode)
                  (tabspaces--buffer-list (window-frame window))
                (buffer-list (window-frame window)))))

(defun lispm-desktop-platinum-shade-or-zoom (window)
  "Roll WINDOW up or down, or zoom it when it cannot roll up.
A double click on a title bar does this, as on Mac OS 9; a window alone
in its column cannot roll up, so it zooms instead.  A side window or
chrome does neither."
  (cond ((lispm-desktop-platinum--box-function window :collapse)
         (funcall (lispm-desktop-platinum--box-function window :collapse) window))
        ((lispm-desktop-platinum--fixed-p window))
        ((or (window-parameter window 'lispm-desktop-platinum-shade) (window-combined-p window))
         (lispm-desktop-platinum-collapse-window window))
        (t (lispm-desktop-platinum-zoom-window window))))

(defvar-local lispm-desktop-platinum-title-menu-function nil
  "Function returning the menu a Command click on this buffer's title pops
up, called with the window, or nil for the workspace's buffers.  The
Finder window gives its folder's path, as Mac OS 9's did.")

(defun lispm-desktop-platinum-title-menu (window)
  "Pop up the buffers WINDOW could show, and show the one chosen.
As Mac OS 9 popped up a window's path from its title, the title pops up
the workspace's buffers, the one WINDOW shows checked, or what its
buffer's `lispm-desktop-platinum-title-menu-function' gives.  A buffer
chosen goes to WINDOW unless the main area's columns send it elsewhere:
a session to the left, a file to the right."
  (lispm-desktop-popup-menu
   (if-let* ((function (buffer-local-value 'lispm-desktop-platinum-title-menu-function
                                           (window-buffer window))))
       (funcall function window)
     (mapcar (lambda (buffer)
               (list :label (lispm-desktop-platinum--buffer-title buffer)
                     :action (lambda ()
                               (select-window window)
                               (switch-to-buffer buffer))
                     :checked (eq buffer (window-buffer window))))
             (seq-take (lispm-desktop-platinum--window-buffers window) 30)))))

;;;; Menu bar

(defvar lispm-desktop-platinum--open-menu nil
  "The menu bar item whose menu is popped up, drawn highlighted meanwhile.")

(defvar lispm-desktop-platinum-menu-closed-hook nil
  "Hook run when a menu dropped from the menu bar closes, chosen or not.")

(defvar lispm-desktop-platinum--menu-bar-walk nil
  "Non-nil while the menu bar is walked from the keyboard, menu to menu:
the window that had the keyboard gets it back once the walk is over.")

(defmacro lispm-desktop-platinum--with-open-menu (key &rest body)
  "Run BODY, which pops up a menu, with menu bar item KEY highlighted."
  (declare (indent 1))
  `(let ((lispm-desktop-platinum--open-menu ,key))
     (force-mode-line-update t)
     (redisplay t)
     (unwind-protect (progn ,@body)
       (force-mode-line-update t)
       (unless lispm-desktop-platinum--menu-bar-walk
         (run-hooks 'lispm-desktop-platinum-menu-closed-hook)))))

(defconst lispm-desktop-platinum-menu-padding 10
  "Blank pixels on either side of a menu bar title.")

(defconst lispm-desktop-platinum--screen-corner
  '("#####5ADW" "###5AWWW" "##5DW" "#5DW" "#AW" "5W" "AW" "DW")
  "The screen's top left corner, rounded over the menu bar's left end, a
row of pixels a string: black (#) softened in greys (5, A, D) into the
bar's lit edges (W).  The right corner is its mirror.")

(cl-defun lispm-desktop-platinum-draw-menu-title (palette width &key label highlighted corner)
  "Return an SVG of a stretch of the menu bar WIDTH pixels wide, in PALETTE.
The bar is `lispm-desktop-platinum-menu-bar-height' tall: a lit top line,
platinum, a shaded line and a black line along its bottom.  LABEL, a
string as Emacs would show it in the menu titles' face, its icons
included, is centred; HIGHLIGHTED draws it as an open menu's title, in
the variation colour, light on dark.  CORNER, `left' or `right', rounds
the screen's corner over that end."
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum--fill svg 0 0 width h (funcall grey #xDD))
    (lispm-desktop-platinum--row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-desktop-platinum--row svg 0 (1- width) (- h 2) (funcall grey #x99))
    (lispm-desktop-platinum--row svg 0 (1- width) (1- h) (plist-get palette :outline))
    (when highlighted
      (lispm-desktop-platinum--fill svg 0 1 width (- h 3) (plist-get palette :highlight))
      (lispm-desktop-platinum--row svg 0 (1- width) 0 (plist-get palette :thumb-dark))
      (lispm-desktop-platinum--row svg 0 (1- width) (- h 2) (plist-get palette :highlight-dark)))
    (when label
      (lispm-desktop-platinum--draw-content
       svg label (/ (- width (lispm-desktop-platinum--content-width label 'lispm-desktop-platinum-menu-title)) 2)
       1 (- h 3)
       (plist-get palette (if highlighted :highlight-text :text))
       'lispm-desktop-platinum-menu-title))
    (when corner
      (let ((colors `((?# . ,(plist-get palette :outline)) (?5 . ,(funcall grey #x55))
                      (?A . ,(funcall grey #xAA)) (?D . ,(funcall grey #xDD))
                      (?W . ,(funcall grey (if (eq corner 'left) #xFF #xDD))))))
        (if (eq corner 'left)
            (progn
              (lispm-desktop-platinum--column svg 0 8 (- h 3) (funcall grey #xFF))
              (lispm-desktop-platinum--draw-map svg 0 0 lispm-desktop-platinum--screen-corner colors))
          (lispm-desktop-platinum--draw-map
           svg (- width 9) 0
           (mapcar (lambda (row) (reverse (concat row (make-string (- 9 (length row)) ?\s))))
                   lispm-desktop-platinum--screen-corner)
           (cons '(?W . nil) colors)))))
    svg))

(defun lispm-desktop-platinum--menu-title (key label command help &optional active corner)
  "Return the menu bar item KEY showing LABEL, which runs COMMAND.
HELP describes it; ACTIVE draws it highlighted, as an open menu, the
highlight spanning the title's padding, as Mac OS 9 highlighted a whole
title.  The title is drawn as an image, so the bar's lines run under it;
CORNER rounds the screen's corner over it, see
`lispm-desktop-platinum-draw-menu-title'.  The title string keeps LABEL
in its `lispm-desktop-platinum-label' property."
  (let* ((width (+ (lispm-desktop-platinum--content-width label 'lispm-desktop-platinum-menu-title)
                   (* 2 lispm-desktop-platinum-menu-padding)))
         (title (propertize " " 'display
                            (lispm-desktop-platinum--image
                             (list 'menu-title (lispm-desktop-platinum--content-key label) width active corner)
                             (lambda ()
                               (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum--palette width
                                                                       :label label :highlighted active
                                                                       :corner corner)))
                            'lispm-desktop-platinum-label label
                            'lispm-desktop-platinum-highlighted active)))
    `(,key menu-item ,title ,command :help ,help)))

(defun lispm-desktop-platinum--menu-filler (width)
  "Return the menu bar's stretch WIDTH pixels wide between its two ends."
  (list 'lispm-desktop-menu-filler 'menu-item
        (propertize " " 'display
                    (lispm-desktop-platinum--image
                     (list 'menu-title nil width nil nil)
                     (lambda () (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum--palette
                                                                        width))))
        #'ignore))

(defun lispm-desktop-platinum-menu-bar-line ()
  "Return the whole menu bar, as `tab-bar-format' takes it: the λ menu and
the frontmost application's menus at its left end, the status and the
Application menu at its right, and the bar's own stretch between them,
so its lines run from edge to edge."
  (let* ((left (lispm-desktop-platinum-menu-bar))
         (right (append (lispm-desktop-platinum-menu-status) (lispm-desktop-platinum-application-menu)))
         (used (apply #'+ (mapcar (lambda (item) (string-pixel-width (nth 2 item))) (append left right)))))
    (append left (list (lispm-desktop-platinum--menu-filler (max 0 (- (frame-inner-width) used)))) right)))

(defun lispm-desktop-platinum--lambda ()
  "Return LispM's rainbow λ, the counterpart of Mac OS 9's rainbow Apple.
Where images cannot be shown, a plain λ stands in."
  (if (display-images-p) (lispm-desktop-platinum-icon 'lambda) "λ"))

(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-available-p "lispm-desktop-popup" ())
(defvar lispm-desktop-popup-menu-bar-step)
(declare-function lispm-desktop-agent-installed-p "lispm-desktop-ai" (agent))
(defvar lispm-desktop-theme)

(defun lispm-desktop-platinum--menu-y ()
  "Return where a menu dropped from the menu bar starts: on the bar's black
bottom line, which the menu's outline then draws, as on Mac OS 9."
  (1- (tab-bar-height nil t)))

(defun lispm-desktop-platinum-lambda-items ()
  "Return the items of the λ menu, as `lispm-desktop-popup-menu' takes them."
  `((:label "About LispM" :action lispm-desktop-platinum-about)
    separator
    ,@(when (fboundp 'lispm-desktop-agent-lispm-new)
        '((:label "New LispM Agent" :action lispm-desktop-agent-lispm-new)))
    ,@(mapcan (pcase-lambda (`(,agent ,label ,command))
                (when (lispm-desktop-agent-installed-p agent)
                  (list (list :label label :action command))))
              '((claude "New Claude Code Session" lispm-desktop-agent-claude-new)
                (codex "New Codex Session" lispm-desktop-agent-codex-new)
                (hermes "New Hermes Session" lispm-desktop-agent-hermes-new)))
    (:label "Resume Session…" :action lispm-desktop-agent-sessions)
    (:label "Send Region to Agent" :action lispm-desktop-agent-send-region :enabled ,(use-region-p))
    separator
    (:label "New Shell" :action lispm-desktop-shell-new)
    ,@(when (lispm-desktop-platinum--browser-p) '((:label "Browser" :action lispm-desktop-browser-show)))
    (:label "Magit Status" :action magit-status)
    separator
    ,@(when (fboundp 'lispm-desktop-settings-panel-items)
        `((:label "Control Panels" :submenu lispm-desktop-settings-panel-items)))
    (:label "Customize lispm-desktop…" :action ,(lambda () (customize-group 'lispm-desktop)))))

(defun lispm-desktop-platinum-lambda-menu ()
  "Pop up the λ menu, LispM's counterpart of the Apple menu."
  (interactive)
  (lispm-desktop-platinum--with-open-menu 'lispm-desktop-lambda
    (lispm-desktop-popup-menu (lispm-desktop-platinum-lambda-items) 4 (lispm-desktop-platinum--menu-y))))

(defun lispm-desktop-platinum-about ()
  "Tell which LispM and lispm-desktop this is, as About This Computer did."
  (interactive)
  (lispm-desktop-popup-alert
   'note "LispM, a Lisp Machine for agents, inside Emacs."
   '(("OK" ok default))
   (format "lispm-desktop, Emacs %s.  github.com/RyanKung/lispm" emacs-version)))

(declare-function lispm-desktop-xwidget-available-p "lispm-desktop-browser" ())

(defun lispm-desktop-platinum--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lispm-desktop-xwidget-available-p) (lispm-desktop-xwidget-available-p)))

(defvar lispm-desktop-platinum-menus-function nil
  "Function returning the frontmost application's menus, or nil.
The menus follow the λ menu in the menu bar, as Mac OS 9's menu bar held
the frontmost application's: a list of (KEY TITLE ITEMS), ITEMS a function
returning the items `lispm-desktop-popup-menu' takes.  Without it the menu bar
holds a title per sidebar view, and Web.")

(defvar lispm-desktop-platinum-application-function nil
  "Function returning the frontmost application as (ICON . NAME), or nil.
The Application menu names it; without it, the workspace is named.")

(defvar lispm-desktop-platinum-applications-function nil
  "Function returning the Application menu's items, or nil for the workspaces.")

(defun lispm-desktop-platinum--application-menu-command (key items left)
  "Return a command dropping menu KEY, whose ITEMS a function returns.
LEFT is the pixel position of its title's left edge, where the menu drops.
A function whose `lispm-desktop-platinum-panel' property is set drops a panel of
its own instead, called with KEY, LEFT and the menu bar's bottom."
  (lambda ()
    (interactive)
    (if (and (symbolp items) (get items 'lispm-desktop-platinum-panel))
        (funcall items key left (lispm-desktop-platinum--menu-y))
      (lispm-desktop-platinum--with-open-menu key
        (lispm-desktop-popup-menu (funcall items) left (lispm-desktop-platinum--menu-y))))))

(defun lispm-desktop-platinum--application-menus (menus left)
  "Return the menu bar titles of MENUS, the first starting LEFT pixels in.
MENUS is a value of `lispm-desktop-platinum-menus-function'."
  (mapcar (pcase-lambda (`(,key ,title ,items))
            (let ((item (lispm-desktop-platinum--menu-title
                         key title (lispm-desktop-platinum--application-menu-command key items left)
                         title (eq lispm-desktop-platinum--open-menu key))))
              (setq left (+ left (string-pixel-width (nth 2 item))))
              item))
          menus))

(defun lispm-desktop-platinum-menu-bar ()
  "Return the left end of the menu bar: the λ menu, then the frontmost
application's menus, see `lispm-desktop-platinum-menus-function'; without one, a
title per view, highlighted while the sidebar shows it, and Web, while the
browser is in front."
  (let ((lambda-title (lispm-desktop-platinum--menu-title
                       'lispm-desktop-lambda (lispm-desktop-platinum--lambda) #'lispm-desktop-platinum-lambda-menu
                       "LispM" (eq lispm-desktop-platinum--open-menu 'lispm-desktop-lambda) 'left)))
    (cons
     lambda-title
     (if lispm-desktop-platinum-menus-function
         (lispm-desktop-platinum--application-menus (funcall lispm-desktop-platinum-menus-function)
                                                    (string-pixel-width (nth 2 lambda-title)))
       (let ((shown (lispm-desktop-sidebar-current-view)))
         (append
          (mapcar (lambda (view)
                    (let ((spec (cdr view)))
                      (lispm-desktop-platinum--menu-title (intern (format "lispm-desktop-view-%s" (car view)))
                                                          (plist-get spec :title)
                                                          (plist-get spec :command)
                                                          (plist-get spec :help)
                                                          (eq (car view) shown))))
                  lispm-desktop-sidebar-views)
          (when (lispm-desktop-platinum--browser-p)
            (list (lispm-desktop-platinum--menu-title 'lispm-desktop-view-web "Web" 'lispm-desktop-browser "Browser"
                                                      (lispm-desktop-browser-workspace-p))))))))))

(defun lispm-desktop-platinum-workspace-menu ()
  "Pop up the Application menu: the applications, see
`lispm-desktop-platinum-applications-function', or else the workspaces, the
current one checked."
  (interactive)
  (let ((current (tab-bar--current-tab-index))
        (tabs (funcall tab-bar-tabs-function)))
    (lispm-desktop-platinum--with-open-menu 'lispm-desktop-application
      (lispm-desktop-popup-menu
       (if lispm-desktop-platinum-applications-function
           (funcall lispm-desktop-platinum-applications-function)
         `(,@(seq-map-indexed
              (lambda (tab index)
                (list :label (alist-get 'name tab)
                      :action (lambda () (tab-bar-select-tab (1+ index)))
                      :checked (= index current)))
              tabs)
           separator
           (:label "New Workspace" :action tab-bar-new-tab)
           (:label "Close Workspace" :action tab-bar-close-tab :enabled ,(and (cdr tabs) t))))
       (- (frame-pixel-width) 4) (lispm-desktop-platinum--menu-y) 'right))))

(defvar tabspaces-default-tab)

(defun lispm-desktop-platinum-application-menu ()
  "Return the right end of the menu bar: the Application menu.
It names the current workspace with its icon, as Mac OS 9's named the
frontmost application: the hard disk for the default workspace, a
project's folder for a project's.  Clicked, it lists every workspace."
  (let ((name (alist-get 'name (tab-bar--current-tab)))
        (application (and lispm-desktop-platinum-application-function
                          (funcall lispm-desktop-platinum-application-function))))
    (list (lispm-desktop-platinum--menu-title
           'lispm-desktop-application
           (concat (cond (application (car application))
                         ((equal name (bound-and-true-p tabspaces-default-tab))
                          (lispm-desktop-platinum-icon "disk"))
                         ((and (fboundp 'lispm-desktop-browser-workspace-p) (lispm-desktop-browser-workspace-p))
                          (lispm-desktop-platinum-icon "netscape"))
                         (t (lispm-desktop-platinum-folder-icon
                             (car (rassoc name (bound-and-true-p tabspaces-project-tab-map))))))
                   " " (or (cdr application) name))
           #'lispm-desktop-platinum-workspace-menu "Workspaces"
           (eq lispm-desktop-platinum--open-menu 'lispm-desktop-application) 'right))))

(defun lispm-desktop-platinum--menu-bar-menus ()
  "Return the menus of the menu bar, left to right, as (KEY . COMMAND):
the λ menu, the frontmost application's menus and the Application menu."
  (mapcar (lambda (item) (cons (car item) (nth 3 item)))
          (append (if lispm-desktop-platinum-menus-function
                      (lispm-desktop-platinum-menu-bar)
                    (list (car (lispm-desktop-platinum-menu-bar))))
                  (lispm-desktop-platinum-application-menu))))

(defun lispm-desktop-platinum-menu-bar-open ()
  "Open the menu bar from the keyboard: the λ menu drops with its first
item highlighted.  Up and Down move through a menu, typing a letter goes
to the item it starts, Left and Right go to the menu beside, Return
chooses and Escape closes, so every menu is reached without the mouse.
The menus drop from the screen's menu bar, as when it is clicked,
whichever window has the keyboard, and it is given back once they close;
from a window out, its screen hidden in the Dock, they drop from the top
of that window."
  (interactive)
  (let ((screen (selected-frame)))
    (while (frame-parent screen)
      (setq screen (frame-parent screen)))
    (when-let* ((out (frame-parameter screen 'lispm-desktop-out))
                ((frame-live-p out)))
      (setq screen (if (frame-parameter out 'lispm-desktop-hidden) (selected-frame) out)))
    (unless (eq (selected-frame) screen)
      (select-frame-set-input-focus screen)))
  (let ((menus (lispm-desktop-platinum--menu-bar-menus))
        (index 0)
        step)
    (unwind-protect
        (let ((lispm-desktop-platinum--menu-bar-walk t))
          (while (setq step (catch 'lispm-desktop-popup-menu-bar-step
                              (let ((lispm-desktop-popup-menu-bar-step t))
                                (call-interactively (cdr (nth index menus))))
                              nil))
            (setq index (mod (+ index step) (length menus)))))
      (run-hooks 'lispm-desktop-platinum-menu-closed-hook))))

;;;; Window bottoms

(defvar-local lispm-desktop-platinum-info nil
  "What the bottom of this buffer's window says instead of the buffer's state.
List windows set it, as Finder's windows counted their items: a string,
or a function returning one when the window is drawn.")

(defconst lispm-desktop-platinum--grow-ridges
  '("wwwwwwwwwwwwwww" "w" "w" "w       ww" "w      w h" "w     w h ww" "w    w h w h"
    "w   w h w h ww" "w  w h w h w h" "w  ih w h w h" "w    w h w h" "w    ih w h"
    "w      w h" "w      ih" "w")
  "The inside of Mac OS 9's grow box, a row of pixels a string: lit along
its top and left (w), crossed by three diagonal ridges, each a lit line
beside a shaded one (h), softened at their lower ends (i).")

(defun lispm-desktop-platinum-draw-grow-box (palette height)
  "Return an SVG of the grow box, the ridged square in a window's corner.
It is sixteen pixels square, outlined along its top and left, in
PALETTE's greys, centred in HEIGHT pixels."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2)))
    (lispm-desktop-platinum--fill svg 0 0 s height (plist-get palette :chrome))
    (lispm-desktop-platinum--fill svg 0 y s s (plist-get palette :outline))
    (lispm-desktop-platinum--fill svg 1 (1+ y) (1- s) (1- s) (lispm-desktop-platinum-grey palette #xCC))
    (lispm-desktop-platinum--draw-map svg 1 (1+ y) lispm-desktop-platinum--grow-ridges
                                      `((?w . ,(lispm-desktop-platinum-grey palette #xFF))
                                        (?h . ,(lispm-desktop-platinum-grey palette #x77))
                                        (?i . ,(lispm-desktop-platinum-grey palette #xAA))))
    svg))

(defun lispm-desktop-platinum--grow-press (event)
  "Resize the child frame whose grow box EVENT pressed, from its corner.
The floating editor resizes so; elsewhere windows tile, and the grow box
only marks the corner."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (lispm-desktop-platinum--movable-p frame))
      (mouse-drag-frame-resize event 'bottom-right)
      (run-hook-with-args 'lispm-desktop-platinum-frame-moved-functions frame))))

(defvar lispm-desktop-platinum--grow-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <down-mouse-1>" #'lispm-desktop-platinum--grow-press)
    (keymap-set map "<mode-line> <mouse-1>" #'ignore)
    map)
  "Keymap of the grow box.")

(defun lispm-desktop-platinum--grow-box ()
  "Return the grow box as a string."
  (propertize " " 'display (lispm-desktop-platinum--image
                            (list 'grow)
                            (lambda () (lispm-desktop-platinum-draw-grow-box lispm-desktop-platinum--palette
                                                                             lispm-desktop-platinum-title-height)))
              'keymap lispm-desktop-platinum--grow-map))

(defun lispm-desktop-platinum--placard (text active)
  "Return TEXT as a placard, the outlined plate Mac OS 9 showed status in.
ACTIVE outlines it in black; an inactive window's placards are outlined
in grey.  The placard's face is layered under TEXT's own, so coloured
counts survive."
  (let* ((pad (propertize " " 'display '(space :width (6))))
         (placard (concat pad text pad)))
    (add-face-text-property 0 (length placard)
                            (if active 'lispm-desktop-platinum-placard 'lispm-desktop-platinum-placard-inactive)
                            t placard)
    placard))

(defun lispm-desktop-platinum--mode-line-text (construct)
  "Return mode line CONSTRUCT formatted, trimmed, and safe to show again.
It is formatted for the current buffer, the one whose window is drawn."
  (string-replace "%" "%%" (string-trim (format-mode-line construct nil nil
                                                          (current-buffer)))))

(defvar evil-mode-line-tag)
(defvar flymake-mode-line-counters)

(defun lispm-desktop-platinum-mode-line-left ()
  "The left of a window's bottom edge: its state, buffer and position.
A list window shows its `lispm-desktop-platinum-info' instead."
  (let ((active (mode-line-window-selected-p)))
    (concat
     (propertize " " 'display (lispm-desktop-platinum--strut 8))
     (if lispm-desktop-platinum-info
         (lispm-desktop-platinum--placard (string-replace "%" "%%" (if (functionp lispm-desktop-platinum-info)
                                                                        (funcall lispm-desktop-platinum-info)
                                                                       lispm-desktop-platinum-info))
                                          active)
       (mapconcat
        (lambda (text) (lispm-desktop-platinum--placard text active))
        (delq nil
              (list (when (and (bound-and-true-p evil-local-mode)
                               (stringp evil-mode-line-tag))
                      (string-trim (substring-no-properties evil-mode-line-tag) "[ <]+" "[> ]+"))
                    (concat (string-replace "%" "%%" (or lispm-desktop-platinum-title (buffer-name)))
                            (when (and buffer-file-name (buffer-modified-p)) " •"))
                    (lispm-desktop-platinum--mode-line-text mode-line-position)))
        " ")))))

(defun lispm-desktop-platinum-mode-line-right ()
  "The right of a window's bottom edge: diagnostics, branch, mode, grow box.
Only the active window shows the grow box, and only where the window can
be resized; a side window cannot."
  (let* ((active (mode-line-window-selected-p))
         (parts (unless lispm-desktop-platinum-info
                  (delq nil
                        (list (when (bound-and-true-p flymake-mode)
                                (let ((counts (lispm-desktop-platinum--mode-line-text
                                               flymake-mode-line-counters)))
                                  (unless (string-empty-p counts) counts)))
                              (when (stringp vc-mode)
                                (replace-regexp-in-string
                                 "\\`[A-Za-z]+[-:@!?]" ""
                                 (string-trim (substring-no-properties vc-mode))))
                              (lispm-desktop-platinum--mode-line-text mode-name))))))
    (concat (mapconcat (lambda (text) (lispm-desktop-platinum--placard text active)) parts " ")
            (propertize " " 'display '(space :width (6)))
            (if (and active (not (window-parameter (selected-window) 'window-side)))
                (lispm-desktop-platinum--grow-box)
              (lispm-desktop-platinum--space 16))
            (lispm-desktop-platinum--space 3))))

(defconst lispm-desktop-platinum-mode-line-format
  '((:eval (lispm-desktop-platinum-mode-line-left))
    mode-line-format-right-align
    (:eval (lispm-desktop-platinum-mode-line-right)))
  "The mode line of every window: its bottom edge, laid out as Mac OS 9's.")

;;;; Window frames
;;
;; A floating window (a child frame with the parameter
;; `lispm-desktop-window', as the Finder window, Control Panels, the
;; floating editor and browser) is framed as Mac OS 9 framed its windows:
;; a frame of its own stands behind it, a few pixels larger, showing the
;; outline, the lit band, the content's line and the drop shadow, drawn by
;; `lispm-desktop-platinum-draw-window-frame'.  It follows its window as it
;; moves, changes size, is raised, hidden or shown, and as it becomes
;; active or not; pressed, it drags its window, as Mac OS 9's frame did.

(defcustom lispm-desktop-platinum-window-frames t
  "Non-nil frames floating windows as Mac OS 9 framed its windows."
  :type 'boolean)

(defun lispm-desktop-platinum-window-border ()
  "Return the border width a floating window's own frame takes: none
where its frame is drawn for it, a pixel of outline where it is not."
  (if lispm-desktop-platinum-window-frames 0 1))

(defvar lispm-desktop-platinum--framing nil
  "Non-nil while a window's frame is raised or shown along with it.")

(defun lispm-desktop-platinum--framed-p (frame)
  "Return non-nil when FRAME is a floating window to be framed."
  (and lispm-desktop-platinum-window-frames lispm-desktop-platinum--palette
       (frame-live-p frame) (frame-parent frame)
       (frame-parameter frame 'lispm-desktop-window)))

(defvar lispm-desktop-platinum--frame-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lispm-desktop-platinum--frame-press)
    (keymap-set map "<mouse-1>" #'ignore)
    (keymap-set map "<drag-mouse-1>" #'ignore)
    map)
  "Keymap of a window's frame: pressed, it drags its window.")

(defun lispm-desktop-platinum--window-frame (frame)
  "Return the frame framing floating window FRAME, made when missing."
  (let ((outer (frame-parameter frame 'lispm-desktop-platinum-frame)))
    (if (frame-live-p outer)
        outer
      (let* ((buffer (generate-new-buffer " *lispm-desktop-window-frame*"))
             (outer (let ((after-make-frame-functions nil)
                          (before-make-frame-hook nil))
                      (make-frame
                       `((parent-frame . ,(frame-parent frame))
                         (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                         (undecorated . t) (unsplittable . t) (no-other-frame . t)
                         (skip-taskbar . t) (desktop-dont-save . t)
                         (internal-border-width . 0) (child-frame-border-width . 0)
                         (left-fringe . 0) (right-fringe . 0)
                         (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                         (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                         (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                         (visibility . nil)
                         (background-color . ,(lispm-desktop-platinum--color :chrome))
                         (lispm-desktop-platinum-frames . ,frame)
                         (width . 2) (height . 1))))))
        (with-current-buffer buffer
          (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                      cursor-type nil cursor-in-non-selected-windows nil
                      truncate-lines t auto-hscroll-mode nil
                      ;; About a window rolled up, the frame is shorter than
                      ;; the four lines Emacs otherwise keeps it to.
                      window-min-height 1)
          (use-local-map lispm-desktop-platinum--frame-map))
        (let ((root (frame-root-window outer)))
          (set-window-buffer root buffer)
          (set-window-dedicated-p root t)
          (set-window-fringes root 0 0))
        (set-frame-parameter frame 'lispm-desktop-platinum-frame outer)
        outer))))

(defun lispm-desktop-platinum--frame-update (frame)
  "Bring FRAME's frame up to date: placed and sized about it, drawn active
or not, shown when FRAME is, beneath it."
  (if (not (lispm-desktop-platinum--framed-p frame))
      (lispm-desktop-platinum--frame-drop frame)
    (let* ((outer (lispm-desktop-platinum--window-frame frame))
           (inset lispm-desktop-platinum-frame-inset)
           (width (+ (frame-pixel-width frame) (* 2 (car inset)) 1))
           (height (+ (frame-pixel-height frame) (cdr inset) (car inset) 1))
           (active (eq frame (selected-frame)))
           (position (frame-position frame))
           (place (list (- (car position) (car inset)) (- (cdr position) (cdr inset)) width height))
           (key (list lispm-desktop-platinum--generation width height active)))
      (unless (and (equal place (frame-parameter outer 'lispm-desktop-platinum-place))
                   ;; Emacs sizes a frame anew to whole lines as its fonts
                   ;; change, as with a theme.
                   (= (frame-pixel-width outer) width)
                   (= (frame-pixel-height outer) height))
        (set-frame-parameter outer 'lispm-desktop-platinum-place place)
        (let ((window-min-width 1) (window-safe-min-width 1)
              (window-min-height 1) (window-safe-min-height 1)
              (frame-resize-pixelwise t))
          (set-frame-size outer width height t))
        (set-frame-position outer (nth 0 place) (nth 1 place)))
      (unless (equal key (frame-parameter outer 'lispm-desktop-platinum-key))
        (set-frame-parameter outer 'lispm-desktop-platinum-key key)
        (with-current-buffer (window-buffer (frame-root-window outer))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize " " 'display
                                (svg-image (lispm-desktop-platinum-draw-window-frame
                                            lispm-desktop-platinum--palette width height :active active)
                                           :ascent 100 :scale 1))))))
      (cond ((and (frame-visible-p frame) (not (frame-visible-p outer)))
             (let ((lispm-desktop-platinum--framing t))
               (make-frame-visible outer)
               (raise-frame frame)))
            ((and (not (frame-visible-p frame)) (frame-visible-p outer))
             (make-frame-invisible outer t))))))

(defun lispm-desktop-platinum--frame-drop (frame)
  "Take FRAME's frame away."
  (let ((outer (and (frame-live-p frame) (frame-parameter frame 'lispm-desktop-platinum-frame))))
    (when (frame-live-p outer)
      (let ((buffer (window-buffer (frame-root-window outer))))
        (delete-frame outer t)
        (kill-buffer buffer)))
    (when (frame-live-p frame)
      (set-frame-parameter frame 'lispm-desktop-platinum-frame nil))))

(defun lispm-desktop-platinum-frames-sync (&rest _)
  "Bring every floating window's frame up to date, and take away the frames
of windows gone."
  (unless lispm-desktop-platinum--framing
    (dolist (frame (frame-list))
      (cond ((frame-parameter frame 'lispm-desktop-platinum-frames)
             (unless (frame-live-p (frame-parameter frame 'lispm-desktop-platinum-frames))
               (let ((buffer (window-buffer (frame-root-window frame))))
                 (delete-frame frame t)
                 (kill-buffer buffer))))
            ((frame-parameter frame 'lispm-desktop-window)
             (lispm-desktop-platinum--frame-update frame))))))

(defun lispm-desktop-platinum--frame-follow (frame &rest _)
  "Bring FRAME's frame along after FRAME was raised, shown or hidden.
This is advice after `raise-frame', `make-frame-visible' and
`make-frame-invisible'."
  (let ((frame (if (framep frame) frame (selected-frame))))
    (when (and (not lispm-desktop-platinum--framing) (lispm-desktop-platinum--framed-p frame))
      (let ((lispm-desktop-platinum--framing t)
            (outer (frame-parameter frame 'lispm-desktop-platinum-frame)))
        (if (and (frame-live-p outer) (frame-visible-p frame) (frame-visible-p outer))
            ;; Raised: the frame first, then the window over it.
            (progn (raise-frame outer) (raise-frame frame))
          (let ((lispm-desktop-platinum--framing nil))
            (lispm-desktop-platinum--frame-update frame)))))))

(defun lispm-desktop-platinum--frame-press (event)
  "Drag the window whose frame EVENT pressed until the button rises."
  (interactive "e")
  (let* ((outer (window-frame (posn-window (event-start event))))
         (frame (frame-parameter outer 'lispm-desktop-platinum-frames))
         (start (mouse-absolute-pixel-position))
         (origin (and (frame-live-p frame) (frame-position frame))))
    (when origin
      (select-frame-set-input-focus frame)
      (track-mouse
        (while (mouse-movement-p (let ((echo-keystrokes 0)) (read-event)))
          (let ((now (mouse-absolute-pixel-position)))
            (set-frame-position frame (+ (car origin) (- (car now) (car start)))
                                (+ (cdr origin) (- (cdr now) (cdr start))))
            (lispm-desktop-platinum--frame-update frame))))
      (run-hook-with-args 'lispm-desktop-platinum-frame-moved-functions frame))))

(defun lispm-desktop-platinum-frames-enable ()
  "Frame floating windows as Mac OS 9 framed its windows, and keep their
frames in step with them."
  (add-hook 'move-frame-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'window-size-change-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'window-selection-change-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'delete-frame-functions #'lispm-desktop-platinum--frame-drop)
  (dolist (function '(raise-frame make-frame-visible make-frame-invisible))
    (advice-add function :after #'lispm-desktop-platinum--frame-follow)))

;;;; Scroll bars
;;
;; Mac OS 9's scroll bar runs down a window's right edge: a recessed
;; track, a thumb in the variation colour with grip ridges, and the two
;; arrow boxes together at its bottom, as Mac OS 9 placed them with its
;; proportional thumbs.  Each window that scrolls gets one, drawn as an
;; SVG in a child frame laid over the window's right fringe, which is
;; widened to make room:
;;
;;   window --geometry--> child frame position and height
;;   window --start, end, size--> thumb --SVG--> child frame's one line
;;
;; Bars follow the windows on every command and scroll; an inactive
;; window's bar, and one with nothing to scroll, lies flat, as Mac OS 9's
;; did.  Sessions (terminals and chats) scroll their own way and get none.

(defcustom lispm-desktop-platinum-scroll-bars t
  "Non-nil gives windows Mac OS 9's scroll bars.
See `lispm-desktop-platinum-scroll-sync'."
  :type 'boolean)

(defconst lispm-desktop-platinum-scroll-width 16
  "Width in pixels of a scroll bar, its outlines included.")

(defconst lispm-desktop-platinum-scroll-arrow 15
  "Height in pixels of a scroll bar's arrow box, the line above it included.")

(defun lispm-desktop-platinum--scroll-track (height)
  "Return the length in pixels of the track of a scroll bar HEIGHT tall.
The arrow boxes and the line closing the bar take the rest."
  (- height (* 2 lispm-desktop-platinum-scroll-arrow) 1))

(defun lispm-desktop-platinum-scroll-thumb (height start end min max)
  "Return the thumb of a scroll bar HEIGHT pixels tall, as (TOP . LENGTH).
The window shows from START to END of a buffer spanning MIN to MAX.  The
value is nil when everything shows, or when the track is too short."
  (let* ((track (lispm-desktop-platinum--scroll-track height))
         (size (- max min))
         (shown (- end start)))
    (when (and (> size 0) (> track 24) (or (> start min) (< end max)))
      (let* ((length (min track (max 16 (round (* track (/ (float shown) size))))))
             (fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (round (* (- track length) (min 1.0 fraction))) length)))))

(defun lispm-desktop-platinum--draw-scroll-arrow (svg y up palette enabled)
  "Draw a scroll bar's arrow box from row Y of SVG, the line above included.
UP points its arrow up.  An ENABLED box is lit along its top and left
and shaded along its bottom and right, its arrow black; otherwise it is
flat, its arrow grey."
  (let* ((grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (top (1+ y))
         (bottom (+ y lispm-desktop-platinum-scroll-arrow -1)))
    (lispm-desktop-platinum--row svg 1 14 y (if enabled (plist-get palette :outline) (funcall grey #x88)))
    (lispm-desktop-platinum--fill svg 1 top 14 (- bottom y) (funcall grey (if enabled #xDD #xEE)))
    (when enabled
      (lispm-desktop-platinum--row svg 1 13 top (funcall grey #xFF))
      (lispm-desktop-platinum--column svg 1 top (1- bottom) (funcall grey #xFF))
      (lispm-desktop-platinum--row svg 2 14 bottom (funcall grey #xBB))
      (lispm-desktop-platinum--column svg 14 (1+ top) bottom (funcall grey #xBB)))
    (dotimes (i 4)
      (lispm-desktop-platinum--row svg (- 7 i) (+ 8 i) (+ top (if up (+ 4 i) (- 7 i)))
                                   (if enabled (plist-get palette :outline) (funcall grey #x88))))))

(defun lispm-desktop-platinum-draw-scroll-bar (palette height thumb active)
  "Return an SVG of a scroll bar HEIGHT pixels tall in PALETTE.
It is `lispm-desktop-platinum-scroll-width' wide, outlined down both
sides: a track recessed under a lip along its top and left, the thumb in
the variation colour, lit along its top and left, with four grip ridges,
then the two arrow boxes.  THUMB, as returned by
`lispm-desktop-platinum-scroll-thumb', places the thumb; nil means there
is nothing to scroll, and the bar lies flat, its arrows grey.  An
inactive bar, ACTIVE nil, lies flat without its arrows."
  (let* ((w lispm-desktop-platinum-scroll-width)
         (svg (svg-create w height))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (plist-get palette :outline))
         (track (lispm-desktop-platinum--scroll-track height))
         (live (and active thumb)))
    (lispm-desktop-platinum--fill svg 0 0 w height outline)
    (lispm-desktop-platinum--fill svg 1 0 (- w 2) height (funcall grey #xEE))
    (when live
      ;; The track, recessed: dark along its top and left, lit along its right.
      (lispm-desktop-platinum--fill svg 1 0 (- w 2) track (funcall grey #xAA))
      (lispm-desktop-platinum--row svg 1 13 0 (funcall grey #x77))
      (lispm-desktop-platinum--column svg 1 0 (1- track) (funcall grey #x77))
      (lispm-desktop-platinum--row svg 2 12 1 (funcall grey #x88))
      (lispm-desktop-platinum--column svg 2 1 (1- track) (funcall grey #x88))
      (lispm-desktop-platinum--column svg 13 1 (1- track) (funcall grey #xBB))
      (lispm-desktop-platinum--column svg 14 0 (1- track) (funcall grey #xCC))
      ;; The thumb, lit along its top and left, parted from the track
      ;; by a line at either end.
      (let* ((top (car thumb))
             (bottom (+ top (cdr thumb) -1)))
        (lispm-desktop-platinum--fill svg 1 top 14 (cdr thumb) (plist-get palette :thumb))
        (lispm-desktop-platinum--row svg 1 13 top (plist-get palette :thumb-light))
        (lispm-desktop-platinum--column svg 1 top (1- bottom) (plist-get palette :thumb-light))
        (lispm-desktop-platinum--row svg 2 14 bottom (plist-get palette :thumb-dark))
        (lispm-desktop-platinum--column svg 14 top bottom (plist-get palette :thumb-dark))
        (when (> top 0)
          (lispm-desktop-platinum--row svg 1 14 (1- top) outline))
        (when (< (1+ bottom) track)
          (lispm-desktop-platinum--row svg 1 14 (1+ bottom) outline))
        ;; Four grip ridges across its middle, each lit over shaded.
        (when (>= (cdr thumb) 14)
          (let ((first (- (+ top (/ (cdr thumb) 2)) 4)))
            (dotimes (i 4)
              (let ((y (+ first (* 2 i))))
                (lispm-desktop-platinum--dot svg 4 y (funcall grey #xEE))
                (lispm-desktop-platinum--row svg 5 10 y (plist-get palette :thumb-light))
                (lispm-desktop-platinum--row svg 5 11 (1+ y) (plist-get palette :highlight))))))))
    (when active
      (lispm-desktop-platinum--draw-scroll-arrow svg track t palette live)
      (lispm-desktop-platinum--draw-scroll-arrow svg (+ track lispm-desktop-platinum-scroll-arrow)
                                                 nil palette live))
    (lispm-desktop-platinum--row svg 0 (1- w) (1- height) outline)
    svg))

(defvar lispm-desktop-platinum--scroll-bars nil
  "Alist of windows to the child frames holding their scroll bars.")

(defvar lispm-desktop-platinum--scroll-timer nil
  "Pending timer bringing the scroll bars up to date, if any.")

(declare-function lispm-desktop-sidebar-buffer-view "lispm-desktop-sidebar" (buffer))
(declare-function lispm-desktop-terminal-buffer-p "lispm-desktop-interface" (buffer))

(defun lispm-desktop-platinum--scroll-window-p (window)
  "Return non-nil when WINDOW gets a scroll bar.
Every window of the main area does but sessions, the browser's page and
the desktop; of side windows, only the sidebar's lists."
  (let ((buffer (window-buffer window)))
    (and (not (window-minibuffer-p window))
         (not (window-parameter window 'lispm-desktop-chrome))
         (not (lispm-desktop-terminal-buffer-p buffer))
         (not (let ((mode (buffer-local-value 'major-mode buffer)))
                (or (provided-mode-derived-p mode 'xwidget-webkit-mode)
                    (provided-mode-derived-p mode 'lispm-desktop-mode)
                    (provided-mode-derived-p mode 'lispm-desktop-empty-mode))))
         (or (not (window-parameter window 'window-side))
             (lispm-desktop-sidebar-buffer-view buffer)
             (eq (buffer-local-value 'major-mode buffer) 'treemacs-mode)))))

(defvar lispm-desktop-platinum--scroll-map
  (let ((map (make-sparse-keymap)))
    (dolist (press '("<down-mouse-1>" "<double-down-mouse-1>" "<triple-down-mouse-1>"))
      (keymap-set map press #'lispm-desktop-platinum-scroll-press))
    (dolist (release '("<mouse-1>" "<double-mouse-1>" "<triple-mouse-1>" "<drag-mouse-1>"))
      (keymap-set map release #'ignore))
    (keymap-set map "<wheel-up>" #'lispm-desktop-platinum-scroll-wheel)
    (keymap-set map "<wheel-down>" #'lispm-desktop-platinum-scroll-wheel)
    map)
  "Keymap of a scroll bar's buffer.")

(defun lispm-desktop-platinum--scroll-frame (window)
  "Return the child frame holding WINDOW's scroll bar, made when missing.
It takes no focus and hooks run for new frames do not see it."
  (let ((frame (window-parameter window 'lispm-desktop-platinum-scroll-frame)))
    (if (frame-live-p frame)
        frame
      (let* ((buffer (generate-new-buffer " *lispm-desktop-scroll-bar*"))
             (frame (let ((after-make-frame-functions nil)
                          (before-make-frame-hook nil))
                      (make-frame
                       `((parent-frame . ,(window-frame window))
                         (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                         (undecorated . t) (unsplittable . t) (no-other-frame . t)
                         (internal-border-width . 0) (child-frame-border-width . 0)
                         (left-fringe . 0) (right-fringe . 0)
                         (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                         (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                         (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                         (desktop-dont-save . t) (visibility . nil)
                         (background-color . ,(lispm-desktop-platinum--color :chrome))
                         (width . 2) (height . 1))))))
        (with-current-buffer buffer
          (setq-local mode-line-format nil
                      header-line-format nil
                      tab-line-format nil
                      cursor-type nil
                      cursor-in-non-selected-windows nil
                      display-line-numbers nil
                      truncate-lines t
                      line-spacing 0
                      ;; The bar's one line is as wide as the frame; scrolled
                      ;; sideways to show point after it, it would lose a column.
                      auto-hscroll-mode nil
                      lispm-desktop-platinum--scroll-target window)
          (use-local-map lispm-desktop-platinum--scroll-map))
        (let ((root (frame-root-window frame)))
          (set-window-buffer root buffer)
          (set-window-dedicated-p root t)
          (set-window-fringes root 0 0)
          (set-window-margins root 0 0))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-target window)
        (set-window-parameter window 'lispm-desktop-platinum-scroll-frame frame)
        (push (cons window frame) lispm-desktop-platinum--scroll-bars)
        frame))))

(defvar-local lispm-desktop-platinum--scroll-target nil
  "The window whose scroll bar this buffer draws.")

(defun lispm-desktop-platinum--scroll-update (window)
  "Bring WINDOW's scroll bar up to date: place it, size it, draw it."
  (let ((width lispm-desktop-platinum-scroll-width))
    (unless (= (or (cadr (window-fringes window)) 0) width)
      (set-window-fringes window nil width nil t))
    (let* ((frame (lispm-desktop-platinum--scroll-frame window))
           (inside (window-inside-pixel-edges window))
           (x (- (nth 2 (window-pixel-edges window))
                 (window-right-divider-width window)
                 (window-scroll-bar-width window)
                 width))
           (y (nth 1 inside))
           (height (max 1 (- (nth 3 inside) y)))
           (buffer (window-buffer window))
           (thumb (with-current-buffer buffer
                    (lispm-desktop-platinum-scroll-thumb height (window-start window)
                                                         (window-end window t)
                                                         (point-min) (point-max))))
           (active (eq window (frame-selected-window (window-frame window))))
           (place (list x y height))
           (key (list lispm-desktop-platinum--generation height thumb active)))
      (unless (equal place (frame-parameter frame 'lispm-desktop-platinum-scroll-place))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-place place)
        ;; Sized in pixels, below the width of the ten columns a window
        ;; may otherwise not shrink under.
        (let ((window-min-width 1)
              (window-safe-min-width 1))
          (set-frame-size frame width height t))
        (set-frame-position frame x y))
      (unless (equal key (frame-parameter frame 'lispm-desktop-platinum-scroll-key))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-key key)
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-thumb thumb)
        (set-frame-parameter frame 'background-color (lispm-desktop-platinum--color :chrome))
        (let ((root (frame-root-window frame)))
          (with-current-buffer (window-buffer root)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize " " 'display
                                  (svg-image (lispm-desktop-platinum-draw-scroll-bar
                                              lispm-desktop-platinum--palette height thumb active)
                                             :ascent 100 :scale 1)))))
          (set-window-point root 1)
          (set-window-hscroll root 0)))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))))

(defun lispm-desktop-platinum--scroll-drop (window frame)
  "Take WINDOW's scroll bar, held in child FRAME, away."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame))))
      (delete-frame frame t)
      (kill-buffer buffer)))
  (when (window-live-p window)
    (set-window-parameter window 'lispm-desktop-platinum-scroll-frame nil)
    (set-window-fringes window nil nil nil t)))

(defun lispm-desktop-platinum-scroll-sync ()
  "Give every window that wants a scroll bar its own, up to date, and
take the bars of windows gone or no longer wanting one away."
  (setq lispm-desktop-platinum--scroll-timer nil)
  (let ((wanted nil))
    (when (and lispm-desktop-platinum-scroll-bars lispm-desktop-platinum--palette)
      (dolist (frame (frame-list))
        ;; Floating windows out on the computer's screen go without, as
        ;; they do over the desktop.
        (when (and (frame-visible-p frame) (not (frame-parent frame))
                   (not (frame-parameter frame 'lispm-desktop-window))
                   (not (frame-parameter frame 'lispm-desktop-out))
                   (display-graphic-p frame))
          (dolist (window (window-list frame 'no-mini))
            (when (lispm-desktop-platinum--scroll-window-p window)
              (push window wanted)
              (lispm-desktop-platinum--scroll-update window))))))
    (setq lispm-desktop-platinum--scroll-bars
          (seq-filter (pcase-lambda (`(,window . ,frame))
                        (or (and (memq window wanted) (frame-live-p frame))
                            (progn (lispm-desktop-platinum--scroll-drop window frame) nil)))
                      lispm-desktop-platinum--scroll-bars))))

(defun lispm-desktop-platinum--scroll-schedule (&rest _)
  "Bring the scroll bars up to date once Emacs has redisplayed."
  (unless (timerp lispm-desktop-platinum--scroll-timer)
    (setq lispm-desktop-platinum--scroll-timer
          (run-with-idle-timer 0 nil #'lispm-desktop-platinum-scroll-sync))))

(defun lispm-desktop-platinum--scroll-in (window step)
  "Call STEP, a function of no arguments, with WINDOW selected; redraw its bar."
  (with-selected-window window
    (ignore-errors (funcall step)))
  (lispm-desktop-platinum--scroll-update window)
  (redisplay t))

(defun lispm-desktop-platinum--scroll-repeat (window step)
  "Call STEP in WINDOW, and again and again while the mouse button is held.
As Mac OS 9's arrows and track did, holding repeats after a pause."
  (lispm-desktop-platinum--scroll-in window step)
  (when (sit-for 0.35)
    (while (sit-for 0.05)
      (lispm-desktop-platinum--scroll-in window step))))

(defun lispm-desktop-platinum--scroll-mouse-y (frame)
  "Return the mouse's height in pixels from the top of scroll bar FRAME."
  (pcase-let ((`(,over ,_ . ,y) (mouse-pixel-position)))
    (cond ((null y) 0)
          ((eq over frame) y)
          (t (- y (cdr (frame-position frame)))))))

(defun lispm-desktop-platinum--scroll-drag (window frame grab thumb)
  "Drag WINDOW's thumb, held GRAB pixels from its top, until the button rises.
THUMB is its (TOP . LENGTH) when the drag began; FRAME holds the bar."
  (let* ((track (lispm-desktop-platinum--scroll-track (frame-pixel-height frame)))
         (length (cdr thumb)))
    (track-mouse
      (while (mouse-movement-p (let ((echo-keystrokes 0)) (read-event)))
        (let* ((top (max 0 (min (- track length)
                                (- (lispm-desktop-platinum--scroll-mouse-y frame) grab))))
               (fraction (/ (float top) (max 1 (- track length)))))
          (lispm-desktop-platinum--scroll-in
           window
           (lambda ()
             (let* ((shown (- (window-end nil t) (window-start)))
                    (span (max 0 (- (point-max) (point-min) shown)))
                    (position (+ (point-min) (round (* fraction span)))))
               (set-window-start nil (save-excursion (goto-char position)
                                                     (line-beginning-position)))))))))))

(defun lispm-desktop-platinum-scroll-press (event)
  "Act on a press in a scroll bar, as Mac OS 9's scroll bars did.
The arrows, together at the bar's bottom, scroll a line and the track
above or below the thumb a page, over and over while held; the thumb
drags the window's text along."
  (interactive "e")
  (let* ((position (event-start event))
         (frame (window-frame (posn-window position)))
         (window (frame-parameter frame 'lispm-desktop-platinum-scroll-target))
         (thumb (frame-parameter frame 'lispm-desktop-platinum-scroll-thumb))
         (y (cdr (posn-x-y position)))
         (track (lispm-desktop-platinum--scroll-track (frame-pixel-height frame))))
    (when (window-live-p window)
      (cond ((>= y (+ track lispm-desktop-platinum-scroll-arrow))
             (lispm-desktop-platinum--scroll-repeat window (lambda () (scroll-up 1))))
            ((>= y track)
             (lispm-desktop-platinum--scroll-repeat window (lambda () (scroll-down 1))))
            ((null thumb))
            ((< y (car thumb)) (lispm-desktop-platinum--scroll-repeat window #'scroll-down))
            ((>= y (+ (car thumb) (cdr thumb))) (lispm-desktop-platinum--scroll-repeat window #'scroll-up))
            (t (lispm-desktop-platinum--scroll-drag window frame (- y (car thumb)) thumb))))))

(defun lispm-desktop-platinum-scroll-wheel (event)
  "Scroll the window of the scroll bar the mouse wheel turned over, by EVENT."
  (interactive "e")
  (let ((window (frame-parameter (window-frame (posn-window (event-start event)))
                                 'lispm-desktop-platinum-scroll-target)))
    (when (window-live-p window)
      (lispm-desktop-platinum--scroll-in window (if (eq (event-basic-type event) 'wheel-up)
                                                    (lambda () (scroll-down 3))
                                                  (lambda () (scroll-up 3)))))))

(defun lispm-desktop-platinum-scroll-enable ()
  "Keep the scroll bars in step with windows, scrolling and selection."
  (add-hook 'post-command-hook #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-scroll-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-size-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-selection-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-buffer-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (lispm-desktop-platinum--scroll-schedule))

;;;; Status: Emacs's own time, load, mail and battery

;; Emacs keeps the time, the load average and the mail flag up to date
;; (`display-time-mode') and the battery (`display-battery-mode').  As
;; Mac OS 9's menu bar held its clock alone at its right end, the time is
;; a title of its own there, its menu giving the date, the calendar and
;; the world clock; the load, the mail and the battery are modules of
;; the Control Strip (lispm-desktop-strip.el), drawn and filled here:
;;
;;   load      a meter; the menu picks the 1, 5 or 15 minute average and
;;             opens the process list
;;   mail      an envelope, flagged when mail has come; the menu reads,
;;             composes and checks mail
;;   battery   a PowerBook's battery filled to its charge; the menu tells
;;             the charge, the power source and the time left
;;
;; Emacs computes everything; `display-time-hook' and
;; `battery-update-functions' only note what it found.  Whatever else is
;; in `global-mode-string', such as LispM's agent count, stays on the menu
;; bar, before the time.

(defvar battery-status-function)
(defvar display-time-string)
(defvar battery-mode-line-string)
(defvar display-time-load-average)
(defvar display-time-format)
;; Bound by `display-time-update' while `display-time-hook' runs; the names
;; are time.el's.
(with-suppressed-warnings ((lexical load mail))
  (defvar load)
  (defvar mail))
(declare-function battery-format "battery" (format alist))
(declare-function display-time-update "time" ())
(declare-function display-time-update--load "time" ())

(defvar lispm-desktop-platinum--load ""
  "The load average `display-time-mode' last showed, \"\" below its threshold.")

(defvar lispm-desktop-platinum--mail nil
  "Non-nil when `display-time-mode' last found new mail.")

(defvar lispm-desktop-platinum--battery nil
  "The battery status `display-battery-mode' last read, an alist.")

(defun lispm-desktop-platinum--note-time ()
  "Note the load average and the mail flag Emacs has just computed.
Run from `display-time-hook', inside `display-time-update'."
  (setq lispm-desktop-platinum--load (string-trim (substring-no-properties load))
        lispm-desktop-platinum--mail (and mail t)))

(defun lispm-desktop-platinum--note-battery (data)
  "Note the battery status DATA Emacs has just read."
  (setq lispm-desktop-platinum--battery data))

(defun lispm-desktop-platinum-draw-battery (palette percent charging)
  "Return an SVG of a battery sixteen pixels square, filled to PERCENT in
PALETTE, as the Control Strip's battery module shows it.  CHARGING draws
a bolt across it; a nearly empty one is filled in red."
  (let* ((svg (svg-create 16 16))
         (ink (plist-get palette :text))
         (level (round (* 10 (/ (max 0 (min 100 percent)) 100.0)))))
    (lispm-desktop-platinum--fill svg 0 4 14 8 ink)
    (lispm-desktop-platinum--fill svg 1 5 12 6 (plist-get palette :strip))
    (lispm-desktop-platinum--fill svg 14 6 2 4 ink)
    (lispm-desktop-platinum--fill svg 2 6 level 4 (if (< percent 10) "#CE3131" ink))
    (when charging
      (svg-polygon svg '((8 . 3) (4 . 8) (6.5 . 8) (5 . 13) (10 . 7) (7.5 . 7) (9 . 3))
                   :fill (plist-get palette :accent) :stroke (plist-get palette :strip)
                   :stroke-width 0.75))
    svg))

(defun lispm-desktop-platinum-draw-meter (palette fraction)
  "Return an SVG of a load meter sixteen pixels square in PALETTE: three
bars rising left to right, filled to FRACTION."
  (let ((svg (svg-create 16 16)))
    (dotimes (i 3)
      (let ((height (* 4 (1+ i))))
        (lispm-desktop-platinum--fill svg (1+ (* i 5)) (- 14 height) 4 height
                                      (if (> fraction (/ i 3.0))
                                          (plist-get palette :text)
                                        (plist-get palette :strip-dark)))))
    svg))

(defun lispm-desktop-platinum-load-fraction ()
  "Return the load average `display-time-mode' shows, over the processors."
  (/ (string-to-number lispm-desktop-platinum--load) (float (max 1 (num-processors)))))

(defun lispm-desktop-platinum-load-items ()
  "Return the load menu: which average to show, and the processes."
  (let ((averages (ignore-errors (load-average t))))
    `((:label "Load Average" :enabled nil)
      ,@(seq-map-indexed
         (lambda (span index)
           (list :label (format "%s: %.2f" span (or (nth index averages) 0))
                 :checked (eql display-time-load-average index)
                 :action (lambda ()
                           (setq display-time-load-average index)
                           (display-time-update))))
         '("1 minute" "5 minutes" "15 minutes"))
      separator
      (:label "Processes" :action proced))))

(defun lispm-desktop-platinum-mail-items ()
  "Return the Mail menu: read, compose and check mail."
  `((:label ,(if lispm-desktop-platinum--mail "You have new mail" "No new mail")
            :enabled nil)
    separator
    (:label "Read Mail" :action ,(lambda () (call-interactively read-mail-command)))
    (:label "Compose Mail…" :action compose-mail)
    separator
    (:label "Check for Mail" :action display-time-update)))

(defun lispm-desktop-platinum-battery-percent ()
  "Return the battery's charge in percent, or nil without a battery shown."
  (and (bound-and-true-p display-battery-mode) lispm-desktop-platinum--battery
       (ignore-errors (string-to-number (alist-get ?p lispm-desktop-platinum--battery)))))

(defun lispm-desktop-platinum-battery-charging-p ()
  "Return non-nil when the battery is on the power adapter."
  (member (alist-get ?L lispm-desktop-platinum--battery) '("AC" "on-line")))

(defun lispm-desktop-platinum-battery-items ()
  "Return the battery menu: its charge, its power source and the time left."
  (let* ((data lispm-desktop-platinum--battery)
         (line (lambda (format) (battery-format format data))))
    `((:label ,(funcall line "Charge: %p%%") :enabled nil)
      (:label ,(concat "Power: "
                       (pcase (alist-get ?L data)
                         ((or "AC" "on-line") "Power Adapter")
                         ((or "Battery" "Battery Power" "off-line") "Battery")
                         (source (or source "unknown"))))
              :enabled nil)
      ,@(unless (member (alist-get ?t data) '(nil "N/A" "0:00"))
          `((:label ,(funcall line "Time left: %t") :enabled nil)))
      separator
      (:label "Battery Status" :action battery))))

(defun lispm-desktop-platinum--status-title (key image label command help)
  "Return the menu bar status item KEY: IMAGE (an SVG or nil) and LABEL.
It runs COMMAND, described by HELP, and is highlighted while its menu is open."
  (lispm-desktop-platinum--menu-title
   key
   (concat (when image
             ;; An icon name gives a string already displayed as the icon.
             (if (stringp image) (lispm-desktop-platinum-icon image) (propertize " " 'display image)))
           (when (and image label) " ")
           label)
   command help (eq lispm-desktop-platinum--open-menu key)))

(defun lispm-desktop-platinum--status-menu (key items)
  "Pop up ITEMS under the status title KEY that was just clicked."
  (let ((x (car (posn-x-y (event-start last-input-event)))))
    (lispm-desktop-platinum--with-open-menu key
      (lispm-desktop-popup-menu items (max 0 (- x 12)) (lispm-desktop-platinum--menu-y)))))

(defun lispm-desktop-platinum-time-menu ()
  "Pop up the clock's menu: the date, the calendar, the world clock."
  (interactive)
  (lispm-desktop-platinum--status-menu
   'lispm-desktop-time
   `((:label ,(format-time-string "%A, %B %-e, %Y") :enabled nil)
     separator
     (:label "Calendar" :action calendar)
     (:label "World Clock" :action world-clock)
     separator
     (:label "Time Format…" :action ,(lambda () (customize-variable 'display-time-format))))))

(defun lispm-desktop-platinum-menu-status ()
  "Return the status titles at the menu bar's right end: whatever
`global-mode-string' holds besides the time and the battery, then the
time.  The load, the mail and the battery are in the Control Strip."
  (let ((others (string-trim
                 (format-mode-line
                  (seq-remove (lambda (part)
                                (memq part '(display-time-string battery-mode-line-string)))
                              (ensure-list global-mode-string))))))
    (delq nil
          (list
           (unless (string-empty-p others)
             (lispm-desktop-platinum--menu-title 'lispm-desktop-status others #'ignore nil))
           (when (bound-and-true-p display-time-mode)
             (lispm-desktop-platinum--status-title
              'lispm-desktop-time nil (format-time-string (or display-time-format "%-I:%M %p"))
              #'lispm-desktop-platinum-time-menu (format-time-string "%A, %B %-e, %Y")))))))

(defun lispm-desktop-platinum--start-status ()
  "Turn on Emacs's own time, load and mail, and the battery if there is one.
The time reads as Mac OS 9's clock did, unless `display-time-format' was
set already."
  (require 'battery)
  (require 'time)
  (add-hook 'display-time-hook #'lispm-desktop-platinum--note-time)
  (add-hook 'battery-update-functions #'lispm-desktop-platinum--note-battery)
  (unless display-time-format
    (setq display-time-format "%-I:%M %p"))
  (display-time-mode 1)
  (when (and battery-status-function
             (not (equal (alist-get ?p (ignore-errors (funcall battery-status-function)))
                         "N/A")))
    (display-battery-mode 1)))

;;;; Lists

(defconst lispm-desktop-platinum-column-width 11
  "Width in columns of a list's right-hand column, the one it is sorted by.")

(defconst lispm-desktop-platinum-list-header-height 21
  "Height in pixels of a list's row of column headings.")

(defconst lispm-desktop-platinum--sort-glyph
  '("      #" "     ##" "    ###" "   ####" "  #####" " ######" "#######")
  "The glyph of a list's sort-direction button, a row of pixels a string.")

(defun lispm-desktop-platinum-draw-list-header (palette columns &optional label-x)
  "Return an SVG of a list's row of column headings, in PALETTE.
COLUMNS lists (LABEL WIDTH SORTED [X]) from left to right; a
sixteen-pixel sort-direction button ends the row.  Each heading is a
bevelled cell, its label in the list's font X pixels in, by default
LABEL-X or eight; the heading of the SORTED column is drawn pushed in
and darker, as Mac OS 9 drew the column a list was sorted by."
  (let* ((h lispm-desktop-platinum-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (x 0))
    (pcase-dolist (`(,label ,w ,sorted ,x-in) (append columns '((nil 16 nil))))
      (cl-flet ((row (y level) (lispm-desktop-platinum--row svg x (+ x w -1) y (funcall grey level)))
                (column (cx level) (lispm-desktop-platinum--column svg (+ x cx) 0 (1- h) (funcall grey level))))
        (if sorted
            (progn
              (lispm-desktop-platinum--fill svg x 0 w h (funcall grey #x88))
              (row (- h 2) #xAA) (column (- w 2) #xAA)
              (row (1- h) #x44) (column (1- w) #x44)
              (row 1 #x55) (column 1 #x55)
              (row 0 #x11) (column 0 #x11))
          (lispm-desktop-platinum--fill svg x 0 w h (funcall grey #xCC))
          (row (- h 2) #x88) (row (1- h) #x33)
          (row 1 #xFF) (column 1 #xFF)
          (row 0 #x66) (column 0 #x66)))
      (when label
        (lispm-desktop-platinum--draw-content svg label (+ x (or x-in label-x 8)) 1 (- h 3)
                                              (plist-get palette :text) 'lispm-desktop-platinum-list))
      (setq x (+ x w)))
    (lispm-desktop-platinum--draw-map svg (- width 12) 7 lispm-desktop-platinum--sort-glyph
                                      `((?# . ,(plist-get palette :text))))
    svg))

(defun lispm-desktop-platinum-columns (name meta width meta-width &optional label-x)
  "Return a list's column headings NAME and META as a line WIDTH pixels wide.
META is the right-hand column, META-WIDTH pixels wide with the
sort-direction button, the column the list is sorted by; NAME is
LABEL-X pixels in.  See `lispm-desktop-platinum-draw-list-header'."
  (let ((name-width (- width meta-width)))
    (concat (propertize " " 'display
                        (lispm-desktop-platinum--image
                         (list 'list-header name meta width meta-width label-x)
                         (lambda ()
                           (lispm-desktop-platinum-draw-list-header
                            lispm-desktop-platinum--palette
                            (list (list name name-width nil) (list meta (- meta-width 16) t 4))
                            label-x))))
            "\n")))

;;;; Treemacs

(declare-function treemacs-load-theme "treemacs-themes" (name))

(defun lispm-desktop-platinum-treemacs-theme ()
  "Draw treemacs's tree with Finder's icons: disks, folders and documents.
A directory shows a disclosure triangle before its folder, as Finder's
list view did; files are indented by a triangle's width, so their icons
line up with the folders'.  It is made again when a theme is enabled,
as the triangles take the theme's colour."
  (when (featurep 'treemacs)
    ;; Treemacs puts its background face in front of the default face; left
    ;; inheriting the default, it would bring back the editor's font over
    ;; the list's.  The project's name is not underlined, as Finder's
    ;; disks were not.
    (set-face-attribute 'treemacs-window-background-face nil :inherit 'unspecified)
    (set-face-attribute 'treemacs-root-face nil :underline nil :weight 'bold :height 1.0)
    (let* ((gap (propertize " " 'display '(space :width (5))))
           (no-triangle (propertize " " 'display '(space :width (12))))
           (row (lambda (lead icon) (concat lead gap (lispm-desktop-platinum-icon icon) gap))))
      (eval
       `(treemacs-create-theme "lispm-desktop-platinum"
          :config
          (progn
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "disk")
                                  :extensions (root-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "disk")
                                  :extensions (root-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "folder")
                                  :extensions (dir-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "folder")
                                  :extensions (dir-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "document-blank")
                                  :extensions (tag-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "document-blank")
                                  :extensions (tag-closed))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-blank")
                                  :extensions (tag-leaf))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-image")
                                  :extensions ("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp"
                                               "tif" "tiff" "ico" "heic"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-lisp")
                                  :extensions ,lispm-desktop-platinum-lisp-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-markdown")
                                  :extensions ("md" "markdown" "mkd"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-org")
                                  :extensions ("org"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-code")
                                  :extensions ,lispm-desktop-platinum-code-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document")
                                  :extensions (fallback))))
       t)
      (treemacs-load-theme "lispm-desktop-platinum"))))

;;;; Faces

(defface lispm-desktop-platinum-title '((t :weight bold))
  "Face of the active window's title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-title-inactive '((t :weight bold :inherit shadow))
  "Face of an inactive window's title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title '((t :weight bold))
  "Face of a menu bar title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title-active '((t :weight bold :inverse-video t))
  "Face of a highlighted menu bar title, as of an open menu."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-list nil
  "Face of list views: the sidebar's rows."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-header nil
  "Face of a list window's header, below its title bar."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-small nil
  "Face of the small system font, as Mac OS 9's Geneva 9: window headers,
the Control Strip's menus."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-list-body nil
  "Face of a list view's body: lighter than the platinum, as Finder's."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-row nil
  "Face of a list's row: a lit line atop it parts it from the row above."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-sorted nil
  "Face of the column a list is sorted by: shaded down its length, as in
Finder's list view."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-placard '((t :box (:line-width (1 . 1))))
  "Face of a placard on the active window's bottom edge."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-placard-inactive '((t :inherit shadow))
  "Face of a placard on an inactive window's bottom edge."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-toolbar nil
  "Face of a toolbar: the browser's, around its address field."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-toolbar-label nil
  "Face of a label in a toolbar, such as the address field's."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-field '((t :box (:line-width (1 . 1))))
  "Face of an edit text field: framed in black, in the list colour."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-field-focused '((t :inherit lispm-desktop-platinum-field))
  "Face of an edit text field with the keyboard: ringed in the focus colour."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-button '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a bevel button."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-button-pressed '((t :box (:line-width (1 . 1) :style pressed-button)))
  "Face of a bevel button pushed in: a toggle that is on."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-column '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a column heading."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-column-sorted '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of the heading of the column a list is sorted by."
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-platinum--family (families)
  "Return the first of FAMILIES installed, or nil."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed)) families)))

(defun lispm-desktop-platinum-apply-faces (&rest _)
  "Derive the palette and every chrome face from the active theme.
Colours come only from faces this function never sets, so re-running
it, as on every theme change, is idempotent."
  (let ((content (face-background 'default nil t))
        (text (face-foreground 'default nil t))
        (dim (face-foreground 'shadow nil t))
        (accent (face-foreground 'font-lock-constant-face nil t)))
    (when (and content text dim accent (color-name-to-rgb content))
      (setq lispm-desktop-platinum--palette (lispm-desktop-platinum--merge-palette
                                             (lispm-desktop-platinum--derive-palette content text dim accent)
                                             lispm-desktop-platinum-colors)
            lispm-desktop-platinum--generation (1+ lispm-desktop-platinum--generation))
      (let ((chrome (lispm-desktop-platinum--color :chrome))
            (header (lispm-desktop-platinum--color :header))
            (system (and (display-graphic-p) (lispm-desktop-platinum--family lispm-desktop-platinum-system-families)))
            (list-family (and (display-graphic-p)
                              (lispm-desktop-platinum--family lispm-desktop-platinum-list-families))))
        (set-face-attribute 'tab-line nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil :height 'unspecified)
        (set-face-attribute 'tab-bar nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil)
        (set-face-attribute 'mode-line nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil :overline 'unspecified :underline 'unspecified
                            :height 'unspecified)
        (set-face-attribute 'mode-line-inactive nil
                            :inherit 'unspecified :background chrome
                            :foreground (lispm-desktop-platinum--color :muted)
                            :box nil :overline 'unspecified :underline 'unspecified
                            :height 'unspecified)
        ;; Lists as Finder's list view drew them: rows lighter than the
        ;; platinum, the column sorted by in platinum, a lit line atop
        ;; each row.
        (set-face-attribute 'lispm-desktop-platinum-sorted nil
                            :background (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xDD)
                            :extend t)
        (set-face-attribute 'lispm-desktop-platinum-list-body nil
                            :inherit 'lispm-desktop-platinum-list
                            :background (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xEE))
        (set-face-attribute 'lispm-desktop-platinum-row nil
                            :overline (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xFF))
        ;; Balloon Help: a note in the list's type, on the header's grey.
        ;; It carries no outline: macOS sizes a tooltip without its box,
        ;; which would cut the right edge off.
        (set-face-attribute 'tooltip nil
                            :inherit 'lispm-desktop-platinum-list :background header
                            :foreground text :height 0.9 :box nil)
        ;; Placards are Mac OS 9's outlined plates; an inactive window's
        ;; lie flat, outlined in grey.  Their text stays text, so what
        ;; other packages show there, images included, still shows.
        (set-face-attribute 'lispm-desktop-platinum-placard nil
                            :inherit 'lispm-desktop-platinum-list :background chrome :foreground text
                            :height 0.95
                            :box `(:line-width (1 . 1) :color ,(lispm-desktop-platinum--color :outline)))
        (set-face-attribute 'lispm-desktop-platinum-placard-inactive nil
                            :inherit 'lispm-desktop-platinum-list :background chrome
                            :foreground (lispm-desktop-platinum--color :muted) :height 0.95
                            :box `(:line-width (1 . 1)
                                   :color ,(lispm-desktop-platinum-grey lispm-desktop-platinum--palette #x99)))
        (dolist (face '(lispm-desktop-platinum-title lispm-desktop-platinum-title-inactive
                        lispm-desktop-platinum-menu-title lispm-desktop-platinum-menu-title-active))
          (set-face-attribute face nil :family (or system 'unspecified) :height 120
                              :weight 'bold))
        (set-face-attribute 'lispm-desktop-platinum-title nil :foreground text)
        (set-face-attribute 'lispm-desktop-platinum-title-inactive nil
                            :foreground (lispm-desktop-platinum--color :muted) :inherit 'unspecified)
        ;; Menu bar titles are drawn as images in these faces; menus'
        ;; items inherit them.
        (set-face-attribute 'lispm-desktop-platinum-menu-title nil
                            :foreground text :background chrome :inverse-video nil :box nil)
        (set-face-attribute 'lispm-desktop-platinum-menu-title-active nil
                            :foreground (lispm-desktop-platinum--color :highlight-text)
                            :background (lispm-desktop-platinum--color :highlight)
                            :inverse-video nil :box nil)
        (set-face-attribute 'lispm-desktop-platinum-list nil :family (or list-family 'unspecified))
        (set-face-attribute 'lispm-desktop-platinum-small nil
                            :inherit 'lispm-desktop-platinum-list :height 90 :weight 'normal)
        (set-face-attribute 'lispm-desktop-platinum-header nil
                            :inherit 'lispm-desktop-platinum-list :background header :foreground dim
                            :box `(:line-width (1 . 4) :color ,header) :underline nil)
        (set-face-attribute 'lispm-desktop-platinum-toolbar nil
                            :inherit 'lispm-desktop-platinum-list :background header :foreground text)
        (set-face-attribute 'lispm-desktop-platinum-toolbar-label nil
                            :inherit 'lispm-desktop-platinum-toolbar :foreground dim)
        (set-face-attribute 'lispm-desktop-platinum-field nil
                            :inherit 'lispm-desktop-platinum-list :background (lispm-desktop-platinum--color :content)
                            :foreground text
                            :box `(:line-width (1 . 1) :color ,(lispm-desktop-platinum--color :outline)))
        ;; Emacs draws one box about text, so the ring stands for the frame
        ;; within it while the field has the keyboard.
        (set-face-attribute 'lispm-desktop-platinum-field-focused nil
                            :inherit 'lispm-desktop-platinum-field
                            :box `(:line-width (2 . 2) :color ,(lispm-desktop-platinum--color :focus)))
        (set-face-attribute 'lispm-desktop-platinum-button nil
                            :inherit 'lispm-desktop-platinum-list :background chrome :foreground text
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lispm-desktop-platinum-button-pressed nil
                            :inherit 'lispm-desktop-platinum-list :background (lispm-desktop-platinum--color :shade)
                            :foreground (lispm-desktop-platinum--color :accent)
                            :box '(:line-width (1 . 1) :style pressed-button))
        (set-face-attribute 'lispm-desktop-platinum-column nil
                            :inherit 'lispm-desktop-platinum-list :background header :foreground text
                            :height 0.9
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lispm-desktop-platinum-column-sorted nil
                            :inherit 'lispm-desktop-platinum-list :background chrome :foreground text
                            :height 0.9
                            :box '(:line-width (1 . 1) :style released-button))
        ;; Menus, alerts and windows are outlined in black, as Mac OS 9's were.
        (set-face-attribute 'child-frame-border nil :background (lispm-desktop-platinum--color :outline))
        ;; Neighbouring windows meet across a divider drawn as a window's
        ;; frame: lit where it leaves the window before, platinum, then
        ;; the outline of the window after.
        (set-face-attribute 'window-divider nil
                            :foreground (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xCC))
        (set-face-attribute 'window-divider-first-pixel nil
                            :foreground (lispm-desktop-platinum-grey lispm-desktop-platinum--palette #xFF))
        (set-face-attribute 'window-divider-last-pixel nil
                            :foreground (lispm-desktop-platinum--color :outline))))))

;;;; Setup

(defun lispm-desktop-setup-platinum ()
  "Lay the frame out as Mac OS 9 did, in the active theme's colours."
  (setq-default tab-line-format '(:eval (lispm-desktop-platinum-title-bar))
                mode-line-format lispm-desktop-platinum-mode-line-format)
  (setq mode-line-right-align-edge 'window)
  (setq tab-bar-format '(lispm-desktop-platinum-menu-bar-line))
  (lispm-desktop-platinum--start-status)
  ;; Frames take sizes in pixels: a scroll bar is exactly as tall as its
  ;; window, and a maximized frame fills the screen.
  (setq frame-resize-pixelwise t)
  ;; The menu bar runs from edge to edge and the desktop starts under its
  ;; black line: the screen has no border of its own.
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (dolist (frame (frame-list))
    (when (and (display-graphic-p frame) (not (frame-parent frame)))
      (set-frame-parameter frame 'internal-border-width 0)))
  (lispm-desktop-platinum-scroll-enable)
  (lispm-desktop-platinum-frames-enable)
  ;; Window frames: a dark outline on each window's edge, platinum between.
  (setq window-divider-default-places t
        window-divider-default-right-width 5
        window-divider-default-bottom-width 5)
  (window-divider-mode 1)
  ;; The insertion point is a thin bar, as Mac OS 9's was; Evil's normal
  ;; state keeps its block.
  (setq-default cursor-type '(bar . 2))
  ;; Help over a control shows as Balloon Help, in Emacs's own frames.
  (setq use-system-tooltips nil
        tooltip-delay 0.6
        tooltip-frame-parameters '((name . "tooltip")
                                   (internal-border-width . 6)
                                   (no-special-glyphs . t)))
  (lispm-desktop-platinum-apply-faces)
  (add-hook 'enable-theme-functions #'lispm-desktop-platinum-apply-faces)
  (with-eval-after-load 'treemacs
    (lispm-desktop-platinum-treemacs-theme)
    (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-platinum-treemacs-theme)) 90))
  (add-hook 'which-key-init-buffer-hook #'lispm-desktop-platinum-no-title-bar)
  (add-hook 'transient-setup-buffer-hook #'lispm-desktop-platinum-no-title-bar)
  ;; Quitting asks in alerts, as Mac OS 9's Quit and Shut Down did.
  (advice-add 'save-buffers-kill-emacs :around #'lispm-desktop-platinum--quit)
  ;; The menu bar from the keyboard; a C-c key, which terminals leave to
  ;; Emacs too.
  (keymap-global-set "C-c m" #'lispm-desktop-platinum-menu-bar-open))


(provide 'lispm-desktop-platinum)
;;; lispm-desktop-platinum.el ends here
