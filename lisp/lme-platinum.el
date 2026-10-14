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
;; (`lme-platinum-apply-faces'); a theme can name them outright in
;; `lme-platinum-colors', as the platinum theme gives Platinum's own greys.
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

(defcustom lme-platinum-colors nil
  "Chrome colours a theme names outright, a plist.
Keys are those of `lme-platinum--derive-palette' (:chrome, :header,
:light, :shade, :frame, :muted, :accent, ...); a colour left out is
derived from the theme's faces.  Themes set it, as the platinum theme
does; users rarely need to."
  :type '(plist :value-type color))

(defconst lme-platinum-title-height 22
  "Height in pixels of a title bar, and of a window's bottom edge.")

(defconst lme-platinum-menu-bar-height 20
  "Height in pixels of the menu bar, Mac OS 9's.")

(defconst lme-platinum-menu-icon-height 16
  "Height in pixels of the icons drawn in the menu bar: the load meter,
the battery.  With a title's text line, 16 pixels for its 12, they
leave the bar `lme-platinum-menu-bar-height' tall.")

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
toward black, and frames are darker still.  The desktop behind the
windows is the content tinted with the accent."
  (let ((chrome (lme-tab-bar--blend content "#000000" 0.25)))
    (list :content content :text text :dim dim :accent accent
          :chrome chrome
          :desktop (lme-tab-bar--blend content accent 0.25)
          :header (lme-tab-bar--blend chrome content 0.5)
          :light (lme-tab-bar--blend chrome text 0.3)
          :shade (lme-tab-bar--blend chrome "#000000" 0.6)
          :frame (lme-tab-bar--blend content "#000000" 0.65)
          :muted (lme-tab-bar--blend dim text 0.35))))

(defun lme-platinum--merge-palette (derived named)
  "Return the palette DERIVED with the colours NAMED put in their place."
  (let ((palette (copy-sequence derived)))
    (cl-loop for (key value) on named by #'cddr
             do (setq palette (plist-put palette key value)))
    palette))

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

(defun lme-platinum-draw-box (palette kind height &optional modified disabled pressed)
  "Return an SVG of a title bar box of KIND, `close', `zoom' or `collapse'.
The box is `lme-platinum-box-size' square, framed and bevelled in PALETTE,
centred in HEIGHT pixels.  MODIFIED puts a dot in a close box; DISABLED
draws an empty frame in the dimmed colour; PRESSED draws the box pushed
in, darker and lit from below, as while the mouse button is held on it."
  (let* ((s lme-platinum-box-size)
         (svg (svg-create s height))
         (y (/ (- height s) 2))
         (frame (plist-get palette (if disabled :muted :frame)))
         (lit (plist-get palette (if pressed :shade :light)))
         (shaded (plist-get palette (if pressed :light :shade))))
    (lme-platinum--fill svg 0 0 s height (plist-get palette :chrome))
    (lme-platinum--fill svg 0 y s s frame)
    (lme-platinum--fill svg 1 (1+ y) (- s 2) (- s 2)
                        (if pressed
                            (lme-tab-bar--blend (plist-get palette :chrome)
                                                (plist-get palette :shade) 0.5)
                          (plist-get palette :chrome)))
    (unless disabled
      (lme-platinum--fill svg 1 (1+ y) (- s 2) 1 lit)
      (lme-platinum--fill svg 1 (1+ y) 1 (- s 2) lit)
      (lme-platinum--fill svg 1 (+ y s -2) (- s 2) 1 shaded)
      (lme-platinum--fill svg (- s 2) (1+ y) 1 (- s 2) shaded)
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

(defun lme-platinum--box (kind &optional modified disabled pressed)
  "Return the image of a title bar box of KIND, see `lme-platinum-draw-box'."
  (lme-platinum--image (list 'box kind modified disabled pressed)
                       (lambda () (lme-platinum-draw-box lme-platinum--palette kind
                                                          lme-platinum-title-height
                                                          modified disabled pressed))))

(defun lme-platinum-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN."
  (propertize " " 'display (lme-platinum--image
                            (list 'triangle open)
                            (lambda () (lme-platinum-draw-triangle lme-platinum--palette open)))))

;;;; Icons
;;
;; Finder's small icons, drawn as sixteen-pixel SVGs in icons/os9: a
;; folder (badged when it holds a git repository), a hard disk, documents
;; (text, code, Lisp, Markdown, Org, picture, blank), a compact Macintosh
;; standing for a terminal, a small robot for the agents and LispM's own
;; application diamond.
;; Unlike the chrome, icons keep their own colours, as Mac OS 9's did
;; whatever the appearance.

(defvar lme-platinum--icons (make-hash-table :test #'equal)
  "Icon images by name.")

(defun lme-platinum-icon-file (name)
  "Return the file of icon NAME: a file of icons/os9 without its extension,
`lambda' for LispM's rainbow λ, or `emacs' for the icon Emacs ships with,
read from this Emacs's own data directory."
  (pcase name
    ('lambda (expand-file-name "icons/lispm-lambda.png" lme-directory))
    ('emacs (let ((file (expand-file-name "images/icons/hicolor/scalable/apps/emacs.svg"
                                          data-directory)))
              (if (file-readable-p file)
                  file
                (lme-platinum-icon-file "document"))))
    (_ (expand-file-name (format "icons/os9/%s.svg" name) lme-directory))))

(defun lme-platinum-icon (name)
  "Return the Finder icon NAME as a string one icon wide.
NAME is as `lme-platinum-icon-file' takes it.  Where images cannot be
shown, a blank stands in."
  (let ((file (lme-platinum-icon-file name)))
    ;; The icon carries a face of its own, so a button it sits in, such as
    ;; a list row, does not underline it.
    (if (and (display-images-p) (file-readable-p file))
        (propertize " " 'face 'lme-platinum-list
                    'display (or (gethash file lme-platinum--icons)
                                 (puthash file (create-image file nil nil :height 16
                                                             :ascent 'center)
                                          lme-platinum--icons)))
      (propertize " " 'face 'lme-platinum-list 'display '(space :width (16))))))

(defconst lme-platinum-lisp-extensions '("el" "lisp" "lsp" "cl" "scm" "ss" "rkt" "clj" "cljs"
                                         "cljc" "edn" "fnl")
  "Extensions of files shown with the Lisp document icon.")

(defconst lme-platinum-code-extensions
  '("c" "h" "cc" "cpp" "hpp" "m" "mm" "swift" "rs" "go" "zig" "py" "rb" "js" "jsx" "ts" "tsx"
    "java" "kt" "scala" "hs" "ml" "ex" "exs" "erl" "lua" "php" "sh" "bash" "zsh" "fish" "nix"
    "sol" "sql" "css" "scss" "html" "xml" "json" "yaml" "yml" "toml" "proto" "mk" "cmake")
  "Extensions of files shown with the code document icon.")

(defun lme-platinum-file-icon-name (file)
  "Return the name of the document icon of FILE, by its kind."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((string-match-p (image-file-name-regexp) file) "document-image")
          ((member extension lme-platinum-lisp-extensions) "document-lisp")
          ((member extension '("md" "markdown" "mkd")) "document-markdown")
          ((equal extension "org") "document-org")
          ((member extension lme-platinum-code-extensions) "document-code")
          (t "document"))))

(defun lme-platinum-folder-icon (directory)
  "Return the folder icon of DIRECTORY: badged when it is a git repository."
  (lme-platinum-icon (if (and directory
                              (file-exists-p (expand-file-name ".git" directory)))
                         "folder-git"
                       "folder")))

(defvar lispm-directory)

(defun lme-platinum-buffer-icon (buffer)
  "Return the Finder icon of BUFFER.
An agent shows its own mark; a terminal, a compact Macintosh; a
directory, a folder; LispM's own records, LispM's diamond; a file, the
document of its kind; any other buffer, a blank document."
  (with-current-buffer buffer
    (cond ((and (fboundp 'lme-agent--buffer-p) (lme-agent--buffer-p buffer))
           (lme-agent--icon (lme-agent--buffer-agent buffer)))
          ((derived-mode-p 'eat-mode 'term-mode 'shell-mode 'eshell-mode)
           (lme-platinum-icon "computer"))
          ((derived-mode-p 'dired-mode) (lme-platinum-folder-icon default-directory))
          ((and buffer-file-name (boundp 'lispm-directory)
                (file-in-directory-p buffer-file-name lispm-directory))
           (lme-platinum-icon "lispm"))
          (buffer-file-name
           (lme-platinum-icon
            (let ((name (lme-platinum-file-icon-name buffer-file-name)))
              (cond ((not (equal name "document")) name)
                    ((derived-mode-p 'lisp-data-mode) "document-lisp")
                    ((derived-mode-p 'prog-mode) "document-code")
                    (t name)))))
          (t (lme-platinum-icon "document-blank")))))

;;;; Title bars

(defvar-local lme-platinum-title nil
  "Title the window of this buffer shows instead of the buffer's own name.")

(defvar-local lme-platinum-icon nil
  "Icon the window of this buffer shows before its title, a string.")

(defvar-local lme-platinum-box-functions nil
  "What the title bar boxes of this buffer's window do instead of their own.
A plist of :close, :zoom and :collapse functions, each called with the
window, for chrome whose boxes act on what it frames, such as the
browser's address window or the editor's title strip.  Where the plist is
set, a box without a function is drawn disabled.")

(defvar-local lme-platinum-active-function nil
  "Function deciding whether this buffer's title bar is drawn active, or nil.
Called with the window, it lets chrome look active along with what it
frames, as the editor's title strip does while the editor is in front.")

(defun lme-platinum--box-function (window box)
  "Return the function WINDOW's buffer gives title bar BOX, or nil.
BOX is :close, :zoom or :collapse; see `lme-platinum-box-functions'."
  (plist-get (buffer-local-value 'lme-platinum-box-functions (window-buffer window)) box))

(defun lme-platinum-no-title-bar ()
  "Give the current buffer's windows no title bar, as for transient popups."
  (setq-local tab-line-format nil))

(defun lme-platinum--buffer-title (buffer)
  "Return the title of BUFFER's window."
  (or (buffer-local-value 'lme-platinum-title buffer)
      (and (fboundp 'lme-agent-buffer-label) (lme-agent-buffer-label buffer))
      (buffer-name buffer)))

(defun lme-platinum--buffer-icon (buffer)
  "Return the proxy icon of BUFFER's window, as Finder's windows showed one."
  (or (buffer-local-value 'lme-platinum-icon buffer)
      (lme-platinum-buffer-icon buffer)))

(defun lme-platinum--window-command (command)
  "Return a mouse command calling COMMAND with the window clicked."
  (lambda (event)
    (interactive "e")
    (funcall command (posn-window (event-start event)))))

(defun lme-platinum-press (position pressed-p command)
  "Hold a button down from the press at POSITION until the mouse button rises.
The button is drawn pushed in, through the window parameter
`lme-platinum-pressed' that the drawing reads, until the release; COMMAND
then runs, with no arguments, only when PRESSED-P accepts the release's
position: as on Mac OS 9, moving off a button before letting go cancels."
  (let ((window (posn-window position))
        (target (lme-platinum--press-target position)))
    (set-window-parameter window 'lme-platinum-pressed target)
    (force-mode-line-update t)
    (redisplay t)
    (let ((release (let ((echo-keystrokes 0)) (read-event))))
      (set-window-parameter window 'lme-platinum-pressed nil)
      (force-mode-line-update t)
      (when (and (consp release)
                 (eq (event-basic-type release) 'mouse-1)
                 (funcall pressed-p (event-end release)))
        (funcall command)))))

(defun lme-platinum--press-target (position)
  "Return what the press at POSITION is on: the `lme-platinum-target' there."
  (when-let* ((string (posn-string position)))
    (get-text-property (cdr string) 'lme-platinum-target (car string))))

(defun lme-platinum--same-target-p (position)
  "Return a predicate true of positions on the same target as POSITION."
  (let ((window (posn-window position))
        (area (posn-area position))
        (target (lme-platinum--press-target position)))
    (lambda (release)
      (and (eq (posn-window release) window)
           (eq (posn-area release) area)
           (equal (lme-platinum--press-target release) target)))))

(defun lme-platinum--pressed-p (target)
  "Return non-nil while TARGET is held down in the window being drawn."
  (equal (window-parameter (selected-window) 'lme-platinum-pressed) target))

(defun lme-platinum--box-button (string kind command help)
  "Return STRING as title bar box KIND, which runs COMMAND with its window.
HELP describes it.  It is pressed rather than clicked, see
`lme-platinum-press'."
  (propertize string
              'help-echo help
              'lme-platinum-target kind
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>"
                                    (lambda (event)
                                      (interactive "e")
                                      (let ((start (event-start event)))
                                        (lme-platinum-press
                                         start (lme-platinum--same-target-p start)
                                         (lambda () (funcall command (posn-window start)))))))
                        (keymap-set map "<tab-line> <mouse-1>" #'ignore)
                        map)))

(defvar lme-platinum-frame-moved-functions nil
  "Functions called with a child frame its title bar or grow box moved or
resized, so it can be kept in place.")

(defun lme-platinum--title-press (event)
  "Drag the child frame whose top title bar EVENT pressed, as a window.
A window at the top of a child frame, such as the floating editor's
title strip, moves the frame, as Mac OS 9 moved a window by its title;
elsewhere the press does nothing and the release selects the window."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (frame-parent frame) (window-at-side-p window 'top))
      (mouse-drag-frame-move event)
      (run-hook-with-args 'lme-platinum-frame-moved-functions frame))))

(defvar lme-platinum--title-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'lme-platinum--title-press)
    (keymap-set map "<tab-line> <mouse-1>"
                (lme-platinum--window-command #'select-window))
    (keymap-set map "<tab-line> <double-down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-mouse-1>"
                (lme-platinum--window-command #'lme-platinum-shade-or-zoom))
    (keymap-set map "<tab-line> <drag-mouse-1>" #'lme-platinum-drag-window)
    (keymap-set map "<tab-line> M-<down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> M-<mouse-1>"
                (lme-platinum--window-command #'lme-platinum-title-menu))
    map)
  "Keymap of a title bar outside its boxes, as Mac OS 9's title bars
behaved: a click brings the window forward, a double click rolls it up,
a drag moves the window, and a Command click (Meta) pops up its buffers.")

(defconst lme-platinum--title-help
  (format "Drag onto another window to swap them; double-click to roll up; %s-click for \
the window's buffers"
          (if (eq system-type 'darwin) "⌘" "M"))
  "Help shown over a title bar.")

(declare-function lme-main-area--window-p "lme-interface" (window))
(declare-function lme-main-area--terminal-p "lme-interface" (window))

(defun lme-platinum-drag-window (event)
  "Move the window whose title bar EVENT dragged to where it was dropped.
Tiled windows cannot float as Mac OS 9's did, so a window dropped on
another trades places with it.  The main area's columns hold: a session
trades places only with a session, anything else only with another
window that is not a session."
  (interactive "e")
  (let ((from (posn-window (event-start event)))
        (to (posn-window (event-end event))))
    (when (and (windowp to) (window-live-p to) (not (eq from to))
               (lme-main-area--window-p from) (lme-main-area--window-p to))
      (if (eq (and (lme-main-area--terminal-p from) t)
              (and (lme-main-area--terminal-p to) t))
          (progn (window-swap-states from to)
                 (select-window to))
        (message "Sessions keep to the left, everything else to the right")))))

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

(defun lme-platinum--fixed-p (window)
  "Return non-nil when WINDOW keeps its place and size: a side window, such
as the sidebar, or chrome, such as the browser's address window."
  (or (window-parameter window 'window-side)
      (window-parameter window 'lme-chrome)))

(defun lme-platinum--window-controls (window)
  "Return the title bar boxes of WINDOW as (CLOSE ZOOM COLLAPSE).
Each is `on', `off' (drawn disabled) or nil.  A side window or chrome
closes but neither zooms nor collapses; a window collapses only when
another shares its column."
  (let ((fixed (lme-platinum--fixed-p window))
        (functions (buffer-local-value 'lme-platinum-box-functions (window-buffer window))))
    (if functions
        (mapcar (lambda (box) (if (plist-get functions box) 'on 'off))
                '(:close :zoom :collapse))
      (list 'on
            (if fixed 'off 'on)
            (if (and (not fixed) (window-combined-p window)) 'on 'off)))))

(defun lme-platinum--title-bar (window)
  "Return the title bar of WINDOW, drawn for its current state."
  (let* ((buffer (window-buffer window))
         ;; Chrome wearing another window's title bar, as the browser's
         ;; address window wears the page's, is active with that window.
         (active (or (mode-line-window-selected-p)
                     (eq (window-parameter window 'lme-platinum-title-for)
                         (old-selected-window))
                     (when-let* ((active (buffer-local-value 'lme-platinum-active-function
                                                             buffer)))
                       (funcall active window))))
         (pressed (window-parameter window 'lme-platinum-pressed))
         (width (- (window-pixel-width window)
                   (window-right-divider-width window)
                   (window-scroll-bar-width window)))
         (icon (lme-platinum--buffer-icon buffer))
         (title (lme-platinum--buffer-title buffer))
         (modified (and (buffer-file-name buffer) (buffer-modified-p buffer)))
         (controls (lme-platinum--window-controls window))
         (key (list lme-platinum--generation width active icon title modified controls
                    pressed))
         (cached (window-parameter window 'lme-platinum-title-bar)))
    (if (equal (car cached) key)
        (cdr cached)
      (let ((bar (lme-platinum--draw-title-bar width active icon title modified controls
                                               pressed)))
        (set-window-parameter window 'lme-platinum-title-bar (cons key bar))
        bar))))

(defun lme-platinum--draw-title-bar (width active icon title modified controls
                                            &optional pressed)
  "Return a title bar WIDTH pixels wide for ICON and TITLE.
ACTIVE draws the stripes and boxes; MODIFIED marks the close box;
CONTROLS is a value of `lme-platinum--window-controls'; PRESSED names the
box held down, if any."
  (let* ((face (if active 'lme-platinum-title 'lme-platinum-title-inactive))
         ;; The bar is a mode line construct, where % starts a directive.
         (label (concat (when icon (concat icon " "))
                        (propertize (string-replace "%" "%%" title) 'face face)))
         (s lme-platinum-box-size)
         (layout (lme-platinum-title-layout width (string-pixel-width label)))
         (label (lme-platinum--fit label (nth 1 layout))))
    (pcase-let ((`(,close ,zoom ,collapse) controls)
                (`(,left ,_ ,right) layout))
      (cl-flet ((box (kind state command help)
                  (cond ((not active) (lme-platinum--space s))
                        ((eq state 'off)
                         (propertize " " 'display (lme-platinum--box kind nil t)))
                        (t (lme-platinum--box-button
                            (propertize " " 'display
                                        (lme-platinum--box kind (and (eq kind 'close) modified)
                                                           nil (eq pressed kind)))
                            kind command help))))
                (stripes (pixels)
                  (if active
                      (propertize " " 'display (lme-platinum--stripes (max 1 pixels)))
                    (lme-platinum--space pixels)))
                (title-part (string)
                  (propertize string 'keymap lme-platinum--title-map
                              'help-echo lme-platinum--title-help)))
        (concat (propertize " " 'display (lme-platinum--strut 8))
                (box 'close close #'lme-platinum-close-window "Close this window")
                (title-part
                 (concat (lme-platinum--space 6)
                         (stripes left)
                         (lme-platinum--space 8)
                         label
                         (lme-platinum--space (+ 8 (- (nth 1 layout) (string-pixel-width label))))
                         (stripes right)
                         (lme-platinum--space 6)))
                (box 'zoom zoom #'lme-platinum-zoom-window "Zoom this window")
                (lme-platinum--space 4)
                (box 'collapse collapse #'lme-platinum-collapse-window
                     "Collapse this window to its title bar")
                (lme-platinum--space 8))))))

(defun lme-platinum-title-bar ()
  "The tab line of every window: its title bar, see `lme-platinum--title-bar'."
  (lme-platinum--title-bar (selected-window)))

;;;; Title bar commands

(declare-function lme-desktop-show "lme-desktop" (&optional window))

(defun lme-platinum-close-window (window)
  "Close WINDOW as Mac OS closed a window: put its document away.
A sidebar is hidden.  Otherwise WINDOW's buffer is killed, once an
alert has asked about unsaved changes or a running process, and WINDOW
goes too when other windows remain in the main area; the last one shows
the desktop."
  (cond
   ((lme-platinum--box-function window :close)
    (funcall (lme-platinum--box-function window :close) window))
   ((window-parameter window 'window-side)
    (when-let* ((view (lme-sidebar-current-view)))
      (lme-sidebar-select view)))
   (t
    (let ((buffer (window-buffer window)))
      (when (lme-platinum--confirm-close buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        ;; The main area's last window uncovers the desktop, as closing
        ;; Mac OS 9's last window did; any other window goes.
        (when (window-live-p window)
          (if (cdr (lme-main-area--windows))
              (delete-window window)
            (when (fboundp 'lme-desktop-show)
              (lme-desktop-show window)))))))))

(defun lme-platinum--confirm-close (buffer)
  "Ask, as Mac OS 9 asked, whether BUFFER may close; return non-nil if so.
A modified file asks to be saved first, and is saved when told to; a
buffer whose process still runs asks before the process is stopped."
  (let ((name (lme-platinum--buffer-title buffer))
        (process (get-buffer-process buffer)))
    (cond
     ((and (buffer-file-name buffer) (buffer-modified-p buffer))
      (pcase (lme-popup-alert
              'caution (format "Save changes to the document “%s” before closing?" name)
              '(("Don’t Save" discard nil ?\M-d) ("Cancel" cancel cancel) ("Save" save default))
              "If you don’t save, your changes will be lost.")
        ('save (with-current-buffer buffer (save-buffer)) t)
        ('discard (with-current-buffer buffer (set-buffer-modified-p nil)) t)))
     ((and process (process-live-p process))
      (eq 'close (lme-popup-alert
                  'caution (format "Close “%s”?" name)
                  '(("Cancel" cancel cancel) ("Close" close default))
                  "What runs in it will be stopped.")))
     (t t))))

(defun lme-platinum-zoom-window (window)
  "Zoom WINDOW to the whole main area, or back to where it was.
The sidebar stays, as `delete-other-windows' leaves side windows be."
  (let ((saved (frame-parameter nil 'lme-platinum-unzoom)))
    (cond
     ((lme-platinum--box-function window :zoom)
      (funcall (lme-platinum--box-function window :zoom) window))
     ((and saved (eq (car saved) window) (not (cdr (lme-main-area--windows))))
      (set-frame-parameter nil 'lme-platinum-unzoom nil)
      (set-window-configuration (cdr saved)))
     (t
      (set-frame-parameter nil 'lme-platinum-unzoom
                           (cons window (current-window-configuration)))
      (delete-other-windows window)))))

(defun lme-platinum-collapse-window (window)
  "Roll WINDOW up to its title bar, or down again, as a window shade.
Only a window sharing its column with another can roll up."
  (cond
   ((lme-platinum--box-function window :collapse)
    (funcall (lme-platinum--box-function window :collapse) window))
   ((window-parameter window 'lme-platinum-shade)
    (let ((height (window-parameter window 'lme-platinum-shade)))
      (set-window-parameter window 'lme-platinum-shade nil)
      (window-resize window (- height (window-total-height window)) nil t)))
   ((window-combined-p window)
    (set-window-parameter window 'lme-platinum-shade (window-total-height window))
    (window-resize window (- (window-min-size window nil t)
                             (window-total-height window))
                   nil t))))

(declare-function tabspaces--buffer-list "tabspaces" (&optional frame tabnum))

(declare-function lme-sidebar-buffer-view "lme-sidebar" (buffer))

(defun lme-platinum--window-buffers (window)
  "Return the buffers WINDOW could show: its workspace's.
Hidden buffers and the sidebar's lists are left out."
  (seq-remove (lambda (buffer)
                (or (string-prefix-p " " (buffer-name buffer))
                    (lme-sidebar-buffer-view buffer)))
              (if (bound-and-true-p tabspaces-mode)
                  (tabspaces--buffer-list (window-frame window))
                (buffer-list (window-frame window)))))

(defun lme-platinum-shade-or-zoom (window)
  "Roll WINDOW up or down, or zoom it when it cannot roll up.
A double click on a title bar does this, as on Mac OS 9; a window alone
in its column cannot roll up, so it zooms instead.  A side window or
chrome does neither."
  (cond ((lme-platinum--box-function window :collapse)
         (funcall (lme-platinum--box-function window :collapse) window))
        ((lme-platinum--fixed-p window))
        ((or (window-parameter window 'lme-platinum-shade) (window-combined-p window))
         (lme-platinum-collapse-window window))
        (t (lme-platinum-zoom-window window))))

(defun lme-platinum-title-menu (window)
  "Pop up the buffers WINDOW could show, and show the one chosen.
As Mac OS 9 popped up a window's path from its title, the title pops up
the workspace's buffers, the one WINDOW shows checked.  The choice goes
to WINDOW unless the main area's columns send it elsewhere: a session
to the left, a file to the right."
  (lme-popup-menu
   (mapcar (lambda (buffer)
             (list :label (lme-platinum--buffer-title buffer)
                   :action (lambda ()
                             (select-window window)
                             (switch-to-buffer buffer))
                   :checked (eq buffer (window-buffer window))))
           (seq-take (lme-platinum--window-buffers window) 30))))

;;;; Menu bar

(defvar lme-platinum--open-menu nil
  "The menu bar item whose menu is popped up, drawn highlighted meanwhile.")

(defvar lme-platinum-menu-closed-hook nil
  "Hook run when a menu dropped from the menu bar closes, chosen or not.")

(defmacro lme-platinum--with-open-menu (key &rest body)
  "Run BODY, which pops up a menu, with menu bar item KEY highlighted."
  (declare (indent 1))
  `(let ((lme-platinum--open-menu ,key))
     (force-mode-line-update t)
     (redisplay t)
     (unwind-protect (progn ,@body)
       (force-mode-line-update t)
       (run-hooks 'lme-platinum-menu-closed-hook))))

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

(defun lme-platinum--lambda ()
  "Return LispM's rainbow λ, the counterpart of Mac OS 9's rainbow Apple.
Where images cannot be shown, a plain λ stands in."
  (if (display-images-p) (lme-platinum-icon 'lambda) "λ"))

(declare-function lme-popup-menu "lme-popup" (items &optional x y align))
(declare-function lme-popup-alert "lme-popup" (icon message buttons &optional detail))
(declare-function lme-agent-installed-p "lme-ai" (agent))
(defvar lme-theme)

(defun lme-platinum--menu-y ()
  "Return where a menu dropped from the menu bar starts: its bottom edge."
  (tab-bar-height nil t))

(defun lme-platinum-lambda-items ()
  "Return the items of the λ menu, as `lme-popup-menu' takes them."
  `((:label "About LispM" :action lme-platinum-about)
    separator
    ,@(when (fboundp 'lme-agent-lispm-new)
        '((:label "New LispM Agent" :action lme-agent-lispm-new)))
    ,@(mapcan (pcase-lambda (`(,agent ,label ,command))
                (when (lme-agent-installed-p agent)
                  (list (list :label label :action command))))
              '((claude "New Claude Code Session" lme-agent-claude-new)
                (codex "New Codex Session" lme-agent-codex-new)
                (hermes "New Hermes Session" lme-agent-hermes-new)))
    (:label "Resume Session…" :action lme-agent-sessions)
    (:label "Send Region to Agent" :action lme-agent-send-region :enabled ,(use-region-p))
    separator
    (:label "New Shell" :action lme-shell-new)
    ,@(when (lme-platinum--browser-p) '((:label "Browser" :action lme-browser-show)))
    (:label "Magit Status" :action magit-status)
    separator
    ,@(when (fboundp 'lme-settings-panel-items)
        `((:label "Settings" :submenu lme-settings-panel-items)))
    (:label "Customize lispm-emacs…" :action ,(lambda () (customize-group 'lispm-emacs)))))

(defun lme-platinum-lambda-menu ()
  "Pop up the λ menu, LispM's counterpart of the Apple menu."
  (interactive)
  (lme-platinum--with-open-menu 'lme-lambda
    (lme-popup-menu (lme-platinum-lambda-items) 4 (lme-platinum--menu-y))))

(defun lme-platinum-about ()
  "Tell which LispM and lispm-emacs this is, as About This Computer did."
  (interactive)
  (lme-popup-alert
   'note "LispM, a Lisp Machine for agents, inside Emacs."
   '(("OK" ok default))
   (format "lispm-emacs, Emacs %s.  github.com/RyanKung/lispm-emacs" emacs-version)))

(declare-function lme-xwidget-available-p "lme-browser" ())

(defun lme-platinum--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lme-xwidget-available-p) (lme-xwidget-available-p)))

(defvar lme-platinum-menus-function nil
  "Function returning the frontmost application's menus, or nil.
The menus follow the λ menu in the menu bar, as Mac OS 9's menu bar held
the frontmost application's: a list of (KEY TITLE ITEMS), ITEMS a function
returning the items `lme-popup-menu' takes.  Without it the menu bar
holds a title per sidebar view, and Web.")

(defvar lme-platinum-application-function nil
  "Function returning the frontmost application as (ICON . NAME), or nil.
The Application menu names it; without it, the workspace is named.")

(defvar lme-platinum-applications-function nil
  "Function returning the Application menu's items, or nil for the workspaces.")

(defun lme-platinum--application-menu-command (key items left)
  "Return a command dropping menu KEY, whose ITEMS a function returns.
LEFT is the pixel position of its title's left edge, where the menu drops.
A function whose `lme-platinum-panel' property is set drops a panel of
its own instead, called with KEY, LEFT and the menu bar's bottom."
  (lambda ()
    (interactive)
    (if (and (symbolp items) (get items 'lme-platinum-panel))
        (funcall items key left (lme-platinum--menu-y))
      (lme-platinum--with-open-menu key
        (lme-popup-menu (funcall items) left (lme-platinum--menu-y))))))

(defun lme-platinum--application-menus (menus left)
  "Return the menu bar titles of MENUS, the first starting LEFT pixels in.
MENUS is a value of `lme-platinum-menus-function'."
  (mapcar (pcase-lambda (`(,key ,title ,items))
            (let ((item (lme-platinum--menu-title
                         key title (lme-platinum--application-menu-command key items left)
                         title (eq lme-platinum--open-menu key))))
              (setq left (+ left (string-pixel-width (nth 2 item))))
              item))
          menus))

(defun lme-platinum-menu-bar ()
  "Return the left end of the menu bar: the λ menu, then the frontmost
application's menus, see `lme-platinum-menus-function'; without one, a
title per view, highlighted while the sidebar shows it, and Web, while the
browser is in front."
  (let ((lambda-title (lme-platinum--menu-title
                       'lme-lambda (lme-platinum--lambda) #'lme-platinum-lambda-menu
                       "LispM" (eq lme-platinum--open-menu 'lme-lambda))))
    (cons
     lambda-title
     (if lme-platinum-menus-function
         (lme-platinum--application-menus (funcall lme-platinum-menus-function)
                                          (string-pixel-width (nth 2 lambda-title)))
       (let ((shown (lme-sidebar-current-view)))
         (append
          (mapcar (lambda (view)
                    (let ((spec (cdr view)))
                      (lme-platinum--menu-title (intern (format "lme-view-%s" (car view)))
                                                (plist-get spec :title)
                                                (plist-get spec :command)
                                                (plist-get spec :help)
                                                (eq (car view) shown))))
                  lme-sidebar-views)
          (when (lme-platinum--browser-p)
            (list (lme-platinum--menu-title 'lme-view-web "Web" 'lme-browser "Browser"
                                            (lme-browser-workspace-p))))))))))

(defun lme-platinum-workspace-menu ()
  "Pop up the Application menu: the applications, see
`lme-platinum-applications-function', or else the workspaces, the current
one checked."
  (interactive)
  (let ((current (tab-bar--current-tab-index))
        (tabs (funcall tab-bar-tabs-function)))
    (lme-platinum--with-open-menu 'lme-application
      (lme-popup-menu
       (if lme-platinum-applications-function
           (funcall lme-platinum-applications-function)
         `(,@(seq-map-indexed
              (lambda (tab index)
                (list :label (alist-get 'name tab)
                      :action (lambda () (tab-bar-select-tab (1+ index)))
                      :checked (= index current)))
              tabs)
           separator
           (:label "New Workspace" :action tab-bar-new-tab)
           (:label "Close Workspace" :action tab-bar-close-tab :enabled ,(and (cdr tabs) t))))
       (- (frame-pixel-width) 4) (lme-platinum--menu-y) 'right))))

(defvar tabspaces-default-tab)

(defun lme-platinum-application-menu ()
  "Return the right end of the menu bar: the Application menu.
It names the current workspace with its icon, as Mac OS 9's named the
frontmost application: the hard disk for the default workspace, a
project's folder for a project's.  Clicked, it lists every workspace."
  (let ((name (alist-get 'name (tab-bar--current-tab)))
        (application (and lme-platinum-application-function
                          (funcall lme-platinum-application-function))))
    (list (lme-platinum--menu-title
           'lme-application
           (concat (cond (application (car application))
                         ((equal name (bound-and-true-p tabspaces-default-tab))
                          (lme-platinum-icon "disk"))
                         ((and (fboundp 'lme-browser-workspace-p) (lme-browser-workspace-p))
                          (lme-platinum-icon "netscape"))
                         (t (lme-platinum-folder-icon
                             (car (rassoc name (bound-and-true-p tabspaces-project-tab-map))))))
                   " " (or (cdr application) name) " ▾")
           #'lme-platinum-workspace-menu "Workspaces"
           (eq lme-platinum--open-menu 'lme-application)))))

;;;; Window bottoms

(defvar-local lme-platinum-info nil
  "What the bottom of this buffer's window says instead of the buffer's state.
List windows set it, as Finder's windows counted their items: a string,
or a function returning one when the window is drawn.")

(defun lme-platinum-draw-grow-box (palette height)
  "Return an SVG of the grow box, the ridged square in a window's corner.
Three diagonal ridges, each a lit line beside a shaded one, in PALETTE,
centred in HEIGHT pixels."
  (let* ((s 15)
         (svg (svg-create s height))
         (y (/ (- height s) 2)))
    (lme-platinum--fill svg 0 0 s height (plist-get palette :chrome))
    (dolist (offset '(4 8 12))
      (svg-line svg (- s offset) (+ y s) s (+ y s (- offset))
                :stroke (plist-get palette :shade) :stroke-width 1)
      (svg-line svg (- s offset -1) (+ y s) s (+ y s (- offset) 1)
                :stroke (plist-get palette :light) :stroke-width 1))
    svg))

(defun lme-platinum--grow-press (event)
  "Resize the child frame whose grow box EVENT pressed, from its corner.
The floating editor resizes so; elsewhere windows tile, and the grow box
only marks the corner."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (frame-parent frame))
      (mouse-drag-frame-resize event 'bottom-right)
      (run-hook-with-args 'lme-platinum-frame-moved-functions frame))))

(defvar lme-platinum--grow-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <down-mouse-1>" #'lme-platinum--grow-press)
    (keymap-set map "<mode-line> <mouse-1>" #'ignore)
    map)
  "Keymap of the grow box.")

(defun lme-platinum--grow-box ()
  "Return the grow box as a string."
  (propertize " " 'display (lme-platinum--image
                            (list 'grow)
                            (lambda () (lme-platinum-draw-grow-box lme-platinum--palette
                                                                    lme-platinum-title-height)))
              'keymap lme-platinum--grow-map))

(defun lme-platinum--placard (text active)
  "Return TEXT as a placard, the raised plate Mac OS 9 showed status in.
ACTIVE draws it raised; an inactive window's placards lie flat.  The
placard's face is layered under TEXT's own, so coloured counts survive."
  (let* ((pad (propertize " " 'display '(space :width (6))))
         (placard (concat pad text pad)))
    (add-face-text-property 0 (length placard)
                            (if active 'lme-platinum-placard 'lme-platinum-placard-inactive)
                            t placard)
    placard))

(defun lme-platinum--mode-line-text (construct)
  "Return mode line CONSTRUCT formatted, trimmed, and safe to show again.
It is formatted for the current buffer, the one whose window is drawn."
  (string-replace "%" "%%" (string-trim (format-mode-line construct nil nil
                                                          (current-buffer)))))

(defvar evil-mode-line-tag)
(defvar flymake-mode-line-counters)

(defun lme-platinum-mode-line-left ()
  "The left of a window's bottom edge: its state, buffer and position.
A list window shows its `lme-platinum-info' instead."
  (let ((active (mode-line-window-selected-p)))
    (concat
     (propertize " " 'display (lme-platinum--strut 8))
     (if lme-platinum-info
         (lme-platinum--placard (string-replace "%" "%%" (if (functionp lme-platinum-info)
                                                              (funcall lme-platinum-info)
                                                            lme-platinum-info))
                                active)
       (mapconcat
        (lambda (text) (lme-platinum--placard text active))
        (delq nil
              (list (when (and (bound-and-true-p evil-local-mode)
                               (stringp evil-mode-line-tag))
                      (string-trim (substring-no-properties evil-mode-line-tag) "[ <]+" "[> ]+"))
                    (concat (string-replace "%" "%%" (or lme-platinum-title (buffer-name)))
                            (when (and buffer-file-name (buffer-modified-p)) " •"))
                    (lme-platinum--mode-line-text mode-line-position)))
        " ")))))

(defun lme-platinum-mode-line-right ()
  "The right of a window's bottom edge: diagnostics, branch, mode, grow box.
Only the active window shows the grow box, and only where the window can
be resized; a side window cannot."
  (let* ((active (mode-line-window-selected-p))
         (parts (unless lme-platinum-info
                  (delq nil
                        (list (when (bound-and-true-p flymake-mode)
                                (let ((counts (lme-platinum--mode-line-text
                                               flymake-mode-line-counters)))
                                  (unless (string-empty-p counts) counts)))
                              (when (stringp vc-mode)
                                (replace-regexp-in-string
                                 "\\`[A-Za-z]+[-:@!?]" ""
                                 (string-trim (substring-no-properties vc-mode))))
                              (lme-platinum--mode-line-text mode-name))))))
    (concat (mapconcat (lambda (text) (lme-platinum--placard text active)) parts " ")
            (propertize " " 'display '(space :width (6)))
            (if (and active (not (window-parameter (selected-window) 'window-side)))
                (lme-platinum--grow-box)
              (lme-platinum--space 15))
            (lme-platinum--space 3))))

(defconst lme-platinum-mode-line-format
  '((:eval (lme-platinum-mode-line-left))
    mode-line-format-right-align
    (:eval (lme-platinum-mode-line-right)))
  "The mode line of every window: its bottom edge, laid out as Mac OS 9's.")

;;;; Scroll bars
;;
;; Mac OS 9's scroll bar runs down a window's right edge: an arrow box at
;; each end, a recessed track, and a thumb in the variation colour with
;; grip ridges.  Each window that scrolls gets one, drawn as an SVG in a
;; child frame laid over the window's right fringe, which is widened to
;; make room:
;;
;;   window --geometry--> child frame position and height
;;   window --start, end, size--> thumb --SVG--> child frame's one line
;;
;; Bars follow the windows on every command and scroll, and an inactive
;; window's bar shows its empty track, as Mac OS 9's did.  Sessions
;; (terminals and chats) scroll their own way and get none.

(defcustom lme-platinum-scroll-bars t
  "Non-nil gives windows Mac OS 9's scroll bars, see `lme-platinum-scroll-sync'."
  :type 'boolean)

(defconst lme-platinum-scroll-width 15
  "Width in pixels of a scroll bar, and the height of its arrow boxes.")

(defun lme-platinum-scroll-thumb (height start end min max)
  "Return the thumb of a scroll bar HEIGHT pixels tall, as (TOP . LENGTH).
The window shows from START to END of a buffer spanning MIN to MAX.  The
value is nil when everything shows, or when the track is too short."
  (let* ((arrow lme-platinum-scroll-width)
         (track (- height (* 2 arrow)))
         (size (- max min))
         (shown (- end start)))
    (when (and (> size 0) (> track 24) (or (> start min) (< end max)))
      (let* ((length (min track (max 16 (round (* track (/ (float shown) size))))))
             (fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (+ arrow (round (* (- track length) (min 1.0 fraction)))) length)))))

(defun lme-platinum--bevel (svg x y width height palette lit shaded fill)
  "Draw a box of WIDTH by HEIGHT at X, Y of SVG: framed, FILL lit and shaded.
LIT and SHADED colour its top-left and bottom-right inner edges; the
frame is PALETTE's."
  (lme-platinum--fill svg x y width height (plist-get palette :frame))
  (lme-platinum--fill svg (1+ x) (1+ y) (- width 2) (- height 2) fill)
  (lme-platinum--fill svg (1+ x) (1+ y) (- width 2) 1 lit)
  (lme-platinum--fill svg (1+ x) (1+ y) 1 (- height 2) lit)
  (lme-platinum--fill svg (1+ x) (+ y height -2) (- width 2) 1 shaded)
  (lme-platinum--fill svg (+ x width -2) (1+ y) 1 (- height 2) shaded))

(defun lme-platinum-draw-scroll-bar (palette height thumb active)
  "Return an SVG of a scroll bar HEIGHT pixels tall in PALETTE.
THUMB, as returned by `lme-platinum-scroll-thumb', places the thumb; nil
draws none, and the arrows dimmed.  An inactive bar, ACTIVE nil, shows
only its empty track."
  (let* ((w lme-platinum-scroll-width)
         (svg (svg-create w height))
         (chrome (plist-get palette :chrome))
         (track (lme-tab-bar--blend chrome (plist-get palette :shade) 0.3))
         (accent (plist-get palette :accent)))
    ;; The track: recessed, a frame line on its left and a shadow inside it.
    (lme-platinum--fill svg 0 0 w height (plist-get palette :frame))
    (lme-platinum--fill svg 1 0 (1- w) height track)
    (lme-platinum--fill svg 1 0 1 height (plist-get palette :shade))
    (when active
      (dolist (end '(up down))
        (let ((y (if (eq end 'up) 0 (- height w)))
              (ink (plist-get palette (if thumb :text :muted))))
          (lme-platinum--bevel svg 0 y w w palette
                               (plist-get palette :light) (plist-get palette :shade) chrome)
          (svg-polygon svg (if (eq end 'up)
                               `((7.5 . ,(+ y 4)) (3.5 . ,(+ y 9)) (11.5 . ,(+ y 9)))
                             `((7.5 . ,(+ y 10)) (3.5 . ,(+ y 5)) (11.5 . ,(+ y 5))))
                       :fill ink :stroke "none")))
      (when thumb
        (let ((top (car thumb)) (length (cdr thumb)))
          (lme-platinum--bevel svg 0 top w length palette
                               (lme-tab-bar--blend accent "#FFFFFF" 0.45)
                               (lme-tab-bar--blend accent "#000000" 0.4)
                               accent)
          ;; Grip ridges across the thumb's middle.
          (when (>= length 14)
            (let ((middle (+ top (/ length 2))))
              (dolist (offset '(-4 -1 2))
                (lme-platinum--fill svg 4 (+ middle offset) 7 1
                                    (lme-tab-bar--blend accent "#FFFFFF" 0.45))
                (lme-platinum--fill svg 4 (+ middle offset 1) 7 1
                                    (lme-tab-bar--blend accent "#000000" 0.4))))))))
    svg))

(defvar lme-platinum--scroll-bars nil
  "Alist of windows to the child frames holding their scroll bars.")

(defvar lme-platinum--scroll-timer nil
  "Pending timer bringing the scroll bars up to date, if any.")

(declare-function lme-sidebar-buffer-view "lme-sidebar" (buffer))
(declare-function lme-terminal-buffer-p "lme-interface" (buffer))

(defun lme-platinum--scroll-window-p (window)
  "Return non-nil when WINDOW gets a scroll bar.
Every window of the main area does but sessions, the browser's page and
the desktop; of side windows, only the sidebar's lists."
  (let ((buffer (window-buffer window)))
    (and (not (window-minibuffer-p window))
         (not (window-parameter window 'lme-chrome))
         (not (lme-terminal-buffer-p buffer))
         (not (let ((mode (buffer-local-value 'major-mode buffer)))
                (or (provided-mode-derived-p mode 'xwidget-webkit-mode)
                    (provided-mode-derived-p mode 'lme-desktop-mode)
                    (provided-mode-derived-p mode 'lme-desktop-empty-mode))))
         (or (not (window-parameter window 'window-side))
             (lme-sidebar-buffer-view buffer)
             (eq (buffer-local-value 'major-mode buffer) 'treemacs-mode)))))

(defvar lme-platinum--scroll-map
  (let ((map (make-sparse-keymap)))
    (dolist (press '("<down-mouse-1>" "<double-down-mouse-1>" "<triple-down-mouse-1>"))
      (keymap-set map press #'lme-platinum-scroll-press))
    (dolist (release '("<mouse-1>" "<double-mouse-1>" "<triple-mouse-1>" "<drag-mouse-1>"))
      (keymap-set map release #'ignore))
    (keymap-set map "<wheel-up>" #'lme-platinum-scroll-wheel)
    (keymap-set map "<wheel-down>" #'lme-platinum-scroll-wheel)
    map)
  "Keymap of a scroll bar's buffer.")

(defun lme-platinum--scroll-frame (window)
  "Return the child frame holding WINDOW's scroll bar, made when missing.
It takes no focus and hooks run for new frames do not see it."
  (let ((frame (window-parameter window 'lme-platinum-scroll-frame)))
    (if (frame-live-p frame)
        frame
      (let* ((buffer (generate-new-buffer " *lme-scroll-bar*"))
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
                         (background-color . ,(lme-platinum--color :chrome))
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
                      lme-platinum--scroll-target window)
          (use-local-map lme-platinum--scroll-map))
        (let ((root (frame-root-window frame)))
          (set-window-buffer root buffer)
          (set-window-dedicated-p root t)
          (set-window-fringes root 0 0)
          (set-window-margins root 0 0))
        (set-frame-parameter frame 'lme-platinum-scroll-target window)
        (set-window-parameter window 'lme-platinum-scroll-frame frame)
        (push (cons window frame) lme-platinum--scroll-bars)
        frame))))

(defvar-local lme-platinum--scroll-target nil
  "The window whose scroll bar this buffer draws.")

(defun lme-platinum--scroll-update (window)
  "Bring WINDOW's scroll bar up to date: place it, size it, draw it."
  (let ((width lme-platinum-scroll-width))
    (unless (= (or (cadr (window-fringes window)) 0) width)
      (set-window-fringes window nil width nil t))
    (let* ((frame (lme-platinum--scroll-frame window))
           (inside (window-inside-pixel-edges window))
           (x (- (nth 2 (window-pixel-edges window))
                 (window-right-divider-width window)
                 (window-scroll-bar-width window)
                 width))
           (y (nth 1 inside))
           (height (max 1 (- (nth 3 inside) y)))
           (buffer (window-buffer window))
           (thumb (with-current-buffer buffer
                    (lme-platinum-scroll-thumb height (window-start window)
                                               (window-end window t)
                                               (point-min) (point-max))))
           (active (eq window (frame-selected-window (window-frame window))))
           (place (list x y height))
           (key (list lme-platinum--generation height thumb active)))
      (unless (equal place (frame-parameter frame 'lme-platinum-scroll-place))
        (set-frame-parameter frame 'lme-platinum-scroll-place place)
        ;; Sized in pixels, below the width of the ten columns a window
        ;; may otherwise not shrink under.
        (let ((window-min-width 1)
              (window-safe-min-width 1))
          (set-frame-size frame width height t))
        (set-frame-position frame x y))
      (unless (equal key (frame-parameter frame 'lme-platinum-scroll-key))
        (set-frame-parameter frame 'lme-platinum-scroll-key key)
        (set-frame-parameter frame 'lme-platinum-scroll-thumb thumb)
        (set-frame-parameter frame 'background-color (lme-platinum--color :chrome))
        (let ((root (frame-root-window frame)))
          (with-current-buffer (window-buffer root)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize " " 'display
                                  (svg-image (lme-platinum-draw-scroll-bar
                                              lme-platinum--palette height thumb active)
                                             :ascent 100 :scale 1)))))
          (set-window-point root 1)
          (set-window-hscroll root 0)))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))))

(defun lme-platinum--scroll-drop (window frame)
  "Take WINDOW's scroll bar, held in child FRAME, away."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame))))
      (delete-frame frame t)
      (kill-buffer buffer)))
  (when (window-live-p window)
    (set-window-parameter window 'lme-platinum-scroll-frame nil)
    (set-window-fringes window nil nil nil t)))

(defun lme-platinum-scroll-sync ()
  "Give every window that wants a scroll bar its own, up to date, and
take the bars of windows gone or no longer wanting one away."
  (setq lme-platinum--scroll-timer nil)
  (let ((wanted nil))
    (when (and lme-platinum-scroll-bars lme-platinum--palette)
      (dolist (frame (frame-list))
        (when (and (frame-visible-p frame) (not (frame-parent frame))
                   (display-graphic-p frame))
          (dolist (window (window-list frame 'no-mini))
            (when (lme-platinum--scroll-window-p window)
              (push window wanted)
              (lme-platinum--scroll-update window))))))
    (setq lme-platinum--scroll-bars
          (seq-filter (pcase-lambda (`(,window . ,frame))
                        (or (and (memq window wanted) (frame-live-p frame))
                            (progn (lme-platinum--scroll-drop window frame) nil)))
                      lme-platinum--scroll-bars))))

(defun lme-platinum--scroll-schedule (&rest _)
  "Bring the scroll bars up to date once Emacs has redisplayed."
  (unless (timerp lme-platinum--scroll-timer)
    (setq lme-platinum--scroll-timer
          (run-with-idle-timer 0 nil #'lme-platinum-scroll-sync))))

(defun lme-platinum--scroll-in (window step)
  "Call STEP, a function of no arguments, with WINDOW selected; redraw its bar."
  (with-selected-window window
    (ignore-errors (funcall step)))
  (lme-platinum--scroll-update window)
  (redisplay t))

(defun lme-platinum--scroll-repeat (window step)
  "Call STEP in WINDOW, and again and again while the mouse button is held.
As Mac OS 9's arrows and track did, holding repeats after a pause."
  (lme-platinum--scroll-in window step)
  (when (sit-for 0.35)
    (while (sit-for 0.05)
      (lme-platinum--scroll-in window step))))

(defun lme-platinum--scroll-mouse-y (frame)
  "Return the mouse's height in pixels from the top of scroll bar FRAME."
  (pcase-let ((`(,over ,_ . ,y) (mouse-pixel-position)))
    (cond ((null y) 0)
          ((eq over frame) y)
          (t (- y (cdr (frame-position frame)))))))

(defun lme-platinum--scroll-drag (window frame grab thumb)
  "Drag WINDOW's thumb, held GRAB pixels from its top, until the button rises.
THUMB is its (TOP . LENGTH) when the drag began; FRAME holds the bar."
  (let* ((width lme-platinum-scroll-width)
         (height (frame-pixel-height frame))
         (track (- height (* 2 width)))
         (length (cdr thumb)))
    (track-mouse
      (while (mouse-movement-p (let ((echo-keystrokes 0)) (read-event)))
        (let* ((top (max width (min (- height width length)
                                    (- (lme-platinum--scroll-mouse-y frame) grab))))
               (fraction (/ (float (- top width)) (max 1 (- track length)))))
          (lme-platinum--scroll-in
           window
           (lambda ()
             (let* ((shown (- (window-end nil t) (window-start)))
                    (span (max 0 (- (point-max) (point-min) shown)))
                    (position (+ (point-min) (round (* fraction span)))))
               (set-window-start nil (save-excursion (goto-char position)
                                                     (line-beginning-position)))))))))))

(defun lme-platinum-scroll-press (event)
  "Act on a press in a scroll bar, as Mac OS 9's scroll bars did.
The arrows scroll a line and the track above or below the thumb a page,
over and over while held; the thumb drags the window's text along."
  (interactive "e")
  (let* ((position (event-start event))
         (frame (window-frame (posn-window position)))
         (window (frame-parameter frame 'lme-platinum-scroll-target))
         (thumb (frame-parameter frame 'lme-platinum-scroll-thumb))
         (y (cdr (posn-x-y position)))
         (height (frame-pixel-height frame))
         (width lme-platinum-scroll-width))
    (when (window-live-p window)
      (cond ((< y width) (lme-platinum--scroll-repeat window (lambda () (scroll-down 1))))
            ((>= y (- height width)) (lme-platinum--scroll-repeat window (lambda () (scroll-up 1))))
            ((null thumb))
            ((< y (car thumb)) (lme-platinum--scroll-repeat window #'scroll-down))
            ((>= y (+ (car thumb) (cdr thumb))) (lme-platinum--scroll-repeat window #'scroll-up))
            (t (lme-platinum--scroll-drag window frame (- y (car thumb)) thumb))))))

(defun lme-platinum-scroll-wheel (event)
  "Scroll the window of the scroll bar the mouse wheel turned over, by EVENT."
  (interactive "e")
  (let ((window (frame-parameter (window-frame (posn-window (event-start event)))
                                 'lme-platinum-scroll-target)))
    (when (window-live-p window)
      (lme-platinum--scroll-in window (if (eq (event-basic-type event) 'wheel-up)
                                          (lambda () (scroll-down 3))
                                        (lambda () (scroll-up 3)))))))

(defun lme-platinum-scroll-enable ()
  "Keep the scroll bars in step with windows, scrolling and selection."
  (add-hook 'post-command-hook #'lme-platinum--scroll-schedule)
  (add-hook 'window-scroll-functions #'lme-platinum--scroll-schedule)
  (add-hook 'window-size-change-functions #'lme-platinum--scroll-schedule)
  (add-hook 'window-selection-change-functions #'lme-platinum--scroll-schedule)
  (add-hook 'window-buffer-change-functions #'lme-platinum--scroll-schedule)
  (lme-platinum--scroll-schedule))

;;;; Status: Emacs's own time, load, mail and battery

;; Emacs keeps the time, the load average and the mail flag up to date
;; (`display-time-mode') and the battery (`display-battery-mode'); the
;; menu bar shows each as a title of its own at its right end, as Mac OS 9
;; showed its clock there, each opening a menu:
;;
;;   load      a meter and the load average; the menu picks the 1, 5 or
;;             15 minute average and opens the process list
;;   mail      an envelope, flagged when mail has come; the Mail menu
;;             reads, composes and checks mail
;;   battery   a PowerBook's battery filled to its charge; the menu tells
;;             the charge, the power source and the time left
;;   time      the time as `display-time-format' has it; the menu gives
;;             the date, the calendar and the world clock
;;
;; Emacs computes everything; `display-time-hook' and
;; `battery-update-functions' only note what it found.  Whatever else is
;; in `global-mode-string', such as LispM's agent count, stays before.

(defvar battery-status-function)
(defvar battery-echo-area-format)
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

(defvar lme-platinum--load ""
  "The load average `display-time-mode' last showed, \"\" below its threshold.")

(defvar lme-platinum--mail nil
  "Non-nil when `display-time-mode' last found new mail.")

(defvar lme-platinum--battery nil
  "The battery status `display-battery-mode' last read, an alist.")

(defun lme-platinum--note-time ()
  "Note the load average and the mail flag Emacs has just computed.
Run from `display-time-hook', inside `display-time-update'."
  (setq lme-platinum--load (string-trim (substring-no-properties load))
        lme-platinum--mail (and mail t)))

(defun lme-platinum--note-battery (data)
  "Note the battery status DATA Emacs has just read."
  (setq lme-platinum--battery data))

(defun lme-platinum-draw-battery (palette percent charging)
  "Return an SVG of a battery filled to PERCENT in PALETTE.
CHARGING draws a bolt across it; a nearly empty one is filled in red."
  (let* ((svg (svg-create 24 lme-platinum-menu-icon-height))
         (y (/ (- lme-platinum-menu-icon-height 11) 2))
         (ink (plist-get palette :text))
         (level (round (* 17 (/ (max 0 (min 100 percent)) 100.0)))))
    (lme-platinum--fill svg 0 y 21 11 ink)
    (lme-platinum--fill svg 1 (1+ y) 19 9 (plist-get palette :chrome))
    (lme-platinum--fill svg 21 (+ y 3) 2 5 ink)
    (lme-platinum--fill svg 2 (+ y 2) level 7 (if (< percent 10) "#CE3131" ink))
    (when charging
      (svg-polygon svg `((12 . ,(+ y 1)) (7 . ,(+ y 6)) (10.5 . ,(+ y 6))
                         (9 . ,(+ y 10)) (14 . ,(+ y 5)) (10.5 . ,(+ y 5)))
                   :fill (plist-get palette :accent) :stroke (plist-get palette :chrome)
                   :stroke-width 0.75))
    svg))

(defun lme-platinum-draw-meter (palette fraction)
  "Return an SVG of a load meter in PALETTE: three bars, filled to FRACTION."
  (let* ((svg (svg-create 14 lme-platinum-menu-icon-height))
         (bottom (+ (/ lme-platinum-menu-icon-height 2) 6))
         (ink (plist-get palette :text))
         (dim (plist-get palette :muted)))
    (dotimes (i 3)
      (let ((height (* 4 (1+ i))))
        (lme-platinum--fill svg (* i 5) (- bottom height) 4 height
                            (if (> fraction (/ i 3.0)) ink dim))))
    svg))

(defun lme-platinum--status-title (key image label command help)
  "Return the menu bar status item KEY: IMAGE (an SVG or nil) and LABEL.
It runs COMMAND, described by HELP, and is highlighted while its menu is open."
  (lme-platinum--menu-title
   key
   (concat (when image
             ;; An icon name gives a string already displayed as the icon.
             (if (stringp image) (lme-platinum-icon image) (propertize " " 'display image)))
           (when (and image label) " ")
           label)
   command help (eq lme-platinum--open-menu key)))

(defun lme-platinum--status-menu (key items)
  "Pop up ITEMS under the status title KEY that was just clicked."
  (let ((x (car (posn-x-y (event-start last-input-event)))))
    (lme-platinum--with-open-menu key
      (lme-popup-menu items (max 0 (- x 12)) (lme-platinum--menu-y)))))

(defun lme-platinum-load-menu ()
  "Pop up the load menu: which average to show, and the processes."
  (interactive)
  (let ((averages (ignore-errors (load-average t))))
    (lme-platinum--status-menu
     'lme-load
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
       (:label "Processes" :action proced)))))

(defun lme-platinum-mail-menu ()
  "Pop up the Mail menu: read, compose and check mail."
  (interactive)
  (lme-platinum--status-menu
   'lme-mail
   `((:label ,(if lme-platinum--mail "You have new mail" "No new mail")
             :enabled nil)
     separator
     (:label "Read Mail" :action ,(lambda () (call-interactively read-mail-command)))
     (:label "Compose Mail…" :action compose-mail)
     separator
     (:label "Check for Mail" :action display-time-update))))

(defun lme-platinum-battery-menu ()
  "Pop up the battery menu: its charge, its power source and the time left."
  (interactive)
  (let* ((data lme-platinum--battery)
         (line (lambda (format) (battery-format format data))))
    (lme-platinum--status-menu
     'lme-battery
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
       (:label "Battery Status" :action battery)))))

(defun lme-platinum-time-menu ()
  "Pop up the clock's menu: the date, the calendar, the world clock."
  (interactive)
  (lme-platinum--status-menu
   'lme-time
   `((:label ,(format-time-string "%A, %B %-e, %Y") :enabled nil)
     separator
     (:label "Calendar" :action calendar)
     (:label "World Clock" :action world-clock)
     separator
     (:label "Time Format…" :action ,(lambda () (customize-variable 'display-time-format))))))

(defun lme-platinum-menu-status ()
  "Return the status titles at the menu bar's right end.
Whatever `global-mode-string' holds besides the time and the battery,
then the load, the mail, the battery and the time."
  (let* ((others (string-trim
                  (format-mode-line
                   (seq-remove (lambda (part)
                                 (memq part '(display-time-string battery-mode-line-string)))
                               (ensure-list global-mode-string)))))
         (time-on (bound-and-true-p display-time-mode))
         (percent (and (bound-and-true-p display-battery-mode) lme-platinum--battery
                       (ignore-errors (string-to-number (alist-get ?p lme-platinum--battery))))))
    (delq nil
          (list
           (unless (string-empty-p others)
             (lme-platinum--menu-title 'lme-status others #'ignore nil))
           (when (and time-on (not (string-empty-p lme-platinum--load)))
             (lme-platinum--status-title
              'lme-load
              (lme-platinum--image
               (list 'meter (min 3 (floor (* 3 (/ (string-to-number lme-platinum--load)
                                                 (max 1 (num-processors)))))))
               (lambda ()
                 (lme-platinum-draw-meter lme-platinum--palette
                                          (/ (string-to-number lme-platinum--load)
                                             (float (max 1 (num-processors)))))))
              lme-platinum--load #'lme-platinum-load-menu "Load average"))
           (when time-on
             (lme-platinum--status-title 'lme-mail (if lme-platinum--mail "mail-new" "mail")
                                         nil #'lme-platinum-mail-menu
                                         (if lme-platinum--mail "You have new mail" "Mail")))
           (when percent
             (let ((charging (member (alist-get ?L lme-platinum--battery) '("AC" "on-line"))))
               (lme-platinum--status-title
                'lme-battery
                (lme-platinum--image (list 'battery (round percent) charging)
                                     (lambda () (lme-platinum-draw-battery lme-platinum--palette
                                                                           percent charging)))
                (format "%d%%" (round percent)) #'lme-platinum-battery-menu
                (battery-format battery-echo-area-format lme-platinum--battery))))
           (when time-on
             (lme-platinum--status-title
              'lme-time nil (format-time-string (or display-time-format "%-I:%M %p"))
              #'lme-platinum-time-menu (format-time-string "%A, %B %-e, %Y")))))))

(defun lme-platinum--start-status ()
  "Turn on Emacs's own time, load and mail, and the battery if there is one.
The time reads as Mac OS 9's clock did, unless `display-time-format' was
set already."
  (require 'battery)
  (require 'time)
  (add-hook 'display-time-hook #'lme-platinum--note-time)
  (add-hook 'battery-update-functions #'lme-platinum--note-battery)
  (unless display-time-format
    (setq display-time-format "%-I:%M %p"))
  (display-time-mode 1)
  (when (and battery-status-function
             (not (equal (alist-get ?p (ignore-errors (funcall battery-status-function)))
                         "N/A")))
    (display-battery-mode 1)))

;;;; Lists

(defconst lme-platinum-column-width 11
  "Width in columns of a list's right-hand column, the one it is sorted by.")

(defun lme-platinum-columns (name meta)
  "Return a list's column headings NAME and META as one line of bevels.
META, the column the list is sorted by, is shaded as Mac OS 9 shaded it."
  (concat (propertize (concat " " name)
                      'face 'lme-platinum-column)
          (propertize " " 'display `(space :align-to (- right ,lme-platinum-column-width))
                      'face 'lme-platinum-column)
          (propertize (concat " " meta " ")
                      'face 'lme-platinum-column-sorted)
          (propertize " " 'display '(space :align-to right)
                      'face 'lme-platinum-column-sorted)
          "\n"))

;;;; Treemacs

(declare-function treemacs-load-theme "treemacs-themes" (name))

(defun lme-platinum-treemacs-theme ()
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
           (row (lambda (lead icon) (concat lead gap (lme-platinum-icon icon) gap))))
      (eval
       `(treemacs-create-theme "lme-platinum"
          :config
          (progn
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle t) "disk")
                                  :extensions (root-open))
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle nil) "disk")
                                  :extensions (root-closed))
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle t) "folder")
                                  :extensions (dir-open))
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle nil) "folder")
                                  :extensions (dir-closed))
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle t) "document-blank")
                                  :extensions (tag-open))
            (treemacs-create-icon :icon ,(funcall row (lme-platinum-triangle nil) "document-blank")
                                  :extensions (tag-closed))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-blank")
                                  :extensions (tag-leaf))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-image")
                                  :extensions ("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp"
                                               "tif" "tiff" "ico" "heic"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-lisp")
                                  :extensions ,lme-platinum-lisp-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-markdown")
                                  :extensions ("md" "markdown" "mkd"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-org")
                                  :extensions ("org"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-code")
                                  :extensions ,lme-platinum-code-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document")
                                  :extensions (fallback))))
       t)
      (treemacs-load-theme "lme-platinum"))))

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

(defface lme-platinum-sorted nil
  "Face of the column a list is sorted by: shaded down its length, as in
Finder's list view."
  :group 'lme-platinum)

(defface lme-platinum-placard '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a placard on the active window's bottom edge."
  :group 'lme-platinum)

(defface lme-platinum-placard-inactive '((t :inherit shadow))
  "Face of a placard on an inactive window's bottom edge."
  :group 'lme-platinum)

(defface lme-platinum-toolbar nil
  "Face of a toolbar: the browser's, around its address field."
  :group 'lme-platinum)

(defface lme-platinum-toolbar-label nil
  "Face of a label in a toolbar, such as the address field's."
  :group 'lme-platinum)

(defface lme-platinum-field '((t :box (:line-width (1 . 1) :style pressed-button)))
  "Face of an edit text field: inset, in the list colour."
  :group 'lme-platinum)

(defface lme-platinum-tab '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a tab of a tab control, behind the one chosen."
  :group 'lme-platinum)

(defface lme-platinum-tab-selected '((t :weight bold :box (:line-width (1 . 1) :style released-button)))
  "Face of the chosen tab of a tab control: in front, in the content's colour."
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
      (setq lme-platinum--palette (lme-platinum--merge-palette
                                   (lme-platinum--derive-palette content text dim accent)
                                   lme-platinum-colors)
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
        (set-face-attribute 'mode-line nil
                            :inherit 'unspecified :background chrome :foreground text
                            :box nil :overline 'unspecified :underline 'unspecified
                            :height 'unspecified)
        (set-face-attribute 'mode-line-inactive nil
                            :inherit 'unspecified :background chrome
                            :foreground (lme-platinum--color :muted)
                            :box nil :overline 'unspecified :underline 'unspecified
                            :height 'unspecified)
        (set-face-attribute 'lme-platinum-sorted nil
                            :background (lme-tab-bar--blend (lme-platinum--color :content)
                                                            chrome 0.3)
                            :extend t)
        ;; Balloon Help: a note in the list's type, on the header's grey.
        ;; It carries no outline: macOS sizes a tooltip without its box,
        ;; which would cut the right edge off.
        (set-face-attribute 'tooltip nil
                            :inherit 'lme-platinum-list :background header
                            :foreground text :height 0.9 :box nil)
        (set-face-attribute 'lme-platinum-placard nil
                            :inherit 'lme-platinum-list :background header :foreground text
                            :height 0.95
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lme-platinum-placard-inactive nil
                            :inherit 'lme-platinum-list :background chrome
                            :foreground (lme-platinum--color :muted) :height 0.95
                            :box `(:line-width (1 . 1) :color ,chrome))
        (dolist (face '(lme-platinum-title lme-platinum-title-inactive
                        lme-platinum-menu-title lme-platinum-menu-title-active))
          (set-face-attribute face nil :family (or system 'unspecified) :height 120
                              :weight 'bold))
        (set-face-attribute 'lme-platinum-title nil :foreground text)
        (set-face-attribute 'lme-platinum-title-inactive nil
                            :foreground (lme-platinum--color :muted) :inherit 'unspecified)
        ;; A box in each title's own fill pads it vertically, so the bar is
        ;; as tall as Mac OS 9's and a highlight fills its height.
        (let ((pad (max 0 (/ (- lme-platinum-menu-bar-height lme-platinum-menu-icon-height) 2))))
          (set-face-attribute 'lme-platinum-menu-title nil
                              :foreground text :background chrome :inverse-video nil
                              :box `(:line-width (1 . ,pad) :color ,chrome))
          (set-face-attribute 'lme-platinum-menu-title-active nil
                              :foreground (lme-platinum--color :content) :background accent
                              :inverse-video nil
                              :box `(:line-width (1 . ,pad) :color ,accent)))
        (set-face-attribute 'lme-platinum-list nil :family (or list-family 'unspecified))
        (set-face-attribute 'lme-platinum-header nil
                            :inherit 'lme-platinum-list :background header :foreground dim
                            :box `(:line-width (1 . 4) :color ,header) :underline nil)
        (set-face-attribute 'lme-platinum-toolbar nil
                            :inherit 'lme-platinum-list :background header :foreground text)
        (set-face-attribute 'lme-platinum-toolbar-label nil
                            :inherit 'lme-platinum-toolbar :foreground dim)
        (set-face-attribute 'lme-platinum-field nil
                            :inherit 'lme-platinum-list :background (lme-platinum--color :content)
                            :foreground text
                            :box '(:line-width (1 . 1) :style pressed-button))
        (set-face-attribute 'lme-platinum-button nil
                            :inherit 'lme-platinum-list :background chrome :foreground text
                            :box '(:line-width (1 . 1) :style released-button))
        ;; Tabs as Mac OS 9's tab controls drew them: the chosen one in
        ;; front, in the colour of what it shows, the others behind it.
        (set-face-attribute 'lme-platinum-tab nil
                            :inherit 'lme-platinum-list :background chrome
                            :foreground (lme-platinum--color :muted)
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lme-platinum-tab-selected nil
                            :inherit 'lme-platinum-list
                            :background (lme-platinum--color :content)
                            :foreground text :weight 'bold
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
        ;; Menus and alerts are outlined as Mac OS 9's were.
        (set-face-attribute 'child-frame-border nil :background (lme-platinum--color :frame))
        ;; Neighbouring windows meet as Mac OS 9's windows were framed: each
        ;; edge a dark outline, platinum between them.
        (set-face-attribute 'window-divider nil :foreground chrome)
        (set-face-attribute 'window-divider-first-pixel nil
                            :foreground (lme-platinum--color :frame))
        (set-face-attribute 'window-divider-last-pixel nil
                            :foreground (lme-platinum--color :frame))))))

;;;; Setup

(defun lme-setup-platinum ()
  "Lay the frame out as Mac OS 9 did, in the active theme's colours."
  (setq-default tab-line-format '(:eval (lme-platinum-title-bar))
                mode-line-format lme-platinum-mode-line-format)
  (setq mode-line-right-align-edge 'window)
  (setq tab-bar-format '(lme-platinum-menu-bar
                         tab-bar-format-align-right
                         lme-platinum-menu-status
                         lme-platinum-application-menu))
  (lme-platinum--start-status)
  ;; Frames take sizes in pixels: a scroll bar is exactly as tall as its
  ;; window, and a maximized frame fills the screen.
  (setq frame-resize-pixelwise t)
  (lme-platinum-scroll-enable)
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
  (lme-platinum-apply-faces)
  (add-hook 'enable-theme-functions #'lme-platinum-apply-faces)
  (with-eval-after-load 'treemacs
    (lme-platinum-treemacs-theme)
    (add-hook 'enable-theme-functions (lambda (&rest _) (lme-platinum-treemacs-theme)) 90))
  (add-hook 'which-key-init-buffer-hook #'lme-platinum-no-title-bar)
  (add-hook 'transient-setup-buffer-hook #'lme-platinum-no-title-bar))


(provide 'lme-platinum)
;;; lme-platinum.el ends here
