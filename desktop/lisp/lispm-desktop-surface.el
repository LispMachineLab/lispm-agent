;;; lispm-desktop-surface.el --- The desktop behind every window  -*- lexical-binding: t; -*-

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

;; Mac OS 9's desktop lay behind every window, and its icons opened the
;; disks, the applications and the Trash.  Here the desktop is a workspace
;; of its own, the one Emacs starts in, and one of the applications (see
;; lispm-desktop-apps.el): in front, its menus are File and Special.  Where another
;; workspace's main area holds nothing, the desktop's colour shows there,
;; without its icons, as the desktop seen through.
;;
;; Its icons stand down the right edge, where Mac OS 9 put them: the home
;; disk at the top, Emacs, the browser and the Control Panels folder under
;; it, the Trash in the bottom corner.
;; A click selects an icon, darkening it and inverting its name, and a
;; double click opens it; the arrow keys select, Return or Command-O opens.
;; An icon is dragged anywhere on the desktop, a translucent copy following
;; the mouse, and is dropped on the grid the column lies on, in the free
;; cell nearest; Arrange Icons, in the View menu and the desktop's
;; right-click menu, puts every icon back where it stood at first.
;;
;;   window --size--> layout: each icon where it was put, from the edges
;;                    nearest it, or in the column; the Trash at the bottom
;;   icon --selected--> its SVG with every colour halved

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)

(declare-function lispm-desktop-browser-show "lispm-desktop-browser" ())
(declare-function lispm-desktop-settings-open-folder "lispm-desktop-settings" ())
(declare-function lispm-desktop-xwidget-available-p "lispm-desktop-browser" ())
(declare-function lispm-desktop-editor-activate "lispm-desktop-editor" ())
(declare-function lispm-desktop-finder-home "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(declare-function lispm-desktop-finder-shown-p "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-open "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-close "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder--buffer "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-real-p "lispm-desktop-finder" ())
(defvar lispm-desktop-finder--selected)
(defvar lispm-desktop-finder--had-focus)
(declare-function lispm-desktop-main-area--windows "lispm-desktop-interface" ())

(defconst lispm-desktop-buffer-name " *Desktop*"
  "Name of the desktop's buffer; the leading space keeps it out of buffer lists.")

(defconst lispm-desktop-empty-buffer-name " *Desktop Background*"
  "Name of the buffer showing the desktop's colour without its icons.
A workspace's main area shows it where it holds nothing else.")

(defconst lispm-desktop-tab-name "Desktop"
  "Name of the desktop's own workspace.")

(defun lispm-desktop--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lispm-desktop-xwidget-available-p) (lispm-desktop-xwidget-available-p)))

(defvar lispm-desktop-items
  '((home :label "Home" :icon "hard-disk-32" :command lispm-desktop-finder-home
          :help "Your home folder, in a Finder window")
    (editor :label "Emacs" :icon emacs :command lispm-desktop-editor-activate
            :help "The editor: your projects, agents and shells")
    (browser :label "Browser" :icon "netscape" :command lispm-desktop-browser-show
             :visible lispm-desktop--browser-p :help "The web browser")
    (control-panels :label "Control Panels" :icon "control-panels-32"
                    :command lispm-desktop-settings-open-folder
                    :help "The control panels: appearance, desktop, keyboard, Internet")
    (trash :label "Trash" :icon-function lispm-desktop--trash-icon
           :command lispm-desktop-open-trash :bottom t :help "What was thrown away"))
  "The desktop's icons, from the top: (KEY . PROPERTIES).
:label names an icon, :icon is as `lispm-desktop-platinum-icon-file' takes it
\(the 32-pixel drawing, or Emacs's own scalable icon), or :icon-function
returns it as it changes, as the Trash's does; :command opens it
and :help describes it.  :visible, when given, is a function deciding
whether the icon shows; :bottom puts it in the bottom corner, as Mac OS 9
kept the Trash there.")

(defconst lispm-desktop-icon-size 32
  "Side in pixels of a desktop icon.")

(defconst lispm-desktop-column 80
  "Width in pixels of the column the icons stand in, their names included.")

(defconst lispm-desktop-margin 12
  "Pixels between the icons' column and the desktop's top, right and bottom.")

(defconst lispm-desktop-gap 18
  "Pixels between an icon's name and the next icon.")

;;;; Commands the icons run

(defun lispm-desktop--trash-directory ()
  "Return the directory holding the Trash's files."
  (if (lispm-desktop-macos-p)
      (expand-file-name "~/.Trash")
    (expand-file-name "Trash/files" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))

(defun lispm-desktop-open-trash ()
  "Open the Trash: in the file manager where Emacs may read it.
macOS lets only programs with Full Disk Access read the Trash; without it,
the system's own Trash window opens instead.  Readable, it opens in a
Finder window."
  (interactive)
  (let ((directory (lispm-desktop--trash-directory)))
    (if (ignore-errors (directory-files directory))
        (lispm-desktop-finder-open-directory directory)
      (if (lispm-desktop-macos-p)
          (call-process "open" nil 0 nil directory)
        (user-error "The Trash cannot be read: %s" directory)))))

;;;; The Trash

(defvar lispm-desktop--trash-full nil
  "Non-nil when the Trash was last found holding something.")

(defun lispm-desktop-trash-full-p ()
  "Return non-nil when the Trash holds something.
Where Emacs may read the Trash its items are counted.  macOS lets only
programs with Full Disk Access read it, but it can still be measured: on
APFS a folder's link count is two more than its entries, and the one
hidden entry an emptied Trash keeps, Finder's .DS_Store, can be looked
for by name."
  (let ((directory (lispm-desktop--trash-directory)))
    (if-let* ((names (ignore-errors
                       (directory-files directory nil directory-files-no-dot-files-regexp t))))
        (and (seq-remove (lambda (name) (equal name ".DS_Store")) names) t)
      (when-let* ((attributes (file-attributes directory)))
        (> (- (file-attribute-link-number attributes) 2
              (if (file-exists-p (expand-file-name ".DS_Store" directory)) 1 0))
           0)))))

(defun lispm-desktop--trash-icon ()
  "Return the Trash's icon: full or empty, as it was last found."
  (if lispm-desktop--trash-full "trash-full-32" "trash-32"))

(defun lispm-desktop-check-trash ()
  "Look whether the Trash holds something, and redraw the desktop when that
changed, as Mac OS 9's Trash bulged once something was thrown away."
  (let ((full (lispm-desktop-trash-full-p)))
    (unless (eq full lispm-desktop--trash-full)
      (setq lispm-desktop--trash-full full)
      (when (get-buffer lispm-desktop-buffer-name)
        (lispm-desktop--render)))))

(defvar lispm-desktop--trash-timer nil
  "Timer looking at the Trash every few seconds.")

;;;; Settings

(defun lispm-desktop--setting-changed (&rest _)
  "Redraw the desktop once one of its settings changed."
  (run-at-time 0 nil #'lispm-desktop--paint))

(defcustom lispm-desktop-color nil
  "Colour of the desktop, or nil for the theme's background tinted with its
accent.  `lispm-desktop-picture', when set, lies over it."
  :type '(choice (const :tag "From the theme" nil) color)
  :group 'lispm-desktop)

(defcustom lispm-desktop-picture nil
  "A picture covering the desktop, an image file, or nil for none.
`lispm-desktop-picture-position' says how it covers it."
  :type '(choice (const :tag "None" nil) file)
  :group 'lispm-desktop)

(defcustom lispm-desktop-picture-position 'fill
  "How `lispm-desktop-picture' covers the desktop, as Mac OS 9's Desktop
Pictures placed one: `fill' the whole desktop, cropped to it; `fit' it
whole, the desktop's colour around; `center' at its own size; `tile'
repeated over the desktop."
  :type '(choice (const fill) (const fit) (const center) (const tile))
  :group 'lispm-desktop)

(defcustom lispm-desktop-icon-places nil
  "Where the desktop's icons were put, by key of `lispm-desktop-items':
\((KEY . PLACE) ...).  PLACE gives the pixels between the icon, its name
included, and the desktop's edges nearest it: :left or :right, and :top
or :bottom, so an icon near the right edge stays there as the desktop
changes size.  Icons with no place stand in the column at the right edge.
Dragging an icon sets this."
  :type '(alist :key-type symbol :value-type plist)
  :group 'lispm-desktop)

(defun lispm-desktop-choose-picture (file)
  "Cover the desktop with picture FILE, now and at every later start."
  (interactive (list (read-file-name "Desktop picture: " "~/Pictures/" nil t)))
  (customize-save-variable 'lispm-desktop-picture (and file (abbreviate-file-name file))))

(defun lispm-desktop-remove-picture ()
  "Show the desktop's colour without a picture, now and at every later start."
  (interactive)
  (customize-save-variable 'lispm-desktop-picture nil))

;;;; Drawing
;;
;; The desktop is one SVG the size of its window: its colour, the picture
;; over it, and its icons with their names; a click is matched against
;; where each icon was drawn.  An icon's SVG is nested in it as it is,
;; Emacs's own icon included, its namespaces carried along.

(defface lispm-desktop-label nil
  "Face of a desktop icon's name: the theme's text on its background."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-label-selected nil
  "Face of a selected desktop icon's name: inverted, as Mac OS 9's was."
  :group 'lispm-desktop-platinum)

(defvar-keymap lispm-desktop-mode-map
  :doc "Keymap of the desktop."
  "<down-mouse-1>" #'lispm-desktop-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lispm-desktop-mouse-open
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-desktop-context-menu
  "<mouse-3>" #'ignore
  "<down>" #'lispm-desktop-select-next
  "<right>" #'lispm-desktop-select-next
  "C-n" #'lispm-desktop-select-next
  "C-f" #'lispm-desktop-select-next
  "TAB" #'lispm-desktop-select-next
  "<up>" #'lispm-desktop-select-previous
  "<left>" #'lispm-desktop-select-previous
  "C-p" #'lispm-desktop-select-previous
  "C-b" #'lispm-desktop-select-previous
  "<backtab>" #'lispm-desktop-select-previous
  "RET" #'lispm-desktop-open
  "M-o" #'lispm-desktop-open
  "M-<down>" #'lispm-desktop-open
  "<escape>" #'lispm-desktop-deselect
  "C-g" #'lispm-desktop-deselect
  ;; The icon selected moves with Meta added to the keys moving the
  ;; selection, as dragging it would.
  "C-M-f" #'lispm-desktop-move-icon-right
  "C-M-b" #'lispm-desktop-move-icon-left
  "C-M-n" #'lispm-desktop-move-icon-down
  "C-M-p" #'lispm-desktop-move-icon-up
  "q" #'ignore)

(defun lispm-desktop--color ()
  "Return the desktop's colour: `lispm-desktop-color', or the theme's."
  (or lispm-desktop-color (lispm-desktop-platinum--color :desktop) "#666699"))

(defun lispm-desktop--darken (svg)
  "Return SVG with every colour halved, as Mac OS 9 darkened a selected icon."
  (replace-regexp-in-string
   "#\\([[:xdigit:]]\\{6\\}\\)"
   (lambda (match)
     (apply #'format "#%02X%02X%02X"
            (mapcar (lambda (i) (/ (string-to-number (substring match i (+ i 2)) 16) 2))
                    '(1 3 5))))
   svg t t))

(defvar lispm-desktop--svg-icons (make-hash-table :test #'equal)
  "Icons ready to nest, by (NAME . SELECTED): (ROOT-ATTRIBUTES . CONTENT).")

(defun lispm-desktop--icon-parts (name selected)
  "Return icon NAME, darkened when SELECTED, ready to nest in the desktop.
The value is (ATTRIBUTES . CONTENT): what its root element declares that
its content needs (its view box, namespaces, rendering) and the content."
  (or (gethash (cons name selected) lispm-desktop--svg-icons)
      (puthash
       (cons name selected)
       (let* ((text (with-temp-buffer
                      (insert-file-contents (lispm-desktop-platinum-icon-file name))
                      (buffer-string)))
              (text (if selected (lispm-desktop--darken text) text))
              (start (string-match "<svg\\b[^>]*>" text))
              (root (match-string 0 text))
              (end (string-match "</svg>[[:space:]]*\\'" text))
              (box (if (string-match "viewBox=\"\\([^\"]*\\)\"" root)
                       (match-string 1 root)
                     (format "0 0 %s %s"
                             (if (string-match "width=\"\\([0-9.]+\\)" root) (match-string 1 root) 32)
                             (if (string-match "height=\"\\([0-9.]+\\)" root) (match-string 1 root) 32)))))
         (cons (concat (format "viewBox=\"%s\"" box)
                       (let ((pos 0) (kept ""))
                         (while (string-match "\\(xmlns:[A-Za-z]+\\|shape-rendering\\)=\"[^\"]*\"" root pos)
                           (setq kept (concat kept " " (match-string 0 root))
                                 pos (match-end 0)))
                         kept))
               (substring text (+ start (length root)) end)))
       lispm-desktop--svg-icons)))

(defun lispm-desktop--nest (name selected x y)
  "Return icon NAME, darkened when SELECTED, as SVG standing at X, Y."
  (pcase-let ((`(,attributes . ,content) (lispm-desktop--icon-parts name selected)))
    (format "<svg x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" %s>%s</svg>"
            x y lispm-desktop-icon-size lispm-desktop-icon-size attributes content)))

(defvar-local lispm-desktop--selected nil
  "Key of the selected desktop icon, or nil.")

(defvar-local lispm-desktop--hits nil
  "Where each icon and its name were drawn: (KEY LEFT TOP RIGHT BOTTOM).")

(defun lispm-desktop--items ()
  "Return the desktop's icons that show, see `lispm-desktop-items'."
  (seq-filter (lambda (item)
                (let ((visible (plist-get (cdr item) :visible)))
                  (or (not visible) (funcall visible))))
              lispm-desktop-items))

(defun lispm-desktop--item-icon (spec)
  "Return the icon of the desktop item whose properties are SPEC."
  (if-let* ((function (plist-get spec :icon-function)))
      (funcall function)
    (plist-get spec :icon)))

(defun lispm-desktop--escape (text)
  "Return TEXT escaped for SVG."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun lispm-desktop--font ()
  "Return the names' font as (FAMILY SIZE ASCENT), sizes in pixels."
  (let* ((font (ignore-errors (face-font 'lispm-desktop-platinum-list)))
         (info (and font (ignore-errors (font-info font))))
         (family (face-attribute 'lispm-desktop-platinum-list :family nil t)))
    (list (if (stringp family) family "Geneva")
          (if info (aref info 2) 12)
          (if info (aref info 8) 10))))

(defvar lispm-desktop--picture-sizes (make-hash-table :test #'equal)
  "Sizes of pictures by file and modification time: (WIDTH . HEIGHT).")

(defun lispm-desktop--picture-size (file)
  "Return the size in pixels of picture FILE, as (WIDTH . HEIGHT)."
  (let ((key (cons file (file-attribute-modification-time (file-attributes file)))))
    (or (gethash key lispm-desktop--picture-sizes)
        (puthash key (or (ignore-errors (image-size (create-image file nil nil :scale 1) t))
                         '(256 . 256))
                 lispm-desktop--picture-sizes))))

;; A picture is drawn by the SVG renderer, which reads the whole file each
;; time the desktop is drawn: a photograph many times the desktop's size
;; makes every click slow, and a drag slower still.  So the desktop is
;; drawn from a copy scaled to it, made once in the background by the
;; system's own tool (sips on macOS, ImageMagick elsewhere); until it is
;; made, and where no tool is found, the picture itself is drawn.

(defvar lispm-desktop--scaled nil
  "The picture's copy scaled to the desktop: (FILE TIME WIDTH HEIGHT COPY).")

(defvar lispm-desktop--scaling nil
  "The process scaling the picture, while it runs.")

(defun lispm-desktop--scale-command (file copy width height)
  "Return the command scaling picture FILE to WIDTH by HEIGHT pixels into
COPY, or nil when no tool for it is installed."
  (let ((size (format "%dx%d!" width height)))
    (cond ((executable-find "sips")
           (list "sips" "-s" "format" "jpeg" "-s" "formatOptions" "90"
                 "-z" (number-to-string height) (number-to-string width) file "--out" copy))
          ((executable-find "magick") (list "magick" file "-resize" size "-quality" "90" copy))
          ((executable-find "convert") (list "convert" file "-resize" size "-quality" "90" copy)))))

(defun lispm-desktop--scale-picture (file time width height)
  "Start scaling picture FILE of modification TIME to WIDTH by HEIGHT pixels,
in the background; the desktop is drawn again from the copy once made."
  (let* ((directory (file-name-as-directory (locate-user-emacs-file "lispm-desktop-surface")))
         (copy (expand-file-name (format "picture-%s.jpg"
                                         (md5 (format "%s %S %d %d" file time width height)))
                                 directory))
         (command (lispm-desktop--scale-command file copy width height)))
    (when (and command (not (process-live-p lispm-desktop--scaling)))
      (make-directory directory t)
      (setq lispm-desktop--scaling
            (make-process
             :name "lispm-desktop-picture" :command command :noquery t
             :buffer nil :connection-type 'pipe
             :sentinel
             (lambda (process _event)
               (when (and (eq (process-status process) 'exit)
                          (= (process-exit-status process) 0)
                          (file-readable-p copy))
                 ;; Only the copy now drawn is kept.
                 (dolist (old (directory-files directory t "\`picture-.*\.jpg\'"))
                   (unless (equal old copy)
                     (ignore-errors (delete-file old))))
                 (setq lispm-desktop--scaled (list file time width height copy))
                 (when (get-buffer lispm-desktop-buffer-name)
                   (lispm-desktop--render)))))))))

(defun lispm-desktop--picture-file (width height)
  "Return the picture file to draw a WIDTH by HEIGHT desktop from: a copy of
`lispm-desktop-picture' scaled to it, when made, else the picture itself.
Only a picture filling the desktop or fitted to it is scaled, to the
display's pixels; a copy too small is made again, larger."
  (let* ((file (expand-file-name lispm-desktop-picture))
         (time (file-attribute-modification-time (file-attributes file))))
    (pcase-let* ((`(,picture-width . ,picture-height) (lispm-desktop--picture-size file))
                 (density (if (fboundp 'frame-scale-factor) (frame-scale-factor) 1))
                 (factor (* density (funcall (if (eq lispm-desktop-picture-position 'fit) #'min #'max)
                                             (/ width (float picture-width))
                                             (/ height (float picture-height)))))
                 (wanted-width (ceiling (* picture-width factor)))
                 (wanted-height (ceiling (* picture-height factor))))
      (cond ((or (not (memq lispm-desktop-picture-position '(fill fit)))
                 (>= factor 0.9))
             file)
            ((pcase lispm-desktop--scaled
               (`(,(pred (equal file)) ,(pred (equal time)) ,copy-width ,copy-height ,copy)
                (and (>= copy-width wanted-width) (>= copy-height wanted-height)
                     (file-readable-p copy) copy))))
            (t (lispm-desktop--scale-picture file time wanted-width wanted-height)
               file)))))

(defun lispm-desktop--picture (width height &optional file)
  "Return the SVG laying a picture over WIDTH by HEIGHT, or nil: FILE, by
default `lispm-desktop-picture'.  The picture is named relative to its own
folder, the desktop image's base."
  (when-let* ((file (or file (and lispm-desktop-picture (expand-file-name lispm-desktop-picture))))
              ((file-readable-p file)))
    (let ((href (lispm-desktop--escape (url-hexify-string (file-name-nondirectory file)))))
      (pcase lispm-desktop-picture-position
        ('center
         (pcase-let ((`(,w . ,h) (lispm-desktop--picture-size file)))
           (format "<image href=\"%s\" xlink:href=\"%s\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"/>"
                   href href (/ (- width w) 2) (/ (- height h) 2) w h)))
        ('tile
         (pcase-let ((`(,w . ,h) (lispm-desktop--picture-size file)))
           (concat (format "<defs><pattern id=\"lispm-desktop-tile\" width=\"%d\" height=\"%d\" patternUnits=\"userSpaceOnUse\">" w h)
                   (format "<image href=\"%s\" xlink:href=\"%s\" width=\"%d\" height=\"%d\"/>" href href w h)
                   "</pattern></defs>"
                   (format "<rect width=\"%d\" height=\"%d\" fill=\"url(#lispm-desktop-tile)\"/>" width height))))
        (position
         (format "<image href=\"%s\" xlink:href=\"%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid %s\"/>"
                 href href width height (if (eq position 'fit) "meet" "slice")))))))

(defvar-local lispm-desktop--image nil
  "The desktop's image as last drawn.")

(defun lispm-desktop--cell-size ()
  "Return the size of an icon's cell, its name included: (WIDTH . HEIGHT)."
  (pcase-let ((`(,_ ,size ,_) (lispm-desktop--font)))
    (cons lispm-desktop-column (+ lispm-desktop-icon-size 4 size 4))))

(defun lispm-desktop--place-xy (place width height)
  "Return where PLACE puts a cell on a desktop WIDTH by HEIGHT: (X . Y),
its top left corner, kept whole on the desktop."
  (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
    (cons (max 0 (min (- width cell-width)
                      (if-let* ((right (plist-get place :right)))
                          (- width right cell-width)
                        (or (plist-get place :left) 0))))
          (max 0 (min (- height cell-height)
                      (if-let* ((bottom (plist-get place :bottom)))
                          (- height bottom cell-height)
                        (or (plist-get place :top) 0)))))))

(defun lispm-desktop--xy-place (x y width height)
  "Return the place of a cell at X, Y on a desktop WIDTH by HEIGHT, measured
from the edges nearest it."
  (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
    (append (if (> (+ x (/ cell-width 2)) (/ width 2))
                (list :right (- width x cell-width))
              (list :left x))
            (if (> (+ y (/ cell-height 2)) (/ height 2))
                (list :bottom (- height y cell-height))
              (list :top y)))))

(defun lispm-desktop--column-place (index bottom)
  "Return the place of the INDEXth cell of the column, counted up from the
bottom corner when BOTTOM, else down from the top."
  (pcase-let ((`(,_ . ,cell-height) (lispm-desktop--cell-size)))
    (list :right lispm-desktop-margin
          (if bottom :bottom :top) (+ lispm-desktop-margin (* index (+ cell-height lispm-desktop-gap))))))

(defun lispm-desktop--cell-free-p (xy taken)
  "Return non-nil when a cell at XY, (X . Y), overlaps none of the cells at
TAKEN, a list of (X Y)."
  (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
    (seq-every-p (pcase-lambda (`(,x ,y))
                   (or (>= (abs (- x (car xy))) cell-width)
                       (>= (abs (- y (cdr xy))) cell-height)))
                 taken)))

(defun lispm-desktop--layout (width height)
  "Return where each icon stands on a desktop WIDTH by HEIGHT:
\((ITEM X Y) ...), X and Y its cell's top left corner.  An icon put
somewhere stands there, see `lispm-desktop-icon-places'; the others take the
first cells of the column no put icon covers."
  (let* ((items (lispm-desktop--items))
         (placed (seq-keep (lambda (item)
                             (when-let* ((place (alist-get (car item) lispm-desktop-icon-places)))
                               (let ((xy (lispm-desktop--place-xy place width height)))
                                 (list item (car xy) (cdr xy)))))
                           items))
         (taken (mapcar #'cdr placed))
         (indices (list 0 0)))
    (cl-flet ((free-p (xy) (lispm-desktop--cell-free-p xy taken)))
      (append
       placed
       (seq-keep
        (lambda (item)
          (unless (assq item placed)
            (let* ((bottom (plist-get (cdr item) :bottom))
                   (slot (if bottom 1 0))
                   (xy nil))
              (while (progn
                       (setq xy (lispm-desktop--place-xy
                                 (lispm-desktop--column-place (nth slot indices) bottom)
                                 width height))
                       (cl-incf (nth slot indices))
                       (and (not (free-p xy)) (< (nth slot indices) 64))))
              (push (list (car xy) (cdr xy)) taken)
              (list item (car xy) (cdr xy)))))
        ;; The bottom corner fills from the last icon up.
        (append (seq-remove (lambda (item) (plist-get (cdr item) :bottom)) items)
                (reverse (seq-filter (lambda (item) (plist-get (cdr item) :bottom)) items))))))))

(defun lispm-desktop--draw-icon (spec x y selected label-width family size ascent)
  "Return SVG of the icon SPEC with its name, its cell's top left at X, Y,
darkened and its name inverted when SELECTED.  LABEL-WIDTH, FAMILY, SIZE
and ASCENT measure and set its name."
  (let* ((text (or (lispm-desktop-platinum--color :text) "#000000"))
         (content (or (lispm-desktop-platinum--color :content) "#FFFFFF"))
         (center (+ x (/ lispm-desktop-column 2)))
         (icon-left (- center (/ lispm-desktop-icon-size 2)))
         (label-left (- center (/ label-width 2)))
         (label-top (+ y lispm-desktop-icon-size 4)))
    (concat
     (lispm-desktop--nest (lispm-desktop--item-icon spec) selected icon-left y)
     (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
             label-left label-top label-width (+ size 4) (if selected text content))
     (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" text-anchor=\"middle\" fill=\"%s\">%s</text>"
             center (+ label-top 2 ascent) (lispm-desktop--escape family) size
             (if selected content text) (lispm-desktop--escape (plist-get spec :label))))))

(defun lispm-desktop--svg (width height &optional picture)
  "Return the desktop drawn WIDTH by HEIGHT pixels, as SVG text, PICTURE
over it, by default `lispm-desktop-picture'.  Icons stand where
`lispm-desktop--layout' puts them; where each went is kept in
`lispm-desktop--hits'."
  (pcase-let* ((`(,family ,size ,ascent) (lispm-desktop--font))
               (parts nil))
    (setq lispm-desktop--hits nil)
    (pcase-dolist (`((,key . ,spec) ,x ,y) (lispm-desktop--layout width height))
      (let* ((label-width (+ 8 (string-pixel-width
                                (propertize (plist-get spec :label) 'face 'lispm-desktop-platinum-list))))
             (center (+ x (/ lispm-desktop-column 2))))
        (push (list key
                    (min (- center (/ lispm-desktop-icon-size 2)) (- center (/ label-width 2)))
                    y
                    (max (+ center (/ lispm-desktop-icon-size 2)) (+ center (/ label-width 2)))
                    (+ y lispm-desktop-icon-size 4 size 4))
              lispm-desktop--hits)
        (push (lispm-desktop--draw-icon spec x y (eq key lispm-desktop--selected)
                                        label-width family size ascent)
              parts)))
    (concat
     (format "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
             width height width height)
     (lispm-desktop--background width height picture)
     (apply #'concat (reverse parts))
     "</svg>")))

(defun lispm-desktop--background (width height &optional picture)
  "Return SVG of the desktop's colour, PICTURE over it, WIDTH by HEIGHT."
  (concat (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" width height (lispm-desktop--color))
          (or (lispm-desktop--picture width height picture) "")))

(defun lispm-desktop--copy-svg (key x y width height &optional picture)
  "Return SVG of the dragged copy of icon KEY, its cell's top left at X, Y
of a desktop WIDTH by HEIGHT: the cell's piece of the desktop, PICTURE
included, and the icon over it, translucent, as Mac OS 9 dragged one."
  (pcase-let* ((`(,family ,size ,ascent) (lispm-desktop--font))
               (`(,cell-width . ,cell-height) (lispm-desktop--cell-size))
               (spec (alist-get key lispm-desktop-items))
               (label-width (+ 8 (string-pixel-width
                                  (propertize (plist-get spec :label) 'face 'lispm-desktop-platinum-list))))
               (copy-width (max cell-width label-width))
               (left (- x (/ (- copy-width cell-width) 2))))
    (concat
     (format "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\">"
             copy-width cell-height left y copy-width cell-height)
     (lispm-desktop--background width height picture)
     (format "<g opacity=\"0.5\">%s</g>"
             (lispm-desktop--draw-icon spec x y nil label-width family size ascent))
     "</svg>")))

(defun lispm-desktop--render (&optional window)
  "Draw the desktop for WINDOW, by default the window showing it.
Where images cannot be shown, its icons are listed by name."
  (let ((buffer (lispm-desktop-buffer)))
    (when-let* ((window (or window (get-buffer-window buffer t))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (width (window-body-width window t))
              (height (1- (window-body-height window t))))
          (erase-buffer)
          (if (display-images-p)
              (let* ((picture (and lispm-desktop-picture
                                   (file-readable-p (expand-file-name lispm-desktop-picture))
                                   (lispm-desktop--picture-file width height)))
                     (image (apply #'create-image (lispm-desktop--svg width height picture) 'svg t
                                   :scale 1 :ascent 100
                                   (when picture (list :base-uri picture)))))
                (insert (propertize " " 'display image 'pointer 'arrow))
                ;; Each drawing is a new image as large as the window; the
                ;; last one is let go at once, or a drag would heap them up.
                (when lispm-desktop--image
                  (image-flush lispm-desktop--image t))
                (setq lispm-desktop--image image))
            (dolist (item (lispm-desktop--items))
              (insert (if (eq (car item) lispm-desktop--selected) " ▶ " "   ")
                      (plist-get (cdr item) :label) "\n")))
          (goto-char (point-min))
          (set-window-start window (point-min) t)
          (set-window-point window (point-min)))))))

;;;; Selecting and opening

(defun lispm-desktop--select (key)
  "Select the desktop icon KEY, or none when KEY is nil, and redraw."
  (with-current-buffer (lispm-desktop-buffer)
    (unless (eq key lispm-desktop--selected)
      (setq lispm-desktop--selected key)
      (lispm-desktop--render))))

(defun lispm-desktop--key-at (x y)
  "Return the key of the desktop icon drawn at X, Y, or nil."
  (car (seq-find (pcase-lambda (`(,_ ,left ,top ,right ,bottom))
                   (and (<= left x right) (<= top y bottom)))
                 (buffer-local-value 'lispm-desktop--hits (lispm-desktop-buffer)))))

(defun lispm-desktop--item-at (event)
  "Return the key of the desktop icon mouse EVENT is over, or nil."
  (when-let* ((xy (posn-object-x-y (event-start event))))
    (lispm-desktop--key-at (car xy) (cdr xy))))

(defvar lispm-desktop--last-click nil
  "The last icon clicked without being dragged: (KEY TIME (X . Y)).")

(defconst lispm-desktop-double-click-fuzz 8
  "Pixels a double click's second press may stray from its first.")

(defun lispm-desktop--second-click-p (key press)
  "Return non-nil when a press at PRESS on icon KEY is a double click's
second: the same icon clicked, not dragged, close by and within
`double-click-time'.  Emacs tells a double click itself, but not once a
first click that moved a little was taken for a drag."
  (pcase lispm-desktop--last-click
    (`(,last-key ,time (,x . ,y))
     (and (eq last-key key)
          (numberp double-click-time)
          (< (- (float-time) time) (/ double-click-time 1000.0))
          (<= (+ (abs (- (car press) x)) (abs (- (cdr press) y)))
              lispm-desktop-double-click-fuzz)))))

(defun lispm-desktop-mouse-select (event)
  "Select the desktop icon pressed by EVENT, as Mac OS 9 did on the press,
or none on the desktop between them, and drag the icon while the button
is held, see `lispm-desktop--drag'.  A click on the desktop brings the
desktop in front."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (setq lispm-desktop-finder--had-focus nil)
  (lispm-desktop-apps-set-frontmost 'desktop)
  (let ((key (lispm-desktop--item-at event))
        (press (posn-x-y (event-start event))))
    (lispm-desktop--select key)
    (cond ((null key) (setq lispm-desktop--last-click nil))
          ((lispm-desktop--second-click-p key press)
           ;; The second click of a double click: open, never drag.
           (setq lispm-desktop--last-click nil)
           (lispm-desktop-open))
          ((lispm-desktop--drag key (posn-window (event-start event)) press)
           (setq lispm-desktop--last-click nil))
          (t (setq lispm-desktop--last-click (list key (float-time) press))))))

(defconst lispm-desktop-drag-threshold 6
  "Pixels the mouse moves, held down on an icon, before the icon is dragged:
far enough that a hand clicking, or double clicking, does not drag.")

(defun lispm-desktop--pointer (window)
  "Return the mouse's place in WINDOW's text area, in pixels: (X . Y), or
nil when it is over no frame of WINDOW's.  Over a child frame, such as
the dragged copy, it is carried into WINDOW's frame through the child's
position, so no screen coordinates are involved, which macOS reports
askew in full screen."
  (pcase-let ((`(,frame ,x . ,y) (mouse-pixel-position))
              (screen (window-frame window)))
    (when (and x y)
      (while (and frame (not (eq frame screen)) (frame-parent frame))
        (pcase-let ((`(,left . ,top) (frame-position frame)))
          (setq x (+ x left)
                y (+ y top)
                frame (frame-parent frame))))
      (when (eq frame screen)
        (pcase-let ((`(,left ,top . ,_) (window-inside-pixel-edges window)))
          (cons (- x left) (- y top)))))))

(defun lispm-desktop--cell-at (key window)
  "Return where the cell of icon KEY stands on the desktop in WINDOW."
  (let ((width (window-body-width window t))
        (height (1- (window-body-height window t))))
    (cdr (seq-find (lambda (entry) (eq (car (car entry)) key))
                   (lispm-desktop--layout width height)))))

(defun lispm-desktop--snap (x y width height taken)
  "Return the place on the grid nearest a cell dropped at X, Y of a desktop
WIDTH by HEIGHT, among those no cell at TAKEN, a list of (X Y), covers.
The grid is the column's: cells a column wide and a row apart, counted
from the edges nearest the drop, as the column counts from the right edge
and the Trash from the bottom, so a dropped icon lines up with them."
  (pcase-let* ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size))
               (pitch (+ cell-height lispm-desktop-gap))
               (horizontal (if (> (+ x (/ cell-width 2)) (/ width 2)) :right :left))
               (vertical (if (> (+ y (/ cell-height 2)) (/ height 2)) :bottom :top))
               (columns (max 1 (/ (- width (* 2 lispm-desktop-margin)) cell-width)))
               (rows (max 1 (/ (+ (- height (* 2 lispm-desktop-margin)) lispm-desktop-gap) pitch)))
               (cells nil))
    (dotimes (column columns)
      (dotimes (row rows)
        (let* ((place (list horizontal (+ lispm-desktop-margin (* column cell-width))
                            vertical (+ lispm-desktop-margin (* row pitch))))
               (xy (lispm-desktop--place-xy place width height)))
          (push (list (+ (expt (- (car xy) x) 2) (expt (- (cdr xy) y) 2)) place xy) cells))))
    (setq cells (sort cells (lambda (a b) (< (car a) (car b)))))
    (nth 1 (or (seq-find (lambda (cell) (lispm-desktop--cell-free-p (nth 2 cell) taken)) cells)
               (car cells)))))

(defun lispm-desktop--drop (key x y window)
  "Leave icon KEY dropped with its cell's top left at X, Y of the desktop in
WINDOW, on the nearest free cell of the grid, see `lispm-desktop--snap'.
Every other icon is kept where it stands now, so that only KEY moves,
and the places are saved for the next start."
  (let* ((width (window-body-width window t))
         (height (1- (window-body-height window t)))
         (layout (lispm-desktop--layout width height))
         (others (seq-remove (lambda (entry) (eq (car (car entry)) key)) layout))
         (here (cdr (seq-find (lambda (entry) (eq (car (car entry)) key)) layout)))
         (place (lispm-desktop--snap x y width height (mapcar #'cdr others)))
         (places (mapcar (pcase-lambda (`((,item . ,_) ,item-x ,item-y))
                           (cons item
                                 (if (eq item key)
                                     place
                                   (lispm-desktop--xy-place item-x item-y width height))))
                         layout)))
    ;; Dropped back on its own cell, nothing moved: nothing is saved.
    (unless (equal (lispm-desktop--place-xy place width height)
                   (cons (car here) (cadr here)))
      (customize-save-variable 'lispm-desktop-icon-places places))))

(defun lispm-desktop--move-selected (columns rows)
  "Move the icon selected COLUMNS cells right and ROWS down, as dropping it
there would: onto the nearest free cell of the grid, see
`lispm-desktop--snap'.  An icon in the way is passed, as a drop beside it
would land back on the icon's own cell."
  (let* ((buffer (lispm-desktop-buffer))
         (key (or (buffer-local-value 'lispm-desktop--selected buffer)
                  (user-error "No icon is selected")))
         (window (or (get-buffer-window buffer) (user-error "The desktop is not shown")))
         (here (lispm-desktop--cell-at key window)))
    (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
      (let ((pitch (+ cell-height lispm-desktop-gap))
            (step 1))
        (while (and (<= step 8) (equal (lispm-desktop--cell-at key window) here))
          (lispm-desktop--drop key
                               (+ (car here) (* step columns cell-width))
                               (+ (cadr here) (* step rows pitch))
                               window)
          (setq step (1+ step)))))
    (with-current-buffer buffer
      (lispm-desktop--render))))

(defun lispm-desktop-move-icon-right ()
  "Move the icon selected a cell right."
  (interactive)
  (lispm-desktop--move-selected 1 0))

(defun lispm-desktop-move-icon-left ()
  "Move the icon selected a cell left."
  (interactive)
  (lispm-desktop--move-selected -1 0))

(defun lispm-desktop-move-icon-down ()
  "Move the icon selected a cell down."
  (interactive)
  (lispm-desktop--move-selected 0 1))

(defun lispm-desktop-move-icon-up ()
  "Move the icon selected a cell up."
  (interactive)
  (lispm-desktop--move-selected 0 -1))

(declare-function lispm-desktop-popup--frame "lispm-desktop-popup" (parent))
(declare-function lispm-desktop-popup--dispose "lispm-desktop-popup" (frame))
(declare-function lispm-desktop-popup--size "lispm-desktop-popup" (frame width height))

(defun lispm-desktop--copy-frame (window)
  "Return a hidden child frame for the dragged copy over the desktop in
WINDOW: borderless, taking no focus."
  (let ((frame (lispm-desktop-popup--frame (window-frame window))))
    (modify-frame-parameters frame `((child-frame-border-width . 0)
                                     (background-color . ,(lispm-desktop--color))))
    frame))

(defun lispm-desktop--place-copy (frame x y window)
  "Put the copy's FRAME with its cell at X, Y of the desktop in WINDOW."
  (let ((edges (window-inside-pixel-edges window))
        (cell-width (car (lispm-desktop--cell-size))))
    (set-frame-position frame
                        (+ (nth 0 edges) x (- (/ (- (frame-text-width frame) cell-width) 2)))
                        (+ (nth 1 edges) y))))

(defun lispm-desktop--draw-copy (frame key x y window picture)
  "Draw in FRAME the copy of icon KEY with its cell at X, Y of the desktop in
WINDOW, PICTURE under it, and fit FRAME to it."
  (let* ((width (window-body-width window t))
         (height (1- (window-body-height window t)))
         (image (create-image (lispm-desktop--copy-svg key x y width height picture) 'svg t
                              :scale 1 :ascent 'center
                              :base-uri (or picture default-directory)))
         (size (image-size image t))
         (buffer (window-buffer (frame-root-window frame))))
    (with-current-buffer buffer
      (let ((old (get-text-property (point-min) 'display)))
        (erase-buffer)
        (insert (propertize " " 'display image))
        (when old
          (image-flush old t))))
    (lispm-desktop-popup--size frame (car size) (cdr size))))

(defun lispm-desktop--show-copy (frame key x y window picture)
  "Show in FRAME the copy of icon KEY with its cell at X, Y of the desktop in
WINDOW, PICTURE under it.  The copy moves at once.  On the desktop's
colour it is drawn only the first time, for the colour under it does not
change; over PICTURE its piece of the picture is drawn again once the
mouse has no more moves queued, so drawing never holds the copy back."
  (let ((fresh (not (frame-visible-p frame))))
    (when fresh
      (lispm-desktop--draw-copy frame key x y window picture))
    (lispm-desktop--place-copy frame x y window)
    (if fresh
        (make-frame-visible frame)
      (when (and picture (not (input-pending-p)))
        (lispm-desktop--draw-copy frame key x y window picture)))))

(defun lispm-desktop--drag (key window press)
  "Drag icon KEY of the desktop in WINDOW while the mouse button is held,
from PRESS, where it was pressed in WINDOW's text area: (X . Y).
Return non-nil when the icon was dragged, nil for a click.
Once the mouse has moved a few pixels a translucent copy of the icon
follows it, as Mac OS 9's did, in a small frame of its own so that only
the copy is drawn as it moves; let go, the icon stays there.  The move is
measured from the press, not from where the mouse was once the icon was
drawn selected, so the copy stays where it was grabbed.  Escape gives
up, and a press let go where it was only selects."
  (pcase-let* ((`(,start-x . ,start-y) press)
               (`(,cell-x ,cell-y) (lispm-desktop--cell-at key window))
               (width (window-body-width window t))
               (height (1- (window-body-height window t)))
               (picture (and lispm-desktop-picture
                             (file-readable-p (expand-file-name lispm-desktop-picture))
                             (lispm-desktop--picture-file width height)))
               (frame nil)
               (to nil)
               (event nil))
    (unwind-protect
        (track-mouse
          (setq track-mouse 'dragging)
          (let ((mouse-fine-grained-tracking t))
            (while (progn
                     (setq event (read-event))
                     ;; The copy under the mouse, a frame of its own, brings
                     ;; focus and frame events: they do not end the drag.
                     (or (null event)
                         (mouse-movement-p event)
                         (memq (event-basic-type event)
                               '(switch-frame focus-in focus-out select-window help-echo))))
              (when (mouse-movement-p event)
                ;; Only the latest position counts.  Input can be pending
                ;; with nothing to read yet: a read that times out gives
                ;; nil, which ends the catching up and is never put back.
                (while (and (input-pending-p)
                            (let ((next (read-event nil nil 0)))
                              (cond ((null next) nil)
                                    ((mouse-movement-p next) t)
                                    (t (setq unread-command-events
                                             (append unread-command-events (list next)))
                                       nil)))))
                (pcase-let ((`(,x . ,y) (or (lispm-desktop--pointer window)
                                            (cons start-x start-y))))
                  (when (or to (> (+ (expt (- x start-x) 2) (expt (- y start-y) 2))
                                  (expt lispm-desktop-drag-threshold 2)))
                    (setq to (lispm-desktop--place-xy
                              (lispm-desktop--xy-place (+ cell-x (- x start-x))
                                                       (+ cell-y (- y start-y))
                                                       width height)
                              width height))
                    (unless frame
                      (setq frame (lispm-desktop--copy-frame window)))
                    (lispm-desktop--show-copy frame key (car to) (cdr to) window picture)))))))
      (when frame
        (lispm-desktop-popup--dispose frame)))
    (cond ((and to (memq (event-basic-type event) '(mouse-1 drag-mouse-1)))
           (lispm-desktop--drop key (car to) (cdr to) window)
           (lispm-desktop--render window))
          ((not (memq (event-basic-type event) '(mouse-1 drag-mouse-1 escape)))
           (setq unread-command-events (list event))))
    ;; Dragged or not: a press let go where it was is a click.
    (and to t)))

(defun lispm-desktop-arrange-icons ()
  "Arrange the desktop's icons as they stood at first: down the column at
the right edge, the Trash in the bottom corner.  However they were
dragged about, this puts them back."
  (interactive)
  (customize-save-variable 'lispm-desktop-icon-places nil)
  (lispm-desktop--render))

(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup--frame-xy "lispm-desktop-popup" (position))

(defun lispm-desktop--arrange-items ()
  "Return the menu items arranging the desktop's icons."
  `((:label "Arrange Icons" :action lispm-desktop-arrange-icons
            :enabled ,(and lispm-desktop-icon-places t))))

(defun lispm-desktop--menu (x y &optional align)
  "Pop up the desktop's menu at X, Y of the frame.
ALIGN is as `lispm-desktop-popup-menu' takes it.  The menu has Open for
the icon selected, then Arrange Icons and the desktop picture, as Mac OS
9's desktop offered them."
  (let ((selected (buffer-local-value 'lispm-desktop--selected (lispm-desktop-buffer))))
    (lispm-desktop-popup-menu
     `(,@(when selected
           '((:label "Open" :action lispm-desktop-open :key "M-o")
             separator))
       ,@(lispm-desktop--arrange-items)
       separator
       (:label "Desktop Picture…" :action lispm-desktop-choose-picture)
       (:label "No Desktop Picture" :action lispm-desktop-remove-picture
               :enabled ,(and lispm-desktop-picture t)))
     x y align)))

(defun lispm-desktop-context-menu (event)
  "Pop up the desktop's menu where EVENT right-clicked, of the icon there."
  (interactive "e")
  (lispm-desktop-mouse-select event)
  (let ((xy (lispm-desktop-popup--frame-xy event)))
    (lispm-desktop--menu (car xy) (cdr xy))))

(defun lispm-desktop--keyboard-menu ()
  "Pop up the desktop's menu from the keyboard, under the icon selected,
see `lispm-desktop-context-menu-function'.  An icon in the right half of the
desktop has the menu end at its right, so it stays on the desktop."
  (let* ((buffer (lispm-desktop-buffer))
         (window (get-buffer-window buffer))
         (body (window-body-pixel-edges window))
         (hit (assq (buffer-local-value 'lispm-desktop--selected buffer)
                    (buffer-local-value 'lispm-desktop--hits buffer))))
    (pcase-let ((`(,_ ,left ,_ ,right ,bottom) (or hit (list nil 40 40 40 40))))
      (if (> left (/ (window-body-width window t) 2))
          (lispm-desktop--menu (+ (car body) right) (+ (cadr body) bottom) 'right)
        (lispm-desktop--menu (+ (car body) left) (+ (cadr body) bottom))))))

(defun lispm-desktop-mouse-open (event)
  "Open the desktop icon double-clicked by EVENT."
  (interactive "e")
  (lispm-desktop-mouse-select event)
  (when (lispm-desktop--item-at event)
    (lispm-desktop-open)))

(defun lispm-desktop-deselect ()
  "Select no desktop icon: a click on the desktop itself, which brings the
desktop in front."
  (interactive)
  (setq lispm-desktop-finder--had-focus nil)
  (lispm-desktop-apps-set-frontmost 'desktop)
  (lispm-desktop--select nil))

(defun lispm-desktop-open ()
  "Open the selected desktop icon."
  (interactive)
  (when-let* ((key (buffer-local-value 'lispm-desktop--selected (lispm-desktop-buffer)))
              (command (plist-get (alist-get key lispm-desktop-items) :command)))
    (lispm-desktop--select nil)
    (call-interactively command)))

(defun lispm-desktop--step (direction)
  "Select the icon DIRECTION (1 or -1) from the selected one, wrapping around."
  (let* ((keys (mapcar #'car (lispm-desktop--items)))
         (index (seq-position keys lispm-desktop--selected)))
    (when keys
      (lispm-desktop--select
       (nth (mod (if index (+ index direction) (if (> direction 0) 0 -1)) (length keys))
            keys)))))

(defun lispm-desktop-select-next ()
  "Select the next desktop icon down."
  (interactive)
  (lispm-desktop--step 1))

(defun lispm-desktop-select-previous ()
  "Select the next desktop icon up."
  (interactive)
  (lispm-desktop--step -1))

;;;; Painting

(defvar-local lispm-desktop--background nil
  "Face remapping cookies colouring a desktop buffer.")

(defun lispm-desktop--paint-buffer (buffer)
  "Colour BUFFER as the desktop, its fringes included."
  (with-current-buffer buffer
    (mapc #'face-remap-remove-relative lispm-desktop--background)
    (setq lispm-desktop--background
          (list (face-remap-add-relative 'default :background (lispm-desktop--color))
                (face-remap-add-relative 'fringe :background (lispm-desktop--color))))))

(defun lispm-desktop--paint ()
  "Colour the desktop, its background and its names from the palette."
  (when lispm-desktop-platinum--palette
    (let ((content (lispm-desktop-platinum--color :content))
          (text (lispm-desktop-platinum--color :text)))
      (set-face-attribute 'lispm-desktop-label nil
                          :inherit 'lispm-desktop-platinum-list :background content :foreground text)
      (set-face-attribute 'lispm-desktop-label-selected nil
                          :inherit 'lispm-desktop-platinum-list :background text :foreground content)
      (dolist (name (list lispm-desktop-buffer-name lispm-desktop-empty-buffer-name))
        (when-let* ((buffer (get-buffer name)))
          (lispm-desktop--paint-buffer buffer)))
      (when (get-buffer lispm-desktop-buffer-name)
        (lispm-desktop--render)))))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(defun lispm-desktop--bare ()
  "Strip the current buffer's windows to the desktop: no title bar, no
bottom edge, no cursor, fringes or line numbers, and its one line never
wrapped, which would hold a column for the wrapping mark.  Visual line
mode is turned off first: turned off, it restores the lines it found."
  (visual-line-mode -1)
  (setq-local mode-line-format nil
              header-line-format nil
              tab-line-format nil
              cursor-type nil
              truncate-lines t
              ;; Fringes on both sides, however thin, keep Emacs from holding
              ;; the last column of the text area for its truncation mark,
              ;; which a picture as wide as the window would stop short of.
              left-fringe-width 1
              right-fringe-width 1
              show-trailing-whitespace nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil))
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(define-derived-mode lispm-desktop-mode special-mode "Desktop"
  "Major mode of the desktop, the place behind every window.
\\{lispm-desktop-mode-map}"
  (lispm-desktop--bare)
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-desktop--render))
              lispm-desktop-context-menu-function #'lispm-desktop--keyboard-menu)
  (add-hook 'window-size-change-functions #'lispm-desktop--render nil t)
  (add-hook 'window-buffer-change-functions #'lispm-desktop--render nil t))

(defvar-keymap lispm-desktop-empty-mode-map
  :doc "Keymap of the desktop's background: nothing to click."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'ignore
  "q" #'ignore)

(define-derived-mode lispm-desktop-empty-mode special-mode "Desktop"
  "Major mode of the desktop's colour without its icons, the background of
a workspace whose main area holds nothing else."
  (lispm-desktop--bare))

(defun lispm-desktop-buffer ()
  "Return the desktop's buffer, made when missing."
  (or (get-buffer lispm-desktop-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-buffer-name)
        (lispm-desktop-mode)
        (setq lispm-desktop--background nil)
        (lispm-desktop--paint)
        (current-buffer))))

(defun lispm-desktop-empty-buffer ()
  "Return the desktop's background buffer, made when missing."
  (or (get-buffer lispm-desktop-empty-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-empty-buffer-name)
        (lispm-desktop-empty-mode)
        (when lispm-desktop-platinum--palette
          (lispm-desktop--paint-buffer (current-buffer)))
        (current-buffer))))

;;;; The desktop's workspace

(defun lispm-desktop-show-in-new-tab (buffer)
  "Show BUFFER in the window of a workspace being made, in place; return nil.
This is how a `tab-bar-new-tab-choice' shows a new workspace's first
buffer.  A buffer returned to tab-bar instead is shown by
`switch-to-buffer', which obeys `display-buffer-alist' while the old
workspace's window is still the one selected: from a session's window,
an agent's or a terminal's, the buffer would be put beside the session,
and the session carried into the new workspace; from the desktop, a
document would be sent to the editor."
  (let ((window (selected-window)))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (set-window-start window 1))
  nil)

(defun lispm-desktop--new-tab-empty ()
  "Show the desktop's colour in a workspace being made; return nil.
A `tab-bar-new-tab-choice', see `lispm-desktop-show-in-new-tab'."
  (lispm-desktop-show-in-new-tab (lispm-desktop-empty-buffer)))

(defun lispm-desktop--new-tab-desktop ()
  "Show the desktop in its workspace being made; return nil.
A `tab-bar-new-tab-choice', see `lispm-desktop-show-in-new-tab'."
  (lispm-desktop-show-in-new-tab (lispm-desktop-buffer)))

(defun lispm-desktop--tab-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, is in the
desktop's workspace."
  (equal (alist-get 'name (tab-bar--current-tab nil frame)) lispm-desktop-tab-name))

(defvar lispm-desktop-empty-function nil
  "Function filling WINDOW, its one argument, of a workspace whose main area
holds nothing; it returns nil to leave WINDOW to the desktop's colour.
The editor shows Emacs's own startup screen there.")

(defun lispm-desktop-show (&optional window)
  "Show the desktop in WINDOW, by default the selected window.
In the desktop's workspace it shows with its icons; in any other, where
the main area holds nothing else, what `lispm-desktop-empty-function' shows,
or the desktop's bare colour."
  (let ((window (or window (selected-window))))
    (set-window-dedicated-p window nil)
    (cond ((lispm-desktop--tab-p (window-frame window))
           (set-window-buffer window (lispm-desktop-buffer))
           (lispm-desktop--render window))
          ((and lispm-desktop-empty-function (funcall lispm-desktop-empty-function window)))
          (t (set-window-buffer window (lispm-desktop-empty-buffer))))))

(defun lispm-desktop-p (&optional window)
  "Return non-nil when WINDOW, by default the selected one, shows the desktop."
  (member (buffer-name (window-buffer window))
          (list lispm-desktop-buffer-name lispm-desktop-empty-buffer-name)))

(defun lispm-desktop--select-tab (&optional frame)
  "Select the desktop's workspace in FRAME, made when missing: the desktop
alone, filling the frame."
  (with-selected-frame (or frame (selected-frame))
    (if-let* ((index (tab-bar--tab-index-by-name lispm-desktop-tab-name)))
        (unless (lispm-desktop--tab-p)
          (tab-bar-select-tab (1+ index)))
      (let ((tab-bar-new-tab-choice #'lispm-desktop--new-tab-desktop))
        (tab-bar-new-tab))
      (tab-bar-rename-tab lispm-desktop-tab-name))
    (let ((window (car (lispm-desktop-main-area--windows))))
      (when (or (cdr (window-list nil 'no-mini)) (not (lispm-desktop-p window)))
        (let ((ignore-window-parameters t))
          (delete-other-windows window)))
      (lispm-desktop-show window))))

(defvar lispm-desktop-activate-hook nil
  "Hook run when the desktop comes in front, as windows left over it show.")

(defun lispm-desktop-activate ()
  "Bring the desktop in front: its workspace, the floating editor left over it."
  (interactive)
  (let ((screen (lispm-desktop-apps-screen)))
    (unless (eq (selected-frame) screen)
      (select-frame-set-input-focus screen))
    (lispm-desktop--select-tab screen))
  (lispm-desktop-apps-set-frontmost 'desktop)
  (run-hooks 'lispm-desktop-activate-hook))

(defun lispm-desktop--on-new-workspace (&rest _)
  "Show the desktop's colour in a new workspace that opened onto *scratch*."
  (let ((window (selected-window)))
    (when (and (equal (buffer-name (window-buffer window)) "*scratch*")
               (not (window-parameter window 'window-side)))
      (lispm-desktop-show window))))

;;;; The desktop's menus

(defun lispm-desktop--file-items ()
  "Return the items of the desktop's File menu.  Open and Close Window act
on the Finder window when it had the keyboard last, else on the desktop."
  (let* ((finder (and (fboundp 'lispm-desktop-finder-shown-p) (lispm-desktop-finder-shown-p)))
         (in-finder (and finder (bound-and-true-p lispm-desktop-finder--had-focus)))
         (selected (buffer-local-value 'lispm-desktop--selected (lispm-desktop-buffer))))
    `((:label "New Folder…" :action lispm-desktop-finder-new-folder
              :enabled ,(and finder (lispm-desktop-finder-real-p) t))
      (:label "Open" :action ,(if in-finder 'lispm-desktop-finder-open 'lispm-desktop-open)
              :enabled ,(and (or in-finder selected) t))
      (:label "Close Window" :action lispm-desktop-finder-close :enabled ,(and finder t))
      separator
      (:label "Move to Trash" :action lispm-desktop-finder-move-to-trash :key "M-DEL"
              :enabled ,(and in-finder (lispm-desktop-finder-real-p)
                             (with-current-buffer (lispm-desktop-finder--buffer) lispm-desktop-finder--selected)
                             t))
      separator
      ,@(mapcar (lambda (item)
                  (list :label (plist-get (cdr item) :label)
                        :action (plist-get (cdr item) :command)))
                (lispm-desktop--items)))))

(defun lispm-desktop--special-items ()
  "Return the items of the desktop's Special menu, Mac OS 9's last ones."
  `(,@(when (fboundp 'restart-emacs)
        '((:label "Restart" :action restart-emacs)))
    (:label "Shut Down" :action save-buffers-kill-emacs)))

(defun lispm-desktop--view-items ()
  "Return the items of the desktop's View menu."
  (lispm-desktop--arrange-items))

(defun lispm-desktop-menus ()
  "Return the desktop's menus, for the menu bar while it is in front."
  '((lispm-desktop-menu-file "File" lispm-desktop--file-items)
    (lispm-desktop-menu-view "View" lispm-desktop--view-items)
    (lispm-desktop-menu-go "Go" lispm-desktop-finder--go-items)
    (lispm-desktop-menu-special "Special" lispm-desktop--special-items)))

(defun lispm-desktop-setup-desktop ()
  "Put the desktop behind every window, and start Emacs on it.
Its settings take effect when changed however they are, a `setq' in
personal.el included, and the Trash is looked at every few seconds."
  (setq initial-buffer-choice #'lispm-desktop-empty-buffer
        tab-bar-new-tab-choice #'lispm-desktop--new-tab-empty)
  (dolist (setting '(lispm-desktop-color lispm-desktop-picture lispm-desktop-picture-position
                     lispm-desktop-icon-places))
    (add-variable-watcher setting #'lispm-desktop--setting-changed))
  (lispm-desktop-check-trash)
  (unless lispm-desktop--trash-timer
    (setq lispm-desktop--trash-timer (run-with-timer 5 5 #'lispm-desktop-check-trash)))
  (add-hook 'tab-bar-tab-post-open-functions #'lispm-desktop--on-new-workspace)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop--paint)) 90)
  (add-hook 'window-setup-hook #'lispm-desktop-activate 95))

(provide 'lispm-desktop-surface)
;;; lispm-desktop-surface.el ends here
