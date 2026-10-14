;;; lme-desktop.el --- The desktop behind every window  -*- lexical-binding: t; -*-

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

;; Mac OS 9's desktop lay behind every window, and its icons opened the
;; disks, the applications and the Trash.  Here the desktop is a workspace
;; of its own, the one Emacs starts in, and one of the applications (see
;; lme-apps.el): in front, its menus are File and Special.  Where another
;; workspace's main area holds nothing, the desktop's colour shows there,
;; without its icons, as the desktop seen through.
;;
;; Its icons stand down the right edge, where Mac OS 9 put them: the home
;; disk at the top, Emacs and the browser under it, the Trash in the
;; bottom corner.
;; A click selects an icon, darkening it and inverting its name, and a
;; double click opens it; the arrow keys select, Return or Command-O opens.
;;
;;   window --size--> layout: icons from the top, the Trash at the bottom
;;   icon --selected--> its SVG with every colour halved

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'lme-core)
(require 'lme-platinum)
(require 'lme-apps)

(declare-function lme-browser-show "lme-browser" ())
(declare-function lme-xwidget-available-p "lme-browser" ())
(declare-function lme-editor-activate "lme-editor" ())
(declare-function lme-finder-home "lme-finder" ())
(declare-function lme-finder-open-directory "lme-finder" (directory))
(declare-function lme-finder-shown-p "lme-finder" ())
(declare-function lme-finder-open "lme-finder" ())
(declare-function lme-finder-close "lme-finder" ())
(declare-function lme-finder--buffer "lme-finder" ())
(defvar lme-finder--selected)
(defvar lme-finder--had-focus)
(declare-function lme-main-area--windows "lme-interface" ())

(defconst lme-desktop-buffer-name " *Desktop*"
  "Name of the desktop's buffer; the leading space keeps it out of buffer lists.")

(defconst lme-desktop-empty-buffer-name " *Desktop Background*"
  "Name of the buffer showing the desktop's colour without its icons.
A workspace's main area shows it where it holds nothing else.")

(defconst lme-desktop-tab-name "Desktop"
  "Name of the desktop's own workspace.")

(defun lme-desktop--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lme-xwidget-available-p) (lme-xwidget-available-p)))

(defvar lme-desktop-items
  '((home :label "Home" :icon "hard-disk-32" :command lme-finder-home
          :help "Your home folder, in a Finder window")
    (editor :label "Emacs" :icon emacs :command lme-editor-activate
            :help "The editor: your projects, agents and shells")
    (browser :label "Browser" :icon "netscape" :command lme-browser-show
             :visible lme-desktop--browser-p :help "The web browser")
    (trash :label "Trash" :icon-function lme-desktop--trash-icon
           :command lme-desktop-open-trash :bottom t :help "What was thrown away"))
  "The desktop's icons, from the top: (KEY . PROPERTIES).
:label names an icon, :icon is as `lme-platinum-icon-file' takes it
\(the 32-pixel drawing, or Emacs's own scalable icon), or :icon-function
returns it as it changes, as the Trash's does; :command opens it
and :help describes it.  :visible, when given, is a function deciding
whether the icon shows; :bottom puts it in the bottom corner, as Mac OS 9
kept the Trash there.")

(defconst lme-desktop-icon-size 32
  "Side in pixels of a desktop icon.")

(defconst lme-desktop-column 80
  "Width in pixels of the column the icons stand in, their names included.")

(defconst lme-desktop-margin 12
  "Pixels between the icons' column and the desktop's top, right and bottom.")

(defconst lme-desktop-gap 18
  "Pixels between an icon's name and the next icon.")

;;;; Commands the icons run

(defun lme-desktop--trash-directory ()
  "Return the directory holding the Trash's files."
  (if (lme-macos-p)
      (expand-file-name "~/.Trash")
    (expand-file-name "Trash/files" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))

(defun lme-desktop-open-trash ()
  "Open the Trash: in the file manager where Emacs may read it.
macOS lets only programs with Full Disk Access read the Trash; without it,
the system's own Trash window opens instead.  Readable, it opens in a
Finder window."
  (interactive)
  (let ((directory (lme-desktop--trash-directory)))
    (if (ignore-errors (directory-files directory))
        (lme-finder-open-directory directory)
      (if (lme-macos-p)
          (call-process "open" nil 0 nil directory)
        (user-error "The Trash cannot be read: %s" directory)))))

;;;; The Trash

(defvar lme-desktop--trash-full nil
  "Non-nil when the Trash was last found holding something.")

(defun lme-desktop-trash-full-p ()
  "Return non-nil when the Trash holds something.
Where Emacs may read the Trash its items are counted.  macOS lets only
programs with Full Disk Access read it, but it can still be measured: on
APFS a folder's link count is two more than its entries, and the one
hidden entry an emptied Trash keeps, Finder's .DS_Store, can be looked
for by name."
  (let ((directory (lme-desktop--trash-directory)))
    (if-let* ((names (ignore-errors
                       (directory-files directory nil directory-files-no-dot-files-regexp t))))
        (and (seq-remove (lambda (name) (equal name ".DS_Store")) names) t)
      (when-let* ((attributes (file-attributes directory)))
        (> (- (file-attribute-link-number attributes) 2
              (if (file-exists-p (expand-file-name ".DS_Store" directory)) 1 0))
           0)))))

(defun lme-desktop--trash-icon ()
  "Return the Trash's icon: full or empty, as it was last found."
  (if lme-desktop--trash-full "trash-full-32" "trash-32"))

(defun lme-desktop-check-trash ()
  "Look whether the Trash holds something, and redraw the desktop when that
changed, as Mac OS 9's Trash bulged once something was thrown away."
  (let ((full (lme-desktop-trash-full-p)))
    (unless (eq full lme-desktop--trash-full)
      (setq lme-desktop--trash-full full)
      (when (get-buffer lme-desktop-buffer-name)
        (lme-desktop--render)))))

(defvar lme-desktop--trash-timer nil
  "Timer looking at the Trash every few seconds.")

;;;; Settings

(defun lme-desktop--setting-changed (&rest _)
  "Redraw the desktop once one of its settings changed."
  (run-at-time 0 nil #'lme-desktop--paint))

(defcustom lme-desktop-color nil
  "Colour of the desktop, or nil for the theme's background tinted with its
accent.  `lme-desktop-picture', when set, lies over it."
  :type '(choice (const :tag "From the theme" nil) color)
  :group 'lispm-emacs)

(defcustom lme-desktop-picture nil
  "A picture covering the desktop, an image file, or nil for none.
`lme-desktop-picture-position' says how it covers it."
  :type '(choice (const :tag "None" nil) file)
  :group 'lispm-emacs)

(defcustom lme-desktop-picture-position 'fill
  "How `lme-desktop-picture' covers the desktop, as Mac OS 9's Desktop
Pictures placed one: `fill' the whole desktop, cropped to it; `fit' it
whole, the desktop's colour around; `center' at its own size; `tile'
repeated over the desktop."
  :type '(choice (const fill) (const fit) (const center) (const tile))
  :group 'lispm-emacs)

(defun lme-desktop-choose-picture (file)
  "Cover the desktop with picture FILE, now and at every later start."
  (interactive (list (read-file-name "Desktop picture: " "~/Pictures/" nil t)))
  (customize-save-variable 'lme-desktop-picture (and file (abbreviate-file-name file))))

(defun lme-desktop-remove-picture ()
  "Show the desktop's colour without a picture, now and at every later start."
  (interactive)
  (customize-save-variable 'lme-desktop-picture nil))

;;;; Drawing
;;
;; The desktop is one SVG the size of its window: its colour, the picture
;; over it, and its icons with their names; a click is matched against
;; where each icon was drawn.  An icon's SVG is nested in it as it is,
;; Emacs's own icon included, its namespaces carried along.

(defface lme-desktop-label nil
  "Face of a desktop icon's name: the theme's text on its background."
  :group 'lme-platinum)

(defface lme-desktop-label-selected nil
  "Face of a selected desktop icon's name: inverted, as Mac OS 9's was."
  :group 'lme-platinum)

(defvar-keymap lme-desktop-mode-map
  :doc "Keymap of the desktop."
  "<down-mouse-1>" #'lme-desktop-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lme-desktop-mouse-open
  "<drag-mouse-1>" #'ignore
  "<down>" #'lme-desktop-select-next
  "<right>" #'lme-desktop-select-next
  "TAB" #'lme-desktop-select-next
  "<up>" #'lme-desktop-select-previous
  "<left>" #'lme-desktop-select-previous
  "<backtab>" #'lme-desktop-select-previous
  "RET" #'lme-desktop-open
  "M-o" #'lme-desktop-open
  "M-<down>" #'lme-desktop-open
  "<escape>" #'lme-desktop-deselect
  "q" #'ignore)

(defun lme-desktop--color ()
  "Return the desktop's colour: `lme-desktop-color', or the theme's."
  (or lme-desktop-color (lme-platinum--color :desktop) "#666699"))

(defun lme-desktop--darken (svg)
  "Return SVG with every colour halved, as Mac OS 9 darkened a selected icon."
  (replace-regexp-in-string
   "#\\([[:xdigit:]]\\{6\\}\\)"
   (lambda (match)
     (apply #'format "#%02X%02X%02X"
            (mapcar (lambda (i) (/ (string-to-number (substring match i (+ i 2)) 16) 2))
                    '(1 3 5))))
   svg t t))

(defvar lme-desktop--svg-icons (make-hash-table :test #'equal)
  "Icons ready to nest, by (NAME . SELECTED): (ROOT-ATTRIBUTES . CONTENT).")

(defun lme-desktop--icon-parts (name selected)
  "Return icon NAME, darkened when SELECTED, ready to nest in the desktop.
The value is (ATTRIBUTES . CONTENT): what its root element declares that
its content needs (its view box, namespaces, rendering) and the content."
  (or (gethash (cons name selected) lme-desktop--svg-icons)
      (puthash
       (cons name selected)
       (let* ((text (with-temp-buffer
                      (insert-file-contents (lme-platinum-icon-file name))
                      (buffer-string)))
              (text (if selected (lme-desktop--darken text) text))
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
       lme-desktop--svg-icons)))

(defun lme-desktop--nest (name selected x y)
  "Return icon NAME, darkened when SELECTED, as SVG standing at X, Y."
  (pcase-let ((`(,attributes . ,content) (lme-desktop--icon-parts name selected)))
    (format "<svg x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" %s>%s</svg>"
            x y lme-desktop-icon-size lme-desktop-icon-size attributes content)))

(defvar-local lme-desktop--selected nil
  "Key of the selected desktop icon, or nil.")

(defvar-local lme-desktop--hits nil
  "Where each icon and its name were drawn: (KEY LEFT TOP RIGHT BOTTOM).")

(defun lme-desktop--items ()
  "Return the desktop's icons that show, see `lme-desktop-items'."
  (seq-filter (lambda (item)
                (let ((visible (plist-get (cdr item) :visible)))
                  (or (not visible) (funcall visible))))
              lme-desktop-items))

(defun lme-desktop--item-icon (spec)
  "Return the icon of the desktop item whose properties are SPEC."
  (if-let* ((function (plist-get spec :icon-function)))
      (funcall function)
    (plist-get spec :icon)))

(defun lme-desktop--escape (text)
  "Return TEXT escaped for SVG."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun lme-desktop--font ()
  "Return the names' font as (FAMILY SIZE ASCENT), sizes in pixels."
  (let* ((font (ignore-errors (face-font 'lme-platinum-list)))
         (info (and font (ignore-errors (font-info font))))
         (family (face-attribute 'lme-platinum-list :family nil t)))
    (list (if (stringp family) family "Geneva")
          (if info (aref info 2) 12)
          (if info (aref info 8) 10))))

(defun lme-desktop--picture-size (file)
  "Return the size in pixels of picture FILE, as (WIDTH . HEIGHT)."
  (or (ignore-errors (image-size (create-image file nil nil :scale 1) t))
      '(256 . 256)))

(defun lme-desktop--picture (width height)
  "Return the SVG laying `lme-desktop-picture' over WIDTH by HEIGHT, or nil.
The picture is named relative to its own folder, the desktop image's base."
  (when-let* ((file (and lme-desktop-picture (expand-file-name lme-desktop-picture)))
              ((file-readable-p file)))
    (let ((href (lme-desktop--escape (url-hexify-string (file-name-nondirectory file)))))
      (pcase lme-desktop-picture-position
        ('center
         (pcase-let ((`(,w . ,h) (lme-desktop--picture-size file)))
           (format "<image href=\"%s\" xlink:href=\"%s\" x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"/>"
                   href href (/ (- width w) 2) (/ (- height h) 2) w h)))
        ('tile
         (pcase-let ((`(,w . ,h) (lme-desktop--picture-size file)))
           (concat (format "<defs><pattern id=\"lme-tile\" width=\"%d\" height=\"%d\" patternUnits=\"userSpaceOnUse\">" w h)
                   (format "<image href=\"%s\" xlink:href=\"%s\" width=\"%d\" height=\"%d\"/>" href href w h)
                   "</pattern></defs>"
                   (format "<rect width=\"%d\" height=\"%d\" fill=\"url(#lme-tile)\"/>" width height))))
        (position
         (format "<image href=\"%s\" xlink:href=\"%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMidYMid %s\"/>"
                 href href width height (if (eq position 'fit) "meet" "slice")))))))

(defun lme-desktop--svg (width height)
  "Return the desktop drawn WIDTH by HEIGHT pixels, as SVG text.
Icons stand down the right edge from the top, those marked :bottom up from
the bottom corner; where each went is kept in `lme-desktop--hits'."
  (pcase-let* ((`(,family ,size ,ascent) (lme-desktop--font))
               (text (or (lme-platinum--color :text) "#000000"))
               (content (or (lme-platinum--color :content) "#FFFFFF"))
               (items (lme-desktop--items))
               (label-height (+ size 4))
               (cell (+ lme-desktop-icon-size 4 label-height))
               (center (- width lme-desktop-margin (/ lme-desktop-column 2)))
               (places nil)
               (parts nil))
    (let ((y lme-desktop-margin))
      (dolist (item (seq-remove (lambda (item) (plist-get (cdr item) :bottom)) items))
        (push (cons item y) places)
        (setq y (+ y cell lme-desktop-gap))))
    (let ((y (- height lme-desktop-margin cell)))
      (dolist (item (reverse (seq-filter (lambda (item) (plist-get (cdr item) :bottom)) items)))
        (push (cons item y) places)
        (setq y (- y cell lme-desktop-gap))))
    (setq lme-desktop--hits nil)
    (pcase-dolist (`((,key . ,spec) . ,y) places)
      (let* ((selected (eq key lme-desktop--selected))
             (label (plist-get spec :label))
             (label-width (+ 8 (string-pixel-width (propertize label 'face 'lme-platinum-list))))
             (icon-left (- center (/ lme-desktop-icon-size 2)))
             (label-left (- center (/ label-width 2)))
             (label-top (+ y lme-desktop-icon-size 4)))
        (push (list key (min icon-left label-left) y
                    (max (+ icon-left lme-desktop-icon-size) (+ label-left label-width))
                    (+ label-top label-height))
              lme-desktop--hits)
        (push (concat
               (lme-desktop--nest (lme-desktop--item-icon spec) selected icon-left y)
               (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                       label-left label-top label-width label-height (if selected text content))
               (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" text-anchor=\"middle\" fill=\"%s\">%s</text>"
                       center (+ label-top 2 ascent) (lme-desktop--escape family) size
                       (if selected content text) (lme-desktop--escape label)))
              parts)))
    (concat
     (format "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
             width height width height)
     (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" width height (lme-desktop--color))
     (or (lme-desktop--picture width height) "")
     (apply #'concat parts)
     "</svg>")))

(defun lme-desktop--render (&optional window)
  "Draw the desktop for WINDOW, by default the window showing it.
Where images cannot be shown, its icons are listed by name."
  (let ((buffer (lme-desktop-buffer)))
    (when-let* ((window (or window (get-buffer-window buffer t))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (width (window-body-width window t))
              (height (1- (window-body-height window t))))
          (erase-buffer)
          (if (display-images-p)
              (insert (propertize
                       " " 'display
                       (apply #'create-image (lme-desktop--svg width height) 'svg t
                              :scale 1 :ascent 100
                              (when lme-desktop-picture
                                (list :base-uri (expand-file-name lme-desktop-picture))))
                       'pointer 'arrow))
            (dolist (item (lme-desktop--items))
              (insert (if (eq (car item) lme-desktop--selected) " ▶ " "   ")
                      (plist-get (cdr item) :label) "\n")))
          (goto-char (point-min))
          (set-window-start window (point-min) t)
          (set-window-point window (point-min)))))))

;;;; Selecting and opening

(defun lme-desktop--select (key)
  "Select the desktop icon KEY, or none when KEY is nil, and redraw."
  (with-current-buffer (lme-desktop-buffer)
    (unless (eq key lme-desktop--selected)
      (setq lme-desktop--selected key)
      (lme-desktop--render))))

(defun lme-desktop--key-at (x y)
  "Return the key of the desktop icon drawn at X, Y, or nil."
  (car (seq-find (pcase-lambda (`(,_ ,left ,top ,right ,bottom))
                   (and (<= left x right) (<= top y bottom)))
                 (buffer-local-value 'lme-desktop--hits (lme-desktop-buffer)))))

(defun lme-desktop--item-at (event)
  "Return the key of the desktop icon mouse EVENT is over, or nil."
  (when-let* ((xy (posn-object-x-y (event-start event))))
    (lme-desktop--key-at (car xy) (cdr xy))))

(defun lme-desktop-mouse-select (event)
  "Select the desktop icon pressed by EVENT, as Mac OS 9 did on the press,
or none on the desktop between them.  A click on the desktop brings the
desktop in front."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (setq lme-finder--had-focus nil)
  (lme-apps-set-frontmost 'desktop)
  (lme-desktop--select (lme-desktop--item-at event)))

(defun lme-desktop-mouse-open (event)
  "Open the desktop icon double-clicked by EVENT."
  (interactive "e")
  (lme-desktop-mouse-select event)
  (when (lme-desktop--item-at event)
    (lme-desktop-open)))

(defun lme-desktop-deselect ()
  "Select no desktop icon: a click on the desktop itself, which brings the
desktop in front."
  (interactive)
  (setq lme-finder--had-focus nil)
  (lme-apps-set-frontmost 'desktop)
  (lme-desktop--select nil))

(defun lme-desktop-open ()
  "Open the selected desktop icon."
  (interactive)
  (when-let* ((key (buffer-local-value 'lme-desktop--selected (lme-desktop-buffer)))
              (command (plist-get (alist-get key lme-desktop-items) :command)))
    (lme-desktop--select nil)
    (call-interactively command)))

(defun lme-desktop--step (direction)
  "Select the icon DIRECTION (1 or -1) from the selected one, wrapping around."
  (let* ((keys (mapcar #'car (lme-desktop--items)))
         (index (seq-position keys lme-desktop--selected)))
    (when keys
      (lme-desktop--select
       (nth (mod (if index (+ index direction) (if (> direction 0) 0 -1)) (length keys))
            keys)))))

(defun lme-desktop-select-next ()
  "Select the next desktop icon down."
  (interactive)
  (lme-desktop--step 1))

(defun lme-desktop-select-previous ()
  "Select the next desktop icon up."
  (interactive)
  (lme-desktop--step -1))

;;;; Painting

(defvar-local lme-desktop--background nil
  "Face remapping cookies colouring a desktop buffer.")

(defun lme-desktop--paint-buffer (buffer)
  "Colour BUFFER as the desktop, its fringes included."
  (with-current-buffer buffer
    (mapc #'face-remap-remove-relative lme-desktop--background)
    (setq lme-desktop--background
          (list (face-remap-add-relative 'default :background (lme-desktop--color))
                (face-remap-add-relative 'fringe :background (lme-desktop--color))))))

(defun lme-desktop--paint ()
  "Colour the desktop, its background and its names from the palette."
  (when lme-platinum--palette
    (let ((content (lme-platinum--color :content))
          (text (lme-platinum--color :text)))
      (set-face-attribute 'lme-desktop-label nil
                          :inherit 'lme-platinum-list :background content :foreground text)
      (set-face-attribute 'lme-desktop-label-selected nil
                          :inherit 'lme-platinum-list :background text :foreground content)
      (dolist (name (list lme-desktop-buffer-name lme-desktop-empty-buffer-name))
        (when-let* ((buffer (get-buffer name)))
          (lme-desktop--paint-buffer buffer)))
      (when (get-buffer lme-desktop-buffer-name)
        (lme-desktop--render)))))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(defun lme-desktop--bare ()
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

(define-derived-mode lme-desktop-mode special-mode "Desktop"
  "Major mode of the desktop, the place behind every window.
\\{lme-desktop-mode-map}"
  (lme-desktop--bare)
  (setq-local revert-buffer-function (lambda (&rest _) (lme-desktop--render)))
  (add-hook 'window-size-change-functions #'lme-desktop--render nil t)
  (add-hook 'window-buffer-change-functions #'lme-desktop--render nil t))

(defvar-keymap lme-desktop-empty-mode-map
  :doc "Keymap of the desktop's background: nothing to click."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'ignore
  "q" #'ignore)

(define-derived-mode lme-desktop-empty-mode special-mode "Desktop"
  "Major mode of the desktop's colour without its icons, the background of
a workspace whose main area holds nothing else."
  (lme-desktop--bare))

(defun lme-desktop-buffer ()
  "Return the desktop's buffer, made when missing."
  (or (get-buffer lme-desktop-buffer-name)
      (with-current-buffer (get-buffer-create lme-desktop-buffer-name)
        (lme-desktop-mode)
        (setq lme-desktop--background nil)
        (lme-desktop--paint)
        (current-buffer))))

(defun lme-desktop-empty-buffer ()
  "Return the desktop's background buffer, made when missing."
  (or (get-buffer lme-desktop-empty-buffer-name)
      (with-current-buffer (get-buffer-create lme-desktop-empty-buffer-name)
        (lme-desktop-empty-mode)
        (when lme-platinum--palette
          (lme-desktop--paint-buffer (current-buffer)))
        (current-buffer))))

;;;; The desktop's workspace

(defun lme-desktop--tab-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, is in the
desktop's workspace."
  (equal (alist-get 'name (tab-bar--current-tab nil frame)) lme-desktop-tab-name))

(defvar lme-desktop-empty-function nil
  "Function filling WINDOW, its one argument, of a workspace whose main area
holds nothing; it returns nil to leave WINDOW to the desktop's colour.
The editor shows Emacs's own startup screen there.")

(defun lme-desktop-show (&optional window)
  "Show the desktop in WINDOW, by default the selected window.
In the desktop's workspace it shows with its icons; in any other, where
the main area holds nothing else, what `lme-desktop-empty-function' shows,
or the desktop's bare colour."
  (let ((window (or window (selected-window))))
    (set-window-dedicated-p window nil)
    (cond ((lme-desktop--tab-p (window-frame window))
           (set-window-buffer window (lme-desktop-buffer))
           (lme-desktop--render window))
          ((and lme-desktop-empty-function (funcall lme-desktop-empty-function window)))
          (t (set-window-buffer window (lme-desktop-empty-buffer))))))

(defun lme-desktop-p (&optional window)
  "Return non-nil when WINDOW, by default the selected one, shows the desktop."
  (member (buffer-name (window-buffer window))
          (list lme-desktop-buffer-name lme-desktop-empty-buffer-name)))

(defun lme-desktop--select-tab (&optional frame)
  "Select the desktop's workspace in FRAME, made when missing: the desktop
alone, filling the frame."
  (with-selected-frame (or frame (selected-frame))
    (if-let* ((index (tab-bar--tab-index-by-name lme-desktop-tab-name)))
        (unless (lme-desktop--tab-p)
          (tab-bar-select-tab (1+ index)))
      (let ((tab-bar-new-tab-choice #'lme-desktop-buffer))
        (tab-bar-new-tab))
      (tab-bar-rename-tab lme-desktop-tab-name))
    (let ((window (car (lme-main-area--windows))))
      (when (or (cdr (window-list nil 'no-mini)) (not (lme-desktop-p window)))
        (let ((ignore-window-parameters t))
          (delete-other-windows window)))
      (lme-desktop-show window))))

(defvar lme-desktop-activate-hook nil
  "Hook run when the desktop comes in front, as windows left over it show.")

(defun lme-desktop-activate ()
  "Bring the desktop in front: its workspace, the floating editor left over it."
  (interactive)
  (let ((screen (lme-apps-screen)))
    (unless (eq (selected-frame) screen)
      (select-frame-set-input-focus screen))
    (lme-desktop--select-tab screen))
  (lme-apps-set-frontmost 'desktop)
  (run-hooks 'lme-desktop-activate-hook))

(defun lme-desktop--on-new-workspace (&rest _)
  "Show the desktop's colour in a new workspace that opened onto *scratch*."
  (let ((window (selected-window)))
    (when (and (equal (buffer-name (window-buffer window)) "*scratch*")
               (not (window-parameter window 'window-side)))
      (lme-desktop-show window))))

;;;; The desktop's menus

(defun lme-desktop--file-items ()
  "Return the items of the desktop's File menu.  Open and Close Window act
on the Finder window when it had the keyboard last, else on the desktop."
  (let* ((finder (and (fboundp 'lme-finder-shown-p) (lme-finder-shown-p)))
         (in-finder (and finder (bound-and-true-p lme-finder--had-focus)))
         (selected (buffer-local-value 'lme-desktop--selected (lme-desktop-buffer))))
    `((:label "New Folder…" :action lme-finder-new-folder :enabled ,(and finder t))
      (:label "Open" :action ,(if in-finder 'lme-finder-open 'lme-desktop-open)
              :enabled ,(and (or in-finder selected) t))
      (:label "Close Window" :action lme-finder-close :enabled ,(and finder t))
      separator
      (:label "Move to Trash" :action lme-finder-move-to-trash :key "M-DEL"
              :enabled ,(and in-finder
                             (with-current-buffer (lme-finder--buffer) lme-finder--selected)
                             t))
      separator
      ,@(mapcar (lambda (item)
                  (list :label (plist-get (cdr item) :label)
                        :action (plist-get (cdr item) :command)))
                (lme-desktop--items)))))

(defun lme-desktop--special-items ()
  "Return the items of the desktop's Special menu, Mac OS 9's last ones."
  `(,@(when (fboundp 'restart-emacs)
        '((:label "Restart" :action restart-emacs)))
    (:label "Shut Down" :action save-buffers-kill-emacs)))

(defun lme-desktop-menus ()
  "Return the desktop's menus, for the menu bar while it is in front."
  '((lme-desktop-file "File" lme-desktop--file-items)
    (lme-desktop-go "Go" lme-finder--go-items)
    (lme-desktop-special "Special" lme-desktop--special-items)))

(defun lme-setup-desktop ()
  "Put the desktop behind every window, and start Emacs on it.
Its settings take effect when changed however they are, a `setq' in
personal.el included, and the Trash is looked at every few seconds."
  (setq initial-buffer-choice #'lme-desktop-empty-buffer
        tab-bar-new-tab-choice #'lme-desktop-empty-buffer)
  (dolist (setting '(lme-desktop-color lme-desktop-picture lme-desktop-picture-position))
    (add-variable-watcher setting #'lme-desktop--setting-changed))
  (lme-desktop-check-trash)
  (unless lme-desktop--trash-timer
    (setq lme-desktop--trash-timer (run-with-timer 5 5 #'lme-desktop-check-trash)))
  (add-hook 'tab-bar-tab-post-open-functions #'lme-desktop--on-new-workspace)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lme-desktop--paint)) 90)
  (add-hook 'window-setup-hook #'lme-desktop-activate 95))

(provide 'lme-desktop)
;;; lme-desktop.el ends here
