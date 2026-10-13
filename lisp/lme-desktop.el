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

;; Mac OS 9's desktop lay behind every window: closing the last window
;; showed it, and its icons opened the disks, the applications and the
;; Trash.  Here the desktop is a buffer the main area shows when nothing
;; else is there: at the start, in a new workspace, and when a close box
;; closes the main area's last window.  Whatever opens next covers it.
;;
;; Its icons stand down the right edge, where Mac OS 9 put them: the home
;; disk at the top, the browser under it, the Trash in the bottom corner.
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

(declare-function lme-browser-show "lme-browser" ())
(declare-function lme-xwidget-available-p "lme-browser" ())

(defconst lme-desktop-buffer-name " *Desktop*"
  "Name of the desktop's buffer; the leading space keeps it out of buffer lists.")

(defun lme-desktop--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lme-xwidget-available-p) (lme-xwidget-available-p)))

(defvar lme-desktop-items
  '((home :label "Home" :icon "hard-disk-32" :command lme-desktop-open-home
          :help "Your home folder, in the file manager")
    (browser :label "Browser" :icon "globe-32" :command lme-browser-show
             :visible lme-desktop--browser-p :help "The web browser")
    (trash :label "Trash" :icon "trash-32" :command lme-desktop-open-trash
           :bottom t :help "What was thrown away"))
  "The desktop's icons, from the top: (KEY . PROPERTIES).
:label names an icon, :icon is its file in icons/os9, :command opens it
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

(defun lme-desktop-open-home ()
  "Open the home folder in the file manager, as a disk opened in Finder."
  (interactive)
  (dired "~"))

(defun lme-desktop--trash-directory ()
  "Return the directory holding the Trash's files."
  (if (lme-macos-p)
      (expand-file-name "~/.Trash")
    (expand-file-name "Trash/files" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))

(defun lme-desktop-open-trash ()
  "Open the Trash: in the file manager where Emacs may read it.
macOS lets only programs with Full Disk Access read the Trash; without it,
the system's own Trash window opens instead."
  (interactive)
  (let ((directory (lme-desktop--trash-directory)))
    (if (ignore-errors (directory-files directory))
        (dired directory)
      (if (lme-macos-p)
          (call-process "open" nil 0 nil directory)
        (user-error "The Trash cannot be read: %s" directory)))))

;;;; Drawing

(defface lme-desktop-label nil
  "Face of a desktop icon's name: the theme's text on its background."
  :group 'lme-platinum)

(defface lme-desktop-label-selected nil
  "Face of a selected desktop icon's name: inverted, as Mac OS 9's was."
  :group 'lme-platinum)

(defvar-keymap lme-desktop-item-map
  :doc "Keymap of a desktop icon: a press selects it, a double click opens it."
  "<down-mouse-1>" #'lme-desktop-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lme-desktop-mouse-open
  "<drag-mouse-1>" #'ignore)

(defvar-keymap lme-desktop-mode-map
  :doc "Keymap of the desktop."
  "<down-mouse-1>" #'lme-desktop-deselect
  "<mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
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

(defvar lme-desktop--icons (make-hash-table :test #'equal)
  "Desktop icon images by (NAME . SELECTED).")

(defun lme-desktop--darken (svg)
  "Return SVG with every colour halved, as Mac OS 9 darkened a selected icon."
  (replace-regexp-in-string
   "#\\([[:xdigit:]]\\{6\\}\\)"
   (lambda (match)
     (apply #'format "#%02X%02X%02X"
            (mapcar (lambda (i) (/ (string-to-number (substring match i (+ i 2)) 16) 2))
                    '(1 3 5))))
   svg t t))

(defun lme-desktop--icon (name selected)
  "Return the icon NAME as a string showing it, darkened when SELECTED.
Where images cannot be shown, a blank of the icon's width stands in."
  (if (not (display-images-p))
      (propertize " " 'display `(space :width (,lme-desktop-icon-size)))
    (propertize
     " " 'display
     (or (gethash (cons name selected) lme-desktop--icons)
         (puthash (cons name selected)
                  (let ((svg (with-temp-buffer
                               (insert-file-contents
                                (expand-file-name (format "icons/os9/%s.svg" name)
                                                  lme-directory))
                               (buffer-string))))
                    (create-image (if selected (lme-desktop--darken svg) svg) 'svg t
                                  :height lme-desktop-icon-size :ascent 'center))
                  lme-desktop--icons)))))

(defvar-local lme-desktop--selected nil
  "Key of the selected desktop icon, or nil.")

(defun lme-desktop--items ()
  "Return the desktop's icons that show, see `lme-desktop-items'."
  (seq-filter (lambda (item)
                (let ((visible (plist-get (cdr item) :visible)))
                  (or (not visible) (funcall visible))))
              lme-desktop-items))

(defun lme-desktop--align (half)
  "Return a blank reaching to HALF pixels left of the icons' column's centre."
  (propertize " " 'display
              `(space :align-to (- right (,(+ lme-desktop-margin (/ lme-desktop-column 2) half))))))

(defun lme-desktop--spacer (height)
  "Return a line HEIGHT pixels tall."
  (concat (propertize " " 'display `(space :width (1) :height (,(max 1 height))))
          "\n"))

(defun lme-desktop--insert-item (item)
  "Insert desktop ITEM: its icon over its name, centred in the icons' column."
  (let* ((key (car item))
         (spec (cdr item))
         (selected (eq key lme-desktop--selected))
         (label (propertize (concat " " (plist-get spec :label) " ")
                            'face (if selected 'lme-desktop-label-selected 'lme-desktop-label)))
         (props `(lme-desktop-item ,key keymap ,lme-desktop-item-map
                                   help-echo ,(plist-get spec :help) pointer arrow)))
    (insert (lme-desktop--align (/ lme-desktop-icon-size 2))
            (apply #'propertize (lme-desktop--icon (plist-get spec :icon) selected) props)
            "\n"
            (lme-desktop--align (/ (string-pixel-width label) 2))
            (apply #'propertize label props)
            "\n")))

(defun lme-desktop--render (&optional window)
  "Lay the desktop out for WINDOW, by default the window showing it.
Icons stand from the top of the right edge; those marked :bottom stand in
the bottom corner, a spacer filling the height between."
  (let ((buffer (lme-desktop-buffer)))
    (when-let* ((window (or window (get-buffer-window buffer))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (items (lme-desktop--items))
              spacer)
          (erase-buffer)
          (insert (lme-desktop--spacer lme-desktop-margin))
          (dolist (item (seq-remove (lambda (item) (plist-get (cdr item) :bottom)) items))
            (lme-desktop--insert-item item)
            (insert (lme-desktop--spacer lme-desktop-gap)))
          (when-let* ((bottom (seq-filter (lambda (item) (plist-get (cdr item) :bottom)) items)))
            (setq spacer (point))
            (insert (lme-desktop--spacer 1))
            (let ((below (point)))
              (dolist (item bottom)
                (lme-desktop--insert-item item))
              ;; The spacer takes up the height the icons above and below
              ;; leave free.
              (let ((free (- (window-body-height window t) lme-desktop-margin
                             (cdr (window-text-pixel-size window (point-min) spacer))
                             (cdr (window-text-pixel-size window below (point-max))))))
                (put-text-property spacer (1+ spacer) 'display
                                   `(space :width (1) :height (,(max 1 free)))))))
          (goto-char (point-min))
          (set-window-start window (point-min))
          (set-window-point window (point-min)))))))

;;;; Selecting and opening

(defun lme-desktop--select (key)
  "Select the desktop icon KEY, or none when KEY is nil, and redraw."
  (with-current-buffer (lme-desktop-buffer)
    (unless (eq key lme-desktop--selected)
      (setq lme-desktop--selected key)
      (lme-desktop--render))))

(defun lme-desktop--item-at (event)
  "Return the key of the desktop icon mouse EVENT is over, or nil."
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position)))
      (get-text-property point 'lme-desktop-item
                         (window-buffer (posn-window position))))))

(defun lme-desktop-mouse-select (event)
  "Select the desktop icon pressed by EVENT, as Mac OS 9 did on the press."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (lme-desktop--select (lme-desktop--item-at event)))

(defun lme-desktop-mouse-open (event)
  "Open the desktop icon double-clicked by EVENT."
  (interactive "e")
  (lme-desktop-mouse-select event)
  (lme-desktop-open))

(defun lme-desktop-deselect ()
  "Select no desktop icon: a click on the desktop itself."
  (interactive)
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

(defvar-local lme-desktop--background nil
  "Face remapping cookie colouring the desktop.")

(defun lme-desktop--paint ()
  "Colour the desktop and its names from the palette."
  (when lme-platinum--palette
    (let ((content (lme-platinum--color :content))
          (text (lme-platinum--color :text)))
      (set-face-attribute 'lme-desktop-label nil
                          :inherit 'lme-platinum-list :background content :foreground text)
      (set-face-attribute 'lme-desktop-label-selected nil
                          :inherit 'lme-platinum-list :background text :foreground content)
      (when-let* ((buffer (get-buffer lme-desktop-buffer-name)))
        (with-current-buffer buffer
          (when lme-desktop--background
            (face-remap-remove-relative lme-desktop--background))
          (setq lme-desktop--background
                (face-remap-add-relative 'default :background (lme-platinum--color :desktop)))
          (lme-desktop--render))))))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lme-desktop-mode special-mode "Desktop"
  "Major mode of the desktop, the place behind every window.
\\{lme-desktop-mode-map}"
  (setq-local mode-line-format nil
              header-line-format nil
              tab-line-format nil
              cursor-type nil
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              revert-buffer-function (lambda (&rest _) (lme-desktop--render)))
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (add-hook 'window-size-change-functions #'lme-desktop--render nil t)
  (add-hook 'window-buffer-change-functions #'lme-desktop--render nil t))

(defun lme-desktop-buffer ()
  "Return the desktop's buffer, made when missing."
  (or (get-buffer lme-desktop-buffer-name)
      (with-current-buffer (get-buffer-create lme-desktop-buffer-name)
        (lme-desktop-mode)
        (setq lme-desktop--background nil)
        (lme-desktop--paint)
        (current-buffer))))

(defun lme-desktop-show (&optional window)
  "Show the desktop in WINDOW, by default the selected window."
  (let ((window (or window (selected-window))))
    (set-window-dedicated-p window nil)
    (set-window-buffer window (lme-desktop-buffer))
    (lme-desktop--render window)))

(defun lme-desktop-p (&optional window)
  "Return non-nil when WINDOW, by default the selected one, shows the desktop."
  (equal (buffer-name (window-buffer window)) lme-desktop-buffer-name))

(defun lme-desktop--on-new-workspace (&rest _)
  "Show the desktop in a new workspace that opened onto *scratch*."
  (let ((window (selected-window)))
    (when (and (equal (buffer-name (window-buffer window)) "*scratch*")
               (not (window-parameter window 'window-side)))
      (lme-desktop-show window))))

(defun lme-setup-desktop ()
  "Put the desktop behind every window: at the start and in new workspaces."
  (setq initial-buffer-choice #'lme-desktop-buffer
        tab-bar-new-tab-choice #'lme-desktop-buffer)
  (add-hook 'tab-bar-tab-post-open-functions #'lme-desktop--on-new-workspace)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lme-desktop--paint)) 90))

(provide 'lme-desktop)
;;; lme-desktop.el ends here
