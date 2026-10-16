;;; lispm-desktop-finder.el --- The file manager: Finder windows on the desktop  -*- lexical-binding: t; -*-

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

;; The desktop's Home icon opens a Finder window, as a disk opened in Mac
;; OS 9's Finder: a folder's items as large icons, floating over the
;; desktop.  Its toolbar goes back and forward through the folders shown,
;; up to the enclosing folder, and makes a new folder; its bottom edge
;; counts the items and the room left.  A folder opens in the window; a
;; file opens in the editor, which comes forward.
;;
;; The window is the desktop's own, a child frame of the screen: it shows
;; while the desktop is in front, and over the floating editor, and steps
;; aside for the full-screen editor and the browser.
;;
;;   folder --items--> rows of icons --click--> selected --double click--> opened
;;
;; Keys, as Finder's: the arrows or C-f, C-b, C-n and C-p select, Return
;; or Command-O opens, Command-Up or Delete goes to the enclosing folder,
;; Command-[ and Command-] back and forward, Command-Shift-N makes a
;; folder, Command-Delete moves the selected item to the Trash, Command-W
;; closes the window, and typing a name's first letters selects it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-surface)

(declare-function lispm-desktop-editor-activate "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup--frame-xy "lispm-desktop-popup" (position))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(defvar lispm-desktop-editor--frame)
(defvar lispm-desktop-browser--frame)
(defvar lispm-desktop-browser-floating)

(defconst lispm-desktop-finder-buffer-name " *Finder*"
  "Name of the Finder window's buffer.")

(defconst lispm-desktop-finder-cell-width 100
  "Width in pixels of an item's cell in the window: its icon and its name.")

(defconst lispm-desktop-finder-icon-size 32
  "Side in pixels of an item's icon.")

(defvar lispm-desktop-finder--frame nil
  "The Finder window, a child frame of the screen, or nil when closed.")

(defvar lispm-desktop-finder--geometry nil
  "Where the Finder window was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lispm-desktop-finder--had-focus nil
  "Non-nil while the Finder window was the desktop's last focused window.")

(defvar-local lispm-desktop-finder--directory nil
  "The folder the window shows.")

(defvar-local lispm-desktop-finder--back nil
  "Folders shown before this one, the last first.")

(defvar-local lispm-desktop-finder--forward nil
  "Folders gone back from, the nearest first.")

(defvar-local lispm-desktop-finder--items nil
  "The folder's items in the order shown: (NAME . FOLDER-P).")

(defvar-local lispm-desktop-finder--selected nil
  "Name of the selected item, or nil.")

(defvar-local lispm-desktop-finder--columns 1
  "Items a row of the window holds, as last drawn.")

(defvar-local lispm-desktop-finder--typed nil
  "(TEXT . TIME): the letters typed to select an item, and when.")

(defvar-local lispm-desktop-finder--shade nil
  "While the window is rolled up: its text height.")

;;;; Items

(defvar lispm-desktop-finder-folders nil
  "Folders that are no directory on disk, shown as the Finder shows one, as
Mac OS 9's Control Panels folder was: (KEY . PROPERTIES).  KEY, a symbol,
stands for the folder where a directory's name would.  :title names the
folder, :icon gives its small icon as `lispm-desktop-platinum-icon-file' takes
it, and :items is a function returning its items, each (NAME ICON ACTION):
ICON its large icon, ACTION a function opening it.  Nothing is made, renamed
or thrown away in such a folder.")

(defun lispm-desktop-finder--folder (directory)
  "Return the properties of DIRECTORY when it is a virtual folder.
The virtual folders are `lispm-desktop-finder-folders'."
  (and directory (symbolp directory) (alist-get directory lispm-desktop-finder-folders)))

(defun lispm-desktop-finder--folder-items (directory)
  "Return the items of DIRECTORY, one of `lispm-desktop-finder-folders'."
  (funcall (plist-get (lispm-desktop-finder--folder directory) :items)))

(defun lispm-desktop-finder--read (directory)
  "Return DIRECTORY's items by name, hidden ones left out: (NAME . FOLDER-P)."
  (sort (if (lispm-desktop-finder--folder directory)
            (mapcar (lambda (item) (cons (car item) nil)) (lispm-desktop-finder--folder-items directory))
          (mapcar (lambda (name)
                    (cons name (file-directory-p (expand-file-name name directory))))
                  (ignore-errors (directory-files directory nil "\\`[^.]" t))))
        (lambda (a b) (string-collate-lessp (car a) (car b) nil t))))

(defun lispm-desktop-finder--icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY."
  (if (lispm-desktop-finder--folder directory)
      (nth 1 (assoc (car item) (lispm-desktop-finder--folder-items directory)))
    (lispm-desktop-finder--file-icon-name directory item)))

(defun lispm-desktop-finder--file-icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY, a directory."
  (let ((path (expand-file-name (car item) directory)))
    (if (cdr item)
        (if (file-exists-p (expand-file-name ".git" path)) "folder-git-32" "folder-32")
      (let ((name (concat (lispm-desktop-platinum-file-icon-name path) "-32")))
        (if (file-exists-p (lispm-desktop-platinum-icon-file name)) name "document-32")))))

(defvar lispm-desktop-finder--icons (make-hash-table :test #'equal)
  "Large icon images by (NAME . SELECTED).")

(defun lispm-desktop-finder--icon (name selected)
  "Return the large icon NAME as a string showing it, darkened when SELECTED."
  (if (not (display-images-p))
      (propertize " " 'display `(space :width (,lispm-desktop-finder-icon-size)))
    (propertize
     " " 'display
     (or (gethash (cons name selected) lispm-desktop-finder--icons)
         (puthash (cons name selected)
                  (let ((svg (with-temp-buffer
                               (insert-file-contents (lispm-desktop-platinum-icon-file name))
                               (buffer-string))))
                    (create-image (if selected (lispm-desktop--darken svg) svg) 'svg t
                                  :height lispm-desktop-finder-icon-size :ascent 'center))
                  lispm-desktop-finder--icons)))))

;;;; Drawing

(defvar-keymap lispm-desktop-finder-item-map
  :doc "Keymap of an item: a press selects it, a double click opens it."
  "<down-mouse-1>" #'lispm-desktop-finder-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lispm-desktop-finder-mouse-open
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-desktop-finder-context-menu
  "<mouse-3>" #'ignore)


(defface lispm-desktop-finder-label '((t :inherit lispm-desktop-platinum-list))
  "Face of an item's name in the Finder window."
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-finder--header ()
  "Return the window's header: Mac OS 9's strip counting its items and the
room left on the disk, under the title bar.  The window has no toolbar:
back, forward and the enclosing folder are on their keys, the folder's
path on its title's Command click, New Folder in the File menu."
  (lispm-desktop-platinum-header-strip (lispm-desktop-finder--info)))

(defun lispm-desktop-finder--info ()
  "Return what the window's header says: its items, and the room left on
the disk."
  (let ((count (length lispm-desktop-finder--items))
        (available (and (stringp lispm-desktop-finder--directory)
                        (nth 2 (ignore-errors (file-system-info lispm-desktop-finder--directory))))))
    (concat (format "%d item%s" count (if (= count 1) "" "s"))
            (when available
              (format ", %s available" (file-size-human-readable available 'si " " "B"))))))

(defun lispm-desktop-finder--title (directory)
  "Return the window's title for DIRECTORY: the folder's name."
  (if-let* ((folder (lispm-desktop-finder--folder directory)))
      (plist-get folder :title)
    (let ((name (file-name-nondirectory (directory-file-name directory))))
      (if (string-empty-p name) directory name))))

(defun lispm-desktop-finder--cell-left (column)
  "Return the pixel left of the window's cell COLUMN."
  (+ 6 (* column lispm-desktop-finder-cell-width)))

(defun lispm-desktop-finder--render (&rest _)
  "Draw the folder's items as rows of large icons over their names."
  (when-let* ((buffer (get-buffer lispm-desktop-finder-buffer-name))
              (window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (cell lispm-desktop-finder-cell-width)
             (columns (max 1 (/ (- (window-body-width window t) 12) cell)))
             (props (lambda (name)
                      (list 'lispm-desktop-finder-item name 'keymap lispm-desktop-finder-item-map
                            'pointer 'arrow 'help-echo name))))
        (setq lispm-desktop-finder--columns columns)
        (erase-buffer)
        (insert (propertize " " 'display '(space :height (12))) "\n")
        (dolist (row (seq-partition lispm-desktop-finder--items columns))
          (seq-do-indexed
           (lambda (item column)
             (insert (propertize " " 'display
                                 `(space :align-to (,(+ (lispm-desktop-finder--cell-left column)
                                                        (/ (- cell lispm-desktop-finder-icon-size) 2)))))
                     (apply #'propertize
                            (lispm-desktop-finder--icon (lispm-desktop-finder--icon-name lispm-desktop-finder--directory item)
                                                        (equal (car item) lispm-desktop-finder--selected))
                            (funcall props (car item)))))
           row)
          (insert "\n")
          (seq-do-indexed
           (lambda (item column)
             (let ((label (lispm-desktop-platinum--fit
                           (propertize (concat " " (car item) " ")
                                       'face (if (equal (car item) lispm-desktop-finder--selected)
                                                 'lispm-desktop-label-selected
                                               'lispm-desktop-finder-label))
                           (- cell 6))))
               (insert (propertize " " 'display
                                   `(space :align-to (,(+ (lispm-desktop-finder--cell-left column)
                                                          (/ (- cell (string-pixel-width label))
                                                             2)))))
                       (apply #'propertize label (funcall props (car item))))))
           row)
          (insert "\n" (propertize " " 'display '(space :height (14))) "\n"))
        (setq lispm-desktop-platinum-title (lispm-desktop-finder--title lispm-desktop-finder--directory)
              lispm-desktop-platinum-icon (if-let* ((folder (lispm-desktop-finder--folder lispm-desktop-finder--directory)))
                                              (lispm-desktop-platinum-icon (plist-get folder :icon))
                                            (lispm-desktop-platinum-folder-icon lispm-desktop-finder--directory))
              ;; The header counts the items; the bottom edge holds only
              ;; the grow box.
              lispm-desktop-platinum-info "")
        ;; The selected item is kept in view.
        (let ((at (or (and lispm-desktop-finder--selected
                           (text-property-any (point-min) (point-max)
                                              'lispm-desktop-finder-item lispm-desktop-finder--selected))
                      (point-min))))
          (goto-char at)
          (set-window-point window at))
        (force-mode-line-update)))))

;;;; Going places

(defun lispm-desktop-finder--buffer ()
  "Return the Finder window's buffer, made when missing."
  (or (get-buffer lispm-desktop-finder-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-finder-buffer-name)
        (lispm-desktop-finder-mode)
        (current-buffer))))

(defmacro lispm-desktop-finder--in (&rest body)
  "Run BODY in the Finder window's buffer."
  (declare (indent 0))
  `(with-current-buffer (lispm-desktop-finder--buffer) ,@body))

(defun lispm-desktop-finder-real-p ()
  "Return non-nil when the Finder window shows a directory on disk, where
items are made, renamed and thrown away."
  (lispm-desktop-finder--in (and lispm-desktop-finder--directory (stringp lispm-desktop-finder--directory))))

(defun lispm-desktop-finder--parent (directory)
  "Return the folder enclosing DIRECTORY, or nil at the top, as for a folder
of `lispm-desktop-finder-folders'."
  (when (stringp directory)
    (let ((parent (file-name-directory (directory-file-name directory))))
      (unless (or (null parent) (equal parent directory)) parent))))

(defun lispm-desktop-finder--path-items (_window)
  "Return the menu of the shown folder's path, as its title's Command click
pops it up, as Mac OS 9's did: the folder, then each folder enclosing it,
each going there."
  (let ((folders nil)
        (directory (lispm-desktop-finder--in lispm-desktop-finder--directory)))
    (while directory
      (push directory folders)
      (setq directory (lispm-desktop-finder--parent directory)))
    (mapcar (lambda (folder)
              (list :label (lispm-desktop-finder--title folder)
                    :action (lambda () (lispm-desktop-finder--go folder t))))
            (nreverse folders))))

(defun lispm-desktop-finder--go (directory &optional record select)
  "Show DIRECTORY, with SELECT selected; RECORD keeps the folder left in
the history, as going there by hand does."
  (lispm-desktop-finder--in
    (when (and record lispm-desktop-finder--directory)
      (push lispm-desktop-finder--directory lispm-desktop-finder--back)
      (setq lispm-desktop-finder--forward nil))
    (setq lispm-desktop-finder--directory (if (lispm-desktop-finder--folder directory)
                                              directory
                                            (file-name-as-directory (expand-file-name directory)))
          lispm-desktop-finder--items (lispm-desktop-finder--read lispm-desktop-finder--directory)
          lispm-desktop-finder--selected select)
    (lispm-desktop-finder--render)))

(defun lispm-desktop-finder-back ()
  "Show the folder shown before this one."
  (interactive)
  (lispm-desktop-finder--in
    (when-let* ((previous (pop lispm-desktop-finder--back)))
      (push lispm-desktop-finder--directory lispm-desktop-finder--forward)
      (lispm-desktop-finder--go previous))))

(defun lispm-desktop-finder-forward ()
  "Show again the folder gone back from."
  (interactive)
  (lispm-desktop-finder--in
    (when-let* ((next (pop lispm-desktop-finder--forward)))
      (push lispm-desktop-finder--directory lispm-desktop-finder--back)
      (lispm-desktop-finder--go next))))

(defun lispm-desktop-finder-up ()
  "Show the enclosing folder, the folder left selected in it."
  (interactive)
  (lispm-desktop-finder--in
    (when-let* ((parent (lispm-desktop-finder--parent lispm-desktop-finder--directory)))
      (lispm-desktop-finder--go parent t (lispm-desktop-finder--title lispm-desktop-finder--directory)))))

(defun lispm-desktop-finder-refresh ()
  "Read the folder again."
  (interactive)
  (lispm-desktop-finder--in
    (lispm-desktop-finder--go lispm-desktop-finder--directory nil lispm-desktop-finder--selected)))

(defun lispm-desktop-finder--edit (file)
  "Open FILE in the editor, brought in front."
  (lispm-desktop-editor-activate)
  (find-file file))

(defun lispm-desktop-finder-open ()
  "Open the selected item: a folder in the window, a file in the editor, an
item of a folder of `lispm-desktop-finder-folders' by its own action."
  (interactive)
  (let ((action nil))
    (lispm-desktop-finder--in
      (when-let* ((name lispm-desktop-finder--selected))
        (if (lispm-desktop-finder--folder lispm-desktop-finder--directory)
            (setq action (nth 2 (assoc name (lispm-desktop-finder--folder-items lispm-desktop-finder--directory))))
          (let ((path (expand-file-name name lispm-desktop-finder--directory)))
            (if (file-directory-p path)
                (lispm-desktop-finder--go path t)
              (lispm-desktop-finder--edit path))))))
    (when action
      (funcall action))))

(defun lispm-desktop-finder--real-only ()
  "Refuse to change the folder shown unless it is a directory on disk."
  (unless (lispm-desktop-finder-real-p)
    (user-error "Nothing can be made, renamed or thrown away in “%s”"
                (lispm-desktop-finder--in (lispm-desktop-finder--title lispm-desktop-finder--directory)))))

(defun lispm-desktop-finder-new-folder (name)
  "Make a folder called NAME in the folder shown, and select it."
  (interactive (progn (lispm-desktop-finder--real-only)
                      (list (read-string "Name of the new folder: " "untitled folder"))))
  (lispm-desktop-finder--real-only)
  (lispm-desktop-finder--in
    (let ((path (expand-file-name name lispm-desktop-finder--directory)))
      (when (file-exists-p path)
        (user-error "There is already an item called “%s” here" name))
      (make-directory path)
      (lispm-desktop-finder--go lispm-desktop-finder--directory nil name))))

(defun lispm-desktop-finder-move-to-trash ()
  "Move the selected item to the Trash, as Command-Delete did in Finder.
It goes to the system's own Trash, from where it can be put back; the item
after it is selected."
  (interactive)
  (lispm-desktop-finder--real-only)
  (lispm-desktop-finder--in
    (when-let* ((name lispm-desktop-finder--selected))
      (let* ((names (mapcar #'car lispm-desktop-finder--items))
             (index (or (seq-position names name) 0))
             (next (or (nth (1+ index) names) (and (> index 0) (nth (1- index) names)))))
        (condition-case err
            (progn
              (move-file-to-trash (expand-file-name name lispm-desktop-finder--directory))
              (lispm-desktop-check-trash)
              (lispm-desktop-finder--go lispm-desktop-finder--directory nil next)
              (message "“%s” was moved to the Trash" name))
          (error
           (lispm-desktop-popup-alert 'stop (format "“%s” could not be moved to the Trash." name)
                                      '(("OK" ok default))
                                      (error-message-string err))))))))

(defun lispm-desktop-finder-rename (name)
  "Rename the selected item to NAME."
  (interactive (progn (lispm-desktop-finder--real-only)
                      (list (read-string "Rename to: " (lispm-desktop-finder--in lispm-desktop-finder--selected)))))
  (lispm-desktop-finder--real-only)
  (lispm-desktop-finder--in
    (when lispm-desktop-finder--selected
      (rename-file (expand-file-name lispm-desktop-finder--selected lispm-desktop-finder--directory)
                   (expand-file-name name lispm-desktop-finder--directory))
      (lispm-desktop-finder--go lispm-desktop-finder--directory nil name))))

;;;; Selecting

(defun lispm-desktop-finder--select (name)
  "Select the item NAME, or none when NAME is nil, and redraw."
  (lispm-desktop-finder--in
    (unless (equal name lispm-desktop-finder--selected)
      (setq lispm-desktop-finder--selected name)
      (lispm-desktop-finder--render))))

(defun lispm-desktop-finder--step (step)
  "Select the item STEP places from the selected one."
  (lispm-desktop-finder--in
    (let* ((names (mapcar #'car lispm-desktop-finder--items))
           (index (seq-position names lispm-desktop-finder--selected)))
      (when names
        (lispm-desktop-finder--select
         (nth (max 0 (min (1- (length names)) (if index (+ index step) 0))) names))))))

(defun lispm-desktop-finder-select-right () "Select the next item." (interactive) (lispm-desktop-finder--step 1))
(defun lispm-desktop-finder-select-left () "Select the item before." (interactive) (lispm-desktop-finder--step -1))
(defun lispm-desktop-finder-select-down ()
  "Select the item below." (interactive) (lispm-desktop-finder--step (lispm-desktop-finder--in lispm-desktop-finder--columns)))
(defun lispm-desktop-finder-select-up ()
  "Select the item above." (interactive) (lispm-desktop-finder--step (- (lispm-desktop-finder--in lispm-desktop-finder--columns))))

(defun lispm-desktop-finder-type-select ()
  "Select the first item whose name begins with the letters just typed."
  (interactive)
  (lispm-desktop-finder--in
    (let* ((now (float-time))
           (text (concat (if (and lispm-desktop-finder--typed (< (- now (cdr lispm-desktop-finder--typed)) 1.0))
                             (car lispm-desktop-finder--typed)
                           "")
                         (string last-command-event))))
      (setq lispm-desktop-finder--typed (cons text now))
      (when-let* ((item (seq-find (lambda (item) (string-prefix-p text (car item) t))
                                  lispm-desktop-finder--items)))
        (lispm-desktop-finder--select (car item))))))

(defun lispm-desktop-finder--item-at (event)
  "Return the name of the item mouse EVENT is over, or nil."
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position)))
      (get-text-property point 'lispm-desktop-finder-item (window-buffer (posn-window position))))))

(defun lispm-desktop-finder-mouse-select (event)
  "Select the item pressed by EVENT, as Finder did on the press."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (lispm-desktop-finder--select (lispm-desktop-finder--item-at event)))

(defun lispm-desktop-finder-mouse-open (event)
  "Open the item double-clicked by EVENT."
  (interactive "e")
  (lispm-desktop-finder-mouse-select event)
  (lispm-desktop-finder-open))

(defun lispm-desktop-finder-deselect ()
  "Select nothing: a click between the items."
  (interactive)
  (lispm-desktop-finder--select nil))

(defun lispm-desktop-finder-context-menu (event)
  "Pop up the menu of the item right-clicked by EVENT, or of the folder."
  (interactive "e")
  (lispm-desktop-finder-mouse-select event)
  (lispm-desktop-finder--menu (lispm-desktop-popup--frame-xy event)))

(defun lispm-desktop-finder--keyboard-menu ()
  "Pop up the menu of the item selected, under its name, or of the folder,
from the keyboard, see `lispm-desktop-context-menu-function'."
  (lispm-desktop-finder--in
    (let ((item (and lispm-desktop-finder--selected
                     (text-property-any (point-min) (point-max)
                                        'lispm-desktop-finder-item lispm-desktop-finder--selected))))
      (lispm-desktop-finder--menu (lispm-desktop-popup--frame-xy (or item (window-start)))))))

(defun lispm-desktop-finder--menu (xy)
  "Pop up the menu of the item selected, or of the folder, at XY of the frame."
  (let* ((selected (lispm-desktop-finder--in lispm-desktop-finder--selected))
         (real (lispm-desktop-finder-real-p))
         (file (and selected real t)))
    (lispm-desktop-popup-menu
     `((:label "Open" :action lispm-desktop-finder-open :enabled ,(and selected t))
       separator
       (:label "New Folder…" :action lispm-desktop-finder-new-folder :enabled ,real)
       (:label "Rename…" :action lispm-desktop-finder-rename :enabled ,file)
       (:label "Move to Trash" :action lispm-desktop-finder-move-to-trash :key "M-DEL"
               :enabled ,file)
       (:label "Copy Path" :enabled ,file
               :action ,(lambda ()
                          (lispm-desktop-finder--in
                            (kill-new (expand-file-name lispm-desktop-finder--selected
                                                        lispm-desktop-finder--directory))))))
     (car xy) (cdr xy))))

(defvar-keymap lispm-desktop-finder-mode-map
  :doc "Keymap of the Finder window."
  ;; Its own parent, so special mode's single-letter keys do not take the
  ;; letters typed to select an item.
  :parent (make-sparse-keymap)
  "<down-mouse-1>" #'lispm-desktop-finder-deselect
  "<mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-desktop-finder-context-menu
  "<mouse-3>" #'ignore
  "<right>" #'lispm-desktop-finder-select-right
  "<left>" #'lispm-desktop-finder-select-left
  "<down>" #'lispm-desktop-finder-select-down
  "<up>" #'lispm-desktop-finder-select-up
  "C-f" #'lispm-desktop-finder-select-right
  "C-b" #'lispm-desktop-finder-select-left
  "C-n" #'lispm-desktop-finder-select-down
  "C-p" #'lispm-desktop-finder-select-up
  "TAB" #'lispm-desktop-finder-select-right
  "<backtab>" #'lispm-desktop-finder-select-left
  "RET" #'lispm-desktop-finder-open
  "M-o" #'lispm-desktop-finder-open
  "M-<down>" #'lispm-desktop-finder-open
  "M-<up>" #'lispm-desktop-finder-up
  "DEL" #'lispm-desktop-finder-up
  "M-[" #'lispm-desktop-finder-back
  "M-]" #'lispm-desktop-finder-forward
  "M-N" #'lispm-desktop-finder-new-folder
  "M-DEL" #'lispm-desktop-finder-move-to-trash
  "M-<backspace>" #'lispm-desktop-finder-move-to-trash
  "M-r" #'lispm-desktop-finder-refresh
  "M-w" #'lispm-desktop-finder-close
  "<escape>" #'lispm-desktop-finder-deselect
  "C-g" #'lispm-desktop-finder-deselect
  "<remap> <self-insert-command>" #'lispm-desktop-finder-type-select)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lispm-desktop-finder-mode special-mode "Finder"
  "Major mode of the Finder window: a folder's items as large icons.
\\{lispm-desktop-finder-mode-map}"
  (visual-line-mode -1)
  (setq-local cursor-type nil
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              header-line-format '(:eval (lispm-desktop-finder--header))
              mode-line-format '("" mode-line-format-right-align
                                 (:eval (lispm-desktop-platinum-mode-line-right)))
              lispm-desktop-platinum-title-menu-function #'lispm-desktop-finder--path-items
              lispm-desktop-context-menu-function #'lispm-desktop-finder--keyboard-menu
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-finder--close-box
                                                         :zoom #'lispm-desktop-finder--zoom-box
                                                         :collapse #'lispm-desktop-finder--collapse-box))
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list)
  (face-remap-add-relative 'header-line '(:box nil :underline nil :overline nil))
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (add-hook 'window-size-change-functions #'lispm-desktop-finder--render nil t))

;;;; The window

(defun lispm-desktop-finder--default-geometry (screen)
  "Return where the Finder window opens first: the left of SCREEN's desktop."
  (list 40 (+ (tab-bar-height screen t) 30)
        (min 700 (round (* (frame-inner-width screen) 0.55)))
        (min 460 (round (* (frame-inner-height screen) 0.6)))))

(defun lispm-desktop-finder--make-frame (screen)
  "Return a Finder window over SCREEN's desktop, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lispm-desktop-finder--geometry (lispm-desktop-finder--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Finder") (lispm-desktop-finder . t) (lispm-desktop-app . desktop)
                           (lispm-desktop-window . t) (top-visible . 30)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 0)
                           (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height)))))))
    (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum--color :chrome))
    (let ((window (frame-root-window frame)))
      (set-window-buffer window (lispm-desktop-finder--buffer))
      (set-window-dedicated-p window t))
    frame))

(defun lispm-desktop-finder-open-directory (directory)
  "Show DIRECTORY in the Finder window over the desktop, the desktop in front.
DIRECTORY is a directory's name, or a key of `lispm-desktop-finder-folders'."
  (interactive "DFolder: ")
  (lispm-desktop-activate)
  (unless (frame-live-p lispm-desktop-finder--frame)
    (setq lispm-desktop-finder--frame (lispm-desktop-finder--make-frame (lispm-desktop-apps-screen))))
  (lispm-desktop-finder--go directory (lispm-desktop-finder--in (and lispm-desktop-finder--directory t)))
  (make-frame-visible lispm-desktop-finder--frame)
  (lispm-desktop-apps-keep-on-desktop lispm-desktop-finder--frame)
  (raise-frame lispm-desktop-finder--frame)
  (select-frame-set-input-focus lispm-desktop-finder--frame)
  (setq lispm-desktop-finder--had-focus t)
  (lispm-desktop-finder--render))

(defun lispm-desktop-finder-home ()
  "Show the home folder in the Finder window, as the desktop's Home disk does."
  (interactive)
  (lispm-desktop-finder-open-directory "~"))

(defun lispm-desktop-finder-close ()
  "Close the Finder window."
  (interactive)
  (when (frame-live-p lispm-desktop-finder--frame)
    (lispm-desktop-finder--in (when lispm-desktop-finder--shade (lispm-desktop-finder--collapse-box nil)))
    (setq lispm-desktop-finder--geometry (lispm-desktop-apps--geometry lispm-desktop-finder--frame))
    (let ((frame lispm-desktop-finder--frame))
      (lispm-desktop-apps-focus-after frame)
      (delete-frame frame)
      (setq lispm-desktop-finder--frame nil lispm-desktop-finder--had-focus nil)))
  (when-let* ((buffer (get-buffer lispm-desktop-finder-buffer-name)))
    (kill-buffer buffer)))

(defun lispm-desktop-finder--close-box (_window)
  "Close the Finder window, from its close box."
  (lispm-desktop-finder-close))

(defun lispm-desktop-finder--zoom-box (_window)
  "Grow the Finder window over the desktop, or back, from its zoom box."
  (when (frame-live-p lispm-desktop-finder--frame)
    (let* ((frame lispm-desktop-finder--frame)
           (screen (lispm-desktop-apps-window-screen frame))
           (zoomed (frame-parameter frame 'lispm-desktop-finder-unzoom)))
      (if zoomed
          (pcase-let ((`(,left ,top ,width ,height) zoomed))
            (set-frame-parameter frame 'lispm-desktop-finder-unzoom nil)
            (set-frame-position frame left top)
            (set-frame-size frame width height t))
        (pcase-let ((`(,left . ,top) (frame-position frame)))
          (set-frame-parameter frame 'lispm-desktop-finder-unzoom
                               (list left top (frame-text-width frame) (frame-text-height frame))))
        (if (lispm-desktop-apps-out-p frame)
            ;; Out on the computer's screen, it grows over the room there.
            (pcase-let ((`(,x ,y ,w ,h) (lispm-desktop-apps--workarea frame)))
              (set-frame-position frame (+ x 12) (+ y 12))
              (set-frame-size frame (- w 40) (- h 70) t))
          (let ((top (+ (tab-bar-height screen t) 12)))
            (set-frame-position frame 12 top)
            ;; The desktop's icons stay in sight at the right.
            (set-frame-size frame (- (frame-inner-width screen) 140)
                            (- (frame-inner-height screen) top 30) t)))))))

(defun lispm-desktop-finder--collapse-box (_window)
  "Roll the Finder window up into its title bar, or down again."
  (when (frame-live-p lispm-desktop-finder--frame)
    (lispm-desktop-finder--in
      (if lispm-desktop-finder--shade
          (progn (setq header-line-format '(:eval (lispm-desktop-finder--header))
                       mode-line-format '("" mode-line-format-right-align
                                          (:eval (lispm-desktop-platinum-mode-line-right))))
                 (lispm-desktop-platinum-roll-down lispm-desktop-finder--frame lispm-desktop-finder--shade)
                 (setq lispm-desktop-finder--shade nil))
        (setq lispm-desktop-finder--shade (frame-text-height lispm-desktop-finder--frame))
        (setq header-line-format nil
              mode-line-format nil)
        (lispm-desktop-platinum-roll-up
         lispm-desktop-finder--frame
         (window-tab-line-height (frame-root-window lispm-desktop-finder--frame)))))))

;;;; With the desktop

(defun lispm-desktop-finder--follow-frontmost (app)
  "Show the Finder window with the desktop and the control panels, and with
the floating editor or browser; hide it from the full-screen editor and
browser.
This is one of `lispm-desktop-apps-frontmost-functions'."
  (when (and (frame-live-p lispm-desktop-finder--frame)
             ;; Out on the computer's screen, it stays where it is.
             (not (lispm-desktop-apps-out-p lispm-desktop-finder--frame)))
    (let ((shown (or (memq app '(desktop settings))
                     (and (eq app 'editor) (fboundp 'lispm-desktop-editor-floating-p)
                          (lispm-desktop-editor-floating-p))
                     (and (eq app 'browser) (bound-and-true-p lispm-desktop-browser-floating)))))
      (cond ((and shown (not (frame-visible-p lispm-desktop-finder--frame)))
             (make-frame-visible lispm-desktop-finder--frame)
             ;; The application in front stays over it.
             (when-let* ((front (pcase app
                                  ('editor (bound-and-true-p lispm-desktop-editor--frame))
                                  ('browser (bound-and-true-p lispm-desktop-browser--frame))))
                         ((frame-live-p front)))
               (raise-frame front)))
            ((and (not shown) (frame-visible-p lispm-desktop-finder--frame))
             (make-frame-invisible lispm-desktop-finder--frame))))))

(defun lispm-desktop-finder--note-selected (&rest _)
  "Note that the Finder window has the keyboard once it is selected; its
`lispm-desktop-app' parameter puts the desktop in front, see
`lispm-desktop-apps--follow-window'.  This is a
`window-selection-change-functions' function."
  (when (and (frame-live-p lispm-desktop-finder--frame) (eq (selected-frame) lispm-desktop-finder--frame))
    (setq lispm-desktop-finder--had-focus t)))

(defun lispm-desktop-finder--refocus ()
  "Give the Finder window the keyboard back after a menu bar menu closes,
when it had it before."
  (when (and lispm-desktop-finder--had-focus (eq (lispm-desktop-apps-frontmost) 'desktop)
             (frame-live-p lispm-desktop-finder--frame) (frame-visible-p lispm-desktop-finder--frame)
             (not (eq (selected-frame) lispm-desktop-finder--frame)))
    (select-frame-set-input-focus lispm-desktop-finder--frame)))

(defun lispm-desktop-finder-shown-p ()
  "Return non-nil while the Finder window is open."
  (frame-live-p lispm-desktop-finder--frame))

(defun lispm-desktop-finder--go-items ()
  "Return the items of the desktop's Go menu, moving the Finder window."
  (let ((open (lispm-desktop-finder-shown-p)))
    `((:label "Back" :action lispm-desktop-finder-back
              :enabled ,(and open (lispm-desktop-finder--in lispm-desktop-finder--back) t))
      (:label "Forward" :action lispm-desktop-finder-forward
              :enabled ,(and open (lispm-desktop-finder--in lispm-desktop-finder--forward) t))
      (:label "Enclosing Folder" :action lispm-desktop-finder-up :enabled ,(and open t))
      separator
      (:label "Home" :action lispm-desktop-finder-home))))

(defun lispm-desktop-setup-finder ()
  "Let the desktop's Finder window follow the application in front."
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-finder--follow-frontmost)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-finder--refocus)
  (add-hook 'window-selection-change-functions #'lispm-desktop-finder--note-selected))

(provide 'lispm-desktop-finder)
;;; lispm-desktop-finder.el ends here
