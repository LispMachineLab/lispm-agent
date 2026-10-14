;;; lme-finder.el --- The file manager: Finder windows on the desktop  -*- lexical-binding: t; -*-

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
;; Keys, as Finder's: the arrows select, Return or Command-O opens,
;; Command-Up goes to the enclosing folder, Command-[ and Command-] back
;; and forward, Command-Shift-N makes a folder, Command-Delete moves the
;; selected item to the Trash, Command-W closes the window, and typing a
;; name's first letters selects it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lme-core)
(require 'lme-platinum)
(require 'lme-apps)
(require 'lme-desktop)

(declare-function lme-editor-activate "lme-editor" ())
(declare-function lme-editor-floating-p "lme-editor" ())
(declare-function lme-sidebar--toolbar-button "lme-sidebar" (label command help row &optional face))
(declare-function lme-popup-menu "lme-popup" (items &optional x y align))
(declare-function lme-popup--frame-xy "lme-popup" (position))
(declare-function lme-popup-alert "lme-popup" (icon message buttons &optional detail))
(defvar lme-editor--frame)
(defvar lme-browser--frame)
(defvar lme-browser-floating)

(defconst lme-finder-buffer-name " *Finder*"
  "Name of the Finder window's buffer.")

(defconst lme-finder-cell-width 100
  "Width in pixels of an item's cell in the window: its icon and its name.")

(defconst lme-finder-icon-size 32
  "Side in pixels of an item's icon.")

(defvar lme-finder--frame nil
  "The Finder window, a child frame of the screen, or nil when closed.")

(defvar lme-finder--geometry nil
  "Where the Finder window was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lme-finder--had-focus nil
  "Non-nil while the Finder window was the desktop's last focused window.")

(defvar-local lme-finder--directory nil
  "The folder the window shows.")

(defvar-local lme-finder--back nil
  "Folders shown before this one, the last first.")

(defvar-local lme-finder--forward nil
  "Folders gone back from, the nearest first.")

(defvar-local lme-finder--items nil
  "The folder's items in the order shown: (NAME . FOLDER-P).")

(defvar-local lme-finder--selected nil
  "Name of the selected item, or nil.")

(defvar-local lme-finder--columns 1
  "Items a row of the window holds, as last drawn.")

(defvar-local lme-finder--typed nil
  "(TEXT . TIME): the letters typed to select an item, and when.")

(defvar-local lme-finder--shade nil
  "While the window is rolled up: its text height.")

;;;; Items

(defun lme-finder--read (directory)
  "Return DIRECTORY's items by name, hidden ones left out: (NAME . FOLDER-P)."
  (sort (mapcar (lambda (name)
                  (cons name (file-directory-p (expand-file-name name directory))))
                (ignore-errors (directory-files directory nil "\\`[^.]" t)))
        (lambda (a b) (string-collate-lessp (car a) (car b) nil t))))

(defun lme-finder--icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY."
  (let ((path (expand-file-name (car item) directory)))
    (if (cdr item)
        (if (file-exists-p (expand-file-name ".git" path)) "folder-git-32" "folder-32")
      (let ((name (concat (lme-platinum-file-icon-name path) "-32")))
        (if (file-exists-p (lme-platinum-icon-file name)) name "document-32")))))

(defvar lme-finder--icons (make-hash-table :test #'equal)
  "Large icon images by (NAME . SELECTED).")

(defun lme-finder--icon (name selected)
  "Return the large icon NAME as a string showing it, darkened when SELECTED."
  (if (not (display-images-p))
      (propertize " " 'display `(space :width (,lme-finder-icon-size)))
    (propertize
     " " 'display
     (or (gethash (cons name selected) lme-finder--icons)
         (puthash (cons name selected)
                  (let ((svg (with-temp-buffer
                               (insert-file-contents (lme-platinum-icon-file name))
                               (buffer-string))))
                    (create-image (if selected (lme-desktop--darken svg) svg) 'svg t
                                  :height lme-finder-icon-size :ascent 'center))
                  lme-finder--icons)))))

;;;; Drawing

(defvar-keymap lme-finder-item-map
  :doc "Keymap of an item: a press selects it, a double click opens it."
  "<down-mouse-1>" #'lme-finder-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lme-finder-mouse-open
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lme-finder-context-menu
  "<mouse-3>" #'ignore)


(defface lme-finder-label '((t :inherit lme-platinum-list))
  "Face of an item's name in the Finder window."
  :group 'lme-platinum)

(defface lme-finder-button-disabled '((t :inherit lme-platinum-button))
  "Face of a toolbar button that cannot be pressed: dimmed."
  :group 'lme-platinum)

(defun lme-finder--paint (&rest _)
  "Fit the Finder's faces to the theme."
  (when lme-platinum--palette
    (set-face-attribute 'lme-finder-button-disabled nil
                        :foreground (lme-platinum--color :muted))))

(defun lme-finder--button (label command help enabled)
  "Return a toolbar button LABEL running COMMAND, described by HELP.
When not ENABLED it is drawn dimmed and does nothing."
  (if enabled
      (lme-sidebar--toolbar-button (concat " " label " ") command help 'header-line)
    (propertize (concat " " label " ") 'face 'lme-finder-button-disabled)))

(defun lme-finder--toolbar ()
  "Return the window's toolbar: back, forward, enclosing folder, New Folder."
  (let ((new (lme-finder--button "New Folder" #'lme-finder-new-folder
                                 "Make a new folder here" t)))
    (concat " "
            (lme-finder--button "◀" #'lme-finder-back "Back" lme-finder--back) " "
            (lme-finder--button "▶" #'lme-finder-forward "Forward" lme-finder--forward) " "
            (lme-finder--button "▲" #'lme-finder-up "Enclosing folder"
                                (lme-finder--parent lme-finder--directory))
            (propertize " " 'display
                        `(space :align-to (- right (,(+ (string-pixel-width new) 6)))))
            new)))

(defun lme-finder--info ()
  "Return the window's bottom line: its items, and the room left on the disk."
  (let ((count (length lme-finder--items))
        (available (nth 2 (ignore-errors (file-system-info lme-finder--directory)))))
    (concat (format "%d item%s" count (if (= count 1) "" "s"))
            (when available
              (format ", %s available" (file-size-human-readable available 'si " " "B"))))))

(defun lme-finder--title (directory)
  "Return the window's title for DIRECTORY: the folder's name."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (if (string-empty-p name) directory name)))

(defun lme-finder--cell-left (column)
  "Return the pixel left of the window's cell COLUMN."
  (+ 6 (* column lme-finder-cell-width)))

(defun lme-finder--render (&rest _)
  "Draw the folder's items as rows of large icons over their names."
  (when-let* ((buffer (get-buffer lme-finder-buffer-name))
              (window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (cell lme-finder-cell-width)
             (columns (max 1 (/ (- (window-body-width window t) 12) cell)))
             (props (lambda (name)
                      (list 'lme-finder-item name 'keymap lme-finder-item-map
                            'pointer 'arrow 'help-echo name))))
        (setq lme-finder--columns columns)
        (erase-buffer)
        (insert (propertize " " 'display '(space :height (12))) "\n")
        (dolist (row (seq-partition lme-finder--items columns))
          (seq-do-indexed
           (lambda (item column)
             (insert (propertize " " 'display
                                 `(space :align-to (,(+ (lme-finder--cell-left column)
                                                        (/ (- cell lme-finder-icon-size) 2)))))
                     (apply #'propertize
                            (lme-finder--icon (lme-finder--icon-name lme-finder--directory item)
                                              (equal (car item) lme-finder--selected))
                            (funcall props (car item)))))
           row)
          (insert "\n")
          (seq-do-indexed
           (lambda (item column)
             (let ((label (lme-platinum--fit
                           (propertize (concat " " (car item) " ")
                                       'face (if (equal (car item) lme-finder--selected)
                                                 'lme-desktop-label-selected
                                               'lme-finder-label))
                           (- cell 6))))
               (insert (propertize " " 'display
                                   `(space :align-to (,(+ (lme-finder--cell-left column)
                                                          (/ (- cell (string-pixel-width label))
                                                             2)))))
                       (apply #'propertize label (funcall props (car item))))))
           row)
          (insert "\n" (propertize " " 'display '(space :height (14))) "\n"))
        (setq lme-platinum-title (lme-finder--title lme-finder--directory)
              lme-platinum-icon (lme-platinum-folder-icon lme-finder--directory)
              lme-platinum-info (lme-finder--info))
        ;; The selected item is kept in view.
        (let ((at (or (and lme-finder--selected
                           (text-property-any (point-min) (point-max)
                                              'lme-finder-item lme-finder--selected))
                      (point-min))))
          (goto-char at)
          (set-window-point window at))
        (force-mode-line-update)))))

;;;; Going places

(defun lme-finder--buffer ()
  "Return the Finder window's buffer, made when missing."
  (or (get-buffer lme-finder-buffer-name)
      (with-current-buffer (get-buffer-create lme-finder-buffer-name)
        (lme-finder-mode)
        (current-buffer))))

(defmacro lme-finder--in (&rest body)
  "Run BODY in the Finder window's buffer."
  (declare (indent 0))
  `(with-current-buffer (lme-finder--buffer) ,@body))

(defun lme-finder--parent (directory)
  "Return the folder enclosing DIRECTORY, or nil at the top."
  (let ((parent (file-name-directory (directory-file-name directory))))
    (unless (or (null parent) (equal parent directory)) parent)))

(defun lme-finder--go (directory &optional record select)
  "Show DIRECTORY, with SELECT selected; RECORD keeps the folder left in
the history, as going there by hand does."
  (lme-finder--in
    (when (and record lme-finder--directory)
      (push lme-finder--directory lme-finder--back)
      (setq lme-finder--forward nil))
    (setq lme-finder--directory (file-name-as-directory (expand-file-name directory))
          lme-finder--items (lme-finder--read lme-finder--directory)
          lme-finder--selected select)
    (lme-finder--render)))

(defun lme-finder-back ()
  "Show the folder shown before this one."
  (interactive)
  (lme-finder--in
    (when-let* ((previous (pop lme-finder--back)))
      (push lme-finder--directory lme-finder--forward)
      (lme-finder--go previous))))

(defun lme-finder-forward ()
  "Show again the folder gone back from."
  (interactive)
  (lme-finder--in
    (when-let* ((next (pop lme-finder--forward)))
      (push lme-finder--directory lme-finder--back)
      (lme-finder--go next))))

(defun lme-finder-up ()
  "Show the enclosing folder, the folder left selected in it."
  (interactive)
  (lme-finder--in
    (when-let* ((parent (lme-finder--parent lme-finder--directory)))
      (lme-finder--go parent t (lme-finder--title lme-finder--directory)))))

(defun lme-finder-refresh ()
  "Read the folder again."
  (interactive)
  (lme-finder--in
    (lme-finder--go lme-finder--directory nil lme-finder--selected)))

(defun lme-finder--edit (file)
  "Open FILE in the editor, brought in front."
  (lme-editor-activate)
  (find-file file))

(defun lme-finder-open ()
  "Open the selected item: a folder in the window, a file in the editor."
  (interactive)
  (lme-finder--in
    (when-let* ((name lme-finder--selected))
      (let ((path (expand-file-name name lme-finder--directory)))
        (if (file-directory-p path)
            (lme-finder--go path t)
          (lme-finder--edit path))))))

(defun lme-finder-new-folder (name)
  "Make a folder called NAME in the folder shown, and select it."
  (interactive (list (read-string "Name of the new folder: " "untitled folder")))
  (lme-finder--in
    (let ((path (expand-file-name name lme-finder--directory)))
      (when (file-exists-p path)
        (user-error "There is already an item called “%s” here" name))
      (make-directory path)
      (lme-finder--go lme-finder--directory nil name))))

(defun lme-finder-move-to-trash ()
  "Move the selected item to the Trash, as Command-Delete did in Finder.
It goes to the system's own Trash, from where it can be put back; the item
after it is selected."
  (interactive)
  (lme-finder--in
    (when-let* ((name lme-finder--selected))
      (let* ((names (mapcar #'car lme-finder--items))
             (index (or (seq-position names name) 0))
             (next (or (nth (1+ index) names) (and (> index 0) (nth (1- index) names)))))
        (condition-case err
            (progn
              (move-file-to-trash (expand-file-name name lme-finder--directory))
              (lme-desktop-check-trash)
              (lme-finder--go lme-finder--directory nil next)
              (message "“%s” was moved to the Trash" name))
          (error
           (lme-popup-alert 'stop (format "“%s” could not be moved to the Trash." name)
                            '(("OK" ok default))
                            (error-message-string err))))))))

(defun lme-finder-rename (name)
  "Rename the selected item to NAME."
  (interactive (list (read-string "Rename to: " (lme-finder--in lme-finder--selected))))
  (lme-finder--in
    (when lme-finder--selected
      (rename-file (expand-file-name lme-finder--selected lme-finder--directory)
                   (expand-file-name name lme-finder--directory))
      (lme-finder--go lme-finder--directory nil name))))

;;;; Selecting

(defun lme-finder--select (name)
  "Select the item NAME, or none when NAME is nil, and redraw."
  (lme-finder--in
    (unless (equal name lme-finder--selected)
      (setq lme-finder--selected name)
      (lme-finder--render))))

(defun lme-finder--step (step)
  "Select the item STEP places from the selected one."
  (lme-finder--in
    (let* ((names (mapcar #'car lme-finder--items))
           (index (seq-position names lme-finder--selected)))
      (when names
        (lme-finder--select
         (nth (max 0 (min (1- (length names)) (if index (+ index step) 0))) names))))))

(defun lme-finder-select-right () "Select the next item." (interactive) (lme-finder--step 1))
(defun lme-finder-select-left () "Select the item before." (interactive) (lme-finder--step -1))
(defun lme-finder-select-down ()
  "Select the item below." (interactive) (lme-finder--step (lme-finder--in lme-finder--columns)))
(defun lme-finder-select-up ()
  "Select the item above." (interactive) (lme-finder--step (- (lme-finder--in lme-finder--columns))))

(defun lme-finder-type-select ()
  "Select the first item whose name begins with the letters just typed."
  (interactive)
  (lme-finder--in
    (let* ((now (float-time))
           (text (concat (if (and lme-finder--typed (< (- now (cdr lme-finder--typed)) 1.0))
                             (car lme-finder--typed)
                           "")
                         (string last-command-event))))
      (setq lme-finder--typed (cons text now))
      (when-let* ((item (seq-find (lambda (item) (string-prefix-p text (car item) t))
                                  lme-finder--items)))
        (lme-finder--select (car item))))))

(defun lme-finder--item-at (event)
  "Return the name of the item mouse EVENT is over, or nil."
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position)))
      (get-text-property point 'lme-finder-item (window-buffer (posn-window position))))))

(defun lme-finder-mouse-select (event)
  "Select the item pressed by EVENT, as Finder did on the press."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (lme-finder--select (lme-finder--item-at event)))

(defun lme-finder-mouse-open (event)
  "Open the item double-clicked by EVENT."
  (interactive "e")
  (lme-finder-mouse-select event)
  (lme-finder-open))

(defun lme-finder-deselect ()
  "Select nothing: a click between the items."
  (interactive)
  (lme-finder--select nil))

(defun lme-finder-context-menu (event)
  "Pop up the menu of the item right-clicked by EVENT, or of the folder."
  (interactive "e")
  (lme-finder-mouse-select event)
  (let ((selected (lme-finder--in lme-finder--selected)))
    (lme-popup-menu
     `((:label "Open" :action lme-finder-open :enabled ,(and selected t))
       separator
       (:label "New Folder…" :action lme-finder-new-folder)
       (:label "Rename…" :action lme-finder-rename :enabled ,(and selected t))
       (:label "Move to Trash" :action lme-finder-move-to-trash :key "M-DEL"
               :enabled ,(and selected t))
       (:label "Copy Path" :enabled ,(and selected t)
               :action ,(lambda ()
                          (lme-finder--in
                            (kill-new (expand-file-name lme-finder--selected
                                                        lme-finder--directory))))))
     (car (lme-popup--frame-xy event)) (cdr (lme-popup--frame-xy event)))))

(defvar-keymap lme-finder-mode-map
  :doc "Keymap of the Finder window."
  ;; Its own parent, so special mode's single-letter keys do not take the
  ;; letters typed to select an item.
  :parent (make-sparse-keymap)
  "<down-mouse-1>" #'lme-finder-deselect
  "<mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lme-finder-context-menu
  "<mouse-3>" #'ignore
  "<right>" #'lme-finder-select-right
  "<left>" #'lme-finder-select-left
  "<down>" #'lme-finder-select-down
  "<up>" #'lme-finder-select-up
  "TAB" #'lme-finder-select-right
  "<backtab>" #'lme-finder-select-left
  "RET" #'lme-finder-open
  "M-o" #'lme-finder-open
  "M-<down>" #'lme-finder-open
  "M-<up>" #'lme-finder-up
  "M-[" #'lme-finder-back
  "M-]" #'lme-finder-forward
  "M-N" #'lme-finder-new-folder
  "M-DEL" #'lme-finder-move-to-trash
  "M-<backspace>" #'lme-finder-move-to-trash
  "M-r" #'lme-finder-refresh
  "M-w" #'lme-finder-close
  "<escape>" #'lme-finder-deselect
  "<remap> <self-insert-command>" #'lme-finder-type-select)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lme-finder-mode special-mode "Finder"
  "Major mode of the Finder window: a folder's items as large icons.
\\{lme-finder-mode-map}"
  (visual-line-mode -1)
  (setq-local cursor-type nil
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              header-line-format '(:eval (lme-finder--toolbar))
              lme-platinum-box-functions (list :close #'lme-finder--close-box
                                               :zoom #'lme-finder--zoom-box
                                               :collapse #'lme-finder--collapse-box))
  (face-remap-add-relative 'default 'lme-platinum-list)
  (face-remap-add-relative 'header-line 'lme-platinum-header)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (add-hook 'window-size-change-functions #'lme-finder--render nil t))

;;;; The window

(defun lme-finder--default-geometry (screen)
  "Return where the Finder window opens first: the left of SCREEN's desktop."
  (list 40 (+ (tab-bar-height screen t) 30)
        (min 700 (round (* (frame-inner-width screen) 0.55)))
        (min 460 (round (* (frame-inner-height screen) 0.6)))))

(defun lme-finder--make-frame (screen)
  "Return a Finder window over SCREEN's desktop, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lme-finder--geometry (lme-finder--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Finder") (lme-finder . t) (lme-app . desktop)
                           (lme-window . t) (top-visible . 30)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 2) (child-frame-border-width . 1)
                           (drag-internal-border . t)
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height)))))))
    (set-face-attribute 'internal-border frame :background (lme-platinum--color :chrome))
    (let ((window (frame-root-window frame)))
      (set-window-buffer window (lme-finder--buffer))
      (set-window-dedicated-p window t))
    frame))

(defun lme-finder-open-directory (directory)
  "Show DIRECTORY in the Finder window over the desktop, the desktop in front."
  (interactive "DFolder: ")
  (lme-desktop-activate)
  (unless (frame-live-p lme-finder--frame)
    (setq lme-finder--frame (lme-finder--make-frame (lme-apps-screen))))
  (lme-finder--go directory (lme-finder--in (and lme-finder--directory t)))
  (make-frame-visible lme-finder--frame)
  (lme-apps-keep-on-desktop lme-finder--frame)
  (raise-frame lme-finder--frame)
  (select-frame-set-input-focus lme-finder--frame)
  (setq lme-finder--had-focus t)
  (lme-finder--render))

(defun lme-finder-home ()
  "Show the home folder in the Finder window, as the desktop's Home disk does."
  (interactive)
  (lme-finder-open-directory "~"))

(defun lme-finder-close ()
  "Close the Finder window."
  (interactive)
  (when (frame-live-p lme-finder--frame)
    (lme-finder--in (when lme-finder--shade (lme-finder--collapse-box nil)))
    (pcase-let ((`(,left . ,top) (frame-position lme-finder--frame)))
      (setq lme-finder--geometry (list left top (frame-text-width lme-finder--frame)
                                       (frame-text-height lme-finder--frame))))
    (let ((screen (frame-parent lme-finder--frame)))
      (delete-frame lme-finder--frame)
      (setq lme-finder--frame nil lme-finder--had-focus nil)
      (select-frame-set-input-focus screen)))
  (when-let* ((buffer (get-buffer lme-finder-buffer-name)))
    (kill-buffer buffer)))

(defun lme-finder--close-box (_window)
  "Close the Finder window, from its close box."
  (lme-finder-close))

(defun lme-finder--zoom-box (_window)
  "Grow the Finder window over the desktop, or back, from its zoom box."
  (when (frame-live-p lme-finder--frame)
    (let* ((frame lme-finder--frame)
           (screen (frame-parent frame))
           (zoomed (frame-parameter frame 'lme-finder-unzoom)))
      (if zoomed
          (pcase-let ((`(,left ,top ,width ,height) zoomed))
            (set-frame-parameter frame 'lme-finder-unzoom nil)
            (set-frame-position frame left top)
            (set-frame-size frame width height t))
        (pcase-let ((`(,left . ,top) (frame-position frame)))
          (set-frame-parameter frame 'lme-finder-unzoom
                               (list left top (frame-text-width frame) (frame-text-height frame))))
        (let ((top (+ (tab-bar-height screen t) 12)))
          (set-frame-position frame 12 top)
          ;; The desktop's icons stay in sight at the right.
          (set-frame-size frame (- (frame-inner-width screen) 140)
                          (- (frame-inner-height screen) top 30) t))))))

(defun lme-finder--collapse-box (_window)
  "Roll the Finder window up into its title bar, or down again."
  (when (frame-live-p lme-finder--frame)
    (lme-finder--in
      (if lme-finder--shade
          (progn (setq header-line-format '(:eval (lme-finder--toolbar)))
                 (kill-local-variable 'mode-line-format)
                 (set-frame-height lme-finder--frame lme-finder--shade nil t)
                 (setq lme-finder--shade nil))
        (setq lme-finder--shade (frame-text-height lme-finder--frame))
        (setq header-line-format nil
              mode-line-format nil)
        (set-frame-height lme-finder--frame
                          (window-tab-line-height (frame-root-window lme-finder--frame))
                          nil t)))))

;;;; With the desktop

(defun lme-finder--follow-frontmost (app)
  "Show the Finder window with the desktop, and with the floating editor or
browser; hide it from the full-screen editor and browser.
This is one of `lme-apps-frontmost-functions'."
  (when (frame-live-p lme-finder--frame)
    (let ((shown (or (eq app 'desktop)
                     (and (eq app 'editor) (fboundp 'lme-editor-floating-p)
                          (lme-editor-floating-p))
                     (and (eq app 'browser) (bound-and-true-p lme-browser-floating)))))
      (cond ((and shown (not (frame-visible-p lme-finder--frame)))
             (make-frame-visible lme-finder--frame)
             ;; The application in front stays over it.
             (when-let* ((front (pcase app
                                  ('editor (bound-and-true-p lme-editor--frame))
                                  ('browser (bound-and-true-p lme-browser--frame))))
                         ((frame-live-p front)))
               (raise-frame front)))
            ((and (not shown) (frame-visible-p lme-finder--frame))
             (make-frame-invisible lme-finder--frame))))))

(defun lme-finder--note-selected (&rest _)
  "Note that the Finder window has the keyboard once it is selected; its
`lme-app' parameter puts the desktop in front, see `lme-apps--follow-window'.
This is a `window-selection-change-functions' function."
  (when (and (frame-live-p lme-finder--frame) (eq (selected-frame) lme-finder--frame))
    (setq lme-finder--had-focus t)))

(defun lme-finder--refocus ()
  "Give the Finder window the keyboard back after a menu bar menu closes,
when it had it before."
  (when (and lme-finder--had-focus (eq (lme-apps-frontmost) 'desktop)
             (frame-live-p lme-finder--frame) (frame-visible-p lme-finder--frame)
             (not (eq (selected-frame) lme-finder--frame)))
    (select-frame-set-input-focus lme-finder--frame)))

(defun lme-finder-shown-p ()
  "Return non-nil while the Finder window is open."
  (frame-live-p lme-finder--frame))

(defun lme-finder--go-items ()
  "Return the items of the desktop's Go menu, moving the Finder window."
  (let ((open (lme-finder-shown-p)))
    `((:label "Back" :action lme-finder-back
              :enabled ,(and open (lme-finder--in lme-finder--back) t))
      (:label "Forward" :action lme-finder-forward
              :enabled ,(and open (lme-finder--in lme-finder--forward) t))
      (:label "Enclosing Folder" :action lme-finder-up :enabled ,(and open t))
      separator
      (:label "Home" :action lme-finder-home))))

(defun lme-setup-finder ()
  "Let the desktop's Finder window follow the application in front."
  (add-hook 'lme-apps-frontmost-functions #'lme-finder--follow-frontmost)
  (add-hook 'lme-platinum-menu-closed-hook #'lme-finder--refocus)
  (add-hook 'enable-theme-functions #'lme-finder--paint 90)
  (lme-finder--paint)
  (add-hook 'window-selection-change-functions #'lme-finder--note-selected))

(provide 'lme-finder)
;;; lme-finder.el ends here
