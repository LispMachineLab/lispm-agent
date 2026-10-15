;;; lispm-desktop-settings.el --- Control Panels, as Mac OS 9's  -*- lexical-binding: t; -*-

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

;; Control Panels is an application of its own, as Mac OS 9's control
;; panels were.  Its folder, opened from the desktop's Control Panels icon,
;; is a Finder window holding a panel each; the λ menu's Control Panels
;; lists them too.  A panel opens in a window floating over the desktop,
;; titled with its name, its tabs choosing among the panels as the
;; Appearance control panel's did:
;;
;;   Appearance  the colour theme
;;   Desktop     the desktop's colour, its picture and how it covers it,
;;               and Arrange Icons
;;   Keyboard    Vim or Emacs keys
;;   Internet    the browser's home page and search address, its history
;;               and its bookmarks
;;
;; Each choice takes effect at once where it can, and is saved for the next
;; start with Customize, as the menus' choices are; personal.el can still
;; set any of them.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)

(defvar lispm-desktop-color)
(defvar lispm-desktop-picture)
(defvar lispm-desktop-picture-position)
(defvar lispm-desktop-icon-places)
(defvar lispm-desktop-browser-home-url)
(defvar lispm-desktop-xwidget-search-url)
(defvar lispm-desktop-browser-history-size)
(defvar lispm-desktop-editor--frame)
(defvar lispm-desktop-browser--frame)
(defvar lispm-desktop-browser-floating)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(declare-function lispm-desktop-activate "lispm-desktop-surface" ())
(declare-function lispm-desktop-choose-picture "lispm-desktop-surface" (file))
(declare-function lispm-desktop-remove-picture "lispm-desktop-surface" ())
(declare-function lispm-desktop-arrange-icons "lispm-desktop-surface" ())
(declare-function lispm-desktop--color "lispm-desktop-surface" ())
(declare-function lispm-desktop--hex-color "lispm-desktop-surface" (color))
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(declare-function lispm-desktop-browser-floating-p "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser--load-history "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-clear-history "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-bookmarks "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-edit-bookmarks "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-xwidget-available-p "lispm-desktop-browser" ())

(defconst lispm-desktop-settings-buffer-name " *Control Panels*"
  "Name of the control panel's window's buffer.")

(defconst lispm-desktop-settings-panels
  '((themes "Appearance" lispm-desktop-settings--themes "appearance-32")
    (desktop "Desktop" lispm-desktop-settings--desktop "desktop-picture-32")
    (keyboard "Keyboard" lispm-desktop-settings--keyboard "keyboard-32")
    (browser "Internet" lispm-desktop-settings--browser "globe-32" lispm-desktop-settings--browser-p))
  "The panels, in their tabs' order: (KEY LABEL INSERT ICON [VISIBLE]).
INSERT fills the window with the panel, ICON is its large icon in the
Control Panels folder; VISIBLE, when given, is a function deciding whether
the panel shows.")

(defvar lispm-desktop-settings--frame nil
  "The control panel's window's frame, while it is open.")

(defvar lispm-desktop-settings--geometry nil
  "Where the control panel's window was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar-local lispm-desktop-settings--panel 'themes
  "The panel the control panel's window shows.")

(defun lispm-desktop-settings--browser-p ()
  "Return non-nil when the browser can run, and so has settings."
  (and (fboundp 'lispm-desktop-xwidget-available-p) (lispm-desktop-xwidget-available-p)))

(defun lispm-desktop-settings--panels ()
  "Return the panels that show here."
  (seq-filter (lambda (panel)
                (let ((visible (nth 4 panel)))
                  (or (not visible) (funcall visible))))
              lispm-desktop-settings-panels))

(defun lispm-desktop-settings-open-p ()
  "Return non-nil while the control panel's window is open."
  (frame-live-p lispm-desktop-settings--frame))

;;;; Controls
;;
;; Radio buttons and push buttons are drawn as Mac OS 9's, in the theme's
;; colours.  Both are text buttons: Tab moves between them, ringing the one
;; reached in the focus colour, and Return or Space presses it.

(defconst lispm-desktop-settings--heading-indent (propertize " " 'display '(space :width (12)))
  "The blank before a heading.")

(defconst lispm-desktop-settings--indent (propertize " " 'display '(space :width (24)))
  "The blank before a row of settings.")

(defun lispm-desktop-settings--radio-button (label on action help)
  "Insert a radio button named LABEL, ON or off, calling ACTION when pressed.
HELP describes it."
  (let ((radio (lambda (focused)
                 (lispm-desktop-platinum--image-of (lispm-desktop-platinum-radio on nil focused)))))
    (insert-text-button
     (concat (propertize " " 'display (funcall radio nil)
                         'lispm-desktop-platinum-plain (funcall radio nil)
                         'lispm-desktop-platinum-focused (funcall radio t))
             " " label)
     'action (lambda (_) (funcall action) (lispm-desktop-settings--render))
     'face 'default 'follow-link t 'help-echo help)))

(defun lispm-desktop-settings--radio (label on action &optional note)
  "Insert a radio button named LABEL, ON or off, running ACTION when pressed,
NOTE after it in the dimmed text."
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--radio-button label on action (format "Choose %s" label))
  (when note
    (insert "  " (propertize note 'face 'shadow)))
  (insert "\n"))

(defun lispm-desktop-settings--button (label action &optional help)
  "Insert a push button named LABEL running ACTION when pressed; HELP
describes it."
  (lispm-desktop-platinum-insert-push-button
   label (lambda () (funcall action) (lispm-desktop-settings--render))
   :help (or help label)))

(defun lispm-desktop-settings--heading (title)
  "Insert the heading TITLE of a group of settings.
The group runs to the next heading; once the panel is drawn, it is framed
in a group box titled TITLE, see `lispm-desktop-settings--frame-groups'."
  (insert (propertize (concat lispm-desktop-settings--heading-indent title)
                      'face 'bold 'lispm-desktop-settings-group title)
          "\n"))

(defconst lispm-desktop-settings--group-margin 12
  "Pixels between a group box and the pane's sides.")

(defun lispm-desktop-settings--frame-groups ()
  "Frame each group of settings in the current buffer in a group box, as
Mac OS 9's control panels framed theirs: its heading becomes the box's
titled top edge, each of its lines gets the box's sides, as tall as the
line, and its bottom edge closes it, a gap after it."
  (let* ((window (get-buffer-window (current-buffer) t))
         (width (if window (window-body-width window t) 500))
         (margin lispm-desktop-settings--group-margin)
         (outside (propertize " " 'display `(space :width (,margin))))
         (box (- width (* 2 margin)))
         (headings nil))
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((title (get-text-property (point) 'lispm-desktop-settings-group)))
        (push (cons (point) title) headings))
      (forward-line 1))
    ;; From the last group up, so earlier places hold.
    (let ((end (point-max)))
      (pcase-dolist (`(,start . ,title) headings)
        (save-excursion
          (goto-char end)
          (insert outside (lispm-desktop-platinum-group-edge box nil t) "\n"
                  (propertize " " 'display '(space :height (8))) "\n")
          (goto-char start)
          (forward-line 1)
          (while (< (point) end)
            (let* ((height (if window
                               ;; The newline counted in, so a blank line
                               ;; measures as tall as it shows.
                               (max 1 (cdr (window-text-pixel-size window (line-beginning-position)
                                                                   (min (1+ (line-end-position))
                                                                        (point-max)))))
                             16))
                   (left (concat outside (lispm-desktop-platinum-group-side height)))
                   (right (concat (propertize " " 'display `(space :align-to (- right (,(+ margin 2)))))
                                  (lispm-desktop-platinum-group-side height))))
              (put-text-property (line-beginning-position) (min (1+ (line-end-position)) (point-max))
                                 'line-prefix left)
              (end-of-line)
              ;; On a blank line the side is the line's first text, so it
              ;; carries the line's prefix too.
              (insert (propertize right 'line-prefix left))
              (setq end (+ end (length right)))
              (forward-line 1)))
          (goto-char start)
          (delete-region start (line-end-position))
          (insert outside (propertize (lispm-desktop-platinum-group-edge box title)
                                      'lispm-desktop-platinum-label title)))
        (setq end start)))))

(defun lispm-desktop-settings--note (text)
  "Insert TEXT, a remark about the settings above, dimmed."
  (insert lispm-desktop-settings--indent (propertize text 'face 'shadow) "\n"))

(defun lispm-desktop-settings--save (variable value)
  "Set VARIABLE to VALUE now and for every later start."
  (customize-save-variable variable value))

;;;; Panels

(defun lispm-desktop-settings--other-theme ()
  "Ask for any theme installed, and switch to it now and for later starts."
  (let ((theme (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))
                                nil t)))
    (unless (string-empty-p theme)
      (lispm-desktop-set-theme (intern theme)))))

(defun lispm-desktop-settings--themes ()
  "Insert the Themes panel: the colour theme."
  (lispm-desktop-settings--heading "Theme")
  (lispm-desktop-settings--radio "Dracula" (eq lispm-desktop-theme 'dracula)
                                 (lambda () (lispm-desktop-set-theme 'dracula)) "dark, Dracula's colours")
  (lispm-desktop-settings--radio "Platinum" (eq lispm-desktop-theme 'platinum)
                                 (lambda () (lispm-desktop-set-theme 'platinum)) "Mac OS 9's greys")
  (when (and lispm-desktop-theme (not (memq lispm-desktop-theme '(dracula platinum))))
    (lispm-desktop-settings--radio (capitalize (symbol-name lispm-desktop-theme)) t #'ignore))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Other Theme…" #'lispm-desktop-settings--other-theme
                                  "Choose any other theme installed")
  (insert "\n\n")
  (lispm-desktop-settings--note "The menu bar, the title bars and the lists take their colours")
  (lispm-desktop-settings--note "from the theme."))

(defun lispm-desktop-settings--swatch (color)
  "Return a swatch of COLOR, one icon wide."
  (propertize " " 'display (lispm-desktop-platinum--image
                            (list 'swatch color)
                            (lambda ()
                              (let ((svg (svg-create 28 14)))
                                (svg-rectangle svg 0.5 0.5 27 13 :fill color
                                               :stroke (lispm-desktop-platinum--color :text))
                                svg)))))

(defun lispm-desktop-settings--choose-color ()
  "Ask for the desktop's colour and keep it, written #RRGGBB.
`read-color' returns a colour typed by name as #RRRRGGGGBBBB, which the
desktop's SVG cannot read."
  (let ((color (lispm-desktop--hex-color (read-color "Desktop colour: " t))))
    (when color
      (lispm-desktop-settings--save 'lispm-desktop-color color))))

(defun lispm-desktop-settings--picture-preview ()
  "Insert a small view of the desktop picture."
  (when-let* ((file (and lispm-desktop-picture (expand-file-name lispm-desktop-picture)))
              ((file-readable-p file))
              ((display-images-p)))
    (insert lispm-desktop-settings--indent)
    (insert-image (create-image file nil nil :max-width 160 :max-height 100 :ascent 'center))
    (insert "\n")))

(defun lispm-desktop-settings--desktop ()
  "Insert the Desktop panel: its colour, its picture, its icons."
  (lispm-desktop-settings--heading "Colour")
  (lispm-desktop-settings--radio "From the theme" (null lispm-desktop-color)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-color nil)))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--radio-button "Your own" (and lispm-desktop-color t)
                                        #'lispm-desktop-settings--choose-color "Choose a colour")
  (insert " " (lispm-desktop-settings--swatch (lispm-desktop--color)) " ")
  (lispm-desktop-settings--button "Choose…" #'lispm-desktop-settings--choose-color "Choose the desktop's colour")
  (insert "\n")
  (lispm-desktop-settings--heading "Picture")
  (lispm-desktop-settings--picture-preview)
  (lispm-desktop-settings--note (if lispm-desktop-picture
                                    (abbreviate-file-name lispm-desktop-picture)
                                  "None: the desktop shows its colour."))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Choose…" (lambda () (call-interactively #'lispm-desktop-choose-picture))
                                  "Choose a picture for the desktop")
  (insert " ")
  (when lispm-desktop-picture
    (lispm-desktop-settings--button "None" #'lispm-desktop-remove-picture "Show the desktop's colour alone"))
  (insert "\n\n")
  (pcase-dolist (`(,position ,label ,note) '((fill "Fill" "cropped to the desktop")
                                             (fit "Fit" "whole, the colour around")
                                             (center "Center" "at its own size")
                                             (tile "Tile" "repeated")))
    (lispm-desktop-settings--radio label (eq lispm-desktop-picture-position position)
                                   (lambda () (lispm-desktop-settings--save 'lispm-desktop-picture-position position))
                                   note))
  (lispm-desktop-settings--heading "Icons")
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Arrange Icons" #'lispm-desktop-arrange-icons
                                  "Put the desktop's icons back where they stood at first")
  (insert "\n")
  (lispm-desktop-settings--note "Dragged icons land on the grid; this puts every one back."))

(defun lispm-desktop-settings--keyboard ()
  "Insert the Keyboard panel: Vim or Emacs keys."
  (lispm-desktop-settings--heading "Editing")
  (lispm-desktop-settings--radio "Vim keys" (eq lispm-desktop-editing-style 'vim)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'vim))
                                 "Evil in files; Emacs keys elsewhere")
  (lispm-desktop-settings--radio "Emacs keys" (eq lispm-desktop-editing-style 'emacs)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'emacs)))
  (insert "\n")
  (lispm-desktop-settings--note "Takes effect at the next start.")
  (when (eq system-type 'darwin)
    (lispm-desktop-settings--note "Command is Meta: Command-C and Command-V copy and paste.")))

(defun lispm-desktop-settings--change (variable prompt)
  "Ask with PROMPT for a new value of the string VARIABLE, and keep it."
  (let ((value (string-trim (read-string prompt (symbol-value variable)))))
    (unless (string-empty-p value)
      (lispm-desktop-settings--save variable value))))

(defun lispm-desktop-settings--browser ()
  "Insert the Browser panel: home page, search, history and bookmarks."
  (lispm-desktop-settings--heading "Home Page")
  (lispm-desktop-settings--note lispm-desktop-browser-home-url)
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…"
                                  (lambda () (lispm-desktop-settings--change 'lispm-desktop-browser-home-url "Home page: "))
                                  "Change the page Home goes to")
  (insert "\n")
  (lispm-desktop-settings--heading "Search")
  (lispm-desktop-settings--note lispm-desktop-xwidget-search-url)
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…"
                                  (lambda () (lispm-desktop-settings--change 'lispm-desktop-xwidget-search-url
                                                                             "Search address (%s for the words): "))
                                  "Change where what is typed in the location field is searched")
  (insert "\n")
  (lispm-desktop-settings--heading "History")
  (lispm-desktop-settings--note (format "%d pages visited are kept, of at most %d."
                                        (length (lispm-desktop-browser--load-history)) lispm-desktop-browser-history-size))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Clear History…" #'lispm-desktop-browser-clear-history
                                  "Forget every page visited")
  (insert "\n")
  (lispm-desktop-settings--heading "Bookmarks")
  (lispm-desktop-settings--note (format "%d bookmarks." (length (lispm-desktop-browser-bookmarks))))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Edit Bookmarks…" #'lispm-desktop-browser-edit-bookmarks
                                  "List the bookmarks in the browser")
  (insert "\n"))

;;;; The window

(defvar-keymap lispm-desktop-settings-tab-map
  :doc "Keymap of a panel's tab."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'lispm-desktop-settings-tab-click)

(defun lispm-desktop-settings-tab-click (event)
  "Show the panel whose tab EVENT clicked."
  (interactive "e")
  (when-let* ((string (posn-string (event-start event)))
              (panel (get-text-property (cdr string) 'lispm-desktop-settings-panel (car string))))
    (lispm-desktop-settings-select panel)))

(defun lispm-desktop-settings--tabs ()
  "Return the window's header line: a tab per panel, the shown one in front."
  (lispm-desktop-platinum-tabs
   (mapcar (pcase-lambda (`(,key ,label . ,_))
             (list label (eq key lispm-desktop-settings--panel)
                   'lispm-desktop-settings-panel key 'keymap lispm-desktop-settings-tab-map
                   'pointer 'arrow 'help-echo (format "Show %s" label)))
           (lispm-desktop-settings--panels))
   (- (window-pixel-width) (window-right-divider-width))))

(defun lispm-desktop-settings--control-index ()
  "Return how many buttons come before point's, when point is on one."
  (when (button-at (point))
    (let ((count 0) (position (point-min)))
      (while (and (setq position (next-button position)) (< (button-start position) (point)))
        (setq count (1+ count)))
      count)))

(defun lispm-desktop-settings--render ()
  "Fill the control panel's window with the panel it shows, point kept."
  (when-let* ((buffer (get-buffer lispm-desktop-settings-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (panel (or (assq lispm-desktop-settings--panel (lispm-desktop-settings--panels))
                       (car (lispm-desktop-settings--panels)))))
        (setq lispm-desktop-settings--panel (car panel))
        (let ((control (lispm-desktop-settings--control-index)))
          (erase-buffer)
          (insert (propertize " " 'display '(space :height (10))) "\n")
          (funcall (nth 2 panel))
          (lispm-desktop-settings--frame-groups)
          (goto-char (point-min))
          (if control
              ;; The keyboard stays on the control it was on.
              (dotimes (_ (1+ control)) (forward-button 1 nil nil t))
            (forward-line (1- line))))
        (setq lispm-desktop-platinum--focus nil
              lispm-desktop-platinum-title (nth 1 panel))
        (lispm-desktop-platinum-show-focus)
        (force-mode-line-update)))))

(defun lispm-desktop-settings-select (panel)
  "Show PANEL, a key of the panels, in the control panel's window.
The panels are `lispm-desktop-settings-panels'."
  (with-current-buffer (lispm-desktop-settings--buffer)
    (setq lispm-desktop-settings--panel panel)
    (goto-char (point-min))
    (lispm-desktop-settings--render)))

(defun lispm-desktop-settings-next-panel (&optional back)
  "Show the next panel, or the previous one when BACK."
  (interactive)
  (let* ((keys (mapcar #'car (lispm-desktop-settings--panels)))
         (index (or (seq-position keys lispm-desktop-settings--panel) 0)))
    (lispm-desktop-settings-select (nth (mod (+ index (if back -1 1)) (length keys)) keys))))

(defun lispm-desktop-settings-previous-panel ()
  "Show the previous panel."
  (interactive)
  (lispm-desktop-settings-next-panel t))

(defun lispm-desktop-settings--close-box (_window)
  "Close the control panel's window, from its close box."
  (lispm-desktop-settings-close))

(defvar lispm-desktop-settings--shade nil
  "The control panel's window's height while it is rolled up, or nil.")

(defun lispm-desktop-settings--collapse-box (_window)
  "Roll the control panel's window up into its title bar, or down again."
  (when (lispm-desktop-settings-open-p)
    (with-current-buffer (lispm-desktop-settings--buffer)
      (if lispm-desktop-settings--shade
          (progn (setq header-line-format '(:eval (lispm-desktop-settings--tabs)))
                 (kill-local-variable 'mode-line-format)
                 (set-frame-height lispm-desktop-settings--frame lispm-desktop-settings--shade nil t)
                 (setq lispm-desktop-settings--shade nil))
        (setq lispm-desktop-settings--shade (frame-text-height lispm-desktop-settings--frame))
        (setq header-line-format nil
              mode-line-format nil)
        (set-frame-height lispm-desktop-settings--frame
                          (window-tab-line-height (frame-root-window lispm-desktop-settings--frame))
                          nil t)))))

(defvar-local lispm-desktop-settings--width nil
  "The width in pixels the panel's group boxes were drawn for.")

(defun lispm-desktop-settings--refit (window)
  "Draw the panel again once WINDOW's width changed, its group boxes as wide
as the pane.  This is a buffer-local `window-size-change-functions' function."
  (let ((width (window-body-width window t)))
    (unless (eql width lispm-desktop-settings--width)
      (setq lispm-desktop-settings--width width)
      (lispm-desktop-settings--render))))

(defvar-keymap lispm-desktop-settings-mode-map
  :doc "Keymap of the control panel's window."
  :parent button-buffer-map
  "<right>" #'lispm-desktop-settings-next-panel
  "<left>" #'lispm-desktop-settings-previous-panel
  "C-f" #'lispm-desktop-settings-next-panel
  "C-b" #'lispm-desktop-settings-previous-panel
  "SPC" #'lispm-desktop-platinum-push
  "q" #'lispm-desktop-settings-close
  "M-w" #'lispm-desktop-settings-close)

(define-derived-mode lispm-desktop-settings-mode special-mode "Control Panels"
  "Major mode of the control panel's window: a panel of settings, a tab each.
\\{lispm-desktop-settings-mode-map}"
  (visual-line-mode -1)
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              lispm-desktop-platinum-info "Choices are kept for the next start."
              header-line-format '(:eval (lispm-desktop-settings--tabs))
              lispm-desktop-platinum-title "Control Panels"
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "control-panels")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-settings--close-box
                                                         :collapse #'lispm-desktop-settings--collapse-box))
  ;; The pane the tabs choose among, lighter than the window's platinum.
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list-body)
  (add-hook 'window-size-change-functions #'lispm-desktop-settings--refit nil t)
  ;; The tabs fill the header line, their pane's edge under them.
  (face-remap-add-relative 'header-line '(:box nil :underline nil :overline nil))
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t)
  (hl-line-mode -1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-settings--buffer ()
  "Return the control panel's window's buffer, made when missing."
  (or (get-buffer lispm-desktop-settings-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-settings-buffer-name)
        (lispm-desktop-settings-mode)
        (current-buffer))))

(defun lispm-desktop-settings--default-geometry (screen)
  "Return where the control panel's window opens first: the middle of SCREEN."
  (let ((width 520)
        (height 460))
    (list (max 20 (/ (- (frame-inner-width screen) width) 2))
          (+ (tab-bar-height screen t) 50)
          width height)))

(defun lispm-desktop-settings--make-frame (screen)
  "Return a control panel's window over SCREEN's desktop, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lispm-desktop-settings--geometry (lispm-desktop-settings--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Control Panels") (lispm-desktop-app . settings)
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
      (set-window-buffer window (lispm-desktop-settings--buffer))
      (set-window-dedicated-p window t))
    frame))

(defun lispm-desktop-settings-show (&optional panel)
  "Open the control panel's window over the desktop, showing PANEL, a key of
`lispm-desktop-settings-panels', or the panel it showed last."
  (interactive)
  ;; With the desktop hidden, the window opens out on the computer's screen.
  (unless (lispm-desktop-apps-hidden-p)
    (lispm-desktop-activate))
  (unless (lispm-desktop-settings-open-p)
    (setq lispm-desktop-settings--frame (lispm-desktop-settings--make-frame (lispm-desktop-apps-screen))))
  (if panel
      (lispm-desktop-settings-select panel)
    (lispm-desktop-settings--render))
  (make-frame-visible lispm-desktop-settings--frame)
  (lispm-desktop-apps-keep-on-desktop lispm-desktop-settings--frame)
  (raise-frame lispm-desktop-settings--frame)
  (select-frame-set-input-focus lispm-desktop-settings--frame)
  (lispm-desktop-apps-set-frontmost 'settings))

(defun lispm-desktop-settings-close ()
  "Close the control panel's window; the desktop comes in front."
  (interactive)
  (when (lispm-desktop-settings-open-p)
    (setq lispm-desktop-settings--geometry (lispm-desktop-apps--geometry lispm-desktop-settings--frame))
    (let ((frame lispm-desktop-settings--frame))
      (lispm-desktop-apps-focus-after frame)
      (delete-frame frame)
      (setq lispm-desktop-settings--frame nil)))
  (unless (lispm-desktop-apps-hidden-p)
    (lispm-desktop-apps-set-frontmost 'desktop)))

(defun lispm-desktop-settings--follow-frontmost (app)
  "Show the control panel's window with the desktop and the floating applications;
hide it from the full-screen editor and browser, as the Finder window.
This is one of `lispm-desktop-apps-frontmost-functions'."
  (when (and (lispm-desktop-settings-open-p)
             ;; Out on the computer's screen, it stays where it is.
             (not (lispm-desktop-apps-out-p lispm-desktop-settings--frame)))
    (let ((shown (or (memq app '(desktop settings))
                     (and (eq app 'editor) (fboundp 'lispm-desktop-editor-floating-p)
                          (lispm-desktop-editor-floating-p))
                     (and (eq app 'browser) (bound-and-true-p lispm-desktop-browser-floating)))))
      (cond ((and shown (not (frame-visible-p lispm-desktop-settings--frame)))
             (make-frame-visible lispm-desktop-settings--frame)
             (when-let* ((front (pcase app
                                  ('editor (bound-and-true-p lispm-desktop-editor--frame))
                                  ('browser (bound-and-true-p lispm-desktop-browser--frame))))
                         ((frame-live-p front)))
               (raise-frame front)))
            ((and (not shown) (frame-visible-p lispm-desktop-settings--frame))
             (make-frame-invisible lispm-desktop-settings--frame))))))

(defun lispm-desktop-settings--refocus ()
  "Give the control panel's window the keyboard back after a menu bar menu closes,
while Control Panels is in front."
  (when (and (eq (lispm-desktop-apps-frontmost) 'settings) (lispm-desktop-settings-open-p)
             (frame-visible-p lispm-desktop-settings--frame)
             (not (eq (selected-frame) lispm-desktop-settings--frame)))
    (select-frame-set-input-focus lispm-desktop-settings--frame)))

;;;; Menus

(defun lispm-desktop-settings-panel-items ()
  "Return a menu item opening each panel, as the λ menu's Control Panels
lists them, as Mac OS 9's Apple menu listed its control panels."
  (mapcar (pcase-lambda (`(,key ,label . ,_))
            (list :label label :action (lambda () (lispm-desktop-settings-show key))))
          (lispm-desktop-settings--panels)))

(defun lispm-desktop-settings--folder-items ()
  "Return the Control Panels folder's items, see `lispm-desktop-finder-folders'."
  (mapcar (pcase-lambda (`(,key ,label ,_ ,icon . ,_))
            (list label icon (lambda () (lispm-desktop-settings-show key))))
          (lispm-desktop-settings--panels)))

(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(defvar lispm-desktop-finder-folders)

(defun lispm-desktop-settings-open-folder ()
  "Open the Control Panels folder in the Finder window: a panel each, as
the Control Panels folder of Mac OS 9's System Folder."
  (interactive)
  (lispm-desktop-finder-open-directory 'control-panels))

(defun lispm-desktop-settings--file-items ()
  "Return the items of the Control Panels File menu."
  '((:label "Close Window" :action lispm-desktop-settings-close)))

(defun lispm-desktop-settings--view-items ()
  "Return the items of the Control Panels View menu: a panel each, the shown one
checked."
  (let ((shown (buffer-local-value 'lispm-desktop-settings--panel (lispm-desktop-settings--buffer))))
    (mapcar (pcase-lambda (`(,key ,label . ,_))
              (list :label label :action (lambda () (lispm-desktop-settings-show key))
                    :checked (eq key shown)))
            (lispm-desktop-settings--panels))))

(defun lispm-desktop-settings-menus ()
  "Return the Control Panels menus, for the menu bar while they are in front."
  '((lispm-desktop-settings-file "File" lispm-desktop-settings--file-items)
    (lispm-desktop-settings-view "View" lispm-desktop-settings--view-items)))

(defun lispm-desktop-setup-settings ()
  "Make Control Panels an application: its folder holds a panel each, and
its window follows the application in front and shows the theme's colours
when they change."
  (setf (alist-get 'control-panels lispm-desktop-finder-folders)
        '(:title "Control Panels" :icon "control-panels" :items lispm-desktop-settings--folder-items))
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-settings--follow-frontmost)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-settings--refocus)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-settings--render)) 95))

(provide 'lispm-desktop-settings)
;;; lispm-desktop-settings.el ends here
