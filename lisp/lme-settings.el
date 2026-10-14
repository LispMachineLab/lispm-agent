;;; lme-settings.el --- Settings, as Mac OS 9's control panels  -*- lexical-binding: t; -*-

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

;; Settings is an application of its own, as Mac OS 9's control panels
;; were: a window floating over the desktop, opened from the λ menu's
;; Settings, its tabs choosing a panel as the Appearance control panel's
;; did:
;;
;;   Themes     the colour theme
;;   Desktop    the desktop's colour, its picture and how it covers it,
;;              and Arrange Icons
;;   Keyboard   Vim or Emacs keys
;;   Browser    the home page, the search address, the history and the
;;              bookmarks
;;
;; Each choice takes effect at once where it can, and is saved for the next
;; start with Customize, as the menus' choices are; personal.el can still
;; set any of them.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lme-core)
(require 'lme-platinum)
(require 'lme-apps)

(defvar lme-desktop-color)
(defvar lme-desktop-picture)
(defvar lme-desktop-picture-position)
(defvar lme-desktop-icon-places)
(defvar lme-browser-home-url)
(defvar lme-xwidget-search-url)
(defvar lme-browser-history-size)
(defvar lme-editor--frame)
(defvar lme-browser--frame)
(defvar lme-browser-floating)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(declare-function lme-desktop-activate "lme-desktop" ())
(declare-function lme-desktop-choose-picture "lme-desktop" (file))
(declare-function lme-desktop-remove-picture "lme-desktop" ())
(declare-function lme-desktop-arrange-icons "lme-desktop" ())
(declare-function lme-desktop--color "lme-desktop" ())
(declare-function lme-editor-floating-p "lme-editor" ())
(declare-function lme-browser-floating-p "lme-browser" ())
(declare-function lme-browser--load-history "lme-browser-places" ())
(declare-function lme-browser-clear-history "lme-browser-places" ())
(declare-function lme-browser-bookmarks "lme-browser-places" ())
(declare-function lme-browser-edit-bookmarks "lme-browser-places" ())
(declare-function lme-xwidget-available-p "lme-browser" ())

(defconst lme-settings-buffer-name " *Settings*"
  "Name of the Settings window's buffer.")

(defconst lme-settings-panels
  '((themes "Themes" lme-settings--themes)
    (desktop "Desktop" lme-settings--desktop)
    (keyboard "Keyboard" lme-settings--keyboard)
    (browser "Browser" lme-settings--browser lme-settings--browser-p))
  "The panels, in their tabs' order: (KEY LABEL INSERT [VISIBLE]).
INSERT fills the window with the panel; VISIBLE, when given, is a
function deciding whether the panel shows.")

(defvar lme-settings--frame nil
  "The Settings window's frame, while it is open.")

(defvar lme-settings--geometry nil
  "Where the Settings window was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar-local lme-settings--panel 'themes
  "The panel the Settings window shows.")

(defun lme-settings--browser-p ()
  "Return non-nil when the browser can run, and so has settings."
  (and (fboundp 'lme-xwidget-available-p) (lme-xwidget-available-p)))

(defun lme-settings--panels ()
  "Return the panels that show here."
  (seq-filter (lambda (panel)
                (let ((visible (nth 3 panel)))
                  (or (not visible) (funcall visible))))
              lme-settings-panels))

(defun lme-settings-open-p ()
  "Return non-nil while the Settings window is open."
  (frame-live-p lme-settings--frame))

;;;; Controls
;;
;; Radio buttons are drawn as Mac OS 9's, in the theme's colours; push
;; buttons are bevel buttons.  Both are text buttons: Tab moves between
;; them and Return presses the one at point.

(defconst lme-settings--heading-indent (propertize " " 'display '(space :width (12)))
  "The blank before a heading.")

(defconst lme-settings--indent (propertize " " 'display '(space :width (24)))
  "The blank before a row of settings.")

(defun lme-settings--draw-radio (on)
  "Return an SVG of a radio button, ON or off."
  (let ((svg (svg-create 14 14)))
    (svg-circle svg 7 7 5.5 :fill (lme-platinum--color :content)
                :stroke (lme-platinum--color :text) :stroke-width 1)
    (when on
      (svg-circle svg 7 7 2.5 :fill (lme-platinum--color :accent)))
    svg))

(defun lme-settings--radio (label on action &optional note)
  "Insert a radio button named LABEL, ON or off, running ACTION when pressed,
NOTE after it in the dimmed text."
  (insert lme-settings--indent)
  (insert-text-button
   (concat (propertize " " 'display (lme-platinum--image (list 'radio on)
                                                         (lambda () (lme-settings--draw-radio on))))
           " " label)
   'action (lambda (_) (funcall action) (lme-settings--render))
   'face 'default 'mouse-face 'highlight 'follow-link t
   'help-echo (format "Choose %s" label))
  (when note
    (insert "  " (propertize note 'face 'shadow)))
  (insert "\n"))

(defun lme-settings--button (label action &optional help)
  "Insert a bevel button named LABEL running ACTION when pressed; HELP
describes it."
  (insert-text-button (concat " " label " ")
                      'action (lambda (_) (funcall action) (lme-settings--render))
                      'face 'lme-platinum-button 'mouse-face 'lme-platinum-button-pressed
                      'follow-link t 'help-echo (or help label)))

(defun lme-settings--heading (title)
  "Insert the heading TITLE of a group of settings."
  (insert "\n" lme-settings--heading-indent (propertize title 'face 'bold) "\n"))

(defun lme-settings--note (text)
  "Insert TEXT, a remark about the settings above, dimmed."
  (insert lme-settings--indent (propertize text 'face 'shadow) "\n"))

(defun lme-settings--save (variable value)
  "Set VARIABLE to VALUE now and for every later start."
  (customize-save-variable variable value))

;;;; Panels

(defun lme-settings--other-theme ()
  "Ask for any theme installed, and switch to it now and for later starts."
  (let ((theme (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))
                                nil t)))
    (unless (string-empty-p theme)
      (lme-set-theme (intern theme)))))

(defun lme-settings--themes ()
  "Insert the Themes panel: the colour theme."
  (lme-settings--heading "Theme")
  (lme-settings--radio "Dracula" (eq lme-theme 'dracula)
                       (lambda () (lme-set-theme 'dracula)) "dark, Dracula's colours")
  (lme-settings--radio "Platinum" (eq lme-theme 'platinum)
                       (lambda () (lme-set-theme 'platinum)) "Mac OS 9's greys")
  (when (and lme-theme (not (memq lme-theme '(dracula platinum))))
    (lme-settings--radio (capitalize (symbol-name lme-theme)) t #'ignore))
  (insert lme-settings--indent)
  (lme-settings--button "Other Theme…" #'lme-settings--other-theme
                        "Choose any other theme installed")
  (insert "\n\n")
  (lme-settings--note "The menu bar, the title bars and the lists take their colours")
  (lme-settings--note "from the theme."))

(defun lme-settings--swatch (color)
  "Return a swatch of COLOR, one icon wide."
  (propertize " " 'display (lme-platinum--image
                            (list 'swatch color)
                            (lambda ()
                              (let ((svg (svg-create 28 14)))
                                (svg-rectangle svg 0.5 0.5 27 13 :fill color
                                               :stroke (lme-platinum--color :text))
                                svg)))))

(defun lme-settings--choose-color ()
  "Ask for the desktop's colour and keep it."
  (let ((color (read-color "Desktop colour: " t)))
    (unless (string-empty-p color)
      (lme-settings--save 'lme-desktop-color color))))

(defun lme-settings--picture-preview ()
  "Insert a small view of the desktop picture."
  (when-let* ((file (and lme-desktop-picture (expand-file-name lme-desktop-picture)))
              ((file-readable-p file))
              ((display-images-p)))
    (insert lme-settings--indent)
    (insert-image (create-image file nil nil :max-width 160 :max-height 100 :ascent 'center))
    (insert "\n")))

(defun lme-settings--desktop ()
  "Insert the Desktop panel: its colour, its picture, its icons."
  (lme-settings--heading "Colour")
  (lme-settings--radio "From the theme" (null lme-desktop-color)
                       (lambda () (lme-settings--save 'lme-desktop-color nil)))
  (insert lme-settings--indent)
  (insert-text-button
   (concat (propertize " " 'display (lme-platinum--image
                                     (list 'radio (and lme-desktop-color t))
                                     (lambda () (lme-settings--draw-radio
                                                 (and lme-desktop-color t)))))
           " Your own ")
   'action (lambda (_) (lme-settings--choose-color) (lme-settings--render))
   'face 'default 'mouse-face 'highlight 'follow-link t 'help-echo "Choose a colour")
  (insert (lme-settings--swatch (lme-desktop--color)) " ")
  (lme-settings--button "Choose…" #'lme-settings--choose-color "Choose the desktop's colour")
  (insert "\n")
  (lme-settings--heading "Picture")
  (lme-settings--picture-preview)
  (lme-settings--note (if lme-desktop-picture
                          (abbreviate-file-name lme-desktop-picture)
                        "None: the desktop shows its colour."))
  (insert lme-settings--indent)
  (lme-settings--button "Choose…" (lambda () (call-interactively #'lme-desktop-choose-picture))
                        "Choose a picture for the desktop")
  (insert " ")
  (when lme-desktop-picture
    (lme-settings--button "None" #'lme-desktop-remove-picture "Show the desktop's colour alone"))
  (insert "\n\n")
  (pcase-dolist (`(,position ,label ,note) '((fill "Fill" "cropped to the desktop")
                                             (fit "Fit" "whole, the colour around")
                                             (center "Center" "at its own size")
                                             (tile "Tile" "repeated")))
    (lme-settings--radio label (eq lme-desktop-picture-position position)
                         (lambda () (lme-settings--save 'lme-desktop-picture-position position))
                         note))
  (lme-settings--heading "Icons")
  (insert lme-settings--indent)
  (lme-settings--button "Arrange Icons" #'lme-desktop-arrange-icons
                        "Put the desktop's icons back where they stood at first")
  (insert "\n")
  (lme-settings--note "Dragged icons land on the grid; this puts every one back."))

(defun lme-settings--keyboard ()
  "Insert the Keyboard panel: Vim or Emacs keys."
  (lme-settings--heading "Editing")
  (lme-settings--radio "Vim keys" (eq lme-editing-style 'vim)
                       (lambda () (lme-settings--save 'lme-editing-style 'vim))
                       "Evil in files; Emacs keys elsewhere")
  (lme-settings--radio "Emacs keys" (eq lme-editing-style 'emacs)
                       (lambda () (lme-settings--save 'lme-editing-style 'emacs)))
  (insert "\n")
  (lme-settings--note "Takes effect at the next start.")
  (when (eq system-type 'darwin)
    (lme-settings--note "Command is Meta: Command-C and Command-V copy and paste.")))

(defun lme-settings--change (variable prompt)
  "Ask with PROMPT for a new value of the string VARIABLE, and keep it."
  (let ((value (string-trim (read-string prompt (symbol-value variable)))))
    (unless (string-empty-p value)
      (lme-settings--save variable value))))

(defun lme-settings--browser ()
  "Insert the Browser panel: home page, search, history and bookmarks."
  (lme-settings--heading "Home Page")
  (lme-settings--note lme-browser-home-url)
  (insert lme-settings--indent)
  (lme-settings--button "Change…"
                        (lambda () (lme-settings--change 'lme-browser-home-url "Home page: "))
                        "Change the page Home goes to")
  (insert "\n")
  (lme-settings--heading "Search")
  (lme-settings--note lme-xwidget-search-url)
  (insert lme-settings--indent)
  (lme-settings--button "Change…"
                        (lambda () (lme-settings--change 'lme-xwidget-search-url
                                                         "Search address (%s for the words): "))
                        "Change where what is typed in the location field is searched")
  (insert "\n")
  (lme-settings--heading "History")
  (lme-settings--note (format "%d pages visited are kept, of at most %d."
                              (length (lme-browser--load-history)) lme-browser-history-size))
  (insert lme-settings--indent)
  (lme-settings--button "Clear History…" #'lme-browser-clear-history
                        "Forget every page visited")
  (insert "\n")
  (lme-settings--heading "Bookmarks")
  (lme-settings--note (format "%d bookmarks." (length (lme-browser-bookmarks))))
  (insert lme-settings--indent)
  (lme-settings--button "Edit Bookmarks…" #'lme-browser-edit-bookmarks
                        "List the bookmarks in the browser")
  (insert "\n"))

;;;; The window

(defvar-keymap lme-settings-tab-map
  :doc "Keymap of a panel's tab."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'lme-settings-tab-click)

(defun lme-settings-tab-click (event)
  "Show the panel whose tab EVENT clicked."
  (interactive "e")
  (when-let* ((string (posn-string (event-start event)))
              (panel (get-text-property (cdr string) 'lme-settings-panel (car string))))
    (lme-settings-select panel)))

(defun lme-settings--tabs ()
  "Return the window's header line: a bevel tab per panel, the shown one
pushed in."
  (concat
   (propertize " " 'display '(space :width (8) :height (28) :ascent 65))
   (mapconcat (pcase-lambda (`(,key ,label . ,_))
                (let ((tab (concat " " label " ")))
                  (add-face-text-property 0 (length tab)
                                          (if (eq key lme-settings--panel)
                                              'lme-platinum-tab-selected
                                            'lme-platinum-tab)
                                          t tab)
                  (propertize tab 'lme-settings-panel key 'keymap lme-settings-tab-map
                              'pointer 'arrow 'help-echo (format "Show %s" label))))
              (lme-settings--panels) " ")))

(defun lme-settings--render ()
  "Fill the Settings window with the panel it shows, point kept."
  (when-let* ((buffer (get-buffer lme-settings-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (panel (or (assq lme-settings--panel (lme-settings--panels))
                       (car (lme-settings--panels)))))
        (setq lme-settings--panel (car panel))
        (erase-buffer)
        (funcall (nth 2 panel))
        (goto-char (point-min))
        (forward-line (1- line))
        (setq lme-platinum-title (format "Settings — %s" (nth 1 panel)))
        (force-mode-line-update)))))

(defun lme-settings-select (panel)
  "Show PANEL, a key of `lme-settings-panels', in the Settings window."
  (with-current-buffer (lme-settings--buffer)
    (setq lme-settings--panel panel)
    (goto-char (point-min))
    (lme-settings--render)))

(defun lme-settings-next-panel (&optional back)
  "Show the next panel, or the previous one when BACK."
  (interactive)
  (let* ((keys (mapcar #'car (lme-settings--panels)))
         (index (or (seq-position keys lme-settings--panel) 0)))
    (lme-settings-select (nth (mod (+ index (if back -1 1)) (length keys)) keys))))

(defun lme-settings-previous-panel ()
  "Show the previous panel."
  (interactive)
  (lme-settings-next-panel t))

(defun lme-settings--close-box (_window)
  "Close the Settings window, from its close box."
  (lme-settings-close))

(defvar-keymap lme-settings-mode-map
  :doc "Keymap of the Settings window."
  :parent button-buffer-map
  "<right>" #'lme-settings-next-panel
  "<left>" #'lme-settings-previous-panel
  "q" #'lme-settings-close
  "M-w" #'lme-settings-close)

(define-derived-mode lme-settings-mode special-mode "Settings"
  "Major mode of the Settings window: a panel of settings, a tab each.
\\{lme-settings-mode-map}"
  (visual-line-mode -1)
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              lme-platinum-info "Choices are kept for the next start."
              header-line-format '(:eval (lme-settings--tabs))
              lme-platinum-title "Settings"
              lme-platinum-icon (lme-platinum-icon "control-panels")
              lme-platinum-box-functions (list :close #'lme-settings--close-box))
  (face-remap-add-relative 'default 'lme-platinum-list)
  (face-remap-add-relative 'header-line 'lme-platinum-header)
  (hl-line-mode -1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lme-settings--buffer ()
  "Return the Settings window's buffer, made when missing."
  (or (get-buffer lme-settings-buffer-name)
      (with-current-buffer (get-buffer-create lme-settings-buffer-name)
        (lme-settings-mode)
        (current-buffer))))

(defun lme-settings--default-geometry (screen)
  "Return where the Settings window opens first: the middle of SCREEN."
  (let ((width 520)
        (height 460))
    (list (max 20 (/ (- (frame-inner-width screen) width) 2))
          (+ (tab-bar-height screen t) 50)
          width height)))

(defun lme-settings--make-frame (screen)
  "Return a Settings window over SCREEN's desktop, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lme-settings--geometry (lme-settings--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . "Settings") (lme-app . settings)
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
      (set-window-buffer window (lme-settings--buffer))
      (set-window-dedicated-p window t))
    frame))

(defun lme-settings-show (&optional panel)
  "Open the Settings window over the desktop, showing PANEL, a key of
`lme-settings-panels', or the panel it showed last."
  (interactive)
  (lme-desktop-activate)
  (unless (lme-settings-open-p)
    (setq lme-settings--frame (lme-settings--make-frame (lme-apps-screen))))
  (if panel
      (lme-settings-select panel)
    (lme-settings--render))
  (make-frame-visible lme-settings--frame)
  (lme-apps-keep-on-desktop lme-settings--frame)
  (raise-frame lme-settings--frame)
  (select-frame-set-input-focus lme-settings--frame)
  (lme-apps-set-frontmost 'settings))

(defun lme-settings-close ()
  "Close the Settings window; the desktop comes in front."
  (interactive)
  (when (lme-settings-open-p)
    (pcase-let ((`(,left . ,top) (frame-position lme-settings--frame)))
      (setq lme-settings--geometry (list left top (frame-text-width lme-settings--frame)
                                         (frame-text-height lme-settings--frame))))
    (let ((screen (frame-parent lme-settings--frame)))
      (delete-frame lme-settings--frame)
      (setq lme-settings--frame nil)
      (select-frame-set-input-focus screen)))
  (lme-apps-set-frontmost 'desktop))

(defun lme-settings--follow-frontmost (app)
  "Show the Settings window with the desktop and the floating applications;
hide it from the full-screen editor and browser, as the Finder window.
This is one of `lme-apps-frontmost-functions'."
  (when (lme-settings-open-p)
    (let ((shown (or (memq app '(desktop settings))
                     (and (eq app 'editor) (fboundp 'lme-editor-floating-p)
                          (lme-editor-floating-p))
                     (and (eq app 'browser) (bound-and-true-p lme-browser-floating)))))
      (cond ((and shown (not (frame-visible-p lme-settings--frame)))
             (make-frame-visible lme-settings--frame)
             (when-let* ((front (pcase app
                                  ('editor (bound-and-true-p lme-editor--frame))
                                  ('browser (bound-and-true-p lme-browser--frame))))
                         ((frame-live-p front)))
               (raise-frame front)))
            ((and (not shown) (frame-visible-p lme-settings--frame))
             (make-frame-invisible lme-settings--frame))))))

(defun lme-settings--refocus ()
  "Give the Settings window the keyboard back after a menu bar menu closes,
while Settings is in front."
  (when (and (eq (lme-apps-frontmost) 'settings) (lme-settings-open-p)
             (frame-visible-p lme-settings--frame)
             (not (eq (selected-frame) lme-settings--frame)))
    (select-frame-set-input-focus lme-settings--frame)))

;;;; Menus

(defun lme-settings-panel-items ()
  "Return a menu item opening each panel, as the λ menu's Settings lists them."
  (mapcar (pcase-lambda (`(,key ,label . ,_))
            (list :label (concat label "…") :action (lambda () (lme-settings-show key))))
          (lme-settings--panels)))

(defun lme-settings--file-items ()
  "Return the items of the Settings File menu."
  '((:label "Close Window" :action lme-settings-close)))

(defun lme-settings--view-items ()
  "Return the items of the Settings View menu: a panel each, the shown one
checked."
  (let ((shown (buffer-local-value 'lme-settings--panel (lme-settings--buffer))))
    (mapcar (pcase-lambda (`(,key ,label . ,_))
              (list :label label :action (lambda () (lme-settings-show key))
                    :checked (eq key shown)))
            (lme-settings--panels))))

(defun lme-settings-menus ()
  "Return the Settings menus, for the menu bar while Settings is in front."
  '((lme-settings-file "File" lme-settings--file-items)
    (lme-settings-view "View" lme-settings--view-items)))

(defun lme-setup-settings ()
  "Make Settings an application: its window follows the one in front, and
shows the theme's colours when they change."
  (add-hook 'lme-apps-frontmost-functions #'lme-settings--follow-frontmost)
  (add-hook 'lme-platinum-menu-closed-hook #'lme-settings--refocus)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lme-settings--render)) 95))

(provide 'lme-settings)
;;; lme-settings.el ends here
