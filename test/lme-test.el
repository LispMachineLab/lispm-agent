;;; lme-test.el --- Tests for lispm-emacs  -*- lexical-binding: t; -*-

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

;; Run with `make test'.  They need none of the packages lispm-emacs
;; installs: setup functions are not called, only what they decide with.

;;; Code:

(require 'ert)
(require 'lme)

(ert-deftest lme-test-agent-installed-p ()
  "An agent counts as installed exactly when its CLI is on PATH."
  (let ((lme-agent-programs '((claude . "lme-test-no-such-program")
                              (codex . "emacs"))))
    (should-not (lme-agent-installed-p 'claude))
    (should (lme-agent-installed-p 'codex))))

(ert-deftest lme-test-launch-requires-installed-cli ()
  "Starting an agent whose CLI is missing is a user error, before anything runs."
  (let ((lme-agent-programs '((claude . "lme-test-no-such-program"))))
    (should-error (lme-agent--launch 'claude temporary-file-directory "x" nil)
                  :type 'user-error)))

(ert-deftest lme-test-sessions-skip-missing-agents ()
  "No record of an agent whose CLI is missing is read or listed."
  (let ((lme-agent-emacs-agents nil)
        (read nil))
    (cl-letf (((symbol-function 'lme-agent-installed-p) #'ignore)
              ((symbol-function 'lme-agent--claude-files) (lambda () (setq read t) nil))
              ((symbol-function 'lme-agent--codex-files) (lambda () (setq read t) nil))
              ((symbol-function 'lme-agent--codex-thread-names) (lambda () (setq read t) nil))
              ((symbol-function 'lme-agent-hermes-sessions) (lambda () (setq read t) nil)))
      (should (null (lme-agent-sessions-list)))
      (should-not read))))

(ert-deftest lme-test-sessions-include-emacs-agents ()
  "Sessions of agents running inside Emacs are listed, newest first."
  (let ((lme-agent-emacs-agents
         `((lispm :sessions ,(lambda ()
                               (list (list :agent 'lispm :id "a" :time 1.0)
                                     (list :agent 'lispm :id "b" :time 2.0)))))))
    (cl-letf (((symbol-function 'lme-agent-installed-p) #'ignore))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) (lme-agent-sessions-list))
                     '("b" "a"))))))

(ert-deftest lme-test-first-start-in-batch ()
  "A batch Emacs settles on Emacs keys and Dracula without asking.
Choices already made are kept, and the start that finds them made is
not a first start."
  (let ((lme-editing-style nil)
        (lme-theme nil))
    (cl-letf (((symbol-function 'lme--read-editing-style)
               (lambda () (error "Asked in batch")))
              ((symbol-function 'lme--read-theme)
               (lambda () (error "Asked in batch"))))
      (should (lme--first-start))
      (should (eq lme-editing-style 'emacs))
      (should (eq lme-theme 'dracula))))
  (let ((lme-editing-style 'vim)
        (lme-theme 'platinum))
    (should-not (lme--first-start))
    (should (eq lme-editing-style 'vim))
    (should (eq lme-theme 'platinum))))

(ert-deftest lme-test-lispm-requirements ()
  "Of a package's requirements, those not installed are returned; Emacs never."
  (let ((file (make-temp-file "lme-test-" nil ".el"
                              (concat ";;; lme-test-pkg.el --- A test  -*- lexical-binding: t; -*-\n"
                                      ";; Version: 0.1\n"
                                      ";; Package-Requires: ((emacs \"30.1\") (lme-test-missing \"1.0\"))\n"
                                      ";;; Commentary:\n;;; Code:\n"
                                      "(provide 'lme-test-pkg)\n;;; lme-test-pkg.el ends here\n"))))
    (unwind-protect
        (should (equal (mapcar #'car (lme-lispm--requirements file)) '(lme-test-missing)))
      (delete-file file))))

(ert-deftest lme-test-lispm-ensure-uses-checkout ()
  "A LispM on `load-path' is used: nothing is installed from its URL."
  (let ((installed nil))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/checkout/lisp/lispm.el"))
              ((symbol-function 'lme-lispm--requirements) #'ignore)
              ((symbol-function 'package-vc-install) (lambda (&rest _) (setq installed t))))
      (lme-lispm-ensure)
      (should-not installed))))

(ert-deftest lme-test-lispm-ensure-installs-when-missing ()
  "With no LispM anywhere, it is installed from `lme-lispm-url'."
  (let ((installed nil))
    (cl-letf (((symbol-function 'locate-library) #'ignore)
              ((symbol-function 'package-vc-install) (lambda (url) (setq installed url))))
      (lme-lispm-ensure)
      (should (equal installed lme-lispm-url)))))

(ert-deftest lme-test-directory ()
  "`lme-directory' is the checkout, holding the agents' icons and the Hermes bridge."
  (should (file-exists-p (expand-file-name "lisp/lme.el" lme-directory)))
  (should (file-exists-p lme-agent-mcp-hermes-bridge))
  (should (file-directory-p lme-agent-icon-directory)))


;;;; Platinum

(ert-deftest lme-test-title-layout-centres ()
  "A short title sits in the middle of its bar, with stripes either side."
  (pcase-let ((`(,left ,title ,right) (lme-platinum-title-layout 600 100)))
    (should (= title 100))
    (should (> left 12))
    (should (> right 12))
    ;; The bar's chrome adds up to its width.
    (let ((s lme-platinum-box-size))
      (should (= (+ 8 s 6 left 8 title 8 right 6 s 4 s 8) 600)))))

(ert-deftest lme-test-title-layout-narrows-long-titles ()
  "A title too wide for its bar is narrowed, keeping stripes on both sides."
  (pcase-let ((`(,left ,title ,right) (lme-platinum-title-layout 300 1000)))
    (should (< title 300))
    (should (>= left 12))
    (should (>= right 12))))

(ert-deftest lme-test-palette-derives-from-theme ()
  "The chrome is the content darkened; bevels lean to the text and to black."
  (let ((palette (lme-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (should (equal (plist-get palette :content) "#282a36"))
    (should (equal (plist-get palette :accent) "#bd93f9"))
    (cl-flet ((lum (color) (apply #'+ (lme-tab-bar--rgb color))))
      (should (< (lum (plist-get palette :chrome)) (lum "#282a36")))
      (should (> (lum (plist-get palette :light)) (lum (plist-get palette :chrome))))
      (should (< (lum (plist-get palette :shade)) (lum (plist-get palette :chrome)))))))

(ert-deftest lme-test-boxes-draw ()
  "Every box kind draws, enabled, modified and disabled."
  (let ((palette (lme-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (dolist (kind '(close zoom collapse))
      (should (eq (dom-tag (lme-platinum-draw-box palette kind 22)) 'svg))
      (should (eq (dom-tag (lme-platinum-draw-box palette kind 22 t)) 'svg))
      (should (eq (dom-tag (lme-platinum-draw-box palette kind 22 nil t)) 'svg)))
    (should (eq (dom-tag (lme-platinum-draw-stripes palette 50 22)) 'svg))
    (should (eq (dom-tag (lme-platinum-draw-triangle palette t)) 'svg))))

(ert-deftest lme-test-rgb-reads-hex-without-a-display ()
  "A #RRGGBB colour is read exactly, even in batch."
  (should (equal (lme-tab-bar--rgb "#ff0080") (list 1.0 0.0 (/ 128 255.0))))
  (should (equal (lme-tab-bar--blend "#000000" "#ff0080" 1.0) "#ff0080")))

(ert-deftest lme-test-window-controls ()
  "A lone window zooms but cannot roll up; a side window only closes."
  (save-window-excursion
    (delete-other-windows)
    (should (equal (lme-platinum--window-controls (selected-window)) '(on on off)))
    (let ((below (split-window-below)))
      (should (equal (lme-platinum--window-controls below) '(on on on))))
    (let ((side (display-buffer-in-side-window (get-buffer-create " lme-test-side")
                                               '((side . left)))))
      (should (equal (lme-platinum--window-controls side) '(on off off))))))

(ert-deftest lme-test-chrome-controls ()
  "Chrome, such as the browser's address window, only closes, and its close
box runs the buffer's own close function."
  (save-window-excursion
    (delete-other-windows)
    (let ((chrome (split-window-below))
          (closed nil))
      (set-window-parameter chrome 'lme-chrome t)
      (should (equal (lme-platinum--window-controls chrome) '(on off off)))
      (with-temp-buffer
        (setq-local lme-platinum-box-functions (list :close (lambda (window) (setq closed window))))
        (set-window-buffer chrome (current-buffer))
        (lme-platinum-close-window chrome)
        (should (eq closed chrome))
        (should (buffer-live-p (current-buffer)))))))

(defun lme-test--menu-bar-titles ()
  "Return the keys of the menu bar's left end."
  (mapcar #'car (lme-platinum-menu-bar)))

(ert-deftest lme-test-web-title ()
  "Web follows the views where the browser can run, lit while it is in
front; without xwidgets it is left out."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'always)
            ((symbol-function 'lme-browser-workspace-p) #'ignore))
    (let ((bar (lme-platinum-menu-bar)))
      (should (equal (last (mapcar #'car bar) 2) '(lme-view-shells lme-view-web)))
      (should-not (text-property-any 0 (length (nth 2 (car (last bar))))
                                     'face 'lme-platinum-menu-title-active
                                     (nth 2 (car (last bar)))))))
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'always)
            ((symbol-function 'lme-browser-workspace-p) #'always))
    (let ((web (nth 2 (car (last (lme-platinum-menu-bar))))))
      (should (memq 'lme-platinum-menu-title-active
                    (ensure-list (get-text-property 0 'face web))))))
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (should-not (memq 'lme-view-web (lme-test--menu-bar-titles)))))

(ert-deftest lme-test-desktop-darkens-selected-icons ()
  "A selected icon is its SVG with every colour halved."
  (should (equal (lme-desktop--darken "<rect fill=\"#FF8040\"/><rect fill=\"#000000\"/>")
                 "<rect fill=\"#7F4020\"/><rect fill=\"#000000\"/>")))

(ert-deftest lme-test-desktop-items ()
  "The browser's icon shows where the browser can run; the Trash is last."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (should (equal (mapcar #'car (lme-desktop--items)) '(home editor settings trash))))
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'always))
    (should (equal (mapcar #'car (lme-desktop--items))
                   '(home editor browser settings trash)))))

(ert-deftest lme-test-desktop-icons-stand-where-put ()
  "Icons with no place stand in the column at the right edge, the Trash in
the bottom corner; one put elsewhere stands there, measured from the edges
nearest it, kept whole on the desktop; the others keep to free cells."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore)
            ((symbol-function 'lme-desktop--font) (lambda () '("Geneva" 12 10))))
    (pcase-let ((`(,cell-width . ,cell-height) (lme-desktop--cell-size)))
      (let ((lme-desktop-icon-places nil))
        (let ((layout (lme-desktop--layout 800 600)))
          (should (equal (mapcar #'cdr layout)
                         `((,(- 800 lme-desktop-margin cell-width) ,lme-desktop-margin)
                           (,(- 800 lme-desktop-margin cell-width)
                            ,(+ lme-desktop-margin cell-height lme-desktop-gap))
                           (,(- 800 lme-desktop-margin cell-width)
                            ,(+ lme-desktop-margin (* 2 (+ cell-height lme-desktop-gap))))
                           (,(- 800 lme-desktop-margin cell-width)
                            ,(- 600 lme-desktop-margin cell-height)))))))
      ;; Home put at the top left; Emacs keeps its cell, the second.
      (let ((lme-desktop-icon-places '((home :left 20 :top 30))))
        (let ((layout (lme-desktop--layout 800 600)))
          (should (equal (cdr (seq-find (lambda (entry) (eq (car (car entry)) 'home)) layout))
                         '(20 30)))
          (should (equal (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor)) layout))
                         lme-desktop-margin))))
      ;; An icon put on Emacs's cell pushes it down the column.
      (let ((lme-desktop-icon-places `((home :right ,lme-desktop-margin :top ,lme-desktop-margin)
                                       (trash :right ,lme-desktop-margin :top ,lme-desktop-margin))))
        (should (> (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor))
                                    (lme-desktop--layout 800 600)))
                   lme-desktop-margin)))
      ;; A place past the edge is kept whole on the desktop.
      (should (equal (lme-desktop--place-xy '(:left 2000 :top -5) 800 600)
                     (cons (- 800 cell-width) 0)))
      ;; Measured from the nearest edges, so it follows them.
      (should (equal (lme-desktop--xy-place 700 500 800 600)
                     (list :right (- 800 700 cell-width) :bottom (- 600 500 cell-height))))
      (should (equal (lme-desktop--xy-place 10 20 800 600) '(:left 10 :top 20)))
      (should (equal (lme-desktop--place-xy (lme-desktop--xy-place 700 20 800 600) 1000 600)
                     '(900 . 20))))))

(ert-deftest lme-test-desktop-drop-keeps-the-others ()
  "Dropping an icon keeps every other where it stands and saves them all."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore)
            ((symbol-function 'lme-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'lme-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let ((lme-desktop-icon-places nil)
          (pitch (+ (cdr (lme-desktop--cell-size)) lme-desktop-gap)))
      ;; Dropped near the top left, it lands on the grid cell nearest.
      (lme-desktop--drop 'home 100 120 nil)
      (should (equal (alist-get 'home lme-desktop-icon-places)
                     (list :left (+ lme-desktop-margin lme-desktop-column)
                           :top (+ lme-desktop-margin (* 2 pitch)))))
      (should (equal (alist-get 'editor lme-desktop-icon-places)
                     (lme-desktop--xy-place (- 800 lme-desktop-margin lme-desktop-column)
                                            (+ lme-desktop-margin pitch)
                                            800 600)))
      ;; Dropped on Emacs's cell, it takes the free cell nearest instead.
      (lme-desktop--drop 'home (- 800 lme-desktop-margin lme-desktop-column)
                         (+ lme-desktop-margin pitch 5) nil)
      (let ((home (lme-desktop--place-xy (alist-get 'home lme-desktop-icon-places) 800 600))
            (editor (lme-desktop--place-xy (alist-get 'editor lme-desktop-icon-places) 800 600)))
        (should-not (equal home editor))
        (should (lme-desktop--cell-free-p home (list (list (car editor) (cdr editor))))))
      ;; Near the bottom right corner, it lines up with the Trash's row.
      (lme-desktop--drop 'home (- 800 200) (- 600 lme-desktop-margin 60) nil)
      (should (equal (plist-get (alist-get 'home lme-desktop-icon-places) :bottom)
                     lme-desktop-margin))
      (should (zerop (mod (- (plist-get (alist-get 'home lme-desktop-icon-places) :right)
                             lme-desktop-margin)
                          lme-desktop-column)))
      (lme-desktop-arrange-icons)
      (should-not lme-desktop-icon-places))))

(ert-deftest lme-test-desktop-drag-survives-empty-reads ()
  "Catching up with the mouse while input is pending but nothing can be read
yet does not end the drag: the icon is still dropped where it is let go."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore)
            ((symbol-function 'lme-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lme-desktop--copy-frame) #'ignore)
            ((symbol-function 'lme-desktop--show-copy) #'ignore)
            ((symbol-function 'lme-popup--dispose) #'ignore)
            ((symbol-function 'lme-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lme-desktop-icon-places nil)
           (cell (lme-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (moves 0)
           (pointer press)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lme-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'input-pending-p) #'always)
                ((symbol-function 'read-event)
                 (lambda (&optional _prompt _method seconds)
                   (cond (seconds nil)
                         (unread-command-events (pop unread-command-events))
                         ((< moves 5)
                          (setq moves (1+ moves)
                                pointer (cons (- (car pointer) 20) (+ (cdr pointer) 10)))
                          '(mouse-movement (nil)))
                         (t '(drag-mouse-1 (nil) (nil)))))))
        (lme-desktop--drag 'home nil press))
      (should (equal (alist-get 'home lme-desktop-icon-places)
                     (lme-desktop--snap (- (car cell) 100) (+ (cadr cell) 50) 800 600
                                        (mapcar #'cdr (seq-remove
                                                       (lambda (entry) (eq (car (car entry)) 'home))
                                                       (lme-desktop--layout 800 600))))))
      (should-not unread-command-events))))

(ert-deftest lme-test-desktop-right-click-menu ()
  "A right click on the desktop offers Arrange Icons and the picture; on an
icon, Open first."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (let ((shown nil)
          (key nil)
          (lme-desktop-icon-places '((home :left 10 :top 10))))
      (cl-letf (((symbol-function 'lme-desktop-mouse-select)
                 (lambda (_) (with-current-buffer (lme-desktop-buffer)
                               (setq lme-desktop--selected key))))
                ((symbol-function 'lme-popup--frame-xy) (lambda (_) '(0 . 0)))
                ((symbol-function 'lme-popup-menu) (lambda (items &rest _) (setq shown items))))
        (lme-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Arrange Icons"))
        (should (plist-get (car shown) :enabled))
        (setq key 'home)
        (lme-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Open"))
        (should (member "Desktop Picture…"
                        (mapcar (lambda (item) (and (consp item) (plist-get item :label))) shown)))))))

(ert-deftest lme-test-desktop-double-click-opens-not-drags ()
  "A double click opens the icon, even when its first click strayed a few
pixels; a press let go after a jitter is a click, and a drop back on the
icon's own cell saves nothing."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore)
            ((symbol-function 'lme-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lme-desktop--copy-frame) #'ignore)
            ((symbol-function 'lme-desktop--show-copy) #'ignore)
            ((symbol-function 'lme-popup--dispose) #'ignore)
            ((symbol-function 'lme-desktop--render) #'ignore))
    (let* ((lme-desktop-icon-places nil)
           (lme-desktop--last-click nil)
           (saved nil)
           (opened nil)
           (cell (lme-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (pointer press)
           (moves nil)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (variable value) (setq saved t) (set variable value)))
                ((symbol-function 'lme-desktop-open) (lambda () (setq opened t)))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'lme-apps-set-frontmost) #'ignore)
                ((symbol-function 'lme-desktop--select) #'ignore)
                ((symbol-function 'lme-desktop--item-at) (lambda (_) 'home))
                ((symbol-function 'lme-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'input-pending-p) #'ignore)
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (if moves
                       (progn (setq pointer (pop moves)) '(mouse-movement (nil)))
                     '(mouse-1 (nil))))))
        (let ((event `(down-mouse-1 (nil 1 ,press 0))))
          ;; A first click that jitters 3 and 4 pixels is a click.
          (setq moves (list (cons (+ (car press) 3) (+ (cdr press) 4))))
          (lme-desktop-mouse-select event)
          (should-not saved)
          (should (eq (car lme-desktop--last-click) 'home))
          ;; The second press, quick and close by, opens without dragging.
          (setq moves (list (cons (+ (car press) 40) (cdr press))))
          (lme-desktop-mouse-select event)
          (should opened)
          (should-not saved)
          (should moves)
          ;; A drag that comes back to the icon's own cell saves nothing.
          (setq moves (list (cons (+ (car press) 10) (+ (cdr press) 9)))
                opened nil)
          (lme-desktop-mouse-select event)
          (should-not opened)
          (should-not saved))))))

(ert-deftest lme-test-desktop-draws-the-dragged-copy ()
  "The dragged copy is its cell's piece of the desktop, the icon translucent
over it."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (let* ((lme-desktop-picture nil)
           (svg (lme-desktop--copy-svg 'home 50 60 800 600)))
      (should (string-match-p "viewBox=\"[0-9-]+ 60 " svg))
      (should (string-match-p (format "<rect width=\"800\" height=\"600\" fill=\"%s\"/>"
                                      (lme-desktop--color))
                              svg))
      (should (string-suffix-p "Home</text></g></svg>" svg))
      (should (string-match-p "<g opacity=\"0.5\">" svg)))))

(ert-deftest lme-test-desktop-picture-scaled-copy ()
  "A picture filling the desktop is drawn from a copy scaled to it once one
is made; until then, and for a picture at its own size, from itself."
  (let* ((lme-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lme-directory))
         (file lme-desktop-picture)
         (time (file-attribute-modification-time (file-attributes file)))
         (started nil))
    (cl-letf (((symbol-function 'lme-desktop--picture-size) (lambda (_) '(4000 . 3000)))
              ((symbol-function 'frame-scale-factor) (lambda (&rest _) 2.0))
              ((symbol-function 'lme-desktop--scale-picture)
               (lambda (&rest args) (setq started args))))
      (let ((lme-desktop-picture-position 'fill)
            (lme-desktop--scaled nil))
        (should (equal (lme-desktop--picture-file 800 600) file))
        (should (equal started (list file time 1600 1200)))
        (setq lme-desktop--scaled (list file time 1600 1200 file))
        (setq started nil)
        (should (equal (lme-desktop--picture-file 800 600) file))
        (should-not started)
        ;; Grown past the copy, the copy is made again.
        (lme-desktop--picture-file 1000 600)
        (should started))
      (let ((lme-desktop-picture-position 'center)
            (lme-desktop--scaled nil))
        (setq started nil)
        (should (equal (lme-desktop--picture-file 800 600) file))
        (should-not started)))))

(ert-deftest lme-test-desktop-selects-and-names ()
  "The arrow keys step through the icons, wrapping; the selected one's name
is inverted, and a click finds the icon drawn under it, the Trash in the
bottom corner."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (with-current-buffer (lme-desktop-buffer)
      (setq lme-desktop--selected nil)
      (lme-desktop-select-next)
      (should (eq lme-desktop--selected 'home))
      (lme-desktop-select-previous)
      (should (eq lme-desktop--selected 'trash))
      (lme-desktop-select-previous)
      (should (eq lme-desktop--selected 'settings))
      (setq lme-desktop--selected 'home)
      (let ((svg (lme-desktop--svg 800 600)))
        (should (string-match-p "fill=\"#000000\"/><text[^>]*fill=\"#FFFFFF\">Home</text>" svg))
        (should (string-match-p "fill=\"#FFFFFF\"/><text[^>]*fill=\"#000000\">Trash</text>" svg))
        (pcase-let ((`(,_ ,left ,top ,right ,bottom) (assq 'home lme-desktop--hits)))
          (should (eq (lme-desktop--key-at (/ (+ left right) 2) (/ (+ top bottom) 2)) 'home)))
        (should (> (nth 2 (assq 'trash lme-desktop--hits)) 400))
        (should-not (lme-desktop--key-at 10 10)))
      (lme-desktop-deselect)
      (should-not lme-desktop--selected))))

(ert-deftest lme-test-desktop-picture ()
  "A desktop picture lies over the desktop's colour as its position says."
  (let ((lme-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lme-directory)))
    (let ((lme-desktop-picture-position 'fill))
      (should (string-match-p "<image href=\"globe-32.svg\"[^>]*xMidYMid slice"
                              (lme-desktop--svg 800 600))))
    (let ((lme-desktop-picture-position 'fit))
      (should (string-match-p "xMidYMid meet" (lme-desktop--svg 800 600))))
    (let ((lme-desktop-picture-position 'tile))
      (should (string-match-p "<pattern id=\"lme-tile\"" (lme-desktop--svg 800 600)))))
  (let ((lme-desktop-picture nil)
        (lme-desktop-color "#123456"))
    (let ((svg (lme-desktop--svg 800 600)))
      (should (string-match-p "fill=\"#123456\"" svg))
      (should-not (string-match-p "<image" svg)))))

(ert-deftest lme-test-trash-full ()
  "The Trash is full when it holds anything but Finder's .DS_Store: counted
where it can be read, measured by its link count where it cannot."
  (cl-letf (((symbol-function 'directory-files)
             (lambda (&rest _) '(".DS_Store" "old.txt"))))
    (should (lme-desktop-trash-full-p)))
  (cl-letf (((symbol-function 'directory-files) (lambda (&rest _) '(".DS_Store"))))
    (should-not (lme-desktop-trash-full-p)))
  (dolist (case '((3 t nil) (4 t t) (2 nil nil) (3 nil t)))
    (pcase-let ((`(,links ,ds-store ,full) case))
      (cl-letf (((symbol-function 'directory-files)
                 (lambda (&rest _) (signal 'file-error '("Operation not permitted"))))
                ((symbol-function 'file-attributes)
                 (lambda (&rest _) (list t links 501 20 nil nil nil 64)))
                ((symbol-function 'file-exists-p) (lambda (_) ds-store)))
        (should (eq (and (lme-desktop-trash-full-p) t) full))))))

(ert-deftest lme-test-closing-the-last-window-shows-the-desktop ()
  "The close box takes a window away while others remain in the main area;
the last one uncovers the desktop."
  (save-window-excursion
    (delete-other-windows)
    (let ((first (generate-new-buffer " lme-test-first"))
          (second (generate-new-buffer " lme-test-second")))
      (set-window-buffer (selected-window) first)
      (let ((other (split-window-right)))
        (set-window-buffer other second)
        (lme-platinum-close-window other)
        (should-not (window-live-p other))
        (should-not (buffer-live-p second))
        (lme-platinum-close-window (selected-window))
        (should-not (buffer-live-p first))
        (should (lme-desktop-p (selected-window)))))))

(ert-deftest lme-test-editor-opens-on-the-startup-screen ()
  "In the editor, the last window closed shows Emacs's own startup screen."
  (save-window-excursion
    (delete-other-windows)
    (let ((lme-desktop-empty-function #'lme-editor--show-splash)
          (buffer (generate-new-buffer " lme-test-last")))
      (set-window-buffer (selected-window) buffer)
      (lme-platinum-close-window (selected-window))
      (should (equal (buffer-name (window-buffer)) lme-editor-splash-buffer-name)))))

(ert-deftest lme-test-new-workspace-opens-on-the-desktop ()
  "A workspace opened onto *scratch* shows the desktop instead."
  (save-window-excursion
    (delete-other-windows)
    (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
    (lme-desktop--on-new-workspace)
    (should (lme-desktop-p))))

(defmacro lme-test--with-folder (&rest body)
  "Run BODY with `folder' bound to a fresh folder of a few items."
  (declare (indent 0))
  `(let ((folder (make-temp-file "lme-finder" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "zeta" folder))
           (make-directory (expand-file-name "Alpha/.git" folder) t)
           (dolist (name '("notes.org" "init.el" "beta.txt" ".hidden"))
             (write-region "" nil (expand-file-name name folder)))
           ,@body)
       (delete-directory folder t))))

(ert-deftest lme-test-finder-reads-folders ()
  "A folder's items come by name, case aside, hidden ones left out, each
with its large icon."
  (lme-test--with-folder
    (let ((items (lme-finder--read folder)))
      (should (equal (mapcar #'car items) '("Alpha" "beta.txt" "init.el" "notes.org" "zeta")))
      (should (cdr (assoc "zeta" items)))
      (should-not (cdr (assoc "init.el" items)))
      (should (equal (lme-finder--icon-name folder (assoc "Alpha" items)) "folder-git-32"))
      (should (equal (lme-finder--icon-name folder (assoc "zeta" items)) "folder-32"))
      (should (equal (lme-finder--icon-name folder (assoc "init.el" items)) "document-lisp-32"))
      (should (equal (lme-finder--icon-name folder (assoc "notes.org" items)) "document-org-32"))
      (should (equal (lme-finder--icon-name folder (assoc "beta.txt" items)) "document-32")))))

(ert-deftest lme-test-finder-history ()
  "Opening a folder records the one left: back, forward and up move through
them, up selecting the folder it came from."
  (lme-test--with-folder
    (let ((root (file-name-as-directory folder)))
      (with-current-buffer (lme-finder--buffer)
        (setq lme-finder--directory nil lme-finder--back nil lme-finder--forward nil))
      (lme-finder--go folder)
      (lme-finder--go (expand-file-name "zeta" folder) t)
      (lme-finder--in
        (should (equal lme-finder--back (list root)))
        (should (equal (lme-finder--title lme-finder--directory) "zeta")))
      (lme-finder-back)
      (lme-finder--in
        (should (equal lme-finder--directory root))
        (should (equal lme-finder--forward (list (file-name-as-directory
                                                  (expand-file-name "zeta" folder))))))
      (lme-finder-forward)
      (lme-finder-up)
      (lme-finder--in
        (should (equal lme-finder--directory root))
        (should (equal lme-finder--selected "zeta"))
        (should (string-match-p "\\`5 items" (lme-finder--info)))))
    (kill-buffer lme-finder-buffer-name)))

(ert-deftest lme-test-finder-moves-to-trash ()
  "Command-Delete sends the selected item to the Trash and selects the next."
  (lme-test--with-folder
    (let ((trashed nil))
      (cl-letf (((symbol-function 'move-file-to-trash)
                 (lambda (file) (push file trashed) (delete-file file))))
        (lme-finder--go folder)
        (lme-finder--select "beta.txt")
        (lme-finder-move-to-trash)
        (should (equal trashed (list (expand-file-name "beta.txt" folder))))
        (lme-finder--in
          (should-not (assoc "beta.txt" lme-finder--items))
          (should (equal lme-finder--selected "init.el")))))
    (kill-buffer lme-finder-buffer-name)))

(ert-deftest lme-test-views-ask-the-editor-first ()
  "A view is asked for through the editor, which comes forward first."
  (let ((asked nil))
    (cl-letf (((symbol-function 'lme-editor--prepare-view) (lambda () (setq asked t)))
              (lme-sidebar-views '((test :visible ignore :show ignore :hide ignore)))
              (lme-sidebar-prepare-function #'lme-editor--prepare-view))
      (lme-sidebar-select 'test)
      (should asked))))

(ert-deftest lme-test-menu-bar-follows-the-application ()
  "The menu bar holds the menus of the application in front."
  (let ((lme-platinum-menus-function #'lme-apps-menus))
    (let ((lme-apps--frontmost 'editor))
      (should (equal (lme-test--menu-bar-titles)
                     '(lme-lambda lme-editor-file lme-editor-edit lme-editor-view
                                  lme-editor-project lme-editor-window))))
    (let ((lme-apps--frontmost 'desktop))
      (should (equal (lme-test--menu-bar-titles)
                     '(lme-lambda lme-desktop-file lme-desktop-view lme-desktop-go
                                  lme-desktop-special))))
    (let ((lme-apps--frontmost 'browser))
      (should (equal (lme-test--menu-bar-titles)
                     '(lme-lambda lme-browser-file lme-browser-view lme-browser-go
                                  lme-browser-bookmarks lme-browser-window))))))

(ert-deftest lme-test-application-follows-its-selected-window ()
  "Selecting a floating window puts its application in front; the screen,
selected by a click on the menu bar, leaves the one in front."
  (let ((lme-apps--frontmost 'editor)
        (lme-apps-frontmost-functions nil)
        (app 'browser))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (and (eq parameter 'lme-app) app))))
      (lme-apps--follow-window)
      (should (eq (lme-apps-frontmost) 'browser))
      (setq app nil)
      (lme-apps--follow-window)
      (should (eq (lme-apps-frontmost) 'browser)))))

(ert-deftest lme-test-browser-boxes ()
  "The browser's zoom box moves it between full screen and floating; only
the floating browser rolls up."
  (cl-letf (((symbol-function 'lme-browser-floating-p) #'ignore))
    (should (equal (lme-browser--boxes)
                   '(:close lme-browser--close-window :zoom lme-browser--zoom-box))))
  (cl-letf (((symbol-function 'lme-browser-floating-p) #'always))
    (should (eq (plist-get (lme-browser--boxes) :collapse) 'lme-browser--collapse-box))))

(ert-deftest lme-test-application-menu-lists-applications ()
  "The Application menu lists the applications, the one in front checked;
the browser only where it can run."
  (let ((lme-apps--frontmost 'editor))
    (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
      (let ((items (lme-apps-application-items)))
        (should (equal (mapcar (lambda (item) (plist-get item :label)) items)
                       '("Desktop" "Emacs")))
        (should (plist-get (nth 1 items) :checked))
        (should (equal (cdr (lme-apps-application)) "Emacs"))))
    (cl-letf (((symbol-function 'lme-xwidget-available-p) #'always))
      (should (= 3 (length (lme-apps-application-items)))))))

(ert-deftest lme-test-documents-go-to-the-editor ()
  "Outside the editor, documents are sent to it; the browser's pages, the
desktop and internal buffers are not."
  (with-current-buffer (get-buffer-create "lme-test-document.txt")
    (setq buffer-file-name "/tmp/lme-test-document.txt")
    (let ((document (current-buffer)))
      (cl-letf (((symbol-function 'lme-editor--in-workspace-p) #'ignore))
        (should (lme-editor--astray-p document nil))
        (should-not (lme-editor--astray-p (lme-desktop-buffer) nil))
        (let ((lme-editor--displaying t))
          (should-not (lme-editor--astray-p document nil))))
      (cl-letf (((symbol-function 'lme-editor--in-workspace-p) #'always))
        (should-not (lme-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lme-test-documents-leave-the-floating-browser ()
  "A document opened from the floating browser, an application's window
of its own, goes to the editor; popups are left be."
  (with-current-buffer (get-buffer-create "lme-test-document.txt")
    (setq buffer-file-name "/tmp/lme-test-document.txt")
    (let ((document (current-buffer))
          (parameters '((lme-window . t))))
      (cl-letf (((symbol-function 'frame-parent) #'always)
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter) (alist-get parameter parameters))))
        (should (lme-editor--astray-p document nil))
        (setq parameters nil)
        (should-not (lme-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lme-test-new-workspace-starts-on-startup-screen ()
  "A new workspace shows Emacs's startup screen itself, handing tab-bar no
buffer: one shown through `switch-to-buffer' would obey the editor's
routing while the old workspace is still in front."
  (save-window-excursion
    (should-not (lme-editor--new-workspace-buffer))
    (should (equal (buffer-name (window-buffer)) lme-editor-splash-buffer-name))))

(ert-deftest lme-test-editor-workspaces-leave-out-desktop-and-browser ()
  "The desktop's and the browser's workspaces are not the editor's."
  (should-not (lme-editor--workspace-tab-p '(tab (name . "Desktop"))))
  (should-not (lme-editor--workspace-tab-p '(tab (name . "Browser"))))
  (should (lme-editor--workspace-tab-p '(tab (name . "lispm-emacs")))))

(ert-deftest lme-test-buffer-manager-counts-process-trees ()
  "A buffer's process is counted with every process under it."
  (let ((processes (make-hash-table)))
    (puthash 10 '(1 1.5 1000) processes)
    (puthash 11 '(10 2.0 500) processes)
    (puthash 12 '(11 0.5 250) processes)
    (puthash 20 '(1 9.0 9000) processes)
    (let ((children (lme-buffer-manager--children processes)))
      (should (equal (lme-buffer-manager--tree-cost 10 processes children) '(4.0 . 1750)))
      (should (equal (lme-buffer-manager--tree-cost 12 processes children) '(0.5 . 250))))))

(ert-deftest lme-test-buffer-manager-rows ()
  "Every buffer but internal ones has a row, its text measured."
  (with-current-buffer (get-buffer-create "lme-test-row")
    (erase-buffer)
    (insert "hello")
    (cl-letf (((symbol-function 'lme-buffer-manager--processes) (lambda () (make-hash-table))))
      (let* ((entries (lme-buffer-manager--entries))
             (row (assq (current-buffer) entries)))
        (should row)
        (should (equal (get-text-property 0 'lme-value (aref (cadr row) 2)) 5))
        (should-not (seq-find (lambda (entry) (string-prefix-p " " (buffer-name (car entry))))
                              entries))))
    (kill-buffer)))

(ert-deftest lme-test-palette-takes-named-colours ()
  "Colours a theme names replace the derived ones; the rest stay derived."
  (let* ((derived (lme-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (palette (lme-platinum--merge-palette derived '(:chrome "#DDDDDD" :accent "#6363CE"))))
    (should (equal (plist-get palette :chrome) "#DDDDDD"))
    (should (equal (plist-get palette :accent) "#6363CE"))
    (should (equal (plist-get palette :shade) (plist-get derived :shade)))
    ;; The derived palette itself is left untouched.
    (should (equal (plist-get derived :accent) "#bd93f9"))))

(ert-deftest lme-test-platinum-theme ()
  "The platinum theme loads, and names Platinum's greys for the chrome."
  (let ((custom-theme-load-path (cons (expand-file-name "themes/" lme-directory)
                                      custom-theme-load-path)))
    (unwind-protect
        (progn
          (load-theme 'platinum t)
          (should (memq 'platinum custom-enabled-themes))
          (should (equal (plist-get lme-platinum-colors :chrome) "#DDDDDD"))
          (should (equal (plist-get lme-platinum-colors :accent) "#6363CE")))
      (disable-theme 'platinum))
    (should-not (plist-get lme-platinum-colors :chrome))))

(ert-deftest lme-test-window-bottom ()
  "A window's bottom edge counts a list's items, or shows the buffer."
  (let ((lme-platinum--palette (lme-platinum--derive-palette "#282a36" "#f8f8f2"
                                                             "#6272a4" "#bd93f9")))
    (with-temp-buffer
      (rename-buffer "lme-test 100% done" t)
      (let ((left (lme-platinum-mode-line-left)))
        ;; A % in the buffer name is escaped, as the bottom is a mode line.
        (should (string-search "100%% done" left)))
      (setq-local lme-platinum-info "3 sessions")
      (should (string-search "3 sessions" (lme-platinum-mode-line-left)))
      (should (string-empty-p (string-trim (substring-no-properties
                                            (lme-platinum-mode-line-right))))))))

(ert-deftest lme-test-sidebar-count ()
  "List windows count their items in the singular or the plural."
  (should (equal (lme-sidebar--count 1 "session") "1 session"))
  (should (equal (lme-sidebar--count 3 "session" "1 running") "3 sessions, 1 running")))

(ert-deftest lme-test-pressed-box-is-inverted ()
  "A box held down is lit from below and filled darker."
  (let* ((palette (lme-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (up (format "%S" (lme-platinum-draw-box palette 'close 22)))
         (down (format "%S" (lme-platinum-draw-box palette 'close 22 nil nil t))))
    (should-not (equal up down))
    ;; The fill of a pressed box is not the chrome.
    (should (string-search (lme-tab-bar--blend (plist-get palette :chrome)
                                               (plist-get palette :shade) 0.5)
                           down))))

(ert-deftest lme-test-press-runs-on-release-over-button ()
  "A pressed button runs only when let go over it, and is drawn pushed in
while held."
  (save-window-excursion
    (let* ((window (selected-window))
           (press (list window 'tab-line '(0 . 0) 0))
           (ran 0)
           (held nil))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (setq held (window-parameter window 'lme-platinum-pressed))))
                ((symbol-function 'lme-platinum--press-target) (lambda (_) 'close)))
        ;; Let go over the button: it runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lme-platinum-press press (lambda (_) t) (lambda () (cl-incf ran))))
        (should (= ran 1))
        (should (eq held 'close))
        (should-not (window-parameter window 'lme-platinum-pressed))
        ;; Moved off before letting go: nothing runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lme-platinum-press press (lambda (_) nil) (lambda () (cl-incf ran))))
        (should (= ran 1))))))

(ert-deftest lme-test-drag-swaps-windows ()
  "A title bar dropped on another window swaps the two, columns permitting."
  (lme-test--with-layout
   (let* ((session (lme-test--session "lme-test session 8"))
          (a (lme-test--file "lme-test-g.txt"))
          (b (lme-test--file "lme-test-h.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (let* ((file-window (selected-window))
            (lower (split-window-below)))
       (set-window-buffer lower b)
       (cl-flet ((drag (from to)
                   (lme-platinum-drag-window
                    (list 'drag-mouse-1 (list from 'tab-line '(0 . 0) 0)
                          (list to 'tab-line '(0 . 0) 0)))))
         ;; Two files trade places.
         (drag file-window lower)
         (should (eq (window-buffer file-window) b))
         (should (eq (window-buffer lower) a))
         ;; A file dropped on the session does not move.
         (drag lower (get-buffer-window session))
         (should (eq (window-buffer (get-buffer-window session)) session)))))))

(ert-deftest lme-test-file-icons ()
  "Files are shown with the document of their kind."
  (should (equal (lme-platinum-file-icon-name "a.el") "document-lisp"))
  (should (equal (lme-platinum-file-icon-name "a.rs") "document-code"))
  (should (equal (lme-platinum-file-icon-name "README.md") "document-markdown"))
  (should (equal (lme-platinum-file-icon-name "notes.org") "document-org"))
  (should (equal (lme-platinum-file-icon-name "a.png") "document-image"))
  (should (equal (lme-platinum-file-icon-name "LICENSE") "document"))
  (dolist (name '("document-lisp" "document-code" "document-markdown" "document-org"
                  "folder-git" "lispm" "robot"))
    (should (file-exists-p (expand-file-name (format "icons/os9/%s.svg" name) lme-directory)))))

(ert-deftest lme-test-scroll-thumb ()
  "The thumb is as long as the share shown, and as far down as the start."
  (let ((w lme-platinum-scroll-width))
    ;; Everything shows: no thumb.
    (should-not (lme-platinum-scroll-thumb 400 1 1000 1 1000))
    ;; The top of a buffer ten times the window: a tenth of the track, at its top.
    (pcase-let ((`(,top . ,length) (lme-platinum-scroll-thumb (+ 300 (* 2 w)) 1 101 1 1001)))
      (should (= top w))
      (should (= length 30)))
    ;; The end: the thumb's bottom meets the down arrow.
    (pcase-let ((`(,top . ,length) (lme-platinum-scroll-thumb (+ 300 (* 2 w)) 901 1001 1 1001)))
      (should (= (+ top length) (+ 300 w))))
    ;; A short thumb is never shorter than a box.
    (should (= (cdr (lme-platinum-scroll-thumb 400 1 2 1 100000)) 16))))

(ert-deftest lme-test-scroll-bar-draws ()
  "Scroll bars draw active, inactive, and with nothing to scroll."
  (let ((palette (lme-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (should (eq (dom-tag (lme-platinum-draw-scroll-bar palette 300 '(40 . 30) t)) 'svg))
    (should (eq (dom-tag (lme-platinum-draw-scroll-bar palette 300 nil t)) 'svg))
    (should (eq (dom-tag (lme-platinum-draw-scroll-bar palette 300 '(40 . 30) nil)) 'svg))))

(ert-deftest lme-test-scroll-bar-windows ()
  "Files get scroll bars; sessions and side windows other than lists do not."
  (save-window-excursion
    (delete-other-windows)
    (let ((file (lme-test--file "lme-test-i.txt"))
          (session (lme-test--session "lme-test session 9")))
      (set-window-buffer (selected-window) file)
      (should (lme-platinum--scroll-window-p (selected-window)))
      (set-window-buffer (selected-window) session)
      (should-not (lme-platinum--scroll-window-p (selected-window)))
      (let ((side (display-buffer-in-side-window (get-buffer-create " lme-test popup")
                                                 '((side . bottom)))))
        (should-not (lme-platinum--scroll-window-p side))))))

(ert-deftest lme-test-menu-text ()
  "A menu lists its items one to a line, separators as rules, checks marked."
  (let* ((lme-platinum--palette (lme-platinum--derive-palette "#FFFFFF" "#000000"
                                                              "#777777" "#6363CE"))
         (items '((:label "One" :action ignore :checked t)
                  separator
                  (:label "Two" :action ignore :enabled nil)
                  (:label "Three" :action ignore)))
         (text (lme-popup-menu-text items 200 3)))
    (should (= (length (split-string text "\n")) 4))
    (should (string-search "✓" text))
    (should (eq (get-text-property (string-search "Three" text) 'face text)
                'lme-popup-item-highlight))
    (should (eq (get-text-property (string-search "Two" text) 'face text)
                'lme-popup-item-disabled))
    ;; Moving down from One skips the separator and the disabled Two.
    (should (= (lme-popup--step items 0 1) 3))
    (should (= (lme-popup--step items 3 1) 0))))

(ert-deftest lme-test-menu-keys ()
  "Key equivalents are keyboard sequences only."
  (should (lme-popup--keyboard-p (kbd "C-c a l")))
  (should-not (lme-popup--keyboard-p [C-M-drag-mouse-1]))
  (should-not (lme-popup--keyboard-p [menu-bar file open])))

(ert-deftest lme-test-keymap-items ()
  "A menu keymap becomes menu items: labels, conditions, checks, submenus."
  (let ((map (make-sparse-keymap "Test"))
        (sub (make-sparse-keymap "Sub")))
    (define-key sub [inner] '(menu-item "Inner" ignore))
    ;; Bindings added later come first, as Emacs shows them.
    (define-key map [last] '(menu-item "Last" ignore :enable nil))
    (define-key map [sub] `(menu-item "More" ,sub))
    (define-key map [sep] '(menu-item "--"))
    (define-key map [hidden] '(menu-item "Hidden" ignore :visible nil))
    (define-key map [toggle] '(menu-item "Toggle" ignore :button (:toggle . t)))
    (define-key map [first] '(menu-item "First" ignore :keys "C-x f"))
    (let ((items (lme-popup-keymap-items map)))
      (should (equal (mapcar (lambda (item) (if (eq item 'separator) item (plist-get item :label)))
                             items)
                     '("First" "Toggle" separator "More" "Last")))
      (should (equal (plist-get (nth 0 items) :key) "C-x f"))
      (should (plist-get (nth 1 items) :checked))
      (should-not (lme-popup--enabled-p (nth 4 items)))
      (should (equal (mapcar (lambda (item) (plist-get item :label))
                             (lme-popup--submenu (nth 3 items)))
                     '("Inner"))))))

(defun lme-test--choose (label)
  "Return a stand-in for `lme-popup-choose' that picks the item LABEL.
It follows submenus, as a click through a hierarchical menu does."
  (lambda (items &rest _)
    (named-let pick ((items items))
      (seq-some (lambda (item)
                  (when (consp item)
                    (cond ((equal (plist-get item :label) label) item)
                          ((plist-get item :submenu) (pick (lme-popup--submenu item))))))
                items))))

(ert-deftest lme-test-x-popup-menu-keymap ()
  "A keymap popped up by `x-popup-menu' answers with the chosen key path,
as the project tree's right-click menu looks its command up by it."
  (let ((map (make-sparse-keymap "Tree"))
        (sub (make-sparse-keymap "New")))
    (define-key sub [file] '(menu-item "File" find-file))
    (define-key map [new] `(menu-item "New" ,sub))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (cl-letf (((symbol-function 'lme-popup-available-p) #'always)
              ((symbol-function 'lme-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lme-popup-choose) (lme-test--choose "File")))
      (let ((choice (lme-popup--x-popup-menu #'ignore t map)))
        (should (equal choice '(new file)))
        (should (eq (lookup-key map (apply #'vector choice)) 'find-file))))
    (cl-letf (((symbol-function 'lme-popup-available-p) #'always)
              ((symbol-function 'lme-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lme-popup-choose) #'ignore))
      (should-not (lme-popup--x-popup-menu #'ignore t map)))))

(ert-deftest lme-test-x-popup-menu-panes ()
  "A menu of panes answers with the chosen item's value; lone strings
cannot be chosen, and panes are parted by separators."
  (let ((menu '("Spelling" ("" ("teh" . teh) "Suggestions") ("Actions" ("Save" . save) "--"))))
    (should (equal (mapcar (lambda (item) (if (consp item) (plist-get item :label) item))
                           (lme-popup--pane-items menu))
                   '("teh" "Suggestions" separator "Actions" "Save" separator)))
    (should-not (lme-popup--enabled-p (nth 1 (lme-popup--pane-items menu))))
    (cl-letf (((symbol-function 'lme-popup-available-p) #'always)
              ((symbol-function 'lme-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lme-popup-choose) (lme-test--choose "Save")))
      (should (eq (lme-popup--x-popup-menu #'ignore t menu) 'save)))))

(ert-deftest lme-test-x-popup-menu-passes-through ()
  "Without a position, or a display to draw on, the original menu runs."
  (let ((map (make-sparse-keymap "Tree")))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (should (eq (lme-popup--x-popup-menu (lambda (&rest _) 'native) nil map) 'native))
    (should (eq (lme-popup--x-popup-menu (lambda (&rest _) 'native) t map) 'native))))

(ert-deftest lme-test-right-click-defers-to-local-command ()
  "A buffer with a right-click command of its own keeps it."
  (with-temp-buffer
    (should-not (lme-popup--local-right-click-p nil))
    (let ((map (make-sparse-keymap)))
      (keymap-set map "<mouse-3>" #'rename-file)
      (use-local-map map)
      (should (lme-popup--local-right-click-p nil)))))

(ert-deftest lme-test-popups-fall-back ()
  "Without a graphical display, menus and alerts ask in the minibuffer."
  (should-not (lme-popup-available-p))
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Two")))
    (should (equal (plist-get (lme-popup-menu '((:label "One" :action ignore)
                                                (:label "Two" :action ignore)))
                              :label)
                   "Two")))
  (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "Save"))))
    (should (eq (lme-popup-alert 'caution "Save?" '(("Cancel" cancel cancel)
                                                   ("Save" save default)))
                'save))))

(ert-deftest lme-test-alert-text ()
  "An alert shows its message, its detail and a button per choice."
  (let* ((lme-platinum--palette (lme-platinum--derive-palette "#FFFFFF" "#000000"
                                                              "#777777" "#6363CE"))
         (text (lme-popup-alert-text 'caution "Save?" "Changes will be lost."
                                     '(("Cancel" cancel cancel) ("Save" save default))
                                     nil))
         (buttons nil))
    (should (string-search "Save?" text))
    (should (string-search "Changes will be lost." text))
    (dotimes (i (length text))
      (when-let* ((value (get-text-property i 'lme-popup-button text)))
        (cl-pushnew value buttons)))
    (should (equal (sort buttons #'string<) '(cancel save)))))

(ert-deftest lme-test-title-bar-clicks ()
  "A title bar brings its window forward, rolls up on a double click, and
pops up its buffers on a Meta click."
  (should (keymap-lookup lme-platinum--title-map "<tab-line> <mouse-1>"))
  (should (keymap-lookup lme-platinum--title-map "<tab-line> <double-mouse-1>"))
  (should (keymap-lookup lme-platinum--title-map "<tab-line> M-<mouse-1>")))

(ert-deftest lme-test-shade-or-zoom ()
  "A window alone in its column zooms; one sharing it rolls up."
  (save-window-excursion
    (delete-other-windows)
    (let ((zoomed nil) (shaded nil))
      (cl-letf (((symbol-function 'lme-platinum-zoom-window) (lambda (_) (setq zoomed t)))
                ((symbol-function 'lme-platinum-collapse-window) (lambda (_) (setq shaded t))))
        (lme-platinum-shade-or-zoom (selected-window))
        (should (and zoomed (not shaded)))
        (setq zoomed nil)
        (lme-platinum-shade-or-zoom (split-window-below))
        (should (and shaded (not zoomed)))))))

(ert-deftest lme-test-status-drawings ()
  "The battery and the load meter draw at any level."
  (let ((palette (lme-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (dolist (percent '(0 5 50 100))
      (should (eq (dom-tag (lme-platinum-draw-battery palette percent t)) 'svg))
      (should (eq (dom-tag (lme-platinum-draw-battery palette percent nil)) 'svg)))
    (dolist (fraction '(0.0 0.5 1.5))
      (should (eq (dom-tag (lme-platinum-draw-meter palette fraction)) 'svg)))))

(require 'time)
(defvar load)
(defvar mail)

(ert-deftest lme-test-status-notes ()
  "What Emacs computes for the time and the battery is noted for the menu bar."
  (let ((load " 1.25") (mail t) (lme-platinum--load nil) (lme-platinum--mail nil))
    (lme-platinum--note-time)
    (should (equal lme-platinum--load "1.25"))
    (should (eq lme-platinum--mail t)))
  (let ((lme-platinum--battery nil))
    (lme-platinum--note-battery '((?p . "87")))
    (should (equal (alist-get ?p lme-platinum--battery) "87"))))

(ert-deftest lme-test-menu-status ()
  "The menu bar keeps other status text, and adds the time and mail when on."
  (let ((labels nil)
        (lme-platinum--palette (lme-platinum--derive-palette "#FFFFFF" "#000000"
                                                             "#777777" "#6363CE"))
        (global-mode-string '("" "λ 2" display-time-string))
        (display-time-string "9:41AM")
        (display-time-mode t)
        (display-time-format "%-I:%M %p")
        (lme-platinum--load "")
        (lme-platinum--mail nil))
    ;; A batch Emacs formats every mode line construct as "".
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (construct &rest _)
                 (mapconcat (lambda (part) (if (stringp part) part "")) (ensure-list construct) ""))))
      (setq labels (mapcar (lambda (item) (substring-no-properties (nth 2 item)))
                           (lme-platinum-menu-status))))
    (progn
      (should (string-search "λ 2" (nth 0 labels)))
      (should-not (seq-some (lambda (label) (string-search "9:41AM" label)) labels))
      (should (= (length labels) 3))))
  (let ((global-mode-string nil) (display-time-mode nil))
    (should-not (lme-platinum-menu-status))))

(ert-deftest lme-test-age-short-date ()
  "Old sessions are dated as Finder dated files; recent ones by their age."
  (let ((now (float-time (encode-time '(0 0 12 13 10 2026 nil -1 nil)))))
    (should (equal (lme-agent--age (- now 120) now) "2m"))
    (should (equal (lme-agent--age (float-time (encode-time '(0 0 12 13 5 2026 nil -1 nil))) now)
                   "5/13/26"))))

(ert-deftest lme-test-row-shades-sorted-column ()
  "A row's right-hand column carries the sorted column's face to the edge."
  (cl-letf (((symbol-function 'lme-sidebar--column-pixels) (lambda () 100)))
    (let ((row (lme-sidebar--row "" "name" "3h" 300)))
      (should (string-search "3h" row))
      (should (equal (get-text-property (1- (length row)) 'face row) 'lme-platinum-sorted)))))

;;;; Main area columns

(defmacro lme-test--with-layout (&rest body)
  "Run BODY in a frame of one main window, with the columns' rules on."
  `(save-window-excursion
     (let ((display-buffer-alist
            '((lme-main-area--astray-p lme-main-area-display-file)
              (lme-main-area--session-p lme-main-area-display-session)))
           (switch-to-buffer-obey-display-actions t)
           (window-min-width 4))
       (delete-other-windows)
       ,@body)))

(defun lme-test--session (name)
  "Return a fresh buffer NAME laid out as a session."
  (with-current-buffer (get-buffer-create name)
    (setq lme-conversation-buffer t)
    (current-buffer)))

(defun lme-test--file (name)
  "Return a buffer visiting a temporary file NAME."
  (find-file-noselect (expand-file-name name temporary-file-directory)))

(defun lme-test--left-p (a b)
  "Return non-nil when the window of buffer A is left of B's."
  (< (car (window-edges (get-buffer-window a))) (car (window-edges (get-buffer-window b)))))

(ert-deftest lme-test-file-from-session-goes-right ()
  "A file opened from a session window opens to its right; the session stays."
  (lme-test--with-layout
   (let ((session (lme-test--session "lme-test session"))
         (file (lme-test--file "lme-test-a.txt")))
     (switch-to-buffer session)
     (switch-to-buffer file)
     (should (get-buffer-window session))
     (should (lme-test--left-p session file))
     (should (eq (window-buffer (selected-window)) file)))))

(ert-deftest lme-test-session-from-file-goes-left ()
  "A session shown from a file window goes to the left of the file."
  (lme-test--with-layout
   (let ((session (lme-test--session "lme-test session 2"))
         (file (lme-test--file "lme-test-b.txt")))
     (switch-to-buffer file)
     (switch-to-buffer session)
     (should (get-buffer-window file))
     (should (lme-test--left-p session file)))))

(ert-deftest lme-test-session-reuses-session-column ()
  "With both columns open, a session from the file side replaces the session."
  (lme-test--with-layout
   (let ((one (lme-test--session "lme-test session 3"))
         (two (lme-test--session "lme-test session 4"))
         (file (lme-test--file "lme-test-c.txt")))
     (switch-to-buffer one)
     (switch-to-buffer file)
     (switch-to-buffer two)
     (should (= (length (window-list)) 2))
     (should-not (get-buffer-window one))
     (should (lme-test--left-p two file)))))

(ert-deftest lme-test-other-buffer-from-session-goes-right ()
  "Any buffer switched to from a session window goes right, not over it."
  (lme-test--with-layout
   (let ((session (lme-test--session "lme-test session 6"))
         (other (get-buffer-create "lme-test other")))
     (switch-to-buffer session)
     (switch-to-buffer other)
     (should (get-buffer-window session))
     (should (lme-test--left-p session other)))))

(ert-deftest lme-test-popups-are-left-alone ()
  "A buffer displayed as a popup is not taken over by the columns."
  (lme-test--with-layout
   (let ((session (lme-test--session "lme-test session 7")))
     (switch-to-buffer session)
     (should-not (lme-main-area--astray-p (get-buffer-create "lme-test popup")
                                          '(display-buffer-pop-up-window))))))

(define-derived-mode lme-test-chat-mode special-mode "Chat"
  "A stand-in for an agent's chat mode.")

(ert-deftest lme-test-chat-mode-is-a-session ()
  "A buffer in a registered chat mode keeps to the session column at once."
  (lme-test--with-layout
   (let ((lme-conversation-modes '(lme-test-chat-mode))
         (file (lme-test--file "lme-test-f.txt"))
         (chat (with-current-buffer (get-buffer-create "lme-test chat")
                 (lme-test-chat-mode)
                 (current-buffer))))
     (switch-to-buffer file)
     (pop-to-buffer chat)
     (should (lme-test--left-p chat file)))))

(ert-deftest lme-test-second-file-stays-right ()
  "A second file opened from the session replaces the file, not the session."
  (lme-test--with-layout
   (let ((session (lme-test--session "lme-test session 5"))
         (a (lme-test--file "lme-test-d.txt"))
         (b (lme-test--file "lme-test-e.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (select-window (get-buffer-window session))
     (switch-to-buffer b)
     (should (= (length (window-list)) 2))
     (should (lme-test--left-p session b)))))

(ert-deftest lme-test-menu-bar-only-outside-the-frame ()
  "A graphical frame keeps its menu bar only where NS draws it, outside."
  (let (set)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (frame) (not (eq frame 'tty))))
              ((symbol-function 'framep)
               (lambda (frame) (if (eq frame 'tty) t frame)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (push (list frame parameter value) set))))
      (lme-frame-menu-bar 'ns)
      (lme-frame-menu-bar 'tty)
      (lme-frame-menu-bar 'x))
    (should (equal set '((x menu-bar-lines 0))))))

(ert-deftest lme-test-default-font-first-installed ()
  "The default face takes the first preferred family that is installed,
unless the user customized it."
  (let (set)
    (cl-letf (((symbol-function 'font-family-list)
               (lambda (&optional _frame) '("Andale Mono" "Menlo")))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest attributes)
                 (push (cons face attributes) set))))
      (lme-pin-default-font)
      (should (equal set '((default :family "Menlo" :height 120))))
      (setq set nil)
      (cl-letf (((get 'default 'saved-face) '((t :family "Iosevka"))))
        (lme-pin-default-font))
      (should-not set))))

(defun lme-test--x-properties (checks)
  "Stand in for `x-window-property', reading CHECKS.
CHECKS maps a window (0 for the root) to the window its
_NET_SUPPORTING_WM_CHECK names, or to `gone' for a destroyed window."
  (lambda (_property _frame _type source &rest _)
    (pcase (alist-get source checks)
      ('nil nil)
      ('gone (error "BadWindow"))
      (check check))))

(ert-deftest lme-test-exwm-only-without-a-window-manager ()
  "Emacs manages an X display only when no window manager runs on it."
  (let ((lme-window-manager 'auto))
    (cl-letf (((symbol-function 'framep) (lambda (frame) frame)))
      (cl-letf (((symbol-function 'x-window-property)
                 (lme-test--x-properties nil)))
        (should-not (lme-exwm-wanted-p 'ns))
        (should (lme-exwm-wanted-p 'x))
        (let ((lme-window-manager nil))
          (should-not (lme-exwm-wanted-p 'x))))
      ;; A running window manager: the root names a window naming itself.
      (cl-letf (((symbol-function 'x-window-property)
                 (lme-test--x-properties '((0 . 7) (7 . 7)))))
        (should-not (lme-exwm-wanted-p 'x)))
      ;; A window manager that quit left its check window's name behind.
      (cl-letf (((symbol-function 'x-window-property)
                 (lme-test--x-properties '((0 . 7) (7 . gone)))))
        (should (lme-exwm-wanted-p 'x))))))

;;;; The browser's history and bookmarks

(defmacro lme-test--with-places (&rest body)
  "Run BODY with an empty history and bookmark list, in files of their own."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "lme-places" t))
          (lme-browser-history-file (expand-file-name "history.eld" directory))
          (lme-browser--history nil)
          (lme-browser--history-loaded nil)
          (lme-browser--history-timer nil)
          (bookmark-default-file (expand-file-name "bookmarks" directory))
          (bookmark-alist nil)
          (bookmark-bookmarks-timestamp nil)
          (bookmark-save-flag nil))
     (unwind-protect
         (cl-letf (((symbol-function 'run-with-idle-timer) #'ignore))
           ,@body)
       (delete-directory directory t))))

(ert-deftest lme-test-history-keeps-each-page-once ()
  "A page visited again moves to the top with its visits counted; only web
and file pages are kept, and only so many."
  (lme-test--with-places
    (let ((lme-browser-history-size 2))
      (lme-browser-history-add "https://a.example/" "A")
      (lme-browser-history-add "about:blank" "")
      (lme-browser-history-add "https://b.example/" "B")
      (lme-browser-history-add "https://a.example/" "")
      (should (equal (mapcar #'car lme-browser--history)
                     '("https://a.example/" "https://b.example/")))
      (should (equal (nth 1 (car lme-browser--history)) "A"))
      (should (= (nth 3 (car lme-browser--history)) 2))
      (lme-browser-history-add "https://c.example/" "C")
      (should (equal (mapcar #'car lme-browser--history)
                     '("https://c.example/" "https://a.example/"))))))

(ert-deftest lme-test-history-is-saved-and-read ()
  "The history is written to its file and read back from it."
  (lme-test--with-places
    (lme-browser-history-add "https://a.example/" "A")
    (lme-browser--save-history)
    (setq lme-browser--history nil
          lme-browser--history-loaded nil)
    (should (equal (car (car (lme-browser--load-history))) "https://a.example/"))))

(ert-deftest lme-test-history-days ()
  "History is listed under the day of each visit."
  (let ((now (float-time)))
    (should (equal (lme-browser--history-day now) "Today"))
    (should (equal (lme-browser--history-day (- now 86400)) "Yesterday"))
    (should (equal (lme-browser--history-day (- now (* 10 86400))) "Last Week"))
    (should (equal (lme-browser--history-day (- now (* 40 86400))) "Earlier"))))

(ert-deftest lme-test-bookmarks-are-emacs-bookmarks ()
  "A page bookmarked is an Emacs bookmark xwidget can open; it is kept
once, and the browser lists only web pages' bookmarks, oldest first."
  (lme-test--with-places
    (bookmark-store "notes" '((filename . "/tmp/notes.org")) nil)
    (cl-letf (((symbol-function 'lme-browser--shown-buffer) (lambda () (current-buffer)))
              ((symbol-function 'lme-browser--title) (lambda (_) "Example"))
              ((symbol-function 'lme-browser--uri) (lambda (_) "https://example.com/")))
      (lme-browser-add-bookmark)
      (lme-browser-add-bookmark))
    (should (equal (lme-browser-bookmarks) '(("Example" . "https://example.com/"))))
    (should (eq (bookmark-prop-get "Example" 'handler) 'xwidget-webkit-bookmark-jump-handler))
    (should (equal (lme-browser--bookmark-name "Example") "Example <2>"))
    (let ((labels (mapcar (lambda (item) (and (consp item) (plist-get item :label)))
                          (lme-browser--bookmark-items))))
      (should (equal labels '("Add Bookmark" "Edit Bookmarks…" nil "Example"))))
    (lme-browser-delete-bookmark "Example")
    (should-not (lme-browser-bookmarks))))

(ert-deftest lme-test-browser-keys-show-in-menus ()
  "The browser's Command keys show beside their menu items."
  (should (equal (lme-browser--key 'lme-browser-add-bookmark) "M-d"))
  (let ((lme-browser--navigation '(:page t :back t :forward t)))
    (should (equal (plist-get (car (lme-browser--go-items)) :key) "M-["))))

(ert-deftest lme-test-throbber-turns-while-loading ()
  "The throbber turns while the shown page loads and rests once it is done."
  (let ((lme-browser--throbber-frame nil)
        (lme-browser--throbber-timer nil)
        (loading t))
    (with-temp-buffer
      (setq-local xwidget-webkit--loading-p t)
      (cl-letf (((symbol-function 'lme-browser--shown-buffer)
                 (let ((buffer (current-buffer)))
                   (lambda () (setq-local xwidget-webkit--loading-p loading) buffer))))
        (lme-browser--turn-throbber)
        (lme-browser--turn-throbber)
        (should (eql lme-browser--throbber-frame 1))
        (setq loading nil)
        (lme-browser--turn-throbber)
        (should-not lme-browser--throbber-frame)))))

;;;; Settings

(ert-deftest lme-test-settings-panels-draw ()
  "Each panel draws its settings: the themes, the desktop's picture and its
positions, the keys, the browser's pages."
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'always)
            ((symbol-function 'lme-browser--load-history) (lambda () '(("u" "t" 0 1))))
            ((symbol-function 'lme-browser-bookmarks) (lambda () nil)))
    (with-current-buffer (lme-settings--buffer)
      (pcase-dolist (`(,panel . ,labels)
                     '((themes "Dracula" "Platinum")
                       (desktop "From the theme" "Fill" "Tile" "Arrange Icons")
                       (keyboard "Vim keys" "Emacs keys" "next start")
                       (browser "Home Page" "Clear History" "Edit Bookmarks")))
        (lme-settings-select panel)
        (dolist (label labels)
          (should (string-search label (buffer-string))))
        (should (string-search (capitalize (symbol-name panel)) lme-platinum-title))))))

(ert-deftest lme-test-settings-radio-saves ()
  "Pressing a radio button saves its choice and draws the panel again."
  (let ((saved nil)
        (lme-editing-style 'vim))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value) (push (cons variable value) saved))))
      (with-current-buffer (lme-settings--buffer)
        (lme-settings-select 'keyboard)
        (let ((button (next-button (point-min))))
          (while (and button (not (string-search "Emacs keys" (button-label button))))
            (setq button (next-button (button-end button))))
          (should button)
          (button-activate button))
        (should (equal (car saved) '(lme-editing-style . emacs)))))))

(ert-deftest lme-test-settings-in-menus ()
  "The λ menu opens each panel; the Application menu lists Settings while
its window is open."
  (let ((settings (seq-find (lambda (item) (and (consp item) (equal (plist-get item :label) "Settings")))
                            (lme-platinum-lambda-items))))
    (should settings)
    (should (member "Themes…" (mapcar (lambda (item) (plist-get item :label))
                                      (funcall (plist-get settings :submenu))))))
  (cl-letf (((symbol-function 'lme-xwidget-available-p) #'ignore))
    (cl-letf (((symbol-function 'lme-settings-open-p) #'ignore))
      (should-not (member "Settings" (mapcar (lambda (item) (plist-get item :label))
                                             (lme-apps-application-items)))))
    (cl-letf (((symbol-function 'lme-settings-open-p) #'always))
      (should (member "Settings" (mapcar (lambda (item) (plist-get item :label))
                                         (lme-apps-application-items)))))))

(provide 'lme-test)
;;; lme-test.el ends here
