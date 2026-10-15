;;; lispm-desktop-test.el --- Tests for lispm-desktop  -*- lexical-binding: t; -*-

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

;; Run with `make test'.  They need none of the packages lispm-desktop
;; installs: setup functions are not called, only what they decide with.

;;; Code:

(require 'ert)
(require 'lispm-desktop)

(ert-deftest lispm-desktop-test-agent-installed-p ()
  "An agent counts as installed exactly when its CLI is on PATH."
  (let ((lispm-desktop-agent-programs '((claude . "lispm-desktop-test-no-such-program")
                                        (codex . "emacs"))))
    (should-not (lispm-desktop-agent-installed-p 'claude))
    (should (lispm-desktop-agent-installed-p 'codex))))

(ert-deftest lispm-desktop-test-launch-requires-installed-cli ()
  "Starting an agent whose CLI is missing is a user error, before anything runs."
  (let ((lispm-desktop-agent-programs '((claude . "lispm-desktop-test-no-such-program"))))
    (should-error (lispm-desktop-agent--launch 'claude temporary-file-directory "x" nil)
                  :type 'user-error)))

(ert-deftest lispm-desktop-test-sessions-skip-missing-agents ()
  "No record of an agent whose CLI is missing is read or listed."
  (let ((lispm-desktop-agent-emacs-agents nil)
        (read nil))
    (cl-letf (((symbol-function 'lispm-desktop-agent-installed-p) #'ignore)
              ((symbol-function 'lispm-desktop-agent--claude-files) (lambda () (setq read t) nil))
              ((symbol-function 'lispm-desktop-agent--codex-files) (lambda () (setq read t) nil))
              ((symbol-function 'lispm-desktop-agent--codex-thread-names) (lambda () (setq read t) nil))
              ((symbol-function 'lispm-desktop-agent-hermes-sessions) (lambda () (setq read t) nil)))
      (should (null (lispm-desktop-agent-sessions-list)))
      (should-not read))))

(ert-deftest lispm-desktop-test-sessions-include-emacs-agents ()
  "Sessions of agents running inside Emacs are listed, newest first."
  (let ((lispm-desktop-agent-emacs-agents
         `((lispm :sessions ,(lambda ()
                               (list (list :agent 'lispm :id "a" :time 1.0)
                                     (list :agent 'lispm :id "b" :time 2.0)))))))
    (cl-letf (((symbol-function 'lispm-desktop-agent-installed-p) #'ignore))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) (lispm-desktop-agent-sessions-list))
                     '("b" "a"))))))

(ert-deftest lispm-desktop-test-first-start-in-batch ()
  "A batch Emacs settles on Emacs keys and Dracula without asking.
Choices already made are kept, and the start that finds them made is
not a first start."
  (let ((lispm-desktop-editing-style nil)
        (lispm-desktop-theme nil))
    (cl-letf (((symbol-function 'lispm-desktop--read-editing-style)
               (lambda () (error "Asked in batch")))
              ((symbol-function 'lispm-desktop--read-theme)
               (lambda () (error "Asked in batch"))))
      (should (lispm-desktop--first-start))
      (should (eq lispm-desktop-editing-style 'emacs))
      (should (eq lispm-desktop-theme 'dracula))))
  (let ((lispm-desktop-editing-style 'vim)
        (lispm-desktop-theme 'platinum))
    (should-not (lispm-desktop--first-start))
    (should (eq lispm-desktop-editing-style 'vim))
    (should (eq lispm-desktop-theme 'platinum))))

(ert-deftest lispm-desktop-test-lispm-requirements ()
  "Of a package's requirements, those not installed are returned; Emacs never."
  (let ((file (make-temp-file "lispm-desktop-test-" nil ".el"
                              (concat ";;; lispm-desktop-test-pkg.el --- A test  -*- lexical-binding: t; -*-\n"
                                      ";; Version: 0.1\n"
                                      ";; Package-Requires: ((emacs \"30.1\") (lispm-desktop-test-missing \"1.0\"))\n"
                                      ";;; Commentary:\n;;; Code:\n"
                                      "(provide 'lispm-desktop-test-pkg)\n;;; lispm-desktop-test-pkg.el ends here\n"))))
    (unwind-protect
        (should (equal (mapcar #'car (lispm-desktop-lispm--requirements file)) '(lispm-desktop-test-missing)))
      (delete-file file))))

(ert-deftest lispm-desktop-test-lispm-ensure-uses-checkout ()
  "A LispM already on `load-path' is used: `load-path' is left as it is."
  (let ((load-path '("/elsewhere")))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/checkout/lisp/lispm.el"))
              ((symbol-function 'lispm-desktop-lispm--requirements) #'ignore))
      (lispm-desktop-lispm-ensure)
      (should (equal load-path '("/elsewhere"))))))

(ert-deftest lispm-desktop-test-lispm-ensure-uses-the-repository ()
  "With no LispM on `load-path', the repository's own lisp/ joins it, the
one beside the desktop."
  (let ((load-path nil))
    (cl-letf (((symbol-function 'locate-library) #'ignore))
      (lispm-desktop-lispm-ensure)
      (should (equal load-path (list lispm-desktop-lispm-directory)))))
  (should (file-exists-p (expand-file-name "lispm.el" lispm-desktop-lispm-directory))))

(ert-deftest lispm-desktop-test-directory ()
  "`lispm-desktop-directory' is the checkout, holding the agents' icons and the Hermes bridge."
  (should (file-exists-p (expand-file-name "lisp/lispm-desktop.el" lispm-desktop-directory)))
  (should (file-exists-p lispm-desktop-agent-mcp-hermes-bridge))
  (should (file-directory-p lispm-desktop-agent-icon-directory)))


;;;; Platinum

(ert-deftest lispm-desktop-test-title-layout-centres ()
  "A short title sits in the middle of its bar, with stripes either side."
  (pcase-let ((`(,left ,title ,right) (lispm-desktop-platinum-title-layout 600 100)))
    (should (= title 100))
    (should (> left 12))
    (should (> right 12))
    ;; The bar's chrome adds up to its width.
    (let ((s lispm-desktop-platinum-box-size))
      (should (= (+ 8 s 6 left 8 title 8 right 6 s 4 s 8) 600)))))

(ert-deftest lispm-desktop-test-title-layout-narrows-long-titles ()
  "A title too wide for its bar is narrowed, keeping stripes on both sides."
  (pcase-let ((`(,left ,title ,right) (lispm-desktop-platinum-title-layout 300 1000)))
    (should (< title 300))
    (should (>= left 12))
    (should (>= right 12))))

(ert-deftest lispm-desktop-test-palette-derives-from-theme ()
  "The chrome is the content darkened; bevels lean to the text and to black."
  (let ((palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (should (equal (plist-get palette :content) "#282a36"))
    (should (equal (plist-get palette :accent) "#bd93f9"))
    (cl-flet ((lum (color) (apply #'+ (lispm-desktop-tab-bar--rgb color))))
      (should (< (lum (plist-get palette :chrome)) (lum "#282a36")))
      (should (> (lum (plist-get palette :light)) (lum (plist-get palette :chrome))))
      (should (< (lum (plist-get palette :shade)) (lum (plist-get palette :chrome)))))))

(ert-deftest lispm-desktop-test-boxes-draw ()
  "Every box kind draws, enabled, modified and disabled."
  (let ((palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (dolist (kind '(close zoom collapse))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind 22)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind 22 t)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind 22 nil t)) 'svg)))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-stripes palette 50 22)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-triangle palette t)) 'svg))))

(ert-deftest lispm-desktop-test-rgb-reads-hex-without-a-display ()
  "A #RRGGBB colour is read exactly, even in batch."
  (should (equal (lispm-desktop-tab-bar--rgb "#ff0080") (list 1.0 0.0 (/ 128 255.0))))
  (should (equal (lispm-desktop-tab-bar--blend "#000000" "#ff0080" 1.0) "#ff0080")))

(ert-deftest lispm-desktop-test-window-controls ()
  "A lone window zooms but cannot roll up; a side window only closes."
  (save-window-excursion
    (delete-other-windows)
    (should (equal (lispm-desktop-platinum--window-controls (selected-window)) '(on on off)))
    (let ((below (split-window-below)))
      (should (equal (lispm-desktop-platinum--window-controls below) '(on on on))))
    (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test-side")
                                               '((side . left)))))
      (should (equal (lispm-desktop-platinum--window-controls side) '(on off off))))))

(ert-deftest lispm-desktop-test-chrome-controls ()
  "Chrome, such as the browser's address window, only closes, and its close
box runs the buffer's own close function."
  (save-window-excursion
    (delete-other-windows)
    (let ((chrome (split-window-below))
          (closed nil))
      (set-window-parameter chrome 'lispm-desktop-chrome t)
      (should (equal (lispm-desktop-platinum--window-controls chrome) '(on off off)))
      (with-temp-buffer
        (setq-local lispm-desktop-platinum-box-functions (list :close (lambda (window) (setq closed window))))
        (set-window-buffer chrome (current-buffer))
        (lispm-desktop-platinum-close-window chrome)
        (should (eq closed chrome))
        (should (buffer-live-p (current-buffer)))))))

(defun lispm-desktop-test--menu-bar-titles ()
  "Return the keys of the menu bar's left end."
  (mapcar #'car (lispm-desktop-platinum-menu-bar)))

(ert-deftest lispm-desktop-test-web-title ()
  "Web follows the views where the browser can run, lit while it is in
front; without xwidgets it is left out."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always)
            ((symbol-function 'lispm-desktop-browser-workspace-p) #'ignore))
    (let ((bar (lispm-desktop-platinum-menu-bar)))
      (should (equal (last (mapcar #'car bar) 2) '(lispm-desktop-view-shells lispm-desktop-view-web)))
      (should-not (text-property-any 0 (length (nth 2 (car (last bar))))
                                     'face 'lispm-desktop-platinum-menu-title-active
                                     (nth 2 (car (last bar)))))))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always)
            ((symbol-function 'lispm-desktop-browser-workspace-p) #'always))
    (let ((web (nth 2 (car (last (lispm-desktop-platinum-menu-bar))))))
      (should (memq 'lispm-desktop-platinum-menu-title-active
                    (ensure-list (get-text-property 0 'face web))))))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (should-not (memq 'lispm-desktop-view-web (lispm-desktop-test--menu-bar-titles)))))

(ert-deftest lispm-desktop-test-desktop-darkens-selected-icons ()
  "A selected icon is its SVG with every colour halved."
  (should (equal (lispm-desktop--darken "<rect fill=\"#FF8040\"/><rect fill=\"#000000\"/>")
                 "<rect fill=\"#7F4020\"/><rect fill=\"#000000\"/>")))

(ert-deftest lispm-desktop-test-desktop-items ()
  "The browser's icon shows where the browser can run; the Trash is last."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (should (equal (mapcar #'car (lispm-desktop--items)) '(home editor control-panels trash))))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always))
    (should (equal (mapcar #'car (lispm-desktop--items))
                   '(home editor browser control-panels trash)))))

(ert-deftest lispm-desktop-test-desktop-icons-stand-where-put ()
  "Icons with no place stand in the column at the right edge, the Trash in
the bottom corner; one put elsewhere stands there, measured from the edges
nearest it, kept whole on the desktop; the others keep to free cells."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10))))
    (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
      (let ((lispm-desktop-icon-places nil))
        (let ((layout (lispm-desktop--layout 800 600)))
          (should (equal (mapcar #'cdr layout)
                         `((,(- 800 lispm-desktop-margin cell-width) ,lispm-desktop-margin)
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin cell-height lispm-desktop-gap))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin (* 2 (+ cell-height lispm-desktop-gap))))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(- 600 lispm-desktop-margin cell-height)))))))
      ;; Home put at the top left; Emacs keeps its cell, the second.
      (let ((lispm-desktop-icon-places '((home :left 20 :top 30))))
        (let ((layout (lispm-desktop--layout 800 600)))
          (should (equal (cdr (seq-find (lambda (entry) (eq (car (car entry)) 'home)) layout))
                         '(20 30)))
          (should (equal (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor)) layout))
                         lispm-desktop-margin))))
      ;; An icon put on Emacs's cell pushes it down the column.
      (let ((lispm-desktop-icon-places `((home :right ,lispm-desktop-margin :top ,lispm-desktop-margin)
                                         (trash :right ,lispm-desktop-margin :top ,lispm-desktop-margin))))
        (should (> (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor))
                                    (lispm-desktop--layout 800 600)))
                   lispm-desktop-margin)))
      ;; A place past the edge is kept whole on the desktop.
      (should (equal (lispm-desktop--place-xy '(:left 2000 :top -5) 800 600)
                     (cons (- 800 cell-width) 0)))
      ;; Measured from the nearest edges, so it follows them.
      (should (equal (lispm-desktop--xy-place 700 500 800 600)
                     (list :right (- 800 700 cell-width) :bottom (- 600 500 cell-height))))
      (should (equal (lispm-desktop--xy-place 10 20 800 600) '(:left 10 :top 20)))
      (should (equal (lispm-desktop--place-xy (lispm-desktop--xy-place 700 20 800 600) 1000 600)
                     '(900 . 20))))))

(ert-deftest lispm-desktop-test-desktop-drop-keeps-the-others ()
  "Dropping an icon keeps every other where it stands and saves them all."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let ((lispm-desktop-icon-places nil)
          (pitch (+ (cdr (lispm-desktop--cell-size)) lispm-desktop-gap)))
      ;; Dropped near the top left, it lands on the grid cell nearest.
      (lispm-desktop--drop 'home 100 120 nil)
      (should (equal (alist-get 'home lispm-desktop-icon-places)
                     (list :left (+ lispm-desktop-margin lispm-desktop-column)
                           :top (+ lispm-desktop-margin (* 2 pitch)))))
      (should (equal (alist-get 'editor lispm-desktop-icon-places)
                     (lispm-desktop--xy-place (- 800 lispm-desktop-margin lispm-desktop-column)
                                              (+ lispm-desktop-margin pitch)
                                              800 600)))
      ;; Dropped on Emacs's cell, it takes the free cell nearest instead.
      (lispm-desktop--drop 'home (- 800 lispm-desktop-margin lispm-desktop-column)
                           (+ lispm-desktop-margin pitch 5) nil)
      (let ((home (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600))
            (editor (lispm-desktop--place-xy (alist-get 'editor lispm-desktop-icon-places) 800 600)))
        (should-not (equal home editor))
        (should (lispm-desktop--cell-free-p home (list (list (car editor) (cdr editor))))))
      ;; Near the bottom right corner, it lines up with the Trash's row.
      (lispm-desktop--drop 'home (- 800 200) (- 600 lispm-desktop-margin 60) nil)
      (should (equal (plist-get (alist-get 'home lispm-desktop-icon-places) :bottom)
                     lispm-desktop-margin))
      (should (zerop (mod (- (plist-get (alist-get 'home lispm-desktop-icon-places) :right)
                             lispm-desktop-margin)
                          lispm-desktop-column)))
      (lispm-desktop-arrange-icons)
      (should-not lispm-desktop-icon-places))))

(ert-deftest lispm-desktop-test-desktop-drag-survives-empty-reads ()
  "Catching up with the mouse while input is pending but nothing can be read
yet does not end the drag: the icon is still dropped where it is let go."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frame) #'ignore)
            ((symbol-function 'lispm-desktop--show-copy) #'ignore)
            ((symbol-function 'lispm-desktop-popup--dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lispm-desktop-icon-places nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (moves 0)
           (pointer press)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
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
        (lispm-desktop--drag 'home nil press))
      (should (equal (alist-get 'home lispm-desktop-icon-places)
                     (lispm-desktop--snap (- (car cell) 100) (+ (cadr cell) 50) 800 600
                                          (mapcar #'cdr (seq-remove
                                                         (lambda (entry) (eq (car (car entry)) 'home))
                                                         (lispm-desktop--layout 800 600))))))
      (should-not unread-command-events))))

(ert-deftest lispm-desktop-test-desktop-right-click-menu ()
  "A right click on the desktop offers Arrange Icons and the picture; on an
icon, Open first."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (let ((shown nil)
          (key nil)
          (lispm-desktop-icon-places '((home :left 10 :top 10))))
      (cl-letf (((symbol-function 'lispm-desktop-mouse-select)
                 (lambda (_) (with-current-buffer (lispm-desktop-buffer)
                               (setq lispm-desktop--selected key))))
                ((symbol-function 'lispm-desktop-popup--frame-xy) (lambda (_) '(0 . 0)))
                ((symbol-function 'lispm-desktop-popup-menu) (lambda (items &rest _) (setq shown items))))
        (lispm-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Arrange Icons"))
        (should (plist-get (car shown) :enabled))
        (setq key 'home)
        (lispm-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Open"))
        (should (member "Desktop Picture…"
                        (mapcar (lambda (item) (and (consp item) (plist-get item :label))) shown)))))))

(ert-deftest lispm-desktop-test-desktop-double-click-opens-not-drags ()
  "A double click opens the icon, even when its first click strayed a few
pixels; a press let go after a jitter is a click, and a drop back on the
icon's own cell saves nothing."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frame) #'ignore)
            ((symbol-function 'lispm-desktop--show-copy) #'ignore)
            ((symbol-function 'lispm-desktop-popup--dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore))
    (let* ((lispm-desktop-icon-places nil)
           (lispm-desktop--last-click nil)
           (saved nil)
           (opened nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (pointer press)
           (moves nil)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (variable value) (setq saved t) (set variable value)))
                ((symbol-function 'lispm-desktop-open) (lambda () (setq opened t)))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'lispm-desktop-apps-set-frontmost) #'ignore)
                ((symbol-function 'lispm-desktop--select) #'ignore)
                ((symbol-function 'lispm-desktop--item-at) (lambda (_) 'home))
                ((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'input-pending-p) #'ignore)
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (if moves
                       (progn (setq pointer (pop moves)) '(mouse-movement (nil)))
                     '(mouse-1 (nil))))))
        (let ((event `(down-mouse-1 (nil 1 ,press 0))))
          ;; A first click that jitters 3 and 4 pixels is a click.
          (setq moves (list (cons (+ (car press) 3) (+ (cdr press) 4))))
          (lispm-desktop-mouse-select event)
          (should-not saved)
          (should (eq (car lispm-desktop--last-click) 'home))
          ;; The second press, quick and close by, opens without dragging.
          (setq moves (list (cons (+ (car press) 40) (cdr press))))
          (lispm-desktop-mouse-select event)
          (should opened)
          (should-not saved)
          (should moves)
          ;; A drag that comes back to the icon's own cell saves nothing.
          (setq moves (list (cons (+ (car press) 10) (+ (cdr press) 9)))
                opened nil)
          (lispm-desktop-mouse-select event)
          (should-not opened)
          (should-not saved))))))

(ert-deftest lispm-desktop-test-desktop-draws-the-dragged-copy ()
  "The dragged copy is its cell's piece of the desktop, the icon translucent
over it."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (let* ((lispm-desktop-picture nil)
           (svg (lispm-desktop--copy-svg 'home 50 60 800 600)))
      (should (string-match-p "viewBox=\"[0-9-]+ 60 " svg))
      (should (string-match-p (format "<rect width=\"800\" height=\"600\" fill=\"%s\"/>"
                                      (lispm-desktop--color))
                              svg))
      (should (string-suffix-p "Home</text></g></svg>" svg))
      (should (string-match-p "<g opacity=\"0.5\">" svg)))))

(ert-deftest lispm-desktop-test-desktop-picture-scaled-copy ()
  "A picture filling the desktop is drawn from a copy scaled to it once one
is made; until then, and for a picture at its own size, from itself."
  (let* ((lispm-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lispm-desktop-directory))
         (file lispm-desktop-picture)
         (time (file-attribute-modification-time (file-attributes file)))
         (started nil))
    (cl-letf (((symbol-function 'lispm-desktop--picture-size) (lambda (_) '(4000 . 3000)))
              ((symbol-function 'frame-scale-factor) (lambda (&rest _) 2.0))
              ((symbol-function 'lispm-desktop--scale-picture)
               (lambda (&rest args) (setq started args))))
      (let ((lispm-desktop-picture-position 'fill)
            (lispm-desktop--scaled nil))
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should (equal started (list file time 1600 1200)))
        (setq lispm-desktop--scaled (list file time 1600 1200 file))
        (setq started nil)
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should-not started)
        ;; Grown past the copy, the copy is made again.
        (lispm-desktop--picture-file 1000 600)
        (should started))
      (let ((lispm-desktop-picture-position 'center)
            (lispm-desktop--scaled nil))
        (setq started nil)
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should-not started)))))

(ert-deftest lispm-desktop-test-desktop-selects-and-names ()
  "The arrow keys step through the icons, wrapping; the selected one's name
is inverted, and a click finds the icon drawn under it, the Trash in the
bottom corner."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (with-current-buffer (lispm-desktop-buffer)
      (setq lispm-desktop--selected nil)
      (lispm-desktop-select-next)
      (should (eq lispm-desktop--selected 'home))
      (lispm-desktop-select-previous)
      (should (eq lispm-desktop--selected 'trash))
      (lispm-desktop-select-previous)
      (should (eq lispm-desktop--selected 'control-panels))
      (setq lispm-desktop--selected 'home)
      (let ((svg (lispm-desktop--svg 800 600)))
        (should (string-match-p "fill=\"#000000\"/><text[^>]*fill=\"#FFFFFF\">Home</text>" svg))
        (should (string-match-p "fill=\"#FFFFFF\"/><text[^>]*fill=\"#000000\">Trash</text>" svg))
        (pcase-let ((`(,_ ,left ,top ,right ,bottom) (assq 'home lispm-desktop--hits)))
          (should (eq (lispm-desktop--key-at (/ (+ left right) 2) (/ (+ top bottom) 2)) 'home)))
        (should (> (nth 2 (assq 'trash lispm-desktop--hits)) 400))
        (should-not (lispm-desktop--key-at 10 10)))
      (lispm-desktop-deselect)
      (should-not lispm-desktop--selected))))

(ert-deftest lispm-desktop-test-desktop-picture ()
  "A desktop picture lies over the desktop's colour as its position says."
  (let ((lispm-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lispm-desktop-directory)))
    (let ((lispm-desktop-picture-position 'fill))
      (should (string-match-p "<image href=\"globe-32.svg\"[^>]*xMidYMid slice"
                              (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-picture-position 'fit))
      (should (string-match-p "xMidYMid meet" (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-picture-position 'tile))
      (should (string-match-p "<pattern id=\"lispm-desktop-tile\"" (lispm-desktop--svg 800 600)))))
  (let ((lispm-desktop-picture nil)
        (lispm-desktop-color "#123456"))
    (let ((svg (lispm-desktop--svg 800 600)))
      (should (string-match-p "fill=\"#123456\"" svg))
      (should-not (string-match-p "<image" svg)))))

(ert-deftest lispm-desktop-test-desktop-color-is-hex ()
  "Any colour Emacs takes reaches the desktop's SVG written #RRGGBB: the
12-digit hex `read-color' returns, short hex, rgb:, and names, which a
graphic display resolves."
  (dolist (case '(("#483D8B" . "#483d8b")
                  ("#48483D3D8B8B" . "#483d8b")
                  ("#F00" . "#ff0000")
                  ("rgb:ff/80/00" . "#ff8000")
                  ("dark slate blue" . "#483d8b")
                  ("DarkSlateBlue" . "#483d8b")
                  ("gray50" . "#7f7f7f")))
    (should (equal (lispm-desktop--hex-color (car case)) (cdr case))))
  (should-not (lispm-desktop--hex-color "no such colour"))
  (should-not (lispm-desktop--hex-color ""))
  (should-not (lispm-desktop--hex-color nil))
  (cl-letf (((symbol-function 'display-graphic-p) #'always)
            ((symbol-function 'color-values)
             (lambda (color &optional _) (and (equal color "SystemWindow") '(65535 0 0)))))
    (should (equal (lispm-desktop--hex-color "SystemWindow") "#ff0000")))
  (let ((lispm-desktop-picture nil))
    (let ((lispm-desktop-color "dark slate blue"))
      (should (equal (lispm-desktop--color) "#483d8b"))
      (should (string-match-p "fill=\"#483d8b\"" (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-color "#48483D3D8B8B"))
      (should (equal (lispm-desktop--color) "#483d8b")))
    ;; A colour Emacs does not know leaves the desktop the theme's, not black.
    (let ((lispm-desktop-color "no such colour"))
      (should (equal (lispm-desktop--color)
                     (let ((lispm-desktop-color nil)) (lispm-desktop--color)))))))

(ert-deftest lispm-desktop-test-settings-choose-color-keeps-hex ()
  "The colour chosen is kept written #RRGGBB, as a name typed comes back
from `read-color' 12-digit; nothing, or no colour, is not kept."
  (let (saved)
    (cl-letf (((symbol-function 'lispm-desktop-settings--save)
               (lambda (variable value) (push (cons variable value) saved))))
      (dolist (answer '("#48483D3D8B8B" "" "no such colour"))
        (cl-letf (((symbol-function 'read-color) (lambda (&rest _) answer)))
          (lispm-desktop-settings--choose-color))))
    (should (equal saved '((lispm-desktop-color . "#483d8b"))))))

(ert-deftest lispm-desktop-test-trash-full ()
  "The Trash is full when it holds anything but Finder's .DS_Store: counted
where it can be read, measured by its link count where it cannot."
  (cl-letf (((symbol-function 'directory-files)
             (lambda (&rest _) '(".DS_Store" "old.txt"))))
    (should (lispm-desktop-trash-full-p)))
  (cl-letf (((symbol-function 'directory-files) (lambda (&rest _) '(".DS_Store"))))
    (should-not (lispm-desktop-trash-full-p)))
  (dolist (case '((3 t nil) (4 t t) (2 nil nil) (3 nil t)))
    (pcase-let ((`(,links ,ds-store ,full) case))
      (cl-letf (((symbol-function 'directory-files)
                 (lambda (&rest _) (signal 'file-error '("Operation not permitted"))))
                ((symbol-function 'file-attributes)
                 (lambda (&rest _) (list t links 501 20 nil nil nil 64)))
                ((symbol-function 'file-exists-p) (lambda (_) ds-store)))
        (should (eq (and (lispm-desktop-trash-full-p) t) full))))))

(ert-deftest lispm-desktop-test-closing-the-last-window-shows-the-desktop ()
  "The close box takes a window away while others remain in the main area;
the last one uncovers the desktop."
  (save-window-excursion
    (delete-other-windows)
    (let ((first (generate-new-buffer " lispm-desktop-test-first"))
          (second (generate-new-buffer " lispm-desktop-test-second")))
      (set-window-buffer (selected-window) first)
      (let ((other (split-window-right)))
        (set-window-buffer other second)
        (lispm-desktop-platinum-close-window other)
        (should-not (window-live-p other))
        (should-not (buffer-live-p second))
        (lispm-desktop-platinum-close-window (selected-window))
        (should-not (buffer-live-p first))
        (should (lispm-desktop-p (selected-window)))))))

(ert-deftest lispm-desktop-test-editor-opens-on-the-startup-screen ()
  "In the editor, the last window closed shows Emacs's own startup screen."
  (save-window-excursion
    (delete-other-windows)
    (let ((lispm-desktop-empty-function #'lispm-desktop-editor--show-splash)
          (buffer (generate-new-buffer " lispm-desktop-test-last")))
      (set-window-buffer (selected-window) buffer)
      (lispm-desktop-platinum-close-window (selected-window))
      (should (equal (buffer-name (window-buffer)) lispm-desktop-editor-splash-buffer-name)))))

(ert-deftest lispm-desktop-test-new-workspace-opens-on-the-desktop ()
  "A workspace opened onto *scratch* shows the desktop instead."
  (save-window-excursion
    (delete-other-windows)
    (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
    (lispm-desktop--on-new-workspace)
    (should (lispm-desktop-p))))

(defmacro lispm-desktop-test--with-folder (&rest body)
  "Run BODY with `folder' bound to a fresh folder of a few items."
  (declare (indent 0))
  `(let ((folder (make-temp-file "lispm-desktop-finder" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "zeta" folder))
           (make-directory (expand-file-name "Alpha/.git" folder) t)
           (dolist (name '("notes.org" "init.el" "beta.txt" ".hidden"))
             (write-region "" nil (expand-file-name name folder)))
           ,@body)
       (delete-directory folder t))))

(ert-deftest lispm-desktop-test-finder-reads-folders ()
  "A folder's items come by name, case aside, hidden ones left out, each
with its large icon."
  (lispm-desktop-test--with-folder
    (let ((items (lispm-desktop-finder--read folder)))
      (should (equal (mapcar #'car items) '("Alpha" "beta.txt" "init.el" "notes.org" "zeta")))
      (should (cdr (assoc "zeta" items)))
      (should-not (cdr (assoc "init.el" items)))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "Alpha" items)) "folder-git-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "zeta" items)) "folder-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "init.el" items)) "document-lisp-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "notes.org" items)) "document-org-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "beta.txt" items)) "document-32")))))

(ert-deftest lispm-desktop-test-finder-history ()
  "Opening a folder records the one left: back, forward and up move through
them, up selecting the folder it came from."
  (lispm-desktop-test--with-folder
    (let ((root (file-name-as-directory folder)))
      (with-current-buffer (lispm-desktop-finder--buffer)
        (setq lispm-desktop-finder--directory nil lispm-desktop-finder--back nil lispm-desktop-finder--forward nil))
      (lispm-desktop-finder--go folder)
      (lispm-desktop-finder--go (expand-file-name "zeta" folder) t)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--back (list root)))
        (should (equal (lispm-desktop-finder--title lispm-desktop-finder--directory) "zeta")))
      (lispm-desktop-finder-back)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--directory root))
        (should (equal lispm-desktop-finder--forward (list (file-name-as-directory
                                                            (expand-file-name "zeta" folder))))))
      (lispm-desktop-finder-forward)
      (lispm-desktop-finder-up)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--directory root))
        (should (equal lispm-desktop-finder--selected "zeta"))
        (should (string-match-p "\\`5 items" (lispm-desktop-finder--info)))))
    (kill-buffer lispm-desktop-finder-buffer-name)))

(ert-deftest lispm-desktop-test-finder-moves-to-trash ()
  "Command-Delete sends the selected item to the Trash and selects the next."
  (lispm-desktop-test--with-folder
    (let ((trashed nil))
      (cl-letf (((symbol-function 'move-file-to-trash)
                 (lambda (file) (push file trashed) (delete-file file))))
        (lispm-desktop-finder--go folder)
        (lispm-desktop-finder--select "beta.txt")
        (lispm-desktop-finder-move-to-trash)
        (should (equal trashed (list (expand-file-name "beta.txt" folder))))
        (lispm-desktop-finder--in
          (should-not (assoc "beta.txt" lispm-desktop-finder--items))
          (should (equal lispm-desktop-finder--selected "init.el")))))
    (kill-buffer lispm-desktop-finder-buffer-name)))

(ert-deftest lispm-desktop-test-views-ask-the-editor-first ()
  "A view is asked for through the editor, which comes forward first."
  (let ((asked nil))
    (cl-letf (((symbol-function 'lispm-desktop-editor--prepare-view) (lambda () (setq asked t)))
              (lispm-desktop-sidebar-views '((test :visible ignore :show ignore :hide ignore)))
              (lispm-desktop-sidebar-prepare-function #'lispm-desktop-editor--prepare-view))
      (lispm-desktop-sidebar-select 'test)
      (should asked))))

(ert-deftest lispm-desktop-test-menu-bar-follows-the-application ()
  "The menu bar holds the menus of the application in front."
  (let ((lispm-desktop-platinum-menus-function #'lispm-desktop-apps-menus))
    (let ((lispm-desktop-apps--frontmost 'editor))
      (should (equal (lispm-desktop-test--menu-bar-titles)
                     '(lispm-desktop-lambda lispm-desktop-editor-file lispm-desktop-editor-edit lispm-desktop-editor-view
                                            lispm-desktop-editor-project lispm-desktop-editor-window))))
    (let ((lispm-desktop-apps--frontmost 'desktop))
      (should (equal (lispm-desktop-test--menu-bar-titles)
                     '(lispm-desktop-lambda lispm-desktop-menu-file lispm-desktop-menu-view lispm-desktop-menu-go
                                            lispm-desktop-menu-special))))
    (let ((lispm-desktop-apps--frontmost 'browser))
      (should (equal (lispm-desktop-test--menu-bar-titles)
                     '(lispm-desktop-lambda lispm-desktop-browser-file lispm-desktop-browser-view lispm-desktop-browser-go
                                            lispm-desktop-browser-bookmarks lispm-desktop-browser-window))))))

(ert-deftest lispm-desktop-test-application-follows-its-selected-window ()
  "Selecting a floating window puts its application in front; the screen,
selected by a click on the menu bar, leaves the one in front."
  (let ((lispm-desktop-apps--frontmost 'editor)
        (lispm-desktop-apps-frontmost-functions nil)
        (app 'browser))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (and (eq parameter 'lispm-desktop-app) app))))
      (lispm-desktop-apps--follow-window)
      (should (eq (lispm-desktop-apps-frontmost) 'browser))
      (setq app nil)
      (lispm-desktop-apps--follow-window)
      (should (eq (lispm-desktop-apps-frontmost) 'browser)))))

(ert-deftest lispm-desktop-test-browser-boxes ()
  "The browser's zoom box moves it between full screen and floating; only
the floating browser rolls up."
  (cl-letf (((symbol-function 'lispm-desktop-browser-floating-p) #'ignore))
    (should (equal (lispm-desktop-browser--boxes)
                   '(:close lispm-desktop-browser--close-window :zoom lispm-desktop-browser--zoom-box))))
  (cl-letf (((symbol-function 'lispm-desktop-browser-floating-p) #'always))
    (should (eq (plist-get (lispm-desktop-browser--boxes) :collapse) 'lispm-desktop-browser--collapse-box))))

(ert-deftest lispm-desktop-test-application-menu-lists-applications ()
  "The Application menu lists the applications, the one in front checked;
the browser only where it can run."
  (let ((lispm-desktop-apps--frontmost 'editor))
    (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
      (let ((items (lispm-desktop-apps-application-items)))
        (should (equal (mapcar (lambda (item) (plist-get item :label)) items)
                       '("Desktop" "Emacs")))
        (should (plist-get (nth 1 items) :checked))
        (should (equal (cdr (lispm-desktop-apps-application)) "Emacs"))))
    (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always))
      (should (= 3 (length (lispm-desktop-apps-application-items)))))))

(ert-deftest lispm-desktop-test-documents-go-to-the-editor ()
  "Outside the editor, documents are sent to it; the browser's pages, the
desktop and internal buffers are not."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-document.txt")
    (setq buffer-file-name "/tmp/lispm-desktop-test-document.txt")
    (let ((document (current-buffer)))
      (cl-letf (((symbol-function 'lispm-desktop-editor--in-workspace-p) #'ignore))
        (should (lispm-desktop-editor--astray-p document nil))
        (should-not (lispm-desktop-editor--astray-p (lispm-desktop-buffer) nil))
        (let ((lispm-desktop-editor--displaying t))
          (should-not (lispm-desktop-editor--astray-p document nil))))
      (cl-letf (((symbol-function 'lispm-desktop-editor--in-workspace-p) #'always))
        (should-not (lispm-desktop-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lispm-desktop-test-documents-leave-the-floating-browser ()
  "A document opened from the floating browser, an application's window
of its own, goes to the editor; popups are left be."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-document.txt")
    (setq buffer-file-name "/tmp/lispm-desktop-test-document.txt")
    (let ((document (current-buffer))
          (parameters '((lispm-desktop-window . t))))
      (cl-letf (((symbol-function 'frame-parent) #'always)
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter) (alist-get parameter parameters))))
        (should (lispm-desktop-editor--astray-p document nil))
        (setq parameters nil)
        (should-not (lispm-desktop-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lispm-desktop-test-new-workspace-leaves-sessions-behind ()
  "A workspace made while a session's window is selected starts on its own
first buffer alone: the session is not carried along, as it was when
the first buffer went through the routing that keeps sessions uncovered."
  (let ((session (get-buffer-create "lispm-desktop-test-session"))
        (display-buffer-alist '((lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session)
                                (lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file)))
        (switch-to-buffer-obey-display-actions t)
        (tab-bar-mode-was tab-bar-mode))
    (cl-letf (((symbol-function 'lispm-desktop-terminal-buffer-p)
               (lambda (buffer) (eq (get-buffer buffer) session))))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) session)
            (let ((tab-bar-new-tab-choice #'lispm-desktop--new-tab-empty))
              (tab-bar-new-tab))
            (should (equal (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                   (window-list nil 'no-mini))
                           (list lispm-desktop-empty-buffer-name))))
        (tab-bar-close-tab)
        (unless tab-bar-mode-was (tab-bar-mode -1))
        (kill-buffer session)))))

(ert-deftest lispm-desktop-test-new-workspace-starts-on-startup-screen ()
  "A new workspace shows Emacs's startup screen itself, handing tab-bar no
buffer: one shown through `switch-to-buffer' would obey the editor's
routing while the old workspace is still in front."
  (save-window-excursion
    (should-not (lispm-desktop-editor--new-workspace-buffer))
    (should (equal (buffer-name (window-buffer)) lispm-desktop-editor-splash-buffer-name))))

(ert-deftest lispm-desktop-test-editor-workspaces-leave-out-desktop-and-browser ()
  "The desktop's and the browser's workspaces are not the editor's."
  (should-not (lispm-desktop-editor--workspace-tab-p '(tab (name . "Desktop"))))
  (should-not (lispm-desktop-editor--workspace-tab-p '(tab (name . "Browser"))))
  (should (lispm-desktop-editor--workspace-tab-p '(tab (name . "lispm-desktop")))))

(ert-deftest lispm-desktop-test-buffer-manager-counts-process-trees ()
  "A buffer's process is counted with every process under it."
  (let ((processes (make-hash-table)))
    (puthash 10 '(1 1.5 1000) processes)
    (puthash 11 '(10 2.0 500) processes)
    (puthash 12 '(11 0.5 250) processes)
    (puthash 20 '(1 9.0 9000) processes)
    (let ((children (lispm-desktop-buffer-manager--children processes)))
      (should (equal (lispm-desktop-buffer-manager--tree-cost 10 processes children) '(4.0 . 1750)))
      (should (equal (lispm-desktop-buffer-manager--tree-cost 12 processes children) '(0.5 . 250))))))

(ert-deftest lispm-desktop-test-buffer-manager-rows ()
  "Every buffer but internal ones has a row, its text measured."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-row")
    (erase-buffer)
    (insert "hello")
    (cl-letf (((symbol-function 'lispm-desktop-buffer-manager--processes) (lambda () (make-hash-table))))
      (let* ((entries (lispm-desktop-buffer-manager--entries))
             (row (assq (current-buffer) entries)))
        (should row)
        (should (equal (get-text-property 0 'lispm-desktop-value (aref (cadr row) 2)) 5))
        (should-not (seq-find (lambda (entry) (string-prefix-p " " (buffer-name (car entry))))
                              entries))))
    (kill-buffer)))

(ert-deftest lispm-desktop-test-palette-takes-named-colours ()
  "Colours a theme names replace the derived ones; the rest stay derived."
  (let* ((derived (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (palette (lispm-desktop-platinum--merge-palette derived '(:chrome "#DDDDDD" :accent "#6363CE"))))
    (should (equal (plist-get palette :chrome) "#DDDDDD"))
    (should (equal (plist-get palette :accent) "#6363CE"))
    (should (equal (plist-get palette :shade) (plist-get derived :shade)))
    ;; The derived palette itself is left untouched.
    (should (equal (plist-get derived :accent) "#bd93f9"))))

(ert-deftest lispm-desktop-test-platinum-theme ()
  "The platinum theme loads, and names Platinum's greys for the chrome."
  (let ((custom-theme-load-path (cons (expand-file-name "themes/" lispm-desktop-directory)
                                      custom-theme-load-path)))
    (unwind-protect
        (progn
          (load-theme 'platinum t)
          (should (memq 'platinum custom-enabled-themes))
          (should (equal (plist-get lispm-desktop-platinum-colors :chrome) "#DDDDDD"))
          (should (equal (plist-get lispm-desktop-platinum-colors :accent) "#6363CE")))
      (disable-theme 'platinum))
    (should-not (plist-get lispm-desktop-platinum-colors :chrome))))

(ert-deftest lispm-desktop-test-window-bottom ()
  "A window's bottom edge counts a list's items, or shows the buffer."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((lispm-desktop-platinum--palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2"
                                                                                 "#6272a4" "#bd93f9")))
    (with-temp-buffer
      (rename-buffer "lispm-desktop-test 100% done" t)
      (let ((left (lispm-desktop-platinum-mode-line-left)))
        ;; A % in the buffer name is escaped, as the bottom is a mode line.
        (should (string-search "100%% done" left)))
      (setq-local lispm-desktop-platinum-info "3 sessions")
      (should (string-search "3 sessions" (lispm-desktop-platinum-mode-line-left)))
      (should (string-empty-p (string-trim (substring-no-properties
                                            (lispm-desktop-platinum-mode-line-right))))))))

(ert-deftest lispm-desktop-test-sidebar-count ()
  "List windows count their items in the singular or the plural."
  (should (equal (lispm-desktop-sidebar--count 1 "session") "1 session"))
  (should (equal (lispm-desktop-sidebar--count 3 "session" "1 running") "3 sessions, 1 running")))

(ert-deftest lispm-desktop-test-pressed-box-is-inverted ()
  "A box held down is lit from below and filled darker."
  (let* ((palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (up (format "%S" (lispm-desktop-platinum-draw-box palette 'close 22)))
         (down (format "%S" (lispm-desktop-platinum-draw-box palette 'close 22 nil nil t))))
    (should-not (equal up down))
    ;; The fill of a pressed box is not the chrome.
    (should (string-search (lispm-desktop-tab-bar--blend (plist-get palette :chrome)
                                                         (plist-get palette :shade) 0.5)
                           down))))

(ert-deftest lispm-desktop-test-alert-button-focus-ring ()
  "The alert button the keyboard is on is outlined in the accent colour."
  (let* ((palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (plain (format "%S" (lispm-desktop-popup-draw-button palette "Cancel" 80)))
         (focused (format "%S" (lispm-desktop-popup-draw-button palette "Cancel" 80 nil nil t))))
    (should-not (string-search (plist-get palette :accent) plain))
    (should (string-search (plist-get palette :accent) focused))))

(ert-deftest lispm-desktop-test-toolbar-buttons-with-image-icons-are-apart ()
  "Two toolbar buttons whose icons are images read alike, \" +\", but are
different buttons: pressing one draws only it pushed in."
  (let* ((icon (lambda (file) (propertize " " 'display (list 'image :type 'png :file file))))
         (hermes (lispm-desktop-sidebar--toolbar-button (concat (funcall icon "hermes.png") "+")
                                                        #'lispm-desktop-agent-hermes-new "Hermes" 'header-line))
         (lispm (lispm-desktop-sidebar--toolbar-button (concat (funcall icon "lispm.png") "+")
                                                       #'lispm-desktop-agent-lispm-new "LispM" 'header-line)))
    (should (equal hermes lispm))
    (should-not (equal (get-text-property 0 'lispm-desktop-platinum-target hermes)
                       (get-text-property 0 'lispm-desktop-platinum-target lispm)))))

(ert-deftest lispm-desktop-test-press-runs-on-release-over-button ()
  "A pressed button runs only when let go over it, and is drawn pushed in
while held."
  (save-window-excursion
    (let* ((window (selected-window))
           (press (list window 'tab-line '(0 . 0) 0))
           (ran 0)
           (held nil))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (setq held (window-parameter window 'lispm-desktop-platinum-pressed))))
                ((symbol-function 'lispm-desktop-platinum--press-target) (lambda (_) 'close)))
        ;; Let go over the button: it runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lispm-desktop-platinum-press press (lambda (_) t) (lambda () (cl-incf ran))))
        (should (= ran 1))
        (should (eq held 'close))
        (should-not (window-parameter window 'lispm-desktop-platinum-pressed))
        ;; Moved off before letting go: nothing runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lispm-desktop-platinum-press press (lambda (_) nil) (lambda () (cl-incf ran))))
        (should (= ran 1))))))

(ert-deftest lispm-desktop-test-drag-swaps-windows ()
  "A title bar dropped on another window swaps the two, columns permitting."
  (lispm-desktop-test--with-layout
   (let* ((session (lispm-desktop-test--session "lispm-desktop-test session 8"))
          (a (lispm-desktop-test--file "lispm-desktop-test-g.txt"))
          (b (lispm-desktop-test--file "lispm-desktop-test-h.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (let* ((file-window (selected-window))
            (lower (split-window-below)))
       (set-window-buffer lower b)
       (cl-flet ((drag (from to)
                   (lispm-desktop-platinum-drag-window
                    (list 'drag-mouse-1 (list from 'tab-line '(0 . 0) 0)
                          (list to 'tab-line '(0 . 0) 0)))))
         ;; Two files trade places.
         (drag file-window lower)
         (should (eq (window-buffer file-window) b))
         (should (eq (window-buffer lower) a))
         ;; A file dropped on the session does not move.
         (drag lower (get-buffer-window session))
         (should (eq (window-buffer (get-buffer-window session)) session)))))))

(ert-deftest lispm-desktop-test-file-icons ()
  "Files are shown with the document of their kind."
  (should (equal (lispm-desktop-platinum-file-icon-name "a.el") "document-lisp"))
  (should (equal (lispm-desktop-platinum-file-icon-name "a.rs") "document-code"))
  (should (equal (lispm-desktop-platinum-file-icon-name "README.md") "document-markdown"))
  (should (equal (lispm-desktop-platinum-file-icon-name "notes.org") "document-org"))
  (should (equal (lispm-desktop-platinum-file-icon-name "a.png") "document-image"))
  (should (equal (lispm-desktop-platinum-file-icon-name "LICENSE") "document"))
  (dolist (name '("document-lisp" "document-code" "document-markdown" "document-org"
                  "folder-git" "lispm" "robot"))
    (should (file-exists-p (expand-file-name (format "icons/os9/%s.svg" name) lispm-desktop-directory)))))

(ert-deftest lispm-desktop-test-scroll-thumb ()
  "The thumb is as long as the share shown, and as far down as the start."
  (let ((w lispm-desktop-platinum-scroll-width))
    ;; Everything shows: no thumb.
    (should-not (lispm-desktop-platinum-scroll-thumb 400 1 1000 1 1000))
    ;; The top of a buffer ten times the window: a tenth of the track, at its top.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 (* 2 w)) 1 101 1 1001)))
      (should (= top w))
      (should (= length 30)))
    ;; The end: the thumb's bottom meets the down arrow.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 (* 2 w)) 901 1001 1 1001)))
      (should (= (+ top length) (+ 300 w))))
    ;; A short thumb is never shorter than a box.
    (should (= (cdr (lispm-desktop-platinum-scroll-thumb 400 1 2 1 100000)) 16))))

(ert-deftest lispm-desktop-test-scroll-bar-draws ()
  "Scroll bars draw active, inactive, and with nothing to scroll."
  (let ((palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 nil t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) nil)) 'svg))))

(ert-deftest lispm-desktop-test-scroll-bar-windows ()
  "Files get scroll bars; sessions and side windows other than lists do not."
  (save-window-excursion
    (delete-other-windows)
    (let ((file (lispm-desktop-test--file "lispm-desktop-test-i.txt"))
          (session (lispm-desktop-test--session "lispm-desktop-test session 9")))
      (set-window-buffer (selected-window) file)
      (should (lispm-desktop-platinum--scroll-window-p (selected-window)))
      (set-window-buffer (selected-window) session)
      (should-not (lispm-desktop-platinum--scroll-window-p (selected-window)))
      (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test popup")
                                                 '((side . bottom)))))
        (should-not (lispm-desktop-platinum--scroll-window-p side))))))

(ert-deftest lispm-desktop-test-menu-text ()
  "A menu lists its items one to a line, separators as rules, checks marked."
  (let* ((lispm-desktop-platinum--palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000"
                                                                                  "#777777" "#6363CE"))
         (items '((:label "One" :action ignore :checked t)
                  separator
                  (:label "Two" :action ignore :enabled nil)
                  (:label "Three" :action ignore)))
         (text (lispm-desktop-popup-menu-text items 200 3)))
    (should (= (length (split-string text "\n")) 4))
    (should (string-search "✓" text))
    (should (eq (get-text-property (string-search "Three" text) 'face text)
                'lispm-desktop-popup-item-highlight))
    (should (eq (get-text-property (string-search "Two" text) 'face text)
                'lispm-desktop-popup-item-disabled))
    ;; Moving down from One skips the separator and the disabled Two.
    (should (= (lispm-desktop-popup--step items 0 1) 3))
    (should (= (lispm-desktop-popup--step items 3 1) 0))))

(ert-deftest lispm-desktop-test-menu-keys ()
  "Key equivalents are keyboard sequences only."
  (should (lispm-desktop-popup--keyboard-p (kbd "C-c a l")))
  (should-not (lispm-desktop-popup--keyboard-p [C-M-drag-mouse-1]))
  (should-not (lispm-desktop-popup--keyboard-p [menu-bar file open])))

(ert-deftest lispm-desktop-test-keymap-items ()
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
    (let ((items (lispm-desktop-popup-keymap-items map)))
      (should (equal (mapcar (lambda (item) (if (eq item 'separator) item (plist-get item :label)))
                             items)
                     '("First" "Toggle" separator "More" "Last")))
      (should (equal (plist-get (nth 0 items) :key) "C-x f"))
      (should (plist-get (nth 1 items) :checked))
      (should-not (lispm-desktop-popup--enabled-p (nth 4 items)))
      (should (equal (mapcar (lambda (item) (plist-get item :label))
                             (lispm-desktop-popup--submenu (nth 3 items)))
                     '("Inner"))))))

(defun lispm-desktop-test--choose (label)
  "Return a stand-in for `lispm-desktop-popup-choose' that picks the item LABEL.
It follows submenus, as a click through a hierarchical menu does."
  (lambda (items &rest _)
    (named-let pick ((items items))
      (seq-some (lambda (item)
                  (when (consp item)
                    (cond ((equal (plist-get item :label) label) item)
                          ((plist-get item :submenu) (pick (lispm-desktop-popup--submenu item))))))
                items))))

(ert-deftest lispm-desktop-test-x-popup-menu-keymap ()
  "A keymap popped up by `x-popup-menu' answers with the chosen key path,
as the project tree's right-click menu looks its command up by it."
  (let ((map (make-sparse-keymap "Tree"))
        (sub (make-sparse-keymap "New")))
    (define-key sub [file] '(menu-item "File" find-file))
    (define-key map [new] `(menu-item "New" ,sub))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) (lispm-desktop-test--choose "File")))
      (let ((choice (lispm-desktop-popup--x-popup-menu #'ignore t map)))
        (should (equal choice '(new file)))
        (should (eq (lookup-key map (apply #'vector choice)) 'find-file))))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) #'ignore))
      (should-not (lispm-desktop-popup--x-popup-menu #'ignore t map)))))

(ert-deftest lispm-desktop-test-x-popup-menu-panes ()
  "A menu of panes answers with the chosen item's value; lone strings
cannot be chosen, and panes are parted by separators."
  (let ((menu '("Spelling" ("" ("teh" . teh) "Suggestions") ("Actions" ("Save" . save) "--"))))
    (should (equal (mapcar (lambda (item) (if (consp item) (plist-get item :label) item))
                           (lispm-desktop-popup--pane-items menu))
                   '("teh" "Suggestions" separator "Actions" "Save" separator)))
    (should-not (lispm-desktop-popup--enabled-p (nth 1 (lispm-desktop-popup--pane-items menu))))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) (lispm-desktop-test--choose "Save")))
      (should (eq (lispm-desktop-popup--x-popup-menu #'ignore t menu) 'save)))))

(ert-deftest lispm-desktop-test-x-popup-menu-passes-through ()
  "Without a position, or a display to draw on, the original menu runs."
  (let ((map (make-sparse-keymap "Tree")))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (should (eq (lispm-desktop-popup--x-popup-menu (lambda (&rest _) 'native) nil map) 'native))
    (should (eq (lispm-desktop-popup--x-popup-menu (lambda (&rest _) 'native) t map) 'native))))

(ert-deftest lispm-desktop-test-right-click-defers-to-local-command ()
  "A buffer with a right-click command of its own keeps it."
  (with-temp-buffer
    (should-not (lispm-desktop-popup--local-right-click-p nil))
    (let ((map (make-sparse-keymap)))
      (keymap-set map "<mouse-3>" #'rename-file)
      (use-local-map map)
      (should (lispm-desktop-popup--local-right-click-p nil)))))

(ert-deftest lispm-desktop-test-popups-fall-back ()
  "Without a graphical display, menus and alerts ask in the minibuffer."
  (should-not (lispm-desktop-popup-available-p))
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Two")))
    (should (equal (plist-get (lispm-desktop-popup-menu '((:label "One" :action ignore)
                                                          (:label "Two" :action ignore)))
                              :label)
                   "Two")))
  (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "Save"))))
    (should (eq (lispm-desktop-popup-alert 'caution "Save?" '(("Cancel" cancel cancel)
                                                             ("Save" save default)))
                'save))))

(ert-deftest lispm-desktop-test-alert-text ()
  "An alert shows its message, its detail and a button per choice."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let* ((lispm-desktop-platinum--palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000"
                                                                                  "#777777" "#6363CE"))
         (text (lispm-desktop-popup-alert-text 'caution "Save?" "Changes will be lost."
                                               '(("Cancel" cancel cancel) ("Save" save default))
                                               nil))
         (buttons nil))
    (should (string-search "Save?" text))
    (should (string-search "Changes will be lost." text))
    (dotimes (i (length text))
      (when-let* ((value (get-text-property i 'lispm-desktop-popup-button text)))
        (cl-pushnew value buttons)))
    (should (equal (sort buttons #'string<) '(cancel save)))))

(ert-deftest lispm-desktop-test-title-bar-clicks ()
  "A title bar brings its window forward, rolls up on a double click, and
pops up its buffers on a Meta click."
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> <mouse-1>"))
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> <double-mouse-1>"))
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> M-<mouse-1>")))

(ert-deftest lispm-desktop-test-shade-or-zoom ()
  "A window alone in its column zooms; one sharing it rolls up."
  (save-window-excursion
    (delete-other-windows)
    (let ((zoomed nil) (shaded nil))
      (cl-letf (((symbol-function 'lispm-desktop-platinum-zoom-window) (lambda (_) (setq zoomed t)))
                ((symbol-function 'lispm-desktop-platinum-collapse-window) (lambda (_) (setq shaded t))))
        (lispm-desktop-platinum-shade-or-zoom (selected-window))
        (should (and zoomed (not shaded)))
        (setq zoomed nil)
        (lispm-desktop-platinum-shade-or-zoom (split-window-below))
        (should (and shaded (not zoomed)))))))

(ert-deftest lispm-desktop-test-status-drawings ()
  "The battery and the load meter draw at any level."
  (let ((palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (dolist (percent '(0 5 50 100))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-battery palette percent t)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-battery palette percent nil)) 'svg)))
    (dolist (fraction '(0.0 0.5 1.5))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-meter palette fraction)) 'svg)))))

(require 'time)
(defvar load)
(defvar mail)

(ert-deftest lispm-desktop-test-status-notes ()
  "What Emacs computes for the time and the battery is noted for the menu bar."
  (let ((load " 1.25") (mail t) (lispm-desktop-platinum--load nil) (lispm-desktop-platinum--mail nil))
    (lispm-desktop-platinum--note-time)
    (should (equal lispm-desktop-platinum--load "1.25"))
    (should (eq lispm-desktop-platinum--mail t)))
  (let ((lispm-desktop-platinum--battery nil))
    (lispm-desktop-platinum--note-battery '((?p . "87")))
    (should (equal (alist-get ?p lispm-desktop-platinum--battery) "87"))))

(ert-deftest lispm-desktop-test-menu-status ()
  "The menu bar keeps other status text, and adds the time and mail when on."
  (let ((labels nil)
        (lispm-desktop-platinum--palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000"
                                                                                 "#777777" "#6363CE"))
        (global-mode-string '("" "λ 2" display-time-string))
        (display-time-string "9:41AM")
        (display-time-mode t)
        (display-time-format "%-I:%M %p")
        (lispm-desktop-platinum--load "")
        (lispm-desktop-platinum--mail nil))
    ;; A batch Emacs formats every mode line construct as "".
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (construct &rest _)
                 (mapconcat (lambda (part) (if (stringp part) part "")) (ensure-list construct) ""))))
      (setq labels (mapcar (lambda (item) (substring-no-properties (nth 2 item)))
                           (lispm-desktop-platinum-menu-status))))
    (progn
      (should (string-search "λ 2" (nth 0 labels)))
      (should-not (seq-some (lambda (label) (string-search "9:41AM" label)) labels))
      (should (= (length labels) 3))))
  (let ((global-mode-string nil) (display-time-mode nil))
    (should-not (lispm-desktop-platinum-menu-status))))

(ert-deftest lispm-desktop-test-age-short-date ()
  "Old sessions are dated as Finder dated files; recent ones by their age."
  (let ((now (float-time (encode-time '(0 0 12 13 10 2026 nil -1 nil)))))
    (should (equal (lispm-desktop-agent--age (- now 120) now) "2m"))
    (should (equal (lispm-desktop-agent--age (float-time (encode-time '(0 0 12 13 5 2026 nil -1 nil))) now)
                   "5/13/26"))))

(ert-deftest lispm-desktop-test-row-shades-sorted-column ()
  "A row's right-hand column carries the sorted column's face to the edge."
  (cl-letf (((symbol-function 'lispm-desktop-sidebar--column-pixels) (lambda () 100)))
    (let ((row (lispm-desktop-sidebar--row "" "name" "3h" 300)))
      (should (string-search "3h" row))
      (should (equal (get-text-property (1- (length row)) 'face row) 'lispm-desktop-platinum-sorted)))))

;;;; Main area columns

(defmacro lispm-desktop-test--with-layout (&rest body)
  "Run BODY in a frame of one main window, with the columns' rules on."
  `(save-window-excursion
     (let ((display-buffer-alist
            '((lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file)
              (lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session)))
           (switch-to-buffer-obey-display-actions t)
           (window-min-width 4))
       (delete-other-windows)
       ,@body)))

(defun lispm-desktop-test--session (name)
  "Return a fresh buffer NAME laid out as a session."
  (with-current-buffer (get-buffer-create name)
    (setq lispm-desktop-conversation-buffer t)
    (current-buffer)))

(defun lispm-desktop-test--file (name)
  "Return a buffer visiting a temporary file NAME."
  (find-file-noselect (expand-file-name name temporary-file-directory)))

(defun lispm-desktop-test--left-p (a b)
  "Return non-nil when the window of buffer A is left of B's."
  (< (car (window-edges (get-buffer-window a))) (car (window-edges (get-buffer-window b)))))

(ert-deftest lispm-desktop-test-file-from-session-goes-right ()
  "A file opened from a session window opens to its right; the session stays."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session"))
         (file (lispm-desktop-test--file "lispm-desktop-test-a.txt")))
     (switch-to-buffer session)
     (switch-to-buffer file)
     (should (get-buffer-window session))
     (should (lispm-desktop-test--left-p session file))
     (should (eq (window-buffer (selected-window)) file)))))

(ert-deftest lispm-desktop-test-session-from-file-goes-left ()
  "A session shown from a file window goes to the left of the file."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 2"))
         (file (lispm-desktop-test--file "lispm-desktop-test-b.txt")))
     (switch-to-buffer file)
     (switch-to-buffer session)
     (should (get-buffer-window file))
     (should (lispm-desktop-test--left-p session file)))))

(ert-deftest lispm-desktop-test-session-reuses-session-column ()
  "With both columns open, a session from the file side replaces the session."
  (lispm-desktop-test--with-layout
   (let ((one (lispm-desktop-test--session "lispm-desktop-test session 3"))
         (two (lispm-desktop-test--session "lispm-desktop-test session 4"))
         (file (lispm-desktop-test--file "lispm-desktop-test-c.txt")))
     (switch-to-buffer one)
     (switch-to-buffer file)
     (switch-to-buffer two)
     (should (= (length (window-list)) 2))
     (should-not (get-buffer-window one))
     (should (lispm-desktop-test--left-p two file)))))

(ert-deftest lispm-desktop-test-other-buffer-from-session-goes-right ()
  "Any buffer switched to from a session window goes right, not over it."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 6"))
         (other (get-buffer-create "lispm-desktop-test other")))
     (switch-to-buffer session)
     (switch-to-buffer other)
     (should (get-buffer-window session))
     (should (lispm-desktop-test--left-p session other)))))

(ert-deftest lispm-desktop-test-popups-are-left-alone ()
  "A buffer displayed as a popup is not taken over by the columns."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 7")))
     (switch-to-buffer session)
     (should-not (lispm-desktop-main-area--astray-p (get-buffer-create "lispm-desktop-test popup")
                                                    '(display-buffer-pop-up-window))))))

(define-derived-mode lispm-desktop-test-chat-mode special-mode "Chat"
  "A stand-in for an agent's chat mode.")

(ert-deftest lispm-desktop-test-chat-mode-is-a-session ()
  "A buffer in a registered chat mode keeps to the session column at once."
  (lispm-desktop-test--with-layout
   (let ((lispm-desktop-conversation-modes '(lispm-desktop-test-chat-mode))
         (file (lispm-desktop-test--file "lispm-desktop-test-f.txt"))
         (chat (with-current-buffer (get-buffer-create "lispm-desktop-test chat")
                 (lispm-desktop-test-chat-mode)
                 (current-buffer))))
     (switch-to-buffer file)
     (pop-to-buffer chat)
     (should (lispm-desktop-test--left-p chat file)))))

(ert-deftest lispm-desktop-test-second-file-stays-right ()
  "A second file opened from the session replaces the file, not the session."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 5"))
         (a (lispm-desktop-test--file "lispm-desktop-test-d.txt"))
         (b (lispm-desktop-test--file "lispm-desktop-test-e.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (select-window (get-buffer-window session))
     (switch-to-buffer b)
     (should (= (length (window-list)) 2))
     (should (lispm-desktop-test--left-p session b)))))

(ert-deftest lispm-desktop-test-menu-bar-only-outside-the-frame ()
  "A graphical frame keeps its menu bar only where NS draws it, outside."
  (let (set)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (frame) (not (eq frame 'tty))))
              ((symbol-function 'framep)
               (lambda (frame) (if (eq frame 'tty) t frame)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (push (list frame parameter value) set))))
      (lispm-desktop-frame-menu-bar 'ns)
      (lispm-desktop-frame-menu-bar 'tty)
      (lispm-desktop-frame-menu-bar 'x))
    (should (equal set '((x menu-bar-lines 0))))))

(ert-deftest lispm-desktop-test-default-font-first-installed ()
  "The default face takes the first preferred family that is installed,
unless the user customized it."
  (let (set)
    (cl-letf (((symbol-function 'font-family-list)
               (lambda (&optional _frame) '("Andale Mono" "Menlo")))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest attributes)
                 (push (cons face attributes) set))))
      (lispm-desktop-pin-default-font)
      (should (equal set '((default :family "Menlo" :height 120))))
      (setq set nil)
      (cl-letf (((get 'default 'saved-face) '((t :family "Iosevka"))))
        (lispm-desktop-pin-default-font))
      (should-not set))))

(defun lispm-desktop-test--x-properties (checks)
  "Stand in for `x-window-property', reading CHECKS.
CHECKS maps a window (0 for the root) to the window its
_NET_SUPPORTING_WM_CHECK names, or to `gone' for a destroyed window."
  (lambda (_property _frame _type source &rest _)
    (pcase (alist-get source checks)
      ('nil nil)
      ('gone (error "BadWindow"))
      (check check))))

(ert-deftest lispm-desktop-test-exwm-only-without-a-window-manager ()
  "Emacs manages an X display only when no window manager runs on it."
  (let ((lispm-desktop-window-manager 'auto))
    (cl-letf (((symbol-function 'framep) (lambda (frame) frame)))
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties nil)))
        (should-not (lispm-desktop-exwm-wanted-p 'ns))
        (should (lispm-desktop-exwm-wanted-p 'x))
        (let ((lispm-desktop-window-manager nil))
          (should-not (lispm-desktop-exwm-wanted-p 'x))))
      ;; A running window manager: the root names a window naming itself.
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties '((0 . 7) (7 . 7)))))
        (should-not (lispm-desktop-exwm-wanted-p 'x)))
      ;; A window manager that quit left its check window's name behind.
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties '((0 . 7) (7 . gone)))))
        (should (lispm-desktop-exwm-wanted-p 'x))))))

;;;; The browser's history and bookmarks

(defmacro lispm-desktop-test--with-places (&rest body)
  "Run BODY with an empty history and bookmark list, in files of their own."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "lispm-desktop-places" t))
          (lispm-desktop-browser-history-file (expand-file-name "history.eld" directory))
          (lispm-desktop-browser--history nil)
          (lispm-desktop-browser--history-loaded nil)
          (lispm-desktop-browser--history-timer nil)
          (bookmark-default-file (expand-file-name "bookmarks" directory))
          (bookmark-alist nil)
          (bookmark-bookmarks-timestamp nil)
          (bookmark-save-flag nil))
     (unwind-protect
         (cl-letf (((symbol-function 'run-with-idle-timer) #'ignore))
           ,@body)
       (delete-directory directory t))))

(ert-deftest lispm-desktop-test-history-keeps-each-page-once ()
  "A page visited again moves to the top with its visits counted; only web
and file pages are kept, and only so many."
  (lispm-desktop-test--with-places
    (let ((lispm-desktop-browser-history-size 2))
      (lispm-desktop-browser-history-add "https://a.example/" "A")
      (lispm-desktop-browser-history-add "about:blank" "")
      (lispm-desktop-browser-history-add "https://b.example/" "B")
      (lispm-desktop-browser-history-add "https://a.example/" "")
      (should (equal (mapcar #'car lispm-desktop-browser--history)
                     '("https://a.example/" "https://b.example/")))
      (should (equal (nth 1 (car lispm-desktop-browser--history)) "A"))
      (should (= (nth 3 (car lispm-desktop-browser--history)) 2))
      (lispm-desktop-browser-history-add "https://c.example/" "C")
      (should (equal (mapcar #'car lispm-desktop-browser--history)
                     '("https://c.example/" "https://a.example/"))))))

(ert-deftest lispm-desktop-test-history-is-saved-and-read ()
  "The history is written to its file and read back from it."
  (lispm-desktop-test--with-places
    (lispm-desktop-browser-history-add "https://a.example/" "A")
    (lispm-desktop-browser--save-history)
    (setq lispm-desktop-browser--history nil
          lispm-desktop-browser--history-loaded nil)
    (should (equal (car (car (lispm-desktop-browser--load-history))) "https://a.example/"))))

(ert-deftest lispm-desktop-test-history-days ()
  "History is listed under the day of each visit."
  (let ((now (float-time)))
    (should (equal (lispm-desktop-browser--history-day now) "Today"))
    (should (equal (lispm-desktop-browser--history-day (- now 86400)) "Yesterday"))
    (should (equal (lispm-desktop-browser--history-day (- now (* 10 86400))) "Last Week"))
    (should (equal (lispm-desktop-browser--history-day (- now (* 40 86400))) "Earlier"))))

(ert-deftest lispm-desktop-test-bookmarks-are-emacs-bookmarks ()
  "A page bookmarked is an Emacs bookmark xwidget can open; it is kept
once, and the browser lists only web pages' bookmarks, oldest first."
  (lispm-desktop-test--with-places
    (bookmark-store "notes" '((filename . "/tmp/notes.org")) nil)
    (cl-letf (((symbol-function 'lispm-desktop-browser--shown-buffer) (lambda () (current-buffer)))
              ((symbol-function 'lispm-desktop-browser--title) (lambda (_) "Example"))
              ((symbol-function 'lispm-desktop-browser--uri) (lambda (_) "https://example.com/")))
      (lispm-desktop-browser-add-bookmark)
      (lispm-desktop-browser-add-bookmark))
    (should (equal (lispm-desktop-browser-bookmarks) '(("Example" . "https://example.com/"))))
    (should (eq (bookmark-prop-get "Example" 'handler) 'xwidget-webkit-bookmark-jump-handler))
    (should (equal (lispm-desktop-browser--bookmark-name "Example") "Example <2>"))
    (let ((labels (mapcar (lambda (item) (and (consp item) (plist-get item :label)))
                          (lispm-desktop-browser--bookmark-items))))
      (should (equal labels '("Add Bookmark" "Edit Bookmarks…" nil "Example"))))
    (lispm-desktop-browser-delete-bookmark "Example")
    (should-not (lispm-desktop-browser-bookmarks))))

(ert-deftest lispm-desktop-test-browser-keys-show-in-menus ()
  "The browser's Command keys show beside their menu items."
  (should (equal (lispm-desktop-browser--key 'lispm-desktop-browser-add-bookmark) "M-d"))
  (let ((lispm-desktop-browser--navigation '(:page t :back t :forward t)))
    (should (equal (plist-get (car (lispm-desktop-browser--go-items)) :key) "M-["))))

(ert-deftest lispm-desktop-test-browser-address-typing-replaces ()
  "Typing or pasting in the address field replaces the address Open
Location marks, though `delete-selection-mode' stays off: elsewhere text
typed still goes in beside the region, and other keys keep their ways."
  (let ((transient-mark-mode t)
        (delete-selection-mode nil)
        (kill-ring '("lispm.org"))
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (global (current-global-map))
        (buffer (lispm-desktop-browser--address-buffer)))
    (cl-letf (((symbol-function 'lispm-desktop-browser--shown-buffer) #'ignore)
              ((symbol-function 'lispm-desktop-browser--bookmarks-button) (lambda () "Bookmarks"))
              ((symbol-function 'lispm-desktop-platinum-icon) (lambda (_) "*"))
              ((symbol-function 'lispm-desktop-clipboard-text) (lambda () "example.com")))
      (unwind-protect
          (save-window-excursion
            ;; Command-V pastes, as `lispm-desktop-setup-keys' binds it.
            (use-global-map (define-keymap :parent global "M-v" #'lispm-desktop-paste))
            (switch-to-buffer buffer)
            (cl-flet ((type (keys)
                        (lispm-desktop-browser--render-address "https://duckduckgo.com/")
                        (lispm-desktop-browser-focus-address)
                        (execute-kbd-macro (kbd keys))
                        (lispm-desktop-browser--edited-address)))
              (should (equal (lispm-desktop-browser--target (type "github.com/RyanKung/lispm"))
                             "https://github.com/RyanKung/lispm"))
              (should (equal (type "C-y") "lispm.org"))
              (should (equal (type "M-v") "example.com"))
              ;; Moving and copying leave the address as it was.
              (should (equal (type "C-b M-w") "https://duckduckgo.com/"))
              (should (equal (car kill-ring) "https://duckduckgo.com"))
              ;; Once the mark is gone, text typed goes in at point.
              (should (equal (type "C-SPC C-SPC ?q=1") "https://duckduckgo.com/?q=1")))
            (with-temp-buffer
              (switch-to-buffer (current-buffer))
              (insert "abc")
              (push-mark (point-min) t t)
              (execute-kbd-macro "x")
              (should (equal (buffer-string) "abcx"))
              ;; Text pasted into a page leaves the buffer's region alone.
              (setq-local lispm-desktop-paste-function #'ignore)
              (should-not (lispm-desktop--paste-delete-selection))))
        (use-global-map global)
        (kill-buffer buffer)))))

(ert-deftest lispm-desktop-test-throbber-turns-while-loading ()
  "The throbber turns while the shown page loads and rests once it is done."
  (let ((lispm-desktop-browser--throbber-frame nil)
        (lispm-desktop-browser--throbber-timer nil)
        (loading t))
    (with-temp-buffer
      (setq-local xwidget-webkit--loading-p t)
      (cl-letf (((symbol-function 'lispm-desktop-browser--shown-buffer)
                 (let ((buffer (current-buffer)))
                   (lambda () (setq-local xwidget-webkit--loading-p loading) buffer))))
        (lispm-desktop-browser--turn-throbber)
        (lispm-desktop-browser--turn-throbber)
        (should (eql lispm-desktop-browser--throbber-frame 1))
        (setq loading nil)
        (lispm-desktop-browser--turn-throbber)
        (should-not lispm-desktop-browser--throbber-frame)))))

;;;; Settings

(ert-deftest lispm-desktop-test-settings-panels-draw ()
  "Each panel draws its settings: the themes, the desktop's picture and its
positions, the keys, the browser's pages."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always)
            ((symbol-function 'lispm-desktop-browser--load-history) (lambda () '(("u" "t" 0 1))))
            ((symbol-function 'lispm-desktop-browser-bookmarks) (lambda () nil)))
    (with-current-buffer (lispm-desktop-settings--buffer)
      (pcase-dolist (`(,panel . ,labels)
                     '((themes "Dracula" "Platinum")
                       (desktop "From the theme" "Fill" "Tile" "Arrange Icons")
                       (keyboard "Vim keys" "Emacs keys" "next start")
                       (browser "Home Page" "Clear History" "Edit Bookmarks")))
        (lispm-desktop-settings-select panel)
        (dolist (label labels)
          (should (string-search label (buffer-string))))
        (should (equal (nth 1 (assq panel lispm-desktop-settings-panels)) lispm-desktop-platinum-title))))))

(ert-deftest lispm-desktop-test-settings-radio-saves ()
  "Pressing a radio button saves its choice and draws the panel again."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((saved nil)
        (lispm-desktop-editing-style 'vim))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value) (push (cons variable value) saved))))
      (with-current-buffer (lispm-desktop-settings--buffer)
        (lispm-desktop-settings-select 'keyboard)
        (let ((button (next-button (point-min))))
          (while (and button (not (string-search "Emacs keys" (button-label button))))
            (setq button (next-button (button-end button))))
          (should button)
          (button-activate button))
        (should (equal (car saved) '(lispm-desktop-editing-style . emacs)))))))

(ert-deftest lispm-desktop-test-control-panels-folder ()
  "The Control Panels folder is a Finder folder of a panel each: opening an
item opens its panel, and nothing is made, renamed or thrown away there."
  (let ((opened nil)
        (lispm-desktop-finder-folders nil))
    (setf (alist-get 'control-panels lispm-desktop-finder-folders)
          '(:title "Control Panels" :icon "control-panels" :items lispm-desktop-settings--folder-items))
    (cl-letf (((symbol-function 'lispm-desktop-settings-show) (lambda (panel) (setq opened panel)))
              ((symbol-function 'lispm-desktop-finder--render) #'ignore))
      (lispm-desktop-finder--go 'control-panels)
      (with-current-buffer (lispm-desktop-finder--buffer)
        (should (member "Appearance" (mapcar #'car lispm-desktop-finder--items)))
        (should (equal (lispm-desktop-finder--icon-name lispm-desktop-finder--directory '("Keyboard"))
                       "keyboard-32"))
        (should (equal (lispm-desktop-finder--title lispm-desktop-finder--directory) "Control Panels"))
        (should-not (lispm-desktop-finder-real-p))
        (should-not (lispm-desktop-finder--parent lispm-desktop-finder--directory))
        (setq lispm-desktop-finder--selected "Appearance"))
      (lispm-desktop-finder-open)
      (should (eq opened 'themes))
      (should-error (lispm-desktop-finder-new-folder "x") :type 'user-error)
      (should-error (lispm-desktop-finder-move-to-trash) :type 'user-error)
      (lispm-desktop-finder--go temporary-file-directory)
      (should (lispm-desktop-finder-real-p)))))

(ert-deftest lispm-desktop-test-settings-in-menus ()
  "The λ menu opens each panel; the Application menu lists Settings while
its window is open."
  (let ((settings (seq-find (lambda (item) (and (consp item) (equal (plist-get item :label) "Control Panels")))
                            (lispm-desktop-platinum-lambda-items))))
    (should settings)
    (should (member "Appearance" (mapcar (lambda (item) (plist-get item :label))
                                      (funcall (plist-get settings :submenu))))))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (cl-letf (((symbol-function 'lispm-desktop-settings-open-p) #'ignore))
      (should-not (member "Control Panels" (mapcar (lambda (item) (plist-get item :label))
                                             (lispm-desktop-apps-application-items)))))
    (cl-letf (((symbol-function 'lispm-desktop-settings-open-p) #'always))
      (should (member "Control Panels" (mapcar (lambda (item) (plist-get item :label))
                                         (lispm-desktop-apps-application-items)))))))

;;;; lispm-desktop-coherence

(ert-deftest lispm-desktop-test-coherence-inside-fraction ()
  "How much of a window lies over the desktop decides where it belongs."
  (should (= 1.0 (lispm-desktop-coherence-inside-fraction '(10 10 100 100) '(0 0 1000 800))))
  (should (= 0.25 (lispm-desktop-coherence-inside-fraction '(950 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(1200 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(0 0 0 100) '(0 0 1000 800)))))

(defmacro lispm-desktop-test--with-drop (child inside hidden &rest body)
  "Run BODY with a window dropped: a CHILD of the screen or out, INSIDE its
fraction over the screen, the screen HIDDEN or not.  `out' and `in' are
bound to whether the window was taken out or brought in."
  (declare (indent 3))
  `(let ((out nil) (in nil))
     (cl-letf (((symbol-function 'lispm-desktop-coherence-available-p) #'always)
               ((symbol-function 'frame-parameter)
                (lambda (_frame parameter) (eq parameter 'lispm-desktop-window)))
               ((symbol-function 'frame-parent) (lambda (_) (and ,child 'screen)))
               ((symbol-function 'lispm-desktop-apps-window-screen) (lambda (_) 'screen))
               ((symbol-function 'lispm-desktop-apps-hidden-p) (lambda (&rest _) ,hidden))
               ((symbol-function 'frame-live-p) #'always)
               ((symbol-function 'lispm-desktop-coherence--outer-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence--screen-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence-inside-fraction) (lambda (&rest _) ,inside))
               ((symbol-function 'lispm-desktop-coherence-take-out) (lambda (_) (setq out t)))
               ((symbol-function 'lispm-desktop-coherence-bring-in) (lambda (_) (setq in t))))
       (lispm-desktop-coherence--dropped 'window)
       ,@body)))

(ert-deftest lispm-desktop-test-coherence-drop-out-and-in ()
  "A window dropped mostly past the desktop's edge goes out; one out dropped
mostly over the desktop shown comes in; nothing else moves a window."
  (lispm-desktop-test--with-drop t 0.2 nil (should out) (should-not in))
  (lispm-desktop-test--with-drop t 0.8 nil (should-not out) (should-not in))
  (lispm-desktop-test--with-drop nil 0.8 nil (should in) (should-not out))
  (lispm-desktop-test--with-drop nil 0.8 t (should-not in) (should-not out))
  (lispm-desktop-test--with-drop nil 0.2 nil (should-not in) (should-not out)))

(ert-deftest lispm-desktop-test-coherence-application-menu ()
  "The Application menu begins with Hide Desktop, or Show Desktop while the
desktop is hidden, where windows can go out."
  (let ((lispm-desktop-apps--frontmost 'desktop))
    (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
              ((symbol-function 'lispm-desktop-coherence-available-p) #'always))
      (cl-letf (((symbol-function 'lispm-desktop-apps-hidden-p) #'ignore))
        (let ((items (lispm-desktop-apps-application-items)))
          (should (equal (plist-get (car items) :label) "Hide Desktop"))
          (should (eq (cadr items) 'separator))))
      (cl-letf (((symbol-function 'lispm-desktop-apps-hidden-p) #'always))
        (should (equal (plist-get (car (lispm-desktop-apps-application-items)) :label) "Show Desktop"))))))

(ert-deftest lispm-desktop-test-coherence-geometry-counts-from-the-screen ()
  "A window out remembers its place over its screen, as one floating there:
it opens there again, a child of the screen."
  (cl-letf (((symbol-function 'frame-position) (lambda (_) '(500 . 300)))
            ((symbol-function 'frame-parent) #'ignore)
            ((symbol-function 'lispm-desktop-apps-out-p) (lambda (_) 'screen))
            ((symbol-function 'lispm-desktop-apps--screen-origin) (lambda (_) '(40 . 100)))
            ((symbol-function 'frame-text-width) (lambda (_) 700))
            ((symbol-function 'frame-text-height) (lambda (_) 400)))
    (should (equal (lispm-desktop-apps--geometry 'window) '(460 200 700 400)))))

(ert-deftest lispm-desktop-test-coherence-keep-on-screen ()
  "A window out keeps its title bar under the menu bar and part of it on the
computer's screen."
  (let ((moved nil))
    (cl-letf (((symbol-function 'lispm-desktop-apps--workarea) (lambda (_) '(0 25 1728 1000)))
              ((symbol-function 'frame-position) (lambda (_) '(2000 . 5)))
              ((symbol-function 'frame-pixel-width) (lambda (_) 700))
              ((symbol-function 'set-frame-position) (lambda (_ x y) (setq moved (list x y)))))
      (lispm-desktop-apps--keep-on-screen 'window)
      (should (equal moved '(1648 25))))))

(ert-deftest lispm-desktop-test-coherence-screen-of-a-window-out ()
  "The screen is the desktop's frame, also while a window out is selected."
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'window))
            ((symbol-function 'frame-parent) #'ignore)
            ((symbol-function 'frame-parameter) (lambda (_ parameter) (and (eq parameter 'lispm-desktop-out) 'screen)))
            ((symbol-function 'frame-live-p) #'always))
    (should (eq (lispm-desktop-apps-screen) 'screen))))

;;;; lispm-desktop-term

(defmacro lispm-desktop-test--with-term (bindings &rest body)
  "Run BODY with BINDINGS of terminal buffers made by `lispm-desktop-term-make', each
(VAR NAME PROGRAM ARGS), killed afterwards; skipped where the module is
not built."
  (declare (indent 1))
  `(progn
     (skip-unless (lispm-desktop-term-ready-p))
     (let ,(mapcar (pcase-lambda (`(,var ,name ,program ,args))
                     `(,var (lispm-desktop-term-make ,name ,program ,args temporary-file-directory)))
                   bindings)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (b) `(let ((kill-buffer-query-functions nil)) (kill-buffer ,(car b))))
                   bindings)))))

(defun lispm-desktop-test--term-wait (buffer &optional seconds)
  "Wait until BUFFER's terminal program exited, at most SECONDS."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (null (buffer-local-value 'lispm-desktop-term--exit-code buffer))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (lispm-desktop-term--tick))))

(ert-deftest lispm-desktop-test-term-draws-colours-and-input ()
  "A terminal draws its program's text with the cells' colours, takes what
is typed, keeps its last screen when the program exits, and reports the
exit code."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-term" "/bin/sh"
                                        '("-c" "printf 'a\\033[1;32mgreen\\033[0mb\\n'; read line; echo \"got:$line\"; exit 3")))
    (with-current-buffer term
      (accept-process-output nil 0.2)
      (lispm-desktop-term-send-string "hello\r")
      (lispm-desktop-test--term-wait term)
      (goto-char (point-min))
      (should (search-forward "agreenb" nil t))
      (should (equal (get-text-property (+ (point-min) 1) 'face)
                     '(:foreground "#00ff00" :weight bold)))
      (should (search-forward "got:hello" nil t))
      (should (search-forward "[Process exited with code 3]" nil t))
      (should (eql lispm-desktop-term--exit-code 3))
      (should-not (lispm-desktop-term-live-p)))))

(ert-deftest lispm-desktop-test-term-environment-is-emacs-own ()
  "The program sees `process-environment', not the environment Emacs was
started with."
  (let ((process-environment (cons "LISPM_DESKTOP_TEST_VALUE=from-lisp"
                                   (seq-remove (lambda (e) (string-prefix-p "HOME=" e))
                                               process-environment))))
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-env" "/bin/sh"
                                          '("-c" "echo \"[$LISPM_DESKTOP_TEST_VALUE][${HOME:-none}][$TERM]\"")))
      (lispm-desktop-test--term-wait term)
      (with-current-buffer term
        (goto-char (point-min))
        (should (search-forward "[from-lisp][none][xterm-256color]" nil t))))))

(ert-deftest lispm-desktop-test-term-hidden-terminal-does-not-wake-emacs ()
  "A terminal no window shows never wakes Emacs for its output, only for
its exit: its program's writing costs Emacs's thread nothing."
  (skip-unless (lispm-desktop-term-ready-p))
  (let ((woken nil))
    (cl-letf (((symbol-function 'lispm-desktop-term--schedule) #'ignore))
      (setq lispm-desktop-term--pending nil)
      (let ((term (lispm-desktop-term-make "lispm-desktop-test-hidden" "/bin/sh"
                                           '("-c" "for i in 1 2 3 4 5; do echo line $i; sleep 0.05; done; sleep 0.3")
                                           temporary-file-directory)))
        (unwind-protect
            (with-current-buffer term
              (let ((deadline (+ (float-time) 0.25)))
                (while (< (float-time) deadline)
                  (accept-process-output nil 0.02)))
              (setq woken (memq lispm-desktop-term--id lispm-desktop-term--pending))
              (should-not woken)
              (let ((deadline (+ (float-time) 3)))
                (while (and (not (memq lispm-desktop-term--id lispm-desktop-term--pending)) (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should (memq lispm-desktop-term--id lispm-desktop-term--pending))
              (should (string-match-p "line 5" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer term)))))))

(ert-deftest lispm-desktop-test-term-resizes ()
  "Resizing a terminal tells its program the new size."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-size" "/bin/sh" '("-c" "sleep 0.3; stty size")))
    (with-current-buffer term
      (lispm-desktop-term-module-resize lispm-desktop-term--terminal 57 13)
      (lispm-desktop-test--term-wait term)
      (should (string-match-p "13 57" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil))))))

(ert-deftest lispm-desktop-test-term-starts-at-selected-window-size ()
  "A terminal no window shows yet starts the size of the selected window,
so the program sees no resize when the terminal is shown there."
  (let ((size (lispm-desktop-term--window-size (selected-window))))
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-start-size" "/bin/sh" '("-c" "stty size")))
      (lispm-desktop-test--term-wait term)
      (with-current-buffer term
        (should (equal lispm-desktop-term--size size))
        (should (string-match-p (format "%d %d" (cdr size) (car size))
                                (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))))

(ert-deftest lispm-desktop-test-term-kill-running-program ()
  "Killing a terminal whose program still runs hangs the program up, and
Emacs keeps running when it exits.  Its exit once raised SIGPIPE, which
ends a batch Emacs such as the one running these tests."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-kill" "sleep" '("30")))
    (let ((pid (with-current-buffer term (lispm-desktop-term-module-pid lispm-desktop-term--terminal)))
          (deadline (+ (float-time) 5)))
      (accept-process-output nil 0.2)
      (should (zerop (signal-process pid 0)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer term))
      ;; Once the program is reaped, the module hears of its exit.
      (while (and (zerop (signal-process pid 0)) (< (float-time) deadline))
        (accept-process-output nil 0.02)
        (lispm-desktop-term--tick))
      (should-not (zerop (signal-process pid 0)))
      (dotimes (_ 10)
        (accept-process-output nil 0.02)
        (lispm-desktop-term--tick)))))

(ert-deftest lispm-desktop-test-term-channel-closes-and-reopens ()
  "The module stops writing to its channel before Emacs exits and closes
it, as a write to a closed pipe raises SIGPIPE; a channel made after that
wakes Emacs instead."
  (skip-unless (lispm-desktop-term-ready-p))
  (should (memq #'lispm-desktop-term--close-channel kill-emacs-hook))
  (let ((old lispm-desktop-term--channel))
    (lispm-desktop-term--close-channel)
    (delete-process old)
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-channel" "/bin/sh" '("-c" "exit 4")))
      (should-not (eq lispm-desktop-term--channel old))
      (lispm-desktop-test--term-wait term)
      (should (eql (buffer-local-value 'lispm-desktop-term--exit-code term) 4)))))

(ert-deftest lispm-desktop-test-agent-list-refreshes-after-agents-only ()
  "Only an agent terminal's exit refreshes the agent sidebar, and only once
Emacs is idle: reading transcripts must not stall the terminal scheduler."
  (let ((lispm-desktop-agent-list--refresh-timer nil)
        (scheduled 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer))
              ((symbol-function 'lispm-desktop-agent-list-refresh)
               (lambda () (error "Refreshed synchronously"))))
      (with-temp-buffer
        (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
        (should (= scheduled 0)))
      (with-current-buffer (get-buffer-create "*claude:lispm-desktop-test*")
        (unwind-protect
            (progn
              (setq major-mode 'lispm-desktop-term-mode)
              (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
              (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
              (should (= scheduled 1)))
          (kill-buffer))))))

;;;;; Program status (OSC 7501)

(defun lispm-desktop-test--term-until (buffer predicate &optional seconds)
  "Run the terminal scheduler until PREDICATE holds in BUFFER, at most
SECONDS; return PREDICATE's value."
  (let ((deadline (+ (float-time) (or seconds 5)))
        (value nil))
    (while (and (not (setq value (with-current-buffer buffer (funcall predicate))))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (lispm-desktop-term--tick))
    value))

(ert-deftest lispm-desktop-test-term-program-status ()
  "What a program reports of itself (OSC 7501) reaches its terminal's
buffer; what it finished is dismissed once the user types, and what it
was still doing ends with it."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-status" "/bin/sh"
             (list "-c" (concat "printf '\\033]7501;state=working:id=build:progress=40:app=make\\007'; "
                                "printf '\\033]7501;state=blocked:kind=permission:msg="
                                (base64-encode-string "Run tests?") "\\033\\\\'; "
                                "read line; printf '\\033]7501;state=done\\007'; read line; read line"))))
    (should (lispm-desktop-test--term-until
             term (lambda () (eq (plist-get (lispm-desktop-term-status) :state) 'blocked))))
    (with-current-buffer term
      (should (equal lispm-desktop-term-status-records
                     '((:id nil :state blocked :kind permission :progress nil :app nil :title nil
                            :msg "Run tests?")
                       (:id "build" :state working :kind nil :progress 40 :app "make" :title nil
                            :msg nil))))
      (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status)) "needs approval"))
      (should (memq term (lispm-desktop-term-waiting-buffers)))
      (lispm-desktop-term-send-string "\r"))
    (should (lispm-desktop-test--term-until
             term (lambda () (eq (plist-get (car lispm-desktop-term-status-records) :state) 'done))))
    ;; A task still working outweighs the program's own done.
    (should (eq (plist-get (lispm-desktop-term-status term) :state) 'working))
    (with-current-buffer term (lispm-desktop-term-send-string "\r"))
    (should (lispm-desktop-test--term-until
             term (lambda () (equal (mapcar (lambda (r) (plist-get r :id)) lispm-desktop-term-status-records)
                                    '("build")))))
    (should-not (memq term (lispm-desktop-term-waiting-buffers)))
    (with-current-buffer term (lispm-desktop-term-send-string "\r"))
    (lispm-desktop-test--term-wait term)
    (should-not (buffer-local-value 'lispm-desktop-term-status-records term))))

(ert-deftest lispm-desktop-test-term-program-status-query ()
  "A program asking whether the terminal takes status reports is answered,
before the answer to what it asks next."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-status-query" "/bin/sh"
             '("-c" "stty -icanon -echo min 0 time 10; printf '\\033]7501;?\\033\\\\\\033[c'; sleep 0.3; reply=$(dd bs=256 count=1 2>/dev/null); printf '%s\\n' \"$reply\" | tr '\\033' E")))
    (lispm-desktop-test--term-wait term)
    ;; Joined: the answers are longer than the terminal is wide.
    (let ((text (string-replace "\n" "" (with-current-buffer term
                                          (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))
      (should (string-match-p
               (regexp-quote "E]7501;?:states=idle,working,done,blocked,error:kinds=permission,question,authE\\E[?")
               text)))))

(ert-deftest lispm-desktop-test-term-status-picks-what-matters ()
  "A terminal's status is the record that matters most to the user, and
its label says what the program waits for."
  (with-temp-buffer
    (setq lispm-desktop-term-status-records
          '((:id nil :state working :progress 30)
            (:id "a" :state done)
            (:id "b" :state blocked :kind question :msg "Which branch?")))
    (let ((record (lispm-desktop-term-status)))
      (should (equal (plist-get record :id) "b"))
      (should (equal (lispm-desktop-term-status-label record) "has a question"))
      (should (equal (get-text-property 0 'help-echo (lispm-desktop-term-status-label record))
                     "Which branch?")))
    (setq lispm-desktop-term-status-records '((:id nil :state working :progress 30) (:id "a" :state done)))
    (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status)) "working 30%"))
    (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status) t) "working"))
    (setq lispm-desktop-term-status-records nil)
    (should-not (lispm-desktop-term-status))))

(ert-deftest lispm-desktop-test-agent-next-waiting ()
  "\\`C-c a n' goes to the terminals waiting for the user, those blocked
first, then those failed or done, and round again."
  (should (eq (keymap-lookup lispm-desktop-agent-map "n") #'lispm-desktop-agent-next-waiting))
  (should-error (lispm-desktop-agent-next-waiting) :type 'user-error)
  (let ((buffers nil)
        (shown nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(,name . ,state) '(("busy" . working) ("done" . done) ("asks" . blocked) ("quiet")))
            (with-current-buffer (generate-new-buffer (format " *lispm-desktop-test-%s*" name))
              (setq lispm-desktop-term--terminal 'fake)
              (when state
                (setq lispm-desktop-term-status-records (list (list :state state))))
              (push (current-buffer) buffers)))
          (setq buffers (nreverse buffers))
          (should (equal (lispm-desktop-term-waiting-buffers) (list (nth 2 buffers) (nth 1 buffers))))
          (cl-letf (((symbol-function 'lispm-desktop-agent--show) (lambda (buffer) (push buffer shown)))
                    ((symbol-function 'lispm-desktop-sidebar-show-in-main) (lambda (buffer) (push buffer shown))))
            (lispm-desktop-agent-next-waiting)
            (should (eq (car shown) (nth 2 buffers)))
            (with-current-buffer (nth 2 buffers) (lispm-desktop-agent-next-waiting))
            (should (eq (car shown) (nth 1 buffers)))
            (with-current-buffer (nth 1 buffers) (lispm-desktop-agent-next-waiting))
            (should (eq (car shown) (nth 2 buffers)))))
      (mapc #'kill-buffer buffers))))

(ert-deftest lispm-desktop-test-agent-list-refreshes-on-status-changes ()
  "An agent terminal's status refreshes the agent sidebar, once Emacs is
idle, when its label there changes; progress alone does not."
  (let ((lispm-desktop-agent-list--refresh-timer nil)
        (scheduled 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) nil)))
      (with-current-buffer (get-buffer-create "*codex:lispm-desktop-test*")
        (unwind-protect
            (progn
              (setq major-mode 'lispm-desktop-term-mode)
              (setq lispm-desktop-term-status-records '((:state working :progress 10)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (setq lispm-desktop-term-status-records '((:state working :progress 20)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (should (= scheduled 1))
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "working"))
              (setq lispm-desktop-term-status-records '((:state blocked :kind permission)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (should (= scheduled 2))
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "needs approval"))
              (setq lispm-desktop-term-status-records nil)
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "running")))
          (kill-buffer))))))

(ert-deftest lispm-desktop-test-title-bar-shows-program-status ()
  "A terminal's title bar shows what its program says it is doing."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let* ((lispm-desktop-platinum--palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000"
                                                                                  "#777777" "#6363CE"))
         (bar (lispm-desktop-platinum--draw-title-bar 600 t nil "codex" nil '(on on off) nil
                                                      (propertize "needs approval" 'face 'warning)))
         (at (string-search "codex  needs approval" bar)))
    (should at)
    (should (memq 'warning (ensure-list (get-text-property (+ at 8) 'face bar))))))

(ert-deftest lispm-desktop-test-term-module-stale-after-source-change ()
  "The module counts as stale once a source is newer than its library."
  (let* ((root (make-temp-file "lispm-desktop-term-src" t))
         (lispm-desktop-term-source-directory root)
         (library (expand-file-name "target/release/liblispm_desktop_term.dylib" root))
         (source (expand-file-name "src/lib.rs" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory library) t)
          (make-directory (file-name-directory source) t)
          (dolist (file (list source (expand-file-name "Cargo.toml" root)
                              (expand-file-name "Cargo.lock" root)))
            (write-region "" nil file)
            (set-file-times file (time-subtract nil 60)))
          (write-region "" nil library)
          (should (equal (lispm-desktop-term--module-file) library))
          (should-not (lispm-desktop-term--stale-p))
          (set-file-times source (time-add nil 60))
          (should (lispm-desktop-term--stale-p)))
      (delete-directory root t))))

(ert-deftest lispm-desktop-test-term-wheel ()
  "The wheel is reported to a program that asked for the mouse, turned into
cursor keys for one holding the whole screen, and else scrolls the
history, as Alacritty does; with Shift it always scrolls the history."
  (let ((window (selected-window))
        (old (window-buffer (selected-window)))
        sent scrolled)
    (with-temp-buffer
      (setq-local lispm-desktop-term--size '(80 . 24))
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-term--send) (lambda (string) (push string sent)))
                    ((symbol-function 'lispm-desktop-term-module-scroll)
                     (lambda (_ lines) (push lines scrolled)))
                    ((symbol-function 'lispm-desktop-term--render) #'ignore))
            (let ((event (lambda (type)
                           (list type (list window 1 '(10 . 5) 0 nil 1 '(10 . 5) nil nil '(1 . 1))))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) (logior 4 8 16))))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (lispm-desktop-term-wheel-down (funcall event 'wheel-down))
                (should (equal (reverse sent) '("\e[<64;11;6M" "\e[<65;11;6M")))
                (setq sent nil)
                (lispm-desktop-term-wheel-up (funcall event 'S-wheel-up))
                (should-not sent)
                (should (equal scrolled '(3))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 8)))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (should (equal sent '("\e[M`+&"))))
              (setq sent nil scrolled nil)
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 4)))
                (lispm-desktop-term-wheel-down (funcall event 'wheel-down))
                (should (equal sent '("\eOB\eOB\eOB"))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (should (equal scrolled '(3))))))
        (set-window-buffer window old)))))

(ert-deftest lispm-desktop-test-term-clicks ()
  "A program that takes the mouse is told of a press, of the moves while
the button is held when it asked for them, and of the release; without
mouse reports, and with Shift, clicks stay Emacs's."
  (let ((window (selected-window))
        (old (window-buffer (selected-window)))
        sent)
    (with-temp-buffer
      (setq-local lispm-desktop-term--size '(80 . 24))
      (lispm-desktop-term-keys-mode 1)
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-term--send) (lambda (string) (push string sent))))
            (let ((posn (lambda (x y) (list window 1 (cons x y) 0 nil 1 (cons x y) nil nil '(1 . 1)))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) (logior 8 16 128))))
                (should (eq (key-binding [down-mouse-1]) #'lispm-desktop-term-mouse-press))
                (should (eq (key-binding [mouse-1]) #'ignore))
                (should (eq (key-binding [S-down-mouse-1]) #'lispm-desktop-term-mouse-emacs))
                (let ((unread-command-events
                       (list (list 'mouse-movement (funcall posn 4 2))
                             (list 'mouse-movement (funcall posn 4 2))
                             (list 'drag-mouse-1 (funcall posn 1 1) (funcall posn 6 3)))))
                  (lispm-desktop-term-mouse-press (list 'down-mouse-1 (funcall posn 1 1)))
                  (should (equal (reverse sent)
                                 '("\e[<0;2;2M" "\e[<32;5;3M" "\e[<0;7;4m")))))
              (setq sent nil)
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 8)))
                (let ((unread-command-events
                       (list (list 'mouse-movement (funcall posn 4 2))
                             (list 'mouse-3 (funcall posn 2 0)))))
                  (lispm-desktop-term-mouse-press (list 'down-mouse-3 (funcall posn 2 0)))
                  (should (equal (reverse sent) '("\e[M\"#!" "\e[M##!")))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
                (should-not (eq (key-binding [down-mouse-1]) #'lispm-desktop-term-mouse-press))
                (should-not (eq (key-binding [mouse-1]) #'ignore)))))
        (set-window-buffer window old)))))

(defmacro lispm-desktop-test--with-quit (answers documents programs &rest body)
  "Run BODY quitting through alerts answered by ANSWERS in turn, with
DOCUMENTS unsaved and PROGRAMS running.  Bind `asked' to the alerts'
messages and details, `killed', `saved' to what was killed and saved."
  (declare (indent 3))
  `(let ((answers ,answers) (asked nil) (killed nil) (saved nil)
         (kill-emacs-query-functions nil) (save-some-buffers-functions nil)
         (confirm-kill-processes t))
     (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) (lambda () t))
               ((symbol-function 'lispm-desktop-popup-alert)
                (lambda (_icon message buttons &optional detail)
                  (push (list message detail (mapcar #'car buttons)) asked)
                  (pop answers)))
               ((symbol-function 'lispm-desktop-platinum--unsaved-documents) (lambda () ,documents))
               ((symbol-function 'lispm-desktop-platinum--running-programs) (lambda () ,programs))
               ((symbol-function 'save-buffer) (lambda (&rest _) (push (current-buffer) saved)))
               ((symbol-function 'kill-emacs) (lambda (&rest args) (push args killed))))
       ,@body)))

(ert-deftest lispm-desktop-test-quit-asks-in-alerts ()
  "Quitting asks of each unsaved document and of the programs running in
alerts; Cancel keeps Emacs, Don't Save and Quit end it."
  (with-temp-buffer
    (let ((document (current-buffer))
          (confirm-kill-emacs nil))
      (lispm-desktop-test--with-quit '(cancel) (list document) nil
        (lispm-desktop-platinum--quit #'ignore)
        (should-not killed)
        (should (= (length asked) 1))
        (should (string-match-p "Save changes to the document .* before quitting\\?"
                                (car (car asked)))))
      (lispm-desktop-test--with-quit '(discard quit) (list document) '("claude" "zsh")
        (lispm-desktop-platinum--quit #'ignore)
        (should (equal killed '((nil nil))))
        (should-not saved)
        (should (equal (cadr (car asked)) "Quitting stops “claude” and “zsh”."))
        (should (equal (nth 2 (car asked)) '("Cancel" "Quit"))))
      (lispm-desktop-test--with-quit '(save cancel) (list document) '("zsh")
        (lispm-desktop-platinum--quit #'ignore nil t)
        (should (equal saved (list document)))
        (should-not killed)
        (should (equal (car (car asked)) "Restart Emacs while programs are running?"))))))

(ert-deftest lispm-desktop-test-quit-yes-or-no-questions-are-alerts ()
  "The questions of `kill-emacs-query-functions' and `confirm-kill-emacs'
are alerts too, the latter asked only when nothing else was."
  (lispm-desktop-test--with-quit '(yes no) nil nil
    (let ((confirm-kill-emacs #'y-or-n-p))
      (add-hook 'kill-emacs-query-functions
                (lambda () (yes-or-no-p "This Emacs session has clients; exit anyway? ")))
      (lispm-desktop-platinum--quit #'ignore)
      (should-not killed)
      (should (equal (mapcar #'car (reverse asked))
                     '("This Emacs session has clients; exit anyway?" "Really quit Emacs?")))
      (should (equal (nth 2 (car asked)) '("No" "Yes")))))
  (lispm-desktop-test--with-quit '() nil nil
    (let ((confirm-kill-emacs nil))
      (lispm-desktop-platinum--quit #'ignore)
      (should (equal killed '((nil nil))))
      (should-not asked)))
  (should (equal (lispm-desktop-platinum--names '("a" "b" "c" "d" "e")) "“a”, “b”, “c” and 2 others"))
  (should (equal (lispm-desktop-platinum--names '("*shell:a*")) "“shell:a”")))

(ert-deftest lispm-desktop-test-typed-events ()
  "Characters and function keys are typed; mouse events and the area
symbols `read-event' returns before them are not, so a panel dropped
from the menu bar outlives the release of the click that dropped it."
  (should (lispm-desktop-typed-event-p ?a))
  (should (lispm-desktop-typed-event-p 'f1))
  (should (lispm-desktop-typed-event-p 'escape))
  (should-not (lispm-desktop-typed-event-p 'tab-bar))
  (should-not (lispm-desktop-typed-event-p 'mode-line))
  (should-not (lispm-desktop-typed-event-p '(mouse-1 nil)))
  (should-not (lispm-desktop-typed-event-p nil)))

(ert-deftest lispm-desktop-test-menu-typed-keys ()
  "Menus are walked from the keyboard: a typed letter goes to the next
enabled item it starts, and the keys shown beside items can be typed:
no function keys, and no Super keys where no key is Super."
  (let ((items '((:label "Undo" :action ignore) separator (:label "Paste" :action ignore)
                 (:label "Print" :action ignore :enabled nil)
                 (:label " paste again" :action ignore))))
    (should (= (lispm-desktop-popup--item-starting items nil ?p) 2))
    (should (= (lispm-desktop-popup--item-starting items 2 ?P) 4))
    (should (= (lispm-desktop-popup--item-starting items 4 ?p) 2))
    (should-not (lispm-desktop-popup--item-starting items nil ?z)))
  (should (lispm-desktop-popup--keyboard-p (kbd "C-x h")))
  (should (lispm-desktop-popup--keyboard-p (kbd "M-RET")))
  (should-not (lispm-desktop-popup--keyboard-p (kbd "<f10>")))
  (should-not (lispm-desktop-popup--keyboard-p (kbd "<open>")))
  (should-not (lispm-desktop-popup--keyboard-p (kbd "M-<down>")))
  (should-not (lispm-desktop-popup--keyboard-p (kbd "<mouse-1>")))
  (let ((window-system 'ns) (ns-command-modifier 'meta) (ns-alternate-modifier 'none))
    (should-not (lispm-desktop-popup--keyboard-p (kbd "s-a"))))
  (should (lispm-desktop-popup--keyboard-p (kbd "M-c"))))

(ert-deftest lispm-desktop-test-browser-hint-labels ()
  "Hint labels are distinct and all as long, so none begins another: one
letter each while the letters suffice, two when they do not."
  (require 'lispm-desktop-browser-hints)
  (let ((lispm-desktop-browser-hint-characters "asd"))
    (should (equal (lispm-desktop-browser-hint-labels 3) '("a" "s" "d")))
    (let ((labels (lispm-desktop-browser-hint-labels 7)))
      (should (= (length labels) 7))
      (should (= (length (delete-dups (copy-sequence labels))) 7))
      (should (seq-every-p (lambda (label) (= (length label) 2)) labels))
      (should (equal (car labels) "aa")))
    (should-not (lispm-desktop-browser-hint-labels 0))))

(ert-deftest lispm-desktop-test-term-keys ()
  "Keys send the bytes terminals expect: cursor keys follow the program's
mode, C-c C-c sends C-c, and Escape reaches the program."
  (should (equal (cdr (assoc "C-c C-c" lispm-desktop-term-keys)) "\C-c"))
  (should (equal (cdr (assoc "<escape>" lispm-desktop-term-keys)) "\e"))
  (should (equal (cdr (assoc "S-<return>" lispm-desktop-term-keys)) "\e[13;2u"))
  (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 1)))
    (should (equal (lispm-desktop-term--cursor-key "A") "\eOA")))
  (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
    (should (equal (lispm-desktop-term--cursor-key "A") "\e[A")))
  (should (eq (lookup-key lispm-desktop-term-keys-mode-map (kbd "C-c C-c")) #'lispm-desktop-term-send-key))
  (should (eq (lookup-key lispm-desktop-term-keys-mode-map (kbd "M-x")) #'execute-extended-command)))

(provide 'lispm-desktop-test)
;;; lispm-desktop-test.el ends here
