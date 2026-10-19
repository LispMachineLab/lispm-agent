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

(ert-deftest lispm-desktop-test-claude-follows-emacs-theme ()
  "Claude Code starts with its theme \"auto\", asking the terminal for
Emacs's colours, unless its user chose a theme: in its user, project or
local settings, or in its old global configuration other than the default
\"dark\".  Settings it cannot read choose nothing."
  (let* ((home (make-temp-file "lispm-desktop-test-home" t))
         (project (expand-file-name "project/" home))
         (process-environment (cons (concat "HOME=" home) process-environment))
         (auto '("--settings" "{\"theme\":\"auto\"}")))
    (cl-flet ((put-json (file text)
                (make-directory (file-name-directory file) t)
                (with-temp-file file (insert text))))
      (unwind-protect
          (progn
            (make-directory project)
            (should (equal (lispm-desktop-agent--theme-arguments 'claude project) auto))
            (should-not (lispm-desktop-agent--theme-arguments 'codex project))
            (should-not (lispm-desktop-agent--theme-arguments 'hermes project))
            (put-json (expand-file-name ".claude.json" home) "{\"numStartups\":3,\"theme\":\"dark\"}")
            (put-json (expand-file-name ".claude/settings.json" home) "{\"theme\":")
            (should (equal (lispm-desktop-agent--theme-arguments 'claude project) auto))
            (dolist (file (list (expand-file-name ".claude.json" home)
                                (expand-file-name ".claude/settings.json" home)
                                (expand-file-name ".claude/settings.json" project)
                                (expand-file-name ".claude/settings.local.json" project)))
              (put-json file "{\"permissions\":{},\"theme\":\"light-daltonized\"}")
              (should-not (lispm-desktop-agent--theme-arguments 'claude project))
              (put-json file "[]"))
            ;; Launched, Claude gets the arguments before its own.
            (let (command)
              (cl-letf (((symbol-function 'lispm-desktop-agent--check-installed) #'ignore)
                        ((symbol-function 'lispm-desktop-agent-ide-start) #'ignore)
                        ((symbol-function 'lispm-desktop-terminal-make)
                         (lambda (_name &rest program)
                           (setq command program)
                           (get-buffer-create " *lispm-desktop-test-launch*"))))
                (unwind-protect
                    (lispm-desktop-agent--launch 'claude project "lispm-desktop-test-launch"
                                                 '("--session-id" "x"))
                  (kill-buffer " *lispm-desktop-test-launch*")))
              (should (equal command `("claude" ,@auto "--session-id" "x")))))
        (delete-directory home t)))))

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

(defmacro lispm-desktop-test--with-themes (&rest body)
  "Run BODY with two empty themes, `lispm-desktop-test-light' enabled and
saved with Customize as `lispm-desktop-theme', and
`lispm-desktop-test-dark'; Emacs set up, the variable watched.  Themes
loaded are pushed onto `loads', and timers onto `timers' instead of run:
`lispm-desktop-test--run-timers' runs them, as Emacs would once the
command is done."
  (declare (indent 0))
  `(let ((properties (list (get 'user 'theme-settings)
                           (get 'lispm-desktop-theme 'theme-value)
                           (get 'lispm-desktop-theme 'saved-value)
                           (get 'lispm-desktop-theme 'customized-value)))
         (lispm-desktop-theme nil)
         (custom-enabled-themes nil)
         (custom-known-themes custom-known-themes)
         (enable-theme-functions nil)
         (disable-theme-functions nil)
         (lispm-desktop--set-up t)
         (loads nil)
         (timers nil))
     (dolist (theme '(lispm-desktop-test-light lispm-desktop-test-dark))
       (custom-declare-theme theme (intern (format "%s-theme" theme))))
     (unwind-protect
         (cl-letf (((symbol-function 'load-theme)
                    (lambda (theme &rest _) (push theme loads) (enable-theme theme)))
                   ((symbol-function 'run-at-time)
                    (lambda (_time _repeat function &rest args) (push (cons function args) timers)))
                   ((symbol-function 'custom-save-all) #'ignore))
           (customize-set-variable 'lispm-desktop-theme 'lispm-desktop-test-light)
           (enable-theme 'lispm-desktop-test-light)
           (add-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
           ,@body)
       (remove-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
       (mapc #'disable-theme custom-enabled-themes)
       (pcase-let ((`(,settings ,value ,saved ,customized) properties))
         (put 'user 'theme-settings settings)
         (put 'lispm-desktop-theme 'theme-value value)
         (put 'lispm-desktop-theme 'saved-value saved)
         (put 'lispm-desktop-theme 'customized-value customized)))))

(defmacro lispm-desktop-test--run-timers (timers)
  "Run the functions the variable TIMERS holds, (FUNCTION . ARGS) newest
first, and those they leave in turn, ten rounds at most; return what is
left after that."
  `(let ((rounds 0))
     (while (and ,timers (< (setq rounds (1+ rounds)) 10))
       (let ((due (reverse ,timers)))
         (setq ,timers nil)
         (dolist (timer due)
           (apply (car timer) (cdr timer)))))
     ,timers))

(ert-deftest lispm-desktop-test-theme-switch-loads-once ()
  "Switching themes loads the new theme once, and only it, though the value
saved with Customize names the old one until the switch saves the new:
enabling a theme sets the variable back to that saved value meanwhile,
which once loaded the old theme again and then the new one."
  (lispm-desktop-test--with-themes
    (lispm-desktop-set-theme 'lispm-desktop-test-dark)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))
    (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
    (should (eq lispm-desktop-theme 'lispm-desktop-test-dark))))

(ert-deftest lispm-desktop-test-theme-set-loads-it ()
  "Setting `lispm-desktop-theme' once Emacs is set up loads that theme when
the command is done, and the variable keeps the value set, though another
was saved with Customize."
  (lispm-desktop-test--with-themes
    (setq lispm-desktop-theme 'lispm-desktop-test-dark)
    (should-not loads)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))
    (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
    (should (eq lispm-desktop-theme 'lispm-desktop-test-dark))
    ;; Set to the theme enabled, nothing is loaded again.
    (setq lispm-desktop-theme 'lispm-desktop-test-dark)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))))

(ert-deftest lispm-desktop-test-theme-loads-in-redisplay ()
  "On NS, a theme is loaded by the redisplay run for it, as it starts and
before it draws; it is loaded all the same when no redisplay runs, and
an error loading it is signalled once the redisplay is done."
  (lispm-desktop-test--with-themes
    (let ((events nil))
      ;; Only LispM Desktop is told it runs on NS: Emacs told so would treat
      ;; the batch frame as a window-system frame as a theme is disabled,
      ;; and CI's Emacs builds have no window system.
      (cl-letf (((symbol-function 'lispm-desktop--theme-in-redisplay-p) (lambda () t))
                ((symbol-function 'load-theme)
                 (lambda (theme &rest _) (push (list 'load theme) events) (enable-theme theme)))
                ((symbol-function 'redisplay)
                 (lambda (&rest _)
                   (push 'redisplay events)
                   (funcall pre-redisplay-function t)
                   (push 'drawn events))))
        (lispm-desktop-load-theme 'lispm-desktop-test-dark)
        (should (equal (reverse events) '(redisplay (load lispm-desktop-test-dark) drawn)))
        (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))
        ;; A redisplay that does not run, as while a keyboard macro runs.
        (setq events nil)
        (cl-letf (((symbol-function 'redisplay) #'ignore))
          (lispm-desktop-load-theme 'lispm-desktop-test-light))
        (should (equal events '((load lispm-desktop-test-light))))
        (should (equal custom-enabled-themes '(lispm-desktop-test-light)))
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))
        (cl-letf (((symbol-function 'load-theme)
                   (lambda (&rest _) (error "No such theme"))))
          (should (equal (should-error (lispm-desktop-load-theme 'lispm-desktop-test-dark))
                         '(error "No such theme"))))
        (should-not lispm-desktop--pending-theme)
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))))))

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
  "A short title sits in the middle of its bar, with ridges either side,
the boxes spaced as Mac OS 9 spaced them."
  (let* ((layout (lispm-desktop-platinum-title-layout 600 100))
         (s lispm-desktop-platinum-box-size)
         (ridges (plist-get layout :ridges)))
    (should (= (plist-get layout :title) 100))
    (should (= (plist-get layout :label-x) 250))
    ;; Close box three pixels in, four to the ridges.
    (should (= (plist-get layout :start) (+ 3 s)))
    (should (= (car (nth 0 ridges)) (+ 3 s 4)))
    ;; Four to the zoom box, three to the collapse box, three to the edge.
    (should (= (plist-get layout :end) (- 600 3 s 3 s)))
    (should (= (cdr (nth 1 ridges)) (- 600 3 s 3 s 5)))))

(ert-deftest lispm-desktop-test-title-layout-narrows-long-titles ()
  "A title too wide for its bar is narrowed, keeping ridges on both sides."
  (pcase-let* ((layout (lispm-desktop-platinum-title-layout 300 1000))
               (`((,a . ,b) (,c . ,d)) (plist-get layout :ridges)))
    (should (< (plist-get layout :title) 300))
    (should (>= (- b a) 12))
    (should (>= (- d c) 12))))

(ert-deftest lispm-desktop-test-title-layout-without-boxes ()
  "A bar lacking boxes gives their room to the title."
  (let ((all (lispm-desktop-platinum-title-layout 300 1000))
        (close-only (lispm-desktop-platinum-title-layout 300 1000 '(on nil nil))))
    (should (> (plist-get close-only :end) (plist-get all :end)))
    (should (> (plist-get close-only :title) (plist-get all :title)))))

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
  "Every box kind draws, plain, modified and pressed, in Dracula too."
  (let ((palette (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (dolist (kind '(close zoom collapse))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind t)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind nil t)) 'svg)))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-title-part palette 50 :active t :ridges '((0 . 49))))
                'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-triangle palette t)) 'svg))))

(ert-deftest lispm-desktop-test-title-box-pixels ()
  "The close box is a recessed well round a dark frame, a bevel and a
square shaded from grey to white; the zoom and collapse boxes add their
glyphs; the bar is lit along its top, shaded along its bottom."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (close (lispm-desktop-platinum-draw-box palette 'close))
         (zoom (lispm-desktop-platinum-draw-box palette 'zoom))
         (collapse (lispm-desktop-platinum-draw-box palette 'collapse)))
    (should (equal (lispm-desktop-test--size close) '(13 . 21)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at close 5 0) "#FFFFFF"))
      (should (equal (at close 5 1) "#CCCCCC"))
      (should (equal (at close 5 19) "#999999"))
      (should (equal (at close 5 20) "#000000"))
      (should (equal (at close 0 3) "#888888"))
      (should (equal (at close 12 15) "#FFFFFF"))
      (should (equal (at close 1 4) "#222222"))
      (should (equal (at close 2 5) "#FFFFFF"))
      (should (equal (at close 3 6) "#999999"))
      (should (equal (at close 9 12) "#FFFFFF"))
      (should (equal (at close 10 8) "#888888"))
      (should (equal (at zoom 7 6) "#222222"))
      (should (equal (at zoom 4 10) "#222222"))
      (should (equal (at collapse 6 8) "#222222"))
      (should (equal (at collapse 6 10) "#222222")))
    (let ((part (lispm-desktop-platinum-draw-title-part palette 40 :active t :ridges '((2 . 30)))))
      (cl-flet ((at (x y) (lispm-desktop-test--pixel part x y)))
        ;; Six ridges, their ends engraved.
        (should (equal (at 2 3) "#FFFFFF"))
        (should (equal (at 2 4) "#CCCCCC"))
        (should (equal (at 3 4) "#777777"))
        (should (equal (at 30 4) "#777777"))
        (should (equal (at 30 3) "#CCCCCC"))
        (should (equal (at 10 13) "#FFFFFF"))
        (should (equal (at 10 15) "#CCCCCC"))))
    (let ((inactive (lispm-desktop-platinum-draw-title-part palette 40)))
      (should (equal (lispm-desktop-test--pixel inactive 10 0) "#DDDDDD"))
      (should (equal (lispm-desktop-test--pixel inactive 10 20) "#555555")))))

(ert-deftest lispm-desktop-test-rgb-reads-hex-without-a-display ()
  "A #RRGGBB colour is read exactly, even in batch."
  (should (equal (lispm-desktop-tab-bar--rgb "#ff0080") (list 1.0 0.0 (/ 128 255.0))))
  (should (equal (lispm-desktop-tab-bar--blend "#000000" "#ff0080" 1.0) "#ff0080")))

(ert-deftest lispm-desktop-test-window-controls ()
  "A lone window zooms but cannot roll up; a side window only closes."
  (save-window-excursion
    (delete-other-windows)
    (should (equal (lispm-desktop-platinum--window-controls (selected-window)) '(on on nil)))
    (let ((below (split-window-below)))
      (should (equal (lispm-desktop-platinum--window-controls below) '(on on on))))
    (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test-side")
                                               '((side . left)))))
      (should (equal (lispm-desktop-platinum--window-controls side) '(on nil nil))))))

(ert-deftest lispm-desktop-test-chrome-controls ()
  "Chrome, such as the browser's address window, only closes, and its close
box runs the buffer's own close function."
  (save-window-excursion
    (delete-other-windows)
    (let ((chrome (split-window-below))
          (closed nil))
      (set-window-parameter chrome 'lispm-desktop-chrome t)
      (should (equal (lispm-desktop-platinum--window-controls chrome) '(on nil nil)))
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
      (should-not (get-text-property 0 'lispm-desktop-platinum-highlighted (nth 2 (car (last bar)))))))
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'always)
            ((symbol-function 'lispm-desktop-browser-workspace-p) #'always))
    (let ((web (nth 2 (car (last (lispm-desktop-platinum-menu-bar))))))
      (should (get-text-property 0 'lispm-desktop-platinum-highlighted web))))
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
  "Reads that come back empty, as the mouse is looked at between moves, do
not end the drag, nor do moves Emacs reports: the icon is still dropped
where it is let go."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop--place-copy) #'ignore)
            ((symbol-function 'lispm-desktop-popup--dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lispm-desktop-icon-places nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (reads 0)
           (pointer press)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (setq reads (1+ reads))
                   (cond ((> reads 10) '(drag-mouse-1 (nil) (nil)))
                         ;; Every other read times out with nothing.
                         ((cl-oddp reads)
                          (setq pointer (cons (- (car pointer) 20) (+ (cdr pointer) 10)))
                          nil)
                         (t '(mouse-movement (nil)))))))
        (should (lispm-desktop--drag 'home nil press)))
      (should (equal (alist-get 'home lispm-desktop-icon-places)
                     (lispm-desktop--snap (- (car cell) 100) (+ (cadr cell) 50) 800 600
                                          (mapcar #'cdr (seq-remove
                                                         (lambda (entry) (eq (car (car entry)) 'home))
                                                         (lispm-desktop--layout 800 600))))))
      (should-not unread-command-events))))

(ert-deftest lispm-desktop-test-desktop-copy-follows-the-pointer ()
  "The dragged copy follows the pointer pixel by pixel, keeping it where it
grabbed the icon, and only the drop snaps the icon to the grid; Escape
gives the drag up, the icon left where it was."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop-popup--dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lispm-desktop-icon-places nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (pointer press)
           (script nil)
           (placed nil)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'lispm-desktop--place-copy)
                 (lambda (_frames _key x y _window) (push (cons x y) placed)))
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (pcase-let ((`(,dx ,dy . ,event) (pop script)))
                     (setq pointer (cons (+ (car press) dx) (+ (cdr press) dy)))
                     event))))
        ;; A jitter shows no copy; then the copy goes wherever the pointer
        ;; does, by the pixel.
        (setq script '((-3 2) (-57 23) (-101 47) (-101 47 drag-mouse-1 (nil) (nil))))
        (should (lispm-desktop--drag 'home nil press))
        (should (equal placed (list (cons (- (car cell) 101) (+ (cadr cell) 47))
                                    (cons (- (car cell) 57) (+ (cadr cell) 23)))))
        ;; Let go, the icon is put on the grid cell nearest.
        (let ((saved (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600)))
          (should (equal (lispm-desktop--xy-place (car saved) (cdr saved) 800 600)
                         (lispm-desktop--snap (- (car cell) 101) (+ (cadr cell) 47) 800 600
                                              (mapcar #'cdr (seq-remove
                                                             (lambda (entry) (eq (car (car entry)) 'home))
                                                             (lispm-desktop--layout 800 600))))))
          (should-not (equal saved (car placed)))
          ;; Escape, and nothing more moves.
          (setq placed nil
                script '((-30 40) (-30 40 . 27)))
          (lispm-desktop--drag 'home nil press)
          (should placed)
          (should (equal (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600)
                         saved)))))))

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
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop--place-copy) #'ignore)
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
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (if moves
                       (progn (setq pointer (pop moves)) nil)
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
  "The dragged copy is the icon alone and its name inverted, as Mac OS 9's
drag image was, no piece of the desktop about them; the icon stands
centred at the top of its cell, its name under it."
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'string-pixel-width) (lambda (&rest _) 40)))
    (let ((lispm-desktop-picture nil))
      (pcase-let ((`(,icon . ,name) (lispm-desktop--copy-svgs 'home)))
        (should (string-match-p "width=\"32\" height=\"32\" viewBox=\"0 0 32 32\"" icon))
        (should-not (string-match-p (regexp-quote (lispm-desktop--color)) icon))
        (should-not (string-match-p "opacity" icon))
        (should (string-match-p (format "<rect width=\"48\" height=\"16\" fill=\"%s\"/>"
                                        (or (lispm-desktop-platinum--color :text) "#000000"))
                                name))
        (should (string-match-p (format "fill=\"%s\">Home</text></svg>\\'"
                                        (or (lispm-desktop-platinum--color :content) "#FFFFFF"))
                                name)))
      (should (equal (lispm-desktop--copy-place 'home 100 50)
                     (list (cons (+ 100 40 -16) 50) (cons (+ 100 40 -24) (+ 50 32 4))))))))

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

(ert-deftest lispm-desktop-test-finder-rolls-up ()
  "The Finder window's collapse box rolls it up to its title bar, its
header and its bottom line put away, and down to its height again."
  (save-window-excursion
    (delete-other-windows)
    (let ((lispm-desktop-finder--frame (selected-frame))
          (rolls nil))
      (set-window-buffer (selected-window) (lispm-desktop-finder--buffer))
      (cl-letf (((symbol-function 'lispm-desktop-platinum-roll-up)
                 (lambda (frame height) (push (list 'up frame height) rolls)))
                ((symbol-function 'lispm-desktop-platinum-roll-down)
                 (lambda (frame height) (push (list 'down frame height) rolls))))
        (let ((height (frame-text-height)))
          (lispm-desktop-finder--collapse-box nil)
          (should (equal (car rolls) (list 'up (selected-frame) (window-tab-line-height))))
          (lispm-desktop-finder--in
            (should-not header-line-format)
            (should-not mode-line-format))
          (lispm-desktop-finder--collapse-box nil)
          (should (equal (car rolls) (list 'down (selected-frame) height)))
          (lispm-desktop-finder--in
            (should header-line-format)
            (should mode-line-format)
            (should-not lispm-desktop-finder--shade)))))
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
      (cl-letf (((symbol-function 'lispm-desktop-browser-extensions-supported-p) #'ignore))
        (should (equal (lispm-desktop-test--menu-bar-titles)
                       '(lispm-desktop-lambda lispm-desktop-browser-file lispm-desktop-browser-view lispm-desktop-browser-go
                                              lispm-desktop-browser-bookmarks lispm-desktop-browser-window))))
      ;; Where pages can run Chrome extensions, they have a menu.
      (cl-letf (((symbol-function 'lispm-desktop-browser-extensions-supported-p) #'always))
        (should (equal (lispm-desktop-test--menu-bar-titles)
                       '(lispm-desktop-lambda lispm-desktop-browser-file lispm-desktop-browser-view lispm-desktop-browser-go
                                              lispm-desktop-browser-bookmarks lispm-desktop-browser-extensions-menu
                                              lispm-desktop-browser-window)))))))

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
  "A box held down is lit from below, its square darkened."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (up (lispm-desktop-platinum-draw-box palette 'close))
         (down (lispm-desktop-platinum-draw-box palette 'close nil t)))
    (should (equal (lispm-desktop-test--pixel up 9 12) "#FFFFFF"))
    (should (equal (lispm-desktop-test--pixel down 9 12) "#BBBBBB"))
    (should (equal (lispm-desktop-test--pixel down 10 8) "#CCCCCC"))))

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
  "The thumb is as long as the share shown, and as far down as the start;
the arrows take the bar's bottom."
  (let ((arrows (1+ (* 2 lispm-desktop-platinum-scroll-arrow))))
    ;; Everything shows: no thumb.
    (should-not (lispm-desktop-platinum-scroll-thumb 400 1 1000 1 1000))
    ;; The top of a buffer ten times the window: a tenth of the track, at its top.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 arrows) 1 101 1 1001)))
      (should (= top 0))
      (should (= length 30)))
    ;; The end: the thumb's bottom meets the arrows.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 arrows) 901 1001 1 1001)))
      (should (= (+ top length) 300)))
    ;; A short thumb is never shorter than a box.
    (should (= (cdr (lispm-desktop-platinum-scroll-thumb 400 1 2 1 100000)) 16))))

(ert-deftest lispm-desktop-test-scroll-bar-draws ()
  "Scroll bars draw active, inactive, and with nothing to scroll."
  (let ((palette (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 nil t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) nil)) 'svg))))

;;;; Controls

(defun lispm-desktop-test--platinum-palette ()
  "Return the palette the platinum theme gives: Mac OS 9's own colours."
  (let ((custom-theme-load-path (cons (expand-file-name "themes/" lispm-desktop-directory)
                                      custom-theme-load-path)))
    (unwind-protect
        (progn
          (load-theme 'platinum t)
          (lispm-desktop-platinum--merge-palette
           (lispm-desktop-platinum--derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")
           lispm-desktop-platinum-colors))
      (disable-theme 'platinum))))

(defun lispm-desktop-test--dracula-palette ()
  "Return the palette derived from Dracula's colours."
  (lispm-desktop-platinum--derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))

(defun lispm-desktop-test--pixel (svg x y)
  "Return the colour SVG paints the pixel at X, Y with, upcased, or nil.
Only rectangles count, the last painted over the pixel winning, as the
drawing functions draw their pixels as rectangles."
  (let ((color nil))
    (dolist (node (dom-children svg))
      (when (and (consp node) (eq (dom-tag node) 'rect))
        (let ((x0 (dom-attr node 'x)) (y0 (dom-attr node 'y))
              (fill (dom-attr node 'fill)))
          (when (and (numberp x0) (stringp fill) (not (equal fill "none"))
                     (<= x0 (+ x 0.5) (+ x0 (dom-attr node 'width)))
                     (<= y0 (+ y 0.5) (+ y0 (dom-attr node 'height))))
            (setq color (upcase fill))))))
    color))

(defun lispm-desktop-test--size (svg)
  "Return the width and height of SVG as a cons."
  (cons (dom-attr svg 'width) (dom-attr svg 'height)))

(ert-deftest lispm-desktop-test-images-without-svg ()
  "An Emacs built without SVG gets the chrome's images all the same, rather
than an error: the text under them shows instead."
  (cl-letf* ((available (symbol-function 'image-type-available-p))
             ((symbol-function 'image-type-available-p)
              (lambda (type) (and (not (eq type 'svg)) (funcall available type)))))
    (let ((image (lispm-desktop-platinum--svg-image (svg-create 4 4))))
      (should (eq (car image) 'image))
      (should (eq (image-property image :type) 'svg))
      (should (string-prefix-p "<svg" (image-property image :data))))))

(ert-deftest lispm-desktop-test-grey-ramp ()
  "Platinum's greys come back as themselves; another theme's keep their order."
  (let ((platinum (lispm-desktop-test--platinum-palette))
        (dracula (lispm-desktop-test--dracula-palette)))
    (dolist (level '(#x00 #x22 #x77 #x88 #xAA #xCC #xDD #xEE #xFF))
      (should (equal (upcase (lispm-desktop-platinum-grey platinum level))
                     (format "#%02X%02X%02X" level level level))))
    (cl-flet ((lum (level) (lispm-desktop-platinum--luminance (lispm-desktop-platinum-grey dracula level))))
      (should (< (lum #x22) (lum #x77) (lum #xAA) (lum #xDD) (lum #xFF))))))

(ert-deftest lispm-desktop-test-palette-new-keys ()
  "Every theme gets the ramp of the accent, the outline and the Control
Strip's greys; Platinum names Mac OS 9's."
  (let ((dracula (lispm-desktop-test--dracula-palette))
        (platinum (lispm-desktop-test--platinum-palette)))
    (dolist (key '(:outline :highlight :highlight-dark :highlight-text :thumb :thumb-light
                   :thumb-dark :focus :alert :alert-dark :strip :strip-light :strip-shade :strip-dark))
      (should (stringp (plist-get dracula key)))
      (should (stringp (plist-get platinum key))))
    (should (equal (plist-get platinum :highlight) "#333399"))
    (should (equal (plist-get platinum :thumb) "#9999FF"))
    (should (equal (plist-get platinum :focus) "#6666CC"))
    (should (equal (plist-get platinum :strip) "#C0C0C0"))
    ;; Text on a highlighted Dracula menu stays legible.
    (should (equal (plist-get dracula :highlight-text) "#f8f8f2"))))

(ert-deftest lispm-desktop-test-push-button ()
  "A push button is drawn as Mac OS 9's: twenty pixels tall, its corners
rounded, two lit lines and two shaded ones, darkened when pressed and
flat when disabled."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (normal (lispm-desktop-platinum-draw-push-button palette "Cancel" 60 :font font))
         (pressed (lispm-desktop-platinum-draw-push-button palette "Cancel" 60 :state 'pressed :font font))
         (disabled (lispm-desktop-platinum-draw-push-button palette "Eject" 80 :state 'disabled :font font)))
    (should (equal (lispm-desktop-test--size normal) '(60 . 20)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; The corners are cut, the outline steps in.
      (should-not (at normal 0 0))
      (should (equal (at normal 2 0) "#222222"))
      (should (equal (at normal 3 0) "#000000"))
      (should (equal (at normal 1 1) "#000000"))
      (should (equal (at normal 0 10) "#000000"))
      ;; Lit, face, shaded.
      (should (equal (at normal 10 1) "#DDDDDD"))
      (should (equal (at normal 10 2) "#FFFFFF"))
      (should (equal (at normal 2 10) "#FFFFFF"))
      (should (equal (at normal 30 10) "#DDDDDD"))
      (should (equal (at normal 57 10) "#AAAAAA"))
      (should (equal (at normal 58 10) "#777777"))
      (should (equal (at normal 10 17) "#AAAAAA"))
      (should (equal (at normal 10 18) "#777777"))
      ;; Pressed: dark, lit from below.
      (should (equal (at pressed 10 1) "#444444"))
      (should (equal (at pressed 10 2) "#555555"))
      (should (equal (at pressed 30 10) "#666666"))
      (should (equal (at pressed 58 10) "#888888"))
      ;; Disabled: a grey outline round a flat face.
      (should (equal (at disabled 0 10) "#888888"))
      (should (equal (at disabled 2 10) "#DDDDDD"))
      (should (equal (at disabled 78 10) "#DDDDDD")))
    ;; Labels: black, white when pressed, dimmed when disabled.
    (should (equal (dom-attr (car (dom-by-tag normal 'text)) 'fill) "#000000"))
    (should (equal (upcase (dom-attr (car (dom-by-tag pressed 'text)) 'fill)) "#FFFFFF"))
    (should (equal (upcase (dom-attr (car (dom-by-tag disabled 'text)) 'fill)) "#777777"))))

(ert-deftest lispm-desktop-test-push-button-rings ()
  "The default button is ringed three pixels out, 26 tall in all; the
focus ring stands two pixels further, in the focus colour; a padded
button keeps its size whichever rings it wears."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (default (lispm-desktop-platinum-draw-push-button palette "Save" 60 :default t :font font))
         (plain (lispm-desktop-platinum-draw-push-button palette "Save" 60 :pad 5 :font font))
         (both (lispm-desktop-platinum-draw-push-button palette "Save" 60 :default t :focused t
                                                        :pad 5 :font font)))
    (should (equal (lispm-desktop-test--size default) '(66 . 26)))
    (should (equal (lispm-desktop-test--size plain) '(70 . 30)))
    (should (equal (lispm-desktop-test--size both) '(70 . 30)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel default x y)))
      ;; Outline, then lit and shaded bands, then the button's own outline.
      (should (equal (at 0 13) "#000000"))
      (should (equal (at 1 13) "#DDDDDD"))
      (should (equal (at 2 13) "#AAAAAA"))
      (should (equal (at 3 13) "#000000"))
      (should (equal (at 63 13) "#AAAAAA"))
      (should (equal (at 64 13) "#777777"))
      (should (equal (at 65 13) "#000000")))
    (should-not (string-search "#6666CC" (format "%S" plain)))
    (should (string-search "#6666CC" (format "%S" both)))))

(ert-deftest lispm-desktop-test-bevel-button ()
  "A bevel button is square, lit along its top and left, darker pressed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (up (lispm-desktop-platinum-draw-bevel-button palette 48 48))
         (down (lispm-desktop-platinum-draw-bevel-button palette 48 48 :state 'pressed)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at up 0 0) "#666666"))
      (should (equal (at up 2 10) "#FFFFFF"))
      (should (equal (at up 20 20) "#CCCCCC"))
      (should (equal (at up 45 20) "#999999"))
      (should (equal (at up 47 20) "#333333"))
      (should (equal (at down 0 10) "#111111"))
      (should (equal (at down 20 20) "#888888"))
      (should (equal (at down 46 20) "#BBBBBB")))))

(ert-deftest lispm-desktop-test-tabs ()
  "Tabs stand on the pane's edge: the one in front opens into the pane in
its colour, those behind are darker with the pane's outline under them."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (front (lispm-desktop-platinum-draw-tab palette "Desktop" 70 :front t))
         (back (lispm-desktop-platinum-draw-tab palette "Fonts" 50))
         (edge (lispm-desktop-platinum-draw-tab-edge palette 8)))
    (should (equal (lispm-desktop-test--size front) '(70 . 24)))
    (should (equal (lispm-desktop-test--size edge) '(8 . 24)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; Rounded top corners, slanted sides.
      (should (equal (at back 0 0) "#DDDDDD"))
      (should (equal (at back 11 0) "#000000"))
      (should (equal (at back 0 20) "#222222"))
      (should (equal (at back 6 4) "#000000"))
      ;; Faces, and the pane's outline under the tab behind only.
      (should (equal (at back 25 10) "#CCCCCC"))
      (should (equal (at front 35 10) "#EEEEEE"))
      (should (equal (at back 25 21) "#000000"))
      (should (equal (at front 35 21) "#EEEEEE"))
      (should (equal (at edge 4 21) "#000000"))
      (should (equal (at edge 4 23) "#FFFFFF")))
    ;; Every label is black and none bold.
    (should (equal (dom-attr (car (dom-by-tag front 'text)) 'fill) "#000000"))
    (should (equal (dom-attr (car (dom-by-tag back 'text)) 'fill) "#000000"))))

(ert-deftest lispm-desktop-test-disclosure-triangle ()
  "The disclosure triangle is outlined, in the variation colour, shadowed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (right (lispm-desktop-platinum-draw-triangle palette nil))
         (down (lispm-desktop-platinum-draw-triangle palette t)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at right 3 0) "#000000"))
      (should (equal (at right 4 3) "#CCCCFF"))
      (should (equal (at right 5 4) "#9999FF"))
      (should (equal (at right 7 5) "#6666CC"))
      (should (equal (at right 9 6) "#BBBBBB"))
      ;; Pointing down it is the same triangle turned over its diagonal.
      (should (equal (at down 3 4) "#CCCCFF"))
      (should (equal (at down 0 3) "#000000")))))

(ert-deftest lispm-desktop-test-small-controls ()
  "Radio buttons, check boxes and placards draw at Mac OS 9's sizes."
  (let ((palette (lispm-desktop-test--dracula-palette)))
    (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-radio palette t)) '(12 . 12)))
    (should (string-search "url(#sphere)" (format "%S" (lispm-desktop-platinum-draw-radio palette nil))))
    (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-check-box palette t)) '(14 . 14)))
    (should (dom-by-tag (lispm-desktop-platinum-draw-check-box palette t) 'polyline))
    (should-not (dom-by-tag (lispm-desktop-platinum-draw-check-box palette nil) 'polyline))
    (let ((placard (lispm-desktop-platinum-draw-placard palette 100 20 :label "9 items"
                                                        :font '(:family "Geneva" :size 10
                                                                :weight "normal" :synthetic nil))))
      (should (equal (lispm-desktop-test--size placard) '(100 . 20)))
      (should (equal (lispm-desktop-test--pixel placard 0 0)
                     (upcase (plist-get palette :outline)))))))

(ert-deftest lispm-desktop-test-window-frame ()
  "An active window's frame is outlined, lit from the top left, lined round
its content, and casts a shadow; beside the title bar it takes the bar's
lines; an inactive one lies flat in a dark grey outline."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (active (lispm-desktop-platinum-draw-window-frame palette 101 81 :active t))
         (inactive (lispm-desktop-platinum-draw-window-frame palette 101 81)))
    (should (equal (lispm-desktop-test--size active) '(101 . 81)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; Left and right of the content: outline, band, the content's line.
      (should (equal (mapcar (lambda (x) (at active x 40)) '(0 1 2 3 4 5))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000")))
      (should (equal (mapcar (lambda (x) (at active x 40)) '(94 95 96 97 98 99 100))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000" "#000000")))
      ;; The bottom, and the shadow under it.
      (should (equal (mapcar (lambda (y) (at active 50 y)) '(74 75 76 77 78 79 80))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000" "#000000")))
      ;; Beside the title bar: its lit top, its shade and its black line.
      (should (equal (at active 3 1) "#FFFFFF"))
      (should (equal (at active 3 10) "#CCCCCC"))
      (should (equal (at active 3 20) "#999999"))
      (should (equal (at active 3 21) "#000000"))
      (should (equal (at inactive 0 40) "#555555"))
      (should (equal (at inactive 2 40) "#DDDDDD")))))

(defmacro lispm-desktop-test--with-frames (frames &rest body)
  "Run BODY among fake frames: FRAMES evaluates to a list of
\(NAME . PARAMETERS), symbols standing for frames, whose parameters the frame primitives read
and set; `parent-frame', `position', `size' and `visibility' stand for a
frame's parent, place, size and visibility.  In BODY, `moves' holds the
moves made, (FRAME X Y), the latest first, and `raised' the frames
raised, the latest first."
  (declare (indent 1))
  `(let ((parameters (mapcar (lambda (frame) (cons (car frame) (copy-tree (cdr frame)))) ,frames))
         (moves nil)
         (raised nil))
     (cl-flet ((frame-get (frame name) (alist-get name (alist-get frame parameters)))
               (frame-put (frame name value) (setf (alist-get name (alist-get frame parameters)) value)))
       (cl-letf (((symbol-function 'frame-live-p) (lambda (frame) (and (assq frame parameters) t)))
                 ((symbol-function 'frame-parameter) (lambda (frame name) (frame-get frame name)))
                 ((symbol-function 'set-frame-parameter) (lambda (frame name value) (frame-put frame name value)))
                 ((symbol-function 'modify-frame-parameters)
                  (lambda (frame alist) (pcase-dolist (`(,name . ,value) alist) (frame-put frame name value))))
                 ((symbol-function 'frame-parent) (lambda (&optional frame) (frame-get frame 'parent-frame)))
                 ((symbol-function 'frame-position) (lambda (&optional frame) (frame-get frame 'position)))
                 ((symbol-function 'set-frame-position)
                  (lambda (frame x y) (push (list frame x y) moves) (frame-put frame 'position (cons x y))))
                 ((symbol-function 'frame-visible-p) (lambda (frame) (frame-get frame 'visibility)))
                 ((symbol-function 'make-frame-visible) (lambda (&optional frame) (frame-put frame 'visibility t)))
                 ((symbol-function 'make-frame-invisible)
                  (lambda (&optional frame _force) (frame-put frame 'visibility nil)))
                 ((symbol-function 'raise-frame) (lambda (&optional frame) (push frame raised)))
                 ((symbol-function 'frame-pixel-width) (lambda (&optional frame) (car (frame-get frame 'size))))
                 ((symbol-function 'frame-pixel-height) (lambda (&optional frame) (cdr (frame-get frame 'size))))
                 ((symbol-function 'frame-text-width) (lambda (&optional frame) (car (frame-get frame 'size))))
                 ((symbol-function 'frame-text-height) (lambda (&optional frame) (cdr (frame-get frame 'size))))
                 ((symbol-function 'frame-edges)
                  (lambda (&optional frame _type)
                    (pcase-let ((`(,x . ,y) (frame-get frame 'position))
                                (`(,w . ,h) (frame-get frame 'size)))
                      (list x y (+ x w) (+ y h)))))
                 ((symbol-function 'frame-list) (lambda () (mapcar #'car parameters)))
                 ((symbol-function 'frame-list-z-order)
                  (lambda (&optional _) (seq-remove (lambda (frame) (frame-get frame 'parent-frame))
                                                    (mapcar #'car parameters))))
                 ((symbol-function 'lispm-desktop-platinum--frameable-p) #'ignore)
                 ((symbol-function 'lispm-desktop-platinum--frame-update) #'ignore))
         ,@body))))

(defconst lispm-desktop-test--window-frames
  '((screen (position 0 . 0) (size 1200 . 800) (visibility . t))
    (outer (lispm-desktop-platinum-frames . window) (parent-frame . screen)
           (position 94 . 49) (size 413 . 308) (visibility . t))
    (window (lispm-desktop-window . t) (parent-frame . outer) (lispm-desktop-platinum-frame . outer)
            (position 6 . 1) (size 400 . 300) (visibility . t)))
  "A floating window in its frame over the screen, for
`lispm-desktop-test--with-frames'.")

(ert-deftest lispm-desktop-test-window-in-its-frame ()
  "A framed window stands in its frame, its parent: moved, raised, shown or
hidden, the frame is, the window going with it, as Mac OS 9 kept a
window's structure and content as one."
  (lispm-desktop-test--with-frames lispm-desktop-test--window-frames
      (should (eq (lispm-desktop-platinum-window-outer 'window) 'outer))
      (should (eq (lispm-desktop-platinum-window-outer 'screen) 'screen))
      (should (eq (lispm-desktop-platinum-window-frame-p 'outer) 'window))
      (should (lispm-desktop-platinum-chrome-frame-p 'outer))
      (should-not (lispm-desktop-apps-screen-p 'outer))
      (should (eq (lispm-desktop-platinum-window-parent 'window) 'screen))
      (should (eq (lispm-desktop-apps-window-screen 'window) 'screen))
      (should (equal (lispm-desktop-platinum-window-position 'window) '(100 . 50)))
      (lispm-desktop-platinum-move-window 'window 300 200)
      (should (equal moves '((outer 294 199))))
      (should (equal (frame-position 'window) '(6 . 1)))
      (should (equal (lispm-desktop-platinum-window-position 'window) '(300 . 200)))
      (should (equal (lispm-desktop-apps--geometry 'window) '(300 200 400 300)))
      (lispm-desktop-platinum-raise-window 'window)
      (should (equal raised '(window outer)))
      (lispm-desktop-platinum-hide-window 'window)
      (should-not (frame-visible-p 'outer))
      (should-not (frame-visible-p 'window))
      (lispm-desktop-platinum-show-window 'window)
      (should (frame-visible-p 'outer))
      (should (frame-visible-p 'window))))

(ert-deftest lispm-desktop-test-outline-geometry ()
  "Moving, a window's outline is a band two pixels thick round its outer
rectangle; growing, a line a pixel thick round it and, within, the
title bar's line, the content's lines and the grow box, where Mac OS 9's
GrowWindow drew them."
  (should (equal (lispm-desktop-platinum-outline-lines 'move 10 20 300 200)
                 '((10 20 300 2 nil) (10 218 300 2 nil) (10 22 196 2 t) (308 22 196 2 t))))
  (let ((lines (lispm-desktop-platinum-outline-lines 'grow 10 20 300 200 21)))
    (should (= (length lines) 10))
    (dolist (line '((10 20 300 1 nil) (10 219 300 1 nil) (10 21 198 1 t) (309 21 198 1 t)
                    ;; Under the title bar, from the content's left line to
                    ;; its right one, and round the content.
                    (15 41 289 1 nil) (15 42 171 1 t) (303 42 171 1 t) (15 213 289 1 nil)
                    ;; The grow box in the content's bottom right corner.
                    (288 198 15 1 nil) (288 199 14 1 t)))
      (should (member line lines)))
    (dolist (line lines)
      (pcase-let ((`(,x ,y ,length ,thickness ,vertical) line))
        (should (<= 10 x (+ x (if vertical thickness length)) 310))
        (should (<= 20 y (+ y (if vertical length thickness)) 220)))))
  (should (= 4 (length (lispm-desktop-platinum-outline-lines 'grow 10 20 300 200)))))

(ert-deftest lispm-desktop-test-outline-dots ()
  "The outline's dots are the 50% grey pattern lined up with the screen's
pixels: a checkerboard whose phase follows where the line is."
  (should (equal (lispm-desktop-platinum-dots 4 2 0)
                 (vector (bool-vector t nil t nil) (bool-vector nil t nil t))))
  (should (equal (lispm-desktop-platinum-dots 3 1 1) (vector (bool-vector nil t nil)))))

(ert-deftest lispm-desktop-test-outline-dots-image ()
  "The outline's dots are an XBM image of the bits, as large as they are."
  ;; CI's Emacs builds have no window system, and so no XBM images.
  (skip-unless (image-type-available-p 'xbm))
  ;; Bits are sized by `:data-width' and `:data-height': `:width' and
  ;; `:height' would scale the image, and make the spec invalid.
  (let ((image (lispm-desktop-platinum--dots-image 6 2 1)))
    (should (eq (plist-get (cdr image) :type) 'xbm))
    (should (equal (list (plist-get (cdr image) :data-width) (plist-get (cdr image) :data-height)) '(6 2)))
    (should-not (plist-member (cdr image) :width))))

(ert-deftest lispm-desktop-test-drag-keeps-in-front ()
  "What a drag draws comes back over whatever came in front of it, and is
left where it is while it is in front."
  (let ((order '(window outer line-a line-b))
        (raised nil))
    (cl-letf (((symbol-function 'frame-list-z-order) (lambda (&optional _) order))
              ((symbol-function 'raise-frame) (lambda (frame) (push frame raised))))
      (lispm-desktop-platinum-keep-in-front '(line-a line-b) 'screen)
      (should (equal raised '(line-b line-a)))
      (setq raised nil order '(line-b window outer line-a))
      (lispm-desktop-platinum-keep-in-front '(line-a line-b) 'screen)
      (should-not raised)
      ;; On the computer's screen, the outline floats above by itself.
      (setq order '(window line-a))
      (lispm-desktop-platinum-keep-in-front '(line-a) nil)
      (should-not raised))))

(defmacro lispm-desktop-test--with-outline (script &rest body)
  "Run BODY with the framed window of `lispm-desktop-test--window-frames'
and a mouse following SCRIPT, a list of ((X . Y) . EVENT): each read
puts the mouse at X, Y on the computer's screen and gives EVENT, nil for
a read that waited in vain.  In BODY, `ran' counts the runs of
`lispm-desktop-platinum-frame-moved-functions', `outlines' holds the
outlines drawn, the latest first, and `sizes' the sizes given the window."
  (declare (indent 1))
  `(lispm-desktop-test--with-frames lispm-desktop-test--window-frames
       (let* ((script ,script)
              (pointer (car (car script)))
              (ran 0)
              (outlines nil)
              (sizes nil)
              (unread-command-events nil)
              (lispm-desktop-platinum-frame-moved-functions (list (lambda (_) (setq ran (1+ ran))))))
         (cl-letf (((symbol-function 'mouse-absolute-pixel-position) (lambda () pointer))
                   ((symbol-function 'read-event)
                    (lambda (&rest _)
                      (pcase-let ((`(,place . ,event) (pop script)))
                        (setq pointer place)
                        event)))
                   ((symbol-function 'lispm-desktop-platinum--show-outline)
                    (lambda (_parent lines _origin) (push lines outlines)))
                   ((symbol-function 'lispm-desktop-platinum--hide-outline) #'ignore)
                   ((symbol-function 'lispm-desktop-platinum--origin) (lambda (_) '(0 . 0)))
                   ((symbol-function 'select-frame-set-input-focus) #'ignore)
                   ((symbol-function 'set-frame-size)
                    (lambda (frame width height &optional _) (push (list frame width height) sizes))))
           ,@body))))

(ert-deftest lispm-desktop-test-outline-moves-the-window-once ()
  "Dragged by its outline, a window stays where it is while the outline
follows the mouse, and moves once the button rises, by as far as the
mouse went; the moved functions run once.  Escape or C-g leaves it where
it was; a press let go where it was is handed on as a click."
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(510 . 305) nil)
                                          (cons '(530 . 320) '(mouse-movement (nil)))
                                          (cons '(560 . 340) nil)
                                          (cons '(560 . 340) '(drag-mouse-1 (nil) (nil))))
    (should (lispm-desktop-platinum-drag-outline 'window 'move))
    (should (equal moves '((outer 154 89))))
    (should (= ran 1))
    (should (= (length outlines) 3))
    (should (equal (car (car outlines)) '(154 89 413 2 nil)))
    (should (equal (car (car (last outlines))) '(104 54 413 2 nil))))
  (dolist (stop '(27 escape ?\C-g))
    (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(580 . 360) nil)
                                            (cons '(580 . 360) stop))
      (should-not (lispm-desktop-platinum-drag-outline 'window 'move))
      (should outlines)
      (should-not moves)
      (should (= ran 0))))
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(502 . 301) nil)
                                          (cons '(502 . 301) '(mouse-1 (nil))))
    (should-not (lispm-desktop-platinum-drag-outline 'window 'move))
    (should-not moves)
    (should (= ran 0))
    (should (equal unread-command-events '((mouse-1 (nil)))))))

(ert-deftest lispm-desktop-test-outline-grows-the-window-once ()
  "Grown by its outline, a window is given its new size once, when the
button rises, and grows no smaller than its least."
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(520 . 310) nil)
                                          (cons '(540 . 330) '(drag-mouse-1 (nil) (nil))))
    (should (lispm-desktop-platinum-drag-outline 'window 'grow))
    (should (equal sizes '((window 440 330))))
    (should-not moves)
    (should (= ran 1))
    (should (= (length (car outlines)) 10))
    (should (member '(94 49 453 1 nil) (car outlines))))
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil)
                                          (cons '(0 . 0) '(drag-mouse-1 (nil) (nil))))
    (lispm-desktop-platinum-drag-outline 'window 'grow)
    (should (equal sizes (list (list 'window (car lispm-desktop-platinum-window-min-size)
                                     (cdr lispm-desktop-platinum-window-min-size)))))))

(ert-deftest lispm-desktop-test-scroll-bar-parts ()
  "A scroll bar is outlined down both sides, its track recessed, its thumb
in the variation colour with four ridges, its arrows together at the
bottom; with nothing to scroll it lies flat."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (bar (lispm-desktop-platinum-draw-scroll-bar palette 200 '(40 . 60) t))
         (flat (lispm-desktop-platinum-draw-scroll-bar palette 200 nil t))
         (track (lispm-desktop-platinum--scroll-track 200)))
    (should (equal (lispm-desktop-test--size bar) '(16 . 200)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at bar 0 10) "#000000"))
      (should (equal (at bar 15 10) "#000000"))
      (should (equal (at bar 1 10) "#777777"))
      (should (equal (at bar 2 10) "#888888"))
      (should (equal (at bar 8 10) "#AAAAAA"))
      ;; The thumb, parted from the track by lines.
      (should (equal (at bar 8 39) "#000000"))
      (should (equal (at bar 8 40) "#CCCCFF"))
      (should (equal (at bar 8 45) "#9999FF"))
      (should (equal (at bar 14 45) "#6666CC"))
      (should (equal (at bar 8 100) "#000000"))
      (should (= 4 (seq-count (lambda (y) (equal (at bar 8 y) "#333399"))
                              (number-sequence 40 99))))
      ;; The arrow boxes, lit and outlined.
      (should (equal (at bar 8 track) "#000000"))
      (should (equal (at bar 8 (1+ track)) "#FFFFFF"))
      (should (equal (at bar 3 (+ track 5)) "#DDDDDD"))
      (should (equal (at bar 14 (+ track 5)) "#BBBBBB"))
      ;; Nothing to scroll: flat, grey arrows.
      (should (equal (at flat 8 10) "#EEEEEE"))
      (should (equal (at flat 8 track) "#888888")))))

(ert-deftest lispm-desktop-test-grow-box ()
  "The grow box is sixteen pixels square, outlined at top and left, ridged."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (box (lispm-desktop-platinum-draw-grow-box palette 16)))
    (should (equal (lispm-desktop-test--size box) '(16 . 16)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel box x y)))
      (should (equal (at 0 8) "#000000"))
      (should (equal (at 8 0) "#000000"))
      (should (equal (at 1 8) "#FFFFFF"))
      (should (equal (at 3 3) "#CCCCCC"))
      (should (equal (at 10 5) "#777777")))))

(ert-deftest lispm-desktop-test-header-strip ()
  "A window's header strip, under its title bar, is lit along its top,
platinum, and shaded along its bottom over a black line."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (strip (lispm-desktop-platinum-draw-header-strip palette 200 "3 items")))
    (should (equal (lispm-desktop-test--size strip) '(200 . 22)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel strip x y)))
      (should (equal (at 5 0) "#FFFFFF"))
      (should (equal (at 5 10) "#DDDDDD"))
      (should (equal (at 5 20) "#AAAAAA"))
      (should (equal (at 5 21) "#000000")))
    (should (dom-by-tag strip 'text))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-header-strip
                          (lispm-desktop-test--dracula-palette) 200 "3 items"))
                'svg))))

(ert-deftest lispm-desktop-test-group-box-edge ()
  "A group box's top edge is an engraved line, grey over lit, running down
its sides; its title breaks the line on the panel's grey."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (edge (lispm-desktop-platinum-draw-group-edge palette 200))
         (titled (lispm-desktop-platinum-draw-group-edge palette 200 "Appearance")))
    (should (equal (lispm-desktop-test--size edge) '(200 . 18)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at edge 50 9) "#888888"))
      (should (equal (at edge 50 10) "#FFFFFF"))
      (should (equal (at edge 0 14) "#888888"))
      (should (equal (at edge 1 14) "#FFFFFF"))
      (should (equal (at edge 198 14) "#888888"))
      (should (equal (at edge 199 14) "#FFFFFF"))
      (should (equal (at titled 10 9) "#EEEEEE")))
    (should (dom-by-tag titled 'text))))

(ert-deftest lispm-desktop-test-finder-path-menu ()
  "The Finder window counts its items in its header, and its title pops up
its folder's path, the folder first, each going there."
  (lispm-desktop-test--with-folder
    (unwind-protect
        (let ((root (file-name-as-directory folder)))
          (lispm-desktop-finder--go (expand-file-name "zeta" folder))
          (lispm-desktop-finder--in
            (should (eq lispm-desktop-platinum-title-menu-function #'lispm-desktop-finder--path-items))
            (should (equal header-line-format '(:eval (lispm-desktop-finder--header))))
            (let ((items (lispm-desktop-finder--path-items nil)))
              (should (equal (plist-get (car items) :label) "zeta"))
              (should (equal (plist-get (cadr items) :label) (lispm-desktop-finder--title root)))
              (funcall (plist-get (cadr items) :action)))
            (should (equal lispm-desktop-finder--directory root))))
      (kill-buffer lispm-desktop-finder-buffer-name))))

(ert-deftest lispm-desktop-test-control-strip-segments ()
  "The Control Strip's segments are 24 pixels tall in its own greys, each
parted from the next by a black line; a pressed module is darkened."
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (dolist (kind '(collapse left right module tab))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-strip-segment palette kind))
                     (cons (lispm-desktop-platinum-strip-segment-width kind) 24))))
    (let ((module (lispm-desktop-platinum-draw-strip-segment
                   palette 'module :menu t
                   :icon (expand-file-name "icons/os9/mail.svg" lispm-desktop-directory)))
          (pressed (lispm-desktop-platinum-draw-strip-segment palette 'module :state 'pressed))
          (disabled (lispm-desktop-platinum-draw-strip-segment palette 'left :state 'disabled)))
      (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
        (should (equal (at module 10 0) "#000000"))
        (should (equal (at module 0 10) "#C0C0C0"))
        (should (equal (at module 1 10) "#FFFFFF"))
        (should (equal (at module 25 10) "#A0A0A0"))
        (should (equal (at module 26 10) "#808080"))
        (should (equal (at module 27 10) "#000000"))
        ;; The menu's triangle.
        (should (equal (at module 21 11) "#000000"))
        (should (equal (at pressed 10 12) "#808080"))
        (should (equal (at pressed 0 12) "#404040"))
        (should (equal (at disabled 0 12) "#808080")))
      (when (fboundp 'libxml-parse-xml-region)
        (should (dom-by-tag module 'svg))))))

;;;; Controls on lines

(defun lispm-desktop-test--image-width (string position)
  "Return the width of the SVG image STRING displays at POSITION."
  (let* ((spec (get-text-property position 'display string))
         (data (plist-get (cdr spec) :data)))
    (and (string-match "width=\"\\([0-9]+\\)\"" data)
         (string-to-number (match-string 1 data)))))

(ert-deftest lispm-desktop-test-tab-row ()
  "A row of tabs is as wide as asked, the pane's edge running on after the
tabs, which carry their properties."
  (let* ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette))
         (map (make-sparse-keymap))
         (row (lispm-desktop-platinum-tabs `(("One" t keymap ,map help-echo "First") ("Two" nil)) 400)))
    (should (= (length row) 6))
    (should (= (apply #'+ (mapcar (lambda (i) (lispm-desktop-test--image-width row i))
                                  (number-sequence 0 5)))
               400))
    (should (eq (get-text-property 1 'keymap row) map))
    (should (equal (get-text-property 1 'help-echo row) "First"))
    ;; The tab in front and the one behind are drawn apart.
    (should-not (equal (get-text-property 1 'display row) (get-text-property 3 'display row)))))

(ert-deftest lispm-desktop-test-content-runs ()
  "A label drawn into a control keeps its runs: words in the system face,
icon glyphs in their own family and colour, images as images."
  (let* ((svg (svg-create 100 20))
         (icon (propertize "x" 'face '((:family "Symbols Nerd Font Mono") (:foreground "#D97757"))
                           'display '(raise 0.0)))
         (image (propertize " " 'display (list 'image :type 'svg :width 16 :height 16
                                               :file (expand-file-name "icons/os9/folder.svg"
                                                                       lispm-desktop-directory)))))
    (lispm-desktop-platinum--draw-content svg (concat icon " New" image) 0 0 20 "#000000"
                                          'lispm-desktop-platinum-list)
    (let ((texts (dom-by-tag svg 'text)))
      (should (seq-find (lambda (text) (string-prefix-p "Symbols Nerd Font Mono"
                                                        (dom-attr text 'font-family)))
                        texts))
      (should (seq-find (lambda (text) (equal (dom-attr text 'fill) "#D97757")) texts))
      (should (seq-find (lambda (text) (string-search "New" (dom-text text))) texts)))
    (when (fboundp 'libxml-parse-xml-region)
      (should (dom-by-tag svg 'svg)))))

(ert-deftest lispm-desktop-test-toolbar-label-keeps-image-icons ()
  "Trimming a toolbar label drops plain spaces but keeps a space showing an icon."
  (let ((icon (propertize " " 'display '(image :type png :file "x.png"))))
    (should (equal (lispm-desktop-sidebar--trim (concat " " icon "+ ")) (concat icon "+")))
    (should (get-text-property 0 'display (lispm-desktop-sidebar--trim (concat " " icon "+ "))))))

(ert-deftest lispm-desktop-test-focus-ring-follows-point ()
  "The control point is on is drawn focused; leaving it draws it plain again."
  (let ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette)))
    (with-temp-buffer
      (lispm-desktop-platinum-insert-push-button "OK" #'ignore)
      (insert " ")
      (lispm-desktop-platinum-insert-push-button "Cancel" #'ignore)
      (let ((plain (get-text-property 1 'lispm-desktop-platinum-plain))
            (focused (get-text-property 1 'lispm-desktop-platinum-focused)))
        (should-not (equal plain focused))
        (goto-char 1)
        (lispm-desktop-platinum-show-focus)
        (should (equal (get-text-property 1 'display) focused))
        (goto-char 3)
        (lispm-desktop-platinum-show-focus)
        (should (equal (get-text-property 1 'display) plain))
        (should (equal (get-text-property 3 'display) (get-text-property 3 'lispm-desktop-platinum-focused)))))))

(ert-deftest lispm-desktop-test-push-button-pushes ()
  "Return on a push button runs its action, drawn pushed in meanwhile."
  (let ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette))
        (ran 0)
        (shown nil))
    (with-temp-buffer
      (lispm-desktop-platinum-insert-push-button "OK" (lambda () (cl-incf ran)))
      (cl-letf (((symbol-function 'sit-for)
                 (lambda (&rest _) (setq shown (get-text-property 1 'display)) t))
                ((symbol-function 'redisplay) #'ignore))
        (let ((last-input-event ?\r))
          (lispm-desktop-platinum-push 1)))
      (should (= ran 1))
      (should (equal shown (get-text-property 1 'lispm-desktop-platinum-pressed)))
      (should-not (equal (get-text-property 1 'display) shown)))))

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
  "A menu is drawn as Mac OS 9's: lit and shaded edges, items sixteen pixels
tall, separators engraved, checks marked, the item under the mouse in the
variation colour, disabled items greyed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum--palette palette)
         (items '((:label "One" :action ignore :checked t)
                  separator
                  (:label "Two" :action ignore :enabled nil)
                  (:label "Three" :action ignore)
                  (:label "More" :submenu (( :label "Four" :action ignore)))))
         (svg (lispm-desktop-popup-draw-menu palette items 200 3))
         (texts (dom-by-tag svg 'text)))
    (should (equal (car (lispm-desktop-popup--rows items)) '(0 16 22 38 54)))
    (should (equal (lispm-desktop-test--size svg) '(200 . 70)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel svg x y))
              (ink (label) (dom-attr (seq-find (lambda (text) (string-search label (dom-text text))) texts)
                                     'fill)))
      (should (equal (at 50 0) "#FFFFFF"))
      (should (equal (at 0 8) "#FFFFFF"))
      (should (equal (at 199 8) "#999999"))
      (should (equal (at 50 69) "#999999"))
      ;; The separator, engraved across the whole width.
      (should (equal (at 0 18) "#888888"))
      (should (equal (at 199 19) "#FFFFFF"))
      ;; Three, under the mouse.
      (should (equal (at 50 45) "#333399"))
      (should (equal (at 0 45) "#6666CC"))
      (should (equal (at 199 45) "#000088"))
      (should (equal (upcase (ink "Three")) "#FFFFFF"))
      (should (equal (upcase (ink "Two")) "#777777"))
      ;; SVG text carries non-ASCII characters as references.
      (should (ink (format "&#%d;" ?✓)))
      ;; More's submenu triangle.
      (should (equal (at 190 61) "#000000")))
    ;; The item under a y.
    (should (= (lispm-desktop-popup--index-at items 40) 3))
    (should-not (lispm-desktop-popup--index-at items 18))
    (should-not (lispm-desktop-popup--index-at items 25))
    ;; Moving down from One skips the separator and the disabled Two.
    (should (= (lispm-desktop-popup--step items 0 1) 3))
    (should (= (lispm-desktop-popup--step items 3 1) 4))
    ;; A dark theme greys disabled items in its own dim colour.
    (let* ((dracula (lispm-desktop-test--dracula-palette))
           (dark (lispm-desktop-popup-draw-menu dracula items 200 nil)))
      (should (equal (dom-attr (seq-find (lambda (text) (string-search "Two" (dom-text text)))
                                         (dom-by-tag dark 'text))
                               'fill)
                     (plist-get dracula :dim))))))

(ert-deftest lispm-desktop-test-small-menu ()
  "A small menu, as the Control Strip's, has items 13 pixels tall in the
small system font, labels 15 pixels in."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum--palette palette)
         (lispm-desktop-popup-small t)
         (items '((:label "One" :action ignore) (:label "Two" :action ignore)))
         (svg (lispm-desktop-popup-draw-menu palette items 100 1)))
    (should (equal (lispm-desktop-test--size svg) '(100 . 26)))
    (should (equal (lispm-desktop-test--pixel svg 50 20) "#333399"))
    (should (= (lispm-desktop-popup--menu-width items)
               (+ 15 (lispm-desktop-platinum--content-width "Two" 'lispm-desktop-platinum-small) 11)))
    (should (seq-every-p (lambda (text) (= (dom-attr text 'x) 15))
                         (seq-remove (lambda (text) (string-search "&#" (dom-text text)))
                                     (dom-by-tag svg 'text))))))

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

(ert-deftest lispm-desktop-test-popup-holds-size ()
  "A popup frame keeps the size it was given as its font changes, when
Emacs sizes a frame anew to whole lines and columns of the new font, no
less than its window's minimum: the popup's buffer asks its window for a
line and a column, not four lines and ten columns, and has its frame
sized back."
  (with-temp-buffer
    (setq-local mode-line-format nil)
    (lispm-desktop-popup--hold-size)
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (should (= (window-min-size nil nil nil t) (frame-char-height)))
      (should (= (window-min-size nil t nil t) (frame-char-width)))))
  (let ((frame (selected-frame))
        (text '(80 . 25))
        (sized nil))
    (cl-letf (((symbol-function 'set-frame-size)
               (lambda (_frame width height &optional _pixelwise)
                 (push (cons width height) sized)
                 (setq text (cons width height))))
              ((symbol-function 'frame-text-width) (lambda (&optional _frame) (car text)))
              ((symbol-function 'frame-text-height) (lambda (&optional _frame) (cdr text))))
      (unwind-protect
          (progn
            ;; A frame `lispm-desktop-popup--size' never sized is left alone.
            (lispm-desktop-popup--keep-size (selected-window))
            (should-not sized)
            (lispm-desktop-popup--size frame 256 24)
            (should (equal sized '((256 . 24))))
            ;; A larger font: Emacs made it whole columns, and four lines.
            (setq text '(288 . 76) sized nil)
            (lispm-desktop-popup--keep-size (selected-window))
            (should (equal sized '((256 . 24))))
            (setq sized nil)
            (lispm-desktop-popup--keep-size (selected-window))
            (should-not sized))
        (set-frame-parameter frame 'lispm-desktop-popup-size nil)))))

(ert-deftest lispm-desktop-test-alert-text ()
  "An alert shows its message, its detail and a button per choice, framed
as Mac OS 9 framed its alerts."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum--palette palette)
         (buttons '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default)))
         (svg (lispm-desktop-popup-draw-alert palette 'caution "Save?" "Changes will be lost."
                                              buttons nil nil))
         (texts (mapcar #'dom-text (dom-by-tag svg 'text))))
    (should (seq-find (lambda (text) (string-search "Save?" text)) texts))
    (should (seq-find (lambda (text) (string-search "Changes will be lost." text)) texts))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at 0 30) "#FF9999"))
      (should (equal (at 1 30) "#FFFFFF"))
      (should (equal (at 30 1) "#FFFFFF"))
      (should (equal (at 50 40) "#DDDDDD"))
      (should (equal (at (- lispm-desktop-popup-alert-width 2) 30) "#999999"))
      (should (equal (at (- lispm-desktop-popup-alert-width 1) 30) "#FF6666")))))

(ert-deftest lispm-desktop-test-alert-layout ()
  "Don't Save stands apart under the message; Cancel and the default button
stand at the right, ten pixels apart, the default's ring twelve pixels
from the edge."
  (let* ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette))
         (layout (lispm-desktop-popup--alert-layout
                  "Save changes to the document before closing?" nil
                  '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default))))
         (buttons (plist-get layout :buttons))
         (w lispm-desktop-popup-alert-width))
    (should (equal (mapcar #'cadr buttons) '(discard cancel save)))
    (pcase-let ((`(,_ ,_ ,_ ,dont ,_ ,_) (nth 0 buttons))
                (`(,_ ,_ ,_ ,cancel ,_ ,cancel-width) (nth 1 buttons))
                (`(,_ ,_ ,_ ,save ,_ ,save-width) (nth 2 buttons)))
      (should (= dont (- lispm-desktop-popup-alert-text-x 4)))
      (should (= (+ save save-width 3) (- w 12)))
      (should (= (- (- save 3) (+ cancel cancel-width)) lispm-desktop-popup-button-gap)))
    (cl-letf (((symbol-function 'lispm-desktop-popup--over)
               (lambda (event _frame) (event-start event)))
              ((symbol-function 'frame-parameter) (lambda (&rest _) buttons)))
      (pcase-let ((`(,_ ,_ ,_ ,x ,y ,_) (nth 1 buttons)))
        (should (eq (lispm-desktop-popup--button-at
                     (list 'mouse-1 (list (selected-window) 1 (cons (+ x 5) (+ y 5)) 0))
                     nil)
                    'cancel))))))

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

(ert-deftest lispm-desktop-test-roll-up ()
  "A floating window rolls up to its title bar alone, as Mac OS 9's
WindowShade rolled it: no minimum height of Emacs's holds it taller, and
no scroll function runs on it while none of its text is in sight.  It
rolls down to the height it is given, its buffer as it was."
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (setq-local tab-line-format "Title" header-line-format nil mode-line-format nil)
      (let ((frame (selected-frame))
            (window (selected-window))
            (heights nil))
        (set-window-buffer window (current-buffer))
        (let ((title (window-tab-line-height window))
              (minimum (window-min-size window nil nil t))
              (unrolled (frame-text-height frame)))
          (should (> minimum title))
          (cl-letf (((symbol-function 'set-frame-height)
                     (lambda (_frame height &optional _pretend pixelwise)
                       (should pixelwise)
                       (push height heights))))
            (lispm-desktop-platinum-roll-up frame title)
            (should (= (window-min-size window nil nil t) title))
            ;; The title bar, unless a line of the font is as tall: Emacs
            ;; shows a tab line only in a window taller than a line.
            (should (= (car heights) (max title (1+ (frame-char-height frame)))))
            (should (= (lispm-desktop-platinum-rolled-up-height frame) unrolled))
            (should (local-variable-p 'window-scroll-functions))
            (should-not window-scroll-functions)
            (lispm-desktop-platinum-roll-down frame 300)
            (should (= (car heights) 300))
            (should (= (window-min-size window nil nil t) minimum))
            (should-not (local-variable-p 'window-scroll-functions))
            (should-not (lispm-desktop-platinum-rolled-up-height frame))))))))

(ert-deftest lispm-desktop-test-rolled-up-window-kept-on-desktop ()
  "A window rolled up keeps to its title bar when put back on the desktop,
as after it was moved or its screen changed size, and is remembered at
the height it rolls down to."
  (let* ((frame (selected-frame))
         (rolled (frame-text-height frame))
         (sizes nil))
    (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _) frame))
              ((symbol-function 'set-frame-size)
               (lambda (_frame width height &optional _pixelwise) (push (cons width height) sizes)))
              ((symbol-function 'set-frame-position) #'ignore)
              ;; CI's Emacs builds have no window system, and so no tab bar.
              ((symbol-function 'tab-bar-height) (lambda (&rest _) 0)))
      (unwind-protect
          (progn
            (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up (cons (current-buffer) 300))
            (should (= (nth 3 (lispm-desktop-apps--geometry frame)) 300))
            (lispm-desktop-apps--put frame (list 0 0 (frame-text-width frame) 300))
            (should (seq-every-p (lambda (size) (= (cdr size) rolled)) sizes))
            (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)
            (should (= (nth 3 (lispm-desktop-apps--geometry frame)) rolled))
            (lispm-desktop-apps--put frame (list 0 0 (frame-text-width frame) 300))
            (should (/= (cdar sizes) rolled)))
        (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)))))

(ert-deftest lispm-desktop-test-status-drawings ()
  "The battery and the load meter, the Control Strip's module icons, are
sixteen pixels square and draw at any level."
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (dolist (percent '(0 5 50 100))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-battery palette percent t))
                     '(16 . 16)))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-battery palette percent nil)) 'svg)))
    (should (equal (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-battery palette 100 nil) 5 7)
                   "#000000"))
    (should (equal (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-battery palette 0 nil) 5 7)
                   "#C0C0C0"))
    (dolist (fraction '(0.0 0.5 1.5))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-meter palette fraction))
                     '(16 . 16))))
    (cl-flet ((bar (fraction x) (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-meter palette fraction)
                                                           x 13)))
      (should (equal (bar 0.5 2) "#000000"))
      (should (equal (bar 0.5 12) "#808080")))))

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

(ert-deftest lispm-desktop-test-menu-bar-lines ()
  "The menu bar is drawn: a lit top line, platinum, a shaded line and a black
line, the screen's corners rounded over its ends, an open menu's title
in the variation colour; the bar runs from edge to edge."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (plain (lispm-desktop-platinum-draw-menu-title palette 40 :corner 'left))
         (open (lispm-desktop-platinum-draw-menu-title palette 40 :highlighted t :corner 'right)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (mapcar (lambda (y) (at plain 20 y)) '(0 1 10 18 19))
                     '("#FFFFFF" "#DDDDDD" "#DDDDDD" "#999999" "#000000")))
      (should (equal (at plain 0 0) "#000000"))
      (should (equal (at plain 0 10) "#FFFFFF"))
      (should (equal (at open 39 0) "#000000"))
      (should (equal (mapcar (lambda (y) (at open 20 y)) '(0 1 10 18 19))
                     '("#6666CC" "#333399" "#333399" "#000088" "#000000")))))
  (let ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette))
        (lispm-desktop-platinum-menus-function nil))
    (cl-letf (((symbol-function 'lispm-desktop-platinum-menu-status) #'ignore)
              ((symbol-function 'frame-inner-width) (lambda (&rest _) 900)))
      ;; The bar's own stretch takes what its titles leave.
      (let* ((items (lispm-desktop-platinum-menu-bar-line))
             (filler (assq 'lispm-desktop-menu-filler items))
             (others (remq filler items)))
        (should (= (lispm-desktop-test--image-width (nth 2 filler) 0)
                   (- 900 (apply #'+ (mapcar (lambda (item) (string-pixel-width (nth 2 item)))
                                             others)))))))))

(ert-deftest lispm-desktop-test-menu-status ()
  "The menu bar keeps other status text and the time, as Mac OS 9's held
its clock alone; the load, the mail and the battery are in the Control
Strip."
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
      (setq labels (mapcar (lambda (item)
                             (substring-no-properties
                              (get-text-property 0 'lispm-desktop-platinum-label (nth 2 item))))
                           (lispm-desktop-platinum-menu-status))))
    (progn
      (should (string-search "λ 2" (nth 0 labels)))
      (should-not (seq-some (lambda (label) (string-search "9:41AM" label)) labels))
      (should (= (length labels) 2))))
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
        (let ((text (concat (buffer-string)
                            ;; Push buttons draw their labels in their images.
                            (mapconcat #'identity
                                       (delq nil (mapcar (lambda (position)
                                                           (get-text-property position 'lispm-desktop-platinum-label))
                                                         (number-sequence (point-min) (1- (point-max)))))
                                       " "))))
          (dolist (label labels)
            (should (string-search label text))))
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

(ert-deftest lispm-desktop-test-settings-close-rolls-down ()
  "The control panel's window closed rolled up rolls down first, as the
Finder window does: it opens again with its tabs, as tall as it was."
  (let ((lispm-desktop-settings--frame (selected-frame))
        (lispm-desktop-settings--shade 300)
        (lispm-desktop-settings--geometry nil)
        (rolled nil))
    (cl-letf (((symbol-function 'lispm-desktop-settings--collapse-box)
               (lambda (_window) (setq rolled t lispm-desktop-settings--shade nil)))
              ((symbol-function 'lispm-desktop-apps--geometry)
               (lambda (_frame) (should rolled) '(0 0 400 300)))
              ((symbol-function 'lispm-desktop-apps-focus-after) #'ignore)
              ((symbol-function 'delete-frame) #'ignore)
              ((symbol-function 'lispm-desktop-apps-hidden-p) #'always))
      (lispm-desktop-settings-close)
      (should rolled)
      (should (equal lispm-desktop-settings--geometry '(0 0 400 300))))))

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

(defvar exwm-wm-mode)

(ert-deftest lispm-desktop-test-coherence-inside-fraction ()
  "How much of a window lies over the desktop decides where it belongs."
  (should (= 1.0 (lispm-desktop-coherence-inside-fraction '(10 10 100 100) '(0 0 1000 800))))
  (should (= 0.25 (lispm-desktop-coherence-inside-fraction '(950 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(1200 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(0 0 0 100) '(0 0 1000 800)))))

(defmacro lispm-desktop-test--with-drop (spec &rest body)
  "Run BODY with a window dropped as SPEC says: :child, a child of the
screen rather than out; :inside, its fraction over the screen; :hidden,
the screen hidden; :full, the screen full screen; :drag, windows going
out when dropped, t when left out.  `out' and `in' are bound to whether
the window was taken out or brought in."
  (declare (indent 1))
  `(let ((out nil) (in nil)
         (lispm-desktop-coherence-drag-out ,(if (plist-member spec :drag) (plist-get spec :drag) t)))
     (cl-letf (((symbol-function 'lispm-desktop-coherence-available-p) #'always)
               ((symbol-function 'frame-parameter)
                (lambda (_frame parameter) (eq parameter 'lispm-desktop-window)))
               ((symbol-function 'frame-parent) (lambda (_) (and ,(plist-get spec :child) 'screen)))
               ((symbol-function 'lispm-desktop-apps-window-screen) (lambda (_) 'screen))
               ((symbol-function 'lispm-desktop-apps-hidden-p) (lambda (&rest _) ,(plist-get spec :hidden)))
               ((symbol-function 'lispm-desktop-coherence--full-screen-p) (lambda (_) ,(plist-get spec :full)))
               ((symbol-function 'frame-live-p) #'always)
               ((symbol-function 'lispm-desktop-coherence--outer-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence--screen-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence-inside-fraction)
                (lambda (&rest _) ,(plist-get spec :inside)))
               ((symbol-function 'lispm-desktop-coherence-take-out) (lambda (_) (setq out t)))
               ((symbol-function 'lispm-desktop-coherence-bring-in) (lambda (_) (setq in t))))
       (lispm-desktop-coherence--dropped 'window)
       ,@body)))

(ert-deftest lispm-desktop-test-coherence-drop-out-and-in ()
  "A window dropped mostly past the desktop's edge goes out; one out dropped
mostly over the desktop shown comes in; nothing else moves a window."
  (lispm-desktop-test--with-drop (:child t :inside 0.2) (should out) (should-not in))
  (lispm-desktop-test--with-drop (:child t :inside 0.8) (should-not out) (should-not in))
  (lispm-desktop-test--with-drop (:inside 0.8) (should in) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :hidden t) (should-not in) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.2) (should-not in) (should-not out)))

(ert-deftest lispm-desktop-test-coherence-drop-rules ()
  "Full screen, the desktop moves no window out or in; with dragging out
turned off, a window dropped past its edge stays, and one out still
comes back in."
  (lispm-desktop-test--with-drop (:child t :inside 0.2 :full t) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :full t) (should-not in))
  (lispm-desktop-test--with-drop (:child t :inside 0.2 :drag nil) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :drag nil) (should in)))

(ert-deftest lispm-desktop-test-coherence-unsupported ()
  "Windows go out on macOS only; elsewhere the desktop says why it cannot
hide, and Hide Desktop is not offered."
  (should (lispm-desktop-coherence-unsupported (selected-frame)))
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'always))
      (cl-letf (((symbol-function 'framep) (lambda (_) 'ns)))
        (should-not (lispm-desktop-coherence-unsupported 'screen)))
      (cl-letf (((symbol-function 'framep) (lambda (_) 'x)))
        (should (string-match-p "macOS" (lispm-desktop-coherence-unsupported 'screen)))
        (let ((exwm-wm-mode t))
          (ignore exwm-wm-mode)
          (should (string-match-p "manages" (lispm-desktop-coherence-unsupported 'screen))))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (should (string-match-p "terminal" (lispm-desktop-coherence-unsupported 'screen)))))
  (should-error (lispm-desktop-hide-desktop) :type 'user-error)
  (cl-letf (((symbol-function 'lispm-desktop-xwidget-available-p) #'ignore))
    (should-not (member "Hide Desktop" (mapcar (lambda (item) (and (consp item) (plist-get item :label)))
                                             (lispm-desktop-apps-application-items))))))

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
            ((symbol-function 'frame-text-height) (lambda (_) 400))
            ((symbol-function 'lispm-desktop-platinum-rolled-up-height) #'ignore))
    (should (equal (lispm-desktop-apps--geometry 'window) '(460 200 700 400)))))

(ert-deftest lispm-desktop-test-coherence-keep-on-screen ()
  "A window out keeps its title bar under the menu bar and part of it on the
computer's screen."
  (let ((moved nil))
    (cl-letf (((symbol-function 'lispm-desktop-apps--workarea) (lambda (_) '(0 25 1728 1000)))
              ((symbol-function 'lispm-desktop-platinum-window-outer) #'identity)
              ((symbol-function 'frame-position) (lambda (_) '(2000 . 5)))
              ((symbol-function 'frame-pixel-width) (lambda (_) 700))
              ((symbol-function 'set-frame-position) (lambda (_ x y) (setq moved (list x y)))))
      (lispm-desktop-apps--keep-on-screen 'window)
      (should (equal moved '(1648 25))))))

(ert-deftest lispm-desktop-test-coherence-window-out-keeps-its-frame ()
  "A window taken out goes out in its frame: the frame carrying it becomes a
frame of its own where it was, the window still standing in it, framed;
brought back in, the frame is a child of the screen again where it is."
  (lispm-desktop-test--with-frames lispm-desktop-test--window-frames
      (cl-letf (((symbol-function 'lispm-desktop-coherence--minibuffer) (lambda (_) 'minibuffer))
                ((symbol-function 'minibuffer-window) (lambda (&optional frame) (list 'mini frame)))
                ((symbol-function 'lispm-desktop-apps--screen-origin) (lambda (_) '(40 . 100)))
                ((symbol-function 'lispm-desktop-apps--keep-on-screen) #'ignore)
                ((symbol-function 'lispm-desktop-apps-keep-on-desktop) #'ignore))
        (lispm-desktop-coherence-take-out 'window)
        (should-not (frame-parent 'outer))
        (should (eq (frame-parent 'window) 'outer))
        (should (lispm-desktop-platinum--framed-p 'window))
        (should (equal (frame-position 'outer) '(134 . 149)))
        (should (equal (frame-position 'window) '(6 . 1)))
        (should (eq (lispm-desktop-apps-out-p 'window) 'screen))
        (should (eq (lispm-desktop-apps-out-p 'outer) 'screen))
        (should-not (lispm-desktop-apps-screen-p 'outer))
        (should (eq (lispm-desktop-apps-window-screen 'window) 'screen))
        (should-not (lispm-desktop-platinum-window-parent 'window))
        (should (equal (frame-parameter 'window 'minibuffer) '(mini minibuffer)))
        (should (equal (frame-parameter 'outer 'minibuffer) '(mini minibuffer)))
        (should (equal (lispm-desktop-apps-windows-out-in-order 'screen) '(window)))
        (lispm-desktop-coherence-bring-in 'window)
        (should (eq (frame-parent 'outer) 'screen))
        (should (eq (frame-parent 'window) 'outer))
        (should (equal (frame-position 'outer) '(94 . 49)))
        (should-not (lispm-desktop-apps-out-p 'window))
        (should-not (lispm-desktop-apps-out-p 'outer))
        (should (equal (frame-parameter 'window 'minibuffer) '(mini screen))))))

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

(ert-deftest lispm-desktop-test-term-colour-scheme ()
  "A program asking whether the background is dark (DSR 996) is answered,
and one that asked to hear of it (DEC mode 2031) is told when the colours
Emacs gives turn the background from light to dark, and only then."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-scheme" "/bin/sh"
             '("-c" "read go; stty -icanon -echo min 0 time 30; printf '\\033[?996n\\033[?2031h'; a=$(dd bs=64 count=1 2>/dev/null); echo ready; b=$(dd bs=64 count=1 2>/dev/null); printf '[%s][%s]\\n' \"$a\" \"$b\" | tr '\\033' E")))
    (with-current-buffer term
      (let ((colours (lambda (background)
                       (lispm-desktop-term-module-set-colors
                        lispm-desktop-term--terminal
                        (vconcat (make-vector 16 "#808080") (list "#000000" background)))))
            (text (lambda ()
                    (string-replace "\n" "" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))
        (funcall colours "#ffffff")
        (lispm-desktop-term-send-string "\r")
        (should (lispm-desktop-test--term-until term (lambda () (string-match-p "ready" (funcall text)))))
        (funcall colours "#e8e8e8")
        (funcall colours "#1e1e1e")
        (lispm-desktop-test--term-wait term)
        (should (string-match-p (regexp-quote "[E[?997;2n][E[?997;1n]") (funcall text)))))))

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
         (bar (lispm-desktop-platinum--draw-title-bar 600 t nil "codex" nil '(on on nil) nil
                                                      (propertize "needs approval" 'face '(:foreground "#CE3131"))))
         (title (seq-find (lambda (position)
                            (equal (get-text-property position 'keymap bar) lispm-desktop-platinum--title-map))
                          (number-sequence 0 (1- (length bar)))))
         (data (plist-get (cdr (get-text-property title 'display bar)) :data))
         (svg (with-temp-buffer (insert data) (libxml-parse-xml-region (point-min) (point-max))))
         (texts (dom-by-tag svg 'text)))
    ;; The title is drawn in the bar, the status after it in its own colour.
    (should (seq-find (lambda (text) (string-search "codex" (dom-text text))) texts))
    (let ((status (seq-find (lambda (text) (string-search "needs approval" (dom-text text))) texts)))
      (should status)
      (should (equal (dom-attr status 'fill) "#CE3131")))))

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

(defvar xwidget-list)

(ert-deftest lispm-desktop-test-browser-keeps-script-callbacks ()
  "A script's callback is kept from the garbage collector until WebKit
calls it, and let go then; a script sent without one keeps nothing.  A
symbol stands in for the xwidget, which batch Emacs cannot make, and the
primitive handing scripts to WebKit is stubbed."
  (let ((lispm-desktop-browser--script-callbacks nil)
        (xwidget (make-symbol "xwidget"))
        (sent nil)
        (answers nil))
    (cl-letf (((symbol-function 'xwidget-webkit-execute-script)
               (lambda (xwidget script &optional fun) (push (list xwidget script fun) sent))))
      (lispm-desktop-browser--run-script xwidget "1 + 1")
      (should (equal sent `((,xwidget "1 + 1" nil))))
      (should-not lispm-desktop-browser--script-callbacks)
      (lispm-desktop-browser--run-script xwidget "document.title" (lambda (value) (push value answers)))
      (let ((answered (nth 2 (car sent))))
        (should (functionp answered))
        (should (equal (mapcar #'car lispm-desktop-browser--script-callbacks) (list answered)))
        (should (eq (nth 1 (car lispm-desktop-browser--script-callbacks)) xwidget))
        (funcall answered "Title")
        (should (equal answers '("Title")))
        (should-not lispm-desktop-browser--script-callbacks)))))

(ert-deftest lispm-desktop-test-browser-prunes-script-callbacks ()
  "Callbacks WebKit never calls are let go as scripts are sent: once a
minute old, those of closed pages all, those of open pages but the
newest few.  Younger ones are all kept, a closed page's too."
  (let* ((open (make-symbol "open"))
         (closed (make-symbol "closed"))
         (xwidget-list (list open))
         (now (float-time))
         (old (- now lispm-desktop-browser--script-callback-age 1))
         (young-closed (list 'young-closed closed now))
         (old-open-1 (list 'old-open-1 open old))
         (old-closed (list 'old-closed closed old))
         (old-open-2 (list 'old-open-2 open old))
         (old-open-3 (list 'old-open-3 open old))
         (lispm-desktop-browser--script-callbacks
          (list young-closed old-open-1 old-closed old-open-2 old-open-3))
         (lispm-desktop-browser--script-callbacks-kept 2))
    (cl-letf (((symbol-function 'xwidget-webkit-execute-script) #'ignore))
      (lispm-desktop-browser--run-script open "1 + 1" #'ignore))
    (should (equal (cdr lispm-desktop-browser--script-callbacks)
                   (list young-closed old-open-1 old-open-2)))
    (should (eq (nth 1 (car lispm-desktop-browser--script-callbacks)) open))))

(ert-deftest lispm-desktop-test-browser-script-callers-keep-callbacks ()
  "The favicon lookup and the hints send their scripts through
`lispm-desktop-browser--run-script', so the lambdas they pass are kept."
  (require 'lispm-desktop-browser-hints)
  (let ((lispm-desktop-browser--script-callbacks nil)
        (xwidget (make-symbol "xwidget"))
        (sent nil))
    (cl-letf (((symbol-function 'xwidget-webkit-execute-script)
               (lambda (_xwidget _script &optional fun) (push fun sent)))
              ((symbol-function 'xwidget-at) (lambda (_pos) xwidget))
              ((symbol-function 'lispm-desktop-browser--page-xwidget) (lambda () xwidget)))
      (with-temp-buffer
        (lispm-desktop-browser--look-up-favicon (current-buffer) "https://example.org/"))
      (lispm-desktop-browser-first-field)
      (should (= (length sent) 2))
      (should (seq-every-p #'functionp sent))
      (should (equal (mapcar #'car lispm-desktop-browser--script-callbacks) sent)))))

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

(defvar xwidget-list)

(ert-deftest lispm-desktop-test-browser-drops-events-of-killed-xwidgets ()
  "An xwidget event reaches `xwidget-event-handler' only while its xwidget
is in `xwidget-list'; other events pass.  Symbols stand in for xwidgets,
which batch Emacs cannot make: the guard only compares them."
  (let* ((live (make-symbol "live"))
         (killed (make-symbol "killed"))
         (xwidget-list (list live)))
    (should (lispm-desktop-browser--xwidget-event-live-p
             `(xwidget-event load-changed ,live "load-finished")))
    (should-not (lispm-desktop-browser--xwidget-event-live-p
                 `(xwidget-event load-changed ,killed "load-finished")))
    (should-not (lispm-desktop-browser--xwidget-event-live-p
                 `(xwidget-event javascript-callback ,killed ignore "answer")))
    (let ((last-input-event `(xwidget-event load-changed ,killed "load-started")))
      (should-not (lispm-desktop-browser--xwidget-event-live-p)))
    (should (lispm-desktop-browser--xwidget-event-live-p ?a))))

(ert-deftest lispm-desktop-test-browser-keeps-killed-xwidgets ()
  "A killed buffer's xwidgets are kept from the garbage collector until a
timer lets them go, and a buffer without xwidgets keeps nothing."
  (let ((lispm-desktop-browser--killed-xwidgets nil)
        (xwidget (make-symbol "xwidget"))
        (xwidgets nil)
        (timers nil))
    (cl-letf (((symbol-function 'get-buffer-xwidgets) (lambda (_) xwidgets))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args) (push (cons function args) timers))))
      (with-temp-buffer (lispm-desktop-browser--keep-killed-xwidgets))
      (should-not lispm-desktop-browser--killed-xwidgets)
      (should-not timers)
      (setq xwidgets (list xwidget))
      (with-temp-buffer (lispm-desktop-browser--keep-killed-xwidgets))
      (should (equal lispm-desktop-browser--killed-xwidgets (list xwidget)))
      (should (= (length timers) 1))
      (apply (caar timers) (cdar timers))
      (should-not lispm-desktop-browser--killed-xwidgets))))

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

;;;; Control Strip

(defmacro lispm-desktop-test--with-strip (&rest body)
  "Run BODY with the Control Strip's state fresh, every module shown, and
nothing saved or drawn."
  (declare (indent 0))
  `(let ((lispm-desktop-strip-modules '(appearance desktop keyboard internet battery load mail))
         (lispm-desktop-strip-collapsed nil)
         (lispm-desktop-strip-edge 'left)
         (lispm-desktop-strip-raised 0)
         (lispm-desktop-strip-shown t)
         (lispm-desktop-strip-width nil)
         (lispm-desktop-strip--first 0)
         (lispm-desktop-strip--pressed nil)
         (lispm-desktop-strip--focus nil)
         (saved nil))
     (ignore saved)
     (cl-letf (((symbol-function 'lispm-desktop-settings--browser-p) #'always)
               ((symbol-function 'lispm-desktop-platinum-battery-percent) (lambda () 80))
               ((symbol-function 'lispm-desktop-strip--time-p) #'always)
               ((symbol-function 'lispm-desktop-strip--render) #'ignore)
               ((symbol-function 'lispm-desktop-settings--save)
                (lambda (variable value) (push (cons variable value) saved) (set variable value))))
       ,@body)))

(ert-deftest lispm-desktop-test-strip-parts ()
  "The strip runs collapse box, left arrow, modules, right arrow and tab; the
tab drags how many modules show, the arrows scroll to the rest; collapsed,
the tab alone is left."
  (lispm-desktop-test--with-strip
    (should (equal (lispm-desktop-strip--parts)
                   '(collapse left (module . appearance) (module . desktop) (module . keyboard)
                              (module . internet) (module . battery) (module . load) (module . mail)
                              right tab)))
    (should (= (lispm-desktop-strip--width) (+ 15 14 (* 7 28) 14 17)))
    (should (eq (lispm-desktop-strip--state 'left) 'disabled))
    (should (eq (lispm-desktop-strip--state 'right) 'disabled))
    (setq lispm-desktop-strip-width 3)
    (lispm-desktop-strip-scroll 1)
    (should (equal (seq-filter #'consp (lispm-desktop-strip--parts))
                   '((module . desktop) (module . keyboard) (module . internet))))
    (should-not (lispm-desktop-strip--state 'left))
    (should-not (lispm-desktop-strip--state 'right))
    (should (= (lispm-desktop-strip--x '(module . keyboard)) (+ 15 14 28)))
    (lispm-desktop-strip-scroll 10)
    (should (equal (car (seq-filter #'consp (lispm-desktop-strip--parts))) '(module . battery)))
    (should (eq (lispm-desktop-strip--state 'right) 'disabled))
    (lispm-desktop-strip-collapse)
    (should (equal saved '((lispm-desktop-strip-collapsed . t))))
    (should (equal (lispm-desktop-strip--parts) '(tab)))
    (lispm-desktop-strip-collapse t)
    (should-not lispm-desktop-strip-collapsed)))

(ert-deftest lispm-desktop-test-strip-modules ()
  "Each module has a sixteen-pixel icon, its help, and a menu ending in its
panel where it has one; modules that cannot work are left out."
  (require 'battery)
  (lispm-desktop-test--with-strip
    (let ((lispm-desktop-platinum--palette (lispm-desktop-test--platinum-palette))
          (lispm-desktop-theme 'platinum)
          (lispm-desktop-platinum--battery '((?p . "80") (?L . "AC") (?t . "N/A")))
          (lispm-desktop-platinum--load "1.25")
          (battery-echo-area-format "Power %L, battery %p%%"))
      (dolist (key (lispm-desktop-strip--modules))
        (let ((icon (lispm-desktop-strip--icon key)))
          (should (if (stringp icon) (file-readable-p icon) (equal (lispm-desktop-test--size icon) '(16 . 16)))))
        (should (stringp (lispm-desktop-strip--help (cons 'module key))))
        (should (lispm-desktop-strip--items key)))
      (pcase-dolist (`(,key . ,panel) '((appearance . "Open Appearance…") (desktop . "Open Desktop…")
                                        (keyboard . "Open Keyboard…") (internet . "Open Internet…")))
        (should (equal (plist-get (car (last (lispm-desktop-strip--items key))) :label) panel)))
      (should (plist-get (seq-find (lambda (item) (equal (plist-get item :label) "Platinum"))
                                   (lispm-desktop-strip--items 'appearance))
                         :checked)))
    (cl-letf (((symbol-function 'lispm-desktop-settings--browser-p) #'ignore)
              ((symbol-function 'lispm-desktop-platinum-battery-percent) #'ignore))
      (should (equal (lispm-desktop-strip--modules) '(appearance desktop keyboard load mail))))))

(ert-deftest lispm-desktop-test-strip-segment-focus ()
  "A module the keyboard is on is ringed in the focus colour; a drawn icon
is embedded as it is; the tab's end and the triangles stand out on any
theme."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (icon (lispm-desktop-platinum-draw-meter palette 1.0))
         (module (lispm-desktop-platinum-draw-strip-segment palette 'module :icon icon :focused t)))
    (should (equal (lispm-desktop-test--pixel module 0 10) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 1 10) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 13 1) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 27 10) "#000000"))
    (should (= (length (dom-by-tag module 'svg)) 2)))
  ;; The tab's pointed end is set off against what the strip lies on.
  (let ((tab (lispm-desktop-platinum-draw-strip-segment (lispm-desktop-test--platinum-palette) 'tab
                                                        :behind "#6666AA")))
    (should (equal (lispm-desktop-test--pixel tab 16 0) "#6666AA"))
    (should (equal (lispm-desktop-test--pixel tab 0 10) "#000000")))
  ;; Its triangle stands out on a dark theme's greys too.
  (let ((dracula (lispm-desktop-test--dracula-palette)))
    (should (equal (lispm-desktop-test--pixel
                    (lispm-desktop-platinum-draw-strip-segment dracula 'module :menu t) 21 11)
                   (upcase (plist-get dracula :text))))))

(ert-deftest lispm-desktop-test-strip-holds-size ()
  "The strip's frame keeps the strip's size as the font changes, where it
grew to the four lines Emacs keeps a window to, over the echo area."
  (when-let* ((old (get-buffer lispm-desktop-strip-buffer-name)))
    (kill-buffer old))
  (let ((buffer (lispm-desktop-strip--buffer)))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer nil buffer)
          (should (= (window-min-size nil nil nil t) (frame-char-height)))
          (with-current-buffer buffer
            (should (memq #'lispm-desktop-popup--keep-size window-size-change-functions))))
      (kill-buffer buffer))))

(ert-deftest lispm-desktop-test-strip-menu-place ()
  "A module's menu pops up small, its outline on the module's left edge and
its bottom two pixels into the strip, as Mac OS 9's did."
  (lispm-desktop-test--with-strip
    (let ((items '((:label "One" :action ignore) separator (:label "Two" :action ignore))))
      (let ((lispm-desktop-popup-small t))
        (should (= (cdr (lispm-desktop-popup--rows items)) (+ 13 6 13))))
      (should (= (cdr (lispm-desktop-popup--rows items)) (+ 16 6 16)))
      (cl-letf (((symbol-function 'frame-position) (lambda (&rest _) '(0 . 500))))
        (should (equal (lispm-desktop-strip--menu-place 'desktop items)
                       (cons (+ 15 14 28 -1) (- 501 32))))))))

(ert-deftest lispm-desktop-test-strip-place-split ()
  "The strip goes in its screen's bottom-left corner when the screen is
split in windows too, whose root window is no live window; never above
the root window, under the menu bar."
  (lispm-desktop-test--with-strip
    (save-window-excursion
      (delete-other-windows)
      (split-window-right)
      (split-window-below)
      (let ((frame (selected-frame)) placed)
        (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                  ((symbol-function 'frame-parent) (lambda (&rest _) frame))
                  ((symbol-function 'set-frame-position)
                   (lambda (_frame x y) (setq placed (cons x y)))))
          (lispm-desktop-strip--place)
          (pcase-let ((`(,_ ,top ,_ ,bottom) (window-pixel-edges (frame-root-window frame))))
            (should (equal placed
                           (cons 0 (max top (- bottom lispm-desktop-platinum-strip-height)))))))))))

(ert-deftest lispm-desktop-test-strip-right-edge-parts ()
  "On the right edge the tab is the strip's left end and the collapse box
its right end, as Mac OS 9 drew it; collapsed, the tab alone is left."
  (lispm-desktop-test--with-strip
    (let ((lispm-desktop-strip-edge 'right)
          (lispm-desktop-strip-width 2))
      (should (equal (lispm-desktop-strip--parts)
                     '(tab left (module . appearance) (module . desktop) right collapse)))
      (should (= (lispm-desktop-strip--x '(module . appearance)) (+ 17 14)))
      (let ((lispm-desktop-strip-collapsed t))
        (should (equal (lispm-desktop-strip--parts) '(tab)))))))

(ert-deftest lispm-desktop-test-strip-position ()
  "The strip lies on its edge, raised from the bottom, never above the
bounds' top, under the menu bar."
  (let ((bounds '(0 20 800 600))
        (h lispm-desktop-platinum-strip-height))
    (should (equal (lispm-desktop-strip--position bounds 200 'left 0) (cons 0 (- 600 h))))
    (should (equal (lispm-desktop-strip--position bounds 200 'right 100) (cons 600 (- 600 h 100))))
    (should (equal (lispm-desktop-strip--position bounds 200 'right 9999) '(600 . 20)))))

(ert-deftest lispm-desktop-test-strip-moved ()
  "Dragged by its tab, the strip goes to the edge whose half of the screen
the mouse is in, as high as the mouse took it, kept on the screen."
  (let* ((bounds '(0 20 800 600))
         (h lispm-desktop-platinum-strip-height)
         (start (- 600 h)))
    (should (equal (lispm-desktop-strip--moved bounds start '(100 . -200)) '(left . 200)))
    (should (equal (lispm-desktop-strip--moved bounds start '(400 . -200)) '(right . 200)))
    (should (equal (lispm-desktop-strip--moved bounds start '(399 . 50)) '(left . 0)))
    (should (equal (lispm-desktop-strip--moved bounds start '(700 . -9999)) (cons 'right (- 600 h 20))))))

(ert-deftest lispm-desktop-test-strip-moves-and-hides-from-the-keyboard ()
  "In the strip, C-p and C-n move it up and down its edge, C-a and C-e to
the left and the right edge; C-c B hides it and shows it again, for the
next start too."
  (lispm-desktop-test--with-strip
    (let ((frame (selected-frame))
          (h lispm-desktop-platinum-strip-height))
      (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                ((symbol-function 'lispm-desktop-strip--open) #'ignore)
                ((symbol-function 'frame-parent) (lambda (&rest _) frame))
                ((symbol-function 'lispm-desktop-strip--bounds) (lambda (_) '(0 20 800 600))))
        (let ((unread-command-events (list ?\C-p ?\C-p ?\C-n ?\C-e ?\C-g)))
          (lispm-desktop-strip-select))
        (should (eq lispm-desktop-strip-edge 'right))
        (should (= lispm-desktop-strip-raised h))
        (let ((unread-command-events (list ?\C-n ?\C-n ?\C-a ?\C-g)))
          (lispm-desktop-strip-select))
        (should (eq lispm-desktop-strip-edge 'left))
        (should (= lispm-desktop-strip-raised 0))
        (should (equal (assq 'lispm-desktop-strip-edge saved) '(lispm-desktop-strip-edge . left))))
      (let ((live t) (calls nil))
        (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) (lambda () live))
                  ((symbol-function 'lispm-desktop-strip-show) (lambda () (push 'show calls) (setq live t)))
                  ((symbol-function 'lispm-desktop-strip-hide) (lambda () (push 'hide calls) (setq live nil))))
          (lispm-desktop-strip-toggle)
          (should-not lispm-desktop-strip-shown)
          (lispm-desktop-strip-toggle)
          (should lispm-desktop-strip-shown)
          (should (equal (reverse calls) '(hide show))))))))

(ert-deftest lispm-desktop-test-strip-outline ()
  "Moving the strip draws Mac OS 9's outline: a solid band two pixels thick
round it, stepping round the tab's slanted end; on the right edge its
mirror."
  (let* ((width 200)
         (tab (- width 17))
         (left (lispm-desktop-platinum-strip-outline-lines 0 100 width 'left "#666633"))
         (right (lispm-desktop-platinum-strip-outline-lines 0 100 width 'right "#666633")))
    ;; Over and along its top, to the tab's top corner; down its left edge.
    (should (member (list -1 99 (+ tab 8 2) 1 nil "#666633") left))
    (should (member (list -1 100 (+ tab 8 3) 1 nil "#666633") left))
    (should (member (list -1 101 22 2 t "#666633") left))
    ;; The tab's straight end, rows 9 to 14, two pixels thick.
    (should (member (list (+ tab 16) 109 6 2 t "#666633") left))
    ;; Where it slants, four: its first slanted row.
    (should (member (list (+ tab 7) 101 4 1 nil "#666633") left))
    ;; The right edge's outline is the left's mirror.
    (cl-flet ((mirror (line)
                (pcase-let ((`(,x ,y ,length ,thickness ,vertical ,color) line))
                  (list (- width 1 (+ x (if vertical thickness length) -1)) y length thickness vertical color))))
      (should (equal (sort (mapcar #'mirror left) (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))
                     (sort (copy-sequence right) (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b)))))))))

(ert-deftest lispm-desktop-test-strip-tab-right ()
  "The right edge's tab is the left one's outline mirrored, Mac OS 9's light
kept: lit at its left, shaded at its right."
  (let ((left lispm-desktop-platinum--strip-tab)
        (right lispm-desktop-platinum--strip-tab-right))
    (should (= (length left) (length right)))
    (cl-loop for l in left for r in right
             do (should (= (length l) (length r) 17))
             do (dotimes (i 17)
                  (should (eq (eq (aref l i) ?.) (eq (aref r (- 16 i)) ?.)))))
    (should (equal (lispm-desktop-platinum--strip-tab-ends right) (make-list 24 16)))
    (dolist (row (seq-subseq right 2 21))
      (should (eq (aref row 15) ?8)))))

(ert-deftest lispm-desktop-test-strip-inverse ()
  "The outline is drawn in the inverse of what it lies on, as Mac OS 9's XOR."
  (should (equal (lispm-desktop-platinum-inverse "#9999CC") "#666633"))
  (should (equal (lispm-desktop-platinum-inverse "black") "#ffffff")))

(ert-deftest lispm-desktop-test-strip-keyboard ()
  "C-c b goes to the strip: C-f and C-b move between the modules, Return or
Space opens the ringed one's menu, and C-g leaves; a collapsed strip shows
whole meanwhile."
  (lispm-desktop-test--with-strip
    (let ((opened nil)
          (rings nil)
          (lispm-desktop-strip-collapsed t))
      (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                ((symbol-function 'lispm-desktop-strip--render)
                 (lambda () (when lispm-desktop-strip--focus
                              (push (cons lispm-desktop-strip--focus lispm-desktop-strip-collapsed) rings))))
                ((symbol-function 'lispm-desktop-strip--open)
                 (lambda (key) (push key opened) '(:label "Chosen"))))
        (let ((unread-command-events (list ?\C-f ?\C-f ?\C-b ?\C-f ?
)))
          (lispm-desktop-strip-select))
        (should (equal opened '(keyboard)))
        (should (equal (mapcar #'car (reverse rings)) '(appearance desktop keyboard desktop keyboard keyboard)))
        (should-not (cdr (car rings)))
        (should lispm-desktop-strip-collapsed)
        (should-not lispm-desktop-strip--focus)
        (setq opened nil)
        (let ((unread-command-events (list ?\C-b ?\C-g)))
          (lispm-desktop-strip-select))
        (should-not opened)))))

(ert-deftest lispm-desktop-test-strip-background-set-when-changed ()
  "Drawing the strip again sets its frame's background only when the
colour changed: NS Emacs clears a frame whenever its background is set."
  (let ((lispm-desktop-strip--frame (selected-frame))
        (color "#C0C0C0")
        (background "#C0C0C0")
        (sets nil))
    (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
              ((symbol-function 'lispm-desktop-strip--line) (lambda () "strip"))
              ((symbol-function 'lispm-desktop-platinum--color) (lambda (_key) color))
              ((symbol-function 'lispm-desktop-popup--size) #'ignore)
              ((symbol-function 'lispm-desktop-strip--width) (lambda () 100))
              ((symbol-function 'lispm-desktop-strip--place) #'ignore)
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (and (eq parameter 'background-color) background)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame parameter value)
                 (push (cons parameter value) sets)
                 (when (eq parameter 'background-color) (setq background value)))))
      (unwind-protect
          (progn
            (lispm-desktop-strip--render)
            (lispm-desktop-strip--render)
            (should-not sets)
            (setq color "#1b1d24")
            (lispm-desktop-strip--render)
            (lispm-desktop-strip--render)
            (should (equal sets '((background-color . "#1b1d24")))))
        (when (get-buffer lispm-desktop-strip-buffer-name)
          (kill-buffer lispm-desktop-strip-buffer-name))))))

(ert-deftest lispm-desktop-test-strip-walks-menus ()
  "In a module's menu opened from the keyboard, Right and Left go on to the
menu of the module beside, as along the menu bar."
  (lispm-desktop-test--with-strip
    (let ((opened nil))
      (cl-letf (((symbol-function 'lispm-desktop-strip--open)
                 (lambda (key)
                   (push key opened)
                   (when (< (length opened) 3)
                     (throw 'lispm-desktop-popup-menu-bar-step (if (eq key 'appearance) -1 1)))
                   nil)))
        (should (equal (lispm-desktop-strip--walk (lispm-desktop-strip--modules) 0) '(nil . 0)))
        (should (equal opened '(appearance mail appearance)))))))

;;;; Chrome extensions in the browser

(ert-deftest lispm-desktop-test-extensions-only-on-a-mac-with-pages ()
  "Pages run extensions only in the Mac build with xwidgets, on macOS 15.4
or later."
  (let ((system-type 'gnu/linux))
    (should-not (lispm-desktop-browser-extensions-supported-p)))
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'featurep) (lambda (feature &rest _) (memq feature '(ns xwidget-internal)))))
      (let ((lispm-desktop-browser-extensions--macos-version "15.3.2"))
        (should-not (lispm-desktop-browser-extensions-supported-p)))
      (let ((lispm-desktop-browser-extensions--macos-version "15.4"))
        (should (lispm-desktop-browser-extensions-supported-p)))
      (let ((lispm-desktop-browser-extensions--macos-version "27.0"))
        (should (lispm-desktop-browser-extensions-supported-p))))))

(ert-deftest lispm-desktop-test-extensions-know-a-page-by-its-view ()
  "A page is named to the module by its view's address, which Emacs prints
with its xwidget; a killed xwidget names none."
  (should (eql (lispm-desktop-browser-extensions--address-in "#<xwidget 3 0x600001c2a0f0>")
               #x600001c2a0f0))
  (should-not (lispm-desktop-browser-extensions--address-in "#<killed xwidget>"))
  (should-not (lispm-desktop-browser-extensions--address-in "#<buffer *scratch*>")))

(ert-deftest lispm-desktop-test-extensions-read-store-pages ()
  "The extension of a Chrome Web Store page is read from its address, on the
store itself over HTTPS only."
  (let ((id "ddkjiahejlhfcafbddmgiahcphecmpfh"))
    (should (equal (lispm-desktop-browser-extensions--store-id
                    (concat "https://chromewebstore.google.com/detail/ublock-origin-lite/" id))
                   id))
    (should (equal (lispm-desktop-browser-extensions--store-id
                    (concat "https://chromewebstore.google.com/detail/" id "?hl=en"))
                   id))
    (should (equal (lispm-desktop-browser-extensions--store-id
                    (concat "https://chrome.google.com/webstore/detail/name/" id "/reviews"))
                   id))
    (should-not (lispm-desktop-browser-extensions--store-id
                 (concat "http://chromewebstore.google.com/detail/name/" id)))
    (should-not (lispm-desktop-browser-extensions--store-id
                 (concat "https://evil.example/detail/name/" id)))
    (should-not (lispm-desktop-browser-extensions--store-id
                 (concat "https://chromewebstore.google.com/detail/name/" id "a")))
    (should-not (lispm-desktop-browser-extensions--store-id nil))))

(ert-deftest lispm-desktop-test-extensions-stale-module ()
  "The module is stale when a source of it is newer than its library."
  (let* ((source (make-temp-file "lispm-desktop-webext" t))
         (lispm-desktop-browser-extensions-source-directory source)
         (library (expand-file-name ".build/release/liblispm_desktop_webext.dylib" source))
         (swift (expand-file-name "Sources/LispmDesktopWebext/Host.swift" source)))
    (unwind-protect
        (progn
          (should-not (lispm-desktop-browser-extensions--module-file))
          (make-directory (file-name-directory library) t)
          (make-directory (file-name-directory swift) t)
          (dolist (file (list (expand-file-name "Package.swift" source) swift))
            (write-region "" nil file)
            (set-file-times file (time-subtract nil 60)))
          (write-region "" nil library)
          (should (equal (lispm-desktop-browser-extensions--module-file) library))
          (should-not (lispm-desktop-browser-extensions--stale-p))
          (set-file-times swift (time-add nil 60))
          (should (lispm-desktop-browser-extensions--stale-p)))
      (delete-directory source t))))

(ert-deftest lispm-desktop-test-extensions-events ()
  "The module's events: a message shows, a change forgets the list and runs
the hook, and a question is asked after the filter returns and answered,
no when quit."
  (let ((lispm-desktop-browser-extensions--installed '((:id "a")))
        (changed nil)
        (answers nil)
        (later nil)
        (reply 'yes))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args) (push (cons function args) later)))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (should (string-match-p "Add “X 1”\\?\n  • Block\n  • Read\n" prompt))
                 (pcase reply ('yes t) ('no nil) ('quit (signal 'quit nil)))))
              ((symbol-function 'lispm-desktop-webext-module-answer)
               (lambda (id value) (push (cons id value) answers))))
      (let ((lispm-desktop-browser-extensions-changed-hook (list (lambda () (setq changed t)))))
        (lispm-desktop-webext--event "changed" "null")
        (should changed)
        (should (eq lispm-desktop-browser-extensions--installed 'unread)))
      (let ((shown nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (setq shown (apply #'format-message format args)))))
          (lispm-desktop-webext--event "message" "\"X is installed\""))
        (should (equal shown "X is installed")))
      (let ((ask "{\"request\":7,\"question\":\"Add “X 1”?\",\"details\":[\"Block\",\"Read\"]}"))
        (lispm-desktop-webext--event "ask" ask)
        (should-not answers)
        (pcase-let ((`(,function . ,args) (pop later)))
          (apply function args))
        (should (equal answers '((7 . 1))))
        (setq reply 'no answers nil)
        (lispm-desktop-webext--event "ask" ask)
        (pcase-let ((`(,function . ,args) (pop later))) (apply function args))
        (should (equal answers '((7))))
        (setq reply 'quit answers nil)
        (lispm-desktop-webext--event "ask" ask)
        (pcase-let ((`(,function . ,args) (pop later)))
          (should-error (apply function args) :type 'quit))
        (should (equal answers '((7))))))))

(ert-deftest lispm-desktop-test-extensions-open-tabs ()
  "An extension's new tab opens in the browser, made for the extension's
page when it is one, and the module is answered with its view."
  (let ((calls nil)
        (answers nil))
    (cl-letf (((symbol-function 'lispm-desktop-browser--window) #'always)
              ((symbol-function 'lispm-desktop-webext-module-expect-page)
               (lambda (url)
                 (push (list 'expect url) calls)
                 (string-prefix-p "chrome-extension:" url)))
              ((symbol-function 'lispm-desktop-browser-new-tab)
               (lambda (url) (push (list 'tab url) calls)))
              ((symbol-function 'xwidget-webkit-goto-uri)
               (lambda (_xwidget url) (push (list 'go url) calls)))
              ((symbol-function 'lispm-desktop-browser-extensions--address)
               (lambda (_xwidget) 42))
              ((symbol-function 'lispm-desktop-webext-module-answer)
               (lambda (id value) (push (cons id value) answers))))
      ;; The extension's page: the tab opens blank and is sent there, as
      ;; `xwidget-webkit-browse-url' would take chrome-extension: for a host.
      (lispm-desktop-browser-extensions--open
       '(:url "chrome-extension://abc/welcome.html" :active t :request 3))
      (should (equal (reverse calls) '((expect "chrome-extension://abc/welcome.html")
                                       (tab "about:blank")
                                       (go "chrome-extension://abc/welcome.html"))))
      (should (equal answers '((3 . 42))))
      ;; A website opens straight away; asked for nothing back, nothing is
      ;; answered.
      (setq answers nil calls nil)
      (lispm-desktop-browser-extensions--open '(:url "https://example.com/" :active t))
      (should (equal (reverse calls) '((expect "https://example.com/") (tab "https://example.com/"))))
      (should-not answers))))

(ert-deftest lispm-desktop-test-extensions-follow-the-page-in-front ()
  "The module hears which page is in front, once it runs."
  (let ((lispm-desktop-browser-extensions--started t)
        (told nil)
        (address 1))
    (with-temp-buffer
      (let ((page (current-buffer)))
        (cl-letf (((symbol-function 'lispm-desktop-browser-extensions--front-buffer) (lambda () page))
                  ((symbol-function 'xwidget-at) (lambda (_) 'xwidget))
                  ((symbol-function 'lispm-desktop-browser-extensions--address) (lambda (_) address))
                  ((symbol-function 'lispm-desktop-webext-module-activate) (lambda (a) (push a told))))
          (lispm-desktop-browser-extensions--follow)
          (setq address 2)
          (lispm-desktop-browser-extensions--follow)
          (should (equal told '(2 1)))
          (let ((lispm-desktop-browser-extensions--started nil))
            (lispm-desktop-browser-extensions--follow))
          (should (equal told '(2 1))))))))

(ert-deftest lispm-desktop-test-extensions-menu ()
  "The Extensions menu has each running extension's button, its badge in
the label, then adding them, from the page shown when it is a store page."
  (let ((lispm-desktop-browser-extensions--started t)
        (lispm-desktop-browser-extensions--installed
         '((:id "a" :name "Blocker" :loaded t :badge "12" :actionEnabled t)
           (:id "b" :name "Off" :enabled nil :loaded nil))))
    (cl-letf (((symbol-function 'lispm-desktop-browser-extensions--shown-store-id) #'ignore))
      (let ((items (lispm-desktop-browser-extensions--menu-items)))
        (should (equal (plist-get (car items) :label) "Blocker [12]"))
        (should (eq (nth 1 items) 'separator))
        (should (equal (plist-get (nth 2 items) :label) "Add This Extension"))
        (should-not (plist-get (nth 2 items) :enabled))
        (should-not (seq-find (lambda (item) (equal (plist-get item :label) "Off"))
                              (seq-filter #'consp items)))))
    (cl-letf (((symbol-function 'lispm-desktop-browser-extensions--shown-store-id)
               (lambda () "ddkjiahejlhfcafbddmgiahcphecmpfh")))
      (should (plist-get (nth 2 (lispm-desktop-browser-extensions--menu-items)) :enabled))))
  (let ((lispm-desktop-browser-extensions--started nil))
    (cl-letf (((symbol-function 'lispm-desktop-browser-extensions--shown-store-id) #'ignore))
      (should (equal (plist-get (car (lispm-desktop-browser-extensions--menu-items)) :label)
                     "Add This Extension")))))

(provide 'lispm-desktop-test)
;;; lispm-desktop-test.el ends here
