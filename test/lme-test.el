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

(ert-deftest lme-test-clock ()
  "The menu bar clock shows the time as Mac OS 9 did, or nothing."
  (let ((lme-platinum-clock-format "%-I:%M %p"))
    (should (string-match-p "\\`[0-9]\\{1,2\\}:[0-9]\\{2\\} [AP]M\\'"
                            (substring-no-properties
                             (string-trim (nth 2 (car (lme-platinum-clock))))))))
  (let ((lme-platinum-clock-format nil))
    (should-not (lme-platinum-clock))))

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

(provide 'lme-test)
;;; lme-test.el ends here
