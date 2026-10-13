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
  "A batch Emacs settles on Emacs keys without asking; a set style is kept."
  (let ((lme-editing-style nil))
    (cl-letf (((symbol-function 'lme--read-editing-style)
               (lambda () (error "Asked in batch"))))
      (should (lme--first-start))
      (should (eq lme-editing-style 'emacs))))
  (let ((lme-editing-style 'vim))
    (should-not (lme--first-start))
    (should (eq lme-editing-style 'vim))))

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

(provide 'lme-test)
;;; lme-test.el ends here
