;;; lispm-desktop-early.el --- Settings made before the first frame  -*- lexical-binding: t; -*-

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

;; Loaded from early-init.el: it runs before the first frame is drawn and
;; before packages are initialized.

;;; Code:

;; Terminal emulation (Eat) and language servers allocate heavily while
;; streaming output; at the default 800KB threshold a full-screen agent
;; redraw triggers a collection every fraction of a second.  Collect rarely
;; while busy and catch up when idle instead.
(setq gc-cons-threshold (* 64 1024 1024)
      read-process-output-max (* 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

(setq frame-title-format nil
      icon-title-format nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil
        ns-use-native-fullscreen nil))

;; Log asynchronous native-compilation warnings from third-party packages to
;; *Warnings* without popping up its window, and cap background compilation
;; at two concurrent jobs so a full package rebuild cannot saturate the machine.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-async-jobs-number 2)

;; Frames stay fully opaque: on macOS any transparency makes every redraw
;; composite the whole window, which made scrolling a terminal stutter.
(dolist (frame-setting '((height . 40)
                         (width . 160)
                         (tool-bar-lines . 0)
                         (vertical-scroll-bars . nil)))
  (add-to-list 'default-frame-alist frame-setting)
  (add-to-list 'initial-frame-alist frame-setting))


(provide 'lispm-desktop-early)
;;; lispm-desktop-early.el ends here
