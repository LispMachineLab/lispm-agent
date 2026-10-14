;;; lme-exwm.el --- Emacs as the window manager of an X display  -*- lexical-binding: t; -*-

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

;; On an X display that no window manager runs on, Emacs becomes its
;; window manager: EXWM shows every X window as a buffer, which then
;; goes wherever buffers go.  The same configuration runs unchanged on
;; Mac OS Emacs (NS), in a terminal, and on X displays that already have
;; a window manager (XQuartz's rootless quartz-wm, a Linux desktop):
;; there the display decides that nothing happens.

;;; Code:

(require 'lme-core)

(defvar exwm-class-name)
(defvar exwm-workspace-number)
(declare-function x-window-property "xfns.c"
                  (prop &optional frame type source delete-p vector-ret-p))
(declare-function exwm-wm-mode "exwm" (&optional arg))
(declare-function exwm-workspace-rename-buffer "exwm-workspace" (newname))

(defcustom lme-window-manager 'auto
  "Whether Emacs manages the windows of its X display.
`auto' makes Emacs the window manager (EXWM) when it runs on an X
display that has none; nil never does."
  :type '(choice (const :tag "When the X display has no window manager" auto)
                 (const :tag "Never" nil))
  :group 'lispm-emacs)

(defun lme-exwm--check-window (frame &optional window)
  "Return the window WINDOW names as a running window manager's, or nil.
WINDOW defaults to the root window of FRAME's display."
  (let ((value (ignore-errors
                 (x-window-property "_NET_SUPPORTING_WM_CHECK" frame "WINDOW"
                                    (or window 0) nil t))))
    ;; One value comes back as a number, several as a vector.
    (cond ((integerp value) value)
          ((and (vectorp value) (> (length value) 0)) (aref value 0)))))

(defun lme-exwm--window-manager-p (frame)
  "Return non-nil when a window manager runs on FRAME's X display.
An EWMH window manager names a check window on the root window, and
that window names itself.  A name a window manager left behind when it
quit points at a window that is gone, or that no longer does."
  (when-let* ((check (lme-exwm--check-window frame)))
    (eql (lme-exwm--check-window frame check) check)))

(defun lme-exwm-wanted-p (&optional frame)
  "Return non-nil when Emacs should manage the windows of FRAME's display."
  (let ((frame (or frame (selected-frame))))
    (and (eq lme-window-manager 'auto)
         (eq (framep frame) 'x)
         (not (lme-exwm--window-manager-p frame)))))

(defun lme-exwm--name-buffer ()
  "Name an X window's buffer after its application class."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun lme-setup-exwm ()
  "Make Emacs the window manager of its X display when the display has none."
  ;; A package is installed even when the `:if' of its declaration is
  ;; false, so EXWM is declared only where it is used.
  (when (lme-exwm-wanted-p)
    (use-package exwm
      :config
      (setq exwm-workspace-number 1)
      (add-hook 'exwm-update-class-hook #'lme-exwm--name-buffer))
    (exwm-wm-mode 1)))

(provide 'lme-exwm)
;;; lme-exwm.el ends here
