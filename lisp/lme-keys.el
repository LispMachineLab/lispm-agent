;;; lme-keys.el --- Global keys and the Evil leader  -*- lexical-binding: t; -*-

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

;; Global keys, and the leader key (SPC) of the vim editing style.

;;; Code:

(defconst lme-leader-bindings
  '(("e" . find-file)
    ("b" . consult-buffer)
    ("k" . kill-buffer)
    ("d" . dired)
    ("z" . repeat)
    ("n" . next-buffer)
    ("p" . previous-buffer)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . split-window-right)
    ("4" . other-window)
    ("5" . other-frame)
    ("g" . magit-status)
    ("t" . eat)
    ("ff" . consult-find)
    ("fr" . consult-ripgrep)
    ("T" . treemacs-add-and-display-current-project)
    ("xb" . lme-xwidget-browse-url)
    ("xs" . lme-xwidget-search)
    ("xu" . lme-xwidget-open-current-url))
  "Key suffixes bound under the evil leader key, paired with their commands.")

(defun lme-setup-keymapping ()
  "Set up basic keybindings."
  (with-eval-after-load 'evil
    (evil-set-leader '(normal visual motion) (kbd "SPC"))
    (dolist (binding lme-leader-bindings)
      (evil-define-key '(normal visual motion) 'global
        (kbd (concat "<leader>" (car binding))) (cdr binding))))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-x C-b") 'consult-buffer)
  ;; C-x RET and C-x C-m are the same key sequence; bind it once.
  (global-set-key (kbd "C-x RET") #'toggle-frame-fullscreen)
  (global-set-key (kbd "M-x") #'execute-extended-command)
  (global-set-key (kbd "M-c") #'lme-copy-region)
  (global-set-key (kbd "M-v") #'lme-paste)
  (global-set-key (kbd "C-w") #'lme-cut-region)
  (global-set-key (kbd "M-w") #'lme-copy-region)
  (global-set-key (kbd "C-y") #'lme-paste)
  (lme-setup-meta-key-issue))


(provide 'lme-keys)
;;; lme-keys.el ends here
