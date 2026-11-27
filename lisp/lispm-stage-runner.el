;;; lispm-stage-runner.el --- Shared controller for live tool stages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared stage controller mechanics used by `lispm-shell-stage' and
;; `lispm-eval-stage':
;;
;;   - badge & elapsed-time header formatting
;;   - per-agent stage buffer creation and major-mode setup
;;   - spotlight/edit-aware window presentation
;;   - shared elapsed-time ticker lifecycle

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-i18n)
(require 'lispm-stage)
(require 'lispm-edits)
(require 'lispm-spotlight)

(defgroup lispm-stage-runner nil
  "Shared controller for agents' live tool stages."
  :group 'lispm
  :prefix "lispm-stage-runner-")

(defface lispm-stage-runner-prompt
  '((t :inherit font-lock-keyword-face :weight bold))
  "Prompt face used at the top of a stage buffer."
  :group 'lispm-stage-runner)

(defface lispm-stage-runner-ok
  '((((background dark)) :foreground "#a6e3a1" :weight bold)
    (t :foreground "#1e662d" :weight bold))
  "Badge face for stage executions completing successfully."
  :group 'lispm-stage-runner)

(defface lispm-stage-runner-fail
  '((((background dark)) :foreground "#f38ba8" :weight bold)
    (t :foreground "#b42318" :weight bold))
  "Badge face for stage executions failing or timing out."
  :group 'lispm-stage-runner)

(defun lispm-stage-runner-live-p (enabled in-batch)
  "Return non-nil if a stage with ENABLED and IN-BATCH is active."
  (and enabled (or (not noninteractive) in-batch)))

(defun lispm-stage-runner-setup-mode (header-var)
  "Configure the current buffer for stage display using HEADER-VAR."
  (setq-local buffer-read-only t
              truncate-lines nil
              header-line-format `(:eval ,header-var))
  (if (fboundp 'lispm-disable-line-numbers)
      (lispm-disable-line-numbers)
    (setq-local display-line-numbers nil)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))))

(defun lispm-stage-runner-buffer (tag agent mode &optional call-id)
  "Return or create AGENT's stage buffer for TAG with major MODE.
When CALL-ID is non-nil, suffix the buffer name to isolate concurrent calls."
  (let* ((label (if agent (lispm-principal-name agent) "LispM"))
         (name (if call-id
                   (format "*lispm-stage-%s: %s (%s)*" tag label call-id)
                 (format "*lispm-stage-%s: %s*" tag label)))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p mode)
        (funcall mode)))
    buf))

(defun lispm-stage-runner-header (agent summary status-str status-face)
  "Build a stage header line for AGENT, SUMMARY, STATUS-STR and STATUS-FACE."
  (let* ((color (lispm-edits-color agent))
         (label (if agent (symbol-name (lispm-principal-name agent)) "LispM"))
         (short (truncate-string-to-width (or summary "") 52 nil nil "…")))
    (concat (propertize " " 'face `(:background ,color))
            " "
            (propertize (concat " λ " label " ")
                        'face `(:background ,color :foreground "#1e1e2e" :weight bold))
            " "
            (propertize short 'face 'bold)
            " · "
            (propertize status-str 'face status-face))))

(defun lispm-stage-runner-running-header (agent summary started-at now)
  "Build a running stage header line for AGENT, SUMMARY, STARTED-AT and NOW."
  (lispm-stage-runner-header
   agent summary
   (lispm-tr "running %.1fs" (max 0.0 (- now started-at)))
   'font-lock-constant-face))

(defun lispm-stage-runner-set-header (buffer mode header-var text)
  "Set stage header TEXT on BUFFER (via HEADER-VAR when in MODE)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (derived-mode-p mode)
          (set header-var text)
        (setq-local header-line-format text))
      (force-mode-line-update))))

(cl-defun lispm-stage-runner-present (agent buffer kind &key follow top-p on-shown)
  "Project BUFFER of KIND onto AGENT's stage window unless edits animate.
FOLLOW enables automatic presentation; TOP-P keeps point at the top of
BUFFER; ON-SHOWN is called with the shown window when presented.
Returns the stage window when presented, or nil."
  (when (and (buffer-live-p buffer)
             follow
             (lispm-spotlight-follow-agent-p agent (float-time))
             (not (lispm-edits-active-p nil agent)))
    (when-let* ((win (lispm-stage-window buffer t agent kind)))
      (unless (eq win (selected-window))
        (with-current-buffer buffer
          (let ((pos (if top-p (point-min) (point-max))))
            (set-window-point win pos)
            (when top-p
              (set-window-start win (point-min))))))
      (when on-shown
        (funcall on-shown win))
      win)))

(defun lispm-stage-runner-tick (sessions timer-sym on-session)
  "Refresh active entries in SESSIONS or cancel TIMER-SYM when empty.
ON-SESSION is called with (SESSION NOW) for each active entry."
  (if (zerop (hash-table-count sessions))
      (let ((timer (symbol-value timer-sym)))
        (when (timerp timer)
          (cancel-timer timer)
          (set timer-sym nil)))
    (let ((now (float-time)))
      (maphash (lambda (_agent session)
                 (funcall on-session session now))
               sessions))))

(defun lispm-stage-runner-ensure-ticker (sessions timer-sym tick-fn &optional interval)
  "Ensure TIMER-SYM runs TICK-FN every INTERVAL seconds while SESSIONS is active."
  (when (and (> (hash-table-count sessions) 0)
             (not (timerp (symbol-value timer-sym))))
    (set timer-sym (run-at-time (or interval 0.2) (or interval 0.2) tick-fn))))

(provide 'lispm-stage-runner)
;;; lispm-stage-runner.el ends here
