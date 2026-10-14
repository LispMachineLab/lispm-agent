;;; lme-buffer-manager.el --- What every buffer costs  -*- lexical-binding: t; -*-

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

;; The buffer manager lists the open buffers with what each costs, as Mac
;; OS 9's About This Computer barred each application's memory and as a
;; task manager lists processes: its kind, the size of its text, and for a
;; buffer running a process (a terminal, an agent's CLI) the CPU and the
;; memory of that process and every process it started, so an agent's
;; whole tree of tools is counted.  The list refreshes itself while shown.
;;
;;   ps --> pid: parent, CPU, memory --> each buffer's process tree --> row

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'tabulated-list)
(require 'lme-platinum)

(declare-function lme-terminal-buffer-p "lme-interface" (buffer))
(declare-function lme-agent--buffer-p "lme-ai" (buffer))
(declare-function lme-sidebar-show-in-main "lme-sidebar" (buffer))

(defconst lme-buffer-manager-buffer-name "*Buffer Manager*"
  "Name of the buffer manager's buffer.")

(defconst lme-buffer-manager-interval 2
  "Seconds between refreshes of the buffer manager while it is shown.")

;;;; Processes

(defun lme-buffer-manager--processes ()
  "Return the system's processes: a table of pid to (PARENT CPU MEMORY).
CPU is a percentage of one core, MEMORY the resident size in kilobytes,
read from ps in one go.  Where ps is missing the table is empty."
  (let ((table (make-hash-table)))
    (dolist (line (ignore-errors (process-lines "ps" "-axo" "pid=,ppid=,pcpu=,rss=")))
      (pcase (split-string line)
        (`(,pid ,parent ,cpu ,memory)
         (puthash (string-to-number pid)
                  (list (string-to-number parent) (string-to-number cpu)
                        (string-to-number memory))
                  table))))
    table))

(defun lme-buffer-manager--children (processes)
  "Return a table of pid to the pids of its children, from PROCESSES."
  (let ((children (make-hash-table)))
    (maphash (lambda (pid info) (push pid (gethash (car info) children))) processes)
    children))

(defun lme-buffer-manager--tree-cost (pid processes children)
  "Return (CPU . MEMORY) of process PID and every process under it.
PROCESSES and CHILDREN are the tables of `lme-buffer-manager--processes'
and `lme-buffer-manager--children'."
  (let ((cpu 0) (memory 0) (seen nil) (queue (list pid)))
    (while queue
      (let ((next (pop queue)))
        (unless (memq next seen)
          (push next seen)
          (when-let* ((info (gethash next processes)))
            (setq cpu (+ cpu (nth 1 info))
                  memory (+ memory (nth 2 info))))
          (setq queue (append (gethash next children) queue)))))
    (cons cpu memory)))

;;;; Rows

(defun lme-buffer-manager--kind (buffer)
  "Return what kind of buffer BUFFER is, in words."
  (with-current-buffer buffer
    (cond ((and (fboundp 'lme-agent--buffer-p) (lme-agent--buffer-p buffer)) "Agent")
          ((and (fboundp 'lme-terminal-buffer-p) (lme-terminal-buffer-p buffer)) "Terminal")
          ((derived-mode-p 'xwidget-webkit-mode) "Web Page")
          ((derived-mode-p 'dired-mode) "Folder")
          (buffer-file-name "Document")
          ((derived-mode-p 'help-mode) "Help")
          (t (let ((name (format-mode-line mode-name nil nil buffer)))
               (if (string-empty-p name) (symbol-name major-mode) name))))))

(defun lme-buffer-manager--buffers ()
  "Return the buffers to list: every one but Emacs's internal buffers."
  (seq-remove (lambda (buffer)
                (or (string-prefix-p " " (buffer-name buffer))
                    (minibufferp buffer)))
              (buffer-list)))

(defun lme-buffer-manager--number (string value)
  "Return STRING carrying VALUE, the number its column sorts by."
  (propertize string 'lme-value value))

(defun lme-buffer-manager--size (bytes)
  "Return BYTES as a short size, as Finder wrote them."
  (if (< bytes 1024)
      (format "%d B" bytes)
    (file-size-human-readable bytes 'iec " ")))

(defun lme-buffer-manager--bar (fraction)
  "Return a memory bar FRACTION full, as About This Computer drew one."
  (let ((filled (round (* (min 1 (max 0 fraction)) 58))))
    (propertize " " 'display
                (lme-platinum--image
                 (list 'memory-bar filled)
                 (lambda ()
                   (let ((svg (svg-create 60 10)))
                     (svg-rectangle svg 0 0 60 10 :fill (lme-platinum--color :frame))
                     (svg-rectangle svg 1 1 58 8 :fill (lme-platinum--color :content))
                     (when (> filled 0)
                       (svg-rectangle svg 1 1 filled 8 :fill (lme-platinum--color :accent)))
                     svg))))))

(defun lme-buffer-manager--entries ()
  "Return the buffer manager's rows, as `tabulated-list-entries' takes them."
  (let* ((processes (lme-buffer-manager--processes))
         (children (lme-buffer-manager--children processes))
         (rows
          (mapcar
           (lambda (buffer)
             (let* ((process (get-buffer-process buffer))
                    ;; An lme-term's program is the module's, not Emacs's.
                    (pid (or (and process (process-id process))
                             (and (fboundp 'lme-term-pid) (lme-term-pid buffer))))
                    (cost (and pid (lme-buffer-manager--tree-cost pid processes children)))
                    (bytes (with-current-buffer buffer
                             (max 0 (1- (position-bytes (point-max)))))))
               (list buffer process pid cost bytes)))
           (lme-buffer-manager--buffers)))
         (most (max 1 (apply #'max 1 (mapcar (lambda (row) (or (cdr (nth 3 row)) 0)) rows)))))
    (mapcar
     (pcase-lambda (`(,buffer ,process ,pid ,cost ,bytes))
       (let ((memory (or (cdr cost) 0)))
         (list buffer
               (vector
                ;; The icon is one character drawn two wide: the name is cut
                ;; short of the column, so it never runs into the next.
                (concat (lme-platinum-buffer-icon buffer) " "
                        (truncate-string-to-width (lme-platinum--buffer-title buffer)
                                                  29 nil nil "…"))
                (lme-buffer-manager--kind buffer)
                (lme-buffer-manager--number (lme-buffer-manager--size bytes) bytes)
                (cond ((and pid process) (format "%s (%d)" (process-name process) pid))
                      (pid (format "terminal (%d)" pid))
                      (process (format "%s (network)" (process-name process)))
                      (t ""))
                (lme-buffer-manager--number (if cost (format "%.1f%%" (car cost)) "")
                                            (or (car cost) -1))
                (lme-buffer-manager--number
                 (if cost
                     (concat (lme-buffer-manager--size (* 1024 memory)) " "
                             (lme-buffer-manager--bar (/ (float memory) most)))
                   "")
                 (if cost memory -1))))))
     rows)))

(defun lme-buffer-manager--summary ()
  "Return the buffer manager's bottom line: the buffers, and Emacs's own cost."
  (let* ((attributes (ignore-errors (process-attributes (emacs-pid))))
         (cpu (alist-get 'pcpu attributes))
         (memory (alist-get 'rss attributes)))
    (concat (format "%d buffers" (length (lme-buffer-manager--buffers)))
            (when memory
              (format " · Emacs %s%s"
                      (lme-buffer-manager--size (* 1024 memory))
                      (if cpu (format ", %.1f%% CPU" cpu) ""))))))

(defun lme-buffer-manager--by (column)
  "Return a predicate sorting rows by the number in COLUMN."
  (lambda (a b)
    (< (or (get-text-property 0 'lme-value (aref (cadr a) column)) -1)
       (or (get-text-property 0 'lme-value (aref (cadr b) column)) -1))))

;;;; The list

(defvar lme-buffer-manager--timer nil
  "Repeating timer refreshing the buffer manager while it is shown.")

(defun lme-buffer-manager-refresh (&rest _)
  "Refresh the buffer manager's rows and bottom line, keeping the row at point.
The refresh timer stops once the list is no longer shown."
  (interactive)
  (if-let* ((buffer (get-buffer lme-buffer-manager-buffer-name))
            ((get-buffer-window buffer t)))
      (with-current-buffer buffer
        (setq tabulated-list-entries (lme-buffer-manager--entries)
              lme-platinum-info (lme-buffer-manager--summary))
        (tabulated-list-print t t)
        (force-mode-line-update))
    (when lme-buffer-manager--timer
      (cancel-timer lme-buffer-manager--timer)
      (setq lme-buffer-manager--timer nil))))

(defun lme-buffer-manager-show-buffer ()
  "Show the buffer of the row at point in the main area."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id))
              ((buffer-live-p buffer)))
    (lme-sidebar-show-in-main buffer)))

(defun lme-buffer-manager-close-buffer ()
  "Close the buffer of the row at point, asking first as its close box would."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id))
              ((buffer-live-p buffer))
              ((lme-platinum--confirm-close buffer)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))
    (lme-buffer-manager-refresh)))

(defvar-keymap lme-buffer-manager-mode-map
  :doc "Keymap of the buffer manager."
  :parent tabulated-list-mode-map
  "RET" #'lme-buffer-manager-show-buffer
  "<double-mouse-1>" #'lme-buffer-manager-show-buffer
  "k" #'lme-buffer-manager-close-buffer
  "d" #'lme-buffer-manager-close-buffer
  "<delete>" #'lme-buffer-manager-close-buffer
  "g" #'lme-buffer-manager-refresh)

(define-derived-mode lme-buffer-manager-mode tabulated-list-mode "Buffer Manager"
  "List the open buffers with what each costs: text, CPU and memory.
\\{lme-buffer-manager-mode-map}"
  (setq tabulated-list-format
        (vector '("Name" 34 t)
                '("Kind" 11 t)
                (list "Text" 9 (lme-buffer-manager--by 2) :right-align t)
                '("Process" 20 t)
                (list "CPU" 7 (lme-buffer-manager--by 4) :right-align t)
                (list "Memory" 18 (lme-buffer-manager--by 5)))
        tabulated-list-padding 1
        tabulated-list-sort-key '("Memory" . t))
  (setq-local lme-platinum-title "Buffer Manager"
              lme-platinum-icon (lme-platinum-icon "computer"))
  ;; One line per buffer, cut at the window's edge rather than wrapped;
  ;; turning visual line mode off restores the lines it found, so it goes
  ;; first.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (face-remap-add-relative 'default 'lme-platinum-list)
  (face-remap-add-relative 'header-line 'lme-platinum-header)
  (tabulated-list-init-header))

(defun lme-buffer-manager ()
  "Show the buffer manager: every open buffer, and what each costs."
  (interactive)
  (let ((buffer (get-buffer-create lme-buffer-manager-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lme-buffer-manager-mode)
        (lme-buffer-manager-mode)))
    (lme-sidebar-show-in-main buffer)
    (lme-buffer-manager-refresh)
    (unless lme-buffer-manager--timer
      (setq lme-buffer-manager--timer
            (run-with-timer lme-buffer-manager-interval lme-buffer-manager-interval
                            #'lme-buffer-manager-refresh)))
    buffer))

(provide 'lme-buffer-manager)
;;; lme-buffer-manager.el ends here
