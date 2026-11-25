;;; lispm-job.el --- Unified async jobs and bounded chunk collectors -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Unified lifecycle handle and owner cancellation registry for asynchronous
;; operations (kernel Elisp workers, argv programs, kernel shell runs, and
;; background API requests), plus a non-destructive bounded chunk collector.
;;
;; Formal basis (`lispm-chunk-collector'):
;;   Basis: (limit size chunks truncated)
;;   Span:  Streaming bounded text capture with strict prefix retention and
;;          truncation witness.
;;   Minimality: `limit' bounds capacity, `chunks' holds segments in reverse
;;               arrival order, `size' avoids O(n) re-measurement per chunk,
;;               and `truncated' witnesses overflow even if limit = 0.
;;   Tightness: Reading via `lispm-chunk-collector-string' never mutates
;;              `chunks'.
;;
;; Formal basis (`lispm-job'):
;;   Basis: (owner kind cancel-fn data finished)
;;   Span:  All cancellable background jobs owned by a `lispm-principal' or
;;          native `lispm-agent'.
;;   Minimality: Each job has one owner, one symbolic kind, one idempotent
;;               cancellation thunk, optional caller payload `data', and one
;;               completion bit `finished'.
;;   Tightness: A single global list `lispm-job--active' owns lifecycle hooks
;;              lazily; finishing or cancelling a job before its callback fires
;;              runs `cancel-fn' at most once and suppresses late callbacks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)

(defvar lispm-agents-context-release-functions)
(defvar lispm-agents-bridge--agent)

;;;; Bounded chunk collector

(cl-defstruct (lispm-chunk-collector (:constructor lispm-chunk-collector--create)
                                     (:copier nil))
  "A non-destructive bounded accumulator for streamed text chunks."
  limit (size 0) chunks truncated)

(defun lispm-chunk-collector-create (limit)
  "Create a chunk collector keeping at most LIMIT characters."
  (lispm-chunk-collector--create :limit (max 0 (or limit 0))))

(defun lispm-chunk-collector-push (collector text)
  "Append TEXT to COLLECTOR within its character budget and return COLLECTOR."
  (when (and (stringp text) (not (string-empty-p text)))
    (let ((room (max 0 (- (lispm-chunk-collector-limit collector)
                          (lispm-chunk-collector-size collector)))))
      (when (> (length text) room)
        (setf (lispm-chunk-collector-truncated collector) t))
      (when (> room 0)
        (let ((kept (substring text 0 (min room (length text)))))
          (push kept (lispm-chunk-collector-chunks collector))
          (cl-incf (lispm-chunk-collector-size collector) (length kept))))))
  collector)

(defun lispm-chunk-collector-string (collector)
  "Return the accumulated text in COLLECTOR without mutating it."
  (apply #'concat (reverse (lispm-chunk-collector-chunks collector))))

(defun lispm-chunk-collector-truncated-p (collector)
  "Return non-nil if COLLECTOR dropped characters beyond its limit."
  (lispm-chunk-collector-truncated collector))

;;;; Unified job registry

(cl-defstruct (lispm-job (:constructor lispm-job--create)
                         (:copier nil))
  "Handle for an in-flight asynchronous job tied to a principal or agent."
  owner kind cancel-fn data finished)

(defvar lispm-job--active nil
  "List of currently active `lispm-job' handles.")

(defun lispm-job--install-hooks ()
  "Install owner lifecycle hooks while jobs are active."
  (add-hook 'lispm-state-change-functions #'lispm-job--state-changed)
  (add-hook 'lispm-session-end-functions #'lispm-job--session-ended)
  (add-hook 'lispm-agents-context-release-functions #'lispm-job--terminal-ended))

(defun lispm-job--remove-hooks ()
  "Remove owner lifecycle hooks once no jobs remain active."
  (remove-hook 'lispm-state-change-functions #'lispm-job--state-changed)
  (remove-hook 'lispm-session-end-functions #'lispm-job--session-ended)
  (remove-hook 'lispm-agents-context-release-functions #'lispm-job--terminal-ended))

(defun lispm-job-register (owner kind cancel-fn &optional data)
  "Register a new active job of KIND owned by OWNER and return it.
CANCEL-FN is called with no arguments when the job is cancelled before
finishing.  Optional DATA associates caller state with the job."
  (let ((job (lispm-job--create :owner owner :kind kind :cancel-fn cancel-fn :data data)))
    (unless lispm-job--active
      (lispm-job--install-hooks))
    (push job lispm-job--active)
    job))

(defun lispm-job-finish (job)
  "Mark JOB finished and unregister it from `lispm-job--active'.
Return t if this call finished JOB, or nil if it was already finished."
  (unless (lispm-job-finished job)
    (setf (lispm-job-finished job) t)
    (setq lispm-job--active (delq job lispm-job--active))
    (unless lispm-job--active
      (lispm-job--remove-hooks))
    t))

(defun lispm-job-cancel (job)
  "Cancel JOB idempotently, running its cancellation callback at most once.
Return non-nil if this call cancelled an active job."
  (when (lispm-job-finish job)
    (let ((fn (lispm-job-cancel-fn job)))
      (when (functionp fn)
        (condition-case nil (funcall fn) ((error quit) nil))))
    t))

(defun lispm-job-cancel-owner (owner)
  "Cancel all active jobs belonging to OWNER."
  (dolist (job (copy-sequence lispm-job--active))
    (when (eq (lispm-job-owner job) owner)
      (lispm-job-cancel job))))

(defun lispm-job-cancel-kind (kind)
  "Cancel all active jobs of KIND."
  (dolist (job (copy-sequence lispm-job--active))
    (when (eq (lispm-job-kind job) kind)
      (lispm-job-cancel job))))

(defun lispm-job-cancel-all ()
  "Cancel every active job in `lispm-job--active'."
  (mapc #'lispm-job-cancel (copy-sequence lispm-job--active)))

(defun lispm-job-active (&optional owner kind)
  "Return active jobs matching optional OWNER and KIND."
  (seq-filter (lambda (job)
                (and (or (null owner) (eq (lispm-job-owner job) owner))
                     (or (null kind) (eq (lispm-job-kind job) kind))))
              lispm-job--active))

(defun lispm-job-active-count (&optional owner kind)
  "Return the count of active jobs matching optional OWNER and KIND."
  (length (lispm-job-active owner kind)))

(defun lispm-job--state-changed (agent)
  "Cancel AGENT's active jobs when its turn is aborted."
  (when (and (lispm-agent-p agent) (eq (lispm-agent-state agent) 'aborting))
    (lispm-job-cancel-owner agent)))

(defun lispm-job--session-ended (owner)
  "Cancel all active jobs belonging to OWNER when its session ends."
  (lispm-job-cancel-owner owner))

(defun lispm-job--terminal-ended (terminal)
  "Cancel active jobs belonging to the external principal in TERMINAL."
  (when (and (buffer-live-p terminal)
             (local-variable-p 'lispm-agents-bridge--agent terminal))
    (lispm-job-cancel-owner (buffer-local-value 'lispm-agents-bridge--agent terminal))))

(provide 'lispm-job)
;;; lispm-job.el ends here
