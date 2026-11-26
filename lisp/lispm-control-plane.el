;;; lispm-control-plane.el --- Editor Effect Host and kernel agent control plane -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; Bridges Emacs (acting as the Editor Effect Host) with `lispm-kernel'
;; CSP agent actors (`P_agents') over the typed S-exp protocol in
;; `lispm-control-protocol'.  Handles editor-only effects (`read_buffer',
;; `list_buffers', `edit_buffer', `edit_file', `write_file', `eval_elisp'),
;; interactive approval prompts, viewport-aware subscription throttling,
;; turn cancellation, pure-Elisp fallback when kernel actors are absent,
;; and lossless state/stream restoration across Emacs reconnects.
;;
;; Layer 2 module: strictly below Stage/Chat/Office UI (Layer 3-4).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-control-protocol)
(require 'lispm-api)
(require 'lispm-job)
(require 'lispm-tool-core)
(require 'lispm-tools)
(require 'lispm-turns)
(require 'lispm-approval)
(require 'lispm-runtime)

(declare-function lispm-kernel-live-p "lispm-kernel" (runtime))
(declare-function lispm-kernel-feature-p "lispm-kernel" (runtime feature))
(declare-function lispm-kernel-connect "lispm-kernel" (socket-path &optional host))
(declare-function lispm-kernel-detach "lispm-kernel" (runtime))
(declare-function lispm-kernel-send-control "lispm-kernel" (runtime form))
(declare-function lispm-kernel-exec--runtime "lispm-kernel-exec" (&optional job directory))

;;;; Customization and Hooks

(defcustom lispm-control-plane-mode 'auto
  "Control-plane execution backend for agent turns.
- `auto': use `lispm-kernel' CSP actors when live with `:agent-actors',
  otherwise fall back transparently to pure-Elisp turns.
- `kernel': route turns to `lispm-kernel' (or test transport override).
- `elisp': force pure-Elisp execution in Emacs."
  :type '(choice (const :tag "Auto (kernel when available, else Elisp)" auto)
                 (const :tag "Kernel control plane" kernel)
                 (const :tag "Pure-Elisp fallback" elisp))
  :group 'lispm)

(defcustom lispm-control-plane-effect-timeout 30
  "Default seconds before an unanswered editor effect times out on the host."
  :type 'integer
  :group 'lispm)

(defvar lispm-control-plane-runtime-function #'lispm-control-plane--default-runtime
  "Function returning live `lispm-kernel-runtime' for optional directory, or nil.")

(defvar lispm-control-plane-send-function nil
  "Optional transport override `(RUNTIME FORM)' for control-plane RPC tests.")

(defvar lispm-control-plane-approval-function nil
  "Optional handler `(AGENT CALL-ID TOOL INPUT REASONS SESSION)' for approvals.")

(defvar lispm-control-plane-event-functions nil
  "Hook run with `(SESSION EVENT-PLIST)' on each kernel `turn-event'.")

;;;; Session State

(cl-defstruct (lispm-control-session
               (:constructor lispm-control-session--create)
               (:copier nil))
  "Emacs-side state mirror and effect host registry for one agent session."
  id agent runtime
  (state :idle)
  (viewport :visible)
  (last-seq 0)
  turn-id
  (step 0)
  (active-effects (make-hash-table :test #'eql))
  (pending-approvals (make-hash-table :test #'equal))
  (applied-tools (make-hash-table :test #'equal))
  (applied-turns (make-hash-table :test #'equal))
  (stream-text "")
  (stream-thinking "")
  stream-tools
  tool-results
  turn-results
  snapshot
  resync-waiters)

(defvar lispm-control-plane--sessions (make-hash-table :test #'equal)
  "Map from session identifier string to `lispm-control-session'.")

(defvar lispm-control-plane--list-waiters nil
  "Callbacks waiting for the next `(agent-list ...)' frame.")

(defun lispm-control-plane--default-runtime (&optional directory)
  "Return the active kernel runtime for DIRECTORY, or nil."
  (and (fboundp 'lispm-kernel-exec--runtime)
       (lispm-kernel-exec--runtime :exec directory)))

(defun lispm-control-plane-runtime (&optional directory)
  "Return the configured control-plane kernel runtime for DIRECTORY."
  (and lispm-control-plane-runtime-function
       (funcall lispm-control-plane-runtime-function directory)))

(defun lispm-control-plane-active-p (&optional runtime)
  "Return non-nil when turns should run on `lispm-kernel' actors.
Optional RUNTIME overrides the lookup from `lispm-control-plane-runtime'."
  (unless (eq lispm-control-plane-mode 'elisp)
    (let ((rt (or runtime (lispm-control-plane-runtime))))
      (or (and (eq lispm-control-plane-mode 'kernel)
               lispm-control-plane-send-function
               t)
          (and rt
               (fboundp 'lispm-kernel-live-p)
               (lispm-kernel-live-p rt)
               (fboundp 'lispm-kernel-feature-p)
               (lispm-kernel-feature-p rt :agent-actors)
               t)))))

(defun lispm-control-plane-session-id (agent-or-id)
  "Return canonical string session ID for AGENT-OR-ID."
  (cond ((stringp agent-or-id) agent-or-id)
        ((lispm-agent-p agent-or-id) (lispm-agent-id agent-or-id))
        (t (format "%s" agent-or-id))))

(defun lispm-control-plane-session (agent-or-id &optional create runtime)
  "Return the `lispm-control-session' for AGENT-OR-ID.
When CREATE is non-nil, initialize a missing session bound to RUNTIME."
  (let* ((id (lispm-control-plane-session-id agent-or-id))
         (sess (gethash id lispm-control-plane--sessions)))
    (when (and (null sess) create)
      (setq sess (lispm-control-session--create
                  :id id
                  :agent (and (lispm-agent-p agent-or-id) agent-or-id)
                  :runtime runtime))
      (puthash id sess lispm-control-plane--sessions))
    (when sess
      (when (lispm-agent-p agent-or-id)
        (setf (lispm-control-session-agent sess) agent-or-id))
      (when runtime
        (setf (lispm-control-session-runtime sess) runtime)))
    sess))

(defun lispm-control-plane-reset ()
  "Cancel local effects/timers and clear tracked control-plane sessions."
  (maphash (lambda (_id sess) (lispm-control-plane--clear-local-effects sess))
           lispm-control-plane--sessions)
  (clrhash lispm-control-plane--sessions)
  (setq lispm-control-plane--list-waiters nil))

(defun lispm-control-plane--send (runtime form)
  "Send validated wire FORM over `lispm-control-plane-send-function' or RUNTIME."
  (let ((wire-form (lispm-control-protocol-assert-wire form)))
    (if lispm-control-plane-send-function
        (funcall lispm-control-plane-send-function runtime wire-form)
      (let ((rt (or runtime (lispm-control-plane-runtime))))
        (unless (and rt (fboundp 'lispm-kernel-send-control))
          (signal 'lispm-control-protocol-error
                  '("No live kernel runtime for control-plane command")))
        (lispm-kernel-send-control rt wire-form)))))

;;;; Outbound Control Operations & Pure-Elisp Fallback

(cl-defun lispm-control-plane-register-agent (agent &rest config)
  "Register AGENT with the kernel control plane using CONFIG plist."
  (let* ((rt (or (plist-get config :runtime)
                 (lispm-control-plane-runtime (and (lispm-agent-p agent)
                                                   (lispm-agent-root agent)))))
         (sess (lispm-control-plane-session agent t rt))
         (wire-config (cl-loop for (k v) on config by #'cddr
                               unless (eq k :runtime)
                               append (list k v))))
    (when (and (lispm-agent-p agent) (not (plist-member wire-config :root)))
      (setq wire-config (plist-put wire-config :root (lispm-agent-root agent))))
    (when (lispm-control-plane-active-p rt)
      (lispm-control-plane--send
       rt (lispm-control-protocol-agent-start (lispm-control-session-id sess) wire-config)))
    sess))

(cl-defun lispm-control-plane-start-turn (agent input &key config runtime fallback-fn)
  "Start a turn for AGENT with INPUT using kernel actors or Elisp fallback.
CONFIG may configure the session when registering lazily.  RUNTIME
overrides the target kernel runtime.  FALLBACK-FN, when provided, is
called as `(FALLBACK-FN AGENT INPUT)' in pure-Elisp fallback mode.
Return `kernel' when routed to `lispm-kernel', or `elisp' on fallback."
  (let ((rt (or runtime (lispm-control-plane-runtime (and (lispm-agent-p agent)
                                                        (lispm-agent-root agent))))))
    (if (lispm-control-plane-active-p rt)
        (let* ((sess (if config
                         (apply #'lispm-control-plane-register-agent
                                agent (append config (list :runtime rt)))
                       (lispm-control-plane-session agent t rt)))
               (payload (if (stringp input)
                            (list :body (lispm-json-encode `((messages . [((role . "user") (content . ,input))]))))
                          input)))
          (setf (lispm-control-session-stream-text sess) ""
                (lispm-control-session-stream-thinking sess) ""
                (lispm-control-session-stream-tools sess) nil)
          (when (lispm-agent-p agent)
            (lispm-set-state agent 'requesting "thinking" 'thinking))
          (lispm-control-plane--send
           rt (lispm-control-protocol-agent-turn (lispm-control-session-id sess) payload))
          'kernel)
      ;; Pure-Elisp fallback mode
      (cond
       (fallback-fn
        (funcall fallback-fn agent input)
        'elisp)
       ((and (lispm-agent-p agent)
             (or (stringp input) (stringp (plist-get input :prompt))))
        (lispm-tell agent (or (and (stringp input) input) (plist-get input :prompt)))
        'elisp)
       ((lispm-agent-p agent)
        (lispm-turns-begin agent)
        'elisp)
       (t
        (signal 'lispm-control-protocol-error
                '("Pure-Elisp fallback requires a `lispm-agent' or :fallback-fn")))))))

(defun lispm-control-plane-cancel (agent-or-session)
  "Cancel active kernel turn and local editor effects for AGENT-OR-SESSION."
  (when-let* ((sess (lispm-control-plane-session agent-or-session nil)))
    (lispm-control-plane--clear-local-effects sess t)
    (clrhash (lispm-control-session-pending-approvals sess))
    (setf (lispm-control-session-state sess) :idle
          (lispm-control-session-turn-id sess) nil)
    (let ((rt (or (lispm-control-session-runtime sess) (lispm-control-plane-runtime))))
      (when (lispm-control-plane-active-p rt)
        (lispm-control-plane--send
         rt (lispm-control-protocol-agent-cancel (lispm-control-session-id sess)))))
    t))

(defun lispm-control-plane-set-viewport (agent visible-p)
  "Idempotently notify the kernel whether AGENT's chat is VISIBLE-P."
  (when-let* ((sess (lispm-control-plane-session agent nil)))
    (let ((mode (if visible-p :visible :hidden))
          (rt (or (lispm-control-session-runtime sess) (lispm-control-plane-runtime))))
      (when (and (not (eq mode (lispm-control-session-viewport sess)))
                 (lispm-control-plane-active-p rt))
        (setf (lispm-control-session-viewport sess) mode)
        (lispm-control-plane--send
         rt (lispm-control-protocol-viewport-subscribe
             (lispm-control-session-id sess) mode))
        t))))

(defun lispm-control-plane-resolve-approval (agent-or-session call-id decision &optional reason)
  "Send approval DECISION (`approve', `allow', `deny') with REASON for CALL-ID.
AGENT-OR-SESSION identifies the session awaiting approval."
  (when-let* ((sess (lispm-control-plane-session agent-or-session nil)))
    (when (gethash call-id (lispm-control-session-pending-approvals sess))
      (remhash call-id (lispm-control-session-pending-approvals sess))
      (let ((wire-decision (pcase decision
                             ((or 'approve :approve) :approve)
                             ((or 'allow :allow) :allow)
                             ((or 'deny :deny 'reject :reject) :deny)
                             (_ :deny)))
            (rt (or (lispm-control-session-runtime sess) (lispm-control-plane-runtime))))
        (when (lispm-control-plane-active-p rt)
          (lispm-control-plane--send
           rt (lispm-control-protocol-approval-decision
               (lispm-control-session-id sess) call-id wire-decision reason))))
      t)))

;;;; Editor Effect Host (`read_buffer', `edit_buffer', `eval_elisp', tools)

(defun lispm-control-plane--clear-local-effects (sess &optional notify-p)
  "Cancel all active local host effects and timers tracked on SESS.
When NOTIFY-P is non-nil, send `:cancelled' replies for each active effect."
  (let (ids)
    (maphash (lambda (eid _entry) (push eid ids)) (lispm-control-session-active-effects sess))
    (dolist (eid ids)
      (when-let* ((entry (gethash eid (lispm-control-session-active-effects sess))))
        (if notify-p
            (progn
              (lispm-control-plane--send-effect-reply
               sess eid :cancelled "Editor effect cancelled")
              (when-let* ((job (plist-get entry :job)))
                (lispm-job-cancel job)))
          (plist-put entry :settled t)
          (remhash eid (lispm-control-session-active-effects sess))
          (when-let* ((timer (plist-get entry :timer)))
            (cancel-timer timer))
          (when-let* ((job (plist-get entry :job)))
            (lispm-job-cancel job)))))))

(defun lispm-control-plane--resolve-tool-name (kind payload)
  "Resolve registered `lispm-tool' name for effect KIND and PAYLOAD."
  (or (plist-get payload :tool)
      (pcase kind
        (:read-buffer "read_buffer")
        (:list-buffers "list_buffers")
        (:edit-buffer "edit_buffer")
        (:eval-elisp "eval_elisp")
        (_ nil))))

(defun lispm-control-plane--send-effect-reply (sess effect-id status result)
  "Idempotently settle EFFECT-ID on SESS and send `agent-effect-reply'."
  (let ((entry (gethash effect-id (lispm-control-session-active-effects sess))))
    (when (and entry (not (plist-get entry :settled)))
      (plist-put entry :settled t)
      (remhash effect-id (lispm-control-session-active-effects sess))
      (when-let* ((timer (plist-get entry :timer)))
        (cancel-timer timer))
      (when-let* ((job (plist-get entry :job)))
        (lispm-job-finish job))
      (let ((rt (or (lispm-control-session-runtime sess) (lispm-control-plane-runtime))))
        (when (lispm-control-plane-active-p rt)
          (lispm-control-plane--send
           rt (lispm-control-protocol-effect-reply
               (lispm-control-session-id sess) effect-id status result)))))))

(defun lispm-control-plane--handle-effect-request (runtime msg)
  "Execute `editor-effect-request' MSG on Emacs and reply cleanly over RUNTIME."
  (let* ((session-id (plist-get msg :session))
         (effect-id (plist-get msg :effect-id))
         (kind (plist-get msg :kind))
         (payload (plist-get msg :payload))
         (timeout (or (plist-get msg :timeout) lispm-control-plane-effect-timeout))
         (sess (lispm-control-plane-session session-id t runtime)))
    (unless (gethash effect-id (lispm-control-session-active-effects sess))
      (let* ((tool-name (lispm-control-plane--resolve-tool-name kind payload))
             (tool (and tool-name (lispm-get-tool tool-name)))
             (agent (lispm-control-session-agent sess))
             (owner (if (lispm-agent-p agent) agent session-id))
             (entry (list :id effect-id :tool tool-name :call-id (plist-get payload :call-id)))
             (timer nil)
             (job nil))
        (setq timer
              (run-at-time
               (max 0.05 timeout) nil
               (lambda ()
                 (when-let* ((cur (gethash effect-id (lispm-control-session-active-effects sess)))
                             ((not (plist-get cur :settled))))
                   (lispm-control-plane--send-effect-reply
                    sess effect-id :error
                    (format "Editor effect %s timed out after %ss" tool-name timeout))
                   (when-let* ((j (plist-get cur :job)))
                     (lispm-job-cancel j))))))
        (setq job
              (lispm-job-register
               owner 'editor-effect
               (lambda ()
                 (lispm-control-plane--send-effect-reply
                  sess effect-id :cancelled "Editor effect cancelled"))))
        (plist-put entry :timer timer)
        (plist-put entry :job job)
        (puthash effect-id entry (lispm-control-session-active-effects sess))
        (if (not tool)
            (lispm-control-plane--send-effect-reply
             sess effect-id :error (format "No editor tool named %s" tool-name))
          (let* ((raw-json (or (plist-get payload :input-json) "{}"))
                 (decoded (condition-case err
                              (if (string-empty-p (string-trim raw-json))
                                  nil
                                (lispm-json-decode raw-json))
                            (error (cons :json-error (error-message-string err))))))
            (if (eq (car-safe decoded) :json-error)
                (lispm-control-plane--send-effect-reply
                 sess effect-id :error (format "Invalid JSON input: %s" (cdr decoded)))
              (condition-case err
                  (let ((lispm-current-agent (and (lispm-agent-p agent) agent))
                        (default-directory (if (lispm-agent-p agent)
                                               (lispm-agent-root agent)
                                             default-directory)))
                    (lispm-tool-invoke
                     tool decoded
                     (lambda (text errorp &optional _images)
                       (lispm-control-plane--send-effect-reply
                        sess effect-id (if errorp :error :ok) text))))
                (error
                 (lispm-control-plane--send-effect-reply
                  sess effect-id :error (error-message-string err)))))))))))

;;;; Inbound Event Application, Approvals, and Reconnect/Resync

(defun lispm-control-plane--handle-approval-request (runtime msg)
  "Record `approval-request' MSG from RUNTIME on its session and notify hooks."
  (let* ((session-id (plist-get msg :session))
         (call-id (plist-get msg :call-id))
         (tool (plist-get msg :tool))
         (input-json (plist-get msg :input-json))
         (reasons (plist-get msg :reasons))
         (sess (lispm-control-plane-session session-id t runtime))
         (input (condition-case nil (lispm-json-decode input-json) (error nil))))
    (puthash call-id (list :call-id call-id :tool tool :input-json input-json :reasons reasons)
             (lispm-control-session-pending-approvals sess))
    (when (lispm-agent-p (lispm-control-session-agent sess))
      (lispm-set-state (lispm-control-session-agent sess) 'acting "awaiting approval" 'waiting))
    (when lispm-control-plane-approval-function
      (funcall lispm-control-plane-approval-function
               (lispm-control-session-agent sess) call-id tool input reasons sess))))

(defun lispm-control-plane--apply-event (sess event)
  "Apply one kernel `turn-event' plist EVENT idempotently onto SESS."
  (let ((seq (plist-get event :seq))
        (kind (plist-get event :type))
        (agent (lispm-control-session-agent sess)))
    (unless (and (integerp seq) (<= seq (lispm-control-session-last-seq sess)))
      (when (integerp seq)
        (setf (lispm-control-session-last-seq sess) seq))
      (pcase kind
        (:turn-started
         (setf (lispm-control-session-state sess) :streaming
               (lispm-control-session-turn-id sess) (plist-get event :turn-id)
               (lispm-control-session-step sess) (or (plist-get event :step) 1)
               (lispm-control-session-stream-text sess) ""
               (lispm-control-session-stream-thinking sess) ""
               (lispm-control-session-stream-tools sess) nil)
         (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
           (unless (lispm-live-region (lispm-agent-buffer agent))
             (lispm-turns-live-begin agent))))
        (:delta
         (let ((dkind (pcase (plist-get event :kind)
                        (:text 'text)
                        (:thinking 'thinking)
                        (:tool 'tool)
                        (other other)))
               (text (or (plist-get event :text) "")))
           (pcase dkind
             ('text (setf (lispm-control-session-stream-text sess)
                          (concat (lispm-control-session-stream-text sess) text)))
             ('thinking (setf (lispm-control-session-stream-thinking sess)
                              (concat (lispm-control-session-stream-thinking sess) text)))
             ('tool (push text (lispm-control-session-stream-tools sess))))
           (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
             (unless (lispm-live-region (lispm-agent-buffer agent))
               (lispm-turns-live-begin agent))
             (lispm-turns-live-insert agent dkind text))))
        (:tool-result
         (let ((key (format "%s:%s:%s"
                            (plist-get event :turn-id)
                            (plist-get event :step)
                            (plist-get event :call-id))))
           (unless (gethash key (lispm-control-session-applied-tools sess))
             (puthash key t (lispm-control-session-applied-tools sess))
             (setf (lispm-control-session-tool-results sess)
                   (append (lispm-control-session-tool-results sess) (list event))))))
        (:effect-timeout
         (let ((eid (plist-get event :effect-id)))
           (when-let* ((entry (gethash eid (lispm-control-session-active-effects sess))))
             (plist-put entry :settled t)
             (remhash eid (lispm-control-session-active-effects sess))
             (when-let* ((timer (plist-get entry :timer)))
               (cancel-timer timer))
             (when-let* ((job (plist-get entry :job)))
               (lispm-job-cancel job)))))
        ((or :turn-done :turn-cancelled :turn-error :step-limit)
         (let ((key (format "%s:%s" (plist-get event :turn-id) kind)))
           (setf (lispm-control-session-state sess) :idle
                 (lispm-control-session-turn-id sess) nil)
           (clrhash (lispm-control-session-pending-approvals sess))
           (unless (gethash key (lispm-control-session-applied-turns sess))
             (puthash key t (lispm-control-session-applied-turns sess))
             (setf (lispm-control-session-turn-results sess)
                   (append (lispm-control-session-turn-results sess) (list event))))
           (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
             (lispm-turns-live-delete agent)
             (lispm-set-state agent 'idle)))))
      (run-hook-with-args 'lispm-control-plane-event-functions sess event)
      t)))

(defun lispm-control-plane--restore-live-buffer (sess snapshot)
  "Synchronize AGENT's live streaming buffer in SESS from SNAPSHOT when active."
  (when-let* ((agent (lispm-control-session-agent sess))
              ((lispm-agent-p agent))
              (buf (lispm-agent-buffer agent))
              ((buffer-live-p buf))
              ((eq (plist-get snapshot :state) :streaming)))
    (let ((stream (plist-get snapshot :stream)))
      (lispm-turns-live-delete agent)
      (lispm-turns-live-begin agent)
      (let ((thinking (plist-get stream :thinking))
            (text (plist-get stream :text)))
        (when (and (stringp thinking) (not (string-empty-p thinking)))
          (lispm-turns-live-insert agent 'thinking thinking))
        (when (and (stringp text) (not (string-empty-p text)))
          (lispm-turns-live-insert agent 'text text)))
      (lispm-set-state agent 'requesting "streaming" 'thinking))))

(defun lispm-control-plane--handle-snapshot (runtime msg)
  "Apply reconnect `agent-snapshot' MSG over RUNTIME to restore session state."
  (let* ((session-id (plist-get msg :session))
         (snap (plist-get msg :snapshot))
         (sess (lispm-control-plane-session session-id t runtime)))
    (setf (lispm-control-session-snapshot sess) snap)
    (unless (eq (plist-get snap :state) :missing)
      ;; Replay missed incremental events before settling snapshot summary
      (dolist (ev (plist-get snap :events))
        (lispm-control-plane--apply-event sess ev))
      (dolist (tool-ev (plist-get snap :completed-tools))
        (let ((key (format "%s:%s:%s"
                           (plist-get tool-ev :turn-id)
                           (plist-get tool-ev :step)
                           (plist-get tool-ev :call-id))))
          (unless (gethash key (lispm-control-session-applied-tools sess))
            (puthash key t (lispm-control-session-applied-tools sess))
            (setf (lispm-control-session-tool-results sess)
                  (append (lispm-control-session-tool-results sess) (list tool-ev))))))
      (dolist (turn-ev (plist-get snap :completed-turns))
        (let* ((status-kw (pcase (plist-get turn-ev :status)
                            (:done :turn-done)
                            (:cancelled :turn-cancelled)
                            (:error :turn-error)
                            (:step-limit :step-limit)
                            (other other)))
               (key (format "%s:%s" (plist-get turn-ev :turn-id) status-kw)))
          (unless (gethash key (lispm-control-session-applied-turns sess))
            (puthash key t (lispm-control-session-applied-turns sess))
            (setf (lispm-control-session-turn-results sess)
                  (append (lispm-control-session-turn-results sess)
                          (list (plist-put (copy-sequence turn-ev) :type status-kw)))))))
      (let ((stream (plist-get snap :stream)))
        (setf (lispm-control-session-state sess) (or (plist-get snap :state) :idle)
              (lispm-control-session-turn-id sess) (plist-get snap :turn-id)
              (lispm-control-session-step sess) (or (plist-get snap :step) 0)
              (lispm-control-session-last-seq sess)
              (max (lispm-control-session-last-seq sess) (or (plist-get snap :event-seq) 0))
              (lispm-control-session-stream-text sess) (or (plist-get stream :text) "")
              (lispm-control-session-stream-thinking sess) (or (plist-get stream :thinking) "")
              (lispm-control-session-stream-tools sess) (plist-get stream :tools)))
      (clrhash (lispm-control-session-pending-approvals sess))
      (dolist (ap (plist-get snap :pending-approvals))
        (puthash (plist-get ap :call-id) ap (lispm-control-session-pending-approvals sess)))
      (lispm-control-plane--restore-live-buffer sess snap))
    (let ((waiters (nreverse (lispm-control-session-resync-waiters sess))))
      (setf (lispm-control-session-resync-waiters sess) nil)
      (dolist (fn waiters)
        (funcall fn sess snap)))))

(defun lispm-control-plane-handle-frame (runtime frame)
  "Handle an inbound `lispm-kernel' control-plane FRAME over RUNTIME."
  (let ((parsed (lispm-control-protocol-parse-frame frame)))
    (pcase (plist-get parsed :frame)
      ('turn-event
       (let ((sess (lispm-control-plane-session (plist-get parsed :session) t runtime)))
         (lispm-control-plane--apply-event sess (plist-get parsed :event))))
      ('editor-effect-request
       (lispm-control-plane--handle-effect-request runtime parsed))
      ('approval-request
       (lispm-control-plane--handle-approval-request runtime parsed))
      ('agent-snapshot
       (lispm-control-plane--handle-snapshot runtime parsed))
      ('agent-list
       (let ((sessions (plist-get parsed :sessions))
             (waiters (nreverse lispm-control-plane--list-waiters)))
         (setq lispm-control-plane--list-waiters nil)
         (dolist (fn waiters)
           (funcall fn sessions)))))))

;;;; Reconnect and Resync API

(defun lispm-control-plane-connect (socket-path &optional host)
  "Connect to a persistent `lispm-kernel' Unix socket at SOCKET-PATH on HOST."
  (unless (and (require 'lispm-kernel nil t) (fboundp 'lispm-kernel-connect))
    (error "Socket reconnect requires `lispm-kernel'"))
  (lispm-kernel-connect socket-path host))

(defun lispm-control-plane-detach (runtime)
  "Detach from persistent socket RUNTIME while keeping kernel actors running."
  (when (fboundp 'lispm-kernel-detach)
    (lispm-kernel-detach runtime)))

(defun lispm-control-plane-resync (agent-or-session &optional callback runtime)
  "Request live snapshot and missed events for AGENT-OR-SESSION over RUNTIME.
Optional CALLBACK is invoked with `(SESSION SNAPSHOT)' when arrived."
  (let* ((rt (or runtime (lispm-control-plane-runtime)))
         (sess (lispm-control-plane-session agent-or-session t rt)))
    (when callback
      (push callback (lispm-control-session-resync-waiters sess)))
    (when (lispm-control-plane-active-p rt)
      (lispm-control-plane--send
       rt (lispm-control-protocol-agent-resync
           (lispm-control-session-id sess)
           (lispm-control-session-last-seq sess))))
    sess))

(defun lispm-control-plane-reconnect (runtime &optional sessions callback)
  "Reattach RUNTIME across known or listed SESSIONS and restore their state.
CALLBACK, when non-nil, receives each `(SESSION SNAPSHOT)' as it resyncs."
  (let ((known (or sessions
                   (hash-table-keys lispm-control-plane--sessions))))
    (maphash (lambda (_id sess) (setf (lispm-control-session-runtime sess) runtime))
             lispm-control-plane--sessions)
    (if known
        (dolist (id known)
          (lispm-control-plane-resync id callback runtime))
      (push (lambda (items)
              (dolist (item items)
                (lispm-control-plane-resync (plist-get item :session) callback runtime)))
            lispm-control-plane--list-waiters)
      (lispm-control-plane--send runtime (lispm-control-protocol-agent-list)))))

;;;; Integration Hooks

(defun lispm-control-plane--on-resolve (agent id decision &optional reason)
  "Bridge interactive `lispm-resolve' on AGENT CALL ID with DECISION and REASON."
  (lispm-control-plane-resolve-approval agent id decision reason))

(add-hook 'lispm-abort-functions #'lispm-control-plane-cancel)
(add-hook 'lispm-session-end-functions #'lispm-control-plane-cancel)
(add-hook 'lispm-resolve-functions #'lispm-control-plane--on-resolve)

(with-eval-after-load 'lispm-kernel
  (defvar lispm-kernel-event-functions)
  (add-hook 'lispm-kernel-event-functions #'lispm-control-plane-handle-frame))

(provide 'lispm-control-plane)
;;; lispm-control-plane.el ends here
