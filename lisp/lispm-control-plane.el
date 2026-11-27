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
;; `lispm-control-protocol'.  Routes real `lispm-turns' requests to
;; `P_agents' when active, persists full Org transcripts and token usage,
;; delegates approvals through `lispm-tool-decision' and `lispm-resolve',
;; executes editor-only effects (`read_buffer', `list_buffers',
;; `edit_buffer', `edit_file', `write_file', `eval_elisp'), synchronizes
;; viewport throttling, falls back transparently to pure Elisp, and
;; restores live state across Emacs socket reconnects.
;;
;; Layer 2 module: strictly below Stage/Chat/Office UI (Layer 3-4).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-transcript)
(require 'lispm-session)
(require 'lispm-sandbox)
(require 'lispm-control-protocol)
(require 'lispm-api)
(require 'lispm-job)
(require 'lispm-tool-core)
(require 'lispm-tool-permissions)
(require 'lispm-tools)
(require 'lispm-turns)
(require 'lispm-approval)
(require 'lispm-runtime)

(declare-function lispm-kernel-live-p "lispm-kernel" (runtime))
(declare-function lispm-kernel-feature-p "lispm-kernel" (runtime feature))
(declare-function lispm-kernel-runtime-hello "lispm-kernel" (runtime))
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

;;;; Session & Effect State

(cl-defstruct (lispm-control-effect
               (:constructor lispm-control-effect--create)
               (:copier nil))
  "One live editor-side effect invocation tracked until reply or cancel."
  id tool call-id timer job settled)

(cl-defstruct (lispm-control-session
               (:constructor lispm-control-session--create)
               (:copier nil))
  "Emacs-side state mirror and effect host registry for one agent session."
  id agent runtime epoch turns-owned
  (state :idle)
  (viewport :visible)
  (last-seq 0)
  turn-id
  (step 0)
  (active-effects (make-hash-table :test #'eql))
  (pending-approvals (make-hash-table :test #'equal))
  (call-markers (make-hash-table :test #'equal))
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
        ((lispm-control-session-p agent-or-id) (lispm-control-session-id agent-or-id))
        ((lispm-agent-p agent-or-id) (lispm-agent-id agent-or-id))
        (t (format "%s" agent-or-id))))

(defun lispm-control-plane--find-live-agent (id)
  "Return registered `lispm-agent' whose id equals ID, or nil."
  (cl-find-if (lambda (ag) (equal (lispm-agent-id ag) id)) (lispm-agents)))

(defun lispm-control-plane-session (agent-or-id &optional create runtime)
  "Return the `lispm-control-session' for AGENT-OR-ID.
When CREATE is non-nil, initialize a missing session bound to RUNTIME."
  (let* ((id (lispm-control-plane-session-id agent-or-id))
         (sess (gethash id lispm-control-plane--sessions))
         (resolved-agent (or (and (lispm-agent-p agent-or-id) agent-or-id)
                             (and sess (lispm-control-session-agent sess))
                             (lispm-control-plane--find-live-agent id))))
    (when (and (null sess) create)
      (setq sess (lispm-control-session--create
                  :id id
                  :agent resolved-agent
                  :runtime runtime))
      (puthash id sess lispm-control-plane--sessions))
    (when sess
      (when (and resolved-agent (null (lispm-control-session-agent sess)))
        (setf (lispm-control-session-agent sess) resolved-agent))
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

;;;; Policy Derivation & Host Pre-flight

(defun lispm-control-plane--tool-policy (agent tool root)
  "Compute `(NAME :host HOST :approval APPROVAL)' for AGENT's TOOL in ROOT."
  (let* ((name (lispm-tool-name tool))
         (perm (lispm-tool-permission tool))
         (action (lispm-policy-action agent perm))
         (sandboxed (not (eq lispm-sandbox-level 'none)))
         (remote (and (stringp root) (file-remote-p root)))
         (host (cond
                ((and (string= name "run_shell") sandboxed) 'editor)
                (remote 'editor)
                (t (lispm-control-protocol-tool-host name))))
         (approval
          (cond
           ((eq action 'deny) :deny)
           ;; Defer to full `lispm-tool-decision' on Emacs whenever argument inspection,
           ;; action hooks, sandbox/write rules, or `ask' policy apply.
           ((or (eq action 'ask)
                lispm-tool-action-functions
                (lispm-tool-always-ask tool)
                (eq perm 'write)
                (eq perm 'shell)
                (eq perm 'exec))
            :ask)
           ((eq action 'auto) :auto)
           (t :ask))))
    (list name :host (if (eq host 'kernel) :kernel :editor) :approval approval)))

(defun lispm-control-plane--agent-policies (agent)
  "Build kernel `:tool-policies' alist for AGENT's currently offered tools."
  (let ((root (lispm-agent-root agent)))
    (mapcar (lambda (tool) (lispm-control-plane--tool-policy agent tool root))
            (lispm-agent-tools* agent))))

(defun lispm-control-plane--unsaved-files (root)
  "Return local paths of modified buffers under ROOT forcing editor fallback."
  (when (and (stringp root) (not (file-remote-p root)))
    (let (files)
      (dolist (buf (buffer-list))
        (when-let* ((file (buffer-file-name buf))
                    ((buffer-modified-p buf))
                    ((not (file-remote-p file)))
                    ((file-in-directory-p file root)))
          (push (expand-file-name file) files)))
      (nreverse files))))

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
          (condition-case err
              (lispm-control-plane--send
               rt (lispm-control-protocol-agent-turn (lispm-control-session-id sess) payload))
            (quit
             (when (lispm-agent-p agent)
               (lispm-turns-abort-request agent "Interrupted." t))
             (signal 'quit nil))
            (error
             (when (lispm-agent-p agent)
               (lispm-turns-abort-request agent (error-message-string err)))
             (signal (car err) (cdr err))))
          'kernel)
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

(defun lispm-control-plane--take-request (agent request)
  "Intercept `lispm-turns-request' for AGENT with REQUEST into kernel `P_agents'.
Return non-nil when handled by the kernel control plane, else nil."
  (when (and (eq lispm-api-function #'lispm-api-stream)
             (lispm-agent-p agent)
             (buffer-live-p (lispm-agent-buffer agent)))
    (let* ((root (lispm-agent-root agent))
           (rt (lispm-control-plane-runtime root))
           (backend (lispm-agent-backend* agent))
           (dialect (or (plist-get backend :api) 'anthropic)))
      (when (and (not (file-remote-p root))
                 (memq dialect '(anthropic openai))
                 (lispm-control-plane-active-p rt))
        (let* ((handled nil)
               (prep (condition-case err
                         (lispm-api-prepare-request backend request)
                       (quit
                        (setq handled t)
                        (lispm-turns-abort-request agent "Interrupted." t)
                        nil)
                       (error
                        (setq handled t)
                        (lispm-turns-abort-request agent (error-message-string err))
                        nil))))
          (when prep
            (let* ((view (lispm-agent-view agent))
                   (visible-p (or (not (buffer-live-p view))
                                  (and (fboundp 'get-buffer-window)
                                       (get-buffer-window view 'visible))))
                   (policies (lispm-control-plane--agent-policies agent))
                   (unsaved (lispm-control-plane--unsaved-files root))
                   (remaining (max 1 (- (or (bound-and-true-p lispm-max-steps) 16)
                                        (max 0 (1- (lispm-agent-steps agent))))))
                   (sess nil))
              (condition-case err
                  (progn
                    (setq sess (lispm-control-plane-register-agent
                                agent
                                :runtime rt
                                :dialect (if (eq dialect 'openai) :openai :anthropic)
                                :url (plist-get prep :url)
                                :headers (plist-get prep :headers)
                                :root root
                                :max-steps remaining
                                :viewport (if visible-p :visible :hidden)
                                :tool-policies policies))
                    (setf (lispm-control-session-turns-owned sess) t
                          (lispm-control-session-stream-text sess) ""
                          (lispm-control-session-stream-thinking sess) ""
                          (lispm-control-session-stream-tools sess) nil)
                    (clrhash (lispm-control-session-call-markers sess))
                    (lispm-set-state agent 'requesting "thinking" 'thinking)
                    (lispm-turns-live-begin agent)
                    (lispm-control-plane--send
                     rt (lispm-control-protocol-agent-turn
                         (lispm-control-session-id sess)
                         (list :turn-id (format "t%d-%d" (lispm-agent-turn agent) (lispm-agent-steps agent))
                               :url (plist-get prep :url)
                               :headers (plist-get prep :headers)
                               :max-steps remaining
                               :unsaved-files unsaved
                               :body (plist-get prep :body))))
                    (setq handled t))
                (quit
                 (when sess (setf (lispm-control-session-turns-owned sess) nil))
                 (lispm-turns-abort-request agent "Interrupted." t)
                 (setq handled t))
                (error
                 (when sess (setf (lispm-control-session-turns-owned sess) nil))
                 (lispm-turns-abort-request agent (error-message-string err))
                 (setq handled t)))))
          handled)))))

(defun lispm-control-plane-cancel (agent-or-session)
  "Cancel active kernel turn and local editor effects for AGENT-OR-SESSION."
  (when-let* ((sess (lispm-control-plane-session agent-or-session nil)))
    (let ((rt (or (lispm-control-session-runtime sess) (lispm-control-plane-runtime))))
      ;; Cancel on kernel BEFORE cleaning local effects so P_agents does not
      ;; advance the turn from an effect cancellation reply before :agent-cancel.
      (when (lispm-control-plane-active-p rt)
        (lispm-control-plane--send
         rt (lispm-control-protocol-agent-cancel (lispm-control-session-id sess))))
      (lispm-control-plane--clear-local-effects sess nil)
      (clrhash (lispm-control-session-pending-approvals sess))
      (clrhash (lispm-control-session-call-markers sess))
      (setf (lispm-control-session-turns-owned sess) nil
            (lispm-control-session-state sess) :idle
            (lispm-control-session-turn-id sess) nil))
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
              (when-let* ((job (lispm-control-effect-job entry)))
                (lispm-job-cancel job)))
          (setf (lispm-control-effect-settled entry) t)
          (remhash eid (lispm-control-session-active-effects sess))
          (when-let* ((timer (lispm-control-effect-timer entry)))
            (cancel-timer timer))
          (when-let* ((job (lispm-control-effect-job entry)))
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
    (when (and entry (not (lispm-control-effect-settled entry)))
      (setf (lispm-control-effect-settled entry) t)
      (remhash effect-id (lispm-control-session-active-effects sess))
      (when-let* ((timer (lispm-control-effect-timer entry)))
        (cancel-timer timer))
      (when-let* ((job (lispm-control-effect-job entry)))
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
             (entry (lispm-control-effect--create
                     :id effect-id :tool tool-name :call-id (plist-get payload :call-id))))
        (setf (lispm-control-effect-timer entry)
              (run-at-time
               (max 0.05 timeout) nil
               (lambda ()
                 (when-let* ((cur (gethash effect-id (lispm-control-session-active-effects sess)))
                             ((not (lispm-control-effect-settled cur))))
                   (lispm-control-plane--send-effect-reply
                    sess effect-id :error
                    (format "Editor effect %s timed out after %ss" tool-name timeout))
                   (when-let* ((j (lispm-control-effect-job cur)))
                     (lispm-job-cancel j))))))
        (setf (lispm-control-effect-job entry)
              (lispm-job-register
               owner 'editor-effect
               (lambda ()
                 (lispm-control-plane--send-effect-reply
                  sess effect-id :cancelled "Editor effect cancelled"))))
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

;;;; Inbound Event Application, Approvals, Transcript & Reconnect

(defun lispm-control-plane--prompt-agent-approval (sess call-id tool-name tool-obj input reasons)
  "Consult `lispm-tool-decision' for SESS/CALL-ID and either settle or ask user.
TOOL-NAME and TOOL-OBJ identify the tool; INPUT is its decoded JSON
alist and REASONS lists kernel-supplied policy explanations."
  (let* ((agent (lispm-control-session-agent sess))
         (decision (if tool-obj
                       (lispm-tool-decision agent tool-obj input)
                     (cons 'ask reasons)))
         (action (car decision))
         (why (or (cdr decision) reasons)))
    (pcase action
      ('auto
       (lispm-control-plane-resolve-approval sess call-id 'approve))
      ('deny
       (let ((msg (if tool-obj
                      (lispm-tools-denial agent tool-obj why)
                    (or (car why) "Denied by policy."))))
         (when-let* ((marker (gethash call-id (lispm-control-session-call-markers sess)))
                     ((marker-buffer marker)))
           (lispm-in-session agent (lispm-transcript-set-state marker "DENIED")))
         (lispm-control-plane-resolve-approval sess call-id 'deny msg)))
      (_
       (let* ((marker (gethash call-id (lispm-control-session-call-markers sess)))
              (call (list :status 'ask
                          :kernel-approval t
                          :turn (lispm-agent-turn agent)
                          :name tool-name
                          :tool tool-obj
                          :input input
                          :marker marker
                          :reasons why)))
         (puthash call-id call (lispm-agent-pending agent))
         (when (and marker (marker-buffer marker))
           (lispm-in-session agent (lispm-transcript-set-state marker "WAITING")))
         (lispm-set-state agent 'waiting)
         (lispm-turns-update-state agent)
         (when lispm-runtime-ask-approval-function
           (funcall lispm-runtime-ask-approval-function agent call-id)))))))

(defun lispm-control-plane--handle-approval-request (runtime msg)
  "Record `approval-request' MSG from RUNTIME on its session and notify hooks."
  (let* ((session-id (plist-get msg :session))
         (call-id (plist-get msg :call-id))
         (tool (plist-get msg :tool))
         (input-json (plist-get msg :input-json))
         (reasons (plist-get msg :reasons))
         (sess (lispm-control-plane-session session-id t runtime))
         (agent (lispm-control-session-agent sess))
         (input (condition-case nil (lispm-json-decode input-json) (error nil))))
    (puthash call-id (list :call-id call-id :tool tool :input-json input-json :reasons reasons)
             (lispm-control-session-pending-approvals sess))
    (cond
     (lispm-control-plane-approval-function
      (when (lispm-agent-p agent)
        (lispm-set-state agent 'waiting "awaiting approval" 'waiting))
      (funcall lispm-control-plane-approval-function
               agent call-id tool input reasons sess))
     ((lispm-agent-p agent)
      (lispm-control-plane--prompt-agent-approval
       sess call-id tool (lispm-get-tool tool) input reasons)))))

(defun lispm-control-plane--clear-kernel-pending (agent)
  "Remove kernel-managed pending approval entries from AGENT."
  (when (lispm-agent-p agent)
    (let (drop)
      (maphash (lambda (id call)
                 (when (plist-get call :kernel-approval) (push id drop)))
               (lispm-agent-pending agent))
      (dolist (id drop)
        (remhash id (lispm-agent-pending agent))))))

(defun lispm-control-plane--apply-step-response (sess event)
  "Persist kernel `:step-response' EVENT onto SESS's Org session buffer."
  (let* ((agent (lispm-control-session-agent sess))
         (step (or (plist-get event :step) 1)))
    (setf (lispm-control-session-step sess) step)
    (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
      (lispm-turns-live-delete agent)
      (when (> step 1)
        (cl-incf (lispm-agent-steps agent)))
      (let* ((msg (plist-get (lispm-control-protocol-decode-llm-result
                              (list :ok t
                                    :status 200
                                    :stop-reason (or (plist-get event :stop-reason) "end_turn")
                                    :usage (plist-get event :usage)
                                    :invalid (plist-get event :invalid)
                                    :blocks (plist-get event :blocks)))
                             :message))
             (content (append (alist-get 'content msg) nil))
             (uses (seq-filter (lambda (b) (equal (alist-get 'type b) "tool_use")) content))
             calls)
        (lispm-turns-record-usage agent (alist-get 'usage msg))
        (setf (lispm-agent-last-reply agent) (lispm-content-text content))
        (when content
          (setq calls (lispm-in-session agent
                        (lispm-session-newline)
                        (insert (lispm-transcript-heading 'assistant))
                        (lispm-transcript-insert-blocks content (lambda (_b) "RUNNING"))))
          (dolist (pair calls)
            (puthash (car pair) (cdr pair) (lispm-control-session-call-markers sess)))
          (lispm-save-session agent))
        (when uses
          (lispm-set-state agent 'acting))))))

(defun lispm-control-plane--apply-tool-result (sess event)
  "Record `:tool-result' EVENT onto SESS and update its Org transcript heading."
  (let* ((agent (lispm-control-session-agent sess))
         (call-id (plist-get event :call-id))
         (result (or (plist-get event :result) ""))
         (errorp (and (plist-get event :is-error) t))
         (key (format "%s:%s:%s"
                      (plist-get event :turn-id)
                      (plist-get event :step)
                      call-id)))
    (unless (gethash key (lispm-control-session-applied-tools sess))
      (puthash key t (lispm-control-session-applied-tools sess))
      (setf (lispm-control-session-tool-results sess)
            (append (lispm-control-session-tool-results sess) (list event)))
      (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
        (remhash call-id (lispm-agent-pending agent))
        (when-let* ((marker (gethash call-id (lispm-control-session-call-markers sess)))
                    ((marker-buffer marker)))
          (remhash call-id (lispm-control-session-call-markers sess))
          (lispm-in-session agent
            (unless (and errorp
                         (save-excursion
                           (goto-char marker)
                           (looking-at-p "^\\*+ DENIED ")))
              (lispm-transcript-set-state marker (if errorp "FAILED" "DONE")))
            (goto-char (lispm-transcript-subtree-end marker 2))
            (let ((start (point)))
              (insert (lispm-transcript-result
                       result errorp nil
                       (and buffer-file-name (file-name-directory buffer-file-name))))
              (when (> (cl-count ?\n result) 12)
                (lispm-transcript-fold start))))
          (lispm-save-session agent))))))

(defun lispm-control-plane--apply-terminal-event (sess kind event)
  "Finalize SESS and owned `lispm-agent' turn upon terminal EVENT of KIND."
  (let ((agent (lispm-control-session-agent sess))
        (turns-owned (lispm-control-session-turns-owned sess))
        (key (format "%s:%s" (plist-get event :turn-id) kind)))
    (setf (lispm-control-session-state sess) :idle
          (lispm-control-session-turn-id sess) nil
          (lispm-control-session-turns-owned sess) nil)
    (clrhash (lispm-control-session-pending-approvals sess))
    (clrhash (lispm-control-session-call-markers sess))
    (lispm-control-plane--clear-kernel-pending agent)
    (unless (gethash key (lispm-control-session-applied-turns sess))
      (puthash key t (lispm-control-session-applied-turns sess))
      (setf (lispm-control-session-turn-results sess)
            (append (lispm-control-session-turn-results sess) (list event))))
    (when (and (lispm-agent-p agent) (buffer-live-p (lispm-agent-buffer agent)))
      (lispm-turns-live-delete agent)
      (if (not turns-owned)
          (lispm-set-state agent 'idle)
        (pcase kind
          (:turn-done
           (let* ((stop (or (plist-get event :stop-reason) "end_turn"))
                  (outcome (lispm-turns-stop-outcome stop nil))
                  (note (car outcome))
                  (cond-type (cdr outcome)))
             (if (and cond-type lispm-runtime-signal-function)
                 (funcall lispm-runtime-signal-function agent cond-type note)
               (lispm-turns-finish agent note))))
          (:step-limit
           (if lispm-runtime-signal-function
               (funcall lispm-runtime-signal-function
                        agent 'step-limit
                        (format "Stopped after %d kernel steps." (or (plist-get event :step) 1)))
             (lispm-turns-finish agent "Step limit reached.")))
          (:turn-error
           (let ((msg (format "The request failed%s: %s: %s"
                              (if (and (plist-get event :status) (> (plist-get event :status) 0))
                                  (format " (HTTP %s)" (plist-get event :status))
                                "")
                              (or (plist-get event :error-type) "error")
                              (or (plist-get event :message) "Request failed"))))
             (if lispm-runtime-signal-function
                 (funcall lispm-runtime-signal-function agent 'request-failed msg)
               (lispm-turns-finish agent msg))))
          (:turn-cancelled
           (unless (eq (lispm-agent-state agent) 'idle)
             (lispm-turns-finish agent (plist-get event :reason)))))))))

(defun lispm-control-plane--sync-epoch (sess epoch)
  "Update SESS's daemon EPOCH, resetting `last-seq' when kernel restarted."
  (when (and (integerp epoch) (not (eql epoch (lispm-control-session-epoch sess))))
    (when (lispm-control-session-epoch sess)
      (setf (lispm-control-session-last-seq sess) 0))
    (setf (lispm-control-session-epoch sess) epoch)))

(defun lispm-control-plane--apply-event (sess event)
  "Apply one kernel `turn-event' plist EVENT idempotently onto SESS."
  (lispm-control-plane--sync-epoch sess (plist-get event :epoch))
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
        (:step-response
         (lispm-control-plane--apply-step-response sess event))
        (:tool-result
         (lispm-control-plane--apply-tool-result sess event))
        (:effect-timeout
         (let ((eid (plist-get event :effect-id)))
           (when-let* ((entry (gethash eid (lispm-control-session-active-effects sess))))
             (setf (lispm-control-effect-settled entry) t)
             (remhash eid (lispm-control-session-active-effects sess))
             (when-let* ((timer (lispm-control-effect-timer entry)))
               (cancel-timer timer))
             (when-let* ((job (lispm-control-effect-job entry)))
               (lispm-job-cancel job)))))
        ((or :turn-done :turn-cancelled :turn-error :step-limit)
         (lispm-control-plane--apply-terminal-event sess kind event)))
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

(defun lispm-control-plane--replay-snapshot-pending (runtime sess snap)
  "Re-dispatch missing approvals and effects on SESS from SNAP over RUNTIME."
  (clrhash (lispm-control-session-pending-approvals sess))
  (dolist (ap (plist-get snap :pending-approvals))
    (lispm-control-plane--handle-approval-request
     runtime (list :session (lispm-control-session-id sess)
                   :call-id (plist-get ap :call-id)
                   :tool (plist-get ap :tool)
                   :input-json (or (plist-get ap :input-json) "{}")
                   :reasons (plist-get ap :reasons))))
  (dolist (eff (plist-get snap :pending-effects))
    (let ((eid (plist-get eff :effect-id)))
      (unless (gethash eid (lispm-control-session-active-effects sess))
        (lispm-control-plane--handle-effect-request
         runtime
         (list :session (lispm-control-session-id sess)
               :effect-id eid
               :kind (or (plist-get eff :kind) :tool)
               :payload (list :call-id (plist-get eff :call-id)
                              :tool (plist-get eff :tool)
                              :input-json (or (plist-get eff :input-json) "{}"))))))))

(defun lispm-control-plane--handle-snapshot (runtime msg)
  "Apply reconnect `agent-snapshot' MSG over RUNTIME to restore session state."
  (let* ((session-id (plist-get msg :session))
         (snap (plist-get msg :snapshot))
         (sess (lispm-control-plane-session session-id t runtime)))
    (setf (lispm-control-session-snapshot sess) snap)
    (lispm-control-plane--sync-epoch sess (plist-get snap :epoch))
    (unless (eq (plist-get snap :state) :missing)
      (dolist (ev (plist-get snap :events))
        (lispm-control-plane--apply-event sess ev))
      (dolist (tool-ev (plist-get snap :completed-tools))
        (lispm-control-plane--apply-tool-result sess tool-ev))
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
      (lispm-control-plane--restore-live-buffer sess snap)
      (lispm-control-plane--replay-snapshot-pending runtime sess snap))
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

(defun lispm-control-plane--when-ready (runtime fn &optional deadline)
  "Invoke FN once RUNTIME finishes handshake, or immediately if ready."
  (let ((dl (or deadline (+ (float-time) 5.0))))
    (cond
     ((or lispm-control-plane-send-function
          (and runtime (fboundp 'lispm-kernel-runtime-hello) (lispm-kernel-runtime-hello runtime)))
      (when (lispm-control-plane-active-p runtime)
        (funcall fn)))
     ((and runtime (fboundp 'lispm-kernel-live-p) (lispm-kernel-live-p runtime)
           (< (float-time) dl))
      (run-at-time 0.01 nil #'lispm-control-plane--when-ready runtime fn dl)))))

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
    (lispm-control-plane--when-ready
     rt
     (lambda ()
       (when callback
         (push callback (lispm-control-session-resync-waiters sess)))
       (lispm-control-plane--send
        rt (lispm-control-protocol-agent-resync
            (lispm-control-session-id sess)
            (lispm-control-session-last-seq sess)))))
    sess))

(defun lispm-control-plane-reconnect (runtime &optional sessions callback)
  "Reattach RUNTIME across known or listed SESSIONS and restore their state.
CALLBACK, when non-nil, receives each `(SESSION SNAPSHOT)' as it resyncs."
  (let ((known (or sessions (hash-table-keys lispm-control-plane--sessions))))
    (maphash (lambda (id sess)
               (setf (lispm-control-session-runtime sess) runtime)
               (unless (lispm-control-session-agent sess)
                 (setf (lispm-control-session-agent sess)
                       (lispm-control-plane--find-live-agent id))))
             lispm-control-plane--sessions)
    (lispm-control-plane--when-ready
     runtime
     (lambda ()
       (if known
           (dolist (id known)
             (lispm-control-plane-resync id callback runtime))
         (push (lambda (items)
                 (dolist (item items)
                   (lispm-control-plane-resync (plist-get item :session) callback runtime)))
               lispm-control-plane--list-waiters)
         (lispm-control-plane--send runtime (lispm-control-protocol-agent-list)))))))

;;;; Integration Hooks

(defun lispm-control-plane--on-resolve (agent id decision &optional reason)
  "Bridge interactive `lispm-resolve' on AGENT CALL ID with DECISION and REASON."
  (lispm-control-plane-resolve-approval agent id decision reason))

(add-hook 'lispm-abort-functions #'lispm-control-plane-cancel)
(add-hook 'lispm-session-end-functions #'lispm-control-plane-cancel)
(add-hook 'lispm-resolve-functions #'lispm-control-plane--on-resolve)
(setq lispm-turns-control-plane-function #'lispm-control-plane--take-request)

(with-eval-after-load 'lispm-kernel
  (defvar lispm-kernel-event-functions)
  (add-hook 'lispm-kernel-event-functions #'lispm-control-plane-handle-frame))

(provide 'lispm-control-plane)
;;; lispm-control-plane.el ends here
