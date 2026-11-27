;;; lispm-control-protocol.el --- Typed RPC protocol between Emacs and kernel actors -*- lexical-binding: t; -*-

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

;; Versioned S-expression control-plane schema shared between Emacs
;; (Editor Effect Host) and `lispm-kernel' CSP agent actors (`P_agents'):
;;
;;   Emacs -> Kernel:
;;     (agent-start SESSION CONFIG)
;;     (agent-turn SESSION INPUT)
;;     (agent-cancel SESSION)
;;     (agent-effect-reply SESSION EFFECT-ID STATUS RESULT)
;;     (agent-approval-decision SESSION CALL-ID DECISION [:reason REASON])
;;     (agent-viewport-subscribe SESSION MODE)    ; :visible | :hidden
;;     (agent-resync SESSION [:since-seq SEQ])
;;     (agent-list)
;;     (agent-stop SESSION)
;;
;;   Kernel -> Emacs:
;;     (turn-event SESSION EVENT-PLIST)
;;     (editor-effect-request SESSION EFFECT-ID KIND PAYLOAD [:timeout N])
;;     (approval-request SESSION CALL-ID TOOL INPUT-JSON REASONS)
;;     (agent-snapshot SESSION SNAPSHOT-PLIST)
;;     (agent-list SESSION-SUMMARIES)
;;
;; Layer 1 pure module: zero upward dependencies on tools, turns, or UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-util)
(require 'lispm-core)

(defconst lispm-control-protocol-version 1
  "Control-plane wire protocol version matching `lispm-kernel' protocol 1.")

(define-error 'lispm-control-protocol-error
              "Invalid LispM control-plane frame"
              'lispm-error)

;;;; Tool Host Classification (Kernel vs Editor Effect Host)

(defconst lispm-control-protocol-editor-tools
  '("read_buffer" "list_buffers" "edit_buffer" "edit_file" "write_file" "eval_elisp")
  "Tools whose semantics depend on Emacs buffers, UI, or live editor state.")

(defconst lispm-control-protocol-kernel-tools
  '("run_shell" "search_files" "workspace_cmd" "read_file")
  "Tools that run off-thread inside `lispm-kernel' without blocking Emacs.")

(defun lispm-control-protocol-tool-host (name &optional context)
  "Classify execution host (`kernel' or `editor') for tool NAME.
Optional CONTEXT plist may carry `:policies', `:path', `:directory', or
`:unsaved-buffer-p'."
  (let* ((policies (plist-get context :policies))
         (override (and (listp policies) (cdr (assoc name policies))))
         (explicit (and (listp override) (plist-get override :host)))
         (target (or (plist-get context :path) (plist-get context :directory))))
    (cond
     ((memq explicit '(kernel editor)) explicit)
     ((and (listp policies) (member name (plist-get policies :editor))) 'editor)
     ((and (listp policies) (member name (plist-get policies :kernel))) 'kernel)
     ((member name lispm-control-protocol-editor-tools) 'editor)
     ((or (plist-get context :unsaved-buffer-p)
          (and (stringp target) (file-remote-p target)))
      'editor)
     ((member name lispm-control-protocol-kernel-tools) 'kernel)
     (t 'editor))))

;;;; Wire Validation & Frame Constructors (Emacs -> Kernel)

;; Cross-repo Layer 1 wire guard (duplicated intentionally from `lispm-kernel'
;; so Layer 1 `lispm-control-protocol' has zero runtime dependency on kernel).
(defun lispm-control-protocol--wire-symbol-p (sym)
  "Return non-nil when SYM prints and reads losslessly across the wire."
  (let ((name (string-remove-prefix ":" (symbol-name sym))))
    (and (string-match-p "\\`[a-zA-Z0-9_/*+<>=!-][a-zA-Z0-9_/*+<>=!?-]*\\'" name)
         (not (and (string-match-p "[0-9]" name)
                   (string-match-p "\\`[-+.eE0-9]+\\'" name))))))

(defun lispm-control-protocol-wire-safe-p (datum &optional depth)
  "Return non-nil when DATUM at DEPTH is valid on the kernel S-exp wire."
  (let ((depth (or depth 0)))
    (and (<= depth 63)
         (cond
          ((or (null datum) (eq datum t) (integerp datum) (stringp datum)) t)
          ((symbolp datum) (lispm-control-protocol--wire-symbol-p datum))
          ((consp datum)
           (and (proper-list-p datum)
                (cl-every (lambda (x) (lispm-control-protocol-wire-safe-p x (1+ depth)))
                          datum)))
          ((vectorp datum)
           (cl-every (lambda (x) (lispm-control-protocol-wire-safe-p x (1+ depth)))
                     datum))))))

(defun lispm-control-protocol-assert-wire (form)
  "Signal `lispm-control-protocol-error' unless FORM is wire-safe; return FORM."
  (unless (and (consp form) (lispm-control-protocol-wire-safe-p form))
    (signal 'lispm-control-protocol-error
            (list (format "Control frame is not wire-safe: %S" form))))
  form)

(defun lispm-control-protocol-agent-start (session config)
  "Build `(agent-start SESSION CONFIG)' frame."
  (unless (and (stringp session) (not (string-empty-p session)) (listp config))
    (signal 'lispm-control-protocol-error '("agent-start requires SESSION string and CONFIG plist")))
  (lispm-control-protocol-assert-wire `(agent-start ,session ,config)))

(defun lispm-control-protocol-agent-turn (session input)
  "Build `(agent-turn SESSION INPUT)' frame."
  (unless (and (stringp session) (not (string-empty-p session)) (listp input))
    (signal 'lispm-control-protocol-error '("agent-turn requires SESSION string and INPUT plist")))
  (lispm-control-protocol-assert-wire `(agent-turn ,session ,input)))

(defun lispm-control-protocol-agent-cancel (session)
  "Build `(agent-cancel SESSION)' frame."
  (unless (and (stringp session) (not (string-empty-p session)))
    (signal 'lispm-control-protocol-error '("agent-cancel requires non-empty SESSION string")))
  `(agent-cancel ,session))

(defun lispm-control-protocol-effect-reply (session effect-id status result)
  "Build `(agent-effect-reply SESSION EFFECT-ID STATUS RESULT)' frame."
  (unless (and (stringp session) (integerp effect-id)
               (memq status '(:ok :error :cancelled)))
    (signal 'lispm-control-protocol-error
            (list (format "Invalid agent-effect-reply: %S %S %S" session effect-id status))))
  (lispm-control-protocol-assert-wire
   `(agent-effect-reply ,session ,effect-id ,status ,(or result ""))))

(defun lispm-control-protocol-approval-decision (session call-id decision &optional reason)
  "Build `(agent-approval-decision SESSION CALL-ID DECISION)' with optional REASON."
  (unless (and (stringp session) (stringp call-id)
               (memq decision '(:approve :allow :deny :reject)))
    (signal 'lispm-control-protocol-error
            (list (format "Invalid approval decision: %S %S %S" session call-id decision))))
  (lispm-control-protocol-assert-wire
   (if reason
       `(agent-approval-decision ,session ,call-id ,decision :reason ,reason)
     `(agent-approval-decision ,session ,call-id ,decision))))

(defun lispm-control-protocol-viewport-subscribe (session mode)
  "Build `(agent-viewport-subscribe SESSION MODE)' for `:visible' or `:hidden'."
  (unless (and (stringp session) (memq mode '(:visible :hidden)))
    (signal 'lispm-control-protocol-error
            (list (format "Invalid viewport mode: %S %S" session mode))))
  `(agent-viewport-subscribe ,session ,mode))

(defun lispm-control-protocol-agent-resync (session &optional since-seq)
  "Build `(agent-resync SESSION ...)' frame starting after optional SINCE-SEQ."
  (unless (and (stringp session) (or (null since-seq) (integerp since-seq)))
    (signal 'lispm-control-protocol-error '("agent-resync requires SESSION and integer SINCE-SEQ")))
  (if since-seq
      `(agent-resync ,session :since-seq ,since-seq)
    `(agent-resync ,session)))

(defun lispm-control-protocol-agent-list ()
  "Build `(agent-list)' frame."
  '(agent-list))

(defun lispm-control-protocol-agent-stop (session)
  "Build `(agent-stop SESSION)' frame."
  (unless (and (stringp session) (not (string-empty-p session)))
    (signal 'lispm-control-protocol-error '("agent-stop requires non-empty SESSION string")))
  `(agent-stop ,session))

;;;; Inbound Frame Parser & Validator (Kernel -> Emacs)

(defun lispm-control-protocol-parse-frame (frame)
  "Validate and normalize inbound kernel control-plane FRAME.
Return a plist tagged with `:frame', or signal `lispm-control-protocol-error'."
  (unless (and (consp frame) (symbolp (car frame)))
    (signal 'lispm-control-protocol-error (list (format "Expected frame list, got %S" frame))))
  (pcase frame
    (`(turn-event ,(and session (pred stringp)) ,(and event (pred listp)))
     (list :frame 'turn-event :session session :event event))
    (`(editor-effect-request ,(and session (pred stringp))
                             ,(and effect-id (pred integerp))
                             ,(and kind (pred symbolp))
                             ,(and payload (pred listp))
                             . ,keys)
     (list :frame 'editor-effect-request
           :session session
           :effect-id effect-id
           :kind kind
           :payload payload
           :timeout (plist-get keys :timeout)))
    (`(approval-request ,(and session (pred stringp))
                        ,(and call-id (pred stringp))
                        ,(and tool (pred stringp))
                        ,(and input-json (pred stringp))
                        ,(and reasons (pred listp)))
     (list :frame 'approval-request
           :session session
           :call-id call-id
           :tool tool
           :input-json input-json
           :reasons reasons))
    (`(agent-snapshot ,(and session (pred stringp)) ,(and snapshot (pred listp)))
     (list :frame 'agent-snapshot :session session :snapshot snapshot))
    (`(agent-list ,(and sessions (pred listp)))
     (list :frame 'agent-list :sessions sessions))
    (_
     (signal 'lispm-control-protocol-error (list (format "Unknown or malformed frame: %S" frame))))))

;;;; Canonical LLM Response Adapter (Messages Shape Parity)

(defun lispm-control-protocol--decode-usage (usage)
  "Convert kernel keyword USAGE plist into Messages-shape usage alist."
  (let ((pairs nil))
    (dolist (spec '((:input-tokens . input_tokens)
                    (:output-tokens . output_tokens)
                    (:cache-read-input-tokens . cache_read_input_tokens)
                    (:cache-creation-input-tokens . cache_creation_input_tokens)))
      (when-let* ((val (plist-get usage (car spec)))
                  ((integerp val)))
        (push (cons (cdr spec) val) pairs)))
    (nreverse pairs)))

(defun lispm-control-protocol-decode-llm-result (res)
  "Convert canonical kernel `:llm-stream' plist RES into Messages or error plist.
Return `(:ok t :message ALIST)' on success, or `(:ok nil :error PLIST)'."
  (if (plist-get res :ok)
      (let ((invalid (copy-sequence (plist-get res :invalid)))
            (content nil))
        (dolist (b (plist-get res :blocks))
          (pcase (plist-get b :type)
            (:text
             (push `((type . "text") (text . ,(or (plist-get b :text) ""))) content))
            (:thinking
             (push (lispm-thought-block (or (plist-get b :thinking) "")
                                        (plist-get b :signature)
                                        (plist-get b :ephemeral))
                   content))
            (:tool-use
             (let* ((id (or (plist-get b :id) ""))
                    (name (or (plist-get b :name) ""))
                    (raw-json (string-trim (or (plist-get b :input-json) "{}")))
                    (input (if (string-empty-p raw-json)
                               nil
                             (condition-case nil
                                 (lispm-json-decode raw-json)
                               (error
                                (unless (member id invalid) (push id invalid))
                                nil)))))
               (push `((type . "tool_use") (id . ,id) (name . ,name) (input . ,input))
                     content)))
            (:other
             (when-let* ((json-str (plist-get b :json))
                         (parsed (condition-case nil (lispm-json-decode json-str) (error nil))))
               (push parsed content)))))
        (list :ok t
              :message
              `((role . "assistant")
                (content . ,(vconcat (nreverse content)))
                (stop_reason . ,(or (plist-get res :stop-reason) "end_turn"))
                (usage ,@(lispm-control-protocol--decode-usage (plist-get res :usage)))
                (lispm-invalid . ,invalid))))
    (list :ok nil
          :error (list :status (plist-get res :status)
                       :type (or (plist-get res :type) "http_error")
                       :message (or (plist-get res :message) "Request failed")))))

(provide 'lispm-control-protocol)
;;; lispm-control-protocol.el ends here
