;;; lispm-anthropic.el --- The Anthropic Messages API shape  -*- lexical-binding: t; -*-

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

;; Requests and responses in the shape of the Anthropic Messages API.
;; This is also LispM's internal conversation format, the one session
;; transcripts parse into, so requests need little translation; what
;; this file adds is the first-party extensions a backend may support
;; (see `lispm-backends') and a decoder for the Messages event stream,
;; `lispm-anthropic-feed', a plain state machine testable without a
;; network.

;;; Code:

(require 'seq)
(require 'lispm-core)

(defconst lispm-anthropic--updates-models
  "\\`claude-\\(?:opus-5-5\\|fable-5\\(?:-1\\)?\\|mythos-5-1\\|sonnet-5-5\\)\\'"
  "Models that return progress updates as thinking blocks.")

(defconst lispm-anthropic--fallback-models
  "\\`claude-\\(?:opus-5\\(?:-5\\)?\\|fable-5-1\\|sonnet-5-5\\)\\'"
  "Models accepting server-side refusal fallbacks.")

;;;; Requests

(defun lispm-anthropic--plain-tool-errors (messages)
  "MESSAGES with the is_error flag of tool results folded into their text."
  (mapcar
   (lambda (message)
     (let ((content (alist-get 'content message)))
       (if (not (vectorp content))
           message
         `((role . ,(alist-get 'role message))
           (content . ,(vconcat
                        (mapcar (lambda (block)
                                  (if (eq (alist-get 'is_error block) t)
                                      `((type . "tool_result")
                                        (tool_use_id . ,(alist-get 'tool_use_id block))
                                        (content . ,(let ((content (alist-get 'content block)))
                                                      (if (stringp content)
                                                          (concat "Error: " content)
                                                        (vconcat '(((type . "text") (text . "Error:")))
                                                                 content)))))
                                    block))
                                content)))))))
   messages))

(defun lispm-anthropic-request (backend model effort system tools messages)
  "A Messages request for BACKEND; return (BODY . BETAS).
MODEL, EFFORT and SYSTEM are strings; TOOLS and MESSAGES are lists
of alists in Messages shape."
  (let* ((features (plist-get backend :features))
         (tools (if (memq 'eager-input features)
                    (mapcar (lambda (tool) (append tool '((eager_input_streaming . t)))) tools)
                  tools))
         (messages (if (memq 'tool-errors features)
                       messages
                     (lispm-anthropic--plain-tool-errors messages)))
         (body `((model . ,model)
                 (max_tokens . ,(or (plist-get backend :max-tokens) lispm-max-tokens))
                 (stream . ,(if (memq 'streaming features) t :false))
                 ,@(and system (not (string-empty-p system)) `((system . ,system)))
                 (cache_control (type . "ephemeral"))
                 (output_config (effort . ,effort))
                 ,@(and tools `((tools . ,(vconcat tools))))
                 (messages . ,(vconcat messages))))
         betas)
    (when (memq 'adaptive-thinking features)
      ;; Keep thinking blocks valid when the user edits the transcript:
      ;; an edit drops the reasoning after it instead of failing.
      (push "thinking-binding-controls-2026-08-01" betas)
      (let ((updates (string-match-p lispm-anthropic--updates-models model)))
        (when updates (push "thinking-display-updates-2026-08-18" betas))
        (setq body (append body
                           `((thinking (type . "adaptive")
                                       ,@(and updates '((display . "updates")))
                                       (block_binding
                                        (prefix_mismatch_behavior . "drop_block"))))))))
    (when (and (memq 'fallbacks features)
               (string-match-p lispm-anthropic--fallback-models model))
      (push "server-side-fallback-2026-07-01" betas)
      (setq body (append body '((fallbacks . "default")))))
    (cons body (nreverse betas))))

(defun lispm-anthropic-headers (key betas)
  "HTTP headers of a Messages request with KEY and BETAS."
  (append '("content-type: application/json" "anthropic-version: 2023-06-01")
          (and key (list (concat "x-api-key: " key)))
          (and betas (list (concat "anthropic-beta: " (string-join betas ","))))))

;;;; Responses

(cl-defstruct (lispm-anthropic-stream (:constructor lispm-anthropic-stream-create)
                                      (:copier nil))
  "Decoding state of a Messages event stream."
  (pending "")                          ; unparsed tail of the stream
  message                               ; from message_start
  (blocks (make-hash-table))            ; index -> content block
  (json (make-hash-table))              ; index -> partial tool input
  invalid                               ; ids of tool calls with bad input
  error                                 ; error object, if any
  done)                                 ; non-nil after message_stop

(defun lispm-anthropic-feed (stream chunk &optional on-delta)
  "Feed CHUNK of a Messages event stream into STREAM.
ON-DELTA, if non-nil, is called with KIND and a string as content
arrives: `text', `thinking' or `tool' (a tool name)."
  (pcase-let ((`(,events . ,rest)
               (lispm--sse-split (concat (lispm-anthropic-stream-pending stream) chunk))))
    (setf (lispm-anthropic-stream-pending stream) rest)
    (dolist (data events)
      (lispm-anthropic--event stream (lispm--json-decode data) on-delta))))

(defun lispm-anthropic--event (stream ev on-delta)
  "Apply the decoded event EV to STREAM, reporting content to ON-DELTA."
  (let ((index (alist-get 'index ev))
        (blocks (lispm-anthropic-stream-blocks stream)))
    (pcase (alist-get 'type ev)
      ("message_start"
       (setf (lispm-anthropic-stream-message stream) (copy-alist (alist-get 'message ev))))
      ("content_block_start"
       (let ((block (copy-alist (alist-get 'content_block ev))))
         (puthash index block blocks)
         (when (and on-delta (equal (alist-get 'type block) "tool_use"))
           (funcall on-delta 'tool (alist-get 'name block)))))
      ("content_block_delta"
       (lispm-anthropic--delta stream index (alist-get 'delta ev) on-delta))
      ("content_block_stop"
       (let ((block (gethash index blocks)))
         (when (equal (alist-get 'type block) "tool_use")
           (let ((json (string-trim (gethash index (lispm-anthropic-stream-json stream) ""))))
             (setf (alist-get 'input block)
                   (if (string-empty-p json)
                       (alist-get 'input block)
                     (condition-case nil
                         (lispm--json-decode json)
                       (error
                        (push (alist-get 'id block) (lispm-anthropic-stream-invalid stream))
                        nil))))
             (puthash index block blocks)))))
      ("message_delta"
       (let ((message (lispm-anthropic-stream-message stream)))
         (pcase-dolist (`(,key . ,value) (alist-get 'delta ev))
           (setf (alist-get key message) value))
         (let ((usage (alist-get 'usage message)))
           (pcase-dolist (`(,key . ,value) (alist-get 'usage ev))
             (setf (alist-get key usage) value))
           (setf (alist-get 'usage message) usage))
         (setf (lispm-anthropic-stream-message stream) message)))
      ("message_stop" (setf (lispm-anthropic-stream-done stream) t))
      ("error" (setf (lispm-anthropic-stream-error stream) (alist-get 'error ev))))))

(defun lispm-anthropic--delta (stream index delta on-delta)
  "Apply DELTA to the block at INDEX of STREAM."
  (let ((block (gethash index (lispm-anthropic-stream-blocks stream))))
    (pcase (alist-get 'type delta)
      ("text_delta"
       (let ((text (alist-get 'text delta)))
         (setf (alist-get 'text block) (concat (alist-get 'text block) text))
         (when on-delta (funcall on-delta 'text text))))
      ("thinking_delta"
       (let ((text (alist-get 'thinking delta)))
         (setf (alist-get 'thinking block) (concat (alist-get 'thinking block) text))
         (when on-delta (funcall on-delta 'thinking text))))
      ("signature_delta"
       (setf (alist-get 'signature block)
             (concat (let ((s (alist-get 'signature block))) (if (stringp s) s ""))
                     (alist-get 'signature delta))))
      ("input_json_delta"
       (let ((json (concat (gethash index (lispm-anthropic-stream-json stream) "")
                           (alist-get 'partial_json delta))))
         (puthash index json (lispm-anthropic-stream-json stream))
         (when on-delta
           (funcall on-delta 'tool-input
                    (list :id (alist-get 'id block) :name (alist-get 'name block) :json json)))))
      ("citations_delta"
       (setf (alist-get 'citations block)
             (vconcat (alist-get 'citations block)
                      (vector (alist-get 'citation delta))))))
    (puthash index block (lispm-anthropic-stream-blocks stream))))

(defun lispm-anthropic-result (stream)
  "The complete message STREAM received, as an alist.
Its content is a vector of blocks in order.  The key `lispm-invalid'
lists tool calls whose input was not valid JSON."
  (let* ((blocks (lispm-anthropic-stream-blocks stream))
         (content (mapcar (lambda (i) (gethash i blocks))
                          (sort (hash-table-keys blocks) #'<)))
         (message (copy-alist (lispm-anthropic-stream-message stream))))
    (setf (alist-get 'content message) (vconcat (lispm-anthropic--after-fallback content)))
    (setf (alist-get 'lispm-invalid message) (lispm-anthropic-stream-invalid stream))
    message))

(defun lispm-anthropic-whole (json)
  "The message in JSON, a response that was not streamed, or nil."
  (when (equal (alist-get 'type json) "message")
    (setf (alist-get 'content json)
          (vconcat (lispm-anthropic--after-fallback (append (alist-get 'content json) nil))))
    json))

(defun lispm-anthropic--after-fallback (content)
  "CONTENT as it must be echoed back after a mid-stream fallback.
Before the last fallback block only text survives: reasoning and
tool calls of the model that declined are not part of the answer."
  (let ((cut (cl-position "fallback" content :from-end t
                          :key (lambda (b) (alist-get 'type b)) :test #'equal)))
    (if (not cut)
        content
      (append (seq-filter (lambda (b) (equal (alist-get 'type b) "text"))
                          (seq-take content cut))
              (nthcdr cut content)))))

(provide 'lispm-anthropic)
;;; lispm-anthropic.el ends here
