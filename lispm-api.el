;;; lispm-api.el --- Streaming Messages API client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; A small client for the Anthropic Messages API (and the many local
;; gateways that speak it, such as rotom), streaming over curl so the
;; Emacs event loop never blocks.  `lispm-api-stream' starts a request
;; and reports back through three callbacks: deltas as they arrive (for
;; live display), the final message, or an error.  The SSE decoder is a
;; plain state machine, `lispm-sse-feed', testable without a network.

;;; Code:

(require 'auth-source)
(require 'seq)
(require 'lispm-core)

(defcustom lispm-curl-program "curl"
  "The curl executable."
  :type 'string
  :group 'lispm)

(defcustom lispm-api-max-retries 3
  "How often to retry a request that failed with a transient error."
  :type 'integer
  :group 'lispm)

(defvar lispm-api-function #'lispm-api-stream
  "Function performing a model request.
Called with BACKEND, BODY, BETAS and the keyword callbacks :on-delta,
:on-done and :on-error, as `lispm-api-stream'.  Tests replace it.")

(defconst lispm-api--updates-models
  "\\`claude-\\(?:opus-5-5\\|fable-5\\(?:-1\\)?\\|mythos-5-1\\|sonnet-5-5\\)\\'"
  "Models that return progress updates as thinking blocks.")

(defconst lispm-api--fallback-models
  "\\`claude-\\(?:opus-5\\(?:-5\\)?\\|fable-5-1\\|sonnet-5-5\\)\\'"
  "Models accepting server-side refusal fallbacks.")

;;;; Requests

(defun lispm-api--key (backend)
  "API key for BACKEND, or nil if it needs none."
  (let ((key (plist-get backend :key)))
    (cond ((null key) nil)
          ((stringp key) key)
          ((eq key 'anthropic)
           (or (getenv "ANTHROPIC_API_KEY")
               (when-let* ((found (car (auth-source-search :host "api.anthropic.com"
                                                           :max 1))))
                 (let ((secret (plist-get found :secret)))
                   (if (functionp secret) (funcall secret) secret)))
               (user-error "No API key: set ANTHROPIC_API_KEY or add api.anthropic.com to auth-source")))
          ((functionp key) (funcall key))
          (t (user-error "Bad :key in backend: %S" key)))))

(defun lispm-api--plain-tool-errors (messages)
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
                                        (content . ,(concat "Error: " (alist-get 'content block))))
                                    block))
                                content)))))))
   messages))

(defun lispm-api-request (backend model effort system tools messages)
  "Build a streaming request for BACKEND; return (BODY . BETAS).
MODEL, EFFORT and SYSTEM are strings; TOOLS and MESSAGES lists of
alists in API shape."
  (let* ((features (plist-get backend :features))
         (tools (if (memq 'eager-input features)
                    (mapcar (lambda (tool) (append tool '((eager_input_streaming . t)))) tools)
                  tools))
         (messages (if (memq 'tool-errors features)
                       messages
                     (lispm-api--plain-tool-errors messages)))
         (body `((model . ,model)
                (max_tokens . ,lispm-max-tokens)
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
      (let ((updates (string-match-p lispm-api--updates-models model)))
        (when updates (push "thinking-display-updates-2026-08-18" betas))
        (setq body (append body
                           `((thinking (type . "adaptive")
                                       ,@(and updates '((display . "updates")))
                                       (block_binding
                                        (prefix_mismatch_behavior . "drop_block"))))))))
    (when (and (memq 'fallbacks features)
               (string-match-p lispm-api--fallback-models model))
      (push "server-side-fallback-2026-07-01" betas)
      (setq body (append body '((fallbacks . "default")))))
    (cons body (nreverse betas))))

;;;; Server-sent events

(cl-defstruct (lispm-sse (:constructor lispm-sse--create) (:copier nil))
  (pending "")                          ; unparsed tail of the stream
  message                               ; from message_start
  (blocks (make-hash-table))            ; index -> content block
  (json (make-hash-table))              ; index -> partial tool input
  invalid                               ; ids of tool calls with bad input
  error                                 ; error object, if any
  done)                                 ; non-nil after message_stop

(defun lispm-sse-feed (sse chunk &optional on-delta)
  "Feed CHUNK of an event stream into SSE.
ON-DELTA, if non-nil, is called with KIND and a string as content
arrives: `text', `thinking' or `tool' (a tool name)."
  (let ((data (concat (lispm-sse-pending sse) chunk))
        (start 0))
    (while (string-match "\r?\n\r?\n" data start)
      (let ((event (substring data start (match-beginning 0))))
        (setq start (match-end 0))
        (lispm-sse--event sse event on-delta)))
    (setf (lispm-sse-pending sse) (substring data start))))

(defun lispm-sse--event (sse event on-delta)
  "Apply one raw EVENT to SSE, reporting content to ON-DELTA."
  (let ((data (mapconcat (lambda (line) (string-remove-prefix " " (substring line 5)))
                         (seq-filter (lambda (line) (string-prefix-p "data:" line))
                                     (split-string event "\r?\n"))
                         "\n")))
    (unless (string-empty-p data)
      (let* ((ev (lispm--json-decode data))
             (index (alist-get 'index ev))
             (blocks (lispm-sse-blocks sse)))
        (pcase (alist-get 'type ev)
          ("message_start"
           (setf (lispm-sse-message sse) (copy-alist (alist-get 'message ev))))
          ("content_block_start"
           (let ((block (copy-alist (alist-get 'content_block ev))))
             (puthash index block blocks)
             (when (and on-delta (equal (alist-get 'type block) "tool_use"))
               (funcall on-delta 'tool (alist-get 'name block)))))
          ("content_block_delta"
           (lispm-sse--delta sse index (alist-get 'delta ev) on-delta))
          ("content_block_stop"
           (let ((block (gethash index blocks)))
             (when (equal (alist-get 'type block) "tool_use")
               (let ((json (string-trim (gethash index (lispm-sse-json sse) ""))))
                 (setf (alist-get 'input block)
                       (if (string-empty-p json)
                           (alist-get 'input block)
                         (condition-case nil
                             (lispm--json-decode json)
                           (error
                            (push (alist-get 'id block) (lispm-sse-invalid sse))
                            nil))))
                 (puthash index block blocks)))))
          ("message_delta"
           (let ((message (lispm-sse-message sse)))
             (pcase-dolist (`(,key . ,value) (alist-get 'delta ev))
               (setf (alist-get key message) value))
             (let ((usage (alist-get 'usage message)))
               (pcase-dolist (`(,key . ,value) (alist-get 'usage ev))
                 (setf (alist-get key usage) value))
               (setf (alist-get 'usage message) usage))
             (setf (lispm-sse-message sse) message)))
          ("message_stop" (setf (lispm-sse-done sse) t))
          ("error" (setf (lispm-sse-error sse) (alist-get 'error ev))))))))

(defun lispm-sse--delta (sse index delta on-delta)
  "Apply DELTA to the block at INDEX of SSE."
  (let ((block (gethash index (lispm-sse-blocks sse))))
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
       (puthash index (concat (gethash index (lispm-sse-json sse) "")
                              (alist-get 'partial_json delta))
                (lispm-sse-json sse)))
      ("citations_delta"
       (setf (alist-get 'citations block)
             (vconcat (alist-get 'citations block)
                      (vector (alist-get 'citation delta))))))
    (puthash index block (lispm-sse-blocks sse))))

(defun lispm-sse-result (sse)
  "The complete message SSE received, as an alist.
Its content is a vector of blocks in order.  The key `lispm-invalid'
lists tool calls whose input was not valid JSON."
  (let* ((indexes (sort (hash-table-keys (lispm-sse-blocks sse)) #'<))
         (content (mapcar (lambda (i) (gethash i (lispm-sse-blocks sse))) indexes))
         (message (copy-alist (lispm-sse-message sse))))
    (setf (alist-get 'content message) (vconcat (lispm-sse--after-fallback content)))
    (setf (alist-get 'lispm-invalid message) (lispm-sse-invalid sse))
    message))

(defun lispm-sse--after-fallback (content)
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

;;;; Transport

(defun lispm-api--curl-config (backend betas)
  "curl configuration (fed on stdin, so keys stay off the command line)."
  (let ((key (lispm-api--key backend))
        (esc (lambda (s) (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" s))))
    (concat "header = \"content-type: application/json\"\n"
            "header = \"anthropic-version: 2023-06-01\"\n"
            (if key (format "header = \"x-api-key: %s\"\n" (funcall esc key)) "")
            (if betas
                (format "header = \"anthropic-beta: %s\"\n" (string-join betas ","))
              ""))))

(defconst lispm-api--status-marker "\n__LISPM_HTTP_STATUS__:"
  "Separator curl prints before the HTTP status.")

(cl-defun lispm-api-stream (backend body betas &key on-delta on-done on-error)
  "POST BODY to BACKEND's Messages endpoint and stream the response.
Responses that are not streamed (BODY asks for none) arrive whole.
BETAS are beta feature names for the anthropic-beta header.
ON-DELTA receives (KIND STRING) as content streams in; ON-DONE the
final message (see `lispm-sse-result'); ON-ERROR a plist with
:status, :type and :message.  Return the curl process."
  (let* ((file (make-temp-file "lispm-request-" nil ".json"))
         (sse (lispm-sse--create))
         (raw nil)
         (url (concat (string-remove-suffix "/" (plist-get backend :url)) "/v1/messages"))
         proc)
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert (json-serialize body))))
    (setq proc
          (make-process
           :name "lispm-api"
           :buffer nil
           :noquery t
           :connection-type 'pipe
           :coding '(utf-8-unix . utf-8-unix)
           :command (list lispm-curl-program "--silent" "--show-error" "--no-buffer"
                          "--config" "-"
                          "--data-binary" (concat "@" file)
                          "--write-out" (concat lispm-api--status-marker "%{http_code}\n")
                          url)
           :filter (lambda (_proc chunk)
                     (push chunk raw)
                     (condition-case err
                         (lispm-sse-feed sse chunk on-delta)
                       (error (setf (lispm-sse-error sse)
                                    `((type . "client_error")
                                      (message . ,(error-message-string err)))))))
           :sentinel (lambda (p _event)
                       (unless (process-live-p p)
                         (ignore-errors (delete-file file))
                         (unless (process-get p 'lispm-aborted)
                           (lispm-api--finish sse (apply #'concat (nreverse raw))
                                              on-done on-error))))))
    (process-send-string proc (lispm-api--curl-config backend betas))
    (process-send-eof proc)
    proc))

(defun lispm-api--finish (sse output on-done on-error)
  "Report the outcome of a request whose stream SSE produced OUTPUT."
  (let* ((at (string-search lispm-api--status-marker output))
         (status (and at (string-to-number
                          (substring output (+ at (length lispm-api--status-marker))))))
         (body (string-trim (if at (substring output 0 at) output))))
    (cond
     ((and (eql status 200) (null (lispm-sse-message sse)) (string-prefix-p "{" body))
      ;; A response that was not streamed.
      (let ((message (ignore-errors (lispm--json-decode body))))
        (if (and (consp message) (equal (alist-get 'type message) "message"))
            (progn
              (setf (alist-get 'content message)
                    (vconcat (lispm-sse--after-fallback
                              (append (alist-get 'content message) nil))))
              (funcall on-done message))
          (funcall on-error (list :status status :type "bad_response"
                                  :message (lispm--truncate body 500))))))
     ((and (eql status 200) (lispm-sse-error sse))
      (let ((err (lispm-sse-error sse)))
        (funcall on-error (list :status status :type (alist-get 'type err)
                                :message (alist-get 'message err)))))
     ((and (eql status 200) (lispm-sse-done sse))
      (funcall on-done (lispm-sse-result sse)))
     ((eql status 200)
      (funcall on-error (list :status status :type "incomplete_stream"
                              :message "The response stream ended early")))
     ((or (null status) (zerop status))
      (funcall on-error (list :status nil :type "network_error"
                              :message (if (string-empty-p body) "curl failed" body))))
     (t
      (let* ((json (ignore-errors (lispm--json-decode body)))
             (err (and (consp json) (alist-get 'error json))))
        (funcall on-error (list :status status
                                :type (or (and (consp err) (alist-get 'type err)) "http_error")
                                :message (or (and (consp err) (alist-get 'message err))
                                             (lispm--truncate body 500)))))))))

(defun lispm-api-retryable-p (err)
  "Non-nil if the error plist ERR is worth retrying."
  (let ((status (plist-get err :status)))
    (or (member (plist-get err :type)
                '("overloaded_error" "rate_limit_error" "api_error"
                  "network_error" "incomplete_stream"))
        (and status (or (= status 429) (>= status 500))))))

(provide 'lispm-api)
;;; lispm-api.el ends here
