;;; lispm-openai.el --- The OpenAI Chat Completions shape  -*- lexical-binding: t; -*-

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

;; The OpenAI Chat Completions API is what most model servers speak:
;; OpenAI itself and the many compatible servers and gateways (Ollama,
;; vLLM, LM Studio, llama.cpp, OpenRouter, ...).
;;
;; LispM keeps a single conversation format, the Messages shape its
;; transcripts parse into; this file translates requests into Chat
;; Completions and responses back:
;;
;;   system prompt         -> a leading "system" message
;;   text blocks           -> message content
;;   tool_use blocks       -> an assistant message's tool_calls
;;   tool_result blocks    -> "tool" messages, one per result; failures
;;                            are marked in the text, as Chat Completions
;;                            has no error flag
;;   thinking and other    -> not sent
;;   Messages-only blocks
;;
;; Reasoning text some servers return (reasoning_content, reasoning)
;; becomes a "reasoning" block: shown in the transcript, never sent back.

;;; Code:

(require 'seq)
(require 'lispm-core)

;;;; Requests

(defun lispm-openai--tool (tool)
  "TOOL, defined in Messages shape, as a Chat Completions function."
  `((type . "function")
    (function (name . ,(alist-get 'name tool))
              (description . ,(alist-get 'description tool))
              (parameters . ,(alist-get 'input_schema tool)))))

(defun lispm-openai--blocks (message)
  "Content blocks of the Messages-shape MESSAGE, as a list."
  (let ((content (alist-get 'content message)))
    (if (stringp content)
        (list `((type . "text") (text . ,content)))
      (append content nil))))

(defun lispm-openai--text (blocks)
  "Text of the text BLOCKS, joined, or nil if there is none."
  (let ((texts (delq nil (mapcar (lambda (b)
                                   (and (equal (alist-get 'type b) "text")
                                        (alist-get 'text b)))
                                 blocks))))
    (and texts (string-join texts "\n\n"))))

(defun lispm-openai--result-text (block)
  "Content of the tool_result BLOCK as text; its images are left out."
  (let* ((content (alist-get 'content block))
         (text (if (stringp content)
                   content
                 (or (lispm-openai--text (append content nil)) ""))))
    (if (eq (alist-get 'is_error block) t) (concat "Error: " text) text)))

(defun lispm-openai--image-part (block)
  "The Chat Completions content part for the image BLOCK."
  (let ((source (alist-get 'source block)))
    `((type . "image_url")
      (image_url (url . ,(format "data:%s;base64,%s"
                                 (alist-get 'media_type source) (alist-get 'data source)))))))

(defun lispm-openai--images (blocks)
  "The image blocks among BLOCKS and the content of their tool results."
  (cl-loop for b in blocks
           append (pcase (alist-get 'type b)
                    ("image" (list b))
                    ("tool_result"
                     (let ((content (alist-get 'content b)))
                       (and (vectorp content)
                            (seq-filter (lambda (x) (equal (alist-get 'type x) "image"))
                                        content)))))))

(defun lispm-openai--messages (system messages)
  "SYSTEM and Messages-shape MESSAGES as Chat Completions messages."
  (let (out)
    (when (and system (not (string-empty-p system)))
      (push `((role . "system") (content . ,system)) out))
    (dolist (message messages)
      (let ((blocks (lispm-openai--blocks message)))
        (if (equal (alist-get 'role message) "assistant")
            (let ((text (lispm-openai--text blocks))
                  (calls (cl-loop for b in blocks
                                  when (equal (alist-get 'type b) "tool_use")
                                  collect `((id . ,(alist-get 'id b))
                                            (type . "function")
                                            (function
                                             (name . ,(alist-get 'name b))
                                             (arguments . ,(lispm--json-encode
                                                            (alist-get 'input b))))))))
              (when (or text calls)
                (push `((role . "assistant")
                        (content . ,(or text :null))
                        ,@(and calls `((tool_calls . ,(vconcat calls)))))
                      out)))
          ;; Tool results must directly follow the call; text comes after.
          ;; Tool messages hold text only, so images, of tool results or
          ;; the user's, follow in a user message.
          (dolist (b blocks)
            (when (equal (alist-get 'type b) "tool_result")
              (push `((role . "tool")
                      (tool_call_id . ,(alist-get 'tool_use_id b))
                      (content . ,(lispm-openai--result-text b)))
                    out)))
          (let ((text (lispm-openai--text blocks))
                (images (lispm-openai--images blocks)))
            (cond
             (images
              (push `((role . "user")
                      (content . ,(vconcat
                                   (if text
                                       `(((type . "text") (text . ,text)))
                                     '(((type . "text") (text . "Images from the tool results above."))))
                                   (mapcar #'lispm-openai--image-part images))))
                    out))
             (text (push `((role . "user") (content . ,text)) out)))))))
    (nreverse out)))

(defun lispm-openai-request (backend model effort system tools messages)
  "A Chat Completions request for BACKEND; return (BODY . nil).
MODEL, EFFORT and SYSTEM are strings; TOOLS and MESSAGES are lists
of alists in Messages shape."
  (let* ((features (plist-get backend :features))
         (stream (memq 'streaming features)))
    (cons `((model . ,model)
            (,(if (memq 'max-completion-tokens features) 'max_completion_tokens 'max_tokens)
             . ,(or (plist-get backend :max-tokens) lispm-max-tokens))
            (stream . ,(if stream t :false))
            ,@(and stream (memq 'stream-usage features)
                   '((stream_options (include_usage . t))))
            ,@(and (memq 'reasoning-effort features) `((reasoning_effort . ,effort)))
            ,@(and tools `((tools . ,(vconcat (mapcar #'lispm-openai--tool tools)))))
            (messages . ,(vconcat (lispm-openai--messages system messages))))
          nil)))

(defun lispm-openai-headers (key _betas)
  "HTTP headers of a Chat Completions request with KEY."
  (append '("content-type: application/json")
          (and key (list (concat "authorization: Bearer " key)))))

;;;; Responses

(defun lispm-openai--string (value)
  "VALUE if it is a non-empty string, else nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun lispm-openai--usage (usage)
  "Chat Completions USAGE as Messages usage."
  (when (consp usage)
    (let ((details (alist-get 'prompt_tokens_details usage)))
      `((input_tokens . ,(alist-get 'prompt_tokens usage))
        (output_tokens . ,(alist-get 'completion_tokens usage))
        ,@(and (consp details) (numberp (alist-get 'cached_tokens details))
               `((cache_read_input_tokens . ,(alist-get 'cached_tokens details))))))))

(defun lispm-openai--message (reasoning text calls finish usage)
  "A Messages-shape message from the parts of a Chat Completions response.
REASONING and TEXT are strings or nil; CALLS a list of (ID NAME
ARGUMENTS); FINISH the finish reason; USAGE the usage object."
  (let ((n 0) content invalid)
    (when (lispm-openai--string reasoning)
      (push `((type . "reasoning") (text . ,reasoning)) content))
    (when (lispm-openai--string text)
      (push `((type . "text") (text . ,text)) content))
    (pcase-dolist (`(,id ,name ,arguments) calls)
      (let* ((id (or (lispm-openai--string id)
                     (format "call_%08x%d" (random #xffffffff) n)))
             (json (string-trim (or arguments "")))
             (input (if (string-empty-p json)
                        nil
                      (condition-case nil (lispm--json-decode json) (error :invalid)))))
        (unless (listp input)
          (push id invalid)
          (setq input nil))
        (push `((type . "tool_use") (id . ,id) (name . ,(or name "")) (input . ,input))
              content)
        (cl-incf n)))
    `((role . "assistant")
      (content . ,(vconcat (nreverse content)))
      (stop_reason . ,(pcase finish
                        ((or "tool_calls" "function_call") "tool_use")
                        ("length" "max_tokens")
                        ("content_filter" "refusal")
                        (_ "end_turn")))
      (usage ,@(lispm-openai--usage usage))
      (lispm-invalid . ,invalid))))

(defun lispm-openai--error (err)
  "ERR, an error object or string, as an alist with type and message."
  (if (consp err)
      `((type . ,(or (lispm-openai--string (alist-get 'type err))
                     (let ((code (alist-get 'code err))) (and code (not (eq code :null))
                                                              (format "%s" code)))
                     "api_error"))
        (message . ,(or (lispm-openai--string (alist-get 'message err)) "")))
    `((type . "api_error") (message . ,(format "%s" err)))))

(defun lispm-openai-whole (json)
  "The message in JSON, a Chat Completions response that was not streamed, or nil."
  (let ((choices (alist-get 'choices json)))
    (when (and (vectorp choices) (> (length choices) 0))
      (let* ((choice (aref choices 0))
             (message (alist-get 'message choice))
             (calls (alist-get 'tool_calls message)))
        (lispm-openai--message
         (or (lispm-openai--string (alist-get 'reasoning_content message))
             (lispm-openai--string (alist-get 'reasoning message)))
         (lispm-openai--string (alist-get 'content message))
         (seq-map (lambda (call)
                    (let* ((fn (alist-get 'function call))
                           (arguments (alist-get 'arguments fn)))
                      (list (alist-get 'id call)
                            (alist-get 'name fn)
                            (if (stringp arguments) arguments (lispm--json-encode arguments)))))
                  (if (vectorp calls) calls []))
         (alist-get 'finish_reason choice)
         (alist-get 'usage json))))))

(cl-defstruct (lispm-openai-stream (:constructor lispm-openai-stream-create)
                                   (:copier nil))
  "Decoding state of a Chat Completions event stream."
  (pending "")                          ; unparsed tail of the stream
  started                               ; non-nil once a chunk arrived
  (text "")
  (reasoning "")
  (calls (make-hash-table))             ; index -> (ID NAME ARGUMENTS)
  finish usage error done)

(defun lispm-openai-feed (stream chunk &optional on-delta)
  "Feed CHUNK of a Chat Completions event stream into STREAM.
ON-DELTA, if non-nil, is called with KIND and a string as content
arrives: `text', `thinking' (reasoning text) or `tool' (a tool name)."
  (pcase-let ((`(,events . ,rest)
               (lispm--sse-split (concat (lispm-openai-stream-pending stream) chunk))))
    (setf (lispm-openai-stream-pending stream) rest)
    (dolist (data events)
      (if (equal (string-trim data) "[DONE]")
          (setf (lispm-openai-stream-done stream) t)
        (lispm-openai--chunk stream (lispm--json-decode data) on-delta)))))

(defun lispm-openai--chunk (stream chunk on-delta)
  "Apply the decoded CHUNK to STREAM, reporting content to ON-DELTA."
  (setf (lispm-openai-stream-started stream) t)
  (let ((err (alist-get 'error chunk)))
    (when (and err (not (eq err :null)))
      (setf (lispm-openai-stream-error stream) (lispm-openai--error err))))
  (when (consp (alist-get 'usage chunk))
    (setf (lispm-openai-stream-usage stream) (alist-get 'usage chunk)))
  (let ((choices (alist-get 'choices chunk)))
    (seq-doseq (choice (if (vectorp choices) choices []))
      (let ((delta (alist-get 'delta choice))
            (finish (alist-get 'finish_reason choice)))
        (when (consp delta)
          (when-let* ((text (lispm-openai--string (alist-get 'content delta))))
            (setf (lispm-openai-stream-text stream)
                  (concat (lispm-openai-stream-text stream) text))
            (when on-delta (funcall on-delta 'text text)))
          (when-let* ((thought (or (lispm-openai--string (alist-get 'reasoning_content delta))
                                   (lispm-openai--string (alist-get 'reasoning delta)))))
            (setf (lispm-openai-stream-reasoning stream)
                  (concat (lispm-openai-stream-reasoning stream) thought))
            (when on-delta (funcall on-delta 'thinking thought)))
          (let ((calls (alist-get 'tool_calls delta)))
            (seq-doseq (call (if (vectorp calls) calls []))
              (lispm-openai--call-delta stream call on-delta))))
        (when (stringp finish)
          (setf (lispm-openai-stream-finish stream) finish))))))

(defun lispm-openai--call-delta (stream call on-delta)
  "Merge the tool CALL fragment into STREAM."
  (let* ((calls (lispm-openai-stream-calls stream))
         (index (let ((i (alist-get 'index call)))
                  (if (integerp i) i (hash-table-count calls))))
         (entry (gethash index calls (list nil nil "")))
         (fn (alist-get 'function call))
         (name (and (consp fn) (lispm-openai--string (alist-get 'name fn))))
         (arguments (and (consp fn) (alist-get 'arguments fn))))
    (when-let* ((id (lispm-openai--string (alist-get 'id call))))
      (setf (nth 0 entry) id))
    (when (and name (not (nth 1 entry)))
      (setf (nth 1 entry) name)
      (when on-delta (funcall on-delta 'tool name)))
    (when (stringp arguments)
      (setf (nth 2 entry) (concat (nth 2 entry) arguments))
      (when (and on-delta (nth 1 entry))
        (funcall on-delta 'tool-input
                 (list :id (nth 0 entry) :name (nth 1 entry) :json (nth 2 entry)))))
    (puthash index entry calls)))

(defun lispm-openai-stream-complete-p (stream)
  "Non-nil if STREAM received the whole response."
  (or (lispm-openai-stream-done stream)
      (lispm-openai-stream-finish stream)))

(defun lispm-openai-result (stream)
  "The complete message STREAM received, in Messages shape."
  (let ((calls (lispm-openai-stream-calls stream)))
    (lispm-openai--message (lispm-openai-stream-reasoning stream)
                           (lispm-openai-stream-text stream)
                           (mapcar (lambda (i) (gethash i calls))
                                   (sort (hash-table-keys calls) #'<))
                           (lispm-openai-stream-finish stream)
                           (lispm-openai-stream-usage stream))))

(provide 'lispm-openai)
;;; lispm-openai.el ends here
