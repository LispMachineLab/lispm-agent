;;; lispm-api.el --- Model requests over curl, in either API shape  -*- lexical-binding: t; -*-

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

;; Model requests run as curl processes, so the Emacs event loop never
;; blocks.  `lispm-api-stream' starts one and reports back through
;; three callbacks: content as it arrives (for live display), the final
;; message, or an error.
;;
;; What differs between APIs lives in `lispm-api-shapes': how to build
;; a request, which path and headers to use, how to decode a response.
;; Two shapes are defined, OpenAI Chat Completions (lispm-openai.el)
;; and Anthropic Messages (lispm-anthropic.el).  Whatever the shape,
;; the runtime sees requests built from, and responses returned in, one
;; format: the Messages shape its transcripts parse into.

;;; Code:

(require 'auth-source)
(require 'url-parse)
(require 'lispm-core)
(require 'lispm-anthropic)
(require 'lispm-openai)

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

(defconst lispm-api-shapes
  '((openai :path "/chat/completions"
            :request lispm-openai-request
            :headers lispm-openai-headers
            :stream lispm-openai-stream-create
            :feed lispm-openai-feed
            :started lispm-openai-stream-started
            :error lispm-openai-stream-error
            :complete lispm-openai-stream-complete-p
            :result lispm-openai-result
            :whole lispm-openai-whole)
    (anthropic :path "/v1/messages"
               :request lispm-anthropic-request
               :headers lispm-anthropic-headers
               :stream lispm-anthropic-stream-create
               :feed lispm-anthropic-feed
               :started lispm-anthropic-stream-message
               :error lispm-anthropic-stream-error
               :complete lispm-anthropic-stream-done
               :result lispm-anthropic-result
               :whole lispm-anthropic-whole))
  "API shapes, by name.  Each is a plist of:
  :path      appended to the backend's URL
  :request   (BACKEND MODEL EFFORT SYSTEM TOOLS MESSAGES) -> (BODY . BETAS)
  :headers   (KEY BETAS) -> list of \"Name: value\" strings
  :stream    () -> a fresh decoding state
  :feed      (STATE CHUNK ON-DELTA) decodes part of an event stream
  :started   (STATE) -> non-nil once events were decoded
  :error     (STATE) -> an error object received in the stream, or nil
  :complete  (STATE) -> non-nil once the whole response arrived
  :result    (STATE) -> the message, in Messages shape
  :whole     (JSON) -> the message of a response that was not streamed")

(defun lispm-api--shape (backend)
  "The API shape BACKEND speaks."
  (let ((name (or (plist-get backend :api) 'anthropic)))
    (or (alist-get name lispm-api-shapes)
        (user-error "Unknown API shape %s (see `lispm-api-shapes')" name))))

(defun lispm-api--find-key (variable host)
  "An API key from the environment VARIABLE or auth-source entry for HOST."
  (or (and variable (getenv variable))
      (when-let* ((found (car (auth-source-search :host host :max 1))))
        (let ((secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret)))
      (user-error "No API key: set %s or add %s to auth-source"
                  (or variable "the key") host)))

(defun lispm-api--key (backend)
  "API key for BACKEND, or nil if it needs none."
  (let ((key (plist-get backend :key))
        (variable (plist-get backend :key-env)))
    (cond (variable (or (getenv variable) (user-error "No API key: set %s" variable)))
          ((null key) nil)
          ((stringp key) key)
          ((eq key 'anthropic) (lispm-api--find-key "ANTHROPIC_API_KEY" "api.anthropic.com"))
          ((eq key 'openai) (lispm-api--find-key "OPENAI_API_KEY" "api.openai.com"))
          ((eq key 'auth-source)
           (lispm-api--find-key nil (url-host (url-generic-parse-url (plist-get backend :url)))))
          ((functionp key) (funcall key))
          (t (user-error "Bad :key in backend: %S" key)))))

(defun lispm-api-request (backend model effort system tools messages)
  "Build a request for BACKEND in the shape it speaks; return (BODY . BETAS).
MODEL, EFFORT and SYSTEM are strings; TOOLS and MESSAGES are lists of
alists in Messages shape."
  (funcall (plist-get (lispm-api--shape backend) :request)
           backend model effort system tools messages))

;;;; Transport

(defun lispm-api--curl-config (headers)
  "curl configuration sending HEADERS.
It is fed on stdin, so keys stay off the command line."
  (mapconcat (lambda (header)
               (format "header = \"%s\"\n"
                       (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" header)))
             headers ""))

(defconst lispm-api--status-marker "\n__LISPM_HTTP_STATUS__:"
  "Separator curl prints before the HTTP status.")

(cl-defun lispm-api-stream (backend body betas &key on-delta on-done on-error)
  "POST BODY to BACKEND and stream the response.
Responses that are not streamed (BODY asks for none) arrive whole.
BETAS are beta feature names, for APIs that have them.  ON-DELTA
receives (KIND STRING) as content streams in: KIND text, thinking or
tool (STRING being the tool's name); and (tool-input PLIST) as a tool
call's input arrives, PLIST holding its :id, :name and the :json
received so far.  ON-DONE receives the final message, in Messages
shape; ON-ERROR a plist with :status, :type and :message.  Return the
curl process."
  (let* ((shape (lispm-api--shape backend))
         (state (funcall (plist-get shape :stream)))
         (file (make-temp-file "lispm-request-" nil ".json"))
         (url (concat (string-remove-suffix "/" (plist-get backend :url))
                      (plist-get shape :path)))
         (raw nil)
         (client-error nil)
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
                     (unless client-error
                       (condition-case err
                           (funcall (plist-get shape :feed) state chunk on-delta)
                         (error (setq client-error
                                      `((type . "client_error")
                                        (message . ,(error-message-string err))))))))
           :sentinel (lambda (p _event)
                       (unless (process-live-p p)
                         (ignore-errors (delete-file file))
                         (unless (process-get p 'lispm-aborted)
                           (lispm-api--finish shape state client-error
                                              (apply #'concat (nreverse raw))
                                              on-done on-error))))))
    (process-send-string
     proc (lispm-api--curl-config
           (funcall (plist-get shape :headers) (lispm-api--key backend) betas)))
    (process-send-eof proc)
    proc))

(defun lispm-api--error-plist (status err)
  "The error plist for HTTP STATUS and the error object or string ERR."
  (list :status status
        :type (or (and (consp err) (stringp (alist-get 'type err)) (alist-get 'type err))
                  "http_error")
        :message (cond ((stringp err) err)
                       ((and (consp err) (stringp (alist-get 'message err)))
                        (alist-get 'message err))
                       (t (format "%S" err)))))

(defun lispm-api--finish (shape state client-error output on-done on-error)
  "Report the outcome of a request in SHAPE whose decoding STATE produced OUTPUT.
CLIENT-ERROR is an error raised while decoding, if any."
  (let* ((at (string-search lispm-api--status-marker output))
         (status (and at (string-to-number
                          (substring output (+ at (length lispm-api--status-marker))))))
         (body (string-trim (if at (substring output 0 at) output)))
         (err (or client-error (funcall (plist-get shape :error) state))))
    (cond
     ((and (eql status 200)
           (not (funcall (plist-get shape :started) state))
           (string-prefix-p "{" body))
      ;; A response that was not streamed.
      (let* ((json (ignore-errors (lispm--json-decode body)))
             (message (and (consp json) (funcall (plist-get shape :whole) json))))
        (cond (message (funcall on-done message))
              ((and (consp json) (alist-get 'error json))
               (funcall on-error (lispm-api--error-plist status (alist-get 'error json))))
              (t (funcall on-error (list :status status :type "bad_response"
                                         :message (lispm--truncate body 500)))))))
     ((and (eql status 200) err)
      (funcall on-error (lispm-api--error-plist status err)))
     ((and (eql status 200) (funcall (plist-get shape :complete) state))
      (funcall on-done (funcall (plist-get shape :result) state)))
     ((eql status 200)
      (funcall on-error (list :status status :type "incomplete_stream"
                              :message "The response stream ended early")))
     ((or (null status) (zerop status))
      (funcall on-error (list :status nil :type "network_error"
                              :message (if (string-empty-p body) "curl failed" body))))
     (t
      (let ((json (ignore-errors (lispm--json-decode body))))
        (funcall on-error
                 (lispm-api--error-plist
                  status (or (and (consp json) (alist-get 'error json))
                             (lispm--truncate body 500)))))))))

(defun lispm-api-retryable-p (err)
  "Non-nil if the error plist ERR is worth retrying."
  (let ((status (plist-get err :status)))
    (or (member (plist-get err :type)
                '("overloaded_error" "rate_limit_error" "api_error" "server_error"
                  "network_error" "incomplete_stream"))
        (and status (or (= status 429) (>= status 500))))))

(provide 'lispm-api)
;;; lispm-api.el ends here
