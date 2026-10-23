;;; lispm-local-http.el --- Minimal local HTTP server -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-agent
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

;; An HTTP/1.1 server for local agents, small enough to audit.  It listens
;; on 127.0.0.1 only, answers one request per connection, and refuses a
;; request unless the server's authorizer accepts its headers.
;;
;;   bytes --filter--> head + Content-Length body --authorize--> handler
;;   --> (STATUS BODY) --> response, then the connection closes

;;; Code:

(require 'subr-x)

(defconst lispm-local-http-max-request (* 64 1024 1024)
  "Largest request body, in bytes, the server reads.")

(defvar lispm-local-http-respond nil
  "While a server's handler runs, a function sending the request a response.
It takes (STATUS BODY TYPE), as the handler returns it.  A handler that
answers later keeps it and returns `deferred'.")

(defun lispm-local-http-headers (head)
  "Return the headers of the HTTP request HEAD as an alist of downcased names."
  (let (headers)
    (dolist (line (cdr (split-string head "\r\n" t)))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line)) headers)))
    headers))

(defun lispm-local-http-serve (name authorize handler)
  "Start an HTTP server called NAME on a free local port; return it.
AUTHORIZE is called with a request's headers, an alist of downcased names
to values, and must return non-nil to admit it.  HANDLER is called with
the method, the path, the headers and the decoded body, and returns
\(STATUS BODY TYPE): an HTTP status such as \"200 OK\", a unibyte body, or
nil for none, and the body's content type, JSON when left out; or
`deferred', to send it later with `lispm-local-http-respond'.  The port
is `(process-contact SERVER :service)'."
  (make-network-process
   :name name
   :server t
   :host "127.0.0.1"
   :service t
   :family 'ipv4
   :coding 'binary
   :noquery t
   :filter #'lispm-local-http--filter
   :sentinel #'ignore
   :log (lambda (server connection _message) (process-put connection :server server))
   :plist (list :authorize authorize :handler handler)))

(defun lispm-local-http--filter (connection bytes)
  "Accumulate BYTES from CONNECTION and answer the request once complete."
  (let* ((input (concat (process-get connection :input) bytes))
         (end (string-search "\r\n\r\n" input)))
    (process-put connection :input input)
    (cond
     (end
      (let* ((head (substring input 0 end))
             (headers (lispm-local-http-headers head))
             (length (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
             (handler (plist-get (process-plist (process-get connection :server)) :handler))
             (authorize (plist-get (process-plist (process-get connection :server)) :authorize)))
        (cond
         ((> length lispm-local-http-max-request)
          (lispm-local-http--reply connection '("413 Content Too Large")))
         ((>= (length input) (+ end 4 length))
          (process-put connection :input nil)
          (pcase-let ((`(,method ,path) (split-string (car (split-string head "\r\n")) " ")))
            (let* ((lispm-local-http-respond
                    (lambda (response)
                      (when (process-live-p connection)
                        (lispm-local-http--reply connection response))))
                   (response
                    (if (funcall authorize headers)
                        (funcall handler method path headers
                                 (decode-coding-string (substring input (+ end 4) (+ end 4 length))
                                                       'utf-8))
                      '("401 Unauthorized"))))
              (unless (eq response 'deferred)
                (lispm-local-http--reply connection response))))))))
     ((> (length input) 16384)
      (lispm-local-http--reply connection '("431 Request Header Fields Too Large"))))))

(defun lispm-local-http--reply (connection response)
  "Send CONNECTION the (STATUS BODY TYPE) RESPONSE and close it."
  (pcase-let ((`(,status ,body ,type) response))
    (process-send-string
     connection
     (concat "HTTP/1.1 " status "\r\n"
             (if body (format "Content-Type: %s\r\n" (or type "application/json")) "")
             (format "Content-Length: %d\r\nConnection: close\r\n\r\n" (length body))
             body))
    (process-send-eof connection)))

(provide 'lispm-local-http)
;;; lispm-local-http.el ends here
