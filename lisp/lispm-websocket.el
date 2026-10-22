;;; lispm-websocket.el --- Minimal local WebSocket server -*- lexical-binding: t; -*-

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

;; A WebSocket server (RFC 6455) for local agents, small enough to audit.
;; It listens on 127.0.0.1 only, carries text messages only, and refuses a
;; client at the handshake unless the server's authorizer accepts the
;; request's headers.  A browser cannot set custom headers on a WebSocket,
;; so requiring a secret header shuts out web pages that probe local ports.
;;
;; Each connection is a network process whose plist holds its state:
;;
;;   bytes --filter--> handshake --authorize--> frames --> text messages
;;
;; Server frames are unmasked and unfragmented; client frames must be
;; masked, as the RFC requires.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-local-http)

(defconst lispm-websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "Key suffix whose SHA-1 proves the server understood the handshake.")

(defconst lispm-websocket-max-message (* 64 1024 1024)
  "Largest message, in bytes, a client may send before it is disconnected.
Agents send whole files in one message, so the bound is generous; it only
stops a runaway client from exhausting memory.")

(defun lispm-websocket-serve (name authorize on-open on-message on-close)
  "Start a WebSocket server called NAME on a free local port; return it.
AUTHORIZE is called with the handshake's headers, an alist of downcased
names to values, and must return non-nil to admit the client.  ON-OPEN is
called with each admitted connection, ON-MESSAGE with a connection and a
decoded text message, and ON-CLOSE with a connection that closed.  The
port is `(process-contact SERVER :service)'."
  (make-network-process
   :name name
   :server t
   :host "127.0.0.1"
   :service t
   :family 'ipv4
   :coding 'binary
   :noquery t
   :filter #'lispm-websocket--filter
   :sentinel #'lispm-websocket--sentinel
   :log (lambda (server connection _message) (process-put connection :server server))
   :plist (list :authorize authorize :on-open on-open
                :on-message on-message :on-close on-close)))

(defun lispm-websocket-send (connection text)
  "Send TEXT to CONNECTION as one text frame.
A multibyte TEXT is encoded as UTF-8; a unibyte one is taken to be UTF-8
already, as `json-serialize' returns it, and is sent as is."
  (when (process-live-p connection)
    (process-send-string connection
                         (lispm-websocket--frame 1 (if (multibyte-string-p text)
                                                       (encode-coding-string text 'utf-8)
                                                    text)))))

(defun lispm-websocket-close (connection)
  "Close CONNECTION with a close frame."
  (when (process-live-p connection)
    (process-send-string connection (lispm-websocket--frame 8 ""))
    (delete-process connection)))

(defun lispm-websocket--server-get (connection property)
  "Return PROPERTY of the server CONNECTION was accepted by.
The server's log function records it on each connection as it is accepted."
  (plist-get (process-plist (process-get connection :server)) property))

(defun lispm-websocket--sentinel (connection _event)
  "Report CONNECTION closed once its handshake succeeded."
  (when (and (process-get connection :open)
             (not (process-live-p connection)))
    (process-put connection :open nil)
    (funcall (lispm-websocket--server-get connection :on-close) connection)))

(defun lispm-websocket--filter (connection bytes)
  "Accumulate BYTES from CONNECTION and act on each complete unit.
Before the handshake a unit is the HTTP request; after it, a frame."
  (process-put connection :input (concat (process-get connection :input) bytes))
  (if (process-get connection :open)
      (lispm-websocket--read-frames connection)
    (lispm-websocket--read-handshake connection)))

(defun lispm-websocket--read-handshake (connection)
  "Answer CONNECTION's HTTP upgrade request once it has fully arrived.
A request that is not a WebSocket upgrade, or that the authorizer refuses,
is answered with an HTTP error and the connection is dropped."
  (let* ((input (process-get connection :input))
         (end (string-search "\r\n\r\n" input)))
    (cond
     (end
      (let* ((headers (lispm-local-http-headers (substring input 0 end)))
             (key (cdr (assoc "sec-websocket-key" headers)))
             (protocols (cdr (assoc "sec-websocket-protocol" headers))))
        (process-put connection :input (substring input (+ end 4)))
        (cond
         ((not (and key
                    (string-match-p "\\`GET " input)
                    (string-equal-ignore-case
                     (or (cdr (assoc "upgrade" headers)) "") "websocket")))
          (lispm-websocket--refuse connection "400 Bad Request"))
         ((not (funcall (lispm-websocket--server-get connection :authorize) headers))
          (lispm-websocket--refuse connection "401 Unauthorized"))
         (t
          (process-send-string
           connection
           (concat "HTTP/1.1 101 Switching Protocols\r\n"
                   "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                   "Sec-WebSocket-Accept: "
                   (base64-encode-string
                    (secure-hash 'sha1 (concat key lispm-websocket-guid) nil nil t))
                   "\r\n"
                   ;; The first subprotocol the client offers is accepted:
                   ;; clients fail a handshake that selects none.
                   (if protocols
                       (format "Sec-WebSocket-Protocol: %s\r\n"
                               (car (split-string protocols "[, ]+" t)))
                     "")
                   "\r\n"))
          (process-put connection :open t)
          (funcall (lispm-websocket--server-get connection :on-open) connection)
          (lispm-websocket--read-frames connection)))))
     ((> (length input) 16384)
      (lispm-websocket--refuse connection "431 Request Header Fields Too Large")))))

(defun lispm-websocket--refuse (connection status)
  "Answer CONNECTION with HTTP STATUS and drop it."
  (process-send-string connection
                       (format "HTTP/1.1 %s\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
                               status))
  (delete-process connection))

(defun lispm-websocket--frame (opcode payload)
  "Return a final, unmasked frame of OPCODE carrying the unibyte PAYLOAD."
  (let ((length (length payload)))
    (concat (unibyte-string (logior #x80 opcode))
            (cond ((< length 126) (unibyte-string length))
                  ((< length 65536)
                   (unibyte-string 126 (ash length -8) (logand length #xff)))
                  (t (apply #'unibyte-string 127
                            (mapcar (lambda (shift) (logand (ash length (- shift)) #xff))
                                    '(56 48 40 32 24 16 8 0)))))
            payload)))

(defun lispm-websocket--big-endian (string start count)
  "Return the unsigned integer in COUNT bytes of STRING from START."
  (seq-reduce (lambda (value byte) (logior (ash value 8) byte))
              (substring string start (+ start count))
              0))

(defun lispm-websocket--parse-frame (input)
  "Parse the first frame of the unibyte INPUT.
Return (FIN OPCODE PAYLOAD . CONSUMED), nil while the frame is incomplete,
or `invalid' for an unmasked or oversized frame."
  (when (>= (length input) 2)
    (let* ((fin (/= 0 (logand (aref input 0) #x80)))
           (opcode (logand (aref input 0) #x0f))
           (masked (/= 0 (logand (aref input 1) #x80)))
           (short (logand (aref input 1) #x7f))
           (offset (pcase short (126 4) (127 10) (_ 2)))
           (length (and (>= (length input) offset)
                        (pcase short
                          (126 (lispm-websocket--big-endian input 2 2))
                          (127 (lispm-websocket--big-endian input 2 8))
                          (_ short)))))
      (cond
       ((not masked) 'invalid)
       ((and length (> length lispm-websocket-max-message)) 'invalid)
       ((and length (>= (length input) (+ offset 4 length)))
        (let ((mask (substring input offset (+ offset 4)))
              (payload (substring input (+ offset 4) (+ offset 4 length))))
          (dotimes (i length)
            (aset payload i (logxor (aref payload i) (aref mask (% i 4)))))
          (cl-list* fin opcode payload (+ offset 4 length))))))))

(defun lispm-websocket--read-frames (connection)
  "Act on every complete frame buffered for CONNECTION.
Text messages, possibly fragmented, are decoded and delivered; pings are
answered; a close frame, a binary message or a malformed frame ends the
connection."
  (catch 'done
    (while (process-live-p connection)
      (let ((frame (lispm-websocket--parse-frame (process-get connection :input))))
        (pcase frame
          ('nil (throw 'done nil))
          ('invalid (delete-process connection) (throw 'done nil))
          (`(,fin ,opcode ,payload . ,consumed)
           (process-put connection :input
                        (substring (process-get connection :input) consumed))
           (pcase opcode
             ((or 0 1)
              (let ((message (concat (process-get connection :fragments) payload)))
                (if (> (length message) lispm-websocket-max-message)
                    (delete-process connection)
                  (process-put connection :fragments (unless fin message))
                  (when fin
                    (funcall (lispm-websocket--server-get connection :on-message)
                             connection (decode-coding-string message 'utf-8))))))
             (9 (process-send-string connection (lispm-websocket--frame 10 payload)))
             (10 nil)
             (_ (lispm-websocket-close connection)))))))))

(provide 'lispm-websocket)
;;; lispm-websocket.el ends here
