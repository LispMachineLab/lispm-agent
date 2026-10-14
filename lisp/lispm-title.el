;;; lispm-title.el --- Sessions that title themselves  -*- lexical-binding: t; -*-

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

;; A session is listed under a title: one the user gave it with
;; `lispm-rename-session', or, until then, the first line of its first
;; prompt.  Once a turn has ended, an untitled session asks the model,
;; in a small request of its own, for a title summing it up, in the
;; user's language or in `lispm-title-language', and records it as its
;; #+lispm_title.  A title the user gave is never replaced.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-api)
(require 'lispm-runtime)

(defcustom lispm-auto-title t
  "Non-nil for sessions to be titled by the model after their first turn."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-title-model nil
  "Model titling sessions, on the agent's backend; nil for the agent's model."
  :type '(choice (const :tag "The agent's model" nil) string)
  :group 'lispm)

(defcustom lispm-title-language nil
  "Language sessions are titled in, such as \"English\"; nil for the user's."
  :type '(choice (const :tag "The language the user writes in" nil) string)
  :group 'lispm)

(defvar lispm-title-change-functions nil
  "Abnormal hook run with an agent whose session got a new title.")

(defconst lispm-title--prompt
  "You title conversations between a user and an AI agent. Reply with the title only: \
a few words (at most 6, or 15 characters of Chinese or Japanese) naming the task or \
topic, in %s. No quotes, no final punctuation."
  "System prompt of the request for a title; %s is the language.")

(defun lispm-title--system ()
  "The system prompt asking for a title in `lispm-title-language'."
  (format lispm-title--prompt (or lispm-title-language "the language the user writes in")))

(defvar lispm-title--asked (make-hash-table :test #'equal)
  "Ids of the sessions a title was asked for in this Emacs session.")

(defun lispm-session-title (agent)
  "The #+lispm_title of AGENT's session, or nil."
  (when (buffer-live-p (lispm-agent-buffer agent))
    (with-current-buffer (lispm-agent-buffer agent)
      (save-restriction
        (widen)
        (cdr (assoc "title" (lispm-transcript-keywords)))))))

(defun lispm-title--first-prompt (agent)
  "The text of the first message in AGENT's session."
  (with-current-buffer (lispm-agent-buffer agent)
    (let ((first (car (plist-get (lispm-transcript-parse) :messages))))
      (alist-get 'text (seq-find (lambda (block) (equal (alist-get 'type block) "text"))
                                 (alist-get 'content first))))))

(defun lispm-title--clean (text)
  "TEXT, a model's reply, as a title: its first line, without quotes."
  (let ((line (string-trim (car (split-string (string-trim (or text "")) "\n")))))
    (setq line (string-trim line "[\"'“”‘’「」『』*#` \t]+" "[\"'“”‘’「」『』*` \t.。!！]+"))
    (and (not (string-empty-p line)) (truncate-string-to-width line 80))))

(defun lispm-title--ask (agent reply)
  "Ask the model for a title of AGENT's session, which ended a turn with REPLY."
  (let* ((prompt (lispm-title--first-prompt agent))
         (file (buffer-file-name (lispm-agent-buffer agent)))
         (backend (lispm-agent-backend* agent))
         (request (lispm-api-request
                   backend (or lispm-title-model (lispm-agent-model* agent)) "low"
                   (lispm-title--system) nil
                   `(((role . "user")
                      (content . [((type . "text")
                                   (text . ,(format "The user wrote:\n%s\n\nThe agent replied:\n%s"
                                                    (lispm--truncate (or prompt "") 3000)
                                                    (lispm--truncate (or reply "") 2000))))]))))))
    (funcall lispm-api-function backend (car request) (cdr request)
             :on-done
             (lambda (message)
               (let ((title (lispm-title--clean
                             (lispm--reply-text (append (alist-get 'content message) nil)))))
                 ;; Unless the user titled it meanwhile.
                 (when (and title file (file-exists-p file) (not (lispm-session-title agent)))
                   (lispm-rename-session file title)
                   (run-hook-with-args 'lispm-title-change-functions agent))))
             :on-error #'ignore)))

(defun lispm-title--maybe (agent reply)
  "Title AGENT's session, untitled until its turn ended with REPLY."
  (when (and lispm-auto-title
             (lispm-backend-name agent)
             (buffer-live-p (lispm-agent-buffer agent))
             (buffer-file-name (lispm-agent-buffer agent))
             (not (gethash (lispm-agent-id agent) lispm-title--asked))
             (not (lispm-session-title agent)))
    (puthash (lispm-agent-id agent) t lispm-title--asked)
    (condition-case err
        (lispm-title--ask agent reply)
      (error (message "LispM: could not title %s: %s"
                      (lispm-agent-name agent) (error-message-string err))))))

(add-hook 'lispm-turn-end-functions #'lispm-title--maybe)

(provide 'lispm-title)
;;; lispm-title.el ends here
