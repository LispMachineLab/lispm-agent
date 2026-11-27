;;; lispm-secrets.el --- Secret store interface and LispM Auth bridge -*- lexical-binding: t; -*-

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

;; Secret storage hooks and LispM Auth lifecycle bridge.
;; LispM keeps no secret itself: an external store (such as `lispm-auth')
;; attaches to these hooks, and LispM unlocks or queries keys only on demand.

;;; Code:

(require 'lispm-util)
(require 'lispm-auth)

(defvar lispm-secret-get-functions nil
  "Functions reading a secret from a store, for `lispm-secret-get'.
Each is called with ID and NO-PROMPT and returns the secret, a string;
nil when it does not keep ID.  NO-PROMPT non-nil asks it never to ask
the user for the password, answering nil while the store is locked.")

(defvar lispm-secret-set-functions nil
  "Functions storing a secret, for `lispm-secret-set'.
Each is called with ID and SECRET, and returns non-nil when it stored
it.  One may ask for the password if the store is locked.")

(defvar lispm-secret-delete-functions nil
  "Functions removing a secret, for `lispm-secret-delete'.
Each is called with ID and returns non-nil when it removed it.")

(defvar lispm-secret-available-functions nil
  "Functions saying whether a secret store is here.
Each is called with no argument and returns non-nil when it is, as
`lispm-secret-available-p' answers.")

(defvar lispm-secret-exists-functions nil
  "Functions checking whether a secret exists, for `lispm-secret-exists-p'.
Each is called with ID and returns non-nil when the store keeps ID
without prompting or decrypting.")

(defvar lispm--auth-started nil
  "Non-nil once `lispm-ensure-auth' has opened LispM Auth for this session.")

(defun lispm--ensure-auth-service ()
  "Ensure LispM Auth's service is installed when running interactively."
  (when (and (not noninteractive)
             (not (bound-and-true-p ert--running-tests))
             (fboundp 'lispm-auth--service-program)
             (fboundp 'lispm-auth-install-service)
             (null (bound-and-true-p lispm-auth-service-program)))
    (condition-case nil
        (lispm-auth--service-program)
      (lispm-auth-unavailable
       (ignore-errors (lispm-auth-install-service))))))

(defun lispm-ensure-auth ()
  "Ensure LispM Auth is ready and unlocked for `lispm' when interactive."
  (when (and (not noninteractive)
             (not (bound-and-true-p ert--running-tests))
             (not lispm--auth-started)
             (memq #'lispm-auth--secret-available lispm-secret-available-functions))
    (setq lispm--auth-started t)
    (lispm--ensure-auth-service)
    (when (and (fboundp 'lispm-auth-store-exists-p)
               (lispm-auth-store-exists-p)
               (or (not (fboundp 'lispm-auth-locked-p))
                   (lispm-auth-locked-p))
               (or (null (bound-and-true-p lispm-auth-page-window-draw-function))
                   (bound-and-true-p lispm-auth-page-windowed)
                   (and (fboundp 'lispm-auth--started-hidden-p)
                        (lispm-auth--started-hidden-p)))
               (fboundp 'lispm-auth-start-app))
      (lispm-auth-start-app 'lispm))))

(defun lispm-secret-available-p ()
  "Return non-nil when a program keeps secrets for LispM."
  (when (memq #'lispm-auth--secret-available lispm-secret-available-functions)
    (lispm--ensure-auth-service))
  (and (run-hook-with-args-until-success 'lispm-secret-available-functions) t))

(defun lispm-secret-get (id &optional no-prompt)
  "Return the secret ID keeps, or nil when there is none.
NO-PROMPT non-nil asks the store not to ask the user for its password,
as a window of settings does when it only says how a backend stands."
  (run-hook-with-args-until-success 'lispm-secret-get-functions id no-prompt))

(defun lispm-secret-exists-p (id)
  "Return non-nil when ID has a stored secret without prompting or decrypting.
Uses `lispm-secret-exists-functions' when registered; falls back to a
non-prompting `lispm-secret-get' check otherwise."
  (if lispm-secret-exists-functions
      (and (run-hook-with-args-until-success 'lispm-secret-exists-functions id) t)
    (let ((secret (lispm-secret-get id t)))
      (and (stringp secret) (not (string-empty-p secret))))))

(defun lispm-secret-set (id secret)
  "Keep SECRET as ID in the secret store.
The store may ask the user for its password.  Signal an error when
there is no store: install LispM Auth for one."
  (when (memq #'lispm-auth--secret-set lispm-secret-set-functions)
    (lispm--ensure-auth-service))
  (unless (run-hook-with-args-until-success 'lispm-secret-set-functions id secret)
    (user-error "No secret store: install LispM Auth to keep keys encrypted")))

(defun lispm-secret-delete (id)
  "Remove the secret ID from the secret store; return non-nil if it did."
  (run-hook-with-args-until-success 'lispm-secret-delete-functions id))

(provide 'lispm-secrets)
;;; lispm-secrets.el ends here
