;;; lispm-backend-form.el --- A model backend's form  -*- lexical-binding: t; -*-

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

;; A backend is a server LispM's agents ask: the API it speaks, an
;; address, a model, and where its key comes from.  `lispm-backend-form'
;; draws its form with LispM UI (lispm-ui-form.el), to add one or change
;; one; `lispm-settings.el' keeps the backend itself.
;;
;; The form keeps no window: what shows it decides where it stands,
;; titled with the form's own title.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-settings)
(require 'lispm-ui-form)

(defun lispm-backend-form--key-source (backend)
  "How BACKEND's key comes, as its form's key-source."
  (let ((key (plist-get backend :key)))
    (cond ((and (consp key) (eq (car key) 'secret)) 'secret)
          ((stringp key) 'typed)
          ((plist-get backend :key-env) 'env)
          ((assq key lispm-key-sources) 'env)
          ((eq key 'auth-source) 'auth-source)
          (t 'none))))

(defun lispm-backend-form--key-source-choices (source)
  "The key sources the form offers, SOURCE the one it shows at first."
  (let ((choices (list (cons 'typed (lispm-tr "Typed here"))
                       (cons 'env (lispm-tr "Environment variable"))
                       (cons 'auth-source (lispm-tr "auth-source"))
                       (cons 'none (lispm-tr "None")))))
    (when (or (lispm-secret-available-p) (eq source 'secret))
      (push (cons 'secret (lispm-tr "Stored encrypted")) choices))
    choices))

(defun lispm-backend-form--models (text model)
  "The models TEXT names, one on a line; MODEL, already the backend's own,
left out.  A list, or nil."
  (delete-dups
   (delete model
           (seq-filter (lambda (name) (not (string-empty-p name)))
                       (mapcar #'string-trim (split-string (or text "") "\n"))))))

(defun lispm-backend-form--with-key (backend name source key variable)
  "BACKEND with its key, as NAME's form has it from SOURCE, KEY and VARIABLE.
Signal an error when the source needs something that is missing."
  (let ((backend (copy-tree backend))
        (old-key (plist-get backend :key))
        (old-variable (plist-get backend :key-env)))
    (cl-remf backend :key)
    (cl-remf backend :key-env)
    (pcase source
      ('secret
       (let ((id (lispm-backend-secret-id name)))
         (when (and (stringp key) (not (string-empty-p key)))
           (lispm-secret-set id key))
         (unless (or (and (stringp key) (not (string-empty-p key)))
                     (lispm-secret-get id t))
           (user-error "%s" (lispm-tr "Type a key, or choose where it comes from")))
         (plist-put backend :key (cons 'secret id))))
      ('typed
       (cond ((and (stringp key) (not (string-empty-p key)))
              (plist-put backend :key key))
             ((stringp old-key) backend)
             (t (user-error "%s" (lispm-tr "Type a key, or choose where it comes from")))))
      ('env
       (when (or (not (stringp variable)) (string-empty-p variable))
         (user-error "%s" (lispm-tr "Name the variable holding the key")))
       (if (and (null old-variable)
                (assq old-key lispm-key-sources)
                (equal variable (nth 1 (assq old-key lispm-key-sources))))
           (plist-put backend :key old-key)
         (plist-put backend :key-env variable)))
      ('auth-source
       (plist-put backend :key 'auth-source))
      ('none backend)
      (_ (user-error "%s" (lispm-tr "Choose where the key comes from"))))))

(defun lispm-backend-form--submit (name values)
  "Keep the backend VALUES describe; NAME nil adds one, else changes it."
  (let* ((new (null name))
         (name (or name (intern (string-trim (alist-get 'name values)))))
         (old (and (not new) (lispm-backend-get name)))
         (backend (if new (list :features '(streaming)) (copy-tree old)))
         (url (string-remove-suffix "/" (string-trim (alist-get 'url values))))
         (model (string-trim (alist-get 'model values)))
         (models (lispm-backend-form--models (alist-get 'models values) model))
         (source (alist-get 'key-source values))
         (key (alist-get 'key values))
         (variable (and (stringp (alist-get 'key-env values))
                        (string-trim (alist-get 'key-env values)))))
    (if new
        (lispm-backend--check-name name)
      (unless old
        (user-error "%s" (lispm-tr "There is no backend %s" name))))
    (setq backend (plist-put backend :api (alist-get 'api values)))
    (setq backend (plist-put backend :url url))
    (setq backend (plist-put backend :model model))
    (if models
        (setq backend (plist-put backend :models models))
      (cl-remf backend :models))
    (setq backend (lispm-backend-form--with-key backend name source key variable))
    (if new
        (lispm-backend-add name backend t)
      (lispm-backend-change name backend t))
    (when (alist-get 'use values)
      (lispm-set-model name nil t))))

(defun lispm-backend-form (&optional name)
  "Make the form that adds a backend, or changes backend NAME; return its buffer.
The form's window shows it; `lispm-backend-form--submit' keeps what it
says, once it is valid."
  (let* ((backend (and name (lispm-backend-get name)))
         (source (if backend
                     (lispm-backend-form--key-source backend)
                   (if (lispm-secret-available-p) 'secret 'typed)))
         (stored-secret (and name
                             (eq source 'secret)
                             (condition-case nil
                                 (lispm-secret-get (lispm-backend-secret-id name) t)
                               (error nil))))
         (stored-key (and backend (stringp (plist-get backend :key))
                          (not (string-empty-p (plist-get backend :key)))))
         (variable (and backend (lispm--key-variable backend)))
         (use-p (or (null name) (not (eq name (lispm-backend-name)))))
         (fields
          (append
           (unless name
             `((name :type text :label ,(lispm-tr "Name") :required t
                     :value ""
                     :validate ,(lambda (value)
                                  (when (stringp value)
                                    (let ((name (intern (string-trim value))))
                                      (cond ((assq name lispm-backends)
                                             (lispm-tr "There is already a backend %s" name))
                                            ((not (string-match-p
                                                   "\\`[[:alnum:]][[:alnum:]_.-]*\\'"
                                                   (symbol-name name)))
                                             (lispm-tr "Use letters, digits, `-', `_' or `.'"))))))
                     :help ,(lispm-tr "A name to know the backend by, as `my-gateway'."))))
           `((api :type radio :label ,(lispm-tr "API")
                  :choices ,(mapcar (lambda (api)
                                      (cons (car api) (lispm-backend-api-name (car api))))
                                    lispm-backend-apis)
                  :value ,(or (plist-get backend :api) 'openai)
                  :help ,(lispm-tr "The API the server speaks; LispM adds `/v1/messages' or `/chat/completions'."))
             (url :type text :label ,(lispm-tr "Address") :required t
                  :value ,(or (plist-get backend :url) "")
                  :validate ,(lambda (value)
                               (unless (and (stringp value)
                                            (string-match-p "\\`https?://[^/[:space:]]+" value))
                                 (lispm-tr "Starts with http:// or https://")))
                  :help ,(lispm-tr "The server's base address, without the path LispM adds."))
             (model :type text :label ,(lispm-tr "Model") :required t
                    :value ,(or (plist-get backend :model) "")
                    :help ,(lispm-tr "The model agents use when they name none."))
             (models :type text-area :label ,(lispm-tr "Other models") :lines 4
                     :value ,(mapconcat #'identity (plist-get backend :models) "\n")
                     :help ,(lispm-tr "One on a line; they appear in the model menu."))
             (key-source :type choice :label ,(lispm-tr "Key") :required t
                         :choices ,(lispm-backend-form--key-source-choices source)
                         :value ,source
                         :help ,(lambda (values)
                                  (pcase (alist-get 'key-source values)
                                    ('secret (if stored-secret
                                                 (lispm-tr "A key is stored encrypted; leave it empty to keep it.")
                                               (lispm-tr "LispM Auth keeps it encrypted.")))
                                    ('typed (if stored-key
                                                (lispm-tr "A key is set; leave it empty to keep it, or type a new one.")
                                              (lispm-tr "Saved in Customize in plain text.")))
                                    ('env (lispm-tr "The variable holding the key."))
                                    ('auth-source (lispm-tr "Looked up for the server's host as an agent first asks."))
                                    (_ (lispm-tr "The server asks for no key.")))))
             (key :type password :label ,(lispm-tr "The key")
                  :value ""
                  :when ,(lambda (values)
                           (memq (alist-get 'key-source values) '(secret typed)))
                  :help ,(lispm-tr "Never shown again; leave it empty to keep the one there is."))
             (key-env :type text :label ,(lispm-tr "Variable")
                      :value ,(or variable "")
                      :when ,(lambda (values)
                               (eq (alist-get 'key-source values) 'env))
                      :required t
                      :validate ,(lambda (value)
                                   (unless (and (stringp value)
                                                (string-match-p
                                                 "\\`[[:alpha:]_][[:alnum:]_]*\\'" value))
                                     (lispm-tr "A variable's name, as ANTHROPIC_API_KEY")))
                      :help ,(lispm-tr "The environment variable holding the key."))
             (use :type check :text ,(lispm-tr "Use this backend for agents that name no model")
                  :value ,(and (null name) (null lispm-backend))
                  :when ,(lambda (_values) use-p))))))
    (lispm-ui-form
     (if name (lispm-tr "Backend %s" name) (lispm-tr "Add Backend"))
     fields
     :intro (lispm-tr "A server that speaks the Anthropic or OpenAI API, with where its key comes from.")
     :submit (lispm-tr "Save")
     :cancel (lispm-tr "Cancel")
     :on-submit (lambda (values) (lispm-backend-form--submit name values)))))

(provide 'lispm-backend-form)
;;; lispm-backend-form.el ends here
