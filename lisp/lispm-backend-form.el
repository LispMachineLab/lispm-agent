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

(defconst lispm-backend-presets
  `((custom :label ,(lispm-tr-noop "Custom"))
    (openrouter :label "OpenRouter"
                :api openai
                :url "https://openrouter.ai/api/v1"
                :model "anthropic/claude-sonnet-4.5"
                :models ("openai/gpt-5.4" "google/gemini-2.5-pro" "deepseek/deepseek-v3.2"))
    (deepseek :label "DeepSeek"
              :api openai
              :url "https://api.deepseek.com/v1"
              :model "deepseek-chat"
              :models ("deepseek-reasoner"))
    (gemini :label "Google Gemini"
            :api openai
            :url "https://generativelanguage.googleapis.com/v1beta/openai"
            :model "gemini-2.5-pro"
            :models ("gemini-2.5-flash"))
    (ollama :label ,(lispm-tr-noop "Ollama (local)")
            :api openai
            :url "http://localhost:11434/v1"
            :model "qwen3-coder"
            :key-source none)
    (lm-studio :label ,(lispm-tr-noop "LM Studio (local)")
               :api openai
               :url "http://localhost:1234/v1"
               :model "qwen3-coder"
               :key-source none))
  "Presets offered when adding a backend in `lispm-backend-form'.")

(defun lispm-backend-form--preset-choices ()
  "The preset choices offered when adding a backend."
  (mapcar (pcase-lambda (`(,key . ,props))
            (cons key (lispm-tr (plist-get props :label))))
          lispm-backend-presets))

(defun lispm-backend-form--apply-preset (preset)
  "Fill the current backend form from PRESET, a key of `lispm-backend-presets'."
  (when-let* ((props (cdr (assq preset lispm-backend-presets)))
              ((not (eq preset 'custom))))
    (let ((default-source (if (lispm-secret-available-p) 'secret 'typed)))
      (lispm-ui-form-set
       'name (if (assq preset lispm-backends) "" (symbol-name preset))
       'api (or (plist-get props :api) 'openai)
       'url (or (plist-get props :url) "")
       'model (or (plist-get props :model) "")
       'key-source (or (plist-get props :key-source) default-source)))))

(defun lispm-backend-form--key-source (backend)
  "How BACKEND's key comes, as its form's key-source."
  (let ((key (plist-get backend :key)))
    (cond ((and (consp key) (eq (car key) 'secret)) 'secret)
          ((stringp key) 'typed)
          ((plist-get backend :key-env) 'env)
          ((assq key lispm-key-sources)
           (let ((variable (nth 1 (assq key lispm-key-sources))))
             (if (or (and variable (not (string-empty-p (or (getenv variable) ""))))
                     (not (lispm-secret-available-p)))
                 'env
               'secret)))
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
         (preset-props (and new (cdr (assq (alist-get 'preset values) lispm-backend-presets))))
         (backend (if new (list :features '(streaming)) (copy-tree old)))
         (url (string-remove-suffix "/" (string-trim (alist-get 'url values))))
         (model (string-trim (alist-get 'model values)))
         (models (if (assq 'models values)
                     (lispm-backend-form--models (alist-get 'models values) model)
                   (delete model (copy-sequence (plist-get (or old preset-props) :models)))))
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
             `((preset :type choice :label ,(lispm-tr "Preset")
                       :choices ,(lispm-backend-form--preset-choices)
                       :value custom
                       :on-change ,#'lispm-backend-form--apply-preset
                       :help ,(lispm-tr "Fill in the address and models of a known provider."))
               (name :type text :label ,(lispm-tr "Name") :required t
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
                  :value ,(null lispm-backend)
                  :when ,(lambda (_values) use-p)))))
         (buffer
          (lispm-ui-form
           (if name (lispm-tr "Backend %s" name) (lispm-tr "Add Backend"))
           fields
           :intro (lispm-tr "A server that speaks the Anthropic or OpenAI API, with where its key comes from.")
           :submit (lispm-tr "Save")
           :cancel (lispm-tr "Cancel")
           :on-submit (lambda (values) (lispm-backend-form--submit name values)))))
    (when (and name backend (not (lispm--backend-key-at-hand-p backend)))
      (with-current-buffer buffer
        (when-let* ((widget (alist-get 'key (lispm-ui-form-widgets buffer))))
          (goto-char (or (widget-field-start widget) (widget-get widget :from))))))
    buffer))

(defun lispm-backend-form-show (buffer)
  "Show BUFFER, a backend form, and return it."
  (pop-to-buffer-same-window buffer)
  (when-let* ((window (get-buffer-window buffer)))
    (set-window-start window (point-min)))
  buffer)

(defvar lispm-backend-form-show-function #'lispm-backend-form-show
  "Function showing a backend form buffer, called with the buffer.")

(defun lispm-model-setup-form--provider-choices ()
  "The provider choices offered in `lispm-model-setup-form'."
  (let ((existing
         (mapcar (pcase-lambda (`(,name . ,_))
                   (cons name (pcase name
                                ('anthropic "Anthropic")
                                ('openai "OpenAI")
                                (_ (symbol-name name)))))
                 lispm-backends))
        (presets
         (cl-loop for (key . props) in lispm-backend-presets
                  unless (or (eq key 'custom) (assq key lispm-backends))
                  collect (cons key (lispm-tr (plist-get props :label))))))
    (append existing presets (list (cons 'custom (lispm-tr "Custom"))))))

(defun lispm-model-setup-form--props (provider)
  "The backend properties for PROVIDER in `lispm-model-setup-form'."
  (or (lispm-backend-get provider)
      (cdr (assq provider lispm-backend-presets))))

(defun lispm-model-setup-form--select-provider (provider)
  "Fill the model setup form for PROVIDER."
  (if (eq provider 'custom)
      (lispm-ui-form-set
       'url ""
       'model "")
    (when-let* ((props (lispm-model-setup-form--props provider)))
      (lispm-ui-form-set
       'api (or (plist-get props :api) 'openai)
       'url (or (plist-get props :url) "")
       'model (or (plist-get props :model) "")))))

(defun lispm-model-setup-form--submit (values)
  "Save the backend and model VALUES describe, and make it LispM's model."
  (let* ((provider (alist-get 'provider values))
         (props (lispm-model-setup-form--props provider))
         (name (if (eq provider 'custom)
                   (intern (string-trim (or (alist-get 'name values) "")))
                 provider))
         (old (lispm-backend-get name))
         (backend (if old (copy-tree old) (list :features '(streaming))))
         (api (or (alist-get 'api values) (plist-get props :api) 'openai))
         (url (string-remove-suffix
               "/" (string-trim (or (alist-get 'url values)
                                    (plist-get props :url) ""))))
         (model (string-trim (or (alist-get 'model values)
                                 (plist-get props :model) "")))
         (models (if (assq 'models values)
                     (lispm-backend-form--models (alist-get 'models values) model)
                   (delete model (copy-sequence (plist-get props :models)))))
         (key (string-trim (or (alist-get 'key values) "")))
         (source (cond ((alist-get 'key-source values))
                       ((eq (plist-get props :key-source) 'none) 'none)
                       ((and (string-empty-p key)
                             old
                             (lispm--backend-key-at-hand-p old))
                        (lispm-backend-form--key-source old))
                       ((and (eq provider 'custom) (string-empty-p key))
                        'none)
                       ((lispm-secret-available-p) 'secret)
                       (t 'typed)))
         (variable (or (and (stringp (alist-get 'key-env values))
                            (string-trim (alist-get 'key-env values)))
                       (and old (lispm--key-variable old)))))
    (unless (and (symbolp name)
                 (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" (symbol-name name)))
      (user-error "%s" (lispm-tr "Use letters, digits, `-', `_' or `.'")))
    (setq backend (plist-put backend :api api))
    (setq backend (plist-put backend :url url))
    (setq backend (plist-put backend :model model))
    (if models
        (setq backend (plist-put backend :models models))
      (cl-remf backend :models))
    (setq backend (lispm-backend-form--with-key backend name source key variable))
    (if old
        (lispm-backend-change name backend t)
      (lispm-backend-add name backend t))
    (lispm-set-model name model t)))

(defun lispm-model-setup-form ()
  "Make the form that sets up LispM's model and provider; return its buffer."
  (let* ((choices (lispm-model-setup-form--provider-choices))
         (provider (or (lispm-backend-name) (caar choices) 'custom))
         (props (lispm-model-setup-form--props provider))
         (active-model (or (and (eq provider (lispm-backend-name))
                                (lispm-default-model))
                           (plist-get props :model)
                           ""))
         (custom-p (lambda (values) (eq (alist-get 'provider values) 'custom)))
         (address-p (lambda (values)
                      (memq (alist-get 'provider values) '(custom ollama lm-studio))))
         (key-p (lambda (values)
                  (let* ((prov (alist-get 'provider values))
                         (p (lispm-model-setup-form--props prov)))
                    (not (or (memq prov '(ollama lm-studio))
                             (eq (plist-get p :key-source) 'none)
                             (and (assq prov lispm-backends)
                                  (eq (lispm-backend-form--key-source p) 'none)))))))
         (fields
          `((model-group :type heading :label ,(lispm-tr "Model Provider"))
            (provider :type choice :label ,(lispm-tr "Provider")
                      :choices ,choices
                      :value ,provider
                      :on-change ,#'lispm-model-setup-form--select-provider
                      :help ,(lispm-tr "Choose the provider or server LispM's agents ask."))
            (name :type text :label ,(lispm-tr "Name") :required t
                  :value ""
                  :when ,custom-p
                  :validate ,(lambda (value)
                               (when (stringp value)
                                 (let ((name (intern (string-trim value))))
                                   (unless (string-match-p
                                            "\\`[[:alnum:]][[:alnum:]_.-]*\\'"
                                            (symbol-name name))
                                     (lispm-tr "Use letters, digits, `-', `_' or `.'")))))
                  :help ,(lispm-tr "A name to know the backend by, as `my-gateway'."))
            (api :type radio :label ,(lispm-tr "API")
                 :choices ,(mapcar (lambda (api)
                                     (cons (car api) (lispm-backend-api-name (car api))))
                                   lispm-backend-apis)
                 :value ,(or (plist-get props :api) 'openai)
                 :when ,custom-p
                 :help ,(lispm-tr "The API the server speaks; LispM adds `/v1/messages' or `/chat/completions'."))
            (url :type text :label ,(lispm-tr "Address") :required t
                 :value ,(or (plist-get props :url) "")
                 :when ,address-p
                 :validate ,(lambda (value)
                              (unless (and (stringp value)
                                           (string-match-p "\\`https?://[^/[:space:]]+" value))
                                (lispm-tr "Starts with http:// or https://")))
                 :help ,(lispm-tr "The server's base address, without the path LispM adds."))
            (model :type text :label ,(lispm-tr "Model") :required t
                   :value ,active-model
                   :help ,(lispm-tr "The model agents use when they name none."))
            (key :type password :label ,(lispm-tr "The key")
                 :value ""
                 :when ,key-p
                 :help ,(lambda (values)
                          (let* ((prov (alist-get 'provider values))
                                 (back (and prov (not (eq prov 'custom))
                                            (lispm-backend-get prov))))
                            (if (and back (lispm--backend-key-at-hand-p back))
                                (lispm-tr "A key is stored encrypted; leave it empty to keep it.")
                              (lispm-tr "LispM Auth keeps it encrypted.")))))))
         (buffer
          (lispm-ui-form
           (lispm-tr "Set Up a Model")
           fields
           :intro (lispm-tr "Choose a model provider and enter its API key, or choose a local server.")
           :submit (lispm-tr "Use Model")
           :cancel (lispm-tr "Cancel")
           :setup (lambda ()
                    (when lispm-ui-form-show-header
                      (setq-local lispm-ui-form-width 28)))
           :on-submit #'lispm-model-setup-form--submit)))
    (with-current-buffer buffer
      (let* ((need-key (and (funcall key-p (list (cons 'provider provider)))
                            (not (and props (lispm--backend-key-at-hand-p props)))))
             (target (if need-key 'key 'model)))
        (when-let* ((widget (alist-get target (lispm-ui-form-widgets buffer))))
          (goto-char (or (widget-field-start widget) (widget-get widget :from))))))
    buffer))

;;;###autoload
(defun lispm-setup-model ()
  "Open the form that sets up a model provider and API key for LispM."
  (interactive)
  (funcall lispm-backend-form-show-function (lispm-model-setup-form)))

(defcustom lispm-setup-on-start t
  "Non-nil to offer `lispm-setup-model' once when LispM starts with no model."
  :type 'boolean
  :group 'lispm)

(defvar lispm--setup-offered nil
  "Non-nil once `lispm-maybe-setup-model' has offered model setup in this session.")

(defun lispm-maybe-setup-model (&optional agent)
  "Offer `lispm-setup-model' once when AGENT has no model backend."
  (when (and lispm-setup-on-start
             (not noninteractive)
             (not (bound-and-true-p ert--running-tests))
             (not lispm--setup-offered))
    (if (and (null (lispm-backend-name agent))
             (not (seq-some (pcase-lambda (`(,_ . ,props))
                              (let ((key (plist-get props :key)))
                                (and (consp key) (eq (car key) 'secret))))
                            lispm-backends)))
        (progn
          (setq lispm--setup-offered t)
          (lispm-setup-model)
          (unless noninteractive
            (redisplay t))
          (lispm-ensure-auth))
      (lispm-ensure-auth)
      (when (null (lispm-backend-name agent))
        (setq lispm--setup-offered t)
        (lispm-setup-model)))))

(provide 'lispm-backend-form)
;;; lispm-backend-form.el ends here
