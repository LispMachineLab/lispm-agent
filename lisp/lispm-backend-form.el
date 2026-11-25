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
             ((stringp old-key)
              (plist-put backend :key old-key))
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
        (lispm-backend-check-name name)
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

(defun lispm-backend-form--validate-url (value)
  "Validate that VALUE is an HTTP or HTTPS base address."
  (unless (and (stringp value)
               (string-match-p "\\`https?://[^/[:space:]]+" value))
    (lispm-tr "Starts with http:// or https://")))

(defun lispm-backend-form--key-help (stored-secret stored-key)
  "Return a help function for `key-source' given STORED-SECRET and STORED-KEY."
  (lambda (values)
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

(defun lispm-backend-form--new-fields ()
  "Return the preset and name fields used when adding a new backend."
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
                             (ignore-errors
                               (lispm-secret-get (lispm-backend-secret-id name) t))))
         (stored-key (and backend (stringp (plist-get backend :key))
                          (not (string-empty-p (plist-get backend :key)))))
         (variable (and backend (lispm-key-variable backend)))
         (use-p (or (null name) (not (eq name (lispm-backend-name)))))
         (fields
          (append
           (unless name
             (lispm-backend-form--new-fields))
           `((api :type radio :label ,(lispm-tr "API")
                  :choices ,(mapcar (lambda (api)
                                      (cons (car api) (lispm-backend-api-name (car api))))
                                    lispm-backend-apis)
                  :value ,(or (plist-get backend :api) 'openai)
                  :help ,(lispm-tr "The API the server speaks; LispM adds `/v1/messages' or `/chat/completions'."))
             (url :type text :label ,(lispm-tr "Address") :required t
                  :value ,(or (plist-get backend :url) "")
                  :validate ,#'lispm-backend-form--validate-url
                  :help ,(lispm-tr "The server's base address, without the path LispM adds."))
             (model :type text :label ,(lispm-tr "Model") :required t
                    :value ,(or (plist-get backend :model) "")
                    :help ,(lispm-tr "The model agents use when they name none."))
             (key-source :type choice :label ,(lispm-tr "Key") :required t
                         :choices ,(lispm-backend-form--key-source-choices source)
                         :value ,source
                         :help ,(lispm-backend-form--key-help stored-secret stored-key))
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
    (when (and name backend (not (lispm-backend-key-at-hand-p backend)))
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
                             (lispm-backend-key-at-hand-p old))
                        (lispm-backend-form--key-source old))
                       ((and (eq provider 'custom) (string-empty-p key))
                        'none)
                       ((lispm-secret-available-p) 'secret)
                       (t 'typed)))
         (variable (or (and (stringp (alist-get 'key-env values))
                            (string-trim (alist-get 'key-env values)))
                       (and old (lispm-key-variable old)))))
    (unless (and (symbolp name)
                 (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" (symbol-name name)))
      (user-error "%s" (lispm-tr "Use letters, digits, `-', `_' or `.'")))
    (when (and (eq provider 'custom) old)
      (user-error "%s" (lispm-tr "There is already a backend %s" name)))
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
                                 (let* ((trimmed (string-trim value))
                                        (name (intern trimmed)))
                                   (cond ((not (string-match-p
                                                "\\`[[:alnum:]][[:alnum:]_.-]*\\'"
                                                trimmed))
                                          (lispm-tr "Use letters, digits, `-', `_' or `.'"))
                                         ((lispm-backend-get name)
                                          (lispm-tr "There is already a backend %s" name))))))
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
                 :validate ,#'lispm-backend-form--validate-url
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
                            (if (and back (lispm-backend-key-at-hand-p back))
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
                            (not (and props (lispm-backend-key-at-hand-p props)))))
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

;;;; MCP server form and skill form

(defconst lispm-mcp-presets
  `((custom :label ,(lispm-tr-noop "Custom"))
    (peekaboo :label "Peekaboo"
              :command "npx"
              :args ("-y" "@steipete/peekaboo" "mcp")
              :description "See the macOS screen and use its applications."
              :sandbox none)
    (filesystem :label "Filesystem"
                :command "npx"
                :args ("-y" "@modelcontextprotocol/server-filesystem" "~")
                :description "Read, search and manage files in the allowed directories.")
    (fetch :label "Fetch"
           :command "uvx"
           :args ("mcp-server-fetch")
           :description "Fetch web pages and convert HTML to markdown.")
    (github :label "GitHub"
            :command "npx"
            :args ("-y" "@modelcontextprotocol/server-github")
            :description "Search repositories, issues, pull requests and code on GitHub.")
    (memory :label "Memory"
            :command "npx"
            :args ("-y" "@modelcontextprotocol/server-memory")
            :description "Keep a knowledge graph of entities and relations."))
  "Presets offered when adding an MCP server in `lispm-mcp-form'.")

(defun lispm-mcp-form--preset-choices ()
  "The preset choices offered when adding an MCP server."
  (mapcar (pcase-lambda (`(,key . ,props))
            (cons key (lispm-tr (plist-get props :label))))
          lispm-mcp-presets))

(defun lispm-mcp-form--apply-preset (preset)
  "Fill the current MCP server form from PRESET, a key of `lispm-mcp-presets'."
  (when-let* ((props (cdr (assq preset lispm-mcp-presets)))
              ((not (eq preset 'custom))))
    (let ((name (symbol-name preset)))
      (lispm-ui-form-set
       'name (if (assoc name lispm-mcp-servers) "" name)
       'transport (if (plist-get props :url) 'http 'stdio)
       'command (or (plist-get props :command) "")
       'args (string-join (plist-get props :args) " ")
       'description (or (plist-get props :description) "")
       'sandbox (or (plist-get props :sandbox) 'default)))))

(defun lispm-mcp-form--env-text (env)
  "Format ENV, a plist of environment variables, as KEY=VALUE words."
  (string-join
   (cl-loop for (key val) on env by #'cddr
            for k = (string-trim-left (symbol-name key) ":")
            for v = (format "%s" val)
            collect (format "%s=%s" k
                            (if (or (string-empty-p v) (string-match-p "[ \t\n,\"'\\\\]" v))
                                (prin1-to-string v)
                              v)))
   " "))

(defun lispm-mcp-form--parse-env (text)
  "Parse TEXT, KEY=VALUE entries separated by spaces or commas, into a plist.
Values containing spaces or commas may be enclosed in single or double quotes."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let (plist)
      (while (progn (skip-chars-forward " \t\n,") (not (eobp)))
        (unless (looking-at "\\([[:alpha:]_][[:alnum:]_]*\\)=")
          (let ((bad (buffer-substring (point) (progn (skip-chars-forward "^ \t\n,") (point)))))
            (user-error "Write environment variables as KEY=VALUE, not %s" bad)))
        (let ((key (intern (concat ":" (match-string 1))))
              (parts nil))
          (goto-char (match-end 0))
          (while (and (not (eobp)) (not (memq (char-after) '(?\s ?\t ?\n ?,))))
            (let ((ch (char-after)))
              (cond
               ((memq ch '(?\" ?\'))
                (let ((quote-ch ch)
                      (start (progn (forward-char 1) (point)))
                      (closed nil))
                  (while (and (not (eobp)) (not closed))
                    (let ((c (char-after)))
                      (cond
                       ((and (eq quote-ch ?\") (eq c ?\\) (< (1+ (point)) (point-max)))
                        (push (buffer-substring start (point)) parts)
                        (forward-char 1)
                        (setq start (point))
                        (forward-char 1))
                       ((eq c quote-ch)
                        (push (buffer-substring start (point)) parts)
                        (forward-char 1)
                        (setq closed t))
                       (t (forward-char 1)))))
                  (unless closed
                    (user-error "Write environment variables as KEY=VALUE: unterminated quote"))))
               (t
                (let ((start (point)))
                  (skip-chars-forward "^ \t\n,\"'")
                  (push (buffer-substring start (point)) parts))))))
          (setq plist (plist-put plist key (apply #'concat (nreverse parts))))))
      plist)))

(defun lispm-mcp-form--submit (name values)
  "Keep the MCP server VALUES describe; NAME nil adds one, else changes it."
  (let* ((new (null name))
         (name (or name (string-trim (or (alist-get 'name values) ""))))
         (old (and (not new) (lispm-settings-mcp-get name)))
         (transport (or (alist-get 'transport values) 'stdio))
         (command (string-trim (or (alist-get 'command values) "")))
         (args (and (eq transport 'stdio)
                    (split-string-and-unquote (string-trim (or (alist-get 'args values) "")))))
         (env (and (eq transport 'stdio)
                   (lispm-mcp-form--parse-env (alist-get 'env values))))
         (url (string-trim (or (alist-get 'url values) "")))
         (token (string-trim (or (alist-get 'token values) "")))
         (description (string-trim (or (alist-get 'description values) "")))
         (defer (alist-get 'defer values))
         (permission (or (alist-get 'permission values) 'mcp))
         (sandbox (alist-get 'sandbox values))
         (config (copy-tree old)))
    (dolist (key '(:command :args :env :url :token :description :defer :permission :sandbox))
      (cl-remf config key))
    (if (eq transport 'http)
        (progn
          (setq config (plist-put config :url url))
          (let ((kept-token (if (not (string-empty-p token))
                                token
                              (plist-get old :token))))
            (when (and (stringp kept-token) (not (string-empty-p kept-token)))
              (setq config (plist-put config :token kept-token)))))
      (setq config (plist-put config :command command))
      (when args
        (setq config (plist-put config :args args)))
      (when env
        (setq config (plist-put config :env env)))
      (when (and sandbox (not (eq sandbox 'default)))
        (setq config (plist-put config :sandbox sandbox))))
    (unless (string-empty-p description)
      (setq config (plist-put config :description description)))
    (unless defer
      (setq config (plist-put config :defer nil)))
    (unless (eq permission 'mcp)
      (setq config (plist-put config :permission permission)))
    (if new
        (lispm-settings-mcp-add name config t)
      (lispm-settings-mcp-change name config t))))

(defun lispm-mcp-form--new-fields ()
  "Return the preset and name fields used when adding a new MCP server."
  `((preset :type choice :label ,(lispm-tr "Preset")
            :choices ,(lispm-mcp-form--preset-choices)
            :value custom
            :on-change ,#'lispm-mcp-form--apply-preset
            :help ,(lispm-tr "Fill in the command and arguments of a known MCP server."))
    (name :type text :label ,(lispm-tr "Name") :required t
          :value ""
          :validate ,(lambda (value)
                       (when (stringp value)
                         (let ((trimmed (string-trim value)))
                           (cond ((assoc trimmed lispm-mcp-servers)
                                  (lispm-tr "There is already an MCP server %s" trimmed))
                                 ((not (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" trimmed))
                                  (lispm-tr "Use letters, digits, `-', `_' or `.'"))))))
          :help ,(lispm-tr "A short name for the server, as `peekaboo' or `github'."))))

(defun lispm-mcp-form (&optional name)
  "Make the form that adds an MCP server, or changes MCP server NAME;
return its buffer."
  (let* ((config (and name (lispm-settings-mcp-get name)))
         (transport (if (plist-get config :url) 'http 'stdio))
         (stdio-p (lambda (values) (eq (alist-get 'transport values) 'stdio)))
         (http-p (lambda (values) (eq (alist-get 'transport values) 'http)))
         (fields
          (append
           (unless name
             (lispm-mcp-form--new-fields))
           `((transport :type radio :label ,(lispm-tr "Transport")
                        :choices ((stdio . ,(lispm-tr "Command (stdio)"))
                                  (http . ,(lispm-tr "HTTP")))
                        :value ,transport
                        :help ,(lispm-tr "How LispM connects to the MCP server."))
             (command :type text :label ,(lispm-tr "Command") :required t
                      :value ,(or (plist-get config :command) "")
                      :when ,stdio-p
                      :help ,(lispm-tr "The program to run, as `npx' or `uvx'."))
             (args :type text :label ,(lispm-tr "Arguments")
                   :value ,(string-join (plist-get config :args) " ")
                   :when ,stdio-p
                   :help ,(lispm-tr "Arguments passed to the command, separated by spaces."))
             (env :type text :label ,(lispm-tr "Environment")
                  :value ,(lispm-mcp-form--env-text (plist-get config :env))
                  :when ,stdio-p
                  :validate ,(lambda (value)
                               (when (and (stringp value) (not (string-blank-p value)))
                                 (condition-case err
                                     (progn (lispm-mcp-form--parse-env value) nil)
                                   (error (error-message-string err)))))
                  :help ,(lispm-tr "Optional KEY=VALUE variables for the server's process."))
             (url :type text :label ,(lispm-tr "Address") :required t
                  :value ,(or (plist-get config :url) "")
                  :when ,http-p
                  :validate ,#'lispm-backend-form--validate-url
                  :help ,(lispm-tr "The HTTP address of the MCP server."))
             (token :type password :label ,(lispm-tr "The key")
                    :value ""
                    :when ,http-p
                    :help ,(lispm-tr "Never shown again; leave it empty to keep the one there is."))
             (description :type text :label ,(lispm-tr "Description")
                          :value ,(or (plist-get config :description) "")
                          :help ,(lispm-tr "What the server's tools do; agents read this when searching tools."))
             (defer :type check :text ,(lispm-tr "Load tools on demand with search_tools")
                    :value ,(if (plist-member config :defer) (plist-get config :defer) t))
             (permission :type choice :label ,(lispm-tr "Permission")
                         :choices ,(mapcar (pcase-lambda (`(,class . ,_))
                                             (cons class (format "%s — %s" class
                                                                 (lispm-permission-class-description class))))
                                           lispm-permission-classes)
                         :value ,(or (plist-get config :permission) 'mcp)
                         :help ,(lispm-tr "The permission class of the server's tools."))
             (sandbox :type choice :label ,(lispm-tr "Sandbox")
                      :when ,stdio-p
                      :choices ((default . ,(lispm-tr "Follow LispM's sandbox"))
                                (none . ,(lispm-sandbox-level-name 'none))
                                (low . ,(lispm-sandbox-level-name 'low))
                                (middle . ,(lispm-sandbox-level-name 'middle))
                                (high . ,(lispm-sandbox-level-name 'high)))
                      :value ,(or (plist-get config :sandbox) 'default)
                      :help ,(lispm-tr "The sandbox level the server's process runs at."))))))
    (lispm-ui-form
     (if name (lispm-tr "MCP Server %s" name) (lispm-tr "Add MCP Server"))
     fields
     :intro (lispm-tr "A Model Context Protocol server whose tools agents can use.")
     :submit (lispm-tr "Save")
     :cancel (lispm-tr "Cancel")
     :on-submit (lambda (values) (lispm-mcp-form--submit name values)))))

(defun lispm-skill-form ()
  "Make the form that creates a new skill in `lispm-write-skill-directory';
return its buffer."
  (lispm-ui-form
   (lispm-tr "New Skill")
   `((name :type text :label ,(lispm-tr "Name") :required t
           :value ""
           :validate ,(lambda (value)
                        (when (stringp value)
                          (let ((trimmed (string-trim value)))
                            (cond ((not (let ((case-fold-search nil))
                                          (string-match-p "\\`[a-z0-9][a-z0-9_-]\\{0,63\\}\\'" trimmed)))
                                   (lispm-tr "Use lowercase letters, digits, `-' or `_'"))
                                  ((lispm-skills-find trimmed nil)
                                   (lispm-tr "There is already a skill %s" trimmed))))))
           :help ,(lispm-tr "The skill's identifier, as `org-tables' or `deploy-check'."))
     (title :type text :label ,(lispm-tr "Title")
            :value ""
            :help ,(lispm-tr "A short title shown for the skill (optional)."))
     (description :type text :label ,(lispm-tr "Description") :required t
                  :value ""
                  :help ,(lispm-tr "One line saying what the skill is for: agents find it by this."))
     (instructions :type text-area :label ,(lispm-tr "Instructions") :lines 6
                   :value ""
                   :help ,(lispm-tr "How to do the task, which functions or tools to use, and pitfalls to avoid.")))
   :intro (lispm-tr "A manual in Org that teaches agents how to do a task in Emacs, saved in your skills directory and opened for editing.")
   :submit (lispm-tr "Create")
   :cancel (lispm-tr "Cancel")
   :on-submit (lambda (values)
                (lispm-create-skill
                 (string-trim (alist-get 'name values))
                 (string-trim (alist-get 'description values))
                 (alist-get 'instructions values)
                 (alist-get 'title values)))))

(provide 'lispm-backend-form)
;;; lispm-backend-form.el ends here
