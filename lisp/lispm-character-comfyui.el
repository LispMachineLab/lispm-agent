;;; lispm-character-comfyui.el --- ComfyUI image generation for characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Character ComfyUI capability extension: per-character appearance prefix,
;; workflow routing, portrait/image generation commands, `generate_image' tool,
;; and character form fields.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-i18n)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-characters)

(declare-function lispm-comfyui--canvas-buffers "lispm-comfyui" ())
(declare-function lispm-comfyui--need-types "lispm-comfyui" ())
(declare-function lispm-comfyui-open "lispm-comfyui" (name))
(declare-function lispm-comfyui-run "lispm-comfyui" (&optional callback))
(declare-function lispm-comfyui-graph-nodes "lispm-comfyui-graph" (graph))
(declare-function lispm-comfyui-node-type "lispm-comfyui-graph" (node))
(declare-function lispm-comfyui-node-title "lispm-comfyui-graph" (node))
(declare-function lispm-comfyui-set-widget "lispm-comfyui-graph" (node types name value))
(declare-function lispm-comfyui-types-of-server "lispm-comfyui-client" (callback &optional again))
(declare-function lispm-comfyui-queue "lispm-comfyui-client" (prompt workflow callback))
(declare-function lispm-comfyui-history "lispm-comfyui-client" (prompt-id callback))
(declare-function lispm-comfyui-fetch-image "lispm-comfyui-client" (image callback))

;;;; Character extension slots & predicate

(defun lispm-character-comfyui (character)
  "Non-nil when ComfyUI image generation is enabled for CHARACTER."
  (lispm-character-extension character :comfyui))

(gv-define-setter lispm-character-comfyui (value character)
  `(setf (lispm-character-extension ,character :comfyui) ,value))

(defun lispm-character-appearance (character)
  "Visual prompt prefix configured on CHARACTER for ComfyUI."
  (lispm-character-extension character :appearance))

(gv-define-setter lispm-character-appearance (value character)
  `(setf (lispm-character-extension ,character :appearance) ,value))

(defun lispm-character-workflow (character)
  "ComfyUI workflow name or file configured on CHARACTER."
  (lispm-character-extension character :workflow))

(gv-define-setter lispm-character-workflow (value character)
  `(setf (lispm-character-extension ,character :workflow) ,value))

(defun lispm-character-comfyui-enabled-p (character-or-agent)
  "Non-nil when CHARACTER-OR-AGENT has ComfyUI image generation enabled."
  (when-let* ((character (cond ((lispm-principal-p character-or-agent)
                                (lispm-agent-character* character-or-agent))
                               ((lispm-character-p character-or-agent)
                                character-or-agent)
                               (t (lispm-get-character character-or-agent t)))))
    (and (or (lispm-character-comfyui character)
             (lispm-character-appearance character)
             (lispm-character-workflow character))
         t)))

;;;; ComfyUI image generation for characters

(defvar lispm-comfyui-generate-function #'lispm-characters--comfyui-generate
  "Function running a ComfyUI image generation request.
Called as `(FUNC PROMPT &key NEGATIVE-PROMPT WORKFLOW CALLBACK)' where
CALLBACK receives `(FILES ERROR)'.")

(defun lispm-characters--comfyui-set-prompts (buffer positive &optional negative)
  "Set POSITIVE and optional NEGATIVE prompt texts in ComfyUI canvas BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'lispm-comfyui--root) lispm-comfyui--root)
      (let* ((types (lispm-comfyui--need-types))
             (nodes (lispm-comfyui-graph-nodes lispm-comfyui--root))
             (encoders (seq-filter (lambda (n)
                                     (member (lispm-comfyui-node-type n)
                                             '("CLIPTextEncode" "CLIPTextEncodeSDXL"
                                               "CLIPTextEncodeFlux" "PrimitiveStringMultiline")))
                                   nodes))
             (neg-node (seq-find (lambda (n)
                                   (string-match-p "neg" (downcase (or (lispm-comfyui-node-title n) ""))))
                                 encoders))
             (pos-node (or (seq-find (lambda (n) (not (eq n neg-node))) encoders)
                           (car encoders))))
        (when pos-node
          (ignore-errors (lispm-comfyui-set-widget pos-node types "text" positive)))
        (when (and negative (or neg-node (cadr encoders)))
          (ignore-errors (lispm-comfyui-set-widget (or neg-node (cadr encoders))
                                                   types "text" negative)))))))

(defun lispm-characters--comfyui-default-prompt (checkpoint positive negative &optional seed)
  "Build a 7-node ComfyUI API prompt alist using CHECKPOINT, POSITIVE and NEGATIVE."
  (let ((seed (or seed (random 1000000000))))
    `((1 . ((class_type . "CheckpointLoaderSimple")
            (inputs . ((ckpt_name . ,checkpoint)))))
      (2 . ((class_type . "CLIPTextEncode")
            (inputs . ((text . ,positive)
                       (clip . ["1" 1])))))
      (3 . ((class_type . "CLIPTextEncode")
            (inputs . ((text . ,(or negative "low quality, blurry, deformed"))
                       (clip . ["1" 1])))))
      (4 . ((class_type . "EmptyLatentImage")
            (inputs . ((width . 512) (height . 512) (batch_size . 1)))))
      (5 . ((class_type . "KSampler")
            (inputs . ((seed . ,seed)
                       (steps . 20)
                       (cfg . 7.0)
                       (sampler_name . "euler")
                       (scheduler . "normal")
                       (denoise . 1.0)
                       (model . ["1" 0])
                       (positive . ["2" 0])
                       (negative . ["3" 0])
                       (latent_image . ["4" 0])))))
      (6 . ((class_type . "VAEDecode")
            (inputs . ((samples . ["5" 0])
                       (vae . ["1" 2])))))
      (7 . ((class_type . "SaveImage")
            (inputs . ((filename_prefix . "lispm_character")
                       (images . ["6" 0]))))))))

(defun lispm-characters--comfyui-First-checkpoint (types)
  "First checkpoint filename offered by the ComfyUI server's TYPES hash table."
  (when (hash-table-p types)
    (when-let* ((def (gethash "CheckpointLoaderSimple" types))
                (input (alist-get 'input def))
                (req (alist-get 'required input))
                (ckpt (alist-get 'ckpt_name req))
                (choices (car ckpt)))
      (cond ((vectorp choices) (and (> (length choices) 0) (aref choices 0)))
            ((listp choices) (car choices))))))

(defun lispm-characters--comfyui-poll-history (prompt-id attempts callback)
  "Poll ComfyUI `/history/PROMPT-ID' up to ATTEMPTS times, then call
CALLBACK with `(FILES ERROR)'."
  (if (<= attempts 0)
      (funcall callback nil "ComfyUI timed out waiting for image generation")
    (lispm-comfyui-history
     prompt-id
     (lambda (entry error)
       (cond
        (error (funcall callback nil error))
        ((null entry)
         (run-at-time 0.5 nil #'lispm-characters--comfyui-poll-history
                      prompt-id (1- attempts) callback))
        (t
         (let (images)
           (pcase-dolist (`(,_node . ,out) (alist-get 'outputs entry))
             (let ((imgs (alist-get 'images out)))
               (dolist (img (if (vectorp imgs) (append imgs nil) imgs))
                 (push img images))))
           (setq images (nreverse images))
           (if (null images)
               (funcall callback nil "ComfyUI finished without producing an image")
             (let ((pending (length images))
                   files failed)
               (dolist (img images)
                 (lispm-comfyui-fetch-image
                  img
                  (lambda (file err)
                    (if err (setq failed err) (push file files))
                    (when (zerop (cl-decf pending))
                      (if failed
                          (funcall callback nil failed)
                        (funcall callback (nreverse files) nil)))))))))))))))

(cl-defun lispm-characters--comfyui-generate (prompt &key negative-prompt workflow callback)
  "Generate an image with ComfyUI for PROMPT; call CALLBACK with `(FILES ERROR)'."
  (unless (require 'lispm-comfyui nil t)
    (funcall callback nil "LispM-ComfyUI (`lispm-comfyui') is not installed")
    (cl-return-from lispm-characters--comfyui-generate nil))
  (condition-case err
      (let ((buffer (if (and workflow (not (string-empty-p workflow)))
                        (let ((match-fn
                               (lambda (buf)
                                 (when-let* ((file (buffer-file-name buf)))
                                   (or (equal (expand-file-name file)
                                              (expand-file-name workflow))
                                       (and (not (file-name-directory workflow))
                                            (equal (file-name-nondirectory file) workflow)))))))
                          (or (seq-find match-fn (lispm-comfyui--canvas-buffers))
                              (let ((opened (save-window-excursion
                                              (lispm-comfyui-open workflow))))
                                (or (and (buffer-live-p opened) opened)
                                    (seq-find match-fn (lispm-comfyui--canvas-buffers))
                                    (car (lispm-comfyui--canvas-buffers))))))
                      (car (lispm-comfyui--canvas-buffers)))))
        (if buffer
            (progn
              (lispm-characters--comfyui-set-prompts buffer prompt negative-prompt)
              (with-current-buffer buffer
                (lispm-comfyui-run
                 (lambda (run)
                   (if (eq (plist-get run :status) 'done)
                       (funcall callback (plist-get run :files) nil)
                     (funcall callback nil (or (plist-get run :message) "ComfyUI workflow run failed")))))))
          (lispm-comfyui-types-of-server
           (lambda (types error)
             (if error
                 (funcall callback nil error)
               (let ((ckpt (lispm-characters--comfyui-First-checkpoint types)))
                 (if (not ckpt)
                     (funcall callback nil "No checkpoint model found on the ComfyUI server")
                   (lispm-comfyui-queue
                    (lispm-characters--comfyui-default-prompt ckpt prompt negative-prompt)
                    nil
                    (lambda (answer q-err)
                      (if q-err
                          (funcall callback nil q-err)
                        (lispm-characters--comfyui-poll-history
                         (alist-get 'prompt_id answer) 120 callback)))))))))))
    (error (funcall callback nil (error-message-string err)))))

(cl-defun lispm-character-generate-image (character prompt &key negative-prompt set-avatar callback)
  "Generate an image for CHARACTER via ComfyUI with PROMPT.
Combines CHARACTER's `#+appearance:' with PROMPT when present.  When
SET-AVATAR is non-nil, saves the first generated image as CHARACTER's
`:avatar'.  Calls CALLBACK with `(FILES ERROR)'."
  (let* ((character (lispm-get-character character))
         (name (lispm-character-name character))
         (appearance (lispm-character-appearance character))
         (full-prompt (if (and appearance (not (string-empty-p appearance)))
                          (if (and prompt (not (string-empty-p prompt)))
                              (concat appearance ", " prompt)
                            appearance)
                        (or prompt (lispm-character-description character)
                            (lispm-character-title* character))))
         (workflow (lispm-character-workflow character)))
    (funcall lispm-comfyui-generate-function
             full-prompt
             :negative-prompt negative-prompt
             :workflow workflow
             :callback
             (lambda (files error)
               (when (and (not error) set-avatar (car files) (file-readable-p (car files)))
                 (lispm-save-character name :avatar (car files)))
               (when callback
                 (funcall callback files error))))))

;;;###autoload
(defun lispm-character-generate-avatar (character &optional prompt callback)
  "Generate a new avatar picture for CHARACTER with ComfyUI, and save it."
  (interactive (list (lispm-read-character (lispm-tr "Change the character: "))))
  (let* ((character (lispm-get-character character))
         (title (lispm-character-title* character)))
    (lispm-character-generate-image
     character
     (or prompt "portrait avatar, centered face, clean background")
     :set-avatar t
     :callback (lambda (files error)
                 (if error
                     (message "ComfyUI: %s" error)
                   (message "Updated avatar of %s." title))
                 (when callback (funcall callback files error))))))

(lispm-deftool generate_image
    ((prompt string "What to draw: scene, expression, or subject. Your `#+appearance:' is automatically prepended.")
     (negative_prompt string "What to avoid in the image." :optional t)
     (set_avatar boolean "If true, also set the generated image as your avatar." :optional t))
  "Generate an image with ComfyUI using your ComfyUI workflow (or the server's
checkpoint) and return the generated image so the user sees it in the chat."
  :permission web
  :async t
  :for #'lispm-character-comfyui-enabled-p
  (let ((character (or (lispm-agent-character* lispm-current-agent)
                       (error "You are no character"))))
    (lispm-character-generate-image
     character prompt
     :negative-prompt negative_prompt
     :set-avatar set_avatar
     :callback (lambda (files error)
                 (if error
                     (funcall done (format "ComfyUI image generation failed: %s" error) t)
                   (funcall done
                            (format "Generated %d image%s%s: %s"
                                    (length files)
                                    (if (= (length files) 1) "" "s")
                                    (if set_avatar " (and updated your avatar)" "")
                                    (mapconcat #'file-name-nondirectory files ", "))
                            nil files))))))

;;;; Extension registration

(lispm-character-register-extension
 'comfyui
 :keys '(:comfyui :appearance :workflow)
 :read (lambda (line bool _symbol)
         (list :comfyui (funcall bool "comfyui")
               :appearance (funcall line "appearance")
               :workflow (funcall line "workflow")))
 :format (lambda (props line-fn)
           `(("comfyui" . ,(and (plist-get props :comfyui) "yes"))
             ("appearance" . ,(funcall line-fn (plist-get props :appearance)))
             ("workflow" . ,(funcall line-fn (plist-get props :workflow)))))
 :form-values (lambda (character)
                `((comfyui . ,(and character (lispm-character-comfyui-enabled-p character)))
                  (appearance . ,(and character (lispm-character-appearance character)))
                  (workflow . ,(and character (lispm-character-workflow character)))))
 :form-props (lambda (get)
               (let ((comfyui (and (funcall get 'comfyui) t)))
                 (list :comfyui comfyui
                       :appearance (and comfyui (funcall get 'appearance))
                       :workflow (and comfyui (funcall get 'workflow)))))
 :form-fields (lambda ()
                `((comfyui :type check :label ,(lispm-tr "ComfyUI")
                           :text ,(lispm-tr "Generate images and portraits with ComfyUI"))
                  (appearance :type text :label ,(lispm-tr "Appearance")
                              :help ,(lispm-tr "What it looks like, for ComfyUI.")
                              :when ,(lispm-characters-when-checked 'comfyui))
                  (workflow :type text :label ,(lispm-tr "ComfyUI workflow")
                            :help ,(lispm-tr "Workflow name or file; empty for the open workflow.")
                            :when ,(lispm-characters-when-checked 'comfyui)))))

(provide 'lispm-character-comfyui)
;;; lispm-character-comfyui.el ends here
