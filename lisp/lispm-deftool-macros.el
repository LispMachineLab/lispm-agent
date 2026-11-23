;;; lispm-deftool-macros.el --- Compile-time passes for lispm-deftool -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Compile-time schema validation, parameter key normalization and
;; declarative `:edit' expansion for `lispm-deftool'.  Keeping these
;; syntax-to-syntax passes separate from the live runtime keeps
;; `lispm-tools.el' focused on tool execution, permissions and buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun lispm-tools--key (arg)
  "JSON property name for the parameter ARG."
  (intern (replace-regexp-in-string "-" "_" (symbol-name arg))))

(defun lispm-tools--compile-params (name params)
  "Validate PARAMS of tool NAME and return precomputed `(PROPS . REQUIRED)'."
  (unless (proper-list-p params)
    (error "Invalid parameter list in `lispm-deftool' %s: %S" name params))
  (let (props required seen)
    (dolist (spec params)
      (unless (and (consp spec) (consp (cdr spec)) (consp (cddr spec))
                   (proper-list-p (cdddr spec)) (cl-evenp (length (cdddr spec))))
        (error "Invalid parameter spec in `lispm-deftool' %s: %S" name spec))
      (pcase-let ((`(,arg ,type ,doc . ,opts) spec))
        (unless (and arg (symbolp arg) (not (keywordp arg)) (not (eq arg t)))
          (error "Invalid parameter name %S in `lispm-deftool' %s" arg name))
        (unless (memq type '(string integer number boolean array object))
          (error "Invalid parameter type %S for %s in `lispm-deftool' %s" type arg name))
        (unless (stringp doc)
          (error "Parameter %s in `lispm-deftool' %s needs a docstring" arg name))
        (let ((tail opts) opt-seen)
          (while tail
            (let ((kw (pop tail))
                  (val (pop tail)))
              (unless (memq kw '(:optional :enum :items))
                (error "Unknown parameter option %S for %s in `lispm-deftool' %s"
                       kw arg name))
              (when (memq kw opt-seen)
                (error "Duplicate parameter option %S for %s in `lispm-deftool' %s"
                       kw arg name))
              (when (and (eq kw :enum) (not (and (consp val) (proper-list-p val))))
                (error "Option `:enum' for %s in `lispm-deftool' %s must be a non-empty list: %S"
                       arg name val))
              (push kw opt-seen))))
        (let ((key (lispm-tools--key arg)))
          (when (memq key seen)
            (error "Duplicate parameter %s in `lispm-deftool' %s" arg name))
          (push key seen)
          (push `(,key
                  (type . ,(symbol-name type))
                  (description . ,doc)
                  ,@(when-let* ((enum (plist-get opts :enum))) `((enum . ,(vconcat enum))))
                  ,@(when-let* ((items (plist-get opts :items))) `((items . ,items))))
                props)
          (unless (plist-get opts :optional)
            (push (symbol-name key) required)))))
    (cons (nreverse props) (nreverse required))))

(defun lispm-tools--ignore-vars (vars)
  "Return `((ignore ...))' for non-underscore symbols in VARS."
  (when-let* ((live (seq-remove (lambda (v)
                                  (or (null v) (string-prefix-p "_" (symbol-name v))))
                                vars)))
    `((ignore ,@live))))

(defun lispm-tools--parse-edit-spec (name spec)
  "Validate `:edit' plist SPEC of tool NAME and return parsed fields."
  (unless (and (proper-list-p spec) (cl-evenp (length spec)))
    (error "Invalid `:edit' spec in `lispm-deftool' %s: %S" name spec))
  (let ((tail spec) seen path-form buf-form create transform-spec binds)
    (while tail
      (let ((kw (pop tail))
            (val (pop tail)))
        (when (memq kw seen)
          (error "Duplicate `:edit' option %S in `lispm-deftool' %s" kw name))
        (push kw seen)
        (pcase kw
          (:file (setq path-form val))
          (:buffer (setq buf-form val))
          (:create (setq create val))
          (:transform (setq transform-spec val))
          (:bind (setq binds val))
          (_ (error "Unknown `:edit' option %S in `lispm-deftool' %s" kw name)))))
    (when (eq (null path-form) (null buf-form))
      (error "`:edit' in `lispm-deftool' %s needs `:file' or `:buffer'" name))
    (when (and buf-form create)
      (error "`:edit' `:buffer' in `lispm-deftool' %s cannot use `:create'" name))
    (unless (and (consp transform-spec) (symbolp (car transform-spec))
                 (consp (cdr transform-spec)) (null (cddr transform-spec)))
      (error "`:edit' `:transform' in `lispm-deftool' %s must be `(VAR FORM)'" name))
    (unless (and (proper-list-p binds) (<= (length binds) 3)
                 (seq-every-p #'symbolp binds))
      (error "`:edit' `:bind' in `lispm-deftool' %s must list at most 3 symbols" name))
    (list path-form buf-form create transform-spec binds)))

(defun lispm-tools--expand-file-edit (args path-form create text-var transform binds body)
  "Return `(DIFF CHANGES TARGETS BODY)' for file edit of PATH-FORM.
ARGS, CREATE, TEXT-VAR, TRANSFORM, BINDS and BODY come from `:edit'."
  (pcase-let ((`(,meta-var ,label-var ,existed-var) binds)
              (gfile (make-symbol "file"))
              (gdir (make-symbol "dir"))
              (gold (make-symbol "old"))
              (gbuf (make-symbol "buf"))
              (gedit (make-symbol "edit")))
    (let ((mvar (or meta-var (make-symbol "meta")))
          (lvar (or label-var (make-symbol "label")))
          (evar (or existed-var (make-symbol "existed")))
          (targets (unless (and (eq path-form 'path) (memq 'path args))
                     `(lambda ,args
                        ,@(lispm-tools--ignore-vars args)
                        (list (lispm-tools--file ,path-form))))))
      (list
       `(lambda ,args
          ,@(lispm-tools--ignore-vars args)
          (let* ((,gfile (lispm-tools--file ,path-form))
                 (,gold (lispm--file-text ,gfile))
                 (,text-var ,gold))
            ,@(lispm-tools--ignore-vars (list text-var))
            (list ,gold (car ,transform) (lispm-tools--label ,gfile))))
       `(lambda ,args
          ,@(lispm-tools--ignore-vars args)
          (let ((,gfile (lispm-tools--file ,path-form)))
            (lispm-tools--check-writable ,gfile)
            ,@(when create `((lispm-tools--check-new-file ,gfile)))
            (when-let* ((,gbuf (lispm-tools--to-show ,gfile ,create)))
              (let ((,text-var (lispm-tools--buffer-text ,gbuf)))
                ,@(lispm-tools--ignore-vars (list text-var))
                (cons ,gbuf (car ,transform))))))
       targets
       (if create
           `(let* ((,gfile (lispm-tools--file ,path-form))
                   (,gdir (file-name-directory ,gfile))
                   (,evar (file-exists-p ,gfile)))
              (lispm-tools--check-writable ,gfile)
              (lispm-tools--check-new-file ,gfile)
              (when (and lispm-save-edits (not (file-directory-p ,gdir)))
                (make-directory ,gdir t))
              (let* ((,gbuf (lispm-tools--visit ,gfile))
                     (,text-var (lispm-tools--buffer-text ,gbuf))
                     (,gedit (progn ,@(lispm-tools--ignore-vars (list text-var)) ,transform))
                     (,mvar (cdr ,gedit))
                     (,lvar (lispm-tools--label ,gfile)))
                ,@(lispm-tools--ignore-vars (list evar mvar lvar))
                (lispm-tools--set-text ,gbuf (car ,gedit))
                ,@body))
         `(let* ((,gfile (lispm-tools--file ,path-form))
                 (,evar (file-exists-p ,gfile))
                 (,gbuf (lispm-tools--existing ,gfile))
                 (,text-var (lispm-tools--buffer-text ,gbuf))
                 (,gedit (progn ,@(lispm-tools--ignore-vars (list text-var)) ,transform))
                 (,mvar (cdr ,gedit))
                 (,lvar (lispm-tools--label ,gfile)))
            ,@(lispm-tools--ignore-vars (list evar mvar lvar))
            (lispm-tools--set-text ,gbuf (car ,gedit))
            ,@body))))))

(defun lispm-tools--expand-buffer-edit (args buf-form text-var transform binds body)
  "Return `(DIFF CHANGES TARGETS BODY)' for buffer edit of BUF-FORM.
ARGS, TEXT-VAR, TRANSFORM, BINDS and BODY come from `:edit'."
  (pcase-let ((`(,meta-var ,label-var) binds)
              (gbname (make-symbol "buffer-name"))
              (gold (make-symbol "old"))
              (gb (make-symbol "buf"))
              (gfile (make-symbol "file"))
              (gedit (make-symbol "edit")))
    (let ((mvar (or meta-var (make-symbol "meta")))
          (lvar (or label-var (make-symbol "label")))
          (targets (unless (and (eq buf-form 'buffer) (memq 'buffer args))
                     `(lambda ,args
                        ,@(lispm-tools--ignore-vars args)
                        (when-let* ((,gb (get-buffer (lispm-tools-buffer-name ,buf-form)))
                                    (,gfile (buffer-file-name ,gb)))
                          (list ,gfile))))))
      (list
       `(lambda ,args
          ,@(lispm-tools--ignore-vars args)
          (let* ((,gbname ,buf-form)
                 (,gold (lispm-tools--buffer-file-text (lispm-tools--buffer ,gbname)))
                 (,text-var ,gold))
            ,@(lispm-tools--ignore-vars (list text-var))
            (list ,gold (car ,transform) ,gbname)))
       `(lambda ,args
          ,@(lispm-tools--ignore-vars args)
          (let ((,gb (lispm-tools--buffer ,buf-form)))
            (when-let* ((,gfile (buffer-file-name ,gb)))
              (lispm-tools--check-writable ,gfile))
            (when (eq (lispm-tools--state ,gb) 'current)
              (let ((,text-var (lispm-tools--buffer-text ,gb)))
                ,@(lispm-tools--ignore-vars (list text-var))
                (cons ,gb (car ,transform))))))
       targets
       `(let* ((,gbname ,buf-form)
               (,gb (lispm-tools--buffer-to-change ,gbname))
               (,text-var (lispm-tools--buffer-text ,gb))
               (,gedit (progn ,@(lispm-tools--ignore-vars (list text-var)) ,transform))
               (,mvar (cdr ,gedit))
               (,lvar ,gbname))
          ,@(lispm-tools--ignore-vars (list mvar lvar))
          (lispm-tools--set-text ,gb (car ,gedit))
          ,@body)))))

(defun lispm-tools--expand-edit (name args spec body)
  "Expand `:edit' SPEC in `lispm-deftool' NAME with ARGS and BODY."
  (pcase-let ((`(,path-form ,buf-form ,create (,text-var ,transform) ,binds)
               (lispm-tools--parse-edit-spec name spec)))
    (if path-form
        (lispm-tools--expand-file-edit args path-form create text-var transform binds body)
      (lispm-tools--expand-buffer-edit args buf-form text-var transform binds body))))

(provide 'lispm-deftool-macros)
;;; lispm-deftool-macros.el ends here
