;;; lispm-lisp-macros.el --- Macro tools in run_lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Syntax-level tools for `run_lisp' programs.  Plain query and action
;; tools stay first-class functions so programs can pass them to `funcall'
;; and `mapcar'; macros appear only where evaluation order, lexical
;; bindings, S-expression quoting or replay-aware dynamic cleanup is the
;; contract:
;;
;; - `ignoring_call_denied' and `with_tool_fallback': structured recovery
;;   from `lispm-call-denied' and `lispm-call-failed' without catching
;;   `quit' or `lispm-lisp-refused'.
;; - `with_terminal' and `with_worktree': dynamic-extent brackets whose
;;   cleanup runs on completion or error, skips suspension passes, and
;;   invalidates unwound entries on `:lisp-log' retry.
;; - `run_elisp_forms' and `eval_elisp_forms': quote Lisp forms and bind
;;   serializable lexical values in source order without string escaping,
;;   still mediated by `run_elisp' and `eval_elisp' policy.
;; - `transform_file': read exact live text via `read_file', bind it
;;   lexically, and invoke `edit_file' (or `write_file' when empty) only
;;   when the transformed text differs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-lisp)

;;;; Helpers for macro expansions

(defun lispm-lisp-macros--call (fn &rest args)
  "Construct an unforgeable internal helper call to FN with ARGS."
  `(,lispm-lisp--internal ,fn ,@args))

(defun lispm-lisp-macros--require-tools (macro required)
  "Refuse MACRO unless every symbol in REQUIRED is in `lispm-lisp--tools'."
  (dolist (tool required)
    (unless (memq tool lispm-lisp--tools)
      (lispm-lisp--refuse "%s requires the tool %s" macro tool))))

(defun lispm-lisp-macros--sanitize (value tool seen)
  "Return VALUE stripped of text properties if printable, else signal for TOOL.
SEEN tracks composite containers to reject circular structures."
  (cond ((or (null value) (memq value '(t :false)) (numberp value) (symbolp value))
         value)
        ((stringp value)
         (substring-no-properties value))
        ((or (consp value) (vectorp value))
         (when (gethash value seen)
           (signal 'lispm-lisp-serialization-error
                   (list (format "Circular value for %s" tool))))
         (puthash value t seen)
         (if (vectorp value)
             (vconcat (mapcar (lambda (v) (lispm-lisp-macros--sanitize v tool seen)) value))
           (cons (lispm-lisp-macros--sanitize (car value) tool seen)
                 (lispm-lisp-macros--sanitize (cdr value) tool seen))))
        (t
         (signal 'lispm-lisp-serialization-error
                 (list (format "%S for %s: bind plain Lisp data" value tool))))))

(defun lispm-lisp-macros--serialize (tool bindings forms)
  "Serialize FORMS wrapped in lexical BINDINGS `(SYM . VAL)' for TOOL."
  (let* ((seen (make-hash-table :test #'eq))
         (let-bindings
          (mapcar (pcase-lambda (`(,sym . ,val))
                    (list sym (list 'quote (lispm-lisp-macros--sanitize val tool seen))))
                  bindings))
         (program (if let-bindings
                      (list `(let ,let-bindings ,@forms))
                    forms))
         (print-length nil)
         (print-level nil)
         (print-circle nil)
         (print-quoted t)
         (print-escape-newlines t)
         (print-escape-control-characters t))
    (mapconcat #'prin1-to-string program "\n")))

(defun lispm-lisp-macros--parse-binds (macro specs)
  "Validate `:bind' SPECS for MACRO and return checked pair forms."
  (unless (proper-list-p specs)
    (lispm-lisp--refuse "%s: :bind must be a list of variables or (VAR EXPR) pairs" macro))
  (mapcar (lambda (spec)
            (pcase spec
              ((pred symbolp)
               (lispm-lisp--binder spec)
               `(cons ',spec ,spec))
              (`(,sym ,expr)
               (lispm-lisp--binder sym)
               `(cons ',sym ,expr))
              (_ (lispm-lisp--refuse "%s: invalid :bind entry %S" macro spec))))
          specs))

(defun lispm-lisp-macros--expand-code-tool (macro tool allow-timeout args)
  "Expand code-quoting MACRO for TOOL from ARGS, evaluating options in order.
When ALLOW-TIMEOUT is non-nil, `:timeout' is accepted in the leading plist."
  (lispm-lisp-macros--require-tools macro (list tool))
  (let (opts body)
    (if (and (consp args) (consp (car args)) (keywordp (caar args)))
        (setq opts (car args) body (cdr args))
      (setq body args))
    (unless (and (proper-list-p opts) (cl-evenp (length opts)))
      (lispm-lisp--refuse "%s: options must be a keyword plist" macro))
    (unless (and body (proper-list-p body))
      (lispm-lisp--refuse "%s requires one or more Lisp forms" macro))
    (let ((tail opts)
          (gbinds (make-symbol "binds"))
          (gtimeout (make-symbol "timeout"))
          (gcode (make-symbol "code"))
          seen-opts ordered has-binds has-timeout)
      (while tail
        (let ((kw (pop tail))
              (val (pop tail)))
          (when (memq kw seen-opts)
            (lispm-lisp--refuse "%s: duplicate option %s" macro kw))
          (push kw seen-opts)
          (cond ((eq kw :bind)
                 (setq has-binds t)
                 (push `(,gbinds (list ,@(lispm-lisp-macros--parse-binds macro val)))
                       ordered))
                ((and allow-timeout (eq kw :timeout))
                 (setq has-timeout t)
                 (push `(,gtimeout ,val) ordered))
                (t (lispm-lisp--refuse "%s has no option %s" macro kw)))))
      `(let* (,@(nreverse ordered)
              (,gcode ,(lispm-lisp-macros--call
                        'lispm-lisp-macros--serialize
                        `',tool (if has-binds gbinds nil) `',body)))
         ,(lispm-lisp-macros--call
           'lispm-lisp--call-pairs
           `',tool
           `(list (cons 'code ,gcode)
                  ,@(when has-timeout `((cons 'timeout ,gtimeout)))))))))

(defun lispm-lisp-macros--worktree-name (role request base)
  "Return a replay-deterministic worktree name from ROLE, REQUEST and BASE."
  (let ((stem (replace-regexp-in-string
               "\\`-+\\|-+\\'" ""
               (replace-regexp-in-string "[^[:alnum:]._-]+" "-" (format "%s" role)))))
    (format "%s-%06x" (if (string-empty-p stem) "work" stem)
            (logand (sxhash-equal (list role request base)) #xffffff))))

(defun lispm-lisp-macros--unnumber-page (raw)
  "Parse RAW `read_file' output into `(LINES . NEXT-OFFSET)'."
  (let ((text raw) next)
    (when (string-match "\\`\\[\\(?:open in Emacs\\|the session of an agent\\)[^\n]*\\]\n" text)
      (setq text (substring text (match-end 0))))
    (when (string-match "\n\\[\\.\\.\\. [0-9]+ more lines?; continue with offset \\([0-9]+\\)\\]\\'" text)
      (setq next (string-to-number (match-string 1 text))
            text (substring text 0 (match-beginning 0))))
    (cons (mapcar (lambda (line)
                    (if (string-match "\\` *[0-9]+\t" line)
                        (substring line (match-end 0))
                      line))
                  (split-string text "\n"))
          next)))

(defun lispm-lisp-macros--live-file-text (run path)
  "Return exact live text of PATH for RUN's agent if open or readable."
  (let* ((agent (lispm-lisp--run-agent run))
         (lispm-current-agent agent)
         (default-directory (lispm-agent-root agent))
         (file-name-handler-alist (lispm-lisp--run-handlers run))
         (file (ignore-errors (lispm-tools--file path)))
         (open (and file (find-buffer-visiting file)))
         (state (and open (lispm-tools--state open))))
    (cond ((and open (memq state '(current conflict held)))
           (lispm-tools--buffer-text open))
          ((and file (file-regular-p file) (file-readable-p file))
           (lispm--file-text file)))))

(defun lispm-lisp-macros--read-file-text (path)
  "Read PATH via `read_file' and return exact replay-cached unnumbered text."
  (let* ((run (or lispm-lisp--run (error "No program is running")))
         (first-index (lispm-lisp--run-index run))
         (raw (lispm-lisp--call-pairs 'read_file
                                      (list (cons 'path path) (cons 'offset 1)))))
    (when (and (string-prefix-p "[" raw)
               (string-match-p "\\`\\[\\(?:image\\|video\\) " raw))
      (signal 'lispm-call-failed
              (list (propertize (format "transform_file: %s is a media file" path)
                                'lispm-call-index first-index))))
    (let* ((entry (nth first-index (lispm-lisp--run-log run)))
           (logged (and entry (nth 3 entry)))
           (cached (and (stringp logged) (not (string-empty-p logged))
                        (get-text-property 0 'lispm-raw-text logged))))
      (if cached
          (cdr cached)
        (let ((exact (or (lispm-lisp-macros--live-file-text run path)
                         (pcase-let ((`(,lines . ,next)
                                      (lispm-lisp-macros--unnumber-page raw)))
                           (let ((pages (list lines))
                                 (offset next))
                             (while offset
                               (let ((page (lispm-lisp--call-pairs
                                            'read_file
                                            (list (cons 'path path)
                                                  (cons 'offset offset)
                                                  (cons 'limit 2000)))))
                                 (pcase-let ((`(,more . ,n)
                                              (lispm-lisp-macros--unnumber-page page)))
                                   (push more pages)
                                   (setq offset n))))
                             (let ((joined (string-join (apply #'append (nreverse pages)) "\n")))
                               (if (string-empty-p joined) "" (concat joined "\n"))))))))
          (when (and (stringp logged) (not (string-empty-p logged)))
            (let ((copy (copy-sequence logged)))
              (put-text-property 0 1 'lispm-raw-text (cons t exact) copy)
              (setf (nth 3 entry) copy)))
          exact)))))

(defun lispm-lisp-macros--write-changed (path old new)
  "Write NEW to PATH when different from OLD; return non-nil when changed."
  (unless (stringp new)
    (signal 'lispm-lisp-type-error
            (list (format "transform_file body must return a string, got %S" new))))
  (let ((clean (substring-no-properties new)))
    (unless (equal clean old)
      (if (string-empty-p old)
          (lispm-lisp--call-pairs 'write_file (list (cons 'path path) (cons 'content clean)))
        (lispm-lisp--call-pairs 'edit_file
                                (list (cons 'path path)
                                      (cons 'old_text old)
                                      (cons 'new_text clean))))
      t)))

;;;; Macro tools in `run_lisp'

(lispm-lisp-defmacro ignoring_call_denied (args)
  "Evaluate ARGS; return nil when a call signals `lispm-call-denied'."
  `(condition-case nil
       (progn ,@args)
     (lispm-call-denied nil)))

(lispm-lisp-defmacro with_tool_fallback (args)
  "Evaluate `(car ARGS)'; on `lispm-call-failed', run `(cdr ARGS)'."
  (unless (and (consp args) (cdr args) (proper-list-p args))
    (lispm-lisp--refuse "with_tool_fallback requires a primary form and fallback forms"))
  `(condition-case nil
       ,(car args)
     (lispm-call-failed ,@(cdr args))))

(lispm-lisp-defmacro with_terminal (args)
  "Enter LispM's terminal for ARGS and leave it on completion or error."
  (lispm-lisp-macros--require-tools 'with_terminal '(terminal_enter terminal_leave))
  (let ((gidx (make-symbol "idx"))
        (gdone (make-symbol "done")))
    `(let ((,gidx ,(lispm-lisp-macros--call 'lispm-lisp--current-index))
           (,gdone nil))
       (terminal_enter)
       (unwind-protect
           (prog1
               (progn ,@args)
             (setq ,gdone t))
         (unless ,(lispm-lisp-macros--call 'lispm-lisp--stopping-p)
           (terminal_leave)
           (unless ,gdone
             ,(lispm-lisp-macros--call 'lispm-lisp--note-unwind gidx)))))))

(lispm-lisp-defmacro with_worktree (args)
  "Delegate in a scoped git worktree for ARGS and remove it on exit.
Syntax: `(with_worktree (VAR :role R :request Q ...) BODY...)'."
  (lispm-lisp-macros--require-tools 'with_worktree '(worktree_ask worktree_remove))
  (unless (and (consp args) (cdr args) (consp (car args)) (proper-list-p (car args))
               (cl-oddp (length (car args))))
    (lispm-lisp--refuse
     "with_worktree requires (VAR :role ROLE :request REQ ...) followed by body forms"))
  (let* ((spec (car args))
         (var (lispm-lisp--binder (car spec)))
         (tail (cdr spec))
         (grole (make-symbol "role"))
         (greq (make-symbol "req"))
         (gopt-name (make-symbol "opt-name"))
         (gbase (make-symbol "base"))
         (gkeep (make-symbol "keep"))
         (gname (make-symbol "name"))
         (gidx (make-symbol "idx"))
         (gdone (make-symbol "done"))
         (greply (make-symbol "reply"))
         seen ordered)
    (while tail
      (let ((kw (pop tail))
            (val (pop tail)))
        (when (memq kw seen)
          (lispm-lisp--refuse "with_worktree: duplicate option %s" kw))
        (push kw seen)
        (pcase kw
          (:role (push `(,grole ,val) ordered))
          (:request (push `(,greq ,val) ordered))
          (:name (push `(,gopt-name ,val) ordered))
          (:base (push `(,gbase ,val) ordered))
          (:keep (push `(,gkeep ,val) ordered))
          (_ (lispm-lisp--refuse "with_worktree has no option %s" kw)))))
    (unless (and (memq :role seen) (memq :request seen))
      (lispm-lisp--refuse "with_worktree requires :role and :request"))
    (dolist (pair `((:name . ,gopt-name) (:base . ,gbase) (:keep . ,gkeep)))
      (unless (memq (car pair) seen)
        (push `(,(cdr pair) nil) ordered)))
    `(let* (,@(nreverse ordered)
            (,gname (or ,gopt-name
                        ,(lispm-lisp-macros--call
                          'lispm-lisp-macros--worktree-name grole greq gbase)))
            (,gidx ,(lispm-lisp-macros--call 'lispm-lisp--current-index))
            (,gdone nil)
            (,greply ,(lispm-lisp-macros--call
                       'lispm-lisp--call-pairs
                       ''worktree_ask
                       `(list (cons 'role ,grole)
                              (cons 'request ,greq)
                              (cons 'name ,gname)
                              (cons 'base ,gbase)))))
       (unwind-protect
           (prog1
               (let ((,var (list :name ,gname
                                 :branch (concat "lispm/" ,gname)
                                 :reply ,greply)))
                 ,@(cdr args))
             (setq ,gdone t))
         (unless ,(lispm-lisp-macros--call 'lispm-lisp--stopping-p)
           (unless ,gkeep
             (condition-case nil
                 ,(lispm-lisp-macros--call
                   'lispm-lisp--call-pairs
                   ''worktree_remove
                   `(list (cons 'name ,gname) (cons 'force t)))
               (lispm-call-failed nil))
             (unless ,gdone
               ,(lispm-lisp-macros--call 'lispm-lisp--note-unwind gidx))))))))

(lispm-lisp-defmacro run_elisp_forms (args)
  "Quote S-expressions in ARGS with `:bind'/`:timeout' for `run_elisp'."
  (lispm-lisp-macros--expand-code-tool 'run_elisp_forms 'run_elisp t args))

(lispm-lisp-defmacro eval_elisp_forms (args)
  "Quote S-expressions in ARGS with optional `:bind' for `eval_elisp'."
  (lispm-lisp-macros--expand-code-tool 'eval_elisp_forms 'eval_elisp nil args))

(lispm-lisp-defmacro transform_file (args)
  "Read `(VAR PATH)' in ARGS, evaluate body, and write only if changed."
  (lispm-lisp-macros--require-tools 'transform_file '(read_file edit_file write_file))
  (unless (and (consp args) (proper-list-p (car args)) (= (length (car args)) 2) (cdr args))
    (lispm-lisp--refuse "transform_file requires (VAR PATH) followed by body forms"))
  (let* ((var (lispm-lisp--binder (caar args)))
         (path-form (cadar args))
         (body (cdr args))
         (gpath (make-symbol "path"))
         (gold (make-symbol "old"))
         (gnew (make-symbol "new")))
    `(let* ((,gpath ,path-form)
            (,gold ,(lispm-lisp-macros--call 'lispm-lisp-macros--read-file-text gpath))
            (,var ,gold)
            (,gnew (progn ,@body)))
       ,(lispm-lisp-macros--call 'lispm-lisp-macros--write-changed gpath gold gnew))))

(provide 'lispm-lisp-macros)
;;; lispm-lisp-macros.el ends here
