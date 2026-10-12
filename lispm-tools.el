;;; lispm-tools.el --- Tools: Emacs actions an agent can take  -*- lexical-binding: t; -*-

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

;; A tool is an Emacs Lisp function the model may call, declared with
;; `lispm-deftool'.  The declaration gives each tool a permission class
;; (read, memory, comm, write or exec), from which an agent's policy
;; decides whether a call runs at once, waits for the user, or is
;; refused; and optionally a preview function that shows the user what
;; a call would change (a diff) before they approve it.
;;
;; The built-in tools work on live Emacs state rather than on disk:
;; reading a file that is open returns the buffer, unsaved edits
;; included; edits go through the visiting buffer, as one undo step,
;; so the user sees them happen, can undo them with C-/, and every
;; buffer touched is snapshotted in the agent's journal for
;; `lispm-revert-turn'.

;;; Code:

(require 'help-fns)
(require 'seq)
(require 'lispm-core)

(defcustom lispm-rationale-permissions '(write exec)
  "Permission classes whose tools take a required `why' argument.
The model states in one sentence why it makes the call; the user
reads it when approving, and it stays in the transcript."
  :type '(repeat symbol)
  :group 'lispm)

(defcustom lispm-tool-output-limit 40000
  "Maximum characters of tool output returned to the model."
  :type 'integer
  :group 'lispm)

(defcustom lispm-save-edits t
  "Whether file edits by agents are saved to disk at once."
  :type 'boolean
  :group 'lispm)

(defcustom lispm-shell-timeout 120
  "Default timeout in seconds for shell commands run by agents."
  :type 'integer
  :group 'lispm)

(cl-defstruct (lispm-tool (:constructor lispm-tool--create) (:copier nil))
  "A tool callable by agents."
  name description params permission function async preview)

(defvar lispm-tools--registry (make-hash-table :test #'equal)
  "Tool name -> `lispm-tool'.")

(defun lispm-register-tool (tool)
  "Make TOOL available to agents, replacing any tool of the same name."
  (puthash (lispm-tool-name tool) tool lispm-tools--registry)
  tool)

(defun lispm-get-tool (name)
  "The tool called NAME (a string or symbol), or nil."
  (gethash (format "%s" name) lispm-tools--registry))

(defmacro lispm-deftool (name params docstring &rest body)
  "Define a tool NAME that agents can call.
NAME's symbol name is what the model sees.  PARAMS is a list of
\(ARG TYPE DESCRIPTION . OPTIONS), TYPE being string, integer,
number, boolean, array or object, and OPTIONS among :optional t,
:enum LIST and :items SCHEMA.  The model sees ARG with dashes
turned into underscores.  DOCSTRING is the description the model
reads to decide when to call the tool.

BODY may start with keyword options:
  :permission CLASS  read (the default), memory, comm, write or exec
  :async t           BODY gets a function `done' to call later with
                     the result (and non-nil as second argument on
                     failure); its own value is ignored
  :preview FUNCTION  called with the same arguments; returns a string
                     (usually a diff) shown while the call awaits approval

Otherwise BODY's value is the result: a string, or any object, which
is printed.  Signal an error to report a failure to the model."
  (declare (indent 3) (doc-string 3))
  (let ((permission 'read) async preview
        (args (mapcar #'car params)))
    (while (keywordp (car body))
      (pcase (pop body)
        (:permission (setq permission (pop body)))
        (:async (setq async (pop body)))
        (:preview (setq preview (pop body)))))
    `(lispm-register-tool
      (lispm-tool--create
       :name ,(symbol-name name)
       :description ,docstring
       :params ',params
       :permission ',permission
       :async ,async
       :preview ,preview
       :function ,(if async
                      `(lambda (done ,@args) (ignore done ,@args) ,@body)
                    `(lambda ,args (ignore ,@args) ,@body))))))

(defun lispm-tools--key (arg)
  "JSON property name for the parameter ARG."
  (intern (replace-regexp-in-string "-" "_" (symbol-name arg))))

(defun lispm-tool-schema (tool)
  "The API definition of TOOL."
  (let (props required)
    (when (memq (lispm-tool-permission tool) lispm-rationale-permissions)
      (push '(why (type . "string")
                  (description . "One short sentence for the human reviewing this call: why it is needed."))
            props)
      (push "why" required))
    (pcase-dolist (`(,arg ,type ,doc . ,opts) (lispm-tool-params tool))
      (push `(,(lispm-tools--key arg)
              (type . ,(symbol-name type))
              (description . ,doc)
              ,@(when-let* ((enum (plist-get opts :enum))) `((enum . ,(vconcat enum))))
              ,@(when-let* ((items (plist-get opts :items))) `((items . ,items))))
            props)
      (unless (plist-get opts :optional)
        (push (symbol-name (lispm-tools--key arg)) required)))
    `((name . ,(lispm-tool-name tool))
      (description . ,(lispm-tool-description tool))
      (input_schema (type . "object")
                    (properties . ,(nreverse props))
                    (required . ,(vconcat (nreverse required)))))))

(defun lispm-agent-tools* (agent)
  "Tools AGENT may call, sorted by name (a stable request prefix)."
  (let ((spec (lispm-agent-tools agent)) tools)
    (maphash (lambda (name tool)
               (when (or (eq spec t)
                         (member name (mapcar (lambda (x) (format "%s" x)) spec)))
                 (push tool tools)))
             lispm-tools--registry)
    (sort tools (lambda (a b) (string< (lispm-tool-name a) (lispm-tool-name b))))))

(defun lispm-tools--argument (key type value opts)
  "Check VALUE of parameter KEY against TYPE and OPTS; return it for Lisp."
  (let ((ok (pcase type
              ('string (stringp value))
              ('integer (integerp value))
              ('number (numberp value))
              ('boolean (memq value '(t :false)))
              ('array (vectorp value))
              ('object (listp value))
              (_ t))))
    (unless ok
      (error "Parameter `%s' must be of type %s" key type))
    (when-let* ((enum (plist-get opts :enum)))
      (unless (member value enum)
        (error "Parameter `%s' must be one of %s" key (string-join enum ", "))))
    (if (eq value :false) nil value)))

(defun lispm-tools--arguments (tool input)
  "Validate INPUT against TOOL's parameters; return the argument list."
  (unless (listp input)
    (error "Input must be a JSON object"))
  (mapcar (lambda (param)
            (pcase-let* ((`(,arg ,type ,_doc . ,opts) param)
                         (key (lispm-tools--key arg))
                         (value (alist-get key input)))
              (if (memq value '(nil :null))
                  (if (plist-get opts :optional)
                      nil
                    (error "Missing required parameter `%s'" key))
                (lispm-tools--argument key type value opts))))
          (lispm-tool-params tool)))

(defun lispm-tools--result-text (result)
  "RESULT of a tool as text for the model."
  (let ((text (cond ((stringp result) result)
                    ((null result) "OK")
                    (t (let ((print-length 200) (print-level 8))
                         (prin1-to-string result))))))
    (lispm--truncate (if (string-empty-p text) "(no output)" text)
                     lispm-tool-output-limit)))

(defun lispm-tool-invoke (tool input done)
  "Run TOOL on INPUT, a decoded JSON object.
Call DONE exactly once with the result text and non-nil if the call
failed.  Synchronous tools call DONE before returning."
  (if (lispm-tool-async tool)
      (let* ((called nil)
             (once (lambda (result &optional errorp)
                     (unless called
                       (setq called t)
                       (funcall done (lispm-tools--result-text result) errorp)))))
        (condition-case err
            (apply (lispm-tool-function tool) once (lispm-tools--arguments tool input))
          (error (funcall once (error-message-string err) t))))
    (pcase-let ((`(,result . ,errorp)
                 (condition-case err
                     (cons (apply (lispm-tool-function tool)
                                  (lispm-tools--arguments tool input))
                           nil)
                   (error (cons (error-message-string err) t)))))
      (funcall done (lispm-tools--result-text result) errorp))))

(defun lispm-tool-preview-text (tool input)
  "What calling TOOL with INPUT would do, as a string, or nil."
  (when-let* ((preview (lispm-tool-preview tool)))
    (condition-case err
        (apply preview (lispm-tools--arguments tool input))
      (error (format "(no preview: %s)" (error-message-string err))))))

;;;; Permissions

(defun lispm-tools--in-root-p (agent file)
  "Non-nil if FILE lies under AGENT's root directory."
  (file-in-directory-p (expand-file-name file (lispm-agent-root agent))
                       (lispm-agent-root agent)))

(defun lispm-tool-action (agent tool input)
  "Whether AGENT may run TOOL on INPUT: `auto', `ask' or `deny'.
Tools the user allowed for this Emacs session run at once.  Writes
outside the agent's root always ask, whatever the policy says."
  (let ((action (lispm-policy-action agent (lispm-tool-permission tool)))
        (path (and (listp input) (alist-get 'path input))))
    (cond ((member (lispm-tool-name tool) (lispm-agent-allowed agent)) 'auto)
          ((and (eq action 'auto)
                (eq (lispm-tool-permission tool) 'write)
                (stringp path)
                (not (lispm-tools--in-root-p agent path)))
           'ask)
          (t action))))

;;;; Journal

(defun lispm-journal-snapshot (buffer)
  "Record BUFFER's contents before the acting agent first changes it this turn."
  (when-let* ((agent lispm-current-agent))
    (let* ((turn (lispm-agent-turn agent))
           (cell (or (assq turn (lispm-agent-journal agent))
                     (car (push (list turn) (lispm-agent-journal agent))))))
      (unless (assq buffer (cdr cell))
        (with-current-buffer buffer
          (setcdr cell (cons (list buffer
                                   :name (buffer-name)
                                   :file buffer-file-name
                                   :existed (and buffer-file-name
                                                 (file-exists-p buffer-file-name))
                                   :text (save-restriction
                                           (widen)
                                           (buffer-substring-no-properties
                                            (point-min) (point-max))))
                             (cdr cell)))))
      (setf (lispm-agent-journal agent) (seq-take (lispm-agent-journal agent) 20)))))

;;;; Helpers

(defun lispm-tools--root ()
  "Root directory of the acting agent."
  (if lispm-current-agent (lispm-agent-root lispm-current-agent) default-directory))

(defun lispm-tools--file (path)
  "PATH expanded against the acting agent's root."
  (expand-file-name (substitute-in-file-name path) (lispm-tools--root)))

(defun lispm-tools--label (file)
  "FILE as shown in diffs: relative to the root when inside it."
  (if (file-in-directory-p file (lispm-tools--root))
      (file-relative-name file (lispm-tools--root))
    (abbreviate-file-name file)))

(defun lispm-tools--number-lines (text offset limit)
  "Lines of TEXT from OFFSET (1-based), at most LIMIT, with line numbers."
  (let* ((lines (split-string text "\n"))
         (total (length lines))
         (offset (max 1 (or offset 1)))
         (limit (or limit 2000))
         (shown (seq-take (nthcdr (1- offset) lines) limit))
         (n (1- offset)))
    (concat (mapconcat (lambda (line) (format "%6d\t%s" (cl-incf n) line)) shown "\n")
            (if (< (+ offset (length shown) -1) total)
                (format "\n[... %d more lines; continue with offset %d]"
                        (- total offset (length shown) -1) (+ offset (length shown)))
              ""))))

(defun lispm-tools--replace (text old new all)
  "TEXT with OLD replaced by NEW: the single occurrence, or ALL of them."
  (when (string-empty-p old)
    (error "old_text is empty"))
  (let ((count (with-temp-buffer
                 (insert text)
                 (goto-char (point-min))
                 (let ((case-fold-search nil) (n 0))
                   (while (search-forward old nil t) (cl-incf n))
                   n))))
    (cond ((zerop count)
           (error "old_text was not found; read the file again, it may have changed"))
          ((and (> count 1) (not all))
           (error "old_text occurs %d times; include more context or set replace_all"
                  count)))
    (cons (if all
              (string-replace old new text)
            (let ((at (string-search old text)))
              (concat (substring text 0 at) new (substring text (+ at (length old))))))
          (if all count 1))))

(defun lispm-tools--set-text (buffer text)
  "Make BUFFER's text TEXT as one undo step, journaled and saved."
  (with-current-buffer buffer
    (lispm-journal-snapshot buffer)
    (let ((source (generate-new-buffer " *lispm-new-text*" t)))
      (unwind-protect
          (progn
            (with-current-buffer source (insert text))
            (save-restriction
              (widen)
              ;; One undo step of its own, apart from the user's edits.
              (undo-boundary)
              (with-undo-amalgamate
                (with-no-warnings (replace-buffer-contents source 1.0)))
              (undo-boundary)))
        (kill-buffer source)))
    (when (and buffer-file-name lispm-save-edits)
      (make-directory (file-name-directory buffer-file-name) t)
      (let ((inhibit-message t))
        (save-buffer)))))

(defun lispm-tools--buffer (name)
  "The live buffer called NAME."
  (or (get-buffer name) (error "No buffer named %s" name)))

(defun lispm-tools--buffer-text (buffer)
  "All of BUFFER's text."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Built-in tools: reading

(lispm-deftool read_file
    ((path string "File path, absolute or relative to your root directory.")
     (offset integer "First line to return, 1-based." :optional t)
     (limit integer "Maximum number of lines (default 2000)." :optional t))
  "Read a text file; lines come prefixed with their numbers.
If the file is open in Emacs, you get the buffer's current contents,
unsaved edits included: what the user sees."
  :permission read
  (let* ((file (lispm-tools--file path))
         (buffer (find-buffer-visiting file)))
    (cond (buffer
           (concat (if (buffer-modified-p buffer) "[open in Emacs, unsaved changes]\n" "")
                   (lispm-tools--number-lines (lispm-tools--buffer-text buffer)
                                              offset limit)))
          ((file-directory-p file) (error "%s is a directory; use list_directory" path))
          ((file-readable-p file)
           (lispm-tools--number-lines (lispm--file-text file) offset limit))
          (t (error "No such file: %s" file)))))

(lispm-deftool list_directory
    ((path string "Directory, absolute or relative to your root (default: the root)."
           :optional t))
  "List a directory's entries; subdirectories end with a slash."
  :permission read
  (let* ((dir (file-name-as-directory (lispm-tools--file (or path "."))))
         (entries (directory-files dir nil directory-files-no-dot-files-regexp)))
    (concat (abbreviate-file-name dir) "\n"
            (mapconcat (lambda (f)
                         (if (file-directory-p (expand-file-name f dir)) (concat f "/") f))
                       (seq-take entries 500)
                       "\n")
            (if (> (length entries) 500)
                (format "\n[... %d more]" (- (length entries) 500))
              ""))))

(lispm-deftool search_files
    ((pattern string "Regular expression (extended syntax) to search for.")
     (path string "File or directory to search (default: your root)." :optional t)
     (glob string "Only search files matching this glob, e.g. \"*.el\"." :optional t))
  "Search file contents recursively; returns matching lines as file:line:text."
  :permission read
  (let* ((target (lispm-tools--file (or path ".")))
         (default-directory (lispm-tools--root))
         (rg (executable-find "rg"))
         (args (if rg
                   `("rg" "--line-number" "--no-heading" "--color=never"
                     ,@(and glob (list "--glob" glob)) "-e" ,pattern ,target)
                 `("grep" "-rnE" "--binary-files=without-match" "--exclude-dir=.git"
                   ,@(and glob (list (concat "--include=" glob))) "-e" ,pattern ,target))))
    (with-temp-buffer
      (apply #'call-process (car args) nil t nil (cdr args))
      (let ((lines (split-string (buffer-string) "\n" t)))
        (if (null lines)
            "No matches."
          (concat (string-join (seq-take lines 200) "\n")
                  (if (> (length lines) 200)
                      (format "\n[... %d more matches]" (- (length lines) 200))
                    "")))))))

(lispm-deftool list_buffers ()
  "List the buffers open in Emacs: name, major mode, file and whether modified.
Buffers are the user's workspace; this shows what they are working on."
  :permission read
  (mapconcat
   (lambda (b)
     (with-current-buffer b
       (format "%s\t%s\t%s%s" (buffer-name b) major-mode
               (if buffer-file-name (abbreviate-file-name buffer-file-name) "-")
               (if (and buffer-file-name (buffer-modified-p)) "\t[modified]" ""))))
   (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b))) (buffer-list))
   "\n"))

(lispm-deftool read_buffer
    ((buffer string "Buffer name, as listed by list_buffers.")
     (offset integer "First line to return, 1-based." :optional t)
     (limit integer "Maximum number of lines (default 2000)." :optional t))
  "Read an Emacs buffer, e.g. *compilation*, *Messages* or a REPL.
Lines come prefixed with their numbers."
  :permission read
  (let ((b (lispm-tools--buffer buffer)))
    (concat (with-current-buffer b
              (format "[%s, point at line %d]\n" major-mode (line-number-at-pos)))
            (lispm-tools--number-lines (lispm-tools--buffer-text b) offset limit))))

(lispm-deftool describe_symbol
    ((name string "Name of an Emacs Lisp function, variable or face."))
  "Describe an Emacs Lisp symbol: function signature and documentation,
or a variable's documentation and current value.  Use it to learn
how this Emacs works before writing code for it."
  :permission read
  (let ((sym (intern-soft name)) parts)
    (unless sym (error "No symbol named %s" name))
    (when (fboundp sym)
      (push (format "Function %s %s\n%s" name
                    (or (help-function-arglist sym t) "()")
                    (or (ignore-errors (documentation sym t)) "Undocumented."))
            parts))
    (when (boundp sym)
      (push (format "Variable %s\nValue: %s\n%s" name
                    (lispm--truncate (let ((print-length 50) (print-level 5))
                                       (prin1-to-string (symbol-value sym)))
                                     2000)
                    (or (documentation-property sym 'variable-documentation t)
                        "Undocumented."))
            parts))
    (when (facep sym)
      (push (format "Face %s\n%s" name (or (face-documentation sym) "")) parts))
    (if parts
        (string-join (nreverse parts) "\n\n")
      (error "%s is neither a function, a variable nor a face" name))))

;;;; Built-in tools: writing

(defun lispm-tools--edit-preview (path old-text new-text &optional replace-all)
  "Diff that edit_file with these arguments would produce."
  (let* ((file (lispm-tools--file path))
         (old (lispm--file-text file)))
    (lispm--diff old (car (lispm-tools--replace old old-text new-text replace-all))
                 (lispm-tools--label file))))

(lispm-deftool edit_file
    ((path string "File to edit, absolute or relative to your root.")
     (old-text string "Exact text to replace, with enough context to be unique.")
     (new-text string "Replacement text.")
     (replace-all boolean "Replace every occurrence instead of exactly one." :optional t))
  "Replace text in a file.  The edit happens in the file's Emacs buffer
as one undoable step and is then saved.  old_text must match the
current contents exactly, so read the file first."
  :permission write
  :preview #'lispm-tools--edit-preview
  (let* ((file (lispm-tools--file path))
         (buffer (find-file-noselect file))
         (edit (lispm-tools--replace (lispm-tools--buffer-text buffer)
                                     old-text new-text replace-all)))
    (lispm-tools--set-text buffer (car edit))
    (format "Replaced %d occurrence%s in %s." (cdr edit) (if (= (cdr edit) 1) "" "s")
            (lispm-tools--label file))))

(lispm-deftool write_file
    ((path string "File to create or overwrite, absolute or relative to your root.")
     (content string "The complete new contents."))
  "Create a file, or replace a file's entire contents.  Prefer edit_file
for changes to existing files."
  :permission write
  :preview (lambda (path content)
             (let ((file (lispm-tools--file path)))
               (lispm--diff (lispm--file-text file) content (lispm-tools--label file))))
  (let* ((file (lispm-tools--file path))
         (existed (file-exists-p file)))
    (lispm-tools--set-text (find-file-noselect file) content)
    (format "%s %s." (if existed "Overwrote" "Created") (lispm-tools--label file))))

(lispm-deftool edit_buffer
    ((buffer string "Buffer name.")
     (old-text string "Exact text to replace, with enough context to be unique.")
     (new-text string "Replacement text.")
     (replace-all boolean "Replace every occurrence instead of exactly one." :optional t))
  "Replace text in an Emacs buffer that is not a file, such as *scratch*.
For files use edit_file."
  :permission write
  :preview (lambda (buffer old-text new-text &optional replace-all)
             (let ((old (lispm-tools--buffer-text (lispm-tools--buffer buffer))))
               (lispm--diff old (car (lispm-tools--replace old old-text new-text replace-all))
                            buffer)))
  (let* ((b (lispm-tools--buffer buffer))
         (edit (lispm-tools--replace (lispm-tools--buffer-text b)
                                     old-text new-text replace-all)))
    (lispm-tools--set-text b (car edit))
    (format "Replaced %d occurrence%s in %s." (cdr edit) (if (= (cdr edit) 1) "" "s") buffer)))

;;;; Built-in tools: execution

(lispm-deftool run_shell
    ((command string "Shell command, run in your root directory.")
     (timeout integer "Seconds before the command is killed (default 120)." :optional t))
  "Run a shell command and return its output and exit status.  The
output also streams into a buffer the user can watch."
  :permission exec
  :async t
  (let* ((agent lispm-current-agent)
         (buffer (get-buffer-create
                  (format "*lispm-shell: %s*" (if agent (lispm-agent-name agent) "?"))))
         (default-directory (lispm-tools--root))
         (output "")
         (proc (make-process
                :name "lispm-shell" :buffer buffer :noquery t
                :connection-type 'pipe
                :command (list shell-file-name shell-command-switch command)
                :filter
                (lambda (p chunk)
                  (setq output (concat output chunk))
                  (when (buffer-live-p (process-buffer p))
                    (with-current-buffer (process-buffer p)
                      (save-excursion
                        (goto-char (process-mark p))
                        (insert chunk)
                        (set-marker (process-mark p) (point))))))
                :sentinel
                (lambda (p _event)
                  (unless (process-live-p p)
                    (when-let* ((timer (process-get p 'timer))) (cancel-timer timer))
                    (let ((status (process-exit-status p)))
                      (funcall done
                               (concat output
                                       (if (process-get p 'timed-out)
                                           "\n[killed: timeout]"
                                         (format "\n[exit status %d]" status)))
                               (process-get p 'timed-out))))))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "\n$ %s\n" command))
      (set-marker (process-mark proc) (point)))
    (process-put proc 'lispm-agent agent)
    (process-put proc 'timer
                 (run-at-time (or timeout lispm-shell-timeout) nil
                              (lambda ()
                                (when (process-live-p proc)
                                  (process-put proc 'timed-out t)
                                  (kill-process proc)))))))

(defun lispm-tools--read-forms (code)
  "All Lisp forms in the string CODE."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(lispm-deftool eval_elisp
    ((code string "One or more Emacs Lisp forms, evaluated in order with lexical binding."))
  "Evaluate Emacs Lisp in the running Emacs and return the last value,
plus anything printed.  This reaches all of Emacs: buffers, windows,
processes, org, magit, any installed package."
  :permission exec
  (let* ((output (generate-new-buffer " *lispm-eval*" t))
         (standard-output output)
         (default-directory (lispm-tools--root))
         value)
    (unwind-protect
        (progn
          (dolist (form (lispm-tools--read-forms code))
            (setq value (eval form t)))
          (let ((printed (with-current-buffer output (buffer-string)))
                (print-length 100) (print-level 10))
            (concat (if (string-empty-p printed) "" (concat printed "\n"))
                    "=> " (prin1-to-string value))))
      (kill-buffer output))))

(provide 'lispm-tools)
;;; lispm-tools.el ends here
