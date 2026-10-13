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
(require 'project)
(require 'lispm-core)
(require 'lispm-transcript)

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
  "A tool callable by agents.
Tools defined with `lispm-deftool' have PARAMS, from which their schema
is made and their arguments checked.  Tools from elsewhere, such as
MCP servers, have an INPUT-SCHEMA instead (a JSON schema as an alist,
or a function returning one) and a FUNCTION taking the whole input.
A DEFERRED tool is not offered to an agent until search_tools has
found it for that agent.  CHANGES, for a tool that changes a buffer,
tells what a call would make of it (see `lispm-deftool')."
  name description params input-schema permission function async preview deferred
  changes)

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
  :changes FUNCTION  for a tool that changes a buffer: called with the
                     same arguments, returns (BUFFER . TEXT), the buffer
                     the call would change and the text it would leave
                     there; the change is then shown in BUFFER while the
                     call is written and while it awaits approval
  :deferred t        offered only once search_tools finds it (see
                     `lispm-tool-sources'): for tools seldom needed

Otherwise BODY's value is the result: a string, or any object, which
is printed.  Signal an error to report a failure to the model."
  (declare (indent 3) (doc-string 3))
  (let ((permission 'read) async preview changes deferred
        (args (mapcar #'car params)))
    (while (keywordp (car body))
      (pcase (pop body)
        (:permission (setq permission (pop body)))
        (:async (setq async (pop body)))
        (:preview (setq preview (pop body)))
        (:changes (setq changes (pop body)))
        (:deferred (setq deferred (pop body)))))
    `(lispm-register-tool
      (lispm-tool--create
       :name ,(symbol-name name)
       :description ,docstring
       :params ',params
       :permission ',permission
       :async ,async
       :preview ,preview
       :changes ,changes
       :deferred ,deferred
       :function ,(if async
                      `(lambda (done ,@args) (ignore done ,@args) ,@body)
                    `(lambda ,args (ignore ,@args) ,@body))))))

(defun lispm-tools--key (arg)
  "JSON property name for the parameter ARG."
  (intern (replace-regexp-in-string "-" "_" (symbol-name arg))))

(defun lispm-tool-schema (tool)
  "The API definition of TOOL."
  (if-let* ((schema (lispm-tool-input-schema tool)))
      `((name . ,(lispm-tool-name tool))
        (description . ,(let ((description (lispm-tool-description tool)))
                          (if (functionp description) (funcall description) description)))
        (input_schema . ,(if (functionp schema) (funcall schema) schema)))
    (lispm-tools--params-schema tool)))

(defun lispm-tools--params-schema (tool)
  "The API definition of TOOL, made from its parameters."
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
  "Tools AGENT may call, sorted by name (a stable request prefix).
In a role's list of tools, a name ending with * stands for every
tool whose name starts with the rest, such as mcp__peekaboo__*.
Deferred tools are left out until search_tools has loaded them for
AGENT; search_tools itself is there when there are any to find."
  (let* ((spec (and (listp (lispm-agent-tools agent))
                    (mapcar (lambda (x) (format "%s" x)) (lispm-agent-tools agent))))
         (all (eq (lispm-agent-tools agent) t))
         (searchable (lispm-tools--searchable-p))
         tools)
    (maphash (lambda (name tool)
               (when (and (or all
                              (seq-some (lambda (pattern)
                                          (if (string-suffix-p "*" pattern)
                                              (string-prefix-p (substring pattern 0 -1) name)
                                            (equal pattern name)))
                                        spec))
                          (or (not (lispm-tool-deferred tool))
                              (member name (lispm-agent-loaded-tools agent)))
                          (or searchable (not (equal name "search_tools"))))
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
  "Validate INPUT against TOOL's parameters; return the argument list.
A tool with a schema of its own gets INPUT whole."
  (unless (listp input)
    (error "Input must be a JSON object"))
  (if (lispm-tool-input-schema tool)
      (list input)
    (lispm-tools--param-arguments tool input)))

(defun lispm-tools--param-arguments (tool input)
  "INPUT checked against TOOL's parameters, as an argument list."
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
failed, and, if the result has images, a third argument: their files.
Synchronous tools call DONE before returning.  An asynchronous tool's
`done' takes the same arguments."
  (if (lispm-tool-async tool)
      (let* ((called nil)
             (once (lambda (result &optional errorp images)
                     (unless called
                       (setq called t)
                       (apply done (lispm-tools--result-text result) errorp
                              (and images (list images)))))))
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

(defun lispm-tool-would-change (tool input)
  "What calling TOOL with INPUT would change: (BUFFER . TEXT), or nil.
Nil for a tool that changes no buffer; an error if INPUT makes no
valid call of TOOL."
  (when-let* ((changes (lispm-tool-changes tool)))
    (apply changes (lispm-tools--arguments tool input))))

;;;; Deferred tools
;;
;; An agent carries a layer of tools it commonly needs; others, many or
;; seldom needed, are deferred: Peekaboo's 26 tools, for using the
;; computer, would take 14,000 tokens of every request.  Using one is
;; two steps.  The model searches for what it needs with search_tools,
;; which loads the tools it finds, and from its next step on, they are
;; offered to it like the others, for the rest of the session.  A
;; resumed session has them again: the transcript records which tools
;; it searched for and called.

(defcustom lispm-tool-search-results 5
  "How many tools search_tools loads for a search by keywords."
  :type 'integer
  :group 'lispm)

(defvar lispm-tool-sources nil
  "Functions listing sources of deferred tools, such as MCP servers.
Each returns a list of plists, one per source:
  :name         the source's name, which a search can use
  :description  what its tools are for
  :pending      non-nil while its tools are not registered yet
  :prepare      a function of a callback, registering its tools (a
                server may have to start), then calling the callback
                with nil, or with what went wrong
  :provides     a predicate on tool names: whether the source has it")

(defun lispm-tools--sources ()
  "The sources of deferred tools, from `lispm-tool-sources'."
  (apply #'append (mapcar #'funcall lispm-tool-sources)))

(defun lispm-tools--deferred ()
  "The deferred tools registered."
  (let (tools)
    (maphash (lambda (_name tool) (when (lispm-tool-deferred tool) (push tool tools)))
             lispm-tools--registry)
    (sort tools (lambda (a b) (string< (lispm-tool-name a) (lispm-tool-name b))))))

(defun lispm-tools--searchable-p ()
  "Non-nil if there are deferred tools, or sources of them."
  (or (lispm-tools--sources)
      (catch 'found
        (maphash (lambda (_name tool) (when (lispm-tool-deferred tool) (throw 'found t)))
                 lispm-tools--registry)
        nil)))

(defun lispm-tools--prepare (sources callback)
  "Prepare SOURCES, all at once; call CALLBACK with their problems."
  (if (null sources)
      (funcall callback nil)
    (let ((waiting (length sources)) problems)
      (dolist (source sources)
        (funcall (plist-get source :prepare)
                 (lambda (problem)
                   (when problem (push problem problems))
                   (when (zerop (cl-decf waiting))
                     (funcall callback (nreverse problems)))))))))

(defun lispm-tools--description (tool)
  "The first line of TOOL's description."
  (let ((description (lispm-tool-description tool)))
    (car (split-string (if (functionp description) (funcall description) description) "\n"))))

(defun lispm-tools--find (query tools limit &optional sources)
  "The TOOLS QUERY asks for, at most LIMIT of them.
\"select:A,B\" asks for those tools by name.  Otherwise a word of
QUERY naming one of SOURCES (as `lispm-tool-sources' describes them)
keeps that source's tools only, and each other word found in a tool's
name counts twice, in its description once; with no other word, all
the source's tools are found."
  (if (string-prefix-p "select:" query)
      (let ((names (split-string (substring query 7) "[, ]+" t)))
        (seq-filter (lambda (tool) (member (lispm-tool-name tool) names)) tools))
    (let* ((words (split-string (downcase query) "[^[:alnum:]_]+" t))
           (named (seq-filter (lambda (source) (member (downcase (plist-get source :name)) words))
                              sources))
           (words (seq-difference words (mapcar (lambda (source) (downcase (plist-get source :name)))
                                                named)))
           (tools (if named
                      (seq-filter (lambda (tool)
                                    (seq-some (lambda (source)
                                                (funcall (plist-get source :provides)
                                                         (lispm-tool-name tool)))
                                              named))
                                  tools)
                    tools))
           (scored (mapcar (lambda (tool)
                             (let ((name (downcase (lispm-tool-name tool)))
                                   (description (downcase (lispm-tools--description tool))))
                               (cons (cl-loop for word in words
                                              sum (+ (if (string-search word name) 2 0)
                                                     (if (string-search word description) 1 0)))
                                     tool)))
                           tools)))
      (mapcar #'cdr (seq-take (seq-filter (lambda (pair) (or (> (car pair) 0) (null words)))
                                          (cl-stable-sort scored #'> :key #'car))
                              limit)))))

(defun lispm-tools-load (agent names)
  "Offer AGENT the deferred tools NAMES from its next request on."
  (let ((new (seq-remove (lambda (name) (member name (lispm-agent-loaded-tools agent))) names)))
    (when new
      (setf (lispm-agent-loaded-tools agent) (append (lispm-agent-loaded-tools agent) new))
      (run-hook-with-args 'lispm-tools-changed-functions agent))))

(defconst lispm-tools--loaded-prefix "Tools now available: "
  "How a search_tools result starts the list of the tools it loaded.")

(lispm-register-tool
 (lispm-tool--create
  :name "search_tools"
  :description
  (lambda ()
    (concat "Find and load tools you do not have yet, when a request needs one: they are yours "
            "from your next step on, for the rest of the conversation. Search by keywords "
            "(\"screenshot click\"), a source's name, or exact names (\"select:NAME,NAME\").\n"
            "Sources:\n"
            (mapconcat (lambda (source)
                         (format "- %s: %s" (plist-get source :name) (plist-get source :description)))
                       (lispm-tools--sources) "\n")))
  :input-schema
  '((type . "object")
    (properties (query (type . "string")
                       (description . "Keywords, a source's name, or select:NAME,NAME."))
                (max_results (type . "integer")
                             (description . "How many tools to load for keywords (default 5).")))
    (required . ["query"]))
  :permission 'read
  :async t
  :function
  (lambda (done input)
    (let ((agent lispm-current-agent)
          (query (alist-get 'query input))
          (limit (alist-get 'max_results input)))
      (unless (stringp query) (error "Say what tools to search for"))
      (lispm-tools--prepare
       (seq-filter (lambda (source) (plist-get source :pending)) (lispm-tools--sources))
       (lambda (problems)
         (let ((found (lispm-tools--find
                       query
                       (seq-remove (lambda (tool) (member (lispm-tool-name tool)
                                                          (lispm-agent-loaded-tools agent)))
                                   (lispm-tools--deferred))
                       (if (integerp limit) limit lispm-tool-search-results)
                       (lispm-tools--sources))))
           (lispm-tools-load agent (mapcar #'lispm-tool-name found))
           (funcall done
                    (concat (if found
                                (concat lispm-tools--loaded-prefix
                                        (mapconcat #'lispm-tool-name found ", ") ".\n"
                                        (mapconcat (lambda (tool)
                                                     (format "- %s: %s" (lispm-tool-name tool)
                                                             (lispm-tools--description tool)))
                                                   found "\n"))
                              (format "No tools match %S; you have loaded: %s." query
                                      (if (lispm-agent-loaded-tools agent)
                                          (string-join (lispm-agent-loaded-tools agent) ", ")
                                        "none")))
                            (if problems (concat "\n" (string-join problems "\n")) ""))))))))))

(defvar lispm-session-mode)
(defvar lispm-session-agent)

(defun lispm-tools--used ()
  "Tools the session in the current buffer loaded with search_tools or called."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil) names)
        (while (re-search-forward
                (concat "^\\*\\* \\(?:[A-Z]+ \\)?\\([^ \n]+\\)\\|^"
                        (regexp-quote lispm-tools--loaded-prefix) "\\(.*\\)\\.$")
                nil t)
          (if (match-beginning 1)
              (push (match-string-no-properties 1) names)
            (setq names (append (reverse (split-string (match-string-no-properties 2) ", " t))
                                names))))
        (delete-dups (nreverse names))))))

(defun lispm-tools--restore ()
  "Load again the deferred tools the session in this buffer had loaded.
They count as loaded at once, so the session's next request has them,
waiting for them if a server has to start."
  (when-let* ((agent (and (bound-and-true-p lispm-session-mode) lispm-session-agent)))
    (let* ((sources (lispm-tools--sources))
           (names (seq-filter (lambda (name)
                                (let ((tool (lispm-get-tool name)))
                                  (if tool
                                      (lispm-tool-deferred tool)
                                    (seq-some (lambda (source) (funcall (plist-get source :provides) name))
                                              sources))))
                              (lispm-tools--used))))
      (when names
        (setf (lispm-agent-loaded-tools agent)
              (seq-uniq (append (lispm-agent-loaded-tools agent) names)))
        (lispm-tools--prepare
         (seq-filter (lambda (source)
                       (and (plist-get source :pending)
                            (seq-some (plist-get source :provides) names)))
                     sources)
         (lambda (problems)
           (when problems
             (message "LispM: %s" (string-join problems "; ")))))))))

(add-hook 'lispm-session-mode-hook #'lispm-tools--restore)

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
  (let* ((lines (split-string (string-remove-suffix "\n" text) "\n"))
         (total (length lines))
         (offset (max 1 (or offset 1)))
         (limit (or limit 2000))
         (shown (seq-take (nthcdr (1- offset) lines) limit))
         (n (1- offset)))
    (concat (mapconcat (lambda (line) (format "%6d\t%s" (cl-incf n) line)) shown "\n")
            (let ((more (- total offset (length shown) -1)))
              (if (> more 0)
                  (format "\n[... %d more line%s; continue with offset %d]"
                          more (if (= more 1) "" "s") (+ offset (length shown)))
                "")))))

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
  "Make BUFFER's text TEXT as one undo step, journaled and saved.
Then tell `lispm-buffer-change-functions'."
  (let ((old (lispm-tools--buffer-text buffer)))
    (lispm-tools--replace-text buffer text)
    ;; The change is made: whatever shows it must not undo the call.
    (with-demoted-errors "LispM: showing a change: %S"
      (run-hook-with-args 'lispm-buffer-change-functions buffer old text))))

(defun lispm-tools--replace-text (buffer text)
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

;;;; Searching
;;
;; search_files is Emacs-native: project.el lists the files (so a git
;; project's .gitignore applies), and Emacs regexps search them in
;; buffers, a file's own buffer when Emacs visits it, so unsaved edits
;; are found, as read_file shows them.  The search runs in slices
;; between which Emacs stays responsive.  Models write regexps in the
;; syntax of ripgrep and grep -E; `lispm-tools--emacs-regexp' translates.

(defcustom lispm-search-max-matches 200
  "Matching lines after which search_files stops."
  :type 'integer
  :group 'lispm)

(defcustom lispm-search-max-file-size (* 1024 1024)
  "Files larger than this many bytes are not searched."
  :type 'integer
  :group 'lispm)

(defconst lispm-tools--search-slice 0.03
  "Seconds search_files works before letting Emacs handle other events.")

(defun lispm-tools--bracket (pattern start)
  "Translate the bracket expression at START in PATTERN.
Return (EMACS-REGEXP . END), END being the position after it."
  (let ((i (1+ start)) (n (length pattern)) negated rbracket dash caret items)
    (when (and (< i n) (eq (aref pattern i) ?^)) (setq negated t) (cl-incf i))
    (when (and (< i n) (eq (aref pattern i) ?\])) (setq rbracket t) (cl-incf i))
    (catch 'end
      (while t
        (when (>= i n) (error "Unterminated [ in %s" pattern))
        (let ((c (aref pattern i)))
          (cond
           ((eq c ?\]) (cl-incf i) (throw 'end nil))
           ((and (eq c ?\[) (< (1+ i) n) (eq (aref pattern (1+ i)) ?:))
            (let ((close (string-search ":]" pattern (+ i 2))))
              (unless close (error "Unterminated [: in %s" pattern))
              (push (substring pattern i (+ close 2)) items)
              (setq i (+ close 2))))
           ((eq c ?\\)
            (when (>= (1+ i) n) (error "Unterminated [ in %s" pattern))
            (let ((d (aref pattern (1+ i))))
              (setq i (+ i 2))
              (pcase d
                (?d (push "0-9" items))
                (?s (push "[:space:]" items))
                (?w (push "[:alnum:]_" items))
                (?t (push "\t" items))
                (?n (push "\n" items))
                (?\] (setq rbracket t))
                (?- (setq dash t))
                (?^ (setq caret t))
                ((or ?D ?S ?W) (error "\\%c inside [...] is not supported" d))
                (_ (push (string d) items)))))
           (t (push (string c) items) (cl-incf i))))))
    (cons (if (and caret (not negated) (not rbracket) (not dash) (null items))
              "\\^"
            (concat "[" (if negated "^" "") (if rbracket "]" "")
                    (apply #'concat (nreverse items))
                    (if caret "^" "") (if dash "-" "") "]"))
          i)))

(defun lispm-tools--emacs-regexp (pattern)
  "PATTERN, a regexp in the syntax of ripgrep and grep -E, as an Emacs regexp.
Groups, alternation and intervals lose their backslashes' meaning in
the translation; \\d, \\s and \\w become character classes.  Signal an
error for what Emacs regexps cannot express, such as lookaround."
  (let ((i 0) (n (length pattern)) out)
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((eq c ?\\)
          (when (= (1+ i) n) (error "Trailing backslash in %s" pattern))
          (let ((d (aref pattern (1+ i))))
            (setq i (+ i 2))
            (push (pcase d
                    (?d "[0-9]") (?D "[^0-9]")
                    (?s "[[:space:]]") (?S "[^[:space:]]")
                    (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?b "\\b") (?B "\\B") (?< "\\<") (?> "\\>")
                    (?t "\t") (?n "\n") (?r "\r")
                    ((pred (lambda (d) (<= ?1 d ?9))) (string ?\\ d))
                    (_ (regexp-quote (string d))))
                  out)))
         ((eq c ?\()
          (cond ((eq (string-match "(\\?:" pattern i) i)
                 (push "\\(?:" out) (setq i (+ i 3)))
                ((eq (string-match "(\\?" pattern i) i)
                 (error "(?...) groups other than (?:...), such as lookaround, are not supported: %s"
                        pattern))
                (t (push "\\(" out) (cl-incf i))))
         ((eq c ?\)) (push "\\)" out) (cl-incf i))
         ((eq c ?|) (push "\\|" out) (cl-incf i))
         ((and (eq c ?{) (eq (string-match "{\\([0-9]*\\)\\(,[0-9]*\\)?}" pattern i) i)
               (string-match-p "[0-9]" (match-string 0 pattern)))
          (push (concat "\\{" (match-string 1 pattern) (or (match-string 2 pattern) "") "\\}") out)
          (setq i (match-end 0)))
         ((eq c ?\[)
          (pcase-let ((`(,bracket . ,end) (lispm-tools--bracket pattern i)))
            (push bracket out)
            (setq i end)))
         (t (push (string c) out) (cl-incf i)))))
    (apply #'concat (nreverse out))))

(defun lispm-tools--glob-body (glob)
  "GLOB as an unanchored Emacs regexp."
  (let ((i 0) (n (length glob)) out)
    (while (< i n)
      (let ((c (aref glob i)))
        (cond
         ((eq (string-search "**/" glob i) i) (push "\\(?:.*/\\)?" out) (setq i (+ i 3)))
         ((eq (string-search "**" glob i) i) (push ".*" out) (setq i (+ i 2)))
         ((eq c ?*) (push "[^/]*" out) (cl-incf i))
         ((eq c ??) (push "[^/]" out) (cl-incf i))
         ((and (eq c ?{) (string-search "}" glob i))
          (let ((close (string-search "}" glob i)))
            (push (concat "\\(?:"
                          (mapconcat #'lispm-tools--glob-body
                                     (split-string (substring glob (1+ i) close) ",")
                                     "\\|")
                          "\\)")
                  out)
            (setq i (1+ close))))
         ((and (eq c ?\[) (string-search "]" glob (1+ i)))
          (let ((close (string-search "]" glob (+ i 2))))
            (push (concat "[" (replace-regexp-in-string "\\`!" "^" (substring glob (1+ i) close)) "]")
                  out)
            (setq i (1+ close))))
         (t (push (regexp-quote (string c)) out) (cl-incf i)))))
    (apply #'concat (nreverse out))))

(defun lispm-tools--glob-predicate (glob directory)
  "Predicate on file names matching GLOB, a path glob under DIRECTORY.
A GLOB without a slash is matched against the file's name alone."
  (let ((regexp (concat "\\`" (lispm-tools--glob-body glob) "\\'")))
    (if (string-search "/" glob)
        (lambda (file) (string-match-p regexp (file-relative-name file directory)))
      (lambda (file) (string-match-p regexp (file-name-nondirectory file))))))

(defun lispm-tools--searchable-directory-p (directory)
  "Non-nil if a search outside a project should descend into DIRECTORY."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (not (or (string-prefix-p "." name)
             (member name vc-directory-exclusion-list)
             (member name '("node_modules"))))))

(defun lispm-tools--search-files (target glob)
  "The files a search of TARGET, a file or directory, goes through.
Those of the project around TARGET (as `project-files' lists them),
else all those below it; only those matching GLOB, if given."
  (let ((files (cond ((file-regular-p target) (list target))
                     ((not (file-directory-p target))
                      (error "No such file or directory: %s" target))
                     ((let ((process-environment (lispm--git-environment)))
                        (when-let* ((project (project-current nil target)))
                          ;; The project's backend may not work here, such as
                          ;; a checkout of a VCS that is not installed.
                          (ignore-errors (project-files project (list target))))))
                     (t (sort (directory-files-recursively
                               target "" nil #'lispm-tools--searchable-directory-p)
                              #'string<)))))
    (if glob
        (seq-filter (lispm-tools--glob-predicate
                     glob (if (file-directory-p target) target (file-name-directory target)))
                    files)
      files)))

(defun lispm-tools--matches (regexp label limit)
  "Lines of the current buffer matching REGEXP, at most LIMIT.
Each is a string LABEL:LINE:TEXT, TEXT cut at 300 characters."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((line 1) (bol (point-min)) matches)
        (while (and (< (length matches) limit)
                    (not (eobp))
                    (re-search-forward regexp nil t))
          (let ((start (line-beginning-position)))
            (setq line (+ line (count-lines bol start))
                  bol start)
            (push (format "%s:%d:%s" label line
                          (truncate-string-to-width
                           (buffer-substring-no-properties start (line-end-position)) 300 nil nil "…"))
                  matches)
            (forward-line 1)))
        (nreverse matches)))))

(defun lispm-tools--search (agent regexp case-fold files done)
  "Search FILES for REGEXP in slices, and call DONE with what is found.
CASE-FOLD non-nil ignores case.  The search is abandoned if the turn
of AGENT in which it started ends meanwhile."
  (let ((root (lispm-tools--root))
        (turn (and agent (lispm-agent-busy-p agent) (lispm-agent-turn agent)))
        (buffer (generate-new-buffer " *lispm-search*" t))
        (visiting (make-hash-table :test #'equal))
        (matches nil) (count 0) (large 0) (truncated nil))
    (dolist (b (buffer-list))
      (when-let* ((file (buffer-file-name b)))
        (puthash (expand-file-name file) b visiting)))
    (cl-labels
        ((label (file)
           (if (file-in-directory-p file root) (file-relative-name file root)
             (abbreviate-file-name file)))
         (search-file (file)
           (let ((open (gethash file visiting))
                 (limit (- lispm-search-max-matches count)))
             (if (buffer-live-p open)
                 (with-current-buffer open
                   (let ((case-fold-search case-fold))
                     (lispm-tools--matches regexp (label file) limit)))
               (with-current-buffer buffer
                 (let* ((size (or (file-attribute-size (file-attributes file)) 0))
                        (small (<= size lispm-search-max-file-size))
                        ;; Read files as they are, no decompression or
                        ;; decryption, and as UTF-8: detecting each file's
                        ;; coding system would double the time.
                        (file-name-handler-alist nil)
                        (coding-system-for-read 'utf-8))
                   (erase-buffer)
                   ;; Binary files are skipped; of a large one only the
                   ;; head is read, to tell whether it is.
                   (when (and (ignore-errors
                                (insert-file-contents file nil 0 (if small size 8000))
                                t)
                              (not (save-excursion
                                     (goto-char (point-min))
                                     (search-forward "\0" (min (point-max) 8000) t)))
                              (or small (progn (cl-incf large) nil)))
                     (let ((case-fold-search case-fold))
                       (lispm-tools--matches regexp (label file) limit))))))))
         (finish ()
           (kill-buffer buffer)
           (funcall done
                    (if (null matches)
                        (concat "No matches."
                                (if (> large 0) (format " (%d files over %d bytes were not searched)"
                                                        large lispm-search-max-file-size)
                                  ""))
                      (concat (string-join (nreverse matches) "\n")
                              (if truncated
                                  (format "\n[stopped after %d matches; narrow the search with path or glob]"
                                          count)
                                "")
                              (if (> large 0)
                                  (format "\n[%d files over %d bytes were not searched]"
                                          large lispm-search-max-file-size)
                                "")))))
         (step ()
           (condition-case err
               (if (and turn (or (not (lispm-agent-busy-p agent))
                                 (/= turn (lispm-agent-turn agent))))
                   (kill-buffer buffer)
                 (let ((deadline (+ (float-time) lispm-tools--search-slice)))
                   ;; At least one file per slice, however short the slice.
                   (while (progn
                            (let ((found (and files (search-file (pop files)))))
                              (setq matches (nconc (nreverse found) matches)
                                    count (+ count (length found))
                                    truncated (>= count lispm-search-max-matches)))
                            (and files (not truncated) (< (float-time) deadline))))
                   (if (and files (not truncated))
                       (run-at-time 0 nil #'step)
                     (finish))))
             (error (when (buffer-live-p buffer) (kill-buffer buffer))
                    (funcall done (error-message-string err) t)))))
      (step))))

(lispm-deftool search_files
    ((pattern string "Regular expression, in the syntax of ripgrep or grep -E: (a|b), \\d, \\w, \\s, x{2,3}, [^...]; no lookaround.")
     (path string "File or directory to search (default: your root)." :optional t)
     (glob string "Only search files matching this glob, e.g. \"*.el\" or \"src/**/*.ts\"." :optional t)
     (ignore-case boolean "Match regardless of case." :optional t))
  "Search file contents; returns matching lines as path:line:text.
Searches the project's files (a git project's .gitignore applies) and
the unsaved contents of files open in Emacs."
  :async t
  (let* ((agent lispm-current-agent)
         (case-fold (or ignore-case (string-prefix-p "(?i)" pattern)))
         (regexp (lispm-tools--emacs-regexp (string-remove-prefix "(?i)" pattern)))
         (target (lispm-tools--file (or path "."))))
    ;; An invalid regexp fails here, not file after file.
    (string-match-p regexp "")
    (lispm-tools--search agent regexp case-fold (lispm-tools--search-files target glob) done)))

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
  :changes (lambda (path old-text new-text &optional replace-all)
             (let ((buffer (find-file-noselect (lispm-tools--file path))))
               (cons buffer (car (lispm-tools--replace (lispm-tools--buffer-text buffer)
                                                       old-text new-text replace-all)))))
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
  :changes (lambda (path content)
             (cons (find-file-noselect (lispm-tools--file path)) content))
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
  :changes (lambda (buffer old-text new-text &optional replace-all)
             (let ((b (lispm-tools--buffer buffer)))
               (cons b (car (lispm-tools--replace (lispm-tools--buffer-text b)
                                                  old-text new-text replace-all)))))
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

(defcustom lispm-eval-timeout 120
  "Seconds eval_elisp waits for a value promised with `lispm-async'."
  :type 'integer
  :group 'lispm)

(cl-defstruct (lispm-async (:constructor lispm-async--create) (:copier nil))
  "A value computed asynchronously: see `lispm-async'."
  settled value failed waiters)

(defun lispm-async (function)
  "Return a promise of the value FUNCTION computes asynchronously.
FUNCTION is called at once with a function RESOLVE, to call later with
the value, and non-nil as second argument if it is a failure.  Code
that eval_elisp evaluates can end with such a promise, and the agent
gets the value once it is resolved:

  (lispm-async
   (lambda (resolve)
     (url-retrieve URL (lambda (_) (funcall resolve (buffer-string))))))"
  (let ((promise (lispm-async--create)))
    (funcall function
             (lambda (value &optional failed)
               (unless (lispm-async-settled promise)
                 (setf (lispm-async-settled promise) t
                       (lispm-async-value promise) value
                       (lispm-async-failed promise) failed)
                 (dolist (waiter (lispm-async-waiters promise))
                   (funcall waiter value failed)))))
    promise))

(defun lispm-async-then (promise function)
  "Call FUNCTION with PROMISE's value and failure flag once it is settled."
  (if (lispm-async-settled promise)
      (funcall function (lispm-async-value promise) (lispm-async-failed promise))
    (push function (lispm-async-waiters promise))))

(defun lispm-tools--eval-result (printed value)
  "What eval_elisp returns: PRINTED output, then VALUE."
  (let ((print-length 100) (print-level 10))
    (concat (if (string-empty-p printed) "" (concat printed "\n"))
            "=> " (if (stringp value) value (prin1-to-string value)))))

(lispm-deftool eval_elisp
    ((code string "One or more Emacs Lisp forms, evaluated in order with lexical binding."))
  "Evaluate Emacs Lisp in the running Emacs and return the last value,
plus anything printed.  This reaches all of Emacs: buffers, windows,
processes, the network, org, magit, any installed package.  Find
functions with apropos; learn them with describe_symbol and read_info.
To wait for something asynchronous (a process, a page loading, a
timer), end with (lispm-async (lambda (resolve) ...)) and call
\(funcall resolve VALUE) once the value is there."
  :permission exec
  :async t
  (let* ((output (generate-new-buffer " *lispm-eval*" t))
         (standard-output output)
         (default-directory (lispm-tools--root))
         value printed)
    (unwind-protect
        (progn
          (dolist (form (lispm-tools--read-forms code))
            (setq value (eval form t)))
          (setq printed (with-current-buffer output (buffer-string))))
      (kill-buffer output))
    (if (not (lispm-async-p value))
        (funcall done (lispm-tools--eval-result printed value))
      (let ((timer (run-at-time
                    lispm-eval-timeout nil
                    (lambda ()
                      (funcall done (format "Timed out after %d seconds waiting for the promised value."
                                            lispm-eval-timeout)
                               t)))))
        (lispm-async-then value
                          (lambda (result failed)
                            (cancel-timer timer)
                            (funcall done (lispm-tools--eval-result printed result) failed)))))))

(provide 'lispm-tools)
;;; lispm-tools.el ends here
