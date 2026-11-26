;;; lispm-tool-core.el --- Tool registry, deftool, invocation, and buffers  -*- lexical-binding: t; -*-

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

;; Core tool abstraction (`lispm-tool', `lispm-deftool'), tool
;; invocation, preview diffs, live buffer snapshotting, and file/buffer
;; helpers shared across built-in tools and permissions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-agents-context)
(require 'lispm-deftool-macros)

(defvar lispm-session-mode)
(defvar lispm-session-agent)
(defvar lispm-current-call)

(defcustom lispm-rationale-permissions '(write shell exec)
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

(defcustom lispm-revert-after-calls t
  "Whether buffers are reverted after a call that may have changed files.
After each such call, the buffers without unsaved changes of the files
under the agent's root that changed on disk are reverted, so the user
sees what a shell command, Lisp or an MCP server did there.  Whatever
this says, a tool brings a buffer it uses up to date first."
  :type 'boolean
  :group 'lispm)

(defconst lispm-tools--still-permissions '(read memory comm web lisp)
  "Permission classes whose tools change no file of the agent's.
No buffers are reverted after their calls.  A Lisp program changes
files only through the tools it calls, which revert them.")

(defvar lispm-revert-skip-functions nil
  "Hook of predicates `(BUFFER AGENT)' skipping `lispm-tools--revert-changed'.
If any function returns non-nil for BUFFER after AGENT completes a call,
that buffer is not reverted on disk-change detection.")

(defvar lispm-tools--revert-paths nil
  "Dynamic scope restricting `lispm-tools--revert-changed' to affected paths.
May be nil (scan root), `none' (skip all), or a list of file paths.")

(cl-defstruct (lispm-tool (:constructor lispm-make-tool) (:copier nil))
  "A tool callable by agents.
Tools defined with `lispm-deftool' have PARAMS, from which their schema
is made and their arguments checked.  Tools from elsewhere, such as
MCP servers, have an INPUT-SCHEMA instead (a JSON schema as an alist,
or a function returning one) and a FUNCTION taking the whole input.
A DEFERRED tool is not offered to an agent until search_tools has
found it for that agent.  PREVIEW, or DIFF, tells what a call would
do; CHANGES, for a tool that changes a buffer, what a call would make
of it; TARGETS, for a tool whose arguments do not name the files it
writes, which (see `lispm-deftool').  Calls of an ALWAYS-ASK tool
await the user's approval whatever the policy says.  FILE is the file
a tool defined with `lispm-deftool' was loaded from, if any.  A tool
with FOR, a function of an agent, is only that of agents it holds for.
CLASS, a function of a call's arguments, says which class the call
belongs to when it does less than the tool can (see
`lispm-tool-call-permission').  CONCURRENCY is `parallel',
`resource', `editor', or a function of the call's arguments returning one
of those (or `(resource . KEYS)') to guide intra-turn scheduling.
KEYWORDS are explicit search terms, a list of strings; they do not
change the tool's name or schema."
  name description params input-schema permission function async preview deferred
  changes targets always-ask file
  ;; Last: packages compiled before it read the others where they were.
  diff for class keywords spotlight param-schema concurrency)

(defalias 'lispm-tool--create #'lispm-make-tool)

(defvar lispm-tools--registry (make-hash-table :test #'equal)
  "Tool name -> `lispm-tool'.")

(defun lispm-register-tool (tool)
  "Make TOOL available to agents, replacing any tool of the same name."
  (puthash (lispm-tool-name tool) tool lispm-tools--registry)
  tool)

(defun lispm-get-tool (name)
  "The tool called NAME (a string or symbol), or nil."
  (gethash (format "%s" name) lispm-tools--registry))

(defun lispm-tools-unregister (name)
  "Remove any tool called NAME (a string or symbol) from the registry."
  (remhash (format "%s" name) lispm-tools--registry))

(defun lispm-tools-count ()
  "Return the number of currently registered tools."
  (hash-table-count lispm-tools--registry))

(defun lispm-tools-registered ()
  "Return a list of all registered `lispm-tool' structs."
  (hash-table-values lispm-tools--registry))


(defun lispm-tools-snapshot ()
  "The tools registered now, for `lispm-tools-adopt' to tell those defined since."
  (copy-hash-table lispm-tools--registry))

(defmacro lispm-deftool (name params docstring &rest body)
  "Define a tool NAME that agents can call.
NAME's symbol name is what the model sees.  PARAMS is a list of
\(ARG TYPE DESCRIPTION . OPTIONS), TYPE being string, integer,
number, boolean, array or object, and OPTIONS among :optional t,
:enum LIST and :items SCHEMA.  The model sees ARG with dashes
turned into underscores.  DOCSTRING is the description the model
reads to decide when to call the tool.

BODY may start with keyword options:
  :permission CLASS  read (the default), memory, comm, write, shell or exec
  :async t           BODY gets a function `done' to call later with
                     the result (and non-nil as second argument on
                     failure); its own value is ignored
  :preview FUNCTION  called with the same arguments; returns a string
                     shown while the call awaits approval
  :diff FUNCTION     instead of :preview, for a call that changes a file
                     or buffer: called with the same arguments, returns
                     (OLD NEW LABEL), the text there now and the text
                     the call would leave, whose diff, LABEL naming the
                     file, is the preview; it is made in the background
  :changes FUNCTION  for a tool that changes a buffer: called with the
                     same arguments, returns (BUFFER . TEXT), the buffer
                     the call would change and the text it would leave
                     there; the change is then shown in BUFFER while the
                     call is written and while it awaits approval
  :edit SPEC         declarative file/buffer edit plist (:file or :buffer,
                     :create, :transform (TEXT-VAR FORM), :bind VARS) that
                     synthesizes :diff, :changes and atomic buffer write
  :targets FUNCTION  for a tool of class write whose `path' or `buffer'
                     argument does not name what it writes: called with
                     the same arguments before the call runs, returns the
                     files it would change, for the rule that writes
                     outside the agent's root always ask (see
                     `lispm-tool-action'); it changes nothing
  :deferred t        offered only once search_tools finds it (see
                     `lispm-tool-sources'): for tools seldom needed
  :always-ask t      every call awaits the user's approval, whatever
                     the policy says (one that denies the class still
                     denies it): for calls whose effects outlast the
                     session, such as saving code later sessions run
  :for FUNCTION      called with an agent: the tool is that agent's
                     only if it returns non-nil, as a character's own
                     tools are only characters'
  :class FUNCTION    called with the same arguments before the call is
                     decided: returns the permission class of this call
                     when it does less than the tool can, as a shell
                     command that only reads does, or nil for the tool's
                     own; it changes nothing (see
                     `lispm-tool-call-permission')
  :concurrency SPEC  `parallel', `resource', `editor', or a function of the
                     call's arguments returning one (or `(resource . KEYS)')
  :keywords LIST     explicit search terms, a list of strings; these
                     aid discovery without adding callable aliases
  :spotlight FN      for a tool that reads or looks up places: called
                     with the result text and the call's arguments once
                     the call succeeds, returns a spotlight plist or nil

Otherwise BODY's value is the result: a string, or any object, which
is printed; or a `lispm-tool-result', text with image or video files
the model sees too.  Signal an error to report a failure to the model."
  (declare (indent 3) (doc-string 3)
           (debug (&define name sexp stringp [&rest keywordp form] def-body)))
  (let ((permission 'read) async preview diff changes edit targets deferred always-ask for class
        concurrency keywords spotlight
        (compiled (lispm-tools-compile-params name params))
        (args (mapcar #'car params)))
    (while (keywordp (car body))
      (let ((opt (pop body)))
        (unless body
          (error "Missing value for %S in `lispm-deftool' %s" opt name))
        (pcase opt
          (:permission (setq permission (pop body)))
          (:async (setq async (pop body)))
          (:preview (setq preview (pop body)))
          (:diff (setq diff (pop body)))
          (:changes (setq changes (pop body)))
          (:edit (setq edit (pop body)))
          (:targets (setq targets (pop body)))
          (:deferred (setq deferred (pop body)))
          (:always-ask (setq always-ask (pop body)))
          (:for (setq for (pop body)))
          (:class (setq class (pop body)))
          (:concurrency (setq concurrency (pop body)))
          (:keywords (setq keywords (pop body)))
          (:spotlight (setq spotlight (pop body)))
          (_ (error "Unknown `lispm-deftool' option %S in %s" opt name)))))
    (unless body
      (error "Missing body in `lispm-deftool' %s" name))
    (when edit
      (when (or async diff changes)
        (error "`:edit' in `lispm-deftool' %s cannot combine with `:async', `:diff' or `:changes'"
               name))
      (pcase-let ((`(,ediff ,echanges ,etargets ,ebody)
                   (lispm-tools-expand-edit name args edit body)))
        (setq diff ediff
              changes echanges
              targets (or targets etargets)
              body (list ebody))))
    `(lispm-register-tool
      (lispm-make-tool
       :name ,(symbol-name name)
       :description ,docstring
       :params ',params
       :param-schema ',compiled
       :permission ',permission
       :async ,async
       :preview ,preview
       :diff ,diff
       :changes ,changes
       :targets ,targets
       :deferred ,deferred
       :always-ask ,always-ask
       :for ,for
       :class ,class
       :concurrency ,concurrency
       :keywords ,keywords
       :spotlight ,spotlight
       :file (or load-file-name
                 (and buffer-file-name (string-suffix-p ".el" buffer-file-name) buffer-file-name))
       :function ,(if async
                      `(lambda (done ,@args) (ignore done ,@args) ,@body)
                    `(lambda ,args (ignore ,@args) ,@body))))))

(defun lispm-tool--normalize-resource (resource)
  "Normalize RESOURCE key into canonical comparable form."
  (cond
   ((stringp resource)
    (let ((ref (condition-case nil
                   (if (bound-and-true-p lispm-current-agent)
                       (lispm-tools-file resource)
                     (expand-file-name (car (lispm-tools-reference resource))))
                 (error (expand-file-name resource)))))
      (if (and (not (file-remote-p ref)) (file-exists-p ref))
          (file-truename ref)
        ref)))
   ((and (consp resource) (eq (car resource) 'buffer) (stringp (cdr resource)))
    (cons 'buffer
          (condition-case nil
              (lispm-tools-buffer-name (cdr resource))
            (error (cdr resource)))))
   (t resource)))

(defun lispm-tool-call-resources (tool input)
  "Return deduplicated canonical resource keys targeted by TOOL on INPUT."
  (let ((targets (lispm-tool-targets tool))
        (buf-arg (alist-get 'buffer input))
        raw)
    (cond
     (targets
      (let ((got (condition-case nil
                     (apply targets (lispm-tools-arguments tool input))
                   (error nil))))
        (setq raw (if (listp got) got (list got)))))
     (t
      (when-let* ((path (or (alist-get 'path input) (alist-get 'file input)))
                  ((stringp path)))
        (push path raw))
      (when (stringp buf-arg)
        (push (cons 'buffer buf-arg) raw))))
    (when (stringp buf-arg)
      (let* ((resolved (condition-case nil
                           (lispm-tools-buffer-name buf-arg)
                         (error buf-arg)))
             (buf (get-buffer resolved)))
        (when-let* (((buffer-live-p buf))
                    (file (buffer-file-name buf)))
          (push file raw))))
    (delete-dups (mapcar #'lispm-tool--normalize-resource (delq nil raw)))))

(defun lispm-tool--canonical-concurrency (raw)
  "Normalize RAW concurrency specification to `(CLASS . KEYS)'."
  (pcase raw
    ('parallel '(parallel))
    ('editor '(editor))
    ('resource '(resource))
    (`(resource . ,keys)
     (cons 'resource (delete-dups (mapcar #'lispm-tool--normalize-resource keys))))
    (_ '(editor))))

(defun lispm-tool-concurrency-spec (tool input)
  "Return conflict spec `(CLASS . KEYS)' for calling TOOL with INPUT.
CLASS is `parallel', `resource', or `editor'."
  (if (not tool)
      '(parallel)
    (let ((explicit (lispm-tool-concurrency tool)))
      (cond
       (explicit
        (lispm-tool--canonical-concurrency
         (if (functionp explicit)
             (condition-case nil
                 (apply explicit (lispm-tools-arguments tool input))
               (error 'editor))
           explicit)))
       ((memq (lispm-tool-permission tool) '(read web comm shell exec))
        '(parallel))
       ((eq (lispm-tool-permission tool) 'memory)
        '(resource (memory)))
       ((eq (lispm-tool-permission tool) 'lisp)
        '(editor))
       ((eq (lispm-tool-permission tool) 'write)
        (if-let* ((resources (lispm-tool-call-resources tool input)))
            (cons 'resource resources)
          '(editor)))
       (t '(editor))))))

(defun lispm-tool-concurrency-conflict-p (spec-a spec-b)
  "Return non-nil when calls with SPEC-A and SPEC-B must not overlap."
  (let ((ca (car spec-a))
        (cb (car spec-b)))
    (cond
     ((or (eq ca 'editor) (eq cb 'editor)) t)
     ((and (eq ca 'resource) (eq cb 'resource))
      (let ((ra (cdr spec-a))
            (rb (cdr spec-b)))
        (or (null ra) (null rb)
            (and (cl-intersection ra rb :test #'equal) t))))
     (t nil))))

(defun lispm-tool-to-spec (tool &optional function description)
  "Return a canonical `lispm-tool-spec' representing TOOL.
FUNCTION, when non-nil, overrides TOOL's handler function in the spec.
DESCRIPTION, when non-nil, overrides TOOL's description (for example a
short summary from `lispm-tools-description')."
  (let ((name (lispm-tool-name tool))
        (desc (or description (lispm-tool-description tool)))
        (fn (or function (lispm-tool-function tool)))
        (raw-schema (lispm-tool-input-schema tool)))
    (if raw-schema
        (lispm-tool-spec-create name desc :function fn :input-schema raw-schema)
      (pcase-let* ((`(,base-props . ,base-req)
                    (or (condition-case nil (lispm-tool-param-schema tool) (args-out-of-range nil))
                        (lispm-tools-compile-params name (lispm-tool-params tool))))
                   (why-p (memq (lispm-tool-permission tool) lispm-rationale-permissions))
                   (props (if why-p
                              (cons '(why (type . "string")
                                          (description . "One short sentence for the human reviewing this call: why it is needed."))
                                    base-props)
                            base-props))
                   (required (vconcat (if why-p (cons "why" base-req) base-req))))
        (lispm-tool-spec-create name desc
                                :function fn
                                :properties props
                                :required required)))))

(defun lispm-tool-schema (tool)
  "The API definition of TOOL."
  (lispm-tool-spec-to-api (lispm-tool-to-spec tool)))

(defun lispm-tools-allowed-p (agent name)
  "Non-nil if AGENT's role gives it the tool NAME, offered or deferred.
In a role's list of tools, a name ending with * stands for every
tool whose name starts with the rest, such as mcp__peekaboo__*.  A
role giving the skill tool gives search_tools too, which finds skills.
A tool for some agents only (its :for) is never another's."
  (let ((spec (lispm-principal-tools agent))
        (for (when-let* ((tool (gethash name lispm-tools--registry)))
               (lispm-tool-for tool))))
    (and (or (null for) (funcall for agent))
         (or (eq spec t)
             (and (listp spec)
                  (let ((patterns (mapcar (lambda (x) (format "%s" x)) spec)))
                    (or (seq-some (lambda (pattern)
                                    (if (string-suffix-p "*" pattern)
                                        (string-prefix-p (substring pattern 0 -1) name)
                                      (equal pattern name)))
                                  patterns)
                        (and (equal name "search_tools") (member "skill" patterns)))))))))

(defun lispm-agent-tools* (agent)
  "Tools AGENT may call, sorted by name (a stable request prefix).
Those its role gives it (see `lispm-tools-allowed-p').  Deferred tools
are left out until search_tools has loaded them for AGENT;
search_tools itself is there when there is anything to find."
  (let* ((searchable (lispm-tools-searchable-p))
         tools)
    (maphash (lambda (name tool)
               (when (and (lispm-tools-allowed-p agent name)
                          (or (not (lispm-tool-deferred tool))
                              (member name (lispm-principal-loaded-tools agent)))
                          (or searchable (not (equal name "search_tools"))))
                 (push tool tools)))
             lispm-tools--registry)
    (sort tools (lambda (a b) (string< (lispm-tool-name a) (lispm-tool-name b))))))

(define-error 'lispm-tool-argument-error "Invalid tool argument" 'lispm-tool-error)

(defun lispm-asked (call)
  "What CALL awaits approval for, as a plist (:tool :name :input :reasons).
The call itself; or for a call that makes calls in turn, as a Lisp
program does, the call of its that waits (see `lispm-call-ask'), with
:always, what approving it for good means, if it says.  :reasons are
why functions of `lispm-tool-action-functions' had it ask, if they
did: a list of strings."
  (or (plist-get call :asking)
      (list :tool (or (plist-get call :tool) (lispm-get-tool (plist-get call :name)))
            :name (plist-get call :name)
            :input (plist-get call :input)
            :reasons (plist-get call :reasons))))

(defun lispm-tools-argument (key type value opts)
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
      (signal 'lispm-tool-argument-error
              (list (format "Parameter `%s' must be of type %s" key type))))
    (when-let* ((enum (plist-get opts :enum)))
      (unless (member value enum)
        (signal 'lispm-tool-argument-error
                (list (format "Parameter `%s' must be one of %s" key (string-join enum ", "))))))
    (if (eq value :false) nil value)))

(defun lispm-tools-arguments (tool input)
  "Validate INPUT against TOOL's parameters; return the argument list.
A tool with a schema of its own gets INPUT whole."
  (unless (listp input)
    (signal 'lispm-tool-argument-error '("Input must be a JSON object")))
  (if (lispm-tool-input-schema tool)
      (list input)
    (lispm-tools--param-arguments tool input)))

(defun lispm-tools--param-arguments (tool input)
  "INPUT checked against TOOL's parameters, as an argument list."
  (mapcar (lambda (param)
            (pcase-let* ((`(,arg ,type ,_doc . ,opts) param)
                         (key (lispm-tools-key arg))
                         (value (alist-get key input)))
              (if (memq value '(nil :null))
                  (if (plist-get opts :optional)
                      nil
                    (signal 'lispm-tool-argument-error
                            (list (format "Missing required parameter `%s'" key))))
                (lispm-tools-argument key type value opts))))
          (lispm-tool-params tool)))

(cl-defstruct (lispm-tool-result (:constructor lispm-tool-result (text &optional images)))
  "The result of a tool that shows something: TEXT, and IMAGES, a list of
the image and video files the model sees with it, which the transcript
links and the chat shows.  A tool returns one, or hands one to `done'."
  text images)

(defconst lispm-tools--benign-stderr-regexp
  (rx bol (* (not "\n"))
      "Emacs[" (+ digit) ":" (+ digit) "] "
      "Failed to initialize color list unarchiver: "
      "Error Domain=NSCocoaErrorDomain Code=4864 "
      (* (not "\n")) (? "\n"))
  "Known benign macOS AppKit diagnostics emitted when child Emacs starts.")

(defun lispm-tools-strip-benign-stderr (text)
  "Return TEXT without known benign host startup diagnostics, or nil if nil."
  (when (stringp text)
    (save-match-data
      (replace-regexp-in-string lispm-tools--benign-stderr-regexp "" text t t))))

(defun lispm-tools--result-text (result)
  "RESULT of a tool as text for the model."
  (let ((text (cond ((stringp result) result)
                    ((null result) "OK")
                    (t (let ((print-length 200) (print-level 8))
                         (prin1-to-string result))))))
    (lispm-truncate (if (string-empty-p text) "(no output)" text)
                     lispm-tool-output-limit)))

(defconst lispm-tools-interrupted
  "Interrupted: the user pressed C-g while the call ran.  What it did until then stands."
  "Result of a call the user interrupted.")

(defun lispm-tools-interruptibly (function args)
  "Apply FUNCTION to ARGS so that C-g interrupts it, signaling `quit'.
Calls run from process sentinels and timers, where Emacs inhibits
quitting: without this, a runaway call could not be stopped.  A C-g
pressed before FUNCTION started was meant for something else, and is
left for the code around it.  Input does not end a `while-no-input'
around it either: that would leave the call without a result."
  (let ((pending quit-flag))
    (setq quit-flag nil)
    (unwind-protect
        (let ((inhibit-quit nil)
              (throw-on-input nil))
          (apply function args))
      (when pending
        (setq quit-flag pending)))))

(defun lispm-tool-call-class (tool input)
  "Return the effective permission class of calling TOOL on INPUT."
  (or (when-let* ((class (lispm-tool-class tool)))
        (condition-case nil
            (apply class (lispm-tools-arguments tool input))
          (error nil)))
      (lispm-tool-permission tool)))

(defun lispm-tools--deliver (done agent tool text errorp images &optional input call-id)
  "Give DONE the result of AGENT's call of TOOL: TEXT, ERRORP and IMAGES.
INPUT, when provided, is the call's decoded JSON input.  CALL-ID binds
`lispm-current-call' while running finish hooks.
First, if TOOL may have changed files, TRAMP forgets what it knew of
those of a remote root (see `lispm-tools--flush-remote'), and the
buffers of those under AGENT's root that changed on disk are reverted,
so that the user sees what the call did there (see
`lispm-revert-after-calls'); C-g stops that.  DONE goes on with the
turn: it records the result and may ask the model again, which C-g
must not leave half done, so quitting is inhibited meanwhile."
  (let ((inhibit-quit t))
    (when (and agent
               (not (memq (lispm-tool-permission tool) lispm-tools--still-permissions))
               (not (eq (lispm-tool-call-class tool input) 'read)))
      (with-demoted-errors "LispM: forgetting remote file attributes: %S"
        (lispm-tools--flush-remote (lispm-principal-root agent)))
      (when lispm-revert-after-calls
        ;; Hooks the reverts run are the agent's doing.
        (let* ((lispm-current-agent agent)
               (lispm-tools--revert-paths
                (when (eq (lispm-tool-permission tool) 'write)
                  (or (seq-filter #'stringp (lispm-tool-call-resources tool input))
                      'none))))
          (condition-case err
              (lispm-tools-interruptibly #'lispm-tools--revert-changed
                                          (list (lispm-principal-root agent)))
            (quit nil)
            (error
             (message "LispM: reverting changed buffers: %S" err))))))
    (with-demoted-errors "LispM: tool call hook: %S"
      (let ((lispm-current-agent agent)
            (lispm-current-call (or call-id (bound-and-true-p lispm-current-call)))
            (default-directory (if agent (lispm-principal-root agent) default-directory)))
        (run-hook-with-args 'lispm-tool-call-functions
                            agent 'finish tool input text errorp)))
    (apply done text errorp (and images (list images)))))

(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-flush-directory-properties "tramp-cache")

(defun lispm-tools--flush-remote (root)
  "Make TRAMP forget what it knows of the files of ROOT's machine, if remote.
TRAMP keeps the attributes of remote files a while, and directories'
listings.  A call that may have changed files, a shell command there,
changed them behind its back; `process-file' has TRAMP forget them so,
while `make-process' does not."
  (when (file-remote-p root)
    (tramp-flush-directory-properties (tramp-dissect-file-name root) "/")))

(defun lispm-tool-invoke (tool input done)
  "Run TOOL on INPUT, a decoded JSON object.
Call DONE exactly once with the result text and non-nil if the call
failed, and, if the result has images, a third argument: their files.
Synchronous tools call DONE before returning.  An asynchronous tool's
`done' takes the same arguments.

C-g interrupts what the tool does while Emacs runs it, a loop in
eval_elisp say, and the call fails (see `lispm-tools-interruptibly').
What an asynchronous tool waits for afterwards, a process or another
agent, is stopped by aborting the turn.  Only the tool is
interruptible: what comes after it, the result and the rest of the
turn, is not, so C-g never leaves it half done or reports a call that
ran to its end as interrupted.  DONE is never called while the tool
runs."
  (let* ((inhibit-quit t)
         (agent lispm-current-agent)
         (call-id (bound-and-true-p lispm-current-call))
         (running t)
         (called nil)
         (result nil)
         (finish (lambda (value &optional errorp images)
                   (unless called
                     (when (lispm-tool-result-p value)
                       (setq images (append images (lispm-tool-result-images value))
                             value (lispm-tool-result-text value)))
                     (let ((args (list (lispm-tools--result-text value) errorp images input call-id)))
                       (setq called t)
                       (if running
                           (setq result args)
                         (run-at-time 0 nil #'apply #'lispm-tools--deliver
                                      done agent tool args)))))))
    (with-demoted-errors "LispM: tool call hook: %S"
      (run-hook-with-args 'lispm-tool-call-functions agent 'start tool input nil nil))
    (condition-case err
        ;; What the tool says is for the model: in English, though it
        ;; reach a function that speaks to the user too.
        (lispm-in-english
          (let ((args (lispm-tools-arguments tool input)))
            (if (lispm-tool-async tool)
                (lispm-tools-interruptibly (lispm-tool-function tool) (cons finish args))
              (funcall finish (lispm-tools-interruptibly (lispm-tool-function tool) args)))))
      (error (funcall finish (error-message-string err) t))
      (quit (funcall finish lispm-tools-interrupted t)))
    (setq running nil)
    (when result
      (apply #'lispm-tools--deliver done agent tool result))))

(defun lispm-tool-preview-text (tool input &optional callback)
  "What calling TOOL with INPUT would do, as a string, or nil.
With CALLBACK, return nil, and call CALLBACK with that instead: at
once, unless it is the diff of a tool that has one (see `lispm-deftool'),
which a program makes in the background; then later.  Without CALLBACK,
Emacs waits for that program."
  (let ((diff (lispm-tool-diff tool))
        (preview (lispm-tool-preview tool))
        (later nil)
        text)
    (condition-case err
        (cond (diff
               (pcase-let ((`(,old ,new ,label) (apply diff (lispm-tools-arguments tool input))))
                 (if (not callback)
                     (setq text (lispm-diff old new label))
                   (lispm-diff old new label
                                (lambda (made &optional why)
                                  (funcall callback (or made (format "(no preview: %s)" why)))))
                   (setq later t))))
              (preview (setq text (apply preview (lispm-tools-arguments tool input)))))
      (error (setq text (format "(no preview: %s)" (error-message-string err)))))
    (cond (later nil)
          (callback (ignore (funcall callback text)))
          (t text))))

(defun lispm-tool-would-change (tool input)
  "What calling TOOL with INPUT would change: (BUFFER . TEXT), or nil.
Nil for a tool that changes no buffer; an error if INPUT makes no
valid call of TOOL."
  (when-let* ((changes (lispm-tool-changes tool)))
    (apply changes (lispm-tools-arguments tool input))))

(defun lispm-tool-would-spotlight (tool input result)
  "Return what calling TOOL with INPUT read in RESULT, or nil.
Nil for a tool without `:spotlight'."
  (when-let* ((spotlight (lispm-tool-spotlight tool)))
    (apply spotlight result (lispm-tools-arguments tool input))))

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

(defvar lispm-tool-search-functions nil
  "Functions giving search_tools more to find than deferred tools.
Each is called with the agent searching and returns a list of plists,
one per thing it can find:
  :fields  what it is found by: a list of (WEIGHT . TEXT), as
           `lispm-tools-search-score' takes them
  :line    what a result says of it, one line
  :tools   names of the tools it holds, which the search names with
           it and does not load itself, as a skill's tools, which
           come with the skill")

(defun lispm-tools-sources ()
  "The sources of deferred tools, from `lispm-tool-sources'."
  (apply #'append (mapcar #'funcall lispm-tool-sources)))

(defun lispm-tools-searchable-p ()
  "Non-nil if there are tools, sources of them, or more to find."
  (or (lispm-tools-sources)
      lispm-tool-search-functions
      (catch 'found
        (maphash (lambda (name _tool) (unless (equal name "search_tools") (throw 'found t)))
                 lispm-tools--registry)
        nil)))

;;;; Journal

(defun lispm-journal-snapshot (buffer)
  "Record BUFFER's contents before the acting agent first changes it this turn."
  (when-let* ((agent lispm-current-agent)
              ((lispm-agent-p agent)))
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

(defun lispm-tools-root ()
  "Root directory of the acting agent."
  (if lispm-current-agent (lispm-principal-root lispm-current-agent) default-directory))

(defun lispm-tools-reference (path)
  "The file and line PATH refers to, as (FILE . LINE), LINE nil if none.
PATH is a file name, or an org link to a file, with a line or not, as
agents pass places to each other: [[file:FILE::LINE]], with a
description or not, or file:FILE::LINE."
  (if (string-match "\\`\\(?:\\[\\[\\)?file:\\([^]\n]+?\\)\\(?:::\\([0-9]+\\)\\)?\\(?:\\]\\(?:\\[[^]\n]*\\]\\)?\\]\\)?\\'"
                    path)
      (cons (match-string 1 path)
            (and (match-string 2 path) (string-to-number (match-string 2 path))))
    (cons path nil)))

(defun lispm-tools-buffer-name (name)
  "The name of the buffer NAME refers to: NAME, or that of a link [[buffer:NAME]]."
  (if (string-match "\\`\\[\\[buffer:\\([^]\n]+\\)\\]\\(?:\\[[^]\n]*\\]\\)?\\]\\'" name)
      (match-string 1 name)
    name))

(defun lispm-tools-file (path)
  "PATH expanded against the acting agent's root.
PATH may be a reference to a file (see `lispm-tools-reference').  On a
remote root, an absolute PATH that names no machine names a file on the
root's (see `lispm-expand-file-name'): the agent works there."
  (lispm-expand-file-name (substitute-in-file-name (car (lispm-tools-reference path)))
                           (lispm-tools-root)))

(defun lispm-tools-label (file)
  "FILE as shown in diffs: relative to the root when inside it."
  (if (file-in-directory-p file (lispm-tools-root))
      (file-relative-name file (lispm-tools-root))
    (abbreviate-file-name file)))

(defun lispm-tools-number-lines (text offset limit)
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

(defun lispm-tools-replace (text old new all)
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

;;;; Buffers and the files on disk
;;
;; Tools read and change the buffer visiting a file, not the file, so
;; that agents see what the user sees.  Other programs change files
;; too: a shell command, git, the user's own shell.  So before a tool
;; changes or reads a buffer, the buffer is brought up to date with
;; its file, and never by asking the user, as Emacs would: one without
;; unsaved changes is reverted; one with them is in conflict with its
;; file, which a change reports to the model as an error.  Previews,
;; and the changes shown while a call is written, change nothing: they
;; go by `lispm-file-text'.  After a call that may have changed files,
;; the buffers of those under the agent's root that changed on disk are
;; reverted, so the user sees what it did (`lispm-revert-after-calls').

(defun lispm-tools-state (buffer)
  "What BUFFER is with respect to the file it visits; nothing is changed.
`current': up to date with it, or visiting no file.
`stale': out of date, without unsaved changes: reverting brings it
up to date.
`conflict': with unsaved changes, while the file changed on disk too.
`held': an agent's session transcript out of date with its file; the
runtime holds positions in it, so it is kept as it is.
`gone': without unsaved changes, while its file was deleted on disk."
  (with-current-buffer buffer
    (cond ((or (not buffer-file-name) (verify-visited-file-modtime buffer)) 'current)
          ((buffer-modified-p) 'conflict)
          ((bound-and-true-p lispm-session-mode) 'held)
          ((file-exists-p buffer-file-name) 'stale)
          (t 'gone))))

(defun lispm-tools-sync (buffer)
  "Bring BUFFER up to date with the file it visits, if that changed on disk.
A `stale' buffer is reverted; return its state then, as
`lispm-tools-state' tells it: `current', `conflict', `held' or `gone'."
  (let ((state (lispm-tools-state buffer)))
    (if (not (eq state 'stale))
        state
      (with-current-buffer buffer
        (let ((inhibit-message t))
          (revert-buffer t t t)))
      'current)))

(defun lispm-tools--conflict (buffer)
  "Signal that BUFFER cannot be changed: it is in conflict with its file.
Which to keep is the user's choice, not the model's."
  (error (if (buffer-modified-p buffer)
             "%s changed on disk, and its buffer in Emacs has unsaved changes; \
ask the user which to keep"
           "%s changed on disk, and its buffer is the session of an agent, which \
LispM keeps as it is; ask the user which to keep")
         (abbreviate-file-name (buffer-file-name buffer))))

(defun lispm-tools-find-file (file)
  "Visit FILE, asking nothing; an error if it is too large to.
Emacs asks before it visits a file larger than
`large-file-warning-threshold'; tools refuse instead."
  (let ((size (file-attribute-size (file-attributes file))))
    (when (and size large-file-warning-threshold (> size large-file-warning-threshold))
      (error "%s is too large to open in Emacs (%s; see `large-file-warning-threshold')"
             (abbreviate-file-name file) (file-size-human-readable size)))
    (find-file-noselect file t)))

(defun lispm-tools-visiting (file &optional conflicts)
  "The buffer visiting FILE, up to date with it, or nil.
Nil too if FILE was deleted on disk.  A buffer in conflict with FILE,
or held as it is (see `lispm-tools-state'), is an error, unless
CONFLICTS is non-nil: then it is returned as it is."
  (when-let* ((buffer (find-buffer-visiting file)))
    (pcase (lispm-tools-sync buffer)
      ('current buffer)
      ((or 'conflict 'held) (if conflicts buffer (lispm-tools--conflict buffer))))))

(defun lispm-tools-visit (file &optional conflicts)
  "A buffer visiting FILE, up to date with it; FILE is visited if need be.
A buffer whose file was deleted on disk is emptied, as the buffer of a
new file.  CONFLICTS is as for `lispm-tools-visiting'."
  (or (lispm-tools-visiting file conflicts)
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer))
          (set-buffer-modified-p nil)
          (clear-visited-file-modtime))
        buffer)
      (lispm-tools-find-file file)))

(defun lispm-tools-existing (file)
  "A buffer holding FILE's text, for a change to it; an error if there is none.
That is the buffer visiting FILE, up to date with it, which may hold a
new file's text not saved yet; else FILE, visited now.  A FILE that
cannot be written is an error, before it is visited (see
`lispm-tools-check-writable')."
  (lispm-tools-check-writable file)
  (or (lispm-tools-visiting file)
      (and (file-exists-p file) (lispm-tools-find-file file))
      (error "No such file: %s" (abbreviate-file-name file))))

(defun lispm-tools-to-show (file &optional new)
  "A buffer to show a change to FILE in while it is proposed, or nil.
Nothing is changed for it: the buffer visiting FILE if it is up to
date with it; else, if no buffer visits FILE, FILE visited now, if it
exists or NEW is non-nil."
  (if-let* ((buffer (find-buffer-visiting file)))
      (and (eq (lispm-tools-state buffer) 'current) buffer)
    (and (or new (file-exists-p file))
         (lispm-tools-find-file file))))

(defun lispm-tools-buffer-file-text (buffer)
  "BUFFER's text, or its file's if BUFFER is out of date with it.
See `lispm-file-text'."
  (if-let* ((file (buffer-file-name buffer)))
      (lispm-file-text file)
    (lispm-tools-buffer-text buffer)))

(defvar lispm-tools--observed-revisions (make-hash-table :test #'eq :weakness 'key)
  "Agent -> hash table mapping canonical file path -> last observed SHA-256 hash.")

(defun lispm-tools-content-hash (text)
  "Return the SHA-256 hex digest of UTF-8 encoded TEXT."
  (secure-hash 'sha256 (encode-coding-string (or text "") 'utf-8)))

(defun lispm-tools-record-revision (agent file text)
  "Record for AGENT that FILE was observed holding content TEXT."
  (when (and agent file)
    (let* ((lispm-current-agent agent)
           (canon (lispm-tool--normalize-resource file))
           (tbl (or (gethash agent lispm-tools--observed-revisions)
                    (let ((new (make-hash-table :test #'equal)))
                      (puthash agent new lispm-tools--observed-revisions)
                      new))))
      (puthash canon (lispm-tools-content-hash text) tbl))))

(defun lispm-tools-observed-revision (agent file)
  "Return the last observed SHA-256 hash for AGENT and FILE, or nil."
  (when-let* ((agent)
              (file)
              (tbl (gethash agent lispm-tools--observed-revisions)))
    (let ((lispm-current-agent agent))
      (gethash (lispm-tool--normalize-resource file) tbl))))

(defun lispm-tools--clear-revisions (agent)
  "Drop observed file revisions recorded for AGENT when its session ends."
  (remhash agent lispm-tools--observed-revisions))

(add-hook 'lispm-session-end-functions #'lispm-tools--clear-revisions)

(defun lispm-tools-check-cas (agent file current-text &optional expected-hash check-observed)
  "Verify MVCC/CAS preconditions before AGENT writes FILE with CURRENT-TEXT.
When EXPECTED-HASH is \"missing\", fail if FILE exists.
When EXPECTED-HASH is a SHA-256 hex string, fail unless CURRENT-TEXT matches it.
Otherwise when CHECK-OBSERVED is non-nil and AGENT previously observed FILE,
fail if FILE changed since AGENT last read it."
  (let ((actual (lispm-tools-content-hash current-text)))
    (cond
     ((equal expected-hash "missing")
      (when (file-exists-p file)
        (error "CAS conflict on %s: file already exists (expected missing)"
               (lispm-tools-label file))))
     ((stringp expected-hash)
      (unless (equal expected-hash actual)
        (error "CAS conflict on %s: expected hash %s, current hash is %s; re-read before retrying"
               (lispm-tools-label file) expected-hash actual)))
     (check-observed
      (when-let* ((observed (lispm-tools-observed-revision agent file))
                  ((not (equal observed actual))))
        (error "CAS conflict on %s: file changed since you last read it (observed %s, current %s); re-read before overwriting"
               (lispm-tools-label file) observed actual))))))

(defun lispm-tools--revert-changed (root &optional paths)
  "Revert stale buffers under ROOT (or PATHS when specified).
Respects dynamic `lispm-tools--revert-paths' and `lispm-revert-skip-functions'
so unrelated buffers and buffers holding sibling edit proposals stay untouched."
  (let ((scope (or paths lispm-tools--revert-paths)))
    (unless (eq scope 'none)
      (let* ((root (file-name-as-directory (expand-file-name root)))
             (allowed (when (consp scope)
                        (let ((tbl (make-hash-table :test #'equal)))
                          (dolist (p scope tbl)
                            (when (stringp p)
                              (puthash (lispm-tool--normalize-resource p) t tbl)))))))
        (dolist (buffer (buffer-list))
          (let ((file (buffer-file-name buffer)))
            (when (and file
                       (not (buffer-modified-p buffer))
                       (string-prefix-p root file)
                       (or (not allowed)
                           (gethash (lispm-tool--normalize-resource file) allowed))
                       (or (file-remote-p root) (not (file-remote-p file)))
                       (not (run-hook-with-args-until-success
                             'lispm-revert-skip-functions buffer lispm-current-agent))
                       (eq (lispm-tools-state buffer) 'stale))
              (with-demoted-errors "LispM: reverting a buffer: %S"
                (let ((old (lispm-tools-buffer-text buffer)))
                  (lispm-tools-sync buffer)
                  (let ((new (lispm-tools-buffer-text buffer)))
                    (unless (equal old new)
                      (run-hook-with-args 'lispm-buffer-change-functions buffer old new))))))))))))

(defun lispm-tools-set-text (buffer text)
  "Make BUFFER's text TEXT as one undo step, journaled and saved.
Then tell `lispm-buffer-change-functions'.  A BUFFER in conflict with
its file, or held as it is, is an error (see `lispm-tools-state'), and
so is a read-only one, before anything is journaled."
  (when (memq (lispm-tools-sync buffer) '(conflict held))
    (lispm-tools--conflict buffer))
  (when (and (buffer-local-value 'buffer-read-only buffer) (not inhibit-read-only))
    (error "Cannot change buffer %s: it is read-only" (buffer-name buffer)))
  (let ((old (lispm-tools-buffer-text buffer)))
    (lispm-tools--replace-text buffer text)
    (when (and lispm-current-agent (buffer-file-name buffer))
      (lispm-tools-record-revision lispm-current-agent (buffer-file-name buffer)
                                   (lispm-tools-buffer-text buffer)))
    ;; The change is made: whatever shows it must not undo the call.
    (with-demoted-errors "LispM: showing a change: %S"
      (run-hook-with-args 'lispm-buffer-change-functions buffer old text))))

(defun lispm-tools--replace-text (buffer text)
  "Make BUFFER's text TEXT as one undo step, journaled and saved."
  (with-current-buffer buffer
    (lispm-journal-snapshot buffer)
    (let ((source (generate-new-buffer " *lispm-new-text*" t))
          (keep-final-newline (or (zerop (buffer-size))
                                  (eq (char-before (point-max)) ?\n))))
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
        (kill-buffer source))
      (when (and buffer-file-name lispm-save-edits)
        (make-directory (file-name-directory buffer-file-name) t)
        (let* ((inhibit-message t)
               (require-final-newline (and keep-final-newline require-final-newline))
               (make-backup-files nil)
               (select-safe-coding-system-function nil)
               (base (and buffer-file-coding-system
                          (coding-system-base buffer-file-coding-system)))
               (coding-system-for-write
                (or coding-system-for-write
                    (if (memq base '(nil undecided us-ascii))
                        (coding-system-change-text-conversion
                         (or buffer-file-coding-system 'utf-8-unix) 'utf-8)
                      buffer-file-coding-system))))
          (save-buffer))))))

(defun lispm-tools-buffer (name)
  "The live buffer NAME refers to (see `lispm-tools-buffer-name')."
  (let ((name (lispm-tools-buffer-name name)))
    (or (get-buffer name) (error "No buffer named %s" name))))

(defun lispm-tools-buffer-to-change (name)
  "The live buffer called NAME, up to date with its file if it visits one.
An error if it is in conflict with its file, or its file was deleted,
or cannot be written (see `lispm-tools-check-writable')."
  (let ((buffer (lispm-tools-buffer name)))
    (pcase (lispm-tools-sync buffer)
      ((or 'conflict 'held) (lispm-tools--conflict buffer))
      ('gone (error "%s was deleted on disk; buffer %s holds its old text"
                    (abbreviate-file-name (buffer-file-name buffer)) name))
      (_ (when-let* ((file (buffer-file-name buffer)))
           (lispm-tools-check-writable file))
         buffer))))

(defun lispm-tools-buffer-text (buffer)
  "All of BUFFER's text."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun lispm-tools-check-writable (file)
  "Signal an error if FILE is there and cannot be written; change nothing.
The error names FILE and the cause: it is a directory, or not writable.
Emacs visits a file that is not writable in a read-only buffer, and
asks before saving it once the user made that buffer writable, while
tools never ask.  But with `lispm-save-edits' off, edits are left to
the user to save: a buffer visiting FILE that is not read-only is no
error then."
  (let ((label (lispm-tools-label file)))
    (cond ((file-directory-p file)
           (error "Cannot write %s: it is a directory" label))
          ((or (not (file-exists-p file)) (file-writable-p file)))
          ((and (not lispm-save-edits)
                (when-let* ((buffer (find-buffer-visiting file)))
                  (not (buffer-local-value 'buffer-read-only buffer)))))
          (t (error "Cannot write %s: it is not writable" label)))))


(defun lispm-tools-check-new-file (file)
  "Signal an error if FILE is new and cannot be written; change nothing.
The error names FILE and the cause: a parent is not a directory, or
the directory to write FILE in, or to make that directory in, is not
writable.  A missing directory that can be made is no error."
  (unless (file-exists-p file)
    (let* ((directory (file-name-directory file))
           (parent (directory-file-name directory)))
      ;; The nearest of DIRECTORY and its parents that is there, whatever it is.
      (while (not (or (file-exists-p parent) (file-symlink-p parent)
                      (equal parent (file-name-directory parent))))
        (setq parent (directory-file-name (file-name-directory parent))))
      (let ((label (lispm-tools-label file))
            (where (lispm-tools-label (file-name-as-directory parent))))
        (cond ((not (file-directory-p parent))
               (error "Cannot write %s: %s is not a directory"
                      label (lispm-tools-label parent)))
              ((file-writable-p (file-name-as-directory parent)))
              ((file-directory-p directory)
               (error "Cannot write %s: %s is not writable" label where))
              (t
               (error "Cannot write %s: %s does not exist, and cannot be made: %s is not writable"
                      label (lispm-tools-label directory) where)))))))


(provide 'lispm-tool-core)

;;; lispm-tool-core.el ends here
