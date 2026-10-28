;;; lispm-tools.el --- Tools: Emacs actions an agent can take  -*- lexical-binding: t; -*-

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

;; A tool is an Emacs Lisp function the model may call, declared with
;; `lispm-deftool'.  The declaration gives each tool a permission class
;; (read, memory, comm, write, shell or exec), from which an agent's policy
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
(require 'lispm-sandbox)

;; Only an Emacs built with image support has it.
(declare-function image-size "image.c" (spec &optional pixels frame))

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

(cl-defstruct (lispm-tool (:constructor lispm-tool--create) (:copier nil))
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
`lispm-tool-call-permission')."
  name description params input-schema permission function async preview deferred
  changes targets always-ask file
  ;; Last: packages compiled before it read the others where they were.
  diff for class)

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

Otherwise BODY's value is the result: a string, or any object, which
is printed; or a `lispm-tool-result', text with image or video files
the model sees too.  Signal an error to report a failure to the model."
  (declare (indent 3) (doc-string 3))
  (let ((permission 'read) async preview diff changes targets deferred always-ask for class
        (args (mapcar #'car params)))
    (while (keywordp (car body))
      (pcase (pop body)
        (:permission (setq permission (pop body)))
        (:async (setq async (pop body)))
        (:preview (setq preview (pop body)))
        (:diff (setq diff (pop body)))
        (:changes (setq changes (pop body)))
        (:targets (setq targets (pop body)))
        (:deferred (setq deferred (pop body)))
        (:always-ask (setq always-ask (pop body)))
        (:for (setq for (pop body)))
        (:class (setq class (pop body)))))
    `(lispm-register-tool
      (lispm-tool--create
       :name ,(symbol-name name)
       :description ,docstring
       :params ',params
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
       :file (or load-file-name
                 (and buffer-file-name (string-suffix-p ".el" buffer-file-name) buffer-file-name))
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

(defun lispm-tools-allowed-p (agent name)
  "Non-nil if AGENT's role gives it the tool NAME, offered or deferred.
In a role's list of tools, a name ending with * stands for every
tool whose name starts with the rest, such as mcp__peekaboo__*.  A
role giving the skill tool gives search_tools too, which finds skills.
A tool for some agents only (its :for) is never another's."
  (let ((spec (lispm-agent-tools agent))
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
  (let* ((searchable (lispm-tools--searchable-p))
         tools)
    (maphash (lambda (name tool)
               (when (and (lispm-tools-allowed-p agent name)
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

(cl-defstruct (lispm-tool-result (:constructor lispm-tool-result (text &optional images)))
  "The result of a tool that shows something: TEXT, and IMAGES, a list of
the image and video files the model sees with it, which the transcript
links and the chat shows.  A tool returns one, or hands one to `done'."
  text images)

(defun lispm-tools--result-text (result)
  "RESULT of a tool as text for the model."
  (let ((text (cond ((stringp result) result)
                    ((null result) "OK")
                    (t (let ((print-length 200) (print-level 8))
                         (prin1-to-string result))))))
    (lispm--truncate (if (string-empty-p text) "(no output)" text)
                     lispm-tool-output-limit)))

(defconst lispm-tools--interrupted
  "Interrupted: the user pressed C-g while the call ran.  What it did until then stands."
  "Result of a call the user interrupted.")

(defun lispm-tools--interruptibly (function args)
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

(defun lispm-tools--deliver (done agent tool text errorp images)
  "Give DONE the result of AGENT's call of TOOL: TEXT, ERRORP and IMAGES.
First, if TOOL may have changed files, TRAMP forgets what it knew of
those of a remote root (see `lispm-tools--flush-remote'), and the
buffers of those under AGENT's root that changed on disk are reverted,
so that the user sees what the call did there (see
`lispm-revert-after-calls'); C-g stops that.  DONE goes on with the
turn: it records the result and may ask the model again, which C-g
must not leave half done, so quitting is inhibited meanwhile."
  (let ((inhibit-quit t))
    (when (and agent (not (memq (lispm-tool-permission tool) lispm-tools--still-permissions)))
      (with-demoted-errors "LispM: forgetting remote file attributes: %S"
        (lispm-tools--flush-remote (lispm-agent-root agent)))
      (when lispm-revert-after-calls
        ;; Hooks the reverts run are the agent's doing.
        (let ((lispm-current-agent agent))
          (condition-case nil
              (lispm-tools--interruptibly #'lispm-tools--revert-changed
                                          (list (lispm-agent-root agent)))
            (quit nil)))))
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
eval_elisp say, and the call fails (see `lispm-tools--interruptibly').
What an asynchronous tool waits for afterwards, a process or another
agent, is stopped by aborting the turn.  Only the tool is
interruptible: what comes after it, the result and the rest of the
turn, is not, so C-g never leaves it half done or reports a call that
ran to its end as interrupted.  DONE is never called while the tool
runs."
  (let* ((inhibit-quit t)
         (agent lispm-current-agent)
         (running t)
         (called nil)
         (result nil)
         (finish (lambda (value &optional errorp images)
                   (unless called
                     (when (lispm-tool-result-p value)
                       (setq images (append images (lispm-tool-result-images value))
                             value (lispm-tool-result-text value)))
                     (let ((args (list (lispm-tools--result-text value) errorp images)))
                       (setq called t)
                       (if running
                           (setq result args)
                         (apply #'lispm-tools--deliver done agent tool args)))))))
    (condition-case err
        ;; What the tool says is for the model: in English, though it
        ;; reach a function that speaks to the user too.
        (lispm-in-english
          (let ((args (lispm-tools--arguments tool input)))
            (if (lispm-tool-async tool)
                (lispm-tools--interruptibly (lispm-tool-function tool) (cons finish args))
              (funcall finish (lispm-tools--interruptibly (lispm-tool-function tool) args)))))
      (error (funcall finish (error-message-string err) t))
      (quit (funcall finish lispm-tools--interrupted t)))
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
               (pcase-let ((`(,old ,new ,label) (apply diff (lispm-tools--arguments tool input))))
                 (if (not callback)
                     (setq text (lispm--diff old new label))
                   (lispm--diff old new label
                                (lambda (made &optional why)
                                  (funcall callback (or made (format "(no preview: %s)" why)))))
                   (setq later t))))
              (preview (setq text (apply preview (lispm-tools--arguments tool input)))))
      (error (setq text (format "(no preview: %s)" (error-message-string err)))))
    (cond (later nil)
          (callback (ignore (funcall callback text)))
          (t text))))

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
;;
;; The search is fuzzy (`lispm-tools-search-score'): a word may be
;; part of one it finds, or misspelt.  It finds what
;; `lispm-tool-search-functions' offer too, as the skills, the manuals
;; of the parts and apps of the system: a tool of a skill comes with
;; its skill, which the search names, so the model reads how to use it.
;;
;; A tool an agent defines itself, with eval_elisp or the code of a
;; skill it loads, is deferred too, and loaded for that agent at once:
;; it is the agent's from its next step on, other agents find it with
;; search_tools, and it joins no one's core layer (`lispm-tools--adopt').

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
  "Non-nil if there are deferred tools, sources of them, or more to find."
  (or (lispm-tools--sources)
      lispm-tool-search-functions
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

;;;;; Fuzzy search

(defconst lispm-tools--search-stop-words
  '("a" "an" "and" "any" "as" "at" "be" "by" "can" "do" "for" "from" "how" "i" "in" "into" "is"
    "it" "me" "my" "of" "on" "or" "some" "the" "to" "use" "with")
  "Words of a search that say nothing of what it looks for.")

(defun lispm-tools-search-words (query)
  "The words of the search QUERY, lowercase.
It is split at all but letters, digits, _ and -.  Words saying
nothing of what is looked for, such as \"the\", and single letters
are left out, unless nothing else is left."
  (let* ((words (split-string (downcase query) "[^[:alnum:]_-]+" t))
         (telling (seq-remove (lambda (word) (or (< (length word) 2)
                                                 (member word lispm-tools--search-stop-words)))
                              words)))
    (seq-uniq (or telling words))))

(defun lispm-tools--search-tokens (text)
  "The words of TEXT a search matches against, lowercase.
Words holding _ or - are taken apart too."
  (let ((words (split-string (downcase text) "[^[:alnum:]_-]+" t)))
    (seq-uniq (append words (mapcan (lambda (word)
                                      (and (string-match-p "[_-]" word)
                                           (split-string word "[_-]+" t)))
                                    words)))))

(defun lispm-tools--word-match (word token)
  "How well WORD of a search matches TOKEN, a word of what it looks at: 0 to 1."
  (let ((lw (length word))
        (lt (length token)))
    (cond ((equal word token) 1.0)
          ((and (>= lw 3) (string-prefix-p word token)) 0.8)
          ((and (>= lt 4) (string-prefix-p token word)) 0.7)
          ((and (>= lw 4) (string-search word token)) 0.5)
          ;; A misspelling: the same first letter, a word long enough that
          ;; one letter off is no other word, as "chat" is "what".
          ((and (>= lw 5) (>= lt 5) (eq (aref word 0) (aref token 0))
                (let ((most (if (>= lw 8) 2 1)))
                  (and (<= (abs (- lw lt)) most) (<= (string-distance word token) most))))
           0.4)
          (t 0))))

(defun lispm-tools-search-score (words fields)
  "How well the search WORDS match FIELDS, a list of (WEIGHT . TEXT).
0 if not at all.  Each word counts with its best match: a field's
WEIGHT times how well the word matches one of the field's words, 1
when they are the same, less when one begins the other, holds it,
or is it misspelt by a letter or two.  The sum is scaled by the
share of WORDS matching at all, so that what matches them all comes
first.  WORDS are as `lispm-tools-search-words' makes them; a TEXT
may be nil."
  (let ((fields (delq nil (mapcar (lambda (field)
                                    (and (stringp (cdr field))
                                         (cons (car field) (lispm-tools--search-tokens (cdr field)))))
                                  fields)))
        (sum 0)
        (matched 0))
    (dolist (word words)
      (let ((best 0))
        (pcase-dolist (`(,weight . ,tokens) fields)
          (dolist (token tokens)
            (let ((score (* weight (lispm-tools--word-match word token))))
              (when (> score best)
                (setq best score)))))
        (when (> best 0)
          (cl-incf matched)
          (cl-incf sum best))))
    (if (zerop matched) 0 (/ (* sum matched) (float (length words))))))

(defun lispm-tools--fields (tool)
  "What a search finds TOOL by, as `lispm-tools-search-score' takes it."
  `((3 . ,(lispm-tool-name tool)) (1.5 . ,(lispm-tools--description tool))))

(defconst lispm-tools--search-cutoff 0.3
  "The least share of the best score a thing a search finds must score.
What scores less matches by chance, as a word found inside another
does, and would cost a request room for nothing.")

(defun lispm-tools--rank (words candidates limit fields)
  "The CANDIDATES the search WORDS match best, at most LIMIT, best first.
FIELDS is a function of a candidate, what it is found by.  Those
scoring less than `lispm-tools--search-cutoff' of the best are left
out.  With no WORDS, the first LIMIT of the candidates; equal ones
keep their order."
  (if (null words)
      (seq-take candidates limit)
    (let* ((scored (cl-stable-sort
                    (seq-filter (lambda (pair) (> (car pair) 0))
                                (mapcar (lambda (candidate)
                                          (cons (lispm-tools-search-score words (funcall fields candidate))
                                                candidate))
                                        candidates))
                    #'> :key #'car))
           (least (* lispm-tools--search-cutoff (or (caar scored) 0))))
      (mapcar #'cdr (seq-take (seq-take-while (lambda (pair) (>= (car pair) least)) scored) limit)))))

(defun lispm-tools--named-sources (query sources)
  "The SOURCES a word of QUERY names, and QUERY's other words: (NAMED . WORDS)."
  (let* ((words (split-string (downcase query) "[^[:alnum:]_-]+" t))
         (named (seq-filter (lambda (source) (member (downcase (plist-get source :name)) words))
                            sources)))
    (cons named
          (lispm-tools-search-words
           (string-join (seq-difference words (mapcar (lambda (source) (downcase (plist-get source :name)))
                                                      named))
                        " ")))))

(defun lispm-tools--of-sources (tools sources)
  "Those of TOOLS one of SOURCES provides."
  (seq-filter (lambda (tool)
                (seq-some (lambda (source) (funcall (plist-get source :provides) (lispm-tool-name tool)))
                          sources))
              tools))

(defun lispm-tools--find (query tools limit &optional sources)
  "The TOOLS QUERY asks for, at most LIMIT of them.
\"select:A,B\" asks for those tools by name.  Otherwise a word of
QUERY naming one of SOURCES (as `lispm-tool-sources' describes them)
keeps that source's tools only, and the other words rank them, as
`lispm-tools-search-score' does; with no other word, all the source's
tools are found."
  (if (string-prefix-p "select:" query)
      (let ((names (split-string (substring query 7) "[, ]+" t)))
        (seq-filter (lambda (tool) (member (lispm-tool-name tool) names)) tools))
    (pcase-let ((`(,named . ,words) (lispm-tools--named-sources query sources)))
      (lispm-tools--rank words (if named (lispm-tools--of-sources tools named) tools)
                         limit #'lispm-tools--fields))))

(defun lispm-tools-load (agent names)
  "Offer AGENT the deferred tools NAMES from its next request on."
  (let ((new (seq-remove (lambda (name) (member name (lispm-agent-loaded-tools agent))) names)))
    (when new
      (setf (lispm-agent-loaded-tools agent) (append (lispm-agent-loaded-tools agent) new))
      (run-hook-with-args 'lispm-tools-changed-functions agent))))

(defun lispm-tools--snapshot ()
  "The tools registered now, for `lispm-tools--adopt' to tell those defined since."
  (copy-hash-table lispm-tools--registry))

(defun lispm-tools--adopt (agent before)
  "Make the tools defined since BEFORE deferred tools of AGENT.
BEFORE is the registry as `lispm-tools--snapshot' took it.  A tool
new since then is deferred, so that it is offered to no other agent
until search_tools finds it; one defined again keeps its place,
deferred or in the core layer, unless it is defined as deferred now.
The deferred ones are loaded for AGENT, if non-nil, from its next
request on.  Return their names, sorted."
  (let (names)
    (maphash (lambda (name tool)
               (let ((old (gethash name before)))
                 (unless (eq tool old)
                   (unless (lispm-tool-deferred tool)
                     (setf (lispm-tool-deferred tool) (or (null old) (lispm-tool-deferred old))))
                   (when (lispm-tool-deferred tool)
                     (push name names)))))
             lispm-tools--registry)
    (setq names (sort names #'string<))
    (when agent
      (lispm-tools-load agent names))
    names))

(defconst lispm-tools--loaded-prefix "Tools now available: "
  "How a search_tools result starts the list of the tools it loaded.")

(defun lispm-tools--discover (agent query limit)
  "What AGENT's search QUERY finds, at most LIMIT things: (TOOLS . OTHERS).
TOOLS are deferred tools AGENT's role gives it and it has not loaded,
to load; OTHERS are plists of `lispm-tool-search-functions', such as
skills.  A tool one of those holds is not among TOOLS: it comes with
what holds it, found in its stead.  \"select:A,B\" finds the tools A
and B, or what holds them; a word naming a source of tools (see
`lispm-tool-sources') keeps that source's tools only."
  (let* ((others (let ((lispm-current-agent agent))
                   (mapcan (lambda (function) (copy-sequence (funcall function agent)))
                           lispm-tool-search-functions)))
         (held (mapcan (lambda (other) (copy-sequence (plist-get other :tools))) others))
         (tools (seq-remove (lambda (tool)
                              (let ((name (lispm-tool-name tool)))
                                (or (member name (lispm-agent-loaded-tools agent))
                                    (member name held)
                                    (not (lispm-tools-allowed-p agent name)))))
                            (lispm-tools--deferred))))
    (if (string-prefix-p "select:" query)
        (let ((names (split-string (substring query 7) "[, ]+" t)))
          (cons (lispm-tools--find query tools limit)
                (seq-filter (lambda (other) (seq-intersection names (plist-get other :tools))) others)))
      (pcase-let ((`(,named . ,words) (lispm-tools--named-sources query (lispm-tools--sources))))
        (if named
            (cons (lispm-tools--rank words (lispm-tools--of-sources tools named) limit #'lispm-tools--fields)
                  nil)
          (let ((found (lispm-tools--rank words
                                          (append (mapcar (lambda (tool) (cons 'tool tool)) tools)
                                                  (mapcar (lambda (other) (cons 'other other)) others))
                                          limit
                                          (lambda (candidate)
                                            (if (eq (car candidate) 'tool)
                                                (lispm-tools--fields (cdr candidate))
                                              (plist-get (cdr candidate) :fields))))))
            (cons (mapcar #'cdr (seq-filter (lambda (candidate) (eq (car candidate) 'tool)) found))
                  (mapcar #'cdr (seq-filter (lambda (candidate) (eq (car candidate) 'other)) found)))))))))

(lispm-register-tool
 (lispm-tool--create
  :name "search_tools"
  :description
  (lambda ()
    (concat "Find what you can do that you do not have yet: the skills of this system and its "
            "apps, their manuals, each with its tools, which you load with skill; and tools. "
            "Search whenever a request needs something you lack, and before you say you cannot "
            "do something. Search by keywords (\"play music\", \"screenshot click\"): a word may "
            "be part of a longer one, or misspelt. Tools found are yours from your next step on, "
            "for the rest of the conversation; a skill's tools come when you load the skill. "
            "A source's name keeps its tools; \"select:NAME,NAME\" asks for tools by name.\n"
            "Sources of tools:\n"
            (mapconcat (lambda (source)
                         (format "- %s: %s" (plist-get source :name) (plist-get source :description)))
                       (lispm-tools--sources) "\n")))
  :input-schema
  '((type . "object")
    (properties (query (type . "string")
                       (description . "Keywords, a source's name, or select:NAME,NAME."))
                (max_results (type . "integer")
                             (description . "How many skills and tools to find for keywords (default 5).")))
    (required . ["query"]))
  :permission 'read
  :async t
  :function
  (lambda (done input)
    (let ((agent lispm-current-agent)
          (query (alist-get 'query input))
          (limit (alist-get 'max_results input)))
      (unless (stringp query) (error "Say what to search for"))
      (lispm-tools--prepare
       (seq-filter (lambda (source) (plist-get source :pending)) (lispm-tools--sources))
       (lambda (problems)
         (pcase-let ((`(,found . ,others)
                      (lispm-tools--discover agent query
                                             (if (integerp limit) limit lispm-tool-search-results))))
           (lispm-tools-load agent (mapcar #'lispm-tool-name found))
           (funcall done
                    (concat (if (or found others)
                                (string-join
                                 (delq nil
                                       (list (and found
                                                  (concat lispm-tools--loaded-prefix
                                                          (mapconcat #'lispm-tool-name found ", ") ".\n"
                                                          (mapconcat (lambda (tool)
                                                                       (format "- %s: %s" (lispm-tool-name tool)
                                                                               (lispm-tools--description tool)))
                                                                     found "\n")))
                                             (and others
                                                  (mapconcat (lambda (other) (plist-get other :line))
                                                             others "\n"))))
                                 "\n")
                              (format "Nothing matches %S; you have loaded: %s." query
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
             (message "LispM: %s" (string-join problems (lispm-tr-in "between the parts of a list" "; "))))))))))

(add-hook 'lispm-session-mode-hook #'lispm-tools--restore)

;;;; Permissions

(defun lispm-tools--in-root-p (agent file)
  "Non-nil if FILE lies under AGENT's root directory."
  (file-in-directory-p (expand-file-name file (lispm-agent-root agent))
                       (lispm-agent-root agent)))

(defvar tramp-local-host-regexp)

(defun lispm-tools-elsewhere-p (directory)
  "Non-nil if DIRECTORY is on another machine than Emacs.
That is a remote DIRECTORY (a TRAMP file name, such as /ssh:vm:/work/
or /docker:box:/work/) whose host is not this one: not one of the
names of the local host TRAMP knows (`tramp-local-host-regexp'), as
those of /sudo::/ and /su::/ are.  A shell command there cannot reach
Emacs, unless that machine reaches this one's files, as a container
does that mounts the user's home directory.  Without a host name, or
with no names of the local host to compare with, it is taken to be
this machine."
  (when-let* ((host (file-remote-p directory 'host)))
    (require 'tramp)
    (and (stringp tramp-local-host-regexp)
         (not (string-empty-p host))
         (not (string-match-p tramp-local-host-regexp host)))))

(defun lispm-tools-apart-p (agent)
  "Non-nil if AGENT's shell commands cannot reach Emacs.
They run on another machine (see `lispm-tools-elsewhere-p'), or on this
one in the sandbox, which keeps Emacs's server socket and configuration
from them (see `lispm-sandbox-confines-p').  Such commands cannot change
the agent's policy, so the user may allow every later one at once."
  (when agent
    (let ((root (lispm-agent-root agent)))
      (if (file-remote-p root)
          (lispm-tools-elsewhere-p root)
        (lispm-sandbox-confines-p)))))

(defun lispm-tools--call-key (input)
  "INPUT as an approval for this exact call remembers it: without `why'."
  (if (listp input)
      (seq-remove (lambda (entry) (eq (car-safe entry) 'why)) input)
    input))

(defun lispm-tools-allowance (tool input &optional agent)
  "What approving AGENT's call of TOOL on INPUT for good adds to its allowed.
The tool's name: every later call of it runs at once.  For a tool of
class `exec', which reaches all of Emacs, this exact call: (NAME .
INPUT), INPUT without `why'.  Allowing every later call would put the
agent past its policy, which such a call can change.  So for a tool of
class `shell', since a shell command reaches Emacs too, unless AGENT's
commands are kept apart from Emacs (see `lispm-tools-apart-p'): on
another machine, or in the sandbox, they cannot change the policy, and
the tool is allowed.  Approving one call of a tool that always asks is
never approving the next."
  (if (or (lispm-tool-always-ask tool)
          (pcase (lispm-tool-permission tool)
            ('exec t)
            ('shell (not (lispm-tools-apart-p agent)))))
      (cons (lispm-tool-name tool) (lispm-tools--call-key input))
    (lispm-tool-name tool)))

(defun lispm-tools--allowed-p (agent tool input)
  "Non-nil if the user allowed AGENT this call of TOOL on INPUT for good.
A tool of class `shell' allowed as a whole was allowed while AGENT's
commands could not reach Emacs (see `lispm-tools-allowance'), and
holds only while that is so: with the sandbox off again, each command
asks, as it did before."
  (let ((allowed (lispm-agent-allowed agent))
        (name (lispm-tool-name tool)))
    (or (and (member name allowed)
             (or (not (eq (lispm-tool-permission tool) 'shell))
                 (lispm-tools-apart-p agent)))
        (member (cons name (lispm-tools--call-key input)) allowed))))

(defun lispm-tools--targets (agent tool input)
  "Files AGENT's call of TOOL with INPUT changes, as far as can be told before.
Those TOOL's targets function returns, if it has one (see
`lispm-deftool'); else those a `path' argument names, and the file of
the buffer a `buffer' argument names, either of them a reference (see
`lispm-tools-reference').  t if the targets function fails: the call may
change any file."
  (if-let* ((targets (lispm-tool-targets tool)))
      (condition-case nil
          (let ((lispm-current-agent agent)
                (default-directory (lispm-agent-root agent)))
            (apply targets (lispm-tools--arguments tool input)))
        (error t))
    (when (listp input)
      (let ((path (alist-get 'path input))
            (buffer (and (stringp (alist-get 'buffer input))
                         (get-buffer (lispm-tools-buffer-name (alist-get 'buffer input))))))
        ;; The file a path names is found as the tool finds it.
        (condition-case nil
            (delq nil (list (and (stringp path)
                                 (let ((lispm-current-agent agent))
                                   (lispm-tools--file path)))
                            (and buffer (buffer-file-name buffer))))
          (error t))))))

(defvar lispm-tool-action-functions nil
  "Functions that may hold a tool call back for approval, or refuse it.
Each is called with the agent, the tool (a `lispm-tool') and the input
of a call LispM's own rules do not deny (see `lispm-tool-action'), and
returns nil, to leave the call as those rules decide it; `ask', for it
to await the user's approval; or `deny', to refuse it.  Either may
come with a reason, as (ask . REASON) or (deny . REASON), REASON a
sentence: the approval shows it, and a denial tells it the model.

The strictest answer wins, so a function can make a call ask or be
refused, never let one through that would ask or be refused: what
the user allowed for good is asked about again while a function asks.
A function that signals an error, or answers anything else, asks:
an error never lets a call through.  Functions are called for every
call before it is decided, and again whenever it is decided anew, so
they should be quick and answer alike for the same call.")

(defun lispm-tools--answer (function agent tool input)
  "What FUNCTION of `lispm-tool-action-functions' answers for AGENT's call
of TOOL on INPUT: nil, or (ACTION . REASON).  An answer without a reason
gets one naming FUNCTION."
  (let ((name (if (symbolp function)
                  (format "`%s'" function)
                "A function of `lispm-tool-action-functions'")))
    (condition-case err
        (pcase (funcall function agent tool input)
          ('nil nil)
          ((or (and (or 'ask 'deny) action (let reason nil))
               (and `(,(and (or 'ask 'deny) action) . ,reason)
                    (guard (or (null reason) (stringp reason)))))
           (cons action (or reason (format (if (eq action 'ask) "%s asks about it." "%s refused it.")
                                           name))))
          (answer (cons 'ask (format "%s answered %S, which is neither ask nor deny."
                                     name answer))))
      (error (cons 'ask (format "%s failed: %s." name (error-message-string err)))))))

(defun lispm-tool-decision (agent tool input)
  "Whether AGENT may run TOOL on INPUT, and why not: (ACTION . REASONS).
ACTION is `auto', `ask' or `deny', what LispM's own rules decide (see
`lispm-tool-action') or, if stricter, what a function of
`lispm-tool-action-functions' answers.  REASONS are the reasons of the
functions answering ACTION, a list of strings; LispM's own rules give
none."
  (let ((action (lispm-tools--action agent tool input))
        answers)
    (unless (eq action 'deny)
      (run-hook-wrapped 'lispm-tool-action-functions
                        (lambda (function)
                          (when-let* ((answer (lispm-tools--answer function agent tool input)))
                            (push answer answers)
                            (setq action (lispm--stricter action (car answer))))
                          nil)))
    (cons action (delq nil (mapcar (lambda (answer) (and (eq (car answer) action) (cdr answer)))
                                   (nreverse answers))))))

(defun lispm-tools-denial (agent tool reasons)
  "What the model is told of AGENT's call of TOOL, denied for REASONS.
REASONS as `lispm-tool-decision' gives them; with none, the policy
denied it."
  (if reasons
      (concat "Denied: " (string-join reasons " "))
    (format "Denied by policy: %s may not use %s tools."
            (lispm-agent-name agent) (lispm-tool-permission tool))))

(defun lispm-tool-action (agent tool input)
  "Whether AGENT may run TOOL on INPUT: `auto', `ask' or `deny'.
The policy decides by the class of the call, the tool's unless the
call does less (see `lispm-tool-call-permission').  Calls the user
allowed for this Emacs session run at once (see
`lispm-tools-allowance').  Writes outside the agent's root always ask,
whatever the policy says: to files named by a `path' argument, visited
by a buffer a `buffer' argument names, or that the tool's targets
function returns (see `lispm-deftool'), as a write whose files that
function cannot tell does.  So do the calls of a tool that always asks
(see `lispm-deftool'), such as write_skill.  Then the functions of
`lispm-tool-action-functions' may have a call ask, or deny it; see
`lispm-tool-decision' for why."
  (car (lispm-tool-decision agent tool input)))

(defun lispm-tool-call-permission (tool input)
  "The class of TOOL's call on INPUT: the tool's, unless the call does less.
A tool with a class function (`:class' in `lispm-deftool') has it say
which class the call belongs to, as run_shell says `read' of a command
that only reads (see `lispm-shell-read-only-p'); the policy then
decides the call as it decides that class.  A function that answers
nil, or fails, leaves the call in the tool's class."
  (or (when-let* ((class (lispm-tool-class tool)))
        (condition-case nil
            (apply class (lispm-tools--arguments tool input))
          (error nil)))
      (lispm-tool-permission tool)))

(defun lispm-tools--action (agent tool input)
  "What LispM's own rules decide of AGENT's call of TOOL on INPUT.
See `lispm-tool-action'."
  (let ((action (lispm-policy-action agent (lispm-tool-call-permission tool input))))
    (cond ((lispm-tools--allowed-p agent tool input) 'auto)
          ((and (eq action 'auto) (lispm-tool-always-ask tool)) 'ask)
          ((and (eq action 'auto)
                (eq (lispm-tool-permission tool) 'write)
                (let ((targets (lispm-tools--targets agent tool input)))
                  (or (eq targets t)
                      (seq-some (lambda (file) (not (lispm-tools--in-root-p agent file)))
                                targets))))
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

(defun lispm-tools--file (path)
  "PATH expanded against the acting agent's root.
PATH may be a reference to a file (see `lispm-tools-reference').  On a
remote root, an absolute PATH that names no machine names a file on the
root's (see `lispm--expand-file-name'): the agent works there."
  (lispm--expand-file-name (substitute-in-file-name (car (lispm-tools-reference path)))
                           (lispm-tools--root)))

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
;; go by `lispm--file-text'.  After a call that may have changed files,
;; the buffers of those under the agent's root that changed on disk are
;; reverted, so the user sees what it did (`lispm-revert-after-calls').

(defun lispm-tools--state (buffer)
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

(defun lispm-tools--sync (buffer)
  "Bring BUFFER up to date with the file it visits, if that changed on disk.
A `stale' buffer is reverted; return its state then, as
`lispm-tools--state' tells it: `current', `conflict', `held' or `gone'."
  (let ((state (lispm-tools--state buffer)))
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

(defun lispm-tools--find-file (file)
  "Visit FILE, asking nothing; an error if it is too large to.
Emacs asks before it visits a file larger than
`large-file-warning-threshold'; tools refuse instead."
  (let ((size (file-attribute-size (file-attributes file))))
    (when (and size large-file-warning-threshold (> size large-file-warning-threshold))
      (error "%s is too large to open in Emacs (%s; see `large-file-warning-threshold')"
             (abbreviate-file-name file) (file-size-human-readable size)))
    (find-file-noselect file t)))

(defun lispm-tools--visiting (file &optional conflicts)
  "The buffer visiting FILE, up to date with it, or nil.
Nil too if FILE was deleted on disk.  A buffer in conflict with FILE,
or held as it is (see `lispm-tools--state'), is an error, unless
CONFLICTS is non-nil: then it is returned as it is."
  (when-let* ((buffer (find-buffer-visiting file)))
    (pcase (lispm-tools--sync buffer)
      ('current buffer)
      ((or 'conflict 'held) (if conflicts buffer (lispm-tools--conflict buffer))))))

(defun lispm-tools--visit (file &optional conflicts)
  "A buffer visiting FILE, up to date with it; FILE is visited if need be.
A buffer whose file was deleted on disk is emptied, as the buffer of a
new file.  CONFLICTS is as for `lispm-tools--visiting'."
  (or (lispm-tools--visiting file conflicts)
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer))
          (set-buffer-modified-p nil)
          (clear-visited-file-modtime))
        buffer)
      (lispm-tools--find-file file)))

(defun lispm-tools--existing (file)
  "A buffer holding FILE's text, for a change to it; an error if there is none.
That is the buffer visiting FILE, up to date with it, which may hold a
new file's text not saved yet; else FILE, visited now.  A FILE that
cannot be written is an error, before it is visited (see
`lispm-tools--check-writable')."
  (lispm-tools--check-writable file)
  (or (lispm-tools--visiting file)
      (and (file-exists-p file) (lispm-tools--find-file file))
      (error "No such file: %s" (abbreviate-file-name file))))

(defun lispm-tools--to-show (file &optional new)
  "A buffer to show a change to FILE in while it is proposed, or nil.
Nothing is changed for it: the buffer visiting FILE if it is up to
date with it; else, if no buffer visits FILE, FILE visited now, if it
exists or NEW is non-nil."
  (if-let* ((buffer (find-buffer-visiting file)))
      (and (eq (lispm-tools--state buffer) 'current) buffer)
    (and (or new (file-exists-p file))
         (lispm-tools--find-file file))))

(defun lispm-tools--buffer-file-text (buffer)
  "BUFFER's text, or its file's if BUFFER is out of date with it.
See `lispm--file-text'."
  (if-let* ((file (buffer-file-name buffer)))
      (lispm--file-text file)
    (lispm-tools--buffer-text buffer)))

(defun lispm-tools--revert-changed (root)
  "Revert the stale buffers of files under ROOT (see `lispm-tools--state').
Run after a call that may have changed files (a shell command, Lisp,
an MCP server), so that the buffers the user and the agents see hold
what is on disk; see `lispm-revert-after-calls'.  Remote files are
left out, checking them would mean a connection each, unless ROOT is
remote: then those under it are checked through its connection."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
        (when (and file
                   (not (buffer-modified-p buffer))
                   (string-prefix-p root file)
                   (or (file-remote-p root) (not (file-remote-p file)))
                   (eq (lispm-tools--state buffer) 'stale))
          (with-demoted-errors "LispM: reverting a buffer: %S"
            (lispm-tools--sync buffer)))))))

(defun lispm-tools--set-text (buffer text)
  "Make BUFFER's text TEXT as one undo step, journaled and saved.
Then tell `lispm-buffer-change-functions'.  A BUFFER in conflict with
its file, or held as it is, is an error (see `lispm-tools--state'), and
so is a read-only one, before anything is journaled."
  (when (memq (lispm-tools--sync buffer) '(conflict held))
    (lispm-tools--conflict buffer))
  (when (and (buffer-local-value 'buffer-read-only buffer) (not inhibit-read-only))
    (error "Cannot change buffer %s: it is read-only" (buffer-name buffer)))
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
  "The live buffer NAME refers to (see `lispm-tools-buffer-name')."
  (let ((name (lispm-tools-buffer-name name)))
    (or (get-buffer name) (error "No buffer named %s" name))))

(defun lispm-tools--buffer-to-change (name)
  "The live buffer called NAME, up to date with its file if it visits one.
An error if it is in conflict with its file, or its file was deleted,
or cannot be written (see `lispm-tools--check-writable')."
  (let ((buffer (lispm-tools--buffer name)))
    (pcase (lispm-tools--sync buffer)
      ((or 'conflict 'held) (lispm-tools--conflict buffer))
      ('gone (error "%s was deleted on disk; buffer %s holds its old text"
                    (abbreviate-file-name (buffer-file-name buffer)) name))
      (_ (when-let* ((file (buffer-file-name buffer)))
           (lispm-tools--check-writable file))
         buffer))))

(defun lispm-tools--buffer-text (buffer)
  "All of BUFFER's text."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Built-in tools: reading

(defun lispm-tools--media (file)
  "The result of reading FILE, an image or a video: said in a line, and shown."
  (lispm-tool-result
   (if (eq (lispm-media-kind file) 'video)
       (format "[video %s]" (lispm-video-description file))
     (format "[image %s%s]" (file-name-nondirectory file)
             (if-let* (((fboundp 'image-size))
                       ((display-images-p))
                       (size (ignore-errors (image-size (create-image file) t))))
                 (format ", %d×%d" (car size) (cdr size))
               "")))
   (list file)))

(lispm-deftool read_file
    ((path string "File path, absolute or relative to your root directory, or a place [[file:PATH::LINE]].")
     (offset integer "First line to return, 1-based (default: the place's line, else 1)." :optional t)
     (limit integer "Maximum number of lines (default 2000)." :optional t))
  "Read a file: a text file's lines come prefixed with their numbers;
an image (png, jpg, gif, webp) you see; a video (mp4, mov, webm, mkv)
you see as frames taken across it.  If a text file is open in Emacs,
you get the buffer's current contents, unsaved edits included: what
the user sees.  Other agents name places as org links,
[[file:PATH::LINE]]: pass one as the path to read there."
  :permission read
  (let* ((file (lispm-tools--file path))
         (offset (or offset (cdr (lispm-tools-reference path))))
         (open (find-buffer-visiting file))
         (state (and open (lispm-tools--sync open))))
    (cond ((and (lispm-media-kind file) (file-readable-p file))
           (lispm-tools--media file))
          ((memq state '(current conflict held))
           ;; Out of step with its file, the buffer is what the user
           ;; sees, or what the runtime holds: say so.
           (concat (pcase state
                     ('conflict "[open in Emacs, unsaved changes; the file changed on disk too]\n")
                     ('held "[the session of an agent, as LispM holds it; the file changed on disk]\n")
                     (_ (if (buffer-modified-p open) "[open in Emacs, unsaved changes]\n" "")))
                   (lispm-tools--number-lines (lispm-tools--buffer-text open) offset limit)))
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
         ;; With their attributes: on a remote directory, one round trip
         ;; rather than one per entry.
         (entries (directory-files-and-attributes dir nil directory-files-no-dot-files-regexp)))
    (concat (abbreviate-file-name dir) "\n"
            (mapconcat (lambda (entry)
                         (let ((type (file-attribute-type (cdr entry))))
                           (if (or (eq type t)
                                   ;; A symbolic link to a directory is one.
                                   (and (stringp type) (file-directory-p (expand-file-name (car entry) dir))))
                               (concat (car entry) "/")
                             (car entry))))
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
             ;; A buffer is searched for its unsaved changes, unless it
             ;; is out of date: then the file is.
             (if (and (buffer-live-p open) (lispm--buffer-current-p open))
                 (with-current-buffer open
                   (let ((case-fold-search case-fold))
                     (lispm-tools--matches regexp (label file) limit)))
               (with-current-buffer buffer
                 (let* ((size (or (file-attribute-size (file-attributes file)) 0))
                        (small (<= size lispm-search-max-file-size))
                        ;; Read files as they are, no decompression or
                        ;; decryption, and as UTF-8: detecting each file's
                        ;; coding system would double the time.  A remote
                        ;; file is read through TRAMP's handler still.
                        (file-name-handler-alist (and (file-remote-p file) file-name-handler-alist))
                        (inhibit-file-name-handlers (append '(jka-compr-handler epa-file-handler)
                                                            inhibit-file-name-handlers))
                        (inhibit-file-name-operation 'insert-file-contents)
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

(defun lispm-tools--check-writable (file)
  "Signal an error if FILE is there and cannot be written; change nothing.
The error names FILE and the cause: it is a directory, or not writable.
Emacs visits a file that is not writable in a read-only buffer, and
asks before saving it once the user made that buffer writable, while
tools never ask.  But with `lispm-save-edits' off, edits are left to
the user to save: a buffer visiting FILE that is not read-only is no
error then."
  (let ((label (lispm-tools--label file)))
    (cond ((file-directory-p file)
           (error "Cannot write %s: it is a directory" label))
          ((or (not (file-exists-p file)) (file-writable-p file)))
          ((and (not lispm-save-edits)
                (when-let* ((buffer (find-buffer-visiting file)))
                  (not (buffer-local-value 'buffer-read-only buffer)))))
          (t (error "Cannot write %s: it is not writable" label)))))

(defun lispm-tools--edit-diff (path old-text new-text &optional replace-all)
  "The file at PATH as it is and as edit_file would leave it, and its label.
OLD-TEXT, NEW-TEXT and REPLACE-ALL are edit_file's arguments."
  (let* ((file (lispm-tools--file path))
         (old (lispm--file-text file)))
    (list old (car (lispm-tools--replace old old-text new-text replace-all))
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
  :diff #'lispm-tools--edit-diff
  :changes (lambda (path old-text new-text &optional replace-all)
             (let ((file (lispm-tools--file path)))
               ;; No buffer is visited to show a file that cannot be written.
               (lispm-tools--check-writable file)
               (when-let* ((buffer (lispm-tools--to-show file)))
                 (cons buffer (car (lispm-tools--replace (lispm-tools--buffer-text buffer)
                                                         old-text new-text replace-all))))))
  (let* ((file (lispm-tools--file path))
         (buffer (lispm-tools--existing file))
         (edit (lispm-tools--replace (lispm-tools--buffer-text buffer)
                                     old-text new-text replace-all)))
    (lispm-tools--set-text buffer (car edit))
    (format "Replaced %d occurrence%s in %s." (cdr edit) (if (= (cdr edit) 1) "" "s")
            (lispm-tools--label file))))

(defun lispm-tools--check-new-file (file)
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
      (let ((label (lispm-tools--label file))
            (where (lispm-tools--label (file-name-as-directory parent))))
        (cond ((not (file-directory-p parent))
               (error "Cannot write %s: %s is not a directory"
                      label (lispm-tools--label parent)))
              ((file-writable-p (file-name-as-directory parent)))
              ((file-directory-p directory)
               (error "Cannot write %s: %s is not writable" label where))
              (t
               (error "Cannot write %s: %s does not exist, and cannot be made: %s is not writable"
                      label (lispm-tools--label directory) where)))))))

(lispm-deftool write_file
    ((path string "File to create or overwrite, absolute or relative to your root.")
     (content string "The complete new contents."))
  "Create a file, or replace a file's entire contents.  Prefer edit_file
for changes to existing files."
  :permission write
  :diff (lambda (path content)
          (let ((file (lispm-tools--file path)))
            (list (lispm--file-text file) content (lispm-tools--label file))))
  :changes (lambda (path content)
             (let ((file (lispm-tools--file path)))
               ;; No buffer is visited to show a file that cannot be written.
               (lispm-tools--check-writable file)
               (lispm-tools--check-new-file file)
               (when-let* ((buffer (lispm-tools--to-show file t)))
                 (cons buffer content))))
  (let* ((file (lispm-tools--file path))
         (directory (file-name-directory file))
         (existed (file-exists-p file)))
    (lispm-tools--check-writable file)
    (lispm-tools--check-new-file file)
    ;; Made before FILE is visited: Emacs makes the buffer of a file it
    ;; cannot write read-only, in batch even for want of its directory.
    (when (and lispm-save-edits (not (file-directory-p directory)))
      (make-directory directory t))
    (lispm-tools--set-text (lispm-tools--visit file) content)
    (format "%s %s." (if existed "Overwrote" "Created") (lispm-tools--label file))))

(lispm-deftool edit_buffer
    ((buffer string "Buffer name.")
     (old-text string "Exact text to replace, with enough context to be unique.")
     (new-text string "Replacement text.")
     (replace-all boolean "Replace every occurrence instead of exactly one." :optional t))
  "Replace text in an Emacs buffer that is not a file, such as *scratch*.
For files use edit_file."
  :permission write
  :diff (lambda (buffer old-text new-text &optional replace-all)
          (let ((old (lispm-tools--buffer-file-text (lispm-tools--buffer buffer))))
            (list old (car (lispm-tools--replace old old-text new-text replace-all)) buffer)))
  :changes (lambda (buffer old-text new-text &optional replace-all)
             (let ((b (lispm-tools--buffer buffer)))
               (when-let* ((file (buffer-file-name b)))
                 (lispm-tools--check-writable file))
               (when (eq (lispm-tools--state b) 'current)
                 (cons b (car (lispm-tools--replace (lispm-tools--buffer-text b)
                                                    old-text new-text replace-all))))))
  (let* ((b (lispm-tools--buffer-to-change buffer))
         (edit (lispm-tools--replace (lispm-tools--buffer-text b)
                                     old-text new-text replace-all)))
    (lispm-tools--set-text b (car edit))
    (format "Replaced %d occurrence%s in %s." (cdr edit) (if (= (cdr edit) 1) "" "s") buffer)))

;;;; Shell commands that only read

(defcustom lispm-shell-read-only-programs
  '(("ls" . t) ("dir" . t) ("tree" . t) ("pwd" . t) ("file" . t) ("stat" . t)
    ("du" . t) ("df" . t) ("cat" . t) ("head" . t) ("tail" . t) ("wc" . t)
    ("grep" . t) ("egrep" . t) ("fgrep" . t) ("rg" . t) ("ag" . t)
    ("find" . lispm-shell--find-reads-p) ("fd" . lispm-shell--fd-reads-p)
    ("echo" . t) ("printf" . t) ("true" . t) ("false" . t) ("test" . t) ("[" . t)
    ("which" . t) ("whereis" . t) ("whatis" . t) ("type" . t) ("man" . t)
    ("basename" . t) ("dirname" . t) ("realpath" . t) ("readlink" . t)
    ("sort" . t) ("uniq" . t) ("cut" . t) ("tr" . t) ("diff" . t) ("cmp" . t) ("comm" . t)
    ("column" . t) ("fold" . t) ("nl" . t) ("paste" . t) ("rev" . t) ("seq" . t) ("expr" . t)
    ("jq" . t) ("xxd" . t) ("od" . t) ("hexdump" . t) ("strings" . t) ("nm" . t)
    ("md5" . t) ("md5sum" . t) ("shasum" . t) ("sha1sum" . t) ("sha256sum" . t) ("cksum" . t)
    ("date" . t) ("cal" . t) ("uname" . t) ("hostname" . t) ("whoami" . t) ("id" . t)
    ("uptime" . t) ("printenv" . t) ("locale" . t) ("ps" . t) ("pgrep" . t) ("lsof" . t)
    ("sw_vers" . t) ("arch" . t) ("nproc" . t)
    ("env" . lispm-shell--env-reads-p)
    ("git" . lispm-shell--git-reads-p)
    ("gh" . lispm-shell--gh-reads-p))
  "Programs whose commands only read, each with when: (PROGRAM . WHEN).
WHEN is t when every call of PROGRAM only reads, or a function of the
call's arguments, a list of strings, that returns non-nil when that
call does: `find' reads unless it is told to delete or run something.
A command made only of such calls, such as `git status', is of class
`read' (see `lispm-shell-read-only-p'): it runs as reading does,
unasked under the default policy, where other commands ask.  Programs
that run what they are given, such as sh, xargs, env with a command or
sudo, must not be here, nor any that writes: a reader of files that
can also write them, such as sed with -i, needs a function.  A program
is named without a directory: a command naming one by its path asks."
  :type '(alist :key-type (string :tag "Program")
                :value-type (choice (const :tag "Always" t) (function :tag "When")))
  :group 'lispm)

(defun lispm-shell--words (command)
  "COMMAND's simple commands, each a list of words, or `unsure'.
Words are split as the shell splits them, quotes and backslashes
undone, at spaces, newlines, |, ||, && and ;.  Anything that would
have the shell do more than run programs on words is `unsure': a
redirection other than to /dev/null, from a file or of one descriptor
to another; a command or process substitution, backquotes included; a
subshell, group or function; a job put in the background; a here
document; an environment assignment before a program; a comment.  So
is an empty command."
  (let ((length (length command))
        (i 0)
        commands words word (in-word nil) quote)
    (cl-labels ((char () (and (< i length) (aref command i)))
                (end-word () (when in-word (push (apply #'string (nreverse word)) words))
                           (setq word nil in-word nil))
                (end-command () (end-word)
                             (when words (push (nreverse words) commands))
                             (setq words nil))
                (add (c) (push c word) (setq in-word t))
                (unsure () (throw 'unsure 'unsure))
                (redirection ()
                  ;; At a <, > or &>, any descriptor before it dropped.
                  (let ((start i))
                    (while (memq (char) '(?< ?> ?&)) (cl-incf i))
                    (let ((op (substring command start i)))
                      (while (memq (char) '(?\s ?\t)) (cl-incf i))
                      (let ((target-start i))
                        (while (and (char) (not (memq (char) '(?\s ?\t ?\n ?| ?& ?\; ?< ?>))))
                          (cl-incf i))
                        (let ((target (substring command target-start i)))
                          (pcase op
                            ((or ">" ">>" "&>")
                             (unless (equal target "/dev/null") (unsure)))
                            (">&" (unless (string-match-p "\\`[0-9]+\\'" target) (unsure)))
                            ("<" (when (or (string-empty-p target) (string-prefix-p "(" target))
                                   (unsure)))
                            (_ (unsure)))))))))
      (catch 'unsure
        (while (< i length)
          (let ((c (aref command i)))
            (cond
             ((eq quote ?')
              (if (eq c ?') (setq quote nil) (add c))
              (cl-incf i))
             ((eq quote ?\")
              (cond ((eq c ?\") (setq quote nil) (cl-incf i))
                    ((eq c ?\\)
                     (cl-incf i)
                     (when (char) (add (char)) (cl-incf i)))
                    ((eq c ?`) (unsure))
                    ((and (eq c ?$) (eq (and (< (1+ i) length) (aref command (1+ i))) ?\())
                     (unsure))
                    (t (add c) (cl-incf i))))
             ((memq c '(?' ?\"))
              (setq quote c in-word t)
              (cl-incf i))
             ((eq c ?\\)
              (cl-incf i)
              (when (char) (add (char)) (cl-incf i)))
             ((memq c '(?\s ?\t))
              (end-word)
              (cl-incf i))
             ((memq c '(?\n ?\;))
              (end-command)
              (cl-incf i))
             ((eq c ?|)
              (end-command)
              (cl-incf i)
              (when (eq (char) ?|) (cl-incf i)))
             ((eq c ?&)
              (if (eq (and (< (1+ i) length) (aref command (1+ i))) ?&)
                  (progn (end-command) (cl-incf i 2))
                (if (eq (and (< (1+ i) length) (aref command (1+ i))) ?>)
                    (progn (end-word) (redirection))
                  (unsure))))
             ((memq c '(?< ?>))
              ;; 2>&1: the digits so far are the descriptor, not a word.
              (if (and in-word word (seq-every-p (lambda (d) (<= ?0 d ?9)) word))
                  (setq word nil in-word nil)
                (end-word))
              (redirection))
             ((memq c '(?` ?\( ?\)))
              (unsure))
             ((and (eq c ?$) (eq (and (< (1+ i) length) (aref command (1+ i))) ?\())
              (unsure))
             ((and (eq c ?#) (not in-word))
              (unsure))
             ((and (memq c '(?{ ?})) (not in-word)
                   (let ((next (and (< (1+ i) length) (aref command (1+ i)))))
                     (or (null next) (memq next '(?\s ?\t ?\n ?\;)))))
              (unsure))
             (t (add c) (cl-incf i)))))
        (end-command)
        (when quote (unsure))
        (let ((commands (nreverse commands)))
          (unless commands (unsure))
          (dolist (words commands)
            (when (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words))
              (unsure)))
          commands)))))

(defun lispm-shell-read-only-p (command)
  "Non-nil if the shell COMMAND only reads, as far as can be told.
It does when it runs only programs of `lispm-shell-read-only-programs',
each as that says it only reads, joined by pipes, && || or ;, writing
nowhere but /dev/null, with nothing the shell would run or expand into
a command (see `lispm-shell--words').  Any doubt answers nil: a command
this cannot read is one that may write."
  (when (stringp command)
    (let ((commands (lispm-shell--words command)))
      (and (listp commands)
           (seq-every-p
            (pcase-lambda (`(,program . ,args))
              (and (not (string-search "/" program))
                   (let ((when (alist-get program lispm-shell-read-only-programs nil nil #'equal)))
                     (cond ((eq when t) t)
                           ((functionp when)
                            (condition-case nil (and (funcall when args) t) (error nil)))))))
            commands)))))

(defun lispm-shell--none-of (args options)
  "Non-nil if none of ARGS is one of OPTIONS, or starts with one taking a value."
  (not (seq-some (lambda (arg)
                   (seq-some (lambda (option)
                               (or (equal arg option)
                                   (and (string-prefix-p "--" option)
                                        (string-prefix-p (concat option "=") arg))))
                             options))
                 args)))

(defun lispm-shell--find-reads-p (args)
  "Non-nil if find with ARGS only reads: it neither deletes nor runs nor writes."
  (lispm-shell--none-of args '("-delete" "-exec" "-execdir" "-ok" "-okdir"
                               "-fprint" "-fprint0" "-fprintf" "-fls")))

(defun lispm-shell--fd-reads-p (args)
  "Non-nil if fd with ARGS only reads: it runs nothing it finds."
  (lispm-shell--none-of args '("-x" "--exec" "-X" "--exec-batch" "-l" "--list-details")))

(defun lispm-shell--env-reads-p (args)
  "Non-nil if env with ARGS only prints the environment: it runs nothing."
  (seq-every-p (lambda (arg) (member arg '("-0" "--null"))) args))

(defun lispm-shell--positionals (args consuming)
  "The ARGS that are not options; CONSUMING are options that take the next ARG."
  (let (positionals)
    (while args
      (let ((arg (pop args)))
        (cond ((member arg consuming) (pop args))
              ((equal arg "--") (setq positionals (append (reverse args) positionals) args nil))
              ((string-prefix-p "-" arg))
              (t (push arg positionals)))))
    (nreverse positionals)))

(defconst lispm-shell--git-readers
  '("status" "log" "diff" "show" "blame" "annotate" "grep" "ls-files" "ls-tree" "ls-remote"
    "cat-file" "rev-parse" "rev-list" "describe" "shortlog" "merge-base" "name-rev"
    "count-objects" "check-ignore" "check-attr" "diff-tree" "diff-index" "diff-files"
    "for-each-ref" "show-ref" "show-branch" "var" "version" "whatchanged" "cherry"
    "range-diff" "help" "verify-commit" "verify-tag" "status")
  "Git subcommands that only read, whatever their arguments.")

(defun lispm-shell--git-reads-p (args)
  "Non-nil if git with ARGS only reads.
Of its options before the subcommand, -C, -P, --no-pager, --no-optional-locks,
--git-dir= and --work-tree= are read with; any other, -c among them, which
can have a subcommand run a program, is not.  Subcommands of
`lispm-shell--git-readers' read; branch, tag, remote, config, stash,
reflog, worktree, submodule and notes read while their arguments only ask."
  (let (subcommand)
    (while (and args (not subcommand))
      (let ((arg (pop args)))
        (cond ((equal arg "-C") (pop args))
              ((member arg '("-P" "--no-pager" "--no-optional-locks" "--help" "--version")))
              ((or (string-prefix-p "--git-dir=" arg) (string-prefix-p "--work-tree=" arg)))
              ((string-prefix-p "-" arg) (setq args nil subcommand 'unsure))
              (t (setq subcommand arg)))))
    (pcase subcommand
      ('nil t)
      ('unsure nil)
      ((pred (lambda (s) (member s lispm-shell--git-readers))) t)
      ("branch"
       (and (lispm-shell--none-of args '("-d" "-D" "-m" "-M" "-c" "-C" "-u" "-f" "--delete" "--move"
                                         "--copy" "--force" "--set-upstream-to" "--unset-upstream"
                                         "--edit-description" "--track" "--no-track"))
            (or (member "-l" args) (member "--list" args)
                (null (lispm-shell--positionals
                       args '("--contains" "--no-contains" "--merged" "--no-merged" "--points-at"
                              "--sort" "--format" "--color" "--abbrev"))))))
      ("tag"
       (and (lispm-shell--none-of args '("-d" "-D" "-f" "-a" "-s" "-u" "-m" "-F" "-e" "--delete"
                                         "--force" "--annotate" "--sign" "--local-user"
                                         "--message" "--file" "--edit"))
            (or (member "-l" args) (member "--list" args)
                (null (lispm-shell--positionals
                       args '("--contains" "--no-contains" "--merged" "--no-merged" "--points-at"
                              "--sort" "--format" "--color"))))))
      ("remote" (member (car (lispm-shell--positionals args nil)) '(nil "show" "get-url")))
      ("config"
       (and (lispm-shell--none-of args '("--unset" "--unset-all" "--add" "--replace-all" "-e" "--edit"
                                         "--rename-section" "--remove-section"))
            (let ((positionals (lispm-shell--positionals args '("-f" "--file" "--blob" "-t" "--type"
                                                                 "--default"))))
              (if (seq-some (lambda (arg) (member arg '("--get" "--get-all" "--get-regexp"
                                                        "--get-urlmatch" "-l" "--list"
                                                        "--get-color" "--get-colorbool")))
                            args)
                  (<= (length positionals) 2)
                (<= (length positionals) 1)))))
      ("stash" (member (car (lispm-shell--positionals args nil)) '("list" "show")))
      ("reflog" (member (car (lispm-shell--positionals args nil)) '(nil "show")))
      ("worktree" (equal (car (lispm-shell--positionals args nil)) "list"))
      ("submodule" (member (car (lispm-shell--positionals args nil)) '("status" "summary")))
      ("notes" (member (car (lispm-shell--positionals args nil)) '("show" "list")))
      (_ nil))))

(defconst lispm-shell--gh-readers
  '(("pr" "view" "list" "checks" "diff" "status")
    ("issue" "view" "list" "status")
    ("run" "list" "view")
    ("repo" "view")
    ("release" "list" "view")
    ("auth" "status")
    ("label" "list")
    ("workflow" "list" "view")
    ("gist" "list" "view")
    ("search")
    ("status")
    ("api"))
  "GitHub CLI commands that only read: (COMMAND . SUBCOMMANDS), every
subcommand of a COMMAND with none.")

(defun lispm-shell--gh-reads-p (args)
  "Non-nil if gh with ARGS only reads: a command of `lispm-shell--gh-readers',
not opening the browser (-w, --web), and api only with GET or HEAD and
nothing to send."
  (let ((positionals (lispm-shell--positionals args '("-R" "--repo"))))
    (and (lispm-shell--none-of args '("-w" "--web"))
         (when-let* ((readers (assoc (car positionals) lispm-shell--gh-readers)))
           (and (or (null (cdr readers)) (member (cadr positionals) (cdr readers)))
                (or (not (equal (car positionals) "api"))
                    (and (lispm-shell--none-of args '("-f" "-F" "--field" "--raw-field" "--input"))
                         (let ((method (or (cadr (member "-X" args)) (cadr (member "--method" args))
                                           (seq-some (lambda (arg)
                                                       (and (string-prefix-p "--method=" arg)
                                                            (substring arg 9)))
                                                     args))))
                           (member method '(nil "GET" "HEAD"))))))))))

;;;; Built-in tools: execution

(lispm-deftool run_shell
    ((command string "Shell command, run in your root directory.")
     (timeout integer "Seconds before the command is killed (default 120)." :optional t))
  "Run a shell command and return its output and exit status.  The
output also streams into a buffer the user can watch.  If your root is
remote, the command runs on its machine.  The user may have the
command run in a sandbox: its result then says what the sandbox
allows."
  :permission shell
  ;; A command that only reads is decided as reading is.
  :class (lambda (command &optional _timeout)
           (and (lispm-shell-read-only-p command) 'read))
  :async t
  ;; Setting up must not be cut short by C-g: a process left without
  ;; its timer and its agent could be neither timed out nor aborted.
  ;; A C-g meanwhile stops the command once it is set up.
  (let ((inhibit-quit t))
    (let* ((agent lispm-current-agent)
           (buffer (get-buffer-create
                    (format "*lispm-shell: %s*" (if agent (lispm-agent-name agent) "?"))))
           (default-directory (lispm-tools--root))
           (output "")
           ;; At `lispm-sandbox-level', or not at all if the sandbox
           ;; cannot be had.
           (run (lispm-sandbox-prepare
                 (with-connection-local-variables
                  (list shell-file-name shell-command-switch command))
                 nil (format "the command `%s'" (truncate-string-to-width command 30 nil nil "…"))))
           (note (lispm-sandbox-run-note run))
           (proc (make-process
                  :name "lispm-shell" :buffer buffer :noquery t
                  :connection-type 'pipe
                  ;; On a remote root, TRAMP runs the command there,
                  ;; with that machine's shell.
                  :file-handler t
                  :command (lispm-sandbox-run-command run)
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
                    (unless (or (process-live-p p) (process-get p 'interrupted))
                      (when-let* ((timer (process-get p 'timer))) (cancel-timer timer))
                      (let ((status (process-exit-status p)))
                        (funcall done
                                 (concat output
                                         "\n["
                                         (if (process-get p 'timed-out)
                                             "killed: timeout"
                                           (format "exit status %d" status))
                                         (if note (concat "; " note) "")
                                         "]")
                                 (process-get p 'timed-out))))))))
      (lispm-sandbox-attach run proc)
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
                                    (kill-process proc)))))
      (when quit-flag
        (setq quit-flag nil)
        (cancel-timer (process-get proc 'timer))
        (process-put proc 'interrupted t)
        (delete-process proc)
        (signal 'quit nil)))))

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

(defcustom lispm-tools-file-writers
  '(save-buffer save-some-buffers write-file write-region append-to-file
    with-temp-file delete-file rename-file copy-file make-directory
    delete-directory make-symbolic-link set-file-modes)
  "Functions that change files on disk.
Code naming one of them changes files: see `lispm-tools-code-writes-p'."
  :type '(repeat symbol)
  :group 'lispm)

(defcustom lispm-tools-buffer-writers
  '(insert insert-before-markers delete-region erase-buffer delete-char
    kill-region kill-line kill-buffer replace-regexp replace-string
    replace-regexp-in-region replace-string-in-region indent-region
    comment-region uncomment-region upcase-region downcase-region
    set-buffer-modified-p)
  "Functions that change the text of a buffer.
Code naming one of them changes buffers, unless it names it inside a
`with-temp-buffer' form, whose buffer is gone once it ends: see
`lispm-tools-code-writes-p'."
  :type '(repeat symbol)
  :group 'lispm)

(defun lispm-tools--walk-writes (form temporary writers strings seen)
  "Collect what FORM names into WRITERS and STRINGS, two cons cells.
TEMPORARY non-nil while inside a `with-temp-buffer' form, where only
`lispm-tools-file-writers' count; WRITERS and STRINGS hold the symbols
and strings found, newest first, in their cdr.  SEEN, a hash table, holds
the conses and vectors walked: a form read with circular syntax, such as
#1=(insert . #1#), is walked once and the walk ends."
  (cond ((symbolp form)
         (when (and form
                    (or (memq form lispm-tools-file-writers)
                        (and (not temporary) (memq form lispm-tools-buffer-writers)))
                    (not (memq form (cdr writers))))
           (push form (cdr writers))))
        ((stringp form)
         (unless (member form (cdr strings)) (push form (cdr strings))))
        ((gethash form seen))
        ((vectorp form)
         (puthash form t seen)
         (mapc (lambda (f) (lispm-tools--walk-writes f temporary writers strings seen)) form))
        ((consp form)
         (let ((temporary (or temporary (eq (car-safe form) 'with-temp-buffer))))
           ;; A dotted tail ends the list; a cons seen before, a circle.
           (while (and (consp form) (not (gethash form seen)))
             (puthash form t seen)
             (lispm-tools--walk-writes (pop form) temporary writers strings seen))
           (when (and form (not (consp form)))
             (lispm-tools--walk-writes form temporary writers strings seen))))))

(defun lispm-tools--code-writes (code)
  "What the Lisp CODE, a string, names: (WRITERS . STRINGS), in order.
WRITERS are the symbols of `lispm-tools-file-writers' and
`lispm-tools-buffer-writers' it names (see `lispm-tools-code-writes-p'),
STRINGS its strings.  Where CODE stops reading, the rest is searched for
the writers' names as words, and gives no strings."
  (let ((writers (list nil)) (strings (list nil)) (seen (make-hash-table :test #'eq))
        (at 0) (end (length code)))
    (while (< at end)
      (condition-case nil
          (pcase-let ((`(,form . ,next) (read-from-string code at)))
            (lispm-tools--walk-writes form nil writers strings seen)
            (setq at next))
        ;; The rest does not read: whitespace, or an unbalanced form.
        (error
         (dolist (name (append lispm-tools-file-writers lispm-tools-buffer-writers))
           (when (and (not (memq name (cdr writers)))
                      (string-match-p (concat "\\_<" (regexp-quote (symbol-name name)) "\\_>")
                                      (substring code at)))
             (push name (cdr writers))))
         (setq at end))))
    (cons (nreverse (cdr writers)) (nreverse (cdr strings)))))

(defun lispm-tools-code-writes-p (code)
  "The functions changing buffers or files that the Lisp CODE names, or nil.
CODE is a string of forms.  Those of `lispm-tools-file-writers' count
wherever they appear; those of `lispm-tools-buffer-writers' outside a
`with-temp-buffer' form.  A quoted name counts as much as a call, so
\(mapc #\\='delete-file files) does.  Code that does not read is searched
for the names as words."
  (car (lispm-tools--code-writes code)))

(defun lispm-tools-code-files (code)
  "The strings in the Lisp CODE that look like file names, in order.
A string with no whitespace, naming a directory (a slash, not of a
URL) or ending in a file extension."
  (seq-filter (lambda (s)
                (and (> (length s) 1)
                     (not (string-match-p "[[:space:]\\\\]\\|://" s))
                     (or (string-search "/" s)
                         (string-match-p "\\`[^.]+\\.[A-Za-z0-9]\\{1,5\\}\\'" s))))
              (cdr (lispm-tools--code-writes code))))

(defun lispm-tools-code-writes-note (code)
  "What the user is told of the Lisp CODE if it changes buffers or files.
Nil if it names none of `lispm-tools-file-writers' and
`lispm-tools-buffer-writers' (see `lispm-tools-code-writes-p'); else
that it does, that no diff shows it, and the files it names (see
`lispm-tools-code-files')."
  (when (lispm-tools-code-writes-p code)
    (let ((files (lispm-tools-code-files code)))
      (concat (lispm-tr "This code changes buffers or files; it shows no diff, as the edit tools would.")
              (if files
                  (concat " " (lispm-tr "It names %s." (string-join files ", ")))
                "")))))

(defcustom lispm-eval-timeout 120
  "Seconds eval_elisp waits for a value promised with `lispm-async'."
  :type 'integer
  :group 'lispm)

(defcustom lispm-eval-backtrace t
  "Whether an error of the code eval_elisp evaluates comes with its backtrace.
The backtrace shows the arguments of the calls between the code and
the error: values the model may not have seen otherwise, such as a
password read with `auth-source'.  Turn it off if those should not
leave Emacs."
  :type 'boolean
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

(defconst lispm-tools--backtrace-frames 20
  "How many frames of a backtrace eval_elisp returns with an error, at most.")

(defvar lispm-tools--eval-form nil
  "The form eval_elisp is evaluating: where its backtraces stop.")

(defvar lispm-tools--backtrace nil
  "Frames of the last error the code eval_elisp evaluates signaled.
Each is (FUNCTION . ARGS), innermost first.")

(defun lispm-tools--note-backtrace (_error)
  "Keep the frames of an error the code eval_elisp evaluates signals.
A `handler-bind' handler: it runs where the error is signaled, before
the stack unwinds, and lets the error go on.  As it runs for errors
the code catches too, it only collects the frames up to the `eval' of
`lispm-tools--eval-form', one more than `lispm-tools--backtrace-frames'
at most: they are printed if the error reaches eval_elisp."
  (when lispm-eval-backtrace
    (let ((frames nil) (count 0))
      (catch 'done
        (mapbacktrace
         (lambda (_evald function args _flags)
           (when (and (eq function 'eval) (eq (car-safe args) lispm-tools--eval-form))
             (throw 'done nil))
           (unless (eq function #'lispm-tools--note-backtrace)
             (push (cons function args) frames)
             (when (> (cl-incf count) lispm-tools--backtrace-frames)
               (throw 'done nil))))
         #'lispm-tools--note-backtrace))
      (setq lispm-tools--backtrace (nreverse frames)))))

(defun lispm-tools--backtrace-text (frames)
  "FRAMES, each (FUNCTION . ARGS), innermost first, as text.
Past `lispm-tools--backtrace-frames' of them, \"...\" stands for the rest."
  (let ((print-length 6) (print-level 3) (print-escape-newlines t))
    (lispm--truncate
     (string-join
      (append (mapcar (lambda (frame)
                        (concat "  " (truncate-string-to-width (prin1-to-string frame)
                                                               200 nil nil "…")))
                      (seq-take frames lispm-tools--backtrace-frames))
              (and (nthcdr lispm-tools--backtrace-frames frames) (list "  ...")))
      "\n")
     4000)))

(defun lispm-tools--defined-note (names)
  "What eval_elisp adds to its result about the tools NAMES it defined."
  (if names
      (concat "\n\n" lispm-tools--loaded-prefix (string-join names ", ") ".\n"
              "They are yours from your next step on, until Emacs exits; "
              "to keep them for later sessions, write a skill (write_skill).")
    ""))

(lispm-deftool eval_elisp
    ((code string "One or more Emacs Lisp forms, evaluated in order with lexical binding."))
  "Evaluate Emacs Lisp in the running Emacs and return the last value,
plus anything printed.  This reaches all of Emacs: buffers, windows,
processes, the network, org, magit, any installed package.  Find
functions with apropos, and the skills of this system's parts and apps
with search_tools; learn functions with describe_symbol, and from the
manuals with read_info, which search_tools finds.
To wait for something asynchronous (a process, a page loading, a
timer), end with (lispm-async (lambda (resolve) ...)) and call
\(funcall resolve VALUE) once the value is there.  An error comes back
with the backtrace of the code, innermost call first.  A tool you
define here with lispm-deftool is yours from your next step on, and
other agents can find it with search_tools; it lasts until Emacs
exits, unless you keep it in a skill with write_skill, which
search_tools finds.
Files and buffers are changed with the edit tools (edit_file,
write_file, edit_buffer, edit_definition), which show the user a diff
to approve and can be undone as one step.  Lisp that saves or rewrites
a file gets no such review: it is for what the edit tools cannot do."
  :permission exec
  :async t
  :preview #'lispm-tools-code-writes-note
  (let* ((agent lispm-current-agent)
         (output (generate-new-buffer " *lispm-eval*" t))
         (standard-output output)
         (default-directory (lispm-tools--root))
         (before (lispm-tools--snapshot))
         value printed failure note)
    (unwind-protect
        (condition-case err
            (dolist (form (lispm-tools--read-forms code))
              (let ((lispm-tools--eval-form form)
                    (lispm-tools--backtrace nil))
                (condition-case err
                    (setq value (handler-bind ((error #'lispm-tools--note-backtrace))
                                  (eval form t)))
                  (error
                   (if (not lispm-tools--backtrace)
                       (signal (car err) (cdr err))
                     (error "%s\n\nBacktrace, innermost call first:\n%s"
                            (error-message-string err)
                            (lispm-tools--backtrace-text lispm-tools--backtrace)))))))
          (error (setq failure (error-message-string err))))
      (setq printed (with-current-buffer output (buffer-string)))
      (kill-buffer output)
      ;; Tools the code defined, also before an error or a C-g, are
      ;; the agent's, deferred: in no other agent's core layer.
      (setq note (lispm-tools--defined-note (lispm-tools--adopt agent before))))
    (cond
     (failure (error "%s%s" failure note))
     ((not (lispm-async-p value))
      (funcall done (concat (lispm-tools--eval-result printed value) note)))
     (t
      (let ((timer (run-at-time
                    lispm-eval-timeout nil
                    (lambda ()
                      (funcall done (format "Timed out after %d seconds waiting for the promised value.%s"
                                            lispm-eval-timeout note)
                               t)))))
        (lispm-async-then value
                          (lambda (result failed)
                            (cancel-timer timer)
                            (funcall done (concat (lispm-tools--eval-result printed result) note)
                                     failed))))))))

(provide 'lispm-tools)
;;; lispm-tools.el ends here
