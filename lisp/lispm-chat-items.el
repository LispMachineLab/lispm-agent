;;; lispm-chat-items.el --- Chat faces, item rendering, markdown, and boxes  -*- lexical-binding: t; -*-

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

;; Chat UI faces, item rendering pipeline, markdown presentation
;; linking, diff formatting, welcome boxes, and item caches.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-memory)
(require 'lispm-inspect)
(require 'lispm-sandbox)

(defgroup lispm-chat nil
  "The chat view of LispM sessions."
  :group 'lispm
  :prefix "lispm-chat-")

(defface lispm-chat-user '((t :inherit shadow))
  "Face of the messages you sent."
  :group 'lispm-chat)

(defface lispm-chat-dim '((t :inherit shadow))
  "Face of secondary text: results, hints, rules."
  :group 'lispm-chat)

(defface lispm-chat-thought '((t :inherit shadow :slant italic))
  "Face of the agent's reasoning."
  :group 'lispm-chat)

(defface lispm-chat-tool '((t :weight bold))
  "Face of tool names."
  :group 'lispm-chat)

(defface lispm-chat-ok '((t :inherit success))
  "Face of the marker of a call that succeeded."
  :group 'lispm-chat)

(defface lispm-chat-error '((t :inherit error))
  "Face of the marker and text of a call that failed or was denied."
  :group 'lispm-chat)

(defface lispm-chat-pending '((t :inherit warning))
  "Face of the marker of a call running or awaiting approval."
  :group 'lispm-chat)

(defface lispm-chat-added '((t :inherit diff-added))
  "Face of added lines in diffs."
  :group 'lispm-chat)

(defface lispm-chat-removed '((t :inherit diff-removed))
  "Face of removed lines in diffs."
  :group 'lispm-chat)

(defface lispm-chat-code '((t :inherit font-lock-constant-face))
  "Face of code in the agent's replies."
  :group 'lispm-chat)

(defface lispm-chat-accent '((t :inherit font-lock-keyword-face))
  "Face of the prompt, the spinner and the selected answer."
  :group 'lispm-chat)

(defcustom lispm-chat-preview-lines 3
  "Lines of a tool's output shown before it is expanded."
  :type 'integer)

(defcustom lispm-chat-diff-lines 12
  "Lines of a diff shown before it is expanded."
  :type 'integer)

(defcustom lispm-chat-prompt "λ >> "
  "The prompt before what you write, and before the messages you sent."
  :type 'string)

(defvar-local lispm-chat-agent nil
  "The agent this chat talks to.")
(put 'lispm-chat-agent 'permanent-local t)

(defvar-local lispm-chat-expanded nil
  "Hash table of the keys of the items shown in full.")

(defvar-local lispm-chat-rendered nil
  "Hash table from the items last drawn to their text, so unchanged items
are not drawn again.")

(defvar-local lispm-chat-streamed nil
  "What of a reply streaming in is drawn already, as (TEXT SHOWN LINES CODE):
TEXT its complete lines, SHOWN them as `lispm-chat-markdown' shows them
with nothing presented, LINES non-nil when SHOWN holds a line, CODE
non-nil when a code block is open after TEXT.  Each redraw draws only
the lines that came after.")


(defvar lispm-chat-refresh-jump-bars-function nil
  "Hook called after redrawing to refresh floating jump-to-bottom pills.")

(defvar lispm-chat-update-footer-function nil
  "Hook called after redrawing to refresh the input rule and footer.")

;;;; Drawing items

(defun lispm-chat-width ()
  "Width to draw rules and boxes at.
That is the columns text can use beside line numbers, less one kept for
the numbers gaining a digit as the chat grows."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 40 (min 100 (1- (if window (window-max-chars-per-line window) 80))))))

(defun lispm-chat-indent (text prefix &optional first)
  "TEXT with PREFIX before each line, or FIRST before the first one.
Lines that wrap are indented by PREFIX too."
  (let ((lines (split-string text "\n")))
    (propertize (concat (or first prefix) (car lines)
                        (mapconcat (lambda (line) (concat "\n" prefix line)) (cdr lines) ""))
                'wrap-prefix (make-string (string-width prefix) ?\s))))

(defun lispm-chat-root ()
  "The directory file names in the chat are relative to: its agent's root."
  (if lispm-chat-agent (lispm-agent-root lispm-chat-agent) default-directory))

(defun lispm-chat--present (text)
  "TEXT with the objects it names presented, to act on with RET or a click."
  (lispm-present-text text (lispm-chat-root)))

(defun lispm-chat--inline (line &optional defined plain)
  "LINE of a reply with its Markdown headings, bold, italics and code shown
as faces.  Code naming a symbol or a file is presented as the object,
and code naming what this Emacs lacks is marked; DEFINED are the names
the reply defines.  PLAIN presents no code, for a reply still streaming
in: it is presented once it is complete."
  (let ((line (if (string-match "\\`#+ +\\(.*\\)\\'" line)
                  (propertize (match-string 1 line) 'face 'bold)
                line))
        (start 0) parts)
    ;; Org links name literal objects: the stars in *scratch*, for
    ;; example, are part of the buffer's name, not Markdown emphasis.
    (while (string-match lispm-present-link-re line start)
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (push (lispm-chat--inline-markdown (substring line start beg) defined plain) parts)
        (push (substring line beg end) parts)
        (setq start end)))
    (push (lispm-chat--inline-markdown (substring line start) defined plain) parts)
    (apply #'concat (nreverse parts))))

(defun lispm-chat--inline-markdown (line &optional defined plain)
  "Show Markdown emphasis and code in LINE, which contains no Org links.
DEFINED and PLAIN are as in `lispm-chat--inline'."
  (setq line (replace-regexp-in-string
              "\\*\\*\\([^*\n]+\\)\\*\\*"
              (lambda (m) (propertize (match-string 1 m) 'face 'bold)) line t t))
  (lispm-chat--italics
   (replace-regexp-in-string
    "`\\([^`\n]+\\)`"
    ;; The match data say what of the match to replace: presenting
    ;; the code must leave them as they are.
    (lambda (m) (let ((code (propertize (match-string 1 m) 'face 'lispm-chat-code 'lispm-chat-code t)))
                  (if plain
                      code
                    (save-match-data (lispm-present-code code (lispm-chat-root) defined)))))
    line t t)))

(defun lispm-chat--italics (line)
  "LINE with its Markdown emphasis, *TEXT* or _TEXT_, in italics.
Not inside a word, as in 2*3*4 or snake_case, nor inside code, which
the code's `lispm-chat-code' property marks."
  (let ((start 0) (parts nil))
    (while (string-match "\\([*_]\\)\\([^*_[:space:]]\\(?:[^\n]*?[^[:space:]]\\)?\\)\\1" line start)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (text (match-string 2 line))
             (word-p (lambda (pos) (and (>= pos 0) (< pos (length line))
                                        (string-match-p "[[:alnum:]*_]" (string (aref line pos)))))))
        (if (or (funcall word-p (1- beg)) (funcall word-p end)
                (text-property-not-all beg end 'lispm-chat-code nil line))
            (progn (push (substring line start (1+ beg)) parts)
                   (setq start (1+ beg)))
          (add-face-text-property 0 (length text) 'italic t text)
          (push (substring line start beg) parts)
          (push text parts)
          (setq start end))))
    (apply #'concat (nreverse (cons (substring line start) parts)))))

(defun lispm-chat--fence-p (line)
  "Return non-nil when LINE of a reply opens or closes a code block."
  (string-match-p "\\`[ \t]*```" line))

(defun lispm-chat--markdown-line (line in-code &optional defined plain)
  "LINE of a reply, no fence, as `lispm-chat-markdown' shows it: as code
when IN-CODE, else see `lispm-chat--inline' for DEFINED and PLAIN."
  (if in-code
      (propertize (concat "  " line) 'face 'lispm-chat-code)
    (lispm-chat--inline line defined plain)))

(defun lispm-chat-markdown (text &optional plain)
  "TEXT of a reply, its Markdown shown as faces.
Code blocks lose their fences and keep their lines as they are.  PLAIN
presents no code, see `lispm-chat--inline'."
  (let ((defined (and (not plain) (lispm-ground-definitions text)))
        in-code out)
    (dolist (line (split-string text "\n"))
      (cond ((lispm-chat--fence-p line)
             (setq in-code (not in-code)))
            ;; An image or a video the reply links alone on a line is
            ;; shown there: the model showing it.
            ((and (not in-code) (not plain)
                  (when-let* ((shown (lispm-chat--media-line line)))
                    (push shown out))))
            (t (push (lispm-chat--markdown-line line in-code defined plain) out))))
    (string-join (nreverse out) "\n")))

(defun lispm-chat--streaming-markdown (text)
  "TEXT of a reply streaming in, as `lispm-chat-markdown' shows it with
nothing presented.  Its complete lines are kept as drawn, in
`lispm-chat-streamed', so each step draws only those that came since:
drawn whole, a long reply took longer at every step."
  (pcase-let* ((`(,source ,shown ,lines ,code)
                (if (and lispm-chat-streamed (string-prefix-p (car lispm-chat-streamed) text))
                    lispm-chat-streamed
                  (list "" "" nil nil)))
               (from (length source))
               (end (let ((at from) (newline (string-search "\n" text from)))
                      (while newline
                        (setq at (1+ newline) newline (string-search "\n" text at)))
                      at)))
    (when (> end from)
      (let (new)
        (dolist (line (split-string (substring text from (1- end)) "\n"))
          (if (lispm-chat--fence-p line)
              (setq code (not code))
            (push (lispm-chat--markdown-line line code nil t) new)))
        (when new
          (setq shown (string-join (if lines (cons shown (nreverse new)) (nreverse new)) "\n")
                lines t)))
      (setq source (substring text 0 end)))
    (setq lispm-chat-streamed (list source shown lines code))
    (let ((last (substring text end)))
      (cond ((lispm-chat--fence-p last) shown)
            (lines (concat shown "\n" (lispm-chat--markdown-line last code nil t)))
            (t (lispm-chat--markdown-line last code nil t))))))

(defconst lispm-chat--tools
  `(("read_file" :shown ,(lispm-tr-noop "Read" "tool") :result read)
    ("read_buffer" :shown ,(lispm-tr-noop "Read" "tool") :result read)
    ("edit_file" :shown ,(lispm-tr-noop "Update" "tool") :result edit :ask diff
     :title ,(lispm-tr-noop "Edit file") :question ,(lispm-tr-noop "Do you want to make this edit to %s?"))
    ("edit_buffer" :shown ,(lispm-tr-noop "Update" "tool") :result edit :ask diff
     :title ,(lispm-tr-noop "Edit file") :question ,(lispm-tr-noop "Do you want to make this edit to %s?"))
    ("edit_definition" :shown ,(lispm-tr-noop "Redefine" "tool") :result definition :ask diff
     :title ,(lispm-tr-noop "Edit definition")
     :question ,(lispm-tr-noop "Do you want to make this edit to %s?") :subject name)
    ("write_file" :shown ,(lispm-tr-noop "Write" "tool") :result write :ask diff
     :title ,(lispm-tr-noop "Write file") :question ,(lispm-tr-noop "Do you want to write %s?"))
    ("eval_definition" :shown ,(lispm-tr-noop "Evaluate" "tool") :ask preview
     :title ,(lispm-tr-noop "Evaluate a definition") :question ,(lispm-tr-noop "Do you want to evaluate this?"))
    ("write_skill" :shown "WriteSkill" :ask skill
     :exact ,(lispm-tr-noop "Yes, and don't ask again for this exact skill")
     :title ,(lambda (input _preview) (lispm-tr "Write the skill %s" (alist-get 'name input)))
     :question ,(lambda (_input preview)
                  (if-let* ((file (lispm-chat--diff-file preview)))
                      (lispm-tr "Do you want to save it as %s?" file)
                    (lispm-tr "Do you want to save it?")))
     :note ,(lispm-tr-noop "Its code runs whenever an agent loads the skill, from now on."))
    ("trust_skill" :ask skill :alone t
     :title ,(lambda (input _preview)
               (lispm-tr "Run the code of the project's skill %s" (alist-get 'name input)))
     :question ,(lambda (_input preview)
                  (if-let* ((file (lispm-chat--diff-file preview)))
                      (lispm-tr "Do you want to evaluate this code from %s?" file)
                    (lispm-tr "Do you want to evaluate this code?")))
     :note ,(lambda (input _preview)
              (and (> (length (alist-get 'files input)) 0)
                   (lispm-tr "It may load the files beside it: %s."
                             (mapconcat #'identity (alist-get 'files input) ", ")))))
    ("run_shell" :shown "Bash" :ask command :title ,(lispm-tr-noop "Bash command")
     :exact ,(lispm-tr-noop "Yes, and don't ask again for this exact command"))
    ("eval_elisp" :shown "Eval" :ask code :title ,(lispm-tr-noop "Evaluate Emacs Lisp")
     :question ,(lispm-tr-noop "Do you want to evaluate this?")
     :exact ,(lispm-tr-noop "Yes, and don't ask again for this exact code")
     ;; Code that writes shows no diff, as the edit tools would: say so.
     :note ,(lambda (input _preview)
              (lispm-tools-code-writes-note (or (alist-get 'code input) ""))))
    ("run_lisp" :shown "Lisp" :ask code :title ,(lispm-tr-noop "Run a Lisp program")
     :question ,(lispm-tr-noop "Do you want to run this?"))
    ("search_files" :shown ,(lispm-tr-noop "Search" "tool") :result search)
    ("list_directory" :shown ,(lispm-tr-noop "List" "tool") :result list)
    ("list_buffers" :shown ,(lispm-tr-noop "Buffers" "tool"))
    ("describe_symbol" :shown ,(lispm-tr-noop "Describe" "tool"))
    ("remember" :shown ,(lispm-tr-noop "Remember" "tool")) ("recall" :shown ,(lispm-tr-noop "Recall" "tool"))
    ("forget" :shown ,(lispm-tr-noop "Forget" "tool"))
    ("task_add" :shown ,(lispm-tr-noop "Task" "tool")) ("task_update" :shown ,(lispm-tr-noop "Task" "tool"))
    ("task_list" :shown ,(lispm-tr-noop "Tasks" "tool"))
    ("ask_agent" :shown ,(lispm-tr-noop "Ask" "tool")) ("send_message" :shown ,(lispm-tr-noop "Message" "tool"))
    ("list_agents" :shown ,(lispm-tr-noop "Agents" "tool"))
    ("start_agent" :shown ,(lispm-tr-noop "Start" "tool"))
    ("search_tools" :shown "ToolSearch") ("skill" :shown ,(lispm-tr-noop "Skill" "tool"))
    ("apropos" :shown "Apropos") ("read_info" :shown "Info")
    ("web_search" :shown "WebSearch") ("web_fetch" :shown "WebFetch")
    ("ert_run" :shown ,(lispm-tr-noop "Test" "tool"))
    ("xref_definitions" :shown ,(lispm-tr-noop "Definitions" "tool"))
    ("xref_references" :shown ,(lispm-tr-noop "References" "tool"))
    ("flymake_diagnostics" :shown ,(lispm-tr-noop "Diagnostics" "tool")))
  "How the chat shows each tool's calls: (NAME . PROPERTIES).
:shown is the name a call is shown under.  :result is how its result is
summed up: `read', `edit', `definition', `write', `search' or `list';
lines of it otherwise.  :ask is what its approval shows: `diff',
`preview', `skill', `command' or `code'; the input otherwise.  :title,
:question and :note word the approval, each a string, %s standing for
the call's file or, with :subject `name', the name it names, or a
function of the call's input and preview; :exact is the answer that
approves it for good when that allows only the exact call again, and
:alone keeps its title from naming the program it runs in.  The
strings are English, translated where shown (see lispm-i18n.el).")

(defun lispm-chat-tool-get (name property)
  "PROPERTY of the tool NAME, see `lispm-chat--tools'."
  (plist-get (cdr (assoc name lispm-chat--tools)) property))

(defun lispm-chat-tool-text (name property input preview subject)
  "The text PROPERTY of the tool NAME gives a call of INPUT and PREVIEW,
SUBJECT standing for %s, or nil when it gives none."
  (let ((value (lispm-chat-tool-get name property)))
    (cond ((functionp value) (funcall value input preview))
          (value (lispm-tr value subject)))))

(defun lispm-chat-tool-name (name)
  "The name the tool NAME is shown under; SERVER:TOOL for an MCP server's."
  (cond ((when-let* ((shown (lispm-chat-tool-get name :shown))) (lispm-tr shown)))
        ((and name (string-match "\\`mcp__\\(.+?\\)__\\(.+\\)\\'" name))
         (format "%s:%s" (match-string 1 name) (match-string 2 name)))
        (t name)))

(defun lispm-chat--argument-object (item)
  "The object the main argument of the tool call ITEM names, or nil."
  (let ((input (plist-get item :input))
        (root (lispm-chat-root)))
    (cond ((stringp (alist-get 'path input))
           (when-let* ((file (lispm-present-file (car (lispm-tools-reference (alist-get 'path input)))
                                                  root)))
             (list 'file file (let ((offset (alist-get 'offset input)))
                                (and (integerp offset) offset)))))
          ((stringp (alist-get 'buffer input))
           (list 'buffer (lispm-tools-buffer-name (alist-get 'buffer input))))
          ((and (equal (plist-get item :name) "describe_symbol") (stringp (alist-get 'name input))
                (intern-soft (alist-get 'name input)))
           (list 'symbol (intern-soft (alist-get 'name input))))
          ((when-let* ((name (seq-find #'stringp (list (alist-get 'agent input) (alist-get 'to input))))
                       ((lispm-get-agent name t)))
             (list 'agent name))))))

(defun lispm-chat--tool-title (item)
  "Name(argument) of the tool call ITEM.
The name presents the tool, and the argument the object it names, if
it names one."
  (let* ((input (plist-get item :input))
         (argument (if (and (equal (plist-get item :name) "search_files")
                            (stringp (alist-get 'pattern input)))
                       (concat (if (stringp (alist-get 'path input))
                                   (lispm-tr "%S in %s" (alist-get 'pattern input) (alist-get 'path input))
                                 (format "%S" (alist-get 'pattern input)))
                               (if (stringp (alist-get 'glob input))
                                   (format ", %s" (alist-get 'glob input))
                                 ""))
                     (lispm-transcript-summary input))))
    (concat (lispm-present (propertize (lispm-chat-tool-name (plist-get item :name))
                                       'face 'lispm-chat-tool)
                           (list 'tool (plist-get item :name)))
            (if argument
                (let ((shown (lispm-oneline argument 70))
                      (object (lispm-chat--argument-object item)))
                  (format "(%s)" (if object (lispm-present shown object) shown)))
              ""))))

(defun lispm-chat-input-diff (input)
  "A diff of the edit described by INPUT, for edits that were not previewed."
  (let ((old (or (alist-get 'old_text input) (alist-get 'old-text input)))
        (new (or (alist-get 'new_text input) (alist-get 'new-text input))))
    (when (and (stringp old) (stringp new))
      (concat (mapconcat (lambda (l) (concat "-" l)) (split-string old "\n") "\n") "\n"
              (mapconcat (lambda (l) (concat "+" l)) (split-string new "\n") "\n")))))

(defun lispm-chat-render-diff-lines (diff)
  "The lines of the unified DIFF worth showing, with faces."
  (let (lines)
    (dolist (line (split-string (or diff "") "\n"))
      (cond ((string-match-p "\\`\\(---\\|\\+\\+\\+\\) " line))
            ((string-prefix-p "@@" line) (push (propertize line 'face 'lispm-chat-dim) lines))
            ((string-prefix-p "+" line) (push (propertize line 'face 'lispm-chat-added) lines))
            ((string-prefix-p "-" line) (push (propertize line 'face 'lispm-chat-removed) lines))
            ((string-prefix-p " " line) (push (propertize line 'face 'lispm-chat-dim) lines))))
    (nreverse lines)))

(defun lispm-chat--diff-file (diff)
  "The file the unified DIFF changes, as its +++ line names it, or nil."
  (and diff (string-match "^\\+\\+\\+ b/\\(.+\\)$" diff) (match-string 1 diff)))

(defun lispm-chat--diff-summary (diff path)
  "What DIFF does to PATH, in a sentence."
  (let ((added 0) (removed 0))
    (dolist (line (split-string (or diff "") "\n"))
      (cond ((string-match-p "\\`\\(---\\|\\+\\+\\+\\) " line))
            ((string-prefix-p "+" line) (cl-incf added))
            ((string-prefix-p "-" line) (cl-incf removed))))
    (lispm-tr "Updated %s with %s and %s" (or path (lispm-tr "the file"))
              (lispm-tr-n "%d addition" "%d additions" added)
              (lispm-tr-n "%d removal" "%d removals" removed))))

(defun lispm-chat--more (lines total)
  "LINES, followed by a note of how many of TOTAL lines they leave out."
  (if (> total (length lines))
      (append lines (list (propertize (lispm-tr-n "… +%d line (TAB to expand)" "… +%d lines (TAB to expand)"
                                                  (- total (length lines)))
                                      'face 'lispm-chat-dim)))
    lines))

(defun lispm-chat--preview (lines n expanded)
  "LINES, all of them when EXPANDED, else the first N and how many more."
  (lispm-chat--more (if expanded lines (seq-take lines n)) (length lines)))

(defun lispm-chat--result-lines (item expanded)
  "Lines shown under the tool call ITEM; all of them when EXPANDED."
  (let* ((name (plist-get item :name))
         (input (plist-get item :input))
         (path (or (alist-get 'path input) (alist-get 'buffer input)))
         (result (or (plist-get item :result) ""))
         (lines (and (not (string-blank-p result))
                     (split-string (string-trim-right result) "\n"))))
    (pcase (plist-get item :state)
      ("WRITING" nil)
      ((or "RUNNING" "APPROVED") (list (lispm-tr "Running…")))
      ("WAITING"
       (if-let* ((stopped (plist-get item :stopped))
                 (lines (split-string (string-trim-right stopped) "\n")))
           (mapcar (lambda (l) (propertize l 'face 'lispm-chat-error))
                   (lispm-chat--preview lines 2 expanded))
         (list (lispm-tr "Waiting for your approval…"))))
      ((or "DENIED" "FAILED")
       (mapcar (lambda (l) (propertize l 'face 'lispm-chat-error))
               (lispm-chat--preview lines 2 expanded)))
      (_
       (pcase (lispm-chat-tool-get name :result)
         ((and 'definition (guard (not (plist-get item :preview))))
          ;; Run unasked, it has no diff: its input holds only the new form.
          (let ((new (split-string (string-trim-right (or (alist-get 'new_text input) "")) "\n")))
            (cons (lispm-tr "Redefined %s" (alist-get 'name input))
                  (lispm-chat--preview new lispm-chat-preview-lines expanded))))
         ((or 'edit 'definition)
          (let* ((diff (or (plist-get item :preview) (lispm-chat-input-diff input)))
                 (shown (lispm-chat-render-diff-lines diff)))
            (cons (lispm-chat--diff-summary diff (or path (alist-get 'name input)))
                  (lispm-chat--preview shown lispm-chat-diff-lines expanded))))
         ('write
          (let ((content (split-string (string-trim-right (or (alist-get 'content input) "")) "\n")))
            (cons (lispm-tr-n "Wrote %d line to %s" "Wrote %d lines to %s" (length content)
                              (length content) path)
                  (lispm-chat--preview content lispm-chat-preview-lines expanded))))
         ('read
          ;; An image or a video read is said in its line, not counted.
          (if (or expanded (plist-get item :images)) lines
            (let ((n (cl-count-if (lambda (l) (string-match-p "\\`[ \t]*[0-9]+\t" l)) lines)))
              (list (lispm-tr-n "Read %d line" "Read %d lines" n)))))
         ('list
          (if expanded lines
            (list (lispm-tr-n "Listed %d path" "Listed %d paths" (max 0 (1- (length lines)))))))
         ('search
          (if (or expanded (string-prefix-p "No matches" result)) lines
            (list (lispm-tr-n "Found %d matching line" "Found %d matching lines"
                              (cl-count-if (lambda (l) (not (string-prefix-p "[" l))) lines)))))
         (_ (lispm-chat--preview lines lispm-chat-preview-lines expanded)))))))

(defvar lispm-chat-media-ready-functions nil
  "Abnormal hook called with FILE when async media preview generation finishes.")

(defun lispm-chat--image (file)
  "FILE, an image or a video, shown: the image, or a frame from the
middle of the video with its name under it; named where images cannot
be shown.  RET on an image visits it, on a video plays it."
  (let* ((video (eq (lispm-media-kind file) 'video))
         (name (file-name-nondirectory file))
         (shown (when (and (display-images-p) (file-readable-p file))
                  (if (not video)
                      file
                    (or (lispm-video-poster-cached file)
                        (progn
                          (lispm-video-poster-async
                           file
                           (lambda (poster)
                             (when poster
                               (run-hook-with-args 'lispm-chat-media-ready-functions file))))
                          nil))))))
    (lispm-present
     (cond ((and shown video)
            (concat (lispm-chat--picture-of shown) "\n" (lispm-tr "▶ %s" name)))
           (shown (lispm-chat--picture-of shown))
           (video (lispm-tr "[video %s]" (abbreviate-file-name file)))
           (t (lispm-tr "[image %s]" (abbreviate-file-name file))))
     (if video (list 'video file) (list 'file file nil)))))

(defun lispm-chat--media-line (line)
  "LINE shown as the image or video it links alone, where that file is;
else nil.  See `lispm-transcript-media-link'."
  (when-let* ((link (lispm-transcript-media-link line))
              (file (expand-file-name link (lispm-chat-root)))
              ((file-exists-p file)))
    (lispm-chat--image file)))

(defun lispm-chat--with-media (text)
  "TEXT with each image or video it links alone on a line shown there."
  (mapconcat (lambda (line) (or (lispm-chat--media-line line) line))
             (split-string text "\n") "\n"))

(defun lispm-chat--picture-of (file)
  "The image FILE as a string showing it, as wide as the chat lets it be."
  (propertize (format "[%s]" (file-name-nondirectory file))
              'display (create-image file nil nil
                                     :max-width (min 900 (max 300 (* (lispm-chat-width)
                                                                     (frame-char-width))))
                                     :max-height 700)))

(defun lispm-chat--image-lines (item)
  "Lines showing the images and videos of the tool call ITEM's result."
  (when-let* ((images (plist-get item :images)))
    (if (plist-get item :expanded)
        (mapcar #'lispm-chat--image images)
      (list (propertize (lispm-tr-n "%d image (TAB to show)" "%d images (TAB to show)" (length images))
                        'face 'lispm-chat-dim)))))

(defun lispm-chat--tool (item)
  "Text of the tool call ITEM."
  (let ((face (pcase (plist-get item :state)
                ("DONE" 'lispm-chat-ok)
                ((or "DENIED" "FAILED") 'lispm-chat-error)
                (_ 'lispm-chat-pending)))
        (lines (append (lispm-chat--result-lines item (plist-get item :expanded))
                       (lispm-chat--image-lines item))))
    (concat "\n"
            (let ((bullet (propertize "⏺" 'face face)))
              (if (and (plist-get item :key) lispm-chat-agent)
                  (lispm-present bullet (list 'call (symbol-name (lispm-agent-name lispm-chat-agent))
                                              (plist-get item :key)))
                bullet))
            " " (lispm-chat--tool-title item)
            (if lines
                (concat "\n" (lispm-chat-indent (lispm-chat--present (string-join lines "\n")) "     "
                                                 (propertize "  ⎿  " 'face 'lispm-chat-dim)))
              "")
            "\n")))

(defun lispm-chat--item (item)
  "Text of the display ITEM."
  (pcase (plist-get item :kind)
    ('user
     (let* ((from (plist-get item :from))
            (prefix (if from (lispm-tr "[from %s] " from) ""))
            ;; An image or a video attached is shown where its link is.
            (shown (propertize (lispm-chat-prompted (concat prefix (lispm-chat--with-media
                                                                     (plist-get item :text))))
                               'face 'lispm-chat-user)))
       (when (and from (not (equal from "LispM")) (lispm-known-agent-p from))
         (let ((at (string-search prefix shown))
               (len (length (string-trim-right prefix))))
           (lispm-present-in shown at (+ at len) (list 'agent from))))
       (concat "\n" (lispm-chat--present shown) "\n")))
    ('text
     (let ((text (plist-get item :text)))
       (concat "\n" (lispm-chat-indent (if (plist-get item :live)
                                            ;; Presented and checked once complete.
                                            (lispm-chat--streaming-markdown text)
                                          (lispm-chat--present (lispm-chat-markdown text)))
                                        "  " (concat (if lispm-chat-agent
                                                         (lispm-chat--mark lispm-chat-agent)
                                                       "⏺")
                                                     " "))
               "\n")))
    ('thought
     (let ((text (plist-get item :text)))
       (concat "\n"
               (propertize (lispm-chat-indent
                            (if (or (plist-get item :expanded) (plist-get item :live))
                                text
                              (concat (lispm-oneline text 70) "  " (lispm-tr "(TAB to show)")))
                            "  " "✻ ")
                           'face 'lispm-chat-thought)
               "\n")))
    ('summary
     (concat "\n"
             (propertize (concat "✻ " (lispm-tr "Conversation compacted")) 'face 'lispm-chat-accent)
             (if (plist-get item :expanded)
                 (concat "\n" (propertize (lispm-chat-indent (plist-get item :text) "  ")
                                          'face 'lispm-chat-dim))
               (propertize (concat " " (lispm-tr "(TAB for the summary)")) 'face 'lispm-chat-dim))
             "\n"))
    ('event
     (concat "\n"
             (propertize (lispm-chat-indent (format "%s: %s" (plist-get item :name)
                                                     (plist-get item :text))
                                             "  " "⚡ ")
                         'face 'lispm-chat-dim)
             "\n"))
    ('note
     (concat (lispm-chat--present (propertize (lispm-chat-indent (plist-get item :text) "     " "  ⎿  ")
                                              'face 'lispm-chat-dim))
             "\n"))
    ('tool (lispm-chat--tool item))))

(defun lispm-chat-prompted (text)
  "TEXT after the prompt, its other lines indented to match."
  (lispm-chat-indent text (make-string (string-width lispm-chat-prompt) ?\s)
                      lispm-chat-prompt))

(defun lispm-chat-read-only (string)
  "STRING, made part of the drawn region the user cannot edit."
  (propertize string 'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun lispm-chat--item-string (item drawn)
  "Text of ITEM as drawn, reused from the last draw while ITEM stays the
same, and put in the hash table DRAWN."
  (let ((item (append item
                      (and (gethash (plist-get item :key) lispm-chat-expanded) '(:expanded t))
                      ;; Images shown are as wide as the chat lets them be.
                      (and (plist-get item :images) (list :width (lispm-chat-width))))))
    (puthash item
             (or (gethash item lispm-chat-rendered)
                 (let ((string (lispm-chat--item item)))
                   (when-let* ((key (plist-get item :key)))
                     (setq string (propertize string 'lispm-chat-key key)))
                   (lispm-chat-read-only string)))
             drawn)))

(defun lispm-chat-item-strings (items)
  "Text of ITEMS as drawn.
Only these items are remembered afterwards: a response streaming in is a
new item at each step, and the old steps are not kept."
  (let ((drawn (make-hash-table :test #'equal)))
    (prog1 (mapcar (lambda (item) (lispm-chat--item-string item drawn)) items)
      (setq lispm-chat-rendered drawn))))

(defun lispm-chat--wrap-line (line width)
  "Parts of LINE fitting WIDTH columns, with their text properties.
Break after a space when one fits, otherwise at the display width.
Continuations keep the leading whitespace and repeat a diff's + or -
marker, or @@ for a hunk.  A context line keeps its leading spaces.
Leave room for text if the indentation itself would fill the width."
  (if (<= (string-width line) width)
      (list line)
    (string-match "\\`[ \t]*\\(?:[+-]\\|@@\\)?" line)
    (let ((prefix (truncate-string-to-width (match-string 0 line) (max 0 (- width 2))))
          parts)
      (while (> (string-width line) width)
        (let* ((end (length (truncate-string-to-width line width)))
               (space (cl-position ?\s line :from-end t :end end)))
          (when (and space (>= space (length prefix)))
            (setq end (1+ space)))
          (push (substring line 0 end) parts)
          (setq line (concat prefix (substring line end)))))
      (nreverse (cons line parts)))))

(defun lispm-chat-box (lines width &optional face)
  "LINES framed in a rounded box WIDTH columns wide, the frame in FACE.
Lines that are nil are left out.  Long lines wrap with their properties
and indentation; diff continuations repeat +, -, or @@, and context
lines keep their leading spaces."
  (let* ((face (or face 'lispm-chat-dim))
         (inner (- width 4))
         (bar (lambda (left right)
                (propertize (concat left (make-string (- width 2) ?─) right) 'face face))))
    (concat (funcall bar "╭" "╮") "\n"
            (mapconcat (lambda (line)
                         (concat (propertize "│ " 'face face) line
                                 (make-string (max 0 (- inner (string-width line))) ?\s)
                                 (propertize " │" 'face face)))
                       (mapcan (lambda (line) (and line (lispm-chat--wrap-line line inner)))
                               lines)
                       "\n")
            "\n" (funcall bar "╰" "╯") "\n")))

(defface lispm-chat-name '((t :inherit bold :height 1.3))
  "Face of the name of a character, atop its conversation."
  :group 'lispm-chat)

(defun lispm-chat--picture (agent height)
  "AGENT's character's picture, HEIGHT pixels tall, as a string showing it; or nil.
Nil too where pictures are not shown."
  (when-let* ((character (lispm-agent-character* agent))
              (file (lispm-character-avatar character))
              ((display-images-p))
              ((file-readable-p file))
              (image (create-image file nil nil :height height :ascent 'center)))
    (propertize " " 'display image)))

(defun lispm-chat--mark (agent)
  "What a reply of AGENT's starts with: its character's picture, or ⏺."
  (or (lispm-chat--picture agent (default-line-height)) "⏺"))

(defun lispm-chat--character-header (agent character)
  "The top of the chat of AGENT, a conversation of CHARACTER: who it is."
  (let* ((size (* 3 (default-line-height)))
         (picture (lispm-chat--picture agent size))
         (indent (if picture
                     (propertize " " 'display `(space :align-to (,(+ size (default-font-width)))))
                   ""))
         (dim (lambda (text) (concat indent (propertize text 'face 'lispm-chat-dim) "\n"))))
    (concat (or picture "") (if picture " " "")
            (propertize (lispm-character-title* character) 'face 'lispm-chat-name) "\n"
            (if-let* ((description (lispm-character-description character)))
                (concat indent description "\n")
              "")
            (funcall dim (concat (if (lispm-character-home-p agent)
                                     (lispm-tr "At home")
                                   (lispm-tr "In %s" (abbreviate-file-name (lispm-agent-root agent))))
                                 " · "
                                 (if (lispm-backend-name agent)
                                     (lispm-tr "model: %s via %s · role: %s"
                                               (or (lispm-agent-model* agent) "?")
                                               (lispm-backend-name agent)
                                               (lispm-agent-role agent))
                                   (lispm-tr "no model backend (see `lispm-backend') · role: %s"
                                             (lispm-agent-role agent)))))
            (funcall dim (lispm-tr "/help for help, /model to switch models")))))

(defun lispm-chat-header (agent)
  "The welcome box at the top of AGENT's chat.
A character's says who it is instead."
  (if-let* ((character (lispm-agent-character* agent)))
      (lispm-chat--character-header agent character)
    (lispm-chat--header-box agent)))

(defun lispm-chat--header-box (agent)
  "The welcome box at the top of the chat of AGENT, no character's."
  (lispm-chat-box
   (list (concat (propertize "✻ " 'face 'lispm-chat-accent)
                 (lispm-tr "Welcome to %s!" (propertize "LispM" 'face 'bold)) "  "
                 (propertize (symbol-name (lispm-agent-name agent)) 'face 'lispm-chat-dim))
         ""
         (propertize (concat "  " (lispm-tr "/help for help, /model to switch models")) 'face 'lispm-chat-dim)
         ""
         (propertize (concat "  " (lispm-tr "cwd: %s" (abbreviate-file-name (lispm-agent-root agent))))
                     'face 'lispm-chat-dim)
         (propertize (concat "  "
                             (if (lispm-backend-name agent)
                                 (lispm-tr "model: %s via %s · role: %s"
                                           (or (lispm-agent-model* agent) "?")
                                           (lispm-backend-name agent)
                                           (lispm-agent-role agent))
                               (lispm-tr "no model backend (see `lispm-backend') · role: %s"
                                         (lispm-agent-role agent))))
                     'face 'lispm-chat-dim)
         (unless (eq lispm-sandbox-level 'none)
           (propertize (concat "  " (lispm-tr "sandbox: %s (/sandbox to change)"
                                              (lispm-sandbox-level-name lispm-sandbox-level)))
                       'face 'lispm-chat-dim)))
   (min 64 (lispm-chat-width)) 'lispm-chat-accent))


(provide 'lispm-chat-items)

;;; lispm-chat-items.el ends here
