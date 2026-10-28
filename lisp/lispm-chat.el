;;; lispm-chat.el --- Talking to agents, Claude Code style  -*- lexical-binding: t; -*-

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

;; The interaction layer.  A session is stored as an org document, which
;; is also the agent's context; people talk to the agent in a chat
;; buffer instead, laid out like Claude Code's terminal interface:
;;
;;   λ >> what you asked
;;
;;   ⏺ What the agent said.
;;
;;   ⏺ Read(lispm.el)
;;     ⎿  Read 241 lines
;;
;;   ✻ Thinking… (4s · C-c C-c to interrupt)
;;   ──────────────────────────────────────
;;   λ >> your next message
;;   ──────────────────────────────────────
;;     / for commands · RET send · S-RET newline
;;
;; A / starts a command: while its name is written, the commands it
;; may be show under the input instead of the hints, as Claude Code
;; shows them; C-n and C-p choose, TAB completes, RET runs.
;;
;; The chat is drawn from the org buffer, the single source of truth,
;; whenever the session changes, from the first item that changed; the
;; org buffer itself stays out of sight.  Calls awaiting approval are
;; asked about as Claude Code asks: 1 yes, 2 yes and don't ask again
;; (for a command or any Lisp: again for that exact one), 3 no and say
;; what to do instead.  /transcript shows the org document.
;;
;; What the chat shows are objects, not only text (see
;; lispm-inspect.el): the file a call read, the file:line places and
;; org links in replies and results, the agents named, the symbols
;; written as code.  RET or a click on one acts on it: the file is
;; visited at the line, the agent inspected, the symbol described.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-memory)
(require 'lispm-inspect)
(require 'lispm-sandbox)
(require 'yank-media)
(require 'dnd)

(declare-function lispm--read-session "lispm" (prompt))
(declare-function lispm-dashboard "lispm" ())

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

(defvar-local lispm-chat--drawn nil
  "The strings shown before the prompt, in order.")

(defvar-local lispm-chat--prompt-start nil
  "Marker where the input prompt starts; everything before it is drawn.")

(defvar-local lispm-chat--input-start nil
  "Marker where the text being written starts.")

(defvar-local lispm-chat--footer nil
  "Overlay drawing the rule and hints below the input.")

(defvar-local lispm-chat--input-overlay nil
  "Overlay over the input, indenting its lines under the prompt's end.")

(defvar-local lispm-chat--expanded nil
  "Hash table of the keys of the items shown in full.")

(defvar-local lispm-chat--rendered nil
  "Hash table from the items last drawn to their text, so unchanged items
are not drawn again.")

(defvar-local lispm-chat--streamed nil
  "What of a reply streaming in is drawn already, as (TEXT SHOWN LINES CODE):
TEXT its complete lines, SHOWN them as `lispm-chat--markdown' shows them
with nothing presented, LINES non-nil when SHOWN holds a line, CODE
non-nil when a code block is open after TEXT.  Each redraw draws only
the lines that came after.")

(defvar-local lispm-chat--parsed nil
  "The items last read from the session, as a plist: :buffer and its :tick,
where the response streaming in started, :live, the :stable items before
it, and all the :items.")

(defvar-local lispm-chat--dirty 1
  "The first position of the session changed since it was last read.")

(defvar-local lispm-chat--history nil
  "Messages sent from this chat, newest first.")

(defvar-local lispm-chat--history-index nil
  "Position in `lispm-chat--history' while browsing it, or nil.")

(defcustom lispm-chat-menu-lines 10
  "Most / commands the menu under the input shows at once."
  :type 'natnum)

(defvar-local lispm-chat--menu-choice 0
  "Index of the command chosen in the menu of / commands.")

(defvar-local lispm-chat--choice 0
  "Answer selected in the approval prompt: 0, 1 or 2.")

(defvar-local lispm-chat--render-timer nil
  "Pending redraw.")

(defvar-local lispm-chat--busy-since nil
  "When the agent started working, or nil while it is idle.")

(defvar-local lispm-chat--asked nil
  "Id of the call the approval prompt was last shown for.")

(defvar-local lispm-chat--closing nil
  "Non-nil while this chat is being closed.")

(defvar lispm-chat--spinner-timer nil
  "Timer turning the spinners of busy agents.")

(defconst lispm-chat--spinner ["·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢"]
  "Frames of the working indicator.")

;;;; Reading the session

(defun lispm-chat--assistant-items (section)
  "Items of the assistant SECTION."
  (let (items)
    (dolist (part (lispm-transcript-parts section))
      (pcase (plist-get part :kind)
        ('text (push (list :kind 'text :text (plist-get part :text)) items))
        ('tool
         (push (list :kind 'tool
                     :key (plist-get part :id)
                     :name (plist-get part :name)
                     :state (plist-get part :state)
                     :input (plist-get part :input)
                     :result (plist-get part :result) :failed (plist-get part :error)
                     :preview (plist-get part :preview) :images (plist-get part :images)
                     :stopped (plist-get part :stopped))
               items))
        ('block
         (pcase (plist-get part :type)
           ((or "thinking" "reasoning")
            (let* ((block (plist-get part :block))
                   (text (and (consp block)
                              (or (alist-get 'thinking block) (alist-get 'text block)))))
              (when (and (stringp text) (not (string-blank-p text)))
                (push (list :kind 'thought :key (format "thought@%d" (plist-get part :pos))
                            :text (string-trim text))
                      items))))
           ("fallback"
            (push (list :kind 'note :text (lispm-tr "A fallback model took over this response.")) items))))))
    (nreverse items)))

(defun lispm-chat--live-items (buffer)
  "Items of the response streaming into the session BUFFER."
  (mapcar (lambda (segment)
            (pcase (plist-get segment :kind)
              ('text (list :kind 'text :text (plist-get segment :text) :live t))
              ('thinking (list :kind 'thought :key "thought@live" :text (plist-get segment :text) :live t))
              ('tool (list :kind 'tool :name (plist-get segment :name) :state "WRITING"))))
          (lispm-live-segments buffer)))

(defun lispm-chat--condition-note (section)
  "A note of the condition SECTION holds, once a restart settled it, or nil.
A condition still waiting is the prompt below the chat, not an item."
  (when-let* ((condition (lispm-transcript-section-condition section))
              (restart (plist-get condition :restart)))
    (list :kind 'note
          :text (format "%s\n↻ %s%s" (plist-get condition :message)
                        (lispm-restart-description condition restart)
                        (pcase (plist-get condition :value)
                          ('nil "")
                          (`(,backend . ,model)
                           (format ": %s%s" backend (if model (format ", %s" model) "")))
                          (value (format ": %s" value)))))))

(defun lispm-chat--section-items (beg end)
  "Items of the top-level sections of the current buffer between BEG and END."
  (let (items)
    (dolist (section (lispm-transcript-sections beg end))
      (let ((kind (plist-get section :kind)))
        (if (plist-get section :ignore)
            ;; A response left out of the conversation, after it was cut
            ;; short or refused: say it was there.
            (when (eq kind 'assistant)
              (let ((text (mapconcat (lambda (i) (or (plist-get i :text) ""))
                                     (seq-filter (lambda (i) (eq (plist-get i :kind) 'text))
                                                 (lispm-chat--assistant-items section))
                                     " ")))
                (push (list :kind 'note
                            :text (if (string-blank-p text)
                                      (lispm-tr "A response, left out of the conversation.")
                                    (lispm-tr "Left out of the conversation: %s"
                                              (lispm--oneline text 70))))
                      items)))
          (pcase kind
            ('user
             (let ((text (lispm-transcript-section-text section)))
               (unless (string-empty-p text)
                 (push (list :kind 'user :text text
                             :images (lispm-transcript--images (plist-get section :body)
                                                               (plist-get section :end)))
                       items))))
            ('message
             (push (list :kind 'user :from (plist-get section :arg)
                         :text (lispm-transcript-section-text section)
                         :images (lispm-transcript--images (plist-get section :body)
                                                           (plist-get section :end)))
                   items))
            ('event
             (push (list :kind 'event :name (plist-get section :arg)
                         :text (lispm-transcript-section-text section))
                   items))
            ('from-lispm
             (push (list :kind 'user :from "LispM" :text (lispm-transcript-section-text section))
                   items))
            ('summary
             (push (list :kind 'summary :key (format "summary@%d" (plist-get section :start))
                         :text (lispm-transcript-section-text section))
                   items))
            ('note
             (push (list :kind 'note :text (lispm-transcript-section-text section)) items))
            ('condition
             (when-let* ((note (lispm-chat--condition-note section)))
               (push note items)))
            ('assistant
             (setq items (append (reverse (lispm-chat--assistant-items section)) items)))))))
    (nreverse items)))

(defun lispm-chat--read (buffer &optional stable)
  "Items of the org session BUFFER, as (LIVE-START STABLE-ITEMS LIVE-ITEMS).
LIVE-START is where the response streaming in starts, or nil; the
stable items are those before it.  STABLE, if non-nil, are the stable
items, already known."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((live (car (lispm-live-region buffer))))
          (list live
                (or stable (lispm-chat--section-items (point-min) (or live (point-max))))
                (and live (lispm-chat--live-items buffer))))))))

(defun lispm-chat-items (buffer)
  "The conversation in the org session BUFFER, as display items."
  (apply #'append (cdr (lispm-chat--read buffer))))

(defun lispm-chat--items (agent)
  "The items of AGENT's session, read again only as far as it changed.
While a response streams in, what precedes it is not read again
unless it was edited."
  (let* ((buffer (lispm-agent-buffer agent))
         (tick (buffer-modified-tick buffer)))
    (let ((old lispm-chat--parsed))
      (if (and (eq (plist-get old :buffer) buffer) (eql (plist-get old :tick) tick))
          (plist-get old :items)
        (let* ((live-start (car (lispm-live-region buffer)))
               (reuse (and (eq (plist-get old :buffer) buffer) live-start
                           (eql (plist-get old :live) live-start)
                           (>= lispm-chat--dirty live-start)))
               (read (lispm-chat--read buffer (and reuse (plist-get old :stable))))
               (items (append (nth 1 read) (nth 2 read))))
          (setq lispm-chat--parsed (list :buffer buffer :tick tick :live (nth 0 read)
                                         :stable (nth 1 read) :items items)
                lispm-chat--dirty most-positive-fixnum)
          items)))))

;;;; Drawing items

(defun lispm-chat--width ()
  "Width to draw rules and boxes at.
That is the columns text can use beside line numbers, less one kept for
the numbers gaining a digit as the chat grows."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 40 (min 100 (1- (if window (window-max-chars-per-line window) 80))))))

(defun lispm-chat--indent (text prefix &optional first)
  "TEXT with PREFIX before each line, or FIRST before the first one.
Lines that wrap are indented by PREFIX too."
  (let ((lines (split-string text "\n")))
    (propertize (concat (or first prefix) (car lines)
                        (mapconcat (lambda (line) (concat "\n" prefix line)) (cdr lines) ""))
                'wrap-prefix (make-string (string-width prefix) ?\s))))

(defun lispm-chat--root ()
  "The directory file names in the chat are relative to: its agent's root."
  (if lispm-chat-agent (lispm-agent-root lispm-chat-agent) default-directory))

(defun lispm-chat--present (text)
  "TEXT with the objects it names presented, to act on with RET or a click."
  (lispm-present-text text (lispm-chat--root)))

(defun lispm-chat--inline (line &optional defined plain)
  "LINE of a reply with its Markdown headings, bold, italics and code shown
as faces.  Code naming a symbol or a file is presented as the object,
and code naming what this Emacs lacks is marked; DEFINED are the names
the reply defines.  PLAIN presents no code, for a reply still streaming
in: it is presented once it is complete."
  (let ((line (if (string-match "\\`#+ +\\(.*\\)\\'" line)
                  (propertize (match-string 1 line) 'face 'bold)
                line)))
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
                      (save-match-data (lispm-present-code code (lispm-chat--root) defined)))))
      line t t))))

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
  "LINE of a reply, no fence, as `lispm-chat--markdown' shows it: as code
when IN-CODE, else see `lispm-chat--inline' for DEFINED and PLAIN."
  (if in-code
      (propertize (concat "  " line) 'face 'lispm-chat-code)
    (lispm-chat--inline line defined plain)))

(defun lispm-chat--markdown (text &optional plain)
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
  "TEXT of a reply streaming in, as `lispm-chat--markdown' shows it with
nothing presented.  Its complete lines are kept as drawn, in
`lispm-chat--streamed', so each step draws only those that came since:
drawn whole, a long reply took longer at every step."
  (pcase-let* ((`(,source ,shown ,lines ,code)
                (if (and lispm-chat--streamed (string-prefix-p (car lispm-chat--streamed) text))
                    lispm-chat--streamed
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
    (setq lispm-chat--streamed (list source shown lines code))
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

(defun lispm-chat--tool-get (name property)
  "PROPERTY of the tool NAME, see `lispm-chat--tools'."
  (plist-get (cdr (assoc name lispm-chat--tools)) property))

(defun lispm-chat--tool-text (name property input preview subject)
  "The text PROPERTY of the tool NAME gives a call of INPUT and PREVIEW,
SUBJECT standing for %s, or nil when it gives none."
  (let ((value (lispm-chat--tool-get name property)))
    (cond ((functionp value) (funcall value input preview))
          (value (lispm-tr value subject)))))

(defun lispm-chat--tool-name (name)
  "The name the tool NAME is shown under; SERVER:TOOL for an MCP server's."
  (cond ((when-let* ((shown (lispm-chat--tool-get name :shown))) (lispm-tr shown)))
        ((and name (string-match "\\`mcp__\\(.+?\\)__\\(.+\\)\\'" name))
         (format "%s:%s" (match-string 1 name) (match-string 2 name)))
        (t name)))

(defun lispm-chat--argument-object (item)
  "The object the main argument of the tool call ITEM names, or nil."
  (let ((input (plist-get item :input))
        (root (lispm-chat--root)))
    (cond ((stringp (alist-get 'path input))
           (when-let* ((file (lispm-present--file (car (lispm-tools-reference (alist-get 'path input)))
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
    (concat (lispm-present (propertize (lispm-chat--tool-name (plist-get item :name))
                                       'face 'lispm-chat-tool)
                           (list 'tool (plist-get item :name)))
            (if argument
                (let ((shown (lispm--oneline argument 70))
                      (object (lispm-chat--argument-object item)))
                  (format "(%s)" (if object (lispm-present shown object) shown)))
              ""))))

(defun lispm-chat--input-diff (input)
  "A diff of the edit described by INPUT, for edits that were not previewed."
  (let ((old (or (alist-get 'old_text input) (alist-get 'old-text input)))
        (new (or (alist-get 'new_text input) (alist-get 'new-text input))))
    (when (and (stringp old) (stringp new))
      (concat (mapconcat (lambda (l) (concat "-" l)) (split-string old "\n") "\n") "\n"
              (mapconcat (lambda (l) (concat "+" l)) (split-string new "\n") "\n")))))

(defun lispm-chat--diff-lines (diff)
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
       (pcase (lispm-chat--tool-get name :result)
         ((and 'definition (guard (not (plist-get item :preview))))
          ;; Run unasked, it has no diff: its input holds only the new form.
          (let ((new (split-string (string-trim-right (or (alist-get 'new_text input) "")) "\n")))
            (cons (lispm-tr "Redefined %s" (alist-get 'name input))
                  (lispm-chat--preview new lispm-chat-preview-lines expanded))))
         ((or 'edit 'definition)
          (let* ((diff (or (plist-get item :preview) (lispm-chat--input-diff input)))
                 (shown (lispm-chat--diff-lines diff)))
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

(defun lispm-chat--image (file)
  "FILE, an image or a video, shown: the image, or a frame from the
middle of the video with its name under it; named where images cannot
be shown.  RET on an image visits it, on a video plays it."
  (let* ((video (eq (lispm-media-kind file) 'video))
         (name (file-name-nondirectory file))
         (shown (and (display-images-p) (file-readable-p file)
                     (if video (lispm-video-poster file) file))))
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
              (file (expand-file-name link (lispm-chat--root)))
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
                                     :max-width (min 900 (max 300 (* (lispm-chat--width)
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
                (concat "\n" (lispm-chat--indent (lispm-chat--present (string-join lines "\n")) "     "
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
            (shown (propertize (lispm-chat--prompted (concat prefix (lispm-chat--with-media
                                                                     (plist-get item :text))))
                               'face 'lispm-chat-user)))
       (when (and from (lispm-get-agent from t))
         (let ((at (+ (string-search prefix shown) (string-search from prefix))))
           (lispm-present-in shown at (+ at (length from)) (list 'agent from))))
       (concat "\n" (lispm-chat--present shown) "\n")))
    ('text
     (let ((text (plist-get item :text)))
       (concat "\n" (lispm-chat--indent (if (plist-get item :live)
                                            ;; Presented and checked once complete.
                                            (lispm-chat--streaming-markdown text)
                                          (lispm-chat--present (lispm-chat--markdown text)))
                                        "  " (concat (if lispm-chat-agent
                                                         (lispm-chat--mark lispm-chat-agent)
                                                       "⏺")
                                                     " "))
               "\n")))
    ('thought
     (let ((text (plist-get item :text)))
       (concat "\n"
               (propertize (lispm-chat--indent
                            (if (or (plist-get item :expanded) (plist-get item :live))
                                text
                              (concat (lispm--oneline text 70) "  " (lispm-tr "(TAB to show)")))
                            "  " "✻ ")
                           'face 'lispm-chat-thought)
               "\n")))
    ('summary
     (concat "\n"
             (propertize (concat "✻ " (lispm-tr "Conversation compacted")) 'face 'lispm-chat-accent)
             (if (plist-get item :expanded)
                 (concat "\n" (propertize (lispm-chat--indent (plist-get item :text) "  ")
                                          'face 'lispm-chat-dim))
               (propertize (concat " " (lispm-tr "(TAB for the summary)")) 'face 'lispm-chat-dim))
             "\n"))
    ('event
     (concat "\n"
             (propertize (lispm-chat--indent (format "%s: %s" (plist-get item :name)
                                                     (plist-get item :text))
                                             "  " "⚡ ")
                         'face 'lispm-chat-dim)
             "\n"))
    ('note
     (concat (lispm-chat--present (propertize (lispm-chat--indent (plist-get item :text) "     " "  ⎿  ")
                                              'face 'lispm-chat-dim))
             "\n"))
    ('tool (lispm-chat--tool item))))

(defun lispm-chat--prompted (text)
  "TEXT after the prompt, its other lines indented to match."
  (lispm-chat--indent text (make-string (string-width lispm-chat-prompt) ?\s)
                      lispm-chat-prompt))

(defun lispm-chat--read-only (string)
  "STRING, made part of the drawn region the user cannot edit."
  (propertize string 'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun lispm-chat--item-string (item drawn)
  "Text of ITEM as drawn, reused from the last draw while ITEM stays the
same, and put in the hash table DRAWN."
  (let ((item (append item
                      (and (gethash (plist-get item :key) lispm-chat--expanded) '(:expanded t))
                      ;; Images shown are as wide as the chat lets them be.
                      (and (plist-get item :images) (list :width (lispm-chat--width))))))
    (puthash item
             (or (gethash item lispm-chat--rendered)
                 (let ((string (lispm-chat--item item)))
                   (when-let* ((key (plist-get item :key)))
                     (setq string (propertize string 'lispm-chat-key key)))
                   (lispm-chat--read-only string)))
             drawn)))

(defun lispm-chat--item-strings (items)
  "Text of ITEMS as drawn.
Only these items are remembered afterwards: a response streaming in is a
new item at each step, and the old steps are not kept."
  (let ((drawn (make-hash-table :test #'equal)))
    (prog1 (mapcar (lambda (item) (lispm-chat--item-string item drawn)) items)
      (setq lispm-chat--rendered drawn))))

(defun lispm-chat--box (lines width &optional face)
  "LINES framed in a rounded box WIDTH columns wide, the frame in FACE.
Lines that are nil are left out."
  (let* ((face (or face 'lispm-chat-dim))
         (inner (- width 4))
         (bar (lambda (left right)
                (propertize (concat left (make-string (- width 2) ?─) right) 'face face))))
    (concat (funcall bar "╭" "╮") "\n"
            (mapconcat (lambda (line)
                         (let ((line (truncate-string-to-width line inner nil nil "…")))
                           (concat (propertize "│ " 'face face) line
                                   (make-string (max 0 (- inner (string-width line))) ?\s)
                                   (propertize " │" 'face face))))
                       (delq nil (copy-sequence lines)) "\n")
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

(defun lispm-chat--header (agent)
  "The welcome box at the top of AGENT's chat.
A character's says who it is instead."
  (if-let* ((character (lispm-agent-character* agent)))
      (lispm-chat--character-header agent character)
    (lispm-chat--header-box agent)))

(defun lispm-chat--header-box (agent)
  "The welcome box at the top of the chat of AGENT, no character's."
  (lispm-chat--box
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
   (min 64 (lispm-chat--width)) 'lispm-chat-accent))

;;;; Approval

(defun lispm-chat--waiting ()
  "Id of the call the approval prompt asks about."
  (or (car (lispm-waiting-calls lispm-chat-agent))
      (user-error "%s" (lispm-tr "Nothing awaits your approval"))))

(defun lispm-chat--condition (&optional agent)
  "The condition the prompt asks about, AGENT's or a call's, if any.
See `lispm-asked-condition'."
  (let ((agent (or agent lispm-chat-agent)))
    (and agent (lispm-asked-condition agent))))

(defun lispm-chat--prompting-p ()
  "Non-nil if the chat asks something: a call's approval, or a restart."
  (and lispm-chat-agent (lispm-awaiting lispm-chat-agent)))

(defun lispm-chat-restart (restart)
  "Go on from the condition the prompt asks about by RESTART."
  (lispm-choose-restart-named lispm-chat-agent restart (lispm-chat--condition))
  (goto-char (point-max)))

(defun lispm-chat--answer (n)
  "Give the Nth answer, from 1, to the prompt; non-nil if it has one."
  (if-let* ((condition (lispm-chat--condition)))
      (when-let* ((restart (nth (1- n) (plist-get condition :restarts))))
        (lispm-chat-restart restart)
        t)
    (when-let* (((lispm-waiting-calls lispm-chat-agent))
                (command (nth (1- n) '(lispm-chat-yes lispm-chat-yes-always lispm-chat-no))))
      (call-interactively command)
      t)))

(defun lispm-chat-yes ()
  "Approve the call the prompt asks about, or take the condition's default restart."
  (interactive)
  (lispm-approve-for lispm-chat-agent)
  (goto-char (point-max)))

(defun lispm-chat-yes-always ()
  "Approve the call, and every later call of its tool by this agent.
For a tool that runs any Lisp, or commands that can reach Emacs, which
is any command on this machine outside the sandbox, only this exact
call (see `lispm-tools-allowance')."
  (interactive)
  (lispm-allow lispm-chat-agent (lispm-chat--waiting))
  (goto-char (point-max)))

(defun lispm-chat-no ()
  "Deny the call, saying what to do instead; or stop at the condition."
  (interactive)
  (lispm-deny-for lispm-chat-agent
                  (unless (lispm-chat--condition)
                    (lispm-chat--waiting)
                    (read-string (lispm-tr "What should it do instead? (optional) "))))
  (goto-char (point-max)))

(defun lispm-chat-choose ()
  "Give the selected answer to the prompt."
  (interactive)
  (lispm-chat--answer (1+ lispm-chat--choice)))

(defun lispm-chat--answers ()
  "How many answers the prompt offers."
  (if-let* ((condition (lispm-chat--condition)))
      (length (plist-get condition :restarts))
    3))

(defun lispm-chat-choice-next (&optional n)
  "Select the next answer in the prompt, or the Nth after."
  (interactive "p")
  (setq lispm-chat--choice (mod (+ lispm-chat--choice (or n 1)) (lispm-chat--answers)))
  (lispm-chat--redraw))

(defun lispm-chat-choice-previous ()
  "Select the previous answer in the approval prompt."
  (interactive)
  (lispm-chat-choice-next -1))

(defun lispm-chat--answer-command (n)
  "A command giving the Nth answer to the prompt."
  (lambda ()
    (interactive)
    (lispm-chat--answer n)))

(defvar-keymap lispm-chat-approval-map
  :doc "Keys of the approval prompt, and of the prompt for a restart."
  "1" (lispm-chat--answer-command 1)
  "2" (lispm-chat--answer-command 2)
  "3" (lispm-chat--answer-command 3)
  "4" (lispm-chat--answer-command 4)
  "5" (lispm-chat--answer-command 5)
  "y" #'lispm-chat-yes
  "n" #'lispm-chat-no
  "RET" #'lispm-chat-choose
  "<up>" #'lispm-chat-choice-previous
  "<down>" #'lispm-chat-choice-next)

(defun lispm-chat--options (options)
  "Lines offering OPTIONS, numbered from 1, the one selected marked."
  (cl-loop for option in options
           for i from 0
           collect (if (= i lispm-chat--choice)
                       (propertize (format "❯ %d. %s" (1+ i) option) 'face 'lispm-chat-accent)
                     (format "  %d. %s" (1+ i) option))))

(defun lispm-chat--approval (agent items)
  "The prompt for AGENT's first call awaiting approval, or nil.
ITEMS are the session's items, where the call's preview is."
  (when-let* (;; Not while a call before it, stopped at an error, asks
              ;; for a restart.
              ((not (lispm-asked-condition agent)))
              (id (car (lispm-waiting-calls agent)))
              (call (gethash id (lispm-agent-pending agent))))
    (let* ((asked (lispm-asked call))
           (name (plist-get asked :name))
           (input (and (consp (plist-get asked :input)) (plist-get asked :input)))
           (shown (lispm-chat--tool-name name))
           (path (or (alist-get 'path input) (alist-get 'buffer input)))
           (why (alist-get 'why input))
           (item (seq-find (lambda (i) (equal (plist-get i :key) id)) items))
           (preview (plist-get item :preview))
           (subject (if (eq (lispm-chat--tool-get name :subject) 'name) (alist-get 'name input) path))
           (body (pcase (lispm-chat--tool-get name :ask)
                   ('diff (lispm-chat--diff-lines (or preview (lispm-chat--input-diff input))))
                   ;; The definition, as the preview quotes it from its file.
                   ('preview (split-string (or preview "") "\n"))
                   ;; The file a skill would be, or a project's skill's code;
                   ;; a preview that failed says why.
                   ('skill (or (lispm-chat--diff-lines preview)
                               (and preview (split-string (string-trim preview) "\n"))))
                   ('command (split-string (or (alist-get 'command input) "") "\n"))
                   ('code (split-string (or (alist-get 'code input) "") "\n"))
                   (_ (split-string (lispm--json-pretty input) "\n"))))
           (title (concat (or (lispm-chat--tool-text name :title input preview subject)
                              (lispm-tr "Use %s" shown))
                          (if (and (plist-get call :asking) (not (lispm-chat--tool-get name :alone)))
                              (concat " · " (lispm-tr "in the program %s runs"
                                                      (lispm-chat--tool-name (plist-get call :name))))
                            "")))
           (question (or (lispm-chat--tool-text name :question input preview subject)
                         (lispm-tr "Do you want to proceed?")))
           ;; What saying yes leads to, beyond the call itself.
           (note (lispm-chat--tool-text name :note input preview subject))
           (options (list (lispm-tr "Yes")
                          (pcase (lispm-call-allowance agent id)
                            (`(always . ,text) (lispm-tr text))
                            ('exact (lispm-tr (or (lispm-chat--tool-get name :exact)
                                                  (lispm-tr-noop "Yes, and don't ask again for this exact call"))))
                            (_ (lispm-tr "Yes, and don't ask again for %s calls by this agent" shown)))
                          (lispm-tr "No, and tell it what to do differently"))))
      (propertize
       (concat
        "\n"
        (lispm-chat--box
         (append (list (propertize title 'face 'bold) "")
                 (mapcar (lambda (l) (concat "  " l)) (seq-take body 30))
                 (and (> (length body) 30)
                      (list (propertize (concat "  " (lispm-tr-n "… +%d line" "… +%d lines" (- (length body) 30)))
                                        'face 'lispm-chat-dim)))
                 ;; Why a check of this Emacs has it ask.
                 (mapcan (lambda (reason)
                           (cons "" (mapcar (lambda (line)
                                              (propertize (concat "  " line) 'face 'lispm-chat-pending))
                                            (split-string (string-fill reason (- (lispm-chat--width) 8))
                                                          "\n"))))
                         (plist-get asked :reasons))
                 (and (stringp why) (list "" (propertize (concat "  " why) 'face 'lispm-chat-dim)))
                 (and note
                      (cons "" (mapcar (lambda (line)
                                         (propertize (concat "  " line) 'face 'lispm-chat-dim))
                                       (split-string (string-fill note (- (lispm-chat--width) 8))
                                                     "\n"))))
                 (list "" question)
                 (lispm-chat--options options))
         (lispm-chat--width) 'lispm-chat-pending))
       'keymap lispm-chat-approval-map))))

(defun lispm-chat--condition-prompt (agent)
  "The prompt for a restart of the condition AGENT's turn waits at, or nil."
  (when-let* ((condition (lispm-chat--condition agent)))
    (let ((width (lispm-chat--width)))
      (propertize
       (concat
        "\n"
        (lispm-chat--box
         (append (list (propertize (lispm-tr (plist-get condition :title)) 'face 'bold) "")
                 (mapcar (lambda (l) (concat "  " l))
                         (seq-take (split-string (string-fill (plist-get condition :message)
                                                              (- width 8))
                                                 "\n")
                                   12))
                 (list "" (lispm-tr "How do you want to go on?"))
                 (lispm-chat--options (mapcar (lambda (restart) (lispm-restart-description condition restart))
                                              (plist-get condition :restarts))))
         width 'lispm-chat-pending))
       'keymap lispm-chat-approval-map))))

;;;; Status

(defun lispm-chat--status (agent)
  "The line saying what AGENT is doing, or nil when it is not working."
  (let ((state (lispm-agent-state agent)))
    (when (lispm-agent-working-p agent)
      (let* ((seconds (if lispm-chat--busy-since
                          (floor (- (float-time) lispm-chat--busy-since))
                        0))
             (frame (aref lispm-chat--spinner
                          (mod (floor (* 4 (float-time))) (length lispm-chat--spinner))))
             (status (or (lispm-agent-status agent) ""))
             (verb (cond ((eq state 'compacting) (lispm-tr "Compacting conversation…"))
                         ((eq state 'aborting) (lispm-tr "Stopping…"))
                         (t (pcase (lispm-agent-activity agent)
                              (`(running . ,tool) (lispm-tr "Running %s…" tool))
                              (`(writing . ,tool) (lispm-tr "Writing a call of %s…" tool))
                              ('retry (lispm-tr "Thinking… (%s)" status))
                              ((and 'waiting (guard (not (string-empty-p status))))
                               (concat (upcase (substring status 0 1)) (substring status 1) "…"))
                              (_ (lispm-tr "Thinking…")))))))
        (concat "\n"
                (propertize (concat frame " " verb) 'face 'lispm-chat-accent)
                (propertize (concat " " (lispm-tr "(%ds · C-c C-c to interrupt)" seconds)) 'face 'lispm-chat-dim)
                "\n")))))

(defun lispm-chat--queued (agent)
  "The inputs waiting for AGENT, or nil."
  (when-let* ((inbox (lispm-agent-inbox agent)))
    (concat "\n"
            (mapconcat (lambda (entry)
                         (propertize (lispm-chat--prompted
                                      (concat (cond ((plist-get entry :from)
                                                     (lispm-tr "[from %s] " (plist-get entry :from)))
                                                    ((plist-get entry :event)
                                                     (format "[%s] " (plist-get entry :event)))
                                                    (t ""))
                                              (plist-get entry :text)))
                                     'face 'lispm-chat-dim))
                       inbox "\n")
            (propertize (concat "\n  ⎿  " (lispm-tr "queued; sent when the agent is free")) 'face 'lispm-chat-dim)
            "\n")))

(defun lispm-chat--rule ()
  "A rule across the chat."
  (propertize (make-string (lispm-chat--width) ?─) 'face 'lispm-chat-dim))

(defun lispm-chat--strings (agent)
  "Everything AGENT's chat shows before the prompt, as a list of strings."
  (let ((items (lispm-chat--items agent)))
    (append (list (lispm-chat--read-only (lispm-chat--header agent)))
            (lispm-chat--item-strings items)
            (mapcar #'lispm-chat--read-only
                    (delq nil (list (lispm-chat--queued agent)
                                    (or (lispm-chat--approval agent items)
                                        (lispm-chat--condition-prompt agent))
                                    (lispm-chat--status agent)
                                    (concat "\n" (lispm-chat--rule) "\n")))))))

;;;; Drawing

(defun lispm-chat--redraw ()
  "Bring the current chat up to date, from the first string that changed."
  (let* ((strings (lispm-chat--strings lispm-chat-agent))
         (old lispm-chat--drawn)
         (new strings)
         (pos (point-min)))
    (while (and old new (equal-including-properties (car old) (car new)))
      (setq pos (+ pos (length (car old))) old (cdr old) new (cdr new)))
    (when (or old new)
      (let* ((inhibit-read-only t)
             (inhibit-modification-hooks t)
             (buffer-undo-list t)
             (windows (mapcar (lambda (w)
                                (let ((p (window-point w)))
                                  (list w (>= p lispm-chat--input-start) (- (point-max) p))))
                              (get-buffer-window-list (current-buffer) nil t)))
             (in-input (>= (point) lispm-chat--input-start))
             (from-end (- (point-max) (point)))
             (here (point)))
        (save-excursion
          (delete-region pos lispm-chat--prompt-start)
          (goto-char pos)
          (apply #'insert new))
        (goto-char (if in-input (- (point-max) from-end) (min here lispm-chat--prompt-start)))
        (pcase-dolist (`(,w ,input ,end) windows)
          (if (not input)
              (set-window-point w (min (window-point w) lispm-chat--prompt-start))
            (set-window-point w (- (point-max) end))
            (lispm-chat--follow w))))
      ;; Positions recorded for undoing input edits are stale now.
      (setq buffer-undo-list nil))
    (setq lispm-chat--drawn strings)
    (lispm-chat--update-footer)
    (lispm-chat--maybe-ask)))

(defun lispm-chat--follow (window &optional lines)
  "Scroll WINDOW so that the input, and the LINES (3) below it, are in sight:
the hints, or the menu of / commands."
  (let ((posn (posn-at-point (point-max) window))
        (lines (or lines 3)))
    (when (or (null posn)
              (> (+ (cdr (posn-x-y posn)) (* lines (default-line-height)))
                 (window-body-height window t)))
      (with-selected-window window
        (recenter (- lines))))))

(defun lispm-chat--maybe-ask ()
  "Select the first answer of a newly shown prompt."
  ;; A condition itself, or a call's id: a condition's id can be given again.
  (let ((id (or (lispm-asked-condition lispm-chat-agent)
                (car (lispm-awaiting lispm-chat-agent)))))
    (unless (equal id lispm-chat--asked)
      (setq lispm-chat--asked id
            lispm-chat--choice 0))))

(defun lispm-chat-redraw (chat)
  "Redraw CHAT now."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (setq lispm-chat--render-timer nil)
      (when (and lispm-chat-agent (buffer-live-p (lispm-agent-buffer lispm-chat-agent)))
        (lispm-chat--redraw)))))

(defun lispm-chat--schedule (agent)
  "Redraw AGENT's chat shortly, once for a burst of changes."
  (let ((chat (and agent (lispm-agent-view agent))))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (unless (timerp lispm-chat--render-timer)
          (setq lispm-chat--render-timer (run-at-time 0.05 nil #'lispm-chat-redraw chat)))))))

(defun lispm-chat--state-changed (agent)
  "Note when AGENT starts or stops working, and redraw its chat."
  (let ((chat (lispm-agent-view agent)))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (if (not (lispm-agent-working-p agent))
            (setq lispm-chat--busy-since nil)
          (unless lispm-chat--busy-since
            (setq lispm-chat--busy-since (float-time)))))
      (lispm-chat--animate)
      (lispm-chat--schedule agent))))

(defun lispm-chat--busy-agents ()
  "The agents working while their chat is open."
  (seq-filter (lambda (a) (and (buffer-live-p (lispm-agent-view a))
                               (lispm-agent-working-p a)))
              (lispm-agents)))

(defun lispm-chat--animate ()
  "Keep the spinners of working agents turning, and stop when none is."
  (let ((busy (lispm-chat--busy-agents)))
    (cond ((and busy (not (timerp lispm-chat--spinner-timer)))
           (setq lispm-chat--spinner-timer
                 (run-at-time 0.25 0.25 (lambda ()
                                          (mapc #'lispm-chat--schedule (lispm-chat--busy-agents))
                                          (lispm-chat--animate)))))
          ((and (null busy) (timerp lispm-chat--spinner-timer))
           (cancel-timer lispm-chat--spinner-timer)
           (setq lispm-chat--spinner-timer nil)))))

(defun lispm-chat--transcript-changed (beg _end _length)
  "Note that the session in the current buffer changed from BEG; redraw its chat."
  (let ((chat (and lispm-session-agent (lispm-agent-view lispm-session-agent))))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (setq lispm-chat--dirty (min lispm-chat--dirty beg)))
      (lispm-chat--schedule lispm-session-agent))))

(defun lispm-chat--hide-transcript (buffer)
  "Give the org BUFFER a name that keeps it out of buffer lists."
  (with-current-buffer buffer
    (unless (string-prefix-p " " (buffer-name))
      (rename-buffer (format " *lispm transcript %s*" (file-name-base buffer-file-name)) t))))

(defun lispm-chat--watch-transcript ()
  "Redraw the chat when the session's org buffer changes; keep that buffer hidden."
  (when lispm-session-mode
    (add-hook 'after-change-functions #'lispm-chat--transcript-changed nil t)
    (when (and lispm-session-agent (buffer-live-p (lispm-agent-view lispm-session-agent)))
      (lispm-chat--hide-transcript (current-buffer)))))

(defun lispm-chat--session-ended (agent)
  "Close the chat of AGENT, whose session has closed."
  (let ((chat (lispm-agent-view agent)))
    (when (buffer-live-p chat)
      (setf (lispm-agent-view agent) nil)
      (with-current-buffer chat (setq lispm-chat--closing t))
      (kill-buffer chat))))

(defun lispm-chat--sandbox-changed ()
  "Show the sandbox level now set in every chat."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
        (lispm-chat--redraw)))))

(defun lispm-chat--draw-all-again ()
  "Draw every chat again, whole: in the language now spoken, with code
checked or not as `lispm-ground-code' now says."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
        (setq lispm-chat--parsed nil
              lispm-chat--streamed nil)
        (clrhash lispm-chat--rendered)
        (lispm-chat--redraw)))))

(add-hook 'lispm-session-mode-hook #'lispm-chat--watch-transcript)
(add-hook 'lispm-state-change-functions #'lispm-chat--state-changed)
(add-hook 'lispm-session-end-functions #'lispm-chat--session-ended)
(add-hook 'lispm-sandbox-change-hook #'lispm-chat--sandbox-changed)
(add-hook 'lispm-language-changed-hook #'lispm-chat--draw-all-again)
(add-hook 'lispm-ground-change-hook #'lispm-chat--draw-all-again)

;;;; Input

(defun lispm-chat--input ()
  "The text being written."
  (buffer-substring-no-properties lispm-chat--input-start (point-max)))

(defun lispm-chat-set-input (text)
  "Replace the text being written with TEXT."
  (let ((inhibit-read-only t))
    (delete-region lispm-chat--input-start (point-max))
    (goto-char (point-max))
    (insert text)))

(defun lispm-chat--in-input-p ()
  "Non-nil if point is in the text being written."
  (>= (point) lispm-chat--input-start))

(defvar lispm-chat-commands)

(defun lispm-chat--menu ()
  "The / commands the input may be, while it is a / and the start of a
command's name: those whose name starts so, then those holding it
elsewhere, as entries of `lispm-chat-commands'.  Nil otherwise, and
then no menu shows."
  (when (and (markerp lispm-chat--input-start) (marker-position lispm-chat--input-start))
    (let ((input (lispm-chat--input)))
      (when (string-match "\\`/\\([a-z]*\\)\\'" input)
        (let* ((word (match-string 1 input))
               (starting (seq-filter (lambda (command) (string-prefix-p word (car command)))
                                     lispm-chat-commands)))
          (append starting
                  (seq-filter (lambda (command)
                                (and (not (memq command starting)) (string-search word (car command))))
                              lispm-chat-commands)))))))

(defun lispm-chat--menu-chosen (menu)
  "The index of the command chosen in MENU."
  (min lispm-chat--menu-choice (1- (length menu))))

(defun lispm-chat--menu-command (menu)
  "The name of the command chosen in MENU."
  (car (nth (lispm-chat--menu-chosen menu) menu)))

(defun lispm-chat--menu-lines (menu)
  "MENU as it shows under the input, the command chosen marked; at most
`lispm-chat-menu-lines' of it, around the one chosen."
  (let* ((chosen (lispm-chat--menu-chosen menu))
         (start (max 0 (- chosen (1- lispm-chat-menu-lines))))
         (width (+ 2 (apply #'max (mapcar (lambda (command) (string-width (car command)))
                                          lispm-chat-commands)))))
    (mapconcat (lambda (index)
                 (pcase-let* ((`(,name ,_ ,doc) (nth index menu))
                              (line (concat "/" name (make-string (- width (string-width name)) ?\s)
                                            (lispm-tr doc))))
                   (if (= index chosen)
                       (propertize (concat "❯ " line) 'face 'lispm-chat-accent)
                     (concat "  " (propertize line 'face 'lispm-chat-dim)))))
               (number-sequence start (min (1- (length menu)) (+ start lispm-chat-menu-lines -1)))
               "\n")))

(defun lispm-chat--menu-move (n)
  "Choose the command N lines down the menu, from the bottom to the top."
  (let ((menu (lispm-chat--menu)))
    (setq lispm-chat--menu-choice (mod (+ (lispm-chat--menu-chosen menu) n) (length menu)))
    (lispm-chat--update-footer)))

(defun lispm-chat--show-menu ()
  "Scroll the menu under the input into sight, while there is one."
  (when-let* (((lispm-chat--in-input-p))
              (menu (lispm-chat--menu))
              (window (get-buffer-window (current-buffer))))
    (lispm-chat--follow window (+ 3 (min (length menu) lispm-chat-menu-lines)))))

(defun lispm-chat--update-footer (&rest change)
  "Draw the rule and hints below the input, and a placeholder while it is empty.
While a / command's name is written, the commands it may be show in
place of the hints.  Run on a CHANGE of the input, the first of them is
chosen again."
  (when change
    (setq lispm-chat--menu-choice 0))
  (when (overlayp lispm-chat--footer)
    (let* ((menu (lispm-chat--menu))
           (footer
           (concat (if (and lispm-chat--input-start (= lispm-chat--input-start (point-max)))
                       (propertize (cond ((not (lispm-chat--prompting-p))
                                          (lispm-tr "Try \"explain this project\", or / for commands"))
                                         ((lispm-chat--condition)
                                          (lispm-tr "Choose above with its number, or write to stop and send"))
                                         (t (lispm-tr "Answer above with 1, 2 or 3")))
                                   'face 'lispm-chat-dim)
                     " ")
                   "\n" (lispm-chat--rule) "\n"
                   (if menu
                       (lispm-chat--menu-lines menu)
                     (propertize (concat "  " (if (display-graphic-p)
                                                (lispm-tr "/ for commands · RET send · S-RET newline · TAB expand · C-c C-c interrupt")
                                              ;; A text terminal sends Return alike with Shift held.
                                              (lispm-tr "/ for commands · RET send · C-j newline · TAB expand · C-c C-c interrupt")))
                                 'face 'lispm-chat-dim)))))
      ;; The footer is drawn at the end of the input, where point usually
      ;; is, and Emacs would draw the cursor after it, below the hints.
      ;; Put it on the footer's first character instead: the placeholder,
      ;; or a space (a newline holds no cursor).
      (put-text-property 0 1 'cursor t footer)
      ;; Its lines are not the input's: no indentation under the prompt.
      (add-text-properties 0 (length footer) '(line-prefix "" wrap-prefix "") footer)
      (overlay-put lispm-chat--footer 'after-string footer))))

(defun lispm-chat-return ()
  "Send what is written, or answer the approval prompt if nothing is.
While the menu of / commands shows, run the command chosen there.
Outside the input, act on the object at point (see lispm-inspect.el),
or show the item at point in full, or act as one of
`lispm-act-functions' does there."
  (interactive)
  (cond ((not (lispm-chat--in-input-p))
         (cond ((lispm-object-at) (lispm-act-at-point))
               ((get-text-property (point) 'lispm-chat-key) (lispm-chat-toggle))
               ((run-hook-with-args-until-success 'lispm-act-functions))
               (t (goto-char (point-max)))))
        ((and (string-blank-p (lispm-chat--input)) (lispm-chat--prompting-p))
         (lispm-chat-choose))
        (t (when-let* ((menu (lispm-chat--menu)))
             (lispm-chat-set-input (concat "/" (lispm-chat--menu-command menu))))
           (lispm-chat-send))))

(defun lispm-chat-send ()
  "Send what is written to the agent, or run it as a / command."
  (interactive)
  (let ((text (string-trim (lispm-chat--input)))
        (agent lispm-chat-agent))
    (unless (string-empty-p text)
      (push text lispm-chat--history)
      (setq lispm-chat--history-index nil)
      (lispm-chat-set-input "")
      (if (string-match-p "\\`/[a-z]" text)
          (lispm-chat--command agent text)
        ;; Writing while the turn waits at a condition stops it there,
        ;; as writing after a failed turn always did.
        (lispm-stop-at-condition agent)
        (lispm-tell agent text)
        (lispm-chat--schedule agent)))))

(defun lispm-chat-attach (file)
  "Attach FILE to the message being written: its link goes on a line of
its own, so the agent knows the file; an image or a video the agent
sees, and the chat shows.  Interactively, FILE is asked for."
  (interactive (list (read-file-name (lispm-tr "Attach file: ") nil nil t)) lispm-chat-mode)
  (unless (lispm-chat--in-input-p) (goto-char (point-max)))
  (let ((input (lispm-chat--input))
        (file (expand-file-name file))
        (root (lispm-chat--root)))
    (goto-char (point-max))
    (insert (if (or (string-empty-p input) (string-suffix-p "\n" input)) "" "\n")
            ;; Named from the root, as the agent names files, when under it.
            (format "[[file:%s]]\n" (if (file-in-directory-p file root)
                                        (file-relative-name file root)
                                      (abbreviate-file-name file))))))

(defun lispm-chat--yank-image (type data)
  "Attach the image DATA, pasted, of media TYPE to the message being written.
It is saved in the agent's session, as the images tools return are;
one of a kind models do not take, TIFF say, is made a PNG first."
  (let* ((type (symbol-name type))
         (file (if (rassoc type lispm-transcript--image-types)
                   (lispm-save-image lispm-chat-agent (base64-encode-string data t) type)
                 (let ((pasted (make-temp-file "lispm-pasted-" nil
                                               (concat "." (cadr (split-string type "/"))))))
                   (with-temp-file pasted
                     (set-buffer-multibyte nil)
                     (insert data))
                   (unwind-protect
                       (if-let* ((png (lispm-image-as-png pasted)))
                           (unwind-protect
                               (lispm-save-image lispm-chat-agent
                                                 (with-temp-buffer
                                                   (set-buffer-multibyte nil)
                                                   (insert-file-contents-literally png)
                                                   (base64-encode-string (buffer-string) t))
                                                 "image/png")
                             (delete-file png))
                         (user-error "%s" (lispm-tr "Cannot attach a %s image" type)))
                     (delete-file pasted))))))
    (lispm-chat-attach file)))

(defun lispm-chat--drop (url _action)
  "Attach the file dropped on the chat, URL, to the message being written."
  (when-let* ((file (dnd-get-local-file-name url t)))
    (lispm-chat-attach file)
    'private))

(defun lispm-chat-attach-command (_agent argument)
  "Attach the file ARGUMENT names, asked for if none, to the next message."
  (if (string-empty-p argument)
      (call-interactively #'lispm-chat-attach)
    (lispm-chat-attach (expand-file-name argument (lispm-chat--root)))))

(defun lispm-chat-newline ()
  "Start a new line in the text being written."
  (interactive)
  (unless (lispm-chat--in-input-p) (goto-char (point-max)))
  (insert "\n"))

(defun lispm-chat-self-insert ()
  "Type into the input, wherever point is."
  (interactive)
  (unless (lispm-chat--in-input-p) (goto-char (point-max)))
  (call-interactively #'self-insert-command))

(defun lispm-chat--answer-key (n)
  "A command giving the Nth answer to the prompt shown, else typing."
  (lambda ()
    (interactive)
    (unless (and (string-empty-p (lispm-chat--input)) (lispm-chat--answer n))
      (lispm-chat-self-insert))))

(defun lispm-chat-previous ()
  "Choose the command above in the menu of / commands, or select the
previous answer of an approval prompt, or move up a line."
  (interactive)
  (cond ((and (lispm-chat--in-input-p) (lispm-chat--menu))
         (lispm-chat--menu-move -1))
        ((and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
              (lispm-chat--prompting-p))
         (lispm-chat-choice-previous))
        (t (call-interactively #'previous-line))))

(defun lispm-chat-next ()
  "Choose the command below in the menu of / commands, or select the
next answer of an approval prompt, or move down a line."
  (interactive)
  (cond ((and (lispm-chat--in-input-p) (lispm-chat--menu))
         (lispm-chat--menu-move 1))
        ((and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
              (lispm-chat--prompting-p))
         (lispm-chat-choice-next))
        (t (call-interactively #'next-line))))

(defun lispm-chat-history-previous ()
  "Replace what is written with the previous message sent."
  (interactive)
  (when lispm-chat--history
    (setq lispm-chat--history-index
          (min (1- (length lispm-chat--history))
               (if lispm-chat--history-index (1+ lispm-chat--history-index) 0)))
    (lispm-chat-set-input (nth lispm-chat--history-index lispm-chat--history))))

(defun lispm-chat-history-next ()
  "Replace what is written with the next message sent, or clear it."
  (interactive)
  (if (and lispm-chat--history-index (> lispm-chat--history-index 0))
      (progn (cl-decf lispm-chat--history-index)
             (lispm-chat-set-input (nth lispm-chat--history-index lispm-chat--history)))
    (setq lispm-chat--history-index nil)
    (lispm-chat-set-input "")))

(defun lispm-chat-toggle ()
  "Show the item at point in full, or briefly again; in the input, complete:
while the menu of / commands shows, to the command chosen there."
  (interactive)
  (let ((menu (and (lispm-chat--in-input-p) (lispm-chat--menu)))
        (key (get-text-property (point) 'lispm-chat-key)))
    (cond (menu
           (lispm-chat-set-input (concat "/" (lispm-chat--menu-command menu) " ")))
          (key
           (puthash key (not (gethash key lispm-chat--expanded)) lispm-chat--expanded)
           (lispm-chat--redraw))
          ((lispm-chat--in-input-p)
           (completion-at-point)))))

(defun lispm-chat-interrupt ()
  "Stop what the agent is doing."
  (interactive)
  (if (lispm-agent-busy-p lispm-chat-agent)
      (lispm-abort lispm-chat-agent)
    (message "%s" (lispm-tr "Nothing to interrupt"))))

;;;; Commands

(defconst lispm-chat-commands
  `(("help" lispm-chat-help ,(lispm-tr-noop "List commands and keys"))
    ("clear" lispm-chat-clear ,(lispm-tr-noop "Start a new conversation (this one stays on record)"))
    ("compact" lispm-chat-compact ,(lispm-tr-noop "Summarize the conversation and continue from the summary"))
    ("resume" lispm-chat-resume ,(lispm-tr-noop "Resume a recorded conversation here"))
    ("model" lispm-chat-model ,(lispm-tr-noop "Show the model, or switch with /model NAME"))
    ("sandbox" lispm-chat-sandbox ,(lispm-tr-noop "Choose the sandbox level; /sandbox edit, the Custom one's rules"))
    ("diff" lispm-chat-diff ,(lispm-tr-noop "Show what the agent changed in its last turn"))
    ("undo" lispm-chat-undo ,(lispm-tr-noop "Undo what the agent changed in its last turn"))
    ("memory" lispm-chat-memory ,(lispm-tr-noop "Open this project's memory"))
    ("cost" lispm-chat-cost ,(lispm-tr-noop "Show the tokens used"))
    ("agents" lispm-chat-agents ,(lispm-tr-noop "Show every agent"))
    ("inspect" lispm-chat-inspect ,(lispm-tr-noop "Inspect this agent, or /inspect NAME another"))
    ("attach" lispm-chat-attach-command ,(lispm-tr-noop "Attach a file to the next message; an image or a video the agent sees"))
    ("transcript" lispm-chat-transcript ,(lispm-tr-noop "Open the org transcript of this conversation"))
    ("exit" lispm-chat-exit ,(lispm-tr-noop "End this agent")))
  "The / commands: name, function of the agent and the argument, description.
The descriptions are English, translated where shown.")

(defconst lispm-chat--keys
  `(("RET" ,(lispm-tr-noop "send; on an object (a file, an agent...), act on it; on an item, show it in full"))
    ("S-RET, C-j" ,(lispm-tr-noop "new line"))
    ("TAB" ,(lispm-tr-noop "show the item at point in full; complete a / command"))
    ("C-n, C-p" ,(lispm-tr-noop "choose in the menu of / commands, or an answer to a prompt"))
    ("M-p, M-n" ,(lispm-tr-noop "previous and next messages sent"))
    ("C-c C-c" ,(lispm-tr-noop "interrupt the agent"))
    ("1, 2, 3" ,(lispm-tr-noop "answer an approval prompt (or up, down and RET)"))
    ("C-c C-a" ,(lispm-tr-noop "attach a file: an image or a video the agent sees (pasting or dropping one attaches it too)"))
    ("C-c C-t" ,(lispm-tr-noop "the org transcript")))
  "The keys /help lists: (KEYS DESCRIPTION), the description English,
translated where shown.")

(defun lispm-chat--command (agent text)
  "Run the / command TEXT for AGENT."
  (let* ((words (split-string (substring text 1)))
         (entry (assoc (car words) lispm-chat-commands)))
    (if entry
        (funcall (nth 1 entry) agent (string-join (cdr words) " "))
      (message "%s" (lispm-tr "Unknown command /%s; /help lists them" (car words))))))

(defun lispm-chat-help (_agent _argument)
  "Show the commands and keys of the chat."
  (with-help-window "*LispM help*"
    (princ (concat (lispm-tr "Commands") "\n\n"))
    (pcase-dolist (`(,name ,_ ,doc) lispm-chat-commands)
      (princ (format "  /%-11s %s\n" name (lispm-tr doc))))
    (princ (concat "\n" (lispm-tr "Keys") "\n\n"))
    (pcase-dolist (`(,keys ,doc) lispm-chat--keys)
      (princ (format "  %-11s %s\n" keys (lispm-tr doc))))))

(defun lispm-chat--replace (old new)
  "Show the chat of agent NEW where the chat of agent OLD is."
  (let ((windows (and (buffer-live-p (lispm-agent-view old))
                      (get-buffer-window-list (lispm-agent-view old) nil t)))
        (chat (lispm-chat new)))
    (if windows
        (dolist (w windows) (set-window-buffer w chat))
      (pop-to-buffer chat))
    chat))

(defun lispm-chat-clear (agent _argument)
  "Start a new conversation in place of AGENT's, which stays on record.
Return the chat of the new conversation."
  (lispm-stop-at-condition agent)
  (when (lispm-agent-busy-p agent)
    (user-error "%s" (lispm-tr "%s is busy; C-c C-c interrupts it" (lispm-agent-name agent))))
  (if (lispm-agent-standing agent)
      (let ((old (lispm-agent-buffer agent)))
        (lispm--save-session agent)
        (lispm-session-buffer agent t)
        (when (buffer-live-p old)
          (with-current-buffer old (setq lispm-session-agent nil))
          (kill-buffer old))
        (let ((chat (lispm-chat agent)))
          (lispm-chat-redraw chat)
          (unless (get-buffer-window chat t)
            (pop-to-buffer chat))
          chat))
    (let* ((fresh (lispm-start (lispm-agent-role agent) (lispm-agent-root agent)))
           (chat (lispm-chat--replace agent fresh)))
      (lispm-retire agent)
      chat)))

(defun lispm-chat-compact (agent _argument)
  "Hand AGENT's conversation over to a summary of it."
  (lispm-stop-at-condition agent)
  (lispm-compact agent))

(defun lispm-chat-resume (agent _argument)
  "Resume a recorded conversation where AGENT's chat is."
  (let* ((session (lispm--read-session (lispm-tr "Resume: ")))
         (resumed (lispm-resume-file (plist-get session :file))))
    (unless (eq resumed agent)
      (lispm-chat--replace agent resumed))))

(defun lispm-chat-model (agent argument)
  "Show AGENT's model, or switch it to ARGUMENT."
  (if (string-empty-p argument)
      (message "%s" (lispm-tr "%s uses %s via %s" (lispm-agent-name agent)
                         (lispm-agent-model* agent) (lispm-backend-name agent)))
    (lispm-use-model agent (lispm-agent-backend agent) argument)
    (with-current-buffer (lispm-agent-view agent)
      (lispm-chat--redraw))
    (message "%s" (lispm-tr "%s now uses %s" (lispm-agent-name agent) argument))))

(defun lispm-chat-sandbox (_agent argument)
  "Choose the sandbox level of shell commands and MCP servers, or set ARGUMENT.
ARGUMENT edit edits the rules of the level Custom instead."
  (cond ((string-empty-p argument) (lispm-sandbox-set-level (lispm-sandbox-read-level)))
        ((equal (downcase argument) "edit") (lispm-sandbox-customize))
        (t (lispm-sandbox-set-level (or (lispm-sandbox-level-named argument) (intern (downcase argument)))))))

(defun lispm-chat-diff (agent _argument)
  "Show what AGENT changed in its last turn."
  (lispm-turn-diff agent))

(defun lispm-chat-undo (agent _argument)
  "Undo what AGENT changed in its last turn."
  (when (yes-or-no-p (lispm-tr "Undo the changes %s made in its last turn? "
                               (lispm-agent-name agent)))
    (lispm-revert-turn agent)))

(defun lispm-chat-memory (agent _argument)
  "Open the memory of AGENT's project."
  (pop-to-buffer (lispm-memory--buffer agent)))

(defun lispm-chat-cost (agent _argument)
  "Show the tokens AGENT used."
  (pcase-let ((`(,in ,out ,cached) (lispm-agent-usage agent)))
    (message "%s" (lispm-tr "%s: %d input, %d output, %d cached tokens (%s)"
                       (lispm-agent-name agent) in out cached (lispm-agent-model* agent)))))

(defun lispm-chat-agents (_agent _argument)
  "Show every agent."
  (lispm-dashboard))

(defun lispm-chat-transcript (agent &optional _argument)
  "Show the org transcript of AGENT's conversation."
  (interactive (list lispm-chat-agent))
  (let ((buffer (lispm-agent-buffer agent)))
    (with-current-buffer buffer
      (rename-buffer (file-name-nondirectory buffer-file-name) t))
    (pop-to-buffer buffer)))

(defun lispm-chat-inspect (agent argument)
  "Inspect AGENT, or the agent ARGUMENT names."
  (lispm-inspect (if (string-empty-p argument) agent (lispm-get-agent argument))))

(defun lispm-chat-exit (agent _argument)
  "End AGENT."
  (lispm-retire agent))

(defun lispm-chat--complete ()
  "Complete the / command being written, or the @name of a character."
  (when (lispm-chat--in-input-p)
    (let ((name (save-excursion
                  (skip-chars-backward "[:alnum:]_-" lispm-chat--input-start)
                  (and (eq (char-before) ?@) (point)))))
      (cond
       ((save-excursion
          (goto-char lispm-chat--input-start)
          (looking-at "/[a-z]*\\'"))
        (list (1+ lispm-chat--input-start) (point-max)
              (mapcar #'car lispm-chat-commands)
              :exclusive 'no))
       ((and name (lispm-characters))
        (list name (point) (mapcar #'lispm-character-title* (lispm-characters))
              :exclusive 'no))))))

;;;; The mode

(defvar-keymap lispm-chat-mode-map
  :doc "Keymap of `lispm-chat-mode'."
  "<remap> <self-insert-command>" #'lispm-chat-self-insert
  "RET" #'lispm-chat-return
  "S-<return>" #'lispm-chat-newline
  "M-RET" #'lispm-chat-newline
  "C-j" #'lispm-chat-newline
  "TAB" #'lispm-chat-toggle
  "<up>" #'lispm-chat-previous
  "<down>" #'lispm-chat-next
  "<remap> <previous-line>" #'lispm-chat-previous
  "<remap> <next-line>" #'lispm-chat-next
  "M-p" #'lispm-chat-history-previous
  "M-n" #'lispm-chat-history-next
  "C-c C-c" #'lispm-chat-interrupt
  "C-c C-k" #'lispm-chat-interrupt
  "C-c C-t" #'lispm-chat-transcript
  "C-c C-a" #'lispm-chat-attach
  "1" (lispm-chat--answer-key 1)
  "2" (lispm-chat--answer-key 2)
  "3" (lispm-chat--answer-key 3)
  "4" (lispm-chat--answer-key 4)
  "5" (lispm-chat--answer-key 5))

(define-derived-mode lispm-chat-mode nil "LispM"
  "Talk to a LispM agent.
Write at the bottom and press RET; / starts a command (/help lists them).

\\{lispm-chat-mode-map}"
  :group 'lispm-chat
  (setq-local word-wrap t
              truncate-lines nil
              lispm-chat--expanded (make-hash-table :test #'equal)
              lispm-chat--rendered (make-hash-table :test #'equal))
  (visual-line-mode 1)
  ;; An image pasted or a file dropped is attached to the message.
  (yank-media-handler "image/.*" #'lispm-chat--yank-image)
  (setq-local dnd-protocol-alist (cons '("\\`file:" . lispm-chat--drop) dnd-protocol-alist))
  (add-hook 'completion-at-point-functions #'lispm-chat--complete nil t)
  (add-hook 'after-change-functions #'lispm-chat--update-footer nil t)
  (add-hook 'post-command-hook #'lispm-chat--show-menu nil t)
  (add-hook 'kill-buffer-hook #'lispm-chat--killed nil t)
  (add-hook 'window-size-change-functions #'lispm-chat--resized nil t)
  (add-hook 'display-line-numbers-mode-hook #'lispm-chat--resized nil t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize lispm-chat-prompt 'face 'lispm-chat-accent 'read-only t
                        'front-sticky '(read-only) 'rear-nonsticky t))
    (setq lispm-chat--prompt-start (copy-marker (point-min) t)
          lispm-chat--input-start (point-marker)
          lispm-chat--input-overlay (make-overlay (point-max) (point-max) nil nil t)
          lispm-chat--footer (make-overlay (point-max) (point-max) nil t t))
    (let ((indent (make-string (string-width lispm-chat-prompt) ?\s)))
      (overlay-put lispm-chat--input-overlay 'line-prefix indent)
      (overlay-put lispm-chat--input-overlay 'wrap-prefix indent)))
  (when lispm-chat-agent
    (setq default-directory (lispm-agent-root lispm-chat-agent))
    (when (lispm-agent-working-p lispm-chat-agent)
      (setq lispm-chat--busy-since (float-time)))
    (lispm-chat--redraw)))

(defun lispm-chat--resized (&optional window)
  "Redraw the chat in WINDOW, or the current one, at its new width.
Run when a window shows it anew or at another size, and when line
numbers come or go."
  (with-current-buffer (if window (window-buffer window) (current-buffer))
    (when (and lispm-chat-agent lispm-chat--drawn)
      (lispm-chat--schedule lispm-chat-agent))))

(defun lispm-chat--killed ()
  "End the agent of a chat being killed, unless it is standing."
  (unless lispm-chat--closing
    (setq lispm-chat--closing t)
    (when-let* ((agent lispm-chat-agent))
      (when (eq (lispm-agent-view agent) (current-buffer))
        (setf (lispm-agent-view agent) nil)
        (unless (lispm-agent-standing agent)
          (lispm-retire agent))))))

(defun lispm-chat (agent)
  "AGENT's chat buffer, made if need be."
  (let ((chat (lispm-agent-view agent)))
    (unless (buffer-live-p chat)
      (lispm-session-buffer agent)
      (setq chat (generate-new-buffer (format "*lispm %s*" (lispm-agent-name agent))))
      (setf (lispm-agent-view agent) chat)
      (lispm-chat--hide-transcript (lispm-agent-buffer agent))
      (with-current-buffer chat
        (setq lispm-chat-agent agent)
        (lispm-chat-mode)
        (goto-char (point-max))))
    chat))

(provide 'lispm-chat)
;;; lispm-chat.el ends here
