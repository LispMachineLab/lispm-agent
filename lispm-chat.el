;;; lispm-chat.el --- Talking to agents, Claude Code style  -*- lexical-binding: t; -*-

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

;; The interaction layer.  A session is stored as an org document, which
;; is also the agent's context; people talk to the agent in a chat
;; buffer instead, laid out like Claude Code's terminal interface:
;;
;;   > what you asked
;;
;;   ⏺ What the agent said.
;;
;;   ⏺ Read(lispm.el)
;;     ⎿  Read 241 lines
;;
;;   ✻ Thinking… (4s · C-c C-c to interrupt)
;;   ──────────────────────────────────────
;;   > your next message
;;   ──────────────────────────────────────
;;     / for commands · RET send · S-RET newline
;;
;; The chat is drawn from the org buffer, the single source of truth,
;; whenever the session changes, from the first item that changed; the
;; org buffer itself stays out of sight.  Calls awaiting approval are
;; asked about as Claude Code asks: 1 yes, 2 yes and don't ask again,
;; 3 no and say what to do instead.  /transcript shows the org document.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-memory)

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

(defvar-local lispm-chat--expanded nil
  "Hash table of the keys of the items shown in full.")

(defvar-local lispm-chat--rendered nil
  "Hash table from items to their text, so unchanged items are not redrawn.")

(defvar-local lispm-chat--parsed nil
  "The items last read from the session: (BUFFER TICK LIVE-START STABLE ITEMS).")

(defvar-local lispm-chat--dirty 1
  "The first position of the session changed since it was last read.")

(defvar-local lispm-chat--history nil
  "Messages sent from this chat, newest first.")

(defvar-local lispm-chat--history-index nil
  "Position in `lispm-chat--history' while browsing it, or nil.")

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

(defun lispm-chat--body (beg end)
  "Text of the section body between BEG and END, as it was written."
  (string-trim (org-unescape-code-in-string (buffer-substring-no-properties beg end))))

(defun lispm-chat--assistant-items (beg end)
  "Items of the assistant section whose body is between BEG and END."
  (let* ((children (lispm-transcript--sections 2 beg end))
         (lead (string-trim (buffer-substring-no-properties beg (if children (caar children) end))))
         items)
    (unless (string-empty-p lead)
      (push (list :kind 'text :text (org-unescape-code-in-string lead)) items))
    (pcase-dolist (`(,hbeg ,bbeg ,cend) children)
      (let* ((heading (lispm-transcript--heading hbeg))
             (kids (lispm-transcript--sections 3 bbeg cend))
             (own-end (if kids (caar kids) cend))
             (props (lispm-transcript--properties bbeg own-end))
             (type (cdr (assoc "BLOCK" props))))
        (cond
         ((assoc "TOOL_ID" props)
          (let ((json (lispm-transcript--block bbeg own-end)) result failed preview)
            (pcase-dolist (`(,kbeg ,kbody ,kend) kids)
              (pcase (plist-get (lispm-transcript--heading kbeg) :title)
                ((and (or "Result" "Error") title)
                 (setq result (or (lispm-transcript--block kbody kend) "")
                       failed (equal title "Error")))
                ("Preview" (setq preview (lispm-transcript--block kbody kend)))))
            (let ((input (and json (ignore-errors (lispm--json-decode json)))))
              (push (list :kind 'tool
                          :key (cdr (assoc "TOOL_ID" props))
                          :name (cdr (assoc "TOOL" props))
                          :state (plist-get heading :todo)
                          :input (and (consp input) input)
                          :result result :failed failed :preview preview)
                    items))))
         ((member type '("thinking" "reasoning"))
          (let* ((json (lispm-transcript--block bbeg own-end))
                 (block (and json (ignore-errors (lispm--json-decode json))))
                 (text (and (consp block)
                            (or (alist-get 'thinking block) (alist-get 'text block)))))
            (when (and (stringp text) (not (string-blank-p text)))
              (push (list :kind 'thought :key (format "thought@%d" hbeg) :text (string-trim text))
                    items))))
         ((equal type "fallback")
          (push (list :kind 'note :text "A fallback model took over this response.") items))
         ((equal (plist-get heading :title) "Reply")
          (push (list :kind 'text :text (lispm-transcript--text bbeg own-end)) items)))))
    (nreverse items)))

(defun lispm-chat--live-items (beg end)
  "Items of the response streaming in between BEG and END."
  (let (items (pos beg))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'face nil end))
             (dim (get-text-property pos 'face))
             (text (string-trim (buffer-substring-no-properties pos next))))
        (unless (string-empty-p text)
          (cond ((not dim) (push (list :kind 'text :text text) items))
                ((string-prefix-p "-> " text)
                 (dolist (line (split-string text "\n" t))
                   (when (string-match "\\`-> \\(.*\\) \\.\\.\\.\\'" line)
                     (push (list :kind 'tool :name (match-string 1 line) :state "WRITING")
                           items))))
                (t (push (list :kind 'thought :key "thought@live" :text text :live t) items))))
        (setq pos next)))
    (nreverse items)))

(defun lispm-chat--section-items (beg end)
  "Items of the top-level sections of the current buffer between BEG and END."
  (let ((case-fold-search t) items)
    (pcase-dolist (`(,hbeg ,bbeg ,send) (lispm-transcript--sections 1 beg end))
      (let* ((heading (lispm-transcript--heading hbeg))
             (title (or (plist-get heading :title) "")))
        (unless (member "ignore" (plist-get heading :tags))
          (cond
           ((string-match-p "\\`user\\'" title)
            (let ((text (lispm-chat--body bbeg send)))
              (unless (string-empty-p text)
                (push (list :kind 'user :text text) items))))
           ((string-match "\\`message from \\(.+\\)\\'" title)
            (push (list :kind 'user :from (match-string 1 title)
                        :text (lispm-chat--body bbeg send))
                  items))
           ((string-match "\\`event: *\\(.+\\)\\'" title)
            (push (list :kind 'event :name (match-string 1 title)
                        :text (lispm-chat--body bbeg send))
                  items))
           ((string-match-p "\\`summary\\'" title)
            (push (list :kind 'summary :key (format "summary@%d" hbeg)
                        :text (lispm-chat--body bbeg send))
                  items))
           ((string-match-p "\\`note\\'" title)
            (push (list :kind 'note :text (lispm-chat--body bbeg send)) items))
           ((string-match-p "\\`assistant\\'" title)
            (setq items (append (reverse (lispm-chat--assistant-items bbeg send)) items)))))))
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
        (let ((live (and lispm--live (marker-position (car lispm--live)))))
          (list live
                (or stable (lispm-chat--section-items (point-min) (or live (point-max))))
                (and live
                     (lispm-chat--live-items
                      (save-excursion (goto-char live) (forward-line 1) (point))
                      (max live (marker-position (cdr lispm--live)))))))))))

(defun lispm-chat-items (buffer)
  "The conversation in the org session BUFFER, as display items."
  (apply #'append (cdr (lispm-chat--read buffer))))

(defun lispm-chat--items (agent)
  "The items of AGENT's session, read again only as far as it changed.
While a response streams in, what precedes it is not read again
unless it was edited."
  (let* ((buffer (lispm-agent-buffer agent))
         (tick (buffer-modified-tick buffer)))
    (pcase-let ((`(,old-buffer ,old-tick ,old-live ,old-stable ,old-items) lispm-chat--parsed))
      (if (and (eq old-buffer buffer) (eql old-tick tick))
          old-items
        (let* ((live (buffer-local-value 'lispm--live buffer))
               (live-start (and live (marker-position (car live))))
               (reuse (and (eq old-buffer buffer) live-start (eql old-live live-start)
                           (>= lispm-chat--dirty live-start)))
               (read (lispm-chat--read buffer (and reuse old-stable)))
               (items (append (nth 1 read) (nth 2 read))))
          (setq lispm-chat--parsed (list buffer tick (nth 0 read) (nth 1 read) items)
                lispm-chat--dirty most-positive-fixnum)
          items)))))

;;;; Drawing items

(defun lispm-chat--width ()
  "Width to draw rules and boxes at."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 40 (min 100 (1- (if window (window-body-width window) 80))))))

(defun lispm-chat--indent (text prefix &optional first)
  "TEXT with PREFIX before each line, or FIRST before the first one.
Lines that wrap are indented by PREFIX too."
  (let ((lines (split-string text "\n")))
    (propertize (concat (or first prefix) (car lines)
                        (mapconcat (lambda (line) (concat "\n" prefix line)) (cdr lines) ""))
                'wrap-prefix (make-string (string-width prefix) ?\s))))

(defun lispm-chat--inline (line)
  "LINE of a reply with its Markdown headings, bold and code shown as faces."
  (let ((line (if (string-match "\\`#+ +\\(.*\\)\\'" line)
                  (propertize (match-string 1 line) 'face 'bold)
                line)))
    (setq line (replace-regexp-in-string
                "\\*\\*\\([^*\n]+\\)\\*\\*"
                (lambda (m) (propertize (match-string 1 m) 'face 'bold)) line t t))
    (replace-regexp-in-string
     "`\\([^`\n]+\\)`"
     (lambda (m) (propertize (match-string 1 m) 'face 'lispm-chat-code)) line t t)))

(defun lispm-chat--markdown (text)
  "TEXT of a reply, its Markdown shown as faces.
Code blocks lose their fences and keep their lines as they are."
  (let (in-code out)
    (dolist (line (split-string text "\n"))
      (cond ((string-match-p "\\`[ \t]*```" line) (setq in-code (not in-code)))
            (in-code (push (propertize (concat "  " line) 'face 'lispm-chat-code) out))
            (t (push (lispm-chat--inline line) out))))
    (string-join (nreverse out) "\n")))

(defun lispm-chat--count (n thing)
  "N THINGs, in words."
  (format "%d %s%s" n thing (if (= n 1) "" "s")))

(defconst lispm-chat--tool-names
  '(("read_file" . "Read") ("read_buffer" . "Read") ("edit_file" . "Update")
    ("edit_buffer" . "Update") ("write_file" . "Write") ("run_shell" . "Bash")
    ("eval_elisp" . "Eval") ("search_files" . "Search") ("list_directory" . "List")
    ("list_buffers" . "Buffers") ("describe_symbol" . "Describe")
    ("remember" . "Remember") ("recall" . "Recall") ("forget" . "Forget")
    ("task_add" . "Task") ("task_update" . "Task") ("task_list" . "Tasks")
    ("ask_agent" . "Ask") ("send_message" . "Message") ("list_agents" . "Agents"))
  "The names tools are shown under.")

(defun lispm-chat--tool-name (name)
  "The name the tool NAME is shown under."
  (or (alist-get name lispm-chat--tool-names nil nil #'equal) name))

(defun lispm-chat--tool-title (item)
  "Name(argument) of the tool call ITEM."
  (let* ((input (plist-get item :input))
         (argument (if (and (equal (plist-get item :name) "search_files")
                            (stringp (alist-get 'pattern input)))
                       (concat (format "%S" (alist-get 'pattern input))
                               (if (stringp (alist-get 'path input))
                                   (format " in %s" (alist-get 'path input))
                                 "")
                               (if (stringp (alist-get 'glob input))
                                   (format ", %s" (alist-get 'glob input))
                                 ""))
                     (lispm-transcript--summary input))))
    (concat (propertize (lispm-chat--tool-name (plist-get item :name)) 'face 'lispm-chat-tool)
            (if argument (format "(%s)" (lispm--oneline argument 70)) ""))))

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

(defun lispm-chat--diff-summary (diff path)
  "What DIFF does to PATH, in a sentence."
  (let ((added 0) (removed 0))
    (dolist (line (split-string (or diff "") "\n"))
      (cond ((string-match-p "\\`\\(---\\|\\+\\+\\+\\) " line))
            ((string-prefix-p "+" line) (cl-incf added))
            ((string-prefix-p "-" line) (cl-incf removed))))
    (format "Updated %s with %s and %s" (or path "the file")
            (lispm-chat--count added "addition") (lispm-chat--count removed "removal"))))

(defun lispm-chat--more (lines total)
  "LINES, followed by a note of how many of TOTAL lines they leave out."
  (if (> total (length lines))
      (append lines (list (propertize (format "… +%d lines (TAB to expand)" (- total (length lines)))
                                      'face 'lispm-chat-dim)))
    lines))

(defun lispm-chat--result-lines (item expanded)
  "Lines shown under the tool call ITEM; all of them when EXPANDED."
  (let* ((name (plist-get item :name))
         (input (plist-get item :input))
         (path (or (alist-get 'path input) (alist-get 'buffer input)))
         (result (or (plist-get item :result) ""))
         (lines (split-string (string-trim-right result) "\n")))
    (pcase (plist-get item :state)
      ("WRITING" nil)
      ((or "RUNNING" "APPROVED") (list "Running…"))
      ("WAITING" (list "Waiting for your approval…"))
      ((or "DENIED" "FAILED")
       (mapcar (lambda (l) (propertize l 'face 'lispm-chat-error))
               (lispm-chat--more (if expanded lines (seq-take lines 2)) (length lines))))
      (_
       (pcase name
         ((or "edit_file" "edit_buffer")
          (let* ((diff (or (plist-get item :preview) (lispm-chat--input-diff input)))
                 (shown (lispm-chat--diff-lines diff)))
            (cons (lispm-chat--diff-summary diff path)
                  (lispm-chat--more (if expanded shown (seq-take shown lispm-chat-diff-lines))
                                    (length shown)))))
         ("write_file"
          (let ((content (split-string (string-trim-right (or (alist-get 'content input) "")) "\n")))
            (cons (format "Wrote %s to %s" (lispm-chat--count (length content) "line") path)
                  (lispm-chat--more (if expanded content (seq-take content lispm-chat-preview-lines))
                                    (length content)))))
         ((or "read_file" "read_buffer")
          (if expanded lines
            (list (concat "Read " (lispm-chat--count
                                   (cl-count-if (lambda (l) (string-match-p "\\`[ \t]*[0-9]+\t" l))
                                                lines)
                                   "line")))))
         ("list_directory"
          (if expanded lines
            (list (concat "Listed " (lispm-chat--count (max 0 (1- (length lines))) "path")))))
         ("search_files"
          (if (or expanded (string-prefix-p "No matches" result)) lines
            (list (concat "Found " (lispm-chat--count
                                    (cl-count-if (lambda (l) (not (string-prefix-p "[" l))) lines)
                                    "matching line")))))
         (_ (lispm-chat--more (if expanded lines (seq-take lines lispm-chat-preview-lines))
                              (length lines))))))))

(defun lispm-chat--tool (item)
  "Text of the tool call ITEM."
  (let ((face (pcase (plist-get item :state)
                ("DONE" 'lispm-chat-ok)
                ((or "DENIED" "FAILED") 'lispm-chat-error)
                (_ 'lispm-chat-pending)))
        (lines (lispm-chat--result-lines item (plist-get item :expanded))))
    (concat "\n" (propertize "⏺ " 'face face) (lispm-chat--tool-title item)
            (if lines
                (concat "\n" (lispm-chat--indent (string-join lines "\n") "     "
                                                 (propertize "  ⎿  " 'face 'lispm-chat-dim)))
              "")
            "\n")))

(defun lispm-chat--item (item)
  "Text of the display ITEM."
  (pcase (plist-get item :kind)
    ('user
     (concat "\n"
             (propertize (lispm-chat--indent
                          (concat (if (plist-get item :from)
                                      (format "[from %s] " (plist-get item :from))
                                    "")
                                  (plist-get item :text))
                          "  " "> ")
                         'face 'lispm-chat-user)
             "\n"))
    ('text
     (concat "\n" (lispm-chat--indent (lispm-chat--markdown (plist-get item :text)) "  " "⏺ ")
             "\n"))
    ('thought
     (let ((text (plist-get item :text)))
       (concat "\n"
               (propertize (lispm-chat--indent
                            (if (or (plist-get item :expanded) (plist-get item :live))
                                text
                              (concat (lispm--oneline text 70) "  (TAB to show)"))
                            "  " "✻ ")
                           'face 'lispm-chat-thought)
               "\n")))
    ('summary
     (concat "\n"
             (propertize "✻ Conversation compacted" 'face 'lispm-chat-accent)
             (if (plist-get item :expanded)
                 (concat "\n" (propertize (lispm-chat--indent (plist-get item :text) "  ")
                                          'face 'lispm-chat-dim))
               (propertize " (TAB for the summary)" 'face 'lispm-chat-dim))
             "\n"))
    ('event
     (concat "\n"
             (propertize (lispm-chat--indent (format "%s: %s" (plist-get item :name)
                                                     (plist-get item :text))
                                             "  " "⚡ ")
                         'face 'lispm-chat-dim)
             "\n"))
    ('note
     (concat (propertize (lispm-chat--indent (plist-get item :text) "     " "  ⎿  ")
                         'face 'lispm-chat-dim)
             "\n"))
    ('tool (lispm-chat--tool item))))

(defun lispm-chat--read-only (string)
  "STRING, made part of the drawn region the user cannot edit."
  (propertize string 'read-only t 'front-sticky '(read-only) 'rear-nonsticky '(read-only)))

(defun lispm-chat--item-string (item)
  "Text of ITEM as drawn, remembered while ITEM stays the same."
  (let ((item (if (gethash (plist-get item :key) lispm-chat--expanded)
                  (append item '(:expanded t))
                item)))
    (or (gethash item lispm-chat--rendered)
        (let ((string (lispm-chat--item item)))
          (when-let* ((key (plist-get item :key)))
            (setq string (propertize string 'lispm-chat-key key)))
          (puthash item (lispm-chat--read-only string) lispm-chat--rendered)))))

(defun lispm-chat--box (lines width &optional face)
  "LINES framed in a rounded box WIDTH columns wide, the frame in FACE."
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
                       lines "\n")
            "\n" (funcall bar "╰" "╯") "\n")))

(defun lispm-chat--header (agent)
  "The welcome box at the top of AGENT's chat."
  (lispm-chat--box
   (list (concat (propertize "✻ " 'face 'lispm-chat-accent)
                 "Welcome to " (propertize "LispM" 'face 'bold) "!  "
                 (propertize (symbol-name (lispm-agent-name agent)) 'face 'lispm-chat-dim))
         ""
         (propertize "  /help for help, /model to switch models" 'face 'lispm-chat-dim)
         ""
         (propertize (format "  cwd: %s" (abbreviate-file-name (lispm-agent-root agent)))
                     'face 'lispm-chat-dim)
         (propertize (format "  model: %s via %s · role: %s"
                             (or (lispm-agent-model* agent) "?")
                             (or (lispm-agent-backend agent) lispm-backend)
                             (lispm-agent-role agent))
                     'face 'lispm-chat-dim))
   (min 64 (lispm-chat--width)) 'lispm-chat-accent))

;;;; Approval

(defun lispm-chat--waiting ()
  "Id of the call the approval prompt asks about."
  (or (car (lispm--waiting-calls lispm-chat-agent))
      (user-error "Nothing awaits your approval")))

(defun lispm-chat-yes ()
  "Approve the call the prompt asks about."
  (interactive)
  (lispm-resolve lispm-chat-agent (lispm-chat--waiting) 'approve)
  (goto-char (point-max)))

(defun lispm-chat-yes-always ()
  "Approve the call, and every later call of its tool by this agent."
  (interactive)
  (let* ((agent lispm-chat-agent)
         (name (plist-get (gethash (lispm-chat--waiting) (lispm-agent-pending agent)) :name)))
    (push name (lispm-agent-allowed agent))
    (dolist (id (lispm--waiting-calls agent))
      (when (equal (plist-get (gethash id (lispm-agent-pending agent)) :name) name)
        (lispm-resolve agent id 'approve))))
  (goto-char (point-max)))

(defun lispm-chat-no ()
  "Deny the call, saying what to do instead."
  (interactive)
  (let ((id (lispm-chat--waiting))
        (reason (read-string "What should it do instead? (optional) ")))
    (lispm-resolve lispm-chat-agent id 'deny reason))
  (goto-char (point-max)))

(defun lispm-chat-choose ()
  "Give the selected answer to the approval prompt."
  (interactive)
  (pcase lispm-chat--choice
    (0 (lispm-chat-yes))
    (1 (lispm-chat-yes-always))
    (_ (lispm-chat-no))))

(defun lispm-chat-choice-next (&optional n)
  "Select the next answer in the approval prompt, or the Nth after."
  (interactive "p")
  (setq lispm-chat--choice (mod (+ lispm-chat--choice (or n 1)) 3))
  (lispm-chat--redraw))

(defun lispm-chat-choice-previous ()
  "Select the previous answer in the approval prompt."
  (interactive)
  (lispm-chat-choice-next -1))

(defvar-keymap lispm-chat-approval-map
  :doc "Keys of the approval prompt."
  "1" #'lispm-chat-yes
  "2" #'lispm-chat-yes-always
  "3" #'lispm-chat-no
  "y" #'lispm-chat-yes
  "n" #'lispm-chat-no
  "RET" #'lispm-chat-choose
  "<up>" #'lispm-chat-choice-previous
  "<down>" #'lispm-chat-choice-next)

(defun lispm-chat--approval (agent items)
  "The prompt for AGENT's first call awaiting approval, or nil.
ITEMS are the session's items, where the call's preview is."
  (when-let* ((id (car (lispm--waiting-calls agent)))
              (call (gethash id (lispm-agent-pending agent))))
    (let* ((name (plist-get call :name))
           (input (and (consp (plist-get call :input)) (plist-get call :input)))
           (shown (lispm-chat--tool-name name))
           (path (or (alist-get 'path input) (alist-get 'buffer input)))
           (why (alist-get 'why input))
           (item (seq-find (lambda (i) (equal (plist-get i :key) id)) items))
           (body (pcase name
                   ((or "edit_file" "write_file" "edit_buffer")
                    (lispm-chat--diff-lines (or (plist-get item :preview)
                                                (lispm-chat--input-diff input))))
                   ("run_shell" (split-string (or (alist-get 'command input) "") "\n"))
                   ("eval_elisp" (split-string (or (alist-get 'code input) "") "\n"))
                   (_ (split-string (lispm--json-pretty input) "\n"))))
           (title (pcase name
                    ((or "edit_file" "edit_buffer") "Edit file")
                    ("write_file" "Write file")
                    ("run_shell" "Bash command")
                    ("eval_elisp" "Evaluate Emacs Lisp")
                    (_ (format "Use %s" shown))))
           (question (pcase name
                       ((or "edit_file" "edit_buffer")
                        (format "Do you want to make this edit to %s?" path))
                       ("write_file" (format "Do you want to write %s?" path))
                       ("eval_elisp" "Do you want to evaluate this?")
                       (_ "Do you want to proceed?")))
           (options (list "Yes"
                          (format "Yes, and don't ask again for %s calls by this agent" shown)
                          "No, and tell it what to do differently")))
      (propertize
       (concat
        "\n"
        (lispm-chat--box
         (append (list (propertize title 'face 'bold) "")
                 (mapcar (lambda (l) (concat "  " l)) (seq-take body 30))
                 (and (> (length body) 30)
                      (list (propertize (format "  … +%d lines" (- (length body) 30))
                                        'face 'lispm-chat-dim)))
                 (and (stringp why) (list "" (propertize (concat "  " why) 'face 'lispm-chat-dim)))
                 (list "" question)
                 (cl-loop for option in options
                          for i from 0
                          collect (if (= i lispm-chat--choice)
                                      (propertize (format "❯ %d. %s" (1+ i) option)
                                                  'face 'lispm-chat-accent)
                                    (format "  %d. %s" (1+ i) option))))
         (lispm-chat--width) 'lispm-chat-pending))
       'keymap lispm-chat-approval-map))))

;;;; Status

(defun lispm-chat--status (agent)
  "The line saying what AGENT is doing, or nil when it is not working."
  (let ((state (lispm-agent-state agent)))
    (unless (memq state '(idle waiting))
      (let* ((seconds (if lispm-chat--busy-since
                          (floor (- (float-time) lispm-chat--busy-since))
                        0))
             (frame (aref lispm-chat--spinner
                          (mod (floor (* 4 (float-time))) (length lispm-chat--spinner))))
             (status (or (lispm-agent-status agent) ""))
             (verb (cond ((eq state 'compacting) "Compacting conversation…")
                         ((eq state 'aborting) "Stopping…")
                         ((string-prefix-p "running " status)
                          (format "Running %s…" (substring status 8)))
                         ((string-prefix-p "writing " status)
                          (format "Writing %s…" (substring status 8)))
                         ((string-match-p "retry" status) (format "Thinking… (%s)" status))
                         (t "Thinking…"))))
        (concat "\n"
                (propertize (concat frame " " verb) 'face 'lispm-chat-accent)
                (propertize (format " (%ds · C-c C-c to interrupt)" seconds) 'face 'lispm-chat-dim)
                "\n")))))

(defun lispm-chat--queued (agent)
  "The inputs waiting for AGENT, or nil."
  (when-let* ((inbox (lispm-agent-inbox agent)))
    (concat "\n"
            (mapconcat (lambda (entry)
                         (propertize (lispm-chat--indent
                                      (concat (cond ((plist-get entry :from)
                                                     (format "[from %s] " (plist-get entry :from)))
                                                    ((plist-get entry :event)
                                                     (format "[%s] " (plist-get entry :event)))
                                                    (t ""))
                                              (plist-get entry :text))
                                      "  " "> ")
                                     'face 'lispm-chat-dim))
                       inbox "\n")
            (propertize "\n  ⎿  queued; sent when the agent is free" 'face 'lispm-chat-dim)
            "\n")))

(defun lispm-chat--rule ()
  "A rule across the chat."
  (propertize (make-string (lispm-chat--width) ?─) 'face 'lispm-chat-dim))

(defun lispm-chat--strings (agent)
  "Everything AGENT's chat shows before the prompt, as a list of strings."
  (let ((items (lispm-chat--items agent)))
    (append (list (lispm-chat--read-only (lispm-chat--header agent)))
            (mapcar #'lispm-chat--item-string items)
            (mapcar #'lispm-chat--read-only
                    (delq nil (list (lispm-chat--queued agent)
                                    (lispm-chat--approval agent items)
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

(defun lispm-chat--follow (window)
  "Scroll WINDOW so that the input, and the hints below it, are in sight."
  (let ((posn (posn-at-point (point-max) window)))
    (when (or (null posn)
              (> (+ (cdr (posn-x-y posn)) (* 3 (default-line-height)))
                 (window-body-height window t)))
      (with-selected-window window
        (recenter -3)))))

(defun lispm-chat--maybe-ask ()
  "Select the first answer of a newly shown approval prompt."
  (let ((id (car (lispm--waiting-calls lispm-chat-agent))))
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
        (if (memq (lispm-agent-state agent) '(idle waiting))
            (setq lispm-chat--busy-since nil)
          (unless lispm-chat--busy-since
            (setq lispm-chat--busy-since (float-time)))))
      (lispm-chat--animate)
      (lispm-chat--schedule agent))))

(defun lispm-chat--busy-agents ()
  "The agents working while their chat is open."
  (seq-filter (lambda (a) (and (buffer-live-p (lispm-agent-view a))
                               (not (memq (lispm-agent-state a) '(idle waiting)))))
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

(add-hook 'lispm-session-mode-hook #'lispm-chat--watch-transcript)
(add-hook 'lispm-state-change-functions #'lispm-chat--state-changed)
(add-hook 'lispm-session-end-functions #'lispm-chat--session-ended)

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

(defun lispm-chat--update-footer (&rest _)
  "Draw the rule and hints below the input, and a placeholder while it is empty."
  (when (overlayp lispm-chat--footer)
    (let ((footer
           (concat (if (and lispm-chat--input-start (= lispm-chat--input-start (point-max)))
                       (propertize (if (and lispm-chat-agent (lispm--waiting-calls lispm-chat-agent))
                                       "Answer above with 1, 2 or 3"
                                     "Try \"explain this project\", or / for commands")
                                   'face 'lispm-chat-dim)
                     " ")
                   "\n" (lispm-chat--rule) "\n"
                   (propertize "  / for commands · RET send · S-RET newline · TAB expand · C-c C-c interrupt"
                               'face 'lispm-chat-dim))))
      ;; The footer is drawn at the end of the input, where point usually
      ;; is, and Emacs would draw the cursor after it, below the hints.
      ;; Put it on the footer's first character instead: the placeholder,
      ;; or a space (a newline holds no cursor).
      (put-text-property 0 1 'cursor t footer)
      (overlay-put lispm-chat--footer 'after-string footer))))

(defun lispm-chat-return ()
  "Send what is written, or answer the approval prompt if nothing is.
Outside the input, show the item at point in full."
  (interactive)
  (cond ((not (lispm-chat--in-input-p))
         (if (get-text-property (point) 'lispm-chat-key)
             (lispm-chat-toggle)
           (goto-char (point-max))))
        ((and (string-blank-p (lispm-chat--input)) (lispm--waiting-calls lispm-chat-agent))
         (lispm-chat-choose))
        (t (lispm-chat-send))))

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
        (lispm-tell agent text)
        (lispm-chat--schedule agent)))))

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

(defun lispm-chat--answer-key (answer)
  "A command giving ANSWER (a command) to a pending approval prompt, else typing."
  (lambda ()
    (interactive)
    (if (and (lispm--waiting-calls lispm-chat-agent)
             (string-empty-p (lispm-chat--input)))
        (call-interactively answer)
      (lispm-chat-self-insert))))

(defun lispm-chat-previous ()
  "Select the previous answer of an approval prompt, or move up a line."
  (interactive)
  (if (and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
           (lispm--waiting-calls lispm-chat-agent))
      (lispm-chat-choice-previous)
    (call-interactively #'previous-line)))

(defun lispm-chat-next ()
  "Select the next answer of an approval prompt, or move down a line."
  (interactive)
  (if (and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
           (lispm--waiting-calls lispm-chat-agent))
      (lispm-chat-choice-next)
    (call-interactively #'next-line)))

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
  "Show the item at point in full, or briefly again; in the input, complete."
  (interactive)
  (if-let* ((key (get-text-property (point) 'lispm-chat-key)))
      (progn (puthash key (not (gethash key lispm-chat--expanded)) lispm-chat--expanded)
             (lispm-chat--redraw))
    (when (lispm-chat--in-input-p)
      (completion-at-point))))

(defun lispm-chat-interrupt ()
  "Stop what the agent is doing."
  (interactive)
  (if (lispm-agent-busy-p lispm-chat-agent)
      (lispm-abort lispm-chat-agent)
    (message "Nothing to interrupt")))

;;;; Commands

(defconst lispm-chat-commands
  '(("help" lispm-chat-help "List commands and keys")
    ("clear" lispm-chat-clear "Start a new conversation (this one stays on record)")
    ("compact" lispm-chat-compact "Summarize the conversation and continue from the summary")
    ("resume" lispm-chat-resume "Resume a recorded conversation here")
    ("model" lispm-chat-model "Show the model, or switch with /model NAME")
    ("diff" lispm-chat-diff "Show what the agent changed in its last turn")
    ("undo" lispm-chat-undo "Undo what the agent changed in its last turn")
    ("memory" lispm-chat-memory "Open this project's memory")
    ("cost" lispm-chat-cost "Show the tokens used")
    ("agents" lispm-chat-agents "Show every agent")
    ("transcript" lispm-chat-transcript "Open the org transcript of this conversation")
    ("exit" lispm-chat-exit "End this agent"))
  "The / commands: name, function of the agent and the argument, description.")

(defun lispm-chat--command (agent text)
  "Run the / command TEXT for AGENT."
  (let* ((words (split-string (substring text 1)))
         (entry (assoc (car words) lispm-chat-commands)))
    (if entry
        (funcall (nth 1 entry) agent (string-join (cdr words) " "))
      (message "Unknown command /%s; /help lists them" (car words)))))

(defun lispm-chat-help (_agent _argument)
  "Show the commands and keys of the chat."
  (with-help-window "*LispM help*"
    (princ "Commands\n\n")
    (pcase-dolist (`(,name ,_ ,doc) lispm-chat-commands)
      (princ (format "  /%-11s %s\n" name doc)))
    (princ (concat "\nKeys\n\n"
                   "  RET         send; on an item, show it in full\n"
                   "  S-RET, C-j  new line\n"
                   "  TAB         show the item at point in full; complete a / command\n"
                   "  M-p, M-n    previous and next messages sent\n"
                   "  C-c C-c     interrupt the agent\n"
                   "  1, 2, 3     answer an approval prompt (or up, down and RET)\n"
                   "  C-c C-t     the org transcript\n"))))

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
  (when (lispm-agent-busy-p agent)
    (user-error "%s is busy; C-c C-c interrupts it" (lispm-agent-name agent)))
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
  (lispm-compact agent))

(defun lispm-chat-resume (agent _argument)
  "Resume a recorded conversation where AGENT's chat is."
  (let* ((session (lispm--read-session "Resume: "))
         (resumed (lispm-resume-file (plist-get session :file))))
    (unless (eq resumed agent)
      (lispm-chat--replace agent resumed))))

(defun lispm-chat-model (agent argument)
  "Show AGENT's model, or switch it to ARGUMENT."
  (if (string-empty-p argument)
      (message "%s uses %s via %s" (lispm-agent-name agent)
               (lispm-agent-model* agent) (or (lispm-agent-backend agent) lispm-backend))
    (setf (lispm-agent-model agent) argument)
    (with-current-buffer (lispm-agent-view agent)
      (lispm-chat--redraw))
    (message "%s now uses %s" (lispm-agent-name agent) argument)))

(defun lispm-chat-diff (agent _argument)
  "Show what AGENT changed in its last turn."
  (lispm-turn-diff agent))

(defun lispm-chat-undo (agent _argument)
  "Undo what AGENT changed in its last turn."
  (when (yes-or-no-p (format "Undo the changes %s made in its last turn? "
                             (lispm-agent-name agent)))
    (lispm-revert-turn agent)))

(defun lispm-chat-memory (agent _argument)
  "Open the memory of AGENT's project."
  (pop-to-buffer (lispm-memory--buffer agent)))

(defun lispm-chat-cost (agent _argument)
  "Show the tokens AGENT used."
  (pcase-let ((`(,in ,out ,cached) (lispm-agent-usage agent)))
    (message "%s: %d input, %d output, %d cached tokens (%s)"
             (lispm-agent-name agent) in out cached (lispm-agent-model* agent))))

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

(defun lispm-chat-exit (agent _argument)
  "End AGENT."
  (lispm-retire agent))

(defun lispm-chat--complete ()
  "Complete the / command being written."
  (when (and (lispm-chat--in-input-p)
             (save-excursion
               (goto-char lispm-chat--input-start)
               (looking-at "/[a-z]*\\'")))
    (list (1+ lispm-chat--input-start) (point-max)
          (mapcar #'car lispm-chat-commands)
          :exclusive 'no)))

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
  "M-p" #'lispm-chat-history-previous
  "M-n" #'lispm-chat-history-next
  "C-c C-c" #'lispm-chat-interrupt
  "C-c C-k" #'lispm-chat-interrupt
  "C-c C-t" #'lispm-chat-transcript
  "1" (lispm-chat--answer-key #'lispm-chat-yes)
  "2" (lispm-chat--answer-key #'lispm-chat-yes-always)
  "3" (lispm-chat--answer-key #'lispm-chat-no))

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
  (add-hook 'completion-at-point-functions #'lispm-chat--complete nil t)
  (add-hook 'after-change-functions #'lispm-chat--update-footer nil t)
  (add-hook 'kill-buffer-hook #'lispm-chat--killed nil t)
  (add-hook 'window-size-change-functions #'lispm-chat--resized nil t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "> " 'face 'lispm-chat-accent 'read-only t
                        'front-sticky '(read-only) 'rear-nonsticky t))
    (setq lispm-chat--prompt-start (copy-marker (point-min) t)
          lispm-chat--input-start (point-marker)
          lispm-chat--footer (make-overlay (point-max) (point-max) nil t t)))
  (when lispm-chat-agent
    (setq default-directory (lispm-agent-root lispm-chat-agent))
    (unless (memq (lispm-agent-state lispm-chat-agent) '(idle waiting))
      (setq lispm-chat--busy-since (float-time)))
    (lispm-chat--redraw)))

(defun lispm-chat--resized (window)
  "Redraw the chat in WINDOW at its new width."
  (with-current-buffer (window-buffer window)
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
