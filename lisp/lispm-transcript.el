;;; lispm-transcript.el --- Org transcripts as agent context  -*- lexical-binding: t; -*-

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

;; A session is an org file, and that file *is* the agent's context:
;; every request is built by parsing the buffer, so whatever the user
;; folds, annotates, deletes or tags in it changes what the model sees.
;;
;; Layout of a session buffer:
;;
;;   #+title: coder@lispm 2026-10-12 17:30
;;   #+lispm_agent: coder@lispm      the agent, with its session id, role
;;   #+lispm_id: 20261012-173000-9f3a and project root, so sessions can be
;;   #+lispm_role: coder              listed and resumed
;;   #+lispm_root: ~/src/lispm/
;;   #+todo: WAITING(w) APPROVED(a) RUNNING(r) | DONE(d) DENIED(n) FAILED(f)
;;   * System                       the system prompt, frozen per session
;;   * User                         a user message
;;   * Assistant                    a model response; one child per block
;;   ** Reply                       a text block
;;   ** Thinking: ...               any other block, stored as raw JSON
;;   ** WAITING edit_file  a.el     a tool call; its TODO state is its
;;   :PROPERTIES:                   approval state, so approving a call
;;   :TOOL_ID:  toolu_...           is just C-c C-t (or doing it from
;;   :END:                          the org agenda)
;;   #+begin_src json ... #+end_src the call's input
;;   *** Asks: write_file  b.el     a call the call makes in turn, as a
;;                                  Lisp program does, awaiting approval
;;   *** Preview                    diff shown while awaiting approval,
;;                                  after why it asks, if a check said
;;   *** Result                     the tool's output (or *** Error); tagged
;;                                  :cleared:, it is no longer sent
;;   *** Stopped                    an error the call stopped at, awaiting a
;;                                  restart (:CONDITION: tool-error)
;;   * Message from reviewer        input from another agent
;;   * Event: after-save-hook       input from a hook the user set up
;;   * Summary                      what an earlier session handed over
;;   * From LispM                   LispM passing on a choice of the user's
;;   * WAITING Condition: ...       the turn stopped early, awaiting a
;;   :PROPERTIES:                   restart: retry, continue, abort...
;;   :CONDITION: request-failed     (`lispm-signal'); never sent
;;   :END:
;;
;; Any other top-level heading (say "* Notes") is the user's own and is
;; not sent; neither is any message tagged :ignore:, nor reasoning text
;; from Chat Completions servers (a "reasoning" block), which is shown
;; but never sent back.  A tool result tagged :cleared: is sent as a
;; line saying it was cleared: the runtime tags old results so when a
;; session grows long (see `lispm-compact-keep-results').
;;
;; Text written by the model round-trips byte for byte: models bind
;; their reasoning to the exact conversation prefix, so a transcript
;; that re-renders a reply even slightly differently would invalidate
;; it on every turn.  Bodies are therefore escaped the way org escapes
;; source blocks (a comma before lines starting with "*" or "#+") and
;; end in exactly one newline the parser strips again.

;;; Code:

(require 'org)
(require 'lispm-core)

(defconst lispm-transcript-todo-line
  "#+todo: WAITING(w) APPROVED(a) RUNNING(r) | DONE(d) DENIED(n) FAILED(f)\n"
  "TODO keywords of a session: the life cycle of a tool call.")

(defconst lispm-transcript--state-re
  "\\(WAITING\\|APPROVED\\|RUNNING\\|DONE\\|DENIED\\|FAILED\\)"
  "Regexp matching a tool-call state keyword.")

;;;; Kinds of sections

(defconst lispm-transcript-kinds
  '((system "System" "\\`System\\'" nil)
    (user "User" "\\`User\\'" "")
    (assistant "Assistant" "\\`Assistant\\'" nil)
    (summary "Summary" "\\`Summary\\'"
             "[Summary of the earlier conversation, handed over to this session]\n")
    (from-lispm "From LispM" "\\`From LispM\\'" "[From LispM, for the user]\n")
    (message "Message from %s" "\\`Message from \\(.+\\)\\'" "[Message from agent %s]\n")
    (event "Event: %s" "\\`Event: *\\(.+\\)\\'" "[Event: %s]\n")
    (note "Note" "\\`Note\\'" nil)
    (condition "Condition: %s" "\\`Condition: \\(.+\\)\\'" nil))
  "The kinds of top-level sections LispM writes: (KIND TITLE REGEXP PREFIX).
TITLE is the heading's title, %s standing for the kind's argument: the
agent a message is from, what an event is, what a condition says.
REGEXP matches the title, in any case, its group the argument.  PREFIX,
for a section the model reads as the user's, is what requests put before
its text, %s again the argument.  A section of no kind is the user's own
and is never sent.")

(defun lispm-transcript-title (kind &optional arg)
  "The title of a section of KIND, a key of `lispm-transcript-kinds', for ARG."
  (format (nth 1 (assq kind lispm-transcript-kinds)) arg))

(defun lispm-transcript-heading (kind &optional arg)
  "The heading line of a section of KIND for ARG; see `lispm-transcript-title'."
  (concat "* " (lispm-transcript-title kind arg) "\n"))

(defun lispm-transcript-heading-regexp (&rest kinds)
  "A regexp matching the heading line of a section of any of KINDS, the
newline ending it included."
  (concat "^\\* \\(?:"
          (mapconcat (lambda (kind)
                       (let ((regexp (nth 2 (assq kind lispm-transcript-kinds))))
                         (substring regexp 2 -2)))
                     kinds "\\|")
          "\\)[ \t]*\\(?:\n\\|\\'\\)"))

(defun lispm-transcript-kind (title)
  "The kind of a top-level section titled TITLE, as (KIND . ARG), or nil."
  (let ((case-fold-search t))
    (cl-loop for (kind _ regexp) in lispm-transcript-kinds
             when (string-match regexp title)
             return (cons kind (match-string 1 title)))))

(defun lispm-transcript-input-title (input)
  "The title of the section INPUT starts in a session.
INPUT is a plist with :from, the agent a message is from, or :event, as
inbox entries are; with neither, it is the user's."
  (cond ((plist-get input :from) (lispm-transcript-title 'message (plist-get input :from)))
        ((plist-get input :event) (lispm-transcript-title 'event (plist-get input :event)))
        (t (lispm-transcript-title 'user))))

;;;; Rendering

(defun lispm-transcript-header (title keywords)
  "File header of a session titled TITLE.
KEYWORDS is an alist of (NAME . VALUE) recorded as #+lispm_NAME lines."
  (concat (format "#+title: %s\n" title)
          (mapconcat (pcase-lambda (`(,name . ,value)) (format "#+lispm_%s: %s\n" name value))
                     keywords "")
          lispm-transcript-todo-line))

(defun lispm-transcript-keywords ()
  "The #+lispm_ keywords at the start of the current buffer, as an alist.
Keys are strings without the prefix, such as \"id\" or \"root\"."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((limit (save-excursion (if (re-search-forward "^\\* " nil t)
                                       (match-beginning 0)
                                     (point-max))))
            keywords)
        (while (re-search-forward "^#\\+lispm_\\([a-z_]+\\): *\\(.*?\\) *$" limit t)
          (push (cons (match-string-no-properties 1) (match-string-no-properties 2))
                keywords))
        (nreverse keywords)))))

(defun lispm-transcript-body (text)
  "TEXT as a section body that `lispm-transcript--text' reads back exactly."
  (concat (org-escape-code-in-string text) "\n"))

(defun lispm-transcript-section (title text)
  "A top-level section with heading TITLE and body TEXT."
  (concat "* " title "\n" (lispm-transcript-body text)))

(defun lispm-transcript-condition (title id type restarts message &optional resume)
  "Section of a condition awaiting a restart.
TITLE says what happened; ID and TYPE identify the condition, RESTARTS
are the names of the restarts it offers, MESSAGE says why.  RESUME
non-nil records that it stopped a turn in the middle, not between two."
  (concat "* WAITING Condition: " title "\n"
          ":PROPERTIES:\n"
          ":CONDITION: " (symbol-name type) "\n"
          ":CONDITION_ID: " id "\n"
          ":RESTARTS: " (mapconcat #'symbol-name restarts " ") "\n"
          (if resume ":RESUME: t\n" "")
          ":END:\n"
          (lispm-transcript-body message)))

(defun lispm-transcript-summary (input)
  "A short label for a tool call with INPUT: its main string argument."
  (when (consp input)
    (let ((value (or (cl-loop for key in '(pattern path buffer command query title name to agent id)
                              for value = (alist-get key input)
                              when (stringp value) return value)
                     (cl-loop for (key . value) in input
                              when (and (stringp value) (not (eq key 'why)))
                              return value))))
      (and value (lispm--oneline value 60)))))

(defun lispm-transcript--tool-use (block state)
  "Section text for the tool_use BLOCK, in TODO STATE."
  (let* ((name (alist-get 'name block))
         (input (alist-get 'input block))
         (why (and (consp input) (alist-get 'why input)))
         (summary (lispm-transcript-summary input)))
    (concat "** " state " " name (if summary (concat "  " summary) "") "\n"
            ":PROPERTIES:\n"
            ":TOOL_ID:  " (alist-get 'id block) "\n"
            ":TOOL:     " name "\n"
            ":END:\n"
            (if (stringp why) (concat "- why :: " (lispm--oneline why 200) "\n") "")
            "#+begin_src json\n"
            (lispm-transcript-body (lispm--json-pretty input))
            "#+end_src\n")))

(defun lispm-transcript--raw-title (block)
  "Heading for a content BLOCK that is kept as raw JSON."
  (let ((type (alist-get 'type block)))
    (pcase type
      ("thinking"
       (let ((text (alist-get 'thinking block)))
         (if (and (stringp text) (not (string-blank-p text)))
             (concat "Thinking: " (lispm--oneline text 70))
           "Thinking")))
      ("redacted_thinking" "Redacted thinking")
      ("reasoning" (concat "Reasoning: " (lispm--oneline (alist-get 'text block) 70)))
      ("fallback"
       (format "Fallback: %s -> %s"
               (alist-get 'model (alist-get 'from block))
               (alist-get 'model (alist-get 'to block))))
      (_ (capitalize (replace-regexp-in-string "_" " " (format "%s" type)))))))

(defun lispm-transcript--raw (block)
  "Section text for a content BLOCK kept verbatim as JSON."
  (concat "** " (lispm-transcript--raw-title block) "\n"
          ":PROPERTIES:\n"
          ":BLOCK:    " (format "%s" (alist-get 'type block)) "\n"
          ":END:\n"
          "#+begin_src json\n"
          (lispm-transcript-body (lispm--json-encode block))
          "#+end_src\n"))

(defun lispm-transcript-result (text errorp &optional images directory)
  "Section text for a tool result TEXT; ERRORP marks a failure.
IMAGES are files the result also holds, such as screenshots; they are
linked, relative to DIRECTORY if given, one per line."
  (concat "*** " (if errorp "Error" "Result") "\n"
          "#+begin_example\n"
          (lispm-transcript-body text)
          "#+end_example\n"
          (mapconcat (lambda (file)
                       (format "[[file:%s]]\n"
                               (if directory (file-relative-name file directory) file)))
                     images "")))

;;;; Images
;;
;; An image in a tool result is a file next to the session, linked from
;; the result's section, so org can show it inline.  The parser reads
;; the files into image blocks; only the last `lispm-context-images'
;; are sent, the others replaced by a line of text.

(defcustom lispm-context-images 5
  "How many of a session's latest images requests carry.
Older ones are dropped in batches of this many, so that the request
prefix, and with it the model's cache, changes only once a batch."
  :type 'integer
  :group 'lispm)

(defconst lispm-transcript--image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp"))
  "Image file extensions and their media types.")

(defun lispm-transcript--images (beg end)
  "Image files linked alone on a line between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((base (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
          files)
      (while (re-search-forward "^\\[\\[file:\\([^]\n]+\\)\\]\\][ \t]*$" end t)
        (let ((file (match-string-no-properties 1)))
          (when (assoc (downcase (or (file-name-extension file) "")) lispm-transcript--image-types)
            (push (expand-file-name file base) files))))
      (nreverse files))))

(defun lispm-transcript--image-block (file)
  "A placeholder image block for FILE; `lispm-transcript--load-images' reads it."
  `((type . "image") (lispm-file . ,file)))

(defun lispm-transcript--map-images (function messages)
  "MESSAGES with each image placeholder replaced by FUNCTION's value for it.
Placeholders are content blocks or blocks inside tool results."
  (let ((map (lambda (blocks)
               (vconcat (mapcar (lambda (block)
                                  (cond ((alist-get 'lispm-file block) (funcall function block))
                                        ((and (equal (alist-get 'type block) "tool_result")
                                              (vectorp (alist-get 'content block)))
                                         (let ((copy (copy-alist block)))
                                           (setf (alist-get 'content copy)
                                                 (vconcat (mapcar (lambda (b)
                                                                    (if (alist-get 'lispm-file b)
                                                                        (funcall function b)
                                                                      b))
                                                                  (alist-get 'content block))))
                                           copy))
                                        (t block)))
                                blocks)))))
    (mapcar (lambda (message)
              (let ((content (alist-get 'content message)))
                (if (not (vectorp content))
                    message
                  (let ((copy (copy-alist message)))
                    (setf (alist-get 'content copy) (funcall map content))
                    copy))))
            messages)))

(defun lispm-transcript--load-images (messages)
  "MESSAGES with their latest image placeholders read into image blocks.
The others become a line of text; see `lispm-context-images'."
  (let ((total 0) (seen 0))
    (lispm-transcript--map-images (lambda (block) (cl-incf total) block) messages)
    (if (zerop total)
        messages
      (let* ((keep (max 1 lispm-context-images))
             (drop (- total keep))
             (drop (if (> drop 0) (- drop (% drop keep)) 0)))
        (lispm-transcript--map-images
         (lambda (block)
           (let ((file (alist-get 'lispm-file block)))
             (cl-incf seen)
             (cond ((<= seen drop)
                    `((type . "text") (text . ,(format "[image %s, no longer shown]"
                                                       (file-name-nondirectory file)))))
                   ((not (file-readable-p file))
                    `((type . "text") (text . ,(format "[image %s is missing]" file))))
                   (t
                    `((type . "image")
                      (source (type . "base64")
                              (media_type . ,(cdr (assoc (downcase (file-name-extension file))
                                                         lispm-transcript--image-types)))
                              (data . ,(with-temp-buffer
                                         (set-buffer-multibyte nil)
                                         (insert-file-contents-literally file)
                                         (base64-encode-region (point-min) (point-max) t)
                                         (buffer-string)))))))))
         messages)))))

(defun lispm-transcript-stopped (text &optional restart)
  "Subsection of a call stopped at the error TEXT.
RESTART, once one is chosen, is named in its heading."
  (concat "*** Stopped" (if restart (format ": %s" restart) "") "\n"
          (lispm-transcript-body text)))

(defun lispm-transcript-asks (name input)
  "Subsection of a call awaiting approval of a call of NAME on INPUT it makes."
  (let ((summary (lispm-transcript-summary input))
        (why (and (consp input) (alist-get 'why input))))
    (concat "*** Asks: " name (if summary (concat "  " summary) "") "\n"
            (if (stringp why) (concat "- why :: " (lispm--oneline why 200) "\n") "")
            "#+begin_src json\n"
            (lispm-transcript-body (lispm--json-pretty input))
            "#+end_src\n")))

(defun lispm-transcript-preview (diff &optional reasons)
  "Section text showing DIFF while a call awaits approval, and REASONS.
DIFF may be nil; REASONS are why it asks, a list of strings, each an
item of a list before DIFF (see `lispm-tool-decision')."
  (concat "*** Preview\n"
          (mapconcat (lambda (reason) (concat "- " (lispm--oneline reason 1000) "\n")) reasons "")
          (if diff
              (concat "#+begin_src diff\n" (lispm-transcript-body diff) "#+end_src\n")
            "")))

(defun lispm-transcript--reasons (beg end)
  "The reasons a Preview section between BEG and END gives, a list of strings.
See `lispm-transcript-preview'."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t) reasons)
      (while (and (< (point) end) (not (looking-at-p "[ \t]*#\\+begin_")))
        (when (looking-at "- \\(.+\\)$")
          (push (match-string-no-properties 1) reasons))
        (forward-line 1))
      (nreverse reasons))))

(defun lispm-transcript-fold (pos)
  "Fold the subtree whose heading starts at POS, ignoring failures."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (org-fold-subtree t))))

(defun lispm-transcript-fold-drawers (beg end)
  "Hide the property drawers between BEG and END."
  (ignore-errors
    (save-restriction
      (narrow-to-region beg end)
      (org-fold-hide-drawer-all))))

(defun lispm-transcript-insert-blocks (content state-function)
  "Insert the assistant CONTENT blocks at point, one section each.
STATE-FUNCTION maps a tool_use block to its initial TODO keyword.
Return an alist (TOOL-ID . MARKER) locating each tool-call heading."
  (let ((beg (point)) calls folds)
    (dolist (block content)
      (let ((start (point)))
        (pcase (alist-get 'type block)
          ("text"
           (insert "** Reply\n" (lispm-transcript-body (alist-get 'text block))))
          ("tool_use"
           (insert (lispm-transcript--tool-use block (funcall state-function block)))
           ;; Results of earlier calls are inserted right where this
           ;; heading starts: the marker must move ahead of them.
           (push (cons (alist-get 'id block) (copy-marker start t)) calls))
          (_
           (push start folds)
           (insert (lispm-transcript--raw block))))))
    (lispm-transcript-fold-drawers beg (point))
    (mapc #'lispm-transcript-fold folds)
    (nreverse calls)))

(defun lispm-transcript-set-state (pos state)
  "Change the TODO keyword of the tool-call heading at POS to STATE."
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((case-fold-search nil))
        (when (looking-at (concat "\\*+ " lispm-transcript--state-re "\\_>"))
          (replace-match state t t nil 1))))))

(defun lispm-transcript-set-property (pos name value)
  "Set the property NAME of the heading at POS to VALUE; nil removes it.
The property goes in the drawer right under the heading, if it has one."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
      (let ((end (save-excursion
                   (and (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                        (copy-marker (match-beginning 0))))))
        (when end
          (if (re-search-forward (format "^[ \t]*:%s:.*\n" (regexp-quote name)) end t)
              (replace-match (if value (format ":%s: %s\n" name value) "") t t)
            (when value
              (goto-char end)
              (insert (format ":%s: %s\n" name value))))
          (set-marker end nil))))))

(defun lispm-transcript-tag (pos tag)
  "Add TAG to the heading at POS, unless it has it."
  (let ((tags (plist-get (lispm-transcript--heading pos) :tags)))
    (unless (member tag tags)
      (save-excursion
        (goto-char pos)
        (end-of-line)
        (skip-chars-backward " \t")
        (insert (if tags (concat tag ":") (concat " :" tag ":")))))))

(defun lispm-transcript-subtree-end (pos level)
  "End of the LEVEL subtree whose heading starts at POS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (if (re-search-forward (format "^\\*\\{1,%d\\} " level) nil t)
        (match-beginning 0)
      (point-max))))

;;;; Parsing

(defun lispm-transcript--sections (level beg end)
  "Sections of exactly LEVEL between BEG and END.
Each is a list (START BODY END): the heading start, the line after
it, and where the section ends."
  (save-excursion
    (goto-char beg)
    (let ((re (format "^\\*\\{%d\\} " level))
          starts)
      (while (re-search-forward re end t)
        (push (match-beginning 0) starts))
      (cl-loop for (start . rest) on (nreverse starts)
               collect (list start
                             (progn (goto-char start)
                                    (min end (1+ (line-end-position))))
                             (or (car rest) end))))))

(defun lispm-transcript--heading (pos)
  "Parse the heading at POS into a plist (:todo :title :tags)."
  (save-excursion
    (goto-char pos)
    (let ((line (buffer-substring-no-properties pos (line-end-position)))
          (case-fold-search nil))
      (when (string-match
             (concat "\\`\\*+ +\\(?:" lispm-transcript--state-re " +\\)?"
                     "\\(.*?\\)\\(?:[ \t]+\\(:\\(?:[[:alnum:]_@#%]+:\\)+\\)\\)?[ \t]*\\'")
             line)
        (list :todo (match-string 1 line)
              :title (match-string 2 line)
              :tags (and (match-string 3 line)
                         (split-string (match-string 3 line) ":" t)))))))

(defun lispm-transcript--text (beg end)
  "Read back text stored by `lispm-transcript-body' between BEG and END."
  (let ((s (buffer-substring-no-properties beg end)))
    (org-unescape-code-in-string
     (if (string-suffix-p "\n" s) (substring s 0 -1) s))))

(defun lispm-transcript--properties (beg end)
  "Alist of the properties in the first drawer between BEG and END."
  (save-excursion
    (goto-char beg)
    (let (props)
      (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" end t)
        (let ((drawer-end (save-excursion
                            (re-search-forward "^[ \t]*:END:[ \t]*$" end t))))
          (while (re-search-forward "^[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
                                    drawer-end t)
            (unless (equal (match-string 1) "END")
              (push (cons (match-string-no-properties 1)
                          (match-string-no-properties 2))
                    props)))))
      (nreverse props))))

(defun lispm-transcript--block (beg end)
  "Contents of the first #+begin_ block between BEG and END, or nil."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+begin_\\(\\S-+\\).*\n" end t)
        (let ((content-beg (point))
              (kind (match-string 1)))
          (when (re-search-forward
                 (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote kind)) end t)
            (lispm-transcript--text content-beg (match-beginning 0))))))))

(defun lispm-transcript--role (kind)
  "Role of a top-level section of KIND, (KIND . ARG) as
`lispm-transcript-kind' returns it.  Return `system', `assistant',
\(user . PREFIX) or nil for a section never sent."
  (pcase kind
    (`(system . ,_) 'system)
    (`(assistant . ,_) 'assistant)
    (`(,kind . ,arg)
     (when-let* ((prefix (nth 3 (assq kind lispm-transcript-kinds))))
       (cons 'user (format prefix arg))))))

(defun lispm-transcript--text-block (text)
  "A text content block holding TEXT."
  `((type . "text") (text . ,text)))

(defconst lispm-transcript-cleared-text
  "[This output was cleared to save context. Make the call again if you need it.]"
  "What requests carry in place of a tool result tagged :cleared:.")

(defun lispm-transcript--tool-result (id text errorp &optional images)
  "A tool_result block for the call ID with TEXT; ERRORP marks failure.
IMAGES are files the result holds besides TEXT."
  `((type . "tool_result") (tool_use_id . ,id)
    (content . ,(if images
                    (vconcat (list (lispm-transcript--text-block text))
                             (mapcar #'lispm-transcript--image-block images))
                  text))
    ,@(when errorp '((is_error . t)))))

(defun lispm-transcript--parts (beg end)
  "The parts of an assistant section whose body is between BEG and END.
See `lispm-transcript-parts'."
  (let* ((children (lispm-transcript--sections 2 beg end))
         (lead (string-trim (buffer-substring-no-properties
                             beg (if children (caar children) end))))
         parts)
    (unless (string-empty-p lead)
      (push (list :kind 'text :text (org-unescape-code-in-string lead)) parts))
    (pcase-dolist (`(,hbeg ,bbeg ,cend) children)
      (let* ((heading (lispm-transcript--heading hbeg))
             (kids (lispm-transcript--sections 3 bbeg cend))
             (own-end (if kids (caar kids) cend))
             (props (lispm-transcript--properties bbeg own-end)))
        (cond
         ((assoc "TOOL_ID" props)
          (let* ((json (lispm-transcript--block bbeg own-end))
                 (input (and json (ignore-errors (lispm--json-decode json))))
                 result preview reasons stopped)
            (pcase-dolist (`(,kbeg ,kbody ,kend) kids)
              (let* ((kid (lispm-transcript--heading kbeg))
                     (title (plist-get kid :title)))
                (cond ((and (member title '("Result" "Error")) (not result))
                       (setq result (list :result (or (lispm-transcript--block kbody kend) "")
                                          :error (equal title "Error")
                                          :cleared (and (member "cleared" (plist-get kid :tags)) t)
                                          :images (lispm-transcript--images kbody kend))))
                      ((and (equal title "Preview") (not preview) (not reasons))
                       (setq preview (lispm-transcript--block kbody kend)
                             reasons (lispm-transcript--reasons kbody kend)))
                      ((and (equal title "Stopped") (not stopped))
                       (setq stopped (lispm-transcript--text kbody kend))))))
            (push (append (list :kind 'tool
                                :id (cdr (assoc "TOOL_ID" props))
                                :name (cdr (assoc "TOOL" props))
                                :input (and (consp input) input)
                                :state (plist-get heading :todo)
                                :pos hbeg
                                :properties props
                                :preview preview
                                :reasons reasons
                                :stopped (and (assoc "CONDITION" props) stopped))
                          result)
                  parts)))
         ((assoc "BLOCK" props)
          (push (list :kind 'block
                      :type (cdr (assoc "BLOCK" props))
                      :block (when-let* ((json (lispm-transcript--block bbeg own-end)))
                               (ignore-errors (lispm--json-decode json)))
                      :pos hbeg)
                parts))
         ((equal (plist-get heading :title) "Reply")
          (push (list :kind 'text :text (lispm-transcript--text bbeg own-end)) parts)))))
    (nreverse parts)))

(defun lispm-transcript--parse-assistant (beg end)
  "Parse an assistant section body between BEG and END.
Return (ASSISTANT-BLOCKS RESULT-BLOCKS PENDING-CALLS), each pending
call a plist (:id :name :input :state :pos :properties), :pos being
where its heading starts and :properties the alist of its heading's
properties, :reasons, why it asks if a check said (see
`lispm-transcript-preview'), and :stopped, the error it stopped at, for
a call waiting at one (see `lispm-transcript-stopped')."
  (let (blocks results pending)
    (dolist (part (lispm-transcript--parts beg end))
      (pcase (plist-get part :kind)
        ('text
         (push (lispm-transcript--text-block (plist-get part :text)) blocks))
        ('tool
         (let ((id (plist-get part :id)))
           (push `((type . "tool_use") (id . ,id)
                   (name . ,(plist-get part :name))
                   (input . ,(plist-get part :input)))
                 blocks)
           (cond ((plist-member part :result)
                  (push (if (plist-get part :cleared)
                            (lispm-transcript--tool-result id lispm-transcript-cleared-text
                                                           (plist-get part :error))
                          (lispm-transcript--tool-result id (plist-get part :result)
                                                         (plist-get part :error)
                                                         (plist-get part :images)))
                        results))
                 ((member (plist-get part :state) '("WAITING" "APPROVED" "RUNNING"))
                  (push (list :id id :name (plist-get part :name)
                              :input (plist-get part :input)
                              :state (plist-get part :state) :pos (plist-get part :pos)
                              :properties (plist-get part :properties)
                              :reasons (plist-get part :reasons)
                              :stopped (plist-get part :stopped))
                        pending))
                 (t
                  (push (lispm-transcript--tool-result id "(no result recorded)" t)
                        results)))))
        ('block
         (when (and (plist-get part :block) (not (equal (plist-get part :type) "reasoning")))
           (push (plist-get part :block) blocks)))))
    (list (nreverse blocks) (nreverse results) (nreverse pending))))

(defun lispm-transcript--after-drawer (beg end)
  "Where the text under the property drawer between BEG and END starts:
right after it, or BEG without one."
  (save-excursion
    (goto-char beg)
    (if (re-search-forward "^[ \t]*:END:[ \t]*\n" end t) (point) beg)))

(defun lispm-transcript--condition (hbeg bbeg end)
  "The condition whose section starts at HBEG, its body at BBEG, up to END.
Nil if that section is not a condition.  Otherwise a plist: :id,
:type, :title, :state (its TODO keyword), :restarts, :restart (the
one chosen, once it is) and :value (the value it was given, if any),
:resume (see `lispm-transcript-condition'), :message and :pos (HBEG)."
  (let* ((heading (lispm-transcript--heading hbeg))
         (title (or (plist-get heading :title) "")))
    (when (string-match "\\`Condition: \\(.*\\)\\'" title)
      (let* ((name (match-string 1 title))
             (props (lispm-transcript--properties bbeg end))
             (text (lispm-transcript--after-drawer bbeg end))
             (restart (cdr (assoc "RESTART" props)))
             (value (cdr (assoc "VALUE" props))))
        (list :id (cdr (assoc "CONDITION_ID" props))
              :type (intern (or (cdr (assoc "CONDITION" props)) "condition"))
              :title name
              :state (plist-get heading :todo)
              :restarts (mapcar #'intern (split-string (or (cdr (assoc "RESTARTS" props)) "")))
              :restart (and restart (intern restart))
              :value (and value (ignore-errors (car (read-from-string value))))
              :resume (and (assoc "RESUME" props) t)
              :message (lispm-transcript--text text end)
              :pos hbeg)))))

(defun lispm-transcript--normalize (messages)
  "Merge consecutive MESSAGES of one role; drop leading non-user ones."
  (let (out)
    (dolist (m messages)
      (if (and out (equal (alist-get 'role (car out)) (alist-get 'role m)))
          (setcdr (assq 'content (car out))
                  (vconcat (alist-get 'content (car out)) (alist-get 'content m)))
        (push (copy-alist m) out)))
    (setq out (nreverse out))
    (while (and out (not (equal (alist-get 'role (car out)) "user")))
      (pop out))
    out))

;;;; Sections, as other code reads them

(defun lispm-transcript-sections (&optional beg end)
  "The top-level sections of the current buffer between BEG and END, by
default all of it, in order.  Each is a plist: :kind and :arg, as
`lispm-transcript-kind' reads its title, nil for the user's own; its
heading's :title, :todo and :tags; :ignore, non-nil when it is tagged
:ignore:, left out of requests; and :start, :body and :end, where its
heading and its body start and where it ends."
  (cl-loop for (start body end) in (lispm-transcript--sections 1 (or beg (point-min)) (or end (point-max)))
           for heading = (lispm-transcript--heading start)
           for title = (or (plist-get heading :title) "")
           for kind = (lispm-transcript-kind title)
           collect (list :kind (car kind) :arg (cdr kind)
                         :title title :todo (plist-get heading :todo) :tags (plist-get heading :tags)
                         :ignore (and (member "ignore" (plist-get heading :tags)) t)
                         :start start :body body :end end)))

(defun lispm-transcript-section-text (section)
  "The text of SECTION's body, as it was written, trimmed."
  (string-trim (org-unescape-code-in-string
                (buffer-substring-no-properties (plist-get section :body) (plist-get section :end)))))

(defun lispm-transcript-section-input (section)
  "The text requests carry for SECTION, a user's input: its text after its
kind's prefix.  Nil for a section left out or holding nothing."
  (when-let* (((not (plist-get section :ignore)))
              (role (lispm-transcript--role (cons (plist-get section :kind) (plist-get section :arg))))
              ((consp role))
              (text (lispm-transcript-section-text section))
              ((not (string-empty-p text))))
    (concat (cdr role) text)))

(defun lispm-transcript-parts (section)
  "The parts of the assistant SECTION, in order.  Each is a plist:
- (:kind text :text TEXT), a reply;
- (:kind tool ...), a tool call: :id, :name, :input, :state (its TODO
  keyword), :pos (where its heading starts), :properties (its drawer),
  :preview, :reasons (why it asked, see `lispm-transcript-preview'),
  :stopped (the error it waits at a restart after); once it
  has one, :result, :error, :cleared (tagged :cleared:) and :images;
- (:kind block :type TYPE :block BLOCK :pos POS), a content block kept
  as JSON, such as thinking, BLOCK decoded or nil."
  (lispm-transcript--parts (plist-get section :body) (plist-get section :end)))

(defun lispm-transcript-section-condition (section)
  "The condition SECTION holds, as `lispm-transcript--condition' reads it,
or nil."
  (lispm-transcript--condition (plist-get section :start) (plist-get section :body)
                               (plist-get section :end)))

(defun lispm-transcript-properties (beg end)
  "Alist of the properties in the first drawer between BEG and END."
  (lispm-transcript--properties beg end))

(defun lispm-transcript-text-after-drawer (beg end)
  "The text between BEG and END under the property drawer there, read back
as `lispm-transcript-body' stored it, or nil when there is none."
  (let ((start (lispm-transcript--after-drawer beg end)))
    (and (< start end) (lispm-transcript--text start end))))

(defun lispm-transcript-results ()
  "The tool results of the current session, oldest first.
Each is a list (POS BYTES CLEARED): where its heading starts, the size
in bytes of the output under it, and whether it is tagged :cleared:.
Results in responses that are not sent (tagged :ignore:) are left out."
  (save-excursion
    (save-restriction
      (widen)
      (let (results)
        (pcase-dolist (`(,hbeg ,bbeg ,end) (lispm-transcript--sections 1 (point-min) (point-max)))
          (let ((heading (lispm-transcript--heading hbeg)))
            (when (and heading
                       (not (member "ignore" (plist-get heading :tags)))
                       (eq (car (lispm-transcript-kind (plist-get heading :title))) 'assistant))
              (pcase-dolist (`(,_ ,cbeg ,cend) (lispm-transcript--sections 2 bbeg end))
                (pcase-dolist (`(,kbeg ,kbody ,kend) (lispm-transcript--sections 3 cbeg cend))
                  (let ((kid (lispm-transcript--heading kbeg)))
                    (when (member (plist-get kid :title) '("Result" "Error"))
                      (push (list kbeg
                                  (- (position-bytes kend) (position-bytes kbody))
                                  (and (member "cleared" (plist-get kid :tags)) t))
                            results))))))))
        (nreverse results)))))

(defvar-local lispm-transcript--folded nil
  "What `lispm-transcript-parse' read of the sections before the last one,
kept for the next parse while nothing before them changed: (END KINDS
SYSTEM MESSAGES PENDING CONDITION), END where the last section began,
KINDS the `lispm-transcript-kinds' they were read with, and the rest as
the parse gathers them, MESSAGES newest first.  Nothing in them is
changed in place: they are shared with what the parse returned.")

(defvar-local lispm-transcript--changed nil
  "The first position changed since `lispm-transcript-parse' last read the
buffer, or nil.")

(defun lispm-transcript--note-change (beg _end _length)
  "Note that the buffer changed from BEG on.
This is on `after-change-functions' of a buffer parsed once."
  (when (or (null lispm-transcript--changed) (< beg lispm-transcript--changed))
    (setq lispm-transcript--changed beg)))

(defun lispm-transcript-parse ()
  "Parse the current session buffer.
Return a plist: :system (the system prompt), :messages (Messages API
messages, oldest first), :pending (ids of tool calls without a result
yet), :pending-calls (those calls, as `lispm-transcript--parse-assistant'
describes them), :condition (the condition awaiting a restart, if any,
as `lispm-transcript--condition' describes it) and :last-role.
Only the last section grows as a turn goes on, so what was read of the
sections before it is kept, see `lispm-transcript--folded', and read
again only once something there changed: a request of a long session
read it all at every step."
  (save-excursion
    (save-restriction
      (widen)
      (unless (memq #'lispm-transcript--note-change after-change-functions)
        (add-hook 'after-change-functions #'lispm-transcript--note-change nil t)
        (setq lispm-transcript--folded nil))
      (pcase-let* ((folded lispm-transcript--folded)
                   (`(,from ,_ ,system ,messages ,pending ,condition)
                    (if (and folded
                             (eq (nth 1 folded) lispm-transcript-kinds)
                             (or (null lispm-transcript--changed)
                                 (> lispm-transcript--changed (car folded))))
                        folded
                      (list (point-min) nil nil nil nil nil)))
                   (sections (lispm-transcript-sections from))
                   (last (car (last sections))))
        (setq lispm-transcript--changed nil)
        (dolist (section sections)
          (when (eq section last)
            (setq lispm-transcript--folded
                  (list (plist-get section :start) lispm-transcript-kinds
                        system messages pending condition)))
          (let ((role (and (not (plist-get section :ignore))
                           (lispm-transcript--role (cons (plist-get section :kind)
                                                         (plist-get section :arg))))))
            (when (and (null role) (equal (plist-get section :todo) "WAITING"))
              (when-let* ((found (lispm-transcript-section-condition section)))
                (setq condition found)))
            (pcase role
              ('system (setq system (lispm-transcript--text (plist-get section :body)
                                                            (plist-get section :end))))
              (`(user . ,_)
               (when-let* ((text (lispm-transcript-section-input section)))
                 (push `((role . "user") (content . ,(vector (lispm-transcript--text-block text))))
                       messages)))
              ('assistant
               (pcase-let ((`(,blocks ,results ,calls)
                            (lispm-transcript--parse-assistant (plist-get section :body)
                                                               (plist-get section :end))))
                 (when blocks
                   (push `((role . "assistant") (content . ,(vconcat blocks))) messages))
                 (when results
                   (push `((role . "user") (content . ,(vconcat results))) messages))
                 (setq pending (append pending calls)))))))
        (setq messages (lispm-transcript--load-images
                        (lispm-transcript--normalize (reverse messages))))
        (list :system system
              :messages messages
              :pending (mapcar (lambda (call) (plist-get call :id)) pending)
              :pending-calls (copy-sequence pending)
              :condition condition
              :last-role (alist-get 'role (car (last messages))))))))

(provide 'lispm-transcript)
;;; lispm-transcript.el ends here
