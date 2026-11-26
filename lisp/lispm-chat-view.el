;;; lispm-chat-view.el --- Chat transcript parsing, incremental redraw, and approval prompts  -*- lexical-binding: t; -*-

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

;; Org-to-item conversion, incremental chat buffer redrawing,
;; approval/restart prompt rendering, status spinner, and runtime
;; change hooks.

;;; Code:

(require 'lispm-core)
(require 'lispm-transcript)
(require 'lispm-runtime)
(require 'lispm-control-plane)
(require 'lispm-inspect)
(require 'lispm-sandbox)
(require 'lispm-chat-items)

(defvar-local lispm-chat-drawn nil
  "The strings shown before the prompt, in order.")

(defvar-local lispm-chat-prompt-start nil
  "Marker where the input prompt starts; everything before it is drawn.")

(defvar lispm-stage-prompt-position-function)

(defun lispm-chat-prompt-position (&optional buffer)
  "Return the prompt marker position in BUFFER (default current buffer), or nil."
  (let ((buf (or buffer (current-buffer))))
    (when (and (buffer-live-p buf)
               (local-variable-p 'lispm-chat-prompt-start buf))
      (let ((marker (buffer-local-value 'lispm-chat-prompt-start buf)))
        (when (markerp marker)
          (marker-position marker))))))

(setq lispm-stage-prompt-position-function #'lispm-chat-prompt-position)

(defvar-local lispm-chat-input-start nil
  "Marker where the text being written starts.")


(defvar-local lispm-chat--parsed nil
  "The items last read from the session, as a plist: :buffer and its :tick,
where the response streaming in started, :live, the :stable items before
it, and all the :items.")

(defvar-local lispm-chat--dirty 1
  "The first position of the session changed since it was last read.")

(defvar-local lispm-chat--choice 0
  "Answer selected in the approval prompt: 0, 1 or 2.")

(defvar-local lispm-chat--render-timer nil
  "Pending redraw.")

(defvar-local lispm-chat-busy-since nil
  "When the agent started working, or nil while it is idle.")

(defvar-local lispm-chat--asked nil
  "Id of the call the approval prompt was last shown for.")

(defvar-local lispm-chat-closing nil
  "Non-nil while this chat is being closed.")

(defcustom lispm-chat-defer-hidden-redraws t
  "When non-nil, defer redraws and spinner animation for hidden chat buffers."
  :type 'boolean
  :group 'lispm-chat)

(defvar-local lispm-chat--needs-redraw nil
  "Non-nil when a hidden chat buffer changed and should redraw once visible.")

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
         (let ((block (plist-get part :block)))
           (cond
            ((lispm-thought-block-p block)
             (when-let* ((text (lispm-thought-block-text block)))
               (push (list :kind 'thought :key (format "thought@%d" (plist-get part :pos))
                           :text (string-trim text))
                     items)))
            ((equal (plist-get part :type) "fallback")
             (push (list :kind 'note :text (lispm-tr "A fallback model took over this response.")) items)))))))
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
                                              (lispm-oneline text 70))))
                      items)))
          (pcase kind
            ('user
             (let ((text (lispm-transcript-section-text section)))
               (unless (string-empty-p text)
                 (push (list :kind 'user :text text
                             :images (lispm-transcript-images (plist-get section :body)
                                                               (plist-get section :end)))
                       items))))
            ('message
             (push (list :kind 'user :from (plist-get section :arg)
                         :text (lispm-transcript-section-text section)
                         :images (lispm-transcript-images (plist-get section :body)
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
  (let ((buffer (lispm-agent-buffer agent)))
    (when (buffer-live-p buffer)
      (let* ((tick (buffer-modified-tick buffer))
             (old lispm-chat--parsed))
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
            items))))))


;;;; Approval

(defun lispm-chat-waiting ()
  "Id of the call the approval prompt asks about."
  (or (car (lispm-waiting-calls lispm-chat-agent))
      (user-error "%s" (lispm-tr "Nothing awaits your approval"))))

(defun lispm-chat-condition (&optional agent)
  "The condition the prompt asks about, AGENT's or a call's, if any.
See `lispm-asked-condition'."
  (let ((agent (or agent lispm-chat-agent)))
    (and agent (lispm-asked-condition agent))))

(defun lispm-chat-prompting-p ()
  "Non-nil if the chat asks something: a call's approval, or a restart."
  (and lispm-chat-agent (lispm-awaiting lispm-chat-agent)))

(defun lispm-chat-restart (restart)
  "Go on from the condition the prompt asks about by RESTART."
  (lispm-choose-restart-named lispm-chat-agent restart (lispm-chat-condition))
  (goto-char (point-max)))

(defun lispm-chat-answer (n)
  "Give the Nth answer, from 1, to the prompt; non-nil if it has one."
  (if-let* ((condition (lispm-chat-condition)))
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
  (lispm-allow lispm-chat-agent (lispm-chat-waiting))
  (goto-char (point-max)))

(defun lispm-chat-no ()
  "Deny the call, saying what to do instead; or stop at the condition."
  (interactive)
  (lispm-deny-for lispm-chat-agent
                  (unless (lispm-chat-condition)
                    (lispm-chat-waiting)
                    (read-string (lispm-tr "What should it do instead? (optional) "))))
  (goto-char (point-max)))

(defun lispm-chat-choose ()
  "Give the selected answer to the prompt."
  (interactive)
  (lispm-chat-answer (1+ lispm-chat--choice)))

(defun lispm-chat--answers ()
  "How many answers the prompt offers."
  (if-let* ((condition (lispm-chat-condition)))
      (length (plist-get condition :restarts))
    3))

(defun lispm-chat-choice-next (&optional n)
  "Select the next answer in the prompt, or the Nth after."
  (interactive "p")
  (setq lispm-chat--choice (mod (+ lispm-chat--choice (or n 1)) (lispm-chat--answers)))
  (lispm-chat-redraw-now))

(defun lispm-chat-choice-previous ()
  "Select the previous answer in the approval prompt."
  (interactive)
  (lispm-chat-choice-next -1))

(defun lispm-chat--answer-command (n)
  "A command giving the Nth answer to the prompt."
  (lambda ()
    (interactive)
    (lispm-chat-answer n)))

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
           (shown (lispm-chat-tool-name name))
           (path (or (alist-get 'path input) (alist-get 'buffer input)))
           (why (alist-get 'why input))
           (item (seq-find (lambda (i) (equal (plist-get i :key) id)) items))
           (preview (plist-get item :preview))
           (subject (if (eq (lispm-chat-tool-get name :subject) 'name) (alist-get 'name input) path))
           (body (pcase (lispm-chat-tool-get name :ask)
                   ('diff (lispm-chat-render-diff-lines (or preview (lispm-chat-input-diff input))))
                   ;; The definition, as the preview quotes it from its file.
                   ('preview (split-string (or preview "") "\n"))
                   ;; The file a skill would be, or a project's skill's code;
                   ;; a preview that failed says why.
                   ('skill (or (lispm-chat-render-diff-lines preview)
                               (and preview (split-string (string-trim preview) "\n"))))
                   ('command (split-string (or (alist-get 'command input) "") "\n"))
                   ('code (split-string (or (alist-get 'code input) "") "\n"))
                   (_ (split-string (lispm-json-pretty input) "\n"))))
           (title (concat (or (lispm-chat-tool-text name :title input preview subject)
                              (lispm-tr "Use %s" shown))
                          (if (and (plist-get call :asking) (not (lispm-chat-tool-get name :alone)))
                              (concat " · " (lispm-tr "in the program %s runs"
                                                      (lispm-chat-tool-name (plist-get call :name))))
                            "")))
           (question (or (lispm-chat-tool-text name :question input preview subject)
                         (lispm-tr "Do you want to proceed?")))
           ;; What saying yes leads to, beyond the call itself.
           (note (lispm-chat-tool-text name :note input preview subject))
           (options (list (lispm-tr "Yes")
                          (pcase (lispm-call-allowance agent id)
                            (`(always . ,text) (lispm-tr text))
                            ('exact (lispm-tr (or (lispm-chat-tool-get name :exact)
                                                  (lispm-tr-noop "Yes, and don't ask again for this exact call"))))
                            (_ (lispm-tr "Yes, and don't ask again for %s calls by this agent" shown)))
                          (lispm-tr "No, and tell it what to do differently"))))
      (propertize
       (concat
        "\n"
        (lispm-chat-box
         (append (list (propertize title 'face 'bold) "")
                 (mapcar (lambda (l) (concat "  " l)) (seq-take body 30))
                 (and (> (length body) 30)
                      (list (propertize (concat "  " (lispm-tr-n "… +%d line" "… +%d lines" (- (length body) 30)))
                                        'face 'lispm-chat-dim)))
                 ;; Why a check of this Emacs has it ask.
                 (mapcan (lambda (reason)
                           (cons "" (mapcar (lambda (line)
                                              (propertize (concat "  " line) 'face 'lispm-chat-pending))
                                            (split-string (string-fill reason (- (lispm-chat-width) 8))
                                                          "\n"))))
                         (plist-get asked :reasons))
                 (and (stringp why) (list "" (propertize (concat "  " why) 'face 'lispm-chat-dim)))
                 (and note (list "" (propertize (concat "  " note) 'face 'lispm-chat-dim)))
                 (list "" question)
                 (lispm-chat--options options)
                 (list "" (propertize (lispm-tr "C-c C-y reviews the whole call in the transcript")
                                      'face 'lispm-chat-dim)))
         (lispm-chat-width) 'lispm-chat-pending))
       'keymap lispm-chat-approval-map))))

(defun lispm-chat--condition-prompt (agent)
  "The prompt for a restart of the condition AGENT's turn waits at, or nil."
  (when-let* ((condition (lispm-chat-condition agent)))
    (let ((width (lispm-chat-width)))
      (propertize
       (concat
        "\n"
        (lispm-chat-box
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
      (let* ((seconds (if lispm-chat-busy-since
                          (floor (- (float-time) lispm-chat-busy-since))
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
                         (propertize (lispm-chat-prompted
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

(defun lispm-chat-rule ()
  "A rule across the chat."
  (propertize (make-string (lispm-chat-width) ?─) 'face 'lispm-chat-dim))

(defun lispm-chat--strings (agent)
  "Everything AGENT's chat shows before the prompt, as a list of strings."
  (let ((items (lispm-chat--items agent)))
    (append (list (lispm-chat-read-only (lispm-chat-header agent)))
            (lispm-chat-item-strings items)
            (mapcar #'lispm-chat-read-only
                    (delq nil (list (lispm-chat--queued agent)
                                    (or (lispm-chat--approval agent items)
                                        (lispm-chat--condition-prompt agent))
                                    (lispm-chat--status agent)
                                    (concat "\n" (lispm-chat-rule) "\n")))))))


;;;; Drawing


(defvar-local lispm-chat-unseen-output nil
  "Non-nil when new output arrived while the chat window was scrolled up.")


(defun lispm-chat--shift-undo (undo-list delta threshold)
  "Shift UNDO-LIST positions at or after THRESHOLD by DELTA.
Returns an undo list preserving user input edits after `lispm-chat-redraw-now'."
  (cond
   ((not (listp undo-list)) undo-list)
   ((zerop delta) undo-list)
   (t
    (let ((shift-pos (lambda (p) (if (< p 0) (- (+ (- p) delta)) (+ p delta))))
          out)
      (dolist (item undo-list)
        (let ((shifted
               (cond
                ((null item) nil)
                ((integerp item)
                 (when (>= (abs item) threshold)
                   (funcall shift-pos item)))
                ((and (consp item) (integerp (car item)) (integerp (cdr item)))
                 (when (>= (car item) threshold)
                   (cons (+ (car item) delta) (+ (cdr item) delta))))
                ((and (consp item) (stringp (car item)) (integerp (cdr item)))
                 (when (>= (abs (cdr item)) threshold)
                   (cons (car item) (funcall shift-pos (cdr item)))))
                ((and (consp item) (eq (car item) t)) item)
                ((and (consp item) (markerp (car item)) (integerp (cdr item))) item))))
          (when (or shifted (and (null item) out (car out)))
            (push shifted out))))
      (nreverse out)))))

(defun lispm-chat-redraw-now ()
  "Bring the current chat up to date, from the first string that changed."
  (setq lispm-chat--needs-redraw nil)
  (let* ((strings (lispm-chat--strings lispm-chat-agent))
         (old lispm-chat-drawn)
         (new strings)
         (pos (point-min)))
    (while (and old new (equal-including-properties (car old) (car new)))
      (setq pos (+ pos (length (car old))) old (cdr old) new (cdr new)))
    (when (or old new)
      (let* ((inhibit-read-only t)
             (inhibit-modification-hooks t)
             (old-prompt (marker-position lispm-chat-prompt-start))
             (saved-undo buffer-undo-list)
             (buffer-undo-list t)
             (windows (mapcar (lambda (w)
                                (let* ((p (window-point w))
                                       (in-in (>= p lispm-chat-input-start))
                                       (at-bot (and (not (and (eq w (selected-window))
                                                              (region-active-p)))
                                                    (or (pos-visible-in-window-p
                                                         lispm-chat-prompt-start w)
                                                        (>= (or (window-end w) 0)
                                                            (marker-position
                                                             lispm-chat-prompt-start))))))
                                  (list w in-in at-bot (- (point-max) p))))
                              (get-buffer-window-list (current-buffer) nil t)))
             (in-input (>= (point) lispm-chat-input-start))
             (from-end (- (point-max) (point)))
             (here (point)))
        (save-excursion
          (delete-region pos lispm-chat-prompt-start)
          (goto-char pos)
          (apply #'insert new))
        (goto-char (if in-input (- (point-max) from-end) (min here lispm-chat-prompt-start)))
        (pcase-dolist (`(,w ,w-in ,w-bot ,end) windows)
          (cond
           (w-in
            (let ((target (- (point-max) end)))
              (set-window-point w target)
              (when (eq w (selected-window)) (goto-char target))
              (lispm-chat-follow w)))
           (w-bot
            (set-window-point w (point-max))
            (when (eq w (selected-window)) (goto-char (point-max)))
            (lispm-chat-follow w))
           (t
            (setq lispm-chat-unseen-output t)
            (set-window-point w (min (window-point w) lispm-chat-prompt-start)))))
        (setq buffer-undo-list
              (lispm-chat--shift-undo saved-undo
                                      (- (marker-position lispm-chat-prompt-start) old-prompt)
                                      old-prompt))))
    (setq lispm-chat-drawn strings)
    (when lispm-chat-update-footer-function
      (funcall lispm-chat-update-footer-function))
    (when lispm-chat-refresh-jump-bars-function
      (funcall lispm-chat-refresh-jump-bars-function))
    (lispm-chat--maybe-ask)))

(defun lispm-chat-follow (window &optional lines)
  "Scroll WINDOW so that the input, and the LINES (3) below it, are in sight:
the hints, or the menu of / commands."
  (let ((posn (posn-at-point (point-max) window))
        (lines (or lines 3)))
    (when (or (null posn)
              (> (+ (cdr (posn-x-y posn)) (* lines (default-line-height)))
                 (window-body-height window t)))
      (with-selected-frame (window-frame window)
        (with-selected-window window
          (let ((saved (point)))
            (goto-char (point-max))
            (recenter (- lines))
            (goto-char saved)))))))

(defun lispm-chat--maybe-ask ()
  "Select the first answer of a newly shown prompt."
  ;; A condition itself, or a call's id: a condition's id can be given again.
  (let ((id (or (lispm-asked-condition lispm-chat-agent)
                (car (lispm-awaiting lispm-chat-agent)))))
    (unless (equal id lispm-chat--asked)
      (setq lispm-chat--asked id
            lispm-chat--choice 0))))

(defun lispm-chat--visible-p (chat)
  "Return non-nil when CHAT is visible in a window or deferral is disabled."
  (and (buffer-live-p chat)
       (or (not lispm-chat-defer-hidden-redraws)
           (and (get-buffer-window chat 'visible) t))))

(defun lispm-chat-redraw (chat)
  "Redraw CHAT now."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (setq lispm-chat--render-timer nil
            lispm-chat--needs-redraw nil)
      (when (and lispm-chat-agent (buffer-live-p (lispm-agent-buffer lispm-chat-agent)))
        (lispm-chat-redraw-now)))))

(defun lispm-chat-schedule (agent)
  "Redraw AGENT's chat shortly, once for a burst of changes."
  (let ((chat (and agent (lispm-agent-view agent))))
    (when (buffer-live-p chat)
      (with-current-buffer chat
        (if (lispm-chat--visible-p chat)
            (unless (timerp lispm-chat--render-timer)
              (setq lispm-chat--render-timer (run-at-time 0.05 nil #'lispm-chat-redraw chat)))
          (setq lispm-chat--needs-redraw t))))))

(defun lispm-chat--state-changed (agent)
  "Note when AGENT starts or stops working, and redraw its chat."
  (let ((chat (lispm-agent-view agent)))
    (when (buffer-live-p chat)
      (lispm-control-plane-set-viewport agent (lispm-chat--visible-p chat))
      (with-current-buffer chat
        (if (not (lispm-agent-working-p agent))
            (setq lispm-chat-busy-since nil)
          (unless lispm-chat-busy-since
            (setq lispm-chat-busy-since (float-time)))))
      (lispm-chat--animate)
      (lispm-chat-schedule agent))))

(defun lispm-chat--busy-agents ()
  "The agents working while their chat is visible."
  (seq-filter (lambda (a) (and (lispm-chat--visible-p (lispm-agent-view a))
                               (lispm-agent-working-p a)))
              (lispm-agents)))

(defun lispm-chat--on-window-change (frame-or-window)
  "Flush pending redraws and restart spinner for visible chats in FRAME-OR-WINDOW."
  (let ((any-busy nil)
        (seen (make-hash-table :test #'eq)))
    (walk-windows
     (lambda (win)
       (let ((buf (window-buffer win)))
         (when (and (buffer-live-p buf) (not (gethash buf seen)))
           (puthash buf t seen)
           (with-current-buffer buf
             (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
               (lispm-control-plane-set-viewport lispm-chat-agent t)
               (when lispm-chat--needs-redraw
                 (lispm-chat-redraw buf))
               (when (lispm-agent-working-p lispm-chat-agent)
                 (setq any-busy t)))))))
     'no-minibuf
     (and (framep frame-or-window) frame-or-window))
    (dolist (agent (lispm-agents))
      (let ((chat (lispm-agent-view agent)))
        (when (and (buffer-live-p chat) (not (gethash chat seen)))
          (lispm-control-plane-set-viewport agent (lispm-chat--visible-p chat)))))
    (when any-busy
      (lispm-chat--animate))))

(defun lispm-chat--animate ()
  "Keep the spinners of working agents turning, and stop when none is."
  (let ((busy (lispm-chat--busy-agents)))
    (cond ((and busy (not (timerp lispm-chat--spinner-timer)))
           (setq lispm-chat--spinner-timer
                 (run-at-time 0.25 0.25 (lambda ()
                                          (mapc #'lispm-chat-schedule (lispm-chat--busy-agents))
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
      (lispm-chat-schedule lispm-session-agent))))

(defun lispm-chat-hide-transcript (buffer)
  "Give the org BUFFER a name that keeps it out of buffer lists."
  (with-current-buffer buffer
    (unless (string-prefix-p " " (buffer-name))
      (rename-buffer (format " *lispm transcript %s*" (file-name-base buffer-file-name)) t))))

(defun lispm-chat--watch-transcript ()
  "Redraw the chat when the session's org buffer changes; keep that buffer hidden."
  (when lispm-session-mode
    (add-hook 'after-change-functions #'lispm-chat--transcript-changed nil t)
    (when (and lispm-session-agent (buffer-live-p (lispm-agent-view lispm-session-agent)))
      (lispm-chat-hide-transcript (current-buffer)))))

(defun lispm-chat--session-ended (agent)
  "Close the chat of AGENT, whose session has closed."
  (when (lispm-agent-p agent)
    (let ((chat (lispm-agent-view agent)))
      (when (buffer-live-p chat)
        (setf (lispm-agent-view agent) nil)
        (with-current-buffer chat (setq lispm-chat-closing t))
        (kill-buffer chat)))))

(defun lispm-chat--sandbox-changed ()
  "Show the sandbox level now set in every chat."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
        (if (lispm-chat--visible-p buffer)
            (lispm-chat-redraw-now)
          (setq lispm-chat--needs-redraw t))))))

(defun lispm-chat--draw-all-again ()
  "Draw every chat again, whole: in the language now spoken, with code
checked or not as `lispm-ground-code' now says."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
        (setq lispm-chat--parsed nil
              lispm-chat-streamed nil)
        (when (hash-table-p lispm-chat-rendered)
          (clrhash lispm-chat-rendered))
        (if (lispm-chat--visible-p buffer)
            (lispm-chat-redraw-now)
          (setq lispm-chat--needs-redraw t))))))

(defun lispm-chat--on-media-ready (_file)
  "Invalidate cached item renderings and redraw chats when media previews finish."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-chat-mode) lispm-chat-agent)
        (setq lispm-chat--parsed nil
              lispm-chat-streamed nil)
        (when (hash-table-p lispm-chat-rendered)
          (clrhash lispm-chat-rendered))
        (lispm-chat-schedule lispm-chat-agent)))))

(add-hook 'lispm-chat-media-ready-functions #'lispm-chat--on-media-ready)
(add-hook 'window-buffer-change-functions #'lispm-chat--on-window-change)
(add-hook 'lispm-session-mode-hook #'lispm-chat--watch-transcript)
(add-hook 'lispm-state-change-functions #'lispm-chat--state-changed)
(add-hook 'lispm-session-end-functions #'lispm-chat--session-ended)
(add-hook 'lispm-sandbox-change-hook #'lispm-chat--sandbox-changed)
(add-hook 'lispm-language-changed-hook #'lispm-chat--draw-all-again)
(add-hook 'lispm-ground-change-hook #'lispm-chat--draw-all-again)
(add-hook 'lispm-settings-change-hook #'lispm-chat--draw-all-again)


(provide 'lispm-chat-view)

;;; lispm-chat-view.el ends here
