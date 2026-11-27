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
(require 'lispm-chat-items)
(require 'lispm-chat-view)
(require 'yank-media)
(require 'dnd)

(defvar-local lispm-chat--footer nil
  "Overlay drawing the rule and hints below the input.")

(defvar-local lispm-chat--input-overlay nil
  "Overlay over the input, indenting its lines under the prompt's end.")

(defvar-local lispm-chat--history nil
  "Messages sent from this chat, newest first.")

(defvar-local lispm-chat--history-index nil
  "Position in `lispm-chat--history' while browsing it, or nil.")

(defcustom lispm-chat-menu-lines 10
  "Most / commands the menu under the input shows at once."
  :type 'natnum
  :group 'lispm-chat)

(defvar-local lispm-chat--menu-choice 0
  "Index of the command chosen in the menu of / commands.")


(defvar-local lispm-chat--jump-overlays nil
  "Window-specific jump-to-bottom button overlays in this chat buffer.")


(defvar-local lispm-chat--jump-timer nil
  "Idle timer debouncing `lispm-chat--refresh-jump-bars' after commands.")


;;;; Input

(defun lispm-chat--input ()
  "The text being written."
  (buffer-substring-no-properties lispm-chat-input-start (point-max)))

(defun lispm-chat-set-input (text)
  "Replace the text being written with TEXT."
  (let ((inhibit-read-only t))
    (delete-region lispm-chat-input-start (point-max))
    (goto-char (point-max))
    (insert text)))

(defun lispm-chat-in-input-p ()
  "Non-nil if point is in the text being written."
  (>= (point) lispm-chat-input-start))

(defalias 'lispm-chat--in-input-p #'lispm-chat-in-input-p)

(defvar lispm-chat-commands)

(defun lispm-chat--menu ()
  "The / commands the input may be, while it is a / and the start of a
command's name: those whose name starts so, then those holding it
elsewhere, as entries of `lispm-chat-commands'.  Nil otherwise, and
then no menu shows."
  (when (and (markerp lispm-chat-input-start) (marker-position lispm-chat-input-start))
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
    (lispm-chat-follow window (+ 3 (min (length menu) lispm-chat-menu-lines)))))

(defun lispm-chat--update-footer (&rest change)
  "Draw the rule and hints below the input, and a placeholder while it is empty.
While a / command's name is written, the commands it may be show in
place of the hints.  Run on a CHANGE of the input, the first of them is
chosen again."
  (save-match-data
    (when change
      (setq lispm-chat--menu-choice 0))
    (when (overlayp lispm-chat--footer)
      (let* ((menu (lispm-chat--menu))
             (footer
             (concat (if (and lispm-chat-input-start (= lispm-chat-input-start (point-max)))
                         (propertize (cond ((not (lispm-chat-prompting-p))
                                            (lispm-tr "Try \"explain this project\", or / for commands"))
                                           ((lispm-chat-condition)
                                            (lispm-tr "Choose above with its number, or write to stop and send"))
                                           (t (lispm-tr "Answer above with 1, 2 or 3")))
                                     'face 'lispm-chat-dim)
                       " ")
                     "\n" (lispm-chat-rule) "\n"
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
        (overlay-put lispm-chat--footer 'after-string footer)))))

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
        ((and (string-blank-p (lispm-chat--input)) (lispm-chat-prompting-p))
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
        (lispm-chat-schedule agent)))))

(defun lispm-chat-attach (file)
  "Attach FILE to the message being written: its link goes on a line of
its own, so the agent knows the file; an image or a video the agent
sees, and the chat shows.  Interactively, FILE is asked for."
  (interactive (list (read-file-name (lispm-tr "Attach file: ") nil nil t)) lispm-chat-mode)
  (unless (lispm-chat--in-input-p) (goto-char (point-max)))
  (let ((input (lispm-chat--input))
        (file (expand-file-name file))
        (root (lispm-chat-root)))
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
  (let ((type (symbol-name type)))
    (if (rassoc type lispm-transcript-image-types)
        (lispm-chat-attach
         (lispm-save-image lispm-chat-agent (base64-encode-string data t) type))
      (let ((chat (current-buffer))
            (agent lispm-chat-agent)
            (pasted (make-temp-file "lispm-pasted-" nil
                                    (concat "." (cadr (split-string type "/"))))))
        (with-temp-file pasted
          (set-buffer-multibyte nil)
          (insert data))
        (lispm-image-as-png-async
         pasted
         (lambda (png)
           (unwind-protect
               (if (not png)
                   (message "%s" (lispm-tr "Cannot attach a %s image" type))
                 (unwind-protect
                     (when (lispm-agent-p agent)
                       (let ((file (lispm-save-image
                                    agent
                                    (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (insert-file-contents-literally png)
                                      (base64-encode-string (buffer-string) t))
                                    "image/png")))
                         (when (buffer-live-p chat)
                           (with-current-buffer chat
                             (lispm-chat-attach file)))))
                   (when (file-exists-p png)
                     (ignore-errors (delete-file png)))))
             (when (file-exists-p pasted)
               (ignore-errors (delete-file pasted))))))))))

(defun lispm-chat--drop (url _action)
  "Attach the file dropped on the chat, URL, to the message being written."
  (when-let* ((file (dnd-get-local-file-name url t)))
    (lispm-chat-attach file)
    'private))

(defun lispm-chat-attach-command (_agent argument)
  "Attach the file ARGUMENT names, asked for if none, to the next message."
  (if (string-empty-p argument)
      (call-interactively #'lispm-chat-attach)
    (lispm-chat-attach (expand-file-name argument (lispm-chat-root)))))

(defun lispm-chat--in-prompt-p ()
  "Return non-nil when point sits within the read-only input prompt."
  (and (markerp lispm-chat-prompt-start)
       (markerp lispm-chat-input-start)
       (marker-position lispm-chat-prompt-start)
       (marker-position lispm-chat-input-start)
       (<= lispm-chat-prompt-start (point))
       (< (point) lispm-chat-input-start)))

(defun lispm-chat-beginning-of-line (&optional n)
  "Move point to the start of the current line, never entering the prompt.
Optional argument N specifies how many lines to move."
  (interactive "^p")
  (let ((prompt (and (markerp lispm-chat-prompt-start)
                     (marker-position lispm-chat-prompt-start)))
        (input (and (markerp lispm-chat-input-start)
                    (marker-position lispm-chat-input-start))))
    (if (and prompt input (>= (point) prompt))
        (progn
          (move-beginning-of-line n)
          (when (and (<= prompt (point)) (< (point) input))
            (goto-char input)))
      (if (bound-and-true-p visual-line-mode)
          (beginning-of-visual-line n)
        (move-beginning-of-line n)))))

(defun lispm-chat-end-of-line (&optional n)
  "Move point to the end of the current line, staying before the footer.
Optional argument N specifies how many lines to move."
  (interactive "^p")
  (let ((prompt (and (markerp lispm-chat-prompt-start)
                     (marker-position lispm-chat-prompt-start))))
    (if (and prompt (>= (point) prompt))
        (move-end-of-line n)
      (if (bound-and-true-p visual-line-mode)
          (end-of-visual-line n)
        (move-end-of-line n)))))

(defun lispm-chat-kill-line (&optional arg)
  "Kill the rest of the current line in the chat input, respecting the prompt.
Optional argument ARG specifies how many lines or direction to kill."
  (interactive "P")
  (when (lispm-chat--in-prompt-p)
    (goto-char lispm-chat-input-start))
  (let ((input (and (markerp lispm-chat-input-start)
                    (marker-position lispm-chat-input-start))))
    (cond
     ((and input (>= (point) input) (integerp arg) (<= arg 0))
      (kill-region
       (max input
            (save-excursion
              (lispm-chat-beginning-of-line (if (zerop arg) 1 (1+ arg)))
              (point)))
       (point)))
     ((and input (>= (point) input))
      (kill-line arg))
     ((bound-and-true-p visual-line-mode)
      (kill-visual-line arg))
     (t
      (kill-line arg)))))

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
    (unless (and (string-empty-p (lispm-chat--input)) (lispm-chat-answer n))
      (lispm-chat-self-insert))))

(defun lispm-chat-previous ()
  "Choose the command above in the menu of / commands, or select the
previous answer of an approval prompt, or move up a line."
  (interactive)
  (cond ((and (lispm-chat--in-input-p) (lispm-chat--menu))
         (lispm-chat--menu-move -1))
        ((and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
              (lispm-chat-prompting-p))
         (lispm-chat-choice-previous))
        (t (call-interactively #'previous-line))))

(defun lispm-chat-next ()
  "Choose the command below in the menu of / commands, or select the
next answer of an approval prompt, or move down a line."
  (interactive)
  (cond ((and (lispm-chat--in-input-p) (lispm-chat--menu))
         (lispm-chat--menu-move 1))
        ((and (lispm-chat--in-input-p) (string-empty-p (lispm-chat--input))
              (lispm-chat-prompting-p))
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
           (puthash key (not (gethash key lispm-chat-expanded)) lispm-chat-expanded)
           (lispm-chat-redraw-now))
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
    ("C-c C-y" ,(lispm-tr-noop "review the call awaiting approval, in full, in the org transcript"))
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
        (lispm-save-session agent)
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
  (unless lispm-read-session-function
    (user-error "Session selection is not available"))
  (let* ((session (funcall lispm-read-session-function (lispm-tr "Resume: ")))
         (resumed (lispm-resume-file (plist-get session :file))))
    (unless (eq resumed agent)
      (lispm-chat--replace agent resumed))))

(declare-function lispm-setup-model "lispm-backend-form" ())

(defun lispm-chat-model (agent argument)
  "Show AGENT's model, or switch it to ARGUMENT."
  (if (string-empty-p argument)
      (if (lispm-backend-name agent)
          (message "%s" (lispm-tr "%s uses %s via %s" (lispm-agent-name agent)
                                  (lispm-agent-model* agent) (lispm-backend-name agent)))
        (if (and (not noninteractive) (not (bound-and-true-p ert--running-tests)))
            (progn
              (require 'lispm-backend-form)
              (lispm-setup-model))
          (pcase-let ((`(,backend . ,model) (lispm-read-model agent)))
            (lispm-use-model agent backend model)
            (when (buffer-live-p (lispm-agent-view agent))
              (with-current-buffer (lispm-agent-view agent)
                (lispm-chat-redraw-now)))
            (message "%s" (lispm-tr "%s uses %s via %s" (lispm-agent-name agent)
                                    (lispm-agent-model* agent) (lispm-backend-name agent))))))
    (let* ((inferred (unless (lispm-backend-name agent)
                       (or (plist-get (seq-find (lambda (choice)
                                                  (equal (plist-get choice :model) argument))
                                                (and (fboundp 'lispm-model-choices)
                                                     (lispm-model-choices)))
                                      :backend)
                           (car (car lispm-backends))))))
      (lispm-use-model agent (or (lispm-agent-backend agent) inferred) argument)
      (when (buffer-live-p (lispm-agent-view agent))
        (with-current-buffer (lispm-agent-view agent)
          (lispm-chat-redraw-now)))
      (message "%s" (lispm-tr "%s now uses %s" (lispm-agent-name agent) argument)))))

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
  (pop-to-buffer (lispm-memory-buffer agent)))

(defun lispm-chat-cost (agent _argument)
  "Show the tokens AGENT used."
  (pcase-let ((`(,in ,out ,cached) (lispm-agent-usage agent)))
    (message "%s" (lispm-tr "%s: %d input, %d output, %d cached tokens (%s)"
                       (lispm-agent-name agent) in out cached (lispm-agent-model* agent)))))

(defun lispm-chat-agents (_agent _argument)
  "Show every agent."
  (unless lispm-open-dashboard-function
    (user-error "Dashboard is not available"))
  (funcall lispm-open-dashboard-function))

(defun lispm-chat-transcript (agent &optional _argument)
  "Show the org transcript of AGENT's conversation."
  (interactive (list lispm-chat-agent))
  (let ((buffer (lispm-session-buffer agent)))
    (with-current-buffer buffer
      (when buffer-file-name
        (rename-buffer (file-name-nondirectory buffer-file-name) t)))
    (pop-to-buffer buffer)))

(defun lispm-chat-review ()
  "Show the call awaiting approval in the org transcript, and decide."
  (interactive)
  (let ((agent lispm-chat-agent)
        (id (lispm-chat-waiting)))
    (lispm-chat-transcript agent)
    (lispm-review-call agent id)))

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
                  (skip-chars-backward "[:alnum:]_-" lispm-chat-input-start)
                  (and (eq (char-before) ?@) (point)))))
      (cond
       ((save-excursion
          (goto-char lispm-chat-input-start)
          (looking-at "/[a-z]*\\'"))
        (list (1+ lispm-chat-input-start) (point-max)
              (mapcar #'car lispm-chat-commands)
              :exclusive 'no))
       ((and name (lispm-characters))
        (list name (point) (mapcar #'lispm-character-title* (lispm-characters))
              :exclusive 'no))))))

;;;; The mode

(defun lispm-chat-scroll-to-bottom (&optional event)
  "Scroll the chat window to the bottom input prompt and focus it.
When invoked by mouse EVENT, select the clicked window first."
  (interactive (list last-input-event))
  (when (and event (mouse-event-p event))
    (when-let* ((win (posn-window (event-start event))))
      (when (window-live-p win)
        (select-window win))))
  (let ((win (get-buffer-window (current-buffer) t)))
    (setq lispm-chat-unseen-output nil)
    (goto-char (point-max))
    (when (window-live-p win)
      (set-window-point win (point-max))
      (lispm-chat-follow win))
    (lispm-chat--refresh-jump-bars)))

(defvar-keymap lispm-chat-jump-button-map
  :doc "Keymap on the floating jump-to-bottom pill in `lispm-chat-mode'."
  "<mouse-1>" #'lispm-chat-scroll-to-bottom
  "RET" #'lispm-chat-scroll-to-bottom)

(defun lispm-chat--jump-pill-string ()
  "Build the clickable jump-to-bottom pill shown when scrolled up."
  (let* ((keys "C-c C-b")
         (label (if lispm-chat-unseen-output
                    (lispm-tr "● New output · ↓ Jump to bottom (%s)" keys)
                  (lispm-tr "↓ Jump to bottom (%s)" keys))))
    (concat "  "
            (propertize (concat " " label " ")
                        'face (if lispm-chat-unseen-output
                                  '(:inherit lispm-chat-accent :inverse-video t :weight bold)
                                '(:inherit region :weight bold))
                        'mouse-face 'highlight
                        'pointer 'hand
                        'keymap lispm-chat-jump-button-map)
            "\n")))

(defun lispm-chat--window-scrolled-up-p (window)
  "Return non-nil if chat WINDOW is scrolled above its bottom prompt."
  (and (window-live-p window)
       (markerp lispm-chat-prompt-start)
       (let ((prompt (marker-position lispm-chat-prompt-start)))
         (and (> prompt (point-min))
              (< (window-point window) prompt)
              (if noninteractive
                  (> (count-lines (window-start window) prompt)
                     (max 1 (- (window-body-height window) 2)))
                (not (pos-visible-in-window-p prompt window)))))))

(defun lispm-chat--jump-pill-anchor (window)
  "Return a visible line-start position near the bottom of chat WINDOW."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (if noninteractive
          (progn
            (goto-char (window-start window))
            (forward-line (max 0 (- (window-body-height window) 3)))
            (line-beginning-position))
        (let ((wend (or (window-end window) (window-point window))))
          (goto-char (max (point-min) (1- wend)))
          (line-beginning-position))))))

(defun lispm-chat--refresh-jump-bars (&optional _win _start)
  "Show or hide the floating jump-to-bottom button across windows of this chat."
  (setq lispm-chat--jump-timer nil)
  (when (eq major-mode 'lispm-chat-mode)
    (mapc #'delete-overlay lispm-chat--jump-overlays)
    (setq lispm-chat--jump-overlays nil)
    (let ((any-up nil))
      (dolist (win (get-buffer-window-list (current-buffer) nil t))
        (when (lispm-chat--window-scrolled-up-p win)
          (setq any-up t)
          (let* ((end-pos (lispm-chat--jump-pill-anchor win))
                 (ov (make-overlay end-pos end-pos (current-buffer) t nil)))
            (overlay-put ov 'window win)
            (overlay-put ov 'priority 90)
            (overlay-put ov 'before-string (lispm-chat--jump-pill-string))
            (push ov lispm-chat--jump-overlays))))
      (unless any-up
        (setq lispm-chat-unseen-output nil)))))

(defun lispm-chat--schedule-jump-bars (&optional win _start)
  "Debounce jump-to-bottom overlay updates without forcing synchronous redisplay."
  (let ((buf (if (window-live-p win) (window-buffer win) (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (eq major-mode 'lispm-chat-mode)
                   (not (timerp lispm-chat--jump-timer)))
          (setq lispm-chat--jump-timer
                (run-with-idle-timer
                 0.04 nil
                 (lambda (b)
                   (when (buffer-live-p b)
                     (with-current-buffer b
                       (lispm-chat--refresh-jump-bars))))
                 buf)))))))

(defvar-keymap lispm-chat-mode-map
  :doc "Keymap of `lispm-chat-mode'."
  "<remap> <self-insert-command>" #'lispm-chat-self-insert
  "<remap> <end-of-buffer>" #'lispm-chat-scroll-to-bottom
  "C-a" #'lispm-chat-beginning-of-line
  "C-e" #'lispm-chat-end-of-line
  "C-k" #'lispm-chat-kill-line
  "<remap> <move-beginning-of-line>" #'lispm-chat-beginning-of-line
  "<remap> <beginning-of-visual-line>" #'lispm-chat-beginning-of-line
  "<remap> <move-end-of-line>" #'lispm-chat-end-of-line
  "<remap> <end-of-visual-line>" #'lispm-chat-end-of-line
  "<remap> <kill-line>" #'lispm-chat-kill-line
  "<remap> <kill-visual-line>" #'lispm-chat-kill-line
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
  "C-c C-b" #'lispm-chat-scroll-to-bottom
  "C-c C-c" #'lispm-chat-interrupt
  "C-c C-k" #'lispm-chat-interrupt
  "C-c C-t" #'lispm-chat-transcript
  "C-c C-y" #'lispm-chat-review
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
  (lispm-disable-line-numbers)
  (setq-local word-wrap t
              truncate-lines nil
              lispm-chat-expanded (make-hash-table :test #'equal)
              lispm-chat-rendered (make-hash-table :test #'equal))
  (visual-line-mode 1)
  ;; An image pasted or a file dropped is attached to the message.
  (yank-media-handler "image/.*" #'lispm-chat--yank-image)
  (setq-local dnd-protocol-alist (cons '("\\`file:" . lispm-chat--drop) dnd-protocol-alist))
  (add-hook 'completion-at-point-functions #'lispm-chat--complete nil t)
  (add-hook 'after-change-functions #'lispm-chat--update-footer nil t)
  (add-hook 'post-command-hook #'lispm-chat--show-menu nil t)
  (add-hook 'post-command-hook #'lispm-chat--schedule-jump-bars nil t)
  (add-hook 'window-scroll-functions #'lispm-chat--schedule-jump-bars nil t)
  (add-hook 'kill-buffer-hook #'lispm-chat--killed nil t)
  (add-hook 'window-size-change-functions #'lispm-chat--resized nil t)
  (add-hook 'display-line-numbers-mode-hook #'lispm-chat--resized nil t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize lispm-chat-prompt 'face 'lispm-chat-accent 'read-only t
                        'field 'lispm-chat-prompt
                        'front-sticky '(read-only) 'rear-nonsticky t))
    (setq lispm-chat-prompt-start (copy-marker (point-min) t)
          lispm-chat-input-start (point-marker)
          lispm-chat--input-overlay (make-overlay (point-max) (point-max) nil nil t)
          lispm-chat--footer (make-overlay (point-max) (point-max) nil t t))
    (let ((indent (make-string (string-width lispm-chat-prompt) ?\s)))
      (overlay-put lispm-chat--input-overlay 'line-prefix indent)
      (overlay-put lispm-chat--input-overlay 'wrap-prefix indent)))
  (when lispm-chat-agent
    (setq default-directory (lispm-agent-root lispm-chat-agent))
    (when (lispm-agent-working-p lispm-chat-agent)
      (setq lispm-chat-busy-since (float-time)))
    (lispm-chat-redraw-now)))

(defun lispm-chat--resized (&optional window)
  "Redraw the chat in WINDOW, or the current one, at its new width.
Run when a window shows it anew or at another size, and when line
numbers come or go."
  (with-current-buffer (if window (window-buffer window) (current-buffer))
    (when (and lispm-chat-agent lispm-chat-drawn)
      (lispm-chat-schedule lispm-chat-agent))))

(defun lispm-chat--killed ()
  "End the agent of a chat being killed, unless it is standing."
  (unless lispm-chat-closing
    (setq lispm-chat-closing t)
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
      (lispm-chat-hide-transcript (lispm-agent-buffer agent))
      (with-current-buffer chat
        (setq lispm-chat-agent agent)
        (lispm-chat-mode)
        (goto-char (point-max))))
    chat))

(defun lispm-chat--on-model-change (agent)
  "Redraw AGENT's chat view if open after AGENT switches model."
  (when (buffer-live-p (lispm-agent-view agent))
    (with-current-buffer (lispm-agent-view agent)
      (lispm-chat-redraw-now))))

(add-hook 'lispm-model-change-functions #'lispm-chat--on-model-change)
(setq lispm-open-transcript-function #'lispm-chat-transcript)

(setq lispm-chat-refresh-jump-bars-function #'lispm-chat--refresh-jump-bars
      lispm-chat-update-footer-function #'lispm-chat--update-footer)

(provide 'lispm-chat)
;;; lispm-chat.el ends here
