;;; lispm-terminal.el --- LispM as a terminal: sessions and a listener  -*- lexical-binding: t; -*-

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

;; LispM's terminal: LispM with nothing but a keyboard and conversations,
;; as a Lisp Machine's console held a listener and a who line.
;; M-x lispm-terminal makes the frame three panes, with no menu bar,
;; tool bar, tabs or scroll bars:
;;
;;   Sessions                    │ ╭──────────────────────────────╮
;;                               │ │ ✻ Welcome to LispM!  lispm   │
;;   ▸ λ LispM                   │ ╰──────────────────────────────╯
;;       Plan the week           │
;;                               │ λ >> what is on this week?
;;     Agents                    │
;;     Fix the build           ! │ ⏺ Three things: …
;;       assistant@lispm         │
;;                               │ ──────────────────────────────
;;                               │ λ >> █
;;     Explain the sandbox    2h │ ──────────────────────────────
;;       lispm                   │   / for commands · RET send · …
;;    Fri 23 Oct 14:02   ryan   ~   lispm   idle          gpt-5 via openai
;;
;; On the left, the sessions: LispM itself, the system's own agent (see
;; lispm-bar.el), then live and recorded sessions in opening order, each
;; session under the one that started it.  On the right, the listener:
;; the chat of one of them, where everything is done by asking.  At the
;; bottom, the who line, as on
;; the Lisp Machine: the time, who you are, the project and the agent
;; the listener talks to, what it is doing, and its model.
;;
;; Everything has a key, none of them a function key:
;;
;;   C-c C-s   the sessions, or back to the listener from them
;;   C-c C-n   the next agent alive, C-c C-p the previous one
;;   C-c C-j   any session, chosen by its title
;;   C-c C-o   a new session (with C-u, of a role chosen)
;;   C-c C-q   leave the terminal: the frame as it was
;;
;; and in the sessions: n and p move, RET talks to the session at point
;; and SPC only shows it, o opens a new one, k ends an agent, r renames
;; a session, / jumps to one, g lists them again, q goes back to the
;; listener, ? lists the keys.
;;
;; From a shell, emacs -nw -f lispm-terminal starts Emacs as LispM's
;; terminal; from anywhere, asking LispM for the terminal, or console or
;; TUI mode, enters it, and asking to leave it leaves it (the skill
;; terminal).

;;; Code:

(require 'lispm)
(require 'lispm-agents)

(defgroup lispm-terminal nil
  "LispM as a terminal: the sessions, a listener and a who line."
  :group 'lispm
  :prefix "lispm-terminal-")

(defcustom lispm-terminal-sessions-width 32
  "Columns of the sessions, on the left of the terminal."
  :type 'natnum)

(defcustom lispm-terminal-recent 100
  "How many recent recorded sessions the terminal lists.
Their ancestors and those of live sessions are included as well."
  :type 'natnum)

(defface lispm-terminal-who-line '((t :inverse-video t :extend t))
  "Face of the who line, at the bottom of the terminal."
  :group 'lispm-terminal)

(defface lispm-terminal-dim '((t :inherit lispm-chat-dim))
  "Face of the headings and details of the sessions."
  :group 'lispm-terminal)

(defface lispm-terminal-current '((t :weight bold))
  "Face of the title of the session the listener shows."
  :group 'lispm-terminal)

(defface lispm-terminal-selected '((t :inherit highlight :extend t))
  "Face of the session at point, in the sessions."
  :group 'lispm-terminal)

(defconst lispm-terminal--bare
  '((menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil))
  "Frame parameters of the terminal: no menu bar, tool bar, tabs or scroll bars.")

(defconst lispm-terminal--sessions-name "*LispM Sessions*"
  "Name of the buffer of the sessions.")

(defconst lispm-terminal--who-name " *LispM who line*"
  "Name of the buffer of the who line.")

(defvar lispm-terminal--frame nil
  "The frame that is LispM's terminal, or nil.")

(defvar lispm-terminal--saved nil
  "What the terminal's frame was before, as (CONFIGURATION . PARAMETERS):
its windows, and the values of the parameters `lispm-terminal--bare' set.")

(defvar lispm-terminal--listener nil
  "The window of the listener.")

(defvar lispm-terminal--agent nil
  "The agent the listener last showed.")

(defvar lispm-terminal--recorded nil
  "The recorded sessions listed, as `lispm-sessions' gives them; nil to
read them again.")

(defvar lispm-terminal--timer nil
  "Timer drawing the terminal again, once for a burst of changes.")

(defvar lispm-terminal--clock nil
  "Timer drawing the terminal again every minute: the time, the ages.")

(defvar lispm-terminal-mode)

;;;; The listener

(defun lispm-terminal-p (&optional frame)
  "Non-nil if FRAME, by default the selected one, is LispM's terminal."
  (and lispm-terminal-mode
       (frame-live-p lispm-terminal--frame)
       (eq (or frame (selected-frame)) lispm-terminal--frame)))

(defun lispm-terminal--side-p (window)
  "Non-nil if WINDOW is a side window: the sessions, the who line."
  (window-parameter window 'window-side))

(defun lispm-terminal--listener-window ()
  "The window of the listener, found again if it went."
  (unless (and (window-live-p lispm-terminal--listener)
               (eq (window-frame lispm-terminal--listener) lispm-terminal--frame))
    (let ((windows (seq-remove #'lispm-terminal--side-p
                               (window-list lispm-terminal--frame 'nomini))))
      (setq lispm-terminal--listener
            (or (seq-find (lambda (window)
                            (with-current-buffer (window-buffer window)
                              (derived-mode-p 'lispm-chat-mode)))
                          windows)
                (car windows)))
      (set-window-parameter lispm-terminal--listener 'mode-line-format 'none)))
  lispm-terminal--listener)

(defun lispm-terminal-agent ()
  "The agent the listener talks to: the one it shows, or showed last."
  (when (frame-live-p lispm-terminal--frame)
    (or (buffer-local-value 'lispm-chat-agent (window-buffer (lispm-terminal--listener-window)))
        (and (memq lispm-terminal--agent (lispm-agents)) lispm-terminal--agent))))

(defvar-keymap lispm-terminal-map
  :doc "The keys of LispM's terminal, in the listener and in the sessions."
  "C-c C-s" #'lispm-terminal-sessions
  "C-c C-n" #'lispm-terminal-next
  "C-c C-p" #'lispm-terminal-previous
  "C-c C-j" #'lispm-terminal-jump
  "C-c C-o" #'lispm-terminal-new
  "C-c C-q" #'lispm-terminal-quit)

(define-minor-mode lispm-terminal-keys-mode
  "The keys of LispM's terminal, in a buffer it shows.

\\{lispm-terminal-map}"
  :keymap lispm-terminal-map)

(defun lispm-terminal-show (agent &optional stay)
  "Show AGENT's chat in the listener, and go there unless STAY.
Return the chat."
  (let ((chat (lispm-chat agent))
        (window (lispm-terminal--listener-window)))
    (with-current-buffer chat
      (lispm-terminal-keys-mode 1)
      ;; A column apart from the line beside it; set for the buffer, as
      ;; whatever shows it in the listener again keeps it so.
      (setq-local left-margin-width 1
                  right-margin-width 1))
    (set-window-dedicated-p window nil)
    (set-window-buffer window chat)
    (set-window-point window (with-current-buffer chat (point-max)))
    (setq lispm-terminal--agent agent)
    (unless stay
      (select-window window))
    (lispm-terminal--schedule)
    chat))

(defun lispm-terminal--alive ()
  "The agents alive, in the same tree order as the sessions list."
  (delq nil (mapcar (lambda (entry) (plist-get entry :agent))
                    (lispm-terminal--entries))))

(defun lispm-terminal-next (&optional n)
  "Talk to the next agent alive in the sessions; with N, the Nth next."
  (interactive "p")
  (let* ((agents (lispm-terminal--alive))
         (here (or (seq-position agents (lispm-terminal-agent)) 0)))
    (unless (cdr agents)
      (user-error "%s" (lispm-tr "No other agent is alive")))
    (lispm-terminal-show (nth (mod (+ here (or n 1)) (length agents)) agents))))

(defun lispm-terminal-previous (&optional n)
  "Talk to the previous agent alive in the sessions; with N, the Nth one."
  (interactive "p")
  (lispm-terminal-next (- (or n 1))))

(defun lispm-terminal-new (directory &optional role)
  "Open a new session in the project of DIRECTORY, and talk to it.
DIRECTORY defaults to the project of the agent the listener talks to.
With a prefix argument, choose its ROLE as well."
  (interactive
   (let ((agent (lispm-terminal-agent)))
     (list (read-directory-name (lispm-tr "New session in: ")
                                (if agent (lispm-agent-root agent) default-directory)
                                nil t)
           (and current-prefix-arg (lispm-read-role (lispm-tr "Role"))))))
  (lispm-terminal-show (lispm-start role (lispm--project-root directory))))

(defun lispm-terminal--session-ended (agent)
  "List the sessions again now that AGENT's closed; if the listener showed
it, show another."
  (let ((window (and (frame-live-p lispm-terminal--frame) (lispm-terminal--listener-window))))
    (when (and window (eq (window-buffer window) (lispm-agent-view agent)))
      ;; Once its chat is gone, which this hook's other functions see to.
      (run-at-time 0 nil (lambda ()
                           (when (and (lispm-terminal-p lispm-terminal--frame)
                                      (window-live-p window)
                                      (not (buffer-local-value 'lispm-chat-agent (window-buffer window))))
                             (let ((lispm-terminal--listener window))
                               (lispm-terminal-show
                                (or (seq-find (lambda (other)
                                                (and (not (eq other agent))
                                                     (buffer-live-p (lispm-agent-buffer other))))
                                              (cdr (lispm-terminal--alive)))
                                    (lispm-system-agent))
                                (not (eq (selected-window) window)))))))))
  (lispm-terminal--schedule t))

;;;; The sessions

(defun lispm-terminal--align-right (text)
  "Space up to where TEXT, put after it, ends a column short of the right
edge: a terminal shows `$' in the last one when something reaches it."
  (propertize " " 'display `(space :align-to (- right ,(+ 2 (string-width text))))))

(defun lispm-terminal--recorded ()
  "All recorded sessions, including ancestors outside the recent preview."
  (or lispm-terminal--recorded
      (setq lispm-terminal--recorded (lispm-sessions))))

(defun lispm-terminal--session-node (entry)
  "Return ENTRY's identity and ancestry for the common session tree."
  (let* ((agent (plist-get entry :agent))
         (session (plist-get entry :session))
         (id (plist-get entry :id)))
    (list :agent 'lispm
          ;; A standing agent can be declared before its first session.
          :id (or id (format "live:%s" (lispm-agent-name agent)))
          :parent (unless (eq (plist-get entry :kind) 'system)
                    (if agent (lispm-agent-parent agent) (plist-get session :parent)))
          :started (if agent (lispm--session-started id) (plist-get session :started))
          :entry entry)))

(defun lispm-terminal--entries ()
  "The sessions the terminal lists, as plists: :kind, `system', `agent'
or `recorded'; :id, the session's id; and :agent, or :session, a
recorded session as `lispm-sessions' describes it.  :depth is its tree
indentation.  Siblings sort by opening time, with LispM itself first.
Live sessions replace their recorded rows; the recent preview retains
every available ancestor of its sessions."
  (let* ((system (lispm-system-agent))
         (alive (cons system (seq-remove #'lispm-system-agent-p (lispm-agents))))
         (nodes (make-hash-table :test #'equal))
         (selected (make-hash-table :test #'equal)))
    (dolist (session (lispm-terminal--recorded))
      (let ((node (lispm-terminal--session-node
                   (list :kind 'recorded :id (plist-get session :id) :session session))))
        (puthash (plist-get node :id) node nodes)))
    (dolist (agent alive)
      (let ((node (lispm-terminal--session-node
                   (list :kind (if (eq agent system) 'system 'agent)
                         :id (lispm-agent-id agent) :agent agent))))
        (puthash (plist-get node :id) node nodes)
        (puthash (plist-get node :id) t selected)))
    (let* ((sessions
            (sort (hash-table-values nodes)
                  (lambda (a b)
                    (let ((ta (lispm-agents-session-opened-time a))
                          (tb (lispm-agents-session-opened-time b)))
                      (if (= ta tb) (string< (plist-get a :id) (plist-get b :id))
                        (> ta tb))))))
           (recent (seq-take (seq-filter
                              (lambda (node)
                                (eq (plist-get (plist-get node :entry) :kind) 'recorded))
                              sessions)
                            lispm-terminal-recent)))
      (dolist (node recent) (puthash (plist-get node :id) t selected))
      ;; Keep context even when a newly resumed child has an old parent.
      (dolist (node sessions)
        (when (gethash (plist-get node :id) selected)
          (let ((parent (plist-get node :parent)))
            (while (and parent (gethash parent nodes) (not (gethash parent selected)))
              (puthash parent t selected)
              (setq parent (plist-get (gethash parent nodes) :parent))))))
      (let* ((trees (lispm-agents-session-tree
                     (seq-filter (lambda (node) (gethash (plist-get node :id) selected)) sessions)))
             (system-tree (seq-find (lambda (tree)
                                      (eq (plist-get (plist-get (car tree) :entry) :kind) 'system))
                                    trees)))
        (cl-labels ((flatten (tree depth)
                      (cons (plist-put (plist-get (car tree) :entry) :depth depth)
                            (mapcan (lambda (child) (flatten child (1+ depth))) (cdr tree)))))
          (mapcan (lambda (tree) (flatten tree 0))
                  (cons system-tree (delq system-tree trees))))))))

(defvar lispm-terminal--first-prompts (make-hash-table :test #'equal)
  "The first message of each session listed, by id: read once, as reading
it parses the whole session.")

(defun lispm-terminal--conversation (agent)
  "What AGENT's conversation is about: its title or its first message."
  (or (lispm-session-title agent)
      (gethash (lispm-agent-id agent) lispm-terminal--first-prompts)
      (when-let* (((buffer-live-p (lispm-agent-buffer agent)))
                  (prompt (lispm-title--first-prompt agent)))
        (puthash (lispm-agent-id agent) prompt lispm-terminal--first-prompts))))

(defun lispm-terminal--title (entry)
  "The title the sessions show ENTRY by."
  (pcase (plist-get entry :kind)
    ('system "λ LispM")
    ('agent (or (lispm-terminal--conversation (plist-get entry :agent))
                (lispm-tr "New session")))
    (_ (plist-get (plist-get entry :session) :title))))

(defun lispm-terminal--detail (entry)
  "What the sessions say below ENTRY's title."
  (pcase (plist-get entry :kind)
    ('system (lispm-terminal--conversation (plist-get entry :agent)))
    ('agent (symbol-name (lispm-agent-name (plist-get entry :agent))))
    (_ (let ((session (plist-get entry :session)))
         (or (plist-get session :agent) (lispm--project-name (plist-get session :root)))))))

(defun lispm-terminal--mark (entry)
  "What the sessions show on the right of ENTRY: an agent's state, a
recorded session's age."
  (if-let* ((agent (plist-get entry :agent)))
      (cond ((lispm-awaiting agent) (propertize "!" 'face 'lispm-chat-pending))
            ((lispm-agent-working-p agent) (propertize "✻" 'face 'lispm-chat-accent))
            (t ""))
    (propertize (lispm--age (plist-get (plist-get entry :session) :time))
                'face 'lispm-terminal-dim)))

(defun lispm-terminal--entry-string (entry width current)
  "ENTRY as the sessions show it, WIDTH columns wide; CURRENT if the
listener shows it."
  (let* ((indent (make-string (* 2 (or (plist-get entry :depth) 0)) ?\s))
         (mark (lispm-terminal--mark entry))
         (title (lispm--oneline (lispm-terminal--title entry)
                                (max 4 (- width 5 (length indent) (string-width mark)))))
         (detail (lispm-terminal--detail entry)))
    (propertize
     (concat indent (if current (propertize "▸ " 'face 'lispm-chat-accent) "  ")
             (if current (propertize title 'face 'lispm-terminal-current) title)
             (lispm-terminal--align-right mark)
             mark "\n"
             (if (and detail (not (string-empty-p detail)))
                 (concat indent "    " (propertize (lispm--oneline detail (max 4 (- width 5 (length indent))))
                                           'face 'lispm-terminal-dim)
                         "\n")
               ""))
     'lispm-terminal-entry entry)))

(defun lispm-terminal--heading (text)
  "A heading of the sessions saying TEXT."
  (concat "\n  " (propertize text 'face 'lispm-terminal-dim) "\n"))

(defun lispm-terminal--sessions-text (width)
  "The text of the sessions, WIDTH columns wide."
  (let* ((entries (lispm-terminal--entries))
         (current (lispm-terminal-agent))
         (id (and current (lispm-agent-id current)))
         (string (lambda (entry)
                   (lispm-terminal--entry-string entry width (equal (plist-get entry :id) id))))
         (others (cdr entries)))
    (concat (propertize (lispm-tr "Sessions") 'face 'bold)
            (lispm-terminal--align-right (lispm-tr "? keys"))
            (propertize (lispm-tr "? keys") 'face 'lispm-terminal-dim)
            "\n\n"
            (funcall string (car entries))
            (when others
              (concat (lispm-terminal--heading
                       (if (seq-some (lambda (entry) (eq (plist-get entry :kind) 'agent)) others)
                           (lispm-tr "Agents") (lispm-tr "Recent")))
                      (mapconcat string others ""))))))

(defun lispm-terminal--entry-at (&optional pos)
  "The entry of the sessions at POS, by default point."
  (get-text-property (or pos (point)) 'lispm-terminal-entry))

(defun lispm-terminal--entry-start (pos)
  "Where the entry at POS starts."
  (if (and (> pos (point-min))
           (lispm-terminal--entry-at pos)
           (eq (lispm-terminal--entry-at (1- pos)) (lispm-terminal--entry-at pos)))
      (previous-single-property-change pos 'lispm-terminal-entry nil (point-min))
    pos))

(defun lispm-terminal--next-entry (pos)
  "Where the entry after the one at POS starts, or nil."
  (let ((next (next-single-property-change pos 'lispm-terminal-entry)))
    (while (and next (not (lispm-terminal--entry-at next)))
      (setq next (next-single-property-change next 'lispm-terminal-entry)))
    next))

(defun lispm-terminal--previous-entry (pos)
  "Where the entry before the one at POS starts, or nil."
  (let ((previous (lispm-terminal--entry-start pos))
        (found nil))
    (while (and (not found) (> previous (point-min)))
      (setq previous (previous-single-property-change previous 'lispm-terminal-entry nil (point-min)))
      (when (lispm-terminal--entry-at previous)
        (setq found previous)))
    found))

(defun lispm-terminal--find (id)
  "Where the entry of the session ID starts, or nil."
  (let ((pos (point-min)) (found nil))
    (while (and pos (not found))
      (if (equal (plist-get (lispm-terminal--entry-at pos) :id) id)
          (setq found pos)
        (setq pos (lispm-terminal--next-entry pos))))
    found))

(defun lispm-terminal-next-entry (&optional n)
  "Move to the next session in the list; with N, N sessions on."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (when-let* ((pos (if (> n 0)
                           (lispm-terminal--next-entry (point))
                         (lispm-terminal--previous-entry (point)))))
        (goto-char pos)))))

(defun lispm-terminal-previous-entry (&optional n)
  "Move to the previous session in the list; with N, N sessions back."
  (interactive "p")
  (lispm-terminal-next-entry (- (or n 1))))

(defvar-local lispm-terminal--selection nil
  "Overlay marking the session at point.")

(defun lispm-terminal--select-entry (&rest _)
  "Mark the session at point while the sessions are selected, as a cursor
would; the listener's own is marked anyway."
  (when (overlayp lispm-terminal--selection)
    (if-let* (((eq (window-buffer (selected-window)) (current-buffer)))
              ((lispm-terminal--entry-at))
              (start (lispm-terminal--entry-start (point))))
        (move-overlay lispm-terminal--selection start
                      (or (next-single-property-change start 'lispm-terminal-entry) (point-max)))
      (delete-overlay lispm-terminal--selection))))

(defun lispm-terminal--agent-of (entry)
  "The agent of ENTRY, its session resumed if it is only recorded."
  (or (plist-get entry :agent)
      (let ((session (plist-get entry :session)))
        (or (lispm-resume-session (plist-get session :id))
            (lispm-resume-file (plist-get session :file))))))

(defun lispm-terminal--entry-at-point ()
  "The entry at point, or an error saying there is none."
  (or (lispm-terminal--entry-at)
      (user-error "%s" (lispm-tr "No session here"))))

(defun lispm-terminal-talk (entry)
  "Talk to the session ENTRY, by default the one at point, in the listener."
  (interactive (list (lispm-terminal--entry-at-point)))
  (lispm-terminal-show (lispm-terminal--agent-of entry)))

(defun lispm-terminal-peek (entry)
  "Show the session ENTRY, by default the one at point, staying in the list."
  (interactive (list (lispm-terminal--entry-at-point)))
  (lispm-terminal-show (lispm-terminal--agent-of entry) t))

(defun lispm-terminal-end (entry)
  "End the agent of ENTRY, by default the one at point; its session stays
on record."
  (interactive (list (lispm-terminal--entry-at-point)))
  (pcase (plist-get entry :kind)
    ('system (message "%s" (lispm-tr "LispM itself goes on; /clear starts a new conversation")))
    ('agent (let ((agent (plist-get entry :agent)))
              (when (or (not (lispm-agent-busy-p agent))
                        (y-or-n-p (lispm-tr "%s is at work; end it? " (lispm-agent-name agent))))
                (lispm-retire agent))))
    (_ (message "%s" (lispm-tr "This session has ended already")))))

(defun lispm-terminal--file (entry)
  "The file recording the session ENTRY, or nil."
  (if-let* ((agent (plist-get entry :agent)))
      (and (buffer-live-p (lispm-agent-buffer agent))
           (buffer-file-name (lispm-agent-buffer agent)))
    (plist-get (plist-get entry :session) :file)))

(defun lispm-terminal-rename (entry title)
  "Give the session ENTRY, by default the one at point, the TITLE shown."
  (interactive
   (let ((entry (lispm-terminal--entry-at-point)))
     (list entry (read-string (lispm-tr "Title: ")
                              (if (plist-get entry :agent)
                                  (lispm-session-title (plist-get entry :agent))
                                (lispm-terminal--title entry))))))
  (let ((file (or (lispm-terminal--file entry)
                  (user-error "%s" (lispm-tr "This session is not recorded yet")))))
    (lispm-rename-session file title)
    (lispm-terminal--schedule t)))

(defun lispm-terminal--read-entry (prompt)
  "Read a session with PROMPT, by its title; return its entry."
  (let* ((table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (entry)
                           (let ((label (format "%s  %s"
                                                (lispm--oneline (lispm-terminal--title entry) 60)
                                                (propertize (or (lispm-terminal--detail entry) "")
                                                            'face 'shadow))))
                             (while (gethash label table)
                               (setq label (concat label " ")))
                             (puthash label entry table)
                             label))
                         (lispm-terminal--entries))))
    (gethash (completing-read prompt
                              (lambda (string pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity))
                                  (complete-with-action action labels string pred)))
                              nil t)
             table)))

(defun lispm-terminal-jump (entry)
  "Talk to the session ENTRY, chosen by its title."
  (interactive (list (lispm-terminal--read-entry (lispm-tr "Session: "))))
  (lispm-terminal-talk entry))

(defun lispm-terminal-to-listener ()
  "Go back to the listener."
  (interactive)
  (select-window (lispm-terminal--listener-window)))

(defun lispm-terminal--revert (&rest _)
  "List the sessions again, reading the recorded ones anew."
  (setq lispm-terminal--recorded nil)
  (lispm-terminal-redraw))

(defvar-keymap lispm-terminal-sessions-mode-map
  :doc "Keymap of `lispm-terminal-sessions-mode'."
  "n" #'lispm-terminal-next-entry
  "p" #'lispm-terminal-previous-entry
  "<remap> <next-line>" #'lispm-terminal-next-entry
  "<remap> <previous-line>" #'lispm-terminal-previous-entry
  "RET" #'lispm-terminal-talk
  "SPC" #'lispm-terminal-peek
  "o" #'lispm-terminal-new
  "k" #'lispm-terminal-end
  "r" #'lispm-terminal-rename
  "/" #'lispm-terminal-jump
  "q" #'lispm-terminal-to-listener
  "TAB" #'lispm-terminal-to-listener)

(define-derived-mode lispm-terminal-sessions-mode special-mode "LispM Sessions"
  "The sessions of LispM's terminal: LispM itself, the agents alive, the
sessions recorded.

\\{lispm-terminal-sessions-mode-map}
\\{lispm-terminal-map}"
  :group 'lispm-terminal
  (setq-local truncate-lines t
              left-margin-width 1
              cursor-in-non-selected-windows nil
              revert-buffer-function #'lispm-terminal--revert
              lispm-terminal--selection (make-overlay (point-min) (point-min)))
  (overlay-put lispm-terminal--selection 'face 'lispm-terminal-selected)
  ;; On a text terminal, a line between the sessions and the listener,
  ;; as the chat draws its rules.  The buffer's, not the window's: set
  ;; on a window already shown, rows Emacs did not draw again lost it.
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table 'vertical-border
                          (make-glyph-code ?│ 'vertical-border))
  (lispm-terminal-keys-mode 1)
  (add-hook 'post-command-hook #'lispm-terminal--select-entry nil t)
  (add-hook 'window-selection-change-functions #'lispm-terminal--select-entry nil t)
  (add-hook 'window-size-change-functions #'lispm-terminal--changed nil t))

(defun lispm-terminal--sessions-buffer ()
  "The buffer of the sessions, made if need be."
  (or (get-buffer lispm-terminal--sessions-name)
      (with-current-buffer (get-buffer-create lispm-terminal--sessions-name)
        (lispm-terminal-sessions-mode)
        (current-buffer))))

(defun lispm-terminal--draw-sessions ()
  "Draw the sessions again, point on the same one."
  (with-current-buffer (lispm-terminal--sessions-buffer)
    (let* ((window (get-buffer-window (current-buffer) lispm-terminal--frame))
           (width (if window (window-max-chars-per-line window) lispm-terminal-sessions-width))
           (at (plist-get (lispm-terminal--entry-at) :id))
           (start (and window (window-start window)))
           (inhibit-read-only t))
      (erase-buffer)
      (insert (lispm-terminal--sessions-text width))
      (goto-char (or (and at (lispm-terminal--find at))
                     (let ((agent (lispm-terminal-agent)))
                       (and agent (lispm-terminal--find (lispm-agent-id agent))))
                     (point-min)))
      (when window
        (set-window-start window (min start (point-max)) t)
        (set-window-point window (point)))
      (lispm-terminal--select-entry))))

(defun lispm-terminal-sessions ()
  "Go to the sessions, on the one the listener shows; from there, back to
the listener."
  (interactive)
  (if (eq (current-buffer) (get-buffer lispm-terminal--sessions-name))
      (lispm-terminal-to-listener)
    (lispm-terminal--panes)
    (when-let* ((window (get-buffer-window lispm-terminal--sessions-name lispm-terminal--frame)))
      (select-window window)
      (when-let* ((agent (lispm-terminal-agent))
                  (pos (lispm-terminal--find (lispm-agent-id agent))))
        (goto-char pos)
        (lispm-terminal--select-entry)))))

;;;; The who line

(defun lispm-terminal--who-buffer ()
  "The buffer of the who line, made if need be."
  (or (get-buffer lispm-terminal--who-name)
      (with-current-buffer (get-buffer-create lispm-terminal--who-name)
        (setq-local cursor-type nil
                    cursor-in-non-selected-windows nil
                    truncate-lines t
                    window-size-fixed 'height
                    buffer-read-only t)
        (current-buffer))))

(defun lispm-terminal--state (agent)
  "What AGENT is doing, as the who line says it."
  (cond ((lispm-agent-condition agent) (lispm-tr "stopped"))
        ((lispm-waiting-calls agent) (lispm-tr "awaiting approval"))
        (t (lispm--state-name (lispm-agent-state agent)))))

(defun lispm-terminal--who-line ()
  "The text of the who line."
  (let* ((agent (lispm-terminal-agent))
         (others (length (seq-filter (lambda (other) (and (not (eq other agent)) (lispm-awaiting other)))
                                     (lispm-agents))))
         (left (delq nil (list (lispm-format-time "%a %e %b %H:%M")
                               (user-login-name)
                               (and agent (abbreviate-file-name (lispm-agent-root agent)))
                               (and agent (symbol-name (lispm-agent-name agent)))
                               (and agent (lispm-terminal--state agent))
                               (and (> others 0)
                                    (lispm-tr-n "%d other awaits you" "%d others await you" others)))))
         (right (string-join
                 (delq nil (list (unless (eq lispm-sandbox-level 'none)
                                   (lispm-tr "sandbox: %s" (lispm-sandbox-level-name lispm-sandbox-level)))
                                 (when-let* ((agent)
                                             (backend (lispm-backend-name agent)))
                                   (lispm-tr "%s via %s" (or (lispm-agent-model* agent) "?") backend))))
                 "   ")))
    (concat " " (string-join left "   ")
            (lispm-terminal--align-right right)
            right " \n")))

(defun lispm-terminal--draw-who-line ()
  "Draw the who line again."
  (with-current-buffer (lispm-terminal--who-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (lispm-terminal--who-line) 'face 'lispm-terminal-who-line))
      (goto-char (point-min)))))

;;;; The terminal

(defun lispm-terminal-redraw ()
  "Draw the sessions and the who line again."
  (when (timerp lispm-terminal--timer)
    (cancel-timer lispm-terminal--timer))
  (setq lispm-terminal--timer nil)
  (when (frame-live-p lispm-terminal--frame)
    (lispm-terminal--draw-sessions)
    (lispm-terminal--draw-who-line)))

(defun lispm-terminal--schedule (&optional reread)
  "Draw the terminal again shortly, once for a burst of changes; with
REREAD non-nil, the recorded sessions read anew."
  (when reread
    (setq lispm-terminal--recorded nil))
  (unless (timerp lispm-terminal--timer)
    (setq lispm-terminal--timer (run-at-time 0.1 nil #'lispm-terminal-redraw))))

(defun lispm-terminal--changed (&rest _)
  "Draw the terminal again: something it shows changed."
  (lispm-terminal--schedule))

(defun lispm-terminal--session-opened ()
  "List a session just opened, which may be one recorded before."
  (when lispm-session-mode
    (lispm-terminal--schedule t)))

(defun lispm-terminal--scrolled (window _start)
  "Draw the sessions again whole if WINDOW, the listener, scrolled on a
text terminal.  Emacs then moves the screen's lines, the sessions' part
with them, but forgets the line between the panes on them: the next
change to the listener on such a line wrote a space over it."
  (when (and (eq window lispm-terminal--listener)
             (not (display-graphic-p (window-frame window))))
    ;; After this redisplay, which moves the lines and loses the line.
    (run-at-time 0 nil (lambda ()
                         (when-let* ((sessions (get-buffer-window lispm-terminal--sessions-name
                                                                  lispm-terminal--frame)))
                           (force-window-update sessions))))))

(defun lispm-terminal--tick ()
  "Draw the terminal again as a minute passed: the time, the ages."
  (lispm-terminal--schedule t))

(defun lispm-terminal--frame-deleted (frame)
  "Leave the terminal if its FRAME is deleted."
  (when (eq frame lispm-terminal--frame)
    (setq lispm-terminal--saved nil)
    (lispm-terminal-quit)))

(defconst lispm-terminal--hooks
  '((lispm-state-change-functions . lispm-terminal--changed)
    (lispm-session-end-functions . lispm-terminal--session-ended)
    (lispm-title-change-functions . lispm-terminal--changed)
    (lispm-agent-declared-functions . lispm-terminal--changed)
    (lispm-sandbox-change-hook . lispm-terminal--changed)
    (lispm-language-changed-hook . lispm-terminal--changed)
    (lispm-session-mode-hook . lispm-terminal--session-opened)
    (window-buffer-change-functions . lispm-terminal--changed)
    (window-scroll-functions . lispm-terminal--scrolled)
    (delete-frame-functions . lispm-terminal--frame-deleted))
  "The hooks that keep the terminal up to date, and their functions.")

(define-minor-mode lispm-terminal-mode
  "Keep LispM's terminal up to date: its sessions and its who line.
`lispm-terminal' turns it on, and `lispm-terminal-quit' off, which
gives the frame back as it was."
  :global t
  :group 'lispm-terminal
  (pcase-dolist (`(,hook . ,function) lispm-terminal--hooks)
    (if lispm-terminal-mode
        (add-hook hook function)
      (remove-hook hook function)))
  (dolist (timer (list lispm-terminal--clock lispm-terminal--timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq lispm-terminal--clock nil lispm-terminal--timer nil)
  (when lispm-terminal-mode
    ;; On the minute, as the time shows minutes.
    (setq lispm-terminal--clock (run-at-time (- 60 (mod (float-time) 60)) 60
                                             #'lispm-terminal--tick))))

(defun lispm-terminal--panes ()
  "Show the sessions and the who line beside the listener."
  (with-selected-window (lispm-terminal--listener-window)
    (let ((parameters '((no-delete-other-windows . t) (mode-line-format . none)
                        (header-line-format . none) (tab-line-format . none)))
          ;; The who line has no mode line: one line is a window.
          (window-min-height 1))
      (display-buffer-in-side-window
       (lispm-terminal--who-buffer)
       `((side . bottom) (slot . 0) (window-height . 1) (dedicated . t)
         (window-parameters . ((no-other-window . t) ,@parameters))))
      (display-buffer-in-side-window
       (lispm-terminal--sessions-buffer)
       `((side . left) (slot . 0) (window-width . ,lispm-terminal-sessions-width)
         (preserve-size . (t . nil)) (dedicated . t)
         (window-parameters . ,parameters)))))
  (lispm-terminal-redraw))

;;;###autoload
(defun lispm-terminal (&optional agent)
  "Make the selected frame LispM's terminal: the sessions on the left, the
listener on the right, the who line at the bottom, and nothing else.
The listener talks to AGENT, or else to the agent it talked to last, or
to LispM itself.  Run it again to lay the terminal out again;
\\[lispm-terminal-quit] leaves it.  From a shell, emacs -nw -f
lispm-terminal starts Emacs so."
  (interactive)
  (let ((frame (selected-frame)))
    (unless (eq frame lispm-terminal--frame)
      (when (frame-live-p lispm-terminal--frame)
        (lispm-terminal-quit))
      (setq lispm-terminal--saved
            (cons (current-window-configuration frame)
                  (mapcar (lambda (parameter)
                            (cons (car parameter) (frame-parameter frame (car parameter))))
                          lispm-terminal--bare))
            lispm-terminal--frame frame))
    ;; Run from the command line, Emacs is still starting, and would show
    ;; its startup screen in the listener next.
    (unless inhibit-startup-hooks
      (setq inhibit-startup-screen t))
    (modify-frame-parameters frame lispm-terminal--bare)
    (let ((agent (or agent (lispm-terminal-agent) (lispm-system-agent)))
          (window (frame-selected-window frame)))
      (when (window-minibuffer-p window)
        (setq window (frame-first-window frame)))
      ;; Side windows too, whoever's they are.
      (let ((ignore-window-parameters t))
        (delete-other-windows window))
      (dolist (parameter '(window-side window-slot no-other-window no-delete-other-windows))
        (set-window-parameter window parameter nil))
      (setq lispm-terminal--listener window)
      (set-window-parameter window 'mode-line-format 'none)
      (lispm-terminal-mode 1)
      (lispm-terminal-show agent)
      (lispm-terminal--panes))))

;;;; Asked of an agent
;;
;; The skill terminal (skills/terminal.org) gives agents the tools
;; terminal_enter and terminal_leave, so that LispM enters the terminal
;; and leaves it when asked to, in whatever words.  Another program may
;; enter and leave it its own way, as in a frame of its own: it says how
;; through the two variables below.

(defvar lispm-terminal-enter-function #'lispm-terminal-enter
  "Function entering LispM's terminal for the tool terminal_enter.
It is called with the agent asked, for the listener to show, and
returns what to tell that agent, a string, or nil for the usual words.")

(defvar lispm-terminal-leave-function #'lispm-terminal-leave
  "Function leaving LispM's terminal for the tool terminal_leave.
It is called with the agent asked, and returns what to tell it, a
string, or nil for the usual words.")

(defun lispm-terminal-soon (function)
  "Call FUNCTION once the command or the tool call under way is over, and
nothing is read in the minibuffer: the LispM bar, if open, is closed
for it, as what it asked for replaces it.  The functions of
`lispm-terminal-enter-function' and `lispm-terminal-leave-function'
switch through it."
  (run-at-time 0 nil
               (lambda ()
                 (cond ((lispm-bar-open-p) (lispm-bar-close-then function))
                       ((active-minibuffer-window)
                        (letrec ((once (lambda ()
                                         (remove-hook 'minibuffer-exit-hook once)
                                         (run-at-time 0 nil function))))
                          (add-hook 'minibuffer-exit-hook once)))
                       (t (funcall function))))))

(defun lispm-terminal-enter (agent)
  "Enter LispM's terminal in the selected frame, its listener talking to
AGENT: what terminal_enter does unless `lispm-terminal-enter-function'
says otherwise."
  (lispm-terminal-soon (lambda () (lispm-terminal agent)))
  nil)

(defun lispm-terminal-leave (_agent)
  "Leave LispM's terminal, its frame as it was: what terminal_leave does
unless `lispm-terminal-leave-function' says otherwise."
  (if (not (frame-live-p lispm-terminal--frame))
      "LispM's terminal is not open: there is nothing to leave."
    (lispm-terminal-soon #'lispm-terminal-quit)
    nil))

(defun lispm-terminal-tool-enter (agent)
  "Enter LispM's terminal as AGENT was asked to; what to tell it."
  (or (funcall lispm-terminal-enter-function agent)
      "LispM's terminal opens now: the sessions on the left, this conversation in the \
listener on the right, the who line below.  Go on there."))

(defun lispm-terminal-tool-leave (agent)
  "Leave LispM's terminal as AGENT was asked to; what to tell it."
  (or (funcall lispm-terminal-leave-function agent)
      "LispM's terminal closes now: what was there before it comes back."))

(defun lispm-terminal-quit ()
  "Leave LispM's terminal: its frame as it was before, the agents at work."
  (interactive)
  (let ((frame lispm-terminal--frame)
        (saved lispm-terminal--saved))
    (lispm-terminal-mode -1)
    (setq lispm-terminal--frame nil
          lispm-terminal--saved nil
          lispm-terminal--listener nil)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when lispm-terminal-keys-mode
          (lispm-terminal-keys-mode -1)
          (kill-local-variable 'left-margin-width)
          (kill-local-variable 'right-margin-width))))
    (when (and (frame-live-p frame) saved)
      (modify-frame-parameters frame (cdr saved))
      (set-window-configuration (car saved)))
    (dolist (name (list lispm-terminal--sessions-name lispm-terminal--who-name))
      (when (get-buffer name)
        (kill-buffer name)))))

(provide 'lispm-terminal)
;;; lispm-terminal.el ends here
