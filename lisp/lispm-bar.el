;;; lispm-bar.el --- The LispM bar: one field for the whole system  -*- lexical-binding: t; -*-

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

;; The LispM bar is the way in to the whole system, as Spotlight is a
;; Mac's.  M-x ai (`lispm-ai') opens a field; what you type there finds
;; what is installed at once, and is otherwise asked of LispM itself,
;; the system's own agent (`lispm-system-agent'): one standing agent, of
;; the whole computer rather than of a project, keeping one
;; conversation going.  The chat shows the same conversation, all of it
;; (`lispm-ai-chat').
;;
;;   ❯ Ask LispM “web”                                          ↩
;;     Browse the web  Search the web and read pages, …     Skill
;;   ▸ C-o more · C-c C-o chat · C-g close
;;   λ web█
;;
;; Return on a thing that opens (what other packages add, as a
;; desktop's applications) opens it and closes the bar; on anything
;; else it asks LispM, naming the skill or tool chosen.  The field stays
;; open: the reply streams in over it, under a line saying what LispM
;; does, and a call awaiting approval is answered there, y or n.  C-o
;; opens the bar out: the conversation so far, and more of the skills
;; and tools the words find, described.  C-g or Escape closes it; LispM
;; goes on working, and the next M-x ai shows how far it got.
;;
;; What the bar finds comes from `lispm-bar-source-functions', ranked as
;; search_tools ranks (`lispm-tools-search-score'): the skills and tools
;; LispM has, and whatever other packages add.  It shows in the
;; minibuffer and a window over it, in plain Emacs as on a terminal;
;; `lispm-bar-read-function', `lispm-bar-show-function' and
;; `lispm-bar-hide-function' let another package show it elsewhere.
;; Nothing here needs more than LispM Agent.  With no model to ask, the
;; bar still opens what it
;; finds, and "Ask LispM" becomes "Set up a model…"
;; (`lispm-bar-setup-function'); what was asked waits until a model
;; works (`lispm-bar-model-ready').

;;; Code:

(require 'lispm-core)
(require 'lispm-runtime)
(require 'lispm-transcript)
(require 'lispm-tools)
(require 'lispm-skills)
(require 'lispm-chat)

(declare-function lispm-show "lispm" (agent))

(defgroup lispm-bar nil
  "The LispM bar: one field to find what is installed and to ask LispM."
  :group 'lispm
  :prefix "lispm-bar-")

(defface lispm-bar-selected '((t :inherit highlight :extend t))
  "Face of the line Return acts on."
  :group 'lispm-bar)

(defface lispm-bar-dim '((t :inherit lispm-chat-dim))
  "Face of what the bar says on the side: kinds, details, hints."
  :group 'lispm-bar)

(defface lispm-bar-heading '((t :inherit bold))
  "Face of the headings of the bar opened out."
  :group 'lispm-bar)

(defface lispm-bar-activity '((t :inherit lispm-chat-accent))
  "Face of the line saying what LispM is doing."
  :group 'lispm-bar)

(defface lispm-bar-question '((t :inherit lispm-chat-pending))
  "Face of what LispM asks: a call awaiting approval, a way to go on."
  :group 'lispm-bar)

;;;; LispM itself

(defcustom lispm-system-agent-name 'lispm
  "Name of LispM itself, the system's own agent that the bar talks to."
  :type 'symbol
  :group 'lispm-bar)

(defcustom lispm-system-agent-root "~/"
  "Directory LispM itself works in: relative paths are relative to it."
  :type 'directory
  :group 'lispm-bar)

(defconst lispm-bar--instructions
  "You are LispM itself: the agent of this whole computer and of the Emacs it runs, not of one project. The user reaches you from the LispM bar, a one-line field with a small window for your reply, and from a chat; both are this one conversation. You act for the user across Emacs and the computer: find what the system and what is installed can do (search_tools finds their skills, the manuals with their tools), open and change things, and answer questions. Your reply first shows in that small window: lead with the answer or with what you did, and keep it short unless asked for more."
  "What LispM itself is told it is, beyond what every agent is told.")

(lispm-defrole system
  "LispM itself: the agent of the whole system, which the LispM bar talks to."
  :instructions lispm-bar--instructions)

(defun lispm-system-agent ()
  "LispM itself: the standing agent of the whole system.
It is declared when first needed.  It works in `lispm-system-agent-root',
the home directory, with the role `system', and keeps one conversation
going across Emacs sessions, which the bar and the chat both show.  It
is not one of a project's agents."
  (let ((agent (lispm-get-agent lispm-system-agent-name t)))
    (if (and agent (lispm-agent-standing agent))
        agent
      (lispm-register-agent lispm-system-agent-name :role 'system :root lispm-system-agent-root))))

(defun lispm-system-agent-p (agent)
  "Non-nil if AGENT is LispM itself, see `lispm-system-agent'."
  (and (lispm-agent-p agent)
       (lispm-agent-standing agent)
       (eq (lispm-agent-name agent) lispm-system-agent-name)))

;;;###autoload
(defun lispm-ai-chat ()
  "Show the conversation with LispM itself in a chat, all of it.
The bar shows the same conversation, its latest reply."
  (interactive)
  (lispm-show (lispm-system-agent)))

;;;; What the bar finds

(defcustom lispm-bar-results 5
  "How many of the things it finds the bar shows.
Opened out, it shows three times as many."
  :type 'natnum
  :group 'lispm-bar)

(defvar lispm-bar-source-functions (list #'lispm-bar-skills #'lispm-bar-tools)
  "Functions giving the bar what it can find as the user types.
Each is called with no argument, as the bar opens, and returns a list
of plists, one for each thing:
  :kind    what it is: a symbol of `lispm-bar-kinds', as `application'
  :label   what it is, shown beside it, for a kind not there
  :name    its name, as shown
  :icon    a string shown before the name, as an icon, optional
  :detail  a line saying more of it, optional
  :fields  what it is found by, as `lispm-tools-search-score' takes
           them; by default, its name
  :open    a function of no argument opening it, for a thing Return
           opens, as an application: the bar closes, then it runs
  :ask     for another, a sentence for LispM about it, which Return
           adds to what the user asks, as \"Use the skill notes.\"
Things of the sources earlier in the list come first among those the
words match as well.  LispM's own are its skills and tools; another
package may add more, as a desktop its applications.")

(defconst lispm-bar-kinds
  `((application ,(lispm-tr-noop "Application") ,(lispm-tr-noop "Applications"))
    (skill ,(lispm-tr-noop "Skill") ,(lispm-tr-noop "Skills"))
    (tool ,(lispm-tr-noop "Tool") ,(lispm-tr-noop "Tools")))
  "The kinds of things the bar finds: (KIND SINGULAR PLURAL).
The names are English, translated where shown.")

(defun lispm-bar--kind-label (thing &optional plural)
  "What THING is, as the bar shows it beside it; with PLURAL, as a heading."
  (if-let* ((names (alist-get (plist-get thing :kind) lispm-bar-kinds)))
      (lispm-tr (if plural (nth 1 names) (car names)))
    (or (plist-get thing :label) "")))

(defun lispm-bar-skills ()
  "The skills LispM itself can load, for the bar to find.
Return asks LispM, naming the skill chosen."
  (let ((agent (lispm-system-agent)))
    (when (lispm-tools-allowed-p agent "skill")
      (mapcar (lambda (skill)
                (list :kind 'skill
                      :name (or (plist-get skill :title) (plist-get skill :name))
                      :detail (plist-get skill :description)
                      :fields (lispm-skills-search-fields skill)
                      :ask (format "Use the skill %s." (plist-get skill :name))))
              (lispm-skills agent)))))

(defun lispm-bar-tools ()
  "The tools LispM itself may call, offered or deferred, for the bar to find.
Return asks LispM, naming the tool chosen."
  (let ((agent (lispm-system-agent))
        tools)
    (maphash (lambda (name tool)
               (when (lispm-tools-allowed-p agent name)
                 (push (list :kind 'tool
                             :name name
                             :detail (lispm-tools--description tool)
                             :fields (lispm-tools--fields tool)
                             :ask (format "Use the tool %s." name))
                       tools)))
             lispm-tools--registry)
    (sort tools (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defvar lispm-bar--things nil
  "What the bar can find, as its sources gave it when it opened.")

(defun lispm-bar--gather ()
  "Ask the sources what the bar can find, and keep it."
  (setq lispm-bar--things
        (mapcan (lambda (source)
                  (condition-case err
                      (copy-sequence (funcall source))
                    (error (message "LispM bar: %s: %s" source (error-message-string err))
                           nil)))
                lispm-bar-source-functions)))

(defun lispm-bar-find (text &optional limit)
  "What the bar finds for TEXT, best first, at most LIMIT things.
The things are plists, as `lispm-bar-source-functions' give them, ranked
as search_tools ranks what it finds; none for TEXT without a word.
While the bar is open, they are what the sources gave as it opened."
  (when-let* ((words (lispm-tools-search-words text)))
    (lispm-tools--rank words (if (and (lispm-bar-open-p) lispm-bar--things)
                                 lispm-bar--things
                               (lispm-bar--gather))
                       (or limit lispm-bar-results)
                       (lambda (thing)
                         (or (plist-get thing :fields) `((3 . ,(plist-get thing :name))))))))

;;;; A model to ask

(defvar lispm-bar--open)

(defvar lispm-bar-ready-function #'lispm-bar-model-ready-p
  "Function telling whether LispM has a model to ask.
It is called with the system agent and returns non-nil when it has.
A setup of models of its own sets it, with `lispm-bar-setup-function'.")

(defvar lispm-bar-setup-function #'lispm-bar-setup-model
  "Function helping the user set up a model, called with no argument.
The bar calls it, once closed, when asked something while no model
works (see `lispm-bar-ready-function'); what was asked waits, and is
asked once `lispm-bar-model-ready' is called.")

(defun lispm-bar-model-ready-p (agent)
  "Non-nil if AGENT has a backend to ask, as far as can be told.
Without asking it, that is: a backend of `lispm-backends' (see
`lispm-backend'), whose key, when it reads one from an environment
variable, is set there."
  (condition-case nil
      (let ((variable (plist-get (lispm-agent-backend* agent) :key-env)))
        (or (null variable) (not (string-empty-p (or (getenv variable) "")))))
    (error nil)))

(defun lispm-bar-setup-model ()
  "Show where LispM is given a model: the option `lispm-backend'."
  (customize-option 'lispm-backend)
  (message "%s" (lispm-tr "LispM has no model: set OPENAI_API_KEY or ANTHROPIC_API_KEY, or choose a backend here")))

(defvar lispm-bar--waiting nil
  "What the user asked while no model worked, to ask once one does.
\(TEXT . REQUEST): what they typed, and what LispM is to be asked.")

(defun lispm-bar-model-ready ()
  "Tell the bar that a model works now: what waited for one is asked.
A setup of models calls it once it is done.  The bar shows the reply
when it opens next."
  (when-let* ((waiting lispm-bar--waiting))
    (setq lispm-bar--waiting nil)
    (lispm-bar-ask (cdr waiting) (car waiting))
    (unless lispm-bar--open
      (message "%s" (lispm-tr "LispM is answering “%s”: %s shows the reply"
                              (lispm--oneline (car waiting) 40)
                              (substitute-command-keys "\\[lispm-ai]"))))))

;;;; Asking

(defvar lispm-bar--asked nil
  "What was last asked from the bar: (TEXT . ENTRY-ID), or nil.
TEXT is what the user typed; ENTRY-ID the id of the input it is in
the system agent's inbox, then of its turn.")

(defvar lispm-bar--reply nil
  "The reply of the turn that answered what was last asked, once it ended.")

(defun lispm-bar-ask (request &optional text)
  "Ask LispM itself REQUEST; the bar shows its reply.
TEXT is what the user typed, if REQUEST says more; the bar shows it."
  (let* ((agent (lispm-system-agent))
         (asked (cons (or text request) nil)))
    (setq lispm-bar--asked asked
          lispm-bar--reply nil)
    (lispm-tell agent request
                :callback (lambda (reply &rest _)
                            (when (eq lispm-bar--asked asked)
                              (setq lispm-bar--reply reply)
                              (lispm-bar--changed))))
    (setcdr asked (plist-get (car (last (lispm-agent-inbox agent))) :id))
    (lispm-bar--changed)
    agent))

(defun lispm-bar--asked-state (agent)
  "How far AGENT is with what was last asked.
Nil if nothing was, else `queued', `working' or `done'."
  (when-let* ((asked lispm-bar--asked))
    (cond ((seq-find (lambda (entry) (equal (plist-get entry :id) (cdr asked)))
                     (lispm-agent-inbox agent))
           'queued)
          ((equal (plist-get (lispm-agent-entry agent) :id) (cdr asked)) 'working)
          (lispm-bar--reply 'done))))

;;;; The bar's state

(defvar lispm-bar--open nil
  "Non-nil while the bar is open: the window it was opened from.")

(defvar lispm-bar--text ""
  "What the field holds.")

(defvar lispm-bar--found nil
  "What the bar found for the field's text, best first.")

(defvar lispm-bar--choice 0
  "Which line Return acts on: 0 asks LispM, N the Nth thing found.")

(defvar lispm-bar--expanded nil
  "Non-nil while the bar is opened out.")

(defvar lispm-bar--ready t
  "Whether LispM had a model to ask when the bar last looked.")

(defvar lispm-bar--then nil
  "A function to run once the bar has closed: opening what was chosen.")

(defvar lispm-bar-history nil
  "What was asked from the bar, for M-p and M-n in its field.")

(defvar lispm-bar--redraw-timer nil
  "Timer redrawing the bar soon, once for a burst of changes.")

(defvar lispm-bar--spinner-timer nil
  "Timer turning the activity line's spinner while LispM works.")

(defvar lispm-bar-map)
(defvar lispm-bar-show-function)

(defun lispm-bar-open-p ()
  "Non-nil while the bar is open."
  (and lispm-bar--open t))

(defun lispm-bar-expanded-p ()
  "Non-nil while the bar is opened out, see `lispm-bar-toggle'."
  lispm-bar--expanded)

(defun lispm-bar-ready-p ()
  "Non-nil if LispM has a model to ask, see `lispm-bar-ready-function'."
  (setq lispm-bar--ready (and (funcall lispm-bar-ready-function (lispm-system-agent)) t)))

(defun lispm-bar--default-choice ()
  "The line Return acts on before one is chosen.
Asking LispM, unless there is no model to ask and something found can
be opened: then the bar is a launcher, and the best of those is chosen."
  (or (and (not lispm-bar--ready)
           (when-let* ((index (seq-position lispm-bar--found t
                                            (lambda (thing _) (plist-get thing :open)))))
             (1+ index)))
      0))

(defun lispm-bar--set-text (text)
  "Note that the field holds TEXT; find what it names."
  (unless (equal text lispm-bar--text)
    (setq lispm-bar--text text
          lispm-bar--found (lispm-bar-find text (lispm-bar--limit))
          lispm-bar--choice (lispm-bar--default-choice))))

(defun lispm-bar--limit ()
  "How many things the bar shows of what it finds, opened out or not."
  (if lispm-bar--expanded (* 3 lispm-bar-results) lispm-bar-results))

(defun lispm-bar--chosen ()
  "The thing Return acts on, or nil when it asks LispM."
  (and (> lispm-bar--choice 0) (nth (1- lispm-bar--choice) lispm-bar--found)))

;;;; Drawing

(defconst lispm-bar-buffer-name "*LispM*"
  "Name of the buffer the bar shows its lines in.")

(defvar lispm-bar-triangle-function #'lispm-bar-triangle
  "Function returning the disclosure triangle opening the bar out.
It is called with non-nil when the bar is open, and returns a string.
A desktop may draw its own.")

(defun lispm-bar-triangle (open)
  "The disclosure triangle, pointing down when OPEN, right when not."
  (if open "▾" "▸"))

(defvar-keymap lispm-bar-line-map
  :doc "Keys of a line of the bar, for the mouse."
  "<mouse-1>" #'lispm-bar-click)

(defvar-keymap lispm-bar-triangle-map
  :doc "Keys of the disclosure triangle, for the mouse."
  "<mouse-1>" #'lispm-bar-toggle)

(defun lispm-bar--width ()
  "How wide the bar's lines are, in columns."
  (if-let* ((window (get-buffer-window lispm-bar-buffer-name t)))
      (window-body-width window)
    72))

(defun lispm-bar--right (text)
  "TEXT, put at the right end of the line."
  (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width text)))))
          text))

(defun lispm-bar--line (index name detail kind &optional icon)
  "Line INDEX of what Return acts on: NAME, a DETAIL and the KIND beside it.
ICON, a string, goes before NAME."
  (let* ((selected (= index lispm-bar--choice))
         (room (- (lispm-bar--width) (string-width kind) (if icon 8 6)))
         (name (truncate-string-to-width name (max 8 room) nil nil "…"))
         (detail (and detail (> (- room (string-width name)) 10)
                      (truncate-string-to-width (lispm--oneline detail)
                                                (- room (string-width name) 3) nil nil "…")))
         (line (concat (if selected "❯ " "  ")
                       (if icon (concat icon " ") "")
                       name
                       (if detail (propertize (concat "  " detail) 'face 'lispm-bar-dim) "")
                       (lispm-bar--right (propertize kind 'face 'lispm-bar-dim))
                       "\n")))
    (when selected
      (add-face-text-property 0 (length line) 'lispm-bar-selected nil line))
    (propertize line 'lispm-bar-line index 'keymap lispm-bar-line-map 'mouse-face 'highlight)))

(defvar lispm-bar-ask-icon nil
  "An icon before the line asking LispM: a string, or a function returning one.
A desktop may give LispM's.")

(defun lispm-bar--ask-line ()
  "The line asking LispM what the field holds, or setting up a model first."
  (lispm-bar--line 0
                   (if lispm-bar--ready
                       (lispm-tr "Ask LispM “%s”" (lispm--oneline lispm-bar--text 60))
                     (lispm-tr "Set up a model…"))
                   (unless lispm-bar--ready
                     (lispm-tr "then LispM is asked “%s”" (lispm--oneline lispm-bar--text 40)))
                   "↩"
                   (if (functionp lispm-bar-ask-icon) (funcall lispm-bar-ask-icon) lispm-bar-ask-icon)))

(defun lispm-bar--found-lines ()
  "The lines of what the field's text found, under the line asking LispM.
Opened out, they come under headings, a kind each."
  (concat
   (lispm-bar--ask-line)
   (let ((index 0) (kind nil) (lines nil))
     (dolist (thing lispm-bar--found)
       (cl-incf index)
       (when (and lispm-bar--expanded (not (equal (lispm-bar--kind-label thing t) kind)))
         (setq kind (lispm-bar--kind-label thing t))
         (push (propertize (concat "\n" kind "\n") 'face 'lispm-bar-heading) lines))
       (push (lispm-bar--line index (plist-get thing :name) (plist-get thing :detail)
                              (if lispm-bar--expanded "" (lispm-bar--kind-label thing))
                              (plist-get thing :icon))
             lines))
     (apply #'concat (nreverse lines)))))

(defun lispm-bar--indent (text prefix)
  "TEXT, its first line after PREFIX, the others indented as far."
  (lispm-chat--indent text (make-string (string-width prefix) ?\s) prefix))

(defun lispm-bar--reply-text (agent)
  "What AGENT has replied to what was last asked, so far, or nil."
  (pcase (lispm-bar--asked-state agent)
    ('working (or (let ((live (seq-filter (lambda (s) (eq (plist-get s :kind) 'text))
                                          (and (buffer-live-p (lispm-agent-buffer agent))
                                               (lispm-live-segments (lispm-agent-buffer agent))))))
                    (and live (mapconcat (lambda (s) (plist-get s :text)) live "\n\n")))
                  (lispm-agent-last-reply agent)))
    ('done lispm-bar--reply)))

(defun lispm-bar--reply-lines (agent)
  "What was last asked and how AGENT replied, or nil when nothing was."
  (when-let* ((asked lispm-bar--asked))
    (let ((reply (lispm-bar--reply-text agent)))
      (concat (propertize (lispm-bar--indent (car asked) "λ ") 'face 'lispm-bar-dim) "\n"
              (when (eq (lispm-bar--asked-state agent) 'queued)
                (propertize (concat "  ⎿  " (lispm-tr "queued; sent when the agent is free") "\n")
                            'face 'lispm-bar-dim))
              (when (and reply (not (string-blank-p reply)))
                (concat (lispm-bar--indent (lispm-chat--markdown (string-trim reply)) "⏺ ") "\n"))))))

(defun lispm-bar--history-lines (agent)
  "The conversation with AGENT so far, as the bar opened out shows it."
  (let* ((buffer (or (and (buffer-live-p (lispm-agent-buffer agent)) (lispm-agent-buffer agent))
                     (ignore-errors (lispm-session-buffer agent))))
         (items (and buffer (lispm-chat-items buffer)))
         (items (last items 40)))
    (concat
     (propertize (concat (lispm-tr "Conversation") "\n") 'face 'lispm-bar-heading)
     (if (null items)
         (propertize (concat (lispm-tr "Nothing asked yet.") "\n") 'face 'lispm-bar-dim)
       (mapconcat
        (lambda (item)
          (pcase (plist-get item :kind)
            ('user (propertize (concat (lispm-bar--indent (plist-get item :text) "λ ") "\n")
                               'face 'lispm-bar-dim))
            ('text (concat (lispm-bar--indent (lispm-chat--markdown (string-trim (plist-get item :text))) "⏺ ")
                           "\n"))
            ('tool (propertize (concat "  ⎿  " (lispm-chat--tool-name (plist-get item :name))
                                       (if-let* ((summary (lispm-transcript-summary (plist-get item :input))))
                                           (format "(%s)" summary)
                                         "")
                                       "\n")
                               'face 'lispm-bar-dim))
            ((or 'note 'summary)
             (propertize (concat (lispm-bar--indent (lispm--oneline (plist-get item :text) 200) "  ") "\n")
                         'face 'lispm-bar-dim))
            (_ "")))
        items "")))))

(defvar lispm-bar-activity-functions nil
  "Functions saying what a call LispM runs does, for the bar's activity line.
Each is called with the tool's name and the call's input, an alist,
and returns a short phrase, as \"Opening Stickies…\", or nil.  The
first phrase given is shown.")

(defun lispm-bar--running-call (agent)
  "The call AGENT runs now, if any: (NAME . INPUT)."
  (let (found)
    (maphash (lambda (_id call)
               (when (and (not found) (eq (plist-get call :status) 'run))
                 (setq found (cons (plist-get call :name) (plist-get call :input)))))
             (lispm-agent-pending agent))
    found))

(defconst lispm-bar--spinner ["·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢"]
  "The frames of the spinner the activity line turns, as the chat's.")

(defun lispm-bar--activity (agent)
  "The line saying what AGENT does, while it works; or nil."
  (when (lispm-agent-working-p agent)
    (let* ((call (lispm-bar--running-call agent))
           (frame (aref lispm-bar--spinner (mod (floor (* 4 (float-time))) (length lispm-bar--spinner))))
           (verb (cond ((eq (lispm-agent-state agent) 'compacting) (lispm-tr "Compacting conversation…"))
                       ((eq (lispm-agent-state agent) 'aborting) (lispm-tr "Stopping…"))
                       ((and call (run-hook-with-args-until-success 'lispm-bar-activity-functions
                                                                    (car call) (cdr call))))
                       (call (lispm-tr "Running %s…"
                                       (concat (lispm-chat--tool-name (car call))
                                               (if-let* ((summary (lispm-transcript-summary (cdr call))))
                                                   (format "(%s)" summary)
                                                 ""))))
                       (t (pcase (lispm-agent-activity agent)
                            (`(writing . ,tool) (lispm-tr "Writing a call of %s…" (lispm-chat--tool-name tool)))
                            (_ (lispm-tr "Thinking…")))))))
      (propertize (concat frame " " verb "\n") 'face 'lispm-bar-activity))))

(defun lispm-bar--question (agent)
  "What AGENT asks, answered with y or n: a call awaiting approval or a
way to go on from where its turn stopped; nil if it asks nothing."
  (cond
   ((when-let* ((condition (lispm-asked-condition agent)))
      (let ((restarts (plist-get condition :restarts)))
        (propertize
         (concat "▌ " (lispm-tr (plist-get condition :title)) "\n"
                 (lispm-bar--indent (lispm--oneline (or (plist-get condition :message) "") 200) "  ") "\n"
                 "  " (lispm-tr "y: %s · n: %s"
                                (lispm-restart-description condition (car restarts))
                                (lispm-restart-description condition (lispm-stop-restart condition)))
                 "\n")
         'face 'lispm-bar-question))))
   ((when-let* ((id (car (lispm-waiting-calls agent)))
                (call (gethash id (lispm-agent-pending agent))))
      (let* ((asked (lispm-asked call))
             (name (plist-get asked :name))
             (input (and (consp (plist-get asked :input)) (plist-get asked :input)))
             (summary (lispm-transcript-summary input))
             (title (or (lispm-chat--tool-text name :title input nil (or (alist-get 'path input) summary))
                        (lispm-tr "Use %s" (lispm-chat--tool-name name))))
             (why (alist-get 'why input)))
        (concat
         (propertize (concat "▌ " title (if summary (concat ": " summary) "") "\n") 'face 'lispm-bar-question)
         (when (stringp why)
           (propertize (concat (lispm-bar--indent why "  ") "\n") 'face 'lispm-bar-dim))
         (propertize (concat "  " (lispm-tr "Do you want to proceed?") " "
                             (lispm-tr "y yes · n no") "\n")
                     'face 'lispm-bar-question)))))))

(defun lispm-bar--key (command)
  "The key of COMMAND in the bar's field, as written."
  (if-let* ((key (where-is-internal command lispm-bar-map t)))
      (key-description key)
    (format "M-x %s" command)))

(defun lispm-bar--hint ()
  "The bar's last line: its disclosure triangle and the keys it takes."
  (concat (propertize (funcall lispm-bar-triangle-function lispm-bar--expanded)
                      'keymap lispm-bar-triangle-map 'mouse-face 'highlight
                      'help-echo (if lispm-bar--expanded (lispm-tr "Show less") (lispm-tr "Show more")))
          " "
          (propertize (let ((toggle (lispm-bar--key 'lispm-bar-toggle))
                            (chat (lispm-bar--key 'lispm-bar-chat)))
                        (if lispm-bar--expanded
                            (lispm-tr "%s less · %s chat · %s close" toggle chat "C-g")
                          (lispm-tr "%s more · %s chat · %s close" toggle chat "C-g")))
                      'face 'lispm-bar-dim)
          "\n"))

(defun lispm-bar--text ()
  "Everything the bar shows, as one string."
  (let* ((agent (lispm-system-agent))
         (finding (not (string-blank-p lispm-bar--text))))
    (concat
     (when finding (lispm-bar--found-lines))
     (when (and finding lispm-bar--expanded) "\n")
     (cond (lispm-bar--expanded (lispm-bar--history-lines agent))
           ((not finding)
            (or (lispm-bar--reply-lines agent)
                (when (and lispm-bar--waiting (not lispm-bar--ready))
                  (propertize (concat (lispm-bar--indent (car lispm-bar--waiting) "λ ") "\n"
                                      "  ⎿  " (lispm-tr "asked once LispM has a model") "\n")
                              'face 'lispm-bar-dim))
                (propertize (concat (if (seq-find (lambda (thing) (eq (plist-get thing :kind) 'application))
                                                  lispm-bar--things)
                                        (lispm-tr "Find applications, skills and tools, or ask LispM.")
                                      (lispm-tr "Find skills and tools, or ask LispM."))
                                    "\n")
                            'face 'lispm-bar-dim))))
     (lispm-bar--question agent)
     (lispm-bar--activity agent)
     (lispm-bar--hint))))

(define-derived-mode lispm-bar-mode special-mode "LispM bar"
  "The lines of the LispM bar: what it found, and LispM's reply.
They are written while the field over or under them is read from."
  :group 'lispm-bar
  (setq-local mode-line-format nil
              header-line-format nil
              tab-line-format nil
              display-line-numbers nil
              cursor-type nil
              cursor-in-non-selected-windows nil
              truncate-lines nil
              word-wrap t
              window-min-height 1)
  (setq buffer-undo-list t))

(defun lispm-bar-buffer ()
  "The buffer the bar shows its lines in, made if need be."
  (or (get-buffer lispm-bar-buffer-name)
      (with-current-buffer (get-buffer-create lispm-bar-buffer-name)
        (lispm-bar-mode)
        ;; A global mode may have turned them on as the mode was set.
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))
        ;; Replies are drawn as the chat draws them: objects they name
        ;; are relative to LispM's root.
        (setq-local lispm-chat-agent (lispm-system-agent))
        (current-buffer))))

(defun lispm-bar-redraw ()
  "Draw the bar's lines again, and show them."
  (setq lispm-bar--redraw-timer nil)
  (when (and lispm-bar--open (not (timerp lispm-bar--spinner-timer))
             (lispm-agent-working-p (lispm-system-agent)))
    (setq lispm-bar--spinner-timer (run-at-time 0.25 0.25 #'lispm-bar--spin)))
  (when lispm-bar--open
    (let ((buffer (lispm-bar-buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (text (lispm-bar--text)))
          (unless (equal-including-properties text (buffer-string))
            (erase-buffer)
            (insert (string-remove-suffix "\n" text))
            ;; While it finds, the bar shows the top, where Return acts;
            ;; else the end, where the reply grows.
            (goto-char (if (string-blank-p lispm-bar--text) (point-max) (point-min)))
            (dolist (window (get-buffer-window-list buffer nil t))
              (set-window-point window (point))))))
      (funcall lispm-bar-show-function buffer))))

(defun lispm-bar--changed (&rest _)
  "Redraw the bar soon, if it is open."
  (when (and lispm-bar--open (not (timerp lispm-bar--redraw-timer)))
    (setq lispm-bar--redraw-timer (run-at-time 0.03 nil #'lispm-bar-redraw))))

(defun lispm-bar--agent-changed (agent &rest _)
  "Redraw the bar soon if AGENT is LispM itself."
  (when (lispm-system-agent-p agent)
    (lispm-bar--changed)))

(defun lispm-bar--spin ()
  "Keep the spinner turning while the bar is open and LispM works."
  (if (and lispm-bar--open (lispm-agent-working-p (lispm-system-agent)))
      (lispm-bar--changed)
    (when (timerp lispm-bar--spinner-timer)
      (cancel-timer lispm-bar--spinner-timer)
      (setq lispm-bar--spinner-timer nil))))

(defun lispm-bar--state-changed (agent)
  "Redraw the bar when LispM itself starts or stops doing something."
  (when (lispm-system-agent-p agent)
    (lispm-bar--changed)))

(defun lispm-bar--approval-message (agent tool)
  "Where the user answers LispM itself about a call of TOOL: in the bar.
None while the bar is open, where the call is asked about; else a
message naming the key opening it.  Other agents are left to others.
This is a `lispm-approval-message-functions' function."
  (when (lispm-system-agent-p agent)
    (if lispm-bar--open
        t
      (lispm-tr "LispM asks to use %s: %s to answer"
                (lispm-chat--tool-name tool) (substitute-command-keys "\\[lispm-ai]")))))

(add-hook 'lispm-state-change-functions #'lispm-bar--state-changed)
(add-hook 'lispm-response-delta-functions #'lispm-bar--agent-changed)
(add-hook 'lispm-approval-message-functions #'lispm-bar--approval-message)
(add-hook 'lispm-language-changed-hook #'lispm-bar--changed)

;;;; Where the bar shows

(defvar lispm-bar-read-function #'funcall
  "Function reading the bar's field, called with a function of no argument
that reads it from the minibuffer; it returns what that returns.  It
may read elsewhere than in the selected frame's minibuffer, as a
desktop's menu bar does, by selecting another minibuffer around it.")

(defvar lispm-bar-show-function #'lispm-bar-show-in-window
  "Function showing the bar's lines, called with their buffer.
It is called when the bar opens and whenever its lines change: it
shows the buffer if it is not shown, and fits it to them.")

(defvar lispm-bar-hide-function #'lispm-bar-hide-window
  "Function hiding the bar's lines once it closes, called with their buffer.")

(defcustom lispm-bar-height 0.3
  "How tall the bar's lines may grow, a fraction of the frame.
Opened out, they may grow twice as tall."
  :type 'number
  :group 'lispm-bar)

(defun lispm-bar-show-in-window (buffer)
  "Show BUFFER in a window over the minibuffer, as tall as its lines.
See `lispm-bar-height'."
  (let ((window (or (get-buffer-window buffer)
                    (display-buffer buffer
                                    `((display-buffer-in-side-window)
                                      (side . bottom) (slot . 0) (window-height . 1)
                                      (preserve-size . (nil . t))
                                      (window-parameters (no-other-window . t)
                                                         (no-delete-other-windows . t)))))))
    (when (window-live-p window)
      (set-window-dedicated-p window t)
      (fit-window-to-buffer window
                            (max 3 (floor (* (frame-height (window-frame window))
                                             lispm-bar-height (if lispm-bar--expanded 2 1))))
                            1)
      (with-selected-window window
        (when (>= (point) (point-max))
          (recenter -1))))
    window))

(defun lispm-bar-hide-window (buffer)
  "Delete the window showing BUFFER, the bar's lines."
  (dolist (window (get-buffer-window-list buffer nil t))
    (ignore-errors (delete-window window))))

;;;; The field

(defcustom lispm-bar-prompt "λ "
  "What the bar's field starts with."
  :type 'string
  :group 'lispm-bar)

(defun lispm-bar--field-changed ()
  "Find what the field's text names, and redraw.
This is a `post-command-hook' function of the field."
  (when (and lispm-bar--open (minibufferp))
    (let ((text (minibuffer-contents-no-properties)))
      (unless (equal text lispm-bar--text)
        (lispm-bar--set-text text)
        (lispm-bar-redraw)))))

(defun lispm-bar--setup-field ()
  "Make the minibuffer the bar's field.
This is a `minibuffer-setup-hook' function."
  (add-hook 'post-command-hook #'lispm-bar--field-changed nil t)
  (lispm-bar-redraw))

(defun lispm-bar--empty-p ()
  "Non-nil if the field holds nothing but blanks."
  (string-blank-p (minibuffer-contents-no-properties)))

(defun lispm-bar-return ()
  "Do what the chosen line says: open the thing chosen, or ask LispM.
Asking LispM with no model to ask sets one up first: what was asked
waits until one works."
  (interactive)
  (let* ((text (string-trim (minibuffer-contents-no-properties)))
         (thing (lispm-bar--chosen))
         (request (if (and thing (plist-get thing :ask))
                      (concat text "\n\n" (plist-get thing :ask))
                    text)))
    (cond ((and thing (plist-get thing :open))
           (setq lispm-bar--then (plist-get thing :open))
           (exit-minibuffer))
          ((string-empty-p text))
          ((not (lispm-bar-ready-p))
           (add-to-history 'lispm-bar-history text)
           (setq lispm-bar--waiting (cons text request)
                 lispm-bar--then lispm-bar-setup-function)
           (exit-minibuffer))
          (t
           (add-to-history 'lispm-bar-history text)
           (lispm-bar-ask request text)
           (delete-minibuffer-contents)
           (lispm-bar--set-text "")
           (lispm-bar-redraw)))))

(defun lispm-bar-next (&optional n)
  "Choose the next line Return acts on, or the Nth after."
  (interactive "p")
  (setq lispm-bar--choice (mod (+ lispm-bar--choice (or n 1)) (1+ (length lispm-bar--found))))
  (lispm-bar-redraw))

(defun lispm-bar-previous (&optional n)
  "Choose the previous line Return acts on, or the Nth before."
  (interactive "p")
  (lispm-bar-next (- (or n 1))))

(defun lispm-bar-toggle ()
  "Open the bar out, or close it in again.
Opened out, it shows the conversation so far, and more of what the
field's words find, under a heading a kind."
  (interactive)
  (setq lispm-bar--expanded (not lispm-bar--expanded)
        lispm-bar--found (lispm-bar-find lispm-bar--text (lispm-bar--limit))
        lispm-bar--choice (min lispm-bar--choice (length lispm-bar--found)))
  (lispm-bar-redraw)
  (when-let* ((field (active-minibuffer-window)))
    (select-window field)))

(defun lispm-bar-click (event)
  "Act on the line of the bar clicked, as Return would: EVENT is the click."
  (interactive "e")
  (when-let* ((position (posn-point (event-start event)))
              (index (get-text-property position 'lispm-bar-line (window-buffer (posn-window (event-start event)))))
              (field (active-minibuffer-window)))
    (setq lispm-bar--choice index)
    (select-window field)
    (lispm-bar-return)))

(defun lispm-bar--answer (yes)
  "Give LispM's question the answer YES, or no; non-nil if it asked one.
Only while the field is empty: else the key is typed."
  (let ((agent (lispm-system-agent)))
    (when (and (lispm-bar--empty-p) (lispm-awaiting agent))
      (if yes (lispm-approve-for agent) (lispm-deny-for agent nil))
      (lispm-bar--changed)
      t)))

(defun lispm-bar-yes ()
  "Approve the call LispM asks about, or go on from its condition.
It goes on the way the condition offers first.  Typed in a field
holding text, y is typed."
  (interactive)
  (unless (lispm-bar--answer t)
    (call-interactively #'self-insert-command)))

(defun lispm-bar-no ()
  "Deny the call LispM asks about, or stop at its condition.
Typed in a field holding text, n is typed."
  (interactive)
  (unless (lispm-bar--answer nil)
    (call-interactively #'self-insert-command)))

(defun lispm-bar-interrupt ()
  "Stop what LispM itself is doing."
  (interactive)
  (let ((agent (lispm-system-agent)))
    (when (lispm-agent-busy-p agent)
      (lispm-abort agent))))

(defun lispm-bar-chat ()
  "Close the bar and show the conversation with LispM in a chat."
  (interactive)
  (setq lispm-bar--then #'lispm-ai-chat)
  (exit-minibuffer))

(defun lispm-bar-close ()
  "Close the bar; LispM goes on with what it does."
  (interactive)
  (abort-recursive-edit))

(defvar-keymap lispm-bar-map
  :doc "Keys of the bar's field."
  :parent minibuffer-local-map
  "RET" #'lispm-bar-return
  "C-n" #'lispm-bar-next
  "<down>" #'lispm-bar-next
  "C-p" #'lispm-bar-previous
  "<up>" #'lispm-bar-previous
  "C-o" #'lispm-bar-toggle
  "C-c C-o" #'lispm-bar-chat
  "C-c C-c" #'lispm-bar-interrupt
  "y" #'lispm-bar-yes
  "n" #'lispm-bar-no
  "<escape>" #'lispm-bar-close)

(defun lispm-bar--read (initial)
  "Read the bar's field, starting with INITIAL, until it closes."
  (minibuffer-with-setup-hook #'lispm-bar--setup-field
    (read-from-minibuffer lispm-bar-prompt initial lispm-bar-map nil 'lispm-bar-history)))

;;;###autoload
(defun lispm-ai ()
  "Open the LispM bar: find what is installed, or ask LispM itself.
Type to find skills, tools and what other packages add, as a
desktop's applications; Return opens the one chosen, or asks LispM,
the system's own agent, which replies by the field.
\\<lispm-bar-map>\\[lispm-bar-toggle] opens the bar out: the conversation and more
of what is found.  \\[lispm-bar-chat] shows the conversation in a chat;
\\[lispm-bar-close] or C-g closes the bar.  Opened while open, it closes.

To open it with a key, bind it, as
  (keymap-global-set \"C-c SPC\" #\\='lispm-ai)"
  (interactive)
  (if lispm-bar--open
      (when-let* ((field (active-minibuffer-window)))
        (with-selected-window field (abort-recursive-edit)))
    (let ((initial (car lispm-bar--waiting)))
      (setq lispm-bar--open (selected-window)
            lispm-bar--then nil
            lispm-bar--text ""
            lispm-bar--found nil
            lispm-bar--choice 0)
      (lispm-bar-ready-p)
      (lispm-bar--gather)
      (when initial (lispm-bar--set-text initial))
      (unwind-protect
          (funcall lispm-bar-read-function (lambda () (lispm-bar--read initial)))
        (setq lispm-bar--open nil)
        (when (timerp lispm-bar--redraw-timer)
          (cancel-timer lispm-bar--redraw-timer)
          (setq lispm-bar--redraw-timer nil))
        (when-let* ((buffer (get-buffer lispm-bar-buffer-name)))
          (funcall lispm-bar-hide-function buffer)))
      (when-let* ((then lispm-bar--then))
        (setq lispm-bar--then nil)
        (funcall then)))))

;; M-x ai, unless another package has the name.
;;;###autoload
(unless (fboundp 'ai) (defalias 'ai #'lispm-ai))

(provide 'lispm-bar)
;;; lispm-bar.el ends here
