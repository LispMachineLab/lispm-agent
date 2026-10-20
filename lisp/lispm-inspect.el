;;; lispm-inspect.el --- Presentations and the agent inspector  -*- lexical-binding: t; -*-

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

;; On a Lisp Machine what the screen showed was the objects themselves:
;; Genera's presentations made a pathname, an instance or a symbol on
;; the screen the object it stood for, with commands by its type.  Text
;; LispM shows does the same.  A presentation is a stretch of text whose
;; `lispm-object' property is the object it shows, one of
;;
;;   (file FILE LINE)   a file, and a line in it or nil
;;   (buffer NAME)      a buffer
;;   (agent NAME)       an agent
;;   (symbol SYMBOL)    a function, variable or face
;;   (tool NAME)        a tool
;;   (call AGENT ID)    a tool call, in the session of AGENT
;;
;; and RET or a click acts on it as its type says: visit the file at the
;; line, show the buffer, inspect the agent, describe the symbol, go to
;; the tool's definition, show the call in the transcript.  The chat
;; presents what it draws: the arguments of calls, the file:line places
;; and org links in replies and results, the code in them that names a
;; symbol, the agents named.
;;
;; The inspector is a buffer showing one agent whole, as Genera's
;; inspector showed an object: its identity, state, model, policy and
;; ceiling, the calls it has pending, its inbox, whom it waits on and
;; who waits on it, its journal, its tools and triggers, all of them
;; presentations.  Agents have it too, as the tool inspect_agent.

;;; Code:

(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-runtime)

(declare-function lispm-show "lispm" (agent))
(declare-function lispm-chat-transcript "lispm-chat" (agent &optional argument))

;;;; Presentations

(defface lispm-presentation '((t))
  "Face added to objects in LispM's text one can act on.
By default they look as they would, and light up under the mouse, as
Genera's did."
  :group 'lispm)

(defvar-keymap lispm-presentation-map
  :doc "Keymap of presentations: act on the object with a click or RET."
  "<mouse-2>" #'lispm-act-at-mouse
  "<follow-link>" 'mouse-face)

(defun lispm-present (string object)
  "STRING presenting OBJECT, as lispm-inspect.el describes them."
  (lispm-present-in (copy-sequence string) 0 (length string) object))

(defun lispm-present-in (string start end object)
  "Make the text of STRING from START to END present OBJECT; return STRING."
  (add-text-properties start end
                       (list 'lispm-object object
                             'mouse-face 'highlight
                             'keymap lispm-presentation-map
                             'help-echo (lispm-present--describe object))
                       string)
  (add-face-text-property start end 'lispm-presentation t string)
  string)

(defun lispm-present--describe (object)
  "What acting on OBJECT does, said for a tooltip."
  (pcase object
    (`(file ,file ,line) (format "RET: visit %s%s" (abbreviate-file-name file)
                                 (if line (format " at line %d" line) "")))
    (`(buffer ,name) (format "RET: show buffer %s" name))
    (`(agent ,name) (format "RET: inspect %s" name))
    (`(symbol ,symbol) (format "RET: describe %s" symbol))
    (`(tool ,name) (format "RET: go to the definition of the tool %s" name))
    (`(call ,_agent ,id) (format "RET: show call %s in the transcript" id))))

(defun lispm-object-at (&optional pos)
  "The object presented at POS (default: point), or nil."
  (get-text-property (or pos (point)) 'lispm-object))

(defun lispm-act (object)
  "Act on OBJECT as its type says; see lispm-inspect.el."
  (pcase object
    (`(file ,file ,line)
     (pop-to-buffer (find-file-noselect file))
     (when line
       (goto-char (point-min))
       (forward-line (1- line))))
    (`(buffer ,name)
     (pop-to-buffer (or (get-buffer name) (user-error "No buffer named %s" name))))
    (`(agent ,name) (lispm-inspect (lispm-get-agent name)))
    (`(symbol ,symbol) (describe-symbol symbol))
    (`(tool ,name) (lispm-inspect--visit-tool name))
    (`(call ,agent ,id) (lispm-inspect--visit-call (lispm-get-agent agent) id))
    (_ (user-error "Nothing to act on here"))))

(defun lispm-act-at-point ()
  "Act on the object presented at point."
  (interactive)
  (lispm-act (or (lispm-object-at) (user-error "Nothing to act on here"))))

(defun lispm-act-at-mouse (event)
  "Act on the object presented where EVENT, a click, happened."
  (interactive "e")
  (let ((position (event-start event)))
    (with-current-buffer (window-buffer (posn-window position))
      (lispm-act (or (lispm-object-at (posn-point position))
                     (user-error "Nothing to act on here"))))))

(defun lispm-inspect--visit-tool (name)
  "Show where the tool NAME is defined: its file, or its skill's."
  (let* ((tool (or (lispm-get-tool name) (user-error "No tool named %s" name)))
         (file (or (lispm-tool-file tool)
                   (when-let* ((skill (get (intern name) 'lispm-skill))
                               (found (seq-find (lambda (s) (equal (plist-get s :name) skill))
                                                (and (fboundp 'lispm-skills) (lispm-skills)))))
                     (plist-get found :file))
                   (user-error "Where %s is defined is not known (an MCP server's tool, say)" name))))
    (pop-to-buffer (find-file-noselect file))
    (goto-char (point-min))
    (re-search-forward (format "\\(?:(lispm-deftool\\|:name\\)[ \t\n]+\"?%s\\_>" (regexp-quote name))
                       nil t)
    (beginning-of-line)))

(defun lispm-inspect--visit-call (agent id)
  "Show AGENT's call ID in its org transcript."
  (let ((buffer (lispm-agent-buffer agent)))
    (unless (buffer-live-p buffer)
      (user-error "%s's session is closed" (lispm-agent-name agent)))
    (if (fboundp 'lispm-chat-transcript)
        (lispm-chat-transcript agent)
      (pop-to-buffer buffer))
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (format "^:TOOL_ID: +%s$" (regexp-quote id)) nil t)
      (org-back-to-heading t)
      (ignore-errors (org-fold-show-subtree)))))

(defun lispm-present--file (path root)
  "The file PATH names, relative to ROOT, if it exists; else nil."
  (let ((file (expand-file-name path root)))
    (and (file-regular-p file) file)))

(defconst lispm-present--link-re
  "\\[\\[\\(file\\|buffer\\):\\([^]\n]+?\\)\\(?:::\\([0-9]+\\)\\)?\\]\\(?:\\[[^]\n]*\\]\\)?\\]"
  "Org links to files, with a line or not, and to buffers.")

(defconst lispm-present--place-re
  "\\(?:^\\|[ \t(\"'`]\\)\\(\\(?:~?/\\)?[[:alnum:]_.@+-]+\\(?:/[[:alnum:]_.@+-]+\\)*\\)\\(?::\\([0-9]+\\)\\)?"
  "A file name, and a line after a colon, or not.")

(defconst lispm-present--agent-re
  "\\_<\\([[:alnum:]_-]+@[[:alnum:]_.-]*[[:alnum:]_]\\(?:<[0-9]+>\\)?\\)"
  "An agent's name, ROLE@PROJECT.")

(defun lispm-present-text (text &optional root)
  "TEXT with the objects it names presented.
Those are org links to files and buffers, file names (relative to
ROOT, by default `default-directory') of files there are, with a
directory, an extension or a line after a colon, and the names of
live agents.  Text already
presenting something is left as it is."
  (let ((text (copy-sequence text))
        (root (or root default-directory))
        (case-fold-search nil))
    (cl-flet ((mark (start end object)
                (unless (text-property-not-all start end 'lispm-object nil text)
                  (lispm-present-in text start end object))))
      (let ((start 0))
        (while (string-match lispm-present--link-re text start)
          (let ((beg (match-beginning 0)) (end (match-end 0))
                (kind (match-string 1 text)) (target (match-string 2 text))
                (line (and (match-string 3 text) (string-to-number (match-string 3 text)))))
            (if (equal kind "buffer")
                (mark beg end (list 'buffer target))
              (when-let* ((file (lispm-present--file target root)))
                (mark beg end (list 'file file line))))
            (setq start end))))
      (let ((start 0))
        (while (string-match lispm-present--place-re text start)
          (let* ((beg (match-beginning 1))
                 (line (and (match-string 2 text) (string-to-number (match-string 2 text))))
                 ;; Not the full stop of a sentence.
                 (path (if line (match-string 1 text) (string-trim-right (match-string 1 text) "[.]+")))
                 (end (if line (match-end 0) (+ beg (length path)))))
            ;; A word is a file name only with a directory, an extension
            ;; or a line.
            (when-let* (((or line (string-match-p "[./]" path)))
                        (file (lispm-present--file path root)))
              (mark beg end (list 'file file line)))
            (setq start (max end (1+ beg))))))
      (let ((start 0))
        (while (string-match lispm-present--agent-re text start)
          (let ((beg (match-beginning 1)) (end (match-end 1)))
            (when (lispm-get-agent (match-string 1 text) t)
              (mark beg end (list 'agent (match-string 1 text))))
            (setq start end)))))
    text))

(defun lispm-present-code (code &optional root)
  "CODE, a word set as code in a reply, presented if it names an object.
A symbol that is a function, a variable or a face, or a file (relative
to ROOT), with a line after a colon or not."
  (let ((symbol (intern-soft code)))
    (cond ((and symbol (or (fboundp symbol) (boundp symbol) (facep symbol)))
           (lispm-present code (list 'symbol symbol)))
          ((string-match "\\`\\([^:\n]+\\)\\(?::\\([0-9]+\\)\\)?\\'" code)
           (let ((file (lispm-present--file (match-string 1 code) (or root default-directory)))
                 (line (and (match-string 2 code) (string-to-number (match-string 2 code)))))
             (if file (lispm-present code (list 'file file line)) code)))
          (t code))))

;;;; The inspector

(defvar-local lispm-inspect-agent nil
  "The agent the inspector in this buffer shows.")

(defun lispm-inspect--line (label value)
  "A line of the inspector: LABEL, then VALUE."
  (concat (propertize (format "%-12s" label) 'face 'shadow) " " value))

(defun lispm-inspect--agent-name (agent)
  "AGENT's name, presented."
  (lispm-present (symbol-name (lispm-agent-name agent))
                 (list 'agent (symbol-name (lispm-agent-name agent)))))

(defun lispm-inspect--policy (policy)
  "POLICY, an alist of classes and actions, on one line."
  (mapconcat (pcase-lambda (`(,class . ,action)) (format "%s %s" class action)) policy ", "))

(defun lispm-inspect--entry (entry)
  "An inbox ENTRY, on one line."
  (concat (cond ((plist-get entry :from) (format "[from %s] " (plist-get entry :from)))
                ((plist-get entry :event) (format "[%s] " (plist-get entry :event)))
                (t ""))
          (lispm--oneline (or (plist-get entry :text) "") 70)))

(defun lispm-inspect-sections (agent)
  "What the inspector shows of AGENT: a list of (TITLE . LINES).
LINES are strings, whose objects are presented."
  (let* ((buffer (lispm-agent-buffer agent))
         (file (and (buffer-live-p buffer) (buffer-file-name buffer)))
         (condition (lispm-agent-condition agent))
         (pending (let (calls)
                    (maphash (lambda (id call) (push (cons id call) calls))
                             (lispm-agent-pending agent))
                    (sort calls (lambda (a b)
                                  (< (or (ignore-errors (marker-position (plist-get (cdr a) :marker))) 0)
                                     (or (ignore-errors (marker-position (plist-get (cdr b) :marker))) 0))))))
         (waited-by (seq-filter (lambda (other) (memq agent (lispm-agent-waiting-on other)))
                                (lispm-agents)))
         (usage (lispm-agent-usage agent)))
    (delq nil
          (list
           (cons "Agent"
                 (delq nil
                       (list (lispm-inspect--line "name" (lispm-inspect--agent-name agent))
                             (lispm-inspect--line "role" (format "%s%s" (lispm-agent-role agent)
                                                                 (if (lispm-agent-standing agent)
                                                                     ", standing" "")))
                             (lispm-inspect--line "project" (lispm-present
                                                             (abbreviate-file-name (lispm-agent-root agent))
                                                             (list 'file (lispm-agent-root agent) nil)))
                             (lispm-inspect--line "session" (lispm-agent-id agent))
                             (and file (lispm-inspect--line "file" (lispm-present
                                                                    (abbreviate-file-name file)
                                                                    (list 'file file nil))))
                             (and (lispm-agent-description agent)
                                  (lispm-inspect--line "description" (lispm-agent-description agent))))))
           (cons "State"
                 (delq nil
                       (list (lispm-inspect--line "state" (format "%s%s" (lispm-agent-state agent)
                                                                  (if (lispm-agent-status agent)
                                                                      (concat ": " (lispm-agent-status agent))
                                                                    "")))
                             (lispm-inspect--line "turn" (format "%d, %d request%s" (lispm-agent-turn agent)
                                                                 (lispm-agent-steps agent)
                                                                 (if (= (lispm-agent-steps agent) 1) "" "s")))
                             (and condition
                                  (lispm-inspect--line "condition"
                                                       (format "%s: %s (restarts: %s)"
                                                               (plist-get condition :title)
                                                               (lispm--oneline (plist-get condition :message) 70)
                                                               (mapconcat #'symbol-name
                                                                          (plist-get condition :restarts) ", "))))
                             (lispm-inspect--line "model" (or (ignore-errors
                                                                (format "%s via %s, effort %s"
                                                                        (lispm-agent-model* agent)
                                                                        (lispm-backend-name agent)
                                                                        (lispm-agent-effort* agent)))
                                                              "no backend"))
                             (lispm-inspect--line "usage" (format "%d in, %d out, %d from cache"
                                                                  (nth 0 usage) (nth 1 usage) (nth 2 usage))))))
           (cons "Policy"
                 (delq nil
                       (list (lispm-inspect--line "policy" (lispm-inspect--policy (lispm-policy agent)))
                             (and (lispm-agent-ceiling agent)
                                  (lispm-inspect--line "ceiling" (lispm-inspect--policy (lispm-agent-ceiling agent))))
                             (and (lispm-agent-allowed agent)
                                  (lispm-inspect--line "allowed"
                                                       (mapconcat (lambda (a) (if (consp a)
                                                                                  (format "%s (that call)" (car a))
                                                                                a))
                                                                  (lispm-agent-allowed agent) ", "))))))
           (and pending
                (cons "Pending calls"
                      (mapcar (pcase-lambda (`(,id . ,call))
                                (let* ((asked (lispm-asked call))
                                       (summary (lispm-transcript-summary (plist-get asked :input))))
                                  (concat (lispm-present (plist-get call :name)
                                                         (list 'call (symbol-name (lispm-agent-name agent)) id))
                                          (if summary (concat " " summary) "")
                                          (propertize (format "  %s%s" (plist-get call :status)
                                                              (if (plist-get call :asking)
                                                                  (format ", asks for %s" (plist-get asked :name))
                                                                ""))
                                                      'face 'shadow))))
                              pending)))
           (and (or (lispm-agent-entry agent) (lispm-agent-inbox agent))
                (cons "Inbox"
                      (append (and (lispm-agent-entry agent)
                                   (list (lispm-inspect--line "handling"
                                                              (lispm-inspect--entry (lispm-agent-entry agent)))))
                              (mapcar (lambda (entry) (lispm-inspect--line "queued" (lispm-inspect--entry entry)))
                                      (lispm-agent-inbox agent)))))
           (and (or (lispm-agent-waiting-on agent) waited-by)
                (cons "Delegation"
                      (delq nil
                            (list (and (lispm-agent-waiting-on agent)
                                       (lispm-inspect--line "waits on"
                                                            (mapconcat #'lispm-inspect--agent-name
                                                                       (delete-dups (copy-sequence
                                                                                     (lispm-agent-waiting-on agent)))
                                                                       ", ")))
                                  (and waited-by
                                       (lispm-inspect--line "waited on by"
                                                            (mapconcat #'lispm-inspect--agent-name waited-by ", ")))))))
           (and (seq-some #'cdr (lispm-agent-journal agent))
                (cons "Journal"
                      (mapcar (pcase-lambda (`(,turn . ,snapshots))
                                (lispm-inspect--line
                                 (format "turn %d" turn)
                                 (mapconcat (lambda (s)
                                              (let ((file (plist-get (cdr s) :file)))
                                                (if file
                                                    (lispm-present (lispm-tools--label file) (list 'file file nil))
                                                  (lispm-present (plist-get (cdr s) :name)
                                                                 (list 'buffer (plist-get (cdr s) :name))))))
                                            snapshots ", ")))
                              (seq-filter #'cdr (lispm-agent-journal agent)))))
           (and (or (lispm-agent-loaded-tools agent) (lispm-agent-triggers agent))
                (cons "Extensions"
                      (delq nil
                            (list (and (lispm-agent-loaded-tools agent)
                                       (lispm-inspect--line "tools loaded"
                                                            (mapconcat (lambda (name)
                                                                         (lispm-present name (list 'tool name)))
                                                                       (lispm-agent-loaded-tools agent) ", ")))
                                  (and (lispm-agent-triggers agent)
                                       (lispm-inspect--line "triggers"
                                                            (mapconcat (lambda (trigger) (plist-get trigger :name))
                                                                       (lispm-agent-triggers agent) ", ")))))))))))

(defun lispm-inspect-text (agent)
  "What the inspector shows of AGENT, as plain text."
  (mapconcat (pcase-lambda (`(,title . ,lines))
               (concat "# " title "\n" (mapconcat #'substring-no-properties lines "\n")))
             (lispm-inspect-sections agent) "\n\n"))

(defun lispm-inspect--draw ()
  "Draw the inspector in the current buffer again."
  (let ((agent lispm-inspect-agent)
        (inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (if (not (memq agent (lispm-agents)))
        (insert (format "%s has ended.\n" (lispm-agent-name agent)))
      (insert (mapconcat (pcase-lambda (`(,title . ,lines))
                           (concat (propertize title 'face 'bold) "\n"
                                   (mapconcat (lambda (l) (concat "  " l)) lines "\n")
                                   "\n"))
                         (lispm-inspect-sections agent) "\n")))
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar lispm-inspect--timers nil
  "Inspector buffers to draw again shortly, with their timers.")

(defun lispm-inspect--changed (agent)
  "Draw again, shortly, the inspectors showing AGENT."
  (dolist (buffer (buffer-list))
    (when (and (eq (buffer-local-value 'lispm-inspect-agent buffer) agent)
               (get-buffer-window buffer t)
               (not (assq buffer lispm-inspect--timers)))
      (push (cons buffer (run-at-time 0.2 nil
                                      (lambda ()
                                        (setq lispm-inspect--timers (assq-delete-all buffer lispm-inspect--timers))
                                        (when (buffer-live-p buffer)
                                          (with-current-buffer buffer (lispm-inspect--draw))))))
            lispm-inspect--timers))))

(add-hook 'lispm-state-change-functions #'lispm-inspect--changed)

(defun lispm-inspect-refresh ()
  "Draw the inspector again."
  (interactive)
  (lispm-inspect--draw))

(defun lispm-inspect-chat ()
  "Show the inspected agent's conversation."
  (interactive)
  (lispm-show lispm-inspect-agent))

(defun lispm-inspect-transcript ()
  "Show the inspected agent's org transcript."
  (interactive)
  (pop-to-buffer (or (and (buffer-live-p (lispm-agent-buffer lispm-inspect-agent))
                          (lispm-agent-buffer lispm-inspect-agent))
                     (user-error "%s's session is closed" (lispm-agent-name lispm-inspect-agent)))))

(defun lispm-inspect-abort ()
  "Stop the inspected agent's turn."
  (interactive)
  (lispm-abort lispm-inspect-agent))

(defun lispm-inspect-diff ()
  "Show what the inspected agent changed in its latest turn with changes."
  (interactive)
  (lispm-turn-diff lispm-inspect-agent))

(defvar-keymap lispm-inspect-mode-map
  :doc "Keymap of `lispm-inspect-mode'."
  :parent special-mode-map
  "RET" #'lispm-act-at-point
  "g" #'lispm-inspect-refresh
  "c" #'lispm-inspect-chat
  "t" #'lispm-inspect-transcript
  "d" #'lispm-inspect-diff
  "k" #'lispm-inspect-abort
  "TAB" #'lispm-inspect-next
  "<backtab>" #'lispm-inspect-previous)

(defun lispm-inspect-next ()
  "Move to the next object presented."
  (interactive)
  (let ((pos (point)))
    (when (lispm-object-at pos)
      (setq pos (or (next-single-property-change pos 'lispm-object) (point-max))))
    (let ((next (if (lispm-object-at pos) pos (next-single-property-change pos 'lispm-object))))
      (if next (goto-char next) (message "No more objects")))))

(defun lispm-inspect-previous ()
  "Move to the previous object presented."
  (interactive)
  (let ((pos (point)))
    (when (and (> pos (point-min)) (lispm-object-at (1- pos)) (lispm-object-at pos))
      (setq pos (or (previous-single-property-change pos 'lispm-object) (point-min))))
    (let ((previous (previous-single-property-change pos 'lispm-object)))
      (when (and previous (not (lispm-object-at previous)))
        (setq previous (previous-single-property-change previous 'lispm-object)))
      (if previous (goto-char previous) (message "No more objects")))))

(define-derived-mode lispm-inspect-mode special-mode "λ Inspect"
  "An agent, shown whole: its state, calls, inbox, delegations and journal.
RET on an object acts on it: a file is visited, an agent inspected, a
call shown in the transcript.

\\{lispm-inspect-mode-map}"
  :group 'lispm
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-inspect--draw))))

(defun lispm-inspect (agent)
  "Show the inspector of AGENT: the agent, whole, with its objects to act on.
Interactively, the agent of the current session or chat, or one read."
  (interactive (list (or (bound-and-true-p lispm-inspect-agent)
                         lispm-session-agent (bound-and-true-p lispm-chat-agent)
                         (let ((names (mapcar (lambda (a) (symbol-name (lispm-agent-name a)))
                                              (lispm-agents))))
                           (unless names (user-error "No agent is running"))
                           (lispm-get-agent (completing-read "Inspect agent: " names nil t))))))
  (let ((buffer (get-buffer-create (format "*lispm inspect %s*" (lispm-agent-name agent)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-inspect-mode) (lispm-inspect-mode))
      (setq lispm-inspect-agent agent
            default-directory (lispm-agent-root agent))
      (lispm-inspect--draw))
    (pop-to-buffer buffer)
    buffer))

(lispm-deftool inspect_agent
    ((agent string "The agent to inspect (see list_agents); yourself if not given." :optional t))
  "Inspect an agent in depth: its state, the calls it has pending and
what they wait for, its inbox, whom it waits on and who waits on it,
its policy and ceiling, and the files its turns changed."
  :permission comm
  (lispm-inspect-text (if agent
                          (or (lispm-get-agent agent t)
                              (error "No agent named %s; list_agents shows them" agent))
                        lispm-current-agent)))

(provide 'lispm-inspect)
;;; lispm-inspect.el ends here
