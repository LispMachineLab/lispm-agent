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
;; symbol, the agents named.  Code naming what this Emacs lacks, a
;; symbol nothing defines or a file that is not there, is marked
;; instead; see lispm-ground.el.
;;
;; The inspector is a buffer showing one agent whole, as Genera's
;; inspector showed an object: its identity, state, model, policy and
;; ceiling, the calls it has pending, its inbox, whom it waits on and
;; who waits on it, its journal, its tools and triggers, all of them
;; presentations.  Agents have it too, as the tool inspect_agent.

;;; Code:

(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-ground)

(declare-function lispm-show "lispm" (agent))
(declare-function lispm-chat-transcript "lispm-chat" (agent &optional argument))
(declare-function lispm-browser-available-p "lispm-browser" ())
(declare-function lispm-browser-open-url "lispm-browser" (url &optional new-tab))
(declare-function lispm-browser-normalize-url "lispm-browser" (url))
(declare-function lispm-agents-roster-buffer "lispm-agents-roster" (key))
(declare-function lispm-agents-roster-waits "lispm-agents-roster" (buffer))
(declare-function lispm-agents-bridge-mailbox "lispm-agents-bridge" (terminal))
(declare-function lispm-agents-bridge-observe-work "lispm-agents-bridge" (terminal work))
(declare-function lispm-agents-work-input-summary "lispm-agents" (input))

;;;; Presentations

(defface lispm-presentation '((t))
  "Face added to objects in LispM's text one can act on.
By default they look as they would, and light up under the mouse, as
Genera's did."
  :group 'lispm)

(defface lispm-presentation-acceptable
  '((((class color)) :box (:line-width (-1 . -1)))
    (t :underline t))
  "Face highlighting presentations acceptable to an active `lispm-accept'."
  :group 'lispm)

(defvar lispm-accept-types nil
  "Presentation type or list of types currently accepted by `lispm-accept'.
When `t', any presentation object is acceptable.")

(defvar lispm-accept--constraints nil
  "Active completion constraints `(:collection :predicate :require-match)'.")

(defvar lispm-accept--overlays nil
  "Temporary overlays highlighting acceptable presentations during `lispm-accept'.")

(defvar-keymap lispm-presentation-map
  :doc "Keymap of presentations: act on the object with a click or RET."
  "<mouse-1>" #'lispm-act-at-mouse
  "<mouse-2>" #'lispm-act-at-mouse
  "M-<down-mouse-1>" #'ignore
  "M-<mouse-1>" #'lispm-act-secondary-at-mouse
  "s-<down-mouse-1>" #'ignore
  "s-<mouse-1>" #'lispm-act-secondary-at-mouse
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
    (`(file ,file ,line) (if line
                             (lispm-tr "RET: visit %s at line %d" (abbreviate-file-name file) line)
                           (lispm-tr "RET: visit %s" (abbreviate-file-name file))))
    (`(video ,file) (lispm-tr "RET: play %s" (abbreviate-file-name file)))
    (`(buffer ,name) (lispm-tr "RET: show buffer %s" name))
    (`(agent ,name) (lispm-tr "RET: inspect %s" name))
    (`(symbol ,symbol) (lispm-tr "RET: describe %s" symbol))
    (`(tool ,name) (lispm-tr "RET: go to the definition of the tool %s" name))
    (`(call ,_agent ,id) (lispm-tr "RET: show call %s in the transcript" id))
    (`(url ,url) (lispm-tr "RET: visit %s" url))))

(defcustom lispm-play-video-function #'lispm-open-with-system
  "How RET on a video in the chat plays it: a function of the file.
By default the system opens it with the program it has for videos."
  :type 'function
  :group 'lispm)

(defun lispm-open-with-system (file)
  "Open FILE with the program the system has for it, outside Emacs."
  (let ((file (expand-file-name file)))
    (cond ((eq system-type 'darwin) (call-process "open" nil 0 nil file))
          ((executable-find "xdg-open") (call-process "xdg-open" nil 0 nil file))
          (t (browse-url-of-file file)))))

(defun lispm-object-at (&optional pos)
  "The object presented at POS (default: point), or nil."
  (get-text-property (or pos (point)) 'lispm-object))

(defun lispm-object-value (object)
  "The input string OBJECT stands for when accepted by `lispm-accept'."
  (pcase object
    (`(file ,file ,_line) file)
    (`(video ,file) file)
    (`(buffer ,name) name)
    (`(agent ,name) name)
    (`(symbol ,symbol) (symbol-name symbol))
    (`(tool ,name) name)
    (`(call ,_agent ,id) id)
    (`(url ,url) url)))

(defun lispm-object-acceptable-p (object &optional types)
  "Non-nil if OBJECT matches TYPES and active `lispm-accept' constraints.
TYPES is a presentation type symbol (`file', `agent', `symbol', ...),
a list of such symbols, or `t' for any presentation object."
  (let ((types (or types lispm-accept-types)))
    (and (consp object)
         types
         (or (eq types t)
             (if (listp types)
                 (memq (car object) types)
               (eq (car object) types)))
         (or (null lispm-accept--constraints)
             (let ((value (lispm-object-value object))
                   (collection (plist-get lispm-accept--constraints :collection))
                   (predicate (plist-get lispm-accept--constraints :predicate))
                   (require-match (plist-get lispm-accept--constraints :require-match)))
               (and (stringp value)
                    (cond (collection
                           (if require-match
                               (test-completion value collection predicate)
                             (or (null predicate) (funcall predicate value))))
                          (predicate (funcall predicate value))
                          (t t)))))
         t)))

(defun lispm-accept--highlight (&optional types)
  "Highlight presentations matching TYPES across visible windows."
  (lispm-accept--unhighlight)
  (let ((types (or types lispm-accept-types)))
    (when types
      (walk-windows
       (lambda (window)
         (with-current-buffer (window-buffer window)
           (let ((pos (max (point-min) (window-start window)))
                 (limit (min (point-max) (or (window-end window t) (point-max)))))
             (while (< pos limit)
               (let ((obj (get-text-property pos 'lispm-object))
                     (next (min limit
                                (or (next-single-property-change pos 'lispm-object nil limit)
                                    limit))))
                 (when (lispm-object-acceptable-p obj types)
                   (let ((ov (make-overlay pos next)))
                     (overlay-put ov 'face 'lispm-presentation-acceptable)
                     (overlay-put ov 'help-echo (lispm-present--describe obj))
                     (push ov lispm-accept--overlays)))
                 (setq pos next))))))
       'no-minibuf t))))

(defun lispm-accept--unhighlight ()
  "Remove overlays created by `lispm-accept--highlight'."
  (mapc #'delete-overlay lispm-accept--overlays)
  (setq lispm-accept--overlays nil))

(defun lispm-accept-select (object)
  "Accept OBJECT for an active `lispm-accept' prompt and return its value.
When the minibuffer is active, select its window, insert the value and exit."
  (unless (lispm-object-acceptable-p object)
    (user-error "%s" (lispm-tr "Nothing to act on here")))
  (let ((value (lispm-object-value object))
        (mini (active-minibuffer-window)))
    (when mini
      (with-selected-window mini
        (delete-minibuffer-contents)
        (insert value)
        (exit-minibuffer)))
    value))

(defun lispm-accept (type prompt &optional collection predicate require-match default)
  "Read an input of presentation TYPE with PROMPT, as Genera's `accept' did.
While the minibuffer awaits input, presentations matching TYPE in visible
windows are highlighted in `lispm-presentation-acceptable' and clicking
or pressing RET on one supplies its `lispm-object-value' directly."
  (let ((lispm-accept-types type)
        (lispm-accept--constraints (list :collection collection
                                         :predicate predicate
                                         :require-match require-match)))
    (unwind-protect
        (progn
          (lispm-accept--highlight type)
          (if collection
              (completing-read prompt collection predicate require-match nil nil default)
            (read-string prompt nil nil default)))
      (lispm-accept--unhighlight))))

(defun lispm-known-agent-id (name)
  "Session id of the live or retired agent NAME, or nil."
  (save-match-data
    (cond ((when-let* ((live (lispm-get-agent name t)))
             (lispm-agent-id live)))
          ((when-let* ((id (and (stringp name)
                                (alist-get name lispm--retired-agents nil nil #'equal)))
                       ((lispm-session-files id)))
             id))
          ((when (stringp name)
             (let ((re (format "accepted by %s (\\([0-9]\\{8\\}-[0-9]\\{6\\}-[0-9a-f]+\\))"
                               (regexp-quote name))))
               (cl-loop for agent in (lispm-agents)
                        for buf = (lispm-agent-buffer agent)
                        when (buffer-live-p buf)
                        thereis (with-current-buffer buf
                                  (save-excursion
                                    (save-restriction
                                      (widen)
                                      (goto-char (point-min))
                                      (when (re-search-forward re nil t)
                                        (let ((id (match-string-no-properties 1)))
                                          (when (lispm-session-files id)
                                            (setf (alist-get name lispm--retired-agents nil nil #'equal) id)
                                            id)))))))))))))

(defun lispm-known-agent-p (name)
  "Non-nil if NAME designates a live agent or a known retired session."
  (and (or (lispm-get-agent name t) (lispm-known-agent-id name)) t))

(defun lispm-resolve-agent (name)
  "Return the live agent NAME, or resume its retired session."
  (or (lispm-get-agent name t)
      (when-let* ((id (lispm-known-agent-id name))
                  (agent (lispm-resume-session id)))
        (when (and (stringp name) (not (alist-get (intern name) lispm--agents)))
          (let ((sym (intern name)))
            (when-let* ((cell (rassq agent lispm--agents)))
              (setcar cell sym))
            (setf (lispm-agent-name agent) sym)))
        agent)
      (lispm-get-agent name)))

(defun lispm-open-url (url &optional external)
  "Open URL in `lispm-browser' when available, else with `browse-url'.
With EXTERNAL non-nil (Command-click or prefix arg), open URL in the
system's default browser."
  (cond (external
         (browse-url-default-browser url))
        ((and (fboundp 'lispm-browser-available-p)
              (lispm-browser-available-p)
              (fboundp 'lispm-browser-open-url))
         (lispm-browser-open-url
          (if (fboundp 'lispm-browser-normalize-url)
              (lispm-browser-normalize-url url)
            url)))
        (t
         (browse-url url))))

(defun lispm-act (object &optional external)
  "Act on OBJECT as its type says, or supply it to `lispm-accept'.
With EXTERNAL non-nil, open a `url' object in the default browser.
See `lispm-inspect.el' for presentation types."
  (if lispm-accept-types
      (lispm-accept-select object)
    (pcase object
      (`(file ,file ,line)
       (pop-to-buffer (find-file-noselect file))
       (when line
         (goto-char (point-min))
         (forward-line (1- line))))
      (`(video ,file) (funcall lispm-play-video-function file))
      (`(buffer ,name)
       (pop-to-buffer (or (get-buffer name) (user-error "%s" (lispm-tr "No buffer named %s" name)))))
      (`(agent ,name)
       (let ((agent (lispm-resolve-agent name)))
         (if (or (derived-mode-p 'lispm-inspect-mode)
                 (eq agent (bound-and-true-p lispm-chat-agent))
                 (eq agent (bound-and-true-p lispm-session-agent))
                 (not (fboundp 'lispm-show)))
             (lispm-inspect agent)
           (lispm-show agent))))
      (`(symbol ,symbol) (describe-symbol symbol))
      (`(tool ,name) (lispm-inspect--visit-tool name))
      (`(call ,agent ,id) (lispm-inspect--visit-call (lispm-resolve-agent agent) id))
      (`(url ,url) (lispm-open-url url external))
      (_ (user-error "%s" (lispm-tr "Nothing to act on here"))))))

(defvar lispm-act-functions nil
  "Functions acting on what is at point where no object is presented.
Each is called with no argument and returns non-nil if it acted; RET in
the inspector and the chat tries them in turn.  lispm-hyperbole.el adds
Hyperbole's implicit buttons, once Hyperbole is loaded.")

(defun lispm-act-at-point (&optional arg)
  "Act on the object presented at point; with ARG, use secondary action.
Where none is, act as one of `lispm-act-functions' does."
  (interactive "P")
  (if-let* ((object (lispm-object-at)))
      (lispm-act object (or arg current-prefix-arg))
    (or (run-hook-with-args-until-success 'lispm-act-functions)
        (user-error "%s" (lispm-tr "Nothing to act on here")))))

(defun lispm-act-at-mouse (event)
  "Act on the object presented where EVENT, a click, happened."
  (interactive "e")
  (let ((position (event-start event))
        (external (and (seq-intersection '(meta super) (event-modifiers event)) t)))
    (with-current-buffer (window-buffer (posn-window position))
      (lispm-act (or (lispm-object-at (posn-point position))
                     (user-error "%s" (lispm-tr "Nothing to act on here")))
                 external))))

(defun lispm-act-secondary-at-mouse (event)
  "Act on the object where Command-click EVENT happened, externally for URLs."
  (interactive "e")
  (let ((position (event-start event)))
    (with-current-buffer (window-buffer (posn-window position))
      (lispm-act (or (lispm-object-at (posn-point position))
                     (user-error "%s" (lispm-tr "Nothing to act on here")))
                 t))))

(defun lispm-inspect--visit-tool (name)
  "Show where the tool NAME is defined: its file, or its skill's."
  (let* ((tool (or (lispm-get-tool name) (user-error "%s" (lispm-tr "No tool named %s" name))))
         (file (or (lispm-tool-file tool)
                   (when-let* ((skill (get (intern name) 'lispm-skill))
                               (found (seq-find (lambda (s) (equal (plist-get s :name) skill))
                                                (and (fboundp 'lispm-skills) (lispm-skills)))))
                     (plist-get found :file))
                   (user-error "%s" (lispm-tr "Where %s is defined is not known (an MCP server's tool, say)" name)))))
    (pop-to-buffer (find-file-noselect file))
    (goto-char (point-min))
    (re-search-forward (format "\\(?:(lispm-deftool\\|:name\\)[ \t\n]+\"?%s\\_>" (regexp-quote name))
                       nil t)
    (beginning-of-line)))

(defun lispm-inspect--visit-call (agent id)
  "Show AGENT's call ID in its org transcript."
  (let ((buffer (lispm-agent-buffer agent)))
    (unless (buffer-live-p buffer)
      (user-error "%s" (lispm-tr "%s's session is closed" (lispm-agent-name agent))))
    (if (fboundp 'lispm-chat-transcript)
        (lispm-chat-transcript agent)
      (pop-to-buffer buffer))
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (format "^:TOOL_ID: +%s$" (regexp-quote id)) nil t)
      (org-back-to-heading t)
      (ignore-errors (org-fold-show-subtree)))))

(defvar lispm-present--project-cache (make-hash-table :test #'equal)
  "Cache of (TIMESTAMP . BASENAME-HASH) per project root for `lispm-present--file'.")

(defun lispm-present--project-index (root)
  "Return a hash table mapping file basenames to relative paths under ROOT."
  (let* ((dir (file-name-as-directory (expand-file-name root)))
         (cached (gethash dir lispm-present--project-cache))
         (now (float-time)))
    (if (and cached (< (- now (car cached)) 15.0))
        (cdr cached)
      (let ((table (make-hash-table :test #'equal))
            files)
        (when (and (not (file-remote-p dir)) (file-directory-p dir))
          (setq files
                (or (and (locate-dominating-file dir ".git")
                         (with-temp-buffer
                           (let ((default-directory dir))
                             (when (eq 0 (ignore-errors
                                           (call-process "git" nil t nil
                                                         "ls-files" "-z" "--cached"
                                                         "--others" "--exclude-standard")))
                               (split-string (buffer-string) "\0" t)))))
                    (ignore-errors
                      (mapcar (lambda (f) (file-relative-name f dir))
                              (directory-files-recursively
                               dir "\\`[^.]" nil
                               (lambda (d)
                                 (not (member (file-name-nondirectory (directory-file-name d))
                                              '(".git" "node_modules" "target" ".deps" ".claude")))))))))
          (dolist (rel files)
            (let ((base (file-name-nondirectory rel)))
              (puthash base (cons rel (gethash base table)) table))))
        (puthash dir (cons now table) lispm-present--project-cache)
        table))))

(defun lispm-present--project-file (path root)
  "Find a file under ROOT matching relative suffix PATH, or nil."
  (when (and (stringp path)
             (not (file-name-absolute-p path))
             (not (string-prefix-p "~" path))
             (not (string-prefix-p "." path))
             (string-match-p "\\.[[:alnum:]]+\\'" path))
    (let* ((dir (file-name-as-directory (expand-file-name root)))
           (base (file-name-nondirectory path))
           (suffix (concat "/" path))
           (candidates (seq-filter (lambda (rel)
                                     (or (equal rel path)
                                         (string-suffix-p suffix rel)))
                                   (gethash base (lispm-present--project-index dir)))))
      (when candidates
        (let* ((best (car (sort (copy-sequence candidates)
                                (lambda (a b) (< (length a) (length b))))))
               (full (expand-file-name best dir)))
          (and (file-regular-p full) full))))))

(defun lispm-present--file (path root)
  "The file PATH names, relative to ROOT, if it exists; else nil."
  (save-match-data
    (let ((file (expand-file-name path root)))
      (if (file-regular-p file)
          file
        (lispm-present--project-file path root)))))

(defun lispm-present--mark-extra-lines (string start end file)
  "Mark comma-separated line numbers in STRING from START to END for FILE."
  (save-match-data
    (let ((pos start))
      (while (and (< pos end)
                  (string-match ",L?\\([0-9]+\\)" string pos)
                  (<= (match-end 0) end))
        (let ((nbeg (match-beginning 1))
              (nend (match-end 1))
              (nline (string-to-number (match-string 1 string))))
          (lispm-present-in string nbeg nend (list 'file file nline))
          (setq pos nend)))))
  string)

(defun lispm-present--parse-place (spec root)
  "Resolve SPEC, a file path or URL with optional line, relative to ROOT.
Return `(file FILE LINE)' or nil."
  (save-match-data
    (let ((spec (string-trim (substring-no-properties spec) "`+" "`+"))
          path line)
      (cond ((string-match "\\`https?://[^/]+/[^/]+/[^/]+/\\(?:blob\\|blame\\|tree\\)/[^/]+/\\([^?#\n]+\\)\\(?:#L\\([0-9]+\\).*\\)?\\'" spec)
             (setq path (match-string 1 spec)
                   line (and (match-string 2 spec) (string-to-number (match-string 2 spec)))))
            ((string-match "\\`file://\\([^?#\n]+\\)\\(?:#L?\\([0-9]+\\).*\\)?\\'" spec)
             (setq path (match-string 1 spec)
                   line (and (match-string 2 spec) (string-to-number (match-string 2 spec)))))
            ((string-match "\\`\\([^:#\n]+\\)\\(?:\\(?::L?\\|#L\\)\\([0-9]+\\)\\(?:[-–—,:L0-9]*[0-9]\\)?\\)?\\'" spec)
             (setq path (match-string 1 spec)
                   line (and (match-string 2 spec) (string-to-number (match-string 2 spec))))))
      (when-let* ((path)
                  ((or line (string-match-p "[./]" path)))
                  (file (lispm-present--file path root)))
        (list 'file file line)))))

(defconst lispm-present--link-re
  "\\[\\[\\(file\\|buffer\\):\\([^]\n]+?\\)\\(?:::\\([0-9]+\\)\\)?\\]\\(?:\\[[^]\n]*\\]\\)?\\]"
  "Org links to files, with a line or not, and to buffers.")

(defconst lispm-present--md-link-re
  "\\(?:^\\|[^![\n]\\)\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
  "Markdown links [LABEL](TARGET), excluding images ![ALT](TARGET).")

(defconst lispm-present--place-re
  "\\(?:^\\|[^[:alnum:]_./~+-]\\)\\(\\(?:~?/\\)?[[:alnum:]_.@+-]+\\(?:/[[:alnum:]_.@+-]+\\)*\\)\\(?:\\(:L?\\|#L\\)\\([0-9]+\\)\\(?:[-–—,:L0-9]*[0-9]\\)?\\)?"
  "A file name, and a line or line range/list after a colon or #L, or not.")

(defconst lispm-present--agent-re
  "\\_<\\([[:alnum:]_-]+@[[:alnum:]_.-]*[[:alnum:]_]\\(?:<[0-9]+>\\)?\\)"
  "An agent's name, ROLE@PROJECT.")

(defun lispm-present-text (text &optional root)
  "TEXT with the objects it names presented.
Those are org links to files and buffers, Markdown links, file names
\(relative to ROOT, by default `default-directory') of files there are,
with a directory, an extension or a line after a colon, and the names
of live or known retired agents.  Text already presenting something is
left as it is."
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
        (while (string-match lispm-present--md-link-re text start)
          (let* ((beg (match-beginning 1)) (end (match-end 1))
                 (label (match-string 2 text)) (target (match-string 3 text))
                 (object (or (lispm-present--parse-place label root)
                             (lispm-present--parse-place target root)
                             (and (string-match-p "\\`https?://" target)
                                  (list 'url target)))))
            (when object
              (lispm-present-in text beg end object))
            (setq start end))))
      (let ((start 0))
        (while (string-match lispm-present--place-re text start)
          (let* ((beg (match-beginning 1))
                 (suffix-beg (match-beginning 2))
                 (line (and (match-string 3 text) (string-to-number (match-string 3 text))))
                 ;; Not the full stop of a sentence.
                 (path (if line (match-string 1 text) (string-trim-right (match-string 1 text) "[.]+")))
                 (end (if line (match-end 0) (+ beg (length path)))))
            ;; A word is a file name only with a directory, an extension
            ;; or a line.
            (when-let* (((or line (string-match-p "[./]" path)))
                        (file (lispm-present--file path root)))
              (unless (text-property-not-all beg end 'lispm-object nil text)
                (lispm-present-in text beg end (list 'file file line))
                (when suffix-beg
                  (lispm-present--mark-extra-lines text suffix-beg end file))))
            (setq start (max end (1+ beg))))))
      (let ((start 0))
        (while (string-match lispm-present--agent-re text start)
          (let ((beg (match-beginning 1))
                (end (match-end 1))
                (name (match-string 1 text)))
            (when (lispm-known-agent-p name)
              (mark beg end (list 'agent name)))
            (setq start end)))))
    text))

(defun lispm-present-code (code &optional root defined)
  "CODE, a word set as code in a reply, presented if it names an object.
A symbol that is a function, a variable or a face, quoted or not, a call
of a function, or a file (relative to ROOT), with a line after a colon
or not.  While `lispm-ground-code' is on, code naming what this Emacs
lacks is marked as such (see lispm-ground.el); DEFINED are the names
the reply defines."
  (let* ((call (and (string-match "\\`(\\([^][ \t\n()'\"]+\\)" code) (match-string 1 code)))
         (symbol (intern-soft (cond (call)
                                    ((string-match "\\`#?'\\([^']+\\)\\'" code) (match-string 1 code))
                                    (t code))))
         suffix-beg
         (object
          (cond ((and symbol (if call
                                 (fboundp symbol)
                               (or (fboundp symbol) (boundp symbol) (facep symbol))))
                 (list 'symbol symbol))
                ((string-match "\\`\\([^:#\n]+\\)\\(?:\\(:L?\\|#L\\)\\([0-9]+\\)\\(?:[-–—,:L0-9]*[0-9]\\)?\\)?\\'" code)
                 ;; The line first: looking for the file can change the
                 ;; match data, through a file name handler.
                 (let* ((sbeg (match-beginning 2))
                        (line (and (match-string 3 code) (string-to-number (match-string 3 code))))
                        (file (lispm-present--file (match-string 1 code) (or root default-directory))))
                   (when file
                     (setq suffix-beg sbeg)
                     (list 'file file line))))))
         (presented (if object
                        (let ((s (lispm-present code object)))
                          (when (and suffix-beg (eq (car object) 'file))
                            (lispm-present--mark-extra-lines s suffix-beg (length s) (nth 1 object)))
                          s)
                      code))
         ;; A file presented is there to see.
         (finding (and lispm-ground-code (not (eq (car object) 'file))
                       (lispm-ground-check code root defined))))
    (if finding (lispm-ground-mark presented finding) presented)))

;;;; The inspector

(defvar-local lispm-inspect-agent nil
  "The agent the inspector in this buffer shows.")

(defvar-local lispm-inspect-viewspec 'full
  "The viewspec controlling the level of detail in this inspector buffer.
One of `full' (all sections), `compact' (identity, state, calls, inbox,
delegation) or `summary' (identity, state and pending calls only).")

(defun lispm-inspect--line (label value)
  "A line of the inspector: LABEL, then VALUE."
  (concat (propertize (truncate-string-to-width label 12 nil ?\s) 'face 'shadow) " " value))

(defun lispm-inspect--agent-name (agent)
  "AGENT's name, presented."
  (lispm-present (symbol-name (lispm-agent-name agent))
                 (list 'agent (symbol-name (lispm-agent-name agent)))))

(defun lispm-inspect--policy (policy)
  "POLICY, an alist of classes and actions, on one line."
  (mapconcat (pcase-lambda (`(,class . ,action)) (format "%s %s" class action)) policy ", "))

(defun lispm-inspect--entry (entry)
  "An inbox ENTRY, on one line."
  (concat (cond ((plist-get entry :from) (lispm-tr "[from %s] " (plist-get entry :from)))
                ((plist-get entry :event) (format "[%s] " (plist-get entry :event)))
                (t ""))
          (lispm--oneline (or (plist-get entry :text) "") 70)))

(defun lispm-inspect-data (agent)
  "The sources the inspector shares with data readers, for AGENT.
Return a plist with :pending, an alist of call id to call in transcript
order; :entry and :inbox, the input being handled and queued inputs;
:waiting-on and :waited-on-by, agents on either side of a delegation;
and :journal, the turn to snapshot alist.  These are the agent's own
objects: readers must not change them.  No transcript or diary is read."
  (let (pending)
    (maphash (lambda (id call) (push (cons id call) pending))
             (lispm-agent-pending agent))
    (list :pending (sort pending (lambda (a b)
                                  (< (or (ignore-errors (marker-position (plist-get (cdr a) :marker))) 0)
                                     (or (ignore-errors (marker-position (plist-get (cdr b) :marker))) 0))))
          :entry (lispm-agent-entry agent)
          :inbox (lispm-agent-inbox agent)
          :waiting-on (delete-dups (copy-sequence (lispm-agent-waiting-on agent)))
          :waited-on-by (seq-filter (lambda (other) (memq agent (lispm-agent-waiting-on other)))
                                    (lispm-agents))
          :journal (lispm-agent-journal agent))))

(defun lispm-inspect--agent-section (agent summaryp)
  "Return the \"Agent\" inspector section for AGENT; SUMMARYP omits detail lines."
  (let* ((buffer (lispm-agent-buffer agent))
         (file (and (buffer-live-p buffer) (buffer-file-name buffer))))
    (cons (lispm-tr "Agent")
          (delq nil
                (list (lispm-inspect--line (lispm-tr "name") (lispm-inspect--agent-name agent))
                      (lispm-inspect--line (lispm-tr "role")
                                           (format "%s%s" (lispm-agent-role agent)
                                                   (if (lispm-agent-standing agent)
                                                       (concat ", " (lispm-tr "standing"))
                                                     "")))
                      (lispm-inspect--line (lispm-tr "project")
                                           (lispm-present
                                            (abbreviate-file-name (lispm-agent-root agent))
                                            (list 'file (lispm-agent-root agent) nil)))
                      (and (not summaryp)
                           (lispm-inspect--line (lispm-tr "session") (lispm-agent-id agent)))
                      (and (not summaryp) file
                           (lispm-inspect--line (lispm-tr "file")
                                                (lispm-present
                                                 (abbreviate-file-name file)
                                                 (list 'file file nil))))
                      (and (not summaryp) (lispm-agent-description agent)
                           (lispm-inspect--line (lispm-tr "description")
                                                (lispm-agent-description agent))))))))

(defun lispm-inspect--state-section (agent summaryp)
  "Return the \"State\" inspector section for AGENT; SUMMARYP omits detail lines."
  (let ((condition (lispm-agent-condition agent))
        (usage (lispm-agent-usage agent))
        (backend (lispm-backend-name agent)))
    (cons (lispm-tr "State")
          (delq nil
                (list (lispm-inspect--line (lispm-tr "state")
                                           (format "%s%s" (lispm--state-name (lispm-agent-state agent))
                                                   (if (lispm-agent-status agent)
                                                       (concat ": " (lispm-agent-status agent))
                                                     "")))
                      (lispm-inspect--line (lispm-tr "turn")
                                           (lispm-tr-n "%d, %d request" "%d, %d requests"
                                                       (lispm-agent-steps agent)
                                                       (lispm-agent-turn agent)
                                                       (lispm-agent-steps agent)))
                      (and condition
                           (lispm-inspect--line (lispm-tr "condition")
                                                (lispm-tr "%s: %s (restarts: %s)"
                                                          (lispm-tr (plist-get condition :title))
                                                          (lispm--oneline (plist-get condition :message) 70)
                                                          (mapconcat #'symbol-name
                                                                     (plist-get condition :restarts) ", "))))
                      (and (not summaryp)
                           (lispm-inspect--line (lispm-tr "model")
                                                (if (and backend (assq backend lispm-backends))
                                                    (lispm-tr "%s via %s, effort %s"
                                                              (lispm-agent-model* agent)
                                                              backend
                                                              (lispm-agent-effort* agent))
                                                  (lispm-tr "no backend"))))
                      (and (not summaryp)
                           (lispm-inspect--line (lispm-tr "usage")
                                                (lispm-tr "%d in, %d out, %d from cache"
                                                          (nth 0 usage) (nth 1 usage) (nth 2 usage)))))))))

(defun lispm-inspect--policy-and-calls-sections (agent pending fullp)
  "Return the Policy and Pending calls sections for AGENT."
  (list
   (and fullp
        (cons (lispm-tr "Policy")
              (delq nil
                    (list (lispm-inspect--line (lispm-tr "policy") (lispm-inspect--policy (lispm-policy agent)))
                          (and (lispm-agent-ceiling agent)
                               (lispm-inspect--line (lispm-tr "ceiling")
                                                    (lispm-inspect--policy (lispm-agent-ceiling agent))))
                          (and (lispm-agent-allowed agent)
                               (lispm-inspect--line (lispm-tr "allowed")
                                                    (mapconcat (lambda (a)
                                                                 (if (consp a)
                                                                     (lispm-tr "%s (that call)" (car a))
                                                                   a))
                                                               (lispm-agent-allowed agent) ", ")))))))
   (and pending
        (cons (lispm-tr "Pending calls")
              (mapcar (pcase-lambda (`(,id . ,call))
                        (let* ((asked (lispm-asked call))
                               (summary (lispm-transcript-summary (plist-get asked :input))))
                          (concat (lispm-present (plist-get call :name)
                                                 (list 'call (symbol-name (lispm-agent-name agent)) id))
                                  (if summary (concat " " summary) "")
                                  (propertize (format "  %s%s" (plist-get call :status)
                                                      (if (plist-get call :asking)
                                                          (concat ", " (lispm-tr "asks for %s" (plist-get asked :name)))
                                                        ""))
                                              'face 'shadow))))
                      pending)))))

(defun lispm-inspect--activity-sections (agent data summaryp fullp)
  "Return the Inbox, Delegation, Journal and Extensions sections for AGENT."
  (let ((entry (plist-get data :entry))
        (inbox (plist-get data :inbox))
        (waiting-on (plist-get data :waiting-on))
        (waited-by (plist-get data :waited-on-by))
        (journal (plist-get data :journal)))
    (list
     (and (not summaryp) (or entry inbox)
          (cons (lispm-tr "Inbox")
                (append (and entry
                             (list (lispm-inspect--line (lispm-tr "handling") (lispm-inspect--entry entry))))
                        (mapcar (lambda (item)
                                  (lispm-inspect--line (lispm-tr "queued") (lispm-inspect--entry item)))
                                inbox))))
     (and (not summaryp) (or waiting-on waited-by)
          (cons (lispm-tr "Delegation")
                (delq nil
                      (list (and waiting-on
                                 (lispm-inspect--line (lispm-tr "waits on")
                                                      (mapconcat #'lispm-inspect--agent-name waiting-on ", ")))
                            (and waited-by
                                 (lispm-inspect--line (lispm-tr "waited on by")
                                                      (mapconcat #'lispm-inspect--agent-name waited-by ", ")))))))
     (and fullp (seq-some #'cdr journal)
          (cons (lispm-tr "Journal")
                (mapcar (pcase-lambda (`(,turn . ,snapshots))
                          (lispm-inspect--line
                           (lispm-tr "turn %d" turn)
                           (mapconcat (lambda (s)
                                        (let ((file (plist-get (cdr s) :file)))
                                          (if file
                                              (lispm-present (lispm-tools--label file) (list 'file file nil))
                                            (lispm-present (plist-get (cdr s) :name)
                                                           (list 'buffer (plist-get (cdr s) :name))))))
                                      snapshots ", ")))
                        (seq-filter #'cdr journal))))
     (and fullp (or (lispm-agent-loaded-tools agent) (lispm-agent-triggers agent))
          (cons (lispm-tr "Extensions")
                (delq nil
                      (list (and (lispm-agent-loaded-tools agent)
                                 (lispm-inspect--line (lispm-tr "tools loaded")
                                                      (mapconcat (lambda (name)
                                                                   (lispm-present name (list 'tool name)))
                                                                 (lispm-agent-loaded-tools agent) ", ")))
                            (and (lispm-agent-triggers agent)
                                 (lispm-inspect--line (lispm-tr "triggers")
                                                      (mapconcat (lambda (trigger) (plist-get trigger :name))
                                                                 (lispm-agent-triggers agent) ", "))))))))))

(defun lispm-inspect-sections (agent &optional viewspec)
  "What the inspector shows of AGENT: a list of (TITLE . LINES).
VIEWSPEC controls the level of detail (`full', `compact' or `summary';
by default `lispm-inspect-viewspec' or `full').
LINES are strings, whose objects are presented."
  (let* ((viewspec (or viewspec (bound-and-true-p lispm-inspect-viewspec) 'full))
         (summaryp (eq viewspec 'summary))
         (fullp (eq viewspec 'full))
         (data (lispm-inspect-data agent)))
    (delq nil
          (append (list (lispm-inspect--agent-section agent summaryp)
                        (lispm-inspect--state-section agent summaryp))
                  (lispm-inspect--policy-and-calls-sections
                   agent (plist-get data :pending) fullp)
                  (lispm-inspect--activity-sections agent data summaryp fullp)))))

(defun lispm-inspect--copy-data (data)
  "Copy plain DATA, including the strings in its lists and vectors.
Buffers, numbers and symbols keep their identities.  Sources shared
with the runtime can then be returned without exposing mutable text."
  (cond ((stringp data) (substring-no-properties data))
        ((consp data) (cons (lispm-inspect--copy-data (car data))
                           (lispm-inspect--copy-data (cdr data))))
        ((vectorp data) (apply #'vector (mapcar #'lispm-inspect--copy-data data)))
        (t data)))

(defconst lispm-inspect--recent-call-limit 20
  "Maximum number of completed calls a roster inspector returns.")

(defun lispm-inspect--call-data (id call)
  "The public work record of the pending CALL with ID.
The summary is of the input it awaits approval for, when it asks on
another call's behalf, as in `lispm-inspect-sections'."
  (list :id id :name (plist-get call :name)
        :input-summary (lispm-agents-work-input-summary (plist-get (lispm-asked call) :input))
        :status (plist-get call :status)))

(defun lispm-inspect--recent-calls (agent)
  "AGENT's latest completed calls of this turn, in transcript order.
Read only the current turn's transcript, when an inspector asks for
it, and return at most `lispm-inspect--recent-call-limit' records.
The runtime marks the turn's start: later events and an unsent draft
are not new turns.  Nil when that exact boundary is unknown, as for
a turn recovered from a session written without it."
  (when-let* (((> (lispm-agent-turn agent) 0))
              (buffer (lispm-agent-buffer agent))
              ((buffer-live-p buffer))
              (turn-start (buffer-local-value 'lispm--turn-start buffer))
              ((eql (car turn-start) (lispm-agent-turn agent)))
              (marker (cdr turn-start))
              ((and (markerp marker) (eq (marker-buffer marker) buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let (calls)
            (dolist (section (lispm-transcript-sections marker))
              (when (and (eq (plist-get section :kind) 'assistant)
                         (not (plist-get section :ignore)))
                (dolist (part (lispm-transcript-parts section))
                  (when (and (eq (plist-get part :kind) 'tool)
                             (not (gethash (plist-get part :id) (lispm-agent-pending agent))))
                    (push (list :id (plist-get part :id) :name (plist-get part :name)
                                :input-summary (lispm-agents-work-input-summary (plist-get part :input))
                                :status (pcase (plist-get part :state)
                                          ("DONE" 'done) ("FAILED" 'failed) ("DENIED" 'denied)))
                          calls)))))
            (nreverse (seq-take calls lispm-inspect--recent-call-limit))))))))

(defun lispm-inspect--mailbox-entry (entry stored state)
  "The public fields of mailbox ENTRY, supplemented by STORED, in STATE.
STORED is its persisted inbox record, when known.  Runtime callbacks
and transcript positions never form part of the returned record."
  (when entry
    (let ((record (list :state state)))
      (dolist (field '(:id :agent :session :from :to :event :reply-to :request-id :queued :text))
        (setq record (plist-put record field (or (plist-get entry field) (plist-get stored field)))))
      record)))

(defun lispm-inspect--mailbox (agent data)
  "The handling and queued entries of AGENT from its inspector DATA.
Read the project's inbox only here, to fill metadata unknown in
memory.  A running entry's text is held in memory, not in that file."
  (let* ((entry (plist-get data :entry))
         (inbox (plist-get data :inbox))
         (stored (and (or entry inbox)
                      (condition-case nil
                          (lispm-inbox-entries (lispm-agent-root agent))
                        (file-error nil)))))
    (cl-flet ((record (entry state)
                 (lispm-inspect--mailbox-entry
                  entry (seq-find (lambda (other)
                                    (equal (plist-get entry :id) (plist-get other :id)))
                                  stored)
                  state)))
      (list :handling (record entry 'running)
            :queued (mapcar (lambda (entry) (record entry 'queued)) inbox)))))

(defun lispm-agents-roster-details (key)
  "Return the lazy work and mailbox snapshot of the agent named by KEY.
KEY is (KIND SESSION-ID), as `lispm-agents-roster' returns.  Return
nil for an absent or archived session.  :work holds :turn,
:pending-calls, :recent-calls (at most twenty, completed in this turn)
and :files, the files in this turn's in-memory journal.  Call records
have :id, :name, :input-summary and :status.  :mailbox holds :handling
and :queued entries, with their known sender, reply-to, time and text.
:waits-on, :waited-on-by and :request-ids describe delegation edges.
Terminal kinds may supply :work-details in `lispm-agents-define';
unknown work or a provider-owned mailbox is nil.  Unlike the cheap
roster this function may read one agent's current transcript and inbox.
The returned lists and strings belong to the caller."
  (require 'lispm-agents-roster)
  (when-let* ((buffer (lispm-agents-roster-buffer key)))
    (let* ((agent (or (buffer-local-value 'lispm-session-agent buffer)
                      (and (boundp 'lispm-chat-agent)
                           (buffer-local-value 'lispm-chat-agent buffer))))
           (data (and agent (lispm-inspect-data agent)))
           (work (if agent
                     (list :turn (lispm-agent-turn agent)
                           :pending-calls (mapcar (pcase-lambda (`(,id . ,call))
                                                   (lispm-inspect--call-data id call))
                                                 (plist-get data :pending))
                           :recent-calls (lispm-inspect--recent-calls agent)
                           :files (delete-dups
                                   (delq nil (mapcar (lambda (snapshot) (plist-get (cdr snapshot) :file))
                                                      (cdr (assq (lispm-agent-turn agent)
                                                                 (plist-get data :journal)))))))
                   (when-let* ((details (plist-get (lispm-agents-props (car key)) :work-details)))
                     (let ((work (funcall details buffer)))
                       (lispm-agents-bridge-observe-work buffer work)
                       work))))
           (mailbox (if agent (lispm-inspect--mailbox agent data)
                      (lispm-agents-bridge-mailbox buffer))))
      (lispm-inspect--copy-data
       (append (list :key key :work work :mailbox mailbox)
               (lispm-agents-roster-waits buffer))))))

(defun lispm-inspect-text (agent &optional viewspec)
  "What the inspector shows of AGENT, as plain text, in English: what an
agent reads of it.  VIEWSPEC is `full' (default), `compact' or `summary'."
  (lispm-in-english
    (mapconcat (pcase-lambda (`(,title . ,lines))
                 (concat "# " title "\n" (mapconcat #'substring-no-properties lines "\n")))
               (lispm-inspect-sections agent viewspec) "\n\n")))

(defun lispm-inspect--draw ()
  "Draw the inspector in the current buffer again."
  (let ((agent lispm-inspect-agent)
        (inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (if (not (memq agent (lispm-agents)))
        (insert (lispm-tr "%s has ended." (lispm-agent-name agent)) "\n")
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

(defun lispm-inspect--language-changed ()
  "Draw every inspector again in the language now spoken."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'lispm-inspect-agent buffer)
      (with-current-buffer buffer (lispm-inspect--draw)))))

(add-hook 'lispm-language-changed-hook #'lispm-inspect--language-changed)

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
                     (user-error "%s" (lispm-tr "%s's session is closed" (lispm-agent-name lispm-inspect-agent))))))

(defun lispm-inspect-abort ()
  "Stop the inspected agent's turn."
  (interactive)
  (lispm-abort lispm-inspect-agent))

(defun lispm-inspect-diff ()
  "Show what the inspected agent changed in its latest turn with changes."
  (interactive)
  (lispm-turn-diff lispm-inspect-agent))

(defun lispm-inspect-cycle-viewspec ()
  "Cycle the inspector's viewspec (`full' -> `compact' -> `summary' -> `full')."
  (interactive)
  (setq lispm-inspect-viewspec
        (pcase lispm-inspect-viewspec
          ('full 'compact)
          ('compact 'summary)
          (_ 'full)))
  (lispm-inspect--draw))

(defvar-keymap lispm-inspect-mode-map
  :doc "Keymap of `lispm-inspect-mode'."
  :parent special-mode-map
  "RET" #'lispm-act-at-point
  "g" #'lispm-inspect-refresh
  "v" #'lispm-inspect-cycle-viewspec
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
      (if next (goto-char next) (message "%s" (lispm-tr "No more objects"))))))

(defun lispm-inspect-previous ()
  "Move to the previous object presented."
  (interactive)
  (let ((pos (point)))
    (when (and (> pos (point-min)) (lispm-object-at (1- pos)) (lispm-object-at pos))
      (setq pos (or (previous-single-property-change pos 'lispm-object) (point-min))))
    (let ((previous (previous-single-property-change pos 'lispm-object)))
      (when (and previous (not (lispm-object-at previous)))
        (setq previous (previous-single-property-change previous 'lispm-object)))
      (if previous (goto-char previous) (message "%s" (lispm-tr "No more objects"))))))

(define-derived-mode lispm-inspect-mode special-mode "λ Inspect"
  "An agent, shown whole: its state, calls, inbox, delegations and journal.
RET on an object acts on it: a file is visited, an agent inspected, a
call shown in the transcript.

\\{lispm-inspect-mode-map}"
  :group 'lispm
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-inspect--draw))))

(defun lispm-inspect (agent)
  "Show the inspector of AGENT: the agent, whole, with its objects to act on.
Interactively, the agent of the current session or chat, or one read
with `lispm-accept' (click an agent presentation on screen or complete)."
  (interactive (list (or (bound-and-true-p lispm-inspect-agent)
                         lispm-session-agent (bound-and-true-p lispm-chat-agent)
                         (let ((names (mapcar (lambda (a) (symbol-name (lispm-agent-name a)))
                                              (lispm-agents))))
                           (unless names (user-error "%s" (lispm-tr "No agent is running")))
                           (lispm-get-agent (lispm-accept 'agent (lispm-tr "Inspect agent: ") names nil t))))))
  (let ((buffer (get-buffer-create (format "*lispm inspect %s*" (lispm-agent-name agent)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-inspect-mode) (lispm-inspect-mode))
      (setq lispm-inspect-agent agent
            default-directory (lispm-agent-root agent))
      (lispm-inspect--draw))
    (pop-to-buffer buffer)
    buffer))

(lispm-deftool inspect_agent
    ((agent string "The agent to inspect (see list_agents); yourself if not given." :optional t)
     (viewspec string "Level of detail to display: full (default), compact or summary."
               :optional t :enum ("full" "compact" "summary")))
  "Inspect a LispM agent's permissions, approval policy and pending requests.
Read its state, the calls it has pending and
what they wait for, its inbox, whom it waits on and who waits on it,
its policy and ceiling, remembered allowances, and files its turns changed.
Use this to diagnose repeated permission prompts or requests for full access.
This only reports LispM's policy; it does not grant access or change settings,
and does not inspect macOS privacy permissions."
  :permission comm
  :deferred t
  :keywords '("LispM permissions" "approval policy" "full access" "repeated prompts" "allowance")
  (lispm-inspect-text (if agent
                          (or (lispm-get-agent agent t)
                              (error "No agent named %s; list_agents shows them" agent))
                        lispm-current-agent)
                      (and viewspec (intern viewspec))))

(provide 'lispm-inspect)

;; Hyperbole's Action Key on these objects, and its implicit buttons in
;; LispM's text, once Hyperbole is loaded.  LispM does not need it.
(with-eval-after-load 'hyperbole (require 'lispm-hyperbole))
;;; lispm-inspect.el ends here
