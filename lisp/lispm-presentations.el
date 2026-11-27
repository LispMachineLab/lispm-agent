;;; lispm-presentations.el --- Typed text presentations and interactive actions  -*- lexical-binding: t; -*-

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

;; Clickable text presentations (`file', `video', `buffer', `agent',
;; `symbol', `tool', `call', `url') and `lispm-accept' input
;; acceptance across LispM buffers.

;;; Code:

(require 'lispm-exec)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-ground)

(declare-function lispm-browser-available-p "lispm-browser" ())
(declare-function lispm-browser-open-url "lispm-browser" (url &optional new-tab))
(declare-function lispm-browser-normalize-url "lispm-browser" (url))

(defvar lispm-inspect-mode)
(defvar lispm-presentations-inspect-function nil
  "Function called with AGENT to open the inspector buffer.")

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
          ((when-let* ((id (lispm-retired-agent-id name))
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
                                            (lispm-remember-retired-agent name id))))))))))))))

(defun lispm-known-agent-p (name)
  "Non-nil if NAME designates a live agent or a known retired session."
  (and (or (lispm-get-agent name t) (lispm-known-agent-id name)) t))

(defun lispm-resolve-agent (name)
  "Return the live agent NAME, or resume its retired session."
  (or (lispm-get-agent name t)
      (when-let* ((id (lispm-known-agent-id name))
                  (agent (lispm-resume-session id)))
        (when (stringp name)
          (lispm-rename-agent agent name))
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
                 (not lispm-show-agent-function))
             (when lispm-presentations-inspect-function
               (funcall lispm-presentations-inspect-function agent))
           (funcall lispm-show-agent-function agent))))
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
    (if lispm-open-transcript-function
        (funcall lispm-open-transcript-function agent)
      (pop-to-buffer buffer))
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (format "^:TOOL_ID: +%s$" (regexp-quote id)) nil t)
      (org-back-to-heading t)
      (ignore-errors (org-fold-show-subtree)))))

(defvar lispm-present--project-cache (make-hash-table :test #'equal)
  "Cache of (TIMESTAMP . BASENAME-HASH) per project root for `lispm-present-file'.")

(defvar lispm-present--project-refreshing (make-hash-table :test #'equal)
  "Directory roots currently refreshing their basename index asynchronously.")

(defun lispm-present--build-project-table (files)
  "Build a basename lookup table from relative FILES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (rel files)
      (let ((base (file-name-nondirectory rel)))
        (puthash base (cons rel (gethash base table)) table)))
    table))

(defun lispm-present--refresh-project-index-async (dir &optional callback)
  "Refresh project basename cache for git root DIR asynchronously.
Calls CALLBACK with the resulting hash table when done."
  (let ((root (file-name-as-directory (expand-file-name dir))))
    (cond
     ((or (file-remote-p root)
          (not (file-directory-p root))
          (not (locate-dominating-file root ".git")))
      (when callback (funcall callback (make-hash-table :test #'equal))))
     ((gethash root lispm-present--project-refreshing)
      (when callback
        (push callback (gethash root lispm-present--project-refreshing))))
     (t
      (puthash root (list (or callback #'ignore)) lispm-present--project-refreshing)
      (let ((default-directory root))
        (lispm-exec "git" '("ls-files" "-z" "--cached" "--others" "--exclude-standard")
                    (lambda (exit output _err)
                      (let* ((callbacks (nreverse (gethash root lispm-present--project-refreshing)))
                             (files (and (eql exit 0) (split-string output "\0" t)))
                             (table (lispm-present--build-project-table files)))
                        (remhash root lispm-present--project-refreshing)
                        (when files
                          (puthash root (cons (float-time) table) lispm-present--project-cache))
                        (dolist (cb callbacks)
                          (unless (eq cb #'ignore)
                            (funcall cb table)))))))))))

(defun lispm-present--project-index (root)
  "Return a hash table mapping file basenames to relative paths under ROOT."
  (let* ((dir (file-name-as-directory (expand-file-name root)))
         (cached (gethash dir lispm-present--project-cache))
         (now (float-time)))
    (cond
     ((and cached (< (- now (car cached)) 15.0))
      (cdr cached))
     ((and (not noninteractive)
           (not (file-remote-p dir))
           (file-directory-p dir)
           (locate-dominating-file dir ".git"))
      (lispm-present--refresh-project-index-async dir)
      (or (cdr cached) (make-hash-table :test #'equal)))
     (t
      (let (files)
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
                                              '(".git" "node_modules" "target" ".deps" ".claude"))))))))))
        (let ((table (lispm-present--build-project-table files)))
          (puthash dir (cons now table) lispm-present--project-cache)
          table))))))

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

(defun lispm-present-file (path root)
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
                  (file (lispm-present-file path root)))
        (list 'file file line)))))

(defconst lispm-present-link-re
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
        (while (string-match lispm-present-link-re text start)
          (let ((beg (match-beginning 0)) (end (match-end 0))
                (kind (match-string 1 text)) (target (match-string 2 text))
                (line (and (match-string 3 text) (string-to-number (match-string 3 text)))))
            (if (equal kind "buffer")
                (mark beg end (list 'buffer target))
              (when-let* ((file (lispm-present-file target root)))
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
              (mark beg end object))
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
                        (file (lispm-present-file path root)))
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
                        (file (lispm-present-file (match-string 1 code) (or root default-directory))))
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


(provide 'lispm-presentations)

;;; lispm-presentations.el ends here
