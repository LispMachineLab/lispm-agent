;;; lispm-shell-stage.el --- Agents' shell and Git calls, shown live on stage  -*- lexical-binding: t; -*-

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

;; When an agent runs a shell command (`run_shell') or external program
;; (`run_program'), its execution comes onto the stage window: output streams
;; in with ANSI colours beneath a live status header, `FILE:LINE:' diagnostics
;; become clickable presentations, and Git subcommands (`diff', `show',
;; `status', `log', `branch') render as structured diff, working-tree and
;; history views rather than plain terminal text.
;;
;; Fast read-only environment probes (`pwd', `which', `uname') wait briefly
;; before claiming the stage so 10ms probes never flicker over open code;
;; Git commands, builds, tests, writes, slow commands and any failing command
;; appear immediately.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-i18n)
(require 'lispm-tools)
(require 'lispm-edits)
(require 'lispm-spotlight)
(require 'lispm-inspect)
(require 'lispm-process-tools)

(defgroup lispm-shell-stage nil
  "Agents' shell and Git commands, shown live on the stage."
  :group 'lispm
  :prefix "lispm-shell-stage-")

(defcustom lispm-show-shell-stage t
  "Whether agents' shell and Git commands are shown on the stage."
  :type 'boolean
  :group 'lispm-shell-stage)

(defcustom lispm-shell-stage-follow t
  "Whether an agent's shell or Git stage comes into view automatically."
  :type 'boolean
  :group 'lispm-shell-stage)

(defcustom lispm-shell-stage-probe-delay 0.12
  "Seconds to wait before staging a trivial read-only probe command.
Fast probes such as `pwd' or `which' that succeed within this window do
not replace the current stage buffer; Git commands, builds, tests, slow
commands and failures show immediately."
  :type 'number
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-prompt
  '((t :inherit font-lock-keyword-face :weight bold))
  "Command prompt line at the top of a shell stage buffer."
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-ok
  '((((background dark)) :foreground "#a6e3a1" :weight bold)
    (t :foreground "#1e662d" :weight bold))
  "Badge face for commands exiting with status 0."
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-fail
  '((((background dark)) :foreground "#f38ba8" :weight bold)
    (t :foreground "#b42318" :weight bold))
  "Badge face for commands exiting non-zero or timing out."
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-stderr
  '((((background dark)) :foreground "#fab387")
    (t :foreground "#9a3412"))
  "Face applied to `stderr' chunks in `run_program' output."
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-hash
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for Git commit hashes."
  :group 'lispm-shell-stage)

(defface lispm-shell-stage-ref
  '((t :inherit font-lock-type-face :weight bold))
  "Face for Git branch and tag references."
  :group 'lispm-shell-stage)

(defvar lispm-shell-stage--in-batch nil
  "Non-nil to show the shell stage in batch mode too, as tests do.")

(defconst lispm-shell-stage--probe-programs
  '("pwd" "which" "uname" "whoami" "id" "hostname" "true" "printenv")
  "Trivial probe programs debounced by `lispm-shell-stage-probe-delay'.")

(defun lispm-shell-stage--live-p ()
  "Return non-nil if the shell stage is active in this Emacs session."
  (and lispm-show-shell-stage
       (or (not noninteractive) lispm-shell-stage--in-batch)))

;;;; Pure command & output parsers

(cl-defstruct (lispm-shell-stage-spec (:constructor lispm-shell-stage-spec--create)
                                      (:copier nil))
  "Normalized classification of a `run_shell' or `run_program' call."
  tool-name
  display
  program
  git-subcommand
  probe-p)

(defun lispm-shell-stage--git-subcommand (tokens)
  "Extract the Git subcommand from argv TOKENS following `git'."
  (let ((rest tokens)
        sub)
    (while (and rest (null sub))
      (let ((tok (pop rest)))
        (cond
         ((member tok '("-C" "-c" "--git-dir" "--work-tree" "--namespace"))
          (pop rest))
         ((string-prefix-p "-" tok) nil)
         (t (setq sub tok)))))
    sub))

(defun lispm-shell-stage--unwrap-tokens (tokens)
  "Strip leading env assignments and `timeout' wrappers from argv TOKENS."
  (let ((rest tokens))
    (while (and rest (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car rest)))
      (setq rest (cdr rest)))
    (when (and rest (member (file-name-nondirectory (car rest)) '("env" "nice")))
      (setq rest (cdr rest))
      (while (and rest (string-match-p "\\`\\(?:-\\|[A-Za-z_][A-Za-z0-9_]*=\\)" (car rest)))
        (setq rest (cdr rest))))
    (when (and rest (member (file-name-nondirectory (car rest)) '("timeout" "gtimeout")))
      (setq rest (cdr rest))
      (while (and rest (string-prefix-p "-" (car rest)))
        (when (member (car rest) '("-k" "-s" "--kill-after" "--signal"))
          (setq rest (cdr rest)))
        (setq rest (cdr rest)))
      (when (and rest (string-match-p "\\`[0-9]" (car rest)))
        (setq rest (cdr rest))))
    rest))

(defun lispm-shell-stage--classify-tokens (tool-name display tokens complex-p)
  "Classify argv TOKENS with TOOL-NAME, DISPLAY text and COMPLEX-P flag."
  (let* ((core (lispm-shell-stage--unwrap-tokens tokens))
         (prog (and core (file-name-nondirectory (car core))))
         (rest (cdr core))
         (git-sub (cond
                   ((equal prog "git")
                    (lispm-shell-stage--git-subcommand rest))
                   ((and (equal prog "gh")
                         (equal (car rest) "pr")
                         (equal (cadr rest) "diff"))
                    "diff")))
         (probe-p (and (not complex-p)
                       (null git-sub)
                       (member prog lispm-shell-stage--probe-programs)
                       t)))
    (lispm-shell-stage-spec--create
     :tool-name tool-name
     :display (string-trim (replace-regexp-in-string "[ \t\n]+" " " display))
     :program (or prog "")
     :git-subcommand git-sub
     :probe-p probe-p)))

;;; Post: Returns a `lispm-shell-stage-spec' for shell string COMMAND.
(defun lispm-shell-stage--classify-shell (command)
  "Classify shell string COMMAND, unwrapping leading `cd DIR &&' prefixes."
  (save-match-data
    (let ((trimmed (string-trim (or command ""))))
      (when (string-match "\\`cd[ \t]+\\(?:[^;&|\n]+\\)[ \t]*&&[ \t]*" trimmed)
        (setq trimmed (substring trimmed (match-end 0))))
      (let ((complex-p (not (null (string-match-p "[|;&><`\n]" trimmed))))
            (tokens (split-string trimmed "[ \t\n]+" t)))
        (lispm-shell-stage--classify-tokens "run_shell" command tokens complex-p)))))

;;; Post: Returns a `lispm-shell-stage-spec' for PROGRAM and ARGS sequence.
(defun lispm-shell-stage--classify-program (program args)
  "Classify `run_program' call of PROGRAM with ARGS."
  (let* ((argv (cons program (append args nil)))
         (display (mapconcat #'identity argv " ")))
    (lispm-shell-stage--classify-tokens "run_program" display argv nil)))

;;; Post: Returns plist `(:exit INT-OR-NIL :timed-out-p BOOL :body STRING)'.
(defun lispm-shell-stage--parse-shell-result (text errorp)
  "Extract exit status, timeout state, and clean body from `run_shell' TEXT.
ERRORP marks an execution or timeout failure."
  (save-match-data
    (let ((str (or text "")))
      (cond
       ((string-match "\n\\[exit status \\([0-9]+\\)\\(?:; [^\n]*\\)?\\]\\'" str)
        (list :exit (string-to-number (match-string 1 str))
              :timed-out-p nil
              :body (substring str 0 (match-beginning 0))))
       ((string-match "\n\\[killed: timeout\\(?:; [^\n]*\\)?\\]\\'" str)
        (list :exit nil
              :timed-out-p t
              :body (substring str 0 (match-beginning 0))))
       (t
        (list :exit (if errorp 1 0)
              :timed-out-p nil
              :body str))))))

;;; Post: Returns plist `(:exit INT-OR-NIL :timed-out-p BOOL :stdout S :stderr S)'.
(defun lispm-shell-stage--parse-program-result (text errorp)
  "Decode JSON result TEXT of `run_program', falling back on ERRORP."
  (condition-case nil
      (let* ((obj (json-parse-string (or text "{}")
                                     :object-type 'alist
                                     :null-object nil
                                     :false-object nil))
             (status (alist-get 'status obj))
             (exit (alist-get 'exit_code obj)))
        (list :exit (if (integerp exit) exit (if errorp 1 0))
              :timed-out-p (equal status "timed_out")
              :stdout (or (alist-get 'stdout obj) "")
              :stderr (or (alist-get 'stderr obj) "")))
    (error
     (list :exit (if errorp 1 0)
           :timed-out-p nil
           :stdout (or text "")
           :stderr ""))))

;;; Post: `(FILES INSERTIONS DELETIONS)' when TEXT holds unified diff hunks, else nil.
(defun lispm-shell-stage--diff-stats (text)
  "Count changed files, insertions, and deletions in unified diff TEXT."
  (when (and (stringp text)
             (or (string-search "diff --git " text)
                 (string-match-p "^@@ -[0-9]" text)))
    (let ((files 0)
          (adds 0)
          (dels 0)
          (seen-hunk nil))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-prefix-p "diff --git " line)
          (cl-incf files))
         ((string-prefix-p "@@ " line)
          (setq seen-hunk t)
          (when (zerop files) (setq files 1)))
         ((and seen-hunk (string-prefix-p "+" line) (not (string-prefix-p "+++" line)))
          (cl-incf adds))
         ((and seen-hunk (string-prefix-p "-" line) (not (string-prefix-p "---" line)))
          (cl-incf dels))))
      (and (> (+ files adds dels) 0)
           (list files adds dels)))))

;;; Post: `(:branch STRING-OR-NIL :items LIST)' when TEXT is porcelain status, else nil.
(defun lispm-shell-stage--parse-porcelain-status (text)
  "Parse short or porcelain Git status TEXT into branch and item plists."
  (let* ((trimmed (string-trim-right (or text "")))
         (lines (and (not (string-empty-p trimmed)) (split-string trimmed "\n")))
         branch
         items
         (valid t))
    (dolist (line lines)
      (cond
       ((string-prefix-p "## " line)
        (setq branch (substring line 3)))
       ((and (>= (length line) 4)
             (string-match "\\`\\([ MADRCU?!][ MADRCU?!]\\) \\(.+\\)\\'" line))
        (let* ((xy (match-string 1 line))
               (raw-path (match-string 2 line))
               (path (if (string-match " -> \\(.+\\)\\'" raw-path)
                         (match-string 1 raw-path)
                       raw-path))
               (kind (cond
                      ((equal xy "??") 'untracked)
                      ((string-search "U" xy) 'conflict)
                      ((string-search "D" xy) 'deleted)
                      ((and (not (eq (aref xy 0) ?\s))
                            (eq (aref xy 1) ?\s))
                       'staged)
                      (t 'modified))))
          (push (list :xy xy :kind kind :path path) items)))
       (t (setq valid nil))))
    (and valid
         (list :branch branch :items (nreverse items)))))

;;;; Mode & active run state

(defvar-local lispm-shell-stage--header nil
  "Current header-line string of a shell stage buffer.")

(define-derived-mode lispm-shell-stage-mode special-mode "LispM-Shell"
  "Major mode for viewing agents' live shell and Git executions on stage."
  :interactive nil
  (setq-local buffer-read-only t
              truncate-lines nil
              header-line-format '(:eval lispm-shell-stage--header))
  (if (fboundp 'lispm-disable-line-numbers)
      (lispm-disable-line-numbers)
    (setq-local display-line-numbers nil)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))))

(cl-defstruct (lispm-shell-stage--session
               (:constructor lispm-shell-stage--session-create) (:copier nil))
  "Live state of AGENT's running shell or program call on stage."
  agent
  spec
  root
  stage-buffer
  log-buffer
  log-offset
  stripped-header-p
  started-at
  staged-p
  probe-timer)

(defvar lispm-shell-stage--sessions (make-hash-table :test #'eq)
  "Agent -> active `lispm-shell-stage--session'.")

(defvar lispm-shell-stage--ticker nil
  "Timer refreshing elapsed durations in running shell stage headers.")

(defun lispm-shell-stage--buffer (agent)
  "Return or create the shell stage buffer for AGENT."
  (let* ((name (format "*lispm-stage-shell: %s*"
                       (if agent (lispm-agent-name agent) "LispM")))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lispm-shell-stage-mode)
        (lispm-shell-stage-mode)))
    buf))

(defun lispm-shell-stage--format-header (agent spec status-str status-face)
  "Build the header line for AGENT, command SPEC, STATUS-STR and STATUS-FACE."
  (let* ((color (lispm-edits--color agent))
         (label (if agent (symbol-name (lispm-agent-name agent)) "LispM"))
         (cmd (truncate-string-to-width (lispm-shell-stage-spec-display spec) 52 nil nil "…")))
    (concat (propertize " " 'face `(:background ,color))
            " "
            (propertize (concat " λ " label " ")
                        'face `(:background ,color :foreground "#1e1e2e" :weight bold))
            " "
            (propertize cmd 'face 'bold)
            " · "
            (propertize status-str 'face status-face))))

(defun lispm-shell-stage--update-running-header (session now)
  "Refresh SESSION's header line with elapsed time at NOW."
  (let ((buf (lispm-shell-stage--session-stage-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq lispm-shell-stage--header
              (lispm-shell-stage--format-header
               (lispm-shell-stage--session-agent session)
               (lispm-shell-stage--session-spec session)
               (lispm-tr "running %.1fs"
                         (max 0.0 (- now (lispm-shell-stage--session-started-at session))))
               'font-lock-constant-face))
        (force-mode-line-update)))))

(defun lispm-shell-stage--show (session &optional top-p)
  "Bring SESSION's stage buffer into view; keep point at top if TOP-P."
  (let ((agent (lispm-shell-stage--session-agent session))
        (buf (lispm-shell-stage--session-stage-buffer session)))
    (when (and (buffer-live-p buf)
               lispm-shell-stage-follow
               (lispm-spotlight--follow-agent-p agent (float-time))
               (not (lispm-edits-active-p)))
      (setf (lispm-shell-stage--session-staged-p session) t)
      (when-let* ((win (lispm-stage-window buf t agent 'shell)))
        (when (not (eq win (selected-window)))
          (with-current-buffer buf
            (let ((pos (if top-p (point-min) (point-max))))
              (set-window-point win pos)
              (when top-p
                (set-window-start win (point-min))))))))))

;;;; Semantic Git & terminal rendering

(defun lispm-shell-stage--link-files (root)
  "Turn `FILE:LINE:' locations in current buffer into links under ROOT."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (forward-line 1)
      (while (re-search-forward "^[ \t]*\\([^:\n\t ]+\\):\\([0-9]+\\):" nil t)
        (let* ((raw (match-string-no-properties 1))
               (line (string-to-number (match-string-no-properties 2)))
               (full (expand-file-name raw root)))
          (when (and (> line 0) (file-regular-p full))
            (let ((beg (match-beginning 1))
                  (end (match-end 2)))
              (add-text-properties
               beg end
               (list 'lispm-object (list 'file full line)
                     'mouse-face 'highlight
                     'keymap lispm-presentation-map))
              (add-face-text-property beg end 'lispm-presentation t))))))))

(defun lispm-shell-stage--render-diff (spec body root)
  "Render unified diff BODY for SPEC under ROOT in the current stage buffer."
  (pcase-let ((`(,files ,adds ,dels) (or (lispm-shell-stage--diff-stats body) '(0 0 0))))
    (setq-local truncate-lines t)
    (insert (propertize (format "$ %s\n" (lispm-shell-stage-spec-display spec))
                        'face 'lispm-shell-stage-prompt))
    (when (> (+ files adds dels) 0)
      (insert (propertize (lispm-tr "%d files · +%d -%d" files adds dels) 'face 'bold)
              "  "
              (propertize (make-string (min 12 (max (if (> adds 0) 1 0) (/ adds 5))) ?+)
                          'face 'lispm-shell-stage-ok)
              (propertize (make-string (min 12 (max (if (> dels 0) 1 0) (/ dels 5))) ?-)
                          'face 'lispm-shell-stage-fail)
              "\n\n"))
    (let ((current-file nil))
      (dolist (line (split-string (ansi-color-filter-apply body) "\n"))
        (let ((start (point)))
          (insert line "\n")
          (cond
           ((string-prefix-p "diff --git " line)
            (when (string-match " b/\\(.+\\)\\'" line)
              (setq current-file (expand-file-name (match-string 1 line) root)))
            (add-face-text-property start (point) 'diff-header t))
           ((or (string-prefix-p "--- " line) (string-prefix-p "+++ " line))
            (add-face-text-property start (point) 'diff-file-header t))
           ((string-prefix-p "@@ " line)
            (add-face-text-property start (point) 'diff-hunk-header t))
           ((string-prefix-p "+" line)
            (add-face-text-property start (point) 'diff-added t))
           ((string-prefix-p "-" line)
            (add-face-text-property start (point) 'diff-removed t)))
          (when (and current-file (file-regular-p current-file)
                     (string-prefix-p "+++ b/" line))
            (add-text-properties
             (+ start 6) (1- (point))
             (list 'lispm-object (list 'file current-file nil)
                   'mouse-face 'highlight
                   'keymap lispm-presentation-map))))))))

(defun lispm-shell-stage--render-status (spec parsed root)
  "Render porcelain Git status PARSED for SPEC under ROOT."
  (setq-local truncate-lines t)
  (insert (propertize (format "$ %s\n" (lispm-shell-stage-spec-display spec))
                      'face 'lispm-shell-stage-prompt))
  (when-let* ((branch (plist-get parsed :branch)))
    (insert (propertize (concat "⎇ " branch) 'face 'lispm-shell-stage-ref) "\n"))
  (insert "\n")
  (let ((items (plist-get parsed :items)))
    (if (null items)
        (insert (propertize (concat "✓ " (lispm-tr "Working tree clean"))
                            'face 'lispm-shell-stage-ok)
                "\n")
      (dolist (item items)
        (let* ((xy (plist-get item :xy))
               (kind (plist-get item :kind))
               (path (plist-get item :path))
               (full (expand-file-name path root))
               (face (pcase kind
                       ('staged 'lispm-shell-stage-ok)
                       ((or 'deleted 'conflict) 'lispm-shell-stage-fail)
                       ('modified 'lispm-shell-stage-stderr)
                       (_ 'shadow)))
               (shown (if (file-exists-p full)
                          (lispm-present path (list 'file full nil))
                        path)))
          (insert "  " (propertize xy 'face face) "  " shown "\n"))))))

(defun lispm-shell-stage--render-git-log (spec body)
  "Render Git log or branch output BODY for SPEC with commit/ref highlights."
  (setq-local truncate-lines t)
  (insert (propertize (format "$ %s\n\n" (lispm-shell-stage-spec-display spec))
                      'face 'lispm-shell-stage-prompt))
  (let ((start (point)))
    (insert (ansi-color-apply body))
    (save-excursion
      (save-match-data
        (goto-char start)
        (while (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" nil t)
          (add-face-text-property (match-beginning 1) (match-end 1)
                                  'lispm-shell-stage-hash))
        (goto-char start)
        (while (re-search-forward "(\\([^)\n]*\\(?:HEAD\\|origin/\\|tag:\\)[^)\n]*\\))" nil t)
          (add-face-text-property (match-beginning 0) (match-end 0)
                                  'lispm-shell-stage-ref))))))

(defun lispm-shell-stage--render-terminal (spec stdout stderr root)
  "Render ANSI-coloured STDOUT and STDERR for SPEC under ROOT."
  (setq-local truncate-lines nil)
  (insert (propertize (format "$ %s\n" (lispm-shell-stage-spec-display spec))
                      'face 'lispm-shell-stage-prompt))
  (unless (string-empty-p stdout)
    (let ((beg (point)))
      (insert stdout)
      (ansi-color-apply-on-region beg (point))
      (unless (bolp) (insert "\n"))))
  (unless (string-empty-p stderr)
    (let ((beg (point)))
      (insert stderr)
      (ansi-color-apply-on-region beg (point))
      (add-face-text-property beg (point) 'lispm-shell-stage-stderr t)
      (unless (bolp) (insert "\n"))))
  (lispm-shell-stage--link-files root))

;;;; Streaming listeners & lifecycle

(defun lispm-shell-stage--append-chunk (session chunk &optional stderr-p)
  "Append live CHUNK (stderr if STDERR-P) to SESSION's stage buffer."
  (let ((buf (lispm-shell-stage--session-stage-buffer session)))
    (when (and (buffer-live-p buf) (not (string-empty-p chunk)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (beg (point-max)))
          (save-excursion
            (goto-char beg)
            (insert chunk)
            (ansi-color-apply-on-region beg (point-max))
            (when stderr-p
              (add-face-text-property beg (point-max) 'lispm-shell-stage-stderr t)))))
      (when (lispm-shell-stage--session-staged-p session)
        (when-let* ((win (get-buffer-window buf 'visible)))
          (unless (eq win (selected-window))
            (set-window-point win (with-current-buffer buf (point-max)))))))))

(defun lispm-shell-stage--sync-shell-log (session)
  "Copy any newly appended text from SESSION's `*lispm-shell*' log buffer."
  (let ((log (lispm-shell-stage--session-log-buffer session)))
    (when (buffer-live-p log)
      (let* ((from (lispm-shell-stage--session-log-offset session))
             (to (with-current-buffer log (point-max))))
        (when (> to from)
          (let ((chunk (with-current-buffer log
                         (buffer-substring-no-properties from to))))
            (setf (lispm-shell-stage--session-log-offset session) to)
            (unless (lispm-shell-stage--session-stripped-header-p session)
              (setf (lispm-shell-stage--session-stripped-header-p session) t)
              (when (string-match "\\`\n\\$ [^\n]*\n" chunk)
                (setq chunk (substring chunk (match-end 0)))))
            (lispm-shell-stage--append-chunk session chunk)))))))

(defun lispm-shell-stage--on-log-change (_beg _end _len)
  "Mirror live insertions in a `*lispm-shell*' buffer to its stage session."
  (let ((log (current-buffer)))
    (maphash (lambda (_agent session)
               (when (eq (lispm-shell-stage--session-log-buffer session) log)
                 (lispm-shell-stage--sync-shell-log session)))
             lispm-shell-stage--sessions)))

(defun lispm-shell-stage--on-program-output (agent errorp text)
  "Mirror live `run_program' TEXT (stderr when ERRORP) for AGENT."
  (when-let* ((session (gethash agent lispm-shell-stage--sessions)))
    (lispm-shell-stage--append-chunk session text errorp)))

(defun lispm-shell-stage--tick ()
  "Update running elapsed headers across active shell stage sessions."
  (if (zerop (hash-table-count lispm-shell-stage--sessions))
      (when (timerp lispm-shell-stage--ticker)
        (cancel-timer lispm-shell-stage--ticker)
        (setq lispm-shell-stage--ticker nil))
    (let ((now (float-time)))
      (maphash (lambda (_agent session)
                 (lispm-shell-stage--update-running-header session now))
               lispm-shell-stage--sessions))))

(defun lispm-shell-stage--ensure-ticker ()
  "Ensure the elapsed-time header ticker runs while sessions are active."
  (when (and (> (hash-table-count lispm-shell-stage--sessions) 0)
             (not (timerp lispm-shell-stage--ticker)))
    (setq lispm-shell-stage--ticker
          (run-at-time 0.2 0.2 #'lispm-shell-stage--tick))))

(defun lispm-shell-stage--retire-session (agent)
  "Detach timers and hooks for AGENT's active shell stage session."
  (when-let* ((session (gethash agent lispm-shell-stage--sessions)))
    (remhash agent lispm-shell-stage--sessions)
    (when-let* ((timer (lispm-shell-stage--session-probe-timer session)))
      (cancel-timer timer))
    (let ((log (lispm-shell-stage--session-log-buffer session)))
      (when (buffer-live-p log)
        (with-current-buffer log
          (remove-hook 'after-change-functions #'lispm-shell-stage--on-log-change t))))
    (lispm-shell-stage--tick)
    session))

(defun lispm-shell-stage--start (agent spec)
  "Begin staging AGENT's execution of `lispm-shell-stage-spec' SPEC."
  (lispm-shell-stage--retire-session agent)
  (let* ((now (float-time))
         (root (if agent (lispm-agent-root agent) default-directory))
         (buf (lispm-shell-stage--buffer agent))
         (log (and (equal (lispm-shell-stage-spec-tool-name spec) "run_shell")
                   (get-buffer-create
                    (format "*lispm-shell: %s*" (if agent (lispm-agent-name agent) "?")))))
         (session (lispm-shell-stage--session-create
                   :agent agent
                   :spec spec
                   :root root
                   :stage-buffer buf
                   :log-buffer log
                   :log-offset (if log (with-current-buffer log (point-max)) 0)
                   :started-at now)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local default-directory root
                    truncate-lines nil)
        (insert (propertize (format "$ %s\n" (lispm-shell-stage-spec-display spec))
                            'face 'lispm-shell-stage-prompt))))
    (when log
      (with-current-buffer log
        (add-hook 'after-change-functions #'lispm-shell-stage--on-log-change nil t)))
    (puthash agent session lispm-shell-stage--sessions)
    (lispm-shell-stage--update-running-header session now)
    (lispm-shell-stage--ensure-ticker)
    (if (not (lispm-shell-stage-spec-probe-p spec))
        (lispm-shell-stage--show session t)
      (setf (lispm-shell-stage--session-probe-timer session)
            (run-at-time lispm-shell-stage-probe-delay nil
                         (lambda ()
                           (when (eq (gethash agent lispm-shell-stage--sessions) session)
                             (lispm-shell-stage--show session t))))))))

(defun lispm-shell-stage--finish (agent spec result errorp)
  "Finalize AGENT's shell stage for SPEC with RESULT and ERRORP."
  (let* ((session (or (lispm-shell-stage--retire-session agent)
                      (lispm-shell-stage--session-create
                       :agent agent
                       :spec spec
                       :root (if agent (lispm-agent-root agent) default-directory)
                       :stage-buffer (lispm-shell-stage--buffer agent)
                       :started-at (float-time))))
         (elapsed (max 0.0 (- (float-time) (lispm-shell-stage--session-started-at session))))
         (root (lispm-shell-stage--session-root session))
         (buf (lispm-shell-stage--session-stage-buffer session))
         (parsed (if (equal (lispm-shell-stage-spec-tool-name spec) "run_program")
                     (lispm-shell-stage--parse-program-result result errorp)
                   (let ((sh (lispm-shell-stage--parse-shell-result result errorp)))
                     (list :exit (plist-get sh :exit)
                           :timed-out-p (plist-get sh :timed-out-p)
                           :stdout (plist-get sh :body)
                           :stderr ""))))
         (exit (plist-get parsed :exit))
         (timed-out (plist-get parsed :timed-out-p))
         (failed (or timed-out errorp (and (integerp exit) (/= exit 0))))
         (stdout (plist-get parsed :stdout))
         (stderr (plist-get parsed :stderr))
         (git-sub (lispm-shell-stage-spec-git-subcommand spec))
         (top-p nil))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local default-directory root)
          (cond
           ((or (member git-sub '("diff" "show"))
                (and git-sub (lispm-shell-stage--diff-stats stdout)))
            (setq top-p t)
            (lispm-shell-stage--render-diff spec stdout root))
           ((and (equal git-sub "status")
                 (string-empty-p stderr)
                 (let ((st (lispm-shell-stage--parse-porcelain-status stdout)))
                   (when st
                     (setq top-p t)
                     (lispm-shell-stage--render-status spec st root)
                     t))))
           ((member git-sub '("log" "reflog" "branch"))
            (setq top-p t)
            (lispm-shell-stage--render-git-log spec stdout))
           (t
            (lispm-shell-stage--render-terminal spec stdout stderr root)))
          (setq lispm-shell-stage--header
                (lispm-shell-stage--format-header
                 agent spec
                 (format "%s · %.1fs"
                         (cond
                          (timed-out (concat "✗ " (lispm-tr "timed out")))
                          (failed (concat "✗ " (lispm-tr "exit %d" (or exit 1))))
                          (t (concat "✓ " (lispm-tr "exit %d" (or exit 0)))))
                         elapsed)
                 (if failed 'lispm-shell-stage-fail 'lispm-shell-stage-ok)))
          (force-mode-line-update)))
      ;; Show on stage unless it was a fast probe that succeeded silently.
      (when (or failed
                (lispm-shell-stage--session-staged-p session)
                (not (lispm-shell-stage-spec-probe-p spec)))
        (lispm-shell-stage--show session top-p)))))

(defun lispm-shell-stage--spec-for-call (tool input)
  "Return a stage spec when TOOL on INPUT is a shell or program call."
  (pcase (and tool (lispm-tool-name tool))
    ("run_shell"
     (when-let* ((cmd (alist-get 'command input)))
       (lispm-shell-stage--classify-shell cmd)))
    ("run_program"
     (when-let* ((prog (alist-get 'program input)))
       (lispm-shell-stage--classify-program prog (alist-get 'args input))))))

(defun lispm-shell-stage--on-tool-call (agent phase tool input result errorp)
  "Update AGENT's shell stage for TOOL on INPUT with RESULT and ERRORP at PHASE."
  (when (lispm-shell-stage--live-p)
    (when-let* ((spec (lispm-shell-stage--spec-for-call tool input)))
      (pcase phase
        ('start (lispm-shell-stage--start agent spec))
        ('finish (lispm-shell-stage--finish agent spec result errorp))))))

(defun lispm-shell-stage--on-session-end (agent)
  "Clean up any running shell stage state for AGENT."
  (lispm-shell-stage--retire-session agent))

(add-hook 'lispm-tool-call-functions #'lispm-shell-stage--on-tool-call)
(add-hook 'lispm-process-output-functions #'lispm-shell-stage--on-program-output)
(add-hook 'lispm-session-end-functions #'lispm-shell-stage--on-session-end)

(provide 'lispm-shell-stage)
;;; lispm-shell-stage.el ends here
