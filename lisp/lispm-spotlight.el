;;; lispm-spotlight.el --- Agents' reads and lookups, lit via lispm-ui-spotlight  -*- lexical-binding: t; -*-

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

;; Connects LispM read/lookup tool calls (`read_file', `read_buffer',
;; `search_files', `xref_definitions', `xref_references', `eval_definition',
;; `flymake_diagnostics') to the generic `lispm-ui-spotlight' FRP store and
;; `lispm-stage' multi-pane reconciler:
;;
;; - Resolves tool inputs and outputs into `lispm-spotlight-target' structs.
;; - Feeds actor beams into `lispm-ui-spotlight-store' (`lispm-ui-frp'
;;   signals + single overlay reconciliation effect).
;; - Arbitrates stage viewport scrolling among concurrent subagents and
;;   reaps unseen ephemeral buffers once their beams fade or stage closes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-i18n)
(require 'lispm-tools)
(require 'lispm-ui-frp)
(require 'lispm-ui-spotlight)
(require 'lispm-stage)
(require 'lispm-edits)

(declare-function lispm-agent-busy-p "lispm-core" (agent))
(declare-function lispm-agent-parent "lispm-core" (agent))

(defgroup lispm-spotlight nil
  "Agents' reads and lookups, lit in the buffers they inspect."
  :group 'lispm
  :prefix "lispm-spotlight-")

(defcustom lispm-show-spotlights t
  "Whether places agents read or look up are lit in their buffers."
  :type 'boolean
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-follow t
  "Whether a buffer an agent reads comes into view in the stage window."
  :type 'boolean
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-hold 1.4
  "Seconds a read spotlight holds full brightness once an agent finishes."
  :type 'number
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-fade 0.9
  "Seconds over which a retiring read spotlight fades into the background."
  :type 'number
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-tint 0.14
  "Strength from 0.0 to 1.0 of an agent's colour mixed into spotlighted lines."
  :type 'number
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-whole-threshold 40
  "Line count above which reading a whole buffer tints only its opening lines."
  :type 'integer
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-whole-header-lines 3
  "Opening lines tinted when an agent reads an entire long buffer."
  :type 'integer
  :group 'lispm-spotlight)

(defcustom lispm-spotlight-scroll-lock-seconds 1.5
  "Seconds a subagent holds stage scrolling before a sibling may scroll it."
  :type 'number
  :group 'lispm-spotlight)

(defvar lispm-spotlight--in-batch nil
  "Non-nil to show spotlights in batch mode too, as the tests do.")

(defvar-local lispm-spotlight--ephemeral nil
  "Non-nil in a buffer visited solely to show a read spotlight.")

(defun lispm-spotlight--live-p ()
  "Return non-nil if read spotlights are active in this Emacs session."
  (and lispm-show-spotlights (or (not noninteractive) lispm-spotlight--in-batch)))

;;;; Domain structures & pure aliases (delegated to `lispm-ui-spotlight')

(cl-defstruct (lispm-spotlight-target (:constructor lispm-spotlight-target--create)
                                      (:copier nil))
  "Resolved buffer and line intervals illuminated by a read tool call."
  buffer
  ephemeral-p
  anchor-line
  line-ranges
  note)

(defalias 'lispm-spotlight--beam-alpha #'lispm-ui-spotlight-beam-alpha)
(defalias 'lispm-spotlight--coalesce-lines #'lispm-ui-spotlight-coalesce-lines)
(defalias 'lispm-spotlight--compose-tags #'lispm-ui-spotlight-compose-tags)
(defalias 'lispm-spotlight--compose-segments #'lispm-ui-spotlight-compose-segments)
(defalias 'lispm-spotlight--weighted-color #'lispm-ui-spotlight-weighted-color)

(defun lispm-spotlight--parse-file-hits (text root)
  "Extract `(FILE . LINES)' from `FILE:LINE:...' lines in TEXT under ROOT."
  (when (and (stringp text) (not (string-empty-p text)))
    (save-match-data
      (let ((pos 0)
            primary
            lines)
        (while (string-match "^\\([^:\n]+\\):\\([0-9]+\\):" text pos)
          (setq pos (match-end 0))
          (let* ((raw (match-string 1 text))
                 (line (string-to-number (match-string 2 text)))
                 (file (expand-file-name raw root)))
            (when (and (> line 0) (file-regular-p file))
              (unless primary
                (setq primary file))
              (when (equal file primary)
                (push line lines)))))
        (and primary lines
             (cons primary (delete-dups (sort lines #'<))))))))

;;;; Resolving tool calls to spotlight targets

(defun lispm-spotlight--keep-buffer ()
  "Mark the current buffer as user-touched so spotlight cleanup preserves it."
  (setq-local lispm-spotlight--ephemeral nil
              lispm-ui-stage-managed nil))

(defvar lispm-spotlight--ephemeral-buffers nil
  "Buffers opened solely to show a spotlight and eligible for cleanup.")

(defun lispm-spotlight--visit-file (file)
  "Return `(BUFFER . EPHEMERAL-P)' for FILE, or nil if unreadable."
  (if-let* ((existing (find-buffer-visiting file)))
      (cons existing nil)
    (when (file-readable-p file)
      (condition-case nil
          (let* ((enable-local-variables :safe)
                 (inhibit-message t)
                 (buffer (lispm-tools--find-file file)))
            (with-current-buffer buffer
              (setq-local lispm-spotlight--ephemeral t
                          lispm-ui-stage-managed t)
              (add-hook 'first-change-hook #'lispm-spotlight--keep-buffer nil t))
            (cl-pushnew buffer lispm-spotlight--ephemeral-buffers :test #'eq)
            (cons buffer t))
        (error nil)))))

(defun lispm-spotlight--buffer-line-count (buffer)
  "Return the total 1-based line count of BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (max 1 (line-number-at-pos (max (point-min) (1- (point-max)))))))))

(defun lispm-spotlight--query-lines (buffer start-line tail limit query)
  "Return matching 1-based lines for QUERY in BUFFER.
Searches from START-LINE or the last TAIL lines, returning at most LIMIT."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (if tail
              (progn
                (goto-char (point-max))
                (when (and (bolp) (not (bobp)))
                  (backward-char))
                (forward-line (- 1 tail)))
            (goto-char (point-min))
            (forward-line (1- (max 1 (or start-line 1)))))
          (let ((line (line-number-at-pos))
                (case-fold-search t)
                (count 0)
                hits)
            (while (and (not (eobp)) (< count limit))
              (when (save-excursion (search-forward query (line-end-position) t))
                (push line hits)
                (cl-incf count))
              (forward-line 1)
              (cl-incf line))
            (nreverse hits)))))))

(defun lispm-spotlight--definition-end-line (buffer start-line)
  "Return the inclusive end line of the form at START-LINE in BUFFER, capped."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- start-line))
        (skip-chars-forward " \t")
        (let ((end (condition-case nil
                       (progn
                         (forward-sexp 1)
                         (line-number-at-pos (max (point-min) (1- (point)))))
                     (scan-error start-line)
                     (error start-line))))
          (max start-line (min end (+ start-line 24))))))))

(defun lispm-spotlight--range-target (buffer ephemeral-p start-line end-line label)
  "Build a target for BUFFER from START-LINE to END-LINE named LABEL.
EPHEMERAL-P marks whether BUFFER was opened solely for spotlighting."
  (let* ((total (lispm-spotlight--buffer-line-count buffer))
         (lo (max 1 (min start-line total)))
         (hi (max lo (min end-line total)))
         (whole-p (and (= lo 1) (>= hi total))))
    (lispm-spotlight-target--create
     :buffer buffer
     :ephemeral-p ephemeral-p
     :anchor-line lo
     :line-ranges (list (cons lo (if (and whole-p
                                          (> total lispm-spotlight-whole-threshold))
                                     (min total (max 1 lispm-spotlight-whole-header-lines))
                                   hi)))
     :note (cond
            (whole-p (lispm-tr "reading %s" label))
            ((= lo hi) (lispm-tr "reading line %d" lo))
            (t (lispm-tr "reading lines %d–%d" lo hi))))))

(defun lispm-spotlight--parse-diagnostic-lines (text)
  "Extract sorted positive line numbers from `LINE:COL:' lines in TEXT."
  (when (and (stringp text) (not (string-empty-p text)))
    (save-match-data
      (let ((pos 0)
            lines)
        (while (string-match "^\\([0-9]+\\):[0-9]+:" text pos)
          (setq pos (match-end 0))
          (let ((line (string-to-number (match-string 1 text))))
            (when (> line 0)
              (push line lines))))
        (and lines (delete-dups (sort lines #'<)))))))

(defun lispm-spotlight--resolve-eval-def (spec root)
  "Resolve an `eval-definition' spotlight SPEC under ROOT."
  (save-match-data
    (let ((text (or (plist-get spec :result) ""))
          (explicit (plist-get spec :path)))
      (when (string-match
             "\\`Evaluated the definition of \\([^ \n]+\\) in \\([^,\n]+\\), line \\([0-9]+\\)\\."
             text)
        (let* ((sym-name (match-string 1 text))
               (raw-file (or explicit (match-string 2 text)))
               (line (string-to-number (match-string 3 text)))
               (file (expand-file-name raw-file root))
               (visited (and (> line 0) (lispm-spotlight--visit-file file))))
          (when visited
            (let ((buf (car visited)))
              (lispm-spotlight-target--create
               :buffer buf
               :ephemeral-p (cdr visited)
               :anchor-line line
               :line-ranges (list (cons line
                                        (lispm-spotlight--definition-end-line buf line)))
               :note (lispm-tr "evaluated %s" sym-name)))))))))

(defun lispm-spotlight--resolve-diagnostics (spec)
  "Resolve a `flymake-diagnostics' spotlight SPEC."
  (when-let* ((hits (lispm-spotlight--parse-diagnostic-lines (plist-get spec :result)))
              (visited (cond
                        ((plist-get spec :buffer)
                         (cons (plist-get spec :buffer) nil))
                        ((plist-get spec :file)
                         (lispm-spotlight--visit-file (plist-get spec :file)))))
              (buf (car visited)))
    (when (buffer-live-p buf)
      (lispm-spotlight-target--create
       :buffer buf
       :ephemeral-p (cdr visited)
       :anchor-line (car hits)
       :line-ranges (lispm-ui-spotlight-coalesce-lines hits)
       :note (lispm-tr "diagnostics in %s" (buffer-name buf))))))

(defun lispm-spotlight--resolve-spec (spec root)
  "Resolve spotlight plist SPEC under ROOT into a target struct, or nil."
  (pcase (plist-get spec :kind)
    ('eval-definition (lispm-spotlight--resolve-eval-def spec root))
    ('flymake-diagnostics (lispm-spotlight--resolve-diagnostics spec))
    ('read-file
     (when-let* ((file (plist-get spec :file))
                 (visited (lispm-spotlight--visit-file file)))
       (let* ((start (max 1 (or (plist-get spec :start-line) 1)))
              (limit (max 1 (or (plist-get spec :limit) 2000))))
         (lispm-spotlight--range-target (car visited) (cdr visited)
                                        start (+ start limit -1)
                                        (file-name-nondirectory file)))))
    ('read-buffer
     (let ((buffer (plist-get spec :buffer))
           (start (plist-get spec :start-line))
           (limit (max 1 (or (plist-get spec :limit) 2000)))
           (tail (plist-get spec :tail))
           (query (plist-get spec :query)))
       (when (buffer-live-p buffer)
         (if (and (stringp query) (not (string-empty-p query)))
             (when-let* ((hits (lispm-spotlight--query-lines
                                buffer start tail limit query)))
               (lispm-spotlight-target--create
                :buffer buffer :ephemeral-p nil :anchor-line (car hits)
                :line-ranges (lispm-ui-spotlight-coalesce-lines hits)
                :note (lispm-tr "matching “%s”" query)))
           (let* ((total (lispm-spotlight--buffer-line-count buffer))
                  (lo (if tail (max 1 (- total tail -1)) (max 1 (or start 1))))
                  (hi (if tail total (+ lo limit -1))))
             (lispm-spotlight--range-target buffer nil lo hi (buffer-name buffer)))))))
    ((and (or 'search-files 'xref-definitions 'xref-references) kind)
     (when-let* ((parsed (lispm-spotlight--parse-file-hits (plist-get spec :result) root))
                 (file (car parsed))
                 (hits (cdr parsed))
                 (visited (lispm-spotlight--visit-file file)))
       (let* ((buffer (car visited))
              (ephemeral-p (cdr visited))
              (subject (or (plist-get spec :subject) (file-name-nondirectory file)))
              (ranges (if (and (eq kind 'xref-definitions) (= (length hits) 1))
                          (list (cons (car hits)
                                      (lispm-spotlight--definition-end-line
                                       buffer (car hits))))
                        (lispm-ui-spotlight-coalesce-lines hits))))
         (lispm-spotlight-target--create
          :buffer buffer :ephemeral-p ephemeral-p :anchor-line (car hits)
          :line-ranges ranges
          :note (pcase kind
                  ('xref-definitions (lispm-tr "definition of %s" subject))
                  ('xref-references (lispm-tr "references to %s" subject))
                  (_ (lispm-tr "matching “%s”" subject)))))))))

;;;; Reactive store & viewport scroll arbitration

(defvar lispm-spotlight--store nil
  "Singleton `lispm-ui-spotlight-store' managing read spotlight beams.")

(defvar lispm-spotlight--steps (make-hash-table :test #'eq)
  "Agent -> integer step counter incremented on each model response.")

(defvar lispm-spotlight--scroll-lock nil
  "Cons `(AGENT . EXPIRES-AT)' holding temporary stage scroll priority.")

(defun lispm-spotlight--ensure-store ()
  "Return the active `lispm-ui-spotlight-store', syncing custom settings."
  (unless (lispm-ui-spotlight-store-p lispm-spotlight--store)
    (setq lispm-spotlight--store
          (lispm-ui-spotlight-make-store
           :tint lispm-spotlight-tint
           :fade-duration lispm-spotlight-fade
           :priority 5
           :on-reap #'lispm-spotlight--reap-ephemeral)))
  (setf (lispm-ui-spotlight-store-tint lispm-spotlight--store)
        lispm-spotlight-tint
        (lispm-ui-spotlight-store-fade-duration lispm-spotlight--store)
        lispm-spotlight-fade)
  lispm-spotlight--store)

(defun lispm-spotlight-reset ()
  "Dispose and recreate spotlight reactive state (for test isolation)."
  (when (lispm-ui-spotlight-store-p lispm-spotlight--store)
    (lispm-ui-spotlight-dispose-store lispm-spotlight--store)
    (setq lispm-spotlight--store nil))
  (clrhash lispm-spotlight--steps)
  (setq lispm-spotlight--scroll-lock nil))

(defun lispm-spotlight--agent-key (agent)
  "Return the stable actor key string for AGENT."
  (if agent (symbol-name (lispm-principal-name agent)) "LispM"))

(defun lispm-spotlight--actor (agent)
  "Build a `lispm-ui-spotlight-actor' for AGENT."
  (let ((label (lispm-spotlight--agent-key agent)))
    (lispm-ui-spotlight-actor-create
     :key label
     :label label
     :color (or (lispm-edits--color agent) "#89b4fa")
     :glyph "λ")))

(defun lispm-spotlight--window-agent (window)
  "Return the LispM agent spoken to in WINDOW, or nil."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (cond
       ((and (local-variable-p 'lispm-chat-agent buffer)
             (buffer-local-value 'lispm-chat-agent buffer)))
       ((and (local-variable-p 'lispm-session-agent buffer)
             (buffer-local-value 'lispm-session-agent buffer)))))))

(defun lispm-spotlight--primary-agent ()
  "Return the agent currently attended in any visible window, or nil."
  (or (lispm-spotlight--window-agent (selected-window))
      (let ((windows (cl-loop for frame in (visible-frame-list)
                              append (window-list frame 'nomini))))
        (seq-some #'lispm-spotlight--window-agent
                  (sort windows
                        (lambda (a b) (> (window-use-time a) (window-use-time b))))))))

(defun lispm-spotlight--follow-agent-p (agent now)
  "Return non-nil if AGENT may record or scroll its buffer on stage at NOW.
Background root agents record into their own inactive stage subject
without touching visible windows; sibling subagents share scroll-lock."
  (and lispm-spotlight-follow
       (let* ((primary (lispm-spotlight--primary-agent))
              (primary-root (and primary (lispm-stage-root-agent primary)))
              (agent-root (and agent (lispm-stage-root-agent agent))))
         (cond
          ((or (null primary) (null agent) (eq agent primary))
           (setq lispm-spotlight--scroll-lock
                 (cons agent (+ now lispm-spotlight-scroll-lock-seconds)))
           t)
          ((not (eq agent-root primary-root))
           ;; Different root session: `lispm-stage-window' records silently
           ;; on `agent-root' without disturbing `primary-root''s windows.
           t)
          ((and (lispm-stage-descendant-p agent primary)
                (not (lispm-edits-active-p))
                (or (null lispm-spotlight--scroll-lock)
                    (eq (car lispm-spotlight--scroll-lock) agent)
                    (>= now (cdr lispm-spotlight--scroll-lock))))
           (setq lispm-spotlight--scroll-lock
                 (cons agent (+ now lispm-spotlight-scroll-lock-seconds)))
           t)
          (t nil)))))

(defun lispm-spotlight--reapable-buffer-p (buffer)
  "Return non-nil if ephemeral BUFFER is unseen, unmodified, and free of beams."
  (and (buffer-live-p buffer)
       (buffer-local-value 'lispm-spotlight--ephemeral buffer)
       (not (buffer-modified-p buffer))
       (not (get-buffer-window buffer 'visible))
       (not (lispm-stage-retains-buffer-p buffer))
       (not (and (lispm-ui-spotlight-store-p lispm-spotlight--store)
                 (lispm-ui-spotlight-buffer-active-p lispm-spotlight--store buffer)))
       (not (lispm-edits-active-p buffer))))

(defun lispm-spotlight--reap-ephemeral ()
  "Kill ephemeral buffers that are no longer visible or illuminated."
  (setq lispm-spotlight--ephemeral-buffers
        (seq-filter (lambda (buffer)
                      (cond
                       ((not (buffer-live-p buffer)) nil)
                       ((not (buffer-local-value 'lispm-spotlight--ephemeral buffer))
                        nil)
                       ((lispm-spotlight--reapable-buffer-p buffer)
                        (kill-buffer buffer)
                        nil)
                       (t t)))
                    lispm-spotlight--ephemeral-buffers)))

(defun lispm-spotlight--tick (&optional now)
  "Advance the reactive spotlight store clock to NOW and reap ephemerals."
  (when (lispm-ui-spotlight-store-p lispm-spotlight--store)
    (lispm-ui-spotlight-tick lispm-spotlight--store now))
  (lispm-spotlight--reap-ephemeral))

(defun lispm-spotlight--fade-agent (agent fade-at &optional before-step)
  "Schedule steady beams of AGENT before BEFORE-STEP to fade at FADE-AT."
  (when (lispm-ui-spotlight-store-p lispm-spotlight--store)
    (let ((key (lispm-spotlight--agent-key agent)))
      (lispm-ui-spotlight-fade-where
       lispm-spotlight--store
       (lambda (beam)
         (and (equal (lispm-ui-spotlight-actor-key
                      (lispm-ui-spotlight-beam-actor beam))
                     key)
              (or (null before-step)
                  (< (lispm-ui-spotlight-beam-step beam) before-step))))
       fade-at))))

(defun lispm-spotlight-cast (agent target &optional now)
  "Cast AGENT's spotlight onto `lispm-spotlight-target' TARGET at time NOW."
  (let* ((now (or now (float-time)))
         (store (lispm-spotlight--ensure-store))
         (buffer (lispm-spotlight-target-buffer target))
         (step (if agent (gethash agent lispm-spotlight--steps 0) 0))
         (busy (and agent (lispm-agent-busy-p agent))))
    (when (buffer-live-p buffer)
      (let ((beam (lispm-ui-spotlight-cast
                   store
                   (lispm-spotlight--actor agent)
                   buffer
                   (lispm-spotlight-target-anchor-line target)
                   (lispm-spotlight-target-line-ranges target)
                   :step step
                   :note (lispm-spotlight-target-note target)
                   :fade-at (unless busy (+ now lispm-spotlight-hold))
                   :retire-before-step (and agent t)
                   :now now)))
        (lispm-edits-mark-shared buffer)
        (when (lispm-spotlight--follow-agent-p agent now)
          (when-let* ((window (lispm-stage-window buffer t agent 'code)))
            (lispm-edits--reveal
             window
             (marker-position (lispm-ui-spotlight-beam-tag-marker beam)))
            (lispm-spotlight--reap-ephemeral)))
        beam))))

;;;; Hooks

(defun lispm-spotlight--on-response (agent _message)
  "Advance AGENT's response step counter when a new model response arrives."
  (puthash agent (1+ (gethash agent lispm-spotlight--steps 0)) lispm-spotlight--steps))

(defun lispm-spotlight--builtin-spec (tool input result)
  "Fallback spotlight plist for built-in TOOL on INPUT and RESULT."
  (pcase (and tool (lispm-tool-name tool))
    ("read_file"
     (when-let* ((path (alist-get 'path input))
                 (file (lispm-tools--file path)))
       (unless (or (lispm-media-kind file) (file-directory-p file))
         (list :kind 'read-file
               :file file
               :start-line (or (alist-get 'offset input)
                               (cdr (lispm-tools-reference path))
                               1)
               :limit (or (alist-get 'limit input) 2000)))))
    ("read_buffer"
     (when-let* ((name (alist-get 'buffer input)))
       (list :kind 'read-buffer
             :buffer (lispm-tools--buffer name)
             :start-line (alist-get 'offset input)
             :limit (or (alist-get 'limit input) 2000)
             :tail (alist-get 'tail input)
             :query (alist-get 'query input))))
    ("search_files"
     (when-let* ((pattern (alist-get 'pattern input)))
       (list :kind 'search-files
             :subject (string-remove-prefix "(?i)" pattern)
             :result result)))
    ("xref_definitions"
     (list :kind 'xref-definitions
           :subject (alist-get 'identifier input)
           :result result))
    ("xref_references"
     (list :kind 'xref-references
           :subject (alist-get 'identifier input)
           :result result))
    ("eval_definition"
     (list :kind 'eval-definition
           :subject (alist-get 'name input)
           :path (alist-get 'path input)
           :result result))
    ("flymake_diagnostics"
     (list :kind 'flymake-diagnostics
           :file (and (alist-get 'path input)
                      (lispm-tools--file (alist-get 'path input)))
           :buffer (and (alist-get 'buffer input)
                        (lispm-tools--buffer (alist-get 'buffer input)))
           :result result))))

(defun lispm-spotlight--on-tool-call (agent phase tool input result errorp)
  "Light what AGENT's call of TOOL on INPUT read in RESULT at PHASE `finish'.
Nothing is shown when ERRORP is non-nil."
  (when (and (eq phase 'finish) (not errorp) (lispm-spotlight--live-p))
    (let* ((root (if agent (lispm-principal-root agent) default-directory))
           (default-directory root)
           (spec (condition-case nil
                     (or (and (fboundp 'lispm-tool-would-spotlight)
                              (lispm-tool-would-spotlight tool input result))
                         (lispm-spotlight--builtin-spec tool input result))
                   (error nil))))
      (when-let* ((spec spec)
                  (target (lispm-spotlight--resolve-spec spec root)))
        (lispm-spotlight-cast agent target)))))

(defun lispm-spotlight--on-buffer-change (buffer _old _new)
  "Fade `lispm-current-agent''s earlier read spotlights when it edits BUFFER."
  (when (and (lispm-spotlight--live-p) lispm-current-agent)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local lispm-spotlight--ephemeral nil
                    lispm-ui-stage-managed nil)))
    (lispm-spotlight--fade-agent lispm-current-agent (float-time))))

(defun lispm-spotlight--on-state-change (agent)
  "Hold and fade AGENT's steady read spotlights once AGENT stops working."
  (when (and agent (not (lispm-agent-busy-p agent)))
    (lispm-spotlight--fade-agent agent (+ (float-time) lispm-spotlight-hold))))

(defun lispm-spotlight--on-session-end (agent)
  "Clean up step state and fade AGENT's spotlights when its session closes."
  (remhash agent lispm-spotlight--steps)
  (lispm-spotlight--fade-agent agent (float-time)))

(add-hook 'lispm-stage--reap-hooks #'lispm-spotlight--reap-ephemeral)
(add-hook 'lispm-response-functions #'lispm-spotlight--on-response)
(add-hook 'lispm-tool-call-functions #'lispm-spotlight--on-tool-call)
(add-hook 'lispm-buffer-change-functions #'lispm-spotlight--on-buffer-change)
(add-hook 'lispm-state-change-functions #'lispm-spotlight--on-state-change)
(add-hook 'lispm-session-end-functions #'lispm-spotlight--on-session-end)

(provide 'lispm-spotlight)
;;; lispm-spotlight.el ends here
