;;; lispm-eval-stage.el --- Agents' Lisp, test and web calls on stage  -*- lexical-binding: t; -*-

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

;; Projects Lisp evaluation (`run_elisp', `eval_elisp', `run_lisp'), test runs
;; (`ert_run'), and browser actions (`web_act', `web_fetch', `web_search')
;; onto the stage window.
;;
;; Lisp calls display syntax-highlighted S-expressions above their printed
;; output and value; `ert_run' renders a visual pass/fail dot bar with
;; clickable failed-test symbols; `web_act' shows the live WebKit xwidget
;; buffer when available, or a structured reader view with clickable URLs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-core)
(require 'lispm-i18n)
(require 'lispm-tools)
(require 'lispm-inspect)
(require 'lispm-stage-runner)

(defvar lispm-web--webkit-session)
(declare-function lispm-web--fit-xwidget "ext:lispm-web" (buf xw))

(defgroup lispm-eval-stage nil
  "Agents' Lisp, ERT and web calls shown on the stage."
  :group 'lispm
  :prefix "lispm-eval-stage-")

(defcustom lispm-show-eval-stage t
  "Whether agents' Lisp, test and browser calls are shown on the stage."
  :type 'boolean
  :group 'lispm-eval-stage)

(defcustom lispm-eval-stage-follow t
  "Whether an agent's Lisp or browser stage comes into view automatically."
  :type 'boolean
  :group 'lispm-eval-stage)

(defvar lispm-eval-stage--in-batch nil
  "Non-nil to show the eval and web stage in batch mode too, as tests do.")

(defun lispm-eval-stage--live-p ()
  "Return non-nil if the eval and web stage is active in this Emacs session."
  (lispm-stage-runner-live-p lispm-show-eval-stage lispm-eval-stage--in-batch))

;;;; Pure parsers & summaries

;;; Post: Returns `(:passed INT :failed INT :total INT)' or nil.
(defun lispm-eval-stage--parse-ert-summary (text)
  "Extract test counts from `ert_run' report TEXT, or nil."
  (when (and (stringp text) (not (string-empty-p text)))
    (save-match-data
      (when (string-match "\\`Ran \\([0-9]+\\) test" text)
        (let* ((total (string-to-number (match-string 1 text)))
               (first-line (car (split-string text "\n")))
               (passed (if (string-match "\\([0-9]+\\) passed" first-line)
                           (string-to-number (match-string 1 first-line))
                         0))
               (failed (if (string-match "\\([0-9]+\\) failed\\b" first-line)
                           (string-to-number (match-string 1 first-line))
                         0)))
          (list :total total :passed passed :failed failed))))))

;;; Post: Returns `(:title S :url S :action S :body S)' parsed from web TEXT.
(defun lispm-eval-stage--parse-web-result (text)
  "Split `web_fetch' or `web_act' result TEXT into metadata and body."
  (save-match-data
    (let ((rest (or text ""))
          title
          url
          action)
      (when (string-match "\\`Title: \\([^\n]*\\)\n" rest)
        (setq title (string-trim (match-string 1 rest))
              rest (substring rest (match-end 0))))
      (when (string-match "\\`URL: \\([^\n]*\\)\n" rest)
        (setq url (string-trim (match-string 1 rest))
              rest (substring rest (match-end 0))))
      (when (string-match "\\`Action: \\([^\n]*\\)\n" rest)
        (setq action (string-trim (match-string 1 rest))
              rest (substring rest (match-end 0))))
      (list :title title
            :url url
            :action action
            :body (string-trim-left rest "\n")))))

(defun lispm-eval-stage--summarize-call (tool-name input)
  "Build a concise header label for TOOL-NAME on INPUT."
  (pcase tool-name
    ((or "run_elisp" "eval_elisp" "run_lisp")
     (let* ((code (string-trim (or (alist-get 'code input) "")))
            (flat (replace-regexp-in-string "[ \t\n]+" " " code)))
       (format "%s %s" tool-name
               (truncate-string-to-width flat 40 nil nil "…"))))
    ("ert_run"
     (format "ert_run %s" (or (alist-get 'selector input) "t")))
    ("web_search"
     (format "web_search “%s”" (or (alist-get 'query input) "")))
    ("web_fetch"
     (format "web_fetch %s" (or (alist-get 'url input) "")))
    ("web_act"
     (let ((act (or (alist-get 'action input) "snapshot"))
           (tgt (alist-get 'target input))
           (url (alist-get 'url input)))
       (string-join (delq nil (list "web_act" act tgt url)) " ")))))

(defun lispm-eval-stage--kind-for-tool (tool-name)
  "Classify TOOL-NAME into `lisp', `ert', `web', or nil."
  (pcase tool-name
    ((or "run_elisp" "eval_elisp" "run_lisp") 'lisp)
    ("ert_run" 'ert)
    ((or "web_search" "web_fetch" "web_act") 'web)
    (_ nil)))

;;;; Stage buffer & live WebKit helpers

(defvar-local lispm-eval-stage--header nil
  "Current header-line string of an eval or web stage buffer.")

(define-derived-mode lispm-eval-stage-mode special-mode "LispM-Stage"
  "Major mode for Lisp, ERT and web stage buffers."
  :interactive nil
  (lispm-stage-runner-setup-mode 'lispm-eval-stage--header))

(defun lispm-eval-stage--buffer (agent kind &optional call-id)
  "Return or create AGENT's stage buffer for KIND (`lisp' or `web') and CALL-ID."
  (lispm-stage-runner-buffer (if (eq kind 'web) "web" "lisp")
                             agent 'lispm-eval-stage-mode call-id))

(defun lispm-eval-stage--header-text (agent summary status-str face)
  "Format a stage header line for AGENT, SUMMARY, STATUS-STR and FACE."
  (lispm-stage-runner-header agent summary status-str face))

(defun lispm-eval-stage--fontify-elisp (code)
  "Return CODE fontified with `emacs-lisp-mode' syntax faces."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (insert (or code ""))
    (font-lock-ensure)
    (buffer-string)))

(defun lispm-eval-stage--webkit-buffer ()
  "Return the live graphical WebKit xwidget session buffer, if active."
  (when (bound-and-true-p lispm-web--webkit-session)
    (let ((buf (plist-get lispm-web--webkit-session :buffer))
          (xw (plist-get lispm-web--webkit-session :xwidget)))
      (and (buffer-live-p buf)
           (fboundp 'xwidgetp)
           (xwidgetp xw)
           buf))))

(defun lispm-eval-stage--show-buffer (agent buffer &optional kind)
  "Project BUFFER of KIND onto AGENT's stage window unless edits animate."
  (lispm-stage-runner-present
   agent buffer (or kind 'lisp)
   :follow lispm-eval-stage-follow
   :top-p t
   :on-shown (lambda (_win)
               (when (and (bound-and-true-p lispm-web--webkit-session)
                          (eq buffer (plist-get lispm-web--webkit-session :buffer))
                          (fboundp 'lispm-web--fit-xwidget))
                 (lispm-web--fit-xwidget buffer (plist-get lispm-web--webkit-session :xwidget))))))

;;;; Content renderers

(defun lispm-eval-stage--render-lisp (code result errorp running-p)
  "Render Elisp CODE and RESULT (ERRORP / RUNNING-P) in current buffer."
  (insert (propertize ";; Code\n" 'face 'font-lock-comment-face)
          (string-trim-right (lispm-eval-stage--fontify-elisp code))
          "\n\n"
          (propertize ";; Output\n" 'face 'font-lock-comment-face))
  (cond
   (running-p
    (insert (propertize "…" 'face 'shadow) "\n"))
   (errorp
    (insert (propertize (or result "") 'face 'lispm-stage-runner-fail) "\n"))
   ((or (null result) (string-empty-p result))
    (insert (propertize "=> nil" 'face 'font-lock-constant-face) "\n"))
   (t
    (let ((lines (split-string result "\n")))
      (dolist (line lines)
        (if (string-prefix-p "=> " line)
            (insert (propertize "=> " 'face 'lispm-stage-runner-ok)
                    (lispm-eval-stage--fontify-elisp (substring line 3))
                    "\n")
          (insert line "\n")))))))

(defun lispm-eval-stage--render-ert (selector result errorp)
  "Render `ert_run' SELECTOR report RESULT (with ERRORP) in current buffer."
  (insert (propertize (format ";; ERT %s\n\n" (or selector "t"))
                      'face 'font-lock-comment-face))
  (if errorp
      (insert (propertize (or result "") 'face 'lispm-stage-runner-fail) "\n")
    (when-let* ((stats (lispm-eval-stage--parse-ert-summary result)))
      (let* ((passed (min 24 (plist-get stats :passed)))
             (failed (min 24 (plist-get stats :failed)))
             (bar (concat (propertize (make-string passed ?●) 'face 'lispm-stage-runner-ok)
                          (propertize (make-string failed ?✗) 'face 'lispm-stage-runner-fail))))
        (unless (string-empty-p bar)
          (insert bar "\n\n"))))
    (dolist (line (split-string (or result "") "\n"))
      (cond
       ((string-match "\\`\\(FAILED\\)[ \t]+\\([^ \t\n]+\\)" line)
        (let* ((sym-name (match-string 2 line))
               (sym (intern-soft sym-name)))
          (insert (propertize "FAILED  " 'face 'lispm-stage-runner-fail)
                  (if sym (lispm-present sym-name (list 'symbol sym)) sym-name)
                  (substring line (match-end 2))
                  "\n")))
       ((string-prefix-p "Ran " line)
        (insert (propertize line 'face 'bold) "\n"))
       (t
        (insert line "\n"))))))

(defun lispm-eval-stage--link-urls ()
  "Turn `http://' and `https://' URLs in current buffer into presentations."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward "https?://[^ \t\n\"'>)\\]]+" nil t)
        (let ((url (match-string-no-properties 0))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (add-text-properties
           beg end
           (list 'lispm-object (list 'url url)
                 'mouse-face 'highlight
                 'keymap lispm-presentation-map))
          (add-face-text-property beg end 'lispm-presentation t))))))

(defun lispm-eval-stage--render-web (tool-name input result errorp running-p)
  "Render web TOOL-NAME call on INPUT with RESULT, ERRORP and RUNNING-P."
  (cond
   (running-p
    (insert (propertize (lispm-eval-stage--summarize-call tool-name input)
                        'face 'lispm-stage-runner-prompt)
            "\n\n"
            (propertize "…" 'face 'shadow) "\n"))
   (errorp
    (insert (propertize (or result "") 'face 'lispm-stage-runner-fail) "\n"))
   ((equal tool-name "web_search")
    (insert (propertize (format "Search: %s\n\n" (or (alist-get 'query input) ""))
                        'face 'bold)
            (or result "") "\n")
    (lispm-eval-stage--link-urls))
   (t
    (let* ((parsed (lispm-eval-stage--parse-web-result result))
           (title (plist-get parsed :title))
           (url (plist-get parsed :url))
           (action (plist-get parsed :action))
           (body (plist-get parsed :body)))
      (when (and title (not (string-empty-p title)))
        (insert (propertize title 'face '(:weight bold :height 1.1)) "\n"))
      (when (and url (not (string-empty-p url)))
        (insert (lispm-present url (list 'url url)) "\n"))
      (when (and action (not (string-empty-p action)))
        (insert "\n"
                (propertize (concat "⚡ " action) 'face 'lispm-stage-runner-ok)
                "\n"))
      (insert "\n" body "\n")
      (lispm-eval-stage--link-urls)))))

;;;; Session lifecycle & hooks

(defvar lispm-current-call)

(cl-defstruct (lispm-eval-stage--session
               (:constructor lispm-eval-stage--session-create) (:copier nil))
  "Active Lisp, ERT or web call staged for AGENT."
  key
  agent
  call-id
  kind
  tool-name
  input
  summary
  buffer
  started-at
  isolated)

(defvar lispm-eval-stage--sessions (make-hash-table :test #'equal)
  "Call key -> active `lispm-eval-stage--session'.")

(defvar lispm-eval-stage--isolated-buffers (make-hash-table :test #'eq)
  "Agent -> ephemeral isolated eval stage buffers created during concurrency.")

(defvar lispm-eval-stage--ticker nil
  "Timer updating elapsed times in active Lisp and web stage headers.")

(defun lispm-eval-stage--prune-isolated (agent &optional force)
  "Kill retired isolated eval stage buffers owned by AGENT.
Unless FORCE is non-nil, keep buffers still shown in a visible window."
  (let (keep)
    (dolist (buf (gethash agent lispm-eval-stage--isolated-buffers))
      (when (buffer-live-p buf)
        (let (active)
          (maphash (lambda (_k session)
                     (when (eq (lispm-eval-stage--session-buffer session) buf)
                       (setq active t)))
                   lispm-eval-stage--sessions)
          (cond
           (active (push buf keep))
           ((and (not force) (get-buffer-window buf 'visible))
            (push buf keep))
           (t (kill-buffer buf))))))
    (if keep
        (puthash agent (nreverse keep) lispm-eval-stage--isolated-buffers)
      (remhash agent lispm-eval-stage--isolated-buffers))))

(defun lispm-eval-stage--resolve-key (agent &optional call-id)
  "Return the session table key for AGENT and optional CALL-ID."
  (or call-id (bound-and-true-p lispm-current-call) agent))

(defun lispm-eval-stage--agent-busy-p (agent &optional except-key)
  "Return non-nil when AGENT has another active eval stage besides EXCEPT-KEY."
  (let (found)
    (maphash (lambda (key session)
               (when (and (not (equal key except-key))
                          (eq (lispm-eval-stage--session-agent session) agent))
                 (setq found session)))
             lispm-eval-stage--sessions)
    found))

(defun lispm-eval-stage--refresh-running (session &optional now)
  "Update SESSION's running header line at time NOW."
  (lispm-stage-runner-set-header
   (lispm-eval-stage--session-buffer session)
   'lispm-eval-stage-mode
   'lispm-eval-stage--header
   (lispm-stage-runner-running-header
    (lispm-eval-stage--session-agent session)
    (lispm-eval-stage--session-summary session)
    (lispm-eval-stage--session-started-at session)
    (or now (float-time)))))

(defun lispm-eval-stage--tick ()
  "Refresh elapsed durations for all running eval and web stage sessions."
  (lispm-stage-runner-tick
   lispm-eval-stage--sessions
   'lispm-eval-stage--ticker
   #'lispm-eval-stage--refresh-running))

(defun lispm-eval-stage--start (agent kind tool-name input)
  "Stage AGENT's starting call of TOOL-NAME (classified as KIND) on INPUT."
  (lispm-eval-stage--prune-isolated agent)
  (let* ((now (float-time))
         (call-id (bound-and-true-p lispm-current-call))
         (key (lispm-eval-stage--resolve-key agent call-id))
         (isolated (and call-id (lispm-eval-stage--agent-busy-p agent key)))
         (summary (lispm-eval-stage--summarize-call tool-name input))
         (webkit-buf (and (equal tool-name "web_act")
                          (lispm-eval-stage--webkit-buffer)))
         (buf (or webkit-buf
                  (lispm-eval-stage--buffer agent (if (eq kind 'web) 'web 'lisp)
                                            (and isolated call-id))))
         (session (lispm-eval-stage--session-create
                   :key key :agent agent :call-id call-id
                   :kind kind :tool-name tool-name
                   :input input :summary summary
                   :buffer buf :started-at now
                   :isolated (and isolated (not webkit-buf) t))))
    (when (lispm-eval-stage--session-isolated session)
      (push buf (gethash agent lispm-eval-stage--isolated-buffers)))
    (unless webkit-buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (pcase kind
            ('lisp (lispm-eval-stage--render-lisp (alist-get 'code input) nil nil t))
            ('ert (insert (propertize (format ";; ERT %s\n\n…\n"
                                              (or (alist-get 'selector input) "t"))
                                      'face 'font-lock-comment-face)))
            ('web (lispm-eval-stage--render-web tool-name input nil nil t))))))
    (puthash key session lispm-eval-stage--sessions)
    (lispm-eval-stage--refresh-running session now)
    (lispm-stage-runner-ensure-ticker
     lispm-eval-stage--sessions 'lispm-eval-stage--ticker #'lispm-eval-stage--tick)
    (lispm-eval-stage--show-buffer agent buf (if (eq kind 'web) 'web 'lisp))))

(defun lispm-eval-stage--finish (agent kind tool-name input result errorp)
  "Finalize AGENT's stage for TOOL-NAME (KIND) on INPUT with RESULT and ERRORP."
  (let* ((call-id (bound-and-true-p lispm-current-call))
         (key (lispm-eval-stage--resolve-key agent call-id))
         (prev (gethash key lispm-eval-stage--sessions))
         (started (if prev (lispm-eval-stage--session-started-at prev) (float-time)))
         (elapsed (max 0.0 (- (float-time) started)))
         (summary (lispm-eval-stage--summarize-call tool-name input))
         (webkit-buf (and (equal tool-name "web_act")
                          (not errorp)
                          (lispm-eval-stage--webkit-buffer)))
         (buf (or webkit-buf
                  (and prev (lispm-eval-stage--session-buffer prev))
                  (lispm-eval-stage--buffer agent (if (eq kind 'web) 'web 'lisp))))
         (ert-stats (and (eq kind 'ert) (not errorp)
                         (lispm-eval-stage--parse-ert-summary result)))
         (failed (or errorp
                     (and ert-stats (> (plist-get ert-stats :failed) 0)))))
    (remhash key lispm-eval-stage--sessions)
    (lispm-eval-stage--tick)
    (unless webkit-buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (pcase kind
            ('lisp (lispm-eval-stage--render-lisp (alist-get 'code input) result errorp nil))
            ('ert (lispm-eval-stage--render-ert (alist-get 'selector input) result errorp))
            ('web (lispm-eval-stage--render-web tool-name input result errorp nil))))))
    (lispm-stage-runner-set-header
     buf 'lispm-eval-stage-mode 'lispm-eval-stage--header
     (lispm-eval-stage--header-text
      agent summary
      (if failed
          (format "✗ %s · %.1fs" (lispm-tr "failed") elapsed)
        (format "✓ %.1fs" elapsed))
      (if failed 'lispm-stage-runner-fail 'lispm-stage-runner-ok)))
    (lispm-eval-stage--show-buffer agent buf (if (eq kind 'web) 'web 'lisp))))

(defun lispm-eval-stage--on-tool-call (agent phase tool input result errorp)
  "Stage AGENT's Lisp or web TOOL on INPUT with RESULT and ERRORP at PHASE."
  (when (and (lispm-eval-stage--live-p) tool)
    (let ((name (lispm-tool-name tool)))
      (when-let* ((kind (lispm-eval-stage--kind-for-tool name)))
        (pcase phase
          ('start (lispm-eval-stage--start agent kind name input))
          ('finish (lispm-eval-stage--finish agent kind name input result errorp)))))))

(defun lispm-eval-stage--on-session-end (agent)
  "Clean up any running Lisp or web stage state and isolated buffers for AGENT."
  (let (keys)
    (maphash (lambda (key session)
               (when (eq (lispm-eval-stage--session-agent session) agent)
                 (push key keys)))
             lispm-eval-stage--sessions)
    (dolist (key keys)
      (remhash key lispm-eval-stage--sessions)))
  (lispm-eval-stage--prune-isolated agent t)
  (lispm-eval-stage--tick))

(add-hook 'lispm-tool-call-functions #'lispm-eval-stage--on-tool-call)
(add-hook 'lispm-session-end-functions #'lispm-eval-stage--on-session-end)

(provide 'lispm-eval-stage)
;;; lispm-eval-stage.el ends here
