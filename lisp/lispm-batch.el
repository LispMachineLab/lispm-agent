;;; lispm-batch.el --- Headless batch runner and benchmark telemetry  -*- lexical-binding: t; -*-

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

;; Unattended batch execution for LispM (`bin/lispm-batch'), designed for
;; headless CLI automation and benchmark harnesses such as FrontierHarness
;; (Harbor / Terminal-Bench 2.0 and Pier / DeepSWE).
;;
;; In a batch run:
;; - Every tool class runs without interactive prompts (`auto'), including
;;   writes outside the project root inside task containers.
;; - Background side-requests (auto-titling, idle compaction, git snapshots)
;;   are disabled so token accounting reflects only the task turn itself.
;; - Every model response is logged immediately to `lispm-calls.jsonl' and
;;   `lispm-usage.json' so that even if a container watchdog kills the
;;   process on timeout, per-call token and cache telemetry is preserved on
;;   disk.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-agents)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-runtime)
(require 'lispm-memory)
(require 'lispm-title)
(require 'lispm-journal)
(require 'lispm-sandbox)

(defconst lispm-batch-policy
  '((read . auto) (memory . auto) (comm . auto) (lisp . auto)
    (write . auto) (shell . auto) (exec . auto) (mcp . auto) (web . auto))
  "Approval policy for autonomous headless batch execution.")

(defconst lispm-batch-condition-handlers
  '((request-failed retry :after 5 :times 15 :then abort)
    (step-limit abort)
    (output-limit retry-larger :then abort)
    (context-full compact :then abort)
    (refused abort)
    (paused continue :times 10 :then abort)
    (compaction-failed skip)
    (tool-error report)
    (interrupted continue :then abort))
  "Condition handlers for unattended batch turns.")

(defconst lispm-batch-provider-routes
  '(("fireworks_ai" :api openai
     :url "https://api.fireworks.ai/inference/v1"
     :key-env "FIREWORKS_API_KEY"
     :features (streaming reasoning-effort stream-usage))
    ("fireworks" :api openai
     :url "https://api.fireworks.ai/inference/v1"
     :key-env "FIREWORKS_API_KEY"
     :features (streaming reasoning-effort stream-usage))
    ("moonshot" :api openai
     :url "https://api.moonshot.ai/v1"
     :key-env "MOONSHOT_API_KEY"
     :features (streaming stream-usage))
    ("kimi" :api openai
     :url "https://api.moonshot.ai/v1"
     :key-env "MOONSHOT_API_KEY"
     :features (streaming stream-usage))
    ("openrouter" :api openai
     :url "https://openrouter.ai/api/v1"
     :key-env "OPENROUTER_API_KEY"
     :features (streaming reasoning-effort stream-usage))
    ("together_ai" :api openai
     :url "https://api.together.xyz/v1"
     :key-env "TOGETHER_API_KEY"
     :features (streaming reasoning-effort stream-usage))
    ("together" :api openai
     :url "https://api.together.xyz/v1"
     :key-env "TOGETHER_API_KEY"
     :features (streaming reasoning-effort stream-usage))
    ("deepseek" :api openai
     :url "https://api.deepseek.com/v1"
     :key-env "DEEPSEEK_API_KEY"
     :features (streaming stream-usage))
    ("openai" :api openai
     :url "https://api.openai.com/v1"
     :key-env "OPENAI_API_KEY"
     :features (streaming max-completion-tokens reasoning-effort stream-usage))
    ("anthropic" :api anthropic
     :url "https://api.anthropic.com"
     :key-env "ANTHROPIC_API_KEY"
     :features (streaming adaptive-thinking fallbacks eager-input tool-errors))
    ("rotom" :api openai
     :url "http://127.0.0.1:14550/v1"
     :features (reasoning-effort))
    ("rotom-messages" :api anthropic
     :url "http://127.0.0.1:14550"
     :features (tool-errors)))
  "Provider routing table for LiteLLM-style `<provider>/<model>' strings.")

(defun lispm-batch--non-empty-env (name)
  "Value of environment variable NAME if non-empty, else nil."
  (let ((val (and name (getenv name))))
    (and (stringp val) (not (string-empty-p val)) val)))

(defun lispm-batch--first-env (names)
  "First environment variable in NAMES that is set and non-empty."
  (seq-find #'lispm-batch--non-empty-env names))

(defun lispm-batch-resolve-backend (&rest props)
  "Resolve a backend plist from PROPS and environment variables.
PROPS may specify `:model' (including LiteLLM strings such as
\"fireworks_ai/accounts/fireworks/models/kimi-k3\" or
\"anthropic/claude-opus-4-6\"), `:provider', `:api', `:url', `:key',
`:key-env', `:max-tokens' and `:features'."
  (let* ((raw-model (or (plist-get props :model)
                        (lispm-batch--non-empty-env "LISPM_MODEL")
                        lispm-model
                        "claude-opus-4-6"))
         (provider (plist-get props :provider))
         (explicit-url (plist-get props :url))
         (rotom-url-p (and (stringp explicit-url)
                           (string-search "127.0.0.1:14550" explicit-url)))
         (slash (and (stringp raw-model) (string-search "/" raw-model)))
         (prefix (and slash (substring raw-model 0 slash)))
         (route (or (and provider (cdr (assoc (downcase (format "%s" provider))
                                              lispm-batch-provider-routes)))
                    (and rotom-url-p
                         (cdr (assoc "rotom" lispm-batch-provider-routes)))
                    (and prefix (cdr (assoc (downcase prefix)
                                            lispm-batch-provider-routes)))))
         (model (if (and prefix
                         (if rotom-url-p
                             (member (downcase prefix) '("rotom" "rotom-messages"))
                           (assoc (downcase prefix) lispm-batch-provider-routes)))
                    (substring raw-model (1+ slash))
                  raw-model))
         (api (or (plist-get props :api)
                  (when-let* ((env-api (lispm-batch--non-empty-env "LISPM_API")))
                    (intern (downcase env-api)))
                  (plist-get route :api)
                  (if (or (string-prefix-p "claude-" model)
                          (and (not (lispm-batch--non-empty-env "OPENAI_API_KEY"))
                               (not (lispm-batch--non-empty-env "OPENAI_BASE_URL"))
                               (not (lispm-batch--non-empty-env "FIREWORKS_API_KEY"))
                               (not (lispm-batch--non-empty-env "MOONSHOT_API_KEY"))
                               (not (lispm-batch--non-empty-env "OPENROUTER_API_KEY"))
                               (lispm-batch--non-empty-env "ANTHROPIC_API_KEY")))
                      'anthropic
                    'openai)))
         (url (or (plist-get props :url)
                  (lispm-batch--non-empty-env "LISPM_API_URL")
                  (if (eq api 'anthropic)
                      (lispm-batch--non-empty-env "ANTHROPIC_BASE_URL")
                    (or (lispm-batch--non-empty-env "OPENAI_BASE_URL")
                        (and (equal (plist-get route :key-env) "MOONSHOT_API_KEY")
                             (lispm-batch--non-empty-env "MOONSHOT_BASE_URL"))))
                  (plist-get route :url)
                  (if (eq api 'anthropic)
                      "https://api.anthropic.com"
                    "https://api.openai.com/v1")))
         (local-url-p (and (stringp url)
                           (string-match-p
                            "\\`https?://\\(?:127\\.0\\.0\\.1\\|localhost\\|host\\.docker\\.internal\\)\\(?::\\|/\\|\\'\\)"
                            url)))
         (key-env (or (plist-get props :key-env)
                      (and (lispm-batch--non-empty-env "LISPM_API_KEY") "LISPM_API_KEY")
                      (and (not local-url-p) (plist-get route :key-env))
                      (and (not local-url-p)
                           (if (eq api 'anthropic)
                               "ANTHROPIC_API_KEY"
                             (or (lispm-batch--first-env
                                  '("OPENAI_API_KEY" "FIREWORKS_API_KEY" "MOONSHOT_API_KEY"
                                    "OPENROUTER_API_KEY" "TOGETHER_API_KEY" "DEEPSEEK_API_KEY"))
                                 "OPENAI_API_KEY")))))
         (features (if (plist-member props :features)
                       (plist-get props :features)
                     (or (plist-get route :features)
                         (if (eq api 'anthropic)
                             '(streaming adaptive-thinking fallbacks eager-input tool-errors)
                           (if (and (stringp url) (string-search "127.0.0.1:14550" url))
                               '(reasoning-effort)
                             '(streaming stream-usage))))))
         (max-tokens (plist-get props :max-tokens))
         (key (plist-get props :key)))
    `(:api ,api
      :url ,url
      :model ,model
      ,@(cond (key `(:key ,key))
              (key-env `(:key-env ,key-env)))
      ,@(and max-tokens `(:max-tokens ,max-tokens))
      :features ,features)))

(defun lispm-batch--iso-timestamp (&optional time)
  "ISO-8601 UTC timestamp for TIME (or now)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (or time (current-time)) t))

(defun lispm-batch-call-record (index agent message)
  "Build a per-call usage alist for call INDEX of AGENT's response MESSAGE."
  (let* ((usage (alist-get 'usage message))
         (fresh (or (alist-get 'input_tokens usage) 0))
         (cached (or (alist-get 'cache_read_input_tokens usage) 0))
         (created (or (alist-get 'cache_creation_input_tokens usage) 0))
         (output (or (alist-get 'output_tokens usage) 0))
         (total-in (+ fresh cached created))
         (stop (or (alist-get 'stop_reason message) "end_turn"))
         (model (or (lispm-agent-model* agent) "")))
    `((call . ,index)
      (step . ,(lispm-agent-steps agent))
      (timestamp . ,(lispm-batch--iso-timestamp))
      (model . ,model)
      (input_tokens . ,fresh)
      (cache_read_tokens . ,cached)
      (cache_write_tokens . ,created)
      (output_tokens . ,output)
      (total_input_tokens . ,total-in)
      (stop_reason . ,stop))))

(defun lispm-batch-summarize-calls (calls)
  "Compute FrontierHarness-aligned token and cache metrics from CALLS.
CALLS is a list of alists in chronological order as built by
`lispm-batch-call-record'.  In FrontierHarness and Harbor accounting,
`input_tokens' (`n_input_tokens') is total prompt tokens (fresh +
cache_read + cache_write), while each call's `input_tokens' in
`lispm-calls.jsonl' is fresh-only."
  (let ((n (length calls))
        (fresh 0)
        (cached 0)
        (writes 0)
        (output 0)
        first-in
        first-cached)
    (dolist (call calls)
      (let ((c-fresh (or (alist-get 'input_tokens call) 0))
            (c-cached (or (alist-get 'cache_read_tokens call) 0))
            (c-write (or (alist-get 'cache_write_tokens call) 0))
            (c-out (or (alist-get 'output_tokens call) 0)))
        (unless first-in
          (setq first-in (+ c-fresh c-cached)
                first-cached c-cached))
        (cl-incf fresh c-fresh)
        (cl-incf cached c-cached)
        (cl-incf writes c-write)
        (cl-incf output c-out)))
    (let* ((total-in (+ fresh cached writes))
           (warm-cached (if first-cached (max 0 (- cached first-cached)) 0))
           (rate-all (and (> total-in 0) (/ (float cached) total-in)))
           (rate-norm (and (> total-in 0) first-cached
                           (/ (float warm-cached) total-in)))
           (rate-warm (and first-in first-cached (> total-in first-in)
                           (min 1.0 (/ (float warm-cached) (- total-in first-in))))))
      `((turns . ,n)
        (calls . ,n)
        (input_tokens . ,total-in)
        (fresh_input_tokens . ,fresh)
        (cached_input_tokens . ,cached)
        (cache_write_tokens . ,writes)
        (output_tokens . ,output)
        (first_turn_input_tokens . ,(or first-in :null))
        (first_turn_cached_tokens . ,(or first-cached :null))
        (cache_hit_rate_with_first_turn . ,(or rate-all :null))
        (cache_hit_rate_normalized . ,(or rate-norm :null))
        (cache_hit_rate . ,(or rate-warm :null))
        (agent_result . ((n_input_tokens . ,total-in)
                         (n_cache_tokens . ,cached)
                         (n_cache_write_tokens . ,writes)
                         (n_output_tokens . ,output)))))))

(defun lispm-batch--write-json (file object)
  "Write OBJECT as pretty-printed JSON to FILE, creating parent directories."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert (lispm--json-serialize object :null-object :null :false-object :false) "\n"))))

(defun lispm-batch--append-jsonl (file object)
  "Append OBJECT as a single JSON line to FILE."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (let ((coding-system-for-write 'utf-8-unix)
        (line (concat (lispm--json-serialize object :null-object :null :false-object :false) "\n")))
    (write-region line nil file 'append 'silent)))

(defvar tramp-persistency-file-name)
(defvar lispm-kernel-exec-runtime-function)
(declare-function lispm-kernel-start "lispm-kernel" (program))
(declare-function lispm-kernel-stop "lispm-kernel" (runtime))
(declare-function lispm-kernel-dispatch "lispm-kernel" (runtime))
(declare-function lispm-kernel-runtime-hello "lispm-kernel" (runtime))
(declare-function lispm-kernel-exec-mode "lispm-kernel-exec" (&optional arg))
(declare-function xwidget-event-handler "xwidget" ())
(defvar lispm-kernel-exec-mode)

(defun lispm-batch--pump-events ()
  "Dispatch any queued `xwidget-event' when running on a graphical display."
  (when (and (not noninteractive) (fboundp 'xwidget-event-handler))
    (let* ((inhibit-quit t)
           (saved unread-command-events)
           (unread-command-events nil)
           (ev (read-event nil nil 0.01)))
      (setq unread-command-events
            (if (and (consp ev) (eq (car ev) 'xwidget-event))
                (progn
                  (let ((last-input-event ev))
                    (xwidget-event-handler))
                  saved)
              (if ev (append saved (list ev)) saved))))))

(defun lispm-batch--start-kernel ()
  "Start a LispM Kernel runtime if `LISPM_KERNEL_PROGRAM' is available."
  (let ((prog (getenv "LISPM_KERNEL_PROGRAM")))
    (when (and (stringp prog) (file-executable-p prog)
               (require 'lispm-kernel-exec nil t))
      (ignore-errors
        (let ((rt (lispm-kernel-start prog))
              (deadline (+ (float-time) 2.0)))
          (while (and (not (lispm-kernel-runtime-hello rt))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)
            (lispm-kernel-dispatch rt))
          (if (lispm-kernel-runtime-hello rt)
              rt
            (ignore-errors (lispm-kernel-stop rt))
            nil))))))

(defconst lispm-batch--workspace-files-script
  "git -c safe.directory='*' ls-files -z --cached --others --exclude-standard 2>/dev/null ||
find . \\( -name .git -o -name .hg -o -name .svn -o -name CVS -o -name node_modules -o -name vendor -o -name .venv -o -name venv -o -name __pycache__ -o -name .pytest_cache -o -name .mypy_cache -o -name .ruff_cache -o -name .tox -o -name .lispm-state \\) -prune -o -type f -print0 2>/dev/null"
  "Shell snippet listing NUL-separated relative file paths in `default-directory'.")

(defun lispm-batch--workspace-files (root)
  "Return a sorted list of relative file paths under ROOT."
  (let ((default-directory (file-name-as-directory (expand-file-name root))))
    (with-temp-buffer
      (when (ignore-errors
              (process-file "sh" nil t nil "-c" lispm-batch--workspace-files-script)
              t)
        (let (out)
          (dolist (raw (split-string (buffer-string) "\0" t))
            (let ((clean (string-remove-prefix "./" raw)))
              (unless (or (string-empty-p clean)
                          (string-prefix-p ".lispm-state/" clean))
                (push clean out))))
          (sort (delete-dups out) #'string<))))))

(defun lispm-batch--journal-files (agent root)
  "Return relative paths under ROOT touched by AGENT's file tools."
  (let ((root-dir (file-name-as-directory (expand-file-name root)))
        out)
    (dolist (turn-cell (lispm-agent-journal agent))
      (dolist (entry (cdr turn-cell))
        (when-let* ((file (plist-get (cdr entry) :file))
                    (full (expand-file-name file)))
          (when (string-prefix-p root-dir full)
            (push (substring full (length root-dir)) out)))))
    (sort (delete-dups out) #'string<)))

(defun lispm-batch--scratch-file-p (rel-path)
  "Non-nil if REL-PATH looks like a scratch or temporary test file."
  (let ((case-fold-search t)
        (base (file-name-nondirectory rel-path)))
    (or (string-match-p
         "\\(?:\\`\\|[_.-]\\)\\(?:scratch\\|debug\\|tmp\\|temp\\|repro\\)\\(?:[_.-]\\|\\'\\)"
         base)
        (string-match-p
         "\\(?:\\`_?test_\\|_test\\.\\|\\.test\\.\\|\\.spec\\.\\)"
         base))))

(defun lispm-batch--deliverable-build-file-p (rel-path)
  "Non-nil if REL-PATH is an in-place compiled extension or proof artifact."
  (let ((case-fold-search t))
    (or (string-prefix-p "build/" rel-path)
        (string-match-p
         "\\.\\(?:so\\|dylib\\|dll\\|pyd\\|vo\\|vok\\|vos\\|vio\\|glob\\|aux\\|whl\\)\\'"
         rel-path))))

(defun lispm-batch--hygiene-prompt (root before-files agent)
  "Return a follow-up hygiene prompt if suspect new files remain under ROOT."
  (let ((before-set (make-hash-table :test #'equal))
        (tool-set (make-hash-table :test #'equal))
        shell-files
        tool-new-files
        scratch-tool-files)
    (dolist (f before-files)
      (puthash f t before-set))
    (dolist (f (lispm-batch--journal-files agent root))
      (puthash f t tool-set))
    (dolist (f (lispm-batch--workspace-files root))
      (unless (gethash f before-set)
        (if (gethash f tool-set)
            (progn
              (push f tool-new-files)
              (when (lispm-batch--scratch-file-p f)
                (push f scratch-tool-files)))
          (unless (lispm-batch--deliverable-build-file-p f)
            (push f shell-files)))))
    (setq shell-files (nreverse shell-files)
          tool-new-files (nreverse tool-new-files)
          scratch-tool-files (nreverse scratch-tool-files))
    (when (or shell-files scratch-tool-files)
      (concat
       "[From LispM, for the user] Workspace check before finishing — new files were created during this session:\n"
       (if shell-files
           (format "- Created by shell/build commands (outside write_file/edit_file): %s\n"
                   (string-join shell-files ", "))
         "")
       (if tool-new-files
           (format "- Created by write_file/edit_file: %s\n"
                   (string-join tool-new-files ", "))
         "")
       "Remove only temporary scratch files or one-off test binaries (such as a.out or cmain) that are NOT required task deliverables. NEVER delete required deliverables, requested output files, or in-place compiled artifacts (such as .so C extensions, .vo compiled proofs, or target binaries). Then provide your final summary."))))

(defconst lispm-batch--wrapup-prompt
  "[From LispM, for the user] URGENT: The session time or step budget has been reached. Do NOT start any more exploratory scripts or long benchmarks in /tmp. Immediately write or copy your best working implementation and outputs to the exact deliverable file paths required by the task, verify that all required output files exist and are non-empty, remove any temporary scratch files created in the workspace, and provide a brief final summary."
  "Follow-up wrap-up prompt sent when a batch turn hits its soft deadline.")

(defun lispm-batch--reserve-seconds (timeout-sec)
  "Seconds reserved before hard TIMEOUT-SEC for a finalization wrap-up pass."
  (if (and (numberp timeout-sec) (>= timeout-sec 300))
      (min 150 (max 60 (floor (* 0.06 timeout-sec))))
    0))

(defun lispm-batch--budget-note (agent started-flt soft-deadline max-steps stage-cell)
  "Return a one-time budget notice string for AGENT when thresholds are crossed.
STARTED-FLT and SOFT-DEADLINE are `float-time' values; MAX-STEPS is the
turn's step cap; STAGE-CELL is a cons cell `(STAGE . nil)' tracking
whether the 70%% (`1') or 85%% (`2') notice has already been emitted."
  (when (and (numberp started-flt) (numberp soft-deadline) (> soft-deadline started-flt))
    (let* ((now (float-time))
           (total (max 1.0 (- soft-deadline started-flt)))
           (elapsed (max 0.0 (- now started-flt)))
           (rem (max 0 (floor (- soft-deadline now))))
           (time-frac (/ elapsed total))
           (steps (or (and agent (lispm-agent-steps agent)) 0))
           (step-frac (if (and (numberp max-steps) (> max-steps 0))
                          (/ (float steps) max-steps)
                        0.0))
           (frac (max time-frac step-frac))
           (stage (or (car stage-cell) 0)))
      (cond
       ((and (>= frac 0.85) (< stage 2))
        (setcar stage-cell 2)
        (format "[LispM URGENT budget notice: %d%% of session budget used (%dm %ds remaining, step %d/%d). Stop exploratory /tmp experiments immediately, write all required deliverable files to their exact target paths now, and verify they exist on disk.]"
                (min 99 (round (* 100 frac)))
                (/ rem 60) (mod rem 60)
                steps (or max-steps 0)))
       ((and (>= frac 0.70) (< stage 1))
        (setcar stage-cell 1)
        (format "[LispM budget notice: %d%% of session budget used (%dm %ds remaining, step %d/%d). Begin converging on your final implementation and writing required deliverable files soon.]"
                (min 99 (round (* 100 frac)))
                (/ rem 60) (mod rem 60)
                steps (or max-steps 0)))))))

(cl-defun lispm-batch-run (prompt &rest options
                                  &key cwd model provider api url key key-env
                                  max-steps max-tokens effort timeout
                                  output-dir calls-file usage-file result-file
                                  transcript-file role instructions sandbox
                                  &allow-other-keys)
  "Run PROMPT headlessly in LispM and return the result alist.
OPTIONS:
  :cwd              Root directory for the agent (default `default-directory')
  :model            Model identifier or LiteLLM `<provider>/<model>' string
  :provider         Explicit provider name (`openai', `anthropic', ...)
  :api              Backend protocol symbol (`openai' or `anthropic')
  :url              Base URL of the API endpoint
  :key              Literal API key (prefer `:key-env' in production)
  :key-env          Environment variable holding the API key
  :max-steps        Maximum LLM requests in the turn (default `lispm-max-steps')
  :max-tokens       Max output tokens per response (`lispm-max-tokens')
  :effort           Reasoning effort (`low', `medium', `high', `xhigh', `max')
  :timeout          Wall-clock timeout in seconds for the turn (default 1800)
  :output-dir       Directory where `lispm-calls.jsonl', `lispm-usage.json',
                    `result.json' and `session.org' are written
  :calls-file       Explicit path for per-call JSONL telemetry
  :usage-file       Explicit path for incremental usage JSON
  :result-file      Explicit path for final result JSON
  :transcript-file  Explicit path to copy the `.org' session transcript to
  :role             Agent role symbol (default `assistant')
  :instructions     Extra instructions appended to the agent's system prompt
  :sandbox          Whether to enable `lispm-sandbox' (default nil)"
  (ignore options)
  (when (and (stringp cwd) (string-prefix-p "/" cwd) (string-search ":" cwd))
    (require 'tramp)
    (require 'tramp-container nil t)
    (setq tramp-persistency-file-name nil))
  (let* ((root (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (out-dir (and output-dir (file-name-as-directory (expand-file-name output-dir))))
         (calls-path (or (and calls-file (expand-file-name calls-file))
                         (and out-dir (expand-file-name "lispm-calls.jsonl" out-dir))))
         (usage-path (or (and usage-file (expand-file-name usage-file))
                         (and out-dir (expand-file-name "lispm-usage.json" out-dir))))
         (result-path (or (and result-file (expand-file-name result-file))
                          (and out-dir (expand-file-name "result.json" out-dir))))
         (transcript-path (or (and transcript-file (expand-file-name transcript-file))
                              (and out-dir (expand-file-name "session.org" out-dir))))
         (state-dir (if out-dir
                        (expand-file-name ".lispm-state/" out-dir)
                      (make-temp-file "lispm-batch-" t)))
         (backend-spec (lispm-batch-resolve-backend
                        :model model :provider provider :api api
                        :url url :key key :key-env key-env
                        :max-tokens max-tokens))
         (timeout-sec (or timeout 1800))
         (started-time (current-time))
         (started-iso (lispm-batch--iso-timestamp started-time))
         (call-records nil)
         (call-count 0)
         (done nil)
         (timed-out nil)
         (final-reply nil)
         (kernel-rt (lispm-batch--start-kernel)))
    (when (and calls-path (file-exists-p calls-path))
      (delete-file calls-path))
    (let ((lispm-directory state-dir)
          (lispm-agents-state-file (expand-file-name "agent-sessions.eld" state-dir))
          (create-lockfiles nil)
          (make-backup-files nil)
          (auto-save-default nil)
          (lispm-auto-approve-all t)
          (select-safe-coding-system-function nil)
          (lispm-default-policy lispm-batch-policy)
          (lispm-condition-handlers lispm-batch-condition-handlers)
          (lispm-unattended-condition-handlers lispm-batch-condition-handlers)
          (lispm-auto-title nil)
          (lispm-git nil)
          (lispm-journal-git nil)
          (lispm-compact-idle nil)
          (lispm-sandbox-level (if sandbox
                                   (if (symbolp sandbox) sandbox 'low)
                                 'none))
          (lispm-shell-timeout 180)
          (lispm-context-images
           (cond ((lispm-batch--non-empty-env "LISPM_CONTEXT_IMAGES")
                  (string-to-number (getenv "LISPM_CONTEXT_IMAGES")))
                 ((string-match-p "deepseek"
                                  (downcase (or (plist-get backend-spec :model) "")))
                  0)
                 (t lispm-context-images)))
          (lispm-max-steps (or max-steps 400))
          (lispm-max-tokens (or max-tokens lispm-max-tokens))
          (lispm-effort (or effort lispm-effort))
          (lispm-backends (cons (cons 'batch backend-spec) lispm-backends))
          (lispm-backend 'batch)
          (lispm-kernel-exec-runtime-function
           (if kernel-rt
               (lambda () kernel-rt)
             (and (boundp 'lispm-kernel-exec-runtime-function)
                  lispm-kernel-exec-runtime-function)))
          (had-exec-mode (bound-and-true-p lispm-kernel-exec-mode)))
      (let* ((agent (lispm-register-agent 'batch
                                          :role (or role lispm-default-role)
                                          :root root
                                          :backend 'batch
                                          :model (plist-get backend-spec :model)
                                          :effort (or effort lispm-effort)
                                          :policy lispm-batch-policy
                                          :instructions instructions))
             (on-response
              (lambda (_ag message)
                (cl-incf call-count)
                (let ((rec (lispm-batch-call-record call-count agent message)))
                  (push rec call-records)
                  (when calls-path
                    (lispm-batch--append-jsonl calls-path rec))
                  (when usage-path
                    (lispm-batch--write-json
                     usage-path
                     (lispm-batch-summarize-calls (reverse call-records))))))))
        (add-hook 'lispm-response-functions on-response)
        (when (and kernel-rt (fboundp 'lispm-kernel-exec-mode))
          (lispm-kernel-exec-mode 1))
        (unwind-protect
            (let* ((started-flt (float-time started-time))
                   (deadline (+ started-flt timeout-sec))
                   (reserve (lispm-batch--reserve-seconds timeout-sec))
                   (soft-deadline (- deadline reserve))
                   (budget-stage (cons 0 nil))
                   (lispm-turn-started-at started-flt)
                   (lispm-turn-deadline deadline)
                   (lispm-tool-result-note-function
                    (lambda (ag _call _text _errorp)
                      (when (eq ag agent)
                        (lispm-batch--budget-note
                         agent started-flt soft-deadline lispm-max-steps budget-stage))))
                   (initial-files (lispm-batch--workspace-files root)))
              (lispm-tell agent prompt :callback (lambda (reply)
                                                   (setq final-reply reply
                                                         done t)))
              (while (and (not done) (< (float-time) soft-deadline))
                (accept-process-output nil 0.05)
                (lispm-batch--pump-events)
                (when kernel-rt
                  (ignore-errors (lispm-kernel-dispatch kernel-rt))))
              (let ((hit-step-limit (and done (>= (lispm-agent-steps agent) lispm-max-steps)))
                    (hit-soft-deadline (not done)))
                (when (and (or hit-soft-deadline hit-step-limit)
                           (> (- deadline (float-time)) 15))
                  (when hit-soft-deadline
                    (ignore-errors (lispm-abort agent)))
                  (let ((lispm-max-steps 6)
                        (lispm-tool-result-note-function nil))
                    (setq done nil)
                    (lispm-tell agent lispm-batch--wrapup-prompt
                                :callback (lambda (reply)
                                            (when (and (stringp reply)
                                                       (not (string-empty-p (string-trim reply))))
                                              (setq final-reply reply))
                                            (setq done t)))
                    (while (and (not done) (< (float-time) deadline))
                      (accept-process-output nil 0.05)
                      (when kernel-rt
                        (ignore-errors (lispm-kernel-dispatch kernel-rt)))))))
              (when-let* (((and done (< (float-time) deadline)
                                (< call-count lispm-max-steps)))
                          (hygiene-prompt (lispm-batch--hygiene-prompt root initial-files agent)))
                (let ((lispm-max-steps 5)
                      (lispm-tool-result-note-function nil))
                  (setq done nil)
                  (lispm-tell agent hygiene-prompt
                              :callback (lambda (reply)
                                          (when (and (stringp reply)
                                                     (not (string-empty-p (string-trim reply))))
                                            (setq final-reply reply))
                                          (setq done t)))
                  (while (and (not done) (< (float-time) deadline))
                    (accept-process-output nil 0.05)
                    (lispm-batch--pump-events)
                    (when kernel-rt
                      (ignore-errors (lispm-kernel-dispatch kernel-rt))))))
              (when (not done)
                (setq timed-out t)
                (ignore-errors (lispm-abort agent))))
          (when (and kernel-rt (fboundp 'lispm-kernel-exec-mode) (not had-exec-mode))
            (lispm-kernel-exec-mode -1))
          (remove-hook 'lispm-response-functions on-response)
          (when kernel-rt
            (ignore-errors (lispm-kernel-stop kernel-rt))))
        (let* ((finished-time (current-time))
               (finished-iso (lispm-batch--iso-timestamp finished-time))
               (duration (float-time (time-subtract finished-time started-time)))
               (chron-calls (nreverse call-records))
               (summary (lispm-batch-summarize-calls chron-calls))
               (status (cond (timed-out "timeout")
                             ((and done (> call-count 0)) "completed")
                             (done "completed_no_calls")
                             (t "failed")))
               (result
                `((status . ,status)
                  (timed_out . ,(if timed-out t :false))
                  (model . ,(plist-get backend-spec :model))
                  (api . ,(symbol-name (plist-get backend-spec :api)))
                  (url . ,(plist-get backend-spec :url))
                  (cwd . ,root)
                  (started_at . ,started-iso)
                  (finished_at . ,finished-iso)
                  (duration_seconds . ,duration)
                  (reply . ,(or final-reply (lispm-agent-last-reply agent) ""))
                  (agent_execution . ((started_at . ,started-iso)
                                      (finished_at . ,finished-iso)))
                  ,@summary)))
          (when usage-path
            (lispm-batch--write-json usage-path summary))
          (when transcript-path
            (when-let* ((buf (lispm-agent-buffer agent))
                        ((buffer-live-p buf)))
              (make-directory (file-name-directory transcript-path) t)
              (with-current-buffer buf
                (write-region (point-min) (point-max) transcript-path nil 'silent))))
          (when result-path
            (lispm-batch--write-json result-path result))
          (when (fboundp 'lispm-web-close-session)
            (ignore-errors (lispm-web-close-session)))
          (unless out-dir
            (when-let* ((buf (lispm-agent-buffer agent)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (set-buffer-modified-p nil)
                  (setq buffer-file-name nil))
                (kill-buffer buf)))
            (ignore-errors (delete-directory state-dir t)))
          result)))))

(defun lispm-batch-parse-args (args)
  "Parse command-line ARGS into a plist for `lispm-batch-run'."
  (when (equal (car args) "--")
    (pop args))
  (let (opts positional)
    (while args
      (let ((arg (pop args)))
        (pcase arg
          ((or "-p" "--prompt")
           (setq opts (plist-put opts :prompt (pop args))))
          ((or "-f" "--prompt-file")
           (let ((file (pop args)))
             (setq opts (plist-put opts :prompt
                                   (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string))))))
          ((or "-C" "--cwd")
           (setq opts (plist-put opts :cwd (pop args))))
          ((or "-m" "--model")
           (setq opts (plist-put opts :model (pop args))))
          ("--provider"
           (setq opts (plist-put opts :provider (pop args))))
          ("--api"
           (setq opts (plist-put opts :api (intern (downcase (pop args))))))
          ("--url"
           (setq opts (plist-put opts :url (pop args))))
          ("--key-env"
           (setq opts (plist-put opts :key-env (pop args))))
          ("--max-steps"
           (setq opts (plist-put opts :max-steps (string-to-number (pop args)))))
          ("--max-tokens"
           (setq opts (plist-put opts :max-tokens (string-to-number (pop args)))))
          ("--effort"
           (setq opts (plist-put opts :effort (pop args))))
          ("--timeout"
           (setq opts (plist-put opts :timeout (string-to-number (pop args)))))
          ((or "-o" "--output-dir")
           (setq opts (plist-put opts :output-dir (pop args))))
          ("--calls-file"
           (setq opts (plist-put opts :calls-file (pop args))))
          ("--usage-file"
           (setq opts (plist-put opts :usage-file (pop args))))
          ("--result-file"
           (setq opts (plist-put opts :result-file (pop args))))
          ("--transcript-file"
           (setq opts (plist-put opts :transcript-file (pop args))))
          ("--role"
           (setq opts (plist-put opts :role (intern (pop args)))))
          ("--instructions"
           (setq opts (plist-put opts :instructions (pop args))))
          ("--sandbox"
           (setq opts (plist-put opts :sandbox t)))
          ("--json"
           (setq opts (plist-put opts :json t)))
          ((or "-h" "--help")
           (setq opts (plist-put opts :help t)))
          ("--"
           (setq positional (append positional args)
                 args nil))
          ((pred (string-prefix-p "-"))
           (error "Unknown option: %s" arg))
          (_
           (push arg positional)))))
    (when (and (not (plist-get opts :prompt)) positional)
      (setq opts (plist-put opts :prompt (string-join (nreverse positional) " "))))
    opts))

(defun lispm-batch-usage ()
  "Usage string for `bin/lispm-batch'."
  "Usage: lispm-batch [OPTIONS] [PROMPT...]

Run a LispM agent turn headlessly in batch Emacs and record per-call token
and prompt-cache telemetry compatible with FrontierHarness (Harbor / Pier).

Options:
  -p, --prompt TEXT          Task instruction to execute
  -f, --prompt-file FILE     Read task instruction from FILE
  -C, --cwd DIR              Agent root directory (default: current directory)
  -m, --model MODEL          Model or LiteLLM `<provider>/<model>' string
      --provider NAME        Provider route (openai, anthropic, fireworks_ai,
                             moonshot, openrouter, together_ai, deepseek)
      --api API              Protocol (`openai' or `anthropic')
      --url BASE_URL         API base URL override
      --key-env VAR          Environment variable holding the API key
      --max-steps N          Maximum model steps in the turn (default: 60)
      --max-tokens N         Maximum output tokens per response (default: 64000)
      --effort LEVEL         Reasoning effort (low, medium, high, xhigh, max)
      --timeout SECONDS      Wall-clock timeout in seconds (default: 1800)
  -o, --output-dir DIR       Write lispm-calls.jsonl, lispm-usage.json,
                             result.json and session.org to DIR
      --calls-file FILE      Write per-call JSONL telemetry to FILE
      --usage-file FILE      Write cumulative usage JSON to FILE
      --result-file FILE     Write final run result JSON to FILE
      --transcript-file FILE Copy the Org session transcript to FILE
      --json                 Print final result JSON to stdout instead of reply
  -h, --help                 Show this help message\n")

(defun lispm-batch-main ()
  "Entrypoint for `bin/lispm-batch' under `emacs -Q --batch'."
  (condition-case err
      (let* ((opts (lispm-batch-parse-args command-line-args-left))
             (prompt (plist-get opts :prompt)))
        (setq command-line-args-left nil)
        (when (plist-get opts :help)
          (princ (lispm-batch-usage))
          (kill-emacs 0))
        (unless (and (stringp prompt) (not (string-empty-p (string-trim prompt))))
          (princ (lispm-batch-usage) #'external-debugging-output)
          (error "No prompt given (pass --prompt TEXT, --prompt-file FILE, or positional text)"))
        (let ((result (apply #'lispm-batch-run prompt opts)))
          (if (plist-get opts :json)
              (princ (concat (lispm--json-serialize result :null-object :null :false-object :false) "\n"))
            (let ((reply (alist-get 'reply result)))
              (when (and (stringp reply) (not (string-empty-p reply)))
                (princ (concat reply (unless (string-suffix-p "\n" reply) "\n"))))))
          (kill-emacs (if (eq (alist-get 'timed_out result) t) 124 0))))
    (error
     (princ (format "lispm-batch: %s\n" (error-message-string err))
            #'external-debugging-output)
     (kill-emacs 1))))

(provide 'lispm-batch)
;;; lispm-batch.el ends here
