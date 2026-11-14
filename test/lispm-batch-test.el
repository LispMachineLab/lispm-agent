;;; lispm-batch-test.el --- Tests for headless batch runner and FrontierHarness telemetry  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'lispm)
(require 'lispm-batch)

(defconst lispm-batch-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `lispm-batch-test.el'.")

(ert-deftest lispm-test-batch-resolve-backend ()
  "LiteLLM `<provider>/<model>' strings and environment variables resolve cleanly."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "OPENAI_BASE_URL" nil)
    (setenv "ANTHROPIC_BASE_URL" nil)
    (setenv "LISPM_API_URL" nil)
    (let ((fw (lispm-batch-resolve-backend
               :model "fireworks_ai/accounts/fireworks/models/kimi-k3")))
      (should (eq (plist-get fw :api) 'openai))
      (should (equal (plist-get fw :url) "https://api.fireworks.ai/inference/v1"))
      (should (equal (plist-get fw :model) "accounts/fireworks/models/kimi-k3"))
      (should (equal (plist-get fw :key-env) "FIREWORKS_API_KEY")))
    (let ((ms (lispm-batch-resolve-backend :model "moonshot/kimi-k3")))
      (should (eq (plist-get ms :api) 'openai))
      (should (equal (plist-get ms :url) "https://api.moonshot.ai/v1"))
      (should (equal (plist-get ms :model) "kimi-k3"))
      (should (equal (plist-get ms :key-env) "MOONSHOT_API_KEY")))
    (let ((ant (lispm-batch-resolve-backend :model "anthropic/claude-opus-4-6")))
      (should (eq (plist-get ant :api) 'anthropic))
      (should (equal (plist-get ant :url) "https://api.anthropic.com"))
      (should (equal (plist-get ant :model) "claude-opus-4-6"))
      (should (equal (plist-get ant :key-env) "ANTHROPIC_API_KEY")))
    (setenv "OPENAI_BASE_URL" "http://127.0.0.1:19999/v1")
    (let ((custom (lispm-batch-resolve-backend :model "openai/gpt-5.4")))
      (should (eq (plist-get custom :api) 'openai))
      (should (equal (plist-get custom :url) "http://127.0.0.1:19999/v1"))
      (should (equal (plist-get custom :model) "gpt-5.4")))
    (let ((rotom-ds (lispm-batch-resolve-backend
                     :url "http://127.0.0.1:14550/v1"
                     :model "deepseek/deepseek-v4.1-flash-1017")))
      (should (eq (plist-get rotom-ds :api) 'openai))
      (should (equal (plist-get rotom-ds :url) "http://127.0.0.1:14550/v1"))
      (should (equal (plist-get rotom-ds :model) "deepseek/deepseek-v4.1-flash-1017"))
      (should (null (plist-get rotom-ds :key-env)))
      (should (equal (plist-get rotom-ds :features) '(reasoning-effort))))))

(ert-deftest lispm-test-batch-openai-cache-variants ()
  "Chat Completions usage normalizes OpenAI, Moonshot and DeepSeek cache fields."
  ;; Standard OpenAI / Fireworks prompt_tokens_details.cached_tokens
  (should (equal (lispm-openai--usage
                  '((prompt_tokens . 1000)
                    (completion_tokens . 50)
                    (prompt_tokens_details (cached_tokens . 800))))
                 '((input_tokens . 200)
                   (output_tokens . 50)
                   (cache_read_input_tokens . 800))))
  ;; Moonshot top-level cached_tokens
  (should (equal (lispm-openai--usage
                  '((prompt_tokens . 1200)
                    (completion_tokens . 80)
                    (cached_tokens . 900)))
                 '((input_tokens . 300)
                   (output_tokens . 80)
                   (cache_read_input_tokens . 900))))
  ;; DeepSeek top-level prompt_cache_hit_tokens + cache_write_tokens
  (should (equal (lispm-openai--usage
                  '((prompt_tokens . 500)
                    (completion_tokens . 40)
                    (prompt_cache_hit_tokens . 300)
                    (cache_write_tokens . 100)))
                 '((input_tokens . 200)
                   (output_tokens . 40)
                   (cache_read_input_tokens . 300)
                   (cache_creation_input_tokens . 100)))))

(ert-deftest lispm-test-batch-summarize-calls ()
  "Per-call token records produce FrontierHarness-aligned cache and token totals."
  (let* ((calls '(((call . 1)
                   (input_tokens . 800)
                   (cache_read_tokens . 200)
                   (cache_write_tokens . 0)
                   (output_tokens . 50))
                  ((call . 2)
                   (input_tokens . 200)
                   (cache_read_tokens . 1000)
                   (cache_write_tokens . 0)
                   (output_tokens . 60))
                  ((call . 3)
                   (input_tokens . 300)
                   (cache_read_tokens . 1500)
                   (cache_write_tokens . 0)
                   (output_tokens . 90))))
         (summary (lispm-batch-summarize-calls calls))
         (agent-res (alist-get 'agent_result summary)))
    ;; Total input = (800+200) + (200+1000) + (300+1500) = 4000
    ;; Total cached = 200 + 1000 + 1500 = 2700
    ;; First turn: input = 1000, cached = 200
    ;; Warm cached (excluding first turn) = 2700 - 200 = 2500
    (should (= (alist-get 'turns summary) 3))
    (should (= (alist-get 'input_tokens summary) 4000))
    (should (= (alist-get 'fresh_input_tokens summary) 1300))
    (should (= (alist-get 'cached_input_tokens summary) 2700))
    (should (= (alist-get 'output_tokens summary) 200))
    (should (= (alist-get 'first_turn_input_tokens summary) 1000))
    (should (= (alist-get 'first_turn_cached_tokens summary) 200))
    ;; Normalized cache hit rate: (2700 - 200) / 4000 = 0.625
    (should (< (abs (- (alist-get 'cache_hit_rate_normalized summary) 0.625)) 1e-6))
    ;; Warm cache hit rate: (2700 - 200) / (4000 - 1000) = 2500 / 3000 = 0.833333
    (should (< (abs (- (alist-get 'cache_hit_rate summary) (/ 2500.0 3000.0))) 1e-6))
    ;; Raw cache hit rate with first turn: 2700 / 4000 = 0.675
    (should (< (abs (- (alist-get 'cache_hit_rate_with_first_turn summary) 0.675)) 1e-6))
    (should (equal agent-res
                   '((n_input_tokens . 4000)
                     (n_cache_tokens . 2700)
                     (n_cache_write_tokens . 0)
                     (n_output_tokens . 200))))))

(ert-deftest lispm-test-batch-parse-args ()
  "Command-line arguments parse into a `lispm-batch-run' plist."
  (let ((opts (lispm-batch-parse-args
               '("--" "--model" "fireworks_ai/accounts/fireworks/models/kimi-k3"
                 "--cwd" "/testbed"
                 "--output-dir" "/logs/agent"
                 "--max-steps" "40"
                 "--timeout" "900"
                 "--json"
                 "Fix" "the" "bug"))))
    (should (equal (plist-get opts :model) "fireworks_ai/accounts/fireworks/models/kimi-k3"))
    (should (equal (plist-get opts :cwd) "/testbed"))
    (should (equal (plist-get opts :output-dir) "/logs/agent"))
    (should (= (plist-get opts :max-steps) 40))
    (should (= (plist-get opts :timeout) 900))
    (should (eq (plist-get opts :json) t))
    (should (equal (plist-get opts :prompt) "Fix the bug"))))

(ert-deftest lispm-test-batch-run-turn-and-telemetry ()
  "A headless batch run executes tools autonomously and persists telemetry files."
  (lispm-test-env
    (lispm-test--with-server
      (with-temp-file (expand-file-name "a.txt" project)
        (insert "hello\n"))
      (let* ((lispm-api-function #'lispm-api-stream)
             (url (format "http://127.0.0.1:%d/v1" port))
             (out-dir (expand-file-name "batch-out" tmp))
             (outside-file (expand-file-name "outside.txt" tmp))
             (result (lispm-batch-run
                      "Look around"
                      :cwd project
                      :api 'openai
                      :url url
                      :model "kimi-k3"
                      :key "sk-test"
                      :output-dir out-dir
                      :timeout 15)))
        ;; Verify autonomous write outside project root works when lispm-auto-approve-all is t
        (let ((lispm-auto-approve-all t)
              (agent (lispm-register-agent 'writer :root project :policy lispm-batch-policy)))
          (should (eq (lispm-tool-action
                       agent (lispm-get-tool "write_file")
                       `((path . ,outside-file) (content . "ok") (why . "test")))
                      'auto)))
        ;; Verify batch run completed and persisted all telemetry artifacts
        (should (equal (alist-get 'status result) "completed"))
        (should (eq (alist-get 'timed_out result) :false))
        (should (= (alist-get 'turns result) 2))
        ;; Mock server returns 2 streamed OpenAI calls:
        ;; Call 1: prompt_tokens=20, cached_tokens=5 (fresh=15), completion_tokens=7
        ;; Call 2: prompt_tokens=20, cached_tokens=5 (fresh=15), completion_tokens=7
        ;; Total input = 40, fresh = 30, cached = 10, output = 14
        (should (= (alist-get 'input_tokens result) 40))
        (should (= (alist-get 'fresh_input_tokens result) 30))
        (should (= (alist-get 'cached_input_tokens result) 10))
        (should (= (alist-get 'output_tokens result) 14))
        (should (= (alist-get 'first_turn_input_tokens result) 20))
        (should (= (alist-get 'first_turn_cached_tokens result) 5))
        (should (file-exists-p (expand-file-name "lispm-calls.jsonl" out-dir)))
        (should (file-exists-p (expand-file-name "lispm-usage.json" out-dir)))
        (should (file-exists-p (expand-file-name "result.json" out-dir)))
        (should (file-exists-p (expand-file-name "session.org" out-dir)))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "lispm-calls.jsonl" out-dir))
          (let ((lines (split-string (string-trim (buffer-string)) "\n" t)))
            (should (= (length lines) 2))
            (let ((first-call (lispm--json-decode (car lines))))
              (should (= (alist-get 'call first-call) 1))
              (should (= (alist-get 'input_tokens first-call) 15))
              (should (= (alist-get 'cache_read_tokens first-call) 5))
              (should (= (alist-get 'output_tokens first-call) 7)))))
        ;; Verify Python FrontierHarness scorer parses the output directory cleanly
        (when-let* ((python (executable-find "python3"))
                    (scorer (expand-file-name
                             "../eval/frontier-harness/score_trials.py"
                             lispm-batch-test--dir))
                    ((file-exists-p scorer)))
          (with-temp-buffer
            (should (zerop (call-process python nil t nil scorer out-dir)))
            (let* ((scored (lispm--json-decode (buffer-string)))
                   (trials (alist-get 'trials scored))
                   (trial (and (vectorp trials) (> (length trials) 0) (aref trials 0))))
              (should (= (alist-get 'total_trials scored) 1))
              (should (= (alist-get 'passed_trials scored) 1))
              (should (= (alist-get 'turns trial) 2))
              (should (= (alist-get 'input_tokens trial) 40))
              (should (= (alist-get 'cached_input_tokens trial) 10))
              (should (= (alist-get 'output_tokens trial) 14)))))))))

(ert-deftest lispm-test-batch-no-coding-prompt-and-empty-glob-search ()
  "Saving raw bytes / UTF-8 on ASCII buffers never prompts, and empty glob search succeeds."
  (lispm-test-env
    ;; 1. Empty directory search with glob should return nil instead of signaling wrong-type-argument
    (let ((empty-dir (expand-file-name "empty-sub" project)))
      (make-directory empty-dir t)
      (cl-letf (((symbol-function 'lispm-tools--remote-search-files)
                 (lambda (&rest _) nil))
                ((symbol-function 'file-remote-p)
                 (lambda (&rest _) "/docker:test:")))
        (should (null (lispm-tools--search-files empty-dir "*" "needle" nil)))))
    ;; 2. replace_text on a us-ascii-unix buffer with UTF-8 + raw 8-bit bytes must not prompt
    (let ((target (expand-file-name "mixed.txt" project))
          (prompted nil))
      (with-temp-file target
        (insert "old\n"))
      (let ((buf (find-file-noselect target)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq buffer-file-coding-system 'us-ascii-unix))
              (cl-letf (((symbol-function 'select-safe-coding-system)
                         (lambda (&rest _)
                           (setq prompted t)
                           'raw-text)))
                (let ((rep (concat "中文-" (string (unibyte-char-to-multibyte #xff)) "\n")))
                  (lispm-tools--replace-text buf rep)))
              (should-not prompted))
          (when (buffer-live-p buf)
            (kill-buffer buf))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally target)
        (should (equal (buffer-string)
                       (encode-coding-string
                        (concat "中文-" (string (unibyte-char-to-multibyte #xff)) "\n")
                        'utf-8-unix)))))
    ;; 3. Saving a session buffer containing raw 8-bit bytes must not prompt
    (let* ((agent (lispm-register-agent 'tester :root project))
           (sbuf (lispm-session-buffer agent))
           (prompted nil))
      (with-current-buffer sbuf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "raw byte: " (unibyte-char-to-multibyte #xff) "\n")))
      (cl-letf (((symbol-function 'select-safe-coding-system)
                 (lambda (&rest _)
                   (setq prompted t)
                   'raw-text)))
        (lispm--save-session agent))
      (should-not prompted))
    ;; 4. JSON serialization of tool results / API payloads containing raw 8-bit bytes must not fail
    (let* ((raw-str (concat "P5\n958 78\n255\n"
                            (string (unibyte-char-to-multibyte #x96)
                                    (unibyte-char-to-multibyte #xff)
                                    #xd800)))
           (encoded (lispm--json-encode `((role . "user") (content . ,raw-str)))))
      (should (stringp encoded))
      (should (string-search "P5\\n958 78\\n255\\n" encoded)))))

(ert-deftest lispm-test-batch-workspace-hygiene-gate ()
  "Pre-exit workspace check stays silent for clean deliverables and catches shell/scratch files."
  (lispm-test-env
    (let* ((agent (lispm-register-agent 'hygiene :root project))
           (lispm-current-agent agent)
           (initial (lispm-batch--workspace-files project))
           (sub (expand-file-name "polyglot" project))
           (src (expand-file-name "polyglot/main.py.c" project))
           (bin (expand-file-name "polyglot/cmain" project)))
      (make-directory sub t)
      ;; 1. Deliverable created via write_file (recorded in journal) -> no hygiene prompt
      (let ((buf (find-file-noselect src)))
        (unwind-protect
            (lispm-tools--replace-text buf "int main() { return 0; }\n")
          (when (buffer-live-p buf)
            (kill-buffer buf))))
      (should (equal (lispm-batch--journal-files agent project) '("polyglot/main.py.c")))
      (should (null (lispm-batch--hygiene-prompt project initial agent)))
      ;; 2. Stray binary created outside write_file (e.g. gcc -o cmain) -> triggers hygiene prompt
      (with-temp-file bin
        (insert "\x7fELF"))
      (let ((prompt (lispm-batch--hygiene-prompt project initial agent)))
        (should (stringp prompt))
        (should (string-search "polyglot/cmain" prompt))
        (should (string-search "polyglot/main.py.c" prompt)))
      ;; 3. Removing the stray binary restores clean state -> nil again
      (delete-file bin)
      (should (null (lispm-batch--hygiene-prompt project initial agent)))
      ;; 4. Scratch test file created via write_file -> triggers hygiene prompt
      (let* ((scratch (expand-file-name "polyglot/scratch_debug.py" project))
             (buf (find-file-noselect scratch)))
        (unwind-protect
            (lispm-tools--replace-text buf "print(1)\n")
          (when (buffer-live-p buf)
            (kill-buffer buf)))
        (let ((prompt (lispm-batch--hygiene-prompt project initial agent)))
          (should (stringp prompt))
          (should (string-search "polyglot/scratch_debug.py" prompt)))))))

(ert-deftest lispm-test-batch-multiple-run-shell-in-one-turn ()
  "Multiple `run_shell' calls batched in a single turn execute sequentially without colliding."
  (lispm-test-env
    (let* ((default-directory project)
           (agent (lispm-start)))
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell"
                                                   '((why . "first") (command . "printf 'one\\n'")))
                                 (lispm-test--tool "t2" "run_shell"
                                                   '((why . "second") (command . "printf 'two\\n'"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Both done.")))))
      (lispm-tell agent "run two shells")
      (should (lispm-test--wait (lispm-test--idle-p agent) 5))
      (let ((transcript (lispm-test--transcript agent)))
        (should (= (lispm-test--count "^\\*\\* DONE run_shell" transcript) 2))
        (should (string-search "one\n\n[exit status 0]" transcript))
        (should (string-search "two\n\n[exit status 0]" transcript))))))

(ert-deftest lispm-test-batch-deadline-and-budget-notices ()
  "Shell timeout clamping and one-time budget notices preserve cache-safe tool output."
  (lispm-test-env
    ;; 1. Reserve seconds calculation
    (should (= (lispm-batch--reserve-seconds 120) 0))
    (should (= (lispm-batch--reserve-seconds 600) 60))
    (should (= (lispm-batch--reserve-seconds 3600) 150))
    ;; 2. Effective shell timeout clamping against turn deadline
    (let ((lispm-turn-deadline nil)
          (lispm-turn-started-at nil))
      (should (= (lispm-tools--effective-shell-timeout 3000) 3000)))
    (let* ((now (float-time))
           (lispm-turn-started-at (- now 3200.0))
           (lispm-turn-deadline (+ now 400.0)))
      ;; Total turn = 3600s -> reserve = 90s -> remaining budget ~310s (< 3000s requested)
      (let ((clamped (lispm-tools--effective-shell-timeout 3000)))
        (should (<= clamped 315))
        (should (>= clamped 300))))
    ;; 3. One-time 70% and 85% budget notices and tool result note hook
    (let* ((default-directory project)
           (agent (lispm-start))
           (now (float-time))
           (stage (cons 0 nil)))
      ;; Below 70% -> nil
      (setf (lispm-agent-steps agent) 10)
      (should (null (lispm-batch--budget-note agent (- now 500.0) (+ now 500.0) 100 stage)))
      ;; Cross 70% via steps (75/100) -> emits stage 1 once
      (setf (lispm-agent-steps agent) 75)
      (let ((n1 (lispm-batch--budget-note agent (- now 500.0) (+ now 500.0) 100 stage)))
        (should (stringp n1))
        (should (string-search "LispM budget notice: 75%" n1))
        (should (= (car stage) 1)))
      ;; Subsequent call at 76% -> nil (not repeated)
      (setf (lispm-agent-steps agent) 76)
      (should (null (lispm-batch--budget-note agent (- now 500.0) (+ now 500.0) 100 stage)))
      ;; Cross 85% via steps (88/100) -> emits stage 2 once
      (setf (lispm-agent-steps agent) 88)
      (let ((n2 (lispm-batch--budget-note agent (- now 500.0) (+ now 500.0) 100 stage)))
        (should (stringp n2))
        (should (string-search "LispM URGENT budget notice: 88%" n2))
        (should (= (car stage) 2)))
      ;; Subsequent call at 92% -> nil (not repeated)
      (setf (lispm-agent-steps agent) 92)
      (should (null (lispm-batch--budget-note agent (- now 500.0) (+ now 500.0) 100 stage)))
      ;; Verify `lispm-tool-result-note-function' appends to tool result in transcript
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (let ((lispm-tool-result-note-function
             (lambda (_ag _call _text _err)
               "[LispM budget notice: test marker]")))
        (setq lispm-test--scripts
              `((assistant@project
                 ,(lispm-test--msg "tool_use"
                                   (lispm-test--tool "t1" "run_shell"
                                                     '((why . "echo") (command . "printf 'hi\\n'"))))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
        (lispm-tell agent "test note")
        (should (lispm-test--wait (lispm-test--idle-p agent) 5))
        (should (string-search "[LispM budget notice: test marker]"
                               (lispm-test--transcript agent)))))))

(provide 'lispm-batch-test)
;;; lispm-batch-test.el ends here
