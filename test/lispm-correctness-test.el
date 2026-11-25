;;; lispm-correctness-test.el --- Correctness regression tests (#188)  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'lispm-test)
(require 'lispm-backend-form)
(require 'lispm-agents-mcp)
(require 'lispm-agents-bridge)
(require 'lispm-shell-stage)
(require 'lispm-batch)
(require 'lispm-bar)
(require 'lispm-mcp)

(declare-function lispm-comfyui-open "lispm-comfyui" (name))

(ert-deftest lispm-correctness-test-json-false-and-null ()
  "JSON false and null stay distinct across agent bridge and MCP helpers."
  (let ((parsed (lispm-agents-context-parse "{\"jsonrpc\":\"2.0\",\"flag\":false}")))
    (should (eq (alist-get 'flag parsed) :false)))
  (let* ((lispm-agents-bridge--agent nil)
         (seen nil)
         (server (lispm-agents-mcp-start
                  "lispm-corr-mcp" #'lispm-agents-codex-mcp--make temporary-file-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'lispm-agents-mcp-served-tools)
                   (lambda (_srv)
                     `(("probe"
                        ,(lambda (_srv args _id)
                           (setq seen args)
                           '("ok"))
                        nil nil "Probe")))))
          (let ((reply (lispm-agents-mcp--call server "probe" "{\"flag\":false}")))
            (should (equal (car reply) "200 OK"))
            (should (eq (alist-get 'flag seen) :false))))
      (lispm-agents-context-stop server)))
  (let ((input (lispm-agents-bridge--input '((input . "{\"replace_all\":false}")))))
    (should (eq (alist-get 'replace_all input) :false)))
  (let ((decoded (lispm-mcp--json-in '(:ok :json-false :empty :json-null))))
    (should (eq (alist-get 'ok decoded) :false))
    (should (eq (alist-get 'empty decoded) :null))))

(ert-deftest lispm-correctness-test-backend-and-mcp-forms ()
  "Backend form preserves typed key/custom names and MCP env handles quotes."
  (lispm-test--with-settings
    ;; Typed key left blank preserves the previous string key.
    (let ((updated (lispm-backend-form--with-key
                    '(:api openai :url "https://api.example/v1" :key "sk-old")
                    'test 'typed "" nil)))
      (should (equal (plist-get updated :key) "sk-old")))
    ;; Custom provider rejects colliding with an existing backend.
    (let ((lispm-backend-form-show-function (lambda (buf) buf)))
      (with-current-buffer (lispm-model-setup-form)
        (unwind-protect
            (progn
              (lispm-ui-form-set 'provider 'custom)
              (lispm-model-setup-form--select-provider 'custom)
              (lispm-ui-form-set 'name "test" 'url "https://dup.example/v1" 'model "m")
              (should (string-match-p "already a backend" (or (lispm-test--form-problem) "")))
              (should-error (lispm-ui-form-submit) :type 'user-error))
          (kill-buffer (current-buffer)))))
    ;; Quoted env values with spaces, commas and escaped quotes round-trip.
    (let* ((env (lispm-mcp-form--parse-env
                 "PLAIN=abc MSG=\"hello, world\" ESC=\"a\\\"b\" SINGLE='x y'"))
           (text (lispm-mcp-form--env-text env)))
      (should (equal (plist-get env :PLAIN) "abc"))
      (should (equal (plist-get env :MSG) "hello, world"))
      (should (equal (plist-get env :ESC) "a\"b"))
      (should (equal (plist-get env :SINGLE) "x y"))
      (should (equal (lispm-mcp-form--parse-env text) env)))
    (should-error (lispm-mcp-form--parse-env "MSG=\"unterminated") :type 'user-error)))

(ert-deftest lispm-correctness-test-api-streaming-and-tools ()
  "Last status marker wins, streams concatenate in order, and truncated forms fail."
  ;; Model output containing the status marker must not forge HTTP status.
  (let ((body nil)
        (shape (list :error (lambda (_) nil)
                     :started (lambda (_) t)
                     :complete (lambda (_) t)
                     :result (lambda (_) '((ok . t))))))
    (lispm-api--finish
     shape nil nil
     (concat "data: " lispm-api--status-marker "500\n"
             lispm-api--status-marker "200\n")
     (lambda (msg) (setq body msg))
     (lambda (err) (setq body (list :err err))))
    (should (eq (alist-get 'ok body) t)))
  ;; OpenAI stream chunk lists assemble cleanly.
  (let ((oai (lispm-openai-stream-create))
        deltas)
    (lispm-openai-feed
     oai
     (concat "data: {\"choices\":[{\"delta\":{\"reasoning\":\"th\",\"content\":\"hi \"}}]}\n\n"
             "data: {\"choices\":[{\"delta\":{\"content\":\"there\"},\"finish_reason\":\"stop\"}]}\n\n"
             "data: [DONE]\n\n")
     (lambda (k v) (push (cons k v) deltas)))
    (let ((msg (lispm-openai-result oai)))
      (should (equal (lispm-thought-block-text (aref (alist-get 'content msg) 0)) "th"))
      (should (eq (alist-get 'ephemeral (aref (alist-get 'content msg) 0)) t))
      (should (equal (alist-get 'text (aref (alist-get 'content msg) 1)) "hi there"))
      (should deltas)))
  ;; Anthropic stream chunk lists assemble cleanly across blocks.
  (let ((ant (lispm-anthropic-stream-create)))
    (lispm-anthropic-feed
     ant
     (concat "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"role\":\"assistant\",\"content\":[]}}\n\n"
             "event: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
             "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"A\"}}\n\n"
             "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"B\"}}\n\n"
             "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
             "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"))
    (let ((msg (lispm-anthropic-result ant)))
      (should (equal (alist-get 'text (aref (alist-get 'content msg) 0)) "AB"))))
  ;; `lispm-tools-read-forms' allows comments/whitespace, rejects truncated forms.
  (should (equal (lispm-tools-read-forms "(+ 1 2) ; trailing comment\n ") '((+ 1 2))))
  (should-error (lispm-tools-read-forms "(+ 1 2) (unterminated") :type 'end-of-file))

(ert-deftest lispm-correctness-test-runtime-and-core ()
  "`lispm-request-body' leaves `continuing' intact and role updates apply safely."
  (lispm-test-env
    (let ((agent (lispm-defagent checker :root project)))
      (with-current-buffer (lispm-session-buffer agent)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "* User\nhi\n* Assistant\n** Reply\nPartial.\n")))
      (setf (lispm-agent-continuing agent) t)
      (should (lispm-request-body agent))
      (should (lispm-agent-continuing agent)))
    ;; Re-registering a character updates live agent's role and ignores missing roles.
    (lispm-register-role 'sec-reviewer :description "Reviews security." :policy '((write . deny)))
    (let* ((char (lispm-create-character "Iris" :description "Helper." :role 'assistant))
           (agent (lispm-character-agent char)))
      (should (eq (lispm-agent-role agent) 'assistant))
      (lispm-register-character 'iris :description "Helper." :role 'sec-reviewer)
      (should (eq (lispm-agent-role agent) 'sec-reviewer))
      (lispm-register-character 'iris :description "Helper." :role 'nonexistent-role)
      (should (eq (lispm-agent-role agent) 'sec-reviewer)))))

(ert-deftest lispm-correctness-test-rag-comfyui-and-memory ()
  "Character RAG splits system/request context, escapes lorebook, and memory retries."
  (lispm-test-env
    (let* ((ext-doc (expand-file-name "guide.md" project))
           (_ (lispm-test--write ext-doc "# Notes\n\nPhoton emitter calibration guide.\n"))
           (char (lispm-create-character "Lyra" :description "Lab assistant."
                                         :workflow "hero.json" :knowledge ext-doc))
           (agent (lispm-character-agent char)))
      ;; System prompt holds static summary; request context fetches per-turn RAG hits.
      (with-current-buffer (lispm-session-buffer agent)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "* User\nphoton calibration\n")))
      (let ((sys (lispm-system-prompt agent))
            (ctx (lispm-characters--rag-context agent)))
        (should (string-match-p "# Your knowledge base (RAG)" sys))
        (should-not (string-match-p "calibration guide" sys))
        (should (string-match-p "calibration guide" (or ctx ""))))
      ;; `--character-backend' resolves active backend even when `lispm-backend' is nil.
      (let ((lispm-backend nil)
            (lispm-backends (cons '(keyed :api openai :url "https://k.example/v1" :key "sk-k")
                                  lispm-backends)))
        (should (equal (plist-get (lispm-characters--character-backend 'lyra) :url)
                       "https://k.example/v1")))
      ;; `lispm-character-rag-add' escapes leading `*' and `#+'.
      (let ((file (lispm-character-rag-add 'lyra "Escaping Test" "* Fake heading\n#+title: hijack")))
        (should (string-match-p "^,\\* Fake heading" (lispm-file-text file)))
        (let ((hits (lispm-character-rag-search 'lyra "hijack" 2)))
          (should hits)
          (should (string-search "* Fake heading" (plist-get (car hits) :text)))))
      ;; Explicit `workflow' takes precedence over an unrelated open canvas buffer.
      (let* ((other (generate-new-buffer " *other-canvas*"))
             (hero (generate-new-buffer " *hero-canvas*"))
             (opened-target nil)
             (run-buf nil))
        (unwind-protect
            (progn
              (with-current-buffer other (setq buffer-file-name (expand-file-name "other.json" project)))
              (with-current-buffer hero (setq buffer-file-name (expand-file-name "hero.json" project)))
              (cl-letf (((symbol-function 'require) (lambda (_f &rest _) t))
                        ((symbol-function 'lispm-comfyui--canvas-buffers) (lambda () (list other)))
                        ((symbol-function 'lispm-comfyui-open)
                         (lambda (wf) (setq opened-target wf) hero))
                        ((symbol-function 'lispm-characters--comfyui-set-prompts) #'ignore)
                        ((symbol-function 'lispm-comfyui-run)
                         (lambda (cb) (setq run-buf (current-buffer))
                           (funcall cb (list :status 'done :files (list ext-doc))))))
                (let (res)
                  (lispm-characters--comfyui-generate
                   "prompt" :workflow "hero.json" :callback (lambda (files _err) (setq res files)))
                  (should (equal opened-target "hero.json"))
                  (should (eq run-buf hero))
                  (should (equal res (list ext-doc))))))
          (kill-buffer other)
          (kill-buffer hero))))
    ;; Memory and tasks operations work when their buffer is narrowed.
    (let* ((lispm-memory-max-entries 1)
           (mem-file (expand-file-name "mem.org" project))
           (agent (lispm-defagent membot :root project))
           (mem-buf (lispm-memory-buffer agent 'project t t)))
      (lispm-memory-remember agent "First" "fact one")
      (lispm-memory-remember agent "Second" "fact two")
      (let ((tb (lispm-tasks--buffer agent t))
            (tid (lispm-tasks-add agent "Task A")))
        (with-current-buffer mem-buf
          (narrow-to-region (point-min) (point-min)))
        (lispm-memory-remember agent "Third" "fact three")
        (lispm-memory-forget agent "First")
        (with-current-buffer tb
          (narrow-to-region (point-min) (point-min)))
        (lispm-tasks-update agent tid "DONE" "finished")
        (should (seq-some (lambda (e) (equal (plist-get e :title) "Third"))
                          (lispm-memory-entries mem-buf)))
        (ignore mem-file)))))

(ert-deftest lispm-correctness-test-ui-lifecycle-title-and-batch ()
  "Chat preserves undo and match data, bar short-circuits, Hyperbole/bridge/title/batch work."
  ;; Hyperbole integration loads cleanly without Hyperbole installed.
  (unless (featurep 'hyperbole)
    (should (require 'lispm-hyperbole)))
  ;; `lispm-chat--shift-undo' keeps user input undo entries across output growth.
  (with-temp-buffer
    (insert "Output\n> ")
    (let ((undo '((8 . 9) nil "a")))
      (should (equal (lispm-chat--shift-undo undo 0 8) undo))
      (should (equal (lispm-chat--shift-undo '((10 . 11) nil ("x" . -10) 1 nil) 5 10)
                     '((15 . 16) nil ("x" . -15) nil)))))
  ;; `lispm-shell-stage--on-log-change' and `lispm-chat--update-footer' preserve `match-data'.
  (let ((lispm-shell-stage--sessions (make-hash-table :test #'eq)))
    (string-match "\\(hello\\)" "hello")
    (with-temp-buffer
      (lispm-shell-stage--on-log-change 1 1 0)
      (lispm-chat--update-footer))
    (should (equal (match-string 1 "hello") "hello")))
  ;; `lispm-bar-redraw' avoids touching buffer when text is unchanged.
  (lispm-test-env
    (let ((lispm-bar-open t)
          (lispm-bar--text "status line"))
      (lispm-defagent sys :root project)
      (cl-letf (((symbol-function 'lispm-system-agent) (lambda () (car (lispm-agents))))
                ((symbol-function 'lispm-bar--text) (lambda () "status line\n"))
                ((symbol-function 'lispm-bar-show-function) #'ignore)
                (lispm-bar-show-function #'ignore))
        (unwind-protect
            (progn
              (lispm-bar-redraw)
              (let* ((buf (lispm-bar-buffer))
                     (tick (with-current-buffer buf (buffer-chars-modified-tick))))
                (lispm-bar-redraw)
                (should (= tick (with-current-buffer buf (buffer-chars-modified-tick))))))
          (when-let* ((b (get-buffer "*lispm-bar*"))) (kill-buffer b))))))
  ;; Stopped `lispm-agents-mcp' is reported non-live and settles pending answers.
  (let ((server (lispm-agents-mcp-start
                 "lispm-corr-stop" #'lispm-agents-codex-mcp--make temporary-file-directory))
        (settled nil))
    (should (lispm-agents-bridge-server-live-p server))
    (lispm-agents-bridge-server-answer server (lambda (txt errp) (setq settled (cons errp txt))))
    (lispm-agents-context-stop server)
    (should-not (lispm-agents-bridge-server-live-p server))
    (should (car settled)))
  ;; Error messages for `lispm-create-skill' and `lispm-settings-mcp--check*'.
  (should-error (lispm-settings-mcp--check-name "bad name") :type 'user-error)
  (let ((err-msg (condition-case err (lispm-settings-mcp--check '(:url "ftp://x")) (error (cadr err)))))
    (should (string-search "MCP server" err-msg))
    (should-not (string-search "backend" (downcase err-msg))))
  ;; `lispm-title--ask' does not overwrite an on-disk title after buffer is killed.
  (lispm-test-env
    (let* ((agent (lispm-defagent titler :root project))
           (buf (lispm-session-buffer agent))
           (file (buffer-file-name buf))
           (on-done nil))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "* User\nhello\n"))
        (save-buffer))
      (cl-letf (((symbol-function 'lispm-api-function) #'ignore)
                (lispm-api-function
                 (lambda (_b _request &rest plist)
                   (setq on-done (plist-get plist :on-done)))))
        (lispm-title--ask agent "hi"))
      ;; User sets title on disk and closes buffer before async title reply returns.
      (lispm-rename-session file "User Given Title")
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (funcall on-done '((content . [((type . "text") . ((text . "Auto Title")))])))
      (should (equal (lispm-title--file-title file) "User Given Title"))))
  ;; `lispm-batch--run-turns' passes non-nil `pump-p' during wrap-up turn.
  (let (calls)
    (cl-letf (((symbol-function 'lispm-batch--workspace-files) (lambda (_) nil))
              ((symbol-function 'lispm-batch--hygiene-prompt) (lambda (&rest _) nil))
              ((symbol-function 'lispm-abort) #'ignore)
              ((symbol-function 'lispm-batch--turn-runner)
               (lambda (_ag _rt _cb)
                 (lambda (prompt _deadline &optional budget _note pump-p _keep)
                   (push (list prompt budget pump-p) calls)
                   (not (= (length calls) 1))))))
      (let* ((tmp (make-temp-file "lispm-batch-wrap-" t)))
        (unwind-protect
            (let ((agent (lispm-agent--create :name 'b :id "1" :root tmp)))
              (lispm-batch--run-turns agent "do work" tmp 60 (current-time) nil (lambda () 0))
              (let ((wrap (car calls)))
                (should (= (nth 1 wrap) 6))
                (should (nth 2 wrap))))
          (delete-directory tmp t))))))

(provide 'lispm-correctness-test)
;;; lispm-correctness-test.el ends here
