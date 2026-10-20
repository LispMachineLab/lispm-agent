;;; lispm-test.el --- Tests for lispm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run with `make test'.  The model is replaced by a scripted fake
;; (`lispm-test--fake-api'), except in `lispm-test-curl-stream', which
;; drives the real curl transport against test/fake_messages_server.py.

;;; Code:

(require 'ert)
(require 'lispm)

;; Git in any test, not only in `lispm-test-env', acts on the repository
;; of its directory, even when the suite runs under a git hook.
(setq process-environment (lispm--git-environment))

(defvar lispm-test--dir (file-name-directory (or load-file-name buffer-file-name)))

(defvar lispm-test--scripts nil
  "Alist from agent name to the responses its requests get, in order.
A response is a message alist, a function of the request body
returning one, (error . PLIST), a failure with the :status, :type and
:message of PLIST, or `hang' (never answer).")

(defvar lispm-test--requests nil
  "Request bodies the fake API received, newest first.")

;;;; Fixtures

(defun lispm-test--agent-name (body)
  "Name of the agent whose request BODY this is."
  (let ((system (alist-get 'system body)))
    (and (stringp system)
         (string-match "You are \"\\([^\"]+\\)\"" system)
         (intern (match-string 1 system)))))

(defun lispm-test--fake-api (_backend body _betas &rest callbacks)
  "Answer BODY from `lispm-test--scripts' through CALLBACKS."
  (push body lispm-test--requests)
  (let* ((script (assq (lispm-test--agent-name body) lispm-test--scripts))
         (step (pop (cdr script))))
    (unless (eq step 'hang)
      (run-at-time 0.01 nil
                   (lambda ()
                     (cond
                      ((null step)
                       (funcall (plist-get callbacks :on-error)
                                '(:status 400 :type "test_error" :message "script exhausted")))
                      ((eq (car-safe step) 'error)
                       (funcall (plist-get callbacks :on-error) (cdr step)))
                      (t
                       (when-let* ((on-delta (plist-get callbacks :on-delta)))
                         (funcall on-delta 'text "streaming..."))
                       (funcall (plist-get callbacks :on-done)
                                (if (functionp step) (funcall step body) step)))))))
    nil))

(defun lispm-test--msg (stop &rest blocks)
  "An assistant message with BLOCKS that stopped for STOP."
  `((role . "assistant") (content . ,(vconcat blocks)) (stop_reason . ,stop)
    (usage (input_tokens . 10) (output_tokens . 5))))

(defun lispm-test--text (text) `((type . "text") (text . ,text)))
(defun lispm-test--thinking (text signature)
  `((type . "thinking") (thinking . ,text) (signature . ,signature)))
(defun lispm-test--tool (id name input)
  `((type . "tool_use") (id . ,id) (name . ,name) (input . ,input)))

(defmacro lispm-test-env (&rest body)
  "Run BODY with a scratch `lispm-directory', no agents and the fake model.
BODY can use `project', a scratch directory for agents to work in."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "lispm-test-" t)))
          (lispm-directory (expand-file-name "home/" tmp))
          (project (expand-file-name "project/" tmp))
          (lispm-git nil)
          ;; Titles are asked for in tests of their own.
          (lispm-auto-title nil)
          ;; Scripted responses are matched against Messages-shape requests.
          (lispm-backends (cons '(test :api anthropic :url "http://fake.invalid" :model "test-model")
                                lispm-backends))
          (lispm-backend 'test)
          (lispm--agents nil)
          (lispm--roles (copy-sequence lispm--roles))
          (lispm-api-function #'lispm-test--fake-api)
          (lispm-test--scripts nil)
          (lispm-test--requests nil)
          ;; Run from git (a hook, rebase -x), the tests' git commands
          ;; would act on that repository rather than on their own.
          (process-environment (lispm--git-environment)))
     (make-directory project t)
     (unwind-protect
         (progn ,@body)
       (dolist (a (lispm-agents)) (lispm-unwatch a))
       ;; Snapshots still being taken would outlive their project.
       (dolist (p (process-list))
         (when (string-prefix-p "lispm-journal" (process-name p))
           (delete-process p)))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (file-in-directory-p (buffer-file-name b) tmp))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory tmp t))))

(defun lispm-test--wait (predicate &optional seconds)
  "Run the event loop until PREDICATE holds or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defun lispm-test--idle-p (agent)
  "Non-nil when AGENT is idle with nothing queued."
  (lambda () (and (eq (lispm-agent-state agent) 'idle) (null (lispm-agent-inbox agent)))))

(defun lispm-test--call (agent tool input)
  "Run TOOL on INPUT as AGENT; return (TEXT . ERRORP)."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke (lispm-get-tool tool) input
                       (lambda (text errorp) (setq result (cons text errorp))))
    result))

(defun lispm-test--file (project name)
  "Contents of the file NAME in PROJECT."
  (lispm--file-text (expand-file-name name project)))

(defun lispm-test--transcript (agent)
  "AGENT's session buffer text."
  (with-current-buffer (lispm-agent-buffer agent)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; Transcript

(ert-deftest lispm-test-transcript-roundtrip ()
  "Model output survives rendering and parsing byte for byte."
  (let* ((tricky "* not a heading\n,* comma\n#+begin_src x\n#+end_src\nλ→∀\n\n")
         (content (list (lispm-test--thinking "" "sig-1")
                        (lispm-test--text tricky)
                        (lispm-test--thinking "Checking the file" "sig-2")
                        (lispm-test--tool "toolu_1" "read_file"
                                          '((path . "a.txt") (flag . :false) (n . 3)
                                            (list . ["x" "y"]) (obj) (none . :null)))
                        (lispm-test--tool "call-xyz-0" "list_buffers" nil)
                        '((type . "fallback") (from (model . "a")) (to (model . "b"))))))
    (with-temp-buffer
      (insert (lispm-transcript-header "t" '(("agent" . "t")))
              (lispm-transcript-section "System" "be brief\n* really")
              (lispm-transcript-section "User" "hello")
              "* Assistant\n")
      (org-mode)
      (goto-char (point-max))
      (let ((calls (lispm-transcript-insert-blocks content (lambda (_) "DONE"))))
        (should (equal (mapcar #'car calls) '("toolu_1" "call-xyz-0")))
        (pcase-dolist (`(,id . ,marker) calls)
          (goto-char (lispm-transcript-subtree-end marker 2))
          (insert (lispm-transcript-result (format "output of %s\n* starred\n" id)
                                           (equal id "call-xyz-0")))))
      (goto-char (point-max))
      (insert "* Notes\nmy own notes, not sent\n"
              "* User :ignore:\nnot sent either\n"
              "* User\nbye\n")
      (let* ((parsed (lispm-transcript-parse))
             (messages (plist-get parsed :messages)))
        (should (equal (plist-get parsed :system) "be brief\n* really"))
        (should (equal (plist-get parsed :last-role) "user"))
        (should (= (length messages) 3))
        (should (equal (alist-get 'content (nth 1 messages)) (vconcat content)))
        (should (equal (alist-get 'content (nth 2 messages))
                       (vector '((type . "tool_result") (tool_use_id . "toolu_1")
                                 (content . "output of toolu_1\n* starred\n"))
                               '((type . "tool_result") (tool_use_id . "call-xyz-0")
                                 (content . "output of call-xyz-0\n* starred\n")
                                 (is_error . t))
                               '((type . "text") (text . "bye")))))))))

(ert-deftest lispm-test-transcript-pending-and-prefixes ()
  "Calls without results are pending; messages from agents and events are labeled."
  (with-temp-buffer
    (insert (lispm-transcript-header "t" '(("agent" . "t")))
            (lispm-transcript-section "Message from boss" "do it")
            (lispm-transcript-section "Event: after-save-hook" "a.el saved")
            "* Assistant\n")
    (org-mode)
    (goto-char (point-max))
    (lispm-transcript-insert-blocks (list (lispm-test--tool "t1" "edit_file" '((path . "a"))))
                                    (lambda (_) "WAITING"))
    (let ((parsed (lispm-transcript-parse)))
      (should (equal (plist-get parsed :pending) '("t1")))
      (should (equal (alist-get 'content (car (plist-get parsed :messages)))
                     (vector '((type . "text") (text . "[Message from agent boss]\ndo it"))
                             '((type . "text") (text . "[Event: after-save-hook]\na.el saved"))))))
    ;; A call marked DONE whose result was deleted gets a placeholder.
    (goto-char (point-min))
    (re-search-forward "^\\*\\* WAITING")
    (lispm-transcript-set-state (line-beginning-position) "DONE")
    (let ((parsed (lispm-transcript-parse)))
      (should-not (plist-get parsed :pending))
      (should (equal (alist-get 'is_error (aref (alist-get 'content (nth 2 (plist-get parsed :messages))) 0))
                     t)))))

;;;; Streaming

(defun lispm-test--sse (&rest events)
  "EVENTS as a server-sent event stream."
  (mapconcat (lambda (e) (format "event: %s\ndata: %s\n\n" (alist-get 'type e) (lispm--json-encode e)))
             events ""))

(ert-deftest lispm-test-sse-decoder ()
  "The decoder reassembles blocks from arbitrarily split chunks."
  (let ((stream (lispm-test--sse
                 '((type . "message_start")
                   (message (id . "m") (role . "assistant") (content . []) (stop_reason . :null)
                            (usage (input_tokens . 7))))
                 '((type . "content_block_start") (index . 0)
                   (content_block (type . "thinking") (thinking . "") (signature . "")))
                 '((type . "content_block_delta") (index . 0)
                   (delta (type . "thinking_delta") (thinking . "Looking")))
                 '((type . "content_block_delta") (index . 0)
                   (delta (type . "signature_delta") (signature . "abc")))
                 '((type . "content_block_stop") (index . 0))
                 '((type . "content_block_start") (index . 1)
                   (content_block (type . "tool_use") (id . "t1") (name . "x") (input)))
                 '((type . "content_block_delta") (index . 1)
                   (delta (type . "input_json_delta") (partial_json . "{\"a\": [1, ")))
                 '((type . "content_block_delta") (index . 1)
                   (delta (type . "input_json_delta") (partial_json . "2]}")))
                 '((type . "content_block_stop") (index . 1))
                 '((type . "content_block_start") (index . 2)
                   (content_block (type . "tool_use") (id . "t2") (name . "x") (input)))
                 '((type . "content_block_delta") (index . 2)
                   (delta (type . "input_json_delta") (partial_json . "{\"a\": tru")))
                 '((type . "content_block_stop") (index . 2))
                 '((type . "message_delta") (delta (stop_reason . "tool_use"))
                   (usage (output_tokens . 30)))
                 '((type . "message_stop")))))
    (dolist (size '(1 5 13 1000))
      (let ((sse (lispm-anthropic-stream-create)) deltas)
        (cl-loop for i from 0 below (length stream) by size
                 do (lispm-anthropic-feed sse (substring stream i (min (length stream) (+ i size)))
                                    (lambda (kind text) (push (cons kind text) deltas))))
        (let ((message (lispm-anthropic-result sse)))
          (should (lispm-anthropic-stream-done sse))
          (should (equal (alist-get 'stop_reason message) "tool_use"))
          (should (equal (alist-get 'input_tokens (alist-get 'usage message)) 7))
          (should (equal (alist-get 'output_tokens (alist-get 'usage message)) 30))
          (should (equal (aref (alist-get 'content message) 0)
                         '((type . "thinking") (thinking . "Looking") (signature . "abc"))))
          (should (equal (alist-get 'input (aref (alist-get 'content message) 1)) '((a . [1 2]))))
          (should (equal (alist-get 'lispm-invalid message) '("t2")))
          (should (equal (seq-remove (lambda (d) (eq (car d) 'tool-input)) (reverse deltas))
                         '((thinking . "Looking") (tool . "x") (tool . "x"))))
          ;; Each piece of a call's input is reported with what came before.
          (should (equal (seq-filter (lambda (d) (eq (car d) 'tool-input)) (reverse deltas))
                         '((tool-input :id "t1" :name "x" :json "{\"a\": [1, ")
                           (tool-input :id "t1" :name "x" :json "{\"a\": [1, 2]}")
                           (tool-input :id "t2" :name "x" :json "{\"a\": tru")))))))))

(ert-deftest lispm-test-fallback-cleanup ()
  "Before a fallback boundary only text blocks are kept."
  (should (equal (lispm-anthropic--after-fallback
                  (list (lispm-test--thinking "" "s") (lispm-test--text "partial")
                        (lispm-test--tool "t" "x" nil)
                        '((type . "fallback")) (lispm-test--text "rest")))
                 (list (lispm-test--text "partial") '((type . "fallback"))
                       (lispm-test--text "rest")))))

(ert-deftest lispm-test-default-backend ()
  "No backend is assumed: the first with an API key in the environment, else none."
  (lispm-test-env
    (let ((lispm-backends (eval (car (get 'lispm-backends 'standard-value)) t))
          (lispm-backend nil)
          (process-environment
           (seq-remove (lambda (v) (string-match-p "\\`\\(OPENAI\\|ANTHROPIC\\)_API_KEY=" v))
                       process-environment)))
      (should-not (lispm-backend-name))
      (setenv "ANTHROPIC_API_KEY" "sk-test")
      (should (eq (lispm-backend-name) 'anthropic))
      (setenv "OPENAI_API_KEY" "sk-test")
      (should (eq (lispm-backend-name) 'openai))
      ;; An empty variable holds no key.
      (setenv "OPENAI_API_KEY" "")
      (should (eq (lispm-backend-name) 'anthropic))
      (setenv "ANTHROPIC_API_KEY")
      ;; With none, a turn stops at once, saying what to set, and sends
      ;; nothing; stopping there ends it.
      (let ((agent (lispm-defagent tester "Test agent." :root project)))
        (lispm-tell agent "hello")
        (should (lispm-test--wait (lambda () (lispm-agent-condition agent))))
        (should (string-match-p "No model backend: set OPENAI_API_KEY or ANTHROPIC_API_KEY"
                                (plist-get (lispm-agent-condition agent) :message)))
        (lispm-restart agent (plist-get (lispm-agent-condition agent) :id) 'abort)
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should (string-match-p "^\\* Note\nNo model backend: set OPENAI_API_KEY"
                                (lispm-test--transcript agent)))
        (should-not lispm-test--requests)))))

(ert-deftest lispm-test-request-features ()
  "First-party extensions are only sent to backends that support them."
  (let ((tools (list '((name . "t") (input_schema (type . "object")))))
        (messages (list `((role . "user")
                          (content . [((type . "tool_result") (tool_use_id . "x")
                                       (content . "boom") (is_error . t))])))))
    (pcase-let ((`(,body . ,betas)
                 (lispm-api-request (alist-get 'anthropic lispm-backends)
                                    "claude-opus-5-5" "high" "sys" tools messages)))
      (should (equal (alist-get 'thinking body)
                     '((type . "adaptive") (display . "updates")
                       (block_binding (prefix_mismatch_behavior . "drop_block")))))
      (should (equal (alist-get 'fallbacks body) "default"))
      (should (member "server-side-fallback-2026-07-01" betas))
      (should (eq (alist-get 'eager_input_streaming (aref (alist-get 'tools body) 0)) t))
      (should (equal (aref (alist-get 'messages body) 0) (car messages))))
    (pcase-let ((`(,body . ,betas)
                 (lispm-api-request '(:api anthropic :model "m")
                                    "gpt-5.5" "high" "sys" tools messages)))
      (should-not (assq 'thinking body))
      (should-not (assq 'fallbacks body))
      (should-not betas)
      (should (eq (alist-get 'stream body) :false))
      (should (equal (alist-get 'output_config body) '((effort . "high"))))
      (should-not (assq 'eager_input_streaming (aref (alist-get 'tools body) 0)))
      (should (equal (alist-get 'content (aref (alist-get 'messages body) 0))
                     [((type . "tool_result") (tool_use_id . "x") (content . "Error: boom"))])))))

(defmacro lispm-test--with-server (&rest body)
  "Run BODY with test/fake_messages_server.py listening.
BODY can use `port' and `log', the file the server records requests in."
  (declare (indent 0))
  `(progn
     (skip-unless (and (executable-find "python3") (executable-find "curl")))
     (let* ((log (make-temp-file "lispm-log"))
            (server (make-process :name "fake-messages" :noquery t
                                  :buffer (generate-new-buffer " *fake-messages*")
                                  :command (list "python3" (expand-file-name "fake_messages_server.py"
                                                                             lispm-test--dir)
                                                 log)))
            (output (lambda () (with-current-buffer (process-buffer server) (buffer-string)))))
       (unwind-protect
           (progn
             (should (lispm-test--wait (lambda () (string-match-p "^[0-9]+\n" (funcall output))) 10))
             (let ((port (string-to-number (funcall output))))
               (ignore port)
               ,@body))
         (delete-process server)
         (kill-buffer (process-buffer server))
         (delete-file log)))))

(defun lispm-test--logged-requests (log)
  "Requests the fake server recorded in LOG, oldest first."
  (with-temp-buffer
    (insert-file-contents log)
    (mapcar #'lispm--json-decode (split-string (buffer-string) "\n" t))))

(ert-deftest lispm-test-curl-stream ()
  "The curl transport streams, decodes UTF-8 split across chunks and reports errors."
  (lispm-test--with-server
    (let ((backend (list :url (format "http://127.0.0.1:%d" port)
                         :key "test-key"))
          result err deltas)
      (lispm-api-stream backend '((model . "fake") (messages . [((role . "user") (content . "hi"))]))
                        '("beta-a" "beta-b")
                        :on-delta (lambda (kind text) (push (cons kind text) deltas))
                        :on-done (lambda (m) (setq result m))
                        :on-error (lambda (e) (setq err e)))
      (should (lispm-test--wait (lambda () (or result err)) 10))
      (should-not err)
      (should (equal (alist-get 'stop_reason result) "tool_use"))
      (should (equal (append (alist-get 'content result) nil)
                     '(((type . "text") (text . "Hello, λ→∀"))
                       ((type . "tool_use") (id . "toolu_1") (name . "read_file")
                        (input (path . "a.txt"))))))
      (should (equal (mapconcat #'cdr (seq-filter (lambda (d) (eq (car d) 'text)) (reverse deltas)) "")
                     "Hello, λ→∀"))
      (let* ((logged (lispm--json-decode (with-temp-buffer
                                           (insert-file-contents log)
                                           (buffer-substring (point-min) (line-end-position)))))
             (headers (alist-get 'headers logged)))
        (should (equal (alist-get 'path logged) "/v1/messages"))
        (should (equal (alist-get 'x-api-key headers) "test-key"))
        (should (equal (alist-get 'anthropic-beta headers) "beta-a,beta-b"))
        (should (equal (alist-get 'model (alist-get 'body logged)) "fake")))
      (setq result nil err nil)
      (lispm-api-stream backend '((model . "fake") (stream . :false)) nil
                        :on-done (lambda (m) (setq result m))
                        :on-error (lambda (e) (setq err e)))
      (should (lispm-test--wait (lambda () (or result err)) 10))
      (should-not err)
      (should (equal (mapcar (lambda (b) (alist-get 'input b)) (alist-get 'content result))
                     '(nil ((x . 1)) nil)))
      (setq result nil err nil)
      (lispm-api-stream backend '((model . "overloaded")) nil
                        :on-done (lambda (m) (setq result m))
                        :on-error (lambda (e) (setq err e)))
      (should (lispm-test--wait (lambda () (or result err)) 10))
      (should (equal err '(:status 529 :type "overloaded_error" :message "Overloaded")))
      (should (lispm-api-retryable-p err)))))

;;;; OpenAI shape

(defconst lispm-test--conversation
  (list '((role . "user") (content . [((type . "text") (text . "hi"))]))
        '((role . "assistant")
          (content . [((type . "thinking") (thinking . "") (signature . "s"))
                      ((type . "text") (text . "Reading."))
                      ((type . "tool_use") (id . "c1") (name . "read_file") (input (path . "a.txt")))
                      ((type . "tool_use") (id . "c2") (name . "list_buffers") (input))]))
        '((role . "user")
          (content . [((type . "tool_result") (tool_use_id . "c1") (content . "one"))
                      ((type . "tool_result") (tool_use_id . "c2") (content . "boom") (is_error . t))
                      ((type . "text") (text . "and then?"))])))
  "A Messages-shape conversation with parallel tool calls, a failure and a follow-up.")

(ert-deftest lispm-test-openai-request ()
  "Messages-shape conversations become Chat Completions requests."
  (pcase-let ((`(,body . ,betas)
               (lispm-api-request '(:api openai :max-tokens 1000
                                    :features (streaming reasoning-effort stream-usage
                                               max-completion-tokens))
                                  "gpt-x" "low" "Be brief."
                                  (list (lispm-tool-schema (lispm-get-tool "read_file")))
                                  lispm-test--conversation)))
    (should-not betas)
    (should (eq (alist-get 'stream body) t))
    (should (equal (alist-get 'max_completion_tokens body) 1000))
    (should (equal (alist-get 'reasoning_effort body) "low"))
    (should (equal (alist-get 'stream_options body) '((include_usage . t))))
    (let ((tool (aref (alist-get 'tools body) 0)))
      (should (equal (alist-get 'type tool) "function"))
      (should (equal (alist-get 'name (alist-get 'function tool)) "read_file"))
      (should (equal (alist-get 'type (alist-get 'parameters (alist-get 'function tool))) "object")))
    (should (equal (alist-get 'messages body)
                   [((role . "system") (content . "Be brief."))
                    ((role . "user") (content . "hi"))
                    ((role . "assistant") (content . "Reading.")
                     (tool_calls . [((id . "c1") (type . "function")
                                     (function (name . "read_file") (arguments . "{\"path\":\"a.txt\"}")))
                                    ((id . "c2") (type . "function")
                                     (function (name . "list_buffers") (arguments . "{}")))]))
                    ((role . "tool") (tool_call_id . "c1") (content . "one"))
                    ((role . "tool") (tool_call_id . "c2") (content . "Error: boom"))
                    ((role . "user") (content . "and then?"))])))
  ;; Without features: the most widely understood request.
  (pcase-let ((`(,body . ,_) (lispm-api-request '(:api openai) "m" "low" nil nil
                                                (list (car lispm-test--conversation)))))
    (should (equal body `((model . "m") (max_tokens . ,lispm-max-tokens) (stream . :false)
                          (messages . [((role . "user") (content . "hi"))]))))))

(ert-deftest lispm-test-openai-decoder ()
  "Chat Completions streams are reassembled into Messages-shape messages."
  (let ((stream
         (concat
          (mapconcat
           (lambda (e) (format "data: %s\n\n" (lispm--json-encode e)))
           (list '((choices . [((index . 0) (delta (role . "assistant") (content . ""))
                                (finish_reason . :null))]))
                 '((choices . [((index . 0) (delta (reasoning_content . "Think")))]))
                 '((choices . [((index . 0) (delta (content . "Hel")))]))
                 '((choices . [((index . 0) (delta (content . "lo")))]))
                 '((choices . [((index . 0)
                                (delta (tool_calls . [((index . 0) (id . "c1") (type . "function")
                                                       (function (name . "read_file")
                                                                 (arguments . "{\"pa")))])))]))
                 '((choices . [((index . 0)
                                (delta (tool_calls . [((index . 1) (id . "c2")
                                                       (function (name . "x") (arguments . "{bad")))])))]))
                 '((choices . [((index . 0)
                                (delta (tool_calls . [((index . 0)
                                                       (function (arguments . "th\": 1}")))])))]))
                 '((choices . [((index . 0) (delta) (finish_reason . "tool_calls"))]))
                 '((choices . []) (usage (prompt_tokens . 9) (completion_tokens . 4)
                                         (prompt_tokens_details (cached_tokens . 2)))))
           "")
          "data: [DONE]\n\n")))
    (dolist (size '(1 7 1000))
      (let ((state (lispm-openai-stream-create)) deltas)
        (cl-loop for i from 0 below (length stream) by size
                 do (lispm-openai-feed state (substring stream i (min (length stream) (+ i size)))
                                       (lambda (kind text) (push (cons kind text) deltas))))
        (should (lispm-openai-stream-complete-p state))
        (let ((message (lispm-openai-result state)))
          (should (equal (alist-get 'content message)
                         [((type . "reasoning") (text . "Think"))
                          ((type . "text") (text . "Hello"))
                          ((type . "tool_use") (id . "c1") (name . "read_file") (input (path . 1)))
                          ((type . "tool_use") (id . "c2") (name . "x") (input))]))
          (should (equal (alist-get 'stop_reason message) "tool_use"))
          (should (equal (alist-get 'lispm-invalid message) '("c2")))
          (should (equal (alist-get 'usage message)
                         '((input_tokens . 9) (output_tokens . 4) (cache_read_input_tokens . 2))))
          (should (equal (seq-remove (lambda (d) (eq (car d) 'tool-input)) (reverse deltas))
                         '((thinking . "Think") (text . "Hel") (text . "lo")
                           (tool . "read_file") (tool . "x"))))
          (should (equal (seq-filter (lambda (d) (eq (car d) 'tool-input)) (reverse deltas))
                         '((tool-input :id "c1" :name "read_file" :json "{\"pa")
                           (tool-input :id "c2" :name "x" :json "{bad")
                           (tool-input :id "c1" :name "read_file" :json "{\"path\": 1}")))))))))

(ert-deftest lispm-test-openai-turn ()
  "Whole agent turns against a Chat Completions server, streamed and not."
  (lispm-test-env
    (lispm-test--with-server
      (with-temp-file (expand-file-name "a.txt" project) (insert "hello\n"))
      (let ((lispm-api-function #'lispm-api-stream)
            (url (format "http://127.0.0.1:%d/v1" port)))
        (pcase-dolist (`(,name ,features ,usage)
                       '((streamer (streaming reasoning-effort stream-usage) (40 14 10))
                         (whole (reasoning-effort) (40 14 0))))
          (push `(,name :api openai :url ,url :model "fake" :key "sk-test" :features ,features)
                lispm-backends)
          (let ((agent (lispm-register-agent name :root project :backend name))
                reply)
            (lispm-tell agent "Look around" :callback (lambda (r) (setq reply r)))
            (should (lispm-test--wait (lambda () reply) 10))
            ;; The final answer quotes both tool results.
            (should (string-prefix-p "Read:      1\thello | " reply))
            (should (string-match-p " | .*project/?\na\\.txt\\'" reply))
            (should (equal (lispm-agent-usage agent) usage))
            (let ((text (lispm-test--transcript agent)))
              (should (string-match-p "^\\*\\* Reasoning: Plan: read, then list\\.$" text))
              (should (string-match-p "^\\*\\* Reply\nLooking λ→∀\\.\n" text))
              (should (string-match-p "^\\*\\* DONE read_file  a\\.txt$" text))
              (should (string-match-p "^\\*\\* DONE list_directory  \\.$" text)))
            (let* ((request (car (last (lispm-test--logged-requests log))))
                   (body (alist-get 'body request))
                   (messages (alist-get 'messages body)))
              (should (equal (alist-get 'path request) "/v1/chat/completions"))
              (should (equal (alist-get 'authorization (alist-get 'headers request)) "Bearer sk-test"))
              (should (equal (alist-get 'stream body) (if (memq 'streaming features) t :false)))
              (should (equal (alist-get 'reasoning_effort body) "medium"))
              (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                             '("system" "user" "assistant" "tool" "tool")))
              ;; The reasoning is shown, not sent back.
              (should (equal (alist-get 'content (aref messages 2)) "Looking λ→∀."))
              (should (equal (mapcar (lambda (c) (alist-get 'arguments (alist-get 'function c)))
                                     (alist-get 'tool_calls (aref messages 2)))
                             '("{\"path\":\"a.txt\"}" "{\"path\":\".\"}")))
              (should (equal (alist-get 'tool_call_id (aref messages 3)) "call_a"))
              (should (string-match-p "hello" (alist-get 'content (aref messages 3)))))))))))

;;;; Tools

(ert-deftest lispm-test-tool-schema-and-validation ()
  (let* ((tool (lispm-get-tool "edit_file"))
         (schema (lispm-tool-schema tool))
         (input-schema (alist-get 'input_schema schema)))
    (should (equal (alist-get 'name schema) "edit_file"))
    (should (equal (alist-get 'required input-schema) ["why" "path" "old_text" "new_text"]))
    (should (equal (mapcar #'car (alist-get 'properties input-schema))
                   '(why path old_text new_text replace_all)))
    (should (equal (lispm-tools--arguments tool '((path . "a") (old_text . "x") (new_text . "y")
                                                  (replace_all . :false) (why . "w")))
                   '("a" "x" "y" nil)))
    (should-error (lispm-tools--arguments tool '((path . 3) (old_text . "x") (new_text . "y"))))
    (should-error (lispm-tools--arguments tool '((path . "a"))))
    ;; read tools take no rationale
    (should-not (assq 'why (alist-get 'properties (alist-get 'input_schema
                                                             (lispm-tool-schema (lispm-get-tool "read_file"))))))))

(ert-deftest lispm-test-builtin-tools ()
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (file (expand-file-name "a.txt" project)))
      (with-temp-file file (insert "one\ntwo\nthree\n"))
      (should (equal (lispm-test--call agent "read_file" '((path . "a.txt") (offset . 2) (limit . 1)))
                     '("     2\ttwo\n[... 1 more line; continue with offset 3]" . nil)))
      ;; Reads see unsaved buffer edits.
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (insert "four\n"))
      (should (string-match-p "unsaved changes.*\n\\(?:.*\n\\)*.*four"
                              (car (lispm-test--call agent "read_file" '((path . "a.txt"))))))
      (with-current-buffer (find-file-noselect file) (save-buffer))
      ;; Edits are exact, unique, one undo step, and journaled.
      (should (cdr (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "zzz") (new_text . "y")))))
      (should (string-match-p "occurs 2 times"
                              (car (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "e\n")
                                                                         (new_text . "E\n"))))))
      (should (equal (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "two") (new_text . "2")))
                     '("Replaced 1 occurrence in a.txt." . nil)))
      (should (equal (lispm-test--file project "a.txt") "one\n2\nthree\nfour\n"))
      (should (equal (plist-get (cdr (car (cdr (assq 0 (lispm-agent-journal agent))))) :text)
                     "one\ntwo\nthree\nfour\n"))
      (with-current-buffer (find-file-noselect file)
        (let ((last-command nil)) (undo))
        (should (equal (buffer-string) "one\ntwo\nthree\nfour\n")))
      ;; Previews are diffs; nothing changes yet.
      (should (string-match-p "^-three\n\\+3"
                              (let ((lispm-current-agent agent))
                                (lispm-tool-preview-text (lispm-get-tool "edit_file")
                                                         '((path . "a.txt") (old_text . "three")
                                                           (new_text . "3"))))))
      (should (string-match-p "=> 3" (car (lispm-test--call agent "eval_elisp" '((code . "(+ 1 2)"))))))
      (should (string-match-p "Function car"
                              (car (lispm-test--call agent "describe_symbol" '((name . "car"))))))
      ;; Permission: writes outside the root ask even when writes are auto.
      (should (eq (lispm-tool-action agent (lispm-get-tool "write_file") '((path . "b.txt"))) 'auto))
      (should (eq (lispm-tool-action agent (lispm-get-tool "write_file") '((path . "/tmp/x"))) 'ask))
      (should (eq (lispm-tool-action agent (lispm-get-tool "run_shell") nil) 'ask)))))

(ert-deftest lispm-test-search-regexps ()
  "Regexps in ripgrep's syntax become Emacs regexps; globs match paths."
  (let ((case-fold-search nil))
    (pcase-dolist (`(,pattern ,emacs ,string ,match)
                   '(("foo(bar|baz)\\d{2}" "foo\\(bar\\|baz\\)[0-9]\\{2\\}" "foobaz12" t)
                     ("^\\s*\\(defun" "^[[:space:]]*(defun" "  (defun" t)
                     ("(?:ab)+c" "\\(?:ab\\)+c" "ababc" t)
                     ("[^a-z]+$" "[^a-z]+$" "ABC" t)
                     ("[\\d.]+" "[0-9.]+" "3.14" t)
                     ("[a\\]]" "[]a]" "]" t)
                     ("a\\|b{x}" "a|b{x}" "a|b{x}" t)
                     ("\\w+@\\w+" "[[:alnum:]_]+@[[:alnum:]_]+" "assistant@lispm" t)
                     ("x{2,}" "x\\{2,\\}" "x" nil)))
      (should (equal (lispm-tools--emacs-regexp pattern) emacs))
      (should (eq (and (string-match-p emacs string) t) match))))
  (should-error (lispm-tools--emacs-regexp "foo(?=bar)"))
  (should-error (lispm-tools--emacs-regexp "a[b"))
  (pcase-dolist (`(,glob ,file ,match)
                 '(("*.el" "/r/a/b.el" t) ("*.el" "/r/a/b.elc" nil)
                   ("src/**/*.ts" "/r/src/x/y.ts" t) ("src/**/*.ts" "/r/src/y.ts" t)
                   ("*.{ts,tsx}" "/r/a.tsx" t) ("test/*" "/r/test/a/b" nil)))
    (should (eq (and (funcall (lispm-tools--glob-predicate glob "/r/") file) t) match))))

(defun lispm-test--search (agent input)
  "Run search_files on INPUT as AGENT and wait for its result."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke (lispm-get-tool "search_files") input
                       (lambda (text errorp) (setq result (cons text errorp))))
    (lispm-test--wait (lambda () result))
    result))

(ert-deftest lispm-test-search-files ()
  "search_files searches in Emacs: project files, open buffers, in slices."
  (lispm-test-env
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          ;; One file per slice, so the search goes through the event loop.
          (lispm-tools--search-slice 0))
      (dolist (file '("a.el" "sub/b.el" "notes.txt" ".hidden/c.el"))
        (make-directory (file-name-directory (expand-file-name file project)) t)
        (with-temp-file (expand-file-name file project)
          (insert ";; header\n(defun lispm-demo-" (file-name-base file) " ()\n  \"Doc.\")\n")))
      (with-temp-file (expand-file-name "blob.bin" project)
        (set-buffer-multibyte nil)
        (insert "(defun lispm-demo-blob\0\0\0"))
      (should (equal (lispm-test--search agent '((pattern . "\\(defun lispm-demo-(a|b)\\b")))
                     '("a.el:2:(defun lispm-demo-a ()\nsub/b.el:2:(defun lispm-demo-b ()" . nil)))
      ;; Hidden directories and binary files are left out; globs and paths narrow.
      (should (equal (car (lispm-test--search agent '((pattern . "lispm-demo-\\w+") (glob . "*.txt"))))
                     "notes.txt:2:(defun lispm-demo-notes ()"))
      (should (equal (car (lispm-test--search agent '((pattern . "DOC") (path . "sub") (ignore_case . t))))
                     "sub/b.el:3:  \"Doc.\")"))
      (should (equal (car (lispm-test--search agent '((pattern . "(?i)doc") (path . "a.el"))))
                     "a.el:3:  \"Doc.\")"))
      (should (equal (car (lispm-test--search agent '((pattern . "no such text"))))
                     "No matches."))
      (should (equal (substring-no-properties
                      (lispm-chat--tool-title '(:name "search_files"
                                                :input ((pattern . "a|b") (path . "src") (glob . "*.el")))))
                     "Search(\"a|b\" in src, *.el)"))
      (should (cdr (lispm-test--search agent '((pattern . "a(?=b)")))))
      ;; Unsaved edits in a file's buffer are what is searched, as read_file shows them.
      (with-current-buffer (find-file-noselect (expand-file-name "a.el" project))
        (goto-char (point-max))
        (insert "(defun lispm-demo-unsaved ())\n"))
      (should (equal (car (lispm-test--search agent '((pattern . "demo-unsaved"))))
                     "a.el:4:(defun lispm-demo-unsaved ())"))
      ;; In a git project, the project's files are searched: .gitignore applies.
      (when (executable-find "git")
        (let ((default-directory (file-name-as-directory (expand-file-name "repo" project))))
          (make-directory default-directory)
          (call-process "git" nil nil nil "init" "-q")
          (with-temp-file ".gitignore" (insert "ignored.el\n"))
          (with-temp-file "ignored.el" (insert "(defun lispm-demo-ignored ())\n"))
          (with-temp-file "kept.el" (insert "(defun lispm-demo-kept ())\n")))
        (should (equal (car (lispm-test--search agent '((pattern . "lispm-demo-[ik]") (path . "repo"))))
                       "repo/kept.el:1:(defun lispm-demo-kept ())"))
        ;; Also when Emacs runs under git, which points GIT_DIR elsewhere.
        (let ((process-environment (cons (concat "GIT_DIR=" (expand-file-name "elsewhere/.git" project))
                                         process-environment)))
          (should (equal (car (lispm-test--search agent '((pattern . "lispm-demo-[ik]") (path . "repo"))))
                         "repo/kept.el:1:(defun lispm-demo-kept ())")))))))

;;;; MCP and images

(defmacro lispm-test-with-mcp (props &rest body)
  "Run BODY with the fake MCP server of test/ configured, with PROPS.
mcp.el waits for idle time, which batch Emacs never has; plain timers
stand in."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'run-with-idle-timer)
              (lambda (secs repeat function &rest args)
                (apply #'run-at-time secs repeat function args))))
     (let ((lispm-mcp-servers
            (list (append (list "fake" :command "python3"
                                :args (list (expand-file-name "fake_mcp_server.py" lispm-test--dir)))
                          ',props)))
           (mcp-server-wait-initial-time 0))
       (unwind-protect (progn ,@body)
         (lispm-mcp-stop)
         (clrhash lispm-mcp--failed)))))

(ert-deftest lispm-test-images-in-requests ()
  "Images of tool results are read in, the oldest dropped in batches."
  (let* ((dir (make-temp-file "lispm-images-" t))
         (files (cl-loop for i from 1 to 12
                         collect (let ((file (expand-file-name (format "%02d.png" i) dir)))
                                   (with-temp-file file (insert "png"))
                                   file)))
         (messages (mapcar (lambda (file)
                             `((role . "user")
                               (content . [((type . "tool_result") (tool_use_id . "x")
                                            (content . ,(vector (lispm-transcript--text-block "shot")
                                                                (lispm-transcript--image-block file))))])))
                           files))
         (lispm-context-images 5)
         (loaded (lispm-transcript--load-images messages))
         (blocks (mapcar (lambda (m) (aref (alist-get 'content (aref (alist-get 'content m) 0)) 1))
                         loaded)))
    (unwind-protect
        (progn
          ;; 12 images, 5 kept: 7 too many, dropped in a batch of 5.
          (should (equal (mapcar (lambda (b) (alist-get 'type b)) blocks)
                         '("text" "text" "text" "text" "text"
                           "image" "image" "image" "image" "image" "image" "image")))
          (should (equal (alist-get 'source (nth 11 blocks))
                         `((type . "base64") (media_type . "image/png")
                           (data . ,(base64-encode-string "png" t)))))
          ;; Chat Completions: tool messages are text; the images follow.
          (let ((out (lispm-openai--messages nil (last loaded 1))))
            (should (equal (alist-get 'role (nth 0 out)) "tool"))
            (should (equal (alist-get 'content (nth 0 out)) "shot"))
            (should (equal (alist-get 'role (nth 1 out)) "user"))
            (should (equal (alist-get 'url (alist-get 'image_url (aref (alist-get 'content (nth 1 out)) 1)))
                           (concat "data:image/png;base64," (base64-encode-string "png" t)))))
          ;; Results with images round-trip through the transcript.
          (with-temp-buffer
            (setq default-directory (file-name-as-directory dir))
            (insert "* User\nhi\n* Assistant\n")
            (insert (lispm-transcript--tool-use '((type . "tool_use") (id . "t1") (name . "snap") (input))
                                                "DONE"))
            (insert (lispm-transcript-result "Here." nil (list (car files)) dir))
            (should (string-match-p "^\\[\\[file:01\\.png\\]\\]$" (buffer-string)))
            (let ((result (aref (alist-get 'content (nth 2 (plist-get (lispm-transcript-parse) :messages))) 0)))
              (should (equal (alist-get 'type (aref (alist-get 'content result) 1)) "image")))))
      (delete-directory dir t))))

(ert-deftest lispm-test-mcp ()
  "An MCP server's tools are an agent's: they wait for it, take JSON, return images."
  (lispm-test-env
    (lispm-test-with-mcp (:defer nil)
      (let* ((default-directory project)
             ;; Opening the session starts the server.
             (agent (lispm-start)))
        (setf (lispm-agent-policy agent) '((mcp . auto)))
        (setq lispm-test--scripts
              `((assistant@project
                 ,(lispm-test--msg "tool_use"
                                   (lispm-test--tool "t1" "mcp__fake__echo"
                                                     '((text . "hi") (loud . :false))))
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "mcp__fake__snap" nil))
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t3" "mcp__fake__fail" nil))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
        (lispm-tell agent "Use the fake tools")
        (should (lispm-test--wait (lispm-test--idle-p agent) 15))
        ;; The first request waited for the server, so the session has its tools.
        (let ((first (car (last lispm-test--requests))))
          (should (seq-find (lambda (tool) (equal (alist-get 'name tool) "mcp__fake__echo"))
                            (alist-get 'tools first)))
          (should (equal (alist-get 'input_schema
                                    (seq-find (lambda (tool) (equal (alist-get 'name tool) "mcp__fake__snap"))
                                              (alist-get 'tools first)))
                         '((type . "object") (properties)))))
        (let ((transcript (lispm-test--transcript agent)))
          ;; JSON false reaches the server as false.
          (should (string-match-p "hi {\"loud\": false, \"text\": \"hi\"}" transcript))
          (should (string-match-p "^\\*\\* DONE mcp__fake__snap" transcript))
          (should (string-match-p "^\\*\\* FAILED mcp__fake__fail" transcript))
          (should (string-match-p (format "^\\[\\[file:%s\\.images/0001\\.png\\]\\]$"
                                          (regexp-quote (lispm-agent-id agent)))
                                  transcript)))
        (should (file-exists-p (expand-file-name "0001.png" (lispm-image-directory agent))))
        ;; The image went to the model.
        (let* ((messages (alist-get 'messages (car lispm-test--requests)))
               (snap (seq-find (lambda (block) (equal (alist-get 'tool_use_id block) "t2"))
                               (seq-mapcat (lambda (m) (append (alist-get 'content m) nil)) messages))))
          (should (equal (alist-get 'type (aref (alist-get 'content snap) 1)) "image")))
        ;; The chat shows the server's tools by name, and their images.
        (let ((text (lispm-test--chat-text (lispm-chat agent))))
          (should (string-match-p "⏺ fake:snap\n  ⎿  Here it is\\.\n     1 image (TAB to show)" text)))
        (lispm-mcp-stop)
        (should-not (lispm-get-tool "mcp__fake__echo"))))))

(ert-deftest lispm-test-tool-search ()
  "Deferred tools are found with search_tools, then offered, also after a resume."
  (lispm-test-env
    (lispm-test-with-mcp (:description "Fake things for tests.")
      (let* ((default-directory project)
             (agent (lispm-start))
             (names (lambda (request) (mapcar (lambda (tool) (alist-get 'name tool))
                                              (alist-get 'tools request)))))
        ;; Nothing starts with the session.
        (should-not (lispm-mcp--running-p "fake"))
        (setq lispm-test--scripts
              `((assistant@project
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "search_tools" '((query . "echo text"))))
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "mcp__fake__echo" '((text . "on"))))
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t3" "search_tools"
                                                               '((query . "select:mcp__fake__snap"))))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
        (setf (lispm-agent-policy agent) '((mcp . auto)))
        (lispm-tell agent "Use the fake things")
        (should (lispm-test--wait (lispm-test--idle-p agent) 15))
        (let ((requests (reverse lispm-test--requests)))
          ;; The first request offers the search, not the tools ...
          (should (member "search_tools" (funcall names (nth 0 requests))))
          (should-not (seq-some (lambda (name) (string-prefix-p "mcp__" name))
                                (funcall names (nth 0 requests))))
          (should (string-match-p "- fake: Fake things for tests\\."
                                  (alist-get 'description
                                             (seq-find (lambda (tool) (equal (alist-get 'name tool) "search_tools"))
                                                       (alist-get 'tools (nth 0 requests))))))
          ;; ... then the tool the search found, and only it ...
          (should (member "mcp__fake__echo" (funcall names (nth 1 requests))))
          (should-not (member "mcp__fake__snap" (funcall names (nth 1 requests))))
          ;; ... and one asked for by name.
          (should (member "mcp__fake__snap" (funcall names (nth 3 requests)))))
        (let ((transcript (lispm-test--transcript agent)))
          (should (string-match-p "^Tools now available: mcp__fake__echo\\.$" transcript))
          (should (string-match-p "^on {\"text\": \"on\"}$" transcript)))
        ;; A resumed session has them again, as its transcript says.
        (let ((file (buffer-file-name (lispm-agent-buffer agent))))
          (with-current-buffer (lispm-agent-buffer agent) (save-buffer))
          (lispm-retire agent)
          (let ((revived (lispm-resume-file file)))
            (should (equal (lispm-agent-loaded-tools revived) '("mcp__fake__echo" "mcp__fake__snap")))
            (should (member "mcp__fake__snap"
                            (mapcar #'lispm-tool-name (lispm-agent-tools* revived))))))))))

(ert-deftest lispm-test-tool-search-ranking ()
  "Keywords rank tools by their names, then descriptions; select: picks by name."
  (let ((tools (mapcar (lambda (spec)
                         (lispm-tool--create :name (car spec) :description (cdr spec) :deferred t))
                       '(("mcp__p__see" . "Captures a screenshot and an element map.")
                         ("mcp__p__click" . "Clicks on UI elements or coordinates.")
                         ("mcp__p__image" . "Screenshot only; use see for element detection.")
                         ("mcp__g__log" . "Shows the commit log.")))))
    (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "screenshot" tools 5))
                   '("mcp__p__see" "mcp__p__image")))
    (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "click screenshot" tools 1))
                   '("mcp__p__click")))
    (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "mcp__p" tools 5))
                   '("mcp__p__see" "mcp__p__click" "mcp__p__image")))
    (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "select:mcp__g__log, mcp__p__see" tools 1))
                   '("mcp__p__see" "mcp__g__log")))
    (should-not (lispm-tools--find "weather" tools 5))
    ;; A source's name keeps its tools; the other words rank them.
    (let ((sources (list (list :name "p" :provides (lambda (name) (string-prefix-p "mcp__p__" name))))))
      (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "p click" tools 5 sources))
                     '("mcp__p__click")))
      (should (equal (mapcar #'lispm-tool-name (lispm-tools--find "p" tools 2 sources))
                     '("mcp__p__see" "mcp__p__click"))))))

(ert-deftest lispm-test-auto-title ()
  "An untitled session is titled by the model after its first turn, once."
  (lispm-test-env
    (let* ((lispm-auto-title t)
           (agent (lispm-defagent tester "Test agent." :root project))
           (asked 0)
           (lispm-api-function
            (lambda (backend body betas &rest callbacks)
              (if (not (string-prefix-p "You title conversations" (alist-get 'system body)))
                  (apply #'lispm-test--fake-api backend body betas callbacks)
                (cl-incf asked)
                (should-not (alist-get 'tools body))
                (should (string-match-p "Say hello to Lisp"
                                        (alist-get 'text (aref (alist-get 'content (aref (alist-get 'messages body) 0)) 0))))
                (run-at-time 0.01 nil (plist-get callbacks :on-done)
                             (lispm-test--msg "end_turn" (lispm-test--text "“向 Lisp 问好”。")))
                nil))))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "Hello, Lisp."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Again.")))))
      (lispm-tell agent "Say hello to Lisp")
      (should (lispm-test--wait (lambda () (lispm-session-title agent))))
      (should (equal (lispm-session-title agent) "向 Lisp 问好"))
      (should (equal (plist-get (seq-find (lambda (s) (equal (plist-get s :id) (lispm-agent-id agent)))
                                          (lispm-sessions))
                                :title)
                     "向 Lisp 问好"))
      ;; Asked once; a title the user gives is kept.
      (lispm-rename-session (buffer-file-name (lispm-agent-buffer agent)) "Mine")
      (lispm-tell agent "Once more")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (= asked 1))
      (should (equal (lispm-session-title agent) "Mine")))))

(ert-deftest lispm-test-title-language ()
  "Titles are asked for in `lispm-title-language', else in the user's language."
  (let ((lispm-title-language nil))
    (should (string-match-p "topic, in the language the user writes in\\. " (lispm-title--system))))
  (let ((lispm-title-language "English"))
    (should (string-match-p "topic, in English\\. " (lispm-title--system)))))

;;;; Emacs itself, and skills

(ert-deftest lispm-test-eval-async ()
  "eval_elisp waits for a value promised with lispm-async."
  (lispm-test-env
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          result)
      (let ((lispm-current-agent agent))
        (lispm-tool-invoke (lispm-get-tool "eval_elisp")
                           '((code . "(princ \"start\") (lispm-async (lambda (resolve) (run-at-time 0.05 nil resolve (* 6 7))))"))
                           (lambda (text errorp &rest _) (setq result (cons text errorp)))))
      (should-not result)
      (should (lispm-test--wait (lambda () result)))
      (should (equal result '("start\n=> 42" . nil)))
      (setq result nil)
      (let ((lispm-current-agent agent) (lispm-eval-timeout 0.1))
        (lispm-tool-invoke (lispm-get-tool "eval_elisp")
                           '((code . "(lispm-async #'ignore)"))
                           (lambda (text errorp &rest _) (setq result (cons text errorp)))))
      (should (lispm-test--wait (lambda () result)))
      (should (cdr result))
      (should (string-match-p "Timed out" (car result))))))

(ert-deftest lispm-test-apropos-and-info ()
  "apropos finds what Emacs can do; read_info reads its manuals."
  (lispm-test-env
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (should (string-match-p "^lispm-tools--emacs-regexp (function) "
                              (car (lispm-test--call agent "apropos" '((pattern . "lispm emacs regexp"))))))
      (let ((variables (car (lispm-test--call agent "apropos" '((pattern . "lispm eval timeout") (kind . "variable"))))))
        (should (string-match-p "^lispm-eval-timeout (variable) Seconds eval_elisp waits" variables))
        (should-not (string-match-p "(function)" variables)))
      (should (string-match-p "Nothing matches" (car (lispm-test--call agent "apropos" '((pattern . "zzqqxx"))))))
      (should (string-match-p "\\`(eww)Top\n" (car (lispm-test--call agent "read_info" '((manual . "eww"))))))
      (should (string-match-p "\\`(eww)Basics\n" (car (lispm-test--call agent "read_info" '((manual . "eww") (search . "readable"))))))
      (should (string-match-p "\\* Elisp:" (car (lispm-test--call agent "read_info" nil))))
      (should (cdr (lispm-test--call agent "read_info" '((manual . "no-such-manual"))))))))

(defun lispm-test--write (file text)
  "Write TEXT to FILE, making its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(ert-deftest lispm-test-skills ()
  "Skills are listed, loaded with their code and tools, and restored on resume."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "user-skills/" project))
           (lispm-skill-directories (list user-skills))
           (default-directory project)
           (agent (lispm-start))
           (names (lambda (request) (mapcar (lambda (tool) (alist-get 'name tool))
                                            (alist-get 'tools request)))))
      (lispm-test--write (expand-file-name "greet.org" user-skills)
                         (concat "#+title: Greet\n#+description: Greet people warmly.\n\n"
                                 "Use greet_tool.\n\n#+begin_src emacs-lisp :lispm load\n"
                                 "(defun lispm-test-greet-helper () \"hi\")\n"
                                 "(lispm-deftool greet_tool ((who string \"Who.\")) \"Greet WHO.\"\n"
                                 "  (format \"Hello, %s!\" who))\n#+end_src\n"))
      (lispm-test--write (expand-file-name ".lispm/skills/local/SKILL.org" project)
                         (concat "#+title: Local\n#+description: A project's own skill.\n\n"
                                 "#+begin_src emacs-lisp :lispm load\n(defun lispm-test-local-helper () t)\n#+end_src\n"))
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "skill" '((name . "greet"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "greet_tool" '((who . "Lisp"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t3" "skill" '((name . "local"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (unwind-protect
          (progn
            (lispm-tell agent "Greet Lisp")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let ((requests (reverse lispm-test--requests)))
              (let ((skill (seq-find (lambda (tool) (equal (alist-get 'name tool) "skill"))
                                     (alist-get 'tools (nth 0 requests)))))
                (should (string-match-p "- greet: Greet people warmly\\." (alist-get 'description skill)))
                (should (string-match-p "- local: A project's own skill\\." (alist-get 'description skill))))
              (should-not (member "greet_tool" (funcall names (nth 0 requests))))
              (should (member "greet_tool" (funcall names (nth 1 requests)))))
            (let ((transcript (lispm-test--transcript agent)))
              (should (string-match-p "^Skill greet loaded; tools now available: greet_tool\\.$" transcript))
              (should (string-match-p "^Use greet_tool\\.$" transcript))
              (should (string-match-p "^Hello, Lisp!$" transcript))
              (should (string-match-p "^Skill local loaded (a project's skill: its code was not evaluated)\\.$"
                                      transcript)))
            (should (fboundp 'lispm-test-greet-helper))
            (should-not (fboundp 'lispm-test-local-helper))
            ;; A resumed session evaluates the code again and has the tools.
            (let ((file (buffer-file-name (lispm-agent-buffer agent))))
              (with-current-buffer (lispm-agent-buffer agent) (save-buffer))
              (lispm-retire agent)
              (fmakunbound 'lispm-test-greet-helper)
              (let ((revived (lispm-resume-file file)))
                (should (fboundp 'lispm-test-greet-helper))
                (should (member "greet_tool" (mapcar #'lispm-tool-name (lispm-agent-tools* revived)))))))
        (remhash "greet_tool" lispm-tools--registry)
        (fmakunbound 'lispm-test-greet-helper)))))

(ert-deftest lispm-test-agent-aware-schemas ()
  "A tool's description computed for the request sees the agent, also when loaded later.
The tool comes from a source prepared asynchronously, as a server that
starts is, so its schema is made outside the call that loaded it."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (lispm-tool-sources
            (list (lambda ()
                    (list (list :name "late" :description "A source that takes a moment."
                                :pending t
                                :prepare (lambda (callback) (run-at-time 0.01 nil callback nil))
                                :provides (lambda (name) (equal name "whose_tool"))))))))
      (lispm-register-tool
       (lispm-tool--create
        :name "whose_tool" :deferred t :permission 'read
        :description (lambda () (format "For %s." (and lispm-current-agent
                                                       (lispm-agent-name lispm-current-agent))))
        :input-schema '((type . "object") (properties))
        :function (lambda (_input) "mine")))
      (unwind-protect
          (progn
            (setq lispm-test--scripts
                  `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "search_tools"
                                                                           '((query . "select:whose_tool"))))
                            ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
            (lispm-tell agent "Find whose_tool")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let* ((last (car lispm-test--requests))
                   (tool (seq-find (lambda (tool) (equal (alist-get 'name tool) "whose_tool"))
                                   (alist-get 'tools last))))
              (should (equal (alist-get 'description tool) "For tester."))))
        (remhash "whose_tool" lispm-tools--registry)))))

(ert-deftest lispm-test-web-skill ()
  "The web skill searches and reads pages (here through url.el, from a local server)."
  (lispm-test-env
    (let* ((server (make-process :name "lispm-test-web" :buffer (generate-new-buffer " *web*")
                                 :command (list "python3" "-u" "-m" "http.server" "0" "--bind" "127.0.0.1"
                                                "--directory" (expand-file-name "web" lispm-test--dir))
                                 :noquery t))
           (agent (lispm-defagent tester "Test agent." :root project :policy '((web . auto))))
           port)
      (unwind-protect
          (progn
            (should (lispm-test--wait (lambda ()
                                        (with-current-buffer (process-buffer server)
                                          (goto-char (point-min))
                                          (when (re-search-forward "port \\([0-9]+\\)" nil t)
                                            (setq port (match-string 1)))))))
            (should (string-match-p "tools now available: web_fetch, web_search"
                                    (car (lispm-test--call agent "skill" '((name . "web"))))))
            (let ((lispm-web-search-url (format "http://127.0.0.1:%s/search.html?q=%%s" port))
                  result)
              (let ((lispm-current-agent agent))
                (lispm-tool-invoke (lispm-get-tool "web_search") '((query . "lisp machine"))
                                   (lambda (text errorp &rest _) (setq result (cons text errorp)))))
              (should (lispm-test--wait (lambda () result)))
              (should (equal (car result)
                             (concat "Lisp machine - Wikipedia\nhttps://en.wikipedia.org/wiki/Lisp_machine\n"
                                     "Lisp machines were general-purpose computers designed to run Lisp.\n\n"
                                     "Genera\nhttps://example.org/genera\nThe Symbolics operating system.")))
              (setq result nil)
              (let ((lispm-current-agent agent))
                (lispm-tool-invoke (lispm-get-tool "web_fetch")
                                   `((url . ,(format "http://127.0.0.1:%s/page.html" port)))
                                   (lambda (text errorp &rest _) (setq result (cons text errorp)))))
              (should (lispm-test--wait (lambda () result)))
              (let ((page (car result)))
                (should (string-match-p "\\`Title: Lisp Machines\nURL: http://127\\.0\\.0\\.1:[0-9]+/page\\.html\n" page))
                (should (string-match-p "Chaosnet" page))
                (should (string-match-p "历史上的 Lisp 机器" page))
                ;; The readable part, not the navigation; its links, absolute.
                (should-not (string-match-p "Privacy" (car (split-string page "\n\nLinks:\n"))))
                (should (string-match-p (format "^\\[[0-9]+\\] Genera: http://127\\.0\\.0\\.1:%s/genera\\.html$" port)
                                        page)))))
        (delete-process server)
        (kill-buffer (process-buffer server))
        (remhash "web_search" lispm-tools--registry)
        (remhash "web_fetch" lispm-tools--registry)))))

(ert-deftest lispm-test-web-skill-holds-callbacks ()
  "WebKit's script callback and page are held until a while after the page closes.
On macOS nothing else keeps them from the garbage collector (see
skills/web.org).  Batch Emacs makes no xwidgets: stubs stand in."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (page (make-symbol "xwidget"))
           (html "<html><head><title>Example</title></head><body><p>Hello</p></body></html>")
           (held-p (lambda (object) (seq-some (lambda (held) (memq object held)) lispm-web--held)))
           ;; The skill's code, evaluated, calls the stubbed primitives by
           ;; name: native trampolines for them would only cost seconds.
           (native-comp-enable-subr-trampolines nil)
           scripts results)
      (unwind-protect
          (progn
            (lispm-test--call agent "skill" '((name . "web")))
            (cl-letf (((symbol-function 'lispm-web--webkit-p) #'always)
                      ((symbol-function 'make-xwidget) (lambda (&rest _) page))
                      ((symbol-function 'xwidget-put) #'ignore)
                      ((symbol-function 'xwidget-webkit-goto-uri) #'ignore)
                      ((symbol-function 'xwidget-webkit-uri) (lambda (_) "https://example.org/"))
                      ((symbol-function 'xwidget-webkit-estimated-load-progress) (lambda (_) 1.0))
                      ((symbol-function 'xwidget-webkit-title) (lambda (_) "Example"))
                      ((symbol-function 'kill-xwidget) #'ignore)
                      ((symbol-function 'xwidget-webkit-execute-script)
                       (lambda (_xwidget _script fun) (push fun scripts))))
              (let ((lispm-web-settle 0)
                    (lispm-web--hold-seconds 0.5))
                ;; The page answers: it is read, and still held once closed.
                (lispm-web-dom "https://example.org/" (lambda (&rest args) (push args results)))
                (should (lispm-test--wait (lambda () scripts)))
                (should (funcall held-p (car scripts)))
                (funcall (car scripts) html)
                (should (equal (cdar results) '("https://example.org/" "Example" nil)))
                (should (funcall held-p (car scripts)))
                (should (funcall held-p page))
                (should (lispm-test--wait (lambda () (not (funcall held-p (car scripts))))))
                (should-not (funcall held-p page))
                ;; The deadline passes first: the page fails, and its answer,
                ;; coming late, is still held and changes nothing.
                (setq scripts nil results nil)
                (lispm-web-dom "https://example.org/" (lambda (&rest args) (push args results)))
                (should (lispm-test--wait (lambda () scripts)))
                (let ((now (symbol-function 'float-time)))
                  (cl-letf (((symbol-function 'float-time)
                             (lambda (&optional time) (if time (funcall now time) (+ (funcall now) 11)))))
                    (should (lispm-test--wait (lambda () results)))))
                (should (equal (car results) '(nil "https://example.org/" nil "The page did not give its HTML.")))
                (should (funcall held-p (car scripts)))
                (funcall (car scripts) html)
                (should (= (length results) 1))
                (should (lispm-test--wait (lambda () (not (funcall held-p (car scripts)))))))))
        (remhash "web_search" lispm-tools--registry)
        (remhash "web_fetch" lispm-tools--registry)))))

(ert-deftest lispm-test-run-shell ()
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)) result)
      (let ((lispm-current-agent agent))
        (lispm-tool-invoke (lispm-get-tool "run_shell") '((command . "pwd; echo hi; exit 3"))
                           (lambda (text errorp) (setq result (cons text errorp)))))
      (should (lispm-test--wait (lambda () result)))
      (should (string-match-p "project/?\nhi\n\n?\\[exit status 3\\]" (car result)))
      (setq result nil)
      (let ((lispm-current-agent agent))
        (lispm-tool-invoke (lispm-get-tool "run_shell") '((command . "sleep 5") (timeout . 1))
                           (lambda (text errorp) (setq result (cons text errorp)))))
      (should (lispm-test--wait (lambda () result) 5))
      (should (string-match-p "timeout" (car result)))
      (should (cdr result)))))

(ert-deftest lispm-test-tool-interrupted ()
  "C-g interrupts a call, also where Emacs inhibits quitting; the call fails."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)))
      (lispm-register-tool
       (lispm-tool--create :name "lispm_test_quit" :description "Quit." :params nil
                           :function (lambda () (signal 'quit nil))))
      (unwind-protect
          (progn
            (should (equal (lispm-test--call agent "lispm_test_quit" nil)
                           (cons lispm-tools--interrupted t)))
            (should (equal (lispm-test--call agent "eval_elisp" '((code . "(signal 'quit nil)")))
                           (cons lispm-tools--interrupted t)))
            ;; Timers and process sentinels, where calls run, inhibit quitting.
            (should (equal (let ((inhibit-quit t))
                             (lispm-test--call agent "eval_elisp" '((code . "inhibit-quit"))))
                           '("=> nil" . nil)))
            ;; Input does not end a while-no-input around the call.
            (should (equal (let ((throw-on-input 'lispm-test-input))
                             (lispm-test--call agent "eval_elisp" '((code . "throw-on-input"))))
                           '("=> nil" . nil)))
            ;; The result is given once the tool has returned, and going on
            ;; with the turn cannot be interrupted.
            (let (inhibited)
              (lispm-tool-invoke (lispm-get-tool "eval_elisp") '((code . "1"))
                                 (lambda (_text _errorp) (setq inhibited inhibit-quit)))
              (should (eq inhibited t)))
            ;; What follows the tool is not interruptible either.
            (let (inhibited)
              (cl-letf* ((text (symbol-function 'lispm-tools--result-text))
                         ((symbol-function 'lispm-tools--result-text)
                          (lambda (value) (setq inhibited inhibit-quit) (funcall text value))))
                (lispm-test--call agent "describe_symbol" '((name . "car"))))
              (should (eq inhibited t)))
            ;; C-g while run_shell sets up stops the command too.
            (let (proc)
              (cl-letf* ((make (symbol-function 'make-process))
                         ((symbol-function 'make-process)
                          (lambda (&rest args)
                            (prog1 (setq proc (apply make args))
                              (setq quit-flag t)))))
                (should (equal (lispm-test--call agent "run_shell" '((command . "sleep 30")))
                               (cons lispm-tools--interrupted t))))
              (should-not quit-flag)
              (should-not (process-live-p proc)))
            ;; A C-g pressed before the call is left for what it was meant for.
            (let (result kept)
              (let ((inhibit-quit t))
                (setq quit-flag t)
                (setq result (lispm-test--call agent "eval_elisp" '((code . "(+ 1 2)")))
                      kept quit-flag
                      quit-flag nil))
              (should (equal result '("=> 3" . nil)))
              (should kept)))
        (remhash "lispm_test_quit" lispm-tools--registry)))))

(ert-deftest lispm-test-interrupted-call-in-turn ()
  "A call interrupted with C-g fails, and the turn goes on."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((exec . auto))))
          reply)
      ;; As if C-g were pressed while the call runs: from the timer that
      ;; delivers the response, where quitting is inhibited.
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "eval_elisp"
                                                   '((why . "w")
                                                     (code . "(setq quit-flag t) (dotimes (_ 100000) (ignore)) 'finished"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Stopped.")))))
      (lispm-tell agent "loop" :callback (lambda (r) (setq reply r)))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should-not quit-flag)
      (should (equal reply "Stopped."))
      (should (string-match-p "^\\*\\* FAILED eval_elisp" (lispm-test--transcript agent)))
      (let ((results (alist-get 'content (aref (alist-get 'messages (car lispm-test--requests)) 2))))
        (should (string-match-p "Interrupted: the user pressed C-g"
                                (alist-get 'content (aref results 0))))))))

(defmacro lispm-test--never-asking (&rest body)
  "Run BODY, failing if it asks the user anything."
  (declare (indent 0))
  `(cl-letf* ((asked (lambda (&rest args) (error "Asked the user: %S" args)))
              ((symbol-function 'yes-or-no-p) asked)
              ((symbol-function 'y-or-n-p) asked)
              ((symbol-function 'ask-user-about-supersession-threat) asked)
              ((symbol-function 'read-char-choice) asked))
     ,@body))

(defvar lispm-test--disk-clock 0
  "Seconds ahead of now the last change on disk was dated.")

(defun lispm-test--change-on-disk (file text)
  "Make FILE hold TEXT, as another program would, a moment later."
  (call-process "sh" nil nil nil "-c" "printf %s \"$1\" > \"$2\"" "sh" text file)
  ;; Within a second, or two on some file systems, a file's time may not
  ;; tell the change: date each change ten seconds after the last.
  (set-file-times file (time-add nil (cl-incf lispm-test--disk-clock 10))))

(defun lispm-test--disk (file)
  "FILE's text on disk, whatever buffer visits it."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest lispm-test-files-changed-on-disk ()
  "Tools see a file as it is on disk although a buffer visits it, and never ask."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (file (expand-file-name "a.txt" project)))
      (lispm-test--never-asking
        (with-temp-file file (insert "one\n"))
        (find-file-noselect file)
        ;; A buffer without unsaved changes is reverted.
        (lispm-test--change-on-disk file "two\n")
        (should (equal (lispm-test--call agent "read_file" '((path . "a.txt")))
                       '("     1\ttwo" . nil)))
        (should (equal (with-current-buffer (find-buffer-visiting file) (buffer-string)) "two\n"))
        (lispm-test--change-on-disk file "three\n")
        (should (equal (lispm-test--call agent "edit_file"
                                         '((path . "a.txt") (old_text . "three") (new_text . "3")))
                       '("Replaced 1 occurrence in a.txt." . nil)))
        (should (equal (lispm-test--disk file) "3\n"))
        ;; Searches read the file, not an out-of-date buffer.
        (lispm-test--change-on-disk file "four\n")
        (let (result)
          (let ((lispm-current-agent agent))
            (lispm-tool-invoke (lispm-get-tool "search_files") '((pattern . "four"))
                               (lambda (text errorp) (setq result (cons text errorp)))))
          (should (lispm-test--wait (lambda () result)))
          (should (equal result '("a.txt:1:four" . nil))))
        ;; Unsaved changes and a change on disk: a conflict, reported, nothing lost.
        (with-current-buffer (find-buffer-visiting file)
          (revert-buffer t t t)
          (goto-char (point-max))
          (insert "mine\n"))
        (lispm-test--change-on-disk file "five\n")
        (let ((result (lispm-test--call agent "edit_file"
                                        '((path . "a.txt") (old_text . "five") (new_text . "5")))))
          (should (cdr result))
          (should (string-match-p "changed on disk.*unsaved changes" (car result))))
        (should (equal (lispm-test--disk file) "five\n"))
        (should (equal (with-current-buffer (find-buffer-visiting file) (buffer-string))
                       "four\nmine\n"))
        (should (string-match-p "\\`\\[open in Emacs, unsaved changes; the file changed on disk too\\]\n.*four"
                                (car (lispm-test--call agent "read_file" '((path . "a.txt"))))))
        ;; A file deleted on disk is gone, whatever its buffer holds.
        (with-current-buffer (find-buffer-visiting file) (revert-buffer t t t))
        (delete-file file)
        (should (string-match-p "No such file" (car (lispm-test--call agent "read_file" '((path . "a.txt"))))))
        (should (string-match-p "No such file"
                                (car (lispm-test--call agent "edit_file"
                                                       '((path . "a.txt") (old_text . "five")
                                                         (new_text . "5"))))))
        (should (equal (lispm-test--call agent "write_file" '((path . "a.txt") (content . "six\n")))
                       '("Created a.txt." . nil)))
        (should (equal (lispm-test--disk file) "six\n"))
        ;; Previews and the changes shown while a call is written change
        ;; nothing; they read an out-of-date buffer's file.
        (lispm-test--change-on-disk file "seven\n")
        (let ((lispm-current-agent agent)
              (edit '((path . "a.txt") (old_text . "seven") (new_text . "7"))))
          (should (equal (lispm--file-text file) "seven\n"))
          (should (string-match-p "^-seven\n\\+7"
                                  (lispm-tool-preview-text (lispm-get-tool "edit_file") edit)))
          (should-not (lispm-tool-would-change (lispm-get-tool "edit_file") edit))
          (should-not (lispm-tool-would-change (lispm-get-tool "write_file")
                                               '((path . "a.txt") (content . "8\n")))))
        (should (equal (with-current-buffer (find-buffer-visiting file) (buffer-string)) "six\n"))
        ;; A buffer whose file was deleted on disk is not saved back to it.
        (delete-file file)
        (should (string-match-p "deleted on disk"
                                (car (lispm-test--call agent "edit_buffer"
                                                       `((buffer . ,(buffer-name (find-buffer-visiting file)))
                                                         (old_text . "six") (new_text . "6"))))))
        (should-not (file-exists-p file))))))

(ert-deftest lispm-test-write-file-directory ()
  "write_file makes the directory a new file goes in, or says why it cannot.
When it cannot, nothing is left of the call: no buffer, no journal entry."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (locked (expand-file-name "locked/" project)))
      (lispm-test--never-asking
        (with-temp-file (expand-file-name "a.txt" project) (insert "a\n"))
        (make-directory locked)
        (set-file-modes locked #o555)
        (unwind-protect
            (dolist (case `(("a.txt/x" . "Cannot write a.txt/x: a.txt is not a directory")
                            ("a.txt/deeper/x" . "Cannot write a.txt/deeper/x: a.txt is not a directory")
                            ,@(unless (file-writable-p locked)
                                '(("locked/x" . "Cannot write locked/x: locked/ is not writable")
                                  ("locked/new/x" . "Cannot write locked/new/x: locked/new/ does not exist, \
and cannot be made: locked/ is not writable")))))
              (let ((file (expand-file-name (car case) project)))
                ;; Not even while the call is proposed.
                (let ((lispm-current-agent agent))
                  (should-error (lispm-tool-would-change (lispm-get-tool "write_file")
                                                         `((path . ,(car case)) (content . "x\n")))))
                (should-not (get-buffer "x"))
                (should (equal (lispm-test--call agent "write_file"
                                                 `((path . ,(car case)) (content . "x\n")))
                               (cons (cdr case) t)))
                (should-not (find-buffer-visiting file))
                (should-not (get-buffer "x"))))
          (set-file-modes locked #o755))
        (should-not (file-exists-p (expand-file-name "new" locked)))
        (should-not (lispm-agent-journal agent))
        ;; Outside the root too, named as they are.
        (unless (file-writable-p "/")
          (should (equal (lispm-test--call agent "write_file"
                                           '((path . "/lispm-nonexistent-dir/x") (content . "x\n")))
                         '("Cannot write /lispm-nonexistent-dir/x: /lispm-nonexistent-dir/ does not \
exist, and cannot be made: / is not writable" . t)))
          (should-not (get-buffer "x"))
          (should-not (lispm-agent-journal agent)))
        ;; A directory that can be made is, with its parents.
        (should (equal (lispm-test--call agent "write_file"
                                         '((path . "sub/deeper/x") (content . "x\n")))
                       '("Created sub/deeper/x." . nil)))
        (should (equal (lispm-test--disk (expand-file-name "sub/deeper/x" project)) "x\n"))))))

(ert-deftest lispm-test-calls-revert-buffers ()
  "After a call, buffers show what it did to their files."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (clean (expand-file-name "clean.txt" project))
          (edited (expand-file-name "edited.txt" project))
          result)
      (lispm-test--never-asking
        (with-temp-file clean (insert "old\n"))
        (with-temp-file edited (insert "old\n"))
        (find-file-noselect clean)
        (with-current-buffer (find-file-noselect edited) (insert "mine\n"))
        (let ((lispm-current-agent agent))
          (lispm-tool-invoke (lispm-get-tool "run_shell")
                             ;; touch -t: a later time than the buffers know, within a second.
                             '((command . "printf 'new\\n' > clean.txt; printf 'new\\n' > edited.txt; touch -t 203001010000 clean.txt edited.txt"))
                             (lambda (text errorp) (setq result (cons text errorp)))))
        (should (lispm-test--wait (lambda () result)))
        (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string)) "new\n"))
        ;; Unsaved changes are left alone.
        (should (equal (with-current-buffer (find-buffer-visiting edited) (buffer-string)) "mine\nold\n"))
        ;; Not only after shell commands: after Lisp too; and the hooks the
        ;; reverts run know the agent did it.
        (let ((code (format "(call-process \"sh\" nil nil nil \"-c\" %S)"
                            (format "printf lisp > %s; touch -t 203101010000 %s"
                                    (shell-quote-argument clean) (shell-quote-argument clean))))
              reverted-by)
          (with-current-buffer (find-buffer-visiting clean)
            (add-hook 'after-revert-hook (lambda () (setq reverted-by lispm-current-agent)) nil t))
          (should-not (cdr (lispm-test--call agent "eval_elisp" `((code . ,code)))))
          (should (eq reverted-by agent)))
        (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string)) "lisp"))
        ;; Not after calls that change no file.
        (lispm-test--change-on-disk clean "read\n")
        (lispm-test--call agent "list_buffers" nil)
        (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string)) "lisp"))
        ;; Nor when the user would rather not.
        (let ((lispm-revert-after-calls nil))
          (lispm-test--call agent "eval_elisp" '((code . "1"))))
        (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string)) "lisp"))
        ;; Nor buffers of files outside the agent's root.
        (let ((outside (make-temp-file "lispm-outside" nil ".txt" "out\n")))
          (unwind-protect
              (progn
                (find-file-noselect outside)
                (lispm-test--change-on-disk outside "changed\n")
                (lispm-test--call agent "eval_elisp" '((code . "1")))
                (should (equal (with-current-buffer (find-buffer-visiting outside) (buffer-string))
                               "out\n"))
                (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string))
                               "read\n")))
            (kill-buffer (find-buffer-visiting outside))
            (delete-file outside)))
        ;; Session transcripts, whose positions the runtime holds, are not
        ;; reverted, not even for a tool: read as they are, not changed.
        (with-current-buffer (find-buffer-visiting clean) (setq-local lispm-session-mode t))
        (lispm-test--change-on-disk clean "changed\n")
        (lispm-test--call agent "eval_elisp" '((code . "1")))
        (should (string-match-p "\\`\\[the session of an agent, as LispM holds it; the file changed on disk\\]\n.*read"
                                (car (lispm-test--call agent "read_file" '((path . "clean.txt"))))))
        (should (string-match-p "the session of an agent"
                                (car (lispm-test--call agent "write_file"
                                                       '((path . "clean.txt") (content . "x"))))))
        (should (equal (with-current-buffer (find-buffer-visiting clean) (buffer-string)) "read\n"))
        (should (equal (lispm-test--disk clean) "changed\n"))))))

(ert-deftest lispm-test-unwritable-file ()
  "Tools say why they cannot change a file there already, and never ask.
Nothing is left of such a call: no buffer it visited, no journal
entry.  A buffer the user made writable is changed only while edits
are not saved: saving it would ask."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (file (expand-file-name "ro.txt" project))
          (calls '(("edit_file" . ((path . "ro.txt") (old_text . "a") (new_text . "b")))
                   ("write_file" . ((path . "ro.txt") (content . "b\n")))))
          (unwritable '("Cannot write ro.txt: it is not writable" . t)))
      (lispm-test--never-asking
        ;; A directory is no file to write or edit.
        (make-directory (expand-file-name "sub" project))
        (dolist (call '(("write_file" . ((path . "sub") (content . "x\n")))
                        ("edit_file" . ((path . "sub") (old_text . "x") (new_text . "y")))))
          (should (equal (lispm-test--call agent (car call) (cdr call))
                         '("Cannot write sub: it is a directory" . t))))
        (should-not (get-buffer "sub"))
        ;; Nor is a buffer the user made read-only changed.
        (with-temp-file file (insert "a\n"))
        (with-current-buffer (find-file-noselect file) (read-only-mode 1))
        (should (equal (lispm-test--call agent "edit_file" (cdar calls))
                       '("Cannot change buffer ro.txt: it is read-only" . t)))
        (kill-buffer (find-buffer-visiting file))
        (should-not (lispm-agent-journal agent))
        (set-file-modes file #o444)
        (unwind-protect
            ;; Root writes whatever the modes say.
            (unless (file-writable-p file)
              (dolist (call calls)
                ;; Not even while the call is proposed.
                (let ((lispm-current-agent agent))
                  (should-error (lispm-tool-would-change (lispm-get-tool (car call)) (cdr call))))
                (should (equal (lispm-test--call agent (car call) (cdr call)) unwritable))
                (should-not (find-buffer-visiting file)))
              ;; Open in Emacs, read-only as Emacs made it, or writable as
              ;; the user made it (C-x C-q): saving would ask all the same.
              (let ((buffer (find-file-noselect file)))
                (dolist (read-only '(t nil))
                  (with-current-buffer buffer (read-only-mode (if read-only 1 -1)))
                  (dolist (call `(("edit_buffer" . ((buffer . "ro.txt") (old_text . "a")
                                                    (new_text . "b")))
                                  ,@calls))
                    (let ((lispm-current-agent agent))
                      (should-error (lispm-tool-would-change (lispm-get-tool (car call))
                                                             (cdr call))))
                    (should (equal (lispm-test--call agent (car call) (cdr call)) unwritable))))
                (should (equal (with-current-buffer buffer (buffer-string)) "a\n"))
                (should-not (buffer-modified-p buffer))
                (should-not (lispm-agent-journal agent))
                ;; Edits not saved, the buffer the user made writable is
                ;; changed; saving it is up to them.
                (let ((lispm-save-edits nil))
                  (should (equal (lispm-test--call agent "edit_file" (cdar calls))
                                 '("Replaced 1 occurrence in ro.txt." . nil)))
                  (should (equal (with-current-buffer buffer (buffer-string)) "b\n"))
                  (should (buffer-modified-p buffer))
                  (should (lispm-agent-journal agent))
                  ;; Not one read-only, nor the file unless a buffer visits it.
                  (with-current-buffer buffer (read-only-mode 1))
                  (should (equal (lispm-test--call agent "write_file" (cdadr calls)) unwritable))
                  (with-current-buffer buffer (set-buffer-modified-p nil))
                  (kill-buffer buffer)
                  (should (equal (lispm-test--call agent "write_file" (cdadr calls)) unwritable))
                  (should-not (find-buffer-visiting file))))
              (should (equal (lispm-test--disk file) "a\n")))
          (set-file-modes file #o644))))))

(ert-deftest lispm-test-memory-changed-on-disk ()
  "Memory a person changed on disk, by hand or with git, is kept, without a question."
  (lispm-test-env
    (let ((agent (lispm-defagent keeper :root project)))
      (lispm-test--never-asking
        (lispm-test--call agent "remember" '((title . "First") (content . "a")))
        (let ((file (lispm-memory-file agent)))
          (lispm-test--change-on-disk file (concat (lispm-test--disk file) "* By hand\nb\n"))
          (should-not (cdr (lispm-test--call agent "remember" '((title . "Second") (content . "c")))))
          (let ((text (lispm-test--disk file)))
            (should (string-match-p "^\\* First$" text))
            (should (string-match-p "^\\* By hand$" text))
            (should (string-match-p "^\\* Second$" text)))
          ;; Unsaved changes, and a change on disk too: memory is read as
          ;; the user sees it, so sessions still start, but not changed.
          (with-current-buffer (find-buffer-visiting file)
            (goto-char (point-max))
            (insert "* Mine\nd\n"))
          (lispm-test--change-on-disk file (concat (lispm-test--disk file) "* On disk\ne\n"))
          (should (string-match-p "^- Mine$" (lispm-system-prompt agent)))
          (should (string-match-p "unsaved changes"
                                  (car (lispm-test--call agent "remember"
                                                         '((title . "Third") (content . "f"))))))
          (should-not (string-match-p "Third" (lispm-test--disk file)))
          ;; Emptied in Emacs: the header is not saved over the changed file.
          (let ((disk (lispm-test--disk file)))
            (with-current-buffer (find-buffer-visiting file) (erase-buffer))
            (lispm-system-prompt agent)
            (should (equal (lispm-test--disk file) disk))))))))

(ert-deftest lispm-test-memory-deleted-on-disk ()
  "Memory deleted on disk, by hand or with git, stays deleted."
  (lispm-test-env
    (let ((agent (lispm-defagent keeper :root project)))
      (lispm-test--never-asking
        (lispm-test--call agent "remember" '((title . "Old") (content . "a")))
        (let ((file (lispm-memory-file agent)))
          (should (find-buffer-visiting file))
          (delete-file file)
          (should (equal (car (lispm-test--call agent "recall" '((query . ""))))
                         "Memory is empty."))
          (lispm-test--call agent "remember" '((title . "New") (content . "b")))
          (let ((text (lispm-test--disk file)))
            (should (string-match-p "\\`#\\+title: Memory of " text))
            (should (string-match-p "^\\* New$" text))
            (should-not (string-match-p "^\\* Old$" text))))))))

(ert-deftest lispm-test-out-of-date-buffers-elsewhere ()
  "Instruction files, the turn diff and large files go by what is on disk."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (agents (expand-file-name "AGENTS.md" project))
          (a (expand-file-name "a.txt" project)))
      (lispm-test--never-asking
        ;; The system prompt has the instructions on disk, not in a stale buffer.
        (with-temp-file agents (insert "Old rules.\n"))
        (find-file-noselect agents)
        (lispm-test--change-on-disk agents "New rules.\n")
        (let ((prompt (lispm-system-prompt agent)))
          (should (string-match-p "New rules\\." prompt))
          (should-not (string-match-p "Old rules" prompt)))
        ;; The turn diff compares with what is on disk.
        (with-temp-file a (insert "a\n"))
        (setf (lispm-agent-turn agent) 1)
        (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "a") (new_text . "A")))
        (lispm-test--change-on-disk a "on disk\n")
        (lispm-turn-diff agent)
        (should (string-match-p "^\\+on disk$" (with-current-buffer "*lispm-diff*" (buffer-string))))
        ;; A file too large to visit without asking is refused.
        (let ((large-file-warning-threshold 10)
              (big (expand-file-name "big.txt" project)))
          (with-temp-file big (insert (make-string 100 ?x) "\n"))
          (should (string-match-p "too large"
                                  (car (lispm-test--call agent "edit_file"
                                                         '((path . "big.txt") (old_text . "x")
                                                           (new_text . "y"))))))
          (should-not (find-buffer-visiting big)))))))

(ert-deftest lispm-test-revert-turn-conflict ()
  "A turn is reverted whole, or not at all while a file is in conflict."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (a (expand-file-name "a.txt" project))
          (b (expand-file-name "b.txt" project)))
      (lispm-test--never-asking
        (with-temp-file a (insert "a\n"))
        (with-temp-file b (insert "b\n"))
        (setf (lispm-agent-turn agent) 1)
        (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "a") (new_text . "A")))
        (lispm-test--call agent "edit_file" '((path . "b.txt") (old_text . "b") (new_text . "B")))
        (with-current-buffer (find-buffer-visiting b)
          (goto-char (point-max))
          (insert "mine\n"))
        (lispm-test--change-on-disk b "disk\n")
        (should (string-match-p "Nothing reverted: b.txt"
                                (cadr (should-error (lispm-revert-turn agent) :type 'user-error))))
        (should (equal (lispm-test--disk a) "A\n"))
        ;; Once the user has settled it, the whole turn is reverted.
        (with-current-buffer (find-buffer-visiting b) (revert-buffer t t t))
        (lispm-revert-turn agent)
        (should (equal (lispm-test--disk a) "a\n"))
        (should (equal (lispm-test--disk b) "b\n"))))))

;;;; The loop

(ert-deftest lispm-test-turn-with-approval ()
  "A turn: an automatic read, an edit approved through org, a reply, a revert."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello world\n"))
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use" (lispm-test--thinking "" "sig")
                                 (lispm-test--text "Reading.")
                                 (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t2" "edit_file"
                                                   '((why . "Greet Lisp") (path . "a.txt")
                                                     (old_text . "world") (new_text . "lisp"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Greet lisp" :callback (lambda (r) (setq reply r)))
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (equal (lispm-test--file project "a.txt") "hello world\n"))
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (should (re-search-forward "^\\*\\* DONE read_file  a.txt$" nil t))
        (should (re-search-forward "^\\*\\* WAITING edit_file  a.txt$" nil t))
        (should (search-forward "- why :: Greet Lisp" nil t))
        (should (search-forward "+hello lisp" nil t))
        ;; Approve the org way.
        (re-search-backward "^\\*\\* WAITING")
        (org-todo "APPROVED"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done."))
      (should (equal (lispm-test--file project "a.txt") "hello lisp\n"))
      (let ((requests (reverse lispm-test--requests)))
        (should (= (length requests) 3))
        (let ((messages (alist-get 'messages (nth 1 requests))))
          ;; The thinking block goes back verbatim, in order.
          (should (equal (alist-get 'content (aref messages 1))
                         (vector (lispm-test--thinking "" "sig") (lispm-test--text "Reading.")
                                 (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))))
          (should (string-match-p "hello world" (alist-get 'content (aref (alist-get 'content (aref messages 2)) 0)))))
        (let ((messages (alist-get 'messages (nth 2 requests))))
          (should (equal (aref (alist-get 'content (aref messages 4)) 0)
                         '((type . "tool_result") (tool_use_id . "t2")
                           (content . "Replaced 1 occurrence in a.txt."))))))
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (should (re-search-forward "^\\*\\* DONE edit_file" nil t))
        (should (re-search-forward "^\\*\\* Reply\nDone.\n\\* User\n\\'" nil t)))
      ;; Audit and roll back.
      (lispm-revert-turn agent)
      (should (equal (lispm-test--file project "a.txt") "hello world\n"))
      (should (string-match-p "\\* Event: revert\nThe user reverted the changes you made in turn 1"
                              (lispm-test--transcript agent)))
      (should (lispm-test--input-heading-p agent)))))

(ert-deftest lispm-test-org-approval-keeps-transcript ()
  "Approving with `org-todo' changes the call's keyword and nothing else.
The response's earlier blocks keep their case: ids, signatures and
text go back to the model byte for byte."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello world\n"))
    (let ((agent (lispm-defagent tester :root project)))
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use" (lispm-test--thinking "Plan it" "sig-a")
                                 (lispm-test--text "Editing a.txt.")
                                 (lispm-test--tool "t1" "edit_file"
                                                   '((why . "w") (path . "a.txt")
                                                     (old_text . "world") (new_text . "lisp"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (let ((before (lispm-test--transcript agent)))
        (with-current-buffer (lispm-agent-buffer agent)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* WAITING edit_file")
          (org-todo "APPROVED"))
        (let ((after (lispm-test--transcript agent))
              (case-fold-search nil))
          (should (equal (replace-regexp-in-string "^\\*\\* APPROVED " "** WAITING " after)
                         before))))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (alist-get 'content (aref (alist-get 'messages (car lispm-test--requests)) 1))
                     (vector (lispm-test--thinking "Plan it" "sig-a")
                             (lispm-test--text "Editing a.txt.")
                             (lispm-test--tool "t1" "edit_file"
                                               '((why . "w") (path . "a.txt")
                                                 (old_text . "world") (new_text . "lisp")))))))))

(defun lispm-test--input-heading-p (agent)
  "Whether AGENT's session ends with an empty input heading."
  (with-current-buffer (lispm-agent-buffer agent) (lispm--input-heading-p)))

(ert-deftest lispm-test-deny-and-policy ()
  "Denials reach the model with the user's reason; policies refuse without asking."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((write . deny)))))
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell" '((why . "w") (command . "rm -rf build")))
                                 (lispm-test--tool "t2" "write_file" '((why . "w") (path . "x") (content . "")))
                                 (lispm-test--tool "t3" "no_such_tool" nil))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Understood.")))))
      (lispm-tell agent "clean up")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (equal (lispm--waiting-calls agent) '("t1")))
      (lispm-resolve agent "t1" 'deny "keep the build")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let* ((messages (alist-get 'messages (car lispm-test--requests)))
             (results (alist-get 'content (aref messages 2))))
        ;; The test backend lacks tool-errors: failures are marked in the text.
        (should (equal (aref results 0)
                       '((type . "tool_result") (tool_use_id . "t1")
                         (content . "Error: The user denied this call. Their reason: keep the build"))))
        (should (string-match-p "Denied by policy" (alist-get 'content (aref results 1))))
        (should (string-match-p "no tool named no_such_tool" (alist-get 'content (aref results 2)))))
      (let ((text (lispm-test--transcript agent)))
        (should (string-match-p "^\\*\\* DENIED run_shell" text))
        (should (string-match-p "^\\*\\* DENIED write_file" text))
        (should (string-match-p "^\\*\\* FAILED no_such_tool" text))))))

(ert-deftest lispm-test-abort ()
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)))
      (setq lispm-test--scripts '((tester hang)))
      (lispm-tell agent "think forever")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'requesting))))
      (lispm-abort agent)
      (should (eq (lispm-agent-state agent) 'idle))
      (should (string-match-p "\\* Note\nAborted by the user.\n\\* User\n\\'"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-user-edits-are-context ()
  "What the user deletes or tags :ignore: in the buffer is gone from the context."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "first"))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "second")))))
      (lispm-tell agent "secret plan")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (re-search-forward "^\\* User$")
        (end-of-line)
        (insert " :ignore:")
        (goto-char (point-max))
        (insert "new topic")
        (lispm-send))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((messages (alist-get 'messages (car lispm-test--requests))))
        ;; The ignored question is dropped, and so is the reply to it,
        ;; which now leads the conversation.
        (should (= (length messages) 1))
        (should (equal (alist-get 'content (aref messages 0))
                       [((type . "text") (text . "new topic"))]))))))

(ert-deftest lispm-test-tools-frozen-per-session ()
  "The tool list is frozen when a session starts, like the system prompt.
A tool defined later does not change the request prefix of sessions
already running, unless the agent's tools grow on purpose
\(`lispm-tools-changed-functions'); new tools then come after the others."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (names (lambda (body) (mapcar (lambda (tool) (alist-get 'name tool))
                                        (alist-get 'tools body)))))
      (unwind-protect
          (progn
            (setq lispm-test--scripts
                  `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "one"))
                            ,(lispm-test--msg "end_turn" (lispm-test--text "two"))
                            ,(lispm-test--msg "end_turn" (lispm-test--text "three")))))
            (lispm-tell agent "first")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (lispm-deftool lispm_test_probe ()
              "A tool defined while a session runs."
              "ok")
            (lispm-tell agent "second")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (run-hook-with-args 'lispm-tools-changed-functions agent)
            (lispm-tell agent "third")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let ((requests (reverse lispm-test--requests)))
              (should (= (length requests) 3))
              (should (equal (alist-get 'tools (nth 1 requests))
                             (alist-get 'tools (nth 0 requests))))
              (should (equal (funcall names (nth 2 requests))
                             (append (funcall names (nth 0 requests)) '("lispm_test_probe"))))))
        (remhash "lispm_test_probe" lispm-tools--registry)))))

;;;; Agents and sessions

(ert-deftest lispm-test-start-and-resume ()
  "Agents start per project like Claude Code sessions, and resume from their files."
  (lispm-test-env
    (with-temp-file (expand-file-name "AGENTS.md" project) (insert "Answer in haiku.\n"))
    (let* ((default-directory project)
           (first (lispm-start))
           (second (lispm-start)))
      (should (eq (lispm-agent-name first) 'assistant@project))
      (should (eq (lispm-agent-name second) 'assistant@project<2>))
      (should (equal (lispm-agent-root first) project))
      (should-not (equal (lispm-agent-id first) (lispm-agent-id second)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Old pond.")))
              (assistant@project<2> ,(lispm-test--msg "end_turn" (lispm-test--text "Frog jumps.")))))
      (lispm-tell first "Write about a pond\nin a haiku")
      (lispm-tell second "Write about a frog")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p first))
                                                (funcall (lispm-test--idle-p second))
                                                (= (length lispm-test--requests) 2)))))
      ;; Project instructions reach the system prompt.
      (should (string-match-p "# Project instructions (AGENTS.md)\nAnswer in haiku\\."
                              (alist-get 'system (car lispm-test--requests))))
      ;; Both sessions are listed, newest first, titled by their first prompt.
      (let ((sessions (lispm-sessions)))
        (should (equal (sort (mapcar (lambda (s) (plist-get s :title)) sessions) #'string<)
                       '("Write about a frog" "Write about a pond")))
        (should (equal (plist-get (car sessions) :root) project))
        (should (equal (plist-get (car sessions) :role) "assistant")))
      ;; Closing a session ends its agent; opening the file brings it back.
      (let ((id (lispm-agent-id first))
            (file (buffer-file-name (lispm-agent-buffer first))))
        (kill-buffer (lispm-agent-buffer first))
        (should-not (memq first (lispm-agents)))
        (let ((revived (lispm-resume-file file)))
          (should (equal (lispm-agent-id revived) id))
          (should (eq (lispm-agent-name revived) 'assistant@project))
          (should (eq (lispm-agent-role revived) 'assistant))
          (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Splash.")))
                lispm-test--scripts)
          (lispm-tell revived "And then?")
          (should (lispm-test--wait (lispm-test--idle-p revived)))
          ;; The whole conversation went out again.
          (should (equal (mapcar (lambda (m) (alist-get 'role m))
                                 (alist-get 'messages (car lispm-test--requests)))
                         '("user" "assistant" "user")))
          (should (equal (lispm-session-files id) (list file)))
          ;; Renaming changes the listed title.
          (lispm-rename-session file "Pond poems")
          (should (equal (plist-get (seq-find (lambda (s) (equal (plist-get s :id) id))
                                              (lispm-sessions))
                                    :title)
                         "Pond poems")))))))

(defun lispm-test--crash (agent)
  "Save AGENT's session, then end it as an Emacs crash would.
The file keeps what was saved; whatever stopping the agent writes is
undone on disk.  Return the session file."
  (let* ((buffer (lispm-agent-buffer agent))
         (file (buffer-file-name buffer))
         (saved (progn (with-current-buffer buffer (save-buffer))
                       (with-temp-buffer (insert-file-contents file) (buffer-string)))))
    (kill-buffer buffer)
    (let ((coding-system-for-write 'utf-8))
      (write-region saved nil file nil 'silent))
    file))

(ert-deftest lispm-test-resume-suspended-calls ()
  "A session saved mid-turn resumes its calls from the transcript.
A call awaiting approval can still be approved; a running call, whose
process died with Emacs, is recorded as interrupted."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello world\n"))
    (let* ((default-directory project)
           (agent (lispm-start)))
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell"
                                                   '((why . "w") (command . "sleep 30")))
                                 (lispm-test--tool "t2" "edit_file"
                                                   '((why . "w") (path . "a.txt")
                                                     (old_text . "world") (new_text . "lisp")))))))
      (lispm-tell agent "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (let* ((file (lispm-test--crash agent))
             (revived (progn
                        ;; The file holds the turn as it was saved.
                        (should (string-match-p "^\\*\\* RUNNING run_shell" (lispm--file-text file)))
                        (lispm-resume-file file))))
        ;; The call awaiting approval is known again ...
        (should (equal (lispm--waiting-calls revived) '("t2")))
        (should (eq (lispm-agent-state revived) 'waiting))
        ;; ... the one that was running is not left without a result ...
        (should (string-match-p "^\\*\\* FAILED run_shell" (lispm-test--transcript revived)))
        ;; ... and approving the org way finishes the turn.
        (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))
              lispm-test--scripts)
        (with-current-buffer (lispm-agent-buffer revived)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* WAITING edit_file")
          (org-todo "APPROVED"))
        (should (lispm-test--wait (lispm-test--idle-p revived) 2))
        (should (equal (lispm-test--file project "a.txt") "hello lisp\n"))
        (let ((results (alist-get 'content
                                  (aref (alist-get 'messages (car lispm-test--requests)) 2))))
          (should (string-match-p "interrupted" (alist-get 'content (aref results 0))))
          (should (equal (alist-get 'content (aref results 1))
                         "Replaced 1 occurrence in a.txt.")))))))

(ert-deftest lispm-test-resume-call-to-late-tool ()
  "A resumed call gets its tool when it runs, not when the session opens.
Tools of MCP servers are registered only once the servers have
started, after the session opened."
  (lispm-test-env
    (lispm-deftool lispm_test_late ()
      "A tool registered late, as an MCP server's are."
      :permission write
      "late ok")
    (unwind-protect
        (let* ((default-directory project)
               (agent (lispm-start)))
          (setq lispm-test--scripts
                `((assistant@project
                   ,(lispm-test--msg "tool_use" (lispm-test--tool "l1" "lispm_test_late" '((why . "w")))))))
          (lispm-tell agent "Use the late tool")
          (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
          (let ((file (lispm-test--crash agent)))
            ;; Emacs restarts: the tool's server has not started yet.
            (remhash "lispm_test_late" lispm-tools--registry)
            (let ((revived (lispm-resume-file file)))
              (should (equal (lispm--waiting-calls revived) '("l1")))
              (lispm-deftool lispm_test_late ()
                "A tool registered late, as an MCP server's are."
                :permission write
                "late ok")
              (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))
                    lispm-test--scripts)
              (lispm-resolve revived "l1" 'approve)
              (should (lispm-test--wait (lispm-test--idle-p revived)))
              (should (equal (alist-get 'content
                                        (aref (alist-get 'content
                                                         (aref (alist-get 'messages (car lispm-test--requests)) 2))
                                              0))
                             "late ok")))))
      (remhash "lispm_test_late" lispm-tools--registry))))

;;;; Runtime state across restarts of Emacs

(defun lispm-test--crash-all ()
  "End every live agent as an Emacs crash would; return nothing.
Their sessions are saved first, then every file under `lispm-directory'
is put back as it was then, undoing on disk whatever stopping the
agents wrote, and the agents are forgotten, standing ones too."
  (dolist (agent (lispm-agents))
    (when (buffer-live-p (lispm-agent-buffer agent))
      (with-current-buffer (lispm-agent-buffer agent) (save-buffer))))
  (let ((saved (mapcar (lambda (file)
                         (cons file (with-temp-buffer (insert-file-contents file) (buffer-string))))
                       (directory-files-recursively lispm-directory "" nil))))
    (dolist (agent (lispm-agents))
      (when (buffer-live-p (lispm-agent-buffer agent))
        (kill-buffer (lispm-agent-buffer agent))))
    (setq lispm--agents nil)
    (pcase-dolist (`(,file . ,text) saved)
      (unless (and (file-exists-p file)
                   (equal text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (let ((coding-system-for-write 'utf-8))
          (write-region text nil file nil 'silent))))))

(defun lispm-test--inbox (root)
  "The text of the inbox file of the project at ROOT, or nil."
  (let ((file (lispm-inbox-file root)))
    (and (file-exists-p file) (lispm--file-text file))))

(ert-deftest lispm-test-inbox-outlives-emacs ()
  "Inputs queued for a busy agent, and the one its turn handles, outlive Emacs.
Reopened, the session waits at an `interrupted' condition: going on
answers the input the turn had, then the queued one."
  (lispm-test-env
    (let* ((default-directory project)
           (agent (lispm-start))
           (file (buffer-file-name (lispm-agent-buffer agent))))
      (setq lispm-test--scripts '((assistant@project hang)))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'requesting))))
      (lispm-tell agent "second" :from "reviewer")
      (let ((inbox (lispm-test--inbox project)))
        (should (string-match-p "^\\* RUNNING User$" inbox))
        (should (string-match-p "^\\* QUEUED Message from reviewer\n\\(?:.*\n\\)*?:END:\nsecond$" inbox)))
      (lispm-test--crash-all)
      (setq lispm-test--requests nil
            lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Answered first."))
                                 ,(lispm-test--msg "end_turn" (lispm-test--text "Answered second.")))))
      (let ((revived (lispm-resume-file file)))
        ;; The user's own turn waits for them to say whether to go on.
        (should (eq (plist-get (lispm-agent-condition revived) :type) 'interrupted))
        (should (equal (mapcar (lambda (e) (plist-get e :text)) (lispm-agent-inbox revived))
                       '("second")))
        (lispm-choose-restart-named revived 'continue)
        (should (lispm-test--wait (lispm-test--idle-p revived)))
        (should (= (length lispm-test--requests) 2))
        (let ((first (car (last lispm-test--requests)))
              (last (car lispm-test--requests)))
          (should (equal (alist-get 'text (aref (alist-get 'content (car (last (append (alist-get 'messages first) nil))))
                                                0))
                         "first"))
          (should (string-match-p "\\[Message from agent reviewer\\]\nsecond"
                                  (format "%S" (alist-get 'messages last)))))
        ;; Nothing waits any more.
        (should-not (string-match-p "^\\* \\(?:QUEUED\\|RUNNING\\)" (or (lispm-test--inbox project) "")))))))

(ert-deftest lispm-test-interrupted-turn-goes-on ()
  "A turn an event started, which Emacs stopped, goes on once its session opens.
Calls that were running are recorded as interrupted, and the model is
asked again with their results; the response streaming in when Emacs
stopped never reaches the file."
  (lispm-test-env
    (let* ((default-directory project)
           (agent (lispm-start))
           (file (buffer-file-name (lispm-agent-buffer agent))))
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell" '((why . "w") (command . "sleep 30")))))))
      (lispm-tell agent "the build failed" :event "compilation")
      (should (lispm-test--wait (lambda () (equal (lispm-agent-status agent) "running run_shell"))))
      (lispm-test--crash-all)
      (setq lispm-test--requests nil
            lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Looked again.")))))
      (let ((revived (lispm-resume-file file)))
        (should (lispm-test--wait (lispm-test--idle-p revived)))
        (should (= (length lispm-test--requests) 1))
        (should (string-match-p "interrupted" (format "%S" (alist-get 'messages (car lispm-test--requests)))))
        (should (string-match-p "^\\* DONE Condition: Turn interrupted" (lispm-test--transcript revived)))
        (should (string-match-p "Looked again\\." (lispm-test--transcript revived)))))
    ;; A response streams into a session that stays unmodified meanwhile.
    (let* ((default-directory project)
           (agent (lispm-start))
           (file (buffer-file-name (lispm-agent-buffer agent))))
      (setq lispm-test--scripts `((,(lispm-agent-name agent) hang)))
      (lispm-tell agent "think")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'requesting))))
      (lispm--live-insert agent 'text "half an answer")
      (should (string-match-p "half an answer" (lispm-test--transcript agent)))
      (should-not (buffer-modified-p (lispm-agent-buffer agent)))
      (lispm--save-sessions)
      (should-not (string-match-p "half an answer" (lispm--file-text file))))))

(ert-deftest lispm-test-delegation-outlives-emacs ()
  "A helper's reply settles its caller's call after Emacs restarted.
Opening the caller's session opens the helper's again, which goes on
with the request, bounded still by its caller."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes." :policy '((write . auto)))
    (let* ((default-directory project)
           (boss (lispm-start))
           (boss-file (buffer-file-name (lispm-agent-buffer boss))))
      (setf (lispm-agent-policy boss) '((write . ask)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "tool_use"
                                                   (lispm-test--tool "t1" "ask_agent"
                                                                     '((agent . "reviewer")
                                                                       (request . "Looks good?")))))
              (reviewer@project hang)))
      (lispm-tell boss "get a review")
      (should (lispm-test--wait (lambda () (and (lispm-get-agent "reviewer@project" t)
                                                (eq (lispm-agent-state (lispm-get-agent "reviewer@project"))
                                                    'requesting)))))
      (let* ((helper (lispm-get-agent "reviewer@project"))
             (helper-id (lispm-agent-id helper)))
        ;; The call records whom it asked, the helper its caller's ceiling.
        (should (string-match-p (concat ":ASKED: " (regexp-quote helper-id))
                                (lispm-test--transcript boss)))
        (should (string-match-p "^#\\+lispm_ceiling: .*(write \\. ask)" (lispm-test--transcript helper)))
        (lispm-test--crash-all)
        (setq lispm-test--requests nil
              lispm-test--scripts
              `((reviewer@project ,(lispm-test--msg "end_turn" (lispm-test--text "Yes, ship it.")))
                (assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Approved.")))))
        (let ((revived (lispm-resume-file boss-file)))
          (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p revived))
                                                    (= (length lispm-test--requests) 2)))))
          (should (string-match-p "reviewer@project replied:\nYes, ship it\\."
                                  (format "%S" (alist-get 'messages (car lispm-test--requests)))))
          ;; The helper, bounded as before, retired once it had replied.
          (should (lispm-test--wait (lambda () (null (cdr (lispm-agents))))))
          (let ((again (lispm-resume-session helper-id)))
            (should (eq (lispm-policy-action again 'write) 'ask))))))))

(ert-deftest lispm-test-standing-agent-takes-up ()
  "A standing agent declared again, as Emacs starts, handles what was queued for it.
`lispm-resume-agents' resumes the other sessions Emacs stopped in."
  (lispm-test-env
    (let ((keeper (lispm-defagent keeper "Keeps going." :root project))
          (other (let ((default-directory project)) (lispm-start))))
      (setq lispm-test--scripts '((keeper hang) (assistant@project hang)))
      (lispm-tell keeper "first")
      (lispm-tell other "work")
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state keeper) 'requesting)
                                                (eq (lispm-agent-state other) 'requesting)))))
      (lispm-tell keeper "tick" :event "timer")
      (lispm-test--crash-all)
      (setq lispm-test--scripts
            `((keeper ,(lispm-test--msg "end_turn" (lispm-test--text "Ticked.")))
              (assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Worked.")))))
      (let ((keeper (lispm-defagent keeper "Keeps going." :root project)))
        ;; Its own turn waits for the user; the queued event comes after.
        (should (lispm-test--wait (lambda () (lispm-agent-condition keeper))))
        (should (equal (mapcar (lambda (e) (plist-get e :event)) (lispm-agent-inbox keeper)) '("timer")))
        (lispm-stop-at-condition keeper)
        (should (lispm-test--wait (lispm-test--idle-p keeper)))
        (should (string-match-p "Ticked\\." (lispm-test--transcript keeper))))
      (should (equal (mapcar #'lispm-agent-name (lispm-resume-agents)) '(assistant@project)))
      (should (eq (plist-get (lispm-agent-condition (lispm-get-agent 'assistant@project)) :type)
                  'interrupted)))))

(ert-deftest lispm-test-journal-outlives-emacs ()
  "In a git project, a turn's changes are reverted after Emacs restarted.
Snapshots of the work tree cover what a shell command changed, and
live in LispM's own repository, not the project's."
  (lispm-test-env
    (lispm-test--git-repo project)
    (lispm-test--write (expand-file-name "a.txt" project) "a\n")
    (let* ((default-directory project)
           (agent (lispm-start))
           (file (buffer-file-name (lispm-agent-buffer agent))))
      (setf (lispm-agent-policy agent) '((exec . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell"
                                                   '((why . "w")
                                                     (command . "printf 'A\\n' > a.txt; printf 'new\\n' > b.txt"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Changed."))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t2" "run_shell"
                                                   '((why . "w") (command . "printf 'AA\\n' > a.txt"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Changed again.")))))
      ;; The second turn starts as the first ends: its snapshot waits.
      (lispm-tell agent "change things")
      (lispm-tell agent "change a again")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p agent))
                                                (not (lispm-journal-busy-p agent))))))
      (should (equal (lispm-test--file project "b.txt") "new\n"))
      (should (= (length (lispm-journal-records agent)) 2))
      ;; The project's repository has no ref of LispM's.
      (should-not (string-match-p "turns" (lispm-test--git project "for-each-ref")))
      (lispm-test--crash-all)
      (let ((revived (lispm-resume-file file)))
        (should-not (lispm-agent-journal revived))
        (lispm-turn-diff revived)
        (should (string-match-p "^-A\n\\+AA$" (with-current-buffer "*lispm-diff*" (buffer-string))))
        (lispm-revert-turn revived)
        (should (equal (lispm-test--file project "a.txt") "A\n"))
        (lispm-revert-turn revived)
        (should (equal (lispm-test--file project "a.txt") "a\n"))
        (should-not (file-exists-p (expand-file-name "b.txt" project)))
        (should-not (lispm-journal-records revived))
        (should (string-match-p "The user reverted the changes you made in assistant@project turn 1"
                                (lispm-test--transcript revived)))))))

(ert-deftest lispm-test-journal-snapshot-fails-quietly ()
  "A snapshot whose project goes while it is taken fails, and says so to its callback.
No error escapes into the process sentinels, where it would end a
batch Emacs, as on CI when a test's directory went first."
  (lispm-test-env
    (let* ((top (file-name-as-directory (expand-file-name "gone/" project)))
           (dir (lispm-journal--directory project))
           (result 'pending))
      (make-directory top t)
      (lispm-test--git-repo top)
      (lispm-journal--snapshot top dir :index (expand-file-name "index-test" dir)
                               :ref "refs/turns/test/1" :message "m" :author "test"
                               :callback (lambda (tree commit) (setq result (list tree commit))))
      (delete-directory top t)
      (should (lispm-test--wait (lambda () (not (eq result 'pending)))))
      (should (equal result '(nil nil))))))

;;;; Conditions and restarts

(defun lispm-test--failure (status type message)
  "A scripted response failing with STATUS, TYPE and MESSAGE."
  `(error :status ,status :type ,type :message ,message))

(defmacro lispm-test-with-conditions (&rest body)
  "Run BODY in `lispm-test-env', failed requests not retried, no handlers."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-api-max-retries 0)
           (lispm-condition-handlers nil)
           (lispm-unattended-condition-handlers nil))
       ,@body)))

(defun lispm-test--condition-id (agent)
  "Id of the condition AGENT waits at, once it does."
  (should (lispm-test--wait (lambda () (lispm-agent-condition agent))))
  (plist-get (lispm-agent-condition agent) :id))

(ert-deftest lispm-test-condition-retry ()
  "A failed request stops the turn at a condition, saved, never sent; retrying goes on."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Hello.")))))
      (lispm-tell agent "hi" :callback (lambda (r) (setq reply r)))
      (let ((id (lispm-test--condition-id agent))
            (condition (lispm-agent-condition agent)))
        (should (equal id "c1"))
        (should (eq (lispm-agent-state agent) 'waiting))
        (should (eq (plist-get condition :type) 'request-failed))
        (should (equal (plist-get condition :restarts) '(retry use-model abort)))
        (let ((expected (concat "^\\* WAITING Condition: Request failed\n:PROPERTIES:\n"
                                ":CONDITION: request-failed\n:CONDITION_ID: c1\n"
                                ":RESTARTS: retry use-model abort\n:END:\n"
                                "The request failed (HTTP 529): overloaded_error: Overloaded\n\\'")))
          (should (string-match-p expected (lispm-test--transcript agent)))
          ;; Saved: Emacs stopping now would not lose it.
          (should (string-match-p expected (lispm-test--disk (buffer-file-name (lispm-agent-buffer agent))))))
        (should-not reply)
        (lispm-restart agent id 'retry))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Hello."))
      (should (string-match-p "^\\* DONE Condition: Request failed\n:PROPERTIES:\n\\(?:.*\n\\)*:RESTART: retry\n:END:\n"
                              (lispm-test--transcript agent)))
      ;; The retry sent what the failed request would have, and no condition.
      (should (= (length lispm-test--requests) 2))
      (should (equal (car lispm-test--requests) (cadr lispm-test--requests)))
      (should-not (string-match-p "Condition: " (format "%S" (car lispm-test--requests)))))))

(ert-deftest lispm-test-condition-abort ()
  "Stopping at a condition ends the turn as a failed turn always ended."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts `((tester ,(lispm-test--failure 500 "api_error" "Boom"))))
      (lispm-tell agent "hi" :callback (lambda (r) (setq reply r)))
      (lispm-restart agent (lispm-test--condition-id agent) 'abort)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "The request failed (HTTP 500): api_error: Boom"))
      (let ((text (lispm-test--transcript agent)))
        (should (string-match-p "^\\* User\nhi\n\\* Note\nThe request failed (HTTP 500): api_error: Boom\n\\* User\n\\'"
                                text))
        (should-not (string-match-p "Condition" text))))))

(ert-deftest lispm-test-condition-step-limit ()
  "At the step limit the turn waits; continuing gives it as many steps again."
  (lispm-test-with-conditions
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello\n"))
    (let ((lispm-max-steps 1)
          (agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "read" :callback (lambda (r) (setq reply r)))
      (let ((id (lispm-test--condition-id agent)))
        (should (eq (plist-get (lispm-agent-condition agent) :type) 'step-limit))
        (should (equal (plist-get (lispm-agent-condition agent) :restarts) '(continue abort)))
        (should (equal (plist-get (lispm-agent-condition agent) :message)
                       "Stopped after 1 model requests (`lispm-max-steps')."))
        (should (= (length lispm-test--requests) 1))
        (lispm-restart agent id 'continue))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done.")))))

(ert-deftest lispm-test-condition-use-model ()
  "Retrying with another model sends the request to it."
  (lispm-test-with-conditions
    (let ((lispm-backends (cons '(other :api anthropic :url "http://fake.invalid" :model "other-model")
                                lispm-backends))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--failure 404 "not_found_error" "model: test-model")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Hello.")))))
      (lispm-tell agent "hi")
      (lispm-test--condition-id agent)
      ;; Asked for interactively: the backend, then the model, its default the backend's.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "other"))
                ((symbol-function 'read-string) (lambda (_prompt _initial _history default) default)))
        (lispm-choose-restart-named agent 'use-model))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (eq (lispm-agent-backend agent) 'other))
      (should (equal (alist-get 'model (car lispm-test--requests)) "other-model")))))

(ert-deftest lispm-test-condition-org-and-review ()
  "In org a condition is settled like a call: by its TODO state, or with C-c C-y."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "First.")))))
      (lispm-tell agent "one")
      (lispm-test--condition-id agent)
      ;; DONE takes the default restart, retry.
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (re-search-forward "^\\* WAITING Condition")
        (org-todo "DONE"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\*\\* Reply\nFirst\\.$" (lispm-test--transcript agent)))
      (should (string-match-p "^\\* DONE Condition: Request failed\n\\(?:.*\n\\)*:RESTART: retry\n"
                              (lispm-test--transcript agent)))
      ;; DENIED stops there.
      (push `(tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")) lispm-test--scripts)
      (lispm-tell agent "two")
      (lispm-test--condition-id agent)
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (re-search-forward "^\\* WAITING Condition")
        (org-todo "DENIED"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\* User\ntwo\n\\* Note\nThe request failed" (lispm-test--transcript agent)))
      ;; C-c C-y offers the restarts.
      (push `(tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                     ,(lispm-test--msg "end_turn" (lispm-test--text "Third.")))
            lispm-test--scripts)
      (lispm-tell agent "three")
      (lispm-test--condition-id agent)
      (let (offered)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &rest _)
                     (setq offered (mapcar #'cadr choices))
                     (car choices))))
          (with-current-buffer (lispm-agent-buffer agent) (lispm-review)))
        (should (equal offered '("retry" "use-model" "abort" "later"))))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\*\\* Reply\nThird\\.$" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-condition-handlers ()
  "A handler chooses a restart without asking, as often as it may, then the user does."
  (lispm-test-with-conditions
    (let* ((lispm-condition-handlers '((request-failed retry :after 0.05)))
           (agent (lispm-defagent tester "Test agent." :root project))
           (seen nil)
           (note (lambda (_agent c) (push (plist-get c :id) seen))))
      (add-hook 'lispm-condition-functions note)
      (unwind-protect
          (progn
            (setq lispm-test--scripts
                  `((tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                            ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                            ,(lispm-test--msg "end_turn" (lispm-test--text "Hello.")))))
            (lispm-tell agent "hi")
            ;; The first failure is retried by the handler; the second waits.
            (should (lispm-test--wait (lambda () (equal (plist-get (lispm-agent-condition agent) :id) "c2"))))
            (should (equal seen '("c2" "c1")))
            (should (string-match-p "^\\* DONE Condition: Request failed\n\\(?:.*\n\\)*:RESTART: retry"
                                    (lispm-test--transcript agent)))
            (sleep-for 0.1)
            (should (equal (plist-get (lispm-agent-condition agent) :id) "c2"))
            (lispm-restart agent "c2" 'retry)
            (should (lispm-test--wait (lispm-test--idle-p agent))))
        (remove-hook 'lispm-condition-functions note)))))

(ert-deftest lispm-test-condition-unattended ()
  "Turns nobody started from the keyboard go by their own handlers, and end."
  (lispm-test-with-conditions
    (lispm-defrole reviewer "Reviews changes.")
    (let ((lispm-unattended-condition-handlers
           '((request-failed retry :after 0.05 :times 1 :then abort)))
          (boss (lispm-defagent boss :root project)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "reviewer") (request . "Looks good?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "No review.")))
              (reviewer@project ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                                ,(lispm-test--failure 529 "overloaded_error" "Overloaded"))))
      (lispm-tell boss "get a review")
      ;; The helper's failures are retried once, then its turn stops, and
      ;; the caller goes on with the failure.
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                (null (cdr (lispm-agents)))))))
      (should (= (cl-count-if (lambda (b) (string-match-p "You are \"reviewer" (alist-get 'system b)))
                              lispm-test--requests)
                 2))
      (should (string-match-p "reviewer@project replied:\nThe request failed (HTTP 529)"
                              (format "%S" (alist-get 'messages (car lispm-test--requests)))))
      ;; The user's own turn waits for them, as before.
      (setq lispm-test--scripts `((boss ,(lispm-test--failure 529 "overloaded_error" "Overloaded"))))
      (lispm-tell boss "again")
      (lispm-test--condition-id boss)
      (sleep-for 0.1)
      (should (lispm-agent-condition boss)))))

(ert-deftest lispm-test-condition-cut-short ()
  "A response cut short by the output limit is asked for again with more room."
  (lispm-test-with-conditions
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello\n"))
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "max_tokens" (lispm-test--text "Partial")
                                        (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Whole.")))))
      (lispm-tell agent "go" :callback (lambda (r) (setq reply r)))
      (let ((id (lispm-test--condition-id agent)))
        (should (eq (plist-get (lispm-agent-condition agent) :type) 'output-limit))
        (should (equal (plist-get (lispm-agent-condition agent) :restarts) '(retry-larger keep abort)))
        ;; Its call was not run.
        (should (string-match-p "^\\*\\* FAILED read_file" (lispm-test--transcript agent)))
        (lispm-restart agent id 'retry-larger))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Whole."))
      (let ((request (car lispm-test--requests)))
        (should (= (alist-get 'max_tokens request) (* 2 lispm-max-tokens)))
        ;; The cut response is no longer sent, and stays on record.
        (should-not (string-match-p "Partial" (format "%S" (alist-get 'messages request)))))
      (should (string-match-p "^\\* Assistant :ignore:\n\\*\\* Reply\nPartial" (lispm-test--transcript agent)))
      ;; The next turn has the usual limit again.
      (push `(tester ,(lispm-test--msg "end_turn" (lispm-test--text "Again."))) lispm-test--scripts)
      (lispm-tell agent "more")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (= (alist-get 'max_tokens (car lispm-test--requests)) lispm-max-tokens)))))

(ert-deftest lispm-test-condition-cut-short-kept ()
  "A response cut short can be kept, the model told to go on, or left as it is."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "max_tokens" (lispm-test--text "Partial"))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Rest.")))))
      (lispm-tell agent "go")
      (lispm-restart agent (lispm-test--condition-id agent) 'keep)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((sent (format "%S" (alist-get 'messages (car lispm-test--requests)))))
        (should (string-match-p "Partial" sent))
        (should (string-match-p "\\[From LispM, for the user\\]\nYour last response was cut short" sent)))
      ;; The chat shows who said it.
      (should (string-match-p "^LispM Your last response was cut short"
                              (mapconcat (lambda (i) (concat (or (plist-get i :from) "") " "
                                                             (or (plist-get i :text) "")))
                                         (lispm-chat-items (lispm-agent-buffer agent)) "\n")))
      ;; Left as it is: the transcript a turn cut short always had.
      (setq lispm-test--scripts `((tester ,(lispm-test--msg "max_tokens" (lispm-test--text "Again")))))
      (lispm-tell agent "go on")
      (lispm-restart agent (lispm-test--condition-id agent) 'abort)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\* Assistant\n\\*\\* Reply\nAgain\n\\* Note\nThe response hit the output limit (`lispm-max-tokens').\n\\* User\n\\'"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-condition-refused ()
  "A refused response is asked for again; one with nothing in it drops nothing else."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "First."))
                      ,(lispm-test--msg "refusal")
                      ,(lispm-test--msg "refusal" (lispm-test--text "No."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Fine.")))))
      (lispm-tell agent "one")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-tell agent "two" :callback (lambda (r) (setq reply r)))
      (lispm-test--condition-id agent)
      (should (eq (plist-get (lispm-agent-condition agent) :type) 'refused))
      (should (equal (plist-get (lispm-agent-condition agent) :message) "The model declined to continue."))
      (lispm-restart agent (plist-get (lispm-agent-condition agent) :id) 'retry)
      ;; The empty refusal left nothing to drop: the earlier response stays.
      (should (string-match-p "^\\* Assistant\n\\*\\* Reply\nFirst\\." (lispm-test--transcript agent)))
      (lispm-restart agent (lispm-test--condition-id agent) 'retry)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Fine."))
      (should (string-match-p "^\\* Assistant :ignore:\n\\*\\* Reply\nNo\\." (lispm-test--transcript agent)))
      (should-not (string-match-p "\"No\\.\"" (format "%S" (alist-get 'messages (car lispm-test--requests))))))))

(ert-deftest lispm-test-condition-paused ()
  "A paused response goes on by itself, by default."
  (lispm-test-with-conditions
    (let ((lispm-condition-handlers (eval (car (get 'lispm-condition-handlers 'standard-value)) t))
          (agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "pause_turn" (lispm-test--text "Working"))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "go" :callback (lambda (r) (setq reply r)))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done."))
      ;; The paused response went back as it was: nothing after it.
      (let ((messages (alist-get 'messages (car lispm-test--requests))))
        (should (equal (alist-get 'role (aref messages (1- (length messages)))) "assistant"))
        (should (string-match-p "Working" (format "%S" (aref messages (1- (length messages)))))))
      (should (string-match-p "^\\* DONE Condition: Response paused\n\\(?:.*\n\\)*:RESTART: continue\n"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-condition-context-full ()
  "A full context window is handed over in the middle of the turn, which goes on."
  (lispm-test-with-conditions
    (let* ((default-directory project)
           (agent (lispm-start))
           reply)
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "model_context_window_exceeded" (lispm-test--text "Long"))
               ,(lispm-test--msg "end_turn" (lispm-test--text "The summary."))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "go" :callback (lambda (r) (setq reply r)))
      (let ((old (lispm-agent-buffer agent)))
        (should (eq (progn (lispm-test--condition-id agent) (plist-get (lispm-agent-condition agent) :type))
                    'context-full))
        (should (equal (plist-get (lispm-agent-condition agent) :restarts) '(compact abort)))
        (lispm-restart agent (plist-get (lispm-agent-condition agent) :id) 'compact)
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should (equal reply "Done."))
        (should-not (eq (lispm-agent-buffer agent) old))
        ;; The summary was asked for without the response cut short.
        (should-not (string-match-p "Long" (format "%S" (alist-get 'messages (cadr lispm-test--requests)))))
        (let ((text (lispm-test--transcript agent)))
          ;; Told, as a handover between turns does first, to save what
          ;; should outlive the session.
          (should (string-match-p "^\\* Summary\nThe summary\\.\n\\* From LispM\nYour context filled up in the middle of this turn\\(?:.*\n\\)*\\* Assistant\n\\*\\* Reply\nDone\\.\n\\* User\n\\'" text)))))))

(ert-deftest lispm-test-condition-compaction-failed ()
  "A handover that fails waits; skipping it lets the input go on in the long session."
  (lispm-test-with-conditions
    (let ((lispm-compact-threshold 1)
          (lispm-compact-save-memory nil)
          (agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "One."))
                      ,(lispm-test--failure 500 "api_error" "Boom")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Two.")))))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-tell agent "second" :callback (lambda (r) (setq reply r)))
      (let ((id (lispm-test--condition-id agent)))
        (should (eq (plist-get (lispm-agent-condition agent) :type) 'compaction-failed))
        (should (equal (plist-get (lispm-agent-condition agent) :restarts) '(retry skip abort)))
        (should (equal (plist-get (lispm-agent-condition agent) :message)
                       "Compaction failed: api_error: Boom"))
        (lispm-restart agent id 'skip))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Two."))
      (should (string-match-p "^\\* DONE Condition: Compaction failed\n\\(?:.*\n\\)*:RESTART: skip\n:END:\nCompaction failed: api_error: Boom\n\\* User\nsecond\n"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-condition-compaction-retried ()
  "A failed handover is tried again; in the middle of a turn, the turn goes on after it."
  (lispm-test-with-conditions
    ;; Between turns: retried, it hands over, and the input goes on there.
    (let ((lispm-compact-threshold 1)
          (lispm-compact-save-memory nil)
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "One."))
                      ,(lispm-test--failure 500 "api_error" "Boom")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "The summary."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Two.")))))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-tell agent "second")
      (lispm-restart agent (lispm-test--condition-id agent) 'retry)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\* Summary\nThe summary\\.\n\\* User\nsecond\n\\* Assistant\n\\*\\* Reply\nTwo\\."
                              (lispm-test--transcript agent))))
    ;; In the middle of a turn: the condition says so, and skipping it
    ;; goes on with the turn in the long session.
    (let* ((default-directory project)
           (agent (lispm-start))
           reply)
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "model_context_window_exceeded" (lispm-test--text "Long"))
               ,(lispm-test--failure 500 "api_error" "Boom")
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "go" :callback (lambda (r) (setq reply r)))
      (lispm-restart agent (lispm-test--condition-id agent) 'compact)
      (should (lispm-test--wait (lambda () (eq (plist-get (lispm-agent-condition agent) :type)
                                               'compaction-failed))))
      (should (plist-get (lispm-agent-condition agent) :resume))
      (should (string-match-p ":RESUME: t\n" (lispm-test--transcript agent)))
      (lispm-restart agent (plist-get (lispm-agent-condition agent) :id) 'skip)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done.")))))

(ert-deftest lispm-test-condition-raised-limit-refused ()
  "A raised output limit the server refuses goes back, so that retrying can work."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project))
          reply)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "max_tokens" (lispm-test--text "Partial"))
                      ,(lispm-test--failure 400 "invalid_request_error" "max_tokens: too large")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Whole.")))))
      (lispm-tell agent "go" :callback (lambda (r) (setq reply r)))
      (lispm-restart agent (lispm-test--condition-id agent) 'retry-larger)
      (should (lispm-test--wait (lambda () (eq (plist-get (lispm-agent-condition agent) :type)
                                               'request-failed))))
      (should (string-match-p (format "The output limit raised to %d for this turn is back to %d\\."
                                      (* 2 lispm-max-tokens) lispm-max-tokens)
                              (plist-get (lispm-agent-condition agent) :message)))
      (lispm-restart agent (plist-get (lispm-agent-condition agent) :id) 'retry)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Whole."))
      (should (= (alist-get 'max_tokens (car lispm-test--requests)) lispm-max-tokens))
      ;; The chat says the cut response was there.
      (should (string-match-p "Left out of the conversation: Partial"
                              (mapconcat (lambda (i) (or (plist-get i :text) ""))
                                         (lispm-chat-items (lispm-agent-buffer agent)) "\n"))))))

(ert-deftest lispm-test-condition-compaction-stopped ()
  "Stopping at a failed handover drops the input waiting for it; nothing ends a turn."
  (lispm-test-with-conditions
    (let ((lispm-compact-threshold 1)
          (lispm-compact-save-memory nil)
          (agent (lispm-defagent tester "Test agent." :root project))
          told)
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "One."))
                      ,(lispm-test--failure 500 "api_error" "Boom")
                      ,(lispm-test--failure 500 "api_error" "Boom again"))))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      ;; Denied in org: the input is dropped, its sender told.
      (lispm-tell agent "second" :callback (lambda (r) (setq told r)))
      (lispm-test--condition-id agent)
      (should (eq (lispm--stop-restart (lispm-agent-condition agent)) 'abort))
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (point-min))
        (re-search-forward "^\\* WAITING Condition")
        (org-todo "DENIED"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "stopped before handling this: Compaction failed: api_error: Boom" told))
      (let ((text (lispm-test--transcript agent)))
        (should-not (string-match-p "second" text))
        (should-not (string-match-p "^\\* Note" text))
        (should (string-match-p "^\\* Event: compaction\nCompaction failed: api_error: Boom\n\\* User\n\\'" text)))
      ;; Interrupted: no turn is ended either.
      (lispm-tell agent "third")
      (lispm-test--condition-id agent)
      (lispm-abort agent)
      (should (funcall (lispm-test--idle-p agent)))
      (let ((text (lispm-test--transcript agent)))
        (should-not (string-match-p "^\\* Note" text))
        (should (string-match-p "^\\* Event: stopped\nCompaction failed: api_error: Boom again\nAborted by the user\\.\n\\* User\n\\'"
                                text))))))

(ert-deftest lispm-test-condition-unattended-cut-short ()
  "Unattended, a response cut short is asked for again once, larger, then the turn stops."
  (lispm-test-with-conditions
    (let ((lispm-unattended-condition-handlers
           (eval (car (get 'lispm-unattended-condition-handlers 'standard-value)) t))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "max_tokens" (lispm-test--text "Cut"))
                      ,(lispm-test--msg "max_tokens" (lispm-test--text "Cut again")))))
      (lispm-tell agent "Look at your tasks." :event "every hour")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (= (length lispm-test--requests) 2))
      (should (string-match-p "\\* Note\nThe response hit the output limit" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-eval-backtrace ()
  "An error in evaluated Lisp comes back with the backtrace of that code alone."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)))
      (pcase-let ((`(,text . ,errorp)
                   (lispm-test--call agent "eval_elisp"
                                     '((code . "(defun lispm-test-f (x) (car x)) (defun lispm-test-g () (lispm-test-f 3)) (lispm-test-g)")))))
        (should errorp)
        (should (equal text (concat "Wrong type argument: listp, 3\n\nBacktrace, innermost call first:\n"
                                    "  (car 3)\n  (lispm-test-f 3)\n  (lispm-test-g)"))))
      ;; An error the code handles itself is not one, and is not printed.
      (let ((print (symbol-function 'lispm-tools--backtrace-text))
            (printed 0))
        (cl-letf (((symbol-function 'lispm-tools--backtrace-text)
                   (lambda (frames) (cl-incf printed) (funcall print frames))))
          (should (equal (lispm-test--call agent "eval_elisp"
                                           '((code . "(dotimes (_ 50) (ignore-errors (car 3))) (condition-case nil (car 3) (error 'caught))")))
                         '("=> caught" . nil))))
        (should (= printed 0)))
      ;; Past 20 frames, the rest is left out.
      (let ((text (car (lispm-test--call agent "eval_elisp"
                                         '((code . "(defun lispm-test-r (n) (if (= n 0) (car 3) (lispm-test-r (1- n)))) (lispm-test-r 30)"))))))
        (should (string-match "Backtrace, innermost call first:\n" text))
        (let ((lines (split-string (substring text (match-end 0)) "\n")))
          (should (= (length lines) 21))
          (should (equal (car lines) "  (car 3)"))
          (should (equal (car (last lines)) "  ..."))))
      ;; Turned off, an error comes back as it is.
      (let ((lispm-eval-backtrace nil))
        (should (equal (lispm-test--call agent "eval_elisp" '((code . "(lispm-test-g)")))
                       '("Wrong type argument: listp, 3" . t)))))))

(defmacro lispm-test-with-stopped-call (&rest body)
  "Run BODY with an agent `tester' whose read_file call of a missing file stopped.
BODY can use `agent', `id' (the call's) and `reply' (the turn's, once it ends)."
  (declare (indent 0))
  `(lispm-test-with-conditions
     (let ((lispm-debug-tool-errors '("read_file"))
           (agent (lispm-defagent tester "Test agent." :root project))
           (reply nil))
       (setq lispm-test--scripts
             `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
       (lispm-tell agent "read" :callback (lambda (r) (setq reply r)))
       (let ((id (progn (should (lispm-test--wait (lambda () (lispm--asked-condition agent))))
                        (plist-get (lispm--asked-condition agent) :id))))
         (ignore id)
         ,@body))))

(ert-deftest lispm-test-tool-error-stops ()
  "A failure `lispm-debug-tool-errors' covers stops the call, recorded on its heading."
  (lispm-test-with-stopped-call
    (let ((condition (lispm--asked-condition agent)))
      (should (equal id "t1"))
      (should (eq (plist-get condition :type) 'tool-error))
      (should (equal (plist-get condition :restarts) '(report retry use-value abort)))
      (should (string-match-p "\\`read_file failed: No such file: " (plist-get condition :message)))
      (should (eq (lispm-agent-state agent) 'waiting))
      (should (equal (lispm-agent-status agent) "1 call stopped at an error"))
      (should (string-match-p "^\\*\\* WAITING read_file  a\\.txt\n:PROPERTIES:\n:TOOL_ID:  t1\n:TOOL:     read_file\n:CONDITION: tool-error\n:END:\n\\(?:.*\n\\)*\\*\\*\\* Stopped\nNo such file: "
                              (lispm-test--transcript agent)))
      ;; Nothing was sent: the model is waiting for the call's result.
      (should (= (length lispm-test--requests) 1))
      ;; Reported, the error is the result, as it would have been.
      (lispm-restart agent id 'report)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done."))
      (let ((text (lispm-test--transcript agent)))
        (should (string-match-p "^\\*\\* FAILED read_file  a\\.txt\n" text))
        (should-not (string-match-p ":CONDITION:" text))
        (should (string-match-p "^\\*\\*\\* Stopped: report\n" text)))
      (should (string-match-p "No such file" (format "%S" (alist-get 'messages (car lispm-test--requests))))))))

(ert-deftest lispm-test-tool-error-retry-and-value ()
  "A stopped call can be run again once its cause is fixed, or given a result."
  (lispm-test-with-stopped-call
    (with-temp-file (expand-file-name "a.txt" project) (insert "found\n"))
    (lispm-restart agent id 'retry)
    (should (lispm-test--wait (lispm-test--idle-p agent)))
    (should (string-match-p "^\\*\\* DONE read_file  a\\.txt\n" (lispm-test--transcript agent)))
    (should (string-match-p "found" (format "%S" (alist-get 'messages (car lispm-test--requests))))))
  (lispm-test-with-stopped-call
    (should-error (lispm-restart agent id 'use-value) :type 'user-error)
    ;; No result is not one.
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " ")))
      (should-error (lispm-choose-restart-named agent 'use-value) :type 'user-error))
    (should (lispm--call-condition agent id))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "It says hello.")))
      (lispm-choose-restart-named agent 'use-value))
    (should (lispm-test--wait (lispm-test--idle-p agent)))
    (should (string-match-p "^\\*\\* DONE read_file  a\\.txt\n" (lispm-test--transcript agent)))
    (should (string-match-p "It says hello\\." (format "%S" (alist-get 'messages (car lispm-test--requests)))))))

(ert-deftest lispm-test-tool-error-abort-and-org ()
  "Stopping at a stopped call ends the turn, inputs kept; in org, its TODO state settles it."
  (lispm-test-with-stopped-call
    (lispm-tell agent "next")
    (should (lispm-agent-inbox agent))
    (with-current-buffer (lispm-agent-buffer agent)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* WAITING read_file")
      (org-todo "DENIED"))
    (should (lispm-test--wait (lambda () (string-match-p "^\\* User\nnext\n" (lispm-test--transcript agent)))))
    (should (string-match-p "^\\*\\* FAILED read_file  a\\.txt\n\\(?:.*\n\\)*\\* Note\nread_file failed: No such file"
                            (lispm-test--transcript agent)))
    (should (string-match-p "read_file failed" reply)))
  (lispm-test-with-stopped-call
    (with-current-buffer (lispm-agent-buffer agent)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* WAITING read_file")
      (org-todo "DONE"))
    (should (lispm-test--wait (lispm-test--idle-p agent)))
    (should (equal reply "Done."))
    (should (string-match-p "^\\*\\*\\* Stopped: report\n" (lispm-test--transcript agent)))))

(ert-deftest lispm-test-tool-error-resumed ()
  "A call stopped when Emacs stopped is stopped again when the session is opened."
  (lispm-test-with-conditions
    (let* ((lispm-debug-tool-errors t)
           (default-directory project)
           (agent (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file" '((path . "a.txt")))))))
      (lispm-tell agent "read")
      (should (lispm-test--wait (lambda () (lispm--asked-condition agent))))
      (let ((revived (lispm-resume-file (lispm-test--crash agent))))
        (should (eq (lispm-agent-state revived) 'waiting))
        (should (eq (plist-get (lispm--asked-condition revived) :type) 'tool-error))
        (should (string-match-p "No such file" (plist-get (lispm--asked-condition revived) :error)))
        (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Done."))) lispm-test--scripts)
        (lispm-restart revived "t1" 'report)
        (should (lispm-test--wait (lispm-test--idle-p revived)))))))

(ert-deftest lispm-test-tool-error-unattended-and-uncovered ()
  "Unattended, a stopped call reports its error; uncovered tools and C-g never stop."
  (lispm-test-with-conditions
    (let ((lispm-debug-tool-errors t)
          (lispm-unattended-condition-handlers '((tool-error report)))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "read" :event "every hour")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\*\\*\\* Stopped: report\n" (lispm-test--transcript agent)))))
  (lispm-test-with-conditions
    (let ((lispm-debug-tool-errors '("eval_elisp"))
          (agent (lispm-defagent tester "Test agent." :root project :policy '((exec . auto)))))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "read_file" '((path . "a.txt")))
                                        (lispm-test--tool "t2" "eval_elisp" '((why . "w") (code . "(signal 'quit nil)"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "go")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should-not (string-match-p "Stopped" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-chat-tool-error ()
  "The chat shows a stopped call's error and asks how to go on."
  (lispm-test-with-stopped-call
    (let ((chat (lispm-chat agent)))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "⏺ Read(a\\.txt)\n  ⎿  No such file: " text))
        (should (string-match-p "│ Call failed +│" text))
        (should (string-match-p "│ ❯ 1\\. Tell the model it failed +│\n│   2\\. Run it again +│\n│   3\\. Give the model a result of your own +│\n│   4\\. Stop here +│"
                                text)))
      (lispm-test--chat-key chat "1")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal reply "Done.")))))

(defun lispm-test--count (regexp string)
  "How many times REGEXP matches in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (how-many regexp)))

(ert-deftest lispm-test-tool-error-each-call ()
  "Handlers count for each stopped call; aborting the turn settles those still stopped."
  ;; Unattended, each failed call is tried again once, then reported.
  (lispm-test-with-conditions
    (let ((lispm-debug-tool-errors t)
          (lispm-unattended-condition-handlers '((tool-error retry :then report)))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "read_file" '((path . "a.txt")))
                                        (lispm-test--tool "t2" "read_file" '((path . "b.txt"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "read" :event "every hour")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((text (lispm-test--transcript agent)))
        (should (= (lispm-test--count "^\\*\\*\\* Stopped: retry$" text) 2))
        (should (= (lispm-test--count "^\\*\\*\\* Stopped: report$" text) 2)))))
  (lispm-test-with-stopped-call
    (lispm-abort agent)
    (should (lispm-test--wait (lispm-test--idle-p agent)))
    (let ((text (lispm-test--transcript agent)))
      (should (string-match-p "^\\*\\* FAILED read_file  a\\.txt\n" text))
      (should-not (string-match-p ":CONDITION:" text))
      (should (string-match-p "^\\*\\*\\* Stopped: abort\n" text)))))

(defmacro lispm-test-with-stopped-write (&rest body)
  "Run BODY with an agent `tester' whose approved write t1 stopped, and t2 awaits approval.
BODY can use `agent'."
  (declare (indent 0))
  `(lispm-test-with-conditions
     (let ((lispm-debug-tool-errors '("write_file"))
           (agent (lispm-defagent tester "Test agent." :root project)))
       ;; A file where a directory should be: the write fails.
       (with-temp-file (expand-file-name "a.txt" project) (insert "a\n"))
       (setq lispm-test--scripts
             `((tester ,(lispm-test--msg "tool_use"
                                         (lispm-test--tool "t1" "write_file"
                                                           '((why . "w") (path . "a.txt/x") (content . "x")))
                                         (lispm-test--tool "t2" "write_file"
                                                           '((why . "w") (path . "b.txt") (content . "b"))))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
       (lispm-tell agent "write")
       (should (lispm-test--wait (lambda () (equal (lispm--waiting-calls agent) '("t1" "t2")))))
       (lispm-resolve agent "t1" 'approve)
       (should (lispm-test--wait (lambda () (lispm--call-condition agent "t1"))))
       ,@body)))

(ert-deftest lispm-test-tool-error-not-approval ()
  "A stopped call does not await approval: approving its tool for good leaves it alone."
  (lispm-test-with-stopped-write
    (should (equal (lispm--waiting-calls agent) '("t2")))
    (should (equal (lispm--open-calls agent) '("t1" "t2")))
    (should (equal (lispm-agent-status agent) "1 call stopped at an error, 1 call awaits approval"))
    (let ((chat (lispm-chat agent)))
      ;; The chat asks about the first call: the stopped one.
      (should (string-match-p "│ Call failed +│" (lispm-test--chat-text chat)))
      (should-not (string-match-p "Do you want to write" (lispm-test--chat-text chat)))
      (with-current-buffer (lispm-agent-buffer agent)
        (goto-char (plist-get (gethash "t2" (lispm-agent-pending agent)) :marker))
        (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?a "always")))
                  ((symbol-function 'recenter) #'ignore))
          (lispm-review)))
      (should (lispm-test--wait (lambda () (file-exists-p (expand-file-name "b.txt" project)))))
      (should (lispm--call-condition agent "t1"))
      (should-not (string-match-p "Stopped: " (lispm-test--transcript agent)))
      (lispm-test--chat-key chat "1")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\*\\*\\* Stopped: report\n" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-tool-error-retry-policy ()
  "The user's retry runs a stopped call at once; a handler's, only if the policy would."
  (lispm-test-with-stopped-write
    (lispm-restart agent "t1" 'retry)
    (should (lispm-test--wait (lambda () (string-match-p "^\\*\\*\\* Stopped\n"
                                                         (lispm-test--transcript agent)))))
    (should (lispm--call-condition agent "t1"))
    (should (equal (lispm--waiting-calls agent) '("t2"))))
  (lispm-test-with-stopped-write
    (let ((lispm-condition-handlers '((tool-error retry))))
      ;; Stopped before the handler was set: offer it again.
      (lispm--offer-condition agent (lispm--call-condition agent "t1"))
      (should (lispm-test--wait (lambda () (equal (lispm--waiting-calls agent) '("t1" "t2")))))
      (should-not (lispm--call-condition agent "t1"))
      (should (equal (lispm-agent-status agent) "2 calls await approval"))
      (should (string-match-p "^\\*\\* WAITING write_file  a\\.txt/x\n\\(?:.*\n\\)*\\*\\*\\* Stopped: retry\n"
                              (lispm-test--transcript agent)))
      (lispm-resolve agent "t1" 'deny)
      (lispm-resolve agent "t2" 'deny)
      (should (lispm-test--wait (lispm-test--idle-p agent))))))

(ert-deftest lispm-test-tool-error-images ()
  "A stopped call keeps the images of its failure, for the model once reported."
  (lispm-test-with-conditions
    (let ((lispm-debug-tool-errors '("lispm_test_snap"))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (lispm-register-tool
       (lispm-tool--create :name "lispm_test_snap" :description "Fails, with an image."
                           :input-schema '((type . "object") (properties))
                           :permission 'read :async t
                           :function (lambda (done _input)
                                       (funcall done "Broke." t
                                                (list (lispm-save-image agent (base64-encode-string "png")))))))
      (unwind-protect
          (progn
            (setq lispm-test--scripts
                  `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "lispm_test_snap" nil))
                            ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
            (lispm-tell agent "snap")
            (should (lispm-test--wait (lambda () (lispm--call-condition agent "t1"))))
            (lispm-restart agent "t1" 'report)
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let* ((messages (alist-get 'messages (car lispm-test--requests)))
                   (result (seq-find (lambda (block) (equal (alist-get 'tool_use_id block) "t1"))
                                     (seq-mapcat (lambda (m) (append (alist-get 'content m) nil))
                                                 messages))))
              (should (equal (mapcar (lambda (block) (alist-get 'type block)) (alist-get 'content result))
                             '("text" "text" "image")))
              (should (equal (alist-get 'text (aref (alist-get 'content result) 1)) "Broke."))))
        (remhash "lispm_test_snap" lispm-tools--registry)))))

(ert-deftest lispm-test-condition-going-past ()
  "Writing in a helper's chat, in org, or a command goes past a condition."
  (lispm-test-with-conditions
    (lispm-defrole reviewer "Reviews changes.")
    (let ((boss (lispm-defagent boss :root project)))
      ;; A helper the user writes to stays, to answer them, in its session.
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "reviewer") (request . "Looks good?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "No review.")))
              (reviewer@project ,(lispm-test--failure 500 "api_error" "Boom")
                                ,(lispm-test--msg "end_turn" (lispm-test--text "Fine by me.")))))
      (lispm-tell boss "get a review")
      (should (lispm-test--wait (lambda () (lispm-get-agent 'reviewer@project t))))
      (let* ((helper (lispm-get-agent 'reviewer@project))
             (chat (lispm-chat helper))
             (sessions (length (lispm--session-files-in "projects/*/sessions/*.org"))))
        (lispm-test--condition-id helper)
        (lispm-test--chat-type chat "never mind")
        (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                  (funcall (lispm-test--idle-p helper))
                                                  (string-match-p "Fine by me" (lispm-test--transcript helper))))))
        (should (memq helper (lispm-agents)))
        (should (buffer-live-p chat))
        (should (= (length (lispm--session-files-in "projects/*/sessions/*.org")) sessions))
        (should (string-match-p "^\\* Note\nThe request failed (HTTP 500): api_error: Boom\n\\* User\nnever mind\n"
                                (lispm-test--transcript helper)))))
    ;; In the org interface: under a new * User heading after the condition.
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts `((tester ,(lispm-test--failure 500 "api_error" "Boom")
                                          ,(lispm-test--msg "end_turn" (lispm-test--text "Sure.")))))
      (lispm-tell agent "one")
      (lispm-test--condition-id agent)
      (with-current-buffer (lispm-agent-buffer agent)
        (should-error (lispm-send) :type 'user-error)
        (goto-char (point-max))
        (insert "* User\ntwo instead\n")
        (lispm-send))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\* User\none\n\\* Note\nThe request failed (HTTP 500): api_error: Boom\n\\* User\ntwo instead\n\\* Assistant"
                              (lispm-test--transcript agent)))
      ;; /clear goes past it too.
      (setq lispm-test--scripts `((tester ,(lispm-test--failure 500 "api_error" "Boom"))))
      (lispm-tell agent "three")
      (lispm-test--condition-id agent)
      (let ((old (lispm-agent-buffer agent)))
        (lispm-chat-clear agent "")
        (should-not (lispm-agent-condition agent))
        (should-not (eq (lispm-agent-buffer agent) old))))))

(ert-deftest lispm-test-condition-use-model-recorded ()
  "Another model needs naming, is recorded on the heading, and is kept on reopening."
  (lispm-test-with-conditions
    (let* ((lispm-backends (cons '(other :api anthropic :url "http://fake.invalid" :model "other-model")
                                 lispm-backends))
           (default-directory project)
           (agent (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--failure 404 "not_found_error" "model")
                                 ,(lispm-test--msg "end_turn" (lispm-test--text "Hello.")))))
      (lispm-tell agent "hi")
      (let ((id (lispm-test--condition-id agent)))
        ;; Without a value, nothing happens.
        (should-error (lispm-restart agent id 'use-model) :type 'user-error)
        (should (equal (plist-get (lispm-agent-condition agent) :id) id))
        ;; Nor without a backend.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
          (should-error (lispm-choose-restart-named agent 'use-model) :type 'user-error))
        (should (lispm-agent-condition agent))
        (lispm-restart agent id 'use-model '(other . "bigger")))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p ":RESTART: use-model\n:VALUE: (other \\. \"bigger\")\n"
                              (lispm-test--transcript agent)))
      (should (string-match-p "↻ Retry with another backend or model: other, bigger"
                              (mapconcat (lambda (i) (or (plist-get i :text) ""))
                                         (lispm-chat-items (lispm-agent-buffer agent)) "\n")))
      (let ((revived (lispm-resume-file (lispm-test--crash agent))))
        (should (eq (lispm-agent-backend revived) 'other))
        (should (equal (lispm-agent-model revived) "bigger"))))))

(ert-deftest lispm-test-condition-commands ()
  "Commands about a condition ask only what they need, and say when there is none."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts `((tester ,(lispm-test--failure 500 "api_error" "Boom"))))
      (lispm-tell agent "hi")
      (lispm-test--condition-id agent)
      ;; Stopping at a condition asks for no reason.
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "Asked for a reason"))))
        (with-current-buffer (lispm-agent-buffer agent)
          (call-interactively #'lispm-deny)))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should-error (lispm-choose-restart-named agent 'retry) :type 'user-error))))

(ert-deftest lispm-test-condition-resumed ()
  "A condition saved when Emacs stopped is waited at again when the session is opened."
  (lispm-test-with-conditions
    (let* ((default-directory project)
           (agent (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--failure 529 "overloaded_error" "Overloaded"))))
      (lispm-tell agent "hi")
      (lispm-test--condition-id agent)
      (let ((revived (lispm-resume-file (lispm-test--crash agent))))
        (should (eq (lispm-agent-state revived) 'waiting))
        (should (equal (plist-get (lispm-agent-condition revived) :id) "c1"))
        (should (eq (plist-get (lispm-agent-condition revived) :type) 'request-failed))
        (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Back."))) lispm-test--scripts)
        (lispm-restart revived "c1" 'retry)
        (should (lispm-test--wait (lispm-test--idle-p revived)))
        (should (string-match-p "^\\*\\* Reply\nBack\\.$" (lispm-test--transcript revived)))))))

(ert-deftest lispm-test-condition-abort-command ()
  "Interrupting a turn that waits at a condition stops it there."
  (lispm-test-with-conditions
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts `((tester ,(lispm-test--failure 500 "api_error" "Boom"))))
      (lispm-tell agent "hi")
      (lispm-test--condition-id agent)
      (lispm-abort agent)
      (should (funcall (lispm-test--idle-p agent)))
      (should-not (lispm-agent-condition agent))
      (should (string-match-p "^\\* Note\nThe request failed (HTTP 500): api_error: Boom\nAborted by the user\\.\n\\* User\n\\'"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-condition-helper ()
  "A helper's condition waits for the user; its caller waits for the helper."
  (lispm-test-with-conditions
    (lispm-defrole reviewer "Reviews changes.")
    (let ((boss (lispm-defagent boss :root project)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "reviewer") (request . "Looks good?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "No review.")))
              (reviewer@project ,(lispm-test--failure 500 "api_error" "Boom"))))
      (lispm-tell boss "get a review")
      (should (lispm-test--wait (lambda () (lispm-get-agent 'reviewer@project t))))
      (let ((helper (lispm-get-agent 'reviewer@project)))
        (lispm-test--condition-id helper)
        (should (eq (lispm-agent-state boss) 'acting))
        (lispm-restart helper "c1" 'abort))
      (should (lispm-test--wait (lispm-test--idle-p boss)))
      (should (string-match-p "reviewer@project replied:\nThe request failed (HTTP 500): api_error: Boom"
                              (format "%S" (alist-get 'messages (car lispm-test--requests))))))))

(ert-deftest lispm-test-chat-condition ()
  "The chat asks how to go on; a number chooses; writing stops there and sends."
  (lispm-test-with-conditions
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Hello."))
                      ,(lispm-test--failure 529 "overloaded_error" "Overloaded")
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Fine.")))))
      (lispm-test--chat-type chat "hi")
      (lispm-test--condition-id agent)
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "│ Request failed +│" text))
        (should (string-match-p "│   The request failed (HTTP 529): overloaded_error: Overloaded +│" text))
        (should (string-match-p "│ ❯ 1\\. Retry the request +│\n│   2\\. Retry with another backend or model +│\n│   3\\. Stop here +│"
                                text)))
      (lispm-test--chat-key chat "1")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "⎿  The request failed (HTTP 529): overloaded_error: Overloaded\n +↻ Retry the request" text))
        (should (string-match-p "⏺ Hello\\." text))
        (should-not (string-match-p "How do you want to go on" text)))
      ;; Writing while it waits stops there and sends the message.
      (lispm-test--chat-type chat "again")
      (lispm-test--condition-id agent)
      (lispm-test--chat-type chat "never mind")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p agent))
                                                (string-match-p "Fine" (lispm-test--transcript agent))))))
      (should (string-match-p "^\\* User\nagain\n\\* Note\nThe request failed (HTTP 529): overloaded_error: Overloaded\n\\* User\nnever mind\n"
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-placeholder ()
  "An empty input heading shows a hint, which is display only and goes once typing starts."
  (lispm-test-env
    (let* ((default-directory project)
           (agent (lispm-start)))
      (with-current-buffer (lispm-agent-buffer agent)
        (should (overlayp lispm--placeholder))
        (should (string-match-p "C-c RET to send" (overlay-get lispm--placeholder 'after-string)))
        (should (= (overlay-start lispm--placeholder) (1- (point-max))))
        (should-not (string-match-p "C-c RET" (buffer-string)))
        (goto-char (point-max))
        (insert "hi")
        (should-not lispm--placeholder)
        (delete-region (- (point-max) 2) (point-max))
        (should (overlayp lispm--placeholder))))))

(ert-deftest lispm-test-standing-agent ()
  "A standing agent keeps one conversation: it resumes it, even after its buffer closed."
  (lispm-test-env
    (let ((agent (lispm-defagent keeper "Keeps going." :root project)))
      (should (lispm-agent-standing agent))
      (setq lispm-test--scripts
            `((keeper ,(lispm-test--msg "end_turn" (lispm-test--text "one"))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "two")))))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((file (buffer-file-name (lispm-agent-buffer agent))))
        (kill-buffer (lispm-agent-buffer agent))
        (should (memq agent (lispm-agents)))
        (lispm-tell agent "second")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should (equal (buffer-file-name (lispm-agent-buffer agent)) file))
        (should (= (length (alist-get 'messages (car lispm-test--requests))) 3))))))

(ert-deftest lispm-test-ask-role ()
  "Asking a role starts a helper in the caller's project, which retires after replying."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes.")
    (let ((boss (lispm-defagent boss :root project)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "reviewer") (request . "Looks good?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "Approved.")))
              (reviewer@project ,(lispm-test--msg "end_turn" (lispm-test--text "Yes, ship it.")))))
      (lispm-tell boss "get a review")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                (= (length lispm-test--requests) 3)
                                                (null (cdr (lispm-agents)))))))
      (should (equal (mapcar #'lispm-agent-name (lispm-agents)) '(boss)))
      (should (equal (alist-get 'content (aref (alist-get 'content
                                                          (aref (alist-get 'messages (car lispm-test--requests)) 2))
                                               0))
                     "reviewer@project replied:\nYes, ship it."))
      ;; The helper's session stays on record.
      (should (seq-find (lambda (s) (equal (plist-get s :agent) "reviewer@project")) (lispm-sessions))))))

(ert-deftest lispm-test-helper-policy-bounded-by-caller ()
  "A helper may do no more than the agent that asked for it.
Its policy is the stricter of its role's and its caller's."
  (lispm-test-env
    (lispm-defrole coder "Writes code." :policy '((write . auto)))
    (let ((reviewer (lispm-defagent reviewer :root project :policy '((write . deny)))))
      (setq lispm-test--scripts
            `((reviewer ,(lispm-test--msg "tool_use"
                                          (lispm-test--tool "t1" "ask_agent"
                                                            '((agent . "coder") (request . "Write x.txt"))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "Asked.")))
              (coder@project ,(lispm-test--msg "tool_use"
                                               (lispm-test--tool "c1" "write_file"
                                                                 '((why . "asked to") (path . "x.txt")
                                                                   (content . "written\n"))))
                             ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell reviewer "Get x.txt written")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p reviewer))
                                                (= (length lispm-test--requests) 4)
                                                (null (cdr (lispm-agents)))))))
      (should-not (file-exists-p (expand-file-name "x.txt" project)))
      (let* ((helper-request (seq-find (lambda (body)
                                         (eq (lispm-test--agent-name body) 'coder@project))
                                       lispm-test--requests))
             (results (alist-get 'content (aref (alist-get 'messages helper-request) 2))))
        (should (string-match-p "Denied by policy" (alist-get 'content (aref results 0))))))))

;;;; Worktrees

(defvar lispm-worktree-directory)       ; of the worktree skill

(defun lispm-test--git (directory &rest args)
  "Run git ARGS in DIRECTORY; return its output, trimmed."
  (let ((default-directory directory))
    (with-temp-buffer
      (apply #'call-process "git" nil t nil
             "-c" "user.name=test" "-c" "user.email=test@lispm.invalid" "-c" "commit.gpgsign=false"
             args)
      (string-trim (buffer-string)))))

(defun lispm-test--git-repo (directory)
  "Make DIRECTORY a git repository with one commit; return it."
  (lispm-test--write (expand-file-name "README" directory) "hello\n")
  (lispm-test--git directory "init" "-q")
  (lispm-test--git directory "add" ".")
  (lispm-test--git directory "commit" "-q" "-m" "init")
  directory)

(ert-deftest lispm-test-worktree-same-project ()
  "A linked git worktree is the project of its main worktree: same sessions, memory and tasks."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (linked (expand-file-name "../linked/" main))
           (module (expand-file-name "../module/" main)))
      (lispm-test--git main "worktree" "add" "-b" "side" linked)
      (should (file-regular-p (expand-file-name ".git" linked)))
      (should (equal (lispm--main-worktree linked) main))
      (should (equal (lispm--main-worktree (expand-file-name "lisp/" linked))
                     (expand-file-name "lisp/" main)))
      (should (equal (lispm--main-worktree main) main))
      (should (equal (lispm-project-directory linked) (lispm-project-directory main)))
      ;; A submodule's .git is a file too, but it is a project of its own.
      (lispm-test--write (expand-file-name ".git" module) "gitdir: ../project/.git/modules/module\n")
      (should (equal (lispm--main-worktree module) module))
      (let ((here (lispm-defagent here :root main))
            (there (lispm-defagent there :root linked)))
        (lispm-test--call here "remember" '((title . "Shared") (content . "One project.")))
        (should (equal (car (lispm-test--call there "recall" '((query . "shared"))))
                       "## Shared\nOne project."))))))

(ert-deftest lispm-test-worktree-skill ()
  "worktree_ask starts a helper in a worktree of its own; its work stays there until merged."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (lispm-defrole coder "Writes code." :policy '((write . auto)))
    (let* ((project (lispm-test--git-repo (file-truename project)))
           (boss (lispm-defagent boss :root project :policy '((write . auto))))
           (call (lambda (tool input &optional agent)
                   (let ((lispm-current-agent (or agent boss)) result)
                     (lispm-tool-invoke (lispm-get-tool tool) input
                                        (lambda (text errorp &rest _) (setq result (cons text errorp))))
                     (should (lispm-test--wait (lambda () result) 10))
                     result)))
           (trees (expand-file-name "../worktrees/" project))
           (tree (lambda (name) (expand-file-name (concat "project/" name "/") trees)))
           (branch-p (lambda (name)
                       (not (string-empty-p (lispm-test--git project "branch" "--list"
                                                             (concat "lispm/" name)))))))
      (should (string-match-p "tools now available: worktree_ask, worktree_list, worktree_remove"
                              (car (lispm-test--call boss "skill" '((name . "worktree"))))))
      (unwind-protect
          (let ((lispm-worktree-directory trees))
            (setq lispm-test--scripts
                  `((coder@x ,(lispm-test--msg "tool_use"
                                               (lispm-test--tool "c1" "write_file"
                                                                 '((why . "asked to") (path . "x.txt")
                                                                   (content . "from x\n"))))
                             ,(lispm-test--msg "end_turn" (lispm-test--text "Wrote x.txt.")))
                    (coder@y ,(lispm-test--msg "end_turn" (lispm-test--text "Nothing to do.")))
                    (coder@z ,(lispm-test--msg "tool_use"
                                               (lispm-test--tool "c2" "write_file"
                                                                 '((why . "asked to") (path . "z.txt")
                                                                   (content . "from z\n"))))
                             ;; The helper commits its work.
                             ,(lambda (_body)
                                (lispm-test--git (funcall tree "z") "add" ".")
                                (lispm-test--git (funcall tree "z") "commit" "-q" "-m" "Add z")
                                (lispm-test--msg "end_turn" (lispm-test--text "Committed."))))))
            ;; Work left uncommitted stays in the worktree, apart from the project.
            (let ((result (funcall call "worktree_ask" '((why . "apart") (role . "coder")
                                                         (request . "Write x.txt") (name . "x")))))
              (should-not (cdr result))
              (should (string-match-p
                       (concat "\\`coder@x replied:\nWrote x\\.txt\\.\n\nIts work is in the worktree "
                               ".*/worktrees/project/x/, on the branch lispm/x, made from [0-9a-f]\\{7\\}\\.\n"
                               "Uncommitted:\n\\?\\? x\\.txt\n")
                       (car result))))
            (should (equal (lispm-test--file (funcall tree "x") "x.txt") "from x\n"))
            (should-not (file-exists-p (expand-file-name "x.txt" project)))
            (should (lispm-test--wait (lambda () (equal (mapcar #'lispm-agent-name (lispm-agents)) '(boss)))))
            (let ((helper (seq-find (lambda (body) (eq (lispm-test--agent-name body) 'coder@x))
                                    lispm-test--requests)))
              (should (string-match-p "You work in a git worktree of your own, .*/x/, on the branch lispm/x, made from HEAD"
                                      (format "%S" (alist-get 'messages helper)))))
            ;; The helper's session is one of the project's.
            (should (string-prefix-p (lispm-project-directory project)
                                     (plist-get (seq-find (lambda (s) (equal (plist-get s :agent) "coder@x"))
                                                          (lispm-sessions))
                                                :file)))
            (should (string-match-p "\\`- x: branch lispm/x, .*/x/ (uncommitted changes)\\'"
                                    (car (lispm-test--call boss "worktree_list" nil))))
            (should (cdr (funcall call "worktree_remove" '((why . "done") (name . "x")))))
            (should (file-exists-p (funcall tree "x")))
            (should (string-match-p "\\`Removed the worktree .*/x/ and its branch lispm/x\\.\\'"
                                    (car (funcall call "worktree_remove"
                                                  '((why . "drop") (name . "x") (force . t))))))
            (should-not (file-exists-p (funcall tree "x")))
            (should-not (funcall branch-p "x"))
            ;; A helper that changed nothing leaves nothing behind.
            (should (equal (funcall call "worktree_ask" '((why . "apart") (role . "coder")
                                                          (request . "Check y") (name . "y")))
                           (cons (concat "coder@y replied:\nNothing to do.\n\n"
                                         "It changed nothing, so its worktree and branch are removed.")
                                 nil)))
            (should-not (file-exists-p (funcall tree "y")))
            (should-not (funcall branch-p "y"))
            ;; Committed work is merged from its branch, which then goes with its worktree.
            (let ((result (funcall call "worktree_ask" '((why . "apart") (role . "coder")
                                                         (request . "Write z.txt") (name . "z")))))
              (should (string-match-p
                       "\nCommits:\n[0-9a-f]+ Add z\nChanges from [0-9a-f]\\{7\\}:\n z\\.txt | 1 \\+\n"
                       (car result)))
              (should-not (string-match-p "Uncommitted" (car result))))
            (lispm-test--git project "merge" "-q" "--ff-only" "lispm/z")
            (should (equal (lispm-test--file project "z.txt") "from z\n"))
            (should (string-match-p "\\`Removed the worktree .*/z/ and its branch lispm/z\\.\\'"
                                    (car (funcall call "worktree_remove" '((why . "merged") (name . "z"))))))
            (should (equal (car (lispm-test--call boss "worktree_list" nil)) "No worktrees."))
            ;; Mistakes are reported before anything is made.
            (should (equal (funcall call "worktree_ask" '((why . "w") (role . "coder") (request . "r")
                                                          (base . "nope")))
                           '("No commit nope here" . t)))
            (should (equal (funcall call "worktree_ask" '((why . "w") (role . "poet") (request . "r")))
                           '("No role named poet; list_agents shows the roles" . t)))
            (should (equal (funcall call "worktree_ask" '((why . "w") (role . "coder") (request . "r")
                                                          (name . "../up")))
                           '("A worktree name is letters, digits, dots, dashes and underscores" . t)))
            (let ((plain (expand-file-name "../plain/" project)))
              (make-directory plain)
              (should (string-match-p "is not in a git repository\\'"
                                      (car (funcall call "worktree_ask" '((why . "w") (role . "coder")
                                                                          (request . "r"))
                                                    (lispm-defagent plain :root plain)))))))
        (dolist (name '("worktree_ask" "worktree_list" "worktree_remove"))
          (remhash name lispm-tools--registry))))))

(ert-deftest lispm-test-compaction ()
  "A long session is handed over: memory saved, a summary written, the turn continued."
  (lispm-test-env
    (let* ((default-directory project)
           (lispm-compact-threshold nil)
           (agent (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "end_turn" (lispm-test--text "Let's go to Kyoto."))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "m1" "remember"
                                                              '((title . "Trip") (content . "Kyoto in May"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Saved."))
               ,(lispm-test--msg "end_turn" (lispm-test--text "The user plans a trip to Kyoto."))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Booked.")))))
      (lispm-tell agent "Plan a trip")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((old (buffer-file-name (lispm-agent-buffer agent)))
            (lispm-compact-threshold 10))
        ;; The user types the next request; it is carried over to the continuation.
        (with-current-buffer (lispm-agent-buffer agent)
          (goto-char (point-max))
          (insert "Book it")
          (lispm-send))
        (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p agent))
                                                  (= (length lispm-test--requests) 5)))))
        (let* ((requests (reverse lispm-test--requests))
               (summary-request (nth 3 requests))
               (final (nth 4 requests)))
          ;; The summary is asked for without tools, after the whole conversation.
          (should-not (assq 'tools summary-request))
          (should (equal (alist-get 'text (aref (alist-get 'content (car (last (append (alist-get 'messages summary-request) nil)))) 0))
                         lispm--summary-prompt))
          ;; The continuation starts from the summary, followed by the request.
          (should (equal (alist-get 'messages final)
                         [((role . "user")
                           (content . [((type . "text")
                                        (text . "[Summary of the earlier conversation, handed over to this session]\nThe user plans a trip to Kyoto."))
                                       ((type . "text") (text . "Book it"))]))])))
        (let ((new (buffer-file-name (lispm-agent-buffer agent))))
          (should-not (equal new old))
          (should (string-match-p (regexp-quote (format "#+lispm_previous: [[file:%s]]" old))
                                  (lispm--file-text new)))
          (should (string-match-p "\\* Note\nContinued in \\[\\[file:" (lispm--file-text old)))
          (should (string-match-p "Booked\\." (lispm--file-text new)))
          (should (string-match-p "\\* Trip\n" (lispm--file-text (lispm-memory-file agent))))
          ;; Still one session, listed by its newest file under its first title.
          (should (equal (mapcar (lambda (s) (list (plist-get s :file) (plist-get s :title)))
                                 (lispm-sessions))
                         (list (list new "Plan a trip"))))
          (should-not (lispm-agent-compacting agent)))))))

;;;; Collaboration

(ert-deftest lispm-test-ask-agent ()
  (lispm-test-env
    (let ((boss (lispm-defagent boss "Delegates." :root project))
          (helper (lispm-defagent helper "Does arithmetic." :root project)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "helper") (request . "2+2?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "It is 4.")))
              (helper ,(lispm-test--msg "end_turn" (lispm-test--text "4")))))
      (lispm-tell boss "ask helper")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                (funcall (lispm-test--idle-p helper))
                                                (= (length lispm-test--requests) 3)))))
      (should (string-match-p "\\* Message from boss\n2\\+2\\?" (lispm-test--transcript helper)))
      (let ((messages (alist-get 'messages (car lispm-test--requests))))
        (should (equal (alist-get 'content (aref (alist-get 'content (aref messages 2)) 0))
                       "helper replied:\n4")))
      (should-not (lispm-agent-waiting-on boss))
      ;; Each agent's system prompt introduces the other.
      (should (string-match-p "- helper: Does arithmetic." (alist-get 'system (car (last lispm-test--requests)))))
      ;; A request that would make two agents wait on each other is refused.
      (setf (lispm-agent-waiting-on helper) (list boss))
      (let ((result (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "?")))))
        (should (cdr result))
        (should (string-match-p "deadlock" (car result))))
      (should (cdr (lispm-test--call boss "send_message" '((to . "boss") (text . "hi"))))))))

(ert-deftest lispm-test-ask-agents-in-parallel ()
  "An agent that asks several at once waits on each until it replies."
  (lispm-test-env
    (let ((boss (lispm-defagent boss :root project))
          (adder (lispm-defagent adder :root project))
          (doubler (lispm-defagent doubler :root project)))
      (setq lispm-test--scripts
            `((boss ,(lispm-test--msg "tool_use"
                                      (lispm-test--tool "t1" "ask_agent"
                                                        '((agent . "adder") (request . "2+2?")))
                                      (lispm-test--tool "t2" "ask_agent"
                                                        '((agent . "doubler") (request . "2*3?"))))
                    ,(lispm-test--msg "end_turn" (lispm-test--text "4 and 6.")))
              ;; Asking back the agent that waits on it would deadlock,
              ;; though that agent asked someone else after it.
              (adder ,(lispm-test--msg "tool_use"
                                       (lispm-test--tool "a1" "ask_agent"
                                                         '((agent . "boss") (request . "Why?"))))
                     ,(lispm-test--msg "end_turn" (lispm-test--text "4")))
              (doubler ,(lispm-test--msg "end_turn" (lispm-test--text "6")))))
      (lispm-tell boss "ask both")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                (funcall (lispm-test--idle-p adder))
                                                (funcall (lispm-test--idle-p doubler))))))
      (should (string-match-p "deadlock" (lispm-test--transcript adder)))
      (should (string-match-p "adder replied:\n4" (lispm-test--transcript boss)))
      (should (string-match-p "doubler replied:\n6" (lispm-test--transcript boss)))
      (should-not (lispm-agent-waiting-on boss)))))

;;;; Triggers

(ert-deftest lispm-test-watch ()
  "Hooks wake agents; hook runs an agent causes itself do not."
  (lispm-test-env
    (let ((agent (lispm-defagent watcher :root project :policy '((write . auto))))
          (file (expand-file-name "notes.txt" project)))
      (with-temp-file file (insert "draft\n"))
      (setq lispm-test--scripts
            `((watcher ,(lispm-test--msg "tool_use"
                                         (lispm-test--tool "t1" "edit_file"
                                                           '((why . "w") (path . "notes.txt")
                                                             (old_text . "draft") (new_text . "final"))))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Polished.")))))
      (lispm-watch agent 'after-save-hook
                   :when (lambda () (string-suffix-p ".txt" buffer-file-name))
                   :prompt (lambda () (format "%s was saved" (file-name-nondirectory buffer-file-name)))
                   :debounce 0.05)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (insert "more\n")
        (save-buffer))
      (should (lispm-test--wait (lambda () (= (length lispm-test--requests) 2))))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (sleep-for 0.3)
      (should (= (length lispm-test--requests) 2))
      (should (equal (lispm-test--file project "notes.txt") "final\nmore\n"))
      (should (string-match-p "\\* Event: after-save-hook\nnotes.txt was saved"
                              (lispm-test--transcript agent)))
      (lispm-unwatch agent)
      (should-not (lispm-agent-triggers agent)))))

;;;; Memory

(defun lispm-test--git-settled-p ()
  "Non-nil when every queued change has been committed."
  (and (null lispm-git--queue) (null lispm-git--timer)
       (not (process-live-p lispm-git--process))))

(ert-deftest lispm-test-memory-and-tasks ()
  (lispm-test-env
    (let ((agent (lispm-defagent keeper :root project))
          (lispm-git (executable-find "git"))
          id)
      (should (equal (lispm-test--call agent "remember"
                                       '((title . "Indentation") (content . "* Two spaces\nalways")
                                         (tags . ["style" "user pref"])))
                     '("Saved project memory \"Indentation\"." . nil)))
      (should (equal (car (lispm-test--call agent "remember"
                                            '((title . "indentation") (content . "Two spaces, no tabs"))))
                     "Updated project memory \"indentation\"."))
      (lispm-test--call agent "remember" '((title . "Editor") (content . "Uses Emacs 31")))
      (should (equal (car (lispm-test--call agent "recall" '((query . "TABS two"))))
                     "## indentation\nTwo spaces, no tabs"))
      (should (string-match-p "- Editor" (car (lispm-test--call agent "recall" '((query . ""))))))
      (lispm-test--call agent "forget" '((title . "Editor")))
      (should-not (string-match-p "Editor" (car (lispm-test--call agent "recall" '((query . ""))))))
      (let ((added (car (lispm-test--call agent "task_add"
                                          '((title . "Write docs") (notes . "README first"))))))
        (setq id (and (string-match "T[0-9a-f]\\{4\\}" added) (match-string 0 added)))
        (should id)
        (lispm-test--call agent "task_update" `((id . ,id) (state . "DONE") (note . "shipped")))
        (should (equal (car (lispm-test--call agent "task_list" nil)) "No tasks."))
        (should (string-match-p (concat id " DONE Write docs\n  README first\n  - \\[.*\\] shipped")
                                (car (lispm-test--call agent "task_list" '((include_done . t))))))
        (should (cdr (lispm-test--call agent "task_update" `((id . ,id) (state . "WIP"))))))
      (should (equal (car (lispm-test--call agent "remember"
                                            '((title . "Prefers terse replies") (content . "Short answers.")
                                              (scope . "role"))))
                     "Saved role memory \"Prefers terse replies\"."))
      (should (file-exists-p (lispm-memory-file agent 'role)))
      (should (string-match-p "## Prefers terse replies (role memory)\nShort answers\."
                              (car (lispm-test--call agent "recall" '((query . "terse"))))))
      (let ((prompt (lispm-system-prompt agent)))
        (should (string-match-p "# Your long-term memory\n.*\n- indentation$" prompt)))
      (should (string-match-p "^\\* indentation$"
                              (lispm--file-text (lispm-memory-file agent))))
      (when lispm-git
        (let ((settled #'lispm-test--git-settled-p)
              (default-directory lispm-directory))
          (should (lispm-test--wait settled 10))
          ;; One burst of changes, one commit listing each of them.
          (should (equal (shell-command-to-string "git log -1 --format='%an%n%B'")
                         (concat "keeper\nkeeper: 7 changes\n\n"
                                 "keeper: remember \"Indentation\"\n"
                                 "keeper: update memory \"indentation\"\n"
                                 "keeper: remember \"Editor\"\n"
                                 "keeper: forget \"Editor\"\n"
                                 "keeper: add task " id "\n"
                                 "keeper: task " id " -> DONE\n"
                                 "keeper: remember \"Prefers terse replies\"\n\n")))
          (lispm-test--call agent "remember" '((title . "Later") (content . "x")))
          (should (lispm-test--wait settled 10))
          (should (equal (shell-command-to-string "git log --format=%s")
                         "keeper: remember \"Later\"\nkeeper: 7 changes\n")))))))

(ert-deftest lispm-test-git-history-under-git ()
  "Commits go to `lispm-directory', by the agent, even when git started Emacs."
  (skip-unless (executable-find "git"))
  (lispm-test-env
    (let* ((agent (lispm-defagent keeper :root project))
           (lispm-git (executable-find "git"))
           (elsewhere (expand-file-name "../elsewhere/" project))
           (state (lambda () (cons (directory-files-recursively elsewhere "" t)
                                   (lispm--file-text (expand-file-name ".git/config" elsewhere)))))
           before)
      (make-directory elsewhere)
      (let ((default-directory elsewhere))
        (call-process "git" nil nil nil "init" "-q"))
      (setq before (funcall state))
      ;; As a git hook in a linked worktree, run by `git -c', exports them.
      (let ((process-environment
             (append (list (concat "GIT_DIR=" elsewhere ".git")
                           (concat "GIT_INDEX_FILE=" elsewhere ".git/index")
                           "GIT_AUTHOR_NAME=someone"
                           "GIT_CONFIG_PARAMETERS='init.defaultBranch'='inherited'"
                           "GIT_CONFIG_COUNT=1" "GIT_CONFIG_KEY_0=init.defaultBranch"
                           "GIT_CONFIG_VALUE_0=inherited")
                     process-environment)))
        (lispm-test--call agent "remember" '((title . "Note") (content . "x")))
        (should (lispm-test--wait #'lispm-test--git-settled-p 10)))
      (should (equal (funcall state) before))
      (let ((default-directory lispm-directory))
        (should (equal (shell-command-to-string "git log --format=%an%n%s")
                       "keeper\nkeeper: remember \"Note\"\n"))
        (should-not (equal (shell-command-to-string "git symbolic-ref --short HEAD")
                           "inherited\n"))))))

;;;; Commands

(ert-deftest lispm-test-commands ()
  "The user-facing commands run and show what they promise."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "x\n"))
    (let ((agent (lispm-defagent tester "Test agent." :root project :policy '((write . auto)))))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "edit_file"
                                                          '((why . "w") (path . "a.txt")
                                                            (old_text . "x") (new_text . "y"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
      (lispm-tell agent "change x")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-turn-diff agent)
      (should (string-match-p "^-x\n\\+y" (with-current-buffer "*lispm-diff*" (buffer-string))))
      (lispm-show-request agent)
      (should (string-match-p "\"note\": \"Nothing to send"
                              (with-current-buffer "*lispm-request: tester*" (buffer-string))))
      (lispm-dashboard)
      (should (string-match-p "tester +tester +project +idle" (with-current-buffer "*lispm-agents*" (buffer-string))))
      (lispm-mode-line-mode 1)
      (unwind-protect
          (progn
            (should-not (lispm--mode-line))
            (lispm-set-state agent 'waiting)
            (should (equal (substring-no-properties (lispm--mode-line)) " λ[tester!]"))
            (lispm-set-state agent 'idle))
        (lispm-mode-line-mode -1))
      (kill-buffer "*lispm-diff*")
      (kill-buffer "*lispm-request: tester*")
      (kill-buffer "*lispm-agents*"))))

;;;; Chat

(defun lispm-test--chat-text (chat)
  "The text of CHAT, brought up to date."
  (lispm-chat-redraw chat)
  (with-current-buffer chat (buffer-substring-no-properties (point-min) (point-max))))

(defun lispm-test--chat-type (chat text)
  "Write TEXT in CHAT's input and send it."
  (with-current-buffer chat
    (lispm-chat-set-input text)
    (lispm-chat-return)))

(defun lispm-test--chat-key (chat key)
  "Press KEY in CHAT, the input being empty."
  (with-current-buffer chat
    (goto-char (point-max))
    (call-interactively (keymap-lookup lispm-chat-mode-map key))))

(ert-deftest lispm-test-chat-items ()
  "The chat reads the conversation, and only it, out of the org document."
  (with-temp-buffer
    (insert (lispm-transcript-header "t" '(("agent" . "t")))
            (lispm-transcript-section "System" "be brief")
            (lispm-transcript-section "Summary" "Earlier, we met.")
            (lispm-transcript-section "User" "* starred\nhello")
            "* User :ignore:\nnot sent\n"
            (lispm-transcript-section "Message from bob" "ping")
            (lispm-transcript-section "Event: timer" "tick")
            "* Assistant\n")
    (org-mode)
    (lispm-transcript-insert-blocks
     (list (lispm-test--thinking "Pondering deeply" "sig")
           (lispm-test--text "Let me look.")
           (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
     (lambda (_) "WAITING"))
    (insert (lispm-transcript-section "Note" "Continued elsewhere."))
    (let ((items (lispm-chat-items (current-buffer))))
      (should (equal (mapcar (lambda (i) (plist-get i :kind)) items)
                     '(summary user user event thought text tool note)))
      (should (equal (plist-get (nth 1 items) :text) "* starred\nhello"))
      (should (equal (plist-get (nth 2 items) :from) "bob"))
      (should (equal (plist-get (nth 4 items) :text) "Pondering deeply"))
      (should (equal (plist-get (nth 6 items) :state) "WAITING"))
      (should (equal (plist-get (nth 6 items) :input) '((path . "a.txt"))))))
  ;; A response streaming in.
  (with-temp-buffer
    (insert (lispm-transcript-section "User" "hi"))
    (let ((start (point-marker)))
      (insert "* Assistant\n"
              (propertize "weighing options" 'face 'shadow) "\n"
              "Here is a\n* heading-like line\n"
              (propertize "-> edit_file ...\n" 'face 'shadow))
      (setq lispm--live (cons start (point-marker))))
    (let ((items (lispm-chat-items (current-buffer))))
      (should (equal (mapcar (lambda (i) (plist-get i :kind)) items) '(user thought text tool)))
      (should (equal (plist-get (nth 2 items) :text) "Here is a\n* heading-like line"))
      (should (equal (plist-get (nth 3 items) :state) "WRITING")))))

(ert-deftest lispm-test-chat-streaming ()
  "A response streams into the chat; an edit before it still shows."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent))
           (session (lispm-agent-buffer agent)))
      (with-current-buffer session
        (goto-char (point-max))
        (insert "hello\n"))
      (lispm--live-begin agent)
      (lispm--live-insert agent 'text "Partial")
      (should (string-match-p "^λ >> hello\n\n⏺ Partial$" (lispm-test--chat-text chat)))
      (lispm--live-insert agent 'text " answer")
      (should (string-match-p "^⏺ Partial answer$" (lispm-test--chat-text chat)))
      (with-current-buffer session
        (goto-char (point-min))
        (search-forward "hello")
        (replace-match "howdy"))
      (should (string-match-p "^λ >> howdy$" (lispm-test--chat-text chat)))
      (lispm--live-delete agent)
      (should-not (string-match-p "Partial" (lispm-test--chat-text chat))))))

(ert-deftest lispm-test-chat-turn ()
  "A turn through the chat: send, see the calls, approve with 1, expand, commands."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "hello world\n"))
    (let* ((agent (lispm-start 'assistant project))
           (name (lispm-agent-name agent))
           (chat (lispm-chat agent))
           (session (lispm-agent-buffer agent)))
      (setq lispm-test--scripts
            `((,name
               ,(lispm-test--msg "tool_use" (lispm-test--text "Reading.")
                                 (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t2" "edit_file"
                                                   '((why . "Greet Lisp") (path . "a.txt")
                                                     (old_text . "world") (new_text . "lisp"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done, **greeted**.")))))
      ;; The org document is storage, out of sight; the chat is what shows.
      (should (string-prefix-p " " (buffer-name session)))
      (should (eq (buffer-local-value 'major-mode chat) 'lispm-chat-mode))
      (should (string-match-p "Welcome to LispM" (lispm-test--chat-text chat)))
      ;; The cursor is drawn where the input ends, not after the hints below it.
      (with-current-buffer chat
        (dolist (input '("" "typed"))
          (lispm-chat-set-input input)
          (let ((footer (overlay-get lispm-chat--footer 'after-string)))
            (should (= (overlay-start lispm-chat--footer) (point-max)))
            (should (get-text-property 0 'cursor footer))
            (should-not (eq (aref footer 0) ?\n))))
        (lispm-chat-set-input ""))
      (lispm-test--chat-type chat "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "^λ >> Greet lisp$" text))
        (should (string-match-p "^⏺ Reading\\.$" text))
        (should (string-match-p "^⏺ Read(a\\.txt)\n  ⎿  Read 1 line$" text))
        (should (string-match-p "⏺ Update(a\\.txt)\n  ⎿  Waiting for your approval…" text))
        (should (string-match-p "│ Edit file" text))
        (should (string-match-p "│   -hello world" text))
        (should (string-match-p "│   \\+hello lisp" text))
        (should (string-match-p "Do you want to make this edit to a\\.txt\\?" text))
        (should (string-match-p "❯ 1\\. Yes" text)))
      ;; The drawn conversation cannot be edited; typing there goes to the input.
      (with-current-buffer chat
        (goto-char (point-min))
        (should-error (insert "x") :type 'text-read-only))
      (lispm-test--chat-key chat "1")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (lispm-test--file project "a.txt") "hello lisp\n"))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p
                 "⏺ Update(a\\.txt)\n  ⎿  Updated a\\.txt with 1 addition and 1 removal" text))
        (should (string-match-p "^⏺ Done, greeted\\.$" text))
        (should-not (string-match-p "Do you want" text)))
      ;; TAB shows a result in full.
      (with-current-buffer chat
        (goto-char (point-min))
        (search-forward "Read(a.txt)")
        (lispm-chat-toggle))
      (should (string-match-p "⎿       1\thello world" (lispm-test--chat-text chat)))
      ;; Slash commands and history.
      (lispm-test--chat-type chat "/model other-model")
      (should (equal (lispm-agent-model agent) "other-model"))
      (with-current-buffer chat
        (lispm-chat-history-previous)
        (should (equal (lispm-chat--input) "/model other-model"))
        (lispm-chat-set-input ""))
      ;; Closing the chat ends the agent and its session.
      (kill-buffer chat)
      (should-not (buffer-live-p session))
      (should-not (lispm-get-agent name t)))))

(ert-deftest lispm-test-chat-approval-answers ()
  "2 approves a command for good, 3 denies with what to do instead."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "run_shell" '((why . "w") (command . "echo one"))))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t2" "write_file"
                                                   '((why . "w") (path . "b.txt") (content . "b\n"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
      (lispm-test--chat-type chat "go")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (string-match-p "│ Bash command +│\n│ +│\n│   echo one" (lispm-test--chat-text chat)))
      (should (string-match-p "2\\. Yes, and don't ask again for this exact command"
                              (lispm-test--chat-text chat)))
      (lispm-test--chat-key chat "2")
      ;; This command, not every command.
      (should (equal (lispm-agent-allowed agent) '(("run_shell" (command . "echo one")))))
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state agent) 'waiting)
                                                (equal (car (lispm--waiting-calls agent)) "t2")))))
      (should (string-match-p "Do you want to write b\\.txt\\?" (lispm-test--chat-text chat)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "use the scratch buffer")))
        (lispm-test--chat-key chat "3"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should-not (file-exists-p (expand-file-name "b.txt" project)))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "⏺ Bash(echo one)\n  ⎿  one" text))
        (should (string-match-p "⏺ Write(b\\.txt)\n  ⎿  The user denied this call\\. Their reason: use the scratch buffer"
                                text)))
      ;; A standing agent outlives its chat.
      (kill-buffer chat)
      (should (lispm-get-agent 'tester t)))))

(ert-deftest lispm-test-chat-busy ()
  "While the agent works the chat shows it, queues what you send, and interrupts."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts '((tester hang)))
      (lispm-test--chat-type chat "first")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'requesting))))
      (lispm-test--chat-type chat "second")
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "Thinking… ([0-9]+s · C-c C-c to interrupt)" text))
        (should (string-match-p "^λ >> second\n  ⎿  queued" text)))
      (with-current-buffer chat (lispm-chat-interrupt))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((text (lispm-test--chat-text chat)))
        (should-not (string-match-p "Thinking…" text))
        (should (string-match-p "Aborted by the user" text))))))

(ert-deftest lispm-test-chat-line-numbers ()
  "The welcome box and rules fit beside line numbers, and follow the room left."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent))
           (window (selected-window)))
      (cl-flet ((room ()
                  ;; The columns text can use: the body less the line numbers.
                  (with-selected-window window
                    (- (window-body-width window)
                       (ceiling (line-number-display-width 'columns)))))
                (widest ()
                  ;; The widest line drawn above the prompt.
                  (with-current-buffer chat
                    (apply #'max (mapcar #'string-width
                                         (split-string (buffer-substring-no-properties
                                                        (point-min) lispm-chat--prompt-start)
                                                       "\n"))))))
        (save-window-excursion
          (set-window-buffer window chat)
          ;; Margins narrow the body as a smaller window would.  Splitting
          ;; the window would too, but batch Emacs aborts when asked for the
          ;; line numbers of a window just split.
          (set-window-margins window 20 0)
          (with-current-buffer chat (display-line-numbers-mode 1))
          (should (> (with-selected-window window (line-number-display-width)) 0))
          (lispm-chat-redraw chat)
          (should (<= (widest) (room)))
          ;; A window that changes size is drawn at its new width.  Batch
          ;; Emacs does not run `window-size-change-functions', so this
          ;; calls the chat's own as they would.
          (let ((wide (widest)))
            (set-window-margins window 30 0)
            (lispm-chat--resized window)
            (should (lispm-test--wait (lambda () (< (widest) wide))))
            (should (<= (widest) (room))))
          ;; Line numbers turned off leave more room, drawn on.
          (let ((narrow (widest)))
            (with-current-buffer chat (display-line-numbers-mode -1))
            (should (lispm-test--wait (lambda () (> (widest) narrow))))
            (should (<= (widest) (room)))))))))

(ert-deftest lispm-test-chat-commands ()
  "M-x lispm opens a chat; /clear starts over; the org interface remains."
  (lispm-test-env
    (let ((default-directory project))
      (with-current-buffer (lispm)
        (should (eq major-mode 'lispm-chat-mode))
        (let* ((agent lispm-chat-agent)
               (old-chat (current-buffer))
               (new-chat (lispm-chat--command agent "/clear")))
          (should-not (buffer-live-p old-chat))
          (should-not (lispm-get-agent (lispm-agent-name agent) t))
          (with-current-buffer new-chat
            (should (eq major-mode 'lispm-chat-mode))
            (should-not (eq lispm-chat-agent agent)))))
      (let ((lispm-interface 'org))
        (with-current-buffer (lispm)
          (should (bound-and-true-p lispm-session-mode)))))))

;;;; Edits shown where they happen

(defun lispm-test--apply-hunks (old new hunks)
  "OLD with HUNKS, line ranges of OLD and NEW, replaced by NEW's lines."
  (let ((at 0) parts)
    (pcase-dolist (`(,of ,ot ,nf ,nt) hunks)
      (push (substring old at of) parts)
      (push (substring new nf nt) parts)
      (setq at ot))
    (push (substring old at) parts)
    (apply #'concat (nreverse parts))))

(ert-deftest lispm-test-edits-hunks ()
  "Changed lines are found as few, small hunks that turn the old text into the new."
  (pcase-dolist (`(,old ,new ,count)
                 '(("a\nb\nc\n" "a\nb\nc\n" 0)
                   ("a\nb\nc\n" "a\nB\nc\n" 1)
                   ("a\nb\nc\nd\ne\n" "A\nb\nc\nd\nE\n" 2)
                   ("a\nb\n" "a\nb\nc\n" 1)
                   ("a\nb\nc\n" "a\nc\n" 1)
                   ("" "x\ny" 1)
                   ("x\ny" "" 1)
                   ("a\nb" "a\nc" 1)
                   ("λ\n→\n∀\n" "λ\n∀\n→\n" 2)))
    (let ((hunks (lispm-edits--hunks old new)))
      (should (= (length hunks) count))
      (should (equal (lispm-test--apply-hunks old new hunks) new))))
  ;; One changed line in a long text is one small hunk.
  (let* ((old (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 1 500) ""))
         (new (string-replace "line 250\n" "line two hundred fifty\n" old)))
    (pcase (lispm-edits--hunks old new)
      (`((,of ,ot ,nf ,nt))
       (should (equal (substring old of ot) "line 250\n"))
       (should (equal (substring new nf nt) "line two hundred fifty\n")))
      (hunks (ert-fail (list "Expected one hunk" hunks))))))

(ert-deftest lispm-test-edits-partial-json ()
  "The input of a call being written is read as far as it goes."
  (pcase-dolist (`(,json ,members)
                 '(("" nil)
                   ("{" nil)
                   ("{\"pa" nil)
                   ("{\"path\"" nil)
                   ("{\"path\": " nil)
                   ("{\"path\": \"a.txt\", \"content\": \"hel" ((path . "a.txt") (content . "hel")))
                   ("{\"content\": \"a\\nb\\" ((content . "a\nb")))
                   ("{\"content\": \"x\\u00" ((content . "x")))
                   ("{\"content\": \"x\\u00e9" ((content . "xé")))
                   ("{\"t\": \"say \\\"hi\\\" now" ((t . "say \"hi\" now")))
                   ("{\"t\": \"a\\\\" ((t . "a\\")))
                   ("{\"a\": tru" nil)
                   ("{\"a\": 12" nil)
                   ("{\"a\": 12, \"b\": true, \"c\": null" ((a . 12) (b . t) (c . :null)))
                   ("{\"a\": [1, 2], \"b\": {\"c\": \"d\"}, \"e\": \"f\"}"
                    ((a . [1 2]) (b . ((c . "d"))) (e . "f")))))
    (should (equal (lispm-edits--partial-json json) members))))

(defmacro lispm-test-with-edits (&rest body)
  "Run BODY in `lispm-test-env', with edits shown as in an interactive Emacs."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-edits--in-batch t)
           (lispm-edits--shows nil)
           (lispm-edits--proposals nil)
           (lispm-edits--writing (make-hash-table :test #'equal)))
       (unwind-protect
           (progn ,@body)
         (lispm-edits--tick 1e10)
         (mapc (lambda (p) (lispm-edits--retire p nil)) lispm-edits--proposals)))))

(defun lispm-test--edit-overlays (buffer)
  "The overlays showing edits in BUFFER, as (START END . PROPERTIES)."
  (with-current-buffer buffer
    (mapcar (lambda (o)
              (append (list (overlay-start o) (overlay-end o))
                      (cl-loop for key in '(face display before-string)
                               for value = (overlay-get o key)
                               when value append (list key (if (stringp value)
                                                               (substring-no-properties value)
                                                             value)))))
            (seq-filter (lambda (o) (overlay-get o 'lispm-edits))
                        (overlays-in (point-min) (point-max))))))

(ert-deftest lispm-test-edits-window ()
  "A change comes into view in place of another file, never over the chat."
  (lispm-test-with-edits
    (let ((a (find-file-noselect (expand-file-name "a.txt" project)))
          (b (find-file-noselect (expand-file-name "b.txt" project)))
          (chat (generate-new-buffer "*test chat*")))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer chat (setq major-mode 'lispm-chat-mode))
            (delete-other-windows)
            (switch-to-buffer chat)
            ;; Only the chat in view: a window is split off for the file.
            (let ((window (lispm-edits--window a)))
              (should (eq (window-buffer window) a))
              (should (eq (window-buffer (selected-window)) chat)))
            ;; Another file in view: the change takes its place.
            (let ((window (lispm-edits--window b)))
              (should (eq (window-buffer window) b))
              (should-not (get-buffer-window a))
              (should (eq (window-buffer (selected-window)) chat))
              (should (= (length (window-list)) 2))))
        (kill-buffer chat)))))

(ert-deftest lispm-test-edits-typed-in ()
  "A change is typed in over what it replaced, then shows as it is."
  (lispm-test-with-edits
    (let ((file (expand-file-name "a.el" project))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (lispm-test--write file "(defun a ()\n  1)\n\n(provide 'a)\n")
      (let ((buffer (find-file-noselect file)))
        (should (equal (lispm-test--call agent "edit_file" '((path . "a.el") (old_text . "  1)")
                                                              (new_text . "  \"one\"\n  2)")))
                       '("Replaced 1 occurrence in a.el." . nil)))
        ;; The text is the new one at once; only the display lags.
        (should (equal (lispm-test--file project "a.el")
                       "(defun a ()\n  \"one\"\n  2)\n\n(provide 'a)\n"))
        (should (get-buffer-window buffer))
        (let* ((show (car lispm-edits--shows))
               (start (lispm-edits--show-start show))
               (overlays (lispm-test--edit-overlays buffer)))
          (should (eq (lispm-edits--show-agent show) agent))
          (should (> (lispm-edits--show-type show) 0))
          ;; What was replaced lingers above the change.
          (should (seq-some (lambda (o) (equal (plist-get (cddr o) 'before-string) "  1)\n"))
                            overlays))
          ;; What is not typed in yet is hidden behind the agent's cursor.
          (should (seq-some (lambda (o) (string-match-p "λ tester" (or (plist-get (cddr o) 'display) "")))
                            overlays))
          (lispm-edits--tick (+ start (lispm-edits--show-type show) 0.01))
          (setq overlays (lispm-test--edit-overlays buffer))
          (should-not (seq-some (lambda (o) (plist-get (cddr o) 'display)) overlays))
          ;; Then it is over.
          (lispm-edits--tick (+ start 100))
          (should-not lispm-edits--shows)
          (should-not (lispm-test--edit-overlays buffer))
          (should-not lispm-edits--timer))))))

(ert-deftest lispm-test-edits-proposed ()
  "A call awaiting approval shows its change in place, until it is made or refused."
  (lispm-test-with-edits
    (lispm-test--write (expand-file-name "a.txt" project) "hello world\nbye\n")
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "e1" "edit_file"
                                                   '((path . "a.txt") (old_text . "world")
                                                     (new_text . "lisp"))))
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "e2" "write_file"
                                                   '((path . "new.txt") (content . "fresh\n"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (let* ((buffer (get-file-buffer (expand-file-name "a.txt" project)))
             (overlays (lispm-test--edit-overlays buffer)))
        ;; The line it would change, struck out; the new one below; who asks.
        (should (member '(1 13 face lispm-edits-proposed-removed) overlays))
        (should (seq-some (lambda (o) (equal (plist-get (cddr o) 'before-string) "hello lisp\n"))
                          overlays))
        (should (seq-some (lambda (o) (string-match-p "λ tester · awaiting approval"
                                                      (or (plist-get (cddr o) 'before-string) "")))
                          overlays))
        (should (equal (lispm-test--file project "a.txt") "hello world\nbye\n"))
        (lispm-resolve agent "e1" 'approve)
        ;; Made: the proposal goes, and the change, seen already, is not typed in again.
        (should (equal (lispm-test--file project "a.txt") "hello lisp\nbye\n"))
        (should-not (lispm-edits--proposal "e1"))
        (should (= (lispm-edits--show-type (car lispm-edits--shows)) 0)))
      ;; A new file is shown in a buffer made for it, which goes when refused.
      (should (lispm-test--wait (lambda () (lispm-edits--proposal "e2"))))
      (let ((buffer (lispm-edits--proposal-buffer (lispm-edits--proposal "e2"))))
        (should (seq-some (lambda (o) (string-match-p "awaiting approval \nfresh\n"
                                                      (or (plist-get (cddr o) 'before-string) "")))
                          (lispm-test--edit-overlays buffer)))
        (lispm-resolve agent "e2" 'deny "No new files.")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should-not (lispm-edits--proposal "e2"))
        (should-not (buffer-live-p buffer))
        (should-not (file-exists-p (expand-file-name "new.txt" project)))))))

(ert-deftest lispm-test-edits-proposed-reverted ()
  "A proposal over a buffer that is reverted is made again from its new text."
  (lispm-test-with-edits
    (let ((file (expand-file-name "a.txt" project))
          (agent (lispm-defagent tester "Test agent." :root project)))
      (lispm-test--write file "hello world\nbye\n")
      (setq lispm-test--scripts
            `((tester
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "e1" "edit_file"
                                                   '((path . "a.txt") (old_text . "world")
                                                     (new_text . "lisp"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (equal (lispm-edits--proposal-text (lispm-edits--proposal "e1")) "hello lisp\nbye\n"))
      (lispm-test--change-on-disk file "hello world\nbye\nmore\n")
      (with-current-buffer (find-buffer-visiting file) (revert-buffer t t t))
      (should (equal (lispm-edits--proposal-text (lispm-edits--proposal "e1"))
                     "hello lisp\nbye\nmore\n"))
      ;; A call that no longer makes sense shows nothing.
      (lispm-test--change-on-disk file "hello there\n")
      (with-current-buffer (find-buffer-visiting file) (revert-buffer t t t))
      (should-not (lispm-edits--proposal "e1"))
      (should-not (lispm-test--edit-overlays (find-buffer-visiting file)))
      (lispm-resolve agent "e1" 'deny "Never mind.")
      (should (lispm-test--wait (lispm-test--idle-p agent))))))

(ert-deftest lispm-test-edits-streamed ()
  "While the model writes a call, the change grows in place as its input arrives."
  (lispm-test-with-edits
    (lispm-test--write (expand-file-name "a.txt" project) "one\ntwo\nthree\n")
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (pieces '("{\"path\": \"a.txt\", \"old_text\": \"two\\n\", \"new_te"
                     "xt\": \"deux\\nzw"
                     "ei\\n\"}"))
           (lispm-api-function
            (lambda (_backend _body _betas &rest callbacks)
              ;; Stream the call's input, and never finish.
              (let ((json ""))
                (dolist (piece pieces)
                  (setq json (concat json piece))
                  (funcall (plist-get callbacks :on-delta) 'tool-input
                           (list :id "s1" :name "edit_file" :json json))))
              nil))
           (buffer (find-file-noselect (expand-file-name "a.txt" project))))
      (lispm-tell agent "Translate two")
      (should (lispm-test--wait (lambda () (lispm-edits--proposal "s1"))))
      (let ((p (lispm-edits--proposal "s1")))
        (should (lispm-edits--proposal-streaming p))
        (should (equal (lispm-edits--proposal-text p) "one\ndeux\nzwei\nthree\n"))
        (should (member '(5 9 face lispm-edits-proposed-removed) (lispm-test--edit-overlays buffer)))
        (should (seq-some (lambda (o) (string-match-p "λ tester · writing…"
                                                      (or (plist-get (cddr o) 'before-string) "")))
                          (lispm-test--edit-overlays buffer))))
      ;; The text cut short is shown as far as it goes.
      (lispm-edits--input agent (list :id "s1" :name "edit_file"
                                      :json "{\"path\": \"a.txt\", \"old_text\": \"two\\n\", \"new_text\": \"de"))
      (lispm-edits--written "s1")
      (should (equal (lispm-edits--proposal-text (lispm-edits--proposal "s1")) "one\ndethree\n"))
      ;; Stopped before it was done: nothing remains.
      (lispm-abort agent)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should-not (lispm-edits--proposal "s1"))
      (should-not (lispm-test--edit-overlays buffer))
      (should (equal (lispm-test--file project "a.txt") "one\ntwo\nthree\n")))))

;;;; Lisp under policy

(defun lispm-test--lisp (agent code)
  "Run the program CODE with run_lisp as AGENT, outside a turn; return (TEXT . ERRORP)."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke (lispm-get-tool "run_lisp") `((code . ,code))
                       (lambda (text errorp &optional _images) (setq result (cons text errorp))))
    (lispm-test--wait (lambda () result))
    result))

(ert-deftest lispm-test-lisp-computes ()
  "A program computes as Lisp does, and calls tools as functions."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "hello world\n")
    (let ((agent (lispm-defagent tester :root project)))
      (pcase-dolist (`(,code . ,expected)
                     '(("(mapcar (lambda (x) (* x x)) '(1 2 3))" . "=> (1 4 9)")
                       ("(cl-loop for i from 1 to 9 if (cl-oddp i) collect i)" . "=> (1 3 5 7 9)")
                       ("(pcase '(1 2) (`(,a ,b) (+ a b)))" . "=> 3")
                       ("(cl-block nil (dolist (x '(1 2 3)) (when (= x 2) (cl-return x))))" . "=> 2")
                       ("(letrec ((f (lambda (n) (if (< n 2) n (+ (funcall f (- n 1)) (funcall f (- n 2)))))))
                           (funcall f 10))"
                        . "=> 55")
                       ("(with-temp-buffer (insert \"abc\") (goto-char 1) (re-search-forward \"b\") (point))"
                        . "=> 3")
                       ;; Arguments that may be functions may be something else.
                       ("(list (alist-get \"a\" '((\"a\" . 1)) nil nil #'equal) (assoc \"b\" '((\"b\" . 2)) nil)
                               (sort (list 3 1 2) :lessp #'>) (locate-dominating-file \".\" \"no-such-file\"))"
                        . "=> (1 (\"b\" . 2) (3 2 1) nil)")
                       ;; Destructive functions work on a copy.
                       ("(let ((x (list 3 1 2))) (list (sort x #'<) (nreverse x) x))"
                        . "=> ((1 2 3) (2 1 3) (3 1 2))")
                       ("(princ \"hi\") (seq-filter (lambda (f) (string-suffix-p \".txt\" f)) (directory-files \".\"))"
                        . "hi\n=> (\"a.txt\")")))
        (should (equal (lispm-test--lisp agent code) (cons expected nil))))
      (should (equal (lispm-test--lisp agent "(string-trim (substring (read_file :path \"a.txt\") 7))")
                     '("Tool calls:\n- read_file a.txt\n\n=> hello world")))
      ;; A failed call signals an error the program may handle.
      (should (equal (lispm-test--lisp agent "(condition-case e (read_file :path \"no.txt\")
                                                (lispm-call-failed (car e)))")
                     '("Tool calls:\n- read_file no.txt (failed)\n\n=> lispm-call-failed")))
      (let ((result (lispm-test--lisp agent "(read_file :path \"no.txt\")")))
        (should (cdr result))
        (should (string-match-p "\\`Tool calls:\n- read_file no.txt (failed)\n\nTool call failed: read_file: No such file"
                                (car result)))))))

(ert-deftest lispm-test-lisp-refuses ()
  "A program acts only through tools: anything else it would do is refused."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a" project) "keep me\n")
    (let ((agent (lispm-defagent tester :root project))
          (other (generate-new-buffer "other")))
      (with-current-buffer other (insert "text") (goto-char 3))
      (unwind-protect
          (progn
            (pcase-dolist (`(,code . ,why)
                           '(("(delete-file \"a\")" . "delete-file: it is not known")
                             ("(funcall (intern \"delete-file\") \"a\")" . "calling delete-file")
                             ("(assoc \"a\" '((\"a\")) 'delete-file)" . "calling delete-file")
                             ("(mapcar #'funcall (list #'delete-file))" . "calling delete-file")
                             ("(setq lispm-default-policy nil)" . "setting lispm-default-policy")
                             ("(let ((lispm-default-policy nil)) 1)" . "binding lispm-default-policy")
                             ("(setcdr (assq 'write lispm-default-policy) 'auto)" . "setcdr")
                             ("(eval-when-compile (delete-file \"a\"))" . "eval-when-compile")
                             ("(cl-flet ((f () 1)) (eval-when-compile (delete-file \"a\")))"
                              . "eval-when-compile")
                             ("(rx (eval (delete-file \"a\")))" . "eval")
                             ("(with-current-buffer \"other\" (insert \"x\"))" . "insert in other")
                             ("(with-current-buffer \"other\" (goto-char 1))" . "goto-char in other")
                             ("(condition-case nil (while t) (quit 1))" . "handling quit")
                             ("(throw 'exit nil)" . "throwing to exit")
                             ("(print 1 #'ignore)" . "print with argument 2")
                             ("(match-data nil lispm-default-policy)" . "match-data with argument 2")
                             ("(sort (list 2 1) :key #'delete-file)" . "calling delete-file")
                             ("(read_file :path (propertize \"a\" 'face 'bold))" . "propertize")))
              (let ((result (lispm-test--lisp agent code)))
                (should (cdr result))
                (should (string-prefix-p (concat "Not allowed in run_lisp: " why) (car result)))))
            ;; Inside save-excursion point may move: it comes back.
            (should (equal (lispm-test--lisp agent "(with-current-buffer \"other\"
                                                      (save-excursion (goto-char 1) (looking-at \"te\")))")
                           '("=> t")))
            (should (= (with-current-buffer other (point)) 3))
            (should (equal (lispm-test--file project "a") "keep me\n")))
        (kill-buffer other)))))

(ert-deftest lispm-test-lisp-under-policy ()
  "A program's calls go through the policy: reads run, writes await approval.
The program waits; once a call is decided, it runs again, the calls
it made before keeping the results they had."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "hello world\n")
    (lispm-test--write (expand-file-name "b.txt" project) "bye world\n")
    (let ((agent (lispm-defagent tester :root project :policy '((exec . auto))))
          (code "(let (changed)
                   (dolist (f '(\"a.txt\" \"b.txt\"))
                     (when (string-search \"world\" (read_file :path f))
                       (condition-case e
                           (progn (edit_file :path f :old-text \"world\" :new-text \"lisp\")
                                  (push f changed))
                         (lispm-call-denied (princ (cadr e))))))
                   (princ (car (split-string (run_shell :command \"echo shell\") \"\\n\")))
                   changed)"))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "run_lisp" `((code . ,code))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "Greet lisp")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (equal (lispm-agent-status agent) "1 call awaits approval"))
      (should (equal (plist-get (lispm--asked (gethash "t1" (lispm-agent-pending agent))) :input)
                     '((path . "a.txt") (old_text . "world") (new_text . "lisp"))))
      (should (string-match-p "^\\*\\* WAITING run_lisp .*\n\\(?:.*\n\\)*\\*\\*\\* Asks: edit_file  a.txt\n\\(?:.*\n\\)*\\*\\*\\* Preview\n\\(?:.*\n\\)*\\+hello lisp"
                              (lispm-test--transcript agent)))
      (lispm-resolve agent "t1" 'approve)
      (should (lispm-test--wait (lambda () (equal (plist-get (lispm--asked (gethash "t1" (lispm-agent-pending agent)))
                                                             :input)
                                                  '((path . "b.txt") (old_text . "world") (new_text . "lisp"))))))
      (should (equal (lispm-test--file project "a.txt") "hello lisp\n"))
      (lispm-resolve agent "t1" 'deny "not b")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (lispm-test--file project "b.txt") "bye world\n"))
      (let* ((messages (alist-get 'messages (car lispm-test--requests)))
             (result (aref (alist-get 'content (aref messages 2)) 0)))
        (should (equal (alist-get 'content result)
                       (concat "Tool calls:\n- read_file a.txt\n- edit_file a.txt\n- read_file b.txt\n"
                               "- edit_file b.txt (denied)\n- run_shell echo shell\n\n"
                               "edit_file: The user denied this call. Their reason: not bshell\n"
                               "=> (\"a.txt\")")))
        (should-not (alist-get 'is_error result)))
      (should (string-match-p "^\\*\\* DONE run_lisp" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-lisp-replay ()
  "Run again after a call it waited for, a program gets the results its calls had.
They do not run again, even if what they read has changed since."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "hello world\n")
    (let ((agent (lispm-defagent tester :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "run_lisp"
                                                          '((code . "(write_file :path \"b.txt\"
                                                                                 :content (read_file :path \"a.txt\"))"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Copied.")))))
      (lispm-tell agent "copy")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (lispm-test--write (expand-file-name "a.txt" project) "changed\n")
      (lispm-resolve agent "t1" 'approve)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (lispm-test--file project "b.txt") "     1\thello world\n"))
      (should (string-match-p "Tool calls:\n- read_file a.txt\n- write_file b.txt\n\n=> Created b.txt."
                              (lispm-test--transcript agent))))))

(ert-deftest lispm-test-lisp-policy ()
  "A program may do no more than its agent: denied calls fail at once.
Running one is of the class `lisp', which an agent that may not read
is denied too, as programs read Emacs."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "hello world\n")
    (let ((agent (lispm-defagent tester :root project :policy '((write . deny)))))
      (should (equal (lispm-test--lisp agent "(condition-case e
                                                 (write_file :path \"b.txt\" :content \"x\")
                                               (lispm-call-denied (cadr e)))")
                     '("Tool calls:\n- write_file b.txt (denied)\n\n=> write_file: Denied by policy: tester may not use write tools.")))
      (should-not (file-exists-p (expand-file-name "b.txt" project))))
    (should (eq (lispm-policy-action (lispm-defagent t2 :root project :policy '((read . ask)))
                                     'lisp)
                'ask))
    (should (eq (lispm-policy-action (lispm-defagent t3 :root project) 'lisp) 'auto))
    (let ((agent (lispm-defagent t4 :root project :policy '((lisp . deny)))))
      (setq lispm-test--scripts
            `((t4 ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "run_lisp" '((code . "1"))))
                  ,(lispm-test--msg "end_turn" (lispm-test--text "OK.")))))
      (lispm-tell agent "go")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (string-match-p "^\\*\\* DENIED run_lisp" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-lisp-abort ()
  "Stopping the turn stops the program: whatever it waited for, it goes no further."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((exec . auto) (write . auto)))))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "run_lisp"
                                                          '((code . "(run_shell :command \"sleep 2\")
                                                                     (write_file :path \"after.txt\" :content \"x\")")))))))
      (lispm-tell agent "go")
      (should (lispm-test--wait (lambda () (seq-some (lambda (p) (eq (process-get p 'lispm-agent) agent))
                                                     (process-list)))))
      (lispm-abort agent)
      (should (eq (lispm-agent-state agent) 'idle))
      (accept-process-output nil 0.3)
      (should-not (file-exists-p (expand-file-name "after.txt" project)))
      (should (string-match-p "^\\*\\* FAILED run_lisp" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-lisp-chat-approval ()
  "The chat asks about a program's call as about any call; 2 allows its tool for good."
  (lispm-test-env
    (lispm-test--write (expand-file-name "a.txt" project) "one\ntwo\n")
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "run_lisp"
                                                          '((code . "(edit_file :path \"a.txt\" :old-text \"one\" :new-text \"1\")
                                                                     (edit_file :path \"a.txt\" :old-text \"two\" :new-text \"2\")"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
      (lispm-test--chat-type chat "go")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (let ((text (lispm-test--chat-text chat)))
        (should (string-match-p "│ Edit file · in the program Lisp runs" text))
        (should (string-match-p "│   \\+1 " text))
        (should (string-match-p "Do you want to make this edit to a\\.txt\\?" text))
        (should (string-match-p "2\\. Yes, and don't ask again for Update calls by this agent" text)))
      (lispm-test--chat-key chat "2")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (lispm-agent-allowed agent) '("edit_file")))
      (should (equal (lispm-test--file project "a.txt") "1\n2\n")))))

(ert-deftest lispm-test-write-outside-root ()
  "Writes outside an agent's root ask, whatever its policy: by path or by buffer."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project :policy '((write . auto))))
           (outside (expand-file-name "../outside.txt" project))
           (buffer (progn (lispm-test--write outside "x\n") (find-file-noselect outside))))
      (unwind-protect
          (progn
            (should (eq (lispm-tool-action agent (lispm-get-tool "edit_file") '((path . "in.txt"))) 'auto))
            (should (eq (lispm-tool-action agent (lispm-get-tool "edit_file") `((path . ,outside))) 'ask))
            (should (eq (lispm-tool-action agent (lispm-get-tool "edit_buffer")
                                           `((buffer . ,(buffer-name buffer))))
                        'ask))
            (should (eq (lispm-tool-action agent (lispm-get-tool "edit_buffer") '((buffer . "*scratch*")))
                        'auto)))
        (kill-buffer buffer)))))

(provide 'lispm-test)
;;; lispm-test.el ends here
