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

(defvar lispm-test--dir (file-name-directory (or load-file-name buffer-file-name)))

(defvar lispm-test--scripts nil
  "Alist from agent name to the responses its requests get, in order.
A response is a message alist, a function of the request body
returning one, or `hang' (never answer).")

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
                     (if (null step)
                         (funcall (plist-get callbacks :on-error)
                                  '(:status 400 :type "test_error" :message "script exhausted"))
                       (when-let* ((on-delta (plist-get callbacks :on-delta)))
                         (funcall on-delta 'text "streaming..."))
                       (funcall (plist-get callbacks :on-done)
                                (if (functionp step) (funcall step body) step))))))
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
          ;; Scripted responses are matched against Messages-shape requests.
          (lispm-backends (cons '(test :api anthropic :url "http://fake.invalid" :model "test-model")
                                lispm-backends))
          (lispm-backend 'test)
          (lispm--agents nil)
          (lispm--roles (copy-sequence lispm--roles))
          (lispm-api-function #'lispm-test--fake-api)
          (lispm-test--scripts nil)
          (lispm-test--requests nil))
     (make-directory project t)
     (unwind-protect
         (progn ,@body)
       (dolist (a (lispm-agents)) (lispm-unwatch a))
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
          (should (equal (reverse deltas) '((thinking . "Looking") (tool . "x") (tool . "x")))))))))

(ert-deftest lispm-test-fallback-cleanup ()
  "Before a fallback boundary only text blocks are kept."
  (should (equal (lispm-anthropic--after-fallback
                  (list (lispm-test--thinking "" "s") (lispm-test--text "partial")
                        (lispm-test--tool "t" "x" nil)
                        '((type . "fallback")) (lispm-test--text "rest")))
                 (list (lispm-test--text "partial") '((type . "fallback"))
                       (lispm-test--text "rest")))))

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
          (should (equal (reverse deltas)
                         '((thinking . "Think") (text . "Hel") (text . "lo")
                           (tool . "read_file") (tool . "x")))))))))

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
                       "repo/kept.el:1:(defun lispm-demo-kept ())"))))))

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
        ;; The default backend, rotom, gets failures marked in the text.
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
      (setf (lispm-agent-waiting-on helper) boss)
      (let ((result (lispm-test--call boss "ask_agent" '((agent . "helper") (request . "?")))))
        (should (cdr result))
        (should (string-match-p "deadlock" (car result))))
      (should (cdr (lispm-test--call boss "send_message" '((to . "boss") (text . "hi"))))))))

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
        (let ((settled (lambda () (and (null lispm-git--queue) (null lispm-git--timer)
                                       (not (process-live-p lispm-git--process)))))
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
  "2 approves a tool for good, 3 denies with what to do instead."
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
      (lispm-test--chat-key chat "2")
      (should (member "run_shell" (lispm-agent-allowed agent)))
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

(provide 'lispm-test)
;;; lispm-test.el ends here
