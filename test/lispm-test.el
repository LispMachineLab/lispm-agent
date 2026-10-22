;;; lispm-test.el --- Tests for lispm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Run with `make test'.  The model is replaced by a scripted fake
;; (`lispm-test--fake-api'), except in `lispm-test-curl-stream', which
;; drives the real curl transport against test/fake_messages_server.py.

;;; Code:

(require 'ert)
;; TRAMP before ert-x: it then sets up the mock method of remote tests
;; (`ert-remote-temporary-file-directory').  What TRAMP learns in them is
;; never saved, over what the user's Emacs keeps.
(require 'tramp)
(setq tramp-persistency-file-name nil)
(require 'ert-x)
(require 'lispm)
(require 'lispm-agents-claude)
(require 'lispm-agents-codex)

;; The words as the code writes them, whatever the computer's language:
;; a language without a catalog.  The tests of i18n speak others.
(setq lispm-language 'en)

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
          (lispm--role-files (copy-sequence lispm--role-files))
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

(ert-deftest lispm-test-transcript-sections ()
  "Other code reads a session through its sections: each of a kind its
title names, the user's inputs as requests carry them, and an assistant
section's parts, a cleared result read back whole."
  (with-temp-buffer
    (insert (lispm-transcript-header "t" '(("agent" . "t")))
            (lispm-transcript-section (lispm-transcript-title 'system) "Be brief.")
            (lispm-transcript-section (lispm-transcript-input-title '(:from "boss")) "do it")
            (lispm-transcript-section "Notes" "mine")
            (lispm-transcript-heading 'assistant)
            "** Reply\nOn it.\n"
            "** DONE read_file  a\n:PROPERTIES:\n:TOOL_ID:  t1\n:TOOL:     read_file\n:END:\n"
            "#+begin_src json\n{\"path\": \"a\"}\n#+end_src\n"
            "*** Result :cleared:\n#+begin_example\nold output\n#+end_example\n"
            "** Thinking: hm\n:PROPERTIES:\n:BLOCK:    thinking\n:END:\n"
            "#+begin_src json\n{\"type\": \"thinking\", \"thinking\": \"hm\"}\n#+end_src\n"
            (lispm-transcript-section (lispm-transcript-input-title '(:event "timer")) "tick")
            "* User :ignore:\nleft out\n")
    (org-mode)
    (let ((sections (lispm-transcript-sections)))
      (should (equal (mapcar (lambda (s) (list (plist-get s :kind) (plist-get s :arg))) sections)
                     '((system nil) (message "boss") (nil nil) (assistant nil) (event "timer") (user nil))))
      (should (equal (mapcar #'lispm-transcript-section-input sections)
                     '(nil "[Message from agent boss]\ndo it" nil nil "[Event: timer]\ntick" nil)))
      (should (plist-get (car (last sections)) :ignore))
      (pcase-let ((`(,reply ,call ,thinking) (lispm-transcript-parts (nth 3 sections))))
        (should (equal reply '(:kind text :text "On it.")))
        (should (equal (plist-get call :input) '((path . "a"))))
        (should (equal (plist-get call :result) "old output"))
        (should (plist-get call :cleared))
        (should (equal (plist-get thinking :block) '((type . "thinking") (thinking . "hm"))))))
    (should (equal (lispm-transcript-kind "event:  after-save-hook") '(event . "after-save-hook")))
    (goto-char (point-min))
    (should (re-search-forward (lispm-transcript-heading-regexp 'user 'event) nil t))
    (should (looking-at "tick"))))

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
                         '((input_tokens . 7) (output_tokens . 4) (cache_read_input_tokens . 2))))
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
        ;; Input tokens are those not read from the cache, as in Messages.
        (pcase-dolist (`(,name ,features ,usage)
                       '((streamer (streaming reasoning-effort stream-usage) (30 14 10))
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

(ert-deftest lispm-test-mcp-register-functions ()
  "Functions may keep a server's tools back, or have their calls always ask.
The strictest answer for a tool wins; an error, or an answer that is
not one, has every tool ask."
  (let* ((tools (mapcar (lambda (name) (list :name name :description (concat name " things")))
                        '("plain" "asks" "hidden")))
         (seen nil)
         (kinds (lambda (server tools)
                  (push (cons server tools) seen)
                  '(("asks" . ask) ("hidden" . deny))))
         (register (lambda (&rest functions)
                     (let ((lispm-mcp-register-functions functions))
                       (lispm-mcp--register "srv" (vconcat tools)))
                     (mapcar (lambda (name)
                               (let ((tool (lispm-get-tool (concat "mcp__srv__" name))))
                                 (cond ((null tool) 'kept)
                                       ((lispm-tool-always-ask tool) 'ask)
                                       (t 'plain))))
                             '("plain" "asks" "hidden")))))
    (unwind-protect
        (progn
          (should (equal (funcall register kinds) '(plain ask kept)))
          ;; The functions get the server and its tools as it lists them.
          (should (equal seen (list (cons "srv" tools))))
          (should (equal (lispm-mcp-tool-origin "mcp__srv__asks") '("srv" . "asks")))
          (should (equal (lispm-mcp-tool-origin (lispm-get-tool "mcp__srv__plain")) '("srv" . "plain")))
          (should-not (lispm-mcp-tool-origin "mcp__srv__hidden"))
          (should-not (lispm-mcp-tool-origin "read_file"))
          (should (string-match-p "2 tools from MCP server srv; kept back: hidden"
                                  (with-current-buffer "*Messages*"
                                    (save-excursion (goto-char (point-max))
                                                    (buffer-substring (line-beginning-position 0)
                                                                      (point-max))))))
          ;; The strictest answer wins; an action alone answers for all.
          (should (equal (funcall register kinds (lambda (&rest _) '(("asks" . deny))))
                         '(plain kept kept)))
          (should (equal (funcall register (lambda (&rest _) 'ask)) '(ask ask ask)))
          (should (equal (funcall register (lambda (&rest _) (error "Broken")) kinds)
                         '(ask ask kept)))
          (should (equal (funcall register (lambda (&rest _) '(("asks" . auto)))) '(ask ask ask)))
          (should (equal (funcall register) '(plain plain plain))))
      (lispm-mcp--unregister "srv"))
    (should-not (lispm-get-tool "mcp__srv__plain"))
    (should-not (lispm-mcp-tool-origin "mcp__srv__plain"))))

(ert-deftest lispm-test-mcp-refresh ()
  "A running server's tools are registered again, looked at afresh."
  (lispm-test-with-mcp ()
    (let ((lispm-mcp-register-functions nil))
      (lispm-mcp-start "fake")
      (should (lispm-test--wait (lambda () (lispm-get-tool "mcp__fake__snap")) 15))
      (add-hook 'lispm-mcp-register-functions (lambda (_server _tools) '(("snap" . deny))))
      (lispm-mcp-refresh "fake")
      (should (lispm-test--wait (lambda () (not (lispm-get-tool "mcp__fake__snap"))) 15))
      (should (lispm-get-tool "mcp__fake__echo"))
      (should (lispm-mcp--running-p "fake")))))

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

;;;; Developing Lisp in the image

(defconst lispm-test--dev-source
  (concat ";;; dev.el --- A fixture  -*- lexical-binding: t; -*-\n"
          "(defvar lispm-test-dev-var 1\n"
          "  \"A variable.\")\n"
          "(defcustom lispm-test-dev-option 'one\n"
          "  \"An option.\"\n"
          "  :type 'symbol :group 'lispm-test)\n"
          "(defface lispm-test-dev-face '((t :weight bold))\n"
          "  \"A face.\"\n"
          "  :group 'lispm-test)\n"
          "(defun lispm-test-dev-f (x)\n"
          "  \"Add one to X.\"\n"
          "  (+ x 1))\n"
          "(defun lispm-test-dev-g ()\n"
          "  \"Call `lispm-test-dev-f'.\"\n"
          "  (lispm-test-dev-f 2))\n"
          "(defun lispm-test-dev-closure ()\n"
          "  \"A closure over N.\"\n"
          "  (let ((n 7)) (lambda () n)))\n")
  "An Emacs Lisp file: its function lispm-test-dev-f is on lines 10-12.")

(defmacro lispm-test-with-dev (&rest body)
  "Run BODY in `lispm-test-env' with `file', the file dev.el of `project', loaded.
Its definitions, and any of a symbol named lispm-test-dev-*, are gone after."
  (declare (indent 0))
  `(lispm-test-env
     (let ((file (expand-file-name "dev.el" project)))
       (lispm-test--write file lispm-test--dev-source)
       (unwind-protect
           (progn
             (load file nil t t)
             ,@body)
         (mapatoms (lambda (symbol)
                     (when (string-prefix-p "lispm-test-dev-" (symbol-name symbol))
                       (fmakunbound symbol)
                       (makunbound symbol))))))))

(defun lispm-test--call-async (agent tool input)
  "Run the asynchronous TOOL on INPUT as AGENT; wait for (TEXT . ERRORP)."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke (lispm-get-tool tool) input
                       (lambda (text errorp &rest _) (setq result (cons text errorp))))
    (lispm-test--wait (lambda () result) 30)
    result))

(ert-deftest lispm-test-edit-definition ()
  "edit_definition replaces the form defining a symbol, in the file Emacs has it from."
  (lispm-test-with-dev
    (let* ((agent (lispm-defagent tester :root project :policy '((write . auto))))
           (new "(defun lispm-test-dev-f (x)\n  \"Multiply X by ten.\"\n  (* x 10))")
           (input `((name . "lispm-test-dev-f") (new_text . ,new)))
           (changed nil))
      ;; Previewed as a diff of the form, and shown in its buffer; nothing changes yet.
      (let ((lispm-current-agent agent))
        (should (string-match-p (concat "^-  \"Add one to X\\.\"\n-  (\\+ x 1))\n"
                                        "\\+  \"Multiply X by ten\\.\"\n\\+  (\\* x 10))\n")
                                (lispm-tool-preview-text (lispm-get-tool "edit_definition") input)))
        (pcase-let ((`(,buffer . ,text) (lispm-tool-would-change (lispm-get-tool "edit_definition") input)))
          (should (eq buffer (find-buffer-visiting file)))
          (should (string-search (concat "\n" new "\n(defun lispm-test-dev-g") text))))
      (should (equal (lispm-test--disk file) lispm-test--dev-source))
      ;; Made as one undo step, journaled, saved, and told.
      (let ((lispm-buffer-change-functions (list (lambda (buffer _old _new) (push buffer changed)))))
        (should (equal (lispm-test--call agent "edit_definition"
                                         `((name . "lispm-test-dev-f") (new_text . ,(concat "\n" new "\n"))))
                       '("Replaced the definition of lispm-test-dev-f in dev.el, now lines 10-12.  \
Emacs still runs the old one: evaluate this one with eval_definition." . nil))))
      (should (equal (lispm-test--disk file)
                     (string-replace "  \"Add one to X.\"\n  (+ x 1))" "  \"Multiply X by ten.\"\n  (* x 10))"
                                     lispm-test--dev-source)))
      (should (equal changed (list (find-buffer-visiting file))))
      (should (equal (plist-get (cdr (car (cdr (assq 0 (lispm-agent-journal agent))))) :text)
                     lispm-test--dev-source))
      (with-current-buffer (find-buffer-visiting file)
        (let ((last-command nil)) (undo))
        (should (equal (buffer-string) lispm-test--dev-source))
        (save-buffer))
      ;; A variable, a face, and a definition not evaluated yet, in the file given.
      (should-not (cdr (lispm-test--call agent "edit_definition"
                                         '((name . "lispm-test-dev-var")
                                           (new_text . "(defvar lispm-test-dev-var 42\n  \"A variable.\")")))))
      (should-not (cdr (lispm-test--call agent "edit_definition"
                                         '((name . "lispm-test-dev-face") (kind . "face")
                                           (new_text . "(defface lispm-test-dev-face '((t :slant italic))
  \"A face.\"
  :group 'lispm-test)")))))
      (should (string-search "(defvar lispm-test-dev-var 42\n" (lispm-test--disk file)))
      (should (string-search "(defface lispm-test-dev-face '((t :slant italic))\n" (lispm-test--disk file)))
      (lispm-test--write (expand-file-name "new.el" project) "(defun lispm-test-dev-new ()\n  1)\n")
      (should (equal (lispm-test--call agent "edit_definition"
                                       '((name . "lispm-test-dev-new") (path . "new.el")
                                         (new_text . "(defun lispm-test-dev-new ()\n  2)")))
                     '("Replaced the definition of lispm-test-dev-new in new.el, now lines 1-2.  \
Emacs still runs the old one: evaluate this one with eval_definition." . nil)))
      (should (equal (lispm-test--disk (expand-file-name "new.el" project))
                     "(defun lispm-test-dev-new ()\n  2)\n"))
      ;; What is not one form defining the symbol changes nothing.
      (let ((before (lispm-test--disk file)))
        (pcase-dolist (`(,text . ,why) '(("(defun lispm-test-dev-f (x) x) (defun lispm-test-dev-h ())"
                                          . "more than one form")
                                         ("(defun lispm-test-dev-f (x)" . "not a complete Lisp form")
                                         ("(defun lispm-test-dev-h (x) x)" . "does not define lispm-test-dev-f")))
          (pcase-let ((`(,result . ,errorp) (lispm-test--call agent "edit_definition"
                                                              `((name . "lispm-test-dev-f") (new_text . ,text)))))
            (should errorp)
            (should (string-match-p why result))))
        (should (string-match-p "\\`Emacs knows no file that defines lispm-test-dev-nowhere;"
                                (car (lispm-test--call agent "edit_definition"
                                                       '((name . "lispm-test-dev-nowhere")
                                                         (new_text . "(defun lispm-test-dev-nowhere ())"))))))
        (should (equal (lispm-test--disk file) before))))))

(ert-deftest lispm-test-edit-definition-outside-root ()
  "A definition outside the agent's root is written there, so it asks, whatever the policy.
The tools are deferred: the model finds them with search_tools first."
  (lispm-test-with-dev
    (let* ((agent (lispm-defagent tester "Test agent." :root project :policy '((write . auto))))
           (outside (expand-file-name "../elsewhere/ext.el" project))
           (tool (lispm-get-tool "edit_definition"))
           (redefine (lambda (name &rest more)
                       `((name . ,name) (new_text . ,(format "(defun %s ()\n  2)" name)) ,@more)))
           (names (lambda (request) (mapcar (lambda (tool) (alist-get 'name tool))
                                            (alist-get 'tools request)))))
      (lispm-test--write outside (concat ";;; ext.el --- A fixture  -*- lexical-binding: t; -*-\n"
                                         "(defun lispm-test-dev-ext ()\n  1)\n"))
      (load outside nil t t)
      (should (eq (lispm-tool-action agent tool (funcall redefine "lispm-test-dev-f")) 'auto))
      (should (eq (lispm-tool-action agent tool (funcall redefine "lispm-test-dev-ext")) 'ask))
      ;; So does one in Emacs's own sources, one in a file given outside,
      ;; and one whose file cannot be told.
      (should (eq (lispm-tool-action agent tool (funcall redefine "find-function-library")) 'ask))
      (should (eq (lispm-tool-action agent tool (funcall redefine "lispm-test-dev-ext" `(path . ,outside)))
                  'ask))
      (should (eq (lispm-tool-action agent tool (funcall redefine "lispm-test-dev-nowhere")) 'ask))
      (should (seq-every-p (lambda (name) (lispm-tool-deferred (lispm-get-tool name)))
                           '("edit_definition" "eval_definition" "ert_run"
                             "xref_definitions" "xref_references" "flymake_diagnostics")))
      (let ((chat (lispm-chat agent)))
        (setq lispm-test--scripts
              `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "s1" "search_tools"
                                                                       '((query . "lisp redefine"))))
                        ,(lispm-test--msg "tool_use" (lispm-test--tool "e1" "edit_definition"
                                                                       (funcall redefine "lispm-test-dev-ext")))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
        (lispm-test--chat-type chat "Make it two")
        (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
        (let ((requests (reverse lispm-test--requests)))
          (should-not (member "edit_definition" (funcall names (nth 0 requests))))
          (should (string-match-p "^- lisp: edit, evaluate and test Emacs Lisp definitions"
                                  (alist-get 'description
                                             (seq-find (lambda (tool) (equal (alist-get 'name tool) "search_tools"))
                                                       (alist-get 'tools (nth 0 requests))))))
          (should (member "edit_definition" (funcall names (nth 1 requests))))
          (should-not (member "eval_definition" (funcall names (nth 1 requests)))))
        ;; The chat asks with the diff.
        (let ((text (lispm-test--chat-text chat)))
          (should (string-match-p "│ Edit definition" text))
          (should (string-match-p "│   -  1)" text))
          (should (string-match-p "│   \\+  2)" text))
          (should (string-match-p "Do you want to make this edit to lispm-test-dev-ext\\?" text)))
        (should (string-suffix-p "(defun lispm-test-dev-ext ()\n  1)\n" (lispm-test--disk outside)))
        (lispm-resolve agent "e1" 'approve)
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should (string-suffix-p "(defun lispm-test-dev-ext ()\n  2)\n" (lispm-test--disk outside)))
        (should (string-match-p "⏺ Redefine(lispm-test-dev-ext)\n  ⎿  Updated lispm-test-dev-ext with 1 addition and 1 removal"
                                (lispm-test--chat-text chat)))))))

(ert-deftest lispm-test-eval-definition ()
  "eval_definition evaluates a definition from its file into Emacs, as C-M-x does."
  (lispm-test-with-dev
    (let ((agent (lispm-defagent tester :root project :policy '((write . auto))))
          (other (expand-file-name "other.el" project)))
      (lispm-test--call agent "edit_definition" '((name . "lispm-test-dev-f")
                                                  (new_text . "(defun lispm-test-dev-f (x)\n  (* x 10))")))
      ;; Edited, a definition changes in Emacs once it is evaluated.
      (should (= (lispm-test-dev-f 2) 3))
      (let ((lispm-current-agent agent))
        (should (equal (lispm-tool-preview-text (lispm-get-tool "eval_definition") '((name . "lispm-test-dev-f")))
                       "dev.el:10\n(defun lispm-test-dev-f (x)\n  (* x 10))")))
      (should (equal (lispm-test--call agent "eval_definition" '((name . "lispm-test-dev-f")))
                     '("Evaluated the definition of lispm-test-dev-f in dev.el, line 10.\n=> lispm-test-dev-f" . nil)))
      (should (= (lispm-test-dev-f 2) 20))
      ;; A defvar or a defcustom is reset to what it says, as C-M-x resets it.
      (setq lispm-test-dev-var 5
            lispm-test-dev-option 'two)
      (should (equal (lispm-test--call agent "eval_definition" '((name . "lispm-test-dev-var")))
                     '("Evaluated the definition of lispm-test-dev-var in dev.el, line 2.\n=> 1" . nil)))
      (should (= lispm-test-dev-var 1))
      (should-not (cdr (lispm-test--call agent "eval_definition" '((name . "lispm-test-dev-option")))))
      (should (eq lispm-test-dev-option 'one))
      ;; The file's lexical binding holds, and so does another file's dynamic one.
      (should-not (cdr (lispm-test--call agent "eval_definition" '((name . "lispm-test-dev-closure")))))
      (should (= (funcall (lispm-test-dev-closure)) 7))
      (lispm-test--write other (concat ";;; other.el --- A fixture  -*- lexical-binding: nil; -*-\n"
                                       "(defun lispm-test-dev-dynamic ()\n  (let ((n 7)) (lambda () n)))\n"
                                       "(defvar lispm-test-dev-broken (car 3))\n"))
      (should (equal (lispm-test--call agent "eval_definition" '((name . "lispm-test-dev-dynamic") (path . "other.el")))
                     '("Evaluated the definition of lispm-test-dev-dynamic in other.el, line 2.\n=> lispm-test-dev-dynamic"
                       . nil)))
      (should-error (funcall (lispm-test-dev-dynamic)) :type 'void-variable)
      ;; Emacs has it from that file now; the buffer visited for it is gone again.
      (should (file-equal-p (symbol-file 'lispm-test-dev-dynamic 'defun) other))
      (should-not (find-buffer-visiting other))
      ;; An error in the code is the call's.
      (should (equal (lispm-test--call agent "eval_definition"
                                       '((name . "lispm-test-dev-broken") (kind . "variable") (path . "other.el")))
                     '("Wrong type argument: listp, 3" . t))))))

(ert-deftest lispm-test-xref ()
  "The xref tools ask the backend of the code's buffer, with paths relative to the root."
  (lispm-test-with-dev
    (let ((agent (lispm-defagent tester :root project)))
      (should (equal (lispm-test--call agent "xref_definitions" '((identifier . "lispm-test-dev-f")))
                     '("dev.el:10: (defun lispm-test-dev-f)" . nil)))
      (should (equal (lispm-test--call agent "xref_definitions" '((identifier . "lispm-test-dev-var")))
                     '("dev.el:2: (defvar lispm-test-dev-var)" . nil)))
      ;; Asked about an occurrence, in the file's buffer, which is not left behind.
      (should (equal (lispm-test--call agent "xref_definitions"
                                       '((identifier . "lispm-test-dev-f") (path . "dev.el") (line . 15)))
                     '("dev.el:10: (defun lispm-test-dev-f)" . nil)))
      (should-not (find-buffer-visiting file))
      (should (equal (lispm-test--call agent "xref_definitions"
                                       '((identifier . "lispm-test-dev-f") (path . "dev.el") (line . 1)))
                     '("lispm-test-dev-f is not on line 1 of dev.el" . t)))
      (should (equal (lispm-test--call agent "xref_references" '((identifier . "lispm-test-dev-f")))
                     '("dev.el:10: (defun lispm-test-dev-f (x)
dev.el:14: \"Call `lispm-test-dev-f'.\"
dev.el:15: (lispm-test-dev-f 2))" . nil)))
      (should (equal (lispm-test--call agent "xref_definitions" '((identifier . "lispm-test-dev-nowhere")))
                     '("Nothing found for lispm-test-dev-nowhere." . nil))))))

(ert-deftest lispm-test-flymake-diagnostics ()
  "flymake_diagnostics reports what flymake's checkers find, and leaves flymake as it was.
The byte compiler checks only a file Emacs trusts, and the result says so."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (file (expand-file-name "warn.el" project)))
      (lispm-test--write file (concat ";;; warn.el --- A fixture  -*- lexical-binding: t; -*-\n"
                                      "(defun lispm-test-dev-warn (x)\n  (+ x lispm-test-dev-unknown))\n"))
      (pcase-let ((`(,text . ,errorp)
                   (let ((trusted-content :all))
                     (lispm-test--call-async agent "flymake_diagnostics" '((path . "warn.el"))))))
        (should-not errorp)
        (should (string-match-p "\\`warn\\.el: 1 warning, [0-9]+ notes?, from " text))
        (should (string-match-p "^3:[0-9]+: warning: reference to free variable .lispm-test-dev-unknown.$" text))
        (should (string-match-p "^[0-9]+:[0-9]+: note: " text)))
      ;; The buffer visited for it is gone again.
      (should-not (find-buffer-visiting file))
      ;; In a buffer of the user's, flymake stays off.
      (let ((buffer (find-file-noselect file)))
        (pcase-let ((`(,text . ,errorp)
                     (let ((trusted-content nil))
                       (lispm-test--call-async agent "flymake_diagnostics" '((path . "warn.el"))))))
          (should-not errorp)
          ;; Released Emacs 30 and 31 keep checkdoc off untrusted content
          ;; too, so it may say so instead of reporting notes.
          (should (string-match-p (concat "\\`warn\\.el: [0-9]+ notes?, from elisp-flymake-checkdoc\\.\n"
                                          "\\|\\`warn\\.el: no diagnostics, from no checker\\.\n"
                                          "\\[elisp-flymake-checkdoc did not run: .*untrusted")
                                  text))
          (should-not (string-match-p "warning:" text))
          (should (string-match-p "^\\[The byte compiler did not check warn\\.el: .*`trusted-content'" text)))
        (should (buffer-live-p buffer))
        (should-not (buffer-local-value 'flymake-mode buffer)))
      (with-temp-buffer
        (rename-buffer "lispm-test-plain" t)
        (should (string-match-p "\\`Flymake has no checker for lispm-test-plain"
                                (car (lispm-test--call-async agent "flymake_diagnostics"
                                                             `((buffer . ,(buffer-name)))))))))))

(ert-deftest lispm-test-ert-run ()
  "ert_run runs tests in Emacs, from a file it loads, and reports each failure.
This runs ERT inside the ERT run of this suite."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (tests '(lispm-dev-fixture-passes lispm-dev-fixture-fails lispm-dev-fixture-errs
                   lispm-dev-fixture-skips)))
      (lispm-test--write (expand-file-name "dev-tests.el" project)
                         (concat ";;; dev-tests.el --- Fixtures  -*- lexical-binding: t; -*-\n"
                                 "(require 'ert)\n"
                                 "(defun lispm-test-dev-car (x) (car x))\n"
                                 "(ert-deftest lispm-dev-fixture-passes () (should (= (+ 1 1) 2)))\n"
                                 "(ert-deftest lispm-dev-fixture-fails () (should (equal (+ 1 1) 3)))\n"
                                 "(ert-deftest lispm-dev-fixture-errs () (lispm-test-dev-car 3))\n"
                                 "(ert-deftest lispm-dev-fixture-skips () (skip-unless nil))\n"))
      (unwind-protect
          (progn
            (pcase-let ((`(,text . ,errorp)
                         (lispm-test--call agent "ert_run" '((selector . "\\`lispm-dev-fixture-")
                                                             (file . "dev-tests.el")))))
              (should-not errorp)
              (should (string-match-p "\\`Ran 4 tests in [0-9.]+ s: 1 passed, 2 failed, 1 skipped\\.\n" text))
              (should (string-match-p (concat "^FAILED lispm-dev-fixture-fails\n"
                                              "  (ert-test-failed ((should (equal (\\+ 1 1) 3)) :form (equal 2 3)")
                                      text))
              ;; The backtrace ends at the test: ERT's own frames are left out.
              (should (string-match-p (concat "^FAILED lispm-dev-fixture-errs\n"
                                              "  (wrong-type-argument listp 3)\n"
                                              "  Backtrace, innermost call first:\n"
                                              "    (car 3)\n"
                                              "    (lispm-test-dev-car 3)\n")
                                      text))
              (should-not (string-match-p "ert-run-test" text))
              (should (string-match-p "^SKIPPED lispm-dev-fixture-skips: ((skip-unless nil)" text)))
            ;; A test's name selects it alone; a selector in Lisp, those it says.
            (should (string-match-p "\\`Ran 1 test in [0-9.]+ s: 1 passed, 0 failed\\.\\'"
                                    (car (lispm-test--call agent "ert_run"
                                                           '((selector . "lispm-dev-fixture-passes"))))))
            (should (string-match-p "\\`Ran 2 tests in [0-9.]+ s: 1 passed, 1 failed\\."
                                    (car (lispm-test--call
                                          agent "ert_run"
                                          '((selector . "(member lispm-dev-fixture-passes lispm-dev-fixture-fails)"))))))
            (should (string-match-p "\\`No tests match \"lispm-dev-fixture-none\"; if they are in a file"
                                    (car (lispm-test--call agent "ert_run"
                                                           '((selector . "lispm-dev-fixture-none")))))))
        (mapc #'ert-delete-test tests)
        (fmakunbound 'lispm-test-dev-car)))))

(defun lispm-test--write (file text)
  "Write TEXT to FILE, making its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(ert-deftest lispm-test-skills ()
  "Skills are found, loaded with their code and tools, and restored on resume.
search_tools finds a skill and names its tools without loading them; the
skill tool lists none.  A project's skill asks before its code runs;
refused, its text is loaded alone."
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
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t0" "search_tools" '((query . "greet peple"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "skill" '((name . "greet"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "greet_tool" '((who . "Lisp"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t3" "skill" '((name . "local"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (unwind-protect
          (progn
            (lispm-tell agent "Greet Lisp")
            ;; Whatever the policy, the project's code waits for the user.
            (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
            (should (equal (plist-get (lispm-asked (gethash "t3" (lispm-agent-pending agent))) :name)
                           "trust_skill"))
            (lispm-resolve agent "t3" 'deny "not now")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let ((requests (reverse lispm-test--requests)))
              (let ((skill (seq-find (lambda (tool) (equal (alist-get 'name tool) "skill"))
                                     (alist-get 'tools (nth 0 requests)))))
                (should-not (string-match-p "greet\\|local" (alist-get 'description skill))))
              ;; The search names the skill's tool, but leaves it to the skill.
              (should-not (member "greet_tool" (funcall names (nth 0 requests))))
              (should-not (member "greet_tool" (funcall names (nth 1 requests))))
              (should (member "greet_tool" (funcall names (nth 2 requests)))))
            (let ((transcript (lispm-test--transcript agent)))
              (should (string-match-p (concat "^- skill greet: Greet people warmly\\. "
                                              "Its tools: greet_tool\\. Load it with skill\\.$")
                                      transcript))
              (should-not (string-match-p "^Tools now available: greet_tool" transcript)))
            (let ((transcript (lispm-test--transcript agent)))
              (should (string-match-p "^Skill greet loaded; tools now available: greet_tool\\.$" transcript))
              (should (string-match-p "^Use greet_tool\\.$" transcript))
              (should (string-match-p "^Hello, Lisp!$" transcript))
              (should (string-match-p (concat "^Skill local loaded (a project's skill: the user did not let "
                                              "its code run, so it was not evaluated)\\. "
                                              "The user's reason: not now$")
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

(defconst lispm-test--music-skill
  (concat "#+title: Play music\n"
          "#+description: Play music with EMMS: files, playlists and streams.\n\n"
          "* What is there\nThe playlist.\n\n* Playing :care:\nAsk first.\n\n"
          "#+begin_src emacs-lisp :lispm load\n"
          "(defun lispm-test-music-helper () t)\n"
          "(lispm-deftool music_play ((what string \"What.\")) \"Play WHAT with EMMS.\nMore.\"\n"
          "  :permission media :async t\n  (funcall done what))\n"
          "(progn (lispm-deftool music_status () \"Say what plays.\" \"silence\"))\n"
          "(with-eval-after-load 'emms\n"
          "  (lispm-register-tool (lispm-tool--create :name \"music_seek\" :description \"Seek.\"\n"
          "                                           :permission 'write)))\n"
          "#+end_src\n")
  "A skill like LispM Store's music skill, for the tests of finding skills.")

(ert-deftest lispm-test-skill-sources ()
  "Programs register directories of skills; the user's replace theirs by name.
A failing source is passed over; a source's skills carry its origin."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "user-skills/" project))
           (desktop (expand-file-name "desktop-skills/" project))
           (store (expand-file-name "store-skills/" project))
           (lispm-skill-directories (list user-skills))
           (lispm-write-skill-directory nil)
           (lispm-skill-sources
            (list (lambda () (list (list :directory desktop :origin "Test Desktop")))
                  (lambda () (error "Broken"))
                  (lambda () (list (list :directory store :origin "Test Store"))))))
      (lispm-test--write (expand-file-name "windows.org" desktop)
                         "#+title: Windows\n#+description: Arrange windows.\n")
      (lispm-test--write (expand-file-name "music.org" store) lispm-test--music-skill)
      (lispm-test--write (expand-file-name "windows.org" user-skills)
                         "#+title: My windows\n#+description: The user's own.\n")
      (let ((agent (lispm-defagent tester :root project)))
        (should (equal (mapcar #'car (lispm-skills-directories agent))
                       (list (file-name-as-directory desktop) (file-name-as-directory store)
                             (file-name-as-directory user-skills)
                             (expand-file-name ".lispm/skills/" project))))
        (let ((skills (lispm-skills agent)))
          (should (equal (mapcar (lambda (skill) (plist-get skill :name)) skills) '("music" "windows")))
          (should (equal (plist-get (car skills) :origin) "Test Store"))
          ;; The user's skill replaced the desktop's.
          (should (equal (plist-get (cadr skills) :title) "My windows"))
          (should-not (plist-get (cadr skills) :origin)))
        ;; A source naming one of the user's directories brings nothing twice.
        (let ((lispm-skill-sources (list (lambda () (list (list :directory user-skills :origin "X"))))))
          (should (equal (mapcar #'car (lispm-skills-directories agent))
                         (list (file-name-as-directory user-skills)
                               (expand-file-name ".lispm/skills/" project)))))))))

(ert-deftest lispm-test-skill-index ()
  "A skill's tools are read from its code without evaluating it, until it changes."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "user-skills/" project))
           (file (expand-file-name "music.org" user-skills))
           (lispm-skill-directories (list user-skills)))
      (lispm-test--write file lispm-test--music-skill)
      (let ((index (lispm-skills--index (lispm-skills--find "music" nil))))
        (should (equal (plist-get index :tools)
                       '(("music_play" media "Play WHAT with EMMS.")
                         ("music_status" read "Say what plays.")
                         ("music_seek" write "Seek."))))
        (should (equal (plist-get index :headings) '("What is there" "Playing"))))
      (should-not (fboundp 'lispm-test-music-helper))
      (should-not (lispm-get-tool "music_play"))
      ;; Changed, it is read again; code that does not read defines no tools.
      (lispm-test--write file (concat "#+title: Play music\n#+description: Play.\n\n"
                                      "* Broken\n#+begin_src emacs-lisp :lispm load\n"
                                      "(lispm-deftool music_play () \"Play.\"\n#+end_src\n"))
      (set-file-times file (time-add nil 10))
      (let ((index (lispm-skills--index (lispm-skills--find "music" nil))))
        (should-not (plist-get index :tools))
        (should (equal (plist-get index :headings) '("Broken")))))))

(ert-deftest lispm-test-search-score ()
  "The search is fuzzy: parts of words, misspellings, names taken apart at _."
  (let ((score (lambda (query fields) (lispm-tools-search-score (lispm-tools-search-words query) fields))))
    (should (equal (lispm-tools-search-words "Play the music, please") '("play" "music" "please")))
    (should (equal (lispm-tools-search-words "a") '("a")))
    ;; The same word, the start of one, one misspelt.
    (should (= (funcall score "music" '((1 . "Music"))) 1.0))
    (should (> (funcall score "play" '((1 . "the playlist"))) 0))
    (should (> (funcall score "musik" '((1 . "music"))) 0))
    (should (> (funcall score "playing" '((1 . "play"))) 0))
    (should (> (funcall score "play" '((1 . "music_play"))) 0))
    (should (= (funcall score "weather" '((1 . "Play music"))) 0))
    ;; One letter off a short word is another word, not a misspelling.
    (should (= (funcall score "chat" '((1 . "what"))) 0))
    (should (= (funcall score "music" '((1 . "basic"))) 0))
    ;; Exact beats a prefix beats a misspelling.
    (should (> (funcall score "play" '((1 . "play"))) (funcall score "play" '((1 . "playlist")))))
    (should (> (funcall score "music" '((1 . "musical"))) (funcall score "musik" '((1 . "music")))))
    ;; Matching every word beats matching one well.
    (should (> (funcall score "play music" '((1 . "play some music")))
               (funcall score "play music" '((3 . "play")))))
    ;; A field's weight counts.
    (should (> (funcall score "music" '((3 . "music"))) (funcall score "music" '((1 . "music")))))))

(ert-deftest lispm-test-search-cutoff ()
  "What a search finds by chance, far below the best, is left out."
  (let ((candidates '(("app-store" . "Find applications in the App Store and install them.")
                      ("eval_definition" . "Evaluate a definition again, to restore it.")))
        (fields (lambda (candidate) `((3 . ,(car candidate)) (1.5 . ,(cdr candidate))))))
    (should (equal (mapcar #'car (lispm-tools--rank (lispm-tools-search-words "install an app from the store")
                                                    candidates 5 fields))
                   '("app-store")))
    (should (equal (mapcar #'car (lispm-tools--rank (lispm-tools-search-words "restore") candidates 5 fields))
                   '("eval_definition")))))

(ert-deftest lispm-test-search-finds-skills ()
  "search_tools finds skills and the tools of their code, which it leaves to them.
A misspelt or partial word finds them; select: names the skill holding a tool;
a role is kept to; one without the skill tool is shown no skills."
  (lispm-test-env
    (let* ((store (expand-file-name "store-skills/" project))
           (lispm-skill-directories nil)
           (lispm-write-skill-directory (expand-file-name "user-skills/" project))
           (lispm-skill-sources (list (lambda () (list (list :directory store :origin "Test Store")))))
           (agent (lispm-defagent tester :root project))
           (lines (lambda (found) (mapcar (lambda (other) (plist-get other :line)) (cdr found)))))
      (lispm-test--write (expand-file-name "music.org" store) lispm-test--music-skill)
      (lispm-register-tool (lispm-tool--create :name "test_playground" :description "A sandbox to play in."
                                               :deferred t :function #'ignore))
      (unwind-protect
          (progn
            (let ((found (lispm-tools--discover agent "playing musik" 5)))
              (should (equal (funcall lines found)
                             '("- skill music (Test Store): Play music with EMMS: files, playlists and streams. Its tools: music_play, music_status, music_seek. Load it with skill.")))
              ;; A tool matching by chance, far below the skill, is left out ...
              (should-not (car found)))
            ;; ... and found when it matches.
            (should (member "test_playground"
                            (mapcar #'lispm-tool-name (car (lispm-tools--discover agent "play sandbox" 5)))))
            ;; A tool of a skill's code is found by its name, as its skill.
            (should (equal (length (funcall lines (lispm-tools--discover agent "music_status" 5))) 1))
            (should (equal (length (funcall lines (lispm-tools--discover agent "select:music_seek" 5))) 1))
            (should-not (car (lispm-tools--discover agent "select:music_seek" 5)))
            ;; Even registered, the tools of a skill come with it.
            (lispm-register-tool (lispm-tool--create :name "music_play" :description "Play." :deferred t
                                                     :function #'ignore))
            (should-not (member "music_play" (mapcar #'lispm-tool-name (car (lispm-tools--discover agent "play" 5)))))
            ;; A role gives the tools it names, and search_tools with skill.
            (setf (lispm-agent-tools agent) '(read_file skill))
            (should (member "search_tools" (mapcar #'lispm-tool-name (lispm-agent-tools* agent))))
            (let ((found (lispm-tools--discover agent "play" 5)))
              (should-not (car found))
              (should (cdr found)))
            (setf (lispm-agent-tools agent) '(read_file search_tools test_*))
            (let ((found (lispm-tools--discover agent "play" 5)))
              (should (equal (mapcar #'lispm-tool-name (car found)) '("test_playground")))
              (should-not (cdr found)))
            ;; A skill missed by name gets the nearest.
            (let ((lispm-current-agent agent))
              (setf (lispm-agent-tools agent) t)
              (should (string-match-p "No skill named musc (nearest: music)"
                                      (condition-case err
                                          (funcall (lispm-tool-function (lispm-get-tool "skill"))
                                                   #'ignore '((name . "musc")))
                                        (error (error-message-string err)))))))
        (remhash "test_playground" lispm-tools--registry)
        (remhash "music_play" lispm-tools--registry)))))

(ert-deftest lispm-test-core-tools ()
  "Common tools are offered at once; the others are found with search_tools."
  (lispm-test-env
    (let ((names (mapcar #'lispm-tool-name (lispm-agent-tools* (lispm-defagent tester :root project)))))
      (dolist (name '("read_file" "edit_file" "run_shell" "eval_elisp" "apropos" "describe_symbol"
                      "remember" "recall" "task_add" "task_update" "task_list" "ask_agent"
                      "send_message" "return_value" "run_lisp" "search_tools" "skill"))
        (should (member name names)))
      (dolist (name '("write_skill" "read_info" "inspect_agent" "forget" "list_agents"))
        (should-not (member name names))
        (should (lispm-tool-deferred (lispm-get-tool name)))))))

(defun lispm-test--tool-names (request)
  "Names of the tools REQUEST offers."
  (mapcar (lambda (tool) (alist-get 'name tool)) (alist-get 'tools request)))

(defun lispm-test--asked (agent)
  "Name of the tool of the first call AGENT awaits approval for."
  (when-let* ((id (car (lispm-waiting-calls agent))))
    (plist-get (lispm-asked (gethash id (lispm-agent-pending agent))) :name)))

(defmacro lispm-test-with-trust (&rest body)
  "Run BODY with no trusted skills, Customize saving to a scratch file.
The init and custom files are beside `project', so that the user's
own are never written."
  (declare (indent 0))
  `(let ((lispm-trusted-skills nil)
         (user-init-file (expand-file-name "../init.el" project))
         (custom-file (expand-file-name "../custom.el" project)))
     (unwind-protect
         (progn ,@body)
       (put 'lispm-trusted-skills 'saved-value nil)
       (put 'lispm-trusted-skills 'theme-value nil))))

(defconst lispm-test--local-skill
  (concat "#+title: Local\n#+description: A project's own skill.\n\n"
          "#+begin_src emacs-lisp :lispm load\n"
          "(defun lispm-test-local-helper () 1)\n"
          "(lispm-deftool local_tool () \"A project's tool.\" \"local\")\n"
          "#+end_src\n")
  "A project's skill with code, for the tests of trust.")

(ert-deftest lispm-test-skill-trust ()
  "A project's skill code runs once the user lets it: once, or for good, pinned by its hash.
Trusted for good, it runs unasked until it, or a file beside it,
changes; the trust is saved with Customize."
  (lispm-test-env
    (lispm-test-with-trust
      (let* ((lispm-skill-directories (list (expand-file-name "user-skills/" project)))
             (skill (expand-file-name ".lispm/skills/local/SKILL.org" project))
             (beside (expand-file-name ".lispm/skills/local/data.el" project))
             (agent (lispm-defagent tester :root project :policy '((exec . auto)))))
        (lispm-test--write skill lispm-test--local-skill)
        (lispm-test--write beside "(setq lispm-test-data 1)\n")
        (setq lispm-test--scripts
              `((tester ,@(mapcan (lambda (id)
                                    (list (lispm-test--msg "tool_use"
                                                           (lispm-test--tool id "skill" '((name . "local"))))
                                          (lispm-test--msg "end_turn" (lispm-test--text "ok"))))
                                  '("t1" "t2" "t3" "t4")))))
        (unwind-protect
            (progn
              ;; Once: the code runs, the user having seen it.
              (lispm-tell agent "load")
              (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
              (should (equal (lispm-test--asked agent) "trust_skill"))
              (let ((transcript (lispm-test--transcript agent)))
                (should (string-match-p "^\\*\\*\\* Asks: trust_skill  local$" transcript))
                (should (string-match-p "^\"files\": \\[\"data\\.el\"\\]$"
                                        (replace-regexp-in-string "^ +" "" transcript)))
                (should (string-match-p "^\\*\\*\\* Preview\n#\\+begin_src diff\n\\(?:.*\n\\)*\\+(lispm-deftool local_tool"
                                        transcript)))
              (lispm-resolve agent "t1" 'approve)
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should (fboundp 'lispm-test-local-helper))
              (should (string-match-p "^Skill local loaded; tools now available: local_tool\\.$"
                                      (lispm-test--transcript agent)))
              (should (lispm-tool-deferred (lispm-get-tool "local_tool")))
              (should-not lispm-trusted-skills)
              ;; Asked again; for good, the code is pinned, and saved.
              (lispm-tell agent "load again")
              (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
              (lispm-allow agent "t2")
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should (equal lispm-trusted-skills
                             (list (cons (abbreviate-file-name skill) (lispm-skills--hash skill)))))
              (should (string-match-p "lispm-trusted-skills" (lispm-test--file project "../custom.el")))
              (should-not (lispm-agent-allowed agent))
              ;; Then it runs unasked ...
              (fmakunbound 'lispm-test-local-helper)
              (lispm-tell agent "and again")
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should (fboundp 'lispm-test-local-helper))
              ;; ... until a file beside it, which its code may load, changes.
              (lispm-test--write beside "(setq lispm-test-data 2)\n")
              (fmakunbound 'lispm-test-local-helper)
              (lispm-tell agent "once more")
              (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
              (lispm-resolve agent "t4" 'deny)
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should-not (fboundp 'lispm-test-local-helper)))
          (fmakunbound 'lispm-test-local-helper)
          (remhash "local_tool" lispm-tools--registry))))))

(ert-deftest lispm-test-skill-trust-resume ()
  "A resumed session evaluates a project's skill again only while its code is trusted.
`lispm-trust-skill' trusts the code as it is then; with DISTRUST, no
longer.  Code changed while the user is asked is not evaluated."
  (lispm-test-env
    (lispm-test-with-trust
      (let* ((lispm-skill-directories (list (expand-file-name "user-skills/" project)))
             (skill (expand-file-name ".lispm/skills/local.org" project))
             (default-directory project)
             (agent (lispm-start)))
        (lispm-test--write skill lispm-test--local-skill)
        (lispm-trust-skill skill)
        (should (equal (mapcar #'car lispm-trusted-skills) (list (abbreviate-file-name skill))))
        (setq lispm-test--scripts
              `((assistant@project
                 ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "skill" '((name . "local"))))
                 ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
        (unwind-protect
            (let ((file (buffer-file-name (lispm-agent-buffer agent))))
              (lispm-tell agent "load")
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should (fboundp 'lispm-test-local-helper))
              (with-current-buffer (lispm-agent-buffer agent) (save-buffer))
              ;; Trusted as it is: evaluated again.
              (lispm-retire agent)
              (fmakunbound 'lispm-test-local-helper)
              (let ((revived (lispm-resume-file file)))
                (should (fboundp 'lispm-test-local-helper))
                (should (member "local_tool" (mapcar #'lispm-tool-name (lispm-agent-tools* revived))))
                (lispm-retire revived))
              ;; Changed since: not.
              (lispm-test--write skill (string-replace "() 1)" "() 2)" lispm-test--local-skill))
              (fmakunbound 'lispm-test-local-helper)
              (lispm-retire (lispm-resume-file file))
              (should-not (fboundp 'lispm-test-local-helper))
              (lispm-trust-skill skill t)
              (should-not lispm-trusted-skills))
          (fmakunbound 'lispm-test-local-helper)
          (remhash "local_tool" lispm-tools--registry))))
    ;; The code the user is asked about is the code that runs.
    (lispm-test-with-trust
      (let* ((lispm-skill-directories (list (expand-file-name "user-skills/" project)))
             (skill (expand-file-name ".lispm/skills/late.org" project))
             (agent (lispm-defagent tester :root project)))
        (lispm-test--write skill (string-replace "local" "late" lispm-test--local-skill))
        (setq lispm-test--scripts
              `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "skill" '((name . "late"))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
        (lispm-tell agent "load")
        (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
        (lispm-test--write skill (string-replace "() 1)" "() 2)" (string-replace "local" "late" lispm-test--local-skill)))
        (lispm-allow agent "t1")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should (string-match-p "^Skill late loaded (a project's skill: its code changed while the user was asked"
                                (lispm-test--transcript agent)))
        (should-not (fboundp 'lispm-test-late-helper))
        (should-not lispm-trusted-skills)))))

(ert-deftest lispm-test-skill-trust-chat ()
  "The chat shows the code of a project's skill it asks about; 2 trusts it for good."
  (lispm-test-env
    (lispm-test-with-trust
      (let* ((lispm-skill-directories (list (expand-file-name "user-skills/" project)))
             (skill (expand-file-name ".lispm/skills/local.org" project))
             (agent (lispm-defagent tester "Test agent." :root project))
             (chat (lispm-chat agent)))
        (lispm-test--write skill lispm-test--local-skill)
        (setq lispm-test--scripts
              `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "skill" '((name . "local"))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "ok")))))
        (unwind-protect
            (progn
              (lispm-test--chat-type chat "load it")
              (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
              (let ((text (lispm-test--chat-text chat)))
                (should (string-match-p "│ Run the code of the project's skill local" text))
                (should (string-match-p "│   \\+(defun lispm-test-local-helper () 1)" text))
                (should (string-match-p "Do you want to evaluate this code from \\.lispm/skills/local\\.org\\?" text))
                (should (string-match-p "2\\. Yes, and trust this code until it changes" text)))
              (lispm-test--chat-key chat "2")
              (should (lispm-test--wait (lispm-test--idle-p agent)))
              (should (fboundp 'lispm-test-local-helper))
              (should (equal (mapcar #'car lispm-trusted-skills) (list (abbreviate-file-name skill)))))
          (fmakunbound 'lispm-test-local-helper)
          (remhash "local_tool" lispm-tools--registry))))))

(ert-deftest lispm-test-write-skill ()
  "An agent writes a skill: always asked, previewed as its file, then evaluated and saved.
write_skill is deferred, found with search_tools.  The skill's tools are
the agent's at once, deferred; a resumed session loads it again."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "user-skills/" project))
           (lispm-skill-directories (list user-skills))
           (default-directory project)
           (agent (lispm-start))
           (code "(defun lispm-test-shout (text) (upcase text))
(lispm-deftool shout_tool ((text string \"What to shout.\")) \"Shout TEXT.\"
  (lispm-test-shout text))"))
      (setf (lispm-agent-policy agent) '((exec . auto) (write . auto)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t0" "search_tools"
                                                             '((query . "write a skill"))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "write_skill"
                                                             `((name . "shout")
                                                               (description . "Shout things.\nLoudly.")
                                                               (instructions . "Use shout_tool.\n\n* Care\nNot too often.")
                                                               (code . ,code)
                                                               (why . "To keep the tool."))))
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "shout_tool" '((text . "hi"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (unwind-protect
          (let ((file (expand-file-name "shout.org" user-skills)))
            (lispm-tell agent "Keep the shout tool")
            ;; Asked, whatever the policy says, with the file it would be.
            (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
            (should-not (file-exists-p file))
            (should (string-match-p "^\\*\\*\\* Preview\n#\\+begin_src diff\n\\(?:.*\n\\)*\\+#\\+description: Shout things\\. Loudly\\.$"
                                    (lispm-test--transcript agent)))
            (lispm-resolve agent "t1" 'approve)
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (should (equal (lispm-test--file user-skills "shout.org")
                           (concat "#+title: shout\n#+description: Shout things. Loudly.\n\n"
                                   "Use shout_tool.\n\n* Care\nNot too often.\n\n* The code\n\n"
                                   "#+begin_src emacs-lisp :lispm load\n" code "\n#+end_src\n")))
            (let ((transcript (lispm-test--transcript agent)))
              (should (string-match-p "^Skill shout loaded; tools now available: shout_tool\\.$" transcript))
              (should (string-match-p "^Saved as .*/user-skills/shout\\.org: " transcript))
              (should (string-match-p "^HI$" transcript)))
            (let ((requests (reverse lispm-test--requests)))
              ;; Deferred: found with search_tools first.
              (should-not (member "write_skill" (lispm-test--tool-names (nth 0 requests))))
              (should (member "write_skill" (lispm-test--tool-names (nth 1 requests))))
              (should-not (member "shout_tool" (lispm-test--tool-names (nth 1 requests))))
              (should (member "shout_tool" (lispm-test--tool-names (nth 2 requests)))))
            ;; Deferred: another agent does not carry it until it loads the skill.
            (should (lispm-tool-deferred (lispm-get-tool "shout_tool")))
            (should-not (member "shout_tool" (mapcar #'lispm-tool-name
                                                     (lispm-agent-tools* (lispm-defagent other :root project)))))
            (should (equal (plist-get (lispm-skills--find "shout" agent) :file) file))
            ;; A resumed session loads it again.
            (let ((session (buffer-file-name (lispm-agent-buffer agent))))
              (with-current-buffer (lispm-agent-buffer agent) (save-buffer))
              (lispm-retire agent)
              (fmakunbound 'lispm-test-shout)
              (remhash "shout_tool" lispm-tools--registry)
              (let ((revived (lispm-resume-file session)))
                (should (fboundp 'lispm-test-shout))
                (should (member "shout_tool" (mapcar #'lispm-tool-name (lispm-agent-tools* revived)))))))
        (remhash "shout_tool" lispm-tools--registry)
        (fmakunbound 'lispm-test-shout)))))

(ert-deftest lispm-test-write-skill-checks ()
  "write_skill always asks, refuses bad names and skills it would not replace,
and saves nothing whose code fails or does not read; a skill whose code does
not read is loaded without it."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "user-skills/" project))
           (lispm-skill-directories (list lispm-skills--builtin user-skills))
           (agent (lispm-defagent tester :root project :policy '((exec . auto))))
           (tool (lispm-get-tool "write_skill"))
           (input (lambda (name &optional code)
                    `((name . ,name) (description . "Does things.") (instructions . "How.")
                      ,@(and code `((code . ,code))))))
           (write (lambda (name &optional code)
                    (lispm-test--call agent "write_skill" (funcall input name code)))))
      (unwind-protect
          (progn
            ;; No policy lets it run unasked, nor does approving one call the next.
            (should (eq (lispm-tool-action agent tool (funcall input "x")) 'ask))
            (should (equal (lispm-tools-allowance tool '((name . "x") (why . "y")))
                           '("write_skill" (name . "x"))))
            (should (eq (lispm-tool-action (lispm-defagent t2 :root project :policy '((exec . deny)))
                                           tool (funcall input "x"))
                        'deny))
            (dolist (name '("../evil" "a/b" "Web" "" ".hidden"))
              (let ((result (funcall write name)))
                (should (cdr result))
                (should (string-match-p "\\`The name of a skill is" (car result)))))
            (should-not (directory-files-recursively (expand-file-name "../" user-skills) "evil"))
            ;; One of LispM's own, or of the project, is not replaced.
            (should (string-match-p "\\`There is a skill named web already" (car (funcall write "web"))))
            (lispm-test--write (expand-file-name ".lispm/skills/mine.org" project) "#+title: Mine\n")
            (should (string-match-p "\\`There is a skill named mine already" (car (funcall write "mine"))))
            ;; Code that fails is not saved; what it defined first is the agent's, deferred.
            (let ((result (funcall write "broken" "(lispm-deftool lispm_test_half () \"Half.\" 1)\n(error \"Boom\")")))
              (should (cdr result))
              (should (string-match-p "\\`The code of skill broken failed, so the skill was not saved: Boom"
                                      (car result)))
              (should-not (file-exists-p (expand-file-name "broken.org" user-skills)))
              (should (lispm-tool-deferred (lispm-get-tool "lispm_test_half")))
              (should (member "lispm_test_half" (lispm-agent-loaded-tools agent))))
            ;; Nor code that does not read, which is not evaluated at all.
            (let ((result (funcall write "open" "(defun lispm-test-first () 1)\n(defun lispm-test-open ()")))
              (should (string-match-p "\\`Skill open was not saved: The code does not read: the form at its line 2"
                                      (car result)))
              (should-not (fboundp 'lispm-test-first))
              (should-not (file-exists-p (expand-file-name "open.org" user-skills))))
            ;; A skill found so, written by hand, is loaded without its code.
            (lispm-test--write (expand-file-name "unread.org" user-skills)
                               (concat "#+title: Unread\n#+description: Does things.\n\nText.\n\n"
                                       "#+begin_src emacs-lisp :lispm load\n(defun lispm-test-first () 1)\n"
                                       "(defun lispm-test-unread ()\n#+end_src\n"))
            (should (string-match-p (concat "\\`Skill unread loaded (its code was not evaluated: The code "
                                            "does not read: the form at its line 2 is not closed)\\.\n\nText\\.")
                                    (car (lispm-test--call agent "skill" '((name . "unread"))))))
            (should-not (fboundp 'lispm-test-first))
            ;; Written again, a skill is replaced, previewed against what it was.
            (should-not (cdr (funcall write "twice" "(defun lispm-test-twice () 1)")))
            (should (string-match-p "^-(defun lispm-test-twice () 1)\n\\+(defun lispm-test-twice () 2)$"
                                    (let ((lispm-current-agent agent))
                                      (lispm-tool-preview-text
                                       tool (funcall input "twice" "(defun lispm-test-twice () 2)")))))
            (should-not (cdr (funcall write "twice" "(defun lispm-test-twice () 2)")))
            (should (= (lispm-test-twice) 2)))
        (remhash "lispm_test_half" lispm-tools--registry)
        (fmakunbound 'lispm-test-twice)))))

(ert-deftest lispm-test-eval-defines-tools ()
  "A tool an agent defines with eval_elisp is its own at once, deferred: in no other's core layer.
Defined before an error, it is too; a core tool defined again stays core."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project :policy '((exec . auto))))
          (code "(lispm-deftool lispm_test_made () \"Made by an agent.\" \"made\") :defined"))
      (unwind-protect
          (progn
            (setq lispm-test--scripts
                  `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "eval_elisp" `((code . ,code))))
                            ,(lispm-test--msg "tool_use" (lispm-test--tool "t2" "lispm_test_made" nil))
                            ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))
                    (other ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "search_tools"
                                                                         '((query . "select:lispm_test_made"))))
                           ,(lispm-test--msg "end_turn" (lispm-test--text "Found.")))))
            (lispm-tell agent "Make a tool")
            (should (lispm-test--wait (lispm-test--idle-p agent)))
            (let ((requests (reverse lispm-test--requests)))
              (should-not (member "lispm_test_made" (lispm-test--tool-names (nth 0 requests))))
              ;; Added after the others: the request prefix changes once.
              (should (equal (seq-take (lispm-test--tool-names (nth 1 requests))
                                       (length (lispm-test--tool-names (nth 0 requests))))
                             (lispm-test--tool-names (nth 0 requests))))
              (should (member "lispm_test_made" (lispm-test--tool-names (nth 1 requests)))))
            (let ((transcript (lispm-test--transcript agent)))
              (should (string-match-p "^=> :defined\n\nTools now available: lispm_test_made\\.$" transcript))
              (should (string-match-p "^made$" transcript)))
            (should (lispm-tool-deferred (lispm-get-tool "lispm_test_made")))
            ;; Another agent does not carry it, but finds it.
            (let ((other (lispm-defagent other :root project)))
              (should-not (member "lispm_test_made" (mapcar #'lispm-tool-name (lispm-agent-tools* other))))
              (lispm-tell other "Find it")
              (should (lispm-test--wait (lispm-test--idle-p other)))
              (should (member "lispm_test_made" (lispm-test--tool-names (car lispm-test--requests)))))
            ;; Defined before an error, it is the agent's all the same.
            (let ((result (lispm-test--call agent "eval_elisp"
                                            '((code . "(lispm-deftool lispm_test_half () \"Half.\" 1) (error \"Boom\")")))))
              (should (cdr result))
              (should (string-match-p "\\`Boom\n" (car result)))
              (should (string-match-p "^Tools now available: lispm_test_half\\.$" (car result)))
              (should (lispm-tool-deferred (lispm-get-tool "lispm_test_half")))
              (should (member "lispm_test_half" (lispm-agent-loaded-tools agent))))
            ;; A tool of the core layer, defined again, stays there.
            (lispm-deftool lispm_test_core () "A tool of the user's." "one")
            (should (equal (car (lispm-test--call agent "eval_elisp"
                                                  '((code . "(lispm-deftool lispm_test_core () \"Again.\" \"two\")
                                                             (funcall (lispm-tool-function (lispm-get-tool \"lispm_test_core\")))"))))
                           "=> two"))
            (should-not (lispm-tool-deferred (lispm-get-tool "lispm_test_core"))))
        (dolist (name '("lispm_test_made" "lispm_test_half" "lispm_test_core"))
          (remhash name lispm-tools--registry))))))

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

;;;; Shell commands and remote roots

(ert-deftest lispm-test-shell-class ()
  "Shell commands have a class of their own, apart from Lisp.  A policy
that does not name it gives it what it gives exec, as ceilings recorded
before the split do."
  (lispm-test-env
    (let ((shell (lispm-get-tool "run_shell"))
          (elisp (lispm-get-tool "eval_elisp"))
          (action (lambda (policy class)
                    (let ((agent (lispm-make-agent 'assistant project)))
                      (setf (lispm-agent-policy agent) policy)
                      (lispm-policy-action agent class)))))
      (should (eq (lispm-tool-permission shell) 'shell))
      (should (eq (lispm-tool-permission elisp) 'exec))
      ;; Its calls say why, as those of exec do.
      (should (assq 'why (alist-get 'properties (alist-get 'input_schema (lispm-tool-schema shell)))))
      ;; Both ask by default.
      (let ((agent (lispm-make-agent 'assistant project)))
        (should (eq (lispm-tool-action agent shell '((command . "ls"))) 'ask))
        (should (eq (alist-get 'shell (lispm-policy agent)) 'ask)))
      ;; A policy naming exec alone governs shell commands too ...
      (should (eq (funcall action '((exec . auto)) 'shell) 'auto))
      (should (eq (funcall action '((exec . deny)) 'shell) 'deny))
      ;; ... and one naming shell sets them apart.
      (should (eq (funcall action '((shell . auto)) 'shell) 'auto))
      (should (eq (funcall action '((shell . auto)) 'exec) 'ask))
      (should (eq (funcall action '((shell . deny) (exec . auto)) 'shell) 'deny))
      ;; So does a default policy customized before the split.
      (let ((lispm-default-policy '((read . auto) (exec . auto))))
        (should (eq (funcall action nil 'shell) 'auto)))
      ;; A ceiling recorded before the split bounds shell commands as it
      ;; bounds Lisp; one recorded since, by its own entry.
      (let ((agent (lispm-make-agent 'assistant project))
            (caller (lispm-make-agent 'assistant project)))
        (setf (lispm-agent-policy agent) '((shell . auto) (exec . auto))
              (lispm-agent-ceiling agent) '((read . auto) (exec . ask)))
        (should (eq (lispm-policy-action agent 'shell) 'ask))
        (setf (lispm-agent-policy caller) '((shell . auto)))
        (setf (lispm-agent-ceiling agent) (lispm-policy caller))
        (should (eq (alist-get 'shell (lispm-agent-ceiling agent)) 'auto))
        (should (eq (lispm-policy-action agent 'shell) 'auto))
        (should (eq (lispm-policy-action agent 'exec) 'ask))))))

(ert-deftest lispm-test-shell-allowance ()
  "Approving a shell command for good allows that exact command, unless the
agent's root is on another machine: there, the tool.  Lisp, which runs in
Emacs wherever the root is, is allowed call by call."
  (lispm-test-env
    (let ((agent (lispm-make-agent 'assistant project))
          (shell (lispm-get-tool "run_shell"))
          (input '((why . "w") (command . "make"))))
      (should (equal (lispm-tools-allowance shell input agent) '("run_shell" (command . "make"))))
      (should (equal (lispm-tools-allowance shell input) '("run_shell" (command . "make"))))
      ;; Remote, but this machine.
      (dolist (root (list "/sudo::/srv/" "/su::/srv/" "/ssh:localhost:/srv/"
                          (format "/ssh:%s:/srv/" (system-name))))
        (setf (lispm-agent-root agent) root)
        (should-not (lispm-tools-elsewhere-p root))
        (should (consp (lispm-tools-allowance shell input agent))))
      (dolist (root '("/ssh:vm:/work/" "/docker:box:/work/"))
        (setf (lispm-agent-root agent) root)
        (should (lispm-tools-elsewhere-p root))
        (should (equal (lispm-tools-allowance shell input agent) "run_shell"))
        (should (equal (lispm-tools-allowance (lispm-get-tool "eval_elisp") '((code . "1")) agent)
                       '("eval_elisp" (code . "1"))))))))

;;;; The sandbox

(defmacro lispm-test-with-fake-srt (log &rest body)
  "Run BODY with a stand-in for srt bound to `lispm-sandbox-srt-program'.
It runs commands unconfined, after copying its settings to
settings.json in the directory LOG names, and appends the lines of
its control channel to the file control there."
  (declare (indent 1))
  `(let* ((,log (make-temp-file "lispm-srt-" t))
          (lispm-sandbox-srt-program (expand-file-name "srt" ,log))
          (lispm-sandbox--runs nil))
     (with-temp-file lispm-sandbox-srt-program
       (insert "#!/bin/sh\n"
               "log=" (shell-quote-argument ,log) "\n"
               "while [ \"$1\" != -- ]; do\n"
               "  [ \"$1\" = --settings ] && cp \"$2\" \"$log/settings.json\"\n"
               "  shift\n"
               "done\n"
               "shift\n"
               "while read -r line; do echo \"$line\" >> \"$log/control\"; done <&3 2>/dev/null &\n"
               "exec \"$@\"\n"))
     (set-file-modes lispm-sandbox-srt-program #o755)
     (unwind-protect (progn ,@body)
       (delete-directory ,log t))))

(defun lispm-test--json-file (file)
  "The JSON object in FILE, as an alist with lists for arrays."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(ert-deftest lispm-test-sandbox-levels ()
  "Each level gives srt settings stricter than the one before; none gives none.
No level lets commands write where Emacs and LispM keep their files."
  (lispm-test-env
    (let ((lispm-sandbox-allowed-domains '("example.com"))
          (lispm-sandbox-secrets '("~/.ssh")))
      (should-not (lispm-sandbox-settings 'none))
      (let-alist (json-parse-string (json-serialize (lispm-sandbox-settings 'low))
                                    :object-type 'alist :array-type 'list)
        (should (member "example.com" .network.allowedDomains))
        (should (member "registry.npmjs.org" .network.allowedDomains))
        (should (equal .filesystem.denyRead '("~/.ssh")))
        (should (member "." .filesystem.allowWrite))
        (should (member "~/.npm" .filesystem.allowWrite))
        (should (member "/tmp" .filesystem.allowWrite)))
      (let-alist (json-parse-string (json-serialize (lispm-sandbox-settings 'middle))
                                    :object-type 'alist :array-type 'list)
        (should (equal .network.allowedDomains '("example.com")))
        (should (member "/tmp" .filesystem.allowWrite))
        (should-not (member "~/.npm" .filesystem.allowWrite)))
      (let-alist (json-parse-string (json-serialize (lispm-sandbox-settings 'high))
                                    :object-type 'alist :array-type 'list)
        (should (null .network.allowedDomains))
        (should (equal .filesystem.denyRead '("~")))
        (should (member "." .filesystem.allowRead))
        ;; The project, and the sandbox's own temporary directory.
        (should (equal .filesystem.allowWrite (list "." (lispm-sandbox--tmpdir)))))
      (dolist (level '(low middle high))
        (let ((protected (plist-get (plist-get (lispm-sandbox-settings level) :filesystem)
                                    :denyWrite)))
          (should (seq-contains-p protected (directory-file-name (expand-file-name lispm-directory))))
          (should (seq-contains-p protected (directory-file-name
                                             (expand-file-name user-emacs-directory))))))
      (should (equal (lispm-sandbox-settings) (lispm-sandbox-settings lispm-sandbox-level)))
      (should-error (lispm-sandbox-settings 'extreme) :type 'lispm-sandbox-error))))

(ert-deftest lispm-test-sandbox-custom ()
  "The level custom has the rules of `lispm-sandbox-custom', Middle's while
there are none; they reach every option srt has for them, and are checked."
  (lispm-test-env
    (let ((lispm-sandbox-custom nil)
          (lispm-sandbox-secrets '("~/.ssh" "~/.ssh")))
      (should (equal (lispm-sandbox-settings 'custom) (lispm-sandbox-settings 'middle)))
      ;; Settings leave the lists they are made of as they were.
      (should (equal lispm-sandbox-secrets '("~/.ssh" "~/.ssh")))
      (setq lispm-sandbox-custom
            '(:deny-read ("~/.ssh") :allow-read ("~/.ssh/config") :allow-write (".")
              :deny-write ("secrets") :allowed-domains ("github.com" "example.com")
              :denied-domains ("gist.github.com") :local-binding t
              :unix-sockets ("/var/run/docker.sock") :pty t :apple-events t))
      (let-alist (json-parse-string (json-serialize (lispm-sandbox-settings 'custom))
                                    :object-type 'alist :array-type 'list :false-object nil)
        (should (equal .network.allowedDomains '("github.com" "example.com")))
        (should (equal .network.deniedDomains '("gist.github.com")))
        (should (eq .network.allowLocalBinding t))
        (should (equal .network.allowUnixSockets '("/var/run/docker.sock")))
        (should (equal .filesystem.denyRead '("~/.ssh")))
        (should (equal .filesystem.allowRead '("~/.ssh/config")))
        (should (equal .filesystem.allowWrite (list "." (lispm-sandbox--tmpdir))))
        (should (equal (car .filesystem.denyWrite) "secrets"))
        (should (member (directory-file-name (expand-file-name lispm-directory))
                        .filesystem.denyWrite))
        (should (eq .allowPty t))
        (should (eq .allowAppleEvents t)))
      (should (equal (lispm-sandbox-level-description 'custom)
                     (concat "reads all but ~/.ssh (except ~/.ssh/config); writes .; "
                             "reaches github.com, example.com; uses local ports; "
                             "connects to /var/run/docker.sock; uses terminals; opens applications")))
      (should (string-prefix-p "sandbox Custom: reads all but ~/.ssh"
                               (lispm-sandbox-run-note
                                (lispm-sandbox--make-run :started 'custom :settings '(:network nil)))))
      ;; What srt would refuse, LispM refuses first, saying why.
      (dolist (rules '((:allowed-domains ("*")) (:allow-everything t) "rules"))
        (setq lispm-sandbox-custom rules)
        (should-error (lispm-sandbox-settings 'custom) :type 'lispm-sandbox-error))
      ;; Customizing starts from Middle's rules, or replaces them by a level's.
      (cl-letf (((symbol-function 'customize-variable) #'ignore))
        (setq lispm-sandbox-custom nil)
        (lispm-sandbox-customize)
        (should (equal lispm-sandbox-custom (lispm-sandbox-rules 'middle)))
        (lispm-sandbox-customize 'high)
        (should (equal (plist-get lispm-sandbox-custom :deny-read) '("~")))
        (should-not (eq (plist-get lispm-sandbox-custom :deny-read)
                        (plist-get (lispm-sandbox-rules 'high) :deny-read)))))))

(ert-deftest lispm-test-sandbox-run-shell ()
  "Shell commands run through srt at the level set, and a change of level
reaches a command that runs.  The result says what the sandbox allowed."
  (lispm-test-env
    (lispm-test-with-fake-srt log
      (let ((agent (lispm-defagent tester :root project))
            (lispm-sandbox-level 'middle)
            result)
        (let ((lispm-current-agent agent))
          (lispm-tool-invoke (lispm-get-tool "run_shell")
                             '((command . "echo hi; sleep 1; pwd"))
                             (lambda (text errorp) (setq result (cons text errorp)))))
        (let ((run (car lispm-sandbox--runs)))
          (should (eq (lispm-sandbox-run-started run) 'middle))
          (should (file-exists-p (lispm-sandbox-run-fifo run)))
          ;; Set while the command runs, the level reaches it.
          (setq lispm-sandbox-level 'high)
          (should (lispm-test--wait (lambda () result) 10))
          (should (string-match-p
                   "\\`hi\n.*project/?\n\n?\\[exit status 0; sandbox Middle: reads all but secrets"
                   (car result)))
          (should-not (cdr result))
          ;; It started with Middle's settings, and was sent High's.
          (let-alist (lispm-test--json-file (expand-file-name "settings.json" log))
            (should (equal .filesystem.denyRead lispm-sandbox-secrets)))
          (should (lispm-test--wait
                   (lambda () (file-exists-p (expand-file-name "control" log)))))
          (let-alist (json-parse-string
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name "control" log))
                        (buffer-string))
                      :object-type 'alist :array-type 'list)
            (should (equal .filesystem.denyRead '("~"))))
          ;; Once it has exited, its channel is gone.
          (should (lispm-test--wait (lambda () (null lispm-sandbox--runs))))
          (should-not (file-exists-p (lispm-sandbox-run-fifo run))))
        ;; At none, a command runs as before.
        (setq result nil lispm-sandbox-level 'none)
        (let ((lispm-current-agent agent))
          (lispm-tool-invoke (lispm-get-tool "run_shell") '((command . "echo plain"))
                             (lambda (text errorp) (setq result (cons text errorp)))))
        (should (lispm-test--wait (lambda () result)))
        (should (equal (car result) "plain\n\n[exit status 0]"))))))

(ert-deftest lispm-test-sandbox-unavailable ()
  "Without srt, a command that should be sandboxed fails rather than run outside it."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project))
           (lispm-sandbox-level 'low)
           (lispm-sandbox-srt-program (expand-file-name "no-srt" project))
           (marker (expand-file-name "ran" project))
           (result (lispm-test--call agent "run_shell" `((command . ,(concat "touch " marker))))))
      (should (cdr result))
      (should (string-match-p "Sandbox unavailable: the srt program .*no-srt is not found" (car result)))
      (should-not (file-exists-p marker)))))

(ert-deftest lispm-test-sandbox-srt-status ()
  "srt's status says whether it runs, is being installed, or why it cannot be
had; the change hook hears an installation start and end."
  (lispm-test-with-fake-srt log
    (should (eq (lispm-sandbox-srt-status) 'ready)))
  (let ((lispm-sandbox-srt-program "lispm-test-no-srt"))
    (should (equal (lispm-sandbox-srt-status)
                   '(failed . "the srt program lispm-test-no-srt is not found"))))
  (lispm-test-env
    (let* ((bin (expand-file-name "bin" project))
           (npm (expand-file-name "npm" bin))
           (exec-path (cons bin exec-path))
           (lispm-sandbox-srt-program nil)
           (lispm-sandbox--installer nil)
           (lispm-sandbox--install-failure nil)
           (told 0)
           (lispm-sandbox-change-hook (list (lambda () (setq told (1+ told))))))
      (make-directory bin t)
      ;; An npm that cannot reach its registry.
      (with-temp-file npm
        (insert "#!/bin/sh\nread -r go\necho 'npm error network unreachable' >&2\nexit 1\n"))
      (set-file-modes npm #o755)
      (should-not (lispm-sandbox-srt-status))
      (lispm-sandbox-install)
      (should (eq (lispm-sandbox-srt-status) 'installing))
      (should (= told 1))
      (process-send-string lispm-sandbox--installer "go\n")
      (should (lispm-test--wait (lambda () (= told 2))))
      (should (equal (lispm-sandbox-srt-status)
                     '(failed . "srt could not be installed: npm error network unreachable"))))))

(ert-deftest lispm-test-sandbox-mcp ()
  "An MCP server started over stdio runs in the sandbox, at its own level if
it has one; a change of the level set then passes it by."
  (lispm-test-env
    (lispm-test-with-fake-srt log
      (lispm-test-with-mcp (:sandbox high)
        (let ((lispm-sandbox-level 'low))
          (lispm-mcp-start "fake")
          (should (lispm-test--wait (lambda () (lispm-get-tool "mcp__fake__echo")) 15))
          (let ((run (car lispm-sandbox--runs)))
            (should (equal (lispm-sandbox-run-name run) "the MCP server fake"))
            (should (eq (lispm-sandbox-run-level run) 'high)))
          (let-alist (lispm-test--json-file (expand-file-name "settings.json" log))
            (should (equal .filesystem.denyRead '("~"))))
          (setq lispm-sandbox-level 'middle)
          (lispm-sandbox--changed)
          (should-not (file-exists-p (expand-file-name "control" log)))
          (lispm-mcp-stop "fake")
          (should (lispm-test--wait (lambda () (null lispm-sandbox--runs)))))))))

(ert-deftest lispm-test-sandbox-chat ()
  "/sandbox sets the level, which every chat's header shows."
  (lispm-test-env
    (lispm-test-with-fake-srt _log
      (let* ((lispm-sandbox-level 'none)
             (agent (lispm-start nil project))
             (chat (lispm-chat agent)))
        (should-not (string-match-p "sandbox:" (lispm-test--chat-text chat)))
        (lispm-chat--command agent "/sandbox High")
        (should (eq lispm-sandbox-level 'high))
        (should (string-match-p "│   sandbox: High (/sandbox to change) +│" (lispm-test--chat-text chat)))
        (let ((lispm-sandbox-custom nil) edited)
          (lispm-chat--command agent "/sandbox custom")
          (should (string-match-p "│   sandbox: Custom (/sandbox to change) +│"
                                  (lispm-test--chat-text chat)))
          (cl-letf (((symbol-function 'customize-variable)
                     (lambda (variable) (setq edited variable))))
            (lispm-chat--command agent "/sandbox edit"))
          (should (eq edited 'lispm-sandbox-custom))
          (should (eq lispm-sandbox-level 'custom)))
        (should-error (lispm-chat--command agent "/sandbox extreme") :type 'user-error)))))

(ert-deftest lispm-test-sandbox-srt ()
  "The real srt, named by the environment variable LISPM_TEST_SRT, confines a
command: High lets it write only the project; Middle hides secrets."
  (skip-unless (and (getenv "LISPM_TEST_SRT") (file-executable-p (getenv "LISPM_TEST_SRT"))))
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project))
           (lispm-sandbox-srt-program (getenv "LISPM_TEST_SRT"))
           (outside (make-temp-file "lispm-outside-" t))
           (secret (expand-file-name "key" outside)))
      (unwind-protect
          (progn
            (lispm-test--write secret "hidden")
            (let* ((lispm-sandbox-level 'high)
                   (result (lispm-test--wait-call
                            agent (format "echo in > in.txt && cat in.txt; echo out > %s/out.txt"
                                          outside))))
              (should (string-match-p "\\`in\n" result))
              (should (string-match-p "Operation not permitted" result))
              (should (file-exists-p (expand-file-name "in.txt" project)))
              (should-not (file-exists-p (expand-file-name "out.txt" outside))))
            (let* ((lispm-sandbox-level 'middle)
                   (lispm-sandbox-secrets (list outside))
                   (result (lispm-test--wait-call agent (concat "cat " secret))))
              (should (string-match-p "Operation not permitted" result))
              (should-not (string-match-p "hidden" result)))
            ;; Custom rules: the project is writable but for one file.
            (let* ((lispm-sandbox-level 'custom)
                   (lispm-sandbox-custom `(:deny-read (,outside) :allow-write (".")
                                           :deny-write ("locked.txt")))
                   (result (lispm-test--wait-call
                            agent (concat "echo a > free.txt && echo free; echo b > locked.txt; cat "
                                          secret))))
              (should (string-match-p "\\`free\n" result))
              (should (file-exists-p (expand-file-name "free.txt" project)))
              (should-not (file-exists-p (expand-file-name "locked.txt" project)))
              (should-not (string-match-p "hidden" result))
              (should (string-match-p "sandbox Custom: reads all but" result))))
        (delete-directory outside t)))))

(defun lispm-test--wait-call (agent command)
  "The output of AGENT's run_shell COMMAND, once it has run."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke (lispm-get-tool "run_shell") `((command . ,command))
                       (lambda (text _errorp) (setq result text)))
    (lispm-test--wait (lambda () result) 30)
    result))

(defmacro lispm-test-with-remote (&rest body)
  "Run BODY in `lispm-test-env' with `remote', `project' as a remote directory.
It is reached through TRAMP's mock method, which Emacs's own tests of
remote files use (`ert-remote-temporary-file-directory'): a shell on
this machine.  Files are changed only through it, since TRAMP keeps
what it learns of them a while."
  (declare (indent 0))
  `(lispm-test-env
     (skip-unless (and ert-remote-temporary-file-directory
                       (string-prefix-p "/mock:" ert-remote-temporary-file-directory)))
     (let ((remote (concat (file-remote-p ert-remote-temporary-file-directory) project))
           ;; Listings show the files the agent made, without backups.
           (make-backup-files nil))
       (unwind-protect
           (progn ,@body)
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b) (string-prefix-p remote (buffer-file-name b)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (tramp-cleanup-connection (tramp-dissect-file-name remote) t t)))))

(ert-deftest lispm-test-sandbox-remote ()
  "The sandbox confines processes of this machine: on a remote root, a
command it should confine fails."
  (lispm-test-with-remote
    (lispm-test-with-fake-srt _log
      (let* ((agent (lispm-make-agent 'assistant remote))
             (lispm-sandbox-level 'middle)
             (result (lispm-test--call agent "run_shell" '((command . "echo hi")))))
        (should (cdr result))
        (should (string-match-p "\\`Sandbox unavailable: it confines processes of this machine"
                                (car result)))))))

(ert-deftest lispm-test-remote-root ()
  "An agent whose root is remote reads, writes, lists, searches and runs
commands there; absolute paths name files there too."
  (lispm-test-with-remote
    (lispm-test--write (expand-file-name "a.txt" project) "alpha\nbeta\n")
    (let* ((agent (lispm-make-agent 'assistant remote))
           (write (lispm-get-tool "write_file")))
      (setf (lispm-agent-policy agent) '((write . auto) (shell . auto)))
      (should (equal (lispm-agent-root agent) remote))
      (let ((lispm-current-agent agent))
        (should (equal (lispm-tools--file "a.txt") (concat remote "a.txt")))
        ;; An absolute path naming no machine is on the root's.
        (should (equal (lispm-tools--file (expand-file-name "a.txt" project)) (concat remote "a.txt")))
        (should (equal (lispm-tools--file "[[file:/etc/hosts::3]]")
                       (concat (file-remote-p remote) "/etc/hosts"))))
      (should (string-match-p "1\talpha\n +2\tbeta"
                              (car (lispm-test--call agent "read_file" '((path . "a.txt"))))))
      ;; Writes under the root run unasked; outside it, they ask.
      (should (eq (lispm-tool-action agent write '((path . "b.txt"))) 'auto))
      (should (eq (lispm-tool-action agent write `((path . ,(expand-file-name "b.txt" project)))) 'auto))
      (should (eq (lispm-tool-action agent write '((path . "/etc/b.txt"))) 'ask))
      (should (eq (lispm-tool-action agent write `((path . ,(expand-file-name "b.txt" tmp)))) 'ask))
      (should-not (cdr (lispm-test--call agent "edit_file" '((path . "a.txt") (old_text . "beta")
                                                            (new_text . "gamma")))))
      (should-not (cdr (lispm-test--call agent "write_file" '((path . "sub/b.txt") (content . "b\n")))))
      (should (equal (lispm-test--disk (expand-file-name "a.txt" project)) "alpha\ngamma\n"))
      (should (equal (lispm-test--disk (expand-file-name "sub/b.txt" project)) "b\n"))
      (should (equal (car (lispm-test--call agent "list_directory" nil))
                     (concat (abbreviate-file-name remote) "\na.txt\nsub/")))
      (should (equal (car (lispm-test--call-async agent "search_files" '((pattern . "gam+a"))))
                     "a.txt:2:gamma"))
      ;; Commands run in the root, on its machine, with that machine's
      ;; shell, not the one Emacs runs locally.
      (let* ((shell-file-name "/nonexistent/shell")
             (result (lispm-test--call-async agent "run_shell" '((command . "ls -1; exit 2")))))
        (should (equal (car result) "a.txt\nsub\n\n[exit status 2]")))
      ;; What a command changed is seen at once: TRAMP forgets the
      ;; listing it had ...
      (lispm-test--call-async agent "run_shell" '((command . "echo c > c.txt")))
      (should (equal (car (lispm-test--call agent "list_directory" nil))
                     (concat (abbreviate-file-name remote) "\na.txt\nc.txt\nsub/")))
      ;; ... and buffers are reverted.
      (let ((buffer (find-buffer-visiting (concat remote "a.txt"))))
        (should buffer)
        (lispm-test--call-async agent "run_shell"
                                '((command . "printf 'changed\\n' > a.txt; touch -t 203001010000 a.txt")))
        (should (equal (with-current-buffer buffer (buffer-string)) "changed\n")))
      ;; Its turns have the in-memory journal only.
      (should-not (lispm-journal--top remote))
      (should-not (lispm-journal-records agent))
      ;; The model is told where it works.
      (should (string-match-p "Your shell commands run there" (lispm--environment-prompt agent))))))

(ert-deftest lispm-test-remote-turn ()
  "An agent started from a buffer of another machine works there: a whole turn,
told where it is, its command run there unasked under ((shell . auto))."
  (lispm-test-with-remote
    (lispm-test--write (expand-file-name "AGENTS.md" project) "Build with make.\n")
    (setq lispm-test--scripts
          `((assistant@project
             ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "run_shell"
                                                            '((why . "w") (command . "echo hi > d.txt"))))
             ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
    (let ((agent (let ((default-directory remote)) (lispm-start))))
      (should (equal (lispm-agent-root agent) remote))
      (setf (lispm-agent-policy agent) '((shell . auto)))
      (lispm-tell agent "make d")
      (should (lispm-test--wait (lispm-test--idle-p agent) 30))
      (should (equal (lispm-test--disk (expand-file-name "d.txt" project)) "hi\n"))
      (should (string-match-p "^\\*\\* DONE run_shell" (lispm-test--transcript agent)))
      (let ((system (format "%S" (alist-get 'system (car lispm-test--requests)))))
        (should (string-match-p "Build with make" system))
        (should (string-match-p "Your shell commands run there" system))))))

(ert-deftest lispm-test-remote-worktree ()
  "A linked git worktree on another machine is the project of its main worktree
there: the names its .git file and commondir give are names on that machine."
  (skip-unless (executable-find "git"))
  (lispm-test-with-remote
    (let* ((main (lispm-test--git-repo (file-truename project)))
           (linked (expand-file-name "../linked/" main))
           (host (file-remote-p remote)))
      (lispm-test--git main "worktree" "add" "-q" "-b" "side" linked)
      (should (equal (lispm--main-worktree (concat host linked)) (concat host main)))
      (should (equal (lispm--main-worktree (concat host linked "lisp/"))
                     (concat host main "lisp/"))))))

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
      (should (equal (lispm-waiting-calls agent) '("t1")))
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
        (should (equal (lispm-waiting-calls revived) '("t2")))
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
              (should (equal (lispm-waiting-calls revived) '("l1")))
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
      (should (eq (lispm-stop-restart (lispm-agent-condition agent)) 'abort))
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
       (let ((id (progn (should (lispm-test--wait (lambda () (lispm-asked-condition agent))))
                        (plist-get (lispm-asked-condition agent) :id))))
         (ignore id)
         ,@body))))

(ert-deftest lispm-test-tool-error-stops ()
  "A failure `lispm-debug-tool-errors' covers stops the call, recorded on its heading."
  (lispm-test-with-stopped-call
    (let ((condition (lispm-asked-condition agent)))
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
      (should (lispm-test--wait (lambda () (lispm-asked-condition agent))))
      (let ((revived (lispm-resume-file (lispm-test--crash agent))))
        (should (eq (lispm-agent-state revived) 'waiting))
        (should (eq (plist-get (lispm-asked-condition revived) :type) 'tool-error))
        (should (string-match-p "No such file" (plist-get (lispm-asked-condition revived) :error)))
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
       (should (lispm-test--wait (lambda () (equal (lispm-waiting-calls agent) '("t1" "t2")))))
       (lispm-resolve agent "t1" 'approve)
       (should (lispm-test--wait (lambda () (lispm--call-condition agent "t1"))))
       ,@body)))

(ert-deftest lispm-test-tool-error-not-approval ()
  "A stopped call does not await approval: approving its tool for good leaves it alone."
  (lispm-test-with-stopped-write
    (should (equal (lispm-waiting-calls agent) '("t2")))
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
    (should (equal (lispm-waiting-calls agent) '("t2"))))
  (lispm-test-with-stopped-write
    (let ((lispm-condition-handlers '((tool-error retry))))
      ;; Stopped before the handler was set: offer it again.
      (lispm--offer-condition agent (lispm--call-condition agent "t1"))
      (should (lispm-test--wait (lambda () (equal (lispm-waiting-calls agent) '("t1" "t2")))))
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

;;;; Role files

(ert-deftest lispm-test-role-files ()
  "A role file defines a role: its keywords Lisp data, the text after them its instructions.
An earlier directory's file hides a later one's.  A file that is no role file, names
a backend or a model, or names a role defined in Lisp is left out; a role whose file
is gone is removed."
  (lispm-test-env
    (let* ((first (expand-file-name "../roles/" project))
           (second (expand-file-name "../more-roles/" project))
           (lispm-role-directories (list first second (expand-file-name "../nowhere/" project)))
           (reviewer (expand-file-name "reviewer.org" first))
           (problem (lambda (file text)
                      (cadr (should-error (lispm-roles--read (expand-file-name file first) text)
                                          :type 'user-error)))))
      ;; No directory is no error.
      (should-not (lispm-load-roles))
      (lispm-test--write reviewer
                         (concat "#+title: Reviewer\n#+description: Reviews changes.\n"
                                 "#+tools: (read_file \"search_files\")\n#+policy: ((write . deny))\n"
                                 "#+effort: high\n\nPoint out bugs.\n\n* Style\nBe brief.\n"))
      (lispm-test--write (expand-file-name "reviewer.org" second) "#+description: Hidden.\n")
      (lispm-test--write (expand-file-name "helper.org" second) "#+DESCRIPTION: Helps.\n#+effort: \"max\"\n")
      (lispm-test--write (expand-file-name "broken.org" first) "#+tools: (read_file\n")
      (lispm-test--write (expand-file-name "modeled.org" first) "#+model: \"some-model\"\n\nText.\n")
      (lispm-test--write (expand-file-name "assistant.org" first) "#+description: Not this one.\n")
      (lispm-test--write (expand-file-name "Shout.org" first) "#+description: Not this name.\n")
      (lispm-test--write (expand-file-name "notes.txt" first) "No role.\n")
      (should (equal (lispm-load-roles) '(reviewer helper)))
      (let ((role (lispm-get-role 'reviewer)))
        (should (equal (lispm-role-description role) "Reviews changes."))
        (should (equal (lispm-role-instructions role) "Point out bugs.\n\n* Style\nBe brief."))
        (should (equal (lispm-role-tools role) '(read_file "search_files")))
        (should (equal (lispm-role-policy role) '((write . deny))))
        (should (equal (lispm-role-effort role) "high"))
        (should-not (or (lispm-role-backend role) (lispm-role-model role)))
        (should (equal (lispm-role-file role) reviewer)))
      (let ((helper (lispm-get-role 'helper)))
        (should (equal (lispm-role-description helper) "Helps."))
        (should (eq (lispm-role-tools helper) t))
        (should (equal (lispm-role-effort helper) "max"))
        (should-not (lispm-role-instructions helper)))
      (should-not (seq-some (lambda (name) (lispm-get-role name t)) '(broken modeled Shout)))
      (should-not (lispm-role-file 'assistant))
      (should (equal (lispm-role-description (lispm-get-role 'assistant))
                     "A general assistant for whatever the user is doing in Emacs."))
      (should (string-match-p "#\\+tools: is not Lisp data" (funcall problem "x.org" "#+tools: (read_file\n")))
      (should (string-match-p "#\\+tools: is more than one Lisp object" (funcall problem "x.org" "#+tools: (a) (b)\n")))
      (should (string-match-p "#\\+tools: is t, for every tool, or a list" (funcall problem "x.org" "#+tools: 3\n")))
      (should (string-match-p "#\\+policy: is an alist" (funcall problem "x.org" "#+policy: ((write . maybe))\n")))
      (should (string-match-p "#\\+effort: is an effort level" (funcall problem "x.org" "#+effort: 3\n")))
      (should (string-match-p "#\\+backend: a role file names no backend or model"
                              (funcall problem "x.org" "#+backend: openai\n")))
      (should (string-match-p "#\\+title: is there twice" (funcall problem "x.org" "#+title: A\n#+title: B\n")))
      (should (string-match-p "\\`The name of a role is" (funcall problem "Shout.org" "")))
      (should (string-match-p "\\`A role file is NAME\\.org" (funcall problem "notes.txt" "")))
      ;; Its agents have what it says, and the user's model.
      (let ((agent (lispm-make-agent 'reviewer project)))
        (should (equal (lispm-agent-effort* agent) "high"))
        (should (equal (lispm-agent-model* agent) "test-model"))
        (should (equal (mapcar #'lispm-tool-name (lispm-agent-tools* agent)) '("read_file" "search_files")))
        (should (eq (lispm-policy-action agent 'write) 'deny))
        ;; Defined again in Lisp, the role is no longer its file's, nor loaded from it again.
        (lispm-defrole reviewer "In Lisp.")
        (should-not (lispm-role-file 'reviewer))
        (should (equal (lispm-load-roles) '(helper)))
        (should (equal (lispm-role-description (lispm-get-role 'reviewer)) "In Lisp."))
        (should (equal (lispm-agent-description agent) "In Lisp.")))
      ;; A role whose file is gone is removed.
      (delete-file (expand-file-name "helper.org" second))
      (should-not (lispm-load-roles))
      (should-not (lispm-get-role 'helper t))
      ;; Set with Customize, the directories are loaded again.
      (let ((third (expand-file-name "../third-roles/" project)))
        (lispm-test--write (expand-file-name "tester.org" third) "#+description: Tests.\n")
        (funcall (get 'lispm-role-directories 'custom-set) 'lispm-role-directories (list third))
        (should (equal (lispm-role-file 'tester) (expand-file-name "tester.org" third)))))))

(ert-deftest lispm-test-install-role ()
  "A role file is installed in the first role directory, and registered at once.
It is shown first, or the diff against the role file it replaces, and asked about;
with NOCONFIRM it is not.  What is installed is the text shown.  A role defined in
Lisp is not replaced, nor is a file that is no role file installed."
  (lispm-test-env
    (let* ((roles (expand-file-name "../roles/" project))
           (lispm-role-directories (list roles (expand-file-name "../more-roles/" project)))
           (download (expand-file-name "../download/" project))
           (source (expand-file-name "reviewer.org" download))
           (target (expand-file-name "reviewer.org" roles))
           (text "#+title: Reviewer\n#+description: Reviews changes.\n#+tools: (read_file)\n\nPoint out bugs.\n")
           (better (string-replace "Point out bugs." "Point out every bug." text))
           (answer nil)
           asked shown)
      (lispm-test--write source text)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (push prompt asked)
                   (setq shown (with-current-buffer "*LispM role*"
                                 (buffer-substring-no-properties (point-min) (point-max))))
                   (if (functionp answer) (funcall answer) answer))))
        ;; Refused, nothing is installed.
        (should (string-match-p "\\`The role reviewer was not installed"
                                (cadr (should-error (lispm-install-role source) :type 'user-error))))
        (should (equal asked '("Install the role reviewer, as shown? ")))
        (should (equal shown text))
        (should-not (file-exists-p target))
        (should-not (lispm-get-role 'reviewer t))
        (setq answer t)
        (should (eq (lispm-install-role source) 'reviewer))
        (should (equal (lispm-test--disk target) text))
        (should (equal (lispm-role-file 'reviewer) target))
        (should (equal (lispm-role-instructions (lispm-get-role 'reviewer)) "Point out bugs."))
        ;; Replacing it shows the diff; its live agents take the new role on.
        (let ((agent (lispm-make-agent 'reviewer project)))
          (lispm-test--write source better)
          (setq asked nil)
          (should (eq (lispm-install-role source) 'reviewer))
          (should (equal asked '("Replace the role reviewer, as shown? ")))
          (should (string-match-p "^-Point out bugs\\.\n\\+Point out every bug\\.$" shown))
          (should (equal (lispm-test--disk target) better))
          (should (equal (lispm-agent-instructions agent) "Point out every bug.")))
        ;; What is installed is what was shown, whatever the file holds meanwhile.
        (lispm-test--write source text)
        (setq answer (lambda () (lispm-test--write source "#+description: Changed.\n") t))
        (lispm-install-role source)
        (should (equal (lispm-test--disk target) text)))
      ;; With NOCONFIRM nothing is asked.
      (lispm-test--write (expand-file-name "helper.org" download) "#+description: Helps.\n")
      (lispm-test--never-asking
        (should (eq (lispm-install-role (expand-file-name "helper.org" download) t) 'helper)))
      (should (file-exists-p (expand-file-name "helper.org" roles)))
      ;; A role defined in Lisp is not replaced.
      (lispm-defrole coder "In Lisp.")
      (dolist (name '("coder" "assistant"))
        (let ((file (expand-file-name (concat name ".org") download)))
          (lispm-test--write file "#+description: A role file.\n")
          (should (string-match-p (format "\\`There is a role named %s already, defined in Lisp" name)
                                  (cadr (should-error (lispm-test--never-asking (lispm-install-role file t))
                                                      :type 'user-error))))
          (should-not (file-exists-p (expand-file-name (concat name ".org") roles)))))
      (should (equal (lispm-role-description (lispm-get-role 'coder)) "In Lisp."))
      (lispm-test--write (expand-file-name "notes.txt" download) "#+description: No role.\n")
      (should-error (lispm-install-role (expand-file-name "notes.txt" download) t) :type 'user-error)
      ;; An installed role is loaded again whenever LispM loads.
      (lispm-unregister-role 'reviewer)
      (should-not (lispm-get-role 'reviewer t))
      (should (equal (lispm-load-roles) '(helper reviewer)))
      (should (equal (lispm-role-instructions (lispm-get-role 'reviewer)) "Point out bugs.")))))

(ert-deftest lispm-test-delete-role ()
  "Deleting a role deletes its file from every role directory, and unregisters it.
Live agents of the role keep what they had from it.  A role defined in Lisp, the
default role and one there is none of are not deleted; interactively, it asks first."
  (lispm-test-env
    (let* ((roles (expand-file-name "../roles/" project))
           (later (expand-file-name "../more-roles/" project))
           (lispm-role-directories (list roles later))
           (problem (lambda (name)
                      (cadr (should-error (lispm-test--never-asking (lispm-delete-role name))
                                          :type 'user-error)))))
      (lispm-test--write (expand-file-name "reviewer.org" roles) "#+description: Reviews.\n\nPoint out bugs.\n")
      (lispm-test--write (expand-file-name "reviewer.org" later) "#+description: Hidden.\n")
      (lispm-test--write (expand-file-name "helper.org" roles) "#+description: Helps.\n")
      (should (equal (lispm-load-roles) '(helper reviewer)))
      (let ((agent (lispm-make-agent 'reviewer project)))
        ;; Asked interactively; refused, nothing is deleted.
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
          (should-error (funcall-interactively #'lispm-delete-role 'reviewer) :type 'user-error))
        (should (file-exists-p (expand-file-name "reviewer.org" roles)))
        (should (lispm-get-role 'reviewer t))
        (lispm-test--never-asking
          (should (eq (lispm-delete-role "reviewer") 'reviewer)))
        (should-not (lispm-get-role 'reviewer t))
        (should-not (lispm-role-file 'reviewer))
        (should-not (file-exists-p (expand-file-name "reviewer.org" roles)))
        (should-not (file-exists-p (expand-file-name "reviewer.org" later)))
        (should (equal (lispm-agent-description agent) "Reviews."))
        (should (equal (lispm-agent-instructions agent) "Point out bugs."))
        (should-error (lispm-make-agent 'reviewer project) :type 'user-error))
      (lispm-defrole coder "In Lisp.")
      (should (string-match-p "\\`The role assistant is LispM" (funcall problem 'assistant)))
      (should (string-match-p "\\`The role coder is defined in Lisp" (funcall problem 'coder)))
      (should (string-match-p "\\`No role named nobody" (funcall problem 'nobody)))
      (let ((lispm-default-role 'helper))
        (should (string-match-p "\\`The role helper is .lispm-default-role" (funcall problem 'helper))))
      (should (lispm-get-role 'coder t))
      (should (equal (lispm-role-file 'helper) (expand-file-name "helper.org" roles)))
      (should (file-exists-p (expand-file-name "helper.org" roles))))))

;;;; Installing skills

(defun lispm-test--shown (name)
  "The text of the buffer NAME, which is then killed; nil if there is none."
  (when-let* ((buffer (get-buffer name)))
    (prog1 (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer buffer))))

(ert-deftest lispm-test-install-skill ()
  "A skill, a file or a directory, is installed among the user's skills, and so trusted.
Its code is shown first, or the diff against the skill it replaces, and asked about;
with NOCONFIRM it is not.  Code changed meanwhile is not installed, nor is code that
does not read, a skill named as one of LispM's own, or one badly named."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "../user-skills/" project))
           (lispm-skill-directories (list lispm-skills--builtin user-skills))
           (lispm-write-skill-directory nil)
           (download (expand-file-name "../download/" project))
           (greet (expand-file-name "greet.org" download))
           (greet-text (concat "#+title: Greet\n#+description: Greet people.\n\nUse it.\n\n"
                               "#+begin_src emacs-lisp :lispm load\n"
                               "(defun lispm-test-installed-greet () \"hi\")\n#+end_src\n"))
           (tools (expand-file-name "tools/" download))
           (installed (expand-file-name "tools/" user-skills))
           (agent (lispm-defagent tester :root project))
           (answer nil)
           asked)
      (lispm-test--write greet greet-text)
      (lispm-test--write (expand-file-name "SKILL.org" tools)
                         "#+description: Tools.\n\n#+begin_src emacs-lisp :lispm load\n(load \"helper\")\n#+end_src\n")
      (lispm-test--write (expand-file-name "helper.el" tools) "(setq lispm-test-x 1)\n")
      (lispm-test--write (expand-file-name "old.el" tools) "(setq lispm-test-y 1)\n")
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (prompt)
                         (push prompt asked)
                         (if (functionp answer) (funcall answer) answer))))
              ;; Asked, showing its code; refused, nothing is installed.
              (should (string-match-p "\\`Skill greet was not installed"
                                      (cadr (should-error (lispm-install-skill greet) :type 'user-error))))
              (should (equal asked '("Install the skill greet, as shown? ")))
              (should (string-match-p "^(defun lispm-test-installed-greet () \"hi\")$"
                                      (lispm-test--shown "*LispM skill code*")))
              (should-not (file-exists-p (expand-file-name "greet.org" user-skills)))
              ;; Agreed, it is the user's skill, whose code agents evaluate unasked.
              (setq answer t)
              (should (equal (lispm-install-skill greet) "greet"))
              (should (equal (lispm-test--disk (expand-file-name "greet.org" user-skills)) greet-text))
              (let ((skill (lispm-skills--find "greet" agent)))
                (should (equal (plist-get skill :file) (expand-file-name "greet.org" user-skills)))
                (should (lispm-skill-trusted-p skill)))
              (should (string-prefix-p "Skill greet loaded."
                                       (car (lispm-test--call agent "skill" '((name . "greet"))))))
              (should (fboundp 'lispm-test-installed-greet))
              ;; A directory, with NOCONFIRM: nothing is asked.
              (lispm-test--never-asking
                (should (equal (lispm-install-skill tools t) "tools")))
              (should (equal (lispm-test--disk (expand-file-name "helper.el" installed)) "(setq lispm-test-x 1)\n"))
              (should (file-exists-p (expand-file-name "old.el" installed)))
              ;; Replacing it shows the diff, files beside it too, and replaces it whole.
              (lispm-test--write (expand-file-name "helper.el" tools) "(setq lispm-test-x 2)\n")
              (lispm-test--write (expand-file-name "data/new.txt" tools) "New.\n")
              (delete-file (expand-file-name "old.el" tools))
              (setq asked nil)
              (should (equal (lispm-install-skill tools) "tools"))
              (should (equal asked '("Replace the skill tools, as shown? ")))
              (let ((diff (lispm-test--shown "*LispM skill diff*")))
                (should (string-match-p "^\\+\\+\\+ b/tools/helper\\.el\n.*\n-(setq lispm-test-x 1)\n\\+(setq lispm-test-x 2)$"
                                        diff))
                (should (string-match-p "^\\+\\+\\+ b/tools/data/new\\.txt$" diff))
                (should (string-match-p "^-(setq lispm-test-y 1)$" diff))
                (should-not (string-match-p "SKILL\\.org" diff)))
              (should (equal (lispm-test--disk (expand-file-name "helper.el" installed)) "(setq lispm-test-x 2)\n"))
              (should (file-exists-p (expand-file-name "data/new.txt" installed)))
              (should-not (file-exists-p (expand-file-name "old.el" installed)))
              ;; Nothing is left of the copy it was put in place from.
              (should-not (directory-files user-skills nil "\\`\\.[^.]"))
              ;; Code changed while the user was asked is not installed.
              (setq answer (lambda ()
                             (lispm-test--write (expand-file-name "helper.el" tools) "(setq lispm-test-x 3)\n")
                             t))
              (should (string-match-p "\\`The code of skill tools changed while you were asked"
                                      (cadr (should-error (lispm-install-skill tools) :type 'user-error))))
              (should (equal (lispm-test--disk (expand-file-name "helper.el" installed)) "(setq lispm-test-x 2)\n")))
            ;; A skill installed as a directory replaces the file of that name.
            (lispm-test--write (expand-file-name "again/greet/SKILL.org" download) "#+description: Greet again.\n")
            (lispm-test--never-asking
              (should (equal (lispm-install-skill (expand-file-name "again/greet/" download) t) "greet")))
            (should-not (file-exists-p (expand-file-name "greet.org" user-skills)))
            (should (equal (plist-get (lispm-skills--find "greet" agent) :description) "Greet again."))
            ;; What is not installed.
            (let ((problem (lambda (name text)
                             (let ((file (expand-file-name name download)))
                               (lispm-test--write file text)
                               (prog1 (cadr (should-error (lispm-test--never-asking (lispm-install-skill file t))))
                                 (should-not (file-exists-p (expand-file-name (file-name-nondirectory file)
                                                                              user-skills))))))))
              (should (string-match-p "\\`Skill open was not installed: The code does not read"
                                      (funcall problem "open.org" (concat "#+description: Open.\n\n"
                                                                          "#+begin_src emacs-lisp :lispm load\n"
                                                                          "(defun lispm-test-open ()\n#+end_src\n"))))
              (should (string-match-p "\\`There is a skill named web already"
                                      (funcall problem "web.org" "#+description: Mine.\n")))
              (should (string-match-p "\\`The name of a skill is" (funcall problem "Bad.org" "#+description: Bad.\n")))
              (should (string-match-p "\\`A skill is NAME\\.org" (funcall problem "notes.txt" "Notes.\n"))))
            (lispm-test--write (expand-file-name "empty/README.org" download) "#+description: None.\n")
            (should (string-match-p "holds no SKILL\\.org"
                                    (cadr (should-error (lispm-install-skill (expand-file-name "empty/" download) t)
                                                        :type 'user-error))))
            (should-not (file-exists-p (expand-file-name "empty" user-skills))))
        (fmakunbound 'lispm-test-installed-greet)))))

(ert-deftest lispm-test-delete-skill ()
  "Deleting a skill deletes it from the user's skills, a file or a directory.
One of LispM's own, of a project or of another skill directory is not deleted, nor
is one badly named; interactively, it asks first."
  (lispm-test-env
    (let* ((user-skills (expand-file-name "../user-skills/" project))
           (other-skills (expand-file-name "../other-skills/" project))
           (lispm-skill-directories (list lispm-skills--builtin other-skills user-skills))
           (lispm-write-skill-directory nil)
           (agent (lispm-defagent tester :root project))
           (problem (lambda (name)
                      (cadr (should-error (lispm-test--never-asking (lispm-delete-skill name)))))))
      (lispm-test--write (expand-file-name "greet.org" user-skills) "#+description: Greet.\n")
      (lispm-test--write (expand-file-name "tools/SKILL.org" user-skills) "#+description: Tools.\n")
      (lispm-test--write (expand-file-name "tools/helper.el" user-skills) "(setq lispm-test-x 1)\n")
      (lispm-test--write (expand-file-name "theirs.org" other-skills) "#+description: Theirs.\n")
      (lispm-test--write (expand-file-name ".lispm/skills/mine.org" project) "#+description: Mine.\n")
      ;; Asked interactively; refused, nothing is deleted.
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (should-error (funcall-interactively #'lispm-delete-skill "greet") :type 'user-error))
      (should (file-exists-p (expand-file-name "greet.org" user-skills)))
      (lispm-test--never-asking
        (should (equal (lispm-delete-skill "greet") "greet"))
        (should (equal (lispm-delete-skill "tools") "tools")))
      (should-not (file-exists-p (expand-file-name "greet.org" user-skills)))
      (should-not (file-exists-p (expand-file-name "tools/" user-skills)))
      (should-not (lispm-skills--find "greet" agent))
      (should (string-match-p "\\`Skill web is one of LispM" (funcall problem "web")))
      (should (string-match-p "\\`Skill theirs is in .*/other-skills/, not among" (funcall problem "theirs")))
      (should (string-match-p "\\`No skill named mine in" (funcall problem "mine")))
      (with-temp-buffer
        (setq-local lispm-chat-agent agent)
        (should (string-match-p "\\`Skill mine is the project" (funcall problem "mine"))))
      (should (string-match-p "\\`The name of a skill is" (funcall problem "../other-skills/theirs")))
      (should (lispm-skills--find "web" agent))
      (should (file-exists-p (expand-file-name "theirs.org" other-skills)))
      (should (file-exists-p (expand-file-name ".lispm/skills/mine.org" project))))))

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

(defun lispm-test--sized (message tokens)
  "MESSAGE, reporting that its request held TOKENS input tokens."
  (cons `(usage (input_tokens . ,tokens) (output_tokens . 5))
        (assq-delete-all 'usage (copy-alist message))))

(defun lispm-test--results (request)
  "The tool results REQUEST carries, as an alist (ID . CONTENT)."
  (cl-loop for message across (alist-get 'messages request)
           append (cl-loop for block across (alist-get 'content message)
                           when (equal (alist-get 'type block) "tool_result")
                           collect (cons (alist-get 'tool_use_id block)
                                         (alist-get 'content block)))))

(ert-deftest lispm-test-context-size ()
  "A session's size is the one the model reported, plus bytes written since."
  (lispm-test-env
    (let ((agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(cons '(usage (input_tokens . 100) (cache_creation_input_tokens . 20)
                                     (cache_read_input_tokens . 3000) (output_tokens . 5))
                             (lispm-test--msg "end_turn" (lispm-test--text "One."))))))
      ;; Before any response, a token for every three bytes: here a
      ;; character, where the old count by characters said a third.
      (lispm-session-buffer agent)
      (let ((before (lispm--estimated-tokens agent)))
        (lispm--in-session agent (insert (make-string 300 ?∀)))
        (should (= (- (lispm--estimated-tokens agent) before) 300))
        (lispm--in-session agent (delete-char -300)))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      ;; Cached tokens count too; what was written since is estimated.
      (pcase-let ((`(,tokens . ,bytes) (lispm-agent-context agent)))
        (should (= tokens 3120))
        (should (= (lispm--estimated-tokens agent)
                   (+ 3120 (/ (- (lispm--session-bytes agent) bytes) 3))))))))

(ert-deftest lispm-test-context-size-hands-over ()
  "A session the model reports as large is handed over, though its text is short."
  (lispm-test-env
    (let ((lispm-compact-threshold 3000)
          (lispm-compact-save-memory nil)
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "Small."))
                      ,(lispm-test--sized (lispm-test--msg "end_turn" (lispm-test--text "Large.")) 5000)
                      ,(lispm-test--msg "end_turn" (lispm-test--text "The summary."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Three.")))))
      (lispm-tell agent "first")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((old (lispm-agent-buffer agent)))
        (lispm-tell agent "second")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        ;; Reported as small, the session went on.
        (should (eq (lispm-agent-buffer agent) old))
        (lispm-tell agent "third")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should-not (eq (lispm-agent-buffer agent) old))
        (should (string-match-p "^\\* Summary\nThe summary\\.\n" (lispm-test--transcript agent)))
        ;; The sizes reported for the old session went with it.
        (should (< (car (lispm-agent-context agent)) 3000))))))

(ert-deftest lispm-test-clear-results ()
  "Past the threshold, older tool output is cleared first; the session is
handed over once nothing more can be."
  (lispm-test-env
    (let ((lispm-compact-threshold 3000)
          (lispm-compact-save-memory nil)
          (lispm-compact-keep-results 1)
          (lispm-compact-clear-at-least 1000)
          (agent (lispm-defagent tester "Test agent." :root project)))
      (dolist (name '("a" "b"))
        (with-temp-file (expand-file-name (concat name ".txt") project)
          (dotimes (i 100) (insert (format "%s-line %d %s\n" name i (make-string 80 ?x))))))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file"
                                                                     '((why . "look") (path . "a.txt"))))
                      ,(lispm-test--sized (lispm-test--msg "tool_use"
                                                           (lispm-test--tool "t2" "read_file"
                                                                             '((why . "look") (path . "b.txt"))))
                                          5000)
                      ,(lispm-test--sized (lispm-test--msg "end_turn" (lispm-test--text "Read both.")) 5000)
                      ,(lispm-test--msg "end_turn" (lispm-test--text "The summary."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Next.")))))
      (lispm-tell agent "Read a and b")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((requests (reverse lispm-test--requests)))
        ;; The request after the first result carried it whole; the one
        ;; after the second, grown past the threshold, only the newest.
        (should (string-match-p "a-line 99" (cdr (assoc "t1" (lispm-test--results (nth 1 requests))))))
        (let ((results (lispm-test--results (nth 2 requests))))
          (should (equal (cdr (assoc "t1" results)) lispm-transcript-cleared-text))
          (should (string-match-p "b-line 99" (cdr (assoc "t2" results))))))
      ;; The output stays in the session, tagged so that the user sees it.
      (let ((text (lispm-test--transcript agent)))
        (should (= (cl-count-if (lambda (line) (string-match-p "^\\*\\*\\* Result" line))
                                (split-string text "\n"))
                   2))
        (should (string-match-p "^\\*\\*\\* Result :cleared:\n#\\+begin_example\n *1\ta-line 0" text)))
      ;; Still past it, with nothing more to clear: handed over.
      (let ((old (lispm-agent-buffer agent)))
        (lispm-tell agent "Go on")
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (should-not (eq (lispm-agent-buffer agent) old))
        (should (string-match-p "^\\* Summary\nThe summary\\.\n\\* User\nGo on\n"
                                (lispm-test--transcript agent))))
      ;; Too small a gain is not worth the cache: nothing is cleared.
      (with-temp-buffer
        (insert "* Assistant\n** DONE read_file\n:PROPERTIES:\n:TOOL_ID:  t1\n:TOOL:     read_file\n:END:\n"
                "*** Result\n#+begin_example\nshort\n#+end_example\n"
                "** DONE read_file\n:PROPERTIES:\n:TOOL_ID:  t2\n:TOOL:     read_file\n:END:\n"
                "*** Result\n#+begin_example\nshort\n#+end_example\n")
        (setf (lispm-agent-buffer agent) (current-buffer))
        (should-not (lispm--clear-results agent))
        (should-not (string-match-p ":cleared:" (buffer-string)))
        (let ((lispm-compact-clear-at-least 0))
          (should (lispm--clear-results agent)))
        (should (equal (mapcar #'caddr (lispm-transcript-results)) '(t nil)))))))

(ert-deftest lispm-test-transcript-tag ()
  "Tags are added to a heading's own, once."
  (with-temp-buffer
    (insert "*** Result\n** Reply :keep:\n")
    (lispm-transcript-tag 12 "cleared")
    (lispm-transcript-tag 1 "cleared")
    (lispm-transcript-tag 1 "cleared")
    (should (equal (buffer-string) "*** Result :cleared:\n** Reply :keep:cleared:\n"))
    (should (equal (plist-get (lispm-transcript--heading 22) :tags) '("keep" "cleared")))))

(ert-deftest lispm-test-compaction-keeps-turns ()
  "A handover keeps the latest turns after the summary, if they are small enough."
  (lispm-test-env
    (let ((lispm-compact-threshold 3000)
          (agent (lispm-defagent tester "Test agent." :root project)))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "end_turn" (lispm-test--text "One."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Two."))
                      ,(lispm-test--sized (lispm-test--msg "end_turn" (lispm-test--text "Three.")) 5000)
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Saved."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "The summary."))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Four.")))))
      (dolist (input '("first" "second" "third"))
        (lispm-tell agent input)
        (should (lispm-test--wait (lispm-test--idle-p agent))))
      (lispm-tell agent "fourth")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p agent))
                                                (= (length lispm-test--requests) 6)))))
      (let* ((requests (reverse lispm-test--requests))
             (summary-request (nth 4 requests))
             (final (nth 5 requests)))
        ;; The summary covers what comes before the turns kept.
        (should (equal (alist-get 'text (aref (alist-get 'content (car (last (append (alist-get 'messages summary-request) nil)))) 0))
                       (concat lispm--summary-prompt " "
                               (format lispm--summary-kept-prompt "second"))))
        (should (equal (mapcar (lambda (m) (mapconcat (lambda (b) (alist-get 'text b))
                                                      (alist-get 'content m) "|"))
                               (alist-get 'messages final))
                       '("[Summary of the earlier conversation, handed over to this session]\nThe summary.|second"
                         "Two." "third" "Three." "fourth"))))
      ;; The turn that saved memory for the handover is not kept.
      (should (string-match-p "^\\* Summary\nThe summary\\.\n\\* User\nsecond\n\\* Assistant\n\\*\\* Reply\nTwo\\.\n\\* User\nthird\n\\* Assistant\n\\*\\* Reply\nThree\\.\n\\* User\nfourth\n\\* Assistant\n\\*\\* Reply\nFour\\.\n\\* User\n\\'"
                              (lispm-test--transcript agent)))
      ;; Turns that would take more than a quarter of the threshold are
      ;; left to the summary, and so are conditions; one turn always is.
      (with-temp-buffer
        (insert "* System\nYou.\n* User\none\n* Assistant\nOne.\n"
                "* User\n" (make-string 3000 ?y) "\n* Assistant\nTwo.\n"
                "* User\nthree\n* Assistant\nThree.\n"
                "* DONE Condition: Response paused\n:PROPERTIES:\n:CONDITION: paused\n:END:\nPaused.\n"
                "* User\n")
        (setf (lispm-agent-buffer agent) (current-buffer))
        (should (equal (lispm--kept-turns agent)
                       '("* User\nthree\n* Assistant\nThree.\n" . "three")))
        (let ((lispm-compact-keep-turns 5)
              (lispm-compact-threshold 100000))
          (should (equal (cdr (lispm--kept-turns agent)) (concat (make-string 59 ?y) "…"))))
        (let ((lispm-compact-keep-turns 0))
          (should-not (lispm--kept-turns agent)))))))

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

(ert-deftest lispm-test-chat-italics ()
  "Replies show Markdown emphasis, *TEXT* or _TEXT_, in italics, but not
inside words, as in 2*3*4 or snake_case, nor inside code."
  (let ((italic (lambda (text)
                  (let ((shown (lispm-chat--inline text)))
                    (list (substring-no-properties shown)
                          (apply #'string (seq-filter (lambda (i) (memq 'italic (ensure-list (get-text-property i 'face shown))))
                                                      (number-sequence 0 (1- (length shown)))))
                          shown)))))
    (pcase-let ((`(,text ,_ ,shown) (funcall italic "Playing *Calm* from _Quiet Rooms_.")))
      (should (equal text "Playing Calm from Quiet Rooms."))
      (should (memq 'italic (ensure-list (get-text-property 8 'face shown))))
      (should (memq 'italic (ensure-list (get-text-property 18 'face shown))))
      (should-not (get-text-property 0 'face shown)))
    (dolist (plain '("2*3*4 is 24" "run_shell and snake_case_names" "* a list item" "code `a*b*c` stays"))
      (pcase-let ((`(,text ,_ ,shown) (funcall italic plain)))
        (should (equal text (string-replace "`" "" plain)))
        (should-not (seq-some (lambda (i) (memq 'italic (ensure-list (get-text-property i 'face shown))))
                              (number-sequence 0 (1- (length shown)))))))
    (should (equal (car (funcall italic "**bold** and *it* and *more*")) "bold and it and more"))))

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
      ;; Marked as `lispm--live-insert' marks what streams in.
      (insert "* Assistant\n"
              (propertize "weighing options" 'face 'shadow 'lispm-live 'thinking) "\n"
              (propertize "Here is a\n* heading-like line\n" 'lispm-live 'text)
              (propertize "-> edit_file ...\n" 'face 'shadow 'lispm-live '(tool . "edit_file")))
      (setq lispm--live (cons start (point-marker))))
    (let ((items (lispm-chat-items (current-buffer))))
      (should (equal (mapcar (lambda (i) (plist-get i :kind)) items) '(user thought text tool)))
      (should (equal (plist-get (nth 2 items) :text) "Here is a\n* heading-like line"))
      (should (equal (plist-get (nth 3 items) :state) "WRITING"))
      (should (equal (plist-get (nth 3 items) :name) "edit_file")))))

(ert-deftest lispm-test-chat-tools ()
  "Each tool's calls are shown as the chat's table says: under their name,
an MCP server's as SERVER:TOOL, a tool the table does not know by its own
name; long results are cut to a few lines and say how many more there are."
  (should (equal (lispm-chat--tool-name "read_file") "Read"))
  (should (equal (lispm-chat--tool-name "mcp__fake__echo") "fake:echo"))
  (should (equal (lispm-chat--tool-name "made_up") "made_up"))
  (should (equal (lispm-chat--tool-text "edit_definition" :question '((name . "foo")) nil "foo")
                 "Do you want to make this edit to foo?"))
  (should (equal (lispm-chat--tool-text "write_skill" :title '((name . "web")) nil nil)
                 "Write the skill web"))
  (should-not (lispm-chat--tool-text "read_file" :title nil nil nil))
  (should (equal (lispm-chat--preview '("a" "b") 3 nil) '("a" "b")))
  (let ((cut (lispm-chat--preview '("a" "b" "c" "d") 2 nil)))
    (should (equal (seq-take cut 2) '("a" "b")))
    (should (string-match-p "+2 lines" (nth 2 cut))))
  (should (equal (lispm-chat--preview '("a" "b" "c" "d") 2 t) '("a" "b" "c" "d"))))

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
      ;; Only the items drawn now are remembered, not each step of the response.
      (with-current-buffer chat
        (should-not (seq-find (lambda (item) (equal (plist-get item :text) "Partial"))
                              (hash-table-keys lispm-chat--rendered))))
      (with-current-buffer session
        (goto-char (point-min))
        (search-forward "hello")
        (replace-match "howdy"))
      (should (string-match-p "^λ >> howdy$" (lispm-test--chat-text chat)))
      (lispm--live-delete agent)
      (should-not (string-match-p "Partial" (lispm-test--chat-text chat))))))

(ert-deftest lispm-test-live-segments ()
  "What streams into a response is read back as it came, each piece of its
kind; a call being written also says so as the agent's activity."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester "Test agent." :root project))
           (_chat (lispm-chat agent))
           (session (lispm-agent-buffer agent)))
      (lispm--live-begin agent)
      (lispm--live-insert agent 'thinking "Let me see")
      (lispm--live-insert agent 'text "Here it is.")
      (lispm--live-insert agent 'tool "edit_file")
      (should (equal (lispm-live-segments session)
                     '((:kind thinking :text "Let me see")
                       (:kind text :text "Here it is.")
                       (:kind tool :name "edit_file"))))
      (should (equal (lispm-agent-activity agent) '(writing . "edit_file")))
      (should (equal (lispm-agent-status agent) "writing a edit_file call"))
      (lispm--live-delete agent)
      (should-not (lispm-live-region session))
      (should-not (lispm-live-segments session)))))

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

;;;; Presentations and the inspector

(defun lispm-test--object (text part)
  "The object presented where PART first appears in TEXT."
  (get-text-property (string-search part text) 'lispm-object text))

(ert-deftest lispm-test-presentations ()
  "Text names objects one acts on: files at a line, buffers, agents, symbols.
Tools take the places and buffers agents name as org links."
  (lispm-test-env
    (lispm-test--write (expand-file-name "lisp/a.el" project) "(defun a ())\n(defun b ())\n")
    (let* ((default-directory project)
           (agent (lispm-start))
           (a (expand-file-name "lisp/a.el" project))
           (text (lispm-present-text
                  "See lisp/a.el:2, [[file:lisp/a.el::1][a]], [[buffer:*scratch*]] and assistant@project; not.this, nofile.el:3."
                  project)))
      (should (equal (lispm-test--object text "lisp/a.el:2") (list 'file a 2)))
      (should (= (length (substring-no-properties text (string-search "lisp/a.el:2" text)
                                                  (next-single-property-change
                                                   (string-search "lisp/a.el:2" text) 'lispm-object text)))
                 (length "lisp/a.el:2")))
      (should (equal (lispm-test--object text "[[file:") (list 'file a 1)))
      (should (equal (lispm-test--object text "[[buffer:") '(buffer "*scratch*")))
      (should (equal (lispm-test--object text "assistant@project") '(agent "assistant@project")))
      (should-not (lispm-test--object text "not.this"))
      (should-not (lispm-test--object text "nofile.el"))
      (should (equal (get-text-property 0 'lispm-object (lispm-present-code "car")) '(symbol car)))
      (should (equal (get-text-property 0 'lispm-object (lispm-present-code "lisp/a.el:2" project))
                     (list 'file a 2)))
      ;; Acting on a place visits the file there.
      (save-window-excursion
        (lispm-act (list 'file a 2))
        (should (equal (buffer-file-name) a))
        (should (= (line-number-at-pos) 2)))
      ;; Tools resolve the references agents pass each other.
      (should (equal (lispm-tools-reference "[[file:lisp/a.el::2][a]]") '("lisp/a.el" . 2)))
      (should (equal (lispm-tools-reference "lisp/a.el") '("lisp/a.el")))
      (should (string-prefix-p "     2\t(defun b ())"
                               (car (lispm-test--call agent "read_file" '((path . "[[file:lisp/a.el::2]]"))))))
      (should (string-match-p "lisp-interaction-mode\\|fundamental-mode\\|point at"
                              (car (lispm-test--call agent "read_buffer" '((buffer . "[[buffer:*scratch*]]"))))))
      ;; A write outside the root asks, a link naming it or not.
      (setf (lispm-agent-policy agent) '((write . auto)))
      (should (eq (lispm-tool-action agent (lispm-get-tool "write_file")
                                     '((path . "[[file:/tmp/lispm-elsewhere.txt]]") (content . "x")))
                  'ask)))))

(ert-deftest lispm-test-chat-presentations ()
  "The chat shows objects: RET or a click on one acts on it."
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "one\ntwo\n"))
    (let* ((agent (lispm-start 'assistant project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((,(lispm-agent-name agent)
               ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "read_file" '((path . "a.txt"))))
               ,(lispm-test--msg "end_turn" (lispm-test--text "Look at a.txt:2, `car` and `a.txt`.")))))
      (lispm-test--chat-type chat "Read a.txt")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (let ((a (expand-file-name "a.txt" project)))
        (lispm-chat-redraw chat)
        (with-current-buffer chat
          (goto-char (point-min))
          (search-forward "Read(a.txt)")
          (should (equal (lispm-object-at (- (point) 3)) (list 'file a nil)))
          (should (equal (lispm-object-at (- (point) 9)) '(tool "read_file")))
          (beginning-of-line)
          (should (equal (lispm-object-at) (list 'call (symbol-name (lispm-agent-name agent)) "t1")))
          (search-forward "car")
          (should (equal (lispm-object-at (1- (point))) '(symbol car)))
          ;; Code naming a file is the file, shown whole.
          (should (string-search "Look at a.txt:2, car and a.txt." (buffer-string)))
          (search-forward "a.txt")
          (should (equal (lispm-object-at (1- (point))) (list 'file a nil)))
          (goto-char (point-min))
          (search-forward "a.txt:2")
          (goto-char (match-beginning 0))
          (should (equal (lispm-object-at) (list 'file a 2)))
          (save-window-excursion
            (lispm-chat-return)
            (should (equal (buffer-file-name) a))
            (should (= (line-number-at-pos) 2))))))))

(ert-deftest lispm-test-ground ()
  "Code in replies is checked against this Emacs, as PceEmacs checks a
Prolog program: symbols nothing defines, calls with the wrong number of
arguments and files that are not there are found; what is there, words
Emacs knows and names the reply defines pass."
  (lispm-test-env
    (lispm-test--write (expand-file-name "lisp/a.el" project) "(defun a ())\n")
    ;; Made up, in a namespace this Emacs has.
    (should (equal (lispm-ground-check "lispm-test-made-up-zz") '(undefined "lispm-test-made-up-zz")))
    (should (equal (lispm-ground-check "#'lispm-test-made-up-zz") '(undefined "lispm-test-made-up-zz")))
    (should (equal (lispm-ground-check "(lispm-test-made-up-zz 1)") '(undefined "lispm-test-made-up-zz")))
    ;; Checking reads no made-up name into this Emacs.
    (should-not (intern-soft "lispm-test-made-up-zz"))
    ;; The namespace of cl-lib is cl-.
    (should (equal (lispm-ground-check "cl-test-made-up-zz") '(undefined "cl-test-made-up-zz")))
    ;; Defined, a word Emacs knows, no library's namespace, or defined by the
    ;; reply, with the names made from it.
    (should-not (lispm-ground-check "lispm-ground-check"))
    (should-not (lispm-ground-check "help-echo"))
    (dolist (word '("zzq-branch-name" "text-align" "font-size" "justify-content" "make-test" "emacs-plus"))
      (should-not (lispm-ground-check word)))
    (let ((defined (lispm-ground-definitions
                    "```\n(define-minor-mode lispm-test-made-up-zz\n  \"Doc.\")\n```")))
      (should-not (lispm-ground-check "lispm-test-made-up-zz" nil defined))
      (should-not (lispm-ground-check "lispm-test-made-up-zz-hook" nil defined)))
    ;; Not taken for symbols whatever the case of the buffer.
    (let ((case-fold-search t))
      (should-not (lispm-ground-check "Lispm-Test-Made-Up-Zz")))
    ;; A library on the load path, once it is there.
    (lispm-test--write (expand-file-name "lib/lispm-test-lib-zz.el" project) ";; Nothing.\n")
    (should (lispm-ground-check "lispm-test-lib-zz"))
    (let ((load-path (cons (expand-file-name "lib" project) load-path)))
      (should-not (lispm-ground-check "lispm-test-lib-zz")))
    ;; Calls: of a name some code mentions but nothing defines.
    (intern "lispm-test-mentioned-zz")
    (should-not (lispm-ground-check "lispm-test-mentioned-zz"))
    (should (equal (lispm-ground-check "(lispm-test-mentioned-zz)") '(undefined "lispm-test-mentioned-zz")))
    (should (equal (lispm-ground-check "(car x y)") '(arity car 2 (1 . 1))))
    (should (equal (lispm-ground-check "(message)") '(arity message 0 (1 . many))))
    (should-not (lispm-ground-check "(car x)"))
    (should-not (lispm-ground-check "(when a b c)"))
    (should-not (lispm-ground-check "(car x"))
    ;; Files, relative to the root.
    (should-not (lispm-ground-check "lisp/a.el:1" project))
    (should (equal (lispm-ground-check "lisp/b.el" project)
                   (list 'missing (expand-file-name "lisp/b.el" project))))
    (should (equal (lispm-ground-check "./b.txt" project)
                   (list 'missing (expand-file-name "b.txt" project))))
    ;; Not paths: a branch, a URL's path, a domain, a URL, a wildcard.
    (dolist (code '("origin/main" "/api/v1/users" "lispm.ai/install.sh" "https://lispm.ai/x.sh" "lisp/*.el"))
      (should-not (lispm-ground-check code project)))
    ;; Said in words, and shown on the code.
    (should (equal (lispm-ground-describe '(arity car 2 (1 . 1))) "car takes 1 argument; 2 given here"))
    (should (equal (lispm-ground-describe '(arity format 0 (1 . many)))
                   "format takes at least 1 argument; 0 given here"))
    (should (equal (lispm-ground-describe '(arity substring 4 (1 . 3)))
                   "substring takes 1 to 3 arguments; 4 given here"))
    (let ((shown (lispm-present-code "(car x y)")))
      (should (equal (get-text-property 0 'lispm-object shown) '(symbol car)))
      (should (equal (get-text-property 0 'lispm-ground shown) '(arity car 2 (1 . 1))))
      (should (memq 'lispm-ungrounded (ensure-list (get-text-property 0 'face shown))))
      (should (string-search "2 given here" (get-text-property 0 'help-echo shown))))
    (should (equal (get-text-property 0 'lispm-object (lispm-present-code "'car")) '(symbol car)))
    (let ((lispm-ground-code nil))
      (should-not (get-text-property 0 'lispm-ground (lispm-present-code "lispm-test-made-up-zz"))))))

(ert-deftest lispm-test-chat-ground ()
  "The chat marks code in replies that names what this Emacs lacks, but
not what the reply defines itself."
  (lispm-test-env
    (let* ((agent (lispm-start 'assistant project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((,(lispm-agent-name agent)
               ,(lispm-test--msg "end_turn"
                                 (lispm-test--text
                                  (concat "Call `lispm-test-made-up-zz`, `(car 1 2)` and `lispm-test-defined-zz`:\n"
                                          "```\n(defun lispm-test-defined-zz () t)\n```"))))))
      (lispm-test--chat-type chat "Which?")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-chat-redraw chat)
      (with-current-buffer chat
        ;; The code is shown whole, without its backquotes.
        (should (string-search "Call lispm-test-made-up-zz, (car 1 2) and lispm-test-defined-zz:"
                               (buffer-string)))
        (goto-char (point-min))
        (search-forward "lispm-test-made-up-zz")
        (should (equal (get-text-property (1- (point)) 'lispm-ground) '(undefined "lispm-test-made-up-zz")))
        (search-forward "(car 1 2)")
        (should (equal (get-text-property (1- (point)) 'lispm-ground) '(arity car 2 (1 . 1))))
        (search-forward "lispm-test-defined-zz")
        (should-not (get-text-property (1- (point)) 'lispm-ground)))
      ;; Turned off, the chat drawn shows none.
      (unwind-protect
          (progn (customize-set-variable 'lispm-ground-code nil)
                 (with-current-buffer chat
                   (should-not (text-property-not-all (point-min) (point-max) 'lispm-ground nil))))
        (customize-set-variable 'lispm-ground-code t)))))

(ert-deftest lispm-test-inspector ()
  "The inspector shows an agent whole, for the user and for agents."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes.")
    (let* ((default-directory project)
           (boss (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "tool_use"
                                                   (lispm-test--tool "t1" "ask_agent"
                                                                     '((agent . "reviewer")
                                                                       (request . "Looks good?")))))
              (reviewer@project hang)))
      (lispm-tell boss "get a review")
      (should (lispm-test--wait (lambda () (lispm-get-agent "reviewer@project" t))))
      (let ((reviewer (lispm-get-agent "reviewer@project")))
        (should (lispm-test--wait (lambda () (eq (lispm-agent-state reviewer) 'requesting))))
        (let ((text (lispm-inspect-text boss)))
          (should (string-match-p "^# Pending calls\nask_agent reviewer  run$" text))
          (should (string-match-p "^waits on +reviewer@project$" text))
          (should (string-match-p "^policy +read auto, " text)))
        ;; Agents inspect each other with a tool.
        (let ((result (lispm-test--call boss "inspect_agent" '((agent . "reviewer@project")))))
          (should-not (cdr result))
          (should (string-match-p "^waited on by +assistant@project$" (car result)))
          (should (string-match-p "^ceiling +read auto" (car result)))
          (should (string-match-p "^handling +\\[from assistant@project\\] Looks good\\?$" (car result))))
        ;; In the inspector, the agents named are objects too.
        (with-current-buffer (lispm-inspect boss)
          (goto-char (point-min))
          (search-forward "waits on")
          (search-forward "reviewer@project")
          (should (equal (lispm-object-at (1- (point))) '(agent "reviewer@project")))
          (goto-char (1- (point)))
          (lispm-act-at-point)
          (should (equal (buffer-name) "*lispm inspect reviewer@project*")))
        (lispm-abort boss)))))

(ert-deftest lispm-test-return-value ()
  "A helper returns a value with its reply; ask_agent gives it as JSON."
  (lispm-test-env
    (lispm-defrole reviewer "Reviews changes.")
    (let* ((default-directory project)
           (boss (lispm-start)))
      (setq lispm-test--scripts
            `((assistant@project ,(lispm-test--msg "tool_use"
                                                   (lispm-test--tool "t1" "ask_agent"
                                                                     '((agent . "reviewer")
                                                                       (request . "Where is b?"))))
                                 ,(lispm-test--msg "end_turn" (lispm-test--text "Thanks.")))
              (reviewer@project ,(lispm-test--msg "tool_use"
                                                  (lispm-test--tool "r1" "return_value"
                                                                    '((value . ["[[file:a.el::3]]"]))))
                                ,(lispm-test--msg "end_turn" (lispm-test--text "Found one.")))))
      (lispm-tell boss "find b")
      (should (lispm-test--wait (lambda () (and (funcall (lispm-test--idle-p boss))
                                                (= (length lispm-test--requests) 4)))))
      (should (string-match-p (regexp-quote "reviewer@project replied:\nFound one.\n\nValue:\n[\"[[file:a.el::3]]\"]")
                              (format "%s" (alist-get 'content (aref (alist-get 'content (car (last (append (alist-get 'messages (car lispm-test--requests)) nil)))) 0)))))
      ;; Nobody waits for the reply of a turn the user started.
      (should (string-match-p "Nobody waits"
                              (car (lispm-test--call boss "return_value" '((value . 1)))))))))

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
                                                (equal (car (lispm-waiting-calls agent)) "t2")))))
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
      (should (equal (plist-get (lispm-asked (gethash "t1" (lispm-agent-pending agent))) :input)
                     '((path . "a.txt") (old_text . "world") (new_text . "lisp"))))
      (should (string-match-p "^\\*\\* WAITING run_lisp .*\n\\(?:.*\n\\)*\\*\\*\\* Asks: edit_file  a.txt\n\\(?:.*\n\\)*\\*\\*\\* Preview\n\\(?:.*\n\\)*\\+hello lisp"
                              (lispm-test--transcript agent)))
      (lispm-resolve agent "t1" 'approve)
      (should (lispm-test--wait (lambda () (equal (plist-get (lispm-asked (gethash "t1" (lispm-agent-pending agent)))
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

;;;; Checks of tool calls

(defun lispm-test--watch-paths (_agent _tool input)
  "A check of tool calls: some files are watched, some refused."
  (pcase (and (consp input) (alist-get 'path input))
    ("asked.txt" '(ask . "That file is watched."))
    ("denied.txt" '(deny . "Not that file."))))

(defun lispm-test--refuse (_agent _tool _input)
  "A check of tool calls that refuses every call, saying nothing."
  'deny)

(ert-deftest lispm-test-tool-action-functions ()
  "Functions may have a call ask or refuse it, never let one through.
The strictest answer wins, with the reasons of those giving it; an
error, or an answer that is not one, asks."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project :policy '((write . auto) (shell . deny))))
           (write (lispm-get-tool "write_file"))
           (asked '((path . "asked.txt")))
           (consulted 0)
           (count (lambda (&rest _) (cl-incf consulted) nil))
           (lispm-tool-action-functions (list count #'lispm-test--watch-paths)))
      ;; Nil leaves the call as LispM's rules decide it.
      (should (equal (lispm-tool-decision agent write '((path . "b.txt"))) '(auto)))
      (should (equal (lispm-tool-decision agent write asked) '(ask "That file is watched.")))
      (should (eq (lispm-tool-action agent write asked) 'ask))
      (should (equal (lispm-tool-decision agent write '((path . "denied.txt"))) '(deny "Not that file.")))
      (should (equal (lispm-tools-denial agent write '("Not that file.")) "Denied: Not that file."))
      ;; What LispM's rules deny is denied without asking anyone.
      (setq consulted 0)
      (should (equal (lispm-tool-decision agent (lispm-get-tool "run_shell") '((command . "ls")))
                     '(deny)))
      (should (= consulted 0))
      (should (equal (lispm-tools-denial agent (lispm-get-tool "run_shell") nil)
                     "Denied by policy: tester may not use shell tools."))
      ;; The strictest answer wins; a bare one is given a reason.
      (let ((lispm-tool-action-functions (list #'lispm-test--watch-paths #'lispm-test--refuse)))
        (should (equal (lispm-tool-decision agent write asked)
                       '(deny "`lispm-test--refuse' refused it."))))
      ;; An error asks, and so does anything that is not an answer: `auto'
      ;; would relax what LispM decided.
      (let ((lispm-tool-action-functions (list (lambda (&rest _) (error "Broken")))))
        (should (equal (lispm-tool-decision agent write '((path . "b.txt")))
                       '(ask "A function of `lispm-tool-action-functions' failed: Broken."))))
      (let ((lispm-tool-action-functions (list (lambda (&rest _) 'auto))))
        (should (eq (lispm-tool-action agent write '((path . "b.txt"))) 'ask)))
      ;; A call the user allowed for good is asked about again.
      (push "write_file" (lispm-agent-allowed agent))
      (setf (lispm-agent-policy agent) '((write . ask)))
      (should (eq (lispm-tool-action agent write '((path . "b.txt"))) 'auto))
      (should (eq (lispm-tool-action agent write asked) 'ask)))))

(ert-deftest lispm-test-tool-action-reasons ()
  "A call a check refuses tells the model why; one it asks about shows why.
The reason is in the call's preview, in the chat's approval and in a
session reopened; approving a call for good approves no other call a
check still asks about."
  (lispm-test-env
    (let* ((lispm-tool-action-functions (list #'lispm-test--watch-paths))
           (default-directory project)
           (agent (lispm-start))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((assistant@project
               ,(lispm-test--msg "tool_use"
                                 (lispm-test--tool "t1" "write_file"
                                                   '((why . "w") (path . "denied.txt") (content . "x\n")))
                                 (lispm-test--tool "t2" "write_file"
                                                   '((why . "w") (path . "b.txt") (content . "b\n")))
                                 (lispm-test--tool "t3" "write_file"
                                                   '((why . "w") (path . "asked.txt") (content . "a\n")))))))
      (lispm-test--chat-type chat "go")
      (should (lispm-test--wait (lambda () (and (eq (lispm-agent-state agent) 'waiting)
                                                (= (length (lispm-waiting-calls agent)) 2)))))
      (let ((transcript (lispm-test--transcript agent)))
        (should (string-match-p "^\\*\\* DENIED write_file  denied\\.txt\n\\(?:.*\n\\)*?Denied: Not that file\\.$"
                                transcript))
        (should (string-match-p "^\\*\\*\\* Preview\n- That file is watched\\.\n#\\+begin_src diff\n" transcript)))
      (should (equal (plist-get (lispm-asked (gethash "t3" (lispm-agent-pending agent))) :reasons)
                     '("That file is watched.")))
      ;; Allowing writes for good runs b.txt's, not asked.txt's.
      (lispm-allow agent "t2")
      (should (lispm-test--wait (lambda () (equal (lispm-waiting-calls agent) '("t3")))))
      (should (string-match-p "│   That file is watched\\. +│" (lispm-test--chat-text chat)))
      ;; Reopened, the session still says why.
      (let* ((file (lispm-test--crash agent))
             (revived (lispm-resume-file file)))
        (should (equal (lispm-waiting-calls revived) '("t3")))
        (should (equal (plist-get (lispm-asked (gethash "t3" (lispm-agent-pending revived))) :reasons)
                       '("That file is watched.")))
        (push `(assistant@project ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))
              lispm-test--scripts)
        (lispm-resolve revived "t3" 'approve)
        (should (lispm-test--wait (lispm-test--idle-p revived)))
        (should (equal (lispm-test--file project "asked.txt") "a\n"))
        (should-not (file-exists-p (expand-file-name "denied.txt" project)))))))

(ert-deftest lispm-test-tool-action-in-programs ()
  "A program's calls are checked as the model's: refused, or asked about with why."
  (lispm-test-env
    (let ((lispm-tool-action-functions (list #'lispm-test--watch-paths))
          (agent (lispm-defagent tester :root project :policy '((write . auto)))))
      (should (equal (lispm-test--lisp agent "(condition-case e
                                                 (write_file :path \"denied.txt\" :content \"x\")
                                               (lispm-call-denied (cadr e)))")
                     '("Tool calls:\n- write_file denied.txt (denied)\n\n=> write_file: Denied: Not that file.")))
      (setq lispm-test--scripts
            `((tester ,(lispm-test--msg "tool_use"
                                        (lispm-test--tool "t1" "run_lisp"
                                                          '((code . "(write_file :path \"asked.txt\" :content \"a\")"))))
                      ,(lispm-test--msg "end_turn" (lispm-test--text "Done.")))))
      (lispm-tell agent "write")
      (should (lispm-test--wait (lambda () (eq (lispm-agent-state agent) 'waiting))))
      (should (equal (plist-get (lispm-asked (gethash "t1" (lispm-agent-pending agent))) :reasons)
                     '("That file is watched.")))
      (should (string-match-p "^\\*\\*\\* Asks: write_file  asked\\.txt\n\\(?:.*\n\\)*\\*\\*\\* Preview\n- That file is watched\\.$"
                              (lispm-test--transcript agent)))
      (lispm-resolve agent "t1" 'approve)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal (lispm-test--file project "asked.txt") "a\n")))))

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

;;;; The language LispM speaks

(defun lispm-test--source-files ()
  "Return LispM Agent's Lisp files."
  (directory-files (expand-file-name "../lisp" lispm-test--dir) t "\\.el\\'"))

(defun lispm-test--messages ()
  "Return the messages the code gives to translate: a hash table from a
catalog key (see `lispm-i18n--key') to a plist: :message, :context,
:plural, the plural's English, and :places, where the message is
written, as FILE:LINE; and :time for a time format, as
`lispm-format-time' takes."
  (let ((messages (make-hash-table :test #'equal)))
    (dolist (file (lispm-test--source-files))
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (let ((name (file-name-nondirectory file)))
          (goto-char (point-min))
          (while (re-search-forward
                  "(\\(lispm-\\(?:tr\\(?:-n\\|-in\\|-noop\\)?\\|format-time\\)\\)[ \t\n]+\"" nil t)
            (let ((function (intern (match-string 1)))
                  (start (match-beginning 0)))
              (unless (nth 8 (save-excursion (syntax-ppss start)))
                (let* ((line (line-number-at-pos start))
                       (form (save-excursion (goto-char start) (read (current-buffer))))
                       (args (cdr form))
                       (context (cond ((eq function 'lispm-tr-in) (pop args))
                                      ((eq function 'lispm-tr-noop) (nth 1 args))))
                       (message (car args))
                       (key (lispm-i18n--key message context))
                       (entry (gethash key messages)))
                  (puthash key
                           (list :message message :context context
                                 :plural (or (plist-get entry :plural)
                                             (and (eq function 'lispm-tr-n) (nth 1 args)))
                                 :time (or (plist-get entry :time) (eq function 'lispm-format-time))
                                 :places (append (plist-get entry :places) (list (format "%s:%d" name line))))
                           messages))))))))
    messages))

(defun lispm-test--format-arguments (text)
  "Return the arguments format string TEXT takes, as (INDEX . CONVERSION),
sorted: `%1$s' takes argument 1, and a plain `%s' the one after the
last taken."
  (let ((arguments nil) (next 1) (start 0))
    (while (string-match "%\\(?:\\([0-9]+\\)\\$\\)?[-+ #0]*[0-9]*\\(?:\\.[0-9]+\\)?\\([a-zA-Z%]\\)" text start)
      (setq start (match-end 0))
      (unless (equal (match-string 2 text) "%")
        (let ((index (if (match-string 1 text) (string-to-number (match-string 1 text)) next)))
          (push (cons index (match-string 2 text)) arguments)
          (setq next (1+ index)))))
    (sort arguments (lambda (a b) (< (car a) (car b))))))

(defconst lispm-test--translated-languages '(de fr ja zh-Hans zh-Hant)
  "The languages whose catalogs translate every message.")

(defconst lispm-test--spellings
  '(("colour" . "color") ("centre" . "center") ("grey" . "gray") ("favourite" . "favorite")
    ("behaviour" . "behavior") ("cancelled" . "canceled") ("labelled" . "labeled"))
  "Words Britain and the United States spell apart: (BRITISH . AMERICAN),
the start of a word.  Customize stays, Emacs's name for its settings.")

(defun lispm-test--spelled-p (text spellings)
  "Return non-nil when TEXT has a word starting as one of SPELLINGS."
  (let ((case-fold-search t))
    (seq-some (lambda (spelling) (string-match-p (concat "\\b" spelling) text)) spellings)))

(ert-deftest lispm-test-i18n-translates-every-message ()
  "Every language but English translates every message the code gives,
a plural in as many forms as the language has; no catalog holds a
message the code no longer gives."
  (let ((messages (lispm-test--messages)))
    (should (> (hash-table-count messages) 300))
    (dolist (language (mapcar #'car lispm-languages))
      (let ((catalog (plist-get (lispm-i18n-read-catalog language) :messages))
            (forms (if (alist-get language lispm-i18n-plural-forms) 2 1)))
        (should (plist-get (lispm-i18n-read-catalog language) :name))
        (maphash (lambda (key _)
                   (should-not (list language 'stale key)))
                 (let ((stale (copy-hash-table catalog)))
                   (maphash (lambda (key _) (remhash key stale)) messages)
                   stale))
        (when (memq language lispm-test--translated-languages)
          (maphash (lambda (key entry)
                     (let ((translation (gethash key catalog)))
                       (unless (and translation
                                    (if (plist-get entry :plural)
                                        (or (and (stringp translation) (= forms 1))
                                            (and (consp translation) (= (length translation) forms)))
                                      (stringp translation)))
                         (should-not (list language 'missing key)))))
                   messages))))))

(ert-deftest lispm-test-i18n-keeps-arguments ()
  "A translation takes the arguments its message takes, in any order; a
time format, its own."
  (let ((messages (lispm-test--messages)))
    (dolist (language (mapcar #'car lispm-languages))
      (maphash (lambda (key translation)
                 (when-let* ((entry (gethash key messages))
                             ((not (plist-get entry :time))))
                   (let ((expected (lispm-test--format-arguments (plist-get entry :message))))
                     (dolist (text (ensure-list translation))
                       (let ((arguments (lispm-test--format-arguments text)))
                         ;; A plural's form may leave the count unsaid, as
                         ;; "one item" does.
                         (unless (or (equal arguments expected)
                                     (and (plist-get entry :plural)
                                          (equal arguments (cl-remove-if (lambda (a) (equal (cdr a) "d"))
                                                                         expected))))
                           (should-not (list language key text))))))))
               (plist-get (lispm-i18n-read-catalog language) :messages)))))

(ert-deftest lispm-test-i18n-english-spelling ()
  "The code's English spells as Britain does: American English changes
every message spelled so, and British English every one spelled the
American way; neither keeps the other's spelling."
  (let ((messages (lispm-test--messages))
        (british (mapcar #'car lispm-test--spellings))
        (american (mapcar #'cdr lispm-test--spellings)))
    (pcase-dolist (`(,language ,from) `((en-US ,british) (en-GB ,american)))
      (let ((catalog (plist-get (lispm-i18n-read-catalog language) :messages)))
        (maphash (lambda (key entry)
                   (dolist (text (cons (plist-get entry :message) (ensure-list (plist-get entry :plural))))
                     (when (and (lispm-test--spelled-p text from) (not (gethash key catalog)))
                       (should-not (list language 'spelled-as-written key))))
                   (dolist (text (ensure-list (gethash key catalog)))
                     (should-not (and (lispm-test--spelled-p text from) (list language key text)))))
                 messages)))))

(defmacro lispm-test--in-french (&rest body)
  "Run BODY speaking French, from a catalog of the test's own."
  (declare (indent 0))
  `(let ((lispm-i18n--catalogs nil)
         (lispm-language 'fr))
     (cl-letf (((symbol-function 'lispm-i18n-read-catalog)
                (lambda (_language)
                  (let ((table (make-hash-table :test #'equal)))
                    (puthash "Step %d of %d" "Étape %1$d sur %2$d" table)
                    (puthash "%d item" '("%d élément" "%d éléments") table)
                    (puthash (lispm-i18n--key "Open" "state") "Ouvert" table)
                    (puthash "Open" "Ouvrir" table)
                    (puthash (lispm-i18n--key "Low" "sandbox level") "Faible" table)
                    (puthash "no sandbox" "aucun bac à sable" table)
                    (puthash "No agent named %s" "Aucun agent nommé %s" table)
                    (puthash "Welcome to %s!" "Bienvenue dans %s !" table)
                    (puthash "Agent" "Agente" table)
                    (list :name "Français" :messages table)))))
       ,@body)))

(ert-deftest lispm-test-i18n-translates ()
  "A message shows in the language spoken, formatted, as written where
the catalog lacks it; a plural takes the language's form for the count;
a message said in a context is looked up in it; in English, as written."
  (lispm-test--in-french
    (should (equal (lispm-tr "Step %d of %d" 2 3) "Étape 2 sur 3"))
    (should (equal (lispm-tr "Not in the catalog") "Not in the catalog"))
    (should (equal (lispm-tr "%d%% done" 5) "5% done"))
    (should (equal (lispm-tr "Open") "Ouvrir"))
    (should (equal (lispm-tr-in "state" "Open") "Ouvert"))
    (should (equal (lispm-tr (lispm-tr-noop "Open" "state")) "Ouvert"))
    (should (equal (mapcar (lambda (n) (lispm-tr-n "%d item" "%d items" n)) '(0 1 2))
                   '("0 élément" "1 élément" "2 éléments")))
    (should (equal (lispm-tr-n "%d page" "%d pages" 0) "0 pages"))
    (should (equal (lispm-language-name 'fr) "Français"))
    (should (equal (lispm-in-english (lispm-tr "Open")) "Open"))
    (should (equal (string-match "b" "abc") 1))
    (lispm-tr "Open")
    (should (equal (match-beginning 0) 1))))

(ert-deftest lispm-test-i18n-language ()
  "The language spoken is `lispm-language', else that of the program LispM
runs in, else the computer's: the first of its preferred languages LispM
speaks, Chinese by script, else by region, English American in the
United States, else British, American English when none is spoken."
  (should (equal (mapcar #'lispm-i18n-language-of
                         '("zh-Hant-HK" "zh-Hans-HK" "zh-TW" "zh_HK.UTF-8" "zh-CN" "zh" "en" "en-US"
                           "en_US.UTF-8" "en-HK" "en-GB" "fr-CA" "de_CH" "ja-JP" "ko-KR" "C"))
                 '(zh-Hant zh-Hans zh-Hant zh-Hant zh-Hans zh-Hans en-US en-US
                   en-US en-GB en-GB fr de ja nil nil)))
  (let ((lispm-i18n--system-languages '("yue-Hant-HK" "ja-HK" "en-US")))
    (should (eq (lispm-system-language) 'ja)))
  (let ((lispm-i18n--system-languages '("ko-KR")))
    (should (eq (lispm-system-language) 'en-US)))
  (let ((lispm-i18n--system-languages '("zh-Hant-TW"))
        (lispm-language nil))
    (should (eq (lispm-current-language) 'zh-Hant))
    (let ((lispm-language-functions (list #'ignore (lambda () 'de))))
      (should (eq (lispm-current-language) 'de))
      (let ((lispm-language 'fr))
        (should (eq (lispm-current-language) 'fr))))
    ;; A program's language LispM Agent does not speak is not followed.
    (let ((lispm-language-functions (list (lambda () 'en))))
      (should (eq (lispm-current-language) 'zh-Hant)))))

(ert-deftest lispm-test-i18n-language-changed ()
  "Setting `lispm-language' runs `lispm-language-changed-hook' once the
command ends, when the language spoken changes; so does a program saying
its language changed, unless `lispm-language' chooses one."
  (let ((lispm-language 'en)
        (lispm-language-changed-hook nil)
        (lispm-i18n--system-languages '("ja-JP"))
        (drawn 0))
    (add-hook 'lispm-language-changed-hook (lambda () (setq drawn (1+ drawn))))
    (unwind-protect
        (progn
          (setq lispm-language 'de)
          (should (lispm-test--wait (lambda () (= drawn 1)) 1))
          (setq lispm-language 'de)
          (accept-process-output nil 0.05)
          (should (= drawn 1))
          (lispm-language-changed)
          (should (= drawn 1))
          (setq lispm-language nil)
          (should (lispm-test--wait (lambda () (= drawn 2)) 1))
          (lispm-language-changed)
          (should (= drawn 3)))
      (setq lispm-language 'en))))

(ert-deftest lispm-test-knows-no-desktop ()
  "LispM Agent stands alone: no file of lisp/ names LispM Desktop's code,
which hooks into the agents' instead."
  (dolist (file (directory-files (expand-file-name "../lisp" lispm-test--dir) t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (and (re-search-forward "lispm-desktop\\|platinum" nil t)
                       (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)))))))

(ert-deftest lispm-test-i18n-sandbox-levels ()
  "The sandbox levels are named and described in the language spoken, and
read back by either name."
  (lispm-test--in-french
    (should (equal (lispm-sandbox-level-name 'low) "Faible"))
    (should (equal (lispm-sandbox-level-name 'high) "High"))
    (should (equal (lispm-sandbox-level-description 'none) "aucun bac à sable"))
    (should (eq (lispm-sandbox-level-named "faible") 'low))
    (should (eq (lispm-sandbox-level-named "low") 'low))
    (should (eq (lispm-sandbox-level-named "High") 'high))
    (should-not (lispm-sandbox-level-named "Strict"))))

(ert-deftest lispm-test-i18n-models-read-english ()
  "What a model reads stays English in another language: a tool's result,
the error of a function it shares with the user's commands, the
inspector's text; the user is told in the language spoken."
  (lispm-test-env
    (lispm-test--in-french
      (let ((agent (lispm-defagent tester :root project)))
        (should (equal (cdr (should-error (lispm-get-agent "nobody") :type 'user-error))
                       '("Aucun agent nommé nobody")))
        (should (equal (lispm-test--call agent "inspect_agent" '((agent . "nobody")))
                       '("No agent named nobody; list_agents shows them" . t)))
        (lispm-register-tool
         (lispm-tool--create :name "lispm_test_ask" :description "Ask for an agent." :params nil
                             :function (lambda () (lispm-get-agent "nobody"))))
        (unwind-protect
            (should (equal (lispm-test--call agent "lispm_test_ask" nil)
                           '("No agent named nobody" . t)))
          (remhash "lispm_test_ask" lispm-tools--registry))
        (should (string-prefix-p "# Agent\n" (lispm-inspect-text agent)))))))

(ert-deftest lispm-test-i18n-chat ()
  "The chat speaks the language spoken, and again once it changes."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project))
           (chat (lispm-chat agent)))
      (should (string-match-p "Welcome to LispM!" (lispm-test--chat-text chat)))
      (lispm-test--in-french
        (lispm-chat--draw-all-again)
        (should (string-match-p "Bienvenue dans LispM !" (lispm-test--chat-text chat))))
      (lispm-chat--draw-all-again)
      (should (string-match-p "Welcome to LispM!" (lispm-test--chat-text chat))))))

(defun lispm-test-report-catalogs ()
  "Print, for each language, the messages its catalog lacks and those it
holds no longer.  Run with `make i18n'."
  (let ((messages (lispm-test--messages)))
    (dolist (language (mapcar #'car lispm-languages))
      (let ((catalog (plist-get (lispm-i18n-read-catalog language) :messages))
            (missing 0) (stale 0))
        (when (memq language lispm-test--translated-languages)
          (maphash (lambda (key entry)
                     (unless (gethash key catalog)
                       (setq missing (1+ missing))
                       (princ (format "%s: missing %S (%s)\n" language key
                                      (string-join (plist-get entry :places) " ")))))
                   messages))
        (maphash (lambda (key _)
                   (unless (gethash key messages)
                     (setq stale (1+ stale))
                     (princ (format "%s: stale %S\n" language key))))
                 catalog)
        (princ (format "%s: %d missing, %d stale\n" language missing stale))))))

;;;; The editor's agents

(defvar lispm-test--agent-list-map (make-sparse-keymap)
  "A keymap of a list of the agents, where their keys are bound too.")

(defmacro lispm-test--with-agents (agents &rest body)
  "Run BODY with AGENTS, (AGENT . PROPERTIES), the only agents defined,
their keys bound in keymaps of the test's own: `lispm-agents-map''s
and that of a list of the agents.  The state of sessions is kept in a
scratch directory, never the user's."
  (declare (indent 1))
  `(let* ((lispm-agents-defined nil)
          (lispm-agents--keys nil)
          (lispm-agents--root-cache (make-hash-table :test #'equal))
          (lispm-agents-map (make-sparse-keymap))
          (lispm-test--agent-list-map (make-sparse-keymap))
          (lispm-agents-key-maps '(lispm-agents-map lispm-test--agent-list-map))
          (lispm-test--agents-state (make-temp-file "lispm-test-agents-" t))
          (lispm-agents-state-file (expand-file-name "agent-sessions.eld" lispm-test--agents-state))
          (lispm-agents--archived 'unloaded))
     (unwind-protect
         (progn
           (pcase-dolist (`(,agent . ,props) ,agents)
             (apply #'lispm-agents-define agent props))
           ,@body)
       (delete-directory lispm-test--agents-state t))))

(ert-deftest lispm-test-agent-available-p ()
  "An agent run as a CLI can run exactly when its program is on PATH; one
running inside Emacs always can; one never defined cannot."
  (lispm-test--with-agents
      '((claude :name "Claude Code" :program "lispm-test-no-such-program")
        (codex :name "Codex" :program "emacs")
        (lispm :name "LispM" :new ignore))
    (should-not (lispm-agents-available-p 'claude))
    (should (lispm-agents-available-p 'codex))
    (should (lispm-agents-available-p 'lispm))
    (should-not (lispm-agents-available-p 'hermes))
    (should (equal (mapcar #'car (lispm-agents-available)) '(codex lispm)))))

(ert-deftest lispm-test-agents-built-in-first ()
  "Agents are offered those built in first, then in the order defined."
  (lispm-test--with-agents
      '((hermes :name "Hermes" :new ignore)
        (lispm :name "LispM" :new ignore :built-in t)
        (claude :name "Claude Code" :new ignore :built-in t))
    (should (equal (mapcar #'car (lispm-agents-available)) '(lispm claude hermes)))
    (lispm-agents-define 'hermes :name "Hermes, again" :new #'ignore)
    (should (equal (mapcar #'car lispm-agents-defined) '(hermes lispm claude)))
    (lispm-agents-undefine 'lispm)
    (should (equal (mapcar #'car (lispm-agents-available)) '(claude hermes)))))

(ert-deftest lispm-test-agent-keys-follow-the-agent ()
  "An agent's key starts it, in its keymaps and a list's of the agents, while
it can run; it does nothing once its program is gone or it is undefined."
  (let ((program "emacs"))
    (lispm-test--with-agents
        `((codex :name "Codex" :program "emacs" :key "x" :command lispm-agents-codex-new))
      (should (eq (keymap-lookup lispm-agents-map "x") 'lispm-agents-codex-new))
      (should (eq (keymap-lookup lispm-test--agent-list-map "x") 'lispm-agents-codex-new))
      (setf (plist-get (alist-get 'codex lispm-agents-defined) :program) "lispm-test-no-such-program")
      (should-not (keymap-lookup lispm-agents-map "x"))
      (setf (plist-get (alist-get 'codex lispm-agents-defined) :program) program)
      (lispm-agents-undefine 'codex)
      (should-not (keymap-lookup lispm-agents-map "x"))
      (should-not (keymap-lookup lispm-test--agent-list-map "x")))))

(ert-deftest lispm-test-agent-keys-leave-other-commands-be ()
  "An agent's key taken by another command stays that command's, and its
parent's bindings do not count; an agent taking another's key gives it
back when it goes."
  (lispm-test--with-agents nil
    (keymap-set lispm-agents-map "r" #'lispm-agents-send-region)
    (set-keymap-parent lispm-test--agent-list-map special-mode-map)
    (let ((inhibit-message t))
      (lispm-agents-define 'greedy :name "Greedy" :new #'ignore :key "r" :command #'ignore))
    (should (eq (keymap-lookup lispm-agents-map "r") 'lispm-agents-send-region))
    (lispm-agents-undefine 'greedy)
    (should (eq (keymap-lookup lispm-agents-map "r") 'lispm-agents-send-region))
    ;; h is the list's parent's, describe-mode: Hermes may have it.
    (lispm-agents-define 'hermes :name "Hermes" :new #'ignore :key "h" :command 'lispm-hermes-new)
    (should (eq (keymap-lookup lispm-test--agent-list-map "h") 'lispm-hermes-new))
    (lispm-agents-define 'other :name "Other" :new #'ignore :key "h" :command 'other-new)
    (should (eq (keymap-lookup lispm-agents-map "h") 'other-new))
    (lispm-agents-undefine 'other)
    (should (eq (keymap-lookup lispm-agents-map "h") 'lispm-hermes-new))
    (should (eq (keymap-lookup lispm-test--agent-list-map "h") 'lispm-hermes-new))
    (lispm-agents-undefine 'hermes)
    (should-not (keymap-lookup lispm-agents-map "h"))
    (should (eq (keymap-lookup lispm-test--agent-list-map "h") 'describe-mode))))

(ert-deftest lispm-test-launch-requires-installed-cli ()
  "Starting an agent whose CLI is missing, or no agent at all, is a user
error, before anything runs."
  (lispm-test--with-agents
      '((claude :name "Claude Code" :program "lispm-test-no-such-program"))
    (should-error (lispm-agents--launch 'claude temporary-file-directory "x" nil)
                  :type 'user-error)
    (should-error (lispm-agents-new 'hermes temporary-file-directory) :type 'user-error)))

(ert-deftest lispm-test-sessions-skip-missing-agents ()
  "No record of an agent whose CLI is missing is read or listed."
  (let ((read nil))
    (lispm-test--with-agents
        `((claude :name "Claude Code" :program "lispm-test-no-such-program"
                  :sessions ,(lambda () (setq read t) nil)))
      (should (null (lispm-agents-sessions-list)))
      (should-not read))))

(ert-deftest lispm-test-sessions-of-every-agent ()
  "Every agent's sessions are listed, newest first; an agent whose
sessions cannot be read lists none, and the others still list theirs."
  (lispm-test--with-agents
      `((lispm :name "LispM" :new ignore
               :sessions ,(lambda ()
                            (list (list :agent 'lispm :id "a" :time 1.0)
                                  (list :agent 'lispm :id "b" :time 3.0))))
        (codex :name "Codex" :program "emacs"
               :sessions ,(lambda () (list (list :agent 'codex :id "c" :time 2.0))))
        (broken :name "Broken" :new ignore :sessions ,(lambda () (error "No store"))))
    (let ((inhibit-message t))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) (lispm-agents-sessions-list))
                     '("b" "c" "a"))))))

(ert-deftest lispm-test-session-readers-feed-lists-only ()
  "A list's sessions come from an agent's session reader where it has
one, without waiting: an agent whose reader has nothing yet is pending,
one whose reader fails lists none, and agents without a reader are read
as they are.  The session list and the picker never ask a reader."
  (let ((lispm-agents--session-readers nil)
        (lispm-agents--candidates nil)
        (lispm-agents-sessions-arrived-hook nil)
        (arrived 0))
    (add-hook 'lispm-agents-sessions-arrived-hook (lambda () (setq arrived (1+ arrived))))
    (cl-flet ((session (agent id time title)
                (list :agent agent :id id :time time :title title :cwd "/tmp/project/")))
      (lispm-test--with-agents
          `((lispm :name "LispM" :new ignore
                   :sessions ,(lambda () (list (session 'lispm "l" 2.0 "L"))))
            (codex :name "Codex" :new ignore
                   :sessions ,(lambda () (list (session 'codex "c" 3.0 "C"))))
            (claude :name "Claude Code" :new ignore
                    :sessions ,(lambda () (list (session 'claude "a" 1.0 "A"))))
            (hermes :name "Hermes" :new ignore
                    :sessions ,(lambda () (list (session 'hermes "h" 5.0 "H")))))
        (should (eq (lispm-agents-add-session-reader 'codex (lambda () 'pending)) 'codex))
        (lispm-agents-add-session-reader
         'claude (lambda () (list (session 'claude "a" 4.0 "A, read") nil)))
        (lispm-agents-add-session-reader 'hermes (lambda () (error "No store")))
        ;; Each change of readers draws the lists again.
        (should (= arrived 3))
        (let ((inhibit-message t))
          (pcase-let ((`(,sessions . ,pending) (lispm-agents-sessions-snapshot)))
            (should (equal (mapcar (lambda (s) (plist-get s :title)) sessions) '("A, read" "L")))
            (should (equal pending '(codex)))))
        ;; One reader per agent: a new one replaces the old.
        (dolist (agent '(codex claude hermes))
          (lispm-agents-add-session-reader agent (lambda () (error "A reader was asked"))))
        (should (= (length lispm-agents--session-readers) 3))
        (should (equal (mapcar (lambda (s) (plist-get s :id)) (lispm-agents-sessions-list))
                       '("h" "c" "l" "a")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _) (car (all-completions "" table)))))
          (should (equal (plist-get (lispm-agents--read-session "Agent session: ") :id) "h")))
        (lispm-agents-remove-session-reader 'codex)
        (lispm-agents-remove-session-reader 'claude)
        (lispm-agents-remove-session-reader 'hermes)
        (lispm-agents-remove-session-reader 'hermes)
        (should-not lispm-agents--session-readers)
        (should (= arrived 9))
        (should (equal (lispm-agents-sessions-snapshot)
                       (cons (lispm-agents-sessions-list) nil)))))))

(ert-deftest lispm-test-agent-worktrees-join-their-projects ()
  "A Claude worktree joins its repository and a Codex worktree the known
project of its name; the Codex app's scratch tree holds chats."
  (lispm-test--with-agents nil
    (lispm-agents-setup-claude)
    (lispm-agents-setup-codex)
    (let* ((repo (make-temp-file "lispm-test-repo" t))
           (claude (expand-file-name ".claude/worktrees/feature" repo))
           (codex (expand-file-name (format "~/.codex/worktrees/1a2b/%s" (file-name-nondirectory repo))))
           (scratch (expand-file-name "2026-10-20/chat" lispm-agents-codex-chats-directory)))
      (unwind-protect
          (progn
            (make-directory claude t)
            (let ((roots (lispm-agents--resolve-roots (list repo claude codex scratch))))
              (should (equal (cdr (assoc claude roots)) (directory-file-name repo)))
              (should (equal (cdr (assoc codex roots)) (directory-file-name repo)))
              (should (eq (cdr (assoc scratch roots)) 'chats)))
            (should (equal (lispm-agents-location-label claude (directory-file-name repo))
                           "⎇ feature"))
            (should (equal (lispm-agents-location-label codex (directory-file-name repo))
                           "⎇ codex worktree")))
        (clrhash lispm-agents--root-cache)
        (delete-directory repo t)))))

(ert-deftest lispm-test-agent-archive-survives-an-unreadable-state ()
  "Archived sessions are kept across starts; a state file that does not
read is moved aside, not replaced by the next archiving."
  (let* ((directory (make-temp-file "lispm-test-state" t))
         (lispm-agents-state-file (expand-file-name "agent-sessions.eld" directory))
         (lispm-agents--archived 'unloaded)
         (cut-short "(:archived (\"claude:1\" \"cod")
         (one '(:agent claude :id "1"))
         (two '(:agent codex :id "2")))
    (unwind-protect
        (progn
          (lispm-agents--set-archived one t)
          (should-not (file-exists-p (concat lispm-agents-state-file ".part")))
          (setq lispm-agents--archived 'unloaded)
          (should (lispm-agents-archived-p one))
          (should-not (lispm-agents-archived-p two))
          ;; A write cut short leaves a file that does not read.
          (with-temp-file lispm-agents-state-file (insert cut-short))
          (setq lispm-agents--archived 'unloaded)
          (let ((inhibit-message t))
            (should-not (lispm-agents-archived-p one)))
          (lispm-agents--set-archived two t)
          (should (equal (with-temp-buffer
                           (insert-file-contents (concat lispm-agents-state-file ".unreadable"))
                           (buffer-string))
                         cut-short))
          (setq lispm-agents--archived 'unloaded)
          (should (equal (lispm-agents--archived-keys) '("codex:2"))))
      (delete-directory directory t))))

(ert-deftest lispm-test-codex-sessions-named-by-the-index ()
  "A Codex session is titled by its thread name in the index, else by its
first prompt; context Codex injects is no prompt."
  (let* ((home (make-temp-file "lispm-test-codex" t))
         (lispm-agents-codex-sessions-directory (expand-file-name "sessions/" home))
         (lispm-agents-codex-index-file (expand-file-name "session_index.jsonl" home))
         (file (expand-file-name "2026/10/20/rollout-1.jsonl" lispm-agents-codex-sessions-directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "{\"type\":\"session_meta\",\"payload\":{\"id\":\"0199\",\"cwd\":\"/tmp\",\"timestamp\":\"2026-10-20T08:00:00Z\"}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"# AGENTS.md instructions\"}]}}\n"
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"user_message\",\"message\":\"Fix the build\"}}\n"))
          (let ((session (car (lispm-agents-codex-sessions))))
            (should (equal (plist-get session :title) "Fix the build"))
            (should (equal (plist-get session :id) "0199"))
            (should (numberp (plist-get session :started))))
          (lispm-agents-codex--rename (list :id "0199") "Build fix")
          (should (equal (plist-get (car (lispm-agents-codex-sessions)) :title) "Build fix")))
      (remhash file lispm-agents--session-cache)
      (delete-directory home t))))

(ert-deftest lispm-test-claude-sessions-titled-by-their-name ()
  "A Claude session is titled by its first prompt until it is named.  The
name lives beside the transcript, which Claude rewrites, and a blank one
names nothing.  A transcript without a prompt is no session."
  (let* ((lispm-agents-claude-projects-directory (make-temp-file "lispm-test-claude" t))
         (file (expand-file-name "-tmp/0199.jsonl" lispm-agents-claude-projects-directory))
         (empty (expand-file-name "-tmp/0200.jsonl" lispm-agents-claude-projects-directory))
         (title (lambda () (plist-get (car (lispm-agents-claude-sessions)) :title))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "{\"type\":\"user\",\"cwd\":\"/tmp\",\"message\":{\"role\":\"user\",\"content\":\"Fix the build\"}}\n"))
          (with-temp-file empty
            (insert "{\"type\":\"system\",\"cwd\":\"/tmp\"}\n"))
          (should (= (length (lispm-agents-claude-sessions)) 1))
          (should (equal (funcall title) "Fix the build"))
          (lispm-agents-claude--rename (car (lispm-agents-claude-sessions)) "Build fix")
          (should (equal (funcall title) "Build fix"))
          (lispm-agents-claude--rename (car (lispm-agents-claude-sessions)) " ")
          (should (equal (funcall title) "Fix the build")))
      (remhash file lispm-agents--session-cache)
      (remhash empty lispm-agents--session-cache)
      (delete-directory lispm-agents-claude-projects-directory t))))

(ert-deftest lispm-test-age-short-date ()
  "Old sessions are dated, short; recent ones by their age."
  (let ((now (float-time (encode-time '(0 0 12 13 10 2026 nil -1 nil)))))
    (should (equal (lispm-agents-age (- now 120) now) "2m"))
    (should (equal (lispm-agents-age (float-time (encode-time '(0 0 12 13 5 2026 nil -1 nil))) now)
                   "5/13/26"))))

(ert-deftest lispm-test-local-http-content-type ()
  "A local server's answer is JSON unless its handler names its type."
  (let ((sent nil))
    (cl-letf (((symbol-function 'process-send-string) (lambda (_ string) (push string sent)))
              ((symbol-function 'process-send-eof) #'ignore))
      (lispm-local-http--reply 'connection '("200 OK" "{}"))
      (lispm-local-http--reply 'connection '("200 OK" "<p>" "text/html; charset=utf-8")))
    (should (string-search "Content-Type: application/json\r\n" (nth 1 sent)))
    (should (string-search "Content-Type: text/html; charset=utf-8\r\n" (nth 0 sent)))))

(ert-deftest lispm-test-agents-interface-is-emacs ()
  "Without a program drawing an interface around them, the agents run in
Eat's terminals, are shown with `pop-to-buffer', start in the selected
window's project and ask with `yes-or-no-p'; such a program says other
ways, which are followed."
  (should (eq lispm-agents-terminal-make-function #'lispm-agents-eat-make))
  (should (eq lispm-agents-show-function #'pop-to-buffer))
  (should (eq lispm-agents-main-window-function #'selected-window))
  (should (eq lispm-agents-confirm-function #'lispm-agents-yes-or-no-p))
  (let* ((made nil) (shown nil) (asked nil) (deleted nil) (answer nil)
         (lispm-agents-terminal-make-function
          (lambda (name program &rest args)
            (setq made (cons program args))
            (get-buffer-create (format "*%s*" name))))
         (lispm-agents-terminal-live-p-function #'buffer-live-p)
         (lispm-agents-terminal-modes '(fundamental-mode))
         (lispm-agents-show-function (lambda (buffer) (setq shown buffer)))
         (lispm-agents-confirm-function (lambda (question _detail action)
                                          (push (cons question action) asked)
                                          answer)))
    (lispm-test--with-agents
        `((tool :name "Tool" :program "emacs"
                :new-arguments ,(lambda () (cons "0199" '("--new")))
                :delete ,(lambda (session) (push (plist-get session :id) deleted))))
      (let ((buffer (lispm-agents-new 'tool temporary-file-directory)))
        (unwind-protect
            (progn
              (should (equal made '("emacs" "--new")))
              (should (eq shown buffer))
              (should (equal (buffer-local-value 'lispm-agents-buffer-session-id buffer) "0199"))
              (should (string-prefix-p "New Tool · " (lispm-agents-buffer-label buffer))))
          (kill-buffer buffer)))
      (let ((session '(:agent tool :id "0200" :title "Old work")))
        (lispm-agents-delete-session session)
        (should (equal (car asked) '("Delete the Tool session “Old work”?" . "Delete")))
        (should-not deleted)
        (setq answer t)
        (lispm-agents-delete-session session)
        (should (equal deleted '("0200")))))))

(ert-deftest lispm-test-agents-list-claims-sessions ()
  "Listing the sessions tells a terminal started fresh which session it
runs, the first of its agent begun there since it started, with no list
of the agents drawn: so it is shown again rather than resumed twice."
  (let* ((launched (float-time))
         (buffer (generate-new-buffer "*codex:project*"))
         (lispm-agents-terminal-modes '(fundamental-mode))
         (lispm-agents-terminal-live-p-function #'buffer-live-p))
    (unwind-protect
        (lispm-test--with-agents
            `((codex :name "Codex" :program "emacs"
                     :sessions ,(lambda ()
                                  (list (list :agent 'codex :id "0199" :cwd "/tmp/project/"
                                              :title "Fix the build" :time 2.0
                                              :started (+ launched 1))))))
          (with-current-buffer buffer
            (setq default-directory "/tmp/project/"
                  lispm-agents-buffer-agent 'codex
                  lispm-agents-buffer-launched launched))
          (should-not (lispm-agents-session-buffer "0199"))
          (lispm-agents-sessions-list)
          (should (equal (buffer-local-value 'lispm-agents-buffer-session-id buffer) "0199"))
          (should (equal (lispm-agents-buffer-label buffer) "Fix the build"))
          (should (eq (lispm-agents-session-buffer "0199") buffer)))
      (kill-buffer buffer))))

;;;; The LispM bar

(defmacro lispm-test-with-bar (&rest body)
  "Run BODY in `lispm-test-env' with a bar of its own, drawn nowhere.
LispM itself works in `project'; the bar finds a fake application,
Stickies, whose opening `opened' records."
  (declare (indent 0))
  `(lispm-test-env
     (let* ((lispm-system-agent-root project)
            (opened nil)
            (lispm-bar-source-functions
             (list (lambda ()
                     (list (list :kind 'application :name "Stickies" :detail "Notes on the desktop"
                                 :fields '((3 . "Stickies") (1.5 . "Notes on the desktop"))
                                 :open (lambda () (push 'stickies opened)))))
                   #'lispm-bar-skills #'lispm-bar-tools))
            (lispm-bar-read-function #'funcall)
            (lispm-bar-show-function #'ignore)
            (lispm-bar-hide-function #'ignore)
            (lispm-bar-ready-function (lambda (_agent) t))
            (lispm-bar--open nil) (lispm-bar--asked nil) (lispm-bar--reply nil)
            (lispm-bar--waiting nil) (lispm-bar--expanded nil) (lispm-bar--things nil)
            (lispm-bar--text "") (lispm-bar--found nil) (lispm-bar--choice 0)
            (lispm-bar-history nil))
       (unwind-protect
           (progn ,@body)
         (when (get-buffer lispm-bar-buffer-name)
           (kill-buffer lispm-bar-buffer-name))))))

(defun lispm-test--bar-keys (keys)
  "Open the bar and type KEYS in its field, which must close it."
  (condition-case nil
      (execute-kbd-macro (vconcat (kbd "M-x lispm-ai RET") (kbd keys)))
    (quit nil)))

(ert-deftest lispm-test-bar-system-agent ()
  "LispM itself is one standing agent, of the system rather than a project."
  (lispm-test-with-bar
    (let ((agent (lispm-system-agent)))
      (should (lispm-system-agent-p agent))
      (should (eq (lispm-system-agent) agent))
      (should (eq (lispm-agent-role agent) 'system))
      (should (equal (lispm-agent-root agent) (file-name-as-directory project)))
      (should (string-match-p "You are LispM itself" (lispm-system-prompt agent)))
      ;; The project's own agents are others.
      (let ((default-directory project))
        (should-not (lispm--project-agent))
        (let ((other (lispm-start)))
          (should-not (lispm-system-agent-p other))
          (should (eq (lispm--project-agent) other)))))))

(ert-deftest lispm-test-bar-alone ()
  "With LispM Agent alone, the bar finds LispM's own skills and tools and
speaks of nothing else: no applications, no screen of a desktop."
  (lispm-test-with-bar
    (let ((lispm-bar-source-functions (list #'lispm-bar-skills #'lispm-bar-tools))
          (lispm-bar--open t))
      (lispm-bar--gather)
      (should-not (seq-find (lambda (thing) (eq (plist-get thing :kind) 'application)) lispm-bar--things))
      (should (lispm-bar-open-p))
      (should (string-match-p "^Find skills and tools, or ask LispM\\.$"
                              (substring-no-properties (lispm-bar--text))))
      (should-not (string-match-p "screen\\|desktop\\|menu bar" lispm-bar--instructions))
      (should (eq (symbol-function 'ai) #'lispm-ai)))))

(ert-deftest lispm-test-bar-finds ()
  "The bar finds applications, skills and tools as search_tools ranks them."
  (lispm-test-with-bar
    (should-not (lispm-bar-find "  "))
    (should (equal (plist-get (car (lispm-bar-find "stick")) :name) "Stickies"))
    ;; A misspelt word finds it too, by the same scorer.
    (should (equal (plist-get (car (lispm-bar-find "stikies")) :name) "Stickies"))
    (let ((found (lispm-bar-find "read file" 10)))
      (should (member "read_file" (mapcar (lambda (thing) (plist-get thing :name)) found)))
      (should (eq (plist-get (seq-find (lambda (thing) (equal (plist-get thing :name) "read_file")) found)
                             :kind)
                  'tool)))
    ;; A source that fails leaves the others to find.
    (let ((lispm-bar-source-functions (cons (lambda () (error "Broken")) lispm-bar-source-functions))
          (inhibit-message t))
      (lispm-bar--gather)
      (should (lispm-bar-find "stick")))))

(ert-deftest lispm-test-bar-draws ()
  "The bar asks LispM first, then shows what it found; opened out, under headings."
  (lispm-test-with-bar
    (let ((lispm-bar--open t))
      (lispm-bar--set-text "stick")
      (let ((text (substring-no-properties (lispm-bar--text))))
        (should (string-match-p "\\`❯ Ask LispM “stick”" text))
        (should (string-match-p "^  Stickies  Notes on the desktop.*Application$" text))
        (should (string-match-p "more · C-c C-o chat · C-g close" text)))
      (lispm-bar-next)
      (should (eq (plist-get (lispm-bar--chosen) :kind) 'application))
      (should (string-match-p "^❯ Stickies" (substring-no-properties (lispm-bar--text))))
      (lispm-bar-toggle)
      (let ((text (substring-no-properties (lispm-bar--text))))
        (should (string-match-p "^Applications$" text))
        (should (string-match-p "^Conversation$" text))
        (should (string-match-p "Nothing asked yet\\." text))))))

(ert-deftest lispm-test-bar-opens-applications ()
  "Return on an application found opens it and closes the bar."
  (lispm-test-with-bar
    (lispm-test--bar-keys "stick C-n RET")
    (should (equal opened '(stickies)))
    (should-not lispm-bar--open)
    (should-not (lispm-agent-inbox (lispm-system-agent)))))

(ert-deftest lispm-test-bar-asks ()
  "Return asks LispM what the field holds; the field stays open for the
reply, streamed in, and a skill chosen is named in the request."
  (lispm-test-with-bar
    (let* ((agent (lispm-system-agent))
           (deltas nil)
           (lispm-response-delta-functions
            (list (lambda (who kind text) (push (list (lispm-agent-name who) kind text) deltas)))))
      (setq lispm-test--scripts
            `((lispm ,(lispm-test--msg "end_turn" (lispm-test--text "Hello there.")))))
      (lispm-test--bar-keys "hi SPC LispM RET <escape>")
      (should (equal lispm-bar-history '("hi LispM")))
      (should (equal (car lispm-bar--asked) "hi LispM"))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal lispm-bar--reply "Hello there."))
      (should (equal deltas '((lispm text "streaming..."))))
      (should (string-match-p "^\\* User\nhi LispM$" (lispm-test--transcript agent)))
      (let ((lispm-bar--open t))
        (should (string-match-p "λ hi LispM\n⏺ Hello there\\."
                                (substring-no-properties (lispm-bar--text))))))))

(ert-deftest lispm-test-bar-answers-inline ()
  "A call LispM asks approval for is answered in the empty field, y or n."
  (lispm-test-with-bar
    (let ((agent (lispm-system-agent)))
      (setq lispm-test--scripts
            `((lispm ,(lispm-test--msg "tool_use" (lispm-test--tool "t1" "run_shell"
                                                                   '((command . "echo hi") (why . "To greet."))))
                     ,(lispm-test--msg "end_turn" (lispm-test--text "Said hi.")))))
      (lispm-bar-ask "Greet")
      (should (lispm-test--wait (lambda () (lispm-waiting-calls agent))))
      (let ((lispm-bar--open t))
        (should (string-match-p "▌ Bash command: echo hi\n  To greet\\.\n  Do you want to proceed\\? y yes · n no"
                                (substring-no-properties (lispm-bar--text)))))
      ;; Typed after other text, y is only a letter.
      (with-temp-buffer
        (insert "sa")
        (let ((last-command-event ?y)) (lispm-bar-yes))
        (should (equal (buffer-string) "say")))
      (should (lispm-waiting-calls agent))
      (with-temp-buffer
        (lispm-bar-yes))
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal lispm-bar--reply "Said hi."))
      (should (string-match-p "^hi$" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-bar-approval-message ()
  "LispM itself is answered in the bar: while it is open nothing else says
so, while it is closed a message names the key opening it."
  (lispm-test-with-bar
    (let ((agent (lispm-system-agent))
          (other (let ((default-directory project)) (lispm-start))))
      (let ((lispm-bar--open (selected-window)))
        (should (eq (lispm-bar--approval-message agent "run_shell") t)))
      (should (string-match-p "\\`LispM asks to use Bash: .* to answer\\'"
                              (lispm-bar--approval-message agent "run_shell")))
      (should-not (lispm-bar--approval-message other "run_shell")))))

(ert-deftest lispm-test-bar-without-a-model ()
  "With no model, the bar is a launcher, and what was asked waits for one."
  (lispm-test-with-bar
    (let* ((agent (lispm-system-agent))
           (setup 0)
           (lispm-bar-ready-function (lambda (_agent) nil))
           (lispm-bar-setup-function (lambda () (cl-incf setup))))
      (lispm-bar-ready-p)
      (let ((lispm-bar--open t))
        (lispm-bar--set-text "stick")
        ;; The best thing found is what Return opens.
        (should (eq (plist-get (lispm-bar--chosen) :kind) 'application))
        (should (string-match-p "^  Set up a model…  then LispM is asked “stick”"
                                (substring-no-properties (lispm-bar--text)))))
      ;; Words an application is found by open it.
      (lispm-test--bar-keys "take SPC a SPC note RET")
      (should (equal opened '(stickies)))
      (should (= setup 0))
      (lispm-test--bar-keys "how SPC late SPC is SPC it RET")
      (should (= setup 1))
      (should (equal lispm-bar--waiting '("how late is it" . "how late is it")))
      (should-not (lispm-agent-inbox agent))
      (setq lispm-test--scripts
            `((lispm ,(lispm-test--msg "end_turn" (lispm-test--text "Late.")))))
      (let ((inhibit-message t))
        (lispm-bar-model-ready))
      (should-not lispm-bar--waiting)
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (should (equal lispm-bar--reply "Late."))
      (should (string-match-p "^how late is it$" (lispm-test--transcript agent))))))

(ert-deftest lispm-test-bar-model-ready ()
  "LispM has a model when a backend can be named, its key variable set."
  (lispm-test-env
    (let ((lispm-system-agent-root project))
      (should (lispm-bar-model-ready-p (lispm-system-agent)))
      (let ((lispm-backend nil)
            (process-environment (append '("OPENAI_API_KEY" "ANTHROPIC_API_KEY") process-environment)))
        (should-not (lispm-bar-model-ready-p (lispm-system-agent))))
      (let ((lispm-backends (cons '(test :api anthropic :url "http://fake.invalid" :key-env "LISPM_TEST_KEY")
                                  lispm-backends))
            (process-environment (cons "LISPM_TEST_KEY" process-environment)))
        (should-not (lispm-bar-model-ready-p (lispm-system-agent)))
        (setenv "LISPM_TEST_KEY" "secret")
        (should (lispm-bar-model-ready-p (lispm-system-agent)))))))

(provide 'lispm-test)
;;; lispm-test.el ends here
