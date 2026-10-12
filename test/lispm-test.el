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
                       (funcall (plist-get callbacks :on-delta) 'text "streaming...")
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
          (lispm--agents nil)
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
      (insert (lispm-transcript-header "t")
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
    (insert (lispm-transcript-header "t")
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
      (let ((sse (lispm-sse--create)) deltas)
        (cl-loop for i from 0 below (length stream) by size
                 do (lispm-sse-feed sse (substring stream i (min (length stream) (+ i size)))
                                    (lambda (kind text) (push (cons kind text) deltas))))
        (let ((message (lispm-sse-result sse)))
          (should (lispm-sse-done sse))
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
  (should (equal (lispm-sse--after-fallback
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
                 (lispm-api-request (alist-get 'rotom lispm-backends)
                                    "gpt-5.5" "high" "sys" tools messages)))
      (should-not (assq 'thinking body))
      (should-not (assq 'fallbacks body))
      (should-not betas)
      (should (eq (alist-get 'stream body) :false))
      (should (equal (alist-get 'output_config body) '((effort . "high"))))
      (should-not (assq 'eager_input_streaming (aref (alist-get 'tools body) 0)))
      (should (equal (alist-get 'content (aref (alist-get 'messages body) 0))
                     [((type . "tool_result") (tool_use_id . "x") (content . "Error: boom"))])))))

(ert-deftest lispm-test-curl-stream ()
  "The curl transport streams, decodes UTF-8 split across chunks and reports errors."
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
          (let ((backend (list :url (format "http://127.0.0.1:%d" (string-to-number (funcall output)))
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
            (should (lispm-api-retryable-p err))))
      (delete-process server)
      (kill-buffer (process-buffer server))
      (delete-file log))))

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
                     '("     2\ttwo\n[... 2 more lines; continue with offset 3]" . nil)))
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
                     '("Saved memory \"Indentation\"." . nil)))
      (should (equal (car (lispm-test--call agent "remember"
                                            '((title . "indentation") (content . "Two spaces, no tabs"))))
                     "Updated memory \"indentation\"."))
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
                         (concat "keeper\nkeeper: 6 changes\n\n"
                                 "keeper: remember \"Indentation\"\n"
                                 "keeper: update memory \"indentation\"\n"
                                 "keeper: remember \"Editor\"\n"
                                 "keeper: forget \"Editor\"\n"
                                 "keeper: add task " id "\n"
                                 "keeper: task " id " -> DONE\n\n")))
          (lispm-test--call agent "remember" '((title . "Later") (content . "x")))
          (should (lispm-test--wait settled 10))
          (should (equal (shell-command-to-string "git log --format=%s")
                         "keeper: remember \"Later\"\nkeeper: 6 changes\n")))))))

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
      (should (string-match-p "tester +idle" (with-current-buffer "*lispm-agents*" (buffer-string))))
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

(provide 'lispm-test)
;;; lispm-test.el ends here
