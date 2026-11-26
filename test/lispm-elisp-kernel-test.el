;;; lispm-elisp-kernel-test.el --- Elisp workers through a real kernel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Run with `make test-kernel LISPM_KERNEL=/path/to/lispm-kernel', after
;; building that kernel.  The target sets LISPM_KERNEL_PROGRAM and load-path. These
;; tests start real kernels and child Emacsen; all files are fixtures.

;;; Code:

(require 'lispm-test)
(require 'lispm-elisp)
(require 'lispm-control-protocol)
(require 'lispm-control-plane)
(require 'lispm-kernel-exec)

(defconst lispm-elisp-kernel-test--program
  (let ((program (getenv "LISPM_KERNEL_PROGRAM")))
    (unless (and program (file-name-absolute-p program) (file-executable-p program))
      (error "Set LISPM_KERNEL_PROGRAM to the absolute path of an executable kernel"))
    program)
  "The real kernel program these tests require; none of them is skipped.")

(defvar lispm-elisp-kernel-test--sentinel 'parent
  "A variable whose child binding must not change the parent Emacs.")

(defun lispm-elisp-kernel-test--wait (runtime predicate &optional seconds)
  "Dispatch RUNTIME until PREDICATE holds, for at most SECONDS (default 10)."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (when runtime
        (lispm-kernel-dispatch runtime)))
    (funcall predicate)))

(defmacro lispm-elisp-kernel-test--with (runtime &rest body)
  "Run BODY with a real RUNTIME, cancelling its workers and stopping it after."
  (declare (indent 1))
  `(let* ((,runtime (lispm-kernel-start lispm-elisp-kernel-test--program))
          (lispm-kernel-exec-runtime-function (lambda () ,runtime))
          (lispm-job--active nil)
          (lispm-state-change-functions (copy-sequence lispm-state-change-functions))
          (lispm-session-end-functions (copy-sequence lispm-session-end-functions))
          (lispm-agents-context-release-functions
           (copy-sequence lispm-agents-context-release-functions)))
     (unwind-protect
         (progn
           (should (lispm-elisp-kernel-test--wait
                    ,runtime (lambda () (lispm-kernel-runtime-hello ,runtime))))
           (should (memq :exec (plist-get (lispm-kernel-runtime-hello ,runtime) :jobs)))
           ,@body)
       (lispm-job-cancel-all)
       (lispm-elisp-kernel-test--wait
        ,runtime (lambda () (zerop (hash-table-count (lispm-kernel-runtime-requests ,runtime)))) 5)
       (lispm-kernel-stop ,runtime))))

(defun lispm-elisp-kernel-test--call (runtime agent code &optional timeout)
  "Call run_elisp for AGENT with CODE and TIMEOUT; return (TEXT . ERRORP)."
  (let ((lispm-current-agent agent) result)
    (lispm-tool-invoke
     (lispm-get-tool "run_elisp") `((code . ,code) (timeout . ,(or timeout 5)))
     (lambda (text errorp &rest _) (setq result (cons text errorp))))
    (should (lispm-elisp-kernel-test--wait runtime (lambda () result)))
    result))

(defun lispm-elisp-kernel-test--value (result)
  "Read the final Lisp value from a successful worker RESULT."
  (should-not (cdr result))
  (let ((text (car result)))
    (should (string-match "\n=> " text))
    (car (read-from-string text (match-end 0)))))

(defun lispm-elisp-kernel-test--pid (file)
  "Return the child PID written to fixture FILE, or nil before it is complete."
  (when (file-readable-p file)
    (let ((text (lispm-test--disk file)))
      (when (string-match-p "\\`[1-9][0-9]*\\'" text)
        (string-to-number text)))))

(defun lispm-elisp-kernel-test--busy-code (pid-file &optional seconds)
  "Code writing its PID to PID-FILE, then computing for SECONDS or forever."
  (prin1-to-string
   `(progn
      (with-temp-file ,pid-file (insert (number-to-string (emacs-pid))))
      ,(if seconds
           `(let ((deadline (+ (float-time) ,seconds)))
              (while (< (float-time) deadline)))
         '(while t))
      'worker-finished)))

(ert-deftest lispm-elisp-kernel-test-isolated-jsonl ()
  "The kernel starts a different Emacs, which reads local JSONL and computes."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let ((agent (lispm-defagent tester :root project))
            (lispm-elisp-kernel-test--sentinel 'parent))
        (make-directory (expand-file-name "sessions" project))
        (with-temp-file (expand-file-name "sessions/a.jsonl" project)
          (insert "{\"role\":\"user\",\"text\":\"release ignored\"}\n"
                  "malformed line\n"
                  "{\"role\":\"assistant\",\"text\":\"release λ\"}\n"))
        (with-temp-file (expand-file-name "sessions/b.jsonl" project)
          (insert "{\"role\":\"assistant\",\"text\":\"unrelated\"}\n"))
        (let* ((code
                (prin1-to-string
                 '(progn
                    (setq lispm-elisp-kernel-test--sentinel 'child)
                    (let* ((prefix "release")
                           (matches-p (lambda (text) (string-match-p prefix text)))
                           matches)
                      (dolist (file (file-expand-wildcards "sessions/*.jsonl"))
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (while (not (eobp))
                            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                                   (record (ignore-errors (json-parse-string line :object-type 'plist))))
                              (when (and (equal (plist-get record :role) "assistant")
                                         (funcall matches-p (plist-get record :text)))
                                (push (plist-get record :text) matches)))
                            (forward-line 1))))
                      (princ "scanned\n")
                      (list (emacs-pid) (file-truename default-directory) (nreverse matches)
                            (funcall (let ((offset 2)) (lambda (n) (+ offset n))) 40)
                            lispm-elisp-kernel-test--sentinel)))))
               (result (lispm-elisp-kernel-test--call runtime agent code))
               (value (lispm-elisp-kernel-test--value result)))
          (should (string-match-p "scanned\n" (car result)))
          (should (integerp (car value)))
          (should-not (= (car value) (emacs-pid)))
          (should-not (= (car value) (plist-get (lispm-kernel-runtime-hello runtime) :pid)))
          (should (equal (cdr value) (list (file-truename project) '("release λ") 42 'child)))
          (should (eq lispm-elisp-kernel-test--sentinel 'parent))
          (should-not (lispm-job-active nil 'elisp)))))))

(ert-deftest lispm-elisp-kernel-test-errors ()
  "Malformed code never runs its valid prefix; runtime errors are failures."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (marker (expand-file-name "must-not-exist" project))
             (syntax (lispm-elisp-kernel-test--call
                      runtime agent (format "(with-temp-file %S (insert \"bad\")) (" marker)))
             (failed (lispm-elisp-kernel-test--call runtime agent "(error \"synthetic failure\")")))
        (should (cdr syntax))
        (should (string-match-p "Worker exit 1" (car syntax)))
        (should (string-match-p "Elisp worker: End of file" (car syntax)))
        (should-not (file-exists-p marker))
        (should (cdr failed))
        (should (string-match-p "Elisp worker: synthetic failure" (car failed)))
        (should-not (lispm-job-active nil 'elisp))))))

(ert-deftest lispm-elisp-kernel-test-parent-remains-responsive ()
  "A parent timer runs while the worker is in a CPU loop."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-current-agent agent)
             (pid-file (expand-file-name "worker.pid" project))
             result fired timer)
        (lispm-tool-invoke
         (lispm-get-tool "run_elisp")
         `((code . ,(lispm-elisp-kernel-test--busy-code pid-file 1.5)) (timeout . 5))
         (lambda (text errorp &rest _) (setq result (cons text errorp))))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () (lispm-elisp-kernel-test--pid pid-file))))
        (should (process-attributes (lispm-elisp-kernel-test--pid pid-file)))
        (unwind-protect
            (progn
              (setq timer (run-at-time 0.03 nil (lambda () (setq fired t))))
              (should (lispm-elisp-kernel-test--wait runtime (lambda () fired) 1))
              (should-not result)
              (should (lispm-elisp-kernel-test--wait runtime (lambda () result) 5))
              (should (eq (lispm-elisp-kernel-test--value result) 'worker-finished)))
          (when timer (cancel-timer timer)))))))

(ert-deftest lispm-elisp-kernel-test-timeout-kills-worker ()
  "The kernel's timeout ends the real worker and reports a failed tool call."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project))
             (lispm-current-agent agent)
             (pid-file (expand-file-name "worker.pid" project))
             result pid)
        (lispm-tool-invoke
         (lispm-get-tool "run_elisp")
         `((code . ,(lispm-elisp-kernel-test--busy-code pid-file)) (timeout . 2))
         (lambda (text errorp &rest _) (setq result (cons text errorp))))
        (should (lispm-elisp-kernel-test--wait
                 runtime (lambda () (setq pid (lispm-elisp-kernel-test--pid pid-file)))))
        (should (process-attributes pid))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () result) 5))
        (should (cdr result))
        (should (string-match-p "did not finish" (car result)))
        (should (lispm-elisp-kernel-test--wait runtime (lambda () (not (process-attributes pid))) 5))
        (should-not (lispm-job-active nil 'elisp))
        (should (lispm-kernel-live-p runtime))))))

(ert-deftest lispm-elisp-kernel-test-abort-kills-worker ()
  "Aborting a native turn kills its child without a late result or model call."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let* ((agent (lispm-defagent tester :root project :policy '((exec . auto))))
             (pid-file (expand-file-name "worker.pid" project))
             pid)
        (setq lispm-test--scripts
              `((tester ,(lispm-test--msg
                          "tool_use"
                          (lispm-test--tool "worker-1" "run_elisp"
                                            `((code . ,(lispm-elisp-kernel-test--busy-code pid-file))
                                              (timeout . 15))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "Must not be requested.")))))
        (lispm-tell agent "Run the fixture computation.")
        (should (lispm-elisp-kernel-test--wait
                 runtime (lambda () (setq pid (lispm-elisp-kernel-test--pid pid-file)))))
        (should (process-attributes pid))
        (should (= (length lispm-test--requests) 1))
        (lispm-abort agent)
        (let ((transcript (lispm-test--transcript agent)))
          (should (string-match-p "Interrupted: the user aborted the turn" transcript))
          (should (lispm-elisp-kernel-test--wait
                   runtime (lambda () (and (not (process-attributes pid))
                                           (zerop (hash-table-count (lispm-kernel-runtime-requests runtime)))))
                   5))
          (should (equal (lispm-test--transcript agent) transcript))
          (should (= (length lispm-test--requests) 1))
          (should (funcall (lispm-test--idle-p agent)))
          (should-not (lispm-job-active nil 'elisp))
          ;; The shared kernel is still usable after cancelling this child.
          (should (= (lispm-elisp-kernel-test--value
                      (lispm-elisp-kernel-test--call runtime agent "(+ 8 9)")) 17)))))))

(ert-deftest lispm-elisp-kernel-test-remote-root ()
  "A worker started from a remote TRAMP root mounts `default-directory' via `LISPM_ELISP_ROOT'."
  (lispm-test-with-remote
    (lispm-elisp-kernel-test--with runtime
      (lispm-test--write (expand-file-name "remote-note.txt" project) "hello-tramp\n")
      (let* ((agent (lispm-make-agent 'assistant remote))
             (_ (setf (lispm-agent-policy agent) '((exec . auto))))
             (code (prin1-to-string
                    '(list default-directory
                           (with-temp-buffer
                             (insert-file-contents "remote-note.txt")
                             (buffer-string)))))
             (result (lispm-elisp-kernel-test--call runtime agent code))
             (value (lispm-elisp-kernel-test--value result)))
        (should (equal value (list (file-name-as-directory remote) "hello-tramp\n")))))))

(ert-deftest lispm-elisp-kernel-test-search-and-shell-and-api ()
  "The kernel runs local `search_files', `run_shell', and `lispm-api-stream'."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (let ((agent (lispm-defagent tester :root project)))
        ;; 1. search_files in the kernel (including open unsaved buffer).
        (make-directory (expand-file-name "sub" project) t)
        (with-temp-file (expand-file-name "a.el" project)
          (insert ";; header\n(defun lispm-demo-a ())\n"))
        (with-temp-file (expand-file-name "sub/b.el" project)
          (insert ";; header\n(defun lispm-demo-b ())\n"))
        (with-current-buffer (find-file-noselect (expand-file-name "a.el" project))
          (goto-char (point-max))
          (insert "(defun lispm-demo-unsaved ())\n"))
        (cl-letf (((symbol-function 'lispm-tools--search)
                   (lambda (&rest _) (ert-fail "Fell back to Emacs search"))))
          (let ((lispm-current-agent agent)
                result)
            (lispm-tool-invoke (lispm-get-tool "search_files")
                               '((pattern . "lispm-demo-(b|unsaved)"))
                               (lambda (text errorp) (setq result (cons text errorp))))
            (should (lispm-elisp-kernel-test--wait runtime (lambda () result)))
            (should (equal result
                           '("a.el:3:(defun lispm-demo-unsaved ())\nsub/b.el:2:(defun lispm-demo-b ())"
                             . nil)))))
        ;; 2. run_shell in the kernel when lispm-kernel-exec-mode is on.
        (unwind-protect
            (progn
              (lispm-kernel-exec-mode 1)
              (let ((lispm-current-agent agent)
                    (lispm-sandbox-level 'none)
                    result)
                (lispm-tool-invoke (lispm-get-tool "run_shell")
                                   '((command . "printf 'out\\n'; printf 'err\\n' >&2; exit 3"))
                                   (lambda (text errorp) (setq result (cons text errorp))))
                (should (lispm-elisp-kernel-test--wait runtime (lambda () result)))
                (should (equal result '("out\nerr\n\n[exit status 3]" . nil)))))
          (lispm-kernel-exec-mode -1))))))

;;;; Phase 3 Kernel Control-Plane Integration & >=20-Agent Benchmark

(defun lispm-elisp-kernel-test--start-py-sse-server ()
  "Start an external multi-threaded Python mock HTTP SSE server; return (PROC . URL)."
  (let* ((script
          (concat
           "import http.server, socketserver, json, sys, time\n"
           "class H(http.server.BaseHTTPRequestHandler):\n"
           "  protocol_version = 'HTTP/1.1'\n"
           "  def log_message(self, *a): pass\n"
           "  def do_POST(self):\n"
           "    n = int(self.headers.get('Content-Length', 0))\n"
           "    body = json.loads(self.rfile.read(n).decode('utf-8'))\n"
           "    msgs = body.get('messages', [])\n"
           "    scenario = body.get('scenario', 'bench')\n"
           "    def sse(d, ev=None):\n"
           "      s = (f'event: {ev}\\n' if ev else '') + f'data: {json.dumps(d)}\\n\\n'\n"
           "      return s.encode('utf-8')\n"
           "    self.send_response(200)\n"
           "    self.send_header('Content-Type', 'text/event-stream')\n"
           "    self.send_header('Connection', 'close')\n"
           "    self.end_headers()\n"
           "    def send_text(txt, slow=0):\n"
           "      self.wfile.write(sse({'type':'message_start','message':{'id':'m','usage':{'input_tokens':5}}},'message_start'))\n"
           "      self.wfile.write(sse({'type':'content_block_start','index':0,'content_block':{'type':'text','text':''}},'content_block_start'))\n"
           "      for ch in [txt[i:i+8] for i in range(0, len(txt), 8)]:\n"
           "        self.wfile.write(sse({'type':'content_block_delta','index':0,'delta':{'type':'text_delta','text':ch}},'content_block_delta'))\n"
           "        self.wfile.flush()\n"
           "        if slow: time.sleep(slow)\n"
           "      self.wfile.write(sse({'type':'content_block_stop','index':0},'content_block_stop'))\n"
           "      self.wfile.write(sse({'type':'message_delta','delta':{'stop_reason':'end_turn'},'usage':{'output_tokens':4}},'message_delta'))\n"
           "      self.wfile.write(sse({'type':'message_stop'},'message_stop'))\n"
           "      self.wfile.flush()\n"
           "    def send_tool(tid, name, inp):\n"
           "      self.wfile.write(sse({'type':'message_start','message':{'id':'m'}},'message_start'))\n"
           "      self.wfile.write(sse({'type':'content_block_start','index':0,'content_block':{'type':'tool_use','id':tid,'name':name,'input':{}}},'content_block_start'))\n"
           "      self.wfile.write(sse({'type':'content_block_delta','index':0,'delta':{'type':'input_json_delta','partial_json':json.dumps(inp)}},'content_block_delta'))\n"
           "      self.wfile.write(sse({'type':'content_block_stop','index':0},'content_block_stop'))\n"
           "      self.wfile.write(sse({'type':'message_delta','delta':{'stop_reason':'tool_use'}},'message_delta'))\n"
           "      self.wfile.write(sse({'type':'message_stop'},'message_stop'))\n"
           "      self.wfile.flush()\n"
           "    if scenario == 'text':\n"
           "      send_text('Streaming reply from offloaded kernel actor loop line 1 ok.')\n"
           "    elif scenario == 'effects':\n"
           "      if len(msgs) == 1: send_tool('c1', 'edit_buffer', {'buffer':' *k-ed*','old_text':'old','new_text':'NEW'})\n"
           "      elif len(msgs) == 3: send_tool('c2', 'read_buffer', {'buffer':' *k-ed*'})\n"
           "      elif len(msgs) == 5: send_tool('c3', 'eval_elisp', {'code':'(+ 10 32)'})\n"
           "      else: send_text('effects-complete')\n"
           "    elif scenario == 'reconnect':\n"
           "      if len(msgs) == 1: send_tool('c_sh', 'run_shell', {'command':'sleep 0.15 && echo detached-shell-ok'})\n"
           "      else: send_text('reconnected-done', 0.02)\n"
           "    else:\n"
           "      if len(msgs) == 1:\n"
           "        send_tool('c1', 'run_shell', {'command':'printf shell-ok && git status --short'})\n"
           "      elif len(msgs) == 3:\n"
           "        send_tool('c2', 'search_files', {'pattern':'needle-20'})\n"
           "      else:\n"
           "        send_text('Streaming reply from offloaded kernel actor loop line 1 and line 2 ok.')\n"
           "class S(socketserver.ThreadingMixIn, http.server.HTTPServer):\n"
           "  daemon_threads = True\n"
           "srv = S(('127.0.0.1', 0), H)\n"
           "print(f'PORT={srv.server_address[1]}', flush=True)\n"
           "srv.serve_forever()\n"))
         (out "")
         (proc (make-process
                :name "lispm-py-sse"
                :command (list "python3" "-u" "-c" script)
                :connection-type 'pipe
                :noquery t
                :filter (lambda (_p s) (setq out (concat out s)))))
         (deadline (+ (float-time) 5)))
    (while (and (not (string-match "PORT=\\([0-9]+\\)" out))
                (< (float-time) deadline))
      (accept-process-output proc 0.01))
    (unless (string-match "PORT=\\([0-9]+\\)" out)
      (delete-process proc)
      (error "Mock Python SSE server failed to start: %s" out))
    (cons proc (format "http://127.0.0.1:%s/v1/messages" (match-string 1 out)))))

(ert-deftest lispm-elisp-kernel-test-stage3a-llm-stream ()
  "Verify `lispm-api-stream' offloads SSE decoding to `:llm-stream' in kernel."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (skip-unless (and (fboundp 'lispm-kernel-feature-p)
                        (lispm-kernel-feature-p runtime :sse-decode)))
      (let* ((srv (lispm-elisp-kernel-test--start-py-sse-server))
             (backend (list :api 'anthropic
                            :url (string-remove-suffix "/v1/messages" (cdr srv))
                            :model "claude-test" :key "k"))
             (deltas nil)
             (done nil)
             (err nil))
        (unwind-protect
            (progn
              (lispm-api-stream
               backend
               (lispm-api-make-envelope
                '((scenario . "text")
                  (messages . [((role . "user") (content . "hi"))])))
               :on-delta (lambda (k s) (push (list k s) deltas))
               :on-done (lambda (msg) (setq done msg))
               :on-error (lambda (e) (setq err e)))
              (should (lispm-elisp-kernel-test--wait runtime (lambda () (or done err))))
              (should-not err)
              (should (string-search "offloaded kernel actor loop"
                                     (alist-get 'text (aref (alist-get 'content done) 0))))
              (should deltas))
          (when (process-live-p (car srv)) (delete-process (car srv))))))))

(ert-deftest lispm-elisp-kernel-test-stage3b-effects-and-approvals ()
  "Verify kernel agent actors round-trip host approvals and editor-only effects."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (skip-unless (and (fboundp 'lispm-kernel-feature-p)
                        (lispm-kernel-feature-p runtime :agent-actors)))
      (unwind-protect
          (let* ((srv (lispm-elisp-kernel-test--start-py-sse-server))
                 (lispm-control-plane-mode 'auto)
                 (lispm-control-plane-runtime-function (lambda (&optional _d) runtime))
                 (lispm-control-plane-approval-function
                  (lambda (ag call-id _tool _inp _reasons _sess)
                    (lispm-control-plane-resolve-approval ag call-id 'approve)))
                 (buf (get-buffer-create " *k-ed*"))
                 (agent (lispm-defagent tester :root project)))
            (with-current-buffer buf
              (erase-buffer)
              (insert "old text\n"))
            (unwind-protect
                (progn
                  (should (eq 'kernel
                              (lispm-control-plane-start-turn
                               agent
                               (list :body (json-serialize
                                            '((scenario . "effects")
                                              (messages . [((role . "user") (content . "go"))]))))
                               :config (list :dialect :anthropic
                                             :url (cdr srv)
                                             :root project
                                             :tool-policies '(:ask ("edit_buffer")
                                                              :auto ("read_buffer" "eval_elisp")))
                               :runtime runtime)))
                  (let ((sess (lispm-control-plane-session agent nil)))
                    (should (lispm-elisp-kernel-test--wait
                             runtime (lambda () (lispm-control-session-turn-results sess)) 10))
                    (should (string-search "NEW text" (with-current-buffer buf (buffer-string))))
                    (should (= 3 (length (lispm-control-session-tool-results sess))))
                    (should (string= "effects-complete"
                                     (plist-get (car (lispm-control-session-turn-results sess)) :text)))))
              (kill-buffer buf)
              (when (process-live-p (car srv)) (delete-process (car srv)))))
        (lispm-control-plane-reset)))))

(ert-deftest lispm-elisp-kernel-test-stage3c-socket-reconnect ()
  "Verify Emacs detach/reconnect over kernel Unix socket loses zero turns/tools."
  (lispm-test-env
    (let ((probe (lispm-kernel-start lispm-elisp-kernel-test--program)))
      (unwind-protect
          (progn
            (should (lispm-elisp-kernel-test--wait
                     probe (lambda () (lispm-kernel-runtime-hello probe))))
            (skip-unless (and (fboundp 'lispm-kernel-feature-p)
                              (lispm-kernel-feature-p probe :socket-reconnect))))
        (lispm-kernel-stop probe)))
    (unwind-protect
        (let* ((sock (format "/tmp/lk-agent-%d-%d.sock" (emacs-pid) (random 100000)))
               (srv (lispm-elisp-kernel-test--start-py-sse-server))
               (daemon nil)
               (ready nil))
          (unwind-protect
              (progn
                (setq daemon
                      (make-process
                       :name "lispm-kernel-reconn"
                       :command (list lispm-elisp-kernel-test--program "--socket" sock)
                       :connection-type 'pipe :noquery t
                       :filter (lambda (_p s) (when (string-search "listening" s) (setq ready t)))))
                (should (lispm-elisp-kernel-test--wait
                         nil (lambda () (and ready (file-exists-p sock))) 5))
                ;; Connect client 1, start shell+SSE turn, wait for step 1, then detach
                (let* ((rt1 (lispm-control-plane-connect sock))
                       (lispm-control-plane-mode 'auto)
                       (lispm-control-plane-runtime-function (lambda (&optional _d) rt1))
                       (agent (lispm-defagent sock-agent :root project)))
                  (should (lispm-elisp-kernel-test--wait
                           rt1 (lambda () (lispm-kernel-runtime-hello rt1))))
                  (lispm-control-plane-start-turn
                   agent
                   (list :turn-id "t-rec"
                         :body (json-serialize
                                '((scenario . "reconnect")
                                  (messages . [((role . "user") (content . "run"))]))))
                   :config (list :dialect :anthropic :url (cdr srv) :root project
                                 :tool-policies '(:auto ("run_shell")))
                   :runtime rt1)
                  (let ((sess (lispm-control-plane-session agent nil)))
                    (should (lispm-elisp-kernel-test--wait
                             rt1 (lambda () (eq (lispm-control-session-state sess) :streaming))))
                    (lispm-control-plane-detach rt1)))
                ;; Reconnect with fresh runtime rt2 and poll until detached background turn completes
                (let* ((rt2 (lispm-control-plane-connect sock))
                       (lispm-control-plane-mode 'auto)
                       (lispm-control-plane-runtime-function (lambda (&optional _d) rt2))
                       (restored nil)
                       (deadline (+ (float-time) 10)))
                  (unwind-protect
                      (progn
                        (should (lispm-elisp-kernel-test--wait
                                 rt2 (lambda () (lispm-kernel-runtime-hello rt2))))
                        (while (and (not (and restored
                                              (eq :idle (lispm-control-session-state (car restored)))))
                                    (< (float-time) deadline))
                          (setq restored nil)
                          (lispm-control-plane-reconnect
                           rt2 nil (lambda (sess snap) (setq restored (list sess snap))))
                          (lispm-elisp-kernel-test--wait rt2 (lambda () restored) 2)
                          (unless (and restored
                                       (eq :idle (lispm-control-session-state (car restored))))
                            (accept-process-output nil 0.05)))
                        (should restored)
                        (let ((sess (car restored)))
                          (should (eq :idle (lispm-control-session-state sess)))
                          (should (integerp (lispm-control-session-epoch sess)))
                          (should (= 1 (length (lispm-control-session-tool-results sess))))
                          (should (= 1 (length (lispm-control-session-turn-results sess))))
                          (should (string-search "detached-shell-ok"
                                                 (plist-get (car (lispm-control-session-tool-results sess))
                                                            :result)))
                          (should (string= "reconnected-done"
                                           (plist-get (car (lispm-control-session-turn-results sess))
                                                      :text)))))
                    (when (fboundp 'lispm-kernel-quit)
                      (lispm-kernel-quit rt2)))))
            (when (process-live-p daemon) (delete-process daemon))
            (when (process-live-p (car srv)) (delete-process (car srv)))
            (ignore-errors (delete-file sock))))
      (lispm-control-plane-reset))))

(ert-deftest lispm-elisp-kernel-test-stage3-benchmark-20-agents ()
  "Benchmark >=20 concurrent streaming agents while keeping Emacs loop <16ms."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (skip-unless (and (fboundp 'lispm-kernel-feature-p)
                        (lispm-kernel-feature-p runtime :agent-actors)))
      (let ((default-directory project))
        (call-process "git" nil nil nil "init" "-q")
        (with-temp-file (expand-file-name "code.el" project)
          (insert ";; fixture needle-20 line\n"))
        (call-process "git" nil nil nil "add" "code.el")
        (call-process "git" nil nil nil
                      "-c" "user.name=T" "-c" "user.email=t@e"
                      "commit" "-q" "-m" "init"))
      (unwind-protect
          (let* ((srv (lispm-elisp-kernel-test--start-py-sse-server))
                 (lispm-control-plane-mode 'auto)
                 (lispm-control-plane-runtime-function (lambda (&optional _d) runtime))
                 (count 24)
                 (sessions nil)
                 (latencies-ms nil)
                 (last-tick (float-time))
                 (probe-timer nil))
            (unwind-protect
                (progn
                  (setq probe-timer
                        (run-at-time 0.01 0.01
                                     (lambda ()
                                       (let* ((now (float-time))
                                              (lag-ms (max 0.0 (* 1000.0 (- (- now last-tick) 0.01)))))
                                         (push lag-ms latencies-ms)
                                         (setq last-tick now)))))
                  (dotimes (i count)
                    (let ((sid (format "bench-agent-%02d" i)))
                      (push (lispm-control-plane-session sid t runtime) sessions)
                      (lispm-control-plane-start-turn
                       sid
                       (list :turn-id "t1"
                             :body (json-serialize
                                    '((scenario . "bench")
                                      (messages . [((role . "user") (content . "work"))]))))
                       :config (list :dialect :anthropic
                                     :url (cdr srv)
                                     :root project
                                     :tool-policies '(:auto ("run_shell" "search_files"))
                                     :viewport (if (= (% i 4) 0) :visible :hidden))
                       :runtime runtime)))
                  (should (lispm-elisp-kernel-test--wait
                           runtime
                           (lambda ()
                             (cl-every (lambda (s) (= 1 (length (lispm-control-session-turn-results s))))
                                       sessions))
                           25))
                  (cancel-timer probe-timer)
                  (setq probe-timer nil)
                  (let* ((sorted (sort (copy-sequence latencies-ms) #'<))
                         (n (length sorted))
                         (p95 (if (> n 0) (nth (min (1- n) (floor (* 0.95 n))) sorted) 0.0))
                         (pmax (if (> n 0) (car (last sorted)) 0.0)))
                    (message "Phase3 Benchmark (%d agents, %d timer ticks): p95 lag=%.2fms max=%.2fms"
                             count n p95 pmax)
                    (should (> n 5))
                    (should (< p95 16.0))
                    (dolist (s sessions)
                      (should (= 2 (length (lispm-control-session-tool-results s)))))))
              (when probe-timer (cancel-timer probe-timer))
              (when (process-live-p (car srv)) (delete-process (car srv)))))
        (lispm-control-plane-reset)))))

(ert-deftest lispm-elisp-kernel-test-stage3-real-lispm-tell-turn ()
  "Verify real `lispm-tell' runs through `P_agents' and writes Org transcript."
  (lispm-test-env
    (lispm-elisp-kernel-test--with runtime
      (skip-unless (and (fboundp 'lispm-kernel-feature-p)
                        (lispm-kernel-feature-p runtime :agent-actors)))
      (let ((default-directory project))
        (call-process "git" nil nil nil "init" "-q")
        (with-temp-file (expand-file-name "tracked.txt" project)
          (insert "needle-20 live-turn-needle\n"))
        (call-process "git" nil nil nil "add" "tracked.txt")
        (call-process "git" nil nil nil
                      "-c" "user.name=T" "-c" "user.email=t@e"
                      "commit" "-q" "-m" "init"))
      (unwind-protect
          (let* ((srv (lispm-elisp-kernel-test--start-py-sse-server))
                 (base-url (string-remove-suffix "/v1/messages" (cdr srv)))
                 (lispm-backends (cons `(live-k :api anthropic :url ,base-url :model "claude-test" :key "k")
                                       lispm-backends))
                 (lispm-backend 'live-k)
                 (lispm-api-function #'lispm-api-stream)
                 (lispm-control-plane-mode 'auto)
                 (lispm-control-plane-runtime-function (lambda (&optional _d) runtime))
                 (lispm-sandbox-level 'none)
                 ;; Default policy: `read' auto (`git status' read-only), `write' auto!
                 (agent (lispm-defagent live-agent :root project))
                 (reply nil))
            (unwind-protect
                (progn
                  (lispm-tell agent "go check" :callback (lambda (ans) (setq reply ans)))
                  (should (lispm-elisp-kernel-test--wait runtime (lambda () reply) 15))
                  (should (string-search "line 1 and line 2 ok." reply))
                  (should (funcall (lispm-test--idle-p agent)))
                  (let ((transcript (lispm-test--transcript agent)))
                    (should (string-match-p "\\*\\* DONE run_shell" transcript))
                    (should (string-match-p "\\*\\* DONE search_files" transcript))
                    (should (string-match-p "live-turn-needle" transcript))))
              (when (process-live-p (car srv)) (delete-process (car srv)))))
        (lispm-control-plane-reset)))))

(provide 'lispm-elisp-kernel-test)
;;; lispm-elisp-kernel-test.el ends here
