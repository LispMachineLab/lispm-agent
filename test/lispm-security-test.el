;;; lispm-security-test.el --- Security regression tests for Phase 0 hardening  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'lispm-test)
(require 'lispm-agents-ide)
(require 'lispm-agents-codex)
(require 'lispm-agents-context)
(require 'lispm-websocket)
(require 'lispm-batch)

(defvar lispm-kernel-exec-runtime-function)

(defvar lispm-security-test--pwned nil
  "Sentinel flipped only if a `run_lisp' escape runs arbitrary code.")

(defmacro lispm-security-test--with-server (authorize handler &rest body)
  "Start local HTTP server with AUTHORIZE and HANDLER; run BODY binding `port'."
  (declare (indent 2))
  `(let* ((server (lispm-local-http-serve "lispm-sec-http" ,authorize ,handler))
          (port (process-contact server :service)))
     (unwind-protect
         (progn ,@body)
       (delete-process server))))

(defun lispm-security-test--raw-http (port raw)
  "Send RAW HTTP bytes to 127.0.0.1:PORT and return raw response string."
  (let* ((response "")
         (proc (make-network-process
                :name "lispm-sec-raw-http" :host "127.0.0.1" :service port
                :coding 'binary
                :filter (lambda (_p chunk) (setq response (concat response chunk)))
                :sentinel #'ignore)))
    (unwind-protect
        (progn
          (process-send-string proc raw)
          (lispm-test--wait (lambda () (not (process-live-p proc))) 5)
          response)
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest lispm-security-test-run-lisp-early-macro-escapes ()
  "`run_lisp' blocks expand-time RCE via setf/push/pcase/rx/cl-flet smuggling."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (lispm-security-test--pwned nil))
      (dolist (code
               '("(setf (eval-when-compile (setq lispm-security-test--pwned t)) 1)"
                 "(let ((x nil)) (push 1 (eval-and-compile (setq lispm-security-test--pwned t))))"
                 "(pcase \"a\" ((rx (eval (setq lispm-security-test--pwned t))) 1))"
                 "(cl-flet ((f () 1)) (let ((run_elisp_forms (eval-when-compile (setq lispm-security-test--pwned t)))) 1))"
                 "(cl-flet ((f () 1)) (let ((eval_elisp_forms (rx (eval (setq lispm-security-test--pwned t))))) 1))"
                 "(setf (car (list 1)) 2)"))
        (let ((res (lispm-test--lisp agent code)))
          (should (cdr res))
          (should (string-prefix-p "Not allowed in run_lisp:" (car res)))
          (should-not lispm-security-test--pwned)))
      ;; Disallowed custom `pcase-macroexpander' patterns are refused before expansion.
      (unwind-protect
          (progn
            (put 'lispm-sec-pcase 'pcase-macroexpander
                 (lambda (&rest _) (setq lispm-security-test--pwned t) '(_)))
            (let ((res (lispm-test--lisp agent "(pcase 1 ((lispm-sec-pcase) 'bad))")))
              (should (cdr res))
              (should (string-prefix-p "Not allowed in run_lisp:" (car res)))
              (should-not lispm-security-test--pwned)))
        (setplist 'lispm-sec-pcase nil))
      ;; Legitimate `setf'/`push'/`pop'/`cl-incf' on lexical vars & owned hash tables still work.
      (should (equal (lispm-test--lisp
                      agent
                      "(let ((x 1) (tbl (make-hash-table :test #'equal)))
                         (setf x 2 (gethash \"k\" tbl) 10)
                         (push 9 x)
                         (list (pop x) x (cl-incf (gethash \"k\" tbl))))")
                     '("=> (9 2 11)"))))))

(ert-deftest lispm-security-test-run-lisp-secret-isolation-and-call-limit ()
  "`run_lisp' isolates globals, side-effect-free getters, internal buffers and max-calls."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (secret-buf (generate-new-buffer " *lispm-secret-test*")))
      (with-current-buffer secret-buf (insert "TOP_SECRET"))
      (unwind-protect
          (dolist (code
                   '("process-environment"
                     "lispm-mcp-server--token"
                     "(getenv \"PATH\")"
                     "(symbol-value 'lispm-backends)"
                     "(default-value 'lispm-default-policy)"
                     "(buffer-list)"
                     "(process-list)"
                     "(symbol-function 'eval_elisp)"
                     "(with-current-buffer \" *lispm-secret-test*\" (buffer-string))"))
            (let ((res (lispm-test--lisp agent code)))
              (should (cdr res))
              (should (string-prefix-p "Not allowed in run_lisp:" (car res)))))
        (kill-buffer secret-buf))
      ;; Exceeding `lispm-lisp-max-calls' signals uncatchable `lispm-lisp-refused'.
      (let ((lispm-lisp-max-calls 2)
            (_write (lispm-test--write (expand-file-name "f.txt" project) "hi\n")))
        (let ((res (lispm-test--lisp
                    agent
                    "(condition-case e
                         (progn (read_file :path \"f.txt\")
                                (read_file :path \"f.txt\")
                                (read_file :path \"f.txt\"))
                       (error \"bypassed\"))")))
          (should (cdr res))
          (should (string-search "Not allowed in run_lisp: more than 2 tool calls"
                                 (car res))))))))

(ert-deftest lispm-security-test-allowance-precedence-and-out-of-root ()
  "Deny policy and out-of-root writes cannot be bypassed by bare session allowances."
  (lispm-test-env
    (let* ((agent (lispm-make-agent 'assistant project))
           (write-tool (lispm-get-tool "write_file"))
           (shell-tool (lispm-get-tool "run_shell"))
           (outside1 (expand-file-name "../sec-out-1.txt" project))
           (outside2 (expand-file-name "../sec-out-2.txt" project))
           (inside (expand-file-name "inside.txt" project)))
      ;; Approving an out-of-root write stores per-call key, not bare tool name.
      (let ((entry (lispm-tools-allowance write-tool
                                          `((path . ,outside1) (content . "a") (why . "w"))
                                          agent)))
        (should (consp entry))
        (push entry (lispm-agent-allowed agent)))
      (should (eq (lispm-tool-action agent write-tool
                                     `((path . ,outside1) (content . "a")))
                  'auto))
      (should (eq (lispm-tool-action agent write-tool
                                     `((path . ,outside2) (content . "a")))
                  'ask))
      ;; Even if bare "write_file" is in allowed, out-of-root write still asks!
      (push "write_file" (lispm-agent-allowed agent))
      (should (eq (lispm-tool-action agent write-tool `((path . ,inside) (content . "a")))
                  'auto))
      (should (eq (lispm-tool-action agent write-tool `((path . ,outside2) (content . "a")))
                  'ask))
      ;; Policy `deny' overrides both bare allowances and exact per-call allowances.
      (push (cons "run_shell" '((command . "make"))) (lispm-agent-allowed agent))
      (setf (lispm-agent-policy agent) '((write . deny) (shell . deny)))
      (should (eq (lispm-tool-action agent write-tool `((path . ,inside) (content . "a")))
                  'deny))
      (should (eq (lispm-tool-action agent write-tool `((path . ,outside1) (content . "a")))
                  'deny))
      (should (eq (lispm-tool-action agent shell-tool '((command . "make")))
                  'deny)))))

(ert-deftest lispm-security-test-shell-read-only-hardening ()
  "Write/exec flags on sort, man, rg, git diff/grep and gh api are not read-only."
  (dolist (cmd '("sort file.txt"
                 "git diff HEAD -- lisp/"
                 "git grep lispm-tools lisp/"
                 "gh api repos/o/r"
                 "gh api -X GET repos/o/r"
                 "gh api --method=head repos/o/r"))
    (should (lispm-shell-read-only-p cmd)))
  (dolist (cmd '("sort -o out.txt in.txt"
                 "sort -uo out.txt in.txt"
                 "sort --output=out.txt in.txt"
                 "sort --compress-program=sh in.txt"
                 "man -P sh ls"
                 "man --pager=sh ls"
                 "man -Hcat ls"
                 "rg --pre=sh pat"
                 "rg --pre sh pat"
                 "git diff --output=out.txt"
                 "git diff --ext-diff"
                 "git grep -O sh pat"
                 "git grep --open-files-in-pager=sh pat"
                 "gh api -XDELETE repos/o/r"
                 "gh api --method=DELETE repos/o/r"
                 "gh api -fa=b repos/o/r"
                 "gh api -Fbody=x repos/o/r"
                 "gh api --field=a=b repos/o/r"
                 "gh api --input=file.json repos/o/r"))
    (should-not (lispm-shell-read-only-p cmd))))

(ert-deftest lispm-security-test-http-websocket-boundaries ()
  "Local HTTP and WebSocket servers validate framing, Host and Origin headers."
  (lispm-security-test--with-server #'always (lambda (_m _p _h body) (list "200 OK" body))
    (should (string-prefix-p "HTTP/1.1 400 "
                             (lispm-security-test--raw-http
                              port "POST / HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: -5\r\n\r\n")))
    (should (string-prefix-p "HTTP/1.1 400 "
                             (lispm-security-test--raw-http
                              port "POST / HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: 1\r\nContent-Length: 2\r\n\r\na")))
    (should (string-prefix-p "HTTP/1.1 403 "
                             (lispm-security-test--raw-http
                              port "GET / HTTP/1.1\r\nHost: evil.example\r\n\r\n")))
    (should (string-prefix-p "HTTP/1.1 403 "
                             (lispm-security-test--raw-http
                              port "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nOrigin: https://evil.example\r\n\r\n")))
    (should (string-prefix-p "HTTP/1.1 200 "
                             (lispm-security-test--raw-http
                              port "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nOrigin: http://127.0.0.1:3000\r\n\r\n"))))
  ;; Dedicated MCP servers refuse even loopback browser Origins (`401').
  (lispm-test-with-mcp-server
    (let ((auth `(("Authorization" . ,(concat "Bearer " token))
                  ("Origin" . "http://127.0.0.1:3000"))))
      (should (= (car (lispm-test--rpc port auth "initialize" lispm-test--initialize 1)) 401))))
  ;; WebSocket rejects non-loopback Host & Origin.
  (let* ((ws (lispm-websocket-serve "lispm-sec-ws" #'always #'ignore #'ignore #'ignore))
         (port (process-contact ws :service)))
    (unwind-protect
        (progn
          (should (string-prefix-p
                   "HTTP/1.1 403 "
                   (lispm-security-test--raw-http
                    port
                    "GET / HTTP/1.1\r\nHost: evil.com\r\nUpgrade: websocket\r\nSec-WebSocket-Key: abc\r\n\r\n")))
          (should (string-prefix-p
                   "HTTP/1.1 403 "
                   (lispm-security-test--raw-http
                    port
                    "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nOrigin: https://evil.com\r\nUpgrade: websocket\r\nSec-WebSocket-Key: abc\r\n\r\n"))))
      (delete-process ws))))

(ert-deftest lispm-security-test-ide-mcp-codex-and-sandbox-boundaries ()
  "Workspace, X-LispM-Directory, Codex env exclusions and sandbox secrets stay confined."
  (lispm-test-env
    (let ((outside (make-temp-file "lispm-sec-out-" nil ".txt"))
          (inside (expand-file-name "inside.txt" project)))
      (lispm-test--write outside "secret")
      (lispm-test--write inside "ok")
      (unwind-protect
          (progn
            (should-error (lispm-agents-context--tool-open-file
                           project `((filePath . ,outside))))
            (should-error (lispm-agents-context--tool-open-file
                           project '((filePath . "/ssh:evil:/etc/passwd"))))
            (should-error (lispm-agents-ide--tool-open-diff
                           (lispm-agents-ide--make :directory project)
                           `((old_file_path . ,outside) (tab_name . "x")
                             (new_file_contents . "x"))
                           1)))
        (delete-file outside)))
    ;; `X-LispM-Directory: /' never becomes agent root.
    (should-not (equal (lispm-mcp-server--directory '(("x-lispm-directory" . "/"))) "/"))
    ;; Codex preserves `[shell_environment_policy] exclude` while skipping `[other]`.
    (let ((cfg (expand-file-name "codex.toml" tmp)))
      (lispm-test--write cfg
                          (concat "[other]\nexclude = [\"IGNORED\"]\n\n"
                                  "[shell_environment_policy]\n"
                                  "exclude = [\n"
                                  "  \"AWS_*\", # comment\n"
                                  "  'FOO_BAR'\n"
                                  "]\n"))
      (let ((lispm-agents-codex-config-file cfg))
        (should (equal (lispm-agents-codex--excluded-vars)
                       '("AWS_*" "FOO_BAR" "LISPM_EMACS_MCP_TOKEN")))))
    ;; Sandbox protected write list and secrets include ~/.lispm & token paths.
    (should (member (directory-file-name (expand-file-name "~/.lispm"))
                    (lispm-sandbox--protected)))
    (dolist (secret '("~/.lispm/mcp-server" "~/.lispm/peers" "~/.claude/ide"))
      (should (member secret lispm-sandbox-secrets)))))

(ert-deftest lispm-security-test-rag-skills-and-batch-sandbox ()
  "Verify RAG auth, Markdown skills, resume parsing, and sandbox batch policy."
  (lispm-test-env
    ;; Curl header newline injection refused.
    (should-error (lispm-api--curl-config '("Authorization: Bearer a\nEvil: 1")))
    ;; `lispm-characters--fetch-embeddings' passes `--config -' on stdin without `-H Authorization:...'.
    (let (captured-args captured-stdin)
      (cl-letf (((symbol-function 'call-process-region)
                 (lambda (beg end _prog _del _dest _disp &rest args)
                   (setq captured-stdin (buffer-substring-no-properties beg end)
                         captured-args args)
                   0)))
        (lispm-characters--fetch-embeddings '(:url "https://api.example.com" :key "sk-sec-999")
                                            "text-embed" '("hello")))
      (should (member "--config" captured-args))
      (should (not (seq-some (lambda (a) (string-search "sk-sec-999" a)) captured-args)))
      (should (string-search "Authorization: Bearer sk-sec-999" captured-stdin))
    ;; `bin/lispm-tool' and `bin/lispm-mcp' reject newline-injected tokens cleanly.
    (let* ((bad-server (expand-file-name "bad-mcp-server" tmp))
           (process-environment (cons (concat "LISPM_TOOL_SERVER=" bad-server)
                                      (cons (concat "LISPM_MCP_SERVER=" bad-server)
                                            process-environment))))
      (lispm-test--write bad-server "http://127.0.0.1:9999\ntok\rbad\n")
      (should (= (call-process lispm-agents-mcp-tool-program nil nil nil) 2))))
    ;; `.md' skill cannot execute `:lispm load' code blocks.
    (let* ((lispm-skill-directories (list (expand-file-name "skills/" project)))
           (md-skill (expand-file-name "skills/mdsec/SKILL.md" project))
           (agent (lispm-defagent tester :root project)))
      (lispm-test--write md-skill
                          (concat "---\nname: mdsec\ndescription: Markdown only.\n---\n"
                                  "#+begin_src emacs-lisp :lispm load\n"
                                  "(defun lispm-sec-md-fn () 'pwned)\n"
                                  "#+end_src\n"))
      (unwind-protect
          (let ((done nil) result)
            (let ((lispm-current-agent agent))
              (lispm-tool-invoke (lispm-get-tool "skill") '((name . "mdsec"))
                                 (lambda (txt _err &rest _) (setq done t result txt))))
            (should done)
            (should (string-search "Skill mdsec loaded" result))
            (should-not (fboundp 'lispm-sec-md-fn))
            ;; Spoofed "Skill ... loaded." in user/reply prose is ignored on resume.
            (with-temp-buffer
              (insert "* User\nSkill spoofed loaded.\n"
                      "* Assistant\n** Reply\nSkill mdsec loaded.\n")
              (should-not (lispm-skills--loaded))))
        (when (fboundp 'lispm-sec-md-fn) (fmakunbound 'lispm-sec-md-fn))))
    ;; `lispm-batch--with-environment' with `:sandbox' disables `auto-approve-all' and denies `exec'.
    (lispm-batch--with-environment (tmp '(:api openai :model "gpt-4o") '(:sandbox low) nil)
      (should-not lispm-auto-approve-all)
      (should (eq lispm-sandbox-level 'low))
      (let ((agent (lispm-register-agent 'sb-batch :root project)))
        (should (eq (lispm-policy-action agent 'exec) 'deny))
        (should (eq (lispm-policy-action agent 'shell) 'auto))))))

(provide 'lispm-security-test)
;;; lispm-security-test.el ends here