;;; lispm-control-plane-test.el --- Tests for Phase 3 control protocol & Effect Host -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-test)
(require 'lispm-control-protocol)
(require 'lispm-control-plane)

(defun lispm-control-plane-test--pump-until (pred &optional timeout-sec)
  "Accept process/timer events until PRED returns non-nil, up to TIMEOUT-SEC."
  (let ((deadline (+ (float-time) (or timeout-sec 3))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall pred)))

(ert-deftest lispm-control-protocol-wire-safety-and-frames ()
  "Verify typed RPC frame construction, wire safety, and parsing."
  (should (lispm-control-protocol-wire-safe-p '(agent-start "s1" (:dialect :anthropic :max-steps 8))))
  (should-not (lispm-control-protocol-wire-safe-p '(agent-start "s1" (:bad 1.5))))
  (should-not (lispm-control-protocol-wire-safe-p '(agent-start "s1" (:dotted . "pair"))))
  (should-error (lispm-control-protocol-agent-start "s1" '(:bad-float 1.2))
                :type 'lispm-control-protocol-error)
  (let ((parsed (lispm-control-protocol-parse-frame
                 '(editor-effect-request "s1" 7 :edit-buffer
                                         (:call-id "c1" :tool "edit_buffer" :input-json "{}")
                                         :timeout 15))))
    (should (eq (plist-get parsed :frame) 'editor-effect-request))
    (should (equal (plist-get parsed :session) "s1"))
    (should (= (plist-get parsed :effect-id) 7))
    (should (eq (plist-get parsed :kind) :edit-buffer))
    (should (= (plist-get parsed :timeout) 15)))
  (should-error (lispm-control-protocol-parse-frame '(bogus-frame 1 2))
                :type 'lispm-control-protocol-error))

(ert-deftest lispm-control-protocol-decode-llm-result-parity ()
  "Verify kernel `:llm-stream' results adapt into canonical Messages shape."
  (let* ((res (list :ok t :status 200 :stop-reason "tool_use"
                    :usage (list :input-tokens 40 :output-tokens 12
                                 :cache-read-input-tokens 5)
                    :invalid '("bad_1")
                    :blocks (list (list :type :thinking :thinking "ponder" :ephemeral t)
                                  (list :type :text :text "Checking ")
                                  (list :type :tool-use :id "tu_1" :name "eval_elisp"
                                        :input-json "{\"code\":\"(+ 1 2)\"}")
                                  (list :type :tool-use :id "bad_2" :name "read_file"
                                        :input-json "{corrupt"))))
         (decoded (lispm-control-protocol-decode-llm-result res))
         (msg (plist-get decoded :message))
         (content (alist-get 'content msg)))
    (should (eq t (plist-get decoded :ok)))
    (should (equal "tool_use" (alist-get 'stop_reason msg)))
    (should (= 40 (alist-get 'input_tokens (alist-get 'usage msg))))
    (should (= 12 (alist-get 'output_tokens (alist-get 'usage msg))))
    (should (= 5 (alist-get 'cache_read_input_tokens (alist-get 'usage msg))))
    (should (member "bad_1" (alist-get 'lispm-invalid msg)))
    (should (member "bad_2" (alist-get 'lispm-invalid msg)))
    (should (= 4 (length content)))
    (should (eq t (alist-get 'ephemeral (aref content 0))))
    (should (equal "(+ 1 2)" (alist-get 'code (alist-get 'input (aref content 2))))))
  (let ((err (lispm-control-protocol-decode-llm-result
              (list :ok nil :status 429 :type "rate_limit_error" :message "slow"))))
    (should-not (plist-get err :ok))
    (should (equal '(:status 429 :type "rate_limit_error" :message "slow")
                   (plist-get err :error)))))

(ert-deftest lispm-control-protocol-tool-host-and-coalescer ()
  "Verify editor vs kernel host boundary and pure delta coalescing."
  (dolist (ed '("read_buffer" "list_buffers" "edit_buffer" "edit_file" "write_file" "eval_elisp"))
    (should (eq 'editor (lispm-control-protocol-tool-host ed))))
  (dolist (kn '("run_shell" "search_files" "workspace_cmd" "read_file"))
    (should (eq 'kernel (lispm-control-protocol-tool-host kn))))
  ;; TRAMP / unsaved buffer forces `read_file' to Emacs editor host.
  (should (eq 'editor (lispm-control-protocol-tool-host
                       "read_file" '(:path "/ssh:box:/tmp/a.txt"))))
  (should (eq 'editor (lispm-control-protocol-tool-host
                       "read_file" '(:unsaved-buffer-p t))))
  (let ((coal (lispm-control-coalescer-create 10)))
    (should-not (lispm-control-coalescer-push coal 'text "hel"))
    (should-not (lispm-control-coalescer-push coal 'text "lo"))
    (should (equal '((text . "hello") (tool . "read_buffer"))
                   (lispm-control-coalescer-push coal 'tool "read_buffer")))))

(ert-deftest lispm-control-plane-editor-effects-roundtrip ()
  "Verify `read_buffer', `edit_buffer', and `eval_elisp' execute cleanly on host."
  (lispm-test-env
    (unwind-protect
        (let* ((lispm-control-plane-mode 'kernel)
               (sent nil)
               (lispm-control-plane-send-function (lambda (_rt form) (push form sent)))
               (buf (get-buffer-create " *cp-test-buf*"))
               (agent (lispm-defagent tester :root project)))
          (with-current-buffer buf
            (erase-buffer)
            (insert "line-one\nline-two\n"))
          (unwind-protect
              (progn
                (lispm-control-plane-register-agent agent :dialect :anthropic)
                ;; 1. read_buffer
                (lispm-control-plane-handle-frame
                 nil
                 `(editor-effect-request
                   ,(lispm-agent-id agent) 101 :read-buffer
                   (:call-id "c1" :tool "read_buffer"
                             :input-json ,(json-serialize '((buffer . " *cp-test-buf*"))))
                   :timeout 10))
                (let ((r1 (car sent)))
                  (should (eq (car r1) 'agent-effect-reply))
                  (should (= (nth 2 r1) 101))
                  (should (eq (nth 3 r1) :ok))
                  (should (string-search "line-two" (nth 4 r1))))
                ;; 2. edit_buffer
                (lispm-control-plane-handle-frame
                 nil
                 `(editor-effect-request
                   ,(lispm-agent-id agent) 102 :edit-buffer
                   (:call-id "c2" :tool "edit_buffer"
                             :input-json ,(json-serialize '((buffer . " *cp-test-buf*")
                                                            (old_text . "line-two")
                                                            (new_text . "line-TWO"))))
                   :timeout 10))
                (let ((r2 (car sent)))
                  (should (eq (nth 3 r2) :ok))
                  (should (string-search "line-TWO" (with-current-buffer buf (buffer-string)))))
                ;; 3. eval_elisp
                (lispm-control-plane-handle-frame
                 nil
                 `(editor-effect-request
                   ,(lispm-agent-id agent) 103 :eval-elisp
                   (:call-id "c3" :tool "eval_elisp"
                             :input-json ,(json-serialize '((code . "(+ 20 22)"))))
                   :timeout 10))
                (let ((r3 (car sent)))
                  (should (eq (nth 3 r3) :ok))
                  (should (string-search "42" (nth 4 r3)))))
            (kill-buffer buf)))
      (lispm-control-plane-reset))))

(ert-deftest lispm-control-plane-effect-cancel-timeout-and-approvals ()
  "Verify effect cancellation, timeout, approval round-trip, and viewport dedup."
  (lispm-test-env
    (unwind-protect
        (let* ((lispm-control-plane-mode 'kernel)
               (sent nil)
               (lispm-control-plane-send-function (lambda (_rt form) (push form sent)))
               (asked nil)
               (lispm-control-plane-approval-function
                (lambda (_agent call-id tool _input reasons _sess)
                  (push (list call-id tool reasons) asked)))
               (agent (lispm-defagent tester :root project)))
          (lispm-control-plane-register-agent agent :dialect :anthropic)
          ;; Approval round-trip via `lispm-control-plane-resolve-approval' and `lispm-resolve'
          (lispm-control-plane-handle-frame
           nil
           `(approval-request ,(lispm-agent-id agent) "call_ap" "edit_buffer" "{}" ("needs ok")))
          (should (equal '("call_ap" "edit_buffer" ("needs ok")) (car asked)))
          (lispm-control-plane-resolve-approval agent "call_ap" 'approve)
          (should (equal `(agent-approval-decision ,(lispm-agent-id agent) "call_ap" :approve)
                         (car sent)))
          ;; Async effect cancelled by `lispm-control-plane-cancel'
          (lispm-control-plane-handle-frame
           nil
           `(editor-effect-request
             ,(lispm-agent-id agent) 201 :eval-elisp
             (:call-id "c_async" :tool "eval_elisp"
                       :input-json ,(json-serialize
                                     '((code . "(lispm-async (lambda (_res) nil))"))))
             :timeout 5))
          (lispm-control-plane-cancel agent)
          (should (cl-some (lambda (f)
                             (and (eq (car f) 'agent-effect-reply)
                                  (= (nth 2 f) 201)
                                  (eq (nth 3 f) :cancelled)))
                           sent))
          (should (eq (caar sent) 'agent-cancel))
          ;; Async effect timing out on host
          (setq sent nil)
          (lispm-control-plane-handle-frame
           nil
           `(editor-effect-request
             ,(lispm-agent-id agent) 202 :eval-elisp
             (:call-id "c_to" :tool "eval_elisp"
                       :input-json ,(json-serialize
                                     '((code . "(lispm-async (lambda (_res) nil))"))))
             :timeout 0.08))
          (should (lispm-control-plane-test--pump-until
                   (lambda ()
                     (cl-some (lambda (f)
                                (and (eq (car f) 'agent-effect-reply)
                                     (= (nth 2 f) 202)
                                     (eq (nth 3 f) :error)))
                              sent))
                   2))
          ;; Viewport subscription deduplicates identical states
          (setq sent nil)
          (should-not (lispm-control-plane-set-viewport agent t))
          (should (lispm-control-plane-set-viewport agent nil))
          (should-not (lispm-control-plane-set-viewport agent nil))
          (should (equal `((agent-viewport-subscribe ,(lispm-agent-id agent) :hidden)) sent)))
      (lispm-control-plane-reset))))

(ert-deftest lispm-control-plane-resync-and-elisp-fallback ()
  "Verify resync restores state/stream without dupes and Elisp fallback works."
  (lispm-test-env
    (unwind-protect
        (let* ((lispm-control-plane-mode 'kernel)
               (sent nil)
               (lispm-control-plane-send-function (lambda (_rt form) (push form sent)))
               (agent (lispm-defagent tester :root project))
               (sess (lispm-control-plane-register-agent agent)))
          ;; Apply initial delta at seq 1
          (lispm-control-plane-handle-frame
           nil
           `(turn-event ,(lispm-agent-id agent)
                        (:seq 1 :turn-id "t1" :step 1 :type :turn-started)))
          (lispm-control-plane-handle-frame
           nil
           `(turn-event ,(lispm-agent-id agent)
                        (:seq 2 :turn-id "t1" :step 1 :type :delta :kind :text :text "part1")))
          (let ((done-snap nil))
            (lispm-control-plane-resync agent (lambda (_s snap) (setq done-snap snap)))
            (should (equal `(agent-resync ,(lispm-agent-id agent) :since-seq 2) (car sent)))
            ;; Simulate snapshot re-delivering seq 2 (must dedup!) plus seq 3 & seq 4
            (lispm-control-plane-handle-frame
             nil
             `(agent-snapshot
               ,(lispm-agent-id agent)
               (:session ,(lispm-agent-id agent)
                         :state :idle
                         :turn-id nil
                         :step 1
                         :event-seq 4
                         :stream (:text "part1part2" :thinking "" :tools nil)
                         :pending-approvals nil
                         :completed-tools ((:turn-id "t1" :step 1 :call-id "k1" :tool "run_shell" :result "ok"))
                         :completed-turns ((:turn-id "t1" :step 1 :status :done :text "part1part2"))
                         :events ((:seq 2 :turn-id "t1" :step 1 :type :delta :kind :text :text "part1")
                                  (:seq 3 :turn-id "t1" :step 1 :type :tool-result :call-id "k1" :tool "run_shell" :result "ok")
                                  (:seq 4 :turn-id "t1" :step 1 :type :turn-done :text "part1part2")))))
            (should done-snap)
            (should (= 1 (length (lispm-control-session-tool-results sess))))
            (should (= 1 (length (lispm-control-session-turn-results sess))))
            (should (string= "part1part2" (lispm-control-session-stream-text sess))))
          ;; Now verify pure-Elisp fallback when mode is `elisp' or kernel unavailable
          (let ((lispm-control-plane-mode 'elisp)
                (lispm-control-plane-send-function nil)
                (told nil))
            (cl-letf (((symbol-function 'lispm-tell)
                       (lambda (ag text &rest _) (setq told (list ag text)))))
              (should (eq 'elisp (lispm-control-plane-start-turn agent "hello fallback")))
              (should (equal (list agent "hello fallback") told)))))
      (lispm-control-plane-reset))))

(provide 'lispm-control-plane-test)
;;; lispm-control-plane-test.el ends here
