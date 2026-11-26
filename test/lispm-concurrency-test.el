;;; lispm-concurrency-test.el --- Tests for lock-free concurrency and async hot paths  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests verifying declarative intra-turn tool concurrency scheduling,
;; per-call stage isolation, scoped edit gating, non-blocking async hot paths,
;; visible-only chat redraws, and incremental roster child scanning.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm)
(require 'lispm-test)

(ert-deftest lispm-concurrency-test-parallel-intra-turn-out-of-order ()
  "Parallel tools in one turn launch concurrently and settle out-of-order."
  (lispm-test-env
    (let ((lispm-tools--registry (copy-hash-table lispm-tools--registry))
          started completions)
      (lispm-deftool test_par_a ((x string "Input A."))
        "First async parallel test tool."
        :permission read
        :async t
        :concurrency 'parallel
        (push (cons 'a x) started)
        (push (cons 'a done) completions)
        nil)
      (lispm-deftool test_par_b ((x string "Input B."))
        "Second async parallel test tool."
        :permission read
        :async t
        :concurrency 'parallel
        (push (cons 'b x) started)
        (push (cons 'b done) completions)
        nil)
      (let ((agent (lispm-defagent worker :root project :policy '((read . auto)))))
        (setq lispm-test--scripts
              `((worker ,(lispm-test--msg "tool_use"
                                          (lispm-test--tool "p1" "test_par_a" '((x . "1")))
                                          (lispm-test--tool "p2" "test_par_b" '((x . "2"))))
                        ,(lispm-test--msg "end_turn" (lispm-test--text "Both done.")))))
        (lispm-tell agent "go")
        ;; Both parallel tools must start before either finishes.
        (should (lispm-test--wait (lambda () (= (length started) 2))))
        (should (equal (sort (mapcar #'car started) (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                       '(a b)))
        (should (= (hash-table-count (lispm-agent-pending agent)) 2))
        ;; Complete tool B first (out of emission order), then tool A.
        (funcall (cdr (assq 'b completions)) "res-b" nil)
        (should (lispm-test--wait (lambda () (= (hash-table-count (lispm-agent-pending agent)) 1))))
        (should (eq (lispm-agent-state agent) 'acting))
        (funcall (cdr (assq 'a completions)) "res-a" nil)
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        (let ((tr (lispm-test--transcript agent)))
          (should (string-match-p "DONE test_par_a" tr))
          (should (string-match-p "res-a" tr))
          (should (string-match-p "DONE test_par_b" tr))
          (should (string-match-p "res-b" tr))
          (should (string-match-p "Both done\\." tr)))))))

(ert-deftest lispm-concurrency-test-resource-and-editor-serialization ()
  "Disjoint resource calls overlap while same-resource and editor calls serialize FIFO."
  (lispm-test-env
    (let ((lispm-tools--registry (copy-hash-table lispm-tools--registry))
          events callbacks)
      (lispm-deftool test_res_write
          ((why string "Reason.") (path string "Target path."))
        "Async resource-scoped write tool."
        :permission write
        :async t
        :targets (lambda (_why path) (list path))
        (ignore why)
        (push (list 'start 'res path) events)
        (push (cons path done) callbacks)
        nil)
      (lispm-deftool test_editor_op ()
        "Async global editor tool."
        :permission exec
        :async t
        :concurrency 'editor
        (push 'editor-start events)
        (push (cons 'editor done) callbacks)
        nil)
      ;; Part A: same resource serializes, disjoint resource runs concurrently.
      (let ((agent (lispm-defagent coder :root project :policy '((write . auto) (exec . auto)))))
        (setq lispm-test--scripts
              `((coder ,(lispm-test--msg "tool_use"
                                         (lispm-test--tool "r1" "test_res_write"
                                                           '((why . "w1") (path . "alpha.txt")))
                                         (lispm-test--tool "r2" "test_res_write"
                                                           '((why . "w2") (path . "alpha.txt")))
                                         (lispm-test--tool "r3" "test_res_write"
                                                           '((why . "w3") (path . "beta.txt"))))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Writes finished.")))))
        (lispm-tell agent "write")
        (should (lispm-test--wait (lambda () (= (length callbacks) 2))))
        ;; r1 (alpha) and r3 (beta) launched; r2 (alpha) stays queued.
        (should (equal (mapcar #'car callbacks) '("beta.txt" "alpha.txt")))
        (let ((r1-done (cdr (assoc "alpha.txt" callbacks))))
          (setq callbacks (cl-remove "alpha.txt" callbacks :key #'car :test #'equal))
          (funcall r1-done "wrote alpha 1" nil))
        (should (lispm-test--wait (lambda () (assoc "alpha.txt" callbacks))))
        (funcall (cdr (assoc "beta.txt" callbacks)) "wrote beta" nil)
        (funcall (cdr (assoc "alpha.txt" callbacks)) "wrote alpha 2" nil)
        (should (lispm-test--wait (lispm-test--idle-p agent)))
        ;; Part B: editor blocks concurrent calls and drains in FIFO order.
        (setq events nil callbacks nil
              lispm-test--scripts
              `((coder ,(lispm-test--msg "tool_use"
                                         (lispm-test--tool "e1" "test_editor_op" nil)
                                         (lispm-test--tool "r4" "test_res_write"
                                                           '((why . "w4") (path . "gamma.txt")))
                                         (lispm-test--tool "e2" "test_editor_op" nil))
                       ,(lispm-test--msg "end_turn" (lispm-test--text "Editor finished.")))))
        (lispm-tell agent "edit")
        (should (lispm-test--wait (lambda () (= (length callbacks) 1))))
        (should (equal (caar callbacks) 'editor))
        (let ((e1-done (cdar callbacks)))
          (setq callbacks nil)
          (funcall e1-done "ed1" nil))
        (should (lispm-test--wait (lambda () (assoc "gamma.txt" callbacks))))
        (should-not (assq 'editor callbacks))
        (funcall (cdr (assoc "gamma.txt" callbacks)) "wrote gamma" nil)
        (should (lispm-test--wait (lambda () (assq 'editor callbacks))))
        (funcall (cdar callbacks) "ed2" nil)
        (should (lispm-test--wait (lispm-test--idle-p agent)))))))

(ert-deftest lispm-concurrency-test-stage-isolation-and-scoped-edits ()
  "Same-agent concurrent calls isolate stage state and scoped edit gating."
  (lispm-test-env
    (let* ((dir-a (expand-file-name "a/" project))
           (dir-b (expand-file-name "b/" project))
           (_ (progn (make-directory dir-a t) (make-directory dir-b t)))
           (agent-a (lispm-defagent alpha :root dir-a))
           (agent-b (lispm-defagent beta :root dir-b))
           (buf-a (lispm-chat agent-a))
           (buf-b (lispm-chat agent-b))
           (lispm-shell-stage--in-batch t)
           (lispm-shell-stage--sessions (make-hash-table :test #'equal))
           (lispm-eval-stage--in-batch t)
           (lispm-eval-stage--sessions (make-hash-table :test #'equal))
           (lispm-edits--in-batch t)
           (lispm-edits--shows (list (lispm-edits--show-create :agent agent-a :buffer buf-a)))
           (lispm-edits--proposals nil)
           (spec-1 (lispm-shell-stage-spec--create :tool-name "run_shell" :display "echo 1"))
           (spec-2 (lispm-shell-stage-spec--create :tool-name "run_shell" :display "echo 2")))
      ;; Scoped `lispm-edits-active-p': blocks only agent-a/buf-a, never agent-b.
      (should (lispm-edits-active-p))
      (should (lispm-edits-active-p nil agent-a))
      (should (lispm-edits-active-p buf-a nil))
      (should-not (lispm-edits-active-p nil agent-b))
      (should-not (lispm-edits-active-p buf-b nil))
      ;; Two simultaneous shell stage calls for agent-b isolate sessions and buffers.
      (let ((lispm-current-call "call-1"))
        (lispm-shell-stage--start agent-b spec-1))
      (let ((lispm-current-call "call-2"))
        (lispm-shell-stage--start agent-b spec-2))
      (should (= (hash-table-count lispm-shell-stage--sessions) 2))
      (let ((s1 (gethash "call-1" lispm-shell-stage--sessions))
            (s2 (gethash "call-2" lispm-shell-stage--sessions)))
        (should s1)
        (should s2)
        (should-not (eq (lispm-shell-stage--session-stage-buffer s1)
                        (lispm-shell-stage--session-stage-buffer s2)))
        (should-not (eq (lispm-shell-stage--session-log-buffer s1)
                        (lispm-shell-stage--session-log-buffer s2)))
        (let ((lispm-current-call "call-1"))
          (lispm-shell-stage--finish agent-b spec-1 "1\n[exit status 0]" nil))
        (should (= (hash-table-count lispm-shell-stage--sessions) 1))
        (should (gethash "call-2" lispm-shell-stage--sessions))
        (let ((lispm-current-call "call-2"))
          (lispm-shell-stage--finish agent-b spec-2 "2\n[exit status 0]" nil))
        (should (= (hash-table-count lispm-shell-stage--sessions) 0)))
      ;; Two simultaneous eval stage calls for agent-b isolate sessions and buffers.
      (let ((lispm-current-call "eval-1"))
        (lispm-eval-stage--start agent-b 'lisp "eval_elisp" '((code . "(+ 1 1)"))))
      (let ((lispm-current-call "eval-2"))
        (lispm-eval-stage--start agent-b 'lisp "eval_elisp" '((code . "(+ 2 2)"))))
      (should (= (hash-table-count lispm-eval-stage--sessions) 2))
      (let ((e1 (gethash "eval-1" lispm-eval-stage--sessions))
            (e2 (gethash "eval-2" lispm-eval-stage--sessions)))
        (should-not (eq (lispm-eval-stage--session-buffer e1)
                        (lispm-eval-stage--session-buffer e2)))
        (let ((lispm-current-call "eval-1"))
          (lispm-eval-stage--finish agent-b 'lisp "eval_elisp" '((code . "(+ 1 1)")) "2" nil))
        (should (= (hash-table-count lispm-eval-stage--sessions) 1))
        (let ((lispm-current-call "eval-2"))
          (lispm-eval-stage--finish agent-b 'lisp "eval_elisp" '((code . "(+ 2 2)")) "4" nil))
        (should (= (hash-table-count lispm-eval-stage--sessions) 0))))))

(ert-deftest lispm-concurrency-test-async-rag-media-journal-sandbox ()
  "RAG, media poster, journal begin, sandbox mkfifo and bridge avoid sync subprocesses."
  (lispm-test-env
    ;; 1. RAG async embedding + cached context never calls synchronous curl.
    (let* ((char (lispm-create-character "Lyra" :description "Helper."
                                         :rag t :embedding-model "embed-1"))
           (_ (lispm-character-rag-add char "Channels" "Async channel pipelines scale lock-free."))
           (lispm-characters--rag-query-cache (make-hash-table :test #'equal))
           (lispm-characters--rag-warming (make-hash-table :test #'equal))
           (agent (lispm-character-agent char project))
           (rag-res nil)
           (async-calls nil))
      (with-current-buffer (lispm-session-buffer agent)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "* User\nchannels\n")))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args) (error "Unexpected sync call-process: %S" args)))
                ((symbol-function 'call-process-region)
                 (lambda (&rest args) (error "Unexpected sync call-process-region: %S" args)))
                ((symbol-function 'lispm-characters-last-input)
                 (lambda (_a) (cons 1 "channels")))
                (lispm-exec-function
                 (lambda (program _args cb &rest _)
                   (push program async-calls)
                   (run-at-time 0 nil cb 0
                                "{\"data\":[{\"index\":0,\"embedding\":[1.0,0.0]},{\"index\":1,\"embedding\":[1.0,0.0]}]}"
                                ""))))
        ;; Context assembly runs non-blocking without call-process-region.
        (should (string-match-p "Async channel" (or (lispm-characters--rag-context agent) "")))
        ;; Clear warming state and test async fetch-embeddings-async + rag_search.
        (clrhash lispm-characters--rag-warming)
        (let ((lispm-current-agent agent))
          (lispm-tool-invoke (lispm-get-tool "rag_search") '((query . "channels"))
                             (lambda (text err &rest _) (setq rag-res (cons text err)))))
        (should (lispm-test--wait (lambda () rag-res)))
        (should-not (cdr rag-res))
        (should (string-match-p "Channels" (car rag-res)))
        (should async-calls)))
    ;; 2. Video chat render checks poster cache without ffprobe/ffmpeg sync subprocesses.
    (let* ((vid (expand-file-name "clip.mp4" project))
           (ready-file nil)
           (hook-fn (lambda (f) (setq ready-file f))))
      (with-temp-file vid (insert "fake-video"))
      (add-hook 'lispm-chat-media-ready-functions hook-fn)
      (unwind-protect
          (let ((lispm-video--info-cache (make-hash-table :test #'equal))
                (lispm-video--pending (make-hash-table :test #'equal)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest args) (error "Unexpected sync call-process: %S" args)))
                      ((symbol-function 'display-images-p) (lambda () t))
                      ((symbol-function 'executable-find)
                       (lambda (prog &rest _) (when (member prog '("ffprobe" "ffmpeg")) prog)))
                      (lispm-exec-function
                       (lambda (prog args cb &rest _)
                         (run-at-time
                          0 nil
                          (lambda ()
                            (cond
                             ((equal prog "ffprobe")
                              (funcall cb 0 "width=320\nheight=240\nduration=4.0\n" ""))
                             ((equal prog "ffmpeg")
                              (let ((out (car (last args))))
                                (make-directory (file-name-directory out) t)
                                (with-temp-file out (insert "jpg"))
                                (funcall cb 0 "" "")))))))))
              (let ((rendered (lispm-chat--image vid)))
                (should (string-match-p "\\[video " rendered)))
              (should (lispm-test--wait (lambda () (equal ready-file vid))))
              (should (file-exists-p (lispm-video-poster-cached vid)))))
        (remove-hook 'lispm-chat-media-ready-functions hook-fn)))
    ;; 3. Sandbox async mkfifo and pre-start cancellation skip sync call-process.
    (let* ((agent (lispm-defagent sbox :root project))
           (lispm-sandbox-level 'low)
           (log-buf (generate-new-buffer " *lispm-conc-sbox-log*"))
           (pending-cb nil)
           (shell-res nil)
           (cancelled-p nil))
      (unwind-protect
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest args) (error "Unexpected sync call-process: %S" args)))
                    ((symbol-function 'lispm-sandbox--ready) (lambda () '("true")))
                    (lispm-exec-function
                     (lambda (prog args cb &rest _)
                       (when (equal prog "mkfifo")
                         (setq pending-cb (list (car (last args)) cb))))))
            (lispm-tools--shell-run-locally
             agent "echo hi" 10 log-buf (lambda (text err) (setq shell-res (list text err))))
            (should pending-cb)
            (lispm-job-cancel-owner agent)
            (setq cancelled-p t)
            ;; Late mkfifo completion after cancellation must NOT invoke done or spawn shell.
            (funcall (cadr pending-cb) 0 "" "")
            (should cancelled-p)
            (should-not shell-res)
            (should-not (file-exists-p (car pending-cb))))
        (when (buffer-live-p log-buf) (kill-buffer log-buf))))
    ;; 4. Journal begin uses zero sync call-process calls.
    (let ((git-repo (lispm-test--git-repo (expand-file-name "jrepo/" project))))
      (let* ((jagent (lispm-defagent jrnl :root git-repo))
             (exec-seen 0))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest args) (error "Unexpected sync call-process: %S" args)))
                  (lispm-exec-function
                   (lambda (_prog _args cb &rest _)
                     (cl-incf exec-seen)
                     (run-at-time 0 nil cb 0 "" ""))))
          (lispm-journal-begin jagent)
          (should (lispm-test--wait (lambda () (not (lispm-journal-busy-p jagent)))))
          (should (> exec-seen 0)))))
    ;; 5. Presentation index & bridge async detail avoid sync subprocesses.
    (let ((git-repo (lispm-test--git-repo (expand-file-name "prepo/" project))))
      (with-temp-file (expand-file-name "hello.el" git-repo) (insert ";; hi\n"))
      (clrhash lispm-present--project-cache)
      (clrhash lispm-present--project-refreshing)
      (let ((calls 0))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest args) (error "Unexpected sync call-process: %S" args)))
                  (lispm-exec-function
                   (lambda (_prog _args cb &rest _)
                     (cl-incf calls)
                     (run-at-time 0 nil cb 0 "hello.el\0" "")))
                  (noninteractive nil))
          (should (= (hash-table-count (lispm-present--project-index git-repo)) 0))
          (should (= calls 1))
          (should (lispm-test--wait (lambda ()
                                      (> (hash-table-count (lispm-present--project-index git-repo)) 0))))))
      (let* ((b-agent (lispm-defagent bridge :root git-repo))
             (tool (lispm-get-tool "read_file"))
             (detail nil))
        (lispm-agents-bridge--detail-async
         b-agent tool '((path . "hello.el")) '("Needs permission")
         (lambda (text) (setq detail text)))
        (should (string-match-p "Needs permission" detail))))))

(ert-deftest lispm-concurrency-test-hidden-chat-skips-redraw-and-refreshes-on-visible ()
  "Hidden busy chats incur zero spinner/schedule redraws until displayed."
  (lispm-test-env
    (let* ((agent-a (lispm-defagent alpha :root project))
           (agent-b (lispm-defagent beta :root project))
           (chat-a (lispm-chat agent-a))
           (chat-b (lispm-chat agent-b))
           (lispm-chat-defer-hidden-redraws t)
           (redraws nil)
           (saved-buf (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) chat-b)
            (lispm-set-state agent-a 'acting "thinking")
            (lispm-set-state agent-b 'acting "thinking")
            (should (equal (lispm-chat--busy-agents) (list agent-b)))
            (with-current-buffer chat-a (setq lispm-chat--needs-redraw nil))
            (cl-letf (((symbol-function 'lispm-chat-redraw-now)
                       (lambda () (push (current-buffer) redraws))))
              ;; Trigger spinner and scheduled updates while chat-a is hidden.
              (lispm-chat-schedule agent-a)
              (lispm-chat-schedule agent-b)
              (should (buffer-local-value 'lispm-chat--needs-redraw chat-a))
              (should (lispm-test--wait (lambda () (memq chat-b redraws))))
              (should-not (memq chat-a redraws))
              ;; Showing chat-a triggers window-buffer-change refresh immediately.
              (set-window-buffer (selected-window) chat-a)
              (lispm-chat--on-window-change (selected-window))
              (should (memq chat-a redraws))
              (should-not (buffer-local-value 'lispm-chat--needs-redraw chat-a))))
        (when (buffer-live-p saved-buf)
          (set-window-buffer (selected-window) saved-buf))
        (when (timerp lispm-chat--spinner-timer)
          (cancel-timer lispm-chat--spinner-timer)
          (setq lispm-chat--spinner-timer nil))))))

(ert-deftest lispm-concurrency-test-incremental-roster-child-scan ()
  "Roster retired-children scanning is O(1) on unchanged buffers and incremental on growth."
  (lispm-test-env
    (let* ((agent (lispm-defagent parent :root project))
           (_ (lispm-chat agent))
           (tbuf (lispm-agent-buffer agent))
           (searches 0)
           (orig (symbol-function 're-search-forward)))
      (with-current-buffer tbuf
        (goto-char (point-max))
        (insert "accepted by worker-1 (20261126-120000-abcdef12)\n"))
      (cl-letf (((symbol-function 're-search-forward)
                 (lambda (&rest args)
                   (cl-incf searches)
                   (apply orig args))))
        (let* ((idx (lispm-agents-roster--build-index (lispm-agents-live-buffers)))
               (first (lispm-agents-roster--retired-children idx)))
          (should (= (length first) 1))
          (should (equal (plist-get (car first) :name) "worker-1"))
          (should (> searches 0))
          (let ((before-unchanged searches)
                (second (lispm-agents-roster--retired-children idx)))
            (should (= (length second) 1))
            ;; Zero regex scans when transcript did not change.
            (should (= searches before-unchanged)))
          ;; Append filler and a second child record; only tail is scanned incrementally.
          (with-current-buffer tbuf
            (goto-char (point-max))
            (insert (make-string 512 ?x) "\n"
                    "accepted by worker-2 (20261126-120001-12345678)\n"))
          (let ((third (lispm-agents-roster--retired-children idx)))
            (should (= (length third) 2))
            (should (member "worker-2" (mapcar (lambda (r) (plist-get r :name)) third)))))))))

(provide 'lispm-concurrency-test)
;;; lispm-concurrency-test.el ends here
