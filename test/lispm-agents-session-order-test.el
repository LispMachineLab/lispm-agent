;;; lispm-agents-session-order-test.el --- Session tree and opening order -*- lexical-binding: t; -*-

(require 'ert)
(require 'lispm-agents)

(ert-deftest lispm-test-session-order-survives-activity-and-restart ()
  "Opening order persists independently of replies, renames and archives."
  (lispm-test--with-agents nil
    (let ((older (list :agent 'codex :id "old" :started 10 :time 100))
          (newer (list :agent 'codex :id "new" :started 20 :time 20)))
      (should (equal (lispm-agents--sort-sessions (list older newer)) (list newer older)))
      (lispm-agents-session-opened 'codex "old" 30)
      (should (equal (lispm-agents--sort-sessions (list newer older)) (list older newer)))
      (lispm-agents--set-archived older t)
      (setq lispm-agents--opened-times nil lispm-agents--archived 'unloaded)
      (should (= (lispm-agents-session-opened-time older) 30))
      (should (lispm-agents-archived-p older))
      (setf (plist-get newer :time) 1000 (plist-get newer :title) "Renamed")
      (should (equal (lispm-agents--sort-sessions (list newer older)) (list older newer)))
      (lispm-agents-session-opened 'codex "new" 40)
      (setq lispm-agents--archived 'unloaded)
      (should (lispm-agents-archived-p older))
      (should (equal (lispm-agents--sort-sessions (list older newer)) (list newer older)))
      ;; Unknown historical creation times have stable ordering too.
      (let ((unknown (list :agent 'codex :id "unknown" :time 9999)))
        (should (equal (lispm-agents--sort-sessions (list unknown older)) (list older unknown)))))))

(ert-deftest lispm-test-session-order-keeps-freshest-transcript ()
  "Opening order does not change which continuation supplies session data."
  (lispm-test--with-agents nil
    (let ((first '(:agent codex :id "a" :started 10 :time 20 :title "Old"))
          (latest '(:agent codex :id "a" :started 10 :time 40 :title "New"))
          (other '(:agent codex :id "b" :started 30 :time 30)))
      (should (equal (lispm-agents--sort-sessions (list first latest other))
                     (list other latest))))))

(ert-deftest lispm-test-session-resume-bumps-only-on-launch ()
  "Resuming a stopped terminal bumps it; selecting a live one does not."
  (let* ((now 100.0) (live nil)
         (clock (symbol-function 'float-time))
         (buffer (generate-new-buffer "*test-agent:tmp:s*"))
         (session '(:agent test-agent :id "s" :cwd "/tmp/" :title "Work" :started 10 :time 20))
         (lispm-agents-terminal-modes '(fundamental-mode))
         (lispm-agents-terminal-live-p-function (lambda (_buffer) live))
         (lispm-agents-terminal-make-function (lambda (&rest _) (setq live t) buffer))
         (lispm-agents-show-function #'ignore)
         (lispm-agents-sessions-changed-hook nil))
    (unwind-protect
        (lispm-test--with-agents
            `((test-agent :name "Test" :program "emacs" :resume-arguments ,#'list))
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time) (if time (funcall clock time) now))))
            (lispm-agents-resume-session session)
            (should (= (lispm-agents-session-opened-time session) 100))
            (setq now 200.0)
            (lispm-agents-resume-session session)
            (should (= (lispm-agents-session-opened-time session) 100))
            ;; A dead terminal's buffer can be reused with permanent locals.
            (setq live nil now 300.0)
            (lispm-agents-resume-session session)
            (should (= (lispm-agents-session-opened-time session) 300))
            (should (= (buffer-local-value 'lispm-agents-buffer-launched buffer) 300))
            (with-current-buffer buffer
              (setq lispm-agents-buffer-session '(:id "s" :parent "old-parent" :cwd "/old/")
                    lispm-agents--claimed-at 300))
            (setq live nil now 400.0)
            (lispm-agents--launch 'test-agent "/tmp/" "test-agent:tmp:s" nil)
            (should-not (buffer-local-value 'lispm-agents-buffer-session-id buffer))
            (should-not (buffer-local-value 'lispm-agents-buffer-session buffer))
            (should-not (buffer-local-value 'lispm-agents--claimed-at buffer))))
      (kill-buffer buffer))))

(ert-deftest lispm-test-session-list-labels-use-freshest-transcript ()
  "A stale duplicate cannot overwrite the current terminal's title."
  (with-temp-buffer
    (let* ((lispm-agents-terminal-modes '(fundamental-mode))
           (lispm-agents-terminal-live-p-function #'buffer-live-p)
           (latest '(:agent test-agent :id "same" :started 10 :time 30 :title "Latest"))
           (stale '(:agent test-agent :id "same" :started 10 :time 20 :title "Stale")))
      (setq lispm-agents-buffer-agent 'test-agent lispm-agents-buffer-session-id "same")
      (lispm-test--with-agents
          `((test-agent :name "Test" :program "emacs"
                        :sessions ,(lambda () (list latest stale))))
        (dolist (read '(lispm-agents-sessions-list lispm-agents-sessions-snapshot))
          (funcall read)
          (should (equal lispm-agents-buffer-title "Latest")))))))

(ert-deftest lispm-test-session-custom-resume-reused-buffer ()
  "Custom resume callbacks cannot retain a stopped buffer's old order."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (lispm-agents-show-function #'ignore)
          (lispm-agents-sessions-changed-hook nil)
          (session '(:agent custom :id "s" :cwd "/tmp/" :time 1)))
      (setq lispm-agents-buffer-launched 1)
      (lispm-test--with-agents
          `((custom :name "Custom" :new ignore :resume ,(lambda (_) buffer)))
        (lispm-agents-resume-session session)
        (should (> (lispm-agents-session-opened-time session) 1))))))

(ert-deftest lispm-test-session-claim-orders-before-first-snapshot ()
  "An id learned late uses launch time and never claims a child session."
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (lispm-agents-terminal-modes '(fundamental-mode))
           (lispm-agents-terminal-live-p-function #'buffer-live-p)
           (older '(:agent test-agent :id "existing" :cwd "/tmp/" :title "Existing" :started 99 :time 999))
           (child '(:agent test-agent :id "child" :parent "parent" :cwd "/tmp/" :title "Child" :started 95 :time 999))
           (fresh '(:agent test-agent :id "fresh" :cwd "/tmp/" :title "Fresh" :started 96 :time 96)))
      (setq default-directory "/tmp/" lispm-agents-buffer-agent 'test-agent
            lispm-agents-buffer-launched 100)
      (lispm-test--with-agents
          `((test-agent :name "Test" :program "emacs"
                        :sessions ,(lambda () (list older child fresh))))
        (let ((sessions (car (lispm-agents-sessions-snapshot))))
          (should (equal lispm-agents-buffer-session-id "fresh"))
          (should (eq (car sessions) fresh))
          (should (= (lispm-agents-session-opened-time fresh) 100))
          (should (equal (buffer-local-value 'lispm-agents-buffer-title buffer) "Fresh")))))))

(ert-deftest lispm-test-session-tree-cross-directory-and-cycles ()
  "Trees keep descendants and their live buffers in the root project."
  (lispm-test--with-agents nil
    (let* ((root '(:agent codex :id "root" :cwd "/tmp/project/" :started 10 :time 10))
           (child '(:agent codex :id "child" :parent "root" :cwd "/tmp/worktree/" :started 20 :time 100))
           (grandchild '(:agent codex :id "grandchild" :parent "child" :cwd "/tmp/elsewhere/" :started 30 :time 30))
           (sessions (list grandchild child root)))
      (with-temp-buffer
        (setq default-directory "/tmp/worktree/" lispm-agents-buffer-agent 'codex
              lispm-agents-buffer-session-id "child")
        (cl-letf (((symbol-function 'lispm-agents-of-buffer) (lambda (_) 'codex)))
          (let ((groups (lispm-agents-group-by-directory sessions (list (current-buffer)))))
            (should (= (length groups) 1))
            (should (equal (plist-get (car groups) :dir) "/tmp/project"))
            (should (equal (plist-get (car groups) :sessions) sessions))
            (should (equal (plist-get (car groups) :running) (list (current-buffer)))))))
      ;; A malformed parent cycle still lists every session exactly once.
      (let* ((a '(:agent codex :id "a" :parent "b"))
             (b '(:agent codex :id "b" :parent "a"))
             (c '(:agent codex :id "c" :parent "a"))
             (trees (lispm-agents-session-tree (list c a b))))
        (cl-labels ((ids (tree) (cons (plist-get (car tree) :id) (mapcan #'ids (cdr tree)))))
          (should (equal (sort (mapcan #'ids trees) #'string<) '("a" "b" "c"))))))))

(provide 'lispm-agents-session-order-test)
;;; lispm-agents-session-order-test.el ends here
