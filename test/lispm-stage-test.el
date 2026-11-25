;;; lispm-stage-test.el --- Tests for adaptive stage tiling and focus zoom  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests witnessing minimum-dimension split planning, multi-pane
;; auto-tiling with LRU fallback, click-to-zoom freeze, prompt/timeout
;; unzoom restoration, and session-id subagent lineage resolution.

;;; Code:

(require 'ert)
(require 'lispm)
(require 'lispm-test)

(defmacro lispm-stage-test--with-env (&rest body)
  "Run BODY in `lispm-test-env' with isolated FRP stage and spotlight state."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-edits--in-batch t)
           (lispm-edits--shows nil)
           (lispm-edits--proposals nil)
           (lispm-edits--writing (make-hash-table :test #'equal))
           (lispm-spotlight--in-batch t)
           (lispm-spotlight--ephemeral-buffers nil)
           (lispm-shell-stage--in-batch t)
           (lispm-shell-stage--sessions (make-hash-table :test #'eq))
           (lispm-eval-stage--in-batch t)
           (lispm-eval-stage--sessions (make-hash-table :test #'eq)))
       (lispm-stage-reset)
       (lispm-spotlight-reset)
       (unwind-protect
           (progn ,@body)
         (lispm-edits--tick 1e10)
         (lispm-spotlight--tick 1e10)
         (lispm-spotlight-reset)
         (lispm-stage-reset)))))

(ert-deftest lispm-stage-test-pure-geometry-planner ()
  "Split planner enforces minimum width and height for every stage kind."
  ;; Wide enough for two 70-col code columns: splits horizontally.
  (should (equal (plist-get (lispm-stage--plan-split 150 40 70 12 70 12) :side)
                 'right))
  ;; 95 cols x 42 lines: cannot split 70+70 horizontally, splits vertically.
  (should (equal (plist-get (lispm-stage--plan-split 95 42 70 12 70 12) :side)
                 'below))
  ;; 65 cols is below code's 70-col minimum: refuses to split.
  (should-not (lispm-stage--plan-split 65 42 70 12 70 12))
  ;; 95 cols x 21 lines can still split code (70x12) + shell (56x8).
  (should (equal (plist-get (lispm-stage--plan-split 95 21 70 12 56 8) :side)
                 'below))
  ;; 95 cols x 15 lines cannot split code (12) + code (12): returns nil -> LRU.
  (should-not (lispm-stage--plan-split 95 15 70 12 70 12)))

(ert-deftest lispm-stage-test-auto-split-and-lru-eviction ()
  "A large frame auto-splits multiple panes >= min size, then reuses LRU."
  (lispm-stage-test--with-env
    (let ((fa (expand-file-name "a.el" project))
          (fb (expand-file-name "b.el" project))
          (fc (expand-file-name "c.el" project))
          (agent (lispm-defagent tiler "Tiler." :root project)))
      (dolist (f (list fa fb fc))
        (lispm-test--write f "(defun f () 1)\n"))
      (let ((chat (lispm-chat agent))
            (orig-w (frame-width))
            (orig-h (frame-height)))
        (unwind-protect
            (save-window-excursion
              (set-frame-size (selected-frame) 160 44)
              (delete-other-windows)
              (switch-to-buffer chat)
              (let ((lispm-stage-max-tiles 3))
                ;; 1. First file splits off a >=70-col stage column.
                (lispm-test--call agent "read_file" '((path . "a.el")))
                (should (= (length (window-list)) 2))
                (let ((wa (get-buffer-window (find-buffer-visiting fa))))
                  (should (>= (window-total-width wa) 70)))
                ;; 2. Second file auto-splits vertically inside the stage column.
                (lispm-spotlight--on-response agent nil)
                (lispm-test--call agent "read_file" '((path . "b.el")))
                (should (= (length (window-list)) 3))
                (should (get-buffer-window (find-buffer-visiting fa)))
                (should (get-buffer-window (find-buffer-visiting fb)))
                ;; 3. Shell call auto-splits a 3rd pane (code + code + shell).
                (lispm-test--call-async agent "run_shell"
                                        '((command . "git --version")))
                (should (= (length (window-list)) 4))
                (should (get-buffer-window (lispm-shell-stage--buffer agent)))
                ;; 4. At max-tiles=3, a 4th code buffer evicts oldest code tile `a.el'.
                (lispm-spotlight--on-response agent nil)
                (lispm-test--call agent "read_file" '((path . "c.el")))
                (should (= (length (window-list)) 4))
                (should-not (get-buffer-window (find-buffer-visiting fa)))
                (should (get-buffer-window (find-buffer-visiting fb)))
                (should (get-buffer-window (find-buffer-visiting fc)))))
          (set-frame-size (selected-frame) orig-w orig-h))))))

(ert-deftest lispm-stage-test-zoom-freeze-and-prompt-or-timeout-resume ()
  "Selecting a stage pane zooms and freezes it; prompt click or timeout unzooms."
  (lispm-stage-test--with-env
    (let ((fa (expand-file-name "zoom-a.el" project))
          (fb (expand-file-name "zoom-b.el" project))
          (fc (expand-file-name "zoom-c.el" project))
          (agent (lispm-defagent zoomer "Zoomer." :root project)))
      (dolist (f (list fa fb fc))
        (lispm-test--write f "(defun z () 1)\n"))
      (let ((chat (lispm-chat agent))
            (orig-w (frame-width))
            (orig-h (frame-height)))
        (unwind-protect
            (save-window-excursion
              (set-frame-size (selected-frame) 160 44)
              (delete-other-windows)
              (switch-to-buffer chat)
              (setf (lispm-agent-state agent) 'requesting)
              (lispm-test--call agent "read_file" '((path . "zoom-a.el")))
              (lispm-spotlight--on-response agent nil)
              (lispm-test--call agent "read_file" '((path . "zoom-b.el")))
              (let ((ba (find-buffer-visiting fa))
                    (bb (find-buffer-visiting fb)))
                (should (= (length (lispm-stage--windows (selected-frame))) 2))
                ;; 1. User clicks into `zoom-a.el': collapses to 1 zoomed pane.
                (let ((wa (get-buffer-window ba)))
                  (select-window wa)
                  (let ((noninteractive nil))
                    (lispm-stage--on-window-selection-change (selected-frame)))
                  (should (lispm-stage-zoomed-p (selected-frame)))
                  (should (= (length (lispm-stage--windows (selected-frame))) 1))
                  (should (get-buffer-window ba))
                  (should-not (get-buffer-window bb)))
                ;; 2. Agent reads `zoom-c.el' while zoomed: stage stays frozen on `ba'.
                (lispm-spotlight--on-response agent nil)
                (lispm-test--call agent "read_file" '((path . "zoom-c.el")))
                (let ((bc (find-buffer-visiting fc)))
                  (should (= (length (lispm-stage--windows (selected-frame))) 1))
                  (should (get-buffer-window ba))
                  (should-not (get-buffer-window bc))
                  ;; 3. While agent is busy, clicking back into `chat' re-tiles!
                  (select-window (get-buffer-window chat))
                  (let ((noninteractive nil))
                    (lispm-stage--on-window-selection-change (selected-frame)))
                  (should-not (lispm-stage-zoomed-p (selected-frame)))
                  (should (>= (length (lispm-stage--windows (selected-frame))) 2))
                  (should (get-buffer-window bc))
                  ;; 4. While agent is busy, 10s inactivity timeout unzooms & re-tiles.
                  (select-window (get-buffer-window ba))
                  (let ((noninteractive nil))
                    (lispm-stage--on-window-selection-change (selected-frame)))
                  (should (lispm-stage-zoomed-p (selected-frame)))
                  (let ((lispm-stage-zoom-timeout 10))
                    (lispm-stage-set-zoom-last-active (- (float-time) 11.0))
                    (lispm-stage--check-zoom-timeout))
                  (should-not (lispm-stage-zoomed-p (selected-frame)))
                  (should (eq (window-buffer (selected-window)) chat))
                  (should (>= (length (lispm-stage--windows (selected-frame))) 2))
                  ;; 5. Once agent stops (`idle'), clicking a buffer and returning
                  ;; to `chat' keeps the single buffer without re-splitting!
                  (setf (lispm-agent-state agent) 'idle)
                  (select-window (get-buffer-window ba))
                  (let ((noninteractive nil))
                    (lispm-stage--on-window-selection-change (selected-frame)))
                  (should (= (length (lispm-stage--windows (selected-frame))) 1))
                  (select-window (get-buffer-window chat))
                  (let ((noninteractive nil))
                    (lispm-stage--on-window-selection-change (selected-frame)))
                  (should-not (lispm-stage-zoomed-p (selected-frame)))
                  (should (= (length (lispm-stage--windows (selected-frame))) 1))
                  (should (get-buffer-window ba)))))
          (set-frame-size (selected-frame) orig-w orig-h))))))

(ert-deftest lispm-stage-test-subagent-session-id-lineage ()
  "Subagents with session-id `:parent' project onto their parent's stage."
  (lispm-stage-test--with-env
    (let* ((file (expand-file-name "sub.el" project))
           (lead (lispm-defagent lead "Lead." :root project))
           (worker (lispm-defagent worker "Worker." :root project))
           (chat (lispm-chat lead)))
      (lispm-test--write file "(defun sub () 42)\n")
      (setf (lispm-agent-parent worker) (lispm-agent-id lead))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer chat)
        (should (lispm-stage-descendant-p worker lead))
        (should (eq (lispm-stage-agent-frame worker) (selected-frame)))
        (lispm-test--call worker "read_file" '((path . "sub.el")))
        (should (get-buffer-window (find-buffer-visiting file)))))))

(ert-deftest lispm-stage-test-chat-autoscroll-when-bottom-visible ()
  "New chat output auto-scrolls to bottom even if point drifted above input."
  (lispm-stage-test--with-env
    (let* ((agent (lispm-defagent talker "Talker." :root project))
           (chat (lispm-chat agent))
           (session (lispm-agent-buffer agent)))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer chat)
        (with-current-buffer session
          (goto-char (point-max))
          (insert "first question\n"))
        (lispm--live-begin agent)
        (lispm--live-insert agent 'text "line 1\nline 2\n")
        (lispm-chat-redraw chat)
        ;; Simulate clicking/Vim-escaping slightly above `lispm-chat--input-start'
        ;; while the bottom prompt is still visible in the chat window.
        (with-current-buffer chat
          (goto-char (- lispm-chat-prompt-start 5))
          (set-window-point (selected-window) (point)))
        (lispm--live-insert agent 'text
                            (mapconcat (lambda (i) (format "more-%d" i))
                                       (number-sequence 1 30) "\n"))
        (lispm-chat-redraw chat)
        (with-current-buffer chat
          (should (= (window-point (selected-window)) (point-max)))
          ;; Scroll up to top: jump-to-bottom pill appears, then clears on jump.
          (goto-char (point-min))
          (set-window-start (selected-window) (point-min))
          (set-window-point (selected-window) (point-min))
          (lispm-chat--refresh-jump-bars)
          (should lispm-chat--jump-overlays)
          (lispm-chat-scroll-to-bottom)
          (should (= (window-point (selected-window)) (point-max)))
          (should-not lispm-chat--jump-overlays))))))

(ert-deftest lispm-stage-test-coedit-human-vs-agent-colours ()
  "Co-editing stamps distinct provenance faces and preserves disjoint shows."
  (lispm-stage-test--with-env
    (let* ((file (expand-file-name "coedit.el" project))
           (agent (lispm-defagent pair "Pair coder." :root project)))
      (lispm-test--write file "line-1\nline-2\nline-3\nline-4\nline-5\n")
      (let ((buf (find-file-noselect file)))
        ;; 1. Agent edits line 5.
        (lispm-test--call agent "edit_file"
                          '((path . "coedit.el")
                            (old_text . "line-5")
                            (new_text . "agent-line-5")))
        (with-current-buffer buf
          ;; 2. Human edits disjoint line 1 while agent's show on line 5 is active.
          (goto-char (point-min))
          (insert "user-")
          ;; Agent's animation on line 5 was NOT wiped by human edit on line 1!
          (should (lispm-edits-active-p buf))
          (let ((prov (seq-filter (lambda (o) (overlay-get o 'lispm-provenance))
                                  (overlays-in (point-min) (point-max)))))
            (should (seq-some (lambda (o)
                                (and (eq (overlay-get o 'lispm-provenance) 'user)
                                     (eq (overlay-get o 'face) 'lispm-edits-user)))
                              prov))
            (should (seq-some (lambda (o)
                                (eq (overlay-get o 'lispm-provenance) 'agent))
                              prov))))))))

(ert-deftest lispm-stage-test-agent-switching-and-idle-close ()
  "Stage dynamically follows the selected agent and closes when idle or in Files/Shells."
  (lispm-stage-test--with-env
    (let* ((fa (expand-file-name "agent-a.el" project))
           (fc (expand-file-name "agent-c.el" project))
           (agent-a (lispm-defagent alpha-a "Busy agent A." :root project))
           (agent-b (lispm-defagent beta-b "Idle agent B." :root project))
           (agent-c (lispm-defagent gamma-c "Busy agent C." :root project))
           (chat-a (lispm-chat agent-a))
           (chat-b (lispm-chat agent-b))
           (chat-c (lispm-chat agent-c))
           (orig-w (frame-width))
           (orig-h (frame-height)))
      (lispm-test--write fa "(defun a () 1)\n")
      (lispm-test--write fc "(defun c () 3)\n")
      (unwind-protect
          (save-window-excursion
            (set-frame-size (selected-frame) 160 44)
            (delete-other-windows)
            (switch-to-buffer chat-a)
            (setf (lispm-agent-state agent-a) 'running
                  (lispm-agent-state agent-b) 'idle
                  (lispm-agent-state agent-c) 'running)
            (lispm-stage--on-agent-state agent-a)
            (lispm-stage--on-agent-state agent-b)
            (lispm-stage--on-agent-state agent-c)
            ;; Agent A reads `fa'; background Agent C reads `fc'.
            (lispm-test--call agent-a "read_file" '((path . "agent-a.el")))
            (lispm-test--call agent-c "read_file" '((path . "agent-c.el")))
            (let ((ba (find-buffer-visiting fa))
                  (bc (find-buffer-visiting fc)))
              (should (get-buffer-window ba))
              (should-not (get-buffer-window bc))
              ;; 1. Switch chat window to busy Agent C: spotlight switches to `bc'!
              (set-window-buffer (get-buffer-window chat-a) chat-c)
              (let ((noninteractive nil))
                (lispm-stage--on-window-buffer-change (selected-frame)))
              (should-not (get-buffer-window ba))
              (should (get-buffer-window bc))
              ;; 2. Switch chat window to idle Agent B: spotlight closes completely!
              (set-window-buffer (get-buffer-window chat-c) chat-b)
              (let ((noninteractive nil))
                (lispm-stage--on-window-buffer-change (selected-frame)))
              (should (= (length (lispm-stage--windows (selected-frame))) 0))
              (should (= (length (window-list)) 1))
              ;; 3. Switch back to busy Agent C, then suspend stage reactively
              ;; (host Files/Shells mode), re-enable, then C finishes -> closes!
              (set-window-buffer (get-buffer-window chat-b) chat-c)
              (let ((noninteractive nil))
                (lispm-stage--on-window-buffer-change (selected-frame)))
              (should (= (length (lispm-stage--windows (selected-frame))) 1))
              (lispm-stage-set-enabled nil)
              (should (= (length (lispm-stage--windows (selected-frame))) 0))
              (lispm-stage-set-enabled t)
              (should (= (length (lispm-stage--windows (selected-frame))) 1))
              (setf (lispm-agent-state agent-c) 'idle)
              (lispm-stage--on-agent-state agent-c)
              (should (= (length (lispm-stage--windows (selected-frame))) 0))))
        (set-frame-size (selected-frame) orig-w orig-h)))))

(provide 'lispm-stage-test)
;;; lispm-stage-test.el ends here
