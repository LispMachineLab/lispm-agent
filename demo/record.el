;;; record.el --- Play and record the LispM demo in your own Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; demo/record.sh starts Emacs with your configuration, in a scratch
;; HOME, and loads this file after it.  The demo is then played as if
;; typed, while demo/capture-window.swift records the frame, and only
;; it.  The stretches in which the model works are written down, as an
;; ffmpeg filter that speeds them up, for record.sh to cut the final
;; videos with.

;;; Code:

(require 'cl-lib)
(require 'lispm)
(require 'org)

(defvar lispm-demo-raw (getenv "LISPM_DEMO_RAW")
  "Where the recording is written, as it is.")

(defvar lispm-demo-filter (getenv "LISPM_DEMO_FILTER")
  "Where the filter that speeds up the model's work is written.")

(defvar lispm-demo-frame-size '(1280 . 800)
  "Size of the frame's inside, in points.")

(defvar lispm-demo-speedup 8
  "How much faster the model's work is shown.")

(defvar lispm-demo--recorder nil "The window recorder.")
(defvar lispm-demo--started nil "When recording started.")
(defvar lispm-demo--fast nil "Stretches to speed up, (START . END) in `float-time'.")
(defvar lispm-demo--turns 0 "Turns finished so far.")
(defvar lispm-demo--streams nil
  "Stretches (FROM . TO) in which the model wrote a change, newest first.")

(add-hook 'lispm-turn-end-functions
          (lambda (&rest _) (setq lispm-demo--turns (1+ lispm-demo--turns))))

(defun lispm-demo--streamed (_agent call)
  "Note that the model is writing CALL; a change it makes is worth watching."
  (when-let* ((tool (lispm-get-tool (plist-get call :name)))
              ((lispm-tool-changes tool)))
    (let ((now (float-time))
          (last (car lispm-demo--streams)))
      (if (and last (< (- now (cdr last)) 1.0))
          (setcdr last now)
        (push (cons now now) lispm-demo--streams)))))

(add-hook 'lispm-tool-input-functions #'lispm-demo--streamed)

;; `emacsclient -s lispm-demo' looks in while the demo plays.
(require 'server)
(setq server-name "lispm-demo")
(server-start)
;; MCP servers would start npx in the scratch HOME.
(setq lispm-mcp-servers nil)
;; The sample project is ours: let Flymake check it.
(add-to-list 'trusted-content (file-name-as-directory (file-truename "~/slug/")))
;; Keys typed by the script show at once, as typing would.
(setq echo-keystrokes 0.1)
;; A chat is drawn like a terminal: no line numbers.
(add-hook 'lispm-chat-mode-hook
          (lambda ()
            (run-at-time 0 nil (lambda (buffer)
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer (display-line-numbers-mode -1))))
                         (current-buffer))))

;;;; Playing

(defun lispm-demo--agent ()
  "The agent of the demo."
  (car (lispm-agents)))

(defun lispm-demo--press (events)
  "Feed EVENTS to Emacs as if typed."
  (setq unread-command-events (append unread-command-events events)))

(defun lispm-demo--play (steps)
  "Play STEPS, a list of (ACTION . ARGS), one after the other."
  (when steps
    (let ((next (lambda () (lispm-demo--play (cdr steps))))
          (step (car steps)))
      (condition-case err
          (lispm-demo--step step next)
        (error (message "LispM demo: %S failed: %S" step err)
               (lispm-demo--abort))))))

(defun lispm-demo--step (step next)
  "Take STEP, then call NEXT."
  (pcase step
    (`(pause ,seconds) (run-at-time seconds nil next))
    (`(keys ,keys)
     (lispm-demo--press (listify-key-sequence (kbd keys)))
     (run-at-time 0.05 nil next))
    (`(type ,text) (lispm-demo--type (string-to-list text) next))
    (`(call ,function) (funcall function) (run-at-time 0.05 nil next))
    (`(until ,predicate) (lispm-demo--until predicate next))
    (`(model ,predicate) (lispm-demo--model predicate next))
    (`(answer ,turns) (lispm-demo--answer turns next))
    (_ (error "Unknown step %S" step))))

(defun lispm-demo--type (chars next)
  "Type CHARS one by one, then call NEXT."
  (if (null chars)
      (run-at-time 0.1 nil next)
    (lispm-demo--press (list (car chars)))
    (run-at-time (/ (+ 30 (random 40)) 1000.0) nil #'lispm-demo--type (cdr chars) next)))

(defun lispm-demo--until (predicate next)
  "Call NEXT once PREDICATE holds."
  (if (funcall predicate)
      (funcall next)
    (run-at-time 0.1 nil #'lispm-demo--until predicate next)))

(defun lispm-demo--model (predicate next)
  "Call NEXT once PREDICATE holds, noting the wait as stretches to speed up.
The first moments and the last stay at their own speed, and so do
those in which the model writes a change: it shows as it is written."
  (let ((start (float-time)))
    (lispm-demo--until predicate
                       (lambda ()
                         (dolist (piece (lispm-demo--without
                                         (cons (+ start 1.2) (- (float-time) 0.4))
                                         lispm-demo--streams))
                           (when (> (- (cdr piece) (car piece)) 1.0)
                             (push piece lispm-demo--fast)))
                         (funcall next)))))

(defun lispm-demo--without (stretch holes)
  "The pieces of STRETCH, (FROM . TO), outside HOLES and a moment around them."
  (let ((pieces (list stretch)))
    (dolist (hole holes pieces)
      (let ((a (- (car hole) 0.5)) (b (+ (cdr hole) 0.3)))
        (setq pieces
              (mapcan (lambda (piece)
                        (let ((left (cons (car piece) (min a (cdr piece))))
                              (right (cons (max b (car piece)) (cdr piece))))
                          (seq-filter (lambda (p) (< (car p) (cdr p))) (list left right))))
                      pieces))))))

(defun lispm-demo--answer (turns next)
  "Answer whatever the agent asks until TURNS turns are over, then call NEXT.
A command is allowed for the session (2), a change approved once (1),
after a look at it."
  (lispm-demo--model
   (lambda () (or (lispm-demo-approval-p) (>= lispm-demo--turns turns)))
   (lambda ()
     (if (not (lispm-demo-approval-p))
         (funcall next)
       (let* ((agent (lispm-demo--agent))
              (call (gethash (car (lispm--waiting-calls agent)) (lispm-agent-pending agent)))
              (command (equal (plist-get call :name) "run_shell")))
         (run-at-time (if command 2.5 3.5) nil
                      (lambda ()
                        ;; The answer goes to the chat, wherever the focus went.
                        (when-let* ((window (get-buffer-window (lispm-demo--chat))))
                          (select-window window))
                        (lispm-demo--type (list (if command ?2 ?1))
                                          (lambda ()
                                            (run-at-time 0.5 nil #'lispm-demo--answer
                                                         turns next))))))))))

(defun lispm-demo-approval-p ()
  "Non-nil when the agent waits for an answer."
  (when-let* ((agent (lispm-demo--agent)))
    (lispm--waiting-calls agent)))

(defun lispm-demo-turns (n)
  "A predicate holding once N turns are over."
  (lambda () (>= lispm-demo--turns n)))

(defun lispm-demo--chat ()
  "The chat buffer of the demo."
  (when-let* ((agent (lispm-demo--agent)))
    (lispm-agent-view agent)))

;;;; Recording

(defvar lispm-demo-capture (getenv "LISPM_DEMO_CAPTURE")
  "The window recorder, built from demo/capture-window.swift.")

(defun lispm-demo--record (then)
  "Start recording the inside of the selected frame; call THEN once it records."
  (pcase-let ((`(,outer-left ,outer-top ,_ ,_) (frame-edges nil 'outer-edges))
              (`(,left ,top ,right ,bottom) (frame-edges nil 'inner-edges)))
    (setq lispm-demo--recorder
          (make-process
           :name "lispm-demo-capture" :buffer " *lispm-demo-capture*"
           :command (mapcar (lambda (x) (format "%s" x))
                            (list lispm-demo-capture (emacs-pid) lispm-demo-raw
                                  (- left outer-left) (- top outer-top)
                                  (- right left) (- bottom top)))
           :filter (lambda (process output)
                     (with-current-buffer (process-buffer process) (insert output))
                     (when (and (not lispm-demo--started) (string-search "started" output))
                       (setq lispm-demo--started (float-time))
                       (funcall then)))))))

(defun lispm-demo--stop ()
  "Stop recording, write the filter, and leave Emacs."
  (let ((stopped (float-time)))
    (set-process-sentinel
     lispm-demo--recorder
     (lambda (_process _event)
       (lispm-demo--write-filter stopped)
       (kill-emacs 0)))
    (interrupt-process lispm-demo--recorder)))

(defun lispm-demo--abort ()
  "Give up: stop recording and leave Emacs, without a filter."
  (if (not (process-live-p lispm-demo--recorder))
      (kill-emacs 1)
    (set-process-sentinel lispm-demo--recorder (lambda (&rest _) (kill-emacs 1)))
    (interrupt-process lispm-demo--recorder)))

(defun lispm-demo--duration ()
  "Time of the last frame recorded, in seconds."
  (string-to-number
   (car (process-lines "ffprobe" "-v" "error" "-show_entries" "format=duration"
                       "-of" "csv=p=0" lispm-demo-raw))))

(defun lispm-demo--write-filter (stopped)
  "Write the filter speeding up the model's work; STOPPED is when recording ended.
A window is recorded only when it changes, so the movie ends with the
last change: the filter holds that frame until STOPPED."
  (let* ((duration (lispm-demo--duration))
         (origin lispm-demo--started)
         (fast (sort (mapcar (lambda (s) (cons (max 0 (- (car s) origin))
                                               (min duration (- (cdr s) origin))))
                             lispm-demo--fast)
                     (lambda (a b) (< (car a) (car b)))))
         (at 0) (segments nil))
    (dolist (stretch fast)
      (when (> (car stretch) at)
        (push (list at (car stretch) 1) segments))
      (push (list (car stretch) (cdr stretch) lispm-demo-speedup) segments)
      (setq at (cdr stretch)))
    (push (list at duration 1) segments)
    (setq segments (nreverse segments))
    (with-temp-file lispm-demo-filter
      (cl-loop for (from to speed) in segments
               for i from 0
               do (insert (format "[0:v]trim=start=%.3f:end=%.3f,setpts=(PTS-STARTPTS)/%d[s%d];\n"
                                  from to speed i)))
      (cl-loop for i from 0 below (length segments)
               do (insert (format "[s%d]" i)))
      (insert (format "concat=n=%d:v=1:a=0,fps=30,tpad=stop_mode=clone:stop_duration=%.3f[out]\n"
                      (length segments) (max 0 (- stopped origin duration)))))))

;;;; The demo

(defconst lispm-demo-script
  `((pause 1.5)
    ;; Start an agent in the project, the way this configuration does.
    (keys "C-c")
    (pause 0.4)
    (keys "a")
    (pause 1.6)
    (keys "l")
    (until lispm-demo--chat)
    (pause 1.0)
    (call rk/sidebar-agents)
    (call ,(lambda () (select-window (get-buffer-window (lispm-demo--chat)))))
    (pause 1.5)
    (type "Run make test, find out why it fails and fix slug.el, then write a short README.md.")
    (pause 0.6)
    (keys "RET")
    ;; Commands and changes need approval.  A change streams into its file
    ;; as the model writes it, and awaits approval there.
    (answer 1)
    (pause 3.0)
    ;; The chat is drawn from an org document: the tool calls are TODO entries.
    (keys "C-c C-t")
    (pause 0.8)
    (call ,(lambda ()
             (with-current-buffer (window-buffer (selected-window))
               (goto-char (point-min))
               (org-content))))
    (pause 5.0)
    ;; The transcript opened in a workspace of its own: back to the project's.
    (call ,(lambda () (tab-bar-switch-to-tab "slug")))
    (call ,(lambda () (select-window (get-buffer-window (lispm-demo--chat)))))
    (pause 1.5)
    ;; The agent can drive Emacs itself.
    (type "Now show me the git diff of your changes in the window on the right.")
    (pause 0.6)
    (keys "RET")
    (answer 2)
    (pause 5.0)
    (call lispm-demo--stop))
  "What the demo does.")

(defun lispm-demo-start ()
  "Lay out the frame, start recording and play the demo."
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'fullscreen nil)
    (set-frame-position frame 40 70)
    (set-frame-size frame (car lispm-demo-frame-size) (cdr lispm-demo-frame-size) t)
    (dolist (name '("*Warnings*" "*GNU Emacs*"))
      (when (get-buffer name) (kill-buffer name)))
    (delete-other-windows)
    (find-file "~/slug/slug.el")
    (when (fboundp 'rk/sidebar-files) (rk/sidebar-files))
    (select-window (get-buffer-window "slug.el"))
    (select-frame-set-input-focus frame)
    (run-at-time 2 nil (lambda ()
                         ;; Start from a clean echo area.
                         (message nil)
                         (lispm-demo--record #'lispm-demo--begin)))))

(defun lispm-demo--begin ()
  "Play the demo, now that it is being recorded."
  ;; A step that fails stops the script: give up then.
  (run-at-time 360 nil #'lispm-demo--abort)
  (run-at-time 1 nil #'lispm-demo--play lispm-demo-script))

(add-hook 'emacs-startup-hook (lambda () (run-at-time 1 nil #'lispm-demo-start)) 90)

;;; record.el ends here
