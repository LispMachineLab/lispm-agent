;;; lme-term.el --- Terminals emulated off Emacs's thread  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm-emacs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs runs Lisp, process output and redisplay on one thread, one thing
;; at a time.  A terminal emulated in Lisp, as Eat's is, decodes every byte
;; its program writes on that thread, shown or not: agents resuming long
;; sessions all at once froze Emacs while their history was decoded.
;;
;; Here a terminal is emulated in a dynamic module written in Rust
;; (native/lme-term), each on a thread of its own, off Emacs's thread.
;; Emacs is woken only for the terminals it shows, and draws only the
;; lines that changed, so its work depends on the windows shown, never on
;; how many terminals run or how much they write:
;;
;;   program --PTY--> module thread: decode, keep the screen
;;              shown and changed? --channel--> `lme-term--wake'
;;   scheduler `lme-term--tick': the selected terminal first, each frame
;;              within `lme-term-frame-budget', giving way to the keyboard
;;   buffer: the screen's lines, faces from the cells' colours
;;
;; The buffer holds the screen only; the history stays in the module,
;; scrolled with the mouse wheel or `lme-term-scroll-back', and copied into
;; a buffer of its own by `lme-term-history'.  The module is built with
;; Cargo at the first start (`lme-term-build'), and again at a start that
;; finds its sources newer; until then, and where Rust is missing,
;; terminals run in Eat as before.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ansi-color)
(require 'lme-core)

(declare-function lme-term-module-init-channel "lme-term-module" (process))
(declare-function lme-term-module-spawn "lme-term-module"
                  (program args directory environment columns lines))
(declare-function lme-term-module-id "lme-term-module" (terminal))
(declare-function lme-term-module-pid "lme-term-module" (terminal))
(declare-function lme-term-module-alive-p "lme-term-module" (terminal))
(declare-function lme-term-module-input "lme-term-module" (terminal string))
(declare-function lme-term-module-resize "lme-term-module" (terminal columns lines))
(declare-function lme-term-module-set-visible "lme-term-module" (terminal visible))
(declare-function lme-term-module-set-colors "lme-term-module" (terminal colors))
(declare-function lme-term-module-scroll "lme-term-module" (terminal lines))
(declare-function lme-term-module-modes "lme-term-module" (terminal))
(declare-function lme-term-module-happenings "lme-term-module" (terminal))
(declare-function lme-term-module-render "lme-term-module" (terminal full))
(declare-function lme-term-module-text "lme-term-module" (terminal lines))
(declare-function lme-term-module-kill "lme-term-module" (terminal))

(defgroup lme-term nil
  "Terminals emulated off Emacs's thread."
  :group 'lispm-emacs)

(defcustom lme-term-enabled t
  "Non-nil runs agents and shells in lme-term once its module is built.
nil keeps them in Eat."
  :type 'boolean
  :group 'lme-term)

(defcustom lme-term-frame-budget 0.008
  "Seconds of one frame that drawing terminals may take before it gives way."
  :type 'number
  :group 'lme-term)

(defcustom lme-term-frame-interval 0.016
  "Seconds between two frames of terminal drawing, at the least."
  :type 'number
  :group 'lme-term)

(defconst lme-term-source-directory
  (expand-file-name "native/lme-term" (file-name-directory
                                       (directory-file-name
                                        (file-name-directory
                                         (or load-file-name buffer-file-name)))))
  "Directory of the module's Rust sources.")

;;;; The module

(defun lme-term--module-files ()
  "Return every built library of the module.  Cargo puts it under
target/release, or target/TRIPLE/release when a build target is set."
  (seq-filter (lambda (file) (member (file-name-extension file) '("dylib" "so" "dll")))
              (append (file-expand-wildcards
                       (expand-file-name "target/release/liblme_term.*" lme-term-source-directory))
                      (file-expand-wildcards
                       (expand-file-name "target/*/release/liblme_term.*" lme-term-source-directory)))))

(defvar lme-term--building nil
  "The Cargo process building the module, while it runs.")

(defun lme-term--module-file ()
  "Return the newest built library of the module, or nil."
  (car (sort (lme-term--module-files) #'file-newer-than-file-p)))

(defun lme-term--stale-p ()
  "Return non-nil when a source of the module is newer than its library,
as after an update."
  (when-let* ((library (lme-term--module-file)))
    (seq-some (lambda (file) (file-newer-than-file-p file library))
              (append (mapcar (lambda (name) (expand-file-name name lme-term-source-directory))
                              '("Cargo.toml" "Cargo.lock"))
                      (directory-files (expand-file-name "src" lme-term-source-directory)
                                       t "\\.rs\\'")))))

(defun lme-term-load ()
  "Load the module when it is built; return non-nil once it is loaded.
Nothing is loaded while it is being built."
  (or (featurep 'lme-term-module)
      (when-let* (((fboundp 'module-load))
                  ((not (process-live-p lme-term--building)))
                  (file (lme-term--module-file)))
        (module-load file)
        (featurep 'lme-term-module))))

(defun lme-term-build (&optional callback)
  "Build the module with Cargo in the background, then load it and call
CALLBACK.  Its output is in the *lme-term build* buffer."
  (interactive)
  (unless (executable-find "cargo")
    (user-error "Building lme-term needs Rust's Cargo"))
  (unless (process-live-p lme-term--building)
    (let ((default-directory (file-name-as-directory lme-term-source-directory))
          (buffer (get-buffer-create "*lme-term build*")))
      (with-current-buffer buffer (erase-buffer))
      (message "Building lme-term…")
      (setq lme-term--building
            (make-process
             :name "lme-term build" :buffer buffer :noquery t
             :command '("cargo" "build" "--release")
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (if (and (= (process-exit-status process) 0) (lme-term--module-file))
                     (progn
                       ;; Cargo leaves a library it found current untouched.
                       (set-file-times (lme-term--module-file))
                       (if (featurep 'lme-term-module)
                           (message "lme-term is rebuilt: restart Emacs to use it")
                         (lme-term-load)
                         (message "lme-term is built"))
                       (when callback (funcall callback)))
                   (message "lme-term could not be built: see *lme-term build*")))))))))

(defun lme-term-ready-p ()
  "Return non-nil when terminals run in lme-term: it is enabled, the
module is loaded, and Emacs can wake on its channel."
  (and lme-term-enabled (lme-term-load) (lme-term--channel) t))

;;;; Terminals and their buffers

(defvar-local lme-term--terminal nil
  "The module's terminal shown in this buffer.")

(defvar-local lme-term--id nil
  "The id of this buffer's terminal, as the channel names it.")

(defvar-local lme-term--size nil
  "The terminal's size as last set: (COLUMNS . LINES).")

(defvar-local lme-term--exit-code nil
  "The exit code of the terminal's program, once it exited.")

(defvar-local lme-term-title nil
  "The title the terminal's program last set, or nil.")

(defvar lme-term--buffers (make-hash-table)
  "The buffer of each terminal, by id.")

(defvar lme-term-exit-functions nil
  "Functions called with a terminal's buffer and exit code when its program
exits, as `eat-exit-hook' is for Eat.")

(defvar lme-term-title-functions nil
  "Functions called with a terminal's buffer when its program sets a title.")

(defun lme-term-buffer-p (&optional buffer)
  "Return non-nil when BUFFER, by default the current one, is an lme-term."
  (and (buffer-live-p (or buffer (current-buffer)))
       (buffer-local-value 'lme-term--terminal (or buffer (current-buffer)))
       t))

(defun lme-term-live-p (&optional buffer)
  "Return non-nil while BUFFER's terminal program runs."
  (when-let* ((terminal (and (lme-term-buffer-p buffer)
                             (buffer-local-value 'lme-term--terminal (or buffer (current-buffer))))))
    (lme-term-module-alive-p terminal)))

(defun lme-term-pid (&optional buffer)
  "Return the process id of BUFFER's terminal program, or nil."
  (when-let* ((terminal (and (lme-term-buffer-p buffer)
                             (buffer-local-value 'lme-term--terminal (or buffer (current-buffer))))))
    (lme-term-module-pid terminal)))

(defun lme-term-modes (&optional buffer)
  "Return the modes of BUFFER's terminal, see `lme-term-module-modes'."
  (with-current-buffer (or buffer (current-buffer))
    (if lme-term--terminal (lme-term-module-modes lme-term--terminal) 0)))

(defun lme-term-send-string (string &optional buffer paste)
  "Send STRING to BUFFER's terminal program; PASTE sends it as pasted, in
brackets when the program asked for them."
  (with-current-buffer (or buffer (current-buffer))
    (when lme-term--terminal
      (lme-term-module-input
       lme-term--terminal
       (if (and paste (/= 0 (logand 2 (lme-term-modes))))
           (concat "\e[200~" string "\e[201~")
         string)))))

(defun lme-term--kill-buffer ()
  "Stop the terminal's program as its buffer is killed."
  (when lme-term--terminal
    (lme-term-module-kill lme-term--terminal)
    (remhash lme-term--id lme-term--buffers)))

;;;; Colours

(defvar lme-term--faces (make-hash-table :test #'equal)
  "Faces by cell colours and flags: (FOREGROUND BACKGROUND FLAGS).")

(defvar lme-term--palette nil
  "The 16 colours of the theme's terminal palette, as #RRGGBB strings.")

(defun lme-term--palette ()
  "Return the theme's 16 terminal colours, from `ansi-color'."
  (or lme-term--palette
      (setq lme-term--palette
            (vconcat
             (mapcar (lambda (face)
                       (let ((color (face-foreground face nil 'default)))
                         (if (and color (color-defined-p color))
                             (apply #'format "#%02x%02x%02x"
                                    (mapcar (lambda (c) (round (* 255 c)))
                                            (color-name-to-rgb color)))
                           "#808080")))
                     (append ansi-color-normal-colors-vector
                             ansi-color-bright-colors-vector nil))))))

(defun lme-term--theme-changed (&rest _)
  "Forget the colours, so terminals take the new theme's."
  (setq lme-term--palette nil)
  (clrhash lme-term--faces)
  (maphash (lambda (_id buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lme-term--tell-colors)
                 (lme-term--render t))))
           lme-term--buffers))

(defun lme-term--color (code)
  "Return the colour of face CODE, see the module's `color_code', or nil."
  (cond ((< code 0) nil)
        ((< code 16) (aref (lme-term--palette) code))
        ((< code 232)
         (let* ((i (- code 16))
                (level (lambda (v) (if (= v 0) 0 (+ 55 (* v 40))))))
           (format "#%02x%02x%02x" (funcall level (/ i 36)) (funcall level (% (/ i 6) 6))
                   (funcall level (% i 6)))))
        ((< code 256) (let ((v (+ 8 (* (- code 232) 10)))) (format "#%02x%02x%02x" v v v)))
        (t (format "#%06x" (logand code #xffffff)))))

(defun lme-term--face (fg bg flags)
  "Return the face of a cell coloured FG on BG with FLAGS."
  (let ((key (list fg bg flags)))
    (or (gethash key lme-term--faces)
        (puthash
         key
         (let* ((foreground (lme-term--color fg))
                (background (lme-term--color bg))
                (inverse (/= 0 (logand flags 8))))
           (when inverse
             (setq foreground (prog1 (or background (face-background 'default nil t))
                                (setq background (or foreground (face-foreground 'default nil t))))))
           `(,@(when foreground (list :foreground foreground))
             ,@(when background (list :background background))
             ,@(when (/= 0 (logand flags 1)) '(:weight bold))
             ,@(when (/= 0 (logand flags 2)) '(:slant italic))
             ,@(when (/= 0 (logand flags 4)) '(:underline t))
             ,@(when (/= 0 (logand flags 64)) '(:underline (:style wave)))
             ,@(when (/= 0 (logand flags 32)) '(:strike-through t))
             ,@(when (and (/= 0 (logand flags 16)) (not foreground)) '(:inherit shadow))))
         lme-term--faces))))

(defun lme-term--tell-colors ()
  "Tell the terminal the colours it answers programs asking: the palette,
then the default foreground and background, by which Claude Code and
Codex choose a light or a dark look."
  (when lme-term--terminal
    (let ((hex (lambda (color fallback)
                 (if (and color (color-defined-p color))
                     (apply #'format "#%02x%02x%02x"
                            (mapcar (lambda (c) (round (* 255 c))) (color-name-to-rgb color)))
                   fallback))))
      (lme-term-module-set-colors
       lme-term--terminal
       (vconcat (lme-term--palette)
                (list (funcall hex (face-foreground 'default nil t) "#ffffff")
                      (funcall hex (face-background 'default nil t) "#000000")))))))

;;;; Drawing

(defun lme-term--propertize (text runs)
  "Return TEXT with the faces of RUNS, see the module's `render'."
  (let ((i 0) (n (length runs)))
    (while (< i n)
      (let ((start (aref runs i)) (end (aref runs (1+ i))))
        (when (<= end (length text))
          (put-text-property start end 'face
                             (lme-term--face (aref runs (+ i 2)) (aref runs (+ i 3))
                                             (aref runs (+ i 4)))
                             text)))
      (setq i (+ i 5)))
    text))

(defvar-local lme-term--lines nil
  "How many lines the buffer holds, the screen's height when last drawn.")

(defun lme-term--ensure-lines (lines)
  "Make the buffer hold LINES lines, blank when it held another number.
Only the terminal writes the buffer, so the count is kept rather than
counted."
  (unless (eql lme-term--lines lines)
    (erase-buffer)
    (insert (make-string (max 0 (1- lines)) ?\n))
    (setq lme-term--lines lines)))

(defun lme-term--render (&optional full)
  "Draw the changed lines of the current buffer's terminal, every line when
FULL, and place the cursor.  A terminal whose program exited keeps its
last screen and the notice under it."
  (when (and lme-term--terminal (not lme-term--exit-code))
    (pcase-let* ((`[,lines ,cursor-line ,cursor-column ,shape ,changed]
                  (lme-term-module-render lme-term--terminal full))
                 (inhibit-read-only t)
                 (inhibit-modification-hooks t)
                 (buffer-undo-list t))
      (save-restriction
        (widen)
        (lme-term--ensure-lines lines)
        (let ((line 0))
          (goto-char (point-min))
          (seq-doseq (entry changed)
            (pcase-let ((`[,index ,text ,runs] entry))
              (forward-line (- index line))
              (setq line index)
              (delete-region (point) (line-end-position))
              (insert (lme-term--propertize text runs))
              (forward-line 0))))
        (goto-char (point-min))
        (forward-line cursor-line)
        (move-to-column cursor-column t)
        (setq cursor-type (pcase shape (1 'box) (2 'hbar) (3 '(bar . 2)) (4 'hollow) (_ nil))))
      (let ((point (point)))
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (set-window-start window (point-min) t)
          (set-window-point window point)))
      (restore-buffer-modified-p nil))))

(defun lme-term--happen (buffer)
  "Take in what happened in BUFFER's terminal: titles, exits, clipboard."
  (with-current-buffer buffer
    (seq-doseq (happening (lme-term-module-happenings lme-term--terminal))
      (pcase happening
        (`(title . ,title)
         (setq lme-term-title title)
         (run-hook-with-args 'lme-term-title-functions buffer)
         (force-mode-line-update t))
        (`(reset-title . ,_) (setq lme-term-title nil))
        (`(clipboard . ,text) (kill-new text))
        (`(exit . ,code)
         ;; Its last screen is drawn once, shown or not, and kept.
         (lme-term--render t)
         (setq lme-term--exit-code code)
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (point-max))
             (insert (propertize (format "\n[Process exited%s]"
                                         (if (= code 0) "" (format " with code %d" code)))
                                 'face 'shadow))))
         (run-hook-with-args 'lme-term-exit-functions buffer code))))))

;;;; The scheduler
;;
;; The module wakes Emacs through a pipe with the ids of terminals that
;; have something new: output for the shown ones, happenings for any.
;; Waking only marks them; a frame then draws them, the selected window's
;; terminal first, until the frame's budget is spent or a key is waiting,
;; and leaves the rest to the next frame.

(defvar lme-term--channel nil
  "The pipe process the module wakes Emacs through.")

(defvar lme-term--pending nil
  "Ids of terminals woken and not yet drawn.")

(defvar lme-term--timer nil
  "Timer of the next frame, while one is due.")

(defvar lme-term--last-frame 0
  "When the last frame was drawn, a `float-time'.")

(defun lme-term--channel ()
  "Return the pipe the module wakes Emacs through, made when missing."
  (or (and (process-live-p lme-term--channel) lme-term--channel)
      (when (featurep 'lme-term-module)
        (let ((process (make-pipe-process :name "lme-term" :noquery t
                                          :coding 'binary :filter #'lme-term--wake)))
          (lme-term-module-init-channel process)
          (setq lme-term--channel process)))))

(defun lme-term--wake (_process string)
  "Mark the terminals named in STRING, lines of ids, and schedule a frame."
  (dolist (id (split-string string "\n" t))
    (cl-pushnew (string-to-number id) lme-term--pending))
  (lme-term--schedule))

(defun lme-term--schedule ()
  "Schedule the next frame, no sooner than `lme-term-frame-interval' after
the last."
  (unless lme-term--timer
    (setq lme-term--timer
          (run-with-timer (max 0 (- (+ lme-term--last-frame lme-term-frame-interval)
                                    (float-time)))
                          nil #'lme-term--tick))))

(defun lme-term--order (ids)
  "Return IDS in the order they are drawn: the selected window's first."
  (let ((selected (buffer-local-value 'lme-term--id (window-buffer (selected-window)))))
    (if (memq selected ids) (cons selected (delq selected ids)) ids)))

(defun lme-term--tick ()
  "Draw a frame: the woken terminals, within `lme-term-frame-budget',
giving way as soon as a key is waiting."
  (setq lme-term--timer nil
        lme-term--last-frame (float-time))
  (let ((ids (lme-term--order (reverse lme-term--pending)))
        (start (float-time))
        (drawn 0))
    (setq lme-term--pending nil)
    (while ids
      (if (and (> drawn 0)
               (or (> (- (float-time) start) lme-term-frame-budget) (input-pending-p)))
          (setq lme-term--pending (append ids lme-term--pending)
                ids nil)
        (let ((buffer (gethash (pop ids) lme-term--buffers)))
          (when (buffer-live-p buffer)
            ;; The last output is drawn before an exit is taken in.
            (when (get-buffer-window buffer 'visible)
              (with-current-buffer buffer
                (lme-term--render))
              (setq drawn (1+ drawn)))
            (lme-term--happen buffer)))))
    (when lme-term--pending
      (lme-term--schedule))))

;;;; Showing and sizing

(defun lme-term--window-size (window)
  "Return the terminal size WINDOW fits: (COLUMNS . LINES)."
  (with-selected-window window
    (cons (max 1 (window-max-chars-per-line window))
          (max 1 (floor (window-body-height window t) (default-line-height))))))

(defun lme-term--follow-windows (&rest _)
  "Tell each terminal whether it is shown, and size it to its window.
A terminal shown in several windows takes the selected one's size, or
else the first's.  This runs as windows change."
  (maphash
   (lambda (_id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (let* ((windows (get-buffer-window-list buffer nil 'visible))
                (window (if (memq (selected-window) windows) (selected-window) (car windows))))
           (lme-term-module-set-visible lme-term--terminal (and window t))
           (when window
             (let ((size (lme-term--window-size window)))
               (unless (equal size lme-term--size)
                 (setq lme-term--size size)
                 (lme-term-module-resize lme-term--terminal (car size) (cdr size)))))))))
   lme-term--buffers))

;;;; Keys

(defun lme-term--send (string)
  "Send STRING to the current terminal, as typed."
  (when lme-term--terminal
    (lme-term-module-input lme-term--terminal string)))

(defun lme-term-send-char ()
  "Send the character typed."
  (interactive)
  (lme-term--send (string last-command-event)))

(defun lme-term--cursor-key (letter)
  "Return the bytes of the cursor key LETTER, in application mode or not."
  (concat (if (/= 0 (logand 1 (lme-term-modes))) "\eO" "\e[") letter))

(defconst lme-term-keys
  `(("RET" . "\r") ("TAB" . "\t") ("<backtab>" . "\e[Z")
    ("DEL" . "\d") ("<backspace>" . "\d") ("<escape>" . "\e")
    ("S-<return>" . "\e[13;2u") ("C-<return>" . "\e[13;5u")
    ("<delete>" . "\e[3~") ("<deletechar>" . "\e[3~") ("<insert>" . "\e[2~")
    ("<home>" . "\e[H") ("<end>" . "\e[F")
    ("<prior>" . "\e[5~") ("<next>" . "\e[6~")
    ("<f1>" . "\eOP") ("<f2>" . "\eOQ") ("<f3>" . "\eOR") ("<f4>" . "\eOS")
    ("<f5>" . "\e[15~") ("<f6>" . "\e[17~") ("<f7>" . "\e[18~") ("<f8>" . "\e[19~")
    ("<f9>" . "\e[20~") ("<f10>" . "\e[21~") ("<f11>" . "\e[23~") ("<f12>" . "\e[24~")
    ("S-<up>" . "\e[1;2A") ("S-<down>" . "\e[1;2B") ("S-<right>" . "\e[1;2C")
    ("S-<left>" . "\e[1;2D") ("C-<up>" . "\e[1;5A") ("C-<down>" . "\e[1;5B")
    ("C-<right>" . "\e[1;5C") ("C-<left>" . "\e[1;5D")
    ("C-c C-c" . "\C-c") ("C-c C-d" . "\C-d") ("C-c C-z" . "\C-z") ("C-c C-\\" . "\C-\\")
    ("C-c C-x" . "\C-x") ("C-c C-g" . "\C-g") ("C-c C-u" . "\C-u"))
  "Keys and the bytes they send, Emacs keys left aside: C-c is a prefix
whose keys send their control character, Escape alone reaches the program,
and Return with Shift or Control sends the CSI u form Claude Code and
Codex read as a new line.")

(defun lme-term-send-key ()
  "Send the bytes of the key just typed, see `lme-term-keys'."
  (interactive)
  (when-let* ((bytes (cdr (assoc (key-description (this-command-keys-vector)) lme-term-keys))))
    (lme-term--send bytes)))

(defun lme-term-send-control ()
  "Send the control character of the Control key just typed."
  (interactive)
  (let ((event (event-basic-type last-command-event)))
    (when (characterp event)
      (lme-term--send (string (logand event #x1f))))))

(defun lme-term-send-meta ()
  "Send the Meta key just typed: Escape, then the key."
  (interactive)
  (let ((event (event-basic-type last-command-event))
        (modifiers (event-modifiers last-command-event)))
    (when (characterp event)
      (lme-term--send (concat "\e" (string (if (memq 'control modifiers)
                                                (logand event #x1f)
                                              (if (memq 'shift modifiers) (upcase event) event))))))))

(defun lme-term-up () "Send Up." (interactive) (lme-term--send (lme-term--cursor-key "A")))
(defun lme-term-down () "Send Down." (interactive) (lme-term--send (lme-term--cursor-key "B")))
(defun lme-term-right () "Send Right." (interactive) (lme-term--send (lme-term--cursor-key "C")))
(defun lme-term-left () "Send Left." (interactive) (lme-term--send (lme-term--cursor-key "D")))

(defun lme-term-yank ()
  "Paste the kill ring's newest text, as pasted."
  (interactive)
  (lme-term-send-string (current-kill 0) nil t))

(defun lme-term-scroll-back (&optional lines)
  "Scroll the terminal's view LINES into its history, back when negative."
  (interactive "p")
  (lme-term-module-scroll lme-term--terminal (or lines 1))
  (lme-term--render t))

(defun lme-term--mouse-report (button event)
  "Return the report of mouse BUTTON at EVENT's place, in the encoding the
program asked for: SGR's, else the original one, whose positions stop at
95 so that the report stays ASCII."
  (pcase-let* ((`(,column . ,row) (posn-col-row (event-start event)))
               (`(,columns . ,lines) (or lme-term--size '(80 . 24)))
               (column (1+ (min (max column 0) (1- columns))))
               (row (1+ (min (max row 0) (1- lines))))
               (modifiers (event-modifiers event))
               (button (+ button
                          (if (memq 'shift modifiers) 4 0)
                          (if (memq 'meta modifiers) 8 0)
                          (if (memq 'control modifiers) 16 0))))
    (if (/= 0 (logand 16 (lme-term-modes)))
        (format "\e[<%d;%d;%dM" button column row)
      (string ?\e ?\[ ?M (+ 32 button) (+ 32 (min column 95)) (+ 32 (min row 95))))))

(defun lme-term--wheel (event lines)
  "Scroll for the mouse wheel EVENT by LINES, as Alacritty does: a program
that asked for mouse reports is told of the wheel, one holding the whole
screen gets cursor keys, and otherwise the terminal's history scrolls.
With Shift the history always scrolls."
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (let ((modes (lme-term-modes))
          (shift (memq 'shift (event-modifiers event))))
      (cond ((and (/= 0 (logand 8 modes)) (not shift))
             (lme-term--send
              (apply #'concat (make-list (max 1 (event-line-count event))
                                         (lme-term--mouse-report (if (> lines 0) 64 65) event)))))
            ((and (/= 0 (logand 4 modes)) (not shift))
             (lme-term--send (apply #'concat (make-list (abs lines) (if (> lines 0) "\eOA" "\eOB")))))
            (t (lme-term-module-scroll lme-term--terminal lines)
               (lme-term--render t))))))

(defun lme-term-wheel-up (event)
  "Scroll back for mouse wheel EVENT."
  (interactive "e")
  (lme-term--wheel event 3))

(defun lme-term-wheel-down (event)
  "Scroll forward for mouse wheel EVENT."
  (interactive "e")
  (lme-term--wheel event -3))

(defun lme-term-history ()
  "Show the terminal's history and screen in a buffer of their own, to read,
search and copy."
  (interactive)
  (let ((text (lme-term-module-text lme-term--terminal nil))
        (name (format "*%s history*" (string-trim (buffer-name) "\\*" "\\*"))))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (special-mode)
      (goto-char (point-max)))
    (pop-to-buffer name)))

(defvar lme-term-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'lme-term-send-char)
    (dolist (key lme-term-keys)
      (define-key map (kbd (car key)) #'lme-term-send-key))
    (dolist (letter (string-to-list "abdefhijklnopqrstuvwyz"))
      (define-key map (kbd (format "C-%c" letter)) #'lme-term-send-control))
    (dolist (key '("C-SPC" "C-]" "C-_" "C-@"))
      (define-key map (kbd key) #'lme-term-send-control))
    (dolist (letter (string-to-list "abdefghijklnopqrstuyz0123456789.,/<>?;'[]-=`"))
      (define-key map (kbd (format "M-%c" letter)) #'lme-term-send-meta))
    (define-key map (kbd "<up>") #'lme-term-up)
    (define-key map (kbd "<down>") #'lme-term-down)
    (define-key map (kbd "<right>") #'lme-term-right)
    (define-key map (kbd "<left>") #'lme-term-left)
    (define-key map (kbd "C-y") #'lme-paste)
    (define-key map (kbd "M-v") #'lme-paste)
    (define-key map (kbd "M-c") #'lme-copy-region)
    (define-key map (kbd "M-w") #'lme-copy-region)
    (define-key map (kbd "M-x") #'execute-extended-command)
    (define-key map (kbd "C-c C-h") #'lme-term-history)
    (define-key map (kbd "S-<prior>") #'lme-term-scroll-back)
    (define-key map (kbd "<wheel-up>") #'lme-term-wheel-up)
    (define-key map (kbd "<wheel-down>") #'lme-term-wheel-down)
    (define-key map (kbd "S-<wheel-up>") #'lme-term-wheel-up)
    (define-key map (kbd "S-<wheel-down>") #'lme-term-wheel-down)
    (define-key map (kbd "<mouse-4>") #'lme-term-wheel-up)
    (define-key map (kbd "<mouse-5>") #'lme-term-wheel-down)
    map)
  "Keys of a terminal.  Printable keys, Control and Meta keys and the
special keys reach the program; C-x, M-x, C-g, the copy and paste keys
and the C-c prefix stay Emacs's: C-c C-c sends C-c, C-c C-h shows the
history.  The keymap outranks Evil's, see `lme-term-keys-mode'.")

(define-minor-mode lme-term-keys-mode
  "Give a terminal its keys, over every other keymap."
  :init-value nil
  :lighter nil
  :keymap lme-term-keys-mode-map)

(defvar lme-term-keys-mode-map-alist
  `((lme-term-keys-mode . ,lme-term-keys-mode-map)))

(defun lme-term--keys-first ()
  "Put the terminal keymap first among the emulation keymaps, over Evil's."
  (setq emulation-mode-map-alists
        (cons 'lme-term-keys-mode-map-alist
              (delq 'lme-term-keys-mode-map-alist emulation-mode-map-alists))))

;;;; The mode

(defvar lme-terminal-font-family)

(define-derived-mode lme-term-mode fundamental-mode "Term"
  "Major mode of a terminal emulated off Emacs's thread, see lme-term.el.
\\{lme-term-keys-mode-map}"
  (setq-local buffer-read-only t
              truncate-lines t
              scroll-margin 0
              hscroll-margin 0
              auto-hscroll-mode nil
              scroll-conservatively 101
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              show-trailing-whitespace nil
              ;; Programs draw with no-break spaces; they are plain blanks.
              nobreak-char-display nil
              indicate-empty-lines nil
              cursor-in-non-selected-windows 'hollow
              indent-tabs-mode nil
              lme-paste-function (lambda (text) (lme-term-send-string text nil t))
              left-fringe-width 0
              first-change-hook nil
              global-hl-line-mode nil)
  (visual-line-mode -1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (and (boundp 'lme-terminal-font-family) (display-graphic-p)
             (member lme-terminal-font-family (font-family-list)))
    (face-remap-add-relative 'default :family lme-terminal-font-family))
  (add-hook 'kill-buffer-hook #'lme-term--kill-buffer nil t)
  (lme-term--keys-first)
  (lme-term-keys-mode 1))

(defun lme-term-make (name program &optional args directory)
  "Run PROGRAM with ARGS in a terminal buffer named *NAME*, in DIRECTORY or
`default-directory', with `process-environment'; return the buffer.
A buffer of that name still running is returned as it is."
  (let ((buffer (get-buffer-create (format "*%s*" name))))
    (unless (lme-term-live-p buffer)
      (unless (lme-term-ready-p)
        (error "lme-term is not built; see `lme-term-build'"))
      (with-current-buffer buffer
        (lme-term-mode)
        (let* ((directory (expand-file-name (or directory default-directory)))
               (environment (append '("TERM=xterm-256color" "COLORTERM=truecolor")
                                    (seq-remove (lambda (entry)
                                                  (string-match-p "\\`\\(TERM\\|COLORTERM\\|INSIDE_EMACS\\)=" entry))
                                                process-environment)
                                    (list (format "INSIDE_EMACS=%s,lme-term" emacs-version))))
               ;; A terminal not shown yet starts the size of the selected
               ;; window, where it most likely shows: a resize just after
               ;; starting catches some programs mid-setup.
               (window (or (get-buffer-window buffer 'visible)
                           (unless (window-minibuffer-p) (selected-window))))
               (size (if window (lme-term--window-size window) '(80 . 24)))
               ;; Backspace sends DEL, as the terminal's erase character.
               (command (list "/bin/sh" "-c"
                              (format "stty erase '^?' 2>/dev/null; exec %s"
                                      (mapconcat #'shell-quote-argument (cons program args) " ")))))
          (setq default-directory (file-name-as-directory directory)
                lme-term--size size
                lme-term--exit-code nil
                lme-term--terminal (lme-term-module-spawn (car command) (vconcat (cdr command))
                                                          directory (vconcat environment)
                                                          (car size) (cdr size))
                lme-term--id (lme-term-module-id lme-term--terminal))
          (puthash lme-term--id buffer lme-term--buffers)
          (lme-term--tell-colors)
          (let ((inhibit-read-only t))
            (setq lme-term--lines nil)
            (lme-term--ensure-lines (cdr size))))))
    (lme-term--follow-windows)
    buffer))

(defun lme-term-kill (&optional buffer)
  "Stop BUFFER's terminal program."
  (with-current-buffer (or buffer (current-buffer))
    (when lme-term--terminal
      (lme-term-module-kill lme-term--terminal))))

(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-term-send-string-as-yank "eat" (terminal args))
(defvar eat-terminal)

(defun lme-terminal-make (name program &rest args)
  "Run PROGRAM with ARGS in a terminal named *NAME*; return its buffer.
It is an lme-term once the module is built, else an Eat terminal."
  (if (lme-term-ready-p)
      (lme-term-make name program args)
    (require 'eat)
    (apply #'eat-make name program nil args)))

(defun lme-terminal-live-p (buffer)
  "Return non-nil while the program of terminal BUFFER runs."
  (if (lme-term-buffer-p buffer)
      (lme-term-live-p buffer)
    (when-let* ((process (and (buffer-live-p buffer) (get-buffer-process buffer))))
      (process-live-p process))))

(defun lme-terminal-paste (buffer text)
  "Paste TEXT into the program of terminal BUFFER, as pasted."
  (if (lme-term-buffer-p buffer)
      (lme-term-send-string text buffer t)
    (with-current-buffer buffer
      (eat-term-send-string-as-yank eat-terminal (list text)))))

(defun lme-setup-term ()
  "Draw terminals as their windows change and the theme does.  The module
is built in the background the first time, and again when its sources
change, where Rust is installed."
  (add-hook 'window-buffer-change-functions #'lme-term--follow-windows)
  (add-hook 'window-size-change-functions #'lme-term--follow-windows)
  (add-hook 'window-selection-change-functions #'lme-term--follow-windows)
  (add-hook 'enable-theme-functions #'lme-term--theme-changed 90)
  (when (and lme-term-enabled (display-graphic-p) (not (featurep 'lme-term-module))
             (or (null (lme-term--module-file)) (lme-term--stale-p))
             (executable-find "cargo"))
    (lme-term-build)))

(provide 'lme-term)
;;; lme-term.el ends here
