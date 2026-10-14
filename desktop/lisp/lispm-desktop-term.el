;;; lispm-desktop-term.el --- Terminals emulated off Emacs's thread  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm
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
;; (native/lispm-desktop-term), each on a thread of its own, off Emacs's thread.
;; Emacs is woken only for the terminals it shows, and draws only the
;; lines that changed, so its work depends on the windows shown, never on
;; how many terminals run or how much they write:
;;
;;   program --PTY--> module thread: decode, keep the screen
;;              shown and changed? --channel--> `lispm-desktop-term--wake'
;;   scheduler `lispm-desktop-term--tick': the selected terminal first, each frame
;;              within `lispm-desktop-term-frame-budget', giving way to the keyboard
;;   buffer: the screen's lines, faces from the cells' colours
;;
;; The buffer holds the screen only; the history stays in the module,
;; scrolled with the mouse wheel or `lispm-desktop-term-scroll-back', and copied into
;; a buffer of its own by `lispm-desktop-term-history'.  The module is built with
;; Cargo at the first start (`lispm-desktop-term-build'), and again at a start that
;; finds its sources newer; until then, and where Rust is missing,
;; terminals run in Eat as before.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ansi-color)
(require 'lispm-desktop-core)

(declare-function lispm-desktop-term-module-init-channel "lispm-desktop-term-module" (process))
(declare-function lispm-desktop-term-module-spawn "lispm-desktop-term-module"
                  (program args directory environment columns lines))
(declare-function lispm-desktop-term-module-id "lispm-desktop-term-module" (terminal))
(declare-function lispm-desktop-term-module-pid "lispm-desktop-term-module" (terminal))
(declare-function lispm-desktop-term-module-alive-p "lispm-desktop-term-module" (terminal))
(declare-function lispm-desktop-term-module-input "lispm-desktop-term-module" (terminal string))
(declare-function lispm-desktop-term-module-resize "lispm-desktop-term-module" (terminal columns lines))
(declare-function lispm-desktop-term-module-set-visible "lispm-desktop-term-module" (terminal visible))
(declare-function lispm-desktop-term-module-set-colors "lispm-desktop-term-module" (terminal colors))
(declare-function lispm-desktop-term-module-scroll "lispm-desktop-term-module" (terminal lines))
(declare-function lispm-desktop-term-module-modes "lispm-desktop-term-module" (terminal))
(declare-function lispm-desktop-term-module-happenings "lispm-desktop-term-module" (terminal))
(declare-function lispm-desktop-term-module-render "lispm-desktop-term-module" (terminal full))
(declare-function lispm-desktop-term-module-text "lispm-desktop-term-module" (terminal lines))
(declare-function lispm-desktop-term-module-kill "lispm-desktop-term-module" (terminal))

(defgroup lispm-desktop-term nil
  "Terminals emulated off Emacs's thread."
  :group 'lispm-desktop)

(defcustom lispm-desktop-term-enabled t
  "Non-nil runs agents and shells in lispm-desktop-term once its module is built.
nil keeps them in Eat."
  :type 'boolean
  :group 'lispm-desktop-term)

(defcustom lispm-desktop-term-frame-budget 0.008
  "Seconds of one frame that drawing terminals may take before it gives way."
  :type 'number
  :group 'lispm-desktop-term)

(defcustom lispm-desktop-term-frame-interval 0.016
  "Seconds between two frames of terminal drawing, at the least."
  :type 'number
  :group 'lispm-desktop-term)

(defconst lispm-desktop-term-source-directory
  (expand-file-name "native/lispm-desktop-term" (file-name-directory
                                                 (directory-file-name
                                                  (file-name-directory
                                                   (or load-file-name buffer-file-name)))))
  "Directory of the module's Rust sources.")

;;;; The module

(defun lispm-desktop-term--module-files ()
  "Return every built library of the module.  Cargo puts it under
target/release, or target/TRIPLE/release when a build target is set."
  (seq-filter (lambda (file) (member (file-name-extension file) '("dylib" "so" "dll")))
              (append (file-expand-wildcards
                       (expand-file-name "target/release/liblispm_desktop_term.*" lispm-desktop-term-source-directory))
                      (file-expand-wildcards
                       (expand-file-name "target/*/release/liblispm_desktop_term.*" lispm-desktop-term-source-directory)))))

(defvar lispm-desktop-term--building nil
  "The Cargo process building the module, while it runs.")

(defun lispm-desktop-term--module-file ()
  "Return the newest built library of the module, or nil."
  (car (sort (lispm-desktop-term--module-files) #'file-newer-than-file-p)))

(defun lispm-desktop-term--stale-p ()
  "Return non-nil when a source of the module is newer than its library,
as after an update."
  (when-let* ((library (lispm-desktop-term--module-file)))
    (seq-some (lambda (file) (file-newer-than-file-p file library))
              (append (mapcar (lambda (name) (expand-file-name name lispm-desktop-term-source-directory))
                              '("Cargo.toml" "Cargo.lock"))
                      (directory-files (expand-file-name "src" lispm-desktop-term-source-directory)
                                       t "\\.rs\\'")))))

(defun lispm-desktop-term-load ()
  "Load the module when it is built; return non-nil once it is loaded.
Nothing is loaded while it is being built."
  (or (featurep 'lispm-desktop-term-module)
      (when-let* (((fboundp 'module-load))
                  ((not (process-live-p lispm-desktop-term--building)))
                  (file (lispm-desktop-term--module-file)))
        (module-load file)
        (featurep 'lispm-desktop-term-module))))

(defun lispm-desktop-term-build (&optional callback)
  "Build the module with Cargo in the background, then load it and call
CALLBACK.  Its output is in the *lispm-desktop-term build* buffer."
  (interactive)
  (unless (executable-find "cargo")
    (user-error "Building lispm-desktop-term needs Rust's Cargo"))
  (unless (process-live-p lispm-desktop-term--building)
    (let ((default-directory (file-name-as-directory lispm-desktop-term-source-directory))
          (buffer (get-buffer-create "*lispm-desktop-term build*")))
      (with-current-buffer buffer (erase-buffer))
      (message "Building lispm-desktop-term…")
      (setq lispm-desktop-term--building
            (make-process
             :name "lispm-desktop-term build" :buffer buffer :noquery t
             :command '("cargo" "build" "--release")
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (if (and (= (process-exit-status process) 0) (lispm-desktop-term--module-file))
                     (progn
                       ;; Cargo leaves a library it found current untouched.
                       (set-file-times (lispm-desktop-term--module-file))
                       (if (featurep 'lispm-desktop-term-module)
                           (message "lispm-desktop-term is rebuilt: restart Emacs to use it")
                         (lispm-desktop-term-load)
                         (message "lispm-desktop-term is built"))
                       (when callback (funcall callback)))
                   (message "lispm-desktop-term could not be built: see *lispm-desktop-term build*")))))))))

(defun lispm-desktop-term-ready-p ()
  "Return non-nil when terminals run in lispm-desktop-term: it is enabled, the
module is loaded, and Emacs can wake on its channel."
  (and lispm-desktop-term-enabled (lispm-desktop-term-load) (lispm-desktop-term--channel) t))

;;;; Terminals and their buffers

(defvar-local lispm-desktop-term--terminal nil
  "The module's terminal shown in this buffer.")

(defvar-local lispm-desktop-term--id nil
  "The id of this buffer's terminal, as the channel names it.")

(defvar-local lispm-desktop-term--size nil
  "The terminal's size as last set: (COLUMNS . LINES).")

(defvar-local lispm-desktop-term--exit-code nil
  "The exit code of the terminal's program, once it exited.")

(defvar-local lispm-desktop-term-title nil
  "The title the terminal's program last set, or nil.")

(defvar lispm-desktop-term--buffers (make-hash-table)
  "The buffer of each terminal, by id.")

(defvar lispm-desktop-term-exit-functions nil
  "Functions called with a terminal's buffer and exit code when its program
exits, as `eat-exit-hook' is for Eat.")

(defvar lispm-desktop-term-title-functions nil
  "Functions called with a terminal's buffer when its program sets a title.")

(defun lispm-desktop-term-buffer-p (&optional buffer)
  "Return non-nil when BUFFER, by default the current one, is a terminal.
That is a terminal of lispm-desktop-term, not of Eat."
  (and (buffer-live-p (or buffer (current-buffer)))
       (buffer-local-value 'lispm-desktop-term--terminal (or buffer (current-buffer)))
       t))

(defun lispm-desktop-term-live-p (&optional buffer)
  "Return non-nil while BUFFER's terminal program runs."
  (when-let* ((terminal (and (lispm-desktop-term-buffer-p buffer)
                             (buffer-local-value 'lispm-desktop-term--terminal (or buffer (current-buffer))))))
    (lispm-desktop-term-module-alive-p terminal)))

(defun lispm-desktop-term-running-buffers ()
  "Return the buffers of the terminals whose programs run.
Their programs are not Emacs's processes: `process-list' leaves them out."
  (seq-filter #'lispm-desktop-term-live-p (hash-table-values lispm-desktop-term--buffers)))

(defun lispm-desktop-term-pid (&optional buffer)
  "Return the process id of BUFFER's terminal program, or nil."
  (when-let* ((terminal (and (lispm-desktop-term-buffer-p buffer)
                             (buffer-local-value 'lispm-desktop-term--terminal (or buffer (current-buffer))))))
    (lispm-desktop-term-module-pid terminal)))

(defun lispm-desktop-term-modes (&optional buffer)
  "Return the modes of BUFFER's terminal, see `lispm-desktop-term-module-modes'."
  (with-current-buffer (or buffer (current-buffer))
    (if lispm-desktop-term--terminal (lispm-desktop-term-module-modes lispm-desktop-term--terminal) 0)))

(defun lispm-desktop-term-send-string (string &optional buffer paste)
  "Send STRING to BUFFER's terminal program; PASTE sends it as pasted, in
brackets when the program asked for them."
  (with-current-buffer (or buffer (current-buffer))
    (when lispm-desktop-term--terminal
      (lispm-desktop-term-module-input
       lispm-desktop-term--terminal
       (if (and paste (/= 0 (logand 2 (lispm-desktop-term-modes))))
           (concat "\e[200~" string "\e[201~")
         string)))))

(defun lispm-desktop-term--kill-buffer ()
  "Stop the terminal's program as its buffer is killed."
  (when lispm-desktop-term--terminal
    (lispm-desktop-term-module-kill lispm-desktop-term--terminal)
    (remhash lispm-desktop-term--id lispm-desktop-term--buffers)))

;;;; Colours

(defvar lispm-desktop-term--faces (make-hash-table :test #'equal)
  "Faces by cell colours and flags: (FOREGROUND BACKGROUND FLAGS).")

(defvar lispm-desktop-term--palette nil
  "The 16 colours of the theme's terminal palette, as #RRGGBB strings.")

(defun lispm-desktop-term--palette ()
  "Return the theme's 16 terminal colours, from `ansi-color'."
  (or lispm-desktop-term--palette
      (setq lispm-desktop-term--palette
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

(defun lispm-desktop-term--theme-changed (&rest _)
  "Forget the colours, so terminals take the new theme's."
  (setq lispm-desktop-term--palette nil)
  (clrhash lispm-desktop-term--faces)
  (maphash (lambda (_id buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (lispm-desktop-term--tell-colors)
                 (lispm-desktop-term--render t))))
           lispm-desktop-term--buffers))

(defun lispm-desktop-term--color (code)
  "Return the colour of face CODE, see the module's `color_code', or nil."
  (cond ((< code 0) nil)
        ((< code 16) (aref (lispm-desktop-term--palette) code))
        ((< code 232)
         (let* ((i (- code 16))
                (level (lambda (v) (if (= v 0) 0 (+ 55 (* v 40))))))
           (format "#%02x%02x%02x" (funcall level (/ i 36)) (funcall level (% (/ i 6) 6))
                   (funcall level (% i 6)))))
        ((< code 256) (let ((v (+ 8 (* (- code 232) 10)))) (format "#%02x%02x%02x" v v v)))
        (t (format "#%06x" (logand code #xffffff)))))

(defun lispm-desktop-term--face (fg bg flags)
  "Return the face of a cell coloured FG on BG with FLAGS."
  (let ((key (list fg bg flags)))
    (or (gethash key lispm-desktop-term--faces)
        (puthash
         key
         (let* ((foreground (lispm-desktop-term--color fg))
                (background (lispm-desktop-term--color bg))
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
         lispm-desktop-term--faces))))

(defun lispm-desktop-term--tell-colors ()
  "Tell the terminal the colours it answers programs asking: the palette,
then the default foreground and background, by which Claude Code and
Codex choose a light or a dark look."
  (when lispm-desktop-term--terminal
    (let ((hex (lambda (color fallback)
                 (if (and color (color-defined-p color))
                     (apply #'format "#%02x%02x%02x"
                            (mapcar (lambda (c) (round (* 255 c))) (color-name-to-rgb color)))
                   fallback))))
      (lispm-desktop-term-module-set-colors
       lispm-desktop-term--terminal
       (vconcat (lispm-desktop-term--palette)
                (list (funcall hex (face-foreground 'default nil t) "#ffffff")
                      (funcall hex (face-background 'default nil t) "#000000")))))))

;;;; Drawing

(defun lispm-desktop-term--propertize (text runs)
  "Return TEXT with the faces of RUNS, see the module's `render'."
  (let ((i 0) (n (length runs)))
    (while (< i n)
      (let ((start (aref runs i)) (end (aref runs (1+ i))))
        (when (<= end (length text))
          (put-text-property start end 'face
                             (lispm-desktop-term--face (aref runs (+ i 2)) (aref runs (+ i 3))
                                                       (aref runs (+ i 4)))
                             text)))
      (setq i (+ i 5)))
    text))

(defvar-local lispm-desktop-term--lines nil
  "How many lines the buffer holds, the screen's height when last drawn.")

(defun lispm-desktop-term--ensure-lines (lines)
  "Make the buffer hold LINES lines, blank when it held another number.
Only the terminal writes the buffer, so the count is kept rather than
counted."
  (unless (eql lispm-desktop-term--lines lines)
    (erase-buffer)
    (insert (make-string (max 0 (1- lines)) ?\n))
    (setq lispm-desktop-term--lines lines)))

(defun lispm-desktop-term--render (&optional full)
  "Draw the changed lines of the current buffer's terminal, every line when
FULL, and place the cursor.  A terminal whose program exited keeps its
last screen and the notice under it."
  (when (and lispm-desktop-term--terminal (not lispm-desktop-term--exit-code))
    (pcase-let* ((`[,lines ,cursor-line ,cursor-column ,shape ,changed]
                  (lispm-desktop-term-module-render lispm-desktop-term--terminal full))
                 (inhibit-read-only t)
                 (inhibit-modification-hooks t)
                 (buffer-undo-list t))
      (save-restriction
        (widen)
        (lispm-desktop-term--ensure-lines lines)
        (let ((line 0))
          (goto-char (point-min))
          (seq-doseq (entry changed)
            (pcase-let ((`[,index ,text ,runs] entry))
              (forward-line (- index line))
              (setq line index)
              (delete-region (point) (line-end-position))
              (insert (lispm-desktop-term--propertize text runs))
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

(defun lispm-desktop-term--happen (buffer)
  "Take in what happened in BUFFER's terminal: titles, exits, clipboard."
  (with-current-buffer buffer
    (seq-doseq (happening (lispm-desktop-term-module-happenings lispm-desktop-term--terminal))
      (pcase happening
        (`(title . ,title)
         (setq lispm-desktop-term-title title)
         (run-hook-with-args 'lispm-desktop-term-title-functions buffer)
         (force-mode-line-update t))
        (`(reset-title . ,_) (setq lispm-desktop-term-title nil))
        (`(clipboard . ,text) (kill-new text))
        (`(exit . ,code)
         ;; Its last screen is drawn once, shown or not, and kept.
         (lispm-desktop-term--render t)
         (setq lispm-desktop-term--exit-code code)
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (point-max))
             (insert (propertize (format "\n[Process exited%s]"
                                         (if (= code 0) "" (format " with code %d" code)))
                                 'face 'shadow))))
         (run-hook-with-args 'lispm-desktop-term-exit-functions buffer code))))))

;;;; The scheduler
;;
;; The module wakes Emacs through a pipe with the ids of terminals that
;; have something new: output for the shown ones, happenings for any.
;; Waking only marks them; a frame then draws them, the selected window's
;; terminal first, until the frame's budget is spent or a key is waiting,
;; and leaves the rest to the next frame.

(defvar lispm-desktop-term--channel nil
  "The pipe process the module wakes Emacs through.")

(defvar lispm-desktop-term--pending nil
  "Ids of terminals woken and not yet drawn.")

(defvar lispm-desktop-term--timer nil
  "Timer of the next frame, while one is due.")

(defvar lispm-desktop-term--last-frame 0
  "When the last frame was drawn, a `float-time'.")

(defun lispm-desktop-term--channel ()
  "Return the pipe the module wakes Emacs through, made when missing."
  (or (and (process-live-p lispm-desktop-term--channel) lispm-desktop-term--channel)
      (when (featurep 'lispm-desktop-term-module)
        (let ((process (make-pipe-process :name "lispm-desktop-term" :noquery t
                                          :coding 'binary :filter #'lispm-desktop-term--wake)))
          (lispm-desktop-term-module-init-channel process)
          (setq lispm-desktop-term--channel process)))))

(defun lispm-desktop-term--wake (_process string)
  "Mark the terminals named in STRING, lines of ids, and schedule a frame."
  (dolist (id (split-string string "\n" t))
    (cl-pushnew (string-to-number id) lispm-desktop-term--pending))
  (lispm-desktop-term--schedule))

(defun lispm-desktop-term--schedule ()
  "Schedule the next frame, no sooner than a frame interval after the last.
The interval is `lispm-desktop-term-frame-interval'."
  (unless lispm-desktop-term--timer
    (setq lispm-desktop-term--timer
          (run-with-timer (max 0 (- (+ lispm-desktop-term--last-frame lispm-desktop-term-frame-interval)
                                    (float-time)))
                          nil #'lispm-desktop-term--tick))))

(defun lispm-desktop-term--order (ids)
  "Return IDS in the order they are drawn: the selected window's first."
  (let ((selected (buffer-local-value 'lispm-desktop-term--id (window-buffer (selected-window)))))
    (if (memq selected ids) (cons selected (delq selected ids)) ids)))

(defun lispm-desktop-term--tick ()
  "Draw a frame: the woken terminals, within `lispm-desktop-term-frame-budget',
giving way as soon as a key is waiting."
  (setq lispm-desktop-term--timer nil
        lispm-desktop-term--last-frame (float-time))
  (let ((ids (lispm-desktop-term--order (reverse lispm-desktop-term--pending)))
        (start (float-time))
        (drawn 0))
    (setq lispm-desktop-term--pending nil)
    (while ids
      (if (and (> drawn 0)
               (or (> (- (float-time) start) lispm-desktop-term-frame-budget) (input-pending-p)))
          (setq lispm-desktop-term--pending (append ids lispm-desktop-term--pending)
                ids nil)
        (let ((buffer (gethash (pop ids) lispm-desktop-term--buffers)))
          (when (buffer-live-p buffer)
            ;; The last output is drawn before an exit is taken in.
            (when (get-buffer-window buffer 'visible)
              (with-current-buffer buffer
                (lispm-desktop-term--render))
              (setq drawn (1+ drawn)))
            (lispm-desktop-term--happen buffer)))))
    (when lispm-desktop-term--pending
      (lispm-desktop-term--schedule))))

;;;; Showing and sizing

(defun lispm-desktop-term--window-size (window)
  "Return the terminal size WINDOW fits: (COLUMNS . LINES)."
  (with-selected-window window
    (cons (max 1 (window-max-chars-per-line window))
          (max 1 (floor (window-body-height window t) (default-line-height))))))

(defun lispm-desktop-term--follow-windows (&rest _)
  "Tell each terminal whether it is shown, and size it to its window.
A terminal shown in several windows takes the selected one's size, or
else the first's.  This runs as windows change."
  (maphash
   (lambda (_id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (let* ((windows (get-buffer-window-list buffer nil 'visible))
                (window (if (memq (selected-window) windows) (selected-window) (car windows))))
           (lispm-desktop-term-module-set-visible lispm-desktop-term--terminal (and window t))
           (when window
             (let ((size (lispm-desktop-term--window-size window)))
               (unless (equal size lispm-desktop-term--size)
                 (setq lispm-desktop-term--size size)
                 (lispm-desktop-term-module-resize lispm-desktop-term--terminal (car size) (cdr size)))))))))
   lispm-desktop-term--buffers))

;;;; Keys

(defun lispm-desktop-term--send (string)
  "Send STRING to the current terminal, as typed."
  (when lispm-desktop-term--terminal
    (lispm-desktop-term-module-input lispm-desktop-term--terminal string)))

(defun lispm-desktop-term-send-char ()
  "Send the character typed."
  (interactive)
  (lispm-desktop-term--send (string last-command-event)))

(defun lispm-desktop-term--cursor-key (letter)
  "Return the bytes of the cursor key LETTER, in application mode or not."
  (concat (if (/= 0 (logand 1 (lispm-desktop-term-modes))) "\eO" "\e[") letter))

(defconst lispm-desktop-term-keys
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

(defun lispm-desktop-term-send-key ()
  "Send the bytes of the key just typed, see `lispm-desktop-term-keys'."
  (interactive)
  (when-let* ((bytes (cdr (assoc (key-description (this-command-keys-vector)) lispm-desktop-term-keys))))
    (lispm-desktop-term--send bytes)))

(defun lispm-desktop-term-send-control ()
  "Send the control character of the Control key just typed."
  (interactive)
  (let ((event (event-basic-type last-command-event)))
    (when (characterp event)
      (lispm-desktop-term--send (string (logand event #x1f))))))

(defun lispm-desktop-term-send-meta ()
  "Send the Meta key just typed: Escape, then the key."
  (interactive)
  (let ((event (event-basic-type last-command-event))
        (modifiers (event-modifiers last-command-event)))
    (when (characterp event)
      (lispm-desktop-term--send (concat "\e" (string (if (memq 'control modifiers)
                                                          (logand event #x1f)
                                                         (if (memq 'shift modifiers) (upcase event) event))))))))

(defun lispm-desktop-term-up () "Send Up." (interactive) (lispm-desktop-term--send (lispm-desktop-term--cursor-key "A")))
(defun lispm-desktop-term-down () "Send Down." (interactive) (lispm-desktop-term--send (lispm-desktop-term--cursor-key "B")))
(defun lispm-desktop-term-right () "Send Right." (interactive) (lispm-desktop-term--send (lispm-desktop-term--cursor-key "C")))
(defun lispm-desktop-term-left () "Send Left." (interactive) (lispm-desktop-term--send (lispm-desktop-term--cursor-key "D")))

(defun lispm-desktop-term-yank ()
  "Paste the kill ring's newest text, as pasted."
  (interactive)
  (lispm-desktop-term-send-string (current-kill 0) nil t))

(defun lispm-desktop-term-scroll-back (&optional lines)
  "Scroll the terminal's view LINES into its history, back when negative."
  (interactive "p")
  (lispm-desktop-term-module-scroll lispm-desktop-term--terminal (or lines 1))
  (lispm-desktop-term--render t))

(defun lispm-desktop-term--mouse-cell (posn)
  "Return the terminal's cell at POSN, as (COLUMN . LINE) counted from 1.
It is measured in the terminal's font, which may not be the frame's."
  (pcase-let ((`(,x . ,y) (posn-x-y posn))
              (`(,columns . ,lines) (or lispm-desktop-term--size '(80 . 24))))
    (with-selected-window (posn-window posn)
      (cons (1+ (min (max (/ x (max 1 (default-font-width))) 0) (1- columns)))
            (1+ (min (max (/ y (max 1 (default-line-height))) 0) (1- lines)))))))

(defun lispm-desktop-term--mouse-report (button cell &optional modifiers release)
  "Return the report of mouse BUTTON at CELL, see `lispm-desktop-term--mouse-cell',
with the MODIFIERS of an event, pressed or else RELEASE'd, in the encoding
the program asked for: SGR's, else the original one, whose positions stop
at 95 so that the report stays ASCII and which tells no button released."
  (pcase-let ((`(,column . ,line) cell)
              (button (+ button
                         (if (memq 'shift modifiers) 4 0)
                         (if (memq 'meta modifiers) 8 0)
                         (if (memq 'control modifiers) 16 0))))
    (if (/= 0 (logand 16 (lispm-desktop-term-modes)))
        (format "\e[<%d;%d;%d%c" button column line (if release ?m ?M))
      (string ?\e ?\[ ?M (+ 32 (if release (logior 3 (logand button -4)) button))
              (+ 32 (min column 95)) (+ 32 (min line 95))))))

(defun lispm-desktop-term--wheel (event lines)
  "Scroll for the mouse wheel EVENT by LINES, as Alacritty does: a program
that asked for mouse reports is told of the wheel, one holding the whole
screen gets cursor keys, and otherwise the terminal's history scrolls.
With Shift the history always scrolls."
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (let ((modes (lispm-desktop-term-modes))
          (shift (memq 'shift (event-modifiers event))))
      (cond ((and (/= 0 (logand 8 modes)) (not shift))
             (lispm-desktop-term--send
              (apply #'concat (make-list (max 1 (event-line-count event))
                                         (lispm-desktop-term--mouse-report
                                          (if (> lines 0) 64 65)
                                          (lispm-desktop-term--mouse-cell (event-start event))
                                          (event-modifiers event))))))
            ((and (/= 0 (logand 4 modes)) (not shift))
             (lispm-desktop-term--send (apply #'concat (make-list (abs lines) (if (> lines 0) "\eOA" "\eOB")))))
            (t (lispm-desktop-term-module-scroll lispm-desktop-term--terminal lines)
               (lispm-desktop-term--render t))))))

(defun lispm-desktop-term-wheel-up (event)
  "Scroll back for mouse wheel EVENT."
  (interactive "e")
  (lispm-desktop-term--wheel event 3))

(defun lispm-desktop-term-wheel-down (event)
  "Scroll forward for mouse wheel EVENT."
  (interactive "e")
  (lispm-desktop-term--wheel event -3))

(defun lispm-desktop-term--mouse-button (event)
  "Return the number mouse EVENT's button has in mouse reports."
  (pcase (event-basic-type event) ('mouse-1 0) ('mouse-2 1) ('mouse-3 2) (_ 0)))

(defun lispm-desktop-term-mouse-press (event)
  "Tell the program of the button pressed by mouse EVENT, of the mouse's
moves while it is held when the program asked for them, and of its
release.  The press selects the terminal's window, as a click does."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (button (lispm-desktop-term--mouse-button event))
         (done nil))
    (when (window-live-p window)
      (select-window window)
      (with-current-buffer (window-buffer window)
        (let ((cell (lispm-desktop-term--mouse-cell (event-start event)))
              (moves (/= 0 (logand (logior 128 256) (lispm-desktop-term-modes)))))
          (lispm-desktop-term--send (lispm-desktop-term--mouse-report button cell))
          (track-mouse
            (while (not done)
              (let ((next (read-event)))
                (cond
                 ((mouse-movement-p next)
                  (let ((posn (event-start next)))
                    (when (and moves (eq (posn-window posn) window) (posn-x-y posn))
                      (let ((moved (lispm-desktop-term--mouse-cell posn)))
                        (unless (equal moved cell)
                          (setq cell moved)
                          (lispm-desktop-term--send (lispm-desktop-term--mouse-report (+ button 32) cell)))))))
                 ;; The release: a click, a drag, a double or triple click.
                 ((and (consp next) (memq (event-basic-type next) '(mouse-1 mouse-2 mouse-3))
                       (not (memq 'down (event-modifiers next))))
                  (let ((posn (event-end next)))
                    (when (and (eq (posn-window posn) window) (posn-x-y posn))
                      (setq cell (lispm-desktop-term--mouse-cell posn))))
                  (setq done t))
                 ;; Keyboard input ends the press; it is read again.
                 ((lispm-desktop-typed-event-p next)
                  (push next unread-command-events)
                  (setq done t)))))
            (lispm-desktop-term--send (lispm-desktop-term--mouse-report button cell nil t))))))))

(defun lispm-desktop-term--mouse-taken (command)
  "Return COMMAND while the current terminal's program takes the mouse."
  (and (/= 0 (logand 8 (lispm-desktop-term-modes))) command))

(defvar lispm-desktop-term-keys-mode)

(defun lispm-desktop-term-mouse-emacs (event)
  "Do what the mouse EVENT, which Shift was held for, does outside a
terminal: with Shift the mouse stays Emacs's, to select text, when the
program takes it."
  (interactive "e")
  (let* ((name (symbol-name (car event)))
         (plain (string-remove-prefix "S-" name))
         (command (with-current-buffer (window-buffer (posn-window (event-start event)))
                    (let ((lispm-desktop-term-keys-mode nil))
                      (seq-some (lambda (name)
                                  (key-binding (vector (intern name)) t nil (event-start event)))
                                (list name plain
                                      (replace-regexp-in-string
                                       "\\`\\(?:double\\|triple\\)-" "" plain)))))))
    (when (commandp command)
      (setq this-command command)
      (call-interactively command nil (vector event)))))

(defun lispm-desktop-term-history ()
  "Show the terminal's history and screen in a buffer of their own, to read,
search and copy."
  (interactive)
  (let ((text (lispm-desktop-term-module-text lispm-desktop-term--terminal nil))
        (name (format "*%s history*" (string-trim (buffer-name) "\\*" "\\*"))))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (special-mode)
      (goto-char (point-max)))
    (pop-to-buffer name)))

(defvar lispm-desktop-term-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'lispm-desktop-term-send-char)
    (dolist (key lispm-desktop-term-keys)
      (define-key map (kbd (car key)) #'lispm-desktop-term-send-key))
    (dolist (letter (string-to-list "abdefhijklnopqrstuvwyz"))
      (define-key map (kbd (format "C-%c" letter)) #'lispm-desktop-term-send-control))
    (dolist (key '("C-SPC" "C-]" "C-_" "C-@"))
      (define-key map (kbd key) #'lispm-desktop-term-send-control))
    (dolist (letter (string-to-list "abdefghijklnopqrstuyz0123456789.,/<>?;'[]-=`"))
      (define-key map (kbd (format "M-%c" letter)) #'lispm-desktop-term-send-meta))
    (define-key map (kbd "<up>") #'lispm-desktop-term-up)
    (define-key map (kbd "<down>") #'lispm-desktop-term-down)
    (define-key map (kbd "<right>") #'lispm-desktop-term-right)
    (define-key map (kbd "<left>") #'lispm-desktop-term-left)
    (define-key map (kbd "C-y") #'lispm-desktop-paste)
    (define-key map (kbd "M-v") #'lispm-desktop-paste)
    (define-key map (kbd "M-c") #'lispm-desktop-copy-region)
    (define-key map (kbd "M-w") #'lispm-desktop-copy-region)
    (define-key map (kbd "M-x") #'execute-extended-command)
    (define-key map (kbd "C-c C-h") #'lispm-desktop-term-history)
    (define-key map (kbd "S-<prior>") #'lispm-desktop-term-scroll-back)
    (define-key map (kbd "<wheel-up>") #'lispm-desktop-term-wheel-up)
    (define-key map (kbd "<wheel-down>") #'lispm-desktop-term-wheel-down)
    (define-key map (kbd "S-<wheel-up>") #'lispm-desktop-term-wheel-up)
    (define-key map (kbd "S-<wheel-down>") #'lispm-desktop-term-wheel-down)
    ;; Clicks go to a program that takes the mouse; with Shift, and when it
    ;; does not, they are Emacs's.
    (dolist (button '(1 2 3))
      (dolist (count '("" "double-" "triple-"))
        (define-key map (vector (intern (format "%sdown-mouse-%d" count button)))
                    `(menu-item "" lispm-desktop-term-mouse-press :filter lispm-desktop-term--mouse-taken))
        (dolist (kind '("" "drag-"))
          (define-key map (vector (intern (format "%s%smouse-%d" count kind button)))
                      `(menu-item "" ignore :filter lispm-desktop-term--mouse-taken)))
        (dolist (kind '("down-" "" "drag-"))
          (define-key map (vector (intern (format "S-%s%smouse-%d" count kind button)))
                      #'lispm-desktop-term-mouse-emacs))))
    (define-key map (kbd "<mouse-4>") #'lispm-desktop-term-wheel-up)
    (define-key map (kbd "<mouse-5>") #'lispm-desktop-term-wheel-down)
    map)
  "Keys of a terminal.  Printable keys, Control and Meta keys and the
special keys reach the program; C-x, M-x, C-g, the copy and paste keys
and the C-c prefix stay Emacs's: C-c C-c sends C-c, C-c C-h shows the
history.  The keymap outranks Evil's, see `lispm-desktop-term-keys-mode'.")

(define-minor-mode lispm-desktop-term-keys-mode
  "Give a terminal its keys, over every other keymap."
  :init-value nil
  :lighter nil
  :keymap lispm-desktop-term-keys-mode-map)

(defvar lispm-desktop-term-keys-mode-map-alist
  `((lispm-desktop-term-keys-mode . ,lispm-desktop-term-keys-mode-map)))

(defun lispm-desktop-term--keys-first ()
  "Put the terminal keymap first among the emulation keymaps, over Evil's."
  (setq emulation-mode-map-alists
        (cons 'lispm-desktop-term-keys-mode-map-alist
              (delq 'lispm-desktop-term-keys-mode-map-alist emulation-mode-map-alists))))

;;;; The mode

(defvar lispm-desktop-terminal-font-family)

(define-derived-mode lispm-desktop-term-mode fundamental-mode "Term"
  "Major mode of a terminal emulated off Emacs's thread, see lispm-desktop-term.el.
\\{lispm-desktop-term-keys-mode-map}"
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
              lispm-desktop-paste-function (lambda (text) (lispm-desktop-term-send-string text nil t))
              left-fringe-width 0
              first-change-hook nil
              global-hl-line-mode nil)
  (visual-line-mode -1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (and (boundp 'lispm-desktop-terminal-font-family) (display-graphic-p)
             (member lispm-desktop-terminal-font-family (font-family-list)))
    (face-remap-add-relative 'default :family lispm-desktop-terminal-font-family))
  (add-hook 'kill-buffer-hook #'lispm-desktop-term--kill-buffer nil t)
  (lispm-desktop-term--keys-first)
  (lispm-desktop-term-keys-mode 1))

(defun lispm-desktop-term-make (name program &optional args directory)
  "Run PROGRAM with ARGS in a terminal buffer named *NAME*, in DIRECTORY or
`default-directory', with `process-environment'; return the buffer.
A buffer of that name still running is returned as it is."
  (let ((buffer (get-buffer-create (format "*%s*" name))))
    (unless (lispm-desktop-term-live-p buffer)
      (unless (lispm-desktop-term-ready-p)
        (error "lispm-desktop-term is not built; see `lispm-desktop-term-build'"))
      (with-current-buffer buffer
        (lispm-desktop-term-mode)
        (let* ((directory (expand-file-name (or directory default-directory)))
               (environment (append '("TERM=xterm-256color" "COLORTERM=truecolor")
                                    (seq-remove (lambda (entry)
                                                  (string-match-p "\\`\\(TERM\\|COLORTERM\\|INSIDE_EMACS\\)=" entry))
                                                process-environment)
                                    (list (format "INSIDE_EMACS=%s,lispm-desktop-term" emacs-version))))
               ;; A terminal not shown yet starts the size of the selected
               ;; window, where it most likely shows: a resize just after
               ;; starting catches some programs mid-setup.
               (window (or (get-buffer-window buffer 'visible)
                           (unless (window-minibuffer-p) (selected-window))))
               (size (if window (lispm-desktop-term--window-size window) '(80 . 24)))
               ;; Backspace sends DEL, as the terminal's erase character.
               (command (list "/bin/sh" "-c"
                              (format "stty erase '^?' 2>/dev/null; exec %s"
                                      (mapconcat #'shell-quote-argument (cons program args) " ")))))
          (setq default-directory (file-name-as-directory directory)
                lispm-desktop-term--size size
                lispm-desktop-term--exit-code nil
                lispm-desktop-term--terminal (lispm-desktop-term-module-spawn (car command) (vconcat (cdr command))
                                                                              directory (vconcat environment)
                                                                              (car size) (cdr size))
                lispm-desktop-term--id (lispm-desktop-term-module-id lispm-desktop-term--terminal))
          (puthash lispm-desktop-term--id buffer lispm-desktop-term--buffers)
          (lispm-desktop-term--tell-colors)
          (let ((inhibit-read-only t))
            (setq lispm-desktop-term--lines nil)
            (lispm-desktop-term--ensure-lines (cdr size))))))
    (lispm-desktop-term--follow-windows)
    buffer))

(defun lispm-desktop-term-kill (&optional buffer)
  "Stop BUFFER's terminal program."
  (with-current-buffer (or buffer (current-buffer))
    (when lispm-desktop-term--terminal
      (lispm-desktop-term-module-kill lispm-desktop-term--terminal))))

(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-term-send-string-as-yank "eat" (terminal args))
(defvar eat-terminal)

(defun lispm-desktop-terminal-make (name program &rest args)
  "Run PROGRAM with ARGS in a terminal named *NAME*; return its buffer.
It is a lispm-desktop-term once the module is built, else an Eat terminal."
  (if (lispm-desktop-term-ready-p)
      (lispm-desktop-term-make name program args)
    (require 'eat)
    (apply #'eat-make name program nil args)))

(defun lispm-desktop-terminal-live-p (buffer)
  "Return non-nil while the program of terminal BUFFER runs."
  (if (lispm-desktop-term-buffer-p buffer)
      (lispm-desktop-term-live-p buffer)
    (when-let* ((process (and (buffer-live-p buffer) (get-buffer-process buffer))))
      (process-live-p process))))

(defun lispm-desktop-terminal-paste (buffer text)
  "Paste TEXT into the program of terminal BUFFER, as pasted."
  (if (lispm-desktop-term-buffer-p buffer)
      (lispm-desktop-term-send-string text buffer t)
    (with-current-buffer buffer
      (eat-term-send-string-as-yank eat-terminal (list text)))))

(defun lispm-desktop-setup-term ()
  "Draw terminals as their windows change and the theme does.  The module
is built in the background the first time, and again when its sources
change, where Rust is installed."
  (add-hook 'window-buffer-change-functions #'lispm-desktop-term--follow-windows)
  (add-hook 'window-size-change-functions #'lispm-desktop-term--follow-windows)
  (add-hook 'window-selection-change-functions #'lispm-desktop-term--follow-windows)
  (add-hook 'enable-theme-functions #'lispm-desktop-term--theme-changed 90)
  (when (and lispm-desktop-term-enabled (display-graphic-p) (not (featurep 'lispm-desktop-term-module))
             (or (null (lispm-desktop-term--module-file)) (lispm-desktop-term--stale-p))
             (executable-find "cargo"))
    (lispm-desktop-term-build)))

(provide 'lispm-desktop-term)
;;; lispm-desktop-term.el ends here
