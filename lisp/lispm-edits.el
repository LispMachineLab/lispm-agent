;;; lispm-edits.el --- Agents' edits, shown where they happen  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; An agent's changes show in the buffers it changes.  When a tool
;; changes a buffer, the buffer comes into view at the change: the lines
;; removed linger, struck through, and fade; the new ones are typed in
;; behind a cursor bearing the agent's name, and glow for a moment.
;; While a call that would change a buffer awaits approval, the change it
;; proposes is laid over the buffer where it would happen; and while the
;; model is still writing such a call, the proposal grows as its text
;; streams in.
;;
;; All of it is display, overlays over the text: the text is only ever
;; what the tools make it, and a change shown has been made and saved.
;; Tools take part through their :changes function (see `lispm-deftool'),
;; which tells what a call would make of which buffer.  Set
;; `lispm-show-edits' to nil to see none of it.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'lispm-core)
(require 'lispm-tools)

(declare-function lispm--asked "lispm-runtime" (call))

(defgroup lispm-edits nil
  "Agents' edits, shown in the buffers they change."
  :group 'lispm
  :prefix "lispm-edits-")

(defcustom lispm-show-edits t
  "Whether agents' edits show in the buffers they change.
Changes are typed in as they are made; proposed changes are laid over
the buffer while the model writes them and while they await approval."
  :type 'boolean)

(defcustom lispm-edits-follow t
  "Whether a buffer an agent changes comes into view at the change.
It takes the place of another file in view, else a window is split off
for it; the selected window keeps the focus, and a chat stays in view.
When nil, only changes to buffers already in view show."
  :type 'boolean)

(defcustom lispm-edits-speed 900
  "How fast a change is typed in, in characters per second."
  :type 'number)

(defcustom lispm-edits-max-duration 1.5
  "The longest a change takes to be typed in, in seconds."
  :type 'number)

(defcustom lispm-edits-fade 1.2
  "Seconds over which new text stops glowing and removed text fades."
  :type 'number)

(defcustom lispm-edits-colors
  '("#f5a97f" "#8bd5ca" "#c6a0f6" "#eed49f" "#91d7e3" "#f0c6c6")
  "Colors of the agents' cursors; each agent has one of them."
  :type '(repeat color))

(defface lispm-edits-added
  '((((class color) (min-colors 88) (background dark)) :background "#1d4a2a" :extend t)
    (((class color) (min-colors 88) (background light)) :background "#cdf2d4" :extend t)
    (t :inherit diff-added))
  "New text an agent just typed in; it fades to the usual background.")

(defface lispm-edits-removed
  '((((class color) (min-colors 88) (background dark))
     :background "#5a1f26" :strike-through t :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f7cdd1" :strike-through t :extend t)
    (t :inherit diff-removed :strike-through t))
  "Text an agent just removed, lingering for a moment.")

(defface lispm-edits-proposed-added
  '((t :inherit lispm-edits-added :slant italic))
  "Text that a call being written, or awaiting approval, would add.")

(defface lispm-edits-proposed-removed
  '((t :inherit lispm-edits-removed))
  "Text that a call being written, or awaiting approval, would remove.")

(defface lispm-edits-note
  '((t :inherit shadow :slant italic))
  "Notes among proposed text, such as how many lines are not shown.")

(defvar lispm-edits--in-batch nil
  "Non-nil to show edits in batch mode too, as the tests do.")

(defun lispm-edits--live-p ()
  "Non-nil if edits are to be shown."
  (and lispm-show-edits (or (not noninteractive) lispm-edits--in-batch)))

;;;; Lines that changed

(defun lispm-edits--line-starts (text)
  "Offsets in TEXT at which its lines start, and its length last."
  (let ((starts (list 0)) (at 0))
    (while (setq at (string-search "\n" text at))
      (setq at (1+ at))
      (push at starts))
    (unless (= (car starts) (length text))
      (push (length text) starts))
    (vconcat (nreverse starts))))

(defun lispm-edits--lines (text starts)
  "The lines of TEXT, which start at STARTS, as a vector of strings."
  (let ((lines (make-vector (1- (length starts)) nil)))
    (dotimes (i (length lines))
      (aset lines i (substring text (aref starts i) (aref starts (1+ i)))))
    lines))

(defun lispm-edits--hunks (old new)
  "Where the lines of NEW differ from those of OLD, as a list of hunks.
A hunk is (OLD-FROM OLD-TO NEW-FROM NEW-TO): the character offsets of
the lines OLD has there, and of the lines NEW has instead."
  (let* ((os (lispm-edits--line-starts old))
         (ns (lispm-edits--line-starts new))
         (a (lispm-edits--lines old os))
         (b (lispm-edits--lines new ns))
         (n (length a)) (m (length b)) (p 0) (q 0))
    (while (and (< p n) (< p m) (equal (aref a p) (aref b p)))
      (cl-incf p))
    (while (and (< q (- n p)) (< q (- m p))
                (equal (aref a (- n q 1)) (aref b (- m q 1))))
      (cl-incf q))
    (mapcar (pcase-lambda (`(,i1 ,i2 ,j1 ,j2))
              (list (aref os i1) (aref os i2) (aref ns j1) (aref ns j2)))
            (lispm-edits--line-hunks a b p (- n q) p (- m q)))))

(defun lispm-edits--line-hunks (a b i0 i1 j0 j1)
  "Hunks of line indexes between lines I0 to I1 of A and J0 to J1 of B.
Lines both have in the same order, as many as possible, are kept;
the others make the hunks, as (I J I2 J2)."
  (let ((n (- i1 i0)) (m (- j1 j0)))
    (cond
     ((and (zerop n) (zerop m)) nil)
     ;; Nothing in common, or too much to compare: one hunk.
     ((or (zerop n) (zerop m) (> (* n m) 250000)) (list (list i0 i1 j0 j1)))
     (t
      ;; The longest common subsequence, from the end backwards.
      (let* ((w (1+ m))
             (len (make-vector (* (1+ n) w) 0))
             (same (lambda (i j) (equal (aref a (+ i0 i)) (aref b (+ j0 j)))))
             (at (lambda (i j) (aref len (+ (* i w) j)))))
        (cl-loop for i from (1- n) downto 0 do
                 (cl-loop for j from (1- m) downto 0 do
                          (aset len (+ (* i w) j)
                                (if (funcall same i j)
                                    (1+ (funcall at (1+ i) (1+ j)))
                                  (max (funcall at (1+ i) j) (funcall at i (1+ j)))))))
        (let ((i 0) (j 0) start hunks)
          (while (or (< i n) (< j m))
            (if (and (< i n) (< j m) (funcall same i j))
                (progn
                  (when start
                    (push (list (car start) (+ i0 i) (cdr start) (+ j0 j)) hunks)
                    (setq start nil))
                  (cl-incf i)
                  (cl-incf j))
              (unless start (setq start (cons (+ i0 i) (+ j0 j))))
              (if (and (< i n) (or (>= j m) (>= (funcall at (1+ i) j) (funcall at i (1+ j)))))
                  (cl-incf i)
                (cl-incf j))))
          (when start
            (push (list (car start) i1 (cdr start) j1) hunks))
          (nreverse hunks)))))))

;;;; Input still being written

(defun lispm-edits--partial-json (json)
  "The members of the JSON object that the string JSON begins, as an alist.
A string value cut short counts as far as it goes; a member whose value
has not started, or is otherwise cut short, is left out."
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (let (members)
      (catch 'end
        (skip-chars-forward " \t\n\r")
        (unless (eq (char-after) ?{) (throw 'end nil))
        (forward-char)
        (while t
          (skip-chars-forward " \t\n\r,")
          (unless (eq (char-after) ?\") (throw 'end nil))
          (let ((key (lispm-edits--json-string t)))
            (unless key (throw 'end nil))
            (skip-chars-forward " \t\n\r")
            (unless (eq (char-after) ?:) (throw 'end nil))
            (forward-char)
            (skip-chars-forward " \t\n\r")
            (cond
             ((eobp) (throw 'end nil))
             ((eq (char-after) ?\")
              (let ((value (lispm-edits--json-string nil)))
                (when value (push (cons (intern key) value) members))
                (when (eobp) (throw 'end nil))))
             (t
              (let ((value (condition-case nil
                               (json-parse-buffer :object-type 'alist :array-type 'array
                                                  :null-object :null :false-object :false)
                             (error (throw 'end nil)))))
                ;; A number at the very end may go on.
                (when (and (eobp) (numberp value)) (throw 'end nil))
                (push (cons (intern key) value) members)))))))
      (nreverse members))))

(defun lispm-edits--json-string (whole)
  "Read the JSON string at point and move past it.
If it is cut short, return what it holds so far, or nil if WHOLE."
  (let ((start (point)) end)
    (forward-char)
    (while (and (not end) (re-search-forward "[\"\\]" nil t))
      (if (eq (char-before) ?\")
          (setq end (point))
        (unless (eobp) (forward-char))))
    (cond
     (end (json-parse-string (buffer-substring-no-properties start end)))
     (whole (goto-char (point-max)) nil)
     (t
      (goto-char (point-max))
      (let ((raw (buffer-substring-no-properties (1+ start) (point-max))))
        ;; Drop an escape sequence cut in two.
        (when (string-match "\\(\\\\+\\)\\(u[0-9a-fA-F]\\{0,3\\}\\)?\\'" raw)
          (when (cl-oddp (- (match-end 1) (match-beginning 1)))
            (setq raw (substring raw 0 (1- (match-end 1))))))
        (condition-case nil
            (json-parse-string (concat "\"" raw "\""))
          (error nil)))))))

;;;; Showing

(defun lispm-edits--color (agent)
  "The color of AGENT's cursor."
  (if (and agent lispm-edits-colors)
      (nth (mod (sxhash-equal (symbol-name (lispm-agent-name agent))) (length lispm-edits-colors))
           lispm-edits-colors)
    (or (face-foreground 'cursor nil t) "gray")))

(defun lispm-edits--tag (agent &optional note)
  "AGENT's cursor: a bar, then its name, and NOTE, in its color."
  (let ((color (lispm-edits--color agent)))
    (concat (propertize " " 'face `(:background ,color))
            (propertize " " 'face 'default)
            (propertize (concat " λ " (if agent (symbol-name (lispm-agent-name agent)) "LispM")
                                (if note (concat " · " note) "")
                                " ")
                        'face `(:background ,color :foreground "#1e1e2e"
                                            :weight bold :height 0.85)))))

(defun lispm-edits--blend (from to amount)
  "The color AMOUNT (0 to 1) of the way from color FROM to TO, or nil."
  (let ((a (and (stringp from) (color-name-to-rgb from)))
        (b (and (stringp to) (color-name-to-rgb to))))
    (when (and a b)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (x y) (+ x (* (max 0 (min 1 amount)) (- y x)))) a b)
                     '(2))))))

(defun lispm-edits--window (buffer)
  "The window showing BUFFER; it comes into view if `lispm-edits-follow'.
It takes the place of another file in view, else a window is split off
for it.  It never covers the selected window, a side window, or a
chat: that is where calls are approved."
  (or (get-buffer-window buffer 'visible)
      (and lispm-edits-follow
           (when-let* ((window (or (lispm-edits--spare-window) (lispm-edits--split-window))))
             ;; Not `display-buffer': display rules that put files where
             ;; the user works could put this one over the chat.
             (record-window-buffer window)
             (set-window-buffer window buffer)
             window))))

(defun lispm-edits--spare-window ()
  "A window of the selected frame showing a file, the least recently used last."
  (car (sort (seq-filter (lambda (window)
                           (and (not (eq window (selected-window)))
                                (not (window-dedicated-p window))
                                (not (window-parameter window 'window-side))
                                (buffer-file-name (window-buffer window))))
                         (window-list nil 'nomini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun lispm-edits--split-window ()
  "A new window, split off the largest one of the selected frame.
Off a chat only if there is nothing else: a narrow chat reads badly."
  (let* ((windows (seq-remove (lambda (window)
                                (or (window-dedicated-p window)
                                    (window-parameter window 'window-side)))
                              (window-list nil 'nomini)))
         (others (seq-remove (lambda (window)
                               (provided-mode-derived-p
                                (buffer-local-value 'major-mode (window-buffer window))
                                'lispm-chat-mode))
                             windows))
         (window (car (sort (or others windows)
                            (lambda (a b) (> (* (window-total-width a) (window-total-height a))
                                             (* (window-total-width b) (window-total-height b))))))))
    (when window
      (or (ignore-errors (split-window window nil 'right))
          (ignore-errors (split-window window nil 'below))))))

(defun lispm-edits--reveal (window position)
  "Show POSITION in WINDOW, unless WINDOW is where the user works."
  (when (and (window-live-p window) (not (eq window (selected-window))))
    (unless (pos-visible-in-window-p position window)
      (set-window-start window (with-current-buffer (window-buffer window)
                                 (save-excursion
                                   (goto-char position)
                                   (forward-line -3)
                                   (point)))))
    (set-window-point window position)))

(defun lispm-edits--overlay (beg end &rest properties)
  "A new overlay from BEG to END with PROPERTIES."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'lispm-edits t)
    (while properties
      (overlay-put overlay (pop properties) (pop properties)))
    overlay))

;;;; Changes made

(cl-defstruct (lispm-edits--show (:constructor lispm-edits--show-create) (:copier nil))
  "A change to BUFFER being shown: typed in, then fading.
Typing it takes TYPE seconds from START (none for a change seen as a
proposal).  HUNKS are (BEG END REMOVED LENGTH): markers around new
text, the text it replaced, and its length, TOTAL in all."
  buffer agent window start type hunks total overlays)

(defvar lispm-edits--shows nil
  "Changes being shown.")

(defvar lispm-edits--timer nil
  "Timer drawing the changes being shown.")

(defvar-local lispm-edits--seen nil
  "When a proposed change to this buffer was made: it need not be typed in.")

(defun lispm-edits--changed (buffer old new)
  "Show the change a tool just made to BUFFER, from OLD to NEW."
  (when (and (lispm-edits--live-p) (buffer-live-p buffer))
    (lispm-edits--finish buffer)
    (when-let* ((hunks (lispm-edits--hunks old new))
                (window (lispm-edits--window buffer)))
      (with-current-buffer buffer
        (let* ((seen (and lispm-edits--seen (< (- (float-time) lispm-edits--seen) 10)))
               (hunks (mapcar (pcase-lambda (`(,of ,ot ,nf ,nt))
                                (list (copy-marker (1+ nf)) (copy-marker (1+ nt))
                                      (substring old of ot) (- nt nf)))
                              hunks))
               (total (apply #'+ (mapcar (lambda (h) (nth 3 h)) hunks)))
               (show (lispm-edits--show-create
                      :buffer buffer :agent lispm-current-agent :window window
                      :start (float-time) :hunks hunks :total total
                      :type (if (or seen (zerop total))
                                0
                              (min lispm-edits-max-duration
                                   (/ total (float lispm-edits-speed)))))))
          (setq lispm-edits--seen nil)
          (add-hook 'before-change-functions #'lispm-edits--interrupt nil t)
          (lispm-edits--reveal window (marker-position (caar hunks)))
          (push show lispm-edits--shows)
          (lispm-edits--draw show (float-time))
          (unless (timerp lispm-edits--timer)
            (setq lispm-edits--timer (run-at-time 0 (/ 1.0 30) #'lispm-edits--tick))))))))

(defun lispm-edits--tick (&optional now)
  "Draw the changes being shown as they are at NOW (default, the time)."
  (let ((now (or now (float-time))))
    (setq lispm-edits--shows
          (seq-filter (lambda (show)
                        (condition-case err
                            (lispm-edits--draw show now)
                          (error (message "LispM: showing a change: %S" err)
                                 (lispm-edits--drop show)
                                 nil)))
                      lispm-edits--shows))
    (when (and (null lispm-edits--shows) (timerp lispm-edits--timer))
      (cancel-timer lispm-edits--timer)
      (setq lispm-edits--timer nil))))

(defun lispm-edits--draw (show now)
  "Draw SHOW as it is at time NOW; return nil once it is over."
  (let* ((buffer (lispm-edits--show-buffer show))
         (elapsed (- now (lispm-edits--show-start show)))
         (type (lispm-edits--show-type show)))
    (mapc #'delete-overlay (lispm-edits--show-overlays show))
    (setf (lispm-edits--show-overlays show) nil)
    (if (or (not (buffer-live-p buffer)) (>= elapsed (+ type lispm-edits-fade)))
        (progn (lispm-edits--drop show) nil)
      (with-current-buffer buffer
        (let* ((typed (if (> type 0) (min 1.0 (/ elapsed type)) 1.0))
               (left (round (* typed (lispm-edits--show-total show))))
               (background (face-background 'default nil t))
               (glow (lispm-edits--blend (face-background 'lispm-edits-added nil t) background
                                         (if (< elapsed type) 0 (/ (- elapsed type) lispm-edits-fade))))
               (lingering (max 0.0 (- 1 (/ elapsed (max 0.4 (+ type (* 0.5 lispm-edits-fade)))))))
               (tag (lispm-edits--tag (lispm-edits--show-agent show)))
               cursor overlays)
          (pcase-dolist (`(,beg ,end ,removed ,length) (lispm-edits--show-hunks show))
            (let* ((shown (min length left))
                   (at (+ beg shown)))
              (setq left (- left shown))
              (when (and (> lingering 0) (not (string-empty-p removed)))
                (push (lispm-edits--overlay beg beg 'before-string
                                            (lispm-edits--ghost removed lingering))
                      overlays))
              (when (and glow (> shown 0))
                (push (lispm-edits--overlay beg at 'face `(:background ,glow :extend t)) overlays))
              (when (< shown length)
                ;; Not typed in yet: hidden, behind the cursor where typing is.
                (let ((here (not cursor)))
                  (when here (setq cursor at))
                  (push (lispm-edits--overlay at end 'display
                                              (if here
                                                  (concat tag (if (eq (char-before end) ?\n) "\n" ""))
                                                ""))
                        overlays)))))
          ;; Typed in: the cursor lingers after the change a moment.
          (when (and (not cursor) (< elapsed (+ type (* 0.7 lispm-edits-fade))))
            (when-let* ((last (car (last (seq-filter (lambda (h) (> (nth 3 h) 0))
                                                     (lispm-edits--show-hunks show))))))
              (let* ((end (nth 1 last))
                     (at (if (eq (char-before end) ?\n) (1- end) end)))
                (push (lispm-edits--overlay at at 'before-string tag) overlays))))
          (when cursor
            (let ((window (lispm-edits--show-window show)))
              (when (and (window-live-p window) (not (eq window (selected-window)))
                         (eq (window-buffer window) buffer))
                (set-window-point window cursor))))
          (setf (lispm-edits--show-overlays show) overlays)
          t)))))

(defun lispm-edits--ghost (removed lingering)
  "REMOVED text as it lingers, LINGERING going from 1 (just removed) to 0."
  (let* ((lines (split-string (string-remove-suffix "\n" removed) "\n"))
         (text (concat (string-join (seq-take lines 30) "\n") "\n"
                       (if (> (length lines) 30)
                           (format "… %d more lines removed\n" (- (length lines) 30))
                         "")))
         (background (face-background 'default nil t))
         (foreground (face-foreground 'default nil t))
         (bg (lispm-edits--blend background (face-background 'lispm-edits-removed nil t) lingering))
         (fg (lispm-edits--blend background foreground lingering)))
    (propertize text 'face (if (and bg fg)
                               `(:background ,bg :foreground ,fg :strike-through ,fg :extend t)
                             'lispm-edits-removed))))

(defun lispm-edits--drop (show)
  "Remove what shows SHOW."
  (mapc #'delete-overlay (lispm-edits--show-overlays show))
  (setf (lispm-edits--show-overlays show) nil)
  (pcase-dolist (`(,beg ,end . ,_) (lispm-edits--show-hunks show))
    (set-marker beg nil)
    (set-marker end nil)))

(defun lispm-edits--finish (buffer)
  "Stop showing changes to BUFFER: show its text as it is."
  (setq lispm-edits--shows
        (seq-remove (lambda (show)
                      (when (eq (lispm-edits--show-buffer show) buffer)
                        (lispm-edits--drop show)
                        t))
                    lispm-edits--shows)))

(defun lispm-edits--interrupt (&rest _)
  "Stop showing changes to the current buffer, which is changing."
  (when lispm-edits--shows
    (lispm-edits--finish (current-buffer))))

;;;; Changes proposed

(cl-defstruct (lispm-edits--proposal (:constructor lispm-edits--proposal-create)
                                     (:copier nil))
  "The change AGENT's call ID would make: BUFFER's text would be TEXT.
STREAMING while the model is still writing the call.  CREATED if
BUFFER was made to show it; REVEALED once it was brought into view."
  agent id buffer text streaming created revealed overlays)

(defvar lispm-edits--proposals nil
  "Proposed changes being shown.")

(defvar lispm-edits--writing (make-hash-table :test #'equal)
  "Call id -> (AGENT . CALL): the latest input of calls being written.")

(defun lispm-edits--proposal (id)
  "The proposal of call ID, or nil."
  (seq-find (lambda (p) (equal (lispm-edits--proposal-id p) id)) lispm-edits--proposals))

(defun lispm-edits--propose (agent id tool input streaming)
  "Show the change AGENT's call ID of TOOL on INPUT would make.
STREAMING if INPUT is still being written.  Nothing shows if the call
changes no buffer, or does not make sense yet."
  (let* ((before (buffer-list))
         (change (and tool
                      (condition-case nil
                          (let ((lispm-current-agent agent)
                                (default-directory (lispm-agent-root agent)))
                            (lispm-tool-would-change tool input))
                        (error nil)))))
    (pcase change
      (`(,(and (pred buffer-live-p) buffer) . ,(and (pred stringp) text))
       (let ((p (lispm-edits--proposal id)))
         (when (and p (not (eq (lispm-edits--proposal-buffer p) buffer)))
           (lispm-edits--retire p nil)
           (setq p nil))
         (unless p
           (setq p (lispm-edits--proposal-create
                    :agent agent :id id :buffer buffer
                    :created (not (memq buffer before))))
           (push p lispm-edits--proposals))
         (unless (and (equal text (lispm-edits--proposal-text p))
                      (eq streaming (lispm-edits--proposal-streaming p)))
           (setf (lispm-edits--proposal-text p) text
                 (lispm-edits--proposal-streaming p) streaming)
           (lispm-edits--draw-proposal p)))))))

(defun lispm-edits--draw-proposal (p)
  "Lay the change proposal P proposes over its buffer."
  (mapc #'delete-overlay (lispm-edits--proposal-overlays p))
  (setf (lispm-edits--proposal-overlays p) nil)
  (let ((buffer (lispm-edits--proposal-buffer p)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((old (lispm-tools--buffer-text buffer))
               (text (lispm-edits--proposal-text p))
               (streaming (lispm-edits--proposal-streaming p))
               (agent (lispm-edits--proposal-agent p))
               (window (lispm-edits--window buffer))
               (room (if (window-live-p window) (max 4 (- (window-body-height window) 4)) 30))
               (hunks (lispm-edits--hunks old text))
               (tag (concat (lispm-edits--tag agent (if streaming "writing…" "awaiting approval"))
                            "\n"))
               (bar (propertize " " 'face `(:background ,(lispm-edits--color agent))))
               overlays)
          ;; Text still coming does not remove what follows it.
          (when streaming
            (setq hunks (lispm-edits--open-end hunks old text)))
          (cl-loop for ((of ot nf nt) . more) on hunks
                   for first = t then nil
                   do (let ((added (lispm-edits--proposed-text
                                    (substring text nf nt) room streaming
                                    (and streaming (null more) bar))))
                        (when (< of ot)
                          (push (lispm-edits--overlay (1+ of) (1+ ot)
                                                      'face 'lispm-edits-proposed-removed)
                                overlays))
                        (when (and first (< of ot))
                          (push (lispm-edits--overlay (1+ of) (1+ of) 'before-string tag)
                                overlays))
                        (when (or (> (length added) 0) (and first (= of ot)))
                          (push (lispm-edits--overlay (1+ ot) (1+ ot) 'before-string
                                                      (concat (if (and first (= of ot)) tag "")
                                                              added))
                                overlays))))
          (setf (lispm-edits--proposal-overlays p) overlays)
          (when (and hunks (not (lispm-edits--proposal-revealed p)))
            (setf (lispm-edits--proposal-revealed p) t)
            (lispm-edits--reveal window (1+ (caar hunks)))))))))

(defun lispm-edits--open-end (hunks old text)
  "HUNKS, but a last one that runs to the end of OLD and TEXT removes no
more lines than it adds: TEXT is still being written."
  (let ((last (car (last hunks))))
    (if (not (and last (= (nth 1 last) (length old)) (= (nth 3 last) (length text))))
        hunks
      (pcase-let* ((`(,of ,ot ,nf ,nt) last)
                   (lines (cl-count ?\n (substring text nf nt)))
                   (to of))
        (dotimes (_ (1+ lines))
          (setq to (if-let* ((at (string-search "\n" old to))) (1+ at) ot)))
        (append (butlast hunks) (list (list of (min to ot) nf nt)))))))

(defun lispm-edits--proposed-text (added room tail bar)
  "ADDED, as proposed text is shown: at most ROOM lines, the last if TAIL.
BAR, if non-nil, is a cursor at its end."
  (let* ((lines (split-string (string-remove-suffix "\n" added) "\n"))
         (newline (string-suffix-p "\n" added))
         (n (if (string-empty-p added) 0 (length lines)))
         (shown (cond ((<= n room) lines)
                      (tail (last lines room))
                      (t (seq-take lines room))))
         (text (concat (propertize (string-join shown "\n") 'face 'lispm-edits-proposed-added)
                       (or bar "")
                       (if (or newline (> n room)) "\n" ""))))
    (cond ((<= n room) text)
          (tail (concat (propertize (format "… %d lines above\n" (- n room)) 'face 'lispm-edits-note)
                        text))
          (t (concat text (propertize (format "… %d more lines\n" (- n room))
                                      'face 'lispm-edits-note))))))

(defun lispm-edits--retire (p made)
  "Stop showing the proposal P; MADE if its change is being made."
  (mapc #'delete-overlay (lispm-edits--proposal-overlays p))
  (setf (lispm-edits--proposal-overlays p) nil)
  (setq lispm-edits--proposals (delq p lispm-edits--proposals))
  (let ((buffer (lispm-edits--proposal-buffer p)))
    (when (buffer-live-p buffer)
      (if made
          (with-current-buffer buffer (setq lispm-edits--seen (float-time)))
        ;; A buffer made only to show a change that was not made.
        (when (and (lispm-edits--proposal-created p)
                   (not (buffer-modified-p buffer))
                   (not (and (buffer-file-name buffer)
                             (file-exists-p (buffer-file-name buffer)))))
          (kill-buffer buffer))))))

(defun lispm-edits--update (agent)
  "Bring the proposals of AGENT's calls up to date with the calls."
  (let ((pending (lispm-agent-pending agent)))
    (dolist (p (seq-filter (lambda (p) (eq (lispm-edits--proposal-agent p) agent))
                           lispm-edits--proposals))
      (let ((call (gethash (lispm-edits--proposal-id p) pending)))
        (pcase (and call (plist-get call :status))
          ('ask nil)
          ('run (lispm-edits--retire p t))
          (_ (unless (and (lispm-edits--proposal-streaming p)
                          (eq (lispm-agent-state agent) 'requesting))
               (lispm-edits--retire p nil))))))
    (when (lispm-edits--live-p)
      (maphash (lambda (id call)
                 (when (eq (plist-get call :status) 'ask)
                   (let ((asked (lispm--asked call)))
                     (lispm-edits--propose agent id (plist-get asked :tool)
                                           (plist-get asked :input) nil))))
               pending))))

(defun lispm-edits--input (agent call)
  "Note that AGENT is writing CALL, a plist of its :id, :name and :json.
What the call would change is shown a moment later, from the input as
it is then: the input grows faster than it is worth drawing."
  (when-let* (((lispm-edits--live-p))
              (id (plist-get call :id))
              (tool (lispm-get-tool (plist-get call :name)))
              ((lispm-tool-changes tool)))
    (unless (gethash id lispm-edits--writing)
      (run-at-time 0.08 nil #'lispm-edits--written id))
    (puthash id (cons agent call) lispm-edits--writing)))

(defun lispm-edits--written (id)
  "Show what the call ID being written would change, as far as it goes."
  (pcase-let ((`(,agent . ,call) (gethash id lispm-edits--writing)))
    (remhash id lispm-edits--writing)
    ;; Unless the response is over, and the call proposed or made.
    (when (and agent (eq (lispm-agent-state agent) 'requesting))
      (lispm-edits--propose agent id (lispm-get-tool (plist-get call :name))
                            (lispm-edits--partial-json (plist-get call :json)) t))))

(defun lispm-edits--reverted ()
  "Draw again the proposals laid over the current buffer, just reverted.
They were made from its text before, which is gone: those of calls
that still make sense are made again from the text it has now."
  (let* ((buffer (current-buffer))
         (proposals (seq-filter (lambda (p) (eq (lispm-edits--proposal-buffer p) buffer))
                                lispm-edits--proposals)))
    (dolist (p proposals)
      (lispm-edits--retire p nil))
    (dolist (agent (delete-dups (mapcar #'lispm-edits--proposal-agent proposals)))
      (lispm-edits--update agent))))

(add-hook 'after-revert-hook #'lispm-edits--reverted)
(add-hook 'lispm-buffer-change-functions #'lispm-edits--changed)
(add-hook 'lispm-state-change-functions #'lispm-edits--update)
(add-hook 'lispm-tool-input-functions #'lispm-edits--input)

(provide 'lispm-edits)
;;; lispm-edits.el ends here
