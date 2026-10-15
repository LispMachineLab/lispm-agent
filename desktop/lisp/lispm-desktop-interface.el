;;; lispm-desktop-interface.el --- Windows, workspaces, tabs and terminals  -*- lexical-binding: t; -*-

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

;; The frame's layout: a left sidebar, a main area with terminals on the
;; left and files on the right, a workspace per project, window tab
;; lines, fonts that keep terminal columns aligned, and Eat terminals.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'thingatpt)
(require 'url-util)

(defvar lispm-desktop-platinum-mode-line-format)

(defun lispm-desktop-disable-display-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode -1))

(defun lispm-desktop-dired-plain-lines ()
  "Show dired listings without line numbers or visual line wrapping."
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (setq-local truncate-lines t))

;;;; Main area layout
;;
;; The main area is every window that is not a side window.  It keeps two
;; columns, whatever command shows a buffer:
;;
;;   sessions   terminals and agent conversations, on the left: a window
;;              already showing a session is reused, else the leftmost
;;              window, split first when it shows a file
;;   the rest   files, and any buffer switched to in place, on the right,
;;              never over a session nor in the sidebar: they go to the
;;              rightmost window that is not a session, else a session
;;              window is split for them
;;
;; Popups (help, completions, transient menus) open as they ask.  The
;; rules sit in `display-buffer-alist', and `switch-to-buffer' obeys them
;; too, so C-x b, C-x C-f, the sidebar, the title bar's buffer menu and
;; the agents' own display requests all keep the columns.

(defun lispm-desktop-main-area--window-p (window)
  "Return non-nil when WINDOW holds content of the main area.
Side windows and chrome windows, such as the browser's address window
(marked by the `lispm-desktop-chrome' window parameter), do not."
  (not (or (window-parameter window 'window-side)
           (window-parameter window 'lispm-desktop-chrome))))

(defun lispm-desktop-main-area--windows ()
  "Return the live windows of the main area, left to right."
  (sort (seq-filter #'lispm-desktop-main-area--window-p (window-list nil 'no-mini))
        (lambda (a b) (< (car (window-edges a)) (car (window-edges b))))))

(defvar-local lispm-desktop-conversation-buffer nil
  "Non-nil in a buffer holding a conversation with an agent running in Emacs.
It is laid out as a terminal, though it may visit a file.")
(put 'lispm-desktop-conversation-buffer 'permanent-local t)

(defvar lispm-desktop-conversation-modes nil
  "Major modes of agent conversations, laid out as terminals.
Agents running inside Emacs add their chat modes, so a chat keeps to the
session column from the moment it is created.")

(defun lispm-desktop-terminal-buffer-p (buffer)
  "Return non-nil when BUFFER is laid out as a terminal: a session.
That is an Eat terminal, or an agent conversation, see
`lispm-desktop-conversation-buffer' and `lispm-desktop-conversation-modes'."
  (let ((mode (buffer-local-value 'major-mode buffer)))
    (or (memq mode '(eat-mode lispm-desktop-term-mode))
        (buffer-local-value 'lispm-desktop-conversation-buffer buffer)
        (and lispm-desktop-conversation-modes
             (apply #'provided-mode-derived-p mode lispm-desktop-conversation-modes)))))

(defun lispm-desktop-main-area--terminal-p (window)
  "Return non-nil when WINDOW shows a terminal."
  (lispm-desktop-terminal-buffer-p (window-buffer window)))

(defun lispm-desktop-main-area--file-p (window)
  "Return non-nil when WINDOW shows a buffer visiting a file, not a terminal."
  (let ((buffer (window-buffer window)))
    (and (buffer-file-name buffer) (not (lispm-desktop-terminal-buffer-p buffer)))))

(defun lispm-desktop-main-area--by-use (windows)
  "Return WINDOWS, the most recently used first."
  (sort (copy-sequence windows)
        (lambda (a b) (> (window-use-time a) (window-use-time b)))))

(defun lispm-desktop-main-area--session-window (buffer)
  "Return the window session BUFFER belongs in, and whether it is new.
The value is (WINDOW . NEW).  A window showing BUFFER wins, then the
session window used last; else the leftmost window, split off to the
left of a file it shows."
  (let* ((windows (lispm-desktop-main-area--windows))
         (leftmost (or (car windows) (selected-window))))
    (or (when-let* ((window (or (get-buffer-window buffer)
                                (car (lispm-desktop-main-area--by-use
                                      (seq-filter #'lispm-desktop-main-area--terminal-p windows))))))
          (cons window nil))
        (if (lispm-desktop-main-area--file-p leftmost)
            (cons (split-window leftmost nil 'left) t)
          (cons leftmost nil)))))

(defun lispm-desktop-main-area--file-window ()
  "Return the window a file belongs in, and whether it is new.
The value is (WINDOW . NEW).  The rightmost window showing a file wins,
then the rightmost window showing no session; else the rightmost session
window is split, and the file takes its right half."
  (let ((windows (lispm-desktop-main-area--windows)))
    (if-let* ((window (or (car (last (seq-filter #'lispm-desktop-main-area--file-p windows)))
                          (car (last (seq-remove #'lispm-desktop-main-area--terminal-p windows))))))
        (cons window nil)
      (cons (split-window (or (car (last windows)) (selected-window)) nil 'right) t))))

(defun lispm-desktop-main-area-display-session (buffer alist)
  "Display session BUFFER in the session column, per ALIST.
This is a `display-buffer' action function."
  (pcase-let ((`(,window . ,new) (lispm-desktop-main-area--session-window buffer)))
    (window--display-buffer buffer window (if new 'window 'reuse) alist)))

(defun lispm-desktop-main-area-display-file (buffer alist)
  "Display file BUFFER in the file column, per ALIST.
This is a `display-buffer' action function."
  (pcase-let ((`(,window . ,new) (lispm-desktop-main-area--file-window)))
    (window--display-buffer buffer window (if new 'window 'reuse) alist)))

(defun lispm-desktop-main-area-show-terminal (buffer)
  "Show terminal BUFFER in the session column and select its window."
  (select-window (lispm-desktop-main-area-display-session buffer nil))
  buffer)

(defun lispm-desktop-main-area--session-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a session about to be shown.
This is the `display-buffer-alist' condition keeping sessions on the left."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (lispm-desktop-terminal-buffer-p buffer)
         ;; A session shown in place in a session window stays there, so
         ;; two sessions side by side remain possible.
         (not (and (lispm-desktop-main-area--terminal-p (selected-window))
                   (lispm-desktop-main-area--window-p (selected-window))
                   (not (get-buffer-window buffer)))))))

(defun lispm-desktop-main-area--astray-p (buffer-or-name action)
  "Return non-nil when BUFFER-OR-NAME is about to cover a session or the sidebar.
That is a buffer other than a session, shown from a session window or a
side window, such as the sidebar: a file, whatever ACTION asks, or any
buffer switched to in place.  This is the `display-buffer-alist'
condition keeping the session column for sessions."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (not (lispm-desktop-terminal-buffer-p buffer))
         (or (buffer-file-name buffer)
             (memq 'display-buffer-same-window (ensure-list (car-safe action))))
         (or (window-parameter (selected-window) 'window-side)
             (lispm-desktop-main-area--terminal-p (selected-window))))))

;;;; Project workspaces
;;
;; Under `tabspaces-mode' each tab is a workspace owning its buffers.  Entry
;; points that open a project's buffer from anywhere, such as the agent
;; sidebar, first enter that project's workspace, so workspaces follow
;; projects instead of one tab collecting every project.

(declare-function tabspaces--current-tab-name "tabspaces" ())
(declare-function tabspaces--list-tabspaces "tabspaces" ())
(declare-function tabspaces--remember-project-tab "tabspaces" (project-directory tab-name))
(declare-function tabspaces-switch-or-create-workspace "tabspaces" (&optional workspace))
(declare-function tabspaces-reset-buffer-list "tabspaces" ())

(defun lispm-desktop-workspace-enter (name &optional directory)
  "Select the workspace NAME, creating it when missing.
Return non-nil when it was created.  DIRECTORY, when given, is recorded as the
new workspace's project, so project commands run inside it resolve there.
Without `tabspaces-mode' nothing happens."
  (when (and (bound-and-true-p tabspaces-mode)
             (not (equal name (tabspaces--current-tab-name))))
    (let ((fresh (not (member name (tabspaces--list-tabspaces)))))
      (tabspaces-switch-or-create-workspace name)
      (when (and fresh directory)
        (tabspaces--remember-project-tab directory name))
      fresh)))

(declare-function lispm-desktop-shell-new "lispm-desktop-sidebar" (&optional directory))

(defun lispm-desktop-workspace-show-alone (buffer)
  "Show BUFFER as the only buffer of the workspace just created.
A new tab starts with the previous workspace's buffer on display; replacing
it and resetting the tab's buffer list keeps that buffer out."
  (let ((window (lispm-desktop-agent--main-window)))
    (set-window-buffer window buffer)
    (select-window window))
  (tabspaces-reset-buffer-list)
  buffer)

;;;; Tab bar

(defun lispm-desktop-tab-bar--rgb (color)
  "Return COLOR as a list of red, green and blue between 0 and 1, or nil.
A colour written #RRGGBB is read as written, whatever the display can
show; any other name is resolved by the display."
  (if (and (stringp color)
           (string-match (concat "\\`#\\([[:xdigit:]]\\{2\\}\\)"
                                 "\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'")
                         color))
      (mapcar (lambda (group) (/ (string-to-number (match-string group color) 16) 255.0))
              '(1 2 3))
    (and color (color-name-to-rgb color))))

(defun lispm-desktop-tab-bar--blend (from to ratio)
  "Return the colour RATIO of the way from colour FROM to colour TO.
Each component is rounded to the nearest of its 256 values, so a blend
falling on one of them comes back exactly.  Return nil when either
colour cannot be resolved, as on a text terminal."
  (when-let* ((a (lispm-desktop-tab-bar--rgb from))
              (b (lispm-desktop-tab-bar--rgb to)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (* 255 (+ x (* ratio (- y x)))))) a b))))

(defun lispm-desktop-tab-bar--bar-color ()
  "Return the colour of the bars above the windows: the editor a shade darker.
The tab bar and the windows' buffer tab lines share it, so the two read as
one band above the buffers."
  (lispm-desktop-tab-bar--blend (face-background 'default nil t) "#000000" 0.25))

(defun lispm-desktop-tab-bar--muted-color ()
  "Return the text colour of unselected tabs: dimmed, yet legible on the bar.
The theme's `shadow' colour alone is too faint on the darker bar."
  (lispm-desktop-tab-bar--blend (face-foreground 'shadow nil t)
                                (face-foreground 'default nil t) 0.35))

(defun lispm-desktop-tab-bar-tab-name ()
  "Name the current tab after the buffer of its main, non-side window.
Agent terminals are named by their session title."
  (let* ((window (or (seq-find (lambda (window)
                                 (not (window-parameter window 'window-side)))
                               (cons (selected-window) (window-list nil 'no-mini)))
                     (selected-window)))
         (buffer (window-buffer window)))
    (or (lispm-desktop-agent-buffer-label buffer) (buffer-name buffer))))

(defconst lispm-desktop-default-font-families '("Monaco" "Menlo" "DejaVu Sans Mono")
  "Font families for the default face, in order of preference.
Mac OS Emacs (NS) draws in Monaco when nothing is set, but X Emacs asks
fontconfig for \"monospace\", which on a Mac is Andale Mono; naming the
family keeps every build of Emacs alike.")

(defun lispm-desktop-pin-default-font ()
  "Draw the default face in the first installed of the default font families.
They are `lispm-desktop-default-font-families'.  A default face customized
with Customize is left as it is."
  (unless (or (get 'default 'saved-face) (get 'default 'customized-face))
    (when-let* ((family (seq-find (lambda (family) (member family (font-family-list)))
                                  lispm-desktop-default-font-families)))
      (set-face-attribute 'default nil :family family :height 120))))

(defun lispm-desktop-frame-menu-bar (&optional frame)
  "Keep FRAME's menu bar only where it is drawn outside the frame.
Mac OS Emacs (NS) puts it at the top of the screen; on X it would be a
second bar inside the frame, above the Platinum menu bar."
  (let ((frame (or frame (selected-frame))))
    (when (and (display-graphic-p frame) (not (eq (framep frame) 'ns)))
      (set-frame-parameter frame 'menu-bar-lines 0))))

(defconst lispm-desktop-terminal-font-family "Maple Mono NF CN"
  "Monospace family whose glyphs all occupy whole terminal cells.
A CJK ideograph is exactly two Latin cells wide, and box drawing, block
elements and most terminal UI symbols are covered at one cell, so columns
that a terminal program lays out stay aligned.")

(defconst lispm-desktop-cjk-font-families (list lispm-desktop-terminal-font-family "PingFang SC")
  "Font families for CJK scripts, in order of preference.
Without a pinned family Emacs searches fallback fonts for each CJK glyph,
which is slow on macOS and makes text-heavy terminals stutter.  The
terminal family comes first: its ideographs are two cells wide, so CJK
text keeps the column grid; PingFang is proportional and does not.")

(defconst lispm-desktop-fallback-font-rescale '(("Arial Unicode MS" . 0.86))
  "Scale factors for proportional fallback fonts.
Symbols no monospace family covers (dingbat asterisks such as ✻ ✳ ✽)
fall back to Arial Unicode MS, whose glyphs are wider and taller than a
12pt cell; at this scale they fit one cell without growing the line.")

;; A zh_CN locale selects a Chinese language environment, which counts
;; East Asian Ambiguous characters (│ ─ ● … ⏺) as two columns.  Terminal
;; programs follow wcwidth and count them as one, so Eat placed every
;; character after them a column off and split-pane dividers broke apart.
(setopt cjk-ambiguous-chars-are-wide nil)

(defun lispm-desktop-pin-cjk-font ()
  "Draw CJK scripts with the first installed of `lispm-desktop-cjk-font-families'."
  (when-let* ((family (seq-find (lambda (family) (member family (font-family-list)))
                                lispm-desktop-cjk-font-families)))
    (dolist (script '(han cjk-misc kana bopomofo hangul))
      (set-fontset-font t script (font-spec :family family))))
  (dolist (rescale lispm-desktop-fallback-font-rescale)
    (setf (alist-get (car rescale) face-font-rescale-alist nil nil #'equal)
          (cdr rescale))))

(defconst lispm-desktop-terminal-glyph-substitutions '((?⏺ . ?●) (?✢ . ?✶))
  "Display substitutions for symbols no monospace font draws at one cell.
Each pair maps a character a terminal program emits to the look-alike
shown in its place; the buffer text is unchanged.")

(defun lispm-desktop-terminal-display-table ()
  "Return a display table applying `lispm-desktop-terminal-glyph-substitutions'."
  (let ((table (make-display-table)))
    (pcase-dolist (`(,from . ,to) lispm-desktop-terminal-glyph-substitutions)
      (aset table from (vector to)))
    table))

(defun lispm-desktop-eat-mark-unmodified (&rest _)
  "Mark the current Eat buffer unmodified after terminal output.
A terminal has no file to save, so its modified flag carries no meaning; left
set, it makes tab bars show an unsaved marker in place of the close button.
`restore-buffer-modified-p' avoids forcing a mode line update per output."
  (restore-buffer-modified-p nil))

(defun lispm-desktop-eat-terminal-display ()
  "Configure an Eat buffer's display for terminal output.
The terminal already wraps lines to its width, so Emacs-side visual line
wrapping only adds work; terminal text is left-to-right, so bidirectional
reordering is turned off; and a full-screen program redraws every line, so
highlighting the current line only adds an overlay update per redraw.
Terminal output is not an edit: `first-change-hook' is emptied locally, so
output never runs edit-tracking hooks such as tab bars recomputing their
modified markers, which `lispm-desktop-eat-mark-unmodified' would otherwise
re-arm on every update.  Text is drawn in `lispm-desktop-terminal-font-family'
so every glyph fills whole cells and split-pane dividers drawn with box
characters stay straight."
  (visual-line-mode -1)
  (when (member lispm-desktop-terminal-font-family (font-family-list))
    (face-remap-add-relative 'default :family lispm-desktop-terminal-font-family))
  (setq-local buffer-display-table (lispm-desktop-terminal-display-table))
  (setq-local first-change-hook nil)
  (setq-local truncate-lines t
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              global-hl-line-mode nil))

(defun lispm-desktop-setup-interface ()
  "Set up interface."
  (electric-pair-mode 1)
  (when (lispm-desktop-macos-p)
    (define-key global-map [ns-drag-file] 'ns-find-file))
  (when (lispm-desktop-gui-p)
    (lispm-desktop-pin-default-font)
    (lispm-desktop-pin-cjk-font))
  (lispm-desktop-ansi-apply-faces)
  (add-hook 'enable-theme-functions #'lispm-desktop-ansi-apply-faces)
  ;; `switch-to-buffer' obeys these too, so no command bypasses the columns;
  ;; a dedicated window, such as the sidebar's, sends the buffer elsewhere.
  ;; A file of another project opens where it is asked for, as in Emacs:
  ;; only switching projects changes the workspace.
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file))
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session))
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-sidebar--view-buffer-p lispm-desktop-sidebar-display-view))
  (setq switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window 'pop)
  (add-hook 'xwidget-webkit-mode-hook #'lispm-desktop-browser--setup)
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-browser--on-workspace-select)
  ;; The floating browser follows the application in front, as the Finder
  ;; window does, and takes the keyboard back after a menu bar menu.
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-browser--follow-frontmost)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-browser--refocus)
  (with-eval-after-load 'xwidget
    (lispm-desktop-browser-bind-keys xwidget-webkit-mode-map)
    ;; C-y pastes into the page, which the buffer's text is not, and
    ;; Command-W closes the tab, as in the Finder's and Control Panels'
    ;; windows; in the location field M-w copies, as in Emacs.
    (keymap-set xwidget-webkit-mode-map "C-y" #'lispm-desktop-paste)
    (keymap-set xwidget-webkit-mode-map "M-w" #'lispm-desktop-browser-close-shown-tab))
  ;; Emacs opens on the desktop, as Mac OS did: no splash screen.
  (setq inhibit-startup-screen t
        ring-bell-function 'ignore
        js-indent-level 2
        c-basic-offset 2
        c-indent-level 4
        c-default-style "bsd"
        doc-view-continuous t)
  (set-face-attribute 'mode-line nil :height 120)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode 1)
    (mapc #'lispm-desktop-frame-menu-bar (frame-list))
    (add-hook 'after-make-frame-functions #'lispm-desktop-frame-menu-bar))
  (show-paren-mode 1)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (global-prettify-symbols-mode 1)
  (global-visual-line-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (dolist (hook '(eshell-mode-hook term-mode-hook shell-mode-hook eat-mode-hook doc-view-mode-hook treemacs-mode-hook))
    (add-hook hook #'lispm-desktop-disable-display-line-numbers))

  (use-package avy
    :bind (("C-c SPC" . avy-goto-char-timer)
           ("M-g w" . avy-goto-word-1)
           ("M-g l" . avy-goto-line)))

  (use-package treemacs
    :bind (("C-c p t" . treemacs-add-and-display-current-project)
           ("C-c p T" . treemacs))
    :hook (treemacs-mode . lispm-desktop-sidebar--files-toolbar)
    :config
    (setq treemacs-indentation-string (propertize " " 'display '(space :width (12)))
          treemacs-indentation 1)
    ;; Every sidebar view has the same bottom edge as any window, so
    ;; switching views keeps it in place.
    (setq treemacs-width lispm-desktop-sidebar-width
          treemacs-is-never-other-window t
          treemacs-user-mode-line-format lispm-desktop-platinum-mode-line-format)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  (use-package dirvish
    :demand t
    :bind (("C-c d" . dirvish)
           ("C-c D" . dirvish-side)
           :map dirvish-mode-map
           ("a" . dirvish-quick-access)
           ("TAB" . dirvish-subtree-toggle)
           ("f" . dirvish-file-info-menu)
           ("s" . dirvish-quicksort)
           ("y" . dirvish-yank-menu)
           ("v" . dirvish-vc-menu)
           ("^" . dirvish-history-last)
           ("h" . dirvish-history-jump)
           ("M-l" . dirvish-ls-switches-menu)
           ("M-m" . dirvish-mark-menu)
           ("?" . dirvish-dispatch))
    :init
    (when (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))
    (setq dired-dwim-target t
          dired-kill-when-opening-new-dired-buffer t
          dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg)
          dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
          dirvish-header-line-height 26
          dirvish-mode-line-height 26
          dirvish-side-width 32
          dirvish-quick-access-entries `(("h" "~/" "Home")
                                         ("d" "~/Downloads/" "Downloads")
                                         ("e" ,user-emacs-directory "Emacs")))
    :hook ((dired-mode . lispm-desktop-dired-plain-lines)
           (dirvish-directory-view-mode . lispm-desktop-dired-plain-lines))
    :config
    (dirvish-override-dired-mode 1)
    (dirvish-side-follow-mode 1))

  (defun lispm-desktop-eat-new ()
    "Start a new shell: in lispm-desktop-term once built, else in Eat."
    (interactive)
    (if (and (fboundp 'lispm-desktop-term-ready-p) (lispm-desktop-term-ready-p))
        (lispm-desktop-shell-new)
      (eat nil '(4))))

  (use-package eat
    :commands (eat eat-project)
    :bind (("C-c t" . lispm-desktop-eat-new)
           ("C-c T" . eat-project))
    :init
    (setq eat-enable-auto-line-mode nil
          eat-very-visible-cursor-type '(box nil nil)
          eat-very-visible-vertical-bar-cursor-type '(bar nil nil)
          eat-very-visible-horizontal-bar-cursor-type '(hbar nil nil)
          eat-term-name "xterm-256color"
          eat-term-terminfo-directory (cond
                                       ((file-directory-p "/opt/local/share/terminfo")
                                        "/opt/local/share/terminfo")
                                       ((file-directory-p "/usr/share/terminfo")
                                        "/usr/share/terminfo")))
    :config
    (advice-add 'eat-exec :filter-args #'lispm-desktop-eat-wrap-command-with-stty)
    (add-hook 'eat-update-hook #'lispm-desktop-eat-mark-unmodified)
    :hook ((eat-mode . lispm-desktop-eat-terminal-display)
           (eat-mode . lispm-desktop-eat-terminal-keys)
           (eat-exit . lispm-desktop-shell-list-refresh)
           (lispm-desktop-term-exit-functions . lispm-desktop-shell-list-refresh)
           (eshell-load . eat-eshell-mode)
           (eshell-load . eat-eshell-visual-command-mode)))

  (use-package nerd-icons)

  (global-set-key (kbd "C-c x b") #'lispm-desktop-xwidget-browse-url)
  (global-set-key (kbd "C-c x s") #'lispm-desktop-xwidget-search)
  (global-set-key (kbd "C-c x u") #'lispm-desktop-xwidget-open-current-url)

  (use-package tramp
    :ensure nil
    :config (setq tramp-default-method "sshx")))


(provide 'lispm-desktop-interface)
;;; lispm-desktop-interface.el ends here
