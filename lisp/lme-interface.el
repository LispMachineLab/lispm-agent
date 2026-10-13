;;; lme-interface.el --- Windows, workspaces, tabs and terminals  -*- lexical-binding: t; -*-

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

;; The frame's layout: a left sidebar, a main area with terminals on the
;; left and files on the right, a workspace per project, window tab
;; lines, fonts that keep terminal columns aligned, and Eat terminals.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'thingatpt)
(require 'url-util)

(defun lme-disable-display-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode -1))

(defun lme-dired-plain-lines ()
  "Show dired listings without line numbers or visual line wrapping."
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (setq-local truncate-lines t))

;;;; Main area layout
;;
;; The main area is every window that is not a side window.  Terminals
;; (agent sessions and shells) live on its left, files on its right:
;;
;;   showing a terminal   reuse a terminal window, else split a window that
;;                        shows a file and take its left half
;;   visiting a file      from a terminal window, reuse a file window, else
;;                        split the terminal window and take its right half
;;
;; Anything else is shown in place.

(defun lme-main-area--window-p (window)
  "Return non-nil when WINDOW holds content of the main area.
Side windows and chrome windows, such as the browser's address window
(marked by the `lme-chrome' window parameter), do not."
  (not (or (window-parameter window 'window-side)
           (window-parameter window 'lme-chrome))))

(defun lme-main-area--windows ()
  "Return the live windows of the main area, left to right."
  (sort (seq-filter #'lme-main-area--window-p (window-list nil 'no-mini))
        (lambda (a b) (< (car (window-edges a)) (car (window-edges b))))))

(defvar-local lme-conversation-buffer nil
  "Non-nil in a buffer holding a conversation with an agent running in Emacs.
It is laid out as a terminal, though it may visit a file.")
(put 'lme-conversation-buffer 'permanent-local t)

(defun lme-terminal-buffer-p (buffer)
  "Return non-nil when BUFFER is laid out as a terminal.
That is an Eat terminal, or an agent conversation, see `lme-conversation-buffer'."
  (or (eq (buffer-local-value 'major-mode buffer) 'eat-mode)
      (buffer-local-value 'lme-conversation-buffer buffer)))

(defun lme-main-area--terminal-p (window)
  "Return non-nil when WINDOW shows a terminal."
  (lme-terminal-buffer-p (window-buffer window)))

(defun lme-main-area--file-p (window)
  "Return non-nil when WINDOW shows a buffer visiting a file, not a terminal."
  (let ((buffer (window-buffer window)))
    (and (buffer-file-name buffer) (not (lme-terminal-buffer-p buffer)))))

(defun lme-main-area-show-terminal (buffer)
  "Show terminal BUFFER on the left of the main area and select its window.
A window already showing a terminal is reused; otherwise the leftmost main
window is split when it shows a file, keeping the file on the right."
  (let* ((windows (lme-main-area--windows))
         (window (or (seq-find #'lme-main-area--terminal-p windows)
                     (let ((leftmost (or (car windows) (selected-window))))
                       (if (lme-main-area--file-p leftmost)
                           (split-window leftmost nil 'left)
                         leftmost)))))
    (set-window-buffer window buffer)
    (select-window window)
    buffer))

(defun lme-main-area--file-over-terminal-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a file about to replace a terminal.
This is the `display-buffer-alist' condition routing files to the right."
  (let ((buffer (get-buffer buffer-or-name)))
    (and (buffer-file-name buffer)
         (not (lme-terminal-buffer-p buffer))
         (not (window-parameter (selected-window) 'window-side))
         (lme-main-area--terminal-p (selected-window)))))

(defun lme-main-area-display-file (buffer alist)
  "Display file BUFFER on the right of the main area, per ALIST.
A main window already showing a file is reused, the rightmost first;
otherwise the selected terminal window is split and BUFFER takes its right
half.  This is a `display-buffer' action function."
  (let* ((files (seq-filter #'lme-main-area--file-p (lme-main-area--windows)))
         (window (or (car (last files))
                     (split-window (selected-window) nil 'right))))
    (window--display-buffer buffer window (if files 'reuse 'window) alist)))

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

(defun lme-workspace-enter (name &optional directory)
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

(declare-function lme-agent--project-root "lme-agent-list" (directory))
(declare-function lme-agent--git-main-root "lme-agent-list" (directory))

(defun lme-workspace--file-project (buffer)
  "Return (NAME . ROOT) of the git project holding file BUFFER, or nil.
Projects resolve as for agent sessions, so a repository's files and sessions,
its worktrees included, share one workspace."
  (when-let* ((file (and buffer (buffer-file-name buffer)))
              (directory (file-name-directory file))
              ((lme-agent--git-main-root directory))
              (root (lme-agent--project-root directory))
              ((stringp root)))
    (cons (file-name-nondirectory root) root)))

(defun lme-workspace--foreign-file-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a file of another workspace's project.
This is the `display-buffer-alist' condition sending files to their project."
  (and (bound-and-true-p tabspaces-mode)
       (when-let* ((project (lme-workspace--file-project (get-buffer buffer-or-name))))
         (not (equal (car project) (tabspaces--current-tab-name))))))

(defun lme-workspace-display-file (buffer alist)
  "Display file BUFFER in its project's workspace, per ALIST.
The workspace is entered, or created with BUFFER alone in it; there BUFFER
replaces the main window, or goes to the right when a terminal is selected.
This is a `display-buffer' action function."
  (pcase-let* ((`(,name . ,root) (lme-workspace--file-project buffer))
               (fresh (lme-workspace-enter name root)))
    (if (and (not fresh) (lme-main-area--terminal-p (selected-window)))
        (lme-main-area-display-file buffer alist)
      (prog1 (window--display-buffer buffer (lme-agent--main-window) 'reuse alist)
        (when fresh (tabspaces-reset-buffer-list))))))

(defun lme-workspace-show-alone (buffer)
  "Show BUFFER as the only buffer of the workspace just created.
A new tab starts with the previous workspace's buffer on display; replacing
it and resetting the tab's buffer list keeps that buffer out."
  (let ((window (lme-agent--main-window)))
    (set-window-buffer window buffer)
    (select-window window))
  (tabspaces-reset-buffer-list)
  buffer)

;;;; Tab bar

(defun lme-tab-bar--rgb (color)
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

(defun lme-tab-bar--blend (from to ratio)
  "Return the colour RATIO of the way from colour FROM to colour TO.
Return nil when either colour cannot be resolved, as on a text terminal."
  (when-let* ((a (lme-tab-bar--rgb from))
              (b (lme-tab-bar--rgb to)))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (x y) (+ x (* ratio (- y x)))) a b)
                   '(2)))))

(defun lme-tab-bar--bar-color ()
  "Return the colour of the bars above the windows: the editor a shade darker.
The tab bar and the windows' buffer tab lines share it, so the two read as
one band above the buffers."
  (lme-tab-bar--blend (face-background 'default nil t) "#000000" 0.25))

(defun lme-tab-bar--muted-color ()
  "Return the text colour of unselected tabs: dimmed, yet legible on the bar.
The theme's `shadow' colour alone is too faint on the darker bar."
  (lme-tab-bar--blend (face-foreground 'shadow nil t)
                     (face-foreground 'default nil t) 0.35))

(defun lme-tab-bar-tab-name ()
  "Name the current tab after the buffer of its main, non-side window.
Agent terminals are named by their session title."
  (let* ((window (or (seq-find (lambda (window)
                                 (not (window-parameter window 'window-side)))
                               (cons (selected-window) (window-list nil 'no-mini)))
                     (selected-window)))
         (buffer (window-buffer window)))
    (or (lme-agent-buffer-label buffer) (buffer-name buffer))))

(defconst lme-terminal-font-family "Maple Mono NF CN"
  "Monospace family whose glyphs all occupy whole terminal cells.
A CJK ideograph is exactly two Latin cells wide, and box drawing, block
elements and most terminal UI symbols are covered at one cell, so columns
that a terminal program lays out stay aligned.")

(defconst lme-cjk-font-families (list lme-terminal-font-family "PingFang SC")
  "Font families for CJK scripts, in order of preference.
Without a pinned family Emacs searches fallback fonts for each CJK glyph,
which is slow on macOS and makes text-heavy terminals stutter.  The
terminal family comes first: its ideographs are two cells wide, so CJK
text keeps the column grid; PingFang is proportional and does not.")

(defconst lme-fallback-font-rescale '(("Arial Unicode MS" . 0.86))
  "Scale factors for proportional fallback fonts.
Symbols no monospace family covers (dingbat asterisks such as ✻ ✳ ✽)
fall back to Arial Unicode MS, whose glyphs are wider and taller than a
12pt cell; at this scale they fit one cell without growing the line.")

;; A zh_CN locale selects a Chinese language environment, which counts
;; East Asian Ambiguous characters (│ ─ ● … ⏺) as two columns.  Terminal
;; programs follow wcwidth and count them as one, so Eat placed every
;; character after them a column off and split-pane dividers broke apart.
(setopt cjk-ambiguous-chars-are-wide nil)

(defun lme-pin-cjk-font ()
  "Draw CJK scripts with the first installed of `lme-cjk-font-families'."
  (when-let* ((family (seq-find (lambda (family) (member family (font-family-list)))
                                lme-cjk-font-families)))
    (dolist (script '(han cjk-misc kana bopomofo hangul))
      (set-fontset-font t script (font-spec :family family))))
  (dolist (rescale lme-fallback-font-rescale)
    (setf (alist-get (car rescale) face-font-rescale-alist nil nil #'equal)
          (cdr rescale))))

(defconst lme-terminal-glyph-substitutions '((?⏺ . ?●) (?✢ . ?✶))
  "Display substitutions for symbols no monospace font draws at one cell.
Each pair maps a character a terminal program emits to the look-alike
shown in its place; the buffer text is unchanged.")

(defun lme-terminal-display-table ()
  "Return a display table applying `lme-terminal-glyph-substitutions'."
  (let ((table (make-display-table)))
    (pcase-dolist (`(,from . ,to) lme-terminal-glyph-substitutions)
      (aset table from (vector to)))
    table))

(defun lme-eat-mark-unmodified (&rest _)
  "Mark the current Eat buffer unmodified after terminal output.
A terminal has no file to save, so its modified flag carries no meaning; left
set, it makes tab bars show an unsaved marker in place of the close button.
`restore-buffer-modified-p' avoids forcing a mode line update per output."
  (restore-buffer-modified-p nil))

(defun lme-eat-terminal-display ()
  "Configure an Eat buffer's display for terminal output.
The terminal already wraps lines to its width, so Emacs-side visual line
wrapping only adds work; terminal text is left-to-right, so bidirectional
reordering is turned off; and a full-screen program redraws every line, so
highlighting the current line only adds an overlay update per redraw.
Terminal output is not an edit: `first-change-hook' is emptied locally, so
output never runs edit-tracking hooks such as tab bars recomputing their
modified markers, which `lme-eat-mark-unmodified' would otherwise re-arm on
every update.
Text is drawn in `lme-terminal-font-family' so every glyph fills whole
cells and split-pane dividers drawn with box characters stay straight."
  (visual-line-mode -1)
  (when (member lme-terminal-font-family (font-family-list))
    (face-remap-add-relative 'default :family lme-terminal-font-family))
  (setq-local buffer-display-table (lme-terminal-display-table))
  (setq-local first-change-hook nil)
  (setq-local truncate-lines t
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              global-hl-line-mode nil))

(defun lme-setup-interface ()
  "Set up interface."
  (electric-pair-mode 1)
  (when (lme-macos-p)
    (define-key global-map [ns-drag-file] 'ns-find-file))
  (when (lme-gui-p)
    (lme-pin-cjk-font))
  (lme-ansi-apply-faces)
  (add-hook 'enable-theme-functions #'lme-ansi-apply-faces)
  ;; Each entry is added in front, so the workspace rule is consulted first.
  (add-to-list 'display-buffer-alist
               '(lme-main-area--file-over-terminal-p lme-main-area-display-file))
  (add-to-list 'display-buffer-alist
               '(lme-workspace--foreign-file-p lme-workspace-display-file))
  (add-hook 'buffer-list-update-hook #'lme-buffer-list--schedule-refresh)
  (add-hook 'xwidget-webkit-mode-hook #'lme-browser--setup)
  (add-hook 'tab-bar-tab-post-select-functions #'lme-browser--on-workspace-select)
  (with-eval-after-load 'xwidget
    (keymap-set xwidget-webkit-mode-map "C-l" #'lme-browser-focus-address))
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
    (menu-bar-mode 1))
  (show-paren-mode 1)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (global-prettify-symbols-mode 1)
  (global-visual-line-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (dolist (hook '(eshell-mode-hook term-mode-hook shell-mode-hook eat-mode-hook doc-view-mode-hook treemacs-mode-hook))
    (add-hook hook #'lme-disable-display-line-numbers))

  (use-package avy
    :bind (("C-c SPC" . avy-goto-char-timer)
           ("M-g w" . avy-goto-word-1)
           ("M-g l" . avy-goto-line)))

  (use-package treemacs
    :bind (("C-c p t" . treemacs-add-and-display-current-project)
           ("C-c p T" . treemacs))
    :hook (treemacs-mode . lme-sidebar--files-toolbar)
    :config
    ;; No sidebar view has a mode line, so switching views keeps the bottom
    ;; edge in place.
    (setq treemacs-width lme-sidebar-width
          treemacs-is-never-other-window t
          treemacs-user-mode-line-format 'none)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  (use-package treemacs-nerd-icons
    :after treemacs
    :config
    (treemacs-load-theme "nerd-icons"))

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
    :hook ((dired-mode . lme-dired-plain-lines)
           (dirvish-directory-view-mode . lme-dired-plain-lines))
    :config
    (dirvish-override-dired-mode 1)
    (dirvish-side-follow-mode 1))

  (defun lme-eat-new ()
    (interactive)
    (eat nil '(4)))

  (use-package eat
    :commands (eat eat-project)
    :bind (("C-c t" . lme-eat-new)
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
    (advice-add 'eat-exec :filter-args #'lme-eat-wrap-command-with-stty)
    (add-hook 'eat-update-hook #'lme-eat-mark-unmodified)
    :hook ((eat-mode . lme-eat-terminal-display)
           (eat-mode . lme-eat-terminal-keys)
           (eat-exit . lme-shell-list-refresh)
           (eshell-load . eat-eshell-mode)
           (eshell-load . eat-eshell-visual-command-mode)))

  (use-package nerd-icons)

  (global-set-key (kbd "C-c x b") #'lme-xwidget-browse-url)
  (global-set-key (kbd "C-c x s") #'lme-xwidget-search)
  (global-set-key (kbd "C-c x u") #'lme-xwidget-open-current-url)

  (use-package tramp
    :ensure nil
    :config (setq tramp-default-method "sshx")))


(provide 'lme-interface)
;;; lme-interface.el ends here
