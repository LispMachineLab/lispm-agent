;;; lme-core.el --- Platform, clipboard and terminal keys  -*- lexical-binding: t; -*-

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

;; What the other modules build on: the platform tests, the clipboard
;; shared by Emacs, Evil and terminals, and the keys Eat terminals pass
;; through to the programs they run.

;;; Code:

(defconst lme-directory
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The lispm-emacs checkout: the parent of its lisp/ directory.")

(defgroup lispm-emacs nil
  "LispM's Emacs distribution."
  :group 'convenience
  :prefix "lme-")

(defcustom lme-editing-style nil
  "How text is edited: `vim' (Evil) or `emacs'.
Under `vim', Evil edits buffers of program, text and configuration files,
with SPC as the leader key, and every other buffer (terminals, agent
chats, the sidebar, Magit, ...) keeps Emacs keys.  nil means the choice
has not been made yet: the first start asks for it and saves the answer."
  :type '(choice (const :tag "Vim (Evil)" vim)
                 (const :tag "Emacs" emacs)
                 (const :tag "Ask at the next start" nil)))

(defcustom lme-theme nil
  "The colour theme: `dracula', `platinum' (Mac OS 9's Platinum greys), or
any other theme installed; the chrome takes its colours from whichever.
nil means the choice has not been made yet: the first start asks for it.
Change it from the λ menu, with `lme-set-theme', or by setting it in your
personal.el: once Emacs is set up, a new value is loaded at once."
  :type '(choice (const :tag "Dracula" dracula)
                 (const :tag "Platinum" platinum)
                 (symbol :tag "Another installed theme")
                 (const :tag "Ask at the next start" nil)))

(defvar lme--set-up nil
  "Non-nil once lispm-emacs is set up, its theme loaded.")

(defun lme--theme-changed (_symbol theme operation _where)
  "Load THEME when `lme-theme' is set to it once Emacs is set up.
This is a variable watcher."
  (when (and lme--set-up (eq operation 'set) theme
             (not (equal custom-enabled-themes (list theme))))
    (run-at-time 0 nil #'lme-load-theme theme)))

(defun lme-load-theme (theme)
  "Make THEME, by default Dracula, the only colour theme enabled."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (or theme 'dracula) t))

(defun lme-set-theme (theme)
  "Switch to colour THEME now and at every later start."
  (interactive (list (intern (completing-read "Theme: " '("dracula" "platinum") nil t))))
  (lme-load-theme theme)
  (customize-save-variable 'lme-theme theme))

(defcustom lme-user-file (locate-user-emacs-file "personal.el")
  "The user's own configuration, loaded after lispm-emacs is set up."
  :type 'file)

(defconst lme-sidebar-width 40
  "Total width in columns of every left sidebar view.
All views share it, so switching views keeps the main window's width: a
terminal in the main window is not resized, and a full-screen program in it,
such as a coding agent, does not redraw its whole history.")

(defun lme-macos-p ()
  "Return non-nil on macOS."
  (eq system-type 'darwin))

(defun lme-linux-p ()
  "Return non-nil on GNU/Linux."
  (eq system-type 'gnu/linux))

(defun lme-gui-p ()
  "Return non-nil when Emacs is running with a graphical display."
  (display-graphic-p))

(defun lme-setup-meta-key-issue ()
  "Map macOS command key to Meta."
  (when (lme-macos-p)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier nil)))

(defun lme-pbcopy (text &optional _push)
  "Copy TEXT to the macOS clipboard."
  (let ((process-connection-type nil))
    (let ((process (start-process "pbcopy" nil "pbcopy")))
      (process-send-string process text)
      (process-send-eof process))))

(defun lme-pbpaste ()
  "Return text from the macOS clipboard."
  (let ((text (shell-command-to-string "pbpaste")))
    (unless (string= text "")
      text)))

(defun lme-setup-clipboard ()
  "Set up clipboard integration."
  (setq select-enable-clipboard t
        save-interprogram-paste-before-kill t)
  (when (and (lme-macos-p)
             (not (lme-gui-p))
             (executable-find "pbcopy")
             (executable-find "pbpaste"))
    (setq interprogram-cut-function #'lme-pbcopy
          interprogram-paste-function #'lme-pbpaste)))

(defun lme-clipboard-text ()
  "Return text from the system clipboard or kill ring."
  (or (and (lme-macos-p) (executable-find "pbpaste") (lme-pbpaste))
      (current-kill 0 t)))

(defun lme-selection-range ()
  "Return the active selection range."
  (cond
   ((and (bound-and-true-p evil-local-mode)
         (eq evil-state 'visual)
         evil-visual-beginning
         evil-visual-end)
    (cons evil-visual-beginning evil-visual-end))
   ((use-region-p)
    (cons (region-beginning) (region-end)))))

(defun lme-clear-selection ()
  "Clear the active selection."
  (when (and (bound-and-true-p evil-local-mode)
             (eq evil-state 'visual))
    (evil-normal-state))
  (deactivate-mark))

(defvar-local lme-copy-function nil
  "Command copying this buffer's selection, for buffers whose selection
is not an Emacs region, such as a web page; nil copies the region.")

(defvar-local lme-paste-function nil
  "Function inserting the clipboard text, its one argument, for buffers
that cannot take it as buffer text, such as a web page; nil inserts it.")

(defun lme-copy-region ()
  "Copy the active region, or this buffer's selection via `lme-copy-function'."
  (interactive)
  (let ((range (lme-selection-range)))
    (cond
     (lme-copy-function (call-interactively lme-copy-function))
     (range
      (kill-new (filter-buffer-substring (car range) (cdr range)))
      (lme-clear-selection)
      (message "Copied"))
     (t (user-error "No active region")))))

(defun lme-cut-region ()
  "Cut the active region."
  (interactive)
  (let ((range (lme-selection-range)))
    (if range
        (let ((beg (car range))
              (end (cdr range)))
          (kill-new (filter-buffer-substring beg end))
          (unless (or (lme-eat-buffer-p) buffer-read-only)
            (delete-region beg end))
          (lme-clear-selection)
          (message "Cut"))
      (user-error "No active region"))))

(defun lme-eat-buffer-p ()
  "Return non-nil in an Eat terminal buffer."
  (bound-and-true-p eat-terminal))

(defun lme-eat-wrap-command-with-stty (args)
  "Wrap Eat command ARGS to initialize tty erase."
  (pcase-let ((`(,buffer ,name ,command ,startfile ,switches) args))
    (list buffer
          name
          "/bin/sh"
          startfile
          (list "-lc"
                (format "stty erase '^?'; exec %s"
                        (mapconcat #'shell-quote-argument
                                   (cons command switches)
                                   " "))))))

(defun lme-eat-send-backspace ()
  "Send a backward delete character to Eat."
  (interactive)
  (eat-term-send-string eat-terminal "\C-?"))

(defconst lme-eat-terminal-key-sequences
  '(("<escape>" . "\e")
    ("S-<return>" . "\e[13;2u")
    ("C-<return>" . "\e[13;5u")
    ("C-u" . "\C-u")
    ("C-w" . "\C-w")
    ("C-z" . "\C-z"))
  "Keys Emacs would keep from a terminal, with the bytes they send instead.
Escape alone is otherwise a Meta prefix waiting for its next key, so agents
never see it (both stop their work on it).  Return with Shift or Control
has no byte of its own; it is sent in the CSI u form Claude Code and Codex
read as a new line and, for Claude, sending a queued message now.  C-u
clears the input line rather than starting a prefix argument, C-w deletes
a word rather than cutting a region, and C-z reaches the program rather
than leaving Evil's Emacs state.")

(defun lme-eat-send-terminal-key ()
  "Send the terminal the bytes of the key just pressed.
The bytes come from `lme-eat-terminal-key-sequences'."
  (interactive)
  (when-let* ((sequence (cdr (assoc (key-description (this-command-keys-vector))
                                    lme-eat-terminal-key-sequences))))
    (eat-term-send-string eat-terminal sequence)))

(defvar lme-eat-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-x") #'execute-extended-command)
    (define-key map (kbd "M-c") #'lme-copy-region)
    (define-key map (kbd "M-v") #'lme-paste)
    (define-key map (kbd "M-w") #'lme-copy-region)
    (define-key map (kbd "C-y") #'lme-paste)
    (define-key map (kbd "DEL") #'lme-eat-send-backspace)
    (define-key map (kbd "<backspace>") #'lme-eat-send-backspace)
    (define-key map (kbd "<delete>") #'lme-eat-send-backspace)
    (define-key map (kbd "<deletechar>") #'lme-eat-send-backspace)
    (dolist (key lme-eat-terminal-key-sequences)
      (define-key map (kbd (car key)) #'lme-eat-send-terminal-key))
    map)
  "Keymap of Eat terminals: the unified clipboard keys, and the keys of
`lme-eat-terminal-key-sequences' passed through to the program.  Every
other key behaves as Eat binds it.")

(define-minor-mode lme-eat-keys-mode
  "Use the unified clipboard keys and pass agent keys through in Eat.
Its keymap takes precedence over every mode keymap, see
`emulation-mode-map-alists'."
  :init-value nil
  :lighter nil
  :keymap lme-eat-keys-mode-map)

(defvar lme-eat-keys-mode-map-alist
  `((lme-eat-keys-mode . ,lme-eat-keys-mode-map)))

(add-to-list 'emulation-mode-map-alists 'lme-eat-keys-mode-map-alist)

(defun lme-eat-terminal-keys ()
  "Give the current Eat terminal its keys by turning on `lme-eat-keys-mode'.
Keymaps earlier in `emulation-mode-map-alists' win, and Evil, loaded after
this file, puts its own first; its Emacs state binds C-w and C-z.  The
terminal keymap is moved to the front again, so in a terminal it outranks
Evil; elsewhere it is inactive and changes nothing."
  (setq emulation-mode-map-alists
        (cons 'lme-eat-keys-mode-map-alist
              (delq 'lme-eat-keys-mode-map-alist emulation-mode-map-alists)))
  (lme-eat-keys-mode 1))

(defun lme-paste ()
  "Paste from the system clipboard or kill ring.
A buffer's `lme-paste-function', when set, receives the text instead."
  (interactive)
  (let ((text (lme-clipboard-text)))
    (unless text
      (user-error "Clipboard is empty"))
    (cond (lme-paste-function (funcall lme-paste-function text))
          ((lme-eat-buffer-p)
           (eat-term-send-string-as-yank eat-terminal (list text)))
          (t (insert-for-yank text)))))

(defconst lme-area-prefixes
  '(tab-bar tab-line header-line mode-line menu-bar tool-bar vertical-line
    vertical-scroll-bar horizontal-scroll-bar left-fringe right-fringe
    left-margin right-margin right-divider bottom-divider internal-border
    child-frame-border)
  "Symbols `read-event' returns before a mouse event over part of a frame
or window other than text: the event follows.")

(defun lme-typed-event-p (event)
  "Return non-nil when EVENT was typed: a character or a function key, not
a mouse event, nor the area symbol before one, see `lme-area-prefixes'."
  (or (integerp event)
      (and (symbolp event) event (not (memq event lme-area-prefixes)))))

(provide 'lme-core)
;;; lme-core.el ends here
