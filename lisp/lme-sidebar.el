;;; lme-sidebar.el --- The left sidebar  -*- lexical-binding: t; -*-

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

;; The left sidebar and its views: files, shells and open buffers, plus the
;; parts every view is built from (toolbar, chips, two-line entries).  The
;; agents view lives in lme-agent-list.el.

;;; Code:

(require 'cl-lib)
(require 'lme-platinum)

;;;; Left sidebar views
;;
;; The left sidebar shows at most one view at a time.  Each tab bar button
;; selects a view; selecting the view already shown hides the sidebar.  The
;; main window is never touched:
;;
;;   none --files--> files --agents--> agents --agents--> none

(declare-function lme-agent--main-window "lme-ai" ())
(declare-function lme-agent--project-directory "lme-ai" ())
(declare-function lme-agent--fresh-name "lme-ai" (agent directory))
(declare-function lme-agent--buffer-p "lme-ai" (buffer))
(declare-function lme-agent--running-p "lme-ai" (buffer))
(declare-function lme-agent--independent-environment "lme-ai" (environment))
(declare-function lme-agent-list-visible-p "lme-agent-list" ())
(declare-function lme-agent-list-show "lme-agent-list" ())
(declare-function lme-agent-list-hide "lme-agent-list" ())
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(defvar explicit-shell-file-name)
(defvar lme-sidebar-views)

;;;; Sidebar parts
;;
;; Every sidebar view is a side window of `lme-sidebar-width' laid out as a
;; Finder window in list view: the title bar every window wears (see
;; lme-platinum.el) names the view, a header below it holds the view's
;; buttons, and the content is a list of one-line rows under column
;; headings.  The title bar and header stay put while the rows scroll.

(defun lme-sidebar--one-line (text width)
  "Return TEXT collapsed to a single line and truncated to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
   width nil nil "…"))

(defun lme-sidebar--toolbar-command (command)
  "Return a mouse command running COMMAND in the window whose toolbar is clicked.
Toolbar clicks do not select their window, so without this a command such as
creating a file would act on the main window's buffer instead of the view."
  (lambda (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (call-interactively command))))

(defun lme-ansi-apply-faces (&rest _)
  "Keep the two black ANSI colours legible on the active theme.
Dracula draws ANSI black in the editor background and bright black in
pure black, so terminal programs' grey text (Codex streams its status
and reasoning in bright black) vanished until selected.  Bright black
takes the theme's own dimmed text colour, and black a shade below the
background, as in Dracula's terminal palette.  Every terminal and
compilation buffer reads these faces.  A light theme is left alone: on
white, black is legible as it is.  Colours come only from faces this
function never sets, so re-running it is idempotent."
  (require 'ansi-color)
  (when-let* ((background (face-background 'default nil t))
              (dark (< (apply #'+ (or (lme-tab-bar--rgb background) '(1 1 1))) 1.5))
              (dim (face-foreground 'shadow nil t))
              (black (lme-tab-bar--blend background "#000000" 0.2)))
    (set-face-attribute 'ansi-color-bright-black nil :foreground dim :background dim)
    (set-face-attribute 'ansi-color-black nil :foreground black :background black)))

(defun lme-sidebar--toolbar-button (label command help row &optional face)
  "Return LABEL as a button calling COMMAND, described by HELP, on toolbar ROW.
ROW is `tab-line' or `header-line', the window line the button sits in.
FACE, by default `lme-platinum-button', styles the label and is layered
under LABEL's own faces, so icons keep their Nerd Font family.
The press is bound too: unbound, it reaches the global line-dragging command
\(`mouse-drag-tab-line', `mouse-drag-header-line'), which tracks the mouse
and swallows the release, so the click would never arrive."
  (let ((styled (copy-sequence label)))
    (add-face-text-property 0 (length styled) (or face 'lme-platinum-button) t styled)
    (propertize styled
                'mouse-face 'highlight
                'help-echo help
                'keymap (let ((map (make-sparse-keymap)))
                          (keymap-set map (format "<%s> <down-mouse-1>" row) #'ignore)
                          (keymap-set map (format "<%s> <mouse-1>" row)
                                      (lme-sidebar--toolbar-command command))
                          map))))

(defun lme-sidebar--toolbar-buttons (actions row)
  "Return ACTIONS as bevel buttons on ROW.
Each action is (LABEL COMMAND HELP [PRESSED]); PRESSED draws its button
pushed in, as a toggle that is on."
  (mapconcat (pcase-lambda (`(,label ,command ,help ,pressed))
               (lme-sidebar--toolbar-button (concat " " label " ") command help row
                                            (when pressed 'lme-platinum-button-pressed)))
             actions " "))

(defvar-local lme-sidebar--faces nil
  "Face remapping cookies drawing this buffer as a list view.")

(defun lme-sidebar-set-toolbar (view refresh actions &optional title-actions)
  "Give the current buffer the title bar and header of sidebar VIEW.
VIEW, a key of `lme-sidebar-views', names the title bar and gives its
icon.  The header shows ACTIONS at its left end and, at its right end,
TITLE-ACTIONS followed by a button calling REFRESH.  Both action lists
hold (LABEL COMMAND HELP [PRESSED]) entries, see
`lme-sidebar--toolbar-buttons'."
  (let ((spec (alist-get view lme-sidebar-views)))
    (setq-local lme-platinum-title (plist-get spec :title)
                lme-platinum-icon (lme-platinum-icon (plist-get spec :icon))))
  (kill-local-variable 'tab-line-format)
  (unless lme-sidebar--faces
    (setq-local lme-sidebar--faces
                (list (face-remap-add-relative 'default 'lme-platinum-list)
                      (face-remap-add-relative 'header-line 'lme-platinum-header))))
  ;; The right-hand buttons end a few pixels short of the edge.  Their width
  ;; is measured, as bevels and icons are not whole columns.
  (let ((trailing (concat (when title-actions
                            (concat (lme-sidebar--toolbar-buttons title-actions 'header-line) " "))
                          (lme-sidebar--toolbar-button " ⟳ " refresh "Refresh" 'header-line))))
    (setq-local header-line-format
                (concat " " (lme-sidebar--toolbar-buttons actions 'header-line)
                        (propertize " " 'display
                                    `(space :align-to
                                            (- right (,(+ (string-pixel-width trailing) 6)))))
                        trailing))))

(defun lme-sidebar--setup-buffer ()
  "Set up the current buffer as sidebar content.
Visual line mode is turned off explicitly, so `global-visual-line-mode'
leaves the buffer alone and an overlong title is truncated, not wrapped.
Rows are spaced as Finder spaced its list.  The window's bottom edge
counts the rows, from `lme-platinum-info', as Finder's counted items."
  (visual-line-mode -1)
  (setq-local truncate-lines t
              cursor-type nil
              line-spacing 0.35)
  (display-line-numbers-mode -1))

(defun lme-sidebar--count (n singular &optional detail)
  "Return N SINGULAR things counted for a list window's bottom edge.
DETAIL, when non-nil, follows after a comma."
  (concat (format "%d %s%s" n singular (if (= n 1) "" "s"))
          (when detail (concat ", " detail))))

(defun lme-sidebar--display (buffer)
  "Show BUFFER as the left sidebar of this frame and return its window."
  (display-buffer-in-side-window
   buffer `((side . left)
            (slot . -1)
            (window-width . ,lme-sidebar-width)
            (preserve-size . (t . nil))
            (window-parameters . ((no-delete-other-windows . t)
                                  (no-other-window . t))))))

(defun lme-sidebar--hide-buffer (name)
  "Hide the sidebar window showing the buffer called NAME."
  (when-let* ((window (get-buffer-window name)))
    (delete-window window)))

(defun lme-sidebar-show-in-main (buffer)
  "Show BUFFER in the main area and select it.
Terminals go to the left of the main area, see `lme-main-area-show-terminal'.
Other buffers are shown from the most recently used main window through
`display-buffer', so files obey the main area's right-hand rule."
  (if (lme-terminal-buffer-p buffer)
      (lme-main-area-show-terminal buffer)
    (select-window (lme-agent--main-window))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun lme-sidebar--insert-styled-button (label action help)
  "Insert LABEL, whose segments carry their own faces, as a button.
The button sets no face, so icon fonts and colours inside LABEL survive."
  (let ((beg (point)))
    (insert label)
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)))

(defun lme-sidebar--insert-columns (name meta)
  "Insert the column headings NAME and META, the list being sorted by META."
  (insert (lme-platinum-columns name meta)))

(defconst lme-sidebar-indent 18
  "Pixels each level of a list is indented by, the width of a triangle and gap.")

(defun lme-sidebar--row-width ()
  "Return the pixel width rows have to fill: the sidebar's text area."
  (if-let* ((window (get-buffer-window (current-buffer))))
      (window-body-width window t)
    (* lme-sidebar-width (frame-char-width))))

(defun lme-sidebar--blank (spec)
  "Return a blank displayed as SPEC, in the list's face.
Every part of a row carries a face of its own: a part without one would
take the `button' face of the row, underline and all."
  (propertize " " 'display spec 'face 'lme-platinum-list))

(defun lme-sidebar--row (lead name meta width)
  "Return a row of a list WIDTH pixels wide: LEAD, NAME, and META at its right.
LEAD (indentation, a triangle or an icon) and META keep their size; NAME
is shortened to fit between them."
  (let* ((meta (or meta ""))
         (meta-width (string-pixel-width (propertize meta 'face 'lme-platinum-list)))
         (room (- width (string-pixel-width (propertize lead 'face 'lme-platinum-list))
                  meta-width 36)))
    (concat lead
            (lme-platinum--fit (propertize name 'face (or (get-text-property 0 'face name)
                                                          'lme-platinum-list))
                               (max 0 room))
            (lme-sidebar--blank `(space :align-to (- right (,(+ meta-width 10)))))
            meta
            (lme-sidebar--blank '(space :align-to right)))))

(defun lme-sidebar--insert-entry (icon title meta action help &optional properties depth)
  "Insert a one-line row and make it one button calling ACTION.
The row shows ICON and TITLE, indented by DEPTH levels, and META, dimmed,
in the right-hand column; HELP describes the button, which spans the row
to the window's right edge.  PROPERTIES, a property list, is added to the
row's text."
  (let* ((beg (point))
         (meta (copy-sequence (or meta ""))))
    ;; The dimming is layered, so coloured marks such as "running" survive;
    ;; the button below sets no face, which would replace the icon's Nerd
    ;; Font family.
    (add-face-text-property 0 (length meta) 'shadow t meta)
    (insert (lme-sidebar--row
             (concat (lme-sidebar--blank `(space :width (,(+ 8 lme-sidebar-indent
                                                             (* lme-sidebar-indent (or depth 0))))))
                     icon (propertize " " 'face 'lme-platinum-list))
             (lme-sidebar--one-line title 200)
             meta
             (lme-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (when properties
      (add-text-properties beg (point) properties))
    (insert "\n")))

(defun lme-sidebar--insert-group (open name meta action help)
  "Insert a group's row: a disclosure triangle, OPEN or not, a folder and NAME.
META sits in the right-hand column; clicking the row calls ACTION,
described by HELP."
  (let ((beg (point)))
    (insert (lme-sidebar--row
             (concat (lme-sidebar--blank '(space :width (8)))
                     (propertize (lme-platinum-triangle open) 'face 'lme-platinum-list)
                     (lme-sidebar--blank '(space :width (6)))
                     (lme-platinum-icon "folder")
                     (lme-sidebar--blank '(space :width (6))))
             (propertize (lme-sidebar--one-line name 200)
                         'face '(:inherit lme-platinum-list :weight bold))
             (propertize (or meta "") 'face 'shadow)
             (lme-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (insert "\n")))

(defun lme-sidebar--insert-heading (name)
  "Insert NAME as the heading of a group that does not fold."
  (insert (propertize " " 'display '(space :width (8)))
          (propertize name 'face '(:inherit (shadow lme-platinum-list) :weight bold))
          "\n"))

;;;; Shell sidebar

(defconst lme-shell-list-buffer-name "*Shells*"
  "Name of the shell sidebar buffer.")

(defun lme-shell--buffer-p (buffer)
  "Return non-nil when BUFFER is a live Eat shell, not an agent terminal."
  (and (buffer-live-p buffer)
       (eq (buffer-local-value 'major-mode buffer) 'eat-mode)
       (not (lme-agent--buffer-p buffer))
       (lme-agent--running-p buffer)))

(defun lme-shell-new (&optional directory)
  "Start a new shell in DIRECTORY, by default the main window's project root.
The shell is the one Eat would start, run as an independent process."
  (interactive)
  (require 'eat)
  (let* ((directory (or directory
                        (with-selected-window (lme-agent--main-window)
                          (lme-agent--project-directory))))
         (default-directory (file-name-as-directory directory))
         (process-environment (lme-agent--independent-environment process-environment))
         (program (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))
    (lme-sidebar-show-in-main
     (eat-make (lme-agent--fresh-name 'shell directory) program)))
  (lme-shell-list-refresh))

(defun lme-shell-list--render ()
  "Render the running shells into the current buffer."
  (let ((inhibit-read-only t)
        (shells (seq-filter #'lme-shell--buffer-p (buffer-list))))
    (erase-buffer)
    (lme-sidebar-set-toolbar
     'shells #'lme-shell-list-refresh
     '(("New Shell" lme-shell-new "New shell at the main window's project root")))
    (setq lme-platinum-info (lme-sidebar--count (length shells) "shell"))
    (lme-sidebar--insert-columns "Name" "Directory")
    (if (null shells)
        (insert (propertize " " 'display '(space :width (8)))
                (propertize "No running shells" 'face 'shadow) "\n")
      (dolist (buffer shells)
        (let ((directory (buffer-local-value 'default-directory buffer)))
          (lme-sidebar--insert-entry
           (lme-platinum-icon "computer")
           (string-trim (buffer-name buffer) "\\*" "\\*")
           (file-name-nondirectory (directory-file-name directory))
           (apply-partially #'lme-sidebar-show-in-main buffer)
           (format "Show this shell, in %s" (abbreviate-file-name directory))))))))

(defun lme-shell-list-refresh (&rest _)
  "Re-render the shell sidebar when it exists."
  (interactive)
  (when-let* ((buffer (get-buffer lme-shell-list-buffer-name)))
    (with-current-buffer buffer
      (lme-shell-list--render))))

(defvar-keymap lme-shell-list-mode-map
  :doc "Keymap for the shell sidebar."
  :parent special-mode-map
  "g" #'lme-shell-list-refresh
  "+" #'lme-shell-new
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lme-shell-list-mode special-mode "Shells"
  "Sidebar listing running shells.
\\{lme-shell-list-mode-map}"
  (lme-sidebar--setup-buffer))

(defun lme-shell-list-show ()
  "Show the shell sidebar on the left of this frame and return its window."
  (let ((buffer (get-buffer-create lme-shell-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lme-shell-list-mode)
        (lme-shell-list-mode))
      (lme-shell-list--render)
      (goto-char (point-min)))
    (lme-sidebar--display buffer)))

(defun lme-shell-list-visible-p ()
  "Return non-nil when the shell sidebar is shown in the selected frame."
  (and (get-buffer-window lme-shell-list-buffer-name) t))

(defun lme-shell-list-hide ()
  "Hide the shell sidebar in the selected frame."
  (lme-sidebar--hide-buffer lme-shell-list-buffer-name))

;;;; Buffers sidebar

(defconst lme-buffer-list-buffer-name "*Buffers*"
  "Name of the open buffers sidebar buffer.")

(defconst lme-buffer-list-groups
  '((files "Files") (terminals "Terminals") (other "Other"))
  "Groups of the open buffers sidebar, in display order, with their headings.")

(defvar lme-agent-list-buffer-name)

(defun lme-buffer-list--kind (buffer)
  "Return the group of BUFFER: `files', `terminals' or `other'."
  (cond ((lme-terminal-buffer-p buffer) 'terminals)
        ((buffer-file-name buffer) 'files)
        (t 'other)))

(defun lme-buffer-list--candidates ()
  "Return the open buffers to list, from the current workspace when isolated.
Internal buffers (named with a leading space) and the sidebars themselves are
left out."
  (let ((sidebars (list lme-agent-list-buffer-name lme-shell-list-buffer-name
                        lme-buffer-list-buffer-name)))
    (seq-remove (lambda (buffer)
                  (let ((name (buffer-name buffer)))
                    (or (string-prefix-p " " name)
                        (member name sidebars)
                        (eq (buffer-local-value 'major-mode buffer) 'treemacs-mode)
                        (minibufferp buffer))))
                (if (bound-and-true-p tabspaces-mode)
                    (tabspaces-local-buffer-list)
                  (buffer-list)))))

(defun lme-buffer-list--mode-label (buffer)
  "Return a readable name of BUFFER's major mode.
A plain string `mode-name' is used as is; a mode line construct is formatted;
an empty result falls back to the major mode's symbol."
  (let* ((name (buffer-local-value 'mode-name buffer))
         (label (if (stringp name) name (format-mode-line name nil nil buffer))))
    (if (string-empty-p label)
        (symbol-name (buffer-local-value 'major-mode buffer))
      label)))

(defun lme-buffer-list--entry (buffer)
  "Insert the sidebar row of BUFFER.
Its right-hand column names a file's directory, a terminal's working
directory, and any other buffer's major mode, or its directory when the
mode merely repeats the buffer's name, as *Messages* in Messages mode
does.  The full directory is in the row's help."
  (let* ((kind (lme-buffer-list--kind buffer))
         (title (or (and (eq kind 'terminals) (lme-agent-buffer-label buffer))
                    (string-trim (buffer-name buffer) "\\*" "\\*")))
         (directory (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
         (place (file-name-nondirectory (directory-file-name directory))))
    (lme-sidebar--insert-entry
     (lme-platinum-buffer-icon buffer)
     title
     (pcase kind
       ('files (concat (when (buffer-modified-p buffer)
                         (propertize "● " 'face 'warning))
                       place))
       ('terminals place)
       ('other (let ((mode (lme-buffer-list--mode-label buffer)))
                 (if (string-equal-ignore-case mode title) place mode))))
     (apply-partially #'lme-sidebar-show-in-main buffer)
     (format "Show %s, in %s" (buffer-name buffer) directory))))

(defvar lme-buffer-list--rendering nil
  "Non-nil while the open buffers sidebar renders, to ignore its own effects.")

(defun lme-buffer-list--render ()
  "Render the open buffers, grouped by kind, into the current buffer."
  (let ((inhibit-read-only t)
        (lme-buffer-list--rendering t)
        (buffers (lme-buffer-list--candidates)))
    (erase-buffer)
    (lme-sidebar-set-toolbar 'buffers #'lme-buffer-list-refresh
                            '(("Save All" save-some-buffers "Save modified file buffers")))
    (setq lme-platinum-info (lme-sidebar--count (length buffers) "buffer"))
    (lme-sidebar--insert-columns "Name" "Where")
    (pcase-dolist (`(,kind ,heading) lme-buffer-list-groups)
      (when-let* ((members (seq-filter (lambda (buffer) (eq (lme-buffer-list--kind buffer) kind))
                                       buffers)))
        (lme-sidebar--insert-heading heading)
        (mapc #'lme-buffer-list--entry members)))))

(defun lme-buffer-list-refresh (&rest _)
  "Re-render the open buffers sidebar when it exists, keeping point's line."
  (interactive)
  (when-let* ((buffer (get-buffer lme-buffer-list-buffer-name)))
    (with-current-buffer buffer
      (let ((line (line-number-at-pos)))
        (lme-buffer-list--render)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defvar lme-buffer-list--refresh-timer nil
  "Pending idle timer refreshing the open buffers sidebar, if any.")

(defun lme-buffer-list--schedule-refresh ()
  "Refresh the open buffers sidebar once Emacs is idle, when it is shown.
Run from `buffer-list-update-hook', which fires on every buffer switch: the
check is cheap, refreshes coalesce into one idle timer, and the sidebar's own
rendering never schedules another."
  (unless (or lme-buffer-list--rendering
              lme-buffer-list--refresh-timer
              (not (get-buffer-window lme-buffer-list-buffer-name t)))
    (setq lme-buffer-list--refresh-timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (setq lme-buffer-list--refresh-timer nil)
                                 (lme-buffer-list-refresh))))))

(defvar-keymap lme-buffer-list-mode-map
  :doc "Keymap for the open buffers sidebar."
  :parent special-mode-map
  "g" #'lme-buffer-list-refresh
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lme-buffer-list-mode special-mode "Buffers"
  "Sidebar listing the open buffers of the current workspace.
\\{lme-buffer-list-mode-map}"
  (lme-sidebar--setup-buffer))

(defun lme-buffer-list-show ()
  "Show the open buffers sidebar on the left of this frame and return its window."
  (let ((buffer (get-buffer-create lme-buffer-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lme-buffer-list-mode)
        (lme-buffer-list-mode))
      (lme-buffer-list--render)
      (goto-char (point-min)))
    (lme-sidebar--display buffer)))

(defun lme-buffer-list-visible-p ()
  "Return non-nil when the open buffers sidebar is shown in the selected frame."
  (and (get-buffer-window lme-buffer-list-buffer-name) t))

(defun lme-buffer-list-hide ()
  "Hide the open buffers sidebar in the selected frame."
  (lme-sidebar--hide-buffer lme-buffer-list-buffer-name))

;;;; Files sidebar

(declare-function treemacs-current-workspace "treemacs-workspaces" ())
(declare-function treemacs-workspace->projects "treemacs-workspaces" (workspace))
(declare-function treemacs-project->path "treemacs-workspaces" (project))

(defun lme-sidebar--files-info ()
  "Return what the project tree's bottom edge says: the project shown."
  (if-let* ((projects (treemacs-workspace->projects (treemacs-current-workspace))))
      (abbreviate-file-name (treemacs-project->path (car projects)))
    "No project"))

(defun lme-sidebar--files-toolbar ()
  "Give the project tree buffer its sidebar toolbar and bottom edge."
  (setq lme-platinum-info #'lme-sidebar--files-info)
  (lme-sidebar-set-toolbar
   'files #'treemacs-refresh
   '(("New File" treemacs-create-file "New file in the selected directory")
     ("New Folder" treemacs-create-dir "New folder in the selected directory")
     ("Collapse" treemacs-collapse-all-projects "Collapse all"))))

(defun lme-sidebar--files-visible-p ()
  "Return non-nil when the project tree is shown in the selected frame."
  (and (featurep 'treemacs) (eq (treemacs-current-visibility) 'visible)))

(defun lme-sidebar--files-show ()
  "Show the project tree of the main window's buffer, keeping the selection.
Outside any project treemacs falls back to the buffer's directory."
  (require 'treemacs)
  (save-selected-window
    (with-current-buffer (window-buffer (lme-agent--main-window))
      (treemacs-add-and-display-current-project-exclusively))))

(defun lme-sidebar--files-hide ()
  "Hide the project tree in the selected frame."
  (when-let* ((window (and (featurep 'treemacs) (treemacs-get-local-window))))
    (delete-window window)))

(defconst lme-sidebar-views
  '((files :title "Files" :icon "disk" :command lme-sidebar-files
           :help "Project files"
           :visible lme-sidebar--files-visible-p
           :show lme-sidebar--files-show
           :hide lme-sidebar--files-hide)
    (agents :title "Agents" :icon lambda :command lme-sidebar-agents
            :help "Agent sessions"
            :visible lme-agent-list-visible-p
            :show lme-agent-list-show
            :hide lme-agent-list-hide)
    (shells :title "Shells" :icon "computer" :command lme-sidebar-shells
            :help "Shells"
            :visible lme-shell-list-visible-p
            :show lme-shell-list-show
            :hide lme-shell-list-hide)
    (buffers :title "Buffers" :icon "document" :command lme-sidebar-buffers
             :help "Open buffers"
             :visible lme-buffer-list-visible-p
             :show lme-buffer-list-show
             :hide lme-buffer-list-hide))
  "Views the left sidebar can show, in menu bar order.
Each view names its TITLE, its Finder icon (see `lme-platinum-icon'),
the COMMAND its menu bar title
runs, and the functions that test, show and hide it in the selected frame.")

(defun lme-sidebar-current-view ()
  "Return the view the left sidebar shows, or nil when it is hidden."
  (car (seq-find (lambda (view) (funcall (plist-get (cdr view) :visible)))
                 lme-sidebar-views)))

(defun lme-sidebar-select (view)
  "Show VIEW in the left sidebar, or hide the sidebar when VIEW is shown.
Every other view is hidden first, so at most one is ever visible."
  (let ((current (lme-sidebar-current-view)))
    (dolist (entry lme-sidebar-views)
      (when (funcall (plist-get (cdr entry) :visible))
        (funcall (plist-get (cdr entry) :hide))))
    (unless (eq view current)
      (funcall (plist-get (alist-get view lme-sidebar-views) :show))))
  (force-mode-line-update t))

(defun lme-sidebar-files ()
  "Show the project tree in the left sidebar, or hide it when shown."
  (interactive)
  (lme-sidebar-select 'files))

(defun lme-sidebar-shells ()
  "Show the running shells in the left sidebar, or hide them when shown."
  (interactive)
  (lme-sidebar-select 'shells))

(defun lme-sidebar-agents ()
  "Show the agent sessions in the left sidebar, or hide them when shown."
  (interactive)
  (lme-sidebar-select 'agents))

(defun lme-sidebar-buffers ()
  "Show the open buffers in the left sidebar, or hide them when shown."
  (interactive)
  (lme-sidebar-select 'buffers))


(provide 'lme-sidebar)
;;; lme-sidebar.el ends here
