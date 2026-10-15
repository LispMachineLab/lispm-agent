;;; lispm-desktop-sidebar.el --- The left sidebar  -*- lexical-binding: t; -*-

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

;; The left sidebar and its views: files, shells and open buffers, plus the
;; parts every view is built from (toolbar, chips, two-line entries).  The
;; agents view lives in lispm-desktop-agent-list.el.

;;; Code:

(require 'cl-lib)
(require 'lispm-desktop-platinum)

;;;; Left sidebar views
;;
;; The left sidebar shows at most one view at a time.  Each tab bar button
;; selects a view; selecting the view already shown hides the sidebar.  The
;; main window is never touched:
;;
;;   none --files--> files --agents--> agents --agents--> none

(declare-function lispm-desktop-agent--main-window "lispm-desktop-ai" ())
(declare-function lispm-desktop-agent--project-directory "lispm-desktop-ai" ())
(declare-function lispm-desktop-agent--fresh-name "lispm-desktop-ai" (agent directory))
(declare-function lispm-desktop-agent--buffer-p "lispm-desktop-ai" (buffer))
(declare-function lispm-desktop-agent--running-p "lispm-desktop-ai" (buffer))
(declare-function lispm-desktop-agent--independent-environment "lispm-desktop-ai" (environment))
(declare-function lispm-desktop-agent-list-visible-p "lispm-desktop-agent-list" ())
(declare-function lispm-desktop-agent-list-show "lispm-desktop-agent-list" ())
(declare-function lispm-desktop-agent-list-hide "lispm-desktop-agent-list" ())
(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(defvar explicit-shell-file-name)
(defvar lispm-desktop-sidebar-views)

;;;; Sidebar parts
;;
;; Every sidebar view is a side window of `lispm-desktop-sidebar-width' laid out as a
;; Finder window in list view: the title bar every window wears (see
;; lispm-desktop-platinum.el) names the view, a header below it holds the view's
;; buttons, and the content is a list of one-line rows under column
;; headings.  The title bar and header stay put while the rows scroll.

(defun lispm-desktop-sidebar--one-line (text width)
  "Return TEXT collapsed to a single line and truncated to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
   width nil nil "…"))

(defun lispm-desktop-ansi-apply-faces (&rest _)
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
              (dark (< (apply #'+ (or (lispm-desktop-tab-bar--rgb background) '(1 1 1))) 1.5))
              (dim (face-foreground 'shadow nil t))
              (black (lispm-desktop-tab-bar--blend background "#000000" 0.2)))
    (set-face-attribute 'ansi-color-bright-black nil :foreground dim :background dim)
    (set-face-attribute 'ansi-color-black nil :foreground black :background black)))

(defun lispm-desktop-sidebar--trim (label)
  "Return LABEL without the plain spaces at its ends.
A space showing an icon, as an image or a stretch, stays."
  (let ((start 0) (end (length label)))
    (while (and (< start end) (eq (aref label start) ?\s)
                (not (get-text-property start 'display label)))
      (setq start (1+ start)))
    (while (and (> end start) (eq (aref label (1- end)) ?\s)
                (not (get-text-property (1- end) 'display label)))
      (setq end (1- end)))
    (substring label start end)))

(defun lispm-desktop-sidebar--toolbar-button (label command help row &optional state)
  "Return LABEL as a bevel button calling COMMAND, described by HELP.
It sits on toolbar ROW, the window line holding it: `tab-line',
`header-line' or `mode-line'.  LABEL is shown in the list's face, its
icons keeping their own faces.  STATE is nil, `pressed' for a toggle
that is on, or `disabled', when the button does nothing.  STATE may be
a face instead: LABEL is then text in that face, as the browser's
Netscape toolbars draw theirs.  The button is pressed rather than
clicked: drawn pushed in while held, and run on release over it, see
`lispm-desktop-platinum-press'.  Built while its line is drawn, as
toolbars are, it shows itself pushed in from the window's
`lispm-desktop-platinum-pressed'."
  (let* ((face (and (facep state) state))
         (label (if face label (lispm-desktop-sidebar--trim label)))
         ;; The command tells buttons apart: `equal' ignores text
         ;; properties, and icons drawn as images all read " ", so the new
         ;; session buttons of two agents with image icons had one label.
         (target (list row command label))
         (pressed (lispm-desktop-platinum--pressed-p target))
         (button (if face
                     (let ((styled (copy-sequence label)))
                       (add-face-text-property 0 (length styled)
                                               (if pressed 'lispm-desktop-platinum-button-pressed face)
                                               t styled)
                       styled)
                   (lispm-desktop-platinum-bevel-button label (if pressed 'pressed state)))))
    (if (eq state 'disabled)
        button
      (propertize button
                  'help-echo help
                  'lispm-desktop-platinum-target target
                  'keymap (let ((map (make-sparse-keymap)))
                            (keymap-set map (format "<%s> <down-mouse-1>" row)
                                        (lambda (event)
                                          (interactive "e")
                                          (let ((start (event-start event)))
                                            (lispm-desktop-platinum-press
                                             start (lispm-desktop-platinum--same-target-p start)
                                             (lambda ()
                                               (with-selected-window (posn-window start)
                                                 (call-interactively command)))))))
                            (keymap-set map (format "<%s> <mouse-1>" row) #'ignore)
                            map)))))

(defun lispm-desktop-sidebar--toolbar-buttons (actions row)
  "Return ACTIONS as bevel buttons on ROW.
Each action is (LABEL COMMAND HELP [PRESSED]); PRESSED draws its button
pushed in, as a toggle that is on."
  (mapconcat (pcase-lambda (`(,label ,command ,help ,pressed))
               (lispm-desktop-sidebar--toolbar-button label command help row (and pressed 'pressed)))
             actions (propertize " " 'display '(space :width (4)))))

(defvar-local lispm-desktop-sidebar--faces nil
  "Face remapping cookies drawing this buffer as a list view.")

(defvar-local lispm-desktop-sidebar--header nil
  "This list window's header: (ACTIONS REFRESH TITLE-ACTIONS).")

(defun lispm-desktop-sidebar--header-line ()
  "Return the header of the list window being drawn, its buttons pressed or not.
ACTIONS sit at its left end; TITLE-ACTIONS and the refresh button end a
few pixels short of its right, their width measured, as bevels and icons
are not whole columns."
  (pcase-let* ((`(,actions ,refresh ,title-actions) lispm-desktop-sidebar--header)
               (trailing (concat (when title-actions
                                   (concat (lispm-desktop-sidebar--toolbar-buttons title-actions
                                                                                   'header-line)
                                           " "))
                                 (lispm-desktop-sidebar--toolbar-button "⟳" refresh "Refresh"
                                                                        'header-line))))
    (concat " " (lispm-desktop-sidebar--toolbar-buttons actions 'header-line)
            (propertize " " 'display
                        `(space :align-to (- right (,(+ (string-pixel-width trailing) 6)))))
            trailing)))

(defun lispm-desktop-sidebar-set-toolbar (view refresh actions &optional title-actions)
  "Give the current buffer the title bar and header of sidebar VIEW.
VIEW, a key of `lispm-desktop-sidebar-views', names the title bar and gives its
icon.  The header shows ACTIONS at its left end and, at its right end,
TITLE-ACTIONS followed by a button calling REFRESH.  Both action lists
hold (LABEL COMMAND HELP [PRESSED]) entries, see
`lispm-desktop-sidebar--toolbar-buttons'."
  (let ((spec (alist-get view lispm-desktop-sidebar-views)))
    (setq-local lispm-desktop-platinum-title (plist-get spec :title)
                lispm-desktop-platinum-icon (lispm-desktop-platinum-icon (plist-get spec :icon))))
  (kill-local-variable 'tab-line-format)
  (unless lispm-desktop-sidebar--faces
    (setq-local lispm-desktop-sidebar--faces
                (list (face-remap-add-relative 'default 'lispm-desktop-platinum-list)
                      (face-remap-add-relative 'header-line 'lispm-desktop-platinum-header))))
  (setq-local lispm-desktop-sidebar--header (list actions refresh title-actions)
              header-line-format '(:eval (lispm-desktop-sidebar--header-line))))

(defun lispm-desktop-sidebar--setup-buffer ()
  "Set up the current buffer as sidebar content.
Visual line mode is turned off explicitly, so `global-visual-line-mode'
leaves the buffer alone and an overlong title is truncated, not wrapped.
Rows are spaced as Finder spaced its list.  The window's bottom edge
counts the rows, from `lispm-desktop-platinum-info', as Finder's counted items.
A list shows no text cursor, whatever state Evil is in: the row at point
is highlighted in the selection colour instead, as Finder showed the
selected item."
  (visual-line-mode -1)
  (setq-local truncate-lines t
              cursor-type nil
              line-spacing 0.35)
  (dolist (cursor '(evil-emacs-state-cursor evil-normal-state-cursor
                    evil-motion-state-cursor))
    (set (make-local-variable cursor) '(nil)))
  (face-remap-add-relative 'hl-line 'region)
  (display-line-numbers-mode -1))

(defun lispm-desktop-sidebar--count (n singular &optional detail)
  "Return N SINGULAR things counted for a list window's bottom edge.
DETAIL, when non-nil, follows after a comma."
  (concat (format "%d %s%s" n singular (if (= n 1) "" "s"))
          (when detail (concat ", " detail))))

(defun lispm-desktop-sidebar--display (buffer)
  "Show BUFFER as the left sidebar of this frame and return its window.
The window is dedicated to BUFFER, so nothing else is ever shown in it:
switching buffers there goes to the main area instead."
  (display-buffer-in-side-window
   buffer `((side . left)
            (slot . -1)
            (window-width . ,lispm-desktop-sidebar-width)
            (preserve-size . (t . nil))
            (dedicated . t)
            (window-parameters . ((no-delete-other-windows . t)
                                  (no-other-window . t))))))

(defun lispm-desktop-sidebar-buffer-view (buffer)
  "Return the sidebar view BUFFER is the list of, or nil.
See the :mode of `lispm-desktop-sidebar-views'."
  (car (seq-find (lambda (view)
                   (when-let* ((mode (plist-get (cdr view) :mode)))
                     (eq (buffer-local-value 'major-mode buffer) mode)))
                 lispm-desktop-sidebar-views)))

(defun lispm-desktop-sidebar--view-buffer-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a sidebar list about to be shown.
This is the `display-buffer-alist' condition keeping lists in the sidebar."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (lispm-desktop-sidebar-buffer-view buffer)))

(defun lispm-desktop-sidebar-display-view (buffer _alist)
  "Show the sidebar view whose list BUFFER is, and return its window.
This is a `display-buffer' action function: however a list is asked for,
it is shown in the sidebar, never in the main area."
  (let ((view (lispm-desktop-sidebar-buffer-view buffer)))
    (unless (eq view (lispm-desktop-sidebar-current-view))
      (lispm-desktop-sidebar-select view))
    (get-buffer-window buffer)))

(defun lispm-desktop-sidebar--hide-buffer (name)
  "Hide the sidebar window showing the buffer called NAME."
  (when-let* ((window (get-buffer-window name)))
    (delete-window window)))

(defun lispm-desktop-sidebar-show-in-main (buffer)
  "Show BUFFER in the main area and select it.
Sessions go to the session column, see `lispm-desktop-main-area-show-terminal';
anything else to the file column, through `display-buffer', so a file of
another project still opens in that project's workspace."
  (if (lispm-desktop-terminal-buffer-p buffer)
      (lispm-desktop-main-area-show-terminal buffer)
    (select-window (display-buffer buffer '(lispm-desktop-main-area-display-file)))
    buffer))

(defun lispm-desktop-sidebar--insert-styled-button (label action help)
  "Insert LABEL, whose segments carry their own faces, as a button.
The button sets no face, so icon fonts and colours inside LABEL survive."
  (let ((beg (point)))
    (insert label)
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)))

(defun lispm-desktop-sidebar--insert-columns (name meta)
  "Insert the column headings NAME and META, the list being sorted by META."
  (insert (lispm-desktop-platinum-columns name meta)))

(defconst lispm-desktop-sidebar-indent 18
  "Pixels each level of a list is indented by, the width of a triangle and gap.")

(defun lispm-desktop-sidebar--row-width ()
  "Return the pixel width rows have to fill: the sidebar's text area."
  (if-let* ((window (get-buffer-window (current-buffer))))
      (window-body-width window t)
    (* lispm-desktop-sidebar-width (frame-char-width))))

(defun lispm-desktop-sidebar--blank (spec)
  "Return a blank displayed as SPEC, in the list's face.
Every part of a row carries a face of its own: a part without one would
take the `button' face of the row, underline and all."
  (propertize " " 'display spec 'face 'lispm-desktop-platinum-list))

(defun lispm-desktop-sidebar--column-pixels ()
  "Return the width in pixels of a list's right-hand column."
  (* lispm-desktop-platinum-column-width (frame-char-width)))

(defun lispm-desktop-sidebar--sorted-column (meta)
  "Return the right-hand column of a row holding META, flush right.
The column is shaded down its whole length, as Finder shaded the column
a list was sorted by."
  (let* ((meta (lispm-desktop-platinum--fit (propertize (or meta "") 'face
                                                        (or (get-text-property 0 'face (or meta ""))
                                                            'lispm-desktop-platinum-list))
                                            (- (lispm-desktop-sidebar--column-pixels) 16)))
         (meta-width (string-pixel-width meta)))
    (add-face-text-property 0 (length meta) 'lispm-desktop-platinum-sorted t meta)
    (concat (lispm-desktop-sidebar--blank `(space :align-to (- right ,lispm-desktop-platinum-column-width)))
            (propertize " " 'display `(space :align-to (- right (,(+ meta-width 10))))
                        'face 'lispm-desktop-platinum-sorted)
            meta
            (propertize " " 'display '(space :align-to right) 'face 'lispm-desktop-platinum-sorted))))

(defun lispm-desktop-sidebar--row (lead name meta width)
  "Return a row of a list WIDTH pixels wide: LEAD, NAME, and META at its right.
LEAD (indentation, a triangle or an icon) keeps its size; META sits in
the right-hand column, see `lispm-desktop-sidebar--sorted-column'; NAME is
shortened to fit between them."
  (let ((room (- width (string-pixel-width (propertize lead 'face 'lispm-desktop-platinum-list))
                 (lispm-desktop-sidebar--column-pixels) 12)))
    (concat lead
            (lispm-desktop-platinum--fit (propertize name 'face (or (get-text-property 0 'face name)
                                                                    'lispm-desktop-platinum-list))
                                         (max 0 room))
            (lispm-desktop-sidebar--sorted-column meta))))

(defun lispm-desktop-sidebar--insert-entry (icon title meta action help &optional properties depth)
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
    (insert (lispm-desktop-sidebar--row
             (concat (lispm-desktop-sidebar--blank `(space :width (,(+ 8 lispm-desktop-sidebar-indent
                                                                       (* lispm-desktop-sidebar-indent (or depth 0))))))
                     icon (propertize " " 'face 'lispm-desktop-platinum-list))
             (lispm-desktop-sidebar--one-line title 200)
             meta
             (lispm-desktop-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (when properties
      (add-text-properties beg (point) properties))
    (insert "\n")))

(defun lispm-desktop-sidebar--insert-group (open name meta action help &optional directory)
  "Insert a group's row: a disclosure triangle, OPEN or not, a folder and NAME.
The folder is badged when DIRECTORY is a git repository.  META sits in
the right-hand column; clicking the row calls ACTION, described by HELP."
  (let ((beg (point)))
    (insert (lispm-desktop-sidebar--row
             (concat (lispm-desktop-sidebar--blank '(space :width (8)))
                     (propertize (lispm-desktop-platinum-triangle open) 'face 'lispm-desktop-platinum-list)
                     (lispm-desktop-sidebar--blank '(space :width (6)))
                     (lispm-desktop-platinum-folder-icon directory)
                     (lispm-desktop-sidebar--blank '(space :width (6))))
             (propertize (lispm-desktop-sidebar--one-line name 200)
                         'face '(:inherit lispm-desktop-platinum-list :weight bold))
             (propertize (or meta "") 'face 'shadow)
             (lispm-desktop-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (insert "\n")))

(defun lispm-desktop-sidebar--insert-heading (name)
  "Insert NAME as the heading of a group that does not fold."
  (insert (lispm-desktop-sidebar--blank '(space :width (8)))
          (propertize name 'face '(:inherit (shadow lispm-desktop-platinum-list) :weight bold))
          (lispm-desktop-sidebar--sorted-column nil)
          "\n"))

;;;; Shell sidebar

(defconst lispm-desktop-shell-list-buffer-name "*Shells*"
  "Name of the shell sidebar buffer.")

(defun lispm-desktop-shell--buffer-p (buffer)
  "Return non-nil when BUFFER is a live shell terminal, not an agent's."
  (and (buffer-live-p buffer)
       (memq (buffer-local-value 'major-mode buffer) '(eat-mode lispm-desktop-term-mode))
       (not (lispm-desktop-agent--buffer-p buffer))
       (lispm-desktop-agent--running-p buffer)))

(defun lispm-desktop-shell-new (&optional directory)
  "Start a new shell in DIRECTORY, by default the main window's project root.
The shell is the user's, run as an independent process in a terminal, see
`lispm-desktop-terminal-make'."
  (interactive)
  (let* ((directory (or directory
                        (with-selected-window (lispm-desktop-agent--main-window)
                          (lispm-desktop-agent--project-directory))))
         (default-directory (file-name-as-directory directory))
         (process-environment (lispm-desktop-agent--independent-environment process-environment))
         (program (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))
    (lispm-desktop-sidebar-show-in-main
     (lispm-desktop-terminal-make (lispm-desktop-agent--fresh-name 'shell directory) program "-l")))
  (lispm-desktop-shell-list-refresh))

(defun lispm-desktop-shell-list--render ()
  "Render the running shells into the current buffer."
  (let ((inhibit-read-only t)
        (shells (seq-filter #'lispm-desktop-shell--buffer-p (buffer-list))))
    (erase-buffer)
    (lispm-desktop-sidebar-set-toolbar
     'shells #'lispm-desktop-shell-list-refresh
     '(("New Shell" lispm-desktop-shell-new "New shell at the main window's project root")))
    (setq lispm-desktop-platinum-info (lispm-desktop-sidebar--count (length shells) "shell"))
    (lispm-desktop-sidebar--insert-columns "Name" "Directory")
    (if (null shells)
        (insert (lispm-desktop-sidebar--blank '(space :width (8)))
                (propertize "No running shells" 'face 'shadow)
                (lispm-desktop-sidebar--sorted-column nil) "\n")
      (dolist (buffer shells)
        (let ((directory (buffer-local-value 'default-directory buffer)))
          (lispm-desktop-sidebar--insert-entry
           (lispm-desktop-platinum-icon "computer")
           (string-trim (buffer-name buffer) "\\*" "\\*")
           (file-name-nondirectory (directory-file-name directory))
           (apply-partially #'lispm-desktop-sidebar-show-in-main buffer)
           (format "Show this shell, in %s" (abbreviate-file-name directory))))))))

(defun lispm-desktop-shell-list-refresh (&rest _)
  "Re-render the shell sidebar when it exists."
  (interactive)
  (when-let* ((buffer (get-buffer lispm-desktop-shell-list-buffer-name)))
    (with-current-buffer buffer
      (lispm-desktop-shell-list--render))))

(defvar-keymap lispm-desktop-shell-list-mode-map
  :doc "Keymap for the shell sidebar."
  :parent special-mode-map
  "g" #'lispm-desktop-shell-list-refresh
  "+" #'lispm-desktop-shell-new
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lispm-desktop-shell-list-mode special-mode "Shells"
  "Sidebar listing running shells.
\\{lispm-desktop-shell-list-mode-map}"
  (lispm-desktop-sidebar--setup-buffer))

(defun lispm-desktop-shell-list-show ()
  "Show the shell sidebar on the left of this frame and return its window."
  (let ((buffer (get-buffer-create lispm-desktop-shell-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-desktop-shell-list-mode)
        (lispm-desktop-shell-list-mode))
      (lispm-desktop-shell-list--render)
      (goto-char (point-min)))
    (lispm-desktop-sidebar--display buffer)))

(defun lispm-desktop-shell-list-visible-p ()
  "Return non-nil when the shell sidebar is shown in the selected frame."
  (and (get-buffer-window lispm-desktop-shell-list-buffer-name) t))

(defun lispm-desktop-shell-list-hide ()
  "Hide the shell sidebar in the selected frame."
  (lispm-desktop-sidebar--hide-buffer lispm-desktop-shell-list-buffer-name))

;;;; Files sidebar

(declare-function treemacs-current-workspace "treemacs-workspaces" ())
(declare-function treemacs-workspace->projects "treemacs-workspaces" (workspace))
(declare-function treemacs-project->path "treemacs-workspaces" (project))

(defun lispm-desktop-sidebar--files-info ()
  "Return what the project tree's bottom edge says: the project shown."
  (if-let* ((projects (treemacs-workspace->projects (treemacs-current-workspace))))
      (abbreviate-file-name (treemacs-project->path (car projects)))
    "No project"))

(defun lispm-desktop-sidebar--files-toolbar ()
  "Give the project tree buffer its sidebar toolbar and bottom edge."
  (setq lispm-desktop-platinum-info #'lispm-desktop-sidebar--files-info)
  (lispm-desktop-sidebar-set-toolbar
   'files #'treemacs-refresh
   '(("New File" treemacs-create-file "New file in the selected directory")
     ("New Folder" treemacs-create-dir "New folder in the selected directory")
     ("Collapse" treemacs-collapse-all-projects "Collapse all"))))

(defun lispm-desktop-sidebar--files-visible-p ()
  "Return non-nil when the project tree is shown in the selected frame."
  (and (featurep 'treemacs) (eq (treemacs-current-visibility) 'visible)))

(defun lispm-desktop-sidebar--files-show ()
  "Show the project tree of the main window's buffer, keeping the selection.
Outside any project treemacs falls back to the buffer's directory."
  (require 'treemacs)
  (save-selected-window
    (with-current-buffer (window-buffer (lispm-desktop-agent--main-window))
      (treemacs-add-and-display-current-project-exclusively))))

(defun lispm-desktop-sidebar--files-hide ()
  "Hide the project tree in the selected frame."
  (when-let* ((window (and (featurep 'treemacs) (treemacs-get-local-window))))
    (delete-window window)))

(defconst lispm-desktop-sidebar-views
  '((files :title "Files" :icon "disk" :command lispm-desktop-sidebar-files
           :help "Project files"
           :visible lispm-desktop-sidebar--files-visible-p
           :show lispm-desktop-sidebar--files-show
           :hide lispm-desktop-sidebar--files-hide)
    (agents :title "Agents" :icon "robot" :mode lispm-desktop-agent-list-mode
            :command lispm-desktop-sidebar-agents
            :help "Agent sessions"
            :visible lispm-desktop-agent-list-visible-p
            :show lispm-desktop-agent-list-show
            :hide lispm-desktop-agent-list-hide)
    (shells :title "Shells" :icon "computer" :mode lispm-desktop-shell-list-mode
            :command lispm-desktop-sidebar-shells
            :help "Shells"
            :visible lispm-desktop-shell-list-visible-p
            :show lispm-desktop-shell-list-show
            :hide lispm-desktop-shell-list-hide))
  "Views the left sidebar can show, in the order of the editor's tabs.
Each view names its TITLE, its Finder icon (see `lispm-desktop-platinum-icon'),
the MODE of its list buffer when it has one, the COMMAND its menu bar title
runs, and the functions that test, show and hide it in the selected frame.")

(defun lispm-desktop-sidebar-current-view ()
  "Return the view the left sidebar shows, or nil when it is hidden."
  (car (seq-find (lambda (view) (funcall (plist-get (cdr view) :visible)))
                 lispm-desktop-sidebar-views)))

(defvar lispm-desktop-sidebar-prepare-function nil
  "Function called before a view is shown or hidden, or nil.
The views belong to the editor; it brings the editor in front when a
view is asked for elsewhere, as on the desktop.")

(defvar lispm-desktop-sidebar-change-hook nil
  "Hook run after the sidebar's view changed.")

(defun lispm-desktop-sidebar-select (view)
  "Show VIEW in the left sidebar, or hide the sidebar when VIEW is shown.
Every other view is hidden first, so at most one is ever visible; see
`lispm-desktop-sidebar-prepare-function' for views asked for outside the editor."
  (when lispm-desktop-sidebar-prepare-function
    (funcall lispm-desktop-sidebar-prepare-function))
  (let ((current (lispm-desktop-sidebar-current-view)))
    (dolist (entry lispm-desktop-sidebar-views)
      (when (funcall (plist-get (cdr entry) :visible))
        (funcall (plist-get (cdr entry) :hide))))
    (unless (eq view current)
      (funcall (plist-get (alist-get view lispm-desktop-sidebar-views) :show))))
  (run-hooks 'lispm-desktop-sidebar-change-hook)
  (force-mode-line-update t))

(defun lispm-desktop-sidebar-files ()
  "Show the project tree in the left sidebar, or hide it when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'files))

(defun lispm-desktop-sidebar-shells ()
  "Show the running shells in the left sidebar, or hide them when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'shells))

(defun lispm-desktop-sidebar-agents ()
  "Show the agent sessions in the left sidebar, or hide them when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'agents))

(defun lispm-desktop-sidebar--window ()
  "Return the window of the left sidebar in the selected frame, or nil."
  (window-with-parameter 'window-side 'left))

(defun lispm-desktop-sidebar-focus ()
  "Move the keyboard into the left sidebar, or out of it to the main area.
The sidebar is left out of `other-window', as Emacs leaves side windows
out, so its list keys are reached this way; a hidden sidebar shows the
Agents view first."
  (interactive)
  (let ((window (lispm-desktop-sidebar--window)))
    (cond ((and window (eq window (selected-window)))
           (select-window (lispm-desktop-agent--main-window)))
          (window (select-window window))
          (t (lispm-desktop-sidebar-select 'agents)
             (when-let* ((window (lispm-desktop-sidebar--window)))
               (select-window window))))))



(provide 'lispm-desktop-sidebar)
;;; lispm-desktop-sidebar.el ends here
