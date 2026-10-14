;;; lme-browser-places.el --- The browser's history and bookmarks  -*- lexical-binding: t; -*-

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

;; What the browser remembers, as Netscape Communicator remembered it: the
;; pages visited, its History, and the pages kept, its bookmarks.
;;
;;   page --finished loading--> history, a file of its own
;;   Add Bookmark --> Emacs's own bookmarks (bookmark.el), with xwidget's
;;                    handler, so `bookmark-jump' opens them too
;;
;; Both list in a pane at the browser window's left, a tab choosing one:
;; History by day, the most recent first, and bookmarks in the order they
;; were made.  A click opens a page in the shown tab, a middle click in a
;; new one; a right click offers the rest.  Bookmarks also fill the
;; Bookmarks menu, the location toolbar's Bookmarks button and the
;; personal toolbar.

;;; Code:

(require 'bookmark)
(require 'seq)
(require 'subr-x)
(require 'lme-platinum)
(require 'lme-browser)

(declare-function xwidget-webkit-uri "xwidget.c" (xwidget))
(declare-function xwidget-webkit-title "xwidget.c" (xwidget))
(declare-function lme-popup-menu "lme-popup" (items &optional x y align))
(declare-function lme-sidebar--toolbar-button "lme-sidebar" (label command help row &optional face))

;;;; History

(defcustom lme-browser-history-file (locate-user-emacs-file "lme-browser-history.eld")
  "File keeping the pages the browser visited."
  :type 'file
  :group 'lispm-emacs)

(defcustom lme-browser-history-size 1000
  "How many pages the browser's history keeps, the most recently visited."
  :type 'natnum
  :group 'lispm-emacs)

(defvar lme-browser--history nil
  "Pages visited, the most recent first: (URL TITLE LAST-VISIT VISITS).
LAST-VISIT is a `float-time'.  Read from `lme-browser-history-file' when
first wanted.")

(defvar lme-browser--history-loaded nil
  "Non-nil once the history was read from its file.")

(defvar lme-browser--history-timer nil
  "Idle timer writing the history to its file after a change.")

(defun lme-browser--load-history ()
  "Return the history, read from its file the first time."
  (unless lme-browser--history-loaded
    (setq lme-browser--history-loaded t
          lme-browser--history
          (and (file-readable-p lme-browser-history-file)
               (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents lme-browser-history-file)
                   (read (current-buffer)))))))
  lme-browser--history)

(defun lme-browser--save-history ()
  "Write the history to `lme-browser-history-file'."
  (setq lme-browser--history-timer nil)
  (when lme-browser--history-loaded
    (ignore-errors
      (with-temp-file lme-browser-history-file
        (let ((print-length nil)
              (print-level nil))
          (insert ";; The pages lispm-emacs's browser visited.  -*- mode: lisp-data -*-\n")
          (prin1 lme-browser--history (current-buffer))
          (insert "\n"))))))

(defun lme-browser--history-changed ()
  "Write the history once Emacs is idle, and redraw what lists it."
  (unless lme-browser--history-timer
    (setq lme-browser--history-timer
          (run-with-idle-timer 2 nil #'lme-browser--save-history)))
  (lme-browser--places-changed))

(defun lme-browser-history-add (url title)
  "Note that the page at URL, titled TITLE, was visited just now.
Only web and file pages are kept, each once, at its latest visit."
  (when (and (stringp url) (string-match-p "\\`\\(https?\\|file\\|ftp\\):" url))
    (let* ((history (lme-browser--load-history))
           (old (assoc url history))
           (title (if (string-empty-p (or title "")) (or (nth 1 old) url) title)))
      (setq lme-browser--history
            (seq-take (cons (list url title (float-time) (1+ (or (nth 3 old) 0)))
                            (delq old history))
                      lme-browser-history-size))
      (lme-browser--history-changed))))

(defun lme-browser-history-remove (url)
  "Forget the visits to the page at URL."
  (setq lme-browser--history (assoc-delete-all url (lme-browser--load-history)))
  (lme-browser--history-changed))

(defun lme-browser-clear-history ()
  "Forget every page visited, after asking."
  (interactive)
  (when (yes-or-no-p "Forget every page the browser visited? ")
    (lme-browser--load-history)
    (setq lme-browser--history nil)
    (lme-browser--history-changed)))

(defun lme-browser--note-load (xwidget type)
  "Add the page XWIDGET finished loading to the history.
This follows `xwidget-webkit-callback' with its arguments, XWIDGET and
the event TYPE: xwidget has no hook of its own for a finished load."
  (when (and (eq type 'load-changed)
             (equal (nth 3 last-input-event) "load-finished"))
    (lme-browser-history-add (ignore-errors (xwidget-webkit-uri xwidget))
                             (ignore-errors (xwidget-webkit-title xwidget)))))

(defun lme-browser--history-day (time)
  "Return the heading TIME, a `float-time', is listed under in the History."
  (let ((days (- (time-to-days (current-time)) (time-to-days time))))
    (cond ((<= days 0) "Today")
          ((= days 1) "Yesterday")
          ((< days 7) (format-time-string "%A" time))
          ((< days 14) "Last Week")
          (t "Earlier"))))

;;;; Bookmarks

(defun lme-browser--bookmark-p (bookmark)
  "Return non-nil when BOOKMARK, a bookmark record, is a web page's."
  (and (bookmark-prop-get bookmark 'page)
       (eq (bookmark-prop-get bookmark 'handler) 'xwidget-webkit-bookmark-jump-handler)))

(defun lme-browser-bookmarks ()
  "Return the browser's bookmarks, in the order they were made: (NAME . URL)."
  (bookmark-maybe-load-default-file)
  (mapcar (lambda (bookmark)
            (cons (bookmark-name-from-full-record bookmark) (bookmark-prop-get bookmark 'page)))
          (reverse (seq-filter #'lme-browser--bookmark-p bookmark-alist))))

(defun lme-browser--bookmark-name (title)
  "Return a name for a bookmark titled TITLE that no bookmark has yet."
  (let ((name (if (string-empty-p title) "Untitled" title))
        (count 1))
    (while (bookmark-get-bookmark name 'noerror)
      (setq count (1+ count)
            name (format "%s <%d>" title count)))
    name))

(defun lme-browser-add-bookmark ()
  "Bookmark the shown page under its title, as Netscape's Add Bookmark did.
A page already bookmarked is not bookmarked twice."
  (interactive)
  (let* ((buffer (lme-browser--shown-buffer))
         (url (and buffer (lme-browser--uri buffer))))
    (unless url
      (user-error "No page is shown"))
    (if (rassoc url (lme-browser-bookmarks))
        (message "Already bookmarked: %s" url)
      (let ((name (lme-browser--bookmark-name (lme-browser--title buffer))))
        (bookmark-store name
                        `((page . ,url)
                          (handler . xwidget-webkit-bookmark-jump-handler)
                          (last-modified . ,(current-time)))
                        nil)
        (message "Bookmarked: %s" name)))
    (lme-browser--places-changed)))

(defun lme-browser-delete-bookmark (name)
  "Delete the bookmark NAME."
  (bookmark-delete name)
  (lme-browser--places-changed))

(defun lme-browser-rename-bookmark (name)
  "Rename the bookmark NAME, asking for its new name."
  (let ((new (string-trim (read-string (format "Rename “%s” to: " name) name))))
    (unless (or (string-empty-p new) (equal new name))
      (bookmark-rename name new)
      (lme-browser--places-changed))))

(defun lme-browser--bookmark-items ()
  "Return the items of the Bookmarks menu: Add Bookmark and Edit Bookmarks,
then a bookmark each, as Netscape's Bookmarks menu had them."
  (let ((bookmarks (lme-browser-bookmarks)))
    `((:label "Add Bookmark" :action lme-browser-add-bookmark
              :enabled ,(and (lme-browser--shown-buffer) t)
              :key ,(lme-browser--key 'lme-browser-add-bookmark))
      (:label "Edit Bookmarks…" :action lme-browser-edit-bookmarks
              :checked ,(eq (lme-browser--places-shown) 'bookmarks)
              :key ,(lme-browser--key 'lme-browser-edit-bookmarks))
      separator
      ,@(if bookmarks
            (mapcar (pcase-lambda (`(,name . ,url))
                      (list :label (truncate-string-to-width name 48 nil nil "…")
                            :action (lambda () (lme-browser-open-url url))))
                    bookmarks)
          '((:label "No Bookmarks" :action ignore :enabled nil))))))

;;;; The personal toolbar

(defun lme-browser--ribbon ()
  "Return the bookmark ribbon, one icon wide."
  (propertize " " 'display
              (lme-browser--image
               '(ribbon)
               (lambda ()
                 (lme-browser--svg
                  16 16
                  (format "<g transform='scale(0.66)'>%s</g>"
                          (format-spec (alist-get 'bookmarks lme-browser--icons)
                                       (lme-browser--ink t))))))))

(defun lme-browser--personal-toolbar ()
  "Return the personal toolbar, the address window's mode line: a button
per bookmark, as Netscape's personal toolbar held them."
  (let ((bookmarks (lme-browser-bookmarks)))
    (concat
     (propertize " " 'display '(space :width (8) :height (26) :ascent 70))
     (if bookmarks
         (mapconcat (pcase-lambda (`(,name . ,url))
                      (lme-sidebar--toolbar-button
                       (concat " " (lme-browser--ribbon) " "
                               (truncate-string-to-width name 22 nil nil "…") " ")
                       (lambda () (interactive) (lme-browser-open-url url))
                       url 'mode-line))
                    bookmarks " ")
       (propertize "Bookmarks you add show here." 'face 'lme-platinum-toolbar-label)))))

;;;; The pane

(defconst lme-browser-places-buffer-name " *Browser Places*"
  "Name of the buffer of the browser's History and Bookmarks pane.")

(defconst lme-browser-places-width 280
  "Width in pixels of the History and Bookmarks pane.")

(defvar-local lme-browser--places-view 'bookmarks
  "What the pane lists: `bookmarks' or `history'.")

(defun lme-browser--places-window ()
  "Return the pane's window in the browser, or nil."
  (get-buffer-window lme-browser-places-buffer-name
                     (and (lme-browser-floating-p) lme-browser--frame)))

(defun lme-browser--places-shown ()
  "Return what the pane lists while it is shown, `bookmarks' or `history'."
  (when-let* ((window (lme-browser--places-window)))
    (buffer-local-value 'lme-browser--places-view (window-buffer window))))

(defvar-keymap lme-browser-places-tab-map
  :doc "Keymap of a tab of the pane."
  "<tab-line> <down-mouse-1>" #'ignore
  "<tab-line> <mouse-1>" #'lme-browser-places-tab-click)

(defun lme-browser-places-tab-click (event)
  "Show the list whose tab EVENT clicked."
  (interactive "e")
  (let ((position (event-start event)))
    (when-let* ((view (get-text-property (cdr (posn-string position)) 'lme-browser-view
                                         (car (posn-string position)))))
      (with-current-buffer (window-buffer (posn-window position))
        (setq lme-browser--places-view view)
        (lme-browser--render-places)))))

(defun lme-browser--places-tab (view label)
  "Return the pane's tab for VIEW named LABEL, pushed in when shown."
  (let ((tab (concat " " label " ")))
    (add-face-text-property 0 (length tab)
                            (if (eq view lme-browser--places-view)
                                'lme-platinum-tab-selected
                              'lme-platinum-tab)
                            t tab)
    (propertize tab 'lme-browser-view view 'keymap lme-browser-places-tab-map
                'pointer 'arrow 'help-echo (format "Show the %s" label))))

(defun lme-browser--places-tabs ()
  "Return the pane's tab line: a tab each for Bookmarks and History, and a
button closing the pane."
  (let ((close (lme-sidebar--toolbar-button " ✕ " #'lme-browser-hide-places
                                            "Close the pane" 'tab-line)))
    (concat (propertize " " 'display '(space :width (6) :height (28) :ascent 65))
            (lme-browser--places-tab 'bookmarks "Bookmarks") " "
            (lme-browser--places-tab 'history "History")
            (propertize " " 'display `(space :align-to (- right (,(+ (string-pixel-width close) 6)))))
            close)))

(defun lme-browser--place-line (url text &rest properties)
  "Return a pane row for the page at URL showing TEXT, with PROPERTIES."
  (apply #'propertize (concat text "\n")
         'lme-browser-url url 'mouse-face 'highlight 'help-echo url
         properties))

(defun lme-browser--places-count (count noun)
  "Return COUNT NOUNs in words."
  (format "%d %s%s" count noun (if (= count 1) "" "s")))

(defun lme-browser--render-places ()
  "List the bookmarks or the history in the pane's buffer."
  (when-let* ((buffer (get-buffer lme-browser-places-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (pcase lme-browser--places-view
          ('bookmarks
           (let ((bookmarks (lme-browser-bookmarks)))
             (dolist (bookmark bookmarks)
               (insert (lme-browser--place-line (cdr bookmark)
                                                (concat " " (lme-browser--ribbon) " " (car bookmark))
                                                'lme-browser-bookmark (car bookmark))))
             (unless bookmarks
               (insert (propertize "\n  No bookmarks yet: Bookmarks › Add Bookmark\n  keeps the page shown.\n"
                                   'face 'shadow)))
             (setq lme-platinum-info (lme-browser--places-count (length bookmarks) "bookmark"))))
          (_
           (let ((history (lme-browser--load-history))
                 (heading nil))
             (dolist (entry history)
               (pcase-let ((`(,url ,title ,time) entry))
                 (let ((day (lme-browser--history-day time)))
                   (unless (equal day heading)
                     (setq heading day)
                     (insert (propertize (concat " " day "\n") 'face 'bold)))
                   (insert (lme-browser--place-line
                            url (concat "   " (or title url)
                                        (propertize (format-time-string "  %H:%M" time)
                                                    'face 'shadow)))))))
             (unless history
               (insert (propertize "\n  No pages visited yet.\n" 'face 'shadow)))
             (setq lme-platinum-info (lme-browser--places-count (length history) "page")))))
        (goto-char (point-min))
        (forward-line (1- line))
        (force-mode-line-update)))))

(defun lme-browser--places-changed ()
  "Redraw what lists the bookmarks or the history: the pane, the personal
toolbar."
  (lme-browser--render-places)
  (when-let* ((buffer (get-buffer lme-browser-address-buffer-name)))
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun lme-browser--place-at (&optional position)
  "Return the address of the pane row at POSITION, by default point."
  (get-text-property (or position (point)) 'lme-browser-url))

(defun lme-browser-places-open (&optional new-tab)
  "Open the page of the row at point, in a new tab when NEW-TAB."
  (interactive "P")
  (when-let* ((url (lme-browser--place-at)))
    (lme-browser-open-url url new-tab)))

(defun lme-browser-places-open-new-tab ()
  "Open the page of the row at point in a new tab."
  (interactive)
  (lme-browser-places-open t))

(defun lme-browser-places-click (event)
  "Open the page of the row EVENT clicked; a middle click opens a new tab."
  (interactive "e")
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position)))
      (with-selected-window (posn-window position)
        (goto-char point)
        (lme-browser-places-open (eq (event-basic-type event) 'mouse-2))))))

(defun lme-browser-places-delete ()
  "Delete the bookmark, or forget the page, of the row at point."
  (interactive)
  (when-let* ((url (lme-browser--place-at)))
    (if-let* ((name (get-text-property (point) 'lme-browser-bookmark)))
        (lme-browser-delete-bookmark name)
      (lme-browser-history-remove url))))

(defun lme-browser-places-rename ()
  "Rename the bookmark of the row at point."
  (interactive)
  (if-let* ((name (get-text-property (point) 'lme-browser-bookmark)))
      (lme-browser-rename-bookmark name)
    (user-error "Only a bookmark can be renamed")))

(defun lme-browser-places-context-menu (event)
  "Pop up what can be done with the row EVENT clicked."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (when (posn-point position)
        (goto-char (posn-point position)))
      (when-let* ((url (lme-browser--place-at)))
        (let ((bookmark (get-text-property (point) 'lme-browser-bookmark)))
          (lme-popup-menu
           `((:label "Open" :action lme-browser-places-open)
             (:label "Open in New Tab" :action lme-browser-places-open-new-tab)
             separator
             (:label "Copy Address" :action ,(lambda () (kill-new url) (message "Copied %s" url)))
             ,@(if bookmark
                   '((:label "Rename…" :action lme-browser-places-rename)
                     (:label "Delete Bookmark" :action lme-browser-places-delete))
                 '((:label "Forget Page" :action lme-browser-places-delete)
                   separator
                   (:label "Clear History…" :action lme-browser-clear-history))))))))))

(defun lme-browser-places-switch ()
  "Show the other list, bookmarks or history."
  (interactive)
  (setq lme-browser--places-view (if (eq lme-browser--places-view 'bookmarks) 'history 'bookmarks))
  (lme-browser--render-places))

(defvar-keymap lme-browser-places-mode-map
  :doc "Keymap of the browser's History and Bookmarks pane."
  "RET" #'lme-browser-places-open
  "M-RET" #'lme-browser-places-open-new-tab
  "<mouse-1>" #'lme-browser-places-click
  "<mouse-2>" #'lme-browser-places-click
  "<mouse-3>" #'lme-browser-places-context-menu
  "d" #'lme-browser-places-delete
  "DEL" #'lme-browser-places-delete
  "<delete>" #'lme-browser-places-delete
  "r" #'lme-browser-places-rename
  "g" #'lme-browser--render-places
  "TAB" #'lme-browser-places-switch
  "q" #'lme-browser-hide-places)

(define-derived-mode lme-browser-places-mode special-mode "Places"
  "Major mode of the browser's History and Bookmarks pane.
\\{lme-browser-places-mode-map}"
  (setq-local tab-line-format '(:eval (lme-browser--places-tabs))
              header-line-format nil
              truncate-lines t
              cursor-type nil
              lme-platinum-title "Bookmarks")
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (face-remap-add-relative 'default 'lme-platinum-list)
  (hl-line-mode 1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lme-browser--places-buffer ()
  "Return the pane's buffer, made when missing."
  (or (get-buffer lme-browser-places-buffer-name)
      (with-current-buffer (get-buffer-create lme-browser-places-buffer-name)
        (lme-browser-places-mode)
        (current-buffer))))

(defun lme-browser--show-places (view)
  "Show the pane listing VIEW, `bookmarks' or `history', at the browser
window's left, the browser shown first when it is not."
  (unless (lme-browser--window)
    (lme-browser-show))
  (with-current-buffer (lme-browser--places-buffer)
    (setq lme-browser--places-view view)
    (lme-browser--render-places))
  (unless (lme-browser--places-window)
    (when-let* ((page (lme-browser--window)))
      (let ((window (split-window page (- lme-browser-places-width) 'left t)))
        (set-window-buffer window (lme-browser--places-buffer))
        (set-window-dedicated-p window t)
        (set-window-parameter window 'lme-chrome t)
        (set-window-parameter window 'no-other-window t)))))

(defun lme-browser-hide-places ()
  "Close the History and Bookmarks pane."
  (interactive)
  (when-let* ((window (lme-browser--places-window)))
    (delete-window window)))

(defun lme-browser-show-history ()
  "Show the pages visited in the pane, or close the pane when it shows them."
  (interactive)
  (if (eq (lme-browser--places-shown) 'history)
      (lme-browser-hide-places)
    (lme-browser--show-places 'history)))

(defun lme-browser-edit-bookmarks ()
  "Show the bookmarks in the pane, where they are opened, renamed and deleted."
  (interactive)
  (lme-browser--show-places 'bookmarks))

(defun lme-setup-browser-places ()
  "Keep the browser's history as pages load, and write it when Emacs quits."
  (advice-add 'xwidget-webkit-callback :after #'lme-browser--note-load)
  (add-hook 'kill-emacs-hook #'lme-browser--save-history))

(provide 'lme-browser-places)
;;; lme-browser-places.el ends here
