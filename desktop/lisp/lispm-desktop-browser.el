;;; lispm-desktop-browser.el --- The embedded browser  -*- lexical-binding: t; -*-

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

;; The embedded WebKit browser: an application on the desktop (see
;; lispm-desktop-apps.el) with its own workspace and menus, a Mac OS 9 browser window
;; (title bar, toolbar, address field, tabs), and helpers that open URLs in
;; it.  Requires an Emacs built with xwidgets.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'delsel)
(require 'lispm-desktop-apps)
(require 'format-spec)
(require 'thingatpt)
(require 'url-parse)
(require 'url-util)

(defun lispm-desktop-xwidget-available-p ()
  "Return non-nil when xwidget webkit browsing is available."
  (and (featurep 'xwidget-internal)
       (fboundp 'xwidget-webkit-browse-url)))

(defun lispm-desktop-xwidget-normalize-url (url)
  "Normalize URL for xwidget browsing."
  (if (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" url)
      url
    (concat "https://" url)))

(defcustom lispm-desktop-xwidget-search-url "https://duckduckgo.com/?q=%s"
  "Address searched for what is typed in the location field that is no
address, %s standing for the words searched."
  :type 'string
  :group 'lispm-desktop)

(defun lispm-desktop-xwidget-browse-url (url)
  "Open URL using xwidget webkit when available."
  (interactive (list (read-string "URL: " (or (thing-at-point-url-at-point) ""))))
  (if (lispm-desktop-xwidget-available-p)
      (xwidget-webkit-browse-url (lispm-desktop-xwidget-normalize-url url))
    (browse-url (lispm-desktop-xwidget-normalize-url url))))

(defun lispm-desktop-xwidget-search (query)
  "Search QUERY using xwidget webkit when available."
  (interactive (list (read-string "Search: " (or (thing-at-point 'symbol t) ""))))
  (lispm-desktop-xwidget-browse-url (format lispm-desktop-xwidget-search-url (url-hexify-string query))))

(defun lispm-desktop-xwidget-open-current-url ()
  "Open the URL at point using xwidget webkit when available."
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (unless url
      (user-error "No URL at point"))
    (lispm-desktop-xwidget-browse-url url)))

;;;; Browser
;;
;; The embedded WebKit browser is laid out as Netscape Communicator's window
;; was on Mac OS 9, its icons drawn in the theme's colours:
;;
;;   address window  title bar     the shown page's icon and title
;;                   header line   navigation toolbar: Back, Forward,
;;                                 Reload, Home, Search, History, Stop,
;;                                 and the Netscape N, its meteors falling
;;                                 while the page loads
;;                   text line     location toolbar: Bookmarks, the page's
;;                                 icon, the address in an edit field (RET)
;;                   mode line     personal toolbar: a button per bookmark
;;   page window     tab line      a bevel button per tab, then "+"
;;                   below         the page
;;                   mode line     status bar: the padlock, the progress
;;                                 bar, what the page is doing
;;
;; The address window is a real buffer, so the address is edited in place.
;; A timer keeps it in step with the page while it is not being edited.
;; History and bookmarks are lispm-desktop-browser-places.el's.
;;
;; The window is drawn in one of two ways, as the editor's is, and its zoom
;; box moves it from one to the other:
;;
;;   full screen   in the main area of a workspace of its own, under the
;;                 menu bar
;;   floating      in a child frame over the desktop, moved by its title
;;                 bar and resized from its grow box or its edges
;;
;; Its pages are buffers, so moving the window only shows them elsewhere.

(declare-function lispm-desktop-show-desktop "lispm-desktop-coherence" (&optional screen))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-webkit-uri "xwidget" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-back "xwidget" ())
(declare-function xwidget-webkit-forward "xwidget" ())
(declare-function xwidget-webkit-reload "xwidget" ())
(declare-function xwidget-webkit-stop "xwidget" ())
(declare-function xwidget-webkit-back-forward-list "xwidget.c" (xwidget &optional limit))
(declare-function xwidget-webkit-goto-history "xwidget.c" (xwidget rel-pos))
(declare-function xwidget-webkit-estimated-load-progress "xwidget.c" (xwidget))
(defvar xwidget-webkit-mode-map)
(defvar xwidget-webkit--loading-p)
(declare-function lispm-desktop-platinum-icon "lispm-desktop-platinum" (name))
(defvar lispm-desktop-platinum-title)
(defvar lispm-desktop-platinum-icon)
(defvar lispm-desktop-platinum-box-functions)

(defconst lispm-desktop-browser-workspace-name "Browser"
  "Name of the workspace holding the embedded browser.")

(defcustom lispm-desktop-browser-home-url "https://duckduckgo.com"
  "Page a new browser tab opens, and Home goes to."
  :type 'string
  :group 'lispm-desktop)

(defconst lispm-desktop-browser-address-buffer-name " *Browser Address*"
  "Name of the address window's buffer; the leading space keeps it unlisted.")

(defconst lispm-desktop-browser-tab-width 24
  "Maximum width in columns of a browser tab's title.")

(defvar lispm-desktop-browser--tab-order nil
  "Browser tab buffers in the order they were opened.
Buffer names follow page titles, so they cannot order the tabs.")

(defun lispm-desktop-browser--buffers ()
  "Return the live browser tabs, in the order they were opened."
  (setq lispm-desktop-browser--tab-order (seq-filter #'buffer-live-p lispm-desktop-browser--tab-order)))

(defun lispm-desktop-browser-workspace-p ()
  "Return non-nil when the browser workspace is selected."
  (and (bound-and-true-p tabspaces-mode)
       (equal (tabspaces--current-tab-name) lispm-desktop-browser-workspace-name)))

(defvar lispm-desktop-browser--frame nil
  "The floating browser's child frame, or nil while the browser is full screen.")

(defvar lispm-desktop-browser-floating nil
  "Non-nil when the browser opens floating over the desktop rather than full
screen.  Its zoom box, and the View menu, switch it.")

(defvar lispm-desktop-browser--geometry nil
  "Where the floating browser was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lispm-desktop-browser--shade nil
  "While the floating browser is rolled up: its windows and text height.")

(defvar lispm-desktop-browser--navigation nil
  "What the shown tab can do, as the last sync found it:
\(:page P :back B :forward F :loading L).")

(defun lispm-desktop-browser-floating-p ()
  "Return non-nil while the browser floats in a window of its own."
  (frame-live-p lispm-desktop-browser--frame))

(defun lispm-desktop-browser--window ()
  "Return the window showing a browser tab, if any: in the floating browser,
or else in the main area."
  (seq-find (lambda (window)
              (eq (buffer-local-value 'major-mode (window-buffer window))
                  'xwidget-webkit-mode))
            (if (lispm-desktop-browser-floating-p)
                (window-list lispm-desktop-browser--frame 'no-mini)
              (lispm-desktop-main-area--windows))))

(defun lispm-desktop-browser--address-window ()
  "Return the address window: the floating browser's, or this frame's."
  (get-buffer-window lispm-desktop-browser-address-buffer-name
                     (and (lispm-desktop-browser-floating-p) lispm-desktop-browser--frame)))

(defun lispm-desktop-browser--uri (&optional buffer)
  "Return the address of the page in browser BUFFER, by default the shown tab."
  (when-let* ((buffer (or buffer (and (lispm-desktop-browser--window)
                                      (window-buffer (lispm-desktop-browser--window)))))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min)))))
    (xwidget-webkit-uri xwidget)))

(defun lispm-desktop-browser--title (buffer)
  "Return the page title of browser BUFFER, from its buffer name."
  (let ((name (string-trim (buffer-name buffer) "\\*" "\\*")))
    (string-trim (replace-regexp-in-string "\\`xwidget[- ]webkit: ?" "" name))))

(defun lispm-desktop-browser--target (input)
  "Return the address to visit for address bar INPUT.
A local host (localhost or 127.0.0.1, with any port or path) is served over
http; input that names a scheme, or a dotted host without spaces, is an
address; anything else is a search."
  (cond ((string-match-p "\\`\\(localhost\\|127\\.0\\.0\\.1\\)\\([:/]\\|\\'\\)" input)
         (concat "http://" input))
        ((or (string-match-p "\\`[a-z][a-z0-9+.-]*://" input)
             (and (string-match-p "\\." input) (not (string-match-p " " input))))
         (lispm-desktop-xwidget-normalize-url input))
        (t (format lispm-desktop-xwidget-search-url (url-hexify-string input)))))

(defconst lispm-desktop-browser--paste-script
  "(function (text) {
  var field = document.activeElement;
  if (field && typeof field.setRangeText === 'function') {
    field.setRangeText(text, field.selectionStart, field.selectionEnd, 'end');
    field.dispatchEvent(new InputEvent('input', {bubbles: true,
      inputType: 'insertFromPaste', data: text}));
  } else {
    document.execCommand('insertText', false, text);
  }
})(%s);"
  "Script inserting a text, its JSON-encoded argument, at the page's caret.
Text fields take it through `setRangeText', which leaves it verbatim, and
are told by an input event, so scripted pages see the change;
`insertText' would have WebKit turn straight quotes into curly ones.
Rich-text editors, which have no such method, take `insertText'.")

(defun lispm-desktop-browser--paste (text)
  "Insert TEXT verbatim at the caret of the shown page's focused field.
The text travels as a JSON string literal, so quotes and newlines in it
cannot break out of `lispm-desktop-browser--paste-script'."
  (lispm-desktop-browser--run-script (xwidget-webkit-current-session)
                                     (format lispm-desktop-browser--paste-script (json-encode text))))

(defun lispm-desktop-browser-open-external ()
  "Open the shown browser tab's page in the system's default browser.
Sites needing browser extensions, such as MetaMask or 1Password, work
there; the embedded WebKit view has no extension support."
  (interactive)
  (if-let* ((uri (lispm-desktop-browser--uri)))
      (browse-url-default-browser uri)
    (user-error "No page is shown")))

(defun lispm-desktop-browser--in-page (command)
  "Run COMMAND, a WebKit navigation command, in the shown browser tab.
Navigating discards an unsubmitted edit of the address, as a desktop browser
does: the address window shows the page's real address again at once."
  (when-let* ((window (lispm-desktop-browser--window)))
    (with-selected-window window
      (funcall command))
    (lispm-desktop-browser--sync t)))

(defun lispm-desktop-browser-back ()
  "Go back a page in the shown browser tab."
  (interactive)
  (lispm-desktop-browser--in-page #'xwidget-webkit-back))

(defun lispm-desktop-browser-forward ()
  "Go forward a page in the shown browser tab."
  (interactive)
  (lispm-desktop-browser--in-page #'xwidget-webkit-forward))

(defun lispm-desktop-browser-reload ()
  "Load the shown browser tab's page again."
  (interactive)
  (lispm-desktop-browser--in-page #'xwidget-webkit-reload))

(defun lispm-desktop-browser-home ()
  "Go to `lispm-desktop-browser-home-url' in the shown browser tab."
  (interactive)
  (lispm-desktop-browser--in-page
   (lambda () (xwidget-webkit-goto-uri (xwidget-at (point-min)) lispm-desktop-browser-home-url))))

(defun lispm-desktop-browser-stop ()
  "Stop loading the shown browser tab's page."
  (interactive)
  (lispm-desktop-browser--in-page #'xwidget-webkit-stop))

(defun lispm-desktop-browser-search ()
  "Select the location field for a search: what is typed there that is not
an address is searched for, see `lispm-desktop-browser--target'."
  (interactive)
  (lispm-desktop-browser-focus-address))

(defun lispm-desktop-browser--shown-buffer ()
  "Return the shown browser tab's buffer, or nil."
  (when-let* ((window (lispm-desktop-browser--window)))
    (window-buffer window)))

(defun lispm-desktop-browser-open-url (url &optional new-tab)
  "Show the page at URL in the shown browser tab, or in a new tab when
NEW-TAB or when none is shown.  The browser comes in front."
  (let ((window (lispm-desktop-browser--window)))
    (cond ((and window (not new-tab))
           (with-current-buffer (window-buffer window)
             (xwidget-webkit-goto-uri (xwidget-at (point-min)) url))
           (lispm-desktop-browser--sync t))
          (window (lispm-desktop-browser-new-tab url))
          (t (lispm-desktop-browser-show)
             (lispm-desktop-browser-open-url url)))
    (unless (eq (lispm-desktop-apps-frontmost) 'browser)
      (lispm-desktop-browser-show))))

(defun lispm-desktop-browser-close-shown-tab ()
  "Close the shown browser tab, see `lispm-desktop-browser-close-tab'."
  (interactive)
  (when-let* ((window (lispm-desktop-browser--window)))
    (lispm-desktop-browser-close-tab (window-buffer window))))

;;;;; Menus

(declare-function lispm-desktop-browser-add-bookmark "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-edit-bookmarks "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser-show-history "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--bookmark-items "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--places-shown "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-browser--personal-toolbar "lispm-desktop-browser-places" ())
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))

(defun lispm-desktop-browser--file-items ()
  "Return the items of the browser's File menu."
  (let ((page (and (lispm-desktop-browser--window) t)))
    `((:label "New Tab" :action lispm-desktop-browser-new-tab)
      (:label "Open Location…" :action lispm-desktop-browser-focus-address
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-focus-address))
      (:label "Close Tab" :action lispm-desktop-browser-close-shown-tab :enabled ,page)
      separator
      (:label "Open in Default Browser" :action lispm-desktop-browser-open-external :enabled ,page)
      separator
      (:label "Quit Browser" :action lispm-desktop-browser-quit))))

(defun lispm-desktop-browser--view-items ()
  "Return the items of the browser's View menu: full screen or floating,
then reloading and stopping, as Netscape's View menu had them."
  (let ((nav lispm-desktop-browser--navigation))
    `((:label "Full Screen" :action lispm-desktop-browser-full-screen
              :checked ,(not (lispm-desktop-browser-floating-p)))
      (:label "Floating Window" :action lispm-desktop-browser-float
              :checked ,(lispm-desktop-browser-floating-p))
      separator
      (:label "Reload" :action lispm-desktop-browser-reload :enabled ,(plist-get nav :page)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-reload))
      (:label "Stop Loading" :action lispm-desktop-browser-stop :enabled ,(plist-get nav :loading)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-stop)))))

(defun lispm-desktop-browser--session-items ()
  "Return the shown tab's pages back and forward as Go menu items, the one
shown checked, as Netscape's Go menu listed the window's pages.  Only
WebKitGTK tells them; on macOS there are none."
  (when-let* (((fboundp 'xwidget-webkit-back-forward-list))
              (buffer (lispm-desktop-browser--shown-buffer))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min))))
              (pages (ignore-errors (xwidget-webkit-back-forward-list xwidget 8))))
    (pcase-let ((`(,back ,here ,forward) pages))
      (mapcar (pcase-lambda (`(,index ,title ,uri))
                (list :label (truncate-string-to-width
                              (if (string-empty-p (or title "")) (or uri "") title)
                              48 nil nil "…")
                      :action (lambda ()
                                (xwidget-webkit-goto-history xwidget index)
                                (lispm-desktop-browser--sync t))
                      :checked (= index 0)))
              (append (reverse forward) (list here) back)))))

(defun lispm-desktop-browser--go-items ()
  "Return the items of the browser's Go menu: Back, Forward and Home, then
the shown tab's pages."
  (let* ((nav lispm-desktop-browser--navigation)
         (pages (lispm-desktop-browser--session-items)))
    `((:label "Back" :action lispm-desktop-browser-back :enabled ,(plist-get nav :back)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-back))
      (:label "Forward" :action lispm-desktop-browser-forward :enabled ,(plist-get nav :forward)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-forward))
      (:label "Home" :action lispm-desktop-browser-home :enabled ,(plist-get nav :page))
      ,@(when pages (cons 'separator pages)))))

(defun lispm-desktop-browser--window-items ()
  "Return the items of the browser's Window menu: History, then a tab each,
the shown one checked, as Mac OS 9's Window menus listed an application's
windows."
  (let ((shown (lispm-desktop-browser--shown-buffer)))
    `((:label "History" :action lispm-desktop-browser-show-history
              :checked ,(eq (lispm-desktop-browser--places-shown) 'history)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-show-history))
      separator
      ,@(mapcar (lambda (buffer)
                  (list :label (truncate-string-to-width (lispm-desktop-browser--title buffer) 48 nil nil "…")
                        :action (lambda () (lispm-desktop-browser-select-tab buffer))
                        :checked (eq buffer shown)))
                (lispm-desktop-browser--buffers)))))

(declare-function lispm-desktop-browser-extensions-supported-p "lispm-desktop-browser-extensions" ())
(declare-function lispm-desktop-browser-extensions--menu-items "lispm-desktop-browser-extensions" ())

(defun lispm-desktop-browser-menus ()
  "Return the browser's menus, for the menu bar while it is in front.
Extensions is among them where pages can run Chrome extensions."
  `((lispm-desktop-browser-file "File" lispm-desktop-browser--file-items)
    (lispm-desktop-browser-view "View" lispm-desktop-browser--view-items)
    (lispm-desktop-browser-go "Go" lispm-desktop-browser--go-items)
    (lispm-desktop-browser-bookmarks "Bookmarks" lispm-desktop-browser--bookmark-items)
    ,@(when (and (fboundp 'lispm-desktop-browser-extensions-supported-p)
                 (lispm-desktop-browser-extensions-supported-p))
        '((lispm-desktop-browser-extensions-menu "Extensions" lispm-desktop-browser-extensions--menu-items)))
    (lispm-desktop-browser-window "Window" lispm-desktop-browser--window-items)))

;;;;; Drawing Netscape's chrome
;;
;; Each part is an SVG drawn once per palette and state, then kept.

(defvar lispm-desktop-platinum--generation)
(declare-function lispm-desktop-tab-bar--blend "lispm-desktop-interface" (from to ratio))

(defvar lispm-desktop-browser--images (make-hash-table :test #'equal)
  "Drawn chrome images, by palette generation and key.")

(defun lispm-desktop-browser--image (key draw)
  "Return the image of KEY, calling DRAW for its SVG text the first time."
  (let ((key (cons lispm-desktop-platinum--generation key)))
    (or (gethash key lispm-desktop-browser--images)
        (progn
          (when (> (hash-table-count lispm-desktop-browser--images) 256)
            (clrhash lispm-desktop-browser--images))
          (puthash key (create-image (funcall draw) 'svg t :ascent 'center :scale 1)
                   lispm-desktop-browser--images)))))

(defun lispm-desktop-browser--svg (width height &rest parts)
  "Return an SVG document WIDTH by HEIGHT pixels holding PARTS."
  (concat (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                  width height width height)
          (apply #'concat parts)
          "</svg>"))

(defun lispm-desktop-browser--escape (text)
  "Return TEXT safe inside SVG."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun lispm-desktop-browser--font ()
  "Return the toolbar labels' font as (FAMILY SIZE), the size in pixels:
a size under the list font's, as Netscape's labels were small."
  (let* ((font (ignore-errors (face-font 'lispm-desktop-platinum-list)))
         (info (and font (ignore-errors (font-info font))))
         (family (face-attribute 'lispm-desktop-platinum-list :family nil t)))
    (list (if (stringp family) family "Geneva")
          (max 10 (- (if info (aref info 2) 12) 2)))))

(defun lispm-desktop-browser--label-width (label)
  "Return the width in pixels of toolbar LABEL."
  (pcase-let ((`(,family ,size) (lispm-desktop-browser--font)))
    (string-pixel-width (propertize label 'face `(:family ,family :height ,(* 10 size))))))

(defun lispm-desktop-browser--ink (enabled)
  "Return the icons' colours as an alist for `format-spec': outline (i),
fill (f), face (c), the stop sign's red (r) and white (w); greyed unless
ENABLED."
  (let ((red (or (ignore-errors (face-foreground 'error nil t)) "#CC3333")))
    (if enabled
        `((?i . ,(lispm-desktop-platinum--color :text)) (?f . ,(lispm-desktop-platinum--color :accent))
          (?c . ,(lispm-desktop-platinum--color :content)) (?r . ,red) (?w . "#FFFFFF"))
      `((?i . ,(lispm-desktop-platinum--color :shade)) (?f . ,(lispm-desktop-platinum--color :muted))
        (?c . ,(lispm-desktop-platinum--color :chrome)) (?r . ,(lispm-desktop-platinum--color :muted))
        (?w . ,(lispm-desktop-platinum--color :chrome))))))

(defconst lispm-desktop-browser--icons
  '((back . "<path d='M2.5 12 L11.5 3.5 L11.5 8 L21.5 8 L21.5 16 L11.5 16 L11.5 20.5 Z' \
fill='%f' stroke='%i' stroke-linejoin='round'/>")
    (forward . "<path d='M21.5 12 L12.5 3.5 L12.5 8 L2.5 8 L2.5 16 L12.5 16 L12.5 20.5 Z' \
fill='%f' stroke='%i' stroke-linejoin='round'/>")
    (reload . "<path d='M17.6 6.6 A7.6 7.6 0 1 0 19.6 12.6' fill='none' stroke='%i' \
stroke-width='4.6'/><path d='M17.6 6.6 A7.6 7.6 0 1 0 19.6 12.6' fill='none' stroke='%f' \
stroke-width='2.6'/><path d='M13.2 6.8 L21 2.4 L21.2 11 Z' fill='%f' stroke='%i' \
stroke-linejoin='round'/>")
    (home . "<rect x='15.5' y='4.5' width='3' height='5' fill='%f' stroke='%i'/>\
<path d='M2 12.5 L12 3 L22 12.5 Z' fill='%f' stroke='%i' stroke-linejoin='round'/>\
<rect x='5.5' y='12.5' width='13' height='8' fill='%c' stroke='%i'/>\
<rect x='10.5' y='15' width='3' height='5.5' fill='%f' stroke='%i'/>")
    (search . "<path d='M14.6 14.6 L20.4 20.4' stroke='%i' stroke-width='4' \
stroke-linecap='round'/><circle cx='10' cy='10' r='6.2' fill='%c' stroke='%i' \
stroke-width='2.4'/><path d='M6.6 10 A3.4 3.4 0 0 1 10 6.6' fill='none' stroke='%f' \
stroke-width='1.6'/>")
    (history . "<circle cx='12' cy='12' r='9' fill='%c' stroke='%i' stroke-width='1.6'/>\
<path d='M12 3.8 L12 5.8 M20.2 12 L18.2 12 M12 20.2 L12 18.2 M3.8 12 L5.8 12' stroke='%i' \
stroke-width='1.4'/><path d='M12 12 L12 6.8 M12 12 L15.8 14.2' stroke='%f' stroke-width='2.2' \
stroke-linecap='round'/><circle cx='12' cy='12' r='1.3' fill='%i'/>")
    (stop . "<path d='M8 2.5 L16 2.5 L21.5 8 L21.5 16 L16 21.5 L8 21.5 L2.5 16 L2.5 8 Z' \
fill='%r' stroke='%i' stroke-linejoin='round'/><path d='M8.8 8.8 L15.2 15.2 M15.2 8.8 \
L8.8 15.2' stroke='%w' stroke-width='2.6' stroke-linecap='round'/>")
    (bookmarks . "<path d='M5.5 2.5 L18.5 2.5 L18.5 21.5 L12 16.5 L5.5 21.5 Z' fill='%f' \
stroke='%i' stroke-linejoin='round'/><path d='M8.5 6.5 L15.5 6.5' stroke='%c' \
stroke-width='1.5'/>"))
  "The toolbar's icons, each SVG drawn on a 24-pixel square; see
`lispm-desktop-browser--ink' for the colours named in them.")

(defconst lispm-desktop-browser-tool-height 46
  "Height in pixels of a navigation toolbar button.")

(defun lispm-desktop-browser--bevel (width height pressed)
  "Return SVG of a WIDTH by HEIGHT bevel, pushed in when PRESSED."
  (let ((light (lispm-desktop-platinum--color :light))
        (shade (lispm-desktop-platinum--color :shade))
        (chrome (lispm-desktop-platinum--color :chrome)))
    (concat
     (format "<rect width='%d' height='%d' fill='%s'/>" width height
             (if pressed (lispm-desktop-tab-bar--blend chrome shade 0.45) chrome))
     (format "<rect width='%d' height='1' fill='%s'/><rect width='1' height='%d' fill='%s'/>"
             width (if pressed shade light) height (if pressed shade light))
     (format "<rect y='%d' width='%d' height='1' fill='%s'/><rect x='%d' width='1' height='%d' fill='%s'/>"
             (1- height) width (if pressed light shade) (1- width) height (if pressed light shade)))))

(defun lispm-desktop-browser--draw-tool (icon label enabled pressed width)
  "Return SVG of the toolbar button ICON named LABEL, WIDTH pixels wide:
greyed unless ENABLED, pushed in when PRESSED."
  (pcase-let ((`(,family ,size) (lispm-desktop-browser--font))
              (ink (lispm-desktop-browser--ink enabled))
              (height lispm-desktop-browser-tool-height))
    (lispm-desktop-browser--svg
     width height
     (lispm-desktop-browser--bevel width height pressed)
     (format "<g transform='translate(%d %d)'>%s</g>" (/ (- width 24) 2) (if pressed 6 5)
             (format-spec (alist-get icon lispm-desktop-browser--icons) ink))
     (format "<text x='%d' y='%d' font-family='%s' font-size='%d' text-anchor='middle' fill='%s'>%s</text>"
             (/ width 2) (- height 6 (if pressed -1 0)) (lispm-desktop-browser--escape family) size
             (if enabled (lispm-desktop-platinum--color :text) (lispm-desktop-platinum--color :shade))
             (lispm-desktop-browser--escape label)))))

(defconst lispm-desktop-browser--logo
  "<defs><linearGradient id='ns-sky' x1='0' y1='0' x2='0' y2='1'>\
<stop offset='0' stop-color='#03032A'/><stop offset='1' stop-color='#22449C'/></linearGradient>\
<linearGradient id='ns-planet' x1='0' y1='0' x2='0' y2='1'><stop offset='0' \
stop-color='#46C2C2'/><stop offset='1' stop-color='#083650'/></linearGradient>\
<linearGradient id='ns-n' x1='0' y1='0' x2='1' y2='1'><stop offset='0' stop-color='#D8FFFF'/>\
<stop offset='0.55' stop-color='#5CC6D8'/><stop offset='1' stop-color='#1A6E96'/>\
</linearGradient></defs><rect width='40' height='40' fill='url(#ns-sky)'/><g fill='#FFFFFF'>\
<rect x='3' y='4' width='1' height='1'/><rect x='8' y='2' width='1' height='1'/>\
<rect x='19' y='3' width='1' height='1' opacity='0.7'/><rect x='36' y='9' width='1' height='1'/>\
<rect x='5' y='20' width='1' height='1' opacity='0.6'/><rect x='35' y='22' width='1' \
height='1' opacity='0.8'/><rect x='21' y='16' width='1' height='1' opacity='0.5'/>\
<rect x='2' y='12' width='1' height='1' opacity='0.8'/></g>%s<ellipse cx='20' cy='54' \
rx='36' ry='22' fill='url(#ns-planet)'/><path d='M6 6 L16 6 L26 21 L26 9 L23 9 L23 6 L34 6 \
L34 9 L31 9 L31 34 L26 34 L14 16 L14 31 L17 31 L17 34 L6 34 L6 31 L9 31 L9 9 L6 9 Z' \
fill='url(#ns-n)' stroke='#00142E' stroke-width='1.1'/>"
  "The Netscape N over its planet, on a 40-pixel square; %s takes the
meteors, drawn behind the planet.")

(defconst lispm-desktop-browser-throbber-frames 10
  "Frames of the throbber's meteor shower, one every tick while loading.")

(defun lispm-desktop-browser--meteor (x y length opacity)
  "Return SVG of a meteor at X, Y falling down to the left, its tail LENGTH
long, at OPACITY."
  (format "<g opacity='%.2f'><path d='M%.1f %.1f L%.1f %.1f L%.1f %.1f Z' fill='#FFFFFF' \
opacity='0.55'/><circle cx='%.1f' cy='%.1f' r='1.1' fill='#FFFFFF'/></g>"
          opacity x (- y 0.7) (+ x length) (- y (* 0.55 length)) x (+ y 0.7) x y))

(defun lispm-desktop-browser--meteors (frame)
  "Return SVG of the meteors in FRAME of the shower, or of the still sky
when FRAME is nil."
  (if (null frame)
      (lispm-desktop-browser--meteor 29.5 3.5 7 1)
    (mapconcat
     (lambda (start)
       (let ((step (mod (+ frame start) lispm-desktop-browser-throbber-frames)))
         (lispm-desktop-browser--meteor (- 40 (* 3.6 step)) (+ 1 (* 2.4 step)) 8
                                        (- 1 (/ (float step) lispm-desktop-browser-throbber-frames)))))
     '(0 4 7) "")))

(defun lispm-desktop-browser--draw-throbber (frame pressed)
  "Return SVG of the throbber: the Netscape N in a bevel, FRAME of its
meteor shower falling (nil while the page rests), pushed in when PRESSED."
  (let ((size lispm-desktop-browser-tool-height))
    (lispm-desktop-browser--svg
     size size
     (lispm-desktop-browser--bevel size size pressed)
     (format "<svg x='4' y='4' width='%d' height='%d' viewBox='0 0 40 40'>%s</svg>"
             (- size 8) (- size 8) (format lispm-desktop-browser--logo (lispm-desktop-browser--meteors frame))))))

(defun lispm-desktop-browser--draw-padlock (secure)
  "Return SVG of the status bar's padlock: shut and in the accent colour
when SECURE, open and grey when not."
  (let ((ink (lispm-desktop-platinum--color (if secure :text :shade)))
        (fill (lispm-desktop-platinum--color (if secure :accent :muted))))
    (lispm-desktop-browser--svg
     12 14
     (format "<path d='%s' fill='none' stroke='%s' stroke-width='1.6'/>"
             (if secure "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 7"
               "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 5")
             ink)
     (format "<rect x='1.5' y='6.5' width='9' height='7' fill='%s' stroke='%s'/>" fill ink))))

(defun lispm-desktop-browser--draw-progress (fraction)
  "Return SVG of the status bar's progress bar, FRACTION full."
  (let ((filled (round (* (min 1 (max 0 fraction)) 78))))
    (lispm-desktop-browser--svg
     80 10
     (format "<rect width='80' height='10' fill='%s'/>" (lispm-desktop-platinum--color :frame))
     (format "<rect x='1' y='1' width='78' height='8' fill='%s'/>" (lispm-desktop-platinum--color :content))
     (if (> filled 0)
         (format "<rect x='1' y='1' width='%d' height='8' fill='%s'/>" filled
                 (lispm-desktop-platinum--color :accent))
       ""))))

;;;;; Navigation toolbar

(defvar lispm-desktop-browser--throbber-frame nil
  "The throbber's frame while the shown page loads, or nil.")

(defvar lispm-desktop-browser--throbber-timer nil
  "Timer turning the throbber while the shown page loads.")

(defconst lispm-desktop-browser-tools
  '((back "Back" lispm-desktop-browser-back "Go back a page" :back)
    (forward "Forward" lispm-desktop-browser-forward "Go forward a page" :forward)
    (reload "Reload" lispm-desktop-browser-reload "Load the page again" :page)
    (home "Home" lispm-desktop-browser-home "Go to the home page" :page)
    (search "Search" lispm-desktop-browser-search "Type a search in the location field" t)
    (history "History" lispm-desktop-browser-show-history "List the pages visited" t)
    (stop "Stop" lispm-desktop-browser-stop "Stop loading the page" :loading))
  "The navigation toolbar's buttons: (ICON LABEL COMMAND HELP ENABLED).
ENABLED is t, or a key of `lispm-desktop-browser--navigation' saying when.")

(defun lispm-desktop-browser--press-map (command row)
  "Return a keymap pressing a button on ROW that runs COMMAND, see
`lispm-desktop-platinum-press'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map (format "<%s> <down-mouse-1>" row)
                (lambda (event)
                  (interactive "e")
                  (let ((start (event-start event)))
                    (lispm-desktop-platinum-press start (lispm-desktop-platinum--same-target-p start)
                                                  (lambda ()
                                                    (with-selected-window (posn-window start)
                                                      (call-interactively command)))))))
    (keymap-set map (format "<%s> <mouse-1>" row) #'ignore)
    map))

(defun lispm-desktop-browser--tool-button (tool)
  "Return navigation toolbar button TOOL, an entry of the toolbar's tools.
See `lispm-desktop-browser-tools'."
  (pcase-let* ((`(,icon ,label ,command ,help ,condition) tool)
               (enabled (or (eq condition t) (plist-get lispm-desktop-browser--navigation condition)))
               (target (cons 'header-line icon))
               (pressed (and enabled (lispm-desktop-platinum--pressed-p target)))
               (width (max 46 (+ 14 (lispm-desktop-browser--label-width label)))))
    (propertize " " 'display (lispm-desktop-browser--image
                              (list 'tool icon enabled pressed width)
                              (lambda () (lispm-desktop-browser--draw-tool icon label enabled pressed width)))
                'help-echo help 'pointer 'arrow 'lispm-desktop-platinum-target target
                'keymap (and enabled (lispm-desktop-browser--press-map command 'header-line)))))

(defun lispm-desktop-browser--throbber ()
  "Return the throbber, which goes home when clicked, as Netscape's N went
to Netscape's own page."
  (let* ((target '(header-line . throbber))
         (pressed (lispm-desktop-platinum--pressed-p target))
         (frame lispm-desktop-browser--throbber-frame))
    (propertize " " 'display (lispm-desktop-browser--image
                              (list 'throbber frame pressed)
                              (lambda () (lispm-desktop-browser--draw-throbber frame pressed)))
                'help-echo "Go to the home page" 'pointer 'arrow 'lispm-desktop-platinum-target target
                'keymap (lispm-desktop-browser--press-map #'lispm-desktop-browser-home 'header-line))))

(defun lispm-desktop-browser--toolbar ()
  "Return the navigation toolbar, the address window's header line: big
buttons, an icon over each name, and the throbber at the right end."
  (concat (lispm-desktop-platinum--space 6)
          (mapconcat #'lispm-desktop-browser--tool-button lispm-desktop-browser-tools (lispm-desktop-platinum--space 2))
          (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-browser-tool-height 10)))))
          (lispm-desktop-browser--throbber)))

(defun lispm-desktop-browser--turn-throbber ()
  "Turn the throbber one frame while the shown page loads; stop it, still,
once the page rests."
  (let ((loading (when-let* ((buffer (lispm-desktop-browser--shown-buffer)))
                   (buffer-local-value 'xwidget-webkit--loading-p buffer))))
    (if loading
        (setq lispm-desktop-browser--throbber-frame
              (mod (1+ (or lispm-desktop-browser--throbber-frame -1)) lispm-desktop-browser-throbber-frames))
      (setq lispm-desktop-browser--throbber-frame nil)
      (when lispm-desktop-browser--throbber-timer
        (cancel-timer lispm-desktop-browser--throbber-timer)
        (setq lispm-desktop-browser--throbber-timer nil)))
    (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
      (with-current-buffer buffer
        (force-mode-line-update)))))

(defun lispm-desktop-browser--navigation-state ()
  "Return what the shown tab can do, see `lispm-desktop-browser--navigation'.
Where WebKit cannot tell the pages back and forward, as on macOS, Back and
Forward are open to any page."
  (let* ((buffer (lispm-desktop-browser--shown-buffer))
         (xwidget (and buffer (with-current-buffer buffer (xwidget-at (point-min)))))
         (known (fboundp 'xwidget-webkit-back-forward-list))
         (pages (and xwidget known (ignore-errors (xwidget-webkit-back-forward-list xwidget 1)))))
    (list :page (and xwidget t)
          :back (and xwidget (or (not known) (car pages)) t)
          :forward (and xwidget (or (not known) (nth 2 pages)) t)
          :loading (and buffer (buffer-local-value 'xwidget-webkit--loading-p buffer) t))))

;;;;; Status bar

(defvar-local lispm-desktop-browser-status-note nil
  "What the status bar of this page says instead of its state, or nil.
Following a link from the keyboard and typing in a page tell their keys
there.")

(defun lispm-desktop-browser--status ()
  "Return the status bar of the page being drawn: the padlock, then the
progress bar and what is loading, or Netscape's \"Document: Done\", or
the page's `lispm-desktop-browser-status-note'."
  (let* ((xwidget (xwidget-at (point-min)))
         (uri (and xwidget (xwidget-webkit-uri xwidget)))
         (secure (and uri (string-prefix-p "https:" uri)))
         (host (and uri (url-host (url-generic-parse-url uri)))))
    (concat (propertize " " 'display (lispm-desktop-browser--image
                                      (list 'padlock secure)
                                      (lambda () (lispm-desktop-browser--draw-padlock secure))))
            " "
            (if (and xwidget xwidget-webkit--loading-p)
                (let ((progress (or (ignore-errors (xwidget-webkit-estimated-load-progress xwidget))
                                    0)))
                  (concat (propertize " " 'display (lispm-desktop-browser--image
                                                    (list 'progress (round (* 20 progress)))
                                                    (lambda () (lispm-desktop-browser--draw-progress progress))))
                          " "
                          (if (and host (not (string-empty-p host)))
                              (format "Loading %s…" host)
                            "Loading…")))
              (or lispm-desktop-browser-status-note "Document: Done")))))

;;;;; Address window

(defvar-local lispm-desktop-browser--address-start nil
  "Marker at the start of the editable address in the address buffer.")

(defvar-local lispm-desktop-browser--address-edited nil
  "Non-nil once the address was edited since the address buffer was filled.
An edited address is kept while the page stays put; see
`lispm-desktop-browser--sync'.")

(defvar-local lispm-desktop-browser--field nil
  "Overlay drawing the address as an edit field.")

(defun lispm-desktop-browser--note-address-edit (&rest _)
  "Record that the address was edited, from `after-change-functions'."
  (setq lispm-desktop-browser--address-edited t))

(defconst lispm-desktop-browser-address-height 30
  "Pixel height of the address line, the edit field and the toolbar around it.")

(defvar lispm-desktop-browser-bookmarks-button-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lispm-desktop-browser-bookmarks-menu)
    (keymap-set map "<mouse-1>" #'ignore)
    map)
  "Keymap of the location toolbar's Bookmarks button.")

(defun lispm-desktop-browser-bookmarks-menu (&optional _event)
  "Pop up the Bookmarks menu, as the location toolbar's Bookmarks button did."
  (interactive (list last-nonmenu-event))
  (lispm-desktop-popup-menu (lispm-desktop-browser--bookmark-items)))

(defun lispm-desktop-browser--bookmarks-button ()
  "Return the location toolbar's Bookmarks button: a bevel button holding
the bookmark ribbon, its name and a menu arrow."
  (let ((label (concat " "
                       (propertize " " 'display (lispm-desktop-browser--image
                                                 '(ribbon)
                                                 (lambda ()
                                                   (lispm-desktop-browser--svg
                                                    16 16
                                                    (format "<g transform='scale(0.66)'>%s</g>"
                                                            (format-spec (alist-get 'bookmarks lispm-desktop-browser--icons)
                                                                         (lispm-desktop-browser--ink t)))))))
                       " Bookmarks ▾ ")))
    (add-face-text-property 0 (length label) 'lispm-desktop-platinum-button t label)
    (propertize label 'keymap lispm-desktop-browser-bookmarks-button-map 'pointer 'arrow
                'mouse-face 'lispm-desktop-platinum-button-pressed
                'help-echo "Bookmarks: add one, edit them, or go to one")))

(defun lispm-desktop-browser--render-address (uri)
  "Fill the address buffer with the location toolbar and the editable URI:
the Bookmarks button, the page's icon and the Location label, as
Netscape's location toolbar had them.  The URI sits in an edit field, see
`lispm-desktop-browser--draw-field'.  Filling is not an edit: change hooks are
inhibited and the edit mark cleared."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (setq lispm-desktop-browser--address-edited nil)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (page (lispm-desktop-browser--shown-buffer)))
      (erase-buffer)
      (insert (propertize " " 'display `(space :width (8) :height (,lispm-desktop-browser-address-height)
                                               :ascent 65))
              (lispm-desktop-browser--bookmarks-button)
              (lispm-desktop-platinum--space 10)
              (or (and page (lispm-desktop-browser--favicon page)) (lispm-desktop-platinum-icon "globe"))
              " "
              (propertize "Location:" 'face 'lispm-desktop-platinum-toolbar-label)
              " ")
      (add-text-properties (point-min) (point)
                           '(read-only t rear-nonsticky (read-only)))
      (setq lispm-desktop-browser--address-start (copy-marker (point)))
      (insert (or uri ""))
      (lispm-desktop-browser--draw-field)
      (when-let* ((window (get-buffer-window (current-buffer))))
        (set-window-point window (point-max))))))

(defun lispm-desktop-browser--draw-field ()
  "Draw the address as Mac OS 9's edit text field: framed in black, in the
list colour, reaching across the toolbar to a few pixels short of its
right edge; ringed in the focus colour while it has the keyboard.  The
field grows as the address is typed at its end."
  (let ((face (if (eq (selected-window) (get-buffer-window (current-buffer)))
                  'lispm-desktop-platinum-field-focused
                'lispm-desktop-platinum-field)))
    (unless lispm-desktop-browser--field
      (setq lispm-desktop-browser--field (make-overlay 1 1 nil nil t)))
    (overlay-put lispm-desktop-browser--field 'face face)
    (overlay-put lispm-desktop-browser--field 'before-string
                 (propertize " " 'face face 'display '(space :width (4))))
    ;; The cursor at the address's end stands on the field's blank rest.
    (overlay-put lispm-desktop-browser--field 'after-string
                 (propertize " " 'face face 'cursor t
                             'display '(space :align-to (- right (12)))))
    (when lispm-desktop-browser--address-start
      (move-overlay lispm-desktop-browser--field lispm-desktop-browser--address-start (point-max)))))

(defun lispm-desktop-browser--field-focus (_window)
  "Ring the address field while its window has the keyboard, and not after.
This is a buffer-local `window-selection-change-functions' function."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (lispm-desktop-browser--draw-field)))

(defun lispm-desktop-browser--edited-address ()
  "Return the address typed in the address buffer, or nil before its first render."
  (with-current-buffer (lispm-desktop-browser--address-buffer)
    (when lispm-desktop-browser--address-start
      (string-trim (buffer-substring-no-properties lispm-desktop-browser--address-start (point-max))))))

(defun lispm-desktop-browser-address-submit ()
  "Visit the address or search typed in the address window."
  (interactive)
  (when-let* ((input (lispm-desktop-browser--edited-address))
              ((not (string-empty-p input)))
              (window (lispm-desktop-browser--window))
              (target (lispm-desktop-browser--target input)))
    (with-current-buffer (window-buffer window)
      (xwidget-webkit-goto-uri (xwidget-at (point-min)) target))
    (lispm-desktop-browser--render-address target)
    (select-window window)))

(defun lispm-desktop-browser-focus-address ()
  "Select the address window with the whole address marked for replacement:
typing or pasting replaces it, see
`lispm-desktop-browser--replace-marked-address'."
  (interactive)
  (when-let* ((window (lispm-desktop-browser--address-window)))
    (unless (eq (window-frame window) (selected-frame))
      (select-frame-set-input-focus (window-frame window)))
    (select-window window)
    (goto-char (point-max))
    (push-mark lispm-desktop-browser--address-start t t)))

(defun lispm-desktop-browser--replace-marked-address ()
  "Delete the marked address before a command that types or pastes, as
`delete-selection-mode' does, from the address buffer's `pre-command-hook':
the desktop leaves that mode off, and the field behaves so all the same.
Only commands with a `delete-selection' property act on the mark; the
others, moving, selecting, killing, do as they always do."
  (unless delete-selection-mode
    (let ((delete-selection-mode t))
      (delete-selection-pre-hook))))

(defconst lispm-desktop-browser-keys
  '(("C-l" . lispm-desktop-browser-focus-address)
    ("M-l" . lispm-desktop-browser-focus-address)
    ("M-[" . lispm-desktop-browser-back)
    ("M-]" . lispm-desktop-browser-forward)
    ("M-r" . lispm-desktop-browser-reload)
    ("M-." . lispm-desktop-browser-stop)
    ("M-d" . lispm-desktop-browser-add-bookmark)
    ("M-b" . lispm-desktop-browser-edit-bookmarks)
    ("M-y" . lispm-desktop-browser-show-history)
    ("M-t" . lispm-desktop-browser-new-tab)
    ("M-}" . lispm-desktop-browser-next-tab)
    ("M-{" . lispm-desktop-browser-previous-tab)
    ("M-e" . lispm-desktop-browser-extensions-press))
  "The browser's keys, Command keys as Netscape's were where Command is
Meta: Command-[ and ] go back and forward, Command-D bookmarks the page,
Command-B edits the bookmarks.  History is Command-Y, as macOS keeps
Command-H for hiding.  Command-T opens a tab and Command-} and Command-{
go to the next and the previous, as browsers with tabs do; on the page,
Command-W closes the tab, see `lispm-desktop-setup-interface'.  Command-E
presses an extension's button, see `lispm-desktop-browser-extensions-press'.")

(defun lispm-desktop-browser-bind-keys (map)
  "Bind `lispm-desktop-browser-keys' in keymap MAP."
  (pcase-dolist (`(,key . ,command) lispm-desktop-browser-keys)
    (keymap-set map key command)))

(defun lispm-desktop-browser--key (command)
  "Return the key of COMMAND among `lispm-desktop-browser-keys', for a menu."
  (car (rassq command lispm-desktop-browser-keys)))

(defvar lispm-desktop-browser-address-mode-map
  (let ((map (make-sparse-keymap)))
    (lispm-desktop-browser-bind-keys map)
    (keymap-set map "RET" #'lispm-desktop-browser-address-submit)
    map)
  "Keymap of the browser address window.")

(defun lispm-desktop-browser--close-window (_window)
  "Close the browser window's shown tab, from its title bar's close box."
  (lispm-desktop-browser-close-shown-tab))

(defun lispm-desktop-browser--zoom-box (_window)
  "Move the browser between full screen and floating, from its zoom box."
  (lispm-desktop-browser-toggle-floating))

(defun lispm-desktop-browser--collapse-box (_window)
  "Roll the floating browser up to its toolbar, or down again."
  (lispm-desktop-browser-shade))

(defun lispm-desktop-browser--boxes ()
  "Return the browser window's title bar boxes, see
`lispm-desktop-platinum-box-functions': only a floating window rolls up."
  `(:close lispm-desktop-browser--close-window :zoom lispm-desktop-browser--zoom-box
    ,@(when (lispm-desktop-browser-floating-p) '(:collapse lispm-desktop-browser--collapse-box))))

(define-derived-mode lispm-desktop-browser-address-mode fundamental-mode "Address"
  "Major mode of the browser's address window.
Its title bar names the shown page, its header line is the navigation
toolbar, its one line the location toolbar and its mode line the personal
toolbar.
\\{lispm-desktop-browser-address-mode-map}"
  (setq-local mode-line-format '(:eval (lispm-desktop-browser--personal-toolbar))
              header-line-format '(:eval (lispm-desktop-browser--toolbar))
              truncate-lines t
              cursor-type 'bar
              lispm-desktop-platinum-title lispm-desktop-browser-workspace-name
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "netscape")
              lispm-desktop-platinum-box-functions (lispm-desktop-browser--boxes))
  ;; Its height is the toolbars': a frame growing, as on a tall screen,
  ;; gives its height to the page, the toolbars fitted again at once.
  (add-hook 'window-size-change-functions #'lispm-desktop-browser--refit-address nil t)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-toolbar)
  (face-remap-add-relative 'header-line 'lispm-desktop-platinum-header)
  ;; The personal toolbar is a toolbar, lit or not.
  (face-remap-add-relative 'mode-line 'lispm-desktop-platinum-toolbar)
  (face-remap-add-relative 'mode-line-inactive 'lispm-desktop-platinum-toolbar)
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (add-hook 'pre-command-hook #'lispm-desktop-browser--replace-marked-address nil t)
  (add-hook 'after-change-functions #'lispm-desktop-browser--note-address-edit nil t)
  (add-hook 'window-selection-change-functions #'lispm-desktop-browser--field-focus nil t))

(defun lispm-desktop-browser--address-buffer ()
  "Return the address buffer, creating it in `lispm-desktop-browser-address-mode'."
  (or (get-buffer lispm-desktop-browser-address-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-browser-address-buffer-name)
        (lispm-desktop-browser-address-mode)
        (current-buffer))))

;;;;; Page icons
;;
;; A tab shows its page's icon.  Once a page has loaded, a script reads the
;; icon's address from the page; the icon is fetched once per address,
;; without cookies, and decoded by the image backend:
;;
;;   page --script--> icon address --fetch--> bytes --decode--> image
;;
;; Until the icon is known, or when the page has none, a globe stands in.

(defconst lispm-desktop-browser--favicon-script
  "(function () {
  var link = document.querySelector('link[rel~=\"icon\"]')
          || document.querySelector('link[rel=\"apple-touch-icon\"]');
  var icon = link && link.href ? link.href
           : location.protocol.indexOf('http') === 0
             ? location.origin + '/favicon.ico' : '';
  return location.href + '\\n' + icon;
})()"
  "Script returning the document's address and its icon's, on two lines.
A page names its icon in a link element; failing that, browsers look for
/favicon.ico at the page's origin.  The icon line is empty when there is
none.  The document's address tells whose icon it is: while a page loads,
the view's address is already the new page's but the document is the old.")

(defvar lispm-desktop-browser--favicons (make-hash-table :test #'equal)
  "Icon per icon address: an image, `pending' while fetched, `none' if unusable.")

(defvar-local lispm-desktop-browser--favicon-source nil
  "(PAGE . ICON): the page address this tab's icon was looked up for, and
the icon's address, `pending' while the page is asked, or nil for none.")

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun lispm-desktop-browser--favicon-image (data)
  "Return icon DATA as an image a line high, or nil when it is no image.
Formats Emacs recognizes are accepted, and ICO by its magic bytes: the
macOS image backend decodes ICO when asked for PNG.  Anything else would
draw as an empty box, so it is refused."
  (when-let* ((type (or (image-type-from-data data)
                        (and (string-prefix-p "\0\0\1\0" data) 'png))))
    (create-image data type t
                  :height (round (* 0.85 (frame-char-height)))
                  :ascent 'center)))

(defun lispm-desktop-browser--data-url-bytes (url)
  "Return the bytes a data URL holds, or nil when URL is malformed."
  (when (string-match "\\`data:[^,]*?\\(;base64\\)?," url)
    (let ((base64 (match-beginning 1))
          (payload (substring url (match-end 0))))
      (if base64
          (ignore-errors (base64-decode-string payload))
        (encode-coding-string (url-unhex-string payload) 'utf-8)))))

(defun lispm-desktop-browser--retitle ()
  "Name the browser window after the shown page, with the page's icon.
Each tab's window bottom is named after its page too, not after the
buffer, whose name WebKit prefixes."
  (dolist (buffer (lispm-desktop-browser--buffers))
    (with-current-buffer buffer
      (setq lispm-desktop-platinum-title (lispm-desktop-browser--title buffer))))
  (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (with-current-buffer buffer
        (setq lispm-desktop-platinum-title (if shown (lispm-desktop-browser--title shown)
                                             lispm-desktop-browser-workspace-name)
              lispm-desktop-platinum-icon (or (and shown (lispm-desktop-browser--favicon shown))
                                              (lispm-desktop-platinum-icon "globe")))))))

(defun lispm-desktop-browser--redraw-tabs ()
  "Redraw the browser window's title bar and tabs, leaving the address alone."
  (lispm-desktop-browser--retitle)
  (force-mode-line-update t))

(defun lispm-desktop-browser--store-favicon (icon data)
  "Record icon address ICON as fetched with DATA, nil on failure, and redraw."
  (puthash icon (or (and data (lispm-desktop-browser--favicon-image data)) 'none)
           lispm-desktop-browser--favicons)
  (lispm-desktop-browser--redraw-tabs))

(defun lispm-desktop-browser--fetch-favicon (icon)
  "Fetch the icon at address ICON, once, and store it as it arrives.
A data URL is decoded in place; others are fetched without cookies, and
only a successful response's body is kept."
  (puthash icon 'pending lispm-desktop-browser--favicons)
  (if (string-prefix-p "data:" icon)
      (lispm-desktop-browser--store-favicon icon (lispm-desktop-browser--data-url-bytes icon))
    (require 'url)
    (url-retrieve
     icon
     (lambda (status)
       (let ((data (and (not (plist-get status :error))
                        (eql url-http-response-status 200)
                        (bound-and-true-p url-http-end-of-headers)
                        (buffer-substring-no-properties
                         (1+ url-http-end-of-headers) (point-max)))))
         (kill-buffer)
         (lispm-desktop-browser--store-favicon icon data)))
     nil t t)))

(defun lispm-desktop-browser--look-up-favicon (buffer page)
  "Ask browser BUFFER's PAGE for its icon address, then fetch the icon.
Only web and data addresses are followed.  An answer arriving after the
tab moved to another page is dropped.  An answer from another document
than PAGE's, the one being replaced while PAGE loads, is no answer: the
lookup is forgotten and runs again at the next redraw, which the finished
load brings."
  (with-current-buffer buffer
    (setq lispm-desktop-browser--favicon-source (cons page 'pending))
    (when-let* ((xwidget (xwidget-at (point-min))))
      (lispm-desktop-browser--run-script
       xwidget lispm-desktop-browser--favicon-script
       (lambda (answer)
         (when (and (buffer-live-p buffer)
                    (equal (car (buffer-local-value 'lispm-desktop-browser--favicon-source buffer))
                           page))
           (pcase-let* ((`(,document ,icon) (and (stringp answer)
                                                 (split-string answer "\n")))
                        (icon (and icon
                                   (string-match-p "\\`\\(?:https?\\|data\\):" icon)
                                   icon)))
             (with-current-buffer buffer
               (setq lispm-desktop-browser--favicon-source
                     (and (equal document page) (cons page icon))))
             (when (and icon (equal document page)
                        (not (gethash icon lispm-desktop-browser--favicons)))
               (lispm-desktop-browser--fetch-favicon icon))
             (lispm-desktop-browser--redraw-tabs))))))))

(defun lispm-desktop-browser--favicon (buffer)
  "Return browser BUFFER's page icon as a string showing it, or nil.
Nil stands for an icon not known yet or absent.  A loaded page whose icon
was not looked up yet has its lookup started."
  (when-let* ((page (lispm-desktop-browser--uri buffer)))
    (let ((source (buffer-local-value 'lispm-desktop-browser--favicon-source buffer)))
      (cond ((buffer-local-value 'xwidget-webkit--loading-p buffer) nil)
            ((not (equal (car source) page))
             (lispm-desktop-browser--look-up-favicon buffer page)
             nil)
            ((stringp (cdr source))
             (let ((image (gethash (cdr source) lispm-desktop-browser--favicons)))
               (when (consp image)
                 (propertize " " 'display image))))))))

;;;;; Tabs

(defun lispm-desktop-browser--tab-command (function &rest args)
  "Return an interactive command applying FUNCTION to ARGS."
  (lambda () (interactive) (apply function args)))

(defun lispm-desktop-browser--tab-keymap (command)
  "Return the keymap of a tab atop the page, running COMMAND when clicked."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <mouse-1>" command)
    map))

(defun lispm-desktop-browser--tab-line ()
  "Return the row of tabs atop the page: a tab per page, then + for a new one.
The tabs are Mac OS 9's, the shown page's in front.  Each shows its
page's icon before its title, or the globe until the icon is known and
when the page has none."
  (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
    (lispm-desktop-platinum-tabs
     (append
      (mapcar (lambda (buffer)
                (list (concat (or (lispm-desktop-browser--favicon buffer) (lispm-desktop-platinum-icon "globe"))
                              " "
                              (truncate-string-to-width (lispm-desktop-browser--title buffer)
                                                        lispm-desktop-browser-tab-width nil nil "…"))
                      (eq buffer shown)
                      'keymap (lispm-desktop-browser--tab-keymap
                               (lispm-desktop-browser--tab-command #'lispm-desktop-browser-select-tab buffer))
                      'help-echo (or (lispm-desktop-browser--uri buffer) "Show this tab")
                      'pointer 'arrow))
              (lispm-desktop-browser--buffers))
      (list (list "+" nil 'keymap (lispm-desktop-browser--tab-keymap #'lispm-desktop-browser-new-tab)
                  'help-echo "New tab" 'pointer 'arrow)))
     (- (window-pixel-width) (window-right-divider-width) (window-scroll-bar-width)))))

(defun lispm-desktop-browser-select-tab (buffer)
  "Show browser tab BUFFER in the browser window."
  (when-let* ((window (lispm-desktop-browser--window)))
    (set-window-buffer window buffer)
    (lispm-desktop-browser--sync t)))

(defun lispm-desktop-browser--step-tab (step)
  "Show the browser tab STEP places from the one shown, wrapping around."
  (let* ((tabs (lispm-desktop-browser--buffers))
         (shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window))))
         (index (or (seq-position tabs shown) 0)))
    (when (cdr tabs)
      (lispm-desktop-browser-select-tab (nth (mod (+ index step) (length tabs)) tabs)))))

(defun lispm-desktop-browser-next-tab ()
  "Show the browser's next tab."
  (interactive)
  (lispm-desktop-browser--step-tab 1))

(defun lispm-desktop-browser-previous-tab ()
  "Show the browser's previous tab."
  (interactive)
  (lispm-desktop-browser--step-tab -1))

(defun lispm-desktop-browser-new-tab (&optional url)
  "Open URL, by default `lispm-desktop-browser-home-url', in a new browser tab and
show it."
  (interactive)
  (with-selected-window (or (lispm-desktop-browser--window) (lispm-desktop-agent--main-window))
    (xwidget-webkit-browse-url (or url lispm-desktop-browser-home-url) t))
  (lispm-desktop-browser--sync t))

(declare-function lispm-desktop-show "lispm-desktop-surface" (&optional window))
(declare-function lispm-desktop-empty-buffer "lispm-desktop-surface" ())
(declare-function lispm-desktop--new-tab-empty "lispm-desktop-surface" ())
(declare-function lispm-desktop--select-tab "lispm-desktop-surface" (&optional frame))
(declare-function lispm-desktop-editor-hide "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(defvar lispm-desktop-activate-hook)
(defvar lispm-desktop-editor--frame)

(defun lispm-desktop-browser-close-tab (buffer)
  "Close browser tab BUFFER without asking, then show another tab.
Closing the last tab quits the browser, see `lispm-desktop-browser-quit'."
  (let ((window (lispm-desktop-browser--window))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)
    (if-let* ((next (car (lispm-desktop-browser--buffers))))
        (when (window-live-p window) (set-window-buffer window next))
      (when-let* ((address (lispm-desktop-browser--address-window)))
        (delete-window address))
      ;; Nothing is left open: the browser quits, and the desktop comes in
      ;; front with its icons.
      (lispm-desktop-browser-quit)))
  (lispm-desktop-browser--sync t))

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())

(defun lispm-desktop-browser-quit ()
  "Quit the browser: close its tabs, its window and its workspace, and bring
the desktop in front.  It opens again full screen or floating, as it was."
  (interactive)
  (lispm-desktop-browser--delete-frame)
  (let ((kill-buffer-query-functions nil))
    (mapc #'kill-buffer (lispm-desktop-browser--buffers))
    (when-let* ((address (get-buffer lispm-desktop-browser-address-buffer-name)))
      (kill-buffer address)))
  (lispm-desktop-activate)
  (lispm-desktop-browser--close-workspace))

;;;;; Layout and synchronisation

(defvar lispm-desktop-browser--sync-timer nil
  "Repeating timer keeping the address window in step with the page.")

(defvar lispm-desktop-browser--synced-state nil
  "Address, tab titles, shown tab and what it can do at the last sync.")


(defun lispm-desktop-browser--state ()
  "Return what the address window shows: the address, tab titles, shown tab.
What the shown tab can do is included, see
`lispm-desktop-browser--navigation': a finished load redraws the tab line,
whose page icons are looked up once a page has loaded, and the toolbar, whose
buttons it enables."
  (let ((shown (lispm-desktop-browser--shown-buffer)))
    (list (lispm-desktop-browser--uri)
          (mapcar #'buffer-name (lispm-desktop-browser--buffers))
          shown
          (lispm-desktop-browser--navigation-state))))

(defun lispm-desktop-browser--sync (&optional force)
  "Show the current page's address and tabs in the address window.
Nothing is redrawn unless the state of `lispm-desktop-browser--state' changed,
or FORCE: the page is never redisplayed by an idle tick, which could drop a
click in progress.  The address follows the page unless it was edited: an
edited address is kept while the page stays put, and while it is being typed
even when the page moves; FORCE, as navigation and switching tabs pass, always
shows the page's address.  The timer stops once no address window is left."
  (if-let* ((address (lispm-desktop-browser--address-window)))
      (let* ((state (lispm-desktop-browser--state))
             (moved (not (equal (car state) (car lispm-desktop-browser--synced-state))))
             (edited (buffer-local-value 'lispm-desktop-browser--address-edited
                                         (window-buffer address))))
        (when (or force (not (equal state lispm-desktop-browser--synced-state)))
          (setq lispm-desktop-browser--synced-state state
                lispm-desktop-browser--navigation (nth 3 state))
          (when (and (plist-get lispm-desktop-browser--navigation :loading)
                     (not lispm-desktop-browser--throbber-timer))
            (setq lispm-desktop-browser--throbber-timer
                  (run-with-timer 0 0.12 #'lispm-desktop-browser--turn-throbber)))
          (lispm-desktop-browser--redraw-tabs)
          (when (or force (not edited)
                    (and moved (not (eq address (selected-window)))))
            (lispm-desktop-browser--render-address (car state)))
          (with-current-buffer (window-buffer address)
            (force-mode-line-update))))
    (when lispm-desktop-browser--sync-timer
      (cancel-timer lispm-desktop-browser--sync-timer)
      (setq lispm-desktop-browser--sync-timer nil))))

(defun lispm-desktop-browser--fit-address ()
  "Fit the address window to its title bar, toolbar and address line.
They are taller than text lines, so the window is fitted in pixels, then
kept so: a frame growing, as on a tall screen, gives its height to the
page, never to the toolbars."
  (when-let* ((address (lispm-desktop-browser--address-window)))
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer address nil nil nil nil t))))

(defun lispm-desktop-browser--refit-address (window)
  "Fit the address WINDOW to its toolbars again once its size changed, as
a frame growing or shrinking changes it, unless it is alone in its frame,
as in a rolled-up browser.  This is a buffer-local
`window-size-change-functions' function."
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lispm-desktop-browser--ensure-layout ()
  "Put the address window above the browser window, once, and keep it synced.
The address window is fitted to its lines, see
`lispm-desktop-browser--fit-address'."
  (when-let* ((window (lispm-desktop-browser--window)))
    (with-current-buffer (lispm-desktop-browser--address-buffer)
      (setq lispm-desktop-platinum-box-functions (lispm-desktop-browser--boxes)))
    (unless (lispm-desktop-browser--address-window)
      (let* ((window-min-height 1)
             (address (split-window window -2 'above)))
        (set-window-buffer address (lispm-desktop-browser--address-buffer))
        (set-window-parameter address 'lispm-desktop-chrome t)
        (set-window-dedicated-p address t)))
    ;; The address window wears the browser window's title bar, active
    ;; while the page is.
    (set-window-parameter (lispm-desktop-browser--address-window) 'lispm-desktop-platinum-title-for window)
    (lispm-desktop-browser--sync t)
    ;; A new window's lines are measured right only once it was drawn, so
    ;; it is fitted again after the next redisplay.
    (lispm-desktop-browser--fit-address)
    (run-at-time 0 nil #'lispm-desktop-browser--fit-address)
    (unless lispm-desktop-browser--sync-timer
      (setq lispm-desktop-browser--sync-timer (run-with-timer 0.7 0.7 #'lispm-desktop-browser--sync)))))

(defun lispm-desktop-browser-show ()
  "Show the browser, full screen or floating as it was last, with the most
recent tab, opening `lispm-desktop-browser-home-url' when there is no tab.  The
desktop's Browser icon runs this, and the browser comes in front."
  (interactive)
  ;; With the desktop hidden, the browser floats out on the computer's screen.
  (if (or lispm-desktop-browser-floating (lispm-desktop-apps-hidden-p))
      (lispm-desktop-browser--show-floating)
    (lispm-desktop-browser--show-full-screen))
  (force-mode-line-update t))

(defun lispm-desktop-browser--show-full-screen (&optional shown)
  "Show the browser in its workspace, SHOWN or its first tab in the main area
under the address window.  The floating editor, whose window would cover
the page, is hidden.  The workspace starts on the desktop's bare colour,
not the editor's start page, which a document there would take for the
editor's own; it is shown in place, see `lispm-desktop-show-in-new-tab', so
no session of the workspace left comes along."
  (when (fboundp 'lispm-desktop-editor-hide)
    (lispm-desktop-editor-hide))
  (lispm-desktop-apps-set-frontmost 'browser)
  (let* ((fresh (let ((tab-bar-new-tab-choice #'lispm-desktop--new-tab-empty))
                  (lispm-desktop-workspace-enter lispm-desktop-browser-workspace-name)))
         (browser (or shown (car (lispm-desktop-browser--buffers)))))
    (cond ((and browser fresh) (lispm-desktop-workspace-show-alone browser))
          (browser (unless (get-buffer-window browser)
                     (select-window (lispm-desktop-agent--main-window))
                     (pop-to-buffer-same-window browser)))
          (t (select-window (lispm-desktop-agent--main-window))
             (xwidget-webkit-browse-url lispm-desktop-browser-home-url t)
             (when fresh (tabspaces-reset-buffer-list))))
    (lispm-desktop-browser--ensure-layout)))

(defun lispm-desktop-browser--default-geometry (screen)
  "Return where the browser floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (tab-bar-height screen t) 30))
         (left (round (* width 0.1))))
    (list left top (- width (* 2 left)) (- height top 50))))

(defun lispm-desktop-browser--make-frame (screen)
  "Return the floating browser's child frame of SCREEN, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lispm-desktop-browser--geometry (lispm-desktop-browser--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (with-current-buffer (lispm-desktop-empty-buffer)
                          (make-frame
                           `((parent-frame . ,screen)
                             (minibuffer . ,(minibuffer-window screen))
                             (name . "Browser") (lispm-desktop-browser . t) (lispm-desktop-app . browser)
                             (lispm-desktop-window . t) (top-visible . 30)
                             (undecorated . t) (visibility . nil)
                             (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                             (menu-bar-lines . 0) (tool-bar-lines . 0)
                             (internal-border-width . 0)
                             (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                             (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                             (user-position . t)
                             (left . ,left) (top . ,top)
                             (width . (text-pixels . ,width))
                             (height . (text-pixels . ,height))))))))
    (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum--color :chrome))
    frame))

(defun lispm-desktop-browser--show-floating (&optional shown)
  "Show the browser floating over the desktop, SHOWN or its first tab in its
window, the window made when missing.  The floating windows left over the
desktop show with it."
  (let ((screen (lispm-desktop-apps-screen))
        (browser (or shown (car (lispm-desktop-browser--buffers)))))
    (unless (lispm-desktop-apps-hidden-p screen)
      (with-selected-frame screen
        (lispm-desktop--select-tab))
      (run-hooks 'lispm-desktop-activate-hook))
    (unless (lispm-desktop-browser-floating-p)
      (setq lispm-desktop-browser--frame (lispm-desktop-browser--make-frame screen)))
    (with-selected-frame lispm-desktop-browser--frame
      (cond ((lispm-desktop-browser--window)
             (when shown (set-window-buffer (lispm-desktop-browser--window) shown)))
            (browser (set-window-buffer (frame-first-window) browser))
            (t (select-window (frame-first-window))
               (xwidget-webkit-browse-url lispm-desktop-browser-home-url t)))
      (lispm-desktop-browser--ensure-layout))
    (lispm-desktop-platinum-show-window lispm-desktop-browser--frame)
    (lispm-desktop-apps-keep-on-desktop lispm-desktop-browser--frame)
    (select-frame-set-input-focus lispm-desktop-browser--frame)
    (select-window (or (lispm-desktop-browser--window) (frame-first-window lispm-desktop-browser--frame)))
    (lispm-desktop-apps-set-frontmost 'browser)))

(defun lispm-desktop-browser--delete-frame ()
  "Delete the floating browser's window, remembering where it was."
  (when (lispm-desktop-browser-floating-p)
    (let ((frame lispm-desktop-browser--frame))
      (lispm-desktop-browser--unshade)
      (setq lispm-desktop-browser--geometry (lispm-desktop-apps--geometry frame)
            lispm-desktop-browser--frame nil)
      (when (eq (selected-frame) frame)
        (lispm-desktop-apps-focus-after frame))
      (delete-frame frame))))

(defun lispm-desktop-browser--close-workspace ()
  "Close the full-screen browser's workspace, the desktop's in front instead."
  (when-let* ((screen (lispm-desktop-apps-screen))
              ((with-selected-frame screen
                 (tab-bar--tab-index-by-name lispm-desktop-browser-workspace-name))))
    (with-selected-frame screen
      (when (lispm-desktop-browser-workspace-p)
        (lispm-desktop--select-tab))
      (tab-bar-close-tab (1+ (tab-bar--tab-index-by-name lispm-desktop-browser-workspace-name))))))

(defun lispm-desktop-browser-float ()
  "Float the browser in a window of its own over the desktop."
  (interactive)
  (setq lispm-desktop-browser-floating t)
  (unless (lispm-desktop-browser-floating-p)
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (lispm-desktop-browser--close-workspace)
      (lispm-desktop-browser--show-floating shown))))

(defun lispm-desktop-browser-full-screen ()
  "Bring the floating browser back to full screen, under the menu bar."
  (interactive)
  (setq lispm-desktop-browser-floating nil)
  ;; Full screen is the desktop's: it shows first.
  (when (and (lispm-desktop-apps-hidden-p) (fboundp 'lispm-desktop-show-desktop))
    (lispm-desktop-show-desktop))
  (when (lispm-desktop-browser-floating-p)
    (let ((shown (and (lispm-desktop-browser--window) (window-buffer (lispm-desktop-browser--window)))))
      (lispm-desktop-browser--delete-frame)
      (lispm-desktop-browser--show-full-screen shown))))

(defun lispm-desktop-browser-toggle-floating ()
  "Move the browser between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lispm-desktop-browser-floating-p) (lispm-desktop-browser-full-screen) (lispm-desktop-browser-float)))

(defun lispm-desktop-browser--unshade ()
  "Roll the floating browser down again, when it is rolled up."
  (when lispm-desktop-browser--shade
    (pcase-let ((`(,config . ,height) lispm-desktop-browser--shade))
      (setq lispm-desktop-browser--shade nil)
      (when (lispm-desktop-browser-floating-p)
        (lispm-desktop-platinum-roll-down lispm-desktop-browser--frame height)
        (with-selected-frame lispm-desktop-browser--frame
          (set-window-configuration config))))))

(defun lispm-desktop-browser-shade ()
  "Roll the floating browser up to its title bar and toolbar, or down
again, as Mac OS 9 rolled a window up into its title bar."
  (interactive)
  (when (lispm-desktop-browser-floating-p)
    (if lispm-desktop-browser--shade
        (lispm-desktop-browser--unshade)
      (when-let* ((address (lispm-desktop-browser--address-window)))
        (with-selected-frame lispm-desktop-browser--frame
          (let ((height (window-pixel-height address))
                (ignore-window-parameters t))
            (setq lispm-desktop-browser--shade (cons (current-window-configuration)
                                                     (frame-text-height lispm-desktop-browser--frame)))
            (delete-other-windows address)
            (lispm-desktop-platinum-roll-up lispm-desktop-browser--frame height)))))))

(defun lispm-desktop-browser--follow-frontmost (app)
  "Show the floating browser with the desktop and alongside the floating
editor; hide it from the full-screen editor, whose workspace it would
cover.  This is one of `lispm-desktop-apps-frontmost-functions'."
  (when (and (lispm-desktop-browser-floating-p)
             ;; Out on the computer's screen, it stays where it is.
             (not (lispm-desktop-apps-out-p lispm-desktop-browser--frame)))
    (let ((shown (or (memq app '(desktop browser settings))
                     (and (eq app 'editor) (fboundp 'lispm-desktop-editor-floating-p)
                          (lispm-desktop-editor-floating-p)))))
      (cond ((and shown (not (frame-visible-p lispm-desktop-browser--frame)))
             (lispm-desktop-platinum-show-window lispm-desktop-browser--frame)
             ;; The editor in front stays over it.
             (when (and (eq app 'editor) (frame-live-p (bound-and-true-p lispm-desktop-editor--frame)))
               (lispm-desktop-platinum-raise-window lispm-desktop-editor--frame)))
            ((and (not shown) (frame-visible-p lispm-desktop-browser--frame))
             (lispm-desktop-platinum-hide-window lispm-desktop-browser--frame))))))

(defun lispm-desktop-browser--refocus ()
  "Give the floating browser the keyboard back after a menu bar menu closes.
A click on the menu bar focuses the frame under it; the browser stays in
front, so it takes the focus again."
  (when (and (eq (lispm-desktop-apps-frontmost) 'browser) (lispm-desktop-browser-floating-p)
             (frame-visible-p lispm-desktop-browser--frame)
             (not (eq (selected-frame) lispm-desktop-browser--frame)))
    (select-frame-set-input-focus lispm-desktop-browser--frame)))

(defun lispm-desktop-browser ()
  "Switch to the browser, or back to the previous workspace when its
full-screen workspace is in front; see `lispm-desktop-browser-show'."
  (interactive)
  (if (lispm-desktop-browser-workspace-p)
      (progn (tab-bar-switch-to-recent-tab)
             (force-mode-line-update t))
    (lispm-desktop-browser-show)))

(defun lispm-desktop-browser--arrow-pointer (buffer)
  "Show the arrow mouse pointer over the page in browser BUFFER.
Emacs picks the pointer over a window itself, and the page is a character
displaying the WebKit view, so it would otherwise get the text I-beam."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local void-text-area-pointer 'arrow)
      (with-silent-modifications
        (let ((inhibit-read-only t))
          (put-text-property (point-min) (point-max) 'pointer 'arrow))))))

(defun lispm-desktop-browser--setup ()
  "Set up a new browser tab's buffer and append it to the tab order.
Its tabs live in the address window, which stands at the top of the
browser as its title bar, so the page shows neither a title bar of its own
nor WebKit's title line.  The page is inserted after this mode hook runs,
so the arrow pointer is applied once the current command finishes."
  ;; The browser window's title bar is the address window's, above the
  ;; toolbar; the page's own top line holds the tabs, and WebKit's title
  ;; line would only repeat the title.
  (setq-local tab-line-format '(:eval (lispm-desktop-browser--tab-line))
              header-line-format nil
              ;; The bottom edge is Netscape's status bar.
              lispm-desktop-platinum-info #'lispm-desktop-browser--status)
  ;; The title bar above lights and dims with the page.
  (add-hook 'window-selection-change-functions
            (lambda (_) (force-mode-line-update t)) nil t)
  ;; The unified copy and paste keys act on the page's selection and its
  ;; focused field, not on the buffer, which holds only the view.
  (setq-local lispm-desktop-copy-function #'xwidget-webkit-copy-selection-as-kill
              lispm-desktop-paste-function #'lispm-desktop-browser--paste)
  (run-at-time 0 nil #'lispm-desktop-browser--arrow-pointer (current-buffer))
  (display-line-numbers-mode -1)
  (setq lispm-desktop-browser--tab-order (append lispm-desktop-browser--tab-order (list (current-buffer)))))

(defun lispm-desktop-browser--on-workspace-select (&rest _)
  "Resume address synchronisation when the browser workspace is selected."
  (when (lispm-desktop-browser-workspace-p)
    (lispm-desktop-browser--ensure-layout)))

;;;;; Late events of closed pages
;;
;; Closing a page could crash Emacs on macOS (Emacs 31.1, NS build): a
;; segmentation fault in print_object, under `xwidget-log', under
;; `xwidget-event-handler'.  The bug is Emacs's.  A page's WebKit view
;; keeps a plain C pointer to its xwidget, in its navigation delegate and
;; in the completion handler of each script it runs, and the garbage
;; collector does not see it.  Killing the page's buffer kills its xwidget,
;; which itself loads about:blank to silence the page, yet WebKit may still
;; call back: a load or a script finishing after the kill posts an
;; `xwidget-event' naming the xwidget, by then perhaps collected.  The
;; handler first logs the event with %S, printing the freed object; the
;; collector, marking the event while it waits to be read, may fault on it
;; even before.  It is a race, lost now and then; stopping the page before
;; killing it would not win it, as the kill loads a page of its own.
;;
;; So the xwidgets of a killed buffer are kept from the collector a minute,
;; which keeps WebKit's pointer valid: its navigation callbacks then find
;; the buffer dead and post nothing, and a finished script's event names a
;; killed but valid xwidget.  And an event whose xwidget is no longer in
;; `xwidget-list' is dropped before the handler runs.  `memq' finds the
;; xwidget there or not without reading it; `xwidget-live-p' would read it,
;; and on macOS a killed xwidget keeps its buffer, which it checks.

(defvar xwidget-list)
(declare-function get-buffer-xwidgets "xwidget.c" (buffer))

(defvar lispm-desktop-browser--killed-xwidgets nil
  "Xwidgets of killed buffers, kept from the garbage collector a minute.")

(defun lispm-desktop-browser--keep-killed-xwidgets ()
  "Keep the xwidgets of the buffer being killed from the garbage collector.
They are let go a minute later, when WebKit has long called back for the
last time.  This is on `kill-buffer-hook', which runs before the buffer's
xwidgets are killed."
  (when-let* (((fboundp 'get-buffer-xwidgets))
              (xwidgets (get-buffer-xwidgets (current-buffer))))
    (setq lispm-desktop-browser--killed-xwidgets
          (append xwidgets lispm-desktop-browser--killed-xwidgets))
    (run-at-time 60 nil #'lispm-desktop-browser--let-go-xwidgets xwidgets)))

(defun lispm-desktop-browser--let-go-xwidgets (xwidgets)
  "Let the garbage collector have XWIDGETS, of a buffer killed a minute ago."
  (dolist (xwidget xwidgets)
    (setq lispm-desktop-browser--killed-xwidgets
          (delq xwidget lispm-desktop-browser--killed-xwidgets))))

(defun lispm-desktop-browser--xwidget-event-live-p (&optional event)
  "Return non-nil unless EVENT is an `xwidget-event' of a killed xwidget.
EVENT defaults to `last-input-event'.  As an advice of
`xwidget-event-handler', this drops the events of killed xwidgets, which
the handler would print although they may have been freed."
  (let ((event (or event last-input-event)))
    (or (not (eq (car-safe event) 'xwidget-event))
        (memq (nth 2 event) (bound-and-true-p xwidget-list)))))

(defun lispm-desktop-setup-browser ()
  "Keep the late events of closed pages from crashing Emacs."
  (add-hook 'kill-buffer-hook #'lispm-desktop-browser--keep-killed-xwidgets)
  (advice-add 'xwidget-event-handler :before-while #'lispm-desktop-browser--xwidget-event-live-p))

;;;;; Callbacks of page scripts
;;
;; A function given to `xwidget-webkit-execute-script' could crash Emacs on
;; macOS (Emacs 31.1, NS build).  The bug is Emacs's.  Only GTK builds keep
;; the function from the garbage collector, saved with the xwidget; on
;; macOS it is held only by the completion handler WebKit runs when the
;; script returns (src/nsxwidget.m), which the collector does not see.  A
;; closure nothing else holds, such as a lambda written at the call, may
;; then be collected while the script runs.  The handler tests the freed
;; object with FUNCTIONP and posts an `xwidget-event' holding it, which
;; `xwidget-event-handler' prints with `xwidget-log' and calls; the
;; collector, marking the event while it waits to be read, may fault on it
;; even before.  A symbol is safe, as an interned symbol is never collected.
;;
;; So pages are sent scripts by `lispm-desktop-browser--run-script', which
;; keeps each callback in `lispm-desktop-browser--script-callbacks' until
;; it is called, with the page's xwidget, which the event names.  On
;; macOS, WebKit calls back only with a value: not when a script returns
;; null or undefined, fails, or is blocked by the page's policy.  Such
;; callbacks would be kept for ever, so the list is pruned as scripts are
;; sent.  A callback a minute old, long after WebKit would have called
;; it, is let go if its page is closed, its xwidget no longer in
;; `xwidget-list'; of those whose page is open, only the newest few are
;; kept.  A page just closed may still answer, so its callbacks are not
;; let go at once.

(declare-function xwidget-webkit-execute-script "xwidget.c" (xwidget script &optional fun))

(defconst lispm-desktop-browser--script-callback-age 60
  "Seconds a script's callback is kept at least, waiting to be called.")

(defconst lispm-desktop-browser--script-callbacks-kept 32
  "How many callbacks past that age are kept for open pages, the newest.")

(defvar lispm-desktop-browser--script-callbacks nil
  "Callbacks of scripts sent to pages, kept from the garbage collector.
Each is (FUNCTION XWIDGET TIME): the function WebKit calls with the
script's value, the page's xwidget, and when the script was sent.  The
newest comes first.")

(defun lispm-desktop-browser--prune-script-callbacks ()
  "Let go of the callbacks of scripts WebKit will not call back for.
A callback a minute old is let go when its page is closed; of those
whose page is open, the newest `lispm-desktop-browser--script-callbacks-kept'
are kept."
  (let ((now (float-time))
        (kept 0))
    (setq lispm-desktop-browser--script-callbacks
          (cl-remove-if-not
           (lambda (entry)
             (pcase-let ((`(,_ ,xwidget ,time) entry))
               (or (< (- now time) lispm-desktop-browser--script-callback-age)
                   (and (memq xwidget (bound-and-true-p xwidget-list))
                        (<= (cl-incf kept) lispm-desktop-browser--script-callbacks-kept)))))
           lispm-desktop-browser--script-callbacks))))

(defun lispm-desktop-browser--run-script (xwidget script &optional callback)
  "Run SCRIPT, JavaScript, in XWIDGET's page; call CALLBACK with its value.
Unlike `xwidget-webkit-execute-script' on macOS, this keeps CALLBACK
from the garbage collector until WebKit calls it.  On macOS WebKit
calls it only with a value, never for null or undefined."
  (if (null callback)
      (xwidget-webkit-execute-script xwidget script)
    (lispm-desktop-browser--prune-script-callbacks)
    (let* ((entry (list nil xwidget (float-time)))
           (answered (lambda (value)
                       (setq lispm-desktop-browser--script-callbacks
                             (delq entry lispm-desktop-browser--script-callbacks))
                       (funcall callback value))))
      (setcar entry answered)
      (push entry lispm-desktop-browser--script-callbacks)
      (xwidget-webkit-execute-script xwidget script answered))))


(provide 'lispm-desktop-browser)
;;; lispm-desktop-browser.el ends here
