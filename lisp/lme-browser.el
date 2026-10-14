;;; lme-browser.el --- The embedded browser  -*- lexical-binding: t; -*-

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

;; The embedded WebKit browser: an application on the desktop (see
;; lme-apps.el) with its own workspace and menus, a Mac OS 9 browser window
;; (title bar, toolbar, address field, tabs), and helpers that open URLs in
;; it.  Requires an Emacs built with xwidgets.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'lme-apps)
(require 'format-spec)
(require 'thingatpt)
(require 'url-parse)
(require 'url-util)

(defun lme-xwidget-available-p ()
  "Return non-nil when xwidget webkit browsing is available."
  (and (featurep 'xwidget-internal)
       (fboundp 'xwidget-webkit-browse-url)))

(defun lme-xwidget-normalize-url (url)
  "Normalize URL for xwidget browsing."
  (if (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" url)
      url
    (concat "https://" url)))

(defcustom lme-xwidget-search-url "https://duckduckgo.com/?q=%s"
  "Address searched for what is typed in the location field that is no
address, %s standing for the words searched."
  :type 'string
  :group 'lispm-emacs)

(defun lme-xwidget-browse-url (url)
  "Open URL using xwidget webkit when available."
  (interactive (list (read-string "URL: " (or (thing-at-point-url-at-point) ""))))
  (if (lme-xwidget-available-p)
      (xwidget-webkit-browse-url (lme-xwidget-normalize-url url))
    (browse-url (lme-xwidget-normalize-url url))))

(defun lme-xwidget-search (query)
  "Search QUERY using xwidget webkit when available."
  (interactive (list (read-string "Search: " (or (thing-at-point 'symbol t) ""))))
  (lme-xwidget-browse-url (format lme-xwidget-search-url (url-hexify-string query))))

(defun lme-xwidget-open-current-url ()
  "Open the URL at point using xwidget webkit when available."
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (unless url
      (user-error "No URL at point"))
    (lme-xwidget-browse-url url)))

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
;; History and bookmarks are lme-browser-places.el's.
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
(declare-function lme-sidebar--toolbar-button "lme-sidebar" (label command help row &optional face))
(declare-function lme-sidebar--toolbar-buttons "lme-sidebar" (actions row))
(declare-function lme-platinum-icon "lme-platinum" (name))
(defvar lme-platinum-title)
(defvar lme-platinum-icon)
(defvar lme-platinum-box-functions)

(defconst lme-browser-workspace-name "Browser"
  "Name of the workspace holding the embedded browser.")

(defcustom lme-browser-home-url "https://duckduckgo.com"
  "Page a new browser tab opens, and Home goes to."
  :type 'string
  :group 'lispm-emacs)

(defconst lme-browser-address-buffer-name " *Browser Address*"
  "Name of the address window's buffer; the leading space keeps it unlisted.")

(defconst lme-browser-tab-width 24
  "Maximum width in columns of a browser tab's title.")

(defvar lme-browser--tab-order nil
  "Browser tab buffers in the order they were opened.
Buffer names follow page titles, so they cannot order the tabs.")

(defun lme-browser--buffers ()
  "Return the live browser tabs, in the order they were opened."
  (setq lme-browser--tab-order (seq-filter #'buffer-live-p lme-browser--tab-order)))

(defun lme-browser-workspace-p ()
  "Return non-nil when the browser workspace is selected."
  (and (bound-and-true-p tabspaces-mode)
       (equal (tabspaces--current-tab-name) lme-browser-workspace-name)))

(defvar lme-browser--frame nil
  "The floating browser's child frame, or nil while the browser is full screen.")

(defvar lme-browser-floating nil
  "Non-nil when the browser opens floating over the desktop rather than full
screen.  Its zoom box, and the View menu, switch it.")

(defvar lme-browser--geometry nil
  "Where the floating browser was last: (LEFT TOP WIDTH HEIGHT) in pixels.")

(defvar lme-browser--shade nil
  "While the floating browser is rolled up: its windows and text height.")

(defvar lme-browser--navigation nil
  "What the shown tab can do, as the last sync found it:
\(:page P :back B :forward F :loading L).")

(defun lme-browser-floating-p ()
  "Return non-nil while the browser floats in a window of its own."
  (frame-live-p lme-browser--frame))

(defun lme-browser--window ()
  "Return the window showing a browser tab, if any: in the floating browser,
or else in the main area."
  (seq-find (lambda (window)
              (eq (buffer-local-value 'major-mode (window-buffer window))
                  'xwidget-webkit-mode))
            (if (lme-browser-floating-p)
                (window-list lme-browser--frame 'no-mini)
              (lme-main-area--windows))))

(defun lme-browser--address-window ()
  "Return the address window: the floating browser's, or this frame's."
  (get-buffer-window lme-browser-address-buffer-name
                     (and (lme-browser-floating-p) lme-browser--frame)))

(defun lme-browser--uri (&optional buffer)
  "Return the address of the page in browser BUFFER, by default the shown tab."
  (when-let* ((buffer (or buffer (and (lme-browser--window)
                                      (window-buffer (lme-browser--window)))))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min)))))
    (xwidget-webkit-uri xwidget)))

(defun lme-browser--title (buffer)
  "Return the page title of browser BUFFER, from its buffer name."
  (let ((name (string-trim (buffer-name buffer) "\\*" "\\*")))
    (string-trim (replace-regexp-in-string "\\`xwidget[- ]webkit: ?" "" name))))

(defun lme-browser--target (input)
  "Return the address to visit for address bar INPUT.
A local host (localhost or 127.0.0.1, with any port or path) is served over
http; input that names a scheme, or a dotted host without spaces, is an
address; anything else is a search."
  (cond ((string-match-p "\\`\\(localhost\\|127\\.0\\.0\\.1\\)\\([:/]\\|\\'\\)" input)
         (concat "http://" input))
        ((or (string-match-p "\\`[a-z][a-z0-9+.-]*://" input)
             (and (string-match-p "\\." input) (not (string-match-p " " input))))
         (lme-xwidget-normalize-url input))
        (t (format lme-xwidget-search-url (url-hexify-string input)))))

(defconst lme-browser--paste-script
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

(defun lme-browser--paste (text)
  "Insert TEXT verbatim at the caret of the shown page's focused field.
The text travels as a JSON string literal, so quotes and newlines in it
cannot break out of `lme-browser--paste-script'."
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 (format lme-browser--paste-script (json-encode text))))

(defun lme-browser-open-external ()
  "Open the shown browser tab's page in the system's default browser.
Sites needing browser extensions, such as MetaMask or 1Password, work
there; the embedded WebKit view has no extension support."
  (interactive)
  (if-let* ((uri (lme-browser--uri)))
      (browse-url-default-browser uri)
    (user-error "No page is shown")))

(defun lme-browser--in-page (command)
  "Run COMMAND, a WebKit navigation command, in the shown browser tab.
Navigating discards an unsubmitted edit of the address, as a desktop browser
does: the address window shows the page's real address again at once."
  (when-let* ((window (lme-browser--window)))
    (with-selected-window window
      (funcall command))
    (lme-browser--sync t)))

(defun lme-browser-back ()
  "Go back a page in the shown browser tab."
  (interactive)
  (lme-browser--in-page #'xwidget-webkit-back))

(defun lme-browser-forward ()
  "Go forward a page in the shown browser tab."
  (interactive)
  (lme-browser--in-page #'xwidget-webkit-forward))

(defun lme-browser-reload ()
  "Load the shown browser tab's page again."
  (interactive)
  (lme-browser--in-page #'xwidget-webkit-reload))

(defun lme-browser-home ()
  "Go to `lme-browser-home-url' in the shown browser tab."
  (interactive)
  (lme-browser--in-page
   (lambda () (xwidget-webkit-goto-uri (xwidget-at (point-min)) lme-browser-home-url))))

(defun lme-browser-stop ()
  "Stop loading the shown browser tab's page."
  (interactive)
  (lme-browser--in-page #'xwidget-webkit-stop))

(defun lme-browser-search ()
  "Select the location field for a search: what is typed there that is not
an address is searched for, see `lme-browser--target'."
  (interactive)
  (lme-browser-focus-address))

(defun lme-browser--shown-buffer ()
  "Return the shown browser tab's buffer, or nil."
  (when-let* ((window (lme-browser--window)))
    (window-buffer window)))

(defun lme-browser-open-url (url &optional new-tab)
  "Show the page at URL in the shown browser tab, or in a new tab when
NEW-TAB or when none is shown.  The browser comes in front."
  (let ((window (lme-browser--window)))
    (cond ((and window (not new-tab))
           (with-current-buffer (window-buffer window)
             (xwidget-webkit-goto-uri (xwidget-at (point-min)) url))
           (lme-browser--sync t))
          (window (lme-browser-new-tab url))
          (t (lme-browser-show)
             (lme-browser-open-url url)))
    (unless (eq (lme-apps-frontmost) 'browser)
      (lme-browser-show))))

(defun lme-browser-close-shown-tab ()
  "Close the shown browser tab, see `lme-browser-close-tab'."
  (interactive)
  (when-let* ((window (lme-browser--window)))
    (lme-browser-close-tab (window-buffer window))))

;;;;; Menus

(declare-function lme-browser-add-bookmark "lme-browser-places" ())
(declare-function lme-browser-edit-bookmarks "lme-browser-places" ())
(declare-function lme-browser-show-history "lme-browser-places" ())
(declare-function lme-browser--bookmark-items "lme-browser-places" ())
(declare-function lme-browser--places-shown "lme-browser-places" ())
(declare-function lme-browser--personal-toolbar "lme-browser-places" ())
(declare-function lme-popup-menu "lme-popup" (items &optional x y align))

(defun lme-browser--file-items ()
  "Return the items of the browser's File menu."
  (let ((page (and (lme-browser--window) t)))
    `((:label "New Tab" :action lme-browser-new-tab)
      (:label "Open Location…" :action lme-browser-focus-address
              :key ,(lme-browser--key 'lme-browser-focus-address))
      (:label "Close Tab" :action lme-browser-close-shown-tab :enabled ,page)
      separator
      (:label "Open in Default Browser" :action lme-browser-open-external :enabled ,page)
      separator
      (:label "Quit Browser" :action lme-browser-quit))))

(defun lme-browser--view-items ()
  "Return the items of the browser's View menu: full screen or floating,
then reloading and stopping, as Netscape's View menu had them."
  (let ((nav lme-browser--navigation))
    `((:label "Full Screen" :action lme-browser-full-screen
              :checked ,(not (lme-browser-floating-p)))
      (:label "Floating Window" :action lme-browser-float
              :checked ,(lme-browser-floating-p))
      separator
      (:label "Reload" :action lme-browser-reload :enabled ,(plist-get nav :page)
              :key ,(lme-browser--key 'lme-browser-reload))
      (:label "Stop Loading" :action lme-browser-stop :enabled ,(plist-get nav :loading)
              :key ,(lme-browser--key 'lme-browser-stop)))))

(defun lme-browser--session-items ()
  "Return the shown tab's pages back and forward as Go menu items, the one
shown checked, as Netscape's Go menu listed the window's pages.  Only
WebKitGTK tells them; on macOS there are none."
  (when-let* (((fboundp 'xwidget-webkit-back-forward-list))
              (buffer (lme-browser--shown-buffer))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min))))
              (pages (ignore-errors (xwidget-webkit-back-forward-list xwidget 8))))
    (pcase-let ((`(,back ,here ,forward) pages))
      (mapcar (pcase-lambda (`(,index ,title ,uri))
                (list :label (truncate-string-to-width
                              (if (string-empty-p (or title "")) (or uri "") title)
                              48 nil nil "…")
                      :action (lambda ()
                                (xwidget-webkit-goto-history xwidget index)
                                (lme-browser--sync t))
                      :checked (= index 0)))
              (append (reverse forward) (list here) back)))))

(defun lme-browser--go-items ()
  "Return the items of the browser's Go menu: Back, Forward and Home, then
the shown tab's pages."
  (let* ((nav lme-browser--navigation)
         (pages (lme-browser--session-items)))
    `((:label "Back" :action lme-browser-back :enabled ,(plist-get nav :back)
              :key ,(lme-browser--key 'lme-browser-back))
      (:label "Forward" :action lme-browser-forward :enabled ,(plist-get nav :forward)
              :key ,(lme-browser--key 'lme-browser-forward))
      (:label "Home" :action lme-browser-home :enabled ,(plist-get nav :page))
      ,@(when pages (cons 'separator pages)))))

(defun lme-browser--window-items ()
  "Return the items of the browser's Window menu: History, then a tab each,
the shown one checked, as Mac OS 9's Window menus listed an application's
windows."
  (let ((shown (lme-browser--shown-buffer)))
    `((:label "History" :action lme-browser-show-history
              :checked ,(eq (lme-browser--places-shown) 'history)
              :key ,(lme-browser--key 'lme-browser-show-history))
      separator
      ,@(mapcar (lambda (buffer)
                  (list :label (truncate-string-to-width (lme-browser--title buffer) 48 nil nil "…")
                        :action (lambda () (lme-browser-select-tab buffer))
                        :checked (eq buffer shown)))
                (lme-browser--buffers)))))

(defun lme-browser-menus ()
  "Return the browser's menus, for the menu bar while it is in front."
  '((lme-browser-file "File" lme-browser--file-items)
    (lme-browser-view "View" lme-browser--view-items)
    (lme-browser-go "Go" lme-browser--go-items)
    (lme-browser-bookmarks "Bookmarks" lme-browser--bookmark-items)
    (lme-browser-window "Window" lme-browser--window-items)))

;;;;; Drawing Netscape's chrome
;;
;; Each part is an SVG drawn once per palette and state, then kept.

(defvar lme-platinum--generation)
(declare-function lme-tab-bar--blend "lme-interface" (from to ratio))

(defvar lme-browser--images (make-hash-table :test #'equal)
  "Drawn chrome images, by palette generation and key.")

(defun lme-browser--image (key draw)
  "Return the image of KEY, calling DRAW for its SVG text the first time."
  (let ((key (cons lme-platinum--generation key)))
    (or (gethash key lme-browser--images)
        (progn
          (when (> (hash-table-count lme-browser--images) 256)
            (clrhash lme-browser--images))
          (puthash key (create-image (funcall draw) 'svg t :ascent 'center :scale 1)
                   lme-browser--images)))))

(defun lme-browser--svg (width height &rest parts)
  "Return an SVG document WIDTH by HEIGHT pixels holding PARTS."
  (concat (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                  width height width height)
          (apply #'concat parts)
          "</svg>"))

(defun lme-browser--escape (text)
  "Return TEXT safe inside SVG."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun lme-browser--font ()
  "Return the toolbar labels' font as (FAMILY SIZE), the size in pixels:
a size under the list font's, as Netscape's labels were small."
  (let* ((font (ignore-errors (face-font 'lme-platinum-list)))
         (info (and font (ignore-errors (font-info font))))
         (family (face-attribute 'lme-platinum-list :family nil t)))
    (list (if (stringp family) family "Geneva")
          (max 10 (- (if info (aref info 2) 12) 2)))))

(defun lme-browser--label-width (label)
  "Return the width in pixels of toolbar LABEL."
  (pcase-let ((`(,family ,size) (lme-browser--font)))
    (string-pixel-width (propertize label 'face `(:family ,family :height ,(* 10 size))))))

(defun lme-browser--ink (enabled)
  "Return the icons' colours as an alist for `format-spec': outline (i),
fill (f), face (c), the stop sign's red (r) and white (w); greyed unless
ENABLED."
  (let ((red (or (ignore-errors (face-foreground 'error nil t)) "#CC3333")))
    (if enabled
        `((?i . ,(lme-platinum--color :text)) (?f . ,(lme-platinum--color :accent))
          (?c . ,(lme-platinum--color :content)) (?r . ,red) (?w . "#FFFFFF"))
      `((?i . ,(lme-platinum--color :shade)) (?f . ,(lme-platinum--color :muted))
        (?c . ,(lme-platinum--color :chrome)) (?r . ,(lme-platinum--color :muted))
        (?w . ,(lme-platinum--color :chrome))))))

(defconst lme-browser--icons
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
`lme-browser--ink' for the colours named in them.")

(defconst lme-browser-tool-height 46
  "Height in pixels of a navigation toolbar button.")

(defun lme-browser--bevel (width height pressed)
  "Return SVG of a WIDTH by HEIGHT bevel, pushed in when PRESSED."
  (let ((light (lme-platinum--color :light))
        (shade (lme-platinum--color :shade))
        (chrome (lme-platinum--color :chrome)))
    (concat
     (format "<rect width='%d' height='%d' fill='%s'/>" width height
             (if pressed (lme-tab-bar--blend chrome shade 0.45) chrome))
     (format "<rect width='%d' height='1' fill='%s'/><rect width='1' height='%d' fill='%s'/>"
             width (if pressed shade light) height (if pressed shade light))
     (format "<rect y='%d' width='%d' height='1' fill='%s'/><rect x='%d' width='1' height='%d' fill='%s'/>"
             (1- height) width (if pressed light shade) (1- width) height (if pressed light shade)))))

(defun lme-browser--draw-tool (icon label enabled pressed width)
  "Return SVG of the toolbar button ICON named LABEL, WIDTH pixels wide:
greyed unless ENABLED, pushed in when PRESSED."
  (pcase-let ((`(,family ,size) (lme-browser--font))
              (ink (lme-browser--ink enabled))
              (height lme-browser-tool-height))
    (lme-browser--svg
     width height
     (lme-browser--bevel width height pressed)
     (format "<g transform='translate(%d %d)'>%s</g>" (/ (- width 24) 2) (if pressed 6 5)
             (format-spec (alist-get icon lme-browser--icons) ink))
     (format "<text x='%d' y='%d' font-family='%s' font-size='%d' text-anchor='middle' fill='%s'>%s</text>"
             (/ width 2) (- height 6 (if pressed -1 0)) (lme-browser--escape family) size
             (if enabled (lme-platinum--color :text) (lme-platinum--color :shade))
             (lme-browser--escape label)))))

(defconst lme-browser--logo
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

(defconst lme-browser-throbber-frames 10
  "Frames of the throbber's meteor shower, one every tick while loading.")

(defun lme-browser--meteor (x y length opacity)
  "Return SVG of a meteor at X, Y falling down to the left, its tail LENGTH
long, at OPACITY."
  (format "<g opacity='%.2f'><path d='M%.1f %.1f L%.1f %.1f L%.1f %.1f Z' fill='#FFFFFF' \
opacity='0.55'/><circle cx='%.1f' cy='%.1f' r='1.1' fill='#FFFFFF'/></g>"
          opacity x (- y 0.7) (+ x length) (- y (* 0.55 length)) x (+ y 0.7) x y))

(defun lme-browser--meteors (frame)
  "Return SVG of the meteors in FRAME of the shower, or of the still sky
when FRAME is nil."
  (if (null frame)
      (lme-browser--meteor 29.5 3.5 7 1)
    (mapconcat
     (lambda (start)
       (let ((step (mod (+ frame start) lme-browser-throbber-frames)))
         (lme-browser--meteor (- 40 (* 3.6 step)) (+ 1 (* 2.4 step)) 8
                              (- 1 (/ (float step) lme-browser-throbber-frames)))))
     '(0 4 7) "")))

(defun lme-browser--draw-throbber (frame pressed)
  "Return SVG of the throbber: the Netscape N in a bevel, FRAME of its
meteor shower falling (nil while the page rests), pushed in when PRESSED."
  (let ((size lme-browser-tool-height))
    (lme-browser--svg
     size size
     (lme-browser--bevel size size pressed)
     (format "<svg x='4' y='4' width='%d' height='%d' viewBox='0 0 40 40'>%s</svg>"
             (- size 8) (- size 8) (format lme-browser--logo (lme-browser--meteors frame))))))

(defun lme-browser--draw-padlock (secure)
  "Return SVG of the status bar's padlock: shut and in the accent colour
when SECURE, open and grey when not."
  (let ((ink (lme-platinum--color (if secure :text :shade)))
        (fill (lme-platinum--color (if secure :accent :muted))))
    (lme-browser--svg
     12 14
     (format "<path d='%s' fill='none' stroke='%s' stroke-width='1.6'/>"
             (if secure "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 7"
               "M3.5 7 L3.5 4.5 A2.5 2.5 0 0 1 8.5 4.5 L8.5 5")
             ink)
     (format "<rect x='1.5' y='6.5' width='9' height='7' fill='%s' stroke='%s'/>" fill ink))))

(defun lme-browser--draw-progress (fraction)
  "Return SVG of the status bar's progress bar, FRACTION full."
  (let ((filled (round (* (min 1 (max 0 fraction)) 78))))
    (lme-browser--svg
     80 10
     (format "<rect width='80' height='10' fill='%s'/>" (lme-platinum--color :frame))
     (format "<rect x='1' y='1' width='78' height='8' fill='%s'/>" (lme-platinum--color :content))
     (if (> filled 0)
         (format "<rect x='1' y='1' width='%d' height='8' fill='%s'/>" filled
                 (lme-platinum--color :accent))
       ""))))

;;;;; Navigation toolbar

(defvar lme-browser--throbber-frame nil
  "The throbber's frame while the shown page loads, or nil.")

(defvar lme-browser--throbber-timer nil
  "Timer turning the throbber while the shown page loads.")

(defconst lme-browser-tools
  '((back "Back" lme-browser-back "Go back a page" :back)
    (forward "Forward" lme-browser-forward "Go forward a page" :forward)
    (reload "Reload" lme-browser-reload "Load the page again" :page)
    (home "Home" lme-browser-home "Go to the home page" :page)
    (search "Search" lme-browser-search "Type a search in the location field" t)
    (history "History" lme-browser-show-history "List the pages visited" t)
    (stop "Stop" lme-browser-stop "Stop loading the page" :loading))
  "The navigation toolbar's buttons: (ICON LABEL COMMAND HELP ENABLED).
ENABLED is t, or a key of `lme-browser--navigation' saying when.")

(defun lme-browser--press-map (command row)
  "Return a keymap pressing a button on ROW that runs COMMAND, see
`lme-platinum-press'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map (format "<%s> <down-mouse-1>" row)
                (lambda (event)
                  (interactive "e")
                  (let ((start (event-start event)))
                    (lme-platinum-press start (lme-platinum--same-target-p start)
                                        (lambda ()
                                          (with-selected-window (posn-window start)
                                            (call-interactively command)))))))
    (keymap-set map (format "<%s> <mouse-1>" row) #'ignore)
    map))

(defun lme-browser--tool-button (tool)
  "Return navigation toolbar button TOOL, an entry of `lme-browser-tools'."
  (pcase-let* ((`(,icon ,label ,command ,help ,condition) tool)
               (enabled (or (eq condition t) (plist-get lme-browser--navigation condition)))
               (target (cons 'header-line icon))
               (pressed (and enabled (lme-platinum--pressed-p target)))
               (width (max 46 (+ 14 (lme-browser--label-width label)))))
    (propertize " " 'display (lme-browser--image
                              (list 'tool icon enabled pressed width)
                              (lambda () (lme-browser--draw-tool icon label enabled pressed width)))
                'help-echo help 'pointer 'arrow 'lme-platinum-target target
                'keymap (and enabled (lme-browser--press-map command 'header-line)))))

(defun lme-browser--throbber ()
  "Return the throbber, which goes home when clicked, as Netscape's N went
to Netscape's own page."
  (let* ((target '(header-line . throbber))
         (pressed (lme-platinum--pressed-p target))
         (frame lme-browser--throbber-frame))
    (propertize " " 'display (lme-browser--image
                              (list 'throbber frame pressed)
                              (lambda () (lme-browser--draw-throbber frame pressed)))
                'help-echo "Go to the home page" 'pointer 'arrow 'lme-platinum-target target
                'keymap (lme-browser--press-map #'lme-browser-home 'header-line))))

(defun lme-browser--toolbar ()
  "Return the navigation toolbar, the address window's header line: big
buttons, an icon over each name, and the throbber at the right end."
  (concat (lme-platinum--space 6)
          (mapconcat #'lme-browser--tool-button lme-browser-tools (lme-platinum--space 2))
          (propertize " " 'display `(space :align-to (- right (,(+ lme-browser-tool-height 10)))))
          (lme-browser--throbber)))

(defun lme-browser--turn-throbber ()
  "Turn the throbber one frame while the shown page loads; stop it, still,
once the page rests."
  (let ((loading (when-let* ((buffer (lme-browser--shown-buffer)))
                   (buffer-local-value 'xwidget-webkit--loading-p buffer))))
    (if loading
        (setq lme-browser--throbber-frame
              (mod (1+ (or lme-browser--throbber-frame -1)) lme-browser-throbber-frames))
      (setq lme-browser--throbber-frame nil)
      (when lme-browser--throbber-timer
        (cancel-timer lme-browser--throbber-timer)
        (setq lme-browser--throbber-timer nil)))
    (when-let* ((buffer (get-buffer lme-browser-address-buffer-name)))
      (with-current-buffer buffer
        (force-mode-line-update)))))

(defun lme-browser--navigation-state ()
  "Return what the shown tab can do, see `lme-browser--navigation'.
Where WebKit cannot tell the pages back and forward, as on macOS, Back and
Forward are open to any page."
  (let* ((buffer (lme-browser--shown-buffer))
         (xwidget (and buffer (with-current-buffer buffer (xwidget-at (point-min)))))
         (known (fboundp 'xwidget-webkit-back-forward-list))
         (pages (and xwidget known (ignore-errors (xwidget-webkit-back-forward-list xwidget 1)))))
    (list :page (and xwidget t)
          :back (and xwidget (or (not known) (car pages)) t)
          :forward (and xwidget (or (not known) (nth 2 pages)) t)
          :loading (and buffer (buffer-local-value 'xwidget-webkit--loading-p buffer) t))))

;;;;; Status bar

(defvar-local lme-browser-status-note nil
  "What the status bar of this page says instead of its state, or nil.
Following a link from the keyboard and typing in a page tell their keys
there.")

(defun lme-browser--status ()
  "Return the status bar of the page being drawn: the padlock, then the
progress bar and what is loading, or Netscape's \"Document: Done\", or
the page's `lme-browser-status-note'."
  (let* ((xwidget (xwidget-at (point-min)))
         (uri (and xwidget (xwidget-webkit-uri xwidget)))
         (secure (and uri (string-prefix-p "https:" uri)))
         (host (and uri (url-host (url-generic-parse-url uri)))))
    (concat (propertize " " 'display (lme-browser--image
                                      (list 'padlock secure)
                                      (lambda () (lme-browser--draw-padlock secure))))
            " "
            (if (and xwidget xwidget-webkit--loading-p)
                (let ((progress (or (ignore-errors (xwidget-webkit-estimated-load-progress xwidget))
                                    0)))
                  (concat (propertize " " 'display (lme-browser--image
                                                    (list 'progress (round (* 20 progress)))
                                                    (lambda () (lme-browser--draw-progress progress))))
                          " "
                          (if (and host (not (string-empty-p host)))
                              (format "Loading %s…" host)
                            "Loading…")))
              (or lme-browser-status-note "Document: Done")))))

;;;;; Address window

(defvar-local lme-browser--address-start nil
  "Marker at the start of the editable address in the address buffer.")

(defvar-local lme-browser--address-edited nil
  "Non-nil once the address was edited since the address buffer was filled.
An edited address is kept while the page stays put; see `lme-browser--sync'.")

(defvar-local lme-browser--field nil
  "Overlay drawing the address as an edit field.")

(defun lme-browser--note-address-edit (&rest _)
  "Record that the address was edited, from `after-change-functions'."
  (setq lme-browser--address-edited t))

(defconst lme-browser-address-height 30
  "Pixel height of the address line, the edit field and the toolbar around it.")

(defvar lme-browser-bookmarks-button-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lme-browser-bookmarks-menu)
    (keymap-set map "<mouse-1>" #'ignore)
    map)
  "Keymap of the location toolbar's Bookmarks button.")

(defun lme-browser-bookmarks-menu (&optional _event)
  "Pop up the Bookmarks menu, as the location toolbar's Bookmarks button did."
  (interactive (list last-nonmenu-event))
  (lme-popup-menu (lme-browser--bookmark-items)))

(defun lme-browser--bookmarks-button ()
  "Return the location toolbar's Bookmarks button: a bevel button holding
the bookmark ribbon, its name and a menu arrow."
  (let ((label (concat " "
                       (propertize " " 'display (lme-browser--image
                                                 '(ribbon)
                                                 (lambda ()
                                                   (lme-browser--svg
                                                    16 16
                                                    (format "<g transform='scale(0.66)'>%s</g>"
                                                            (format-spec (alist-get 'bookmarks lme-browser--icons)
                                                                         (lme-browser--ink t)))))))
                       " Bookmarks ▾ ")))
    (add-face-text-property 0 (length label) 'lme-platinum-button t label)
    (propertize label 'keymap lme-browser-bookmarks-button-map 'pointer 'arrow
                'mouse-face 'lme-platinum-button-pressed
                'help-echo "Bookmarks: add one, edit them, or go to one")))

(defun lme-browser--render-address (uri)
  "Fill the address buffer with the location toolbar and the editable URI:
the Bookmarks button, the page's icon and the Location label, as
Netscape's location toolbar had them.  The URI sits in an edit field, see
`lme-browser--draw-field'.  Filling is not an edit: change hooks are
inhibited and the edit mark cleared."
  (with-current-buffer (lme-browser--address-buffer)
    (setq lme-browser--address-edited nil)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (page (lme-browser--shown-buffer)))
      (erase-buffer)
      (insert (propertize " " 'display `(space :width (8) :height (,lme-browser-address-height)
                                               :ascent 65))
              (lme-browser--bookmarks-button)
              (lme-platinum--space 10)
              (or (and page (lme-browser--favicon page)) (lme-platinum-icon "globe"))
              " "
              (propertize "Location:" 'face 'lme-platinum-toolbar-label)
              " ")
      (add-text-properties (point-min) (point)
                           '(read-only t rear-nonsticky (read-only)))
      (setq lme-browser--address-start (copy-marker (point)))
      (insert (or uri ""))
      (lme-browser--draw-field)
      (when-let* ((window (get-buffer-window (current-buffer))))
        (set-window-point window (point-max))))))

(defun lme-browser--draw-field ()
  "Draw the address as Mac OS 9's edit text field: inset, in the list colour,
reaching across the toolbar to a few pixels short of its right edge.
The field grows as the address is typed at its end."
  (unless lme-browser--field
    (setq lme-browser--field (make-overlay 1 1 nil nil t))
    (overlay-put lme-browser--field 'face 'lme-platinum-field)
    (overlay-put lme-browser--field 'before-string
                 (propertize " " 'face 'lme-platinum-field 'display '(space :width (4))))
    ;; The cursor at the address's end stands on the field's blank rest.
    (overlay-put lme-browser--field 'after-string
                 (propertize " " 'face 'lme-platinum-field 'cursor t
                             'display '(space :align-to (- right (12))))))
  (move-overlay lme-browser--field lme-browser--address-start (point-max)))

(defun lme-browser--edited-address ()
  "Return the address typed in the address buffer, or nil before its first render."
  (with-current-buffer (lme-browser--address-buffer)
    (when lme-browser--address-start
      (string-trim (buffer-substring-no-properties lme-browser--address-start (point-max))))))

(defun lme-browser-address-submit ()
  "Visit the address or search typed in the address window."
  (interactive)
  (when-let* ((input (lme-browser--edited-address))
              ((not (string-empty-p input)))
              (window (lme-browser--window))
              (target (lme-browser--target input)))
    (with-current-buffer (window-buffer window)
      (xwidget-webkit-goto-uri (xwidget-at (point-min)) target))
    (lme-browser--render-address target)
    (select-window window)))

(defun lme-browser-focus-address ()
  "Select the address window with the whole address marked for replacement."
  (interactive)
  (when-let* ((window (lme-browser--address-window)))
    (unless (eq (window-frame window) (selected-frame))
      (select-frame-set-input-focus (window-frame window)))
    (select-window window)
    (goto-char (point-max))
    (push-mark lme-browser--address-start t t)))

(defconst lme-browser-keys
  '(("C-l" . lme-browser-focus-address)
    ("M-l" . lme-browser-focus-address)
    ("M-[" . lme-browser-back)
    ("M-]" . lme-browser-forward)
    ("M-r" . lme-browser-reload)
    ("M-." . lme-browser-stop)
    ("M-d" . lme-browser-add-bookmark)
    ("M-b" . lme-browser-edit-bookmarks)
    ("M-y" . lme-browser-show-history)
    ("M-t" . lme-browser-new-tab)
    ("M-}" . lme-browser-next-tab)
    ("M-{" . lme-browser-previous-tab))
  "The browser's keys, Command keys as Netscape's were where Command is
Meta: Command-[ and ] go back and forward, Command-D bookmarks the page,
Command-B edits the bookmarks.  History is Command-Y, as macOS keeps
Command-H for hiding.  Command-T opens a tab and Command-} and Command-{
go to the next and the previous, as browsers with tabs do; on the page,
Command-W closes the tab, see `lme-setup-interface'.")

(defun lme-browser-bind-keys (map)
  "Bind `lme-browser-keys' in keymap MAP."
  (pcase-dolist (`(,key . ,command) lme-browser-keys)
    (keymap-set map key command)))

(defun lme-browser--key (command)
  "Return the key of COMMAND among `lme-browser-keys', for a menu."
  (car (rassq command lme-browser-keys)))

(defvar lme-browser-address-mode-map
  (let ((map (make-sparse-keymap)))
    (lme-browser-bind-keys map)
    (keymap-set map "RET" #'lme-browser-address-submit)
    map)
  "Keymap of the browser address window.")

(defun lme-browser--close-window (_window)
  "Close the browser window's shown tab, from its title bar's close box."
  (lme-browser-close-shown-tab))

(defun lme-browser--zoom-box (_window)
  "Move the browser between full screen and floating, from its zoom box."
  (lme-browser-toggle-floating))

(defun lme-browser--collapse-box (_window)
  "Roll the floating browser up to its toolbar, or down again."
  (lme-browser-shade))

(defun lme-browser--boxes ()
  "Return the browser window's title bar boxes, see
`lme-platinum-box-functions': only a floating window rolls up."
  `(:close lme-browser--close-window :zoom lme-browser--zoom-box
    ,@(when (lme-browser-floating-p) '(:collapse lme-browser--collapse-box))))

(define-derived-mode lme-browser-address-mode fundamental-mode "Address"
  "Major mode of the browser's address window.
Its title bar names the shown page, its header line is the navigation
toolbar, its one line the location toolbar and its mode line the personal
toolbar.
\\{lme-browser-address-mode-map}"
  (setq-local mode-line-format '(:eval (lme-browser--personal-toolbar))
              header-line-format '(:eval (lme-browser--toolbar))
              truncate-lines t
              cursor-type 'bar
              lme-platinum-title lme-browser-workspace-name
              lme-platinum-icon (lme-platinum-icon "netscape")
              lme-platinum-box-functions (lme-browser--boxes))
  ;; Its height is the toolbars': a frame growing, as on a tall screen,
  ;; gives its height to the page, the toolbars fitted again at once.
  (add-hook 'window-size-change-functions #'lme-browser--refit-address nil t)
  (face-remap-add-relative 'default 'lme-platinum-toolbar)
  (face-remap-add-relative 'header-line 'lme-platinum-header)
  ;; The personal toolbar is a toolbar, lit or not.
  (face-remap-add-relative 'mode-line 'lme-platinum-toolbar)
  (face-remap-add-relative 'mode-line-inactive 'lme-platinum-toolbar)
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (add-hook 'after-change-functions #'lme-browser--note-address-edit nil t))

(defun lme-browser--address-buffer ()
  "Return the address buffer, creating it in `lme-browser-address-mode'."
  (or (get-buffer lme-browser-address-buffer-name)
      (with-current-buffer (get-buffer-create lme-browser-address-buffer-name)
        (lme-browser-address-mode)
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

(defconst lme-browser--favicon-script
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

(defvar lme-browser--favicons (make-hash-table :test #'equal)
  "Icon per icon address: an image, `pending' while fetched, `none' if unusable.")

(defvar-local lme-browser--favicon-source nil
  "(PAGE . ICON): the page address this tab's icon was looked up for, and
the icon's address, `pending' while the page is asked, or nil for none.")

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun lme-browser--favicon-image (data)
  "Return icon DATA as an image a line high, or nil when it is no image.
Formats Emacs recognizes are accepted, and ICO by its magic bytes: the
macOS image backend decodes ICO when asked for PNG.  Anything else would
draw as an empty box, so it is refused."
  (when-let* ((type (or (image-type-from-data data)
                        (and (string-prefix-p "\0\0\1\0" data) 'png))))
    (create-image data type t
                  :height (round (* 0.85 (frame-char-height)))
                  :ascent 'center)))

(defun lme-browser--data-url-bytes (url)
  "Return the bytes a data URL holds, or nil when URL is malformed."
  (when (string-match "\\`data:[^,]*?\\(;base64\\)?," url)
    (let ((base64 (match-beginning 1))
          (payload (substring url (match-end 0))))
      (if base64
          (ignore-errors (base64-decode-string payload))
        (encode-coding-string (url-unhex-string payload) 'utf-8)))))

(defun lme-browser--retitle ()
  "Name the browser window after the shown page, with the page's icon.
Each tab's window bottom is named after its page too, not after the
buffer, whose name WebKit prefixes."
  (dolist (buffer (lme-browser--buffers))
    (with-current-buffer buffer
      (setq lme-platinum-title (lme-browser--title buffer))))
  (when-let* ((buffer (get-buffer lme-browser-address-buffer-name)))
    (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
      (with-current-buffer buffer
        (setq lme-platinum-title (if shown (lme-browser--title shown)
                                   lme-browser-workspace-name)
              lme-platinum-icon (or (and shown (lme-browser--favicon shown))
                                    (lme-platinum-icon "globe")))))))

(defun lme-browser--redraw-tabs ()
  "Redraw the browser window's title bar and tabs, leaving the address alone."
  (lme-browser--retitle)
  (force-mode-line-update t))

(defun lme-browser--store-favicon (icon data)
  "Record icon address ICON as fetched with DATA, nil on failure, and redraw."
  (puthash icon (or (and data (lme-browser--favicon-image data)) 'none)
           lme-browser--favicons)
  (lme-browser--redraw-tabs))

(defun lme-browser--fetch-favicon (icon)
  "Fetch the icon at address ICON, once, and store it as it arrives.
A data URL is decoded in place; others are fetched without cookies, and
only a successful response's body is kept."
  (puthash icon 'pending lme-browser--favicons)
  (if (string-prefix-p "data:" icon)
      (lme-browser--store-favicon icon (lme-browser--data-url-bytes icon))
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
         (lme-browser--store-favicon icon data)))
     nil t t)))

(defun lme-browser--look-up-favicon (buffer page)
  "Ask browser BUFFER's PAGE for its icon address, then fetch the icon.
Only web and data addresses are followed.  An answer arriving after the
tab moved to another page is dropped.  An answer from another document
than PAGE's, the one being replaced while PAGE loads, is no answer: the
lookup is forgotten and runs again at the next redraw, which the finished
load brings."
  (with-current-buffer buffer
    (setq lme-browser--favicon-source (cons page 'pending))
    (when-let* ((xwidget (xwidget-at (point-min))))
      (xwidget-webkit-execute-script
       xwidget lme-browser--favicon-script
       (lambda (answer)
         (when (and (buffer-live-p buffer)
                    (equal (car (buffer-local-value 'lme-browser--favicon-source buffer))
                           page))
           (pcase-let* ((`(,document ,icon) (and (stringp answer)
                                                 (split-string answer "\n")))
                        (icon (and icon
                                   (string-match-p "\\`\\(?:https?\\|data\\):" icon)
                                   icon)))
             (with-current-buffer buffer
               (setq lme-browser--favicon-source
                     (and (equal document page) (cons page icon))))
             (when (and icon (equal document page)
                        (not (gethash icon lme-browser--favicons)))
               (lme-browser--fetch-favicon icon))
             (lme-browser--redraw-tabs))))))))

(defun lme-browser--favicon (buffer)
  "Return browser BUFFER's page icon as a string showing it, or nil.
Nil stands for an icon not known yet or absent.  A loaded page whose icon
was not looked up yet has its lookup started."
  (when-let* ((page (lme-browser--uri buffer)))
    (let ((source (buffer-local-value 'lme-browser--favicon-source buffer)))
      (cond ((buffer-local-value 'xwidget-webkit--loading-p buffer) nil)
            ((not (equal (car source) page))
             (lme-browser--look-up-favicon buffer page)
             nil)
            ((stringp (cdr source))
             (let ((image (gethash (cdr source) lme-browser--favicons)))
               (when (consp image)
                 (propertize " " 'display image))))))))

;;;;; Tabs

(defun lme-browser--tab-command (function &rest args)
  "Return an interactive command applying FUNCTION to ARGS."
  (lambda () (interactive) (apply function args)))

(defun lme-browser--tab-line ()
  "Return the row of tabs atop the page: a bevel button per tab, then +.
The shown tab's button is pushed in, as a Mac OS 9 group of bevel buttons
showed its choice.  Each shows its page's icon before its title, or the
globe until the icon is known and when the page has none."
  (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
    (concat
     (propertize " " 'display '(space :width (6) :height (28) :ascent 65))
     (mapconcat
      (lambda (buffer)
        (lme-sidebar--toolbar-button
         (concat " " (or (lme-browser--favicon buffer) (lme-platinum-icon "globe")) " "
                 (truncate-string-to-width (lme-browser--title buffer)
                                           lme-browser-tab-width nil nil "…")
                 " ")
         (lme-browser--tab-command #'lme-browser-select-tab buffer)
         (or (lme-browser--uri buffer) "Show this tab")
         'tab-line
         (if (eq buffer shown) 'lme-platinum-tab-selected 'lme-platinum-tab)))
      (lme-browser--buffers) " ")
     " "
     (lme-sidebar--toolbar-button " + " #'lme-browser-new-tab "New tab" 'tab-line))))

(defun lme-browser-select-tab (buffer)
  "Show browser tab BUFFER in the browser window."
  (when-let* ((window (lme-browser--window)))
    (set-window-buffer window buffer)
    (lme-browser--sync t)))

(defun lme-browser--step-tab (step)
  "Show the browser tab STEP places from the one shown, wrapping around."
  (let* ((tabs (lme-browser--buffers))
         (shown (and (lme-browser--window) (window-buffer (lme-browser--window))))
         (index (or (seq-position tabs shown) 0)))
    (when (cdr tabs)
      (lme-browser-select-tab (nth (mod (+ index step) (length tabs)) tabs)))))

(defun lme-browser-next-tab ()
  "Show the browser's next tab."
  (interactive)
  (lme-browser--step-tab 1))

(defun lme-browser-previous-tab ()
  "Show the browser's previous tab."
  (interactive)
  (lme-browser--step-tab -1))

(defun lme-browser-new-tab (&optional url)
  "Open URL, by default `lme-browser-home-url', in a new browser tab and
show it."
  (interactive)
  (with-selected-window (or (lme-browser--window) (lme-agent--main-window))
    (xwidget-webkit-browse-url (or url lme-browser-home-url) t))
  (lme-browser--sync t))

(declare-function lme-desktop-show "lme-desktop" (&optional window))
(declare-function lme-desktop-empty-buffer "lme-desktop" ())
(declare-function lme-desktop--new-tab-empty "lme-desktop" ())
(declare-function lme-desktop--select-tab "lme-desktop" (&optional frame))
(declare-function lme-editor-hide "lme-editor" ())
(declare-function lme-editor-floating-p "lme-editor" ())
(defvar lme-desktop-activate-hook)
(defvar lme-editor--frame)

(defun lme-browser-close-tab (buffer)
  "Close browser tab BUFFER without asking, then show another tab.
Closing the last tab quits the browser, see `lme-browser-quit'."
  (let ((window (lme-browser--window))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)
    (if-let* ((next (car (lme-browser--buffers))))
        (when (window-live-p window) (set-window-buffer window next))
      (when-let* ((address (lme-browser--address-window)))
        (delete-window address))
      ;; Nothing is left open: the browser quits, and the desktop comes in
      ;; front with its icons.
      (lme-browser-quit)))
  (lme-browser--sync t))

(declare-function lme-desktop-activate "lme-desktop" ())

(defun lme-browser-quit ()
  "Quit the browser: close its tabs, its window and its workspace, and bring
the desktop in front.  It opens again full screen or floating, as it was."
  (interactive)
  (lme-browser--delete-frame)
  (let ((kill-buffer-query-functions nil))
    (mapc #'kill-buffer (lme-browser--buffers))
    (when-let* ((address (get-buffer lme-browser-address-buffer-name)))
      (kill-buffer address)))
  (lme-desktop-activate)
  (lme-browser--close-workspace))

;;;;; Layout and synchronisation

(defvar lme-browser--sync-timer nil
  "Repeating timer keeping the address window in step with the page.")

(defvar lme-browser--synced-state nil
  "Address, tab titles, shown tab and what it can do at the last sync.")


(defun lme-browser--state ()
  "Return what the address window shows: the address, tab titles, shown tab.
What the shown tab can do is included, see `lme-browser--navigation': a
finished load redraws the tab line, whose page icons are looked up once a
page has loaded, and the toolbar, whose buttons it enables."
  (let ((shown (lme-browser--shown-buffer)))
    (list (lme-browser--uri)
          (mapcar #'buffer-name (lme-browser--buffers))
          shown
          (lme-browser--navigation-state))))

(defun lme-browser--sync (&optional force)
  "Show the current page's address and tabs in the address window.
Nothing is redrawn unless the state of `lme-browser--state' changed, or FORCE:
the page is never redisplayed by an idle tick, which could drop a click in
progress.  The address follows the page unless it was edited: an edited
address is kept while the page stays put, and while it is being typed even
when the page moves; FORCE, as navigation and switching tabs pass, always
shows the page's address.  The timer stops once no address window is left."
  (if-let* ((address (lme-browser--address-window)))
      (let* ((state (lme-browser--state))
             (moved (not (equal (car state) (car lme-browser--synced-state))))
             (edited (buffer-local-value 'lme-browser--address-edited
                                         (window-buffer address))))
        (when (or force (not (equal state lme-browser--synced-state)))
          (setq lme-browser--synced-state state
                lme-browser--navigation (nth 3 state))
          (when (and (plist-get lme-browser--navigation :loading)
                     (not lme-browser--throbber-timer))
            (setq lme-browser--throbber-timer
                  (run-with-timer 0 0.12 #'lme-browser--turn-throbber)))
          (lme-browser--redraw-tabs)
          (when (or force (not edited)
                    (and moved (not (eq address (selected-window)))))
            (lme-browser--render-address (car state)))
          (with-current-buffer (window-buffer address)
            (force-mode-line-update))))
    (when lme-browser--sync-timer
      (cancel-timer lme-browser--sync-timer)
      (setq lme-browser--sync-timer nil))))

(defun lme-browser--fit-address ()
  "Fit the address window to its title bar, toolbar and address line.
They are taller than text lines, so the window is fitted in pixels, then
kept so: a frame growing, as on a tall screen, gives its height to the
page, never to the toolbars."
  (when-let* ((address (lme-browser--address-window)))
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer address nil nil nil nil t))))

(defun lme-browser--refit-address (window)
  "Fit the address WINDOW to its toolbars again once its size changed, as
a frame growing or shrinking changes it, unless it is alone in its frame,
as in a rolled-up browser.  This is a buffer-local
`window-size-change-functions' function."
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lme-browser--ensure-layout ()
  "Put the address window above the browser window, once, and keep it synced.
The address window is fitted to its lines, see `lme-browser--fit-address'."
  (when-let* ((window (lme-browser--window)))
    (with-current-buffer (lme-browser--address-buffer)
      (setq lme-platinum-box-functions (lme-browser--boxes)))
    (unless (lme-browser--address-window)
      (let* ((window-min-height 1)
             (address (split-window window -2 'above)))
        (set-window-buffer address (lme-browser--address-buffer))
        (set-window-parameter address 'lme-chrome t)
        (set-window-dedicated-p address t)))
    ;; The address window wears the browser window's title bar, active
    ;; while the page is.
    (set-window-parameter (lme-browser--address-window) 'lme-platinum-title-for window)
    (lme-browser--sync t)
    ;; A new window's lines are measured right only once it was drawn, so
    ;; it is fitted again after the next redisplay.
    (lme-browser--fit-address)
    (run-at-time 0 nil #'lme-browser--fit-address)
    (unless lme-browser--sync-timer
      (setq lme-browser--sync-timer (run-with-timer 0.7 0.7 #'lme-browser--sync)))))

(defun lme-browser-show ()
  "Show the browser, full screen or floating as it was last, with the most
recent tab, opening `lme-browser-home-url' when there is no tab.  The
desktop's Browser icon runs this, and the browser comes in front."
  (interactive)
  (if lme-browser-floating
      (lme-browser--show-floating)
    (lme-browser--show-full-screen))
  (force-mode-line-update t))

(defun lme-browser--show-full-screen (&optional shown)
  "Show the browser in its workspace, SHOWN or its first tab in the main area
under the address window.  The floating editor, whose window would cover
the page, is hidden.  The workspace starts on the desktop's bare colour,
not the editor's start page, which a document there would take for the
editor's own; it is shown in place, see `lme-desktop-show-in-new-tab', so
no session of the workspace left comes along."
  (when (fboundp 'lme-editor-hide)
    (lme-editor-hide))
  (lme-apps-set-frontmost 'browser)
  (let* ((fresh (let ((tab-bar-new-tab-choice #'lme-desktop--new-tab-empty))
                  (lme-workspace-enter lme-browser-workspace-name)))
         (browser (or shown (car (lme-browser--buffers)))))
    (cond ((and browser fresh) (lme-workspace-show-alone browser))
          (browser (unless (get-buffer-window browser)
                     (select-window (lme-agent--main-window))
                     (pop-to-buffer-same-window browser)))
          (t (select-window (lme-agent--main-window))
             (xwidget-webkit-browse-url lme-browser-home-url t)
             (when fresh (tabspaces-reset-buffer-list))))
    (lme-browser--ensure-layout)))

(defun lme-browser--default-geometry (screen)
  "Return where the browser floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (tab-bar-height screen t) 30))
         (left (round (* width 0.1))))
    (list left top (- width (* 2 left)) (- height top 50))))

(defun lme-browser--make-frame (screen)
  "Return the floating browser's child frame of SCREEN, not yet shown."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or lme-browser--geometry (lme-browser--default-geometry screen)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (with-current-buffer (lme-desktop-empty-buffer)
                          (make-frame
                           `((parent-frame . ,screen)
                             (minibuffer . ,(minibuffer-window screen))
                             (name . "Browser") (lme-browser . t) (lme-app . browser)
                             (lme-window . t) (top-visible . 30)
                             (undecorated . t) (visibility . nil)
                             (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                             (menu-bar-lines . 0) (tool-bar-lines . 0)
                             (internal-border-width . 2) (child-frame-border-width . 1)
                             (drag-internal-border . t)
                             (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                             (user-position . t)
                             (left . ,left) (top . ,top)
                             (width . (text-pixels . ,width))
                             (height . (text-pixels . ,height))))))))
    (set-face-attribute 'internal-border frame :background (lme-platinum--color :chrome))
    frame))

(defun lme-browser--show-floating (&optional shown)
  "Show the browser floating over the desktop, SHOWN or its first tab in its
window, the window made when missing.  The floating windows left over the
desktop show with it."
  (let ((screen (lme-apps-screen))
        (browser (or shown (car (lme-browser--buffers)))))
    (with-selected-frame screen
      (lme-desktop--select-tab))
    (run-hooks 'lme-desktop-activate-hook)
    (unless (lme-browser-floating-p)
      (setq lme-browser--frame (lme-browser--make-frame screen)))
    (with-selected-frame lme-browser--frame
      (cond ((lme-browser--window)
             (when shown (set-window-buffer (lme-browser--window) shown)))
            (browser (set-window-buffer (frame-first-window) browser))
            (t (select-window (frame-first-window))
               (xwidget-webkit-browse-url lme-browser-home-url t)))
      (lme-browser--ensure-layout))
    (make-frame-visible lme-browser--frame)
    (lme-apps-keep-on-desktop lme-browser--frame)
    (select-frame-set-input-focus lme-browser--frame)
    (select-window (or (lme-browser--window) (frame-first-window lme-browser--frame)))
    (lme-apps-set-frontmost 'browser)))

(defun lme-browser--delete-frame ()
  "Delete the floating browser's window, remembering where it was."
  (when (lme-browser-floating-p)
    (let ((frame lme-browser--frame))
      (lme-browser--unshade)
      (setq lme-browser--geometry (lme-apps--geometry frame)
            lme-browser--frame nil)
      (when (eq (selected-frame) frame)
        (select-frame-set-input-focus (frame-parent frame)))
      (delete-frame frame))))

(defun lme-browser--close-workspace ()
  "Close the full-screen browser's workspace, the desktop's in front instead."
  (when-let* ((screen (lme-apps-screen))
              ((with-selected-frame screen
                 (tab-bar--tab-index-by-name lme-browser-workspace-name))))
    (with-selected-frame screen
      (when (lme-browser-workspace-p)
        (lme-desktop--select-tab))
      (tab-bar-close-tab (1+ (tab-bar--tab-index-by-name lme-browser-workspace-name))))))

(defun lme-browser-float ()
  "Float the browser in a window of its own over the desktop."
  (interactive)
  (setq lme-browser-floating t)
  (unless (lme-browser-floating-p)
    (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
      (lme-browser--close-workspace)
      (lme-browser--show-floating shown))))

(defun lme-browser-full-screen ()
  "Bring the floating browser back to full screen, under the menu bar."
  (interactive)
  (setq lme-browser-floating nil)
  (when (lme-browser-floating-p)
    (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
      (lme-browser--delete-frame)
      (lme-browser--show-full-screen shown))))

(defun lme-browser-toggle-floating ()
  "Move the browser between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lme-browser-floating-p) (lme-browser-full-screen) (lme-browser-float)))

(defun lme-browser--unshade ()
  "Roll the floating browser down again, when it is rolled up."
  (when lme-browser--shade
    (pcase-let ((`(,config . ,height) lme-browser--shade))
      (setq lme-browser--shade nil)
      (when (lme-browser-floating-p)
        (set-frame-height lme-browser--frame height nil t)
        (with-selected-frame lme-browser--frame
          (set-window-configuration config))))))

(defun lme-browser-shade ()
  "Roll the floating browser up to its title bar and toolbar, or down
again, as Mac OS 9 rolled a window up into its title bar."
  (interactive)
  (when (lme-browser-floating-p)
    (if lme-browser--shade
        (lme-browser--unshade)
      (when-let* ((address (lme-browser--address-window)))
        (with-selected-frame lme-browser--frame
          (let ((height (window-pixel-height address))
                (ignore-window-parameters t))
            (setq lme-browser--shade (cons (current-window-configuration)
                                           (frame-text-height lme-browser--frame)))
            (delete-other-windows address)
            (set-frame-height lme-browser--frame height nil t)))))))

(defun lme-browser--follow-frontmost (app)
  "Show the floating browser with the desktop and alongside the floating
editor; hide it from the full-screen editor, whose workspace it would
cover.  This is one of `lme-apps-frontmost-functions'."
  (when (lme-browser-floating-p)
    (let ((shown (or (memq app '(desktop browser settings))
                     (and (eq app 'editor) (fboundp 'lme-editor-floating-p)
                          (lme-editor-floating-p)))))
      (cond ((and shown (not (frame-visible-p lme-browser--frame)))
             (make-frame-visible lme-browser--frame)
             ;; The editor in front stays over it.
             (when (and (eq app 'editor) (frame-live-p (bound-and-true-p lme-editor--frame)))
               (raise-frame lme-editor--frame)))
            ((and (not shown) (frame-visible-p lme-browser--frame))
             (make-frame-invisible lme-browser--frame))))))

(defun lme-browser--refocus ()
  "Give the floating browser the keyboard back after a menu bar menu closes.
A click on the menu bar focuses the frame under it; the browser stays in
front, so it takes the focus again."
  (when (and (eq (lme-apps-frontmost) 'browser) (lme-browser-floating-p)
             (frame-visible-p lme-browser--frame)
             (not (eq (selected-frame) lme-browser--frame)))
    (select-frame-set-input-focus lme-browser--frame)))

(defun lme-browser ()
  "Switch to the browser, or back to the previous workspace when its
full-screen workspace is in front; see `lme-browser-show'."
  (interactive)
  (if (lme-browser-workspace-p)
      (progn (tab-bar-switch-to-recent-tab)
             (force-mode-line-update t))
    (lme-browser-show)))

(defun lme-browser--arrow-pointer (buffer)
  "Show the arrow mouse pointer over the page in browser BUFFER.
Emacs picks the pointer over a window itself, and the page is a character
displaying the WebKit view, so it would otherwise get the text I-beam."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local void-text-area-pointer 'arrow)
      (with-silent-modifications
        (let ((inhibit-read-only t))
          (put-text-property (point-min) (point-max) 'pointer 'arrow))))))

(defun lme-browser--setup ()
  "Set up a new browser tab's buffer and append it to the tab order.
Its tabs live in the address window, which stands at the top of the
browser as its title bar, so the page shows neither a title bar of its own
nor WebKit's title line.  The page is inserted after this mode hook runs,
so the arrow pointer is applied once the current command finishes."
  ;; The browser window's title bar is the address window's, above the
  ;; toolbar; the page's own top line holds the tabs, and WebKit's title
  ;; line would only repeat the title.
  (setq-local tab-line-format '(:eval (lme-browser--tab-line))
              header-line-format nil
              ;; The bottom edge is Netscape's status bar.
              lme-platinum-info #'lme-browser--status)
  ;; The title bar above lights and dims with the page.
  (add-hook 'window-selection-change-functions
            (lambda (_) (force-mode-line-update t)) nil t)
  ;; The unified copy and paste keys act on the page's selection and its
  ;; focused field, not on the buffer, which holds only the view.
  (setq-local lme-copy-function #'xwidget-webkit-copy-selection-as-kill
              lme-paste-function #'lme-browser--paste)
  (run-at-time 0 nil #'lme-browser--arrow-pointer (current-buffer))
  (display-line-numbers-mode -1)
  (setq lme-browser--tab-order (append lme-browser--tab-order (list (current-buffer)))))

(defun lme-browser--on-workspace-select (&rest _)
  "Resume address synchronisation when the browser workspace is selected."
  (when (lme-browser-workspace-p)
    (lme-browser--ensure-layout)))


(provide 'lme-browser)
;;; lme-browser.el ends here
