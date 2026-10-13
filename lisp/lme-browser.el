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

;; The embedded WebKit browser: an application of its own, as Mac OS 9's
;; browsers were, with its own workspace, its own menus in the menu bar and
;; a browser window (title bar, toolbar, address field, tabs), and helpers
;; that open URLs in it.  Requires an Emacs built with xwidgets.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'thingatpt)
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

(defvar lme-xwidget-search-url "https://duckduckgo.com/?q=%s")

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
;; The embedded WebKit browser runs in the main area of a workspace of its
;; own, laid out as a Mac OS 9 browser window:
;;
;;   address window  title bar     the shown page's icon and title
;;                   header line   toolbar: back, forward, reload, home
;;                   text line     the address, in an edit field (RET)
;;   page window     tab line      a bevel button per tab, then "+"
;;                   below         the page
;;
;; The address window is a real buffer, so the address is edited in place.
;; A timer keeps it in step with the page while it is not being edited.
;; While the workspace is frontmost, the menu bar holds the browser's own
;; menus (File, Go, Window) instead of the sidebar's views, as Mac OS 9's
;; menu bar held the frontmost application's.

(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-webkit-uri "xwidget" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))
(declare-function xwidget-webkit-browse-url "xwidget" (url &optional new-session))
(declare-function xwidget-webkit-back "xwidget" ())
(declare-function xwidget-webkit-forward "xwidget" ())
(declare-function xwidget-webkit-reload "xwidget" ())
(defvar xwidget-webkit-mode-map)
(declare-function lme-sidebar--toolbar-button "lme-sidebar" (label command help row &optional face))
(declare-function lme-sidebar--toolbar-buttons "lme-sidebar" (actions row))
(declare-function lme-platinum-icon "lme-platinum" (name))
(defvar lme-platinum-title)
(defvar lme-platinum-icon)
(defvar lme-platinum-close-function)

(defconst lme-browser-workspace-name "Browser"
  "Name of the workspace holding the embedded browser.")

(defconst lme-browser-home-url "https://duckduckgo.com"
  "Page a new browser tab opens.")

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

(defun lme-browser--window ()
  "Return the main window showing a browser tab, if any."
  (seq-find (lambda (window)
              (eq (buffer-local-value 'major-mode (window-buffer window))
                  'xwidget-webkit-mode))
            (lme-main-area--windows)))

(defun lme-browser--address-window ()
  "Return the address window of this frame, if any."
  (get-buffer-window lme-browser-address-buffer-name))

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

(defun lme-browser-close-shown-tab ()
  "Close the shown browser tab, see `lme-browser-close-tab'."
  (interactive)
  (when-let* ((window (lme-browser--window)))
    (lme-browser-close-tab (window-buffer window))))

;;;;; Menus

(defun lme-browser--file-items ()
  "Return the items of the browser's File menu."
  (let ((page (and (lme-browser--window) t)))
    `((:label "New Tab" :action lme-browser-new-tab)
      (:label "Open Location…" :action lme-browser-focus-address)
      (:label "Close Tab" :action lme-browser-close-shown-tab :enabled ,page)
      separator
      (:label "Open in Default Browser" :action lme-browser-open-external :enabled ,page))))

(defun lme-browser--go-items ()
  "Return the items of the browser's Go menu."
  (let ((page (and (lme-browser--window) t)))
    `((:label "Back" :action lme-browser-back :enabled ,page)
      (:label "Forward" :action lme-browser-forward :enabled ,page)
      (:label "Reload" :action lme-browser-reload :enabled ,page)
      separator
      (:label "Home" :action lme-browser-home :enabled ,page))))

(defun lme-browser--window-items ()
  "Return the items of the browser's Window menu: a tab each, the shown one
checked, as Mac OS 9's Window menus listed an application's windows."
  (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
    (mapcar (lambda (buffer)
              (list :label (truncate-string-to-width (lme-browser--title buffer) 48 nil nil "…")
                    :action (lambda () (lme-browser-select-tab buffer))
                    :checked (eq buffer shown)))
            (lme-browser--buffers))))

(defconst lme-browser-menus
  '((lme-browser-file "File" lme-browser--file-items)
    (lme-browser-go "Go" lme-browser--go-items)
    (lme-browser-window "Window" lme-browser--window-items))
  "The browser's menus in the menu bar: (KEY TITLE ITEMS-FUNCTION).")

(defun lme-browser-application-menus ()
  "Return `lme-browser-menus' while the browser workspace is frontmost.
This is one of `lme-platinum-application-menus-functions'."
  (when (lme-browser-workspace-p)
    lme-browser-menus))

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

(defun lme-browser--render-address (uri)
  "Fill the address buffer with its label and the editable URI.
The URI sits in an edit field, see `lme-browser--draw-field'.  Filling is
not an edit: change hooks are inhibited and the edit mark cleared."
  (with-current-buffer (lme-browser--address-buffer)
    (setq lme-browser--address-edited nil)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (insert (propertize " " 'display `(space :width (8) :height (,lme-browser-address-height)
                                               :ascent 65))
              (propertize "Address:" 'face 'lme-platinum-toolbar-label)
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
    (select-window window)
    (goto-char (point-max))
    (push-mark lme-browser--address-start t t)))

(defvar-keymap lme-browser-address-mode-map
  :doc "Keymap of the browser address window."
  "RET" #'lme-browser-address-submit
  "C-l" #'lme-browser-focus-address)

(defun lme-browser--toolbar ()
  "Return the browser's toolbar, the address window's header line.
Bevel buttons go back, forward, reload and home; one at the right end
opens the page in the default browser."
  (let ((trailing (lme-sidebar--toolbar-buttons
                   '(("Open in Browser" lme-browser-open-external
                      "Open the page in the default browser"))
                   'header-line)))
    (concat " "
            (lme-sidebar--toolbar-buttons
             '(("◀ Back" lme-browser-back "Go back a page")
               ("Forward ▶" lme-browser-forward "Go forward a page")
               ("Reload" lme-browser-reload "Load the page again")
               ("Home" lme-browser-home "Go to the home page"))
             'header-line)
            (propertize " " 'display
                        `(space :align-to (- right (,(+ (string-pixel-width trailing) 6)))))
            trailing)))

(defun lme-browser--close-window (_window)
  "Close the browser window's shown tab, from its title bar's close box."
  (lme-browser-close-shown-tab))

(define-derived-mode lme-browser-address-mode fundamental-mode "Address"
  "Major mode of the browser's address window.
Its title bar names the shown page, its header line is the toolbar and
its one line the address.
\\{lme-browser-address-mode-map}"
  (setq-local mode-line-format nil
              header-line-format '(:eval (lme-browser--toolbar))
              truncate-lines t
              cursor-type 'bar
              lme-platinum-title lme-browser-workspace-name
              lme-platinum-icon (lme-platinum-icon "globe")
              lme-platinum-close-function #'lme-browser--close-window)
  (face-remap-add-relative 'default 'lme-platinum-toolbar)
  (face-remap-add-relative 'header-line 'lme-platinum-header)
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
         (when (eq buffer shown) 'lme-platinum-button-pressed)))
      (lme-browser--buffers) " ")
     " "
     (lme-sidebar--toolbar-button " + " #'lme-browser-new-tab "New tab" 'tab-line))))

(defun lme-browser-select-tab (buffer)
  "Show browser tab BUFFER in the browser window."
  (when-let* ((window (lme-browser--window)))
    (set-window-buffer window buffer)
    (lme-browser--sync t)))

(defun lme-browser-new-tab ()
  "Open `lme-browser-home-url' in a new browser tab and show it."
  (interactive)
  (with-selected-window (or (lme-browser--window) (lme-agent--main-window))
    (xwidget-webkit-browse-url lme-browser-home-url t))
  (lme-browser--sync t))

(defun lme-browser-close-tab (buffer)
  "Close browser tab BUFFER without asking, then show another tab.
Closing the last tab opens a new one, so the browser never goes blank."
  (let ((window (lme-browser--window))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)
    (if-let* ((next (car (lme-browser--buffers))))
        (when (window-live-p window) (set-window-buffer window next))
      (when (window-live-p window)
        (with-selected-window window
          (xwidget-webkit-browse-url lme-browser-home-url t)))))
  (lme-browser--sync t))

;;;;; Layout and synchronisation

(defvar lme-browser--sync-timer nil
  "Repeating timer keeping the address window in step with the page.")

(defvar lme-browser--synced-state nil
  "Address, tab titles, shown tab and its loading flag at the last sync.")

(defvar xwidget-webkit--loading-p)

(defun lme-browser--state ()
  "Return what the address window shows: the address, tab titles, shown tab.
The shown tab's loading flag is included, so a finished load redraws the
tab line, whose page icons are looked up once a page has loaded."
  (let ((shown (and (lme-browser--window) (window-buffer (lme-browser--window)))))
    (list (lme-browser--uri)
          (mapcar #'buffer-name (lme-browser--buffers))
          shown
          (and shown (buffer-local-value 'xwidget-webkit--loading-p shown)))))

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
          (setq lme-browser--synced-state state)
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
kept so."
  (when-let* ((address (lme-browser--address-window)))
    (let ((window-resize-pixelwise t))
      (fit-window-to-buffer address nil nil nil nil t))))

(defun lme-browser--ensure-layout ()
  "Put the address window above the browser window, once, and keep it synced.
The address window is fitted to its lines, see `lme-browser--fit-address'."
  (when-let* ((window (lme-browser--window)))
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

(defun lme-browser ()
  "Switch to the browser workspace, or back to the previous one when there.
The workspace shows the most recent browser tab in the main area under the
address window, opening `lme-browser-home-url' when there is no tab."
  (interactive)
  (if (lme-browser-workspace-p)
      (tab-bar-switch-to-recent-tab)
    (let ((fresh (lme-workspace-enter lme-browser-workspace-name))
          (browser (car (lme-browser--buffers))))
      (cond ((and browser fresh) (lme-workspace-show-alone browser))
            (browser (unless (get-buffer-window browser)
                       (select-window (lme-agent--main-window))
                       (pop-to-buffer-same-window browser)))
            (t (select-window (lme-agent--main-window))
               (xwidget-webkit-browse-url lme-browser-home-url t)
               (when fresh (tabspaces-reset-buffer-list))))
      (lme-browser--ensure-layout)))
  (force-mode-line-update t))

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
              header-line-format nil)
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
