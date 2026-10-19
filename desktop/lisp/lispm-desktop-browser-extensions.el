;;; lispm-desktop-browser-extensions.el --- Chrome extensions in the browser  -*- lexical-binding: t; -*-

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

;; Chrome extensions in the browser's pages, on macOS 15.4 and later.
;; WebKit runs them itself (WKWebExtension, the engine Safari's extensions
;; run on) in every page view whose configuration carries its controller.
;; Emacs makes its page views itself, so the module lispm-desktop-webext
;; (Swift, in native/lispm-desktop-webext) hands each one the controller as
;; it is made, and hosts the extensions: the browser's pages are their tabs,
;; their popups hang from the page in front, and their questions are asked
;; here.  Most of the module is ported from Search, a WebKit browser for
;; macOS, with the script Search puts in every extension to fill in the
;; Chrome APIs WebKit lacks, so that heavy ones such as the Rabby wallet run;
;; a window an extension opens of its own is a Mac window.
;;
;;   Chrome Web Store link --install--> fetched, signature checked, asked, loaded
;;   M-e, an extension's button   --->  its popup, or its click
;;   Extensions menu, M-x lispm-desktop-browser-extensions: the list of them
;;
;; The module is built with Swift (`lispm-desktop-browser-extensions-build') the
;; first time an extension is added, and again at a start that finds its
;; sources changed.  Pages made before it is loaded run no extensions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)

(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-buffer "xwidget.c" (xwidget))
(declare-function lispm-desktop-browser--window "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser--shown-buffer "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser--uri "lispm-desktop-browser" (&optional buffer))
(declare-function lispm-desktop-browser-show "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-new-tab "lispm-desktop-browser" (&optional url))
(declare-function lispm-desktop-browser-select-tab "lispm-desktop-browser" (buffer))
(declare-function lispm-desktop-browser-close-tab "lispm-desktop-browser" (buffer))
(declare-function lispm-desktop-webext-module-start "lispm-desktop-webext-module" (directory))
(declare-function lispm-desktop-webext-module-stop "lispm-desktop-webext-module" ())
(declare-function lispm-desktop-webext-module-list "lispm-desktop-webext-module" ())
(declare-function lispm-desktop-webext-module-install-store "lispm-desktop-webext-module" (text))
(declare-function lispm-desktop-webext-module-install-folder "lispm-desktop-webext-module" (path))
(declare-function lispm-desktop-webext-module-reload "lispm-desktop-webext-module" (id))
(declare-function lispm-desktop-webext-module-remove "lispm-desktop-webext-module" (id))
(declare-function lispm-desktop-webext-module-set-enabled "lispm-desktop-webext-module" (id enabled))
(declare-function lispm-desktop-webext-module-action "lispm-desktop-webext-module" (id))
(declare-function lispm-desktop-webext-module-options-url "lispm-desktop-webext-module" (id))
(declare-function lispm-desktop-webext-module-expect-page "lispm-desktop-webext-module" (url))
(declare-function lispm-desktop-webext-module-activate "lispm-desktop-webext-module" (address))
(declare-function lispm-desktop-webext-module-answer "lispm-desktop-webext-module" (id value))
(defvar xwidget-list)

(defgroup lispm-desktop-browser-extensions nil
  "Chrome extensions in the browser's pages."
  :group 'lispm-desktop)

(defcustom lispm-desktop-browser-extensions-enabled t
  "Whether the browser's pages run Chrome extensions where Emacs can: on
macOS 15.4 and later, in an Emacs built with xwidgets.  Takes effect at
the next start."
  :type 'boolean
  :group 'lispm-desktop-browser-extensions)

(defcustom lispm-desktop-browser-extensions-directory
  (locate-user-emacs-file "lispm-desktop-browser-extensions/")
  "Directory the extensions are kept in, each unpacked in a folder named
by its id, beside the list of them, installed.json."
  :type 'directory
  :group 'lispm-desktop-browser-extensions)

(defconst lispm-desktop-browser-extensions-source-directory
  (expand-file-name "native/lispm-desktop-webext"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Directory of the module's Swift package.")

(defvar lispm-desktop-browser-extensions-changed-hook nil
  "Hook run when the extensions, their buttons or their badges change.")

;;;; Where they can run

(defvar lispm-desktop-browser-extensions--macos-version nil
  "The version of macOS, once read.")

(defun lispm-desktop-browser-extensions--macos-version ()
  "Return the version of macOS this Emacs runs on, as a string, or nil."
  (or lispm-desktop-browser-extensions--macos-version
      (setq lispm-desktop-browser-extensions--macos-version
            (car (ignore-errors (process-lines "sw_vers" "-productVersion"))))))

(defun lispm-desktop-browser-extensions-supported-p ()
  "Return non-nil when pages can run Chrome extensions here: in the Mac
build of Emacs, built with xwidgets, on macOS 15.4 or later."
  (and (eq system-type 'darwin)
       (featurep 'ns)
       (featurep 'xwidget-internal)
       (when-let* ((version (lispm-desktop-browser-extensions--macos-version)))
         (version<= "15.4" version))))

;;;; The module

(defun lispm-desktop-browser-extensions--module-file ()
  "Return the built library of the module, or nil."
  (let ((file (expand-file-name ".build/release/liblispm_desktop_webext.dylib"
                                lispm-desktop-browser-extensions-source-directory)))
    (and (file-exists-p file) file)))

(defun lispm-desktop-browser-extensions--stale-p ()
  "Return non-nil when a source of the module is newer than its library,
as after an update."
  (when-let* ((library (lispm-desktop-browser-extensions--module-file)))
    (let ((source lispm-desktop-browser-extensions-source-directory))
      (seq-some (lambda (file) (file-newer-than-file-p file library))
                (append (mapcar (lambda (name) (expand-file-name name source))
                                '("Package.swift" "Package.resolved"))
                        (directory-files-recursively (expand-file-name "Sources" source)
                                                     "\\.swift\\'"))))))

(defvar lispm-desktop-browser-extensions--building nil
  "The Swift process building the module, while it runs.")

(defvar lispm-desktop-browser-extensions--after-build nil
  "Functions to call once the module being built is built.")

(defun lispm-desktop-browser-extensions-build (&optional callback)
  "Build the module with Swift in the background, then call CALLBACK.
Its output is in the *lispm-desktop-webext build* buffer."
  (interactive)
  (unless (executable-find "swift")
    (user-error "Building lispm-desktop-webext needs Swift: install Xcode or its command line tools"))
  (when callback
    (push callback lispm-desktop-browser-extensions--after-build))
  (unless (process-live-p lispm-desktop-browser-extensions--building)
    (let ((default-directory (file-name-as-directory lispm-desktop-browser-extensions-source-directory))
          (buffer (get-buffer-create "*lispm-desktop-webext build*")))
      (with-current-buffer buffer (erase-buffer))
      (message "Building lispm-desktop-webext, the browser's extension host…")
      (setq lispm-desktop-browser-extensions--building
            (make-process
             :name "lispm-desktop-webext build" :buffer buffer :noquery t
             :command '("swift" "build" "-c" "release" "--product" "lispm_desktop_webext")
             :sentinel #'lispm-desktop-browser-extensions--built)))))

(defun lispm-desktop-browser-extensions--built (process _event)
  "Report how the module's build PROCESS went, and call what waited for it."
  (unless (process-live-p process)
    (let ((waiting (reverse lispm-desktop-browser-extensions--after-build)))
      (setq lispm-desktop-browser-extensions--after-build nil)
      (if (and (= (process-exit-status process) 0) (lispm-desktop-browser-extensions--module-file))
          (progn
            ;; Swift leaves a library it found current untouched.
            (set-file-times (lispm-desktop-browser-extensions--module-file))
            (if (featurep 'lispm-desktop-webext-module)
                (message "lispm-desktop-webext is rebuilt: restart Emacs to use it")
              (message "lispm-desktop-webext is built"))
            (mapc #'funcall waiting))
        (message "lispm-desktop-webext could not be built: see *lispm-desktop-webext build*")))))

(defvar lispm-desktop-browser-extensions--started nil
  "Non-nil once the module's extension host runs.")

(defun lispm-desktop-browser-extensions--start ()
  "Load the module and start its extension host when it is built; return
non-nil once it runs.  Pages already open run no extensions."
  (or lispm-desktop-browser-extensions--started
      (when-let* (((lispm-desktop-browser-extensions-supported-p))
                  ((fboundp 'module-load))
                  ((not (process-live-p lispm-desktop-browser-extensions--building)))
                  (file (lispm-desktop-browser-extensions--module-file)))
        (unless (featurep 'lispm-desktop-webext-module)
          (module-load file))
        (make-directory lispm-desktop-browser-extensions-directory t)
        (lispm-desktop-webext-module-start (expand-file-name lispm-desktop-browser-extensions-directory))
        (add-hook 'kill-emacs-hook #'lispm-desktop-browser-extensions--stop)
        (when (bound-and-true-p xwidget-list)
          (message "Extensions run in the pages opened from now on"))
        (setq lispm-desktop-browser-extensions--started t))))

(defun lispm-desktop-browser-extensions--stop ()
  "Stop the module calling Emacs back, as Emacs exits and closes its pipe."
  (when (fboundp 'lispm-desktop-webext-module-stop)
    (lispm-desktop-webext-module-stop)))

(defun lispm-desktop-browser-extensions--ensure (then)
  "Call THEN once the extension host runs, the module built first if need be."
  (cond ((not (lispm-desktop-browser-extensions-supported-p))
         (user-error "Chrome extensions need the Mac build of Emacs with xwidgets, on macOS 15.4 or later"))
        ((lispm-desktop-browser-extensions--start) (funcall then))
        (t (lispm-desktop-browser-extensions-build
            (lambda () (when (lispm-desktop-browser-extensions--start) (funcall then)))))))

;;;; Pages, as the module knows them

(defun lispm-desktop-browser-extensions--address-in (printed)
  "Return the view address in PRINTED, an xwidget as `prin1' writes it."
  (when (string-match "\\`#<xwidget [0-9]+ 0x\\([[:xdigit:]]+\\)>\\'" printed)
    (string-to-number (match-string 1 printed) 16)))

(defun lispm-desktop-browser-extensions--address (xwidget)
  "Return the address of XWIDGET's WebKit view, the module's name for its page.
Emacs on macOS prints an xwidget with that address."
  (and xwidget (lispm-desktop-browser-extensions--address-in (prin1-to-string xwidget))))

(defun lispm-desktop-browser-extensions--buffer (address)
  "Return the buffer of the page whose view is at ADDRESS, or nil."
  (seq-some (lambda (xwidget)
              (and (eql (lispm-desktop-browser-extensions--address xwidget) address)
                   (buffer-live-p (xwidget-buffer xwidget))
                   (xwidget-buffer xwidget)))
            (bound-and-true-p xwidget-list)))

(defun lispm-desktop-browser-extensions--front-buffer ()
  "Return the page in front: the selected window's, or else the browser's."
  (let ((selected (window-buffer (selected-window))))
    (if (eq (buffer-local-value 'major-mode selected) 'xwidget-webkit-mode)
        selected
      (and (fboundp 'lispm-desktop-browser--shown-buffer)
           (lispm-desktop-browser--shown-buffer)))))

(defun lispm-desktop-browser-extensions--follow (&rest _)
  "Tell the extensions which page is in front.  The module tells WebKit only
when it changed."
  (when-let* ((lispm-desktop-browser-extensions--started)
              (buffer (lispm-desktop-browser-extensions--front-buffer))
              ((buffer-live-p buffer))
              (xwidget (with-current-buffer buffer (xwidget-at (point-min))))
              (address (lispm-desktop-browser-extensions--address xwidget)))
    (lispm-desktop-webext-module-activate address)))

;;;; What the module says

(defvar lispm-desktop-browser-extensions--installed 'unread
  "The installed extensions as the module last listed them, plists, or
`unread' until they are listed again.")

(defun lispm-desktop-browser-extensions--parse (json)
  "Return JSON parsed, objects as plists and arrays as lists."
  (json-parse-string json :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun lispm-desktop-browser-extensions-installed ()
  "Return the installed extensions, each a plist: :id, :name, :version,
:enabled, :loaded, :fromStore, :source, and, while it runs, :label, :badge,
:actionEnabled and :popup of its button for the page in front, :options
and :errors."
  (when (eq lispm-desktop-browser-extensions--installed 'unread)
    (setq lispm-desktop-browser-extensions--installed
          (and lispm-desktop-browser-extensions--started
               (lispm-desktop-browser-extensions--parse (lispm-desktop-webext-module-list)))))
  lispm-desktop-browser-extensions--installed)

(defun lispm-desktop-webext--event (kind json)
  "Handle event KIND of the module, JSON its payload.
The module calls this from its channel's process filter, so whatever waits
for the user, or opens and closes pages, runs after it returns."
  (let ((payload (lispm-desktop-browser-extensions--parse json)))
    (pcase kind
      ("message" (message "%s" payload))
      ("changed" (lispm-desktop-browser-extensions--changed))
      ("ask" (run-at-time 0 nil #'lispm-desktop-browser-extensions--ask payload))
      ("open" (run-at-time 0 nil #'lispm-desktop-browser-extensions--open payload))
      ("close" (run-at-time 0 nil #'lispm-desktop-browser-extensions--close payload))
      ("activate" (run-at-time 0 nil #'lispm-desktop-browser-extensions--activate payload))
      ("search" (run-at-time 0 nil #'lispm-desktop-browser-extensions--search payload)))))

(defun lispm-desktop-browser-extensions--changed ()
  "Forget the list, and redraw what shows it."
  (setq lispm-desktop-browser-extensions--installed 'unread)
  (when-let* ((buffer (get-buffer "*Browser Extensions*")))
    (with-current-buffer buffer (revert-buffer)))
  (run-hooks 'lispm-desktop-browser-extensions-changed-hook))

(defun lispm-desktop-browser-extensions--question (payload)
  "Return the question PAYLOAD asks, its details below it, for a prompt."
  (concat (plist-get payload :question) "\n"
          (mapconcat (lambda (line) (concat "  • " line)) (plist-get payload :details) "\n")
          "\n"))

(defun lispm-desktop-browser-extensions--ask (payload)
  "Ask the question in PAYLOAD and answer the module, no when quit."
  (let (yes)
    (unwind-protect
        (setq yes (yes-or-no-p (lispm-desktop-browser-extensions--question payload)))
      (lispm-desktop-webext-module-answer (plist-get payload :request) (and yes 1)))))

(declare-function xwidget-webkit-goto-uri "xwidget" (xwidget uri))

(defun lispm-desktop-browser-extensions--open-tab (url active)
  "Open URL in a new browser tab, in front when ACTIVE; return the address of
its view.  A running extension's page is shown by a view made for it."
  (unless (or (not active) (lispm-desktop-browser--window))
    (lispm-desktop-browser-show))
  (let* ((own (lispm-desktop-webext-module-expect-page url))
         ;; `xwidget-webkit-browse-url' takes an address whose scheme has a
         ;; hyphen, chrome-extension:, for a bare host and puts https://
         ;; before it; the tab opens blank and is then sent there.
         (first (if own "about:blank" url)))
    (if active
        (lispm-desktop-browser-new-tab first)
      (save-window-excursion (lispm-desktop-browser-new-tab first)))
    ;; Emacs puts the newest xwidget first.
    (let ((xwidget (car (bound-and-true-p xwidget-list))))
      (when own (xwidget-webkit-goto-uri xwidget url))
      (lispm-desktop-browser-extensions--address xwidget))))

(defun lispm-desktop-browser-extensions--open (payload)
  "Open the page in PAYLOAD in a new tab; answer with its view when asked."
  (let ((request (plist-get payload :request))
        address)
    (unwind-protect
        (setq address (lispm-desktop-browser-extensions--open-tab
                       (plist-get payload :url) (plist-get payload :active)))
      (when request
        (lispm-desktop-webext-module-answer request address)))))

(defun lispm-desktop-browser-extensions--close (payload)
  "Close the tab of the page whose view PAYLOAD names."
  (when-let* ((buffer (lispm-desktop-browser-extensions--buffer (plist-get payload :view))))
    (lispm-desktop-browser-close-tab buffer)))

(defvar lispm-desktop-xwidget-search-url)
(declare-function lispm-desktop-browser-open-url "lispm-desktop-browser" (url &optional new-tab))

(defun lispm-desktop-browser-extensions--search (payload)
  "Search for the words in PAYLOAD with the browser's search engine, in a
new tab when it asks for one (chrome.search.query)."
  (let ((text (plist-get payload :text)))
    (unless (string-empty-p (or text ""))
      (lispm-desktop-browser-open-url
       (format lispm-desktop-xwidget-search-url (url-hexify-string text))
       (plist-get payload :newTab)))))

(defun lispm-desktop-browser-extensions--activate (payload)
  "Bring the tab of the page whose view PAYLOAD names in front."
  (when-let* ((buffer (lispm-desktop-browser-extensions--buffer (plist-get payload :view))))
    (unless (lispm-desktop-browser--window)
      (lispm-desktop-browser-show))
    (lispm-desktop-browser-select-tab buffer)))

;;;; Commands

(defun lispm-desktop-browser-extensions--store-id (url)
  "Return the extension id of URL when it is a Chrome Web Store page, else nil."
  (when (and url (string-match (concat "\\`https://\\(?:chromewebstore\\.google\\.com/detail"
                                       "\\|chrome\\.google\\.com/webstore/detail\\)"
                                       "/\\(?:[^/?#]+/\\)?\\([a-p]\\{32\\}\\)\\(?:[/?#]\\|\\'\\)")
                               url))
    (match-string 1 url)))

(defun lispm-desktop-browser-extensions--shown-store-id ()
  "Return the id of the extension whose Chrome Web Store page the browser shows."
  (and (fboundp 'lispm-desktop-browser--uri)
       (lispm-desktop-browser-extensions--store-id (ignore-errors (lispm-desktop-browser--uri)))))

(defun lispm-desktop-browser-extensions--label (extension)
  "Return EXTENSION's name, with its button's badge when it shows one."
  (let ((badge (plist-get extension :badge)))
    (if (and badge (not (string-empty-p badge)))
        (format "%s [%s]" (plist-get extension :name) badge)
      (plist-get extension :name))))

(defun lispm-desktop-browser-extensions--read (prompt &optional predicate)
  "Read an installed extension with PROMPT, among those PREDICATE accepts;
return its id.  In the list of extensions, it is the one at point."
  (if-let* (((derived-mode-p 'lispm-desktop-browser-extensions-mode))
            (id (tabulated-list-get-id)))
      id
    (lispm-desktop-browser-extensions--choose prompt predicate)))

(defun lispm-desktop-browser-extensions--choose (prompt predicate)
  "Read an installed extension with PROMPT, among those PREDICATE accepts."
  (let* ((extensions (seq-filter (or predicate #'identity) (lispm-desktop-browser-extensions-installed)))
         (choices (mapcar (lambda (extension)
                            (cons (format "%s %s" (plist-get extension :name) (plist-get extension :version))
                                  (plist-get extension :id)))
                          extensions)))
    (unless choices (user-error "No extension to choose from"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

;;;###autoload
(defun lispm-desktop-browser-extensions-install (text)
  "Add the Chrome extension TEXT names, its Chrome Web Store link or id.
It is fetched from the store and its signature checked, then you are asked,
with what it will be able to do.  The page shown is the default when it
is a store page."
  (interactive
   (let ((shown (lispm-desktop-browser-extensions--shown-store-id)))
     (list (read-string (format-prompt "Add from the Chrome Web Store (link or id)" shown)
                        nil nil shown))))
  (lispm-desktop-browser-extensions--ensure
   (lambda () (lispm-desktop-webext-module-install-store text))))

(defun lispm-desktop-browser-extensions-install-shown ()
  "Add the extension whose Chrome Web Store page the browser shows."
  (interactive)
  (if-let* ((id (lispm-desktop-browser-extensions--shown-store-id)))
      (lispm-desktop-browser-extensions-install id)
    (user-error "The page shown is no Chrome Web Store extension")))

;;;###autoload
(defun lispm-desktop-browser-extensions-load-unpacked (directory)
  "Add the unpacked extension in DIRECTORY, a copy of it, as Chrome's
developer mode does; `lispm-desktop-browser-extensions-reload' takes its
changes."
  (interactive "DLoad the extension in folder: ")
  (lispm-desktop-browser-extensions--ensure
   (lambda () (lispm-desktop-webext-module-install-folder (expand-file-name directory)))))

(defun lispm-desktop-browser-extensions--running-p (extension)
  "Return non-nil when EXTENSION is on and runs."
  (plist-get extension :loaded))

;;;###autoload
(defun lispm-desktop-browser-extensions-press (id)
  "Press the button of extension ID for the page in front: its popup opens,
or it is told it was clicked.  Asks which one, badges shown."
  (interactive
   (progn (unless lispm-desktop-browser-extensions--started
            (user-error "No extension runs: add one with M-x lispm-desktop-browser-extensions-install"))
          (lispm-desktop-browser-extensions--follow)
          (list (lispm-desktop-browser-extensions--read
                 "Press the button of: " #'lispm-desktop-browser-extensions--running-p))))
  (unless (lispm-desktop-webext-module-action id)
    (user-error "That extension has no button")))

(defun lispm-desktop-browser-extensions-options (id)
  "Open the options page of extension ID in a new tab."
  (interactive (list (lispm-desktop-browser-extensions--read
                      "Options of: " (lambda (extension) (plist-get extension :options)))))
  (if-let* ((url (lispm-desktop-webext-module-options-url id)))
      (lispm-desktop-browser-extensions--open-tab url t)
    (user-error "That extension has no options page")))

(defun lispm-desktop-browser-extensions-reload (id)
  "Load extension ID again, copied afresh from its folder if it came from one."
  (interactive (list (lispm-desktop-browser-extensions--read "Reload: ")))
  (lispm-desktop-webext-module-reload id))

(defun lispm-desktop-browser-extensions-toggle (id)
  "Turn extension ID off when it is on, else on."
  (interactive (list (lispm-desktop-browser-extensions--read "Turn on or off: ")))
  (let ((extension (seq-find (lambda (extension) (equal (plist-get extension :id) id))
                             (lispm-desktop-browser-extensions-installed))))
    (lispm-desktop-webext-module-set-enabled id (not (plist-get extension :enabled)))))

(defun lispm-desktop-browser-extensions-remove (id)
  "Remove extension ID and what it kept, once you say yes."
  (interactive (list (lispm-desktop-browser-extensions--read "Remove: ")))
  (let ((extension (seq-find (lambda (extension) (equal (plist-get extension :id) id))
                             (lispm-desktop-browser-extensions-installed))))
    (when (yes-or-no-p (format "Remove “%s” and what it kept? " (plist-get extension :name)))
      (lispm-desktop-webext-module-remove id))))

;;;; The list of them

(defun lispm-desktop-browser-extensions--entries ()
  "Return the rows of the extension list."
  (mapcar (lambda (extension)
            (list (plist-get extension :id)
                  (vector (plist-get extension :name)
                          (plist-get extension :version)
                          (cond ((plist-get extension :loaded) "on")
                                ((plist-get extension :enabled) "failed")
                                (t "off"))
                          (or (plist-get extension :badge) "")
                          (if (plist-get extension :fromStore) "Chrome Web Store"
                            (abbreviate-file-name (or (plist-get extension :source) ""))))))
          (lispm-desktop-browser-extensions-installed)))

(defvar-keymap lispm-desktop-browser-extensions-mode-map
  :doc "Keys of the extension list; those of one extension act on the line's."
  "RET" #'lispm-desktop-browser-extensions-press
  "a" #'lispm-desktop-browser-extensions-press
  "o" #'lispm-desktop-browser-extensions-options
  "e" #'lispm-desktop-browser-extensions-toggle
  "r" #'lispm-desktop-browser-extensions-reload
  "D" #'lispm-desktop-browser-extensions-remove
  "i" #'lispm-desktop-browser-extensions-install
  "l" #'lispm-desktop-browser-extensions-load-unpacked)

(define-derived-mode lispm-desktop-browser-extensions-mode tabulated-list-mode "Extensions"
  "The browser's Chrome extensions.
\\<lispm-desktop-browser-extensions-mode-map>
\\[lispm-desktop-browser-extensions-install] adds one from the Chrome Web Store,
\\[lispm-desktop-browser-extensions-load-unpacked] one from a folder.  On a line,
\\[lispm-desktop-browser-extensions-press] presses its button,
\\[lispm-desktop-browser-extensions-options] opens its options,
\\[lispm-desktop-browser-extensions-toggle] turns it on or off,
\\[lispm-desktop-browser-extensions-reload] reloads it and
\\[lispm-desktop-browser-extensions-remove] removes it."
  (setq tabulated-list-format [("Name" 28 t) ("Version" 12 nil) ("State" 7 t)
                               ("Badge" 6 nil) ("From" 0 nil)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (lispm-desktop-browser-extensions--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun lispm-desktop-browser-extensions ()
  "List the browser's Chrome extensions, to add, press, set up and remove them."
  (interactive)
  (lispm-desktop-browser-extensions--ensure
   (lambda ()
     (with-current-buffer (get-buffer-create "*Browser Extensions*")
       (lispm-desktop-browser-extensions-mode)
       (setq tabulated-list-entries (lispm-desktop-browser-extensions--entries))
       (tabulated-list-print)
       (pop-to-buffer (current-buffer))))))

;;;; The browser's Extensions menu

(defun lispm-desktop-browser-extensions--menu-items ()
  "Return the items of the browser's Extensions menu: each running
extension's button, then adding and managing them."
  (let ((running (and lispm-desktop-browser-extensions--started
                      (seq-filter #'lispm-desktop-browser-extensions--running-p
                                  (lispm-desktop-browser-extensions-installed)))))
    `(,@(mapcar (lambda (extension)
                  (let ((id (plist-get extension :id)))
                    (list :label (lispm-desktop-browser-extensions--label extension)
                          :action (lambda () (lispm-desktop-browser-extensions-press id))
                          :enabled (plist-get extension :actionEnabled))))
                running)
      ,@(when running '(separator))
      (:label "Add This Extension" :action lispm-desktop-browser-extensions-install-shown
              :enabled ,(and (lispm-desktop-browser-extensions--shown-store-id) t))
      (:label "Add from Chrome Web Store…" :action lispm-desktop-browser-extensions-install)
      (:label "Load Unpacked…" :action lispm-desktop-browser-extensions-load-unpacked)
      separator
      (:label "Manage Extensions" :action lispm-desktop-browser-extensions))))

;;;; Setup

(defun lispm-desktop-setup-browser-extensions ()
  "Run Chrome extensions in the browser's pages where Emacs can.
The host starts at once when its module is built, so that every page runs
them; the module is built in the background when extensions were added
before and it is missing or its sources changed."
  (when (and lispm-desktop-browser-extensions-enabled (lispm-desktop-browser-extensions-supported-p))
    (add-hook 'window-selection-change-functions #'lispm-desktop-browser-extensions--follow)
    (add-hook 'window-buffer-change-functions #'lispm-desktop-browser-extensions--follow)
    (let ((added (file-exists-p (expand-file-name "installed.json" lispm-desktop-browser-extensions-directory))))
      (cond ((and (lispm-desktop-browser-extensions--module-file)
                  (not (lispm-desktop-browser-extensions--stale-p)))
             (lispm-desktop-browser-extensions--start))
            ((and added (executable-find "swift"))
             (lispm-desktop-browser-extensions-build #'lispm-desktop-browser-extensions--start))
            ;; Changed sources and no Swift to build them: the module built
            ;; before runs still.
            ((lispm-desktop-browser-extensions--module-file)
             (lispm-desktop-browser-extensions--start))))))

(provide 'lispm-desktop-browser-extensions)
;;; lispm-desktop-browser-extensions.el ends here
