;;; lispm-develop.el --- Developing Lisp in the running image  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; On a Lisp Machine a program grew in the world it ran in: edit one
;; definition, evaluate it into the running image, try it, and the
;; editor knew where every definition was and who used it.  These
;; tools give agents that loop for Emacs Lisp:
;;
;;   edit_definition   replaces the form defining a function, macro,
;;                     variable or face in its file, found as
;;                     `find-function' finds it, so that the model
;;                     writes the new form and never the old one;
;;   eval_definition   evaluates it into Emacs from the file's buffer,
;;                     as C-M-x does: a defvar, defcustom or defface is
;;                     reset to what it now says;
;;   ert_run           runs ERT tests in Emacs and reports the failures.
;;
;; and, for code in any language Emacs knows about:
;;
;;   xref_definitions, xref_references
;;                     ask the xref backend of the code's buffer:
;;                     Emacs Lisp's own, a language server's with eglot;
;;   flymake_diagnostics
;;                     runs the flymake backends of a buffer: for Emacs
;;                     Lisp the byte compiler and checkdoc.
;;
;; They are deferred tools, which search_tools finds (their source is
;; called "lisp"): most conversations need none of them.
;;
;; edit_definition changes a file as edit_file does: as one undo step,
;; journaled, saved, previewed as a diff and shown in place.  Its file
;; is found from the symbol, wherever Emacs loaded the definition from,
;; Emacs's own sources included; its :targets says which file that is,
;; so that a write outside the agent's root asks, whatever the policy.
;; eval_definition and ert_run run code: their class is `exec'.

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'flymake)
(require 'ert)
(require 'project)
(require 'lispm-core)
(require 'lispm-tools)
(require 'lispm-develop-core)

;;;; Tests

(defun lispm-develop--selector (text)
  "The ERT selector TEXT stands for.
Lisp, if it starts with a parenthesis, a colon or a double quote, or is
t; else the name of a test, if one has that name; else a regexp
matching the names of tests."
  (let ((text (string-trim text)))
    (cond ((string-empty-p text) (error "Say which tests to run"))
          ((string-match-p "\\`\\(?:[(:\"]\\|t\\'\\)" text)
           (pcase-let ((`(,selector . ,end)
                        (condition-case nil
                            (read-from-string text)
                          (error (error "Cannot read the selector %s" text)))))
             (unless (string-blank-p (substring text end))
               (error "The selector %s holds more than one form" text))
             selector))
          ((when-let* ((symbol (intern-soft text)))
             (and (ert-test-boundp symbol) symbol)))
          (t text))))

(defun lispm-develop--ert-listener (event &rest arguments)
  "Follow a run of ert_run, as ERT calls a listener with EVENT and ARGUMENTS.
ERT records a test that C-g quits and goes on to the next; this stops
the run instead, so that C-g interrupts the call."
  (when (and (eq event 'test-ended) (ert-test-quit-p (nth 2 arguments)))
    (signal 'quit nil)))

(defun lispm-develop--ert-frames (test result)
  "The innermost calls of the backtrace of RESULT, a result of TEST.
Each is (FUNCTION . ARGUMENTS), innermost first: the calls made from
TEST's body, without the special forms and without the `signal' and
`ert-fail' that report a failed `should'."
  (let ((body (ert-test-body test)) frames)
    (catch 'body
      (dolist (frame (ert-test-result-with-condition-backtrace result))
        (when (eq (backtrace-frame-fun frame) body)
          (throw 'body nil))
        (when (backtrace-frame-evald frame)
          (push (cons (backtrace-frame-fun frame) (backtrace-frame-args frame)) frames))))
    (seq-drop-while (lambda (frame) (memq (car frame) '(signal ert-fail)))
                    (nreverse frames))))

(defun lispm-develop--ert-failure (test result)
  "What went wrong in RESULT, a result of TEST, as lines of text."
  (let ((print-length 12) (print-level 6) (print-escape-newlines t))
    (if (not (ert-test-result-with-condition-p result))
        (list "  It exited without a result, by a throw or the like.")
      (let ((frames (lispm-develop--ert-frames test result)))
        (append
         (mapcar (lambda (info) (format "  %s%s" (car info) (cdr info)))
                 (ert-test-result-with-condition-infos result))
         (list (concat "  " (lispm-truncate
                             (prin1-to-string (ert-test-result-with-condition-condition result))
                             1500)))
         (and frames
              (cons "  Backtrace, innermost call first:"
                    (mapcar (lambda (line) (concat "  " line))
                            (split-string (lispm-tools-backtrace-text (seq-take frames 8))
                                          "\n")))))))))

(defun lispm-develop--ert-report (tests seconds)
  "The result of ert_run, which ran TESTS in SECONDS."
  (let ((passed 0) (expected-failures 0) failures skipped surprises)
    (dolist (test tests)
      (let ((result (ert-test-most-recent-result test))
            (name (ert-test-name test)))
        (cond ((null result)
               (push (list (format "NOT RUN %s" name)) failures))
              ((ert-test-skipped-p result)
               (push (format "SKIPPED %s: %s" name
                             (let ((print-length 8) (print-level 4))
                               (lispm-oneline (prin1-to-string
                                                (cadr (ert-test-result-with-condition-condition result)))
                                               200)))
                     skipped))
              ((ert-test-result-expected-p test result)
               (if (ert-test-passed-p result) (cl-incf passed) (cl-incf expected-failures)))
              ((ert-test-passed-p result)
               (push (format "PASSED %s, which was expected to fail" name) surprises))
              (t
               (push (cons (format "FAILED %s" name) (lispm-develop--ert-failure test result))
                     failures)))))
    (string-join
     (append
      (list (format "Ran %d test%s in %.2f s: %s."
                    (length tests) (if (cdr tests) "s" "") seconds
                    (string-join
                     (delq nil (list (format "%d passed" passed)
                                     (format "%d failed" (length failures))
                                     (and skipped (format "%d skipped" (length skipped)))
                                     (and (> expected-failures 0)
                                          (format "%d failed as expected" expected-failures))
                                     (and surprises (format "%d passed unexpectedly"
                                                            (length surprises)))))
                     ", ")))
      (mapcar (lambda (lines) (concat "\n" (string-join lines "\n"))) (nreverse failures))
      (and surprises (cons "" (nreverse surprises)))
      (and skipped (cons "" (nreverse skipped))))
     "\n")))

(lispm-deftool ert_run
    ((selector string "Which tests: a regexp matching their names, a test's name, or an ERT selector in Lisp such as t (all), (tag NAME) or (not (tag slow)).")
     (file string "A file of tests to load first, relative to your root, when they are not in Emacs yet or have changed."
           :optional t))
  "Run ERT tests in this Emacs and report how they went.
Returns a summary, then each failure with its condition and the
innermost calls of its backtrace.  Run the tests of what you changed
once eval_definition has loaded it; give file to load tests first."
  :permission exec
  :deferred t
  (when file
    (let ((file (lispm-tools-file file)))
      (condition-case err
          (load file nil t t)
        (error (error "Loading %s failed: %s" (lispm-tools-label file) (error-message-string err))))))
  (let* ((selector (lispm-develop--selector selector))
         (tests (ert-select-tests selector t)))
    (if (null tests)
        (format "No tests match %S%s." selector
                (if file "" "; if they are in a file Emacs has not loaded, give it as file"))
      (let ((start (float-time)))
        (ert-run-tests `(member ,@tests) #'lispm-develop--ert-listener)
        (lispm-develop--ert-report tests (- (float-time) start))))))

;;;; Cross-references

(defun lispm-develop--xref-line (location)
  "The line of the xref LOCATION, or nil if it cannot be told without visiting.
An Emacs Lisp definition's line is found in its file's text as
edit_definition finds it."
  (cond ((xref-location-line location))
        ((xref-elisp-location-p location)
         (let ((file (xref-location-group location)))
           (when (and (stringp file) (file-readable-p file))
             (with-temp-buffer
               (insert (lispm-file-text file))
               (when-let* ((bounds (lispm-develop-search (xref-elisp-location-symbol location)
                                                          (xref-elisp-location-type location))))
                 (line-number-at-pos (car bounds)))))))
        ((xref-buffer-location-p location)
         (let ((marker (xref-location-marker location)))
           (with-current-buffer (marker-buffer marker)
             (lispm-develop-line marker))))))

(defun lispm-develop--xref-lines (items)
  "ITEMS, xref items, as text: a line FILE:LINE: SUMMARY each.
FILE is relative to the acting agent's root inside it; lines alike are
given once, and at most `lispm-xref-max-results' of them."
  (let (lines)
    (dolist (item items)
      (let* ((location (xref-item-location item))
             (group (xref-location-group location))
             (line (ignore-errors (lispm-develop--xref-line location)))
             (text (format "%s:%s %s"
                           (if (and (stringp group) (file-name-absolute-p group))
                               (lispm-tools-label group)
                             group)
                           (if line (format "%d:" line) "")
                           (lispm-oneline (substring-no-properties (xref-item-summary item)) 200))))
        (unless (member text lines)
          (push text lines))))
    (setq lines (nreverse lines))
    (concat (string-join (seq-take lines lispm-xref-max-results) "\n")
            (if (> (length lines) lispm-xref-max-results)
                (format "\n[... %d more]" (- (length lines) lispm-xref-max-results))
              ""))))

(defun lispm-develop--xref (identifier path line find)
  "What FIND finds for IDENTIFIER, as text (see `lispm-develop--xref-lines').
FIND is called with an xref backend and an identifier, and returns xref
items.  It is called in the buffer of the file PATH, or for Emacs Lisp
in the running Emacs if PATH is nil.  Point is put on IDENTIFIER first:
on LINE of PATH if given, where the backend is asked about the
identifier at point, else at its first occurrence, as a language server
answers about a position."
  (lispm-develop-in-buffer
   (and path (lispm-tools-file path))
   (lambda ()
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (let ((regexp (concat "\\_<" (regexp-quote identifier) "\\_>"))
               (case-fold-search nil))
           (cond (line
                  (unless (and (> line 0) (zerop (forward-line (1- line))))
                    (error "%s has no line %d" path line))
                  (unless (re-search-forward regexp (line-end-position) t)
                    (error "%s is not on line %d of %s" identifier line path))
                  (goto-char (match-beginning 0)))
                 ((re-search-forward regexp nil t)
                  (goto-char (match-beginning 0)))))
         (let* ((backend (or (xref-find-backend)
                             (error "Emacs has no xref backend for %s (%s)" (buffer-name) mode-name)))
                (id (or (and line (xref-backend-identifier-at-point backend)) identifier))
                (inhibit-message t)
                (items (funcall find backend id)))
           (if items
               (lispm-develop--xref-lines items)
             (format "Nothing found for %s." identifier))))))))

(lispm-deftool xref_definitions
    ((identifier string "The name to look up, such as an Emacs Lisp symbol.")
     (path string "A file of the code, whose language decides how to look (default: Emacs Lisp, in this Emacs)."
           :optional t)
     (line integer "The line of path where the identifier is, to ask about that occurrence (default: its first)."
           :optional t))
  "Find where an identifier is defined, through Emacs's xref: file:line: summary.
For Emacs Lisp (the default) from the definitions in this Emacs, of
functions, variables, faces and more; for another language, give a file
of it as path: its xref backend answers, a language server's with eglot."
  :permission read
  :deferred t
  :spotlight (lambda (result identifier &optional _path _line)
               (list :kind 'xref-definitions
                     :subject identifier
                     :result result))
  (lispm-develop--xref identifier path line #'xref-backend-definitions))

(lispm-deftool xref_references
    ((identifier string "The name to look up, such as an Emacs Lisp symbol.")
     (path string "A file of the code, whose language decides how to look (default: Emacs Lisp, in this Emacs)."
           :optional t)
     (line integer "The line of path where the identifier is, to ask about that occurrence (default: its first)."
           :optional t)
     (everywhere boolean "Search the project's external roots too, for Emacs Lisp all of load-path: slow."
                 :optional t))
  "Find the uses of an identifier, through Emacs's xref: file:line: the line.
Who calls a function, who reads a variable: before changing a
definition, see what depends on it.  For Emacs Lisp (the default) the
project's files are searched; for another language, give a file of it
as path: its xref backend answers, a language server's with eglot."
  :permission read
  :deferred t
  :spotlight (lambda (result identifier &optional _path _line _everywhere)
               (list :kind 'xref-references
                     :subject identifier
                     :result result))
  (lispm-develop--xref
   identifier path line
   (lambda (backend id)
     ;; The project is the buffer's, never one asked for; outside one,
     ;; the buffer's directory stands for it.
     (let ((project-current-directory-override default-directory)
           (project-vc-external-roots-function
            (if everywhere project-vc-external-roots-function #'ignore)))
       (xref-backend-references backend id)))))

;;;; Diagnostics

(defun lispm-develop--severity (type)
  "The severity of the flymake diagnostic TYPE: error, warning or note."
  (pcase (or (get type 'flymake-category) type)
    ('flymake-error "error")
    ('flymake-warning "warning")
    ('flymake-note "note")
    (_ (string-remove-prefix ":" (format "%s" type)))))

(defun lispm-develop--diagnostic-line (diagnostic)
  "The flymake DIAGNOSTIC of the current buffer as LINE:COLUMN: TYPE: MESSAGE."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (flymake-diagnostic-beg diagnostic))
      (format "%d:%d: %s: %s" (line-number-at-pos) (1+ (current-column))
              (lispm-develop--severity (flymake-diagnostic-type diagnostic))
              (lispm-oneline (flymake-diagnostic-text diagnostic) 500)))))

(defun lispm-develop--not-run (backend)
  "Why the flymake BACKEND of the current buffer did not run, as a note."
  (if (and (eq backend 'elisp-flymake-byte-compile)
           (fboundp 'trusted-content-p)
           (not (trusted-content-p)))
      (format "[The byte compiler did not check %s: Emacs does not trust its content \
\(see `trusted-content'), and compiling it would run its code.  Nothing was said about \
compiler warnings; if the user trusts the file, they add it to `trusted-content'.]"
              (buffer-name))
    (let ((why (when-let* (((boundp 'flymake--state))
                           ((hash-table-p flymake--state))
                           (st (gethash backend flymake--state))
                           ((fboundp 'flymake--state-disabled)))
                 (flymake--state-disabled st))))
      (format "[%s did not run: %s]" backend
              (cond ((stringp why) why)
                    ((and (consp why) (symbolp (car why))) (error-message-string why))
                    (why (format "%S" why))
                    (t "it disabled itself"))))))

(defun lispm-develop--diagnostics (label waiting)
  "What flymake found in the current buffer, as flymake_diagnostics says it.
LABEL names the buffer's file; WAITING are the backends that have not
reported yet."
  (let* ((diagnostics (sort (flymake-diagnostics)
                            (lambda (a b) (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))
         (disabled (flymake-disabled-backends))
         (ran (seq-difference (flymake-running-backends) waiting))
         (counts (mapcar (lambda (severity)
                           (let ((n (cl-count severity diagnostics
                                              :key (lambda (d) (lispm-develop--severity
                                                                (flymake-diagnostic-type d)))
                                              :test #'equal)))
                             (and (> n 0) (format "%d %s%s" n severity (if (= n 1) "" "s")))))
                         '("error" "warning" "note"))))
    (string-join
     (append
      (list (format "%s: %s, from %s." label
                    (if (delq nil counts) (string-join (delq nil counts) ", ") "no diagnostics")
                    (if ran (mapconcat #'symbol-name ran ", ") "no checker")))
      (mapcar #'lispm-develop--diagnostic-line diagnostics)
      (mapcar #'lispm-develop--not-run disabled)
      (and waiting (list (format "[Still running after %d s, not reported: %s]"
                                 lispm-diagnostics-timeout
                                 (mapconcat #'symbol-name waiting ", ")))))
     "\n")))

(defun lispm-develop--diagnose (buffer created done)
  "Run the flymake backends of BUFFER and call DONE with what they report.
BUFFER is left as it was found: `flymake-mode', if it was off there, is
turned on for the check and off again; and with CREATED non-nil, BUFFER
was visited for the check and is killed after it.  The backends report
asynchronously; the agent's turn in which the check started may end
meanwhile, and then no result is given."
  (let* ((agent lispm-current-agent)
         (turn (and agent (lispm-agent-busy-p agent) (lispm-agent-turn agent)))
         (label (if (buffer-file-name buffer) (lispm-tools-label (buffer-file-name buffer))
                  (buffer-name buffer)))
         (was-on (buffer-local-value 'flymake-mode buffer))
         (deadline (+ (float-time) lispm-diagnostics-timeout))
         timer)
    (cl-labels
        ((restore ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and flymake-mode (not was-on))
                 (flymake-mode -1)))
             (when (and created (not (buffer-modified-p buffer)))
               (kill-buffer buffer))))
         (finish (text &optional errorp)
           (when timer (cancel-timer timer))
           (restore)
           (unless (and turn (or (not (lispm-agent-busy-p agent))
                                 (/= turn (lispm-agent-turn agent))))
             (funcall done text errorp)))
         (check ()
           (condition-case err
               (if (not (buffer-live-p buffer))
                   (finish "The buffer was killed before its checkers reported." t)
                 (with-current-buffer buffer
                   (let ((waiting (seq-difference (flymake-running-backends)
                                                  (flymake-reporting-backends))))
                     (when (or (null waiting) (> (float-time) deadline))
                       (finish (lispm-develop--diagnostics label waiting))))))
             (error (finish (error-message-string err) t)))))
      (condition-case err
          (with-current-buffer buffer
            (unless (run-hook-wrapped 'flymake-diagnostic-functions (lambda (_backend) t))
              (error "Flymake has no checker for %s (%s)" label mode-name))
            (unless was-on
              (let ((flymake-start-on-flymake-mode nil))
                (flymake-mode 1)))
            (let ((inhibit-message t))
              ;; Disabled backends too: they say why they are.
              (flymake-start nil t)))
        (error (restore)
               (signal (car err) (cdr err))))
      (setq timer (run-at-time 0.05 0.1 #'check)))))

(lispm-deftool flymake_diagnostics
    ((path string "File to check, absolute or relative to your root." :optional t)
     (buffer string "A buffer to check instead, by name." :optional t))
  "Check a file or buffer with flymake, without building: line:col: type: message.
The checkers are those Emacs runs on it: for Emacs Lisp the byte
compiler (in a file the user trusts) and checkdoc, with eglot the
language server.  Check code after changing it."
  :async t
  :deferred t
  :spotlight (lambda (result &optional path buffer)
               (list :kind 'flymake-diagnostics
                     :file (and path (lispm-tools-file path))
                     :buffer (and buffer (lispm-tools-buffer buffer))
                     :result result))
  (cond (buffer (lispm-develop--diagnose (lispm-tools-buffer buffer) nil done))
        (path (let* ((file (lispm-tools-file path))
                     (open (find-buffer-visiting file))
                     (visited (lispm-develop-buffer-of file)))
                (lispm-develop--diagnose visited (not open) done)))
        (t (error "Give a path or a buffer to check"))))

;;;; In-memory caller lookup, granular definition revert, and SCT system definition

(defun lispm-develop--el-files (root)
  "Emacs Lisp files under ROOT, pruning `.git', `.deps' and cache directories."
  (directory-files-recursively
   root "\\.el\\'" nil
   (lambda (dir)
     (not (member (file-name-nondirectory (directory-file-name dir))
                  '(".git" ".deps" "eln-cache" "node_modules"))))))

(defun lispm-develop--index-path (path)
  "Index Emacs Lisp file or directory PATH into the `who-calls' database."
  (let ((target (lispm-tools-file path)))
    (cond
     ((file-directory-p target)
      (dolist (file (lispm-develop--el-files target))
        (with-temp-buffer
          (insert (lispm-file-text file))
          (lispm-develop-index-buffer (current-buffer) file))))
     ((file-regular-p target)
      (with-temp-buffer
        (insert (lispm-file-text target))
        (lispm-develop-index-buffer (current-buffer) target)))
     (t (error "No such file or directory: %s" (lispm-tools-label target))))))

(defun lispm-develop--history-entry (name &optional kind path agent)
  "Latest history entry for NAME (matching KIND, PATH and AGENT when given)."
  (let* ((name-str (if (symbolp name) (symbol-name name) (format "%s" name)))
         (target-type (and kind (cdr (assoc kind lispm-develop-kinds))))
         (target-file (and path (lispm-tools-file path))))
    (seq-find (lambda (h)
                (and (equal (or (plist-get h :name)
                                (symbol-name (plist-get h :symbol)))
                            name-str)
                     (or (null kind) (eq (plist-get h :type) target-type))
                     (or (null target-file) (equal (plist-get h :file) target-file))
                     (or (null agent) (eq (plist-get h :agent) agent))))
              lispm-develop-history)))

(defun lispm-develop--revert-latest-p (entry)
  "Non-nil if ENTRY is still in history and is the latest edit for its target."
  (and entry
       (memq entry lispm-develop-history)
       (eq entry
           (lispm-develop--history-entry
            (or (plist-get entry :name) (symbol-name (plist-get entry :symbol)))
            (car (rassq (plist-get entry :type) lispm-develop-kinds))
            (plist-get entry :file)
            nil))))

(defun lispm-revert-definition (name &optional kind path agent entry)
  "Restore the previous code-object definition of NAME from history.
Unlike full-file revert, this replaces only NAME's form in its file
\(leaving concurrent edits to other definitions intact).  KIND and PATH
disambiguate which definition of NAME to restore; AGENT filters history
by agent; ENTRY, when given, is the exact pinned history record.
Does not evaluate code: use `eval_definition' to load the restored
definition into Emacs."
  (let ((entry (or entry (lispm-develop--history-entry name kind path agent))))
    (unless entry
      (error "No recorded definition edit for %s%s"
             name (if path (format " in %s" path) "")))
    (unless (lispm-develop--revert-latest-p entry)
      (error "The definition edit for %s was already reverted or superseded" name))
    (let* ((symbol (plist-get entry :symbol))
           (type (plist-get entry :type))
           (file (plist-get entry :file))
           (old-form (plist-get entry :old-form))
           (new-form (plist-get entry :new-form))
           (buffer (lispm-tools-existing file)))
      (with-current-buffer buffer
        (pcase-let* ((`(,_t ,cbeg . ,cend) (lispm-develop-located symbol (list type) file))
                     (current (string-trim (save-restriction
                                             (widen)
                                             (buffer-substring-no-properties cbeg cend)))))
          (when (and new-form (not (equal current (string-trim new-form))))
            (error "The definition of %s in %s has changed since that edit"
                   symbol (lispm-tools-label file)))
          (pcase-let ((`(,text ,beg ,end ,_t)
                       (lispm-develop-edit symbol (list type) file old-form)))
            (lispm-tools-set-text buffer text)
            (setq lispm-develop-history (delq entry lispm-develop-history))
            (format "Restored the definition of %s in %s (lines %d-%d).  If the newer \
definition was evaluated, evaluate this one with eval_definition."
                    symbol (lispm-tools-label file)
                    (lispm-develop-line beg) (lispm-develop-line (1- end)))))))))

(defun lispm-develop--module-deps (text)
  "Extract `(PROVIDES . REQUIRES)' from top-level Lisp forms in TEXT.
Reads forms without evaluating them so strings and comments are ignored."
  (let (provides requires)
    (cl-labels
        ((walk (form)
           (when (consp form)
             (pcase form
               (`(require (quote ,(and feat (pred symbolp))) . ,_)
                (push feat requires))
               (`(provide (quote ,(and feat (pred symbolp))) . ,_)
                (push feat provides))
               (`(,(or 'progn 'eval-when-compile 'eval-and-compile
                       'when 'unless 'if 'with-eval-after-load)
                  . ,body)
                (dolist (sub (if (proper-list-p body) body nil))
                  (walk sub)))
               (`(cond . ,clauses)
                (dolist (clause (if (proper-list-p clauses) clauses nil))
                  (when (proper-list-p clause)
                    (mapc #'walk clause))))))))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (walk (read (current-buffer))))
          (end-of-file nil)
          (error nil))))
    (cons (nreverse (delete-dups provides))
          (nreverse (delete-dups requires)))))

(defun lispm-system-definition (&optional root)
  "Inspect the Emacs Lisp system structure under ROOT (as Genera's SCT did).
Return a plist `(:root ROOT :modules MODULES :modified MODIFIED)' where
each module has `:file', `:provides', `:requires' and `:loaded'."
  (let* ((root (file-name-as-directory
                (if root (lispm-tools-file root) (lispm-tools-root))))
         (files (lispm-develop--el-files root))
         modules)
    (dolist (file files)
      (pcase-let ((`(,provides . ,requires)
                   (lispm-develop--module-deps (lispm-file-text file))))
        (push (list :file (file-relative-name file root)
                    :provides provides
                    :requires requires
                    :loaded (and provides (seq-every-p #'featurep provides) t))
              modules)))
    (let ((modified
           (delete-dups
            (delq nil
                  (mapcar (lambda (h)
                            (and (file-in-directory-p (plist-get h :file) root)
                                 (plist-get h :symbol)))
                          lispm-develop-history)))))
      (list :root root :modules (nreverse modules) :modified modified))))

(defun lispm-develop--patch-form (text lexical)
  "Format definition TEXT for an incremental patch with LEXICAL binding.
When TEXT defines a variable or face (`defvar', `defcustom', `defface'),
applies `elisp--eval-defun-1' so loading the patch resets an already-bound
definition just as `eval_definition' (`C-M-x') does."
  (let* ((parsed (car (read-from-string text)))
         (resetp (and (consp parsed)
                      (memq (car parsed) '(defvar defcustom defface))))
         (effective (if resetp
                        (elisp--eval-defun-1 (macroexpand parsed))
                      parsed)))
    (cond ((not lexical)
           (format "(eval '%S nil)" effective))
          (resetp
           (format "%S" effective))
          (t text))))

(defun lispm-develop-patch (&optional root agent)
  "Generate a Genera-style incremental code-object patch for ROOT (and AGENT).
Includes the latest modified definition per `(FILE NAME TYPE)' from
`lispm-develop-history', preserving each file's lexical or dynamic
binding semantics."
  (let* ((root (and root (file-name-as-directory (lispm-tools-file root))))
         (seen (make-hash-table :test #'equal))
         entries)
    (dolist (h lispm-develop-history)
      (let ((key (list (plist-get h :file)
                       (or (plist-get h :name) (symbol-name (plist-get h :symbol)))
                       (plist-get h :type))))
        (when (and (not (gethash key seen))
                   (or (null root) (file-in-directory-p (plist-get h :file) root))
                   (or (null agent) (eq (plist-get h :agent) agent)))
          (puthash key t seen)
          (push h entries))))
    (if (null entries)
        ";;; Incremental patch: no modified definitions recorded.\n"
      (concat ";;; Incremental Patch --- Code objects modified in image  -*- lexical-binding: t; -*-\n\n"
              (mapconcat
               (lambda (h)
                 (let ((form (plist-get h :new-form))
                       (lexical (plist-get h :lexical-binding)))
                   (format ";; %s (%s%s)\n%s\n"
                           (plist-get h :symbol)
                           (if root
                               (file-relative-name (plist-get h :file) root)
                             (abbreviate-file-name (plist-get h :file)))
                           (if lexical "" ", lexical-binding: nil")
                           (lispm-develop--patch-form form lexical))))
               entries
               "\n")))))

;;;; The source of these tools

(defun lispm-develop--sources ()
  "The tools of this file, as a source of deferred tools for search_tools."
  (list (list :name "lisp"
              :description (concat "edit, evaluate and test Emacs Lisp definitions in this Emacs; "
                                   "definitions, references and diagnostics of code in any "
                                   "language Emacs has xref or flymake for")
              :pending nil
              :prepare (lambda (callback) (funcall callback nil))
              :provides (lambda (name) (member name lispm-develop-tools)))))

(add-hook 'lispm-tool-sources #'lispm-develop--sources)

(provide 'lispm-develop)
;;; lispm-develop.el ends here
