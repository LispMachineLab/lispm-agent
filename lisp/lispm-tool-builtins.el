;;; lispm-tool-builtins.el --- Built-in file, buffer, search, and edit tools  -*- lexical-binding: t; -*-

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

;; Built-in tools for reading files and directories, searching files
;; and buffers, describing symbols, and editing files and buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'project)
(require 'help-fns)
(require 'lispm-core)
(require 'lispm-agents-context)
(require 'lispm-media)
(require 'lispm-tool-core)

;; Only an Emacs built with image support has it.
(declare-function image-size "image.c" (spec &optional pixels frame))

;;;; Built-in tools: reading

(defun lispm-tools--media (file)
  "The result of reading FILE, an image or a video: said in a line, and shown."
  (let ((desc (if (eq (lispm-media-kind file) 'video)
                  (format "[video %s]" (lispm-video-description file))
                (format "[image %s%s]" (file-name-nondirectory file)
                        (if-let* (((fboundp 'image-size))
                                  ((display-images-p))
                                  (size (ignore-errors (image-size (create-image file) t))))
                            (format ", %d×%d" (car size) (cdr size))
                          "")))))
    (if (and (boundp 'lispm-context-images) (<= lispm-context-images 0))
        (format "%s (binary media attachment is disabled for this text-only model; inspect image/video files programmatically with Python cv2/PIL/numpy or ffprobe instead of read_file)"
                desc)
      (lispm-tool-result desc (list file)))))

(lispm-deftool read_file
    ((path string "File path, absolute or relative to your root directory, or a place [[file:PATH::LINE]].")
     (offset integer "First line to return, 1-based (default: the place's line, else 1)." :optional t)
     (limit integer "Maximum number of lines (default 2000)." :optional t))
  "Read a file: a text file's lines come prefixed with their numbers;
an image (png, jpg, gif, webp) you see; a video (mp4, mov, webm, mkv)
you see as frames taken across it.  If a text file is open in Emacs,
you get the buffer's current contents, unsaved edits included: what
the user sees.  Other agents name places as org links,
[[file:PATH::LINE]]: pass one as the path to read there."
  :permission read
  :spotlight (lambda (_result path &optional offset limit)
               (let ((file (lispm-tools-file path)))
                 (unless (or (lispm-media-kind file) (file-directory-p file))
                   (list :kind 'read-file
                         :file file
                         :start-line (or offset (cdr (lispm-tools-reference path)) 1)
                         :limit (or limit 2000)))))
  (let* ((file (lispm-tools-file path))
         (offset (or offset (cdr (lispm-tools-reference path))))
         (open (find-buffer-visiting file))
         (state (and open (lispm-tools-sync open))))
    (cond ((and (lispm-media-kind file) (file-readable-p file))
           (lispm-tools--media file))
          ((memq state '(current conflict held))
           ;; Out of step with its file, the buffer is what the user
           ;; sees, or what the runtime holds: say so.
           (let ((text (lispm-tools-buffer-text open)))
             (lispm-tools-record-revision lispm-current-agent file text)
             (concat (pcase state
                       ('conflict "[open in Emacs, unsaved changes; the file changed on disk too]\n")
                       ('held "[the session of an agent, as LispM holds it; the file changed on disk]\n")
                       (_ (if (buffer-modified-p open) "[open in Emacs, unsaved changes]\n" "")))
                     (lispm-tools-number-lines text offset limit))))
          ((file-directory-p file) (error "%s is a directory; use list_directory" path))
          ((file-readable-p file)
           (let ((text (lispm-file-text file)))
             (lispm-tools-record-revision lispm-current-agent file text)
             (lispm-tools-number-lines text offset limit)))
          (t (error "No such file: %s" file)))))

(lispm-deftool list_directory
    ((path string "Directory, absolute or relative to your root (default: the root)."
           :optional t))
  "List a directory's entries; subdirectories end with a slash."
  :permission read
  (let* ((dir (file-name-as-directory (lispm-tools-file (or path "."))))
         ;; With their attributes: on a remote directory, one round trip
         ;; rather than one per entry.
         (entries (directory-files-and-attributes dir nil directory-files-no-dot-files-regexp)))
    (concat (abbreviate-file-name dir) "\n"
            (mapconcat (lambda (entry)
                         (let ((type (file-attribute-type (cdr entry))))
                           (if (or (eq type t)
                                   ;; A symbolic link to a directory is one.
                                   (and (stringp type) (file-directory-p (expand-file-name (car entry) dir))))
                               (concat (car entry) "/")
                             (car entry))))
                       (seq-take entries 500)
                       "\n")
            (if (> (length entries) 500)
                (format "\n[... %d more]" (- (length entries) 500))
              ""))))

;;;; Searching
;;
;; search_files is Emacs-native: project.el lists the files (so a git
;; project's .gitignore applies), and Emacs regexps search them in
;; buffers, a file's own buffer when Emacs visits it, so unsaved edits
;; are found, as read_file shows them.  The search runs in slices
;; between which Emacs stays responsive.  Models write regexps in the
;; syntax of ripgrep and grep -E; `lispm-tools-emacs-regexp' translates.

(defcustom lispm-search-max-matches 200
  "Matching lines after which search_files stops."
  :type 'integer
  :group 'lispm)

(defcustom lispm-search-max-file-size (* 1024 1024)
  "Files larger than this many bytes are not searched."
  :type 'integer
  :group 'lispm)

(defconst lispm-tools--search-slice 0.03
  "Seconds search_files works before letting Emacs handle other events.")

(defun lispm-tools--bracket (pattern start)
  "Translate the bracket expression at START in PATTERN.
Return (EMACS-REGEXP . END), END being the position after it."
  (let ((i (1+ start)) (n (length pattern)) negated rbracket dash caret items)
    (when (and (< i n) (eq (aref pattern i) ?^)) (setq negated t) (cl-incf i))
    (when (and (< i n) (eq (aref pattern i) ?\])) (setq rbracket t) (cl-incf i))
    (catch 'end
      (while t
        (when (>= i n) (error "Unterminated [ in %s" pattern))
        (let ((c (aref pattern i)))
          (cond
           ((eq c ?\]) (cl-incf i) (throw 'end nil))
           ((and (eq c ?\[) (< (1+ i) n) (eq (aref pattern (1+ i)) ?:))
            (let ((close (string-search ":]" pattern (+ i 2))))
              (unless close (error "Unterminated [: in %s" pattern))
              (push (substring pattern i (+ close 2)) items)
              (setq i (+ close 2))))
           ((eq c ?\\)
            (when (>= (1+ i) n) (error "Unterminated [ in %s" pattern))
            (let ((d (aref pattern (1+ i))))
              (setq i (+ i 2))
              (pcase d
                (?d (push "0-9" items))
                (?s (push "[:space:]" items))
                (?w (push "[:alnum:]_" items))
                (?t (push "\t" items))
                (?n (push "\n" items))
                (?\] (setq rbracket t))
                (?- (setq dash t))
                (?^ (setq caret t))
                ((or ?D ?S ?W) (error "\\%c inside [...] is not supported" d))
                (_ (push (string d) items)))))
           (t (push (string c) items) (cl-incf i))))))
    (cons (if (and caret (not negated) (not rbracket) (not dash) (null items))
              "\\^"
            (concat "[" (if negated "^" "") (if rbracket "]" "")
                    (apply #'concat (nreverse items))
                    (if caret "^" "") (if dash "-" "") "]"))
          i)))

(defun lispm-tools-emacs-regexp (pattern)
  "PATTERN, a regexp in the syntax of ripgrep and grep -E, as an Emacs regexp.
Groups, alternation and intervals lose their backslashes' meaning in
the translation; \\d, \\s and \\w become character classes.  Signal an
error for what Emacs regexps cannot express, such as lookaround."
  (let ((i 0) (n (length pattern)) out)
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((eq c ?\\)
          (when (= (1+ i) n) (error "Trailing backslash in %s" pattern))
          (let ((d (aref pattern (1+ i))))
            (setq i (+ i 2))
            (push (pcase d
                    (?d "[0-9]") (?D "[^0-9]")
                    (?s "[[:space:]]") (?S "[^[:space:]]")
                    (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?b "\\b") (?B "\\B") (?< "\\<") (?> "\\>")
                    (?t "\t") (?n "\n") (?r "\r")
                    ((pred (lambda (d) (<= ?1 d ?9))) (string ?\\ d))
                    (_ (regexp-quote (string d))))
                  out)))
         ((eq c ?\()
          (cond ((eq (string-match "(\\?:" pattern i) i)
                 (push "\\(?:" out) (setq i (+ i 3)))
                ((eq (string-match "(\\?" pattern i) i)
                 (error "(?...) groups other than (?:...), such as lookaround, are not supported: %s"
                        pattern))
                (t (push "\\(" out) (cl-incf i))))
         ((eq c ?\)) (push "\\)" out) (cl-incf i))
         ((eq c ?|) (push "\\|" out) (cl-incf i))
         ((and (eq c ?{) (eq (string-match "{\\([0-9]*\\)\\(,[0-9]*\\)?}" pattern i) i)
               (string-match-p "[0-9]" (match-string 0 pattern)))
          (push (concat "\\{" (match-string 1 pattern) (or (match-string 2 pattern) "") "\\}") out)
          (setq i (match-end 0)))
         ((eq c ?\[)
          (pcase-let ((`(,bracket . ,end) (lispm-tools--bracket pattern i)))
            (push bracket out)
            (setq i end)))
         (t (push (string c) out) (cl-incf i)))))
    (apply #'concat (nreverse out))))

(defun lispm-tools--ere-regexp (pattern)
  "Translate PATTERN (ripgrep / grep -E syntax) to portable POSIX ERE."
  (let ((i 0) (n (length pattern)) out)
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((eq c ?\\)
          (when (= (1+ i) n) (error "Trailing backslash in %s" pattern))
          (let ((d (aref pattern (1+ i))))
            (setq i (+ i 2))
            (push (pcase d
                    (?d "[0-9]") (?D "[^0-9]")
                    (?s "[[:space:]]") (?S "[^[:space:]]")
                    (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?t "\t") (?n "\n") (?r "\r")
                    (_ (concat "\\" (string d))))
                  out)))
         ((eq c ?\()
          (cond ((eq (string-match "(\\?:" pattern i) i)
                 (push "(" out) (setq i (+ i 3)))
                ((eq (string-match "(\\?" pattern i) i)
                 (error "(?...) groups other than (?:...), such as lookaround, are not supported: %s"
                        pattern))
                (t (push "(" out) (cl-incf i))))
         ((eq c ?\[)
          (pcase-let ((`(,bracket . ,end) (lispm-tools--bracket pattern i)))
            (push bracket out)
            (setq i end)))
         (t (push (string c) out) (cl-incf i)))))
    (apply #'concat (nreverse out))))

(defun lispm-tools--glob-body (glob)
  "GLOB as an unanchored Emacs regexp."
  (let ((i 0) (n (length glob)) out)
    (while (< i n)
      (let ((c (aref glob i)))
        (cond
         ((eq (string-search "**/" glob i) i) (push "\\(?:.*/\\)?" out) (setq i (+ i 3)))
         ((eq (string-search "**" glob i) i) (push ".*" out) (setq i (+ i 2)))
         ((eq c ?*) (push "[^/]*" out) (cl-incf i))
         ((eq c ??) (push "[^/]" out) (cl-incf i))
         ((and (eq c ?{) (string-search "}" glob i))
          (let ((close (string-search "}" glob i)))
            (push (concat "\\(?:"
                          (mapconcat #'lispm-tools--glob-body
                                     (split-string (substring glob (1+ i) close) ",")
                                     "\\|")
                          "\\)")
                  out)
            (setq i (1+ close))))
         ((and (eq c ?\[) (string-search "]" glob (1+ i)))
          (let ((close (string-search "]" glob (+ i 2))))
            (push (concat "[" (replace-regexp-in-string "\\`!" "^" (substring glob (1+ i) close)) "]")
                  out)
            (setq i (1+ close))))
         (t (push (regexp-quote (string c)) out) (cl-incf i)))))
    (apply #'concat (nreverse out))))

(defun lispm-tools--glob-predicate (glob directory)
  "Predicate on file names matching GLOB, a path glob under DIRECTORY.
A GLOB without a slash is matched against the file's name alone."
  (let ((regexp (concat "\\`" (lispm-tools--glob-body glob) "\\'")))
    (if (string-search "/" glob)
        (lambda (file) (string-match-p regexp (file-relative-name file directory)))
      (lambda (file) (string-match-p regexp (file-name-nondirectory file))))))

(defun lispm-tools--searchable-directory-p (directory)
  "Non-nil if a search outside a project should descend into DIRECTORY."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (not (or (string-prefix-p "." name)
             (member name vc-directory-exclusion-list)
             (member name '("node_modules" "vendor" "dist" "build" "target" "venv" "__pycache__"))))))

(defun lispm-tools--remote-search-script ()
  "Return the remote POSIX shell script used by `search_files'."
  (concat
   "list_files() {\n"
   "  git -c safe.directory='*' ls-files -z -co --exclude-standard 2>/dev/null ||\n"
   "  find . \\( -name '.?*' -o -name CVS -o -name RCS -o -name SCCS -o -name _darcs"
   " -o -name '{arch}' -o -name node_modules -o -name vendor -o -name dist"
   " -o -name build -o -name target -o -name venv -o -name __pycache__ \\)"
   " -prune -o -type f -size -" (number-to-string (1+ lispm-search-max-file-size)) "c -print0 2>/dev/null\n"
   "}\n"
   "if [ -n \"$2\" ]; then\n"
   "  grep \"$2\" -E -- \"$1\" /dev/null >/dev/null 2>&1\n"
   "else\n"
   "  grep -E -- \"$1\" /dev/null >/dev/null 2>&1\n"
   "fi\n"
   "if [ $? -lt 2 ]; then\n"
   "  if [ -n \"$2\" ]; then\n"
   "    list_files | xargs -0 grep \"$2\" -IlE -- \"$1\" /dev/null 2>/dev/null || true\n"
   "  else\n"
   "    list_files | xargs -0 grep -IlE -- \"$1\" /dev/null 2>/dev/null || true\n"
   "  fi\n"
   "else\n"
   "  list_files | tr '\\0' '\\n'\n"
   "fi\n"))

(defun lispm-tools--parse-remote-search-output (dir rel-files)
  "Filter REL-FILES under remote DIR and merge open matching buffers."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (rel rel-files)
      (unless (equal rel "/dev/null")
        (let* ((clean (string-remove-prefix "./" rel))
               (parts (split-string clean "/" t)))
          (unless (seq-some (lambda (p)
                              (or (string-prefix-p "." p)
                                  (member p vc-directory-exclusion-list)
                                  (member p '("node_modules" "vendor" "dist" "build"
                                              "target" "venv" "__pycache__"))))
                            (butlast parts))
            (let ((full (expand-file-name clean dir)))
              (unless (gethash full seen)
                (puthash full t seen)
                (push full out)))))))
    (dolist (b (buffer-list))
      (when-let* ((f (buffer-file-name b))
                  (full (expand-file-name f)))
        (when (and (file-in-directory-p full dir)
                   (not (gethash full seen)))
          (puthash full t seen)
          (push full out))))
    (nreverse out)))

(defun lispm-tools--remote-search-files (target ere case-fold)
  "Return remote files under directory TARGET matching ERE.
Uses a single remote `process-file' invocation (`git ls-files' or `find'
piped to `grep -IlE'), plus any open buffers visiting files under TARGET."
  (let* ((dir (file-name-as-directory target))
         (default-directory dir)
         (flag (if case-fold "-i" ""))
         (script (lispm-tools--remote-search-script))
         (rel-files
          (with-temp-buffer
            (when (ignore-errors
                    (process-file "sh" nil t nil "-c" script "sh" ere flag)
                    t)
              (split-string (buffer-string) "\n" t)))))
    (lispm-tools--parse-remote-search-output dir rel-files)))

(defun lispm-tools--remote-search-files-async (target ere case-fold callback)
  "List candidate remote files under TARGET matching ERE without blocking.
Calls CALLBACK with the list of file paths (falling back to `(list DIR)')."
  (let* ((dir (file-name-as-directory target))
         (default-directory dir)
         (flag (if case-fold "-i" ""))
         (script (lispm-tools--remote-search-script))
         (buf (generate-new-buffer " *lispm-remote-search*" t))
         (started nil))
    (unwind-protect
        (condition-case nil
            (let ((proc
                   (make-process
                    :name "lispm-remote-search"
                    :buffer buf
                    :command (list "sh" "-c" script "sh" ere flag)
                    :file-handler t
                    :connection-type 'pipe
                    :noquery t
                    :sentinel
                    (lambda (p _event)
                      (when (and (not (process-live-p p)) (buffer-live-p buf))
                        (let ((exit (and (eq (process-status p) 'exit)
                                         (process-exit-status p)))
                              (output (with-current-buffer buf (buffer-string))))
                          (kill-buffer buf)
                          (if (and exit (< exit 2))
                              (funcall callback
                                       (lispm-tools--parse-remote-search-output
                                        dir (split-string output "\n" t)))
                            (funcall callback (list dir)))))))))
              (setq started t)
              (when (and (not (process-live-p proc)) (buffer-live-p buf))
                (funcall (process-sentinel proc) proc "finished\n")))
          (error
           (setq started nil)
           (run-at-time 0 nil callback (list dir))))
      (unless started
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun lispm-tools--search-files (target glob &optional ere case-fold)
  "The files a search of TARGET, a file or directory, goes through.
Those of the project around TARGET (as `project-files' lists them),
else a directory entry for TARGET that `lispm-tools--search' expands in
slices; only those matching GLOB, if given."
  (let ((files (cond ((file-regular-p target) (list target))
                     ((not (file-directory-p target))
                      (error "No such file or directory: %s" target))
                     ((and (file-remote-p target) ere)
                      (lispm-tools--remote-search-files target ere case-fold))
                     ((let ((process-environment (lispm-git-environment)))
                        (when-let* ((project (project-current nil target))
                                    ((and (eq (car-safe project) 'vc)
                                          (cadr project)
                                          (file-in-directory-p target (project-root project)))))
                          ;; The project's backend may not work here, such as
                          ;; a checkout of a VCS that is not installed.
                          (ignore-errors (project-files project (list target))))))
                     (t (list (file-name-as-directory target))))))
    (if (and glob (not (and (car files) (null (cdr files)) (directory-name-p (car files)))))
        (seq-filter (lispm-tools--glob-predicate
                     glob (if (file-directory-p target) target (file-name-directory target)))
                    files)
      files)))

(defun lispm-tools--matches (regexp label limit)
  "Lines of the current buffer matching REGEXP, at most LIMIT.
Each is a string LABEL:LINE:TEXT, TEXT cut at 300 characters."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((line 1) (bol (point-min)) matches)
        (while (and (< (length matches) limit)
                    (not (eobp))
                    (re-search-forward regexp nil t))
          (let ((start (line-beginning-position)))
            (setq line (+ line (count-lines bol start))
                  bol start)
            (push (format "%s:%d:%s" label line
                          (truncate-string-to-width
                           (buffer-substring-no-properties start (line-end-position)) 300 nil nil "…"))
                  matches)
            (forward-line 1)))
        (nreverse matches)))))

(defun lispm-tools--relative-labeler (root)
  "Return a curried function mapping a file path to its display label under ROOT."
  (lambda (file)
    (if (file-in-directory-p file root)
        (file-relative-name file root)
      (abbreviate-file-name file))))

(defun lispm-tools--format-search-matches (matches count large truncated)
  "Format search MATCHES, COUNT, LARGE skipped count and TRUNCATED flag."
  (if (null matches)
      (concat "No matches."
              (if (> large 0)
                  (format " (%d files over %d bytes were not searched)"
                          large lispm-search-max-file-size)
                ""))
    (concat (string-join (nreverse matches) "\n")
            (if truncated
                (format "\n[stopped after %d matches; narrow the search with path or glob]"
                        count)
              "")
            (if (> large 0)
                (format "\n[%d files over %d bytes were not searched]"
                        large lispm-search-max-file-size)
              ""))))

(defun lispm-tools--file-searcher (regexp case-fold visiting buffer label-fn)
  "Return a curried function (FILE LIMIT) searching FILE for REGEXP.
Returns `large' when FILE exceeds `lispm-search-max-file-size' and is
non-binary, or the list of match lines (at most LIMIT)."
  (lambda (file limit)
    (let ((open (gethash file visiting))
          (label (funcall label-fn file)))
      ;; A buffer is searched for its unsaved changes, unless it
      ;; is out of date: then the file is.
      (if (and (buffer-live-p open) (lispm-buffer-current-p open))
          (with-current-buffer open
            (let ((case-fold-search case-fold))
              (lispm-tools--matches regexp label limit)))
        (with-current-buffer buffer
          (let* ((size (or (file-attribute-size (file-attributes file)) 0))
                 (small (<= size lispm-search-max-file-size))
                 ;; Read files as they are, no decompression or
                 ;; decryption, and as UTF-8: detecting each file's
                 ;; coding system would double the time.  A remote
                 ;; file is read through TRAMP's handler still.
                 (file-name-handler-alist (and (file-remote-p file) file-name-handler-alist))
                 (inhibit-file-name-handlers (append '(jka-compr-handler epa-file-handler)
                                                     inhibit-file-name-handlers))
                 (inhibit-file-name-operation 'insert-file-contents)
                 (coding-system-for-read 'utf-8))
            (erase-buffer)
            ;; Binary files are skipped; of a large one only the
            ;; head is read, to tell whether it is.
            (when (and (condition-case nil
                           (progn
                             (insert-file-contents file nil 0 (if small size 8000))
                             t)
                         (file-error nil))
                       (not (save-excursion
                              (goto-char (point-min))
                              (search-forward "\0" (min (point-max) 8000) t))))
              (if (not small)
                  'large
                (let ((case-fold-search case-fold))
                  (lispm-tools--matches regexp label limit))))))))))

(defun lispm-tools--search (agent regexp case-fold files done &optional glob-predicate)
  "Search FILES for REGEXP in slices, and call DONE with what is found.
Directory entries in FILES (ending in `/') are expanded one directory at
a time within the slices, descending into subdirectories that satisfy
`lispm-tools--searchable-directory-p' and keeping files that satisfy
GLOB-PREDICATE, if given.  CASE-FOLD non-nil ignores case.  The search
is abandoned if the turn of AGENT in which it started ends meanwhile."
  (let* ((label-fn (lispm-tools--relative-labeler (lispm-tools-root)))
         (turn (and agent (lispm-agent-busy-p agent) (lispm-agent-turn agent)))
         (buffer (generate-new-buffer " *lispm-search*" t))
         (visiting (make-hash-table :test #'equal))
         (search-file (lispm-tools--file-searcher regexp case-fold visiting buffer label-fn))
         (matches nil) (count 0) (large 0) (truncated nil))
    (dolist (b (buffer-list))
      (when-let* ((file (buffer-file-name b)))
        (puthash (expand-file-name file) b visiting)))
    (cl-labels
        ((expand-directory (dir)
           (dolist (entry (nreverse (condition-case nil
                                        (directory-files-and-attributes
                                         dir t directory-files-no-dot-files-regexp)
                                      (file-error nil))))
             (let ((path (car entry))
                   (type (file-attribute-type (cdr entry))))
               (cond ((eq type t)
                      (when (lispm-tools--searchable-directory-p path)
                        (push (file-name-as-directory path) files)))
                     ((or (null type) (and (stringp type) (file-regular-p path)))
                      (when (or (null glob-predicate) (funcall glob-predicate path))
                        (push path files)))))))
         (step ()
           (condition-case err
               (if (and turn (or (not (lispm-agent-busy-p agent))
                                 (/= turn (lispm-agent-turn agent))))
                   (kill-buffer buffer)
                 (let ((deadline (+ (float-time) lispm-tools--search-slice)))
                   ;; At least one file per slice, however short the slice.
                   (while (progn
                            (when-let* ((next (pop files)))
                              (if (directory-name-p next)
                                  (expand-directory next)
                                (let ((found (funcall search-file next (- lispm-search-max-matches count))))
                                  (if (eq found 'large)
                                      (cl-incf large)
                                    (setq matches (nconc (nreverse found) matches)
                                          count (+ count (length found))
                                          truncated (>= count lispm-search-max-matches))))))
                            (and files (not truncated) (< (float-time) deadline))))
                   (if (and files (not truncated))
                       (run-at-time 0 nil #'step)
                     (kill-buffer buffer)
                     (funcall done (lispm-tools--format-search-matches
                                    matches count large truncated)))))
             (error (when (buffer-live-p buffer) (kill-buffer buffer))
                    (funcall done (error-message-string err) t)))))
      (step))))

(declare-function lispm-kernel-search-files "lispm-kernel-exec")
(declare-function lispm-kernel-exec--submit "lispm-kernel-exec")
(declare-function lispm-kernel-exec--runtime "lispm-kernel-exec")
(defvar lispm-kernel-exec-mode)

(defun lispm-tools--search-via-kernel (agent regexp clean-pat case-fold files glob glob-dir
                                       glob-predicate done)
  "Search FILES for CLEAN-PAT via `lispm-kernel-search-files'.
Return non-nil when the kernel took the search, or nil to fall back."
  (when (and (fboundp 'lispm-kernel-search-files)
             (consp files)
             (stringp (car files))
             (not (string-search "\\n" clean-pat)))
    (let ((host (file-remote-p (car files))))
      (when (cl-every (lambda (f) (and (stringp f) (equal (file-remote-p f) host))) files)
        (let ((label-fn (lispm-tools--relative-labeler (lispm-tools-root)))
              (turn (and agent (lispm-agent-busy-p agent) (lispm-agent-turn agent)))
              (visiting (make-hash-table :test #'equal))
              (open-files nil))
          (dolist (b (buffer-list))
            (when-let* ((file (buffer-file-name b))
                        (full (expand-file-name file)))
              (when (and (equal (file-remote-p full) host)
                         (lispm-buffer-current-p b))
                (puthash full b visiting)
                (push full open-files))))
          (lispm-kernel-search-files
           files clean-pat case-fold
           (lambda (res)
             (if (plist-get res :error)
                 (lispm-tools--search agent regexp case-fold files done glob-predicate)
               (unless (and turn (or (not (lispm-agent-busy-p agent))
                                     (/= turn (lispm-agent-turn agent))))
                 (condition-case err
                     (let ((matches nil)
                           (count 0)
                           (large 0)
                           (truncated nil))
                       (dolist (item (plist-get res :items))
                         (unless truncated
                           (let* ((file (plist-get item :file))
                                  (label (and file (funcall label-fn file)))
                                  (limit (- lispm-search-max-matches count)))
                             (cond
                              ((plist-get item :large)
                               (cl-incf large))
                              ((plist-get item :open)
                               (let ((open (gethash file visiting)))
                                 (when (and (buffer-live-p open) (lispm-buffer-current-p open))
                                   (let ((found (with-current-buffer open
                                                  (let ((case-fold-search case-fold))
                                                    (lispm-tools--matches regexp label limit)))))
                                     (setq matches (nconc (nreverse found) matches)
                                           count (+ count (length found))
                                           truncated (>= count lispm-search-max-matches))))))
                              (t
                               (dolist (m (plist-get item :matches))
                                 (unless truncated
                                   (push (format "%s:%d:%s" label (car m) (cadr m)) matches)
                                   (cl-incf count)
                                   (when (>= count lispm-search-max-matches)
                                     (setq truncated t)))))))))
                       (funcall done (lispm-tools--format-search-matches
                                      matches count large truncated)))
                   (error (funcall done (error-message-string err) t))))))
           :glob glob
           :glob-dir (and glob-dir (or (file-remote-p glob-dir 'localname) glob-dir))
           :open-files open-files
           :max-file-size lispm-search-max-file-size
           :max-matches lispm-search-max-matches))))))

(lispm-deftool search_files
    ((pattern string "Regular expression, in the syntax of ripgrep or grep -E: (a|b), \\d, \\w, \\s, x{2,3}, [^...]; no lookaround.")
     (path string "File or directory to search (default: your root)." :optional t)
     (glob string "Only search files matching this glob, e.g. \"*.el\" or \"src/**/*.ts\"." :optional t)
     (ignore-case boolean "Match regardless of case." :optional t))
  "Search file contents; returns matching lines as path:line:text.
Searches the project's files (a git project's .gitignore applies) and
the unsaved contents of files open in Emacs."
  :async t
  :spotlight (lambda (result pattern &optional _path _glob _ignore-case)
               (list :kind 'search-files
                     :subject (string-remove-prefix "(?i)" pattern)
                     :result result))
  (let* ((agent lispm-current-agent)
         (case-fold (or ignore-case (string-prefix-p "(?i)" pattern)))
         (clean-pat (string-remove-prefix "(?i)" pattern))
         (regexp (lispm-tools-emacs-regexp clean-pat))
         (ere (lispm-tools--ere-regexp clean-pat))
         (target (lispm-tools-file (or path ".")))
         (remote-kernel (and (file-remote-p target)
                             (fboundp 'lispm-kernel-exec--runtime)
                             (lispm-kernel-exec--runtime :search-files target)))
         (remote-async (and (file-remote-p target)
                            (file-directory-p target)
                            ere
                            (not remote-kernel)))
         (files (cond
                 (remote-kernel (list target))
                 (remote-async nil)
                 (t (lispm-tools--search-files target glob ere case-fold))))
         (glob-dir (if (or remote-kernel (file-directory-p target))
                       target
                     (file-name-directory target)))
         (glob-predicate (and glob (lispm-tools--glob-predicate glob glob-dir))))
    ;; An invalid regexp fails here, not file after file.
    (string-match-p regexp "")
    (unless (and (not remote-async)
                 (lispm-tools--search-via-kernel
                  agent regexp clean-pat case-fold files glob glob-dir glob-predicate done))
      (if (and (file-remote-p target) (file-directory-p target) ere)
          (lispm-tools--remote-search-files-async
           target ere case-fold
           (lambda (found)
             (let ((filtered (if glob-predicate (seq-filter glob-predicate found) found)))
               (lispm-tools--search agent regexp case-fold filtered done glob-predicate))))
        (lispm-tools--search
         agent regexp case-fold
         (if remote-kernel (lispm-tools--search-files target glob ere case-fold) files)
         done glob-predicate)))))

(lispm-deftool list_buffers ()
  "List the buffers open in Emacs: name, major mode, file and whether modified.
Buffers are the user's workspace; this shows what they are working on."
  :permission read
  (mapconcat
   (lambda (b)
     (with-current-buffer b
       (format "%s\t%s\t%s%s" (buffer-name b) major-mode
               (if buffer-file-name (abbreviate-file-name buffer-file-name) "-")
               (if (and buffer-file-name (buffer-modified-p)) "\t[modified]" ""))))
   (seq-remove (lambda (b) (string-prefix-p " " (buffer-name b))) (buffer-list))
   "\n"))

(lispm-deftool read_buffer
    ((buffer string "Buffer name, as listed by list_buffers.")
     (offset integer "First line to return, 1-based." :optional t)
     (limit integer "Maximum lines to return, 1 to 2000 (default 2000)." :optional t)
     (tail integer "Search or read only the last N lines, 1 to 2000; excludes offset." :optional t)
     (query string "Literal, case-insensitive text to find; returns matching lines only." :optional t))
  "Read an Emacs buffer, e.g. *compilation*, *Messages* or a REPL.
Lines come prefixed with their original numbers. Use tail for recent process
logs and query to search buffer text without writing Elisp. A query searches
from offset to the end, or within the tail window; limit caps returned lines.
Reading preserves point, narrowing and unsaved edits. Output is bounded."
  :permission read
  :spotlight (lambda (_result buffer &optional offset limit tail query)
               (list :kind 'read-buffer
                     :buffer (lispm-tools-buffer buffer)
                     :start-line offset
                     :limit (or limit 2000)
                     :tail tail
                     :query query))
  (let ((b (lispm-tools-buffer buffer))
        (limit (or limit 2000)))
    (unless (and (integerp limit) (<= 1 limit 2000))
      (error "limit must be from 1 to 2000"))
    (when (and offset (< offset 1)) (error "offset must be positive"))
    (when (and tail (not (<= 1 tail 2000))) (error "tail must be from 1 to 2000"))
    (when (and tail offset) (error "Use tail or offset, not both"))
    (when (and query (string-empty-p query)) (error "query must not be empty"))
    (when (buffer-file-name b)
      (lispm-tools-record-revision lispm-current-agent (buffer-file-name b)
                                   (lispm-tools-buffer-text b)))
    (with-current-buffer b
      (save-excursion
        (save-restriction
          (widen)
          (let ((header (format "[%s, point at line %d]\n" major-mode (line-number-at-pos)))
                (room (max 0 (- lispm-tool-output-limit 200)))
                (count 0) lines truncated)
            (if tail
                (progn
                  (goto-char (point-max))
                  (when (and (bolp) (not (bobp))) (backward-char))
                  (forward-line (- 1 tail)))
              (goto-char (point-min))
              (forward-line (1- (or offset 1))))
            (let ((line (line-number-at-pos))
                  (case-fold-search t))
              (while (and (not (eobp)) (< count limit) (not truncated))
                (when (or (null query)
                          (save-excursion (search-forward query (line-end-position) t)))
                  (let* ((prefix (format "%6d\t" line))
                         (available (max 0 (- room (length prefix) 1)))
                         (end (min (line-end-position) (+ (point) available)))
                         (text (concat prefix (buffer-substring-no-properties (point) end))))
                    (setq truncated (< end (line-end-position)))
                    (push text lines)
                    (cl-incf count)
                    (cl-decf room (1+ (length text)))
                    (when (<= room 0) (setq truncated t))))
                (forward-line 1)
                (cl-incf line)))
            (concat header (string-join (nreverse lines) "\n")
                    (cond (truncated "\n[Buffer output truncated]")
                          ((and query (zerop count)) "[No matching lines in this range]")
                          ((and (= count limit) (not (eobp))) "\n[Line limit reached]")))))))))

(lispm-deftool describe_symbol
    ((name string "Name of an Emacs Lisp function, variable or face.")
     (buffer string "Inspect the variable's binding in this live buffer; otherwise its default value."
             :optional t))
  "Describe an Emacs Lisp symbol: function signature and documentation,
variable value and binding status, and the files its definitions were loaded
from. Pass buffer for buffer-local values. Use xref_definitions for source
locations, or runtime_status for loaded library paths and capabilities.
Nothing is loaded or evaluated; use this before writing live-editor Elisp."
  :permission read
  (let ((sym (intern-soft name))
        (context (and buffer (lispm-tools-buffer buffer))) parts)
    (unless sym (error "No symbol named %s" name))
    (when (fboundp sym)
      (push (format "Function %s %s\n%s" name
                    (or (help-function-arglist sym t) "()")
                    (or (ignore-errors (documentation sym t)) "Undocumented."))
            parts))
    (when (if context (with-current-buffer context (boundp sym)) (default-boundp sym))
      (push (format "Variable %s\nBinding: %s\nValue: %s\n%s" name
                    (if context
                        (format "%s in %s" (if (local-variable-p sym context) "buffer-local" "default")
                                (buffer-name context))
                      "default")
                    (lispm-truncate (let ((print-length 50) (print-level 5))
                                       (prin1-to-string (if context
                                                            (buffer-local-value sym context)
                                                          (default-value sym))))
                                     2000)
                    (or (documentation-property sym 'variable-documentation t)
                        "Undocumented."))
            parts))
    (when (facep sym)
      (push (format "Face %s\n%s" name (or (face-documentation sym) "")) parts))
    (dolist (kind '(defun defvar defface))
      (when-let* ((file (symbol-file sym kind)))
        (push (format "Loaded %s from: %s" kind file) parts)))
    (if parts
        (string-join (nreverse parts) "\n\n")
      (error "%s is neither a function, a variable nor a face" name))))

;;;; Built-in tools: writing

(lispm-deftool edit_file
    ((path string "File to edit, absolute or relative to your root.")
     (old-text string "Exact text to replace, with enough context to be unique.")
     (new-text string "Replacement text.")
     (replace-all boolean "Replace every occurrence instead of exactly one." :optional t)
     (expected-hash string "Optional SHA-256 content hash precondition; fails on conflict."
                    :optional t))
  "Replace text in a file.  The edit happens in the file's Emacs buffer
as one undoable step and is then saved.  old_text must match the
current contents exactly, so read the file first."
  :permission write
  :edit (:file path
         :transform (text (progn
                            (when expected-hash
                              (lispm-tools-check-cas
                               lispm-current-agent (lispm-tools-file path) text expected-hash nil))
                            (lispm-tools-replace text old-text new-text replace-all)))
         :bind (count label))
  (format "Replaced %d occurrence%s in %s." count (if (= count 1) "" "s") label))

(lispm-deftool write_file
    ((path string "File to create or overwrite, absolute or relative to your root.")
     (content string "The complete new contents.")
     (expected-hash string "Optional SHA-256 content hash precondition (or \"missing\"); fails on conflict."
                    :optional t))
  "Create a file, or replace a file's entire contents.  Prefer edit_file
for existing files; create new files only when required."
  :permission write
  :edit (:file path :create t
         :transform (text (progn
                            (lispm-tools-check-cas
                             lispm-current-agent (lispm-tools-file path) text expected-hash t)
                            (cons content nil)))
         :bind (_meta label existed))
  (format "%s %s." (if existed "Overwrote" "Created") label))

(lispm-deftool edit_buffer
    ((buffer string "Buffer name.")
     (old-text string "Exact text to replace, with enough context to be unique.")
     (new-text string "Replacement text.")
     (replace-all boolean "Replace every occurrence instead of exactly one." :optional t))
  "Replace text in an Emacs buffer that is not a file, such as *scratch*.
For files use edit_file."
  :permission write
  :edit (:buffer buffer
         :transform (text (lispm-tools-replace text old-text new-text replace-all))
         :bind (count label))
  (format "Replaced %d occurrence%s in %s." count (if (= count 1) "" "s") label))


(provide 'lispm-tool-builtins)

;;; lispm-tool-builtins.el ends here
