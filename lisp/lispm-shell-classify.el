;;; lispm-shell-classify.el --- Classify read-only shell commands for tool policy  -*- lexical-binding: t; -*-

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

;; Conservative POSIX shell lexer and read-only command classifier
;; used to treat safe read-only `run_shell' invocations under the
;; `read' permission class.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-util)

;;;; Shell commands that only read

(defcustom lispm-shell-read-only-programs
  '(("ls" . t) ("dir" . t) ("tree" . t) ("pwd" . t) ("file" . t) ("stat" . t)
    ("du" . t) ("df" . t) ("cat" . t) ("head" . t) ("tail" . t) ("wc" . t)
    ("zcat" . t) ("gzcat" . t) ("zgrep" . t) ("sed" . lispm-shell--sed-reads-p)
    ("grep" . t) ("egrep" . t) ("fgrep" . t) ("rg" . lispm-shell--rg-reads-p) ("ag" . t)
    ("find" . lispm-shell--find-reads-p) ("fd" . lispm-shell--fd-reads-p)
    ("echo" . t) ("printf" . t) ("true" . t) ("false" . t) ("test" . t) ("[" . t)
    ("which" . t) ("whereis" . t) ("whatis" . t) ("type" . t) ("man" . lispm-shell--man-reads-p)
    ("basename" . t) ("dirname" . t) ("realpath" . t) ("readlink" . t)
    ("sort" . lispm-shell--sort-reads-p) ("uniq" . t) ("cut" . t) ("tr" . t) ("diff" . t) ("cmp" . t) ("comm" . t)
    ("column" . t) ("fold" . t) ("nl" . t) ("paste" . t) ("rev" . t) ("seq" . t) ("expr" . t)
    ("jq" . t) ("xxd" . t) ("od" . t) ("hexdump" . t) ("strings" . t) ("nm" . t)
    ("md5" . t) ("md5sum" . t) ("shasum" . t) ("sha1sum" . t) ("sha256sum" . t) ("cksum" . t)
    ("date" . t) ("cal" . t) ("uname" . t) ("hostname" . t) ("whoami" . t) ("id" . t)
    ("uptime" . t) ("printenv" . t) ("locale" . t) ("ps" . t) ("pgrep" . t) ("lsof" . t)
    ("sw_vers" . t) ("arch" . t) ("nproc" . t)
    ("cd" . lispm-shell--cd-reads-p)
    ("env" . lispm-shell--env-reads-p)
    ("git" . lispm-shell--git-reads-p)
    ("gh" . lispm-shell--gh-reads-p))
  "Programs whose commands only read, each with when: (PROGRAM . WHEN).
WHEN is t when every call of PROGRAM only reads, or a function of the
call's arguments, a list of strings, that returns non-nil when that
call does: `find' reads unless it is told to delete or run something.
A command made only of such calls, such as `git status', is of class
`read' (see `lispm-shell-read-only-p'): it runs as reading does,
unasked under the default policy, where other commands ask.  Programs
that run what they are given, such as sh, xargs, env with a command or
sudo, must not be here, nor any that writes: a reader of files that
can also write them, as sed does with -i or its w command, needs a
function.  A program
is named without a directory: a command naming one by its path asks."
  :type '(alist :key-type (string :tag "Program")
                :value-type (choice (const :tag "Always" t) (function :tag "When")))
  :group 'lispm)

(defun lispm-shell--scan-redirection (command i length)
  "Scan a shell redirection in COMMAND starting at I (of LENGTH).
Return the new index after the redirection target, or throw `unsure'."
  ;; At a <, > or &>, any descriptor before it dropped.
  (let ((start i))
    (while (and (< i length) (memq (aref command i) '(?< ?> ?&)))
      (cl-incf i))
    (let ((op (substring command start i)))
      (while (and (< i length) (memq (aref command i) '(?\s ?\t)))
        (cl-incf i))
      (let ((target-start i))
        (while (and (< i length)
                    (not (memq (aref command i) '(?\s ?\t ?\n ?| ?& ?\; ?< ?>))))
          (cl-incf i))
        (let ((target (substring command target-start i)))
          ;; Only literal targets: quoting or expansion can
          ;; hide a word boundary, including a later #.
          (when (seq-some (lambda (c)
                            (memq c '(?\\ ?' ?\" ?$ ?` ?* ?? ?~
                                      ?\( ?\) ?{ ?} ?\[ ?\])))
                          target)
            (throw 'unsure 'unsure))
          (pcase op
            ((or ">" ">>" "&>")
             (unless (equal target "/dev/null") (throw 'unsure 'unsure)))
            (">&" (unless (string-match-p "\\`[0-9]+\\'" target) (throw 'unsure 'unsure)))
            ("<" (when (or (string-empty-p target) (string-prefix-p "(" target))
                   (throw 'unsure 'unsure)))
            (_ (throw 'unsure 'unsure)))
          i)))))

(defun lispm-shell--scan-squote (command i length)
  "Scan single-quoted word part in COMMAND from I up to LENGTH.
Return (CHARS . NEXT-INDEX), CHARS reversed, or throw `unsure'."
  (let (chars closed)
    (while (and (< i length) (not closed))
      (let ((c (aref command i)))
        (if (eq c ?')
            (setq closed t)
          (push c chars)))
      (cl-incf i))
    (unless closed (throw 'unsure 'unsure))
    (cons chars i)))

(defun lispm-shell--scan-dquote (command i length)
  "Scan double-quoted word part in COMMAND from I up to LENGTH.
Return (CHARS . NEXT-INDEX), CHARS reversed, or throw `unsure'."
  (let (chars closed)
    (while (and (< i length) (not closed))
      (let ((c (aref command i)))
        (cond
         ((eq c ?\") (setq closed t) (cl-incf i))
         ((eq c ?\\)
          (cl-incf i)
          (when (< i length)
            (let ((next (aref command i)))
              (unless (eq next ?\n)
                (unless (memq next '(?$ ?` ?\" ?\\))
                  (push ?\\ chars))
                (push next chars))
              (cl-incf i))))
         ((eq c ?`) (throw 'unsure 'unsure))
         ((and (eq c ?$) (< (1+ i) length) (eq (aref command (1+ i)) ?\())
          (throw 'unsure 'unsure))
         (t (push c chars) (cl-incf i)))))
    (unless closed (throw 'unsure 'unsure))
    (cons chars i)))

(defun lispm-shell--valid-commands-p (commands)
  "Return COMMANDS when non-empty and free of leading env assignments."
  (when (or (null commands)
            (seq-some (lambda (w)
                        (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car w)))
                      commands))
    (throw 'unsure 'unsure))
  commands)

(defun lispm-shell--words (command)
  "COMMAND's simple commands, each a list of words, or `unsure'.
Words are split as the shell splits them, quotes and backslashes
undone, at spaces, newlines, |, ||, && and ;.  Comments end at the
next newline; escaped newlines elsewhere join lines.  Anything that
would have the shell do more than run programs on words is `unsure': a
redirection other than to /dev/null, from a file or of one descriptor
to another; a command or process substitution, backquotes included; a
subshell, group or function; a job put in the background; a here
document; an environment assignment before a program.  So
is an empty command."
  (let ((length (length command))
        (i 0)
        commands words word in-word)
    (cl-labels ((char () (and (< i length) (aref command i)))
                (end-word () (when in-word (push (apply #'string (nreverse word)) words))
                           (setq word nil in-word nil))
                (end-command () (end-word)
                             (when words (push (nreverse words) commands))
                             (setq words nil))
                (add (c) (push c word) (setq in-word t))
                (unsure () (throw 'unsure 'unsure)))
      (catch 'unsure
        (while (< i length)
          (let ((c (aref command i)))
            (cond
             ((memq c '(?' ?\"))
              (pcase-let ((`(,chars . ,next)
                           (if (eq c ?')
                               (lispm-shell--scan-squote command (1+ i) length)
                             (lispm-shell--scan-dquote command (1+ i) length))))
                (setq word (append chars word) in-word t i next)))
             ((eq c ?\\)
              (cl-incf i)
              (when (char) (unless (eq (char) ?\n) (add (char))) (cl-incf i)))
             ((memq c '(?\s ?\t)) (end-word) (cl-incf i))
             ((memq c '(?\n ?\;)) (end-command) (cl-incf i))
             ((eq c ?|)
              (end-command) (cl-incf i) (when (eq (char) ?|) (cl-incf i)))
             ((eq c ?&)
              (if (eq (and (< (1+ i) length) (aref command (1+ i))) ?&)
                  (progn (end-command) (cl-incf i 2))
                (if (eq (and (< (1+ i) length) (aref command (1+ i))) ?>)
                    (progn (end-word) (setq i (lispm-shell--scan-redirection command i length)))
                  (unsure))))
             ((memq c '(?< ?>))
              (if (and in-word word (seq-every-p (lambda (d) (<= ?0 d ?9)) word))
                  (setq word nil in-word nil)
                (end-word))
              (setq i (lispm-shell--scan-redirection command i length)))
             ((or (memq c '(?` ?\( ?\)))
                  (and (eq c ?$) (eq (and (< (1+ i) length) (aref command (1+ i))) ?\()))
              (unsure))
             ((and (eq c ?#) (not in-word))
              (while (and (char) (not (eq (char) ?\n))) (cl-incf i)))
             ((and (memq c '(?{ ?})) (not in-word)
                   (let ((next (and (< (1+ i) length) (aref command (1+ i)))))
                     (or (null next) (memq next '(?\s ?\t ?\n ?\;)))))
              (unsure))
             (t (add c) (cl-incf i)))))
        (end-command)
        (lispm-shell--valid-commands-p (nreverse commands))))))

(defun lispm-shell-read-only-p (command)
  "Non-nil if the shell COMMAND only reads, as far as can be told.
It does when it runs only programs of `lispm-shell-read-only-programs',
each as that says it only reads, joined by pipes, && || or ;, writing
nowhere but /dev/null, with nothing the shell would run or expand into
a command (see `lispm-shell--words').  Any doubt answers nil: a command
this cannot read is one that may write."
  (when (stringp command)
    (let ((commands (lispm-shell--words command)))
      (and (listp commands)
           (seq-every-p
            (pcase-lambda (`(,program . ,args))
              (and (not (string-search "/" program))
                   (let ((when (alist-get program lispm-shell-read-only-programs nil nil #'equal)))
                     (cond ((eq when t) t)
                           ((functionp when)
                            (condition-case nil (and (funcall when args) t) (error nil)))))))
            commands)))))

(defun lispm-shell--none-of (args options)
  "Non-nil if none of ARGS is one of OPTIONS, or starts with one taking a value."
  (not (seq-some (lambda (arg)
                   (seq-some (lambda (option)
                               (or (equal arg option)
                                   (and (string-prefix-p "--" option)
                                        (string-prefix-p (concat option "=") arg))))
                             options))
                 args)))

(defun lispm-shell--short-option-has-p (args chars)
  "Non-nil if any single-dash option in ARGS before `--' has a char in CHARS."
  (catch 'found
    (dolist (arg args)
      (when (equal arg "--")
        (throw 'found nil))
      (when (and (> (length arg) 1)
                 (eq (aref arg 0) ?-)
                 (not (eq (aref arg 1) ?-)))
        (dolist (ch chars)
          (when (seq-position arg ch 1)
            (throw 'found t)))))
    nil))

(defun lispm-shell--sort-reads-p (args)
  "Non-nil if sort with ARGS only reads: no output file or compress command."
  (and (lispm-shell--none-of args '("-o" "--output" "--compress-program"))
       (not (lispm-shell--short-option-has-p args '(?o)))))

(defun lispm-shell--man-reads-p (args)
  "Non-nil if man with ARGS only reads: no custom pager or command hook."
  (and (lispm-shell--none-of args '("-P" "--pager" "-H" "--html"
                                   "-p" "--preprocessor" "-l" "--local-file"
                                   "-T" "--troff-device"))
       (not (lispm-shell--short-option-has-p args '(?P ?H ?p ?l ?T)))))

(defun lispm-shell--rg-reads-p (args)
  "Non-nil if rg with ARGS only reads: no preprocessor command (`--pre')."
  (lispm-shell--none-of args '("--pre" "--pre-glob")))

(defun lispm-shell--cd-reads-p (args)
  "Non-nil for cd's ordinary directory change, with at most one operand.
Only -L, -P and -- are recognized; other shell-specific forms ask."
  (while (member (car args) '("-L" "-P")) (setq args (cdr args)))
  (let ((end-options (equal (car args) "--")))
    (when end-options (setq args (cdr args)))
    (and (<= (length args) 1)
         (or (null args) end-options (equal (car args) "-")
             (not (string-prefix-p "-" (car args)))))))

(defun lispm-shell--find-reads-p (args)
  "Non-nil if find with ARGS only reads: it neither deletes nor runs nor writes."
  (lispm-shell--none-of args '("-delete" "-exec" "-execdir" "-ok" "-okdir"
                               "-fprint" "-fprint0" "-fprintf" "-fls")))

(defun lispm-shell--fd-reads-p (args)
  "Non-nil if fd with ARGS only reads: it runs nothing it finds."
  (lispm-shell--none-of args '("-x" "--exec" "-X" "--exec-batch" "-l" "--list-details")))

(defun lispm-shell--env-reads-p (args)
  "Non-nil if env with ARGS only prints the environment: it runs nothing."
  (seq-every-p (lambda (arg) (member arg '("-0" "--null"))) args))

(defun lispm-shell--sed-reads-p (args)
  "Non-nil if sed with ARGS only reads: it edits no file in place, and its
scripts only print, delete, substitute and the like, never write a file
or run a command (see `lispm-shell--sed-script-reads-p')."
  (catch 'lispm-shell-unsure
    (let (scripts positionals (options t))
      (while args
        (let ((arg (pop args)))
          (cond ((or (not options) (equal arg "-") (not (string-prefix-p "-" arg)))
                 (push arg positionals))
                ((equal arg "--") (setq options nil))
                ((string-prefix-p "--" arg)
                 (cond ((string-prefix-p "--expression=" arg)
                        (push (substring arg (length "--expression=")) scripts))
                       ((equal arg "--expression") (push (pop args) scripts))
                       ((equal arg "--line-length") (pop args))
                       ((not (member arg '("--quiet" "--silent" "--regexp-extended" "--null-data"
                                           "--separate" "--unbuffered" "--debug" "--sandbox" "--posix"
                                           "--help" "--version")))
                        ;; --in-place, --file, or something unknown.
                        (throw 'lispm-shell-unsure nil))))
                (t
                 ;; Letters, each an option: -ne SCRIPT, -E, -i.
                 (let ((letters (string-to-list (substring arg 1))))
                   (while letters
                     (let ((letter (pop letters)))
                       (cond ((memq letter '(?n ?E ?r ?s ?u ?z ?a))) ; a: BSD's append mode
                             ((memq letter '(?e ?l))
                              ;; Takes a value: the rest of the word, or the next argument.
                              (let ((value (if letters (apply #'string letters) (pop args))))
                                (setq letters nil)
                                (when (eq letter ?e) (push value scripts))))
                             (t (throw 'lispm-shell-unsure nil))))))))))
      (when (and (null scripts) positionals)
        (push (car (last positionals)) scripts))
      (and scripts (seq-every-p #'lispm-shell--sed-script-reads-p scripts)))))

(defun lispm-shell--sed-script-reads-p (script)
  "Non-nil if the sed SCRIPT only reads: no w or W command, no e command, no
w or e flag on s.  Addresses, p, d, s, y, q, =, n, N, and the other
commands that touch only the stream are read; what this cannot parse
is not."
  (let ((length (length script)) (i 0)
        (digits '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
    (cl-labels ((char () (and (< i length) (aref script i)))
                (skip (chars) (while (and (char) (memq (char) chars)) (cl-incf i)))
                (to-eol () (while (and (char) (not (eq (char) ?\n))) (cl-incf i)))
                (delimited (delim)
                  ;; Past the text up to and including the unescaped DELIM.
                  (let (done)
                    (while (and (char) (not done))
                      (cond ((eq (char) ?\\) (cl-incf i 2))
                            ((eq (char) delim) (cl-incf i) (setq done t))
                            (t (cl-incf i))))
                    (unless done (throw 'lispm-shell-unsure nil))))
                (address ()
                  (cond ((null (char)) nil)
                        ((memq (char) digits) (skip digits) t)
                        ((eq (char) ?$) (cl-incf i) t)
                        ((eq (char) ?/) (cl-incf i) (delimited ?/) t)
                        ((eq (char) ?\\) (cl-incf i)
                         (let ((delim (char))) (cl-incf i) (delimited delim)) t))))
      (catch 'lispm-shell-unsure
        (while (< i length)
          (skip '(?\s ?\t ?\n ?\;))
          (when (char)
            ;; Addresses: one, or two apart by a comma, each maybe with ~N or +N.
            (when (address)
              (when (memq (char) '(?~ ?+)) (cl-incf i) (skip digits))
              (when (eq (char) ?,)
                (cl-incf i)
                (skip '(?\s))
                (unless (or (address) (and (memq (char) '(?~ ?+)) (progn (cl-incf i) t)))
                  (throw 'lispm-shell-unsure nil))
                (skip digits)))
            (skip '(?\s))
            (when (eq (char) ?!) (cl-incf i) (skip '(?\s)))
            (let ((command (char)))
              (unless command (throw 'lispm-shell-unsure nil))
              (cl-incf i)
              (pcase command
                ((or ?p ?P ?d ?D ?= ?n ?N ?h ?H ?g ?G ?x ?z ?F ?{ ?}))
                ((or ?q ?Q ?l) (skip (cons ?\s digits)))
                (?s (let ((delim (char)))
                      (unless delim (throw 'lispm-shell-unsure nil))
                      (cl-incf i)
                      (delimited delim)
                      (delimited delim)
                      ;; Flags: g, p, I, i, m, M, a number; never w or e.
                      (while (and (char) (not (memq (char) '(?\; ?\n ?} ?\s))))
                        (unless (or (memq (char) '(?g ?p ?I ?i ?m ?M)) (memq (char) digits))
                          (throw 'lispm-shell-unsure nil))
                        (cl-incf i))))
                (?y (let ((delim (char)))
                      (unless delim (throw 'lispm-shell-unsure nil))
                      (cl-incf i)
                      (delimited delim)
                      (delimited delim)))
                ;; Text to the end of the line: appended, inserted or read in,
                ;; a label, a comment.
                ((or ?a ?i ?c ?r ?R ?: ?#) (to-eol))
                ((or ?b ?t ?T) (while (and (char) (not (memq (char) '(?\; ?\n ?})))) (cl-incf i)))
                ;; w, W and e write a file or run a command; anything else is
                ;; not known to read.
                (_ (throw 'lispm-shell-unsure nil))))))
        t))))

(defun lispm-shell--positionals (args consuming)
  "The ARGS that are not options; CONSUMING are options that take the next ARG."
  (let (positionals)
    (while args
      (let ((arg (pop args)))
        (cond ((member arg consuming) (pop args))
              ((equal arg "--") (setq positionals (append (reverse args) positionals) args nil))
              ((string-prefix-p "-" arg))
              (t (push arg positionals)))))
    (nreverse positionals)))

(defconst lispm-shell--git-readers
  '("status" "log" "show" "blame" "annotate" "ls-files" "ls-tree" "ls-remote"
    "cat-file" "rev-parse" "rev-list" "describe" "shortlog" "merge-base" "name-rev"
    "count-objects" "check-ignore" "check-attr" "diff-tree" "diff-index" "diff-files"
    "for-each-ref" "show-ref" "show-branch" "var" "version" "whatchanged" "cherry"
    "range-diff" "help" "verify-commit" "verify-tag")
  "Git subcommands that only read, whatever their arguments.")

(defun lispm-shell--git-reads-p (args)
  "Non-nil if git with ARGS only reads.
Of its options before the subcommand, -C, -P, --no-pager, --no-optional-locks,
--git-dir= and --work-tree= are read with; any other, -c among them, which
can have a subcommand run a program, is not.  Subcommands of
`lispm-shell--git-readers' read; diff, grep, branch, tag, remote, config,
stash, reflog, worktree, submodule and notes read while their arguments ask."
  (let (subcommand)
    (while (and args (not subcommand))
      (let ((arg (pop args)))
        (cond ((equal arg "-C") (pop args))
              ((member arg '("-P" "--no-pager" "--no-optional-locks" "--help" "--version")))
              ((or (string-prefix-p "--git-dir=" arg) (string-prefix-p "--work-tree=" arg)))
              ((string-prefix-p "-" arg) (setq args nil subcommand 'unsure))
              (t (setq subcommand arg)))))
    (pcase subcommand
      ('nil t)
      ('unsure nil)
      ((pred (lambda (s) (member s lispm-shell--git-readers))) t)
      ("diff"
       (lispm-shell--none-of args '("--output" "--ext-diff" "--textconv")))
      ("grep"
       (and (lispm-shell--none-of args '("-O" "--open-files-in-pager"))
            (not (lispm-shell--short-option-has-p args '(?O)))))
      ("branch"
       (and (lispm-shell--none-of args '("-d" "-D" "-m" "-M" "-c" "-C" "-u" "-f" "--delete" "--move"
                                         "--copy" "--force" "--set-upstream-to" "--unset-upstream"
                                         "--edit-description" "--track" "--no-track"))
            (or (member "-l" args) (member "--list" args)
                (null (lispm-shell--positionals
                       args '("--contains" "--no-contains" "--merged" "--no-merged" "--points-at"
                              "--sort" "--format" "--color" "--abbrev"))))))
      ("tag"
       (and (lispm-shell--none-of args '("-d" "-D" "-f" "-a" "-s" "-u" "-m" "-F" "-e" "--delete"
                                         "--force" "--annotate" "--sign" "--local-user"
                                         "--message" "--file" "--edit"))
            (or (member "-l" args) (member "--list" args)
                (null (lispm-shell--positionals
                       args '("--contains" "--no-contains" "--merged" "--no-merged" "--points-at"
                              "--sort" "--format" "--color"))))))
      ("remote" (member (car (lispm-shell--positionals args nil)) '(nil "show" "get-url")))
      ("config"
       (and (lispm-shell--none-of args '("--unset" "--unset-all" "--add" "--replace-all" "-e" "--edit"
                                         "--rename-section" "--remove-section"))
            (let ((positionals (lispm-shell--positionals args '("-f" "--file" "--blob" "-t" "--type"
                                                                 "--default"))))
              (if (seq-some (lambda (arg) (member arg '("--get" "--get-all" "--get-regexp"
                                                        "--get-urlmatch" "-l" "--list"
                                                        "--get-color" "--get-colorbool")))
                            args)
                  (<= (length positionals) 2)
                (<= (length positionals) 1)))))
      ("stash" (member (car (lispm-shell--positionals args nil)) '("list" "show")))
      ("reflog" (member (car (lispm-shell--positionals args nil)) '(nil "show")))
      ("worktree" (equal (car (lispm-shell--positionals args nil)) "list"))
      ("submodule" (member (car (lispm-shell--positionals args nil)) '("status" "summary")))
      ("notes" (member (car (lispm-shell--positionals args nil)) '("show" "list")))
      (_ nil))))

(defconst lispm-shell--gh-readers
  '(("pr" "view" "list" "checks" "diff" "status")
    ("issue" "view" "list" "status")
    ("run" "list" "view")
    ("repo" "view" "list")
    ("release" "list" "view")
    ("auth" "status")
    ("label" "list")
    ("workflow" "list" "view")
    ("gist" "list" "view")
    ("search")
    ("status")
    ("api"))
  "GitHub CLI commands that only read: (COMMAND . SUBCOMMANDS), every
subcommand of a COMMAND with none.")

(defun lispm-shell--gh-api-reads-p (args)
  "Non-nil if `gh api' ARGS only use GET/HEAD and send no body fields."
  (catch 'write
    (let ((method nil)
          (tail args))
      (while tail
        (let ((arg (pop tail)))
          (cond
           ((equal arg "--") (setq tail nil))
           ((or (member arg '("-f" "-F" "--field" "--raw-field" "--input"))
                (string-prefix-p "-f" arg)
                (string-prefix-p "-F" arg)
                (string-prefix-p "--field=" arg)
                (string-prefix-p "--raw-field=" arg)
                (string-prefix-p "--input=" arg))
            (throw 'write nil))
           ((member arg '("-X" "--method"))
            (setq method (pop tail)))
           ((string-prefix-p "--method=" arg)
            (setq method (substring arg (length "--method="))))
           ((string-prefix-p "-X" arg)
            (setq method (substring arg 2))))))
      (member (and method (upcase method)) '(nil "GET" "HEAD")))))

(defun lispm-shell--gh-reads-p (args)
  "Non-nil if gh with ARGS only reads: a command of `lispm-shell--gh-readers',
not opening the browser (-w, --web), and api only with GET or HEAD and
nothing to send."
  (let ((positionals (lispm-shell--positionals args '("-R" "--repo" "-X" "--method"))))
    (and (lispm-shell--none-of args '("-w" "--web"))
         (when-let* ((readers (assoc (car positionals) lispm-shell--gh-readers)))
           (and (or (null (cdr readers)) (member (cadr positionals) (cdr readers)))
                (or (not (equal (car positionals) "api"))
                    (lispm-shell--gh-api-reads-p args)))))))


(provide 'lispm-shell-classify)

;;; lispm-shell-classify.el ends here
