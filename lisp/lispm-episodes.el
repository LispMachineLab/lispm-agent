;;; lispm-episodes.el --- What happened: summaries of past conversations  -*- lexical-binding: t; -*-

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

;; Memory holds what is known; episodes, what happened.  The summary a
;; session is handed over with (see `lispm-compact') is kept as an
;; episode, in the directory of the agent's own memory (its project's,
;; or the system's for an agent of the whole system):
;;
;;   episodes/2026-10-22.org  the summaries of a day, a heading each
;;   episodes/2026-W43.org    a week's, rolled up into one summary
;;   episodes/2026-10.org     a month's, rolled up from its weeks
;;
;; Once a week is over, its days are rolled up into it, and once a
;; month is over, its weeks: a model condenses them, at idle, and the
;; files rolled up are removed, in a commit, so that `git log' keeps
;; them.  So rollups wait while memory is not under git (`lispm-git').  A week belongs to the month of its Thursday, as in ISO 8601 it
;; belongs to the year of it.  Nothing else is lost either: a day's
;; summaries are those the continued sessions begin with.
;;
;; A new session of an agent of the whole system starts knowing what
;; happened lately: its system prompt carries the latest episode.

;;; Code:

(require 'seq)
(require 'lispm-core)
(require 'lispm-api)
(require 'lispm-memory)
(require 'lispm-runtime)

(defcustom lispm-episodes t
  "Whether the summaries sessions are handed over with are kept as episodes.
They are kept by day, and rolled up into weeks and months once those
are over (see lispm-episodes.el).  The system prompt of an agent of
the whole system carries the latest."
  :type 'boolean
  :group 'lispm)

(defconst lispm-episodes--roll-up-prompt
  "You condense the summaries of an AI agent's past conversations into one summary of %s, \
for the agent to remember that time by. Keep what may matter later: the user's goals and \
requests, the decisions made and why, what was done and how it turned out, what was left \
open; names, paths, values. Leave out what was transient. Reply with the summary only."
  "System prompt of the request rolling episodes up; %s names the period.")

(defvar lispm-episodes--rolling (make-hash-table :test #'equal)
  "Episode directories a rollup is being written for now.")

(defvar lispm-episodes--failed (make-hash-table :test #'equal)
  "Episode directories a rollup failed in, to the time to try again after.")

(defun lispm-episodes-directory (agent)
  "Directory of AGENT's episodes: those of its project, or of the system."
  (expand-file-name "episodes/" (lispm-home-directory agent)))

;;;; Periods

(defun lispm-episodes--kind (file)
  "What FILE of episodes covers: `day', `week' or `month'; nil if none."
  (let ((name (file-name-nondirectory file)))
    (cond ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'" name) 'day)
          ((string-match-p "\\`[0-9]\\{4\\}-W[0-9]\\{2\\}\\.org\\'" name) 'week)
          ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'" name) 'month))))

(defun lispm-episodes--files (agent kind)
  "AGENT's files of episodes of KIND, oldest first."
  (let ((dir (lispm-episodes-directory agent)))
    (and (file-directory-p dir)
         (seq-filter (lambda (file) (eq (lispm-episodes--kind file) kind))
                     (directory-files dir t "\\`[0-9].*\\.org\\'")))))

(defun lispm-episodes--noon (year month day)
  "Noon of the date YEAR-MONTH-DAY, local time."
  (encode-time (list 0 0 12 day month year nil -1 nil)))

(defun lispm-episodes--week (day)
  "The ISO week of DAY, a date such as \"2026-10-22\": \"2026-W43\"."
  (pcase-let ((`(,year ,month ,day) (mapcar #'string-to-number (split-string day "-"))))
    (format-time-string "%G-W%V" (lispm-episodes--noon year month day))))

(defun lispm-episodes--month (week)
  "The month WEEK, such as \"2026-W43\", belongs to: that of its Thursday."
  (pcase-let* ((`(,year ,n) (mapcar #'string-to-number (split-string week "-W")))
               ;; The 4th of January is in week 1.
               (jan4 (lispm-episodes--noon year 1 4))
               (weekday (string-to-number (format-time-string "%u" jan4))))
    (format-time-string "%Y-%m" (time-add jan4 (* 86400 (+ (- 4 weekday) (* 7 (1- n))))))))

(defun lispm-episodes--due (agent)
  "The rollups due in AGENT's episodes, the oldest first.
Each is (TARGET KIND . SOURCES): the name of the week or month that is
over, `week' or `month', and the files to roll up into it."
  (let ((this-week (format-time-string "%G-W%V"))
        (this-month (format-time-string "%Y-%m"))
        due)
    (pcase-dolist (`(,week . ,days)
                   (seq-group-by (lambda (file) (lispm-episodes--week (file-name-base file)))
                                 (lispm-episodes--files agent 'day)))
      (when (string< week this-week)
        (push (cons week (cons 'week days)) due)))
    (pcase-dolist (`(,month . ,weeks)
                   (seq-group-by (lambda (file) (lispm-episodes--month (file-name-base file)))
                                 (lispm-episodes--files agent 'week)))
      (when (string< month this-month)
        (push (cons month (cons 'month weeks)) due)))
    ;; Weeks first: rolled up, they may make a month due.
    (sort due (lambda (a b) (if (eq (cadr a) (cadr b))
                                (string< (car a) (car b))
                              (eq (cadr a) 'week))))))

;;;; Episodes

(defun lispm-episodes--entries (file)
  "The episodes in FILE, as `lispm-memory--entries' gives them."
  (with-temp-buffer
    (insert (lispm--file-text file))
    (delay-mode-hooks (org-mode))
    (lispm-memory--entries (current-buffer))))

(defun lispm-episodes-record (agent summary file)
  "Keep SUMMARY, the one AGENT's session in FILE was handed over with.
It joins the episodes of the day."
  (when lispm-episodes
    (let* ((dir (lispm-episodes-directory agent))
           (day (progn (make-directory dir t)
                       (expand-file-name (format-time-string "%Y-%m-%d.org") dir)))
           (buffer (lispm-memory--visit day (format-time-string "#+title: Episodes of %Y-%m-%d\n") t)))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (lispm-memory--entry-text
                     (format "%s %s" (format-time-string "%H:%M") (lispm-agent-name agent)) nil
                     `(("AGENT" . ,(format "%s" (lispm-agent-name agent)))
                       ("SESSION" . ,(and file (format "[[file:%s]]" (abbreviate-file-name file))))
                       ("CREATED" . ,(lispm-memory--stamp)))
                     summary)))))
      (lispm-memory--save buffer)
      (lispm-git-commit agent (format "%s: episode of %s" (lispm-agent-name agent)
                                      (file-name-base day))))))

(add-hook 'lispm-handover-functions #'lispm-episodes-record)

(defun lispm-episodes-latest (agent)
  "AGENT's latest episode, as (WHEN . SUMMARY), or nil.
That is the latest summary of a day, if any is left, else that of the
latest week, else that of the latest month."
  (seq-some (lambda (kind)
              (when-let* ((file (car (last (lispm-episodes--files agent kind))))
                          (entry (car (last (lispm-episodes--entries file)))))
                (cons (if (eq kind 'day)
                          (format "%s %s" (file-name-base file) (plist-get entry :title))
                        (file-name-base file))
                      (plist-get entry :body))))
            '(day week month)))

(defun lispm-episodes--prompt (agent)
  "System prompt section of an agent of the whole system: its latest episode.
Not that of a session it is handed over to, which begins with it."
  (when (and lispm-episodes (lispm-agent-system agent) (not (lispm-agent-compacting agent)))
    (when-let* ((latest (lispm-episodes-latest agent)))
      (format "# What happened lately\nThe latest summary of your earlier conversations (%s):\n%s"
              (car latest) (lispm--truncate (cdr latest) 12000)))))

;; After the memory titles.
(add-hook 'lispm-system-prompt-functions #'lispm-episodes--prompt t)

;;;; Rolling up

(defun lispm-episodes--maintain (agent _reply)
  "Once AGENT's turn has ended, roll its episodes up at idle, if any are due.
Not while memory is not under git: a rollup removes the files it
rolls up, which the commit keeps; nor for a while after a rollup
failed."
  (let ((dir (lispm-episodes-directory agent)))
    (when (and lispm-episodes (lispm-backend-name agent) (lispm-git-p)
               (not (time-less-p nil (gethash dir lispm-episodes--failed 0)))
               (lispm-episodes--due agent))
      (lispm-when-idle (cons 'episodes dir) #'lispm-episodes-roll-up agent))))

(add-hook 'lispm-turn-end-functions #'lispm-episodes--maintain)

(defun lispm-episodes--unsaved (files)
  "The first of FILES whose buffer has unsaved changes, or nil."
  (seq-find (lambda (file)
              (when-let* ((buffer (find-buffer-visiting file)))
                (buffer-modified-p buffer)))
            files))

(defun lispm-episodes-roll-up (agent)
  "Roll up the oldest of AGENT's weeks or months of episodes that are over.
A model condenses them; the next rollup due waits for the next idle
time.  Not while memory is not under git, nor once AGENT has ended.
See lispm-episodes.el."
  (let ((dir (lispm-episodes-directory agent))
        (due (car (lispm-episodes--due agent))))
    (when (and due (memq agent (lispm-agents)) (lispm-git-p)
               (not (gethash dir lispm-episodes--rolling)))
      (pcase-let* ((`(,target ,kind . ,sources) due)
                   (file (expand-file-name (concat target ".org") dir))
                   ;; A rollup written already, as when a day came late, is rolled up again.
                   (sources (if (file-exists-p file) (cons file sources) sources))
                   (failed (lambda (reason)
                             (remhash dir lispm-episodes--rolling)
                             (puthash dir (time-add nil lispm-memory--retry-after)
                                      lispm-episodes--failed)
                             (message "%s" (lispm-tr "LispM: could not roll up the episodes of %s: %s"
                                                     (abbreviate-file-name file) reason)))))
        (puthash dir t lispm-episodes--rolling)
        (condition-case err
            (let* ((texts (mapcar (lambda (source) (cons source (lispm--file-text source))) sources))
                   (text (mapconcat (lambda (source)
                                      (mapconcat (lambda (entry)
                                                   (let ((name (file-name-base source))
                                                         (title (plist-get entry :title)))
                                                     (format "## %s\n%s"
                                                             (if (equal title name) name
                                                               (concat name " " title))
                                                             (plist-get entry :body))))
                                                 (lispm-episodes--entries source) "\n\n"))
                                    sources "\n\n"))
                   (backend (lispm-agent-backend* agent))
                   (request (lispm-api-request
                             backend (or lispm-memory-model (lispm-agent-model* agent)) "low"
                             (format lispm-episodes--roll-up-prompt (format "the %s %s" kind target))
                             nil
                             `(((role . "user")
                                (content . [((type . "text") (text . ,text))]))))))
              (when-let* ((edited (lispm-episodes--unsaved sources)))
                (error "%s has unsaved changes" (abbreviate-file-name edited)))
              (funcall lispm-api-function backend (car request) (cdr request)
                       :on-done
                       (lambda (message)
                         (let ((summary (lispm--reply-text (append (alist-get 'content message) nil))))
                           (if (string-blank-p summary)
                               (funcall failed "the summary came back empty")
                             (condition-case err
                                 (progn
                                   (lispm-episodes--write agent file kind target texts summary)
                                   (remhash dir lispm-episodes--rolling)
                                   (when (lispm-episodes--due agent)
                                     (lispm-when-idle (cons 'episodes dir)
                                                      #'lispm-episodes-roll-up agent)))
                               (error (funcall failed (error-message-string err)))))))
                       :on-error
                       (lambda (err) (funcall failed (plist-get err :message)))))
          (error (funcall failed (error-message-string err))))))))

(defun lispm-episodes--write (agent file kind target sources summary)
  "Write SUMMARY of the KIND TARGET to FILE, and remove the SOURCES it rolls up.
SOURCES are (FILE . TEXT): the files rolled up, and what they held
when the summary was asked for.  If one holds something else now, or
its buffer has unsaved changes, that is an error, and nothing is
changed.  That is one commit, by AGENT."
  (let ((coding-system-for-write 'utf-8)
        (names (mapcar (lambda (source) (file-name-base (car source))) sources)))
    (when-let* ((edited (lispm-episodes--unsaved (mapcar #'car sources))))
      (error "%s has unsaved changes" (abbreviate-file-name edited)))
    (pcase-dolist (`(,source . ,text) sources)
      (unless (equal (lispm--file-text source) text)
        (error "%s changed meanwhile" (abbreviate-file-name source))))
    (write-region
     (concat (format "#+title: Episodes of the %s %s\n" kind target)
             (lispm-memory--entry-text
              target nil
              `(("CREATED" . ,(lispm-memory--stamp))
                ("SOURCES" . ,(string-join names " | ")))
              summary))
     nil file nil 'silent)
    (pcase-dolist (`(,source . ,_) sources)
      (unless (equal source file)
        (when-let* ((buffer (find-buffer-visiting source)))
          (kill-buffer buffer))
        (delete-file source)))
    (when-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (let ((inhibit-message t)) (revert-buffer t t t))))
    (lispm-git-commit agent (format "%s: roll up the episodes of %s (%s)"
                                    (lispm-agent-name agent) target
                                    (string-join (remove target names) ", ")))))

(provide 'lispm-episodes)
;;; lispm-episodes.el ends here
