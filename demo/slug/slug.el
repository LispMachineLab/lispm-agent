;;; slug.el --- Turn titles into URL slugs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; The sample project of the LispM demo.

;;; Code:

(defun slug-from-title (title)
  "Turn TITLE into a URL slug: its words, lowercase, joined by dashes."
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" title)))

(provide 'slug)
;;; slug.el ends here
