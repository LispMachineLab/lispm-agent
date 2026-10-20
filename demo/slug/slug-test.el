;;; slug-test.el --- Tests for slug.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'slug)

(ert-deftest slug-one-word ()
  (should (equal (slug-from-title "Lisp") "lisp")))

(ert-deftest slug-punctuation ()
  (should (equal (slug-from-title "Hello, World!") "hello-world")))

(ert-deftest slug-spaces ()
  (should (equal (slug-from-title "  Lisp   Machines  ") "lisp-machines")))

;;; slug-test.el ends here
