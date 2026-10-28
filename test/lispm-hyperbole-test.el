;;; lispm-hyperbole-test.el --- Tests of lispm-hyperbole.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; `make test' runs these in an Emacs of their own, after the others:
;; loading Hyperbole changes syntax tables, keys and Org for the whole
;; Emacs.  Without Hyperbole (`make deps' installs it) they are skipped.

;;; Code:

(require 'lispm-test)
(require 'hyperbole nil t)

(defun lispm-hyperbole-test--press (chat text key)
  "Press KEY in CHAT, shown in the selected window, on TEXT in the reply."
  (switch-to-buffer chat)
  (goto-char (point-min))
  (search-forward "Where?")
  (search-forward text)
  (goto-char (1+ (match-beginning 0)))
  (call-interactively (key-binding (kbd key))))

(ert-deftest lispm-hyperbole-test-keys ()
  "With Hyperbole mode on, M-RET on an object LispM presents acts on it,
RET where LispM presents nothing activates the implicit button Hyperbole
recognizes there, and M-RET in the chat's input starts a new line."
  (skip-unless (featurep 'hyperbole))
  (should (featurep 'lispm-hyperbole))
  (lispm-test-env
    (with-temp-file (expand-file-name "a.txt" project) (insert "one\ntwo\n"))
    (with-temp-file (expand-file-name "b.txt" project) (insert "three\n"))
    (let* ((a (expand-file-name "a.txt" project))
           (b (expand-file-name "b.txt" project))
           ;; A path Hyperbole reads and LispM does not: a variable in it.
           (process-environment (cons (concat "LISPM_TEST_PROJECT=" (directory-file-name project))
                                      process-environment))
           (agent (lispm-start 'assistant project))
           (chat (lispm-chat agent)))
      (setq lispm-test--scripts
            `((,(lispm-agent-name agent)
               ,(lispm-test--msg "end_turn" (lispm-test--text
                                             (format "See a.txt:2, %s and \"${LISPM_TEST_PROJECT}/b.txt\" there."
                                                     (lispm-agent-name agent)))))))
      (lispm-test--chat-type chat "Where?")
      (should (lispm-test--wait (lispm-test--idle-p agent)))
      (lispm-chat-redraw chat)
      (hyperbole-mode 1)
      (unwind-protect
          (progn
            (with-current-buffer chat
              ;; Hyperbole mode's M-RET comes before the chat's.
              (should (eq (key-binding (kbd "M-RET")) #'hkey-either)))
            ;; LispM's implicit button type, not Hyperbole's own for a
            ;; path and a line, acts on the place.
            (save-window-excursion
              (lispm-hyperbole-test--press chat "a.txt:2" "M-RET")
              (should (eq (hattr:get 'hbut:current 'categ) 'ibtypes::lispm-presentation))
              (should (equal (buffer-file-name) a))
              (should (= (line-number-at-pos) 2)))
            ;; An agent, which only LispM knows, is inspected.
            (save-window-excursion
              (lispm-hyperbole-test--press chat (symbol-name (lispm-agent-name agent)) "M-RET")
              (should (derived-mode-p 'lispm-inspect-mode))
              (should (eq lispm-inspect-agent agent)))
            (save-window-excursion
              (lispm-hyperbole-test--press chat "${LISPM_TEST_PROJECT}" "RET")
              (should (get-buffer-window (find-buffer-visiting b))))
            (with-current-buffer chat
              (goto-char (point-min))
              (search-forward "${LISPM_TEST_PROJECT}")
              (should-not (lispm-object-at (match-beginning 0)))
              (search-backward "assistant@")
              (should (equal (lispm-object-at) (list 'agent (symbol-name (lispm-agent-name agent))))))
            (kill-buffer (find-buffer-visiting b))
            ;; The inspector's RET is the same command.
            (save-window-excursion
              (with-temp-buffer
                (insert "\"${LISPM_TEST_PROJECT}/b.txt\"")
                (goto-char 3)
                (lispm-act-at-point)
                (should (find-buffer-visiting b))))
            (kill-buffer (find-buffer-visiting b))
            (save-window-excursion
              (switch-to-buffer chat)
              (lispm-chat-set-input "x")
              (goto-char (point-max))
              (call-interactively (key-binding (kbd "M-RET")))
              (should (equal (lispm-chat--input) "x\n"))
              (lispm-chat-set-input ""))
            ;; With Hyperbole mode off, RET there goes to the input, as before.
            (hyperbole-mode 0)
            (save-window-excursion
              (lispm-hyperbole-test--press chat "${LISPM_TEST_PROJECT}" "RET")
              (should (eq (current-buffer) chat))
              (should (= (point) (point-max)))
              (should-not (find-buffer-visiting b))))
        (hyperbole-mode 0)))))

(provide 'lispm-hyperbole-test)
;;; lispm-hyperbole-test.el ends here
