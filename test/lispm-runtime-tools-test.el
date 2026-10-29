;;; lispm-runtime-tools-test.el --- Runtime query tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'lispm-runtime-tools)

(ert-deftest lispm-runtime-tools-test-buffer-tail-search ()
  "Log queries preserve editor state and retain original line numbers."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project)))
      (with-temp-buffer
        (insert "older\nError [x]\nrecent\nERROR [x]\nlast\n")
        (goto-char 12)
        (narrow-to-region 7 27)
        (let* ((before (list (point) (point-min) (point-max) (buffer-modified-p)))
               (call (lambda (&rest args)
                       (lispm-test--call agent "read_buffer"
                                        (cons (cons 'buffer (buffer-name)) args)))))
          (let ((result (funcall call '(tail . 3) '(query . "error [x]"))))
            (should-not (cdr result))
            (should (string-search "4\tERROR [x]" (car result)))
            (should-not (string-search "2\tError" (car result))))
          (should (string-search "5\tlast" (car (funcall call '(tail . 1)))))
          (should (string-search "2\tError [x]" (car (funcall call '(offset . 2) '(limit . 1)))))
          (should (string-search "No matching lines" (car (funcall call '(query . ".*")))))
          (should (equal before (list (point) (point-min) (point-max) (buffer-modified-p))))
          (dolist (args '(((tail . 1) (offset . 2)) ((tail . 0)) ((offset . 0))
                          ((limit . -1)) ((limit . 2001)) ((query . ""))))
            (should (cdr (apply call args)))))))))

(ert-deftest lispm-runtime-tools-test-buffer-output-bound ()
  "A huge line is never copied whole or allowed to hide the tail."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (lispm-tool-output-limit 500))
      (with-temp-buffer
        (insert (make-string 10000 ?x) "\nlast without newline")
        (let ((result (lispm-test--call agent "read_buffer" `((buffer . ,(buffer-name))))))
          (should (< (length (car result)) 500))
          (should (string-search "Buffer output truncated" (car result))))
        (let ((result (lispm-test--call agent "read_buffer"
                                       `((buffer . ,(buffer-name)) (tail . 1)))))
          (should (string-search "2\tlast without newline" (car result))))
        (erase-buffer)
        (should-not (cdr (lispm-test--call agent "read_buffer"
                                          `((buffer . ,(buffer-name)) (tail . 2)))))))))

(ert-deftest lispm-runtime-tools-test-symbol-bindings-and-source ()
  "Symbol queries distinguish default/local values and loaded provenance."
  (lispm-test-env
    (let ((agent (lispm-defagent tester :root project))
          (file (expand-file-name "probe.el" project))
          (load-history (copy-sequence load-history)))
      (lispm-test--write file
                         ";;; -*- lexical-binding: t; -*-\n(defvar lispm-runtime-test-probe 1)\n(defun lispm-runtime-test-probe () 1)\n")
      (unwind-protect
          (progn
            (load file nil t t)
            (with-temp-buffer
              (set (make-local-variable 'lispm-runtime-test-probe) 9)
              (let ((default (lispm-test--call agent "describe_symbol" '((name . "lispm-runtime-test-probe"))))
                    (local (lispm-test--call agent "describe_symbol"
                                            `((name . "lispm-runtime-test-probe") (buffer . ,(buffer-name))))))
                (should (string-search "Binding: default\nValue: 1" (car default)))
                (should (string-search "Binding: buffer-local" (car local)))
                (should (string-search "Value: 9" (car local)))
                (should (string-search (concat "Loaded defvar from: " file) (car local)))
                (should (string-search (concat "Loaded defun from: " file) (car local))))
              (should (cdr (lispm-test--call agent "describe_symbol"
                                            '((name . "lispm-runtime-test-probe") (buffer . "no-such-buffer")))))))
        (makunbound 'lispm-runtime-test-probe)
        (fmakunbound 'lispm-runtime-test-probe)))))

(ert-deftest lispm-runtime-tools-test-no-load-and-provenance ()
  "Discovery distinguishes loaded and available copies without executing one."
  (lispm-test-env
    (let* ((agent (lispm-defagent tester :root project))
           (file (expand-file-name "lispm-test-runtime-probe.el" project))
           (load-path (cons project load-path))
           (load-history (cons '("/previous/lispm-test-runtime-probe.elc") load-history)))
      (lispm-test--write file "(error \"Discovery must not load me\")\n")
      (let* ((result (lispm-test--call agent "runtime_status"
                                     '((libraries . ["lispm-test-runtime-probe" "lispm-no-such-library"])))))
        (should-not (cdr result))
        (let* ((data (lispm--json-decode (car result)))
               (libs (append (alist-get 'libraries data) nil)))
          (should (equal (alist-get 'emacs_version data) emacs-version))
          (should (equal (alist-get 'pid data) (emacs-pid)))
          (should (equal (alist-get 'loaded_file (car libs)) "/previous/lispm-test-runtime-probe.elc"))
          (should (equal (alist-get 'available_file (car libs)) file))
          (should-not (eq (alist-get 'loaded (cadr libs)) t))))
      (dolist (input '(((libraries . ["../lispm"])) ((libraries . [1]))))
        (should (cdr (lispm-test--call agent "runtime_status" input))))
      (let* ((lispm-tool-output-limit 500)
             (result (lispm-test--call agent "runtime_status" nil)))
        (should-not (cdr result))
        (should (<= (length (car result)) 500))
        (should (eq (alist-get 'libraries_truncated (lispm--json-decode (car result))) t))))))

(provide 'lispm-runtime-tools-test)
;;; lispm-runtime-tools-test.el ends here
