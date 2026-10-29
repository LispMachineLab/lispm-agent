;;; lispm-runtime-tools.el --- Inspect the running image -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Small, read-only queries replace repeated eval_elisp probes of the
;; running image.  Discovery must not load the libraries it describes.

;;; Code:

(require 'lispm-tools)

(defconst lispm-runtime-tools--libraries
  '("lispm" "lispm-agents" "lispm-runtime" "lispm-tools" "lispm-elisp" "lispm-kernel")
  "Libraries inspected by runtime_status when none are named.")

(defun lispm-runtime-tools--library (name)
  "Describe library NAME without loading it or running its code."
  (let* ((pattern (concat "\\(?:\\`\\|/\\)" (regexp-quote name)
                          "\\(?:\\.elc?\\)?\\(?:\\.gz\\)?\\'"))
         (loaded (seq-find (lambda (entry)
                             (and (stringp (car entry))
                                  (string-match-p pattern (car entry))))
                           load-history))
         (located (locate-library name)))
    `((name . ,name)
      (loaded . ,(if loaded t :false))
      (loaded_file . ,(or (car loaded) :null))
      (available_file . ,(or located :null)))))

(lispm-deftool runtime_status
    ((libraries array "Library names to inspect, at most 20; defaults to LispM components."
                :items ((type . "string")) :optional t))
  "Inspect the running Emacs version, executable, PID, init file and LispM
library paths. Reports which libraries are loaded and which are available
on load-path, without loading them. Use this for runtime installation,
source provenance and missing capability diagnostics. A loaded file can
differ from the file found on load-path after an update. Use describe_symbol
for function availability or variable values, inspect_agent for LispM
permissions, approval policy, pending calls and agent state."
  :permission read
  :deferred t
  :keywords '("runtime" "Emacs version" "loaded libraries" "installation" "capabilities")
  (let ((names (if libraries (append libraries nil) lispm-runtime-tools--libraries)))
    (unless (and (<= (length names) 20)
                 (seq-every-p (lambda (name)
                                (and (stringp name)
                                     (string-match-p "\\`[[:alnum:]_-]+\\'" name)))
                              names))
      (error "libraries must contain at most 20 library names without paths or extensions"))
    (let* ((records (mapcar #'lispm-runtime-tools--library (seq-uniq names)))
           (data `((observed_at . ,(format-time-string "%FT%TZ" nil t))
                   (emacs_version . ,emacs-version)
                   (system_type . ,(symbol-name system-type))
                   (pid . ,(emacs-pid))
                   (executable . ,(expand-file-name invocation-name invocation-directory))
                   (init_file . ,(or user-init-file :null))
                   (registered_tools . ,(hash-table-count lispm-tools--registry))
                   (libraries_truncated . :false)
                   (libraries . ,(vconcat records))))
           (result (lispm--json-encode data)))
      (while (and records (> (length result) lispm-tool-output-limit))
        (setq records (butlast records))
        (setf (alist-get 'libraries data) (vconcat records)
              (alist-get 'libraries_truncated data) t)
        (setq result (lispm--json-encode data)))
      (when (> (length result) lispm-tool-output-limit)
        (error "Tool output limit is too small for runtime status"))
      result)))

(provide 'lispm-runtime-tools)
;;; lispm-runtime-tools.el ends here
