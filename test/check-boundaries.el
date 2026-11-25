;;; check-boundaries.el --- Enforce strict 5-layer DAG boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Static architecture verifier for `LispMachineLab/lispm-agent'.
;; Invoked by `make check-layers' and tests in `lispm-layers-test.el' to verify:
;;   1. Every file in `lisp/*.el' is assigned to one of the 5 DAG layers and
;;      listed in `Makefile' ELS.
;;   2. There are zero `require' cycles across `lisp/*.el'.
;;   3. Every `(require ...)' and `(declare-function ...)' targeting an in-repo
;;      module points strictly downward in both layer number and `Makefile' order.
;;   4. Lower layers (Layers 0-2) contain no direct `lispm-chat--' or
;;      `lispm-agents-roster-' references.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst lispm-boundaries-layers
  '((0 . ("lispm-i18n" "lispm-exec" "lispm-local-http" "lispm-websocket"
          "lispm-agents-context"))
    (1 . ("lispm-core" "lispm-job" "lispm-sandbox" "lispm-services" "lispm-roles"
          "lispm-transcript" "lispm-anthropic" "lispm-openai" "lispm-api"
          "lispm-deftool-macros" "lispm-agents" "lispm-agents-mcp"
          "lispm-agents-ide" "lispm-agents-claude" "lispm-agents-codex"))
    (2 . ("lispm-tools" "lispm-elisp-worker" "lispm-elisp" "lispm-memory"
          "lispm-inbox" "lispm-journal" "lispm-runtime" "lispm-lisp"
          "lispm-lisp-macros" "lispm-collab" "lispm-mcp"))
    (3 . ("lispm-title" "lispm-episodes" "lispm-emacs" "lispm-develop"
          "lispm-skills" "lispm-characters" "lispm-character-rag"
          "lispm-character-comfyui" "lispm-agents-bridge" "lispm-mcp-server"
          "lispm-ground" "lispm-inspect" "lispm-agents-roster" "lispm-settings"
          "lispm-backend-form" "lispm-runtime-tools" "lispm-session-tools"
          "lispm-process-tools"))
    (4 . ("lispm-stage" "lispm-edits" "lispm-spotlight" "lispm-stage-runner"
          "lispm-shell-stage" "lispm-eval-stage" "lispm-chat"
          "lispm-agents-work" "lispm-hyperbole" "lispm-bar" "lispm-batch"
          "lispm" "lispm-terminal")))
  "Mapping from integer layer (0..4) to list of module base names.")

(defconst lispm-boundaries--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root containing `lisp/' and `Makefile'.")

(defun lispm-boundaries--layer-table ()
  "Return hash table mapping module name string -> integer layer."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry lispm-boundaries-layers)
      (dolist (mod (cdr entry))
        (puthash mod (car entry) table)))
    table))

(defun lispm-boundaries--makefile-els (root)
  "Extract ordered list of module base names from `Makefile' ELS in ROOT."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "Makefile" root))
    (let (in-els mods)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match "^ELS[ \t]*=[ \t]*\\(.*\\)" line)
            (setq in-els t
                  line (match-string 1 line)))
          (when in-els
            (let ((pos 0))
              (while (string-match "\\(lispm[a-z0-9-]*\\)\\.el" line pos)
                (push (match-string 1 line) mods)
                (setq pos (match-end 0))))
            (unless (string-suffix-p "\\" (string-trim-right line))
              (setq in-els nil))))
        (forward-line 1))
      (nreverse mods))))

(defun lispm-boundaries--scan-file (file)
  "Scan FILE and return a plist of :requires, :declares, and :upward refs."
  (let (requires declares upward-refs)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((table (make-syntax-table emacs-lisp-mode-syntax-table)))
        (modify-syntax-entry ?\; "<" table)
        (modify-syntax-entry ?\n ">" table)
        (with-syntax-table table
          (goto-char (point-min))
          (condition-case nil
              (cl-labels
                  ((walk (form)
                     (when (consp form)
                       (let ((head (car form)))
                         (cond
                          ((and (eq head 'require)
                                (consp (cdr form))
                                (consp (cadr form))
                                (eq (car (cadr form)) 'quote)
                                (symbolp (cadr (cadr form))))
                           (push (symbol-name (cadr (cadr form))) requires))
                          ((and (eq head 'declare-function)
                                (>= (length form) 3)
                                (stringp (nth 2 form)))
                           (push (cons (nth 1 form) (nth 2 form)) declares)))
                         (let ((tail form))
                           (while (consp tail)
                             (walk (car tail))
                             (setq tail (cdr tail))))))))
                (while (progn (skip-chars-forward " \t\n\r") (not (eobp)))
                  (walk (read (current-buffer)))))
            (end-of-file nil))))
      ;; Scan code lines for disallowed lower-layer UI references outside comments.
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (pos 0))
          (unless (string-match-p "^[ \t]*;" line)
            (while (string-match
                    "\\_<\\(lispm-chat--[a-z0-9-]+\\|lispm-agents-roster-[a-z0-9-]+\\)\\_>"
                    line pos)
              (let ((sym (match-string 1 line)))
                (unless (and (string-suffix-p "/lispm-agents.el" file)
                             (member sym '("lispm-agents-roster-notify"
                                           "lispm-agents-roster-change-functions")))
                  (push sym upward-refs)))
              (setq pos (match-end 0)))))
        (forward-line 1)))
    (list :requires (delete-dups (nreverse requires))
          :declares (delete-dups (nreverse declares))
          :upward (delete-dups (nreverse upward-refs)))))

(defun lispm-boundaries--find-cycles (mods edges)
  "Find all strongly connected components of size > 1 over MODS and EDGES."
  (let ((index 0)
        (indices (make-hash-table :test #'equal))
        (low (make-hash-table :test #'equal))
        (on-stack (make-hash-table :test #'equal))
        (stack nil)
        (sccs nil))
    (cl-labels
        ((strongconnect (v)
           (puthash v index indices)
           (puthash v index low)
           (cl-incf index)
           (push v stack)
           (puthash v t on-stack)
           (dolist (w (gethash v edges))
             (cond
              ((not (gethash w indices))
               (strongconnect w)
               (puthash v (min (gethash v low) (gethash w low)) low))
              ((gethash w on-stack)
               (puthash v (min (gethash v low) (gethash w indices)) low))))
           (when (eql (gethash v low) (gethash v indices))
             (let (comp done)
               (while (not done)
                 (let ((w (pop stack)))
                   (remhash w on-stack)
                   (push w comp)
                   (when (equal w v)
                     (setq done t))))
               (when (> (length comp) 1)
                 (push (sort comp #'string<) sccs))))))
      (dolist (v mods)
        (unless (gethash v indices)
          (strongconnect v))))
    (nreverse sccs)))

(defun lispm-check-boundaries-report (&optional root)
  "Check architecture layer boundaries under ROOT (default repo root).
Return plist (:ok BOOL :modules INT :errors LIST :cycles LIST)."
  (let* ((root (or root lispm-boundaries--root))
         (lisp-dir (expand-file-name "lisp" root))
         (layer-of (lispm-boundaries--layer-table))
         (els (lispm-boundaries--makefile-els root))
         (els-pos (make-hash-table :test #'equal))
         (disk-mods (mapcar (lambda (f) (file-name-sans-extension (file-name-nondirectory f)))
                            (sort (directory-files lisp-dir nil "\\`lispm.*\\.el\\'") #'string<)))
         (edges (make-hash-table :test #'equal))
         errors)
    (cl-loop for mod in els for i from 0 do (puthash mod i els-pos))
    ;; 1. Completeness against layer map and Makefile ELS.
    (dolist (mod disk-mods)
      (unless (gethash mod layer-of)
        (push (format "Missing layer assignment for lisp/%s.el" mod) errors))
      (unless (gethash mod els-pos)
        (push (format "Missing Makefile ELS entry for lisp/%s.el" mod) errors)))
    (maphash (lambda (mod _)
               (unless (member mod disk-mods)
                 (push (format "Stale layer assignment for non-existent lisp/%s.el" mod) errors)))
             layer-of)
    ;; 2. Scan files for require edges, declare-functions, and lower-layer upward refs.
    (dolist (mod disk-mods)
      (let* ((file (expand-file-name (concat mod ".el") lisp-dir))
             (scan (lispm-boundaries--scan-file file))
             (src-layer (gethash mod layer-of))
             (src-pos (gethash mod els-pos)))
        (dolist (req (plist-get scan :requires))
          (when (and (gethash req layer-of) (not (equal req mod)))
            (push req (gethash mod edges))
            (let ((dst-layer (gethash req layer-of))
                  (dst-pos (gethash req els-pos)))
              (when (and src-layer dst-layer (> dst-layer src-layer))
                (push (format "Layer violation: %s (L%d) requires %s (L%d)"
                              mod src-layer req dst-layer)
                      errors))
              (when (and src-pos dst-pos (>= dst-pos src-pos))
                (push (format "Backward require in Makefile ELS order: %s (%d) -> %s (%d)"
                              mod src-pos req dst-pos)
                      errors)))))
        (pcase-dolist (`(,sym . ,target) (plist-get scan :declares))
          (let ((dst (string-remove-prefix "ext:" (string-remove-suffix ".el" target))))
            (when (and (gethash dst layer-of) (not (equal dst mod)))
              (let ((dst-layer (gethash dst layer-of))
                    (dst-pos (gethash dst els-pos)))
                (when (and src-layer dst-layer (> dst-layer src-layer))
                  (push (format "Upward declare-function: %s (L%d) declares `%s' from %s (L%d)"
                                mod src-layer sym dst dst-layer)
                        errors))
                (when (and src-pos dst-pos (>= dst-pos src-pos))
                  (push (format "Backward declare-function in ELS order: %s (%d) -> `%s' (%s %d)"
                                mod src-pos sym dst dst-pos)
                        errors))))))
        (when (and src-layer (<= src-layer 2))
          (dolist (sym (plist-get scan :upward))
            (push (format "Upward UI reference in Layer %d (%s): %s" src-layer mod sym) errors)))))
    (let ((cycles (lispm-boundaries--find-cycles disk-mods edges)))
      (dolist (cycle cycles)
        (push (format "Require cycle detected: %s" (string-join cycle " <-> ")) errors))
      (list :ok (null errors)
            :modules (length disk-mods)
            :cycles cycles
            :errors (nreverse errors)))))

(defun lispm-check-boundaries-batch-and-exit ()
  "Verify layer boundaries and exit Emacs with status 0 or 1."
  (let* ((report (lispm-check-boundaries-report))
         (ok (plist-get report :ok))
         (mods (plist-get report :modules))
         (cycles (plist-get report :cycles))
         (errors (plist-get report :errors)))
    (if ok
        (progn
          (princ (format "check-layers: OK (%d modules, %d layers, 0 cycles, 0 violations)\n"
                         mods (length lispm-boundaries-layers)))
          (kill-emacs 0))
      (dolist (err errors)
        (princ (format "ERROR: %s\n" err) #'external-debugging-output))
      (princ (format "check-layers: FAILED (%d violations, %d cycles)\n"
                     (length errors) (length cycles))
             #'external-debugging-output)
      (kill-emacs 1))))

(provide 'check-boundaries)
;;; check-boundaries.el ends here
