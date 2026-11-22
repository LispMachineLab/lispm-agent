;;; lispm-spotlight-test.el --- Tests for read/write stage spotlights  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; ERT tests witnessing pure interval composition, stage window reuse,
;; partial vs whole-file read spotlights, multi-agent concurrent overlap,
;; viewport scroll arbitration, write priority, and ephemeral cleanup.

;;; Code:

(require 'ert)
(require 'lispm)
(require 'lispm-test)

(defmacro lispm-spotlight-test--with-env (&rest body)
  "Run BODY in `lispm-test-env' with both edits and spotlights active in batch."
  (declare (indent 0))
  `(lispm-test-env
     (let ((lispm-edits--in-batch t)
           (lispm-edits--shows nil)
           (lispm-edits--proposals nil)
           (lispm-edits--writing (make-hash-table :test #'equal))
           (lispm-spotlight--in-batch t)
           (lispm-spotlight--ephemeral-buffers nil))
       (lispm-stage-reset)
       (lispm-spotlight-reset)
       (unwind-protect
           (progn ,@body)
         (lispm-edits--tick 1e10)
         (mapc (lambda (p) (lispm-edits--retire p nil)) lispm-edits--proposals)
         (lispm-spotlight--tick 1e10)
         (lispm-spotlight-reset)
         (lispm-stage-reset)))))

(defun lispm-spotlight-test--overlays (buffer)
  "Return spotlight overlays in BUFFER sorted by start position."
  (with-current-buffer buffer
    (sort (seq-filter (lambda (ov) (overlay-get ov 'lispm-spotlight))
                      (overlays-in (point-min) (point-max)))
          (lambda (a b)
            (if (= (overlay-start a) (overlay-start b))
                (< (overlay-end a) (overlay-end b))
              (< (overlay-start a) (overlay-start b)))))))

(ert-deftest lispm-spotlight-test-pure-interval-composition ()
  "Overlapping agent spans decompose into disjoint intervals and coalesced tags."
  (should (= (lispm-spotlight--beam-alpha nil 100.0 1.0) 1.0))
  (should (= (lispm-spotlight--beam-alpha 100.0 99.0 1.0) 1.0))
  (should (< (abs (- (lispm-spotlight--beam-alpha 100.0 100.5 1.0) 0.5)) 1e-6))
  (should (= (lispm-spotlight--beam-alpha 100.0 101.5 1.0) 0.0))
  (should (equal (lispm-spotlight--coalesce-lines '(12 4 5 6 10 5))
                 '((4 . 6) (10 . 10) (12 . 12))))
  (let* ((items (list (list :agent-key "alpha" :label "alpha" :color "#f5a97f"
                            :alpha 1.0 :note "L2-5" :tag-pos 10
                            :spans '((10 . 50)))
                      (list :agent-key "beta" :label "beta" :color "#8bd5ca"
                            :alpha 1.0 :note "L2-8" :tag-pos 10
                            :spans '((30 . 80)))))
         (tags (lispm-spotlight--compose-tags items))
         (segs (lispm-spotlight--compose-segments items)))
    (should (= (length tags) 1))
    (should (= (plist-get (car tags) :pos) 10))
    (should (equal (mapcar (lambda (it) (plist-get it :agent-key))
                           (plist-get (car tags) :items))
                   '("alpha" "beta")))
    (should (equal (mapcar (lambda (s)
                             (list (plist-get s :beg)
                                   (plist-get s :end)
                                   (mapcar (lambda (it) (plist-get it :agent-key))
                                           (plist-get s :items))))
                           segs)
                   '((10 30 ("alpha"))
                     (30 50 ("alpha" "beta"))
                     (50 80 ("beta")))))))

(ert-deftest lispm-spotlight-test-stage-window-reuse ()
  "Reads and edits of files and non-file buffers share a single stage window."
  (lispm-spotlight-test--with-env
    (let ((file-a (expand-file-name "a.el" project))
          (file-b (expand-file-name "b.el" project))
          (scratch (generate-new-buffer "*spotlight-scratch*"))
          (agent (lispm-defagent reader "Reader." :root project)))
      (lispm-test--write file-a ";; a\n(defun a () 1)\n")
      (lispm-test--write file-b ";; b\n(defun b () 2)\n")
      (with-current-buffer scratch
        (insert "alpha\nbeta\ngamma\n"))
      (let ((chat (lispm-chat agent)))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (switch-to-buffer chat)
              (should (= (length (window-list)) 1))
              ;; 1. Reading a file splits off the stage window once.
              (lispm-test--call agent "read_file" '((path . "a.el")))
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer (selected-window)) chat))
              (let ((stage (get-buffer-window (find-buffer-visiting file-a))))
                (should (window-live-p stage))
                (should (window-parameter stage 'lispm-stage))
                ;; 2. Reading a non-file buffer reuses the same stage window.
                (lispm-test--call agent "read_buffer"
                                  `((buffer . ,(buffer-name scratch))))
                (should (= (length (window-list)) 2))
                (should (eq (window-buffer stage) scratch))
                ;; 3. Editing another file reuses the same stage window too.
                (lispm-test--call agent "edit_file"
                                  '((path . "b.el")
                                    (old_text . "(defun b () 2)")
                                    (new_text . "(defun b () 42)")))
                (should (= (length (window-list)) 2))
                (should (eq (window-buffer stage) (find-buffer-visiting file-b)))
                (should (eq (window-buffer (selected-window)) chat))))
          (when (buffer-live-p scratch)
            (kill-buffer scratch)))))))

(ert-deftest lispm-spotlight-test-partial-vs-whole-file-and-fade ()
  "A partial read lights its exact lines; a whole long file lights its header."
  (lispm-spotlight-test--with-env
    (let* ((file (expand-file-name "long.el" project))
           (content (mapconcat (lambda (i) (format "line-%02d\n" i))
                               (number-sequence 1 50) ""))
           (agent (lispm-defagent reader "Reader." :root project)))
      (lispm-test--write file content)
      (let ((buf (find-file-noselect file)))
        ;; Partial read of lines 10..12.
        (lispm-test--call agent "read_file"
                          '((path . "long.el") (offset . 10) (limit . 3)))
        (let* ((ovs (lispm-spotlight-test--overlays buf))
               (tag-ov (seq-find (lambda (o) (overlay-get o 'before-string)) ovs))
               (tint-ov (seq-find (lambda (o) (overlay-get o 'line-prefix)) ovs)))
          (should tag-ov)
          (should (string-match-p "λ reader · reading lines 10–12"
                                  (substring-no-properties
                                   (overlay-get tag-ov 'before-string))))
          (should tint-ov)
          (with-current-buffer buf
            (should (= (line-number-at-pos (overlay-start tint-ov)) 10))
            (should (= (line-number-at-pos (1- (overlay-end tint-ov))) 12))))
        ;; Retire and verify clean fade completion.
        (lispm-spotlight--tick (+ (float-time) 100.0))
        (should-not (lispm-spotlight-test--overlays buf))
        ;; Whole-file read of 50 lines (> threshold 40) tints only header lines.
        (lispm-test--call agent "read_file" '((path . "long.el")))
        (let* ((ovs (lispm-spotlight-test--overlays buf))
               (tag-ov (seq-find (lambda (o) (overlay-get o 'before-string)) ovs))
               (tint-ov (seq-find (lambda (o) (overlay-get o 'line-prefix)) ovs)))
          (should (string-match-p "λ reader · reading long\\.el"
                                  (substring-no-properties
                                   (overlay-get tag-ov 'before-string))))
          (with-current-buffer buf
            (should (= (line-number-at-pos (overlay-start tint-ov)) 1))
            (should (= (line-number-at-pos (1- (overlay-end tint-ov)))
                       lispm-spotlight-whole-header-lines))))))))

(ert-deftest lispm-spotlight-test-multi-agent-overlap-and-scroll-arbitration ()
  "Concurrent agents reading the same file stack stripes without hijacking scroll."
  (lispm-spotlight-test--with-env
    (let* ((shared (expand-file-name "shared.el" project))
           (other (expand-file-name "other.el" project))
           (lines (mapconcat (lambda (i) (format "row-%02d\n" i))
                             (number-sequence 1 20) ""))
           (alpha (lispm-defagent alpha "Primary agent." :root project))
           (beta (lispm-defagent beta "Background peer." :root project))
           (chat-a (lispm-chat alpha)))
      (lispm-test--write shared lines)
      (lispm-test--write other lines)
      (let ((shared-buf (find-file-noselect shared))
            (other-buf (find-file-noselect other)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer chat-a)
          ;; Attended `alpha' reads lines 3..6 of shared.el.
          (lispm-test--call alpha "read_file"
                            '((path . "shared.el") (offset . 3) (limit . 4)))
          ;; Unattended `beta' reads overlapping lines 5..8 of shared.el.
          (lispm-test--call beta "read_file"
                            '((path . "shared.el") (offset . 5) (limit . 4)))
          (let ((stripe-ovs (seq-filter (lambda (o) (overlay-get o 'line-prefix))
                                        (lispm-spotlight-test--overlays shared-buf))))
            ;; Expect 3 disjoint segments: 3..4 (1 stripe), 5..6 (2 stripes), 7..8 (1 stripe).
            (should (= (length stripe-ovs) 3))
            (should (equal (mapcar (lambda (o)
                                     (substring-no-properties (overlay-get o 'line-prefix)))
                                   stripe-ovs)
                           '("▎" "▎▎" "▎"))))
          ;; Same-line badges coalesce onto one line when `beta' also reads from line 3.
          (lispm-spotlight--on-response beta nil)
          (lispm-test--call beta "read_file"
                            '((path . "shared.el") (offset . 3) (limit . 2)))
          (let* ((tag-ovs (seq-filter (lambda (o) (overlay-get o 'before-string))
                                      (lispm-spotlight-test--overlays shared-buf)))
                 (line3-tags (with-current-buffer shared-buf
                               (seq-filter (lambda (o)
                                             (= (line-number-at-pos (overlay-start o)) 3))
                                           tag-ovs))))
            (should (= (length line3-tags) 1))
            (let ((text (substring-no-properties
                         (overlay-get (car line3-tags) 'before-string))))
              (should (string-match-p "λ alpha" text))
              (should (string-match-p "λ beta" text))
              ;; Single trailing newline: never pushes code down by two rows.
              (should (= (cl-count ?\n text) 1))))
          ;; Background `beta' reading `other.el' paints `other-buf' WITHOUT
          ;; stealing the stage window from attended `alpha''s `shared-buf'.
          (lispm-spotlight--on-response beta nil)
          (lispm-test--call beta "read_file"
                            '((path . "other.el") (offset . 2) (limit . 3)))
          (should (get-buffer-window shared-buf))
          (should-not (get-buffer-window other-buf))
          (should (lispm-spotlight-test--overlays other-buf)))))))

(ert-deftest lispm-spotlight-test-ephemeral-cleanup-and-write-priority ()
  "Unopened files read in passing are reaped once unseen; writes sit above reads."
  (lispm-spotlight-test--with-env
    (let ((f1 (expand-file-name "ephemeral-1.el" project))
          (f2 (expand-file-name "ephemeral-2.el" project))
          (agent (lispm-defagent worker "Worker." :root project)))
      (lispm-test--write f1 ";; one\n(setq x 1)\n")
      (lispm-test--write f2 ";; two\n(setq y 2)\n")
      (let ((chat (lispm-chat agent)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer chat)
          (should-not (find-buffer-visiting f1))
          (lispm-test--call agent "read_file" '((path . "ephemeral-1.el")))
          (let ((b1 (find-buffer-visiting f1)))
            (should (buffer-live-p b1))
            (should (buffer-local-value 'lispm-spotlight--ephemeral b1))
            ;; Move stage to f2 and advance fade: b1 is unseen and reaped.
            (lispm-spotlight--on-response agent nil)
            (lispm-test--call agent "read_file" '((path . "ephemeral-2.el")))
            (lispm-spotlight--tick (+ (float-time) 100.0))
            (should-not (buffer-live-p b1))
            ;; Editing f2 promotes b2 to a workspace buffer and gives write
            ;; overlays higher priority than read spotlight overlays.
            (let ((b2 (find-buffer-visiting f2)))
              (should (buffer-live-p b2))
              (lispm-test--call agent "read_file"
                                '((path . "ephemeral-2.el") (offset . 2) (limit . 1)))
              (lispm-test--call agent "edit_file"
                                '((path . "ephemeral-2.el")
                                  (old_text . "(setq y 2)")
                                  (new_text . "(setq y 99)")))
              (should-not (buffer-local-value 'lispm-spotlight--ephemeral b2))
              (with-current-buffer b2
                (let ((edit-ov (seq-find (lambda (o) (overlay-get o 'lispm-edits))
                                         (overlays-in (point-min) (point-max))))
                      (read-ov (seq-find (lambda (o) (overlay-get o 'lispm-spotlight))
                                         (overlays-in (point-min) (point-max)))))
                  (should edit-ov)
                  (should read-ov)
                  (should (> (overlay-get edit-ov 'priority)
                             (overlay-get read-ov 'priority))))))))))))

(ert-deftest lispm-spotlight-test-query-search-and-xref ()
  "Buffer queries, file searches and xref lookups spotlight their matched lines."
  (lispm-spotlight-test--with-env
    (let ((file (expand-file-name "sample.el" project))
          (log (generate-new-buffer "*spotlight-log*"))
          (agent (lispm-defagent finder "Finder." :root project)))
      (lispm-test--write
       file
       ";;; sample.el --- test  -*- lexical-binding: t; -*-\n\n(defun lispm-spotlight-sample-fn (x)\n  \"Double X.\"\n  (* 2 x))\n\n(provide 'sample)\n")
      (load file nil t)
      (with-current-buffer log
        (insert "ok 1\nERROR needle first\nok 3\nWARN needle second\n"))
      (unwind-protect
          (progn
            ;; 1. read_buffer with query highlights only lines 2 and 4.
            (lispm-test--call agent "read_buffer"
                              `((buffer . ,(buffer-name log)) (query . "needle")))
            (let ((stripe-ovs (seq-filter (lambda (o) (overlay-get o 'line-prefix))
                                          (lispm-spotlight-test--overlays log))))
              (should (= (length stripe-ovs) 2))
              (with-current-buffer log
                (should (equal (mapcar (lambda (o)
                                         (line-number-at-pos (overlay-start o)))
                                       stripe-ovs)
                               '(2 4)))))
            ;; 2. search_files spotlights matching line in sample.el.
            (lispm-spotlight--tick (+ (float-time) 100.0))
            (lispm-test--call agent "search_files" '((pattern . "Double X")))
            (let* ((sample-buf (find-buffer-visiting file))
                   (tag-ov (seq-find (lambda (o) (overlay-get o 'before-string))
                                     (lispm-spotlight-test--overlays sample-buf))))
              (should tag-ov)
              (should (string-match-p "matching “Double X”"
                                      (substring-no-properties
                                       (overlay-get tag-ov 'before-string)))))
            ;; 3. xref_definitions illuminates the whole `defun' form (lines 3..5).
            (lispm-spotlight--tick (+ (float-time) 100.0))
            (lispm-test--call agent "xref_definitions"
                              '((identifier . "lispm-spotlight-sample-fn")))
            (let* ((sample-buf (find-buffer-visiting file))
                   (ovs (lispm-spotlight-test--overlays sample-buf))
                   (tag-ov (seq-find (lambda (o) (overlay-get o 'before-string)) ovs))
                   (tint-ov (seq-find (lambda (o) (overlay-get o 'line-prefix)) ovs)))
              (should (string-match-p "definition of lispm-spotlight-sample-fn"
                                      (substring-no-properties
                                       (overlay-get tag-ov 'before-string))))
              (with-current-buffer sample-buf
                (should (= (line-number-at-pos (overlay-start tint-ov)) 3))
                (should (= (line-number-at-pos (1- (overlay-end tint-ov))) 5)))))
        (when (buffer-live-p log)
          (kill-buffer log))))))

(provide 'lispm-spotlight-test)
;;; lispm-spotlight-test.el ends here
