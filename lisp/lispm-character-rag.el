;;; lispm-character-rag.el --- Hybrid RAG knowledge base for characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Character RAG capability extension: per-character knowledge directories,
;; lorebook entries, hybrid BM25 + embedding retrieval, prompt/context hooks,
;; `rag_search' and `knowledge_add' tools, and character form fields.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'lispm-i18n)
(require 'lispm-exec)
(require 'lispm-core)
(require 'lispm-api)
(require 'lispm-tools)
(require 'lispm-memory)
(require 'lispm-characters)

;;;; Character extension slots & predicate

(defun lispm-character-rag (character)
  "Non-nil when RAG knowledge retrieval is enabled for CHARACTER."
  (lispm-character-extension character :rag))

(gv-define-setter lispm-character-rag (value character)
  `(setf (lispm-character-extension ,character :rag) ,value))

(defun lispm-character-knowledge (character)
  "Extra RAG knowledge paths configured on CHARACTER."
  (lispm-character-extension character :knowledge))

(gv-define-setter lispm-character-knowledge (value character)
  `(setf (lispm-character-extension ,character :knowledge) ,value))

(defun lispm-character-embedding-model (character)
  "Embedding model configured for CHARACTER's RAG vector retrieval."
  (lispm-character-extension character :embedding-model))

(gv-define-setter lispm-character-embedding-model (value character)
  `(setf (lispm-character-extension ,character :embedding-model) ,value))

(defun lispm-character-rag-enabled-p (character-or-agent)
  "Non-nil when CHARACTER-OR-AGENT has its RAG knowledge base enabled."
  (when-let* ((character (cond ((lispm-principal-p character-or-agent)
                                (lispm-agent-character* character-or-agent))
                               ((lispm-character-p character-or-agent)
                                character-or-agent)
                               (t (lispm-get-character character-or-agent t)))))
    (and (or (lispm-character-rag character)
             (lispm-character-knowledge character)
             (lispm-character-embedding-model character)
             (lispm-character-knowledge-files character))
         t)))

;;;; Hybrid RAG knowledge base (Embeddings /v1/embeddings + local BM25 + Lorebook triggers)

(defcustom lispm-embedding-model nil
  "Default embedding model name for `/v1/embeddings' vector search, or nil.
When nil, character RAG uses local BM25 and Lorebook keyword matching
alone unless a character sets its own `#+embedding-model:'."
  :type '(choice (const :tag "Local BM25 only" nil) string)
  :group 'lispm)

(defcustom lispm-character-rag-top-k 4
  "How many knowledge chunks a character's conversation injects by default."
  :type 'integer
  :group 'lispm)

(defcustom lispm-character-rag-chunk-size 1000
  "Maximum characters per knowledge chunk before splitting by paragraphs."
  :type 'integer
  :group 'lispm)

(defvar lispm-embedding-function #'lispm-characters--fetch-embeddings
  "Function fetching embedding vectors for `(BACKEND MODEL TEXTS)'.
Returns a list of float vectors, one per string in TEXTS, or nil on failure.")

(defun lispm-character-knowledge-directory (character)
  "The `knowledge/' directory of CHARACTER, where its lorebook and documents live."
  (let ((name (lispm-character-name (lispm-get-character character))))
    (expand-file-name "knowledge/" (lispm-character-directory name))))

(defun lispm-characters--knowledge-paths (character)
  "Extra knowledge paths configured on CHARACTER via `#+knowledge:'."
  (let ((raw (lispm-character-knowledge (lispm-get-character character)))
        (base (file-name-directory
               (or (lispm-character-file (lispm-get-character character))
                   (expand-file-name "character.org"
                                     (lispm-character-directory
                                      (lispm-character-name (lispm-get-character character))))))))
    (cond ((null raw) nil)
          ((listp raw) (mapcar (lambda (p) (expand-file-name p base)) raw))
          ((stringp raw)
           (mapcar (lambda (part) (expand-file-name (string-trim part) base))
                   (split-string raw "," t "[ \t\n]+"))))))

(defun lispm-character-knowledge-files (character)
  "All readable `.org', `.md' and `.txt' knowledge files of CHARACTER."
  (let* ((character (lispm-get-character character))
         (dir (lispm-character-knowledge-directory character))
         (sources (append (and (file-directory-p dir) (list dir))
                          (lispm-characters--knowledge-paths character)))
         files)
    (dolist (source sources)
      (cond ((and (file-regular-p source) (file-readable-p source))
             (push (expand-file-name source) files))
            ((file-directory-p source)
             (dolist (file (directory-files-recursively
                            source "\\.\\(?:org\\|md\\|txt\\)\\'" nil))
               (when (and (file-regular-p file) (file-readable-p file)
                          (not (string-prefix-p "." (file-name-nondirectory file))))
                 (push (expand-file-name file) files))))))
    (delete-dups (nreverse files))))

(defun lispm-characters--rag-tokenize (text)
  "Tokenize TEXT into lowercase ASCII words and CJK character bigrams/unigrams."
  (let ((lower (downcase (or text "")))
        tokens)
    (save-match-data
      (let ((pos 0))
        (while (string-match "[a-z0-9_]+\\|[^[:ascii:] \t\n\r.,;:!?()\\[\\]{}\"'`~@#$%^&*+=<>|/\\-]+" lower pos)
          (let ((seg (match-string 0 lower)))
            (setq pos (match-end 0))
            (if (string-match-p "\\`[a-z0-9_]+\\'" seg)
                (when (> (length seg) 1)
                  (push seg tokens))
              (let ((len (length seg)))
                (dotimes (i len)
                  (push (substring seg i (1+ i)) tokens)
                  (when (< (1+ i) len)
                    (push (substring seg i (+ i 2)) tokens)))))))))
    (nreverse tokens)))

(defun lispm-characters--split-paragraphs (text max-size)
  "Split TEXT into pieces of at most MAX-SIZE characters along paragraphs."
  (let ((paras (split-string (string-trim (or text "")) "\n[ \t]*\n+" t))
        chunks current)
    (dolist (para paras)
      (setq para (string-trim para))
      (cond ((string-empty-p para) nil)
            ((null current) (setq current para))
            ((<= (+ (length current) 2 (length para)) max-size)
             (setq current (concat current "\n\n" para)))
            (t (push current chunks)
               (setq current para))))
    (when (and current (not (string-empty-p current)))
      (push current chunks))
    (nreverse chunks)))

(defun lispm-characters--file-chunks (file)
  "Split knowledge FILE into chunks: plists with :id, :file, :title,
:keywords, :text and :tokens."
  (let ((text (lispm-file-text file))
        (org-p (string-suffix-p ".org" (downcase file)))
        sections chunks)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (looking-at "#\\+\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)$")
        (forward-line 1))
      (let ((heading-re (if org-p "^\\(\\*+\\)[ \t]+\\(.*\\)$" "^\\(#+\\)[ \t]+\\(.*\\)$"))
            (start (point))
            (title (file-name-base file))
            (keywords nil))
        (while (re-search-forward heading-re nil t)
          (let ((body (string-trim (buffer-substring-no-properties start (match-beginning 0))))
                (next-line (match-string 2)))
            (when (not (string-empty-p body))
              (push (list title keywords
                          (if org-p (org-unescape-code-in-string body) body))
                    sections))
            (if (and org-p (string-match "\\(.*?\\)[ \t]+:\\([[:alnum:]_@#%:]+\\):[ \t]*\\'" next-line))
                (setq title (string-trim (match-string 1 next-line))
                      keywords (split-string (match-string 2 next-line) ":" t))
              (setq title (string-trim next-line)
                    keywords nil))
            (forward-line 1)
            (when org-p
              (while (looking-at "^[ \t]*:\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)$")
                (when (string-equal-ignore-case (match-string 1) "KEYWORDS")
                  (setq keywords (append keywords
                                         (split-string (match-string 2) "[, \t]+" t))))
                (forward-line 1)))
            (setq start (point))))
        (let ((body (string-trim (buffer-substring-no-properties start (point-max)))))
          (when (not (string-empty-p body))
            (push (list title keywords
                        (if org-p (org-unescape-code-in-string body) body))
                  sections)))))
    (pcase-dolist (`(,title ,keywords ,body) (nreverse sections))
      (dolist (piece (lispm-characters--split-paragraphs body lispm-character-rag-chunk-size))
        (let* ((full (concat title "\n" piece))
               (id (substring (secure-hash 'sha1 (concat file "\0" full)) 0 16)))
          (push (list :id id
                      :file file
                      :title title
                      :keywords (mapcar #'downcase keywords)
                      :text piece
                      :tokens (lispm-characters--rag-tokenize full))
                chunks))))
    (nreverse chunks)))

(defun lispm-character-rag-chunks (character)
  "All RAG chunks across CHARACTER's knowledge files."
  (mapcan #'lispm-characters--file-chunks (lispm-character-knowledge-files character)))

(defun lispm-characters--rag-bm25-scores (query chunks)
  "Return an alist of (CHUNK-ID . SCORE) for CHUNKS against QUERY
using BM25 and Lorebook keyword triggers."
  (let* ((q-tokens (delete-dups (lispm-characters--rag-tokenize query)))
         (q-lower (downcase (or query "")))
         (n (max 1 (length chunks)))
         (avgdl (/ (float (max 1 (cl-loop for c in chunks sum (length (plist-get c :tokens))))) n))
         (df (make-hash-table :test #'equal))
         (k1 1.5)
         (b 0.75))
    (dolist (c chunks)
      (dolist (tok (delete-dups (copy-sequence (plist-get c :tokens))))
        (puthash tok (1+ (gethash tok df 0)) df)))
    (mapcar
     (lambda (c)
       (let* ((tokens (plist-get c :tokens))
              (dl (length tokens))
              (tf (make-hash-table :test #'equal))
              (score 0.0))
         (dolist (tok tokens)
           (puthash tok (1+ (gethash tok tf 0)) tf))
         (dolist (qt q-tokens)
           (let ((f (gethash qt tf 0)))
             (when (> f 0)
               (let* ((doc-freq (gethash qt df 0))
                      (idf (log (+ 1.0 (/ (+ (- n doc-freq) 0.5) (+ doc-freq 0.5)))))
                      (num (* f (+ k1 1.0)))
                      (den (+ f (* k1 (+ (- 1.0 b) (* b (/ (float dl) avgdl)))))))
                 (setq score (+ score (* idf (/ num den))))))))
         ;; SillyTavern Lorebook / World Info keyword and title trigger boost.
         (dolist (kw (plist-get c :keywords))
           (when (and (not (string-empty-p kw)) (string-search kw q-lower))
             (setq score (+ score 5.0))))
         (let ((title-lower (downcase (or (plist-get c :title) ""))))
           (when (and (not (string-empty-p title-lower))
                      (or (string-search title-lower q-lower)
                          (string-search q-lower title-lower)))
             (setq score (+ score 2.0))))
         (cons (plist-get c :id) score)))
     chunks)))

(defun lispm-characters--cosine-similarity (v1 v2)
  "Cosine similarity between numeric sequences V1 and V2."
  (let ((dot 0.0) (n1 0.0) (n2 0.0)
        (len (min (length v1) (length v2))))
    (when (> len 0)
      (dotimes (i len)
        (let ((a (float (elt v1 i)))
              (b (float (elt v2 i))))
          (setq dot (+ dot (* a b))
                n1 (+ n1 (* a a))
                n2 (+ n2 (* b b)))))
      (if (or (zerop n1) (zerop n2))
          0.0
        (/ dot (* (sqrt n1) (sqrt n2)))))))

(defun lispm-characters--character-backend (character)
  "Backend plist for CHARACTER, falling back to the active LispM backend."
  (let ((character (lispm-get-character character)))
    (or (lispm-character-endpoint character)
        (and (lispm-character-backend character)
             (lispm-backend (lispm-character-backend character)))
        (when-let* ((name (lispm-backend-name)))
          (lispm-backend name)))))

(defcustom lispm-character-rag-timeout 5
  "Timeout in seconds for character embedding HTTP requests."
  :type 'integer
  :group 'lispm-characters)

(defvar lispm-characters--rag-query-cache (make-hash-table :test #'equal)
  "In-memory cache mapping `(URL MODEL . QUERY)' to float embedding vectors.")

(defvar lispm-characters--rag-warming (make-hash-table :test #'equal)
  "Active background RAG embedding warmups, keyed by `(NAME . QUERY)'.")

(defun lispm-characters--parse-embeddings-json (text)
  "Parse an OpenAI-compatible `/embeddings' response JSON TEXT into vectors."
  (when (and (stringp text) (not (string-empty-p text)))
    (when-let* ((json (ignore-errors
                        (json-parse-string text :object-type 'alist :array-type 'list)))
                (data (alist-get 'data json)))
      (mapcar (lambda (item) (alist-get 'embedding item))
              (sort (copy-sequence data)
                    (lambda (a b) (< (or (alist-get 'index a) 0)
                                     (or (alist-get 'index b) 0))))))))

(defun lispm-characters--embeddings-request-spec (backend model texts)
  "Return `(ENDPOINT CONFIG BODY)' for BACKEND and MODEL embeddings of TEXTS."
  (when-let* (((and backend model texts))
              (url (plist-get backend :url))
              (endpoint (concat (string-remove-suffix "/" url) "/embeddings"))
              (body (encode-coding-string
                     (json-serialize `((model . ,model)
                                       (input . ,(vconcat texts))))
                     'utf-8)))
    (let* ((key (ignore-errors (lispm-api-key backend)))
           (headers (append '("Content-Type: application/json")
                            (when key (list (concat "Authorization: Bearer " key))))))
      (list endpoint (lispm-api-curl-config headers) body))))

(defun lispm-characters--fetch-embeddings (backend model texts)
  "Fetch embeddings from BACKEND's `/embeddings' endpoint for MODEL and TEXTS."
  (when-let* ((spec (lispm-characters--embeddings-request-spec backend model texts))
              (endpoint (nth 0 spec))
              (config (nth 1 spec))
              (body (nth 2 spec)))
    (let ((payload-file (make-temp-file "lispm-embed-" nil ".json")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'binary))
              (with-temp-file payload-file
                (set-buffer-multibyte nil)
                (insert body)))
            (with-temp-buffer
              (let ((out (current-buffer)))
                (with-temp-buffer
                  (insert config)
                  (let ((status (call-process-region
                                 (point-min) (point-max)
                                 lispm-curl-program nil (list out nil) nil
                                 "-sS" "--max-time" (number-to-string lispm-character-rag-timeout)
                                 "--config" "-" "--data-binary" (concat "@" payload-file)
                                 endpoint)))
                    (when (eq status 0)
                      (lispm-characters--parse-embeddings-json
                       (with-current-buffer out (buffer-string)))))))))
        (when (file-exists-p payload-file) (delete-file payload-file))))))

(defun lispm-characters--fetch-embeddings-async (backend model texts callback)
  "Fetch embeddings from BACKEND for MODEL and TEXTS without blocking.
Calls CALLBACK with the list of embedding vectors, or nil on failure."
  (let ((spec (lispm-characters--embeddings-request-spec backend model texts)))
    (if (not spec)
        (funcall callback nil)
      (pcase-let* ((`(,endpoint ,config ,body) spec)
                   (payload-file (make-temp-file "lispm-embed-" nil ".json"))
                   (started nil))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'binary))
                (with-temp-file payload-file
                  (set-buffer-multibyte nil)
                  (insert body)))
              (lispm-exec
               lispm-curl-program
               (list "-sS" "--max-time" (number-to-string lispm-character-rag-timeout)
                     "--config" "-" "--data-binary" (concat "@" payload-file)
                     endpoint)
               (lambda (exit output _errors)
                 (unwind-protect
                     (funcall callback
                              (and (eql exit 0)
                                   (lispm-characters--parse-embeddings-json output)))
                   (when (file-exists-p payload-file)
                     (ignore-errors (delete-file payload-file)))))
               :input config)
              (setq started t))
          (unless started
            (when (file-exists-p payload-file)
              (ignore-errors (delete-file payload-file)))))))))

(defun lispm-characters--rag-index-file (character)
  "File caching embedding vectors for CHARACTER."
  (expand-file-name ".rag-index.eld"
                    (lispm-character-directory (lispm-character-name (lispm-get-character character)))))

(defun lispm-characters--rag-read-cache (index-file)
  "Read persisted embedding hash table from INDEX-FILE, or make a fresh one."
  (or (and (file-readable-p index-file)
           (ignore-errors
             (with-temp-buffer
               (insert-file-contents index-file)
               (read (current-buffer)))))
      (make-hash-table :test #'equal)))

(defun lispm-characters--rag-write-cache (index-file cache)
  "Write embedding CACHE hash table to INDEX-FILE."
  (ignore-errors
    (let ((print-length nil) (print-level nil))
      (with-temp-file index-file
        (prin1 cache (current-buffer))))))

(defun lispm-characters--rag-query-key (backend model query)
  "Return lookup key for QUERY embeddings under BACKEND and MODEL."
  (list (plist-get backend :url) model query))

(defun lispm-characters--rag-cache-query-vec (backend model query vec)
  "Store query vector VEC under BACKEND, MODEL and QUERY."
  (when vec
    (when (>= (hash-table-count lispm-characters--rag-query-cache) 64)
      (clrhash lispm-characters--rag-query-cache))
    (puthash (lispm-characters--rag-query-key backend model query)
             vec lispm-characters--rag-query-cache)))

(defun lispm-characters--rag-scores-from-cache (model chunks cache q-vec)
  "Compute cosine similarity alist for CHUNKS using MODEL, CACHE and Q-VEC."
  (when q-vec
    (cl-loop for c in chunks
             for v = (gethash (concat model ":" (plist-get c :id)) cache)
             when v
             collect (cons (plist-get c :id)
                           (max 0.0 (lispm-characters--cosine-similarity q-vec v))))))

(defun lispm-characters--rag-embedding-scores (character query chunks)
  "Return an alist of `(CHUNK-ID . COSINE-SCORE)' synchronously, or nil."
  (let* ((character (lispm-get-character character))
         (model (or (lispm-character-embedding-model character) lispm-embedding-model))
         (backend (and model (lispm-characters--character-backend character))))
    (when (and model backend chunks)
      (let* ((index-file (lispm-characters--rag-index-file character))
             (cache (lispm-characters--rag-read-cache index-file))
             (missing (seq-filter (lambda (c)
                                    (not (gethash (concat model ":" (plist-get c :id)) cache)))
                                  chunks))
             (dirty nil))
        (when missing
          (when-let* ((vecs (funcall lispm-embedding-function
                                     backend model
                                     (mapcar (lambda (c)
                                               (concat (plist-get c :title) "\n" (plist-get c :text)))
                                             missing))))
            (cl-loop for c in missing
                     for v in vecs
                     when v
                     do (puthash (concat model ":" (plist-get c :id)) v cache)
                        (setq dirty t))))
        (when dirty
          (lispm-characters--rag-write-cache index-file cache))
        (when-let* ((q-vec (car (funcall lispm-embedding-function backend model (list query)))))
          (lispm-characters--rag-cache-query-vec backend model query q-vec)
          (lispm-characters--rag-scores-from-cache model chunks cache q-vec))))))

(defun lispm-characters--warm-embeddings-async (character query chunks &optional callback)
  "Fetch missing embeddings for CHARACTER, QUERY and CHUNKS without blocking.
When CALLBACK is non-nil, call it with the resulting cosine scores alist."
  (let* ((character (lispm-get-character character))
         (model (or (lispm-character-embedding-model character) lispm-embedding-model))
         (backend (and model (lispm-characters--character-backend character))))
    (if (not (and model backend chunks))
        (when callback (funcall callback nil))
      (let* ((index-file (lispm-characters--rag-index-file character))
             (cache (lispm-characters--rag-read-cache index-file))
             (missing (seq-filter (lambda (c)
                                    (not (gethash (concat model ":" (plist-get c :id)) cache)))
                                  chunks))
             (q-vec (gethash (lispm-characters--rag-query-key backend model query)
                             lispm-characters--rag-query-cache)))
        (if (and (null missing) q-vec)
            (when callback
              (funcall callback (lispm-characters--rag-scores-from-cache model chunks cache q-vec)))
          (let ((warm-key (cons (lispm-character-name character) query)))
            (if-let* ((existing (gethash warm-key lispm-characters--rag-warming)))
                (when callback
                  (puthash warm-key (cons callback existing) lispm-characters--rag-warming))
              (puthash warm-key (list (or callback #'ignore)) lispm-characters--rag-warming)
              (let ((texts (append (mapcar (lambda (c)
                                             (concat (plist-get c :title) "\n" (plist-get c :text)))
                                           missing)
                                   (unless q-vec (list query)))))
                (lispm-characters--fetch-embeddings-async
                 backend model texts
                 (lambda (vecs)
                   (let ((callbacks (nreverse (gethash warm-key lispm-characters--rag-warming)))
                         (resolved-q-vec q-vec))
                     (remhash warm-key lispm-characters--rag-warming)
                     (when vecs
                       (let ((dirty nil))
                         (cl-loop for c in missing
                                  for v in vecs
                                  when v
                                  do (puthash (concat model ":" (plist-get c :id)) v cache)
                                     (setq dirty t))
                         (when dirty
                           (lispm-characters--rag-write-cache index-file cache))
                         (unless resolved-q-vec
                           (setq resolved-q-vec (nth (length missing) vecs))
                           (lispm-characters--rag-cache-query-vec
                            backend model query resolved-q-vec))))
                     (let ((scores (and resolved-q-vec
                                        (lispm-characters--rag-scores-from-cache
                                         model chunks cache resolved-q-vec))))
                       (dolist (cb callbacks)
                         (unless (eq cb #'ignore)
                           (funcall cb scores)))))))))))))))

(defun lispm-characters--rag-embedding-scores-cached (character query chunks)
  "Return cached embedding scores for CHARACTER, QUERY and CHUNKS, warming async."
  (let* ((character (lispm-get-character character))
         (model (or (lispm-character-embedding-model character) lispm-embedding-model))
         (backend (and model (lispm-characters--character-backend character))))
    (when (and model backend chunks)
      (let* ((cache (lispm-characters--rag-read-cache (lispm-characters--rag-index-file character)))
             (missing (seq-filter (lambda (c)
                                    (not (gethash (concat model ":" (plist-get c :id)) cache)))
                                  chunks))
             (q-vec (gethash (lispm-characters--rag-query-key backend model query)
                             lispm-characters--rag-query-cache)))
        (when (or missing (not q-vec))
          (lispm-characters--warm-embeddings-async character query chunks))
        (and q-vec
             (lispm-characters--rag-scores-from-cache model chunks cache q-vec))))))

(defun lispm-characters--rag-rank (chunks bm25 emb limit)
  "Combine BM25 and EMB scores over CHUNKS and return at most LIMIT results."
  (let* ((max-bm25 (max 1.0 (apply #'max 0.0 (mapcar #'cdr bm25))))
         (scored
          (cl-loop for c in chunks
                   for id = (plist-get c :id)
                   for b-score = (or (cdr (assoc id bm25)) 0.0)
                   for e-score = (cdr (assoc id emb))
                   for total = (if e-score
                                   (+ (* 0.6 e-score) (* 0.4 (/ b-score max-bm25)))
                                 b-score)
                   when (> total 0.0)
                   collect (plist-put (copy-sequence c) :score total))))
    (seq-take (sort scored (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
              limit)))

(defun lispm-character-rag-search (character query &optional limit)
  "Search CHARACTER's knowledge base for QUERY using hybrid RAG.
Return at most LIMIT (default `lispm-character-rag-top-k') matching
chunk plists, each with `:score'."
  (let* ((character (lispm-get-character character))
         (chunks (lispm-character-rag-chunks character))
         (limit (or limit lispm-character-rag-top-k)))
    (when chunks
      (lispm-characters--rag-rank
       chunks
       (lispm-characters--rag-bm25-scores query chunks)
       (lispm-characters--rag-embedding-scores character query chunks)
       limit))))

(defun lispm-character-rag-search-async (character query limit callback)
  "Search CHARACTER's knowledge base for QUERY without blocking.
Call CALLBACK with at most LIMIT matching chunk plists."
  (let* ((character (lispm-get-character character))
         (chunks (lispm-character-rag-chunks character))
         (limit (or limit lispm-character-rag-top-k)))
    (cond
     ((null chunks)
      (funcall callback nil))
     ((not (eq lispm-embedding-function #'lispm-characters--fetch-embeddings))
      (let* ((bm25 (lispm-characters--rag-bm25-scores query chunks))
             (emb (lispm-characters--rag-embedding-scores character query chunks)))
        (funcall callback (lispm-characters--rag-rank chunks bm25 emb limit))))
     (t
      (let ((bm25 (lispm-characters--rag-bm25-scores query chunks))
            (model (or (lispm-character-embedding-model character) lispm-embedding-model))
            (backend (lispm-characters--character-backend character)))
        (if (not (and model backend))
            (funcall callback (lispm-characters--rag-rank chunks bm25 nil limit))
          (lispm-characters--warm-embeddings-async
           character query chunks
           (lambda (emb)
             (funcall callback (lispm-characters--rag-rank chunks bm25 emb limit))))))))))

(defun lispm-character-rag-add (character title content &optional keywords)
  "Add or update a lorebook entry TITLE with CONTENT and KEYWORDS
in CHARACTER's knowledge base."
  (let* ((character (lispm-get-character character))
         (dir (lispm-character-knowledge-directory character))
         (file (expand-file-name "lorebook.org" dir))
         (tags (and keywords
                    (if (listp keywords)
                        keywords
                      (split-string keywords "[, \t:]+" t))))
         (tag-str (if tags (concat "  :" (string-join tags ":") ":") ""))
         (clean-title (lispm-oneline title 120)))
    (make-directory dir t)
    (let ((buffer (lispm-memory-visit
                   file (format "#+title: Knowledge of %s\n" (lispm-character-title* character))
                   t)))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((re (concat "^\\*+ +\\(?:" (regexp-quote clean-title) "\\)\\(?:[ \t]+:.*:\\)?[ \t]*$")))
              (if (re-search-forward re nil t)
                  (let ((beg (line-beginning-position))
                        (end (if (re-search-forward "^\\*+ " nil t)
                                 (line-beginning-position)
                               (point-max))))
                    (delete-region beg end)
                    (goto-char beg))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))))
            (insert "* " clean-title tag-str "\n"
                    (org-escape-code-in-string (string-trim content)) "\n")))
        (lispm-memory-save buffer)))
    (lispm-git-commit (symbol-name (lispm-character-name character))
                      (format "%s: knowledge: %s" (lispm-character-name character) clean-title))
    file))

(defun lispm-characters--rag-format (chunks)
  "Format RAG CHUNKS for display in a prompt or tool result."
  (mapconcat (lambda (c)
               (format "[%s · %s]\n%s"
                       (file-name-nondirectory (plist-get c :file))
                       (plist-get c :title)
                       (plist-get c :text)))
             chunks "\n\n"))

(defun lispm-characters--rag-prompt (agent)
  "System prompt section summarizing a character's RAG knowledge base for AGENT."
  (when-let* ((character (lispm-agent-character* agent))
              ((lispm-character-rag-enabled-p character))
              (files (lispm-character-knowledge-files character)))
    (format "# Your knowledge base (RAG)\nSources (%d): %s. Use `rag_search' to query your knowledge base and `knowledge_add' to record new lorebook entries.\n"
            (length files)
            (mapconcat #'file-name-nondirectory (seq-take files 10) ", "))))

(defun lispm-characters--rag-context (agent)
  "Request context section retrieving RAG passages for AGENT's current turn."
  (when-let* ((character (lispm-agent-character* agent))
              ((lispm-character-rag-enabled-p character))
              ((lispm-character-knowledge-files character))
              (input (cdr (lispm-characters-last-input agent)))
              ((not (string-empty-p input)))
              (chunks (lispm-character-rag-chunks character)))
    (let* ((bm25 (lispm-characters--rag-bm25-scores input chunks))
           (emb (if (eq lispm-embedding-function #'lispm-characters--fetch-embeddings)
                    (lispm-characters--rag-embedding-scores-cached character input chunks)
                  (lispm-characters--rag-embedding-scores character input chunks)))
           (hits (lispm-characters--rag-rank chunks bm25 emb lispm-character-rag-top-k)))
      (when hits
        (concat "Relevant passages from your knowledge base for this turn:\n"
                (lispm-truncate (lispm-characters--rag-format hits) 6000))))))

(add-hook 'lispm-system-prompt-functions #'lispm-characters--rag-prompt t)
(add-hook 'lispm-request-context-functions #'lispm-characters--rag-context t)

(lispm-deftool rag_search
    ((query string "Words, concepts or names to look up in your knowledge base.")
     (limit integer "Maximum passages to return, 4 by default." :optional t))
  "Search your knowledge base (RAG lorebook and attached documents) using
hybrid vector embeddings and BM25 keyword retrieval."
  :permission read
  :async t
  :concurrency 'parallel
  :for #'lispm-character-rag-enabled-p
  (let* ((character (or (lispm-agent-character* lispm-current-agent)
                        (error "You are no character")))
         (empty (not (lispm-character-knowledge-files character))))
    (lispm-character-rag-search-async
     character query (or limit lispm-character-rag-top-k)
     (lambda (hits)
       (funcall done
                (cond
                 (hits (lispm-characters--rag-format hits))
                 (empty "Your knowledge base is empty; add entries with knowledge_add or place files in your knowledge/ directory.")
                 (t "No matching passages found in your knowledge base.")))))
    nil))

(lispm-deftool knowledge_add
    ((title string "Heading of the knowledge or lorebook entry.")
     (content string "The knowledge content to store for RAG retrieval.")
     (keywords string "Comma-separated trigger keywords or tags for Lorebook matching." :optional t))
  "Add or update an entry in your personal RAG knowledge base (`knowledge/lorebook.org')."
  :permission memory
  :for #'lispm-character-rag-enabled-p
  (let ((character (or (lispm-agent-character* lispm-current-agent)
                       (error "You are no character"))))
    (lispm-character-rag-add character title content keywords)
    (format "Saved \"%s\" in your knowledge base." title)))

;;;; Extension registration

(lispm-character-register-extension
 'rag
 :keys '(:rag :knowledge :embedding-model)
 :read (lambda (line bool _symbol)
         (list :rag (funcall bool "rag")
               :knowledge (funcall line "knowledge")
               :embedding-model (funcall line "embedding-model")))
 :format (lambda (props line-fn)
           `(("rag" . ,(and (plist-get props :rag) "yes"))
             ("knowledge" . ,(funcall line-fn (plist-get props :knowledge)))
             ("embedding-model" . ,(funcall line-fn (plist-get props :embedding-model)))))
 :form-values (lambda (character)
                `((rag . ,(and character (lispm-character-rag-enabled-p character)))
                  (knowledge . ,(and character (lispm-character-knowledge character)))
                  (embedding-model . ,(and character (lispm-character-embedding-model character)))))
 :form-props (lambda (get)
               (let ((rag (and (funcall get 'rag) t)))
                 (list :rag rag
                       :knowledge (and rag (funcall get 'knowledge))
                       :embedding-model (and rag (funcall get 'embedding-model)))))
 :form-fields (lambda ()
                `((rag :type check :label ,(lispm-tr "Knowledge")
                       :text ,(lispm-tr "Use a knowledge base (RAG & Lorebook)"))
                  (knowledge :type text :label ,(lispm-tr "Knowledge files")
                             :help ,(lispm-tr "Files or folders of its knowledge base, separated by commas.")
                             :when ,(lispm-characters-when-checked 'rag))
                  (embedding-model :type text :label ,(lispm-tr "Embedding model")
                                   :help ,(lispm-tr "For /v1/embeddings vector search; empty for local BM25.")
                                   :when ,(lispm-characters-when-checked 'rag)))))

(provide 'lispm-character-rag)
;;; lispm-character-rag.el ends here
