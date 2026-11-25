;;; lispm-tool-search.el --- Deferred tool discovery and session restoration  -*- lexical-binding: t; -*-

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

;; Fuzzy keyword and source-filtered search for deferred tools and
;; skills (`search_tools'), dynamic tool adoption, and transcript
;; restoration on session revisit.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-core)
(require 'lispm-tool-core)

;;;; Deferred tools
;;
;; An agent carries a layer of tools it commonly needs; others, many or
;; seldom needed, are deferred: Peekaboo's 26 tools, for using the
;; computer, would take 14,000 tokens of every request.  Using one is
;; two steps.  The model searches for what it needs with search_tools,
;; which loads the tools it finds, and from its next step on, they are
;; offered to it like the others, for the rest of the session.  A
;; resumed session has them again: the transcript records which tools
;; it searched for and called.  The search also finds tools already
;; offered, so an agent can look up an existing capability again.
;;
;; The search is fuzzy (`lispm-tools-search-score'): a word may be
;; part of one it finds, or misspelt.  It finds what
;; `lispm-tool-search-functions' offer too, as the skills, the manuals
;; of the parts and apps of the system: a tool of a skill comes with
;; its skill, which the search names, so the model reads how to use it.
;;
;; A tool an agent defines itself, with eval_elisp or the code of a
;; skill it loads, is deferred too, and loaded for that agent at once:
;; it is the agent's from its next step on, other agents find it with
;; search_tools, and it joins no one's core layer (`lispm-tools-adopt').

(defcustom lispm-tool-search-results 5
  "How many tools search_tools loads for a search by keywords."
  :type 'integer
  :group 'lispm)

(defun lispm-tools--deferred ()
  "The deferred tools registered."
  (sort (seq-filter #'lispm-tool-deferred (lispm-tools-registered))
        (lambda (a b) (string< (lispm-tool-name a) (lispm-tool-name b)))))

(defun lispm-tools--registered ()
  "The registered tools, sorted by name."
  (sort (lispm-tools-registered)
        (lambda (a b) (string< (lispm-tool-name a) (lispm-tool-name b)))))

(defun lispm-tools-prepare (sources callback)
  "Prepare SOURCES, all at once; call CALLBACK with their problems."
  (if (null sources)
      (funcall callback nil)
    (let ((waiting (length sources)) problems)
      (dolist (source sources)
        (funcall (plist-get source :prepare)
                 (lambda (problem)
                   (when problem (push problem problems))
                   (when (zerop (cl-decf waiting))
                     (funcall callback (nreverse problems)))))))))

(defun lispm-tools-description (tool &optional full)
  "The first line of TOOL's description, or all of it if FULL."
  (let ((description (lispm-tool-description tool)))
    (setq description (if (functionp description) (funcall description) description))
    (if full description (car (split-string description "\n")))))

;;;;; Fuzzy search

(defconst lispm-tools--search-stop-words
  '("a" "an" "and" "any" "as" "at" "be" "by" "can" "do" "for" "from" "how" "i" "in" "into" "is"
    "it" "me" "my" "of" "on" "or" "some" "the" "to" "use" "with")
  "Words of a search that say nothing of what it looks for.")

(defun lispm-tools-search-words (query)
  "The words of the search QUERY, lowercase.
It is split at all but letters, digits, _ and -.  Words saying
nothing of what is looked for, such as \"the\", and single letters
are left out, unless nothing else is left."
  (let* ((words (split-string (downcase query) "[^[:alnum:]_-]+" t))
         (telling (seq-remove (lambda (word) (or (< (length word) 2)
                                                 (member word lispm-tools--search-stop-words)))
                              words)))
    (seq-uniq (or telling words))))

(defun lispm-tools--search-tokens (text)
  "The words of TEXT a search matches against, lowercase.
Words holding _ or - are taken apart too."
  (let ((words (split-string (downcase text) "[^[:alnum:]_-]+" t)))
    (seq-uniq (append words (mapcan (lambda (word)
                                      (and (string-match-p "[_-]" word)
                                           (split-string word "[_-]+" t)))
                                    words)))))

(defun lispm-tools--word-match (word token)
  "How well WORD of a search matches TOKEN, a word of what it looks at: 0 to 1."
  (let ((lw (length word))
        (lt (length token)))
    (cond ((equal word token) 1.0)
          ((and (>= lw 3) (string-prefix-p word token)) 0.8)
          ((and (>= lt 4) (string-prefix-p token word)) 0.7)
          ((and (>= lw 4) (string-search word token)) 0.5)
          ;; A misspelling: the same first letter, a word long enough that
          ;; one letter off is no other word, as "chat" is "what".
          ((and (>= lw 5) (>= lt 5) (eq (aref word 0) (aref token 0))
                (let ((most (if (>= lw 8) 2 1)))
                  (and (<= (abs (- lw lt)) most) (<= (string-distance word token) most))))
           0.4)
          (t 0))))

(defun lispm-tools-search-score (words fields)
  "How well the search WORDS match FIELDS, a list of (WEIGHT . TEXT).
0 if not at all.  Each word counts with its best match: a field's
WEIGHT times how well the word matches one of the field's words, 1
when they are the same, less when one begins the other, holds it,
or is it misspelt by a letter or two.  The sum is scaled by the
share of WORDS matching at all, so that what matches them all comes
first.  WORDS are as `lispm-tools-search-words' makes them; a TEXT
may be nil."
  (let ((fields (delq nil (mapcar (lambda (field)
                                    (and (stringp (cdr field))
                                         (cons (car field) (lispm-tools--search-tokens (cdr field)))))
                                  fields)))
        (sum 0)
        (matched 0))
    (dolist (word words)
      (let ((best 0))
        (pcase-dolist (`(,weight . ,tokens) fields)
          (dolist (token tokens)
            (let ((score (* weight (lispm-tools--word-match word token))))
              (when (> score best)
                (setq best score)))))
        (when (> best 0)
          (cl-incf matched)
          (cl-incf sum best))))
    (if (zerop matched) 0 (/ (* sum matched) (float (length words))))))

(defun lispm-tools-fields (tool)
  "What a search finds TOOL by, as `lispm-tools-search-score' takes it."
  `((3 . ,(lispm-tool-name tool))
    ;; Older compiled tool definitions construct records without this slot.
    (3 . ,(string-join (condition-case nil (lispm-tool-keywords tool)
                         (args-out-of-range nil)) " "))
    (1.5 . ,(lispm-tools-description tool t))))

(defconst lispm-tools--search-cutoff 0.3
  "The least share of the best score a thing a search finds must score.
What scores less matches by chance, as a word found inside another
does, and would cost a request room for nothing.")

(defun lispm-tools-rank (words candidates limit fields)
  "The CANDIDATES the search WORDS match best, at most LIMIT, best first.
FIELDS is a function of a candidate, what it is found by.  Those
scoring less than `lispm-tools--search-cutoff' of the best are left
out.  With no WORDS, the first LIMIT of the candidates; equal ones
keep their order."
  (if (null words)
      (seq-take candidates limit)
    (let* ((scored (cl-stable-sort
                    (seq-filter (lambda (pair) (> (car pair) 0))
                                (mapcar (lambda (candidate)
                                          (cons (lispm-tools-search-score words (funcall fields candidate))
                                                candidate))
                                        candidates))
                    #'> :key #'car))
           (least (* lispm-tools--search-cutoff (or (caar scored) 0))))
      (mapcar #'cdr (seq-take (seq-take-while (lambda (pair) (>= (car pair) least)) scored) limit)))))

(defun lispm-tools--named-sources (query sources)
  "The SOURCES a word of QUERY names, and QUERY's other words: (NAMED . WORDS)."
  (let* ((words (split-string (downcase query) "[^[:alnum:]_-]+" t))
         (named (seq-filter (lambda (source) (member (downcase (plist-get source :name)) words))
                            sources)))
    (cons named
          (lispm-tools-search-words
           (string-join (seq-difference words (mapcar (lambda (source) (downcase (plist-get source :name)))
                                                      named))
                        " ")))))

(defun lispm-tools--of-sources (tools sources)
  "Those of TOOLS one of SOURCES provides."
  (seq-filter (lambda (tool)
                (seq-some (lambda (source) (funcall (plist-get source :provides) (lispm-tool-name tool)))
                          sources))
              tools))

(defun lispm-tools--find (query tools limit &optional sources)
  "The TOOLS QUERY asks for, at most LIMIT of them.
\"select:A,B\" asks for those tools by name.  Otherwise a word of
QUERY naming one of SOURCES (as `lispm-tool-sources' describes them)
keeps that source's tools only, and the other words rank them, as
`lispm-tools-search-score' does; with no other word, all the source's
tools are found."
  (if (string-prefix-p "select:" query)
      (let ((names (split-string (substring query 7) "[, ]+" t)))
        (seq-filter (lambda (tool) (member (lispm-tool-name tool) names)) tools))
    (pcase-let ((`(,named . ,words) (lispm-tools--named-sources query sources)))
      (lispm-tools-rank words (if named (lispm-tools--of-sources tools named) tools)
                         limit #'lispm-tools-fields))))

(defun lispm-tools-load (agent names)
  "Offer AGENT the deferred tools NAMES from its next request on."
  (let ((new (seq-filter (lambda (name)
                           (and (not (member name (lispm-principal-loaded-tools agent)))
                                (when-let* ((tool (lispm-get-tool name)))
                                  (lispm-tool-deferred tool))))
                         (seq-uniq names))))
    (when new
      (setf (lispm-principal-loaded-tools agent) (append (lispm-principal-loaded-tools agent) new))
      (run-hook-with-args 'lispm-tools-changed-functions agent))))

(defun lispm-tools-adopt (agent before)
  "Make the tools defined since BEFORE deferred tools of AGENT.
BEFORE is the registry as `lispm-tools-snapshot' took it.  A tool
new since then is deferred, so that it is offered to no other agent
until search_tools finds it; one defined again keeps its place,
deferred or in the core layer, unless it is defined as deferred now.
The deferred ones are loaded for AGENT, if non-nil, from its next
request on.  Return their names, sorted."
  (let (names)
    (dolist (tool (lispm-tools-registered))
      (let* ((name (lispm-tool-name tool))
             (old (gethash name before)))
        (unless (eq tool old)
          (unless (lispm-tool-deferred tool)
            (setf (lispm-tool-deferred tool) (or (null old) (lispm-tool-deferred old))))
          (when (lispm-tool-deferred tool)
            (push name names)))))
    (setq names (sort names #'string<))
    (when agent
      (lispm-tools-load agent names))
    names))

(defconst lispm-tools-loaded-prefix "Tools now available: "
  "How a search_tools result starts its list of available tool names.")

(defun lispm-tools-discover (agent query limit &optional predicate)
  "What AGENT's search QUERY finds, at most LIMIT things: (TOOLS . OTHERS).
TOOLS are registered tools AGENT's role gives it, including those
already available; OTHERS are plists of `lispm-tool-search-functions',
such as skills.  A deferred tool one of those holds is not among TOOLS
until AGENT has loaded it: what holds it is found in its stead.
\"select:A,B\" finds the tools A
and B, or what holds them; a word naming a source of tools (see
`lispm-tool-sources') keeps that source's tools only.  When PREDICATE
is non-nil, only tools it accepts are considered, before ranking."
  (let* ((others (let ((lispm-current-agent agent))
                   (mapcan (lambda (function) (copy-sequence (funcall function agent)))
                           lispm-tool-search-functions)))
         (held (mapcan (lambda (other) (copy-sequence (plist-get other :tools))) others))
         (tools (seq-remove (lambda (tool)
                              (let ((name (lispm-tool-name tool)))
                                (or (and (equal name "search_tools")
                                         (not (string-prefix-p "select:" query)))
                                    (and (lispm-tool-deferred tool)
                                         (not (member name (lispm-principal-loaded-tools agent)))
                                         (member name held))
                                    (not (lispm-tools-allowed-p agent name))
                                    (and predicate (not (funcall predicate tool))))))
                            (lispm-tools--registered))))
    (if (string-prefix-p "select:" query)
        (let* ((found (lispm-tools--find query tools limit))
               (names (seq-difference (split-string (substring query 7) "[, ]+" t)
                                      (mapcar #'lispm-tool-name found))))
          (cons found (seq-filter (lambda (other) (seq-intersection names (plist-get other :tools))) others)))
      (pcase-let ((`(,named . ,words) (lispm-tools--named-sources query (lispm-tools-sources))))
        (if named
            (cons (lispm-tools-rank words (lispm-tools--of-sources tools named) limit #'lispm-tools-fields)
                  nil)
          (let ((found (lispm-tools-rank words
                                          (append (mapcar (lambda (tool) (cons 'tool tool)) tools)
                                                  (mapcar (lambda (other) (cons 'other other)) others))
                                          limit
                                          (lambda (candidate)
                                            (if (eq (car candidate) 'tool)
                                                (lispm-tools-fields (cdr candidate))
                                              (plist-get (cdr candidate) :fields))))))
            (cons (mapcar #'cdr (seq-filter (lambda (candidate) (eq (car candidate) 'tool)) found))
                  (mapcar #'cdr (seq-filter (lambda (candidate) (eq (car candidate) 'other)) found)))))))))

(defun lispm-tools-selection-help (agent query found others &optional predicate)
  "Explain unknown names in AGENT's selection QUERY after FOUND and OTHERS.
Suggestions are canonical tool names, never implicit selections.
PREDICATE restricts suggested tools as in `lispm-tools-discover'."
  (when (string-prefix-p "select:" query)
    (when-let* ((missing (seq-difference
                         (split-string (substring query 7) "[, ]+" t)
                         (append (mapcar #'lispm-tool-name found)
                                 (mapcan (lambda (other) (copy-sequence (plist-get other :tools))) others)))))
      (let ((suggestions
             (seq-uniq
              (mapcan (lambda (name)
                        (mapcar #'lispm-tool-name
                                (car (lispm-tools-discover
                                      agent (replace-regexp-in-string "[_-]+" " " name) 3 predicate))))
                      missing))))
        (concat "No accessible tool named " (string-join missing ", ") "."
                (when suggestions
                  (concat " Similar tools: " (string-join suggestions ", ")
                          "; use select: with a canonical name to select one.")))))))

(lispm-register-tool
 (lispm-make-tool
  :name "search_tools"
  :description
  (lambda ()
    (concat "Find tools and skills, including tools already available to you. The skills of this system and its "
            "apps are manuals, each with its tools, which you load with skill. "
            "Search whenever a request needs something you lack, and before you say you cannot "
            "do something. Search by keywords (\"play music\", \"screenshot click\"): a word may "
            "be part of a longer one, or misspelt. Tools found are yours from your next step on, "
            "for the rest of the conversation; a skill's tools come when you load the skill. "
            "A source's name keeps its tools; \"select:NAME,NAME\" asks for tools by name.\n"
            "Sources of tools:\n"
            (mapconcat (lambda (source)
                         (format "- %s: %s" (plist-get source :name) (plist-get source :description)))
                       (lispm-tools-sources) "\n")))
  :input-schema
  '((type . "object")
    (properties (query (type . "string")
                       (description . "Keywords, a source's name, or select:NAME,NAME."))
                (max_results (type . "integer")
                             (description . "How many skills and tools to find for keywords (default 5).")))
    (required . ["query"]))
  :permission 'read
  :async t
  :function
  (lambda (done input)
    (let ((agent lispm-current-agent)
          (query (alist-get 'query input))
          (limit (alist-get 'max_results input)))
      (unless (stringp query) (error "Say what to search for"))
      (lispm-tools-prepare
       (seq-filter (lambda (source) (plist-get source :pending)) (lispm-tools-sources))
       (lambda (problems)
         (pcase-let ((`(,found . ,others)
                      (lispm-tools-discover agent query
                                             (if (integerp limit) limit lispm-tool-search-results))))
           (lispm-tools-load agent (mapcar #'lispm-tool-name found))
           (funcall done
                    (concat (if (or found others)
                                (string-join
                                 (delq nil
                                       (list (and found
                                                  (concat lispm-tools-loaded-prefix
                                                          (mapconcat #'lispm-tool-name found ", ") ".\n"
                                                          (mapconcat (lambda (tool)
                                                                       (format "- %s: %s" (lispm-tool-name tool)
                                                                               (lispm-tools-description tool)))
                                                                     found "\n")))
                                             (and others
                                                  (mapconcat (lambda (other) (plist-get other :line))
                                                             others "\n"))))
                                 "\n")
                              (let ((loaded (seq-filter
                                             (lambda (name) (lispm-tools-allowed-p agent name))
                                             (lispm-principal-loaded-tools agent))))
                                (format "Nothing matches %S; you have loaded: %s." query
                                        (if loaded (string-join loaded ", ") "none"))))
                            (when-let* ((help (lispm-tools-selection-help agent query found others)))
                              (concat "\n" help))
                            (if problems (concat "\n" (string-join problems "\n")) ""))))))))))

(defun lispm-tools--used ()
  "Tools the session in the current buffer loaded with search_tools or called."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil) names)
        (while (re-search-forward
                (concat "^\\*\\* \\(?:[A-Z]+ \\)?\\([^ \n]+\\)\\|^"
                        (regexp-quote lispm-tools-loaded-prefix) "\\(.*\\)\\.$")
                nil t)
          (if (match-beginning 1)
              (push (match-string-no-properties 1) names)
            (setq names (append (reverse (split-string (match-string-no-properties 2) ", " t))
                                names))))
        (delete-dups (nreverse names))))))

(defvar lispm-session-mode)
(defvar lispm-session-agent)

(defun lispm-tools--restore ()
  "Load again the deferred tools the session in this buffer had loaded.
They count as loaded at once, so the session's next request has them,
waiting for them if a server has to start."
  (when-let* ((agent (and (bound-and-true-p lispm-session-mode) lispm-session-agent)))
    (let* ((sources (lispm-tools-sources))
           (names (seq-filter (lambda (name)
                                (let ((tool (lispm-get-tool name)))
                                  (if tool
                                      (lispm-tool-deferred tool)
                                    (seq-some (lambda (source) (funcall (plist-get source :provides) name))
                                              sources))))
                              (lispm-tools--used))))
      (when names
        (setf (lispm-principal-loaded-tools agent)
              (seq-uniq (append (lispm-principal-loaded-tools agent) names)))
        (lispm-tools-prepare
         (seq-filter (lambda (source)
                       (and (plist-get source :pending)
                            (seq-some (plist-get source :provides) names)))
                     sources)
         (lambda (problems)
           (when problems
             (message "LispM: %s" (string-join problems (lispm-tr-in "between the parts of a list" "; "))))))))))

(add-hook 'lispm-session-mode-hook #'lispm-tools--restore)


(provide 'lispm-tool-search)

;;; lispm-tool-search.el ends here
