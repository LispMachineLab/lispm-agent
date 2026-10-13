;;; platinum-theme.el --- Mac OS 9's Platinum appearance  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/RyanKung/lispm-emacs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The colours of Mac OS 9's default appearance: windows in platinum
;; greys, documents black on white, the Purple highlight colour behind
;; selected text, and the Lavender variation colour on highlighted menus
;; and selections.  Code is coloured from the same set of twenty
;; variation colours, keywords in Sapphire and comments in Crimson, as the
;; Mac's IDEs of the time coloured them.
;;
;; lispm-emacs's chrome (lme-platinum.el) takes Platinum's greys from
;; `lme-platinum-colors', which this theme sets.

;;; Code:

(deftheme platinum
  "Mac OS 9's Platinum appearance: platinum greys, black on white, Lavender.")

(let* ((white "#FFFFFF")
       (black "#000000")
       ;; Platinum's greys.
       (platinum "#DDDDDD")
       (platinum-light "#EEEEEE")
       (platinum-dark "#BBBBBB")
       (shadow "#888888")
       (frame "#555555")
       (gray "#777777")
       (muted "#999999")
       ;; Highlight colours.
       (purple "#CECEFF")
       (yellow "#FFFF9C")
       (green "#CEFF63")
       ;; Variation colours.
       (lavender "#6363CE")
       (crimson "#CE3131")
       (poppy "#F76B18")
       (gold "#9C9C00")
       (nutmeg "#9C6331")
       (ivy "#316331")
       (emerald "#319C63")
       (teal "#427B7B")
       (sapphire "#3163FF")
       (french-blue "#63639C")
       (plum "#9C639C")
       (magenta "#9C319C"))
  (custom-theme-set-faces
   'platinum
   ;; The document.
   `(default ((t :background ,white :foreground ,black)))
   `(cursor ((t :background ,black)))
   `(fringe ((t :background ,white)))
   `(region ((t :background ,purple :extend t)))
   `(secondary-selection ((t :background ,yellow :extend t)))
   `(highlight ((t :background ,purple)))
   `(hl-line ((t :background "#F4F4FA" :extend t)))
   `(shadow ((t :foreground ,gray)))
   `(line-number ((t :foreground ,muted :background ,white)))
   `(line-number-current-line ((t :foreground ,black :background ,white)))
   `(minibuffer-prompt ((t :foreground ,black :weight bold)))
   `(link ((t :foreground ,sapphire :underline t)))
   `(link-visited ((t :foreground ,plum :underline t)))
   `(button ((t :foreground ,sapphire :underline t)))
   `(error ((t :foreground ,crimson :weight bold)))
   `(warning ((t :foreground ,poppy :weight bold)))
   `(success ((t :foreground ,emerald :weight bold)))
   `(trailing-whitespace ((t :background ,yellow)))
   `(escape-glyph ((t :foreground ,lavender)))
   `(homoglyph ((t :foreground ,lavender)))
   ;; Searching and matching.
   `(isearch ((t :background ,lavender :foreground ,white)))
   `(isearch-fail ((t :background ,crimson :foreground ,white)))
   `(lazy-highlight ((t :background ,yellow)))
   `(match ((t :background ,yellow)))
   `(show-paren-match ((t :background ,purple :weight bold)))
   `(show-paren-mismatch ((t :background ,crimson :foreground ,white)))
   ;; Code.
   `(font-lock-keyword-face ((t :foreground ,sapphire)))
   `(font-lock-comment-face ((t :foreground ,crimson)))
   `(font-lock-comment-delimiter-face ((t :foreground ,crimson)))
   `(font-lock-doc-face ((t :foreground ,nutmeg)))
   `(font-lock-string-face ((t :foreground ,ivy)))
   `(font-lock-constant-face ((t :foreground ,lavender)))
   `(font-lock-builtin-face ((t :foreground ,magenta)))
   `(font-lock-function-name-face ((t :foreground ,teal :weight bold)))
   `(font-lock-variable-name-face ((t :foreground ,french-blue)))
   `(font-lock-type-face ((t :foreground ,plum)))
   `(font-lock-preprocessor-face ((t :foreground ,gold)))
   `(font-lock-negation-char-face ((t :foreground ,crimson)))
   `(font-lock-warning-face ((t :foreground ,poppy :weight bold)))
   ;; Chrome: lispm-emacs derives the rest from `lme-platinum-colors'.
   `(mode-line ((t :background ,platinum :foreground ,black)))
   `(mode-line-inactive ((t :background ,platinum :foreground ,muted)))
   `(header-line ((t :background ,platinum-light :foreground ,black)))
   `(tab-bar ((t :background ,platinum :foreground ,black)))
   `(tab-line ((t :background ,platinum :foreground ,black)))
   `(vertical-border ((t :foreground ,frame)))
   `(window-divider ((t :foreground ,frame)))
   `(window-divider-first-pixel ((t :foreground ,frame)))
   `(window-divider-last-pixel ((t :foreground ,frame)))
   ;; Completion.
   `(completions-common-part ((t :foreground ,lavender :weight bold)))
   `(completions-first-difference ((t :weight bold)))
   `(vertico-current ((t :background ,purple :extend t)))
   `(orderless-match-face-0 ((t :foreground ,lavender :weight bold)))
   `(orderless-match-face-1 ((t :foreground ,teal :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,magenta :weight bold)))
   `(orderless-match-face-3 ((t :foreground ,nutmeg :weight bold)))
   `(marginalia-documentation ((t :foreground ,gray)))
   `(corfu-default ((t :background ,platinum-light :foreground ,black)))
   `(corfu-current ((t :background ,lavender :foreground ,white)))
   `(corfu-border ((t :background ,frame)))
   ;; Diffs and version control.
   `(diff-added ((t :background "#E4FFE4" :foreground ,ivy :extend t)))
   `(diff-removed ((t :background "#FFE4E4" :foreground ,crimson :extend t)))
   `(diff-refine-added ((t :background ,green)))
   `(diff-refine-removed ((t :background "#FFCECE")))
   `(diff-header ((t :background ,platinum-light)))
   `(diff-file-header ((t :background ,platinum :weight bold)))
   `(magit-section-highlight ((t :background "#F4F4FA" :extend t)))
   `(magit-branch-local ((t :foreground ,sapphire)))
   `(magit-branch-remote ((t :foreground ,emerald)))
   `(magit-hash ((t :foreground ,gray)))
   ;; Diagnostics.
   `(flymake-error ((t :underline (:style wave :color ,crimson))))
   `(flymake-warning ((t :underline (:style wave :color ,poppy))))
   `(flymake-note ((t :underline (:style wave :color ,emerald))))
   ;; Outlines.
   `(org-level-1 ((t :foreground ,black :weight bold :height 1.2)))
   `(org-level-2 ((t :foreground ,french-blue :weight bold :height 1.1)))
   `(org-level-3 ((t :foreground ,teal :weight bold)))
   `(org-level-4 ((t :foreground ,plum :weight bold)))
   `(org-block ((t :background "#F7F7F7" :extend t)))
   `(org-code ((t :foreground ,magenta)))
   `(org-todo ((t :foreground ,crimson :weight bold)))
   `(org-done ((t :foreground ,emerald :weight bold)))
   `(markdown-header-face ((t :foreground ,black :weight bold)))
   `(markdown-code-face ((t :background "#F7F7F7" :extend t)))
   ;; Terminals: the ANSI colours are variation colours dark enough to read
   ;; on white, and black stays black.
   `(ansi-color-black ((t :foreground ,black :background ,black)))
   `(ansi-color-red ((t :foreground ,crimson :background ,crimson)))
   `(ansi-color-green ((t :foreground ,emerald :background ,emerald)))
   `(ansi-color-yellow ((t :foreground ,gold :background ,gold)))
   `(ansi-color-blue ((t :foreground ,sapphire :background ,sapphire)))
   `(ansi-color-magenta ((t :foreground ,magenta :background ,magenta)))
   `(ansi-color-cyan ((t :foreground ,teal :background ,teal)))
   `(ansi-color-white ((t :foreground ,platinum-dark :background ,platinum-dark)))
   `(ansi-color-bright-black ((t :foreground ,gray :background ,gray)))
   `(ansi-color-bright-red ((t :foreground ,crimson :background ,crimson)))
   `(ansi-color-bright-green ((t :foreground ,ivy :background ,ivy)))
   `(ansi-color-bright-yellow ((t :foreground ,nutmeg :background ,nutmeg)))
   `(ansi-color-bright-blue ((t :foreground ,lavender :background ,lavender)))
   `(ansi-color-bright-magenta ((t :foreground ,plum :background ,plum)))
   `(ansi-color-bright-cyan ((t :foreground ,teal :background ,teal)))
   `(ansi-color-bright-white ((t :foreground ,platinum :background ,platinum))))

  (custom-theme-set-variables
   'platinum
   `(lme-platinum-colors '(:chrome ,platinum :header ,platinum-light
                           :light ,white :shade ,shadow :frame ,frame
                           :muted ,muted :accent ,lavender))))

(provide-theme 'platinum)

;;; platinum-theme.el ends here
