;;; en-GB.eld --- LispM Agent's words in British English  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; See lispm-i18n.el.  Each entry is (MESSAGE . TRANSLATION), a
;; message said in a context (CONTEXT . MESSAGE), a plural's translation
;; the list of its forms; in the order the code writes them.  The code's
;; English spells as Britain does: only its words change, as Bin for
;; Trash.

(:name "English (UK)"
 :messages
 (("Move the %s session “%s” to the Trash?" . "Move the %s session “%s” to the Bin?")
  ("Its %d file goes to the Trash." "Its %d file goes to the Bin." "Its %d files go to the Bin.")
  ("Move to Trash" . "Move to Bin")))
