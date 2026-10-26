;;; lispm-form.el --- Forms, written as data  -*- lexical-binding: t; -*-

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

;; A form is written as data: a title and a list of fields, each a key
;; and a plist.
;;
;;   (lispm-form "New Character"
;;     '((title :type text :label "Name" :required t)
;;       (avatar :type picture :label "Picture")
;;       (soul :type text-area :label "Soul" :lines 8)
;;       (thinks :type radio :label "Model"
;;               :choices ((lispm . "LispM's") (endpoint . "Its own")))
;;       (url :type text :label "URL" :required t
;;            :when (lambda (values) (eq (alist-get 'thinks values) 'endpoint))))
;;     :submit "Create"
;;     :on-submit (lambda (values) ...))
;;
;; Fields are of a :type:
;;
;;   text       one line of text
;;   text-area  several lines (:lines, 6 by default)
;;   picture    an image file, shown, chosen with a button
;;   choice     one of :choices, a pop-up menu
;;   radio      one of :choices, a radio button each
;;   check      on or off, a check box
;;   heading    no value: a heading that starts a group of fields
;;
;; and take :label, :help (a line under the field), :value (its value
;; at first), :required (empty is refused), :validate (a function of
;; the value returning nil, or what is wrong), :when (a function of
;; the values: the field is shown only while it returns non-nil) and
;; :width (of a text field, in columns).  :choices is a list of
;; (VALUE . LABEL), or a function returning one.
;;
;; `lispm-form' draws it in a buffer of its own with Emacs's widgets
;; (wid-edit.el), and returns the buffer: TAB and S-TAB go from field to
;; field, Return to the next field (a new line in a text area), C-c C-c
;; submits it, C-c C-k cancels it, the mouse presses its buttons.  Once
;; its values are valid, :on-submit is called with them, an alist, and
;; the form goes, unless it signals an error, which is shown.
;;
;; A form knows nothing of where it is shown.  Whatever shows it may
;; dress it, as a desktop does in its look: `lispm-form-glyph-function'
;; gives the pictures of its buttons, check boxes, radio buttons and
;; pop-up menus, `lispm-form-choose-function' shows a pop-up menu's
;; choices, and `lispm-form-drawn-hook' runs once it is drawn, as
;; `lispm-form-redraw' draws it again, when the look changes, say.
;; Each control's widget says what it is, as the glyph function was
;; asked, in its :lispm-form-control: `check' or `radio' (its state is
;; its value), or (KIND STATE LABEL) for a button or a pop-up menu, so
;; that it can be drawn otherwise, as focused.  Without them, a form is
;; drawn as text, which a text terminal shows too.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-i18n)

(defvar lispm-form-glyph-function nil
  "Function giving the picture of a control of a form, or nil for text.
It is called with KIND, STATE and LABEL:
  KIND `check' or `radio', STATE `on' or `off', LABEL nil;
  KIND `button', STATE nil, or `default' for the button C-c C-c
       presses, LABEL its label;
  KIND `popup', a pop-up menu, STATE nil, LABEL what it shows.
It returns an image, or nil for the control drawn as text.")

(defvar lispm-form-choose-function #'lispm-form-choose
  "Function showing the choices of a pop-up menu of a form; it returns one.
It is called with the field's LABEL, its CHOICES, a list of (VALUE .
TEXT), the VALUE chosen now, and the EVENT that pressed the menu, if
any, with point on the menu.  It returns (VALUE), the choice made, or
nil when none was.")

(defvar lispm-form-drawn-hook nil
  "Normal hook run in a form's buffer each time it is drawn.")

(defface lispm-form-title '((t :inherit bold :height 1.2))
  "Face of a form's title."
  :group 'lispm)

(defface lispm-form-label '((t :inherit bold))
  "Face of the label of a field of a form."
  :group 'lispm)

(defvar-local lispm-form--title nil
  "The title of the form in this buffer.")

(defvar-local lispm-form--fields nil
  "The fields of the form in this buffer, as `lispm-form' takes them.")

(defvar-local lispm-form--values nil
  "The values of the form in this buffer: an alist (KEY . VALUE).")

(defvar-local lispm-form--props nil
  "The properties the form in this buffer was made with.")

(defvar-local lispm-form--widgets nil
  "Alist (KEY . WIDGET): the first widget of each field shown.")

(defvar-keymap lispm-form-mode-map
  :doc "Keymap of `lispm-form-mode'."
  :parent widget-keymap
  "C-c C-c" #'lispm-form-submit
  "C-c C-k" #'lispm-form-cancel)

(define-derived-mode lispm-form-mode nil "Form"
  "A form: fields to fill in, written as data (see `lispm-form').
TAB and S-TAB go from field to field, \\[lispm-form-submit] submits it,
\\[lispm-form-cancel] cancels it.

\\{lispm-form-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;;; Making a form

(cl-defun lispm-form (title fields &key values submit cancel intro on-submit on-cancel)
  "Make a form titled TITLE of FIELDS in a buffer of its own; return the buffer.
FIELDS are as lispm-form.el says.  VALUES, an alist (KEY . VALUE),
are the fields' values at first, before their :value.  SUBMIT and
CANCEL label its buttons, \"OK\" and \"Cancel\" by default; INTRO is
a text before its fields.  ON-SUBMIT is called with its values, once
valid, an alist (KEY . VALUE) of those of the fields shown: a text
trimmed, nil when empty.  ON-CANCEL is called when it is cancelled."
  (let ((buffer (generate-new-buffer (format "*%s*" title))))
    (with-current-buffer buffer
      (lispm-form-mode)
      (setq lispm-form--title title
            lispm-form--fields fields
            lispm-form--props (list :submit submit :cancel cancel :intro intro
                                    :on-submit on-submit :on-cancel on-cancel)
            lispm-form--values
            (mapcar (lambda (field)
                      (let ((key (car field)))
                        (cons key (if (assq key values)
                                      (alist-get key values)
                                    (plist-get (cdr field) :value)))))
                    fields))
      (lispm-form--draw)
      (goto-char (point-min))
      (lispm-form--first-field))
    buffer))

(defun lispm-form--first-field ()
  "Move to the first field of the form, if it has one."
  (when-let* ((widget (seq-some (lambda (field) (alist-get (car field) lispm-form--widgets))
                                lispm-form--fields)))
    (goto-char (or (widget-field-start widget) (widget-get widget :from)))))

(defun lispm-form-value (key)
  "The value of the field KEY of the form in this buffer, as it is now."
  (let ((value (alist-get key lispm-form--values)))
    (if (stringp value)
        (let ((text (string-trim value)))
          (and (not (string-empty-p text)) text))
      value)))

(defun lispm-form--shown-p (field)
  "Non-nil if FIELD of the form in this buffer is shown."
  (let ((when (plist-get (cdr field) :when)))
    (or (null when) (funcall when (lispm-form--all-values)))))

(defun lispm-form--all-values ()
  "The values of every field of the form in this buffer: an alist."
  (mapcar (lambda (field) (cons (car field) (lispm-form-value (car field))))
          lispm-form--fields))

(defun lispm-form-values ()
  "The values of the fields of the form in this buffer that are shown: an alist."
  (mapcar (lambda (field) (cons (car field) (lispm-form-value (car field))))
          (seq-filter (lambda (field)
                        (and (lispm-form--shown-p field)
                             (not (eq (plist-get (cdr field) :type) 'heading))))
                      lispm-form--fields)))

(defun lispm-form--set (key value &optional redraw)
  "Make VALUE the value of the field KEY; with REDRAW, draw the form again."
  (setf (alist-get key lispm-form--values) value)
  (when redraw
    (lispm-form--draw)))

(defun lispm-form--choices (field)
  "The choices of FIELD, (VALUE . LABEL) each."
  (let ((choices (plist-get (cdr field) :choices)))
    (if (functionp choices) (funcall choices) choices)))

;;;; Drawing

(defun lispm-form--glyph (kind state &optional label)
  "The picture of a control, see `lispm-form-glyph-function'; or nil.
Where pictures cannot be shown, the control is drawn as text anyway."
  (when lispm-form-glyph-function
    (funcall lispm-form-glyph-function kind state label)))

(defun lispm-form--button (label action &optional default)
  "Insert a push button LABEL calling ACTION; DEFAULT for the one C-c C-c presses."
  (widget-create 'push-button
                 :tag label
                 :tag-glyph (lispm-form--glyph 'button (and default 'default) label)
                 :lispm-form-control (list 'button (and default 'default) label)
                 :notify (lambda (&rest _) (funcall action))))

(defun lispm-form--key-at (position)
  "The key of the field whose widget is at POSITION, or nil."
  (when-let* ((widget (or (widget-field-at position) (widget-at position))))
    (or (widget-get widget :lispm-form-key)
        (when-let* ((parent (widget-get widget :parent)))
          (widget-get parent :lispm-form-key)))))

(defun lispm-form--draw ()
  "Draw the form in this buffer, point staying in the field it was in."
  (let* ((key (lispm-form--key-at (point)))
         (offset (when-let* ((widget (and key (alist-get key lispm-form--widgets))))
                   (max 0 (- (point) (or (widget-field-start widget) (widget-get widget :from) (point))))))
         (inhibit-read-only t))
    ;; The fields go first: erasing them would change them otherwise.
    (setq widget-field-list nil
          widget-field-new nil
          widget-field-last nil
          lispm-form--widgets nil)
    (erase-buffer)
    (remove-overlays)
    (insert (propertize lispm-form--title 'face 'lispm-form-title) "\n")
    (insert (propertize (lispm-tr "TAB goes to the next field · C-c C-c %s · C-c C-k cancels"
                                  (lispm-form--submit-label))
                        'face 'shadow)
            "\n")
    (when-let* ((intro (plist-get lispm-form--props :intro)))
      (insert "\n" intro "\n"))
    (dolist (field lispm-form--fields)
      (when (lispm-form--shown-p field)
        (lispm-form--draw-field field)))
    (insert "\n")
    (lispm-form--button (or (plist-get lispm-form--props :cancel) (lispm-tr "Cancel"))
                        #'lispm-form-cancel)
    (insert " ")
    (lispm-form--button (lispm-form--submit-label) #'lispm-form-submit t)
    (insert "\n")
    (widget-setup)
    (when-let* ((widget (and key (alist-get key lispm-form--widgets))))
      (goto-char (min (+ (or (widget-field-start widget) (widget-get widget :from)) (or offset 0))
                      (or (widget-field-end widget) (widget-get widget :to)))))
    (run-hooks 'lispm-form-drawn-hook)))

(defun lispm-form--submit-label ()
  "The label of the button submitting the form in this buffer."
  (or (plist-get lispm-form--props :submit) (lispm-tr "OK")))

(defun lispm-form--remember (key widget)
  "Note WIDGET as the first of the field KEY's."
  (widget-put widget :lispm-form-key key)
  (widget-put widget :lispm-form-field t)
  (unless (assq key lispm-form--widgets)
    (push (cons key widget) lispm-form--widgets))
  widget)

(defun lispm-form--draw-field (field)
  "Insert FIELD of the form, its label and its help."
  (pcase-let* ((`(,key . ,props) field)
               (type (plist-get props :type))
               (label (plist-get props :label))
               (value (alist-get key lispm-form--values)))
    (insert "\n")
    (when label
      (insert (propertize label 'face (if (eq type 'heading) 'lispm-form-title 'lispm-form-label))
              (if (plist-get props :required) " *" "")
              "\n"))
    (pcase type
      ('heading nil)
      ('text
       (insert "  ")
       (lispm-form--remember
        key (widget-create 'editable-field
                           :size (or (plist-get props :width) 40)
                           :format "%v"
                           :value (or value "")
                           :action (lambda (&rest _) (widget-forward 1))
                           :notify (lambda (widget &rest _)
                                     (lispm-form--set key (widget-value widget)))))
       (insert "\n"))
      ('text-area
       (let* ((lines (or (plist-get props :lines) 6))
              (text (or value ""))
              (shown (string-trim-right text)))
         (insert "  ")
         (lispm-form--remember
          key (widget-create 'text
                             :format "%v"
                             :value (concat shown (make-string (max 1 (- lines (cl-count ?\n shown))) ?\n))
                             :notify (lambda (widget &rest _)
                                       (lispm-form--set key (widget-value widget)))))))
      ('check
       (insert "  ")
       (lispm-form--remember
        key (widget-create 'toggle
                           :format "%[%v%]"
                           :on "[X]" :off "[ ]"
                           :on-glyph (lispm-form--glyph 'check 'on)
                           :off-glyph (lispm-form--glyph 'check 'off)
                           :lispm-form-control 'check
                           :value value
                           :notify (lambda (widget &rest _)
                                     (lispm-form--set key (widget-value widget) t))))
       (insert " " (or (plist-get props :text) "") "\n"))
      ('radio
       (insert "  ")
       (pcase-dolist (`(,choice . ,text) (lispm-form--choices field))
         (lispm-form--remember
          key (widget-create 'toggle
                             :format "%[%v%]"
                             :on "(*)" :off "( )"
                             :on-glyph (lispm-form--glyph 'radio 'on)
                             :off-glyph (lispm-form--glyph 'radio 'off)
                             :lispm-form-control 'radio
                             :value (equal value choice)
                             :action (lambda (&rest _) (lispm-form--set key choice t))))
         (insert " " text "   "))
       (insert "\n"))
      ('choice
       (let* ((choices (lispm-form--choices field))
              (shown (or (cdr (assoc value choices)) (lispm-tr "Choose…"))))
         (insert "  ")
         (lispm-form--remember
          key (widget-create 'push-button
                             :tag (concat shown " ▾")
                             :tag-glyph (lispm-form--glyph 'popup nil shown)
                             :lispm-form-control (list 'popup nil shown)
                             :notify (lambda (_widget &optional _child event)
                                       (when-let* ((choice (funcall lispm-form-choose-function
                                                                    (or label lispm-form--title)
                                                                    choices value event)))
                                         (lispm-form--set key (car choice) t)))))
         (insert "\n")))
      ('picture
       (insert "  ")
       (if (and value (display-graphic-p) (file-readable-p value))
           (insert-image (create-image value nil nil :max-width 64 :max-height 64 :ascent 'center)
                         (file-name-nondirectory value))
         (insert (propertize (if value (file-name-nondirectory value) (lispm-tr "No picture"))
                             'face 'shadow)))
       (insert "  ")
       (lispm-form--remember
        key (lispm-form--button (lispm-tr "Choose…")
                                (lambda ()
                                  (lispm-form--set key (lispm-form--read-picture value) t))))
       (when value
         (insert " ")
         (lispm-form--button (lispm-tr "Remove") (lambda () (lispm-form--set key nil t))))
       (insert "\n"))
      (_ (error "No type of field %S" type)))
    (when-let* ((help (plist-get props :help)))
      (insert (propertize (concat "  " help) 'face 'shadow) "\n"))))

(defun lispm-form-choose (label choices _value event)
  "Show CHOICES, a list of (VALUE . TEXT), as LABEL's; return (VALUE) chosen.
A menu for EVENT, if it is the mouse's, else Emacs's own way for the
keyboard (see `widget-choose')."
  (widget-choose label
                 (mapcar (lambda (choice) (cons (cdr choice) (list (car choice)))) choices)
                 event))

(defun lispm-form-redraw ()
  "Draw the form in this buffer again, as it is, point staying where it was."
  (when (derived-mode-p 'lispm-form-mode)
    (lispm-form--draw)))

(defun lispm-form--read-picture (old)
  "Read the file of a picture, starting from OLD's directory."
  (let ((file (read-file-name (lispm-tr "Picture: ")
                              (and old (file-name-directory old)) nil t nil
                              (lambda (file)
                                (or (file-directory-p file)
                                    (image-supported-file-p file))))))
    (unless (and (file-regular-p file) (image-supported-file-p file))
      (user-error "%s" (lispm-tr "Not a picture: %s" (abbreviate-file-name file))))
    (expand-file-name file)))

;;;; Submitting and cancelling

(defun lispm-form--problem (field)
  "What is wrong with the value of FIELD of the form in this buffer, or nil."
  (let ((value (lispm-form-value (car field)))
        (props (cdr field)))
    (cond ((and (plist-get props :required) (null value))
           (lispm-tr "%s: fill this in" (or (plist-get props :label) (car field))))
          ((when-let* ((validate (plist-get props :validate))
                       (problem (funcall validate value)))
             (format "%s: %s" (or (plist-get props :label) (car field)) problem))))))

(defun lispm-form-submit ()
  "Submit the form in this buffer: once its values are valid, it is done."
  (interactive)
  (unless (derived-mode-p 'lispm-form-mode)
    (user-error "%s" (lispm-tr "Not a form")))
  (when-let* ((field (seq-find #'lispm-form--problem
                               (seq-filter #'lispm-form--shown-p lispm-form--fields))))
    (when-let* ((widget (alist-get (car field) lispm-form--widgets)))
      (goto-char (or (widget-field-start widget) (widget-get widget :from))))
    (user-error "%s" (lispm-form--problem field)))
  (let ((buffer (current-buffer)))
    (when-let* ((on-submit (plist-get lispm-form--props :on-submit)))
      (funcall on-submit (lispm-form-values)))
    (lispm-form--close buffer)))

(defun lispm-form-cancel ()
  "Cancel the form in this buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (when-let* ((on-cancel (plist-get lispm-form--props :on-cancel)))
      (funcall on-cancel))
    (lispm-form--close buffer)))

(defun lispm-form--close (buffer)
  "Put the form in BUFFER away."
  (when (buffer-live-p buffer)
    (if-let* ((window (get-buffer-window buffer)))
        (quit-window t window)
      (kill-buffer buffer))))

(provide 'lispm-form)
;;; lispm-form.el ends here
