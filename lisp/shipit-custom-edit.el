;;; shipit-custom-edit.el --- Read values by their Customize type -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; A buffer that edits an option can read each value by the Customize
;; type of the option, without the Customize buffer.  A string, a
;; symbol, a number or a file is read in the minibuffer.  Text, and a
;; list, is edited in a buffer of its own, one item per line.
;;
;; Nothing here knows shipit, so it can become a package of its own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; The parts of a type

(defun shipit-custom-edit-type-head (type)
  "The name of TYPE, such as `string' or `repeat'."
  (if (consp type) (car type) type))

(defun shipit-custom-edit-type-args (type)
  "The types or values inside TYPE, without its keyword properties."
  (let ((rest (and (consp type) (cdr type)))
        (args nil))
    (while rest
      (if (keywordp (car rest))
          (setq rest (cddr rest))
        (push (pop rest) args)))
    (nreverse args)))

(defun shipit-custom-edit-type-property (type prop)
  "The keyword property PROP of TYPE, or nil."
  (let ((rest (and (consp type) (cdr type))))
    (while (and rest (not (eq (car rest) prop)))
      (setq rest (if (keywordp (car rest)) (cddr rest) (cdr rest))))
    (cadr rest)))

(defun shipit-custom-edit-plist-options (type)
  "The options of TYPE, a plist type, as a list of (KEY VALUE-TYPE)."
  (shipit-custom-edit-type-property type :options))

(defun shipit-custom-edit--name (type)
  "What TYPE is called when a choice offers it: its tag, or its name."
  (or (shipit-custom-edit-type-property type :tag)
      (symbol-name (shipit-custom-edit-type-head type))))

;;;; Showing a value

(defun shipit-custom-edit--tag-of (value type)
  "The tag of the constant of TYPE, a choice, whose value is VALUE, or nil."
  (and (eq (shipit-custom-edit-type-head type) 'choice)
       (seq-some (lambda (alternative)
                   (and (eq (shipit-custom-edit-type-head alternative) 'const)
                        (equal value (car (shipit-custom-edit-type-args alternative)))
                        (shipit-custom-edit-type-property alternative :tag)))
                 (shipit-custom-edit-type-args type))))

(defun shipit-custom-edit-format (value &optional type)
  "VALUE, of TYPE, on one line.
A list is joined with commas, text shows its first line, and a constant
of a choice shows its tag."
  (cond
   ((shipit-custom-edit--tag-of value type))
   ((stringp value)
    (let ((lines (split-string value "\n")))
      (if (cdr lines) (concat (car lines) " ...") value)))
   ((consp value) (mapconcat (lambda (item) (format "%s" item)) value ", "))
   (t (format "%s" value))))

;;;; Editing text in a buffer

(defvar-local shipit-custom-edit--callback nil
  "The function that gets the text of this buffer when it is finished.")

(defvar shipit-custom-edit-text-mode-map (make-sparse-keymap)
  "Keymap of `shipit-custom-edit-text-mode'.")

;; The keys are set on every load, not in the `defvar': a reload keeps a
;; keymap that is already defined, so a key added later would be missing.
(define-key shipit-custom-edit-text-mode-map (kbd "C-c C-c") #'shipit-custom-edit-text-finish)
(define-key shipit-custom-edit-text-mode-map (kbd "C-c C-k") #'shipit-custom-edit-text-cancel)

(define-minor-mode shipit-custom-edit-text-mode
  "Edit a value as text.  \\`C-c C-c' keeps it, and \\`C-c C-k' cancels."
  :lighter " Edit")

(defun shipit-custom-edit-text (title text callback)
  "Edit TEXT in a buffer named after TITLE; CALLBACK gets the text kept."
  (let ((buffer (generate-new-buffer (format "*edit: %s*" title))))
    (pop-to-buffer buffer)
    (text-mode)
    (insert (or text ""))
    (goto-char (point-min))
    (shipit-custom-edit-text-mode 1)
    (setq shipit-custom-edit--callback callback)
    (setq header-line-format
          (format " %s   C-c C-c keeps it, C-c C-k cancels" title))))

(defun shipit-custom-edit-text-finish ()
  "Keep the text of this buffer, without the blank lines at its end."
  (interactive)
  (let ((callback shipit-custom-edit--callback)
        (text (string-trim-right (buffer-string))))
    (unless callback
      (error "shipit: this buffer edits no value"))
    (quit-window t)
    (funcall callback text)))

(defun shipit-custom-edit-text-cancel ()
  "Leave this buffer, and keep the value as it was."
  (interactive)
  (quit-window t)
  (message "Nothing changed"))

;;;; The readers

(defun shipit-custom-edit--read-string (prompt _type current callback candidates)
  "Read a string with PROMPT, from CURRENT; CALLBACK gets it.
CANDIDATES, strings, are offered for completion, but any string is taken.
With candidates, CURRENT is the default and not typed in, because text
typed in hides each candidate that does not match it."
  (funcall callback (if candidates
                        (completing-read prompt candidates nil nil nil nil current)
                      (read-string prompt current))))

(defun shipit-custom-edit--title (prompt)
  "PROMPT as the title of a buffer: without the colon at its end."
  (string-remove-suffix ":" (string-trim-right prompt)))

(defun shipit-custom-edit--read-text (prompt _type current callback _candidates)
  "Edit CURRENT, text, in a buffer titled after PROMPT; CALLBACK gets the text."
  (shipit-custom-edit-text (shipit-custom-edit--title prompt) current callback))

(defun shipit-custom-edit--read-symbol (prompt _type current callback candidates)
  "Read a symbol with PROMPT, one of CANDIDATES when there are some.
CALLBACK gets it.  CURRENT is offered first."
  (funcall callback
           (intern (completing-read prompt (mapcar #'symbol-name candidates) nil
                                    (and candidates t) nil nil
                                    (and current (symbol-name current))))))

(defun shipit-custom-edit--read-number (prompt _type current callback _candidates)
  "Read a number with PROMPT, from CURRENT; CALLBACK gets it."
  (funcall callback (read-number prompt current)))

(defun shipit-custom-edit--read-file (prompt _type current callback _candidates)
  "Read a file name with PROMPT, from CURRENT; CALLBACK gets it."
  (funcall callback (abbreviate-file-name (read-file-name prompt nil current nil current))))

(defun shipit-custom-edit--read-const (_prompt type _current callback _candidates)
  "Hand CALLBACK the value of TYPE, a constant."
  (funcall callback (car (shipit-custom-edit-type-args type))))

(defun shipit-custom-edit--read-set (prompt type current callback _candidates)
  "Read some of the constants of TYPE with PROMPT; CALLBACK gets them.
They keep the order of TYPE.  CURRENT are offered first."
  (let* ((values (mapcar (lambda (const) (car (shipit-custom-edit-type-args const)))
                         (shipit-custom-edit-type-args type)))
         (picked (completing-read-multiple
                  prompt (mapcar #'symbol-name values) nil t
                  (and current (mapconcat #'symbol-name current ",")))))
    (funcall callback (seq-filter (lambda (value) (member (symbol-name value) picked))
                                  values))))

(defun shipit-custom-edit--item (type line)
  "LINE as an item of TYPE, the type of the items of a list."
  (if (eq (shipit-custom-edit-type-head type) 'symbol) (intern line) line))

(defun shipit-custom-edit--read-repeat (prompt type current callback _candidates)
  "Edit CURRENT, a list of TYPE, one item per line; CALLBACK gets the list.
The buffer is titled after PROMPT, and an empty line is no item."
  (let ((item-type (car (shipit-custom-edit-type-args type))))
    (shipit-custom-edit-text
     (shipit-custom-edit--title prompt)
     (mapconcat (lambda (item) (format "%s" item)) current "\n")
     (lambda (text)
       (funcall callback
                (mapcar (lambda (line) (shipit-custom-edit--item item-type line))
                        (split-string text "\n" t "[ \t]+")))))))

(defun shipit-custom-edit--read-choice (prompt type current callback candidates)
  "Ask which alternative of TYPE, a choice, and read a value of it.
PROMPT, CURRENT, CALLBACK and CANDIDATES are as for the reader of it."
  (let* ((alternatives (shipit-custom-edit-type-args type))
         (name (completing-read prompt (mapcar #'shipit-custom-edit--name alternatives)
                                nil t))
         (picked (seq-find (lambda (alternative)
                             (equal name (shipit-custom-edit--name alternative)))
                           alternatives)))
    (shipit-custom-edit-read prompt picked current callback candidates)))

(defconst shipit-custom-edit--readers
  '((string . shipit-custom-edit--read-string)
    (text . shipit-custom-edit--read-text)
    (symbol . shipit-custom-edit--read-symbol)
    (integer . shipit-custom-edit--read-number)
    (natnum . shipit-custom-edit--read-number)
    (number . shipit-custom-edit--read-number)
    (file . shipit-custom-edit--read-file)
    (const . shipit-custom-edit--read-const)
    (set . shipit-custom-edit--read-set)
    (repeat . shipit-custom-edit--read-repeat)
    (choice . shipit-custom-edit--read-choice))
  "The reader of each kind of type, by the name of the type.")

(defun shipit-custom-edit-read (prompt type current callback &optional candidates)
  "Read a value of TYPE, a Customize type, and hand it to CALLBACK.
PROMPT asks for it, and CURRENT is the value now.  CANDIDATES are the
symbols a symbol can be, or the strings a string is completed from.
Text and lists are edited in a buffer, so CALLBACK can run after this
returns."
  (let ((reader (alist-get (shipit-custom-edit-type-head type) shipit-custom-edit--readers)))
    (unless reader
      (error "shipit: no reader for the type %S" type))
    (funcall reader prompt type current callback candidates)))

(provide 'shipit-custom-edit)
;;; shipit-custom-edit.el ends here
