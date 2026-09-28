;;; test-shipit-custom-edit.el --- Reading values by their Customize type -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; `shipit-custom-edit-read' reads a value for a Customize type without
;; Customize.  The minibuffer functions stand in for what a person types.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'shipit-custom-edit)

(defmacro test-custom-edit--typing (answers &rest body)
  "Run BODY with the minibuffer answering ANSWERS, a plist.
Its keys: `:string', `:choice', `:choices' and `:number'."
  (declare (indent 1))
  `(let ((answers ,answers))
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _) (plist-get answers :string)))
               ((symbol-function 'completing-read)
                (lambda (&rest _) (plist-get answers :choice)))
               ((symbol-function 'completing-read-multiple)
                (lambda (&rest _) (plist-get answers :choices)))
               ((symbol-function 'read-number)
                (lambda (&rest _) (plist-get answers :number))))
       ,@body)))

(defun test-custom-edit--read (type &optional current candidates)
  "The value read for TYPE, with CURRENT and CANDIDATES, handed on at once."
  (let ((result 'not-called))
    (shipit-custom-edit-read "Value: " type current
                             (lambda (value) (setq result value))
                             candidates)
    result))

(defun test-custom-edit--edit-buffer ()
  "The buffer that edits text, or nil."
  (seq-find (lambda (buffer) (buffer-local-value 'shipit-custom-edit-text-mode buffer))
            (buffer-list)))

(ert-deftest test-custom-edit-a-string-is-read-in-the-minibuffer ()
  "GIVEN the type `string'
WHEN a value is read
THEN it is what was typed."
  (test-custom-edit--typing '(:string "small")
    (should (equal "small" (test-custom-edit--read 'string "medium")))))

(ert-deftest test-custom-edit-a-string-can-be-completed-from-candidates ()
  "GIVEN the type `string', and candidates
WHEN a value is read
THEN the candidates are offered, and a value that is not one of them
is taken too."
  (let ((offered nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate require-match &rest _)
                 (setq offered (list collection require-match))
                 "medium-9")))
      (should (equal "medium-9"
                     (test-custom-edit--read 'string "medium" '("medium" "large")))))
    (should (equal '(("medium" "large") nil) offered))))

(ert-deftest test-custom-edit-every-candidate-of-a-string-shows ()
  "GIVEN the type `string', a current value, and candidates
WHEN a value is read
THEN the current value is the default, not text typed in already, so
no candidate is filtered out, and RET keeps the current value."
  (let ((args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest rest) (setq args rest) "large")))
      (test-custom-edit--read 'string "medium" '("medium" "large")))
    (should-not (nth 4 args))
    (should (equal "medium" (nth 6 args)))))

(ert-deftest test-custom-edit-a-symbol-is-one-of-the-candidates ()
  "GIVEN the type `symbol', and candidates
WHEN one is picked
THEN the value is that symbol."
  (test-custom-edit--typing '(:choice "beta")
    (should (eq 'beta (test-custom-edit--read 'symbol nil '(alpha beta))))))

(ert-deftest test-custom-edit-a-set-keeps-the-order-of-its-type ()
  "GIVEN a set of constants
WHEN some are picked, in another order
THEN the value holds them in the order of the type."
  (test-custom-edit--typing '(:choices ("files" "description"))
    (should (equal '(description files)
                   (test-custom-edit--read '(set (const description) (const commits)
                                                 (const files)))))))

(ert-deftest test-custom-edit-a-number-is-read-as-a-number ()
  "GIVEN the type `integer'
WHEN a value is read
THEN it is the number typed."
  (test-custom-edit--typing '(:number 5000)
    (should (= 5000 (test-custom-edit--read 'integer)))))

(ert-deftest test-custom-edit-a-choice-asks-which-alternative ()
  "GIVEN a choice of a number and a constant with a tag
WHEN the constant is picked, and then the number
THEN the value is the constant, and then the number typed."
  (let ((type '(choice integer (const :tag "Never ask" nil))))
    (test-custom-edit--typing '(:choice "Never ask")
      (should (null (test-custom-edit--read type 100))))
    (test-custom-edit--typing '(:choice "integer" :number 7)
      (should (= 7 (test-custom-edit--read type))))))

(ert-deftest test-custom-edit-text-is-edited-in-a-buffer ()
  "GIVEN the type `text', and a current value
WHEN the text buffer opens, is changed, and is finished
THEN the value is the new text, and the buffer is gone."
  (let ((value 'not-called))
    (shipit-custom-edit-read "Instructions" 'text "Old."
                             (lambda (new) (setq value new)))
    (with-current-buffer (test-custom-edit--edit-buffer)
      (should (equal "Old." (buffer-string)))
      (erase-buffer)
      (insert "Look for bugs.\nBe brief.\n")
      (shipit-custom-edit-text-finish))
    (should (equal "Look for bugs.\nBe brief." value))
    (should-not (test-custom-edit--edit-buffer))))

(ert-deftest test-custom-edit-cancelled-text-changes-nothing ()
  "GIVEN the text buffer of a value
WHEN it is cancelled
THEN nothing is handed on, and the buffer is gone."
  (let ((value 'not-called))
    (shipit-custom-edit-read "Instructions" 'text "Old."
                             (lambda (new) (setq value new)))
    (with-current-buffer (test-custom-edit--edit-buffer)
      (shipit-custom-edit-text-cancel))
    (should (eq 'not-called value))
    (should-not (test-custom-edit--edit-buffer))))

(ert-deftest test-custom-edit-a-list-is-edited-one-item-per-line ()
  "GIVEN a list of strings
WHEN its text buffer is changed and finished
THEN each line that is not empty is an item."
  (let ((value 'not-called))
    (shipit-custom-edit-read "Skip files" '(repeat string) '("*.lock")
                             (lambda (new) (setq value new)))
    (with-current-buffer (test-custom-edit--edit-buffer)
      (should (equal "*.lock" (buffer-string)))
      (goto-char (point-max))
      (insert "\n\nvendor/*\n")
      (shipit-custom-edit-text-finish))
    (should (equal '("*.lock" "vendor/*") value))))

(ert-deftest test-custom-edit-an-unknown-type-is-an-error ()
  "GIVEN a type no reader knows
WHEN a value of it is read
THEN that fails, and says which type."
  (should (string-match-p "no reader for the type"
                          (cadr (should-error (test-custom-edit--read '(vector string)))))))

(ert-deftest test-custom-edit-the-options-of-a-plist-type ()
  "GIVEN a plist type with options
WHEN its options are asked for
THEN they are the keys with their types, in order."
  (should (equal '((:model string) (:parts (set (const files))))
                 (shipit-custom-edit-plist-options
                  '(plist :tag "Configuration"
                          :options ((:model string) (:parts (set (const files)))))))))

(ert-deftest test-custom-edit-a-value-shows-on-one-line ()
  "GIVEN values of several kinds
WHEN they are shown
THEN a list is joined with commas, and text shows its first line."
  (should (equal "description, files" (shipit-custom-edit-format '(description files))))
  (should (equal "Look for bugs. ..." (shipit-custom-edit-format "Look for bugs.\nBe brief.")))
  (should (equal "5000" (shipit-custom-edit-format 5000))))

(ert-deftest test-custom-edit-a-constant-of-a-choice-shows-its-tag ()
  "GIVEN a choice with a constant that has a tag
WHEN the value of that constant is shown
THEN it shows as the tag."
  (should (equal "Never ask"
                 (shipit-custom-edit-format nil '(choice integer (const :tag "Never ask" nil)))))
  (should (equal "70" (shipit-custom-edit-format 70 '(choice integer (const :tag "Never ask" nil))))))

(provide 'test-shipit-custom-edit)
;;; test-shipit-custom-edit.el ends here
