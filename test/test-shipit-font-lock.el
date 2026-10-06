;;; test-shipit-font-lock.el --- Tests that font-lock leaves rendered faces alone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; shipit renders markdown before it puts it in a buffer, and the faces
;; of the rendering are in the `face' property of the text.  The section
;; mode turns font-lock on, and font-lock takes that property off the
;; text it runs over.  These tests are for the faces staying.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-buffer)

(defun test-font-lock--face-after-font-lock (setup)
  "The face that rendered text still has after font-lock ran over it.
SETUP is called in the new buffer, to make it a buffer of some mode."
  (with-temp-buffer
    (funcall setup)
    (let ((inhibit-read-only t))
      (insert (propertize "A heading" 'face 'bold) "\n"))
    (font-lock-ensure)
    (get-text-property (point-min) 'face)))

(ert-deftest test-font-lock-the-section-mode-alone-takes-faces-off ()
  "GIVEN a buffer of the section mode, with nothing of shipit
WHEN font-lock runs over text with a face
THEN the face is gone.  This is why shipit sets font-lock up itself: a
section mode that keeps the face makes that setup needless."
  (should-not (test-font-lock--face-after-font-lock #'magit-section-mode)))

(ert-deftest test-font-lock-a-shipit-buffer-keeps-the-faces-of-its-text ()
  "GIVEN a buffer of the section mode with the defaults that every
shipit mode applies
WHEN font-lock runs over text with a face
THEN the text still has its face."
  (should (eq 'bold (test-font-lock--face-after-font-lock
                     (lambda ()
                       (magit-section-mode)
                       (shipit--apply-section-defaults))))))

(ert-deftest test-font-lock-a-pull-request-buffer-keeps-the-faces-of-its-text ()
  "GIVEN a pull request buffer
WHEN font-lock runs over text with a face, as rendered markdown has
THEN the text still has its face."
  (should (eq 'bold (test-font-lock--face-after-font-lock #'shipit-mode))))

(provide 'test-shipit-font-lock)
;;; test-shipit-font-lock.el ends here
