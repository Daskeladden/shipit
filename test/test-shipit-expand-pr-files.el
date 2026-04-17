;;; test-shipit-expand-pr-files.el --- Tests for pr-files expansion helper -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for shipit--expand-pr-files-section.  The helper expands the
;; Files Changed root and each file section, and also expands any nested
;; shipit-outdated-comments grandchildren so review comments on outdated
;; code are searchable by text property after expansion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-pr-sections)

(cl-defstruct test-expand--fake-section type children)

(ert-deftest test-expand-pr-files-also-shows-outdated-grandchildren ()
  "GIVEN a pr-files section with a pr-file that has a shipit-outdated-comments child
WHEN shipit--expand-pr-files-section runs
THEN magit-section-show is invoked for the pr-files root, the pr-file, AND
the shipit-outdated-comments grandchild."
  (let* ((outdated (make-test-expand--fake-section
                    :type 'shipit-outdated-comments :children nil))
         (other-grandchild (make-test-expand--fake-section
                            :type 'pr-file-diff :children nil))
         (pr-file (make-test-expand--fake-section
                   :type 'pr-file :children (list outdated other-grandchild)))
         (pr-files (make-test-expand--fake-section
                    :type 'pr-files :children (list pr-file)))
         (shown nil))
    (cl-letf (((symbol-function 'shipit--debug-log) (lambda (&rest _) nil))
              ((symbol-function 'magit-current-section)
               (let ((returned nil))
                 (lambda ()
                   (if returned nil
                     (setq returned t)
                     pr-files))))
              ;; Treat the fake struct as having an `oref'-able `type' and `children'.
              ((symbol-function 'eieio-oref)
               (lambda (obj slot)
                 (pcase slot
                   ('type (test-expand--fake-section-type obj))
                   ('children (test-expand--fake-section-children obj)))))
              ((symbol-function 'magit-section-show)
               (lambda (section) (push (test-expand--fake-section-type section) shown))))
      (with-temp-buffer
        (insert "xxx\n")
        (shipit--expand-pr-files-section)
        ;; WHEN/THEN: pr-files, pr-file, and outdated-comments grandchild are all shown.
        (should (memq 'pr-files shown))
        (should (memq 'pr-file shown))
        (should (memq 'shipit-outdated-comments shown))
        ;; Non-outdated grandchildren should NOT be shown by this helper.
        (should-not (memq 'pr-file-diff shown))))))

(provide 'test-shipit-expand-pr-files)
;;; test-shipit-expand-pr-files.el ends here
