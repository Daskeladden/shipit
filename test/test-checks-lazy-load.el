;;; test-checks-lazy-load.el --- Tests for lazy-loading checks section -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for lazy-loading checks when the checks section is expanded via TAB.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'shipit-core)
(require 'shipit-checks)

(ert-deftest test-checks-lazy-load-hook-registered ()
  "Verify that the post-command hook for lazy-loading checks is registered."
  (should (member #'shipit--check-checks-section-expansion post-command-hook)))

(ert-deftest test-shipit--check-checks-section-expansion-detects-checks-section ()
  "Verify that check-checks-section-expansion detects checks section expansion."
  (let ((expansion-detected nil))
    ;; Mock magit-current-section to return a checks section
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (list :dummy-section)))
              ((symbol-function 'magit-section-type)
               (lambda (sec) 'checks))
              ((symbol-function 'magit-section-hidden)
               (lambda (sec) nil))
              ((symbol-function 'shipit--handle-checks-section-visibility-changed)
               (lambda (section visible)
                 (setq expansion-detected (and (eq (magit-section-type section) 'checks)
                                               (not (magit-section-hidden section)))))))

      (shipit--check-checks-section-expansion)
      (should expansion-detected))))

(ert-deftest test-shipit--handle-checks-section-visibility-changed-function-exists ()
  "Verify that handle-checks-section-visibility-changed function is defined."
  (should (fboundp 'shipit--handle-checks-section-visibility-changed))
  (should (functionp #'shipit--handle-checks-section-visibility-changed)))

(ert-deftest test-checks-section-shows-loading-while-fetching ()
  "Verify that checks section shows loading state during fetch."
  (with-temp-buffer
    ;; Mock mode
    (derived-mode-p 'shipit-mode)

    ;; Set up PR context
    (setq shipit-current-repo "owner/repo")
    (let* ((repo "owner/repo")
           (pr-number 42)
           ;; Simulate uncached state
           (shipit--checks-fetched nil)
           (shipit--checks-loading nil))

      ;; Verify initial state shows lazy loading message
      (cl-letf (((symbol-function 'shipit-get-pull-request)
                 (lambda (pr-num repo) '((state . "open")))))
        (shipit--insert-pr-checks-enabled repo pr-number)
        ;; Buffer should contain "expand to load" message for uncached state
        (should (string-match-p "expand to load" (buffer-string)))))))

(provide 'test-checks-lazy-load)
;;; test-checks-lazy-load.el ends here
