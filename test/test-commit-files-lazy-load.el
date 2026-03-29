;;; test-commit-files-lazy-load.el --- Tests for lazy-loading commit files -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for lazy-loading commit files when the commit-files section is expanded.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'shipit-core)
(require 'shipit-pr-sections)

(ert-deftest test-shipit--fetch-commit-files ()
  "Verify that shipit--fetch-commit-files fetches files from GitHub API."
  (let* ((repo "owner/repo")
         (commit-sha "abc123")
         (expected-files '(((filename . "file1.el")
                           (status . "modified")
                           (additions . 5)
                           (deletions . 2))))
         (api-response `((files . ,expected-files))))

    ;; Mock the GitHub API request
    (cl-letf (((symbol-function 'shipit--api-request)
              (lambda (endpoint &optional params callback)
                (should (string-match-p "/repos/owner/repo/commits/abc123" endpoint))
                api-response)))

      ;; Call the function
      (let ((result (shipit--fetch-commit-files repo commit-sha)))
        ;; Verify it returns the files
        (should (equal result expected-files))))))

(ert-deftest test-shipit--handle-commit-files-section-visibility-changed ()
  "Verify that visibility hook calls fetch for commit-files sections."
  (let* ((fetch-called nil)
         (fetched-files '(((filename . "file1.el")))))

    ;; Mock dependencies
    (cl-letf (((symbol-function 'shipit--fetch-commit-files)
              (lambda (repo sha)
                (setq fetch-called t)
                fetched-files))
             ((symbol-function 'shipit--render-commit-files-section-body)
              (lambda (section files)
                ;; Just verify it's called
                nil)))

      ;; Test with commit-files section being made visible
      ;; Create a mock section object
      (let ((section (list :type 'commit-files :value "abc123"))
            (parent-section (list :type 'shipit-commit)))
        ;; Temporarily redefine the section type check
        (cl-letf (((symbol-function 'magit-section-type)
                  (lambda (sec) 'commit-files))
                 ((symbol-function 'magit-section-value)
                  (lambda (sec) "abc123"))
                 ((symbol-function 'magit-section-parent)
                  (lambda (sec) parent-section))
                 ((symbol-function 'magit-section-start)
                  (lambda (sec) 1))
                 ((symbol-function 'get-text-property)
                  (lambda (pos prop &optional obj) "owner/repo")))

          ;; Call the handler with visible=t
          (shipit--handle-commit-files-section-visibility-changed section t)

          ;; Verify fetch was called
          (should fetch-called))))))

(provide 'test-commit-files-lazy-load)
;;; test-commit-files-lazy-load.el ends here
