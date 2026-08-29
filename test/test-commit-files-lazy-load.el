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

(defun test-lazy--section (type)
  "Return a magit section of TYPE.
Only a few of the slots take a value at construction, so the rest are
set afterwards.  A real section is used rather than something shaped
like one: standing in for the accessors is what let three calls to
accessors magit does not have go unnoticed."
  (let ((section (magit-section :type type)))
    section))

(ert-deftest test-shipit--handle-commit-files-section-visibility-changed ()
  "GIVEN a commit-files section being expanded, under a commit heading
that carries the repository and the pull request number
WHEN the visibility handler runs
THEN the files are fetched for the commit the section names, and
rendered with both of those.

The section is a real one, and its slots are read the way the handler
reads them."
  (let ((fetch-called nil)
        (rendered nil)
        (fetched-files '(((filename . "file1.el")))))
    (cl-letf (((symbol-function 'shipit--fetch-commit-files)
               (lambda (repo sha)
                 (setq fetch-called (list repo sha))
                 fetched-files))
              ((symbol-function 'shipit--render-commit-files-section-body)
               (lambda (_section files pr-number)
                 (setq rendered (list files pr-number)))))
      (with-temp-buffer
        (insert (propertize "commit heading\n"
                            'shipit-repo "owner/repo"
                            'shipit-pr-number 42))
        (let ((parent (test-lazy--section 'shipit-commit))
              (section (test-lazy--section 'commit-files)))
          (oset parent start (point-min))
          (oset section value "abc123")
          (oset section parent parent)
          (shipit--handle-commit-files-section-visibility-changed section t)
          (should (equal '("owner/repo" "abc123") fetch-called))
          (should (equal (list fetched-files 42) rendered)))))))

(provide 'test-commit-files-lazy-load)
;;; test-commit-files-lazy-load.el ends here
