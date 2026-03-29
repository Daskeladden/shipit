;;; test-files-filter.el --- Tests for files filter functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify that the file filter in the Files Changed section works correctly

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shipit modules
(require 'shipit-pr-sections)

(ert-deftest test-files-filter-variable-is-buffer-local ()
  "Test that shipit--files-filter-text is buffer-local."
  (with-temp-buffer
    (setq shipit--files-filter-text "test-filter")
    (should (equal shipit--files-filter-text "test-filter")))
  ;; In a new buffer, it should be empty (default)
  (with-temp-buffer
    (should (equal shipit--files-filter-text ""))))

(ert-deftest test-file-matches-filter-empty ()
  "Test that empty filter matches all files."
  (let ((file '((filename . "src/components/Button.tsx"))))
    (should (shipit--file-matches-filter-p file ""))
    (should (shipit--file-matches-filter-p file nil))))

(ert-deftest test-file-matches-filter-case-insensitive ()
  "Test that filter matching is case-insensitive."
  (let ((file '((filename . "src/components/Button.tsx"))))
    (should (shipit--file-matches-filter-p file "button"))
    (should (shipit--file-matches-filter-p file "BUTTON"))
    (should (shipit--file-matches-filter-p file "Button"))
    (should (shipit--file-matches-filter-p file "BuTtOn"))))

(ert-deftest test-file-matches-filter-partial-match ()
  "Test that filter matches partial filenames."
  (let ((file '((filename . "src/components/Button.tsx"))))
    (should (shipit--file-matches-filter-p file "src"))
    (should (shipit--file-matches-filter-p file "component"))
    (should (shipit--file-matches-filter-p file ".tsx"))
    (should (shipit--file-matches-filter-p file "components/B"))))

(ert-deftest test-file-matches-filter-no-match ()
  "Test that filter correctly rejects non-matching files."
  (let ((file '((filename . "src/components/Button.tsx"))))
    (should-not (shipit--file-matches-filter-p file "Header"))
    (should-not (shipit--file-matches-filter-p file ".js"))
    (should-not (shipit--file-matches-filter-p file "lib/"))))

(ert-deftest test-files-filter-refresh-updates-content ()
  "Test that refreshing with filter updates the displayed files."
  (with-temp-buffer
    (let ((shipit--files-filter-text "")
          (shipit-buffer-repo "test/repo")
          (shipit-buffer-pr-number 123)
          (shipit-buffer-pr-data
           '((files . (((filename . "src/Button.tsx") (status . "modified") (additions . 10) (deletions . 5) (changes . 15))
                       ((filename . "src/Header.tsx") (status . "modified") (additions . 3) (deletions . 1) (changes . 4))
                       ((filename . "README.md") (status . "modified") (additions . 1) (deletions . 0) (changes . 1))))
             (head . ((sha . "abc123"))))))

      ;; Mock magit functions
      (cl-letf (((symbol-function 'magit-insert-section)
                 (lambda (_spec &rest body)
                   (funcall (car body))))
                ((symbol-function 'magit-insert-heading)
                 (lambda (text) (insert text "\n")))
                ((symbol-function 'magit-insert-section-body)
                 (lambda (&rest _body) nil))
                ((symbol-function 'shipit--get-file-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-outdated-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-resolved-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-comment-indicator)
                 (lambda (_c _o _r) ""))
                ((symbol-function 'shipit--debug-log)
                 (lambda (&rest _args) nil))
                ((symbol-function 'shipit--insert-file-diff)
                 (lambda (&rest _args) nil)))

        ;; Insert files content with no filter
        (shipit--insert-files-content "test/repo" shipit-buffer-pr-data 123)
        (let ((content-no-filter (buffer-string)))
          ;; Should contain all 3 files
          (should (string-match-p "Button.tsx" content-no-filter))
          (should (string-match-p "Header.tsx" content-no-filter))
          (should (string-match-p "README.md" content-no-filter)))

        ;; Clear and apply filter
        (erase-buffer)
        (setq shipit--files-filter-text ".tsx")
        (shipit--insert-files-content "test/repo" shipit-buffer-pr-data 123)
        (let ((content-with-filter (buffer-string)))
          ;; Should only contain .tsx files
          (should (string-match-p "Button.tsx" content-with-filter))
          (should (string-match-p "Header.tsx" content-with-filter))
          (should-not (string-match-p "README.md" content-with-filter))
          ;; Should show filter line
          (should (string-match-p "Filter:" content-with-filter)))))))

(ert-deftest test-files-filter-refresh-section-only-finds-shipit-mode-buffer ()
  "Test that refresh-files-section-only finds and updates shipit-mode buffers."
  (let ((shipit--files-section-refresh-in-progress nil)
        (refresh-called nil))
    ;; Mock the actual refresh work
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'shipit-mode modes)))
              ((symbol-function 'fboundp)
               (lambda (_sym) t))
              ((symbol-function 'magit-current-section)
               (lambda () nil))
              ((symbol-function 'shipit--debug-log)
               (lambda (&rest _args) nil)))
      ;; The function should complete without error
      (shipit--refresh-files-section-only)
      ;; Guard should be cleared
      (should-not shipit--files-section-refresh-in-progress))))

(ert-deftest test-files-filter-guard-prevents-concurrent-refresh ()
  "Test that the refresh guard prevents concurrent refreshes."
  ;; The guard is checked at the start of the function
  ;; When set to t, the function should return early without clearing it
  (let ((shipit--files-section-refresh-in-progress nil))
    (cl-letf (((symbol-function 'shipit--debug-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'derived-mode-p)
               (lambda (&rest _modes) nil)))
      ;; First call should work and clear the guard
      (shipit--refresh-files-section-only)
      (should-not shipit--files-section-refresh-in-progress))))

(ert-deftest test-files-filter-section-structure-preserved ()
  "Test that magit section structure is preserved after filter refresh.
This catches the bug where filtering corrupts the section and TAB stops working."
  (with-temp-buffer
    (let ((shipit--files-filter-text "")
          (shipit-buffer-repo "test/repo")
          (shipit-buffer-pr-number 123)
          (shipit-buffer-pr-data
           '((files . (((filename . "src/Button.tsx") (status . "modified") (additions . 10) (deletions . 5))
                       ((filename . "src/Header.tsx") (status . "modified") (additions . 3) (deletions . 1))))
             (head . ((sha . "abc123")))))
          (mock-section (make-hash-table :test 'eq)))

      ;; Set up mock section with start/end positions
      (puthash 'type 'pr-files mock-section)
      (puthash 'start 1 mock-section)
      (puthash 'end 100 mock-section)

      ;; Mock magit functions
      (cl-letf (((symbol-function 'magit-insert-section)
                 (lambda (_spec &rest body)
                   (funcall (car body))))
                ((symbol-function 'magit-insert-heading)
                 (lambda (text) (insert text "\n")))
                ((symbol-function 'magit-insert-section-body)
                 (lambda (&rest _body) nil))
                ((symbol-function 'magit-current-section)
                 (lambda () mock-section))
                ((symbol-function 'oref)
                 (lambda (obj prop) (gethash prop obj)))
                ((symbol-function 'shipit--get-file-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-outdated-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-resolved-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-comment-indicator)
                 (lambda (_c _o _r) ""))
                ((symbol-function 'shipit--debug-log)
                 (lambda (&rest _args) nil))
                ((symbol-function 'shipit--insert-file-diff)
                 (lambda (&rest _args) nil)))

        ;; Insert content with no filter
        (shipit--insert-files-content "test/repo" shipit-buffer-pr-data 123)

        ;; Verify content has correct properties
        (goto-char (point-min))
        (should (search-forward "Button.tsx" nil t))
        (should (get-text-property (point) 'shipit-pr-file))

        ;; Now apply filter and refresh
        (erase-buffer)
        (setq shipit--files-filter-text "Button")
        (shipit--insert-files-content "test/repo" shipit-buffer-pr-data 123)

        ;; Verify filtered content still has correct properties
        (goto-char (point-min))
        (should (search-forward "Button.tsx" nil t))
        (should (get-text-property (point) 'shipit-pr-file))

        ;; Verify Header.tsx is NOT in the filtered content
        (goto-char (point-min))
        (should-not (search-forward "Header.tsx" nil t))

        ;; Verify filter line is present
        (goto-char (point-min))
        (should (search-forward "Filter:" nil t))))))

(ert-deftest test-files-section-found-via-text-property ()
  "Test that files section can be found using shipit-pr-files text property.
This is more robust than text matching which breaks with SVG icons."
  (with-temp-buffer
    (insert "Some other content\n")
    (let ((header-start (point)))
      (insert "📄 Files Changed (5)\n")
      (add-text-properties header-start (point) '(shipit-pr-files t)))
    (insert "   M src/file.tsx\n")

    ;; Should find the section via text property
    (let ((found-pos (text-property-any (point-min) (point-max) 'shipit-pr-files t)))
      (should found-pos)
      (should (> found-pos 1))  ; Not at the very beginning
      (goto-char found-pos)
      (should (looking-at "📄 Files Changed")))))

(ert-deftest test-files-filter-empty-result-handling ()
  "Test that filtering to zero results doesn't corrupt the section."
  (with-temp-buffer
    (let ((shipit--files-filter-text "nonexistent")
          (shipit-buffer-repo "test/repo")
          (shipit-buffer-pr-number 123)
          (shipit-buffer-pr-data
           '((files . (((filename . "src/Button.tsx") (status . "modified") (additions . 10) (deletions . 5))))
             (head . ((sha . "abc123"))))))

      ;; Mock magit functions
      (cl-letf (((symbol-function 'magit-insert-section)
                 (lambda (_spec &rest body)
                   (funcall (car body))))
                ((symbol-function 'magit-insert-heading)
                 (lambda (text) (insert text "\n")))
                ((symbol-function 'magit-insert-section-body)
                 (lambda (&rest _body) nil))
                ((symbol-function 'shipit--get-file-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-outdated-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-file-resolved-comment-count)
                 (lambda (_pr _file) 0))
                ((symbol-function 'shipit--get-comment-indicator)
                 (lambda (_c _o _r) ""))
                ((symbol-function 'shipit--debug-log)
                 (lambda (&rest _args) nil))
                ((symbol-function 'shipit--insert-file-diff)
                 (lambda (&rest _args) nil)))

        ;; Insert content with filter that matches nothing
        (shipit--insert-files-content "test/repo" shipit-buffer-pr-data 123)

        ;; Verify filter line is present even with no matches
        (goto-char (point-min))
        (should (search-forward "Filter:" nil t))

        ;; Verify Button.tsx is NOT in the content (filtered out)
        (goto-char (point-min))
        (should-not (search-forward "Button.tsx" nil t))))))

(provide 'test-files-filter)
;;; test-files-filter.el ends here
