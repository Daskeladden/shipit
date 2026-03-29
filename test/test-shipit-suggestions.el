;;; test-shipit-suggestions.el --- Tests for suggestion rendering and applying -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for GitHub suggestion block rendering and application.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-render)
(require 'shipit-pr-backends)
(require 'shipit-pr-actions)

;;; --- Suggestion Detection Tests ---

(ert-deftest test-shipit-extract-code-snippets-detects-suggestion ()
  "GIVEN a comment body with a suggestion code block
WHEN calling shipit--extract-code-snippets
THEN the snippet has lang starting with \"suggestion\"."
  (let* ((body "Please change this:\n```suggestion\nnew code here\n```\nThanks!")
         (snippets (shipit--extract-code-snippets body)))
    ;; THEN one snippet is found with lang="suggestion"
    (should (= 1 (length snippets)))
    (should (string= "suggestion" (plist-get (car snippets) :lang)))
    (should (string= "new code here" (plist-get (car snippets) :code)))))

(ert-deftest test-shipit-extract-code-snippets-regular-code-block ()
  "GIVEN a comment body with a regular code block (not suggestion)
WHEN calling shipit--extract-code-snippets
THEN the snippet has the correct language, not \"suggestion\"."
  (let* ((body "Example:\n```python\nprint('hello')\n```\n")
         (snippets (shipit--extract-code-snippets body)))
    (should (= 1 (length snippets)))
    (should (string= "python" (plist-get (car snippets) :lang)))))

(ert-deftest test-shipit-extract-code-snippets-multiline-suggestion ()
  "GIVEN a comment body with a multi-line suggestion
WHEN calling shipit--extract-code-snippets
THEN the suggestion code contains all lines."
  (let* ((body "Fix:\n```suggestion\nline one\nline two\nline three\n```\n")
         (snippets (shipit--extract-code-snippets body)))
    (should (= 1 (length snippets)))
    (should (string= "suggestion" (plist-get (car snippets) :lang)))
    (should (string= "line one\nline two\nline three" (plist-get (car snippets) :code)))))

(ert-deftest test-shipit-extract-code-snippets-empty-suggestion ()
  "GIVEN a comment body with an empty suggestion (delete line)
WHEN calling shipit--extract-code-snippets
THEN the snippet is detected with empty code."
  (let* ((body "Remove this:\n```suggestion\n```\nDone")
         (snippets (shipit--extract-code-snippets body)))
    ;; THEN one snippet is found with empty code
    (should (= 1 (length snippets)))
    (should (string= "suggestion" (plist-get (car snippets) :lang)))
    (should (string= "" (plist-get (car snippets) :code)))))

(ert-deftest test-shipit-extract-code-snippets-empty-regular-block ()
  "GIVEN a comment body with an empty regular code block
WHEN calling shipit--extract-code-snippets
THEN the snippet is detected with empty code."
  (let* ((body "Empty:\n```python\n```\n")
         (snippets (shipit--extract-code-snippets body)))
    (should (= 1 (length snippets)))
    (should (string= "python" (plist-get (car snippets) :lang)))
    (should (string= "" (plist-get (car snippets) :code)))))

;;; --- Suggestion Rendering Tests ---

(ert-deftest test-shipit-render-suggestion-block-single-line ()
  "GIVEN a single-line suggestion with old code from diff hunk
WHEN calling shipit--render-suggestion-block
THEN output has old line with - prefix and new line with + prefix."
  (let ((result (shipit--render-suggestion-block "new code here" '("old code here"))))
    ;; THEN result contains diff-style output
    (should (string-match-p "Suggested change:" result))
    (should (string-match-p "^- old code here$" result))
    (should (string-match-p "^\\+ new code here$" result))))

(ert-deftest test-shipit-render-suggestion-block-multiline ()
  "GIVEN a multi-line suggestion replacing multiple old lines
WHEN calling shipit--render-suggestion-block
THEN all old lines have - prefix and all new lines have + prefix."
  (let ((result (shipit--render-suggestion-block
                 "new line 1\nnew line 2"
                 '("old line 1" "old line 2"))))
    (should (string-match-p "^- old line 1$" result))
    (should (string-match-p "^- old line 2$" result))
    (should (string-match-p "^\\+ new line 1$" result))
    (should (string-match-p "^\\+ new line 2$" result))))

(ert-deftest test-shipit-render-suggestion-block-no-old-lines ()
  "GIVEN a suggestion with no extractable old lines
WHEN calling shipit--render-suggestion-block with nil old-lines
THEN only new lines are shown with + prefix."
  (let ((result (shipit--render-suggestion-block "new code" nil)))
    (should (string-match-p "Suggested change:" result))
    (should (string-match-p "^\\+ new code$" result))
    (should-not (string-match-p "^-" result))))

(ert-deftest test-shipit-render-suggestion-block-has-faces ()
  "GIVEN a suggestion block
WHEN rendered
THEN removed lines have diff-removed face and added lines have diff-added face."
  (let ((result (shipit--render-suggestion-block "new" '("old"))))
    (with-temp-buffer
      (insert result)
      (goto-char (point-min))
      ;; Find the line with "- old"
      (search-forward "- old")
      (should (eq 'diff-removed (get-text-property (match-beginning 0) 'face)))
      ;; Find the line with "+ new"
      (search-forward "+ new")
      (should (eq 'diff-added (get-text-property (match-beginning 0) 'face))))))

;;; --- render-comment-with-code Suggestion Integration Tests ---

(ert-deftest test-shipit-render-comment-with-code-suggestion ()
  "GIVEN a comment body containing a suggestion block
WHEN calling shipit--render-comment-with-code
THEN the suggestion is rendered with diff-style formatting instead of [suggestion] label."
  (let ((body "Please fix:\n```suggestion\nnew code\n```\nThanks"))
    (let ((result (shipit--render-comment-with-code body nil)))
      ;; THEN should have "Suggested change:" label, not "[suggestion]"
      (should (string-match-p "Suggested change:" result))
      (should-not (string-match-p "\\[suggestion\\]" result)))))

(ert-deftest test-shipit-render-comment-with-code-non-suggestion ()
  "GIVEN a comment body with a regular code block
WHEN calling shipit--render-comment-with-code
THEN the code block is rendered with [lang] label as before."
  (let ((body "Example:\n```python\nprint('hi')\n```\n"))
    (let ((result (shipit--render-comment-with-code body nil)))
      ;; THEN should have [python] label, not "Suggested change:"
      (should (string-match-p "\\[python\\]" result))
      (should-not (string-match-p "Suggested change:" result)))))

;;; --- render-comment-body Full Pipeline Tests ---

(ert-deftest test-shipit-render-comment-body-suggestion-has-faces ()
  "GIVEN a comment with a suggestion block going through the full render pipeline
WHEN calling shipit--render-comment-body (without markdown)
THEN the output has diff-removed and diff-added faces on the suggestion lines."
  (let* ((comment `((body . "Fix:\n```suggestion\nnew code\n```\nDone")
                    (diff_hunk . "@@ -5,3 +5,3 @@\n context\n old code\n after")
                    (line . 6)
                    (path . "test.el")))
         (shipit-render-markdown nil)
         (result (shipit--render-comment-body comment 4)))
    ;; THEN suggestion diff block is present with faces
    (with-temp-buffer
      (insert result)
      (goto-char (point-min))
      (should (search-forward "Suggested change:" nil t))
      (should (search-forward "+ new code" nil t))
      (should (eq 'diff-added (get-text-property (match-beginning 0) 'face))))))

(ert-deftest test-shipit-render-comment-body-suggestion-with-old-lines ()
  "GIVEN a comment with suggestion and a diff hunk containing the old line
WHEN rendering through the full pipeline
THEN both old (- prefixed) and new (+ prefixed) lines appear."
  (let* ((comment `((body . "Change:\n```suggestion\nnew line\n```\n")
                    (diff_hunk . "@@ -10,3 +10,3 @@\n before\n old line\n after")
                    (line . 11)
                    (path . "test.el")))
         (shipit-render-markdown nil)
         (result (shipit--render-comment-body comment 0)))
    (should (string-match-p "- old line" result))
    (should (string-match-p "\\+ new line" result))))

(ert-deftest test-shipit-render-comment-body-suggestion-fallback-to-file ()
  "GIVEN a comment with suggestion but no diff hunk at all
WHEN the local file exists with the target lines
THEN old lines are read from the file."
  (let* ((temp-dir (make-temp-file "shipit-test" t))
         (temp-file (expand-file-name "src/file.el" temp-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" temp-dir) t)
          (with-temp-file temp-file
            (insert "line1\nold code here\nline3\n"))
          (let* ((comment `((body . "Fix:\n```suggestion\nnew code\n```\n")
                            (line . 2)
                            (path . "src/file.el")))
                 (shipit-render-markdown nil))
            (cl-letf (((symbol-function 'shipit--get-repo-root) (lambda () temp-dir)))
              (let ((result (shipit--render-comment-body comment 0)))
                (should (string-match-p "- old code here" result))
                (should (string-match-p "\\+ new code" result))))))
      (delete-directory temp-dir t))))

(ert-deftest test-shipit-render-comment-body-suggestion-survives-markdown ()
  "GIVEN a comment with suggestion going through markdown rendering
WHEN calling shipit--render-comment-body with markdown enabled
THEN placeholder is correctly replaced and faces are preserved."
  (let* ((comment `((body . "Fix:\n```suggestion\nnew code\n```\nDone")
                    (diff_hunk . "@@ -5,3 +5,3 @@\n context\n old code\n after")
                    (line . 6)
                    (path . "test.el")))
         (shipit-render-markdown t)
         (result (shipit--render-comment-body comment 4)))
    ;; THEN no raw placeholder visible
    (should-not (string-match-p "SHIPITSUGGEST" result))
    ;; AND suggestion content is present
    (with-temp-buffer
      (insert result)
      (goto-char (point-min))
      (should (search-forward "Suggested change:" nil t))
      (should (search-forward "+ new code" nil t)))))

;;; --- Extract Lines from Diff Hunk Tests ---

(ert-deftest test-shipit-extract-lines-from-diff-hunk-single-line ()
  "GIVEN a diff hunk with a target line
WHEN calling shipit--extract-lines-from-diff-hunk for that line
THEN the correct line content is returned."
  (let ((hunk "@@ -10,3 +10,3 @@\n context before\n-old line\n+new line\n context after"))
    ;; WHEN extracting lines 10..10 (the context line maps to new-file line 10)
    ;; Line 10 = "context before", line 11 = "new line" (added), line 12 = "context after"
    (let ((result (shipit--extract-lines-from-diff-hunk hunk 11 11)))
      (should (= 1 (length result)))
      (should (string= "new line" (car result))))))

(ert-deftest test-shipit-extract-lines-from-diff-hunk-multi-line ()
  "GIVEN a diff hunk spanning multiple lines
WHEN extracting a range of lines
THEN all lines in the range are returned."
  (let ((hunk "@@ -5,4 +5,4 @@\n line A\n line B\n line C\n line D"))
    ;; Lines 5-8 are context lines: A, B, C, D
    (let ((result (shipit--extract-lines-from-diff-hunk hunk 5 8)))
      (should (= 4 (length result)))
      (should (string= "line A" (car result)))
      (should (string= "line D" (nth 3 result))))))

(ert-deftest test-shipit-extract-lines-from-diff-hunk-nil-hunk ()
  "GIVEN a nil diff hunk
WHEN calling shipit--extract-lines-from-diff-hunk
THEN nil is returned."
  (should (null (shipit--extract-lines-from-diff-hunk nil 1 1))))

(ert-deftest test-shipit-extract-lines-from-diff-hunk-fallback-last-lines ()
  "GIVEN a diff hunk where exact line matching fails
WHEN calling shipit--extract-lines-from-diff-hunk
THEN falls back to the last N lines from the new-file side."
  ;; Hunk starts at +40 but we ask for line 44 which is beyond the hunk content
  (let ((hunk "@@ -40,3 +40,3 @@\n context_a\n context_b\n target_line"))
    ;; Lines: 40=context_a, 41=context_b, 42=target_line
    ;; Asking for line 44 (doesn't match), but 1 line wanted → get last line
    (let ((result (shipit--extract-lines-from-diff-hunk hunk 44 44)))
      (should (= 1 (length result)))
      (should (string= "target_line" (car result))))))

;;; --- Apply Suggestion Tests ---

(ert-deftest test-shipit-apply-suggestion-no-suggestion-at-point ()
  "GIVEN point is not on a suggestion
WHEN calling shipit-apply-suggestion-at-point
THEN a user-error is signaled."
  (with-temp-buffer
    (insert "Just regular text")
    (goto-char (point-min))
    (should-error (shipit-apply-suggestion-at-point) :type 'user-error)))

(ert-deftest test-shipit-apply-suggestion-wrong-branch ()
  "GIVEN point is on a suggestion but current branch doesn't match PR head
WHEN calling shipit-apply-suggestion-at-point
THEN a user-error about branch mismatch is signaled."
  (with-temp-buffer
    (insert (propertize "suggestion text"
                        'shipit-suggestion t
                        'shipit-suggestion-code "new code"
                        'shipit-suggestion-path "file.el"
                        'shipit-suggestion-start-line 10
                        'shipit-suggestion-end-line 10))
    (goto-char (point-min))
    ;; Mock the PR data with head branch "feature-branch"
    (let ((shipit--current-displayed-pr '((head . ((ref . "feature-branch"))))))
      (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "main")))
        (should-error (shipit-apply-suggestion-at-point) :type 'user-error)))))

(ert-deftest test-shipit-apply-suggestion-replaces-lines ()
  "GIVEN a suggestion with replacement code and a target file
WHEN applying the suggestion
THEN the correct lines in the file are replaced."
  (let* ((temp-dir (make-temp-file "shipit-test" t))
         (temp-file (expand-file-name "test-file.el" temp-dir))
         (original-content "line 1\nline 2\nold code\nline 4\nline 5\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert original-content))
          (with-temp-buffer
            (insert (propertize "suggestion text"
                                'shipit-suggestion t
                                'shipit-suggestion-code "new code"
                                'shipit-suggestion-path "test-file.el"
                                'shipit-suggestion-start-line 3
                                'shipit-suggestion-end-line 3))
            (goto-char (point-min))
            ;; Mock branch check, repo root, and y-or-n-p
            (let ((shipit--current-displayed-pr
                   `((head . ((ref . "feature"))))))
              (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "feature"))
                        ((symbol-function 'shipit--get-repo-root) (lambda () temp-dir))
                        ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
                (shipit-apply-suggestion-at-point))))
          ;; THEN the file has the new code on line 3
          (let ((new-content (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string))))
            (should (string= "line 1\nline 2\nnew code\nline 4\nline 5\n" new-content))))
      (delete-directory temp-dir t))))

(ert-deftest test-shipit-apply-suggestion-multiline-replace ()
  "GIVEN a suggestion replacing multiple lines
WHEN applying the suggestion
THEN all target lines are replaced with the suggestion code."
  (let* ((temp-dir (make-temp-file "shipit-test" t))
         (temp-file (expand-file-name "src/main.py" temp-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" temp-dir) t)
          (with-temp-file temp-file
            (insert "import os\nimport sys\nold_line_1\nold_line_2\nold_line_3\nrest\n"))
          (with-temp-buffer
            (insert (propertize "suggestion"
                                'shipit-suggestion t
                                'shipit-suggestion-code "new_line_1\nnew_line_2"
                                'shipit-suggestion-path "src/main.py"
                                'shipit-suggestion-start-line 3
                                'shipit-suggestion-end-line 5))
            (goto-char (point-min))
            (let ((shipit--current-displayed-pr
                   `((head . ((ref . "fix"))))))
              (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "fix"))
                        ((symbol-function 'shipit--get-repo-root) (lambda () temp-dir))
                        ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
                (shipit-apply-suggestion-at-point))))
          ;; THEN lines 3-5 replaced with 2 new lines
          (let ((new-content (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string))))
            (should (string= "import os\nimport sys\nnew_line_1\nnew_line_2\nrest\n" new-content))))
      (delete-directory temp-dir t))))

;;; --- GraphQL start_line Extraction Test ---

(ert-deftest test-shipit-convert-graphql-thread-extracts-start-line ()
  "GIVEN a GraphQL thread node with startLine on a comment
WHEN converting to shipit comment format
THEN the comment has start_line field."
  (let* ((thread-node
          `((id . "thread-1")
            (isResolved . :json-false)
            (isOutdated . :json-false)
            (isCollapsed . :json-false)
            (path . "src/main.el")
            (line . 15)
            (originalLine . 15)
            (startLine . 10)
            (originalStartLine . 10)
            (comments . ((edges . (((node . ((id . "comment-1")
                                             (body . "suggestion")
                                             (createdAt . "2026-01-01T00:00:00Z")
                                             (updatedAt . "2026-01-01T00:00:00Z")
                                             (author . ((login . "user1")
                                                       (avatarUrl . "")))
                                             (originalPosition . 5)
                                             (position . 5)
                                             (diffHunk . "@@ -10,6 +10,6 @@")
                                             (path . "src/main.el")
                                             (line . 15)
                                             (originalLine . 15)
                                             (startLine . 10)
                                             (originalStartLine . 10)
                                             (side . "RIGHT")
                                             (reactions . ((totalCount . 0)
                                                          (edges))))))))))))
         (result (shipit--convert-graphql-thread-to-comment thread-node))
         (comments (cdr (assq 'comments result)))
         (first-comment (car comments)))
    ;; THEN start_line is extracted
    (should (= 10 (cdr (assq 'start_line first-comment))))
    ;; AND thread-level start_line is also available
    (should (= 10 (cdr (assq 'start_line result))))))

;;; --- json-null Handling Tests ---

(ert-deftest test-shipit-suggestion-old-lines-json-null-start-line ()
  "GIVEN a comment where start_line is :json-null (single-line suggestion)
WHEN calling shipit--get-suggestion-old-lines
THEN falls back to using line for both start and end, and extracts correctly."
  (let* ((comment `((body . "```suggestion\nnew\n```")
                    (diff_hunk . "@@ -10,3 +10,3 @@\n before\n target line\n after")
                    (line . 11)
                    (start_line . :json-null)
                    (path . "test.el")))
         (result (shipit--get-suggestion-old-lines comment)))
    ;; THEN old line is extracted (line 11 = "target line")
    (should result)
    (should (= 1 (length result)))
    (should (string= "target line" (car result)))))

(ert-deftest test-shipit-suggestion-old-lines-json-null-line ()
  "GIVEN a comment where line is :json-null but diff_hunk is present
WHEN calling shipit--get-suggestion-old-lines
THEN falls back to last line of hunk new-file side."
  (let* ((comment `((body . "```suggestion\nnew\n```")
                    (diff_hunk . "@@ -10,3 +10,3 @@\n before\n target\n after")
                    (line . :json-null)
                    (start_line . :json-null)
                    (path . "test.el")))
         (result (shipit--get-suggestion-old-lines comment)))
    ;; THEN last line from hunk is returned as fallback
    (should result)
    (should (= 1 (length result)))
    (should (string= "after" (car result)))))

(ert-deftest test-shipit-suggestion-old-lines-nil-line-nil-hunk ()
  "GIVEN a comment where both line and diff_hunk are nil
WHEN calling shipit--get-suggestion-old-lines
THEN returns nil gracefully without error."
  (let* ((comment `((body . "```suggestion\nnew\n```")
                    (line . nil)
                    (start_line . nil)
                    (path . "test.el")))
         (result (shipit--get-suggestion-old-lines comment)))
    (should-not result)))

(ert-deftest test-shipit-render-comment-body-json-null-start-line ()
  "GIVEN a full comment with :json-null start_line going through render pipeline
WHEN rendering through shipit--render-comment-body
THEN old lines appear correctly using line as fallback."
  (let* ((comment `((body . "Fix:\n```suggestion\nnew code\n```\n")
                    (diff_hunk . "@@ -10,3 +10,3 @@\n before\n old code\n after")
                    (line . 11)
                    (start_line . :json-null)
                    (path . "test.el")))
         (shipit-render-markdown nil)
         (result (shipit--render-comment-body comment 0)))
    ;; THEN both old and new lines appear
    (should (string-match-p "- old code" result))
    (should (string-match-p "\\+ new code" result))))

;;; --- Keybind Context Tests ---

(ert-deftest test-shipit-suggestion-or-approve-dispatches-to-apply ()
  "GIVEN point is on a suggestion
WHEN calling shipit-suggestion-or-approve
THEN shipit-apply-suggestion-at-point is called."
  (with-temp-buffer
    (insert (propertize "suggestion text"
                        'shipit-suggestion t
                        'shipit-suggestion-code "code"
                        'shipit-suggestion-path "file.el"
                        'shipit-suggestion-start-line 1
                        'shipit-suggestion-end-line 1))
    (goto-char (point-min))
    (let (called)
      (cl-letf (((symbol-function 'shipit-apply-suggestion-at-point) (lambda () (setq called t))))
        (shipit-suggestion-or-approve)
        (should called)))))

(ert-deftest test-shipit-suggestion-or-approve-dispatches-to-approve ()
  "GIVEN point is NOT on a suggestion
WHEN calling shipit-suggestion-or-approve
THEN shipit-approve is called."
  (with-temp-buffer
    (insert "regular text")
    (goto-char (point-min))
    (let (called)
      (cl-letf (((symbol-function 'shipit-approve) (lambda () (interactive) (setq called t))))
        (shipit-suggestion-or-approve)
        (should called)))))

(provide 'test-shipit-suggestions)
;;; test-shipit-suggestions.el ends here
