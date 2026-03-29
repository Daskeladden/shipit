;;; test-per-commit-comments.el --- Tests for per-commit inline comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for filtering and displaying inline comments on per-commit file diffs.

;;; Code:

(require 'ert)

;;; Test Data

(defvar test-comments-multi-commit
  '(;; Comment on commit abc123
    ((id . 1001)
     (body . "Comment on first commit")
     (path . "src/file.el")
     (line . 10)
     (commit_id . "abc123def")
     (original_commit_id . "abc123def")
     (in_reply_to_id . nil)
     (outdated . nil))
    ;; Reply to comment 1001 (made on later commit)
    ((id . 1002)
     (body . "Reply to first comment")
     (path . "src/file.el")
     (line . 10)
     (commit_id . "def456ghi")
     (original_commit_id . "abc123def")
     (in_reply_to_id . 1001)
     (outdated . nil))
    ;; Comment on commit def456
    ((id . 2001)
     (body . "Comment on second commit")
     (path . "src/file.el")
     (line . 20)
     (commit_id . "def456ghi")
     (original_commit_id . "def456ghi")
     (in_reply_to_id . nil)
     (outdated . nil))
    ;; Comment on different file, same commit
    ((id . 3001)
     (body . "Comment on other file")
     (path . "src/other.el")
     (line . 5)
     (commit_id . "abc123def")
     (original_commit_id . "abc123def")
     (in_reply_to_id . nil)
     (outdated . nil)))
  "Test comments spanning multiple commits.")

;;; Tests for shipit--filter-comments-by-commit

(ert-deftest test-filter-comments-by-commit/returns-comments-matching-commit-id ()
  "GIVEN comments with different commit_ids
WHEN filtering by a specific commit SHA
THEN root comments with matching commit_id and their replies are returned."
  (let* ((comments test-comments-multi-commit)
         (filtered (shipit--filter-comments-by-commit comments "abc123def")))
    ;; Should return:
    ;; - 1001: root on abc123def
    ;; - 1002: reply to 1001 (included because thread stays together)
    ;; - 3001: root on abc123def
    (should (= (length filtered) 3))
    (should (cl-find 1001 filtered :key (lambda (c) (cdr (assq 'id c)))))
    (should (cl-find 1002 filtered :key (lambda (c) (cdr (assq 'id c)))))
    (should (cl-find 3001 filtered :key (lambda (c) (cdr (assq 'id c)))))
    ;; Should NOT include comment 2001 (root on def456ghi, different commit)
    (should-not (cl-find 2001 filtered :key (lambda (c) (cdr (assq 'id c)))))))

(ert-deftest test-filter-comments-by-commit/includes-replies-to-matching-root ()
  "GIVEN a root comment on commit A and reply made on commit B
WHEN filtering by commit A
THEN both root and reply are included (thread stays together)."
  (let* ((comments test-comments-multi-commit)
         (filtered (shipit--filter-comments-by-commit comments "abc123def")))
    ;; Root comment 1001 is on abc123def
    (should (cl-find 1001 filtered :key (lambda (c) (cdr (assq 'id c)))))
    ;; Reply 1002 has commit_id def456ghi but replies to 1001, so should be included
    (should (cl-find 1002 filtered :key (lambda (c) (cdr (assq 'id c)))))))

(ert-deftest test-filter-comments-by-commit/uses-original-commit-id-fallback ()
  "GIVEN a comment with only original_commit_id set
WHEN filtering by that commit
THEN the comment is included."
  (let* ((comments '(((id . 5001)
                      (body . "Outdated comment")
                      (path . "src/file.el")
                      (line . nil)
                      (commit_id . nil)
                      (original_commit_id . "xyz789abc")
                      (in_reply_to_id . nil)
                      (outdated . t))))
         (filtered (shipit--filter-comments-by-commit comments "xyz789abc")))
    (should (= (length filtered) 1))
    (should (= (cdr (assq 'id (car filtered))) 5001))))

(ert-deftest test-filter-comments-by-commit/handles-sha-prefix-matching ()
  "GIVEN a full commit SHA
WHEN filtering with a prefix of the SHA
THEN comments are matched (GitHub uses abbreviated SHAs)."
  (let* ((comments '(((id . 6001)
                      (body . "Test comment")
                      (path . "src/file.el")
                      (commit_id . "abc123def456789")
                      (original_commit_id . "abc123def456789"))))
         ;; Filter with abbreviated SHA
         (filtered (shipit--filter-comments-by-commit comments "abc123def")))
    (should (= (length filtered) 1))))

(ert-deftest test-filter-comments-by-commit/returns-empty-for-no-matches ()
  "GIVEN comments for commits A and B
WHEN filtering by commit C
THEN empty list is returned."
  (let* ((comments test-comments-multi-commit)
         (filtered (shipit--filter-comments-by-commit comments "nonexistent123")))
    (should (null filtered))))

(ert-deftest test-filter-comments-by-commit/handles-nil-commit-sha ()
  "GIVEN a nil commit-sha argument
WHEN filtering
THEN empty list is returned (graceful handling)."
  (let* ((comments test-comments-multi-commit)
         (filtered (shipit--filter-comments-by-commit comments nil)))
    (should (null filtered))))

(ert-deftest test-filter-comments-by-commit/handles-empty-comments-list ()
  "GIVEN an empty comments list
WHEN filtering by any commit
THEN empty list is returned."
  (let ((filtered (shipit--filter-comments-by-commit nil "abc123")))
    (should (null filtered))))

(ert-deftest test-filter-comments-by-commit/nested-reply-chains ()
  "GIVEN a deeply nested reply chain starting on commit A
WHEN filtering by commit A
THEN entire chain is included."
  (let* ((comments '(;; Root on commit A
                     ((id . 7001)
                      (body . "Root comment")
                      (commit_id . "commit-a")
                      (in_reply_to_id . nil))
                     ;; Reply to root (made on commit B)
                     ((id . 7002)
                      (body . "First reply")
                      (commit_id . "commit-b")
                      (in_reply_to_id . 7001))
                     ;; Reply to reply (made on commit C)
                     ((id . 7003)
                      (body . "Second reply")
                      (commit_id . "commit-c")
                      (in_reply_to_id . 7002))))
         (filtered (shipit--filter-comments-by-commit comments "commit-a")))
    ;; All three should be included since they're part of the same thread
    (should (= (length filtered) 3))))

;;; Provide the test file
(provide 'test-per-commit-comments)
;;; test-per-commit-comments.el ends here
