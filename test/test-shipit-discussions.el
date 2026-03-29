;;; test-shipit-discussions.el --- Tests for discussions search -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitHub Discussions search and transient menu.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-discussions-graphql)
(require 'shipit-discussions)

;;; Search Query Building Tests

(ert-deftest test-shipit-discussions-build-query-basic ()
  "GIVEN a repo with no extra args
WHEN building the search query
THEN it includes repo:owner/name."
  (let ((query (shipit-discussion--build-search-query nil "owner/repo")))
    (should (string-match-p "repo:owner/repo" query))))

(ert-deftest test-shipit-discussions-build-query-category ()
  "GIVEN args with --category=Q&A
WHEN building the search query
THEN it includes category:\"Q&A\"."
  (let ((query (shipit-discussion--build-search-query
                '("--category=Q&A") "owner/repo")))
    (should (string-match-p "category:\"Q&A\"" query))))

(ert-deftest test-shipit-discussions-build-query-answered-yes ()
  "GIVEN args with --answered=yes
WHEN building the search query
THEN it includes is:answered."
  (let ((query (shipit-discussion--build-search-query
                '("--answered=yes") "owner/repo")))
    (should (string-match-p "is:answered" query))))

(ert-deftest test-shipit-discussions-build-query-answered-no ()
  "GIVEN args with --answered=no
WHEN building the search query
THEN it includes is:unanswered."
  (let ((query (shipit-discussion--build-search-query
                '("--answered=no") "owner/repo")))
    (should (string-match-p "is:unanswered" query))))

(ert-deftest test-shipit-discussions-build-query-author ()
  "GIVEN args with --author=alice
WHEN building the search query
THEN it includes author:alice."
  (let ((query (shipit-discussion--build-search-query
                '("--author=alice") "owner/repo")))
    (should (string-match-p "author:alice" query))))

(ert-deftest test-shipit-discussions-build-query-label ()
  "GIVEN args with --label=bug
WHEN building the search query
THEN it includes label:\"bug\"."
  (let ((query (shipit-discussion--build-search-query
                '("--label=bug") "owner/repo")))
    (should (string-match-p "label:\"bug\"" query))))

(ert-deftest test-shipit-discussions-build-query-title ()
  "GIVEN args with --title=help
WHEN building the search query
THEN it includes 'help in:title'."
  (let ((query (shipit-discussion--build-search-query
                '("--title=help") "owner/repo")))
    (should (string-match-p "help in:title" query))))

(ert-deftest test-shipit-discussions-build-query-date-range ()
  "GIVEN args with --created-after=2025-01-01
WHEN building the search query
THEN it includes created:>2025-01-01."
  (let ((query (shipit-discussion--build-search-query
                '("--created-after=2025-01-01") "owner/repo")))
    (should (string-match-p "created:>2025-01-01" query))))

;;; Candidate Building Tests

(ert-deftest test-shipit-discussions-build-candidates ()
  "GIVEN a list of discussion results
WHEN building candidates for completing-read
THEN returns formatted strings with number and title."
  (let ((results '(((number . 42)
                    (title . "How to use feature X?")
                    (user . ((login . "alice")))
                    (category . ((name . "Q&A") (emoji . ":pray:")))
                    (is_answered . t)
                    (upvote_count . 5)
                    (comments_count . 3)
                    (created_at . "2025-01-15T10:00:00Z"))))
        (lookup (make-hash-table :test 'equal)))
    (let ((candidates (shipit-discussions--build-candidates
                       results "owner/repo" lookup)))
      (should (= 1 (length candidates)))
      (should (string-match-p "#42" (car candidates)))
      (should (string-match-p "How to use feature X?" (car candidates))))))

;;; Annotation Tests

(ert-deftest test-shipit-discussions-annotate ()
  "GIVEN a discussion candidate in the completion table
WHEN annotating it
THEN returns string with category, author, and answered state."
  (let ((shipit-discussions--completion-table
         (make-hash-table :test 'equal)))
    (puthash "#42    How to use feature X?"
             (list :number 42 :repo "owner/repo"
                   :data '((user . ((login . "alice")))
                           (category . ((name . "Q&A") (emoji . ":pray:")))
                           (is_answered . t)
                           (upvote_count . 5)
                           (comments_count . 3)
                           (updated_at . "2025-01-15T10:00:00Z")))
             shipit-discussions--completion-table)
    (let ((annotation (shipit-discussions--annotate
                       "#42    How to use feature X?")))
      (should annotation)
      (should (string-match-p "@alice" annotation))
      (should (string-match-p "Q&A" annotation)))))

;;; Extract Limit Tests

(ert-deftest test-shipit-discussions-extract-limit-default ()
  "GIVEN args without --limit
WHEN extracting limit
THEN returns 50."
  (should (= 50 (shipit-discussion--extract-limit-from-args
                  '("--category=General")))))

(ert-deftest test-shipit-discussions-extract-limit-custom ()
  "GIVEN args with --limit=25
WHEN extracting limit
THEN returns 25."
  (should (= 25 (shipit-discussion--extract-limit-from-args
                  '("--limit=25" "--category=General")))))

(provide 'test-shipit-discussions)
;;; test-shipit-discussions.el ends here
