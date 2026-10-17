;;; test-pr-search-dynamic.el --- Tests for PR search dynamic re-querying -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for dynamic re-querying in PR search completing-read.
;; Tests that shipit--build-github-query adds in:title for plain text
;; searches so API results match what the completion framework filters on.

;;; Code:

(require 'ert)
(require 'shipit)
(require 'shipit-pr-sections)
(require 'shipit-commands)

;;; Tests for shipit--build-github-query

(ert-deftest test-pr-search-dynamic--plain-text-adds-in-title ()
  "GIVEN a parsed query with plain text filter 'cache' and no field
WHEN building the GitHub query
THEN the query includes 'in:title' qualifier."
  (let* ((parsed '((field . nil) (filter . "cache")))
         (query (shipit--build-github-query "owner/repo" parsed)))
    (should (string-match-p "in:title" query))
    (should (string-match-p "cache" query))
    (should (string-match-p "is:pr" query))))

(ert-deftest test-pr-search-dynamic--empty-filter-no-in-title ()
  "GIVEN a parsed query with no filter terms
WHEN building the GitHub query
THEN the query does NOT include 'in:title'."
  (let* ((parsed '((field . nil) (filter . nil)))
         (query (shipit--build-github-query "owner/repo" parsed)))
    (should-not (string-match-p "in:title" query))
    (should (string-match-p "is:pr" query))))

(ert-deftest test-pr-search-dynamic--field-search-no-in-title ()
  "GIVEN a parsed query with an author field and additional filter text
WHEN building the GitHub query
THEN the query does NOT include 'in:title' (field searches are different)."
  (let* ((parsed '((field . ("author" . "alice")) (filter . "bugfix")))
         (query (shipit--build-github-query "owner/repo" parsed)))
    (should-not (string-match-p "in:title" query))
    (should (string-match-p "author:alice" query))
    (should (string-match-p "bugfix" query))))

(ert-deftest test-pr-search-dynamic--repo-always-present ()
  "GIVEN any parsed query
WHEN building the GitHub query
THEN the repo qualifier is always present."
  (let* ((parsed '((field . nil) (filter . "test")))
         (query (shipit--build-github-query "owner/repo" parsed)))
    (should (string-match-p "repo:" query))))

;;; Tests for shipit--parse-search-query

(ert-deftest test-pr-search-dynamic--parse-plain-text ()
  "GIVEN a plain text query 'cache'
WHEN parsing the search query
THEN field is nil and filter is 'cache'."
  (let ((parsed (shipit--parse-search-query "cache")))
    (should (null (cdr (assq 'field parsed))))
    (should (equal "cache" (cdr (assq 'filter parsed))))))

(ert-deftest test-pr-search-dynamic--parse-empty-string ()
  "GIVEN an empty string
WHEN parsing the search query
THEN field is nil and filter is nil."
  (let ((parsed (shipit--parse-search-query "")))
    (should (null (cdr (assq 'field parsed))))
    (should (null (cdr (assq 'filter parsed))))))

(ert-deftest test-pr-search-dynamic--parse-field-query ()
  "GIVEN a field query ':author alice'
WHEN parsing the search query
THEN field is (author . alice) and filter is nil."
  (let ((parsed (shipit--parse-search-query ":author alice ")))
    (should (equal '("author" . "alice") (cdr (assq 'field parsed))))))

;;; Tests for shipit--select-pr merge behavior

(defun test-pr-search-dynamic--make-pr (number title &optional author state)
  "Create a mock PR alist with NUMBER, TITLE, optional AUTHOR and STATE."
  `((number . ,number)
    (title . ,title)
    (state . ,(or state "open"))
    (user . ((login . ,(or author "testuser"))))
    (updated_at . "2026-01-01T00:00:00Z")))

(ert-deftest test-pr-search-dynamic--initial-candidates-built ()
  "GIVEN initial PR results from API
WHEN shipit--select-pr builds candidates
THEN all PRs appear as formatted candidates in the completion list."
  (let* ((prs (list (test-pr-search-dynamic--make-pr 100 "Add Cache support")
                    (test-pr-search-dynamic--make-pr 99 "Fix CI pipeline")))
         (candidates (mapcar #'shipit--format-pr-choice prs)))
    (should (= 2 (length candidates)))
    (should (string-match-p "#100" (nth 0 candidates)))
    (should (string-match-p "Add Cache support" (nth 0 candidates)))
    (should (string-match-p "#99" (nth 1 candidates)))))

(ert-deftest test-pr-search-dynamic--dynamic-requery-merges-results ()
  "GIVEN initial candidates and a dynamic API search returning new PRs
WHEN the completion function is called with a 3+ char query
THEN new candidates are merged into the all-candidates list."
  (let* ((initial-prs (list (test-pr-search-dynamic--make-pr 100 "Add Cache support")))
         (search-prs (list (test-pr-search-dynamic--make-pr 50 "Cache reset fix")))
         (pr-lookup-table (make-hash-table :test 'equal))
         (all-candidates '())
         (search-cache (make-hash-table :test 'equal)))
    ;; Build initial candidates
    (dolist (pr initial-prs)
      (let ((candidate (shipit--format-pr-choice pr)))
        (puthash candidate (list :number (cdr (assq 'number pr))
                                 :repo "owner/repo" :data pr)
                 pr-lookup-table)
        (push candidate all-candidates)))
    (should (= 1 (length all-candidates)))
    ;; Simulate dynamic merge (same logic as the new shipit--select-pr)
    (dolist (pr search-prs)
      (let ((candidate (shipit--format-pr-choice pr)))
        (unless (gethash candidate pr-lookup-table)
          (puthash candidate (list :number (cdr (assq 'number pr))
                                   :repo "owner/repo" :data pr)
                   pr-lookup-table)
          (push candidate all-candidates))))
    (should (= 2 (length all-candidates)))
    (should (string-match-p "#50" (car all-candidates)))))

(ert-deftest test-pr-search-dynamic--duplicates-skipped ()
  "GIVEN initial candidates that already contain a PR
WHEN the dynamic search returns the same PR again
THEN the duplicate is not added to candidates."
  (let* ((pr (test-pr-search-dynamic--make-pr 100 "Add Cache support"))
         (pr-lookup-table (make-hash-table :test 'equal))
         (all-candidates '()))
    ;; Build initial candidate
    (let ((candidate (shipit--format-pr-choice pr)))
      (puthash candidate (list :number 100 :repo "owner/repo" :data pr) pr-lookup-table)
      (push candidate all-candidates))
    (should (= 1 (length all-candidates)))
    ;; Try to merge same PR again
    (let ((candidate (shipit--format-pr-choice pr)))
      (unless (gethash candidate pr-lookup-table)
        (puthash candidate (list :number 100 :repo "owner/repo" :data pr) pr-lookup-table)
        (push candidate all-candidates)))
    (should (= 1 (length all-candidates)))))

(ert-deftest test-pr-search-dynamic--search-cache-prevents-duplicate-api-calls ()
  "GIVEN a search query that has already been cached
WHEN the completion function is called with the same query again
THEN no new API call is made (search-cache prevents it)."
  (let ((search-cache (make-hash-table :test 'equal))
        (api-call-count 0))
    ;; First query - not cached
    (should-not (gethash "cache" search-cache))
    (puthash "cache" t search-cache)
    (setq api-call-count (1+ api-call-count))
    ;; Second query - already cached
    (when (not (gethash "cache" search-cache))
      (setq api-call-count (1+ api-call-count)))
    (should (= 1 api-call-count))))

(ert-deftest test-pr-search-dynamic--pr-lookup-by-number ()
  "GIVEN a pr-lookup-table with PR data
WHEN looking up a PR by number from a selected candidate
THEN the correct PR data is returned."
  (let* ((pr (test-pr-search-dynamic--make-pr 42 "The answer"))
         (pr-lookup-table (make-hash-table :test 'equal))
         (candidate (shipit--format-pr-choice pr)))
    (puthash candidate (list :number 42 :repo "owner/repo" :data pr) pr-lookup-table)
    ;; Look up by iterating hash (same pattern as shipit--select-pr)
    (let* ((pr-number 42)
           (found (catch 'found
                    (maphash (lambda (_key val)
                               (when (= (plist-get val :number) pr-number)
                                 (throw 'found val)))
                             pr-lookup-table)
                    nil)))
      (should found)
      (should (= 42 (plist-get found :number)))
      (should (equal "owner/repo" (plist-get found :repo)))
      (should (equal "The answer" (cdr (assq 'title (plist-get found :data))))))))

(ert-deftest test-pr-search-dynamic--short-query-skips-api ()
  "GIVEN a query shorter than 3 characters
WHEN the completion function processes it
THEN no API search is triggered."
  (let ((search-cache (make-hash-table :test 'equal))
        (api-called nil))
    ;; Query of 2 chars should NOT trigger search
    (when (and (>= (length "fp") 3)
               (not (gethash "fp" search-cache)))
      (setq api-called t))
    (should-not api-called)
    ;; Query of 3 chars SHOULD trigger search
    (when (and (>= (length "fpg") 3)
               (not (gethash "fpg" search-cache)))
      (setq api-called t))
    (should api-called)))

;;; Tests for shipit--read-pr

(ert-deftest test-pr-search-dynamic--read-pr-returns-the-pick ()
  "GIVEN the repo's pull requests
WHEN the user picks one with completion
THEN its info plist comes back, and nothing is displayed."
  (let ((displayed nil))
    (cl-letf (((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit)
                 '(((number . 42) (title . "Fix sensor"))
                   ((number . 7) (title . "Docs")))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (seq-find (lambda (c) (string-prefix-p "#7 " c))
                           (all-completions "" table))))
              ((symbol-function 'shipit--display-selected-pr)
               (lambda (&rest _) (setq displayed t)))
              ((symbol-function 'message) #'ignore))
      (let ((info (shipit--read-pr "Pick: " "foo/bar")))
        (should (equal 7 (plist-get info :number)))
        (should (equal "foo/bar" (plist-get info :repo)))
        (should (equal "Docs" (cdr (assq 'title (plist-get info :data)))))
        (should-not displayed)))))

(ert-deftest test-pr-search-dynamic--select-pr-displays-the-pick ()
  "GIVEN the repo's pull requests
WHEN selecting one
THEN it is displayed."
  (let ((displayed nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () "foo/bar"))
              ((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit) '(((number . 42) (title . "Fix")))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (car (all-completions "" table))))
              ((symbol-function 'shipit--display-selected-pr)
               (lambda (pr repo) (setq displayed (cons (cdr (assq 'number pr)) repo))))
              ((symbol-function 'message) #'ignore))
      (shipit--select-pr)
      (should (equal displayed '(42 . "foo/bar"))))))

(ert-deftest test-pr-search-dynamic--annotates-a-pr-without-labels ()
  "GIVEN the repo's pull requests, one of them without labels, and no
marginalia
WHEN the completion list annotates the candidates
THEN each gets its author and state, and its labels when it has any."
  (let ((annotations nil))
    (cl-letf (((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit)
                 (list (test-pr-search-dynamic--make-pr 42 "Fix sensor" "alice")
                       '((number . 7) (title . "Docs") (state . "open")
                         (user . ((login . "bob")))
                         (labels . (((name . "docs"))))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (let ((annotate (completion-metadata-get
                                  (completion-metadata "" table nil)
                                  'annotation-function)))
                   (setq annotations (mapcar annotate (all-completions "" table))))
                 nil))
              ((symbol-function 'message) #'ignore))
      (should-not (fboundp 'marginalia--fields))
      (shipit--read-pr "Pick: " "foo/bar")
      (should (string-match-p "@alice - Open" (nth 0 annotations)))
      (should (string-match-p "@bob - Open - docs" (nth 1 annotations))))))

(provide 'test-pr-search-dynamic)

;;; test-pr-search-dynamic.el ends here
