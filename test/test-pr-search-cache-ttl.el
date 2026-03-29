;;; test-pr-search-cache-ttl.el --- Tests for PR search cache TTL -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify that PR search cache expires and fresh results are fetched

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shipit modules
(require 'shipit-pr-sections)

(ert-deftest test-pr-search-cache-includes-timestamp ()
  "Test that PR search cache entries include a timestamp."
  (let ((shipit--completion-cache nil)
        (shipit--last-search-query "")
        (shipit--last-search-time 0)
        (api-called nil)
        (mock-prs '(((number . 100) (title . "Test PR") (state . "open")
                     (user . ((login . "testuser")))))))
    ;; Mock the search function
    (cl-letf (((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit)
                 (setq api-called t)
                 mock-prs))
              ((symbol-function 'shipit--should-trigger-search)
               (lambda (_string _candidates) t))
              ((symbol-function 'shipit--debug-log)
               (lambda (&rest _args) nil)))
      ;; Perform search
      (shipit--dynamic-pr-completion "test-repo" "" nil t)

      ;; Verify API was called
      (should api-called)

      ;; Verify cache has entry
      (should shipit--completion-cache)

      ;; Verify cache entry has timestamp (5th element)
      (let* ((cache-entry (car shipit--completion-cache))
             (cache-data (cdr cache-entry)))
        (should (>= (length cache-data) 5))
        (should (numberp (nth 4 cache-data)))  ; timestamp
        (should (> (nth 4 cache-data) 0))))))

(ert-deftest test-pr-search-cache-expired-triggers-fresh-fetch ()
  "Test that expired cache entries trigger fresh API fetch."
  (let ((shipit--completion-cache nil)
        (shipit--completion-cache-ttl-seconds 60)  ; 1 minute TTL
        (shipit--last-search-query "")
        (shipit--last-search-time 0)
        (api-call-count 0)
        (mock-prs '(((number . 100) (title . "Test PR") (state . "open")
                     (user . ((login . "testuser")))))))
    ;; Mock the search function
    (cl-letf (((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit)
                 (setq api-call-count (1+ api-call-count))
                 mock-prs))
              ((symbol-function 'shipit--should-trigger-search)
               (lambda (_string _candidates) t))
              ((symbol-function 'shipit--debug-log)
               (lambda (&rest _args) nil)))

      ;; First search - should call API
      (shipit--dynamic-pr-completion "test-repo" "" nil t)
      (should (= api-call-count 1))

      ;; Manually expire the cache by setting old timestamp
      (when shipit--completion-cache
        (let* ((cache-entry (car shipit--completion-cache))
               (cache-data (cdr cache-entry)))
          ;; Set timestamp to 2 minutes ago (older than TTL)
          (setf (nth 4 cache-data) (- (float-time) 120))))

      ;; Reset debounce
      (setq shipit--last-search-query "different-query"
            shipit--last-search-time 0)

      ;; Second search - should call API again because cache is expired
      (shipit--dynamic-pr-completion "test-repo" "" nil t)
      (should (= api-call-count 2)))))

(ert-deftest test-pr-search-cache-valid-uses-cached-data ()
  "Test that valid (non-expired) cache entries are used without API call."
  (let ((shipit--completion-cache nil)
        (shipit--completion-cache-ttl-seconds 300)  ; 5 minute TTL
        (shipit--last-search-query "")
        (shipit--last-search-time 0)
        (api-call-count 0)
        (mock-prs '(((number . 100) (title . "Test PR") (state . "open")
                     (user . ((login . "testuser")))))))
    ;; Mock the search function
    (cl-letf (((symbol-function 'shipit--search-prs-multi-page)
               (lambda (_repo _query _limit)
                 (setq api-call-count (1+ api-call-count))
                 mock-prs))
              ((symbol-function 'shipit--should-trigger-search)
               (lambda (_string _candidates) t))
              ((symbol-function 'shipit--debug-log)
               (lambda (&rest _args) nil)))

      ;; First search - should call API
      (shipit--dynamic-pr-completion "test-repo" "" nil t)
      (should (= api-call-count 1))

      ;; Second search immediately - should use cache (not expired)
      (let ((results (shipit--dynamic-pr-completion "test-repo" "" nil t)))
        ;; API should NOT be called again
        (should (= api-call-count 1))
        ;; But we should still get results
        (should results)
        (should (> (length results) 0))))))

(ert-deftest test-pr-search-default-ttl-is-reasonable ()
  "Test that default cache TTL is set to a reasonable value."
  ;; Should be between 1-10 minutes
  (should (boundp 'shipit--completion-cache-ttl-seconds))
  (should (>= shipit--completion-cache-ttl-seconds 60))
  (should (<= shipit--completion-cache-ttl-seconds 600)))

(provide 'test-pr-search-cache-ttl)
;;; test-pr-search-cache-ttl.el ends here
