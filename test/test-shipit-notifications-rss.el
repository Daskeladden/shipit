;;; test-shipit-notifications-rss.el --- Tests for RSS notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the RSS/Atom feed notification source.

;;; Code:
(require 'ert)
(require 'shipit-notifications-rss)

;;; XML parsing tests

(ert-deftest test-rss-parse-atom-feed ()
  "GIVEN a valid Atom feed XML
   WHEN parsed with shipit-rss--parse-entries
   THEN entries are extracted with correct fields."
  (let* ((xml '(feed nil
                 (title nil "My Blog")
                 (entry nil
                   (title nil "Post One")
                   (link ((href . "https://example.com/post1") (rel . "alternate")))
                   (id nil "urn:uuid:1234")
                   (updated nil "2026-03-10T12:00:00Z")
                   (summary nil "First post summary"))))
         (entries (shipit-rss--parse-entries xml "https://example.com/feed")))
    (should (= 1 (length entries)))
    (let ((e (car entries)))
      (should (string= "Post One" (cdr (assq 'title e))))
      (should (string= "https://example.com/post1" (cdr (assq 'link e))))
      (should (string= "urn:uuid:1234" (cdr (assq 'guid e))))
      (should (string= "2026-03-10T12:00:00Z" (cdr (assq 'published e)))))))

(ert-deftest test-rss-parse-rss2-feed ()
  "GIVEN a valid RSS 2.0 feed XML
   WHEN parsed with shipit-rss--parse-entries
   THEN items are extracted with correct fields."
  (let* ((xml '(rss nil
                 (channel nil
                   (title nil "News Feed")
                   (item nil
                     (title nil "Breaking News")
                     (link nil "https://news.com/article1")
                     (guid nil "https://news.com/article1")
                     (pubDate nil "Mon, 10 Mar 2026 12:00:00 GMT")
                     (description nil "News summary")))))
         (entries (shipit-rss--parse-entries xml "https://news.com/rss")))
    (should (= 1 (length entries)))
    (let ((e (car entries)))
      (should (string= "Breaking News" (cdr (assq 'title e))))
      (should (string= "https://news.com/article1" (cdr (assq 'link e))))
      (should (string= "https://news.com/article1" (cdr (assq 'guid e)))))))

(ert-deftest test-rss-parse-unknown-format ()
  "GIVEN an unknown XML format
   WHEN parsed with shipit-rss--parse-entries
   THEN nil is returned."
  (let ((xml '(html nil (body nil "not a feed"))))
    (should-not (shipit-rss--parse-entries xml "https://example.com"))))

;;; Notification conversion tests

(ert-deftest test-rss-entry-to-activity ()
  "GIVEN an RSS entry and feed config
   WHEN converted to activity
   THEN all required notification fields are present."
  (let* ((feed '(:url "https://blog.com/feed" :name "Dev Blog"))
         (entry '((title . "New Release")
                  (link . "https://blog.com/new-release")
                  (guid . "guid-123")
                  (published . "2026-03-10T12:00:00Z")
                  (summary . "Release notes")))
         (activity (shipit-rss--entry-to-activity feed entry)))
    (should (string= "Dev Blog" (cdr (assq 'repo activity))))
    (should (string= "New Release" (cdr (assq 'subject activity))))
    (should (string= "rss" (cdr (assq 'type activity))))
    (should (eq 'rss (cdr (assq 'source activity))))
    (should (string= "https://blog.com/new-release" (cdr (assq 'browse-url activity))))
    (should (cdr (assq 'updated-at activity)))))

(ert-deftest test-rss-entry-to-activity-uses-feed-title-fallback ()
  "GIVEN a feed config without :name but with cached title
   WHEN converted to activity
   THEN cached feed title is used as repo name."
  (let ((shipit-rss--feed-titles (make-hash-table :test 'equal)))
    (puthash "https://blog.com/feed" "Cached Title" shipit-rss--feed-titles)
    (let* ((feed '(:url "https://blog.com/feed"))
           (entry '((title . "Post") (link . "url") (guid . "g") (published . "") (summary . "")))
           (activity (shipit-rss--entry-to-activity feed entry)))
      (should (string= "Cached Title" (cdr (assq 'repo activity)))))))

;;; Deduplication tests

(ert-deftest test-rss-fetch-filters-by-since ()
  "GIVEN entries with different timestamps
   WHEN notifications are fetched with a since cutoff
   THEN only recent entries are returned."
  (let ((shipit-rss--seen-guids (make-hash-table :test 'equal))
        (shipit-rss-feeds '((:url "https://test.com/feed" :name "Test")))
        (recent-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
        (old-time "2020-01-01T00:00:00Z"))
    (cl-letf (((symbol-function 'shipit-rss--fetch-feed)
               (lambda (_url)
                 `(feed nil
                    (title nil "Test")
                    (entry nil
                      (title nil "Recent") (id nil "guid-new")
                      (updated nil ,recent-time)
                      (link ((href . "url1") (rel . "alternate"))))
                    (entry nil
                      (title nil "Old") (id nil "guid-old")
                      (updated nil ,old-time)
                      (link ((href . "url2") (rel . "alternate"))))))))
      ;; With since=nil, defaults to 1h ago — only recent entry passes
      (let ((results (shipit-rss--fetch-notifications nil)))
        (should (= 1 (length results)))
        (should (string= "Recent" (cdr (assq 'subject (car results)))))))))

;;; Backend registration test

(ert-deftest test-rss-backend-registered ()
  "GIVEN shipit-notifications-rss is loaded
   WHEN checking the issue backend registry
   THEN rss backend is registered with :notifications."
  (let ((backend (cdr (assq 'rss shipit-issue-backends))))
    (should backend)
    (should (string= "RSS" (plist-get backend :name)))
    (should (plist-get backend :notifications))))

(provide 'test-shipit-notifications-rss)
;;; test-shipit-notifications-rss.el ends here
