;;; test-shipit-releases-buffer.el --- Tests for releases buffer -*- lexical-binding: t; -*-
(require 'ert)
(require 'shipit-pr-github)

(ert-deftest test-classify-releases-url ()
  "GIVEN a GitHub releases URL
WHEN classifying the URL
THEN type is releases and repo is extracted."
  (let ((result (shipit-pr-github--classify-url
                 "https://github.com/anthropics/claude-code/releases")))
    (should (eq (plist-get result :type) 'releases))
    (should (equal (plist-get result :repo) "anthropics/claude-code"))
    (should-not (plist-get result :tag))))

(ert-deftest test-classify-releases-tag-url ()
  "GIVEN a GitHub releases/tag URL
WHEN classifying the URL
THEN type is releases with tag extracted."
  (let ((result (shipit-pr-github--classify-url
                 "https://github.com/anthropics/claude-code/releases/tag/v1.2.3")))
    (should (eq (plist-get result :type) 'releases))
    (should (equal (plist-get result :repo) "anthropics/claude-code"))
    (should (equal (plist-get result :tag) "v1.2.3"))))

(ert-deftest test-classify-tags-url ()
  "GIVEN a GitHub tags URL
WHEN classifying the URL
THEN type is tags and repo is extracted."
  (let ((result (shipit-pr-github--classify-url
                 "https://github.com/anthropics/claude-code/tags")))
    (should (eq (plist-get result :type) 'tags))
    (should (equal (plist-get result :repo) "anthropics/claude-code"))))

(ert-deftest test-dispatch-releases-url ()
  "GIVEN a classified releases URL
WHEN dispatching it
THEN shipit-open-releases-buffer is called with repo and tag."
  (require 'shipit-render)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-open-releases-buffer)
               (lambda (repo &optional tag scroll-to)
                 (setq called-with (list repo tag scroll-to)))))
      (shipit--open-classified-url
       '(:type releases :repo "owner/repo" :tag "v1.0"))
      (should (equal called-with '("owner/repo" "v1.0" nil))))))

(ert-deftest test-dispatch-tags-url ()
  "GIVEN a classified tags URL
WHEN dispatching it
THEN shipit-open-releases-buffer is called with tags scroll target."
  (require 'shipit-render)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-open-releases-buffer)
               (lambda (repo &optional tag scroll-to)
                 (setq called-with (list repo tag scroll-to)))))
      (shipit--open-classified-url
       '(:type tags :repo "owner/repo"))
      (should (equal called-with '("owner/repo" nil tags))))))

(ert-deftest test-github-backend-has-releases-keys ()
  "GIVEN the GitHub backend
WHEN checking for releases keys
THEN :fetch-releases and :fetch-tags are present."
  (require 'shipit-pr-github)
  (let* ((entry (assq 'github shipit-pr-backends))
         (plist (cdr entry)))
    (should (plist-get plist :fetch-releases))
    (should (plist-get plist :fetch-tags))))

(ert-deftest test-releases-buffer-creation ()
  "GIVEN a repo name
WHEN opening the releases buffer
THEN a buffer with the correct name and mode is created."
  (require 'shipit-releases-buffer)
  (cl-letf (((symbol-function 'shipit-releases-buffer-refresh) #'ignore))
    (let ((buf (shipit-open-releases-buffer "owner/repo")))
      (unwind-protect
          (with-current-buffer buf
            (should (eq major-mode 'shipit-releases-mode))
            (should (equal (buffer-name) "*shipit-releases: owner/repo*")))
        (kill-buffer buf)))))

(ert-deftest test-releases-section-rendering ()
  "GIVEN release data from the API
WHEN rendering the releases section
THEN each release appears as a magit section with tag, title, author."
  (require 'shipit-releases-buffer)
  (let ((releases `(((tag_name . "v1.0.0")
                     (name . "First Release")
                     (body . "Release notes here")
                     (prerelease . :json-false)
                     (draft . :json-false)
                     (published_at . "2026-04-01T12:00:00Z")
                     (author . ((login . "user1")
                                (avatar_url . "https://example.com/avatar")))
                     (assets . nil)))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (shipit-releases--insert-releases-section releases "owner/repo")))
      (goto-char (point-min))
      (should (search-forward "v1.0.0" nil t))
      (should (search-forward "First Release" nil t))
      (should (search-forward "user1" nil t)))))

(ert-deftest test-tags-section-rendering ()
  "GIVEN tag data from the API
WHEN rendering the tags section
THEN each tag appears with name and abbreviated SHA."
  (require 'shipit-releases-buffer)
  (let ((tags `(((name . "v1.0.0")
                 (commit . ((sha . "abc1234567890def")))))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (shipit-releases--insert-tags-section tags "owner/repo")))
      (goto-char (point-min))
      (should (search-forward "v1.0.0" nil t))
      (should (search-forward "abc1234" nil t)))))

(ert-deftest test-releases-format-size ()
  "GIVEN various byte sizes
WHEN formatting them
THEN human-readable strings are returned."
  (require 'shipit-releases-buffer)
  (should (equal (shipit-releases--format-size nil) "?"))
  (should (equal (shipit-releases--format-size 500) "500 B"))
  (should (equal (shipit-releases--format-size 1536) "1.5 KB"))
  (should (equal (shipit-releases--format-size (* 4.2 1024 1024)) "4.2 MB")))

(provide 'test-shipit-releases-buffer)
;;; test-shipit-releases-buffer.el ends here
