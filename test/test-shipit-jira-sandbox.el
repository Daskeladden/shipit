;;; test-shipit-jira-sandbox.el --- shipit's Jira backend against a fake Jira -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The Jira sandbox runs test/fake-jira.py, a small stand-in for Jira
;; Cloud on this machine.  These tests drive shipit's Jira backend
;; through HTTP against it, as it drives a real site: the requests, the
;; login and the answers all go through url.el.  They skip themselves
;; where there is no python3.

;;; Code:

(require 'ert)
(require 'shipit-http)
(require 'shipit-issue-jira)
(require 'shipit-jira-sandbox)

(defmacro test-jira-sandbox--with (config &rest body)
  "Start the Jira sandbox, bind CONFIG to shipit's config of it, run BODY.
The sandbox's login is the only one there is: a lookup in the logins of
the person running the suite decrypts their auth file, and with no
passphrase cached that waits for a prompt nobody answers."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find "python3"))
     (let ((auth-sources nil))
       (unwind-protect
           (let ((,config (shipit-jira-sandbox-start)))
             ,@body)
         (shipit-jira-sandbox-stop)))))

(defun test-jira-sandbox--image-url (body)
  "The URL of the first image of BODY, markdown as shipit shows it."
  (and (string-match "!\\[[^]]*\\](\\([^)]+\\))" body)
       (match-string 1 body)))

(ert-deftest test-jira-sandbox-issue-shows-its-description-and-image ()
  "GIVEN the sandbox, whose first issue holds an image it has attached
WHEN shipit fetches that issue
THEN it has its summary, AND its description shows the image from the
sandbox's attachment, as it shows one from Jira."
  (test-jira-sandbox--with config
    (let* ((issue (shipit-issue-jira--fetch-issue config "PRJ-1"))
           (url (test-jira-sandbox--image-url (cdr (assq 'body issue)))))
      (should (equal "A bug to try shipit on" (cdr (assq 'title issue))))
      (should url)
      (should (string-prefix-p (concat (plist-get config :base-url)
                                       "/rest/api/3/attachment/content/")
                               url))
      (should (string-suffix-p "?redirect=false" url)))))

(ert-deftest test-jira-sandbox-comment-written-is-read-back ()
  "GIVEN the sandbox
WHEN shipit adds a comment to an issue, and reads the comments
THEN the comment comes back as it was written, from the sandbox's user."
  (test-jira-sandbox--with config
    (let ((added (shipit-issue-jira--add-comment config "PRJ-1" "Hello from shipit")))
      (should (equal "Hello from shipit" (cdr (assq 'body added))))
      (let ((last (car (last (shipit-issue-jira--fetch-comments config "PRJ-1")))))
        (should (equal "Hello from shipit" (cdr (assq 'body last))))
        (should (equal shipit-jira-sandbox-user
                       (cdr (assq 'login (cdr (assq 'user last))))))))))

(ert-deftest test-jira-sandbox-description-written-is-read-back-with-its-image ()
  "GIVEN the sandbox, and wiki markup that embeds the issue's attachment
WHEN shipit writes it as the description, and fetches the issue
THEN the text comes back, AND the embedded attachment comes back as an
image, as Jira turns wiki markup into its document format."
  (test-jira-sandbox--with config
    (shipit-issue-jira--update-description config "PRJ-1" "New text\n\n!fake.png!")
    (let ((body (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1")))))
      (should (string-match-p "New text" body))
      (should (test-jira-sandbox--image-url body)))))

(ert-deftest test-jira-sandbox-image-downloads-with-the-login ()
  "GIVEN the sandbox, which answers only a request that logs in
WHEN shipit downloads the image of an issue
THEN it gets the picture, a PNG."
  (test-jira-sandbox--with config
    (let* ((body (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1"))))
           (file (shipit--download-and-cache-image (test-jira-sandbox--image-url body))))
      (should file)
      (should (string-prefix-p "\x89PNG"
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))

(defun test-jira-sandbox--raw-status (config path)
  "The status the sandbox of CONFIG answers a GET of PATH with, sent bare.
Not through url.el: url.el adds by itself the login it finds in
auth-source, which holds the sandbox's."
  (let* ((url (url-generic-parse-url (plist-get config :base-url)))
         (answer "")
         (stream (open-network-stream "jira-sandbox-raw" nil (url-host url) (url-port url))))
    (unwind-protect
        (progn
          (set-process-filter stream (lambda (_process text) (setq answer (concat answer text))))
          (process-send-string stream (format "GET %s HTTP/1.0\r\nHost: %s\r\n\r\n"
                                              path (url-host url)))
          (while (and (process-live-p stream) (not (string-match-p "\r\n\r\n" answer)))
            (accept-process-output stream 0.1)))
      (delete-process stream))
    (and (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)" answer)
         (string-to-number (match-string 1 answer)))))

(ert-deftest test-jira-sandbox-refuses-a-request-without-a-login ()
  "GIVEN the sandbox
WHEN it is asked who is logged in, without a login
THEN it answers 401, as Jira does."
  (test-jira-sandbox--with config
    (should (equal 401 (test-jira-sandbox--raw-status config "/rest/api/3/myself")))))

(provide 'test-shipit-jira-sandbox)
;;; test-shipit-jira-sandbox.el ends here
