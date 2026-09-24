;;; test-shipit-test-offline.el --- The suite never reaches another machine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; shipit-test-offline.el refuses every connection a test opens to
;; another machine, and fails the test that tried.  These tests hold it
;; to that, and to letting through the connections that stay on this
;; machine, which the Jira sandbox needs.

;;; Code:

(require 'ert)
(require 'shipit-test-offline)

(ert-deftest test-offline-refuses-a-connection-to-github ()
  "GIVEN the suite's guard
WHEN a test opens a connection to api.github.com
THEN it is refused with an error that names the host, AND no
connection is made."
  (let ((before (process-list))
        (shipit-test-offline--attempts nil))
    (should (string-match-p
             "api\\.github\\.com:443"
             (error-message-string
              (should-error (open-network-stream "offline-test" nil "api.github.com" 443)
                            :type 'shipit-test-offline))))
    (should (equal before (process-list)))))

(ert-deftest test-offline-lets-a-local-connection-through ()
  "GIVEN the suite's guard, and a server on this machine
WHEN a test connects to it
THEN the connection is made."
  (let* ((server (make-network-process :name "offline-test-server" :server t
                                       :host "127.0.0.1" :service t :noquery t))
         (port (process-contact server :service)))
    (unwind-protect
        (let ((client (open-network-stream "offline-test-client" nil "127.0.0.1" port)))
          (unwind-protect
              (should (process-live-p client))
            (delete-process client)))
      (delete-process server))))

(ert-deftest test-offline-fails-a-test-that-swallows-the-refusal ()
  "GIVEN a test whose code catches the refusal and goes on
WHEN it is run
THEN it fails all the same, and the failure names the host."
  (let ((result (ert-run-test
                 (make-ert-test
                  :name 'test-offline--swallower
                  :body (lambda ()
                          (ignore-errors
                            (open-network-stream "offline-test" nil "api.github.com" 443)))))))
    (should (ert-test-failed-p result))
    (should (string-match-p "api\\.github\\.com:443"
                            (format "%S" (ert-test-result-with-condition-condition result))))))

(ert-deftest test-offline-git-reaches-no-remote ()
  "GIVEN the suite's guard
WHEN git is asked about a repository on GitHub
THEN it refuses without connecting, and without a login of the person
running the suite: git itself takes no transport but local files."
  (skip-unless (executable-find "git"))
  (with-temp-buffer
    (should-not (zerop (call-process "git" nil t nil "ls-remote"
                                     "https://github.com/octocat/Hello-World")))
    (should (string-match-p "not allowed" (buffer-string)))))

(ert-deftest test-offline-refuses-curl-to-github ()
  "GIVEN the suite's guard
WHEN curl is run for a URL on api.github.com, waited for or not
THEN it is refused before it runs, naming the URL."
  (let ((shipit-test-offline--attempts nil))
    (should (string-match-p
             "api\\.github\\.com"
             (error-message-string
              (should-error (call-process "curl" nil nil nil "-s" "https://api.github.com/user")
                            :type 'shipit-test-offline))))
    (should-error (make-process :name "offline-test-curl"
                                :command '("curl" "-s" "https://api.github.com/user"))
                  :type 'shipit-test-offline)))

(ert-deftest test-offline-lets-curl-to-this-machine-run ()
  "GIVEN the suite's guard
WHEN curl is run for a URL on this machine
THEN it runs."
  (skip-unless (executable-find "curl"))
  (should (numberp (call-process "curl" nil nil nil "-s" "-m" "1" "http://127.0.0.1:9/"))))

(provide 'test-shipit-test-offline)
;;; test-shipit-test-offline.el ends here
