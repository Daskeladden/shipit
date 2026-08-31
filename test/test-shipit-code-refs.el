;;; test-shipit-code-refs.el --- Tests for issue references in code -*- lexical-binding: t; -*-

;;; Commentary:
;; The browse URL for an issue key must come from the repository the
;; buffer is about, not from `default-directory': a shipit buffer's
;; directory is wherever Emacs happened to be, and resolving through
;; it sent a Jira key to the GitHub issue tracker.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-code-refs)

(defconst test-code-refs--jira
  (list :name "Jira"
        :browse-url (lambda (config key)
                      (format "%s/browse/%s" (plist-get config :base-url) key)))
  "A stand-in issue backend that builds Jira-shaped browse URLs.")

(defconst test-code-refs--github
  (list :name "GitHub"
        :browse-url (lambda (config key)
                      (format "https://github.com/%s/issues/%s"
                              (plist-get config :repo) key)))
  "A stand-in for the default backend a repo falls back to.")

(defmacro test-code-refs--with-backends (&rest body)
  "Run BODY with a Jira entry for acme repos and a GitHub default."
  (declare (indent 0))
  `(let ((shipit-issue-backends (list (cons 'jira test-code-refs--jira)
                                      (cons 'github test-code-refs--github)))
         (shipit-issue-backend 'github)
         (shipit-issue-backend-config nil)
         (shipit-issue-repo-backends
          '(("acme/.*" :backend jira
             :base-url "https://acme.atlassian.net"
             :project-keys ("PRJ" "OTH")))))
     ,@body))

(ert-deftest test-code-refs-url-uses-the-buffer-repo-not-the-directory ()
  "GIVEN a buffer about a Jira-backed repository
AND a `default-directory' whose remote names another repository
WHEN the browse URL for a key is built
THEN the buffer's repository decides, and the key goes to Jira."
  (test-code-refs--with-backends
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "someone/elsewhere")))
      (with-temp-buffer
        (setq-local shipit-buffer-repo "acme/product")
        (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                       (shipit-code-refs--browse-url "PRJ-11844")))))))

(ert-deftest test-code-refs-row-repo-outranks-the-buffer-repo ()
  "GIVEN a row carrying its own repository as a text property
WHEN the browse URL is built at that row
THEN the row's repository decides, since one buffer can list many."
  (test-code-refs--with-backends
    (with-temp-buffer
      (setq-local shipit-buffer-repo "someone/elsewhere")
      (insert (propertize "x" 'shipit-repo "acme/product"))
      (goto-char (point-min))
      (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                     (shipit-code-refs--browse-url "PRJ-11844"))))))

(ert-deftest test-code-refs-key-finds-its-backend-when-the-repo-does-not-claim-it ()
  "GIVEN a buffer whose repository resolves to a backend that does not
claim the key's project
AND a configured backend whose project keys do claim it
WHEN the browse URL is built
THEN the key's own backend answers, rather than the key being sent to
an issue tracker that never heard of it."
  (test-code-refs--with-backends
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "someone/elsewhere")))
      (with-temp-buffer
        (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                       (shipit-code-refs--browse-url "PRJ-11844")))))))

(ert-deftest test-code-refs-repo-for-buffer-order ()
  "GIVEN every source of a buffer's repository at once
THEN the row's text property wins, then the buffer's own variable,
and the directory's remote only answers when nothing else does."
  (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "cwd/repo")))
    (with-temp-buffer
      (should (equal "cwd/repo" (shipit--repo-for-buffer)))
      (setq-local shipit-preview--repo "preview/repo")
      (should (equal "preview/repo" (shipit--repo-for-buffer)))
      (setq-local shipit-buffer-repo "buffer/repo")
      (should (equal "buffer/repo" (shipit--repo-for-buffer)))
      (insert (propertize "x" 'shipit-repo "row/repo"))
      (goto-char (point-min))
      (should (equal "row/repo" (shipit--repo-for-buffer))))))

(provide 'test-shipit-code-refs)
;;; test-shipit-code-refs.el ends here
