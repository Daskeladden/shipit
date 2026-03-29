;;; test-shipit-gitlab-http.el --- Tests for GitLab HTTP plumbing -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for GitLab HTTP plumbing functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)

;;; Project path tests

(ert-deftest test-shipit-gitlab-project-path-from-project-path ()
  "GIVEN config with :project-path \"mygroup/myproject\"
WHEN calling shipit-gitlab--project-path
THEN returns URL-encoded \"mygroup%2Fmyproject\"."
  (require 'shipit-gitlab-http)
  (let ((config '(:project-path "mygroup/myproject")))
    (should (string= "mygroup%2Fmyproject"
                      (shipit-gitlab--project-path config)))))

(ert-deftest test-shipit-gitlab-project-path-nested-groups ()
  "GIVEN config with :project-path \"group/sub/project\"
WHEN calling shipit-gitlab--project-path
THEN returns URL-encoded \"group%2Fsub%2Fproject\"."
  (require 'shipit-gitlab-http)
  (let ((config '(:project-path "group/sub/project")))
    (should (string= "group%2Fsub%2Fproject"
                      (shipit-gitlab--project-path config)))))

(ert-deftest test-shipit-gitlab-project-path-from-project-id ()
  "GIVEN config with :project-id 42
WHEN calling shipit-gitlab--project-path
THEN returns \"42\" as string."
  (require 'shipit-gitlab-http)
  (let ((config '(:project-id 42)))
    (should (string= "42"
                      (shipit-gitlab--project-path config)))))

(ert-deftest test-shipit-gitlab-project-path-missing-both ()
  "GIVEN config with neither :project-path nor :project-id
WHEN calling shipit-gitlab--project-path
THEN an error is signaled."
  (require 'shipit-gitlab-http)
  (should-error (shipit-gitlab--project-path '(:api-url "https://gitlab.com"))))

;;; URL construction tests

(ert-deftest test-shipit-gitlab-build-url-default ()
  "GIVEN config without :api-url
WHEN calling shipit-gitlab--build-url
THEN URL uses https://gitlab.com as default."
  (require 'shipit-gitlab-http)
  (let ((config '(:project-path "mygroup/myproject")))
    (should (string= "https://gitlab.com/api/v4/projects/mygroup%2Fmyproject/merge_requests"
                      (shipit-gitlab--build-url config "/projects/mygroup%2Fmyproject/merge_requests")))))

(ert-deftest test-shipit-gitlab-build-url-self-hosted ()
  "GIVEN config with :api-url \"https://gitlab.example.com\"
WHEN calling shipit-gitlab--build-url
THEN URL uses the custom api-url."
  (require 'shipit-gitlab-http)
  (let ((config '(:api-url "https://gitlab.example.com"
                  :project-path "mygroup/myproject")))
    (should (string= "https://gitlab.example.com/api/v4/projects/mygroup%2Fmyproject/merge_requests"
                      (shipit-gitlab--build-url config "/projects/mygroup%2Fmyproject/merge_requests")))))

(ert-deftest test-shipit-gitlab-build-url-strips-trailing-slash ()
  "GIVEN config with :api-url ending in slash
WHEN calling shipit-gitlab--build-url
THEN trailing slash is removed before concatenation."
  (require 'shipit-gitlab-http)
  (let ((config '(:api-url "https://gitlab.example.com/")))
    (should (string= "https://gitlab.example.com/api/v4/test"
                      (shipit-gitlab--build-url config "/test")))))

;;; Auth header tests

(ert-deftest test-shipit-gitlab-auth-header-from-config-token ()
  "GIVEN config with :token \"glpat-xxxx\"
WHEN calling shipit-gitlab--auth-header
THEN returns (\"PRIVATE-TOKEN\" . \"glpat-xxxx\")."
  (require 'shipit-gitlab-http)
  (let ((config '(:token "glpat-xxxx")))
    (should (equal '("PRIVATE-TOKEN" . "glpat-xxxx")
                    (shipit-gitlab--auth-header config)))))

(ert-deftest test-shipit-gitlab-auth-header-from-auth-source ()
  "GIVEN config without :token but with :api-url
WHEN calling shipit-gitlab--auth-header with auth-source returning credentials
THEN returns (\"PRIVATE-TOKEN\" . token-from-auth-source)."
  (require 'shipit-gitlab-http)
  (let ((config '(:api-url "https://gitlab.example.com")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 (list (list :user "__token__"
                             :secret "glpat-from-authsource")))))
      (should (equal '("PRIVATE-TOKEN" . "glpat-from-authsource")
                      (shipit-gitlab--auth-header config))))))

(ert-deftest test-shipit-gitlab-auth-header-no-credentials ()
  "GIVEN config without :token and no auth-source match
WHEN calling shipit-gitlab--auth-header
THEN returns nil."
  (require 'shipit-gitlab-http)
  (let ((config '(:api-url "https://gitlab.example.com")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should-not (shipit-gitlab--auth-header config)))))

;;; curl argument tests

(ert-deftest test-shipit-gitlab-curl-args-get-with-auth ()
  "GIVEN a URL, auth header, GET method, and no data
WHEN calling shipit-gitlab--curl-args
THEN returns correct curl argument list with auth and method."
  (require 'shipit-gitlab-http)
  (let ((args (shipit-gitlab--curl-args
               "https://gitlab.com/api/v4/projects/foo/merge_requests"
               '("PRIVATE-TOKEN" . "glpat-xxxx")
               "GET"
               nil)))
    ;; THEN args include method, auth, content-type, and URL
    (should (member "-X" args))
    (should (member "GET" args))
    (should (member "PRIVATE-TOKEN: glpat-xxxx" args))
    (should (member "Content-Type: application/json" args))
    (should (member "https://gitlab.com/api/v4/projects/foo/merge_requests" args))
    ;; THEN no -d flag for GET
    (should-not (member "-d" args))))

(ert-deftest test-shipit-gitlab-curl-args-post-with-data ()
  "GIVEN a URL, auth, POST method, and data
WHEN calling shipit-gitlab--curl-args
THEN returns curl argument list with -d for encoded data."
  (require 'shipit-gitlab-http)
  (let ((args (shipit-gitlab--curl-args
               "https://gitlab.com/api/v4/test"
               '("PRIVATE-TOKEN" . "tok")
               "POST"
               '((body . "hello")))))
    ;; THEN args include -d
    (should (member "-d" args))
    (should (member "POST" args))))

(ert-deftest test-shipit-gitlab-curl-args-no-auth ()
  "GIVEN no auth header
WHEN calling shipit-gitlab--curl-args
THEN returns args without PRIVATE-TOKEN header."
  (require 'shipit-gitlab-http)
  (let ((args (shipit-gitlab--curl-args
               "https://gitlab.com/api/v4/test" nil "GET" nil)))
    ;; THEN no PRIVATE-TOKEN header
    (should-not (cl-find-if (lambda (a) (and (stringp a) (string-prefix-p "PRIVATE-TOKEN" a)))
                            args))))

;;; Paginated request tests

(ert-deftest test-shipit-gitlab-api-request-paginated-single-page ()
  "GIVEN a GitLab API endpoint that returns a vector of items
WHEN calling shipit-gitlab--api-request-paginated
THEN all items are returned as a list."
  (require 'shipit-gitlab-http)
  (let ((call-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--curl-request-with-headers)
               (lambda (_config _path _method &optional _data)
                 (setq call-count (1+ call-count))
                 (cons '(("x-next-page" . ""))
                       (vector '((id . 1)) '((id . 2)))))))
      (let* ((config '(:project-path "test/project"))
             (result (shipit-gitlab--api-request-paginated config "/test?per_page=100")))
        (should (= 1 call-count))
        (should (= 2 (length result)))
        (should (= 1 (cdr (assq 'id (car result)))))))))

(ert-deftest test-shipit-gitlab-api-request-paginated-multi-page ()
  "GIVEN a GitLab API endpoint with 2 pages of results
WHEN calling shipit-gitlab--api-request-paginated
THEN items from both pages are combined."
  (require 'shipit-gitlab-http)
  (let ((call-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--curl-request-with-headers)
               (lambda (_config path _method &optional _data)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1)
                     ;; First page: has x-next-page
                     (cons '(("x-next-page" . "2"))
                           (vector '((id . 1)) '((id . 2))))
                   ;; Second page: no next page
                   (cons '(("x-next-page" . ""))
                         (vector '((id . 3))))))))
      (let* ((config '(:project-path "test/project"))
             (result (shipit-gitlab--api-request-paginated config "/test?per_page=100")))
        (should (= 2 call-count))
        (should (= 3 (length result)))
        (should (= 3 (cdr (assq 'id (nth 2 result)))))))))

(ert-deftest test-shipit-gitlab-api-request-paginated-nil-response ()
  "GIVEN a GitLab API endpoint that returns nil
WHEN calling shipit-gitlab--api-request-paginated
THEN nil is returned."
  (require 'shipit-gitlab-http)
  (cl-letf (((symbol-function 'shipit-gitlab--curl-request-with-headers)
             (lambda (_config _path _method &optional _data)
               (cons nil nil))))
    (let ((config '(:project-path "test/project")))
      (should-not (shipit-gitlab--api-request-paginated config "/test")))))

;;; Async HTTP primitive tests

(ert-deftest test/gitlab-api-request-async-success ()
  "GIVEN a GitLab config and a mocked url-retrieve returning 200 + JSON
WHEN calling shipit-gitlab--api-request-async
THEN callback receives parsed alist."
  (require 'shipit-gitlab-http)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "glpat-test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional _cbargs _silent)
                 (let ((buf (generate-new-buffer " *test-url*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
                     (insert "{\"id\": 42, \"title\": \"test MR\"}")
                     (funcall callback nil))))))
      (shipit-gitlab--api-request-async
       config "/projects/123/merge_requests/42"
       (lambda (data) (setq callback-result data))))
    ;; THEN callback received parsed JSON
    (should (not (eq 'not-called callback-result)))
    (should (= 42 (cdr (assq 'id callback-result))))
    (should (string= "test MR" (cdr (assq 'title callback-result))))))

(ert-deftest test/gitlab-api-request-async-error ()
  "GIVEN a GitLab API returning 401 Unauthorized
WHEN calling shipit-gitlab--api-request-async
THEN callback receives nil."
  (require 'shipit-gitlab-http)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "bad-token")))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional _cbargs _silent)
                 (let ((buf (generate-new-buffer " *test-url*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 401 Unauthorized\nContent-Type: application/json\n\n")
                     (insert "{\"message\": \"401 Unauthorized\"}")
                     (funcall callback nil))))))
      (shipit-gitlab--api-request-async
       config "/projects/123/merge_requests/42"
       (lambda (data) (setq callback-result data))))
    ;; THEN callback received nil
    (should (not (eq 'not-called callback-result)))
    (should-not callback-result)))

(ert-deftest test/gitlab-api-request-async-network-error ()
  "GIVEN a url-retrieve that signals a network error via :error status
WHEN calling shipit-gitlab--api-request-async
THEN callback receives nil."
  (require 'shipit-gitlab-http)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test")))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional _cbargs _silent)
                 (let ((buf (generate-new-buffer " *test-url*")))
                   (with-current-buffer buf
                     (funcall callback '(:error (connection-refused))))))))
      (shipit-gitlab--api-request-async
       config "/projects/123/merge_requests/42"
       (lambda (data) (setq callback-result data))))
    ;; THEN callback received nil
    (should (not (eq 'not-called callback-result)))
    (should-not callback-result)))

(ert-deftest test/gitlab-api-request-async-api-error-response ()
  "GIVEN a 200 response with API error body (message key, no id key)
WHEN calling shipit-gitlab--api-request-async
THEN callback receives nil (same detection as sync version)."
  (require 'shipit-gitlab-http)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test")))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional _cbargs _silent)
                 (let ((buf (generate-new-buffer " *test-url*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
                     (insert "{\"message\": \"403 Forbidden\"}")
                     (funcall callback nil))))))
      (shipit-gitlab--api-request-async
       config "/projects/123/test"
       (lambda (data) (setq callback-result data))))
    ;; THEN callback received nil (API error detected)
    (should (not (eq 'not-called callback-result)))
    (should-not callback-result)))

(ert-deftest test/gitlab-api-request-async-sets-auth-header ()
  "GIVEN a GitLab config with token
WHEN calling shipit-gitlab--api-request-async
THEN url-retrieve is called with PRIVATE-TOKEN in url-request-extra-headers."
  (require 'shipit-gitlab-http)
  (let ((captured-headers nil)
        (config '(:api-url "https://gitlab.com" :token "glpat-test")))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &optional _cbargs _silent)
                 (setq captured-headers url-request-extra-headers)
                 (let ((buf (generate-new-buffer " *test-url*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\n\n{}")
                     (funcall callback nil))))))
      (shipit-gitlab--api-request-async
       config "/test"
       #'ignore))
    ;; THEN headers include PRIVATE-TOKEN
    (should (assoc "PRIVATE-TOKEN" captured-headers))))

(provide 'test-shipit-gitlab-http)
;;; test-shipit-gitlab-http.el ends here
