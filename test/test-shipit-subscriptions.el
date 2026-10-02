;;; test-shipit-subscriptions.el --- Tests for subscriptions and starring -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for subscription management buffer and starring.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-http)
(require 'shipit-repo-buffer)
(require 'shipit-subscriptions-buffer)

;;; Test helpers

(defun test-subs--make-minimal-plist ()
  "Return a minimal valid PR backend plist."
  (list :name "Test"
        :fetch-pr #'ignore
        :search #'ignore
        :create-pr #'ignore
        :merge-pr #'ignore
        :update-pr #'ignore
        :fetch-reviews #'ignore
        :submit-review #'ignore
        :fetch-review-decision #'ignore
        :fetch-files #'ignore
        :fetch-commits #'ignore
        :fetch-checks #'ignore
        :browse-url #'ignore))

(defmacro test-subs--with-mock-github (&rest body)
  "Run BODY with GitHub backend and standard test bindings.
Resets the repo-status cache so each test sees a clean slate."
  (declare (indent 0))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo"))
     (when (boundp 'shipit-pr-github--repo-status-cache)
       (clrhash shipit-pr-github--repo-status-cache))
     ,@body))

;;; Tests — get-repo-starred

(ert-deftest test-subs-get-starred-returns-t ()
  "GIVEN a GitHub repo that the user has starred (204 response)
WHEN fetching star status
THEN returns t."
  (test-subs--with-mock-github
    (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
               (lambda (_url _method _headers _data)
                 (cons nil 204))))
      (should (eq t (shipit-pr-github--get-repo-starred
                     '(:repo "owner/repo")))))))

(ert-deftest test-subs-get-starred-returns-nil ()
  "GIVEN a GitHub repo that the user has not starred (404 response)
WHEN fetching star status
THEN returns nil."
  (test-subs--with-mock-github
    (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
               (lambda (_url _method _headers _data)
                 (cons nil 404))))
      (should-not (shipit-pr-github--get-repo-starred
                   '(:repo "owner/repo"))))))

;;; Tests — set-repo-starred

(ert-deftest test-subs-star-repo ()
  "GIVEN a repo
WHEN starring it
THEN PUT is called on the starred endpoint."
  (test-subs--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
                 (lambda (url method _headers _data)
                   (setq called-with (list url method))
                   (cons nil 204))))
        (shipit-pr-github--set-repo-starred '(:repo "owner/repo") t)
        (should (string-match-p "starred/owner/repo" (nth 0 called-with)))
        (should (equal "PUT" (nth 1 called-with)))))))

(ert-deftest test-subs-unstar-repo ()
  "GIVEN a repo
WHEN unstarring it
THEN DELETE is called on the starred endpoint."
  (test-subs--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
                 (lambda (url method _headers _data)
                   (setq called-with (list url method))
                   (cons nil 204))))
        (shipit-pr-github--set-repo-starred '(:repo "owner/repo") nil)
        (should (string-match-p "starred/owner/repo" (nth 0 called-with)))
        (should (equal "DELETE" (nth 1 called-with)))))))

;;; Tests — star indicator

(ert-deftest test-subs-star-indicator-shown-when-starred ()
  "GIVEN a repo that is starred
WHEN checking star label
THEN returns a string containing Starred."
  (let ((label (shipit--star-indicator t)))
    (should (string-match-p "Starred" label))))

(ert-deftest test-subs-star-indicator-empty-when-not-starred ()
  "GIVEN a repo that is not starred
WHEN checking star label
THEN returns empty string."
  (should (string-empty-p (shipit--star-indicator nil))))

;;; Tests — unsupported backend for starring

(ert-deftest test-subs-star-unsupported-backend ()
  "GIVEN a backend without :get-repo-starred
WHEN trying to get star status
THEN returns nil (not an error)."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-subs--make-minimal-plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (fn (plist-get backend :get-repo-starred)))
      (should-not fn))))

;;; Tests — fetch-watched-repos

(ert-deftest test-subs-fetch-watched-repos ()
  "GIVEN a GitHub user with watched repos
WHEN fetching watched repos via GraphQL
THEN returns normalized repo alists with canonical keys."
  (test-subs--with-mock-github
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query _vars)
                 '((viewer . ((watching . ((totalCount . 2)
                                           (pageInfo . ((hasNextPage . :json-false)
                                                        (endCursor)))
                                           (nodes . (((nameWithOwner . "owner/repo-1")
                                                       (description . "First repo")
                                                       (owner . ((login . "owner")))
                                                       (viewerSubscription . "SUBSCRIBED")
                                                       (viewerHasStarred . t))
                                                      ((nameWithOwner . "owner/repo-2")
                                                       (description . "Second repo")
                                                       (owner . ((login . "owner")))
                                                       (viewerSubscription . "IGNORED")
                                                       (viewerHasStarred . :json-false))))))))))))
      (let ((repos (shipit-pr-github--fetch-watched-repos
                    '(:repo ""))))
        (should (= 2 (length repos)))
        ;; Verify normalized keys
        (should (equal "owner/repo-1"
                       (cdr (assq 'full_name (car repos)))))
        (should (equal "owner"
                       (cdr (assq 'owner_name (car repos)))))
        (should (equal "SUBSCRIBED"
                       (cdr (assq 'subscription (car repos)))))
        (should (eq t (cdr (assq 'starred (car repos)))))
        ;; Second repo is ignored, not starred
        (should (equal "IGNORED"
                       (cdr (assq 'subscription (cadr repos)))))
        (should-not (cdr (assq 'starred (cadr repos))))))))

(ert-deftest test-subs-fetch-watched-repos-empty ()
  "GIVEN a GitHub user with no watched repos
WHEN fetching watched repos via GraphQL
THEN returns empty list."
  (test-subs--with-mock-github
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query _vars)
                 '((viewer . ((watching . ((totalCount . 0)
                                           (pageInfo . ((hasNextPage . :json-false)
                                                        (endCursor)))
                                           (nodes)))))))))
      (let ((repos (shipit-pr-github--fetch-watched-repos
                    '(:repo ""))))
        (should-not repos)))))

;;; Tests — subscriptions buffer

(ert-deftest test-subs-buffer-creates-singleton ()
  "GIVEN no existing subscriptions buffer
WHEN creating the buffer
THEN a buffer with the correct name and mode is returned."
  (let ((buf (get-buffer "*shipit-subscriptions*")))
    (when buf (kill-buffer buf)))
  (unwind-protect
      (let ((buf (shipit-subscriptions-buffer-create)))
        (should buf)
        (should (string= "*shipit-subscriptions*" (buffer-name buf)))
        (with-current-buffer buf
          (should (eq major-mode 'shipit-subscriptions-mode))))
    (let ((buf (get-buffer "*shipit-subscriptions*")))
      (when buf (kill-buffer buf)))))

(ert-deftest test-subs-buffer-backend-without-watched-repos-omitted ()
  "GIVEN a backend without :fetch-watched-repos
WHEN collecting backends for the subscriptions buffer
THEN that backend is not included."
  (let ((shipit-pr-backends
         (list (cons 'mock
                     (append (test-subs--make-minimal-plist)
                             (list :name "Mock"))))))
    (let ((backends (shipit-subscriptions--backends-with-watched-repos)))
      (should-not backends))))

(ert-deftest test-subs-buffer-backend-with-watched-repos-included ()
  "GIVEN a backend with :fetch-watched-repos
WHEN collecting backends for the subscriptions buffer
THEN that backend is included."
  (let ((shipit-pr-backends
         (list (cons 'mock
                     (append (test-subs--make-minimal-plist)
                             (list :name "Mock"
                                   :fetch-watched-repos #'ignore
                                   :emoji-fallback "🔧"))))))
    (let ((backends (shipit-subscriptions--backends-with-watched-repos)))
      (should (= 1 (length backends)))
      (should (eq 'mock (car (car backends)))))))

;;; Tests — the repositories that the user owns and does not watch

(defun test-subs--own-page (more cursor &rest nodes)
  "A page of what GitHub answers for the repositories that the user owns.
MORE says whether a page follows, after CURSOR.  Each of NODES is a
name and a subscription."
  `((viewer
     . ((repositories
         . ((pageInfo . ((hasNextPage . ,(if more t :json-false)) (endCursor . ,cursor)))
            (nodes . ,(mapcar (lambda (node)
                                `((nameWithOwner . ,(car node))
                                  (description . "A repository")
                                  (owner . ((login . "me")))
                                  (viewerSubscription . ,(cadr node))
                                  (viewerHasStarred . :json-false)))
                              nodes))))))))

(ert-deftest test-subs-fetch-own-unwatched-repos ()
  "GIVEN a GitHub user who owns three repositories: one watched, one
ignored, and one that is neither
WHEN the repositories that the user owns and does not watch are fetched
THEN only the third is returned, in the form of a watched repository,
AND GitHub was asked for the repositories that the user owns."
  (test-subs--with-mock-github
    (let ((asked nil))
      (cl-letf (((symbol-function 'shipit--graphql-query)
                 (lambda (query _vars)
                   (setq asked query)
                   (test-subs--own-page nil nil
                                        '("me/watched" "SUBSCRIBED")
                                        '("me/quiet" "UNSUBSCRIBED")
                                        '("me/ignored" "IGNORED")))))
        (let ((repos (shipit-pr-github--fetch-own-unwatched-repos '(:repo ""))))
          (should (string-match-p "ownerAffiliations: *OWNER" asked))
          (should (equal '("me/quiet")
                         (mapcar (lambda (repo) (cdr (assq 'full_name repo))) repos)))
          (should (equal "me" (cdr (assq 'owner_name (car repos)))))
          (should (equal "UNSUBSCRIBED" (cdr (assq 'subscription (car repos))))))))))

(ert-deftest test-subs-fetch-own-unwatched-repos-reads-every-page ()
  "GIVEN a GitHub user whose repositories come in two pages
WHEN the repositories that the user owns and does not watch are fetched
THEN those of both pages are returned, AND the second page was asked
for with the cursor of the first."
  (test-subs--with-mock-github
    (let ((cursors nil))
      (cl-letf (((symbol-function 'shipit--graphql-query)
                 (lambda (_query vars)
                   (push (cdr (assq 'cursor vars)) cursors)
                   (if (cdr (assq 'cursor vars))
                       (test-subs--own-page nil nil '("me/second" "UNSUBSCRIBED"))
                     (test-subs--own-page t "c1" '("me/first" "UNSUBSCRIBED"))))))
        (should (equal '("me/first" "me/second")
                       (mapcar (lambda (repo) (cdr (assq 'full_name repo)))
                               (shipit-pr-github--fetch-own-unwatched-repos '(:repo "")))))
        (should (equal '(nil "c1") (reverse cursors)))))))

(defun test-subs--repo (name subscription)
  "A repository NAME of the user with SUBSCRIPTION, as a backend gives it."
  `((full_name . ,name) (owner_name . "me") (description . "A repository")
    (subscription . ,subscription) (starred . :json-false)))

(defmacro test-subs--in-buffer (watched own &rest body)
  "Run BODY in the subscriptions buffer of one backend.
The backend gives WATCHED as the repositories that the user watches and
OWN as those that the user owns and does not watch.  With the symbol
`unsupported' as OWN, the backend cannot tell the second kind."
  (declare (indent 2))
  `(let* ((own ,own)
          (shipit-pr-backends
           (list (cons 'mock
                       (append (test-subs--make-minimal-plist)
                               (list :name "Mock" :emoji-fallback "M"
                                     :fetch-watched-repos (lambda (_config) ,watched))
                               (unless (eq own 'unsupported)
                                 (list :fetch-own-unwatched-repos (lambda (_config) own))))))))
     (when (get-buffer "*shipit-subscriptions*")
       (kill-buffer "*shipit-subscriptions*"))
     (unwind-protect
         (progn
           (shipit-subscriptions-refresh)
           (with-current-buffer "*shipit-subscriptions*"
             ,@body))
       (when (get-buffer "*shipit-subscriptions*")
         (kill-buffer "*shipit-subscriptions*")))))

(defun test-subs--own-sections ()
  "The sections of repositories that are owned and not watched, in this buffer.
The section of a backend starts folded, and has its body once it is
shown, so each is shown first."
  (seq-mapcat (lambda (backend)
                (magit-section-show backend)
                (seq-filter (lambda (section)
                              (eq (oref section type) 'shipit-subscriptions-own))
                            (oref backend children)))
              (oref magit-root-section children)))

(defun test-subs--names-in (section)
  "The names of the repositories that SECTION lists."
  (mapcar (lambda (child) (cdr (assq 'full_name (oref child value))))
          (oref section children)))

(ert-deftest test-subs-buffer-lists-own-repos-that-are-not-watched ()
  "GIVEN a backend with one watched repository and two that the user
owns and does not watch
WHEN the subscriptions buffer is drawn
THEN it has a section for the two, in the order of their names, with
their count in its heading."
  (test-subs--in-buffer (list (test-subs--repo "me/watched" "SUBSCRIBED"))
      (list (test-subs--repo "me/quiet" "UNSUBSCRIBED")
            (test-subs--repo "me/also-quiet" "UNSUBSCRIBED"))
    (let ((sections (test-subs--own-sections)))
      (should (= 1 (length sections)))
      (should (equal '("me/also-quiet" "me/quiet") (test-subs--names-in (car sections))))
      (goto-char (oref (car sections) start))
      (should (string-match-p "(2)" (buffer-substring (point) (line-end-position)))))))

(ert-deftest test-subs-buffer-own-repo-line-is-a-repo-to-act-on ()
  "GIVEN the subscriptions buffer with a repository that the user owns
and does not watch
WHEN point is on its line
THEN the repository at point is that one, with its backend, so the keys
that open it and that set its subscription act on it."
  (test-subs--in-buffer nil (list (test-subs--repo "me/quiet" "UNSUBSCRIBED"))
    (should (test-subs--own-sections))
    (goto-char (point-min))
    ;; `text-property-any' compares with `eq', which no two strings are.
    (goto-char (prop-match-beginning
                (text-property-search-forward 'shipit-subscriptions-repo "me/quiet" #'equal)))
    (should (equal '("me/quiet" . mock) (shipit-subscriptions--repo-at-point)))))

(ert-deftest test-subs-buffer-has-no-own-section-with-nothing-to-list ()
  "GIVEN a backend that names no repository as owned and not watched, AND
a backend that cannot tell such repositories at all
WHEN the subscriptions buffer of each is drawn
THEN neither has the section."
  (dolist (own (list nil 'unsupported))
    (test-subs--in-buffer (list (test-subs--repo "me/watched" "SUBSCRIBED")) own
      (should-not (test-subs--own-sections)))))

(ert-deftest test-subs-buffer-text-filter-narrows-the-own-repos ()
  "GIVEN the subscriptions buffer with two repositories that the user
owns and does not watch
WHEN the text filter is set to a part of the name of one AND the buffer
is drawn again
THEN the section lists that one alone."
  (test-subs--in-buffer nil
      (list (test-subs--repo "me/quiet" "UNSUBSCRIBED")
            (test-subs--repo "me/other" "UNSUBSCRIBED"))
    (setq shipit-subscriptions--filter-text "qui")
    (unwind-protect
        (progn
          (shipit-subscriptions--rerender)
          (should (equal '("me/quiet")
                         (test-subs--names-in (car (test-subs--own-sections))))))
      (setq shipit-subscriptions--filter-text ""))))

;;; Tests — GitLab subscription operations

(ert-deftest test-subs-gitlab-get-subscription-watching ()
  "GIVEN a GitLab project with notification level watch
WHEN fetching subscription
THEN returns subscribed=t, ignored=false."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com" :project-path "group/proj")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) '((level . "watch")))))
      (let ((result (shipit-pr-gitlab--get-repo-subscription
                     '(:project-path "group/proj" :api-url "https://gitlab.com"))))
        (should (eq t (cdr (assq 'subscribed result))))
        (should (eq :json-false (cdr (assq 'ignored result))))))))

(ert-deftest test-subs-gitlab-get-subscription-disabled ()
  "GIVEN a GitLab project with notification level disabled
WHEN fetching subscription
THEN returns ignored=t."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) '((level . "disabled")))))
    (let ((result (shipit-pr-gitlab--get-repo-subscription
                   '(:project-path "group/proj" :api-url "https://gitlab.com"))))
      (should (eq t (cdr (assq 'ignored result)))))))

(ert-deftest test-subs-gitlab-set-subscription-watching ()
  "GIVEN a GitLab project
WHEN setting subscription to watching
THEN PUT is called with level=watch."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config _path data method)
                 (setq called-with (list data method)))))
      (shipit-pr-gitlab--set-repo-subscription
       '(:project-path "group/proj" :api-url "https://gitlab.com") "watching")
      (should (equal "watch" (cdr (assq 'level (nth 0 called-with)))))
      (should (equal "PUT" (nth 1 called-with))))))

(ert-deftest test-subs-gitlab-star-repo ()
  "GIVEN a GitLab project
WHEN starring it
THEN POST is called on /star endpoint."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq called-with (list path method)))))
      (shipit-pr-gitlab--set-repo-starred
       '(:project-path "group/proj" :api-url "https://gitlab.com") t)
      (should (string-match-p "/star$" (nth 0 called-with)))
      (should (equal "POST" (nth 1 called-with))))))

(ert-deftest test-subs-gitlab-fetch-merge-methods ()
  "GIVEN a GitLab project with merge method and squash option
WHEN fetching merge methods
THEN returns correct method list."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               '((merge_method . "merge")
                 (squash_option . "default_on")))))
    (let ((methods (shipit-pr-gitlab--fetch-merge-methods
                    '(:project-path "group/proj" :api-url "https://gitlab.com"))))
      (should (member "merge" methods))
      (should (member "squash" methods)))))

(ert-deftest test-subs-gitlab-fetch-merge-methods-ff-only ()
  "GIVEN a GitLab project with ff merge method and squash=never
WHEN fetching merge methods
THEN returns only rebase."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               '((merge_method . "ff")
                 (squash_option . "never")))))
    (let ((methods (shipit-pr-gitlab--fetch-merge-methods
                    '(:project-path "group/proj" :api-url "https://gitlab.com"))))
      (should (member "rebase" methods))
      (should-not (member "merge" methods))
      (should-not (member "squash" methods)))))

(provide 'test-shipit-subscriptions)
;;; test-shipit-subscriptions.el ends here
