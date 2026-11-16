;;; test-shipit-pr-github.el --- Tests for GitHub PR backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitHub PR backend create-pr payload, with focus on
;; the draft flag being threaded into the REST API request body.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-test-http)
;; The search path reaches query builders living outside the backend;
;; under make an earlier file loads them, alone nothing did.
(require 'shipit-commands)

(defun test-github--create-pr-payload (draft)
  "Call create-pr with DRAFT and return the captured POST data alist."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (_endpoint data &optional _method)
                 (setq captured data)
                 '((number . 1) (html_url . "https://example/1")))))
      (shipit-pr-github--create-pr '(:repo "owner/repo")
                                   "Add feature" "Body" "main" "feature" draft)
      captured)))

(ert-deftest test-shipit-pr-github--create-pr-draft-true ()
  "GIVEN draft is non-nil
WHEN creating a GitHub PR
THEN the POST payload sets the draft field to t (true)."
  ;; WHEN
  (let ((data (test-github--create-pr-payload t)))
    ;; THEN
    (should (eq t (cdr (assq 'draft data))))))

(ert-deftest test-shipit-pr-github--create-pr-draft-false ()
  "GIVEN draft is nil
WHEN creating a GitHub PR
THEN the POST payload sets the draft field to :json-false (ready for review)."
  ;; WHEN
  (let ((data (test-github--create-pr-payload nil)))
    ;; THEN
    (should (eq :json-false (cdr (assq 'draft data))))))

(ert-deftest test-shipit-pr-github--create-pr-keeps-core-fields ()
  "GIVEN a draft PR request
WHEN creating a GitHub PR
THEN title, body, base and head are still present in the payload."
  ;; WHEN
  (let ((data (test-github--create-pr-payload t)))
    ;; THEN
    (should (equal "Add feature" (cdr (assq 'title data))))
    (should (equal "Body" (cdr (assq 'body data))))
    (should (equal "main" (cdr (assq 'base data))))
    (should (equal "feature" (cdr (assq 'head data))))))

;;; Draft state toggling (GraphQL-only on GitHub)

(ert-deftest test-shipit-pr-github-set-pr-draft-ready-uses-graphql-mutation ()
  "GIVEN a draft PR whose GraphQL node id resolves
WHEN marking it ready for review through the backend op
THEN the markPullRequestReadyForReview mutation is sent with that id."
  (let (captured-query captured-vars)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) "PR_node123"))
              ((symbol-function 'shipit--graphql-request)
               (lambda (query &optional variables _callback)
                 (setq captured-query query
                       captured-vars variables)
                 '((data (markPullRequestReadyForReview
                          (pullRequest (isDraft . :json-false))))))))
      (should (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))
      (should (string-match-p "markPullRequestReadyForReview" captured-query))
      (should (equal '((id . "PR_node123")) captured-vars)))))

(ert-deftest test-shipit-pr-github-set-pr-draft-convert-uses-graphql-mutation ()
  "GIVEN an open non-draft PR whose GraphQL node id resolves
WHEN converting it to draft through the backend op
THEN the convertPullRequestToDraft mutation is sent with that id."
  (let (captured-query)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) "PR_node123"))
              ((symbol-function 'shipit--graphql-request)
               (lambda (query &optional _variables _callback)
                 (setq captured-query query)
                 '((data (convertPullRequestToDraft
                          (pullRequest (isDraft . t))))))))
      (should (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 t))
      (should (string-match-p "convertPullRequestToDraft" captured-query)))))

(ert-deftest test-shipit-pr-github-set-pr-draft-fails-on-graphql-errors ()
  "GIVEN the GraphQL mutation answers with an errors array
WHEN toggling draft state through the backend op
THEN the op returns nil so callers can report the failure."
  (cl-letf (((symbol-function 'shipit--get-pr-id)
             (lambda (_repo _number) "PR_node123"))
            ((symbol-function 'shipit--graphql-request)
             (lambda (&rest _)
               '((data . nil)
                 (errors . (((message . "boom")))))))
            ((symbol-function 'message) #'ignore))
    (should-not (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))))

(ert-deftest test-shipit-pr-github-set-pr-draft-fails-without-node-id ()
  "GIVEN the PR node id cannot be resolved
WHEN toggling draft state through the backend op
THEN the op returns nil and no mutation is attempted."
  (let (mutation-called)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) nil))
              ((symbol-function 'shipit--graphql-request)
               (lambda (&rest _) (setq mutation-called t) nil)))
      (should-not (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))
      (should-not mutation-called))))

(ert-deftest test-shipit-pr-github-registers-set-pr-draft ()
  "GIVEN the GitHub backend is loaded
WHEN inspecting its registration
THEN the :set-pr-draft operation is present."
  (should (functionp (plist-get (cdr (assq 'github shipit-pr-backends))
                                :set-pr-draft))))

(ert-deftest test-shipit-pr-github-registers-copilot-reviewer-candidate ()
  "GIVEN the GitHub backend
WHEN asking for bot reviewer candidates
THEN Copilot maps to the copilot-pull-request-reviewer bot login."
  (should (equal '(("Copilot" . "copilot-pull-request-reviewer[bot]"))
                 (funcall (plist-get (cdr (assq 'github shipit-pr-backends))
                                     :reviewer-bot-candidates)
                          '(:repo "owner/repo")))))

(ert-deftest test-github-search-takes-a-plist ()
  "GIVEN search arguments as a plist, the form the preview sends
WHEN the github backend searches
THEN the query is built rather than the arguments crashing the
builders, which expect transient strings; the crash was swallowed
upstream and every existing pull request went undiscovered."
  (let ((reached nil))
    (cl-letf (((symbol-function 'shipit--search-prs-with-encoded-query)
               (lambda (&rest args) (setq reached args) nil)))
      (shipit-pr-github--search '(:repo "o/r") '(:state "all"))
      (should reached))))

;;; Uploaded videos

(ert-deftest test-github-an-uploaded-file-is-a-video ()
  "GIVEN the URL of a file uploaded to a repository, the URL of a file
uploaded to a comment, the URL of a file of a branch, AND a URL of
another site
WHEN shipit asks whether each URL is a video
THEN the two uploaded files are, as GitHub shows such a URL alone on a
line as a video, AND the other two are not."
  (dolist (case '(("https://github.com/owner/repo/assets/1107525/e2659674-c813-402a-8a85-e620f0a6a454" . t)
                  ("https://github.com/user-attachments/assets/0f3c2d4e-1a2b-4c3d-9e8f-123456789abc" . t)
                  ("https://github.com/owner/repo/blob/main/cup.mp4" . nil)
                  ("https://example.com/owner/repo/assets/1/e2659674" . nil)))
    (should (eq (cdr case) (and (shipit--video-url-p (car case)) t)))))

(ert-deftest test-github-an-uploaded-video-folds-as-on-github ()
  "GIVEN a body with a video uploaded to GitHub alone on a line, AND a
video file of another site alone on a line
WHEN the videos of the body are processed
THEN the upload gets a header with an arrow, as GitHub shows an upload
in a section that folds, AND the other video gets a Play button, as
GitHub shows no video there."
  (let* ((upload "https://github.com/user-attachments/assets/0f3c2d4e-1a2b-4c3d-9e8f-123456789abc")
         (shipit-use-svglib-icons nil)
         (out (substring-no-properties
               (shipit--process-inline-videos
                (concat upload "\n\nhttps://example.com/cup.mp4\n")))))
    (should (string-match-p "^🎥 Video ▸$" out))
    (should (string-match-p "^\\[> Play Video\\]$" out))))

(ert-deftest test-github-an-uploaded-video-folds-above-a-table ()
  "GIVEN a body with a video uploaded to GitHub alone on a line, AND a
table of two columns lower down
WHEN the body is prepared for rendering, and its videos are processed
THEN the address stays alone on its line and the upload gets its header
with an arrow, as without the table, AND the table is aligned.  pandoc,
which aligns tables, wrote the whole body again and put the address in
angle brackets, which no video check knew."
  (let* ((upload "https://github.com/user-attachments/assets/0f3c2d4e-1a2b-4c3d-9e8f-123456789abc")
         (shipit-use-svglib-icons nil)
         (prepared (shipit--prepare-markdown
                    (concat "Intro.\n\n" upload "\n\n| Cup | Size |\n| --- | --- |\n| tall | 3 dl |\n")))
         (out (substring-no-properties (shipit--process-inline-videos prepared))))
    (should-not (string-match-p (regexp-quote (concat "<" upload ">")) prepared))
    (should (string-match-p "^| tall | 3 dl |$" prepared))
    (should (string-match-p "^🎥 Video ▸$" out))))

;;; Forks

(defvar shipit--forked-from-cache)

(defmacro test-github--answering (answers &rest body)
  "Run BODY with GitHub giving ANSWERS, and nothing remembered about forks.
ANSWERS are as `shipit-test-http-answering' takes them."
  (declare (indent 1))
  `(let ((shipit-github-token "not-a-token")
         (shipit--github-token-cache nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit--forked-from-cache (make-hash-table :test 'equal)))
     (shipit-test-http-answering ,answers
       ,@body)))

(defun test-github--times-asked-about (repo)
  "How often GitHub was asked about REPO itself, as owner/name."
  (seq-count (lambda (request)
               (string-match-p (concat "/repos/" (regexp-quote repo) "\\'") (cadr request)))
             shipit-test-http-requests))

(ert-deftest test-github-a-fork-of-a-fork-is-one-of-its-parent-and-its-source ()
  "A fork of a fork is a fork of its parent and of the first repository of its chain."
  ;; GIVEN GitHub says that me/r is a fork of guest/r, and that the first
  ;;   repository of its chain is o/r
  ;; WHEN shipit asks whether me/r is a fork of guest/r, of o/r and of
  ;;   pots/r
  ;; THEN it is a fork of guest/r and of o/r, and not of pots/r
  ;; AND GitHub was asked about me/r once
  (test-github--answering
      '(("/repos/me/r\\'" 200 ((full_name . "me/r") (fork . t)
                               (parent (full_name . "guest/r"))
                               (source (full_name . "o/r")))))
    (should (shipit--fork-of-p "me/r" "guest/r"))
    (should (shipit--fork-of-p "me/r" "o/r"))
    (should-not (shipit--fork-of-p "me/r" "pots/r"))
    (should (= 1 (test-github--times-asked-about "me/r")))))

;;; The merged pull request of a branch

(ert-deftest test-github-merged-pr-for-branch-is-the-one-that-merged ()
  "The merged pull request of a branch is the closed one that GitHub merged."
  ;; GIVEN GitHub knows two closed pull requests whose head was cups:
  ;;   #7, closed without a merge, and #9, merged
  ;; WHEN shipit asks which pull request of cups merged
  ;; THEN it answers #9, with the commit that cups stood on when it merged
  ;; AND it asked GitHub for the closed pull requests with cups as head
  (test-github--answering
      '(("/repos/kitchen/menu/pulls\\?" 200
         [((number . 7) (merged_at . nil) (head (ref . "cups") (sha . "c7")))
          ((number . 9) (merged_at . "2026-11-14T10:00:00Z")
           (head (ref . "cups") (sha . "c9")))]))
    (should (equal '((number . 9) (sha . "c9"))
                   (shipit-pr-github--fetch-merged-pr-for-branch
                    '(:repo "kitchen/menu") "cups")))
    (let ((address (cadr (car shipit-test-http-requests))))
      (should (string-match-p "state=closed" address))
      (should (string-match-p "head=kitchen\\(:\\|%3[Aa]\\)cups" address)))))

(ert-deftest test-github-merged-pr-for-branch-is-nil-without-a-merge ()
  "A branch whose pull requests closed without a merge has no merged pull request."
  ;; GIVEN GitHub knows one closed pull request whose head was cups, #7,
  ;;   closed without a merge
  ;; WHEN shipit asks which pull request of cups merged
  ;; THEN it answers none
  (test-github--answering
      '(("/repos/kitchen/menu/pulls\\?" 200
         [((number . 7) (merged_at . nil) (head (ref . "cups")))]))
    (should-not (shipit-pr-github--fetch-merged-pr-for-branch
                 '(:repo "kitchen/menu") "cups"))))

(provide 'test-shipit-pr-github)
;;; test-shipit-pr-github.el ends here
