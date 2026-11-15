;;; test-shipit-pr-stack.el --- Tests for stacked pull requests -*- lexical-binding: t; -*-

;;; Commentary:
;; Stacked pull requests: the backend operation that reports a stack,
;; the registry dispatch that makes it optional, and the buffer section
;; that renders it.
;;
;; Every rendering test runs under both variants -- a backend that
;; reports stacks and one that does not -- because the section must
;; elide entirely rather than render empty for GitLab.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-buffer)

;;;; Fixtures

(defconst test-stack--membership
  '((id . 900) (number . 7) (size . 3) (position . 2)
    (base . ((ref . "main") (sha . "aaa"))))
  "The `stack' object a stacked pull request carries on its own resource.")

(defconst test-stack--resource
  '((id . 900) (number . 7) (node_id . "S_kw") (open . t)
    (base . ((ref . "main")))
    (pull_requests . [((number . 101) (state . "open") (draft . t)
                       (merged_at . nil) (head . ((ref . "feat/auth") (sha . "a1")))
                       (title . "Add auth") (user . ((login . "ada"))))
                      ((number . 102) (state . "open") (draft . nil)
                       (merged_at . nil) (head . ((ref . "feat/api") (sha . "b2")))
                       (title . "Add api") (user . ((login . "ada"))))
                      ((number . 103) (state . "open") (draft . nil)
                       (merged_at . nil) (head . ((ref . "feat/ui") (sha . "c3")))
                       (title . "Add ui") (user . ((login . "lin"))))]))
  "The stack resource returned by GET /repos/{owner}/{repo}/stacks/{n}.")

(defconst test-stack--normalized
  '((number . 7) (size . 3) (position . 2) (base . "main") (open . t)
    (entries . (((number . 101) (state . "open") (draft . t) (merged-at . nil)
                 (head . "feat/auth") (title . "Add auth") (author . "ada")
                 (current . nil))
                ((number . 102) (state . "open") (draft . nil) (merged-at . nil)
                 (head . "feat/api") (title . "Add api") (author . "ada")
                 (current . t))
                ((number . 103) (state . "open") (draft . nil) (merged-at . nil)
                 (head . "feat/ui") (title . "Add ui") (author . "lin")
                 (current . nil)))))
  "What the backend must hand back for PR 102 of the fixture stack.")

(defun test-stack--with-backend (plist body)
  "Run BODY with a PR backend registered from PLIST."
  (let ((shipit-pr-backend 'test-stack)
        (shipit-pr-backends (list (cons 'test-stack plist))))
    (funcall body)))

;;;; Normalisation

(ert-deftest test-shipit-stack-normalizes-bottom-to-top ()
  "GIVEN a stack resource and the membership object of PR 102
WHEN the GitHub backend normalises them
THEN the entries come back bottom-to-top, carrying state, draft, head
and a flag marking the pull request the buffer is showing."
  (should (equal test-stack--normalized
                 (shipit-pr-github--normalize-stack
                  test-stack--resource test-stack--membership 102))))

(ert-deftest test-shipit-stack-position-comes-from-the-pull-request ()
  "GIVEN a stack whose size and position live on the pull request
WHEN the stack is normalised
THEN position and size are taken from there, not counted from entries,
so a stack the viewer only partly sees still reports its true depth."
  (let ((stack (shipit-pr-github--normalize-stack
                test-stack--resource
                '((number . 7) (size . 9) (position . 4)) 102)))
    (should (equal 9 (cdr (assq 'size stack))))
    (should (equal 4 (cdr (assq 'position stack))))))

;;;; Fetching

(ert-deftest test-shipit-stack-unstacked-pr-reports-no-stack ()
  "GIVEN a pull request whose resource carries no `stack' object
WHEN the backend is asked for its stack
THEN it answers nil and never requests the stacks endpoint, because an
unstacked pull request is the common case and must stay one request."
  (let ((stack-calls 0))
    (cl-letf (((symbol-function 'shipit-pr-github--fetch-pr)
               (lambda (&rest _) '((number . 55) (title . "solo"))))
              ((symbol-function 'shipit--api-request)
               (lambda (&rest _) (cl-incf stack-calls) nil)))
      (should-not (shipit-pr-github--fetch-stack '(:repo "o/r") 55))
      (should (equal 0 stack-calls)))))

(ert-deftest test-shipit-stack-fetch-composes-pr-and-stack-resource ()
  "GIVEN a stacked pull request
WHEN the backend is asked for its stack
THEN it reads membership from the pull request and the entries from the
stacks endpoint for that stack number."
  (let ((requested nil))
    (cl-letf (((symbol-function 'shipit-pr-github--fetch-pr)
               (lambda (&rest _) `((number . 102) (stack . ,test-stack--membership))))
              ((symbol-function 'shipit--api-request)
               (lambda (endpoint &rest _) (setq requested endpoint) test-stack--resource)))
      (should (equal test-stack--normalized
                     (shipit-pr-github--fetch-stack '(:repo "o/r") 102)))
      (should (equal "/repos/o/r/stacks/7" requested)))))

;;;; Registry dispatch

(ert-deftest test-shipit-stack-absent-op-means-unsupported ()
  "GIVEN a backend with no :fetch-stack, as GitLab has none
WHEN a stack is requested
THEN the dispatch reports the feature unsupported and returns nil,
rather than erroring, so the section can elide."
  (test-stack--with-backend
   '(:name "Stackless")
   (lambda ()
     (should-not (shipit-pr--stack-supported-p "o/r"))
     (should-not (shipit-pr--fetch-stack "o/r" 1)))))

(ert-deftest test-shipit-stack-add-sends-what-the-endpoint-requires ()
  "GIVEN the github backend
WHEN a pull request is added to a stack
THEN the payload carries `pull_requests' as a list of integers, which
is what the endpoint requires.  The singular key it sent before was
refused with a 422 that the caller then counted as success."
  (let ((sent nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional _method)
                 (setq sent (cons endpoint data))
                 '((number . 17)))))
      (funcall (plist-get (cdr (assq 'github shipit-pr-backends))
                          :add-to-stack)
               '(:repo "o/r") 17 18)
      (should (equal "/repos/o/r/stacks/17/add" (car sent)))
      (should (equal [18]
                     (cdr (assq 'pull_requests (cdr sent))))))))

(ert-deftest test-shipit-stack-github-answers-for-a-branchs-pull-request ()
  "GIVEN the github backend
WHEN asked which pull request a branch heads
THEN it answers the number through the shared branch lookup, whose
cache it thereby shares.  The op was missing, so everything that
plans or detects stacks quietly got nil on real GitHub."
  (cl-letf (((symbol-function 'shipit-get-pr-for-branch)
             (lambda (branch &optional repo)
               (when (and (equal branch "part-one") (equal repo "o/r"))
                 '((number . 7) (title . "part one"))))))
    (let ((op (plist-get (cdr (assq 'github shipit-pr-backends))
                         :fetch-pr-for-branch)))
      (should (functionp op))
      (should (equal 7 (funcall op '(:repo "o/r") "part-one")))
      (should-not (funcall op '(:repo "o/r") "no-pr-branch")))))

(ert-deftest test-shipit-stack-present-op-is-dispatched ()
  "GIVEN a backend that registers :fetch-stack
WHEN a stack is requested
THEN the operation is called with the resolved config and the number."
  (let (seen)
    (test-stack--with-backend
     (list :name "Stacked"
           :fetch-stack (lambda (config number &optional pr-data)
                          (setq seen (list (plist-get config :repo) number pr-data))
                          test-stack--normalized))
     (lambda ()
       (should (shipit-pr--stack-supported-p "o/r"))
       (should (equal test-stack--normalized (shipit-pr--fetch-stack "o/r" 102)))
       (should (equal '("o/r" 102 nil) seen))))))

;;;; Rendering

(ert-deftest test-shipit-stack-section-type-registered ()
  "The stack section is a magit section type, so navigation and
collapsing work the way every other section in the buffer does."
  (should (eq (get 'shipit-stack-section 'magit-section) t)))

(ert-deftest test-shipit-stack-section-shows-every-entry-and-position ()
  "GIVEN a pull request that sits second in a stack of three
WHEN the stack section is inserted
THEN the heading states the position, every sibling is listed with its
title, and the row for this pull request is marked."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     (list :name "Stacked" :fetch-stack (lambda (&rest _) test-stack--normalized))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (let ((text (buffer-string)))
         (should (string-match-p "Stack #7" text))
         (should (string-match-p "2 of 3" text))
         (dolist (n '("101" "102" "103"))
           (should (string-match-p n text)))
         (should (string-match-p "Add auth" text))
         (should (string-match-p "this PR" text))
         ;; the base is a row like the others, not an indented orphan
         (should (string-match-p "^   main" text)))))))

(ert-deftest test-shipit-stack-section-elides-without-backend-support ()
  "GIVEN a backend that cannot report stacks
WHEN the stack section is inserted
THEN nothing is written at all, rather than an empty heading."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     '(:name "Stackless")
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (should (equal "" (string-trim (buffer-string))))))))

(ert-deftest test-shipit-stack-section-elides-for-unstacked-pr ()
  "GIVEN a backend that supports stacks but a pull request in none
WHEN the stack section is inserted
THEN nothing is written, so solo pull requests look as they always did."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     (list :name "Stacked" :fetch-stack (lambda (&rest _) nil))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 55))))
       (should (equal "" (string-trim (buffer-string))))))))

(ert-deftest test-shipit-stack-entry-row-carries-navigation-properties ()
  "GIVEN a rendered stack
WHEN a sibling's row is examined
THEN it carries the repository and pull request number, which is what
RET on the row needs; the section is found by text property rather
than by matching its heading text."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     (list :name "Stacked" :fetch-stack (lambda (&rest _) test-stack--normalized))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (let ((pos (text-property-any (point-min) (point-max) 'shipit-stack t)))
         (should pos))
       (goto-char (point-min))
       (let ((row (text-property-any (point-min) (point-max) 'shipit-stack-pr-number 101)))
         (should row)
         (should (equal "o/r" (get-text-property row 'shipit-stack-repo))))))))

(ert-deftest test-shipit-stack-uses-the-pull-request-already-in-hand ()
  "GIVEN the pull request data the buffer already holds
WHEN the stack is fetched with it
THEN no request is made for the pull request again.

The section is drawn on every render.  Refetching to read a field the
caller already has turns each render into a round trip, and a 304 is
still a round trip: it shows up as \"Contacting host\" and makes an
unstacked pull request pay for a feature it is not using."
  (let ((pr-fetches 0))
    (cl-letf (((symbol-function 'shipit-pr-github--fetch-pr)
               (lambda (&rest _) (cl-incf pr-fetches) nil))
              ((symbol-function 'shipit--api-request)
               (lambda (&rest _) test-stack--resource)))
      (should (equal test-stack--normalized
                     (shipit-pr-github--fetch-stack
                      '(:repo "o/r") 102
                      `((number . 102) (stack . ,test-stack--membership)))))
      (should (equal 0 pr-fetches)))))

(ert-deftest test-shipit-stack-unstacked-pr-in-hand-makes-no-request ()
  "GIVEN pull request data carrying no stack
WHEN the stack is fetched with it
THEN nothing is requested at all, which is the common case on every
render of every ordinary pull request."
  (let ((requests 0))
    (cl-letf (((symbol-function 'shipit-pr-github--fetch-pr)
               (lambda (&rest _) (cl-incf requests) nil))
              ((symbol-function 'shipit--api-request)
               (lambda (&rest _) (cl-incf requests) nil)))
      (should-not (shipit-pr-github--fetch-stack
                   '(:repo "o/r") 55 '((number . 55) (title . "solo"))))
      (should (equal 0 requests)))))

(ert-deftest test-shipit-stack-row-washes-in-details-on-expand ()
  "GIVEN a rendered stack whose backend can answer for each sibling
WHEN the buffer is drawn
THEN nothing is fetched and no detail is shown, since six siblings
fetched eagerly would put a dozen requests on every render
WHEN a row is expanded
THEN the sibling's branch, author and description appear, nested
under that row, fetched once
WHEN the row is hidden and expanded again
THEN nothing is fetched again: a row washes once per render."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (let ((pr-fetches 0))
      (test-stack--with-backend
       (list :name "Stacked"
             :fetch-stack (lambda (&rest _) test-stack--normalized)
             :fetch-pr (lambda (_config number)
                         (cl-incf pr-fetches)
                         (list (cons 'number number)
                               (cons 'body "Why this layer exists."))))
       (lambda ()
         (magit-insert-section (shipit-pr-buffer)
           (shipit--insert-stack-section "o/r" '((number . 102))))
         (should (equal 0 pr-fetches))
         (should-not (string-match-p "feat/auth" (buffer-string)))
         (should-not (string-match-p "Why this layer exists." (buffer-string)))
         (let ((layer (seq-find (lambda (s) (equal "Add auth" (oref s value)))
                                (shipit--find-all-sections-by-type
                                 'shipit-stack-layer))))
           (should layer)
           (should (oref layer hidden))
           (magit-section-show layer)
           (should (string-match-p "feat/auth" (buffer-string)))
           (should (string-match-p "ada" (buffer-string)))
           (should (string-match-p "Why this layer exists." (buffer-string)))
           (should (equal 1 pr-fetches))
           (should (oref layer children))
           (dolist (child (oref layer children))
             (should (eq layer (oref child parent))))
           (magit-section-hide layer)
           (magit-section-show layer)
           (should (equal 1 pr-fetches))))))))

(ert-deftest test-shipit-stack-washed-children-start-shut-and-indented ()
  "GIVEN a row washed in on expand
WHEN its details arrive
THEN the description is its own section, born shut, so expanding a
row shows a ladder rather than everything at once
AND every washed line sits deeper than the row's own text: branch and
the child heading two columns in, its content deeper still, past the
column the heading's icon and word occupy."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     (list :name "Stacked"
           :fetch-stack (lambda (&rest _) test-stack--normalized)
           :fetch-pr (lambda (_config number)
                       (list (cons 'number number)
                             (cons 'body "Why this layer exists."))))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (let ((layer (seq-find (lambda (s) (equal "Add auth" (oref s value)))
                              (shipit--find-all-sections-by-type
                               'shipit-stack-layer))))
         (magit-section-show layer)
         (let ((children (oref layer children)))
           (should (equal 1 (length children)))
           (dolist (child children)
             (should (oref child hidden))
             (should (invisible-p (marker-position (oref child content))))))
         (let ((text (buffer-string)))
           (should (string-match-p "^     feat/auth$" text))
           (should (string-match-p "^     [^ ].*Description" text))
           (should (string-match-p "^        Why this layer exists\\." text))))))))

(ert-deftest test-shipit-stack-state-column-wears-the-state-colour ()
  "GIVEN a rendered stack holding a draft and an open sibling
WHEN the state column is inspected
THEN each state word carries the face shipit gives that state
everywhere else, under both icon variants."
  (dolist (shipit-use-svglib-icons '(nil t))
    (with-temp-buffer
      (magit-section-mode)
      (setq-local inhibit-read-only t)
      (test-stack--with-backend
       (list :name "Stacked" :fetch-stack (lambda (&rest _) test-stack--normalized))
       (lambda ()
         (magit-insert-section (shipit-pr-buffer)
           (shipit--insert-stack-section "o/r" '((number . 102))))
         (dolist (state '("draft" "open"))
           (goto-char (point-min))
           (should (search-forward state nil t))
           (should (equal (get-text-property (match-beginning 0) 'font-lock-face)
                          (get-text-property 0 'font-lock-face
                                             (shipit--colorize-pr-state state)))))
         ;; without an icon the word starts the column; the space
         ;; belongs to the icon, not the text
         (should-not (string-prefix-p " " (shipit--stack-entry-state-text
                                           '((state . "open"))))))))))

(ert-deftest test-shipit-stack-ret-in-a-shipit-buffer-opens-a-shipit-buffer ()
  "GIVEN the stack rendered inside a dedicated shipit buffer
WHEN RET is pressed on a sibling's row
THEN the sibling opens as a dedicated buffer too, whatever
`shipit-use-dedicated-buffer' says.  That option chooses how magit's
own views open things; a reader already in a shipit buffer stays in
shipit, which is where RET on this row used to fall back to the magit
integration and look like nothing happened."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (setq major-mode 'shipit-mode)
    (test-stack--with-backend
     (list :name "Stacked" :fetch-stack (lambda (&rest _) test-stack--normalized))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (let ((shipit-use-dedicated-buffer nil)
             (opened nil))
         (cl-letf (((symbol-function 'shipit-open-pr-buffer)
                    (lambda (n &optional r &rest _) (setq opened (list n r)))))
           (goto-char (text-property-any (point-min) (point-max)
                                         'shipit-stack-pr-number 101))
           (funcall (key-binding (kbd "RET"))))
         (should (equal '(101 "o/r") opened)))))))

;;;; Check rollups and review decisions on the rows

(ert-deftest test-shipit-stack-statuses-one-query-answers-for-the-stack ()
  "GIVEN the github backend
WHEN the statuses of a stack's numbers are fetched
THEN one GraphQL query asks for every number and the answer maps each
to its downcased check rollup and review decision, leaving out
siblings with neither, so the caller draws nothing for them the way
the forge does."
  (let ((queries nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (query _variables)
                 (push query queries)
                 '((repository
                    . ((p101 . ((reviewDecision . "APPROVED")
                                (commits
                                 . ((nodes
                                     . [((commit
                                          . ((statusCheckRollup
                                              . ((state . "SUCCESS"))))))])))))
                       (p102 . ((reviewDecision . "CHANGES_REQUESTED")
                                (commits
                                 . ((nodes
                                     . [((commit
                                          . ((statusCheckRollup
                                              . ((state . "FAILURE"))))))])))))
                       (p103 . ((reviewDecision . "REVIEW_REQUIRED")
                                (commits
                                 . ((nodes
                                     . [((commit
                                          . ((statusCheckRollup
                                              . nil))))])))))
                       (p104 . ((reviewDecision . nil)
                                (commits
                                 . ((nodes
                                     . [((commit
                                          . ((statusCheckRollup
                                              . nil))))])))))))))))
      (let ((op (plist-get (cdr (assq 'github shipit-pr-backends))
                           :fetch-stack-statuses)))
        (should (functionp op))
        (should (equal '((101 . ((checks . "success") (review . "approved")))
                         (102 . ((checks . "failure")
                                 (review . "changes_requested")))
                         (103 . ((review . "review_required"))))
                       (funcall op '(:repo "o/r") '(101 102 103 104))))
        (should (equal 1 (length queries)))
        (dolist (number '("101" "102" "103" "104"))
          (should (string-match-p number (car queries))))
        (should (string-match-p "statusCheckRollup" (car queries)))
        (should (string-match-p "reviewDecision" (car queries)))))))

(ert-deftest test-shipit-stack-statuses-absent-op-answers-nothing ()
  "GIVEN a backend with no :fetch-stack-statuses, as the sandbox and
GitLab have none
WHEN statuses are asked for
THEN the dispatch answers nil rather than erroring, so rows render
without a glyph."
  (test-stack--with-backend
   '(:name "Stackless")
   (lambda ()
     (should-not (shipit-pr--fetch-stack-statuses "o/r" '(1 2))))))

(ert-deftest test-shipit-stack-rows-carry-the-check-rollup ()
  "GIVEN a backend that reports stacks and their check rollups
WHEN the stack section is inserted with SVG icons off
THEN a passing sibling's row carries the green check, a failing one's
the red x, and one with no checks nothing extra, so whether the stack
is green can be read without opening each sibling."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (let ((shipit-use-svglib-icons nil)
          (asked nil))
      (test-stack--with-backend
       (list :name "Stacked"
             :fetch-stack (lambda (&rest _) test-stack--normalized)
             :fetch-stack-statuses
             (lambda (_config numbers)
               (setq asked numbers)
               '((101 . ((checks . "success")))
                 (102 . ((checks . "failure"))))))
       (lambda ()
         (magit-insert-section (shipit-pr-buffer)
           (shipit--insert-stack-section "o/r" '((number . 102))))
         (should (equal '(101 102 103) asked))
         (let ((lines (split-string (buffer-string) "\n")))
           (dolist (want '(("Add auth" "✅" t)
                           ("Add api" "❌" t)
                           ("Add ui" "✅\\|❌\\|🟡" nil)))
             (let ((line (cl-find-if
                          (lambda (l) (string-match-p (nth 0 want) l))
                          lines)))
               (should line)
               (if (nth 2 want)
                   (should (string-match-p (nth 1 want) line))
                 (should-not (string-match-p (nth 1 want) line)))))))))))

(ert-deftest test-shipit-stack-rows-carry-the-review-decision ()
  "GIVEN a backend that reports stacks and their review decisions
WHEN the stack section is inserted with SVG icons off
THEN an approved sibling's row carries the green check, one with
changes requested the red x, and one still waiting for review the
hourglass, so where the stack stands with its reviewers can be read
without opening each sibling."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (let ((shipit-use-svglib-icons nil))
      (test-stack--with-backend
       (list :name "Stacked"
             :fetch-stack (lambda (&rest _) test-stack--normalized)
             :fetch-stack-statuses
             (lambda (&rest _)
               '((101 . ((review . "approved")))
                 (102 . ((review . "changes_requested")))
                 (103 . ((review . "review_required"))))))
       (lambda ()
         (magit-insert-section (shipit-pr-buffer)
           (shipit--insert-stack-section "o/r" '((number . 102))))
         (let ((lines (split-string (buffer-string) "\n")))
           (dolist (want '(("Add auth" "✅")
                           ("Add api" "❌")
                           ("Add ui" "⏳")))
             (let ((line (cl-find-if
                          (lambda (l) (string-match-p (nth 0 want) l))
                          lines)))
               (should line)
               (should (string-match-p (nth 1 want) line))))))))))

(ert-deftest test-shipit-stack-rollup-failure-does-not-break-the-section ()
  "GIVEN a rollup fetch that fails
WHEN the stack section is inserted
THEN the rows still render, without glyphs: the rollup is a nicety
and its failure belongs in the debug log, not across the buffer."
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (let ((shipit-use-svglib-icons nil))
      (test-stack--with-backend
       (list :name "Stacked"
             :fetch-stack (lambda (&rest _) test-stack--normalized)
             :fetch-stack-statuses (lambda (&rest _) (error "rollup down")))
       (lambda ()
         (magit-insert-section (shipit-pr-buffer)
           (shipit--insert-stack-section "o/r" '((number . 102))))
         (let ((text (buffer-string)))
           (should (string-match-p "Add auth" text))
           (should-not (string-match-p "✅\\|❌" text))))))))

;;;; Copying the stack's address

(ert-deftest test-shipit-stack-github-names-the-stacks-web-address ()
  "GIVEN the github backend
WHEN asked where a stack of a repository lives
THEN it answers the stacks path under the web host, mirroring the API
path the way the pull request URL mirrors its own."
  (let ((op (plist-get (cdr (assq 'github shipit-pr-backends))
                       :browse-stack-url)))
    (should (functionp op))
    (should (equal "https://github.com/o/r/stacks/17"
                   (funcall op '(:repo "o/r") 17)))))

(ert-deftest test-shipit-stack-copy-absent-op-answers-nothing ()
  "GIVEN a backend with no :browse-stack-url
WHEN the URL dispatch is asked
THEN nil, so M-w falls back to copying the line rather than erroring."
  (test-stack--with-backend
   '(:name "Stackless")
   (lambda ()
     (should-not (shipit-pr--browse-stack-url "o/r" 7)))))

(ert-deftest test-shipit-stack-copy-yields-the-address-of-what-is-named ()
  "GIVEN a rendered stack section
WHEN the copy command runs on the stack heading and then on a row
THEN the heading yields the stack's web address and the row its
sibling pull request's, the same address the pull request title
copies.  A row names a pull request, and M-w answering with a line of
text there read as not working."
  (require 'shipit-actions)
  (with-temp-buffer
    (magit-section-mode)
    (setq-local inhibit-read-only t)
    (test-stack--with-backend
     (list :name "Stacked"
           :fetch-stack (lambda (&rest _) test-stack--normalized)
           :browse-url
           (lambda (_config number)
             (format "https://forge.example/pull/%s" number))
           :browse-stack-url
           (lambda (_config number)
             (format "https://forge.example/stacks/%s" number)))
     (lambda ()
       (magit-insert-section (shipit-pr-buffer)
         (shipit--insert-stack-section "o/r" '((number . 102))))
       (goto-char (text-property-any (point-min) (point-max)
                                     'shipit-stack t))
       (shipit-buffer-copy-dwim)
       (should (equal "https://forge.example/stacks/7" (current-kill 0)))
       (goto-char (text-property-any (point-min) (point-max)
                                     'shipit-stack-pr-number 101))
       (shipit-buffer-copy-dwim)
       (should (equal "https://forge.example/pull/101" (current-kill 0)))))))

(provide 'test-shipit-pr-stack)
;;; test-shipit-pr-stack.el ends here
