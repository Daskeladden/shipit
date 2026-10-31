;;; test-shipit-branches.el --- Tests for the branches of a repository -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the branches of a repository as a backend gives them, for
;; how they are grouped, and for the notification of a new branch in a
;; repository that the user watches.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'shipit-branches)
;; The notification of a new branch loads the view of a branch when it
;; is opened, which would take the place of what a test put there.
(require 'shipit-commit-comments-buffer)
(require 'shipit-pr-github)
(require 'shipit-notifications-buffer)
(require 'shipit-test-http)

(defun test-branches--days-ago (days)
  "The time DAYS ago, as GitHub writes a time."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract nil (days-to-time days)) t))

(defun test-branches--pr (number state branch &optional draft cross)
  "A pull request of the repository: NUMBER, in STATE as GraphQL names it,
from BRANCH, and a DRAFT.  With CROSS its head is a branch of that name
in another repository, as a fork has that takes in the default branch."
  `((number . ,number)
    (title . ,(format "Change %d" number))
    (state . ,state)
    (isDraft . ,(if draft t :json-false))
    (isCrossRepository . ,(if cross t :json-false))
    (headRefName . ,branch)))

(defun test-branches--ref (name login days subject)
  "A branch NAME as GraphQL gives it.
Its last commit is by LOGIN, DAYS ago, with SUBJECT."
  `((name . ,name)
    (target . ((oid . ,(md5 name))
               (committedDate . ,(test-branches--days-ago days))
               (messageHeadline . ,subject)
               (author . ((name . ,(capitalize login))
                          (user . ((login . ,login)))))))))

(defun test-branches--cursor-at (offset opaque)
  "The cursor of the page that starts at OFFSET.
GitHub writes the count of the branches before it in base64.  With
OPAQUE it is something else."
  (if opaque
      (format "after-%d" offset)
    (base64-encode-string (number-to-string offset))))

(defun test-branches--offset (cursor)
  "Where the page at CURSOR starts: 0 for no cursor."
  (cond ((null cursor) 0)
        ((string-prefix-p "after-" cursor) (string-to-number (substring cursor 6)))
        (t (string-to-number (base64-decode-string cursor)))))

(defun test-branches--refs-page (refs size offset opaque endless)
  "The page of REFS that starts at OFFSET, with SIZE refs at most.
The default branch of the repository is main.  OPAQUE is as for
`test-branches--cursor-at'.  With ENDLESS a page after the first says
that another page follows, also when none does."
  (let ((end (min (length refs) (+ offset size))))
    `((data . ((repository
                . ((defaultBranchRef . ((name . "main")))
                   (refs . ((totalCount . ,(length refs))
                            (pageInfo
                             . ((hasNextPage . ,(if (or (< end (length refs))
                                                        (and endless (> offset 0)))
                                                    t :json-false))
                                (endCursor . ,(test-branches--cursor-at end opaque))))
                            (nodes . ,(vconcat (seq-subseq refs (min offset (length refs))
                                                           end))))))))))))

(defun test-branches--prs-page (prs states)
  "The pull requests of PRS in STATES, all of them for no STATES, on one page."
  `((data . ((repository
              . ((pullRequests
                  . ((pageInfo . ((hasNextPage . :json-false) (endCursor . "end")))
                     (nodes . ,(vconcat
                                (seq-filter (lambda (pr)
                                              (or (null states)
                                                  (member (alist-get 'state pr)
                                                          (append states nil))))
                                            prs)))))))))))

(defun test-branches--prs-query-p (request)
  "Whether REQUEST, a GraphQL query as sent, asks for pull requests."
  (string-match-p "pullRequests(states" (cdr (assq 'query request))))

(defun test-branches--named-query-p (request)
  "Whether REQUEST, a GraphQL query as sent, asks for branches by name."
  (string-match-p "qualifiedName" (cdr (assq 'query request))))

(defun test-branches--ref-named (refs qualified-name)
  "The branch of REFS whose QUALIFIED-NAME is refs/heads/ and its name, or nil."
  (cl-find (string-remove-prefix "refs/heads/" qualified-name) refs
           :key (lambda (ref) (cdr (assq 'name ref))) :test #'equal))

(defun test-branches--branch-variables (variables)
  "Those of VARIABLES of a query that name a branch: $b0, $b1 and so on."
  (seq-filter (lambda (variable) (string-match-p "\\`b[0-9]+\\'" (symbol-name (car variable))))
              variables))

(defun test-branches--associated-prs (prs name query)
  "The pull requests of PRS whose head is a branch called NAME.
They are as GitHub gives them to QUERY: also those of a fork, and only
the open ones when it asks for those."
  (seq-filter (lambda (pr)
                (and (equal (alist-get 'headRefName pr) name)
                     (or (not (string-match-p "associatedPullRequests(states: OPEN" query))
                         (equal (alist-get 'state pr) "OPEN"))))
              prs))

(defun test-branches--named-ref (refs prs query qualified-name)
  "The branch of REFS with QUALIFIED-NAME and its pull requests of PRS, or nil.
They are as GitHub gives them to QUERY."
  (when-let* ((ref (test-branches--ref-named refs qualified-name)))
    (append ref
            `((associatedPullRequests
               . ((nodes . ,(vconcat (test-branches--associated-prs
                                      prs (alist-get 'name ref) query)))))))))

(defun test-branches--named-page (refs prs request)
  "The branches of REFS that REQUEST names as $b0, $b1 and so on.
They come with their pull requests of PRS that REQUEST asks for.  A name
that is not among REFS gives null, as GitHub does."
  `((data . ((repository
              . ((defaultBranchRef . ((name . "main")))
                 ,@(mapcar (lambda (variable)
                             (cons (car variable)
                                   (test-branches--named-ref refs prs (alist-get 'query request)
                                                             (cdr variable))))
                           (test-branches--branch-variables
                            (alist-get 'variables request)))))))))

(defun test-branches--names-asked ()
  "The names that the queries of branches by name asked for."
  (cl-loop for (_method address) in shipit-test-http-requests
           for sent in shipit-test-http-bodies
           for request = (and (string-match-p "/graphql\\'" address)
                              (json-read-from-string sent))
           when (and request (test-branches--named-query-p request))
           append (mapcar #'cdr (test-branches--branch-variables
                                 (cdr (assq 'variables request))))))

(defun test-branches--matching-refs (refs)
  "REFS as GitHub lists the refs whose name starts with refs/heads/."
  (vconcat (mapcar (lambda (ref)
                     `((ref . ,(concat "refs/heads/" (cdr (assq 'name ref))))
                       (object . ((sha . ,(cdr (assq 'oid (cdr (assq 'target ref)))))
                                  (type . "commit")))))
                   refs)))

(defun test-branches--etag (refs)
  "The ETag that GitHub gives with REFS; it changes when they change."
  (format "W/\"%s\"" (md5 (prin1-to-string refs))))

(defun test-branches--names-answer (refs)
  "GitHub's answer to the names of the branches that REFS gives, with an ETag.
REFS is a function of no arguments, so that the branches can change."
  (list "/repos/example/tool/git/matching-refs/heads/\\'" 200
        (lambda (_sent) (test-branches--matching-refs (funcall refs)))
        (lambda () `(("ETag" . ,(test-branches--etag (funcall refs)))))))

(defconst test-branches--refusal
  '((data . ((repository))) (errors . [((message . "Something went wrong"))]))
  "What GitHub answers to a query that it refuses.")

(cl-defun test-branches--graphql (refs &key (size 100) (prs (test-branches--prs))
                                       opaque endless refuse-at refuse-prs refuse-named)
  "The answer of GitHub to the queries of branches and of pull requests.
REFS are the branches of the repository, in pages of SIZE.  PRS are its
pull requests.  OPAQUE and ENDLESS are as for `test-branches--refs-page'.
The page of branches that starts at REFUSE-AT is refused, with
REFUSE-PRS the queries of pull requests are, and with REFUSE-NAMED
those of branches by name."
  (lambda (sent)
    (let* ((request (json-read-from-string sent))
           (variables (cdr (assq 'variables request))))
      ;; A query of branches by name asks for their pull requests too.
      (cond ((test-branches--named-query-p request)
             (if refuse-named
                 test-branches--refusal
               (test-branches--named-page refs prs request)))
            ((test-branches--prs-query-p request)
             (if refuse-prs
                 test-branches--refusal
               (test-branches--prs-page prs (cdr (assq 'states variables)))))
            (t
             (let ((offset (test-branches--offset (cdr (assq 'cursor variables)))))
               (if (eql offset refuse-at)
                   test-branches--refusal
                 (test-branches--refs-page refs size offset opaque endless))))))))

(defconst test-branches--user '("/user\\'" 200 ((login . "the-reader")))
  "The answer of GitHub to who the user is.")

(defun test-branches--answers (refs &rest keys)
  "The answers of GitHub: who the user is, and the branches REFS.
KEYS are those of `test-branches--graphql'."
  (list test-branches--user
        (test-branches--names-answer (lambda () refs))
        (list "/graphql\\'" 200 (apply #'test-branches--graphql refs keys))))

(defun test-branches--refs ()
  "The branches of a repository: the default one, one of the user, one
of another, and an old one."
  (list (test-branches--ref "main" "alice" 1 "Release the thing")
        (test-branches--ref "feature/login" "the-reader" 2 "Log in with a key")
        (test-branches--ref "fix/typo" "bob" 3 "Fix a typo")
        (test-branches--ref "old/idea" "carol" 200 "Try an idea")))

(defun test-branches--prs ()
  "The pull requests of the repository of `test-branches--refs': a draft
of the branch of the user, and a merged one of the branch of another."
  (list (test-branches--pr 12 "OPEN" "feature/login" t)
        (test-branches--pr 9 "MERGED" "fix/typo")))

(defun test-branches--cursors-asked ()
  "The cursors that the queries of all branches asked for, nil for the first page."
  (cl-loop for (_method address) in shipit-test-http-requests
           for sent in shipit-test-http-bodies
           for request = (and (string-match-p "/graphql\\'" address)
                              (json-read-from-string sent))
           when (and request
                     (not (test-branches--prs-query-p request))
                     (not (test-branches--named-query-p request)))
           collect (cdr (assq 'cursor (cdr (assq 'variables request))))))

(defmacro test-branches--fresh (answers &rest body)
  "Run BODY with nothing seen or notified yet, and GitHub giving ANSWERS.
example/tool is the one repository that is watched.  No notification
is kept, and what the watch knows is kept in no file."
  (declare (indent 1))
  `(let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
         (shipit--last-notification-count 0)
         (shipit--notification-count 0)
         (shipit--mention-count 0)
         (shipit--mention-prs nil)
         (shipit--modeline-string nil)
         (shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
         (shipit-github-token "token")
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-pr-github--login nil)
         (shipit-issue-repo-backends nil)
         (shipit-branches-watched-repos '("example/tool"))
         (shipit-branches-watch-file nil)
         (shipit-branches--state nil)
         (shipit-branches--etags nil))
     (shipit-test-http-answering ,answers
       ,@body)))

(defun test-branches--fetch (repo)
  "The branches of REPO, fetched without waiting; the answer comes at once."
  (let ((given 'nothing))
    (shipit-branches-fetch-async repo (lambda (branches) (setq given branches)))
    (should-not (eq given 'nothing))
    given))

(defun test-branches--names-fetched (etag)
  "What the backend gives for the names of the branches of example/tool.
ETAG is the one of the names that it gave before, or nil."
  (let ((given 'nothing))
    (shipit-branches--fetch-names-async
     "example/tool" etag (lambda (answer) (setq given answer)))
    (should-not (eq given 'nothing))
    given))

(defun test-branches--named-fetched (names)
  "What the backend gives for the branches of example/tool named NAMES."
  (let ((given 'nothing))
    (shipit-branches--fetch-named-async
     "example/tool" names (lambda (branches) (setq given branches)))
    (should-not (eq given 'nothing))
    given))

(defun test-branches--told (branches)
  "BRANCHES as lists of name, author, own, default, pull request and state."
  (mapcar (lambda (branch)
            (let ((pr (cdr (assq 'pr branch))))
              (list (cdr (assq 'name branch))
                    (cdr (assq 'author branch))
                    (cdr (assq 'own branch))
                    (cdr (assq 'default branch))
                    (cdr (assq 'number pr))
                    (cdr (assq 'state pr)))))
          branches))

(defun test-branches--names (branches)
  "The names of BRANCHES."
  (mapcar (lambda (branch) (cdr (assq 'name branch))) branches))

(defun test-branches--notifications ()
  "The notifications that shipit holds, each as its source and its title."
  (mapcar (lambda (activity)
            (list (cdr (assq 'repo activity)) (cdr (assq 'subject activity))))
          (hash-table-values shipit--notification-pr-activities)))

;;; The branches of a backend

(ert-deftest test-branches-github-gives-each-branch-with-its-last-commit ()
  "GIVEN GitHub with a default branch, a branch of the user with a draft
pull request, one of another with a merged one, and an old one
WHEN the branches of the repository are fetched
THEN each comes with who made its last commit, whether that is the
user, whether it is the default branch, and its pull request."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (should (equal (test-branches--told (test-branches--fetch "example/tool"))
                   '(("main" "alice" nil t nil nil)
                     ("feature/login" "the-reader" t nil 12 "draft")
                     ("fix/typo" "bob" nil nil 9 "merged")
                     ("old/idea" "carol" nil nil nil nil))))))

(ert-deftest test-branches-a-pull-request-of-another-repository-is-left-out ()
  "GIVEN a pull request whose head is a branch main of a fork
WHEN the branches are fetched
THEN the default branch main has no pull request."
  (test-branches--fresh
      (test-branches--answers
       (list (test-branches--ref "main" "alice" 1 "Release"))
       :prs (list (test-branches--pr 1 "OPEN" "main" nil t)))
    (should (equal (test-branches--told (test-branches--fetch "example/tool"))
                   '(("main" "alice" nil t nil nil))))))

(ert-deftest test-branches-an-open-pull-request-comes-before-the-others ()
  "GIVEN a branch with a merged pull request and an open one, the merged
one first
WHEN the branches are fetched
THEN the branch has the open one
GIVEN a branch with two merged pull requests
THEN it has the newer one."
  (test-branches--fresh
      (test-branches--answers
       (list (test-branches--ref "fix/typo" "bob" 1 "Fix")
             (test-branches--ref "fix/more" "bob" 1 "Fix more"))
       :prs (list (test-branches--pr 4 "MERGED" "fix/typo")
                  (test-branches--pr 7 "OPEN" "fix/typo")
                  (test-branches--pr 8 "MERGED" "fix/more")
                  (test-branches--pr 3 "MERGED" "fix/more")))
    (should (equal (mapcar (lambda (told) (nth 4 told))
                           (test-branches--told (test-branches--fetch "example/tool")))
                   '(7 8)))))

(ert-deftest test-branches-a-branch-knows-its-commit-and-its-address ()
  "GIVEN GitHub with a branch fix/typo
WHEN the branches are fetched
THEN that branch has the subject and the time of its last commit, AND
the address of its page."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (let ((branch (cl-find "fix/typo" (test-branches--fetch "example/tool")
                           :key (lambda (branch) (cdr (assq 'name branch)))
                           :test #'equal)))
      (should (equal (cdr (assq 'subject branch)) "Fix a typo"))
      (should (equal (cdr (assq 'time branch)) (test-branches--days-ago 3)))
      (should (equal (cdr (assq 'url branch))
                     "https://github.com/example/tool/tree/fix/typo")))))

(ert-deftest test-branches-asks-for-the-branches-of-the-repository ()
  "GIVEN GitHub
WHEN the branches of example/tool are fetched
THEN each query of GitHub is about example/tool."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (test-branches--fetch "example/tool")
    (dolist (sent (remq nil shipit-test-http-bodies))
      (let ((variables (cdr (assq 'variables (json-read-from-string sent)))))
        (should (equal (cdr (assq 'owner variables)) "example"))
        (should (equal (cdr (assq 'name variables)) "tool"))))))

(ert-deftest test-branches-reads-every-page ()
  "GIVEN GitHub with the branches on two pages
WHEN the branches are fetched
THEN the branches of both pages come."
  (test-branches--fresh (test-branches--answers (test-branches--refs) :size 2)
    (should (equal (test-branches--names (test-branches--fetch "example/tool"))
                   '("main" "feature/login" "fix/typo" "old/idea")))))

(ert-deftest test-branches-the-count-of-branches-says-which-pages-to-ask-for ()
  "GIVEN GitHub with five branches in pages of two, whose cursor is the
count of the branches before it, and whose pages after the first say
that another page follows, also when none does
WHEN the branches are fetched
THEN the pages after the first are asked for by their count, and no
other, AND all five branches come."
  (test-branches--fresh
      (test-branches--answers
       (cons (test-branches--ref "fix/more" "bob" 4 "More") (test-branches--refs))
       :size 2 :endless t)
    (should (= 5 (length (test-branches--fetch "example/tool"))))
    (should (equal (test-branches--cursors-asked)
                   (list nil (base64-encode-string "2") (base64-encode-string "4"))))))

(ert-deftest test-branches-pages-without-a-count-are-read-one-after-the-other ()
  "GIVEN GitHub with the branches in pages of two, whose cursor is not a
count of branches
WHEN the branches are fetched
THEN each page is asked for with the cursor that the page before gave,
AND all branches come."
  (test-branches--fresh (test-branches--answers (test-branches--refs) :size 2 :opaque t)
    (should (equal (test-branches--names (test-branches--fetch "example/tool"))
                   '("main" "feature/login" "fix/typo" "old/idea")))
    (should (equal (test-branches--cursors-asked) '(nil "after-2")))))

(ert-deftest test-branches-a-refused-later-page-gives-no-branches ()
  "GIVEN GitHub that refuses the second page of branches
WHEN the branches are fetched
THEN there are none: the branches of the other pages are not all."
  (test-branches--fresh (test-branches--answers (test-branches--refs) :size 2 :refuse-at 2)
    (should-not (test-branches--fetch "example/tool"))))

(ert-deftest test-branches-refused-pull-requests-leave-the-branches ()
  "GIVEN GitHub that gives the branches and refuses the pull requests
WHEN the branches are fetched
THEN all branches come, without a pull request."
  (test-branches--fresh (test-branches--answers (test-branches--refs) :refuse-prs t)
    (should (equal (mapcar (lambda (told) (nth 4 told))
                           (test-branches--told (test-branches--fetch "example/tool")))
                   '(nil nil nil nil)))))

(ert-deftest test-branches-github-asks-for-a-few-pages-at-a-time ()
  "GIVEN five tasks that wait for their answer, run two at a time
THEN the first two start
WHEN the second gives its result
THEN the third starts
WHEN the others give theirs, in another order
THEN the results come once, after the last, in the order of the tasks."
  (let ((started nil)
        (waiting nil)
        (given nil))
    (shipit-pr-github--run-limited
     (mapcar (lambda (n)
               (lambda (done) (push n started) (push (cons n done) waiting)))
             '(1 2 3 4 5))
     2
     (lambda (results) (push results given)))
    (should (equal (reverse started) '(1 2)))
    (funcall (alist-get 2 waiting) "b")
    (should (equal (reverse started) '(1 2 3)))
    (funcall (alist-get 1 waiting) "a")
    (funcall (alist-get 3 waiting) "c")
    (funcall (alist-get 5 waiting) "e")
    (should-not given)
    (funcall (alist-get 4 waiting) "d")
    (should (equal given '(("a" "b" "c" "d" "e"))))))

(ert-deftest test-branches-the-newest-last-commit-comes-first ()
  "GIVEN GitHub with an old branch on the first page, and newer ones on
the second
WHEN the branches are fetched
THEN the branch with the newest last commit comes first, whatever the
order of GitHub."
  (test-branches--fresh
      (test-branches--answers (list (test-branches--ref "old/idea" "carol" 200 "Try")
                                    (test-branches--ref "fix/typo" "bob" 3 "Fix")
                                    (test-branches--ref "main" "alice" 1 "Release"))
                              :size 1)
    (should (equal (test-branches--names (test-branches--fetch "example/tool"))
                   '("main" "fix/typo" "old/idea")))))

(ert-deftest test-branches-a-refusal-of-github-gives-no-branches ()
  "GIVEN GitHub that refuses the query
WHEN the branches are fetched
THEN there are none."
  (test-branches--fresh (list test-branches--user '("/graphql\\'" 502 ""))
    (should-not (test-branches--fetch "example/tool"))))

(ert-deftest test-branches-a-repository-github-does-not-know-gives-no-branches ()
  "GIVEN GitHub that knows no repository of the name
WHEN the branches are fetched
THEN there are none."
  (test-branches--fresh
      (list test-branches--user
            '("/graphql\\'" 200 ((data . ((repository)))
                                 (errors . [((message . "Could not resolve"))]))))
    (should-not (test-branches--fetch "example/tool"))))

(ert-deftest test-branches-github-gives-the-names-of-the-branches-with-an-etag ()
  "GIVEN GitHub with four branches
WHEN their names are fetched without an ETag
THEN the four names come, without refs/heads/, with the ETag of GitHub
WHEN they are fetched again with that ETag
THEN GitHub is asked with it, AND the answer is that they did not change."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (let ((answer (test-branches--names-fetched nil)))
      (should (equal (cdr answer) '("main" "feature/login" "fix/typo" "old/idea")))
      (should (equal (car answer) (test-branches--etag (test-branches--refs))))
      (should (eq (test-branches--names-fetched (car answer)) 'unchanged))
      (should (equal (cdr (assoc "If-None-Match" (car (last shipit-test-http-headers))))
                     (car answer))))))

(ert-deftest test-branches-github-refusing-the-names-gives-nothing ()
  "GIVEN GitHub that refuses the names of the branches
WHEN they are fetched
THEN nothing comes."
  (test-branches--fresh
      (list '("/repos/example/tool/git/matching-refs/heads/\\'" 502 ""))
    (should-not (test-branches--names-fetched nil))))

(ert-deftest test-branches-github-ending-after-the-headers-gives-nothing ()
  "GIVEN GitHub whose answer of the names of the branches ends after its
headers, as url.el gives it when the connection closes there
WHEN they are fetched
THEN nothing comes."
  (test-branches--fresh
      (list '("/repos/example/tool/git/matching-refs/heads/\\'" 200 ""
              (("ETag" . "W/\"cut\""))))
    (should-not (test-branches--names-fetched nil))))

(ert-deftest test-branches-github-gives-the-branches-of-some-names ()
  "GIVEN GitHub with four branches
WHEN the branches named fix/typo and gone/away are fetched
THEN GitHub is asked for those two names only, AND fix/typo comes with
who made its last commit and what it says, AND gone/away, which is not
there, is left out."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (let ((branches (test-branches--named-fetched '("fix/typo" "gone/away"))))
      (should (equal (test-branches--names-asked)
                     '("refs/heads/fix/typo" "refs/heads/gone/away")))
      (should (equal (test-branches--told branches) '(("fix/typo" "bob" nil nil nil nil))))
      (should (equal (cdr (assq 'subject (car branches))) "Fix a typo")))))

(ert-deftest test-branches-github-gives-the-open-pull-request-of-a-named-branch ()
  "GIVEN GitHub with a branch with a draft pull request, and one with a
merged one and an open one from a fork whose branch has the same name
WHEN the branches of those names are fetched
THEN the first comes with its draft, AND the second with no pull
request, since a merged one is not open and the fork is another
repository."
  (test-branches--fresh
      (test-branches--answers (test-branches--refs)
                              :prs (cons (test-branches--pr 30 "OPEN" "fix/typo" nil t)
                                         (test-branches--prs)))
    (should (equal (test-branches--told
                    (test-branches--named-fetched '("feature/login" "fix/typo")))
                   '(("feature/login" "the-reader" t nil 12 "draft")
                     ("fix/typo" "bob" nil nil nil nil))))))

(ert-deftest test-branches-github-refusing-some-names-gives-nothing ()
  "GIVEN GitHub that refuses the query of branches by name
WHEN the branch named fix/typo is fetched
THEN nothing comes."
  (test-branches--fresh (test-branches--answers (test-branches--refs) :refuse-named t)
    (should-not (test-branches--named-fetched '("fix/typo")))))

;;; The groups of the branches

(ert-deftest test-branches-are-grouped-as-github-does ()
  "GIVEN the default branch, a branch of the user, a branch of another
from three days ago, and one from 200 days ago
WHEN they are grouped
THEN the default branch, those of the user, the active ones and the
stale ones each stand in a group of their own, in that order."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    (should (equal (mapcar (lambda (group)
                             (cons (car group) (test-branches--names (cdr group))))
                           (shipit-branches-groups (test-branches--fetch "example/tool")))
                   '((default "main")
                     (yours "feature/login")
                     (active "fix/typo")
                     (stale "old/idea"))))))

(ert-deftest test-branches-an-empty-group-is-left-out ()
  "GIVEN only a default branch and a branch of another
WHEN they are grouped
THEN there are only the default group and the active one."
  (test-branches--fresh
      (test-branches--answers (list (test-branches--ref "main" "alice" 1 "Release")
                                    (test-branches--ref "fix/typo" "bob" 3 "Fix")))
    (should (equal (mapcar #'car (shipit-branches-groups
                                  (test-branches--fetch "example/tool")))
                   '(default active)))))

;;; The notification of a new branch

(defmacro test-branches--with-branches (refs prs &rest body)
  "Run BODY fresh, with GitHub giving the branches REFS and pull requests PRS.
REFS and PRS are read at each request, so a change to a variable there
changes what GitHub gives from then on."
  (declare (indent 2))
  `(test-branches--fresh
       (list test-branches--user
             (test-branches--names-answer (lambda () ,refs))
             (list "/graphql\\'" 200
                   (lambda (sent) (funcall (test-branches--graphql ,refs :prs ,prs) sent))))
     ,@body))

(defmacro test-branches--with-refs (refs &rest body)
  "Run BODY as `test-branches--with-branches' does, with the pull
requests of `test-branches--prs'."
  (declare (indent 1))
  `(test-branches--with-branches ,refs (test-branches--prs) ,@body))

(defun test-branches--poll ()
  "Poll the notification source of branches, as each poll of shipit does."
  (dolist (entry (shipit--autodiscover-notification-backends))
    (when (eq (plist-get (cdr entry) :backend) 'branch)
      (shipit--poll-one-backend entry nil))))

(defun test-branches--new-session ()
  "Start again as another session of Emacs: it knows only the file."
  (setq shipit-branches--state nil
        shipit-branches--etags nil
        shipit--notification-pr-activities (make-hash-table :test 'equal)
        shipit--locally-marked-read-notifications (make-hash-table :test 'equal)))

(defun test-branches--push (refs name login)
  "REFS with a new branch NAME, whose last commit LOGIN made now."
  (cons (test-branches--ref name login 0 (format "Work on %s" name)) refs))

(ert-deftest test-branches-the-first-poll-learns-the-branches ()
  "GIVEN a watched repository whose branches were never seen
WHEN shipit polls for notifications
THEN there is no notification
WHEN shipit polls again and the branches are the same
THEN there is still none."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (should-not (test-branches--notifications))
      (test-branches--poll)
      (should-not (test-branches--notifications)))))

(ert-deftest test-branches-a-new-branch-of-another-is-a-notification ()
  "GIVEN a watched repository whose branches were seen
WHEN bob pushes a new branch, and shipit polls for notifications
THEN there is one notification, from the repository, that names the
branch, AND it is the notification of a branch, AND it tells who made
its last commit and what it says
WHEN shipit polls again
THEN it is still that one notification."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      (should (equal (test-branches--notifications)
                     '(("example/tool" "New branch feature/search"))))
      (let ((activity (car (hash-table-values shipit--notification-pr-activities))))
        (should (equal (cdr (assq 'type activity)) "branch"))
        (should (equal (cdr (assq 'description activity))
                       "bob: Work on feature/search")))
      (test-branches--poll)
      (should (= 1 (hash-table-count shipit--notification-pr-activities))))))

(ert-deftest test-branches-a-poll-asks-for-the-commits-of-new-branches-only ()
  "GIVEN a watched repository whose branches were seen
WHEN bob pushes a new branch, and shipit polls for notifications
THEN GitHub is asked for the commit of that branch only, AND not for
all branches with their commits."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      (should (equal (test-branches--names-asked) '("refs/heads/feature/search")))
      (should-not (test-branches--cursors-asked)))))

(ert-deftest test-branches-an-unchanged-repository-is-asked-with-its-etag ()
  "GIVEN a watched repository whose branches were seen
WHEN shipit polls again and no branch changed
THEN GitHub is asked for the names with the ETag that came with them,
AND for nothing else."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (let ((asked (length shipit-test-http-requests)))
        (test-branches--poll)
        (should (= (1+ asked) (length shipit-test-http-requests))))
      (should (equal (cdr (assoc "If-None-Match" (car (last shipit-test-http-headers))))
                     (test-branches--etag refs)))
      (should-not (seq-find (lambda (request) (string-match-p "/graphql\\'" (cadr request)))
                            shipit-test-http-requests)))))

(ert-deftest test-branches-a-new-branch-of-the-user-is-no-notification ()
  "GIVEN a watched repository whose branches were seen
WHEN the user pushes a new branch, and shipit polls for notifications
THEN there is no notification."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/mine" "the-reader"))
      (test-branches--poll)
      (should-not (test-branches--notifications)))))

(ert-deftest test-branches-a-new-branch-with-an-open-pull-request-is-no-notification ()
  "GIVEN a watched repository whose branches were seen
WHEN bob pushes a new branch and opens a pull request of it, and shipit
polls for notifications
THEN there is no notification, since the pull request gives one."
  (let ((refs (test-branches--refs))
        (prs (test-branches--prs)))
    (test-branches--with-branches refs prs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob")
            prs (cons (test-branches--pr 20 "OPEN" "feature/search") prs))
      (test-branches--poll)
      (should-not (test-branches--notifications)))))

(ert-deftest test-branches-a-pull-request-takes-the-place-of-the-notification ()
  "GIVEN the notification of a new branch, kept in a file
WHEN bob opens a draft pull request of that branch, and shipit polls
again
THEN GitHub is asked for that branch again, AND the notification is
gone
WHEN a new session reads the file
THEN the notification does not come back."
  (let ((file (make-temp-file "shipit-branches-watch-"))
        (refs (test-branches--refs))
        (prs (test-branches--prs)))
    (unwind-protect
        (test-branches--with-branches refs prs
          (let ((shipit-branches-watch-file file))
            (test-branches--poll)
            (setq refs (test-branches--push refs "feature/search" "bob"))
            (test-branches--poll)
            (should (test-branches--notifications))
            (setq prs (cons (test-branches--pr 20 "OPEN" "feature/search" t) prs))
            (test-branches--poll)
            (should (equal (test-branches--names-asked)
                           '("refs/heads/feature/search" "refs/heads/feature/search")))
            (should-not (test-branches--notifications))
            (test-branches--new-session)
            (let ((shipit-branches-watched-repos nil))
              (test-branches--poll))
            (should-not (test-branches--notifications))))
      (delete-file file))))

(ert-deftest test-branches-a-branch-pushed-again-is-new-again ()
  "GIVEN a watched repository whose branches were seen
WHEN a branch of bob with a merged pull request goes away, shipit
polls, and bob pushes it again
THEN the next poll tells of it as a new branch, since its pull request
is not open."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (seq-remove (lambda (ref) (equal (cdr (assq 'name ref)) "fix/typo"))
                             refs))
      (test-branches--poll)
      (setq refs (test-branches--refs))
      (test-branches--poll)
      (should (equal (test-branches--notifications)
                     '(("example/tool" "New branch fix/typo")))))))

(ert-deftest test-branches-a-refused-poll-forgets-nothing ()
  "GIVEN a watched repository whose branches were seen
WHEN GitHub refuses the names at one poll, bob pushes a new branch, and
shipit polls again
THEN there is one notification, of that new branch."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (shipit-test-http-answering
          '(("/repos/example/tool/git/matching-refs/heads/\\'" 502 ""))
        (test-branches--poll))
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      (should (equal (test-branches--notifications)
                     '(("example/tool" "New branch feature/search")))))))

(ert-deftest test-branches-an-answer-without-names-forgets-nothing ()
  "GIVEN a watched repository whose branches were seen
WHEN the answer of the names ends after its headers at one poll, bob
pushes a new branch, and shipit polls again
THEN there is one notification, of that new branch."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (shipit-test-http-answering
          '(("/repos/example/tool/git/matching-refs/heads/\\'" 200 ""
             (("ETag" . "W/\"cut\""))))
        (test-branches--poll))
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      (should (equal (test-branches--notifications)
                     '(("example/tool" "New branch feature/search")))))))

(ert-deftest test-branches-a-refused-query-of-new-branches-tells-of-them-later ()
  "GIVEN a watched repository whose branches were seen
WHEN bob pushes a new branch, and GitHub refuses the query of its
commit at the next poll
THEN there is no notification
WHEN GitHub answers at the poll after that
THEN there is the notification of that branch."
  (let ((refs (test-branches--refs)))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (shipit-test-http-answering
          (list test-branches--user
                (test-branches--names-answer (lambda () refs))
                (list "/graphql\\'" 200 (test-branches--graphql refs :refuse-named t)))
        (test-branches--poll))
      (should-not (test-branches--notifications))
      (test-branches--poll)
      (should (equal (test-branches--notifications)
                     '(("example/tool" "New branch feature/search")))))))

(ert-deftest test-branches-without-a-watched-repository-nothing-is-asked ()
  "GIVEN no watched repository, and no notification of a branch to read
WHEN shipit polls for notifications
THEN the branches are no source of its notifications, AND GitHub is
asked nothing."
  (test-branches--fresh nil
    (let ((shipit-branches-watched-repos nil))
      (should-not (seq-find (lambda (entry) (eq (plist-get (cdr entry) :backend) 'branch))
                            (shipit--autodiscover-notification-backends)))
      (test-branches--poll)
      (should-not shipit-test-http-requests))))

(ert-deftest test-branches-what-the-watch-knows-is-kept-in-a-file ()
  "GIVEN a file for what the watch knows, a poll that learned the
branches, and a poll that found a new branch of bob
WHEN another session of Emacs polls, and the branches are the same
THEN the notification of that branch is there again, AND no other
WHEN it is marked as read, and a third session polls
THEN there is no notification."
  (let ((file (make-temp-file "shipit-branches-watch-"))
        (refs (test-branches--refs)))
    (unwind-protect
        (test-branches--with-refs refs
          (let ((shipit-branches-watch-file file))
            (test-branches--poll)
            (setq refs (test-branches--push refs "feature/search" "bob"))
            (test-branches--poll)
            (test-branches--new-session)
            (test-branches--poll)
            (should (equal (test-branches--notifications)
                           '(("example/tool" "New branch feature/search"))))
            (let ((activity (car (hash-table-values shipit--notification-pr-activities))))
              (shipit--mark-notification-read (cdr (assq 'number activity)) "example/tool"
                                              t "branch"))
            (test-branches--new-session)
            (test-branches--poll)
            (should-not (test-branches--notifications))))
      (delete-file file))))

(ert-deftest test-branches-a-notification-to-read-comes-without-a-watched-repository ()
  "GIVEN a file with the notification of a new branch that was not read,
and no watched repository any more
WHEN another session of Emacs polls
THEN the notification is there again, AND GitHub is asked nothing."
  (let ((file (make-temp-file "shipit-branches-watch-"))
        (refs (test-branches--refs)))
    (unwind-protect
        (test-branches--with-refs refs
          (let ((shipit-branches-watch-file file))
            (test-branches--poll)
            (setq refs (test-branches--push refs "feature/search" "bob"))
            (test-branches--poll)
            (test-branches--new-session)
            (let ((shipit-branches-watched-repos nil)
                  (asked (length shipit-test-http-requests)))
              (test-branches--poll)
              (should (= asked (length shipit-test-http-requests))))
            (should (equal (test-branches--notifications)
                           '(("example/tool" "New branch feature/search"))))))
      (delete-file file))))

(defmacro test-branches--opening (opened &rest body)
  "Run BODY with what shipit and the browser open recorded in OPENED.
A pull request is recorded as (pr NUMBER REPO), a branch as (branch REPO
BRANCH BASE), a repository as (repo REPO), and an address as (browser
URL)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'shipit-open-pr-buffer)
              (lambda (number repo &rest _) (push (list 'pr number repo) ,opened)))
             ((symbol-function 'shipit-commit-comments-open-branch)
              (lambda (repo branch &optional base) (push (list 'branch repo branch base) ,opened)))
             ((symbol-function 'shipit-open-repo-buffer)
              (lambda (repo &rest _) (push (list 'repo repo) ,opened)))
             ((symbol-function 'browse-url)
              (lambda (url &rest _) (push (list 'browser url) ,opened))))
     ,@body))

(defun test-branches--backends-without (operation)
  "The backends of pull requests, with GitHub without OPERATION."
  (cons (cons 'github (cl-loop for (key value) on (alist-get 'github shipit-pr-backends)
                               by #'cddr
                               unless (eq key operation)
                               nconc (list key value)))
        shipit-pr-backends))

(defun test-branches--open-new-branch (backends)
  "What the notification of a new branch opens, with BACKENDS.
The notification is gone after it is opened."
  (let ((refs (test-branches--refs))
        (opened nil))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      (let ((buffer (shipit-notifications-buffer-create))
            (shipit-pr-backends backends))
        (unwind-protect
            (test-branches--opening opened
              (with-current-buffer buffer
                (shipit-notifications-buffer--rerender)
                (goto-char (point-min))
                (should (re-search-forward "New branch feature/search" nil t))
                (shipit-notifications-buffer-open)))
          (kill-buffer buffer)))
      (should-not (test-branches--notifications)))
    opened))

(ert-deftest test-branches-the-notification-shows-the-branch ()
  "GIVEN the notification of a new branch in the notifications buffer
WHEN it is opened
THEN shipit shows how the branch differs from the default branch, AND
the notification is gone."
  (should (equal (test-branches--open-new-branch shipit-pr-backends)
                 '((branch "example/tool" "feature/search" "main")))))

(ert-deftest test-branches-the-notification-shows-the-web-page-when-shipit-cannot ()
  "GIVEN the notification of a new branch, and a backend that has no
comments of commits, so no view of a branch
WHEN the notification is opened
THEN the browser shows how the branch differs from the default branch,
AND the notification is gone."
  (should (equal (test-branches--open-new-branch
                  (test-branches--backends-without :fetch-commit-comments))
                 '((browser "https://github.com/example/tool/compare/main...feature/search")))))

(defun test-branches--notification-row ()
  "The row of the one notification, as the notifications buffer shows it."
  (let ((buffer (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buffer
          (shipit-notifications-buffer--rerender)
          (goto-char (point-min))
          (should (re-search-forward "New branch" nil t))
          (buffer-substring (line-beginning-position) (line-end-position)))
      (kill-buffer buffer))))

(ert-deftest test-branches-the-notification-has-the-icons-of-a-branch ()
  "GIVEN the notification of a new branch, and SVG icons
WHEN the notifications buffer shows it
THEN the octicon of a watch stands for where it comes from, AND that
of a branch for what it is
GIVEN no SVG icons
THEN the row has no text of an unknown kind."
  (let ((refs (test-branches--refs))
        (asked nil))
    (test-branches--with-refs refs
      (test-branches--poll)
      (setq refs (test-branches--push refs "feature/search" "bob"))
      (test-branches--poll)
      ;; `features' is not special, so only `dlet' binds it for `featurep'.
      ;; shipit keeps each icon that it made: with a table of its own, the
      ;; icons of this test are not what a later test finds there.
      (dlet ((shipit-use-svglib-icons t)
             (shipit--svg-lib-icon-cache (make-hash-table :test 'equal))
             (features (cons 'svg-lib features)))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'svg-lib-icon)
                   (lambda (name &rest _)
                     (push name asked)
                     '(image :type svg :data "<svg/>"))))
          (test-branches--notification-row)))
      (should (member "eye" asked))
      (should (member "git-branch" asked))
      (let ((shipit-use-svglib-icons nil))
        (should-not (string-search "??" (test-branches--notification-row)))))))

;;; Watching a repository

(ert-deftest test-branches-watching-is-saved-and-learns-the-branches ()
  "GIVEN a repository that is not watched, and its branches
WHEN it is watched
THEN it is among the watched repositories, AND the file of the
customizations keeps that, AND its branches count as seen
WHEN it is no longer watched
THEN it is not among them."
  (let* ((custom-file (make-temp-file "shipit-branches-custom-" nil ".el"))
         ;; Customize saves nothing in an Emacs that read no init file.
         (user-init-file custom-file))
    (unwind-protect
        (test-branches--fresh (test-branches--answers (test-branches--refs))
          (let ((shipit-branches-watched-repos nil)
                (branches (test-branches--fetch "example/tool")))
            (shipit-branches-toggle-watch "example/tool" branches)
            (should (shipit-branches-watched-p "example/tool"))
            (should (string-match-p "example/tool"
                                    (with-temp-buffer
                                      (insert-file-contents custom-file)
                                      (buffer-string))))
            (test-branches--poll)
            (should-not (test-branches--notifications))
            (shipit-branches-toggle-watch "example/tool" branches)
            (should-not (shipit-branches-watched-p "example/tool"))))
      (delete-file custom-file))))

(provide 'test-shipit-branches)
;;; test-shipit-branches.el ends here
