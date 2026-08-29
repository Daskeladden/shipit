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
                       (merged_at . nil) (head . ((ref . "feat/auth") (sha . "a1"))))
                      ((number . 102) (state . "open") (draft . nil)
                       (merged_at . nil) (head . ((ref . "feat/api") (sha . "b2"))))
                      ((number . 103) (state . "open") (draft . nil)
                       (merged_at . nil) (head . ((ref . "feat/ui") (sha . "c3"))))]))
  "The stack resource returned by GET /repos/{owner}/{repo}/stacks/{n}.")

(defconst test-stack--normalized
  '((number . 7) (size . 3) (position . 2) (base . "main") (open . t)
    (entries . (((number . 101) (state . "open") (draft . t) (merged-at . nil)
                 (head . "feat/auth") (current . nil))
                ((number . 102) (state . "open") (draft . nil) (merged-at . nil)
                 (head . "feat/api") (current . t))
                ((number . 103) (state . "open") (draft . nil) (merged-at . nil)
                 (head . "feat/ui") (current . nil)))))
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
branch, and the row for this pull request is marked."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
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
         (should (string-match-p "feat/auth" text))
         (should (string-match-p "this PR" text)))))))

(ert-deftest test-shipit-stack-section-elides-without-backend-support ()
  "GIVEN a backend that cannot report stacks
WHEN the stack section is inserted
THEN nothing is written at all, rather than an empty heading."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
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
    (setq inhibit-read-only t)
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
    (setq inhibit-read-only t)
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

(provide 'test-shipit-pr-stack)
;;; test-shipit-pr-stack.el ends here
