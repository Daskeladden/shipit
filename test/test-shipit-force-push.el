;;; test-shipit-force-push.el --- Force-push activity rows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; GitHub's own PR page shows a force push as "<user> force-pushed the
;; <branch> branch from <sha> to <newsha>", with a Compare button for
;; the diff between the two heads.
;;
;; The REST timeline names only the new head, in `commit_id'.  The old
;; head comes from GraphQL, which reports both, and is matched back
;; onto the REST event so the row keeps the event id that unread
;; tracking is built on.  HTTP is the only mocked boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-pr-github)
(require 'shipit-pr-sections)
(require 'shipit-pr-actions)

;;; Fixtures

(defconst test-sfp--before "aaaaaaa111122223333444455556666777788889")
(defconst test-sfp--after "bbbbbbb111122223333444455556666777788889")

(defun test-sfp--event (type &optional commit-id id)
  "Build a REST timeline event of TYPE, optionally naming COMMIT-ID and ID."
  `((event . ,type)
    (id . ,(or id 4242))
    (created_at . "2026-09-08T10:11:12Z")
    (actor . ((login . "carol") (avatar_url . "https://example.com/c.png")))
    ,@(when commit-id `((commit_id . ,commit-id)))))

(defun test-sfp--graphql-reply (nodes)
  "Wrap NODES in the shape `shipit--graphql-request' returns."
  `((data . ((repository . ((pullRequest . ((timelineItems . ((nodes . ,nodes))))))))))) 

(defun test-sfp--graphql-response (&rest pairs)
  "Build the GraphQL reply naming PAIRS of (BEFORE . AFTER) SHAs."
  (test-sfp--graphql-reply
   (mapcar (lambda (pair)
             `((createdAt . "2026-09-08T10:11:12Z")
               (beforeCommit . ((oid . ,(car pair))))
               (afterCommit . ((oid . ,(cdr pair))))))
           pairs)))

(defmacro test-sfp--with-graphql (response &rest body)
  "Run BODY with `shipit--graphql-request' answering RESPONSE.
The number of requests made is available in BODY as `graphql-calls'."
  (declare (indent 1))
  `(let ((graphql-calls 0))
     (ignore graphql-calls)
     (cl-letf (((symbol-function 'shipit--graphql-request)
                (lambda (&rest _)
                  (setq graphql-calls (1+ graphql-calls))
                  ,response)))
       ,@body)))

;;; Backend — pairing the old head onto the REST event

(ert-deftest test-sfp-registry-has-fetch-force-pushes ()
  "GIVEN the GitHub PR backend
WHEN its registered operations are read
THEN :fetch-force-pushes is one of them."
  (should (plist-get (cdr (assq 'github shipit-pr-backends))
                     :fetch-force-pushes)))

(ert-deftest test-sfp-fetch-force-pushes-pairs-new-head-to-old ()
  "GIVEN a PR whose GraphQL timeline holds one force push
WHEN the force pushes are fetched
THEN the new head maps to the head it replaced."
  (test-sfp--with-graphql (test-sfp--graphql-response
                           (cons test-sfp--before test-sfp--after))
    (should (equal (shipit-pr-github--fetch-force-pushes '(:repo "o/r") 7)
                   (list (cons test-sfp--after test-sfp--before))))))

(ert-deftest test-sfp-fetch-force-pushes-skips-missing-commits ()
  "GIVEN a force push whose old head GitHub can no longer resolve
WHEN the force pushes are fetched
THEN it is left out rather than paired with nothing."
  (test-sfp--with-graphql (test-sfp--graphql-reply
                           '(((createdAt . "2026-09-08T10:11:12Z")
                              (beforeCommit . nil)
                              (afterCommit . ((oid . "cccdddd"))))))
    (should (null (shipit-pr-github--fetch-force-pushes '(:repo "o/r") 7)))))

(ert-deftest test-sfp-annotate-adds-both-heads-to-the-event ()
  "GIVEN a REST force-push event naming only the new head
WHEN the timeline is annotated
THEN the event carries both heads and keeps its own id."
  (let ((events (list (test-sfp--event "labeled")
                      (test-sfp--event "head_ref_force_pushed" test-sfp--after 99))))
    (test-sfp--with-graphql (test-sfp--graphql-response
                             (cons test-sfp--before test-sfp--after))
      (let* ((annotated (shipit-pr-github--annotate-force-pushes
                         '(:repo "o/r") 7 events))
             (event (nth 1 annotated)))
        (should (= 1 graphql-calls))
        (should (equal (cdr (assq 'shipit-force-push-before event)) test-sfp--before))
        (should (equal (cdr (assq 'shipit-force-push-after event)) test-sfp--after))
        (should (equal (cdr (assq 'id event)) 99))))))

(ert-deftest test-sfp-annotate-skips-the-request-without-force-pushes ()
  "GIVEN a timeline with no force push in it
WHEN it is annotated
THEN no GraphQL request is made and the events come back untouched."
  (let ((events (list (test-sfp--event "labeled") (test-sfp--event "closed"))))
    (test-sfp--with-graphql (test-sfp--graphql-response)
      (should (equal (shipit-pr-github--annotate-force-pushes '(:repo "o/r") 7 events)
                     events))
      (should (= 0 graphql-calls)))))

;;; The activity row

(ert-deftest test-sfp-action-reads-like-the-web-page ()
  "GIVEN both heads of a force push and the branch it landed on
WHEN the activity line is built
THEN it names the branch and the two short SHAs."
  (should (equal (substring-no-properties
                  (shipit--force-push-action test-sfp--before test-sfp--after "fix/thing"))
                 "force-pushed the fix/thing branch from aaaaaaa to bbbbbbb")))

(ert-deftest test-sfp-action-without-the-heads-stays-plain ()
  "GIVEN a force push whose heads are unknown, as on a backend that
does not report them
WHEN the activity line is built
THEN it says only that a force push happened."
  (should (equal (substring-no-properties
                  (shipit--force-push-action nil nil "fix/thing"))
                 "force-pushed")))

;;; The compare body

(defmacro test-sfp--in-buffer (files &rest body)
  "Run BODY in a magit buffer with the local clone reporting FILES."
  (declare (indent 1))
  `(let ((buffer (get-buffer-create "*test-sfp*"))
         (diff-files ,files)
         (patch-reads 0)
         (shipit-pr-backend 'github)
         (shipit-show-avatars nil))
     (ignore patch-reads)
     (unwind-protect
         (with-current-buffer buffer
           (cl-letf (((symbol-function 'shipit--ensure-diff-revisions)
                      (lambda (revisions &optional _repo) revisions))
                     ((symbol-function 'shipit--range-files-from-git)
                      (lambda (_base _head) diff-files))
                     ((symbol-function 'shipit--file-patch-from-git)
                      (lambda (_base _head filename)
                        (setq patch-reads (1+ patch-reads))
                        (cdr (assoc filename test-sfp--patches)))))
             (let ((inhibit-read-only t))
               (magit-section-mode)
               (erase-buffer)
               ,@body)))
       (kill-buffer buffer))))

(defmacro test-sfp--with-compare (files &rest body)
  "Run BODY inside an open root section, the local clone reporting FILES."
  (declare (indent 1))
  `(test-sfp--in-buffer ,files
     (magit-insert-section (root)
       ,@body)))

(defconst test-sfp--compare-files
  '(((filename . "src/loader.c")
     (status . "modified")
     (additions . 1)
     (deletions . 1)))
  "One file differing between the two heads.")

(defconst test-sfp--patches
  '(("src/loader.c" . "@@ -1,2 +1,2 @@\n-old line\n+new line\n context"))
  "The patch the local clone reports for each file.")

(ert-deftest test-sfp-compare-is-the-direct-difference ()
  "GIVEN two heads of a force push
WHEN the comparison is gathered
THEN it is read as the direct difference between them.

The compare API only does a merge-base comparison, which reports
every commit one side holds and the other does not.  After a rebase
that is the whole branch, where GitHub's own Compare button shows
only what the push actually changed."
  (let ((asked nil)
        (compares 0))
    (cl-letf (((symbol-function 'shipit--ensure-diff-revisions)
               (lambda (revisions &optional _repo) revisions))
              ((symbol-function 'shipit--range-files-from-git)
               (lambda (base head) (setq asked (cons base head)) nil))
              ((symbol-function 'shipit-pr-github--fetch-compare)
               (lambda (&rest _) (setq compares (1+ compares)) nil)))
      (shipit--force-push-compare-files "o/r" test-sfp--before test-sfp--after))
    (should (equal asked (cons test-sfp--before test-sfp--after)))
    (should (= 0 compares))))

(ert-deftest test-sfp-compare-fetches-the-heads-it-needs ()
  "GIVEN a force push whose old head is not in the local clone
WHEN the comparison is gathered
THEN the heads are made ready first, since the difference is read
from the local clone."
  (let ((ensured nil))
    (cl-letf (((symbol-function 'shipit--ensure-diff-revisions)
               (lambda (revisions &optional repo)
                 (setq ensured (list revisions repo))
                 revisions))
              ((symbol-function 'shipit--range-files-from-git) (lambda (&rest _) nil)))
      (shipit--force-push-compare-files "o/r" test-sfp--before test-sfp--after))
    (should (equal ensured (list (cons test-sfp--before test-sfp--after) "o/r")))))

(ert-deftest test-sfp-compare-says-why-when-a-head-is-unreachable ()
  "GIVEN a head neither the local clone nor the remote can produce
WHEN the comparison is rendered
THEN the reason is written into the section, because signalling out of
a washer abandons the render and leaves the section tree without its
children."
  (test-sfp--with-compare test-sfp--compare-files
    (cl-letf (((symbol-function 'shipit--ensure-diff-revisions)
               (lambda (&rest _) (user-error "Could not fetch aaaaaaa"))))
      (shipit--insert-force-push-compare "o/r" test-sfp--before test-sfp--after))
    (should (string-match-p "Could not fetch aaaaaaa"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(defmacro test-sfp--with-rendered-compare (files &rest body)
  "Render the comparison of FILES, then run BODY with the tree closed.
A washer walks its section\='s ancestors, so a file can only be opened
once the sections above it are finished."
  (declare (indent 1))
  `(test-sfp--in-buffer ,files
     (magit-insert-section (root)
       (shipit--insert-force-push-compare "o/r" test-sfp--before test-sfp--after))
     ,@body))

(defun test-sfp--open-file-sections ()
  "Open every file section of the comparison in the current buffer."
  (dolist (section (oref magit-root-section children))
    (when (eq (oref section type) 'pr-file)
      (magit-section-show section))))

(ert-deftest test-sfp-compare-renders-the-changed-files ()
  "GIVEN a force push whose two heads differ in one file
WHEN the comparison is rendered and that file opened
THEN the file and its diff are in the buffer."
  (test-sfp--with-rendered-compare test-sfp--compare-files
    (should (string-match-p "src/loader\\.c"
                            (buffer-substring-no-properties (point-min) (point-max))))
    (test-sfp--open-file-sections)
    (should (string-match-p "\\+new line"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-sfp-compare-reads-no-diff-until-a-file-is-opened ()
  "GIVEN a force push, which after a rebase can move hundreds of files
WHEN the comparison is rendered
THEN no file's diff has been read, and opening one reads only that
one.  Reading and rendering them all costs far more than the reader
asked for, and made expanding such a row unusably slow."
  (test-sfp--with-rendered-compare test-sfp--compare-files
    (should (= 0 patch-reads))
    (test-sfp--open-file-sections)
    (should (= 1 patch-reads))))

(ert-deftest test-sfp-compare-without-changes-says-so ()
  "GIVEN a force push that changed no file, a rebase onto the same tree
WHEN the comparison is rendered
THEN the body says that rather than staying empty."
  (test-sfp--with-compare nil
    (shipit--insert-force-push-compare "o/r" test-sfp--before test-sfp--after)
    (should (string-match-p
             "No file changes"
             (buffer-substring-no-properties (point-min) (point-max))))))

(defun test-sfp--position-with (property value)
  "Return the first position whose PROPERTY equals VALUE.
`text-property-any\=' compares with `eq\=', which never matches a string."
  (let ((pos (point-min))
        (found nil))
    (while (and pos (< pos (point-max)) (not found))
      (if (equal (get-text-property pos property) value)
          (setq found pos)
        (setq pos (next-single-property-change pos property))))
    found))

(ert-deftest test-sfp-compare-files-carry-the-two-heads ()
  "GIVEN a rendered force-push comparison
WHEN a line of it is examined
THEN it names the two heads, so the diff commands at point compare
those and not the pull request's own base and head."
  (test-sfp--with-compare test-sfp--compare-files
    (shipit--insert-force-push-compare "o/r" test-sfp--before test-sfp--after)
    (let ((pos (test-sfp--position-with 'shipit-file-path "src/loader.c")))
      (should pos)
      (should (equal (get-text-property pos 'shipit-base-sha) test-sfp--before))
      (should (equal (get-text-property pos 'shipit-head-sha) test-sfp--after)))))

(ert-deftest test-sfp-compare-leaves-out-the-review-comments ()
  "GIVEN a PR whose review comments are cached for the same file
WHEN a force-push comparison is rendered and that file opened
THEN none of them is drawn into it, because their line numbers belong
to the pull request diff and not to this comparison."
  (test-sfp--in-buffer test-sfp--compare-files
    (setq-local shipit--inline-comments-fetched t)
    (setq-local shipit--cached-inline-comments
                '(((id . 5001)
                   (path . "src/loader.c")
                   (line . 1)
                   (side . "RIGHT")
                   (user . ((login . "dave")))
                   (body . "a review comment on the pull request diff")
                   (created_at . "2026-09-08T09:00:00Z"))))
    (magit-insert-section (root)
      (shipit--insert-force-push-compare "o/r" test-sfp--before test-sfp--after))
    (test-sfp--open-file-sections)
    (should-not (string-match-p
                 "a review comment on the pull request diff"
                 (buffer-substring-no-properties (point-min) (point-max))))))

;;; RET on the row
;; Driven through finished sections rather than stubs.  Until a row is
;; first expanded its comparison is still a washer and the section has
;; no content at all, and that difference is what went wrong.

(defun test-sfp--force-push-event (&optional annotated)
  "Build the REST force-push event, ANNOTATED with both heads when asked."
  (let ((event (test-sfp--event "head_ref_force_pushed" test-sfp--after)))
    (if annotated
        (append `((shipit-force-push-before . ,test-sfp--before)
                  (shipit-force-push-after . ,test-sfp--after))
                event)
      event)))

(defun test-sfp--render-row (event)
  "Insert EVENT as a force-push row and return its finished section."
  (setq-local shipit-buffer-pr-data '((head . ((ref . "fix/thing")))))
  (magit-insert-section (root)
    (shipit--insert-activity-event event "o/r" 7 nil))
  (car (last (oref magit-root-section children))))

(defmacro test-sfp--with-row (event &rest body)
  "Run BODY with point on the finished row for EVENT, the row bound to `row\='."
  (declare (indent 1))
  `(test-sfp--in-buffer test-sfp--compare-files
     (let ((row (test-sfp--render-row ,event)))
       (goto-char (oref row start))
       ,@body)))

(ert-deftest test-sfp-return-opens-the-comparison ()
  "GIVEN point on a force-push row that has never been expanded
WHEN the activity navigation runs
THEN the row opens, which is what GitHub's Compare button does."
  (test-sfp--with-row (test-sfp--force-push-event t)
    (should (oref row hidden))
    (shipit-pr--dispatch-activity-navigation '(:event-type "head_ref_force_pushed"))
    (should-not (oref row hidden))))

(ert-deftest test-sfp-return-closes-the-comparison-again ()
  "GIVEN a force-push row already showing its comparison
WHEN the activity navigation runs again
THEN the row closes, rather than reopening what is already open."
  (test-sfp--with-row (test-sfp--force-push-event t)
    (shipit-pr--dispatch-activity-navigation '(:event-type "head_ref_force_pushed"))
    (should-not (oref row hidden))
    (goto-char (oref row start))
    (shipit-pr--dispatch-activity-navigation '(:event-type "head_ref_force_pushed"))
    (should (oref row hidden))))

(ert-deftest test-sfp-return-without-a-comparison-says-so ()
  "GIVEN a force-push row whose two heads the backend never reported
WHEN the activity navigation runs
THEN it says there is nothing to compare and the row stays shut."
  (let ((messages nil))
    (test-sfp--with-row (test-sfp--force-push-event)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages) nil)))
        (shipit-pr--dispatch-activity-navigation
         '(:event-type "head_ref_force_pushed")))
      (should (oref row hidden))
      (should (string-match-p "No comparison" (car messages))))))

(provide 'test-shipit-force-push)
;;; test-shipit-force-push.el ends here
