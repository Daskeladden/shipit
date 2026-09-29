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
  (magit-map-sections (lambda (section)
                        (when (eq (oref section type) 'pr-file)
                          (magit-section-show section)))))

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

;;; The commits of a push, each with its files
;; A real repository for each case.  The old commits are paired with
;; the new ones by `git range-diff', and each commit opens to the files
;; it changes, so what a rebase brought in never shows.

(defun test-sfp--git (&rest args)
  "The trimmed output of git with ARGS, in `default-directory'."
  (string-trim (apply #'shipit--git-output args)))

(defun test-sfp--commit (file text message)
  "Write TEXT to FILE, commit it with MESSAGE, and return the commit."
  (with-temp-file (expand-file-name file) (insert text))
  (test-sfp--git "add" file)
  (test-sfp--git "commit" "-q" "-m" message)
  (test-sfp--git "rev-parse" "HEAD"))

(defun test-sfp--text ()
  "The text of the buffer, without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defmacro test-sfp--in-repo (&rest body)
  "Run BODY in a new, empty repository, deleted after."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "shipit-force-push-" t)))
          (default-directory dir)
          (shipit-show-avatars nil))
     (unwind-protect
         (progn
           (test-sfp--git "-c" "init.defaultBranch=main" "init" "-q")
           (test-sfp--git "config" "user.email" "test@example.invalid")
           (test-sfp--git "config" "user.name" "Test")
           ,@body)
       (delete-directory dir t))))

(defmacro test-sfp--rendering (base before after &rest body)
  "Run BODY on the comparison of BEFORE and AFTER, rendered in a new buffer.
The pull request has BASE as the commit of its base branch."
  (declare (indent 3))
  `(let ((dir default-directory))
     (with-temp-buffer
       (setq default-directory dir)
       (magit-section-mode)
       (setq-local shipit-buffer-pr-data `((base . ((ref . "main") (sha . ,,base)))))
       (let ((inhibit-read-only t))
         (magit-insert-section (root)
           (shipit--insert-force-push-compare "o/r" ,before ,after)))
       ,@body)))

(defconst test-sfp--branch-lines "two\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\neleven\n"
  "The lines the commit of the branch adds to a.txt.
Enough of them that `git range-diff' still pairs the two versions of
the commit when one of them and a line of context change.")

(defconst test-sfp--added-lines (concat "one\n" test-sfp--branch-lines)
  "What a.txt holds after the commit of the first push.")

(defun test-sfp--history (spec)
  "Build a pull request pushed twice; return its commits as a plist.
Main has a.txt and b.txt, and the one commit of the branch, \"Add
lines\", adds lines to a.txt.  SPEC is a plist.  `:main', a (FILE .
TEXT), is a change main makes, with a new c.txt, before the second push
is rebased onto it.  `:fixed' changes \"three\" to \"THREE\" in the
second push.  The plist returned has `:before', `:after' and `:base',
the commit main ends on."
  (test-sfp--commit "b.txt" "x\n" "b")
  (test-sfp--commit "a.txt" "one\n" "a")
  (test-sfp--git "checkout" "-q" "-b" "work")
  (let ((before (test-sfp--commit "a.txt" test-sfp--added-lines "Add lines"))
        (main (plist-get spec :main)))
    (test-sfp--git "checkout" "-q" "main")
    (when main
      (test-sfp--commit (car main) (cdr main) "main moves")
      (test-sfp--commit "c.txt" "new\n" "main adds"))
    (let ((base (test-sfp--git "rev-parse" "HEAD")))
      (test-sfp--git "checkout" "-q" "-b" "work2")
      (list :before before :base base
            :after (test-sfp--commit
                    "a.txt"
                    (concat (if (equal (car main) "a.txt") (cdr main) "one\n")
                            (if (plist-get spec :fixed)
                                (replace-regexp-in-string "three" "THREE" test-sfp--branch-lines)
                              test-sfp--branch-lines))
                    "Add lines")))))

(defmacro test-sfp--with-history (spec &rest body)
  "Run BODY on the rendered comparison of the history SPEC builds.
See `test-sfp--history'.  BODY sees the plist it returns as `history'."
  (declare (indent 1))
  `(test-sfp--in-repo
     (let ((history (test-sfp--history ,spec)))
       (test-sfp--rendering (plist-get history :base)
           (plist-get history :before) (plist-get history :after)
         ,@body))))

(defun test-sfp--commits-section ()
  "The section of the commits of the comparison, or nil."
  (seq-find (lambda (section) (eq 'force-push-commits (oref section type)))
            (oref magit-root-section children)))

(defun test-sfp--commit-section (subject)
  "The section of the commit of the comparison with SUBJECT."
  (seq-find (lambda (section)
              (string-match-p (regexp-quote subject)
                              (buffer-substring-no-properties
                               (oref section start) (oref section content))))
            (oref (test-sfp--commits-section) children)))

(defun test-sfp--opened-commit (subject)
  "Open the folded commit of the comparison with SUBJECT; return its text."
  (let ((commit (test-sfp--commit-section subject)))
    (should (oref commit hidden))
    (magit-section-show commit)
    (buffer-substring (oref commit start) (oref commit end))))

(defun test-sfp--opened-commit-files (subject)
  "Open the commit with SUBJECT and each of its files; return its text."
  (test-sfp--opened-commit subject)
  (let ((commit (test-sfp--commit-section subject)))
    (dolist (file (oref commit children))
      (magit-section-show file))
    (buffer-substring-no-properties (oref commit start) (oref commit end))))

(ert-deftest test-sfp-after-a-rebase-a-changed-commit-shows-only-its-change ()
  "GIVEN a branch rebased onto a main that changed b.txt and added c.txt,
and its commit fixed in the same push
WHEN the comparison is rendered, and the commit and its file opened
THEN it says the rebase is left out, and the commit shows only a.txt,
whose diff holds only the fix."
  (test-sfp--with-history '(:main ("b.txt" . "x\ny\n") :fixed t)
    (should (string-match-p "Only the author's changes" (test-sfp--text)))
    (should (string-match-p "Commits: 1 changed" (test-sfp--text)))
    (let ((text (test-sfp--opened-commit-files "Add lines")))
      (should (string-match-p "a\\.txt" text))
      (should-not (string-match-p "b\\.txt\\|c\\.txt" text))
      (should (string-match-p "^ *-three" text))
      (should (string-match-p "^ *\\+THREE" text)))))

(ert-deftest test-sfp-a-changed-commit-compares-with-its-old-change-on-the-new-parent ()
  "GIVEN a rebased and fixed commit
WHEN it is opened
THEN its file compares with the old commit applied onto the new
commit's parent, so the diff commands at point leave the rebase out."
  (test-sfp--with-history '(:main ("b.txt" . "x\ny\n") :fixed t)
    (test-sfp--opened-commit "Add lines")
    (let* ((pos (test-sfp--position-with 'shipit-file-path "a.txt"))
           (from (get-text-property pos 'shipit-base-sha)))
      (should (equal test-sfp--added-lines (shipit--git-output "show" (concat from ":a.txt"))))
      (should (equal "x\ny\n" (shipit--git-output "show" (concat from ":b.txt")))))))

(ert-deftest test-sfp-a-push-that-only-rebased-says-so ()
  "GIVEN a branch rebased onto a newer main and changed in nothing else,
as a bot that keeps branches up to date does
WHEN the comparison is rendered
THEN one line says the push only rebased, and its commit is unchanged."
  (test-sfp--with-history '(:main ("b.txt" . "x\ny\n"))
    (should (string-match-p "Only a rebase onto [0-9a-f]\\{7\\}: the push changed nothing else"
                            (test-sfp--text)))
    (should (string-match-p "Commits: 1 unchanged" (test-sfp--text)))
    (should-not (string-match-p "No file changes\\|Only the author" (test-sfp--text)))))

(ert-deftest test-sfp-without-a-rebase-no-rebase-is-mentioned ()
  "GIVEN a push that fixed the commit on the same main
WHEN the comparison is rendered, and the commit and its file opened
THEN nothing speaks of a rebase, and the commit shows the fix."
  (test-sfp--with-history '(:fixed t)
    (should-not (string-match-p "rebase" (test-sfp--text)))
    (should (string-match-p "^ *\\+THREE" (test-sfp--opened-commit-files "Add lines")))))

(ert-deftest test-sfp-a-conflict-shows-how-the-author-resolved-it ()
  "GIVEN a branch rebased onto a main that added a line where the commit
adds its lines, so the old commit conflicts, resolved by the author
WHEN the comparison is rendered, and the commit and its file opened
THEN the commit says it conflicted, and the diff of a.txt goes from
git's conflict markers to the author's resolution."
  (test-sfp--with-history '(:main ("a.txt" . "one\nzero\n") :fixed t)
    (let ((text (test-sfp--opened-commit-files "Add lines")))
      (should (string-match-p "conflict" text))
      (should (string-match-p "^ *-<<<<<<<" text)))))

(ert-deftest test-sfp-a-commit-already-on-the-base-is-on-base ()
  "GIVEN a branch of two commits, where the second changes the line the
first added, and a main that took the first commit as a commit of its
own; the branch is then rebased onto main, which drops that commit
WHEN the comparison is rendered
THEN the first commit counts as on the base, not as dropped, and the
push only rebased."
  (test-sfp--in-repo
    (test-sfp--commit "a.txt" "one\n" "a")
    (test-sfp--git "checkout" "-q" "-b" "work")
    (let* ((first (test-sfp--commit "a.txt" "one\ntwo\n" "two"))
           (before (test-sfp--commit "a.txt" "one\nTWO\n" "TWO")))
      (test-sfp--git "checkout" "-q" "main")
      ;; A commit of main's own first, so the copy is a new commit and
      ;; not the same one again.
      (test-sfp--commit "b.txt" "x\n" "main moves")
      (test-sfp--git "cherry-pick" first)
      (let ((base (test-sfp--commit "c.txt" "new\n" "main adds")))
        (test-sfp--git "checkout" "-q" "-b" "work2")
        (let ((after (test-sfp--commit "a.txt" "one\nTWO\n" "TWO")))
          (test-sfp--rendering base before after
            (should (string-match-p "Only a rebase onto" (test-sfp--text)))
            (should (string-match-p "Commits: 1 on base, 1 unchanged" (test-sfp--text)))
            (should-not (string-match-p "dropped\\|conflict" (test-sfp--text)))))))))

(defmacro test-sfp--with-reword (&rest body)
  "Run BODY on the rendered comparison of a push that only reworded a commit."
  (declare (indent 0))
  `(test-sfp--in-repo
     (let ((base (test-sfp--commit "a.txt" "one\n" "a")))
       (test-sfp--git "checkout" "-q" "-b" "work")
       (let ((before (test-sfp--commit "a.txt" "one\ntwo\n" "Add two")))
         (test-sfp--git "commit" "-q" "--amend" "-m" "Add two\n\nThe reader needs it.")
         (test-sfp--rendering base before (test-sfp--git "rev-parse" "HEAD")
           ,@body)))))

(ert-deftest test-sfp-a-reworded-message-shows-its-change ()
  "GIVEN a push that only reworded the message of a commit
WHEN the comparison is rendered, and the commit opened
THEN the commits count one changed, and the folded commit shows the
words added to its message in their face, and says its changes are
the same."
  (test-sfp--with-reword
    (should (string-match-p "Commits: 1 changed" (test-sfp--text)))
    (let ((text (test-sfp--opened-commit "Add two")))
      (should (string-match-p "Commit message changed:" text))
      (should (eq 'shipit-word-diff-added-face
                  (get-text-property (string-match "reader" text) 'font-lock-face text)))
      (should (string-match-p "changes are the same" text)))))

(ert-deftest test-sfp-the-commits-have-the-commit-octicon ()
  "GIVEN SVG icons, and a push that reworded a commit
WHEN the comparison is rendered
THEN the heading of the commits asks for the git-commit octicon, and
its image is in the buffer, before a title in the face of headings."
  (let ((asked nil))
    ;; `features' is not special, so only `dlet' binds it for `featurep'.
    (dlet ((shipit-use-svglib-icons t)
           (features (cons 'svg-lib features)))
      (cl-letf (((symbol-function 'svg-lib-icon)
                 (lambda (name &rest _)
                   (push name asked)
                   '(image :type svg :data "<svg/>"))))
        (test-sfp--with-reword
          (should (member "git-commit" asked))
          (let ((start (oref (test-sfp--commits-section) start)))
            (should (imagep (get-text-property (+ 3 start) 'display)))
            (goto-char start)
            (search-forward "Commits")
            (should (eq 'magit-section-heading
                        (get-text-property (match-beginning 0) 'font-lock-face)))))))))

(defun test-sfp--column-of (regexp)
  "The column where the first match of REGEXP in the buffer starts."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp)
    (goto-char (match-beginning 0))
    (current-column)))

(ert-deftest test-sfp-the-rows-line-up-under-their-headings ()
  "GIVEN a rendered comparison with a changed commit, opened
WHEN the columns are read
THEN the commit row starts where the title of the commits does, and
what the commit holds, its message and its file, starts 2 columns in."
  (test-sfp--with-reword
    (test-sfp--opened-commit "Add two")
    (let ((title (test-sfp--column-of "Commits")))
      (should (= title (test-sfp--column-of "changed ")))
      (should (= (+ title 2) (test-sfp--column-of "Commit message changed"))))))

(ert-deftest test-sfp-the-files-of-a-commit-sit-under-it ()
  "GIVEN a push that changed a commit
WHEN the comparison is rendered, and the commit opened
THEN no list of files stands apart from the commits, and the files are
sections under the commit."
  (test-sfp--with-history '(:fixed t)
    (should-not (seq-find (lambda (section) (eq 'force-push-files (oref section type)))
                          (oref magit-root-section children)))
    (test-sfp--opened-commit "Add lines")
    (should (equal '(pr-file)
                   (delete-dups (mapcar (lambda (section) (oref section type))
                                        (oref (test-sfp--commit-section "Add lines") children)))))))

(ert-deftest test-sfp-added-and-dropped-commits-list-their-files ()
  "GIVEN a push that dropped the first of two commits, kept the second,
and added a new one
WHEN the comparison is rendered, and the added and dropped ones opened
THEN the commits count each kind, and each opens to the files it
changes, with their diffs."
  (test-sfp--in-repo
    (let ((base (test-sfp--commit "a.txt" "one\n" "a")))
      (test-sfp--git "checkout" "-q" "-b" "work")
      (test-sfp--commit "x.txt" "x\n" "Add x")
      (let* ((kept (test-sfp--commit "k.txt" "k\n" "Add k"))
             (before kept))
        (test-sfp--git "checkout" "-q" "-b" "work2" base)
        (test-sfp--git "cherry-pick" kept)
        (let ((after (test-sfp--commit "n.txt" "n\n" "Add n")))
          (test-sfp--rendering base before after
            (should (string-match-p "Commits: 1 added, 1 dropped, 1 unchanged" (test-sfp--text)))
            (should (string-match-p "n\\.txt[^z]*\\+n" (test-sfp--opened-commit-files "Add n")))
            (should (string-match-p "x\\.txt[^z]*\\+x" (test-sfp--opened-commit-files "Add x")))))))))

(defconst test-sfp--lines "a\nb\nc\nd\ne\nf\ng\nh\n"
  "A file long enough for a change with lines of context around it.")

(ert-deftest test-sfp-a-changed-commit-shows-the-files-its-change-differs-in ()
  "GIVEN a push that changed one of the lines a commit adds
WHEN the comparison is rendered, and the commit and its file opened
THEN the commit shows that file, with the one line that changed."
  (test-sfp--in-repo
    (let ((base (test-sfp--commit "s.txt" test-sfp--lines "s")))
      (test-sfp--git "checkout" "-q" "-b" "work")
      (let ((before (test-sfp--commit "s.txt" "a\nb\nc\nd1\nd2\nd3\nd4\ne\nf\ng\nh\n" "Add d")))
        (test-sfp--git "checkout" "-q" "-b" "work2" base)
        (let ((after (test-sfp--commit "s.txt" "a\nb\nc\nd1\nd2\nD3\nd4\ne\nf\ng\nh\n" "Add d")))
          (test-sfp--rendering base before after
            (should (string-match-p "Commits: 1 changed" (test-sfp--text)))
            (let ((text (test-sfp--opened-commit-files "Add d")))
              (should (string-match-p "s\\.txt" text))
              (should (string-match-p "^ *-d3" text))
              (should (string-match-p "^ *\\+D3" text)))))))))

(ert-deftest test-sfp-a-commit-whose-context-moved-says-so ()
  "GIVEN a branch rebased onto a main that changed a line near the one a
commit changes, so only the lines around its change differ
WHEN the comparison is rendered, and the commit opened
THEN it says its changes are the same and only the lines around them
differ, and it has no file."
  (test-sfp--in-repo
    (test-sfp--commit "s.txt" test-sfp--lines "s")
    (test-sfp--git "checkout" "-q" "-b" "work")
    (let ((before (test-sfp--commit "s.txt" "a\nb\nc\nd\nE\nf\ng\nh\n" "Change e")))
      (test-sfp--git "checkout" "-q" "main")
      (let ((base (test-sfp--commit "s.txt" "a\nB\nc\nd\ne\nf\ng\nh\n" "Change b")))
        (test-sfp--git "checkout" "-q" "-b" "work2")
        (let ((after (test-sfp--commit "s.txt" "a\nB\nc\nd\nE\nf\ng\nh\n" "Change e")))
          (test-sfp--rendering base before after
            (should (string-match-p "Commits: 1 changed" (test-sfp--text)))
            (should (string-match-p "only the lines around them differ"
                                    (test-sfp--opened-commit "Change e")))
            (should-not (oref (test-sfp--commit-section "Change e") children))))))))

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
