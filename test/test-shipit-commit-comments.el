;;; test-shipit-commit-comments.el --- Tests for branch commit comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the branch/commit comments feature: backend op, the
;; PR-buffer-shaped branch view (commits + files + inline comments +
;; general comments), the no-PR fallback, and Commit-notification
;; routing.  HTTP is the only mocked boundary (`shipit--api-request').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-pr-github)
(require 'shipit-commit-comments-buffer)
(require 'shipit-pr-sections)
(require 'shipit-test-http)
;; The shared comment renderer calls Jira mention decoration
;; unconditionally; in a real session shipit loads all backend modules.
(require 'shipit-issue-jira)

;;; Fixtures — shapes mirror the GitHub REST API (arrays are vectors,
;;; exactly as `json-read' produces them).

(defconst test-scc--sha-quiet
  "aaaa111122223333444455556666777788889999")
(defconst test-scc--sha-commented
  "bbbb111122223333444455556666777788889999")

(defconst test-scc--commit-patch
  (concat "@@ -1,3 +1,4 @@\n"
          " line one\n"
          "+added line\n"
          " line two\n"
          " line three")
  "The commented commit's own diff for fw/Loader/main.c.
Positions: 1 = ctx line one (new 1), 2 = +added line (new 2),
3 = ctx line two (new 3), 4 = ctx line three (new 4).")

(defconst test-scc--compare-patch
  (concat "@@ -1,2 +1,3 @@\n"
          " line one\n"
          "+added line\n"
          " line two")
  "The branch's total diff for fw/Loader/main.c.
Covers new-file lines 1-3 only, so a comment resolved to new line 4
is outdated relative to the branch diff.")

(defconst test-scc--deletion-patch
  (concat "@@ -1,3 +1,2 @@\n"
          " line one\n"
          "-line two\n"
          " line three")
  "Patch with a deletion: position 2 is the deleted old line 2.")

(defun test-scc--commit-entry (sha subject comment-count)
  "Build a compare-response commit entry."
  `((sha . ,sha)
    (commit . ((message . ,(concat subject "\n\nLonger body of the message"))
               (comment_count . ,comment-count)
               (author . ((name . "Alice Author")
                          (email . "alice@example.com")
                          (date . "2026-07-17T20:00:00Z")))
               (committer . ((name . "Alice Author")
                             (email . "alice@example.com")
                             (date . "2026-07-17T20:00:00Z")))))
    (author . ((login . "alice") (avatar_url . "https://example.com/a.png")))
    (committer . ((login . "alice") (avatar_url . "https://example.com/a.png")))))

(defun test-scc--compare-response (&optional ahead quiet-only)
  "Compare response with commits and the branch's total files diff.
AHEAD overrides ahead_by.  With QUIET-ONLY, only the quiet commit."
  `((ahead_by . ,(or ahead (if quiet-only 1 2)))
    (behind_by . 0)
    (commits . ,(if quiet-only
                    (vector (test-scc--commit-entry test-scc--sha-quiet "Quiet commit" 0))
                  (vector
                   (test-scc--commit-entry test-scc--sha-quiet "Quiet commit" 0)
                   (test-scc--commit-entry test-scc--sha-commented
                                           "Probe the DDR fetch path" 3))))
    (files . [((filename . "fw/Loader/main.c")
               (status . "modified")
               (additions . 1)
               (deletions . 0)
               (patch . ,test-scc--compare-patch))
              ((filename . "docs/notes.md")
               (status . "added")
               (additions . 2)
               (deletions . 0)
               (patch . "@@ -0,0 +1,2 @@\n+first note\n+second note"))])))

(defconst test-scc--comments-response
  `[((id . 9001)
     (user . ((login . "carol") (avatar_url . "https://example.com/c.png")))
     (body . "Watchdog runs off the reference clock.")
     (path . "fw/Loader/main.c")
     (position . 2)
     (line . nil)
     (commit_id . ,test-scc--sha-commented)
     (created_at . "2026-07-17T21:08:16Z")
     (html_url . "https://github.com/o/r/commit/bbbb1111#r9001"))
    ((id . 9002)
     (user . ((login . "carol") (avatar_url . "https://example.com/c.png")))
     (body . "This part got rewritten later.")
     (path . "fw/Loader/main.c")
     (position . 4)
     (line . nil)
     (commit_id . ,test-scc--sha-commented)
     (created_at . "2026-07-17T21:09:17Z")
     (html_url . "https://github.com/o/r/commit/bbbb1111#r9002"))
    ((id . 9003)
     (user . ((login . "carol") (avatar_url . "https://example.com/c.png")))
     (body . "General remark on the whole commit.")
     (path . nil)
     (position . nil)
     (line . nil)
     (commit_id . ,test-scc--sha-commented)
     (created_at . "2026-07-17T21:10:00Z")
     (html_url . "https://github.com/o/r/commit/bbbb1111#r9003"))]
  "Three commit comments: inline resolvable, inline outdated, general.")

(defun test-scc--commit-response ()
  "Single-commit response (used for position resolution and washers)."
  `((sha . ,test-scc--sha-commented)
    (commit . ((message . "Probe the DDR fetch path\n\nLonger body of the message")
               (author . ((name . "Alice Author")
                          (email . "alice@example.com")
                          (date . "2026-07-17T20:00:00Z")))
               (committer . ((name . "Alice Author")
                             (email . "alice@example.com")
                             (date . "2026-07-17T20:00:00Z")))))
    (author . ((login . "alice") (avatar_url . "https://example.com/a.png")))
    (stats . ((additions . 1) (deletions . 0) (total . 1)))
    (files . [((filename . "fw/Loader/main.c")
               (status . "modified")
               (additions . 1)
               (deletions . 0)
               (patch . ,test-scc--commit-patch))])))

;;; Harness

(defmacro test-scc--with-api (responses &rest body)
  "Run BODY with `shipit--api-request' served from RESPONSES.
RESPONSES is an alist of (ENDPOINT-STRING . RESPONSE) matched
exactly.  Requests for endpoints not in RESPONSES signal an error
so unexpected traffic fails loudly.  The list of requested
endpoints is available in BODY as `api-calls' (newest last)."
  (declare (indent 1))
  `(let ((api-calls nil))
     (ignore api-calls)
     (cl-letf (((symbol-function 'shipit--api-request)
                (lambda (endpoint &optional _params _callback)
                  (setq api-calls (append api-calls (list endpoint)))
                  (let ((hit (assoc endpoint ,responses)))
                    (unless hit
                      (error "test-scc: unexpected API request %s" endpoint))
                    (cdr hit)))))
       ,@body)))

(defmacro test-scc--with-quiet-messages (&rest body)
  "Run BODY capturing `message' output; messages available as `messages'."
  `(let ((messages nil))
     (ignore messages)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt
                    (push (apply #'format fmt args) messages))
                  nil)))
       ,@body)))

(defun test-scc--branch-api-responses ()
  "API responses for the standard branch flow."
  `(("/repos/o/r/compare/master...feature" . ,(test-scc--compare-response))
    (,(format "/repos/o/r/commits/%s/comments" test-scc--sha-commented)
     . ,test-scc--comments-response)
    (,(format "/repos/o/r/commits/%s" test-scc--sha-commented)
     . ,(test-scc--commit-response))))

(defun test-scc--find-section (type &optional value parent)
  "Find a descendant section of PARENT (default root) by TYPE and VALUE."
  (let ((stack (list (or parent magit-root-section)))
        (found nil))
    (while (and stack (not found))
      (let ((section (pop stack)))
        (when (and (eq (oref section type) type)
                   (or (null value) (equal (oref section value) value)))
          (setq found section))
        (setq stack (append stack (oref section children)))))
    found))

(defun test-scc--kill-buffer (name)
  "Kill buffer NAME if it exists."
  (when (get-buffer name)
    (kill-buffer name)))

(defmacro test-scc--with-branch-buffer (varname &rest body)
  "Open the standard branch flow buffer as VARNAME and run BODY."
  (declare (indent 1))
  `(let ((shipit-pr-backend 'github)
         (shipit-show-avatars nil)
         (shipit-show-pr-checks nil))
     (unwind-protect
         (test-scc--with-api (test-scc--branch-api-responses)
           (test-scc--with-quiet-messages
            (let ((,varname (shipit-commit-comments-open-branch "o/r" "feature" "master")))
              ,@body)))
       (test-scc--kill-buffer "*shipit-commit-comments: o/r feature*"))))

;;; Backend registry and endpoint

(ert-deftest test-scc-registry-has-fetch-commit-comments ()
  "GIVEN the GitHub PR backend
WHEN checking the registered operations
THEN :fetch-commit-comments is present."
  (let ((plist (cdr (assq 'github shipit-pr-backends))))
    (should (plist-get plist :fetch-commit-comments))))

(ert-deftest test-scc-github-fetch-commit-comments-endpoint ()
  "GIVEN a repo config and a commit SHA
WHEN fetching commit comments via the GitHub backend
THEN the commit-comments endpoint is requested
     AND the response vector is returned as a list."
  (test-scc--with-api `((,(format "/repos/o/r/commits/%s/comments"
                                  test-scc--sha-commented)
                         . ,test-scc--comments-response))
    (let ((result (shipit-pr-github--fetch-commit-comments
                   '(:repo "o/r") test-scc--sha-commented)))
      (should (equal api-calls
                     (list (format "/repos/o/r/commits/%s/comments"
                                   test-scc--sha-commented))))
      (should (listp result))
      (should (= (length result) 3)))))

;;; Position resolution (pure functions)

(ert-deftest test-scc-position-to-line-right-side ()
  "GIVEN a commit patch with a context line, an addition, and more context
WHEN resolving positions 1, 2 and 4
THEN they map to RIGHT-side new-file lines 1, 2 and 4."
  (let ((p1 (shipit-commit-comments--position-to-line test-scc--commit-patch 1))
        (p2 (shipit-commit-comments--position-to-line test-scc--commit-patch 2))
        (p4 (shipit-commit-comments--position-to-line test-scc--commit-patch 4)))
    (should (equal p1 '(:line 1 :side "RIGHT")))
    (should (equal p2 '(:line 2 :side "RIGHT")))
    (should (equal p4 '(:line 4 :side "RIGHT")))))

(ert-deftest test-scc-position-to-line-left-side ()
  "GIVEN a commit patch containing a deletion at position 2
WHEN resolving that position
THEN it maps to the LEFT-side old-file line 2."
  (should (equal (shipit-commit-comments--position-to-line
                  test-scc--deletion-patch 2)
                 '(:line 2 :side "LEFT"))))

(ert-deftest test-scc-position-to-line-out-of-range ()
  "GIVEN a position beyond the patch
WHEN resolving it
THEN nil is returned."
  (should (null (shipit-commit-comments--position-to-line
                 test-scc--commit-patch 99))))

(ert-deftest test-scc-patch-covers-line-p ()
  "GIVEN the branch's compare patch covering new lines 1-3
WHEN checking coverage of RIGHT lines 2 and 4
THEN line 2 is covered and line 4 is not."
  (should (shipit-commit-comments--patch-covers-line-p
           test-scc--compare-patch 2 "RIGHT"))
  (should-not (shipit-commit-comments--patch-covers-line-p
               test-scc--compare-patch 4 "RIGHT")))

;;; Branch flow — PR-buffer-shaped rendering

(ert-deftest test-scc-open-branch-renders-pr-structure ()
  "GIVEN a branch with commits, files, and commit comments
WHEN opening the branch view
THEN the buffer has PR-buffer structure: a branch title, a
     pr-commits section, and a pr-files section with the changed file."
  (test-scc--with-branch-buffer buf
    (should (bufferp buf))
    (should (equal (buffer-name buf) "*shipit-commit-comments: o/r feature*"))
    (with-current-buffer buf
      (should (eq major-mode 'shipit-commit-comments-mode))
      (should (test-scc--find-section 'pr-commits))
      (should (test-scc--find-section 'pr-files))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "feature" text))
        (should (string-match-p "Commits (2)" text))
        (should (string-match-p "Files Changed" text))))))

(ert-deftest test-scc-open-branch-inline-comment-in-file-diff ()
  "GIVEN an inline commit comment resolving to a line in the branch diff
WHEN expanding the file section
THEN the diff shows the comment body after the commented line
     AND the outdated comment appears in the outdated area."
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (let ((files-section (test-scc--find-section 'pr-files)))
        (should files-section)
        (magit-section-show files-section))
      (let ((file-section (test-scc--find-section 'pr-file "fw/Loader/main.c")))
        (should file-section)
        (should (= 1 (get-text-property (oref file-section start)
                                        'shipit-comment-count)))
        (magit-section-show file-section)
        ;; The outdated group renders collapsed inside the file diff
        (let ((outdated (test-scc--find-section 'shipit-outdated-comments)))
          (should outdated)
          (magit-section-show outdated))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "added line" text))
          (should (string-match-p "Watchdog runs off the reference clock" text))
          (should (string-match-p "This part got rewritten later" text)))))))

(ert-deftest test-scc-open-branch-general-comments-section ()
  "GIVEN a pathless commit comment
WHEN opening the branch view and expanding General Comments
THEN the section shows the comment body."
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (let ((general (test-scc--find-section 'general-comments)))
        (should general)
        (magit-section-show general)
        (should (string-match-p "General remark on the whole commit"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

(ert-deftest test-scc-open-branch-without-comments-still-opens ()
  "GIVEN a branch ahead of base with no commit comments
WHEN opening the branch view
THEN the buffer opens with commits and files but no inline comments."
  (let ((shipit-pr-backend 'github)
        (shipit-show-avatars nil)
        (shipit-show-pr-checks nil))
    (unwind-protect
        (test-scc--with-api `(("/repos/o/r/compare/master...feature"
                               . ,(test-scc--compare-response 1 'quiet-only)))
          (test-scc--with-quiet-messages
           (let ((buf (shipit-commit-comments-open-branch "o/r" "feature" "master")))
             (should (bufferp buf))
             (with-current-buffer buf
               (should (test-scc--find-section 'pr-commits))
               (should (null shipit--cached-inline-comments))))))
      (test-scc--kill-buffer "*shipit-commit-comments: o/r feature*"))))

(ert-deftest test-scc-open-branch-not-ahead-messages ()
  "GIVEN a branch with no commits ahead of base
WHEN opening the branch view
THEN no buffer is created and an informative message names the branch."
  (let ((shipit-pr-backend 'github))
    (test-scc--with-api `(("/repos/o/r/compare/master...feature"
                           . ((ahead_by . 0) (behind_by . 3)
                              (commits . []) (files . []))))
      (cl-letf (((symbol-function 'shipit-commit-comments--local-unpushed-count)
                 (lambda (&rest _) 0)))
        (test-scc--with-quiet-messages
         (let ((result (shipit-commit-comments-open-branch "o/r" "feature" "master")))
           (should (null result))
           (should (null (get-buffer "*shipit-commit-comments: o/r feature*")))
           (should (seq-find (lambda (m)
                               (and (string-match-p "feature" m)
                                    (string-match-p "no commits ahead" m)))
                             messages))))))))

(ert-deftest test-scc-open-branch-renders-under-icon-variants ()
  "GIVEN the comment icon style set to emoji and to text
WHEN opening the branch view under each variant
THEN the PR-shaped structure renders in both."
  (dolist (style '(emoji text))
    (let ((shipit-inline-comment-icon-style style))
      (test-scc--with-branch-buffer buf
        (with-current-buffer buf
          (should (test-scc--find-section 'pr-files)))))))

;;; Single-commit flow (notification landing)

(ert-deftest test-scc-open-commit-single-mode ()
  "GIVEN a commit SHA with comments
WHEN opening the single-commit view
THEN the buffer shows the commit, its files, and inline comments
     resolved against the commit's own diff (never outdated)."
  (let ((shipit-pr-backend 'github)
        (shipit-show-avatars nil)
        (shipit-show-pr-checks nil)
        (buf-name (format "*shipit-commit-comments: o/r %s*"
                          (substring test-scc--sha-commented 0 7))))
    (unwind-protect
        (test-scc--with-api `((,(format "/repos/o/r/commits/%s"
                                        test-scc--sha-commented)
                               . ,(test-scc--commit-response))
                              (,(format "/repos/o/r/commits/%s/comments"
                                        test-scc--sha-commented)
                               . ,test-scc--comments-response))
          (test-scc--with-quiet-messages
           (let ((buf (shipit-commit-comments-open-commit
                       "o/r" test-scc--sha-commented)))
             (should (bufferp buf))
             (should (equal (buffer-name buf) buf-name))
             (with-current-buffer buf
               (should (eq major-mode 'shipit-commit-comments-mode))
               (should (test-scc--find-section 'pr-commits))
               (let ((files-section (test-scc--find-section 'pr-files)))
                 (should files-section)
                 (magit-section-show files-section))
               (let ((file-section (test-scc--find-section 'pr-file "fw/Loader/main.c")))
                 (should file-section)
                 ;; Both inline comments are active against the commit's own
                 ;; diff (positions 2 and 4 both resolve inside it)
                 (should (= 2 (get-text-property (oref file-section start)
                                                 'shipit-comment-count)))
                 (magit-section-show file-section)
                 (should (string-match-p
                          "Watchdog runs off the reference clock"
                          (buffer-substring-no-properties (point-min) (point-max)))))))))
      (test-scc--kill-buffer buf-name))))

;;; Notification routing

(ert-deftest test-scc-notification-open-routes-commit-type ()
  "GIVEN a notification activity of type commit at point
WHEN invoking the notifications-buffer open command
THEN the commit-notification opener receives number, repo, and activity."
  (require 'shipit-notifications-buffer)
  (let* ((activity `((type . "commit")
                     (repo . "o/r")
                     (number . 4242)
                     (browse-url . ,(format "https://github.com/o/r/commit/%s"
                                            test-scc--sha-commented))))
         (opened nil)
         (current-prefix-arg nil))
    (cl-letf (((symbol-function 'shipit-notifications-buffer--activity-at-point)
               (lambda () activity))
              ((symbol-function 'shipit-notifications-buffer--activity-props-at-point)
               (lambda () nil))
              ((symbol-function 'shipit--open-notification-commit)
               (lambda (number repo act)
                 (setq opened (list number repo act)))))
      (shipit-notifications-buffer-open)
      (should (equal opened (list 4242 "o/r" activity))))))

(ert-deftest test-scc-open-notification-commit-extracts-sha ()
  "GIVEN a commit activity whose browse-url contains /commit/SHA
WHEN opening the commit notification
THEN the single-commit view opens for that repo and SHA."
  (require 'shipit-notifications)
  (let ((opened nil)
        (current-prefix-arg nil))
    (cl-letf (((symbol-function 'shipit-commit-comments-open-commit)
               (lambda (repo sha) (setq opened (list repo sha)) 'buffer))
              ((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest _) nil)))
      (shipit--open-notification-commit
       4242 "o/r"
       `((type . "commit")
         (repo . "o/r")
         (number . 4242)
         (browse-url . ,(format "https://github.com/o/r/commit/%s"
                                test-scc--sha-commented))))
      (should (equal opened (list "o/r" test-scc--sha-commented))))))

(ert-deftest test-scc-open-notification-commit-browser-fallback ()
  "GIVEN a commit activity whose browse-url has no /commit/SHA component
WHEN opening the commit notification
THEN the URL opens in the browser instead."
  (require 'shipit-notifications)
  (let ((browsed nil)
        (current-prefix-arg nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq browsed url)))
              ((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest _) nil)))
      (shipit--open-notification-commit
       4242 "o/r"
       '((type . "commit")
         (repo . "o/r")
         (number . 4242)
         (browse-url . "https://github.com/o/r")))
      (should (equal browsed "https://github.com/o/r")))))

;;; No-PR fallback

(ert-deftest test-scc-no-pr-fallback-opens-branch-comments ()
  "GIVEN a branch with no PR
WHEN opening the PR for the current branch
THEN the branch commit-comments view is attempted for that branch."
  (require 'shipit-commands)
  (let ((opened nil))
    (cl-letf (((symbol-function 'shipit--github-token) (lambda () "token"))
              ((symbol-function 'magit-get-current-branch) (lambda () "feature"))
              ((symbol-function 'shipit--get-repo-from-remote) (lambda () "o/r"))
              ((symbol-function 'shipit-get-pr-for-branch-async)
               (lambda (_branch callback &optional _repo)
                 (funcall callback nil)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'shipit-commit-comments-open-branch)
               (lambda (repo branch &optional _base)
                 (setq opened (list repo branch))
                 nil)))
      (test-scc--with-quiet-messages
       (shipit-open-pr-for-current-branch))
      (should (equal opened (list "o/r" "feature"))))))

;;; Reactions on commit comments

(ert-deftest test-scc-adapted-comments-carry-commit-type ()
  "GIVEN raw commit comments (inline and pathless)
WHEN adapting/splitting them for the buffer
THEN both carry shipit-comment-type \"commit\" so the reaction
     machinery can pick the commit-comment endpoints."
  (let* ((patches `(("fw/Loader/main.c" . ,test-scc--commit-patch)))
         (split (shipit-commit-comments--split-comments
                 (append test-scc--comments-response nil) patches patches))
         (inline (car split))
         (general (cdr split)))
    (should (equal (cdr (assq 'shipit-comment-type (car inline))) "commit"))
    (should (equal (cdr (assq 'shipit-comment-type (car general))) "commit"))))

(ert-deftest test-scc-reaction-cache-key-commit-distinct ()
  "GIVEN the same repo and comment id
WHEN building reaction cache keys for the three comment kinds
THEN the commit key is distinct from inline and general."
  (let ((commit-key (shipit--reaction-cache-key "o/r" 9 'commit))
        (inline-key (shipit--reaction-cache-key "o/r" 9 t))
        (general-key (shipit--reaction-cache-key "o/r" 9 nil)))
    (should-not (equal commit-key inline-key))
    (should-not (equal commit-key general-key))
    (should-not (equal inline-key general-key))))

(ert-deftest test-scc-reaction-endpoints-commit-type ()
  "GIVEN a commit-type comment id
WHEN toggling, fetching, and deleting reactions with the commit flag
THEN the commit-comment reactions endpoints are used."
  (require 'shipit-comment-github)
  (let ((posted nil)
        (paginated nil)
        (deleted nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional _method)
                 (setq posted (list endpoint data))
                 '((id . 77))))
              ((symbol-function 'shipit--api-request-paginated)
               (lambda (endpoint &rest _)
                 (setq paginated endpoint)
                 nil))
              ((symbol-function 'shipit--url-retrieve-sync)
               (lambda (url method &rest _)
                 (setq deleted (list method url))
                 204)))
      (shipit-comment-github--toggle-reaction '(:repo "o/r") 9001 "+1" 'commit)
      (shipit-comment-github--fetch-reactions '(:repo "o/r") 9001 'commit)
      (shipit-comment-github--delete-reaction '(:repo "o/r") 9001 77 'commit)
      (should (equal (car posted) "/repos/o/r/comments/9001/reactions"))
      (should (equal paginated "/repos/o/r/comments/9001/reactions"))
      (should (equal (car deleted) "DELETE"))
      (should (string-suffix-p "/repos/o/r/comments/9001/reactions/77"
                               (cadr deleted))))))

(ert-deftest test-scc-toggle-reaction-routes-commit-flag ()
  "GIVEN point on a commit comment in the branch buffer (inline and general)
WHEN toggling a reaction interactively
THEN the comment backend is called with the commit tri-state flag."
  (require 'shipit-pr-actions)
  (require 'shipit-diff)
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (let ((files-section (test-scc--find-section 'pr-files)))
        (magit-section-show files-section))
      (let ((file-section (test-scc--find-section 'pr-file "fw/Loader/main.c")))
        (magit-section-show file-section))
      (let ((general (test-scc--find-section 'general-comments)))
        (magit-section-show general))
      (let* ((calls nil)
             (reaction (cdr (car shipit-reaction-choices))))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (car (car shipit-reaction-choices))))
                  ((symbol-function 'shipit-comment-github--toggle-reaction)
                   (lambda (_config id r flag)
                     (push (list id r flag) calls)
                     '((id . 5))))
                  ((symbol-function 'shipit-comment--fetch-reactions)
                   (lambda (&rest _) nil))
                  ((symbol-function 'shipit--get-current-user) (lambda () "ronny"))
                  ((symbol-function 'shipit--ensure-repository) (lambda () "o/r"))
                  ((symbol-function 'shipit--update-comment-reactions-display)
                   (lambda (&rest _) nil)))
          (let ((inline-pos (text-property-any (point-min) (point-max)
                                               'shipit-comment-id 9001))
                (general-pos (text-property-any (point-min) (point-max)
                                                'shipit-comment-id 9003)))
            (should inline-pos)
            (should general-pos)
            (goto-char inline-pos)
            (shipit--toggle-reaction-interactive nil)
            (goto-char general-pos)
            (shipit--toggle-reaction-interactive nil)
            (should (equal (nreverse calls)
                           (list (list 9001 reaction 'commit)
                                 (list 9003 reaction 'commit))))))))))

;;; No-PR fallback robustness (deferred out of the sentinel, errors surfaced)

(ert-deftest test-scc-no-pr-fallback-surfaces-errors ()
  "GIVEN the branch view signals an error during the no-PR fallback
WHEN opening the PR for the current branch
THEN the error is reported in the echo area instead of vanishing."
  (require 'shipit-commands)
  (cl-letf (((symbol-function 'shipit--github-token) (lambda () "token"))
            ((symbol-function 'magit-get-current-branch) (lambda () "feature"))
            ((symbol-function 'shipit--get-repo-from-remote) (lambda () "o/r"))
            ((symbol-function 'shipit-get-pr-for-branch-async)
             (lambda (_branch callback &optional _repo)
               (funcall callback nil)))
            ((symbol-function 'run-at-time)
             (lambda (_time _repeat fn &rest args) (apply fn args)))
            ((symbol-function 'shipit-commit-comments-open-branch)
             (lambda (&rest _) (error "compare exploded"))))
    (test-scc--with-quiet-messages
     (shipit-open-pr-for-current-branch)
     (should (seq-find (lambda (m)
                         (and (string-match-p "feature" m)
                              (string-match-p "compare exploded" m)))
                       messages)))))

;;; Files filter in the branch buffer

(ert-deftest test-scc-files-filter-comments-only ()
  "GIVEN a branch with a commented and an uncommented file
WHEN applying the with-comments files filter and then clearing it
THEN the Files Changed section shows only the commented file, then both."
  (require 'shipit-file-filter)
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (cl-flet ((visible-files ()
                  (let ((files (test-scc--find-section 'pr-files)))
                    (should files)
                    (magit-section-show files)
                    (mapcar (lambda (s) (oref s value))
                            (seq-filter (lambda (s) (eq (oref s type) 'pr-file))
                                        (oref files children)))))
                (marker-pos (m) (if (markerp m) (marker-position m) m)))
        (let ((files-before (test-scc--find-section 'pr-files)))
          (should (equal (visible-files) '("fw/Loader/main.c" "docs/notes.md")))
          (shipit--filter-files-with-comments)
          (should (equal (visible-files) '("fw/Loader/main.c")))
          (shipit--clear-files-filter)
          (should (equal (visible-files)
                         '("fw/Loader/main.c" "docs/notes.md")))
          ;; Structural integrity after the filter roundtrip: the section
          ;; object must survive (targeted content refresh, not a
          ;; delete-and-append replacement) ...
          (should (eq files-before (test-scc--find-section 'pr-files)))
          ;; ... root's children must stay in buffer order (n/p
          ;; navigation walks this list) ...
          (let* ((children (oref magit-root-section children))
                 (starts (mapcar (lambda (s) (marker-pos (oref s start)))
                                 children)))
            (should (equal starts (sort (copy-sequence starts) #'<)))
            ;; ... and sibling boundaries must not overlap
            (let ((prev-end nil))
              (dolist (s children)
                (let ((start (marker-pos (oref s start)))
                      (end (marker-pos (oref s end))))
                  (when prev-end
                    (should (<= prev-end start)))
                  (should (< start end))
                  (setq prev-end end))))))))))

;;; Copying the branch / commit URL from the title

(ert-deftest test-scc-browse-compare-url-op ()
  "GIVEN the GitHub PR backend
WHEN building a compare browse URL
THEN the op exists and formats the base...head compare URL."
  (let* ((plist (cdr (assq 'github shipit-pr-backends)))
         (fn (plist-get plist :browse-compare-url)))
    (should fn)
    (should (equal (funcall fn '(:repo "o/r") "master" "feature")
                   "https://github.com/o/r/compare/master...feature"))))

(ert-deftest test-scc-copy-url-branch-mode ()
  "GIVEN the branch buffer
WHEN copying the URL from the title
THEN the compare URL lands on the kill ring
     AND the title heading binds M-w to the copy command."
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (let* ((title (test-scc--find-section 'shipit-branch-title))
             (pos (oref title start))
             (km (get-text-property pos 'keymap)))
        (should km)
        (should (eq (lookup-key km (kbd "M-w"))
                    'shipit-commit-comments-copy-url))
        (goto-char pos)
        (shipit-commit-comments-copy-url)
        (should (equal (current-kill 0)
                       "https://github.com/o/r/compare/master...feature"))))))

(ert-deftest test-scc-copy-url-single-commit-mode ()
  "GIVEN the single-commit buffer
WHEN copying the URL from the title
THEN the commit URL lands on the kill ring."
  (let ((shipit-pr-backend 'github)
        (shipit-show-avatars nil)
        (shipit-show-pr-checks nil)
        (buf-name (format "*shipit-commit-comments: o/r %s*"
                          (substring test-scc--sha-commented 0 7))))
    (unwind-protect
        (test-scc--with-api `((,(format "/repos/o/r/commits/%s"
                                        test-scc--sha-commented)
                               . ,(test-scc--commit-response))
                              (,(format "/repos/o/r/commits/%s/comments"
                                        test-scc--sha-commented)
                               . ,test-scc--comments-response))
          (test-scc--with-quiet-messages
           (let ((buf (shipit-commit-comments-open-commit
                       "o/r" test-scc--sha-commented)))
             (with-current-buffer buf
               (shipit-commit-comments-copy-url)
               (should (equal (current-kill 0)
                              (format "https://github.com/o/r/commit/%s"
                                      test-scc--sha-commented)))))))
      (test-scc--kill-buffer buf-name))))

(ert-deftest test-scc-o-opens-the-compare-page-or-the-commit ()
  "GIVEN the branch buffer
WHEN o is pressed on its title
THEN the browser shows the compare page of the branch
WHEN the commits are unfolded, and o is pressed on a commit
THEN the browser shows that commit."
  (test-scc--with-branch-buffer buf
    (with-current-buffer buf
      (let ((opened nil))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (push url opened))))
          (goto-char (oref (test-scc--find-section 'shipit-branch-title) start))
          (call-interactively (key-binding (kbd "o")))
          (magit-section-show (test-scc--find-section 'pr-commits))
          (goto-char (oref (test-scc--find-section 'shipit-commit test-scc--sha-commented)
                           start))
          (call-interactively (key-binding (kbd "o"))))
        (should (equal (reverse opened)
                       (list "https://github.com/o/r/compare/master...feature"
                             (format "https://github.com/o/r/commit/%s"
                                     test-scc--sha-commented))))))))

;;; Empty-branch diagnostics

(ert-deftest test-scc-open-branch-unpushed-branch-explains ()
  "GIVEN a branch that does not exist on GitHub but has local commits
WHEN opening the branch view
THEN the message says the branch is not on GitHub and counts the
     local commits, instead of the misleading \='no commits ahead\='."
  (let ((shipit-pr-backend 'github))
    (test-scc--with-api '(("/repos/o/r/compare/master...feature" . nil))
      (cl-letf (((symbol-function 'shipit-commit-comments--local-unpushed-count)
                 (lambda (_base _branch) 2)))
        (test-scc--with-quiet-messages
         (should (null (shipit-commit-comments-open-branch "o/r" "feature" "master")))
         (should (seq-find (lambda (m)
                             (and (string-match-p "feature" m)
                                  (string-match-p "not on GitHub" m)
                                  (string-match-p "2 local commit" m)
                                  (string-match-p "[Pp]ush" m)))
                           messages)))))))

(ert-deftest test-scc-open-branch-remote-missing-generic ()
  "GIVEN the compare fails and the local clone can't count commits
WHEN opening the branch view
THEN the message still explains the branch/base was not found and
     suggests pushing."
  (let ((shipit-pr-backend 'github))
    (test-scc--with-api '(("/repos/o/r/compare/master...feature" . nil))
      (cl-letf (((symbol-function 'shipit-commit-comments--local-unpushed-count)
                 (lambda (&rest _) nil)))
        (test-scc--with-quiet-messages
         (should (null (shipit-commit-comments-open-branch "o/r" "feature" "master")))
         (should (seq-find (lambda (m)
                             (and (string-match-p "feature" m)
                                  (string-match-p "not found on GitHub" m)
                                  (string-match-p "[Pp]ush" m)))
                           messages)))))))

(ert-deftest test-scc-open-branch-stale-remote-explains-unpushed ()
  "GIVEN a branch that exists on GitHub with no new commits there,
      while the local branch has unpushed commits
WHEN opening the branch view
THEN the message reports the unpushed commits."
  (let ((shipit-pr-backend 'github))
    (test-scc--with-api '(("/repos/o/r/compare/master...feature"
                           . ((ahead_by . 0) (behind_by . 0)
                              (commits . []) (files . []))))
      (cl-letf (((symbol-function 'shipit-commit-comments--local-unpushed-count)
                 (lambda (_base _branch) 3)))
        (test-scc--with-quiet-messages
         (should (null (shipit-commit-comments-open-branch "o/r" "feature" "master")))
         (should (seq-find (lambda (m)
                             (and (string-match-p "feature" m)
                                  (string-match-p "3 unpushed commit" m)))
                           messages)))))))

;;; A branch that changes more files than the compare API lists

(defun test-scc--cup-file (n)
  "The name of the Nth changed file of a branch with many files."
  (format "kitchen/cup-%03d.txt" n))

(defun test-scc--cup-file-entry (n)
  "The compare API's entry of the Nth changed file."
  `((filename . ,(test-scc--cup-file n))
    (status . "modified")
    (additions . 1)
    (deletions . 1)
    (patch . "@@ -1 +1 @@\n-empty\n+full")))

(defun test-scc--cup-file-diff (n)
  "The part of a diff that changes the Nth file."
  (let ((name (test-scc--cup-file n)))
    (format "diff --git a/%s b/%s\nindex 1111111..2222222 100644\n--- a/%s\n+++ b/%s\n@@ -1 +1 @@\n-empty\n+full\n"
            name name name name)))

(defun test-scc--compare-of-cups (listed)
  "A compare response that lists the first LISTED changed files."
  `((ahead_by . 1)
    (behind_by . 0)
    (commits . ,(vector (test-scc--commit-entry test-scc--sha-quiet "Fill the cups" 0)))
    (files . ,(vconcat (mapcar #'test-scc--cup-file-entry
                               (number-sequence 1 listed))))))

(defun test-scc--compare-without-patch-of (changed lacking)
  "A compare response that lists CHANGED files, the LACKING-th without a patch.
GitHub lists a file so when its diff is past a size budget: no patch,
and no lines added or removed."
  (let ((compare (test-scc--compare-of-cups changed)))
    (aset (cdr (assq 'files compare)) (1- lacking)
          `((filename . ,(test-scc--cup-file lacking))
            (status . "modified")
            (additions . 0)
            (deletions . 0)))
    compare))

(defun test-scc--asks-for-diff-p ()
  "Whether the request that is made now asks for a diff, not for JSON."
  (equal "application/vnd.github.diff"
         (cdr (assoc "Accept" url-request-extra-headers))))

(defun test-scc--compare-answer (changed compare)
  "GitHub's answer to a compare of a branch that changes CHANGED files.
Asked for JSON, it gives COMPARE.  Asked for a diff, it gives the diff
of all the files."
  (lambda (_request)
    (if (test-scc--asks-for-diff-p)
        (mapconcat #'test-scc--cup-file-diff (number-sequence 1 changed) "")
      (json-encode compare))))

(defmacro test-scc--in-cups-branch-view (status answer &rest body)
  "Run BODY in the branch view of feature in o/r.
GitHub answers its compare with STATUS and ANSWER, as
`shipit-test-http-answering' takes them.  BODY sees the messages of
the view as `messages'."
  (declare (indent 2))
  `(let ((shipit-github-token "not-a-token")
         (shipit--github-token-cache nil)
         (shipit-pr-backend 'github)
         (shipit-show-avatars nil)
         (shipit-show-pr-checks nil))
     (unwind-protect
         (shipit-test-http-answering
             (list (list "/repos/o/r/compare/master\\.\\.\\.feature\\'" ,status ,answer))
           (test-scc--with-quiet-messages
            (with-current-buffer (shipit-commit-comments-open-branch "o/r" "feature" "master")
              ,@body)))
       (test-scc--kill-buffer "*shipit-commit-comments: o/r feature*"))))

(defun test-scc--text ()
  "The text of this buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun test-scc--shown-file (name)
  "Show the file NAME in the files section of this view; return its text."
  (magit-section-show (test-scc--find-section 'pr-files))
  (let ((file (test-scc--find-section 'pr-file name)))
    (should file)
    (magit-section-show file)
    (buffer-substring-no-properties (oref file start) (oref file end))))

(ert-deftest test-scc-open-branch-lists-every-file-past-the-compare-limit ()
  "GIVEN a branch that changes 301 files, AND GitHub's compare that
lists only the first 300 of them, the most it lists
WHEN the user opens the branch view
THEN the files section counts 301 files AND shows the diff of the
last one."
  (test-scc--in-cups-branch-view 200
      (test-scc--compare-answer 301 (test-scc--compare-of-cups 300))
    (should (string-match-p "Files Changed (301, \\+301 -301)" (test-scc--text)))
    (should (string-match-p "\\+full" (test-scc--shown-file (test-scc--cup-file 301))))))

(ert-deftest test-scc-open-branch-shows-the-diff-github-leaves-out ()
  "GIVEN a branch that changes 2 files, AND GitHub's compare that lists
both, the second without its patch, as it lists a file with a big diff
WHEN the user opens the branch view
THEN the files section counts the lines of both files AND shows the
diff of the second."
  (test-scc--in-cups-branch-view 200
      (test-scc--compare-answer 2 (test-scc--compare-without-patch-of 2 2))
    (should (string-match-p "Files Changed (2, \\+2 -2)" (test-scc--text)))
    (should (string-match-p "\\+full" (test-scc--shown-file (test-scc--cup-file 2))))))

(ert-deftest test-scc-open-branch-asks-for-no-diff-when-each-patch-is-listed ()
  "GIVEN a branch that changes 2 files, AND GitHub's compare that lists
both with their patches
WHEN the user opens the branch view
THEN the view asks GitHub for the compare once, and for no diff."
  (test-scc--in-cups-branch-view 200
      (test-scc--compare-answer 2 (test-scc--compare-of-cups 2))
    (should (string-match-p "Files Changed (2, \\+2 -2)" (test-scc--text)))
    (should (= 1 (length shipit-test-http-requests)))))

(ert-deftest test-scc-open-branch-says-when-github-refuses-the-files-past-the-limit ()
  "GIVEN a branch that changes 301 files, AND GitHub's compare that
lists only the first 300 of them, AND GitHub refusing the diff of all
of them
WHEN the user opens the branch view
THEN the files section shows the 300 files that the compare lists, AND
a message says that only those are shown, and why."
  (test-scc--in-cups-branch-view
      (lambda () (if (test-scc--asks-for-diff-p) 406 200))
      (lambda (_request)
        (if (test-scc--asks-for-diff-p)
            (json-encode '((message . "Sorry, this diff is too large")))
          (json-encode (test-scc--compare-of-cups 300))))
    (should (string-match-p "Files Changed (300, \\+300 -300)" (test-scc--text)))
    (should (seq-find (lambda (m)
                        (and (string-match-p "first 300 changed files" m)
                             (string-match-p "406" m)))
                      messages))))

(ert-deftest test-scc-the-diff-of-all-files-skips-the-cache-of-the-compare ()
  "GIVEN a branch that changes 301 files, AND url.el keeping GitHub's
earlier JSON answer for the address of its compare
WHEN the user opens the branch view, which asks that address for the
diff of all the files
THEN the request of the diff tells url.el to send no date of its copy,
so that GitHub does not answer 304 Not Modified, AND url.el stores no
copy of the diff, which would take the place of the JSON answer."
  (let ((url-automatic-caching t)
        (caching-for-diff 'not-asked))
    (test-scc--in-cups-branch-view 200
        (let ((answer (test-scc--compare-answer 301 (test-scc--compare-of-cups 300))))
          (lambda (request)
            (when (test-scc--asks-for-diff-p)
              (setq caching-for-diff url-automatic-caching))
            (funcall answer request)))
      (let ((diff-headers (seq-find (lambda (headers)
                                      (equal "application/vnd.github.diff"
                                             (cdr (assoc "Accept" headers))))
                                    shipit-test-http-headers)))
        (should (equal "no-cache" (cdr (assoc "Pragma" diff-headers))))
        (should (null caching-for-diff))))))

(defconst test-scc--diff-of-each-kind
  (concat
   "diff --git a/kitchen/new-cup.txt b/kitchen/new-cup.txt\n"
   "new file mode 100644\n"
   "index 0000000..1111111\n"
   "--- /dev/null\n"
   "+++ b/kitchen/new-cup.txt\n"
   "@@ -0,0 +1,2 @@\n"
   "+coffee\n"
   "+milk\n"
   "diff --git a/kitchen/old-cup.txt b/kitchen/old-cup.txt\n"
   "deleted file mode 100644\n"
   "index 1111111..0000000\n"
   "--- a/kitchen/old-cup.txt\n"
   "+++ /dev/null\n"
   "@@ -1 +0,0 @@\n"
   "-tea\n"
   "diff --git a/kitchen/mug.txt b/kitchen/cup.txt\n"
   "similarity index 80%\n"
   "rename from kitchen/mug.txt\n"
   "rename to kitchen/cup.txt\n"
   "index 1111111..2222222 100644\n"
   "--- a/kitchen/mug.txt\n"
   "+++ b/kitchen/cup.txt\n"
   "@@ -1,2 +1,2 @@\n"
   " coffee\n"
   "-small\n"
   "+big\n"
   "diff --git a/kitchen/photo.png b/kitchen/photo.png\n"
   "new file mode 100644\n"
   "index 0000000..3333333\n"
   "Binary files /dev/null and b/kitchen/photo.png differ\n"
   "diff --git \"a/kitchen/caf\\303\\251.txt\" \"b/kitchen/caf\\303\\251.txt\"\n"
   "index 1111111..2222222 100644\n"
   "--- \"a/kitchen/caf\\303\\251.txt\"\n"
   "+++ \"b/kitchen/caf\\303\\251.txt\"\n"
   "@@ -1 +1 @@\n"
   "-closed\n"
   "+open\n"
   "diff --git a/kitchen/big mug.txt b/kitchen/big mug.txt\n"
   "index 1111111..2222222 100644\n"
   "--- a/kitchen/big mug.txt\t\n"
   "+++ b/kitchen/big mug.txt\t\n"
   "@@ -1 +1 @@\n"
   "-cold\n"
   "+hot\n"
   "\\ No newline at end of file\n")
  "A diff with a file of each kind: added, removed, renamed, binary,
with a name outside ASCII, and with a space in its name, which git
ends with a tab on the lines of its two sides.")

(ert-deftest test-scc-github-reads-each-kind-of-file-from-a-diff ()
  "GIVEN GitHub's diff of a compare with a file of each kind: added,
removed, renamed, binary, with a name outside ASCII and with a space
in its name
WHEN the GitHub backend reads the changed files from it
THEN each file has the name, status, counts and previous name that
the compare API gives such a file, AND its patch starts at its first
hunk."
  (let ((shipit-github-token "not-a-token")
        (shipit--github-token-cache nil))
    (shipit-test-http-answering
        `(("/repos/o/r/compare/master\\.\\.\\.feature\\'" 200 ,test-scc--diff-of-each-kind))
      (let ((files (shipit-pr-github--fetch-compare-diff-files
                    '(:repo "o/r") "master" "feature")))
        (should (equal (mapcar (lambda (file)
                                 (mapcar (lambda (key) (cdr (assq key file)))
                                         '(filename status additions deletions previous_filename)))
                               files)
                       '(("kitchen/new-cup.txt" "added" 2 0 nil)
                         ("kitchen/old-cup.txt" "removed" 0 1 nil)
                         ("kitchen/cup.txt" "renamed" 1 1 "kitchen/mug.txt")
                         ("kitchen/photo.png" "added" 0 0 nil)
                         ("kitchen/café.txt" "modified" 1 1 nil)
                         ("kitchen/big mug.txt" "modified" 1 1 nil))))
        (should (equal (cdr (assq 'patch (car files)))
                       "@@ -0,0 +1,2 @@\n+coffee\n+milk"))
        (should-not (assq 'patch (nth 3 files)))))))

(provide 'test-shipit-commit-comments)
;;; test-shipit-commit-comments.el ends here
