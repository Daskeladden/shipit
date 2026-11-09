;;; shipit-pr-overview.el --- The overview parts of a pull request page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Two parts of a pull request page that only a view shows, see
;; `shipit-page-views'.  The summary shows the title, the state and the
;; review status.  The waiting part shows what waits for you: on your
;; own pull request the reviews that ask for changes, and on each pull
;; request the review threads and the general comments that wait for
;; your answer.  The checks are a part of the page of their own.
;;
;; A review thread waits for your answer when it is not resolved and its
;; newest comment is not yours.  A general comment waits when it came
;; after your last general comment.  On a pull request of someone else,
;; only a thread or comments where you wrote can wait for you.
;;
;; Both parts are drawn from what the page got from the forge already.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-cache)
(require 'shipit-page-views)

(declare-function shipit--get-current-user "shipit-pr-actions")
(declare-function shipit--is-comment-in-resolved-thread "shipit-comments")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit--get-status-icon "shipit-render")
(declare-function shipit--get-approval-status-icon "shipit-render")
(declare-function shipit--get-comment-type-icon "shipit-render")
(declare-function shipit--clean-text "shipit-core")
(declare-function shipit--get-user-type-icon "shipit-render")
(declare-function shipit--render-comment-body "shipit-render")
(declare-function shipit--pr-state-line "shipit-http")
(declare-function shipit--group-comments-by-api-replies "shipit-http")
(declare-function shipit--insert-status-hierarchical-comment "shipit-pr-sections")

(defvar shipit-buffer-pr-data)
(defvar shipit-buffer-repo)
(defvar shipit-buffer-pr-number)
(defvar shipit-buffer--ready-hook-fired)

(put 'shipit-page-summary 'magit-section t)
(put 'shipit-page-waiting 'magit-section t)
(put 'shipit-page-waiting-group 'magit-section t)
(put 'shipit-page-waiting-review 'magit-section t)

;;; Who wrote what

(defun shipit-pr-overview--login (item)
  "The login of the user who wrote ITEM, a comment or a review."
  (let ((user (or (cdr (assq 'user item)) (cdr (assq 'author item)))))
    (if (stringp user) user (cdr (assq 'login user)))))

(defun shipit-pr-overview--by-p (me item)
  "Whether ME wrote ITEM."
  (equal (shipit-pr-overview--login item) me))

(defun shipit-pr-overview--time (comment)
  "The time COMMENT was written, as GitHub writes it, which sorts as text."
  (or (cdr (assq 'created_at comment)) ""))

(defun shipit-pr-overview--oldest-first (comments)
  "COMMENTS, the oldest first."
  (sort (copy-sequence comments)
        (lambda (a b) (string< (shipit-pr-overview--time a)
                               (shipit-pr-overview--time b)))))

;;; What waits

(defun shipit-pr-overview--change-requests ()
  "The latest reviews of this page that ask for changes."
  (seq-filter (lambda (review)
                (equal (cdr (assq 'state review)) "CHANGES_REQUESTED"))
              (cdr (assq 'latest-reviews shipit--shown-review-info))))

(defun shipit-pr-overview--threads (comments)
  "The review threads of COMMENTS, as lists of their comments, the oldest first.
A reply names the first comment of its thread."
  (let ((threads nil))
    (dolist (comment comments)
      (let ((root (or (cdr (assq 'in_reply_to_id comment)) (cdr (assq 'id comment)))))
        (push comment (alist-get root threads))))
    (mapcar (lambda (thread) (shipit-pr-overview--oldest-first (cdr thread)))
            (nreverse threads))))

(defun shipit-pr-overview--thread-waits-p (thread me mine)
  "Whether THREAD, its comments the oldest first, waits for an answer of ME.
MINE says whether the pull request is of ME."
  (and (not (shipit--is-comment-in-resolved-thread (cdr (assq 'id (car thread)))))
       (not (shipit-pr-overview--by-p me (car (last thread))))
       (or mine (seq-some (apply-partially #'shipit-pr-overview--by-p me) thread))))

(defun shipit-pr-overview--waiting-threads (me mine)
  "The review threads of this page that wait for an answer of ME.
MINE says whether the pull request is of ME."
  (seq-filter (lambda (thread) (shipit-pr-overview--thread-waits-p thread me mine))
              (shipit-pr-overview--threads shipit--cached-inline-comments)))

(defun shipit-pr-overview--waiting-comments (me mine)
  "The general comments of this page that came after the last one of ME.
MINE says whether the pull request is of ME.  The bodies of reviews are
left out: a review shows as a review."
  (let* ((comments (seq-remove (lambda (comment)
                                 (equal (cdr (assq 'shipit-comment-type comment)) "review"))
                               shipit--cached-general-comments))
         (own (seq-filter (apply-partially #'shipit-pr-overview--by-p me) comments))
         (last-own (car (last (shipit-pr-overview--oldest-first own)))))
    (when (or mine last-own)
      (seq-filter (lambda (comment)
                    (and (not (shipit-pr-overview--by-p me comment))
                         (or (null last-own)
                             (string< (shipit-pr-overview--time last-own)
                                      (shipit-pr-overview--time comment)))))
                  comments))))

(defun shipit-pr-overview--waiting ()
  "What waits for the user on this page, as a plist.
:reviews are the reviews that ask for changes, only on a pull request
of the user.  :threads and :comments are the threads and the general
comments that wait for an answer."
  (let* ((me (shipit--get-current-user))
         (mine (shipit-pr-overview--by-p me shipit-buffer-pr-data)))
    (list :reviews (and mine (shipit-pr-overview--change-requests))
          :threads (shipit-pr-overview--waiting-threads me mine)
          :comments (shipit-pr-overview--waiting-comments me mine))))

;;; Drawing

(defmacro shipit-pr-overview--with-group (heading items &rest body)
  "Insert a section with HEADING, a count of ITEMS, and BODY.
There is no section when there are no ITEMS."
  (declare (indent 2))
  `(when ,items
     (magit-insert-section (shipit-page-waiting-group)
       (magit-insert-heading (format "   %s (%d)" ,heading (length ,items)))
       ,@body)))

(defun shipit-pr-overview--insert-review (review)
  "Insert REVIEW, which asks for changes, with its text wrapped."
  (magit-insert-section (shipit-page-waiting-review review)
    (magit-insert-heading
      (format "      %s %s" (shipit--get-user-type-icon "single" "🧑")
              (propertize (or (shipit-pr-overview--login review) "?")
                          'font-lock-face 'shipit-username-face)))
    (let ((body (shipit--render-comment-body review 9)))
      (unless (string-blank-p body)
        (insert body "\n")))))

(defun shipit-pr-overview--insert-threads (threads)
  "Insert THREADS, each a list of its comments, as the files section shows them.
Each thread shows the lines of the diff that it is about."
  (let ((replies (shipit--group-comments-by-api-replies (apply #'append threads))))
    (dolist (root (gethash 'root replies))
      (shipit--insert-status-hierarchical-comment
       root replies (cdr (assq 'path root)) (cdr (assq 'outdated root))
       0 t t shipit-buffer-repo shipit-buffer-pr-number nil 1))))

(defun shipit-pr-overview--insert-comment (comment)
  "Insert COMMENT, a general comment, as the comments section shows it."
  (shipit--insert-status-hierarchical-comment
   comment nil nil nil 0 nil nil shipit-buffer-repo shipit-buffer-pr-number nil 1))

(defun shipit-pr-overview--insert-groups (waiting)
  "Insert the groups of WAITING, see `shipit-pr-overview--waiting'."
  (let ((reviews (plist-get waiting :reviews))
        (threads (plist-get waiting :threads))
        (comments (plist-get waiting :comments)))
    (shipit-pr-overview--with-group
        (concat (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌") " Changes requested")
        reviews
      (mapc #'shipit-pr-overview--insert-review reviews))
    (shipit-pr-overview--with-group
        (concat (shipit--get-comment-type-icon "comment" "💬") " Threads")
        threads
      (shipit-pr-overview--insert-threads threads))
    (shipit-pr-overview--with-group
        (concat (shipit--get-comment-type-icon "comment" "💬") " Comments")
        comments
      (mapc #'shipit-pr-overview--insert-comment comments))))

(defun shipit-pr-overview--nothing-p (waiting)
  "Whether nothing waits in WAITING."
  (not (cl-some (lambda (key) (plist-get waiting key))
                '(:reviews :threads :comments))))

(defun shipit-pr-overview--insert-waiting ()
  "Insert the waiting part of this page; return its section."
  (magit-insert-section (shipit-page-waiting)
    (magit-insert-heading
      (format "%s Waiting for you" (shipit--get-status-icon "waiting" "⏳")))
    (if (not shipit-buffer--ready-hook-fired)
        (insert "   Loading…\n")
      (let ((waiting (shipit-pr-overview--waiting)))
        (if (shipit-pr-overview--nothing-p waiting)
            (insert "   Nothing waits for you\n")
          (shipit-pr-overview--insert-groups waiting))))
    (insert "\n")))

(defun shipit-pr-overview--insert-summary ()
  "Insert the summary of this page: the title, the state and the review status.
Return its section.  The State and Approval lines are those of the
normal page."
  (let ((pr shipit-buffer-pr-data))
    (magit-insert-section (shipit-page-summary)
      (insert (format "%s #%s %s\n"
                      (shipit--get-pr-field-icon "pull-request" "🔀")
                      (cdr (assq 'number pr))
                      (string-trim (shipit--clean-text (or (cdr (assq 'title pr)) ""))))
              (shipit--pr-state-line pr) "\n"
              (format "%s Approval:  %s\n"
                      (shipit--get-pr-field-icon "approval" "❓")
                      (or (cdr (assq 'status-text shipit--shown-review-info)) "…"))))))

(defconst shipit-pr-overview--folded-check-groups '(successful-checks skipped-checks)
  "The groups of checks that a view folds, because they need nothing of the author.")

(defun shipit-pr-overview--needs-nothing-p (group)
  "Whether the checks in GROUP, a section of the checks section, need nothing."
  (memq (oref group type) shipit-pr-overview--folded-check-groups))

(defun shipit-pr-overview--unfold-checks (section)
  "Unfold the checks SECTION in a view, with the groups that need nothing folded.
A failing, a cancelled, a running or a waiting check stays in sight.
When each check passed or was skipped, the section stays folded to its
heading, which counts them."
  ;; The groups are drawn when the section is first unfolded.
  (magit-section-show section)
  (let ((groups (oref section children)))
    (if (and groups (cl-every #'shipit-pr-overview--needs-nothing-p groups))
        (magit-section-hide section)
      (dolist (group groups)
        (shipit-page-view-fold group (shipit-pr-overview--needs-nothing-p group))))))

(setf (alist-get 'checks shipit-page-view-unfold-functions)
      #'shipit-pr-overview--unfold-checks)

(defun shipit-pr-overview-insert-parts ()
  "Insert the summary and the waiting part of this page, at point."
  (shipit-pr-overview--insert-summary)
  (shipit-pr-overview--insert-waiting))

(defun shipit-pr-overview-redraw ()
  "Draw the summary and the waiting part of this page again, when it has them."
  (when (shipit-page-view--part-of-type 'shipit-page-summary)
    (shipit-page-view-draw-part 'shipit-page-summary #'shipit-pr-overview--insert-summary)
    (shipit-page-view-draw-part 'shipit-page-waiting #'shipit-pr-overview--insert-waiting)))

(provide 'shipit-pr-overview)
;;; shipit-pr-overview.el ends here
