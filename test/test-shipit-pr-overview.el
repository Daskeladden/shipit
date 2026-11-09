;;; test-shipit-pr-overview.el --- Views of a page, past the scenarios -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; What the scenarios of test-shipit-page-views.el leave out.
;;
;; The overview shows what waits for you.  On a pull request of someone
;; else the reviews that ask for changes are not yours to answer, and
;; only a thread or comments where you wrote can wait for you.
;;
;; A hidden part shows nothing, also not the icon at its start.
;;
;; The pages and GitHub are those of test-shipit-page-views.el.

;;; Code:

(require 'ert)
(require 'shipit-editor)
(require 'test-shipit-page-views)

(defconst test-pr-overview--daves-pr
  '(:title "Steam the milk"
    :checks (("grind beans" . "failure"))
    :reviews (("alice" "CHANGES_REQUESTED" "Use fresh beans."))
    :threads ((:resolved nil :line 2 :comments ((41 "bob" "Why two cups?")))
              (:resolved nil :line 3 :comments ((43 "hank" "Rename the pot?")
                                                (44 "me" "Use the kettle.")
                                                (45 "hank" "Which kettle?"))))
    :comments ((51 "carol" "Any news on the mugs?")
               (52 "me" "Mugs come Monday.")
               (53 "ivan" "Can we use the blue mugs?")))
  "A pull request of dave, where I answered one thread and one comment.")

(defconst test-pr-overview--untouched-pr
  '(:title "Warm the cups"
    :checks (("grind beans" . "failure"))
    :reviews (("alice" "CHANGES_REQUESTED" "Use fresh beans."))
    :threads ((:resolved nil :line 2 :comments ((41 "bob" "Why two cups?"))))
    :comments ((51 "carol" "Any news on the mugs?")))
  "A pull request of dave, where I wrote nothing.")

(defmacro test-pr-overview--with-daves-pr (number pr &rest body)
  "Open PR as the pull request NUMBER of dave in the overview; run BODY in it."
  (declare (indent 2))
  `(let ((answers (cons (list (format "/repos/o/r/pulls/%d\\'" ,number) 200
                              (cons '(user (login . "dave"))
                                    (assq-delete-all
                                     'user (test-page-views--pull-request ,number ,pr))))
                        (test-page-views--github (list (cons ,number ,pr))))))
     (test-page-views--as-me
       (shipit-test-http-answering answers
         (let ((page (test-page-views--open-pr ,number)))
           (unwind-protect
               (with-current-buffer page
                 (test-page-views--press "v")
                 ,@body)
             (kill-buffer page)))))))

(ert-deftest test-pr-overview-of-someone-else-shows-only-where-i-wrote ()
  "On a pull request of someone else, only the threads and comments where I wrote wait."
  ;; GIVEN a pull request of dave with a review that asks for changes,
  ;;   a thread of bob, a thread where hank asked again after my
  ;;   answer, and a comment of ivan after mine
  ;; WHEN I press v
  ;; THEN the overview shows the thread of hank and the comment of ivan
  ;; AND not the review or the thread of bob
  (test-pr-overview--with-daves-pr 20 test-pr-overview--daves-pr
    (test-page-views--shows "Which kettle?" "Can we use the blue mugs?")
    (test-page-views--hides "Use fresh beans." "Why two cups?"
                            "Any news on the mugs?")))

(ert-deftest test-pr-overview-of-someone-else-where-i-wrote-nothing ()
  "On a pull request of someone else where I wrote nothing, nothing waits for me."
  ;; GIVEN a pull request of dave with a review that asks for changes,
  ;;   a thread and a comment, none of them of mine
  ;; WHEN I press v
  ;; THEN the overview says Nothing waits for you
  (test-pr-overview--with-daves-pr 21 test-pr-overview--untouched-pr
    (test-page-views--shows "Warm the cups" "Nothing waits for you")))

(ert-deftest test-pr-overview-an-unknown-view-is-named ()
  "A view that `shipit-page-views' does not have is named in the error."
  ;; GIVEN the view setting names a view that the list of views lacks
  ;; WHEN I press v
  ;; THEN the error names that view
  (let ((shipit-page-view 'brunch))
    (with-temp-buffer
      (shipit-mode)
      (let ((err (should-error (call-interactively (key-binding (kbd "v")))
                               :type 'user-error)))
        (should (string-match-p "brunch" (cadr err)))))))

;;; Checks

(defconst test-pr-overview--running-checks
  '((total_count . 2)
    (check_runs . [((id . 21) (name . "heat water") (status . "in_progress")
                    (conclusion) (html_url . "https://github.com/o/r/runs/21")
                    (started_at . "2026-01-01T10:00:00Z"))
                   ((id . 22) (name . "boil water") (status . "completed")
                    (conclusion . "success") (html_url . "https://github.com/o/r/runs/22")
                    (started_at . "2026-01-01T10:00:00Z")
                    (completed_at . "2026-01-01T10:01:00Z"))]))
  "A check that still runs and one that passed, as GitHub gives them.")

(ert-deftest test-pr-overview-a-running-check-shows ()
  "The overview shows a check that still runs, and folds the ones that passed."
  ;; GIVEN my pull request with a check that still runs and one that passed
  ;; WHEN I press v
  ;; THEN the overview shows the check that runs
  ;; AND not the one that passed, whose group is folded
  (let ((answers (cons (list "/check-suites/11/check-runs" 200 test-pr-overview--running-checks)
                       (test-page-views--github (list (cons 12 test-page-views--quiet-pr))))))
    (test-page-views--as-me
      (shipit-test-http-answering answers
        (let ((page (test-page-views--open-pr 12)))
          (unwind-protect
              (with-current-buffer page
                (test-page-views--press "v")
                (test-page-views--shows "heat water" "Successful checks (1)")
                (test-page-views--hides "boil water"))
            (kill-buffer page)))))))

(ert-deftest test-pr-overview-the-normal-view-keeps-my-unfolded-checks ()
  "The normal view shows the checks again as I had unfolded them."
  ;; GIVEN my pull request in the normal view, with the checks and the
  ;;   group of the passing checks unfolded
  ;; WHEN I press v for the overview, which folds that group, and v again
  ;; THEN the passing check shows again
  (test-page-views--with-prs (list (cons 13 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 13)
      (let ((checks (shipit-page-view--part-of-type 'checks)))
        (magit-section-show checks)
        (dolist (group (oref checks children))
          (magit-section-show group)))
      (test-page-views--shows "boil water")
      (test-page-views--press "v")
      (test-page-views--hides "boil water")
      (test-page-views--press "v")
      (test-page-views--shows "boil water"))))

;;; Folds

(ert-deftest test-pr-overview-unfolding-inside-a-hidden-part-shows-nothing ()
  "Unfolding a section inside a hidden part, as magit's commands do, shows nothing."
  ;; GIVEN my pull request in the normal view, which hides the waiting
  ;;   part of the overview
  ;; WHEN a section inside the waiting part is unfolded, as the magit
  ;;   commands that unfold each section of a page do
  ;; THEN the page shows what it showed before
  (test-page-views--with-prs (list (cons 14 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 14)
      (let ((before (test-page-views--shown))
            (inhibit-read-only t))
        (dolist (group (oref (shipit-page-view--part-of-type 'shipit-page-waiting) children))
          (magit-section-hide group)
          (magit-section-show group))
        (should (equal before (test-page-views--shown)))))))

;;; Replies

(defun test-pr-overview--reply-to (author text)
  "Reply TEXT to the comment of AUTHOR in this page, as M-; and Reply do.
The editor opens with the quote of the comment, and TEXT goes below it."
  (save-current-buffer
    (goto-char (point-min))
    (should (search-forward (concat author " (") nil t))
    (shipit--reply-to-comment (get-text-property (point) 'shipit-comment-id))
    (with-current-buffer shipit-editor--buffer-name
      (goto-char (point-max))
      (insert text)
      (shipit-editor-save)))
  (test-page-views--settle))

(defmacro test-pr-overview--with-reply-answered (number &rest body)
  "Open my pull request NUMBER, where GitHub takes a reply; run BODY in it."
  (declare (indent 1))
  `(let ((answers
          (cons (list (format "/repos/o/r/issues/%d/comments\\'" ,number) 201
                      '((id . 60) (user (login . "me")) (body . "Yes, the blue ones.")
                        (created_at . "2026-01-04T10:00:00Z")
                        (updated_at . "2026-01-04T10:00:00Z")))
                (test-page-views--github (list (cons ,number test-page-views--waiting-pr))))))
     (test-page-views--as-me
       (shipit-test-http-answering answers
         (let ((page (test-page-views--open-pr ,number)))
           (unwind-protect
               (with-current-buffer page ,@body)
             (kill-buffer page)))))))

(ert-deftest test-pr-overview-a-comment-i-answer-stops-waiting ()
  "A comment that I answer leaves the waiting list at once."
  ;; GIVEN my pull request in the overview, where the comment of ivan
  ;;   waits for my answer
  ;; WHEN I reply to it from the editor
  ;; THEN the comment of ivan no longer waits
  ;; AND the review and the thread that wait still show
  (test-pr-overview--with-reply-answered 15
    (test-page-views--press "v")
    (test-page-views--shows "Can we use the blue mugs?")
    (test-pr-overview--reply-to "ivan" "Yes, the blue ones.")
    (test-page-views--hides "Can we use the blue mugs?")
    (test-page-views--shows "Use fresh beans." "Why two cups?")))

(ert-deftest test-pr-overview-a-reply-keeps-the-other-comments ()
  "After a reply in the overview the general comments show each comment and the reply."
  ;; GIVEN my pull request in the overview, with three general comments
  ;; WHEN I reply to the comment of ivan in the waiting list, and go
  ;;   back to the normal view
  ;; THEN the general comments show the three comments and my reply
  (test-pr-overview--with-reply-answered 16
    (test-page-views--press "v")
    (test-pr-overview--reply-to "ivan" "Yes, the blue ones.")
    (test-page-views--press "v")
    (magit-section-show (shipit-page-view--part-of-type 'general-comments))
    (test-page-views--shows "Any news on the mugs?" "Mugs come Monday."
                            "Can we use the blue mugs?" "Yes, the blue ones.")))

;;; Long comments

(defconst test-pr-overview--long-comment
  (concat "Can we use the blue mugs? "
          (mapconcat #'identity (make-list 30 "They keep the coffee warm.") " "))
  "A comment longer than the wrap column of a page.")

(ert-deftest test-pr-overview-a-long-comment-is-wrapped ()
  "A comment that waits is wrapped at the wrap column, as on the normal page."
  ;; GIVEN my pull request with a long comment that waits for my answer
  ;; WHEN I press v
  ;; THEN the comment shows on lines no wider than the wrap column
  (let ((pr (plist-put (copy-sequence test-page-views--waiting-pr) :comments
                       `((53 "ivan" ,test-pr-overview--long-comment)))))
    (test-page-views--with-prs (list (cons 11 pr))
      (with-current-buffer (test-page-views--open-pr 11)
        (test-page-views--press "v")
        (let ((lines (split-string (test-page-views--shown) "\n")))
          (should (seq-some (lambda (line) (string-search "Can we use the blue mugs?" line))
                            lines))
          (should (> (seq-count (lambda (line) (string-search "keep the coffee" line)) lines)
                     1))
          (dolist (line lines)
            (should (<= (string-width line) shipit-render-wrap-column))))))))

;;; Icons

(defmacro test-pr-overview--with-icons (&rest body)
  "Run BODY with the icons drawn as images, as svg-lib draws them.
svg-lib is not installed for the tests, so each icon is one image."
  (declare (indent 0))
  ;; `features' is not special, so only `dlet' binds it for `featurep'.
  `(dlet ((shipit-use-svglib-icons t)
          (shipit--svg-lib-icon-cache (make-hash-table :test 'equal))
          (features (cons 'svg-lib features)))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'svg-lib-icon)
                (lambda (&rest _) '(image :type svg :data "<svg/>"))))
       ,@body)))

(defun test-pr-overview--skip-hidden (pos)
  "The first position from POS on that is not invisible."
  (while (and (< pos (point-max)) (invisible-p pos))
    (setq pos (next-single-char-property-change pos 'invisible nil (point-max))))
  pos)

(defun test-pr-overview--drawn ()
  "The text of this page as Emacs draws it, with [icon] for each image.
Emacs looks at the display property of a position before it looks at
the invisible one, so an image at the start of hidden text is drawn."
  (let ((pos (point-min))
        (parts nil))
    (while (< pos (point-max))
      (let ((display (get-char-property pos 'display)))
        (cond (display
               (push (if (stringp display) display "[icon]") parts)
               (setq pos (next-single-char-property-change pos 'display nil (point-max))))
              ((invisible-p pos)
               (setq pos (test-pr-overview--skip-hidden pos)))
              (t
               (push (buffer-substring-no-properties pos (1+ pos)) parts)
               (setq pos (1+ pos))))))
    (apply #'concat (nreverse parts))))

(ert-deftest test-pr-overview-a-hidden-part-shows-no-icon-of-an-issue ()
  "The assignee line of an issue, hidden in the normal view, shows no icon."
  ;; GIVEN an issue page with icons, in the normal view, which hides
  ;;   the assignee line that only the overview shows
  ;; THEN the Metadata heading below it shows its own icon alone
  (test-pr-overview--with-icons
    (test-page-views--as-me
      (shipit-test-http-answering (append test-page-views--issue-answers
                                          test-page-views--repository-answers)
        (let ((page (test-page-views--open-issue)))
          (unwind-protect
              (with-current-buffer page
                (should (string-match-p "^\\[icon\\] Metadata$"
                                        (test-pr-overview--drawn))))
            (kill-buffer page)))))))

(ert-deftest test-pr-overview-hidden-parts-show-no-icons-in-the-overview ()
  "The parts that the overview of a pull request hides show no icons."
  ;; GIVEN my pull request with icons
  ;; WHEN I press v for the overview
  ;; THEN no icon of a hidden part shows: each icon stands before the
  ;;   space and the text that it goes with
  (test-pr-overview--with-icons
    (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
      (with-current-buffer (test-page-views--open-pr 7)
        (test-page-views--press "v")
        (let ((drawn (test-pr-overview--drawn)))
          (should (string-search "Waiting for you" drawn))
          (should-not (string-match-p "\\[icon\\]\\([^ ]\\|\\'\\)" drawn)))))))

(provide 'test-shipit-pr-overview)
;;; test-shipit-pr-overview.el ends here
