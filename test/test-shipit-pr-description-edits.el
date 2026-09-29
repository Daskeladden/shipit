;;; test-shipit-pr-description-edits.el --- Edits of a PR description in its activity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; GitHub's REST timeline has no event for an edited description.  Its
;; GraphQL API keeps the history in `userContentEdits': newest first,
;; each with its editor, its time and the whole text of that version,
;; the oldest being the text the pull request was opened with.  Each
;; edit becomes an activity row, which opens to a word diff against the
;; version before it.  HTTP is the only mocked boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'text-property-search)
(require 'magit-section)
(require 'shipit-pr-github)
(require 'shipit-pr-sections)
(require 'shipit-pr-actions)

;;; Fixtures

(defun test-spde--node (id at login text)
  "A version of a description, ID, saved AT by LOGIN, holding TEXT."
  `((id . ,id) (editedAt . ,at) (diff . ,text)
    (editor . ((login . ,login) (avatarUrl . "https://example.com/a.png")))))

(defun test-spde--reply (&rest nodes)
  "Wrap NODES, newest first, in the shape `shipit--graphql-request' returns."
  `((data . ((repository . ((pullRequest . ((userContentEdits . ((nodes . ,nodes)))))))))))

(defconst test-spde--history
  (test-spde--reply
   (test-spde--node "E3" "2026-09-10T12:00:00Z" "carol" "Fix the parser quickly.\n")
   (test-spde--node "E2" "2026-09-09T12:00:00Z" "bob" "Fix the parser slowly.\n")
   (test-spde--node "E1" "2026-09-08T12:00:00Z" "alice" "Fix the lexer slowly.\n"))
  "Two edits of a description, after the text the pull request was opened with.")

(defmacro test-spde--with-graphql (response &rest body)
  "Run BODY with `shipit--graphql-request' answering RESPONSE.
An asynchronous request gets it through its callback."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'shipit--graphql-request)
              (lambda (_query _variables &optional callback)
                (if callback (funcall callback ,response) ,response))))
     ,@body))

(defun test-spde--event (from to)
  "An edit of the description event, FROM one text TO another."
  `((event . "description_edited") (id . "E2") (created_at . "2026-09-09T12:00:00Z")
    (actor . ((login . "bob") (avatar_url . "https://example.com/b.png")))
    (shipit-description-from . ,from) (shipit-description-to . ,to)))

(defun test-spde--render-row (event)
  "Insert EVENT as an activity row, and return its finished section."
  (magit-insert-section (root)
    (shipit--insert-activity-event event "o/r" 7 nil))
  (car (last (oref magit-root-section children))))

;;; Backend

(ert-deftest test-spde-each-edit-is-an-event-against-the-version-before ()
  "GIVEN a description saved three times: opened, then edited twice
WHEN the edits are read
THEN there are two events, each by its editor, at its time, from the
version before it to its own."
  (let ((events (shipit-pr-github--description-edit-events test-spde--history)))
    (should (= 2 (length events)))
    (let ((newest (car events)))
      (should (equal "description_edited" (alist-get 'event newest)))
      (should (equal "E3" (alist-get 'id newest)))
      (should (equal "2026-09-10T12:00:00Z" (alist-get 'created_at newest)))
      (should (equal "carol" (alist-get 'login (alist-get 'actor newest))))
      (should (equal "Fix the parser slowly.\n" (alist-get 'shipit-description-from newest)))
      (should (equal "Fix the parser quickly.\n" (alist-get 'shipit-description-to newest))))
    (should (equal "Fix the lexer slowly.\n"
                   (alist-get 'shipit-description-from (cadr events))))))

(ert-deftest test-spde-a-description-never-edited-has-no-event ()
  "GIVEN a description with no history, or a request that failed
WHEN the edits are read
THEN there is no event."
  (should-not (shipit-pr-github--description-edit-events (test-spde--reply)))
  (should-not (shipit-pr-github--description-edit-events nil)))

(ert-deftest test-spde-the-timeline-gets-the-edits ()
  "GIVEN a timeline of a pull request whose description was edited twice
WHEN the edits are added to it, the way the buffer fetches the timeline
THEN the events of the timeline stay, and the two edits follow."
  (let ((got nil))
    (test-spde--with-graphql test-spde--history
      (shipit-pr-github--add-description-edits-async
       '(:repo "o/r") 7 '(((event . "labeled") (id . 1)))
       (lambda (events) (setq got events))))
    (should (equal '("labeled" "description_edited" "description_edited")
                   (mapcar (lambda (event) (alist-get 'event event)) got)))))

;;; The activity row

(ert-deftest test-spde-an-edit-row-opens-to-its-words ()
  "GIVEN an edit of the description that changed one word
WHEN its activity row shows, and is opened
THEN it says the description was edited, it was folded, and it shows
the old word removed and the new word added, each in its face."
  (with-temp-buffer
    (let ((row (test-spde--render-row
                (test-spde--event "Fix the parser slowly.\n" "Fix the parser quickly.\n"))))
      (should (string-match-p "edited the description"
                              (buffer-substring-no-properties (oref row start) (point-max))))
      (should (oref row hidden))
      (magit-section-show row)
      (let ((text (buffer-substring (oref row start) (oref row end))))
        (should (eq 'shipit-word-diff-removed-face
                    (get-text-property (string-match "slowly" text) 'font-lock-face text)))
        (should (eq 'shipit-word-diff-added-face
                    (get-text-property (string-match "quickly" text) 'font-lock-face text)))))))

(ert-deftest test-spde-an-edit-row-has-the-pencil-octicon ()
  "GIVEN SVG icons
WHEN an edit row shows
THEN its icon is the pencil octicon, drawn as an image."
  (let ((asked nil))
    ;; `features' is not special, so only `dlet' binds it for `featurep'.
    (dlet ((shipit-use-svglib-icons t)
           (features (cons 'svg-lib features)))
      (cl-letf (((symbol-function 'svg-lib-icon)
                 (lambda (name &rest _)
                   (push name asked)
                   '(image :type svg :data "<svg/>"))))
        (with-temp-buffer
          (let ((row (test-spde--render-row (test-spde--event "a\n" "b\n"))))
            (should (member "pencil" asked))
            (goto-char (point-min))
            (let ((match (text-property-search-forward 'display nil
                                                       (lambda (_ value) (imagep value)))))
              (should match)
              (should (= (line-number-at-pos (oref row start))
                         (line-number-at-pos (prop-match-beginning match)))))))))))

(ert-deftest test-spde-return-opens-the-edit ()
  "GIVEN point on an edit row that was never opened
WHEN the activity navigation runs
THEN the row opens."
  (with-temp-buffer
    (let ((row (test-spde--render-row (test-spde--event "a\n" "b\n"))))
      (goto-char (oref row start))
      (shipit-pr--dispatch-activity-navigation '(:event-type "description_edited"))
      (should-not (oref row hidden)))))

(provide 'test-shipit-pr-description-edits)
;;; test-shipit-pr-description-edits.el ends here
