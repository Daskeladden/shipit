;;; test-shipit-notifications-external.el --- Tests for pushed notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the notifications that another package hands to shipit.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'text-property-search)
(require 'shipit-notifications-external)
(require 'shipit-notifications-rss)
(require 'shipit-notifications-buffer)
(require 'shipit-pr-github)
(require 'shipit-test-http)

(defmacro test-external--fresh (&rest body)
  "Run BODY with no notification kept or marked read.
A request to a server is an error: a pushed notification needs none."
  (declare (indent 0))
  `(let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
         (shipit--last-notification-count 0)
         (shipit--notification-count 0)
         (shipit--mention-count 0)
         (shipit--mention-prs nil)
         (shipit--modeline-string nil))
     (shipit-test-http-answering nil
       ,@body)))

(defun test-external--push-mail (&rest more)
  "Push a notification of new mail, with MORE keys of its plist."
  (apply #'shipit-notifications-push
         :source "mail.example" :title "New mail" :body "From alice" more))

(defun test-external--the-one ()
  "The one notification that shipit holds."
  (let ((activities (hash-table-values shipit--notification-pr-activities)))
    (should (= 1 (length activities)))
    (car activities)))

(defmacro test-external--in-buffer (&rest body)
  "Run BODY in the notifications buffer, drawn from what shipit holds.
Point is on the line of the pushed notification."
  (declare (indent 0))
  `(let ((buffer (shipit-notifications-buffer-create)))
     (unwind-protect
         (with-current-buffer buffer
           (shipit-notifications-buffer--rerender)
           (goto-char (point-min))
           (let ((line (text-property-search-forward
                        'shipit-notification-type "external" #'equal)))
             (should line)
             (goto-char (prop-match-beginning line)))
           ,@body)
       (kill-buffer buffer))))

(defun test-external--line ()
  "The line of the notification at point, as text."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;;; Pushing

(ert-deftest test-external-a-pushed-notification-is-kept-as-unread ()
  "GIVEN no notifications
WHEN another package pushes one, with a source, a title and a body
THEN shipit holds one unread notification with those, AND the mode line
counts it."
  (test-external--fresh
    (test-external--push-mail)
    (let ((activity (test-external--the-one)))
      (should (equal "mail.example" (cdr (assq 'repo activity))))
      (should (equal "New mail" (cdr (assq 'subject activity))))
      (should (equal "From alice" (cdr (assq 'description activity))))
      (should (equal "external" (cdr (assq 'type activity))))
      (should (eq 'external (cdr (assq 'backend-id activity))))
      (should (cdr (assq 'updated-at activity))))
    (should (= 1 (shipit--count-unread-activities)))
    (should (string-match-p "1" shipit--modeline-string))))

(ert-deftest test-external-two-pushes-are-two-notifications ()
  "GIVEN a pushed notification
WHEN the same source pushes one with the same title again
THEN shipit holds two: each push is a notification of its own."
  (test-external--fresh
    (test-external--push-mail)
    (test-external--push-mail)
    (should (= 2 (hash-table-count shipit--notification-pr-activities)))))

(ert-deftest test-external-a-notification-needs-a-source-and-a-title ()
  "GIVEN a push without a source, and one without a title
THEN each is an error that names what is missing, AND nothing is kept."
  (test-external--fresh
    (should (string-match-p ":source"
                            (cadr (should-error (shipit-notifications-push :title "T")))))
    (should (string-match-p ":title"
                            (cadr (should-error (shipit-notifications-push :source "s")))))
    (should (zerop (hash-table-count shipit--notification-pr-activities)))))

;;; Mentions

(defconst test-external--github-mention
  '((id . "thread-1")
    (subject . ((type . "Issue")
                (title . "A bug")
                (url . "https://api.github.com/repos/owner/repo/issues/7")))
    (reason . "mention")
    (unread . t)
    (repository . ((full_name . "owner/repo")
                   (html_url . "https://github.com/owner/repo")))
    (updated_at . "2026-04-24T08:00:00Z"))
  "A notification of GitHub that says the user was mentioned in an issue.")

(defun test-external--github-poll (notifications)
  "Have shipit process NOTIFICATIONS, as a poll of GitHub gives them."
  (let ((shipit-notification-alert-backend nil))
    (shipit--process-notifications notifications)))

(ert-deftest test-external-a-notification-says-why-it-came ()
  "GIVEN a package that knows why a notification came
WHEN it pushes the notification with that reason
THEN the notification has the reason, AND one that is pushed without a
reason has \"new\"."
  (test-external--fresh
    (test-external--push-mail :reason "mention")
    (should (equal "mention" (cdr (assq 'reason (test-external--the-one))))))
  (test-external--fresh
    (test-external--push-mail)
    (should (equal "new" (cdr (assq 'reason (test-external--the-one)))))))

(ert-deftest test-external-a-reason-must-be-a-string ()
  "GIVEN a push with a reason that is a symbol
THEN it is an error that names the reason, AND nothing is kept."
  (test-external--fresh
    (should (string-match-p ":reason"
                            (cadr (should-error
                                   (test-external--push-mail :reason 'mention)))))
    (should (zerop (hash-table-count shipit--notification-pr-activities)))))

(ert-deftest test-external-a-pushed-mention-is-counted-as-a-mention ()
  "GIVEN no mention
WHEN a package pushes a notification with the reason \"mention\"
THEN shipit counts one mention, AND the mode line shows it at the @."
  (test-external--fresh
    (test-external--push-mail :reason "mention")
    (should (= 1 shipit--mention-count))
    (should (string-match-p "@1" shipit--modeline-string))))

(ert-deftest test-external-a-pushed-notification-with-another-reason-is-no-mention ()
  "GIVEN no mention
WHEN a package pushes a notification without a reason
THEN shipit counts no mention."
  (test-external--fresh
    (test-external--push-mail)
    (should (= 0 shipit--mention-count))
    (should-not (string-match-p "@" shipit--modeline-string))))

(ert-deftest test-external-a-poll-of-github-keeps-a-pushed-mention ()
  "GIVEN a pushed mention
WHEN shipit processes a poll of GitHub that holds no mention
THEN the pushed mention is still counted."
  (test-external--fresh
    (test-external--push-mail :reason "mention")
    (test-external--github-poll nil)
    (should (= 1 shipit--mention-count))))

(ert-deftest test-external-a-pushed-mention-and-one-of-github-are-two ()
  "GIVEN a pushed mention
WHEN shipit processes a poll of GitHub that holds a mention
THEN two mentions are counted, AND a second poll with the same mention
counts two again: none is counted twice."
  (test-external--fresh
    (test-external--push-mail :reason "mention")
    (test-external--github-poll (list test-external--github-mention))
    (should (= 2 shipit--mention-count))
    (test-external--github-poll (list test-external--github-mention))
    (should (= 2 shipit--mention-count))))

(ert-deftest test-external-a-second-push-does-not-drop-a-mention-of-github ()
  "GIVEN a mention that a poll of GitHub gave
WHEN a package pushes a notification that is no mention
THEN the mention of GitHub is still counted."
  (test-external--fresh
    (test-external--github-poll (list test-external--github-mention))
    (test-external--push-mail)
    (should (= 1 shipit--mention-count))))

(ert-deftest test-external-a-pushed-mention-that-is-opened-is-counted-no-more ()
  "GIVEN a pushed mention that can be opened
WHEN it is opened in the notifications buffer
THEN no mention is counted."
  (test-external--fresh
    (test-external--push-mail :reason "mention" :open #'ignore)
    (test-external--in-buffer
      (shipit-notifications-buffer-open))
    (should (= 0 shipit--mention-count))))

;;; In the notifications buffer

(ert-deftest test-external-the-buffer-shows-the-reason-of-a-mention ()
  "GIVEN a pushed notification that is a mention
WHEN the notifications buffer is drawn
THEN its line shows the reason, as the line of a mention of GitHub does."
  (test-external--fresh
    (test-external--push-mail :reason "mention")
    (test-external--in-buffer
      (should (string-match-p "New mail.*mention" (test-external--line))))))

(ert-deftest test-external-the-buffer-shows-the-source-and-the-title ()
  "GIVEN a pushed notification
WHEN the notifications buffer is drawn
THEN its line shows the source where a repository stands, and the
title, AND no number."
  (test-external--fresh
    (test-external--push-mail)
    (test-external--in-buffer
      (should (string-match-p "mail\\.example .*New mail" (test-external--line)))
      (should-not (string-match-p "#" (test-external--line))))))

(ert-deftest test-external-opening-calls-the-function-of-the-notification ()
  "GIVEN a pushed notification that came with a function to open it
WHEN it is opened in the notifications buffer
THEN that function is called, AND no browser is asked."
  (test-external--fresh
    (let ((opened 0))
      (test-external--push-mail :open (lambda () (cl-incf opened))
                                :url "https://mail.example/inbox")
      (test-external--in-buffer
        (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (error "No browser"))))
          (shipit-notifications-buffer-open)))
      (should (= 1 opened)))))

(ert-deftest test-external-opening-marks-it-as-read ()
  "GIVEN two pushed notifications, each with a function to open it
WHEN one is opened in the notifications buffer
THEN shipit holds only the other one, AND the mode line counts one: no
server holds a pushed notification as unread, so to open it is to deal
with it."
  (test-external--fresh
    (let ((opened nil))
      (shipit-notifications-push :source "chat.example" :title "A message"
                                 :open (lambda () (push 'chat opened)))
      (test-external--push-mail :open (lambda () (push 'mail opened)))
      (let ((buffer (shipit-notifications-buffer-create)))
        (unwind-protect
            (with-current-buffer buffer
              (shipit-notifications-buffer--rerender)
              (goto-char (point-min))
              (let (line)
                (while (and (setq line (text-property-search-forward
                                        'shipit-notification-type "external" #'equal))
                            (not (string-match-p
                                  "New mail"
                                  (buffer-substring-no-properties
                                   (prop-match-beginning line) (prop-match-end line))))))
                (should line)
                (goto-char (prop-match-beginning line)))
              (shipit-notifications-buffer-open))
          (kill-buffer buffer)))
      (should (equal '(mail) opened)))
    (should (equal "A message" (cdr (assq 'subject (test-external--the-one)))))
    (should (= 1 (shipit--count-unread-activities)))))

(ert-deftest test-external-one-that-cannot-be-opened-stays-unread ()
  "GIVEN a pushed notification whose function to open it signals an error
WHEN it is opened in the notifications buffer
THEN the error reaches the reader, AND shipit still holds the
notification: nothing was dealt with."
  (test-external--fresh
    (test-external--push-mail :open (lambda () (error "The page is gone")))
    (test-external--in-buffer
      (should-error (shipit-notifications-buffer-open)))
    (should (= 1 (hash-table-count shipit--notification-pr-activities)))))

(ert-deftest test-external-opening-without-a-function-goes-to-the-address ()
  "GIVEN a pushed notification with an address and no function to open it
WHEN it is opened in the notifications buffer
THEN the browser is asked for the address."
  (test-external--fresh
    (let ((asked nil))
      (test-external--push-mail :url "https://mail.example/inbox")
      (test-external--in-buffer
        (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq asked url))))
          (shipit-notifications-buffer-open)))
      (should (equal "https://mail.example/inbox" asked)))))

(ert-deftest test-external-opening-one-that-cannot-be-opened-says-so ()
  "GIVEN a pushed notification with no function and no address
WHEN it is opened in the notifications buffer
THEN an error says that it has nothing to open."
  (test-external--fresh
    (test-external--push-mail)
    (test-external--in-buffer
      (should (string-match-p "nothing to open"
                              (cadr (should-error (shipit-notifications-buffer-open)
                                                  :type 'user-error)))))))

(ert-deftest test-external-unfolding-shows-the-body ()
  "GIVEN a pushed notification with a body
WHEN its section is filled, as unfolding it does
THEN the buffer shows the body."
  (test-external--fresh
    (test-external--push-mail)
    (test-external--in-buffer
      (shipit-notifications-buffer--load-section-content
       (magit-current-section) (shipit-notifications-buffer--activity-at-point))
      (should (string-match-p "From alice" (buffer-string))))))

(ert-deftest test-external-the-action-menu-opens-and-marks-read ()
  "GIVEN a pushed notification in the notifications buffer
WHEN its action menu is asked for
THEN it offers to open it and to mark it as read, and not the actions
of a pull request, AND opening calls the function of the notification."
  (test-external--fresh
    (let ((opened 0) (offered nil))
      (test-external--push-mail :open (lambda () (cl-incf opened)))
      (test-external--in-buffer
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (setq offered collection) "Open")))
          (shipit-notifications-buffer-action)))
      (should (equal '("Open" "Mark as read") offered))
      (should (= 1 opened)))))

(ert-deftest test-external-marking-read-takes-it-away ()
  "GIVEN a pushed notification
WHEN it is marked as read
THEN shipit holds it no more, AND the mode line counts none, AND no
server is asked."
  (test-external--fresh
    (test-external--push-mail)
    (let ((activity (test-external--the-one)))
      (shipit--mark-notification-read (cdr (assq 'number activity)) "mail.example" t "external"))
    (should (zerop (hash-table-count shipit--notification-pr-activities)))
    (should (zerop (shipit--count-unread-activities)))))

;;; Icons, with SVG and without

(defun test-external--octicons-of-the-line ()
  "Draw the buffer with SVG icons; the names of the icons its line asked for.
It is nil when the line of the pushed notification has no image."
  (let ((asked nil))
    ;; `features' is not special, so only `dlet' binds it for `featurep'.
    (dlet ((shipit-use-svglib-icons t)
           (features (cons 'svg-lib features)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'svg-lib-icon)
                 (lambda (name &rest _)
                   (push name asked)
                   '(image :type svg :data "<svg/>"))))
        (test-external--in-buffer
          (and (cl-some (lambda (pos) (imagep (get-text-property pos 'display)))
                        (number-sequence (line-beginning-position) (line-end-position)))
               asked))))))

(ert-deftest test-external-the-line-has-octicons-with-svg-icons ()
  "GIVEN SVG icons
WHEN the notifications buffer shows a pushed notification
THEN its line has images, the plug octicon for where it comes from and
the inbox octicon for what it is."
  (test-external--fresh
    (test-external--push-mail)
    (let ((icons (test-external--octicons-of-the-line)))
      (should (member "plug" icons))
      (should (member "inbox" icons)))))

(ert-deftest test-external-the-line-has-letters-without-svg-icons ()
  "GIVEN no SVG icons
WHEN the notifications buffer shows a pushed notification
THEN its line starts with letters for where it comes from and for what
it is, AND has no image."
  (test-external--fresh
    (test-external--push-mail)
    (let ((shipit-use-svglib-icons nil))
      (test-external--in-buffer
        (should (string-match-p "EX +NT " (test-external--line)))
        (should-not (cl-some (lambda (pos) (imagep (get-text-property pos 'display)))
                             (number-sequence (line-beginning-position)
                                              (line-end-position))))))))

;;; The registry

(ert-deftest test-external-is-a-source-of-notifications-only ()
  "GIVEN the backends of shipit
THEN the pushed notifications have one of their own, which is marked as
giving notifications only, AND an operation on an issue is an error
there, as it is for the RSS feeds."
  (dolist (id '(external rss))
    (let ((backend (cdr (assq id shipit-issue-backends))))
      (should backend)
      (should (plist-get backend :notification-only))
      (should (string-match-p "notifications only"
                              (cadr (should-error
                                     (funcall (plist-get backend :fetch-issue) nil 1))))))))

(ert-deftest test-external-an-rss-entry-still-opens-in-the-browser ()
  "GIVEN an entry of an RSS feed among the notifications
WHEN it is opened in the notifications buffer
THEN the browser is asked for its link, as before the pushed
notifications came."
  (test-external--fresh
    (shipit--merge-backend-notifications
     (list (shipit-rss--entry-to-activity
            '(:url "https://blog.example/feed" :name "Dev Blog")
            '((title . "New Release") (link . "https://blog.example/new") (guid . "g1")
              (published . "2026-03-10T12:00:00Z") (summary . "Notes")))))
    (let ((asked nil)
          (buffer (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buffer
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (goto-char (prop-match-beginning
                        (text-property-search-forward 'shipit-notification-type "rss" #'equal)))
            (should-not (string-match-p "#" (test-external--line)))
            (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq asked url))))
              (shipit-notifications-buffer-open)))
        (kill-buffer buffer))
      (should (equal "https://blog.example/new" asked)))))

(provide 'test-shipit-notifications-external)
;;; test-shipit-notifications-external.el ends here
