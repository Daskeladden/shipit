;;; test-shipit-notifications-confluence-cases.el --- More cases of the Confluence notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The cases of the notifications of Confluence that the scenarios of
;; `test-shipit-notifications-confluence.el' leave out: what each search
;; asks for, what a notification holds, a site that fails, and a read
;; notification that comes back when its page changes.  The answers of
;; Confluence and the helpers are those of that file.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'url-parse)
(require 'url-util)
(require 'test-shipit-notifications-confluence)

;;; A fresh shipit

(defmacro test-confluence-cases--keeping-no-revisions (&rest body)
  "Run BODY with no revision kept of a notification that was read."
  `(let ((shipit--locally-marked-read-revisions (make-hash-table :test 'equal)))
     ,@body))

(defmacro test-confluence-cases--fresh (answers &rest body)
  "Run BODY as `test-confluence--fresh' does, with Confluence giving ANSWERS.
No revision of a notification that was read is kept from before."
  (declare (indent 1))
  `(test-confluence-cases--keeping-no-revisions
    (test-confluence--fresh ,answers ,@body)))

(defmacro test-confluence-cases--with-pages (mentioned watched &rest body)
  "Run BODY as `test-confluence--with-pages' does, with MENTIONED and WATCHED.
No revision of a notification that was read is kept from before."
  (declare (indent 2))
  `(test-confluence-cases--keeping-no-revisions
    (test-confluence--with-pages ,mentioned ,watched ,@body)))

;;; What Confluence was asked

(defun test-confluence-cases--query (address)
  "The query of ADDRESS, decoded, as a list of (NAME VALUE)."
  (url-parse-query-string (cdr (url-path-and-query (url-generic-parse-url address)))))

(defun test-confluence-cases--asked (name)
  "The value of NAME in the query of each search, oldest first."
  (mapcar (lambda (search)
            (cadr (assoc name (test-confluence-cases--query (car search)))))
          (test-confluence--searches)))

(defun test-confluence-cases--asked-who-you-are ()
  "How many times Confluence was asked who the user is."
  (cl-count-if (lambda (request)
                 (string-match-p (car test-confluence--current-user) (cadr request)))
               shipit-test-http-requests))

(defun test-confluence-cases--minutes-ago (minutes)
  "The time MINUTES ago, as shipit keeps the time of its last poll."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract nil (* 60 minutes)) t))

;;; Answers of Confluence

(defun test-confluence-cases--put (alist key value)
  "A copy of ALIST in which KEY has VALUE."
  (cons (cons key value) (assq-delete-all key (copy-alist alist))))

(defun test-confluence-cases--put-in (alist keys value)
  "A copy of ALIST, nested alists, in which the path KEYS has VALUE.
ALIST itself stays as it is: parts of it can be shared by each page."
  (test-confluence-cases--put
   alist (car keys)
   (if (cdr keys)
       (test-confluence-cases--put-in (alist-get (car keys) alist) (cdr keys) value)
     value)))

(defun test-confluence-cases--refused (path)
  "The answer of Confluence when it refuses each request of PATH."
  (list path 401 test-confluence--refusal))

(defun test-confluence-cases--mentions-answer (answer)
  "Answer the search for the pages that mention the user with ANSWER.
The search for the pages that the user watches finds none, and the user
is known."
  (list (list (concat test-confluence--search-path ".*mention") 200 answer)
        (test-confluence--search "watcher" #'ignore)
        test-confluence--current-user))

(defun test-confluence-cases--without-base (pages)
  "The answer of a search that finds PAGES and does not say its base address."
  (assq-delete-all '_links (copy-alist (test-confluence--search-answer pages))))

;;; Asking Confluence directly

(defun test-confluence-cases--ask-directly ()
  "Ask the source of Confluence for its notifications, outside a poll.
No poll of shipit is around the request, so nothing catches an error.
Gives each list of notifications that the source called back with."
  (let ((calls nil)
        (fetch (plist-get (cdr (assq 'confluence shipit-issue-backends))
                          :notifications-async)))
    (funcall fetch (cdr test-confluence--entry) nil
             (lambda (activities) (push activities calls)))
    calls))

;;; What the searches ask for

(ert-deftest test-confluence-cases-the-first-poll-looks-back-one-hour ()
  "GIVEN no poll of Confluence before in this session
WHEN shipit polls
THEN each search asks for the pages that changed in the last 60 minutes."
  (test-confluence-cases--with-pages nil nil
    (test-confluence--poll)
    (let ((queries (test-confluence-cases--asked "cql")))
      (should (= 2 (length queries)))
      (dolist (query queries)
        (should (string-search "lastmodified >= now(\"-60m\")" query))))))

(ert-deftest test-confluence-cases-a-later-poll-looks-back-to-the-poll-before ()
  "GIVEN a poll of Confluence nine and a half minutes ago
WHEN shipit polls again
THEN each search asks for the pages that changed in the last 15 minutes:
the minutes since the poll before, rounded up, and 5 more."
  (test-confluence-cases--with-pages nil nil
    (shipit--poll-one-backend test-confluence--entry
                              (test-confluence-cases--minutes-ago 9.5))
    (let ((queries (test-confluence-cases--asked "cql")))
      (should (= 2 (length queries)))
      (dolist (query queries)
        (should (string-search "lastmodified >= now(\"-15m\")" query))))))

(ert-deftest test-confluence-cases-the-searches-ask-for-pages-and-blog-posts ()
  "GIVEN the Confluence site
WHEN shipit polls
THEN it searches once for the pages that mention you and once for the
pages that you watch, AND each search asks for pages and blog posts,
AND for the version and the space of each page."
  (test-confluence-cases--with-pages nil nil
    (test-confluence--poll)
    (let ((queries (test-confluence-cases--asked "cql")))
      (should (= 1 (cl-count-if (lambda (query) (string-search "mention = currentUser()" query))
                                queries)))
      (should (= 1 (cl-count-if (lambda (query) (string-search "watcher = currentUser()" query))
                                queries)))
      (dolist (query queries)
        (should (string-search "type in (page, blogpost)" query))))
    (dolist (expand (test-confluence-cases--asked "expand"))
      (should (string-search "content.version" expand))
      (should (string-search "content.space" expand)))))

;;; Who the user is

(ert-deftest test-confluence-cases-two-polls-ask-who-you-are-once ()
  "GIVEN the Confluence site
WHEN shipit polls twice
THEN Confluence was asked who you are once."
  (test-confluence-cases--with-pages nil nil
    (test-confluence--poll)
    (test-confluence--poll)
    (should (= 1 (test-confluence-cases--asked-who-you-are)))))

(ert-deftest test-confluence-cases-pages-come-when-who-you-are-is-unknown ()
  "GIVEN a Confluence site that refuses to say who you are, AND the page
Coffee menu, which mentions you and which alice changed
WHEN shipit polls
THEN the page is a notification with the reason mention
WHEN shipit polls again
THEN Confluence is asked again who you are."
  (test-confluence-cases--fresh
      (list (test-confluence--search
             "mention" (lambda () (list (test-confluence--coffee-menu 7 "alice"))))
            (test-confluence--search "watcher" #'ignore)
            (test-confluence-cases--refused (car test-confluence--current-user)))
    (test-confluence--poll)
    (should (equal '(("Coffee menu" "mention")) (test-confluence--notifications)))
    (test-confluence--poll)
    (should (= 2 (test-confluence-cases--asked-who-you-are)))))

;;; A site that fails

(ert-deftest test-confluence-cases-a-refusing-site-calls-back-once-with-nothing ()
  "GIVEN a Confluence site that refuses each request
WHEN the source of Confluence is asked for its notifications, with no
poll around it that catches an error
THEN it signals nothing, AND it calls back once, with no notification."
  (test-confluence-cases--fresh
      (list (test-confluence-cases--refused test-confluence--search-path)
            (test-confluence-cases--refused (car test-confluence--current-user)))
    (should (equal '(nil) (test-confluence-cases--ask-directly)))
    (should (test-confluence--searches))))

(ert-deftest test-confluence-cases-a-site-without-a-login-calls-back-once-with-nothing ()
  "GIVEN a Confluence site for which auth-source holds no login
WHEN the source of Confluence is asked for its notifications, with no
poll around it that catches an error
THEN it signals nothing, AND it calls back once, with no notification,
AND nothing was sent to the site."
  (test-confluence-cases--fresh nil
    (cl-letf (((symbol-function 'auth-source-search) #'ignore))
      (should (equal '(nil) (test-confluence-cases--ask-directly))))
    (should-not shipit-test-http-requests)))

(ert-deftest test-confluence-cases-an-answer-that-is-no-json-gives-nothing ()
  "GIVEN a Confluence site that answers each request with a web page, and
not with JSON
WHEN the source of Confluence is asked for its notifications, with no
poll around it that catches an error
THEN it signals nothing, AND it calls back once, with no notification."
  (test-confluence-cases--fresh
      (list (list "/wiki/" 200 "<html><body>Log in</body></html>"))
    (should (equal '(nil) (test-confluence-cases--ask-directly)))))

(ert-deftest test-confluence-cases-an-answer-of-another-shape-gives-nothing ()
  "GIVEN a Confluence site whose search answers with JSON that holds no
pages, but text where the pages should be
WHEN the source of Confluence is asked for its notifications, with no
poll around it that catches an error
THEN it signals nothing, AND it calls back once, with no notification."
  (test-confluence-cases--fresh
      (test-confluence-cases--mentions-answer '((results . ["not a page"])))
    (should (equal '(nil) (test-confluence-cases--ask-directly)))))

;;; What a notification holds

(ert-deftest test-confluence-cases-the-text-is-the-message-then-the-excerpt ()
  "GIVEN the page Coffee menu, whose newest change has the message
Change 7, and whose excerpt is The text of Coffee menu
WHEN shipit polls
THEN the text of its notification is the message, an empty line, and
the excerpt."
  (test-confluence-cases--with-pages (list (test-confluence--coffee-menu 7 "alice")) nil
    (test-confluence--poll)
    (should (equal "Change 7\n\nThe text of Coffee menu"
                   (alist-get 'description (test-confluence--the-one))))))

(ert-deftest test-confluence-cases-a-change-without-a-message-has-the-excerpt-alone ()
  "GIVEN the page Coffee menu, whose newest change has no message, AND
whose excerpt holds the marks with which the search shows the words
that it found
WHEN shipit polls
THEN the text of its notification is the excerpt alone, without the
marks."
  (let ((page (test-confluence-cases--put
               (test-confluence-cases--put-in (test-confluence--coffee-menu 7 "alice")
                                              '(content version message) "")
               'excerpt "The @@@hl@@@menu@@@endhl@@@ of the week")))
    (test-confluence-cases--with-pages (list page) nil
      (test-confluence--poll)
      (should (equal "The menu of the week"
                     (alist-get 'description (test-confluence--the-one)))))))

(ert-deftest test-confluence-cases-the-address-without-the-base-of-the-answer ()
  "GIVEN a search answer that does not say the address that the paths of
its pages start from, AND the page Coffee menu in it
WHEN shipit polls the site, given with or without a slash at its end
THEN the address of the notification is the page in the wiki of the
site."
  (dolist (site '("https://example.atlassian.net" "https://example.atlassian.net/"))
    (test-confluence-cases--fresh
        (test-confluence-cases--mentions-answer
         (test-confluence-cases--without-base (list (test-confluence--coffee-menu 7 "alice"))))
      (let ((shipit-issue-repo-backends
             (list (list "confluence" :backend 'confluence :base-url site))))
        (test-confluence--poll))
      (should (equal test-confluence--coffee-menu-address
                     (alist-get 'browse-url (test-confluence--the-one)))))))

;;; A read page that changes

(ert-deftest test-confluence-cases-your-own-change-comes-back-when-bob-changes-it ()
  "GIVEN the page Cup inventory, which you watch, whose last change is
your own, so that its notification is not unread
WHEN bob changes the page, and shipit polls
THEN the notification of the page is back, unread, with the reason
subscribed and bob as the one who changed it."
  (let ((watched (list (test-confluence--cup-inventory 7 "me"))))
    (test-confluence-cases--with-pages nil watched
      (test-confluence--poll)
      (should (= 0 (shipit--count-unread-activities)))
      (setq watched (list (test-confluence--cup-inventory 8 "bob")))
      (test-confluence--poll)
      (should (equal '(("Cup inventory" "subscribed")) (test-confluence--notifications)))
      (should (equal "bob" (alist-get 'author (test-confluence--the-one))))
      (should (= 1 (shipit--count-unread-activities))))))

(ert-deftest test-confluence-cases-a-page-opened-with-return-comes-back-when-it-changes ()
  "GIVEN the page Coffee menu, which mentions you, opened with RET from
the notifications buffer
WHEN shipit polls and the page has not changed
THEN there is no notification
WHEN alice changes the page again, and shipit polls
THEN the notification of the page is back, unread."
  (let ((mentioned (list (test-confluence--coffee-menu 7 "alice"))))
    (test-confluence-cases--with-pages mentioned nil
      (test-confluence--poll)
      (test-confluence--press-return "Coffee menu")
      (test-confluence--poll)
      (should-not (test-confluence--notifications))
      (setq mentioned (list (test-confluence--coffee-menu 8 "alice")))
      (test-confluence--poll)
      (should (equal '(("Coffee menu" "mention")) (test-confluence--notifications)))
      (should (= 1 (shipit--count-unread-activities))))))

;;; The rule for each backend: a read notification with a revision

(defun test-confluence-cases--coffee-machine (&optional revision)
  "The notification of the issue Coffee machine, from no Confluence site.
It has REVISION, or no revision when REVISION is nil."
  `((repo . "Kitchen") (number . 7) (type . "issue")
    (subject . "Coffee machine") (reason . "subscribed")
    ,@(when revision `((revision . ,revision)))))

(defconst test-confluence-cases--ways-to-mark-coffee-machine-read
  (list (lambda () (shipit--mark-notification-read 7 "Kitchen" t "issue"))
        (lambda () (shipit--mark-notification-read-async 7 "Kitchen" "issue")))
  "The two ways in which shipit marks the issue Coffee machine as read.
The first is that of RET and of the menu of a row, the second that of
the mark command of the notifications buffer and of the rules that mark
notifications as read.")

(ert-deftest test-confluence-cases-a-read-notification-with-a-revision-comes-back ()
  "GIVEN the notification of the issue Coffee machine at revision 3, of a
backend that is not Confluence, marked as read in either way
WHEN a poll brings it again at revision 3
THEN there is no notification
WHEN a poll brings it at revision 4
THEN the notification is back, unread."
  (dolist (mark-read test-confluence-cases--ways-to-mark-coffee-machine-read)
    (test-confluence-cases--fresh nil
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine 3)))
      (funcall mark-read)
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine 3)))
      (should-not (test-confluence--notifications))
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine 4)))
      (should (equal '(("Coffee machine" "subscribed")) (test-confluence--notifications)))
      (should (= 1 (shipit--count-unread-activities))))))

(ert-deftest test-confluence-cases-a-read-notification-without-a-revision-stays-away ()
  "GIVEN the notification of the issue Coffee machine without a revision,
of a backend that is not Confluence, marked as read in either way
WHEN polls bring it again
THEN there is no notification, as before revisions were known."
  (dolist (mark-read test-confluence-cases--ways-to-mark-coffee-machine-read)
    (test-confluence-cases--fresh nil
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine)))
      (funcall mark-read)
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine)))
      (shipit--merge-backend-notifications (list (test-confluence-cases--coffee-machine)))
      (should-not (test-confluence--notifications)))))

(ert-deftest test-confluence-cases-clearing-the-read-marks-brings-a-page-back ()
  "GIVEN the page Cup inventory, which you watch, marked as read
WHEN the marks of read notifications are cleared for another repository
THEN the page stays away at the next poll
WHEN they are cleared for the space Kitchen, or all of them, and shipit
polls
THEN the notification of the page is back, unread, AND no revision of a
read notification is kept."
  (dolist (clear '(("Kitchen") ()))
    (let ((shipit--backend-last-poll-time nil))
      (test-confluence-cases--with-pages nil (list (test-confluence--cup-inventory 7 "bob"))
        (test-confluence--poll)
        (test-confluence--mark-read "Cup inventory")
        (shipit-notifications-clear-locally-marked-read "Garden")
        (test-confluence--poll)
        (should-not (test-confluence--notifications))
        (apply #'shipit-notifications-clear-locally-marked-read clear)
        (should (zerop (hash-table-count shipit--locally-marked-read-revisions)))
        (test-confluence--poll)
        (should (equal '(("Cup inventory" "subscribed")) (test-confluence--notifications)))
        (should (= 1 (shipit--count-unread-activities)))))))

;;; The row in the notifications buffer

(defun test-confluence-cases--row-has-image-p ()
  "Whether the row at point has an image."
  (cl-some (lambda (pos) (imagep (get-text-property pos 'display)))
           (number-sequence (line-beginning-position) (line-end-position))))

(defun test-confluence-cases--icons-of-the-row (title)
  "Draw the buffer with SVG icons; the icons that the row of TITLE asked for.
It is nil when the row of the page has no image."
  (let ((asked nil))
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
        (test-confluence--in-buffer title
          (and (test-confluence-cases--row-has-image-p) asked))))))

(ert-deftest test-confluence-cases-the-row-has-the-icons-of-a-page-with-svg ()
  "GIVEN the page Coffee menu, which mentions you, AND SVG icons
WHEN the notifications buffer shows it
THEN its row has images: the icon of Confluence for where it comes from
and the file octicon for what it is."
  (test-confluence-cases--with-pages (list (test-confluence--coffee-menu 7 "alice")) nil
    (test-confluence--poll)
    (let ((icons (test-confluence-cases--icons-of-the-row "Coffee menu")))
      (should (member "confluence" icons))
      (should (member "file" icons)))))

(ert-deftest test-confluence-cases-the-row-has-letters-without-svg ()
  "GIVEN the page Coffee menu, which mentions you, AND no SVG icons
WHEN the notifications buffer shows it
THEN its row starts with letters for where it comes from and for what
it is, then the space and the title, AND it has no image and no letters
of an unknown kind."
  (test-confluence-cases--with-pages (list (test-confluence--coffee-menu 7 "alice")) nil
    (test-confluence--poll)
    (let ((shipit-use-svglib-icons nil))
      (test-confluence--in-buffer "Coffee menu"
        (should (string-match-p "CF +CF +Kitchen +Coffee menu" (test-confluence--line)))
        (should-not (string-search "??" (test-confluence--line)))
        (should-not (test-confluence-cases--row-has-image-p))))))

;;; Loading

(ert-deftest test-confluence-cases-shipit-knows-what-to-load-for-the-backend ()
  "GIVEN an entry of the backends option with the backend confluence
THEN shipit knows the feature that it loads for the entry, AND that
feature registers Confluence as a source of notifications only."
  (let ((feature (cdr (assq 'confluence shipit-issue-backend-features))))
    (should (eq 'shipit-notifications-confluence feature))
    (require feature)
    (let ((backend (cdr (assq 'confluence shipit-issue-backends))))
      (should (equal "Confluence" (plist-get backend :name)))
      (should (plist-get backend :notification-only)))))

(provide 'test-shipit-notifications-confluence-cases)
;;; test-shipit-notifications-confluence-cases.el ends here
