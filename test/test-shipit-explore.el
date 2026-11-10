;;; test-shipit-explore.el --- Tests for the feed of Explore -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the feed of what the people that the user follows do, as a
;; backend gives it, and for the notification of what is new there.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-explore)
;; The notification of new entries loads the buffer of Explore when it
;; is opened, which would take the place of what a test put there.
(require 'shipit-explore-buffer)
(require 'shipit-pr-github)
(require 'shipit-notifications-external)
(require 'shipit-notifications-buffer)
(require 'shipit-test-http)

(defun test-explore--event (id type actor repo payload created)
  "An event of GitHub: ID, TYPE, by ACTOR, in REPO, with PAYLOAD, at CREATED."
  `((id . ,id)
    (type . ,type)
    (actor . ((login . ,actor)))
    (repo . ((name . ,repo)))
    (payload . ,payload)
    (created_at . ,created)))

(defun test-explore--events ()
  "The events of a feed, the newest first, as GitHub gives them.
One of each kind that is shown, and a push and a new branch, which are
not."
  (list
   (test-explore--event "7" "PushEvent" "alice" "example/tool" '((size . 1))
                        "2026-10-01T10:07:00Z")
   (test-explore--event "6" "CreateEvent" "carol" "carol/new-thing"
                        '((ref_type . "branch") (ref . "main")) "2026-10-01T10:06:00Z")
   (test-explore--event "5" "PublicEvent" "dave" "dave/once-private" nil
                        "2026-10-01T10:05:00Z")
   (test-explore--event
    "4" "ReleaseEvent" "alice" "example/tool"
    '((action . "published")
      (release . ((tag_name . "v1.2.0")
                  (name . "The new one")
                  (html_url . "https://github.com/example/tool/releases/tag/v1.2.0"))))
    "2026-10-01T10:04:00Z")
   (test-explore--event "3" "CreateEvent" "carol" "carol/new-thing"
                        '((ref_type . "repository")) "2026-10-01T10:03:00Z")
   (test-explore--event "2" "ForkEvent" "bob" "example/tool"
                        '((forkee . ((full_name . "bob/tool")))) "2026-10-01T10:02:00Z")
   (test-explore--event "1" "WatchEvent" "alice" "example/tool" '((action . "started"))
                        "2026-10-01T10:01:00Z")))

(defconst test-explore--user '("/user\\'" 200 ((login . "the-reader")))
  "The answer of GitHub to who the user is.")

(defun test-explore--answers (&optional events)
  "The answers of GitHub: who the user is, and the feed with EVENTS."
  (list test-explore--user
        (list "/users/the-reader/received_events" 200
              (vconcat (or events (test-explore--events))))))

(defun test-explore--page-answer (feed page events)
  "The answer of GitHub to PAGE of FEED, holding EVENTS.
FEED is \"\" for the feed of every repository and \"/public\" for that of
the public events."
  (list (format "/users/the-reader/received_events%s\\?per_page=100&page=%d\\'" feed page)
        200 (vconcat events)))

(defun test-explore--paged-answers (pages public-pages)
  "The answers of GitHub: who the user is, and the two feeds page by page.
PAGES holds the events of each page of the feed of every repository, and
PUBLIC-PAGES those of each page of the feed of the public events."
  (cons test-explore--user
        (append (cl-loop for events in pages for page from 1
                         collect (test-explore--page-answer "" page events))
                (cl-loop for events in public-pages for page from 1
                         collect (test-explore--page-answer "/public" page events)))))

(defun test-explore--asked-of-the-feed ()
  "What was asked of the feed, each address from its last part on, sorted."
  (sort (cl-loop for (_method address) in shipit-test-http-requests
                 when (string-match "/users/the-reader/\\(received_events.*\\)" address)
                 collect (match-string 1 address))
        #'string<))

(defmacro test-explore--fresh (answers &rest body)
  "Run BODY with nothing seen or notified yet, and GitHub giving ANSWERS.
No notification is kept, and what was seen is kept in no file."
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
         (shipit-pr-github--login nil)
         (shipit-explore-seen-file nil)
         (shipit-explore--seen nil)
         (shipit-explore--notified nil)
         (shipit-external--read nil)
         (shipit-explore-kinds '("star" "fork" "create" "release" "public"))
         (shipit-explore-about 'all)
         (shipit-explore-notify t))
     (shipit-test-http-answering ,answers
       ,@body)))

(defun test-explore--fetch ()
  "The entries of the feed, fetched without waiting; the answer comes at once."
  (let ((given 'nothing))
    (shipit-explore-fetch-async (lambda (entries) (setq given entries)))
    (should-not (eq given 'nothing))
    given))

(defun test-explore--told (entries)
  "ENTRIES as lists of kind, who did it, repository and detail."
  (mapcar (lambda (entry)
            (mapcar (lambda (key) (cdr (assq key entry))) '(kind actor repo detail)))
          entries))

(defun test-explore--notifications ()
  "The notifications that shipit holds, each as its source and its title."
  (mapcar (lambda (activity)
            (list (cdr (assq 'repo activity)) (cdr (assq 'subject activity))))
          (hash-table-values shipit--notification-pr-activities)))

;;; The feed of a backend

(ert-deftest test-explore-the-feed-of-github-becomes-entries ()
  "GIVEN GitHub answers the feed with a public repository, a release, a
new repository, a fork and a star, AND a push and a new branch
WHEN the feed is fetched
THEN it gives the five as entries, the newest first, each with what it
is, who did it, the repository and a detail: the fork, or the tag, AND
the push and the branch are left out."
  (test-explore--fresh (test-explore--answers)
    (should (equal (test-explore--told (test-explore--fetch))
                   '(("public" "dave" "dave/once-private" nil)
                     ("release" "alice" "example/tool" "v1.2.0")
                     ("create" "carol" "carol/new-thing" nil)
                     ("fork" "bob" "example/tool" "bob/tool")
                     ("star" "alice" "example/tool" nil))))))

(ert-deftest test-explore-the-newest-entry-comes-first-whatever-the-order-of-github ()
  "GIVEN GitHub answers the feed with its events in no order of time, as
it does for a feed that mixes the events of many repositories
WHEN the feed is fetched
THEN the entries come with the newest first."
  (test-explore--fresh
      (test-explore--answers (let ((events (test-explore--events)))
                               (append (last events 3) (butlast events 3))))
    (should (equal (mapcar (lambda (entry) (cdr (assq 'kind entry))) (test-explore--fetch))
                   '("public" "release" "create" "fork" "star")))))

(ert-deftest test-explore-an-entry-knows-its-address-and-its-time ()
  "GIVEN the feed with a star and a release
WHEN the feed is fetched
THEN the star has the address of the repository, AND the release that of
its page and its name, AND each has the id and the time of its event."
  (test-explore--fresh (test-explore--answers)
    (let* ((entries (test-explore--fetch))
           (release (nth 1 entries))
           (star (nth 4 entries)))
      (should (equal "https://github.com/example/tool" (cdr (assq 'url star))))
      (should (equal "1" (cdr (assq 'id star))))
      (should (equal "2026-10-01T10:01:00Z" (cdr (assq 'time star))))
      (should (equal "https://github.com/example/tool/releases/tag/v1.2.0"
                     (cdr (assq 'url release))))
      (should (equal "The new one" (cdr (assq 'title release)))))))

(ert-deftest test-explore-asks-for-the-feed-of-the-user ()
  "GIVEN a user that GitHub knows as the-reader
WHEN the feed is fetched
THEN GitHub is asked for the three pages of the events that this user
received, AND for the three pages of the public ones
WHEN the feed is fetched again
THEN GitHub is asked for the six again, AND was asked who the user is
once."
  (test-explore--fresh (test-explore--answers)
    (test-explore--fetch)
    (should (equal (test-explore--asked-of-the-feed)
                   '("received_events/public?per_page=100&page=1"
                     "received_events/public?per_page=100&page=2"
                     "received_events/public?per_page=100&page=3"
                     "received_events?per_page=100&page=1"
                     "received_events?per_page=100&page=2"
                     "received_events?per_page=100&page=3")))
    (test-explore--fetch)
    (should (= 12 (length (test-explore--asked-of-the-feed))))
    (should (= 1 (cl-count-if (lambda (address) (string-suffix-p "/user" address))
                              (mapcar #'cadr shipit-test-http-requests))))))

(ert-deftest test-explore-reads-every-page-of-the-feed ()
  "GIVEN a feed whose first page holds only a push, as when the work of a
busy repository fills it, AND a star on its second page and a fork on
its third
WHEN the feed is fetched
THEN the fork and the star are entries."
  (let ((events (test-explore--events)))
    (test-explore--fresh
        (test-explore--paged-answers
         (list (list (nth 0 events)) (list (nth 6 events)) (list (nth 5 events)))
         '(nil nil nil))
      (should (equal (test-explore--told (test-explore--fetch))
                     '(("fork" "bob" "example/tool" "bob/tool")
                       ("star" "alice" "example/tool" nil)))))))

(ert-deftest test-explore-reads-the-feed-of-the-public-events-too ()
  "GIVEN a feed that holds only a push on each of its pages, AND the feed
of the public events, which reaches further back, with a star on its
third page
WHEN the feed is fetched
THEN the star is an entry."
  (let* ((events (test-explore--events))
         (push (list (nth 0 events))))
    (test-explore--fresh
        (test-explore--paged-answers (list push push push)
                                     (list nil nil (list (nth 6 events))))
      (should (equal (test-explore--told (test-explore--fetch))
                     '(("star" "alice" "example/tool" nil)))))))

(ert-deftest test-explore-an-event-of-both-feeds-is-one-entry ()
  "GIVEN a star that both the feed and the feed of the public events hold
WHEN the feed is fetched
THEN the star is one entry."
  (let ((star (list (nth 6 (test-explore--events)))))
    (test-explore--fresh
        (test-explore--paged-answers (list star nil nil) (list star nil nil))
      (should (equal (test-explore--told (test-explore--fetch))
                     '(("star" "alice" "example/tool" nil)))))))

(ert-deftest test-explore-a-refused-page-leaves-the-entries-of-the-others ()
  "GIVEN GitHub refuses the third page of the feed, AND gives a star on
the first
WHEN the feed is fetched
THEN the star is an entry."
  (let ((star (list (nth 6 (test-explore--events)))))
    (test-explore--fresh
        (cons '("/received_events\\?per_page=100&page=3\\'" 422 ((message . "No")))
              (test-explore--paged-answers (list star nil) '(nil nil nil)))
      (should (equal (test-explore--told (test-explore--fetch))
                     '(("star" "alice" "example/tool" nil)))))))

(ert-deftest test-explore-asks-for-each-page-itself ()
  "GIVEN url.el, which keeps a copy of each answer on disk and then asks
only for what changed since, to which GitHub answers with no page at
all when nothing did
WHEN the feed is fetched
THEN each request for a page says that no copy is to be used, so that
GitHub gives the page itself."
  (test-explore--fresh (test-explore--answers)
    (let ((answer (symbol-function 'url-retrieve))
          (pragmas nil))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url &rest arguments)
                   (when (string-match-p "received_events" (shipit-test-http--address url))
                     (push (cdr (assoc "Pragma" url-request-extra-headers)) pragmas))
                   (apply answer url arguments))))
        (test-explore--fetch))
      (should (equal pragmas (make-list 6 "no-cache"))))))

(ert-deftest test-explore-keeps-no-page-of-the-feed ()
  "GIVEN a feed
WHEN it is fetched
THEN shipit keeps no page of it for a later request, as it keeps the
answers of most requests: a page of a feed is large, and changes with
each event."
  (test-explore--fresh (test-explore--answers)
    (test-explore--fetch)
    (should-not (seq-some (lambda (key) (string-match-p "received_events" key))
                          (hash-table-keys shipit-gh-etag--persistent-cache)))))

(ert-deftest test-explore-shows-only-the-kinds-that-are-asked-for ()
  "GIVEN a setting that asks for releases and stars only
WHEN the feed is fetched
THEN it gives the release and the star."
  (test-explore--fresh (test-explore--answers)
    (let ((shipit-explore-kinds '("release" "star")))
      (should (equal (mapcar (lambda (entry) (cdr (assq 'kind entry))) (test-explore--fetch))
                     '("release" "star"))))))

(defun test-explore--events-with-own ()
  "The events of the feed, and before them a star on a repository of the user."
  (cons (test-explore--event "8" "WatchEvent" "frank" "the-reader/thing"
                             '((action . "started")) "2026-10-01T10:08:00Z")
        (test-explore--events)))

(ert-deftest test-explore-an-entry-says-whether-the-repository-is-the-users ()
  "GIVEN a feed with a star on a repository of the user, and entries about
the repositories of others
WHEN the feed is fetched
THEN the star on the repository of the user says that it is the user's
own, AND no other entry does."
  (test-explore--fresh (test-explore--answers (test-explore--events-with-own))
    (should (equal (mapcar (lambda (entry) (and (cdr (assq 'own entry)) t))
                           (test-explore--fetch))
                   '(t nil nil nil nil nil)))))

(ert-deftest test-explore-shows-what-the-setting-is-about ()
  "GIVEN a feed with one entry about a repository of the user and five
about the repositories of others
WHEN the setting asks for the user's own repositories
THEN the feed gives the one
WHEN it asks for what the people that the user follows do elsewhere
THEN the feed gives the five
WHEN it asks for all
THEN the feed gives the six."
  (test-explore--fresh (test-explore--answers (test-explore--events-with-own))
    (let ((shipit-explore-about 'mine))
      (should (equal (test-explore--told (test-explore--fetch))
                     '(("star" "frank" "the-reader/thing" nil)))))
    (let ((shipit-explore-about 'others))
      (should (= 5 (length (test-explore--fetch)))))
    (let ((shipit-explore-about 'all))
      (should (= 6 (length (test-explore--fetch)))))))

(ert-deftest test-explore-a-refusal-of-github-gives-no-entry ()
  "GIVEN GitHub refuses the feed
WHEN the feed is fetched
THEN it gives no entry, AND that is no error."
  (test-explore--fresh
      (list test-explore--user
            '("/users/the-reader/received_events" 403 ((message . "No"))))
    (should-not (test-explore--fetch))))

;;; What was seen

(ert-deftest test-explore-what-was-seen-is-new-no-more ()
  "GIVEN the entries of a feed, of which none was seen
THEN all of them are new
WHEN the three oldest are marked as seen
THEN the two after them are new
WHEN all are marked as seen
THEN none is new."
  (test-explore--fresh (test-explore--answers)
    (let ((entries (test-explore--fetch)))
      (should (= 5 (length (shipit-explore-unseen entries))))
      (shipit-explore-mark-seen (last entries 3))
      (should (equal (mapcar (lambda (entry) (cdr (assq 'kind entry)))
                             (shipit-explore-unseen entries))
                     '("public" "release")))
      (shipit-explore-mark-seen entries)
      (should-not (shipit-explore-unseen entries)))))

(ert-deftest test-explore-what-was-seen-is-kept-in-a-file ()
  "GIVEN a file for what was seen, and all entries marked as seen
WHEN another session of Emacs reads the file
THEN none of the entries is new there."
  (let ((file (make-temp-file "shipit-explore-seen-")))
    (unwind-protect
        (test-explore--fresh (test-explore--answers)
          (let ((shipit-explore-seen-file file)
                (entries (test-explore--fetch)))
            (shipit-explore-mark-seen entries)
            (setq shipit-explore--seen nil)
            (should-not (shipit-explore-unseen entries))))
      (delete-file file))))

;;; The notification of what is new

(ert-deftest test-explore-new-entries-are-one-notification ()
  "GIVEN a feed with five entries that were not seen
WHEN shipit polls for notifications
THEN shipit holds one notification, from Explore, that says how many are
new, AND it is counted as unread
WHEN shipit polls again and the feed is the same
THEN it is still that one notification."
  (test-explore--fresh (test-explore--answers)
    (shipit-explore--check)
    (should (equal (test-explore--notifications) '(("Explore" "5 new in the feed"))))
    (should (= 1 (shipit--count-unread-activities)))
    (shipit-explore--check)
    (should (equal (test-explore--notifications) '(("Explore" "5 new in the feed"))))))

(ert-deftest test-explore-the-notification-says-what-is-new ()
  "GIVEN a feed with entries that were not seen
WHEN shipit polls for notifications
THEN the text of the notification names each entry: who did what."
  (test-explore--fresh (test-explore--answers)
    (shipit-explore--check)
    (let ((text (cdr (assq 'description
                           (car (hash-table-values shipit--notification-pr-activities))))))
      (should (string-match-p "alice starred example/tool" text))
      (should (string-match-p "bob forked example/tool to bob/tool" text))
      (should (string-match-p "alice released v1.2.0 of example/tool" text)))))

(ert-deftest test-explore-a-notification-that-is-marked-read-stays-away ()
  "GIVEN the notification of five new entries, marked as read
WHEN shipit polls again and the feed is the same
THEN no notification comes back
WHEN a newer star is in the feed at the next poll
THEN one notification says that six are new: the five were never looked
at in Explore."
  (test-explore--fresh nil
    (shipit-test-http-answering (test-explore--answers)
      (shipit-explore--check)
      (let ((activity (car (hash-table-values shipit--notification-pr-activities))))
        (shipit--mark-notification-read (cdr (assq 'number activity)) "Explore" t "external"))
      (should-not (test-explore--notifications))
      (shipit-explore--check)
      (should-not (test-explore--notifications)))
    (shipit-test-http-answering
        (test-explore--answers
         (cons (test-explore--event "9" "WatchEvent" "erin" "example/other"
                                    '((action . "started")) "2026-10-01T11:00:00Z")
               (test-explore--events)))
      (shipit-explore--check)
      (should (equal (test-explore--notifications) '(("Explore" "6 new in the feed")))))))

(ert-deftest test-explore-a-newer-entry-takes-the-place-of-the-notification ()
  "GIVEN the notification of five new entries, which is still unread
WHEN a newer star is in the feed at the next poll
THEN shipit holds one notification, of six, and not two."
  (test-explore--fresh nil
    (shipit-test-http-answering (test-explore--answers)
      (shipit-explore--check))
    (shipit-test-http-answering
        (test-explore--answers
         (cons (test-explore--event "9" "WatchEvent" "erin" "example/other"
                                    '((action . "started")) "2026-10-01T11:00:00Z")
               (test-explore--events)))
      (shipit-explore--check)
      (should (equal (test-explore--notifications) '(("Explore" "6 new in the feed")))))))

(ert-deftest test-explore-nothing-is-asked-when-the-notification-is-off ()
  "GIVEN the notification of new entries turned off, as it is at first
WHEN shipit polls for notifications
THEN GitHub is asked nothing, AND there is no notification."
  (should-not (default-value 'shipit-explore-notify))
  (test-explore--fresh nil
    (let ((shipit-explore-notify nil))
      (shipit-explore--check)
      (should-not shipit-test-http-requests)
      (should-not (test-explore--notifications)))))

(ert-deftest test-explore-what-was-seen-gives-no-notification ()
  "GIVEN a feed whose entries were all seen in Explore
WHEN shipit polls for notifications
THEN there is no notification."
  (test-explore--fresh (test-explore--answers)
    (shipit-explore-mark-seen (test-explore--fetch))
    (shipit-explore--check)
    (should-not (test-explore--notifications))))

(ert-deftest test-explore-the-poll-of-notifications-checks-the-feed ()
  "GIVEN shipit with Explore loaded
THEN the check of the feed is among what a poll of notifications runs
WHEN shipit polls its backends
THEN what stands on that hook runs."
  (should (memq #'shipit-explore--check shipit-notifications-poll-hook))
  (let ((ran 0)
        (shipit-issue-repo-backends nil))
    (let ((shipit-notifications-poll-hook (list (lambda () (cl-incf ran)))))
      (cl-letf (((symbol-function 'shipit--autodiscover-notification-backends) #'ignore))
        (shipit--poll-backend-notifications)))
    (should (= 1 ran))))

(ert-deftest test-explore-a-poll-function-that-fails-stops-no-other ()
  "GIVEN two functions on the hook of a poll: the first one fails, and the
second one counts its runs
WHEN shipit runs the hook of a poll
THEN no error is signalled, AND the second function ran once."
  (let* ((ran 0)
         (shipit-notifications-poll-hook
          (list (lambda () (error "The cup is broken"))
                (lambda () (cl-incf ran)))))
    (shipit--run-poll-hook)
    (should (= 1 ran))))

(ert-deftest test-explore-the-notification-opens-explore ()
  "GIVEN the notification of new entries in the notifications buffer
WHEN it is opened
THEN Explore opens, AND the notification is gone."
  (test-explore--fresh (test-explore--answers)
    (shipit-explore--check)
    (let ((opened 0)
          (buffer (shipit-notifications-buffer-create)))
      (unwind-protect
          (cl-letf (((symbol-function 'shipit-explore) (lambda () (cl-incf opened))))
            (with-current-buffer buffer
              (shipit-notifications-buffer--rerender)
              (goto-char (point-min))
              (should (re-search-forward "5 new in the feed" nil t))
              (shipit-notifications-buffer-open)))
        (kill-buffer buffer))
      (should (= 1 opened))
      (should-not (test-explore--notifications)))))

(provide 'test-shipit-explore)
;;; test-shipit-explore.el ends here
