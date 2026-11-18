;;; test-shipit-notifications-confluence.el --- Tests for Confluence notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the notifications of Confluence: a page that mentions the
;; user, or a page that the user watches, is a notification of shipit.
;; Confluence is answered at url.el, the login at auth-source and the
;; browser at `browse-url-browser-function'.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'browse-url)
(require 'text-property-search)
(require 'shipit-notifications)
(require 'shipit-notifications-buffer)
(require 'shipit-issue-backends)
(require 'shipit-test-http)
(require 'shipit-notifications-confluence nil t)

(defconst test-confluence--site "https://example.atlassian.net"
  "The address of the Confluence site.")

(defconst test-confluence--entry
  `("confluence" :backend confluence :base-url ,test-confluence--site)
  "The entry of `shipit-issue-repo-backends' that switches the source on.")

(defconst test-confluence--search-path "/wiki/rest/api/search"
  "The path of the search of Confluence.")

(defconst test-confluence--authorization
  (concat "Basic " (base64-encode-string "me@example.com:token" t))
  "The Authorization header that the login of the site gives.")

(defconst test-confluence--coffee-menu-address
  "https://example.atlassian.net/wiki/spaces/KITCHEN/pages/101/Coffee+menu"
  "The address of the page Coffee menu in the browser.")

(defconst test-confluence--current-user
  '("/wiki/rest/api/user/current" 200 ((accountId . "account-me") (displayName . "me")))
  "The answer of Confluence when it is asked who the user is.")

(defconst test-confluence--refusal
  '((code . 401) (message . "Client must be authenticated to access this resource."))
  "The body of the answer of Confluence when it refuses a request.")

;;; The answers of Confluence

(defun test-confluence--time-of-version (version)
  "The time when VERSION of a page was made, as Confluence writes a time.
Each version is a minute newer than the one before it, and none is older
than half an hour."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z"
                      (time-subtract nil (* 60 (- 30 version))) t))

(defun test-confluence--page (id title version author)
  "The page ID with TITLE in the space Kitchen, as the search gives it.
Its newest VERSION was made by AUTHOR, the name of a user."
  (let ((path (format "/spaces/KITCHEN/pages/%s/%s" id (string-replace " " "+" title)))
        (time (test-confluence--time-of-version version)))
    `((content . ((id . ,id)
                  (type . "page")
                  (title . ,title)
                  (space . ((key . "KITCHEN") (name . "Kitchen")))
                  (version . ((number . ,version)
                              (when . ,time)
                              (message . ,(format "Change %d" version))
                              (by . ((accountId . ,(concat "account-" author))
                                     (displayName . ,author)))))
                  (_links . ((webui . ,path)))))
      (title . ,title)
      (excerpt . ,(format "The text of %s" title))
      (url . ,path)
      (resultGlobalContainer . ((title . "Kitchen") (displayUrl . "/spaces/KITCHEN")))
      (lastModified . ,time))))

(defun test-confluence--coffee-menu (version author)
  "The page Coffee menu at VERSION, which AUTHOR made."
  (test-confluence--page "101" "Coffee menu" version author))

(defun test-confluence--cup-inventory (version author)
  "The page Cup inventory at VERSION, which AUTHOR made."
  (test-confluence--page "102" "Cup inventory" version author))

(defun test-confluence--search-answer (pages)
  "The answer of a search of Confluence that finds PAGES."
  `((results . ,(vconcat pages))
    (start . 0)
    (limit . 50)
    (size . ,(length pages))
    (totalSize . ,(length pages))
    (_links . ((base . ,(concat test-confluence--site "/wiki"))
               (context . "/wiki")))))

(defun test-confluence--search (word pages)
  "The answer to each search whose query holds WORD.
PAGES is a function that gives the pages found, at each search."
  (list (concat test-confluence--search-path ".*" word) 200
        (lambda (_sent) (test-confluence--search-answer (funcall pages)))))

(defun test-confluence--answers (mentioned watched)
  "The answers of Confluence: the searches, and who the user is.
MENTIONED and WATCHED are functions that give, at each search, the
pages that mention the user and the pages that the user watches."
  (list (test-confluence--search "mention" mentioned)
        (test-confluence--search "watcher" watched)
        test-confluence--current-user))

(defun test-confluence--credentials (&rest query)
  "The logins that auth-source holds for the host that QUERY names.
It holds one, for the Confluence site."
  (when (equal (plist-get query :host) "example.atlassian.net")
    (list (list :user "me@example.com" :secret (lambda () "token")))))

;;; A fresh shipit

(defun test-confluence--reset ()
  "Have the source of Confluence forget what an earlier test taught it."
  (when (fboundp 'shipit-confluence-reset)
    (shipit-confluence-reset)))

(defmacro test-confluence--fresh (answers &rest body)
  "Run BODY with no notification kept, and Confluence giving ANSWERS.
The Confluence site is the one entry of the backends option, its login
is in auth-source, and the rules that mark notifications as read are
the defaults."
  (declare (indent 1))
  `(let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
         (shipit--last-notification-count 0)
         (shipit--notification-count 0)
         (shipit--mention-count 0)
         (shipit--mention-prs nil)
         (shipit--modeline-string nil)
         (shipit-notifications-auto-mark-read-rules nil)
         (shipit-notifications-auto-mute-own-activity t)
         (shipit-issue-repo-backends (list test-confluence--entry)))
     (test-confluence--reset)
     (cl-letf (((symbol-function 'auth-source-search) #'test-confluence--credentials))
       (shipit-test-http-answering ,answers
         ,@body))))

(defmacro test-confluence--with-pages (mentioned watched &rest body)
  "Run BODY fresh, with Confluence finding the pages MENTIONED and WATCHED.
MENTIONED are the pages that mention the user, and WATCHED the pages
that the user watches.  Both are forms that are evaluated at each
search, so a change to a variable there changes what Confluence finds
from then on."
  (declare (indent 2))
  `(test-confluence--fresh
       (test-confluence--answers (lambda () ,mentioned) (lambda () ,watched))
     ,@body))

;;; What the tests do and look at

(defun test-confluence--poll ()
  "Poll each entry of the backends option, as each poll of shipit does."
  (dolist (entry shipit-issue-repo-backends)
    (shipit--poll-one-backend entry nil)))

(defun test-confluence--notifications ()
  "The notifications that shipit holds, each as its title and its reason.
They are in the order of their titles."
  (sort (mapcar (lambda (activity)
                  (list (alist-get 'subject activity) (alist-get 'reason activity)))
                (hash-table-values shipit--notification-pr-activities))
        (lambda (a b) (string< (car a) (car b)))))

(defun test-confluence--the-one ()
  "The one notification that shipit holds."
  (let ((activities (hash-table-values shipit--notification-pr-activities)))
    (should (= 1 (length activities)))
    (car activities)))

(defun test-confluence--searches ()
  "Each search that Confluence was asked, oldest first, as (ADDRESS HEADERS)."
  (cl-loop for request in shipit-test-http-requests
           for headers in shipit-test-http-headers
           when (string-match-p test-confluence--search-path (cadr request))
           collect (list (cadr request) headers)))

(defun test-confluence--should-search-with-login ()
  "Check that Confluence was searched at the site, with the login of the site."
  (let ((searches (test-confluence--searches)))
    (should searches)
    (dolist (search searches)
      (should (string-prefix-p (concat test-confluence--site test-confluence--search-path "?")
                               (car search)))
      (should (equal test-confluence--authorization
                     (cdr (assoc "Authorization" (cadr search))))))))

(defun test-confluence--line ()
  "The line at point, as text."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun test-confluence--row-of-p (title)
  "Whether the line at point is a row of Confluence that shows TITLE."
  (and (equal "confluence" (get-text-property (point) 'shipit-notification-type))
       (string-match-p (regexp-quote title) (test-confluence--line))))

(defun test-confluence--goto-row (title)
  "Put point on the row of Confluence that shows the page TITLE."
  (goto-char (point-min))
  (while (not (or (eobp) (test-confluence--row-of-p title)))
    (forward-line 1))
  (should (test-confluence--row-of-p title)))

(defmacro test-confluence--in-buffer (title &rest body)
  "Run BODY in the notifications buffer, on the row of the page TITLE.
The buffer is drawn from what shipit holds."
  (declare (indent 1))
  `(let ((buffer (shipit-notifications-buffer-create)))
     (unwind-protect
         (with-current-buffer buffer
           (shipit-notifications-buffer--rerender)
           (test-confluence--goto-row ,title)
           ,@body)
       (kill-buffer buffer))))

(defun test-confluence--unread-rows ()
  "The rows of Confluence that the notifications buffer shows as unread."
  (let ((buffer (shipit-notifications-buffer-create))
        (rows nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq shipit-notifications-buffer--display-scope 'unread)
          (shipit-notifications-buffer--rerender)
          (goto-char (point-min))
          (while (not (eobp))
            (when (equal "confluence" (get-text-property (point) 'shipit-notification-type))
              (push (test-confluence--line) rows))
            (forward-line 1)))
      (kill-buffer buffer))
    (nreverse rows)))

(defun test-confluence--press-return (title)
  "Press RET on the row of the page TITLE in the notifications buffer.
Gives the addresses that the browser was asked for."
  (let* ((asked nil)
         (browse-url-browser-function (lambda (url &rest _) (push url asked))))
    (test-confluence--in-buffer title
      (call-interactively (key-binding (kbd "RET"))))
    (nreverse asked)))

(defun test-confluence--mark-read (title)
  "Mark the row of the page TITLE as read, with the command of the buffer.
The time in which the mark can be undone ends at once."
  (test-confluence--in-buffer title
    (call-interactively #'shipit-notifications-buffer-mark-read)
    (shipit-notifications-buffer--commit-all-pending-marks)))

(defun test-confluence--with-excerpt (page excerpt)
  "A copy of PAGE, as the search gives it, whose excerpt is EXCERPT."
  (cons (cons 'excerpt excerpt) (assq-delete-all 'excerpt (copy-alist page))))

(defun test-confluence--words (count)
  "A text of COUNT words, from cup1 to cupCOUNT."
  (mapconcat (lambda (n) (format "cup%d" n)) (number-sequence 1 count) " "))

(defun test-confluence--unfold (title)
  "Press TAB on the row of the page TITLE; the lines of text that show.
Each line is as the buffer shows it, with its indent."
  (test-confluence--in-buffer title
    (call-interactively (key-binding (kbd "TAB")))
    (let ((section (magit-current-section)))
      (split-string (buffer-substring-no-properties (oref section content) (oref section end))
                    "\n" t))))

(defun test-confluence--unfold-coffee-menu (excerpt)
  "Poll with the page Coffee menu, whose excerpt is EXCERPT, and unfold it.
Its newest change has the message Change 7.  Gives the lines of its
text, as `test-confluence--unfold' does."
  (test-confluence--with-pages
      (list (test-confluence--with-excerpt (test-confluence--coffee-menu 7 "alice") excerpt))
      nil
    (test-confluence--poll)
    (test-confluence--unfold "Coffee menu")))

(defun test-confluence--should-fit (lines width)
  "Check that each of LINES is indented and at most WIDTH columns wide."
  (should lines)
  (dolist (line lines)
    (should (string-prefix-p "    " line))
    (should (<= (string-width line) width))))

;;; The scenarios

(ert-deftest test-confluence-a-page-that-mentions-you-is-a-notification ()
  "GIVEN the Confluence site in the backends option, and the page Coffee
menu in the space Kitchen, which mentions you and which alice changed
WHEN shipit polls
THEN shipit holds one unread notification, from Kitchen, with the title
Coffee menu, the reason mention, the author alice, the type confluence
and the address of the page, AND Confluence was searched with the login
of the site."
  (test-confluence--with-pages (list (test-confluence--coffee-menu 7 "alice")) nil
    (test-confluence--poll)
    (let ((activity (test-confluence--the-one)))
      (should (equal "Kitchen" (alist-get 'repo activity)))
      (should (equal "Coffee menu" (alist-get 'subject activity)))
      (should (equal "mention" (alist-get 'reason activity)))
      (should (equal "alice" (alist-get 'author activity)))
      (should (equal "confluence" (alist-get 'type activity)))
      (should (equal test-confluence--coffee-menu-address (alist-get 'browse-url activity))))
    (should (= 1 (shipit--count-unread-activities)))
    (test-confluence--should-search-with-login)))

(ert-deftest test-confluence-a-watched-page-is-a-notification ()
  "GIVEN the page Cup inventory, which you watch and which bob changed,
AND the page Coffee menu, which you watch, which mentions you and which
alice changed
WHEN shipit polls
THEN there are two unread notifications: Coffee menu with the reason
mention, and Cup inventory with the reason subscribed."
  (test-confluence--with-pages
      (list (test-confluence--coffee-menu 7 "alice"))
      (list (test-confluence--cup-inventory 3 "bob")
            (test-confluence--coffee-menu 7 "alice"))
    (test-confluence--poll)
    (should (equal '(("Coffee menu" "mention") ("Cup inventory" "subscribed"))
                   (test-confluence--notifications)))
    (should (= 2 (shipit--count-unread-activities)))))

(ert-deftest test-confluence-your-own-change-is-not-unread ()
  "GIVEN a page that you watch, whose last change is your own, AND the
default rules that mark notifications as read
WHEN shipit polls
THEN Confluence was searched, AND no notification is unread, AND the
notifications buffer shows no unread row of Confluence."
  (test-confluence--with-pages nil (list (test-confluence--cup-inventory 7 "me"))
    (test-confluence--poll)
    (should (test-confluence--searches))
    (should (= 0 (shipit--count-unread-activities)))
    (should-not (test-confluence--unread-rows))))

(ert-deftest test-confluence-return-opens-the-page-and-marks-it-read ()
  "GIVEN the row of the page Coffee menu, which mentions you, in the
notifications buffer
WHEN you press RET on it
THEN the browser is asked for the address of the page, AND shipit holds
no notification of the page any more."
  (test-confluence--with-pages (list (test-confluence--coffee-menu 7 "alice")) nil
    (test-confluence--poll)
    (should (equal (list test-confluence--coffee-menu-address)
                   (test-confluence--press-return "Coffee menu")))
    (should-not (test-confluence--notifications))))

(ert-deftest test-confluence-a-newer-change-replaces-the-unread-row ()
  "GIVEN an unread notification of the page Cup inventory, which you
watch, for the change that bob made
WHEN alice changes the page, and shipit polls again
THEN there is still one notification of the page, AND it names alice
as the one who changed it."
  (let ((watched (list (test-confluence--cup-inventory 7 "bob"))))
    (test-confluence--with-pages nil watched
      (test-confluence--poll)
      (should (equal "bob" (alist-get 'author (test-confluence--the-one))))
      (setq watched (list (test-confluence--cup-inventory 8 "alice")))
      (test-confluence--poll)
      (should (equal "alice" (alist-get 'author (test-confluence--the-one)))))))

(ert-deftest test-confluence-a-read-page-comes-back-when-it-changes ()
  "GIVEN the notification of the page Cup inventory, which you watch,
marked as read with the command of the notifications buffer
WHEN shipit polls and the page has not changed
THEN there is no notification
WHEN bob changes the page, and shipit polls
THEN the notification of the page is back, unread."
  (let ((watched (list (test-confluence--cup-inventory 7 "bob"))))
    (test-confluence--with-pages nil watched
      (test-confluence--poll)
      (test-confluence--mark-read "Cup inventory")
      (test-confluence--poll)
      (should-not (test-confluence--notifications))
      (setq watched (list (test-confluence--cup-inventory 8 "bob")))
      (test-confluence--poll)
      (should (equal '(("Cup inventory" "subscribed")) (test-confluence--notifications)))
      (should (= 1 (shipit--count-unread-activities))))))

(ert-deftest test-confluence-a-refused-search-gives-no-notification ()
  "GIVEN a Confluence site that refuses each search
WHEN shipit polls
THEN the poll signals nothing, AND Confluence was searched, AND shipit
holds no notification."
  (test-confluence--fresh
      (list (list test-confluence--search-path 401 test-confluence--refusal)
            test-confluence--current-user)
    (test-confluence--poll)
    (should (test-confluence--searches))
    (should-not (test-confluence--notifications))))

(ert-deftest test-confluence-the-text-of-a-page-wraps-at-the-wrap-column ()
  "The text of an unfolded page is wrapped at the wrap column."
  ;; GIVEN the page Coffee menu, whose newest change has the message
  ;; Change 7 and whose excerpt has 100 words, AND the wrap column of 120
  ;; WHEN you press TAB on its row
  ;; THEN each line of the text is indented and at most 120 columns
  ;; wide, AND the lines hold the message and then each word of the
  ;; excerpt, in order
  (let* ((shipit-render-wrap-column 120)
         (excerpt (test-confluence--words 100))
         (lines (test-confluence--unfold-coffee-menu excerpt)))
    (test-confluence--should-fit lines 120)
    (should (equal (concat "Change 7 " excerpt)
                   (mapconcat #'string-trim lines " ")))))

(ert-deftest test-confluence-the-text-of-a-page-follows-the-wrap-column ()
  "A smaller wrap column gives narrower lines of text."
  ;; GIVEN the page Coffee menu, whose excerpt has 100 words, AND the
  ;; wrap column set to 60
  ;; WHEN you press TAB on its row
  ;; THEN each line of the text is indented and at most 60 columns wide
  (let ((shipit-render-wrap-column 60))
    (test-confluence--should-fit
     (test-confluence--unfold-coffee-menu (test-confluence--words 100)) 60)))

(ert-deftest test-confluence-a-long-address-in-the-text-stays-whole ()
  "An address wider than the wrap column keeps a line of its own."
  ;; GIVEN the page Coffee menu, whose excerpt has an address of 148
  ;; characters between two parts of a sentence, AND the wrap column
  ;; of 120
  ;; WHEN you press TAB on its row
  ;; THEN one line holds the whole address and nothing else, AND each
  ;; other line is indented and at most 120 columns wide
  (let* ((shipit-render-wrap-column 120)
         (address (concat "https://example.com/kitchen/" (make-string 120 ?a)))
         (address-line (concat "    " address))
         (lines (test-confluence--unfold-coffee-menu
                 (format "The new menu is at %s and it starts on Monday." address))))
    (should (member address-line lines))
    (test-confluence--should-fit (remove address-line lines) 120)))

(provide 'test-shipit-notifications-confluence)
;;; test-shipit-notifications-confluence.el ends here
