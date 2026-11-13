;;; test-shipit-confluence-search-cases.el --- More cases of the search of Confluence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The cases of the search of Confluence that the scenarios of
;; `test-shipit-confluence-search.el' leave out: what the search asks
;; for, words with odd characters, what a row shows, two sites, empty
;; words, a missing login, and the check whether consult is there.  The
;; answers of Confluence and the helpers are those of that file and of
;; `test-shipit-notifications-confluence.el'.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'parse-time)
(require 'test-shipit-notifications-confluence)
(require 'test-shipit-notifications-confluence-cases)
(require 'test-shipit-confluence-search)
(require 'shipit-code-refs)
(require 'shipit-atlassian-kanban)
(require 'shipit-confluence-search nil t)

(defconst test-confluence-search-cases--other-site "https://example.com"
  "The address of a second Confluence site, for which auth-source holds no login.")

;;; The answers of Confluence

(defun test-confluence-search-cases--finding-anything (&rest pages)
  "The answers of Confluence when each search finds PAGES, whatever its words."
  (list (list test-confluence--search-path 200
              (lambda (_sent) (test-confluence--search-answer pages)))))

(defun test-confluence-search-cases--blog-post (id author)
  "The blog post ID with the title Coffee menu in the space Kitchen, which AUTHOR made."
  (test-confluence-cases--put-in (test-confluence--page id "Coffee menu" 1 author)
                                 '(content type) "blogpost"))

;;; A prompt of consult that shows what it was given

(defvar test-confluence-search-cases--consult-call nil
  "What the stand-in of consult saw at its last call, as a plist.
`:rows' are the rows of the words, `:read' the options of the prompt and
`:collection' the options of the rows that follow the typing.")

(defun test-confluence-search-cases--consult-read (collection options words choice)
  "What the stand-in of `consult--read' gives for WORDS: CHOICE.
COLLECTION is what the stand-in of `consult--dynamic-collection' gave,
and OPTIONS the options of the prompt.  The rows of WORDS are asked
once.  With no CHOICE the prompt quits, as C-g does."
  (setq test-confluence-search-cases--consult-call
        (list :rows (funcall (car collection) words)
              :read options
              :collection (cdr collection)))
  (or choice (signal 'quit nil)))

(defun test-confluence-search-cases--with-consult (words choice)
  "Run the search with a stand-in of consult, which gets WORDS and gives CHOICE.
See `test-confluence-search-cases--consult-read'.  Emacs keeps
`features' out of the special variables, so a `let' would not make
consult a feature."
  (setq test-confluence-search-cases--consult-call nil)
  (cl-letf (((symbol-value 'features) (cons 'consult features))
            ((symbol-function 'consult--dynamic-collection)
             (lambda (fetch &rest options) (cons fetch options)))
            ((symbol-function 'consult--read)
             (lambda (collection &rest options)
               (test-confluence-search-cases--consult-read collection options words choice))))
    (call-interactively #'shipit-confluence-search)))

(defun test-confluence-search-cases--opened (search)
  "Call SEARCH, and give the addresses that the browser was asked for.
A quit of the prompt ends SEARCH."
  (let* ((opened nil)
         (browse-url-browser-function (lambda (url &rest _) (push url opened))))
    (condition-case nil
        (funcall search)
      (quit nil))
    (nreverse opened)))

;;; The prompts of Emacs

(defun test-confluence-search-cases--annotations-here ()
  "The annotation of each row that the minibuffer offers now."
  (let* ((table minibuffer-completion-table)
         (predicate minibuffer-completion-predicate)
         (annotate (completion-metadata-get (completion-metadata "" table predicate)
                                            'annotation-function)))
    (mapcar annotate (all-completions "" table predicate))))

(defun test-confluence-search-cases--annotations-with-emacs (words)
  "Search for WORDS at the prompts of Emacs; the annotations of the rows offered."
  (let* ((annotations nil)
         (test-confluence-search--prompt 'emacs)
         (minibuffer-setup-hook
          (cons (lambda ()
                  (when minibuffer-completion-table
                    (setq annotations (test-confluence-search-cases--annotations-here))))
                minibuffer-setup-hook)))
    (test-confluence-search--search words)
    annotations))

(defun test-confluence-search-cases--annotations-with-consult (words)
  "Search for WORDS at the prompt of consult; the annotations of the rows offered."
  (test-confluence-search-cases--opened
   (lambda () (test-confluence-search-cases--with-consult words nil)))
  (let ((call test-confluence-search-cases--consult-call))
    (mapcar (plist-get (plist-get call :read) :annotate) (plist-get call :rows))))

(defun test-confluence-search-cases--choosing-site (site words)
  "Choose SITE at the prompt of sites, search for WORDS, and leave the prompt.
The next prompt is the one of consult or those of Emacs, as
`test-confluence-search--prompt' says.  Gives the sites that the prompt
of sites offered."
  (let* ((offered nil)
         (minibuffer-setup-hook
          (cons (lambda ()
                  (unless offered
                    (setq offered (all-completions "" minibuffer-completion-table
                                                   minibuffer-completion-predicate))
                    (insert site)))
                minibuffer-setup-hook)))
    (test-confluence-search-cases--opened
     (lambda ()
       (if (eq test-confluence-search--prompt 'consult)
           (ert-simulate-keys (list ?\r)
             (test-confluence-search-cases--with-consult words nil))
         (ert-simulate-keys (append (list ?\r) (string-to-list words) (list ?\r))
           (call-interactively #'shipit-confluence-search)))))
    offered))

;;; What the search asks for

(ert-deftest test-confluence-search-cases-the-search-asks-for-the-words-and-25-pages ()
  "GIVEN the Confluence site, and a search for coffee that finds the page
Coffee menu
WHEN you search for coffee
THEN Confluence is asked once, for the pages and blog posts whose title
or text holds coffee, AND for the first 25 of them, AND for the version
and the space of each."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee" (test-confluence--coffee-menu 1 "alice"))
      (test-confluence-search--search "coffee")
      (should (equal '("siteSearch ~ \"coffee\" and type in (page, blogpost)")
                     (test-confluence-cases--asked "cql")))
      (should (equal '("25") (test-confluence-cases--asked "limit")))
      (should (equal '("content.version,content.space")
                     (test-confluence-cases--asked "expand"))))))

(ert-deftest test-confluence-search-cases-quotes-and-backslashes-in-the-words-are-escaped ()
  "GIVEN the Confluence site
WHEN you search for \"coffee menu\" in double quotes
AND you search for a\\b, with a backslash
THEN the CQL of each search holds the words in double quotes, with a
backslash before each double quote and each backslash of the words."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search-cases--finding-anything)
      (test-confluence-search--search "\"coffee menu\"")
      (test-confluence-search--search "a\\b")
      (should (equal '("siteSearch ~ \"\\\"coffee menu\\\"\" and type in (page, blogpost)"
                       "siteSearch ~ \"a\\\\b\" and type in (page, blogpost)")
                     (test-confluence-cases--asked "cql"))))))

;;; What a row shows

(ert-deftest test-confluence-search-cases-a-row-shows-the-title-without-highlight ()
  "GIVEN a search for coffee that finds the page Coffee menu, whose title
in the answer holds the marks with which the search shows the words it
found
WHEN you search for coffee
THEN the row shows the title without the marks, two spaces, then the
name of the space."
  (let ((page (test-confluence-cases--put (test-confluence--coffee-menu 1 "alice")
                                          'title "@@@hl@@@Coffee@@@endhl@@@ menu")))
    (test-confluence-search--at-each-prompt
      (test-confluence--fresh (test-confluence-search--finding "coffee" page)
        (should (equal '("Coffee menu  Kitchen")
                       (plist-get (test-confluence-search--search "coffee") :rows)))))))

(ert-deftest test-confluence-search-cases-two-pages-with-one-row-get-two-rows ()
  "GIVEN a search for coffee that finds two blog posts with the title
Coffee menu, both in the space Kitchen
WHEN you search for coffee
THEN the prompt offers two rows, and the second ends with the id of its
blog post
WHEN you choose the first row
THEN the browser opens the first blog post
WHEN you search again and choose the second row
THEN the browser opens the second blog post."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee"
                             (test-confluence-search-cases--blog-post "101" "alice")
                             (test-confluence-search-cases--blog-post "103" "bob"))
      (let ((result (test-confluence-search--search "coffee" '("Coffee menu" "Kitchen"))))
        (should (equal '("Coffee menu  Kitchen" "Coffee menu  Kitchen (103)")
                       (plist-get result :rows)))
        (should (equal (list test-confluence--coffee-menu-address)
                       (plist-get result :opened))))
      (let ((result (test-confluence-search--search "coffee" '("Coffee menu" "(103)"))))
        (should (equal '("https://example.atlassian.net/wiki/spaces/KITCHEN/pages/103/Coffee+menu")
                       (plist-get result :opened)))))))

(ert-deftest test-confluence-search-cases-the-annotation-is-the-time-of-the-last-change ()
  "GIVEN a search for coffee that finds the page Coffee menu, which alice
changed 29 minutes ago
WHEN you search for coffee, with the times of shipit as a date or as how
long ago
THEN the annotation of its row is the time of the change, written as
shipit writes times, in the prompt of pages of Emacs and in the prompt
of consult."
  (let* ((page (test-confluence--coffee-menu 1 "alice"))
         (changed (alist-get 'lastModified page)))
    (dolist (style (list (cons 'relative "29m ago")
                         (cons 'absolute (format-time-string
                                          "%Y-%m-%d %H:%M"
                                          (parse-iso8601-time-string changed)))))
      (let ((shipit-timestamp-format (car style)))
        (ert-info ((format "With the times as %s" (car style)))
          (test-confluence--fresh (test-confluence-search--finding "coffee" page)
            (dolist (annotations (list (test-confluence-search-cases--annotations-with-emacs "coffee")
                                       (test-confluence-search-cases--annotations-with-consult "coffee")))
              (should (= 1 (length annotations)))
              (should (string-search (cdr style) (car annotations))))))))))

(ert-deftest test-confluence-search-cases-a-result-without-a-title-or-address-is-left-out ()
  "GIVEN a search for coffee that finds Coffee menu without a title of its
content, a page without an address, a page without an id, and the page
Cup inventory
WHEN you search for coffee
THEN the prompt offers one row, the one of Cup inventory."
  (let ((pages (list (test-confluence-cases--put-in (test-confluence--coffee-menu 1 "alice")
                                                    '(content title) nil)
                     (test-confluence-cases--put-in (test-confluence--page "104" "Tea menu" 1 "bob")
                                                    '(content _links) nil)
                     (test-confluence-cases--put-in (test-confluence--page "105" "Milk menu" 1 "bob")
                                                    '(content id) nil)
                     (test-confluence--cup-inventory 1 "alice"))))
    (test-confluence-search--at-each-prompt
      (test-confluence--fresh (apply #'test-confluence-search--finding "coffee" pages)
        (should (equal '("Cup inventory  Kitchen")
                       (plist-get (test-confluence-search--search "coffee") :rows)))))))

;;; The prompt of consult

(ert-deftest test-confluence-search-cases-consult-keeps-its-own-defaults ()
  "GIVEN the Confluence site, and consult
WHEN you search for coffee
THEN the rows follow the typing with the defaults of consult for the
shortest input and the delays, AND the prompt keeps the order of
Confluence, AND it takes only an offered row."
  (test-confluence--fresh (test-confluence-search--finding
                           "coffee" (test-confluence--coffee-menu 1 "alice"))
    (test-confluence-search-cases--opened
     (lambda () (test-confluence-search-cases--with-consult "coffee" nil)))
    (let* ((call test-confluence-search-cases--consult-call)
           (options (plist-get call :read)))
      (should-not (plist-get call :collection))
      (should (plist-member options :sort))
      (should-not (plist-get options :sort))
      (should (eq t (plist-get options :require-match))))))

(ert-deftest test-confluence-search-cases-an-empty-choice-opens-nothing ()
  "GIVEN a search for coffee that finds the page Coffee menu, and consult
WHEN the prompt gives back an empty row
THEN no page opens, and shipit signals nothing."
  (test-confluence--fresh (test-confluence-search--finding
                           "coffee" (test-confluence--coffee-menu 1 "alice"))
    (should-not (test-confluence-search-cases--opened
                 (lambda () (test-confluence-search-cases--with-consult "coffee" ""))))))

(ert-deftest test-confluence-search-cases-a-row-that-was-not-offered-is-a-bug ()
  "GIVEN a search for coffee that finds the page Coffee menu, and consult
WHEN you search for coffee and the prompt gives back a row that it was
not offered
THEN shipit signals an error that names the row and is no error of the
user, AND no page opens."
  (test-confluence--fresh (test-confluence-search--finding
                           "coffee" (test-confluence--coffee-menu 1 "alice"))
    (let* ((opened nil)
           (browse-url-browser-function (lambda (url &rest _) (push url opened)))
           (err (should-error (test-confluence-search-cases--with-consult
                               "coffee" "Tea menu  Kitchen"))))
      (should (test-confluence--searches))
      (should (string-match-p "Tea menu  Kitchen" (error-message-string err)))
      (should-not (eq 'user-error (car err)))
      (should-not opened))))

(ert-deftest test-confluence-search-cases-typing-does-not-cut-a-request-short ()
  "GIVEN the Confluence site, and consult, which asks for the rows while
it listens for typing and drops the work when a key arrives
WHEN you search for coffee
THEN the request to Confluence runs without that listening, so a key
cannot cut it short.  url.el leaves the buffer of a request that is cut
short alive."
  (let ((listening 'not-asked))
    (test-confluence--fresh
        (list (test-confluence--search
               "coffee" (lambda ()
                          (setq listening throw-on-input)
                          (list (test-confluence--coffee-menu 1 "alice")))))
      (test-confluence-search-cases--opened
       (lambda ()
         (let ((throw-on-input 'typing))
           (test-confluence-search-cases--with-consult "coffee" nil))))
      (should (null listening)))))

;;; The site and the login

(ert-deftest test-confluence-search-cases-with-two-sites-it-asks-which ()
  "GIVEN two Confluence sites in the backends option
WHEN you run the search, choose the site example.atlassian.net, and
search for coffee
THEN the prompt of sites offered both sites
AND Confluence was searched once, at the chosen site, with its login."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee" (test-confluence--coffee-menu 1 "alice"))
      (let* ((shipit-issue-repo-backends
              (list (list "other" :backend 'confluence
                          :base-url test-confluence-search-cases--other-site)
                    test-confluence--entry))
             (offered (test-confluence-search-cases--choosing-site
                       test-confluence--site "coffee")))
        (should (equal (sort (list test-confluence--site test-confluence-search-cases--other-site)
                             #'string<)
                       (sort offered #'string<)))
        (should (= 1 (length (test-confluence--searches))))
        (test-confluence--should-search-with-login)))))

(ert-deftest test-confluence-search-cases-a-missing-login-says-how-to-add-it ()
  "GIVEN the Confluence site, for which auth-source holds no login
WHEN you search for coffee
THEN shipit says that it found no login for the site, and how to add it
AND it asks Confluence nothing AND no page opens."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh nil
      (cl-letf (((symbol-function 'auth-source-search) #'ignore))
        (let ((result (test-confluence-search--search "coffee")))
          (should (string-match-p "No Jira credentials found for example.atlassian.net"
                                  (plist-get result :said)))
          (should (string-match-p "machine example.atlassian.net" (plist-get result :said)))
          (should-not shipit-test-http-requests)
          (should-not (plist-get result :opened)))))))

;;; The prompts of Emacs

(ert-deftest test-confluence-search-cases-empty-words-ask-nothing ()
  "GIVEN the Confluence site, and no consult
WHEN you run the search and type only spaces at the prompt of the words
THEN shipit says that it needs words
AND it asks Confluence nothing AND no page opens."
  (let ((test-confluence-search--prompt 'emacs))
    (test-confluence--fresh nil
      (let ((result (test-confluence-search--search "   ")))
        (should (string-match-p "words" (plist-get result :said)))
        (should-not shipit-test-http-requests)
        (should-not (plist-get result :opened))))))

;;; Whether consult is there

(defun test-confluence-search-cases--answers ()
  "The answers of the checks for consult, each t or nil.
They are the check for the two functions of the search, the check for
`consult--read' alone, the check of the issue picker in code and the
check of the kanban board."
  (mapcar (lambda (answer) (and answer t))
          (list (shipit--consult-available-p 'consult--read 'consult--dynamic-collection)
                (shipit--consult-available-p 'consult--read)
                (shipit-code-refs--consult-available-p)
                (shipit-atlassian-kanban--consult-available-p))))

(defun test-confluence-search-cases--answers-with-consult (functions)
  "The answers of the checks for consult when consult defines only FUNCTIONS.
See `test-confluence-search-cases--answers'."
  (cl-letf (((symbol-value 'features) (cons 'consult features))
            ((symbol-function 'consult--read)
             (and (memq 'consult--read functions) #'ignore))
            ((symbol-function 'consult--dynamic-collection)
             (and (memq 'consult--dynamic-collection functions) #'ignore)))
    (test-confluence-search-cases--answers)))

(ert-deftest test-confluence-search-cases-the-check-for-consult ()
  "GIVEN no consult that Emacs can load
THEN each check says that consult is not there
GIVEN consult with `consult--read' and without `consult--dynamic-collection'
THEN the check for both functions says no, the check for
`consult--read' says yes, the issue picker in code says no, as it needs
both, and the kanban board says yes, as it needs `consult--read' only
GIVEN consult with both functions
THEN each check says yes."
  (cl-letf (((symbol-value 'features) (remq 'consult features)))
    (let ((load-path nil))
      (should (equal '(nil nil nil nil) (test-confluence-search-cases--answers)))))
  (should (equal '(nil t nil t)
                 (test-confluence-search-cases--answers-with-consult '(consult--read))))
  (should (equal '(t t t t)
                 (test-confluence-search-cases--answers-with-consult
                  '(consult--read consult--dynamic-collection)))))

(provide 'test-shipit-confluence-search-cases)
;;; test-shipit-confluence-search-cases.el ends here
