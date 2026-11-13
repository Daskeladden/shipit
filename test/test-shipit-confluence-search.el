;;; test-shipit-confluence-search.el --- Tests for the search of Confluence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the search of Confluence pages: you type words, the prompt
;; offers the pages that Confluence finds for them, and the page that
;; you choose opens in the browser.
;;
;; Each scenario of the prompt runs twice.  Without consult the prompts
;; are those of Emacs, in a real minibuffer that gets the keys of the
;; test.  With consult the prompt is the one of consult.  The packages
;; of the tests hold no consult, so a stand-in takes its two functions
;; there: it asks for the rows of the words and gives the chosen row
;; back.
;;
;; Confluence is answered at url.el, the login at auth-source and the
;; browser at `browse-url-browser-function'.  The answers of Confluence
;; and the login are those of `test-shipit-notifications-confluence.el'.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'minibuffer)
(require 'transient)
(require 'shipit)
(require 'shipit-test-http)
(require 'test-shipit-notifications-confluence)
(require 'shipit-confluence-search nil t)

(defconst test-confluence-search--bar-menu-address
  "https://example.atlassian.net/wiki/spaces/BAR/pages/201/Coffee+menu"
  "The address in the browser of the page Coffee menu of the space Bar.")

;;; The answers of Confluence

(defun test-confluence-search--in-space (page key name)
  "PAGE as the search gives it when it is in the space NAME, which has KEY."
  (let* ((page (copy-tree page))
         (content (alist-get 'content page))
         (path (string-replace "/spaces/KITCHEN/" (format "/spaces/%s/" key)
                               (alist-get 'webui (alist-get '_links content)))))
    (setf (alist-get 'space content) `((key . ,key) (name . ,name)))
    (setf (alist-get 'webui (alist-get '_links content)) path)
    (setf (alist-get 'url page) path)
    (setf (alist-get 'resultGlobalContainer page)
          `((title . ,name) (displayUrl . ,(concat "/spaces/" key))))
    page))

(defun test-confluence-search--bar-menu ()
  "The page Coffee menu of the space Bar."
  (test-confluence-search--in-space
   (test-confluence--page "201" "Coffee menu" 1 "alice") "BAR" "Bar"))

(defun test-confluence-search--finding (word &rest pages)
  "The answers of Confluence when a search for WORD finds PAGES, in that order.
A search that does not hold WORD gets no answer, and neither does any
other request."
  (list (test-confluence--search word (lambda () pages))))

(defconst test-confluence-search--refusing
  (list (list test-confluence--search-path 401 test-confluence--refusal))
  "The answers of Confluence when it refuses each search.")

;;; The rows of a prompt

(defvar test-confluence-search--prompt nil
  "The prompt that a scenario runs with now: `consult' or `emacs'.")

(defvar test-confluence-search--rows nil
  "The rows that the prompt offered at the newest search, top row first.")

(defun test-confluence-search--row-p (row title space)
  "Whether ROW shows the page TITLE of SPACE."
  (and (string-match-p (regexp-quote title) row)
       (string-match-p (regexp-quote space) row)))

(defun test-confluence-search--row (rows page)
  "The row among ROWS of PAGE, which is (TITLE SPACE), or nil."
  (and page
       (seq-find (lambda (row)
                   (test-confluence-search--row-p row (car page) (cadr page)))
                 rows)))

(defun test-confluence-search--as-shown (rows order)
  "ROWS as text, in the order in which a prompt shows them.
ORDER is the function that the prompt was given to order them.  With
none a prompt orders the rows itself, here by the alphabet."
  (let ((rows (mapcar #'substring-no-properties rows)))
    (if order (funcall order rows) (sort rows #'string<))))

;;; The prompts of Emacs

(defun test-confluence-search--offered ()
  "The rows that the minibuffer offers now, in the order in which it shows them."
  (let* ((table minibuffer-completion-table)
         (predicate minibuffer-completion-predicate)
         (metadata (completion-metadata "" table predicate)))
    (test-confluence-search--as-shown
     (all-completions "" table predicate)
     (completion-metadata-get metadata 'display-sort-function))))

(defun test-confluence-search--at-the-pages (page)
  "Look at the rows of the prompt of pages, and write the row of PAGE into it.
Does nothing in a prompt that completes nothing, as the one of the words."
  (when minibuffer-completion-table
    (setq test-confluence-search--rows (test-confluence-search--offered))
    (when-let* ((row (test-confluence-search--row test-confluence-search--rows page)))
      (insert row))))

(defun test-confluence-search--with-emacs (words page)
  "Search for WORDS at the prompts of Emacs, and choose PAGE.
The keys go to a real minibuffer.  The first prompt gets WORDS and RET.
At the prompt of pages the row of PAGE is written, as a move to that row
does, and RET chooses it.  With no PAGE, C-g leaves that prompt."
  (let ((minibuffer-setup-hook
         (cons (lambda () (test-confluence-search--at-the-pages page))
               minibuffer-setup-hook)))
    (ert-simulate-keys (append (string-to-list words)
                               (list ?\r)
                               (and page (list ?\r)))
      (call-interactively #'shipit-confluence-search))))

;;; The prompt of consult

(defun test-confluence-search--consult-orders-p (options)
  "Whether consult orders the rows itself, when it is called with OPTIONS.
It does unless it is told not to."
  (if (plist-member options :sort) (plist-get options :sort) t))

(defun test-confluence-search--consult-read (rows-of options words page)
  "What `consult--read' gives when the user types WORDS and chooses PAGE.
ROWS-OF is the function of the search that gives the rows of what was
typed, and OPTIONS are the options of the prompt.  ROWS-OF is asked
once, as consult asks when the typing stops.  The row of PAGE comes back
as text without properties, as a prompt gives it.  With no PAGE the
prompt quits, as C-g does.  A search that signals while consult asks
for the rows fails the test: consult asks from a timer, where an error
reaches nobody."
  (let ((rows (condition-case err
                  (funcall rows-of words)
                (error (ert-fail (list "The search signalled inside consult" err))))))
    (setq test-confluence-search--rows
          (test-confluence-search--as-shown
           rows (unless (test-confluence-search--consult-orders-p options) #'identity)))
    (or (test-confluence-search--row test-confluence-search--rows page)
        (signal 'quit nil))))

(defun test-confluence-search--with-consult (words page)
  "Search for WORDS at the prompt of consult, and choose PAGE.
A stand-in takes the two functions of consult that the search uses, see
`test-confluence-search--consult-read'.  Emacs keeps `features' out of
the special variables, so a `let' would not make consult a feature."
  (cl-letf (((symbol-value 'features) (cons 'consult features))
            ((symbol-function 'consult--dynamic-collection)
             (lambda (rows-of &rest _options) rows-of))
            ((symbol-function 'consult--read)
             (lambda (rows-of &rest options)
               (test-confluence-search--consult-read rows-of options words page))))
    (call-interactively #'shipit-confluence-search)))

;;; What the user does

(defmacro test-confluence-search--at-each-prompt (&rest body)
  "Run BODY twice: with the prompt of consult, and with the prompts of Emacs."
  (declare (indent 0))
  `(dolist (prompt '(consult emacs))
     (let ((test-confluence-search--prompt prompt))
       (ert-info ((format "With the prompt of %s" prompt))
         ,@body))))

(defun test-confluence-search--search (words &optional page)
  "Run the search as the user does: type WORDS, and choose PAGE when given.
PAGE is (TITLE SPACE).  Without PAGE the user looks at the rows and
leaves the prompt.  Gives a plist: `:rows' are the rows that the prompt
offered, top row first, `:opened' the addresses that the browser was
asked for, and `:said' what shipit told the user, in a message or in the
error of the command."
  (let* ((opened nil)
         (browse-url-browser-function (lambda (url &rest _) (push url opened)))
         (refusal nil))
    (setq test-confluence-search--rows nil)
    (ert-with-message-capture messages
      (condition-case err
          (funcall (if (eq test-confluence-search--prompt 'consult)
                       #'test-confluence-search--with-consult
                     #'test-confluence-search--with-emacs)
                   words page)
        (quit nil)
        (user-error (setq refusal (error-message-string err))))
      (list :rows test-confluence-search--rows
            :opened (nreverse opened)
            :said (concat messages refusal)))))

;;; The scenarios

(ert-deftest test-confluence-search-offers-the-pages-that-hold-the-words ()
  "GIVEN the Confluence site in the backends option, and a search of
Confluence for coffee that finds the page Cup inventory and then the
page Coffee menu, both in the space Kitchen
WHEN you run the search, type coffee and look at the rows
THEN the prompt offers two rows, Cup inventory above Coffee menu, as
Confluence orders them, each with the title and the name of the space
AND Confluence was asked once, at the site, with your login
AND no page opens."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee"
                             (test-confluence--cup-inventory 1 "alice")
                             (test-confluence--coffee-menu 1 "alice"))
      (let* ((result (test-confluence-search--search "coffee"))
             (rows (plist-get result :rows)))
        (should (= 2 (length rows)))
        (should (test-confluence-search--row-p (nth 0 rows) "Cup inventory" "Kitchen"))
        (should (test-confluence-search--row-p (nth 1 rows) "Coffee menu" "Kitchen"))
        (should (= 1 (length (test-confluence--searches))))
        (test-confluence--should-search-with-login)
        (should-not (plist-get result :opened))))))

(ert-deftest test-confluence-search-return-opens-the-page-in-the-browser ()
  "GIVEN the Confluence site in the backends option, and a search for
coffee that finds the page Cup inventory and then the page Coffee menu
WHEN you type coffee and choose the second row, Coffee menu, with RET
THEN the browser opens the page Coffee menu, and no other page."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee"
                             (test-confluence--cup-inventory 1 "alice")
                             (test-confluence--coffee-menu 1 "alice"))
      (let ((result (test-confluence-search--search
                     "coffee" '("Coffee menu" "Kitchen"))))
        (should (equal (list test-confluence--coffee-menu-address)
                       (plist-get result :opened)))))))

(ert-deftest test-confluence-search-tells-two-pages-with-one-title-apart ()
  "GIVEN a search for coffee that finds two pages with the title Coffee
menu, one in the space Kitchen and one in the space Bar
WHEN you type coffee and choose the row of the space Bar
THEN the prompt offered one row for each page
AND the browser opens the page of the space Bar
WHEN you type coffee again and choose the row of the space Kitchen
THEN the browser opens the page of the space Kitchen."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding
                             "coffee"
                             (test-confluence--coffee-menu 1 "alice")
                             (test-confluence-search--bar-menu))
      (let ((result (test-confluence-search--search "coffee" '("Coffee menu" "Bar"))))
        (should (= 2 (length (plist-get result :rows))))
        (should (equal (list test-confluence-search--bar-menu-address)
                       (plist-get result :opened))))
      (let ((result (test-confluence-search--search "coffee" '("Coffee menu" "Kitchen"))))
        (should (equal (list test-confluence--coffee-menu-address)
                       (plist-get result :opened)))))))

(ert-deftest test-confluence-search-words-that-no-page-holds-open-nothing ()
  "GIVEN the Confluence site in the backends option, and a search for
tea that finds no page
WHEN you run the search and type tea
THEN the prompt offers no row AND no page opens
AND without consult, where no list shows that it is empty, shipit says
that it found no page for tea."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh (test-confluence-search--finding "tea")
      (let ((result (test-confluence-search--search "tea")))
        (should (test-confluence--searches))
        (should-not (plist-get result :rows))
        (should-not (plist-get result :opened))
        (when (eq test-confluence-search--prompt 'emacs)
          (should (string-match-p "tea" (plist-get result :said))))))))

(ert-deftest test-confluence-search-a-refused-search-says-so ()
  "GIVEN the Confluence site in the backends option, and a Confluence
that refuses each search with the status 401
WHEN you run the search and type coffee
THEN shipit says that Confluence refused the search, with the status 401
AND the prompt offers no row AND no page opens."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh test-confluence-search--refusing
      (let ((result (test-confluence-search--search "coffee")))
        (should (test-confluence--searches))
        (should (string-match-p "401" (plist-get result :said)))
        (should-not (plist-get result :rows))
        (should-not (plist-get result :opened))))))

(ert-deftest test-confluence-search-without-a-site-says-what-to-add ()
  "GIVEN no Confluence site in the backends option
WHEN you run the search
THEN shipit says that the option `shipit-issue-repo-backends' needs an
entry for Confluence
AND it asks Confluence nothing AND no page opens."
  (test-confluence-search--at-each-prompt
    (test-confluence--fresh nil
      (let* ((shipit-issue-repo-backends nil)
             (result (test-confluence-search--search "coffee")))
        (should (string-match-p "shipit-issue-repo-backends" (plist-get result :said)))
        (should-not shipit-test-http-requests)
        (should-not (plist-get result :rows))
        (should-not (plist-get result :opened))))))

(ert-deftest test-confluence-search-the-menu-of-shipit-has-the-search ()
  "GIVEN the main menu of shipit
WHEN the key `w' is looked up in it
THEN it is named Search Confluence AND it runs the search
AND it shows with a Confluence site in the backends option, and not
without one."
  (let* ((suffix (cdr (transient-get-suffix 'shipit-menu "w")))
         (shows (plist-get suffix :if)))
    (should (equal "Search Confluence" (plist-get suffix :description)))
    (should (eq 'shipit-confluence-search (plist-get suffix :command)))
    (should (commandp 'shipit-confluence-search))
    (let ((shipit-issue-repo-backends (list test-confluence--entry)))
      (should (funcall shows)))
    (let ((shipit-issue-repo-backends nil))
      (should-not (funcall shows)))))

(provide 'test-shipit-confluence-search)
;;; test-shipit-confluence-search.el ends here
