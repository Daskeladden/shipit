;;; test-shipit-confluence-search-marginalia.el --- The search of Confluence with marginalia -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the search of Confluence pages when marginalia is on: the
;; name of the space of a page and the time of its last change show as
;; columns of marginalia beside the row of the page.  `marginalia-cycle'
;; steps through two views: the space, the last editor and the time,
;; then the text around the words found before those three columns.
;; After the second view, nothing shows beside the rows.
;;
;; Marginalia is the real package, from the packages of the tests.  A
;; list of completions asks the prompt for the text beside its rows, and
;; the tests ask in the same way, in a real minibuffer.
;;
;; Each scenario runs twice, as in `test-shipit-confluence-search.el'.
;; The packages of the tests hold no consult, so a stand-in takes its
;; two functions there.  It shows the rows in a minibuffer of Emacs,
;; whose list has the category and the annotation that the search gave
;; to consult.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'subr-x)
(require 'parse-time)
(require 'minibuffer)
(require 'marginalia)
(require 'test-shipit-notifications-confluence)
(require 'test-shipit-confluence-search)
(require 'shipit-confluence-search nil t)

(defconst test-confluence-search-marginalia--long-space
  "The pantry behind the kitchen of the old house"
  "The name of a space that is longer than the column of the space.")

;;; The text beside the rows of a prompt

(defvar test-confluence-search-marginalia--shown nil
  "What the prompt showed at the newest search, top row first.
Each element is (ROW . BESIDE): the text of a row, and the text that the
list of the prompt shows beside it.")

(defun test-confluence-search-marginalia--beside (rows metadata)
  "The text that a list of completions shows beside each of ROWS.
METADATA is the metadata of the prompt.  A list asks first for the
affixes of its rows, which is what marginalia gives, and else for the
annotation of each row."
  (if-let* ((affixes (completion-metadata-get metadata 'affixation-function)))
      (mapcar (lambda (affixed) (nth 2 affixed)) (funcall affixes rows))
    (let ((annotate (completion-metadata-get metadata 'annotation-function)))
      (mapcar (lambda (row) (or (and annotate (funcall annotate row)) "")) rows))))

(defun test-confluence-search-marginalia--look (column presses)
  "Look at the rows of the prompt of pages, and write one of them into it.
First run `marginalia-cycle', the command that a user binds to M-A,
PRESSES times.  The row written is the one with COLUMN in the text
beside it, when COLUMN is given.  Does nothing in a prompt that
completes nothing, as the one of the words."
  (when minibuffer-completion-table
    (dotimes (_ (or presses 0))
      (call-interactively #'marginalia-cycle))
    (let* ((table minibuffer-completion-table)
           (predicate minibuffer-completion-predicate)
           (metadata (completion-metadata "" table predicate))
           (rows (test-confluence-search--as-shown
                  (all-completions "" table predicate)
                  (completion-metadata-get metadata 'display-sort-function)))
           (shown (cl-mapcar #'cons rows
                             (test-confluence-search-marginalia--beside rows metadata))))
      (setq test-confluence-search-marginalia--shown shown)
      (when-let* ((row (and column
                            (car (seq-find (lambda (row) (string-search column (cdr row)))
                                           shown)))))
        (insert row)))))

(defun test-confluence-search-marginalia--place (beside)
  "The place in the window where the columns of BESIDE start, or nil.
Marginalia puts a stretch of space before its columns, and the stretch
reaches up to that place."
  (when-let* ((at (text-property-not-all 0 (length beside) 'display nil beside))
              (display (get-text-property at 'display beside)))
    (and (eq 'space (car-safe display))
         (plist-get (cdr display) :align-to))))

(defun test-confluence-search-marginalia--date (page)
  "The time of the last change of PAGE, as shipit writes a date."
  (format-time-string "%Y-%m-%d %H:%M"
                      (parse-iso8601-time-string (alist-get 'lastModified page))))

(defun test-confluence-search-marginalia--with-excerpt (page excerpt)
  "PAGE as the search gives it when the text around the words found is EXCERPT."
  (let ((page (copy-tree page)))
    (setf (alist-get 'excerpt page) excerpt)
    page))

;;; The prompts of Emacs

(defun test-confluence-search-marginalia--with-emacs (words column)
  "Search for WORDS at the prompts of Emacs, and choose the row with COLUMN.
The keys go to a real minibuffer.  The first prompt gets WORDS and RET.
At the prompt of pages the row with COLUMN beside it is written, as a
move to that row does, and RET chooses it.  With no COLUMN, C-g leaves
that prompt."
  (ert-simulate-keys (append (string-to-list words)
                             (list ?\r)
                             (and column (list ?\r)))
    (call-interactively #'shipit-confluence-search)))

;;; The prompt of consult

(defun test-confluence-search-marginalia--list-of-consult (rows options)
  "The list of completions that consult makes of ROWS for a prompt with OPTIONS.
It has the category and the annotation of OPTIONS, and it keeps the
order of ROWS when OPTIONS tell consult not to order them."
  (let ((metadata `(metadata
                    (category . ,(plist-get options :category))
                    (annotation-function . ,(plist-get options :annotate))
                    ,@(unless (test-confluence-search--consult-orders-p options)
                        '((display-sort-function . identity)
                          (cycle-sort-function . identity))))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action rows string predicate)))))

(defun test-confluence-search-marginalia--consult-read (rows-of options words column)
  "What `consult--read' gives when the user types WORDS and chooses a row.
ROWS-OF is the function of the search that gives the rows of what was
typed, and OPTIONS are the options of the prompt.  The rows show in a
real minibuffer, in the list of
`test-confluence-search-marginalia--list-of-consult'.  The row with
COLUMN beside it comes back as text without properties, as a prompt
gives it.  With no COLUMN the prompt quits, as C-g does."
  (let ((table (test-confluence-search-marginalia--list-of-consult
                (funcall rows-of words) options)))
    (ert-simulate-keys (and column (list ?\r))
      (substring-no-properties
       (completing-read (plist-get options :prompt) table nil t)))))

(defun test-confluence-search-marginalia--with-consult (words column)
  "Search for WORDS at the prompt of consult, and choose the row with COLUMN.
A stand-in takes the two functions of consult that the search uses, see
`test-confluence-search-marginalia--consult-read'.  Emacs keeps
`features' out of the special variables, so a `let' would not make
consult a feature."
  (cl-letf (((symbol-value 'features) (cons 'consult features))
            ((symbol-function 'consult--dynamic-collection)
             (lambda (rows-of &rest _options) rows-of))
            ((symbol-function 'consult--read)
             (lambda (rows-of &rest options)
               (test-confluence-search-marginalia--consult-read
                rows-of options words column))))
    (call-interactively #'shipit-confluence-search)))

;;; What the user does

(defmacro test-confluence-search-marginalia--with-marginalia (&rest body)
  "Run BODY with marginalia on, and its annotations at the right of the window.
`marginalia-cycle' changes the order of the annotators, which it keeps
for later prompts.  BODY changes a copy of them, so each test starts at
the first view."
  (declare (indent 0))
  `(let ((marginalia-align 'right)
         (marginalia-annotators (copy-tree marginalia-annotators)))
     (marginalia-mode 1)
     (unwind-protect
         (progn ,@body)
       (marginalia-mode -1))))

(defun test-confluence-search-marginalia--search (words &rest how)
  "Run the search as the user does: type WORDS and look at the rows.
HOW is a plist.  With `:presses' N the user presses M-A N times at the
prompt of pages before looking.  With `:choose' COLUMN the user chooses
the row that has COLUMN in the text beside it, and else leaves the
prompt.  Gives a plist: `:shown' is what the prompt showed, see
`test-confluence-search-marginalia--shown', and `:opened' the addresses
that the browser was asked for."
  (let* ((opened nil)
         (browse-url-browser-function (lambda (url &rest _) (push url opened)))
         (column (plist-get how :choose))
         (presses (plist-get how :presses))
         (minibuffer-setup-hook
          (cons (lambda () (test-confluence-search-marginalia--look column presses))
                minibuffer-setup-hook)))
    (setq test-confluence-search-marginalia--shown nil)
    (condition-case nil
        (funcall (if (eq test-confluence-search--prompt 'consult)
                     #'test-confluence-search-marginalia--with-consult
                   #'test-confluence-search-marginalia--with-emacs)
                 words column)
      (quit nil))
    (list :shown test-confluence-search-marginalia--shown
          :opened (nreverse opened))))

;;; The scenarios

(ert-deftest test-confluence-search-marginalia-the-space-the-editor-and-the-time-are-columns ()
  "GIVEN marginalia is on, with its annotations at the right of the window
AND a search for coffee that finds the page Cup inventory of the space
Kitchen, which alice changed last, and then the page Coffee menu of the
space Bar, which bob changed last
WHEN you type coffee and look at the rows
THEN each row is the title of its page, without the name of its space
AND beside each row are three columns of marginalia: the name of the
space of the page, the name of the user who made the last change, and
the time of that change as a date
AND the columns start at the same place in both rows, so the names of
the spaces are under each other, and the names of the users are too."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (let ((cups (test-confluence--cup-inventory 1 "alice"))
            (menu (test-confluence-search--in-space
                   (test-confluence--page "201" "Coffee menu" 2 "bob") "BAR" "Bar"))
            (shipit-timestamp-format 'absolute))
        (test-confluence--fresh (test-confluence-search--finding "coffee" cups menu)
          (let* ((shown (plist-get (test-confluence-search-marginalia--search "coffee")
                                   :shown))
                 (beside-cups (cdr (nth 0 shown)))
                 (beside-menu (cdr (nth 1 shown))))
            (should (equal '("Cup inventory" "Coffee menu") (mapcar #'car shown)))
            (should (string-match-p
                     (concat "Kitchen +alice +"
                             (regexp-quote (test-confluence-search-marginalia--date cups))
                             "\\'")
                     beside-cups))
            (should (string-match-p
                     (concat "Bar +bob +"
                             (regexp-quote (test-confluence-search-marginalia--date menu))
                             "\\'")
                     beside-menu))
            (should (eq 'right
                        (cadr (test-confluence-search-marginalia--place beside-cups))))
            (should (equal (test-confluence-search-marginalia--place beside-cups)
                           (test-confluence-search-marginalia--place beside-menu)))
            (should (= (string-search "Kitchen" beside-cups)
                       (string-search "Bar" beside-menu)))
            (should (= (string-search "alice" beside-cups)
                       (string-search "bob" beside-menu)))))))))

(ert-deftest test-confluence-search-marginalia-the-time-is-written-as-shipit-writes-times ()
  "GIVEN marginalia is on, and the times of shipit are set to how long ago
AND a search for coffee that finds the page Coffee menu of the space
Kitchen, which alice changed 29 minutes ago
WHEN you type coffee and look at the rows
THEN the column of the time, behind the columns of the space and of the
user, says 29m ago."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (let ((shipit-timestamp-format 'relative))
        (test-confluence--fresh (test-confluence-search--finding
                                 "coffee" (test-confluence--coffee-menu 1 "alice"))
          (let ((shown (plist-get (test-confluence-search-marginalia--search "coffee")
                                  :shown)))
            (should (= 1 (length shown)))
            (should (string-match-p "Kitchen +alice +29m ago\\'"
                                    (cdr (nth 0 shown))))))))))

(ert-deftest test-confluence-search-marginalia-the-space-tells-two-pages-with-one-title-apart ()
  "GIVEN marginalia is on
AND a search for coffee that finds two pages with the title Coffee menu,
one in the space Kitchen and one in the space Bar
WHEN you type coffee and choose the row with Bar in its column
THEN the prompt offered one row for each page, and the two rows differ
AND the browser opens the page of the space Bar
WHEN you type coffee again and choose the row with Kitchen in its column
THEN the browser opens the page of the space Kitchen."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (test-confluence--fresh (test-confluence-search--finding
                               "coffee"
                               (test-confluence--coffee-menu 1 "alice")
                               (test-confluence-search--bar-menu))
        (let* ((result (test-confluence-search-marginalia--search "coffee" :choose "Bar"))
               (rows (mapcar #'car (plist-get result :shown))))
          (should (= 2 (length rows)))
          (should-not (equal (nth 0 rows) (nth 1 rows)))
          (should (equal (list test-confluence-search--bar-menu-address)
                         (plist-get result :opened))))
        (let ((result (test-confluence-search-marginalia--search "coffee" :choose "Kitchen")))
          (should (equal (list test-confluence--coffee-menu-address)
                         (plist-get result :opened))))))))

(ert-deftest test-confluence-search-marginalia-past-the-second-view-the-row-names-the-space ()
  "GIVEN marginalia is on
AND a search for coffee that finds the page Cup inventory of the space
Kitchen
WHEN you type coffee, press M-A twice, once for each view, and leave
the prompt
AND you type coffee again and look at the rows
THEN the row is the title of the page and the name of its space, as it
is without marginalia
AND nothing shows beside the row."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (test-confluence--fresh (test-confluence-search--finding
                               "coffee" (test-confluence--cup-inventory 1 "alice"))
        (test-confluence-search-marginalia--search "coffee" :presses 2)
        (let ((shown (plist-get (test-confluence-search-marginalia--search "coffee")
                                :shown)))
          (should (= 1 (length shown)))
          (should (test-confluence-search--row-p
                   (car (nth 0 shown)) "Cup inventory" "Kitchen"))
          (should (string-blank-p (cdr (nth 0 shown)))))))))

(ert-deftest test-confluence-search-marginalia-a-long-name-of-a-space-is-cut ()
  "GIVEN marginalia is on
AND a search for coffee that finds the page Cup inventory, in a space
whose name is longer than 25 characters, and then the page Coffee menu
of the space Kitchen
WHEN you type coffee and look at the rows
THEN the column of the space is 25 characters wide: it starts with the
long name and does not hold all of it
AND the names of the users of both rows are still under each other, and
the times are too."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (let* ((name test-confluence-search-marginalia--long-space)
             (cups (test-confluence-search--in-space
                    (test-confluence--cup-inventory 1 "alice") "PANTRY" name))
             (menu (test-confluence--coffee-menu 1 "alice"))
             (date (test-confluence-search-marginalia--date cups))
             (shipit-timestamp-format 'absolute))
        (test-confluence--fresh (test-confluence-search--finding "coffee" cups menu)
          (let* ((shown (plist-get (test-confluence-search-marginalia--search "coffee")
                                   :shown))
                 (beside-cups (cdr (nth 0 shown)))
                 (beside-menu (cdr (nth 1 shown)))
                 (starts (string-search (substring name 0 20) beside-cups)))
            (should starts)
            (should-not (string-search name beside-cups))
            (should (= 25 (- (string-search "alice" beside-cups)
                             starts
                             (length marginalia-separator))))
            (should (= (string-search "alice" beside-cups)
                       (string-search "alice" beside-menu)))
            (should (= (string-search date beside-cups)
                       (string-search (test-confluence-search-marginalia--date menu)
                                      beside-menu)))))))))

(ert-deftest test-confluence-search-marginalia-a-reload-gives-marginalia-the-views-of-now ()
  "GIVEN marginalia is on, and it holds the views of the pages from an
earlier load of shipit, which named a view that shipit no longer has
WHEN shipit loads again, as `shipit-reload' does
AND you type coffee and look at the rows
THEN the rows show the columns of the first view of the shipit loaded
now: the space, the last editor and the time."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (setq marginalia-annotators
            (cons (list 'shipit-confluence-page
                        'shipit-confluence-search-a-view-that-is-gone 'none)
                  (assq-delete-all 'shipit-confluence-page marginalia-annotators)))
      (load "shipit-confluence-search" nil t)
      (test-confluence--fresh (test-confluence-search--finding
                               "coffee" (test-confluence--cup-inventory 1 "alice"))
        (let ((shown (plist-get (test-confluence-search-marginalia--search "coffee")
                                :shown)))
          (should (equal '("Cup inventory") (mapcar #'car shown)))
          (should (string-match-p "\\`[[:space:]]*Kitchen +alice +" (cdr (nth 0 shown)))))))))

;;; The second view: the text, the space, the user and the time

(defconst test-confluence-search-marginalia--long-text
  (concat "Fresh @@@hl@@@coffee@@@endhl@@@ beans arrive on Mondays, and the"
          " grinder by the window is cleaned on Fridays after the last cup")
  "A text around the word coffee that is longer than any window.")

(defun test-confluence-search-marginalia--short-page (id title author)
  "The page ID with the short TITLE in the space Kitchen, which AUTHOR changed.
The search marks coffee in its text, which is longer than any window.
The window of the tests is 80 columns, so a short title leaves room for
the text beside the other columns."
  (test-confluence-search-marginalia--with-excerpt
   (test-confluence--page id title 1 author)
   test-confluence-search-marginalia--long-text))

(defun test-confluence-search-marginalia--search-measured (words &rest how)
  "Like `test-confluence-search-marginalia--search' with WORDS and HOW.
The plist that it gives also has `:width', the width of the window of
the minibuffer at the prompt of pages."
  (let* ((width nil)
         (minibuffer-setup-hook
          (cons (lambda ()
                  (when minibuffer-completion-table
                    (setq width (window-width (active-minibuffer-window)))))
                minibuffer-setup-hook))
         (result (apply #'test-confluence-search-marginalia--search words how)))
    (plist-put result :width width)))

(ert-deftest test-confluence-search-marginalia-the-second-view-puts-the-text-first ()
  "GIVEN marginalia is on, and the times of shipit are set to how long ago
AND a search for coffee that finds the page Cups of the space Kitchen,
which alice changed 29 minutes ago, where the search marks the word
coffee in a long text around it
WHEN you type coffee and look at the rows
AND you type coffee again and press M-A once
THEN the second time, beside the row are the start of the text, without
the marks of the search, and then the space, the user and the time
AND the space, the user and the time stand where they stood the first
time."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (let ((shipit-timestamp-format 'relative))
        (test-confluence--fresh (test-confluence-search--finding
                                 "coffee"
                                 (test-confluence-search-marginalia--short-page
                                  "102" "Cups" "alice"))
          (let* ((first (cdr (nth 0 (plist-get (test-confluence-search-marginalia--search
                                                "coffee")
                                               :shown))))
                 (second (cdr (nth 0 (plist-get (test-confluence-search-marginalia--search
                                                 "coffee" :presses 1)
                                                :shown)))))
            (should (string-match-p "\\`[[:space:]]*Fresh.* +Kitchen +alice +29m ago\\'"
                                    second))
            (should-not (string-search "@@@" second))
            (should (= (- (length first) (string-search "Kitchen" first))
                       (- (length second) (string-search "Kitchen" second))))))))))

(ert-deftest test-confluence-search-marginalia-the-text-is-cut-to-the-room-left ()
  "GIVEN marginalia is on
AND a search for coffee that finds the pages Cups and Menus of the space
Kitchen, whose texts around the word found are longer than the window
WHEN you type coffee and press M-A once
THEN each row and the columns beside it fit in the window: the text,
the space, the user and the time
AND the text of each page is cut, and shows its start
AND the column of the text is equally wide in both rows, so the spaces,
the users and the times are under each other."
  (test-confluence-search--at-each-prompt
    (test-confluence-search-marginalia--with-marginalia
      (test-confluence--fresh (test-confluence-search--finding
                               "coffee"
                               (test-confluence-search-marginalia--short-page
                                "102" "Cups" "alice")
                               (test-confluence-search-marginalia--short-page
                                "103" "Menus" "bob"))
        (let* ((result (test-confluence-search-marginalia--search-measured
                        "coffee" :presses 1))
               (shown (plist-get result :shown))
               (width (plist-get result :width)))
          (should (= 2 (length shown)))
          (dolist (row shown)
            (should (string-match-p "Fresh.* +Kitchen +\\(?:alice\\|bob\\) +[^ ]"
                                    (cdr row)))
            (should (< (+ (string-width (car row)) (string-width (cdr row))) width))
            (should-not (string-search "last cup" (cdr row))))
          (should (= (string-search "Kitchen" (cdr (nth 0 shown)))
                     (string-search "Kitchen" (cdr (nth 1 shown))))))))))

(provide 'test-shipit-confluence-search-marginalia)
;;; test-shipit-confluence-search-marginalia.el ends here
