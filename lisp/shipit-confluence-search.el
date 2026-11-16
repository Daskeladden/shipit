;;; shipit-confluence-search.el --- Search the pages of Confluence Cloud -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `shipit-confluence-search' asks for words, offers the pages and blog
;; posts that the search of Confluence Cloud finds for them, and opens
;; the chosen page in the browser.  The site is the entry of
;; `shipit-issue-repo-backends' with `:backend confluence', the same one
;; that the notifications of Confluence use.
;;
;; With consult the rows follow the typing.  Without consult there are
;; two prompts: one for the words, then one for the pages.  One prompt of
;; Emacs would match the typing against the rows again, and so hide each
;; page whose title lacks the words.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'shipit-core)
(require 'shipit-notifications-confluence)

(declare-function consult--read "consult")
(declare-function consult--dynamic-collection "consult")
(defvar marginalia-annotators)
(defvar marginalia-mode)
(defvar marginalia-separator)

(defconst shipit-confluence-search--limit 25
  "How many pages one search offers.")

(defvar shipit-confluence-search--history nil
  "The words of the earlier searches of Confluence.")

(defvar shipit-confluence-search--pages nil
  "The table from the text of a row to its page, while a search asks.
The annotators of marginalia get only the text of a row, so they find
its page here.")

;;; The site

;;;###autoload
(defun shipit-confluence-search-available-p ()
  "Return non-nil when a Confluence site is in `shipit-issue-repo-backends'.
The menu of shipit shows the search only then.  The name of the backend
stays in this file, so `shipit.el' does not need to know it."
  (and (shipit-confluence--configs) t))

(defun shipit-confluence-search--ask-site (configs)
  "Ask which of CONFIGS, the configs of Confluence sites, to search.
Returns the config of the chosen site."
  (let* ((address (lambda (config) (plist-get config :base-url)))
         (chosen (completing-read "Confluence site: " (mapcar address configs) nil t)))
    (or (cl-find chosen configs :key address :test #'equal)
        (user-error "No Confluence site chosen"))))

(defun shipit-confluence-search--site ()
  "The config of the Confluence site to search.
With several sites in `shipit-issue-repo-backends', ask which."
  (let ((configs (shipit-confluence--configs)))
    (unless configs
      (user-error "No Confluence site: add an entry with :backend confluence \
and a :base-url to `shipit-issue-repo-backends'"))
    (mapc #'shipit-confluence--check-config configs)
    (if (cdr configs)
        (shipit-confluence-search--ask-site configs)
      (car configs))))

;;; The rows

(defun shipit-confluence-search--row (page)
  "The text of the row of PAGE: its title, then the name of its space, dim.
When marginalia shows the space in a column, the row is the title only."
  (let-alist page
    (if (and .space (not (shipit-confluence-search--in-columns-p)))
        (concat .title "  " (propertize .space 'face 'shadow))
      .title)))

(defun shipit-confluence-search--add-row (pages page)
  "Add PAGE to PAGES, the table from the text of a row to its page.
Returns the text of the row.  When another page has that text already,
the row of PAGE ends with the id of PAGE, so that no two pages share a
row."
  (let* ((row (shipit-confluence-search--row page))
         (held (gethash row pages))
         (row (if (and held (not (equal (alist-get 'id held) (alist-get 'id page))))
                  (format "%s (%s)" row (alist-get 'id page))
                row)))
    (puthash row page pages)
    row))

(defun shipit-confluence-search--rows (config pages words)
  "The rows of the pages of the site of CONFIG that hold WORDS.
Each row is added to PAGES, the table from the text of a row to its page."
  (mapcar (lambda (page) (shipit-confluence-search--add-row pages page))
          (shipit-confluence--find-pages config words shipit-confluence-search--limit)))

(defun shipit-confluence-search--annotation (pages row)
  "The annotation of ROW: the time of the last change of its page in PAGES."
  (concat "  " (shipit--format-timestamp (alist-get 'changed (gethash row pages)))))

(defun shipit-confluence-search--chosen (pages row)
  "The page in PAGES of ROW, the row that a prompt gave back, or nil.
ROW is nil or empty when nothing was chosen.  A prompt can give back the
text of a row without its properties, so the page is found by the text."
  (cond
   ((or (null row) (string-empty-p row)) nil)
   ((gethash row pages))
   (t (error "shipit: the prompt gave back %S, which is no row of a page" row))))

;;; The columns of marginalia

(defconst shipit-confluence-search--views
  '(shipit-confluence-search-space-editor-and-time
    shipit-confluence-search-text-space-editor-and-time)
  "The annotators of marginalia for the rows of pages.
`marginalia-cycle' shows them in this order, and then no columns.  Each
of them shows the space, because the row then holds only the title.")

(defconst shipit-confluence-search--space-width 25
  "How many characters the column of the space holds.")

(defconst shipit-confluence-search--editor-width 15
  "How many characters the column of the last editor holds.")

(defconst shipit-confluence-search--time-width 16
  "How many characters the column of the time holds.
A date and a time, as 2026-11-16 19:29, fill it.")

(defun shipit-confluence-search--in-columns-p ()
  "Whether marginalia shows the space of each page in a column.
This is so when marginalia is on and its annotator of the pages is one
of `shipit-confluence-search--views'.  `marginalia-cycle' changes which
annotator that is."
  (and (bound-and-true-p marginalia-mode)
       (memq (cadr (assq 'shipit-confluence-page marginalia-annotators))
             shipit-confluence-search--views)
       t))

(defun shipit-confluence-search--fields (&rest fields)
  "FIELDS as columns of marginalia, each one as `marginalia--fields' takes it.
`marginalia--fields' is a macro.  This file is compiled without
marginalia, so the macro is expanded here, when marginalia calls an
annotator and so is loaded."
  (eval `(marginalia--fields ,@fields) t))

(defun shipit-confluence-search--space-field (page)
  "The column of the name of the space of PAGE."
  `(,(or (alist-get 'space page) "")
    :truncate ,shipit-confluence-search--space-width :face 'marginalia-type))

(defun shipit-confluence-search--editor-field (page)
  "The column of the name of the user who made the last change of PAGE."
  `(,(or (alist-get 'editor page) "")
    :truncate ,shipit-confluence-search--editor-width :face 'marginalia-value))

(defun shipit-confluence-search--time-field (page)
  "The column of the time of the last change of PAGE.
The time follows `shipit-timestamp-format'."
  `(,(shipit--format-timestamp (alist-get 'changed page))
    :width ,(- shipit-confluence-search--time-width) :face 'marginalia-date))

(defun shipit-confluence-search--window-width ()
  "The width of the narrowest window that shows the prompt of pages.
Without an active minibuffer, the width of the selected window."
  (if-let* ((minibuffer (active-minibuffer-window)))
      (cl-loop for window in (get-buffer-window-list (window-buffer minibuffer) nil t)
               minimize (window-width window))
    (window-width)))

(defun shipit-confluence-search--text-width ()
  "How many characters the column of the text holds.
It is the room that the widest row of the search leaves in the window
beside the space, the editor and the time, so that each row fits.  All
rows get this width, so that their other columns stay under each other."
  (let ((widest (cl-loop for row being the hash-keys of shipit-confluence-search--pages
                         maximize (string-width row)))
        (others (+ shipit-confluence-search--space-width
                   shipit-confluence-search--editor-width
                   shipit-confluence-search--time-width))
        ;; Marginalia puts a separator before each of the four columns,
        ;; and one character before all of them that aligns them.  The
        ;; last column of the window stays free.
        (gaps (+ 2 (* 4 (string-width marginalia-separator)))))
    (max 0 (- (shipit-confluence-search--window-width) widest others gaps))))

(defun shipit-confluence-search--text-field (page)
  "The column of the text around the words found in PAGE, on one line.
The text is cut to `shipit-confluence-search--text-width'."
  `(,(string-join (split-string (or (alist-get 'excerpt page) "")) " ")
    :truncate ,(shipit-confluence-search--text-width) :face 'marginalia-documentation))

(defun shipit-confluence-search--columns (row &rest columns)
  "The text beside ROW: the COLUMNS of its page, or nil when ROW has no page.
Each of COLUMNS is a function that gives a field of the page."
  (when-let* ((page (and shipit-confluence-search--pages
                         (gethash row shipit-confluence-search--pages))))
    (apply #'shipit-confluence-search--fields
           (mapcar (lambda (column) (funcall column page)) columns))))

(defun shipit-confluence-search-space-editor-and-time (row)
  "Show beside ROW the space of its page, its last editor, and the time.
An annotator of marginalia for the search of Confluence."
  (shipit-confluence-search--columns row
                                     #'shipit-confluence-search--space-field
                                     #'shipit-confluence-search--editor-field
                                     #'shipit-confluence-search--time-field))

(defun shipit-confluence-search-text-space-editor-and-time (row)
  "Show beside ROW the text found, the space, the last editor and the time.
The text is the one around the words found in the page of ROW.  An
annotator of marginalia for the search of Confluence."
  (shipit-confluence-search--columns row
                                     #'shipit-confluence-search--text-field
                                     #'shipit-confluence-search--space-field
                                     #'shipit-confluence-search--editor-field
                                     #'shipit-confluence-search--time-field))

;; Each load sets the views, so that a reload replaces those of the
;; shipit loaded before, which can name a view that is gone.  To change
;; them, set the entry after this file is loaded.
(with-eval-after-load 'marginalia
  (setf (alist-get 'shipit-confluence-page marginalia-annotators)
        (append shipit-confluence-search--views '(none))))

;;; With consult

(defun shipit-confluence-search--fetch (config pages)
  "The function that gives consult the rows for the words typed.
It searches the site of CONFIG and adds each row to PAGES.  It never
signals, because consult calls it from a timer, where an error reaches
nobody.  So an error is a message, a line in the debug log, and no rows."
  (lambda (words)
    (condition-case err
        ;; Consult drops this work when a key arrives.  url.el leaves
        ;; the buffer of a request alive when the request is dropped
        ;; half-way.  So the request does not listen for keys: a key
        ;; that arrives waits until the answer is there.
        (let ((throw-on-input nil))
          (shipit-confluence-search--rows config pages words))
      (error
       (shipit--debug-log "Confluence: the search for %S failed: %S" words err)
       (message "%s" (error-message-string err))
       nil))))

(defun shipit-confluence-search--choose-with-consult (config pages)
  "Ask with consult for a page of the site of CONFIG; return the chosen row.
The rows follow the typing.  Each row is added to PAGES."
  (consult--read (consult--dynamic-collection
                  (shipit-confluence-search--fetch config pages))
                 :prompt "Search Confluence: "
                 :sort nil
                 :require-match t
                 :category 'shipit-confluence-page
                 :annotate (apply-partially #'shipit-confluence-search--annotation pages)
                 :history '(:input shipit-confluence-search--history)))

;;; Without consult

(defun shipit-confluence-search--read-words ()
  "Ask for the words to search for, and signal when there are none."
  (let ((words (string-trim (read-string "Search Confluence: " nil
                                         'shipit-confluence-search--history))))
    (when (string-empty-p words)
      (user-error "Type the words to search for"))
    words))

(defun shipit-confluence-search--table (rows pages)
  "A completion table of ROWS that keeps their order.
The annotation of a row is the time of the last change of its page in
PAGES."
  (let ((metadata
         `(metadata (category . shipit-confluence-page)
                    ;; Without these the prompt sorts the rows itself.
                    (display-sort-function . identity)
                    (cycle-sort-function . identity)
                    (annotation-function
                     . ,(apply-partially #'shipit-confluence-search--annotation pages)))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action rows string predicate)))))

(defun shipit-confluence-search--choose-with-emacs (config pages)
  "Ask for words, then for a page of the site of CONFIG that holds them.
Returns the chosen row, or nil when the search found no page.  Each row
is added to PAGES."
  (let* ((words (shipit-confluence-search--read-words))
         (rows (shipit-confluence-search--rows config pages words)))
    (if rows
        (completing-read "Page: " (shipit-confluence-search--table rows pages) nil t)
      (message "Confluence found no page for %s" words)
      nil)))

;;; The command

(defun shipit-confluence-search--choose (config pages)
  "Ask for a page of the site of CONFIG; return the chosen row, or nil.
Each row that the prompt offers is added to PAGES."
  (if (shipit--consult-available-p 'consult--read 'consult--dynamic-collection)
      (shipit-confluence-search--choose-with-consult config pages)
    (shipit-confluence-search--choose-with-emacs config pages)))

;;;###autoload
(defun shipit-confluence-search ()
  "Search the pages of Confluence for words, and open the chosen page.
The prompt offers the pages and blog posts whose title or text holds the
words, the best match first.  RET opens the page of a row in the
browser.  The site is the entry of `shipit-issue-repo-backends' with
`:backend confluence', or the one you choose when there are several.
With marginalia on, the space, the last editor and the time show in
columns beside the title, and `marginalia-cycle' shows the text around
the words found before them."
  (interactive)
  (let* ((config (shipit-confluence-search--site))
         ;; Each search adds the rows of its answer and removes none:
         ;; consult can still show the rows of an earlier answer while
         ;; the typing interrupts a newer request.
         (pages (make-hash-table :test 'equal))
         (shipit-confluence-search--pages pages)
         (page (shipit-confluence-search--chosen
                pages (shipit-confluence-search--choose config pages))))
    (when page
      (browse-url (alist-get 'url page)))))

(provide 'shipit-confluence-search)
;;; shipit-confluence-search.el ends here
