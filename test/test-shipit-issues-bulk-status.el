;;; test-shipit-issues-bulk-status.el --- Change the state of many issues at once -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The issue search offers its issues as rows that embark can act on.
;; These tests run the action that changes the state of the issues on
;; those rows, the way embark runs it: one call with the list of all
;; the rows.
;;
;; The trackers are replaced where shipit talks to them.  Jira is
;; reached through url.el and logs in through auth-source, so a small
;; fake Jira below takes the place of those two and answers each
;; request by its method, its path and its query.  GitHub is answered
;; at url.el by `shipit-test-http-answering'.  The user is replaced at
;; the prompt, through `completing-read-function'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'url-parse)
(require 'url-http)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)
(require 'shipit-test-http)

;; Embark is not on the load path of the tests.  The tests bind these
;; two the way embark keeps them.
(defvar embark-keymap-alist)
(defvar embark-multitarget-actions)

;;; The rows of the issue search

(defconst test-bulk-status--repo "acme/door"
  "The repository whose issues the tests change.")

(defun test-bulk-status--issue (number)
  "Issue NUMBER as a search of the tracker finds it."
  `((number . ,number)
    (title . ,(format "Fix part %s of the door" number))))

(defun test-bulk-status--rows (numbers)
  "The rows that the issue search of acme/door shows for the issues NUMBERS.
They come from the completion table of the search, as embark takes
them from its minibuffer."
  (all-completions "" (shipit-issues--make-completion-table
                       (mapcar #'test-bulk-status--issue numbers)
                       test-bulk-status--repo
                       (make-hash-table :test 'equal)
                       nil)))

;;; The user at the prompt

(defvar test-bulk-status--answers nil
  "The answers that the user still has to give, in turn.")

(defvar test-bulk-status--prompts nil
  "The prompts that the user saw, oldest first, each as (PROMPT CANDIDATES).")

(defun test-bulk-status--answer (prompt offered)
  "The next answer of the user to PROMPT, which OFFERED the candidates.
The answer `:first' is the first candidate.  A prompt with no answer
left is an error, and so is an answer that is not offered."
  (let ((answer (or (pop test-bulk-status--answers)
                    (error "The user has no answer left for %S" prompt))))
    (cond ((eq answer :first)
           (or (car offered) (error "%S offers nothing to pick" prompt)))
          ((member answer offered) answer)
          (t (error "The user cannot pick %S at %S, which offers %S"
                    answer prompt offered)))))

(defun test-bulk-status--user-reads (prompt collection &optional predicate &rest _)
  "Answer PROMPT as the user does, in place of `completing-read-function'.
Records PROMPT and the candidates of COLLECTION that PREDICATE keeps."
  (let ((offered (all-completions "" collection predicate)))
    (setq test-bulk-status--prompts
          (append test-bulk-status--prompts (list (list prompt offered))))
    (test-bulk-status--answer prompt offered)))

;;; The action

(defun test-bulk-status--action ()
  "The command that `s' runs in embark on rows of the issue search."
  (lookup-key shipit-issues-embark-map (kbd "s")))

(defun test-bulk-status--change-status (rows answers)
  "Run the state action of the issue search on ROWS, as embark does.
Embark hands the action all ROWS in one call.  The user gives ANSWERS
to the prompts, in turn, and the form fails when one is left over.
Returns a plist: `:prompts' are the prompts, each as (PROMPT
CANDIDATES), and `:said' is what `message' printed."
  (let ((test-bulk-status--answers answers)
        (test-bulk-status--prompts nil)
        (completing-read-function #'test-bulk-status--user-reads))
    (ert-with-message-capture said
      (funcall (test-bulk-status--action) rows)
      (when test-bulk-status--answers
        (ert-fail (list "The user had answers for prompts that never came"
                        test-bulk-status--answers)))
      (list :prompts test-bulk-status--prompts :said said))))

(defun test-bulk-status--prompt-count (run)
  "The number of prompts that the user saw in RUN."
  (length (plist-get run :prompts)))

(defun test-bulk-status--offered (run n)
  "The candidates that prompt N of RUN offered, counted from 0."
  (nth 1 (or (nth n (plist-get run :prompts))
             (error "The user saw no prompt %d" n))))

(defun test-bulk-status--message-lines (run)
  "The lines that `message' printed in RUN."
  (split-string (plist-get run :said) "\n" t))

(defun test-bulk-status--last-message (run)
  "The last line that `message' printed in RUN."
  (car (last (test-bulk-status--message-lines run))))

(defun test-bulk-status--said-together-p (run &rest words)
  "Non-nil when one line that `message' printed in RUN holds all WORDS."
  (seq-some (lambda (line)
              (seq-every-p (lambda (word) (string-search word line)) words))
            (test-bulk-status--message-lines run)))

;;; A fake Jira

(defconst test-bulk-status--jira-url "https://acme.atlassian.net"
  "The address of the fake Jira site.")

(defconst test-bulk-status--statuses
  '(("To Do" "1" "new")
    ("In Progress" "2" "indeterminate")
    ("Done" "3" "done"))
  "The states of the fake Jira: the name, the id and the category of each.
A move has the name of the state it goes to.")

(defconst test-bulk-status--with-fields "expand=transitions.fields"
  "The query with which a request for the moves of an issue asks for their fields.")

(defvar test-bulk-status--site nil
  "The fake Jira site that answers now, a plist.
`:moves' is an alist of each issue key and the moves that the issue
offers.  A move that takes no resolution is the name of the state that
it goes to.  A move that takes one is (NAME :resolutions NAMES), with
the names of the resolutions that it allows, in the order of Jira.
`:refusals' is an alist of each issue key whose move Jira refuses and
the words of the refusal.")

(defvar test-bulk-status--requests nil
  "The requests that the fake Jira got, oldest first, as (METHOD PATH BODY).
PATH has no query.  BODY is the JSON that the request sent, as an
alist, or nil.")

(defvar test-bulk-status--unanswered nil
  "The requests that the fake Jira had no answer for, as \"METHOD PATH\".
PATH ends with its query, when it has one.")

(defun test-bulk-status--status (name)
  "The state NAME of the fake Jira, as (NAME ID CATEGORY)."
  (or (assoc name test-bulk-status--statuses)
      (error "The fake Jira has no state %S" name)))

(defun test-bulk-status--moves-of (key)
  "The moves that the issue KEY offers, as `test-bulk-status--site' holds them."
  (cdr (assoc key (plist-get test-bulk-status--site :moves))))

(defun test-bulk-status--name-of-move (move)
  "The name of MOVE, as `test-bulk-status--site' holds it."
  (if (consp move) (car move) move))

(defun test-bulk-status--move-resolutions (move)
  "The resolutions that MOVE allows, in the order of Jira, or nil for none."
  (and (consp move) (plist-get (cdr move) :resolutions)))

(defun test-bulk-status--move-id (key name)
  "The id of the move NAME of the issue KEY.
Each issue has ids of its own, so a move sent with the id of another
issue is not taken for the move of this one."
  (format "%s%s1" (car (last (split-string key "-")))
          (nth 1 (test-bulk-status--status name))))

(defun test-bulk-status--resolution-field (names)
  "The field resolution on the screen of a move that allows the resolutions NAMES."
  `((required . t)
    (name . "Resolution")
    (key . "resolution")
    (operations . ["set"])
    (allowedValues . ,(vconcat (seq-map-indexed
                                (lambda (name index)
                                  `((id . ,(number-to-string (1+ index)))
                                    (name . ,name)))
                                names)))))

(defun test-bulk-status--fields (move)
  "The fields on the screen of MOVE, as Jira lists them.
A move that takes no resolution has no screen, and Jira gives an empty
object for its fields.  `json-encode' writes an empty hash table as
that object, and nil as null."
  (if-let* ((names (test-bulk-status--move-resolutions move)))
      `((resolution . ,(test-bulk-status--resolution-field names)))
    (make-hash-table)))

(defun test-bulk-status--transition (key move with-fields)
  "MOVE of the issue KEY, as Jira lists it.
The fields of the move are there only when WITH-FIELDS is non-nil, as
Jira leaves them out when the request does not ask for them."
  (let ((name (test-bulk-status--name-of-move move)))
    (pcase-let ((`(,_ ,status-id ,category) (test-bulk-status--status name)))
      `((id . ,(test-bulk-status--move-id key name))
        (name . ,name)
        (to . ((name . ,name)
               (id . ,status-id)
               (statusCategory . ((key . ,category)))))
        (hasScreen . ,(if (test-bulk-status--move-resolutions move) t :json-false))
        ,@(when with-fields
            `((fields . ,(test-bulk-status--fields move))))))))

(defun test-bulk-status--transitions (key with-fields)
  "The JSON of the moves that the issue KEY offers.
Their fields are there when WITH-FIELDS is non-nil."
  (json-encode
   `((transitions . ,(vconcat (mapcar (lambda (move)
                                        (test-bulk-status--transition
                                         key move with-fields))
                                      (test-bulk-status--moves-of key)))))))

(defun test-bulk-status--refusal (key)
  "The words with which Jira refuses a move of the issue KEY, or nil."
  (cdr (assoc key (plist-get test-bulk-status--site :refusals))))

(defun test-bulk-status--move-answer (key)
  "Jira's answer to a move of the issue KEY: a refusal, or no content."
  (if-let* ((words (test-bulk-status--refusal key)))
      (list 400 (json-encode `((errorMessages . ,(vector words)))))
    (list 204 "")))

(defun test-bulk-status--issue-of (path)
  "The key of the issue whose moves PATH is about, or nil."
  (when (string-match "\\`/rest/api/2/issue/\\([^/]+\\)/transitions\\'" path)
    (match-string 1 path)))

(defun test-bulk-status--no-answer (method path)
  "Record that the fake Jira has no answer for METHOD on PATH, and signal."
  (push (format "%s %s" method path) test-bulk-status--unanswered)
  (error "The fake Jira has no answer for %s %s" method path))

(defun test-bulk-status--route (method path query)
  "The status and the body with which the fake Jira answers METHOD on PATH.
QUERY is the query of the request, or nil.  A request for the moves of
an issue gets their fields when it asks for them."
  (let ((key (test-bulk-status--issue-of path))
        (with-fields (equal query test-bulk-status--with-fields)))
    (cond
     ((and key (equal method "GET") (or (null query) with-fields)
           (test-bulk-status--moves-of key))
      (list 200 (test-bulk-status--transitions key with-fields)))
     ((and key (equal method "POST") (null query) (test-bulk-status--moves-of key))
      (test-bulk-status--move-answer key))
     (t (test-bulk-status--no-answer
         method (string-join (delq nil (list path query)) "?"))))))

(defun test-bulk-status--response (status body)
  "A buffer that holds a response of STATUS with BODY, as url.el leaves one.
The reader of a response looks for the end of the headers and for the
status in variables of url.el that are local to the buffer."
  (let ((buffer (generate-new-buffer " *test-bulk-status-jira*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 %d\nContent-Type: application/json\n\n" status))
      (setq-local url-http-end-of-headers (copy-marker (1- (point))))
      (setq-local url-http-response-status status)
      (insert (encode-coding-string body 'utf-8)))
    buffer))

(defun test-bulk-status--request-body ()
  "The JSON that the request made now sends, as an alist, or nil."
  (when url-request-data
    (json-parse-string (decode-coding-string url-request-data 'utf-8)
                       :object-type 'alist)))

(defun test-bulk-status--jira-answer (url &rest _)
  "Answer the request of URL as the fake Jira does, and record it.
It takes the place of `url-retrieve-synchronously', so the method and
the body of the request are in the variables of url.el."
  (pcase-let* ((`(,path . ,query) (url-path-and-query (url-generic-parse-url url)))
               (method (or url-request-method "GET")))
    (unless (string-prefix-p (concat test-bulk-status--jira-url "/") url)
      (test-bulk-status--no-answer method url))
    (setq test-bulk-status--requests
          (append test-bulk-status--requests
                  (list (list method path (test-bulk-status--request-body)))))
    (apply #'test-bulk-status--response
           (test-bulk-status--route method path query))))

(defun test-bulk-status--jira-login (&rest _)
  "The login that auth-source finds for the fake Jira."
  (list (list :user "someone@example.com" :secret "secret")))

(defmacro test-bulk-status--with-jira (site &rest body)
  "Run BODY with the issues of acme/door on a fake Jira that holds SITE.
SITE is a plist as `test-bulk-status--site' describes it.  The default
resolution is \"Done\".  The form fails when a request got no answer,
also when the code that made it caught the error."
  (declare (indent 1))
  `(let ((shipit-issue-backend 'github)
         (shipit-issue-backend-config nil)
         (shipit-issue-repo-backends
          (list (list test-bulk-status--repo :backend 'jira
                      :base-url test-bulk-status--jira-url
                      :project-keys '("PRJ"))))
         (shipit-issue-jira-default-resolution "Done")
         (test-bulk-status--site ,site)
         (test-bulk-status--requests nil)
         (test-bulk-status--unanswered nil))
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                #'test-bulk-status--jira-answer)
               ((symbol-function 'auth-source-search)
                #'test-bulk-status--jira-login))
       (prog1 (progn ,@body)
         (when test-bulk-status--unanswered
           (ert-fail (list "The fake Jira had no answer for"
                           (reverse test-bulk-status--unanswered))))))))

(defun test-bulk-status--resolution-sent (body)
  "The resolution that the move with BODY sent: its name, or nil for none.
A resolution that is there but has no name is given as it was sent."
  (let ((resolution (alist-get 'resolution (alist-get 'fields body))))
    (if (consp resolution) (alist-get 'name resolution) resolution)))

(defun test-bulk-status--move-name (key id)
  "The name of the move of the issue KEY whose id is ID.
For an id that the issue does not offer, a text that says so."
  (or (seq-find (lambda (name) (equal id (test-bulk-status--move-id key name)))
                (mapcar #'test-bulk-status--name-of-move
                        (test-bulk-status--moves-of key)))
      (format "no move of %s has the id %s" key id)))

(defun test-bulk-status--move (request)
  "REQUEST as (KEY MOVE RESOLUTION) when it is a move that Jira made, else nil."
  (pcase-let* ((`(,method ,path ,body) request)
               (key (test-bulk-status--issue-of path)))
    (when (and (equal method "POST") key (not (test-bulk-status--refusal key)))
      (list key
            (test-bulk-status--move-name
             key (alist-get 'id (alist-get 'transition body)))
            (test-bulk-status--resolution-sent body)))))

(defun test-bulk-status--jira-moves ()
  "The moves that the fake Jira made, by issue key, as (KEY MOVE RESOLUTION).
MOVE is the name of the move whose id was sent, and RESOLUTION the name
of the resolution sent with it, or nil.  A move that Jira refused is
left out."
  (sort (delq nil (mapcar #'test-bulk-status--move test-bulk-status--requests))
        (lambda (a b) (string< (car a) (car b)))))

;;; GitHub

(defun test-bulk-status--github-answers (numbers)
  "GitHub's answers for the open issues NUMBERS of acme/door.
A change of an issue is answered with the issue."
  (mapcar (lambda (number)
            (list (format "/repos/%s/issues/%d\\'" test-bulk-status--repo number)
                  200 `((number . ,number) (state . "open"))))
          numbers))

(defmacro test-bulk-status--with-github (numbers &rest body)
  "Run BODY with the open issues NUMBERS of acme/door at GitHub.
No entry names a tracker of its own for acme/door, so its issues are at
GitHub."
  (declare (indent 1))
  `(let ((shipit-issue-backend 'github)
         (shipit-issue-backend-config nil)
         (shipit-issue-repo-backends nil)
         (shipit-github-token "test-token"))
     (shipit-test-http-answering (test-bulk-status--github-answers ,numbers)
       ,@body)))

(defun test-bulk-status--github-change (request body)
  "REQUEST that sent BODY as (NUMBER STATE REASON) when it changed an issue.
Nil for any other request."
  (when (and body
             (equal (car request) "PATCH")
             (string-match "/issues/\\([0-9]+\\)\\'" (cadr request)))
    (let ((number (string-to-number (match-string 1 (cadr request))))
          (sent (json-parse-string body :object-type 'alist)))
      (list number (alist-get 'state sent) (alist-get 'state_reason sent)))))

(defun test-bulk-status--github-changes ()
  "The changes of state sent to GitHub, by issue, as (NUMBER STATE REASON)."
  (sort (delq nil (cl-mapcar #'test-bulk-status--github-change
                             shipit-test-http-requests shipit-test-http-bodies))
        (lambda (a b) (< (car a) (car b)))))

;;; The scenarios

(ert-deftest test-shipit-issues-bulk-status-moves-each-issue-to-the-state-picked ()
  "Three Jira issues all go to Done, with the resolution Done."
  ;; GIVEN three Jira issues that each can move to In Progress and to
  ;; Done, as rows of the issue search
  ;; AND the move to Done takes a resolution and allows Done and Won't Do
  ;; WHEN the state action runs on all rows, and the user picks
  ;; `Done (3 of 3)' and then the first resolution offered
  ;; THEN each of the three issues gets one move to Done with the
  ;; resolution "Done"
  ;; AND the message is `Moved 3 issues to Done'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" "In Progress" ("Done" :resolutions ("Done" "Won't Do")))
                ("PRJ-2" "In Progress" ("Done" :resolutions ("Done" "Won't Do")))
                ("PRJ-3" "In Progress" ("Done" :resolutions ("Done" "Won't Do")))))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1" "PRJ-2" "PRJ-3"))
                '("Done (3 of 3)" :first))))
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-1" "Done" "Done")
                       ("PRJ-2" "Done" "Done")
                       ("PRJ-3" "Done" "Done"))))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 3 issues to Done")))))

(ert-deftest test-shipit-issues-bulk-status-leaves-alone-an-issue-that-cannot-go-there ()
  "An issue that does not offer the move picked gets no move."
  ;; GIVEN three Jira issues: PRJ-2 and PRJ-3 can move to Done and to
  ;; To Do, PRJ-1 only to To Do
  ;; AND the move to Done takes a resolution and allows Done and Won't Do
  ;; AND the first row is PRJ-3, whose first move is Done
  ;; WHEN the state action runs on all rows
  ;; THEN the state prompt offers exactly `To Do (3 of 3)' and
  ;; `Done (2 of 3)', in this order: the move that most issues offer
  ;; comes first
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" "To Do")
                ("PRJ-2" ("Done" :resolutions ("Done" "Won't Do")) "To Do")
                ("PRJ-3" ("Done" :resolutions ("Done" "Won't Do")) "To Do")))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1" "PRJ-2" "PRJ-3"))
                '("Done (2 of 3)" :first))))
      (should (equal (test-bulk-status--offered run 0)
                     '("To Do (3 of 3)" "Done (2 of 3)")))
      ;; WHEN the user picks `Done (2 of 3)' and the first resolution
      ;; THEN PRJ-2 and PRJ-3 are moved to Done
      ;; AND PRJ-1 gets no move
      ;; AND the message is
      ;; `Moved 2 issues to Done, 1 cannot go there: PRJ-1'
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-2" "Done" "Done")
                       ("PRJ-3" "Done" "Done"))))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 2 issues to Done, 1 cannot go there: PRJ-1")))))

(ert-deftest test-shipit-issues-bulk-status-offers-the-default-resolution-first ()
  "The resolution prompt offers the default first, and sends the pick."
  ;; GIVEN two Jira issues whose move to Done allows the resolutions
  ;; Won't Do, Done and Duplicate, in this order from Jira
  ;; WHEN the state action runs on both rows and the user picks
  ;; `Done (2 of 2)'
  ;; THEN the resolution prompt offers `Done' first, then `Won't Do'
  ;; and `Duplicate'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" ("Done" :resolutions ("Won't Do" "Done" "Duplicate")))
                ("PRJ-2" ("Done" :resolutions ("Won't Do" "Done" "Duplicate")))))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1" "PRJ-2"))
                '("Done (2 of 2)" "Won't Do"))))
      (should (equal (test-bulk-status--offered run 1)
                     '("Done" "Won't Do" "Duplicate")))
      ;; WHEN the user picks `Won't Do'
      ;; THEN each issue is moved to Done with the resolution "Won't Do"
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-1" "Done" "Won't Do")
                       ("PRJ-2" "Done" "Won't Do")))))))

(ert-deftest test-shipit-issues-bulk-status-asks-no-resolution-for-a-move-that-does-not-close ()
  "A move to In Progress asks for no resolution and sends none."
  ;; GIVEN one Jira issue that can move to In Progress and to Done
  ;; WHEN the state action runs on its one row and the user picks
  ;; `In Progress (1 of 1)'
  ;; THEN no resolution is asked for
  ;; AND the issue is moved to In Progress with no resolution in the
  ;; request
  ;; AND the message is `Moved 1 issue to In Progress'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" "In Progress" "Done")))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1"))
                '("In Progress (1 of 1)"))))
      (should (= 1 (test-bulk-status--prompt-count run)))
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-1" "In Progress" nil))))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 1 issue to In Progress")))))

(ert-deftest test-shipit-issues-bulk-status-sends-no-resolution-for-a-move-that-takes-none ()
  "A move to Done that takes no resolution asks for none and sends none."
  ;; GIVEN two Jira issues whose move to Done takes no resolution
  ;; WHEN the state action runs on both rows and the user picks
  ;; `Done (2 of 2)'
  ;; THEN no resolution is asked for
  ;; AND each issue is moved to Done with no resolution in the request
  ;; AND the message is `Moved 2 issues to Done'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" "Done")
                ("PRJ-2" "Done")))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1" "PRJ-2"))
                '("Done (2 of 2)"))))
      (should (= 1 (test-bulk-status--prompt-count run)))
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-1" "Done" nil)
                       ("PRJ-2" "Done" nil))))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 2 issues to Done")))))

(ert-deftest test-shipit-issues-bulk-status-sends-the-resolution-only-to-a-move-that-takes-one ()
  "Only the issue whose move takes a resolution gets the resolution picked."
  ;; GIVEN PRJ-1, whose move to Done takes a resolution and allows Done
  ;; and Won't Do
  ;; AND PRJ-2, whose move to Done takes none
  ;; WHEN the state action runs on both rows, and the user picks
  ;; `Done (2 of 2)' and then `Won't Do'
  ;; THEN PRJ-1 is moved to Done with the resolution "Won't Do"
  ;; AND PRJ-2 is moved to Done with no resolution
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" ("Done" :resolutions ("Done" "Won't Do")))
                ("PRJ-2" "Done")))
    (test-bulk-status--change-status
     (test-bulk-status--rows '("PRJ-1" "PRJ-2"))
     '("Done (2 of 2)" "Won't Do"))
    (should (equal (test-bulk-status--jira-moves)
                   '(("PRJ-1" "Done" "Won't Do")
                     ("PRJ-2" "Done" nil))))))

(ert-deftest test-shipit-issues-bulk-status-reports-the-issue-that-the-tracker-refused ()
  "A move that the tracker refuses is reported with its words."
  ;; GIVEN three Jira issues that can move to Done, and a tracker that
  ;; refuses the move of PRJ-2 with the words "Not allowed"
  ;; AND the move to Done takes a resolution and allows Done and Won't Do
  ;; WHEN the state action runs on all rows, and the user picks
  ;; `Done (3 of 3)' and the first resolution
  ;; THEN PRJ-1 and PRJ-3 are moved
  ;; AND a message line names PRJ-2 together with the words of the
  ;; tracker
  ;; AND the last message is `Moved 2 issues to Done, 1 failed: PRJ-2'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" ("Done" :resolutions ("Done" "Won't Do")))
                ("PRJ-2" ("Done" :resolutions ("Done" "Won't Do")))
                ("PRJ-3" ("Done" :resolutions ("Done" "Won't Do"))))
        :refusals (("PRJ-2" . "Not allowed")))
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '("PRJ-1" "PRJ-2" "PRJ-3"))
                '("Done (3 of 3)" :first))))
      (should (equal (test-bulk-status--jira-moves)
                     '(("PRJ-1" "Done" "Done")
                       ("PRJ-3" "Done" "Done"))))
      (should (test-bulk-status--said-together-p run "PRJ-2" "Not allowed"))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 2 issues to Done, 1 failed: PRJ-2")))))

(ert-deftest test-shipit-issues-bulk-status-closes-github-issues-as-not-planned ()
  "Two GitHub issues are closed as not planned, with no resolution asked."
  ;; GIVEN two open GitHub issues, 12 and 13, of a repository without a
  ;; Jira entry, as rows of the issue search
  ;; WHEN the state action runs on both rows and the user picks
  ;; `Close as not planned (2 of 2)'
  ;; THEN no resolution is asked for
  ;; AND GitHub is sent the state closed with the reason not planned,
  ;; for each of the two
  ;; AND the message is `Moved 2 issues to Close as not planned'
  (test-bulk-status--with-github '(12 13)
    (let ((run (test-bulk-status--change-status
                (test-bulk-status--rows '(12 13))
                '("Close as not planned (2 of 2)"))))
      (should (= 1 (test-bulk-status--prompt-count run)))
      (should (equal (test-bulk-status--github-changes)
                     '((12 "closed" "not_planned")
                       (13 "closed" "not_planned"))))
      (should (equal (test-bulk-status--last-message run)
                     "Moved 2 issues to Close as not planned")))))

(ert-deftest test-shipit-issues-bulk-status-embark-hands-all-rows-to-the-action ()
  "A row of the issue search gets the state action, which takes all rows."
  ;; GIVEN embark with a keymap list and a list of multi-target actions
  ;; of its own
  ;; WHEN the issue search registers with embark
  ;; THEN a row of the issue search gets the actions of
  ;; `shipit-issues-embark-map'
  ;; AND `s' in that map is the action that changes the state
  ;; AND embark hands all rows to that action in one call
  (let ((embark-keymap-alist (list (cons 'file 'embark-file-map)))
        (embark-multitarget-actions (list 'embark-insert)))
    (shipit-issues--embark-register)
    (should (eq (cdr (assq 'shipit-issue embark-keymap-alist))
                'shipit-issues-embark-map))
    (should (eq (lookup-key shipit-issues-embark-map (kbd "s"))
                'shipit-issues-embark-change-status))
    (should (memq 'shipit-issues-embark-change-status
                  embark-multitarget-actions))))

(provide 'test-shipit-issues-bulk-status)
;;; test-shipit-issues-bulk-status.el ends here
