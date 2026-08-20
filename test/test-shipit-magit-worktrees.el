;;; test-shipit-magit-worktrees.el --- Tests for the magit worktree section -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pull request status shipit adds to magit's `Worktrees'
;; section.  Verifies that:
;; 1. a worktree's pull request number is read off its directory name
;; 2. one batched GraphQL query covers every worktree on screen
;; 3. states parse, and terminal ones are cached without expiry
;; 4. the margin text fits the margin, and size follows magit's details
;; 5. rows carry `shipit-pr-number', so shipit's point-based commands
;;    and the dwim handler work on them
;; 6. a number the forge has no pull request for is remembered too, so
;;    the next refresh does not ask again; a failed request refreshes
;;    nothing, for the same reason

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-magit-worktrees)

;;;; Reading a worktree

(ert-deftest test-shipit-magit-worktrees-number-from-name ()
  "Worktree directories shipit created carry their PR number."
  (should (= (shipit-magit-worktrees--number
              "/repo/.worktrees/pr-20357-MISC-add-constant/")
             20357))
  (should (= (shipit-magit-worktrees--number "/repo/.worktrees/pr-1-dummy") 1)))

(ert-deftest test-shipit-magit-worktrees-number-ignores-others ()
  "Worktrees that are not per pull request carry no number."
  (should-not (shipit-magit-worktrees--number "/repo/.worktrees/spike"))
  (should-not (shipit-magit-worktrees--number "/home/user/work/acme-project")))

;;;; Querying

(ert-deftest test-shipit-magit-worktrees-query-is-batched ()
  "One document asks for every number, not one request per worktree."
  (let ((query (shipit-magit-worktrees--query "acme/acme-project" '(20357 19083))))
    (should (string-match-p "owner:\\\\?\"acme\\\\?\"" query))
    (should (string-match-p "name:\\\\?\"acme-project\\\\?\"" query))
    (should (string-match-p "p20357:pullRequest(number:20357)" query))
    (should (string-match-p "p19083:pullRequest(number:19083)" query))
    (should (= 1 (cl-count-if (lambda (line) (string-match-p "repository(" line))
                              (split-string query "{" t))))))

(defconst test-shipit-magit-worktrees--response
  '((data
     (repository
      (p20357 (number . 20357) (state . "MERGED") (isDraft . :false)
              (mergedAt . "2026-07-07T10:47:53Z") (title . "add constant"))
      (p15449 (number . 15449) (state . "CLOSED") (isDraft . :false)
              (mergedAt) (title . "lint for large files"))
      (p19999 (number . 19999) (state . "OPEN") (isDraft . t)
              (mergedAt) (title . "work in progress"))
      (p19998 (number . 19998) (state . "OPEN") (isDraft . :false)
              (mergedAt) (title . "ready")))))
  "A GraphQL answer shaped the way `shipit--graphql-request' hands one over.")

(ert-deftest test-shipit-magit-worktrees-parse-states ()
  "Every state a pull request can be in survives parsing."
  (let ((states (mapcar (lambda (info)
                          (cons (plist-get info :number) (plist-get info :state)))
                        (shipit-magit-worktrees--parse
                         test-shipit-magit-worktrees--response))))
    (should (eq (alist-get 20357 states) 'merged))
    (should (eq (alist-get 15449 states) 'closed))
    (should (eq (alist-get 19999 states) 'draft))
    (should (eq (alist-get 19998 states) 'open))))

(ert-deftest test-shipit-magit-worktrees-parse-keeps-merge-date ()
  "A merged pull request keeps the date the margin shows."
  (let ((info (seq-find (lambda (i) (= (plist-get i :number) 20357))
                        (shipit-magit-worktrees--parse
                         test-shipit-magit-worktrees--response))))
    (should (equal (plist-get info :merged-at) "2026-07-07T10:47:53Z"))))

(ert-deftest test-shipit-magit-worktrees-parse-survives-nothing ()
  "A failed request yields no states rather than an error."
  (should-not (shipit-magit-worktrees--parse nil))
  (should-not (shipit-magit-worktrees--parse '((errors . [((message . "nope"))])))))

;;;; Caching

(ert-deftest test-shipit-magit-worktrees-terminal-states-never-expire ()
  "Merged is forever, so the next query only asks about live ones."
  (let ((shipit-magit-worktrees-ttl 60)
        (old (- (float-time) 86400)))
    (should-not (shipit-magit-worktrees--stale-p
                 (list :state 'merged :fetched old)))
    (should-not (shipit-magit-worktrees--stale-p
                 (list :state 'closed :fetched old)))
    (should (shipit-magit-worktrees--stale-p (list :state 'open :fetched old)))
    (should (shipit-magit-worktrees--stale-p nil))))

;;;; Asking the forge

(defun test-shipit-magit-worktrees--answer (callback body &optional status)
  "Hand CALLBACK a response holding BODY, the way url.el would.
STATUS is url.el's word on how it went; a failure carries `:error'."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n" body)
    (funcall callback status)))

(ert-deftest test-shipit-magit-worktrees-a-missing-pull-request-is-asked-about-once ()
  ;; GIVEN a worktree named for a pull request the repository does not have
  ;; AND a forge that answers nothing for that number
  ;; WHEN the section asks, and the next refresh asks about what is stale
  ;; THEN the number is remembered as missing AND the forge is not asked again
  (let ((shipit-magit-worktrees--states (make-hash-table :test 'equal))
        (shipit-magit-worktrees--pending nil)
        (shipit-magit-worktrees-show-size nil)
        (requests 0)
        (worktrees '(("/nowhere/pr-19083-gone"))))
    (cl-letf (((symbol-function 'shipit--github-token) (lambda () "tok"))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (cl-incf requests)
                 (test-shipit-magit-worktrees--answer
                  callback
                  (concat "{\"data\":{\"repository\":{\"p19083\":null}},"
                          "\"errors\":[{\"message\":\"Could not resolve to a "
                          "PullRequest with the number of 19083.\"}]}")))))
      (with-temp-buffer
        (shipit-magit-worktrees--refresh-missing "acme/acme-project" worktrees)
        (shipit-magit-worktrees--refresh-missing "acme/acme-project" worktrees))
      (should (= 1 requests))
      (let ((info (gethash "acme/acme-project#19083" shipit-magit-worktrees--states)))
        (should (eq (plist-get info :state) 'missing))
        (should-not (shipit-magit-worktrees--stale-p info)))
      (should-not shipit-magit-worktrees--pending))))

(ert-deftest test-shipit-magit-worktrees-a-failed-request-leaves-the-buffer-alone ()
  ;; GIVEN a forge that fails to answer
  ;; WHEN the section asks
  ;; THEN nothing is remembered, nothing stays pending, AND the buffer is
  ;; not refreshed, since a refresh would only ask again
  (let ((shipit-magit-worktrees--states (make-hash-table :test 'equal))
        (shipit-magit-worktrees--pending nil)
        (refreshes 0))
    (cl-letf (((symbol-function 'shipit--github-token) (lambda () "tok"))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (test-shipit-magit-worktrees--answer
                  callback "" '(:error (error http 500)))))
              ((symbol-function 'magit-refresh-buffer)
               (lambda (&rest _) (cl-incf refreshes))))
      (with-temp-buffer
        (setq major-mode 'magit-status-mode)
        (shipit-magit-worktrees--fetch "acme/acme-project" '(19083) (current-buffer)))
      (should (= 0 refreshes))
      (should (= 0 (hash-table-count shipit-magit-worktrees--states)))
      (should-not shipit-magit-worktrees--pending))))

(ert-deftest test-shipit-magit-worktrees-missing-fills-what-the-answer-lacks ()
  ;; GIVEN two numbers asked about and one of them answered
  ;; THEN the other is a plist in state `missing', fetched now
  (let ((missing (shipit-magit-worktrees--missing
                  '(20357 19083) (list (list :number 20357 :state 'merged)))))
    (should (= 1 (length missing)))
    (should (= 19083 (plist-get (car missing) :number)))
    (should (eq 'missing (plist-get (car missing) :state)))
    (should (< (- (float-time) (plist-get (car missing) :fetched)) 5))))

(ert-deftest test-shipit-magit-worktrees-answered-only-by-a-repository ()
  ;; GIVEN answers with a repository, with a null repository, and none at all
  ;; THEN only the first two count as the forge having answered
  (should (shipit-magit-worktrees--answered-p test-shipit-magit-worktrees--response))
  (should (shipit-magit-worktrees--answered-p '((data (repository (p1))))))
  (should-not (shipit-magit-worktrees--answered-p '((data (repository)))))
  (should-not (shipit-magit-worktrees--answered-p nil)))

;;;; Rendering

(ert-deftest test-shipit-magit-worktrees-margin-shows-state-and-date ()
  "The margin carries the reference, the state and a short date."
  (let ((text (shipit-magit-worktrees--margin-text
               '(:number 20357 :state merged :merged-at "2026-07-07T10:47:53Z"
                         :width 0))))
    (should (string-match-p "#20357" text))
    (should (string-match-p "merged" text))
    (should (string-match-p "07-07" text))))

(ert-deftest test-shipit-magit-worktrees-size-follows-details ()
  "Size appears only with magit's margin details on, like a log author."
  (let ((info '(:number 20357 :state merged :size 550502400 :width 0)))
    (should-not (string-match-p "525M" (shipit-magit-worktrees--margin-text info)))
    (should (string-match-p "525M" (shipit-magit-worktrees--margin-text
                                    (append info '(:details t)))))))

(ert-deftest test-shipit-magit-worktrees-margin-fits-width ()
  "Text is truncated to the margin rather than clipped mid-glyph."
  (let ((text (shipit-magit-worktrees--margin-text
               '(:number 20357 :state merged :merged-at "2026-07-07T10:47:53Z"
                         :size 550502400 :details t :width 12))))
    (should (= (string-width text) 12))))

(ert-deftest test-shipit-magit-worktrees-widens-a-narrow-margin ()
  "Magit sizes the margin for a date alone, which the state does not fit.
Magit derives 11 columns from the `age' style, so the section widens the
buffer\='s margin rather than asking the user for an invalid
`magit-status-margin'."
  (with-temp-buffer
    (setq-local magit--right-margin-config (list t 'age 11 nil 18))
    (let ((shipit-magit-worktrees-margin-min-width 28))
      (shipit-magit-worktrees--widen-margin)
      (should (= (nth 2 magit--right-margin-config) 28))
      (should (= (car (shipit-magit-worktrees--margin-config)) 28)))))

(ert-deftest test-shipit-magit-worktrees-keeps-a-wider-margin ()
  "Turning magit\='s details on widens the margin past ours, so leave it.
`d' under `L' is what shows each worktree\='s size, and shrinking the
margin back would truncate the thing that toggle asked for."
  (with-temp-buffer
    (setq-local magit--right-margin-config (list t 'age 30 t 18))
    (let ((shipit-magit-worktrees-margin-min-width 28))
      (shipit-magit-worktrees--widen-margin)
      (should (= (nth 2 magit--right-margin-config) 30)))))

(ert-deftest test-shipit-magit-worktrees-margin-width-can-be-off ()
  "Nil leaves magit\='s own width alone."
  (with-temp-buffer
    (setq-local magit--right-margin-config (list t 'age 11 nil 18))
    (let ((shipit-magit-worktrees-margin-min-width nil))
      (shipit-magit-worktrees--widen-margin)
      (should (= (nth 2 magit--right-margin-config) 11)))))

(ert-deftest test-shipit-magit-worktrees-margin-width-outside-magit ()
  "No margin config yet means nothing to widen, not an error."
  (with-temp-buffer
    (setq-local magit--right-margin-config nil)
    (let ((shipit-magit-worktrees-margin-min-width 28))
      (should-not (shipit-magit-worktrees--widen-margin)))))

(ert-deftest test-shipit-magit-worktrees-margin-while-unknown ()
  "A worktree renders before its query lands."
  (should (string-match-p
           "…" (shipit-magit-worktrees--margin-text '(:number 21501 :width 0)))))

(ert-deftest test-shipit-magit-worktrees-row-carries-pr-number ()
  "Rows carry `shipit-pr-number', which shipit's commands read at point."
  (let* ((text (shipit-magit-worktrees--margin-text
                '(:number 21501 :state open :width 0)))
         (at (string-match "#" text)))
    (should (= (get-text-property at 'shipit-pr-number text) 21501))
    (should (keymapp (get-text-property at 'keymap text)))))

;;;; Actions

(ert-deftest test-shipit-magit-worktrees-pr-url ()
  "The URL actions copy and browse is built from repo and number."
  (should (equal (shipit-magit-worktrees--pr-url "acme/acme-project" 20357)
                 "https://github.com/acme/acme-project/pull/20357")))

(ert-deftest test-shipit-magit-worktrees-dwim-claims-worktree-rows ()
  "The dwim handler claims a worktree row with a pull request, nothing else."
  (with-temp-buffer
    (insert (propertize "row\n" 'shipit-pr-number 20357))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'magit-current-section) (lambda () 'section))
              ((symbol-function 'magit-section-match)
               (lambda (type &rest _) (memq 'worktree (if (listp type) type
                                                        (list type))))))
      (should (shipit-magit-worktrees--dwim-match-p)))
    ;; a row without a pull request, and a section of another type
    (cl-letf (((symbol-function 'magit-current-section) (lambda () 'section))
              ((symbol-function 'magit-section-match) (lambda (&rest _) nil)))
      (should-not (shipit-magit-worktrees--dwim-match-p)))))

(ert-deftest test-shipit-magit-worktrees-menu-is-on-the-dwim-key ()
  "Worktree rows answer to the key shipit uses for dwim elsewhere."
  (should (eq (keymap-lookup magit-worktree-section-map "M-;")
              'shipit-magit-worktrees-menu)))

;;;; Surviving a reload

(ert-deftest test-shipit-magit-worktrees-install-replaces-magits-section ()
  "Installing takes over magit's worktree section rather than doubling it."
  (let ((magit-status-sections-hook (list #'magit-insert-worktrees)))
    (shipit-magit-worktrees--install t)
    (should (memq #'shipit-magit-worktrees-insert-section
                  magit-status-sections-hook))
    (should-not (memq #'magit-insert-worktrees magit-status-sections-hook))
    (shipit-magit-worktrees--install nil)
    (should-not (memq #'shipit-magit-worktrees-insert-section
                      magit-status-sections-hook))
    ;; handed back, so worktrees still list the plain way
    (should (memq #'magit-insert-worktrees magit-status-sections-hook))))

(ert-deftest test-shipit-magit-worktrees-uninstall-adds-nothing-magit-lacked ()
  "Uninstalling restores what was there, and invents nothing.
`magit-insert-worktrees' is not in magit's default
`magit-status-sections-hook' -- it is opt-in -- so turning this section
off must not leave a worktree section behind for someone who never had
one."
  (let ((magit-status-sections-hook (list #'magit-insert-stashes)))
    (shipit-magit-worktrees--install t)
    (should (memq #'shipit-magit-worktrees-insert-section
                  magit-status-sections-hook))
    (shipit-magit-worktrees--install nil)
    (should-not (memq #'shipit-magit-worktrees-insert-section
                      magit-status-sections-hook))
    (should-not (memq #'magit-insert-worktrees magit-status-sections-hook))
    (should (equal magit-status-sections-hook (list #'magit-insert-stashes)))))

(ert-deftest test-shipit-magit-worktrees-uninstall-restores-the-old-order ()
  "Magit's section comes back where it was, not appended to the end.
Its position on the hook is where it renders in the status buffer, so
handing back at the tail would silently reorder someone's sections."
  (let ((magit-status-sections-hook (list #'magit-insert-worktrees
                                          #'magit-insert-stashes)))
    (shipit-magit-worktrees--install t)
    (shipit-magit-worktrees--install nil)
    (should (equal magit-status-sections-hook
                   (list #'magit-insert-worktrees
                         #'magit-insert-stashes)))))

(ert-deftest test-shipit-magit-worktrees-survives-a-reload ()
  "The reload strips shipit functions from the hook, so it must reload
this file to put the section back.  Without the file in its list the
worktree section vanishes on the first reload -- magit's own is gone
too, since this one replaced it."
  (let ((utils (expand-file-name
                "shipit-dev-utils.el"
                (file-name-directory
                 (directory-file-name
                  (file-name-directory (locate-library "shipit-magit-worktrees")))))))
    (should (file-exists-p utils))
    (with-temp-buffer
      (insert-file-contents utils)
      (goto-char (point-min))
      (should (search-forward "\"shipit-magit-worktrees.el\"" nil t)))))

(provide 'test-shipit-magit-worktrees)
;;; test-shipit-magit-worktrees.el ends here
