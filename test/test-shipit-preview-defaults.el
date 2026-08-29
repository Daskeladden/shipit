;;; test-shipit-preview-defaults.el --- Preview defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; What a preview starts with before anyone edits it: whether the pull
;; request is a draft, and what the description is pre-filled from.

;;; Code:

(require 'ert)
(require 'shipit-preview)
(require 'shipit-editor)

;;;; Draft default

(ert-deftest test-shipit-preview-draft-default-follows-the-stack ()
  "GIVEN the default left at `stacked'
WHEN a branch is in a stack, and when it is not
THEN only the stacked one starts as a draft.  A stack opens several
pull requests at once and the upper ones cannot merge until the
bottom lands, so announcing them all for review is noise; a lone
pull request is unaffected."
  (let ((shipit-preview-default-draft 'stacked))
    (should (shipit-preview--default-draft-p t))
    (should-not (shipit-preview--default-draft-p nil))))

(ert-deftest test-shipit-preview-draft-default-can-be-forced-either-way ()
  "GIVEN the default set to t or to nil
WHEN a draft decision is made
THEN the setting wins whether or not there is a stack, so anyone who
wants every pull request drafted, or none, still gets that."
  (let ((shipit-preview-default-draft t))
    (should (shipit-preview--default-draft-p nil))
    (should (shipit-preview--default-draft-p t)))
  (let ((shipit-preview-default-draft nil))
    (should-not (shipit-preview--default-draft-p nil))
    (should-not (shipit-preview--default-draft-p t))))

;;;; Pull request template

(defmacro test-preview--with-tree (files &rest body)
  "Create FILES, an alist of relative path to content, then run BODY.
`default-directory' is the root of the tree."
  (declare (indent 1))
  `(let ((dir (make-temp-file "shipit-template-test-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory dir)))
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) dir)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory dir t))))

(ert-deftest test-shipit-preview-finds-the-template-github-looks-for ()
  "GIVEN a template in any of the places GitHub reads one from
WHEN the template is looked up
THEN it is found in each, because a repository picks one of them and
a preview that missed it would drop the structure reviewers expect."
  (dolist (path '(".github/PULL_REQUEST_TEMPLATE.md"
                  ".github/pull_request_template.md"
                  "PULL_REQUEST_TEMPLATE.md"
                  "docs/PULL_REQUEST_TEMPLATE.md"))
    (test-preview--with-tree (list (cons path "## Why\n"))
      (should (equal "## Why\n" (shipit-preview--pr-template default-directory))))))

(ert-deftest test-shipit-preview-has-no-template-when-there-is-none ()
  "GIVEN a repository with no template
WHEN one is looked up
THEN nothing is returned, and nothing is pre-filled."
  (test-preview--with-tree '(("README.md" . "hi"))
    (should-not (shipit-preview--pr-template default-directory))))

(ert-deftest test-shipit-preview-template-directory-offers-every-choice ()
  "GIVEN the directory form, where a repository keeps several templates
WHEN they are looked up
THEN every one is offered rather than one picked silently, since only
the author knows which kind of change this is."
  (test-preview--with-tree '((".github/PULL_REQUEST_TEMPLATE/bug.md" . "bug")
                             (".github/PULL_REQUEST_TEMPLATE/feature.md" . "feature"))
    (should (equal '("bug.md" "feature.md")
                   (sort (mapcar #'file-name-nondirectory
                                 (shipit-preview--pr-template-files default-directory))
                         #'string<)))))

;;;; What the description starts as

(ert-deftest test-shipit-preview-template-wins-over-a-single-commit ()
  "GIVEN a template and a branch of one commit
WHEN the description is pre-filled
THEN the template is used.  It is the structure the repository asks
every pull request for, and the commit message is already visible in
the commits section."
  (should (equal "## Why\n"
                 (shipit-preview--maybe-auto-fill-description
                  nil '((:message "one commit")) "## Why\n"))))

(ert-deftest test-shipit-preview-single-commit-fills-in-with-no-template ()
  "GIVEN one commit and no template
WHEN the description is pre-filled
THEN the commit message is used, which is the behaviour that was
there before templates were read at all."
  (should (equal "one commit"
                 (shipit-preview--maybe-auto-fill-description
                  nil '((:message "one commit")) nil))))

(ert-deftest test-shipit-preview-never-overwrites-what-was-written ()
  "GIVEN a description that already has something in it
WHEN pre-filling runs again, as it does on every reopen
THEN it is left alone, template or not."
  (should (equal "mine"
                 (shipit-preview--maybe-auto-fill-description
                  "mine" '((:message "one commit")) "## Why\n"))))


(provide 'test-shipit-preview-defaults)
;;; test-shipit-preview-defaults.el ends here
