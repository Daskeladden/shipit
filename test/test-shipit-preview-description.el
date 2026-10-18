;;; test-shipit-preview-description.el --- The description of a preview -*- lexical-binding: t; -*-

;;; Commentary:
;; How the preview of a new pull request draws its description.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-render)
(require 'shipit-preview)
(require 'shipit-pr-github)

(ert-deftest test-shipit-preview-description-gets-code-block-backgrounds ()
  "GIVEN a draft description with a fenced code block
WHEN the preview inserts its description section
THEN the code block has the overlay of a code block.
The background is given, because a batch frame gives the code face none,
and the pass makes no overlay without one."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (shipit-render-markdown t)
          (shipit-code-block-background "#303030"))
      (setq-local shipit-preview--repo "o/r")
      (setq-local shipit-preview--description "Intro\n\n```elisp\n(+ 1 2)\n```\n")
      (magit-insert-section (root)
        (shipit-preview--insert-description-section))
      (should (cl-some (lambda (ov) (overlay-get ov 'shipit-code-block))
                       (overlays-in (point-min) (point-max)))))))

(ert-deftest test-shipit-preview-description-wraps-a-long-line ()
  "GIVEN a draft description with one paragraph of 400 characters on a line
WHEN the preview inserts its description section
THEN no line of the section is wider than 80 columns and its indent,
AND every word of the paragraph is still there."
  (let ((words (mapcar (lambda (i) (format "word%d" i)) (number-sequence 1 60))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t)
            (shipit-render-markdown t))
        (setq-local shipit-preview--description (concat (string-join words " ") "\n"))
        (magit-insert-section (root)
          (shipit-preview--insert-description-section))
        (let ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
          (should (<= (apply #'max (mapcar #'string-width lines)) 83))
          (should (equal words (split-string (string-join (cdr lines) " ")))))))))

(defmacro test-shipit-preview-description--with-section (description &rest body)
  "Run BODY in a buffer with the description section of a preview of
DESCRIPTION.  `shipit-preview--edit-description' and `browse-url' only
note their calls, in `edits' and `opened'."
  (declare (indent 1))
  `(with-temp-buffer
     (magit-section-mode)
     (let ((inhibit-read-only t)
           (shipit-render-markdown t)
           (edits 0)
           (opened nil))
       (setq-local shipit-preview--description ,description)
       (magit-insert-section (root)
         (shipit-preview--insert-description-section))
       (cl-letf (((symbol-function 'shipit-preview--edit-description)
                  (lambda () (cl-incf edits)))
                 ((symbol-function 'browse-url)
                  (lambda (url &rest _) (push url opened))))
         ,@body))))

(ert-deftest test-shipit-preview-ret-on-a-description-line-edits ()
  "GIVEN a preview description, and point on a line of its text with no link
WHEN RET runs the dwim of the preview
THEN the description editor opens, AND the section stays open."
  (test-shipit-preview-description--with-section "Some text of the change.\n"
    (goto-char (point-min))
    (search-forward "text of")
    (shipit-preview--dwim)
    (should (= 1 edits))
    (should-not (oref (magit-current-section) hidden))))

(ert-deftest test-shipit-preview-ret-on-a-description-link-offers-the-url ()
  "GIVEN a preview description with a markdown link, and point on the link
WHEN RET runs the dwim of the preview, and Open URL is chosen
THEN the menu offers Open URL and Edit description, AND the link opens."
  (test-shipit-preview-description--with-section
      "See [the guide](https://example.com/guide) first.\n"
    (goto-char (point-min))
    (search-forward "the gu")
    (let ((offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (setq offered choices) "Open URL")))
        (shipit-preview--dwim))
      (should (equal '("Open URL" "Edit description") offered))
      (should (equal '("https://example.com/guide") opened))
      (should (= 0 edits)))))

(ert-deftest test-shipit-preview-ret-on-the-description-heading-edits ()
  "GIVEN a preview description, and point on the heading of its section
WHEN RET runs the dwim of the preview
THEN the description editor opens, as before."
  (test-shipit-preview-description--with-section "Some text of the change.\n"
    (goto-char (point-min))
    (search-forward "Description")
    (shipit-preview--dwim)
    (should (= 1 edits))))

(ert-deftest test-shipit-preview-description-gets-link-overlays ()
  "GIVEN a draft description of o/r with the reference #7 and the URL of
pull request 12
WHEN the preview inserts its description section
THEN the reference and the URL have the link overlays of a PR buffer."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (shipit-render-markdown t))
      (setq-local shipit-preview--repo "o/r")
      (setq-local shipit-preview--description "See #7 and https://github.com/o/r/pull/12 now.\n")
      (magit-insert-section (root)
        (shipit-preview--insert-description-section))
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov) (eql 7 (overlay-get ov 'shipit-issue-id))) overlays))
        (should (cl-some (lambda (ov) (equal '(12 . "o/r") (overlay-get ov 'shipit-pr-link))) overlays))))))

(provide 'test-shipit-preview-description)
;;; test-shipit-preview-description.el ends here
