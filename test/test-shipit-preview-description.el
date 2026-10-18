;;; test-shipit-preview-description.el --- The description of a preview -*- lexical-binding: t; -*-

;;; Commentary:
;; How the preview of a new pull request draws its description.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-render)
(require 'shipit-preview)

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
      (setq-local shipit-preview--description "Intro\n\n```elisp\n(+ 1 2)\n```\n")
      (magit-insert-section (root)
        (shipit-preview--insert-description-section))
      (should (cl-some (lambda (ov) (overlay-get ov 'shipit-code-block))
                       (overlays-in (point-min) (point-max)))))))

(provide 'test-shipit-preview-description)
;;; test-shipit-preview-description.el ends here
