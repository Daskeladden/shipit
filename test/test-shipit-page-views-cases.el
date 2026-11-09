;;; test-shipit-page-views-cases.el --- More cases of the views of a page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The cases of the views of a page that the scenarios of
;; test-shipit-page-views.el leave out: a page still hides the markup of
;; the text that it renders.
;;
;; markdown-mode marks the markup of a text, such as the stars around
;; bold text, with an invisible value of its own.  The suite runs
;; without markdown-mode, so the tests put a text with that value into
;; the page.
;;
;; They use the pages and the stand-ins of test-shipit-page-views.el.

;;; Code:

(require 'ert)
(require 'test-shipit-page-views)

(defun test-page-views-cases--insert-rendered-text ()
  "Put the text \"**cups**\" at the end of this page, as markdown-mode renders it."
  (let ((inhibit-read-only t)
        (stars (propertize "**" 'invisible 'markdown-markup)))
    (save-excursion
      (goto-char (point-max))
      (insert stars "cups" stars "\n"))))

(defun test-page-views-cases--last-line-shown ()
  "The last line of this page that has text, as the user sees it."
  (car (last (split-string (test-page-views--shown) "\n" t))))

(ert-deftest test-page-views-cases-an-issue-page-hides-markup ()
  "The page of an issue hides the markup of rendered text."
  ;; GIVEN the page of an issue
  ;; WHEN bold text is in it as markdown-mode renders it
  ;; THEN the page shows the text without the stars around it
  (test-page-views--as-me
    (shipit-test-http-answering (append test-page-views--issue-answers
                                        test-page-views--repository-answers)
      (let ((page (test-page-views--open-issue)))
        (unwind-protect
            (with-current-buffer page
              (test-page-views-cases--insert-rendered-text)
              (should (equal "cups" (test-page-views-cases--last-line-shown))))
          (kill-buffer page))))))

(ert-deftest test-page-views-cases-a-pull-request-page-hides-markup ()
  "The page of a pull request hides the markup of rendered text."
  ;; GIVEN the page of a pull request
  ;; WHEN bold text is in it as markdown-mode renders it
  ;; THEN the page shows the text without the stars around it
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 7)
      (test-page-views-cases--insert-rendered-text)
      (should (equal "cups" (test-page-views-cases--last-line-shown))))))

(provide 'test-shipit-page-views-cases)
;;; test-shipit-page-views-cases.el ends here
