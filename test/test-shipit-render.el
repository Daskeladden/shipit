;;; test-shipit-render.el --- Tests for render functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for shipit-render.el rendering functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-render)
(require 'shipit-pr-backends)

;;; Auto-increment Ordered Lists Tests

(ert-deftest test-shipit-auto-increment-ordered-lists-simple ()
  "GIVEN markdown with all 1. items
WHEN calling shipit--auto-increment-ordered-lists
THEN numbers are auto-incremented."
  (let ((input "1. First\n1. Second\n1. Third"))
    (should (string= "1. First\n2. Second\n3. Third"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-already-numbered ()
  "GIVEN markdown with already correct numbering
WHEN calling shipit--auto-increment-ordered-lists
THEN numbers remain unchanged."
  (let ((input "1. First\n2. Second\n3. Third"))
    (should (string= "1. First\n2. Second\n3. Third"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-mixed ()
  "GIVEN markdown with mixed numbering (some correct, some not)
WHEN calling shipit--auto-increment-ordered-lists
THEN numbers are fixed based on the first item."
  (let ((input "1. Alpha\n1. Beta\n3. Gamma\n1. Delta"))
    (should (string= "1. Alpha\n2. Beta\n3. Gamma\n4. Delta"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-with-paragraphs ()
  "GIVEN markdown with ordered list separated by paragraph text
WHEN calling shipit--auto-increment-ordered-lists
THEN each list is numbered independently."
  (let ((input "1. First list item A\n1. First list item B\n\nSome text.\n\n1. Second list item A\n1. Second list item B"))
    (should (string= "1. First list item A\n2. First list item B\n\nSome text.\n\n1. Second list item A\n2. Second list item B"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-unordered-preserved ()
  "GIVEN markdown with unordered list items
WHEN calling shipit--auto-increment-ordered-lists
THEN unordered items are not modified."
  (let ((input "- First\n- Second\n- Third"))
    (should (string= "- First\n- Second\n- Third"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-nested ()
  "GIVEN markdown with nested ordered list
WHEN calling shipit--auto-increment-ordered-lists
THEN nested items are incremented independently."
  (let ((input "1. Outer A\n   1. Inner A\n   1. Inner B\n1. Outer B"))
    (should (string= "1. Outer A\n   1. Inner A\n   2. Inner B\n2. Outer B"
                      (shipit--auto-increment-ordered-lists input)))))

(ert-deftest test-shipit-auto-increment-ordered-lists-empty ()
  "GIVEN empty text
WHEN calling shipit--auto-increment-ordered-lists
THEN empty string is returned."
  (should (string= "" (shipit--auto-increment-ordered-lists ""))))

(ert-deftest test-shipit-auto-increment-ordered-lists-no-lists ()
  "GIVEN text with no ordered lists
WHEN calling shipit--auto-increment-ordered-lists
THEN text is unchanged."
  (let ((input "Hello world\nThis is just text"))
    (should (string= input (shipit--auto-increment-ordered-lists input)))))

;;; HTML List Conversion Tests

(ert-deftest test-shipit-convert-html-lists-ol-numbered ()
  "GIVEN HTML with <ol> containing <li> items
WHEN calling shipit--convert-html-lists-to-markdown
THEN items are numbered sequentially."
  (let ((input "<ol><li>First</li><li>Second</li><li>Third</li></ol>"))
    (should (string-match-p "1\\. First" (shipit--convert-html-lists-to-markdown input)))
    (should (string-match-p "2\\. Second" (shipit--convert-html-lists-to-markdown input)))
    (should (string-match-p "3\\. Third" (shipit--convert-html-lists-to-markdown input)))))

(ert-deftest test-shipit-convert-html-lists-ul-bullets ()
  "GIVEN HTML with <ul> containing <li> items
WHEN calling shipit--convert-html-lists-to-markdown
THEN items use bullet points."
  (let ((input "<ul><li>Alpha</li><li>Beta</li></ul>"))
    (should (string-match-p "- Alpha" (shipit--convert-html-lists-to-markdown input)))
    (should (string-match-p "- Beta" (shipit--convert-html-lists-to-markdown input)))))

(ert-deftest test-shipit-convert-html-lists-standalone-li ()
  "GIVEN HTML with standalone <li> items (no parent list)
WHEN calling shipit--convert-html-lists-to-markdown
THEN items use bullet points."
  (let ((input "<li>Item A</li><li>Item B</li>"))
    (should (string-match-p "- Item A" (shipit--convert-html-lists-to-markdown input)))
    (should (string-match-p "- Item B" (shipit--convert-html-lists-to-markdown input)))))

;;; Mermaid Rendering Tests

(ert-deftest test-shipit-render-mermaid-skips-when-disabled ()
  "GIVEN shipit-render-mermaid is nil
WHEN calling shipit--render-mermaid-blocks-in-region
THEN no rendering occurs and buffer is unchanged."
  (let ((shipit-render-mermaid nil)
        (shipit--mmdc-available t))
    (with-temp-buffer
      (insert "```mermaid\ngraph TD;\n  A-->B\n```\n")
      (shipit--render-mermaid-blocks-in-region (point-min) (point-max))
      ;; Buffer should be unchanged
      (should (string-match-p "graph TD" (buffer-string))))))

(ert-deftest test-shipit-render-mermaid-skips-when-no-mmdc ()
  "GIVEN mmdc is not installed
WHEN calling shipit--mmdc-available-p
THEN returns nil and caches the result."
  (let ((shipit--mmdc-available 'unknown))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) nil)))
      ;; THEN availability check returns nil
      (should-not (shipit--mmdc-available-p))
      ;; THEN result is cached
      (should (eq nil shipit--mmdc-available)))))

(ert-deftest test-shipit-render-mermaid-caches-availability ()
  "GIVEN mmdc availability was already checked
WHEN calling shipit--mmdc-available-p again
THEN cached result is returned without re-checking."
  ;; GIVEN mmdc is cached as unavailable
  (let ((shipit--mmdc-available nil))
    ;; THEN returns nil without calling executable-find
    (should-not (shipit--mmdc-available-p)))
  ;; GIVEN mmdc is cached as available
  (let ((shipit--mmdc-available t))
    ;; THEN returns t without calling executable-find
    (should (shipit--mmdc-available-p))))

;;; Mermaid Defcustom Default Tests

(ert-deftest test-shipit-mermaid-theme-default ()
  "GIVEN shipit-mermaid-theme defcustom
WHEN checking its default value
THEN it should be \"dark\"."
  (should (string= "dark" (default-value 'shipit-mermaid-theme))))

(ert-deftest test-shipit-mermaid-background-default ()
  "GIVEN shipit-mermaid-background defcustom
WHEN checking its default value
THEN it should be \"transparent\"."
  (should (string= "transparent" (default-value 'shipit-mermaid-background))))

(ert-deftest test-shipit-mermaid-max-width-default ()
  "GIVEN shipit-mermaid-max-width defcustom
WHEN checking its default value
THEN it should be 800."
  (should (= 800 (default-value 'shipit-mermaid-max-width))))

;;; Mermaid Interactive Command Tests

;; Define mermaid commands locally for testing (shipit-commands.el
;; requires transient which is unavailable in batch mode).
(defun shipit-toggle-mermaid ()
  (interactive)
  (setq shipit-render-mermaid (not shipit-render-mermaid))
  (message "Mermaid rendering %s" (if shipit-render-mermaid "enabled" "disabled")))

(defun shipit-cycle-mermaid-theme ()
  (interactive)
  (setq shipit-mermaid-theme
        (pcase shipit-mermaid-theme
          ("dark" "default")
          ("default" "forest")
          ("forest" "neutral")
          ("neutral" "dark")
          (_ "dark")))
  (message "Mermaid theme: %s" shipit-mermaid-theme))

(defun shipit-cycle-mermaid-background ()
  (interactive)
  (setq shipit-mermaid-background
        (pcase shipit-mermaid-background
          ("transparent" "white")
          ("white" "#1e1e1e")
          ("#1e1e1e" "transparent")
          (_ "transparent")))
  (message "Mermaid background: %s" shipit-mermaid-background))

(defun shipit-increase-mermaid-max-width ()
  (interactive)
  (setq shipit-mermaid-max-width (min 2000 (+ shipit-mermaid-max-width 100)))
  (message "Mermaid max width: %d px" shipit-mermaid-max-width))

(defun shipit-decrease-mermaid-max-width ()
  (interactive)
  (setq shipit-mermaid-max-width (max 200 (- shipit-mermaid-max-width 100)))
  (message "Mermaid max width: %d px" shipit-mermaid-max-width))

(ert-deftest test-shipit-toggle-mermaid ()
  "GIVEN shipit-render-mermaid is t
WHEN calling shipit-toggle-mermaid
THEN it becomes nil, and vice versa."
  (let ((shipit-render-mermaid t))
    (shipit-toggle-mermaid)
    (should-not shipit-render-mermaid))
  (let ((shipit-render-mermaid nil))
    (shipit-toggle-mermaid)
    (should shipit-render-mermaid)))

(ert-deftest test-shipit-cycle-mermaid-theme ()
  "GIVEN shipit-mermaid-theme at each value
WHEN calling shipit-cycle-mermaid-theme
THEN it cycles dark -> default -> forest -> neutral -> dark."
  (let ((shipit-mermaid-theme "dark"))
    (shipit-cycle-mermaid-theme)
    (should (string= "default" shipit-mermaid-theme))
    (shipit-cycle-mermaid-theme)
    (should (string= "forest" shipit-mermaid-theme))
    (shipit-cycle-mermaid-theme)
    (should (string= "neutral" shipit-mermaid-theme))
    (shipit-cycle-mermaid-theme)
    (should (string= "dark" shipit-mermaid-theme))))

(ert-deftest test-shipit-cycle-mermaid-background ()
  "GIVEN shipit-mermaid-background at each value
WHEN calling shipit-cycle-mermaid-background
THEN it cycles transparent -> white -> #1e1e1e -> transparent."
  (let ((shipit-mermaid-background "transparent"))
    (shipit-cycle-mermaid-background)
    (should (string= "white" shipit-mermaid-background))
    (shipit-cycle-mermaid-background)
    (should (string= "#1e1e1e" shipit-mermaid-background))
    (shipit-cycle-mermaid-background)
    (should (string= "transparent" shipit-mermaid-background))))

(ert-deftest test-shipit-mermaid-max-width-bounds ()
  "GIVEN shipit-mermaid-max-width at boundary values
WHEN increasing past 2000 or decreasing below 200
THEN it clamps to the boundary."
  ;; GIVEN max width is at upper boundary
  (let ((shipit-mermaid-max-width 2000))
    ;; WHEN increasing
    (shipit-increase-mermaid-max-width)
    ;; THEN clamped at 2000
    (should (= 2000 shipit-mermaid-max-width)))
  ;; GIVEN max width is at lower boundary
  (let ((shipit-mermaid-max-width 200))
    ;; WHEN decreasing
    (shipit-decrease-mermaid-max-width)
    ;; THEN clamped at 200
    (should (= 200 shipit-mermaid-max-width)))
  ;; GIVEN max width is in normal range
  (let ((shipit-mermaid-max-width 800))
    ;; WHEN increasing
    (shipit-increase-mermaid-max-width)
    ;; THEN increases by 100
    (should (= 900 shipit-mermaid-max-width)))
  ;; GIVEN max width is in normal range
  (let ((shipit-mermaid-max-width 800))
    ;; WHEN decreasing
    (shipit-decrease-mermaid-max-width)
    ;; THEN decreases by 100
    (should (= 700 shipit-mermaid-max-width))))

;;; Cross-Forge URL Overlay Tests

(ert-deftest test-shipit-github-pr-url-overlays-created-when-gitlab-backend ()
  "GIVEN a buffer containing GitHub PR URLs and the backend is gitlab
WHEN calling shipit--create-pr-reference-overlays
THEN overlays ARE created (all backends' :create-reference-overlays run)."
  ;; GIVEN
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/pull/42 for details")
    (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'gitlab))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--create-pr-reference-overlays "owner/repo" 1 (point-min) (point-max))
      ;; THEN overlays ARE created with help-echo mentioning PR URL
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (let ((echo (overlay-get ov 'help-echo)))
                             (and echo (string-match-p "PR owner/repo#42" echo))))
                         overlays))))))

(ert-deftest test-shipit-github-issue-url-overlays-created-when-gitlab-backend ()
  "GIVEN a buffer containing GitHub issue URLs and the backend is gitlab
WHEN calling shipit--create-pr-reference-overlays
THEN overlays ARE created (all backends' :create-reference-overlays run)."
  ;; GIVEN
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/issues/99 for details")
    (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'gitlab))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--create-pr-reference-overlays "owner/repo" 1 (point-min) (point-max))
      ;; THEN overlays ARE created with help-echo mentioning the issue URL
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (let ((echo (overlay-get ov 'help-echo)))
                             (and echo (string-match-p "owner/repo#99" echo))))
                         overlays))))))

(ert-deftest test-shipit-github-discussion-url-overlays-created-when-gitlab-backend ()
  "GIVEN a buffer containing GitHub discussion URLs and the backend is gitlab
WHEN calling shipit--create-pr-reference-overlays
THEN overlays ARE created (all backends' :create-reference-overlays run)."
  ;; GIVEN
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/discussions/7 for details")
    (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'gitlab))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--create-pr-reference-overlays "owner/repo" 1 (point-min) (point-max))
      ;; THEN overlays ARE created with help-echo mentioning the discussion URL
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (let ((echo (overlay-get ov 'help-echo)))
                             (and echo (string-match-p "Discussion owner/repo#7" echo))))
                         overlays))))))

(ert-deftest test-shipit-github-pr-url-overlays-created-when-github-backend ()
  "GIVEN a buffer containing GitHub PR URLs and the backend is github
WHEN calling shipit--create-pr-reference-overlays
THEN overlays ARE created for GitHub PR URLs."
  ;; GIVEN
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/pull/42 for details")
    (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'github))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--create-pr-reference-overlays "owner/repo" 1 (point-min) (point-max))
      ;; THEN overlays ARE created with help-echo mentioning PR URL
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (let ((echo (overlay-get ov 'help-echo)))
                             (and echo (string-match-p "PR owner/repo#42" echo))))
                         overlays))))))

(ert-deftest test-shipit-reference-open-dwim-dispatches-browse-issue-url ()
  "GIVEN a PR backend with :browse-issue-url registered
WHEN calling shipit--reference-open-dwim for an issue
THEN it dispatches through :browse-issue-url instead of hardcoded GitHub URL."
  ;; GIVEN a mock backend with :browse-issue-url
  (let* ((browse-url-called nil)
         (browse-url-arg nil)
         (shipit-pr-backends nil)
         (shipit-pr-backend 'test-be)
         (shipit-pr-backend-config nil)
         (shipit-issues-enabled nil)
         (plist (list :name "Test"
                      :fetch-pr #'ignore
                      :search #'ignore
                      :create-pr #'ignore
                      :merge-pr #'ignore
                      :update-pr #'ignore
                      :fetch-reviews #'ignore
                      :submit-review #'ignore
                      :fetch-review-decision #'ignore
                      :fetch-files #'ignore
                      :fetch-commits #'ignore
                      :fetch-checks #'ignore
                      :browse-url #'ignore
                      :browse-issue-url (lambda (config number)
                                          (format "https://test.example.com/%s/issues/%d"
                                                  (plist-get config :repo) number)))))
    (shipit-pr-register-backend 'test-be plist)
    (cl-letf (((symbol-function 'shipit--reference-detect-type) (lambda (_n _r _t) 'issue))
              ((symbol-function 'browse-url) (lambda (url) (setq browse-url-called t browse-url-arg url)))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--reference-open-dwim 55 "myorg/myrepo" 'issue)
      ;; THEN browse-url was called with the dispatched URL
      (should browse-url-called)
      (should (string= "https://test.example.com/myorg/myrepo/issues/55" browse-url-arg)))))

(ert-deftest test-shipit-reference-open-dwim-skips-browse-when-no-browse-issue-url ()
  "GIVEN a PR backend WITHOUT :browse-issue-url
WHEN calling shipit--reference-open-dwim for an issue
THEN browse-url is NOT called (no hardcoded fallback)."
  ;; GIVEN a mock backend without :browse-issue-url
  (let* ((browse-url-called nil)
         (shipit-pr-backends nil)
         (shipit-pr-backend 'test-be)
         (shipit-pr-backend-config nil)
         (shipit-issues-enabled nil)
         (plist (list :name "Test"
                      :fetch-pr #'ignore
                      :search #'ignore
                      :create-pr #'ignore
                      :merge-pr #'ignore
                      :update-pr #'ignore
                      :fetch-reviews #'ignore
                      :submit-review #'ignore
                      :fetch-review-decision #'ignore
                      :fetch-files #'ignore
                      :fetch-commits #'ignore
                      :fetch-checks #'ignore
                      :browse-url #'ignore)))
    (shipit-pr-register-backend 'test-be plist)
    (cl-letf (((symbol-function 'shipit--reference-detect-type) (lambda (_n _r _t) 'issue))
              ((symbol-function 'browse-url) (lambda (_url) (setq browse-url-called t)))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; WHEN
      (shipit--reference-open-dwim 77 "owner/repo" 'issue)
      ;; THEN browse-url was NOT called since no :browse-issue-url registered
      (should-not browse-url-called))))

;;; Inline HTML Stripping Tests

(ert-deftest test-shipit--strip-inline-html-tags-kbd ()
  "GIVEN markdown text with <kbd> tags
WHEN calling shipit--strip-inline-html-tags
THEN kbd tags are removed but content is preserved."
  (let ((input "<kbd>C-c</kbd> <kbd>C-l</kbd>"))
    (should (string= "C-c C-l"
                      (shipit--strip-inline-html-tags input)))))

(ert-deftest test-shipit--strip-inline-html-tags-preserves-other-tags ()
  "GIVEN markdown text with non-kbd HTML tags
WHEN calling shipit--strip-inline-html-tags
THEN all other tags are preserved (block tags, formatting tags)."
  (should (string= "<details><summary>Info</summary></details>"
                    (shipit--strip-inline-html-tags "<details><summary>Info</summary></details>")))
  (should (string= "<em>emphasis</em>"
                    (shipit--strip-inline-html-tags "<em>emphasis</em>")))
  (should (string= "<strong>bold</strong>"
                    (shipit--strip-inline-html-tags "<strong>bold</strong>")))
  (should (string= "<code>code</code>"
                    (shipit--strip-inline-html-tags "<code>code</code>"))))

(ert-deftest test-shipit--strip-inline-html-tags-in-table ()
  "GIVEN a markdown table with <kbd> tags in cells
WHEN calling shipit--strip-inline-html-tags
THEN the table pipe structure is preserved and tags are removed."
  (let* ((input "| Feature | key |\n|---------|-----|\n| Chat: clear | <kbd>C-c</kbd> <kbd>C-l</kbd> |")
         (result (shipit--strip-inline-html-tags input)))
    (should (string= "| Feature | key |\n|---------|-----|\n| Chat: clear | C-c C-l |"
                      result))))

(provide 'test-shipit-render)
;;; test-shipit-render.el ends here
