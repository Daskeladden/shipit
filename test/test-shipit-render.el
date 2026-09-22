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

;;; Pandoc table alignment guard tests

(ert-deftest test-shipit--align-markdown-tables-skips-when-no-table ()
  "GIVEN markdown text with no table separator line
WHEN calling shipit--align-markdown-tables-with-pandoc
THEN pandoc is not invoked and the text is returned unchanged."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/pandoc"))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq called t) 0)))
      (shipit--align-markdown-tables-with-pandoc "Just regular text")
      (should-not called))))

(ert-deftest test-shipit--align-markdown-tables-skips-text-with-stray-pipes ()
  "GIVEN markdown text containing pipes but no table separator
WHEN calling shipit--align-markdown-tables-with-pandoc
THEN pandoc is not invoked."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/pandoc"))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq called t) 0)))
      (shipit--align-markdown-tables-with-pandoc
       "This is `text | with | pipes` in inline code")
      (should-not called))))

(ert-deftest test-shipit--align-markdown-tables-skips-cells-without-separator ()
  "GIVEN text resembling a table row but lacking a separator line
WHEN calling shipit--align-markdown-tables-with-pandoc
THEN pandoc is not invoked."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/pandoc"))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq called t) 0)))
      (shipit--align-markdown-tables-with-pandoc
       "| col1 | col2 |\n| a | b |")
      (should-not called))))

(ert-deftest test-shipit--align-markdown-tables-runs-with-real-table ()
  "GIVEN markdown text containing a real table separator
WHEN calling shipit--align-markdown-tables-with-pandoc
THEN pandoc IS invoked."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/pandoc"))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq called t) 0)))
      (shipit--align-markdown-tables-with-pandoc
       "| col1 | col2 |\n|------|------|\n| a    | b    |")
      (should called))))

(ert-deftest test-shipit--align-markdown-tables-runs-with-aligned-separator ()
  "GIVEN markdown text containing an alignment separator like |:---:|
WHEN calling shipit--align-markdown-tables-with-pandoc
THEN pandoc IS invoked."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/pandoc"))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq called t) 0)))
      (shipit--align-markdown-tables-with-pandoc
       "| col1 | col2 |\n|:----:|----:|\n| a    | b    |")
      (should called))))

;;; Markdown render cache tests

(ert-deftest test-shipit--render-markdown-cache-hit ()
  "GIVEN the markdown cache is empty
WHEN rendering the same text twice
THEN the second call returns the cached value and only one entry exists."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal)))
    (let ((first (shipit--render-markdown "Hello **world**"))
          (second (shipit--render-markdown "Hello **world**")))
      (should (equal first second))
      (should (= 1 (hash-table-count shipit--markdown-render-cache))))))

(ert-deftest test-shipit--render-markdown-cache-different-text ()
  "GIVEN two different markdown texts
WHEN rendering each
THEN the cache holds two distinct entries."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal)))
    (shipit--render-markdown "Text one")
    (shipit--render-markdown "Text two")
    (should (= 2 (hash-table-count shipit--markdown-render-cache)))))

(ert-deftest test-shipit--render-markdown-cache-skips-rerendering ()
  "GIVEN the cache holds a rendered entry
WHEN rendering the same text again
THEN the underlying markdown processing is not re-executed."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal))
        (call-count 0))
    (shipit--render-markdown "Cached content")
    (cl-letf (((symbol-function 'shipit--strip-inline-html-tags)
               (lambda (text) (cl-incf call-count) text)))
      (shipit--render-markdown "Cached content")
      (should (= 0 call-count)))))

(ert-deftest test-shipit--render-markdown-cache-clear ()
  "GIVEN the cache has entries
WHEN shipit--markdown-cache-clear is called
THEN the cache is empty."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal)))
    (shipit--render-markdown "Some text")
    (should (> (hash-table-count shipit--markdown-render-cache) 0))
    (shipit--markdown-cache-clear)
    (should (= 0 (hash-table-count shipit--markdown-render-cache)))))

(ert-deftest test-shipit--render-markdown-cache-evicts-when-full ()
  "GIVEN the cache is at the size limit
WHEN rendering a new text
THEN the cache is cleared before storing the new entry."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal))
        (shipit-markdown-render-cache-size 3))
    (shipit--render-markdown "one")
    (shipit--render-markdown "two")
    (shipit--render-markdown "three")
    (should (= 3 (hash-table-count shipit--markdown-render-cache)))
    (shipit--render-markdown "four")
    ;; Cache was cleared and the new entry stored, so count is 1
    (should (= 1 (hash-table-count shipit--markdown-render-cache)))))

;;; Shebang Language Detection Tests

(ert-deftest test-shipit-detect-language-from-shebang-python ()
  "GIVEN a code block starting with #!/usr/bin/env python3
WHEN detecting language from shebang
THEN returns python."
  (with-temp-buffer
    (insert "#!/usr/bin/env python3\nimport os\nprint('hello')\n")
    (should (string= "python"
                      (shipit--detect-language-from-shebang 1 (point-max))))))

(ert-deftest test-shipit-detect-language-from-shebang-bash ()
  "GIVEN a code block starting with #!/bin/bash
WHEN detecting language from shebang
THEN returns bash."
  (with-temp-buffer
    (insert "#!/bin/bash\necho hello\n")
    (should (string= "bash"
                      (shipit--detect-language-from-shebang 1 (point-max))))))

(ert-deftest test-shipit-detect-language-from-shebang-node ()
  "GIVEN a code block starting with #!/usr/bin/env node
WHEN detecting language from shebang
THEN returns javascript."
  (with-temp-buffer
    (insert "#!/usr/bin/env node\nconsole.log('hi')\n")
    (should (string= "javascript"
                      (shipit--detect-language-from-shebang 1 (point-max))))))

(ert-deftest test-shipit-detect-language-from-shebang-none ()
  "GIVEN a code block with no shebang
WHEN detecting language from shebang
THEN returns nil."
  (with-temp-buffer
    (insert "import os\nprint('hello')\n")
    (should-not (shipit--detect-language-from-shebang 1 (point-max)))))

(ert-deftest test-shipit-detect-language-from-shebang-indented ()
  "GIVEN a code block with indented shebang line
WHEN detecting language from shebang
THEN still detects the language."
  (with-temp-buffer
    (insert "   #!/usr/bin/env ruby\nputs 'hi'\n")
    (should (string= "ruby"
                      (shipit--detect-language-from-shebang 1 (point-max))))))

(ert-deftest test-shipit-detect-language-from-shebang-empty-region ()
  "GIVEN an empty region
WHEN detecting language from shebang
THEN returns nil."
  (with-temp-buffer
    (should-not (shipit--detect-language-from-shebang 1 1))))

(ert-deftest test-shipit-detect-language-from-shebang-unknown-interpreter ()
  "GIVEN a code block with shebang for an unknown interpreter
WHEN detecting language from shebang
THEN returns nil."
  (with-temp-buffer
    (insert "#!/usr/bin/env tclsh\nset x 1\n")
    (should-not (shipit--detect-language-from-shebang 1 (point-max)))))

(ert-deftest test-shipit-detect-language-from-shebang-disabled ()
  "GIVEN shipit-code-block-detect-shebang is nil
WHEN applying code block backgrounds to a block with a shebang
THEN no syntax highlighting is applied."
  (with-temp-buffer
    (let ((shipit-code-block-detect-shebang nil)
          (shipit-code-block-background nil))
      (insert "```\n#!/usr/bin/env python3\nimport os\n```\n")
      (shipit--apply-code-block-backgrounds-in-region 1 (point-max))
      ;; Code text should have no face properties since highlighting was skipped
      (should-not (get-text-property 5 'face)))))

;;; Org rendering tests

(ert-deftest test-shipit-detect-body-format-empty ()
  "GIVEN empty or nil text
WHEN detecting body format
THEN markdown is returned (the default)."
  (should (eq 'markdown (shipit--detect-body-format nil)))
  (should (eq 'markdown (shipit--detect-body-format ""))))

(ert-deftest test-shipit-detect-body-format-plain-markdown ()
  "GIVEN plain markdown text without org markers
WHEN detecting body format
THEN markdown is returned."
  (should (eq 'markdown
              (shipit--detect-body-format "# Heading\n\nSome **bold** text."))))

(ert-deftest test-shipit-detect-body-format-org-title ()
  "GIVEN text starting with #+TITLE: keyword
WHEN detecting body format
THEN org is returned."
  (should (eq 'org
              (shipit--detect-body-format "#+TITLE: My Project\n\nContent."))))

(ert-deftest test-shipit-detect-body-format-org-options ()
  "GIVEN text containing #+OPTIONS: keyword
WHEN detecting body format
THEN org is returned."
  (should (eq 'org
              (shipit--detect-body-format
               "Some intro\n#+OPTIONS: toc:nil\n\nBody text"))))

(ert-deftest test-shipit-detect-body-format-org-properties-drawer ()
  "GIVEN text containing a :PROPERTIES: drawer
WHEN detecting body format
THEN org is returned."
  (should (eq 'org
              (shipit--detect-body-format
               "* Heading\n:PROPERTIES:\n:ID: abc\n:END:\n"))))

(ert-deftest test-shipit-detect-body-format-org-keywords-case-insensitive ()
  "GIVEN org keywords in lowercase form (#+title:)
WHEN detecting body format
THEN org is returned (case-insensitive match)."
  (should (eq 'org
              (shipit--detect-body-format "#+title: lower\n\nbody"))))

(ert-deftest test-shipit-detect-body-format-embedded-src-block-stays-markdown ()
  "GIVEN markdown text that happens to embed an org src block
WHEN detecting body format
THEN markdown is returned (org src blocks alone are not enough)."
  (should (eq 'markdown
              (shipit--detect-body-format
               "# Markdown heading\n\nSome example:\n\n#+BEGIN_SRC python\nprint(1)\n#+END_SRC\n"))))

(ert-deftest test-shipit-format-for-readme-filename-org ()
  "GIVEN a README filename ending in .org
WHEN selecting renderer format
THEN org is returned."
  (should (eq 'org (shipit--format-for-readme-filename "README.org")))
  (should (eq 'org (shipit--format-for-readme-filename "readme.org"))))

(ert-deftest test-shipit-format-for-readme-filename-markdown ()
  "GIVEN a README filename without .org extension or no filename
WHEN selecting renderer format
THEN markdown is returned (the default)."
  (should (eq 'markdown (shipit--format-for-readme-filename "README.md")))
  (should (eq 'markdown (shipit--format-for-readme-filename "README")))
  (should (eq 'markdown (shipit--format-for-readme-filename nil))))

(ert-deftest test-shipit-render-org-empty ()
  "GIVEN nil or empty input
WHEN calling shipit--render-org
THEN empty string is returned (matches markdown renderer behavior)."
  (should (string= "" (shipit--render-org nil))))

(ert-deftest test-shipit-render-org-preserves-content ()
  "GIVEN simple org content
WHEN rendering with shipit--render-org
THEN the rendered string contains the original text."
  (let ((result (shipit--render-org "* Heading\nBody text")))
    (should (stringp result))
    (should (string-match-p "Heading" result))
    (should (string-match-p "Body text" result))))

(ert-deftest test-shipit-render-org-uses-cache ()
  "GIVEN identical input rendered twice
WHEN calling shipit--render-org
THEN the second call returns the same cached propertized string."
  (shipit--org-cache-clear)
  (let* ((text "* Heading\nBody")
         (first (shipit--render-org text))
         (second (shipit--render-org text)))
    (should (eq first second))))

(ert-deftest test-shipit-render-body-dispatches-to-org-on-detection ()
  "GIVEN text that contains org document keywords
WHEN calling shipit--render-body without explicit format
THEN org renderer is invoked."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit--render-org)
               (lambda (text) (setq called-with (list 'org text)) text))
              ((symbol-function 'shipit--render-markdown)
               (lambda (text) (setq called-with (list 'markdown text)) text)))
      (shipit--render-body "#+TITLE: hi\nbody")
      (should (eq (car called-with) 'org)))))

(ert-deftest test-shipit-render-body-dispatches-to-markdown-on-plain ()
  "GIVEN plain text without org markers
WHEN calling shipit--render-body without explicit format
THEN markdown renderer is invoked."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit--render-org)
               (lambda (text) (setq called-with (list 'org text)) text))
              ((symbol-function 'shipit--render-markdown)
               (lambda (text) (setq called-with (list 'markdown text)) text)))
      (shipit--render-body "# Markdown\n\nbody")
      (should (eq (car called-with) 'markdown)))))

(ert-deftest test-shipit-render-body-respects-explicit-format ()
  "GIVEN explicit format argument
WHEN calling shipit--render-body
THEN that format wins over content detection."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit--render-org)
               (lambda (text) (setq called-with (list 'org text)) text))
              ((symbol-function 'shipit--render-markdown)
               (lambda (text) (setq called-with (list 'markdown text)) text)))
      (shipit--render-body "# Markdown\n\nbody" 'org)
      (should (eq (car called-with) 'org))
      (setq called-with nil)
      (shipit--render-body "#+TITLE: hi\nbody" 'markdown)
      (should (eq (car called-with) 'markdown)))))

(ert-deftest test-shipit-strip-face-on-whitespace-keeps-inter-word ()
  "Inter-word whitespace on a single line keeps its `font-lock-face'
so underlined runs (org-link) stay visually continuous across spaces."
  (let* ((s (propertize "I want the cursor" 'font-lock-face 'org-link))
         (cleaned (shipit--strip-face-on-whitespace s)))
    ;; Spaces inside the run still have face
    (should (eq 'org-link (get-text-property 1 'font-lock-face cleaned)))
    (should (eq 'org-link (get-text-property 7 'font-lock-face cleaned)))))

(ert-deftest test-shipit-strip-face-on-whitespace-line-edges ()
  "Trailing whitespace + newline and leading whitespace lose face."
  (let* ((s (concat (propertize "abc " 'font-lock-face 'org-link)
                    (propertize "\n   " 'font-lock-face 'org-link)
                    (propertize "def" 'font-lock-face 'org-link)))
         (cleaned (shipit--strip-face-on-whitespace s)))
    ;; "abc" keeps face
    (should (eq 'org-link (get-text-property 0 'font-lock-face cleaned)))
    ;; trailing space before newline loses face
    (should (eq nil (get-text-property 3 'font-lock-face cleaned)))
    ;; newline loses face
    (should (eq nil (get-text-property 4 'font-lock-face cleaned)))
    ;; leading whitespace loses face
    (should (eq nil (get-text-property 5 'font-lock-face cleaned)))
    ;; word after leading whitespace keeps face
    (should (eq 'org-link (get-text-property 8 'font-lock-face cleaned)))))

(ert-deftest test-shipit-strip-face-on-whitespace-handles-empty ()
  "Empty/nil inputs are returned unchanged."
  (should (string= "" (shipit--strip-face-on-whitespace "")))
  (should (null (shipit--strip-face-on-whitespace nil))))

(ert-deftest test-shipit-gif-pixel-dimensions-parses-header ()
  "GIVEN a fake GIF89a file with width=300, height=200
WHEN parsing the header
THEN returns (300 . 200)."
  (let ((tmp (make-temp-file "shipit-gif-test" nil ".gif")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert (concat "GIF89a"
                              (string (mod 300 256) (/ 300 256)
                                      (mod 200 256) (/ 200 256))))))
          (let ((dim (shipit--gif-pixel-dimensions tmp)))
            (should (equal '(300 . 200) dim))))
      (delete-file tmp))))

(ert-deftest test-shipit-gif-pixel-dimensions-rejects-non-gif ()
  "GIVEN a non-GIF file
WHEN parsing as GIF dimensions
THEN returns nil."
  (let ((tmp (make-temp-file "shipit-not-gif" nil ".bin")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert "PNG\x89\x00\x00\x00")))
          (should-not (shipit--gif-pixel-dimensions tmp)))
      (delete-file tmp))))

(ert-deftest test-shipit-org-image-links-wrapped-link ()
  "GIVEN org `[[LINK][file:IMG]]' wrapper around a badge image
WHEN converting org image links to HTML
THEN result is `<img src=\"IMG\" />' (the wrapping link is dropped)."
  (let ((out (shipit--org-image-links-to-html
              "[[https://elpa.nongnu.org/p.html][file:https://example.com/badge.svg]]")))
    (should (string-match-p "<img src=\"https://example\\.com/badge\\.svg\" />" out))
    (should-not (string-match-p "\\[\\[" out))))

(ert-deftest test-shipit-org-image-links-bare-file-link ()
  "GIVEN bare org `[[file:IMG]]' image link
WHEN converting org image links to HTML
THEN result is `<img src=\"IMG\" alt=\"\" />'."
  (let ((out (shipit--org-image-links-to-html "[[file:foo.png]]")))
    (should (string-match-p "<img src=\"foo\\.png\" alt=\"\" />" out))))

(ert-deftest test-shipit-org-image-links-with-alt ()
  "GIVEN org `[[file:IMG][ALT]]' with alt text
WHEN converting org image links to HTML
THEN result is `<img src=\"IMG\" alt=\"ALT\" />'."
  (let ((out (shipit--org-image-links-to-html "[[file:bar.svg][BarAlt]]")))
    (should (string-match-p "<img src=\"bar\\.svg\" alt=\"BarAlt\" />" out))))

(ert-deftest test-shipit-rewrite-relative-markdown-links-converts-paths ()
  "GIVEN a relative markdown link `[SDK](sdk/README.md)' inside a repo
WHEN rewriting relative links
THEN the URL becomes the absolute github.com/<repo>/blob/HEAD/<path> form."
  (let ((shipit--image-base-repo "owner/repo"))
    (let ((out (shipit--rewrite-relative-markdown-links "- [SDK](sdk/README.md)\n")))
      (should (string-match-p "\\[SDK\\](https://github\\.com/owner/repo/blob/HEAD/sdk/README\\.md)"
                              out)))))

(ert-deftest test-shipit-rewrite-relative-markdown-links-leaves-absolute-alone ()
  "GIVEN already-absolute markdown links
WHEN rewriting relative links
THEN https URLs, anchors, and mailto links are not modified."
  (let ((shipit--image-base-repo "owner/repo"))
    (dolist (input '("[X](https://example.com/y)"
                     "[Anchor](#section)"
                     "[Email](mailto:foo@example.com)"))
      (should (string= input (shipit--rewrite-relative-markdown-links input))))))

(ert-deftest test-shipit-rewrite-relative-markdown-links-skips-images ()
  "GIVEN an image link `![alt](path/to.png)'
WHEN rewriting relative links
THEN the image link is not rewritten (image processor handles those)."
  (let ((shipit--image-base-repo "owner/repo"))
    (let ((out (shipit--rewrite-relative-markdown-links "![alt](path/to.png)\n")))
      (should (string-match-p "!\\[alt\\](path/to\\.png)" out))
      (should-not (string-match-p "github\\.com" out)))))

(ert-deftest test-shipit-org-image-links-merges-consecutive-lines ()
  "GIVEN org image links on consecutive lines (one per badge)
WHEN converting org image links to HTML
THEN consecutive image-only lines collapse to a single line so badges
     render side-by-side."
  (let* ((src (concat "[[https://a.example/p][file:https://a.example/badge.svg]]\n"
                      "[[https://b.example/p][file:https://b.example/badge.svg]]\n"))
         (out (shipit--org-image-links-to-html src)))
    ;; No newline survives between the two img tags.
    (should (string-match-p "<img src=\"https://a\\.example/badge\\.svg\" /> <img src=\"https://b\\.example/badge\\.svg\" />"
                            out))
    (should-not (string-match-p "/>\\s-*\\n\\s-*<img" out))))

(ert-deftest test-shipit-resolve-reference-style-images-shorthand ()
  "GIVEN markdown with shorthand reference image and matching definition
WHEN resolving reference-style images
THEN the shorthand becomes inline form and the definition is removed."
  (let* ((src (concat "[![npm]](https://www.npmjs.com/foo)\n"
                      "\n"
                      "[npm]: https://example.com/badge.svg\n"
                      "\n"
                      "rest.\n"))
         (out (shipit--resolve-reference-style-images src)))
    (should (string-match-p
             "\\[!\\[npm\\](https://example\\.com/badge\\.svg)\\](https://www\\.npmjs\\.com/foo)"
             out))
    (should-not (string-match-p "^\\[npm\\]:" out))
    (should (string-match-p "rest\\." out))))

(ert-deftest test-shipit-resolve-reference-style-images-alt-and-ref ()
  "GIVEN alt+ref image with a separate definition
WHEN resolving reference-style images
THEN the result is inline form."
  (let* ((src "Badge: ![Node.js][nodejs]\n[nodejs]: https://example.com/n.svg\n")
         (out (shipit--resolve-reference-style-images src)))
    (should (string-match-p "!\\[Node\\.js\\](https://example\\.com/n\\.svg)" out))
    (should-not (string-match-p "^\\[nodejs\\]:" out))))

(ert-deftest test-shipit-resolve-reference-style-images-leaves-unknown-alone ()
  "GIVEN a reference-style image whose label has no definition
WHEN resolving reference-style images
THEN the original text is left unchanged."
  (let* ((src "![label][missing]\nmore.\n")
         (out (shipit--resolve-reference-style-images src)))
    (should (string= src out))))

(ert-deftest test-shipit-create-link-overlays-by-line-splits-at-newline ()
  "GIVEN a wrapped link spanning two lines with continuation indent
WHEN creating link overlays via shipit--create-link-overlays-by-line
THEN one overlay covers the visible chars on each line, skipping the
     leading whitespace of the continuation line — so the configured
     face (underline) does not paint across the indent."
  (with-temp-buffer
    (insert "Privacy\n     Policy.")
    (let* ((start (point-min))
           (end (1- (point-max)))
           (created '()))
      (shipit--create-link-overlays-by-line
       start end
       (lambda (ov)
         (push (cons (overlay-start ov) (overlay-end ov)) created)))
      (should (= 2 (length created)))
      (let* ((sorted (sort created #'car-less-than-car))
             (l1 (nth 0 sorted))
             (l2 (nth 1 sorted)))
        (should (= 1 (car l1)))
        (should (= 8 (cdr l1)))
        (should (= 14 (car l2)))
        (should (= end (cdr l2)))))))

(ert-deftest test-shipit--apply-comment-body-decorations-blockquote ()
  "GIVEN a buffer with a `> '-prefixed blockquote line
WHEN `shipit--apply-comment-body-decorations' decorates the region
THEN `shipit-blockquote-face' lives on `font-lock-face' across the
     line -- so activity-body callers (notifications buffer,
     PR-buffer activity timeline) get the same blockquote styling
     the comment renderer applies."
  (with-temp-buffer
    (insert "> quoted text\nplain\n")
    (shipit--apply-comment-body-decorations (point-min) (point-max))
    (let ((flf (get-text-property (point-min) 'font-lock-face)))
      (should flf)
      (should (or (eq flf 'shipit-blockquote-face)
                  (and (listp flf)
                       (memq 'shipit-blockquote-face flf)))))))

(ert-deftest test-shipit--apply-comment-body-decorations-noop-empty-region ()
  "GIVEN BODY-START equal to BODY-END (e.g. activity body inserted nothing)
WHEN `shipit--apply-comment-body-decorations' is invoked
THEN it returns silently without touching the buffer or signalling --
     decorators must tolerate empty bodies for activity events that
     have no body text (label, assigned, force-pushed, ...)."
  (with-temp-buffer
    (insert "untouched\n")
    (let ((before (buffer-string)))
      (shipit--apply-comment-body-decorations (point-min) (point-min))
      (should (string= before (buffer-string))))))

(ert-deftest test-shipit--apply-strikethrough-faces-double-tilde ()
  "GIVEN a buffer containing GFM strikethrough markup ~~text~~
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN `shipit-strikethrough-face' lives on `font-lock-face' across the
     match -- writing to plain `face' gets wiped by font-lock-mode
     in shipit buffers, which is why strikethrough never showed."
  (with-temp-buffer
    (insert "done ~~removed text~~ tail\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (let ((flf (get-text-property 8 'font-lock-face)))
      (should (or (eq flf 'shipit-strikethrough-face)
                  (and (listp flf)
                       (memq 'shipit-strikethrough-face flf)))))))

(ert-deftest test-shipit--apply-strikethrough-faces-single-tilde ()
  "GIVEN a buffer containing single-tilde strikethrough markup ~text~
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN `shipit-strikethrough-face' lives on `font-lock-face' across the match."
  (with-temp-buffer
    (insert "keep ~old~ tail\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (let ((flf (get-text-property 8 'font-lock-face)))
      (should (or (eq flf 'shipit-strikethrough-face)
                  (and (listp flf)
                       (memq 'shipit-strikethrough-face flf)))))))

(ert-deftest test-shipit--apply-strikethrough-faces-survives-face-strip ()
  "GIVEN a decorated buffer whose `face' props font-lock then wipes
     (shipit buffers enable font-lock-mode, and refontification clears
     the `face' property -- observed live: markdown's strikethrough face
     was stripped from a PR buffer, leaving struck text looking plain)
WHEN the `face' property is removed from the whole buffer
THEN the strikethrough styling survives on `font-lock-face'."
  (with-temp-buffer
    (insert "note ~~obsolete advice~~ end\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (let ((flf (get-text-property 8 'font-lock-face)))
      (should (or (eq flf 'shipit-strikethrough-face)
                  (and (listp flf)
                       (memq 'shipit-strikethrough-face flf)))))))

(ert-deftest test-shipit--apply-strikethrough-faces-preserves-existing ()
  "GIVEN struck text that already carries a `font-lock-face' (e.g. bold
     from the markdown renderer)
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN both the pre-existing face and `shipit-strikethrough-face' apply."
  (with-temp-buffer
    (insert "~~struck bold~~\n")
    (put-text-property (point-min) (point-max) 'font-lock-face 'bold)
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (let ((flf (get-text-property 4 'font-lock-face)))
      (should (listp flf))
      (should (memq 'shipit-strikethrough-face flf))
      (should (memq 'bold flf)))))

(ert-deftest test-shipit--apply-strikethrough-faces-wrapped-span ()
  "GIVEN a ~~...~~ span that the body wrapper broke across a newline
     with continuation-line indentation (exactly how long struck
     sentences render in PR descriptions -- observed live in a PR
     buffer where the span never got its face)
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN the strikethrough face applies across the whole wrapped span."
  (with-temp-buffer
    (insert "   ~~Note: faster readout will come\n   as a follow-up.~~\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "follow-up")
    (let ((flf (get-text-property (match-beginning 0) 'font-lock-face)))
      (should (or (eq flf 'shipit-strikethrough-face)
                  (and (listp flf)
                       (memq 'shipit-strikethrough-face flf)))))))

(ert-deftest test-shipit--apply-strikethrough-faces-wrapped-span-skips-indent ()
  "GIVEN a ~~...~~ span wrapped onto an indented continuation line
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN the newline and the continuation line's leading indentation carry
     no strikethrough face -- only the text does -- so the strike
     doesn't draw a floating line through empty space at the left
     margin of wrapped lines."
  (with-temp-buffer
    (insert "   ~~Note: faster readout will come\n   as a follow-up.~~\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "come\n")
    (let ((newline-pos (1- (point)))
          (indent-pos (point)))
      (should-not (get-text-property newline-pos 'font-lock-face))
      (should-not (get-text-property indent-pos 'font-lock-face))
      (search-forward "as a")
      (let ((flf (get-text-property (match-beginning 0) 'font-lock-face)))
        (should (or (eq flf 'shipit-strikethrough-face)
                    (and (listp flf)
                         (memq 'shipit-strikethrough-face flf))))))))

(ert-deftest test-shipit--apply-strikethrough-faces-rejects-paragraph-break ()
  "GIVEN an opening ~~ and a closing ~~ separated by a blank line
     (a paragraph break -- GFM strikethrough never spans paragraphs)
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN no strikethrough face is applied."
  (with-temp-buffer
    (insert "~~not a span\n\nstill not~~\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "still")
    (should-not (get-text-property (match-beginning 0) 'font-lock-face))))

(ert-deftest test-shipit--apply-strikethrough-faces-single-tilde-line-bound ()
  "GIVEN two single tildes on different lines (e.g. two home-directory
     paths -- not strikethrough markup)
WHEN `shipit--apply-strikethrough-faces' decorates the region
THEN nothing is struck -- only ~~...~~ may span wrapped lines."
  (with-temp-buffer
    (insert "run ~/bin/a then\nrun ~/bin/b now\n")
    (shipit--apply-strikethrough-faces (point-min) (point-max))
    (should-not (text-property-not-all (point-min) (point-max)
                                       'font-lock-face nil))))

(ert-deftest test-shipit--apply-comment-body-decorations-strikethrough ()
  "GIVEN a buffer with ~~strikethrough~~ markup in a comment body
WHEN `shipit--apply-comment-body-decorations' decorates the region
THEN `shipit-strikethrough-face' lives on `font-lock-face' -- so every
     body renderer (comments, descriptions, activity bodies) shows
     strikethrough through the shared decoration pass."
  (with-temp-buffer
    (insert "we ~~no longer~~ do this\n")
    (shipit--apply-comment-body-decorations (point-min) (point-max))
    (let ((flf (get-text-property 6 'font-lock-face)))
      (should (or (eq flf 'shipit-strikethrough-face)
                  (and (listp flf)
                       (memq 'shipit-strikethrough-face flf)))))))

(ert-deftest test-shipit-render-icon-cache-keys-on-font-height ()
  "GIVEN the same icon asked for while the default face height differs
WHEN each request is served
THEN svg-lib builds one icon per height rather than one for both,
AND a repeat under the latest height comes from the cache.
svg-lib bakes the selected frame\='s font height into the SVG it
builds, so an icon cached under one font answers at the wrong size
under another: notification rows then mix icon sizes."
  (let ((shipit--svg-lib-icon-cache (make-hash-table :test 'equal))
        (builds 0)
        (old-height (face-attribute 'default :height)))
    (cl-letf (((symbol-function 'svg-lib-icon)
               (lambda (&rest _) (cl-incf builds) '(image :type svg :data "<svg/>"))))
      (unwind-protect
          (progn
            (set-face-attribute 'default nil :height 100)
            (shipit--svg-lib-icon-with-bg "bell" nil :collection "octicons")
            (set-face-attribute 'default nil :height 150)
            (shipit--svg-lib-icon-with-bg "bell" nil :collection "octicons")
            (should (= 2 builds))
            (shipit--svg-lib-icon-with-bg "bell" nil :collection "octicons")
            (should (= 2 builds)))
        (when (numberp old-height)
          (set-face-attribute 'default nil :height old-height))))))

;;;; Videos

(defconst test-shipit-render--tracker-video
  "https://jira.example.com/rest/api/3/attachment/content/31733?redirect=false"
  "A video attached on a tracker, whose URL does not name the file.")

(defun test-shipit-render--tracker-login (url)
  "Login headers for URL on the stand-in tracker, as a tracker's hook gives them."
  (when (string-match-p "\\`https://jira\\.example\\.com/" url)
    '(("Authorization" . "Basic x"))))

(defun test-shipit-render--video-url-of (text)
  "Return the video URL the first Play button of TEXT plays, or nil."
  (when-let* ((at (string-match "Play" text)))
    (get-text-property at 'shipit-video-url text)))

(ert-deftest test-shipit-render-video-link-named-by-its-text-plays ()
  "GIVEN a markdown link whose text names a video file, and whose URL does not
WHEN the videos of a body are processed
THEN the link becomes a Play button for its URL, named by the file."
  (let ((out (shipit--process-inline-videos
              (format "see [Screencast\\_1.webm](%s) here" test-shipit-render--tracker-video))))
    (should (string-match-p "Play Screencast_1.webm" out))
    (should-not (string-match-p "attachment/content" (substring-no-properties out)))
    (should (equal test-shipit-render--tracker-video (test-shipit-render--video-url-of out)))))

(ert-deftest test-shipit-render-media-step-shows-a-video ()
  "GIVEN rendered text with a bare video URL
WHEN its media are processed, as both the markdown and the org renderer do
THEN it shows a Play button for that URL."
  (let ((out (shipit--process-inline-media "Watch https://example.com/demo-media-step.mp4 now")))
    (should (equal "https://example.com/demo-media-step.mp4"
                   (test-shipit-render--video-url-of out)))))

(ert-deftest test-shipit-render-markdown-shows-a-video ()
  "GIVEN a markdown body with a bare video URL
WHEN it is rendered
THEN it shows a Play button for that URL, as an org body does.
Runs where the real markdown-mode is installed; without it markdown is
not rendered, and test-stubs provides a stand-in by its name."
  (skip-unless (and (require 'markdown-mode nil t) (boundp 'markdown-mode-version)))
  (let ((out (shipit--render-markdown "Watch https://example.com/demo-render-test.mp4 now")))
    (should (equal "https://example.com/demo-render-test.mp4"
                   (test-shipit-render--video-url-of out)))))

(defun test-shipit-render--page-of (file-url)
  "Return the text of the local page FILE-URL, and delete the page."
  (let ((file (url-filename (url-generic-parse-url file-url))))
    (prog1 (with-temp-buffer (insert-file-contents file) (buffer-string))
      (delete-file file))))

(defmacro test-shipit-render--playing (player &rest body)
  "Run BODY where PLAYER is on the path, or none is when nil.
In BODY, `opened' is what the browser was given and `ran' what a
player was started with."
  (declare (indent 1))
  `(let ((shipit-video-embed-in-frame nil)
         (opened nil)
         (ran nil))
     (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
               ((symbol-function 'executable-find) (lambda (&rest _) ,player))
               ((symbol-function 'start-process) (lambda (&rest args) (setq ran args) nil))
               ((symbol-function 'call-process) (lambda (&rest args) (setq ran args) 0)))
       ,@body)))

(ert-deftest test-shipit-render-video-behind-a-login-plays-in-a-browser-page ()
  "GIVEN a video whose site needs a login, and no tracker hands out a URL
without one
WHEN it is played where mpv is on the path
THEN no player is started, as it has no login, and the browser opens a
page whose video element plays the URL."
  (let ((shipit-image-auth-functions (list #'test-shipit-render--tracker-login))
        (shipit-media-url-functions nil))
    (test-shipit-render--playing "/usr/bin/mpv"
      (shipit--play-video-url test-shipit-render--tracker-video)
      (should-not ran)
      (should (string-prefix-p "file://" opened))
      (should (string-match-p (regexp-quote "<video src=\"https://jira.example.com/rest/api/3/attachment/content/31733?redirect=false\"")
                              (test-shipit-render--page-of opened))))))

(ert-deftest test-shipit-render-video-plays-the-url-the-tracker-hands-out ()
  "GIVEN a video whose tracker hands out a URL that needs no login
WHEN it is played where mpv is on the path
THEN mpv plays the URL the tracker handed out."
  (let ((shipit-image-auth-functions (list #'test-shipit-render--tracker-login))
        (shipit-media-url-functions
         (list (lambda (url)
                 (and (equal url test-shipit-render--tracker-video)
                      "https://media.example.com/file/1?token=t&dl=true")))))
    (test-shipit-render--playing "/usr/bin/mpv"
      (shipit--play-video-url test-shipit-render--tracker-video)
      (should (equal "mpv" (car ran)))
      (should (member "https://media.example.com/file/1?token=t&dl=true" ran)))))

(ert-deftest test-shipit-render-video-button-shows-a-hand ()
  "GIVEN a video's Play button
THEN the mouse pointer over it is a hand, as over any link."
  (let ((out (shipit--render-video-link "https://example.com/v.mp4" "v.mp4")))
    (should (eq 'hand (get-text-property (string-match "Play" out) 'pointer out)))))

(ert-deftest test-shipit-render-video-page-has-no-scrollbar ()
  "GIVEN a page made to play a video
THEN its video fills the window and nothing scrolls: a video laid out
inline leaves room under it, which gave the page a scrollbar."
  (let ((page (test-shipit-render--page-of (shipit--video-page "https://example.com/v.mp4"))))
    (should (string-match-p "overflow:hidden" page))
    (should (string-match-p "display:block" page))))

(ert-deftest test-shipit-render-video-without-a-player-plays-in-a-browser-page ()
  "GIVEN a video no login guards, and no player on the path
WHEN it is played
THEN the browser opens a page whose video element plays it, since a
browser downloads a video served as an attachment instead of playing it."
  (let ((shipit-image-auth-functions nil)
        (shipit-media-url-functions nil))
    (test-shipit-render--playing nil
      (shipit--play-video-url "https://example.com/demo.mp4?a=1&b=2")
      (should (string-match-p (regexp-quote "<video src=\"https://example.com/demo.mp4?a=1&amp;b=2\"")
                              (test-shipit-render--page-of opened))))))

(ert-deftest test-shipit-render-video-behind-a-login-has-no-thumbnail ()
  "GIVEN a video whose site needs a login
WHEN its Play button is made where images can show and ffmpeg exists
THEN ffmpeg is not asked for a thumbnail, as it has no login to fetch with."
  (let ((shipit-image-auth-functions (list #'test-shipit-render--tracker-login))
        (ran nil))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/ffmpeg"))
              ((symbol-function 'call-process) (lambda (&rest args) (setq ran args) 1)))
      (shipit--render-video-link test-shipit-render--tracker-video "clip.webm"))
    (should-not ran)))

(ert-deftest test-shipit-render-public-video-plays-in-the-player ()
  "GIVEN a video no login guards, and mpv as the player, not embedded
WHEN it is played
THEN mpv gets its URL."
  (let ((shipit-image-auth-functions nil)
        (shipit-video-embed-in-frame nil)
        (ran nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/mpv"))
              ((symbol-function 'call-process) (lambda (&rest args) (setq ran args) 0)))
      (shipit--play-video-url "https://example.com/demo.mp4"))
    (should (equal "mpv" (car ran)))
    (should (member "https://example.com/demo.mp4" ran))))

;;;; Videos played in the buffer

(defmacro test-shipit-render--with-video-button (inline &rest body)
  "Run BODY in a buffer holding the Play button of a public video.
INLINE stands in for `shipit-video-inline-function'.  Point is on the
button.  In BODY, `inlined' is the last call of INLINE, `opened' what
the browser was given, and `ran' what a player was started with."
  (declare (indent 1))
  `(let ((shipit-image-auth-functions nil)
         (shipit-media-url-functions nil)
         (inlined nil))
     (test-shipit-render--playing nil
       (let ((shipit-video-inline-function
              (lambda (&rest args) (setq inlined args) (funcall ,inline))))
         (with-temp-buffer
           (insert "Before\n" (shipit--render-video-link "https://example.com/v.mp4" "v.mp4") "After\n")
           (goto-char (point-min))
           (search-forward "Play")
           ,@body)))))

(defun test-shipit-render--press-play ()
  "Press RET on the Play button at point, through its own keys."
  (call-interactively (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))))

(defvar test-shipit-render--frame-pressed nil
  "How often the stand-in video frame was pressed.")

(defun test-shipit-render--video-frame ()
  "A stand-in for the text of a video played in the buffer.
RET on it counts a press, as a real one plays or pauses its video."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                (lambda () (interactive) (cl-incf test-shipit-render--frame-pressed)))
    (propertize "[VIDEO]" 'keymap map)))

(defun test-shipit-render--line-after-button ()
  "Return the text of the line after the Play button at point."
  (save-excursion
    (forward-line 1)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(ert-deftest test-shipit-render-video-plays-under-its-button ()
  "GIVEN a video's Play button, where videos can play in the buffer
WHEN RET is pressed on it
THEN the video plays at once on the line under the button, from a page
whose video element plays it, sized for the buffer, and nothing opens
outside."
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (test-shipit-render--press-play)
    (should (string-match-p "\\[VIDEO\\]" (test-shipit-render--line-after-button)))
    (pcase-let ((`(,page ,width ,height ,host) inlined))
      (should (string-match-p (regexp-quote "<video src=\"https://example.com/v.mp4\" controls autoplay")
                              (test-shipit-render--page-of page)))
      (should (= height (round (* width 9) 16)))
      (should (eq host (current-buffer))))
    (should-not opened)
    (should-not ran)))

(ert-deftest test-shipit-render-video-shown-plays-at-a-press-of-its-button ()
  "GIVEN a video shown under its Play button
WHEN RET is pressed on the button
THEN the video's frame is pressed, which plays or pauses it, and no
other video is put in or played."
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (let ((test-shipit-render--frame-pressed 0))
      (test-shipit-render--press-play)
      (test-shipit-render--page-of (car inlined))
      (setq inlined nil)
      (test-shipit-render--press-play)
      (should (= 1 test-shipit-render--frame-pressed))
      (should (= 1 (how-many "\\[VIDEO\\]" (point-min) (point-max))))
      (should-not inlined)
      (should-not opened))))

(ert-deftest test-shipit-render-videos-show-paused-when-the-buffer-is-ready ()
  "GIVEN a buffer drawn with a video's Play button
WHEN its videos are shown, as the buffer's ready hook does, twice
THEN the video is under its button once, from a page that shows its
first frame and waits for a click to play."
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (shipit--show-inline-videos)
    (let ((page (test-shipit-render--page-of (car inlined))))
      (should (string-match-p "<video src=\"https://example.com/v.mp4\" controls preload" page))
      (should-not (string-match-p "autoplay" page)))
    (setq inlined nil)
    (shipit--show-inline-videos)
    (should-not inlined)
    (should (string-match-p "\\[VIDEO\\]" (test-shipit-render--line-after-button)))
    (should (= 1 (how-many "\\[VIDEO\\]" (point-min) (point-max))))))

(ert-deftest test-shipit-render-videos-hidden-wait-for-their-button ()
  "GIVEN a video whose Play button is hidden, as in a shut section
WHEN the buffer's videos are shown
THEN that video is left to its button."
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (put-text-property (point-min) (point-max) 'invisible t)
    (shipit--show-inline-videos)
    (should-not inlined)))

(ert-deftest test-shipit-render-videos-show-only-when-asked ()
  "GIVEN videos set not to show until their Play button is pressed
WHEN the buffer's videos are shown
THEN none is put in."
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (let ((shipit-video-inline-show nil))
      (shipit--show-inline-videos))
    (should-not inlined)))

(ert-deftest test-shipit-render-videos-arriving-later-show-too ()
  "GIVEN a buffer that watches for videos, drawn without one
WHEN plain text arrives, and then two comments with a video each
THEN the plain text asks for nothing, the comments ask once, and once
Emacs is idle each video shows under its button."
  (let ((shipit-image-auth-functions nil)
        (shipit-media-url-functions nil)
        (shipit-video-inline-function (lambda (&rest _) (test-shipit-render--video-frame))))
    (with-temp-buffer
      (shipit--watch-videos)
      (insert "Plain text\n")
      (should-not shipit--videos-timer)
      (insert "A comment:" (shipit--render-video-link "https://example.com/a.mp4" "a.mp4"))
      (let ((timer shipit--videos-timer))
        (should (timerp timer))
        (insert "Another:" (shipit--render-video-link "https://example.com/b.mp4" "b.mp4"))
        (should (eq timer shipit--videos-timer))
        (cancel-timer timer)
        (apply (timer--function timer) (timer--args timer)))
      (should (= 2 (how-many "\\[VIDEO\\]" (point-min) (point-max))))
      (should-not shipit--videos-timer))))

(ert-deftest test-shipit-render-videos-show-in-issue-and-pr-buffers ()
  "GIVEN an issue buffer and a PR buffer
THEN each watches for videos, whenever its text arrives."
  (require 'shipit-issues-buffer)
  (require 'shipit-buffer)
  (dolist (mode '(shipit-issue-mode shipit-mode))
    (with-temp-buffer
      (funcall mode)
      (should (memq #'shipit--video-buttons-inserted after-change-functions)))))

(ert-deftest test-shipit-render-video-plays-outside-when-the-buffer-cannot ()
  "GIVEN a video's Play button, where the buffer cannot play it, as in a
terminal frame
WHEN RET is pressed on it
THEN it plays outside the buffer, as without inline videos."
  (test-shipit-render--with-video-button #'ignore
    (test-shipit-render--press-play)
    (test-shipit-render--page-of (car inlined))
    (should (string-prefix-p "file://" opened))
    (test-shipit-render--page-of opened)))

(ert-deftest test-shipit-render-video-plays-outside-without-canvas-browser ()
  "GIVEN a video's Play button, where the inline function names a package
that is not installed, as `canvas-browser-embed' does without canvas-browser
WHEN the buffer shows its videos, and RET is pressed on the button
THEN nothing is shown in the buffer, AND it plays outside the buffer."
  (let ((shipit-image-auth-functions nil)
        (shipit-media-url-functions nil)
        (shipit-video-inline-function 'test-shipit-render--not-installed-embed))
    (should-not (fboundp shipit-video-inline-function))
    (test-shipit-render--playing nil
      (with-temp-buffer
        (insert "Before\n" (shipit--render-video-link "https://example.com/v.mp4" "v.mp4") "After\n")
        (shipit--show-inline-videos)
        (should-not (text-property-any (point-min) (point-max) 'shipit-video-inline t))
        (goto-char (point-min))
        (search-forward "Play")
        (test-shipit-render--press-play)
        (should (string-prefix-p "file://" opened))))))

(provide 'test-shipit-render)
;;; test-shipit-render.el ends here
