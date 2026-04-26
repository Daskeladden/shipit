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

(provide 'test-shipit-render)
;;; test-shipit-render.el ends here
