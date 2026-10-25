;;; test-shipit-render.el --- Tests for render functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for shipit-render.el rendering functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-render)
(require 'shipit-pr-backends)
(require 'shipit-canvas)
(require 'shipit-http)
(require 'shipit-test-http)

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

(defun test-shipit-render--image-count ()
  "The number of pictures in the current buffer: runs of a `display'
property whose value is an image."
  (let ((pos (point-min))
        (count 0))
    (while (< pos (point-max))
      (when (eq (car-safe (get-text-property pos 'display)) 'image)
        (cl-incf count))
      (setq pos (next-single-property-change pos 'display nil (point-max))))
    count))

(defvar canvas-diagram-font)

(defun test-shipit-render--draw-mermaid (text)
  "Insert TEXT in the current buffer and run the mermaid pass on it, on a
frame that counts as graphic, as no batch frame does.
canvas-diagram then believes the frame too, and asks it for its font,
which a batch frame does not have, so the font is given."
  (insert text)
  (let ((canvas-diagram-font "Sans 12px"))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (shipit--render-mermaid-blocks-in-region (point-min) (point-max)))))

(defconst test-shipit-render--png
  (base64-decode-string "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")
  "A PNG of one pixel, for a stand-in of mmdc to write.")

(ert-deftest test-shipit-render-mermaid-draws-with-canvas-mermaid ()
  "GIVEN a mermaid block, canvas-mermaid that loads, and no mmdc
WHEN the mermaid pass runs
THEN the block shows one picture, its text is gone, AND both fence lines stay."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid "```mermaid\nflowchart TD\n  A --> B\n```\n")
      (should (= 1 (test-shipit-render--image-count)))
      (should-not (string-match-p "flowchart TD" (buffer-string)))
      (should (string-prefix-p "```mermaid\n" (buffer-string)))
      (should (string-suffix-p "\n```\n" (buffer-string))))))

(ert-deftest test-shipit-render-mermaid-prefers-canvas-mermaid-to-mmdc ()
  "GIVEN a mermaid block, and both canvas-mermaid and mmdc there
WHEN the mermaid pass runs
THEN canvas-mermaid draws the one picture, AND mmdc is not called."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available t)
        (calls nil))
    (cl-letf (((symbol-function 'shipit--mmdc-call)
               (lambda (&rest args) (push args calls) 0)))
      (with-temp-buffer
        (test-shipit-render--draw-mermaid "```mermaid\nflowchart TD\n  A --> B\n```\n")
        (should (= 1 (test-shipit-render--image-count)))
        (should-not calls)))))

(ert-deftest test-shipit-render-mermaid-uses-mmdc-without-canvas-mermaid ()
  "GIVEN a mermaid block, mmdc there, and canvas-mermaid not there
WHEN the mermaid pass runs
THEN mmdc draws the one picture, with the theme of `shipit-mermaid-theme'."
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available t)
        (calls nil))
    (cl-letf (((symbol-function 'shipit-canvas-library-available-p)
               (lambda (library) (not (eq library 'canvas-mermaid))))
              ((symbol-function 'shipit--mmdc-call)
               (lambda (_input output &rest args)
                 (push args calls)
                 (let ((coding-system-for-write 'binary))
                   (write-region test-shipit-render--png nil output nil 'silent))
                 0)))
      (with-temp-buffer
        (test-shipit-render--draw-mermaid "```mermaid\nflowchart TD\n  A --> B\n```\n")
        (should (= 1 (test-shipit-render--image-count)))
        (should (equal shipit-mermaid-theme (cadr (member "-t" (car calls)))))))))

(ert-deftest test-shipit-render-mermaid-keeps-a-bad-block-as-code-and-draws-the-next ()
  "GIVEN a mermaid block that canvas-mermaid cannot read, and a good block
after it
WHEN the mermaid pass runs
THEN the bad block stays code, the message of the reader is on the line
before its closing fence in the error face, AND the good block shows one
picture."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid
       "```mermaid\nflowchart TD\n  subgraph\n  end\n```\n\n```mermaid\nflowchart TD\n  A --> B\n```\n")
      (should (string-match "  subgraph\n  end\n\\(canvas-mermaid: a subgraph with no name\\)\n```\n"
                            (buffer-string)))
      (should (eq 'error (get-text-property (1+ (match-beginning 1)) 'font-lock-face)))
      (should (= 1 (test-shipit-render--image-count))))))

(ert-deftest test-shipit-render-mermaid-keeps-a-kind-it-cannot-draw-as-code ()
  "GIVEN a mermaid pie, a kind that canvas-mermaid has no layout for
WHEN the mermaid pass runs
THEN the block stays code, AND the message of the reader is on the line
before its closing fence."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid "```mermaid\npie\n  \"a\" : 1\n```\n")
      (should (string-suffix-p "```mermaid\npie\n  \"a\" : 1\ncanvas-mermaid: a pie has no layout to draw it yet\n```\n"
                               (buffer-string)))
      (should (= 0 (test-shipit-render--image-count))))))

(ert-deftest test-shipit-render-mermaid-draws-an-indented-block-with-front-matter ()
  "GIVEN a mermaid block with front matter, every line indented by the
three spaces of the preview
WHEN the mermaid pass runs
THEN the block shows one picture."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid
       "   ```mermaid\n   ---\n   title: T\n   ---\n   flowchart TD\n     A --> B\n   ```\n")
      (should (= 1 (test-shipit-render--image-count))))))

(ert-deftest test-shipit-render-mermaid-draws-each-of-two-blocks ()
  "GIVEN two mermaid blocks that canvas-mermaid can draw
WHEN the mermaid pass runs
THEN each block shows a picture."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid
       "```mermaid\nflowchart TD\n  A --> B\n```\n\nText.\n\n```mermaid\nflowchart TD\n  C --> D\n```\n")
      (should (= 2 (test-shipit-render--image-count))))))

(ert-deftest test-shipit-render-mermaid-message-stays-once-in-the-editor-preview ()
  "GIVEN a description draft that ends at the closing fence of a mermaid
block that canvas-mermaid cannot read, and the live preview of the editor
WHEN the preview draws the draft again at two pauses
THEN the preview holds the message of the reader once."
  (skip-unless (shipit-canvas-library-available-p 'canvas-mermaid))
  (require 'shipit-editor)
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil)
        (canvas-diagram-font "Sans 12px"))
    (with-temp-buffer
      (let ((source (current-buffer)))
        (insert "Title\n")
        (let ((overlay (make-overlay (point) (point))))
          (with-temp-buffer
            (insert "```mermaid\npie\n```")
            (setq-local shipit-editor--type 'description)
            (setq-local shipit-editor--source-buffer source)
            (setq-local shipit-editor--preview-overlay overlay)
            (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
              (shipit-editor--update-preview (current-buffer))
              (shipit-editor--update-preview (current-buffer))))
          (should (= 1 (how-many "canvas-mermaid: a pie has no layout" (point-min) (point-max)))))))))

(ert-deftest test-shipit-render-mermaid-leaves-an-empty-block-alone ()
  "GIVEN an empty mermaid block
WHEN the mermaid pass runs
THEN the text stays as it is, with no message line."
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (test-shipit-render--draw-mermaid "```mermaid\n```\n")
      (should (equal "```mermaid\n```\n" (buffer-string))))))

(ert-deftest test-shipit-render-mermaid-changes-nothing-without-a-graphic-frame ()
  "GIVEN a mermaid block that canvas-mermaid can draw, on a frame that is
not graphic
WHEN the mermaid pass runs
THEN the text stays as it is."
  (let ((shipit-render-mermaid t)
        (shipit--mmdc-available nil))
    (with-temp-buffer
      (insert "```mermaid\nflowchart TD\n  A --> B\n```\n")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
        (shipit--render-mermaid-blocks-in-region (point-min) (point-max)))
      (should (equal "```mermaid\nflowchart TD\n  A --> B\n```\n" (buffer-string))))))

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

;;; HTML comments

(ert-deftest test-shipit-html-comment-on-its-own-line-goes-with-the-line ()
  "GIVEN markdown with an HTML comment on a line of its own, as a pull
request template has under each heading
WHEN the HTML comments are taken out
THEN the line is gone too, AND no blank line stands in its place."
  (should (equal "### What\nAdd a thing\n"
                 (shipit--strip-html-comments
                  "### What\n<!-- A short summary -->\nAdd a thing\n"))))

(ert-deftest test-shipit-html-comment-over-several-lines-goes-with-its-lines ()
  "GIVEN an HTML comment that fills three lines
WHEN the HTML comments are taken out
THEN all three lines are gone."
  (should (equal "### Evidence\nIt works.\n"
                 (shipit--strip-html-comments
                  "### Evidence\n<!-- Screenshots.\nThe goal is to show it.\nSee above -->\nIt works.\n"))))

(ert-deftest test-shipit-html-comment-inside-a-line-leaves-the-rest ()
  "GIVEN an HTML comment between two words of a line
WHEN the HTML comments are taken out
THEN the words stand with one space between them."
  (should (equal "before after\n"
                 (shipit--strip-html-comments "before <!-- hidden --> after\n"))))

(ert-deftest test-shipit-html-comment-in-code-is-text ()
  "GIVEN an HTML comment in a fenced code block, AND one in a code span
WHEN the HTML comments are taken out
THEN both stay: there they are text that the author shows."
  (dolist (text '("```html\n<!-- kept -->\n```\n"
                  "~~~\n<!-- kept -->\n~~~\n"
                  "Write `<!-- kept -->` to hide a hint.\n"))
    (should (equal text (shipit--strip-html-comments text)))))

(ert-deftest test-shipit-html-comment-after-a-code-block-is-taken-out ()
  "GIVEN an HTML comment after a fenced code block that is closed
WHEN the HTML comments are taken out
THEN the comment goes, AND the code block stays as it is."
  (should (equal "```\ncode\n```\ntext\n"
                 (shipit--strip-html-comments "```\ncode\n```\n<!-- gone -->\ntext\n"))))

(ert-deftest test-shipit-html-comment-that-never-ends-hides-the-rest ()
  "GIVEN an HTML comment that is opened and never closed
WHEN the HTML comments are taken out
THEN the text from there on is gone, as a forge shows it."
  (should (equal "shown\n"
                 (shipit--strip-html-comments "shown\n<!-- not closed\nhidden\n"))))

(ert-deftest test-shipit-text-without-html-comments-is-left-as-it-is ()
  "GIVEN markdown without an HTML comment, with blank lines in it
WHEN the HTML comments are taken out
THEN the text is the same."
  (let ((text "### What\n\nAdd a thing\n\n- [x] None\n"))
    (should (equal text (shipit--strip-html-comments text)))))

;;; The hashes of a heading

(defun test-shipit-render--invisible-text ()
  "The text of this buffer that is invisible, each run as a string."
  (let ((runs nil) (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'invisible nil (point-max))))
        (when (get-text-property pos 'invisible)
          (push (buffer-substring-no-properties pos next) runs))
        (setq pos next)))
    (nreverse runs)))

(ert-deftest test-shipit-the-hashes-of-a-heading-are-hidden ()
  "GIVEN markdown that is fontified as markdown-mode leaves it: the
hashes of a heading have the face of a heading delimiter
WHEN the hashes of the headings are hidden
THEN the hashes in front of each heading, with the space after them,
are invisible, AND so are the hashes that close a heading, AND the text
of the heading is not."
  (with-temp-buffer
    (insert (propertize "###" 'face 'markdown-header-delimiter-face) " What\n"
            "Add a thing\n"
            (propertize "##" 'face '(markdown-header-delimiter-face bold)) " Why "
            (propertize "##" 'face 'markdown-header-delimiter-face) "\n")
    (shipit--hide-heading-hashes)
    (should (equal '("### " "## " " ##") (test-shipit-render--invisible-text)))
    (should (equal "### What\nAdd a thing\n## Why ##\n"
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-shipit-hashes-that-are-no-heading-stay ()
  "GIVEN hashes at the start of a line that markdown-mode did not take
for a heading: a comment in a code block, an issue number
WHEN the hashes of the headings are hidden
THEN nothing of these lines is invisible."
  (with-temp-buffer
    (insert (propertize "# a comment in code\n" 'face 'markdown-pre-face)
            "#123 is the issue\n")
    (shipit--hide-heading-hashes)
    (should-not (test-shipit-render--invisible-text))))

;;; A horizontal rule

(ert-deftest test-shipit-a-rule-of-dashes-is-drawn-as-a-line ()
  "GIVEN markdown that is fontified as markdown-mode leaves it: a line
of three dashes, AND one of stars with spaces, have the face of a rule
WHEN the rules are drawn
THEN each of the two lines is shown as a horizontal line, AND the text
of the buffer is as it was."
  (with-temp-buffer
    (insert "above\n\n"
            (propertize "---" 'face 'markdown-hr-face) "\n\n"
            (propertize "* * *" 'face '(markdown-hr-face)) "\n"
            "below\n")
    (shipit--draw-horizontal-rules)
    (dolist (rule '("---" "* * *"))
      (goto-char (point-min))
      (search-forward rule)
      (let ((shown (get-text-property (match-beginning 0) 'display)))
        (should (stringp shown))
        (should (string-match-p "\\`\u2500+\\'" shown))
        (should (equal shown (get-text-property (1- (match-end 0)) 'display)))))
    (should (equal "above\n\n---\n\n* * *\nbelow\n"
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-shipit-dashes-that-are-no-rule-stay-as-text ()
  "GIVEN three dashes that markdown-mode did not take for a rule: in a
code block, AND under a line of text, where they make it a heading
WHEN the rules are drawn
THEN neither is shown as a line."
  (with-temp-buffer
    (insert (propertize "---\n" 'face 'markdown-pre-face)
            "A title\n"
            (propertize "---" 'face 'markdown-header-rule-face) "\n")
    (shipit--draw-horizontal-rules)
    (should-not (text-property-not-all (point-min) (point-max) 'display nil))))

(ert-deftest test-shipit-an-html-rule-and-a-markdown-rule-are-the-same-line ()
  "GIVEN an <hr> tag, which is turned into a line before the markdown
is fontified, AND a rule of dashes, which is drawn after
THEN both are the same line of the same length."
  (with-temp-buffer
    (insert (propertize "---" 'face 'markdown-hr-face) "\n")
    (shipit--draw-horizontal-rules)
    (should (equal (get-text-property (point-min) 'display)
                   (string-trim-right
                    (substring-no-properties (shipit--convert-inline-html "<hr>")))))))

;;; The marks of a quote

(defun test-shipit-render--bar-places ()
  "The places of this buffer, counted from 0, where a quote bar is shown."
  (let ((places nil))
    (dotimes (place (buffer-size))
      (when (equal shipit--quote-bar (get-text-property (1+ place) 'display))
        (push place places)))
    (nreverse places)))

(ert-deftest test-shipit-the-mark-of-a-quote-is-drawn-as-a-bar ()
  "GIVEN markdown that is fontified as markdown-mode leaves it: the
first mark of a quote has the face of markup, AND a quote inside a
quote has its second mark in the text of the first
WHEN the marks of the quotes are drawn
THEN each of the three marks is shown as a bar, AND nothing else is,
AND the text of the buffer is as it was."
  (with-temp-buffer
    (insert (propertize ">" 'face '(markdown-markup-face markdown-blockquote-face))
            (propertize " one" 'face 'markdown-blockquote-face) "\n"
            (propertize ">" 'face '(markdown-markup-face markdown-blockquote-face))
            (propertize " > two" 'face 'markdown-blockquote-face) "\n")
    (shipit--draw-quote-bars)
    (should (equal '(0 6 8) (test-shipit-render--bar-places)))
    (should (equal "> one\n> > two\n"
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-shipit-a-mark-that-starts-no-quote-stays ()
  "GIVEN a `>' at the start of a line that markdown-mode did not take
for a quote: the prompt of a shell in a code block
WHEN the marks of the quotes are drawn
THEN nothing is shown as a bar."
  (with-temp-buffer
    (insert (propertize "> ls\n" 'face 'markdown-pre-face))
    (shipit--draw-quote-bars)
    (should-not (test-shipit-render--bar-places))))

(ert-deftest test-shipit-a-heading-in-a-quote-is-a-heading ()
  "GIVEN markdown that is fontified as markdown-mode leaves it: a quote
with a heading of the second level, as release notes have, AND a line
of text, which markdown-mode gives the face of a quote
WHEN the headings in quotes are drawn
THEN the hashes of the heading and the space after them are hidden,
the heading has the face of a heading of the second level, the line of
text has none, AND the text of the buffer is as it was."
  (with-temp-buffer
    (insert (propertize ">" 'face '(markdown-markup-face markdown-blockquote-face))
            (propertize " ## v1.3" 'face 'markdown-blockquote-face) "\n"
            (propertize ">" 'face '(markdown-markup-face markdown-blockquote-face))
            (propertize " Boil faster" 'face 'markdown-blockquote-face) "\n")
    (shipit--draw-quoted-headings)
    (should (equal '("## ") (test-shipit-render--invisible-text)))
    (should (shipit--markdown-face-p 6 'markdown-header-face-2))
    (should-not (shipit--markdown-face-p 14 'markdown-header-face-2))
    (should (equal "> ## v1.3\n> Boil faster\n"
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-shipit-a-heading-after-a-mark-that-starts-no-quote-stays ()
  "GIVEN a line that looks like a heading in a quote, in a code block
WHEN the headings in quotes are drawn
THEN nothing is hidden, AND the line gets no face of a heading."
  (with-temp-buffer
    (insert (propertize "> ## v1.3\n" 'face 'markdown-pre-face))
    (shipit--draw-quoted-headings)
    (should-not (test-shipit-render--invisible-text))
    (should-not (shipit--markdown-face-p 6 'markdown-header-face-2))))

(ert-deftest test-shipit-rendered-quote-shows-its-heading-as-a-heading ()
  "GIVEN markdown with a quote that starts with a heading of the third
level, as the release notes in a dependency update have
WHEN it is rendered
THEN the hashes are hidden, AND the heading has the face of a heading
of the third level."
  (skip-unless (and (require 'markdown-mode nil t) (boundp 'markdown-mode-version)))
  (let* ((shipit--markdown-render-cache (make-hash-table :test 'equal))
         (rendered (shipit--render-markdown "> ### Features\n>\n> - Boil faster\n"))
         (start (string-match "Features" rendered)))
    (with-temp-buffer
      (insert rendered)
      (should (equal '("### ") (test-shipit-render--invisible-text)))
      (should (shipit--markdown-face-p (1+ start) 'markdown-header-face-3)))))

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

;;; HTML tags

(ert-deftest test-shipit-html-emphasis-with-attributes-becomes-markdown ()
  "GIVEN markdown with <strong> and <em> tags that have attributes, AND
with <b> and <i> tags
WHEN the HTML tags are converted
THEN each is the emphasis of markdown, AND no tag is left."
  (should (equal "**Bold** and *soft*, **b** and *i*"
                 (shipit--convert-inline-html
                  (concat "<strong align=\"center\">Bold</strong> and "
                          "<em class=\"x\">soft</em>, <b>b</b> and <i>i</i>")))))

(ert-deftest test-shipit-html-link-around-an-image-becomes-a-markdown-link ()
  "GIVEN an <a> tag with an image in it, as the badge of a README is
WHEN the HTML tags are converted
THEN it is a markdown link around the image."
  (should (equal "[<img src=\"https://example.com/badge.svg\" alt=\"License\">](LICENSE)"
                 (shipit--convert-inline-html
                  (concat "<a href=\"LICENSE\">"
                          "<img src=\"https://example.com/badge.svg\" alt=\"License\">"
                          "</a>")))))

(ert-deftest test-shipit-html-link-takes-its-address-from-any-place-in-the-tag ()
  "GIVEN an <a> tag with an attribute before its address, AND the
address in single quotes
WHEN the HTML tags are converted
THEN it is a markdown link to that address."
  (should (equal "[Docs](docs/a.md)"
                 (shipit--convert-inline-html
                  "<a title=\"The docs\" href='docs/a.md'>Docs</a>"))))

(ert-deftest test-shipit-html-link-without-an-address-goes ()
  "GIVEN an <a> tag that only names a place in the text
WHEN the HTML tags are converted
THEN the tag is gone, AND the text around it stays."
  (should (equal "Usage"
                 (shipit--convert-inline-html "<a name=\"usage\"></a>Usage"))))

(ert-deftest test-shipit-html-link-has-no-white-space-at-its-edges ()
  "GIVEN an <a> tag around a picture on a line of its own, as the logo of
a README has, AND one in a centered paragraph, AND an <a> tag around a
word between spaces
WHEN the HTML tags are converted
THEN each markdown link starts and ends with what it shows, so that the
underline of the link shows no white space beside it, AND the text
around each link stays."
  (dolist (case '(("<a href=\"https://example.com\">\n<img src=\"cup.svg\">\n</a>"
                   . "[<img src=\"cup.svg\">](https://example.com)")
                  ("<p align=\"center\"><a href=\"https://example.com\">\n<img src=\"cup.svg\">\n</a></p>"
                   . "<shipit-center>\n\n[<img src=\"cup.svg\">](https://example.com)\n\n</shipit-center>")
                  ("Read <a href=\"docs/a.md\"> the docs </a> first."
                   . "Read [the docs](docs/a.md) first.")))
    (should (equal (cdr case) (shipit--convert-inline-html (car case))))))

(ert-deftest test-shipit-html-paragraphs-are-paragraphs-of-markdown ()
  "GIVEN two <p> elements with no blank line between them, and text
after them, as the head of a README has
WHEN the HTML tags are converted
THEN each is a paragraph, with one blank line between them, AND no <p>
tag is left."
  (should (equal (concat "<img src=\"./logo.png\" width=\"140\" />\n\n"
                         "**Tagline**\n\n"
                         "## Install\n")
                 (shipit--convert-inline-html
                  (concat "<p dir=\"auto\">\n"
                          "  <img src=\"./logo.png\" width=\"140\" />\n"
                          "</p>\n"
                          "<p dir=\"auto\">\n"
                          "  <strong align=\"center\">Tagline</strong>\n"
                          "</p>\n"
                          "\n\n"
                          "## Install\n")))))

(ert-deftest test-shipit-html-paragraph-lines-are-one-line ()
  "GIVEN a <p> element with one link on each of its lines
WHEN the HTML tags are converted
THEN the links stand on one line with one space between them, as a
browser shows them."
  (should (equal "[A](a.md) · [B](b.md)"
                 (shipit--convert-inline-html
                  (concat "<p dir=\"auto\">\n"
                          "  <a href=\"a.md\" title=\"A\">A</a> ·\n"
                          "  <a href=\"b.md\" title=\"B\">B</a>\n"
                          "</p>")))))

(ert-deftest test-shipit-html-heading-with-a-centered-block-is-a-centered-heading ()
  "GIVEN an <h2> element with a <center> tag at the start of its text,
which the end of the heading also ends, as the title of a README can be
WHEN the HTML tags are converted
THEN the title is one heading line, its marks before its text, AND it
stands between the markers of centered text, which end with the
heading."
  (should (equal "<shipit-center>\n\n## Cup of tea\n\n</shipit-center>\n\nText"
                 (shipit--convert-inline-html "<h2><center>Cup of tea</h2>\n\nText"))))

(ert-deftest test-shipit-html-line-break-starts-a-new-line ()
  "GIVEN <br> tags in a line, in each way that they are written, AND
one in a <p> element
WHEN the HTML tags are converted
THEN the text after each starts a new line."
  (should (equal "one\ntwo\nthree\nfour"
                 (shipit--convert-inline-html "one<br />  two<BR>three<br/>four")))
  (should (equal "one\ntwo"
                 (shipit--convert-inline-html "<p>one<br>\n  two</p>"))))

(ert-deftest test-shipit-html-line-break-takes-the-white-space-around-it ()
  "GIVEN a link around a picture, a <br/> on an indented line of its
own, AND a name, as a cell of a README table has
WHEN the HTML tags are converted
THEN the name stands on the line after the picture with no line of
white space between them, so markdown keeps both in one link, AND two
<br> tags in a row still leave an empty line."
  (should (equal "[<img src=\"cup.png\" />\n**Cup**](https://example.com)"
                 (shipit--convert-inline-html
                  (concat "<a href=\"https://example.com\">\n"
                          "  <img src=\"cup.png\" />\n"
                          "  <br/>\n"
                          "  <b>Cup</b>\n"
                          "</a>"))))
  (should (equal "Hot\n\nCold" (shipit--convert-inline-html "Hot<br><br>Cold")))
  (should (equal "Hot\n\nCold" (shipit--convert-inline-html "Hot <br>\n <br> Cold"))))

(ert-deftest test-shipit-html-block-with-markdown-keeps-its-lines ()
  "GIVEN a <div> element with blank lines in it, which holds markdown
WHEN the HTML tags are converted
THEN the tags are gone, AND the lines of the markdown stay as they are."
  (should (equal "# Title\nline one\nline two\n\nafter"
                 (shipit--convert-inline-html
                  (concat "<div class=\"head\">\n\n"
                          "# Title\nline one\nline two\n\n"
                          "</div>\nafter")))))

(ert-deftest test-shipit-html-block-with-preformatted-text-keeps-its-lines ()
  "GIVEN a <div> element with a <pre> element in it
WHEN the HTML tags are converted
THEN the lines of the preformatted text stay as they are."
  (should (equal "<pre>\na\n  b\n</pre>"
                 (shipit--convert-inline-html
                  "<div>\n<pre>\na\n  b\n</pre>\n</div>"))))

(ert-deftest test-shipit-html-heading-becomes-a-markdown-heading ()
  "GIVEN an <h2> element with an attribute, AND text on the next line
WHEN the HTML tags are converted
THEN it is a markdown heading of the second level, with a blank line
after it."
  (should (equal "## Title\n\ntext"
                 (shipit--convert-inline-html
                  "<h2 id=\"title\">Title</h2>\ntext"))))

(ert-deftest test-shipit-html-tags-without-a-look-go ()
  "GIVEN tags that only group or place their text: <span>, <sub>, AND
<picture> with a <source>
WHEN the HTML tags are converted
THEN the tags are gone, AND their text and the image stay."
  (should (equal "a H2O <img src=\"light.png\">"
                 (shipit--convert-inline-html
                  (concat "<span class=\"x\">a</span> H<sub>2</sub>O "
                          "<picture><source srcset=\"dark.png\">"
                          "<img src=\"light.png\"></picture>")))))

(ert-deftest test-shipit-html-tags-in-code-are-text ()
  "GIVEN HTML tags in a fenced code block, AND in a code span
WHEN the HTML tags are converted
THEN they stay: there they are text that the author shows."
  (dolist (text '("```html\n<p align=\"center\">\n  <strong>x</strong>\n</p>\n```\n"
                  "Write `<br>` for a new line and `<a href=\"x\">y</a>` for a link.\n"))
    (should (equal text (shipit--convert-inline-html text)))))

(ert-deftest test-shipit-html-tags-that-are-not-known-stay ()
  "GIVEN text in angle brackets that is no tag that shipit converts: a
type, a place to fill in, a <details> element
WHEN the HTML tags are converted
THEN the text is the same."
  (let ((text "Vec<T> of <your-name>\n<details><summary>More</summary>\nbody\n</details>\n"))
    (should (equal text (shipit--convert-inline-html text)))))

(ert-deftest test-shipit-html-list-becomes-a-markdown-list ()
  "GIVEN an HTML list with bullets, each item on a line of its own, AND
a numbered list with its items on one line
WHEN the HTML tags are converted
THEN each is a markdown list, one item to a line, with bullets or
numbers."
  (should (equal "- Alpha\n- Beta"
                 (shipit--convert-inline-html
                  "<ul>\n  <li>Alpha</li>\n  <li>Beta</li>\n</ul>")))
  (should (equal "1. One\n2. Two"
                 (shipit--convert-inline-html "<ol><li>One</li><li>Two</li></ol>"))))

(ert-deftest test-shipit-html-list-in-a-list-stands-under-its-item ()
  "GIVEN an HTML list in an item of a list with bullets, AND one in an
item of a numbered list
WHEN the HTML tags are converted
THEN its items stand under that item, indented as far as the text of
the item."
  (should (equal "- Fruit\n  - Apple\n- Tea"
                 (shipit--convert-inline-html
                  "<ul><li>Fruit<ul><li>Apple</li></ul></li><li>Tea</li></ul>")))
  (should (equal "1. Fruit\n   - Apple"
                 (shipit--convert-inline-html
                  "<ol><li>Fruit<ul><li>Apple</li></ul></li></ol>"))))

(ert-deftest test-shipit-html-list-stands-between-paragraphs ()
  "GIVEN an HTML list between two lines of text
WHEN the HTML tags are converted
THEN a blank line stands before and after the list, so that the text
is no part of the list."
  (should (equal "Intro\n\n- A\n\nAfter"
                 (shipit--convert-inline-html "Intro\n<ul><li>A</li></ul>\nAfter"))))

(ert-deftest test-shipit-html-quote-becomes-a-markdown-quote ()
  "GIVEN an HTML quote of two paragraphs, AND text after it
WHEN the HTML tags are converted
THEN each line of the quote starts with the mark of a markdown quote,
the blank line too, AND the text after it is no part of it."
  (should (equal "> Quoted\n>\n> More\n\nAfter"
                 (shipit--convert-inline-html
                  "<blockquote>\n<p>Quoted</p>\n<p>More</p>\n</blockquote>\nAfter"))))

(ert-deftest test-shipit-html-quote-after-a-paragraph-starts-on-its-own-line ()
  "GIVEN a paragraph, AND after it an HTML quote that starts with a
heading, as the release notes in a dependency update do
WHEN the HTML tags are converted
THEN the paragraph has no mark of a quote, AND the quote starts at its
heading, a blank line below the paragraph."
  (should (equal "Intro\n\n> ## Title\n>\n> Text"
                 (shipit--convert-inline-html
                  "<p>Intro</p>\n<blockquote>\n<h2>Title</h2>\n<p>Text</p>\n</blockquote>"))))

(ert-deftest test-shipit-html-code-and-changes-become-markdown ()
  "GIVEN code, deleted text, struck text, inserted text AND underlined
text in HTML tags
WHEN the HTML tags are converted
THEN the code is a code span, the deleted and the struck text are struck
through, AND the tags of the inserted and the underlined text are gone."
  (should (equal "Run `make`, ~~old~~ ~~gone~~ new and fine"
                 (shipit--convert-inline-html
                  (concat "Run <code>make</code>, <del>old</del> <s>gone</s> "
                          "<ins>new</ins> and <u>fine</u>")))))

(ert-deftest test-shipit-html-entities-become-their-characters ()
  "GIVEN HTML entities with a name, with a number, AND an escaped one
WHEN the HTML of the text is converted
THEN each is the character it stands for, AND the escaped one shows
the entity it escapes, as a forge shows them."
  (skip-unless (libxml-available-p))
  (should (equal "a <b> \"c\" & d's — e &lt;"
                 (shipit--convert-inline-html
                  "a &lt;b&gt; &quot;c&quot; &amp; d&#39;s &mdash; e &amp;lt;"))))

(ert-deftest test-shipit-html-entity-of-a-space-is-a-plain-space ()
  "GIVEN a no-break space as an entity, AND an entity that HTML does not
know
WHEN the HTML of the text is converted
THEN the no-break space is a plain space, which Emacs shows without
the face it gives a no-break space, AND the unknown entity stays."
  (skip-unless (libxml-available-p))
  (should (equal "Tea • Coffee &cupcake;"
                 (shipit--convert-inline-html "Tea&nbsp;&bull;&nbsp;Coffee &cupcake;"))))

(ert-deftest test-shipit-html-entities-in-markdown-code-stay ()
  "GIVEN an HTML entity in a code span, AND one in a fenced code block
WHEN the HTML of the text is converted
THEN both stay: in markdown code a forge shows the entity."
  (let ((text "Write `&lt;` here.\n```\n&lt;tag&gt;\n```\n"))
    (should (equal text (shipit--convert-inline-html text)))))

(ert-deftest test-shipit-html-entities-in-a-code-element-become-characters ()
  "GIVEN an HTML <code> element with entities, as the release notes in
a dependency update have
WHEN the HTML of the text is converted
THEN the code span shows the characters: the text of the element is
HTML, not markdown."
  (skip-unless (libxml-available-p))
  (should (equal "Add `#[command(defer = <bool>)]` and `\"x\".f()`"
                 (shipit--convert-inline-html
                  (concat "Add <code>#[command(defer = &lt;bool&gt;)]</code> "
                          "and <code>&quot;x&quot;.f()</code>")))))

(ert-deftest test-shipit-html-link-to-a-file-of-the-repo-gets-its-address ()
  "GIVEN markdown of the repository owner/repo with an <a> tag to a
file of that repository
WHEN the markdown is made ready for markdown-mode
THEN the link has the whole address of the file, which makes it a link
that can be followed."
  (let ((shipit--image-base-repo "owner/repo"))
    (should (equal "[Docs](https://github.com/owner/repo/blob/HEAD/docs/a.md)"
                   (shipit--prepare-markdown "<a href=\"docs/a.md\">Docs</a>")))))

(ert-deftest test-shipit-html-table-with-markdown-has-its-cells-between-markers ()
  "GIVEN an HTML table with two cells, each with a markdown heading and
a quote between blank lines, as a README has to show two texts side by
side
WHEN the HTML tags are converted
THEN no tag of the table is left, AND a marker of a table row stands
where the row started, AND the markdown of each cell stands between the
two markers of a cell, AND each marker is a paragraph of its own."
  (should (equal (concat "## What changes\n\n"
                         "<shipit-row>\n\n"
                         "<shipit-cell>\n\n## Before\n\n> long\n\n</shipit-cell>\n\n"
                         "<shipit-cell>\n\n## After\n\n> short\n\n</shipit-cell>\n\n"
                         "## The rules\n")
                 (shipit--convert-inline-html
                  (concat "## What changes\n\n\n"
                          "<table>\n<tr>\n<td width=\"50%\">\n\n"
                          "## Before\n\n> long\n\n"
                          "</td>\n\n<td width=\"50%\">\n\n"
                          "## After\n\n> short\n\n"
                          "</td>\n</tr>\n</table>\n\n\n"
                          "## The rules\n")))))

(ert-deftest test-shipit-html-table-with-a-picture-or-code-has-its-cells-marked ()
  "GIVEN an HTML table with markdown in its two cells, the first of
which holds a picture, written as markdown or as a tag, or a fenced
code block
WHEN the markdown is made ready for markdown-mode
THEN the markdown of each cell is marked as a cell of the same row, as
in a table of text, so that the two can stand side by side."
  (dolist (cell '("![shot](shot.png)" "<img src=\"shot.png\">" "```sh\nmake\n```"))
    (should (equal `((1 0 ,cell) (1 1 "text"))
                   (test-shipit-render--table-cells
                    (shipit--prepare-markdown
                     (concat "<table>\n<tr>\n<td>\n\n" cell "\n\n</td>\n"
                             "<td>\n\ntext\n\n</td>\n</tr>\n</table>\n")))))))

(ert-deftest test-shipit-html-table-cell-alignment-goes-in-its-marker ()
  "GIVEN an HTML table of two rows with markdown: the first with valign
middle and three cells, one with valign top, one with a style of
vertical-align bottom, AND one with no alignment, AND the second with
valign top and a cell with no alignment
WHEN the HTML tags are converted
THEN the marker of each cell names the alignment of the cell, or else
that of its row, AND a cell in the middle, where HTML puts it when
nothing is said, gets the plain marker."
  (should (equal (concat "Menu\n\n"
                         "<shipit-row>\n\n"
                         "<shipit-cell top>\n\na\n\n</shipit-cell>\n\n"
                         "<shipit-cell bottom>\n\nb\n\n</shipit-cell>\n\n"
                         "<shipit-cell>\n\nc\n\n</shipit-cell>\n\n"
                         "<shipit-row>\n\n"
                         "<shipit-cell top>\n\nd\n\n</shipit-cell>\n\n"
                         "After\n")
                 (shipit--convert-inline-html
                  (concat "Menu\n\n<table>\n<tr valign=\"middle\">\n"
                          "<td valign=\"top\">\n\na\n\n</td>\n"
                          "<td style=\"padding: 4px; vertical-align: bottom\">\n\nb\n\n</td>\n"
                          "<td>\n\nc\n\n</td>\n</tr>\n"
                          "<tr valign=\"top\">\n<td>\n\nd\n\n</td>\n</tr>\n</table>\n\n"
                          "After\n")))))

(ert-deftest test-shipit-html-table-of-plain-rows-has-its-cells-between-markers ()
  "GIVEN an HTML table with no blank line in it, which holds a row of
heads and a row of data
WHEN the HTML tags are converted
THEN no tag of the table is left, AND each row and each cell stands
between markers, as in a table with markdown in its cells."
  (should (equal (concat "Menu\n\n"
                         "<shipit-row>\n\n"
                         "<shipit-cell>\n\nDrink\n\n</shipit-cell>\n\n"
                         "<shipit-cell>\n\nSize\n\n</shipit-cell>\n\n"
                         "<shipit-row>\n\n"
                         "<shipit-cell>\n\nLatte\n\n</shipit-cell>\n\n"
                         "<shipit-cell>\n\nLarge\n\n</shipit-cell>\n\n"
                         "After\n")
                 (shipit--convert-inline-html
                  (concat "Menu\n\n<table>\n"
                          "<tr><th>Drink</th><th>Size</th></tr>\n"
                          "<tr><td>Latte</td><td>Large</td></tr>\n"
                          "</table>\n\nAfter\n")))))

(ert-deftest test-shipit-html-table-in-code-stays ()
  "GIVEN an HTML table in a fenced code block
WHEN the HTML tags are converted
THEN the table is the same."
  (let ((text "```html\n<table>\n\n<tr><td>x</td></tr>\n\n</table>\n```\n"))
    (should (equal text (shipit--convert-inline-html text)))))

;;; Centered text

(defun test-shipit-render--centered-text (text)
  "The parts of the string TEXT that are marked as centered, each a string."
  (let ((runs nil) (pos 0))
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'shipit-align text)
                      (length text))))
        (when (eq 'center (get-text-property pos 'shipit-align text))
          (push (substring-no-properties text pos next) runs))
        (setq pos next)))
    (nreverse runs)))

(ert-deftest test-shipit-html-centered-block-stands-between-markers ()
  "GIVEN a <p> element that asks for centered text, AND a <center>
element after a line of text
WHEN the HTML tags are converted
THEN the text of each stands between the two markers of centered
text, AND each marker is a paragraph of its own."
  (should (equal "<shipit-center>\n\n**Tagline**\n\n</shipit-center>\n\ntext"
                 (shipit--convert-inline-html
                  "<p align=\"center\">\n  <strong>Tagline</strong>\n</p>\ntext")))
  (should (equal "before\n\n<shipit-center>\n\nmiddle\n\n</shipit-center>"
                 (shipit--convert-inline-html "before\n<CENTER>middle</CENTER>"))))

(ert-deftest test-shipit-html-block-with-another-alignment-has-no-marker ()
  "GIVEN a <p> element that asks for text at the left
WHEN the HTML tags are converted
THEN no marker of centered text is put."
  (should (equal "text"
                 (shipit--convert-inline-html "<p align=\"left\">text</p>"))))

(ert-deftest test-shipit-alignment-markers-become-a-mark-on-the-text ()
  "GIVEN markdown with two lines between the markers of centered text
WHEN the markers are applied
THEN the markers are gone, with one blank line where each stood, AND
the two lines are marked as centered, AND no other text is."
  (let ((text (shipit--apply-layout-markers
               (concat "before\n\n<shipit-center>\n\n**Tagline**\nsecond\n\n"
                       "</shipit-center>\n\nafter"))))
    (should (equal "before\n\n**Tagline**\nsecond\n\nafter" text))
    (should (equal '("**Tagline**\nsecond")
                   (test-shipit-render--centered-text text)))))

(ert-deftest test-shipit-alignment-markers-inside-each-other-mark-all-the-text ()
  "GIVEN a pair of markers of centered text inside another pair
WHEN the markers are applied
THEN no marker is left, AND all the text of the outer pair is marked."
  (let ((text (shipit--apply-layout-markers
               (concat "<shipit-center>\n\n<shipit-center>\n\nx\n\n"
                       "</shipit-center>\n\ny\n\n</shipit-center>"))))
    (should (equal "x\n\ny" text))
    (should (equal '("x\n\ny") (test-shipit-render--centered-text text)))))

(ert-deftest test-shipit-alignment-marker-without-its-end-marks-nothing ()
  "GIVEN a marker that starts centered text, AND no marker that ends it
WHEN the markers are applied
THEN the marker is gone, AND no text is marked."
  (let ((text (shipit--apply-layout-markers "<shipit-center>\n\nx\n")))
    (should (equal "x\n" text))
    (should-not (test-shipit-render--centered-text text))))

(ert-deftest test-shipit-centered-html-is-marked-in-the-markdown-to-fontify ()
  "GIVEN markdown with two centered <p> elements and a heading after
them, as the head of a README has
WHEN the markdown is made ready for markdown-mode
THEN no tag and no marker is left, AND the text of the two elements is
marked as centered, AND the heading is not."
  (let ((text (shipit--prepare-markdown
               (concat "<p align=\"center\">\n"
                       "  <img src=\"./logo.png\" width=\"140\" />\n"
                       "</p>\n"
                       "<p align=\"center\">\n"
                       "  <strong>Tagline</strong>\n"
                       "</p>\n\n"
                       "## Install\n"))))
    (should (equal "<img src=\"./logo.png\" width=\"140\" />\n\n**Tagline**\n\n## Install\n"
                   text))
    (should (equal '("<img src=\"./logo.png\" width=\"140\" />" "**Tagline**")
                   (test-shipit-render--centered-text text)))))

(ert-deftest test-shipit-centered-html-stays-marked-next-to-a-markdown-table ()
  "GIVEN markdown with a centered <p> element and a markdown table,
which pandoc aligns by writing the whole text again
WHEN the markdown is made ready for markdown-mode
THEN the text of the element is still marked as centered."
  (skip-unless (executable-find "pandoc"))
  (let ((text (shipit--prepare-markdown
               "<p align=\"center\">Tagline</p>\n\n| a | b |\n|---|---|\n| 1 | 2 |\n")))
    (should (equal '("Tagline") (test-shipit-render--centered-text text)))
    (should-not (string-match-p "shipit-center" text))))

(ert-deftest test-shipit-a-centered-line-starts-at-half-of-the-room-left ()
  "GIVEN a buffer with an indented line of 4 characters that is marked
as centered, AND a line that is not marked
WHEN the lines are centered in a column that is 20 wide
THEN the marked line is drawn 8 further in, half of the room that its
text leaves, AND the other line is drawn where it was."
  (with-temp-buffer
    (insert "   " (propertize "abcd" 'shipit-align 'center) "\n   plain\n")
    (shipit--center-aligned-lines-in-region (point-min) (point-max) 20)
    (should (equal '(space :width (8))
                   (get-text-property (point-min) 'line-prefix)))
    (forward-line -1)
    (should-not (get-text-property (point) 'line-prefix))))

(ert-deftest test-shipit-a-centered-line-is-measured-as-it-is-shown ()
  "GIVEN a centered line with 2 characters to see and hidden markup
around them, AND a centered line whose first character is a space that
shows a picture 3 wide
WHEN the lines are centered in a column that is 20 wide
THEN the first is drawn 9 further in, AND the second 8: hidden text
takes no room, AND the space of a picture is no indent."
  (with-temp-buffer
    (insert "   "
            (propertize "[" 'invisible t 'shipit-align 'center)
            (propertize "ab" 'shipit-align 'center)
            (propertize "](https://example.com/a)" 'invisible t 'shipit-align 'center)
            "\n   "
            (propertize " " 'display "PIC" 'shipit-align 'center)
            "\n")
    (shipit--center-aligned-lines-in-region (point-min) (point-max) 20)
    (goto-char (point-min))
    (should (equal '(space :width (9)) (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (equal '(space :width (8)) (get-text-property (point) 'line-prefix)))))

(ert-deftest test-shipit-a-centered-line-that-fills-the-column-stays ()
  "GIVEN a centered line that is 30 wide
WHEN the lines are centered in a column that is 20 wide
THEN the line is drawn where it was."
  (with-temp-buffer
    (insert (propertize (make-string 30 ?x) 'shipit-align 'center) "\n")
    (shipit--center-aligned-lines-in-region (point-min) (point-max) 20)
    (should-not (get-text-property (point-min) 'line-prefix))))

;;; Table cells side by side

(defun test-shipit-render--table-cells (text)
  "The table cells that are marked in the string TEXT, each as (ROW PLACE TEXT)."
  (let ((cells nil) (pos 0))
    (while (< pos (length text))
      (let ((mark (get-text-property pos 'shipit-cell text))
            (next (or (next-single-property-change pos 'shipit-cell text)
                      (length text))))
        (when mark
          (push (list (car mark) (cdr mark) (substring-no-properties text pos next))
                cells))
        (setq pos next)))
    (nreverse cells)))

(defun test-shipit-render--cell (row place text)
  "TEXT marked as the cell at PLACE of the table row ROW."
  (propertize text 'shipit-cell (cons row place)))

(defun test-shipit-render--row-lines (text)
  "The lines of the string TEXT that are lines of a table row."
  (seq-filter (lambda (line)
                (and (not (string-empty-p line))
                     (get-text-property 0 'shipit-no-wrap line)))
              (split-string text "\n")))

(ert-deftest test-shipit-cell-markers-become-a-mark-on-each-cell ()
  "GIVEN markdown with the markers of two table rows, the first of two
cells and the second of one
WHEN the markers are applied
THEN the markers are gone, with one blank line where each stood, AND
the text of each cell is marked with its row and its place in the row."
  (let ((text (shipit--apply-layout-markers
               (concat "<shipit-row>\n\n"
                       "<shipit-cell>\n\na\n\n</shipit-cell>\n\n"
                       "<shipit-cell>\n\nb\nc\n\n</shipit-cell>\n\n"
                       "<shipit-row>\n\n"
                       "<shipit-cell>\n\nd\n\n</shipit-cell>\n\nafter"))))
    (should (equal "a\n\nb\nc\n\nd\n\nafter" text))
    (should (equal '((1 0 "a") (1 1 "b\nc") (2 0 "d"))
                   (test-shipit-render--table-cells text)))))

(ert-deftest test-shipit-a-cell-marker-with-an-alignment-marks-the-cell-with-it ()
  "GIVEN markdown with a table row of a cell whose marker names top, AND
a cell whose marker names no alignment
WHEN the markers are applied
THEN both are marked as cells of the row, AND the text of the first is
marked as standing at the top, AND the second has no such mark."
  (let ((text (shipit--apply-layout-markers
               (concat "<shipit-row>\n\n"
                       "<shipit-cell top>\n\na\n\n</shipit-cell>\n\n"
                       "<shipit-cell>\n\nb\n\n</shipit-cell>"))))
    (should (equal '((1 0 "a") (1 1 "b")) (test-shipit-render--table-cells text)))
    (should (eq 'top (get-text-property 0 'shipit-valign text)))
    (should-not (get-text-property (string-search "b" text) 'shipit-valign text))))

(ert-deftest test-shipit-html-table-cells-are-marked-in-the-markdown-to-fontify ()
  "GIVEN markdown with an HTML table of two cells that hold markdown
WHEN the markdown is made ready for markdown-mode
THEN no tag and no marker is left, AND the markdown of each cell is
marked as a cell of the same row."
  (let ((text (shipit--prepare-markdown
               (concat "<table>\n<tr>\n<td width=\"50%\">\n\n"
                       "## Before\n\n> long\n\n"
                       "</td>\n\n<td width=\"50%\">\n\n"
                       "## After\n\n> short\n\n"
                       "</td>\n</tr>\n</table>\n\n\n"
                       "## The rules\n"))))
    (should (equal "## Before\n\n> long\n\n## After\n\n> short\n\n## The rules\n" text))
    (should (equal '((1 0 "## Before\n\n> long") (1 1 "## After\n\n> short"))
                   (test-shipit-render--table-cells text)))))

(ert-deftest test-shipit-the-columns-of-a-table-are-as-wide-as-their-text ()
  "GIVEN a table of two rows of two marked cells, with only white space
between the rows: prev and slides back, AND slideTo and slides to the
index
WHEN the table rows are laid out in 43 columns
THEN the first column is 7 wide in both rows, as its widest text is,
so the bar of each row is marked to start at column 8, AND a rule
parts the two rows, crossing the bar where it stands, in place of the
blank line between them."
  (let* ((text (concat (test-shipit-render--cell 1 0 "prev") "\n\n"
                       (test-shipit-render--cell 1 1 "slides back") "\n\n"
                       (test-shipit-render--cell 2 0 "slideTo") "\n\n"
                       (test-shipit-render--cell 2 1 "slides to the index")))
         (result (shipit--lay-out-table-rows text 43)))
    (should (equal (concat "prev │ slides back\n"
                           (make-string 8 ?─) "┼" (make-string 20 ?─) "\n"
                           "slideTo │ slides to the index")
                   (substring-no-properties result)))
    (should (equal 8 (get-text-property (string-search " │ slides back" result)
                                        'shipit-column result)))
    (should (equal 8 (get-text-property (string-search " │ slides to" result)
                                        'shipit-column result)))
    (should (get-text-property (string-search "┼" result) 'shipit-no-wrap result))))

(ert-deftest test-shipit-the-cells-of-a-table-row-stand-side-by-side ()
  "GIVEN a text with a table row of two marked cells between two
paragraphs: the first cell has one line AND the second has two
WHEN the table rows are laid out in 43 columns
THEN the row has two lines, each with the line of the first cell, a
bar, and the line of the second, AND the place where the bar must
start, column 4 after the first cell of 3, is marked on the space
before it, AND the paragraphs around the row are as they were."
  (let* ((text (concat "above\n\n"
                       (test-shipit-render--cell 1 0 "one") "\n\n"
                       (test-shipit-render--cell 1 1 "two\nthree")
                       "\n\nbelow"))
         (result (shipit--lay-out-table-rows text 43)))
    (should (equal "above\n\none │ two\n │ three\n\nbelow"
                   (substring-no-properties result)))
    (should (equal '("one │ two" " │ three")
                   (mapcar #'substring-no-properties
                           (test-shipit-render--row-lines result))))
    (should (equal 4 (get-text-property (string-search " │ two" result)
                                        'shipit-column result)))
    (should (equal 4 (get-text-property (string-search " │ three" result)
                                        'shipit-column result)))))

(ert-deftest test-shipit-a-table-cell-is-wrapped-to-its-column ()
  "GIVEN a table row of two marked cells, the first of which is one
line of 59 characters, AND the second a word of 5
WHEN the table rows are laid out in 43 columns
THEN the word keeps its 5, AND the first cell gets the 35 that are
left, so its text takes more than one line and no line of it is wider
than 35, AND the text of the second cell stands beside it."
  (let* ((text (concat (test-shipit-render--cell
                        1 0 "one two three four five six seven eight nine ten eleven one")
                       "\n\n"
                       (test-shipit-render--cell 1 1 "right")))
         (lines (mapcar #'substring-no-properties
                        (test-shipit-render--row-lines
                         (shipit--lay-out-table-rows text 43)))))
    (should (> (length lines) 1))
    (should (seq-some (lambda (line) (string-suffix-p " │ right" line)) lines))
    (should (> (apply #'max (mapcar (lambda (line) (string-search " │ " line)) lines))
               20))
    (dolist (line lines)
      (should (<= (string-search " │ " line) 35)))))

(ert-deftest test-shipit-a-short-table-cell-stands-at-its-alignment ()
  "GIVEN table rows of a cell of four lines AND a cell of one line, which
is marked as standing at the top, at the bottom, or not marked
WHEN the table rows are laid out in 43 columns
THEN the short cell stands on the first line at the top, on the last at
the bottom, AND on the second when it is not marked: in the middle, as
HTML has it, and on the upper line when the room above and below
cannot be the same."
  (dolist (case '((top . 0) (bottom . 3) (nil . 1)))
    (let* ((text (concat (test-shipit-render--cell 1 0 "a\nb\nc\nd") "\n\n"
                         (propertize (test-shipit-render--cell 1 1 "tea")
                                     'shipit-valign (car case))))
           (lines (mapcar #'substring-no-properties
                          (test-shipit-render--row-lines
                           (shipit--lay-out-table-rows text 43)))))
      (should (= 4 (length lines)))
      (should (equal (cdr case)
                     (seq-position lines "tea" (lambda (line tea)
                                                 (string-suffix-p tea line))))))))

(ert-deftest test-shipit-a-heading-in-a-table-cell-loses-its-hidden-hashes ()
  "GIVEN a table row of two marked cells that each start with a heading
whose hashes are hidden
WHEN the table rows are laid out
THEN the first line of the row has the two titles and no hash: a line
that starts with hashes is taken for a heading of the document."
  (let* ((heading (lambda (title)
                    (concat (propertize "## " 'invisible t) title "\n\ntext")))
         (text (concat (test-shipit-render--cell 1 0 (funcall heading "Before"))
                       "\n\n"
                       (test-shipit-render--cell 1 1 (funcall heading "After"))))
         (lines (test-shipit-render--row-lines
                 (shipit--lay-out-table-rows text 43))))
    (should (equal "Before │ After" (substring-no-properties (car lines))))))

(ert-deftest test-shipit-text-in-a-table-cell-is-not-marked-as-centered ()
  "GIVEN a table row of two marked cells, the first of which is marked
as centered text
WHEN the table rows are laid out
THEN no text of the row is marked as centered: a centered line gets its
place from the start of the line, and that moves both cells."
  (let* ((text (concat (propertize (test-shipit-render--cell 1 0 "one")
                                   'shipit-align 'center)
                       "\n\n"
                       (test-shipit-render--cell 1 1 "two")))
         (result (shipit--lay-out-table-rows text 43)))
    (should (equal "one │ two" (substring-no-properties result)))
    (should-not (test-shipit-render--centered-text result))))

(ert-deftest test-shipit-a-table-row-that-does-not-fit-stays ()
  "GIVEN a table of one row of one marked cell, AND, after a paragraph,
a table of one row of three cells of 30 characters each
WHEN the table rows are laid out in 43 columns, where each of the
three cells would get fewer than the 20 columns that a cell needs
THEN the text is as it was."
  (let* ((long (make-string 30 ?x))
         (text (concat (test-shipit-render--cell 1 0 "alone") "\n\nbetween\n\n"
                       (test-shipit-render--cell 2 0 long) "\n\n"
                       (test-shipit-render--cell 2 1 long) "\n\n"
                       (test-shipit-render--cell 2 2 long))))
    (should (equal (substring-no-properties text)
                   (substring-no-properties (shipit--lay-out-table-rows text 43))))))

(defun test-shipit-render--char-faces (line)
  "The face of each character of the string LINE, as a list."
  (mapcar (lambda (i) (get-text-property i 'face line))
          (number-sequence 0 (1- (length line)))))

(defun test-shipit-render--background-p (face)
  "Whether FACE, a face property, gives a background."
  (seq-some (lambda (part) (and (consp part) (plist-get part :background)))
            (if (and (consp face) (keywordp (car face))) (list face) (ensure-list face))))

(ert-deftest test-shipit-a-code-block-in-a-table-cell-stands-in-its-column ()
  "GIVEN a table row of two marked cells: a fenced code block whose
widest line is 20, AND a text
WHEN the table rows are laid out in 50 columns
THEN the code column is 22 wide, the code with a space on each side,
AND the text gets the rest, AND the fences are gone, AND each character
of the code column has the code background and is marked as code of a
table, so that no code block of the whole text is drawn over it."
  (let* ((shipit-code-block-background "#333333")
         (text (concat (test-shipit-render--cell
                        1 0 "```sh\nmake tea\nmake coffee --strong\n```")
                       "\n\n"
                       (test-shipit-render--cell 1 1 "Brew it")))
         (lines (test-shipit-render--row-lines (shipit--lay-out-table-rows text 50))))
    (should (equal (list (format "%-22s │ Brew it" " make tea")
                         " make coffee --strong  │ ")
                   (mapcar #'substring-no-properties lines)))
    (should (equal 23 (get-text-property 22 'shipit-column (car lines))))
    (dolist (line lines)
      (should (cl-every #'test-shipit-render--background-p
                        (test-shipit-render--char-faces (substring line 0 22))))
      (should (eq t (get-text-property 0 'shipit-table-code line)))
      (should-not (text-property-not-all 0 22 'shipit-table-code t line)))))

(ert-deftest test-shipit-a-code-block-too-wide-for-its-row-stays ()
  "GIVEN a table row of two marked cells: a fenced code block whose
widest line is 40, AND a text
WHEN the table rows are laid out in 50 columns, where the text would
get fewer than the 20 columns that a cell needs
THEN the text is as it was, with the fences of the code."
  (let ((text (concat (test-shipit-render--cell
                       1 0 (concat "```sh\n" (make-string 40 ?x) "\n```"))
                      "\n\n"
                      (test-shipit-render--cell 1 1 "Brew it"))))
    (should (equal (substring-no-properties text)
                   (substring-no-properties (shipit--lay-out-table-rows text 50))))))

(defmacro test-shipit-render--with-picture-display (&rest body)
  "Run BODY where a picture is 400 by 100 pixels, or as wide as its
`:max-width' with the same shape, a character is 8 pixels wide, AND a
line is 18 pixels high.  Batch Emacs shows no picture."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'image-size)
              (lambda (image &rest _)
                (let ((width (min 400 (or (plist-get (cdr image) :max-width) 400))))
                  (cons width (/ (* width 100) 400)))))
             ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
             ((symbol-function 'frame-char-height) (lambda (&rest _) 18))
             ((symbol-function 'default-line-height) (lambda () 18)))
     ,@body))

(ert-deftest test-shipit-a-picture-in-a-table-cell-is-cut-into-lines ()
  "GIVEN a table row of two marked cells: a text of 7 characters, AND a
picture of 400 by 100 pixels on a line of its own
WHEN the table rows are laid out in 43 columns, where the text keeps
its 7 and the picture gets the 33 left, 264 pixels where a character
is 8
THEN the picture is made 264 pixels wide and so 66 high, AND it stands
in its column as 4 strips, one on each line of the row, each 18 pixels
high, so that the text can stand beside it, in the middle of the row,
AND each strip is centered
on its line as the text is, so that it makes its line no higher and no
gap shows between two strips, AND the last strip, 12 high, is raised
by the 3 pixels that would else be above it, to the top of its line."
  (test-shipit-render--with-picture-display
    (let* ((cup '(image :type png :file "cup.png"))
           (text (concat (test-shipit-render--cell 1 0 "Brew it")
                         "\n\n"
                         (test-shipit-render--cell 1 1 (propertize " " 'display cup))))
           (lines (test-shipit-render--row-lines (shipit--lay-out-table-rows text 43)))
           (fitted (append cup '(:max-width 264 :ascent center))))
      (should (equal '(" │  " "Brew it │  " " │  " " │  ")
                     (mapcar #'substring-no-properties lines)))
      (should (equal (append (mapcar (lambda (top) `((slice 0 ,top 264 18) ,fitted))
                                     '(0 18 36))
                             `(((raise ,(/ 3.0 18)) (slice 0 54 264 12) ,fitted)))
                     (mapcar (lambda (line)
                               (get-text-property (1- (length line)) 'display line))
                             lines))))))

(ert-deftest test-shipit-a-picture-keeps-the-cell-it-stands-in ()
  "GIVEN an <img> tag, a markdown picture, AND a linked markdown picture,
each marked as the second cell of a table row
WHEN the pictures of the text are processed
THEN what stands in place of each is marked as that cell, AND as
standing at the top of it, so that the row still has two cells, and
the cell its alignment."
  (let ((shipit-render-images t))
    (dolist (input '("<img src=\"https://example.com/cup.png\" alt=\"cup\">"
                     "![cup](https://example.com/cup.png)"
                     "[![cup](https://example.com/cup.png)](https://example.com)"))
      (let ((result (shipit-test-http-answering '(("\\`https://example\\.com/" 404 ""))
                      (shipit--process-inline-images
                       (propertize (test-shipit-render--cell 1 1 input)
                                   'shipit-valign 'top)))))
        (should (string-match-p "\\[Image: cup" result))
        (should (equal '(1 . 1) (get-text-property 0 'shipit-cell result)))
        (should (eq 'top (get-text-property 0 'shipit-valign result)))))))

(defmacro test-shipit-render--with-moving-pictures (frames shown &rest body)
  "Run BODY in a buffer for moving pictures whose frames FRAMES tells.
FRAMES is the number of frames and the seconds of each, as
`image-multi-frame-p' gives them.  When SHOWN, the selected window
shows the buffer, all of it, as in batch Emacs; else no window does.
The frames show pictures, which the frame of batch Emacs does not."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'image-multi-frame-p) (lambda (&rest _) ,frames))
             ((symbol-function 'display-images-p) (lambda (&rest _) t)))
     (with-temp-buffer
       (when ,shown
         (set-window-buffer (selected-window) (current-buffer)))
       (unwind-protect
           (progn ,@body)
         (when shipit--picture-clock
           (cancel-timer shipit--picture-clock))))))

(defun test-shipit-render--insert-picture (image &rest properties)
  "Insert a line that shows IMAGE, with the text PROPERTIES too."
  (insert (apply #'propertize " " 'display image properties) "\n"))

(defun test-shipit-render--frame (image)
  "The frame that IMAGE shows: nil before its first move."
  (plist-get (cdr image) :index))

(ert-deftest test-shipit-a-moving-picture-keeps-its-speed-under-the-frame-cap ()
  "GIVEN a moving picture in view whose frames last 0.03 seconds
WHEN the clock moves the pictures, and again 0.1 seconds later
THEN the picture is three frames on: it skips the frames that the cap
on the frame rate leaves out, so it keeps its speed."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (let ((cup (list 'image :type 'gif :file "cup.gif")))
      (test-shipit-render--insert-picture cup)
      (shipit--animate-images-in-region (point-min) (point-max))
      (shipit--move-pictures-at 100.0)
      (shipit--move-pictures-at 100.1)
      (should (= 3 (test-shipit-render--frame cup))))))

(ert-deftest test-shipit-the-clock-comes-back-at-most-at-the-frame-rate ()
  "GIVEN a moving picture in view whose frames last 0.03 seconds
WHEN the clock moves the pictures, with a cap of ten frames a second,
and with no cap
THEN the clock comes back 0.1 seconds later with the cap, and when the
next frame is due without it."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (test-shipit-render--insert-picture (list 'image :type 'gif :file "cup.gif"))
    (shipit--animate-images-in-region (point-min) (point-max))
    (let ((shipit-image-max-frame-rate 10))
      (should (= 0.1 (shipit--move-pictures-at 100.0))))
    (let ((shipit-image-max-frame-rate nil))
      (should (< (abs (- 0.025 (shipit--move-pictures-at 100.035))) 1e-6)))))

(ert-deftest test-shipit-a-slow-picture-brings-the-clock-back-when-it-is-due ()
  "GIVEN a moving picture in view whose frames last 0.17 seconds, and a
cap of ten frames a second
WHEN the clock moves the pictures
THEN the clock comes back when the next frame is due, 0.17 seconds
later, and not each 0.1 seconds for nothing."
  (test-shipit-render--with-moving-pictures '(8 . 0.17) t
    (test-shipit-render--insert-picture (list 'image :type 'gif :file "cup.gif"))
    (shipit--animate-images-in-region (point-min) (point-max))
    (let ((shipit-image-max-frame-rate 10))
      (should (< (abs (- 0.17 (shipit--move-pictures-at 100.0))) 1e-6)))))

(ert-deftest test-shipit-moving-pictures-in-view-redraw-the-window-once ()
  "GIVEN two moving pictures in view, each with a frame that is due
WHEN the clock moves the pictures
THEN both move, and the window is drawn again once, not once for each."
  (let ((redraws 0))
    (cl-letf (((symbol-function 'force-window-update)
               (lambda (&rest _) (setq redraws (1+ redraws)))))
      (test-shipit-render--with-moving-pictures '(8 . 0.03) t
        (let ((cup (list 'image :type 'gif :file "cup.gif"))
              (tea (list 'image :type 'gif :file "tea.gif")))
          (test-shipit-render--insert-picture cup)
          (test-shipit-render--insert-picture tea)
          (shipit--animate-images-in-region (point-min) (point-max))
          (shipit--move-pictures-at 100.0)
          (shipit--move-pictures-at 100.04)
          (should (= 1 (test-shipit-render--frame cup)))
          (should (= 1 (test-shipit-render--frame tea)))
          (should (= 1 redraws)))))))

(ert-deftest test-shipit-the-strips-of-a-moving-picture-move-once ()
  "GIVEN a buffer in view with three strips of one moving picture
WHEN the clock moves the pictures when one frame is due
THEN that picture is one frame on, not three."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (let ((cup (list 'image :type 'gif :file "cup.gif")))
      (dolist (top '(0 18 36))
        (test-shipit-render--insert-picture `((slice 0 ,top 160 18) ,cup)))
      (shipit--animate-images-in-region (point-min) (point-max))
      (shipit--move-pictures-at 100.0)
      (shipit--move-pictures-at 100.04)
      (should (= 1 (test-shipit-render--frame cup))))))

(ert-deftest test-shipit-a-moving-picture-out-of-view-holds-still ()
  "GIVEN a moving picture in a buffer that no window shows
WHEN the clock moves the pictures, also after its frame is due
THEN the picture holds its frame, AND the clock does not come back:
the picture costs nothing until the buffer is in view again."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) nil
    (let ((cup (list 'image :type 'gif :file "cup.gif")))
      (test-shipit-render--insert-picture cup)
      (shipit--animate-images-in-region (point-min) (point-max))
      (should-not (shipit--move-pictures-at 100.0))
      (should-not (shipit--move-pictures-at 100.1))
      (should-not (test-shipit-render--frame cup)))))

(ert-deftest test-shipit-a-moving-picture-in-a-terminal-window-holds-still ()
  "GIVEN a moving picture that only a window of a terminal frame shows,
as the first frame of an Emacs daemon can
WHEN the clock moves the pictures, also after its frame is due
THEN the picture holds its frame: a terminal shows no pictures."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (let ((cup (list 'image :type 'gif :file "cup.gif")))
      (test-shipit-render--insert-picture cup)
      (shipit--animate-images-in-region (point-min) (point-max))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
        (shipit--move-pictures-at 100.0)
        (shipit--move-pictures-at 100.1))
      (should-not (test-shipit-render--frame cup)))))

(ert-deftest test-shipit-pictures-render-where-no-frame-shows-pictures ()
  "GIVEN a buffer that shows a picture, and no frame that shows pictures,
as in batch Emacs or in a daemon that has only its terminal frame
WHEN the pictures of the buffer are set moving
THEN no error stops the render, AND no picture moves: only a frame
that shows pictures can tell whether a picture has frames."
  (with-temp-buffer
    ;; PBM is built into every Emacs, so the test needs no image library.
    (test-shipit-render--insert-picture (create-image "P1\n1 1\n1\n" 'pbm t))
    (shipit--animate-images-in-region (point-min) (point-max))
    (should-not shipit--moving-pictures)
    (should-not shipit--picture-clock)))

(ert-deftest test-shipit-a-folded-moving-picture-holds-still ()
  "GIVEN a moving picture in view, in text that a folded section hides
WHEN the clock moves the pictures, also after its frame is due
THEN the picture holds its frame."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (let ((cup (list 'image :type 'gif :file "cup.gif")))
      (test-shipit-render--insert-picture cup 'invisible t)
      (shipit--animate-images-in-region (point-min) (point-max))
      (shipit--move-pictures-at 100.0)
      (shipit--move-pictures-at 100.1)
      (should-not (test-shipit-render--frame cup)))))

(ert-deftest test-shipit-the-clock-stops-out-of-view-and-starts-at-a-command ()
  "GIVEN moving pictures in a buffer that no window shows, just set moving
WHEN the clock runs, AND then a command ends in the buffer
THEN the clock stops when it runs, AND it starts again after the
command, which may have brought the pictures into view."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) nil
    (test-shipit-render--insert-picture (list 'image :type 'gif :file "cup.gif"))
    (shipit--animate-images-in-region (point-min) (point-max))
    (should (timerp shipit--picture-clock))
    (timer-event-handler shipit--picture-clock)
    (should-not shipit--picture-clock)
    (run-hooks 'post-command-hook)
    (should (timerp shipit--picture-clock))))

(ert-deftest test-shipit-the-clock-starts-when-a-window-scrolls-without-a-command ()
  "GIVEN moving pictures in a buffer that the selected window shows, and
a clock that stopped
WHEN the window scrolls with no command in the buffer, as a wheel over
a window that is not selected does, and redisplay runs the scroll hooks
THEN the clock starts again, as the scroll may have brought the
pictures into view."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (test-shipit-render--insert-picture (list 'image :type 'gif :file "cup.gif"))
    (shipit--animate-images-in-region (point-min) (point-max))
    (cancel-timer shipit--picture-clock)
    (setq shipit--picture-clock nil)
    (run-hook-with-args 'window-scroll-functions (selected-window) (point-min))
    (should (timerp shipit--picture-clock))))

(ert-deftest test-shipit-a-picture-that-a-redraw-left-behind-holds-still ()
  "GIVEN a buffer in view with a moving picture, set moving, that is
drawn again as a new copy, which is set moving
WHEN the clock moves the pictures after a frame is due
THEN the new copy moves, AND the old copy holds still: the buffer shows
it no more."
  (test-shipit-render--with-moving-pictures '(8 . 0.03) t
    (let ((old (list 'image :type 'gif :file "cup.gif"))
          (new (list 'image :type 'gif :file "cup.gif")))
      (test-shipit-render--insert-picture old)
      (shipit--animate-images-in-region (point-min) (point-max))
      (shipit--move-pictures-at 100.0)
      (erase-buffer)
      (test-shipit-render--insert-picture new)
      (shipit--animate-images-in-region (point-min) (point-max))
      (shipit--move-pictures-at 100.5)
      (shipit--move-pictures-at 100.54)
      (should (= 1 (test-shipit-render--frame new)))
      (should-not (test-shipit-render--frame old)))))

(ert-deftest test-shipit-table-code-keeps-its-background-at-the-line-edges ()
  "GIVEN a line of a table row whose code, marked as code of a table,
starts with spaces and ends with the spaces that fill its column
WHEN the faces on the white space at the edges of lines are taken off
THEN those spaces keep the code background, so the code column stays a
box."
  (let* ((face '(:background "#333333"))
         (result (shipit--strip-face-on-whitespace
                  (concat (propertize "  make  " 'face face 'shipit-table-code t)
                          "\n"))))
    (should (equal face (get-text-property 0 'face result)))
    (should (equal face (get-text-property 7 'face result)))))

(ert-deftest test-shipit-a-code-block-background-ends-at-its-fence ()
  "GIVEN a buffer with an indented code block of elisp, with an indented
blank line in its code, AND a heading after it
WHEN the code blocks of the buffer are drawn
THEN the background of the block ends at the end of the line of its
closing fence, though the colored code that takes the place of the
code is shorter, AND the heading has no background."
  (let ((shipit-code-block-background "#333333"))
    (with-temp-buffer
      (insert "     ```elisp\n     (setq cup 1)\n     \n     (setq tea 2)\n     ```\n"
              "   Heading\n")
      (shipit--apply-code-block-backgrounds-in-region (point-min) (point-max))
      (let ((block (seq-find (lambda (ov) (overlay-get ov 'shipit-code-block))
                             (overlays-in (point-min) (point-max))))
            (heading (save-excursion (goto-char (point-min))
                                     (search-forward "Heading")
                                     (line-beginning-position))))
        (should (equal heading (overlay-end block)))))))

(ert-deftest test-shipit-table-code-gets-no-code-block-over-its-row ()
  "GIVEN a buffer with a line of a table row whose code, marked as code
of a table, has the face of an indented code block
WHEN the code blocks of the buffer are drawn
THEN no code block is drawn over the row, as the code of the table has
its background already."
  (let ((shipit-code-block-background "#333333"))
    (with-temp-buffer
      (insert (propertize " make tea " 'face 'markdown-pre-face 'shipit-table-code t)
              " │ Brew it\nafter\n")
      (shipit--apply-code-block-backgrounds-in-region (point-min) (point-max))
      (should-not (seq-some (lambda (ov) (overlay-get ov 'shipit-code-block))
                            (overlays-in (point-min) (point-max)))))))

(ert-deftest test-shipit-a-table-column-starts-at-its-place-after-the-indent ()
  "GIVEN a buffer with a line of a table row after an indent of 5: the
space before its bar is marked with column 21, AND a plain line
WHEN the table columns are put in their places
THEN that space is shown as room up to column 26, AND nothing of the
plain line is shown differently."
  (with-temp-buffer
    (insert "     "
            (propertize (concat "one" (propertize " " 'shipit-column 21) "│ two")
                        'shipit-no-wrap t)
            "\n     plain\n")
    (shipit--align-table-columns-in-region (point-min) (point-max))
    (should (equal '(space :align-to 26) (get-text-property 9 'display)))
    (should (equal 9 (text-property-not-all (point-min) (point-max) 'display nil)))
    (should-not (text-property-not-all 10 (point-max) 'display nil))))

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

(ert-deftest test-shipit--render-markdown-cache-keeps-frames-without-images-apart ()
  "GIVEN the markdown cache is empty
WHEN the same text renders once where the frame shows no images, as a
terminal frame, and once where the frame shows them
THEN the cache holds two renders, so that a frame that shows images
never gets the text that stands for a picture in a terminal."
  (let ((shipit--markdown-render-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
      (shipit--render-markdown "Hello **world**"))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (shipit--render-markdown "Hello **world**"))
    (should (= 2 (hash-table-count shipit--markdown-render-cache)))))

(ert-deftest test-shipit--render-org-cache-keeps-frames-without-images-apart ()
  "GIVEN the org cache is empty
WHEN the same text renders once where the frame shows no images, and
once where the frame shows them
THEN the cache holds two renders, as the markdown cache does."
  (let ((shipit--org-render-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
      (shipit--render-org "Hello *world*"))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (shipit--render-org "Hello *world*"))
    (should (= 2 (hash-table-count shipit--org-render-cache)))))

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

;;; Color names in a CSS code block

(ert-deftest test-shipit-css-code-block-paints-no-color-names ()
  "GIVEN a fenced CSS code block that names the color white
WHEN shipit colors the code block in the buffer
THEN the color name gets no box and no background of its own, because
the background of the code block covers that background and leaves
only the box, while the property name keeps its CSS color."
  (with-temp-buffer
    (let ((shipit-code-block-background "#202020"))
      (insert "```css\n.cup { color: white; }\n```\n")
      (shipit--apply-code-block-backgrounds-in-region 1 (point-max))
      (let ((white (flatten-tree (get-text-property
                                  (1+ (string-search "white" (buffer-string))) 'face)))
            (color (flatten-tree (get-text-property
                                  (1+ (string-search "color" (buffer-string))) 'face))))
        (should-not (memq :box white))
        (should-not (memq :background white))
        (should (memq 'css-property color))))))

;;; Quotes, mentions and commits inside other text

(defun test-shipit-render--overlay-texts (property)
  "Return the text under each overlay of this buffer that has PROPERTY."
  (sort (mapcar (lambda (ov) (buffer-substring-no-properties
                              (overlay-start ov) (overlay-end ov)))
                (seq-filter (lambda (ov) (overlay-get ov property))
                            (overlays-in (point-min) (point-max))))
        #'string<))

(ert-deftest test-shipit-arrow-in-a-line-is-no-quote ()
  "GIVEN a comment line with an arrow, and two quote lines, one indented
WHEN the quote face is applied to the comment
THEN only the lines that start with the mark of a quote get it, so
a line that holds `->' keeps its own look."
  (with-temp-buffer
    (insert "Pour milk -> cup\n> Hot\n   > Cold\n")
    (shipit--apply-blockquote-faces (point-min) (point-max))
    (should-not (get-text-property 1 'font-lock-face))
    (should (memq 'shipit-blockquote-face
                  (ensure-list (get-text-property (string-search "Hot" (buffer-string))
                                                  'font-lock-face))))
    (should (memq 'shipit-blockquote-face
                  (ensure-list (get-text-property (string-search "Cold" (buffer-string))
                                                  'font-lock-face))))))

(ert-deftest test-shipit-at-sign-inside-a-word-is-no-mention ()
  "GIVEN a comment with an address that holds an at sign, and a mention
WHEN the mentions of the comment are made into links
THEN only the at sign that starts a word is a mention, as on GitHub."
  (with-temp-buffer
    (let ((shipit--cached-current-user "nobody"))
      (insert "Mail barista@cafe or the tray@2 and ask @kim\n")
      (shipit--create-user-mention-overlays "acme/widget" (point-min) (point-max))
      (should (equal '("@kim") (test-shipit-render--overlay-texts 'help-echo))))))

(ert-deftest test-shipit-hex-in-a-path-is-no-commit ()
  "GIVEN a comment with a hex part of a file path, and a commit
WHEN the commits of the comment are made into links
THEN only the commit that stands by itself is a link, not the part of
the path."
  (with-temp-buffer
    (insert "See /kitchen/c0ffee1234.grinder/beans, fixed in abc1234.\n")
    (shipit--create-commit-sha-overlays "acme/widget" (point-min) (point-max))
    (should (equal '("abc1234") (test-shipit-render--overlay-texts 'shipit-commit-sha)))))

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

(defun test-shipit-render--moving-gif (directory)
  "Make a GIF of two frames, 40 by 20 pixels, in DIRECTORY; its file."
  (let ((file (expand-file-name "cup.gif" directory)))
    (call-process "convert" nil nil nil "-size" "40x20"
                  "xc:red" "xc:blue" "-loop" "0" file)
    file))

(defun test-shipit-render--gif-frames (file)
  "The number of frames of the GIF FILE, as ImageMagick counts them."
  (with-temp-buffer
    (call-process "identify" nil t nil file)
    (count-lines (point-min) (point-max))))

(ert-deftest test-shipit-a-gif-is-resized-by-imagemagick-without-gifsicle ()
  "GIVEN a moving GIF of two frames, 40 pixels wide, AND no gifsicle on
the path, but ImageMagick
WHEN the GIF is made ready to be shown 20 pixels wide
THEN a new file is shown, 20 pixels wide and still of two frames, so
that Emacs does not scale each frame as it draws it."
  (skip-unless (and (executable-find "convert") (executable-find "identify")))
  (let ((directory (make-temp-file "shipit-gif-test" t))
        (find (symbol-function 'executable-find)))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &rest args)
                     (unless (equal name "gifsicle") (apply find name args)))))
          (let* ((gif (test-shipit-render--moving-gif directory))
                 (shown (shipit--prepare-gif-for-display gif 20)))
            (should-not (equal gif shown))
            (should (equal 20 (car (shipit--gif-pixel-dimensions shown))))
            (should (= 2 (test-shipit-render--gif-frames shown)))))
      (delete-directory directory t))))

(ert-deftest test-shipit-a-gif-stays-without-a-tool-to-resize-it ()
  "GIVEN a GIF 40 pixels wide, AND neither gifsicle nor ImageMagick on
the path
WHEN the GIF is made ready to be shown 20 pixels wide
THEN the GIF itself is shown, and Emacs scales it."
  (skip-unless (executable-find "convert"))
  (let ((directory (make-temp-file "shipit-gif-test" t)))
    (unwind-protect
        (let ((gif (test-shipit-render--moving-gif directory)))
          (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
            (should (equal gif (shipit--prepare-gif-for-display gif 20)))))
      (delete-directory directory t))))

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
  "Return the video URL the first video button of TEXT plays, or nil."
  (when-let* ((at (text-property-any 0 (length text) 'shipit-video-button t text)))
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

(ert-deftest test-shipit-render-a-lone-url-that-its-site-calls-a-video-plays ()
  "GIVEN a line that holds only a URL that names no file, which its site
says is a video, AND that URL in a sentence, AND a line that holds only
a URL that no site claims
WHEN the videos of a body are processed
THEN the lone URL of the video becomes a Play button for it, as a forge
shows an uploaded video, AND the other two stay as they were."
  (let* ((video "https://example.com/uploads/7/cup")
         (other "https://example.com/uploads/7/tea")
         (shipit-video-url-functions (list (lambda (url) (equal url video))))
         (out (shipit--process-inline-videos
               (concat "Showcase:\n\n" video "\n\nSee " video " here.\n\n" other "\n"))))
    (should (equal video (test-shipit-render--video-url-of out)))
    (should (string-match-p (concat "See " (regexp-quote video) " here")
                            (substring-no-properties out)))
    (should (string-match-p (concat "^" (regexp-quote other) "$")
                            (substring-no-properties out)))
    (should (= 1 (with-temp-buffer
                   (insert out)
                   (how-many "Play" (point-min) (point-max)))))))

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
    (should (eq 'hand (get-text-property (string-match "v.mp4" out) 'pointer out)))))

(defun test-shipit-render--header-line (text)
  "Return the line of TEXT that holds its first video header, as plain text."
  (let ((at (text-property-any 0 (length text) 'shipit-video-button t text)))
    (with-temp-buffer
      (insert text)
      (goto-char (1+ at))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(ert-deftest test-shipit-render-video-its-site-folds-gets-a-header ()
  "GIVEN videos that their site shows in a section that folds, one with a
file name and one with none, AND a video of a site that does not
WHEN the button of each is made
THEN each of the first two is a header that says the name, or Video, and
an arrow that points right, as no video shows under it yet, AND the
third is a Play button."
  (let ((shipit-use-svglib-icons nil)
        (shipit-video-fold-functions
         (list (lambda (url) (string-prefix-p "https://example.com/" url)))))
    (should (string-match-p "v\\.mp4 ▸\\'" (test-shipit-render--header-line
                                             (shipit--render-video-link "https://example.com/v.mp4" "v.mp4"))))
    (should (string-match-p "Video ▸\\'" (test-shipit-render--header-line
                                         (shipit--render-video-link "https://example.com/v.mp4"))))
    (should-not (string-match-p "Play" (shipit--render-video-link "https://example.com/v.mp4")))
    (should (string-match-p "\\[> Play v\\.mp4\\]" (test-shipit-render--header-line
                                                   (shipit--render-video-link "https://example.org/v.mp4" "v.mp4"))))))

(ert-deftest test-shipit-render-video-header-has-the-camera-octicon ()
  "GIVEN SVG icons, AND a video that its site shows in a section that folds
WHEN the header of the video is made
THEN it starts with the image of the camera octicon, as that site draws it."
  (let ((asked nil)
        (shipit-video-fold-functions (list #'always)))
    ;; `features' is not special, so only `dlet' binds it for `featurep'.
    (dlet ((shipit-use-svglib-icons t)
           (features (cons 'svg-lib features)))
      (cl-letf (((symbol-function 'svg-lib-icon)
                 (lambda (name &rest _)
                   (push name asked)
                   '(image :type svg :data "<svg/>"))))
        (let* ((out (shipit--render-video-link "https://example.com/v.mp4" "v.mp4"))
               (at (text-property-any 0 (length out) 'shipit-video-button t out)))
          (should (imagep (get-text-property at 'display out))))))
    (should (equal '("device-camera-video") asked))))

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
           (search-forward "v.mp4")
           ,@body)))))

(defun test-shipit-render--press-play ()
  "Press RET on the video button at point, through its own keys."
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
  "Return the text of the line after the video button at point."
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

(defun test-shipit-render--video-hidden-p ()
  "Return non-nil when the video of this buffer is hidden."
  (save-excursion
    (goto-char (point-min))
    (search-forward "[VIDEO]")
    (invisible-p (match-beginning 0))))

(defun test-shipit-render--arrow ()
  "Return the arrow at the end of the line of point."
  (string (char-before (line-end-position))))

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

(ert-deftest test-shipit-render-tab-on-a-video-header-folds-its-video ()
  "GIVEN a video shown under its header, as its site shows it in a
section that folds, AND a video under a Play button
WHEN TAB is pressed on the header, as a GUI frame sends it and as a
terminal does, and then again
THEN the first press hides the video and the second shows it again, as
TAB folds a section, and not the section the header is in, AND TAB on
the Play button is left to the buffer, which folds the section it is in."
  (let ((shipit-video-fold-functions (list #'always)))
    (test-shipit-render--with-video-button #'test-shipit-render--video-frame
      (shipit--show-inline-videos)
      (dolist (key (list [tab] (kbd "TAB")))
        (call-interactively (lookup-key (get-text-property (point) 'keymap) key))
        (should (test-shipit-render--video-hidden-p))
        (call-interactively (lookup-key (get-text-property (point) 'keymap) key))
        (should-not (test-shipit-render--video-hidden-p)))))
  (test-shipit-render--with-video-button #'test-shipit-render--video-frame
    (should-not (lookup-key (get-text-property (point) 'keymap) (kbd "TAB")))
    (should-not (lookup-key (get-text-property (point) 'keymap) [tab]))))

(defun test-shipit-render--run-video-timer ()
  "Run the timer that shows the videos of this buffer, as Emacs does when idle."
  (let ((timer shipit--videos-timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(defmacro test-shipit-render--in-sections-with-video (&rest body)
  "Run BODY in a buffer of sections that watches for videos.
The section Showcases holds a line of text and a header over a video
that folds, indented as a body is, and the section Framework comes
after it.  BODY runs once Emacs was idle, with point at the start."
  (declare (indent 0))
  `(let ((shipit-image-auth-functions nil)
         (shipit-media-url-functions nil)
         (shipit-video-fold-functions (list #'always))
         (shipit-video-inline-function (lambda (&rest _) (test-shipit-render--video-frame))))
     (with-temp-buffer
       (magit-section-mode)
       (shipit--watch-videos)
       (let ((inhibit-read-only t))
         (magit-insert-section (root)
           (magit-insert-section (showcases)
             (magit-insert-heading "Showcases")
             (insert "   Text\n   "
                     (string-trim-left (shipit--render-video-link "https://example.com/a.mp4") "\n")))
           (magit-insert-section (framework)
             (magit-insert-heading "Framework"))))
       (test-shipit-render--run-video-timer)
       (goto-char (point-min))
       ,@body)))

(defun test-shipit-render--video-header-p ()
  "Return non-nil when point is at the start of the line of a video header."
  (and (bolp) (looking-at-p "   .*Video [▸▾]$")))

(ert-deftest test-shipit-render-n-and-p-land-on-a-video-header ()
  "GIVEN a section whose body holds a header over a video that folds, and
a section after it
WHEN n is pressed on the heading of the first section, then n again,
and then p
THEN the first n lands on the header, as on a section of its own, the
second on the next section, AND p goes back to the header."
  (test-shipit-render--in-sections-with-video
    (magit-section-forward)
    (should (test-shipit-render--video-header-p))
    (magit-section-forward)
    (should (looking-at-p "Framework"))
    (magit-section-backward)
    (should (test-shipit-render--video-header-p))))

(ert-deftest test-shipit-render-tab-hides-the-section-of-a-video ()
  "GIVEN a header over a video that folds, in a buffer of sections
WHEN TAB is pressed on the header, and then again
THEN the first press hides the section of the video, as TAB hides any
section, with the arrow to the right, AND the second shows it again."
  (test-shipit-render--in-sections-with-video
    (magit-section-forward)
    (dolist (hidden '(t nil))
      (call-interactively (lookup-key (get-text-property (point) 'keymap) [tab]))
      (should (eq hidden (oref (magit-current-section) hidden)))
      (should (eq hidden (and (test-shipit-render--video-hidden-p) t)))
      (should (equal (if hidden "▸" "▾") (test-shipit-render--arrow))))))

(ert-deftest test-shipit-render-a-video-played-later-is-in-its-section ()
  "GIVEN a header over a video that folds, in a buffer of sections, set
not to show its video until the header is pressed
WHEN RET is pressed on the header, and then TAB
THEN the video plays under the header, inside the section of the header,
so TAB hides it."
  (let ((shipit-video-inline-show nil))
    (test-shipit-render--in-sections-with-video
      (magit-section-forward)
      (test-shipit-render--press-play)
      (should (string-match-p "\\[VIDEO\\]" (test-shipit-render--line-after-button)))
      (call-interactively (lookup-key (get-text-property (point) 'keymap) [tab]))
      (should (oref (magit-current-section) hidden))
      (should (test-shipit-render--video-hidden-p)))))

(ert-deftest test-shipit-render-tab-anywhere-on-a-video-header-line-folds ()
  "GIVEN a buffer that watches for videos, with a header over a video
that folds, indented as a body is
WHEN Emacs is idle, so the buffer shows its videos, and TAB is pressed
on the indent before the header, and then again
THEN the first press hides the video and the second shows it again, as
on the text of the header: the indent comes after the header is made."
  (let ((shipit-image-auth-functions nil)
        (shipit-media-url-functions nil)
        (shipit-video-fold-functions (list #'always))
        (shipit-video-inline-function (lambda (&rest _) (test-shipit-render--video-frame))))
    (with-temp-buffer
      (shipit--watch-videos)
      (insert "Body:" (shipit--render-video-link "https://example.com/a.mp4") "After\n")
      (goto-char (point-min))
      (forward-line 1)
      (insert "   ")
      (let ((timer shipit--videos-timer))
        (cancel-timer timer)
        (apply (timer--function timer) (timer--args timer)))
      (goto-char (line-beginning-position))
      (dolist (hidden '(t nil))
        (call-interactively (lookup-key (get-text-property (point) 'keymap) [tab]))
        (should (eq hidden (and (test-shipit-render--video-hidden-p) t)))))))

(ert-deftest test-shipit-render-video-header-hides-and-shows-its-video ()
  "GIVEN a video shown under its header, as its site shows it in a
section that folds
WHEN RET is pressed on the header, and then again
THEN the arrow points down while the video shows, the first press hides
the video and turns the arrow to the right, and the second shows it
again, as that site folds a video at a click on its header, AND the
video is not pressed, nor another put in or played."
  (let ((shipit-video-fold-functions (list #'always)))
    (test-shipit-render--with-video-button #'test-shipit-render--video-frame
      (let ((test-shipit-render--frame-pressed 0))
      (shipit--show-inline-videos)
      (test-shipit-render--page-of (car inlined))
      (setq inlined nil)
      (should (equal "▾" (test-shipit-render--arrow)))
      (test-shipit-render--press-play)
      (should (test-shipit-render--video-hidden-p))
      (should (equal "▸" (test-shipit-render--arrow)))
      (test-shipit-render--press-play)
      (should-not (test-shipit-render--video-hidden-p))
      (should (equal "▾" (test-shipit-render--arrow)))
      (should (= 0 test-shipit-render--frame-pressed))
      (should (= 1 (how-many "\\[VIDEO\\]" (point-min) (point-max))))
      (should-not inlined)
      (should-not opened)))))

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

(ert-deftest test-shipit-render-videos-show-in-issue-pr-and-repo-buffers ()
  "GIVEN an issue buffer, a PR buffer and a repo buffer with its README
THEN each watches for videos, whenever its text arrives."
  (require 'shipit-issues-buffer)
  (require 'shipit-buffer)
  (require 'shipit-repo-buffer)
  (dolist (mode '(shipit-issue-mode shipit-mode shipit-repo-mode))
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
        (search-forward "v.mp4")
        (test-shipit-render--press-play)
        (should (string-prefix-p "file://" opened))))))

;;; Comments where Jira is not in use

(defmacro test-shipit-render--without-jira (&rest body)
  "Run BODY as in an Emacs where the Jira backend is not loaded.
It is loaded only when Jira is configured."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'shipit--create-jira-mention-overlays) nil))
     ,@body))

(defconst test-shipit-render--comment
  '((id . 1) (body . "It breaks when the file is empty.")
    (user (login . "someone")) (created_at . "2026-01-01T10:00:00Z"))
  "A comment of an issue or a pull request.")

(ert-deftest test-shipit-render-a-comment-body-without-jira ()
  "GIVEN an Emacs where the Jira backend is not loaded
WHEN the body of a comment is drawn, as the issue buffer draws it
THEN the body shows."
  (require 'shipit-http)
  (with-temp-buffer
    (test-shipit-render--without-jira
      (shipit--insert-comment-body-only test-shipit-render--comment 6 nil nil "o/r" 7 nil t))
    (should (string-match-p "It breaks when the file is empty" (buffer-string)))))

(ert-deftest test-shipit-render-a-comment-without-jira ()
  "GIVEN an Emacs where the Jira backend is not loaded
WHEN a comment is drawn, as a pull request draws it
THEN the comment shows."
  (require 'shipit-pr-sections)
  (with-temp-buffer
    (test-shipit-render--without-jira
      (shipit--insert-comment test-shipit-render--comment
                              :base-indent 6 :repo "o/r" :pr-number 7 :include-header t))
    (should (string-match-p "It breaks when the file is empty" (buffer-string)))))

;;; The lines of a paragraph

(ert-deftest test-shipit-the-lines-of-a-paragraph-are-joined ()
  "GIVEN a paragraph of two lines of text, a paragraph of three pictures
on lines of their own, as the badges of a README are, a list item with
a line that goes on under it, AND a quote of two lines
WHEN the lines of the paragraphs are joined
THEN each paragraph and each list item is one line, its lines parted
by a space, as a forge shows a file, AND each picture stays."
  (let* ((picture (propertize " " 'display '(image :type svg :file "badge.svg")))
         (result (shipit--join-paragraph-lines
                  (concat "Pour the milk\ninto the cup.\n\n"
                          picture "\n" picture "\n" picture "\n\n"
                          "- Stir\n  slowly\n\n"
                          "> Hot\n> milk\n"))))
    (should (equal (concat "Pour the milk into the cup.\n\n"
                           "     \n\n"
                           "- Stir slowly\n\n"
                           "> Hot milk\n")
                   (substring-no-properties result)))
    (should (= 3 (cl-count-if (lambda (i) (get-text-property i 'display result))
                              (number-sequence 0 (1- (length result))))))))

(ert-deftest test-shipit-lines-that-are-no-paragraph-stay ()
  "GIVEN two list items, a heading above a line of text, a table of two
rows, a fenced code block, an indented code block, a line that ends
with two spaces, a line that ends where a <br> tag stood, a quote line
above a line of text, an HTML tag above a line of text, AND a line
with the line of a heading under it
WHEN the lines of the paragraphs are joined
THEN each text is as it was."
  (dolist (text (list "- one\n- two"
                      "## Title\nText"
                      "| a |\n| b |"
                      "```sh\nmake tea\nmake coffee\n```"
                      (propertize "    make tea\n    make coffee" 'face 'markdown-pre-face)
                      "Hot  \nCold"
                      (concat "Hot" (propertize "\n" 'shipit-line-break t) "Cold")
                      "> Hot\nCold"
                      "<div>\nCold"
                      "Title\n====="))
    (should (equal (substring-no-properties text)
                   (substring-no-properties (shipit--join-paragraph-lines text))))))

(ert-deftest test-shipit-a-line-break-tag-keeps-its-line-when-lines-are-joined ()
  "GIVEN markdown with a <br> tag in a paragraph of two lines
WHEN the markdown is made ready AND the lines of its paragraphs are
joined
THEN the line break of the tag stays, AND the line of the source after
it joins the line before."
  (should (equal "Hot\nCold Tea"
                 (substring-no-properties
                  (shipit--join-paragraph-lines
                   (shipit--prepare-markdown "Hot<br>Cold\nTea"))))))

(ert-deftest test-shipit-the-lines-of-a-table-cell-are-joined-in-the-cell ()
  "GIVEN a table cell of two lines
WHEN the lines of the paragraphs are joined
THEN the cell is one line, AND all of it is still that cell."
  (should (equal '((1 0 "Pour milk"))
                 (test-shipit-render--table-cells
                  (shipit--join-paragraph-lines
                   (test-shipit-render--cell 1 0 "Pour\nmilk"))))))

;;; Preview popups

(defmacro test-shipit-render--popup-in-test-frame (&rest body)
  "Run BODY where a preview popup is shown in the frame of the test.
Batch Emacs makes no frame, and has no place on the screen for point
or for the edges of its frame."
  (declare (indent 0))
  `(let ((shipit--current-pr-preview-data nil))
     (cl-letf (((symbol-function 'make-frame) (lambda (&rest _) (selected-frame)))
               ((symbol-function 'frame-edges) (lambda (&rest _) '(0 0 800 600)))
               ((symbol-function 'posn-at-point)
                (lambda (&rest _) (list (selected-window) 1 '(0 . 0) 0))))
       ,@body)))

(defun test-shipit-render--popup-buffer (frame)
  "Return the buffer of the preview popup FRAME."
  (window-buffer (frame-selected-window frame)))

(ert-deftest test-shipit-render-a-preview-popup-is-a-buffer-to-read ()
  "GIVEN the preview popup of a pull request, AND that of an issue
WHEN each is shown
THEN its buffer is in a mode made from `special-mode', so that a package
of modal keys such as meow leaves the keys of the popup to it, AND `q'
and `b' are the keys of the popup."
  (let ((data '((title . "Order a coffee") (state . "open") (body . "Oat milk"))))
    (dolist (show (list (lambda () (shipit--create-pr-preview-frame 1 data))
                        (lambda () (shipit--create-issue-preview-frame 2 data))))
      (let ((buffer (test-shipit-render--popup-in-test-frame
                      (test-shipit-render--popup-buffer (funcall show)))))
        (unwind-protect
            (with-current-buffer buffer
              (should (derived-mode-p 'special-mode))
              (should (local-key-binding "q"))
              (should (local-key-binding "b"))
              (should-not mode-line-format))
          (kill-buffer buffer))))))

(provide 'test-shipit-render)
;;; test-shipit-render.el ends here
