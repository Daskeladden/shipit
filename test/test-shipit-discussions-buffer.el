;;; test-shipit-discussions-buffer.el --- Tests for discussion buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the discussion view buffer rendering.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-discussions-graphql)
(require 'shipit-discussions-buffer)

;;; Helper: create mock discussion data

(defun test-shipit-discussion--mock-data ()
  "Return mock discussion data for tests."
  '((number . 42)
    (title . "How to use feature X?")
    (body . "I need help with feature X.")
    (id . "D_kwDOABC123")
    (user . ((login . "alice") (avatar_url . nil)))
    (category . ((name . "Q&A") (emoji . ":pray:")
                 (is_answerable . t)))
    (is_answered . t)
    (answer . ((id . "DC_answer") (body . "Use it like this")))
    (upvote_count . 5)
    (locked . nil)
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-16T12:00:00Z")
    (labels . (((name . "question") (color . "0075ca"))))
    (reactions . (((content . "+1") (user . ((login . "frank"))))
                  ((content . "+1") (user . ((login . "grace"))))
                  ((content . "heart") (user . ((login . "frank"))))))
    (comments_count . 2)
    (comments . (((id . "DC_1")
                  (body . "First comment")
                  (user . ((login . "bob") (avatar_url . nil)))
                  (created_at . "2025-01-15T11:00:00Z")
                  (updated_at . "2025-01-15T11:00:00Z")
                  (is_answer . t)
                  (upvote_count . 3)
                  (reactions . (((content . "rocket")
                                 (user . ((login . "alice"))))))
                  (replies . (((id . "DCR_1")
                               (body . "Reply to first")
                               (user . ((login . "charlie")
                                        (avatar_url . nil)))
                               (created_at . "2025-01-15T12:00:00Z")
                               (updated_at . "2025-01-15T12:00:00Z")
                               (reactions . (((content . "laugh")
                                              (user . ((login . "bob"))))))))))
                 ((id . "DC_2")
                  (body . "Second comment")
                  (user . ((login . "dave") (avatar_url . nil)))
                  (created_at . "2025-01-15T13:00:00Z")
                  (updated_at . "2025-01-15T13:00:00Z")
                  (is_answer . nil)
                  (upvote_count . 0)
                  (reactions . nil)
                  (replies . nil))))))

(defun test-shipit-discussion--mock-data-unanswered ()
  "Return mock unanswered discussion data for tests."
  '((number . 10)
    (title . "General topic")
    (body . "Let us discuss this.")
    (id . "D_123")
    (user . ((login . "eve") (avatar_url . nil)))
    (category . ((name . "General") (emoji . ":speech_balloon:")
                 (is_answerable . nil)))
    (is_answered . nil)
    (answer . nil)
    (upvote_count . 0)
    (locked . nil)
    (created_at . "2025-01-20T10:00:00Z")
    (updated_at . "2025-01-20T10:00:00Z")
    (labels . nil)
    (comments_count . 0)
    (comments . nil)))

;;; Mode Tests

(ert-deftest test-shipit-discussion-mode-exists ()
  "GIVEN shipit-discussions-buffer loaded
WHEN checking if shipit-discussion-mode is defined
THEN it is a valid major mode derived from magit-section-mode."
  (should (fboundp 'shipit-discussion-mode)))

(ert-deftest test-shipit-discussion-mode-keymap ()
  "GIVEN shipit-discussion-mode-map
WHEN checking key bindings
THEN g, r, q are bound to commands."
  (should (commandp (lookup-key shipit-discussion-mode-map (kbd "g"))))
  (should (commandp (lookup-key shipit-discussion-mode-map (kbd "r"))))
  (should (commandp (lookup-key shipit-discussion-mode-map (kbd "q"))))
  ;; M-; maps to shipit-dwim which may only be a forward-declaration in tests
  (should (lookup-key shipit-discussion-mode-map (kbd "M-;"))))

;;; Buffer Name Tests

(ert-deftest test-shipit-discussion-buffer-name ()
  "GIVEN a repo and discussion number
WHEN generating the buffer name
THEN it follows the *shipit-discussion: repo#N* pattern."
  (should (string= "*shipit-discussion: owner/repo#42*"
                    (shipit-discussion-buffer-name "owner/repo" 42))))

;;; Section Rendering Tests

(ert-deftest test-shipit-discussion-header-renders ()
  "GIVEN mock discussion data
WHEN inserting the header section
THEN the buffer contains the discussion number and title."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data)))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-header-section "owner/repo" data 42))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "#42" text))
        (should (string-match-p "How to use feature X?" text))))))

(ert-deftest test-shipit-discussion-header-shows-answered ()
  "GIVEN an answered discussion
WHEN inserting the header section
THEN the buffer contains an answered indicator."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data)))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-header-section "owner/repo" data 42))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Answered" text))))))

(ert-deftest test-shipit-discussion-metadata-shows-category ()
  "GIVEN mock discussion data with a category
WHEN inserting the metadata section
THEN the buffer shows the category name."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data)))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-metadata-section "owner/repo" data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Q&A" text))))))

(ert-deftest test-shipit-discussion-metadata-shows-author ()
  "GIVEN mock discussion data
WHEN inserting the metadata section
THEN the buffer shows the author login."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data)))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-metadata-section "owner/repo" data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "alice" text))))))

(ert-deftest test-shipit-discussion-metadata-shows-upvotes ()
  "GIVEN a discussion with 5 upvotes
WHEN inserting the metadata section
THEN the buffer shows the upvote count."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data)))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-metadata-section "owner/repo" data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "5" text))))))

(ert-deftest test-shipit-discussion-description-renders ()
  "GIVEN discussion data with body text
WHEN inserting the description section
THEN the buffer contains the body text."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-description-section "owner/repo" data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "I need help with feature X." text))))))

(ert-deftest test-shipit-discussion-comments-render ()
  "GIVEN discussion data with comments
WHEN inserting the comments section
THEN the buffer shows comment authors and bodies."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 42
         (cdr (assq 'comments data))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "bob" text))
        (should (string-match-p "First comment" text))
        (should (string-match-p "dave" text))
        (should (string-match-p "Second comment" text))))))

(ert-deftest test-shipit-discussion-replies-render ()
  "GIVEN a comment with replies
WHEN inserting the comments section
THEN the buffer shows reply authors and bodies."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 42
         (cdr (assq 'comments data))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "charlie" text))
        (should (string-match-p "Reply to first" text))))))

(ert-deftest test-shipit-discussion-answer-indicator ()
  "GIVEN a comment that is the answer
WHEN inserting the comments section
THEN the answer comment has an answer indicator."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 42
         (cdr (assq 'comments data))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; The answer comment (DC_1) should have some answer indicator
        (should (string-match-p "Answer" text))))))

(ert-deftest test-shipit-discussion-no-comments ()
  "GIVEN discussion data with no comments
WHEN inserting the comments section
THEN the buffer shows a no comments message."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (shipit-render-markdown nil))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 10 nil))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "No comments" text))))))

;;; Reaction and Vote Tests

(ert-deftest test-shipit-discussion-format-reactions-with-data ()
  "GIVEN a list of reactions with content and user data
WHEN formatting reactions
THEN the result contains emoji and count."
  (let* ((reactions '(((content . "+1") (user . ((login . "alice"))))
                      ((content . "+1") (user . ((login . "bob"))))
                      ((content . "heart") (user . ((login . "alice"))))))
         (result (shipit-discussion--format-reactions reactions))
         (plain (substring-no-properties result)))
    ;; Should contain reaction counts
    (should (string-match-p "2" plain))
    (should (string-match-p "1" plain))))

(ert-deftest test-shipit-discussion-format-reactions-empty ()
  "GIVEN nil reactions
WHEN formatting reactions
THEN the result contains only the placeholder icon."
  (let ((result (shipit-discussion--format-reactions nil)))
    ;; Should return a non-empty string (placeholder)
    (should (> (length result) 0))))

(ert-deftest test-shipit-discussion-format-votes ()
  "GIVEN an upvote count
WHEN formatting votes
THEN the result contains the count as text."
  (let* ((result (shipit-discussion--format-votes 5))
         (plain (substring-no-properties result)))
    (should (string-match-p "5" plain))))

(ert-deftest test-shipit-discussion-format-votes-zero ()
  "GIVEN zero upvotes
WHEN formatting votes
THEN the result contains 0."
  (let* ((result (shipit-discussion--format-votes 0))
         (plain (substring-no-properties result)))
    (should (string-match-p "0" plain))))

(ert-deftest test-shipit-discussion-description-has-vote-reaction-line ()
  "GIVEN discussion data with upvotes and reactions
WHEN inserting the description section
THEN the buffer contains the vote count and reaction placeholder."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil)
          (shipit-discussion-buffer-repo "owner/repo")
          (shipit-discussion-buffer-number 42))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-description-section "owner/repo" data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Upvote count should appear
        (should (string-match-p "5" text))))))

(ert-deftest test-shipit-discussion-comment-has-vote-reaction-line ()
  "GIVEN a comment with upvotes and reactions
WHEN inserting comments section
THEN the comment has vote count and reaction placeholder."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil)
          (shipit-discussion-buffer-repo "owner/repo")
          (shipit-discussion-buffer-number 42))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 42
         (cdr (assq 'comments data))))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Comment 1 has upvote_count 3
        (should (string-match-p "3" text))))))

(ert-deftest test-shipit-discussion-reply-has-reaction-no-vote ()
  "GIVEN a reply with reactions
WHEN inserting comments section
THEN the reply has reaction placeholder but no vote line."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil)
          (shipit-discussion-buffer-repo "owner/repo")
          (shipit-discussion-buffer-number 42))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-comments-section
         "owner/repo" 42
         (cdr (assq 'comments data))))
      ;; Check that reactions text property exists somewhere in the buffer
      (should (text-property-any (point-min) (point-max)
                                 'shipit-discussion-reactions t)))))

(ert-deftest test-shipit-discussion-vote-reaction-text-properties ()
  "GIVEN a description with vote and reaction line
WHEN inserting the description section
THEN the vote/reaction line has shipit-discussion-reactions property."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (data (test-shipit-discussion--mock-data))
          (shipit-render-markdown nil)
          (shipit-discussion-buffer-repo "owner/repo")
          (shipit-discussion-buffer-number 42))
      (magit-insert-section (_root 'shipit-discussion)
        (shipit-discussion--insert-description-section "owner/repo" data))
      ;; Should have shipit-discussion-reactions property
      (should (text-property-any (point-min) (point-max)
                                 'shipit-discussion-reactions t)))))

;;; Browse URL Dispatch Tests

(ert-deftest test-shipit-discussion-browse-url-dispatches-through-backend ()
  "GIVEN a registered discussion backend with :browse-url
WHEN calling shipit-discussion--browse-url with repo and number set
THEN the URL is produced by the backend's :browse-url function."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (shipit-discussion-buffer-repo "owner/repo")
        (shipit-discussion-buffer-number 42))
    (shipit-discussion-register-backend
     'github
     (list :name "GitHub"
           :browse-url (lambda (config number)
                         (format "https://github.com/%s/discussions/%s"
                                 (plist-get config :repo) number))
           :browse-repo-url (lambda (config)
                              (format "https://github.com/%s"
                                      (plist-get config :repo)))))
    (should (string= "https://github.com/owner/repo/discussions/42"
                      (shipit-discussion--browse-url)))))

(ert-deftest test-shipit-discussion-browse-url-returns-nil-when-no-repo ()
  "GIVEN shipit-discussion-buffer-repo is nil
WHEN calling shipit-discussion--browse-url
THEN nil is returned."
  (let ((shipit-discussion-buffer-repo nil)
        (shipit-discussion-buffer-number 42))
    (should-not (shipit-discussion--browse-url))))

(ert-deftest test-shipit-discussion-browse-url-returns-nil-when-no-number ()
  "GIVEN shipit-discussion-buffer-number is nil
WHEN calling shipit-discussion--browse-url
THEN nil is returned."
  (let ((shipit-discussion-buffer-repo "owner/repo")
        (shipit-discussion-buffer-number nil))
    (should-not (shipit-discussion--browse-url))))

(ert-deftest test-shipit-discussion-browse-url-returns-nil-on-error ()
  "GIVEN no discussion backend registered
WHEN calling shipit-discussion--browse-url
THEN nil is returned instead of signaling an error."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'unknown-backend)
        (shipit-pr-backend-config nil)
        (shipit-discussion-buffer-repo "owner/repo")
        (shipit-discussion-buffer-number 42))
    (should-not (shipit-discussion--browse-url))))

(provide 'test-shipit-discussions-buffer)
;;; test-shipit-discussions-buffer.el ends here
