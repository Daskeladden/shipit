;;; test-shipit-discussions-graphql.el --- Tests for discussions GraphQL layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitHub Discussions GraphQL API layer.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-discussions-graphql)

;;; Normalization Tests

(ert-deftest test-shipit-discussion-normalize-basic ()
  "GIVEN a GraphQL discussion node with standard fields
WHEN normalizing the node
THEN returns alist with snake_case keys and correct values."
  (let* ((node '((number . 42)
                 (title . "How do I use feature X?")
                 (body . "I need help with feature X.")
                 (id . "D_kwDOABC123")
                 (author . ((login . "alice")
                            (avatarUrl . "https://example.com/alice.png")))
                 (category . ((name . "Q&A")
                              (emoji . ":pray:")
                              (isAnswerable . t)))
                 (isAnswered . :json-false)
                 (answer . nil)
                 (upvoteCount . 5)
                 (locked . :json-false)
                 (createdAt . "2025-01-15T10:00:00Z")
                 (updatedAt . "2025-01-16T12:00:00Z")
                 (labels . ((nodes . [])))
                 (comments . ((totalCount . 3)))))
         (result (shipit-discussion--normalize node)))
    (should (equal 42 (cdr (assq 'number result))))
    (should (string= "How do I use feature X?" (cdr (assq 'title result))))
    (should (string= "I need help with feature X." (cdr (assq 'body result))))
    (should (string= "D_kwDOABC123" (cdr (assq 'id result))))
    (should (string= "alice" (cdr (assq 'login (cdr (assq 'user result))))))
    (should (string= "Q&A" (cdr (assq 'name (cdr (assq 'category result))))))
    (should (eq nil (cdr (assq 'is_answered result))))
    (should (equal 5 (cdr (assq 'upvote_count result))))
    (should (eq nil (cdr (assq 'locked result))))
    (should (string= "2025-01-15T10:00:00Z" (cdr (assq 'created_at result))))
    (should (string= "2025-01-16T12:00:00Z" (cdr (assq 'updated_at result))))
    (should (equal 3 (cdr (assq 'comments_count result))))))

(ert-deftest test-shipit-discussion-normalize-answered ()
  "GIVEN a GraphQL discussion node that is answered
WHEN normalizing the node
THEN is_answered is t."
  (let* ((node '((number . 10)
                 (title . "Answered question")
                 (body . "Body")
                 (id . "D_123")
                 (author . ((login . "bob") (avatarUrl . nil)))
                 (category . ((name . "Q&A")
                              (emoji . ":pray:")
                              (isAnswerable . t)))
                 (isAnswered . t)
                 (answer . ((id . "DC_456") (body . "The answer")))
                 (upvoteCount . 0)
                 (locked . :json-false)
                 (createdAt . "2025-01-15T10:00:00Z")
                 (updatedAt . "2025-01-15T10:00:00Z")
                 (labels . ((nodes . [])))
                 (comments . ((totalCount . 1)))))
         (result (shipit-discussion--normalize node)))
    (should (eq t (cdr (assq 'is_answered result))))))

(ert-deftest test-shipit-discussion-normalize-labels ()
  "GIVEN a discussion node with multiple labels
WHEN normalizing
THEN labels list contains alists with name and color keys."
  (let* ((label-nodes (vector '((name . "bug") (color . "d73a4a"))
                              '((name . "help") (color . "0075ca"))))
         (node `((number . 1) (title . "T") (body . "B") (id . "D_1")
                 (author . ((login . "u") (avatarUrl . nil)))
                 (category . ((name . "General")
                              (emoji . ":speech_balloon:")
                              (isAnswerable . :json-false)))
                 (isAnswered . :json-false) (answer . nil)
                 (upvoteCount . 0) (locked . :json-false)
                 (createdAt . "2025-01-01T00:00:00Z")
                 (updatedAt . "2025-01-01T00:00:00Z")
                 (labels . ((nodes . ,label-nodes)))
                 (comments . ((totalCount . 0)))))
         (result (shipit-discussion--normalize node))
         (labels (cdr (assq 'labels result))))
    (should (= 2 (length labels)))
    (should (string= "bug" (cdr (assq 'name (car labels)))))
    (should (string= "d73a4a" (cdr (assq 'color (car labels)))))))

;;; Comment Normalization Tests

(ert-deftest test-shipit-discussion-normalize-comment ()
  "GIVEN a GraphQL discussion comment node
WHEN normalizing
THEN returns alist with id, body, user, timestamps, reactions, and replies."
  (let* ((reply-node '((id . "DCR_reply1")
                       (body . "A reply")
                       (author . ((login . "eve") (avatarUrl . nil)))
                       (createdAt . "2025-01-15T13:00:00Z")
                       (updatedAt . "2025-01-15T13:00:00Z")
                       (reactions . ((nodes . [])))))
         (reaction-node '((content . "THUMBS_UP")
                          (user . ((login . "dave")))))
         (comment-node
          `((id . "DC_abc123")
            (body . "This is a comment")
            (author . ((login . "charlie")
                       (avatarUrl . "https://example.com/charlie.png")))
            (createdAt . "2025-01-15T11:00:00Z")
            (updatedAt . "2025-01-15T12:00:00Z")
            (isAnswer . :json-false)
            (upvoteCount . 2)
            (reactions . ((nodes . ,(vector reaction-node))))
            (replies . ((nodes . ,(vector reply-node))))))
         (result (shipit-discussion--normalize-comment comment-node)))
    (should (string= "DC_abc123" (cdr (assq 'id result))))
    (should (string= "This is a comment" (cdr (assq 'body result))))
    (should (string= "charlie" (cdr (assq 'login (cdr (assq 'user result))))))
    (should (string= "2025-01-15T11:00:00Z" (cdr (assq 'created_at result))))
    (should (eq nil (cdr (assq 'is_answer result))))
    (should (equal 2 (cdr (assq 'upvote_count result))))
    ;; Check reactions normalized
    (let ((reactions (cdr (assq 'reactions result))))
      (should (= 1 (length reactions)))
      (should (string= "+1" (cdr (assq 'content (car reactions))))))
    ;; Check replies
    (let ((replies (cdr (assq 'replies result))))
      (should (= 1 (length replies)))
      (should (string= "DCR_reply1" (cdr (assq 'id (car replies)))))
      (should (string= "A reply" (cdr (assq 'body (car replies))))))))

(ert-deftest test-shipit-discussion-normalize-comment-is-answer ()
  "GIVEN a discussion comment that is the answer
WHEN normalizing
THEN is_answer is t."
  (let* ((comment-node
          '((id . "DC_answer")
            (body . "The answer")
            (author . ((login . "alice") (avatarUrl . nil)))
            (createdAt . "2025-01-15T10:00:00Z")
            (updatedAt . "2025-01-15T10:00:00Z")
            (isAnswer . t)
            (upvoteCount . 10)
            (reactions . ((nodes . [])))
            (replies . ((nodes . [])))))
         (result (shipit-discussion--normalize-comment comment-node)))
    (should (eq t (cdr (assq 'is_answer result))))))

;;; Reaction Normalization Tests

(ert-deftest test-shipit-discussion-normalize-reactions ()
  "GIVEN GraphQL reaction nodes
WHEN normalizing
THEN content is converted from THUMBS_UP to +1 format."
  (let* ((nodes (vector '((content . "THUMBS_UP") (user . ((login . "alice"))))
                        '((content . "HEART") (user . ((login . "bob"))))
                        '((content . "ROCKET") (user . ((login . "charlie"))))))
         (result (shipit-discussion--normalize-reactions nodes)))
    (should (= 3 (length result)))
    (should (string= "+1" (cdr (assq 'content (nth 0 result)))))
    (should (string= "heart" (cdr (assq 'content (nth 1 result)))))
    (should (string= "rocket" (cdr (assq 'content (nth 2 result)))))))

(ert-deftest test-shipit-discussion-normalize-reactions-empty ()
  "GIVEN an empty reactions vector
WHEN normalizing
THEN returns nil."
  (should (null (shipit-discussion--normalize-reactions [])))
  (should (null (shipit-discussion--normalize-reactions nil))))

;;; Fetch Tests

(ert-deftest test-shipit-discussion-fetch-calls-graphql ()
  "GIVEN a repo and discussion number
WHEN calling shipit-discussion--fetch
THEN it calls shipit--graphql-query with correct variables."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((repository
                    . ((discussion
                        . ((number . 42) (title . "Test") (body . "Body")
                           (id . "D_1")
                           (author . ((login . "u") (avatarUrl . nil)))
                           (category . ((name . "G") (emoji . ":x:")
                                        (isAnswerable . :json-false)))
                           (isAnswered . :json-false) (answer . nil)
                           (upvoteCount . 0) (locked . :json-false)
                           (createdAt . "2025-01-01T00:00:00Z")
                           (updatedAt . "2025-01-01T00:00:00Z")
                           (labels . ((nodes . [])))
                           (comments . ((totalCount . 0)
                                        (nodes . [])))))))))))
      (let ((result (shipit-discussion--fetch "owner/repo" 42)))
        (should called-variables)
        (should (string= "owner" (cdr (assq 'owner called-variables))))
        (should (string= "repo" (cdr (assq 'name called-variables))))
        (should (equal 42 (cdr (assq 'number called-variables))))
        (should result)
        (should (equal 42 (cdr (assq 'number result))))))))

(ert-deftest test-shipit-discussion-search-builds-query ()
  "GIVEN search args
WHEN calling shipit-discussion--search
THEN it passes a search query with repo:owner/repo to GraphQL."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((search . ((nodes . [])
                              (pageInfo . ((hasNextPage . :json-false)))))))))
      (shipit-discussion--search "owner/repo" '("--limit=50"))
      (should called-variables)
      (let ((query-str (cdr (assq 'query called-variables))))
        (should (string-match-p "repo:owner/repo" query-str))))))

(ert-deftest test-shipit-discussion-search-with-category-filter ()
  "GIVEN search args with --category=Q&A
WHEN building the search query
THEN it includes category:Q&A in the query string."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((search . ((nodes . [])
                              (pageInfo . ((hasNextPage . :json-false)))))))))
      (shipit-discussion--search "owner/repo" '("--category=Q&A" "--limit=50"))
      (let ((query-str (cdr (assq 'query called-variables))))
        (should (string-match-p "category:\"Q&A\"" query-str))))))

(ert-deftest test-shipit-discussion-search-with-answered-filter ()
  "GIVEN search args with --answered=yes
WHEN building the search query
THEN it includes is:answered in the query string."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((search . ((nodes . [])
                              (pageInfo . ((hasNextPage . :json-false)))))))))
      (shipit-discussion--search "owner/repo" '("--answered=yes" "--limit=50"))
      (let ((query-str (cdr (assq 'query called-variables))))
        (should (string-match-p "is:answered" query-str))))))

(ert-deftest test-shipit-discussion-search-with-unanswered-filter ()
  "GIVEN search args with --answered=no
WHEN building the search query
THEN it includes is:unanswered in the query string."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((search . ((nodes . [])
                              (pageInfo . ((hasNextPage . :json-false)))))))))
      (shipit-discussion--search "owner/repo" '("--answered=no" "--limit=50"))
      (let ((query-str (cdr (assq 'query called-variables))))
        (should (string-match-p "is:unanswered" query-str))))))

(ert-deftest test-shipit-discussion-fetch-categories ()
  "GIVEN a repo
WHEN calling shipit-discussion--fetch-categories
THEN it calls GraphQL and returns a list of category alists."
  (let ((shipit-discussion--categories-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query _variables)
                 (let ((cat1 '((id . "DIC_1") (name . "General")
                               (emoji . ":speech_balloon:")
                               (description . "General discussions")
                               (isAnswerable . :json-false)))
                       (cat2 '((id . "DIC_2") (name . "Q&A")
                               (emoji . ":pray:")
                               (description . "Ask questions")
                               (isAnswerable . t))))
                   `((repository
                      . ((discussionCategories
                          . ((nodes . ,(vector cat1 cat2)))))))))))
      (let ((result (shipit-discussion--fetch-categories "owner/repo")))
        (should (= 2 (length result)))
        (should (string= "General" (cdr (assq 'name (car result)))))
        (should (string= "Q&A" (cdr (assq 'name (cadr result)))))
        (should (eq t (cdr (assq 'is_answerable (cadr result)))))))))

(ert-deftest test-shipit-discussion-create-calls-mutation ()
  "GIVEN repo, title, body, and category-id
WHEN calling shipit-discussion--create
THEN it calls GraphQL createDiscussion mutation with correct variables."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((createDiscussion
                    . ((discussion
                        . ((number . 99) (title . "New discussion")
                           (body . "Discussion body") (id . "D_new")
                           (author . ((login . "me") (avatarUrl . nil)))
                           (category . ((name . "General") (emoji . ":x:")
                                        (isAnswerable . :json-false)))
                           (isAnswered . :json-false) (answer . nil)
                           (upvoteCount . 0) (locked . :json-false)
                           (createdAt . "2025-01-20T10:00:00Z")
                           (updatedAt . "2025-01-20T10:00:00Z")
                           (labels . ((nodes . [])))
                           (comments . ((totalCount . 0))))))))))
              ((symbol-function 'shipit-discussion--fetch-repo-id)
               (lambda (_repo) "R_abc123")))
      (let ((result (shipit-discussion--create
                     "owner/repo" "New discussion" "Discussion body" "DIC_1")))
        (should called-variables)
        (should (string= "R_abc123"
                          (cdr (assq 'repositoryId called-variables))))
        (should (string= "New discussion"
                          (cdr (assq 'title called-variables))))
        (should (string= "Discussion body"
                          (cdr (assq 'body called-variables))))
        (should (string= "DIC_1"
                          (cdr (assq 'categoryId called-variables))))
        (should result)
        (should (equal 99 (cdr (assq 'number result))))))))

(ert-deftest test-shipit-discussion-add-comment-calls-mutation ()
  "GIVEN a discussion ID and body
WHEN calling shipit-discussion--add-comment
THEN it calls GraphQL addDiscussionComment mutation."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((addDiscussionComment
                    . ((comment
                        . ((id . "DC_new") (body . "A new comment")
                           (author . ((login . "me") (avatarUrl . nil)))
                           (createdAt . "2025-01-20T10:00:00Z")
                           (updatedAt . "2025-01-20T10:00:00Z")
                           (isAnswer . :json-false) (upvoteCount . 0)
                           (reactions . ((nodes . [])))
                           (replies . ((nodes . [])))))))))))
      (shipit-discussion--add-comment "D_kwDO123" "A new comment")
      (should called-variables)
      (should (string= "D_kwDO123"
                        (cdr (assq 'discussionId called-variables))))
      (should (string= "A new comment"
                        (cdr (assq 'body called-variables)))))))

(ert-deftest test-shipit-discussion-reply-calls-mutation ()
  "GIVEN a discussion ID, reply-to ID, and body
WHEN calling shipit-discussion--reply-to-comment
THEN it calls GraphQL addDiscussionComment mutation with replyToId."
  (let ((called-variables nil))
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query variables)
                 (setq called-variables variables)
                 '((addDiscussionComment
                    . ((comment
                        . ((id . "DCR_new") (body . "A reply")
                           (author . ((login . "me") (avatarUrl . nil)))
                           (createdAt . "2025-01-20T10:00:00Z")
                           (updatedAt . "2025-01-20T10:00:00Z")
                           (reactions . ((nodes . [])))))))))))
      (shipit-discussion--reply-to-comment "D_kwDO123" "DC_parent" "A reply")
      (should called-variables)
      (should (string= "D_kwDO123"
                        (cdr (assq 'discussionId called-variables))))
      (should (string= "DC_parent"
                        (cdr (assq 'replyToId called-variables))))
      (should (string= "A reply"
                        (cdr (assq 'body called-variables)))))))

;;; Reaction Mutation Tests

(ert-deftest test-shipit-discussion-rest-to-graphql-reaction ()
  "GIVEN REST API reaction content strings
WHEN converting to GraphQL format
THEN returns correct ReactionContent enum values."
  (should (string= "THUMBS_UP"
                    (shipit-discussion--rest-to-graphql-reaction "+1")))
  (should (string= "THUMBS_DOWN"
                    (shipit-discussion--rest-to-graphql-reaction "-1")))
  (should (string= "HEART"
                    (shipit-discussion--rest-to-graphql-reaction "heart")))
  (should (string= "ROCKET"
                    (shipit-discussion--rest-to-graphql-reaction "rocket")))
  (should (string= "LAUGH"
                    (shipit-discussion--rest-to-graphql-reaction "laugh")))
  (should (string= "EYES"
                    (shipit-discussion--rest-to-graphql-reaction "eyes"))))

(ert-deftest test-shipit-discussion-normalize-includes-reactions ()
  "GIVEN a GraphQL discussion node with reactions
WHEN normalizing the node
THEN the reactions field is populated with converted content."
  (let* ((node '((number . 1)
                 (title . "Test")
                 (body . "Body")
                 (id . "D_123")
                 (author . ((login . "user1") (avatarUrl . nil)))
                 (category . ((name . "General") (emoji . nil)
                              (isAnswerable . :json-false)))
                 (isAnswered . :json-false)
                 (answer . nil)
                 (upvoteCount . 0)
                 (locked . :json-false)
                 (createdAt . "2025-01-01T00:00:00Z")
                 (updatedAt . "2025-01-01T00:00:00Z")
                 (labels . ((nodes . [])))
                 (reactions . ((nodes . [((content . "THUMBS_UP")
                                          (user . ((login . "alice"))))
                                         ((content . "HEART")
                                          (user . ((login . "bob"))))])))
                 (comments . ((totalCount . 0)))))
         (result (shipit-discussion--normalize node))
         (reactions (cdr (assq 'reactions result))))
    (should (= 2 (length reactions)))
    (should (string= "+1" (cdr (assq 'content (car reactions)))))
    (should (string= "heart" (cdr (assq 'content (cadr reactions)))))))

(provide 'test-shipit-discussions-graphql)
;;; test-shipit-discussions-graphql.el ends here
