;;; shipit-discussions-graphql.el --- GitHub Discussions GraphQL API -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; GraphQL queries and data normalization for GitHub Discussions.
;; Discussions are GraphQL-only (no REST API).

;;; Code:

(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-discussion-backends)

;;; Caches

(defvar shipit-discussion--categories-cache (make-hash-table :test 'equal)
  "Cache of categories per repo. Key: repo string, Value: list of category alists.")

(defvar shipit-discussion--repo-id-cache (make-hash-table :test 'equal)
  "Cache of GraphQL node IDs per repo. Key: repo string, Value: node ID string.")

;;; Emoji Shortcode Conversion

(defconst shipit-discussion--emoji-table
  '(;; GitHub default discussion category emoji
    (":mega:" . "📣")
    (":speech_balloon:" . "💬")
    (":bulb:" . "💡")
    (":pray:" . "🙏")
    (":raised_hands:" . "🙌")
    (":bar_chart:" . "📊")
    (":people_holding_hands:" . "🧑‍🤝‍🧑")
    ;; Common extras
    (":rocket:" . "🚀")
    (":dart:" . "🎯")
    (":question:" . "❓")
    (":package:" . "📦")
    (":clipboard:" . "📋")
    (":bookmark:" . "🔖")
    (":hammer_and_wrench:" . "🛠️")
    (":bug:" . "🐛")
    (":sparkles:" . "✨")
    (":warning:" . "⚠️")
    (":lock:" . "🔒")
    (":gear:" . "⚙️")
    (":book:" . "📖")
    (":link:" . "🔗")
    (":star:" . "⭐")
    (":heart:" . "❤️")
    (":thumbsup:" . "👍")
    (":thumbsdown:" . "👎")
    (":eyes:" . "👀")
    (":tada:" . "🎉")
    (":fire:" . "🔥")
    (":zap:" . "⚡")
    (":memo:" . "📝")
    (":white_check_mark:" . "✅")
    (":x:" . "❌")
    (":newspaper:" . "📰")
    (":loudspeaker:" . "📢")
    (":pushpin:" . "📌")
    (":wrench:" . "🔧")
    (":earth_americas:" . "🌎")
    (":globe_with_meridians:" . "🌐")
    (":wave:" . "👋")
    (":light_bulb:" . "💡")
    (":thought_balloon:" . "💭")
    (":computer:" . "💻")
    (":mailbox:" . "📫"))
  "Mapping of GitHub emoji shortcodes to Unicode characters.")

(defun shipit-discussion--shortcode-to-emoji (shortcode)
  "Convert an emoji SHORTCODE like \":pray:\" to its Unicode character.
Returns the shortcode stripped of colons if no mapping is found."
  (if (or (null shortcode) (string-empty-p shortcode))
      ""
    (or (cdr (assoc shortcode shipit-discussion--emoji-table))
        ;; Strip colons as fallback
        (replace-regexp-in-string "^:\\|:$" "" shortcode))))

;;; Data Normalization

(defun shipit-discussion--json-false-p (value)
  "Return t if VALUE represents a JSON false value."
  (or (eq value :json-false)
      (eq value :false)
      (eq value 'false)))

(defun shipit-discussion--normalize-bool (value)
  "Normalize a JSON boolean VALUE to Elisp t/nil."
  (if (shipit-discussion--json-false-p value) nil t))

(defun shipit-discussion--normalize (node)
  "Normalize a GraphQL discussion NODE to an alist.
Converts camelCase GraphQL fields to snake_case."
  (let* ((author (cdr (assq 'author node)))
         (category-raw (cdr (assq 'category node)))
         (labels-raw (cdr (assq 'nodes (cdr (assq 'labels node)))))
         (reactions-raw (cdr (assq 'nodes (cdr (assq 'reactions node)))))
         (comments-obj (cdr (assq 'comments node)))
         (answer-raw (cdr (assq 'answer node))))
    `((number . ,(cdr (assq 'number node)))
      (title . ,(cdr (assq 'title node)))
      (body . ,(cdr (assq 'body node)))
      (id . ,(cdr (assq 'id node)))
      (user . ((login . ,(cdr (assq 'login author)))
               (avatar_url . ,(cdr (assq 'avatarUrl author)))))
      (category . ((name . ,(cdr (assq 'name category-raw)))
                   (emoji . ,(shipit-discussion--shortcode-to-emoji
                              (cdr (assq 'emoji category-raw))))
                   (is_answerable . ,(shipit-discussion--normalize-bool
                                      (cdr (assq 'isAnswerable category-raw))))))
      (is_answered . ,(shipit-discussion--normalize-bool
                       (cdr (assq 'isAnswered node))))
      (answer . ,(when answer-raw
                   `((id . ,(cdr (assq 'id answer-raw)))
                     (body . ,(cdr (assq 'body answer-raw))))))
      (upvote_count . ,(cdr (assq 'upvoteCount node)))
      (viewer_has_upvoted . ,(shipit-discussion--normalize-bool
                              (cdr (assq 'viewerHasUpvoted node))))
      (locked . ,(shipit-discussion--normalize-bool
                  (cdr (assq 'locked node))))
      (created_at . ,(cdr (assq 'createdAt node)))
      (updated_at . ,(cdr (assq 'updatedAt node)))
      (labels . ,(shipit-discussion--normalize-labels labels-raw))
      (reactions . ,(shipit-discussion--normalize-reactions reactions-raw))
      (comments_count . ,(cdr (assq 'totalCount comments-obj))))))

(defun shipit-discussion--normalize-labels (nodes)
  "Normalize a vector of label NODES to a list of alists."
  (when (and nodes (> (length nodes) 0))
    (mapcar (lambda (label)
              `((name . ,(cdr (assq 'name label)))
                (color . ,(cdr (assq 'color label)))))
            (append nodes nil))))

(defun shipit-discussion--normalize-comment (node)
  "Normalize a GraphQL discussion comment NODE to an alist."
  (let* ((author (cdr (assq 'author node)))
         (reactions-raw (cdr (assq 'nodes (cdr (assq 'reactions node)))))
         (replies-raw (cdr (assq 'nodes (cdr (assq 'replies node))))))
    `((id . ,(cdr (assq 'id node)))
      (body . ,(cdr (assq 'body node)))
      (user . ((login . ,(cdr (assq 'login author)))
               (avatar_url . ,(cdr (assq 'avatarUrl author)))))
      (created_at . ,(cdr (assq 'createdAt node)))
      (updated_at . ,(cdr (assq 'updatedAt node)))
      (is_answer . ,(shipit-discussion--normalize-bool
                     (cdr (assq 'isAnswer node))))
      (upvote_count . ,(cdr (assq 'upvoteCount node)))
      (viewer_has_upvoted . ,(shipit-discussion--normalize-bool
                              (cdr (assq 'viewerHasUpvoted node))))
      (reactions . ,(shipit-discussion--normalize-reactions reactions-raw))
      (replies . ,(shipit-discussion--normalize-replies replies-raw)))))

(defun shipit-discussion--normalize-reply (node)
  "Normalize a GraphQL discussion reply NODE to an alist."
  (let* ((author (cdr (assq 'author node)))
         (reactions-raw (cdr (assq 'nodes (cdr (assq 'reactions node))))))
    `((id . ,(cdr (assq 'id node)))
      (body . ,(cdr (assq 'body node)))
      (user . ((login . ,(cdr (assq 'login author)))
               (avatar_url . ,(cdr (assq 'avatarUrl author)))))
      (created_at . ,(cdr (assq 'createdAt node)))
      (updated_at . ,(cdr (assq 'updatedAt node)))
      (reactions . ,(shipit-discussion--normalize-reactions reactions-raw)))))

(defun shipit-discussion--normalize-replies (nodes)
  "Normalize a vector of reply NODES to a list of alists."
  (when (and nodes (> (length nodes) 0))
    (mapcar #'shipit-discussion--normalize-reply (append nodes nil))))

(defun shipit-discussion--normalize-reactions (nodes)
  "Normalize a vector of reaction NODES to a list of alists.
Converts GraphQL THUMBS_UP to REST +1 format."
  (when (and nodes (> (length nodes) 0))
    (mapcar (lambda (reaction)
              (let ((content (cdr (assq 'content reaction)))
                    (user (cdr (assq 'user reaction))))
                `((content . ,(shipit--graphql-reaction-to-rest content))
                  (user . ,user))))
            (append nodes nil))))

(defun shipit-discussion--normalize-category (node)
  "Normalize a GraphQL category NODE to an alist."
  `((id . ,(cdr (assq 'id node)))
    (name . ,(cdr (assq 'name node)))
    (emoji . ,(shipit-discussion--shortcode-to-emoji
               (cdr (assq 'emoji node))))
    (description . ,(cdr (assq 'description node)))
    (is_answerable . ,(shipit-discussion--normalize-bool
                       (cdr (assq 'isAnswerable node))))))

;;; GraphQL Queries

(defconst shipit-discussion--fetch-query
  "query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    discussion(number: $number) {
      id number title body
      author { login avatarUrl }
      category { name emoji isAnswerable }
      isAnswered
      answer { id body author { login avatarUrl } createdAt }
      upvoteCount viewerHasUpvoted locked
      createdAt updatedAt
      labels(first: 20) { nodes { name color } }
      reactions(first: 10) {
        nodes { content user { login } }
      }
      comments(first: 50) {
        totalCount
        nodes {
          id body
          author { login avatarUrl }
          createdAt updatedAt
          isAnswer upvoteCount viewerHasUpvoted
          reactions(first: 10) {
            nodes { content user { login } }
          }
          replies(first: 20) {
            nodes {
              id body
              author { login avatarUrl }
              createdAt updatedAt
              reactions(first: 10) {
                nodes { content user { login } }
              }
            }
          }
        }
      }
    }
  }
}"
  "GraphQL query to fetch a single discussion with comments.")

(defconst shipit-discussion--search-query
  "query($query: String!, $first: Int!) {
  search(query: $query, type: DISCUSSION, first: $first) {
    nodes {
      ... on Discussion {
        id number title
        author { login avatarUrl }
        category { name emoji isAnswerable }
        isAnswered upvoteCount
        comments { totalCount }
        createdAt updatedAt
      }
    }
    pageInfo { hasNextPage endCursor }
  }
}"
  "GraphQL query to search discussions.")

(defconst shipit-discussion--categories-query
  "query($owner: String!, $name: String!) {
  repository(owner: $owner, name: $name) {
    discussionCategories(first: 25) {
      nodes {
        id name emoji description isAnswerable
      }
    }
  }
}"
  "GraphQL query to fetch discussion categories.")

(defconst shipit-discussion--repo-id-query
  "query($owner: String!, $name: String!) {
  repository(owner: $owner, name: $name) { id }
}"
  "GraphQL query to get repository node ID.")

(defconst shipit-discussion--create-mutation
  "mutation($repositoryId: ID!, $title: String!, $body: String!, $categoryId: ID!) {
  createDiscussion(input: {
    repositoryId: $repositoryId
    title: $title
    body: $body
    categoryId: $categoryId
  }) {
    discussion {
      id number title body
      author { login avatarUrl }
      category { name emoji isAnswerable }
      isAnswered answer { id body }
      upvoteCount locked
      createdAt updatedAt
      labels(first: 20) { nodes { name color } }
      comments { totalCount }
    }
  }
}"
  "GraphQL mutation to create a discussion.")

(defconst shipit-discussion--add-comment-mutation
  "mutation($discussionId: ID!, $body: String!) {
  addDiscussionComment(input: {
    discussionId: $discussionId
    body: $body
  }) {
    comment {
      id body
      author { login avatarUrl }
      createdAt updatedAt
      isAnswer upvoteCount
      reactions(first: 100) { nodes { content user { login } } }
      replies(first: 100) {
        nodes {
          id body
          author { login avatarUrl }
          createdAt updatedAt
          reactions(first: 100) { nodes { content user { login } } }
        }
      }
    }
  }
}"
  "GraphQL mutation to add a top-level comment.")

(defconst shipit-discussion--reply-mutation
  "mutation($discussionId: ID!, $replyToId: ID!, $body: String!) {
  addDiscussionComment(input: {
    discussionId: $discussionId
    replyToId: $replyToId
    body: $body
  }) {
    comment {
      id body
      author { login avatarUrl }
      createdAt updatedAt
      reactions(first: 100) { nodes { content user { login } } }
    }
  }
}"
  "GraphQL mutation to reply to a comment.")

(defconst shipit-discussion--mark-answer-mutation
  "mutation($commentId: ID!) {
  markDiscussionCommentAsAnswer(input: { id: $commentId }) {
    discussion { isAnswered }
  }
}"
  "GraphQL mutation to mark a comment as the answer.")

;;; API Functions

(defun shipit-discussion--split-repo (repo)
  "Split REPO string \"owner/name\" into (owner . name) cons."
  (let ((parts (split-string repo "/")))
    (cons (car parts) (cadr parts))))

(defun shipit-discussion--fetch (repo number)
  "Fetch discussion NUMBER from REPO and return normalized alist."
  (let* ((parts (shipit-discussion--split-repo repo))
         (variables `((owner . ,(car parts))
                      (name . ,(cdr parts))
                      (number . ,number)))
         (result (shipit--graphql-query
                  shipit-discussion--fetch-query variables))
         (discussion (cdr (assq 'discussion
                                (cdr (assq 'repository result))))))
    (shipit--debug-log "Fetched discussion #%s from %s" number repo)
    (when discussion
      (let* ((normalized (shipit-discussion--normalize discussion))
             (comment-nodes (cdr (assq 'nodes
                                       (cdr (assq 'comments discussion)))))
             (comments (when (and comment-nodes (> (length comment-nodes) 0))
                         (mapcar #'shipit-discussion--normalize-comment
                                 (append comment-nodes nil)))))
        (append normalized `((comments . ,comments)))))))

(defun shipit-discussion--fetch-async (repo number callback)
  "Fetch discussion NUMBER from REPO asynchronously.
Calls CALLBACK with the normalized discussion alist."
  (let* ((parts (shipit-discussion--split-repo repo))
         (variables `((owner . ,(car parts))
                      (name . ,(cdr parts))
                      (number . ,number))))
    (shipit--debug-log "Fetching discussion #%s from %s async" number repo)
    (shipit--graphql-request
     shipit-discussion--fetch-query variables
     (lambda (response)
       (let* ((data (cdr (assq 'data response)))
              (discussion (cdr (assq 'discussion
                                     (cdr (assq 'repository data))))))
         (when discussion
           (let* ((normalized (shipit-discussion--normalize discussion))
                  (comment-nodes (cdr (assq 'nodes
                                            (cdr (assq 'comments discussion)))))
                  (comments (when (and comment-nodes
                                       (> (length comment-nodes) 0))
                              (mapcar #'shipit-discussion--normalize-comment
                                      (append comment-nodes nil)))))
             (funcall callback
                      (append normalized `((comments . ,comments)))))))))))

(defun shipit-discussion--build-search-query (args repo)
  "Build a GraphQL search query string from transient ARGS for REPO."
  (let ((parts (list (format "repo:%s" repo))))
    (dolist (arg args)
      (cond
       ((string-prefix-p "--category=" arg)
        (let ((val (substring arg 11)))
          (when (> (length val) 0)
            (push (format "category:\"%s\"" val) parts))))
       ((string-prefix-p "--answered=" arg)
        (let ((val (substring arg 11)))
          (cond
           ((string= val "yes") (push "is:answered" parts))
           ((string= val "no") (push "is:unanswered" parts)))))
       ((string-prefix-p "--author=" arg)
        (let ((val (substring arg 9)))
          (when (> (length val) 0)
            (push (format "author:%s" val) parts))))
       ((string-prefix-p "--label=" arg)
        (let ((val (substring arg 8)))
          (when (> (length val) 0)
            (push (format "label:\"%s\"" val) parts))))
       ((string-prefix-p "--title=" arg)
        (let ((val (substring arg 8)))
          (when (> (length val) 0)
            (push (format "%s in:title" val) parts))))
       ((string-prefix-p "--body=" arg)
        (let ((val (substring arg 7)))
          (when (> (length val) 0)
            (push (format "%s in:body" val) parts))))
       ((string-prefix-p "--created-after=" arg)
        (let ((val (substring arg 16)))
          (when (> (length val) 0)
            (push (format "created:>%s" val) parts))))
       ((string-prefix-p "--created-before=" arg)
        (let ((val (substring arg 17)))
          (when (> (length val) 0)
            (push (format "created:<%s" val) parts))))
       ((string-prefix-p "--updated-after=" arg)
        (let ((val (substring arg 16)))
          (when (> (length val) 0)
            (push (format "updated:>%s" val) parts))))
       ((string-prefix-p "--updated-before=" arg)
        (let ((val (substring arg 17)))
          (when (> (length val) 0)
            (push (format "updated:<%s" val) parts))))))
    (string-join (nreverse parts) " ")))

(defun shipit-discussion--extract-limit-from-args (args)
  "Extract the --limit=N value from ARGS, default 50."
  (let ((limit 50))
    (dolist (arg args limit)
      (when (string-prefix-p "--limit=" arg)
        (setq limit (string-to-number (substring arg 8)))))))

(defun shipit-discussion--search (repo args)
  "Search discussions in REPO with transient ARGS.
Returns a list of normalized discussion alists."
  (let* ((query-str (shipit-discussion--build-search-query args repo))
         (limit (shipit-discussion--extract-limit-from-args args))
         (variables `((query . ,query-str)
                      (first . ,(min 50 limit))))
         (result (shipit--graphql-query
                  shipit-discussion--search-query variables))
         (search-data (cdr (assq 'search result)))
         (nodes (cdr (assq 'nodes search-data))))
    (shipit--debug-log "Discussion search: query=%s results=%d"
                       query-str (length nodes))
    (when (and nodes (> (length nodes) 0))
      (mapcar #'shipit-discussion--normalize (append nodes nil)))))

(defun shipit-discussion--fetch-categories (repo)
  "Fetch discussion categories for REPO.  Cached."
  (or (gethash repo shipit-discussion--categories-cache)
      (let* ((parts (shipit-discussion--split-repo repo))
             (variables `((owner . ,(car parts))
                          (name . ,(cdr parts))))
             (result (shipit--graphql-query
                      shipit-discussion--categories-query variables))
             (nodes (cdr (assq 'nodes
                               (cdr (assq 'discussionCategories
                                          (cdr (assq 'repository result)))))))
             (categories (when (and nodes (> (length nodes) 0))
                           (mapcar #'shipit-discussion--normalize-category
                                   (append nodes nil)))))
        (puthash repo categories shipit-discussion--categories-cache)
        (shipit--debug-log "Fetched %d discussion categories for %s"
                           (length categories) repo)
        categories)))

(defun shipit-discussion--fetch-repo-id (repo)
  "Fetch the GraphQL node ID for REPO.  Cached."
  (or (gethash repo shipit-discussion--repo-id-cache)
      (let* ((parts (shipit-discussion--split-repo repo))
             (variables `((owner . ,(car parts))
                          (name . ,(cdr parts))))
             (result (shipit--graphql-query
                      shipit-discussion--repo-id-query variables))
             (repo-id (cdr (assq 'id (cdr (assq 'repository result))))))
        (puthash repo repo-id shipit-discussion--repo-id-cache)
        (shipit--debug-log "Fetched repo ID for %s: %s" repo repo-id)
        repo-id)))

(defun shipit-discussion--create (repo title body category-id)
  "Create a discussion in REPO with TITLE, BODY, and CATEGORY-ID.
Returns the normalized discussion alist."
  (let* ((repo-id (shipit-discussion--fetch-repo-id repo))
         (variables `((repositoryId . ,repo-id)
                      (title . ,title)
                      (body . ,body)
                      (categoryId . ,category-id)))
         (result (shipit--graphql-query
                  shipit-discussion--create-mutation variables))
         (discussion (cdr (assq 'discussion
                                (cdr (assq 'createDiscussion result))))))
    (shipit--debug-log "Created discussion in %s: #%s"
                       repo (cdr (assq 'number discussion)))
    (when discussion
      (shipit-discussion--normalize discussion))))

(defun shipit-discussion--add-comment (discussion-id body)
  "Add a top-level comment with BODY to discussion DISCUSSION-ID.
Returns the normalized comment alist."
  (let* ((variables `((discussionId . ,discussion-id)
                      (body . ,body)))
         (result (shipit--graphql-query
                  shipit-discussion--add-comment-mutation variables))
         (comment (cdr (assq 'comment
                              (cdr (assq 'addDiscussionComment result))))))
    (shipit--debug-log "Added comment to discussion %s" discussion-id)
    (when comment
      (shipit-discussion--normalize-comment comment))))

(defun shipit-discussion--reply-to-comment (discussion-id reply-to-id body)
  "Reply with BODY to comment REPLY-TO-ID in discussion DISCUSSION-ID.
Returns the normalized reply alist."
  (let* ((variables `((discussionId . ,discussion-id)
                      (replyToId . ,reply-to-id)
                      (body . ,body)))
         (result (shipit--graphql-query
                  shipit-discussion--reply-mutation variables))
         (comment (cdr (assq 'comment
                              (cdr (assq 'addDiscussionComment result))))))
    (shipit--debug-log "Replied to comment %s in discussion %s"
                       reply-to-id discussion-id)
    (when comment
      (shipit-discussion--normalize-reply comment))))

(defun shipit-discussion--mark-as-answer (comment-id)
  "Mark comment COMMENT-ID as the accepted answer."
  (let* ((variables `((commentId . ,comment-id)))
         (result (shipit--graphql-query
                  shipit-discussion--mark-answer-mutation variables)))
    (shipit--debug-log "Marked comment %s as answer" comment-id)
    result))

(defun shipit-discussion--toggle-upvote (subject-id has-upvoted)
  "Toggle upvote on SUBJECT-ID.
If HAS-UPVOTED is non-nil, removes the upvote; otherwise adds it."
  (let* ((mutation (if has-upvoted
                      "mutation($id: ID!) {
  removeUpvote(input: {subjectId: $id}) {
    subject { ... on Discussion { upvoteCount viewerHasUpvoted }
              ... on DiscussionComment { upvoteCount viewerHasUpvoted } }
  }
}"
                    "mutation($id: ID!) {
  addUpvote(input: {subjectId: $id}) {
    subject { ... on Discussion { upvoteCount viewerHasUpvoted }
              ... on DiscussionComment { upvoteCount viewerHasUpvoted } }
  }
}"))
         (variables `((id . ,subject-id)))
         (result (shipit--graphql-query mutation variables)))
    (shipit--debug-log "%s upvote on %s"
                       (if has-upvoted "Removed" "Added") subject-id)
    result))

(defun shipit-discussion--rest-to-graphql-reaction (content)
  "Convert REST API reaction CONTENT to GraphQL ReactionContent enum."
  (pcase content
    ("+1" "THUMBS_UP")
    ("-1" "THUMBS_DOWN")
    ("laugh" "LAUGH")
    ("hooray" "HOORAY")
    ("confused" "CONFUSED")
    ("heart" "HEART")
    ("rocket" "ROCKET")
    ("eyes" "EYES")
    (_ (upcase content))))

(defun shipit-discussion--add-reaction (subject-id content)
  "Add reaction CONTENT to discussion subject SUBJECT-ID.
CONTENT is REST format (e.g., \"+1\", \"heart\")."
  (let* ((mutation "mutation($subjectId: ID!, $content: ReactionContent!) {
  addReaction(input: {subjectId: $subjectId, content: $content}) {
    reaction { id content user { login } }
  }
}")
         (variables `((subjectId . ,subject-id)
                      (content . ,(shipit-discussion--rest-to-graphql-reaction
                                   content))))
         (result (shipit--graphql-query mutation variables)))
    (shipit--debug-log "Added reaction %s to %s" content subject-id)
    result))

(defun shipit-discussion--remove-reaction (subject-id content)
  "Remove reaction CONTENT from discussion subject SUBJECT-ID.
CONTENT is REST format (e.g., \"+1\", \"heart\")."
  (let* ((mutation "mutation($subjectId: ID!, $content: ReactionContent!) {
  removeReaction(input: {subjectId: $subjectId, content: $content}) {
    reaction { id content }
  }
}")
         (variables `((subjectId . ,subject-id)
                      (content . ,(shipit-discussion--rest-to-graphql-reaction
                                   content))))
         (result (shipit--graphql-query mutation variables)))
    (shipit--debug-log "Removed reaction %s from %s" content subject-id)
    result))

;;; Backend registration

(shipit-discussion-register-backend
 'github
 (list :name "GitHub"
       :browse-url (lambda (config number)
                     (format "https://github.com/%s/discussions/%s"
                             (plist-get config :repo) number))
       :browse-repo-url (lambda (config)
                          (format "https://github.com/%s"
                                  (plist-get config :repo)))))

(provide 'shipit-discussions-graphql)
;;; shipit-discussions-graphql.el ends here
