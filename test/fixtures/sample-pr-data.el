;;; sample-pr-data.el --- Recorded PR data for testing -*- lexical-binding: t; -*-

;;; Commentary:
;; Real PR data recorded from GitHub API for use in tests.
;; This provides realistic test fixtures instead of minimal mock data.

;;; Code:

(defvar test-recorded-pr-data
  '((number . 42)
    (title . "Add machine learning model with optimized backend")
    (body . "This PR adds a new neural network model implementation with optimized backend for improved performance.

## Changes
- Added model architecture in `src/models/network.py`
- Implemented optimized backend for convolution operations
- Added comprehensive unit tests
- Updated documentation

## Performance
- 2.3x speedup on inference
- 15% reduction in memory usage

## Testing
- All existing tests pass
- New unit tests added for network model
- Integration tests included")
    (state . "open")
    (user . ((login . "alice-dev")
             (id . 12345)
             (avatar_url . "https://avatars.githubusercontent.com/u/12345?v=4")))
    (assignees . (((login . "bob-reviewer")
                   (id . 67890)
                   (avatar_url . "https://avatars.githubusercontent.com/u/67890?v=4"))
                  ((login . "alice-dev")
                   (id . 12345)
                   (avatar_url . "https://avatars.githubusercontent.com/u/12345?v=4"))))
    (labels . (((name . "enhancement")
                (color . "a2eeef")
                (description . "New feature or request"))
               ((name . "performance")
                (color . "d4c5f9")
                (description . "Performance improvements"))
               ((name . "needs-review")
                (color . "fbca04")
                (description . "Waiting for code review"))))
    (head . ((sha . "abc123def456789")
             (ref . "feature/ml-model-optimization")
             (repo . ((name . "awesome-project")
                      (full_name . "example-org/awesome-project")))))
    (base . ((ref . "main")
             (sha . "def456789abc123")
             (repo . ((name . "awesome-project")
                      (full_name . "example-org/awesome-project")))))
    (created_at . "2025-01-15T10:30:00Z")
    (updated_at . "2025-01-15T14:22:00Z")
    (merged_at . nil)
    (merge_commit_sha . nil)
    (mergeable . t)
    (mergeable_state . "clean")
    (draft . nil)
    (commits . 8)
    (additions . 342)
    (deletions . 28)
    (changed_files . 12)))

(defvar test-recorded-comments-data
  '(;; General comments (PR conversation)
    ((id . 2275989375)
     (body . "This looks great! The performance improvements are impressive. I have a few questions about the optimized backend implementation though.")
     (user . ((login . "charlie-reviewer")
              (id . 98765)
              (avatar_url . "https://avatars.githubusercontent.com/u/98765?v=4")))
     (created_at . "2025-01-15T11:15:00Z")
     (updated_at . "2025-01-15T11:15:00Z")
     (path . nil)
     (line . nil)
     (diff_hunk . nil)
     (in_reply_to_id . nil))
    
    ;; Inline comments on specific lines
    ((id . 2276136007)
     (body . "An alternative to this transpose is to update `conv2d_transpose` function to index differently in the weights buffer:

```py
weights[dim.x, dim.y, out_ch, dim.z],
```

I don't know which one is correct though. What shape of weights makes sense for the `conv2d_transpose` function?")
     (user . ((login . "charlie-reviewer")
              (id . 98765)
              (avatar_url . "https://avatars.githubusercontent.com/u/98765?v=4")))
     (created_at . "2025-01-15T12:30:00Z")
     (updated_at . "2025-01-15T12:30:00Z")
     (path . "src/models/network.py")
     (line . 212)
     (start_line . 210)
     (diff_hunk . "@@ -207,8 +207,12 @@ def conv2d_transpose(input, weights, bias=None):\n     # Implementation of transposed convolution\n     height, width, in_channels, batch_size = input.shape\n     out_channels, kernel_h, kernel_w, in_ch = weights.shape\n+    \n+    # Transpose weights for proper indexing\n+    weights_t = weights.transpose()\n+    \n     # Create output buffer\n     output = create_buffer([height*2, width*2, out_channels, batch_size])")
     (in_reply_to_id . nil))
    
    ((id . 2276158827)
     (body . "I thought about doing that as well, can look into it")
     (user . ((login . "dana-contributor")
              (id . 54321)
              (avatar_url . "https://avatars.githubusercontent.com/u/54321?v=4")))
     (created_at . "2025-01-15T14:22:00Z")
     (updated_at . "2025-01-15T14:22:00Z")
     (path . "src/models/network.py")
     (line . 212)
     (start_line . 210)
     (diff_hunk . "@@ -207,8 +207,12 @@ def conv2d_transpose(input, weights, bias=None):\n     # Implementation of transposed convolution\n     height, width, in_channels, batch_size = input.shape\n     out_channels, kernel_h, kernel_w, in_ch = weights.shape\n+    \n+    # Transpose weights for proper indexing\n+    weights_t = weights.transpose()\n+    \n     # Create output buffer\n     output = create_buffer([height*2, width*2, out_channels, batch_size])")
     (in_reply_to_id . nil))
    
    ;; Comments on different files
    ((id . 2276116474)
     (body . "Could we add a docstring here explaining the expected input shape? It would help with debugging tensor shape mismatches.")
     (user . ((login . "charlie-reviewer")
              (id . 98765)
              (avatar_url . "https://avatars.githubusercontent.com/u/98765?v=4")))
     (created_at . "2025-01-15T11:45:00Z")
     (updated_at . "2025-01-15T11:45:00Z")
     (path . "src/models/network.py")
     (line . 168)
     (start_line . 167)
     (diff_hunk . "@@ -164,6 +164,8 @@ class NetworkModel:\n         \"\"\"\n         Initialize network model with optimized backend.\n         \"\"\"\n+        # TODO: Add input validation\n+        self.backend = OptimizedBackend()\n         self.encoder_layers = []")
     (in_reply_to_id . nil))
    
    ((id . 2275990123)
     (body . "Good catch! I'll add comprehensive docstrings for all public methods.")
     (user . ((login . "alice-dev")
              (id . 12345)
              (avatar_url . "https://avatars.githubusercontent.com/u/12345?v=4")))
     (created_at . "2025-01-15T13:10:00Z")
     (updated_at . "2025-01-15T13:10:00Z")
     (path . "src/models/network.py")
     (line . 168)
     (start_line . 167)
     (diff_hunk . "@@ -164,6 +164,8 @@ class NetworkModel:\n         \"\"\"\n         Initialize network model with optimized backend.\n         \"\"\"\n+        # TODO: Add input validation\n+        self.backend = OptimizedBackend()\n         self.encoder_layers = []")
     (in_reply_to_id . 2276116474))))

(defvar test-recorded-reviews-data
  '(((id . 8901234)
     (user . ((login . "charlie-reviewer")
              (id . 98765)
              (avatar_url . "https://avatars.githubusercontent.com/u/98765?v=4")))
     (state . "CHANGES_REQUESTED")
     (body . "Great work on the performance improvements! I have a few suggestions for the optimized backend implementation. Please address the inline comments about weight tensor indexing.")
     (submitted_at . "2025-01-15T12:45:00Z"))
    
    ((id . 8901235)
     (user . ((login . "bob-reviewer")
              (id . 67890)
              (avatar_url . "https://avatars.githubusercontent.com/u/67890?v=4")))
     (state . "APPROVED")
     (body . "LGTM! The test coverage is excellent and the performance gains are substantial.")
     (submitted_at . "2025-01-15T15:30:00Z"))))

(defvar test-recorded-checks-data
  '(((name . "continuous-integration/github-actions")
     (status . "completed")
     (conclusion . "success")
     (started_at . "2025-01-15T10:31:00Z")
     (completed_at . "2025-01-15T10:45:00Z")
     (html_url . "https://github.com/example-org/awesome-project/actions/runs/123456"))
    
    ((name . "codecov/patch")
     (status . "completed")
     (conclusion . "success")
     (started_at . "2025-01-15T10:32:00Z")
     (completed_at . "2025-01-15T10:44:00Z")
     (html_url . "https://codecov.io/gh/example-org/awesome-project/pull/42"))
    
    ((name . "security/codeql")
     (status . "completed")
     (conclusion . "success")
     (started_at . "2025-01-15T10:31:00Z")
     (completed_at . "2025-01-15T10:50:00Z")
     (html_url . "https://github.com/example-org/awesome-project/security/code-scanning"))
    
    ((name . "performance-tests")
     (status . "completed")
     (conclusion . "success")
     (started_at . "2025-01-15T10:35:00Z")
     (completed_at . "2025-01-15T11:15:00Z")
     (html_url . "https://github.com/example-org/awesome-project/actions/runs/123457"))))

(provide 'sample-pr-data)
;;; sample-pr-data.el ends here