# Changelog

## v1.1.0 (2026-04-12)

### Features

- **Thread subscriptions** — subscribe to individual PRs, Issues, and Discussions via `w` then `t` in the subscription transient
- **Pinned comments** — shown prominently in issue buffers with SVG pin icon, truncated preview, collapsible full body, and rounded background
- **Comment filters** — press `f` to filter by author, date, text search, hide bots, min reactions; filters stack with AND logic
- **Comment pagination** — large issues show first and last N comments with a Load more transient (`l`); supports recent-first and oldest-first direction toggle
- **Comment position indicator** — mode line shows current position when navigating comments
- **Issue notification timeline** — expanding an issue notification shows recent activity

### Performance

- Lazy comment fetching via Link header (2 API calls instead of N for large issues)
- Reactions fetched only for visible comments, not all
- Async description reactions
- Recursive reaction pagination with no cap
- Markdown render cache by content hash
- Last-page timeline fetch via Link header for notifications

## v1.0.0 (2026-02-16)

Initial public release.

### Features

- **Code review workflow** — submit reviews, approve or request changes, reply to comment threads, resolve conversations
- **Inline diff comments** — view and post comments on specific lines in diff buffers, with full threading support
- **Notifications** — modeline indicator with mention/total counts, dedicated notification buffer, mark as read
- **PR browsing** — description, labels, assignees, reviewers, file changes with syntax highlighting
- **Comment editor** — multi-line editing with `#` PR refs, `@` mentions, `:` emoji completion, and live markdown preview
- **CI/Checks** — view check suite results inline with hierarchical status display
- **Worktrees** — checkout PR branches into isolated worktrees with automatic context tracking
- **Issues** — search, view, and create issues with full backend support
- **Discussions** — GitHub Discussions search, viewing, and creation
- **Rich rendering** — inline images, avatars, syntax-highlighted code blocks, mermaid diagrams
- **Backend abstraction** — pluggable backend registries for PR, comment, and issue operations
