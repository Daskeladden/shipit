# Changelog

## v1.2.0 (2026-04-18)

### Features

- **Atlassian dashboard** — new `M-x shipit-atlassian-dashboard` buffer for Jira projects with My Open Issues, What's Next, Kanban Board, Frequently Visited, and a filterable Issues section. Refuses to open when the active backend isn't Jira.
- **Issues filters** — rich filter transient covering status, assignee, reporter, type, priority, resolution, component, text/comment search, sort, plus timeline filters (Updated since/before, Created since/before). Assignee, reporter, status, and component filters use dynamic completion against the Jira backend.
- **Create issue from dashboard** — `M-; n` on the dashboard opens the rich create buffer with Jira fields (issue type, components, labels, assignee) pre-wired to the dashboard's repo.
- **Linked tracker issue on PR buffer** — new "Issue" section on each PR auto-detects the key from the branch name (e.g. `ZIVID-12624-foo`) with a transient for link/clear/open/browse/copy-URL/transition/create-and-link. Manual overrides persist per (repo, pr-number) in `~/.emacs.d/shipit-pr-linked-issues.el`.
- **Activity navigation from notifications** — pressing RET on an activity line in an expanded PR notification opens the PR and jumps directly to the comment/commit/review; a pulse highlights the landing line.
- **Jira comment reactions** — issue comments render reaction rows via the internal DC/Server endpoint (Cloud stays placeholder-only, tracked by JRACLOUD-78153).
- **Shebang language detection** — code blocks without a fence language tag get syntax highlighting inferred from `#!/usr/bin/env python` style shebang lines.

### Performance

- Atlassian dashboard now fetches the four sections concurrently via the Jira async API. Render happens once at completion instead of four times (each render regenerates every SVG icon — ~5× faster end-to-end on large projects).
- Memoised Jira issue-type / priority icons; rendering ~450 rows now calls svg-lib ~10 times instead of ~450. Dashboards with many issues load in ~1 s rather than ~15 s.
- Board pagination bumped to `maxResults=100`, halving round trips.

### Fixes

- **Empty Issues section** — JQL now quotes project keys (`project in ("ZIVID")` instead of `project in (ZIVID)`); Jira's `/search/jql` endpoint rejects the unquoted form with HTTP 400.
- **Duplicated comments** — head-and-tail pagination fallback no longer renders the same comment twice when the total fits in the window.
- **Thread subscription on Jira issues** — `w` on a Jira issue buffer (string key like `ZIVID-12624`) no longer raises a wrong-type-argument error; thread subscription is GitHub-only and now elides cleanly for string IDs.
- **Stale commit sub-sections** — commit detail sub-sections are collapsed by default.

### Docs

- README: correct Jira config key (`:base-url`, not `:host`), add `shipit-issue-repo-backends` example, mention the dashboard guard, and list the new features in the top-level summary.

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
