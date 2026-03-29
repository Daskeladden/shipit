# Shipit

Code review in Emacs. View pull requests, post comments, track CI, and manage notifications without leaving your editor.

![PR Buffer](screenshots/pr-buffer.png)

This project was built as a learning exercise in agentic AI-assisted programming using [Claude Code](https://claude.ai/claude-code).

**GitHub** is the primary backend with full feature support. **GitLab** and **Jira** backends are available but considered experimental.

## What it does

- **Pull requests** — view description, commits, file changes, labels, assignees, reviewers
- **Comments** — inline diff comments with threading, general comments, reactions, live markdown preview
- **Reviews** — submit approvals, request changes, resolve conversations
- **CI/Checks** — expand check suites to see step logs with timestamp gap coloring
- **Notifications** — poll for updates, see activity that triggered each notification, mark as read
- **Issues** — search, create, and view issues with status transitions and assignee management
- **Discussions** — view and participate in GitHub Discussions
- **Worktrees** — checkout PR branches into isolated worktrees

Supports SVG icons, inline images, avatars, and syntax-highlighted code blocks.

## Install

Emacs 27.1+, [magit](https://magit.vc/), [transient](https://github.com/magit/transient).

Shipit builds on magit-section for all buffer rendering and navigation, and transient for menus and actions.

```elisp
;; straight.el
(use-package shipit
  :straight (:host github :repo "Daskeladden/shipit" :files ("lisp/*.el"))
  :after magit
  :config
  (shipit-init))

;; elpaca
(use-package shipit
  :elpaca (:host github :repo "Daskeladden/shipit" :files ("lisp/*.el"))
  :after magit
  :config
  (shipit-init))

;; manual
(add-to-list 'load-path "/path/to/shipit/lisp")
(require 'shipit)
(shipit-init)
```

## Authentication

Shipit uses Emacs auth-source for credentials. Add entries to `~/.authinfo.gpg` (or `~/.authinfo`):

```
# GitHub
machine api.github.com login your-username password ghp_...

# GitLab
machine gitlab.com login your-username password glpat-...

# Jira
machine your-instance.atlassian.net login your@email.com password your-api-token
```

GitHub tokens need `repo` scope. You can also set `shipit-github-token` directly.

For GitLab and Jira, configure backends in your init file:

```elisp
(setq shipit-issue-backend 'jira)
(setq shipit-issue-backend-config
      '(:host "https://your-instance.atlassian.net"
        :project-keys ("PROJ")))
```

Per-repo configuration via `.dir-locals.el` is also supported.

## Usage

Run `M-x shipit` from any git repo with a remote.

Navigate with `TAB` to expand/collapse, `RET` to open, and `M-;` for context actions.

## Configuration

```elisp
(setq shipit-use-svglib-icons t)              ; SVG icons (needs svg-lib)
(setq shipit-show-avatars t)                   ; show user avatars
(setq shipit-round-avatars t)                  ; round avatar images
(setq shipit-notifications-enabled t)          ; poll for notifications
(setq shipit-notifications-poll-frequency 300) ; poll interval (seconds)
(setq shipit-render-markdown t)                ; render markdown in descriptions
```

## Troubleshooting

`M-x shipit-doctor` checks your token, API access, and rate limits.

`M-x shipit-toggle-debug-logging` enables debug logging to `~/.emacs.d/shipit-debug.log`. Note: debug logging slows down PR loading — disable it when not actively debugging.

## Screenshots

**Main menu (`M-x shipit`)**

![Main Menu](screenshots/main-menu.png)

**Inline comments with threaded replies**

![Inline Comments](screenshots/inline-comments.png)

**Notifications**

![Notifications](screenshots/notifications.png)

**GitHub Actions — workflow runs with elapsed time bars**

![Actions Workflows](screenshots/actions-workflows.png)

## License

GPL-3.0-or-later
