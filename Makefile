# Makefile for Shipit

EMACS = emacs
PACKAGE_NAME = shipit
PACKAGE_FILE = lisp/$(PACKAGE_NAME).el
LISP_FILES = lisp/*.el
AUTOLOADS_FILE = $(PACKAGE_NAME)-autoloads.el

# canvas-diagram owns the canvas-cairo module the canvas views draw
# with, and canvas-graph lays out the stack canvas on top of it.
# canvas-diagram requires canvas-keys, so its checkout goes on the load
# path too.  All three are sibling checkouts, and all are optional:
# without them the canvas tests skip themselves and the rest of shipit
# is unaffected.  A missing one skips the canvas tests silently, so a
# run that skips many more than usual is worth a look.
CANVAS_DIAGRAM = ../canvas-diagram
CANVAS_GRAPH = ../canvas-graph
CANVAS_KEYS = ../canvas-keys

.PHONY: all compile clean test test-older install autoloads package

all: compile autoloads

# Compile the package
compile: $(LISP_FILES)
	$(EMACS) -batch -Q \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
		-eval "(package-initialize)" \
		-eval "(unless (package-installed-p 'dash) (package-install 'dash))" \
		-eval "(unless (package-installed-p 'transient) (package-install 'transient))" \
		-eval "(unless (package-installed-p 'magit) (package-install 'magit))" \
		-L lisp \
		-L $(CANVAS_DIAGRAM) \
		-L $(CANVAS_GRAPH) \
		-L $(CANVAS_KEYS) \
		-f batch-byte-compile $(LISP_FILES)

# Generate autoloads
# `package-generate-autoloads' was called with the package name wrapped
# in a stray trailing quote, so it produced a file under the wrong name
# and lisp/shipit-autoloads.el was never refreshed.
autoloads: $(PACKAGE_FILE)
	$(EMACS) -batch -Q \
		-eval "(require 'loaddefs-gen)" \
		-eval "(loaddefs-generate \"lisp\" \"lisp/$(AUTOLOADS_FILE)\")"

# Clean compiled files
clean:
	rm -f lisp/*.elc $(AUTOLOADS_FILE)
	rm -rf .packages
	rm -f ~/.emacs.d/eln-cache/*/shipit*.eln

# Tests run with a throwaway user-emacs-directory.  shipit keeps its
# per-user state there, read and unread marks among it, and a test that
# saves any of it would otherwise write over the session's own copy.
# They read no logins either: a lookup decrypts the auth file of the
# person running the suite, and with no passphrase cached in gpg-agent
# that waits for a prompt a batch run never answers.
TEST_HOME = -eval "(setq user-emacs-directory (file-name-as-directory (make-temp-file \"shipit-test-home-\" t)))" \
	-eval "(setq auth-sources nil)"

# Run unit tests
test-unit: compile
	$(EMACS) -batch -Q \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
		$(TEST_HOME) \
		-eval "(package-initialize)" \
		-eval "(unless (package-installed-p 'dash) (package-install 'dash))" \
		-eval "(unless (package-installed-p 'transient) (package-install 'transient))" \
		-L lisp \
		-L test \
		-L $(CANVAS_DIAGRAM) \
		-L $(CANVAS_GRAPH) \
		-L $(CANVAS_KEYS) \
		-l test/test-shipit.el \
		-l test/test-shipit-editor.el \
		-l test/test-shipit-magit-requires.el \
		-l test/test-shipit-rounded-section.el \
		-l test/test-shipit-force-push.el \
		-l test/test-shipit-diff-entry-points.el \
		-l test/test-shipit-key-hints.el \
		-l test/test-shipit-open-in-worktree.el \
		-l test/test-shipit-gh-etag-exit.el \
		-l test/test-shipit-duplicate-defuns.el \
		-l test/test-shipit-file-registration.el \
		-l test/test-shipit-requires.el \
		-l test/test-files-filter.el \
		-l test/test-shipit-review-mode-integration.el \
		-l test/test-shipit-review-revision.el \
		-l test/test-tab-collapse-fix.el \
		-l test/test-per-commit-comments.el \
		-l test/test-shipit-discussion-backends.el \
		-l test/test-shipit-review-mode.el \
		-l test/test-pr-approval-endpoint.el \
		-l test/test-shipit-pr-stack.el \
		-l test/test-shipit-stack.el \
		-l test/test-shipit-push.el \
		-l test/test-shipit-stacked-flow.el \
		-l test/test-shipit-stack-plan.el \
		-l test/test-shipit-stacked-preview.el \
		-l test/test-shipit-commit-files-local.el \
		-l test/test-shipit-stack-render.el \
		-l test/test-shipit-preview-defaults.el \
		-l test/test-shipit-issues.el \
		-l test/test-shipit-issue-backends.el \
		-l test/test-shipit-issue-github.el \
		-l test/test-shipit-issue-jira.el \
		-l test/test-shipit-jira-sandbox.el \
		-l test/test-shipit-issue-render.el \
		-l test/test-shipit-issue-eldoc.el \
		-l test/test-shipit-issue-create.el \
		-l test/test-shipit-issue-create-buffer.el \
		-l test/test-shipit-issue-comment-actions.el \
		-l test/test-notifications-types.el \
		-l test/test-jira-fields-transitions.el \
		-l test/test-shipit-discussions-graphql.el \
		-l test/test-shipit-discussions-buffer.el \
		-l test/test-shipit-magit-worktrees.el \
		-l test/test-shipit-discussions.el \
		-l test/test-shipit-pr-backends.el \
		-l test/test-shipit-pr-github.el \
		-l test/test-shipit-comment-backends.el \
		-l test/test-shipit-gitlab-http.el \
		-l test/test-shipit-pr-gitlab.el \
		-l test/test-shipit-issue-gitlab.el \
		-l test/test-shipit-comment-gitlab.el \
		-l test/test-shipit-render.el \
		-l test/test-shipit-pr-actions.el \
		-l test/test-shipit-repo-buffer.el \
		-l test/test-shipit-canvas.el \
		-l test/test-shipit-atlassian-board-shared.el \
		-l test/test-shipit-atlassian-history.el \
		-l test/test-shipit-atlassian-kanban.el \
		-l test/test-shipit-stack-canvas.el \
		-l test/test-shipit-actions-timeline.el \
		-l test/test-shipit-atlassian-dashboard.el \
		-l test/test-shipit-issues-buffer.el \
		-l test/test-shipit-issue-branch.el \
		-l test/test-shipit-open-url.el \
		-l test/test-shipit-suggestions.el \
		-l test/test-shipit-notifications-rss.el \
		-l test/test-shipit-actions-timestamps.el \
		-l test/test-shipit-merge.el \
		-l test/test-shipit-repo-subscription.el \
		-l test/test-shipit-subscriptions.el \
		-l test/test-thread-subscription.el \
		-l test/test-shipit-releases-buffer.el \
		-l test/test-shipit-commit-comments.el \
		-l test/test-shipit-http-request-guard.el \
		-l test/test-shipit-buffer-ready-hook.el \
		-l test/test-shipit-activity-dispatch.el \
		-l test/test-shipit-notifications-activity-nav.el \
		-l test/test-shipit-pulse-nav.el \
		-l test/test-shipit-expand-pr-files.el \
		-l test/test-shipit-pr-linked-issue.el \
		-l test/test-shipit-actions-check-suite.el \
		-l test/test-shipit-review-history.el \
		-l test/test-shipit-code-refs.el \
		-f ert-run-tests-batch-and-exit

# Run integration tests
test-integration: compile
	$(EMACS) -batch -Q \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
		$(TEST_HOME) \
		-eval "(package-initialize)" \
		-eval "(unless (package-installed-p 'dash) (package-install 'dash))" \
		-eval "(unless (package-installed-p 'transient) (package-install 'transient))" \
		-eval "(unless (package-installed-p 'magit) (package-install 'magit))" \
		-L lisp \
		-L test \
		-l test/test-integration.el \
		-l test/test-commit-files-section.el \
		-l test/test-pr-search-dynamic.el \
		-l test/test-notifications-integration.el \
		-l test/test-shipit-actions-list.el \
		-f ert-run-tests-batch-and-exit

# Run all tests
test: test-unit test-integration

# The whole suite on an older Emacs, one without the canvas, which shipit
# must still compile and run on.  It runs in a git worktree carrying the
# working tree's changes: the older Emacs would otherwise write its .elc
# files over this tree's and mix its packages into .packages, and some
# tests need a git repository, which a plain copy is not and .git is far
# too large to copy.  Its packages stay in .packages-older between runs.
# The worktree's make is started from inside it, because make -C leaves
# PWD pointing here and the package directory is built from PWD.
OLDER_EMACS = /usr/bin/emacs

test-older:
	@tree=$$(mktemp -d); \
	git worktree add --quiet --detach "$$tree" HEAD || { rm -rf "$$tree"; exit 1; }; \
	rsync -a --delete --exclude .git --exclude '.packages*' --exclude .worktrees \
		--exclude screenshots --exclude '*.elc' ./ "$$tree"/; \
	mkdir -p "$(CURDIR)/.packages-older"; \
	ln -s "$(CURDIR)/.packages-older" "$$tree/.packages"; \
	echo "Suite under $$($(OLDER_EMACS) --version | head -1), in $$tree"; \
	(cd "$$tree" && $(MAKE) test EMACS=$(OLDER_EMACS) \
		CANVAS_DIAGRAM=$(abspath $(CANVAS_DIAGRAM)) \
		CANVAS_GRAPH=$(abspath $(CANVAS_GRAPH)) \
		CANVAS_KEYS=$(abspath $(CANVAS_KEYS))); \
	status=$$?; git worktree remove --force "$$tree"; exit $$status

# Install locally for testing
install: compile autoloads
	@echo "To install, add the following to your Emacs config:"
	@echo "(add-to-list 'load-path \"$(PWD)/lisp\")"
	@echo "(require '$(PACKAGE_NAME))"

# Create package tarball
package: clean compile autoloads
	tar -czf $(PACKAGE_NAME)-$(shell grep "Version:" $(PACKAGE_FILE) | sed 's/.*Version: *//').tar.gz \
		--exclude='.git*' \
		--exclude='Makefile' \
		--exclude='*.tar.gz' \
		.

# Development helpers
lint: $(PACKAGE_FILE)
	$(EMACS) -batch -Q \
		-eval "(require 'package)" \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
		-eval "(package-initialize)" \
		-eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		-eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(PACKAGE_FILE)

checkdoc: $(PACKAGE_FILE)
	$(EMACS) -batch -Q \
		-eval "(checkdoc-file \"$(PACKAGE_FILE)\")"
