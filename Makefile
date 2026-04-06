# Makefile for Shipit

EMACS = emacs
PACKAGE_NAME = shipit
PACKAGE_FILE = lisp/$(PACKAGE_NAME).el
LISP_FILES = lisp/*.el
AUTOLOADS_FILE = $(PACKAGE_NAME)-autoloads.el

.PHONY: all compile clean test install autoloads package

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
		-f batch-byte-compile $(LISP_FILES)

# Generate autoloads
autoloads: $(PACKAGE_FILE)
	$(EMACS) -batch -Q \
		-eval "(require 'package)" \
		-eval "(package-generate-autoloads '$(PACKAGE_NAME)' \"lisp\")"

# Clean compiled files
clean:
	rm -f lisp/*.elc $(AUTOLOADS_FILE)
	rm -rf .packages
	rm -rf ~/.emacs.d/eln-cache/*shipit*

# Run unit tests
test-unit: compile
	$(EMACS) -batch -Q \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
		-eval "(package-initialize)" \
		-eval "(unless (package-installed-p 'dash) (package-install 'dash))" \
		-eval "(unless (package-installed-p 'transient) (package-install 'transient))" \
		-L lisp \
		-L test \
		-l test/test-shipit.el \
		-l test/test-shipit-editor.el \
		-l test/test-shipit-issues.el \
		-l test/test-shipit-issue-backends.el \
		-l test/test-shipit-issue-github.el \
		-l test/test-shipit-issue-jira.el \
		-l test/test-shipit-issue-render.el \
		-l test/test-shipit-issue-create.el \
		-l test/test-shipit-issue-create-buffer.el \
		-l test/test-shipit-issue-comment-actions.el \
		-l test/test-notifications-types.el \
		-l test/test-jira-fields-transitions.el \
		-l test/test-shipit-discussions-graphql.el \
		-l test/test-shipit-discussions-buffer.el \
		-l test/test-shipit-discussions.el \
		-l test/test-shipit-pr-backends.el \
		-l test/test-shipit-comment-backends.el \
		-l test/test-shipit-gitlab-http.el \
		-l test/test-shipit-pr-gitlab.el \
		-l test/test-shipit-issue-gitlab.el \
		-l test/test-shipit-comment-gitlab.el \
		-l test/test-shipit-render.el \
		-l test/test-shipit-pr-actions.el \
		-l test/test-shipit-repo-buffer.el \
		-l test/test-shipit-atlassian-dashboard.el \
		-l test/test-shipit-issues-buffer.el \
		-l test/test-shipit-open-url.el \
		-l test/test-shipit-suggestions.el \
		-l test/test-shipit-notifications-rss.el \
		-l test/test-shipit-actions-timestamps.el \
		-l test/test-shipit-merge.el \
		-l test/test-shipit-repo-subscription.el \
		-l test/test-shipit-subscriptions.el \
		-f ert-run-tests-batch-and-exit

# Run integration tests
test-integration: compile
	$(EMACS) -batch -Q \
		-eval "(setq package-user-dir \"$(PWD)/.packages\")" \
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
