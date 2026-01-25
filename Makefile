.PHONY: test test-quick setup-hooks help

help:
	@echo "Targets:"
	@echo "  test        - Run all quality gates (tests, byte-compile, checkdoc)"
	@echo "  test-quick  - Run ERT tests for affected files only"
	@echo "  setup-hooks - Install pre-commit hooks"

test: setup-hooks
	nix-shell -p pre-commit --run "pre-commit run --all-files"

test-quick:
	@affected=$$(./scripts/affected-tests.sh); \
	if [ "$$affected" = "none" ]; then \
		echo "No affected test files"; \
	elif [ "$$affected" = "all" ]; then \
		./scripts/run-tests.sh; \
	else \
		./scripts/run-tests.sh $$affected; \
	fi

setup-hooks:
	@if [ ! -f .git/hooks/pre-commit ]; then \
		./scripts/setup-hooks.sh; \
	fi
