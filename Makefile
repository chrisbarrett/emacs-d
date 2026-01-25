.PHONY: test test-quick setup-hooks help

help:
	@echo "Targets:"
	@echo "  test        - Run all quality gates (tests, byte-compile, checkdoc)"
	@echo "  test-quick  - Run ERT tests only"
	@echo "  setup-hooks - Install pre-commit hooks"

test: setup-hooks
	nix-shell -p pre-commit --run "pre-commit run --all-files"

test-quick:
	./scripts/run-tests.sh

setup-hooks:
	@if [ ! -f .git/hooks/pre-commit ]; then \
		./scripts/setup-hooks.sh; \
	fi
