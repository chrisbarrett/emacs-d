.PHONY: test test-quick setup-hooks help build-affected

help:
	@echo "Targets:"
	@echo "  test           - Run all quality gates (tests, byte-compile, checkdoc)"
	@echo "  test-quick     - Run ERT tests for affected files only"
	@echo "  build-affected - Byte-compile transitively affected files"
	@echo "  setup-hooks    - Install pre-commit hooks"

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

build-affected:
	@affected=$$(./scripts/affected.sh); \
	if [ "$$affected" = "none" ]; then \
		echo "No affected files to compile"; \
	elif [ "$$affected" = "all" ]; then \
		./scripts/byte-compile.sh; \
	else \
		./scripts/byte-compile.sh $$affected; \
	fi

setup-hooks:
	@if [ ! -f .git/hooks/pre-commit ]; then \
		printf '%s\n' \
			'#!/usr/bin/env bash' \
			'exec nix-shell -p pre-commit --run "pre-commit run --hook-stage pre-commit"' \
			> .git/hooks/pre-commit; \
		chmod +x .git/hooks/pre-commit; \
		echo "Pre-commit hook installed"; \
	fi
