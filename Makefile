.PHONY: test test-quick setup-hooks help build-affected test-affected lint-affected pre-commit

# Module lib files that contribute to autoloads
MODULE_LIB_FILES := $(wildcard modules/*/lib.el) \
                    $(wildcard modules/*/*-lib.el) \
                    $(wildcard modules/*/lib/*.el)

help:
	@echo "Targets:"
	@echo "  test           - Run all quality gates (tests, byte-compile, checkdoc)"
	@echo "  test-quick     - Run ERT tests for affected files only"
	@echo "  build-affected - Byte-compile transitively affected files"
	@echo "  test-affected  - Run ERT tests for transitively affected files"
	@echo "  lint-affected  - Run checkdoc on directly affected files"
	@echo "  pre-commit     - Run lint-affected, build-affected, test-affected in sequence"
	@echo "  setup-hooks    - Install prek hooks"

lisp/+autoloads.el: $(MODULE_LIB_FILES)
	@echo "Generating $@..."
	@emacs -Q --batch \
		--eval "(add-to-list 'load-path \"$(CURDIR)/lisp\")" \
		--eval "(require '+modules)" \
		--eval "(+modules-write-autoloads (+modules-collect-autoloads))"

test: setup-hooks lisp/+autoloads.el
	./scripts/run-tests.sh
	./scripts/byte-compile.sh
	./scripts/checkdoc.sh

test-quick: lisp/+autoloads.el
	@affected=$$(./scripts/affected-tests.sh); \
	if [ "$$affected" = "none" ]; then \
		echo "No affected test files"; \
	elif [ "$$affected" = "all" ]; then \
		./scripts/run-tests.sh; \
	else \
		./scripts/run-tests.sh $$affected; \
	fi

build-affected: lisp/+autoloads.el
	@affected=$$(./scripts/affected.sh); \
	if [ "$$affected" = "none" ]; then \
		echo "No affected files to compile"; \
	elif [ "$$affected" = "all" ]; then \
		./scripts/byte-compile.sh; \
	else \
		./scripts/byte-compile.sh $$affected; \
	fi

test-affected: lisp/+autoloads.el
	@affected=$$(./scripts/affected.sh); \
	if [ "$$affected" = "none" ]; then \
		echo "No affected test files"; \
	elif [ "$$affected" = "all" ]; then \
		./scripts/run-tests.sh; \
	else \
		test_files=""; \
		for src in $$affected; do \
			test_file=$${src%.el}-tests.el; \
			if [ -f "$$test_file" ]; then \
				test_files="$$test_files $$test_file"; \
			fi; \
		done; \
		if [ -n "$$test_files" ]; then \
			./scripts/run-tests.sh $$test_files; \
		else \
			echo "No test files for affected sources"; \
		fi; \
	fi

lint-affected:
	@files=$$({ git diff --cached --name-only --diff-filter=ACMR 2>/dev/null; \
		git diff --name-only --diff-filter=ACMR 2>/dev/null; } | \
		sort -u | grep -E '^(lisp|lib)/.*\.el$$' | grep -v -- '-tests\.el$$' || true); \
	if [ -z "$$files" ]; then \
		echo "No affected files to lint"; \
	else \
		./scripts/checkdoc.sh $$files; \
	fi

pre-commit: lisp/+autoloads.el lint-affected build-affected test-affected

setup-hooks:
	@if [ ! -f .git/hooks/pre-commit ]; then \
		nix develop --command prek install; \
	fi
