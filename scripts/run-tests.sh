#!/usr/bin/env bash
# Run ERT tests.
# Usage:
#   ./run-tests.sh              # Run all tests
#   ./run-tests.sh --affected   # Run tests for affected files only
#   ./run-tests.sh file1 file2  # Run specific test files
set -euo pipefail

cd "$(dirname "$0")/.."

# Handle --affected flag
if [[ $# -eq 1 && "$1" == "--affected" ]]; then
  mapfile -t affected < <(./scripts/affected-tests.sh)
  if [[ ${#affected[@]} -eq 0 ]]; then
    echo "No affected test files"
    exit 0
  fi
  if [[ "${affected[0]}" == "all" ]]; then
    exec emacs -Q --batch -l tests.el
  fi
  if [[ "${affected[0]}" == "none" ]]; then
    echo "No tests to run for affected files"
    exit 0
  fi
  # Run only affected test files
  set -- "${affected[@]}"
fi

if [[ $# -eq 0 ]]; then
  exec emacs -Q --batch -l tests.el
else
  # Run specific test files by loading them directly
  args=()
  for file in "$@"; do
    if [[ -f "$file" ]]; then
      args+=(-l "$file")
    fi
  done
  if [[ ${#args[@]} -eq 0 ]]; then
    echo "No valid test files found"
    exit 0
  fi
  # Set up load paths via test-setup.el, then load specific test files and run tests.
  exec emacs -Q --batch \
    -l scripts/test-setup.el \
    "${args[@]}" \
    --eval "(ert-run-tests-batch-and-exit t)"
fi
