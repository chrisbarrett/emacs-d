#!/usr/bin/env bash
# Run ERT tests.
# Usage:
#   ./run-tests.sh              # Run all tests
#   ./run-tests.sh file1 file2  # Run specific test files
set -euo pipefail

cd "$(dirname "$0")/.."

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
  exec emacs -Q --batch \
    --eval "(require 'ert)" \
    -l tests.el \
    --eval "(setq command-line-args-left nil)" \
    "${args[@]}" \
    --eval "(ert-run-tests-batch-and-exit t)"
fi
