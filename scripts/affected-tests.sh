#!/usr/bin/env bash
# Compute which test files need to run based on changed files and dependencies.
# Output: list of test file paths, or "all" if dependencies can't be computed.
set -euo pipefail

cd "$(dirname "$0")/.."

# Get changed files relative to main branch or working tree
get_changed_files() {
  local base_ref="${1:-HEAD}"
  local files=()

  # Staged changes
  mapfile -t staged < <(git diff --cached --name-only --diff-filter=ACMR 2>/dev/null || true)
  files+=("${staged[@]}")

  # Unstaged changes
  mapfile -t unstaged < <(git diff --name-only --diff-filter=ACMR 2>/dev/null || true)
  files+=("${unstaged[@]}")

  # If comparing against a ref (e.g., main)
  if [[ "$base_ref" != "HEAD" ]]; then
    mapfile -t committed < <(git diff --name-only --diff-filter=ACMR "$base_ref"...HEAD 2>/dev/null || true)
    files+=("${committed[@]}")
  fi

  # Deduplicate and filter to .el files in lisp/ or lib/
  printf '%s\n' "${files[@]}" | sort -u | grep -E '^(lisp|lib)/.*\.el$' | grep -v -- '-tests\.el$' || true
}

# Extract require forms from a file and output feature names
extract_requires() {
  local file="$1"
  grep -oE "\\(require '([^)]+)\\)" "$file" 2>/dev/null | \
    sed -E "s/\\(require '([^)]+)\\)/\\1/" | \
    sort -u || true
}

# Map feature name to file path
feature_to_file() {
  local feature="$1"
  # Check lisp/ directory
  local lisp_file="lisp/${feature}.el"
  if [[ -f "$lisp_file" ]]; then
    echo "$lisp_file"
    return
  fi
  # Check lib/ subdirectories
  for dir in lib/*/; do
    local lib_file="${dir}${feature}.el"
    if [[ -f "$lib_file" ]]; then
      echo "$lib_file"
      return
    fi
  done
}

# Map file path to feature name
file_to_feature() {
  local file="$1"
  basename "${file%.el}"
}

# Build reverse dependency map: feature -> files that depend on it
declare -A REVERSE_DEPS

build_dependency_graph() {
  local files=()
  shopt -s nullglob
  files+=(lisp/*.el lib/*/*.el)
  shopt -u nullglob

  for file in "${files[@]}"; do
    [[ -f "$file" ]] || continue
    [[ "$file" == *-tests.el ]] && continue

    local feature
    feature=$(file_to_feature "$file")

    for req in $(extract_requires "$file"); do
      # Only track internal dependencies (starting with + or in lib/)
      if [[ "$req" == +* ]] || [[ -n "$(feature_to_file "$req" 2>/dev/null)" ]]; then
        local existing="${REVERSE_DEPS[$req]:-}"
        if [[ -n "$existing" ]]; then
          REVERSE_DEPS[$req]="$existing $feature"
        else
          REVERSE_DEPS[$req]="$feature"
        fi
      fi
    done
  done
}

# Find all transitive dependents of a feature
find_dependents() {
  local feature="$1"
  local -A visited
  local queue=("$feature")
  local result=()

  while [[ ${#queue[@]} -gt 0 ]]; do
    local current="${queue[0]}"
    queue=("${queue[@]:1}")

    [[ -n "${visited[$current]:-}" ]] && continue
    visited[$current]=1
    result+=("$current")

    for dep in ${REVERSE_DEPS[$current]:-}; do
      if [[ -z "${visited[$dep]:-}" ]]; then
        queue+=("$dep")
      fi
    done
  done

  printf '%s\n' "${result[@]}"
}

# Find test file for a feature
find_test_file() {
  local feature="$1"
  local test_file

  # Check lisp/
  test_file="lisp/${feature}-tests.el"
  if [[ -f "$test_file" ]]; then
    echo "$test_file"
    return
  fi

  # Check lib/*/
  for dir in lib/*/; do
    test_file="${dir}${feature}-tests.el"
    if [[ -f "$test_file" ]]; then
      echo "$test_file"
      return
    fi
  done
}

main() {
  local base_ref="${1:-HEAD}"

  # Get changed .el files
  local changed_files
  mapfile -t changed_files < <(get_changed_files "$base_ref")

  if [[ ${#changed_files[@]} -eq 0 ]]; then
    echo "all"
    exit 0
  fi

  # Build dependency graph
  build_dependency_graph

  # Collect all affected features
  local -A affected_features
  for file in "${changed_files[@]}"; do
    local feature
    feature=$(file_to_feature "$file")
    affected_features[$feature]=1

    # Add all dependents
    for dep in $(find_dependents "$feature"); do
      affected_features[$dep]=1
    done
  done

  # Find test files for affected features
  local test_files=()
  for feature in "${!affected_features[@]}"; do
    local test_file
    test_file=$(find_test_file "$feature")
    if [[ -n "$test_file" ]]; then
      test_files+=("$test_file")
    fi
  done

  if [[ ${#test_files[@]} -eq 0 ]]; then
    echo "none"
    exit 0
  fi

  printf '%s\n' "${test_files[@]}" | sort -u
}

main "$@"
