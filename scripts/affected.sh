#!/usr/bin/env bash
# Compute which source files are transitively affected by staged changes.
# Output: list of source file paths (one per line), or "all" if no changes.
#
# Handles: require, load, use-package :after
#
# Usage: ./scripts/affected.sh [base-ref]
#        base-ref defaults to HEAD (staged + unstaged changes only)
set -euo pipefail

cd "$(dirname "$0")/.."

# Get changed .el files (excluding tests)
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

  # Deduplicate and filter to .el files in lisp/, lib/, config/, init/
  printf '%s\n' "${files[@]}" | sort -u | \
    grep -E '^(lisp|lib|config|init)/.*\.el$' | \
    grep -v -- '-tests\.el$' || true
}

# Extract dependencies from a file
# Handles: require, load (with string path), use-package :after
extract_deps() {
  local file="$1"
  {
    # require forms: (require 'feature)
    grep -oE "\(require '[^)]+\)" "$file" 2>/dev/null | \
      sed -E "s/\(require '([^)]+)\)/\1/"

    # load forms: (load "path")
    grep -oE '\(load "[^"]+"' "$file" 2>/dev/null | \
      sed -E 's/\(load "([^"]+)".*/\1/' | \
      while read -r path; do basename "$path" .el 2>/dev/null || true; done

    # use-package :after forms: (use-package pkg :after dep ...)
    # Matches: :after symbol, :after (list of symbols)
    grep -oE ':after [^:)]+' "$file" 2>/dev/null | \
      sed -E 's/:after //' | \
      tr -d '()' | \
      tr ' ' '\n' | \
      grep -v '^$'
  } | sort -u || true
}

# Map feature name to file path (returns empty if external)
feature_to_file() {
  local feature="$1"

  # Check lisp/ directory
  local lisp_file="lisp/${feature}.el"
  if [[ -f "$lisp_file" ]]; then
    echo "$lisp_file"
    return
  fi

  # Check config/ directory
  local config_file="config/${feature}.el"
  if [[ -f "$config_file" ]]; then
    echo "$config_file"
    return
  fi

  # Check init/ directory
  local init_file="init/${feature}.el"
  if [[ -f "$init_file" ]]; then
    echo "$init_file"
    return
  fi

  # Check lib/ subdirectories
  for dir in lib/*/; do
    [[ -d "$dir" ]] || continue
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
  files+=(lisp/*.el lib/*/*.el config/*.el init/*.el)
  shopt -u nullglob

  for file in "${files[@]}"; do
    [[ -f "$file" ]] || continue
    [[ "$file" == *-tests.el ]] && continue

    local feature
    feature=$(file_to_feature "$file")

    for dep in $(extract_deps "$file"); do
      # Only track internal dependencies
      local dep_file
      dep_file=$(feature_to_file "$dep" 2>/dev/null || true)
      if [[ -n "$dep_file" ]]; then
        local existing="${REVERSE_DEPS[$dep]:-}"
        if [[ -n "$existing" ]]; then
          REVERSE_DEPS[$dep]="$existing $feature"
        else
          REVERSE_DEPS[$dep]="$feature"
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

  # Convert features back to file paths
  local affected_files=()
  for feature in "${!affected_features[@]}"; do
    local file
    file=$(feature_to_file "$feature")
    if [[ -n "$file" && -f "$file" ]]; then
      affected_files+=("$file")
    fi
  done

  if [[ ${#affected_files[@]} -eq 0 ]]; then
    echo "none"
    exit 0
  fi

  printf '%s\n' "${affected_files[@]}" | sort -u
}

main "$@"
