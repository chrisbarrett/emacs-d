#!/usr/bin/env bash
set -euo pipefail

# Run checkdoc on Emacs Lisp files and report errors
# Usage:
#   ./checkdoc.sh              # Check all files in lisp/
#   ./checkdoc.sh --affected   # Check affected files only
#   ./checkdoc.sh file1 file2  # Check specific files

cd "$(dirname "$0")/.."

# Handle --affected flag
if [[ $# -eq 1 && "$1" == "--affected" ]]; then
    mapfile -t affected < <(./scripts/affected.sh)
    if [[ ${#affected[@]} -eq 0 ]]; then
        echo "No affected files"
        exit 0
    fi
    if [[ "${affected[0]}" == "all" ]]; then
        # Run full check
        mapfile -t files < <(git ls-files 'lisp/*.el' 'lib/**/*.el' | grep -v -e '-tests\.el$')
    elif [[ "${affected[0]}" == "none" ]]; then
        echo "No files to check for affected changes"
        exit 0
    else
        # Filter to only lisp/ and lib/ files (checkdoc's scope)
        mapfile -t files < <(printf '%s\n' "${affected[@]}" | grep -E '^(lisp|lib)/' | grep -v -e '-tests\.el$' || true)
    fi
elif [[ $# -eq 0 ]]; then
    mapfile -t files < <(git ls-files 'lisp/*.el' 'lib/**/*.el' | grep -v -e '-tests\.el$')
else
    files=("$@")
fi

if [[ ${#files[@]} -eq 0 ]]; then
    echo "No files to check"
    exit 0
fi

emacs -Q --batch \
    --eval "(require 'checkdoc)" \
    --eval "(setq checkdoc-arguments-in-order-flag nil)" \
    --eval "(setq sentence-end-double-space nil)" \
    --eval "(let ((errors nil))
              (dolist (file command-line-args-left)
                (with-temp-buffer
                  (insert-file-contents file)
                  (emacs-lisp-mode)
                  (setq buffer-file-name file)
                  (condition-case err
                      (checkdoc-current-buffer t)
                    (error
                     (message \"checkdoc error in %s: %s\" file err)
                     (setq errors t)))))
              (when errors
                (kill-emacs 1)))" \
    "${files[@]}"
