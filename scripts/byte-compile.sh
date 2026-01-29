#!/usr/bin/env bash
set -euo pipefail

# Byte-compile Emacs Lisp files and fail on warnings
# Usage:
#   ./byte-compile.sh              # Compile all files in lisp/
#   ./byte-compile.sh --affected   # Compile affected files only
#   ./byte-compile.sh file1 file2  # Compile specific files

cd "$(dirname "$0")/.."
ROOT=$(pwd)

# Handle --affected flag
if [[ $# -eq 1 && "$1" == "--affected" ]]; then
    mapfile -t affected < <(./scripts/affected.sh)
    if [[ ${#affected[@]} -eq 0 ]]; then
        echo "No affected files"
        exit 0
    fi
    if [[ "${affected[0]}" == "all" ]]; then
        # Run full compilation
        mapfile -t files < <(git ls-files 'lisp/*.el' | grep -v -e '-tests\.el$')
    elif [[ "${affected[0]}" == "none" ]]; then
        echo "No files to compile for affected changes"
        exit 0
    else
        # Filter to only lisp/ files (byte-compile's scope)
        mapfile -t files < <(printf '%s\n' "${affected[@]}" | grep -E '^lisp/' | grep -v -e '-tests\.el$' || true)
    fi
elif [[ $# -eq 0 ]]; then
    mapfile -t files < <(git ls-files 'lisp/*.el' | grep -v -e '-tests\.el$')
else
    files=("$@")
fi

if [[ ${#files[@]} -eq 0 ]]; then
    echo "No files to compile"
    exit 0
fi

emacs -Q --batch \
    --eval "(add-to-list 'load-path \"$ROOT/lisp\")" \
    --eval "(when (file-directory-p \"$ROOT/elpaca/builds\")
              (dolist (dir (directory-files \"$ROOT/elpaca/builds\" t \"^[^.]\"))
                (when (file-directory-p dir)
                  (add-to-list 'load-path dir))))" \
    --eval "(setq byte-compile-error-on-warn t)" \
    -f batch-byte-compile \
    "${files[@]}"
