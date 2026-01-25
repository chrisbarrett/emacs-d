#!/usr/bin/env bash
set -euo pipefail

# Byte-compile Emacs Lisp files and fail on warnings
# Usage: byte-compile.sh [file...]
# If no files specified, compiles files in lisp/ and lib/

cd "$(dirname "$0")/.."
ROOT=$(pwd)

if [[ $# -eq 0 ]]; then
    mapfile -t files < <(git ls-files 'lisp/*.el' 'lib/**/*.el' | grep -v -e '-tests\.el$')
else
    files=("$@")
fi

if [[ ${#files[@]} -eq 0 ]]; then
    echo "No files to compile"
    exit 0
fi

emacs -Q --batch \
    --eval "(add-to-list 'load-path \"$ROOT/lisp\")" \
    --eval "(dolist (dir (directory-files \"$ROOT/lib\" t \"^[^.]\"))
              (when (file-directory-p dir)
                (add-to-list 'load-path dir)))" \
    --eval "(when (file-directory-p \"$ROOT/elpaca/builds\")
              (dolist (dir (directory-files \"$ROOT/elpaca/builds\" t \"^[^.]\"))
                (when (file-directory-p dir)
                  (add-to-list 'load-path dir))))" \
    --eval "(setq byte-compile-error-on-warn t)" \
    -f batch-byte-compile \
    "${files[@]}"
