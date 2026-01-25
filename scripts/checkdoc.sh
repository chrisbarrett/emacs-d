#!/usr/bin/env bash
set -euo pipefail

# Run checkdoc on Emacs Lisp files and report errors
# Usage: checkdoc.sh [file...]
# If no files specified, checks files in lisp/ and lib/

cd "$(dirname "$0")/.."

if [[ $# -eq 0 ]]; then
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
