#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
exec emacs -Q --batch -l tests.el
