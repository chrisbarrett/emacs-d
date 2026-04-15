#!/usr/bin/env bash
first() {
    cat <<'ELISP'
(message "first")
ELISP
}

second() {
    cat <<'PYTHON'
print("second")
PYTHON
}
