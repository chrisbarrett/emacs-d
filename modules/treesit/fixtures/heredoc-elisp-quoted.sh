#!/usr/bin/env bash
cat <<'ELISP'
(defun greet (name)
  "Return greeting for NAME."
  (format "Hello, %s!" name))
ELISP
