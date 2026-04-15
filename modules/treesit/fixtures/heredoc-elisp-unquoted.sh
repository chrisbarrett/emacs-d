#!/usr/bin/env bash
RESULT=$(cat <<ELISP
(let ((file "$FILENAME"))
  (find-file file)
  (goto-char (point-min)))
ELISP
)
