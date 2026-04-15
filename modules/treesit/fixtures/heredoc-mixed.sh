#!/usr/bin/env bash
# Quoted heredoc — no shell expansion
cat <<'ELISP'
(message "no $expansion here")
ELISP

# Unquoted heredoc — shell expansion active
cat <<ELISP
(insert-file-contents "$ARGSFILE")
(insert "$NORMAL_VAR")
(insert "\$ESCAPED")
(insert "\\$DOUBLE_ESCAPED")
(insert "\\\$TRIPLE_ESCAPED")
ELISP

# Backslash-quoted — no shell expansion
cat <<\ELISP
(message "$also_no_expansion")
ELISP

# Double-quote quoted — no shell expansion
cat <<"ELISP"
(message "$still_no_expansion")
ELISP

# Tab-stripping unquoted — shell expansion active
cat <<-ELISP
	(insert "$TABBED_VAR")
	(insert "${BRACED_VAR}")
	(insert "$(command-sub)")
	(insert "$((1 + 2))")
ELISP
