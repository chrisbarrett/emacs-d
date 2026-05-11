;; may-i configuration  -*- mode: may-i-config; -*-
;;
;; Repo-local rules merged on top of ~/.config/may-i/config.lisp.
;; Inert until approved via `may-i trust`.

;; Authorise agents to terminate their own sandbox Emacs daemons.
;; The daemon name is documented in AGENTS.md as
;; `emacs-claude-sandbox-<unique-suffix>'; the regex requires at
;; least one character after the prefix so the bare prefix cannot
;; match other agents' sandboxes via wildcard.
(rule "pkill"
  (when (parameter "f" (regex "^emacs-claude-sandbox-.+$"))
    (allow "Killing this agent's sandbox Emacs daemon")))

(check
  (allow "pkill -f emacs-claude-sandbox-12345")
  (allow "pkill -f \"emacs-claude-sandbox-$$\"")
  ;; Bare prefix must NOT match — would hit other agents' sandboxes.
  (ask "pkill -f emacs-claude-sandbox-")
  ;; Unrelated pkill stays subject to the global rule.
  (ask "pkill -f some-other-process"))
