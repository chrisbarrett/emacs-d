;;; init.el --- Claude module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude Code integration for Emacs with MCP bridge.

;;; Code:

(require '+autoloads)
(require '+corelib)
(require '+hooks)

;; Run claude-code inside Emacs; creates an MCP bridge between the processes
;; to provide editor integration.

(use-package claude-code-ide
  :after-call +first-input-hook +first-file-hook
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-enable-mcp-server t)
  (claude-code-ide-mcp-server-port 19876)
  ;; Disable ediff for proposed changes--too interruptive.
  (claude-code-ide-use-ide-diff nil)
  :config
  (+load "config.el")
  ;; Claude Code CLI 2.1.129 sends `protocolVersion: "2025-11-25"` on /ide
  ;; WebSocket initialize and disconnects when the server replies with
  ;; the older `2024-11-05`. Upstream defconst hasn't been bumped; override
  ;; until manzaltu/claude-code-ide.el#? lands a fix.
  (setq claude-code-ide-mcp-version "2025-11-25")
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server))

;; Prevent non-breaking-space in prompt from being visible.

(add-hook 'eat-exec-hook #'+claude-code-eat-remap-nbsp)

;; git-commit-style editing surface for the prompt files Claude opens via
;; $EDITOR. The editor wrapper opens them through `emacsclient', so we hook
;; `server-visit-hook' (fires only for server-visited buffers) rather than
;; `find-file-hook' (every file) -- ordinary editing pays nothing.
;; `claude-prompt-setup-check-buffer' is autoloaded from lisp/claude-prompt/.

(with-eval-after-load 'server
  (add-hook 'server-visit-hook #'claude-prompt-setup-check-buffer))

;; Disable evil-mode in claude-code-ide buffers

(use-package evil
  :config
  (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

;;; init.el ends here
