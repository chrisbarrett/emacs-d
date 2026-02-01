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
  ;; Disable ediff for proposed changes--too interruptive.
  (claude-code-ide-use-ide-diff nil)
  :config
  (+load "config.el")
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server))

;; Prevent non-breaking-space in prompt from being visible.

(add-hook 'eat-exec-hook #'+claude-code-eat-remap-nbsp)

;; Disable evil-mode in claude-code-ide buffers

(use-package evil
  :config
  (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

;;; init.el ends here
