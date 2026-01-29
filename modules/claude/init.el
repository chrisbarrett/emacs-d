;;; init.el --- Claude module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Claude Code integration for Emacs with MCP bridge.

;;; Code:

(require '+corelib)
(require '+autoloads)

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
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server)

  ;; Auto-scroll claude-code-ide buffers to bottom when switching tabs/windows
  (add-hook '+switch-window-hook #'+claude-code-ide-scroll-to-bottom-h)
  (add-hook '+switch-buffer-hook #'+claude-code-ide-scroll-to-bottom-h))

;; Disable evil-mode in claude-code-ide buffers

(use-package evil
  :config
  (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

;; Prevent non-breaking-space in prompt from being visible.

(add-hook 'eat-exec-hook #'+eat-remap-nbsp)

;; Integrate with mise

(define-advice claude-code-ide--create-terminal-session (:around (fn &rest args) +mise-env)
  "Teach claude-code to use the mise environment, when available."
  (let* ((mise-vars
          (when (and (executable-find "mise")
                     (locate-dominating-file default-directory ".mise.toml"))
            (string-split
             (replace-regexp-in-string (rx bol "export" (+ space))
                                       ""
                                       (shell-command-to-string "mise env"))
             "\n"
             t)))
         (process-environment (append mise-vars process-environment)))
    (apply fn args)))

;;; init.el ends here
