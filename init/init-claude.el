;;; init-claude.el --- Claude-Code integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Run claude-code inside Emacs; creates an MCP bridge between the processes
;; to provide editor integration.
(use-package claude-code-ide :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :after-call +first-input-hook +first-file-hook
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-enable-mcp-server t)
  ;; disable ediff for proposed changes--too interruptive.
  (claude-code-ide-use-ide-diff nil)
  :config
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server)

  ;; Disable evil-mode in ancillary buffers using eat
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

  (define-advice claude-code-ide--create-terminal-session (:around (fn &rest args))
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

  ;; Auto-scroll claude-code-ide buffers to bottom when switching tabs/windows
  (defun +claude-code-ide-scroll-to-bottom-h ()
    "Scroll all visible claude-code-ide buffers to bottom.
This ensures consistent positioning when switching tabs, frames, or windows."
    (dolist (window (window-list nil 'no-minibuffer))
      (with-selected-window window
        (when (and (buffer-live-p (current-buffer))
                   (string-match-p (rx bol "*claude-code") (buffer-name)))
          (goto-char (point-max))
          (recenter -1)))))

  (add-hook '+switch-window-hook #'+claude-code-ide-scroll-to-bottom-h)
  (add-hook '+switch-buffer-hook #'+claude-code-ide-scroll-to-bottom-h))


(provide 'init-claude)

;;; init-claude.el ends here
