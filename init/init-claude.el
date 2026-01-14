;;; init-claude.el --- Claude-Code integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(defsubst +claude-code-ide-active-buffer-p (buf)
  (and (buffer-live-p buf)
       (string-match-p (rx bol "*claude-code") (buffer-name buf))))

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

  ;; Auto-scroll claude-code-ide buffers to bottom when switching tabs/windows

  (defun +claude-code-ide-scroll-to-bottom-h ()
    "Scroll all visible claude-code-ide buffers to bottom.
This ensures consistent positioning when switching tabs, frames, or windows."
    (dolist (window (window-list nil 'no-minibuffer))
      (with-selected-window window
        (when (+claude-code-ide-active-buffer-p (current-buffer))
          (goto-char (point-max))
          (recenter -1)))))

  (add-hook '+switch-window-hook #'+claude-code-ide-scroll-to-bottom-h)
  (add-hook '+switch-buffer-hook #'+claude-code-ide-scroll-to-bottom-h))

;; Disable evil-mode in claude-code-ide buffers

(use-package evil
  :config
  (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

;; Prevent non-breaking-space in prompt from being visible.

(defun +eat-remap-nbsp (proc)
  (when-let* ((buf (process-buffer proc)))
    (when (+claude-code-ide-active-buffer-p buf)
      (with-current-buffer buf
        (face-remap-add-relative 'nobreak-space :inherit 'default)))))

(add-hook 'eat-exec-hook #'+eat-remap-nbsp)

;; Integrate with mise

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

(provide 'init-claude)

;;; init-claude.el ends here
