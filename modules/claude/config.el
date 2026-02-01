;;; config.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)

;; Auto-scroll claude-code-ide buffers to bottom when switching tabs/windows

(add-hook '+switch-window-hook #'+claude-code-ide-scroll-to-bottom-h)
(add-hook '+switch-buffer-hook #'+claude-code-ide-scroll-to-bottom-h)

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

;;; config.el ends here
