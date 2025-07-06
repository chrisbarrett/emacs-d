;;; mod-magit.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'magit)

(autoload 'evil-insert-state "evil-states")
(autoload 'gptel-request "gptel")

;; See: https://cbea.ms/git-commit/#seven-rules
(defvar +git-commit-llm-prompt "\
Generate a git commit message for these changes following these rules:
1. Limit subject line to 72 characters
2. Capitalise the subject line
3. Do not end subject line with a period
4. Use imperative mood (as if giving a command)
5. Subject should complete: 'If applied, this commit will [subject line]'

Be specific about what changed. Only respond with the commit message, no explanation.
")

(defun +git-commit-generate-message-with-gptel ()
  "Generate a commit message using gptel based on staged changes."
  (let ((diff (shell-command-to-string "git diff --cached")))
    (if (string-empty-p (string-trim diff))
        (progn
          (insert "WIP: ")
          (evil-insert-state))
      (gptel-request (concat +git-commit-llm-prompt "\n\nDiff:\n" diff)
                     :callback (lambda (response _)
                                 (when response
                                   (goto-char (point-min))
                                   (insert (string-trim response))
                                   (evil-insert-state)))))))

;; Generate commit message using gptel when starting with empty commit message
(add-hook 'git-commit-mode-hook
          (defun +git-commit-set-message ()
            (when (and (bolp) (eolp))
              (+git-commit-generate-message-with-gptel))))

(provide 'mod-magit)

;;; mod-magit.el ends here
