;;; mod-magit.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'magit)
(require 'general)

(autoload 'evil-insert-state "evil-states")
(autoload 'gptel-request "gptel")
(autoload 'magit-run-git-async "magit-process")

(general-def :keymaps 'git-commit-mode-map
  "C-c C-l" #'+git-commit-generate-message-with-gptel)

;; Automatically enter insert state on empty commit message.
(add-hook! 'git-commit-mode-hook
  (when (and (bolp) (eolp))
    (evil-insert-state)))


;;; Teach magit-commit how to generate commit messages based off diffs via an
;;; LLM integration.

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
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (if (string-empty-p (string-trim diff))
        (evil-insert-state)
      (let ((spinner (make-progress-reporter "Generating commit message"))
            (timer nil))
        (setq timer (run-at-time 0.1 0.1 (lambda () (progress-reporter-update spinner))))
        (gptel-request (concat +git-commit-llm-prompt "\n\nDiff:\n" diff)
          :callback (lambda (response _)
                      (when timer
                        (cancel-timer timer))
                      (progress-reporter-done spinner)
                      (when response
                        (goto-char (point-min))
                        (insert (string-trim response))
                        (evil-insert-state))))))))

(defun +magit-commit-with-llm ()
  "Create a commit in the background using an LLM-generated message."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (if (string-empty-p (string-trim diff))
        (message "No staged changes to commit")
      (let ((spinner (make-progress-reporter "Generating commit message and committing"))
            (timer nil))
        (setq timer (run-at-time 0.1 0.1 (lambda () (progress-reporter-update spinner))))
        (gptel-request (concat +git-commit-llm-prompt "\n\nDiff:\n" diff)
          :callback (lambda (response _)
                      (when timer
                        (cancel-timer timer))
                      (progress-reporter-done spinner)
                      (if response
                          (let ((commit-msg (string-trim response)))
                            (message "Committing: %s" commit-msg)
                            (magit-run-git-async "commit" "-m" commit-msg))
                        (message "Failed to generate commit message"))))))))

;; Add keybinding to magit-commit transient
(with-eval-after-load 'magit-commit
  (transient-append-suffix 'magit-commit "c"
    '("l" "LLM-generated message" +magit-commit-with-llm)))

(provide 'mod-magit)

;;; mod-magit.el ends here
