;;; init.el --- Misery module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Mise integration for tool version management.
;; Automatically configures tool versions (node, python, etc.) based on
;; .mise.toml or .tool-versions files in the project.

;;; Code:

(require '+corelib)

(use-package misery
  :ensure-unless-local ("~/src/chrisbarrett/emacs-misery"
                        (nursery :host github
                                 :repo "chrisbarrett/emacs-misery"))
  :hook (+first-file-hook . misery-global-mode)
  :custom
  (misery-show-summary-in-minibuffer nil))

(provide 'misery-init)

;;; init.el ends here
