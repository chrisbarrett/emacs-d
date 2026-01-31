;;; init.el --- Misery module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Mise integration for tool version management.
;; Automatically configures tool versions (node, python, etc.) based on
;; .mise.toml or .tool-versions files in the project.

;;; Code:

(require '+autoloads)

(require '+corelib)

(use-package misery
  :load-path "~/src/chrisbarrett/emacs-misery"
  :hook (+first-file-hook . misery-global-mode)
  :custom
  (misery-show-summary-in-minibuffer nil))



;;; init.el ends here
