;;; init.el --- Auth module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; 1Password integration for Emacs credential management via auth-source.

;;; Code:

(use-package auth-source-op
  :load-path "~/src/chrisbarrett/emacs-auth-source-op"
  :after auth-source
  :functions auth-source-op-enable
  :demand t
  :custom
  (auth-sources '(1password))
  (auth-source-op-vaults '("Emacs"))
  :config
  (auth-source-op-enable))

(provide 'auth-init)

;;; init.el ends here
