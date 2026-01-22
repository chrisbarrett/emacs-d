;;; init-auth.el --- Authentication configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Auth-source backend for 1Password via the `op' CLI.
;; Enables auth-source-search to retrieve credentials from 1Password for
;; packages like Forge, TRAMP, etc.

;;; Code:

(use-package auth-source-op
  :ensure-unless-local ("~/src/chrisbarrett/emacs-auth-source-op"
                        (auth-source-op :host github :repo "chrisbarrett/emacs-auth-source-op"))
  :after auth-source
  :demand t
  :custom
  (auth-sources '(1password))
  (auth-source-op-vaults '("Emacs"))
  :config
  (auth-source-op-enable))

(provide 'init-auth)

;;; init-auth.el ends here
