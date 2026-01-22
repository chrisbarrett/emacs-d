;;; init-auth.el --- Authentication configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Auth-source backend for 1Password via the `op' CLI.
;; Enables auth-source-search to retrieve credentials from 1Password for
;; packages like Forge, TRAMP, etc.

;;; Code:

(require 'elpaca)

(let ((local-path (expand-file-name "~/src/chrisbarrett/emacs-auth-source-op")))

  (if (file-directory-p local-path)
      (add-to-list 'load-path local-path)
    (elpaca (auth-source-op :host github :repo "chrisbarrett/emacs-auth-source-op"))))


(use-package auth-source-op
  :after auth-source
  :demand t

  :custom
  (auth-sources '(1password))
  (auth-source-op-vaults '("Emacs"))

  :functions (auth-source-op-enable)
  :config
  (auth-source-op-enable))

(provide 'init-auth)

;;; init-auth.el ends here
