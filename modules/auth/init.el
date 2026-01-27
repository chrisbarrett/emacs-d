;;; init.el --- Auth module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; 1Password integration for Emacs credential management via auth-source.

;;; Code:

;; Prefer local copy of auth-source-op.
(add-to-list 'load-path "~/src/chrisbarrett/emacs-auth-source-op")

(with-eval-after-load 'auth-source
  (require 'auth-source-op nil t)
  (when (fboundp 'auth-source-op-enable)
    (setq auth-sources '(1password))
    (setq auth-source-op-vaults '("Emacs"))
    (auth-source-op-enable)))

(provide 'auth-init)

;;; init.el ends here
