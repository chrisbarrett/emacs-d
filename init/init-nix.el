;;; init-nix.el --- Nix configuration language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(+dirlocals-set "/nix/store/"
  '((nil . ((mode . read-only)))))


(use-package nix-ts-mode :ensure t
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-local-vars-hook . eglot-ensure)
  :init
  (add-to-list 'auto-mode-alist (cons (rx "/flake.lock" eos) #'json-ts-mode)))


(use-package +file-templates
  :config
  (+define-file-template (rx "flake.nix" eos) "flake.eld"))


(use-package apheleia
  :defines apheleia-formatters
  :config
  (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
  (setq-hook! 'nix-ts-mode-hook apheleia-formatter 'nixpkgs-fmt))


(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "flake.nix"))

(provide 'init-nix)

;;; init-nix.el ends here
