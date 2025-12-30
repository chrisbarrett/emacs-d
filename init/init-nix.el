;;; init-nix.el --- Nix configuration language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package nix-ts-mode :ensure t
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-hook . eglot-ensure)
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "flake.nix"))
  (add-to-list 'auto-mode-alist `(,(rx "/flake.lock" eos) . json-ts-mode))
  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt")))

  (setq-hook! 'nix-ts-mode-hook apheleia-formatter 'nixpkgs-fmt)

  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "flake.nix")))


(provide 'init-nix)

;;; init-nix.el ends here
