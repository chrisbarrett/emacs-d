;;; lang-nix/init.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with nil or nixd
;; - nixpkgs-fmt formatter via apheleia
;; - Read-only protection for /nix/store/
;; - Project detection via flake.nix

;;; Code:

(require '+autoloads)
(require '+corelib)

;; Read-only protection for Nix store
(+dirlocals-set "/nix/store/"
  '((nil . ((mode . read-only)))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-local-vars-hook . eglot-ensure)
  :init
  ;; flake.lock files are JSON
  (add-to-list 'auto-mode-alist (cons (rx "/flake.lock" eos) #'json-ts-mode)))

(use-package apheleia
  :defines apheleia-formatters
  :config
  (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
  (setq-hook! 'nix-ts-mode-hook apheleia-formatter 'nixpkgs-fmt))

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "flake.nix"))

;; File template for flake.nix
(+define-file-template (rx "flake.nix" eos) "flake.eld")


;;; init.el ends here
