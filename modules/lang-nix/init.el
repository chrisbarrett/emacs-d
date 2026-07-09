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
(require '+lang)

;; Read-only protection for Nix store
(+dirlocals-set "/nix/store/"
  '((nil . ((mode . read-only)))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :init
  ;; flake.lock files are JSON
  (add-to-list 'auto-mode-alist (cons (rx "/flake.lock" eos) #'json-ts-mode)))

(+lang-declare 'nix-ts-mode :lsp t)

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "flake.nix"))

;; File template for flake.nix
(+define-file-template (rx "flake.nix" eos) "flake.eld")

;;; init.el ends here
