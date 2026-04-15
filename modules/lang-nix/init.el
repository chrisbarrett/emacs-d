;;; lang-nix/init.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with nil or nixd
;; - nixpkgs-fmt formatter via apheleia
;; - Read-only protection for /nix/store/
;; - Project detection via flake.nix
;; - Polymode for syntax-highlighted multiline strings

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

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "flake.nix"))

;; File template for flake.nix
(+define-file-template (rx "flake.nix" eos) "flake.eld")

;;; Polymode — syntax highlighting for embedded languages in multiline strings
;;
;; Nix multiline strings use '' delimiters. Nixpkgs convention marks the
;; embedded language with a comment before the string:
;;   buildPhase = /* bash */ ''
;;   buildPhase =
;;     # bash
;;     ''

(use-package polymode
  :defer t
  :init
  (defun +nix-poly--head-matcher (lang)
    "Return head-matcher regex for LANG annotation before a Nix multiline string.
Matches both `/* lang */' block comments and `# lang' line comments,
with `''' potentially on the same or next line."
    (rx-to-string
     `(seq (or (seq "/*" (+ space) ,lang (+ space) "*/")
               (seq "# " ,lang))
           (* space) (? "\n" (* space))
           "''")))

  (define-hostmode poly-nix-ts-hostmode
    :mode 'nix-ts-mode)

  (define-innermode poly-nix-bash-innermode
    :mode 'bash-ts-mode
    :head-matcher (+nix-poly--head-matcher "bash")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-python-innermode
    :mode 'python-ts-mode
    :head-matcher (+nix-poly--head-matcher "python")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-elisp-innermode
    :mode 'emacs-lisp-mode
    :head-matcher (+nix-poly--head-matcher "elisp")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-lua-innermode
    :mode 'lua-ts-mode
    :head-matcher (+nix-poly--head-matcher "lua")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-json-innermode
    :mode 'json-ts-mode
    :head-matcher (+nix-poly--head-matcher "json")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-sql-innermode
    :mode 'sql-mode
    :head-matcher (+nix-poly--head-matcher "sql")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-rust-innermode
    :mode 'rust-ts-mode
    :head-matcher (+nix-poly--head-matcher "rust")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-c-innermode
    :mode 'c-ts-mode
    :head-matcher (+nix-poly--head-matcher "c")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-nix-ts-mode
    :hostmode 'poly-nix-ts-hostmode
    :innermodes '(poly-nix-bash-innermode
                  poly-nix-python-innermode
                  poly-nix-elisp-innermode
                  poly-nix-lua-innermode
                  poly-nix-json-innermode
                  poly-nix-sql-innermode
                  poly-nix-rust-innermode
                  poly-nix-c-innermode))

  (add-to-list 'major-mode-remap-alist '(nix-ts-mode . poly-nix-ts-mode))

  (add-hook 'poly-nix-ts-mode-hook #'+polymode-refontify-inner-spans))


;;; init.el ends here
