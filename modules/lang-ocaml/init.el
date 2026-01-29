;;; init.el --- OCaml module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; OCaml language support with neocaml tree-sitter mode, LSP, and formatter.

;;; Code:

(require '+autoloads)

(require '+corelib)

;; Read-only protection for build directories
(+dirlocals-set-regexp "/_build/"
  '((nil . ((mode . read-only)))))

;; Experimental OCaml major mode using tree-sitter
(use-package neocaml
  :mode (("\\.ocamlinit\\'" . neocaml-mode)
         ("\\.ml\\'" . neocaml-mode)
         ("\\.mli\\'" . neocamli-mode))
  :config
  (use-package neocaml-repl
    :hook (neocaml-mode-hook . neocaml-repl-minor-mode)))

;; Org babel integration
(use-package org
  :config
  (alist-set! org-src-lang-modes "ocaml" 'neocaml)
  (alist-set! org-src-lang-modes "ocamli" 'neocamli))

;; Formatter configuration
(use-package apheleia
  :defines apheleia-mode-alist
  :config
  (alist-set! apheleia-mode-alist 'neocaml-mode 'ocamlformat)
  (alist-set! apheleia-mode-alist 'neocamli-mode 'ocamlformat))

;; LSP server configuration
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(((neocaml-mode :language-id "ocaml")
                                         (neocamli-mode :language-id "ocaml"))
                                        . ("ocamllsp"))))

;; Project root detection
(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "dune-project"))

;; Extended eglot functionality for OCaml LSP
(use-package ocaml-eglot
  :hook
  (neocaml-mode-local-vars-hook . ocaml-eglot)
  (neocamli-mode-local-vars-hook . ocaml-eglot))

;; OPAM files use conf-colon-mode for basic syntax highlighting
(add-to-list 'auto-mode-alist (cons (rx ".opam" eos) 'conf-colon-mode))

;; Make generated opam files read-only
(add-hook! 'conf-colon-mode-hook
  (when (and buffer-file-name
             (string-match-p "\\.opam\\'" buffer-file-name)
             (string-match-p "# This file is generated"
                             (buffer-substring (point-min) (min (point-max) 500))))
    (read-only-mode +1)))

;; Dune build configuration files
(define-derived-mode dune-config-mode lisp-data-mode "Dune Config"
  "Major mode for Dune build configuration files."
  (setq-local comment-add 0))

(add-to-list 'auto-mode-alist (cons (rx "/dune" (? "-" (or "workspace" "project"))
                                        eos)
                                    #'dune-config-mode))



;;; init.el ends here
