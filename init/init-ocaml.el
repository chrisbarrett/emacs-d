;;; init-ocaml.el --- OCaml language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Experimental OCaml major mode using tree-sitter.
(use-package neocaml
  :ensure (neocaml :host github :repo "bbatsov/neocaml" :main "neocaml.el")

  :mode ("\\.ocamlinit\\'"))

;; Extend eglot to support extra functionality provided by the OCaml LSP.
(use-package ocaml-eglot
  :ensure t
  :after neocaml
  :hook
  (neocaml-mode-hook . ocaml-eglot)
  (ocaml-eglot-hook . eglot-ensure))

;; OPAM files use a complex config language, but `conf-colon-mode' is probably good
;; enough as a basis for syntax highlighting.

(add-to-list 'auto-mode-alist (cons (rx ".opam" eos) 'conf-colon-mode))

(add-hook! 'conf-colon-mode-hook
  (when (string-match-p "# This file is generated" (buffer-substring (point-min) (point-max)))
    (read-only-mode +1)))

;; Dune config files use an S-Expression language documented here:
;;
;; https://dune.readthedocs.io/en/stable/reference/lexical-conventions.html
;;
;; Define a custom mode for these.

(define-derived-mode dune-config-mode lisp-data-mode "Dune Config"
  (setq-local comment-add 0))

(add-to-list 'auto-mode-alist (cons (rx "/dune" (? "-" (or "workspace" "project"))
                                        eos)
                                    #'dune-config-mode))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
