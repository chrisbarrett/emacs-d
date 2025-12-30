;;; init-ocaml.el --- OCaml language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Experimental OCaml major mode using tree-sitter.
(use-package neocaml
  :ensure (neocaml :host github :repo "bbatsov/neocaml" :main "neocaml.el")

  :mode ("\\.ocamlinit\\'"))

(use-package ocaml-eglot
  :ensure t
  :after neocaml
  :hook
  (neocaml-mode-hook . ocaml-eglot)
  (ocaml-eglot-hook . eglot-ensure))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
