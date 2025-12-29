;;; init-ocaml.el --- OCaml language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Experimental OCaml major mode using tree-sitter.
(use-package neocaml :ensure (neocaml :host github :repo "bbatsov/neocaml" :main "neocaml.el")
  :init
  (require 'neocaml-autoloads))

(provide 'init-ocaml)

;;; init-ocaml.el ends here
