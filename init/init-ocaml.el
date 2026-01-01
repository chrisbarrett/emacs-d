;;; init-ocaml.el --- OCaml language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Experimental OCaml major mode using tree-sitter.
(use-package neocaml
  :ensure (neocaml :host github :repo "bbatsov/neocaml" :main "neocaml.el")
  :mode (("\\.ocamlinit\\'" . neocaml-mode)
         ("\\.ml\\'" . neocaml-mode)
         ("\\.mli\\'" . neocamli-mode))
  :config
  (use-package neocaml-repl
    :hook (neocaml-mode-hook . neocaml-repl-minor-mode))

  (require 'mod-ocaml))


(use-package apheleia
  :defines apheleia-mode-alist
  :config
  (alist-set! apheleia-mode-alist 'neocaml-mode 'ocamlformat))


(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((neocaml-mode :language-id "ocaml") . ("ocamllsp"))))


(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "dune-project"))


;; Extend eglot to support extra functionality provided by the OCaml LSP.
(use-package ocaml-eglot
  :ensure t
  :hook
  (neocaml-mode-local-vars-hook . ocaml-eglot)
  ;; NOTE: Eglot is not automatically activated in OCaml right now. Something's
  ;; borked in the flake+direnv integration in the repo I'm testing with,
  ;; meaning `exec-path' isn't set correctly and the LSP cannot be found.
  )

;; OPAM files use a complex config language, but `conf-colon-mode' is probably
;; good enough as a basis for syntax highlighting.

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
