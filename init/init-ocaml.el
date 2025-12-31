;;; init-ocaml.el --- OCaml language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; OCaml major mode.
;;
;; NOTE: Keep an eye on neocaml; it's much simpler, but I haven't had much luck
;; with an E2E configuration.

(use-package tuareg
  :ensure t
  )



(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "dune-project"))


;; Extend eglot to support extra functionality provided by the OCaml LSP.
(use-package ocaml-eglot
  :ensure t
  :hook
  (tuareg-mode-hook . ocaml-eglot)
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
