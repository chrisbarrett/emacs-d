;;; opam-mode.el --- Major mode for OPAM package config files -*- lexical-binding: t; -*-

;;; Commentary:

;; `opam-mode' is a major mode for [OPAM][opam] package configuration
;; files (`*.opam').  It derives from `conf-colon-mode' so the
;; inherited colon-separated key/value font-lock and navigation stay
;; intact.
;;
;; Activation is via `auto-mode-alist' for paths ending in `.opam'.
;;
;; The read-only-on-generated-files policy lives in
;; `modules/lang-ocaml/init.el' as a composition concern, not here.
;;
;; [opam]: https://opam.ocaml.org/

;;; Code:

(require 'rx)
(require 'conf-mode)


;;; Mode definition

;;;###autoload
(define-derived-mode opam-mode conf-colon-mode "OPAM Config"
  "Major mode for OPAM package configuration files.")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx ".opam" eos) 'opam-mode))

(provide 'opam-mode)

;;; opam-mode.el ends here
