;;; dune-mode.el --- Major mode for Dune build config files -*- lexical-binding: t; -*-

;;; Commentary:

;; `dune-mode' is a major mode for [Dune][dune] build configuration
;; files (`dune', `dune-workspace', `dune-project').  It derives from
;; `lisp-data-mode' so the inherited s-expression editing experience
;; (paredit-style navigation, indentation) stays intact, and sets
;; `comment-add' to 0 so `M-;' inserts a single `;' rather than `;;'
;; — matching the Dune commenting convention.
;;
;; Activation is via `auto-mode-alist' for paths ending in `dune',
;; `dune-workspace', or `dune-project' with no extension.
;;
;; [dune]: https://dune.build/

;;; Code:

(require 'rx)


;;; Mode definition

;;;###autoload
(define-derived-mode dune-mode lisp-data-mode "Dune Config"
  "Major mode for Dune build configuration files."
  (setq-local comment-add 0))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/dune" (? "-" (or "workspace" "project")) eos)
                                    #'dune-mode))

(provide 'dune-mode)

;;; dune-mode.el ends here
