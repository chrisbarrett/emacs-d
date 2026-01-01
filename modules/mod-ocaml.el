;;; mod-ocaml.el --- OCaml development support -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for OCaml development, including Tempel integration.

;;; Code:

(autoload 'tuareg-smie--backward-token "tuareg")
(defvar tuareg-smie--exp-leaders)


;; Tempel snippet functions

(defvar-local +ocaml--tempel-in-expr-p nil
  "Whether current template expansion started in expression context.")

(defun +ocaml-capture-let-context ()
  "Capture whether point is in an expression context for let.
Returns empty string for template insertion but sets buffer-local state."
  (setq +ocaml--tempel-in-expr-p
        (save-excursion
          (or (member (tuareg-smie--backward-token)
                      tuareg-smie--exp-leaders)
              ;; Also check if preceded by open paren/bracket
              (and (not (bobp))
                   (eq 4 (car (syntax-after (1- (point)))))))))
  "")

(defun +ocaml-maybe-in ()
  "Return \" in\" if template started in expression context."
  (if +ocaml--tempel-in-expr-p " in" ""))

(provide 'mod-ocaml)

;;; mod-ocaml.el ends here
