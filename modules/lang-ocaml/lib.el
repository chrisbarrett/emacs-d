;;; lib.el --- OCaml module library -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for OCaml development, including Tempel integration.

;;; Code:

(autoload 'treesit-node-at "treesit")
(autoload 'treesit-parent-until "treesit")
(autoload 'treesit-node-child-by-field-name "treesit")
(autoload 'treesit-node-start "treesit")
(autoload 'treesit-node-end "treesit")
(autoload 'treesit-node-type "treesit")

(defvar-local +ocaml--tempel-in-expr-p nil
  "Whether current template expansion started in expression context.")

(defun +ocaml--point-in-node-field-p (node field)
  "Return non-nil if point is within NODE's FIELD."
  (when-let* ((child (treesit-node-child-by-field-name node field)))
    (and (>= (point) (treesit-node-start child))
         (<= (point) (treesit-node-end child)))))

(defun +ocaml--treesit-in-expr-context-p ()
  "Check if point is in an expression context using tree-sitter.
Returns non-nil if a let at point would need an `in' suffix."
  (when-let* ((node (treesit-node-at (point)))
              (ancestor (treesit-parent-until
                         node
                         (lambda (n)
                           (pcase (treesit-node-type n)
                             ((or "structure" "signature")
                              t)
                             ("let_binding"
                              (+ocaml--point-in-node-field-p n "body")))))))
    (equal (treesit-node-type ancestor) "let_binding")))

;;;###autoload
(defun +ocaml-capture-let-context ()
  "Capture whether point is in an expression context for let.
Returns empty string for template insertion but sets buffer-local state."
  (setq +ocaml--tempel-in-expr-p (+ocaml--treesit-in-expr-context-p))
  "")

;;;###autoload
(defun +ocaml-maybe-in ()
  "Return \" in\" if template started in expression context."
  (if +ocaml--tempel-in-expr-p " in" ""))



;;; lib.el ends here
