;;; treesit/lib.el --- Tree-sitter library functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Autoloaded functions for tree-sitter and expreg region expansion.

;;; Code:

;;;###autoload
(defun +expreg-expand-n (n)
  "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
  (interactive "p")
  (require 'expreg)
  (dotimes (_ n)
    (expreg-expand)))

;;;###autoload
(defun +expreg-expand-dwim ()
  "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
  (interactive)
  (require 'expreg)

  (when (bound-and-true-p iedit-mode)
    (iedit-done))

  (let ((symbol (bounds-of-thing-at-point 'symbol)))
    (cond
     ((equal (bounds-of-thing-at-point 'word) symbol)
      (+expreg-expand-n 1))
     (symbol (+expreg-expand-n 2))
     (t (expreg-expand)))))

(provide 'treesit-lib)

;;; treesit/lib.el ends here
