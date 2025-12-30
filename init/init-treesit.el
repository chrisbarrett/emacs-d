;;; init-treesit.el --- Tree-Sitter -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Automatic installation of treesitter grammars.
(use-package treesit-auto :ensure t
  :after-call +first-buffer-hook +first-file-hook
  :commands global-treesit-auto-mode
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Cache the expensive grammar detection to avoid TTY performance issues
  (defvar +treesit-auto--cached-remap-alist nil
    "Cache for treesit-auto major mode remap alist.")

  (define-advice treesit-auto--build-major-mode-remap-alist
      (:around (orig-fn &rest args) cache-remap-alist)
    "Cache the result of building the major mode remap alist to avoid expensive
subprocess calls on every file open, especially problematic in TTY."
    (or +treesit-auto--cached-remap-alist
        (setq +treesit-auto--cached-remap-alist
              (apply orig-fn args))))

  (global-treesit-auto-mode +1))


;; Use tree-sitter to mark syntactic elements.
;;
;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
;; a selection, make - call avy.
(use-package expreg
  :ensure t
  :functions expreg-expand
  :init
  (defun +expreg-expand-n (n)
    "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
    (interactive "p")
    (dotimes (_ n)
      (expreg-expand)))

  (defun +expreg-expand-dwim ()
    "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
    (interactive)

    (when (bound-and-true-p iedit-mode)
      (iedit-done))

    (let ((symbol (bounds-of-thing-at-point 'symbol)))
      (cond
       ((equal (bounds-of-thing-at-point 'word) symbol)
        (+expreg-expand-n 1))
       (symbol (+expreg-expand-n 2))
       (t (expreg-expand)))))

  :general
  (:states '(normal motion)
           "-" (general-predicate-dispatch #'avy-goto-char-timer
                 (region-active-p) #'expreg-contract)
           "+" #'+expreg-expand-dwim))

(provide 'init-treesit)

;;; init-treesit.el ends here
