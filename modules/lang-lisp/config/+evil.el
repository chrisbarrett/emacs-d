;;; +evil.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)
(require '+corelib)
(require 'evil)

(autoload 'helpful-at-point "helpful")

(defun +emacs-lisp-lookup-func ()
  "Lookup symbol at point using `helpful' or `describe-symbol'."
  (if (require 'helpful nil t)
      (helpful-at-point)
    (describe-symbol (symbol-at-point))))

(setq-hook! 'emacs-lisp-mode-hook
  evil-lookup-func #'+emacs-lisp-lookup-func)

;; Enter normal state after evaluating a region.

(define-advice eval-region (:around (fn &rest args) clear-visual-state)
  (unwind-protect (apply fn args)
    (when (eq evil-state 'visual)
      (evil-normal-state))))

;;; +evil.el ends here
