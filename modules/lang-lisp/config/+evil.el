;;; +evil.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)
(require '+corelib)
(require 'evil)

(setq-hook! 'emacs-lisp-mode-hook
  evil-lookup-func #'+emacs-lisp-lookup-func)

;; Enter normal state after evaluating a region.

(define-advice eval-region (:around (fn &rest args) clear-visual-state)
  (unwind-protect (apply fn args)
    (when (eq evil-state 'visual)
      (evil-normal-state))))

;;; +evil.el ends here
