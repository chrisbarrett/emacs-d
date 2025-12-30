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

(provide 'init-treesit)

;;; init-treesit.el ends here
