;;; init.el --- lang-text module -*- lexical-binding: t; -*-

;;; Commentary:

;; Plain text file editing configuration.

;;; Code:

(use-package text-mode
  :mode ("/LICENSE\\'")
  :custom
  (text-mode-ispell-word-completion nil))

(provide 'lang-text-init)

;;; init.el ends here
