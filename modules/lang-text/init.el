;;; init.el --- lang-text module -*- lexical-binding: t; -*-

;;; Commentary:

;; Plain text file editing configuration.

;;; Code:

(require '+autoloads)

(use-package text-mode
  :mode ("/LICENSE\\'")
  :custom
  (text-mode-ispell-word-completion nil))



;;; init.el ends here
