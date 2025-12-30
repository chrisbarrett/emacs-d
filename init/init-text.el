;;; init-text.el --- General text file editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package text-mode
  ;; Emacs' general parent mode for non-programming-language text files.

  :mode  ("/LICENSE\\'")

  ;; Not sure of the performance impact of this... leave off for now.
  ;;
  ;; :hook (text-mode-hook . visual-line-mode)

  :custom
  (text-mode-ispell-word-completion nil))

(provide 'init-text)

;;; init-text.el ends here
