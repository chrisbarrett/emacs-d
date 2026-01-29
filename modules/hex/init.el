;;; hex/init.el --- Hex editing -*- lexical-binding: t; -*-

;;; Commentary:

;; Vim-style keybindings for hexl mode.

;;; Code:

(require '+autoloads)

(use-package hexl
  :general-config
  (:states 'normal
   :keymaps 'hexl-mode-map
   "h" #'hexl-backward-char
   "l" #'hexl-forward-char
   "]]" #'hexl-end-of-1k-page
   "[[" #'hexl-beginning-of-1k-page
   "$" #'hexl-end-of-line
   "^" #'hexl-beginning-of-line
   "0" #'hexl-beginning-of-line))



;;; init.el ends here
