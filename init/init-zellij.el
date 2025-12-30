;;; init-zellij.el --- Support for working with Zellij -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Major-mode for the KDL file format used by Zellij.
(use-package kdl-ts-mode :ensure (:host github :repo "merrickluo/kdl-ts-mode")
  :mode "\\.kdl\\'")


(provide 'init-zellij)

;;; init-zellij.el ends here
