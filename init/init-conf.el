;;; init-conf.el --- Configuration files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Unix configuration files
(use-package conf-mode
  :mode ("rc\\'" "\\.dockerignore\\'" "\\.gitignore\\'"))


;; KDL file format used by Zellij etc.
(use-package kdl-ts-mode :ensure (:host github :repo "merrickluo/kdl-ts-mode")
  :mode "\\.kdl\\'")


(use-package json-ts-mode
  :hook (json-ts-mode-local-vars-hook . eglot-ensure))


(use-package yaml-ts-mode
  :hook (yaml-ts-mode-local-vars-hook . eglot-ensure)
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(provide 'init-conf)

;;; init-conf.el ends here
