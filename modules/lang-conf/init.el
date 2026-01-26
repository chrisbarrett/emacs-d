;;; lang-conf/init.el --- Configuration file modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration file format support for JSON, YAML, KDL, and Unix config files.

;;; Code:

(require '+corelib)

;; Unix configuration files
(use-package conf-mode
  :mode ("rc\\'" "\\.dockerignore\\'" "\\.gitignore\\'"))

;; KDL file format used by Zellij etc.
(use-package kdl-ts-mode
  :mode "\\.kdl\\'")

;; JSON with Tree-sitter and LSP
(use-package json-ts-mode
  :hook (json-ts-mode-local-vars-hook . eglot-ensure))

;; YAML with Tree-sitter, LSP, and tab-width 2
(use-package yaml-ts-mode
  :hook (yaml-ts-mode-local-vars-hook . eglot-ensure)
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(provide 'lang-conf-init)

;;; lang-conf/init.el ends here
