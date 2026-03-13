;;; lang-conf/init.el --- Configuration file modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration file format support for JSON, YAML, KDL, and Unix config files.

;;; Code:

(require '+autoloads)
(require '+corelib)

(use-package conf-mode
  :mode ("rc\\'"
         "\\.dockerignore\\'"
         "\\.gitignore\\'"
         "/etc/ssh/.*"))

;; KDL file format used by Zellij etc.
(use-package kdl-ts-mode
  :mode "\\.kdl\\'")

(use-package json-ts-mode
  :hook (json-ts-mode-local-vars-hook . eglot-ensure))

(use-package yaml-ts-mode
  :hook (yaml-ts-mode-local-vars-hook . eglot-ensure)
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

;;; init.el ends here
