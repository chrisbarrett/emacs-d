;;; lang-conf/init.el --- Configuration file modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration file format support for JSON, YAML, KDL, and Unix config files.

;;; Code:

(require '+autoloads)
(require '+corelib)
(require '+lang)

(use-package conf-mode
  :mode ("rc\\'"
         "\\.dockerignore\\'"
         "\\.gitignore\\'"
         "/etc/ssh/.*"))

;; KDL file format used by Zellij etc.
(use-package kdl-ts-mode
  :mode "\\.kdl\\'")

(use-package json-ts-mode
  :mode "\\.json\\'")

(+lang-declare 'json-ts-mode :lsp t)

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(+lang-declare 'yaml-ts-mode :lsp t)

;;; init.el ends here
