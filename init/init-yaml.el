;;; init-yaml.el --- YAML files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yaml-ts-mode
  :hook (yaml-ts-mode-hook . eglot-ensure)
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(provide 'init-yaml)

;;; init-yaml.el ends here
