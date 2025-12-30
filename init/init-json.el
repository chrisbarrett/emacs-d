;;; init-json.el --- JSON files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package json-ts-mode
  :hook (json-ts-mode-hook . eglot-ensure))


(provide 'init-json)

;;; init-json.el ends here
