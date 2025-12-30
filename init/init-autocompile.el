;;; init-autocompile.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package auto-compile :ensure t :demand t
  ;; Ensure we never attempt to load outdated ELC files.
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'init-autocompile)

;;; init-autocompile.el ends here
