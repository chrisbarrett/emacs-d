;;; init-rust.el --- Rust language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package rust-ts-mode
  :hook (rust-ts-mode-hook . eglot-ensure)
  :config
  (setq-hook! 'rust-ts-mode-hook separedit-default-mode 'markdown-mode)
  ;; Make cargo commands run from Emacs look pretty.
  (setenv "CARGO_TERM_COLOR" "always"))


(provide 'init-rust)

;;; init-rust.el ends here
