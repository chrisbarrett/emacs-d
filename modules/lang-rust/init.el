;;; lang-rust/init.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with rust-analyzer
;; - Markdown editing mode for doc comments (separedit)
;; - Colored Cargo output

;;; Code:

(require '+autoloads)

(require '+corelib)

(use-package rust-ts-mode
  :hook (rust-ts-mode-local-vars-hook . eglot-ensure)
  :config
  ;; Rust doc comments are Markdown
  (setq-hook! 'rust-ts-mode-hook separedit-default-mode 'markdown-mode)
  ;; Enable colored Cargo output in compilation buffers
  (setenv "CARGO_TERM_COLOR" "always"))


