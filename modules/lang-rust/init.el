;;; lang-rust/init.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with rust-analyzer
;; - Markdown editing mode for doc comments (separedit)
;; - Colored Cargo output

;;; Code:

(require '+autoloads)

(require '+corelib)
(require '+lang)

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :config
  ;; Rust doc comments are Markdown
  (setq-hook! 'rust-ts-mode-hook separedit-default-mode 'markdown-mode)
  ;; Enable colored Cargo output in compilation buffers
  (setenv "CARGO_TERM_COLOR" "always"))

(+lang-declare 'rust-ts-mode :lsp t)

(use-package toml-ts-mode
  :mode ("/Cargo\\.lock\\'")
  :init
  (alist-set! major-mode-remap-alist #'conf-toml-mode #'toml-ts-mode)
  :config
  (add-hook! 'toml-ts-mode-hook
    (when (string-match-p (rx "/Cargo.lock" eos) (buffer-file-name))
      (read-only-mode +1))))

(+lang-declare 'toml-ts-mode
               :lsp t
               :formatter '(toml-tombi . ("tombi" "format" "-")))

;;; init.el ends here
