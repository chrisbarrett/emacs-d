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
  :mode "\\.rs\\'"
  :hook (rust-ts-mode-local-vars-hook . eglot-ensure)
  :config
  ;; Rust doc comments are Markdown
  (setq-hook! 'rust-ts-mode-hook separedit-default-mode 'markdown-mode)
  ;; Enable colored Cargo output in compilation buffers
  (setenv "CARGO_TERM_COLOR" "always"))

(use-package toml-ts-mode
  :mode ("/Cargo\\.lock\\'")
  :hook (toml-ts-mode-local-vars-hook . eglot-ensure)
  :init
  (alist-set! major-mode-remap-alist #'conf-toml-mode #'toml-ts-mode)
  :config
  (add-hook! 'toml-ts-mode-hook
    (when (string-match-p (rx "/Cargo.lock" eos) (buffer-file-name))
      (read-only-mode +1))))

(use-package apheleia
  :defines (apheleia-formatters apheleia-mode-alist)
  :config
  (add-to-list 'apheleia-formatters '(toml-tombi . ("tombi" "format" "-")))
  (alist-set! apheleia-mode-alist 'toml-ts-mode 'toml-tombi))

;;; init.el ends here
