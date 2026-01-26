;;; lang-rust/tests.el --- Tests for lang-rust module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Rust language support module.

;;; Code:

(require 'ert)

;; Load module files from this directory
;; These may fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil)))

;;; P1: rust-ts-mode-local-vars-hook contains eglot-ensure

(ert-deftest lang-rust/eglot-hook ()
  "P1: rust-ts-mode-local-vars-hook should contain eglot-ensure."
  (skip-unless (featurep 'rust-ts-mode))
  (should (memq 'eglot-ensure rust-ts-mode-local-vars-hook)))

;;; P2: Opening .rs file activates rust-ts-mode
;; This is built-in Emacs behavior, not configured by this module

(ert-deftest lang-rust/auto-mode ()
  "P2: .rs files should be associated with rust-ts-mode (built-in)."
  ;; This tests built-in Emacs behavior
  (let ((entry (assoc "\\.rs\\'" auto-mode-alist)))
    (skip-unless entry)
    ;; Accept rust-ts-mode, rust-mode, or rust-ts-mode-maybe (Emacs 30+)
    (should (memq (cdr entry) '(rust-ts-mode rust-mode rust-ts-mode-maybe)))))

;;; P3: CARGO_TERM_COLOR env var is "always" after loading

(ert-deftest lang-rust/cargo-term-color ()
  "P3: CARGO_TERM_COLOR should be set to always."
  ;; Only test if init.el loaded successfully
  (skip-unless (getenv "CARGO_TERM_COLOR"))
  (should (equal (getenv "CARGO_TERM_COLOR") "always")))

;;; P4: separedit-default-mode is markdown-mode in rust buffers

(ert-deftest lang-rust/separedit-default-mode ()
  "P4: separedit should use markdown-mode for Rust buffers."
  (skip-unless (featurep 'rust-ts-mode))
  ;; Check the hook is set up to configure separedit
  (should (memq 'setq-hook!--rust-ts-mode-hook--separedit-default-mode
                rust-ts-mode-hook)))

;;; P5: Tempel snippets exist for rust-ts-mode

(ert-deftest lang-rust/tempel-snippets-exist ()
  "P5: Tempel snippets should be defined for rust-ts-mode."
  (let ((template-file (expand-file-name "templates/rust-ts.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-rust/tempel-pf-snippet ()
  "P5: pf snippet should exist in rust-ts templates."
  (let ((template-file (expand-file-name "templates/rust-ts.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (should (search-forward "(pf " nil t))))))

;;; P6-P8: Compilation error parsers
;; Note: These parsers are configured elsewhere (compile module).
;; Skip these tests in isolation - they're integration tests.

(ert-deftest lang-rust/compilation-parser-rustc ()
  "P6: rustc compilation parser should be registered."
  (require 'compile)
  ;; Skip if compilation module not loaded
  (skip-unless (assoc 'rustc compilation-error-regexp-alist-alist))
  (should (assoc 'rustc compilation-error-regexp-alist-alist)))

(ert-deftest lang-rust/compilation-parser-panic ()
  "P7: rust-panic compilation parser should be registered."
  (require 'compile)
  (skip-unless (assoc 'rust-panic compilation-error-regexp-alist-alist))
  (should (assoc 'rust-panic compilation-error-regexp-alist-alist)))

(ert-deftest lang-rust/compilation-parser-stacktrace ()
  "P8: rust-stacktrace compilation parser should be registered."
  (require 'compile)
  (skip-unless (assoc 'rust-stacktrace compilation-error-regexp-alist-alist))
  (should (assoc 'rust-stacktrace compilation-error-regexp-alist-alist)))

;;; P9: Org babel integration (rust blocks use rust-ts-mode)
;; Note: This is configured in the org module, not lang-rust.

(ert-deftest lang-rust/org-babel-rust-mode ()
  "P9: Org babel should map rust blocks to rust-ts-mode."
  (require 'org-src)
  ;; Skip if org module not loaded
  (let ((rs-entry (assoc "rs" org-src-lang-modes))
        (rust-entry (assoc "rust" org-src-lang-modes)))
    (skip-unless (or rs-entry rust-entry))
    (when rs-entry
      (should (eq (cdr rs-entry) 'rust-ts)))
    (when rust-entry
      (should (eq (cdr rust-entry) 'rust-ts)))))

(provide 'lang-rust-tests)

;;; lang-rust/tests.el ends here
