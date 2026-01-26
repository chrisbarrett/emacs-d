;;; lang-rust/tests.el --- Tests for lang-rust module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Rust language support module.

;;; Code:

(require 'ert)

;;; P1: rust-ts-mode-local-vars-hook contains eglot-ensure

(ert-deftest lang-rust/eglot-hook ()
  "P1: rust-ts-mode-local-vars-hook should contain eglot-ensure."
  (require 'rust-ts-mode)
  (require 'lang-rust-init)
  (should (memq 'eglot-ensure rust-ts-mode-local-vars-hook)))

;;; P2: Opening .rs file activates rust-ts-mode

(ert-deftest lang-rust/auto-mode ()
  "P2: .rs files should be associated with rust-ts-mode."
  (let ((entry (assoc "\\.rs\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'rust-ts-mode))))

;;; P3: CARGO_TERM_COLOR env var is "always" after loading

(ert-deftest lang-rust/cargo-term-color ()
  "P3: CARGO_TERM_COLOR should be set to always."
  (require 'lang-rust-init)
  (should (equal (getenv "CARGO_TERM_COLOR") "always")))

;;; P4: separedit-default-mode is markdown-mode in rust buffers

(ert-deftest lang-rust/separedit-default-mode ()
  "P4: separedit should use markdown-mode for Rust buffers."
  (require 'lang-rust-init)
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

;;; P6-P8: Compilation error parsers (tested in mod-compilation-tests.el)
;; These tests verify the parsers are registered

(ert-deftest lang-rust/compilation-parser-rustc ()
  "P6: rustc compilation parser should be registered."
  (require 'compile)
  ;; After loading compilation config, rustc should be in alist
  (should (assoc 'rustc compilation-error-regexp-alist-alist)))

(ert-deftest lang-rust/compilation-parser-panic ()
  "P7: rust-panic compilation parser should be registered."
  (require 'compile)
  (should (assoc 'rust-panic compilation-error-regexp-alist-alist)))

(ert-deftest lang-rust/compilation-parser-stacktrace ()
  "P8: rust-stacktrace compilation parser should be registered."
  (require 'compile)
  (should (assoc 'rust-stacktrace compilation-error-regexp-alist-alist)))

;;; P9: Org babel integration (rust blocks use rust-ts-mode)
;; This is configured in mod-org.el, verify the setting exists

(ert-deftest lang-rust/org-babel-rust-mode ()
  "P9: Org babel should map rust blocks to rust-ts-mode."
  (require 'org-src)
  ;; Check both rs and rust are mapped
  (let ((rs-entry (assoc "rs" org-src-lang-modes))
        (rust-entry (assoc "rust" org-src-lang-modes)))
    (should (or rs-entry rust-entry))
    (when rs-entry
      (should (eq (cdr rs-entry) 'rust-ts)))
    (when rust-entry
      (should (eq (cdr rust-entry) 'rust-ts)))))

(provide 'lang-rust-tests)

;;; lang-rust/tests.el ends here
