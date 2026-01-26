;;; tests.el --- Tests for compile module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for compilation mode configuration and custom error parsers.

;;; Code:

(require 'ert)

(defconst compile-module--dir
  (expand-file-name "modules/compile/" user-emacs-directory)
  "Directory containing the compile module.")

;;; P1: comint-prompt-read-only is t

(ert-deftest compile-p1-comint-prompt-read-only ()
  "P1: comint-prompt-read-only should be t."
  (require 'comint)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq t comint-prompt-read-only)))

;;; P2: compilation-always-kill is t

(ert-deftest compile-p2-compilation-always-kill ()
  "P2: compilation-always-kill should be t."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq t compilation-always-kill)))

;;; P3: compilation-ask-about-save is nil

(ert-deftest compile-p3-compilation-ask-about-save ()
  "P3: compilation-ask-about-save should be nil."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq nil compilation-ask-about-save)))

;;; P4: compilation-scroll-output is first-error

(ert-deftest compile-p4-compilation-scroll-output ()
  "P4: compilation-scroll-output should be first-error."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq 'first-error compilation-scroll-output)))

;;; P5: compilation-error-regexp-alist contains all custom parser names

(ert-deftest compile-p5-custom-parsers-registered ()
  "P5: compilation-error-regexp-alist should contain custom parser names."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; Check some key parsers are present
  (should (memq 'generic compilation-error-regexp-alist))
  (should (memq 'generic-no-message compilation-error-regexp-alist))
  (should (memq 'rustc compilation-error-regexp-alist))
  (should (memq 'zig compilation-error-regexp-alist))
  (should (memq 'typescript-tsc compilation-error-regexp-alist))
  (should (memq 'terraform compilation-error-regexp-alist))
  (should (memq 'terragrunt compilation-error-regexp-alist))
  (should (memq 'elixirc compilation-error-regexp-alist)))

;;; P6: Built-in parsers are disabled

(ert-deftest compile-p6-no-builtin-parsers ()
  "P6: Built-in parsers should be disabled (custom list only)."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; The list should not contain built-in parser names like:
  ;; gnu, gcc-include, lcc, gcov-file, etc.
  (should-not (memq 'gnu compilation-error-regexp-alist))
  (should-not (memq 'gcc-include compilation-error-regexp-alist))
  (should-not (memq 'lcc compilation-error-regexp-alist))
  (should-not (memq 'gcov-file compilation-error-regexp-alist))
  (should-not (memq 'gcov-header compilation-error-regexp-alist))
  (should-not (memq 'gcov-nomark compilation-error-regexp-alist))
  (should-not (memq 'perl compilation-error-regexp-alist))
  (should-not (memq 'python compilation-error-regexp-alist))
  (should-not (memq 'ruby compilation-error-regexp-alist)))

;;; P7: ansi-color-compilation-filter is in compilation-filter-hook

(ert-deftest compile-p7-ansi-color-in-filter-hook ()
  "P7: ansi-color-compilation-filter should be in compilation-filter-hook."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (memq 'ansi-color-compilation-filter compilation-filter-hook)))

;;; P8: define-compilation-error-rx produces valid entries

(ert-deftest compile-p8-macro-produces-valid-entries ()
  "P8: define-compilation-error-rx should produce valid alist entries."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; Check that the parsers have proper structure
  (dolist (name compilation-error-regexp-alist)
    (let ((entry (alist-get name compilation-error-regexp-alist-alist)))
      ;; Each entry should be a list starting with a regexp string
      (should (listp entry))
      (should (stringp (car entry))))))

;;; P9: RET keybinding configured

(ert-deftest compile-p9-ret-keybinding-hook ()
  "P9: RET keybinding hook should be registered."
  (skip-unless (featurep 'general))
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (memq '+compilation-ensure-keybindings compilation-mode-hook)))

;;; Additional tests

(ert-deftest compile-url-fontify-in-filter-hook ()
  "URL fontification should be in compilation-filter-hook."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (memq '+compilation-fontify-urls compilation-filter-hook)))

(ert-deftest compile-comint-buffer-maximum-size ()
  "comint-buffer-maximum-size should be doubled from default."
  (require 'comint)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq 2048 comint-buffer-maximum-size)))

(ert-deftest compile-compilation-message-face ()
  "compilation-message-face should be default."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (eq 'default compilation-message-face)))

(ert-deftest compile-parsers-have-defconst ()
  "Custom parsers should have associated defconst forms."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; Sample some parsers to verify they have their +compile-form-- defconst
  (should (boundp '+compile-form--generic))
  (should (boundp '+compile-form--rustc))
  (should (boundp '+compile-form--zig))
  (should (boundp '+compile-form--terraform)))

(ert-deftest compile-terragrunt-transform-rules ()
  "Terragrunt path transforms should be configured."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; Check transformation rules exist - assoc returns the cons cell if found
  (should (assoc (rx "/.terragrunt-stack/") compilation-transform-file-match-alist))
  (should (assoc (rx "/.terragrunt-stack" eos) compilation-transform-file-match-alist)))

(ert-deftest compile-metavars-alist-defined ()
  "+compile-metavars-alist should be defined with file, line, col, message."
  (load (expand-file-name "compile-lib.el" compile-module--dir) nil t)
  (should (boundp '+compile-metavars-alist))
  (should (assq 'file +compile-metavars-alist))
  (should (assq 'line +compile-metavars-alist))
  (should (assq 'col +compile-metavars-alist))
  (should (assq 'message +compile-metavars-alist)))

(ert-deftest compile-pp-parser-defined ()
  "+compile-pp-parser should be a command."
  (load (expand-file-name "compile-lib.el" compile-module--dir) nil t)
  (should (fboundp '+compile-pp-parser))
  (should (commandp '+compile-pp-parser)))

;;; Module structure tests

(ert-deftest compile-module-has-packages-eld ()
  "Module should have packages.eld file."
  (should (file-exists-p (expand-file-name "packages.eld" compile-module--dir))))

(ert-deftest compile-module-has-spec ()
  "Module should have spec.md symlink."
  (should (file-exists-p (expand-file-name "spec.md" compile-module--dir))))

(ert-deftest compile-module-has-lib ()
  "Module should have compile-lib.el file."
  (should (file-exists-p (expand-file-name "compile-lib.el" compile-module--dir))))

(provide 'compile-tests)

;;; tests.el ends here
