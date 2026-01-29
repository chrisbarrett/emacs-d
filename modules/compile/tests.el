;;; tests.el --- Tests for compile module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for compilation mode configuration and custom error parsers.

;;; Code:

(require 'ert)
(require 'compile)

(defconst compile-module--dir
  (expand-file-name "modules/compile/" user-emacs-directory)
  "Directory containing the compile module.")

;;; P5: compilation-error-regexp-alist contains all custom parser names

(ert-deftest compile-p5-custom-parsers-registered ()
  "P5: `compilation-error-regexp-alist' should contain custom parser names."
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
  "P7: `ansi-color-compilation-filter' should be in `compilation-filter-hook'."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  (should (memq 'ansi-color-compilation-filter compilation-filter-hook)))

;;; P8: define-compilation-error-rx produces valid entries

(ert-deftest compile-p8-macro-produces-valid-entries ()
  "P8: `define-compilation-error-rx' should produce valid alist entries."
  (require 'compile)
  (load (expand-file-name "init.el" compile-module--dir) nil t)
  ;; Check that the parsers have proper structure
  (dolist (name compilation-error-regexp-alist)
    (let ((entry (alist-get name compilation-error-regexp-alist-alist)))
      ;; Each entry should be a list starting with a regexp string
      (should (listp entry))
      (should (stringp (car entry))))))

;;; tests.el ends here
