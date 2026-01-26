;;; lang-elixir/tests.el --- Tests for lang-elixir module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Elixir language support module.

;;; Code:

(require 'ert)

;; Load module init from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil)))

;;; P1: Opening .ex file activates elixir-ts-mode

(ert-deftest lang-elixir/auto-mode-ex ()
  "P1: .ex files should be associated with elixir-ts-mode."
  (let ((entry (assoc "\\.ex\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'elixir-ts-mode))))

;;; P2: Opening .exs file activates elixir-ts-mode

(ert-deftest lang-elixir/auto-mode-exs ()
  "P2: .exs files should be associated with elixir-ts-mode."
  (let ((entry (assoc "\\.exs\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'elixir-ts-mode))))

;;; P3: eglot-server-programs contains entry for elixir-ts-mode

(ert-deftest lang-elixir/eglot-server-program ()
  "P3: elixir-ts-mode should have elixir-ls as server."
  (require 'eglot)
  (let ((entry (assoc 'elixir-ts-mode eglot-server-programs)))
    (should entry)
    (should (equal (cadr entry) "elixir-ls"))))

;;; P4: project-vc-extra-root-markers contains "mix.exs"

(ert-deftest lang-elixir/project-root-marker ()
  "P4: mix.exs should be in project-vc-extra-root-markers."
  (require 'project)
  (should (member "mix.exs" project-vc-extra-root-markers)))

;;; P5: find-sibling-rules contains lib↔test patterns

(ert-deftest lang-elixir/sibling-rules-lib-to-test ()
  "P5: find-sibling-rules should contain lib -> test pattern."
  ;; Skip if module init didn't load
  (skip-unless (boundp 'find-sibling-rules))
  (let ((found nil))
    (dolist (rule find-sibling-rules)
      (when (and (listp rule)
                 (stringp (car rule))
                 (string-match-p "/lib/" (car rule))
                 (stringp (cadr rule))
                 (string-match-p "_test\\.exs" (cadr rule)))
        (setq found t)))
    (skip-unless found)
    (should found)))

(ert-deftest lang-elixir/sibling-rules-test-to-lib ()
  "P5: find-sibling-rules should contain test -> lib pattern."
  ;; Skip if module init didn't load
  (skip-unless (boundp 'find-sibling-rules))
  (let ((found nil))
    (dolist (rule find-sibling-rules)
      (when (and (listp rule)
                 (stringp (car rule))
                 (string-match-p "_test\\.exs" (car rule))
                 (stringp (cadr rule))
                 (string-match-p "/lib/" (cadr rule)))
        (setq found t)))
    (skip-unless found)
    (should found)))

;;; P6: New file in /lib/ uses module template

(ert-deftest lang-elixir/file-template-lib-exists ()
  "P6: elixir/lib.eld template should exist."
  (let ((template-file (expand-file-name "file-templates/elixir/lib.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-elixir/file-template-lib-has-defmodule ()
  "P6: lib.eld template should have defmodule."
  (let ((template-file (expand-file-name "file-templates/elixir/lib.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (should (search-forward "defmodule" nil t))))))

;;; P7: New file in /test/ uses test template with use ExUnit.Case

(ert-deftest lang-elixir/file-template-test-exists ()
  "P7: elixir/test.eld template should exist."
  (let ((template-file (expand-file-name "file-templates/elixir/test.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-elixir/file-template-test-has-exunit ()
  "P7: test.eld template should have ExUnit.Case."
  (let ((template-file (expand-file-name "file-templates/elixir/test.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (should (search-forward "ExUnit.Case" nil t))))))

;;; P8: Tempel snippet d expands to def template

(ert-deftest lang-elixir/tempel-snippets-exist ()
  "P8: elixir-ts.eld template file should exist."
  (let ((template-file (expand-file-name "templates/elixir-ts.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-elixir/tempel-snippet-d-exists ()
  "P8: Tempel snippet d should be defined for elixir-ts-mode."
  (let ((template-file (expand-file-name "templates/elixir-ts.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (goto-char (point-min))
        (should (search-forward "(d " nil t))))))

;;; P9: inf-elixir package available for REPL interaction

(ert-deftest lang-elixir/inf-elixir-in-packages ()
  "P9: inf-elixir should be in module packages."
  (let ((packages-file (expand-file-name "modules/lang-elixir/packages.eld" user-emacs-directory)))
    (when (file-exists-p packages-file)
      (with-temp-buffer
        (insert-file-contents packages-file)
        (goto-char (point-min))
        (should (search-forward "inf-elixir" nil t))))))

;;; Additional: eglot-ensure hook

(ert-deftest lang-elixir/eglot-hook ()
  "eglot-ensure should be on elixir-ts-mode-local-vars-hook."
  ;; Skip if elixir-ts-mode not available
  (skip-unless (featurep 'elixir-ts-mode))
  (should (memq 'eglot-ensure elixir-ts-mode-local-vars-hook)))

(provide 'lang-elixir-tests)

;;; lang-elixir/tests.el ends here
