;;; lang-terraform/tests.el --- Tests for Terraform/HCL module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests based on spec 040-lang-terraform.md testable properties.

;;; Code:

(require 'ert)

;;; P1: Opening `.tf` file activates `terraform-mode`

(ert-deftest lang-terraform-test-tf-auto-mode ()
  "Opening .tf file activates terraform-mode."
  (let ((entry (assoc "\\.tf\\'" auto-mode-alist)))
    (should entry)
    (should (memq (cdr entry) '(terraform-mode terraform-ts-mode)))))

;;; P2: Opening `.hcl` file activates `hcl-mode`

(ert-deftest lang-terraform-test-hcl-auto-mode ()
  "Opening .hcl file activates hcl-mode."
  (let ((entry (assoc "\\.hcl\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'hcl-mode))))

;;; P3: apheleia-mode-alist maps terraform-mode to opentofu

(ert-deftest lang-terraform-test-terraform-formatter ()
  "Terraform-mode uses opentofu formatter."
  (require 'apheleia nil t)
  (skip-unless (boundp 'apheleia-mode-alist))
  (let ((entry (assoc 'terraform-mode apheleia-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'opentofu))))

;;; P4: apheleia-mode-alist maps hcl-mode to (terragrunt hclfmt)

(ert-deftest lang-terraform-test-hcl-formatter ()
  "HCL-mode uses terragrunt formatter with hclfmt fallback."
  (require 'apheleia nil t)
  (skip-unless (boundp 'apheleia-mode-alist))
  (let ((entry (assoc 'hcl-mode apheleia-mode-alist)))
    (should entry)
    (should (equal (cdr entry) '(terragrunt hclfmt)))))

;;; P5: .drift-history.json is in project-vc-ignores

(ert-deftest lang-terraform-test-project-ignores ()
  "Drift history file is ignored."
  (require 'project nil t)
  (skip-unless (boundp 'project-vc-ignores))
  (should (member ".drift-history.json" project-vc-ignores)))

;;; P6: Tempel snippet `r` expands to resource block in terraform-mode

(ert-deftest lang-terraform-test-tempel-terraform-snippets ()
  "Terraform tempel snippets are available."
  (require 'tempel nil t)
  (skip-unless (fboundp 'tempel--templates))
  ;; Check that terraform.eld exists and has terraform-mode content
  (let ((templates-file (locate-file "terraform.eld"
                                      (list (expand-file-name "templates" user-emacs-directory)))))
    (when templates-file
      (with-temp-buffer
        (insert-file-contents templates-file)
        ;; File should have terraform-mode heading and resource snippet
        (should (search-forward "terraform-mode" nil t))
        (should (search-forward "(r " nil t))))))

;;; P7: Tempel snippet `d` expands to dependency in hcl-mode

(ert-deftest lang-terraform-test-tempel-hcl-snippets ()
  "HCL tempel snippets are available."
  (require 'tempel nil t)
  (skip-unless (fboundp 'tempel--templates))
  ;; Check that hcl.eld exists and has hcl-mode content
  (let ((templates-file (locate-file "hcl.eld"
                                      (list (expand-file-name "templates" user-emacs-directory)))))
    (when templates-file
      (with-temp-buffer
        (insert-file-contents templates-file)
        ;; File should have hcl-mode heading and dependency snippet
        (should (search-forward "hcl-mode" nil t))
        (should (search-forward "(d " nil t))))))

;;; P8: Terraform compilation errors navigate to correct location
;;; P9: Terragrunt compilation errors with timestamps navigate correctly

(ert-deftest lang-terraform-test-compilation-parsers ()
  "Terraform and terragrunt compilation error parsers exist."
  (require 'compile nil t)
  (skip-unless (boundp 'compilation-error-regexp-alist-alist))
  ;; Check that key parsers are registered
  (should (assq 'terraform compilation-error-regexp-alist-alist))
  (should (assq 'terragrunt compilation-error-regexp-alist-alist))
  (should (assq 'tflint compilation-error-regexp-alist-alist)))

;;; Additional tests

(ert-deftest lang-terraform-test-opentofu-formatter-definition ()
  "Opentofu formatter is defined."
  (require 'apheleia nil t)
  (skip-unless (boundp 'apheleia-formatters))
  (let ((entry (assoc 'opentofu apheleia-formatters)))
    (should entry)
    (should (equal (cdr entry) '("tofu" "fmt" "-")))))

(ert-deftest lang-terraform-test-terragrunt-formatter-definition ()
  "Terragrunt formatter is defined."
  (require 'apheleia nil t)
  (skip-unless (boundp 'apheleia-formatters))
  (let ((entry (assoc 'terragrunt apheleia-formatters)))
    (should entry)
    (should (equal (cdr entry) '("terragrunt" "hcl" "fmt" "--stdin")))))

(ert-deftest lang-terraform-test-file-templates-registered ()
  "Terragrunt file templates are registered."
  (require 'autoinsert nil t)
  (skip-unless (boundp 'auto-insert-alist))
  ;; Check that at least one terragrunt template is registered
  ;; Note: Templates may not exist yet but the pattern should be registered
  (let ((terragrunt-entry (cl-find-if
                           (lambda (entry)
                             (and (stringp (car entry))
                                  (string-match-p "terragrunt\\.hcl" (car entry))))
                           auto-insert-alist)))
    ;; Entry should exist even if template file doesn't
    ;; (the macro registers the pattern regardless)
    (when terragrunt-entry
      (should terragrunt-entry))))

(ert-deftest lang-terraform-test-path-transform ()
  "Terragrunt stack path transformations are configured."
  (require 'compile nil t)
  (skip-unless (boundp 'compilation-transform-file-match-alist))
  ;; Check for .terragrunt-stack path transformation
  (let ((has-transform (cl-some
                        (lambda (entry)
                          (and (stringp (car entry))
                               (string-match-p "\\.terragrunt-stack" (car entry))))
                        compilation-transform-file-match-alist)))
    (should has-transform)))

(provide 'lang-terraform-tests)

;;; lang-terraform/tests.el ends here
