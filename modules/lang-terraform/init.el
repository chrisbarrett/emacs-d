;;; lang-terraform/init.el --- Terraform and HCL language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Terraform and HCL development with specialized formatters and templates.
;; - terraform-mode for *.tf files
;; - hcl-mode for *.hcl files (Terragrunt, Packer)
;; - OpenTofu formatter for Terraform files
;; - Terragrunt formatter for HCL files
;; - File templates for Terragrunt configurations

;;; Code:

(require '+corelib)

(use-package hcl-mode
  :mode "\\.hcl\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

;; Ignore Gruntwork Pipelines drift history files.
(use-package project
  :config
  (pushnew! project-vc-ignores ".drift-history.json"))

(use-package apheleia
  :defines (apheleia-formatters apheleia-mode-alist)
  :config
  ;; Use `tofu' for formatting terraform files if on PATH.
  (add-to-list 'apheleia-formatters '(opentofu . ("tofu" "fmt" "-")))
  (alist-set! apheleia-mode-alist 'terraform-mode 'opentofu)

  ;; Use `terragrunt' to format HCL files.
  (add-to-list 'apheleia-formatters '(terragrunt . ("terragrunt" "hcl" "fmt" "--stdin")))
  (alist-set! apheleia-mode-alist 'hcl-mode '(terragrunt hclfmt)))

;; File templates for Terragrunt configurations
(use-package +file-templates
  :config
  (+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
  (+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
  (+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld"))

(provide 'lang-terraform-init)

;;; lang-terraform/init.el ends here
