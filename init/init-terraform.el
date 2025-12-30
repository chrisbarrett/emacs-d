;;; init-terraform.el --- Configuration for terraform & HCL dialects -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package hcl-mode :ensure t
  :mode ("\\.hcl\\'"))

(use-package terraform-mode :ensure t
  :mode ("\\.tf\\'"))

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


(use-package +file-templates
  :config
  (+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
  (+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
  (+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld"))

(provide 'init-terraform)

;;; init-terraform.el ends here
