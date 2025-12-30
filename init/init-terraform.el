;;; init-terraform.el --- Configuration for terraform & HCL dialects -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package hcl-mode :ensure t
  :mode ("\\.hcl\\'")
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-ignores ".drift-history.json"))
  :config
  (with-eval-after-load 'apheleia-formatters
    (add-to-list 'apheleia-formatters '(terragrunt . ("terragrunt" "hcl" "fmt" "--stdin")))
    (alist-set! apheleia-mode-alist 'hcl-mode '(terragrunt hclfmt))))

(use-package terraform-mode :ensure t
  :mode ("\\.tf\\'")
  :config
  ;; Use `tofu' for formatting terraform files if on PATH.
  (with-eval-after-load 'apheleia-formatters
    (add-to-list 'apheleia-formatters '(opentofu . ("tofu" "fmt" "-")))
    (alist-set! apheleia-mode-alist 'terraform-mode 'opentofu)))


(provide 'init-terraform)

;;; init-terraform.el ends here
