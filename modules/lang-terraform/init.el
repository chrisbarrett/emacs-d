;;; lang-terraform/init.el --- Terraform and HCL language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Terraform and HCL development with specialized formatters and templates.
;; - terraform-mode for *.tf files
;; - hcl-mode for *.hcl files (Terragrunt, Packer)
;; - OpenTofu formatter for Terraform files
;; - Terragrunt formatter for HCL files
;; - File templates for Terragrunt configurations

;;; Code:

(require '+autoloads)

(require '+corelib)

(use-package hcl-mode
  :mode "\\.hcl\\'"
  :preface
  (defun +hcl-mode-setup ()
    (add-hook '+find-sibling-functions #'+terragrunt-find-sibling-file-func nil t))
  :init
  (add-hook 'hcl-mode-hook #'+hcl-mode-setup))

(add-to-list 'auto-mode-alist (cons (rx "/.terragrunt-filters" eos) 'conf-space-mode))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package nerd-icons
  :defer t
  :config
  (alist-set! nerd-icons-mode-icon-alist 'terraform-mode
              '(nerd-icons-mdicon "nf-md-terraform" :face nerd-icons-purple-alt))
  (alist-set! nerd-icons-mode-icon-alist 'hcl-mode
              '(nerd-icons-mdicon "nf-md-terraform" :face nerd-icons-purple-alt))
  (alist-set! nerd-icons-extension-icon-alist "hcl"
              '(nerd-icons-mdicon "nf-md-terraform" :face nerd-icons-purple-alt))
  ;; `nerd-icons-cache' memoises lookups; clear stale entries so post-load
  ;; alist mutations take effect.
  (dolist (fn '(nerd-icons-icon-for-mode
                nerd-icons-icon-for-extension
                nerd-icons-icon-for-file))
    (when-let* ((f (and (fboundp fn) (symbol-function fn)))
                ((byte-code-function-p f))
                (consts (aref f 2)))
      (cl-loop for c across consts
               when (hash-table-p c) do (clrhash c)))))

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
(+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
(+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
(+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld")



;;; init.el ends here
