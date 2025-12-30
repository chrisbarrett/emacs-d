;;; init-templates.el --- Templating & snippets -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(defvar +templates-dir (file-name-concat user-emacs-directory "templates/"))

;; Text snippets.
(use-package tempel :ensure t
  ;; NB. Field navigation uses M-{ and M-}.
  :custom
  (tempel-path (file-name-concat +templates-dir "*.eld"))
  :init
  (add-hook! '(prog-mode-hook text-mode-hook config-mode-hook)
    (add-hook 'completion-at-point-functions #'tempel-expand -90 t))

  :config
  (add-hook '+escape-hook
            (defun +tempel-esc-exit-h ()
              (when (tempel--active-p nil (current-buffer))
                (tempel-done)
                t))))


(use-package autoinsert
  :after-call +first-buffer-hook +first-file-hook
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "file-templates/"))
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode +1))


(use-package +file-templates
  :after autoinsert
  :demand t
  :config
  (require 'string-inflection nil t)

  (+define-file-template (rx ".el" eos) "emacs-lisp.eld")
  (+define-file-template (rx "flake.nix" eos) "flake.eld")

  (+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
  (+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
  (+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld")
  (+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

  (+define-file-template-dispatcher 'elixir-ts-mode
    ((string-match-p "/lib/" (buffer-file-name))
     "elixir/lib.eld")
    ((string-match-p (rx "/test/" (+? nonl) ".exs" eos) (buffer-file-name))
     "elixir/test.eld"))

  (defun +cdk-project-p (&optional dir)
    (locate-dominating-file (or dir default-directory) "cdk.json"))

  (defun +index-ts-p (file)
    (equal "index.ts" (file-name-nondirectory (buffer-file-name))))

  (+define-file-template-dispatcher 'typescript-ts-mode
    ((and (string-match-p "construct" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/construct.eld")
    ((and (string-match-p "/stacks/" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/stack.eld")))


(provide 'init-templates)

;;; init-templates.el ends here
