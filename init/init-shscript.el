;;; init-shscript.el --- Shell scripting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package sh-script
  :init
  (add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))
  :config
  (eval-and-compile
    (define-advice sh-set-shell (:around (fn &rest args) silence-messages)
      (cl-letf (((symbol-function 'message) #'ignore))
        (apply fn args)))))

(use-package +file-templates
  :config
  (+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld"))


;; Make shell-scripts etc executable on save.

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'init-shscript)

;;; init-shscript.el ends here
