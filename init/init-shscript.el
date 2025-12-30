;;; init-shscript.el --- Shell scripting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package sh-script
  :init
  (add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))
  :config
  (define-advice sh-set-shell (:around (fn &rest args) silence-messages)
    (cl-letf (((symbol-function 'message) #'ignore))
      (apply fn args))))


;; Make shell-scripts etc executable on save.

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'init-shscript)

;;; init-shscript.el ends here
