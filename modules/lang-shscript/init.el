;;; init.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Shell script editing with Tree-sitter, auto-executable, and file templates.

;;; Code:

(require '+autoloads)
(require 'cl-lib)

(add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))

(define-advice sh-set-shell (:around (fn &rest args) silence-messages)
  (cl-letf (((symbol-function 'message) #'ignore))
    (apply fn args)))

(+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; init.el ends here
