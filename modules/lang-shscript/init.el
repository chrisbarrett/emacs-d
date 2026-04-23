;;; init.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)
(require 'cl-lib)

(add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))
(alist-set! major-mode-remap-alist 'sh-mode 'bash-ts-mode)

(define-advice sh-set-shell (:around (fn &rest args) silence-messages)
  (cl-letf (((symbol-function 'message) #'ignore))
    (apply fn args)))

(+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; argc-mode — fontify argc directives

(defun +argc-maybe-enable ()
  "Enable `argc-mode' if an argc directive appears in the first 50 lines."
  (unless (or (bound-and-true-p argc-mode)
              (buffer-base-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((limit (save-excursion (forward-line 50) (point))))
          (when (re-search-forward
                 (rx bol (* space) "#" (+ space)
                     "@" (or "describe" "cmd" "alias" "arg" "option"
                             "flag" "env" "meta"))
                 limit t)
            (argc-mode 1)))))))

(add-hook 'sh-mode-hook #'+argc-maybe-enable)
(add-hook 'bash-ts-mode-hook #'+argc-maybe-enable)

;;; Separedit — heredoc language detection for bash-ts-mode

(with-eval-after-load 'separedit
  (add-to-list 'separedit-heredoc-language-regexp-alist
               '(bash-ts-mode . "<<-? *[\\\\'\"]?_*\\([[:alnum:]]+\\)_*[\"']?\\(?:.*\\)?"))
  (dolist (entry '(("json" . json-ts-mode)
                   ("yaml" . yaml-ts-mode)
                   ("nix" . nix-ts-mode)
                   ("python" . python-ts-mode)
                   ("ruby" . ruby-ts-mode)
                   ("bash" . bash-ts-mode)
                   ("js" . js-ts-mode)
                   ("css" . css-ts-mode)))
    (add-to-list 'separedit-code-lang-modes entry)))

;;; init.el ends here
