;;; init.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Shell script editing with Tree-sitter, auto-executable, file templates,
;; and polymode for syntax-highlighted heredocs.

;;; Code:

(require '+autoloads)
(require 'cl-lib)

(add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))

(define-advice sh-set-shell (:around (fn &rest args) silence-messages)
  (cl-letf (((symbol-function 'message) #'ignore))
    (apply fn args)))

(+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; Polymode — syntax highlighting for heredocs
;;
;; Matches all heredoc quoting styles:
;;   <<DELIM  <<'DELIM'  <<"DELIM"  <<\DELIM  <<-DELIM  etc.

(use-package polymode
  :defer t
  :init
  (defun +bash-poly--heredoc-head-matcher (delim)
    "Return head-matcher regex for bash heredoc with DELIM.
Handles <<, <<-, and quoting with single quotes, double quotes, or backslash."
    (rx-to-string
     `(seq "<<" (? "-")
           (or (seq "'" ,delim "'")
               (seq "\"" ,delim "\"")
               (seq "\\" ,delim)
               ,delim)
           eol)))

  (defun +bash-poly--heredoc-tail-matcher (delim)
    "Return tail-matcher regex for bash heredoc with DELIM."
    (rx-to-string `(seq bol ,delim eol)))

  (define-hostmode poly-bash-ts-hostmode
    :mode 'bash-ts-mode)

  (define-innermode poly-bash-elisp-innermode
    :mode 'emacs-lisp-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "ELISP")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "ELISP")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-python-innermode
    :mode 'python-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "PYTHON")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "PYTHON")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-ruby-innermode
    :mode 'ruby-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "RUBY")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "RUBY")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-sql-innermode
    :mode 'sql-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "SQL")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "SQL")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-json-innermode
    :mode 'json-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "JSON")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "JSON")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-yaml-innermode
    :mode 'yaml-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "YAML")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "YAML")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-html-innermode
    :mode 'html-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "HTML")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "HTML")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-css-innermode
    :mode 'css-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "CSS")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "CSS")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-js-innermode
    :mode 'js-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "JS")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "JS")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-nix-innermode
    :mode 'nix-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "NIX")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "NIX")
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-bash-ts-mode
    :hostmode 'poly-bash-ts-hostmode
    :innermodes '(poly-bash-elisp-innermode
                  poly-bash-python-innermode
                  poly-bash-ruby-innermode
                  poly-bash-sql-innermode
                  poly-bash-json-innermode
                  poly-bash-yaml-innermode
                  poly-bash-html-innermode
                  poly-bash-css-innermode
                  poly-bash-js-innermode
                  poly-bash-nix-innermode))

  (add-to-list 'major-mode-remap-alist '(sh-mode . poly-bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(bash-ts-mode . poly-bash-ts-mode))

  (add-hook 'poly-bash-ts-mode-hook #'+polymode-refontify-inner-spans))

;;; argc-mode — fontify argc directives

(defun +argc-maybe-enable ()
  "Enable `argc-mode' if an argc directive appears in the first 50 lines."
  (unless (bound-and-true-p argc-mode)
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
(add-hook 'poly-bash-ts-mode-hook #'+argc-maybe-enable)

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
