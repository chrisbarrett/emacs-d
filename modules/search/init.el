;;; init.el --- Search module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures grep, occur, wgrep, and xref with ripgrep backend.
;; Wgrep enables editing grep buffers directly; changes auto-save on apply.

;;; Code:

(require '+autoloads)

;; Occur: enable hl-line-mode in occur buffers.
(with-eval-after-load 'replace
  (add-hook 'occur-mode-hook #'hl-line-mode))

;; Grep: use ripgrep with grouped headings.
(with-eval-after-load 'grep
  (setq grep-use-headings t)
  (setq grep-template "rg --line-number --with-filename --null --regexp <R> <F>"))

;; Wgrep: editable grep buffers with auto-save.
;; TODO: Replace with built-in `grep-edit-mode' once on Emacs 31.
(with-eval-after-load 'wgrep
  (setq wgrep-auto-save-buffer t))

;; Xref: use ripgrep for project-wide searches.
(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep))



;;; init.el ends here
