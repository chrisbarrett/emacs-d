;;; init-search.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Defines search+replace functionality, including `occur'.
(use-package replace
  :hook
  (occur-mode-hook . hl-line-mode))


;; Buffers showing filesystem search results. The default program is grep;
;; change it to ripgrep.
(use-package grep
  :custom
  (grep-use-headings t)
  (grep-template "rg --line-number --with-filename --null --regexp <R> <F>"))


;; Adds a mode for grep-like results buffers that allows you to edit the
;; underlying files directly.
;;
;; TODO: Replace with built-in `grep-edit-mode' once I'm on Emacs 31.
(use-package wgrep :ensure t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))


(use-package xref
  ;; Provides the interface for navigating symbol definitions & references, etc.
  :custom
  (xref-search-program 'ripgrep))


(provide 'init-search)

;;; init-search.el ends here
