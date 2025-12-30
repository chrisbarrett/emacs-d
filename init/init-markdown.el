;;; init-markdown.el --- Markdown document editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package markdown-mode
  :general-config
  (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  (:keymaps 'markdown-mode-map :states 'normal "SPC n s" #'markdown-narrow-to-subtree)

  :hook ((markdown-mode-hook . visual-line-mode)
         (markdown-ts-mode-hook . visual-line-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  :commands (gfm-mode)
  :init
  (defun +markdown-ts-mode-maybe-gfm ()
    "Use gfm-mode in git repos, markdown-ts-mode otherwise."
    (if (and buffer-file-name (vc-git-root buffer-file-name))
        (gfm-mode)
      (markdown-ts-mode)))
  (add-to-list 'major-mode-remap-alist '(markdown-ts-mode . +markdown-ts-mode-maybe-gfm))
  :config
  (use-package mod-markdown :demand t))


(provide 'init-markdown)

;;; init-markdown.el ends here
