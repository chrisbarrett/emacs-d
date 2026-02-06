;;; lang-markdown/init.el --- Markdown language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown and GitHub Flavored Markdown (GFM) editing support with visual
;; enhancements, smart formatting, and callout highlighting.

;;; Code:

(require '+autoloads)
(require '+corelib)

;;; Mode configuration

(use-package markdown-mode
  :commands (gfm-mode)
  :init
  ;; Directly associate markdown files with gfm-mode for GitHub compatibility
  ;; Use add-to-list to prepend, overriding built-in markdown-ts-mode-maybe
  (add-to-list 'auto-mode-alist `(,(rx "." (or "md" "markdown" "mkd" "mdown" "mkdn") eos) . gfm-mode))
  ;; Associate /prompt files with gfm-mode (Claude prompt files)
  (add-to-list 'auto-mode-alist `(,(rx "/prompt" eos) . gfm-mode))
  ;; Remap any remaining markdown-mode calls to gfm-mode
  (alist-set! major-mode-remap-alist 'markdown-mode 'gfm-mode)

  :general-config
  (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  (:keymaps 'markdown-mode-map :states 'normal "SPC n s" #'markdown-narrow-to-subtree)
  (:states 'insert
   :keymaps '(markdown-mode-map gfm-mode-map)
   "TAB" #'+markdown-tab-dwim)

  :hook
  (gfm-mode-hook . visual-line-mode)
  (gfm-mode-hook . +markdown-fontify-gfm-callouts)

  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)

  :config
  (+local-leader-set-key 'markdown-mode-map
    "l" '(markdown-toggle-url-hiding :wk "toggle URLs")
    "f" '(markdown-insert-footnote :wk "insert footnote")))

;;; Formatting

(use-package apheleia
  :defines apheleia-formatters
  :config
  (setf (alist-get 'deno-markdown apheleia-formatters)
        '("deno" "fmt" "--prose-wrap" "always" (apheleia-formatters-fill-column "--line-width") "--ext=md" "-"))

  (setf (alist-get 'prettier-markdown apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser=markdown" "--prose-wrap" "always" (apheleia-formatters-fill-column "--print-width")))

  (add-hook! (gfm-mode-local-vars)
    (setq-local apheleia-formatter
                (if (executable-find "deno")
                    'deno-markdown
                  'prettier-markdown))))

;;; init.el ends here
