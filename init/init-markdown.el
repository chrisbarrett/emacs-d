;;; init-markdown.el --- Markdown document editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)


(use-package markdown-mode
  :general-config
  (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  (:keymaps 'markdown-mode-map :states 'normal "SPC n s" #'markdown-narrow-to-subtree)

  :hook ((markdown-mode-hook . visual-line-mode)
         (markdown-ts-mode-hook . visual-line-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)

  :config
  (+local-leader-set-key 'markdown-mode-map
    "l" '(markdown-toggle-url-hiding :wk "toggle URLs")
    "f" '(markdown-insert-footnote :wk "insert footnote")))


;; Define a more reasonable TAB key.

(use-package markdown-mode
  :config
  (defun +markdown-tab-dwim ()
    "Try to expand a snippet, otherwise fall back to `markdown-cycle'.
This function first attempts tempel snippet expansion. If no snippet
expansion occurs, it falls back to the default `markdown-cycle' behavior."
    (interactive)
    (or (when (fboundp 'tempel-expand)
          (condition-case nil
              (tempel-expand t)
            (user-error nil)))
        (markdown-cycle)))

  :general-config (:states 'insert
                   :keymaps '(markdown-mode-map gfm-mode-map)
                   "TAB" #'+markdown-tab-dwim))

;; Prefer `gfm-mode' in git repos.

(use-package markdown-mode
  :commands (gfm-mode)
  :preface
  (autoload 'vc-git-root "vc-git")
  (defun +markdown-ts-mode-maybe-gfm ()
    "Use gfm-mode in git repos, markdown-ts-mode otherwise."
    (if (and buffer-file-name (vc-git-root buffer-file-name))
        (gfm-mode)
      (markdown-ts-mode)))
  :init
  (add-to-list 'major-mode-remap-alist '(markdown-ts-mode . +markdown-ts-mode-maybe-gfm)))


;;; GitHub Flavored Markdown Callout Support

;; Define faces for GitHub Flavored Markdown callouts

(defface +markdown-gfm-callout-note-face
  '((t :inherit font-lock-operator-face :weight semibold))
  "Face for [!NOTE] callout markers."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-face
  '((t :inherit font-lock-keyword-face :weight semibold))
  "Face for [!TIP] callout markers."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-face
  '((t :inherit font-lock-warning-face :weight semibold))
  "Face for [!IMPORTANT] callout markers."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-face
  '((t :inherit warning :weight semibold))
  "Face for [!WARNING] callout markers."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-face
  '((t :inherit error :weight semibold))
  "Face for [!CAUTION] callout markers."
  :group 'markdown-faces)

(defface +markdown-prettier-ignore-comment-face
  '((t :inherit shadow :weight light))
  "Face for prettier-ignore comments."
  :group 'markdown-faces)


;; Add font-lock support for GitHub Flavored Markdown callouts

(use-package markdown-mode
  :config
  (eval-and-compile
    (defun +markdown-fontify-gfm-callouts ()
      "Add font-lock keywords for GFM callout syntax."
      (font-lock-add-keywords nil
                              `(
                                (,(rx bol "> " "[!" (group "NOTE") "]") 1 '+markdown-gfm-callout-note-face prepend)
                                (,(rx bol "> " "[!" (group "TIP") "]") 1 '+markdown-gfm-callout-tip-face prepend)
                                (,(rx bol "> " "[!" (group "IMPORTANT") "]") 1 '+markdown-gfm-callout-important-face prepend)
                                (,(rx bol "> " "[!" (group "WARNING") "]") 1 '+markdown-gfm-callout-warning-face prepend)
                                (,(rx bol "> " "[!" (group "CAUTION") "]") 1 '+markdown-gfm-callout-caution-face prepend)
                                (,(rx bol "> " "[!" (group "CRITICAL") "]") 1 '+markdown-gfm-callout-caution-face prepend)
                                (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-start" (1+ space) "-->") 0 '+markdown-prettier-ignore-comment-face prepend)
                                (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->") 0 '+markdown-prettier-ignore-comment-face prepend)))))

  (add-hook 'markdown-mode-hook #'+markdown-fontify-gfm-callouts)
  (add-hook 'gfm-mode-hook #'+markdown-fontify-gfm-callouts))



;;; Formatting

;; Use deno where available--it handles critical things like GFM callouts and
;; YAML frontmatter much better than prettier.

(use-package apheleia
  :defines apheleia-formatters
  :config
  (setf (alist-get 'deno-markdown apheleia-formatters)
        '("deno" "fmt" "--prose-wrap" "always" (apheleia-formatters-fill-column "--line-width") "--ext=md" "-"))

  (setf (alist-get 'prettier-markdown apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser=markdown" "--prose-wrap" "always" (apheleia-formatters-fill-column "--print-width")))

  (add-hook! (markdown-mode-local-vars markdown-ts-mode-local-vars)
    (setq-local apheleia-formatter
                (if (executable-find "deno")
                    'deno-markdown
                  'prettier-markdown))))

(provide 'init-markdown)

;;; init-markdown.el ends here
