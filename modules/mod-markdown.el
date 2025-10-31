;;; mod-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'markdown-mode)
(require 'general)

;;; Keybindings

(+local-leader-set-key 'markdown-mode-map
  "l" '(markdown-toggle-url-hiding :wk "toggle URLs")
  "f" '(markdown-insert-footnote :wk "insert footnote"))

;;; Formatting

(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-formatters '(prettier-markdown . ("prettier" "--stdin-filepath" filepath "--parser=markdown" "--prose-wrap" "always" (apheleia-formatters-fill-column "--print-width"))))
  (alist-set! apheleia-mode-alist 'markdown-mode 'prettier-markdown)
  (alist-set! apheleia-mode-alist 'gfm-mode 'prettier-markdown)
  (alist-set! apheleia-mode-alist 'markdown-ts-mode 'prettier-markdown))

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

;; Add font-lock support for GitHub Flavored Markdown callouts
(defun +markdown-fontify-gfm-callouts ()
  "Add font-lock keywords for GFM callout syntax."
  (font-lock-add-keywords nil
                          `(;; NOTE - informational (cyan, semibold)
                            (,(rx bol "> " "[!" (group "NOTE") "]") 1 '+markdown-gfm-callout-note-face prepend)
                            ;; TIP - helpful suggestion (keyword color, semibold)
                            (,(rx bol "> " "[!" (group "TIP") "]") 1 '+markdown-gfm-callout-tip-face prepend)
                            ;; IMPORTANT - critical information (warning color, semibold)
                            (,(rx bol "> " "[!" (group "IMPORTANT") "]") 1 '+markdown-gfm-callout-important-face prepend)
                            ;; WARNING - potential problems (warning face, semibold)
                            (,(rx bol "> " "[!" (group "WARNING") "]") 1 '+markdown-gfm-callout-warning-face prepend)
                            ;; CAUTION - danger/severe issues (error face, semibold)
                            (,(rx bol "> " "[!" (group "CAUTION") "]") 1 '+markdown-gfm-callout-caution-face prepend))))

(add-hook 'markdown-mode-hook #'+markdown-fontify-gfm-callouts)
(add-hook 'gfm-mode-hook #'+markdown-fontify-gfm-callouts)

(provide 'mod-markdown)

;;; mod-markdown.el ends here
