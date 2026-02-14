;;; lang-markdown/lib.el --- Markdown library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for Markdown support.

;;; Code:

(require 'markdown-mode)

;;; Faces for GitHub Flavored Markdown callouts

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


;;;###autoload
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

;;;###autoload
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
                            (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->") 0 '+markdown-prettier-ignore-comment-face prepend)
                            (,(rx (group "$" (or (seq "{" (1+ (any alnum "_")) "}")
                                                 (seq (any upper "_") (1+ (any upper digit "_"))))))
                             1 'font-lock-constant-face prepend))))



;;; lib.el ends here
