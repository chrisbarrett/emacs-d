;;; lang-markdown/lib.el --- Markdown library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for Markdown support.

;;; Code:

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
                            (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->") 0 '+markdown-prettier-ignore-comment-face prepend))))


