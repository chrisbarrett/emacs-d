;;; lang-markdown/lib.el --- Markdown library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for Markdown support.

;;; Code:

(require 'markdown-mode)

;;; Faces for GitHub Flavored Markdown callouts

(defface +markdown-gfm-callout-note-face
  '((((background dark))  :foreground "#89b4fa" :weight semibold)
    (((background light)) :foreground "#1e66f5" :weight semibold)
    (t :inherit font-lock-keyword-face :weight semibold))
  "Face for [!NOTE] callout markers (blue)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-face
  '((((background dark))  :foreground "#a6e3a1" :weight semibold)
    (((background light)) :foreground "#40a02b" :weight semibold)
    (t :inherit success :weight semibold))
  "Face for [!TIP] callout markers (green)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-face
  '((((background dark))  :foreground "#cba6f7" :weight semibold)
    (((background light)) :foreground "#8839ef" :weight semibold)
    (t :inherit font-lock-keyword-face :weight semibold))
  "Face for [!IMPORTANT] callout markers (purple)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-face
  '((((background dark))  :foreground "#fab387" :weight semibold)
    (((background light)) :foreground "#fe640b" :weight semibold)
    (t :inherit warning :weight semibold))
  "Face for [!WARNING] callout markers (orange)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-face
  '((((background dark))  :foreground "#f38ba8" :weight semibold)
    (((background light)) :foreground "#d20f39" :weight semibold)
    (t :inherit error :weight semibold))
  "Face for [!CAUTION] callout markers (red)."
  :group 'markdown-faces)

(defface +markdown-prettier-ignore-comment-face
  '((t :inherit shadow :weight light))
  "Face for prettier-ignore comments."
  :group 'markdown-faces)

(defface +markdown-overlay-border-face
  '((((background dark))  :foreground "#6c7086")
    (((background light)) :foreground "#b9b2a3")
    (t :inherit shadow))
  "Face for overlay borders around fenced code blocks and tables."
  :group 'markdown-faces)

;;;###autoload
(defun +markdown-style-header-faces ()
  "Set markdown header faces to bold on TTY, semi-bold on graphic frames.
TTY fonts often lack a true semi-bold weight, so fall back to plain
bold there to keep the headings legible."
  (let ((spec '((((type tty)) :weight bold)
                (t :weight semi-bold))))
    (dolist (face '(markdown-header-face
                    markdown-header-face-1
                    markdown-header-face-2
                    markdown-header-face-3
                    markdown-header-face-4
                    markdown-header-face-5
                    markdown-header-face-6))
      (face-spec-set face spec))))


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
