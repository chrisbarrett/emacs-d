;;; lang-markdown/lib.el --- Markdown library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for Markdown support.

;;; Code:

(require 'markdown-mode)

;;; Faces for GitHub Flavored Markdown callouts

(defface +markdown-gfm-callout-note-face
  '((((background dark))  :foreground "#89b4fa" :slant normal)
    (((background light)) :foreground "#1e66f5" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!NOTE] callouts (blue)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-face
  '((((background dark))  :foreground "#a6e3a1" :slant normal)
    (((background light)) :foreground "#40a02b" :slant normal)
    (t :inherit success :slant normal))
  "Header face for [!TIP] callouts (green)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-face
  '((((background dark))  :foreground "#cba6f7" :slant normal)
    (((background light)) :foreground "#8839ef" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!IMPORTANT] callouts (purple)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-face
  '((((background dark))  :foreground "#fab387" :slant normal)
    (((background light)) :foreground "#fe640b" :slant normal)
    (t :inherit warning :slant normal))
  "Header face for [!WARNING] callouts (orange)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-face
  '((((background dark))  :foreground "#f38ba8" :slant normal)
    (((background light)) :foreground "#d20f39" :slant normal)
    (t :inherit error :slant normal))
  "Header face for [!CAUTION]/[!CRITICAL] callouts (red)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-header-face
  '((t :weight semibold))
  "Face merged onto callout marker lines on top of the block face."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-note-body-face
  '((t :slant normal))
  "Body face for [!NOTE] callouts; `:background' set dynamically from theme."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-body-face
  '((t :slant normal))
  "Body face for [!TIP] callouts; `:background' set dynamically from theme."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-body-face
  '((t :slant normal))
  "Body face for [!IMPORTANT] callouts; `:background' set dynamically from theme."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-body-face
  '((t :slant normal))
  "Body face for [!WARNING] callouts; `:background' set dynamically from theme."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-body-face
  '((t :slant normal))
  "Body face for [!CAUTION]/[!CRITICAL] callouts; `:background' set dynamically from theme."
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

(defconst +markdown-gfm-callout-type-face-alist
  '(("NOTE"      . +markdown-gfm-callout-note-face)
    ("TIP"       . +markdown-gfm-callout-tip-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-face)
    ("WARNING"   . +markdown-gfm-callout-warning-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-face))
  "Map of GFM callout type label to its header face.")

(defconst +markdown-gfm-callout-type-body-face-alist
  '(("NOTE"      . +markdown-gfm-callout-note-body-face)
    ("TIP"       . +markdown-gfm-callout-tip-body-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-body-face)
    ("WARNING"   . +markdown-gfm-callout-warning-body-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-body-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-body-face))
  "Map of GFM callout type label to its body (tinted background) face.")

(defun +markdown-gfm-callout--tint-bg (face)
  "Return a hex colour 10% from FACE's foreground toward the theme bg.
Mirrors `gfm-callouts--tinted-bg' so body face background matches the
overlay's tinted panel.  Returns nil if either colour is unresolvable."
  (require 'color)
  (let* ((fg (face-foreground face nil t))
         (bg (or (and (boundp '+theme-default-background)
                      +theme-default-background)
                 (face-background 'default nil t)))
         (fg-rgb (and fg (color-name-to-rgb fg)))
         (bg-rgb (and bg (color-name-to-rgb bg))))
    (when (and fg-rgb bg-rgb)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (b f) (+ b (* 0.1 (- f b))))
                                bg-rgb fg-rgb)
                     '(2))))))

;;;###autoload
(defun +markdown-gfm-callout-refresh-body-faces (&rest _)
  "Recompute `:background' on each callout body face from the current theme."
  (dolist (entry +markdown-gfm-callout-type-body-face-alist)
    (let* ((type (car entry))
           (body-face (cdr entry))
           (header-face (alist-get type +markdown-gfm-callout-type-face-alist
                                   nil nil #'string=))
           (tint (and header-face
                      (+markdown-gfm-callout--tint-bg header-face))))
      (when tint
        (set-face-background body-face tint)))))

(+markdown-gfm-callout-refresh-body-faces)

(add-hook '+theme-changed-hook #'+markdown-gfm-callout-refresh-body-faces)

(defconst +markdown-gfm-callout--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a GFM callout marker line. Group 1 is the type.")

(defun +markdown-gfm-callout--block-end (marker-eol)
  "Return EOL of the last `>'-prefixed line following MARKER-EOL."
  (save-excursion
    (goto-char marker-eol)
    (let ((end marker-eol))
      (forward-line 1)
      (while (and (not (eobp))
                  (eq (char-after) ?>))
        (setq end (line-end-position))
        (forward-line 1))
      end)))

(defun +markdown-gfm-callout--matcher (limit)
  "Font-lock matcher that spans full callout blocks up to LIMIT.
Match data: group 0 = whole block, group 1 = type label, group 2 =
marker line.  Sets `font-lock-multiline' on the matched region so
edits inside the block trigger refontification of the entire block."
  (when (re-search-forward +markdown-gfm-callout--marker-re limit t)
    (let* ((mbeg (match-beginning 0))
           (mend (match-end 0))
           (tbeg (match-beginning 1))
           (tend (match-end 1))
           (block-end (+markdown-gfm-callout--block-end mend)))
      (with-silent-modifications
        (put-text-property mbeg block-end 'font-lock-multiline t))
      (set-match-data (list mbeg block-end tbeg tend mbeg mend))
      t)))

(defun +markdown-gfm-callout--paint-body (type block-beg block-end)
  "Apply TYPE's body face to body content between BLOCK-BEG and BLOCK-END.
Skips newlines so the tinted background does not bleed one column past
the right border on body lines (the trailing newline character would
otherwise pick up `:background')."
  (when-let* ((body-face (alist-get type
                                    +markdown-gfm-callout-type-body-face-alist
                                    nil nil #'string=)))
    (save-excursion
      (goto-char block-beg)
      (forward-line 1)
      (while (< (point) block-end)
        (let ((lbeg (line-beginning-position))
              (lend (min block-end (line-end-position))))
          (when (< lbeg lend)
            (font-lock-prepend-text-property lbeg lend 'face body-face)))
        (forward-line 1)))))

(defun +markdown-gfm-callout--extend-region ()
  "Extend `font-lock-beg' / `font-lock-end' to cover any callout block.
If the region overlaps a `> [!TYPE]' marker line or any of its
`>'-prefixed continuation lines, widen so the whole block is refontified
together.  Without this, typing on a new continuation line refontifies
only that line, missing the multi-line matcher's anchor."
  (let ((changed nil))
    (save-excursion
      (goto-char font-lock-beg)
      (forward-line 0)
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (looking-at-p (rx bol ">"))))
        (forward-line -1))
      (when (looking-at-p +markdown-gfm-callout--marker-re)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (let ((block-end (+markdown-gfm-callout--block-end
                          (line-end-position))))
          (when (> block-end font-lock-end)
            (setq font-lock-end block-end
                  changed t)))))
    changed))

;;;###autoload
(defun +markdown-fontify-gfm-callouts ()
  "Add font-lock keywords for GFM callout syntax."
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'+markdown-gfm-callout--extend-region nil t)
  (font-lock-add-keywords
   nil
   `((+markdown-gfm-callout--matcher
      (2 (alist-get (match-string-no-properties 1)
                    +markdown-gfm-callout-type-face-alist
                    nil nil #'string=)
         prepend)
      (2 '+markdown-gfm-callout-header-face prepend)
      (0 (progn (+markdown-gfm-callout--paint-body
                 (match-string-no-properties 1)
                 (match-beginning 0)
                 (match-end 0))
                nil)))
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-start" (1+ space) "-->")
      0 '+markdown-prettier-ignore-comment-face prepend)
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->")
      0 '+markdown-prettier-ignore-comment-face prepend)
     (,(rx (group "$" (or (seq "{" (1+ (any alnum "_")) "}")
                          (seq (any upper "_") (1+ (any upper digit "_"))))))
      1 'font-lock-constant-face prepend))))



;;; Workarounds

(defvar +markdown--lang-mode-cache (make-hash-table :test 'equal)
  "Memo for `markdown-get-lang-mode': maps LANG string → mode symbol.
Markdown calls the lookup once per native-fontified code block per
redisplay; on a buffer with many code blocks this dominates CPU.")

(defun +markdown--memoise-lang-mode (orig lang)
  "Around-advice on `markdown-get-lang-mode' caching results by LANG.
ORIG is the underlying function.  Treat the absence of an entry, not a
nil value, as the miss signal so blocks tagged with an unknown LANG
also short-circuit on subsequent calls."
  (let ((hit (gethash lang +markdown--lang-mode-cache 'miss)))
    (if (eq hit 'miss)
        (let ((mode (funcall orig lang)))
          (puthash lang mode +markdown--lang-mode-cache)
          mode)
      hit)))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-get-lang-mode :around #'+markdown--memoise-lang-mode))

(defun +markdown--clamp-extend-region (result)
  "Filter-return advice for `markdown-syntax-propertize-extend-region'.
Clamp the returned (NEW-START . NEW-END) cons to the buffer's accessible
portion.  During an undo, jit-lock's after-change handler invokes the
extender for each undone hunk; markdown's heuristic occasionally returns
NEW-END > `point-max' (the buffer is transiently shorter mid-undo),
which `syntax-propertize' rejects with \"Cannot syntax-propertize ...
because of narrowing!\".  Clamping keeps the extender honest without
changing semantics on a fully-restored buffer."
  (when result
    (cons (max (point-min) (car result))
          (min (point-max) (cdr result)))))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-syntax-propertize-extend-region
              :filter-return #'+markdown--clamp-extend-region))

;;; lib.el ends here
