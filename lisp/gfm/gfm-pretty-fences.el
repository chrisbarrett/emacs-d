;;; gfm-pretty-fences.el --- Visual treatment for fenced code blocks -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator that adorns GFM fenced code blocks, leading YAML frontmatter,
;; and 4-space / tab-indented code blocks with a curved border, language
;; icons, and reveal-on-cursor for fence markers.
;;
;;   ┌────────
;;   │ echo hello
;;   │ echo world
;;   └────────
;;
;; Per-window display overlays (Path C — anchor / display split) keep
;; width-independent props on shared anchor overlays and width-dependent
;; rendering on per-window display overlays, so a buffer split across
;; two windows of different widths renders at each window's own width.
;; Discovery is memoised per `buffer-chars-modified-tick'; rebuilds are
;; scoped to the dirty region when possible and prioritised
;; visible-window-first when widths change.  The engine owns the
;; scheduler, scoped-rebuild routing, and generic rebuild stats; this
;; file contributes block discovery, structural-line ranges (fence
;; markers + YAML helmet markers), an edit-adjacency predicate (blank
;; line adjacent to an indent block), per-block apply functions,
;; language-icon resolution, and the YAML body fontification helper.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)
(require 'nerd-icons nil t)

(defgroup gfm-pretty-fences nil
  "Visual treatment for GFM fenced code blocks."
  :group 'markdown-faces)

(defconst gfm-pretty-fences--open-re
  (rx bol (* blank)
      (group "```" (* "`"))
      (* blank)
      (? "{") (* blank)
      (? (group (+ (any alnum ?_ ?-))))
      (* nonl) eol)
  "Regexp for an opening fence. Group 1: backticks. Group 2: language.")

(defconst gfm-pretty-fences--close-re
  (rx bol (* blank) (group "```" (* "`")) (* blank) eol)
  "Regexp for a closing fence. Group 1: backticks.")

(defconst gfm-pretty-fences--yaml-marker-re
  (rx bol "---" (* blank) eol)
  "Line consisting of `---' (with optional trailing whitespace).")

(defconst gfm-pretty-fences--border-face 'gfm-pretty-border-face
  "Face for the box border around pre blocks.")

;;; Language resolution

(defun gfm-pretty-fences--lang-mode (lang)
  "Best-guess major mode symbol for LANG.
Consults `markdown-code-lang-modes' case-insensitively without the
`fboundp' filter `markdown-get-lang-mode' applies, so an icon can still
be picked even when the corresponding mode is not installed."
  (or (alist-get lang markdown-code-lang-modes nil nil
                 (lambda (a b) (string-equal (downcase a) (downcase b))))
      (let ((ts (intern (concat lang "-ts-mode"))))
        (and (fboundp ts) ts))
      (intern (concat lang "-mode"))))

(defun gfm-pretty-fences--icon-for-lang (lang)
  "Return the nerd-icons icon string for LANG, or nil."
  (when (and lang (fboundp 'nerd-icons-icon-for-mode))
    (let ((icon (ignore-errors
                  (nerd-icons-icon-for-mode (gfm-pretty-fences--lang-mode lang)))))
      (and (stringp icon) icon))))

(defconst gfm-pretty-fences--lhs-margin-langs
  '("diff" "patch")
  "Languages whose first body column is a meaningful indicator.
For these the body content starts in what would otherwise be the box's
left padding column — the `+'/`-'/` ' marks in a diff form a visual
margin between the left border and the annotated source.")

(defun gfm-pretty-fences--lang-has-lhs-margin-p (lang)
  "Non-nil if LANG renders an indicator column that doubles as the LHS margin.
See `gfm-pretty-fences--lhs-margin-langs'."
  (and lang (member (downcase lang) gfm-pretty-fences--lhs-margin-langs)))

;;; Block discovery — fenced

(defun gfm-pretty-fences--find-blocks ()
  "Scan the buffer for fenced code blocks (uncached).
Each entry is (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END LANG LANG-BEG LANG-END).

Widens for the duration of its body so the cache key
\(`buffer-chars-modified-tick') is a pure function of buffer contents
regardless of any current narrowing.  See fix-gfm-narrowing-safety."
  (let (blocks)
   (save-restriction
    (widen)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward gfm-pretty-fences--open-re nil t)
          (let* ((open-beg (match-beginning 1))
                 (open-end (match-end 1))
                 (lang (match-string-no-properties 2))
                 (lang-beg (match-beginning 2))
                 (lang-end (match-end 2))
                 (backticks (match-string-no-properties 1)))
            (forward-line 1)
            (let (close-beg close-end)
              (while (and (not (eobp)) (not close-beg))
                (cond
                 ((and (looking-at gfm-pretty-fences--close-re)
                       (>= (length (match-string 1)) (length backticks)))
                  (setq close-beg (match-beginning 1)
                        close-end (match-end 1)))
                 (t (forward-line 1))))
              (when close-beg
                (push (list open-beg open-end close-beg close-end
                            lang lang-beg lang-end)
                      blocks)
                (goto-char (line-end-position)))))))))
    (nreverse blocks)))

;;; Block discovery — YAML helmet

(defun gfm-pretty-fences--find-yaml-helmet ()
  "Scan the buffer for a leading YAML helmet (uncached).
Returns (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END) or nil.
Widens for the duration of its body so the cache is narrowing-independent."
  (save-restriction
    (widen)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (when (looking-at gfm-pretty-fences--yaml-marker-re)
          (let ((open-beg (match-beginning 0))
                (open-end (match-end 0)))
            (forward-line 1)
            (let (close-beg close-end)
              (while (and (not (eobp)) (not close-beg))
                (cond
                 ((looking-at gfm-pretty-fences--yaml-marker-re)
                  (setq close-beg (match-beginning 0)
                        close-end (match-end 0)))
                 (t (forward-line 1))))
              (when close-beg
                (list open-beg open-end close-beg close-end)))))))))

;;; Block discovery — indent

(defun gfm-pretty-fences--line-indent ()
  "Return code-block indent width on the current line, or nil.
4 spaces or 1 tab counts as a markdown indented code block opener."
  (cond ((looking-at "    ") 4)
        ((looking-at "\t") 1)))

(defun gfm-pretty-fences--blank-line-p ()
  "Non-nil if the current line is blank."
  (looking-at-p "[[:blank:]]*$"))

(defun gfm-pretty-fences--find-indent-blocks (excluded-ranges)
  "Return indented code blocks not overlapping EXCLUDED-RANGES (uncached).
Each block is (BLOCK-BEG BLOCK-END INDENT-WIDTH).
Widens for the duration of its body so the cache is narrowing-independent."
  (let (blocks
        (prev-blank t))
   (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((lbeg (line-beginning-position))
               (blank (gfm-pretty-fences--blank-line-p))
               (excluded (gfm-pretty--in-ranges-p lbeg excluded-ranges))
               (indent (and (not blank) (not excluded)
                            (gfm-pretty-fences--line-indent))))
          (cond
           ((and indent prev-blank)
            (let ((block-beg lbeg)
                  (block-end (line-end-position))
                  (block-indent indent)
                  (continue t))
              (forward-line 1)
              (while (and continue (not (eobp)))
                (let ((ll-beg (line-beginning-position)))
                  (cond
                   ((or (gfm-pretty--in-ranges-p ll-beg excluded-ranges)
                        (gfm-pretty-fences--blank-line-p)
                        (not (gfm-pretty-fences--line-indent)))
                    (setq continue nil))
                   (t (setq block-end (line-end-position))
                      (forward-line 1)))))
              (push (list block-beg block-end block-indent) blocks)
              (setq prev-blank nil)))
           (t
            (setq prev-blank blank)
            (forward-line 1)))))))
    (nreverse blocks)))

;;; Overlay registry

(defconst gfm-pretty-fences--registry
  (gfm-pretty--registry-for 'fences 'gfm-pretty-fences)
  "Shared overlay-registry context for fences.")

(defsubst gfm-pretty-fences--register (ov)
  "Tag OV as a fence overlay and remember it for bulk cleanup."
  (gfm-pretty--register gfm-pretty-fences--registry ov))

(defsubst gfm-pretty-fences--make-anchor (beg end &rest props)
  "Make an anchor overlay over [BEG, END] with PROPS."
  (apply #'gfm-pretty--make-anchor
         gfm-pretty-fences--registry beg end props))

(defsubst gfm-pretty-fences--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS."
  (apply #'gfm-pretty--make-display
         gfm-pretty-fences--registry beg end window props))

;;; Bordered rendering — fenced + YAML (shared)

(defun gfm-pretty-fences--face-extend-bg (face)
  "Return FACE's `:background' when FACE also specifies `:extend t', else nil.
FACE is a face symbol, an attribute plist, or a list thereof; for a
list the first element specifying an `:extend t' background wins."
  (cond
   ((null face) nil)
   ((symbolp face)
    (and (facep face)
         (eq (face-attribute face :extend nil t) t)
         (let ((bg (face-attribute face :background nil t)))
           (and (stringp bg) bg))))
   ((and (consp face) (keywordp (car face)))
    (and (eq (plist-get face :extend) t)
         (let ((bg (plist-get face :background)))
           (and (stringp bg) bg))))
   ((consp face)
    (cl-some #'gfm-pretty-fences--face-extend-bg face))
   (t nil)))

(defun gfm-pretty-fences--line-extend-bg (lbeg lend)
  "Return the `:extend t' background colour on the line [LBEG, LEND), or nil.
Scans `face' text properties across the line for the first face
specifying both `:background' and `:extend t' — the background that
native fontification (e.g. `diff-added' in a fenced `diff' block)
would otherwise leak past the right border, and that should instead
fill the box interior up to the border."
  (let ((pos lbeg)
        (result nil))
    (while (and (null result) (< pos lend))
      (setq result (gfm-pretty-fences--face-extend-bg
                    (get-text-property pos 'face)))
      (setq pos (next-single-property-change pos 'face nil lend)))
    result))

(defun gfm-pretty-fences--apply-bordered-anchors
    (_open-line-end _close-line-beg _face)
  "No-op: fenced/yaml body decoration lives entirely on per-window
display overlays.

Past-EOL `:extend t' leaks (a `diff-added' / `diff-removed' face
copied by native fontification onto a ` ```diff ` body line, or an
overlay face like `hl-line' / `region') are suppressed by the per-line
right-edge after-string itself — see
`gfm-pretty--right-after' — not by an anchor here.

Two zero-width overlays at the same buffer position render in an
order Emacs does not guarantee: an anchor carrying just the
`│ ' before-string and a sibling display overlay carrying the
right-edge `│' after-string can render after-string-first on empty
body lines, painting the left border at the right position.
Collapsing both strings onto a single overlay sidesteps the
ambiguity at the cost of duplicating the `│ ' string per window —
acceptable, mirrors gfm-pretty-tables row decoration."
  nil)

(defun gfm-pretty-fences--apply-bordered-display
    (window open-line-beg open-line-end close-line-beg close-line-end face label
            &optional lhs-margin)
  "Build per-WINDOW display overlays for a fenced/yaml block.
Borders sized to WINDOW's width; FACE colours; LABEL right-aligned in
the top border (icon string for fenced, `meta' for YAML, or nil).

When LHS-MARGIN is non-nil the block's body owns its first column as
a meaningful indicator (e.g. `+'/`-'/` ' in a ` ```diff ` block).  In
that mode the left decoration shrinks to a single `│' (no left
padding column), the body content starts at col 1, and the bg-fill's
left-side mask is suppressed so the indicator keeps its own bg."
  (let* ((body-beg (save-excursion
                     (goto-char open-line-end) (forward-line 1) (point)))
         (body-end (max body-beg (1- close-line-beg)))
         (max-content (gfm-pretty--max-line-width body-beg body-end))
         (text-width (gfm-pretty--available-width window))
         ;; Left decoration: 2 cols (`│ ') normally, 1 col (`│') when
         ;; LHS-MARGIN.  Right decoration is always 2 cols (sep + `│').
         (left-deco-w (if lhs-margin 1 2))
         (total-deco-w (+ left-deco-w 2))
         (box-width (min text-width (max 80 (+ max-content total-deco-w))))
         (content-budget (- box-width total-deco-w))
         (open-buf-width (- open-line-end open-line-beg))
         (close-buf-width (- close-line-end close-line-beg))
         (top-split (gfm-pretty-time-phase 'fences 'compose-borders
                      (gfm-pretty--top-strings box-width face
                                                      open-buf-width label)))
         (bot-split (gfm-pretty-time-phase 'fences 'compose-borders
                      (gfm-pretty--bottom-strings box-width face
                                                         close-buf-width))))
    ;; Top — leading on the marker line, trailing after.
    (let* ((top-display-masked (car top-split))
           (top-display-bare
            (gfm-pretty--str-with-region-bg top-display-masked))
           (selected (gfm-pretty--range-selected-p
                      open-line-beg open-line-end)))
      (gfm-pretty-fences--make-display
       open-line-beg open-line-end window
       'gfm-pretty-fences-kind 'top-leading
       'gfm-pretty-fences-revealable t
       'evaporate t
       'gfm-pretty-display-masked top-display-masked
       'gfm-pretty-display-bare top-display-bare
       'display (if selected top-display-bare top-display-masked)))
    (let* ((top-after-masked (cdr top-split))
           (top-after-bare
            (concat (gfm-pretty--str-with-region-bg top-after-masked)
                    (gfm-pretty--region-tail)))
           (selected (gfm-pretty--range-selected-p
                      open-line-beg open-line-end)))
      (gfm-pretty-fences--make-display
       open-line-end open-line-end window
       'gfm-pretty-fences-kind 'top-trailing
       'gfm-pretty-after-masked top-after-masked
       'gfm-pretty-after-bare top-after-bare
       'after-string (if selected top-after-bare top-after-masked)))
    ;; Body lines — single per-window overlay carrying before/wrap/after.
    ;; Iterate via explicit text-position math, not `forward-line': inside
    ;; this overlay-creation loop, `forward-line' interacts with our
    ;; cursor-intangible / display props and can stall mid-block,
    ;; spinning on the same line forever (bisect 2026-05-08).
    (let* ((lhs-masked (propertize (if lhs-margin "│" "│ ") 'face
                                   (gfm-pretty--normalised-border-face face)))
           (lhs-bare (gfm-pretty--str-with-region-bg lhs-masked))
           (wrap (gfm-pretty--wrap-prefix
                  face (and lhs-margin "↪")))
           (p body-beg))
      (while (< p close-line-beg)
        (let* ((lbeg p)
               (lend (save-excursion (goto-char p) (line-end-position)))
               (line-bg (gfm-pretty-fences--line-extend-bg lbeg lend))
               (after-masked (gfm-pretty-time-phase 'fences 'compose-overflow
                               (if (> (- lend lbeg) content-budget)
                                   (gfm-pretty--right-after-overflow
                                    face (buffer-substring-no-properties lbeg lend)
                                    window nil line-bg)
                                 (gfm-pretty--right-after
                                  box-width face line-bg))))
               (after-bare (gfm-pretty--str-with-region-bg after-masked))
               (selected (gfm-pretty--range-selected-p lbeg lend)))
          (gfm-pretty-fences--make-display
           lbeg lend window
           'gfm-pretty-fences-kind 'body
           'wrap-prefix wrap
           'gfm-pretty-before-masked lhs-masked
           'gfm-pretty-before-bare lhs-bare
           'before-string (if selected lhs-bare lhs-masked)
           'gfm-pretty-after-masked after-masked
           'gfm-pretty-after-bare after-bare
           'after-string (if selected after-bare after-masked))
          ;; When the line carries an `:extend t' background, inset
          ;; the band on the left too by masking the first body
          ;; char's text-prop background with the system bg.
          ;; `:background "unspecified-bg"' is the literal Emacs
          ;; marker that paints with the frame's background, even
          ;; when the underlying text-prop face specifies a colour.
          ;; Foreground (e.g. `diff-indicator-added' on a `+') leaks
          ;; through from below since we set only `:background'.
          ;;
          ;; Skip the mask when the block has an LHS margin: the
          ;; first body col IS the indicator (`+'/`-'), and its bg
          ;; is part of the annotation band.
          (when (and line-bg (not lhs-margin) (< lbeg lend))
            (let ((selected (gfm-pretty--range-selected-p lbeg lend)))
              (gfm-pretty-fences--make-display
               lbeg (1+ lbeg) window
               'gfm-pretty-fences-kind 'body-bg-inset
               'gfm-pretty-face-masked '(:background "unspecified-bg")
               'gfm-pretty-face-bare 'region
               'face (if selected 'region '(:background "unspecified-bg")))))
          (setq p (min close-line-beg (1+ lend))))))
    ;; Bottom — leading on the marker line, trailing after.
    (let* ((bot-display-masked (car bot-split))
           (bot-display-bare
            (gfm-pretty--str-with-region-bg bot-display-masked))
           (selected (gfm-pretty--range-selected-p
                      close-line-beg close-line-end)))
      (gfm-pretty-fences--make-display
       close-line-beg close-line-end window
       'gfm-pretty-fences-kind 'bottom-leading
       'gfm-pretty-fences-revealable t
       'evaporate t
       'gfm-pretty-display-masked bot-display-masked
       'gfm-pretty-display-bare bot-display-bare
       'display (if selected bot-display-bare bot-display-masked)))
    (let* ((bot-after-masked (cdr bot-split))
           (bot-after-bare
            (concat (gfm-pretty--str-with-region-bg bot-after-masked)
                    (gfm-pretty--region-tail)))
           (selected (gfm-pretty--range-selected-p
                      close-line-end close-line-end)))
      (gfm-pretty-fences--make-display
       close-line-end close-line-end window
       'gfm-pretty-fences-kind 'bottom-trailing
       'gfm-pretty-after-masked bot-after-masked
       'gfm-pretty-after-bare bot-after-bare
       'after-string (if selected bot-after-bare bot-after-masked)))))

;;; Indent block rendering

(defun gfm-pretty-fences--apply-indent-anchors (beg end indent-width face)
  "Build width-independent anchors for an indent block at [BEG, END]."
  (let* ((lhs-masked (propertize "│ " 'face
                                 (gfm-pretty--normalised-border-face face)))
         (lhs-bare (gfm-pretty--str-with-region-bg lhs-masked))
         (first t)
         (p beg))
    (while (<= p end)
      (let* ((lbeg p)
             (lend (save-excursion (goto-char p) (line-end-position)))
             (cover-end (min (+ lbeg indent-width) lend))
             (selected (gfm-pretty--range-selected-p lbeg lend)))
        ;; Cover indent chars with `│ ' display; carry cursor-intangible.
        (gfm-pretty-fences--make-anchor
         lbeg cover-end
         'gfm-pretty-fences-kind 'indent-body
         'gfm-pretty-fences-indent-first first
         'cursor-intangible t
         'gfm-pretty-display-masked lhs-masked
         'gfm-pretty-display-bare lhs-bare
         'display (if selected lhs-bare lhs-masked))
        ;; Wrap-prefix on the whole line (continuation lines).
        (gfm-pretty-fences--make-anchor
         lbeg lend
         'gfm-pretty-fences-kind 'indent-wrap
         'wrap-prefix (gfm-pretty--wrap-prefix face))
        (setq first nil)
        (setq p (1+ lend))))))

(defun gfm-pretty-fences--apply-indent-display (window beg end indent-width face)
  "Build per-WINDOW display overlays for an indent block at [BEG, END].
INDENT-WIDTH is the buffer indent width; FACE colours the borders."
  (let* ((max-content (gfm-pretty--max-line-width beg end indent-width))
         (text-width (gfm-pretty--available-width window))
         (box-width (min text-width (max 80 (+ max-content 4))))
         (content-budget (- box-width 4))
         (top-split (gfm-pretty-time-phase 'fences 'compose-borders
                      (gfm-pretty--top-strings box-width face 0 nil)))
         (bot-split (gfm-pretty-time-phase 'fences 'compose-borders
                      (gfm-pretty--bottom-strings box-width face 0)))
         (top-str (concat (car top-split) (cdr top-split)))
         (bot-str (concat (car bot-split) (cdr bot-split))))
    (let ((first t)
          (p beg))
      (while (<= p end)
        (let* ((lbeg p)
               (lend (save-excursion (goto-char p) (line-end-position)))
               (last-line (>= lend end))
               (line-content-w (max 0 (- (- lend lbeg) indent-width)))
               (line-text (buffer-substring-no-properties
                           (min (+ lbeg indent-width) lend) lend))
               (line-bg (gfm-pretty-fences--line-extend-bg lbeg lend))
               (overflow-p (> line-content-w content-budget))
               (after-masked-base
                (gfm-pretty-time-phase 'fences 'compose-overflow
                  (if overflow-p
                      (gfm-pretty--right-after-overflow
                       face line-text window nil line-bg)
                    (gfm-pretty--right-after
                     box-width face line-bg))))
               (after-masked (if last-line
                                 (concat after-masked-base "\n" bot-str)
                               after-masked-base))
               ;; Last-line bare keeps the bot-str opaque (masked form)
               ;; so the box's bottom edge doesn't bleed region bg past
               ;; the border when the last body line is selected.
               (after-bare (if last-line
                               (concat
                                (gfm-pretty--str-with-region-bg
                                 after-masked-base)
                                "\n"
                                bot-str)
                             (gfm-pretty--str-with-region-bg
                              after-masked-base)))
               (selected (gfm-pretty--range-selected-p lbeg lend)))
          (when first
            (let* ((top-masked (concat top-str "\n"))
                   (top-bare (concat (gfm-pretty--str-with-region-bg top-str)
                                     "\n")))
              (gfm-pretty-fences--make-display
               lbeg lbeg window
               'gfm-pretty-fences-kind 'indent-top
               'gfm-pretty-before-masked top-masked
               'gfm-pretty-before-bare top-bare
               'before-string (if selected top-bare top-masked))))
          (gfm-pretty-fences--make-display
           lend lend window
           'gfm-pretty-fences-kind 'indent-rhs
           'gfm-pretty-after-masked after-masked
           'gfm-pretty-after-bare after-bare
           'after-string (if selected after-bare after-masked))
          ;; Inset the bg band on the left by masking the first body
          ;; char (after the indent) when the line carries an
          ;; `:extend t' background.  See the fenced display path
          ;; for the `"unspecified-bg"' rationale.
          (let ((body-start (+ lbeg indent-width)))
            (when (and line-bg (< body-start lend))
              (gfm-pretty-fences--make-display
               body-start (1+ body-start) window
               'gfm-pretty-fences-kind 'body-bg-inset
               'face '(:background "unspecified-bg"))))
          (setq first nil)
          (setq p (1+ lend)))))))

;;; Block adapters

(defun gfm-pretty-fences--fenced-line-positions (block)
  "Return (OPEN-LINE-BEG OPEN-LINE-END CLOSE-LINE-BEG CLOSE-LINE-END) for BLOCK."
  (let ((open-beg (nth 0 block))
        (close-beg (nth 2 block))
        (close-end (nth 3 block)))
    (list (save-excursion (goto-char open-beg) (line-beginning-position))
          (save-excursion (goto-char open-beg) (line-end-position))
          (save-excursion (goto-char close-beg) (line-beginning-position))
          close-end)))

(defun gfm-pretty-fences--apply-fenced-block-anchors (block)
  "Apply width-independent anchors for fenced BLOCK."
  (cl-destructuring-bind (_olb ole _clb _cle)
      (gfm-pretty-fences--fenced-line-positions block)
    (gfm-pretty-fences--apply-bordered-anchors
     ole (nth 2 (gfm-pretty-fences--fenced-line-positions block))
     gfm-pretty-fences--border-face)))

(defun gfm-pretty-fences--apply-fenced-block-display (block window)
  "Apply per-WINDOW display overlays for fenced BLOCK."
  (let* ((face gfm-pretty-fences--border-face)
         (lang (nth 4 block))
         (icon (and lang (gfm-pretty-fences--icon-for-lang lang)))
         (lhs-margin (gfm-pretty-fences--lang-has-lhs-margin-p lang))
         (positions (gfm-pretty-fences--fenced-line-positions block)))
    (cl-destructuring-bind (olb ole clb cle) positions
      (gfm-pretty-fences--apply-bordered-display
       window olb ole clb cle face icon lhs-margin))))

(defun gfm-pretty-fences--yaml-line-positions (helmet)
  "Return (OLB OLE CLB CLE) line positions for HELMET."
  (cl-destructuring-bind (open-beg _open-end close-beg close-end) helmet
    (list (save-excursion (goto-char open-beg) (line-beginning-position))
          (save-excursion (goto-char open-beg) (line-end-position))
          (save-excursion (goto-char close-beg) (line-beginning-position))
          close-end)))

(defun gfm-pretty-fences--apply-yaml-block-anchors (helmet)
  "Apply width-independent anchors for YAML HELMET, including body fontification."
  (cl-destructuring-bind (_olb ole clb _cle)
      (gfm-pretty-fences--yaml-line-positions helmet)
    (gfm-pretty-fences--apply-bordered-anchors ole clb 'font-lock-constant-face))
  ;; Fontify body (face overlays — rebuilt per anchor pass; not per window).
  (cl-destructuring-bind (open-beg open-end close-beg _close-end) helmet
    (let* ((body-beg (min close-beg
                          (save-excursion
                            (goto-char open-end) (forward-line 1) (point))))
           (body-end (max body-beg (1- close-beg))))
      (ignore open-beg)
      (gfm-pretty-fences--fontify-yaml-body body-beg body-end))))

(defun gfm-pretty-fences--apply-yaml-block-display (helmet window)
  "Apply per-WINDOW display overlays for YAML HELMET."
  (let* ((face 'font-lock-constant-face)
         (label (propertize "meta" 'face `(:inherit (bold ,face))))
         (positions (gfm-pretty-fences--yaml-line-positions helmet)))
    (cl-destructuring-bind (olb ole clb cle) positions
      (gfm-pretty-fences--apply-bordered-display
       window olb ole clb cle face label))))

(defun gfm-pretty-fences--apply-indent-block-anchors (block)
  "Apply width-independent anchors for indent BLOCK."
  (cl-destructuring-bind (beg end indent-width) block
    (gfm-pretty-fences--apply-indent-anchors
     beg end indent-width gfm-pretty-fences--border-face)))

(defun gfm-pretty-fences--apply-indent-block-display (block window)
  "Apply per-WINDOW display overlays for indent BLOCK."
  (cl-destructuring-bind (beg end indent-width) block
    (gfm-pretty-fences--apply-indent-display
     window beg end indent-width gfm-pretty-fences--border-face)))

;;; YAML body fontification

(defun gfm-pretty-fences--yaml-mode ()
  "Return a yaml major-mode symbol that's currently fboundp, or nil."
  (cond ((and (fboundp 'yaml-ts-mode)
              (fboundp 'treesit-language-available-p)
              (treesit-language-available-p 'yaml))
         'yaml-ts-mode)
        ((fboundp 'yaml-mode) 'yaml-mode)))

(defun gfm-pretty-fences--fontify-yaml-body (start end)
  "Apply YAML font-lock face overlays to buffer region [START, END]."
  (when-let* ((lang-mode (gfm-pretty-fences--yaml-mode))
              ((> end start)))
    (let ((string (buffer-substring-no-properties start end))
          (target-buffer (current-buffer))
          pos next)
      (with-current-buffer
          (get-buffer-create
           (format " *gfm-pretty-fences-yaml-fontification:%s*"
                   (symbol-name lang-mode)))
        (let ((inhibit-modification-hooks nil))
          (delete-region (point-min) (point-max))
          (insert string " "))
        (unless (eq major-mode lang-mode)
          (funcall lang-mode))
        (font-lock-ensure)
        (setq pos (point-min))
        (while (setq next (next-single-property-change pos 'face))
          (when-let* ((val (get-text-property pos 'face)))
            (let ((ov (make-overlay (+ start (1- pos))
                                    (1- (+ start next))
                                    target-buffer)))
              (overlay-put ov 'face val)
              (gfm-pretty-fences--register ov)))
          (setq pos next))))))

;;; Block enumeration & ranges

(cl-defstruct (gfm-pretty-fences--block
               (:constructor gfm-pretty-fences--make-block)
               (:copier nil))
  "Tagged source block for unified rebuild dispatch.
KIND is `fenced', `yaml', or `indent'.  RANGE is (LINE-BEG . LINE-END+1)
covering the block's full source range, used for visibility checks and
scoped-rebuild containment.  PAYLOAD is the kind-specific data tuple."
  kind range payload)

(defun gfm-pretty-fences--collect-blocks ()
  "Return tagged blocks discovered in the current buffer.
Order is: yaml (if any), then fenced in source order, then indent in
source order.  Indent discovery excludes fenced + yaml ranges."
  (let* ((helmet (gfm-pretty-time-phase 'fences 'find-yaml
                   (gfm-pretty-fences--find-yaml-helmet)))
         (fenced (gfm-pretty-time-phase 'fences 'find-fenced
                   (gfm-pretty-fences--find-blocks)))
         (excluded
          (append
           (when helmet
             (cl-destructuring-bind (olb _ole _clb cle)
                 (gfm-pretty-fences--yaml-line-positions helmet)
               (list (cons olb cle))))
           (mapcar (lambda (b)
                     (cl-destructuring-bind (olb _ole _clb cle)
                         (gfm-pretty-fences--fenced-line-positions b)
                       (cons olb cle)))
                   fenced)))
         (indent (gfm-pretty-time-phase 'fences 'find-indent
                   (gfm-pretty-fences--find-indent-blocks excluded)))
         result)
    (when helmet
      (cl-destructuring-bind (olb _ole _clb cle)
          (gfm-pretty-fences--yaml-line-positions helmet)
        (push (gfm-pretty-fences--make-block
               :kind 'yaml :range (cons olb (1+ cle)) :payload helmet)
              result)))
    (dolist (b fenced)
      (cl-destructuring-bind (olb _ole _clb cle)
          (gfm-pretty-fences--fenced-line-positions b)
        (push (gfm-pretty-fences--make-block
               :kind 'fenced :range (cons olb (1+ cle)) :payload b)
              result)))
    (dolist (b indent)
      (cl-destructuring-bind (beg end _iw) b
        (push (gfm-pretty-fences--make-block
               :kind 'indent :range (cons beg (1+ end)) :payload b)
              result)))
    (nreverse result)))

(defun gfm-pretty-fences--apply-block-anchors (block)
  "Apply anchor overlays for tagged BLOCK.
Widens for the duration of the apply so a BLOCK whose source range lies
outside the current restriction (e.g. another slide under
`gfm-present-mode') can still be parsed and decorated.  Display under
narrowing is naturally clipped by Emacs' overlay engine."
  (save-restriction
    (widen)
    (gfm-pretty-time-phase 'fences 'apply
      (cl-case (gfm-pretty-fences--block-kind block)
        (fenced (gfm-pretty-fences--apply-fenced-block-anchors
                 (gfm-pretty-fences--block-payload block)))
        (yaml (gfm-pretty-fences--apply-yaml-block-anchors
               (gfm-pretty-fences--block-payload block)))
        (indent (gfm-pretty-fences--apply-indent-block-anchors
                 (gfm-pretty-fences--block-payload block)))))))

(defun gfm-pretty-fences--apply-block-display (block window)
  "Apply per-WINDOW display overlays for tagged BLOCK.
See `gfm-pretty-fences--apply-block-anchors' for the widening rationale."
  (save-restriction
    (widen)
    (gfm-pretty-time-phase 'fences 'apply
      (cl-case (gfm-pretty-fences--block-kind block)
        (fenced (gfm-pretty-fences--apply-fenced-block-display
                 (gfm-pretty-fences--block-payload block) window))
        (yaml (gfm-pretty-fences--apply-yaml-block-display
               (gfm-pretty-fences--block-payload block) window))
        (indent (gfm-pretty-fences--apply-indent-block-display
                 (gfm-pretty-fences--block-payload block) window))))))

(defun gfm-pretty-fences--apply-block (block window)
  "Engine `:apply-block-fn' — apply WINDOW's overlays for BLOCK.
Routes through `gfm-pretty-borders--apply-with-anchors' so width-
independent anchors are laid at most once per (block, rebuild pass)."
  (gfm-pretty-borders--apply-with-anchors
   block window
   :registry gfm-pretty-fences--registry
   :range (gfm-pretty-fences--block-range block)
   :anchors-fn #'gfm-pretty-fences--apply-block-anchors
   :display-fn #'gfm-pretty-fences--apply-block-display))

(defun gfm-pretty-fences--full-rebuild-required-p (dirty)
  "Engine `:full-rebuild-required-p' — fold structural + adjacency checks.
Non-nil when DIRTY overlaps any fence / YAML marker line (structural)
or a blank line adjacent to an indent block (adjacency)."
  (or (cl-some (lambda (r) (gfm-pretty--region-overlaps-p dirty r))
               (gfm-pretty-fences--fence-line-ranges))
      (gfm-pretty-fences--blank-line-adjacent-to-indent-p dirty)))

;;; Visibility helper

(defun gfm-pretty-fences--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (gfm-pretty--block-visible-p
   block ranges #'gfm-pretty-fences--block-range))

;;; Structural-line + edit-adjacency hooks for engine routing

(defun gfm-pretty-fences--fence-line-ranges ()
  "Return per-line (BEG . END) ranges for every fence opening / closing line.
Includes YAML helmet markers as well so edits to those lines invalidate."
  (let (ranges)
    (dolist (b (gfm-pretty-fences--find-blocks))
      (let ((open-beg (nth 0 b))
            (close-end (nth 3 b)))
        (push (cons (save-excursion
                      (goto-char open-beg) (line-beginning-position))
                    (save-excursion
                      (goto-char open-beg) (line-end-position)))
              ranges)
        (push (cons (save-excursion
                      (goto-char close-end) (line-beginning-position))
                    (save-excursion
                      (goto-char close-end) (line-end-position)))
              ranges)))
    (when-let* ((helmet (gfm-pretty-fences--find-yaml-helmet)))
      (cl-destructuring-bind (open-beg _open-end close-beg close-end) helmet
        (push (cons (save-excursion
                      (goto-char open-beg) (line-beginning-position))
                    (save-excursion
                      (goto-char open-beg) (line-end-position)))
              ranges)
        (push (cons (save-excursion
                      (goto-char close-beg) (line-beginning-position))
                    close-end)
              ranges)))
    ranges))

(defun gfm-pretty-fences--blank-line-adjacent-to-indent-p (region)
  "Non-nil if REGION overlaps a blank line adjacent to an indent block.
Indent-block discovery is gated by blank-line adjacency, so an edit
that synthesises or dissolves an adjacent blank line could create or
destroy a block."
  (let* ((excluded
          (append
           (when-let* ((h (gfm-pretty-fences--find-yaml-helmet)))
             (cl-destructuring-bind (olb _ole _clb cle)
                 (gfm-pretty-fences--yaml-line-positions h)
               (list (cons olb cle))))
           (mapcar (lambda (b)
                     (cl-destructuring-bind (olb _ole _clb cle)
                         (gfm-pretty-fences--fenced-line-positions b)
                       (cons olb cle)))
                   (gfm-pretty-fences--find-blocks))))
         (blocks (gfm-pretty-fences--find-indent-blocks excluded)))
    (cl-some
     (lambda (b)
       (cl-destructuring-bind (beg end _iw) b
         (let* ((before-beg
                 (save-excursion
                   (goto-char beg)
                   (forward-line -1)
                   (line-beginning-position)))
                (before-end
                 (save-excursion
                   (goto-char beg)
                   (forward-line -1)
                   (line-end-position)))
                (after-beg
                 (save-excursion
                   (goto-char end)
                   (forward-line 1)
                   (line-beginning-position)))
                (after-end
                 (save-excursion
                   (goto-char end)
                   (forward-line 1)
                   (line-end-position))))
           (or (and (>= before-end (point-min))
                    (gfm-pretty--region-overlaps-p
                     region (cons before-beg before-end)))
               (and (<= after-beg (point-max))
                    (gfm-pretty--region-overlaps-p
                     region (cons after-beg after-end)))))))
     blocks)))

;;; Lifecycle hooks delegated to engine

(defun gfm-pretty-fences--on-enable ()
  "Per-decorator setup invoked on enable."
  (cursor-intangible-mode 1))

(defun gfm-pretty-fences--on-disable ()
  "Per-decorator teardown invoked on disable."
  (cursor-intangible-mode -1))

;;; Stats command (thin wrapper over engine)

;;;###autoload
(defun gfm-pretty-fences-stats ()
  "Display the fences decorator's rebuild stats and phase totals."
  (interactive)
  (gfm-pretty-stats 'fences))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'fences
    :registry           gfm-pretty-fences--registry
    :collect-fn         #'gfm-pretty-fences--collect-blocks
    :range-fn           #'gfm-pretty-fences--block-range
    :apply-block-fn     #'gfm-pretty-fences--apply-block
    :full-rebuild-required-p #'gfm-pretty-fences--full-rebuild-required-p
    :on-enable-fn       #'gfm-pretty-fences--on-enable
    :on-disable-fn      #'gfm-pretty-fences--on-disable))

(provide 'gfm-pretty-fences)

;;; gfm-pretty-fences.el ends here
