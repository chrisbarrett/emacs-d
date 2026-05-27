;;; gfm-pretty-blockquotes.el --- Left-rail overlay for plain GFM blockquotes -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator that renders plain GitHub Flavored Markdown blockquotes
;; (runs of `^>'-prefixed lines without a `[!TYPE]' marker) as a
;; continuous left rail.  Replaces the leading `> ' with `▌ ' (or `>'
;; with `▌' on bare continuation lines) via a per-window display
;; overlay and overrides `markdown-mode's `wrap-prefix "> "' with an
;; overlay `wrap-prefix "▌ "' so soft-wrapped visual continuation
;; rows continue the rail.
;;
;; Callout-typed blockquotes are owned by `gfm-pretty-callouts'; this
;; decorator filters those out by skipping any `>' run that contains
;; a `> [!TYPE]' marker line.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)
(require 'gfm-pretty-callouts)

(defgroup gfm-pretty-blockquotes nil
  "Left-rail overlays for plain GFM blockquotes."
  :group 'markdown-faces)

(defface gfm-pretty-blockquotes-rail-face
  '((t :inherit font-lock-constant-face))
  "Face for the left rail painted over plain blockquote blocks."
  :group 'gfm-pretty-blockquotes)

(defcustom gfm-pretty-blockquotes-inset-cols 'tab-width
  "Width of the gutter to the left of the blockquote rail.
The symbol `tab-width' (the default) is resolved to the buffer's
`tab-width' at render time; an integer pins the gutter width
independent of `tab-width'."
  :type '(choice integer (const :tag "Buffer's tab-width" tab-width))
  :group 'gfm-pretty-blockquotes)

(defun gfm-pretty-blockquotes--inset-cols ()
  "Resolved inset width: integer literal or current `tab-width'."
  (let ((v gfm-pretty-blockquotes-inset-cols))
    (cond ((integerp v) (max 0 v))
          ((eq v 'tab-width) (max 0 tab-width))
          ((and (symbolp v) (boundp v))
           (let ((sv (symbol-value v)))
             (if (integerp sv) (max 0 sv) tab-width)))
          (t tab-width))))

;;; Overlay registry

(defconst gfm-pretty-blockquotes--registry
  (gfm-pretty--registry-for 'blockquotes 'gfm-pretty-blockquotes)
  "Shared overlay-registry context for blockquotes.")

(defsubst gfm-pretty-blockquotes--make-anchor (beg end &rest props)
  "Make an anchor overlay over [BEG, END] with PROPS."
  (apply #'gfm-pretty--make-anchor
         gfm-pretty-blockquotes--registry beg end props))

(defsubst gfm-pretty-blockquotes--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS."
  (apply #'gfm-pretty--make-display
         gfm-pretty-blockquotes--registry beg end window props))

(defsubst gfm-pretty-blockquotes--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-blockquotes overlays between BEG and END."
  (gfm-pretty--remove-overlays gfm-pretty-blockquotes--registry beg end))

(defsubst gfm-pretty-blockquotes--prune-dead-overlays ()
  "Drop overlays from the registry whose buffer is gone."
  (gfm-pretty--prune-dead-overlays gfm-pretty-blockquotes--registry))

(defsubst gfm-pretty-blockquotes--register (ov)
  "Tag OV as a blockquotes overlay and remember it for bulk cleanup."
  (gfm-pretty--register gfm-pretty-blockquotes--registry ov))

;;; Block discovery

(defconst gfm-pretty-blockquotes--blockquote-line-re
  (rx bol ">")
  "Regexp matching a line whose first character is `>'.")

(defun gfm-pretty-blockquotes--find-blocks ()
  "Scan the buffer for plain blockquote blocks (uncached).
Each entry is (BEG END) where BEG is BOL of the first `>' line and
END is EOL of the last `>' line.  Maximal runs of consecutive
`^>'-prefixed lines are returned; any run containing a `> [!TYPE]'
marker line is dropped (callouts owns those).

Widens for the duration so a narrowed buffer still sees full block
boundaries."
  (let (blocks)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (not (eobp))
            (cond
             ((looking-at-p gfm-pretty-blockquotes--blockquote-line-re)
              (let ((block-beg (line-beginning-position))
                    (block-end (line-end-position))
                    (has-marker
                     (looking-at-p gfm-pretty-callouts--marker-re)))
                (forward-line 1)
                (while (and (not (eobp))
                            (looking-at-p
                             gfm-pretty-blockquotes--blockquote-line-re))
                  (when (looking-at-p gfm-pretty-callouts--marker-re)
                    (setq has-marker t))
                  (setq block-end (line-end-position))
                  (forward-line 1))
                (unless has-marker
                  (push (list block-beg block-end) blocks))))
             (t (forward-line 1)))))))
    (nreverse blocks)))

(cl-defstruct (gfm-pretty-blockquotes--block
               (:constructor gfm-pretty-blockquotes--make-block)
               (:copier nil))
  "Tagged plain-blockquote block for unified rebuild dispatch.
RANGE is (LINE-BEG . LINE-END+1) covering the full source range.
PAYLOAD is the raw (BEG END) tuple."
  range payload)

(defun gfm-pretty-blockquotes--collect-blocks ()
  "Uncached widened scan returning tagged plain-blockquote blocks.
The engine memoises this via `gfm-pretty--collect'."
  (mapcar (lambda (b)
            (gfm-pretty-blockquotes--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-pretty-blockquotes--find-blocks)))

;;; Rendering helpers

(defun gfm-pretty-blockquotes--gutter-string (inset-cols)
  "Build the leading-gutter string used as an anchor before-string."
  (propertize (make-string inset-cols ?\s) 'face 'default))

(defun gfm-pretty-blockquotes--rail-string (&optional one-char-p)
  "Build the rail display string `▌<space>' (`▌' alone when ONE-CHAR-P).
Painted with `gfm-pretty-blockquotes-rail-face'.  No inset — the
inset is emitted separately as the anchor's `before-string' so
reveal exposes the raw `> ' source without losing the gutter."
  (let ((rail (propertize "▌" 'face 'gfm-pretty-blockquotes-rail-face)))
    (if one-char-p
        rail
      (concat rail " "))))

(defun gfm-pretty-blockquotes--wrap-prefix-string (inset-cols)
  "Build the soft-wrap continuation prefix `<inset>▌<space>'.
Continuation visual rows have no buffer chars at column 0 to host a
before-string, so the inset is baked into the wrap-prefix itself."
  (concat (gfm-pretty-blockquotes--gutter-string inset-cols)
          (gfm-pretty-blockquotes--rail-string)))

;;; Rendering

(defun gfm-pretty-blockquotes--apply-block-anchors (block)
  "Apply width-independent anchor overlays for BLOCK.
Per source line: hangs the inset gutter as a `before-string' and
sets `wrap-prefix' to the inset + rail string so soft-wrapped
visual rows continue the rail.  Widens so blocks outside the
current restriction can still be parsed."
  (save-restriction
    (widen)
    (cl-destructuring-bind (beg end)
        (gfm-pretty-blockquotes--block-payload block)
      (let* ((inset-cols (gfm-pretty-blockquotes--inset-cols))
             (gutter (gfm-pretty-blockquotes--gutter-string inset-cols))
             (wrap (gfm-pretty-blockquotes--wrap-prefix-string inset-cols))
             (p beg))
        (while (< p (1+ end))
          (let ((lend (save-excursion
                        (goto-char p) (line-end-position))))
            (gfm-pretty-blockquotes--make-anchor
             p lend
             'before-string gutter
             'wrap-prefix wrap)
            (setq p (min (1+ end) (1+ lend)))))))))

(defun gfm-pretty-blockquotes--apply-block-display (block window)
  "Apply per-WINDOW display overlays for BLOCK.
Per source line: replaces the leading `> ' (2 chars) or bare `>'
\(1 char) with the rail prefix.  Carries paired masked / bare
properties so the engine's reveal walker exposes the raw source
per window on point/selection overlap."
  (save-restriction
    (widen)
    (cl-destructuring-bind (beg end)
        (gfm-pretty-blockquotes--block-payload block)
      (let* ((prefix-two (gfm-pretty-blockquotes--rail-string))
             (prefix-one (gfm-pretty-blockquotes--rail-string t))
             (p beg))
        (while (< p (1+ end))
          (let* ((lbeg p)
                 (lend (save-excursion
                         (goto-char p) (line-end-position)))
                 (line-selected (gfm-pretty--range-selected-p lbeg lend)))
            (cond
             ((and (>= (- lend lbeg) 2)
                   (eq (char-after lbeg) ?>)
                   (eq (char-after (1+ lbeg)) ?\s))
              (let ((bare (gfm-pretty--str-with-region-bg prefix-two)))
                (gfm-pretty-blockquotes--make-display
                 lbeg (+ lbeg 2) window
                 'gfm-pretty-blockquotes-kind 'rail-prefix
                 'gfm-pretty-blockquotes-revealable t
                 'evaporate t
                 'gfm-pretty-display-masked prefix-two
                 'gfm-pretty-display-bare bare
                 'display (if line-selected bare prefix-two))))
             ((and (= (- lend lbeg) 1)
                   (eq (char-after lbeg) ?>))
              (let ((bare (gfm-pretty--str-with-region-bg prefix-one)))
                (gfm-pretty-blockquotes--make-display
                 lbeg (1+ lbeg) window
                 'gfm-pretty-blockquotes-kind 'rail-prefix
                 'gfm-pretty-blockquotes-revealable t
                 'evaporate t
                 'gfm-pretty-display-masked prefix-one
                 'gfm-pretty-display-bare bare
                 'display (if line-selected bare prefix-one)))))
            (setq p (min (1+ end) (1+ lend)))))))))

(defun gfm-pretty-blockquotes--apply-block (block window)
  "Engine `:apply-block-fn' — apply WINDOW's overlays for BLOCK.
Routes through `gfm-pretty-borders--apply-with-anchors' so the
per-line anchor overlays are laid at most once per (block, rebuild
pass) while per-window display overlays apply once per window."
  (gfm-pretty-borders--apply-with-anchors
   block window
   :registry gfm-pretty-blockquotes--registry
   :range (gfm-pretty-blockquotes--block-range block)
   :anchors-fn #'gfm-pretty-blockquotes--apply-block-anchors
   :display-fn #'gfm-pretty-blockquotes--apply-block-display))

;;; Visibility

(defun gfm-pretty-blockquotes--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (gfm-pretty--block-visible-p
   block ranges #'gfm-pretty-blockquotes--block-range))

;;; Edit-adjacency / structural-line helpers

(defun gfm-pretty-blockquotes--blockquote-line-ranges ()
  "Return per-line (BEG . END) ranges for every `^>' line in the buffer."
  (let (ranges)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (not (eobp))
            (when (looking-at-p gfm-pretty-blockquotes--blockquote-line-re)
              (push (cons (line-beginning-position)
                          (line-end-position))
                    ranges))
            (forward-line 1)))))
    (nreverse ranges)))

(defun gfm-pretty-blockquotes--marker-line-ranges ()
  "Return per-line (BEG . END) ranges for every `> [!TYPE]' marker line."
  (let (ranges)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward gfm-pretty-callouts--marker-re nil t)
            (push (cons (line-beginning-position)
                        (line-end-position))
                  ranges)))))
    (nreverse ranges)))

(defun gfm-pretty-blockquotes--region-adjacent-to-block-p (region)
  "Non-nil if REGION overlaps a line directly above or below a blockquote.
Edits there can grow or shrink a block, so the engine must do a full
rebuild rather than scope to a single existing block."
  (cl-some
   (lambda (b)
     (let* ((beg (nth 0 b))
            (end (nth 1 b))
            (before-beg (save-excursion
                          (goto-char beg) (forward-line -1)
                          (line-beginning-position)))
            (before-end (save-excursion
                          (goto-char beg) (forward-line -1)
                          (line-end-position)))
            (after-beg (save-excursion
                         (goto-char end) (forward-line 1)
                         (line-beginning-position)))
            (after-end (save-excursion
                         (goto-char end) (forward-line 1)
                         (line-end-position))))
       (or (and (>= before-end (point-min))
                (gfm-pretty--region-overlaps-p
                 region (cons before-beg before-end)))
           (and (<= after-beg (point-max))
                (gfm-pretty--region-overlaps-p
                 region (cons after-beg after-end))))))
   (gfm-pretty-blockquotes--find-blocks)))

(defun gfm-pretty-blockquotes--full-rebuild-required-p (dirty)
  "Engine `:full-rebuild-required-p' — fold structural + adjacency checks.
Non-nil when DIRTY overlaps the first column of any blockquote line
\(structural-line case — the `>' character is the partition decider),
a line directly above or below an existing blockquote block
\(adjacency case), or a `> [!TYPE]' marker line (covers
callout-to-plain transitions where the partition flips)."
  (or (cl-some (lambda (r) (gfm-pretty--region-overlaps-p dirty r))
               (gfm-pretty-blockquotes--blockquote-line-ranges))
      (gfm-pretty-blockquotes--region-adjacent-to-block-p dirty)
      (cl-some (lambda (r) (gfm-pretty--region-overlaps-p dirty r))
               (gfm-pretty-blockquotes--marker-line-ranges))))

;;; Lifecycle hooks delegated to engine

(defun gfm-pretty-blockquotes--on-enable ()
  "Per-decorator setup invoked on enable.  No-op."
  nil)

(defun gfm-pretty-blockquotes--on-disable ()
  "Per-decorator teardown invoked on disable.  Engine handles overlays."
  nil)

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'blockquotes
    :phase              'overlays
    :registry           gfm-pretty-blockquotes--registry
    :collect-fn         #'gfm-pretty-blockquotes--collect-blocks
    :range-fn           #'gfm-pretty-blockquotes--block-range
    :apply-block-fn     #'gfm-pretty-blockquotes--apply-block
    :full-rebuild-required-p #'gfm-pretty-blockquotes--full-rebuild-required-p
    :on-enable-fn       #'gfm-pretty-blockquotes--on-enable
    :on-disable-fn      #'gfm-pretty-blockquotes--on-disable))

(provide 'gfm-pretty-blockquotes)

;;; gfm-pretty-blockquotes.el ends here
