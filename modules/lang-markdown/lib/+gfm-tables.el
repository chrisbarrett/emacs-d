;;; +gfm-tables.el --- Visual treatment for GFM tables -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that adorns GitHub Flavored Markdown tables with bordered,
;; zebra-striped grids:
;;
;;   ┌─────────────────────┐
;;   │ Header A │ Header B │
;;   ├─────────────────────┤
;;   │ a1       │ b1       │
;;   │ a2       │ b2       │
;;   └─────────────────────┘
;;
;; Source rows stay in place but are display-replaced with a composed
;; per-row string carrying outer `│' borders, padded cells, and a 1-char
;; default-bg gap between cells.  Cursor entry suppresses the row's
;; display so the underlying source can be edited normally; rebuilds
;; are debounced via a 0.2-second idle timer.

;;; Code:

(require 'cl-lib)
(require '+gfm-code-fences nil t)
(require 'markdown-mode nil t)

(defgroup gfm-tables nil
  "Visual treatment for GitHub Flavored Markdown tables."
  :group 'markdown-faces)

(defface gfm-tables-active-cell-face
  '((((background light)) :background "#9ec5ff" :extend t)
    (((background dark))  :background "#2e4a7a" :extend t))
  "Blue background face applied to the table cell at point.
The colour is intentionally muted so theme-set foregrounds (markdown
faces inside the cell) still read clearly."
  :group 'gfm-tables)

(defface gfm-tables-row-alt-face
  '((((background light)) :background "#f6f1e6")
    (((background dark))  :background "#262637"))
  "Stripe colour for alternating GFM table body rows."
  :group 'gfm-tables)

(defface gfm-tables-row-alt-cap-face
  '((((background light)) :foreground "#f6f1e6")
    (((background dark))  :foreground "#262637"))
  "Foreground for alt-row half-block caps.
Mirrors the background of `gfm-tables-row-alt-face' so that
`▐' / `▌' chars at row ends paint a half-cell of alt-bg, extending
the row's background to the header box's vertical edges."
  :group 'gfm-tables)

(defcustom gfm-tables-slow-rebuild-threshold 0.05
  "Threshold in seconds above which a single rebuild emits a warning."
  :type 'number
  :group 'gfm-tables)

(defconst gfm-tables--border-face '+markdown-overlay-border-face
  "Face used for table border characters.")

(defvar gfm-tables--stats)
(defvar-local gfm-tables--dirty-region nil
  "Buffer-local (BEG . END) covering all unrebuilt edits, or nil if clean.
The dirty region grows monotonically until the idle timer fires and
`gfm-tables--rebuild-scoped' consumes it.")

(defvar gfm-tables-mode-map (make-sparse-keymap)
  "Keymap for `gfm-tables-mode'.
Intentionally empty: cell-wise bindings live on row overlays via the
`keymap' overlay property (`gfm-tables--row-map') so they only apply
inside a table.")

(defconst gfm-tables--delim-re
  ;; Matches a delimiter row like `| --- | :---: | ---: |' — outer pipes
  ;; with one or more dash-runs between them, each optionally flanked by
  ;; `:' for column alignment, and arbitrary trailing whitespace.
  (rx line-start
      ;; First column: `| ---' or `| :---:' etc.
      "|" (* " ") (? ":") (+ "-") (? ":") (* " ")
      ;; Subsequent columns: same shape, each starting with `|'.
      (* "|" (* " ") (? ":") (+ "-") (? ":") (* " "))
      ;; Closing pipe + optional trailing whitespace to end-of-line.
      "|" (* blank) line-end)
  "Regexp matching a GFM table delimiter row.")

;;; Cell fontification

(defconst gfm-tables--fontify-buffer-name " *gfm-tables-fontify*")

(defun gfm-tables--fontify-buffer ()
  "Return a reusable hidden buffer in `markdown-mode' for cell fontification."
  (let ((buf (get-buffer gfm-tables--fontify-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create gfm-tables--fontify-buffer-name))
      (with-current-buffer buf
        (let ((inhibit-message t))
          (delay-mode-hooks
            (when (fboundp 'markdown-mode)
              (markdown-mode))))))
    buf))

(defun gfm-tables--fontify-cell (cell)
  "Return CELL with markdown inline syntax fontified.
The visible width of the result equals the visible width of CELL,
provided `markdown-hide-markup' is nil (the default)."
  (cond
   ((or (null cell) (string-empty-p cell)) (or cell ""))
   ((not (fboundp 'markdown-mode)) cell)
   (t
    (with-current-buffer (gfm-tables--fontify-buffer)
      (let ((inhibit-modification-hooks t))
        (erase-buffer)
        (insert cell)
        (font-lock-ensure))
      (buffer-string)))))

(defun gfm-tables--invisible-p (val spec)
  "Non-nil if invisibility property VAL is hidden under SPEC.
SPEC is a `buffer-invisibility-spec' value."
  (cond
   ((null val) nil)
   ((eq spec t) t)
   ((listp spec)
    (cl-some (lambda (e)
               (let ((tag (if (consp e) (car e) e)))
                 (if (listp val) (memq tag val) (eq tag val))))
             spec))))

(defvar gfm-tables--width-cache nil
  "`eq'-keyed hash table memoising `gfm-tables--visible-width' results.
Bound to a fresh table at the start of each rebuild and unbound (nil)
outside one, so the cache never crosses rebuild boundaries.")

(defun gfm-tables--no-width-affecting-props-p (s)
  "Non-nil if S carries no `display', `composition', or `invisible' props."
  (let ((n (length s)) (i 0) (clean t))
    (while (and clean (< i n))
      (cond
       ((or (get-text-property i 'display s)
            (get-text-property i 'composition s)
            (get-text-property i 'invisible s))
        (setq clean nil))
       (t
        (let ((nd (or (next-single-property-change i 'display s) n))
              (ni (or (next-single-property-change i 'invisible s) n))
              (nc (or (next-single-property-change i 'composition s) n)))
          (setq i (min nd ni nc))))))
    clean))

(defun gfm-tables--visible-width--compute (s)
  "Walk S to compute its visible width; honours display/composition/invisible."
  (if (gfm-tables--no-width-affecting-props-p s)
      (string-width s)
    (let ((spec buffer-invisibility-spec)
          (w 0) (i 0) (n (length s)))
      (while (< i n)
        (let* ((invis (get-text-property i 'invisible s))
               (disp (get-text-property i 'display s))
               (comp (find-composition i nil s t)))
          (cond
           ((gfm-tables--invisible-p invis spec)
            (setq i (or (next-single-property-change i 'invisible s) n)))
           ((stringp disp)
            (cl-incf w (string-width disp))
            (setq i (or (next-single-property-change i 'display s) n)))
           ;; Honour only explicit compositions (those backed by a
           ;; `composition' text property).  Auto-compositions from
           ;; `composition-function-table' (e.g. `fl' / `--' ligatures
           ;; in the fontify scratch buffer) are not applied to overlay
           ;; display strings, so counting them here under-pads the
           ;; cell and pushes the closing border off-grid.
           ((and comp (= (nth 0 comp) i)
                 (get-text-property i 'composition s))
            (cl-incf w (or (nth 5 comp) 1))
            (setq i (nth 1 comp)))
           (t
            (let* ((nd (or (next-single-property-change i 'display s) n))
                   (ni (or (next-single-property-change i 'invisible s) n))
                   (nc (or (next-single-property-change i 'composition s) n))
                   (next (min nd ni nc)))
              (cl-incf w (string-width (substring-no-properties s i next)))
              (setq i next))))))
      w)))

(defun gfm-tables--visible-width (s)
  "Return on-screen width of S in the current buffer.
Honours `display' string replacements, the `composition' text property
\(used by `markdown-mode' to collapse hidden URLs into a single glyph),
and any `invisible' property currently hidden by `buffer-invisibility-spec'.
When `gfm-tables--width-cache' is bound, results are memoised by `eq'."
  (cond
   ((and gfm-tables--width-cache
         (gethash s gfm-tables--width-cache)))
   (t
    (let ((w (gfm-tables--visible-width--compute s)))
      (when gfm-tables--width-cache
        (puthash s w gfm-tables--width-cache))
      w))))

;;; Cell parser

(defun gfm-tables--split-row (line)
  "Split table-row LINE into a list of trimmed cell strings.
Honours `\\|' escapes and treats `|' inside backtick code spans as
literal.  Backtick spans support arbitrary opening run lengths and a
matching closing run; an unmatched opening run is treated as literal
backticks."
  (let* ((trim (string-trim line))
         (trim (if (string-prefix-p "|" trim) (substring trim 1) trim))
         (trim (if (string-suffix-p "|" trim) (substring trim 0 -1) trim))
         (n (length trim))
         (i 0)
         (start 0)
         (in-tick 0)
         (cells nil))
    (while (< i n)
      (let ((ch (aref trim i)))
        (cond
         ((and (= in-tick 0) (eq ch ?\\) (< (1+ i) n))
          (setq i (+ i 2)))
         ((eq ch ?`)
          (let ((run-start i))
            (while (and (< i n) (eq (aref trim i) ?`))
              (setq i (1+ i)))
            (let ((run-len (- i run-start)))
              (cond
               ((= in-tick 0)
                (let ((j i)
                      (matched nil))
                  (while (and (< j n) (not matched))
                    (if (eq (aref trim j) ?`)
                        (let ((js j))
                          (while (and (< j n) (eq (aref trim j) ?`))
                            (setq j (1+ j)))
                          (when (= (- j js) run-len)
                            (setq matched t)))
                      (setq j (1+ j))))
                  (when matched
                    (setq in-tick run-len))))
               ((= in-tick run-len)
                (setq in-tick 0))))))
         ((and (= in-tick 0) (eq ch ?|))
          (push (substring trim start i) cells)
          (setq i (1+ i))
          (setq start i))
         (t
          (setq i (1+ i))))))
    (push (substring trim start n) cells)
    (mapcar (lambda (c)
              (string-trim (replace-regexp-in-string "\\\\|" "|" c)))
            (nreverse cells))))

;;; Source-position cell bounds (for navigation)

(defun gfm-tables--cell-bounds (line-beg line-end)
  "Return source-position cell bounds for the table line at LINE-BEG..LINE-END.
Each element is (BEG . END) where BEG is the position right after the
opening `|' and END is the position of the closing `|'.  Honours `\\|'
escapes and `|' inside backtick code spans."
  (let* ((line (buffer-substring-no-properties line-beg line-end))
         (n (length line))
         (i 0)
         (in-tick 0)
         (pipes nil))
    (while (< i n)
      (let ((ch (aref line i)))
        (cond
         ((and (= in-tick 0) (eq ch ?\\) (< (1+ i) n))
          (setq i (+ i 2)))
         ((eq ch ?`)
          (let ((run-start i))
            (while (and (< i n) (eq (aref line i) ?`))
              (setq i (1+ i)))
            (let ((run-len (- i run-start)))
              (cond
               ((= in-tick 0)
                (let ((j i) (matched nil))
                  (while (and (< j n) (not matched))
                    (if (eq (aref line j) ?`)
                        (let ((js j))
                          (while (and (< j n) (eq (aref line j) ?`))
                            (setq j (1+ j)))
                          (when (= (- j js) run-len)
                            (setq matched t)))
                      (setq j (1+ j))))
                  (when matched (setq in-tick run-len))))
               ((= in-tick run-len)
                (setq in-tick 0))))))
         ((and (= in-tick 0) (eq ch ?|))
          (push i pipes)
          (setq i (1+ i)))
         (t (setq i (1+ i))))))
    (let ((sorted (nreverse pipes))
          (cells nil))
      (cl-loop for tail on sorted
               while (cdr tail)
               do (push (cons (+ line-beg (car tail) 1)
                              (+ line-beg (cadr tail)))
                        cells))
      (nreverse cells))))

;;; Block discovery

(defun gfm-tables--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defvar-local gfm-tables--blocks-cache nil
  "Pair (BUFFER-MODIFIED-TICK . BLOCKS) memoising `gfm-tables--find-blocks'.
BLOCKS is the unfiltered, full-buffer block list as returned by
`gfm-tables--find-blocks-1'.  Stale when its tick disagrees with
`buffer-modified-tick'.")

(defun gfm-tables--find-blocks-1 ()
  "Scan the buffer for GFM tables, ignoring any excluded-ranges filtering.
Each entry is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END).  Internal
helper for `gfm-tables--find-blocks'."
  (let (blocks)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward gfm-tables--delim-re nil t)
          (let* ((delim-beg (match-beginning 0))
                 (delim-end (match-end 0))
                 (header-beg
                  (save-excursion
                    (goto-char delim-beg)
                    (and (= (forward-line -1) 0)
                         (looking-at-p "^|")
                         (line-beginning-position)))))
            (cond
             ((not header-beg)
              (goto-char delim-end))
             (t
              (let ((body-beg (save-excursion
                                (goto-char delim-end)
                                (forward-line 1)
                                (line-beginning-position)))
                    (body-end nil))
                (save-excursion
                  (goto-char body-beg)
                  (let ((last body-beg)
                        (any nil))
                    (while (and (not (eobp))
                                (looking-at-p "^|"))
                      (setq any t
                            last (line-end-position))
                      (forward-line 1))
                    (setq body-end (and any last))))
                (cond
                 (body-end
                  (push (list header-beg delim-beg body-beg body-end)
                        blocks)
                  (goto-char body-end))
                 (t (goto-char delim-end))))))))))
    (nreverse blocks)))

(defun gfm-tables--find-blocks (&optional excluded-ranges)
  "Return all GFM tables in the buffer.
Each entry is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END).  Tables whose
delimiter row falls inside any (BEG . END) range in EXCLUDED-RANGES are
omitted.  The unfiltered scan is memoised by `buffer-modified-tick' so
non-modifying callers (cell motion, edit dispatch) reuse a single scan
between edits."
  (let* ((tick (buffer-modified-tick))
         (all (cond
               ((and gfm-tables--blocks-cache
                     (= tick (car gfm-tables--blocks-cache)))
                (cdr gfm-tables--blocks-cache))
               (t
                (let ((blocks (gfm-tables--find-blocks-1)))
                  (setq gfm-tables--blocks-cache (cons tick blocks))
                  blocks)))))
    (if excluded-ranges
        (cl-remove-if (lambda (b)
                        (gfm-tables--in-ranges-p (nth 1 b) excluded-ranges))
                      all)
      all)))

;;; Column widths

(defun gfm-tables--column-widths (rows)
  "Return per-column max widths across ROWS as a vector.
Each row is a list of cell strings; column count is the longest row."
  (let* ((cols (apply #'max 0 (mapcar #'length rows)))
         (widths (make-vector cols 0)))
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 0
               do (aset widths i
                        (max (aref widths i)
                             (gfm-tables--visible-width cell)))))
    widths))

(defun gfm-tables--box-width (col-widths)
  "Return the visual width of the table given COL-WIDTHS.
Width = 2 (outer pipes) + Σ(col-w + 2) + (cols - 1) gaps."
  (let* ((cols (length col-widths))
         (cell-sum (cl-loop for i from 0 below cols
                            sum (+ (aref col-widths i) 2))))
    (+ 2 cell-sum (max 0 (1- cols)))))

;;; Width fitting

(defun gfm-tables--available-width ()
  "Return the available char width for a table in the current buffer.
Uses `window-max-chars-per-line', which accounts for the wrap-indicator
column on ttys and the fringe on graphical frames.  Falls back to
`fill-column' or 80 when no window currently shows the buffer."
  (let ((win (or (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-tables--fit-widths (natural budget)
  "Cap NATURAL column widths so their sum ≤ BUDGET.
Returns a vector the same length as NATURAL.  Uses water-filling:
columns naturally below the cap stay at their natural width; columns
above are capped.  Any integer slack left after capping is
distributed +1 at a time across the capped columns so the result
uses the full BUDGET.  Cap floors at 1 to avoid empty columns."
  (let* ((widths (cl-coerce natural 'vector))
         (total (cl-loop for w across widths sum w)))
    (cond
     ((<= total budget) widths)
     ((zerop (length widths)) widths)
     (t
      (let ((lo 1)
            (hi (apply #'max 1 (cl-coerce widths 'list))))
        (while (< lo hi)
          (let* ((mid (/ (+ lo hi 1) 2))
                 (s (cl-loop for w across widths sum (min w mid))))
            (if (> s budget)
                (setq hi (1- mid))
              (setq lo mid))))
        (let* ((cap (max 1 lo))
               (fitted (cl-map 'vector (lambda (w) (min w cap)) widths))
               (slack (- budget (cl-loop for w across fitted sum w))))
          (cl-loop for i from 0 below (length fitted)
                   while (> slack 0)
                   when (and (= (aref fitted i) cap)
                             (> (aref widths i) cap))
                   do (cl-incf (aref fitted i))
                      (cl-decf slack))
          fitted))))))

;;; Cell wrapping

(defun gfm-tables--cell-tokens (cell)
  "Split CELL on whitespace runs into a list of word substrings.
Text properties on CELL are preserved on each word."
  (let ((n (length cell)) (i 0) (words nil))
    (while (< i n)
      (while (and (< i n) (memq (aref cell i) '(?\s ?\t)))
        (cl-incf i))
      (let ((start i))
        (while (and (< i n) (not (memq (aref cell i) '(?\s ?\t))))
          (cl-incf i))
        (when (> i start)
          (push (substring cell start i) words))))
    (nreverse words)))

(defun gfm-tables--slice-by-visible-width (s width)
  "Slice S into chunks each with visible-width ≤ WIDTH, preserving props.
A WIDTH ≤ 0 still makes progress: at least one char per chunk."
  (let ((target (max 1 width))
        (chunks nil)
        (i 0)
        (n (length s)))
    (while (< i n)
      (let ((j i) (w 0))
        (while (and (< j n)
                    (let ((cw (string-width
                               (substring-no-properties s j (1+ j)))))
                      (or (= j i) (<= (+ w cw) target))))
          (cl-incf w (string-width (substring-no-properties s j (1+ j))))
          (cl-incf j))
        (push (substring s i j) chunks)
        (setq i j)))
    (nreverse chunks)))

(defun gfm-tables--wrap-cell (cell width)
  "Wrap fontified CELL to lines of visible-width ≤ WIDTH.
Returns a non-empty list of propertized line strings.  Words longer
than WIDTH are hard-broken.  An empty CELL returns (\"\")."
  (let ((target (max 1 width))
        (words (gfm-tables--cell-tokens cell))
        (line "")
        (lines nil))
    (dolist (w words)
      (let ((ww (gfm-tables--visible-width w)))
        (cond
         ((> ww target)
          (unless (string-empty-p line)
            (push line lines)
            (setq line ""))
          (let ((chunks (gfm-tables--slice-by-visible-width w target)))
            (dolist (c (butlast chunks))
              (push c lines))
            (setq line (car (last chunks)))))
         ((<= (+ (gfm-tables--visible-width line)
                 (if (string-empty-p line) 0 1)
                 ww)
              target)
          (setq line (if (string-empty-p line) w (concat line " " w))))
         (t
          (push line lines)
          (setq line w)))))
    (push line lines)
    (or (nreverse lines) (list ""))))

;;; Compose strings

(defun gfm-tables--compose-row (cells col-widths role)
  "Compose ROLE row's display string from CELLS and COL-WIDTHS.
ROLE is one of `header', `body-default', or `body-alt'.

Default-bg segments (gap chars, default-bg cells, header cells) are
left un-faced so they inherit the active `default' face.  This lets
focus-dim packages (per-window face remaps) and the terminal's tmux
pane dim track those segments automatically without any rebuild."
  (let* ((border-face gfm-tables--border-face)
         (cell-face (cl-case role
                      (header '(:weight bold))
                      (body-alt 'gfm-tables-row-alt-face)
                      (t nil)))
         (n (length col-widths))
         (parts nil)
         (pipe (propertize "│" 'face border-face))
         (lhs pipe)
         (rhs pipe))
    (push lhs parts)
    (cl-loop for i from 0 below n
             for w = (aref col-widths i)
             for cell = (or (nth i cells) "")
             for cell-w = (gfm-tables--visible-width cell)
             for pad = (max 0 (- w cell-w))
             for rendered = (concat " " cell (make-string pad ?\s) " ")
             do (when cell-face
                  (add-face-text-property 0 (length rendered)
                                          cell-face t rendered))
                (push rendered parts)
                (when (< i (1- n))
                  (push " " parts)))
    (push rhs parts)
    (apply #'concat (nreverse parts))))

(defun gfm-tables--wrap-row-into-lines (cells col-widths)
  "Wrap CELLS to COL-WIDTHS, returning a list of per-visual-line cell-lists.
Each entry is the cells used for one visual line; cells whose wrap is
shorter than the row's height are padded with empty strings."
  (let* ((widths-list (cl-coerce col-widths 'list))
         (wrapped (cl-mapcar #'gfm-tables--wrap-cell cells widths-list))
         (height (apply #'max 1 (mapcar #'length wrapped)))
         (padded (mapcar (lambda (lines)
                           (append lines
                                   (make-list (- height (length lines)) "")))
                         wrapped)))
    (mapcar (lambda (idx)
              (mapcar (lambda (cell-lines) (nth idx cell-lines)) padded))
            (number-sequence 0 (1- height)))))

(cl-defstruct (gfm-tables--row-layout
               (:constructor gfm-tables--make-row-layout)
               (:copier nil))
  "Pre-computed wrapping for a single source row.
LINE-CELLS is a list of cell-lists, one per visual line, used by the
display-string composer.  N-CELLS is the column count, N-LINES the
visual-line count, and BOUNDS-VEC a flat fixnum vector of length
\(* 2 N-CELLS N-LINES) holding (BEG . END) pairs for every cell on
every visual line, packed in row-major order: BEG for cell IDX on
visual line LINE lives at index `(* 2 (+ (* LINE N-CELLS) IDX))', END
at the next slot.  Packing the bounds into a single vector turns the
inner lookup in `gfm-tables--apply-cell-highlight' from nested
cons-list traversal into O(1) `aref'."
  line-cells
  n-cells
  n-lines
  bounds-vec)

(defun gfm-tables--row-layout (cells col-widths)
  "Return a `gfm-tables--row-layout' for CELLS at COL-WIDTHS.
Computes wrapping and per-cell char bounds once so callers can share
the result instead of redoing the wrap."
  (let* ((line-cells (gfm-tables--wrap-row-into-lines cells col-widths))
         (n-cells (length col-widths))
         (n-lines (length line-cells))
         (bounds-vec (make-vector (* 2 n-cells n-lines) 0))
         (line-idx 0))
    (dolist (lc line-cells)
      (let ((cell-idx 0))
        (dolist (b (gfm-tables--row-char-bounds lc col-widths))
          (let ((base (* 2 (+ (* line-idx n-cells) cell-idx))))
            (aset bounds-vec base (car b))
            (aset bounds-vec (1+ base) (cdr b)))
          (cl-incf cell-idx)))
      (cl-incf line-idx))
    (gfm-tables--make-row-layout
     :line-cells line-cells
     :n-cells n-cells
     :n-lines n-lines
     :bounds-vec bounds-vec)))

(defun gfm-tables--display-cell-bounds (layout)
  "Return the (N-CELLS . BOUNDS-VEC) pair stored on a row's overlay.
The pair lets `gfm-tables--apply-cell-highlight' index the packed
bounds vector without keeping `LAYOUT' alive (the wrapped cell
strings in `line-cells' are no longer needed after composition)."
  (cons (gfm-tables--row-layout-n-cells layout)
        (gfm-tables--row-layout-bounds-vec layout)))

(defun gfm-tables--compose-row-from-layout (layout col-widths role)
  "Compose ROLE row's display string from pre-computed LAYOUT and COL-WIDTHS."
  (mapconcat (lambda (line-cells)
               (gfm-tables--compose-row line-cells col-widths role))
             (gfm-tables--row-layout-line-cells layout)
             "\n"))

(defun gfm-tables--compose-multiline-row (cells col-widths role)
  "Compose ROLE row from CELLS, wrapping each to its column in COL-WIDTHS.
Returns a string in which visual lines are joined by newlines.  All
cells in the row are padded to the tallest cell's height with empty
strings, so the column grid stays aligned across visual lines."
  (gfm-tables--compose-row-from-layout
   (gfm-tables--row-layout cells col-widths)
   col-widths role))

(defun gfm-tables--row-char-bounds (cells col-widths)
  "Return per-cell (BEG . END) offsets for CELLS at COL-WIDTHS.
Each range covers the cell's display segment from the leading padding
space through the trailing padding space, exclusive of outer pipes and
inter-cell gaps, in `gfm-tables--compose-row's output.  Bounds reflect
the cell strings' actual lengths, so they remain accurate when cells
contain compositions or other text-property tricks that make displayed
width less than char length."
  (let ((n (length col-widths))
        (pos 1)
        (bounds nil))
    (cl-loop for i from 0 below n
             for cell = (or (nth i cells) "")
             for cell-w = (gfm-tables--visible-width cell)
             for w = (aref col-widths i)
             for pad = (max 0 (- w cell-w))
             for seg-len = (+ 2 (length cell) pad)
             do (push (cons pos (+ pos seg-len)) bounds)
                (setq pos (+ pos seg-len))
                (when (< i (1- n))
                  (setq pos (1+ pos))))
    (nreverse bounds)))

(defun gfm-tables--multiline-row-char-bounds (cells col-widths)
  "Per-visual-line char bounds for CELLS wrapped to COL-WIDTHS.
Returns a list with one entry per visual line in
`gfm-tables--compose-multiline-row's output; each entry is the
`gfm-tables--row-char-bounds' for that line's wrapped cells.
Reconstructs the legacy list-of-lists shape from the packed bounds
vector for callers (and tests) that depend on the older API."
  (let* ((layout (gfm-tables--row-layout cells col-widths))
         (vec (gfm-tables--row-layout-bounds-vec layout))
         (n-cells (gfm-tables--row-layout-n-cells layout))
         (n-lines (gfm-tables--row-layout-n-lines layout)))
    (cl-loop for line below n-lines
             collect (cl-loop for cell below n-cells
                              for base = (* 2 (+ (* line n-cells) cell))
                              collect (cons (aref vec base)
                                            (aref vec (1+ base)))))))

(defun gfm-tables--rule-row (box-width)
  "Return a `├─…─┤' T-junction rule between header and body, width BOX-WIDTH."
  (let ((face gfm-tables--border-face))
    (concat (propertize "├" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┤" 'face face))))

(defun gfm-tables--top-border (box-width)
  "Return a `┌─…─┐' top border of total width BOX-WIDTH."
  (let ((face gfm-tables--border-face))
    (concat (propertize "┌" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┐" 'face face))))

(defun gfm-tables--bottom-border (box-width)
  "Return a `└─…─┘' bottom border of total width BOX-WIDTH."
  (let ((face gfm-tables--border-face))
    (concat (propertize "└" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┘" 'face face))))

;;; Performance instrumentation primitives

(defun gfm-tables--accum-phase (phase delta)
  "Accumulate DELTA seconds into PHASE in `gfm-tables--stats'.
No-op when stats are not initialised."
  (when gfm-tables--stats
    (let ((totals (alist-get 'phase-totals gfm-tables--stats)))
      (when totals
        (setf (alist-get phase totals)
              (+ delta (or (alist-get phase totals) 0.0)))
        (setf (alist-get 'phase-totals gfm-tables--stats) totals)))))

(defmacro gfm-tables--time-phase (phase &rest body)
  "Run BODY, accumulating its wall-time into PHASE on `gfm-tables--stats'."
  (declare (indent 1) (debug (form body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (current-time)))
       (prog1 (progn ,@body)
         (gfm-tables--accum-phase
          ,phase (float-time (time-since ,start)))))))

;;; Overlay application

(defvar-local gfm-tables--overlays nil
  "All gfm-tables overlays currently in this buffer.")

(defun gfm-tables--register (ov)
  "Tag OV as a gfm-tables overlay and remember it for bulk cleanup."
  (overlay-put ov 'gfm-tables t)
  (push ov gfm-tables--overlays)
  ov)

(defun gfm-tables--remove-overlays (&optional beg end)
  "Remove all gfm-tables overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'gfm-tables t)
  (cond
   ((or beg end)
    (setq gfm-tables--overlays
          (cl-remove-if-not #'overlay-buffer gfm-tables--overlays)))
   (t (setq gfm-tables--overlays nil))))

(defun gfm-tables--remove-overlays-in-block (block)
  "Remove gfm-tables overlays within BLOCK and prune the cached list.
BLOCK is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END)."
  (let ((header-beg (nth 0 block))
        (body-end (nth 3 block)))
    (gfm-tables--remove-overlays header-beg (1+ body-end))))

(defun gfm-tables--apply-table (header-beg delim-beg body-beg body-end)
  "Decorate one table identified by HEADER-BEG, DELIM-BEG, BODY-BEG, BODY-END."
  (save-excursion
    (let* ((header-end (save-excursion
                         (goto-char header-beg) (line-end-position)))
           (delim-end (save-excursion
                        (goto-char delim-beg) (line-end-position)))
           (header-line (buffer-substring-no-properties
                         header-beg header-end))
           (header-cells (gfm-tables--time-phase 'parse
                           (mapcar #'gfm-tables--fontify-cell
                                   (gfm-tables--split-row header-line))))
           (body-rows '())
           (body-positions '()))
      (gfm-tables--time-phase 'parse
        (goto-char body-beg)
        (while (< (point) body-end)
          (let* ((lbeg (line-beginning-position))
                 (lend (line-end-position))
                 (line (buffer-substring-no-properties lbeg lend)))
            (push (cons lbeg lend) body-positions)
            (push (mapcar #'gfm-tables--fontify-cell
                          (gfm-tables--split-row line))
                  body-rows))
          (forward-line 1))
        (setq body-rows (nreverse body-rows)
              body-positions (nreverse body-positions)))
      (let* ((all-rows (cons header-cells body-rows))
             (natural (gfm-tables--time-phase 'layout
                        (gfm-tables--column-widths all-rows)))
             (n-cols (length natural))
             (overhead (+ (* 3 n-cols) 1))
             (budget (max n-cols
                          (- (gfm-tables--available-width) overhead)))
             (col-widths (gfm-tables--time-phase 'layout
                           (gfm-tables--fit-widths natural budget)))
             (box-width (gfm-tables--box-width col-widths))
             (top (gfm-tables--top-border box-width))
             (bottom (gfm-tables--bottom-border box-width))
             (rule (gfm-tables--rule-row box-width))
             (n-body (length body-rows))
             (header-layout (gfm-tables--time-phase 'layout
                              (gfm-tables--row-layout header-cells col-widths)))
             (body-layouts (gfm-tables--time-phase 'layout
                             (mapcar (lambda (cells)
                                       (gfm-tables--row-layout cells col-widths))
                                     body-rows))))
        ;; Header overlay carries top border as before-string.
        (gfm-tables--time-phase 'apply
          (let ((ov (make-overlay header-beg header-end))
                (str (gfm-tables--time-phase 'compose
                       (gfm-tables--compose-row-from-layout
                        header-layout col-widths 'header))))
            (overlay-put ov 'display str)
            (overlay-put ov 'before-string (concat top "\n"))
            (overlay-put ov 'keymap gfm-tables--row-map)
            (overlay-put ov 'gfm-tables-cell-bounds
                         (gfm-tables--cell-bounds header-beg header-end))
            (overlay-put ov 'gfm-tables-col-widths col-widths)
            (overlay-put ov 'gfm-tables-display-cell-bounds
                         (gfm-tables--display-cell-bounds header-layout))
            (gfm-tables--register ov))
          ;; Delimiter row → continuous rule.
          (let ((ov (make-overlay delim-beg delim-end)))
            (overlay-put ov 'display rule)
            (gfm-tables--register ov))
          ;; Body rows.
          (cl-loop for (lbeg . lend) in body-positions
                   for cells in body-rows
                   for layout in body-layouts
                   for idx from 1
                   for last-p = (= idx n-body)
                   for role = (if (cl-evenp idx) 'body-alt 'body-default)
                   for str = (gfm-tables--time-phase 'compose
                               (gfm-tables--compose-row-from-layout
                                layout col-widths role))
                   do (let ((ov (make-overlay lbeg lend)))
                        (overlay-put ov 'display str)
                        (overlay-put ov 'keymap gfm-tables--row-map)
                        (overlay-put ov 'gfm-tables-cell-bounds
                                     (gfm-tables--cell-bounds lbeg lend))
                        (overlay-put ov 'gfm-tables-col-widths col-widths)
                        (overlay-put ov 'gfm-tables-display-cell-bounds
                                     (gfm-tables--display-cell-bounds layout))
                        (when last-p
                          (overlay-put ov 'after-string
                                       (concat "\n" bottom)))
                        (gfm-tables--register ov))))))))

(defun gfm-tables--fenced-ranges ()
  "Return (BEG . END) ranges of fenced code blocks, if discoverable."
  (and (fboundp 'gfm-code-fences--find-blocks)
       (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b)))
               (gfm-code-fences--find-blocks))))

(defun gfm-tables--apply-overlays ()
  "Apply table overlays to all GFM tables in the buffer."
  (let* ((blocks (gfm-tables--time-phase 'find-blocks
                   (gfm-tables--find-blocks (gfm-tables--fenced-ranges))))
         (n 0))
    (dolist (block blocks)
      (cl-destructuring-bind (h d bb be) block
        (gfm-tables--apply-table h d bb be)
        (cl-incf n)))
    n))

;;; Performance instrumentation

(defvar-local gfm-tables--stats nil
  "Per-buffer alist of rebuild stats.")

(defconst gfm-tables--phase-keys
  '(find-blocks parse layout compose apply)
  "Keys used in `gfm-tables--stats' `phase-totals' alist, in display order.")

(defun gfm-tables--init-stats ()
  "Reset the per-buffer rebuild stats to zero."
  (setq gfm-tables--stats
        (list (cons 'rebuild-count 0)
              (cons 'total-time 0.0)
              (cons 'last-time 0.0)
              (cons 'max-time 0.0)
              (cons 'table-count 0)
              (cons 'phase-totals
                    (mapcar (lambda (k) (cons k 0.0))
                            gfm-tables--phase-keys)))))

(defun gfm-tables--record-stats (duration table-count)
  "Update stats with DURATION and TABLE-COUNT from one rebuild."
  (unless gfm-tables--stats (gfm-tables--init-stats))
  (setf (alist-get 'rebuild-count gfm-tables--stats)
        (1+ (alist-get 'rebuild-count gfm-tables--stats)))
  (setf (alist-get 'total-time gfm-tables--stats)
        (+ duration (alist-get 'total-time gfm-tables--stats)))
  (setf (alist-get 'last-time gfm-tables--stats) duration)
  (setf (alist-get 'max-time gfm-tables--stats)
        (max duration (alist-get 'max-time gfm-tables--stats)))
  (setf (alist-get 'table-count gfm-tables--stats) table-count)
  (when (> duration gfm-tables-slow-rebuild-threshold)
    (message "gfm-tables: slow rebuild in %s: %.3fs"
             (buffer-name) duration)))

(defun gfm-tables--format-phase-totals (totals)
  "Return a phase-by-phase summary string for TOTALS, sorted by total desc."
  (let ((sorted (sort (copy-sequence totals)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (mapconcat (lambda (p) (format "%s=%.3fs" (car p) (cdr p)))
               sorted " ")))

(defun gfm-tables-stats ()
  "Display the current buffer's gfm-tables rebuild statistics."
  (interactive)
  (if (not gfm-tables--stats)
      (message "gfm-tables: no stats yet")
    (let-alist gfm-tables--stats
      (message
       "gfm-tables [%s]: rebuilds=%d total=%.3fs last=%.3fs max=%.3fs tables=%d | %s"
       (buffer-name)
       .rebuild-count .total-time .last-time .max-time .table-count
       (gfm-tables--format-phase-totals .phase-totals)))))

;;; Rebuild scheduler

(defvar-local gfm-tables--last-available-width nil
  "Available char width recorded on the last full rebuild.
Compared in `gfm-tables--schedule-full-rebuild' so window-config
changes that leave the width unchanged (minibuffer activity, focus
shifts, scroll-margin tweaks) do not schedule a redundant rebuild.")

(defun gfm-tables--rebuild ()
  "Remove and recreate all gfm-tables overlays.
Re-applies the active-cell highlight afterwards so cell selection
survives window-configuration changes and other rebuild triggers."
  (let ((start (current-time))
        (gfm-tables--width-cache (make-hash-table :test 'eq)))
    (gfm-tables--remove-overlays)
    (setq gfm-tables--dirty-region nil)
    (let ((n (gfm-tables--apply-overlays)))
      (gfm-tables--record-stats (float-time (time-since start)) n))
    (setq gfm-tables--last-available-width (gfm-tables--available-width))
    (gfm-tables--update-cursor-highlight)))

(defun gfm-tables--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that table."
  (let ((start (current-time))
        (gfm-tables--width-cache (make-hash-table :test 'eq)))
    (gfm-tables--remove-overlays-in-block block)
    (cl-destructuring-bind (h d bb be) block
      (gfm-tables--apply-table h d bb be))
    (gfm-tables--record-stats (float-time (time-since start)) 1))
  (gfm-tables--update-cursor-highlight))

(defun gfm-tables--extend-dirty-region (beg end)
  "Extend the buffer's dirty region to cover BEG..END."
  (cond
   ((null gfm-tables--dirty-region)
    (setq gfm-tables--dirty-region (cons beg end)))
   (t
    (setcar gfm-tables--dirty-region
            (min (car gfm-tables--dirty-region) beg))
    (setcdr gfm-tables--dirty-region
            (max (cdr gfm-tables--dirty-region) end)))))

(defun gfm-tables--region-overlaps-p (a b)
  "Non-nil if (BEG . END) ranges A and B overlap."
  (and (<= (car a) (cdr b)) (>= (cdr a) (car b))))

(defun gfm-tables--block-line-range (block)
  "Return (HEADER-BEG . BODY-END+1) for BLOCK."
  (cons (nth 0 block) (1+ (nth 3 block))))

(defun gfm-tables--block-fully-contains-p (block region)
  "Non-nil if REGION lies inside BLOCK's HEADER-BEG..BODY-END+1 range."
  (let ((br (gfm-tables--block-line-range block)))
    (and (>= (car region) (car br))
         (<= (cdr region) (cdr br)))))

(defun gfm-tables--fence-line-ranges ()
  "Return per-line (BEG . END) ranges for opening/closing code-fence lines."
  (when (fboundp 'gfm-code-fences--find-blocks)
    (cl-loop for b in (gfm-code-fences--find-blocks)
             for open-beg = (nth 0 b)
             for close-end = (nth 3 b)
             nconc (list
                    (cons (save-excursion
                            (goto-char open-beg) (line-beginning-position))
                          (save-excursion
                            (goto-char open-beg) (line-end-position)))
                    (cons (save-excursion
                            (goto-char close-end) (line-beginning-position))
                          (save-excursion
                            (goto-char close-end) (line-end-position)))))))

(defun gfm-tables--region-overlaps-fence-line-p (region)
  "Non-nil if REGION overlaps a fenced-code-block fence line."
  (cl-some (lambda (r) (gfm-tables--region-overlaps-p region r))
           (gfm-tables--fence-line-ranges)))

(defun gfm-tables--rebuild-scoped ()
  "Rebuild only what `gfm-tables--dirty-region' demands.
No-op when the region intersects no decorated table; falls back to a
full rebuild when the region spans a table boundary, multiple tables,
or any code-fence line."
  (let ((dirty gfm-tables--dirty-region))
    (setq gfm-tables--dirty-region nil)
    (cond
     ((null dirty) nil)
     ((gfm-tables--region-overlaps-fence-line-p dirty)
      (gfm-tables--rebuild))
     (t
      (let* ((excluded (gfm-tables--fenced-ranges))
             (blocks (gfm-tables--find-blocks excluded))
             (matching (cl-loop for b in blocks
                                when (gfm-tables--region-overlaps-p
                                      dirty
                                      (gfm-tables--block-line-range b))
                                collect b)))
        (cond
         ((null matching) nil)
         ((and (null (cdr matching))
               (gfm-tables--block-fully-contains-p (car matching) dirty))
          (gfm-tables--rebuild-block (car matching)))
         (t
          (gfm-tables--rebuild))))))))

(defvar-local gfm-tables--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defvar gfm-tables-mode)

(defun gfm-tables--arm-rebuild-timer (callback)
  "Cancel any pending rebuild timer and schedule CALLBACK after idle."
  (when (timerp gfm-tables--rebuild-timer)
    (cancel-timer gfm-tables--rebuild-timer))
  (setq gfm-tables--rebuild-timer
        (run-with-idle-timer
         0.2 nil
         (lambda (buf cb)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when gfm-tables-mode
                 (funcall cb)))))
         (current-buffer) callback)))

(defun gfm-tables--schedule-rebuild (&optional beg end _len)
  "Merge BEG..END into the dirty region and arm the rebuild timer.
Skips indirect buffers since base buffer overlays already cover them."
  (unless (buffer-base-buffer)
    (when (and beg end)
      (gfm-tables--extend-dirty-region beg end))
    (gfm-tables--arm-rebuild-timer #'gfm-tables--rebuild-scoped)))

(defun gfm-tables--schedule-full-rebuild (&rest _)
  "Schedule a full-buffer rebuild on next idle if the window width changed.
Window-configuration-change-hook fires for many reasons unrelated to
column-affecting size (minibuffer activity, focus, scroll-margin), so
we compare the current `gfm-tables--available-width' against the cached
value from the last rebuild and skip when they match."
  (unless (buffer-base-buffer)
    (let ((w (gfm-tables--available-width)))
      (unless (eql w gfm-tables--last-available-width)
        (gfm-tables--arm-rebuild-timer #'gfm-tables--rebuild)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-tables-mode
  "Adorn GFM tables with bordered, zebra-striped overlays."
  :lighter " gfm-tb"
  :keymap gfm-tables-mode-map
  (if gfm-tables-mode
      (progn
        (gfm-tables--init-stats)
        (gfm-tables--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-tables--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-tables--schedule-full-rebuild nil t)
        (add-hook 'post-command-hook
                  #'gfm-tables--update-cursor-highlight nil t))
    (remove-hook 'after-change-functions
                 #'gfm-tables--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-tables--schedule-full-rebuild t)
    (remove-hook 'post-command-hook
                 #'gfm-tables--update-cursor-highlight t)
    (when (timerp gfm-tables--rebuild-timer)
      (cancel-timer gfm-tables--rebuild-timer))
    (gfm-tables--hide-cursor-highlight)
    (gfm-tables--remove-overlays)))

;;; Indirect editing

(declare-function edit-indirect-region "edit-indirect")
(declare-function orgtbl-mode "org-table")
(declare-function markdown-mode "markdown-mode")

(defun gfm-tables--block-at-point ()
  "Return (BEG . END) of the table block containing point, or nil."
  (let ((pt (point)))
    (cl-loop for (header-beg _delim-beg _body-beg body-end)
             in (gfm-tables--find-blocks)
             when (and (>= pt header-beg) (<= pt body-end))
             return (cons header-beg body-end))))

(defvar-local gfm-tables--edit-source-buffer nil
  "Buffer-local in the indirect edit buffer; points to the parent buffer.")

(defvar-local gfm-tables--edit-pending-cell-info nil
  "Stashed (LINE-OFFSET . CELL-IDX) captured before edit-indirect commits.
Set on the parent buffer by `gfm-tables--edit-before-commit', consumed
by `gfm-tables--edit-after-commit' which runs in the parent.")

(defun gfm-tables--edit-cell-info-here (&optional region-beg)
  "Return (LINE-OFFSET . CELL-IDX) for point on the current line.
LINE-OFFSET is counted from REGION-BEG (defaulting to `point-min'),
so the value is comparable between a source buffer (passing the table's
start) and an indirect edit buffer (whose `point-min' is that start)."
  (let* ((cb (gfm-tables--cell-bounds (line-beginning-position)
                                      (line-end-position)))
         (idx (or (and cb
                       (cl-position-if
                        (lambda (b)
                          (and (>= (point) (car b))
                               (< (point) (cdr b))))
                        cb))
                  0))
         (line-offset (count-lines (or region-beg (point-min))
                                   (line-beginning-position))))
    (cons line-offset idx)))

(defun gfm-tables--goto-cell-on-current-line (idx)
  "Move point to the start of cell IDX on the current line, if it exists."
  (let ((cb (gfm-tables--cell-bounds (line-beginning-position)
                                     (line-end-position))))
    (when (and cb (< idx (length cb)))
      (goto-char (car (nth idx cb))))))

(defun gfm-tables--edit-before-commit ()
  "Capture indirect-buffer cell position into the parent buffer."
  (let ((info (gfm-tables--edit-cell-info-here))
        (parent gfm-tables--edit-source-buffer))
    (when (and parent (buffer-live-p parent))
      (with-current-buffer parent
        (setq gfm-tables--edit-pending-cell-info info)))))

(defun gfm-tables--edit-after-commit (beg _end)
  "After commit, move point to the captured cell starting from BEG.
Recenters if off-screen.  The actual move is deferred to the next
event-loop tick so it runs after any after-advice on
`edit-indirect-commit' (e.g. the one `markdown-mode' installs to track
committed positions)."
  (when gfm-tables--edit-pending-cell-info
    (let* ((info gfm-tables--edit-pending-cell-info)
           (line-offset (car info))
           (cell-idx (cdr info))
           (buf (current-buffer)))
      (setq gfm-tables--edit-pending-cell-info nil)
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (goto-char beg)
             (forward-line line-offset)
             (gfm-tables--goto-cell-on-current-line cell-idx)
             (let ((win (get-buffer-window buf)))
               (when (window-live-p win)
                 (set-window-point win (point))
                 (unless (pos-visible-in-window-p (point) win)
                   (with-selected-window win (recenter)))))
             (when (bound-and-true-p gfm-tables-mode)
               (gfm-tables--update-cursor-highlight)))))))))

(defun gfm-tables--cell-content-bounds (row-ov idx)
  "Return (BEG . END) of cell IDX inside ROW-OV, trimmed of whitespace."
  (let* ((cb (overlay-get row-ov 'gfm-tables-cell-bounds))
         (cell (nth idx cb))
         (raw-beg (car cell))
         (raw-end (cdr cell)))
    (save-excursion
      (goto-char raw-beg)
      (skip-chars-forward " \t" raw-end)
      (let ((b (point)))
        (goto-char raw-end)
        (skip-chars-backward " \t" raw-beg)
        (cons b (point))))))

(defun gfm-tables--cell-link-pos (row-ov idx)
  "Return the buffer position of the first link inside cell IDX of ROW-OV.
Searches for inline `[text](url)', reference `[text][ref]', and bare URI
forms within the cell's source range.  Returns nil if none is found."
  (let* ((bounds (gfm-tables--cell-content-bounds row-ov idx))
         (beg (car bounds))
         (end (cdr bounds)))
    (cl-loop for re in (list markdown-regex-link-inline
                             markdown-regex-link-reference
                             markdown-regex-uri
                             markdown-regex-angle-uri)
             for hit = (save-excursion
                         (goto-char beg)
                         (and (re-search-forward re end t)
                              (match-beginning 0)))
             when hit return hit)))

(defun gfm-tables--edit-header-line (label)
  "Return a header-line string for an indirect edit buffer titled LABEL."
  (concat (propertize (concat " " label) 'face 'mode-line-emphasis)
          (propertize "  C-c C-c" 'face 'help-key-binding) " commit"
          (propertize "  C-c C-k" 'face 'help-key-binding) " abort"))

(defun gfm-tables--cell-edit-sanitise ()
  "Strip newlines and escape unescaped `|' in the current buffer.
Run in the indirect cell edit buffer just before commit."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " " nil t))
    (goto-char (point-min))
    (while (re-search-forward "\\(\\`\\|[^\\\\]\\)\\(|\\)" nil t)
      (replace-match "\\1\\\\|" t nil))))

;;;###autoload
(defun gfm-tables-edit-cell-at-point ()
  "Open the active table cell in an indirect markdown edit buffer.
On commit, embedded newlines are replaced with spaces and any
unescaped `|' is escaped to `\\|'."
  (interactive)
  (require 'edit-indirect)
  (let ((info (gfm-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((row-ov (car info))
           (idx (cdr info))
           (bounds (gfm-tables--cell-content-bounds row-ov idx))
           (edit-indirect-guess-mode-function
            (lambda (_parent _beg _end) (markdown-mode)))
           (buf (edit-indirect-region (car bounds) (cdr bounds) t)))
      (with-current-buffer buf
        (setq-local require-final-newline nil)
        (setq-local mode-require-final-newline nil)
        (setq-local header-line-format
                    (gfm-tables--edit-header-line "Edit cell"))
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-tables--cell-edit-sanitise nil t)))))

;;;###autoload
(defun gfm-tables-edit-table-at-point ()
  "Open the GFM table containing point in an indirect edit buffer.
The edit buffer uses `markdown-mode' with `orgtbl-mode' enabled, so
TAB navigates cells, edits auto-align columns, and M-<right>/<left>
moves columns as in `org-mode'.  Point in the indirect buffer lands
at the same cell as in the parent.  When the indirect buffer is
committed, point in the parent is moved to the cell point was on
in the indirect buffer (recentering if it has scrolled off-screen)."
  (interactive)
  (require 'edit-indirect)
  (let ((bounds (gfm-tables--block-at-point))
        (src-buf (current-buffer)))
    (unless bounds
      (user-error "Point is not inside a GFM table"))
    (let* ((info (gfm-tables--edit-cell-info-here (car bounds)))
           (line-offset (car info))
           (cell-idx (cdr info))
           ;; Extend region to include the trailing newline so markdown-mode's
           ;; `after-commit-function' (which appends \n when the region doesn't
           ;; end in one) is a no-op.
           (region-end (min (point-max) (1+ (cdr bounds))))
           (edit-indirect-guess-mode-function
            (lambda (_parent _beg _end) (markdown-mode)))
           (buf (edit-indirect-region (car bounds) region-end t)))
      (with-current-buffer buf
        (when (fboundp 'orgtbl-mode)
          (orgtbl-mode 1))
        ;; Source region ends mid-line (at body-end, before its newline);
        ;; suppress the auto-trailing newline so commit doesn't insert one.
        (setq-local require-final-newline nil)
        (setq-local mode-require-final-newline nil)
        (setq-local header-line-format
                    (gfm-tables--edit-header-line "Edit table"))
        (setq gfm-tables--edit-source-buffer src-buf)
        (goto-char (point-min))
        (forward-line line-offset)
        (gfm-tables--goto-cell-on-current-line cell-idx)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-tables--edit-before-commit nil t))
      (with-current-buffer src-buf
        (add-hook 'edit-indirect-after-commit-functions
                  #'gfm-tables--edit-after-commit nil t)))))

;;; Active-cell highlight

(defvar-local gfm-tables--highlighted-row-ov nil
  "The row overlay whose display currently carries the cell highlight.")

(defvar-local gfm-tables--current-highlight-key nil
  "The (ROW-OV . IDX) currently rendered as the active-cell highlight.
Compared by `equal' in `gfm-tables--update-cursor-highlight' so motion
that lands on the same cell can skip the display repaint.")

(defvar-local gfm-tables--cursor-anchor nil
  "Buffer position currently flagged with the `cursor' text property.
Used to anchor the visible terminal cursor at the active cell's first
char rather than at the right edge of the overlay's display range.")

(defvar-local gfm-tables--saved-cursor-type 'gfm-tables-unset
  "Stashed `cursor-type' captured before being hidden over a table.
The sentinel `gfm-tables-unset' means no value is stashed; the
sentinel `gfm-tables-global' means the variable was not buffer-local
when we stashed it.")

(defun gfm-tables--save-and-hide-cursor ()
  "Hide the buffer cursor, stashing the previous value for restoration.
Reliable on graphical frames; tty cursor visibility is left as-is
because tmux + post-redisplay re-shows from Emacs's own internals
make it impractical to keep hidden in a terminal without intrusive
hacks."
  (when (eq gfm-tables--saved-cursor-type 'gfm-tables-unset)
    (setq gfm-tables--saved-cursor-type
          (if (local-variable-p 'cursor-type)
              cursor-type
            'gfm-tables-global)))
  (setq-local cursor-type nil))

(defun gfm-tables--restore-cursor ()
  "Restore the stashed `cursor-type', if any."
  (unless (eq gfm-tables--saved-cursor-type 'gfm-tables-unset)
    (if (eq gfm-tables--saved-cursor-type 'gfm-tables-global)
        (kill-local-variable 'cursor-type)
      (setq-local cursor-type gfm-tables--saved-cursor-type))
    (setq gfm-tables--saved-cursor-type 'gfm-tables-unset)))

(defconst gfm-tables--hint-groups
  `(("Edit"
     (((evil-forward-word-end) . "cell")
      ((evil-insert)           . "table"))
     (((evil-yank-line)        . "copy")))
    ("Columns" :when ,#'gfm-tables--in-header-p
     (((gfm-tables-swap-column-left gfm-tables-swap-column-right) . "swap"))))
  "Transient-style key hint groups shown while in a GFM table cell.
Each entry is (HEADING [KEYWORD VALUE...] ROW...) where ROW is a list of
\(CMDS . LABEL).  Supported keywords: `:when' PREDICATE — group is shown
only if PREDICATE returns non-nil.")

(defun gfm-tables--group-split (group)
  "Return (PROPS . ROWS) for GROUP, peeling leading plist keywords."
  (let ((tail (cdr group))
        (props nil))
    (while (keywordp (car tail))
      (push (car tail) props)
      (push (cadr tail) props)
      (setq tail (cddr tail)))
    (cons (nreverse props) tail)))

(defun gfm-tables--group-applicable-p (group)
  "Return non-nil if GROUP's `:when' predicate (if any) passes."
  (let* ((pred (plist-get (car (gfm-tables--group-split group)) :when)))
    (or (null pred) (funcall pred))))

(defun gfm-tables--key-desc (cmds)
  "Return CMDS' shortest active keys joined by `/' as a propertised string."
  (let ((parts (delq nil
                     (mapcar (lambda (c)
                               (when-let* ((seq (where-is-internal c nil t)))
                                 (key-description seq)))
                             cmds))))
    (and parts
         (propertize (mapconcat #'identity parts "/")
                     'face 'transient-key))))

(defun gfm-tables--render-hint-items (rows)
  "Flatten ROWS (list of `((CMDS . LABEL) ...)') to `\"k1 lbl  k2 lbl\"'."
  (let ((items (cl-loop for row in rows
                        append (cl-loop for (cmds . label) in row
                                        for keys = (gfm-tables--key-desc cmds)
                                        when keys
                                        collect (concat keys " " label)))))
    (mapconcat #'identity items "  ")))

(defun gfm-tables--render-hint-group (group)
  "Render GROUP as a single line: heading followed by all key/label items."
  (let ((heading (propertize (car group) 'face 'transient-heading))
        (items (gfm-tables--render-hint-items
                (cdr (gfm-tables--group-split group)))))
    (if (string-empty-p items) heading (concat heading " " items))))

(defvar-local gfm-tables--last-hinted-cell nil
  "Last (LINE-BEG . IDX) for which hints were echoed.
Indexed by row line position rather than overlay identity so a rebuild
\(which replaces row overlays) does not spuriously re-trigger the echo
and clobber another command's message (e.g. `Copied:').")

(defun gfm-tables--cell-key (info)
  "Return a stable (LINE-BEG . IDX) key for cell-info INFO, or nil."
  (and info (cons (overlay-start (car info)) (cdr info))))

(defun gfm-tables--echo-hints ()
  "Echo the transient-style table key hints without logging to `*Messages*'."
  (let* ((groups (seq-filter #'gfm-tables--group-applicable-p
                             gfm-tables--hint-groups))
         (line (mapconcat #'gfm-tables--render-hint-group groups "   "))
         (message-log-max nil))
    (message "%s" line)))

(defun gfm-tables--clear-cursor-anchor ()
  "Remove the `cursor' text property previously set by us, if any."
  (when (and gfm-tables--cursor-anchor
             (< gfm-tables--cursor-anchor (point-max)))
    (with-silent-modifications
      (remove-text-properties gfm-tables--cursor-anchor
                              (1+ gfm-tables--cursor-anchor)
                              '(cursor nil))))
  (setq gfm-tables--cursor-anchor nil))

(defun gfm-tables--set-cursor-anchor (pos)
  "Move the `cursor' text-property anchor to buffer position POS."
  (gfm-tables--clear-cursor-anchor)
  (when (and pos (< pos (point-max)))
    (with-silent-modifications
      (put-text-property pos (1+ pos) 'cursor t))
    (setq gfm-tables--cursor-anchor pos)))

(defun gfm-tables--cell-info-at-point ()
  "Return (ROW-OV . CELL-IDX) for point inside a navigable table row, else nil."
  (let* ((ov (cl-find-if (lambda (o)
                           (overlay-get o 'gfm-tables-cell-bounds))
                         (overlays-at (point))))
         (cb (and ov (overlay-get ov 'gfm-tables-cell-bounds))))
    (when cb
      (cons ov
            (or (cl-position-if (lambda (b)
                                  (and (>= (point) (car b))
                                       (< (point) (cdr b))))
                                cb)
                0)))))

(defun gfm-tables--apply-cell-highlight (display dcb idx)
  "Return a copy of DISPLAY with cell IDX painted with the active-cell face.
DCB is the row's `gfm-tables-display-cell-bounds' overlay property: a
\(N-CELLS . BOUNDS-VEC) cons where BOUNDS-VEC is a flat fixnum vector
holding (BEG END) pairs in row-major (line, cell) order.  The highlight
repeats on every visual line.  The first cell-beg on the first visual
line carries `cursor t' so the visible terminal cursor anchors there
instead of at the right edge of the overlay's display range."
  (let* ((s (copy-sequence display))
         (n (length s))
         (n-cells (car dcb))
         (vec (cdr dcb))
         (n-lines (if (zerop n-cells) 0 (/ (length vec) (* 2 n-cells))))
         (line-start 0)
         (line-idx 0)
         (first-line t))
    (while (and (< line-start n) (< line-idx n-lines) (< idx n-cells))
      (let* ((nl (or (cl-position ?\n s :start line-start) n))
             (base (* 2 (+ (* line-idx n-cells) idx)))
             (range-beg (aref vec base))
             (range-end (aref vec (1+ base)))
             (cell-beg (+ line-start range-beg))
             (cell-end (min nl (+ line-start range-end))))
        (when (< cell-beg cell-end)
          (add-face-text-property cell-beg cell-end
                                  'gfm-tables-active-cell-face nil s)
          (when first-line
            (put-text-property cell-beg (1+ cell-beg) 'cursor t s)
            (setq first-line nil)))
        (setq line-start (1+ nl))
        (setq line-idx (1+ line-idx))))
    s))

(defun gfm-tables--show-cell-highlight (row-ov idx)
  "Repaint ROW-OV's display so cell IDX shows the active-cell face.
Also anchors the visible cursor at the cell's first buffer char."
  (let ((orig (or (overlay-get row-ov 'gfm-tables-saved-display)
                  (overlay-get row-ov 'display)))
        (cb (overlay-get row-ov 'gfm-tables-cell-bounds))
        (dcb (overlay-get row-ov 'gfm-tables-display-cell-bounds)))
    (overlay-put row-ov 'gfm-tables-saved-display orig)
    (overlay-put row-ov 'display
                 (gfm-tables--apply-cell-highlight orig dcb idx))
    (setq gfm-tables--highlighted-row-ov row-ov)
    (when (and cb (< idx (length cb)))
      (gfm-tables--set-cursor-anchor (car (nth idx cb))))))

(defun gfm-tables--hide-cursor-highlight ()
  "Restore the row's original display, clear the cursor anchor, and the cursor."
  (when (and gfm-tables--highlighted-row-ov
             (overlay-buffer gfm-tables--highlighted-row-ov))
    (let* ((ov gfm-tables--highlighted-row-ov)
           (orig (overlay-get ov 'gfm-tables-saved-display)))
      (when orig
        (overlay-put ov 'display orig))
      (overlay-put ov 'gfm-tables-saved-display nil)))
  (gfm-tables--clear-cursor-anchor)
  (gfm-tables--restore-cursor)
  (setq gfm-tables--highlighted-row-ov nil
        gfm-tables--current-highlight-key nil))

(defun gfm-tables--update-cursor-highlight ()
  "Highlight the active cell at point and hide the buffer cursor.
The cursor is hidden because tty Emacs always renders it at the
overlay's right edge regardless of any `cursor' positioning hints, so
the highlight alone conveys cell selection."
  (gfm-tables--maybe-snap-to-cell)
  (let ((info (gfm-tables--cell-info-at-point)))
    (cond
     (info
      (let* ((row-ov (car info))
             (idx (cdr info))
             (key (cons row-ov idx)))
        (unless (equal key gfm-tables--current-highlight-key)
          (unless (eq row-ov gfm-tables--highlighted-row-ov)
            (gfm-tables--hide-cursor-highlight))
          (gfm-tables--show-cell-highlight row-ov idx)
          (setq gfm-tables--current-highlight-key key)))
      (gfm-tables--save-and-hide-cursor)
      (let ((cell-key (gfm-tables--cell-key info)))
        (unless (equal cell-key gfm-tables--last-hinted-cell)
          (gfm-tables--echo-hints)
          (setq gfm-tables--last-hinted-cell cell-key))))
     (t
      (when gfm-tables--current-highlight-key
        (gfm-tables--hide-cursor-highlight)
        (setq gfm-tables--current-highlight-key nil
              gfm-tables--last-hinted-cell nil))))))

;;; Cell-wise navigation

(defun gfm-tables--goto-cell (row-ov idx)
  "Move point to the start of cell IDX in ROW-OV.
IDX is clamped to the row's valid range.  Sets
`disable-point-adjustment' so Emacs does not snap point to the
end of the row-overlay's `display' range after the command."
  (let* ((cb (overlay-get row-ov 'gfm-tables-cell-bounds))
         (n (length cb))
         (clamped (max 0 (min (1- n) idx))))
    (goto-char (car (nth clamped cb)))
    (setq disable-point-adjustment t)))

(defun gfm-tables--row-on-relative-line (direction)
  "Return the gfm-tables navigable row overlay DIRECTION lines from point.
DIRECTION is 1 for the next row, -1 for the previous row.  Skips
non-navigable rows (e.g. the delimiter).  Confined to the current
table block so motion at a table edge yields nil instead of jumping
into a neighbouring table."
  (when-let* ((block (gfm-tables--current-block)))
    (let ((block-beg (nth 0 block))
          (block-end (nth 3 block)))
      (save-excursion
        (let ((found nil)
              (in-block t))
          (while (and (not found) in-block
                      (zerop (forward-line direction)))
            (let ((lbeg (line-beginning-position)))
              (cond
               ((or (< lbeg block-beg) (> lbeg block-end))
                (setq in-block nil))
               (t
                (let ((ov (cl-find-if
                           (lambda (o)
                             (overlay-get o 'gfm-tables-cell-bounds))
                           (overlays-at lbeg))))
                  (when ov (setq found ov)))))))
          found)))))

(defun gfm-tables-cell-forward ()
  "Move to the next cell in the current table row.
Returns non-nil on success, nil when point is already on the last cell."
  (interactive)
  (let* ((info (gfm-tables--cell-info-at-point))
         (cb (and info (overlay-get (car info) 'gfm-tables-cell-bounds)))
         (next-idx (and info (1+ (cdr info)))))
    (when (and info (< next-idx (length cb)))
      (gfm-tables--goto-cell (car info) next-idx)
      t)))

(defun gfm-tables-cell-backward ()
  "Move to the previous cell in the current table row.
Returns non-nil on success, nil when point is already on the first cell."
  (interactive)
  (let* ((info (gfm-tables--cell-info-at-point))
         (prev-idx (and info (1- (cdr info)))))
    (when (and info (>= prev-idx 0))
      (gfm-tables--goto-cell (car info) prev-idx)
      t)))

(defun gfm-tables-row-down ()
  "Move to the same cell in the next table row.
Returns non-nil on success, nil when there is no next table row."
  (interactive)
  (let* ((info (gfm-tables--cell-info-at-point))
         (next (and info (gfm-tables--row-on-relative-line 1))))
    (when next
      (gfm-tables--goto-cell next (cdr info))
      t)))

(defun gfm-tables-row-up ()
  "Move to the same cell in the previous table row.
Returns non-nil on success, nil when there is no previous table row."
  (interactive)
  (let* ((info (gfm-tables--cell-info-at-point))
         (prev (and info (gfm-tables--row-on-relative-line -1))))
    (when prev
      (gfm-tables--goto-cell prev (cdr info))
      t)))

(defun gfm-tables-row-first-cell ()
  "Move to the first cell in the current table row."
  (interactive)
  (let ((info (gfm-tables--cell-info-at-point)))
    (when info (gfm-tables--goto-cell (car info) 0))))

(defun gfm-tables-row-last-cell ()
  "Move to the last cell in the current table row."
  (interactive)
  (let ((info (gfm-tables--cell-info-at-point)))
    (when info
      (gfm-tables--goto-cell (car info) most-positive-fixnum))))

(defun gfm-tables-cell-copy ()
  "Copy the active table cell's content to the kill ring."
  (interactive)
  (let ((info (gfm-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((bounds (gfm-tables--cell-content-bounds (car info) (cdr info)))
           (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (kill-new text)
      (message "Copied: %s"
               (truncate-string-to-width text 60 nil nil t)))))

(defun gfm-tables-do-at-cell ()
  "Dispatch `markdown-do' on the active cell's contents.
If the cell contains a link, point is moved onto it first so
`markdown-do' follows the link instead of acting at the row's
left edge."
  (interactive)
  (let ((info (gfm-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((row-ov (car info))
           (idx (cdr info))
           (link-pos (gfm-tables--cell-link-pos row-ov idx))
           (bounds (gfm-tables--cell-content-bounds row-ov idx)))
      (goto-char (or link-pos (car bounds)))
      (call-interactively #'markdown-do))))

(defun gfm-tables--insert-new-row-after-current ()
  "Insert an empty row after the current one and put point in cell 0."
  (let* ((info (gfm-tables--cell-info-at-point))
         (row-ov (car info))
         (n (length (overlay-get row-ov 'gfm-tables-cell-bounds))))
    (goto-char (line-end-position))
    (insert "\n|"
            (apply #'concat (make-list n "  |")))
    (gfm-tables--rebuild)
    (let ((new-row (cl-find-if
                    (lambda (o) (overlay-get o 'gfm-tables-cell-bounds))
                    (overlays-at (line-beginning-position)))))
      (when new-row
        (gfm-tables--goto-cell new-row 0)))))

(defun gfm-tables-cell-tab ()
  "Move to the next cell.
At end of a row, wrap to the next row's first cell.  At end of the
last row, insert a fresh empty row and land in its first cell."
  (interactive)
  (cond
   ((gfm-tables-cell-forward) t)
   ((let ((next (gfm-tables--row-on-relative-line 1)))
      (when next (gfm-tables--goto-cell next 0) t)))
   (t (gfm-tables--insert-new-row-after-current))))

(defun gfm-tables-cell-backtab ()
  "Move to the previous cell, wrapping to the previous row's last cell at row start."
  (interactive)
  (unless (gfm-tables-cell-backward)
    (let ((prev-row (gfm-tables--row-on-relative-line -1)))
      (when prev-row
        (gfm-tables--goto-cell prev-row most-positive-fixnum)))))

;;; Header column reordering

(defun gfm-tables--current-block ()
  "Return the table block (HEADER-BEG DELIM-BEG BODY-BEG BODY-END) at point."
  (let ((pt (point)))
    (cl-loop for block in (gfm-tables--find-blocks)
             when (and (>= pt (nth 0 block)) (<= pt (nth 3 block)))
             return block)))

(defun gfm-tables--in-header-p ()
  "Non-nil if point is on the header row of a table."
  (let ((block (gfm-tables--current-block)))
    (and block (= (line-beginning-position) (nth 0 block)))))

(defun gfm-tables--swap-cells-in-line (line-pos a b)
  "On the line containing LINE-POS, swap cell A with cell B by index.
A and B are zero-based.  No-op if either index is out of range."
  (save-excursion
    (goto-char line-pos)
    (let* ((lb (line-beginning-position))
           (le (line-end-position))
           (cb (gfm-tables--cell-bounds lb le))
           (n (length cb)))
      (when (and (< a n) (< b n) (/= a b))
        (let* ((lo (min a b))
               (hi (max a b))
               (lo-beg (car (nth lo cb)))
               (lo-end (cdr (nth lo cb)))
               (hi-beg (car (nth hi cb)))
               (hi-end (cdr (nth hi cb)))
               (lo-text (buffer-substring-no-properties lo-beg lo-end))
               (hi-text (buffer-substring-no-properties hi-beg hi-end)))
          ;; Replace high range first so low positions stay valid.
          (delete-region hi-beg hi-end)
          (goto-char hi-beg)
          (insert lo-text)
          (delete-region lo-beg lo-end)
          (goto-char lo-beg)
          (insert hi-text))))))

(defun gfm-tables--swap-columns (direction)
  "Swap the active cell's column with its DIRECTION (-1 or 1) neighbour.
Silently no-op when point is not on a header row, or when the
neighbour column is out of range."
  (let* ((info (and (gfm-tables--in-header-p)
                    (gfm-tables--cell-info-at-point)))
         (block (and info (gfm-tables--current-block)))
         (src-idx (and info (cdr info)))
         (dst-idx (and info (+ src-idx direction)))
         (n-cols (and info
                      (length (overlay-get (car info)
                                           'gfm-tables-cell-bounds)))))
    (when (and info (>= dst-idx 0) (< dst-idx n-cols))
      (let ((header-beg (nth 0 block))
            (delim-beg (nth 1 block))
            (body-beg  (nth 2 block))
            (body-end  (nth 3 block)))
        (save-excursion
          (gfm-tables--swap-cells-in-line header-beg src-idx dst-idx)
          (gfm-tables--swap-cells-in-line delim-beg src-idx dst-idx)
          (goto-char body-beg)
          (while (and (< (point) body-end) (looking-at-p "^|"))
            (gfm-tables--swap-cells-in-line (point) src-idx dst-idx)
            (forward-line 1))))
      (gfm-tables--rebuild)
      (let ((row-ov (cl-find-if (lambda (o)
                                  (overlay-get o 'gfm-tables-cell-bounds))
                                (overlays-at (line-beginning-position)))))
        (when row-ov
          (gfm-tables--goto-cell row-ov dst-idx))))))

(defun gfm-tables-swap-column-left ()
  "Swap the active header column with the column to its left."
  (interactive)
  (gfm-tables--swap-columns -1))

(defun gfm-tables-swap-column-right ()
  "Swap the active header column with the column to its right."
  (interactive)
  (gfm-tables--swap-columns 1))

(defun gfm-tables--maybe-snap-to-cell ()
  "If point is on a table row but outside any cell range, snap to cell 0.
Idempotent when point already sits in a cell so it is safe to call
unconditionally (e.g. from `post-command-hook').  Skipped when the
row is invisible (e.g. inside a folded outline heading) so motion
through hidden text does not pull point into a hidden table."
  (let* ((lbeg (line-beginning-position))
         (row-ov (cl-find-if
                  (lambda (o) (overlay-get o 'gfm-tables-cell-bounds))
                  (overlays-at lbeg))))
    (when (and row-ov (not (invisible-p lbeg)))
      (let* ((cb (overlay-get row-ov 'gfm-tables-cell-bounds))
             (in-cell (cl-find-if
                       (lambda (b) (and (>= (point) (car b))
                                        (< (point) (cdr b))))
                       cb)))
        (unless in-cell
          (gfm-tables--goto-cell row-ov 0))))))

(defmacro gfm-tables--define-evil-motion (name table-fn evil-fn fall-through)
  "Define NAME as a wrapper dispatching between TABLE-FN and EVIL-FN.
When point is in a table, TABLE-FN runs first.  If it returns nil
\(no-op at an edge) and FALL-THROUGH is non-nil, EVIL-FN runs as a
fallback so motion can escape the table; otherwise the no-op is
silent.  Outside a table, EVIL-FN runs directly; if FALL-THROUGH is
the symbol `snap', a post-call landing in a table snaps to cell 0."
  `(defun ,name ()
     ,(format "Cell-aware shim for `%s'." evil-fn)
     (interactive)
     (cond
      ((gfm-tables--cell-info-at-point)
       (or (,table-fn)
           ,(when fall-through
              `(progn (call-interactively #',evil-fn)
                      ,(when (eq fall-through 'snap)
                         '(gfm-tables--maybe-snap-to-cell))))))
      (t (call-interactively #',evil-fn)
         ,(when (eq fall-through 'snap)
            '(gfm-tables--maybe-snap-to-cell))))))

(gfm-tables--define-evil-motion gfm-tables--evil-h gfm-tables-cell-backward  evil-backward-char           nil)
(gfm-tables--define-evil-motion gfm-tables--evil-l gfm-tables-cell-forward   evil-forward-char            nil)
(gfm-tables--define-evil-motion gfm-tables--evil-j gfm-tables-row-down       evil-next-line               snap)
(gfm-tables--define-evil-motion gfm-tables--evil-k gfm-tables-row-up         evil-previous-line           snap)
(gfm-tables--define-evil-motion gfm-tables--evil-^ gfm-tables-row-first-cell evil-first-non-blank         nil)
(gfm-tables--define-evil-motion gfm-tables--evil-$ gfm-tables-row-last-cell  evil-end-of-line             nil)
(gfm-tables--define-evil-motion gfm-tables--evil-w gfm-tables-cell-forward   evil-forward-word-begin      nil)
(gfm-tables--define-evil-motion gfm-tables--evil-W gfm-tables-cell-forward   evil-forward-WORD-begin      nil)
(gfm-tables--define-evil-motion gfm-tables--evil-e gfm-tables-cell-forward   evil-forward-word-end        nil)
(gfm-tables--define-evil-motion gfm-tables--evil-E gfm-tables-cell-forward   evil-forward-WORD-end        nil)
(gfm-tables--define-evil-motion gfm-tables--evil-b gfm-tables-cell-backward  evil-backward-word-begin     nil)
(gfm-tables--define-evil-motion gfm-tables--evil-B gfm-tables-cell-backward  evil-backward-WORD-begin     nil)
(gfm-tables--define-evil-motion gfm-tables--evil-0 gfm-tables-row-first-cell evil-beginning-of-line       nil)

(defun gfm-tables--evil-Y ()
  "Cell-aware shim for `evil-yank-line': copy the active cell when in a table."
  (interactive)
  (cond
   ((gfm-tables--cell-info-at-point) (gfm-tables-cell-copy))
   (t (call-interactively #'evil-yank-line))))

(defvar gfm-tables--row-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB")          #'gfm-tables-cell-tab)
    (define-key m (kbd "<tab>")        #'gfm-tables-cell-tab)
    (define-key m (kbd "<backtab>")    #'gfm-tables-cell-backtab)
    (define-key m (kbd "S-<tab>")      #'gfm-tables-cell-backtab)
    (define-key m (kbd "RET")          #'gfm-tables-do-at-cell)
    (define-key m (kbd "<return>")     #'gfm-tables-do-at-cell)
    (define-key m (kbd "M-h")          #'gfm-tables-swap-column-left)
    (define-key m (kbd "M-l")          #'gfm-tables-swap-column-right)
    (define-key m [remap evil-backward-char]       #'gfm-tables--evil-h)
    (define-key m [remap evil-forward-char]        #'gfm-tables--evil-l)
    (define-key m [remap evil-next-line]           #'gfm-tables--evil-j)
    (define-key m [remap evil-previous-line]       #'gfm-tables--evil-k)
    (define-key m [remap evil-first-non-blank]     #'gfm-tables--evil-^)
    (define-key m [remap evil-end-of-line]         #'gfm-tables--evil-$)
    (define-key m [remap evil-forward-word-begin]  #'gfm-tables--evil-w)
    (define-key m [remap evil-forward-WORD-begin]  #'gfm-tables--evil-W)
    (define-key m [remap evil-forward-word-end]    #'gfm-tables-edit-cell-at-point)
    (define-key m [remap evil-forward-WORD-end]    #'gfm-tables-edit-cell-at-point)
    (define-key m [remap evil-backward-word-begin] #'gfm-tables--evil-b)
    (define-key m [remap evil-backward-WORD-begin] #'gfm-tables--evil-B)
    (define-key m [remap evil-beginning-of-line]   #'gfm-tables--evil-0)
    (define-key m [remap evil-yank-line]           #'gfm-tables--evil-Y)
    m)
  "Keymap attached to row overlays; bindings only fire inside a table.")

;;; Evil insert/replace shim

(defconst gfm-tables--evil-edit-commands
  '(evil-insert evil-append evil-insert-line evil-append-line
                evil-substitute evil-change-whole-line
                evil-open-below evil-open-above
                evil-change evil-change-line
                evil-replace evil-replace-state)
  "Evil commands diverted to `gfm-tables-edit-table-at-point' in tables.")

(defun gfm-tables--maybe-edit-advice (orig &rest args)
  "Around-advice: divert ORIG (called with ARGS) to the table editor in tables."
  (if (and (bound-and-true-p gfm-tables-mode)
           (gfm-tables--block-at-point))
      (gfm-tables-edit-table-at-point)
    (apply orig args)))

(with-eval-after-load 'evil
  (dolist (cmd gfm-tables--evil-edit-commands)
    (when (fboundp cmd)
      (advice-add cmd :around #'gfm-tables--maybe-edit-advice))))

(provide '+gfm-tables)

;;; +gfm-tables.el ends here
