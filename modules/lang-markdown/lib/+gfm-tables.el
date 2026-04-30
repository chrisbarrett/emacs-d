;;; +gfm-tables.el --- Visual treatment for GFM tables -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that adorns GitHub Flavored Markdown tables with bordered,
;; zebra-striped grids:
;;
;;   ┌──────────────────┐
;;   │ Header A │ Header B │
;;   ├──────────────────┤
;;   │ a1       │ b1       │
;;   │ a2       │ b2       │
;;   └──────────────────┘
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

(defconst gfm-tables--border-face 'parenthesis
  "Face used for table border characters.")

(defvar gfm-tables-mode-map (make-sparse-keymap)
  "Keymap for `gfm-tables-mode'.
Cell-wise navigation keys are bound under `with-eval-after-load' for
evil so the file remains usable without evil installed.")

(defconst gfm-tables--delim-re
  "^| *:?-+:? *\\(?:| *:?-+:? *\\)*|[[:blank:]]*$"
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

(defun gfm-tables--visible-width (s)
  "Return on-screen width of S in the current buffer.
Honours `display' string replacements and any `invisible' property
that is currently hidden by `buffer-invisibility-spec'."
  (let ((spec buffer-invisibility-spec)
        (w 0) (i 0) (n (length s)))
    (while (< i n)
      (let* ((nd (or (next-single-property-change i 'display s) n))
             (ni (or (next-single-property-change i 'invisible s) n))
             (next (min nd ni))
             (invis (get-text-property i 'invisible s))
             (disp (get-text-property i 'display s)))
        (cond
         ((gfm-tables--invisible-p invis spec) nil)
         ((stringp disp) (cl-incf w (string-width disp)))
         (t (cl-incf w (string-width (substring-no-properties s i next)))))
        (setq i next)))
    w))

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

(defun gfm-tables--find-blocks (&optional excluded-ranges)
  "Return all GFM tables in the buffer.
Each entry is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END).  Tables whose
delimiter row falls inside any (BEG . END) range in EXCLUDED-RANGES are
skipped."
  (let (blocks)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward gfm-tables--delim-re nil t)
          (let* ((delim-beg (match-beginning 0))
                 (delim-end (match-end 0)))
            (cond
             ((gfm-tables--in-ranges-p delim-beg excluded-ranges)
              (goto-char delim-end))
             (t
              (let ((header-beg
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
                    (when body-end
                      (push (list header-beg delim-beg body-beg body-end)
                            blocks)
                      (goto-char body-end))
                    (unless body-end
                      (goto-char delim-end)))))))))))) ; whew
    (nreverse blocks)))

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
         (lhs (cl-case role
                (header (propertize "│" 'face border-face))
                (body-alt (propertize "▐" 'face 'gfm-tables-row-alt-cap-face))
                (t " ")))
         (rhs (cl-case role
                (header (propertize "│" 'face border-face))
                (body-alt (propertize "▌" 'face 'gfm-tables-row-alt-cap-face))
                (t " "))))
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

(defun gfm-tables--compose-multiline-row (cells col-widths role)
  "Compose ROLE row, wrapping each cell to its column width.
Returns a string in which visual lines are joined by newlines.  All
cells in the row are padded to the tallest cell's height with empty
strings, so the column grid stays aligned across visual lines."
  (let* ((widths-list (cl-coerce col-widths 'list))
         (wrapped (cl-mapcar #'gfm-tables--wrap-cell cells widths-list))
         (height (apply #'max 1 (mapcar #'length wrapped)))
         (padded (mapcar (lambda (lines)
                           (append lines
                                   (make-list (- height (length lines)) "")))
                         wrapped)))
    (mapconcat
     (lambda (idx)
       (gfm-tables--compose-row
        (mapcar (lambda (cell-lines) (nth idx cell-lines)) padded)
        col-widths role))
     (number-sequence 0 (1- height))
     "\n")))

(defun gfm-tables--rule-row (box-width)
  "Return a `└─…─┘' string closing the header row, total width BOX-WIDTH.
Body rows below have no left/right borders, so the rule row caps the
header box at its bottom corners."
  (let ((face gfm-tables--border-face))
    (concat (propertize "└" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┘" 'face face))))

(defun gfm-tables--top-border (box-width)
  "Return a `┌─…─┐' top border of total width BOX-WIDTH."
  (let ((face gfm-tables--border-face))
    (concat (propertize "┌" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┐" 'face face))))

(defun gfm-tables--bottom-border (box-width)
  "Return a horizontal rule of total width BOX-WIDTH.
The leftmost and rightmost cells use half-width box-drawing chars
`╶' and `╴' so the rule begins/ends at the cell's midpoint rather
than its edge, aligning with the body row's leading/trailing pad."
  (let ((face gfm-tables--border-face))
    (concat (propertize "╶" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "╴" 'face face))))

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
  (unless (or beg end)
    (setq gfm-tables--overlays nil)))

(defun gfm-tables--apply-table (header-beg delim-beg body-beg body-end)
  "Decorate one table identified by HEADER-BEG, DELIM-BEG, BODY-BEG, BODY-END."
  (save-excursion
    (let* ((header-end (save-excursion
                         (goto-char header-beg) (line-end-position)))
           (delim-end (save-excursion
                        (goto-char delim-beg) (line-end-position)))
           (header-line (buffer-substring-no-properties
                         header-beg header-end))
           (header-cells (mapcar #'gfm-tables--fontify-cell
                                 (gfm-tables--split-row header-line)))
           (body-rows '())
           (body-positions '()))
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
            body-positions (nreverse body-positions))
      (let* ((all-rows (cons header-cells body-rows))
             (natural (gfm-tables--column-widths all-rows))
             (n-cols (length natural))
             (overhead (+ (* 3 n-cols) 1))
             (budget (max n-cols
                          (- (gfm-tables--available-width) overhead)))
             (col-widths (gfm-tables--fit-widths natural budget))
             (box-width (gfm-tables--box-width col-widths))
             (top (gfm-tables--top-border box-width))
             (bottom (gfm-tables--bottom-border box-width))
             (rule (gfm-tables--rule-row box-width))
             (n-body (length body-rows)))
        ;; Header overlay carries top border as before-string.
        (let ((ov (make-overlay header-beg header-end))
              (str (gfm-tables--compose-multiline-row
                    header-cells col-widths 'header)))
          (overlay-put ov 'display str)
          (overlay-put ov 'before-string (concat top "\n"))
          (overlay-put ov 'gfm-tables-cell-bounds
                       (gfm-tables--cell-bounds header-beg header-end))
          (overlay-put ov 'gfm-tables-col-widths col-widths)
          (gfm-tables--register ov))
        ;; Delimiter row → continuous rule.
        (let ((ov (make-overlay delim-beg delim-end)))
          (overlay-put ov 'display rule)
          (gfm-tables--register ov))
        ;; Body rows.
        (cl-loop for (lbeg . lend) in body-positions
                 for cells in body-rows
                 for idx from 1
                 for last-p = (= idx n-body)
                 for role = (if (cl-evenp idx) 'body-alt 'body-default)
                 for str = (gfm-tables--compose-multiline-row
                            cells col-widths role)
                 do (let ((ov (make-overlay lbeg lend)))
                      (overlay-put ov 'display str)
                      (overlay-put ov 'gfm-tables-cell-bounds
                                   (gfm-tables--cell-bounds lbeg lend))
                      (overlay-put ov 'gfm-tables-col-widths col-widths)
                      (when last-p
                        (overlay-put ov 'after-string
                                     (concat "\n" bottom)))
                      (gfm-tables--register ov)))))))

(defun gfm-tables--fenced-ranges ()
  "Return (BEG . END) ranges of fenced code blocks, if discoverable."
  (and (fboundp 'gfm-code-fences--find-blocks)
       (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b)))
               (gfm-code-fences--find-blocks))))

(defun gfm-tables--apply-overlays ()
  "Apply table overlays to all GFM tables in the buffer."
  (let ((excluded (gfm-tables--fenced-ranges))
        (n 0))
    (dolist (block (gfm-tables--find-blocks excluded))
      (cl-destructuring-bind (h d bb be) block
        (gfm-tables--apply-table h d bb be)
        (cl-incf n)))
    n))

;;; Performance instrumentation

(defvar-local gfm-tables--stats nil
  "Per-buffer alist of rebuild stats.")

(defun gfm-tables--init-stats ()
  "Reset the per-buffer rebuild stats to zero."
  (setq gfm-tables--stats
        (list (cons 'rebuild-count 0)
              (cons 'total-time 0.0)
              (cons 'last-time 0.0)
              (cons 'max-time 0.0)
              (cons 'table-count 0))))

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

(defun gfm-tables-stats ()
  "Display the current buffer's gfm-tables rebuild statistics."
  (interactive)
  (if (not gfm-tables--stats)
      (message "gfm-tables: no stats yet")
    (let-alist gfm-tables--stats
      (message
       "gfm-tables [%s]: rebuilds=%d total=%.3fs last=%.3fs max=%.3fs tables=%d"
       (buffer-name)
       .rebuild-count .total-time .last-time .max-time .table-count))))

;;; Rebuild scheduler

(defun gfm-tables--rebuild ()
  "Remove and recreate all gfm-tables overlays."
  (let ((start (current-time)))
    (gfm-tables--remove-overlays)
    (let ((n (gfm-tables--apply-overlays)))
      (gfm-tables--record-stats (float-time (time-since start)) n))))

(defvar-local gfm-tables--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defvar gfm-tables-mode)

(defun gfm-tables--schedule-rebuild (&rest _)
  "Schedule a debounced overlay rebuild.
Skips indirect buffers since base buffer overlays already cover them."
  (unless (buffer-base-buffer)
    (when (timerp gfm-tables--rebuild-timer)
      (cancel-timer gfm-tables--rebuild-timer))
    (setq gfm-tables--rebuild-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-tables-mode
                   (gfm-tables--rebuild)))))
           (current-buffer)))))

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
                  #'gfm-tables--schedule-rebuild nil t)
        (add-hook 'post-command-hook
                  #'gfm-tables--update-cursor-highlight nil t)
        (when (fboundp 'evil-normalize-keymaps)
          (evil-normalize-keymaps)))
    (remove-hook 'after-change-functions
                 #'gfm-tables--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-tables--schedule-rebuild t)
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

;;;###autoload
(defun gfm-tables-edit-table-at-point ()
  "Open the GFM table containing point in an indirect edit buffer.
The edit buffer uses `markdown-mode' with `orgtbl-mode' enabled, so
TAB navigates cells, content auto-aligns on edit, and column
operations (M-<right>/<left>) work as in `org-mode'.  Point in the
edit buffer is placed at the same source offset as in the parent."
  (interactive)
  (require 'edit-indirect)
  (let ((bounds (gfm-tables--block-at-point))
        (src-pt (point)))
    (unless bounds
      (user-error "Point is not inside a GFM table"))
    (let* ((edit-indirect-guess-mode-function
            (lambda (_parent _beg _end) (markdown-mode)))
           (buf (edit-indirect-region (car bounds) (cdr bounds) t))
           (offset (- src-pt (car bounds))))
      (with-current-buffer buf
        (when (fboundp 'orgtbl-mode)
          (orgtbl-mode 1))
        (goto-char (max (point-min)
                        (min (point-max) (+ (point-min) offset))))))))

;;; Active-cell highlight

(defvar-local gfm-tables--highlighted-row-ov nil
  "The row overlay whose display currently carries the cell highlight.")

(defvar-local gfm-tables--cursor-anchor nil
  "Buffer position currently flagged with the `cursor' text property.
Used to anchor the visible terminal cursor at the active cell's first
char rather than at the right edge of the overlay's display range.")

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

(defun gfm-tables--cell-display-range (col-widths idx)
  "Return (BEG . END) char offsets for cell IDX within one composed line."
  (let* ((widths (cl-coerce col-widths 'list))
         (before (cl-loop for i from 0 below idx
                          sum (+ (nth i widths) 2 1)))
         (start (+ 1 before))
         (end (+ start (nth idx widths) 2)))
    (cons start end)))

(defun gfm-tables--apply-cell-highlight (display col-widths idx)
  "Return a copy of DISPLAY with cell IDX painted with the active-cell face.
DISPLAY may contain `\\n's for wrapped rows; the highlight repeats on
every visual line.  The first cell-beg position on the first visual
line carries `cursor t' so the visible terminal cursor anchors there
instead of at the right edge of the overlay's display range."
  (let* ((s (copy-sequence display))
         (n (length s))
         (range (gfm-tables--cell-display-range col-widths idx))
         (line-start 0)
         (first-line t))
    (while (< line-start n)
      (let* ((nl (or (cl-position ?\n s :start line-start) n))
             (cell-beg (+ line-start (car range)))
             (cell-end (min nl (+ line-start (cdr range)))))
        (when (< cell-beg cell-end)
          (add-face-text-property cell-beg cell-end
                                  'gfm-tables-active-cell-face nil s)
          (when first-line
            (put-text-property cell-beg (1+ cell-beg) 'cursor t s)
            (setq first-line nil)))
        (setq line-start (1+ nl))))
    s))

(defun gfm-tables--show-cell-highlight (row-ov idx)
  "Repaint ROW-OV's display so cell IDX shows the active-cell face.
Also anchors the visible cursor at the cell's first buffer char."
  (let ((orig (or (overlay-get row-ov 'gfm-tables-saved-display)
                  (overlay-get row-ov 'display)))
        (cb (overlay-get row-ov 'gfm-tables-cell-bounds)))
    (overlay-put row-ov 'gfm-tables-saved-display orig)
    (overlay-put row-ov 'display
                 (gfm-tables--apply-cell-highlight
                  orig (overlay-get row-ov 'gfm-tables-col-widths) idx))
    (setq gfm-tables--highlighted-row-ov row-ov)
    (when (and cb (< idx (length cb)))
      (gfm-tables--set-cursor-anchor (car (nth idx cb))))))

(defun gfm-tables--hide-cursor-highlight ()
  "Restore the row's original display and clear the cursor anchor."
  (when (and gfm-tables--highlighted-row-ov
             (overlay-buffer gfm-tables--highlighted-row-ov))
    (let* ((ov gfm-tables--highlighted-row-ov)
           (orig (overlay-get ov 'gfm-tables-saved-display)))
      (when orig
        (overlay-put ov 'display orig))
      (overlay-put ov 'gfm-tables-saved-display nil)))
  (gfm-tables--clear-cursor-anchor)
  (setq gfm-tables--highlighted-row-ov nil))

(defun gfm-tables--update-cursor-highlight ()
  "Highlight the active cell at point.
The visible cursor is anchored at the cell's first display char via
the `cursor' text property on the display string."
  (let ((info (gfm-tables--cell-info-at-point)))
    (cond
     (info
      (let ((row-ov (car info))
            (idx (cdr info)))
        (unless (eq row-ov gfm-tables--highlighted-row-ov)
          (gfm-tables--hide-cursor-highlight))
        (gfm-tables--show-cell-highlight row-ov idx)))
     (t
      (gfm-tables--hide-cursor-highlight)))))

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
non-navigable rows (e.g. the delimiter)."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (zerop (forward-line direction)))
        (let ((ov (cl-find-if
                   (lambda (o) (overlay-get o 'gfm-tables-cell-bounds))
                   (overlays-at (line-beginning-position)))))
          (when ov (setq found ov))))
      found)))

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

(defun gfm-tables--maybe-snap-to-cell ()
  "If point landed inside a table after a non-table motion, snap to cell 0."
  (let ((info (gfm-tables--cell-info-at-point)))
    (when info (gfm-tables--goto-cell (car info) 0))))

(defmacro gfm-tables--define-evil-motion (name table-fn evil-fn fall-through)
  "Define NAME as a wrapper dispatching between TABLE-FN and EVIL-FN.
When point is in a table, TABLE-FN runs first.  If it returns nil
(no-op at an edge) and FALL-THROUGH is non-nil, EVIL-FN runs as a
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

(gfm-tables--define-evil-motion gfm-tables--evil-h gfm-tables-cell-backward  evil-backward-char  nil)
(gfm-tables--define-evil-motion gfm-tables--evil-l gfm-tables-cell-forward   evil-forward-char   nil)
(gfm-tables--define-evil-motion gfm-tables--evil-j gfm-tables-row-down       evil-next-line      snap)
(gfm-tables--define-evil-motion gfm-tables--evil-k gfm-tables-row-up         evil-previous-line  snap)
(gfm-tables--define-evil-motion gfm-tables--evil-^ gfm-tables-row-first-cell evil-first-non-blank nil)
(gfm-tables--define-evil-motion gfm-tables--evil-$ gfm-tables-row-last-cell  evil-end-of-line     nil)

(define-key gfm-tables-mode-map [remap evil-backward-char] #'gfm-tables--evil-h)
(define-key gfm-tables-mode-map [remap evil-forward-char] #'gfm-tables--evil-l)
(define-key gfm-tables-mode-map [remap evil-next-line] #'gfm-tables--evil-j)
(define-key gfm-tables-mode-map [remap evil-previous-line] #'gfm-tables--evil-k)
(define-key gfm-tables-mode-map [remap evil-first-non-blank] #'gfm-tables--evil-^)
(define-key gfm-tables-mode-map [remap evil-end-of-line] #'gfm-tables--evil-$)

;;; Evil insert/replace shim

(defconst gfm-tables--evil-edit-commands
  '(evil-insert evil-append evil-insert-line evil-append-line
                evil-substitute evil-change-whole-line
                evil-open-below evil-open-above
                evil-change evil-change-line
                evil-replace evil-replace-state)
  "Evil commands diverted to `gfm-tables-edit-table-at-point' in tables.")

(defun gfm-tables--maybe-edit-advice (orig &rest args)
  "Around-advice: divert ORIG to the table editor when point is in a table."
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
