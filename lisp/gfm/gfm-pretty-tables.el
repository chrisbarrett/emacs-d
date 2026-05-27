;;; gfm-pretty-tables.el --- Visual treatment for GFM tables -*- lexical-binding: t; -*-

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
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)
(require 'gfm-pretty-fences nil t)
(require 'markdown-mode nil t)

(defvar gfm-pretty-tables--row-map)

;; Forward declarations: the cell-aware motion shims funcall evil
;; commands only when evil is loaded (their wrappers exist for the
;; row keymap's [remap ...] bindings).  Declared here so the
;; --affected pre-commit byte-compile lane resolves the symbols
;; without requiring evil at compile time.
(declare-function evil-backward-char       "evil-commands")
(declare-function evil-forward-char        "evil-commands")
(declare-function evil-next-line           "evil-commands")
(declare-function evil-previous-line       "evil-commands")
(declare-function evil-first-non-blank     "evil-commands")
(declare-function evil-end-of-line         "evil-commands")
(declare-function evil-forward-word-begin  "evil-commands")
(declare-function evil-forward-WORD-begin  "evil-commands")
(declare-function evil-forward-word-end    "evil-commands")
(declare-function evil-forward-WORD-end    "evil-commands")
(declare-function evil-backward-word-begin "evil-commands")
(declare-function evil-backward-WORD-begin "evil-commands")
(declare-function evil-beginning-of-line   "evil-commands")
(declare-function evil-yank-line           "evil-commands")

(defgroup gfm-pretty-tables nil
  "Visual treatment for GitHub Flavored Markdown tables."
  :group 'markdown-faces)

(defface gfm-pretty-tables-active-cell-face
  '((((background light)) :background "#9ec5ff" :extend t)
    (((background dark))  :background "#2e4a7a" :extend t))
  "Blue background face applied to the table cell at point.
The colour is intentionally muted so theme-set foregrounds (markdown
faces inside the cell) still read clearly."
  :group 'gfm-pretty-tables)

(defface gfm-pretty-tables-row-alt-face
  '((((background light)) :background "#f6f1e6")
    (((background dark))  :background "#262637"))
  "Stripe colour for alternating GFM table body rows."
  :group 'gfm-pretty-tables)

(defface gfm-pretty-tables-row-alt-cap-face
  '((((background light)) :foreground "#f6f1e6")
    (((background dark))  :foreground "#262637"))
  "Foreground for alt-row half-block caps.
Mirrors the background of `gfm-pretty-tables-row-alt-face' so that
`▐' / `▌' chars at row ends paint a half-cell of alt-bg, extending
the row's background to the header box's vertical edges."
  :group 'gfm-pretty-tables)

(defconst gfm-pretty-tables--border-face 'gfm-pretty-border-face
  "Face used for table border characters.")

(defconst gfm-pretty-tables--delim-re
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

(defconst gfm-pretty-tables--fontify-buffer-name " *gfm-pretty-tables-fontify*")

(defun gfm-pretty-tables--fontify-buffer ()
  "Return a reusable hidden buffer in `markdown-mode' for cell fontification."
  (let ((buf (get-buffer gfm-pretty-tables--fontify-buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create gfm-pretty-tables--fontify-buffer-name))
      (with-current-buffer buf
        (let ((inhibit-message t))
          (delay-mode-hooks
            (when (fboundp 'markdown-mode)
              (markdown-mode))))))
    buf))

(defun gfm-pretty-tables--transcribe-source-overlays (str buffer beg end)
  "Return STR with width-affecting overlay decoration from BUFFER baked in.
STR is the fontified, trimmed cell text; BUFFER's region [BEG, END) is
the raw source between the cell pipes (including padding).  The
region's leading and trailing whitespace is skipped so STR offset 0
maps to the first content character.  Overlays carrying a `display'
string — for instance the link overlays created by the links decorator —
have that string spliced into STR in place of the covered text, and
overlays hidden via `invisible' splice to the empty string.  Splicing
\(rather than copying the property) is required because a `display'
text property nested inside an overlay's `display' string is not
honoured by redisplay.  Source positions map 1:1 onto STR offsets,
which holds for any cell without backslash-escaped pipes.  Returns STR
unchanged when no width-affecting overlay intersects the region."
  (with-current-buffer buffer
    (while (and (< beg end) (memq (char-after beg) '(?\s ?\t)))
      (setq beg (1+ beg)))
    (while (and (> end beg) (memq (char-after (1- end)) '(?\s ?\t)))
      (setq end (1- end)))
    (let ((slen (length str))
          (edits nil))
      (dolist (ov (overlays-in beg end))
        (unless (overlay-get ov 'gfm-pretty-tables-display)
          (let ((s (- (max beg (overlay-start ov)) beg))
                (e (- (min end (overlay-end ov)) beg))
                (disp (overlay-get ov 'display)))
            (when (and (<= 0 s) (< s e) (<= e slen))
              (cond
               ((stringp disp) (cl-pushnew (list s e disp) edits :test #'equal))
               ((overlay-get ov 'invisible)
                (cl-pushnew (list s e "") edits :test #'equal)))))))
      (if (null edits)
          str
        ;; Apply right-to-left so earlier offsets stay valid.  Per-window
        ;; overlays yield identical (S E DISP) triples — `cl-pushnew'
        ;; collapses those — and `floor' skips any genuine overlap.
        (setq edits (sort edits (lambda (a b) (> (car a) (car b)))))
        (let ((result (copy-sequence str))
              (floor slen))
          (dolist (edit edits)
            (when (<= (nth 1 edit) floor)
              (setq result (concat (substring result 0 (nth 0 edit))
                                   (nth 2 edit)
                                   (substring result (nth 1 edit)))
                    floor (nth 0 edit))))
          result)))))

(defun gfm-pretty-tables--fontify-cell (cell &optional buffer beg end)
  "Return CELL with markdown inline syntax fontified.
The visible width of the result equals the visible width of CELL,
provided `markdown-hide-markup' is nil (the default).

With BUFFER, BEG and END all non-nil, CELL is the trimmed text of the
source-buffer region [BEG, END).  Width-affecting overlay decoration on
that region is baked into the returned string via
`gfm-pretty-tables--transcribe-source-overlays', so a cell containing a
the links decorator-decorated link both renders and sizes its column by
the decorated width rather than the raw markdown width."
  (cond
   ((or (null cell) (string-empty-p cell)) (or cell ""))
   ((not (fboundp 'markdown-mode)) cell)
   (t
    (let ((fontified
           (with-current-buffer (gfm-pretty-tables--fontify-buffer)
             (let ((inhibit-modification-hooks t))
               (erase-buffer)
               (insert cell)
               (font-lock-ensure))
             (buffer-string))))
      (if (and buffer beg end)
          (gfm-pretty-tables--transcribe-source-overlays fontified buffer beg end)
        fontified)))))

(defun gfm-pretty-tables--fontify-row-cells (line-beg line-end)
  "Fontify the cells of the table row spanning LINE-BEG..LINE-END.
Each cell is measured against its source region so overlay
decorations (e.g. the links decorator link overlays) inside a cell size
the column to their decorated width.  Cells past the available source
bounds fall back to plain string measurement."
  (let* ((line (buffer-substring-no-properties line-beg line-end))
         (cells (gfm-pretty-tables--split-row line))
         (bounds (gfm-pretty-tables--cell-bounds line-beg line-end))
         (buffer (current-buffer)))
    (cl-loop for cell in cells
             for i from 0
             for b = (nth i bounds)
             collect (gfm-pretty-tables--fontify-cell
                      cell buffer (and b (car b)) (and b (cdr b))))))

(defun gfm-pretty-tables--source-row-cells (line-beg line-end)
  "Return raw trimmed source cells for the row spanning LINE-BEG..LINE-END.
Source cells skip the fontification pass, so callers checking shape
\(e.g. column-alignment numeric detection) never see markdown inline
markup interfere with the value."
  (gfm-pretty-tables--split-row
   (buffer-substring-no-properties line-beg line-end)))

(defun gfm-pretty-tables--invisible-p (val spec)
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

(defvar gfm-pretty-tables--width-cache nil
  "`eq'-keyed hash table memoising `gfm-pretty-tables--visible-width' results.
Bound to a fresh table at the start of each rebuild and unbound (nil)
outside one, so the cache never crosses rebuild boundaries.")

(defun gfm-pretty-tables--no-width-affecting-props-p (s)
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

(defun gfm-pretty-tables--visible-width--compute-region (buffer beg end)
  "Walk the region [BEG, END) in BUFFER, returning its visible width.
Consults overlay properties as well as text properties via
`get-char-property', so overlay decorations (e.g. the link overlays
created by the links decorator) are measured at their visible width
rather than their underlying source width.  Honours `display',
`invisible', and `composition' the same way the string walker does.
Leading and trailing whitespace of the region is trimmed first so a
table cell measures by its content, not its source padding."
  (with-current-buffer buffer
    (let ((spec buffer-invisibility-spec)
          (w 0))
      ;; Trim source padding: the cell region between pipes includes the
      ;; surrounding spaces, but column width is a property of the content.
      (while (and (< beg end)
                  (memq (char-after beg) '(?\s ?\t)))
        (setq beg (1+ beg)))
      (while (and (> end beg)
                  (memq (char-after (1- end)) '(?\s ?\t)))
        (setq end (1- end)))
      (let ((i beg))
        (while (< i end)
          (let* ((invis (get-char-property i 'invisible buffer))
                 (disp (get-char-property i 'display buffer))
                 (comp (find-composition i nil nil t)))
            (cond
             ((gfm-pretty-tables--invisible-p invis spec)
              (setq i (or (next-single-char-property-change
                           i 'invisible buffer end)
                          end)))
             ((stringp disp)
              (cl-incf w (string-width disp))
              (setq i (or (next-single-char-property-change
                           i 'display buffer end)
                          end)))
             ((and comp (= (nth 0 comp) i)
                   (get-char-property i 'composition buffer))
              (cl-incf w (or (nth 5 comp) 1))
              (setq i (min end (nth 1 comp))))
             (t
              (let* ((nd (or (next-single-char-property-change
                              i 'display buffer end)
                             end))
                     (ni (or (next-single-char-property-change
                              i 'invisible buffer end)
                             end))
                     (nc (or (next-single-char-property-change
                              i 'composition buffer end)
                             end))
                     (next (min nd ni nc end)))
                (cl-incf w (string-width
                            (buffer-substring-no-properties i next)))
                (setq i next)))))))
      w)))

(defun gfm-pretty-tables--visible-width--compute (s &optional buffer beg end)
  "Walk S to compute its visible width; honours display/composition/invisible.
With BUFFER, BEG and END all non-nil, walk the region [BEG, END) in
BUFFER instead of S, consulting overlay properties as well as text
properties — see `gfm-pretty-tables--visible-width--compute-region'.  This
keeps existing single-argument callers behaving exactly as before."
  (if (and buffer beg end)
      (gfm-pretty-tables--visible-width--compute-region buffer beg end)
    (if (gfm-pretty-tables--no-width-affecting-props-p s)
        (string-width s)
      (let ((spec buffer-invisibility-spec)
            (w 0) (i 0) (n (length s)))
        (while (< i n)
          (let* ((invis (get-text-property i 'invisible s))
                 (disp (get-text-property i 'display s))
                 (comp (find-composition i nil s t)))
            (cond
             ((gfm-pretty-tables--invisible-p invis spec)
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
        w))))

(defun gfm-pretty-tables--visible-width (s &optional buffer beg end)
  "Return on-screen width of S in the current buffer.
Honours `display' string replacements, the `composition' text property
\(used by `markdown-mode' to collapse hidden URLs into a single glyph),
and any `invisible' property currently hidden by `buffer-invisibility-spec'.
When `gfm-pretty-tables--width-cache' is bound, results are memoised by `eq'.

With BUFFER, BEG and END all non-nil, the visible width is measured
against the region [BEG, END) in BUFFER — consulting overlays as well
as text properties — instead of against S.  Region measurements are
not memoised, since the same string S may map to different source
regions across rebuilds."
  (cond
   ((and buffer beg end)
    (gfm-pretty-tables--visible-width--compute s buffer beg end))
   ((and gfm-pretty-tables--width-cache
         (gethash s gfm-pretty-tables--width-cache)))
   (t
    (let ((w (gfm-pretty-tables--visible-width--compute s)))
      (when gfm-pretty-tables--width-cache
        (puthash s w gfm-pretty-tables--width-cache))
      w))))

;;; Cell parser

(defun gfm-pretty-tables--split-row (line)
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

(defun gfm-pretty-tables--cell-bounds (line-beg line-end)
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

(defun gfm-pretty-tables--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-pretty-tables--find-blocks-1 ()
  "Scan the buffer for GFM tables, ignoring any excluded-ranges filtering.
Each entry is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END).  Internal
helper for `gfm-pretty-tables--find-blocks'.

The scanner widens for the duration of its body so the cache key
\(`buffer-chars-modified-tick') is a pure function of buffer contents
regardless of any current narrowing.  See fix-gfm-narrowing-safety."
  (let (blocks)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
        (while (re-search-forward gfm-pretty-tables--delim-re nil t)
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
                 (t (goto-char delim-end)))))))))))
    (nreverse blocks)))

(defun gfm-pretty-tables--find-blocks (&optional excluded-ranges)
  "Return all GFM tables in the buffer.
Each entry is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END).  Tables whose
delimiter row falls inside any (BEG . END) range in EXCLUDED-RANGES are
omitted.  Engine-level memoisation in `gfm-pretty--collect' covers the
rebuild path; this helper exists for filtered callers."
  (let ((all (gfm-pretty-tables--find-blocks-1)))
    (if excluded-ranges
        (cl-remove-if (lambda (b)
                        (gfm-pretty-tables--in-ranges-p (nth 1 b) excluded-ranges))
                      all)
      all)))

;;; Column alignment

(defconst gfm-pretty-tables--numeric-cell-rx
  (rx bos
      (? (any "+-"))
      (+ digit)
      (? "." (* digit))
      (? (any "eE") (? (any "+-")) (+ digit))
      eos)
  "Regex matching a plain numeric cell value.
Matches optional sign, digits, optional decimal portion, and optional
scientific exponent.  Thousands separators, percent signs, currency
sigils, and units are intentionally excluded.")

(defun gfm-pretty-tables--numeric-cell-p (cell)
  "Non-nil if CELL is a plain numeric value after trimming.
CELL is the raw source cell text — not the fontified rendering — so
inline markdown markup never disqualifies a numeric cell."
  (and cell
       (string-match-p gfm-pretty-tables--numeric-cell-rx
                       (string-trim cell))))

(defun gfm-pretty-tables--column-alignment (body-rows n-cols)
  "Return a per-column alignment vector for BODY-ROWS of width N-COLS.
Each slot is `right' iff every body-row cell in that column is either
numeric or empty after trimming AND at least one cell is numeric;
otherwise `left'.  Header content is not consulted."
  (let ((align (make-vector n-cols 'left)))
    (cl-loop for i below n-cols do
             (let ((any-numeric nil)
                   (all-num-or-empty t))
               (cl-loop for row in body-rows
                        while all-num-or-empty
                        do (let* ((cell (or (nth i row) ""))
                                  (trimmed (string-trim cell)))
                             (cond
                              ((string-empty-p trimmed) nil)
                              ((string-match-p
                                gfm-pretty-tables--numeric-cell-rx trimmed)
                               (setq any-numeric t))
                              (t (setq all-num-or-empty nil)))))
               (when (and any-numeric all-num-or-empty)
                 (aset align i 'right))))
    align))

;;; Column widths

(defun gfm-pretty-tables--column-widths (rows)
  "Return per-column max widths across ROWS as a vector.
Each row is a list of cell strings; column count is the longest row."
  (let* ((cols (apply #'max 0 (mapcar #'length rows)))
         (widths (make-vector cols 0)))
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 0
               do (aset widths i
                        (max (aref widths i)
                             (gfm-pretty-tables--visible-width cell)))))
    widths))

(defun gfm-pretty-tables--box-width (col-widths)
  "Return the visual width of the table given COL-WIDTHS.
Width = 2 (outer pipes) + Σ(col-w + 2) + (cols - 1) gaps."
  (let* ((cols (length col-widths))
         (cell-sum (cl-loop for i from 0 below cols
                            sum (+ (aref col-widths i) 2))))
    (+ 2 cell-sum (max 0 (1- cols)))))

;;; Width fitting

(defun gfm-pretty-tables--available-width (&optional window)
  "Return the available char width for a table in WINDOW.
WINDOW defaults to a window currently showing the buffer.  Uses
`window-max-chars-per-line', which accounts for the wrap-indicator
column on ttys and the fringe on graphical frames.  Falls back to
`fill-column' or 80 when WINDOW is nil and no window shows the buffer."
  (let ((win (or window
                 (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-pretty-tables--display-windows ()
  "Return windows currently displaying the buffer, for per-window rendering."
  (get-buffer-window-list (current-buffer) nil t))

(defun gfm-pretty-tables--fit-widths (natural budget)
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

(defun gfm-pretty-tables--cell-tokens (cell)
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

(defun gfm-pretty-tables--slice-by-visible-width (s width)
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

(defun gfm-pretty-tables--wrap-cell (cell width)
  "Wrap fontified CELL to lines of visible-width ≤ WIDTH.
Returns a non-empty list of propertized line strings.  Words longer
than WIDTH are hard-broken.  An empty CELL returns (\"\")."
  (let ((target (max 1 width))
        (words (gfm-pretty-tables--cell-tokens cell))
        (line "")
        (lines nil))
    (dolist (w words)
      (let ((ww (gfm-pretty-tables--visible-width w)))
        (cond
         ((> ww target)
          (unless (string-empty-p line)
            (push line lines)
            (setq line ""))
          (let ((chunks (gfm-pretty-tables--slice-by-visible-width w target)))
            (dolist (c (butlast chunks))
              (push c lines))
            (setq line (car (last chunks)))))
         ((<= (+ (gfm-pretty-tables--visible-width line)
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

(defun gfm-pretty-tables--compose-row (cells col-widths role &optional col-align)
  "Compose ROLE row's display string from CELLS and COL-WIDTHS.
ROLE is one of `header', `body-default', or `body-alt'.

COL-ALIGN, when non-nil, is a vector of `left'/`right' symbols parallel
to COL-WIDTHS controlling per-column pad placement.  `right' places the
pad on the leading edge so cell content flushes to the column's right
boundary; `left' (the default for omitted or unrecognised entries)
keeps the legacy trailing-pad behaviour.

Default-bg segments (gap chars, default-bg cells, header cells) are
left un-faced so they inherit the active `default' face.  This lets
focus-dim packages (per-window face remaps) and the terminal's tmux
pane dim track those segments automatically without any rebuild."
  (let* ((border-face gfm-pretty-tables--border-face)
         (cell-face (cl-case role
                      (header '(:weight bold))
                      (body-alt 'gfm-pretty-tables-row-alt-face)
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
             for cell-w = (gfm-pretty-tables--visible-width cell)
             for pad = (max 0 (- w cell-w))
             for align = (if col-align (aref col-align i) 'left)
             for pad-str = (make-string pad ?\s)
             for rendered = (pcase align
                              ('right (concat " " pad-str cell " "))
                              (_      (concat " " cell pad-str " ")))
             do (when cell-face
                  (add-face-text-property 0 (length rendered)
                                          cell-face t rendered))
                (push rendered parts)
                (when (< i (1- n))
                  (push " " parts)))
    (push rhs parts)
    (apply #'concat (nreverse parts))))

(defun gfm-pretty-tables--wrap-row-into-lines (cells col-widths)
  "Wrap CELLS to COL-WIDTHS, returning a list of per-visual-line cell-lists.
Each entry is the cells used for one visual line; cells whose wrap is
shorter than the row's height are padded with empty strings."
  (let* ((widths-list (cl-coerce col-widths 'list))
         (wrapped (cl-mapcar #'gfm-pretty-tables--wrap-cell cells widths-list))
         (height (apply #'max 1 (mapcar #'length wrapped)))
         (padded (mapcar (lambda (lines)
                           (append lines
                                   (make-list (- height (length lines)) "")))
                         wrapped)))
    (mapcar (lambda (idx)
              (mapcar (lambda (cell-lines) (nth idx cell-lines)) padded))
            (number-sequence 0 (1- height)))))

(cl-defstruct (gfm-pretty-tables--row-layout
               (:constructor gfm-pretty-tables--make-row-layout)
               (:copier nil))
  "Pre-computed wrapping for a single source row.
LINE-CELLS is a list of cell-lists, one per visual line, used by the
display-string composer.  N-CELLS is the column count, N-LINES the
visual-line count, and BOUNDS-VEC a flat fixnum vector of length
\(* 2 N-CELLS N-LINES) holding (BEG . END) pairs for every cell on
every visual line, packed in row-major order: BEG for cell IDX on
visual line LINE lives at index `(* 2 (+ (* LINE N-CELLS) IDX))', END
at the next slot.  Packing the bounds into a single vector turns the
inner lookup in `gfm-pretty-tables--apply-cell-highlight' from nested
cons-list traversal into O(1) `aref'."
  line-cells
  n-cells
  n-lines
  bounds-vec)

(defun gfm-pretty-tables--row-layout (cells col-widths)
  "Return a `gfm-pretty-tables--row-layout' for CELLS at COL-WIDTHS.
Computes wrapping and per-cell char bounds once so callers can share
the result instead of redoing the wrap."
  (let* ((line-cells (gfm-pretty-tables--wrap-row-into-lines cells col-widths))
         (n-cells (length col-widths))
         (n-lines (length line-cells))
         (bounds-vec (make-vector (* 2 n-cells n-lines) 0))
         (line-idx 0))
    (dolist (lc line-cells)
      (let ((cell-idx 0))
        (dolist (b (gfm-pretty-tables--row-char-bounds lc col-widths))
          (let ((base (* 2 (+ (* line-idx n-cells) cell-idx))))
            (aset bounds-vec base (car b))
            (aset bounds-vec (1+ base) (cdr b)))
          (cl-incf cell-idx)))
      (cl-incf line-idx))
    (gfm-pretty-tables--make-row-layout
     :line-cells line-cells
     :n-cells n-cells
     :n-lines n-lines
     :bounds-vec bounds-vec)))

(defun gfm-pretty-tables--display-cell-bounds (layout)
  "Return the (N-CELLS . BOUNDS-VEC) pair stored on a row's overlay.
The pair lets `gfm-pretty-tables--apply-cell-highlight' index the packed
bounds vector without keeping `LAYOUT' alive (the wrapped cell
strings in `line-cells' are no longer needed after composition)."
  (cons (gfm-pretty-tables--row-layout-n-cells layout)
        (gfm-pretty-tables--row-layout-bounds-vec layout)))

(defun gfm-pretty-tables--compose-row-from-layout (layout col-widths role
                                                          &optional col-align)
  "Compose ROLE row's display string from pre-computed LAYOUT and COL-WIDTHS.
COL-ALIGN, when non-nil, is forwarded to `gfm-pretty-tables--compose-row'
so every visual line of the layout shares the same per-column pad
placement."
  (mapconcat (lambda (line-cells)
               (gfm-pretty-tables--compose-row line-cells col-widths
                                               role col-align))
             (gfm-pretty-tables--row-layout-line-cells layout)
             "\n"))

(defun gfm-pretty-tables--compose-multiline-row (cells col-widths role
                                                       &optional col-align)
  "Compose ROLE row from CELLS, wrapping each to its column in COL-WIDTHS.
Returns a string in which visual lines are joined by newlines.  All
cells in the row are padded to the tallest cell's height with empty
strings, so the column grid stays aligned across visual lines.
COL-ALIGN, when non-nil, is forwarded to
`gfm-pretty-tables--compose-row-from-layout'."
  (gfm-pretty-tables--compose-row-from-layout
   (gfm-pretty-tables--row-layout cells col-widths)
   col-widths role col-align))

(defun gfm-pretty-tables--row-char-bounds (cells col-widths)
  "Return per-cell (BEG . END) offsets for CELLS at COL-WIDTHS.
Each range covers the cell's display segment from the leading padding
space through the trailing padding space, exclusive of outer pipes and
inter-cell gaps, in `gfm-pretty-tables--compose-row's output.  Bounds reflect
the cell strings' actual lengths, so they remain accurate when cells
contain compositions or other text-property tricks that make displayed
width less than char length."
  (let ((n (length col-widths))
        (pos 1)
        (bounds nil))
    (cl-loop for i from 0 below n
             for cell = (or (nth i cells) "")
             for cell-w = (gfm-pretty-tables--visible-width cell)
             for w = (aref col-widths i)
             for pad = (max 0 (- w cell-w))
             for seg-len = (+ 2 (length cell) pad)
             do (push (cons pos (+ pos seg-len)) bounds)
                (setq pos (+ pos seg-len))
                (when (< i (1- n))
                  (setq pos (1+ pos))))
    (nreverse bounds)))

(defun gfm-pretty-tables--multiline-row-char-bounds (cells col-widths)
  "Per-visual-line char bounds for CELLS wrapped to COL-WIDTHS.
Returns a list with one entry per visual line in
`gfm-pretty-tables--compose-multiline-row's output; each entry is the
`gfm-pretty-tables--row-char-bounds' for that line's wrapped cells.
Reconstructs the legacy list-of-lists shape from the packed bounds
vector for callers (and tests) that depend on the older API."
  (let* ((layout (gfm-pretty-tables--row-layout cells col-widths))
         (vec (gfm-pretty-tables--row-layout-bounds-vec layout))
         (n-cells (gfm-pretty-tables--row-layout-n-cells layout))
         (n-lines (gfm-pretty-tables--row-layout-n-lines layout)))
    (cl-loop for line below n-lines
             collect (cl-loop for cell below n-cells
                              for base = (* 2 (+ (* line n-cells) cell))
                              collect (cons (aref vec base)
                                            (aref vec (1+ base)))))))

(defun gfm-pretty-tables--rule-row (box-width)
  "Return a `├─…─┤' T-junction rule between header and body, width BOX-WIDTH."
  (let ((face gfm-pretty-tables--border-face))
    (concat (propertize "├" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┤" 'face face))))

(defun gfm-pretty-tables--top-border (box-width)
  "Return a `┌─…─┐' top border of total width BOX-WIDTH."
  (let ((face gfm-pretty-tables--border-face))
    (concat (propertize "┌" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┐" 'face face))))

(defun gfm-pretty-tables--bottom-border (box-width)
  "Return a `└─…─┘' bottom border of total width BOX-WIDTH."
  (let ((face gfm-pretty-tables--border-face))
    (concat (propertize "└" 'face face)
            (propertize (make-string (max 0 (- box-width 2)) ?─)
                        'face face)
            (propertize "┘" 'face face))))

;;; Phase timing helpers (route through engine)

(defmacro gfm-pretty-tables--time-phase (phase &rest body)
  "Run BODY, accumulating its wall-time into PHASE on the tables decorator."
  (declare (indent 1) (debug (form body)))
  `(gfm-pretty-time-phase 'tables ,phase ,@body))

;;; Overlay application

(defconst gfm-pretty-tables--registry
  (gfm-pretty--registry-for 'tables 'gfm-pretty-tables)
  "Shared overlay-registry context for tables.
Routes the full-clear teardown through `gfm-pretty--remove-overlays'
so it widens for the duration of the clear, matching the fences/callouts
contract.")

(defun gfm-pretty-tables--register (ov)
  "Tag OV as a gfm-pretty-tables overlay and remember it for bulk cleanup."
  (overlay-put ov 'gfm-pretty-tables t)
  (gfm-pretty--state-push 'tables 'overlays ov)
  ov)

(defun gfm-pretty-tables--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-tables overlays between BEG and END.
Delegates to the shared `gfm-pretty--remove-overlays' so the
no-arg full-clear branch widens; scoped (BEG/END) calls operate on the
literal range as before."
  (gfm-pretty--remove-overlays gfm-pretty-tables--registry beg end))

(defun gfm-pretty-tables--make-anchor (beg end cell-bounds)
  "Create an anchor overlay over [BEG, END] holding CELL-BOUNDS.
Anchors carry source-side bookkeeping — `gfm-pretty-tables-cell-bounds' and
the row keymap — and never carry a `display' property.  They exist
once per source row regardless of how many windows are showing the
buffer; the per-window rendering lives on display overlays."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'gfm-pretty-tables t)
    (overlay-put ov 'gfm-pretty-tables-anchor t)
    (overlay-put ov 'gfm-pretty-tables-cell-bounds cell-bounds)
    (overlay-put ov 'keymap gfm-pretty-tables--row-map)
    (gfm-pretty--state-push 'tables 'overlays ov)
    ov))

(defun gfm-pretty-tables--make-display (beg end window display before after col-widths dcb)
  "Create a display overlay over [BEG, END] for WINDOW.
DISPLAY is the composed row text, BEFORE/AFTER the surrounding border
strings (nil for none), COL-WIDTHS the column-width vector, and DCB
the (N-CELLS . BOUNDS-VEC) packed pair from
`gfm-pretty-tables--display-cell-bounds'.  Display overlays carry the visible
rendering and never the source keymap (the anchor handles motion).
WINDOW non-nil restricts the overlay to that window only — Emacs
renders it just there.  Pass nil for a fallback overlay applying in
every window (used when no window currently shows the buffer)."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'gfm-pretty-tables t)
    (overlay-put ov 'gfm-pretty-tables-display t)
    (when window (overlay-put ov 'window window))
    (overlay-put ov 'display display)
    (when before (overlay-put ov 'before-string before))
    (when after (overlay-put ov 'after-string after))
    (when col-widths (overlay-put ov 'gfm-pretty-tables-col-widths col-widths))
    (when dcb (overlay-put ov 'gfm-pretty-tables-display-cell-bounds dcb))
    (gfm-pretty--state-push 'tables 'overlays ov)
    ov))

(defun gfm-pretty-tables--display-overlay-for-anchor (anchor &optional window)
  "Return the display overlay paired with ANCHOR for WINDOW, or nil.
WINDOW defaults to the selected window.  An unrestricted display
overlay (one created without a window arg) matches any WINDOW."
  (let ((win (or window (selected-window))))
    (cl-find-if (lambda (o)
                  (and (overlay-get o 'gfm-pretty-tables-display)
                       (let ((w (overlay-get o 'window)))
                         (or (null w) (eq w win)))))
                (overlays-in (overlay-start anchor) (overlay-end anchor)))))

(defun gfm-pretty-tables--remove-overlays-in-block (block)
  "Remove gfm-pretty-tables overlays within BLOCK and prune the cached list.
BLOCK is (HEADER-BEG DELIM-BEG BODY-BEG BODY-END)."
  (let ((header-beg (nth 0 block))
        (body-end (nth 3 block)))
    (gfm-pretty-tables--remove-overlays header-beg (1+ body-end))))

(defun gfm-pretty-tables--apply-table-display (window header-beg header-end
                                        delim-beg delim-end body-positions
                                        header-cells body-rows
                                        &optional body-source-rows)
  "Build per-WINDOW display overlays for a parsed table.
WINDOW is either a window object (overlays restrict to it) or nil
\(unrestricted fallback).  Other args are the parse output produced by
`gfm-pretty-tables--apply-table'.  BODY-SOURCE-ROWS, when non-nil, is
the list of raw source-cell lists used to compute per-column
alignment; when nil the alignment defaults to all-left."
  (let* ((all-rows (cons header-cells body-rows))
         (natural (gfm-pretty-tables--time-phase 'layout
                    (gfm-pretty-tables--column-widths all-rows)))
         (n-cols (length natural))
         (overhead (+ (* 3 n-cols) 1))
         (budget (max n-cols
                      (- (gfm-pretty-tables--available-width window) overhead)))
         (col-widths (gfm-pretty-tables--time-phase 'layout
                       (gfm-pretty-tables--fit-widths natural budget)))
         (col-align (gfm-pretty-tables--time-phase 'layout
                      (gfm-pretty-tables--column-alignment
                       (or body-source-rows body-rows) n-cols)))
         (box-width (gfm-pretty-tables--box-width col-widths))
         (top (gfm-pretty-tables--top-border box-width))
         (bottom (gfm-pretty-tables--bottom-border box-width))
         (rule (gfm-pretty-tables--rule-row box-width))
         (n-body (length body-rows))
         (header-layout (gfm-pretty-tables--time-phase 'layout
                          (gfm-pretty-tables--row-layout header-cells col-widths)))
         (body-layouts (gfm-pretty-tables--time-phase 'layout
                         (mapcar (lambda (cells)
                                   (gfm-pretty-tables--row-layout cells col-widths))
                                 body-rows))))
    (gfm-pretty-tables--time-phase 'apply
      (let ((header-display
             (gfm-pretty-tables--time-phase 'compose
               (gfm-pretty-tables--compose-row-from-layout
                header-layout col-widths 'header col-align))))
        (gfm-pretty-tables--make-display
         header-beg header-end window
         header-display (concat top "\n") nil
         col-widths (gfm-pretty-tables--display-cell-bounds header-layout)))
      (gfm-pretty-tables--make-display delim-beg delim-end window
                                rule nil nil nil nil)
      (cl-loop for (lbeg . lend) in body-positions
               for _cells in body-rows
               for layout in body-layouts
               for idx from 1
               for last-p = (= idx n-body)
               for role = (if (cl-evenp idx) 'body-alt 'body-default)
               for str = (gfm-pretty-tables--time-phase 'compose
                           (gfm-pretty-tables--compose-row-from-layout
                            layout col-widths role col-align))
               do (gfm-pretty-tables--make-display
                   lbeg lend window
                   str nil (and last-p (concat "\n" bottom))
                   col-widths
                   (gfm-pretty-tables--display-cell-bounds layout))))))

(defun gfm-pretty-tables--parse-table (header-beg delim-beg body-beg body-end)
  "Parse the table at HEADER-BEG..BODY-END, returning a plist.
Keys: :header-beg :header-end :delim-beg :delim-end :body-positions
\(list of (LBEG . LEND) per body row in source order),
:header-cells (fontified strings), :body-rows (list of fontified
cell-lists), :body-source-rows (list of raw source cell-lists, in the
same order as :body-rows, for shape-based heuristics such as numeric
column detection)."
  (save-excursion
    (let* ((header-end (save-excursion
                         (goto-char header-beg) (line-end-position)))
           (delim-end (save-excursion
                        (goto-char delim-beg) (line-end-position)))
           (header-cells (gfm-pretty-tables--time-phase 'parse
                           (gfm-pretty-tables--fontify-row-cells
                            header-beg header-end)))
           (body-rows '())
           (body-source-rows '())
           (body-positions '()))
      (gfm-pretty-tables--time-phase 'parse
        (goto-char body-beg)
        (while (< (point) body-end)
          (let* ((lbeg (line-beginning-position))
                 (lend (line-end-position)))
            (push (cons lbeg lend) body-positions)
            (push (gfm-pretty-tables--source-row-cells lbeg lend)
                  body-source-rows)
            (push (gfm-pretty-tables--fontify-row-cells lbeg lend)
                  body-rows))
          (forward-line 1)))
      (list :header-beg header-beg :header-end header-end
            :delim-beg delim-beg :delim-end delim-end
            :body-positions (nreverse body-positions)
            :header-cells header-cells
            :body-rows (nreverse body-rows)
            :body-source-rows (nreverse body-source-rows)))))

(defun gfm-pretty-tables--apply-table-display-for-parsed (parsed window)
  "Build display overlays for PARSED in WINDOW (or unrestricted when nil)."
  (gfm-pretty-tables--apply-table-display
   window
   (plist-get parsed :header-beg) (plist-get parsed :header-end)
   (plist-get parsed :delim-beg) (plist-get parsed :delim-end)
   (plist-get parsed :body-positions)
   (plist-get parsed :header-cells) (plist-get parsed :body-rows)
   (plist-get parsed :body-source-rows)))

(defun gfm-pretty-tables--apply-table (header-beg delim-beg body-beg body-end)
  "Decorate one table identified by HEADER-BEG, DELIM-BEG, BODY-BEG, BODY-END.
Builds anchor overlays once per row and one display overlay per row
*per window* currently showing the buffer.  When no window shows the
buffer, falls back to a single unrestricted display overlay.

Widens for the duration of its body so cached positions outside the
current restriction (e.g. a table on another slide under
`gfm-present-mode') can be parsed and decorated.  Display under
narrowing is naturally clipped by Emacs' overlay engine."
  (save-restriction
    (widen)
    (let ((parsed (gfm-pretty-tables--parse-table header-beg delim-beg
                                           body-beg body-end)))
      (gfm-pretty-tables--time-phase 'apply
        (gfm-pretty-tables--make-anchor
         header-beg (plist-get parsed :header-end)
         (gfm-pretty-tables--cell-bounds header-beg (plist-get parsed :header-end)))
        (cl-loop for (lbeg . lend) in (plist-get parsed :body-positions)
                 do (gfm-pretty-tables--make-anchor
                     lbeg lend (gfm-pretty-tables--cell-bounds lbeg lend))))
      (let ((windows (or (gfm-pretty-tables--display-windows) (list nil))))
        (dolist (win windows)
          (gfm-pretty-tables--apply-table-display-for-parsed parsed win))))))

(defun gfm-pretty-tables--remove-display-overlays-in-block (block window)
  "Delete WINDOW's display overlays inside BLOCK.
WINDOW non-nil matches only that window's overlays; nil matches every
display overlay (used when wiping out an unrestricted fallback set)."
  (let ((header-beg (nth 0 block))
        (body-end (nth 3 block)))
    (dolist (ov (overlays-in header-beg (1+ body-end)))
      (when (and (overlay-get ov 'gfm-pretty-tables-display)
                 (or (null window)
                     (eq (overlay-get ov 'window) window)))
        (delete-overlay ov))))
  (gfm-pretty--prune-dead-overlays gfm-pretty-tables--registry))

(defun gfm-pretty-tables--remove-display-overlays-for-window (window)
  "Delete every display overlay restricted to WINDOW across the buffer."
  (dolist (ov (gfm-pretty--state-get 'tables 'overlays))
    (when (and (overlay-buffer ov)
               (overlay-get ov 'gfm-pretty-tables-display)
               (eq (overlay-get ov 'window) window))
      (delete-overlay ov)))
  (gfm-pretty--prune-dead-overlays gfm-pretty-tables--registry))

(defun gfm-pretty-tables--rebuild-block-for-window (block window)
  "Replace WINDOW's display overlays for BLOCK with fresh ones at current width.
Anchors and other windows' display overlays are left untouched.
Widens so a BLOCK whose source range lies outside the current
restriction can still be parsed and re-rendered."
  (save-restriction
    (widen)
    (cl-destructuring-bind (header-beg delim-beg body-beg body-end) block
      (let* ((gfm-pretty-tables--width-cache
              (or gfm-pretty-tables--width-cache
                  (make-hash-table :test 'eq)))
             (parsed (gfm-pretty-tables--parse-table header-beg delim-beg
                                              body-beg body-end)))
        (gfm-pretty-tables--remove-display-overlays-in-block block window)
        (gfm-pretty-tables--apply-table-display-for-parsed parsed window)))))

(defun gfm-pretty-tables--fenced-ranges ()
  "Return (BEG . END) ranges of fenced code blocks, if discoverable."
  (and (fboundp 'gfm-pretty-fences--find-blocks)
       (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b)))
               (gfm-pretty-fences--find-blocks))))

(defun gfm-pretty-tables--apply-overlays ()
  "Apply table overlays to all GFM tables in the buffer."
  (let* ((blocks (gfm-pretty-tables--time-phase 'find-blocks
                   (gfm-pretty-tables--find-blocks (gfm-pretty-tables--fenced-ranges))))
         (n 0))
    (dolist (block blocks)
      (cl-destructuring-bind (h d bb be) block
        (gfm-pretty-tables--apply-table h d bb be)
        (cl-incf n)))
    n))

;;; Rebuild

(defun gfm-pretty-tables--window-state ()
  "Return the (WINDOW . WIDTH) snapshot used to detect rendering drift."
  (mapcar (lambda (w) (cons w (gfm-pretty-tables--available-width w)))
          (gfm-pretty-tables--display-windows)))

(defun gfm-pretty-tables--rebuild ()
  "Remove and recreate all gfm-pretty-tables overlays.
Re-applies the active-cell highlight afterwards so cell selection
survives window-configuration changes and other rebuild triggers.
Stamps the engine's `last-rebuild-tick' so direct callers — e.g.
`--insert-new-row-after-current', `--swap-columns', the test harness
— clear `gfm-pretty--stale-p' without round-tripping through the
engine wrapper."
  (let ((gfm-pretty-tables--width-cache (make-hash-table :test 'eq)))
    (gfm-pretty-tables--remove-overlays)
    (gfm-pretty-tables--apply-overlays)
    (gfm-pretty--state-set 'tables 'last-window-state
                           (gfm-pretty-tables--window-state))
    (gfm-pretty--state-set 'tables 'last-rebuild-tick
                           (buffer-chars-modified-tick))
    (gfm-pretty-tables--update-cursor-highlight)))

(defun gfm-pretty-tables--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that table."
  (let ((gfm-pretty-tables--width-cache (make-hash-table :test 'eq)))
    (gfm-pretty-tables--remove-overlays-in-block block)
    (cl-destructuring-bind (h d bb be) block
      (gfm-pretty-tables--apply-table h d bb be)))
  (gfm-pretty-tables--update-cursor-highlight))

(defun gfm-pretty-tables--rebuild-blocks (blocks)
  "Tear down each block in BLOCKS and re-apply those tables in one pass."
  (let ((gfm-pretty-tables--width-cache (make-hash-table :test 'eq)))
    (dolist (block blocks)
      (gfm-pretty-tables--remove-overlays-in-block block)
      (cl-destructuring-bind (h d bb be) block
        (gfm-pretty-tables--apply-table h d bb be)))))

(defun gfm-pretty-tables--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any (VSTART . VEND) in RANGES."
  (let ((header-beg (nth 0 block))
        (body-end (nth 3 block)))
    (cl-some (lambda (r)
               (and (<= header-beg (cdr r))
                    (>= body-end (car r))))
             ranges)))

(defun gfm-pretty-tables--visible-window-ranges ()
  "Return (VSTART . VEND) pairs for every window currently showing this buffer.
Walks all visible (and iconified) frames so a buffer split across two
windows treats both viewports as on-screen.  `window-end' is asked for
the live value (second arg t), since we're driven by
`window-configuration-change-hook' where the cached value may be stale."
  (mapcar (lambda (w) (cons (window-start w) (window-end w t)))
          (get-buffer-window-list (current-buffer) nil t)))

(defun gfm-pretty-tables--rebuild-window-prioritised (window)
  "Visible-first / off-screen-deferred rebuild of WINDOW's display overlays.
Anchors and other windows' overlays are not touched.  Used by
`gfm-pretty-tables--reconcile-windows' when WINDOW is newly showing the
buffer or has been resized."
  (when (window-live-p window)
    (let* ((blocks (gfm-pretty-tables--find-blocks (gfm-pretty-tables--fenced-ranges)))
           (vstart (window-start window))
           (vend (window-end window t))
           (ranges (list (cons vstart vend)))
           (visible (cl-remove-if-not
                     (lambda (b) (gfm-pretty-tables--block-visible-p b ranges))
                     blocks))
           (offscreen (cl-set-difference blocks visible))
           (gfm-pretty-tables--width-cache (make-hash-table :test 'eq)))
      (dolist (b visible)
        (gfm-pretty-tables--rebuild-block-for-window b window))
      (when offscreen
        (run-with-idle-timer
         0 nil
         (lambda (buf bs win)
           (when (and (buffer-live-p buf) (window-live-p win))
             (with-current-buffer buf
               (when (gfm-pretty--state-get 'tables 'enabled-p)
                 (let ((gfm-pretty-tables--width-cache
                        (make-hash-table :test 'eq)))
                   (dolist (b bs)
                     (gfm-pretty-tables--rebuild-block-for-window b win)))))))
         (current-buffer) offscreen window)))))

(defun gfm-pretty-tables--reconcile-windows ()
  "Reconcile display overlays with current window state.
Compares the current `(WIN . WIDTH)' snapshot to the engine's cached
`last-window-state' for the tables decorator and acts only on the
diff: added/resized windows get a prioritised rebuild; removed
windows have their display overlays deleted.  Falls back to a full
rebuild via the engine when called for the first time or when an
anchor is missing."
  (let ((prev (gfm-pretty--state-get 'tables 'last-window-state)))
    (cond
     ((or (null prev)
          (null (cl-some (lambda (o) (overlay-get o 'gfm-pretty-tables-anchor))
                         (gfm-pretty--state-get 'tables 'overlays))))
      (gfm-pretty--rebuild (gfm-pretty--get 'tables)))
     (t
      (let* ((curr (gfm-pretty-tables--window-state))
             (prev-keys (mapcar #'car prev))
             (curr-keys (mapcar #'car curr))
             (added (cl-remove-if (lambda (e) (memq (car e) prev-keys)) curr))
             (removed (cl-remove-if (lambda (w) (memq w curr-keys)) prev-keys))
             (resized (cl-remove-if-not
                       (lambda (e)
                         (let ((old (assq (car e) prev)))
                           (and old (not (eql (cdr e) (cdr old))))))
                       curr)))
        (dolist (w removed)
          (gfm-pretty-tables--remove-display-overlays-for-window w))
        (dolist (entry (append added resized))
          (gfm-pretty-tables--rebuild-window-prioritised (car entry)))
        (gfm-pretty--state-set 'tables 'last-window-state curr)
        (gfm-pretty-tables--update-cursor-highlight))))))

;;; Engine adapter helpers

(defun gfm-pretty-tables--collect-blocks ()
  "Engine `:collect-fn' — return tagged tables for the engine reconciler."
  (gfm-pretty-tables--find-blocks (gfm-pretty-tables--fenced-ranges)))

(defun gfm-pretty-tables--block-range (block)
  "Engine `:range-fn' — return (HEADER-BEG . BODY-END+1) for BLOCK."
  (cons (nth 0 block) (1+ (nth 3 block))))

(defun gfm-pretty-tables--apply-block (block window)
  "Engine `:apply-block-fn' — apply WINDOW's display overlays for BLOCK.
Anchors are laid by the full `gfm-pretty-tables--apply-table' path
during `--rebuild', so the per-window seam only re-renders the
display layer here."
  (gfm-pretty-tables--rebuild-block-for-window block window))

;;; Lifecycle hooks delegated to engine

(defun gfm-pretty-tables--wcc-handler (&rest _)
  "Buffer-local `window-configuration-change-hook' handler.
Invokes the tables-specific per-window reconciler when the tables
decorator is enabled and the window state has changed."
  (when (gfm-pretty--state-get 'tables 'enabled-p)
    (gfm-pretty-tables--reconcile-windows)))

(defun gfm-pretty-tables--on-enable ()
  "Per-decorator setup invoked on enable.
Installs the active-cell cursor handler and a buffer-local
`window-configuration-change-hook' that routes tables's bespoke
per-window reconciler (the engine's generic reconciler is not used
for tables)."
  (add-hook 'post-command-hook
            #'gfm-pretty-tables--update-cursor-highlight nil t)
  (add-hook 'window-configuration-change-hook
            #'gfm-pretty-tables--wcc-handler nil t))

(defun gfm-pretty-tables--on-disable ()
  "Per-decorator teardown invoked on disable."
  (remove-hook 'post-command-hook
               #'gfm-pretty-tables--update-cursor-highlight t)
  (remove-hook 'window-configuration-change-hook
               #'gfm-pretty-tables--wcc-handler t)
  (gfm-pretty-tables--hide-cursor-highlight))

;;; Stats command (thin wrapper over engine)

;;;###autoload
(defun gfm-pretty-tables-stats ()
  "Display the tables decorator's rebuild stats and phase totals."
  (interactive)
  (gfm-pretty-stats 'tables))

;;; Indirect editing

(declare-function edit-indirect-region "edit-indirect")
(declare-function orgtbl-mode "org-table")
(declare-function markdown-mode "markdown-mode")

(defun gfm-pretty-tables--block-at-point ()
  "Return (BEG . END) of the table block containing point, or nil."
  (let ((pt (point)))
    (cl-loop for (header-beg _delim-beg _body-beg body-end)
             in (gfm-pretty-tables--find-blocks)
             when (and (>= pt header-beg) (<= pt body-end))
             return (cons header-beg body-end))))

(defvar-local gfm-pretty-tables--edit-source-buffer nil
  "Buffer-local in the indirect edit buffer; points to the parent buffer.")

(defvar-local gfm-pretty-tables--edit-pending-cell-info nil
  "Stashed (LINE-OFFSET . CELL-IDX) captured before edit-indirect commits.
Set on the parent buffer by `gfm-pretty-tables--edit-before-commit', consumed
by `gfm-pretty-tables--edit-after-commit' which runs in the parent.")

(defun gfm-pretty-tables--edit-cell-info-here (&optional region-beg)
  "Return (LINE-OFFSET . CELL-IDX) for point on the current line.
LINE-OFFSET is counted from REGION-BEG (defaulting to `point-min'),
so the value is comparable between a source buffer (passing the table's
start) and an indirect edit buffer (whose `point-min' is that start)."
  (let* ((cb (gfm-pretty-tables--cell-bounds (line-beginning-position)
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

(defun gfm-pretty-tables--goto-cell-on-current-line (idx)
  "Move point to the start of cell IDX on the current line, if it exists."
  (let ((cb (gfm-pretty-tables--cell-bounds (line-beginning-position)
                                     (line-end-position))))
    (when (and cb (< idx (length cb)))
      (goto-char (car (nth idx cb))))))

(defun gfm-pretty-tables--edit-before-commit ()
  "Capture indirect-buffer cell position into the parent buffer."
  (let ((info (gfm-pretty-tables--edit-cell-info-here))
        (parent gfm-pretty-tables--edit-source-buffer))
    (when (and parent (buffer-live-p parent))
      (with-current-buffer parent
        (setq gfm-pretty-tables--edit-pending-cell-info info)))))

(defun gfm-pretty-tables--edit-after-commit (beg _end)
  "After commit, move point to the captured cell starting from BEG.
Recenters if off-screen.  The actual move is deferred to the next
event-loop tick so it runs after any after-advice on
`edit-indirect-commit' (e.g. the one `markdown-mode' installs to track
committed positions)."
  (when gfm-pretty-tables--edit-pending-cell-info
    (let* ((info gfm-pretty-tables--edit-pending-cell-info)
           (line-offset (car info))
           (cell-idx (cdr info))
           (buf (current-buffer)))
      (setq gfm-pretty-tables--edit-pending-cell-info nil)
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (goto-char beg)
             (forward-line line-offset)
             (gfm-pretty-tables--goto-cell-on-current-line cell-idx)
             (let ((win (get-buffer-window buf)))
               (when (window-live-p win)
                 (set-window-point win (point))
                 (unless (pos-visible-in-window-p (point) win)
                   (with-selected-window win (recenter)))))
             (when (gfm-pretty--state-get 'tables 'enabled-p)
               (gfm-pretty-tables--update-cursor-highlight)))))))))

(defun gfm-pretty-tables--cell-content-bounds (row-ov idx)
  "Return (BEG . END) of cell IDX inside ROW-OV, trimmed of whitespace."
  (let* ((cb (overlay-get row-ov 'gfm-pretty-tables-cell-bounds))
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

(defun gfm-pretty-tables--cell-link-pos (row-ov idx)
  "Return the buffer position of the first link inside cell IDX of ROW-OV.
Searches for inline `[text](url)', reference `[text][ref]', and bare URI
forms within the cell's source range.  Returns nil if none is found."
  (let* ((bounds (gfm-pretty-tables--cell-content-bounds row-ov idx))
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

(defun gfm-pretty-tables--edit-header-line (label)
  "Return a header-line string for an indirect edit buffer titled LABEL."
  (concat (propertize (concat " " label) 'face 'mode-line-emphasis)
          (propertize "  C-c C-c" 'face 'help-key-binding) " commit"
          (propertize "  C-c C-k" 'face 'help-key-binding) " abort"))

(defun gfm-pretty-tables--cell-edit-sanitise ()
  "Strip newlines and escape unescaped `|' in the current buffer.
Run in the indirect cell edit buffer just before commit."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " " nil t))
    (goto-char (point-min))
    (while (re-search-forward "\\(\\`\\|[^\\\\]\\)\\(|\\)" nil t)
      (replace-match "\\1\\\\|" t nil))))

(defvar-local gfm-pretty-tables--cell-edit-pending nil
  "Non-nil between a cell-edit's before-commit and after-commit hooks.
Set buffer-locally on the parent (markdown) buffer.  Tells
`gfm-pretty-tables--cell-edit-after-commit' to strip the stray trailing
newline that `markdown--edit-indirect-after-commit-function' inserts
\(it treats every committed region as a code block, but a cell edit
commits raw cell content with no terminating newline).")

(defun gfm-pretty-tables--cell-edit-mark-pending ()
  "Indirect-buffer before-commit hook: mark the parent for cleanup."
  (let ((parent (and (boundp 'edit-indirect--overlay)
                     edit-indirect--overlay
                     (overlay-buffer edit-indirect--overlay))))
    (when (buffer-live-p parent)
      (with-current-buffer parent
        (setq gfm-pretty-tables--cell-edit-pending t)))))

(defun gfm-pretty-tables--cell-edit-after-commit (_beg end)
  "After-commit hook: drop the stray trailing newline left by a cell edit.
The end position is the cell-content-end marker passed to markdown's
hook; after markdown inserts `\\n' at that position, the marker stays
just *before* the new newline (default insertion-type), so we look at
`char-after' rather than `char-before'."
  (when gfm-pretty-tables--cell-edit-pending
    (setq gfm-pretty-tables--cell-edit-pending nil)
    (when (and (< end (point-max)) (eq (char-after end) ?\n))
      (let ((inhibit-read-only t))
        (delete-region end (1+ end))))))

;;;###autoload
(defun gfm-pretty-tables-edit-cell-at-point ()
  "Open the active table cell in an indirect markdown edit buffer.
On commit, embedded newlines are replaced with spaces and any
unescaped `|' is escaped to `\\|'."
  (interactive)
  (require 'edit-indirect)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((row-ov (car info))
           (idx (cdr info))
           (bounds (gfm-pretty-tables--cell-content-bounds row-ov idx))
           (src-buf (current-buffer))
           (edit-indirect-guess-mode-function
            (lambda (_parent _beg _end) (markdown-mode)))
           (buf (edit-indirect-region (car bounds) (cdr bounds) t)))
      (with-current-buffer buf
        (setq-local require-final-newline nil)
        (setq-local mode-require-final-newline nil)
        (setq-local header-line-format
                    (gfm-pretty-tables--edit-header-line "Edit cell"))
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-pretty-tables--cell-edit-mark-pending nil t)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-pretty-tables--cell-edit-sanitise nil t))
      (with-current-buffer src-buf
        ;; Append so this runs *after* `markdown--edit-indirect-after-commit-function',
        ;; which is what inserts the spurious newline we need to strip.
        (add-hook 'edit-indirect-after-commit-functions
                  #'gfm-pretty-tables--cell-edit-after-commit
                  'append 'local)))))

;;;###autoload
(defun gfm-pretty-tables-edit-table-at-point ()
  "Open the GFM table containing point in an indirect edit buffer.
The edit buffer uses `markdown-mode' with `orgtbl-mode' enabled, so
TAB navigates cells, edits auto-align columns, and M-<right>/<left>
moves columns as in `org-mode'.  Point in the indirect buffer lands
at the same cell as in the parent.  When the indirect buffer is
committed, point in the parent is moved to the cell point was on
in the indirect buffer (recentering if it has scrolled off-screen)."
  (interactive)
  (require 'edit-indirect)
  (let ((bounds (gfm-pretty-tables--block-at-point))
        (src-buf (current-buffer)))
    (unless bounds
      (user-error "Point is not inside a GFM table"))
    (let* ((info (gfm-pretty-tables--edit-cell-info-here (car bounds)))
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
                    (gfm-pretty-tables--edit-header-line "Edit table"))
        (setq gfm-pretty-tables--edit-source-buffer src-buf)
        (goto-char (point-min))
        (forward-line line-offset)
        (gfm-pretty-tables--goto-cell-on-current-line cell-idx)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-pretty-tables--edit-before-commit nil t))
      (with-current-buffer src-buf
        (add-hook 'edit-indirect-after-commit-functions
                  #'gfm-pretty-tables--edit-after-commit nil t)))))

;;; Active-cell highlight

(defvar-local gfm-pretty-tables--highlighted-anchor nil
  "Anchor overlay whose row currently carries the active-cell highlight.")

(defvar-local gfm-pretty-tables--highlighted-row-ov nil
  "Display overlay whose `display' currently carries the active-cell paint.
Kept for the legacy name expected by tests; tracks the rendering layer
that `gfm-pretty-tables--hide-cursor-highlight' must restore.")

(defvar-local gfm-pretty-tables--current-highlight-key nil
  "The (ANCHOR . IDX) currently rendered as the active-cell highlight.
Compared by `equal' in `gfm-pretty-tables--update-cursor-highlight' so motion
that lands on the same cell can skip the display repaint.")

(defvar-local gfm-pretty-tables--cursor-anchor nil
  "Buffer position currently flagged with the `cursor' text property.
Used to anchor the visible terminal cursor at the active cell's first
char rather than at the right edge of the overlay's display range.")

(defvar-local gfm-pretty-tables--saved-cursor-type 'gfm-pretty-tables-unset
  "Stashed `cursor-type' captured before being hidden over a table.
The sentinel `gfm-pretty-tables-unset' means no value is stashed; the
sentinel `gfm-pretty-tables-global' means the variable was not buffer-local
when we stashed it.")

(defun gfm-pretty-tables--save-and-hide-cursor ()
  "Hide the buffer cursor, stashing the previous value for restoration.
Reliable on graphical frames; tty cursor visibility is left as-is
because tmux + post-redisplay re-shows from Emacs's own internals
make it impractical to keep hidden in a terminal without intrusive
hacks."
  (when (eq gfm-pretty-tables--saved-cursor-type 'gfm-pretty-tables-unset)
    (setq gfm-pretty-tables--saved-cursor-type
          (if (local-variable-p 'cursor-type)
              cursor-type
            'gfm-pretty-tables-global)))
  (setq-local cursor-type nil))

(defun gfm-pretty-tables--restore-cursor ()
  "Restore the stashed `cursor-type', if any."
  (unless (eq gfm-pretty-tables--saved-cursor-type 'gfm-pretty-tables-unset)
    (if (eq gfm-pretty-tables--saved-cursor-type 'gfm-pretty-tables-global)
        (kill-local-variable 'cursor-type)
      (setq-local cursor-type gfm-pretty-tables--saved-cursor-type))
    (setq gfm-pretty-tables--saved-cursor-type 'gfm-pretty-tables-unset)))

(defconst gfm-pretty-tables--hint-groups
  `(("Edit"
     (((evil-forward-word-end) . "cell")
      ((evil-insert)           . "table"))
     (((evil-yank-line)        . "copy")))
    ("Columns" :when ,#'gfm-pretty-tables--in-header-p
     (((gfm-pretty-tables-swap-column-left gfm-pretty-tables-swap-column-right) . "swap"))))
  "Transient-style key hint groups shown while in a GFM table cell.
Each entry is (HEADING [KEYWORD VALUE...] ROW...) where ROW is a list of
\(CMDS . LABEL).  Supported keywords: `:when' PREDICATE — group is shown
only if PREDICATE returns non-nil.")

(defun gfm-pretty-tables--group-split (group)
  "Return (PROPS . ROWS) for GROUP, peeling leading plist keywords."
  (let ((tail (cdr group))
        (props nil))
    (while (keywordp (car tail))
      (push (car tail) props)
      (push (cadr tail) props)
      (setq tail (cddr tail)))
    (cons (nreverse props) tail)))

(defun gfm-pretty-tables--group-applicable-p (group)
  "Return non-nil if GROUP's `:when' predicate (if any) passes."
  (let* ((pred (plist-get (car (gfm-pretty-tables--group-split group)) :when)))
    (or (null pred) (funcall pred))))

(defun gfm-pretty-tables--key-desc (cmds)
  "Return CMDS' shortest active keys joined by `/' as a propertised string."
  (let ((parts (delq nil
                     (mapcar (lambda (c)
                               (when-let* ((seq (where-is-internal c nil t)))
                                 (key-description seq)))
                             cmds))))
    (and parts
         (propertize (mapconcat #'identity parts "/")
                     'face 'transient-key))))

(defun gfm-pretty-tables--render-hint-items (rows)
  "Flatten ROWS (list of `((CMDS . LABEL) ...)') to `\"k1 lbl  k2 lbl\"'."
  (let ((items (cl-loop for row in rows
                        append (cl-loop for (cmds . label) in row
                                        for keys = (gfm-pretty-tables--key-desc cmds)
                                        when keys
                                        collect (concat keys " " label)))))
    (mapconcat #'identity items "  ")))

(defun gfm-pretty-tables--render-hint-group (group)
  "Render GROUP as a single line: heading followed by all key/label items."
  (let ((heading (propertize (car group) 'face 'transient-heading))
        (items (gfm-pretty-tables--render-hint-items
                (cdr (gfm-pretty-tables--group-split group)))))
    (if (string-empty-p items) heading (concat heading " " items))))

(defvar-local gfm-pretty-tables--last-hinted-cell nil
  "Last (LINE-BEG . IDX) for which hints were echoed.
Indexed by row line position rather than overlay identity so a rebuild
\(which replaces row overlays) does not spuriously re-trigger the echo
and clobber another command's message (e.g. `Copied:').")

(defun gfm-pretty-tables--cell-key (info)
  "Return a stable (LINE-BEG . IDX) key for cell-info INFO, or nil."
  (and info (cons (overlay-start (car info)) (cdr info))))

(defun gfm-pretty-tables--echo-hints ()
  "Echo the transient-style table key hints without logging to `*Messages*'."
  (let* ((groups (seq-filter #'gfm-pretty-tables--group-applicable-p
                             gfm-pretty-tables--hint-groups))
         (line (mapconcat #'gfm-pretty-tables--render-hint-group groups "   "))
         (message-log-max nil))
    (message "%s" line)))

(defun gfm-pretty-tables--clear-cursor-anchor ()
  "Remove the `cursor' text property previously set by us, if any."
  (when (and gfm-pretty-tables--cursor-anchor
             (< gfm-pretty-tables--cursor-anchor (point-max)))
    (with-silent-modifications
      (remove-text-properties gfm-pretty-tables--cursor-anchor
                              (1+ gfm-pretty-tables--cursor-anchor)
                              '(cursor nil))))
  (setq gfm-pretty-tables--cursor-anchor nil))

(defun gfm-pretty-tables--set-cursor-anchor (pos)
  "Move the `cursor' text-property anchor to buffer position POS."
  (gfm-pretty-tables--clear-cursor-anchor)
  (when (and pos (< pos (point-max)))
    (with-silent-modifications
      (put-text-property pos (1+ pos) 'cursor t))
    (setq gfm-pretty-tables--cursor-anchor pos)))

(defun gfm-pretty-tables--cell-info-at-point ()
  "Return (ROW-OV . CELL-IDX) for point inside a navigable table row, else nil.
Returns nil when the tables decorator's overlay state is stale
relative to the current `buffer-chars-modified-tick'
\(`gfm-pretty--stale-p').  This propagates through every public
cell-aware command — they all key off this predicate — so a stale
overlay's `cell-bounds' integer list is never dereferenced as a
buffer position."
  (unless (gfm-pretty--stale-p 'tables)
    (let* ((ov (cl-find-if (lambda (o)
                             (overlay-get o 'gfm-pretty-tables-cell-bounds))
                           (overlays-at (point))))
           (cb (and ov (overlay-get ov 'gfm-pretty-tables-cell-bounds))))
      (when cb
        (cons ov
              (or (cl-position-if (lambda (b)
                                    (and (>= (point) (car b))
                                         (< (point) (cdr b))))
                                  cb)
                  0))))))

(defun gfm-pretty-tables--apply-cell-highlight (display dcb idx)
  "Return a copy of DISPLAY with cell IDX painted with the active-cell face.
DCB is the row's `gfm-pretty-tables-display-cell-bounds' overlay property: a
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
                                  'gfm-pretty-tables-active-cell-face nil s)
          (when first-line
            (put-text-property cell-beg (1+ cell-beg) 'cursor t s)
            (setq first-line nil)))
        (setq line-start (1+ nl))
        (setq line-idx (1+ line-idx))))
    s))

(defun gfm-pretty-tables--show-cell-highlight (anchor idx)
  "Repaint ANCHOR's paired display overlay so cell IDX shows the active-cell face.
Also anchors the visible cursor at the cell's first buffer char."
  (let* ((display-ov (gfm-pretty-tables--display-overlay-for-anchor anchor))
         (cb (overlay-get anchor 'gfm-pretty-tables-cell-bounds)))
    (when display-ov
      (let ((orig (or (overlay-get display-ov 'gfm-pretty-saved-display)
                      (overlay-get display-ov 'display)))
            (dcb (overlay-get display-ov 'gfm-pretty-tables-display-cell-bounds)))
        (overlay-put display-ov 'gfm-pretty-saved-display orig)
        (overlay-put display-ov 'display
                     (gfm-pretty-tables--apply-cell-highlight orig dcb idx))
        (setq gfm-pretty-tables--highlighted-anchor anchor
              gfm-pretty-tables--highlighted-row-ov display-ov)))
    (when (and cb (< idx (length cb)))
      (gfm-pretty-tables--set-cursor-anchor (car (nth idx cb))))))

(defun gfm-pretty-tables--hide-cursor-highlight ()
  "Restore the row's original display, clear the cursor anchor, and the cursor."
  (when (and gfm-pretty-tables--highlighted-row-ov
             (overlay-buffer gfm-pretty-tables--highlighted-row-ov))
    (let* ((ov gfm-pretty-tables--highlighted-row-ov)
           (orig (overlay-get ov 'gfm-pretty-saved-display)))
      (when orig
        (overlay-put ov 'display orig))
      (overlay-put ov 'gfm-pretty-saved-display nil)))
  (gfm-pretty-tables--clear-cursor-anchor)
  (gfm-pretty-tables--restore-cursor)
  (setq gfm-pretty-tables--highlighted-anchor nil
        gfm-pretty-tables--highlighted-row-ov nil
        gfm-pretty-tables--current-highlight-key nil))

(defun gfm-pretty-tables--update-cursor-highlight ()
  "Highlight the active cell at point and hide the buffer cursor.
The cursor is hidden because tty Emacs always renders it at the
overlay's right edge regardless of any `cursor' positioning hints, so
the highlight alone conveys cell selection.

When the tables decorator's overlay state is stale relative to the
current buffer tick (`gfm-pretty--stale-p'), any existing highlight
is hidden and the function returns without reading `cell-bounds' —
the next rebuild repaints the correct cell."
  (cond
   ((gfm-pretty--stale-p 'tables)
    (when gfm-pretty-tables--current-highlight-key
      (gfm-pretty-tables--hide-cursor-highlight)
      (setq gfm-pretty-tables--current-highlight-key nil
            gfm-pretty-tables--last-hinted-cell nil)))
   (t
  (gfm-pretty-tables--maybe-snap-to-cell)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (cond
     (info
      (let* ((anchor (car info))
             (idx (cdr info))
             (key (cons anchor idx)))
        (unless (equal key gfm-pretty-tables--current-highlight-key)
          (unless (eq anchor gfm-pretty-tables--highlighted-anchor)
            (gfm-pretty-tables--hide-cursor-highlight))
          (gfm-pretty-tables--show-cell-highlight anchor idx)
          (setq gfm-pretty-tables--current-highlight-key key)))
      (gfm-pretty-tables--save-and-hide-cursor)
      (let ((cell-key (gfm-pretty-tables--cell-key info)))
        (unless (equal cell-key gfm-pretty-tables--last-hinted-cell)
          (gfm-pretty-tables--echo-hints)
          (setq gfm-pretty-tables--last-hinted-cell cell-key))))
     (t
      (when gfm-pretty-tables--current-highlight-key
        (gfm-pretty-tables--hide-cursor-highlight)
        (setq gfm-pretty-tables--current-highlight-key nil
              gfm-pretty-tables--last-hinted-cell nil))))))))

;;; Cell-wise navigation

(defun gfm-pretty-tables--goto-cell (row-ov idx)
  "Move point to the start of cell IDX in ROW-OV.
IDX is clamped to the row's valid range.  Sets
`disable-point-adjustment' so Emacs does not snap point to the
end of the row-overlay's `display' range after the command."
  (let* ((cb (overlay-get row-ov 'gfm-pretty-tables-cell-bounds))
         (n (length cb))
         (clamped (max 0 (min (1- n) idx))))
    (goto-char (car (nth clamped cb)))
    (setq disable-point-adjustment t)))

(defun gfm-pretty-tables--row-on-relative-line (direction)
  "Return the gfm-pretty-tables navigable row overlay DIRECTION lines from point.
DIRECTION is 1 for the next row, -1 for the previous row.  Skips
non-navigable rows (e.g. the delimiter).  Confined to the current
table block so motion at a table edge yields nil instead of jumping
into a neighbouring table."
  (when-let* ((block (gfm-pretty-tables--current-block)))
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
                             (overlay-get o 'gfm-pretty-tables-cell-bounds))
                           (overlays-at lbeg))))
                  (when ov (setq found ov)))))))
          found)))))

(defun gfm-pretty-tables-cell-forward ()
  "Move to the next cell in the current table row.
Returns non-nil on success, nil when point is already on the last cell."
  (interactive)
  (let* ((info (gfm-pretty-tables--cell-info-at-point))
         (cb (and info (overlay-get (car info) 'gfm-pretty-tables-cell-bounds)))
         (next-idx (and info (1+ (cdr info)))))
    (when (and info (< next-idx (length cb)))
      (gfm-pretty-tables--goto-cell (car info) next-idx)
      t)))

(defun gfm-pretty-tables-cell-backward ()
  "Move to the previous cell in the current table row.
Returns non-nil on success, nil when point is already on the first cell."
  (interactive)
  (let* ((info (gfm-pretty-tables--cell-info-at-point))
         (prev-idx (and info (1- (cdr info)))))
    (when (and info (>= prev-idx 0))
      (gfm-pretty-tables--goto-cell (car info) prev-idx)
      t)))

(defun gfm-pretty-tables-row-down ()
  "Move to the same cell in the next table row.
Returns non-nil on success, nil when there is no next table row."
  (interactive)
  (let* ((info (gfm-pretty-tables--cell-info-at-point))
         (next (and info (gfm-pretty-tables--row-on-relative-line 1))))
    (when next
      (gfm-pretty-tables--goto-cell next (cdr info))
      t)))

(defun gfm-pretty-tables-row-up ()
  "Move to the same cell in the previous table row.
Returns non-nil on success, nil when there is no previous table row."
  (interactive)
  (let* ((info (gfm-pretty-tables--cell-info-at-point))
         (prev (and info (gfm-pretty-tables--row-on-relative-line -1))))
    (when prev
      (gfm-pretty-tables--goto-cell prev (cdr info))
      t)))

(defun gfm-pretty-tables-row-first-cell ()
  "Move to the first cell in the current table row."
  (interactive)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (when info (gfm-pretty-tables--goto-cell (car info) 0))))

(defun gfm-pretty-tables-row-last-cell ()
  "Move to the last cell in the current table row."
  (interactive)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (when info
      (gfm-pretty-tables--goto-cell (car info) most-positive-fixnum))))

(defun gfm-pretty-tables-cell-copy ()
  "Copy the active table cell's content to the kill ring."
  (interactive)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((bounds (gfm-pretty-tables--cell-content-bounds (car info) (cdr info)))
           (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (kill-new text)
      (message "Copied: %s"
               (truncate-string-to-width text 60 nil nil t)))))

(defun gfm-pretty-tables-do-at-cell ()
  "Dispatch `markdown-do' on the active cell's contents.
If the cell contains a link, point is moved onto it first so
`markdown-do' follows the link instead of acting at the row's
left edge."
  (interactive)
  (let ((info (gfm-pretty-tables--cell-info-at-point)))
    (unless info
      (user-error "Point is not in a table cell"))
    (let* ((row-ov (car info))
           (idx (cdr info))
           (link-pos (gfm-pretty-tables--cell-link-pos row-ov idx))
           (bounds (gfm-pretty-tables--cell-content-bounds row-ov idx)))
      (goto-char (or link-pos (car bounds)))
      (call-interactively #'markdown-do))))

(defun gfm-pretty-tables--insert-new-row-after-current ()
  "Insert an empty row after the current one and put point in cell 0."
  (let* ((info (gfm-pretty-tables--cell-info-at-point))
         (row-ov (car info))
         (n (length (overlay-get row-ov 'gfm-pretty-tables-cell-bounds))))
    (goto-char (line-end-position))
    (insert "\n|"
            (apply #'concat (make-list n "  |")))
    (gfm-pretty-tables--rebuild)
    (let ((new-row (cl-find-if
                    (lambda (o) (overlay-get o 'gfm-pretty-tables-cell-bounds))
                    (overlays-at (line-beginning-position)))))
      (when new-row
        (gfm-pretty-tables--goto-cell new-row 0)))))

(defun gfm-pretty-tables-cell-tab ()
  "Move to the next cell.
At end of a row, wrap to the next row's first cell.  At end of the
last row, insert a fresh empty row and land in its first cell."
  (interactive)
  (cond
   ((gfm-pretty-tables-cell-forward) t)
   ((let ((next (gfm-pretty-tables--row-on-relative-line 1)))
      (when next (gfm-pretty-tables--goto-cell next 0) t)))
   (t (gfm-pretty-tables--insert-new-row-after-current))))

(defun gfm-pretty-tables-cell-backtab ()
  "Move to the previous cell.
At a row start, wrap to the previous row's last cell."
  (interactive)
  (unless (gfm-pretty-tables-cell-backward)
    (let ((prev-row (gfm-pretty-tables--row-on-relative-line -1)))
      (when prev-row
        (gfm-pretty-tables--goto-cell prev-row most-positive-fixnum)))))

;;; Header column reordering

(defun gfm-pretty-tables--current-block ()
  "Return the table block (HEADER-BEG DELIM-BEG BODY-BEG BODY-END) at point."
  (let ((pt (point)))
    (cl-loop for block in (gfm-pretty-tables--find-blocks)
             when (and (>= pt (nth 0 block)) (<= pt (nth 3 block)))
             return block)))

(defun gfm-pretty-tables--in-header-p ()
  "Non-nil if point is on the header row of a table."
  (let ((block (gfm-pretty-tables--current-block)))
    (and block (= (line-beginning-position) (nth 0 block)))))

(defun gfm-pretty-tables--swap-cells-in-line (line-pos a b)
  "On the line containing LINE-POS, swap cell A with cell B by index.
A and B are zero-based.  No-op if either index is out of range."
  (save-excursion
    (goto-char line-pos)
    (let* ((lb (line-beginning-position))
           (le (line-end-position))
           (cb (gfm-pretty-tables--cell-bounds lb le))
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

(defun gfm-pretty-tables--swap-columns (direction)
  "Swap the active cell's column with its DIRECTION (-1 or 1) neighbour.
Silently no-op when point is not on a header row, or when the
neighbour column is out of range."
  (let* ((info (and (gfm-pretty-tables--in-header-p)
                    (gfm-pretty-tables--cell-info-at-point)))
         (block (and info (gfm-pretty-tables--current-block)))
         (src-idx (and info (cdr info)))
         (dst-idx (and info (+ src-idx direction)))
         (n-cols (and info
                      (length (overlay-get (car info)
                                           'gfm-pretty-tables-cell-bounds)))))
    (when (and info (>= dst-idx 0) (< dst-idx n-cols))
      (let ((header-beg (nth 0 block))
            (delim-beg (nth 1 block))
            (body-beg  (nth 2 block))
            (body-end  (nth 3 block)))
        (save-excursion
          (gfm-pretty-tables--swap-cells-in-line header-beg src-idx dst-idx)
          (gfm-pretty-tables--swap-cells-in-line delim-beg src-idx dst-idx)
          (goto-char body-beg)
          (while (and (< (point) body-end) (looking-at-p "^|"))
            (gfm-pretty-tables--swap-cells-in-line (point) src-idx dst-idx)
            (forward-line 1))))
      (gfm-pretty-tables--rebuild)
      (let ((row-ov (cl-find-if (lambda (o)
                                  (overlay-get o 'gfm-pretty-tables-cell-bounds))
                                (overlays-at (line-beginning-position)))))
        (when row-ov
          (gfm-pretty-tables--goto-cell row-ov dst-idx))))))

(defun gfm-pretty-tables-swap-column-left ()
  "Swap the active header column with the column to its left."
  (interactive)
  (gfm-pretty-tables--swap-columns -1))

(defun gfm-pretty-tables-swap-column-right ()
  "Swap the active header column with the column to its right."
  (interactive)
  (gfm-pretty-tables--swap-columns 1))

(defconst gfm-pretty-tables--snap-suppressing-command-re
  (rx bos (or "isearch-" "evil-search-" "evil-ex-search-"))
  "Regex on `this-command' name; matched commands suppress snap-to-cell.
Search-style commands park point at an exact match position (which
may be a column gap or border) and must not have the post-command
snap yank point back to cell 0 — that would make the next repeat
re-find the same match and trap isearch in an infinite loop.")

(defun gfm-pretty-tables--maybe-snap-to-cell ()
  "If point is on a table row but outside any cell range, snap to cell 0.
Idempotent when point already sits in a cell so it is safe to call
unconditionally (e.g. from `post-command-hook').  Skipped when the
row is invisible (e.g. inside a folded outline heading) so motion
through hidden text does not pull point into a hidden table.  Also
skipped while `isearch-mode' is active or `this-command' is a
search-style command so search results that land on a column gap
or border are not rewound.  Bailed out early when the tables
decorator's overlay state is stale relative to the current buffer
tick (`gfm-pretty--stale-p'); the next rebuild restores correct
snap behaviour."
  (unless (gfm-pretty--stale-p 'tables)
  (let* ((lbeg (line-beginning-position))
         (row-ov (cl-find-if
                  (lambda (o) (overlay-get o 'gfm-pretty-tables-cell-bounds))
                  (overlays-at lbeg))))
    (when (and row-ov
               (not (invisible-p lbeg))
               (not (bound-and-true-p isearch-mode))
               (not (and (symbolp this-command)
                         (string-match-p gfm-pretty-tables--snap-suppressing-command-re
                                         (symbol-name this-command)))))
      (let* ((cb (overlay-get row-ov 'gfm-pretty-tables-cell-bounds))
             (in-cell (cl-find-if
                       (lambda (b) (and (>= (point) (car b))
                                        (< (point) (cdr b))))
                       cb)))
        (unless in-cell
          (gfm-pretty-tables--goto-cell row-ov 0)))))))

(defmacro gfm-pretty-tables--define-evil-motion (name table-fn evil-fn fall-through)
  "Define NAME as a wrapper dispatching between TABLE-FN and EVIL-FN.
When point is in a table, TABLE-FN runs first.  If it returns nil
\(no-op at an edge) and FALL-THROUGH is non-nil, EVIL-FN runs as a
fallback so motion can escape the table; otherwise the no-op is
silent.  Outside a table, EVIL-FN runs directly; if FALL-THROUGH is
the symbol `snap', a post-call landing in a table snaps to cell 0.

When the tables decorator's overlay state is stale relative to the
current `buffer-chars-modified-tick' (`gfm-pretty--stale-p'), the
wrapper bypasses TABLE-FN entirely — running EVIL-FN as if point
were outside any table — and skips the snap step even when
FALL-THROUGH is `snap'.  This prevents `goto-char' against
cached integer offsets that the next rebuild has yet to refresh."
  `(defun ,name ()
     ,(format "Cell-aware shim for `%s'." evil-fn)
     (interactive)
     (cond
      ((gfm-pretty--stale-p 'tables)
       (call-interactively #',evil-fn))
      ((gfm-pretty-tables--cell-info-at-point)
       (or (,table-fn)
           ,(when fall-through
              `(progn (call-interactively #',evil-fn)
                      ,(when (eq fall-through 'snap)
                         '(unless (gfm-pretty--stale-p 'tables)
                            (gfm-pretty-tables--maybe-snap-to-cell)))))))
      (t (call-interactively #',evil-fn)
         ,(when (eq fall-through 'snap)
            '(unless (gfm-pretty--stale-p 'tables)
               (gfm-pretty-tables--maybe-snap-to-cell)))))))

(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-h gfm-pretty-tables-cell-backward  evil-backward-char           nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-l gfm-pretty-tables-cell-forward   evil-forward-char            nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-j gfm-pretty-tables-row-down       evil-next-line               snap)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-k gfm-pretty-tables-row-up         evil-previous-line           snap)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-^ gfm-pretty-tables-row-first-cell evil-first-non-blank         nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-$ gfm-pretty-tables-row-last-cell  evil-end-of-line             nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-w gfm-pretty-tables-cell-forward   evil-forward-word-begin      nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-W gfm-pretty-tables-cell-forward   evil-forward-WORD-begin      nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-e gfm-pretty-tables-cell-forward   evil-forward-word-end        nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-E gfm-pretty-tables-cell-forward   evil-forward-WORD-end        nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-b gfm-pretty-tables-cell-backward  evil-backward-word-begin     nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-B gfm-pretty-tables-cell-backward  evil-backward-WORD-begin     nil)
(gfm-pretty-tables--define-evil-motion gfm-pretty-tables--evil-0 gfm-pretty-tables-row-first-cell evil-beginning-of-line       nil)

(defun gfm-pretty-tables--evil-Y ()
  "Cell-aware shim for `evil-yank-line': copy the active cell when in a table."
  (interactive)
  (cond
   ((gfm-pretty-tables--cell-info-at-point) (gfm-pretty-tables-cell-copy))
   (t (call-interactively #'evil-yank-line))))

(defvar gfm-pretty-tables--row-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB")          #'gfm-pretty-tables-cell-tab)
    (define-key m (kbd "<tab>")        #'gfm-pretty-tables-cell-tab)
    (define-key m (kbd "<backtab>")    #'gfm-pretty-tables-cell-backtab)
    (define-key m (kbd "S-<tab>")      #'gfm-pretty-tables-cell-backtab)
    (define-key m (kbd "RET")          #'gfm-pretty-tables-do-at-cell)
    (define-key m (kbd "<return>")     #'gfm-pretty-tables-do-at-cell)
    (define-key m (kbd "M-h")          #'gfm-pretty-tables-swap-column-left)
    (define-key m (kbd "M-l")          #'gfm-pretty-tables-swap-column-right)
    (define-key m [remap evil-backward-char]       #'gfm-pretty-tables--evil-h)
    (define-key m [remap evil-forward-char]        #'gfm-pretty-tables--evil-l)
    (define-key m [remap evil-next-line]           #'gfm-pretty-tables--evil-j)
    (define-key m [remap evil-previous-line]       #'gfm-pretty-tables--evil-k)
    (define-key m [remap evil-first-non-blank]     #'gfm-pretty-tables--evil-^)
    (define-key m [remap evil-end-of-line]         #'gfm-pretty-tables--evil-$)
    (define-key m [remap evil-forward-word-begin]  #'gfm-pretty-tables--evil-w)
    (define-key m [remap evil-forward-WORD-begin]  #'gfm-pretty-tables--evil-W)
    (define-key m [remap evil-forward-word-end]    #'gfm-pretty-tables-edit-cell-at-point)
    (define-key m [remap evil-forward-WORD-end]    #'gfm-pretty-tables-edit-cell-at-point)
    (define-key m [remap evil-backward-word-begin] #'gfm-pretty-tables--evil-b)
    (define-key m [remap evil-backward-WORD-begin] #'gfm-pretty-tables--evil-B)
    (define-key m [remap evil-beginning-of-line]   #'gfm-pretty-tables--evil-0)
    (define-key m [remap evil-yank-line]           #'gfm-pretty-tables--evil-Y)
    m)
  "Keymap attached to row overlays; bindings only fire inside a table.")

;;; Evil insert/replace shim

(defconst gfm-pretty-tables--evil-edit-commands
  '(evil-insert evil-append evil-insert-line evil-append-line
                evil-substitute evil-change-whole-line
                evil-open-below evil-open-above
                evil-change evil-change-line
                evil-replace evil-replace-state)
  "Evil commands diverted to `gfm-pretty-tables-edit-table-at-point' in tables.")

(defun gfm-pretty-tables--maybe-edit-advice (orig &rest args)
  "Around-advice: divert ORIG (called with ARGS) to the table editor in tables."
  (if (and (gfm-pretty--state-get 'tables 'enabled-p)
           (gfm-pretty-tables--block-at-point))
      (gfm-pretty-tables-edit-table-at-point)
    (apply orig args)))

(with-eval-after-load 'evil
  (dolist (cmd gfm-pretty-tables--evil-edit-commands)
    (when (fboundp cmd)
      (advice-add cmd :around #'gfm-pretty-tables--maybe-edit-advice))))

;;; gfm-pretty decorator registration

(defun gfm-pretty-tables--full-rebuild-required-p (dirty)
  "Engine `:full-rebuild-required-p' — non-nil on code-fence marker edits.
Tables exclude content inside fenced code blocks; touching a fence
line can create or destroy adjacent tables, so any such edit forces
a full rebuild.  Edits inside a table's own header / delimiter /
body fit within the block's source range and scope via the engine's
block-containment fallback."
  (and (fboundp 'gfm-pretty-fences--find-blocks)
       (cl-some
        (lambda (b)
          (let* ((open-beg (nth 0 b))
                 (close-end (nth 3 b))
                 (open-line (cons (save-excursion
                                    (goto-char open-beg)
                                    (line-beginning-position))
                                  (save-excursion
                                    (goto-char open-beg)
                                    (line-end-position))))
                 (close-line (cons (save-excursion
                                     (goto-char close-end)
                                     (line-beginning-position))
                                   (save-excursion
                                     (goto-char close-end)
                                     (line-end-position)))))
            (or (gfm-pretty--region-overlaps-p dirty open-line)
                (gfm-pretty--region-overlaps-p dirty close-line))))
        (gfm-pretty-fences--find-blocks))))

(defalias 'gfm-pretty-tables--edit-at-point #'gfm-pretty-tables-edit-table-at-point
  "Conventional decorator-protocol name for the table editor.
The umbrella `gfm-pretty-edit-block-at-point' dispatch looks for
`<name>--edit-at-point' by intern.")

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'tables
    :phase                 'containers
    :registry              gfm-pretty-tables--registry
    :collect-fn            #'gfm-pretty-tables--collect-blocks
    :range-fn              #'gfm-pretty-tables--block-range
    :apply-block-fn        #'gfm-pretty-tables--apply-block
    :rebuild-fn            #'gfm-pretty-tables--rebuild
    :full-rebuild-required-p #'gfm-pretty-tables--full-rebuild-required-p
    :on-enable-fn          #'gfm-pretty-tables--on-enable
    :on-disable-fn         #'gfm-pretty-tables--on-disable))

(provide 'gfm-pretty-tables)

;;; gfm-pretty-tables.el ends here
